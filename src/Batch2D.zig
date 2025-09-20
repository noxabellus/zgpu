//! A batched 2D renderer for shapes, images, and text.
const Batch2D = @This();

const std = @import("std");
const wgpu = @import("wgpu");
const stbtt = @import("stbtt");
const MultiAtlas = @import("MultiAtlas.zig");

const log = std.log.scoped(.renderer);

test {
    log.debug("semantic analysis for Renderer.zig", .{});
    std.testing.refAllDecls(@This());
}

// --- Public Constants ---
pub const GLYPH_ID_FLAG: u32 = 0x80000000;
const IS_GLYPH_MASK = 0x80000000;

// --- Glyph ID Encoding ---
const FONT_INDEX_SHIFT = 28;
const PIXEL_HEIGHT_SHIFT = 21;
const CHAR_CODE_MASK: u32 = 0x1FFFFF;
const PIXEL_HEIGHT_MASK: u32 = 0x7F;
const FONT_INDEX_MASK: u32 = 0x7;

pub fn encodeGlyphId(font_index: u32, pixel_height: f32, char_code: u21) MultiAtlas.ImageId {
    const height_encoded = @as(u32, @intFromFloat(pixel_height));
    return GLYPH_ID_FLAG |
        ((font_index & FONT_INDEX_MASK) << FONT_INDEX_SHIFT) |
        ((height_encoded & PIXEL_HEIGHT_MASK) << PIXEL_HEIGHT_SHIFT) |
        (@as(u32, @intCast(char_code)) & CHAR_CODE_MASK);
}

pub const DecodedGlyphId = struct { font_index: u32, pixel_height: f32, char_code: u21 };
pub fn decodeGlyphId(id: MultiAtlas.ImageId) DecodedGlyphId {
    return .{
        .font_index = @intCast((id >> FONT_INDEX_SHIFT) & FONT_INDEX_MASK),
        .pixel_height = @as(f32, @floatFromInt((id >> PIXEL_HEIGHT_SHIFT) & PIXEL_HEIGHT_MASK)),
        .char_code = @intCast(id & CHAR_CODE_MASK),
    };
}

// --- Public API Structs ---
pub const Mat4 = [16]f32;
pub const Vec2 = struct { x: f32 = 0.0, y: f32 = 0.0 };
pub const Color = struct { r: f32 = 0.0, g: f32 = 0.0, b: f32 = 0.0, a: f32 = 0.0 };

// --- Internal Structs ---

const Vertex = extern struct {
    position: [2]f32,
    tex_coords: [2]f32,
    color: [4]f32,
    /// Encoded rendering parameters passed to the shader.
    /// - Bit 31: Is Glyph flag (can be used for shader optimizations)
    /// - Bits 0-30: Logical Image ID (index into the indirection table storage buffer)
    encoded_params: u32,
};

const IMAGE_ID_MASK: u32 = 0x7FFFFFFF;

const Uniforms = extern struct {
    projection: Mat4,
};

const Patch = struct {
    image_id: MultiAtlas.ImageId,
    wants_mips: bool,
    vertex_start_index: usize,
    vertex_count: usize,
};

pub const ProviderContext = struct {
    provider: MultiAtlas.DataProvider,
    user_context: ?*anyopaque,
};

// --- WGPU Resources and State ---
allocator: std.mem.Allocator,
device: wgpu.Device,
queue: wgpu.Queue,
pipeline: wgpu.RenderPipeline,
uniform_buffer: wgpu.Buffer,
vertex_buffer: wgpu.Buffer,
vertex_buffer_capacity: usize,
vertices: std.ArrayList(Vertex),
atlas: *MultiAtlas,
bind_group: wgpu.BindGroup,
sampler: wgpu.Sampler,
patch_list: std.ArrayList(Patch),
provider_context: ProviderContext,
indirection_buffer: wgpu.Buffer,
indirection_buffer_capacity: usize,

pub fn init(
    allocator: std.mem.Allocator,
    device: wgpu.Device,
    queue: wgpu.Queue,
    surface_format: wgpu.TextureFormat,
    sample_count: u32,
) !*Batch2D {
    const self = try allocator.create(Batch2D);
    errdefer allocator.destroy(self);

    const atlas = try MultiAtlas.init(allocator, device, queue, 2048, 2048, MultiAtlas.MAX_MIP_LEVELS);
    errdefer atlas.deinit();

    const shader_module = try wgpu.loadShaderText(device, "Renderer", @embedFile("shaders/Renderer.wgsl"));
    defer wgpu.shaderModuleRelease(shader_module);

    const uniform_buffer = wgpu.deviceCreateBuffer(device, &.{
        .label = .fromSlice("uniform_buffer"),
        .usage = wgpu.BufferUsage{ .uniform = true, .copy_dst = true },
        .size = @sizeOf(Uniforms),
    });
    std.debug.assert(uniform_buffer != null);
    errdefer wgpu.bufferRelease(uniform_buffer);

    const initial_indirection_capacity = 2048;
    const indirection_buffer = wgpu.deviceCreateBuffer(device, &.{
        .label = .fromSlice("indirection_buffer"),
        .usage = .{ .storage = true, .copy_dst = true },
        .size = initial_indirection_capacity * @sizeOf(MultiAtlas.ImageMipData),
    });
    std.debug.assert(indirection_buffer != null);
    errdefer wgpu.bufferRelease(indirection_buffer);

    const sampler = wgpu.deviceCreateSampler(device, &.{
        .label = .fromSlice("image_texture_sampler"),
        .address_mode_u = .clamp_to_edge,
        .address_mode_v = .clamp_to_edge,
        .mag_filter = .linear,
        .min_filter = .linear,
        .mipmap_filter = .linear,
    });
    std.debug.assert(sampler != null);
    errdefer wgpu.samplerRelease(sampler);

    const bind_group_layout = wgpu.deviceCreateBindGroupLayout(device, &.{
        .label = .fromSlice("bind_group_layout"),
        .entry_count = 4,
        .entries = &[_]wgpu.BindGroupLayoutEntry{
            .{ .binding = 0, .visibility = .vertexStage, .buffer = .{ .type = .uniform } },
            .{ .binding = 1, .visibility = .fragmentStage, .sampler = .{ .type = .filtering } },
            .{ .binding = 2, .visibility = .fragmentStage, .texture = .{ .sample_type = .float, .view_dimension = .@"2d_array" } },
            .{ .binding = 3, .visibility = .fragmentStage, .buffer = .{ .type = .read_only_storage } },
        },
    });
    std.debug.assert(bind_group_layout != null);
    defer wgpu.bindGroupLayoutRelease(bind_group_layout);

    const pipeline_layout = wgpu.deviceCreatePipelineLayout(device, &.{
        .label = .fromSlice("pipeline_layout"),
        .bind_group_layout_count = 1,
        .bind_group_layouts = &.{bind_group_layout},
    });
    std.debug.assert(pipeline_layout != null);
    defer wgpu.pipelineLayoutRelease(pipeline_layout);

    const blend_state = wgpu.BlendState{
        .color = .{ .operation = .add, .src_factor = .one, .dst_factor = .one_minus_src_alpha },
        .alpha = .{ .operation = .add, .src_factor = .one, .dst_factor = .one_minus_src_alpha },
    };

    const pipeline = wgpu.deviceCreateRenderPipeline(device, &wgpu.RenderPipelineDescriptor{
        .label = .fromSlice("render_pipeline"),
        .layout = pipeline_layout,
        .vertex = .{
            .module = shader_module,
            .entry_point = .fromSlice("vs_main"),
            .buffer_count = 1,
            .buffers = &.{.{
                .array_stride = @sizeOf(Vertex),
                .step_mode = .vertex,
                .attribute_count = 4,
                .attributes = &[_]wgpu.VertexAttribute{
                    .{ .shaderLocation = 0, .offset = @offsetOf(Vertex, "position"), .format = .float32x2 },
                    .{ .shaderLocation = 1, .offset = @offsetOf(Vertex, "tex_coords"), .format = .float32x2 },
                    .{ .shaderLocation = 2, .offset = @offsetOf(Vertex, "color"), .format = .float32x4 },
                    .{ .shaderLocation = 3, .offset = @offsetOf(Vertex, "encoded_params"), .format = .uint32 },
                },
            }},
        },
        .fragment = &wgpu.FragmentState{
            .module = shader_module,
            .entry_point = .fromSlice("fs_main"),
            .target_count = 1,
            .targets = &.{.{ .format = surface_format, .blend = &blend_state, .write_mask = .all }},
        },
        .primitive = .{ .topology = .triangle_list },
        .multisample = .{ .count = sample_count, .mask = 0xFFFFFFFF },
    });
    std.debug.assert(pipeline != null);

    const initial_capacity = 4096;
    self.* = .{
        .allocator = allocator,
        .device = device,
        .queue = queue,
        .pipeline = pipeline,
        .uniform_buffer = uniform_buffer,
        .vertex_buffer = undefined,
        .vertex_buffer_capacity = 0,
        .vertices = .empty,
        .atlas = atlas,
        .bind_group = null,
        .sampler = sampler,
        .patch_list = .empty,
        .provider_context = .{ .provider = undefined, .user_context = null },
        .indirection_buffer = indirection_buffer,
        .indirection_buffer_capacity = initial_indirection_capacity,
    };

    try self.vertices.ensureTotalCapacity(self.allocator, initial_capacity);
    try self.patch_list.ensureTotalCapacity(self.allocator, 128);

    return self;
}

pub fn deinit(self: *Batch2D) void {
    self.atlas.deinit();
    if (self.bind_group != null) wgpu.bindGroupRelease(self.bind_group);
    if (self.vertex_buffer_capacity > 0) wgpu.bufferRelease(self.vertex_buffer);
    wgpu.bufferRelease(self.indirection_buffer);
    wgpu.bufferRelease(self.uniform_buffer);
    wgpu.samplerRelease(self.sampler);
    wgpu.renderPipelineRelease(self.pipeline);
    self.vertices.deinit(self.allocator);
    self.patch_list.deinit(self.allocator);
    self.allocator.destroy(self);
}

pub fn beginFrame(self: *Batch2D, projection: Mat4, context: ProviderContext) void {
    wgpu.queueWriteBuffer(self.queue, self.uniform_buffer, 0, &Uniforms{ .projection = projection }, @sizeOf(Uniforms));
    self.vertices.clearRetainingCapacity();
    self.patch_list.clearRetainingCapacity();
    self.provider_context = context;
}

pub fn endFrame(self: *Batch2D) !void {
    const atlas_recreated = try self.atlas.flush(self.provider_context);

    for (self.patch_list.items) |p| {
        const location = self.atlas.query(p.image_id, p.wants_mips, self.provider_context) catch |err| {
            log.err("image {any} not found in cache after flush. error: {any}", .{ p.image_id, err });
            continue;
        };
        const is_glyph_flag = p.image_id & IS_GLYPH_MASK;
        const encoded_params = is_glyph_flag | (location.indirection_table_index & IMAGE_ID_MASK);
        const verts = self.vertices.items[p.vertex_start_index .. p.vertex_start_index + 6];
        for (verts) |*v| v.encoded_params = encoded_params;
    }

    if (self.atlas.indirection_table.items.len > 0) {
        const required_capacity = self.atlas.indirection_table.items.len;
        if (self.indirection_buffer_capacity < required_capacity) {
            wgpu.bufferRelease(self.indirection_buffer);
            const new_capacity = @max(self.indirection_buffer_capacity * 2, required_capacity);
            self.indirection_buffer = wgpu.deviceCreateBuffer(self.device, &.{
                .label = .fromSlice("indirection_buffer"),
                .usage = .{ .storage = true, .copy_dst = true },
                .size = new_capacity * @sizeOf(MultiAtlas.ImageMipData),
            });
            self.indirection_buffer_capacity = new_capacity;
            log.info("resized indirection buffer to capacity {d}", .{new_capacity});
            if (self.bind_group != null) {
                wgpu.bindGroupRelease(self.bind_group);
                self.bind_group = null;
            }
        }
        wgpu.queueWriteBuffer(self.queue, self.indirection_buffer, 0, self.atlas.indirection_table.items.ptr, required_capacity * @sizeOf(MultiAtlas.ImageMipData));
    }

    if (self.vertices.items.len > 0) {
        const required_size = self.vertices.items.len * @sizeOf(Vertex);
        if (self.vertex_buffer_capacity < required_size) {
            if (self.vertex_buffer_capacity > 0) wgpu.bufferRelease(self.vertex_buffer);
            const new_capacity = @max(self.vertex_buffer_capacity * 2, required_size);
            self.vertex_buffer = wgpu.deviceCreateBuffer(self.device, &.{
                .label = .fromSlice("vertex_buffer"),
                .usage = wgpu.BufferUsage{ .vertex = true, .copy_dst = true },
                .size = new_capacity,
            });
            self.vertex_buffer_capacity = new_capacity;
            log.info("resized vertex buffer to {d} bytes", .{new_capacity});
        }
        wgpu.queueWriteBuffer(self.queue, self.vertex_buffer, 0, self.vertices.items.ptr, required_size);
    }

    if (atlas_recreated and self.bind_group != null) {
        wgpu.bindGroupRelease(self.bind_group);
        self.bind_group = null;
    }

    if (self.bind_group == null) {
        try self.recreateBindGroup();
    }
}

pub fn render(self: *Batch2D, render_pass: wgpu.RenderPassEncoder) !void {
    if (self.vertices.items.len == 0 or self.bind_group == null) return;

    wgpu.renderPassEncoderSetPipeline(render_pass, self.pipeline);
    wgpu.renderPassEncoderSetVertexBuffer(render_pass, 0, self.vertex_buffer, 0, self.vertices.items.len * @sizeOf(Vertex));
    wgpu.renderPassEncoderSetBindGroup(render_pass, 0, self.bind_group, 0, null);
    wgpu.renderPassEncoderDraw(render_pass, @intCast(self.vertices.items.len), 1, 0, 0);
}

pub fn drawTexturedQuad(self: *Batch2D, image_id: MultiAtlas.ImageId, wants_mips: bool, pos: Vec2, size: Vec2, tint: Color) !void {
    const location = self.atlas.query(image_id, wants_mips, self.provider_context) catch |err| {
        if (err == error.ImageNotYetPacked) {
            const vertex_start_index = self.vertices.items.len;
            try self.patch_list.append(self.allocator, .{
                .image_id = image_id,
                .wants_mips = wants_mips,
                .vertex_start_index = vertex_start_index,
                .vertex_count = 6,
            });
            return self.pushPlaceholderQuad(pos, size, tint);
        }
        return err;
    };

    const is_glyph_flag = image_id & IS_GLYPH_MASK;
    const encoded_params = is_glyph_flag | (location.indirection_table_index & IMAGE_ID_MASK);
    try self.pushQuad(encoded_params, pos, size, tint);
}

pub fn drawText(self: *Batch2D, string: []const u8, font_info: *const stbtt.FontInfo, font_index: u32, pixel_height: f32, pos: Vec2, tint: Color) !void {
    const font_scale = stbtt.scaleForPixelHeight(font_info, pixel_height);
    var min_iy0: i32 = 0;
    for (string) |char| {
        var iy0: i32 = 0;
        stbtt.getCodepointBitmapBox(font_info, @intCast(@as(u21, @intCast(char))), font_scale, font_scale, null, &iy0, null, null);
        min_iy0 = @min(min_iy0, iy0);
    }

    const baseline_y = pos.y - @as(f32, @floatFromInt(min_iy0));
    var xpos = pos.x;
    var prev_char: u21 = 0;
    const GLYPH_PADDING: f32 = 2.0;

    for (string) |char| {
        const char_code = @as(u21, @intCast(char));
        if (prev_char != 0) {
            const kern = stbtt.getCodepointKernAdvance(font_info, prev_char, char_code);
            xpos += @as(f32, @floatFromInt(kern)) * font_scale;
        }

        var adv: i32 = 0;
        stbtt.getCodepointHMetrics(font_info, @intCast(char_code), &adv, null);

        var ix0: i32 = 0;
        var iy0: i32 = 0;
        var ix1: i32 = 0;
        var iy1: i32 = 0;
        stbtt.getCodepointBitmapBox(font_info, @intCast(char_code), font_scale, font_scale, &ix0, &iy0, &ix1, &iy1);

        const width: f32 = @floatFromInt(ix1 - ix0);
        const height: f32 = @floatFromInt(iy1 - iy0);

        if (width > 0 and height > 0) {
            const padded_width = width + (GLYPH_PADDING * 2.0);
            const padded_height = height + (GLYPH_PADDING * 2.0);
            const ideal_x = xpos + @as(f32, @floatFromInt(ix0)) - GLYPH_PADDING;
            const ideal_y = baseline_y + @as(f32, @floatFromInt(iy0)) - GLYPH_PADDING;
            const rounded_x = @floor(ideal_x + 0.5);
            const rounded_y = @floor(ideal_y + 0.5);
            const final_pos = Vec2{ .x = rounded_x - 0.5, .y = rounded_y - 0.5 };
            const char_size = Vec2{ .x = padded_width, .y = padded_height };
            const glyph_id = encodeGlyphId(font_index, pixel_height, char_code);
            try self.drawTexturedQuad(glyph_id, false, final_pos, char_size, tint);
        }
        xpos += @as(f32, @floatFromInt(adv)) * font_scale;
        prev_char = char_code;
    }
}

fn pushQuad(self: *Batch2D, encoded_params: u32, pos: Vec2, size: Vec2, tint: Color) !void {
    const x1 = pos.x;
    const y1 = pos.y;
    const x2 = pos.x + size.x;
    const y2 = pos.y + size.y;
    const c: [4]f32 = .{ tint.r, tint.g, tint.b, tint.a };
    try self.vertices.appendSlice(self.allocator, &[_]Vertex{
        .{ .position = .{ x1, y1 }, .tex_coords = .{ 0.0, 0.0 }, .color = c, .encoded_params = encoded_params },
        .{ .position = .{ x2, y1 }, .tex_coords = .{ 1.0, 0.0 }, .color = c, .encoded_params = encoded_params },
        .{ .position = .{ x1, y2 }, .tex_coords = .{ 0.0, 1.0 }, .color = c, .encoded_params = encoded_params },
        .{ .position = .{ x1, y2 }, .tex_coords = .{ 0.0, 1.0 }, .color = c, .encoded_params = encoded_params },
        .{ .position = .{ x2, y1 }, .tex_coords = .{ 1.0, 0.0 }, .color = c, .encoded_params = encoded_params },
        .{ .position = .{ x2, y2 }, .tex_coords = .{ 1.0, 1.0 }, .color = c, .encoded_params = encoded_params },
    });
}

fn pushPlaceholderQuad(self: *Batch2D, pos: Vec2, size: Vec2, tint: Color) !void {
    try self.pushQuad(0, pos, size, tint);
}

fn recreateBindGroup(self: *Batch2D) !void {
    const layout = wgpu.renderPipelineGetBindGroupLayout(self.pipeline, 0);
    const bg = wgpu.deviceCreateBindGroup(self.device, &.{
        .label = .fromSlice("atlas_array_bind_group"),
        .layout = layout,
        .entry_count = 4,
        .entries = &[_]wgpu.BindGroupEntry{
            .{ .binding = 0, .buffer = self.uniform_buffer, .size = @sizeOf(Uniforms) },
            .{ .binding = 1, .sampler = self.sampler },
            .{ .binding = 2, .texture_view = self.atlas.view },
            .{ .binding = 3, .buffer = self.indirection_buffer, .size = self.indirection_buffer_capacity * @sizeOf(MultiAtlas.ImageMipData) },
        },
    });
    std.debug.assert(bg != null);
    self.bind_group = bg;
}
