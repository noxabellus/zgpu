//! A batched 2D renderer for shapes, images, and text.
//!
//! This module provides a high-level API for drawing 2D primitives. It owns
//! the WGPU rendering pipeline and two separate MultiAtlas instances for specialized
//! texture management of images (with mipmaps) and glyphs (without mipmaps).
//! It uses the C-style functional wgpu-zig API.

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
const ATLAS_INDEX_MASK = 0x7FFFFFFF;
const IS_GLYPH_MASK = 0x80000000;

// --- Glyph ID Encoding ---
// We pack font index, pixel height, and char code into a 32-bit ID.
// Bit 31: is_glyph flag
// Bits 28-30 (3 bits): font_index (max 8 fonts)
// Bits 21-27 (7 bits): pixel_height (max 127px)
// Bits 0-20 (21 bits): char_code
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
    // MSB indicates if it's a glyph atlas. Lower bits are the page index.
    encoded_atlas_index: u32,
};

const Uniforms = extern struct {
    projection: Mat4,
};

// A command to patch vertex data after textures are uploaded.
const Patch = struct {
    image_id: MultiAtlas.ImageId,
    vertex_start_index: usize,
    vertex_count: usize,
};

/// A context object required by draw calls to access application data.
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
image_atlas: *MultiAtlas,
glyph_atlas: *MultiAtlas,
image_bind_groups: std.ArrayList(wgpu.BindGroup),
glyph_bind_groups: std.ArrayList(wgpu.BindGroup),
image_sampler: wgpu.Sampler,
glyph_sampler: wgpu.Sampler,
patch_list: std.ArrayList(Patch),
provider_context: ProviderContext,

pub fn init(
    allocator: std.mem.Allocator,
    device: wgpu.Device,
    queue: wgpu.Queue,
    surface_format: wgpu.TextureFormat,
    sample_count: u32,
) !*Batch2D {
    const self = try allocator.create(Batch2D);
    errdefer allocator.destroy(self);

    const image_atlas = try MultiAtlas.init(allocator, device, queue, 2048, 2048, 12); // With mips
    errdefer image_atlas.deinit();

    const glyph_atlas = try MultiAtlas.init(allocator, device, queue, 2048, 2048, 1); // No mips
    errdefer glyph_atlas.deinit();

    const shader_module = try wgpu.loadShaderText(device, "Renderer", @embedFile("shaders/Renderer.wgsl"));
    defer wgpu.shaderModuleRelease(shader_module);

    const uniform_buffer = wgpu.deviceCreateBuffer(device, &.{
        .label = .fromSlice("uniform_buffer"),
        .usage = wgpu.BufferUsage{ .uniform = true, .copy_dst = true },
        .size = @sizeOf(Uniforms),
    });
    std.debug.assert(uniform_buffer != null);
    errdefer wgpu.bufferRelease(uniform_buffer);

    // Sampler for images, with mipmapping.
    const image_sampler = wgpu.deviceCreateSampler(device, &.{
        .label = .fromSlice("image_texture_sampler"),
        .address_mode_u = .clamp_to_edge,
        .address_mode_v = .clamp_to_edge,
        .mag_filter = .linear,
        .min_filter = .linear,
        .mipmap_filter = .linear,
        .lod_min_clamp = 0.0,
        .lod_max_clamp = 12.0, // Match image atlas mip count
    });
    std.debug.assert(image_sampler != null);
    errdefer wgpu.samplerRelease(image_sampler);

    // Sampler for glyphs, with linear filtering but NO mipmapping.
    const glyph_sampler = wgpu.deviceCreateSampler(device, &.{
        .label = .fromSlice("glyph_texture_sampler"),
        .address_mode_u = .clamp_to_edge,
        .address_mode_v = .clamp_to_edge,
        .mag_filter = .linear,
        .min_filter = .linear,
    });
    std.debug.assert(glyph_sampler != null);
    errdefer wgpu.samplerRelease(glyph_sampler);

    const bind_group_layout = wgpu.deviceCreateBindGroupLayout(device, &.{
        .label = .fromSlice("bind_group_layout"),
        .entry_count = 3,
        .entries = &[_]wgpu.BindGroupLayoutEntry{
            .{ .binding = 0, .visibility = .vertexStage, .buffer = .{ .type = .uniform } },
            .{ .binding = 1, .visibility = .fragmentStage, .sampler = .{ .type = .filtering } },
            .{ .binding = 2, .visibility = .fragmentStage, .texture = .{ .sample_type = .float, .view_dimension = .@"2d" } },
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
                    .{ .shaderLocation = 3, .offset = @offsetOf(Vertex, "encoded_atlas_index"), .format = .uint32 },
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
        .image_atlas = image_atlas,
        .glyph_atlas = glyph_atlas,
        .image_bind_groups = .empty,
        .glyph_bind_groups = .empty,
        .image_sampler = image_sampler,
        .glyph_sampler = glyph_sampler,
        .patch_list = .empty,
        .provider_context = .{ .provider = undefined, .user_context = null },
    };

    try self.vertices.ensureTotalCapacity(self.allocator, initial_capacity);
    try self.patch_list.ensureTotalCapacity(self.allocator, 128);

    return self;
}

pub fn deinit(self: *Batch2D) void {
    self.image_atlas.deinit();
    self.glyph_atlas.deinit();
    for (self.image_bind_groups.items) |bg| wgpu.bindGroupRelease(bg);
    self.image_bind_groups.deinit(self.allocator);
    for (self.glyph_bind_groups.items) |bg| wgpu.bindGroupRelease(bg);
    self.glyph_bind_groups.deinit(self.allocator);
    if (self.vertex_buffer_capacity > 0) wgpu.bufferRelease(self.vertex_buffer);
    wgpu.bufferRelease(self.uniform_buffer);
    wgpu.samplerRelease(self.image_sampler);
    wgpu.samplerRelease(self.glyph_sampler);
    wgpu.renderPipelineRelease(self.pipeline);
    self.vertices.deinit(self.allocator);
    self.patch_list.deinit(self.allocator);
    self.allocator.destroy(self);
}

/// Prepares the renderer for a new frame. Must be called before any draw calls.
pub fn beginFrame(self: *Batch2D, projection: Mat4, context: ProviderContext) void {
    wgpu.queueWriteBuffer(self.queue, self.uniform_buffer, 0, &Uniforms{ .projection = projection }, @sizeOf(Uniforms));
    self.vertices.clearRetainingCapacity();
    self.patch_list.clearRetainingCapacity();
    self.provider_context = context;
}

/// Processes all pending images, patches vertex data, and uploads to the GPU.
/// This must be called after all draw calls and before `render`.
pub fn endFrame(self: *Batch2D) !void {
    // 1. Flush both atlases to upload any pending images.
    try self.image_atlas.flush(self.provider_context);
    try self.glyph_atlas.flush(self.provider_context);

    // 2. Process the patch list to fix vertex data for images that were just uploaded.
    for (self.patch_list.items) |p| {
        const is_glyph = (p.image_id & GLYPH_ID_FLAG) != 0;
        const atlas = if (is_glyph) self.glyph_atlas else self.image_atlas;

        // Query again, this time it must be a cache hit.
        const location = atlas.query(p.image_id, self.provider_context) catch |err| {
            log.err("image {any} not found in cache after flush. error: {any}", .{ p.image_id, err });
            continue;
        };

        const u_1 = location.uv_rect[0];
        const v_1 = location.uv_rect[1];
        const u_2 = location.uv_rect[2];
        const v_2 = location.uv_rect[3];
        const atlas_idx = @as(u32, @intCast(location.atlas_index));
        const encoded_index = if (is_glyph) (atlas_idx | IS_GLYPH_MASK) else atlas_idx;

        // This is a quad, so patch 6 vertices
        std.debug.assert(p.vertex_count == 6);
        const verts = self.vertices.items[p.vertex_start_index .. p.vertex_start_index + 6];
        verts[0].tex_coords = .{ u_1, v_1 };
        verts[1].tex_coords = .{ u_2, v_1 };
        verts[2].tex_coords = .{ u_1, v_2 };
        verts[3].tex_coords = .{ u_1, v_2 };
        verts[4].tex_coords = .{ u_2, v_1 };
        verts[5].tex_coords = .{ u_2, v_2 };

        for (verts) |*v| v.encoded_atlas_index = encoded_index;
    }

    // 3. Upload the final, correct vertex data to the GPU.
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
}

/// Records all necessary draw calls into a pre-existing render pass.
/// This should be called after `endFrame`.
pub fn render(self: *Batch2D, render_pass: wgpu.RenderPassEncoder) !void {
    if (self.vertices.items.len == 0) return;

    wgpu.renderPassEncoderSetPipeline(render_pass, self.pipeline);
    wgpu.renderPassEncoderSetVertexBuffer(render_pass, 0, self.vertex_buffer, 0, self.vertices.items.len * @sizeOf(Vertex));

    var current_encoded_index: u32 = std.math.maxInt(u32);
    var batch_start_vertex: u32 = 0;
    var valid_batch: bool = false;

    for (self.vertices.items, 0..) |vertex, i| {
        if (vertex.encoded_atlas_index != current_encoded_index) {
            const vertex_count: u32 = @intCast(i - batch_start_vertex);
            if (vertex_count > 0 and valid_batch) {
                wgpu.renderPassEncoderDraw(render_pass, vertex_count, 1, batch_start_vertex, 0);
            }
            current_encoded_index = vertex.encoded_atlas_index;
            const bind_group = self.getOrCreateBindGroup(current_encoded_index) catch {
                log.err("failed to get bind group for encoded atlas index {d}, skipping batch", .{current_encoded_index});
                valid_batch = false;
                continue;
            };
            valid_batch = true;
            wgpu.renderPassEncoderSetBindGroup(render_pass, 0, bind_group, 0, null);
            batch_start_vertex = @intCast(i);
        }
    }

    const final_vertex_count: u32 = @intCast(self.vertices.items.len - batch_start_vertex);
    if (final_vertex_count > 0 and valid_batch) {
        wgpu.renderPassEncoderDraw(render_pass, final_vertex_count, 1, batch_start_vertex, 0);
    }
}

pub fn drawTexturedQuad(self: *Batch2D, image_id: MultiAtlas.ImageId, pos: Vec2, size: Vec2, tint: Color) !void {
    const is_glyph = (image_id & GLYPH_ID_FLAG) != 0;
    const atlas = if (is_glyph) self.glyph_atlas else self.image_atlas;

    const location = atlas.query(image_id, self.provider_context) catch |err| {
        if (err == error.ImageNotYetPacked) {
            // Cache miss: add a patch request and generate placeholder vertices.
            const vertex_start_index = self.vertices.items.len;
            try self.patch_list.append(self.allocator, .{
                .image_id = image_id,
                .vertex_start_index = vertex_start_index,
                .vertex_count = 6,
            });
            // Use placeholder location for now.
            return self.pushTexturedQuad(.{ .atlas_index = 0, .uv_rect = .{ 0, 0, 0, 0 } }, is_glyph, pos, size, tint);
        }
        return err;
    };

    // Cache hit: generate vertices with the correct data immediately.
    try self.pushTexturedQuad(location, is_glyph, pos, size, tint);
}

pub fn drawText(self: *Batch2D, string: []const u8, font_info: *const stbtt.FontInfo, font_index: u32, pixel_height: f32, pos: Vec2, tint: Color) !void {
    const font_scale = stbtt.scaleForPixelHeight(font_info, pixel_height);

    // By calculating the baseline from the font's maximum ascent, text that doesn't
    // contain the tallest glyphs appears lower than it should. The fix is to perform
    // a pre-pass to find the actual ascent of the string being rendered.
    var min_iy0: i32 = 0;
    for (string) |char| {
        var ix0: i32 = 0;
        var iy0: i32 = 0;
        var ix1: i32 = 0;
        var iy1: i32 = 0;
        // Note: The y-coordinates returned by stb_truetype are measured from the
        // baseline, with positive y being downwards. The top of a glyph (iy0)
        // will therefore be a negative number.
        stbtt.getCodepointBitmapBox(font_info, @intCast(@as(u21, @intCast(char))), font_scale, font_scale, &ix0, &iy0, &ix1, &iy1);
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
        var lsb: i32 = 0;
        stbtt.getCodepointHMetrics(font_info, @intCast(char_code), &adv, &lsb);

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

            // 1. Calculate the ideal floating-point position for the glyph's top-left corner.
            const ideal_x = xpos + @as(f32, @floatFromInt(ix0)) - GLYPH_PADDING;
            const ideal_y = baseline_y + @as(f32, @floatFromInt(iy0)) - GLYPH_PADDING;

            // 2. Round that ideal position to the nearest integer grid point.
            const rounded_x = @floor(ideal_x + 0.5);
            const rounded_y = @floor(ideal_y + 0.5);

            // 3. CRITICAL: Adjust for the GPU's half-pixel rasterization rule.
            //    This aligns the pre-rasterized bitmap grid with the GPU's pixel centers.
            const final_pos = Vec2{
                .x = rounded_x - 0.5,
                .y = rounded_y - 0.5,
            };

            const char_size = Vec2{ .x = padded_width, .y = padded_height };
            const glyph_id = encodeGlyphId(font_index, pixel_height, char_code);
            try self.drawTexturedQuad(glyph_id, final_pos, char_size, tint);
        }

        // IMPORTANT: Advance the high-precision, un-rounded cursor position.
        xpos += @as(f32, @floatFromInt(adv)) * font_scale;
        prev_char = char_code;
    }
}

// --- Internal Implementation ---

fn pushTexturedQuad(self: *Batch2D, location: MultiAtlas.ImageLocation, is_glyph: bool, pos: Vec2, size: Vec2, tint: Color) !void {
    const x1 = pos.x;
    const y1 = pos.y;
    const x2 = pos.x + size.x;
    const y2 = pos.y + size.y;

    const u_1 = location.uv_rect[0];
    const v_1 = location.uv_rect[1];
    const u_2 = location.uv_rect[2];
    const v_2 = location.uv_rect[3];

    const page_index: u32 = @intCast(location.atlas_index);
    const encoded_index = if (is_glyph) (page_index | IS_GLYPH_MASK) else page_index;
    const c: [4]f32 = .{ tint.r, tint.g, tint.b, tint.a };

    try self.vertices.appendSlice(self.allocator, &[_]Vertex{
        .{ .position = .{ x1, y1 }, .tex_coords = .{ u_1, v_1 }, .color = c, .encoded_atlas_index = encoded_index },
        .{ .position = .{ x2, y1 }, .tex_coords = .{ u_2, v_1 }, .color = c, .encoded_atlas_index = encoded_index },
        .{ .position = .{ x1, y2 }, .tex_coords = .{ u_1, v_2 }, .color = c, .encoded_atlas_index = encoded_index },
        .{ .position = .{ x1, y2 }, .tex_coords = .{ u_1, v_2 }, .color = c, .encoded_atlas_index = encoded_index },
        .{ .position = .{ x2, y1 }, .tex_coords = .{ u_2, v_1 }, .color = c, .encoded_atlas_index = encoded_index },
        .{ .position = .{ x2, y2 }, .tex_coords = .{ u_2, v_2 }, .color = c, .encoded_atlas_index = encoded_index },
    });
}

fn getOrCreateBindGroup(self: *Batch2D, encoded_index: u32) !wgpu.BindGroup {
    const is_glyph = (encoded_index & IS_GLYPH_MASK) != 0;
    const page_index = @as(usize, @intCast(encoded_index & ATLAS_INDEX_MASK));

    const atlas = if (is_glyph) self.glyph_atlas else self.image_atlas;
    const bind_groups = if (is_glyph) &self.glyph_bind_groups else &self.image_bind_groups;
    const sampler = if (is_glyph) self.glyph_sampler else self.image_sampler;

    if (page_index < bind_groups.items.len and bind_groups.items[page_index] != null) {
        return bind_groups.items[page_index];
    }

    if (page_index >= bind_groups.capacity) {
        try bind_groups.resize(self.allocator, page_index + 1);
        @memset(bind_groups.items[page_index..], null);
    }
    bind_groups.items.len = @max(bind_groups.items.len, page_index + 1);

    const texture_view = atlas.getTextureView(page_index) orelse {
        log.err("multi-atlas has no texture view for index {d} (is_glyph={any})", .{ page_index, is_glyph });
        return error.InvalidAtlasIndex;
    };

    const layout = wgpu.renderPipelineGetBindGroupLayout(self.pipeline, 0);
    const bg = wgpu.deviceCreateBindGroup(self.device, &.{
        .label = .fromSlice("atlas_bind_group"),
        .layout = layout,
        .entry_count = 3,
        .entries = &[_]wgpu.BindGroupEntry{
            .{ .binding = 0, .buffer = self.uniform_buffer, .size = @sizeOf(Uniforms) },
            .{ .binding = 1, .sampler = sampler },
            .{ .binding = 2, .texture_view = texture_view },
        },
    });
    std.debug.assert(bg != null);

    bind_groups.items[page_index] = bg;
    return bg;
}
