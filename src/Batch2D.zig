//! A batched 2D renderer for shapes, images, and text.

const Batch2D = @This();

const std = @import("std");
const wgpu = @import("wgpu");
const stbtt = @import("stbtt");
const Atlas = @import("Atlas.zig");
const AssetCache = @import("AssetCache.zig");

const log = std.log.scoped(.renderer);

test {
    log.debug("semantic analysis for Renderer.zig", .{});
    std.testing.refAllDecls(@This());
}

// --- Public Constants ---
pub const GLYPH_PADDING = 2;
pub const GLYPH_PADDING_F = 2.0;

// --- Public API Structs ---
pub const Mat4 = [16]f32;
pub const Vec2 = struct { x: f32 = 0.0, y: f32 = 0.0 };
pub const Color = struct { r: f32 = 0.0, g: f32 = 0.0, b: f32 = 0.0, a: f32 = 0.0 };
pub const UvRect = struct { x: f32, y: f32, w: f32, h: f32 };

// --- Internal Structs ---
const USE_NEAREST_MASK: u32 = 0x80000000;
const IMAGE_ID_MASK: u32 = 0x7FFFFFFF;

const Vertex = extern struct {
    position: [2]f32,
    tex_coords: [2]f32,
    color: [4]f32,
    /// Encoded rendering parameters passed to the shader.
    /// - Bit 31: Use Nearest Filter flag (for text, solid colors, etc.)
    /// - Bits 0-30: Logical Image ID (index into the indirection table storage buffer)
    encoded_params: u32,
};

const Uniforms = extern struct {
    projection: Mat4,
};

const Patch = struct {
    image_id: Atlas.ImageId,
    vertex_start_index: usize,
    vertex_count: usize,
};

pub const DataProvider = Atlas.DataProvider;
pub const ProviderContext = Atlas.ProviderContext;

// --- WGPU Resources and State ---
allocator: std.mem.Allocator,
device: wgpu.Device,
queue: wgpu.Queue,
pipeline: wgpu.RenderPipeline,
uniform_buffer: wgpu.Buffer,

// Vertex Buffers
vertex_buffer: wgpu.Buffer,
vertex_buffer_capacity: usize,
vertex_staging_buffer: wgpu.Buffer,
vertex_staging_buffer_capacity: usize,

vertices: std.ArrayList(Vertex),
atlas: *Atlas,
bind_group: wgpu.BindGroup,
linear_sampler: wgpu.Sampler,
nearest_sampler: wgpu.Sampler,
patch_list: std.ArrayList(Patch),
provider_context: Atlas.ProviderContext,

// Indirection Table Buffers
indirection_buffer: wgpu.Buffer,
indirection_buffer_capacity: usize,
indirection_staging_buffer: wgpu.Buffer,
indirection_staging_buffer_capacity: usize,

pub fn init(
    allocator: std.mem.Allocator,
    device: wgpu.Device,
    queue: wgpu.Queue,
    surface_format: wgpu.TextureFormat,
    provider_context: Atlas.ProviderContext,
    sample_count: u32,
) !*Batch2D {
    const self = try allocator.create(Batch2D);
    errdefer allocator.destroy(self);

    const atlas = try Atlas.init(allocator, device, queue, 2048, 2048, Atlas.MAX_MIP_LEVELS);
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
        .size = initial_indirection_capacity * @sizeOf(Atlas.ImageMipData),
    });
    std.debug.assert(indirection_buffer != null);
    errdefer wgpu.bufferRelease(indirection_buffer);

    const indirection_staging_buffer = wgpu.deviceCreateBuffer(device, &.{
        .label = .fromSlice("indirection_staging_buffer"),
        .usage = .{ .map_write = true, .copy_src = true },
        .size = initial_indirection_capacity * @sizeOf(Atlas.ImageMipData),
    });
    std.debug.assert(indirection_staging_buffer != null);
    errdefer wgpu.bufferRelease(indirection_staging_buffer);

    const initial_vertex_capacity_bytes = 4096 * @sizeOf(Vertex);
    const vertex_staging_buffer = wgpu.deviceCreateBuffer(device, &.{
        .label = .fromSlice("vertex_staging_buffer"),
        .usage = .{ .map_write = true, .copy_src = true },
        .size = initial_vertex_capacity_bytes,
    });
    errdefer wgpu.bufferRelease(vertex_staging_buffer);

    const linear_sampler = wgpu.deviceCreateSampler(device, &.{
        .label = .fromSlice("linear_sampler"),
        .address_mode_u = .clamp_to_edge,
        .address_mode_v = .clamp_to_edge,
        .mag_filter = .linear,
        .min_filter = .linear,
    });
    std.debug.assert(linear_sampler != null);
    errdefer wgpu.samplerRelease(linear_sampler);

    const nearest_sampler = wgpu.deviceCreateSampler(device, &.{
        .label = .fromSlice("nearest_sampler"),
        .address_mode_u = .clamp_to_edge,
        .address_mode_v = .clamp_to_edge,
        .mag_filter = .nearest,
        .min_filter = .nearest,
    });
    std.debug.assert(nearest_sampler != null);
    errdefer wgpu.samplerRelease(nearest_sampler);

    const bind_group_layout = wgpu.deviceCreateBindGroupLayout(device, &.{
        .label = .fromSlice("bind_group_layout"),
        .entry_count = 5,
        .entries = &[_]wgpu.BindGroupLayoutEntry{
            .{ .binding = 0, .visibility = .vertexStage, .buffer = .{ .type = .uniform } },
            .{ .binding = 1, .visibility = .fragmentStage, .sampler = .{ .type = .filtering } }, // Linear
            .{ .binding = 2, .visibility = .fragmentStage, .sampler = .{ .type = .filtering } }, // Nearest
            .{ .binding = 3, .visibility = .fragmentStage, .texture = .{ .sample_type = .float, .view_dimension = .@"2d_array" } },
            .{ .binding = 4, .visibility = .fragmentStage, .buffer = .{ .type = .read_only_storage } },
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
        .vertex_buffer = null,
        .vertex_buffer_capacity = 0,
        .vertex_staging_buffer = vertex_staging_buffer,
        .vertex_staging_buffer_capacity = initial_vertex_capacity_bytes,
        .vertices = .empty,
        .atlas = atlas,
        .bind_group = null,
        .linear_sampler = linear_sampler,
        .nearest_sampler = nearest_sampler,
        .patch_list = .empty,
        .provider_context = provider_context,
        .indirection_buffer = indirection_buffer,
        .indirection_buffer_capacity = initial_indirection_capacity,
        .indirection_staging_buffer = indirection_staging_buffer,
        .indirection_staging_buffer_capacity = initial_indirection_capacity * @sizeOf(Atlas.ImageMipData),
    };

    try self.vertices.ensureTotalCapacity(self.allocator, initial_capacity);
    try self.patch_list.ensureTotalCapacity(self.allocator, 128);

    return self;
}

pub fn deinit(self: *Batch2D) void {
    self.atlas.deinit();
    if (self.bind_group != null) wgpu.bindGroupRelease(self.bind_group);
    if (self.vertex_buffer_capacity > 0) wgpu.bufferRelease(self.vertex_buffer);
    wgpu.bufferRelease(self.vertex_staging_buffer);
    wgpu.bufferRelease(self.indirection_buffer);
    wgpu.bufferRelease(self.indirection_staging_buffer);
    wgpu.bufferRelease(self.uniform_buffer);
    wgpu.samplerRelease(self.linear_sampler);
    wgpu.samplerRelease(self.nearest_sampler);
    wgpu.renderPipelineRelease(self.pipeline);
    self.vertices.deinit(self.allocator);
    self.patch_list.deinit(self.allocator);
    self.allocator.destroy(self);
}

pub fn beginFrame(self: *Batch2D, projection: Mat4) void {
    wgpu.queueWriteBuffer(self.queue, self.uniform_buffer, 0, &Uniforms{ .projection = projection }, @sizeOf(Uniforms));
    self.vertices.clearRetainingCapacity();
    self.patch_list.clearRetainingCapacity();
}

pub fn endFrame(self: *Batch2D) !void {
    const atlas_recreated = try self.atlas.flush();

    for (self.patch_list.items) |p| {
        const location = self.atlas.query(p.image_id, self.provider_context) catch |err| {
            log.err("image {any} not found in cache after flush. error: {any}", .{ p.image_id, err });
            continue;
        };

        const use_nearest_flag = if (AssetCache.decodeGlyphId(p.image_id).is_glyph_or_special) USE_NEAREST_MASK else 0;
        const encoded_params = use_nearest_flag | (location.indirection_table_index & IMAGE_ID_MASK);

        const verts = self.vertices.items[p.vertex_start_index .. p.vertex_start_index + p.vertex_count];
        for (verts) |*v| v.encoded_params = encoded_params;
    }

    // --- Upload data to GPU via staging buffers ---
    // We create a dedicated command encoder for these copy operations.
    // This allows the GPU to process data uploads in parallel with the CPU preparing the next frame.
    const upload_encoder = wgpu.deviceCreateCommandEncoder(self.device, &.{ .label = .fromSlice("staging_upload_encoder") });
    defer wgpu.commandEncoderRelease(upload_encoder);

    // Upload indirection table if it has data
    if (self.atlas.indirection_table.items.len > 0) {
        const required_capacity = self.atlas.indirection_table.items.len;
        const required_size = required_capacity * @sizeOf(Atlas.ImageMipData);

        // Resize final GPU buffer if needed
        if (self.indirection_buffer_capacity < required_capacity) {
            if (self.bind_group != null) {
                wgpu.bindGroupRelease(self.bind_group);
                self.bind_group = null;
            }
            wgpu.bufferRelease(self.indirection_buffer);
            const new_capacity = @max(self.indirection_buffer_capacity * 2, required_capacity);
            self.indirection_buffer = wgpu.deviceCreateBuffer(self.device, &.{
                .label = .fromSlice("indirection_buffer"),
                .usage = .{ .storage = true, .copy_dst = true },
                .size = new_capacity * @sizeOf(Atlas.ImageMipData),
            });
            self.indirection_buffer_capacity = new_capacity;
            log.info("resized indirection buffer to capacity {d}", .{new_capacity});
        }

        // Resize staging buffer if needed
        if (self.indirection_staging_buffer_capacity < required_size) {
            wgpu.bufferRelease(self.indirection_staging_buffer);
            const new_capacity = @max(self.indirection_staging_buffer_capacity * 2, required_size);
            self.indirection_staging_buffer = wgpu.deviceCreateBuffer(self.device, &.{
                .label = .fromSlice("indirection_staging_buffer"),
                .usage = .{ .map_write = true, .copy_src = true },
                .size = new_capacity,
            });
            self.indirection_staging_buffer_capacity = new_capacity;
        }

        // Perform the upload
        try uploadSliceToBuffer(self.device, self.indirection_staging_buffer, self.atlas.indirection_table.items);
        wgpu.commandEncoderCopyBufferToBuffer(upload_encoder, self.indirection_staging_buffer, 0, self.indirection_buffer, 0, required_size);
    }

    // Upload vertex data if it has data
    if (self.vertices.items.len > 0) {
        const required_size = self.vertices.items.len * @sizeOf(Vertex);

        // Resize final GPU buffer if needed
        if (self.vertex_buffer_capacity < required_size) {
            if (self.vertex_buffer_capacity > 0) wgpu.bufferRelease(self.vertex_buffer);
            const new_capacity = @max(self.vertex_buffer_capacity * 2, required_size);
            self.vertex_buffer = wgpu.deviceCreateBuffer(self.device, &.{
                .label = .fromSlice("vertex_buffer"),
                .usage = wgpu.BufferUsage{ .vertex = true, .copy_dst = true },
                .size = new_capacity,
            });
            self.vertex_buffer_capacity = new_capacity;
            log.debug("resized vertex buffer to {d} bytes", .{new_capacity});
        }

        // Resize staging buffer if needed
        if (self.vertex_staging_buffer_capacity < required_size) {
            wgpu.bufferRelease(self.vertex_staging_buffer);
            const new_capacity = @max(self.vertex_staging_buffer_capacity * 2, required_size);
            self.vertex_staging_buffer = wgpu.deviceCreateBuffer(self.device, &.{
                .label = .fromSlice("vertex_staging_buffer"),
                .usage = wgpu.BufferUsage{ .map_write = true, .copy_src = true },
                .size = new_capacity,
            });
            self.vertex_staging_buffer_capacity = new_capacity;
        }

        // Perform the upload
        try uploadSliceToBuffer(self.device, self.vertex_staging_buffer, self.vertices.items);
        wgpu.commandEncoderCopyBufferToBuffer(upload_encoder, self.vertex_staging_buffer, 0, self.vertex_buffer, 0, required_size);
    }

    // Submit the upload commands to the GPU.
    const upload_cmd = wgpu.commandEncoderFinish(upload_encoder, null);
    defer wgpu.commandBufferRelease(upload_cmd);
    wgpu.queueSubmit(self.queue, 1, &.{upload_cmd});

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

pub fn drawQuad(self: *Batch2D, pos: Vec2, size: Vec2, tint: Color) !void {
    try self.drawTexturedQuad(AssetCache.WHITE_PIXEL_ID, pos, size, null, tint);
}

pub fn drawTexturedQuad(self: *Batch2D, image_id: Atlas.ImageId, pos: Vec2, size: Vec2, src_rect: ?struct { Vec2, Vec2 }, tint: Color) !void {
    const p1 = pos; // Top-left
    const p2 = Vec2{ .x = pos.x + size.x, .y = pos.y }; // Top-right
    const p3 = Vec2{ .x = pos.x + size.x, .y = pos.y + size.y }; // Bottom-right
    const p4 = Vec2{ .x = pos.x, .y = pos.y + size.y }; // Bottom-left

    const tl, const br = if (src_rect) |rect| rect else .{
        Vec2{ .x = 0.0, .y = 0.0 },
        Vec2{ .x = 1.0, .y = 1.0 },
    };

    const tr = Vec2{ .x = br.x, .y = tl.y };
    const bl = Vec2{ .x = tl.x, .y = br.y };

    // Note the winding order for standard quads: TL, TR, BL and TR, BR, BL
    try self.drawTexturedTriangle(image_id, p1, tl, p2, tr, p4, bl, tint);
    try self.drawTexturedTriangle(image_id, p2, tr, p3, br, p4, bl, tint);
}

pub fn drawTriangle(self: *Batch2D, v1: Vec2, v2: Vec2, v3: Vec2, tint: Color) !void {
    // Solid triangles use the white pixel texture and have (0,0) UVs, which correctly maps to that single pixel.
    const uv = Vec2{ .x = 0.0, .y = 0.0 };
    try self.drawTexturedTriangle(AssetCache.WHITE_PIXEL_ID, v1, uv, v2, uv, v3, uv, tint);
}

pub fn drawTexturedTriangle(self: *Batch2D, image_id: Atlas.ImageId, v1: Vec2, uv1: Vec2, v2: Vec2, uv2: Vec2, v3: Vec2, uv3: Vec2, tint: Color) !void {
    const location = self.atlas.query(image_id, self.provider_context) catch |err| {
        if (err == error.ImageNotYetPacked) {
            // UNPACKED PATH: The image isn't in the atlas yet.
            // 1. Create a patch so we can fix `encoded_params` later.
            const vertex_start_index = self.vertices.items.len;
            try self.patch_list.append(self.allocator, .{
                .image_id = image_id,
                .vertex_start_index = vertex_start_index,
                .vertex_count = 3,
            });
            // 2. Push a placeholder triangle with `encoded_params = 0` and the ORIGINAL image-relative UVs.
            return self.pushTriangle(0, v1, uv1, v2, uv2, v3, uv3, tint);
        }
        return err;
    };

    // PACKED PATH: The image is in the atlas.
    // 1. Calculate the final `encoded_params` with the correct indirection table index.
    const use_nearest_flag = if (AssetCache.decodeGlyphId(image_id).is_glyph_or_special) USE_NEAREST_MASK else 0;
    const encoded_params = use_nearest_flag | (location.indirection_table_index & IMAGE_ID_MASK);

    // 2. Push the triangle with the final `encoded_params` and the ORIGINAL image-relative UVs.
    try self.pushTriangle(encoded_params, v1, uv1, v2, uv2, v3, uv3, tint);
}

pub fn drawLine(self: *Batch2D, p1: Vec2, p2: Vec2, thickness: f32, tint: Color) !void {
    const dx = p2.x - p1.x;
    const dy = p2.y - p1.y;
    const len = std.math.sqrt(dx * dx + dy * dy);
    if (len == 0) return;

    const nx = dx / len;
    const ny = dy / len;

    const px = -ny;
    const py = nx;

    const half_t = thickness / 2.0;

    const v1 = Vec2{ .x = p1.x + px * half_t, .y = p1.y + py * half_t };
    const v2 = Vec2{ .x = p2.x + px * half_t, .y = p2.y + py * half_t };
    const v3 = Vec2{ .x = p2.x - px * half_t, .y = p2.y - py * half_t };
    const v4 = Vec2{ .x = p1.x - px * half_t, .y = p1.y - py * half_t };

    try self.drawTriangle(v1, v2, v4, tint);
    try self.drawTriangle(v2, v3, v4, tint);
}

pub fn drawCircle(self: *Batch2D, center: Vec2, radius: f32, tint: Color) !void {
    const num_segments = @max(12, @as(u32, @intFromFloat(radius / 1.5)));
    var i: u32 = 1;
    while (i <= num_segments) : (i += 1) {
        const angle1 = @as(f32, @floatFromInt(i - 1)) * (2.0 * std.math.pi) / @as(f32, @floatFromInt(num_segments));
        const angle2 = @as(f32, @floatFromInt(i)) * (2.0 * std.math.pi) / @as(f32, @floatFromInt(num_segments));
        const p1 = Vec2{ .x = center.x + std.math.cos(angle1) * radius, .y = center.y + std.math.sin(angle1) * radius };
        const p2 = Vec2{ .x = center.x + std.math.cos(angle2) * radius, .y = center.y + std.math.sin(angle2) * radius };
        try self.drawTriangle(center, p1, p2, tint);
    }
}

pub fn drawSolidTriangleStrip(self: *Batch2D, vertices: []const Vec2, tint: Color) !void {
    if (vertices.len < 3) return;
    var i: usize = 0;
    while (i < vertices.len - 2) : (i += 1) {
        // Alternate winding order to form a strip from a list of vertices
        if (i % 2 == 0) {
            try self.drawTriangle(vertices[i], vertices[i + 1], vertices[i + 2], tint);
        } else {
            try self.drawTriangle(vertices[i + 1], vertices[i], vertices[i + 2], tint);
        }
    }
}

pub fn drawText(self: *Batch2D, string: []const u8, font_info: *const stbtt.FontInfo, font_id: AssetCache.FontId, font_size: AssetCache.FontSize, pos: Vec2, tint: Color) !void {
    // We still need the f32 versions for the stbtt API calls.
    const font_size_f32 = @as(f32, @floatFromInt(font_size));
    const font_scale_f32 = stbtt.scaleForPixelHeight(font_info, font_size_f32);
    const font_scale: f64 = font_scale_f32;

    // First, iterate through the string to find the highest-rising glyph.
    // This allows us to align the entire string's top boundary to pos.y.
    var min_iy0: i32 = 0;
    for (string) |char| {
        var iy0: i32 = 0;
        stbtt.getCodepointBitmapBox(font_info, @intCast(@as(u21, @intCast(char))), font_scale_f32, font_scale_f32, null, &iy0, null, null);
        min_iy0 = @min(min_iy0, iy0);
    }

    // Calculate the baseline's y-position in high precision.
    const baseline_y: f64 = @as(f64, pos.y) - @as(f64, @floatFromInt(min_iy0));
    // Initialize the high-precision horizontal cursor position.
    var xpos: f64 = pos.x;
    var prev_char: u21 = 0;

    for (string) |char| {
        const char_code = @as(u21, @intCast(char));

        // Apply kerning if this is not the first character.
        if (prev_char != 0) {
            const kern = stbtt.getCodepointKernAdvance(font_info, prev_char, char_code);
            xpos += @as(f64, @floatFromInt(kern)) * font_scale;
        }

        // Get glyph metrics (already scaled to integer pixel offsets by stbtt).
        var ix0: i32 = 0;
        var iy0: i32 = 0;
        var ix1: i32 = 0;
        var iy1: i32 = 0;
        stbtt.getCodepointBitmapBox(font_info, @intCast(char_code), font_scale_f32, font_scale_f32, &ix0, &iy0, &ix1, &iy1);

        const width: f64 = @floatFromInt(ix1 - ix0);
        const height: f64 = @floatFromInt(iy1 - iy0);

        if (width > 0 and height > 0) {
            const padded_width = width + (GLYPH_PADDING_F * 2.0);
            const padded_height = height + (GLYPH_PADDING_F * 2.0);

            // Calculate the ideal, un-rounded position for the glyph's quad in f64.
            const ideal_x = xpos + @as(f64, @floatFromInt(ix0)) - GLYPH_PADDING_F;
            const ideal_y = baseline_y + @as(f64, @floatFromInt(iy0)) - GLYPH_PADDING_F;

            // Round to the nearest whole pixel ("pixel snapping") for crisp rendering.
            const rounded_x = @floor(ideal_x + 0.5);
            const rounded_y = @floor(ideal_y + 0.5);

            // Convert to f32 only at the very end when creating the vertex data.
            const final_pos = Vec2{ .x = @floatCast(rounded_x), .y = @floatCast(rounded_y) };
            const char_size = Vec2{ .x = @floatCast(padded_width), .y = @floatCast(padded_height) };

            const glyph_id: Atlas.ImageId = AssetCache.encodeGlyphId(.{
                .char_code = char_code,
                .font_size = font_size,
                .font_id = @intCast(font_id),
                ._reserved = 0,
                .is_glyph_or_special = true,
            });

            try self.drawTexturedQuad(glyph_id, final_pos, char_size, null, tint);
        }

        // Advance the cursor for the next character.
        var adv: i32 = 0;
        stbtt.getCodepointHMetrics(font_info, @intCast(char_code), &adv, null);
        xpos += @as(f64, @floatFromInt(adv)) * font_scale;
        prev_char = char_code;
    }
}

fn pushTriangle(self: *Batch2D, encoded_params: u32, v1: Vec2, uv1: Vec2, v2: Vec2, uv2: Vec2, v3: Vec2, uv3: Vec2, tint: Color) !void {
    const c: [4]f32 = .{ tint.r, tint.g, tint.b, tint.a };
    try self.vertices.appendSlice(self.allocator, &[_]Vertex{
        .{ .position = .{ v1.x, v1.y }, .tex_coords = .{ uv1.x, uv1.y }, .color = c, .encoded_params = encoded_params },
        .{ .position = .{ v2.x, v2.y }, .tex_coords = .{ uv2.x, uv2.y }, .color = c, .encoded_params = encoded_params },
        .{ .position = .{ v3.x, v3.y }, .tex_coords = .{ uv3.x, uv3.y }, .color = c, .encoded_params = encoded_params },
    });
}

fn recreateBindGroup(self: *Batch2D) !void {
    const layout = wgpu.renderPipelineGetBindGroupLayout(self.pipeline, 0);
    const bg = wgpu.deviceCreateBindGroup(self.device, &.{
        .label = .fromSlice("atlas_array_bind_group"),
        .layout = layout,
        .entry_count = 5,
        .entries = &[_]wgpu.BindGroupEntry{
            .{ .binding = 0, .buffer = self.uniform_buffer, .size = @sizeOf(Uniforms) },
            .{ .binding = 1, .sampler = self.linear_sampler },
            .{ .binding = 2, .sampler = self.nearest_sampler },
            .{ .binding = 3, .texture_view = self.atlas.view },
            .{ .binding = 4, .buffer = self.indirection_buffer, .size = self.indirection_buffer_capacity * @sizeOf(Atlas.ImageMipData) },
        },
    });
    std.debug.assert(bg != null);
    self.bind_group = bg;
}

/// A helper function to upload a slice of data to a staging buffer, map it, copy the data, and unmap it.
/// This helper function contains a short, synchronous wait to acquire a memory pointer,
/// but it enables the much longer data transfer to happen completely asynchronously, which is what prevents rendering hiccups.
fn uploadSliceToBuffer(device: wgpu.Device, staging_buffer: wgpu.Buffer, slice: anytype) !void {
    const T = comptime @TypeOf(slice);
    const TInfo = comptime @typeInfo(T).pointer;

    if (comptime TInfo.size != .slice) @compileError("Batch2D.uploadSliceToBuffer input must be a slice type; got " ++ @typeName(T));

    const data_size: u64 = slice.len * @sizeOf(TInfo.child);
    var map_finished = false;
    var map_status: wgpu.MapAsyncStatus = .unknown;

    _ = wgpu.bufferMapAsync(staging_buffer, .writeMode, 0, data_size, .{
        .callback = &struct {
            fn handle_map(status: wgpu.MapAsyncStatus, _: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
                const finished_flag: *bool = @ptrCast(@alignCast(ud1.?));
                const status_ptr: *wgpu.MapAsyncStatus = @ptrCast(@alignCast(ud2.?));
                status_ptr.* = status;
                finished_flag.* = true;
            }
        }.handle_map,
        .userdata1 = &map_finished,
        .userdata2 = &map_status,
    });

    while (!map_finished) {
        _ = wgpu.devicePoll(device, .from(true), null);
    }

    if (map_status != .success) {
        log.err("failed to map staging buffer: {any}", .{map_status});
        return error.StagingBufferMapFailed;
    }

    const mapped_range: [*]u8 = @ptrCast(@alignCast(wgpu.bufferGetMappedRange(staging_buffer, 0, data_size) orelse {
        log.err("failed to get mapped range for staging buffer", .{});
        wgpu.bufferUnmap(staging_buffer);
        return error.StagingBufferMapFailed;
    }));

    const byte_slice = std.mem.sliceAsBytes(slice);
    @memcpy(mapped_range[0..byte_slice.len], byte_slice);
    wgpu.bufferUnmap(staging_buffer);
}
