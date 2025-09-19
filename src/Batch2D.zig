//! A batched 2D renderer for shapes, images, and text.
//!
//! This module provides a high-level API for drawing 2D primitives. It owns
//! the WGPU rendering pipeline and a MultiAtlas instance for texture management.
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

// --- Public API Structs ---
pub const Mat4 = [16]f32;
pub const Vec2 = struct { x: f32 = 0.0, y: f32 = 0.0 };
pub const Color = struct { r: f32 = 0.0, g: f32 = 0.0, b: f32 = 0.0, a: f32 = 0.0 };
const FONT_ID_BASE: MultiAtlas.ImageId = 0x1000;

// --- Internal Structs ---

const Vertex = extern struct {
    position: [2]f32,
    tex_coords: [2]f32,
    color: [4]f32,
    atlas_index: u32,
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
multi_atlas: *MultiAtlas,
bind_groups: std.ArrayList(wgpu.BindGroup),
sampler: wgpu.Sampler,
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

    const multi_atlas = try MultiAtlas.init(allocator, device, queue, 2048, 2048);
    errdefer multi_atlas.deinit();

    const shader_module = try wgpu.loadShaderText(device, "Renderer", @embedFile("shaders/Renderer.wgsl"));
    defer wgpu.shaderModuleRelease(shader_module);

    const uniform_buffer = wgpu.deviceCreateBuffer(device, &.{
        .label = .fromSlice("uniform_buffer"),
        .usage = wgpu.BufferUsage{ .uniform = true, .copy_dst = true },
        .size = @sizeOf(Uniforms),
    });
    std.debug.assert(uniform_buffer != null);
    errdefer wgpu.bufferRelease(uniform_buffer);

    const sampler = wgpu.deviceCreateSampler(device, &.{
        .label = .fromSlice("texture_sampler"),
        .address_mode_u = .clamp_to_edge,
        .address_mode_v = .clamp_to_edge,
        .mag_filter = .linear,
        .min_filter = .linear,
        .mipmap_filter = .linear,
        .lod_min_clamp = 0.0,
        .lod_max_clamp = @as(f32, @floatFromInt(MultiAtlas.mip_level_count)),
    });
    std.debug.assert(sampler != null);
    errdefer wgpu.samplerRelease(sampler);

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
        .color = .{ .operation = .add, .src_factor = .src_alpha, .dst_factor = .one_minus_src_alpha },
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
                    .{ .shaderLocation = 3, .offset = @offsetOf(Vertex, "atlas_index"), .format = .uint32 },
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
        .multi_atlas = multi_atlas,
        .bind_groups = .empty,
        .sampler = sampler,
        .patch_list = .empty,
        .provider_context = .{ .provider = undefined, .user_context = null },
    };

    try self.vertices.ensureTotalCapacity(self.allocator, initial_capacity);
    try self.patch_list.ensureTotalCapacity(self.allocator, 128);

    return self;
}

pub fn deinit(self: *Batch2D) void {
    self.multi_atlas.deinit();
    for (self.bind_groups.items) |bg| wgpu.bindGroupRelease(bg);
    self.bind_groups.deinit(self.allocator);
    if (self.vertex_buffer_capacity > 0) wgpu.bufferRelease(self.vertex_buffer);
    wgpu.bufferRelease(self.uniform_buffer);
    wgpu.samplerRelease(self.sampler);
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
    // 1. Flush the atlas to upload any pending images.
    try self.multi_atlas.flush(self.provider_context);

    // 2. Process the patch list to fix vertex data for images that were just uploaded.
    for (self.patch_list.items) |p| {
        // Query again, this time it must be a cache hit.
        const location = self.multi_atlas.query(p.image_id, self.provider_context) catch |err| {
            log.err("image {any} not found in cache after flush. error: {any}", .{ p.image_id, err });
            continue;
        };

        const u_1 = location.uv_rect[0];
        const v_1 = location.uv_rect[1];
        const u_2 = location.uv_rect[2];
        const v_2 = location.uv_rect[3];
        const atlas_idx = @as(u32, @intCast(location.atlas_index));

        // This is a quad, so patch 6 vertices
        std.debug.assert(p.vertex_count == 6);
        const verts = self.vertices.items[p.vertex_start_index .. p.vertex_start_index + 6];
        verts[0].tex_coords = .{ u_1, v_1 };
        verts[1].tex_coords = .{ u_2, v_1 };
        verts[2].tex_coords = .{ u_1, v_2 };
        verts[3].tex_coords = .{ u_1, v_2 };
        verts[4].tex_coords = .{ u_2, v_1 };
        verts[5].tex_coords = .{ u_2, v_2 };

        for (verts) |*v| v.atlas_index = atlas_idx;
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

    var current_atlas: u32 = std.math.maxInt(u32);
    var batch_start_vertex: u32 = 0;
    var valid_batch: bool = false;

    for (self.vertices.items, 0..) |vertex, i| {
        if (vertex.atlas_index != current_atlas) {
            const vertex_count: u32 = @intCast(i - batch_start_vertex);
            if (vertex_count > 0 and valid_batch) {
                wgpu.renderPassEncoderDraw(render_pass, vertex_count, 1, batch_start_vertex, 0);
            }
            current_atlas = vertex.atlas_index;
            const bind_group = self.getOrCreateBindGroup(current_atlas) catch {
                log.err("failed to get bind group for atlas {d}, skipping batch", .{current_atlas});
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
    const location = self.multi_atlas.query(image_id, self.provider_context) catch |err| {
        if (err == error.ImageNotYetPacked) {
            // Cache miss: add a patch request and generate placeholder vertices.
            const vertex_start_index = self.vertices.items.len;
            try self.patch_list.append(self.allocator, .{
                .image_id = image_id,
                .vertex_start_index = vertex_start_index,
                .vertex_count = 6,
            });
            // Use placeholder location for now.
            return self.pushTexturedQuad(.{ .atlas_index = 0, .uv_rect = .{ 0, 0, 0, 0 } }, pos, size, tint);
        }
        return err;
    };

    // Cache hit: generate vertices with the correct data immediately.
    try self.pushTexturedQuad(location, pos, size, tint);
}

pub fn drawText(self: *Batch2D, string: []const u8, font_info: *const stbtt.FontInfo, font_scale: f32, pos: Vec2, tint: Color) !void {
    var max_y1_unscaled: i32 = 0;
    for (string) |char| {
        const char_code = @as(u21, @intCast(char));
        var x0: i32 = 0;
        var y0: i32 = 0;
        var x1: i32 = 0;
        var y1: i32 = 0;
        _ = stbtt.getCodepointBox(font_info, @intCast(char_code), &x0, &y0, &x1, &y1);
        if (y1 > max_y1_unscaled) max_y1_unscaled = y1;
    }
    const final_ascent = @as(f32, @floatFromInt(max_y1_unscaled)) * font_scale;
    const baseline_y = pos.y + final_ascent;

    var xpos = pos.x;
    var prev_char: u21 = 0;

    // This MUST match the MASTER_GLYPH_HEIGHT in main.zig.
    const MASTER_GLYPH_HEIGHT: f32 = 64.0;
    const master_scale = stbtt.scaleForPixelHeight(font_info, MASTER_GLYPH_HEIGHT);
    const scale_ratio = font_scale / master_scale;

    // This MUST match the GLYPH_PADDING in main.zig
    const GLYPH_PADDING: u32 = 2;

    for (string) |char| {
        // ... kerning and h-metrics code is unchanged ...
        const char_code = @as(u21, @intCast(char));
        if (prev_char != 0) {
            const kern = stbtt.getCodepointKernAdvance(font_info, prev_char, char_code);
            xpos += @as(f32, @floatFromInt(kern)) * font_scale;
        }

        var adv: i32 = 0;
        var lsb: i32 = 0;
        stbtt.getCodepointHMetrics(font_info, @intCast(char_code), &adv, &lsb);

        var master_ix0: i32 = 0;
        var master_iy0: i32 = 0;
        var master_ix1: i32 = 0;
        var master_iy1: i32 = 0;
        stbtt.getCodepointBitmapBox(font_info, @intCast(char_code), master_scale, master_scale, &master_ix0, &master_iy0, &master_ix1, &master_iy1);

        const master_width = master_ix1 - master_ix0;
        const master_height = master_iy1 - master_iy0;

        if (master_width > 0 and master_height > 0) {
            // The size of our quad must account for the padding on both sides.
            const padded_width_f = @as(f32, @floatFromInt(master_width + GLYPH_PADDING * 2)) * scale_ratio;
            const padded_height_f = @as(f32, @floatFromInt(master_height + GLYPH_PADDING * 2)) * scale_ratio;

            // The position must be shifted up and left by the padding amount to compensate for the larger size.
            const char_pos = Vec2{
                .x = xpos + (@as(f32, @floatFromInt(master_ix0 - @as(i32, @intCast(GLYPH_PADDING)))) * scale_ratio),
                .y = baseline_y + (@as(f32, @floatFromInt(master_iy0 - @as(i32, @intCast(GLYPH_PADDING)))) * scale_ratio),
            };

            const char_size = Vec2{ .x = padded_width_f, .y = padded_height_f };

            const glyph_id: MultiAtlas.ImageId = FONT_ID_BASE + char_code;
            try self.drawTexturedQuad(glyph_id, char_pos, char_size, tint);
        }

        xpos += @as(f32, @floatFromInt(adv)) * font_scale;
        prev_char = char_code;
    }
}

// --- Internal Implementation ---

fn pushTexturedQuad(self: *Batch2D, location: MultiAtlas.ImageLocation, pos: Vec2, size: Vec2, tint: Color) !void {
    const x1 = pos.x;
    const y1 = pos.y;
    const x2 = pos.x + size.x;
    const y2 = pos.y + size.y;

    const u_1 = location.uv_rect[0];
    const v_1 = location.uv_rect[1];
    const u_2 = location.uv_rect[2];
    const v_2 = location.uv_rect[3];

    const atlas_idx: u32 = @intCast(location.atlas_index);
    const c: [4]f32 = .{ tint.r, tint.g, tint.b, tint.a };

    try self.vertices.appendSlice(self.allocator, &[_]Vertex{
        .{ .position = .{ x1, y1 }, .tex_coords = .{ u_1, v_1 }, .color = c, .atlas_index = atlas_idx },
        .{ .position = .{ x2, y1 }, .tex_coords = .{ u_2, v_1 }, .color = c, .atlas_index = atlas_idx },
        .{ .position = .{ x1, y2 }, .tex_coords = .{ u_1, v_2 }, .color = c, .atlas_index = atlas_idx },
        .{ .position = .{ x1, y2 }, .tex_coords = .{ u_1, v_2 }, .color = c, .atlas_index = atlas_idx },
        .{ .position = .{ x2, y1 }, .tex_coords = .{ u_2, v_1 }, .color = c, .atlas_index = atlas_idx },
        .{ .position = .{ x2, y2 }, .tex_coords = .{ u_2, v_2 }, .color = c, .atlas_index = atlas_idx },
    });
}

fn getOrCreateBindGroup(self: *Batch2D, atlas_index: u32) !wgpu.BindGroup {
    const idx = @as(usize, @intCast(atlas_index));
    if (idx < self.bind_groups.items.len and self.bind_groups.items[idx] != null) {
        return self.bind_groups.items[idx];
    }

    if (idx >= self.bind_groups.capacity) {
        try self.bind_groups.resize(self.allocator, idx + 1);
        @memset(self.bind_groups.items[idx..], null);
    }
    self.bind_groups.items.len = @max(self.bind_groups.items.len, idx + 1);

    const texture_view = self.multi_atlas.getTextureView(idx) orelse {
        log.err("multi-atlas has no texture view for index {d}", .{idx});
        return error.InvalidAtlasIndex;
    };

    const layout = wgpu.renderPipelineGetBindGroupLayout(self.pipeline, 0);
    const bg = wgpu.deviceCreateBindGroup(self.device, &.{
        .label = .fromSlice("atlas_bind_group"),
        .layout = layout,
        .entry_count = 3,
        .entries = &[_]wgpu.BindGroupEntry{
            .{ .binding = 0, .buffer = self.uniform_buffer, .size = @sizeOf(Uniforms) },
            .{ .binding = 1, .sampler = self.sampler },
            .{ .binding = 2, .texture_view = texture_view },
        },
    });
    std.debug.assert(bg != null);

    self.bind_groups.items[idx] = bg;
    return bg;
}
