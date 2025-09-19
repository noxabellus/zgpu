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
pub const Vec2 = struct { x: f32, y: f32 };
pub const Color = struct { r: f32, g: f32, b: f32, a: f32 };

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

const RenderCommand = union(enum) {
    textured_quad: struct {
        image_id: MultiAtlas.ImageId,
        pos: Vec2,
        size: Vec2,
        tint: Color,
    },
    text: struct {
        string: []const u8,
        font_info: *const stbtt.FontInfo,
        font_scale: f32,
        pos: Vec2,
        tint: Color,
    },
};

// --- WGPU Resources and State ---
allocator: std.mem.Allocator,
device: wgpu.Device,
queue: wgpu.Queue,
pipeline: wgpu.RenderPipeline,
uniform_buffer: wgpu.Buffer,
vertex_buffer: wgpu.Buffer,
vertex_buffer_capacity: usize,
vertices: std.array_list.Managed(Vertex),
multi_atlas: *MultiAtlas,
bind_groups: std.array_list.Managed(wgpu.BindGroup),
sampler: wgpu.Sampler,
command_queue: std.array_list.Managed(RenderCommand),

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
        .lod_max_clamp = @as(f32, @floatFromInt(multi_atlas.mip_level_count)),
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
        .vertices = .init(allocator),
        .multi_atlas = multi_atlas,
        .bind_groups = .init(allocator),
        .sampler = sampler,
        .command_queue = .init(allocator),
    };

    try self.vertices.ensureTotalCapacity(initial_capacity);
    try self.command_queue.ensureTotalCapacity(128);

    return self;
}

pub fn deinit(self: *Batch2D) void {
    self.multi_atlas.deinit();
    for (self.bind_groups.items) |bg| wgpu.bindGroupRelease(bg);
    self.bind_groups.deinit();
    if (self.vertex_buffer_capacity > 0) wgpu.bufferRelease(self.vertex_buffer);
    wgpu.bufferRelease(self.uniform_buffer);
    wgpu.samplerRelease(self.sampler);
    wgpu.renderPipelineRelease(self.pipeline);
    self.vertices.deinit();
    self.command_queue.deinit();
    self.allocator.destroy(self);
}

/// Prepares the renderer for a new frame. Must be called before any draw calls.
pub fn beginFrame(self: *Batch2D, projection: Mat4) void {
    wgpu.queueWriteBuffer(self.queue, self.uniform_buffer, 0, &Uniforms{ .projection = projection }, @sizeOf(Uniforms));
    self.vertices.clearRetainingCapacity();
    self.command_queue.clearRetainingCapacity();
}

/// A context object required by `prepare` to access application data.
pub const PrepareContext = struct {
    provider: MultiAtlas.DataProvider,
    provider_context: ?*anyopaque,
};

/// Processes the command queue, flushes atlases, generates vertices, and uploads
/// them to the GPU. This must be called after all draw calls and before `render`.
pub fn prepare(self: *Batch2D, context: PrepareContext) !void {
    if (self.command_queue.items.len == 0) return;

    try self.precacheImages(context);
    try self.flushAtlas(context);
    try self.generateVertices(context);

    // After generating vertices, upload them to the GPU buffer so it's ready for `render`.
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
/// This should be called after `prepare`.
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
    try self.command_queue.append(.{ .textured_quad = .{ .image_id = image_id, .pos = pos, .size = size, .tint = tint } });
}

pub fn drawText(self: *Batch2D, string: []const u8, font_info: *const stbtt.FontInfo, font_scale: f32, pos: Vec2, tint: Color) !void {
    try self.command_queue.append(.{ .text = .{ .string = string, .font_info = font_info, .font_scale = font_scale, .pos = pos, .tint = tint } });
}

// --- Internal Implementation ---

fn precacheImages(self: *Batch2D, context: PrepareContext) !void {
    for (self.command_queue.items) |cmd| {
        switch (cmd) {
            .textured_quad => |quad| {
                _ = self.multi_atlas.query(quad.image_id, context.provider, context.provider_context) catch |err| if (err != error.ImageNotYetPacked) return err;
            },
            .text => |text| {
                for (text.string) |char| {
                    const char_code = @as(u21, @intCast(char));

                    var ix0: i32 = 0;
                    var iy0: i32 = 0;
                    var ix1: i32 = 0;
                    var iy1: i32 = 0;
                    stbtt.getCodepointBitmapBox(text.font_info, @intCast(char_code), text.font_scale, text.font_scale, &ix0, &iy0, &ix1, &iy1);

                    if ((ix1 - ix0) > 0 and (iy1 - iy0) > 0) {
                        const glyph_id: MultiAtlas.ImageId = @intCast(FONT_ID_BASE + char_code);
                        _ = self.multi_atlas.query(glyph_id, context.provider, context.provider_context) catch |err| if (err != error.ImageNotYetPacked) return err;
                    }
                }
            },
        }
    }
}

const FONT_ID_BASE: MultiAtlas.ImageId = 0x1000;
fn glyphId(char: u21) MultiAtlas.ImageId {
    return FONT_ID_BASE + char;
}

fn flushAtlas(self: *Batch2D, context: PrepareContext) !void {
    if (self.multi_atlas.pending_chains.items.len == 0) return;

    const AppContext = @import("main.zig").AppContext;
    const app: *AppContext = @ptrCast(@alignCast(context.provider_context.?));
    const frame_allocator = app.frame_arena.allocator();

    for (self.multi_atlas.pending_chains.items) |*item| {
        if (item.chain.items.len > 1) continue;
        const base_image = item.chain.items[0];
        if (base_image.format != .rgba) continue;

        var current_w = base_image.width;
        var current_h = base_image.height;
        var last_pixels = base_image.pixels;

        while (@max(current_w, current_h) > 1 and item.chain.items.len < self.multi_atlas.mip_level_count) {
            const next_w = @max(1, current_w / 2);
            const next_h = @max(1, current_h / 2);

            const resized_pixels = try frame_allocator.alloc(u8, next_w * next_h * 4);
            @import("stbi").stbir_resize_uint8_srgb(
                last_pixels.ptr,
                @intCast(current_w),
                @intCast(current_h),
                0,
                resized_pixels.ptr,
                @intCast(next_w),
                @intCast(next_h),
                0,
                4,
            );

            try item.chain.append(.{
                .pixels = resized_pixels,
                .width = next_w,
                .height = next_h,
                .format = .rgba,
            });

            current_w = next_w;
            current_h = next_h;
            last_pixels = resized_pixels;
        }
    }

    try self.multi_atlas.flush();
}

fn generateVertices(self: *Batch2D, context: PrepareContext) !void {
    for (self.command_queue.items) |cmd| {
        switch (cmd) {
            .textured_quad => |quad| {
                const location = self.multi_atlas.query(quad.image_id, context.provider, context.provider_context) catch |err| {
                    log.err("image {any} was not packed after flush. error: {any}", .{ quad.image_id, err });
                    return err;
                };
                try self.pushTexturedQuad(location, quad.pos, quad.size, quad.tint);
            },
            .text => |text| {
                var max_y1_unscaled: i32 = 0;
                for (text.string) |char| {
                    const char_code = @as(u21, @intCast(char));
                    var x0: i32 = 0;
                    var y0: i32 = 0;
                    var x1: i32 = 0;
                    var y1: i32 = 0;
                    _ = stbtt.getCodepointBox(text.font_info, @intCast(char_code), &x0, &y0, &x1, &y1);
                    if (y1 > max_y1_unscaled) {
                        max_y1_unscaled = y1;
                    }
                }
                const final_ascent = @as(f32, @floatFromInt(max_y1_unscaled)) * text.font_scale;
                const baseline_y = text.pos.y + final_ascent;

                var xpos = text.pos.x;
                var prev_char: u21 = 0;

                for (text.string) |char| {
                    const char_code = @as(u21, @intCast(char));
                    if (prev_char != 0) {
                        const kern = stbtt.getCodepointKernAdvance(text.font_info, prev_char, char_code);
                        xpos += @as(f32, @floatFromInt(kern)) * text.font_scale;
                    }

                    var adv: i32 = 0;
                    var lsb: i32 = 0;
                    stbtt.getCodepointHMetrics(text.font_info, @intCast(char_code), &adv, &lsb);

                    var ix0: i32 = 0;
                    var iy0: i32 = 0;
                    var ix1: i32 = 0;
                    var iy1: i32 = 0;
                    stbtt.getCodepointBitmapBox(text.font_info, @intCast(char_code), text.font_scale, text.font_scale, &ix0, &iy0, &ix1, &iy1);

                    const char_width = @as(f32, @floatFromInt(ix1 - ix0));
                    const char_height = @as(f32, @floatFromInt(iy1 - iy0));

                    if (char_width > 0 and char_height > 0) {
                        const char_pos = Vec2{
                            .x = xpos + @as(f32, @floatFromInt(ix0)),
                            .y = baseline_y + @as(f32, @floatFromInt(iy0)),
                        };
                        const char_size = Vec2{ .x = char_width, .y = char_height };

                        const glyph_location = self.multi_atlas.query(glyphId(char_code), context.provider, context.provider_context) catch |err| {
                            log.err("glyph {c} was not packed after flush. error: {any}", .{ char, err });
                            return err;
                        };
                        try self.pushTexturedQuad(glyph_location, char_pos, char_size, text.tint);
                    }
                    xpos += @as(f32, @floatFromInt(adv)) * text.font_scale;
                    prev_char = char_code;
                }
            },
        }
    }
}

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

    try self.vertices.appendSlice(&[_]Vertex{
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
    if (idx < self.bind_groups.items.len) {
        return self.bind_groups.items[idx];
    }

    std.debug.assert(idx == self.bind_groups.items.len);

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

    try self.bind_groups.append(bg);
    return bg;
}
