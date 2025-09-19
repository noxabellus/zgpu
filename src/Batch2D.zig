//! A batched 2D renderer for shapes, images, and text.
//!
//! This module provides a high-level API for drawing 2D primitives. It owns
//! the WGPU rendering pipeline and a MultiAtlas instance for texture management.
//! It uses the C-style functional wgpu-zig API.

const Batch2D = @This();

const std = @import("std");
const wgpu = @import("wgpu");
const MultiAtlas = @import("MultiAtlas.zig");

const log = std.log.scoped(.renderer);

test {
    log.debug("semantic analysis for Renderer.zig", .{});
    std.testing.refAllDecls(@This());
}

// A simple 4x4 matrix for transformations.
pub const Mat4 = [16]f32;

// A 2D vector.
pub const Vec2 = struct { x: f32, y: f32 };
// A 4-component color (RGBA).
pub const Color = struct { r: f32, g: f32, b: f32, a: f32 };

/// The layout of data for a single vertex.
const Vertex = extern struct {
    position: [2]f32,
    tex_coords: [2]f32,
    color: [4]f32,
    /// The index of the texture atlas this vertex should be drawn with.
    /// This is used by the flush() logic to sort draw calls.
    atlas_index: u32,
};

/// Data uploaded to the GPU that is constant for an entire frame.
const Uniforms = extern struct {
    projection: Mat4,
};

// --- WGPU Resources and State ---
allocator: std.mem.Allocator,
device: wgpu.Device,
queue: wgpu.Queue,

pipeline: wgpu.RenderPipeline,
uniform_buffer: wgpu.Buffer,

/// The GPU-side buffer holding all vertex data for a batch.
/// This buffer is resized dynamically if it's too small for a frame.
vertex_buffer: wgpu.Buffer,
vertex_buffer_capacity: usize,

/// The CPU-side list of vertices accumulated during a frame.
vertices: std.array_list.Managed(Vertex),

/// The renderer owns the MultiAtlas system.
multi_atlas: *MultiAtlas,

/// A cache of bind groups, one for each atlas page in the MultiAtlas.
bind_groups: std.array_list.Managed(wgpu.BindGroup),
sampler: wgpu.Sampler,

/// Creates a new 2D batch renderer.
/// - `allocator`, `device`, `queue`: Standard setup components.
/// - `surface_format`: The format of the render target (e.g., the window's swapchain).
///   This is required to create a compatible render pipeline.
pub fn init(
    allocator: std.mem.Allocator,
    device: wgpu.Device,
    queue: wgpu.Queue,
    surface_format: wgpu.TextureFormat,
) !*Batch2D {
    const self = try allocator.create(Batch2D);
    errdefer allocator.destroy(self);

    // --- Initialize MultiAtlas ---
    const multi_atlas = try MultiAtlas.init(allocator, device, queue, 2048, 2048);
    errdefer multi_atlas.deinit();

    // --- Create WGPU Resources ---
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
        .mag_filter = .nearest,
        .min_filter = .nearest,
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
        .multisample = .{ .count = 1, .mask = 0xFFFFFFFF },
    });
    std.debug.assert(pipeline != null);

    // --- Initialize State ---
    const initial_capacity = 4096; // Start with capacity for ~680 quads
    self.* = .{
        .allocator = allocator,
        .device = device,
        .queue = queue,
        .pipeline = pipeline,
        .uniform_buffer = uniform_buffer,
        .vertex_buffer = undefined, // Created on first flush
        .vertex_buffer_capacity = 0,
        .vertices = .init(allocator),
        .multi_atlas = multi_atlas,
        .bind_groups = .init(allocator),
        .sampler = sampler,
    };

    try self.vertices.ensureTotalCapacity(initial_capacity);

    return self;
}

/// Releases all GPU and CPU resources used by the Renderer.
pub fn deinit(self: *Batch2D) void {
    self.multi_atlas.deinit();
    for (self.bind_groups.items) |bg| wgpu.bindGroupRelease(bg);
    self.bind_groups.deinit();
    if (self.vertex_buffer_capacity > 0) wgpu.bufferRelease(self.vertex_buffer);
    wgpu.bufferRelease(self.uniform_buffer);
    wgpu.samplerRelease(self.sampler);
    wgpu.renderPipelineRelease(self.pipeline);
    self.vertices.deinit();
    self.allocator.destroy(self);
}

/// Prepares the renderer for a new frame.
/// - `projection`: The orthographic projection matrix for the current screen size.
pub fn beginFrame(self: *Batch2D, projection: Mat4) void {
    wgpu.queueWriteBuffer(self.queue, self.uniform_buffer, 0, &Uniforms{ .projection = projection }, @sizeOf(Uniforms));
    self.vertices.clearRetainingCapacity();
}

/// Flushes all batched geometry to the GPU and executes the draw calls.
/// - `target_view`: The WGPU texture view to render to (e.g., the swapchain frame).
/// - `clear_color`: The color to clear the screen with, or null to not clear.
pub fn endFrame(self: *Batch2D, target_view: wgpu.TextureView, clear_color: ?Color) !void {
    if (self.vertices.items.len == 0) return;

    // --- Ensure GPU vertex buffer is large enough ---
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

    // --- Upload Vertex Data ---
    wgpu.queueWriteBuffer(self.queue, self.vertex_buffer, 0, self.vertices.items.ptr, required_size);

    // --- Record Render Commands ---
    const encoder = wgpu.deviceCreateCommandEncoder(self.device, null);
    defer wgpu.commandEncoderRelease(encoder);

    const load_op: wgpu.LoadOp = if (clear_color != null) .clear else .load;
    const clear_val = if (clear_color) |c| wgpu.Color{ .r = c.r, .g = c.g, .b = c.b, .a = c.a } else wgpu.Color{};

    const render_pass = wgpu.commandEncoderBeginRenderPass(encoder, &.{
        .color_attachment_count = 1,
        .color_attachments = &.{.{
            .view = target_view,
            .load_op = load_op,
            .store_op = .store,
            .clear_value = clear_val,
        }},
    });
    defer wgpu.renderPassEncoderRelease(render_pass);

    wgpu.renderPassEncoderSetPipeline(render_pass, self.pipeline);
    wgpu.renderPassEncoderSetVertexBuffer(render_pass, 0, self.vertex_buffer, 0, required_size);

    // --- The Batching Draw Loop ---
    var current_atlas: u32 = std.math.maxInt(u32); // Start with an invalid index
    var batch_start_vertex: u32 = 0;
    var valid_batch: bool = false;

    for (self.vertices.items, 0..) |vertex, i| {
        if (vertex.atlas_index != current_atlas) {
            // The atlas has changed, so we must draw the previous batch.
            const vertex_count: u32 = @intCast(i - batch_start_vertex);
            if (vertex_count > 0 and valid_batch) {
                wgpu.renderPassEncoderDraw(render_pass, vertex_count, 1, batch_start_vertex, 0);
            }
            // Switch to the new atlas's bind group.
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

    // Draw the final batch that remains after the loop.
    const final_vertex_count: u32 = @intCast(self.vertices.items.len - batch_start_vertex);
    if (final_vertex_count > 0 and valid_batch) {
        wgpu.renderPassEncoderDraw(render_pass, final_vertex_count, 1, batch_start_vertex, 0);
    }

    wgpu.renderPassEncoderEnd(render_pass);

    const cmd = wgpu.commandEncoderFinish(encoder, null);
    defer wgpu.commandBufferRelease(cmd);

    wgpu.queueSubmit(self.queue, 1, &.{cmd});
}

/// Adds a textured quad to the render batch.
/// - `location`: The image's location provided by the MultiAtlas.
/// - `pos`, `size`: The position and dimensions of the quad.
/// - `tint`: A color to multiply the texture by. Use white for no tint.
pub fn drawTexturedQuad(self: *Batch2D, location: MultiAtlas.ImageLocation, pos: Vec2, size: Vec2, tint: Color) !void {
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

    // We need to create a new bind group for a new atlas page.
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
