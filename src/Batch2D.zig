//! A batched 2D renderer for shapes, images, and text.

const Batch2D = @This();

const std = @import("std");
const wgpu = @import("wgpu");
const stbtt = @import("stbtt");
const Atlas = @import("Atlas.zig");
const linalg = @import("linalg.zig");
const AssetCache = @import("AssetCache.zig");

const vec2 = linalg.vec2;
const mat4 = linalg.mat4;

const log = std.log.scoped(.renderer);

test {
    log.debug("semantic analysis for Renderer.zig", .{});
    std.testing.refAllDecls(@This());
}

// --- Public Constants ---
pub const GLYPH_PADDING = 2;
pub const GLYPH_PADDING_F = 2.0;

// --- Public API Structs ---
pub const ImageId = Atlas.ImageId;
pub const FontId = AssetCache.FontId;

pub const UvRect = struct { vec2, vec2 };
pub const Color = struct {
    r: f32 = 0.0,
    g: f32 = 0.0,
    b: f32 = 0.0,
    a: f32 = 0.0,

    pub const transparent = Color{ .r = 0.0, .g = 0.0, .b = 0.0, .a = 0.0 };
    pub const black = Color{ .r = 0.0, .g = 0.0, .b = 0.0, .a = 1.0 };
    pub const white = Color{ .r = 1.0, .g = 1.0, .b = 1.0, .a = 1.0 };
    pub const red = Color{ .r = 1.0, .g = 0.0, .b = 0.0, .a = 1.0 };
    pub const green = Color{ .r = 0.0, .g = 1.0, .b = 0.0, .a = 1.0 };
    pub const blue = Color{ .r = 0.0, .g = 0.0, .b = 1.0, .a = 1.0 };
    pub const cyan = Color{ .r = 0.0, .g = 1.0, .b = 1.0, .a = 1.0 };
    pub const magenta = Color{ .r = 1.0, .g = 0.0, .b = 1.0, .a = 1.0 };
    pub const yellow = Color{ .r = 1.0, .g = 1.0, .b = 0.0, .a = 1.0 };

    fn convertLinear(c: f32) f32 {
        if (c <= 0.04045) {
            return c / 12.92;
        } else {
            return std.math.pow(f32, (c + 0.055) / 1.055, 2.4);
        }
    }

    pub fn withAlpha(self: Color, new_alpha: f32) Color {
        return Color{ .r = self.r, .g = self.g, .b = self.b, .a = new_alpha };
    }

    pub fn init(r: f32, g: f32, b: f32, a: f32) Color {
        return Color{ .r = r, .g = g, .b = b, .a = a };
    }

    pub fn fromLinearU8(r: u8, g: u8, b: u8, a: u8) Color {
        @setEvalBranchQuota(10_000);
        const inv_255 = 1.0 / 255.0;
        return Color{
            .r = convertLinear(@as(f32, @floatFromInt(r)) * inv_255),
            .g = convertLinear(@as(f32, @floatFromInt(g)) * inv_255),
            .b = convertLinear(@as(f32, @floatFromInt(b)) * inv_255),
            .a = convertLinear(@as(f32, @floatFromInt(a)) * inv_255),
        };
    }

    pub fn lerp(a: Color, b: Color, amount: f32) Color {
        return Color{
            .r = a.r + (b.r - a.r) * amount,
            .g = a.g + (b.g - a.g) * amount,
            .b = a.b + (b.b - a.b) * amount,
            .a = a.a + (b.a - a.a) * amount,
        };
    }
};

pub const CornerRadius = struct {
    top_left: f32 = 0.0,
    top_right: f32 = 0.0,
    bottom_right: f32 = 0.0,
    bottom_left: f32 = 0.0,

    pub fn all(radius: f32) CornerRadius {
        return .{ .top_left = radius, .top_right = radius, .bottom_right = radius, .bottom_left = radius };
    }

    pub fn nonUniform(top_left: f32, top_right: f32, bottom_right: f32, bottom_left: f32) CornerRadius {
        return .{ .top_left = top_left, .top_right = top_right, .bottom_right = bottom_right, .bottom_left = bottom_left };
    }
};

pub const BorderWidth = struct {
    top: f32 = 0.0,
    right: f32 = 0.0,
    bottom: f32 = 0.0,
    left: f32 = 0.0,

    pub fn x(w: f32) BorderWidth {
        return .{ .top = 0, .right = w, .bottom = 0, .left = w };
    }

    pub fn y(h: f32) BorderWidth {
        return .{ .top = h, .right = 0, .bottom = h, .left = 0 };
    }

    pub fn xy(w: f32, h: f32) BorderWidth {
        return .{ .top = h, .right = w, .bottom = h, .left = w };
    }

    pub fn all(width: f32) BorderWidth {
        return .{ .top = width, .right = width, .bottom = width, .left = width };
    }

    pub fn nonUniform(top: f32, right: f32, bottom: f32, left: f32) BorderWidth {
        return .{ .top = top, .right = right, .bottom = bottom, .left = left };
    }

    pub fn withTop(self: *const BorderWidth, w: f32) BorderWidth {
        return .{ .top = w, .right = self.right, .bottom = self.bottom, .left = self.left };
    }

    pub fn withRight(self: *const BorderWidth, w: f32) BorderWidth {
        return .{ .top = self.top, .right = w, .bottom = self.bottom, .left = self.left };
    }

    pub fn withBottom(self: *const BorderWidth, w: f32) BorderWidth {
        return .{ .top = self.top, .right = self.right, .bottom = w, .left = self.left };
    }

    pub fn withLeft(self: *const BorderWidth, w: f32) BorderWidth {
        return .{ .top = self.top, .right = self.right, .bottom = self.bottom, .left = w };
    }

    pub fn withX(self: *const BorderWidth, w: f32) BorderWidth {
        return .{ .top = self.top, .right = w, .bottom = self.bottom, .left = w };
    }

    pub fn withY(self: *const BorderWidth, h: f32) BorderWidth {
        return .{ .top = h, .right = self.right, .bottom = h, .left = self.left };
    }
};

// --- Internal Structs ---
const USE_NEAREST_MASK: u32 = 0x80000000;
const IMAGE_ID_MASK: u32 = 0x7FFFFFFF;

const ScissorRect = extern struct {
    x: u32,
    y: u32,
    width: u32,
    height: u32,
};

const RenderBatch = struct {
    pipeline: PipelineKind,
    start_index: usize,
    count: usize,
    scissor: ScissorRect,
};

const Vertex = extern struct {
    position: [2]f32,
    tex_coords: [2]f32,
    color: [4]f32,
    /// Encoded rendering parameters passed to the shader.
    /// - Bit 31: Use Nearest Filter flag (for text, solid colors, etc.)
    /// - Bits 0-30: Logical Image ID (index into the indirection table storage buffer)
    encoded_params: u32,
};

const Quad = extern struct {
    position: [2]f32,
    size: [2]f32,
    uv_min: [2]f32,
    uv_max: [2]f32,
    color: [4]f32,
    radii: [4]f32,
    border_thickness: [4]f32,
    edge_softness: f32,
    /// Encoded rendering parameters passed to the shader.
    /// - Bit 31: Use Nearest Filter flag (for text, solid colors, etc.)
    /// - Bits 0-30: Logical Image ID (index into the indirection table storage buffer)
    encoded_params: u32,
};

const Uniforms = extern struct {
    projection: mat4,
};

const Patch = struct {
    kind: PipelineKind,
    image_id: Atlas.ImageId,
    start_index: usize,
    count: usize,
};

const PipelineKind = enum {
    tri,
    quad,
};

pub const DataProvider = Atlas.DataProvider;
pub const ProviderContext = Atlas.ProviderContext;

// --- WGPU Resources and State ---
allocator: std.mem.Allocator,
device: wgpu.Device,
queue: wgpu.Queue,
uniform_buffer: wgpu.Buffer,

tri_pipeline: wgpu.RenderPipeline,
quad_pipeline: wgpu.RenderPipeline,

// Viewport State
viewport_width: u32,
viewport_height: u32,

// Vertex Buffers
vertex_buffer: wgpu.Buffer,
vertex_buffer_capacity: usize,
vertex_staging_buffer: wgpu.Buffer,
vertex_staging_buffer_capacity: usize,
vertices: std.ArrayList(Vertex),

// Quad buffers
quad_buffer: wgpu.Buffer,
quad_buffer_capacity: usize,
quad_staging_buffer: wgpu.Buffer,
quad_staging_buffer_capacity: usize,
quads: std.ArrayList(Quad),

// Drawing State
asset_cache: *AssetCache,
atlas: *Atlas,
frame_arena: std.heap.ArenaAllocator,
provider_context: Atlas.ProviderContext,
bind_group: wgpu.BindGroup,
linear_sampler: wgpu.Sampler,
nearest_sampler: wgpu.Sampler,
patch_list: std.ArrayList(Patch),
batch_list: std.ArrayList(RenderBatch),
scissor_stack: std.ArrayList(ScissorRect),

current_pipeline: ?PipelineKind,
current_batch_start: usize,

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
    asset_cache: *AssetCache,
    sample_count: u32,
) !*Batch2D {
    const self = try allocator.create(Batch2D);
    errdefer allocator.destroy(self);

    const atlas = try Atlas.init(allocator, device, queue, 2048, 2048, Atlas.MAX_MIP_LEVELS);
    errdefer atlas.deinit();

    const tri_shader_module = try wgpu.loadShaderText(device, "static/shaders/2d/Triangles.wgsl", @embedFile("shaders/2d/Triangles.wgsl"));
    defer wgpu.shaderModuleRelease(tri_shader_module);

    const quad_shader_module = try wgpu.loadShaderText(device, "static/shaders/2d/Quads.wgsl", @embedFile("shaders/2d/Quads.wgsl"));
    defer wgpu.shaderModuleRelease(quad_shader_module);

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

    const initial_quad_capacity_bytes = 4096 * @sizeOf(Quad);
    const quad_staging_buffer = wgpu.deviceCreateBuffer(device, &.{
        .label = .fromSlice("quad_staging_buffer"),
        .usage = .{ .map_write = true, .copy_src = true },
        .size = initial_quad_capacity_bytes,
    });
    errdefer wgpu.bufferRelease(quad_staging_buffer);

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

    const tri_pipeline = wgpu.deviceCreateRenderPipeline(device, &wgpu.RenderPipelineDescriptor{
        .label = .fromSlice("batch2d_tri_render_pipeline"),
        .layout = pipeline_layout,
        .vertex = .{
            .module = tri_shader_module,
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
            .module = tri_shader_module,
            .entry_point = .fromSlice("fs_main"),
            .target_count = 1,
            .targets = &.{.{ .format = surface_format, .blend = &blend_state, .write_mask = .all }},
        },
        .primitive = .{ .topology = .triangle_list },
        .multisample = .{ .count = sample_count, .mask = 0xFFFFFFFF },
    });
    std.debug.assert(tri_pipeline != null);

    const quad_pipeline = wgpu.deviceCreateRenderPipeline(device, &wgpu.RenderPipelineDescriptor{
        .label = .fromSlice("batch2d_quad_render_pipeline"),
        .layout = pipeline_layout,
        .vertex = .{
            .module = quad_shader_module,
            .entry_point = .fromSlice("vs_main"),
            .buffer_count = 1,
            .buffers = &.{.{
                .array_stride = @sizeOf(Quad),
                .step_mode = .instance,
                .attribute_count = 9,
                .attributes = &[_]wgpu.VertexAttribute{
                    .{ .shaderLocation = 0, .offset = @offsetOf(Quad, "position"), .format = .float32x2 },
                    .{ .shaderLocation = 1, .offset = @offsetOf(Quad, "size"), .format = .float32x2 },
                    .{ .shaderLocation = 2, .offset = @offsetOf(Quad, "uv_min"), .format = .float32x2 },
                    .{ .shaderLocation = 3, .offset = @offsetOf(Quad, "uv_max"), .format = .float32x2 },
                    .{ .shaderLocation = 4, .offset = @offsetOf(Quad, "color"), .format = .float32x4 },
                    .{ .shaderLocation = 5, .offset = @offsetOf(Quad, "radii"), .format = .float32x4 },
                    .{ .shaderLocation = 6, .offset = @offsetOf(Quad, "border_thickness"), .format = .float32x4 },
                    .{ .shaderLocation = 7, .offset = @offsetOf(Quad, "edge_softness"), .format = .float32 },
                    .{ .shaderLocation = 8, .offset = @offsetOf(Quad, "encoded_params"), .format = .uint32 },
                },
            }},
        },
        .fragment = &wgpu.FragmentState{
            .module = quad_shader_module,
            .entry_point = .fromSlice("fs_main"),
            .target_count = 1,
            .targets = &.{.{ .format = surface_format, .blend = &blend_state, .write_mask = .all }},
        },
        .primitive = .{ .topology = .triangle_list },
        .multisample = .{ .count = sample_count, .mask = 0xFFFFFFFF },
    });
    std.debug.assert(tri_pipeline != null);

    self.* = .{
        .allocator = allocator,
        .device = device,
        .queue = queue,
        .tri_pipeline = tri_pipeline,
        .quad_pipeline = quad_pipeline,
        .uniform_buffer = uniform_buffer,
        .viewport_width = 0,
        .viewport_height = 0,
        .vertex_buffer = null,
        .vertex_buffer_capacity = 0,
        .vertex_staging_buffer = vertex_staging_buffer,
        .vertex_staging_buffer_capacity = initial_vertex_capacity_bytes,
        .vertices = .empty,
        .quad_buffer = null,
        .quad_buffer_capacity = 0,
        .quad_staging_buffer = quad_staging_buffer,
        .quad_staging_buffer_capacity = initial_quad_capacity_bytes,
        .quads = .empty,
        .asset_cache = asset_cache,
        .atlas = atlas,
        .frame_arena = std.heap.ArenaAllocator.init(allocator),
        .provider_context = .{
            .provider = AssetCache.dataProvider,
            .user_context = @constCast(asset_cache),
            .frame_allocator = undefined, // This will be set per-frame in beginFrame
        },
        .bind_group = null,
        .linear_sampler = linear_sampler,
        .nearest_sampler = nearest_sampler,
        .patch_list = .empty,
        .batch_list = .empty,
        .scissor_stack = .empty,
        .current_pipeline = null,
        .current_batch_start = 0,
        .indirection_buffer = indirection_buffer,
        .indirection_buffer_capacity = initial_indirection_capacity,
        .indirection_staging_buffer = indirection_staging_buffer,
        .indirection_staging_buffer_capacity = initial_indirection_capacity * @sizeOf(Atlas.ImageMipData),
    };

    try self.vertices.ensureTotalCapacity(self.allocator, 4096);
    try self.quads.ensureTotalCapacity(self.allocator, 4096);
    try self.patch_list.ensureTotalCapacity(self.allocator, 128);
    try self.batch_list.ensureTotalCapacity(self.allocator, 32);
    try self.scissor_stack.ensureTotalCapacity(self.allocator, 8);

    return self;
}

pub fn deinit(self: *Batch2D) void {
    self.atlas.deinit();
    if (self.bind_group != null) wgpu.bindGroupRelease(self.bind_group);
    if (self.vertex_buffer_capacity > 0) wgpu.bufferRelease(self.vertex_buffer);
    if (self.quad_buffer_capacity > 0) wgpu.bufferRelease(self.quad_buffer);
    wgpu.bufferRelease(self.vertex_staging_buffer);
    wgpu.bufferRelease(self.quad_staging_buffer);
    wgpu.bufferRelease(self.indirection_buffer);
    wgpu.bufferRelease(self.indirection_staging_buffer);
    wgpu.bufferRelease(self.uniform_buffer);
    wgpu.samplerRelease(self.linear_sampler);
    wgpu.samplerRelease(self.nearest_sampler);
    wgpu.renderPipelineRelease(self.tri_pipeline);
    self.vertices.deinit(self.allocator);
    self.quads.deinit(self.allocator);
    self.patch_list.deinit(self.allocator);
    self.batch_list.deinit(self.allocator);
    self.scissor_stack.deinit(self.allocator);
    self.frame_arena.deinit();
    self.allocator.destroy(self);
}

pub fn beginFrame(self: *Batch2D, projection: mat4, viewport_width: u32, viewport_height: u32) void {
    self.viewport_width = viewport_width;
    self.viewport_height = viewport_height;

    // Reset the frame arena allocator and update the provider context for this frame.
    // This provides a valid, temporary allocator for any assets that need to be
    // generated on-the-fly during this frame (e.g., new glyphs, mipmaps).
    _ = self.frame_arena.reset(.retain_capacity);
    self.provider_context.frame_allocator = self.frame_arena.allocator();

    wgpu.queueWriteBuffer(self.queue, self.uniform_buffer, 0, &Uniforms{ .projection = projection }, @sizeOf(Uniforms));
    self.vertices.clearRetainingCapacity();
    self.quads.clearRetainingCapacity();
    self.patch_list.clearRetainingCapacity();
    self.batch_list.clearRetainingCapacity();
    self.scissor_stack.clearRetainingCapacity();

    // Start with a default scissor rect that covers the whole screen.
    self.scissor_stack.appendAssumeCapacity(.{
        .x = 0,
        .y = 0,
        .width = viewport_width,
        .height = viewport_height,
    });

    self.current_pipeline = null;
    self.current_batch_start = 0;
}

fn endCurrentBatch(self: *Batch2D) !void {
    if (self.current_pipeline == null) return;

    const len = switch (self.current_pipeline.?) {
        .tri => self.vertices.items.len,
        .quad => self.quads.items.len,
    };

    const count = len - self.current_batch_start;

    if (count > 0) {
        std.debug.assert(self.scissor_stack.items.len > 0);
        const current_scissor = self.scissor_stack.items[self.scissor_stack.items.len - 1];
        try self.batch_list.append(self.allocator, .{
            .pipeline = self.current_pipeline.?,
            .start_index = self.current_batch_start,
            .count = count,
            .scissor = current_scissor,
        });
    }
}

fn ensurePipeline(self: *Batch2D, pipeline: PipelineKind) !void {
    if (self.current_pipeline != pipeline) {
        try self.endCurrentBatch();
        self.current_pipeline = pipeline;
        self.current_batch_start = switch (pipeline) {
            .tri => self.vertices.items.len,
            .quad => self.quads.items.len,
        };
    }
}

pub fn scissorStart(self: *Batch2D, pos: vec2, size: vec2) !void {
    // End the current batch of drawing with the old scissor rect
    try self.endCurrentBatch();

    // Get the current scissor rect to clip against
    std.debug.assert(self.scissor_stack.items.len > 0);
    const parent = self.scissor_stack.items[self.scissor_stack.items.len - 1];

    // Clamp and convert the requested rect to integer coordinates
    const new_x1 = @max(0, @as(i32, @intFromFloat(pos[0])));
    const new_y1 = @max(0, @as(i32, @intFromFloat(pos[1])));
    const new_x2 = @max(new_x1, @as(i32, @intFromFloat(pos[0] + size[0])));
    const new_y2 = @max(new_y1, @as(i32, @intFromFloat(pos[1] + size[1])));

    // Intersect with the parent rect
    const final_x1 = @max(@as(i32, @intCast(parent.x)), new_x1);
    const final_y1 = @max(@as(i32, @intCast(parent.y)), new_y1);
    const final_x2 = @min(@as(i32, @intCast(parent.x + parent.width)), new_x2);
    const final_y2 = @min(@as(i32, @intCast(parent.y + parent.height)), new_y2);

    // Push the new, clipped scissor rect onto the stack
    try self.scissor_stack.append(self.allocator, .{
        .x = @intCast(final_x1),
        .y = @intCast(final_y1),
        .width = @intCast(@max(0, final_x2 - final_x1)),
        .height = @intCast(@max(0, final_y2 - final_y1)),
    });
}

pub fn scissorEnd(self: *Batch2D) !void {
    // End the current batch of drawing with the old scissor rect
    try self.endCurrentBatch();

    // Pop the current scissor rect from the stack
    std.debug.assert(self.scissor_stack.items.len > 1); // Should not pop the root
    _ = self.scissor_stack.pop();
}

pub fn endFrame(self: *Batch2D) !void {
    // Finalize the last batch of the frame
    try self.endCurrentBatch();

    const atlas_recreated = try self.atlas.flush();

    for (self.patch_list.items) |p| {
        const location = self.atlas.query(p.image_id, self.provider_context) catch |err| {
            log.err("image {any} not found in cache after flush. error: {any}", .{ p.image_id, err });
            continue;
        };

        const decoded = AssetCache.decodeGlyphId(p.image_id);

        var use_nearest = false;

        if (decoded.is_glyph_or_special) {
            // It is a glyph or special ID
            if (decoded.font_id == AssetCache.SPECIAL_ID_font_id) {
                // White pixel always needs nearest neighbor
                use_nearest = true;
            } else {
                // Look up the font to see its preference
                const font = &self.asset_cache.fonts.items[decoded.font_id];
                use_nearest = (font.filter_mode == .nearest);
            }
        } else {
            // It is a standard image
            const image_idx = @as(usize, @intCast(p.image_id));
            if (image_idx < self.asset_cache.images.items.len) {
                // TODO: add filter_mode to LoadedImage too
                const img = &self.asset_cache.images.items[image_idx];
                _ = img; // unused for now
            }
        }

        const use_nearest_flag = if (use_nearest) USE_NEAREST_MASK else 0;
        const encoded_params = use_nearest_flag | (location.indirection_table_index & IMAGE_ID_MASK);

        switch (p.kind) {
            .tri => {
                const verts = self.vertices.items[p.start_index .. p.start_index + p.count];
                for (verts) |*v| v.encoded_params = encoded_params;
            },
            .quad => {
                const quads = self.quads.items[p.start_index .. p.start_index + p.count];
                for (quads) |*q| q.encoded_params = encoded_params;
            },
        }
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

    // Upload quad data if it has data
    if (self.quads.items.len > 0) {
        const required_size = self.quads.items.len * @sizeOf(Quad);

        // Resize final GPU buffer if needed
        if (self.quad_buffer_capacity < required_size) {
            if (self.quad_buffer_capacity > 0) wgpu.bufferRelease(self.quad_buffer);
            const new_capacity = @max(self.quad_buffer_capacity * 2, required_size);
            self.quad_buffer = wgpu.deviceCreateBuffer(self.device, &.{
                .label = .fromSlice("quad_buffer"),
                .usage = wgpu.BufferUsage{ .vertex = true, .copy_dst = true },
                .size = new_capacity,
            });
            self.quad_buffer_capacity = new_capacity;
            log.debug("resized quad buffer to {d} bytes", .{new_capacity});
        }

        // Resize staging buffer if needed
        if (self.quad_staging_buffer_capacity < required_size) {
            wgpu.bufferRelease(self.quad_staging_buffer);
            const new_capacity = @max(self.quad_staging_buffer_capacity * 2, required_size);
            self.quad_staging_buffer = wgpu.deviceCreateBuffer(self.device, &.{
                .label = .fromSlice("quad_staging_buffer"),
                .usage = wgpu.BufferUsage{ .map_write = true, .copy_src = true },
                .size = new_capacity,
            });
            self.quad_staging_buffer_capacity = new_capacity;
        }

        // Perform the upload
        try uploadSliceToBuffer(self.device, self.quad_staging_buffer, self.quads.items);
        wgpu.commandEncoderCopyBufferToBuffer(upload_encoder, self.quad_staging_buffer, 0, self.quad_buffer, 0, required_size);
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
    if (self.bind_group == null) return;

    wgpu.renderPassEncoderSetBindGroup(render_pass, 0, self.bind_group, 0, null);

    var bound_pipeline: ?PipelineKind = null;

    // Iterate through the batches and issue a draw call for each one with its specific scissor rect
    for (self.batch_list.items) |batch| {
        if (batch.count == 0) continue;

        if (bound_pipeline != batch.pipeline) {
            switch (batch.pipeline) {
                .tri => {
                    wgpu.renderPassEncoderSetPipeline(render_pass, self.tri_pipeline);
                    wgpu.renderPassEncoderSetVertexBuffer(render_pass, 0, self.vertex_buffer, 0, self.vertices.items.len * @sizeOf(Vertex));
                },
                .quad => {
                    wgpu.renderPassEncoderSetPipeline(render_pass, self.quad_pipeline);
                    wgpu.renderPassEncoderSetVertexBuffer(render_pass, 0, self.quad_buffer, 0, self.quads.items.len * @sizeOf(Quad));
                },
            }
            bound_pipeline = batch.pipeline;
        }

        wgpu.renderPassEncoderSetScissorRect(render_pass, batch.scissor.x, batch.scissor.y, batch.scissor.width, batch.scissor.height);
        switch (batch.pipeline) {
            .tri => wgpu.renderPassEncoderDraw(render_pass, @intCast(batch.count), 1, @intCast(batch.start_index), 0),
            .quad => wgpu.renderPassEncoderDraw(render_pass, 6, @intCast(batch.count), 0, @intCast(batch.start_index)),
        }
    }
}

/// Creates a new dynamic texture ID and reserves a dedicated layer for it in the atlas.
/// NOTE: Because this grows the atlas texture, it will trigger a pipeline bind group recreation.
pub fn createDynamicTexture(self: *Batch2D, width: u32, height: u32) !ImageId {
    const id = self.asset_cache.createDynamicId();

    try self.atlas.reserveDynamicLayer(id, width, height);

    // Growing the texture array invalidates the current bind group.
    // We must release it so it gets recreated before the next draw call.
    if (self.bind_group != null) {
        wgpu.bindGroupRelease(self.bind_group);
        self.bind_group = null;
    }

    return id;
}

/// Blits an offscreen render target into its reserved place in the atlas.
/// Handles dynamic resizing by updating the UV indirection table automatically.
pub fn updateDynamicTexture(self: *Batch2D, encoder: wgpu.CommandEncoder, id: ImageId, source_texture: wgpu.Texture, width: u32, height: u32) !void {
    // Look up the indirection table index assigned to this dynamic ID
    const cache_info = self.atlas.cache.get(id) orelse {
        log.err("Attempted to update a dynamic texture that wasn't reserved in the atlas.", .{});
        return error.DynamicTextureNotRegistered;
    };

    // Delegate the blit and UV mutation to the atlas
    try self.atlas.updateDynamicLayer(encoder, cache_info.indirection_table_index, source_texture, width, height);
}

/// Draws a filled rectangle. This is a convenience wrapper around drawQuad.
pub fn drawRect(self: *Batch2D, pos: vec2, size: vec2, tint: Color) !void {
    try self.pushTexturedQuad(AssetCache.WHITE_PIXEL_ID, pos, size, null, .{}, .all(0.0), tint);
}

/// Draws the outline of a rectangle with a specified thickness.
pub fn drawRectLine(self: *Batch2D, pos: vec2, size: vec2, thickness: BorderWidth, tint: Color) !void {
    // Ensure thickness isn't larger than the rectangle itself
    inline for (comptime std.meta.fieldNames(BorderWidth)) |fieldName| {
        const t = @min(@field(thickness, fieldName), @min(size[0] / 2.0, size[1] / 2.0));
        if (t <= 0.0) return;
    }

    try self.pushTexturedQuad(AssetCache.WHITE_PIXEL_ID, pos, size, null, .{}, thickness, tint);
}

/// Draws a filled rectangle with potentially different radii for each corner.
pub fn drawRoundedRect(self: *Batch2D, pos: vec2, size: vec2, radius: CornerRadius, tint: Color) !void {
    try self.pushTexturedQuad(AssetCache.WHITE_PIXEL_ID, pos, size, null, radius, .all(0.0), tint);
}

/// Draws the outline of a rectangle with potentially different radii for each corner,
/// drawn inwards from the specified boundary.
/// `pos` and `size` define the outer bounding box. `radius` defines the outer corner radii.
pub fn drawRoundedRectLine(self: *Batch2D, pos: vec2, size: vec2, radius: CornerRadius, thickness: BorderWidth, tint: Color) !void {
    // Ensure thickness isn't larger than the rectangle itself
    inline for (comptime std.meta.fieldNames(BorderWidth)) |fieldName| {
        const t = @min(@field(thickness, fieldName), @min(size[0] / 2.0, size[1] / 2.0));
        if (t <= 0.0) return;
    }

    try self.pushTexturedQuad(AssetCache.WHITE_PIXEL_ID, pos, size, null, radius, thickness, tint);
}

pub fn drawTexturedQuad(self: *Batch2D, image_id: Atlas.ImageId, pos: vec2, size: vec2, src_rect: ?UvRect, tint: Color) !void {
    try self.pushTexturedQuad(image_id, pos, size, src_rect, .{}, .all(0.0), tint);
}

/// Draws a textured quad with rounded corners, using a 9-slice method.
/// The `src_rect` defines the texture area, and `radius` defines the screen-space corner size.
pub fn drawRoundedTexturedQuad(self: *Batch2D, image_id: Atlas.ImageId, pos: vec2, size: vec2, radius: CornerRadius, src_rect: ?UvRect, tint: Color) !void {
    try self.pushTexturedQuad(image_id, pos, size, src_rect, radius, .all(0.0), tint);
}

pub fn drawTriangle(self: *Batch2D, v1: vec2, v2: vec2, v3: vec2, tint: Color) !void {
    // Solid triangles use the white pixel texture and have (0,0) UVs, which correctly maps to that single pixel.
    const uv = vec2{ 0.0, 0.0 };
    try self.pushTexturedTriangle(AssetCache.WHITE_PIXEL_ID, v1, uv, v2, uv, v3, uv, tint);
}

pub fn pushTexturedQuad(self: *Batch2D, image_id: Atlas.ImageId, pos: vec2, size: vec2, uv_rect: ?UvRect, corner_radius: CornerRadius, border_thickness: BorderWidth, tint: Color) !void {
    const uv_min, const uv_max = if (uv_rect) |r| r else .{ .{ 0, 0 }, .{ 1, 1 } };
    const radii = [4]f32{ @max(0.0, corner_radius.top_left), @max(0.0, corner_radius.top_right), @max(0.0, corner_radius.bottom_right), @max(0.0, corner_radius.bottom_left) };
    const thickness = [4]f32{ @max(0.0, border_thickness.top), @max(0.0, border_thickness.right), @max(0.0, border_thickness.bottom), @max(0.0, border_thickness.left) };

    const location = self.atlas.query(image_id, self.provider_context) catch |err| {
        if (err == error.ImageNotYetPacked) {
            // UNPACKED PATH: The image isn't in the atlas yet.
            // 1. Create a patch so we can fix `encoded_params` later.
            const quad_index = self.quads.items.len;
            try self.patch_list.append(self.allocator, .{
                .kind = .quad,
                .image_id = image_id,
                .start_index = quad_index,
                .count = 1,
            });
            // 2. Push a placeholder quad with `encoded_params = 0` and the ORIGINAL image-relative UVs.
            return self.pushQuad(0, pos, size, uv_min, uv_max, tint, radii, thickness);
        }
        return err;
    };

    // PACKED PATH: The image is in the atlas.
    // 1. Calculate the final `encoded_params` with the correct indirection table index.
    var use_nearest = false;
    const decoded = AssetCache.decodeGlyphId(image_id);
    if (decoded.is_glyph_or_special) {
        if (decoded.font_id == AssetCache.SPECIAL_ID_font_id) {
            use_nearest = true;
        } else {
            const font = &self.asset_cache.fonts.items[decoded.font_id];
            use_nearest = (font.filter_mode == .nearest);
        }
    }

    const use_nearest_flag = if (use_nearest) USE_NEAREST_MASK else 0;
    const encoded_params = use_nearest_flag | (location.indirection_table_index & IMAGE_ID_MASK);

    // 2. Push the triangle with the final `encoded_params` and the ORIGINAL image-relative UVs.
    try self.pushQuad(encoded_params, pos, size, uv_min, uv_max, tint, radii, thickness);
}

pub fn pushTexturedTriangle(self: *Batch2D, image_id: Atlas.ImageId, v1: vec2, uv1: vec2, v2: vec2, uv2: vec2, v3: vec2, uv3: vec2, tint: Color) !void {
    const location = self.atlas.query(image_id, self.provider_context) catch |err| {
        if (err == error.ImageNotYetPacked) {
            // UNPACKED PATH: The image isn't in the atlas yet.
            // 1. Create a patch so we can fix `encoded_params` later.
            const vertex_start_index = self.vertices.items.len;
            try self.patch_list.append(self.allocator, .{
                .kind = .tri,
                .image_id = image_id,
                .start_index = vertex_start_index,
                .count = 3,
            });
            // 2. Push a placeholder triangle with `encoded_params = 0` and the ORIGINAL image-relative UVs.
            return self.pushTriangle(0, v1, uv1, v2, uv2, v3, uv3, tint);
        }
        return err;
    };

    // PACKED PATH: The image is in the atlas.
    // 1. Calculate the final `encoded_params` with the correct indirection table index.
    var use_nearest = false;
    const decoded = AssetCache.decodeGlyphId(image_id);
    if (decoded.is_glyph_or_special) {
        if (decoded.font_id == AssetCache.SPECIAL_ID_font_id) {
            use_nearest = true;
        } else {
            const font = &self.asset_cache.fonts.items[decoded.font_id];
            use_nearest = (font.filter_mode == .nearest);
        }
    }

    const use_nearest_flag = if (use_nearest) USE_NEAREST_MASK else 0;
    const encoded_params = use_nearest_flag | (location.indirection_table_index & IMAGE_ID_MASK);

    // 2. Push the triangle with the final `encoded_params` and the ORIGINAL image-relative UVs.
    try self.pushTriangle(encoded_params, v1, uv1, v2, uv2, v3, uv3, tint);
}

pub fn drawLine(self: *Batch2D, p1: vec2, p2: vec2, thickness: f32, tint: Color) !void {
    const dx = p2[0] - p1[0];
    const dy = p2[1] - p1[1];
    const len = std.math.sqrt(dx * dx + dy * dy);
    if (len == 0) return;

    const nx = dx / len;
    const ny = dy / len;

    const px = -ny;
    const py = nx;

    const half_t = thickness / 2.0;

    const v1 = vec2{ p1[0] + px * half_t, p1[1] + py * half_t };
    const v2 = vec2{ p2[0] + px * half_t, p2[1] + py * half_t };
    const v3 = vec2{ p2[0] - px * half_t, p2[1] - py * half_t };
    const v4 = vec2{ p1[0] - px * half_t, p1[1] - py * half_t };

    try self.drawTriangle(v1, v2, v4, tint);
    try self.drawTriangle(v2, v3, v4, tint);
}

pub fn drawCircle(self: *Batch2D, center: vec2, radius: f32, tint: Color) !void {
    const num_segments = @max(12, @as(u32, @intFromFloat(radius / 1.5)));
    var i: u32 = 1;
    while (i <= num_segments) : (i += 1) {
        const angle1 = @as(f32, @floatFromInt(i - 1)) * (2.0 * std.math.pi) / @as(f32, @floatFromInt(num_segments));
        const angle2 = @as(f32, @floatFromInt(i)) * (2.0 * std.math.pi) / @as(f32, @floatFromInt(num_segments));
        const p1 = vec2{ center[0] + std.math.cos(angle1) * radius, center[1] + std.math.sin(angle1) * radius };
        const p2 = vec2{ center[0] + std.math.cos(angle2) * radius, center[1] + std.math.sin(angle2) * radius };
        try self.drawTriangle(center, p1, p2, tint);
    }
}

/// Draws a filled, pie-slice-shaped arc. Angles are in radians.
pub fn drawArc(self: *Batch2D, center: vec2, radius: f32, start_angle: f32, end_angle: f32, tint: Color) !void {
    if (radius <= 0.0) return;

    // Normalize angles to ensure we always draw counter-clockwise
    var normalized_end = end_angle;
    while (normalized_end < start_angle) {
        normalized_end += 2.0 * std.math.pi;
    }
    const total_angle = normalized_end - start_angle;
    if (total_angle <= 0.0) return;

    // Calculate the number of segments for a smooth curve.
    // This heuristic aims for each segment on the circumference to be ~1.5 pixels long.
    const num_segments = @max(1, @as(u32, @intFromFloat(total_angle * radius / 1.5)));
    const angle_step = total_angle / @as(f32, @floatFromInt(num_segments));

    var i: u32 = 0;
    while (i < num_segments) : (i += 1) {
        const angle1 = start_angle + @as(f32, @floatFromInt(i)) * angle_step;
        const angle2 = start_angle + @as(f32, @floatFromInt(i + 1)) * angle_step;

        const p1 = vec2{ center[0] + std.math.cos(angle1) * radius, center[1] + std.math.sin(angle1) * radius };
        const p2 = vec2{ center[0] + std.math.cos(angle2) * radius, center[1] + std.math.sin(angle2) * radius };

        try self.drawTriangle(center, p1, p2, tint);
    }
}

/// Draws the outline of an arc with a specified thickness, drawn inwards from the path.
/// The `radius` parameter defines the outer edge of the arc's stroke.
pub fn drawArcLine(self: *Batch2D, center: vec2, radius: f32, start_angle: f32, end_angle: f32, thickness: f32, tint: Color) !void {
    // Ensure thickness is positive and does not exceed the radius, creating an inversion.
    const t = @min(thickness, radius);
    if (radius <= 0.0 or t <= 0.0) return;

    // Normalize angles to ensure we always draw counter-clockwise
    var normalized_end = end_angle;
    while (normalized_end < start_angle) {
        normalized_end += 2.0 * std.math.pi;
    }
    const total_angle = normalized_end - start_angle;
    if (total_angle <= 0.0) return;

    // The outer radius is the one specified. The stroke is drawn inwards from there.
    const outer_radius = radius;
    const inner_radius = radius - t;

    const num_segments = @max(1, @as(u32, @intFromFloat(total_angle * outer_radius / 1.5)));
    const angle_step = total_angle / @as(f32, @floatFromInt(num_segments));

    var i: u32 = 0;
    while (i < num_segments) : (i += 1) {
        const angle1 = start_angle + @as(f32, @floatFromInt(i)) * angle_step;
        const angle2 = start_angle + @as(f32, @floatFromInt(i + 1)) * angle_step;

        const cos1 = std.math.cos(angle1);
        const sin1 = std.math.sin(angle1);
        const cos2 = std.math.cos(angle2);
        const sin2 = std.math.sin(angle2);

        const v_outer1 = vec2{ center[0] + cos1 * outer_radius, center[1] + sin1 * outer_radius };
        const v_inner1 = vec2{ center[0] + cos1 * inner_radius, center[1] + sin1 * inner_radius };
        const v_outer2 = vec2{ center[0] + cos2 * outer_radius, center[1] + sin2 * outer_radius };
        const v_inner2 = vec2{ center[0] + cos2 * inner_radius, center[1] + sin2 * inner_radius };

        // Create a quad for the segment
        try self.drawTriangle(v_outer1, v_outer2, v_inner1, tint);
        try self.drawTriangle(v_inner1, v_outer2, v_inner2, tint);
    }
}

/// Draws a textured, pie-slice-shaped arc. Angles are in radians.
pub fn drawTexturedArc(
    self: *Batch2D,
    image_id: Atlas.ImageId,
    center_pos: vec2,
    radius: f32,
    start_angle: f32,
    end_angle: f32,
    center_uv: vec2,
    radius_uv: vec2, // Use vec2 for potentially non-uniform UV radii
    tint: Color,
) !void {
    if (radius <= 0.0) return;

    var normalized_end = end_angle;
    while (normalized_end < start_angle) {
        normalized_end += 2.0 * std.math.pi;
    }
    const total_angle = normalized_end - start_angle;
    if (total_angle <= 0.0) return;

    const num_segments = @max(1, @as(u32, @intFromFloat(total_angle * radius / 1.5)));
    const angle_step = total_angle / @as(f32, @floatFromInt(num_segments));

    var i: u32 = 0;
    while (i < num_segments) : (i += 1) {
        const angle1 = start_angle + @as(f32, @floatFromInt(i)) * angle_step;
        const angle2 = start_angle + @as(f32, @floatFromInt(i + 1)) * angle_step;

        const cos1 = std.math.cos(angle1);
        const sin1 = std.math.sin(angle1);
        const cos2 = std.math.cos(angle2);
        const sin2 = std.math.sin(angle2);

        const p1 = vec2{ center_pos[0] + cos1 * radius, center_pos[1] + sin1 * radius };
        const p2 = vec2{ center_pos[0] + cos2 * radius, center_pos[1] + sin2 * radius };

        const uv1 = vec2{ center_uv[0] + cos1 * radius_uv[0], center_uv[1] + sin1 * radius_uv[1] };
        const uv2 = vec2{ center_uv[0] + cos2 * radius_uv[0], center_uv[1] + sin2 * radius_uv[1] };

        try self.pushTexturedTriangle(image_id, center_pos, center_uv, p1, uv1, p2, uv2, tint);
    }
}

pub fn drawSolidTriangleStrip(self: *Batch2D, vertices: []const vec2, tint: Color) !void {
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

// Helper struct to pass layout info from the generic layout function to the specific callbacks.
pub const GlyphLayoutInfo = struct {
    glyph_id: Atlas.ImageId,
    pos: vec2,
    size: vec2,
};

/// Generic text layout engine. Iterates through a string and calls a callback for each glyph's calculated position and size.
pub fn layoutText(
    self: *Batch2D,
    // Common text parameters
    string: []const u8,
    font_id: AssetCache.FontId,
    font_size: AssetCache.FontSize,
    line_spacing_override: ?u16,
    pos: vec2,

    // Generic callback mechanism
    comptime T: type,
    comptime E: type,
    context: *T,
    callback: fn (context: *T, info: GlyphLayoutInfo) E!void,
) (E || error{InvalidFontId})!void {
    // --- Font Info Lookup ---
    if (font_id >= self.asset_cache.fonts.items.len) {
        log.err("Invalid font_id {d} passed to layoutText", .{font_id});
        return error.InvalidFontId;
    }
    const font_info = &self.asset_cache.fonts.items[font_id].info;

    // --- Identical Setup ---
    const font_size_f32 = @as(f32, @floatFromInt(font_size));
    const font_scale_f32 = stbtt.scaleForPixelHeight(font_info, font_size_f32);
    const font_scale: f64 = font_scale_f32;

    var ascent: i32 = 0;
    var descent: i32 = 0;
    var line_gap: i32 = 0;
    stbtt.getFontVMetrics(font_info, &ascent, &descent, &line_gap);

    // --- LINE SPACING LOGIC ---
    // Use the override if provided, otherwise calculate from font metrics.
    const line_height: f64 =
        if (line_spacing_override) |spacing|
            @as(f64, @floatFromInt(spacing))
        else
            @as(f64, @floatFromInt(ascent - descent + line_gap)) * font_scale;

    // --- Layout Loop ---
    var baseline_y: f64 = pos[1] + (@as(f64, @floatFromInt(ascent)) * font_scale);
    var xpos: f64 = pos[0];
    var prev_char: u21 = 0;

    for (string) |char| {
        if (char == '\n') {
            xpos = pos[0];
            baseline_y += line_height;
            prev_char = 0;
            continue;
        }

        const char_code = @as(u21, @intCast(char));

        if (prev_char != 0) {
            const kern = stbtt.getCodepointKernAdvance(font_info, prev_char, char_code);
            xpos += @as(f64, @floatFromInt(kern)) * font_scale;
        }

        var ix0: i32 = 0;
        var iy0: i32 = 0;
        var ix1: i32 = 0;
        var iy1: i32 = 0;
        stbtt.getCodepointBitmapBox(font_info, char_code, font_scale_f32, font_scale_f32, &ix0, &iy0, &ix1, &iy1);

        const width: f64 = @floatFromInt(ix1 - ix0);
        const height: f64 = @floatFromInt(iy1 - iy0);

        if (width > 0 and height > 0) {
            const padded_width = width + (GLYPH_PADDING_F * 2.0);
            const padded_height = height + (GLYPH_PADDING_F * 2.0);

            const ideal_x = xpos + @as(f64, @floatFromInt(ix0)) - GLYPH_PADDING_F;
            const ideal_y = baseline_y + @as(f64, @floatFromInt(iy0)) - GLYPH_PADDING_F;

            const rounded_x = @floor(ideal_x + 0.5);
            const rounded_y = @floor(ideal_y + 0.5);

            // Create the layout info and pass it to the callback.
            const info = GlyphLayoutInfo{
                .pos = .{ @floatCast(rounded_x), @floatCast(rounded_y) },
                .size = .{ @floatCast(padded_width), @floatCast(padded_height) },
                .glyph_id = AssetCache.encodeGlyphId(.{
                    .char_code = char_code,
                    .font_size = font_size,
                    .font_id = font_id,
                    ._reserved = 0,
                    .is_glyph_or_special = true,
                }),
            };

            try callback(context, info);
        }

        var adv: i32 = 0;
        stbtt.getCodepointHMetrics(font_info, char_code, &adv, null);
        xpos += @as(f64, @floatFromInt(adv)) * font_scale;
        prev_char = char_code;
    }
}

/// Measures the pixel dimensions of a multi-line string when rendered with the specified font and size.
pub fn measureText(self: *Batch2D, string: []const u8, font_id: AssetCache.FontId, font_size: AssetCache.FontSize, line_spacing_override: ?u16) ?vec2 {
    if (string.len == 0) return .{ 0, 0 };

    // --- Font Info Lookup ---
    if (font_id >= self.asset_cache.fonts.items.len) {
        log.err("Invalid font_id {d} passed to measureText", .{font_id});
        return null;
    }
    const font_info = &self.asset_cache.fonts.items[font_id].info;

    // --- Font Metrics Setup ---
    const font_size_f32 = @as(f32, @floatFromInt(font_size));
    const font_scale_f32 = stbtt.scaleForPixelHeight(font_info, font_size_f32);
    const font_scale: f64 = font_scale_f32;

    var ascent: i32 = 0;
    var descent: i32 = 0;
    var line_gap: i32 = 0;
    stbtt.getFontVMetrics(font_info, &ascent, &descent, &line_gap);

    const line_height: f64 = if (line_spacing_override) |spacing|
        @as(f64, @floatFromInt(spacing))
    else
        @as(f64, @floatFromInt(ascent - descent + line_gap)) * font_scale;

    var current_pos = vec2{ 0, 0 };
    // The height of the first line is determined by the font's ascent/descent.
    var total_size = vec2{ 0, @floatCast(@as(f64, @floatFromInt(ascent - descent)) * font_scale) };
    if (total_size[1] == 0 and string.len > 0) {
        // Fallback for empty fonts or single-line cases
        total_size[1] = font_size_f32;
    }

    var prev_char: u21 = 0;

    for (string) |char| {
        if (char == '\n') {
            // Newline: update max width, reset horizontal pos, advance vertical pos.
            total_size[0] = @max(total_size[0], current_pos[0]);
            current_pos[0] = 0;
            total_size[1] += @floatCast(line_height);
            prev_char = 0;
            continue;
        }

        const char_code = @as(u21, @intCast(char));

        // Add kerning.
        if (prev_char != 0) {
            const kern = stbtt.getCodepointKernAdvance(font_info, prev_char, char_code);
            current_pos[0] += @floatCast(@as(f64, @floatFromInt(kern)) * font_scale);
        }

        // Add character advance width. This is the crucial part that correctly
        // handles spaces and other characters' full width.
        var adv: i32 = 0;
        stbtt.getCodepointHMetrics(font_info, char_code, &adv, null);
        current_pos[0] += @floatCast(@as(f64, @floatFromInt(adv)) * font_scale);

        prev_char = char_code;
    }

    // After the loop, update the max width one last time for the final line.
    total_size[0] = @max(total_size[0], current_pos[0]);

    // If the string was empty or contained only non-visible glyphs, total_size could still be zero.
    if (total_size[0] == 0 and total_size[1] == 0) return null;

    return total_size;
}

// Context for the drawing operation.
const DrawContext = struct {
    batch: *Batch2D,
    tint: Color,

    // Callback function to draw each glyph's quad.
    fn drawGlyph(self: *DrawContext, info: GlyphLayoutInfo) !void {
        try self.batch.drawTexturedQuad(info.glyph_id, info.pos, info.size, null, self.tint);
    }
};

/// Draws a formatted string of text, handling multiple lines.
pub fn formatText(self: *Batch2D, comptime fmt: []const u8, font_id: AssetCache.FontId, font_size: AssetCache.FontSize, line_spacing_override: ?u16, pos: vec2, tint: Color, args: anytype) !void {
    try self.drawText(
        try std.fmt.allocPrint(self.frame_arena.allocator(), fmt, args),
        font_id,
        font_size,
        line_spacing_override,
        pos,
        tint,
    );
}

/// Draws a string of text, handling multiple lines.
pub fn drawText(self: *Batch2D, string: []const u8, font_id: AssetCache.FontId, font_size: AssetCache.FontSize, line_spacing_override: ?u16, pos: vec2, tint: Color) !void {
    var context = DrawContext{
        .batch = self,
        .tint = tint,
    };

    // Use the generic layout engine with our drawing callback.
    try self.layoutText(
        string,
        font_id,
        font_size,
        line_spacing_override,
        pos,
        DrawContext,
        @typeInfo(@typeInfo(@TypeOf(DrawContext.drawGlyph)).@"fn".return_type.?).error_union.error_set,
        &context,
        DrawContext.drawGlyph,
    );
}

/// Pre-packs and uploads all images known to the AssetCache into the renderer's atlas.
/// This is intended to be called during a loading screen to "prime" the atlas and
/// prevent stuttering from on-the-fly atlasing during gameplay or interaction.
///
/// This function will repeatedly call the atlas's query and flush mechanisms until
/// all known images have been processed and uploaded to the GPU.
pub fn preAtlasAllImages(renderer: *Batch2D) !void {
    log.info("Beginning pre-atlasing of {d} images...", .{renderer.asset_cache.images.items.len});
    var timer = try std.time.Timer.start();

    // The Atlas needs a provider context to function. We'll create one here
    // using a temporary arena that will be cleaned up after this function returns.
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const provider_ctx = Atlas.ProviderContext{
        .provider = AssetCache.dataProvider,
        .frame_allocator = arena.allocator(),
        .user_context = @constCast(renderer.asset_cache),
    };

    // We need to iterate over all *logical* images, not the internal storage.
    // The image_map's value iterator gives us the ImageIds.
    var image_id_iterator = renderer.asset_cache.image_map.valueIterator();
    while (image_id_iterator.next()) |image_id_ptr| {
        // 1. Query the image. This will add it to the atlas's pending list.
        //    We expect it to return ImageNotYetPacked. If it returns anything
        //    else, that's either success (already packed) or an error.
        _ = renderer.atlas.query(image_id_ptr.*, provider_ctx) catch |err| {
            if (err != error.ImageNotYetPacked) {
                log.err("Error while querying image {d} for pre-atlasing: {any}", .{ image_id_ptr.*, err });
                return err;
            }
        };
    }

    // Now that all images are in the pending list, we flush.
    if (try renderer.atlas.flush()) {
        // If the flush caused the underlying texture to be recreated, we MUST
        // recreate the renderer's bind group to point to the new texture view.
        // This is a critical step that's normally handled in endFrame.
        log.warn("Atlas was recreated during pre-loading, recreating bind group.", .{});
        if (renderer.bind_group != null) {
            wgpu.bindGroupRelease(renderer.bind_group);
            renderer.bind_group = null;
        }
        try renderer.recreateBindGroup();
    }

    const duration_ms = @as(f64, @floatFromInt(timer.read())) / @as(f64, @floatFromInt(std.time.ns_per_ms));
    log.info("Finished pre-atlasing all images in {:.2}ms.", .{duration_ms});
}

fn pushQuad(self: *Batch2D, encoded_params: u32, pos: vec2, size: vec2, uv_min: vec2, uv_max: vec2, tint: Color, radii: [4]f32, border_thickness: [4]f32) !void {
    try self.ensurePipeline(.quad);

    // We can map 1.0 to pixel density later if we support DPI scaling
    const edge_softness = 1.0;

    try self.quads.append(self.allocator, .{
        .position = .{ pos[0], pos[1] },
        .size = .{ size[0], size[1] },
        .uv_min = .{ uv_min[0], uv_min[1] },
        .uv_max = .{ uv_max[0], uv_max[1] },
        .color = .{ tint.r, tint.g, tint.b, tint.a },
        .radii = radii,
        .border_thickness = border_thickness,
        .edge_softness = edge_softness,
        .encoded_params = encoded_params,
    });
}

fn pushTriangle(self: *Batch2D, encoded_params: u32, v1: vec2, uv1: vec2, v2: vec2, uv2: vec2, v3: vec2, uv3: vec2, tint: Color) !void {
    try self.ensurePipeline(.tri);

    try self.vertices.appendSlice(self.allocator, &[_]Vertex{
        .{ .position = .{ v1[0], v1[1] }, .tex_coords = .{ uv1[0], uv1[1] }, .color = .{ tint.r, tint.g, tint.b, tint.a }, .encoded_params = encoded_params },
        .{ .position = .{ v2[0], v2[1] }, .tex_coords = .{ uv2[0], uv2[1] }, .color = .{ tint.r, tint.g, tint.b, tint.a }, .encoded_params = encoded_params },
        .{ .position = .{ v3[0], v3[1] }, .tex_coords = .{ uv3[0], uv3[1] }, .color = .{ tint.r, tint.g, tint.b, tint.a }, .encoded_params = encoded_params },
    });
}

fn recreateBindGroup(self: *Batch2D) !void {
    const layout = wgpu.renderPipelineGetBindGroupLayout(self.tri_pipeline, 0);
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
