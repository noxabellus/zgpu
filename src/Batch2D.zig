//! A batched 2D renderer for shapes, images, and text.

const Batch2D = @This();

const std = @import("std");
const stbtt = @import("stbtt");
const Atlas = @import("Atlas.zig");
const Gpu = @import("Gpu.zig");
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

pub const MAX_CUSTOM_TEXTURES = 4;
const RenderBatch = struct {
    pipeline: u32,
    start_index: usize,
    count: usize,
    scissor: ScissorRect,
    uniform_data_offset: usize,
    uniform_data_size: usize,
    textures: [MAX_CUSTOM_TEXTURES]?*Gpu.TextureView,
};

const Vertex = extern struct {
    position: [2]f32,
    tex_coords: [2]f32,
    color: [4]f32,
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

pub const PipelineKind = enum {
    tri,
    quad,
};

pub const Pipeline = struct {
    kind: PipelineKind,
    gpu: *Gpu.RenderPipeline,
    texture_count: usize,
    uniform_size: usize,
    bind_group_layout: ?*Gpu.BindGroupLayout,
    cached_textures: [MAX_CUSTOM_TEXTURES]?*Gpu.TextureView,
    cached_custom_uniform_buffer: ?*Gpu.Buffer,
    cached_bind_group: ?*Gpu.BindGroup,

    pub fn getBindGroup(self: *Pipeline, device: *Gpu.Device, textures: [MAX_CUSTOM_TEXTURES]?*Gpu.TextureView, custom_uniform_buffer: ?*Gpu.Buffer) !?*Gpu.BindGroup {
        if (self.bind_group_layout == null) return null;

        if (self.cached_bind_group) |cbg| {
            var cache_valid = true;
            if (self.cached_custom_uniform_buffer != custom_uniform_buffer) cache_valid = false;

            for (0..self.texture_count) |i| {
                if (textures[i] != self.cached_textures[i]) {
                    cache_valid = false;
                    break;
                }
            }

            if (cache_valid) {
                return cbg;
            } else {
                cbg.release();
            }
        }

        var entries: [1 + MAX_CUSTOM_TEXTURES]Gpu.BindGroupEntry = undefined;
        var offset: u32 = 0;
        self.cached_textures = [1]?*Gpu.TextureView{null} ** MAX_CUSTOM_TEXTURES;
        self.cached_custom_uniform_buffer = custom_uniform_buffer;

        if (self.uniform_size > 0) {
            // Note: We bind the exact size of the struct, not the whole buffer.
            entries[offset] = .{ .binding = offset, .buffer = custom_uniform_buffer.?, .size = self.uniform_size };
            offset += 1;
        }

        for (0..self.texture_count) |i| {
            self.cached_textures[i] = textures[i];
            entries[offset] = .{ .binding = offset, .texture_view = textures[i] };
            offset += 1;
        }

        self.cached_bind_group = try device.createBindGroup(&.{
            .label = .fromSlice("Batch2D:custom_pipeline:custom_bind_group"),
            .layout = self.bind_group_layout,
            .entry_count = offset,
            .entries = &entries,
        });

        return self.cached_bind_group.?;
    }
};

pub const DataProvider = Atlas.DataProvider;
pub const ProviderContext = Atlas.ProviderContext;

// --- WGPU Resources and State ---
allocator: std.mem.Allocator,
gpu: *Gpu,
uniform_buffer: *Gpu.Buffer,
sample_count: u32,
surface_format: Gpu.TextureFormat,

// Viewport State
viewport_width: u32,
viewport_height: u32,

// Vertex Buffers
vertex_buffer: ?*Gpu.Buffer,
vertex_buffer_capacity: usize,
vertices: std.ArrayListUnmanaged(Vertex),

// Quad buffers
quad_buffer: ?*Gpu.Buffer,
quad_buffer_capacity: usize,
quads: std.ArrayListUnmanaged(Quad),

// Custom Uniform Buffers (Shared via dynamic offset)
custom_uniform_buffer: ?*Gpu.Buffer,
custom_uniform_buffer_capacity: usize,
uniform_data_storage: std.ArrayListUnmanaged(u8),

// Drawing State
asset_cache: *AssetCache,
atlas: *Atlas,
frame_arena: std.heap.ArenaAllocator,
provider_context: Atlas.ProviderContext,
bind_group: ?*Gpu.BindGroup,
linear_sampler: *Gpu.Sampler,
nearest_sampler: *Gpu.Sampler,
patch_list: std.ArrayListUnmanaged(Patch),
batch_list: std.ArrayListUnmanaged(RenderBatch),
scissor_stack: std.ArrayListUnmanaged(ScissorRect),

current_pipeline: ?u32,
current_batch_start: usize,
current_batch_textures: [MAX_CUSTOM_TEXTURES]?*Gpu.TextureView,
current_batch_uniform_start: usize,
current_batch_uniform_size: usize,
pipelines: std.ArrayListUnmanaged(Pipeline),

// Indirection Table Buffers
indirection_buffer: *Gpu.Buffer,
indirection_buffer_capacity: usize,

var BINDGROUP_LAYOUT: ?*Gpu.BindGroupLayout = null;
pub fn getBindGroupLayout(device: *Gpu.Device) !*Gpu.BindGroupLayout {
    if (BINDGROUP_LAYOUT == null) {
        BINDGROUP_LAYOUT = try device.createBindGroupLayout(&.{
            .label = .fromSlice("Batch2D:bind_group_layout"),
            .entry_count = 6,
            .entries = &[_]Gpu.BindGroupLayoutEntry{
                .{ .binding = 0, .visibility = .vertexStage, .buffer = .{ .type = .uniform } },
                .{ .binding = 1, .visibility = .fragmentStage, .sampler = .{ .type = .filtering } },
                .{ .binding = 2, .visibility = .fragmentStage, .sampler = .{ .type = .filtering } },
                .{ .binding = 3, .visibility = .fragmentStage, .sampler = .{ .type = .non_filtering } },
                .{ .binding = 4, .visibility = .fragmentStage, .texture = .{ .sample_type = .float, .view_dimension = .@"2d_array" } },
                .{ .binding = 5, .visibility = .fragmentStage, .buffer = .{ .type = .read_only_storage } },
            },
        });
        std.debug.assert(BINDGROUP_LAYOUT != null);
    }
    return BINDGROUP_LAYOUT.?;
}

var TRI_SHADER: ?*Gpu.ShaderModule = null;
pub fn getTriShader(device: *Gpu.Device) !*Gpu.ShaderModule {
    if (TRI_SHADER == null) {
        TRI_SHADER = try device.loadShaderText("static/shaders/2d/Triangles.wgsl", @embedFile("shaders/2d/Triangles.wgsl"));
    }
    return TRI_SHADER.?;
}

var QUAD_SHADER: ?*Gpu.ShaderModule = null;
pub fn getQuadShader(device: *Gpu.Device) !*Gpu.ShaderModule {
    if (QUAD_SHADER == null) {
        QUAD_SHADER = try device.loadShaderText("static/shaders/2d/Quads.wgsl", @embedFile("shaders/2d/Quads.wgsl"));
    }
    return QUAD_SHADER.?;
}

pub fn init(
    allocator: std.mem.Allocator,
    gpu: *Gpu,
    surface_format: Gpu.TextureFormat,
    asset_cache: *AssetCache,
    sample_count: u32,
) !*Batch2D {
    const self = try allocator.create(Batch2D);
    errdefer allocator.destroy(self);

    const atlas = try Atlas.init(allocator, gpu, 2048, 2048, Atlas.MAX_MIP_LEVELS);
    errdefer atlas.deinit();

    const uniform_buffer = try gpu.device.createBuffer(&.{
        .label = .fromSlice("Batch2D:uniform_buffer"),
        .usage = Gpu.BufferUsage{ .uniform = true, .copy_dst = true },
        .size = @sizeOf(Uniforms),
    });
    errdefer uniform_buffer.release();

    const initial_indirection_capacity = 2048;
    const indirection_buffer = try gpu.device.createBuffer(&.{
        .label = .fromSlice("Batch2D:indirection_buffer"),
        .usage = .{ .storage = true, .copy_dst = true },
        .size = initial_indirection_capacity * @sizeOf(Atlas.ImageMipData),
    });
    errdefer indirection_buffer.release();

    const linear_sampler = try gpu.device.createSampler(&.{
        .label = .fromSlice("Batch2D:linear_sampler"),
        .address_mode_u = .clamp_to_edge,
        .address_mode_v = .clamp_to_edge,
        .mag_filter = .linear,
        .min_filter = .linear,
    });
    errdefer linear_sampler.release();

    const nearest_sampler = try gpu.device.createSampler(&.{
        .label = .fromSlice("Batch2D:nearest_sampler"),
        .address_mode_u = .clamp_to_edge,
        .address_mode_v = .clamp_to_edge,
        .mag_filter = .nearest,
        .min_filter = .nearest,
    });
    errdefer nearest_sampler.release();

    self.* = .{
        .allocator = allocator,
        .gpu = gpu,
        .uniform_buffer = uniform_buffer,
        .sample_count = sample_count,
        .surface_format = surface_format,
        .viewport_width = 0,
        .viewport_height = 0,
        .vertex_buffer = null, // Created lazily
        .vertex_buffer_capacity = 0,
        .vertices = .empty,
        .quad_buffer = null, // Created lazily
        .quad_buffer_capacity = 0,
        .quads = .empty,
        .custom_uniform_buffer = null,
        .custom_uniform_buffer_capacity = 0,
        .uniform_data_storage = .empty,
        .asset_cache = asset_cache,
        .atlas = atlas,
        .frame_arena = std.heap.ArenaAllocator.init(allocator),
        .provider_context = .{
            .provider = AssetCache.dataProvider,
            .user_context = @constCast(asset_cache),
            .frame_allocator = undefined,
        },
        .bind_group = null,
        .linear_sampler = linear_sampler,
        .nearest_sampler = nearest_sampler,
        .patch_list = .empty,
        .batch_list = .empty,
        .scissor_stack = .empty,
        .current_pipeline = null,
        .pipelines = .empty,
        .current_batch_start = 0,
        .current_batch_textures = [1]?*Gpu.TextureView{null} ** MAX_CUSTOM_TEXTURES,
        .current_batch_uniform_start = 0,
        .current_batch_uniform_size = 0,
        .indirection_buffer = indirection_buffer,
        .indirection_buffer_capacity = initial_indirection_capacity,
    };

    try self.vertices.ensureTotalCapacity(self.allocator, 4096);
    try self.quads.ensureTotalCapacity(self.allocator, 4096);
    try self.patch_list.ensureTotalCapacity(self.allocator, 128);
    try self.batch_list.ensureTotalCapacity(self.allocator, 32);
    try self.scissor_stack.ensureTotalCapacity(self.allocator, 8);
    try self.pipelines.ensureTotalCapacity(self.allocator, 8);
    try self.uniform_data_storage.ensureTotalCapacity(self.allocator, 4096);

    _ = try self.createCustomPipeline("Batch2D:default_tri_pipeline", .tri, try getTriShader(gpu.device), &.{}, 0);
    _ = try self.createCustomPipeline("Batch2D:default_quad_pipeline", .quad, try getQuadShader(gpu.device), &.{}, 0);

    return self;
}

pub fn deinit(self: *Batch2D) void {
    self.atlas.deinit();
    if (self.bind_group) |bg| bg.release();
    if (self.vertex_buffer) |vb| vb.release();
    if (self.quad_buffer) |qb| qb.release();
    if (self.custom_uniform_buffer) |cub| cub.release();
    self.indirection_buffer.release();
    self.uniform_buffer.release();
    self.linear_sampler.release();
    self.nearest_sampler.release();

    for (self.pipelines.items) |pipeline| {
        pipeline.gpu.release();
        if (pipeline.cached_bind_group) |cbg| cbg.release();
    }

    self.pipelines.deinit(self.allocator);
    self.vertices.deinit(self.allocator);
    self.quads.deinit(self.allocator);
    self.patch_list.deinit(self.allocator);
    self.batch_list.deinit(self.allocator);
    self.scissor_stack.deinit(self.allocator);
    self.uniform_data_storage.deinit(self.allocator);
    self.frame_arena.deinit();
    self.allocator.destroy(self);
}

pub fn beginFrame(self: *Batch2D, projection: mat4, viewport_width: u32, viewport_height: u32) void {
    self.viewport_width = viewport_width;
    self.viewport_height = viewport_height;

    _ = self.frame_arena.reset(.retain_capacity);
    self.provider_context.frame_allocator = self.frame_arena.allocator();

    self.gpu.queue.writeBuffer(self.uniform_buffer, 0, &Uniforms{ .projection = projection }, @sizeOf(Uniforms));

    self.vertices.clearRetainingCapacity();
    self.quads.clearRetainingCapacity();
    self.patch_list.clearRetainingCapacity();
    self.batch_list.clearRetainingCapacity();
    self.scissor_stack.clearRetainingCapacity();
    self.uniform_data_storage.clearRetainingCapacity(); // Reset our dynamic offset arena

    self.scissor_stack.appendAssumeCapacity(.{
        .x = 0,
        .y = 0,
        .width = viewport_width,
        .height = viewport_height,
    });

    self.current_pipeline = null;
    self.current_batch_start = 0;
    self.current_batch_textures = [1]?*Gpu.TextureView{null} ** MAX_CUSTOM_TEXTURES;
    self.current_batch_uniform_start = 0;
    self.current_batch_uniform_size = 0;
}

fn endCurrentBatch(self: *Batch2D) !void {
    if (self.current_pipeline == null) return;

    const cp = self.pipelines.items[self.current_pipeline.?];

    const len = switch (cp.kind) {
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
            .textures = self.current_batch_textures,
            .uniform_data_offset = self.current_batch_uniform_start,
            .uniform_data_size = self.current_batch_uniform_size,
        });
    }

    self.current_pipeline = null;
}

pub fn createCustomPipeline(self: *Batch2D, name: []const u8, kind: PipelineKind, module: *Gpu.ShaderModule, texture_bindings: []const Gpu.TextureBindingLayout, uniform_size: usize) !u32 {
    const texture_count = texture_bindings.len;

    if (texture_count > 4) {
        log.err("Cannot bind more than 4 custom textures, got {d}", .{texture_count});
        return error.TooManyCustomTextures;
    }

    const blend_state = Gpu.BlendState{
        .color = .{ .operation = .add, .src_factor = .one, .dst_factor = .one_minus_src_alpha },
        .alpha = .{ .operation = .add, .src_factor = .one, .dst_factor = .one_minus_src_alpha },
    };

    const hasCustomUniforms = uniform_size > 0;

    const bindgroup_layout = if (texture_count > 0 or hasCustomUniforms) make_bgl: {
        var entries: [5]Gpu.BindGroupLayoutEntry = undefined;
        var offset: u32 = 0;
        if (hasCustomUniforms) {
            // Flag this binding to accept dynamic offsets
            entries[offset] = .{ .binding = offset, .visibility = .fragmentStage, .buffer = .{ .type = .uniform, .has_dynamic_offset = .from(true) } };
            offset += 1;
        }
        for (texture_bindings) |layout| {
            entries[offset] = .{ .binding = offset, .visibility = .fragmentStage, .texture = layout };
            offset += 1;
        }
        break :make_bgl try self.gpu.device.createBindGroupLayout(&.{
            .label = .fromSlice("Batch2D:custom_pipeline_bindgroup_layout"),
            .entry_count = offset,
            .entries = &entries,
        });
    } else null;

    const pipeline_layout = try self.gpu.device.createPipelineLayout(&.{
        .label = .fromSlice("Batch2D:pipeline_layout"),
        .bind_group_layout_count = 1 + @as(usize, @intFromBool(bindgroup_layout != null)),
        .bind_group_layouts = &.{ try getBindGroupLayout(self.gpu.device), bindgroup_layout },
    });
    defer pipeline_layout.release();

    const pipeline = switch (kind) {
        .tri => try self.gpu.device.createRenderPipeline(&Gpu.RenderPipelineDescriptor{
            .label = .fromSlice(name),
            .layout = pipeline_layout,
            .vertex = .{
                .module = try getTriShader(self.gpu.device),
                .entry_point = .fromSlice("vs_main"),
                .buffer_count = 1,
                .buffers = &.{.{
                    .array_stride = @sizeOf(Vertex),
                    .step_mode = .vertex,
                    .attribute_count = 4,
                    .attributes = &[_]Gpu.VertexAttribute{
                        .{ .shaderLocation = 0, .offset = @offsetOf(Vertex, "position"), .format = .float32x2 },
                        .{ .shaderLocation = 1, .offset = @offsetOf(Vertex, "tex_coords"), .format = .float32x2 },
                        .{ .shaderLocation = 2, .offset = @offsetOf(Vertex, "color"), .format = .float32x4 },
                        .{ .shaderLocation = 3, .offset = @offsetOf(Vertex, "encoded_params"), .format = .uint32 },
                    },
                }},
            },
            .fragment = &Gpu.FragmentState{
                .module = module,
                .entry_point = .fromSlice("fs_main"),
                .target_count = 1,
                .targets = &.{.{ .format = self.surface_format, .blend = &blend_state, .write_mask = .all }},
            },
            .primitive = .{ .topology = .triangle_list },
            .multisample = .{ .count = self.sample_count, .mask = 0xFFFFFFFF },
        }),
        .quad => try self.gpu.device.createRenderPipeline(&Gpu.RenderPipelineDescriptor{
            .label = .fromSlice(name),
            .layout = pipeline_layout,
            .vertex = .{
                .module = try getQuadShader(self.gpu.device),
                .entry_point = .fromSlice("vs_main"),
                .buffer_count = 1,
                .buffers = &.{.{
                    .array_stride = @sizeOf(Quad),
                    .step_mode = .instance,
                    .attribute_count = 9,
                    .attributes = &[_]Gpu.VertexAttribute{
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
            .fragment = &Gpu.FragmentState{
                .module = module,
                .entry_point = .fromSlice("fs_main"),
                .target_count = 1,
                .targets = &.{.{ .format = self.surface_format, .blend = &blend_state, .write_mask = .all }},
            },
            .primitive = .{ .topology = .triangle_list },
            .multisample = .{ .count = self.sample_count, .mask = 0xFFFFFFFF },
        }),
    };
    errdefer pipeline.release();

    const index: u32 = @intCast(self.pipelines.items.len);
    try self.pipelines.append(self.allocator, .{
        .kind = kind,
        .gpu = pipeline,
        .texture_count = texture_count,
        .bind_group_layout = bindgroup_layout,
        .cached_textures = [1]?*Gpu.TextureView{null} ** MAX_CUSTOM_TEXTURES,
        .cached_custom_uniform_buffer = null,
        .cached_bind_group = null,
        .uniform_size = uniform_size,
    });

    return index;
}

fn ensurePipeline(self: *Batch2D, pipeline: PipelineKind) !void {
    if (self.current_pipeline) |index| {
        const cp = self.pipelines.items[index];
        if (cp.kind == pipeline) return;
    }

    try self.endCurrentBatch();

    switch (pipeline) {
        .tri => {
            self.current_pipeline = 0;
            self.current_batch_start = self.vertices.items.len;
        },
        .quad => {
            self.current_pipeline = 1;
            self.current_batch_start = self.quads.items.len;
        },
    }

    self.current_batch_textures = [1]?*Gpu.TextureView{null} ** MAX_CUSTOM_TEXTURES;
    self.current_batch_uniform_start = 0;
    self.current_batch_uniform_size = 0;
}

pub fn customPipelineStart(self: *Batch2D, index: u32, textures: []const *Gpu.TextureView, uniform_buffer: anytype) !void {
    if (self.current_pipeline == index) return;

    try self.endCurrentBatch();

    const np = self.pipelines.items[index];

    if (textures.len != np.texture_count) {
        log.err("Pipeline at index {d} expects {d} textures, but got {d}", .{ index, np.texture_count, textures.len });
        return error.InvalidTextureCount;
    }

    self.current_batch_textures = [1]?*Gpu.TextureView{null} ** MAX_CUSTOM_TEXTURES;
    for (textures, 0..) |tex, i| {
        self.current_batch_textures[i] = tex;
    }
    self.current_pipeline = index;

    const T = @TypeOf(uniform_buffer);
    if (T == void) {
        self.current_batch_uniform_size = 0;
        self.current_batch_uniform_start = 0;
    } else {
        // Calculate required 256-byte alignment padding for dynamic offsets
        const alignment = 256;
        const current_len = self.uniform_data_storage.items.len;
        const padding = (alignment - (current_len % alignment)) % alignment;

        if (padding > 0) {
            try self.uniform_data_storage.appendNTimes(self.allocator, 0, padding);
        }

        self.current_batch_uniform_start = self.uniform_data_storage.items.len;

        const T_info = @typeInfo(T);
        if (T_info != .pointer) {
            self.current_batch_uniform_size = @sizeOf(T);
            try self.uniform_data_storage.appendSlice(self.allocator, std.mem.asBytes(&uniform_buffer));
        } else {
            switch (T_info.pointer.size) {
                .c, .many => @compileError("customPipelineStart only accepts single item pointer, slice, or value"),
                .one => {
                    self.current_batch_uniform_size = @sizeOf(T_info.pointer.child);
                    try self.uniform_data_storage.appendSlice(self.allocator, std.mem.asBytes(uniform_buffer));
                },
                .slice => {
                    self.current_batch_uniform_size = @sizeOf(T_info.pointer.child) * uniform_buffer.len;
                    try self.uniform_data_storage.appendSlice(self.allocator, std.mem.sliceAsBytes(uniform_buffer));
                },
            }
        }
    }

    switch (np.kind) {
        .tri => self.current_batch_start = self.vertices.items.len,
        .quad => self.current_batch_start = self.quads.items.len,
    }
}

pub fn customPipelineEnd(self: *Batch2D, index: u32) !void {
    if (self.current_pipeline != index) {
        log.err("customPipelineEnd called with invalid index {d}; current is {?d}", .{ index, self.current_pipeline });
        return;
    }

    try self.endCurrentBatch();
}

pub fn scissorStart(self: *Batch2D, pos: vec2, size: vec2) !void {
    try self.endCurrentBatch();

    std.debug.assert(self.scissor_stack.items.len > 0);
    const parent = self.scissor_stack.items[self.scissor_stack.items.len - 1];

    const new_x1 = @max(0, @as(i32, @intFromFloat(pos[0])));
    const new_y1 = @max(0, @as(i32, @intFromFloat(pos[1])));
    const new_x2 = @max(new_x1, @as(i32, @intFromFloat(pos[0] + size[0])));
    const new_y2 = @max(new_y1, @as(i32, @intFromFloat(pos[1] + size[1])));

    const final_x1 = @max(@as(i32, @intCast(parent.x)), new_x1);
    const final_y1 = @max(@as(i32, @intCast(parent.y)), new_y1);
    const final_x2 = @min(@as(i32, @intCast(parent.x + parent.width)), new_x2);
    const final_y2 = @min(@as(i32, @intCast(parent.y + parent.height)), new_y2);

    try self.scissor_stack.append(self.allocator, .{
        .x = @intCast(final_x1),
        .y = @intCast(final_y1),
        .width = @intCast(@max(0, final_x2 - final_x1)),
        .height = @intCast(@max(0, final_y2 - final_y1)),
    });
}

pub fn scissorEnd(self: *Batch2D) !void {
    try self.endCurrentBatch();
    std.debug.assert(self.scissor_stack.items.len > 1);
    _ = self.scissor_stack.pop();
}

pub fn endFrame(self: *Batch2D) !void {
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
            if (decoded.font_id == AssetCache.SPECIAL_ID_font_id) {
                use_nearest = true;
            } else {
                const font = &self.asset_cache.fonts.items[decoded.font_id];
                use_nearest = (font.filter_mode == .nearest);
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

    // --- Direct GPU Uploads via queueWriteBuffer ---

    if (self.atlas.indirection_table.items.len > 0) {
        const required_capacity = self.atlas.indirection_table.items.len;
        const required_size = required_capacity * @sizeOf(Atlas.ImageMipData);

        if (self.indirection_buffer_capacity < required_capacity) {
            if (self.bind_group) |bg| {
                bg.release();
                self.bind_group = null;
            }
            self.indirection_buffer.release();
            const new_capacity = @max(self.indirection_buffer_capacity * 2, required_capacity);
            self.indirection_buffer = try self.gpu.device.createBuffer(&.{
                .label = .fromSlice("Batch2D:indirection_buffer"),
                .usage = .{ .storage = true, .copy_dst = true },
                .size = new_capacity * @sizeOf(Atlas.ImageMipData),
            });
            self.indirection_buffer_capacity = new_capacity;
        }

        self.gpu.queue.writeBuffer(self.indirection_buffer, 0, self.atlas.indirection_table.items.ptr, required_size);
    }

    if (self.vertices.items.len > 0) {
        const required_size = self.vertices.items.len * @sizeOf(Vertex);

        if (self.vertex_buffer_capacity < required_size) {
            if (self.vertex_buffer) |vb| vb.release();
            const new_capacity = @max(self.vertex_buffer_capacity * 2, required_size);
            self.vertex_buffer = try self.gpu.device.createBuffer(&.{
                .label = .fromSlice("Batch2D:vertex_buffer"),
                .usage = Gpu.BufferUsage{ .vertex = true, .copy_dst = true },
                .size = new_capacity,
            });
            self.vertex_buffer_capacity = new_capacity;
        }

        self.gpu.queue.writeBuffer(self.vertex_buffer.?, 0, self.vertices.items.ptr, required_size);
    }

    if (self.quads.items.len > 0) {
        const required_size = self.quads.items.len * @sizeOf(Quad);

        if (self.quad_buffer_capacity < required_size) {
            if (self.quad_buffer) |qb| qb.release();
            const new_capacity = @max(self.quad_buffer_capacity * 2, required_size);
            self.quad_buffer = try self.gpu.device.createBuffer(&.{
                .label = .fromSlice("Batch2D:quad_buffer"),
                .usage = Gpu.BufferUsage{ .vertex = true, .copy_dst = true },
                .size = new_capacity,
            });
            self.quad_buffer_capacity = new_capacity;
        }

        self.gpu.queue.writeBuffer(self.quad_buffer.?, 0, self.quads.items.ptr, required_size);
    }

    if (self.uniform_data_storage.items.len > 0) {
        const required_size = self.uniform_data_storage.items.len;

        if (self.custom_uniform_buffer_capacity < required_size) {
            if (self.custom_uniform_buffer) |cub| cub.release();
            // Defaulting dynamic buffer minimum to a reasonable size
            const new_capacity = @max(self.custom_uniform_buffer_capacity * 2, @max(required_size, 4096));
            self.custom_uniform_buffer = try self.gpu.device.createBuffer(&.{
                .label = .fromSlice("Batch2D:custom_uniform_buffer"),
                .usage = Gpu.BufferUsage{ .uniform = true, .copy_dst = true },
                .size = new_capacity,
            });
            self.custom_uniform_buffer_capacity = new_capacity;
        }

        self.gpu.queue.writeBuffer(self.custom_uniform_buffer.?, 0, self.uniform_data_storage.items.ptr, required_size);
    }

    if (atlas_recreated and self.bind_group != null) {
        self.bind_group.?.release();
        self.bind_group = null;
    }

    if (self.bind_group == null) {
        try self.recreateBindGroup();
    }
}

pub fn render(self: *Batch2D, render_pass: *Gpu.RenderPassEncoder) !void {
    if (self.bind_group == null) {
        log.debug("Bind group is null, skipping render.", .{});
        return;
    }

    render_pass.setBindGroup(0, self.bind_group.?, &.{});

    var bound_pipeline: ?u32 = null;
    var bound_textures: [MAX_CUSTOM_TEXTURES]?*Gpu.TextureView = [1]?*Gpu.TextureView{null} ** MAX_CUSTOM_TEXTURES;
    var bound_uniform_offset: ?usize = null;

    for (self.batch_list.items) |batch| {
        if (batch.count == 0) continue;

        const bp = &self.pipelines.items[batch.pipeline];

        if (bound_pipeline != batch.pipeline) {
            render_pass.setPipeline(bp.gpu);
            switch (bp.kind) {
                .tri => render_pass.setVertexBuffer(0, self.vertex_buffer, 0, self.vertices.items.len * @sizeOf(Vertex)),
                .quad => render_pass.setVertexBuffer(0, self.quad_buffer, 0, self.quads.items.len * @sizeOf(Quad)),
            }
            bound_pipeline = batch.pipeline;

            // Forcing a bind group check by invalidating our local texture cache tracking
            bound_textures = [1]?*Gpu.TextureView{null} ** MAX_CUSTOM_TEXTURES;
        }

        // Check if textures or uniforms shifted from the last batch
        var requires_bind_update = false;

        if (bp.uniform_size > 0 and bound_uniform_offset != batch.uniform_data_offset) {
            requires_bind_update = true;
        } else {
            for (0..bp.texture_count) |i| {
                if (bound_textures[i] != batch.textures[i]) {
                    requires_bind_update = true;
                    break;
                }
            }
        }

        if (requires_bind_update) {
            if (try bp.getBindGroup(self.gpu.device, batch.textures, self.custom_uniform_buffer)) |cbg| {
                if (bp.uniform_size > 0) {
                    const offset = [1]u32{@intCast(batch.uniform_data_offset)};
                    render_pass.setBindGroup(1, cbg, &offset);
                } else {
                    render_pass.setBindGroup(1, cbg, &.{});
                }
            }
            bound_textures = batch.textures;
            bound_uniform_offset = batch.uniform_data_offset;
        }

        render_pass.setScissorRect(batch.scissor.x, batch.scissor.y, batch.scissor.width, batch.scissor.height);
        switch (bp.kind) {
            .tri => render_pass.draw(@intCast(batch.count), 1, @intCast(batch.start_index), 0),
            .quad => render_pass.draw(6, @intCast(batch.count), 0, @intCast(batch.start_index)),
        }
    }
}

pub fn createDynamicTexture(self: *Batch2D, width: u32, height: u32) !ImageId {
    const id = self.asset_cache.createDynamicId();
    try self.atlas.reserveDynamicLayer(id, width, height);
    if (self.bind_group) |bg| {
        bg.release();
        self.bind_group = null;
    }
    return id;
}

pub fn updateDynamicTexture(self: *Batch2D, encoder: *Gpu.CommandEncoder, id: ImageId, source_texture: *Gpu.Texture, width: u32, height: u32) !void {
    const cache_info = self.atlas.cache.get(id) orelse {
        log.err("Attempted to update a dynamic texture that wasn't reserved in the atlas.", .{});
        return error.DynamicTextureNotRegistered;
    };
    try self.atlas.updateDynamicLayer(encoder, cache_info.indirection_table_index, source_texture, width, height);
}

pub fn drawRect(self: *Batch2D, pos: vec2, size: vec2, tint: Color) !void {
    try self.pushTexturedQuad(AssetCache.WHITE_PIXEL_ID, pos, size, null, .{}, .all(0.0), tint);
}

pub fn drawRectLine(self: *Batch2D, pos: vec2, size: vec2, thickness: BorderWidth, tint: Color) !void {
    inline for (comptime std.meta.fieldNames(BorderWidth)) |fieldName| {
        const t = @min(@field(thickness, fieldName), @min(size[0] / 2.0, size[1] / 2.0));
        if (t <= 0.0) return;
    }
    try self.pushTexturedQuad(AssetCache.WHITE_PIXEL_ID, pos, size, null, .{}, thickness, tint);
}

pub fn drawRoundedRect(self: *Batch2D, pos: vec2, size: vec2, radius: CornerRadius, tint: Color) !void {
    try self.pushTexturedQuad(AssetCache.WHITE_PIXEL_ID, pos, size, null, radius, .all(0.0), tint);
}

pub fn drawRoundedRectLine(self: *Batch2D, pos: vec2, size: vec2, radius: CornerRadius, thickness: BorderWidth, tint: Color) !void {
    inline for (comptime std.meta.fieldNames(BorderWidth)) |fieldName| {
        const t = @min(@field(thickness, fieldName), @min(size[0] / 2.0, size[1] / 2.0));
        if (t <= 0.0) return;
    }
    try self.pushTexturedQuad(AssetCache.WHITE_PIXEL_ID, pos, size, null, radius, thickness, tint);
}

pub fn drawTexturedQuad(self: *Batch2D, image_id: Atlas.ImageId, pos: vec2, size: vec2, src_rect: ?UvRect, tint: Color) !void {
    try self.pushTexturedQuad(image_id, pos, size, src_rect, .{}, .all(0.0), tint);
}

pub fn drawRoundedTexturedQuad(self: *Batch2D, image_id: Atlas.ImageId, pos: vec2, size: vec2, radius: CornerRadius, src_rect: ?UvRect, tint: Color) !void {
    try self.pushTexturedQuad(image_id, pos, size, src_rect, radius, .all(0.0), tint);
}

pub fn drawTriangle(self: *Batch2D, v1: vec2, v2: vec2, v3: vec2, tint: Color) !void {
    const uv = vec2{ 0.0, 0.0 };
    try self.pushTexturedTriangle(AssetCache.WHITE_PIXEL_ID, v1, uv, v2, uv, v3, uv, tint);
}

pub fn pushTexturedQuad(self: *Batch2D, image_id: Atlas.ImageId, pos: vec2, size: vec2, uv_rect: ?UvRect, corner_radius: CornerRadius, border_thickness: BorderWidth, tint: Color) !void {
    const uv_min, const uv_max = if (uv_rect) |r| r else .{ .{ 0, 0 }, .{ 1, 1 } };
    const radii = [4]f32{ @max(0.0, corner_radius.top_left), @max(0.0, corner_radius.top_right), @max(0.0, corner_radius.bottom_right), @max(0.0, corner_radius.bottom_left) };
    const thickness = [4]f32{ @max(0.0, border_thickness.top), @max(0.0, border_thickness.right), @max(0.0, border_thickness.bottom), @max(0.0, border_thickness.left) };

    const location = self.atlas.query(image_id, self.provider_context) catch |err| {
        if (err == error.ImageNotYetPacked) {
            const quad_index = self.quads.items.len;
            try self.patch_list.append(self.allocator, .{
                .kind = .quad,
                .image_id = image_id,
                .start_index = quad_index,
                .count = 1,
            });
            return self.pushQuad(0, pos, size, uv_min, uv_max, tint, radii, thickness);
        }
        return err;
    };

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
    try self.pushQuad(encoded_params, pos, size, uv_min, uv_max, tint, radii, thickness);
}

pub fn pushTexturedTriangle(self: *Batch2D, image_id: Atlas.ImageId, v1: vec2, uv1: vec2, v2: vec2, uv2: vec2, v3: vec2, uv3: vec2, tint: Color) !void {
    const location = self.atlas.query(image_id, self.provider_context) catch |err| {
        if (err == error.ImageNotYetPacked) {
            const vertex_start_index = self.vertices.items.len;
            try self.patch_list.append(self.allocator, .{
                .kind = .tri,
                .image_id = image_id,
                .start_index = vertex_start_index,
                .count = 3,
            });
            return self.pushTriangle(0, v1, uv1, v2, uv2, v3, uv3, tint);
        }
        return err;
    };

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

pub fn drawArc(self: *Batch2D, center: vec2, radius: f32, start_angle: f32, end_angle: f32, tint: Color) !void {
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

        const p1 = vec2{ center[0] + std.math.cos(angle1) * radius, center[1] + std.math.sin(angle1) * radius };
        const p2 = vec2{ center[0] + std.math.cos(angle2) * radius, center[1] + std.math.sin(angle2) * radius };

        try self.drawTriangle(center, p1, p2, tint);
    }
}

pub fn drawArcLine(self: *Batch2D, center: vec2, radius: f32, start_angle: f32, end_angle: f32, thickness: f32, tint: Color) !void {
    const t = @min(thickness, radius);
    if (radius <= 0.0 or t <= 0.0) return;

    var normalized_end = end_angle;
    while (normalized_end < start_angle) {
        normalized_end += 2.0 * std.math.pi;
    }
    const total_angle = normalized_end - start_angle;
    if (total_angle <= 0.0) return;

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

        try self.drawTriangle(v_outer1, v_outer2, v_inner1, tint);
        try self.drawTriangle(v_inner1, v_outer2, v_inner2, tint);
    }
}

pub fn drawTexturedArc(
    self: *Batch2D,
    image_id: Atlas.ImageId,
    center_pos: vec2,
    radius: f32,
    start_angle: f32,
    end_angle: f32,
    center_uv: vec2,
    radius_uv: vec2,
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
        if (i % 2 == 0) {
            try self.drawTriangle(vertices[i], vertices[i + 1], vertices[i + 2], tint);
        } else {
            try self.drawTriangle(vertices[i + 1], vertices[i], vertices[i + 2], tint);
        }
    }
}

pub const GlyphLayoutInfo = struct {
    glyph_id: Atlas.ImageId,
    pos: vec2,
    size: vec2,
};

pub fn layoutText(
    self: *Batch2D,
    string: []const u8,
    font_id: AssetCache.FontId,
    font_size: AssetCache.FontSize,
    line_spacing_override: ?u16,
    pos: vec2,
    comptime T: type,
    comptime E: type,
    context: *T,
    callback: fn (context: *T, info: GlyphLayoutInfo) E!void,
) (E || error{InvalidFontId})!void {
    if (font_id >= self.asset_cache.fonts.items.len) {
        log.err("Invalid font_id {d} passed to layoutText", .{font_id});
        return error.InvalidFontId;
    }
    const font_info = &self.asset_cache.fonts.items[font_id].info;

    const font_size_f32 = @as(f32, @floatFromInt(font_size));
    const font_scale_f32 = stbtt.scaleForPixelHeight(font_info, font_size_f32);
    const font_scale: f64 = font_scale_f32;

    var ascent: i32 = 0;
    var descent: i32 = 0;
    var line_gap: i32 = 0;
    stbtt.getFontVMetrics(font_info, &ascent, &descent, &line_gap);

    const line_height: f64 =
        if (line_spacing_override) |spacing|
            @as(f64, @floatFromInt(spacing))
        else
            @as(f64, @floatFromInt(ascent - descent + line_gap)) * font_scale;

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

pub fn measureText(self: *Batch2D, string: []const u8, font_id: AssetCache.FontId, font_size: AssetCache.FontSize, line_spacing_override: ?u16) ?vec2 {
    if (string.len == 0) return .{ 0, 0 };

    if (font_id >= self.asset_cache.fonts.items.len) {
        log.err("Invalid font_id {d} passed to measureText", .{font_id});
        return null;
    }
    const font_info = &self.asset_cache.fonts.items[font_id].info;

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
    var total_size = vec2{ 0, @floatCast(@as(f64, @floatFromInt(ascent - descent)) * font_scale) };
    if (total_size[1] == 0 and string.len > 0) {
        total_size[1] = font_size_f32;
    }

    var prev_char: u21 = 0;

    for (string) |char| {
        if (char == '\n') {
            total_size[0] = @max(total_size[0], current_pos[0]);
            current_pos[0] = 0;
            total_size[1] += @floatCast(line_height);
            prev_char = 0;
            continue;
        }

        const char_code = @as(u21, @intCast(char));

        if (prev_char != 0) {
            const kern = stbtt.getCodepointKernAdvance(font_info, prev_char, char_code);
            current_pos[0] += @floatCast(@as(f64, @floatFromInt(kern)) * font_scale);
        }

        var adv: i32 = 0;
        stbtt.getCodepointHMetrics(font_info, char_code, &adv, null);
        current_pos[0] += @floatCast(@as(f64, @floatFromInt(adv)) * font_scale);

        prev_char = char_code;
    }

    total_size[0] = @max(total_size[0], current_pos[0]);

    if (total_size[0] == 0 and total_size[1] == 0) return null;

    return total_size;
}

const DrawContext = struct {
    batch: *Batch2D,
    tint: Color,

    fn drawGlyph(self: *DrawContext, info: GlyphLayoutInfo) !void {
        try self.batch.drawTexturedQuad(info.glyph_id, info.pos, info.size, null, self.tint);
    }
};

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

pub fn drawText(self: *Batch2D, string: []const u8, font_id: AssetCache.FontId, font_size: AssetCache.FontSize, line_spacing_override: ?u16, pos: vec2, tint: Color) !void {
    var context = DrawContext{
        .batch = self,
        .tint = tint,
    };

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

pub fn preAtlasAllImages(renderer: *Batch2D) !void {
    log.info("Beginning pre-atlasing of {d} images...", .{renderer.asset_cache.images.items.len});
    var timer = try std.time.Timer.start();

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const provider_ctx = Atlas.ProviderContext{
        .provider = AssetCache.dataProvider,
        .frame_allocator = arena.allocator(),
        .user_context = @constCast(renderer.asset_cache),
    };

    var image_id_iterator = renderer.asset_cache.image_map.valueIterator();
    while (image_id_iterator.next()) |image_id_ptr| {
        _ = renderer.atlas.query(image_id_ptr.*, provider_ctx) catch |err| {
            if (err != error.ImageNotYetPacked) {
                log.err("Error while querying image {d} for pre-atlasing: {any}", .{ image_id_ptr.*, err });
                return err;
            }
        };
    }

    if (try renderer.atlas.flush()) {
        log.warn("Atlas was recreated during pre-loading, recreating bind group.", .{});
        if (renderer.bind_group != null) {
            renderer.bind_group.?.release();
            renderer.bind_group = null;
        }
        try renderer.recreateBindGroup();
    }

    const duration_ms = @as(f64, @floatFromInt(timer.read())) / @as(f64, @floatFromInt(std.time.ns_per_ms));
    log.info("Finished pre-atlasing all images in {:.2}ms.", .{duration_ms});
}

fn pushQuad(self: *Batch2D, encoded_params: u32, pos: vec2, size: vec2, uv_min: vec2, uv_max: vec2, tint: Color, radii: [4]f32, border_thickness: [4]f32) !void {
    try self.ensurePipeline(.quad);

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
    const bg = try self.gpu.device.createBindGroup(&.{
        .label = .fromSlice("Batch2D:atlas_array_bind_group"),
        .layout = try getBindGroupLayout(self.gpu.device),
        .entry_count = 6,
        .entries = &[_]Gpu.BindGroupEntry{
            .{ .binding = 0, .buffer = self.uniform_buffer, .size = @sizeOf(Uniforms) },
            .{ .binding = 1, .sampler = self.linear_sampler },
            .{ .binding = 2, .sampler = self.nearest_sampler },
            .{ .binding = 3, .sampler = self.nearest_sampler },
            .{ .binding = 4, .texture_view = self.atlas.view },
            .{ .binding = 5, .buffer = self.indirection_buffer, .size = self.indirection_buffer_capacity * @sizeOf(Atlas.ImageMipData) },
        },
    });
    self.bind_group = bg;
}
