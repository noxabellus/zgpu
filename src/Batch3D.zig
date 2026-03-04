const Batch3D = @This();

const std = @import("std");
const linalg = @import("linalg.zig");
const vec3 = linalg.vec3;
const mat4 = linalg.mat4;
const Gpu = @import("Gpu.zig");
pub const Color = @import("Batch2D.zig").Color;

const log = std.log.scoped(.renderer3d);

const PipelineKind3D = enum {
    shaded_tri,
    wireframe_line,
};

const RenderBatch3D = struct {
    pipeline: PipelineKind3D,
    start_index: usize,
    count: usize,
};

const Vertex3D = extern struct {
    position: [3]f32,
    normal: [3]f32,
    color: [4]f32,
    /// Encodes a unique object ID that can be read back in the picking pass.
    id: u32,
};

const Uniforms3D = extern struct {
    view_projection: mat4,
};

allocator: std.mem.Allocator,
gpu: *Gpu,

uniform_buffer: *Gpu.Buffer,
bind_group: *Gpu.BindGroup,

shaded_pipeline: *Gpu.RenderPipeline,
picking_shaded_pipeline: *Gpu.RenderPipeline,
wireframe_pipeline: *Gpu.RenderPipeline,
picking_wireframe_pipeline: *Gpu.RenderPipeline,

vertex_buffer: ?*Gpu.Buffer,
vertex_buffer_capacity: usize,
vertices: std.ArrayList(Vertex3D),
batch_list: std.ArrayList(RenderBatch3D),

current_pipeline: ?PipelineKind3D,
current_batch_start: usize,

pub fn init(
    allocator: std.mem.Allocator,
    gpu: *Gpu,
    surface_format: Gpu.TextureFormat,
    picking_format: Gpu.TextureFormat,
    depth_format: Gpu.TextureFormat,
    sample_count: u32,
) !*Batch3D {
    const self = try allocator.create(Batch3D);
    errdefer allocator.destroy(self);

    const shader_module = try gpu.device.loadShaderText("static/shaders/Batch3D.wgsl", @embedFile("shaders/Batch3D.wgsl"));
    defer shader_module.release();

    const uniform_buffer = try gpu.device.createBuffer(&.{
        .label = .fromSlice("batch3d_uniform_buffer"),
        .usage = Gpu.BufferUsage{ .uniform = true, .copy_dst = true },
        .size = @sizeOf(Uniforms3D),
    });

    const bind_group_layout = try gpu.device.createBindGroupLayout(&.{
        .label = .fromSlice("batch3d_bind_group_layout"),
        .entry_count = 1,
        .entries = &[_]Gpu.BindGroupLayoutEntry{
            .{ .binding = 0, .visibility = .vertexStage, .buffer = .{ .type = .uniform } },
        },
    });
    defer bind_group_layout.release();

    const bind_group = try gpu.device.createBindGroup(&.{
        .label = .fromSlice("batch3d_bind_group"),
        .layout = bind_group_layout,
        .entry_count = 1,
        .entries = &[_]Gpu.BindGroupEntry{
            .{ .binding = 0, .buffer = uniform_buffer, .size = @sizeOf(Uniforms3D) },
        },
    });

    const pipeline_layout = try gpu.device.createPipelineLayout(&.{
        .label = .fromSlice("batch3d_pipeline_layout"),
        .bind_group_layout_count = 1,
        .bind_group_layouts = &.{bind_group_layout},
    });
    defer pipeline_layout.release();

    const depth_stencil_state = Gpu.DepthStencilState{
        .format = depth_format,
        .depth_write_enabled = .true,
        .depth_compare = .less,
        .stencil_front = .{},
        .stencil_back = .{},
    };

    const vertex_attributes = [_]Gpu.VertexAttribute{
        .{ .shaderLocation = 0, .offset = @offsetOf(Vertex3D, "position"), .format = .float32x3 },
        .{ .shaderLocation = 1, .offset = @offsetOf(Vertex3D, "normal"), .format = .float32x3 },
        .{ .shaderLocation = 2, .offset = @offsetOf(Vertex3D, "color"), .format = .float32x4 },
        .{ .shaderLocation = 3, .offset = @offsetOf(Vertex3D, "id"), .format = .uint32 },
    };

    const vertex_state = Gpu.VertexState{
        .module = shader_module,
        .entry_point = .fromSlice("vs_main"),
        .buffer_count = 1,
        .buffers = &.{.{
            .array_stride = @sizeOf(Vertex3D),
            .step_mode = .vertex,
            .attribute_count = vertex_attributes.len,
            .attributes = &vertex_attributes,
        }},
    };

    // 1. Shaded Triangle Pipelines
    const shaded_pipeline = try gpu.device.createRenderPipeline(&Gpu.RenderPipelineDescriptor{
        .label = .fromSlice("batch3d_shaded_pipeline"),
        .layout = pipeline_layout,
        .vertex = vertex_state,
        .fragment = &Gpu.FragmentState{
            .module = shader_module,
            .entry_point = .fromSlice("fs_shaded"),
            .target_count = 1,
            .targets = &.{.{ .format = surface_format, .write_mask = .all }},
        },
        .primitive = .{ .topology = .triangle_list },
        .depth_stencil = &depth_stencil_state,
        .multisample = .{ .count = sample_count, .mask = 0xFFFFFFFF },
    });

    const picking_shaded_pipeline = try gpu.device.createRenderPipeline(&Gpu.RenderPipelineDescriptor{
        .label = .fromSlice("batch3d_picking_shaded_pipeline"),
        .layout = pipeline_layout,
        .vertex = vertex_state,
        .fragment = &Gpu.FragmentState{
            .module = shader_module,
            .entry_point = .fromSlice("fs_picking"),
            .target_count = 1,
            .targets = &.{.{ .format = picking_format, .write_mask = .all }},
        },
        .primitive = .{ .topology = .triangle_list },
        .depth_stencil = &depth_stencil_state,
        .multisample = .{ .count = 1, .mask = 0xFFFFFFFF },
    });

    // 2. Wireframe Line Pipelines
    const wireframe_pipeline = try gpu.device.createRenderPipeline(&Gpu.RenderPipelineDescriptor{
        .label = .fromSlice("batch3d_wireframe_pipeline"),
        .layout = pipeline_layout,
        .vertex = vertex_state,
        .fragment = &Gpu.FragmentState{
            .module = shader_module,
            .entry_point = .fromSlice("fs_wireframe"),
            .target_count = 1,
            .targets = &.{.{ .format = surface_format, .write_mask = .all }},
        },
        .primitive = .{ .topology = .line_list },
        .depth_stencil = &depth_stencil_state,
        .multisample = .{ .count = sample_count, .mask = 0xFFFFFFFF },
    });

    const picking_wireframe_pipeline = try gpu.device.createRenderPipeline(&Gpu.RenderPipelineDescriptor{
        .label = .fromSlice("batch3d_wireframe_pipeline"),
        .layout = pipeline_layout,
        .vertex = vertex_state,
        .fragment = &Gpu.FragmentState{
            .module = shader_module,
            .entry_point = .fromSlice("fs_picking"),
            .target_count = 1,
            .targets = &.{.{ .format = picking_format, .write_mask = .all }},
        },
        .primitive = .{ .topology = .line_list },
        .depth_stencil = &depth_stencil_state,
        .multisample = .{ .count = 1, .mask = 0xFFFFFFFF },
    });

    self.* = .{
        .allocator = allocator,
        .gpu = gpu,
        .uniform_buffer = uniform_buffer,
        .bind_group = bind_group,
        .shaded_pipeline = shaded_pipeline,
        .picking_shaded_pipeline = picking_shaded_pipeline,
        .wireframe_pipeline = wireframe_pipeline,
        .picking_wireframe_pipeline = picking_wireframe_pipeline,
        .vertex_buffer = null,
        .vertex_buffer_capacity = 0,
        .vertices = .empty,
        .batch_list = .empty,
        .current_pipeline = null,
        .current_batch_start = 0,
    };

    try self.vertices.ensureTotalCapacity(self.allocator, 16384);
    try self.batch_list.ensureTotalCapacity(self.allocator, 32);

    return self;
}

pub fn deinit(self: *Batch3D) void {
    if (self.vertex_buffer) |vb| vb.release();
    self.uniform_buffer.release();
    self.bind_group.release();
    self.shaded_pipeline.release();
    self.picking_shaded_pipeline.release();
    self.wireframe_pipeline.release();
    self.picking_wireframe_pipeline.release();
    self.vertices.deinit(self.allocator);
    self.batch_list.deinit(self.allocator);
    self.allocator.destroy(self);
}

pub fn beginFrame(self: *Batch3D, view_projection: mat4) void {
    self.gpu.queue.writeBuffer(self.uniform_buffer, 0, &Uniforms3D{ .view_projection = view_projection }, @sizeOf(Uniforms3D));
    self.vertices.clearRetainingCapacity();
    self.batch_list.clearRetainingCapacity();
    self.current_pipeline = null;
    self.current_batch_start = 0;
}

fn endCurrentBatch(self: *Batch3D) !void {
    if (self.current_pipeline == null) return;
    const count = self.vertices.items.len - self.current_batch_start;

    if (count > 0) {
        try self.batch_list.append(self.allocator, .{
            .pipeline = self.current_pipeline.?,
            .start_index = self.current_batch_start,
            .count = count,
        });
    }
}

fn ensurePipeline(self: *Batch3D, pipeline: PipelineKind3D) !void {
    if (self.current_pipeline != pipeline) {
        try self.endCurrentBatch();
        self.current_pipeline = pipeline;
        self.current_batch_start = self.vertices.items.len;
    }
}

pub fn endFrame(self: *Batch3D) !void {
    try self.endCurrentBatch();

    if (self.vertices.items.len > 0) {
        const required_size = self.vertices.items.len * @sizeOf(Vertex3D);

        if (self.vertex_buffer_capacity < required_size) {
            if (self.vertex_buffer) |vb| vb.release();
            const new_capacity = @max(@max(self.vertex_buffer_capacity * 2, required_size), 4096);
            self.vertex_buffer = try self.gpu.device.createBuffer(&.{
                .label = .fromSlice("batch3d_vertex_buffer"),
                .usage = .{ .vertex = true, .copy_dst = true },
                .size = new_capacity,
            });
            self.vertex_buffer_capacity = new_capacity;
        }

        self.gpu.queue.writeBuffer(self.vertex_buffer.?, 0, self.vertices.items.ptr, required_size);
    }
}

pub fn render(self: *Batch3D, render_pass: *Gpu.RenderPassEncoder) !void {
    if (self.vertex_buffer == null) return;

    render_pass.setBindGroup(0, self.bind_group, &.{});
    render_pass.setVertexBuffer(0, self.vertex_buffer.?, 0, self.vertices.items.len * @sizeOf(Vertex3D));

    var bound_pipeline: ?PipelineKind3D = null;
    for (self.batch_list.items) |batch| {
        if (batch.count == 0) continue;

        if (bound_pipeline != batch.pipeline) {
            switch (batch.pipeline) {
                .shaded_tri => render_pass.setPipeline(self.shaded_pipeline),
                .wireframe_line => render_pass.setPipeline(self.wireframe_pipeline),
            }
            bound_pipeline = batch.pipeline;
        }

        render_pass.draw(@intCast(batch.count), 1, @intCast(batch.start_index), 0);
    }
}

pub fn renderPicking(self: *Batch3D, render_pass: *Gpu.RenderPassEncoder) !void {
    if (self.vertex_buffer == null) return;
    render_pass.setBindGroup(0, self.bind_group, &.{});
    render_pass.setVertexBuffer(0, self.vertex_buffer.?, 0, self.vertices.items.len * @sizeOf(Vertex3D));

    var bound_pipeline: ?PipelineKind3D = null;
    for (self.batch_list.items) |batch| {
        if (batch.count == 0) continue;

        if (bound_pipeline != batch.pipeline) {
            switch (batch.pipeline) {
                .shaded_tri => render_pass.setPipeline(self.picking_shaded_pipeline),
                .wireframe_line => render_pass.setPipeline(self.picking_wireframe_pipeline),
            }
            bound_pipeline = batch.pipeline;
        }

        render_pass.draw(@intCast(batch.count), 1, @intCast(batch.start_index), 0);
    }
}

// --- 3D Primitive Drawing API ---

pub fn drawLine(self: *Batch3D, p1: vec3, p2: vec3, tint: Color, id: u32) !void {
    try self.ensurePipeline(.wireframe_line);
    const c = [4]f32{ tint.r, tint.g, tint.b, tint.a };
    // Normals don't matter for wireframes
    const n = [3]f32{ 0.0, 0.0, 0.0 };
    try self.vertices.appendSlice(self.allocator, &[_]Vertex3D{
        .{ .position = p1, .normal = n, .color = c, .id = id },
        .{ .position = p2, .normal = n, .color = c, .id = id },
    });
}

pub fn drawTriangle(self: *Batch3D, p1: vec3, p2: vec3, p3: vec3, tint: Color, id: u32) !void {
    try self.ensurePipeline(.shaded_tri);
    const c = [4]f32{ tint.r, tint.g, tint.b, tint.a };

    // Calculate flat face normal
    const u = .{ p2[0] - p1[0], p2[1] - p1[1], p2[2] - p1[2] };
    const v = .{ p3[0] - p1[0], p3[1] - p1[1], p3[2] - p1[2] };
    var n = [3]f32{
        u[1] * v[2] - u[2] * v[1],
        u[2] * v[0] - u[0] * v[2],
        u[0] * v[1] - u[1] * v[0],
    };
    // Normalize
    const len = std.math.sqrt(n[0] * n[0] + n[1] * n[1] + n[2] * n[2]);
    if (len > 0) {
        n[0] /= len;
        n[1] /= len;
        n[2] /= len;
    }

    try self.vertices.appendSlice(self.allocator, &[_]Vertex3D{
        .{ .position = p1, .normal = n, .color = c, .id = id },
        .{ .position = p2, .normal = n, .color = c, .id = id },
        .{ .position = p3, .normal = n, .color = c, .id = id },
    });
}

// --- Planes ---
pub fn drawPlane(self: *Batch3D, transform: mat4, tint: Color, id: u32) !void {
    const p0 = linalg.mat4_apply(transform, linalg.vec3{ -0.5, 0.0, -0.5 });
    const p1 = linalg.mat4_apply(transform, linalg.vec3{ 0.5, 0.0, -0.5 });
    const p2 = linalg.mat4_apply(transform, linalg.vec3{ -0.5, 0.0, 0.5 });
    const p3 = linalg.mat4_apply(transform, linalg.vec3{ 0.5, 0.0, 0.5 });

    try self.drawTriangle(p0, p2, p1, tint, id);
    try self.drawTriangle(p1, p2, p3, tint, id);
}

pub fn drawPlaneWire(self: *Batch3D, transform: mat4, tint: Color, id: u32) !void {
    const p0 = linalg.mat4_apply(transform, linalg.vec3{ -0.5, 0.0, -0.5 });
    const p1 = linalg.mat4_apply(transform, linalg.vec3{ 0.5, 0.0, -0.5 });
    const p2 = linalg.mat4_apply(transform, linalg.vec3{ 0.5, 0.0, 0.5 });
    const p3 = linalg.mat4_apply(transform, linalg.vec3{ -0.5, 0.0, 0.5 });

    try self.drawLine(p0, p1, tint, id);
    try self.drawLine(p1, p2, tint, id);
    try self.drawLine(p2, p3, tint, id);
    try self.drawLine(p3, p0, tint, id);
}

// --- Cubes ---
pub fn drawCubeWire(self: *Batch3D, transform: mat4, tint: Color, id: u32) !void {
    const corners = [_]linalg.vec3{
        linalg.mat4_apply(transform, linalg.vec3{ -0.5, -0.5, -0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ 0.5, -0.5, -0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ 0.5, 0.5, -0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ -0.5, 0.5, -0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ -0.5, -0.5, 0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ 0.5, -0.5, 0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ 0.5, 0.5, 0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ -0.5, 0.5, 0.5 }),
    };

    // Bottom and top loops
    for (0..4) |i| {
        try self.drawLine(corners[i], corners[(i + 1) % 4], tint, id);
        try self.drawLine(corners[i + 4], corners[((i + 1) % 4) + 4], tint, id);
        try self.drawLine(corners[i], corners[i + 4], tint, id);
    }
}

pub fn drawCube(self: *Batch3D, transform: mat4, tint: Color, id: u32) !void {
    const corners = [_]linalg.vec3{
        linalg.mat4_apply(transform, linalg.vec3{ -0.5, -0.5, -0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ 0.5, -0.5, -0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ 0.5, 0.5, -0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ -0.5, 0.5, -0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ -0.5, -0.5, 0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ 0.5, -0.5, 0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ 0.5, 0.5, 0.5 }),
        linalg.mat4_apply(transform, linalg.vec3{ -0.5, 0.5, 0.5 }),
    };

    const indices = [_]usize{
        0, 1, 2, 0, 2, 3, // Front
        1, 5, 6, 1, 6, 2, // Right
        5, 4, 7, 5, 7, 6, // Back
        4, 0, 3, 4, 3, 7, // Left
        3, 2, 6, 3, 6, 7, // Top
        4, 5, 1, 4, 1, 0, // Bottom
    };

    var i: usize = 0;
    while (i < indices.len) : (i += 3) {
        try self.drawTriangle(corners[indices[i]], corners[indices[i + 1]], corners[indices[i + 2]], tint, id);
    }
}

// --- Cylinders ---
pub fn drawCylinderWire(self: *Batch3D, transform: mat4, segments: usize, tint: Color, id: u32) !void {
    const step = (2.0 * std.math.pi) / @as(f32, @floatFromInt(segments));

    for (0..segments) |i| {
        const angle1 = @as(f32, @floatFromInt(i)) * step;
        const angle2 = @as(f32, @floatFromInt((i + 1) % segments)) * step;

        const b1 = linalg.mat4_apply(transform, linalg.vec3{ @cos(angle1) * 0.5, -0.5, @sin(angle1) * 0.5 });
        const b2 = linalg.mat4_apply(transform, linalg.vec3{ @cos(angle2) * 0.5, -0.5, @sin(angle2) * 0.5 });
        const t1 = linalg.mat4_apply(transform, linalg.vec3{ @cos(angle1) * 0.5, 0.5, @sin(angle1) * 0.5 });
        const t2 = linalg.mat4_apply(transform, linalg.vec3{ @cos(angle2) * 0.5, 0.5, @sin(angle2) * 0.5 });

        try self.drawLine(b1, b2, tint, id); // Bottom cap
        try self.drawLine(t1, t2, tint, id); // Top cap
        try self.drawLine(b1, t1, tint, id); // Vertical sides
    }
}

pub fn drawCylinder(self: *Batch3D, transform: mat4, segments: usize, tint: Color, id: u32) !void {
    const step = (2.0 * std.math.pi) / @as(f32, @floatFromInt(segments));
    const center_bottom = linalg.mat4_apply(transform, linalg.vec3{ 0.0, -0.5, 0.0 });
    const center_top = linalg.mat4_apply(transform, linalg.vec3{ 0.0, 0.5, 0.0 });

    for (0..segments) |i| {
        const angle1 = @as(f32, @floatFromInt(i)) * step;
        const angle2 = @as(f32, @floatFromInt((i + 1) % segments)) * step;

        const b1 = linalg.mat4_apply(transform, linalg.vec3{ @cos(angle1) * 0.5, -0.5, @sin(angle1) * 0.5 });
        const b2 = linalg.mat4_apply(transform, linalg.vec3{ @cos(angle2) * 0.5, -0.5, @sin(angle2) * 0.5 });
        const t1 = linalg.mat4_apply(transform, linalg.vec3{ @cos(angle1) * 0.5, 0.5, @sin(angle1) * 0.5 });
        const t2 = linalg.mat4_apply(transform, linalg.vec3{ @cos(angle2) * 0.5, 0.5, @sin(angle2) * 0.5 });

        try self.drawTriangle(center_bottom, b2, b1, tint, id); // Bottom Cap
        try self.drawTriangle(center_top, t1, t2, tint, id); // Top Cap

        // Side Wall Quads (2 triangles)
        try self.drawTriangle(b1, b2, t1, tint, id);
        try self.drawTriangle(t1, b2, t2, tint, id);
    }
}

// --- Cones ---
pub fn drawConeWire(self: *Batch3D, transform: mat4, segments: usize, tint: Color, id: u32) !void {
    const step = (2.0 * std.math.pi) / @as(f32, @floatFromInt(segments));
    const tip = linalg.mat4_apply(transform, linalg.vec3{ 0.0, 0.5, 0.0 });

    for (0..segments) |i| {
        const angle1 = @as(f32, @floatFromInt(i)) * step;
        const angle2 = @as(f32, @floatFromInt((i + 1) % segments)) * step;

        const b1 = linalg.mat4_apply(transform, linalg.vec3{ @cos(angle1) * 0.5, -0.5, @sin(angle1) * 0.5 });
        const b2 = linalg.mat4_apply(transform, linalg.vec3{ @cos(angle2) * 0.5, -0.5, @sin(angle2) * 0.5 });

        try self.drawLine(b1, b2, tint, id);
        try self.drawLine(b1, tip, tint, id);
    }
}

pub fn drawCone(self: *Batch3D, transform: mat4, segments: usize, tint: Color, id: u32) !void {
    const step = (2.0 * std.math.pi) / @as(f32, @floatFromInt(segments));
    const center_bottom = linalg.mat4_apply(transform, linalg.vec3{ 0.0, -0.5, 0.0 });
    const tip = linalg.mat4_apply(transform, linalg.vec3{ 0.0, 0.5, 0.0 });

    for (0..segments) |i| {
        const angle1 = @as(f32, @floatFromInt(i)) * step;
        const angle2 = @as(f32, @floatFromInt((i + 1) % segments)) * step;

        const b1 = linalg.mat4_apply(transform, linalg.vec3{ @cos(angle1) * 0.5, -0.5, @sin(angle1) * 0.5 });
        const b2 = linalg.mat4_apply(transform, linalg.vec3{ @cos(angle2) * 0.5, -0.5, @sin(angle2) * 0.5 });

        try self.drawTriangle(center_bottom, b2, b1, tint, id); // Base
        try self.drawTriangle(b1, b2, tip, tint, id); // Side
    }
}

// --- Ico-Sphere ---
const ico_indices = [_]usize{
    0,  11, 5,
    0,  5,  1,
    0,  1,  7,
    0,  7,  10,
    0,  10, 11,
    1,  5,  9,
    5,  11, 4,
    11, 10, 2,
    10, 7,  6,
    7,  1,  8,
    3,  9,  4,
    3,  4,  2,
    3,  2,  6,
    3,  6,  8,
    3,  8,  9,
    4,  9,  5,
    2,  4,  11,
    6,  2,  10,
    8,  6,  7,
    9,  8,  1,
};

fn getIcoVertices(transform: mat4) [12]linalg.vec3 { // [cite: 68, 70]
    const t = (1.0 + std.math.sqrt(5.0)) / 2.0;
    const verts = [_]linalg.vec3{ // [cite: 70]
        .{ -1, t, 0 }, .{ 1, t, 0 }, .{ -1, -t, 0 }, .{ 1, -t, 0 },
        .{ 0, -1, t }, .{ 0, 1, t }, .{ 0, -1, -t }, .{ 0, 1, -t },
        .{ t, 0, -1 }, .{ t, 0, 1 }, .{ -t, 0, -1 }, .{ -t, 0, 1 },
    };

    var transformed_verts: [12]linalg.vec3 = undefined;
    for (verts, 0..) |v, i| {
        const normalized = linalg.normalize(v);
        // We scale down slightly so it fits inside a unit radius
        transformed_verts[i] = linalg.mat4_apply(transform, normalized * @as(linalg.vec3, @splat(0.5)));
    }
    return transformed_verts;
}

pub fn drawIcoSphereWire(self: *Batch3D, transform: mat4, tint: Color, id: u32) !void {
    const verts = getIcoVertices(transform);
    var i: usize = 0;
    while (i < ico_indices.len) : (i += 3) {
        try self.drawLine(verts[ico_indices[i]], verts[ico_indices[i + 1]], tint, id);
        try self.drawLine(verts[ico_indices[i + 1]], verts[ico_indices[i + 2]], tint, id);
        try self.drawLine(verts[ico_indices[i + 2]], verts[ico_indices[i]], tint, id);
    }
}

pub fn drawIcoSphere(self: *Batch3D, transform: mat4, tint: Color, id: u32) !void {
    const verts = getIcoVertices(transform);
    var i: usize = 0;
    while (i < ico_indices.len) : (i += 3) {
        try self.drawTriangle(verts[ico_indices[i]], verts[ico_indices[i + 1]], verts[ico_indices[i + 2]], tint, id);
    }
}
