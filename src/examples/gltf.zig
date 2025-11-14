const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");
const path = std.fs.path;

const wgpu = @import("wgpu");
const glfw = @import("glfw");
const gltf = @import("gltf");

const debug = @import("../debug.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;
const vec3 = linalg.vec3;
const vec4 = linalg.vec4; // Import vec4 for colors
const mat4 = linalg.mat4;

pub const std_options = std.Options{
    .log_level = .info,
};

test {
    log.debug("semantic analysis for examples/gltf.zig", .{});
    std.testing.refAllDecls(@This());
}

// Define a depth format that we'll use for our depth texture.
const DEPTH_FORMAT = wgpu.TextureFormat.depth32_float;

const Demo = struct {
    instance: wgpu.Instance = null,
    surface: wgpu.Surface = null,
    adapter: wgpu.Adapter = null,
    device: wgpu.Device = null,
    // We need to store the depth texture and its view.
    depth_texture: wgpu.Texture = null,
    depth_view: wgpu.TextureView = null,
    config: wgpu.SurfaceConfiguration = .{},
};

// A struct to define our vertex data, matching the data we'll load from glTF.
// Now includes normals for lighting and colors.
const Vertex = extern struct {
    position: vec3,
    normal: vec3,
    color: vec4,
};

// A struct to hold the WGPU resources for a single drawable part of a model.
const MeshPrimitive = struct {
    vertex_buffer: wgpu.Buffer,
    index_buffer: wgpu.Buffer,
    index_count: u32,
    index_format: wgpu.IndexFormat,
};

// A struct to represent our loaded model.
const Model = struct {
    primitives: []MeshPrimitive,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Model) void {
        for (self.primitives) |p| {
            wgpu.bufferRelease(p.vertex_buffer);
            wgpu.bufferRelease(p.index_buffer);
        }
        self.allocator.free(self.primitives);
    }
};

// A struct for our camera's view-projection matrix uniform.
const CameraUniform = extern struct {
    view_proj: mat4,
};

// The WGSL shader source code.
// Updated to include normals for lighting and vertex colors.
const shader_text =
    \\struct CameraUniform {
    \\    view_proj: mat4x4<f32>,
    \\};
    \\@group(0) @binding(0)
    \\var<uniform> u_camera: CameraUniform;
    \\
    \\struct VertexInput {
    \\    @location(0) position: vec3<f32>,
    \\    @location(1) normal: vec3<f32>,
    \\    @location(2) color: vec4<f32>,
    \\};
    \\
    \\struct VertexOutput {
    \\    @builtin(position) clip_position: vec4<f32>,
    \\    @location(0) normal: vec3<f32>,
    \\    @location(1) color: vec4<f32>,
    \\};
    \\
    \\@vertex
    \\fn vs_main(model: VertexInput) -> VertexOutput {
    \\    var out: VertexOutput;
    \\    out.clip_position = u_camera.view_proj * vec4<f32>(model.position, 1.0);
    \\    out.normal = model.normal;
    \\    out.color = model.color;
    \\    return out;
    \\}
    \\
    \\@fragment
    \\fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    \\    // A simple, hardcoded directional light to see the model's form.
    \\    let light_dir = normalize(vec3<f32>(0.5, 1.0, 0.75));
    \\    let normal = normalize(in.normal);
    \\
    \\    // Lambertian diffuse lighting.
    \\    let diffuse_intensity = max(dot(normal, light_dir), 0.0);
    \\
    \\    // Add some ambient light so the dark side isn't completely black.
    \\    let ambient_intensity = 0.2;
    \\
    \\    let total_intensity = ambient_intensity + diffuse_intensity;
    \\
    \\    // Combine light with vertex color.
    \\    let final_color = in.color.rgb * total_intensity;
    \\
    \\    return vec4<f32>(final_color, in.color.a);
    \\}
;

// This function creates the depth texture and its view.
// It will be called on startup and whenever the window is resized.
fn createDepthTexture(d: *Demo) void {
    // If old resources exist, release them first.
    if (d.depth_view) |v| wgpu.textureViewRelease(v);
    if (d.depth_texture) |t| wgpu.textureRelease(t);

    const depth_texture_descriptor = wgpu.TextureDescriptor{
        .label = .fromSlice("depth_texture"),
        .size = .{ .width = d.config.width, .height = d.config.height, .depth_or_array_layers = 1 },
        .mip_level_count = 1,
        .sample_count = 1,
        .dimension = .@"2d",
        .format = DEPTH_FORMAT,
        .usage = wgpu.TextureUsage.renderAttachmentUsage,
        .view_format_count = 1,
        .view_formats = &.{DEPTH_FORMAT},
    };
    d.depth_texture = wgpu.deviceCreateTexture(d.device, &depth_texture_descriptor);
    d.depth_view = wgpu.textureCreateView(d.depth_texture, null);
}

/// Loads a glTF model from the given path, creating WGPU buffers for its geometry.
fn loadGltfModel(
    allocator: std.mem.Allocator,
    device: wgpu.Device,
    queue: wgpu.Queue,
    model_path: []const u8,
) !Model {
    // Read the main glTF file (JSON).
    const gltf_file_buffer = try std.fs.cwd().readFileAllocOptions(model_path, allocator, .unlimited, .@"4", null);
    defer allocator.free(gltf_file_buffer);

    // Initialize the gltf parser and parse the file.
    var gltf_data = gltf.init(allocator);
    defer gltf_data.deinit();
    try gltf_data.parse(gltf_file_buffer);

    // Load the binary data (.bin files) associated with the glTF.
    var binary_buffers = std.ArrayList([]align(4) const u8).empty;
    defer {
        for (binary_buffers.items) |b| allocator.free(b);
        binary_buffers.deinit(allocator);
    }

    if (gltf_data.glb_binary) |bin| {
        // For .glb files, the binary data is already in memory.
        const owned_bin = try allocator.alignedAlloc(u8, .@"4", bin.len);
        @memcpy(owned_bin, bin);
        try binary_buffers.append(allocator, owned_bin);
    } else {
        // For .gltf files, we need to load the referenced .bin files.
        const model_dir = path.dirname(model_path).?;
        for (gltf_data.data.buffers) |buffer_info| {
            const bin_path = try path.join(allocator, &.{ model_dir, buffer_info.uri.? });
            defer allocator.free(bin_path);
            const bin_buffer = try std.fs.cwd().readFileAllocOptions(bin_path, allocator, .unlimited, .@"4", null);
            try binary_buffers.append(allocator, bin_buffer);
        }
    }

    var primitive_list = std.ArrayList(MeshPrimitive).empty;
    errdefer primitive_list.deinit(allocator);

    // A union to hold an iterator for any of the supported color formats.
    const ColorIterator = union(enum) {
        none,
        float: gltf.AccessorIterator(f32),
        u16: gltf.AccessorIterator(u16),
        // u8 is also a common format, so we'll support it too.
        u8: gltf.AccessorIterator(u8),
    };

    // Process all meshes in the glTF file.
    for (gltf_data.data.meshes) |mesh| {
        for (mesh.primitives) |primitive| {
            // --- Extract Vertex Attributes ---
            var positions_accessor_idx: ?gltf.Index = null;
            var normals_accessor_idx: ?gltf.Index = null;
            var colors_accessor_idx: ?gltf.Index = null;
            for (primitive.attributes) |attr| {
                switch (attr) {
                    .position => |idx| positions_accessor_idx = idx,
                    .normal => |idx| normals_accessor_idx = idx,
                    .color => |idx| colors_accessor_idx = idx, // Note: This now works due to the library fix
                    else => {},
                }
            }

            // We need at least positions and normals for this demo.
            if (positions_accessor_idx == null or normals_accessor_idx == null) {
                log.warn("Primitive in mesh '{s}' is missing positions or normals. Skipping.", .{mesh.name orelse "unnamed"});
                continue;
            }

            const pos_acc = gltf_data.data.accessors[positions_accessor_idx.?];
            const norm_acc = gltf_data.data.accessors[normals_accessor_idx.?];
            std.debug.assert(pos_acc.count == norm_acc.count); // Ensure attributes have same vertex count

            const pos_buffer_view = gltf_data.data.buffer_views[pos_acc.buffer_view.?];
            const pos_binary_data = binary_buffers.items[pos_buffer_view.buffer];
            const norm_buffer_view = gltf_data.data.buffer_views[norm_acc.buffer_view.?];
            const norm_binary_data = binary_buffers.items[norm_buffer_view.buffer];

            var vertices = std.ArrayList(Vertex).empty;
            defer vertices.deinit(allocator);
            try vertices.ensureTotalCapacity(allocator, pos_acc.count);

            var pos_iter = pos_acc.iterator(f32, &gltf_data, pos_binary_data);
            var norm_iter = norm_acc.iterator(f32, &gltf_data, norm_binary_data);

            // Create the correct color iterator based on the accessor's component type.
            var color_iter: ColorIterator = .none;
            if (colors_accessor_idx) |color_idx| {
                const color_acc = gltf_data.data.accessors[color_idx];
                const color_buffer_view = gltf_data.data.buffer_views[color_acc.buffer_view.?];
                const color_binary_data = binary_buffers.items[color_buffer_view.buffer];

                switch (color_acc.component_type) {
                    .float => color_iter = .{ .float = color_acc.iterator(f32, &gltf_data, color_binary_data) },
                    .unsigned_short => color_iter = .{ .u16 = color_acc.iterator(u16, &gltf_data, color_binary_data) },
                    .unsigned_byte => color_iter = .{ .u8 = color_acc.iterator(u8, &gltf_data, color_binary_data) },
                    else => log.warn("Unsupported vertex color format: {any}", .{color_acc.component_type}),
                }
            }

            while (pos_iter.next()) |pos_slice| {
                const norm_slice = norm_iter.next().?;

                const pos_vec: vec3 = .{ pos_slice[0], pos_slice[1], pos_slice[2] };
                const norm_vec: vec3 = .{ norm_slice[0], norm_slice[1], norm_slice[2] };

                // Get the next color, normalizing if necessary.
                const color_vec: vec4 = switch (color_iter) {
                    .none => .{ 1.0, 1.0, 1.0, 1.0 }, // Default to white
                    .float => |*iter| blk: {
                        const color_slice = iter.next().?;
                        break :blk if (color_slice.len == 3)
                            .{ color_slice[0], color_slice[1], color_slice[2], 1.0 }
                        else
                            .{ color_slice[0], color_slice[1], color_slice[2], color_slice[3] };
                    },
                    .u16 => |*iter| blk: {
                        const color_slice = iter.next().?;
                        const norm: f32 = 1.0 / 65535.0;
                        break :blk if (color_slice.len == 3)
                            .{ @as(f32, @floatFromInt(color_slice[0])) * norm, @as(f32, @floatFromInt(color_slice[1])) * norm, @as(f32, @floatFromInt(color_slice[2])) * norm, 1.0 }
                        else
                            .{ @as(f32, @floatFromInt(color_slice[0])) * norm, @as(f32, @floatFromInt(color_slice[1])) * norm, @as(f32, @floatFromInt(color_slice[2])) * norm, @as(f32, @floatFromInt(color_slice[3])) * norm };
                    },
                    .u8 => |*iter| blk: {
                        const color_slice = iter.next().?;
                        const norm: f32 = 1.0 / 255.0;
                        break :blk if (color_slice.len == 3)
                            .{ @as(f32, @floatFromInt(color_slice[0])) * norm, @as(f32, @floatFromInt(color_slice[1])) * norm, @as(f32, @floatFromInt(color_slice[2])) * norm, 1.0 }
                        else
                            .{ @as(f32, @floatFromInt(color_slice[0])) * norm, @as(f32, @floatFromInt(color_slice[1])) * norm, @as(f32, @floatFromInt(color_slice[2])) * norm, @as(f32, @floatFromInt(color_slice[3])) * norm };
                    },
                };

                vertices.appendAssumeCapacity(.{
                    .position = pos_vec,
                    .normal = norm_vec,
                    .color = color_vec,
                });
            }

            const vertex_buffer_bytes = std.mem.sliceAsBytes(vertices.items);
            const vbo = wgpu.deviceCreateBuffer(device, &.{
                .label = wgpu.StringView.fromSlice(mesh.name orelse "anonymous_mesh"),
                .usage = .{ .vertex = true, .copy_dst = true },
                .size = vertex_buffer_bytes.len,
                .mapped_at_creation = .False,
            });
            wgpu.queueWriteBuffer(queue, vbo, 0, vertex_buffer_bytes.ptr, vertex_buffer_bytes.len);

            // --- Extract Indices ---
            // ... (rest of the function is unchanged) ...
            const idx_acc = gltf_data.data.accessors[primitive.indices.?];
            const idx_buffer_view = gltf_data.data.buffer_views[idx_acc.buffer_view.?];
            const idx_binary_data = binary_buffers.items[idx_buffer_view.buffer];

            var ibo: wgpu.Buffer = undefined;
            const index_count: u32 = @intCast(idx_acc.count);
            var index_format: wgpu.IndexFormat = undefined;

            switch (idx_acc.component_type) {
                .unsigned_short => {
                    index_format = .uint16;
                    var indices = std.ArrayList(u16).empty;
                    defer indices.deinit(allocator);
                    try indices.ensureTotalCapacity(allocator, idx_acc.count);

                    var idx_iter = idx_acc.iterator(u16, &gltf_data, idx_binary_data);
                    while (idx_iter.next()) |idx_slice| {
                        for (idx_slice) |idx| indices.appendAssumeCapacity(idx);
                    }
                    const index_buffer_bytes = std.mem.sliceAsBytes(indices.items);
                    ibo = wgpu.deviceCreateBuffer(device, &.{
                        .label = .fromSlice("index_buffer_u16"),
                        .usage = .{ .index = true, .copy_dst = true },
                        .size = index_buffer_bytes.len,
                        .mapped_at_creation = .False,
                    });
                    wgpu.queueWriteBuffer(queue, ibo, 0, index_buffer_bytes.ptr, index_buffer_bytes.len);
                },
                .unsigned_integer => {
                    index_format = .uint32;
                    var indices = std.ArrayList(u32).empty;
                    defer indices.deinit(allocator);
                    try indices.ensureTotalCapacity(allocator, idx_acc.count);

                    var idx_iter = idx_acc.iterator(u32, &gltf_data, idx_binary_data);
                    while (idx_iter.next()) |idx_slice| {
                        for (idx_slice) |idx| indices.appendAssumeCapacity(idx);
                    }
                    const index_buffer_bytes = std.mem.sliceAsBytes(indices.items);
                    ibo = wgpu.deviceCreateBuffer(device, &.{
                        .label = .fromSlice("index_buffer_u32"),
                        .usage = .{ .index = true, .copy_dst = true },
                        .size = index_buffer_bytes.len,
                        .mapped_at_creation = .False,
                    });
                    wgpu.queueWriteBuffer(queue, ibo, 0, index_buffer_bytes.ptr, index_buffer_bytes.len);
                },
                else => return error.UnsupportedIndexFormat,
            }

            try primitive_list.append(allocator, .{
                .vertex_buffer = vbo,
                .index_buffer = ibo,
                .index_count = index_count,
                .index_format = index_format,
            });
        }
    }

    return Model{
        .primitives = try primitive_list.toOwnedSlice(allocator),
        .allocator = allocator,
    };
}

pub fn main() !void {
    var timer = try std.time.Timer.start();

    const gpa = std.heap.page_allocator;

    if (comptime builtin.os.tag != .windows) {
        glfw.initHint(.{ .platform = .x11 });
    } else {
        glfw.initHint(.{ .platform = .win32 });
    }

    try glfw.init();
    defer glfw.deinit();

    var demo = Demo{};

    const instance_extras = wgpu.InstanceExtras{ .chain = .{ .s_type = .instance_extras }, .backends = switch (builtin.os.tag) {
        .windows => if (glfw.isRunningInWine()) wgpu.InstanceBackend.vulkanBackend else wgpu.InstanceBackend.dx12Backend,
        else => wgpu.InstanceBackend.vulkanBackend,
    } };
    wgpu.setLogCallback(&struct {
        pub fn wgpu_logger(level: wgpu.LogLevel, message: wgpu.StringView, _: ?*anyopaque) callconv(.c) void {
            const msg = message.toSlice();

            const prefix = switch (level) {
                .@"error" => "wgpu error: ",
                .warn => "wgpu warn: ",
                .info => "wgpu info: ",
                .debug => "wgpu debug: ",
                .trace => "wgpu trace: ",
                else => "wgpu unknown: ",
            };

            std.debug.print("{s}{s}\n", .{ prefix, msg });
        }
    }.wgpu_logger, null);
    wgpu.setLogLevel(.info);
    demo.instance = wgpu.createInstance(&wgpu.InstanceDescriptor{ .next_in_chain = @ptrCast(&instance_extras) });
    std.debug.assert(demo.instance != null);
    defer wgpu.instanceRelease(demo.instance);

    glfw.windowHint(.{ .client_api = .none });
    const window = try glfw.createWindow(640, 480, "zgpu glTF Loader", null, null);
    defer glfw.destroyWindow(window);
    glfw.setWindowUserPointer(window, &demo);

    _ = glfw.setFramebufferSizeCallback(window, &struct {
        fn handle_glfw_framebuffer_size(w: *glfw.Window, width: i32, height: i32) callconv(.c) void {
            if (width <= 0 and height <= 0) return;
            const d: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse return));
            if (d.surface == null) return;
            d.config.width = @intCast(width);
            d.config.height = @intCast(height);
            wgpu.surfaceConfigure(d.surface, &d.config);
            createDepthTexture(d);
        }
    }.handle_glfw_framebuffer_size);

    if (comptime builtin.os.tag != .windows) {
        const x11_display = glfw.getX11Display();
        const x11_window = glfw.getX11Window(window);
        var xlib_source = wgpu.SurfaceSourceXlibWindow{ .chain = .{ .s_type = .surface_source_xlib_window }, .display = x11_display, .window = x11_window };
        demo.surface = wgpu.instanceCreateSurface(demo.instance, &wgpu.SurfaceDescriptor{ .next_in_chain = @ptrCast(&xlib_source) });
    } else {
        const win32_hwnd = glfw.getWin32Window(window);
        const win32_hinstance = glfw.getWin32ModuleHandle();
        var win32_source = wgpu.SurfaceSourceWindowsHWND{ .chain = .{ .s_type = .surface_source_windows_hwnd }, .hwnd = win32_hwnd, .hinstance = win32_hinstance };
        demo.surface = wgpu.instanceCreateSurface(demo.instance, &wgpu.SurfaceDescriptor{ .next_in_chain = @ptrCast(&win32_source) });
    }
    std.debug.assert(demo.surface != null);
    defer wgpu.surfaceRelease(demo.surface);

    _ = wgpu.instanceRequestAdapter(demo.instance, &wgpu.RequestAdapterOptions{ .compatible_surface = demo.surface }, .{ .callback = &struct {
        fn handle_request_adapter(status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
            _ = ud2;
            if (status == .success) {
                const d: *Demo = @ptrCast(@alignCast(ud1.?));
                d.adapter = adapter;
            } else {
                log.err("request_adapter failed: {s}", .{msg.toSlice()});
            }
        }
    }.handle_request_adapter, .userdata1 = &demo });
    while (demo.adapter == null) wgpu.instanceProcessEvents(demo.instance);
    defer wgpu.adapterRelease(demo.adapter);

    _ = wgpu.adapterRequestDevice(demo.adapter, null, .{ .callback = &struct {
        fn handle_request_device(status: wgpu.RequestDeviceStatus, device: wgpu.Device, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
            _ = ud2;
            if (status == .success) {
                const d: *Demo = @ptrCast(@alignCast(ud1.?));
                d.device = device;
            } else {
                log.err("request_device failed: {s}", .{msg.toSlice()});
            }
        }
    }.handle_request_device, .userdata1 = &demo });
    while (demo.device == null) wgpu.instanceProcessEvents(demo.instance);
    defer wgpu.deviceRelease(demo.device);

    defer if (demo.depth_view) |v| wgpu.textureViewRelease(v);
    defer if (demo.depth_texture) |t| wgpu.textureRelease(t);

    const queue = wgpu.deviceGetQueue(demo.device);
    defer wgpu.queueRelease(queue);
    var surface_capabilities: wgpu.SurfaceCapabilities = undefined;
    _ = wgpu.surfaceGetCapabilities(demo.surface, demo.adapter, &surface_capabilities);
    defer wgpu.surfaceCapabilitiesFreeMembers(surface_capabilities);

    const surface_format = surface_capabilities.formats.?[0];
    demo.config = .{
        .device = demo.device,
        .usage = .renderAttachmentUsage,
        .format = surface_format,
        .present_mode = .immediate,
        .alpha_mode = surface_capabilities.alpha_modes.?[0],
    };

    {
        var width: i32 = 0;
        var height: i32 = 0;
        glfw.getWindowSize(window, &width, &height);
        demo.config.width = @intCast(width);
        demo.config.height = @intCast(height);
    }
    wgpu.surfaceConfigure(demo.surface, &demo.config);
    createDepthTexture(&demo);

    // --- LOAD THE MODEL ---
    var model = try loadGltfModel(gpa, demo.device, queue, "assets/models/vertex-colors.glb");
    defer model.deinit();

    // --- CREATE UNIFORM BUFFER & BIND GROUP ---
    const camera_buffer = wgpu.deviceCreateBuffer(demo.device, &.{
        .label = .fromSlice("camera_buffer"),
        .usage = wgpu.BufferUsage.uniformUsage.merge(.copyDstUsage),
        .size = @sizeOf(CameraUniform),
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(camera_buffer);

    const camera_bind_group_layout = wgpu.deviceCreateBindGroupLayout(demo.device, &.{
        .label = .fromSlice("camera_bind_group_layout"),
        .entry_count = 1,
        .entries = &.{
            .{
                .binding = 0,
                .visibility = wgpu.ShaderStage.vertexStage,
                .buffer = .{ .type = .uniform, .has_dynamic_offset = .False, .min_binding_size = 0 },
            },
        },
    });
    defer wgpu.bindGroupLayoutRelease(camera_bind_group_layout);

    const camera_bind_group = wgpu.deviceCreateBindGroup(demo.device, &.{
        .label = .fromSlice("camera_bind_group"),
        .layout = camera_bind_group_layout,
        .entry_count = 1,
        .entries = &.{.{
            .binding = 0,
            .buffer = camera_buffer,
            .offset = 0,
            .size = @sizeOf(CameraUniform),
        }},
    });
    defer wgpu.bindGroupRelease(camera_bind_group);

    // --- Create Render Pipeline ---
    const shader_module = try wgpu.loadShaderText(demo.device, "gltf_shader.wgsl", shader_text);
    defer wgpu.shaderModuleRelease(shader_module);

    const pipeline_layout = wgpu.deviceCreatePipelineLayout(demo.device, &.{
        .label = .fromSlice("pipeline_layout"),
        .bind_group_layout_count = 1,
        .bind_group_layouts = &.{camera_bind_group_layout},
    });
    defer wgpu.pipelineLayoutRelease(pipeline_layout);

    const vertex_attributes = [_]wgpu.VertexAttribute{
        .{ .shaderLocation = 0, .offset = @offsetOf(Vertex, "position"), .format = .float32x3 },
        .{ .shaderLocation = 1, .offset = @offsetOf(Vertex, "normal"), .format = .float32x3 },
        .{ .shaderLocation = 2, .offset = @offsetOf(Vertex, "color"), .format = .float32x4 },
    };

    const vertex_buffer_layout = wgpu.VertexBufferLayout{
        .array_stride = @sizeOf(Vertex),
        .step_mode = .vertex,
        .attribute_count = vertex_attributes.len,
        .attributes = &vertex_attributes,
    };

    const color_target_state = wgpu.ColorTargetState{
        .format = surface_format,
        .blend = null,
        .write_mask = .all,
    };

    const fragment_state = wgpu.FragmentState{
        .module = shader_module,
        .entry_point = .fromSlice("fs_main"),
        .target_count = 1,
        .targets = &.{color_target_state},
    };

    const depth_stencil_state = wgpu.DepthStencilState{
        .format = DEPTH_FORMAT,
        .depth_write_enabled = .true,
        .depth_compare = .less,
        .stencil_front = .{},
        .stencil_back = .{},
        .stencil_read_mask = 0,
        .stencil_write_mask = 0,
        .depth_bias = 0,
        .depth_bias_slope_scale = 0.0,
        .depth_bias_clamp = 0.0,
    };

    const render_pipeline = wgpu.deviceCreateRenderPipeline(demo.device, &wgpu.RenderPipelineDescriptor{
        .label = .fromSlice("render_pipeline"),
        .layout = pipeline_layout,
        .vertex = .{
            .module = shader_module,
            .entry_point = .fromSlice("vs_main"),
            .buffer_count = 1,
            .buffers = &.{vertex_buffer_layout},
        },
        .primitive = .{
            .topology = .triangle_list,
            .strip_index_format = .undefined,
            .cull_mode = .back,
            .front_face = .ccw,
        },
        .depth_stencil = &depth_stencil_state,
        .multisample = .{
            .count = 1,
            .mask = 0xFFFFFFFF,
            .alpha_to_coverage_enabled = .False,
        },
        .fragment = &fragment_state,
    });
    defer wgpu.renderPipelineRelease(render_pipeline);

    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();

    const startup_ms = debug.start(&timer);

    log.info("startup completed in {d} ms", .{startup_ms});

    // --- Main Loop ---
    main_loop: while (!glfw.windowShouldClose(window)) {
        glfw.pollEvents();
        _ = arena_state.reset(.free_all);

        var camera_uniform: CameraUniform = undefined;
        {
            var width: i32 = 0;
            var height: i32 = 0;
            glfw.getFramebufferSize(window, &width, &height);
            const aspect = if (height == 0) 1.0 else @as(f32, @floatFromInt(width)) / @as(f32, @floatFromInt(height));

            const proj = linalg.mat4_perspective(linalg.deg_to_rad * 45.0, aspect, 0.1, 100.0);
            const view = linalg.mat4_look_at(
                .{ 8.0, 0.0, 8.0 }, // Eye position
                .{ 0.0, 0.0, 0.0 }, // Target (raised slightly)
                .{ 0.0, 1.0, 0.0 }, // Up vector
            );

            camera_uniform.view_proj = linalg.mat4_mul(proj, view);
        }
        wgpu.queueWriteBuffer(queue, camera_buffer, 0, &camera_uniform, @sizeOf(CameraUniform));

        var surface_texture: wgpu.SurfaceTexture = undefined;
        wgpu.surfaceGetCurrentTexture(demo.surface, &surface_texture);
        switch (surface_texture.status) {
            .success_optimal, .success_suboptimal => {},
            .timeout, .outdated, .lost => {
                if (surface_texture.texture != null) wgpu.textureRelease(surface_texture.texture);
                var width: i32 = 0;
                var height: i32 = 0;
                glfw.getWindowSize(window, &width, &height);
                if (width != 0 and height != 0) {
                    demo.config.width = @intCast(width);
                    demo.config.height = @intCast(height);
                    wgpu.surfaceConfigure(demo.surface, &demo.config);
                    createDepthTexture(&demo);
                }
                continue :main_loop;
            },
            else => std.debug.panic("get_current_texture status={any}", .{surface_texture.status}),
        }
        std.debug.assert(surface_texture.texture != null);
        defer wgpu.textureRelease(surface_texture.texture);

        const frame_view = wgpu.textureCreateView(surface_texture.texture, null);
        std.debug.assert(frame_view != null);
        defer wgpu.textureViewRelease(frame_view);

        const encoder = wgpu.deviceCreateCommandEncoder(demo.device, &.{ .label = .fromSlice("main_encoder") });
        defer wgpu.commandEncoderRelease(encoder);

        var depth_stencil_attachment = wgpu.RenderPassDepthStencilAttachment{
            .view = demo.depth_view,
            .depth_load_op = .clear,
            .depth_store_op = .store,
            .depth_clear_value = 1.0,
            .depth_read_only = .False,
            .stencil_load_op = .undefined,
            .stencil_store_op = .undefined,
        };

        const render_pass = wgpu.commandEncoderBeginRenderPass(encoder, &wgpu.RenderPassDescriptor{
            .label = .fromSlice("main_render_pass"),
            .color_attachment_count = 1,
            .color_attachments = &[_]wgpu.RenderPassColorAttachment{.{
                .view = frame_view,
                .resolve_target = null,
                .load_op = .clear,
                .store_op = .store,
                .clear_value = wgpu.Color{ .r = 0.1, .g = 0.2, .b = 0.3, .a = 1 },
            }},
            .depth_stencil_attachment = &depth_stencil_attachment,
        });

        wgpu.renderPassEncoderSetPipeline(render_pass, render_pipeline);
        wgpu.renderPassEncoderSetBindGroup(render_pass, 0, camera_bind_group, 0, null);

        // --- RENDER THE LOADED MODEL ---
        for (model.primitives) |primitive| {
            wgpu.renderPassEncoderSetVertexBuffer(render_pass, 0, primitive.vertex_buffer, 0, wgpu.whole_size);
            wgpu.renderPassEncoderSetIndexBuffer(render_pass, primitive.index_buffer, primitive.index_format, 0, wgpu.whole_size);
            wgpu.renderPassEncoderDrawIndexed(render_pass, primitive.index_count, 1, 0, 0, 0);
        }

        wgpu.renderPassEncoderEnd(render_pass);
        wgpu.renderPassEncoderRelease(render_pass);

        const cmd = wgpu.commandEncoderFinish(encoder, null);
        defer wgpu.commandBufferRelease(cmd);

        wgpu.queueSubmit(queue, 1, &.{cmd});

        _ = wgpu.surfacePresent(demo.surface);

        debug.lap();
    }
}
