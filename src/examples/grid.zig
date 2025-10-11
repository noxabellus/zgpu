//! Test of Voxel Grid

const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");

const wgpu = @import("wgpu");
const glfw = @import("glfw");

const debug = @import("../debug.zig");
const linalg = @import("../linalg.zig");
const Grid = @import("../Grid.zig");

const vec2 = linalg.vec2;
const vec3 = linalg.vec3;
const mat4 = linalg.mat4;

pub const std_options = std.Options{
    .log_level = .info,
};

test {
    log.debug("semantic analysis for examples/grid.zig", .{});
    std.testing.refAllDecls(@This());
}

const Demo = struct {
    instance: wgpu.Instance = null,
    surface: wgpu.Surface = null,
    adapter: wgpu.Adapter = null,
    device: wgpu.Device = null,
    config: wgpu.SurfaceConfiguration = .{},

    // --- Camera state for fly controls ---
    camera_pos: vec3 = .{ 8.125, 8.125, 28.125 }, // Start outside the large sphere
    camera_front: vec3 = .{ 0.0, 0.0, -1.0 },
    camera_up: vec3 = .{ 0.0, 1.0, 0.0 },
    camera_right: vec3 = .{ 0.0, 0.0, 0.0 }, // will be calculated

    yaw: f32 = -90.0,
    pitch: f32 = 0.0,

    first_mouse: bool = true,
    last_x: f64 = 400.0,
    last_y: f64 = 300.0,

    // Timing
    delta_time: f32 = 0.0,
    last_frame: f32 = 0.0,
};

// --- Mouse movement callback ---
fn handle_mouse_move(w: *glfw.Window, xpos: f64, ypos: f64) callconv(.c) void {
    const d: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse return));

    const btn_state = glfw.getMouseButton(w, .button_3);
    if (btn_state != .repeat and btn_state != .press) {
        d.first_mouse = true;
        return;
    }

    if (d.first_mouse) {
        d.last_x = xpos;
        d.last_y = ypos;
        d.first_mouse = false;
    }

    var x_offset = xpos - d.last_x;
    var y_offset = d.last_y - ypos; // reversed since y-coordinates go from top to bottom
    d.last_x = xpos;
    d.last_y = ypos;

    const sensitivity: f64 = 0.05;
    x_offset *= sensitivity;
    y_offset *= sensitivity;

    d.yaw += @floatCast(x_offset);
    d.pitch += @floatCast(y_offset);

    // constrain pitch
    if (d.pitch > 89.0) {
        d.pitch = 89.0;
    }
    if (d.pitch < -89.0) {
        d.pitch = -89.0;
    }

    var front: vec3 = undefined;
    const yaw_rad = linalg.deg_to_rad * d.yaw;
    const pitch_rad = linalg.deg_to_rad * d.pitch;
    front[0] = std.math.cos(yaw_rad) * std.math.cos(pitch_rad);
    front[1] = std.math.sin(pitch_rad);
    front[2] = std.math.sin(yaw_rad) * std.math.cos(pitch_rad);
    d.camera_front = linalg.normalize(front);
    // Also re-calculate the Right and Up vector
    d.camera_right = linalg.normalize(linalg.cross(d.camera_front, .{ 0.0, 1.0, 0.0 }));
    d.camera_up = linalg.normalize(linalg.cross(d.camera_right, d.camera_front));
}

// Use the Vertex definition from the Grid module, as that's what the mesher produces.
const Vertex = Grid.Vertex;

// A struct for our camera's view-projection matrix uniform.
const CameraUniform = extern struct {
    view_proj: mat4,
};

// The WGSL shader source code.
// This now includes basic directional lighting using the vertex normal.
const shader_text =
    \\struct CameraUniform {
    \\    view_proj: mat4x4<f32>,
    \\};
    \\@group(0) @binding(0)
    \\var<uniform> u_camera: CameraUniform;
    \\
    \\struct VertexInput {
    \\    @location(0) pos: vec3<f32>,
    \\    @location(1) norm: vec3<f32>,
    \\    @location(2) uv: vec2<f32>,
    \\    @location(3) color: vec3<f32>,
    \\};
    \\
    \\struct VertexOutput {
    \\    @builtin(position) clip_position: vec4<f32>,
    \\    @location(0) norm: vec3<f32>,
    \\    @location(1) color: vec3<f32>,
    \\};
    \\
    \\@vertex
    \\fn vs_main(
    \\    model: VertexInput,
    \\) -> VertexOutput {
    \\    var out: VertexOutput;
    \\    out.clip_position = u_camera.view_proj * vec4<f32>(model.pos, 1.0);
    \\    out.norm = model.norm;
    \\    out.color = model.color;
    \\    return out;
    \\}
    \\
    \\@fragment
    \\fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    \\    // Basic directional lighting
    \\    let light_dir = normalize(vec3<f32>(0.5, 1.0, 0.75));
    \\    let ambient = 0.2;
    \\    let diffuse = max(dot(in.norm, light_dir), 0.0) * 0.8;
    \\    let intensity = ambient + diffuse;
    \\    return vec4<f32>(in.color * intensity, 1.0);
    \\}
;

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

    // Initial camera vector calculations
    demo.camera_right = linalg.normalize(linalg.cross(demo.camera_front, .{ 0.0, 1.0, 0.0 }));
    demo.camera_up = linalg.normalize(linalg.cross(demo.camera_right, demo.camera_front));

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
    wgpu.setLogLevel(.warn); // Set to warn to reduce noise
    demo.instance = wgpu.createInstance(&wgpu.InstanceDescriptor{ .next_in_chain = @ptrCast(&instance_extras) });
    std.debug.assert(demo.instance != null);
    defer wgpu.instanceRelease(demo.instance);

    glfw.windowHint(.{ .client_api = .none });
    const window = try glfw.createWindow(800, 600, "Voxel Grid", null, null);
    defer glfw.destroyWindow(window);
    glfw.setWindowUserPointer(window, &demo);

    // --- Setup window for camera input ---
    // glfw.setInputMode(window, .{ .cursor = .captured });
    _ = glfw.setCursorPosCallback(window, handle_mouse_move);

    _ = glfw.setFramebufferSizeCallback(window, &struct {
        fn handle_glfw_framebuffer_size(w: *glfw.Window, width: i32, height: i32) callconv(.c) void {
            if (width <= 0 and height <= 0) return;
            const d: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse return));
            if (d.surface == null) return;
            d.config.width = @intCast(width);
            d.config.height = @intCast(height);
            wgpu.surfaceConfigure(d.surface, &d.config);
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
        .present_mode = .fifo, // Use fifo for vsync
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
    const shader_module = try wgpu.loadShaderText(demo.device, "grid.wgsl", shader_text);
    defer wgpu.shaderModuleRelease(shader_module);

    const pipeline_layout = wgpu.deviceCreatePipelineLayout(demo.device, &.{
        .label = .fromSlice("pipeline_layout"),
        .bind_group_layout_count = 1,
        .bind_group_layouts = &.{camera_bind_group_layout},
    });
    defer wgpu.pipelineLayoutRelease(pipeline_layout);

    // Vertex attributes now match Grid.Vertex: pos, norm, uv, color
    const vertex_attributes = [_]wgpu.VertexAttribute{
        .{ .shaderLocation = 0, .offset = @offsetOf(Vertex, "pos"), .format = .float32x3 },
        .{ .shaderLocation = 1, .offset = @offsetOf(Vertex, "norm"), .format = .float32x3 },
        .{ .shaderLocation = 2, .offset = @offsetOf(Vertex, "uv"), .format = .float32x2 },
        .{ .shaderLocation = 3, .offset = @offsetOf(Vertex, "color"), .format = .float32x3 },
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

    // We need a depth buffer for 3D rendering
    const depth_texture = wgpu.deviceCreateTexture(demo.device, &.{
        .label = .fromSlice("depth_texture"),
        .size = .{ .width = demo.config.width, .height = demo.config.height, .depth_or_array_layers = 1 },
        .mip_level_count = 1,
        .sample_count = 1,
        .dimension = .@"2d",
        .format = .depth32_float,
        .usage = .renderAttachmentUsage,
    });
    defer wgpu.textureRelease(depth_texture);
    const depth_view = wgpu.textureCreateView(depth_texture, null);
    defer wgpu.textureViewRelease(depth_view);

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
            .cull_mode = .back, // Cull back faces for solid objects
            .front_face = .ccw,
        },
        .depth_stencil = &wgpu.DepthStencilState{
            .format = .depth32_float,
            .depth_write_enabled = .true,
            .depth_compare = .less,
        },
        .multisample = .{
            .count = 1,
            .mask = 0xFFFFFFFF,
            .alpha_to_coverage_enabled = .False,
        },
        .fragment = &fragment_state,
    });
    defer wgpu.renderPipelineRelease(render_pipeline);

    // --- Create Grid and Generate Mesh ---
    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();
    const frame_arena = arena_state.allocator();

    var grid = try Grid.init(gpa);
    defer grid.deinit();

    // --- DEFINE MATERIALS ---
    const stone_mat = try grid.bindMaterial(.{ .is_opaque = true, .color = .{ 0.5, 0.55, 0.6 } });
    const stone: Grid.LiteVoxel = .{ .material_id = stone_mat };

    // Define a second material for dirt
    const dirt_mat = try grid.bindMaterial(.{ .is_opaque = true, .color = .{ 0.6, 0.4, 0.2 } });
    const dirt: Grid.LiteVoxel = .{ .material_id = dirt_mat };

    // --- GENERATE A SPHERE ---
    const radius: i32 = 32;
    const radius_sq = @as(f32, @floatFromInt(radius * radius));
    {
        var z: i32 = -radius;
        while (z <= radius) : (z += 1) {
            var y: i32 = -radius;
            while (y <= radius) : (y += 1) {
                var x: i32 = -radius;
                while (x <= radius) : (x += 1) {
                    const xf = @as(f32, @floatFromInt(x));
                    const yf = @as(f32, @floatFromInt(y));
                    const zf = @as(f32, @floatFromInt(z));

                    // Check if the point is within the sphere's radius
                    if (xf * xf + yf * yf + zf * zf <= radius_sq) {
                        // Use dirt for the top half, stone for the bottom half
                        if (y <= 0) {
                            try grid.setVoxel(.{ x + radius, y + radius, z + radius }, stone);
                        } else {
                            try grid.setVoxel(.{ x + radius, y + radius, z + radius }, dirt);
                        }
                    }
                }
            }
        }
    }

    log.info("generated sphere with radius {d} voxels", .{radius});

    // Update the grid to calculate visibility, etc.
    try grid.update(frame_arena);

    log.info("grid update complete", .{});

    {
        var page_it = grid.pages.iterator();
        while (page_it.next()) |page_ptrs| {
            const page_pos = page_ptrs.key_ptr.*;
            const page = page_ptrs.value_ptr.*;

            log.info("page at {d}: hetero: {any}, homo: {any}", .{ page_pos, page.isHeterogeneous(), page.homogeneous });

            var voxeme_it = page.heterogeneous.iterator();
            while (voxeme_it.next()) |voxeme_ptrs| {
                const voxeme_pos = voxeme_ptrs.key_ptr.*;
                const voxeme_level_1 = voxeme_ptrs.value_ptr.*;

                log.info("  voxeme at {}: homo: {any}", .{ voxeme_pos, voxeme_level_1.is_homogeneous });

                if (voxeme_level_1.is_homogeneous) {
                    log.info("    {any} ", .{voxeme_level_1.payload.homogeneous});
                } else {
                    log.info("    skipping display of dense heterogeneous collection", .{});
                }
            }
        }
    }

    // Generate the mesh for the world containing our sphere
    var vertices = std.ArrayList(Grid.Vertex).empty;
    defer vertices.deinit(gpa);
    var indices = std.ArrayList(u32).empty;
    defer indices.deinit(gpa);

    try grid.worldMeshBasic(gpa, .{ 0, 0, 0 }, .{ 1, 1, 1 }, &vertices, &indices);

    log.info("generated mesh with {d} vertices and {d} indices", .{ vertices.items.len, indices.items.len });

    // --- Create Buffers from Mesh Data ---
    const vertex_buffer = wgpu.deviceCreateBuffer(demo.device, &.{
        .label = .fromSlice("vertex_buffer"),
        .usage = wgpu.BufferUsage.vertexUsage.merge(.copyDstUsage),
        .size = vertices.items.len * @sizeOf(Vertex),
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(vertex_buffer);
    wgpu.queueWriteBuffer(queue, vertex_buffer, 0, vertices.items.ptr, vertices.items.len * @sizeOf(Vertex));

    const index_buffer = wgpu.deviceCreateBuffer(demo.device, &.{
        .label = .fromSlice("index_buffer"),
        .usage = wgpu.BufferUsage.indexUsage.merge(.copyDstUsage),
        .size = indices.items.len * @sizeOf(u32),
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(index_buffer);
    wgpu.queueWriteBuffer(queue, index_buffer, 0, indices.items.ptr, indices.items.len * @sizeOf(u32));

    const startup_ms = debug.start(&timer);

    log.info("startup completed in {d} ms", .{startup_ms});

    var frame_timer = try std.time.Timer.start();

    // --- Main Loop ---
    main_loop: while (!glfw.windowShouldClose(window)) {
        glfw.pollEvents();
        _ = arena_state.reset(.free_all);

        // --- Delta time calculation ---
        const ns_since_start = frame_timer.read();
        const current_frame = @as(f32, @floatFromInt(ns_since_start)) / std.time.ns_per_s;
        demo.delta_time = current_frame - demo.last_frame;
        demo.last_frame = current_frame;

        // --- Process keyboard input ---
        const camera_speed = 10.0 * demo.delta_time; // Adjust speed as needed
        if (glfw.getKey(window, .w) == .press) {
            demo.camera_pos = demo.camera_pos + demo.camera_front * @as(vec3, @splat(camera_speed));
        }
        if (glfw.getKey(window, .s) == .press) {
            demo.camera_pos = demo.camera_pos - demo.camera_front * @as(vec3, @splat(camera_speed));
        }
        if (glfw.getKey(window, .a) == .press) {
            demo.camera_pos = demo.camera_pos - demo.camera_right * @as(vec3, @splat(camera_speed));
        }
        if (glfw.getKey(window, .d) == .press) {
            demo.camera_pos = demo.camera_pos + demo.camera_right * @as(vec3, @splat(camera_speed));
        }
        if (glfw.getKey(window, .e) == .press) {
            demo.camera_pos = demo.camera_pos + demo.camera_up * @as(vec3, @splat(camera_speed));
        }
        if (glfw.getKey(window, .q) == .press) {
            demo.camera_pos = demo.camera_pos - demo.camera_up * @as(vec3, @splat(camera_speed));
        }

        // --- Calculate the view-projection matrix from fly camera state ---
        var camera_uniform: CameraUniform = undefined;
        {
            var width: i32 = 0;
            var height: i32 = 0;
            glfw.getFramebufferSize(window, &width, &height);
            const aspect = if (height == 0) 1.0 else @as(f32, @floatFromInt(width)) / @as(f32, @floatFromInt(height));

            const proj = linalg.mat4_perspective(linalg.deg_to_rad * 60.0, aspect, 0.1, 1000.0);

            const view = linalg.mat4_look_at(
                demo.camera_pos,
                demo.camera_pos + demo.camera_front,
                demo.camera_up,
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
            .depth_stencil_attachment = &wgpu.RenderPassDepthStencilAttachment{
                .view = depth_view,
                .depth_load_op = .clear,
                .depth_store_op = .store,
                .depth_clear_value = 1.0,
                .depth_read_only = .False,
                .stencil_load_op = .undefined,
                .stencil_store_op = .undefined,
            },
        });

        // --- SET BIND GROUP & DRAW ---
        if (indices.items.len > 0) {
            wgpu.renderPassEncoderSetPipeline(render_pass, render_pipeline);
            wgpu.renderPassEncoderSetBindGroup(render_pass, 0, camera_bind_group, 0, null);
            wgpu.renderPassEncoderSetIndexBuffer(render_pass, index_buffer, .uint32, 0, indices.items.len * @sizeOf(u32));
            wgpu.renderPassEncoderSetVertexBuffer(render_pass, 0, vertex_buffer, 0, vertices.items.len * @sizeOf(Vertex));
            wgpu.renderPassEncoderDrawIndexed(render_pass, @intCast(indices.items.len), 1, 0, 0, 0);
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
