//! Test of WGPU

const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");

const wgpu = @import("wgpu");
const glfw = @import("glfw");
const clay = @import("clay");

const debug = @import("../debug.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;
const vec3 = linalg.vec3;
const mat4 = linalg.mat4;

pub const std_options = std.Options{
    .log_level = .info,
};

test {
    log.debug("semantic analysis for examples/triangle_perspective.zig", .{});
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

// A struct to define our vertex data in 3D space.
const Vertex = extern struct {
    position: vec3, // Changed from vec2 to vec3
    color: vec3,
};

// A struct for our camera's view-projection matrix uniform.
const CameraUniform = extern struct {
    view_proj: mat4,
};

// The WGSL shader source code, now with a uniform matrix.
const shader_text =
    \\struct CameraUniform {
    \\    view_proj: mat4x4<f32>,
    \\};
    \\@group(0) @binding(0)
    \\var<uniform> u_camera: CameraUniform;
    \\
    \\struct VertexInput {
    \\    @location(0) position: vec3<f32>, // Changed from vec2
    \\    @location(1) color: vec3<f32>,
    \\};
    \\
    \\struct VertexOutput {
    \\    @builtin(position) clip_position: vec4<f32>,
    \\    @location(0) color: vec3<f32>,
    \\};
    \\
    \\@vertex
    \\fn vs_main(
    \\    model: VertexInput,
    \\) -> VertexOutput {
    \\    var out: VertexOutput;
    \\    // Transform the 3D vertex position by the view-projection matrix
    \\    out.clip_position = u_camera.view_proj * vec4<f32>(model.position, 1.0);
    \\    out.color = model.color;
    \\    return out;
    \\}
    \\
    \\@fragment
    \\fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    \\    return vec4<f32>(in.color, 1.0);
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
    const window = try glfw.createWindow(640, 480, "zgpu", null, null);
    defer glfw.destroyWindow(window);
    glfw.setWindowUserPointer(window, &demo);

    // --- UPDATE RESIZE CALLBACK ---
    // The resize callback must now also recreate the depth texture.
    _ = glfw.setFramebufferSizeCallback(window, &struct {
        fn handle_glfw_framebuffer_size(w: *glfw.Window, width: i32, height: i32) callconv(.c) void {
            if (width <= 0 and height <= 0) return;
            const d: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse return));
            if (d.surface == null) return;
            d.config.width = @intCast(width);
            d.config.height = @intCast(height);
            wgpu.surfaceConfigure(d.surface, &d.config);
            // Recreate depth texture with new size.
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

    // Make sure to release the depth resources on exit.
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
    // Create the depth texture for the first time.
    createDepthTexture(&demo);

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
    const shader_module = try wgpu.loadShaderText(demo.device, "triangle.wgsl", shader_text);
    defer wgpu.shaderModuleRelease(shader_module);

    const pipeline_layout = wgpu.deviceCreatePipelineLayout(demo.device, &.{
        .label = .fromSlice("pipeline_layout"),
        .bind_group_layout_count = 1,
        .bind_group_layouts = &.{camera_bind_group_layout},
    });
    defer wgpu.pipelineLayoutRelease(pipeline_layout);

    const vertex_attributes = [_]wgpu.VertexAttribute{
        .{ .shaderLocation = 0, .offset = @offsetOf(Vertex, "position"), .format = .float32x3 }, // format is now float32x3
        .{ .shaderLocation = 1, .offset = @offsetOf(Vertex, "color"), .format = .float32x3 },
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

    // --- CONFIGURE DEPTH/STENCIL STATE ---
    // Create a depth stencil state to enable depth testing.
    const depth_stencil_state = wgpu.DepthStencilState{
        .format = DEPTH_FORMAT,
        .depth_write_enabled = .true,
        .depth_compare = .less, // 1.
        .stencil_front = .{}, // 2.
        .stencil_back = .{}, // 2.
        .stencil_read_mask = 0, // 3.
        .stencil_write_mask = 0, // 3.
        .depth_bias = 0,
        .depth_bias_slope_scale = 0.0,
        .depth_bias_clamp = 0.0,
    };
    // 1. Fragments with smaller Z values (closer) will pass the test.
    // 2. We aren't using stencil operations, so these are empty.
    // 3. We aren't using stencil operations, so these are zero.

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
        // --- ATTACH DEPTH/STENCIL STATE TO PIPELINE ---
        .depth_stencil = &depth_stencil_state,
        .multisample = .{
            .count = 1,
            .mask = 0xFFFFFFFF,
            .alpha_to_coverage_enabled = .False,
        },
        .fragment = &fragment_state,
    });
    defer wgpu.renderPipelineRelease(render_pipeline);

    // --- ADD A SECOND TRIANGLE ---
    const vertices = [_]Vertex{
        // Original triangle (now in the back, at z=0.0)
        .{ .position = .{ 0.0, 0.5, 0.0 }, .color = .{ 1.0, 0.0, 0.0 } }, // Top
        .{ .position = .{ 0.5, -0.5, 0.0 }, .color = .{ 0.0, 0.0, 1.0 } }, // Bottom-right
        .{ .position = .{ -0.5, -0.5, 0.0 }, .color = .{ 0.0, 1.0, 0.0 } }, // Bottom-left
        // New magenta triangle (in front, at z=-0.5)
        .{ .position = .{ 0.0 - 0.2, 0.5 - 0.2, 0.5 }, .color = .{ 1.0, 0.0, 1.0 } }, // Top
        .{ .position = .{ 0.5 - 0.2, -0.5 - 0.2, 0.5 }, .color = .{ 1.0, 0.0, 1.0 } }, // Bottom-right
        .{ .position = .{ -0.5 - 0.2, -0.5 - 0.2, 0.5 }, .color = .{ 1.0, 0.0, 1.0 } }, // Bottom-left
    };
    const indices = [_]u32{
        0, 2, 1, // First triangle (CCW winding order)
        3, 5, 4, // Second triangle (CCW winding order)
    };

    const vertex_buffer = wgpu.deviceCreateBuffer(demo.device, &.{
        .label = .fromSlice("vertex_buffer"),
        .usage = wgpu.BufferUsage.vertexUsage.merge(.copyDstUsage),
        .size = @sizeOf(@TypeOf(vertices)),
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(vertex_buffer);
    wgpu.queueWriteBuffer(queue, vertex_buffer, 0, &vertices, @sizeOf(@TypeOf(vertices)));

    const index_buffer = wgpu.deviceCreateBuffer(demo.device, &.{
        .label = .fromSlice("index_buffer"),
        .usage = wgpu.BufferUsage.indexUsage.merge(.copyDstUsage),
        .size = @sizeOf(@TypeOf(indices)),
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(index_buffer);
    wgpu.queueWriteBuffer(queue, index_buffer, 0, &indices, @sizeOf(@TypeOf(indices)));

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
                .{ 0.5, 1.0, 2.0 }, // Eye position
                .{ 0.0, 0.0, 0.0 }, // Target
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
                    // Also recreate depth texture if surface is lost/outdated.
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

        // --- ATTACH DEPTH VIEW TO RENDER PASS ---
        var depth_stencil_attachment = wgpu.RenderPassDepthStencilAttachment{
            .view = demo.depth_view,
            .depth_load_op = .clear,
            .depth_store_op = .store,
            .depth_clear_value = 1.0,
            .depth_read_only = .False,
            .stencil_load_op = .undefined, // not using stencil
            .stencil_store_op = .undefined, // not using stencil
        };

        const render_pass = wgpu.commandEncoderBeginRenderPass(encoder, &wgpu.RenderPassDescriptor{
            .label = .fromSlice("main_render_pass"),
            .color_attachment_count = 1,
            .color_attachments = &[_]wgpu.RenderPassColorAttachment{.{
                .view = frame_view,
                .resolve_target = null,
                .load_op = .clear,
                .store_op = .store,
                .clear_value = wgpu.Color{ .r = 0.1, .g = 0.1, .b = 0.1, .a = 1 },
            }},
            .depth_stencil_attachment = &depth_stencil_attachment,
        });

        wgpu.renderPassEncoderSetPipeline(render_pass, render_pipeline);
        wgpu.renderPassEncoderSetBindGroup(render_pass, 0, camera_bind_group, 0, null);
        wgpu.renderPassEncoderSetIndexBuffer(render_pass, index_buffer, .uint32, 0, @sizeOf(@TypeOf(indices)));
        wgpu.renderPassEncoderSetVertexBuffer(render_pass, 0, vertex_buffer, 0, @sizeOf(@TypeOf(vertices)));
        // --- DRAW ALL 6 INDICES ---
        wgpu.renderPassEncoderDrawIndexed(render_pass, indices.len, 1, 0, 0, 0);

        wgpu.renderPassEncoderEnd(render_pass);
        wgpu.renderPassEncoderRelease(render_pass);

        const cmd = wgpu.commandEncoderFinish(encoder, null);
        defer wgpu.commandBufferRelease(cmd);

        wgpu.queueSubmit(queue, 1, &.{cmd});

        _ = wgpu.surfacePresent(demo.surface);

        debug.lap();
    }
}
