const std = @import("std");
const log = std.log.scoped(.main);

const builtin = @import("builtin");

const wgpu = @import("wgpu.zig");
const glfw = @import("glfw.zig");
const stbi = @import("stbi.zig");

pub const std_options = std.Options{
    .log_level = .warn,
};

test {
    log.debug("semantic analysis for main.zig", .{});
    std.testing.refAllDecls(@This());
}

// --- Application State Struct ---
const Demo = struct {
    instance: wgpu.Instance = null,
    surface: wgpu.Surface = null,
    adapter: wgpu.Adapter = null,
    device: wgpu.Device = null,
    config: wgpu.SurfaceConfiguration = .{},
};

// --- Vertex and Uniform Data Structs ---
const Vertex = extern struct {
    position: [2]f32,
    tex_coords: [2]f32,
};

const Uniforms = extern struct {
    resolution: [2]f32,
};

pub fn main() !void {
    _ = wgpu.setLogCallback(&struct {
        fn log_callback(level: wgpu.LogLevel, message: wgpu.StringView, userdata: ?*anyopaque) callconv(.c) void {
            _ = userdata;
            const level_str = switch (level) {
                .@"error" => "error",
                .warn => "warn",
                .info => "info",
                .debug => "debug",
                .trace => "trace",
                else => "unknown_level",
            };
            const text = message.toSlice();
            inline for (&.{"Suboptimal present of frame"}) |ignore_pattern| {
                if (std.mem.indexOf(u8, text, ignore_pattern) != null) return;
            }
            const wgpu_log = std.log.scoped(.wgpu);
            switch (level) {
                .@"error" => wgpu_log.err("{s}", .{text}),
                .warn => wgpu_log.warn("{s}", .{text}),
                .info => wgpu_log.info("{s}", .{text}),
                .debug => wgpu_log.debug("{s}", .{text}),
                else => wgpu_log.debug("[{s}] {s}", .{ level_str, text }),
            }
        }
    }.log_callback, null);
    wgpu.setLogLevel(.warn);

    if (comptime builtin.os.tag != .windows) {
        glfw.initHint(.{ .platform = .x11 });
    } else {
        glfw.initHint(.{ .platform = .win32 });
    }

    try glfw.init();
    defer glfw.deinit();

    var demo = Demo{};
    demo.instance = wgpu.createInstance(null);
    std.debug.assert(demo.instance != null);
    defer wgpu.instanceRelease(demo.instance);

    const initial_width: i32 = 640;
    const initial_height: i32 = 480;

    glfw.windowHint(.{ .client_api = .none });
    const window = try glfw.createWindow(initial_width, initial_height, "gpu-quad [zig / wgpu + glfw + stb_image]", null, null);
    defer glfw.destroyWindow(window);

    glfw.setWindowUserPointer(window, &demo);
    _ = glfw.setKeyCallback(window, &struct {
        fn handle_glfw_key(w: *glfw.Window, key: glfw.Key, scancode: i32, action: glfw.KeyState32, mods: glfw.Modifier) callconv(.c) void {
            _ = scancode;
            _ = mods;
            if (key == .r and (action == .press or action == .repeat)) {
                const st: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse return));
                if (st.instance == null) return;
                var report: wgpu.GlobalReport = undefined;
                wgpu.generateReport(st.instance, &report);
                wgpu.printGlobalReport(report);
            }
        }
    }.handle_glfw_key);
    _ = glfw.setFramebufferSizeCallback(window, &struct {
        fn handle_glfw_framebuffer_size(w: *glfw.Window, width: i32, height: i32) callconv(.c) void {
            if (width <= 0 and height <= 0) return;
            const st: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse return));
            if (st.surface == null) return;
            st.config.width = @intCast(width);
            st.config.height = @intCast(height);
            wgpu.surfaceConfigure(st.surface, &st.config);
        }
    }.handle_glfw_framebuffer_size);

    if (comptime builtin.os.tag != .windows) {
        const x11_display = glfw.getX11Display();
        const x11_window = glfw.getX11Window(window);
        std.debug.assert(x11_display != null and x11_window != 0);
        var xlib_source = wgpu.SurfaceSourceXlibWindow{ .chain = .{ .s_type = .surface_source_xlib_window }, .display = x11_display, .window = x11_window };
        demo.surface = wgpu.instanceCreateSurface(demo.instance, &wgpu.SurfaceDescriptor{ .next_in_chain = @ptrCast(&xlib_source) });
    } else {
        // TODO: glfw binding for windows
        // const win32_hwnd = glfw.getWin32Window(window);
        // std.debug.assert(win32_hwnd != null);
        // var win32_source = wgpu.SurfaceSourceWindowsHWND{ .chain = .{ .s_type = .surface_source_win32_window }, .hwnd = win32_hwnd, .hinstance = null };
        // demo.surface = wgpu.instanceCreateSurface(demo.instance, &wgpu.SurfaceDescriptor{ .next_in_chain = @ptrCast(&win32_source) });
        @panic("NYI");
    }
    std.debug.assert(demo.surface != null);
    defer wgpu.surfaceRelease(demo.surface);

    _ = wgpu.instanceRequestAdapter(demo.instance, &wgpu.RequestAdapterOptions{ .compatible_surface = demo.surface }, .{ .callback = &struct {
        fn handle_request_adapter(status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, message: wgpu.StringView, userdata1: ?*anyopaque, userdata2: ?*anyopaque) callconv(.c) void {
            _ = userdata2;
            if (status == .success) {
                const state: *Demo = @ptrCast(@alignCast(userdata1.?));
                state.adapter = adapter;
            } else {
                if (message.data) |data| std.debug.print("request_adapter status={any} message={s}\n", .{ status, data[0..message.length] });
            }
        }
    }.handle_request_adapter, .userdata1 = &demo });
    while (demo.adapter == null) wgpu.instanceProcessEvents(demo.instance);
    defer wgpu.adapterRelease(demo.adapter);

    wgpu.printAdapterInfo(demo.adapter);

    _ = wgpu.adapterRequestDevice(demo.adapter, null, .{ .callback = &struct {
        fn handle_request_device(status: wgpu.RequestDeviceStatus, device: wgpu.Device, message: wgpu.StringView, userdata1: ?*anyopaque, userdata2: ?*anyopaque) callconv(.c) void {
            _ = userdata2;
            if (status == .success) {
                const state: *Demo = @ptrCast(@alignCast(userdata1.?));
                state.device = device;
            } else {
                if (message.data) |data| std.debug.print("request_device status={any} message={s}\n", .{ status, data[0..message.length] });
            }
        }
    }.handle_request_device, .userdata1 = &demo });
    while (demo.device == null) wgpu.instanceProcessEvents(demo.instance);
    defer wgpu.deviceRelease(demo.device);

    const queue = wgpu.deviceGetQueue(demo.device);
    std.debug.assert(queue != null);
    defer wgpu.queueRelease(queue);

    const vertex_count = 6;
    const vertex_data_size = vertex_count * @sizeOf(Vertex);

    // --- Create Vertex and Staging Buffers ---
    // The vertex buffer will live on the GPU and is the destination for our vertex data.
    // It's not directly writable by the CPU, making it faster for the GPU to access.
    const vertex_buffer = wgpu.deviceCreateBuffer(demo.device, &wgpu.BufferDescriptor{
        .label = .fromSlice("vertex_buffer"),
        .usage = wgpu.BufferUsage.vertexUsage.merge(.copyDstUsage), // copyDst is needed for the staging copy
        .size = vertex_data_size,
    });
    std.debug.assert(vertex_buffer != null);
    defer wgpu.bufferRelease(vertex_buffer);

    // The staging buffer is a temporary buffer that is writable by the CPU.
    // We will write our vertex data here each frame and then command the GPU
    // to copy it to the main vertex buffer.
    const staging_buffer = wgpu.deviceCreateBuffer(demo.device, &wgpu.BufferDescriptor{
        .label = .fromSlice("staging_buffer"),
        .usage = wgpu.BufferUsage.mapWriteUsage.merge(.copySrcUsage), // mapWrite for CPU, copySrc for GPU
        .size = vertex_data_size,
    });
    std.debug.assert(staging_buffer != null);
    defer wgpu.bufferRelease(staging_buffer);

    // --- Create Uniform Buffer ---
    const uniform_buffer = wgpu.deviceCreateBuffer(demo.device, &wgpu.BufferDescriptor{
        .label = .fromSlice("uniform_buffer"),
        .usage = wgpu.BufferUsage.uniformUsage.merge(.copyDstUsage),
        .size = @sizeOf(Uniforms),
    });
    std.debug.assert(uniform_buffer != null);
    defer wgpu.bufferRelease(uniform_buffer);

    // --- Load Texture ---
    var image = try stbi.Image8.fromPath("assets/wgpu-logo.png", .rgba);
    defer image.deinit();
    std.debug.print("Loaded image: {any}\n", .{image.info});

    const texture_size = wgpu.Extent3D{ .width = image.info.width, .height = image.info.height, .depth_or_array_layers = 1 };
    const texture = wgpu.deviceCreateTexture(demo.device, &wgpu.TextureDescriptor{
        .label = .fromSlice("texture"),
        .size = texture_size,
        .mip_level_count = 1,
        .sample_count = 1,
        .dimension = .@"2d",
        .format = .rgba8_unorm_srgb,
        .usage = wgpu.TextureUsage.textureBindingUsage.merge(.copyDstUsage),
    });
    std.debug.assert(texture != null);
    defer wgpu.textureRelease(texture);
    wgpu.queueWriteTexture(queue, &wgpu.TexelCopyTextureInfo{ .texture = texture }, image.buffer.ptr, image.buffer.len, &wgpu.TexelCopyBufferLayout{ .bytes_per_row = 4 * image.info.width, .rows_per_image = image.info.height }, &texture_size);

    const texture_view = wgpu.textureCreateView(texture, null);
    std.debug.assert(texture_view != null);
    defer wgpu.textureViewRelease(texture_view);
    const sampler = wgpu.deviceCreateSampler(demo.device, &wgpu.SamplerDescriptor{ .label = .fromSlice("sampler"), .mag_filter = .linear, .min_filter = .linear });
    std.debug.assert(sampler != null);
    defer wgpu.samplerRelease(sampler);

    // --- Create Bind Group Layout and Bind Group ---
    const bind_group_layout = wgpu.deviceCreateBindGroupLayout(demo.device, &wgpu.BindGroupLayoutDescriptor{
        .label = .fromSlice("bind_group_layout"),
        .entry_count = 3,
        .entries = &[_]wgpu.BindGroupLayoutEntry{
            .{ .binding = 0, .visibility = .fragmentStage, .sampler = .{ .type = .filtering } },
            .{ .binding = 1, .visibility = .fragmentStage, .texture = .{ .sample_type = .float, .view_dimension = .@"2d" } },
            .{ .binding = 2, .visibility = .vertexStage, .buffer = .{ .type = .uniform } },
        },
    });
    std.debug.assert(bind_group_layout != null);
    defer wgpu.bindGroupLayoutRelease(bind_group_layout);
    const bind_group = wgpu.deviceCreateBindGroup(demo.device, &wgpu.BindGroupDescriptor{
        .label = .fromSlice("bind_group"),
        .layout = bind_group_layout,
        .entry_count = 3,
        .entries = &[_]wgpu.BindGroupEntry{
            .{ .binding = 0, .sampler = sampler },
            .{ .binding = 1, .texture_view = texture_view },
            .{ .binding = 2, .buffer = uniform_buffer, .offset = 0, .size = @sizeOf(Uniforms) },
        },
    });
    std.debug.assert(bind_group != null);
    defer wgpu.bindGroupRelease(bind_group);

    // --- Create Pipeline ---
    const shader_module = try wgpu.loadShader(demo.device, "assets/screenspace_2d.wgsl");
    defer wgpu.shaderModuleRelease(shader_module);
    const pipeline_layout = wgpu.deviceCreatePipelineLayout(demo.device, &wgpu.PipelineLayoutDescriptor{
        .label = .fromSlice("pipeline_layout"),
        .bind_group_layout_count = 1,
        .bind_group_layouts = &[_]wgpu.BindGroupLayout{bind_group_layout},
    });
    std.debug.assert(pipeline_layout != null);
    defer wgpu.pipelineLayoutRelease(pipeline_layout);
    var surface_capabilities: wgpu.SurfaceCapabilities = undefined;
    _ = wgpu.surfaceGetCapabilities(demo.surface, demo.adapter, &surface_capabilities);
    defer wgpu.surfaceCapabilitiesFreeMembers(surface_capabilities);
    const blend_state = wgpu.BlendState{ .color = .{ .operation = .add, .src_factor = .src_alpha, .dst_factor = .one_minus_src_alpha }, .alpha = .{ .operation = .add, .src_factor = .one, .dst_factor = .zero } };
    const vertex_buffer_layout = wgpu.VertexBufferLayout{ .array_stride = @sizeOf(Vertex), .step_mode = .vertex, .attribute_count = 2, .attributes = &[_]wgpu.VertexAttribute{ .{ .shaderLocation = 0, .offset = @offsetOf(Vertex, "position"), .format = .float32x2 }, .{ .shaderLocation = 1, .offset = @offsetOf(Vertex, "tex_coords"), .format = .float32x2 } } };
    const render_pipeline = wgpu.deviceCreateRenderPipeline(demo.device, &wgpu.RenderPipelineDescriptor{
        .label = .fromSlice("render_pipeline"),
        .layout = pipeline_layout,
        .vertex = .{ .module = shader_module, .entry_point = .fromSlice("vs_main"), .buffer_count = 1, .buffers = &.{vertex_buffer_layout} },
        .fragment = &wgpu.FragmentState{ .module = shader_module, .entry_point = .fromSlice("fs_main"), .target_count = 1, .targets = &[_]wgpu.ColorTargetState{.{ .format = surface_capabilities.formats.?[0], .blend = &blend_state, .write_mask = .all }} },
        .primitive = .{ .topology = .triangle_list },
        .multisample = .{ .count = 1, .mask = 0xFFFFFFFF },
    });
    std.debug.assert(render_pipeline != null);
    defer wgpu.renderPipelineRelease(render_pipeline);

    // --- Configure Surface ---
    demo.config = .{ .device = demo.device, .usage = .renderAttachmentUsage, .format = surface_capabilities.formats.?[0], .present_mode = .fifo, .alpha_mode = surface_capabilities.alpha_modes.?[0] };
    {
        var width: i32 = 0;
        var height: i32 = 0;
        glfw.getWindowSize(window, &width, &height);
        demo.config.width = @intCast(width);
        demo.config.height = @intCast(height);
    }
    wgpu.surfaceConfigure(demo.surface, &demo.config);

    // --- Main Loop ---
    main_loop: while (!glfw.windowShouldClose(window)) {
        glfw.pollEvents();

        // --- Regenerate and Upload Mesh Data via Staging Buffer ---
        // 1. Asynchronously request to map the staging buffer for writing.
        _ = wgpu.bufferMapAsync(staging_buffer, .writeMode, 0, vertex_data_size, .{ .callback = &struct {
            fn callback(status: wgpu.MapAsyncStatus, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
                _ = msg;
                _ = ud1;
                _ = ud2;
                if (status != .success) std.debug.print("Failed to map staging buffer: {any}\n", .{status});
            }
        }.callback });

        // 2. Poll the device to wait for the mapping to complete.
        // In a complex engine, you would use the async callback to avoid blocking the main thread.
        // For this example, polling is simpler and effectively synchronizes the operation.
        _ = wgpu.devicePoll(demo.device, .true, null);

        // 3. Get a pointer to the mapped memory and write the new vertex data into it.
        const mapped_range = wgpu.bufferGetMappedRange(staging_buffer, 0, vertex_data_size);
        if (mapped_range) |ptr| {
            const data: [*]Vertex = @ptrCast(@alignCast(ptr));
            var cursor_x: f64 = 0;
            var cursor_y: f64 = 0;
            glfw.getCursorPos(window, &cursor_x, &cursor_y);
            const quad_width: f32 = @as(f32, @floatFromInt(initial_width)) / 2.0;
            const quad_height: f32 = @as(f32, @floatFromInt(initial_height)) / 2.0;
            const x1: f32 = @as(f32, @floatCast(cursor_x)) - quad_width / 2.0;
            const y1: f32 = @as(f32, @floatCast(cursor_y)) - quad_height / 2.0;
            const x2: f32 = @as(f32, @floatCast(cursor_x)) + quad_width / 2.0;
            const y2: f32 = @as(f32, @floatCast(cursor_y)) + quad_height / 2.0;

            const vertices = [_]Vertex{
                .{ .position = .{ x1, y1 }, .tex_coords = .{ 0.0, 0.0 } }, .{ .position = .{ x2, y1 }, .tex_coords = .{ 1.0, 0.0 } }, .{ .position = .{ x1, y2 }, .tex_coords = .{ 0.0, 1.0 } },
                .{ .position = .{ x1, y2 }, .tex_coords = .{ 0.0, 1.0 } }, .{ .position = .{ x2, y1 }, .tex_coords = .{ 1.0, 0.0 } }, .{ .position = .{ x2, y2 }, .tex_coords = .{ 1.0, 1.0 } },
            };
            @memcpy(data[0..vertex_count], &vertices);
            wgpu.bufferUnmap(staging_buffer);
        }

        // --- Update Uniforms ---
        const uniforms = Uniforms{ .resolution = .{ @floatFromInt(demo.config.width), @floatFromInt(demo.config.height) } };
        wgpu.queueWriteBuffer(queue, uniform_buffer, 0, &uniforms, @sizeOf(Uniforms));

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
            .out_of_memory, .device_lost, .@"error" => std.debug.panic("get_current_texture status={any}", .{surface_texture.status}),
            else => std.debug.print("get_current_texture unknown status={any}\n", .{surface_texture.status}),
        }
        std.debug.assert(surface_texture.texture != null);
        defer wgpu.textureRelease(surface_texture.texture);

        const frame = wgpu.textureCreateView(surface_texture.texture, null);
        std.debug.assert(frame != null);
        defer wgpu.textureViewRelease(frame);

        const command_encoder = wgpu.deviceCreateCommandEncoder(demo.device, &wgpu.CommandEncoderDescriptor{ .label = .fromSlice("command_encoder") });
        std.debug.assert(command_encoder != null);
        defer wgpu.commandEncoderRelease(command_encoder);

        // 4. Before the render pass, encode a command to copy the data from the staging
        // buffer to the actual vertex buffer. This copy happens entirely on the GPU.
        wgpu.commandEncoderCopyBufferToBuffer(command_encoder, staging_buffer, 0, vertex_buffer, 0, vertex_data_size);

        const render_pass_encoder = wgpu.commandEncoderBeginRenderPass(command_encoder, &wgpu.RenderPassDescriptor{
            .label = .fromSlice("render_pass_encoder"),
            .color_attachment_count = 1,
            .color_attachments = &[_]wgpu.RenderPassColorAttachment{.{ .view = frame, .load_op = .clear, .store_op = .store, .clear_value = .{ .g = 1.0, .a = 1.0 } }},
        });
        std.debug.assert(render_pass_encoder != null);

        wgpu.renderPassEncoderSetPipeline(render_pass_encoder, render_pipeline);
        wgpu.renderPassEncoderSetBindGroup(render_pass_encoder, 0, bind_group, 0, null);
        wgpu.renderPassEncoderSetVertexBuffer(render_pass_encoder, 0, vertex_buffer, 0, vertex_data_size);
        wgpu.renderPassEncoderDraw(render_pass_encoder, vertex_count, 1, 0, 0);
        wgpu.renderPassEncoderEnd(render_pass_encoder);
        wgpu.renderPassEncoderRelease(render_pass_encoder);

        const command_buffer = wgpu.commandEncoderFinish(command_encoder, &wgpu.CommandBufferDescriptor{ .label = .fromSlice("command_buffer") });
        std.debug.assert(command_buffer != null);
        defer wgpu.commandBufferRelease(command_buffer);

        wgpu.queueSubmit(queue, 1, &[_]wgpu.CommandBuffer{command_buffer});
        _ = wgpu.surfacePresent(demo.surface);
    }
}
