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

            inline for (&.{
                "Suboptimal present of frame", // Suboptimal present is *desired* when resizing the window; alternative is ugly flickering
            }) |ignore_pattern| {
                if (std.mem.indexOf(u8, text, ignore_pattern) != null) {
                    return;
                }
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

    wgpu.setLogLevel(lvl: switch (std_options.log_level) {
        .debug => {
            std.debug.print("log level is debug\n", .{});
            break :lvl .trace;
        },
        .info => {
            std.debug.print("log level is info\n", .{});
            break :lvl .info;
        },
        .warn => {
            std.debug.print("log level is warn\n", .{});
            break :lvl .warn;
        },
        .err => {
            std.debug.print("log level is error\n", .{});
            break :lvl .@"error";
        },
    });

    if (comptime builtin.os.tag != .windows) {
        // this call is required to force glfw to use X11 backend on wayland systems
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

    glfw.windowHint(.{ .client_api = .none });
    const window = try glfw.createWindow(640, 480, "textured-quad [zig / wgpu + glfw + stb_image]", null, null);
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
            if (width <= 0 and height <= 0) {
                return;
            }

            const st: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse return));
            if (st.surface == null) return;

            st.config.width = @intCast(width);
            st.config.height = @intCast(height);

            wgpu.surfaceConfigure(st.surface, &st.config);
        }
    }.handle_glfw_framebuffer_size);

    {
        const x11_display = glfw.getX11Display();
        const x11_window = glfw.getX11Window(window);
        std.debug.assert(x11_display != null and x11_window != 0);

        var xlib_source = wgpu.SurfaceSourceXlibWindow{
            .chain = .{ .s_type = .surface_source_xlib_window },
            .display = x11_display,
            .window = x11_window,
        };
        demo.surface = wgpu.instanceCreateSurface(demo.instance, &wgpu.SurfaceDescriptor{
            .next_in_chain = @ptrCast(&xlib_source),
        });
    }
    std.debug.assert(demo.surface != null);
    defer wgpu.surfaceRelease(demo.surface);

    _ = wgpu.instanceRequestAdapter(demo.instance, &wgpu.RequestAdapterOptions{
        .compatible_surface = demo.surface,
    }, .{ .callback = &struct {
        fn handle_request_adapter(status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, message: wgpu.StringView, userdata1: ?*anyopaque, userdata2: ?*anyopaque) callconv(.c) void {
            _ = userdata2;
            if (status == .success) {
                const state: *Demo = @ptrCast(@alignCast(userdata1.?));
                state.adapter = adapter;
            } else {
                if (message.data) |data| {
                    std.debug.print("request_adapter status={any} message={s}\n", .{ status, data[0..message.length] });
                }
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
                if (message.data) |data| {
                    std.debug.print("request_device status={any} message={s}\n", .{ status, data[0..message.length] });
                }
            }
        }
    }.handle_request_device, .userdata1 = &demo });
    while (demo.device == null) wgpu.instanceProcessEvents(demo.instance);
    defer wgpu.deviceRelease(demo.device);

    const queue = wgpu.deviceGetQueue(demo.device);
    std.debug.assert(queue != null);
    defer wgpu.queueRelease(queue);

    // --- Load Texture ---
    var image = try stbi.Image8.fromPath("assets/wgpu-logo.png", .rgba);
    defer image.deinit();
    std.debug.print("Loaded image: {any}\n", .{image.info});

    // --- Create Texture, Sampler, and upload data ---
    const texture_size = wgpu.Extent3D{
        .width = image.info.width,
        .height = image.info.height,
        .depth_or_array_layers = 1,
    };
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

    wgpu.queueWriteTexture(
        queue,
        &wgpu.TexelCopyTextureInfo{ .texture = texture, .mip_level = 0, .origin = .{} },
        image.buffer.ptr,
        image.buffer.len,
        &wgpu.TexelCopyBufferLayout{
            .offset = 0,
            .bytes_per_row = 4 * image.info.width,
            .rows_per_image = image.info.height,
        },
        &texture_size,
    );

    const texture_view = wgpu.textureCreateView(texture, null);
    std.debug.assert(texture_view != null);
    defer wgpu.textureViewRelease(texture_view);

    const sampler = wgpu.deviceCreateSampler(demo.device, &wgpu.SamplerDescriptor{
        .label = .fromSlice("sampler"),
        .address_mode_u = .clamp_to_edge,
        .address_mode_v = .clamp_to_edge,
        .address_mode_w = .clamp_to_edge,
        .mag_filter = .linear,
        .min_filter = .linear,
        .mipmap_filter = .nearest,
    });
    std.debug.assert(sampler != null);
    defer wgpu.samplerRelease(sampler);

    // --- Create Bind Group Layout and Bind Group ---
    const bind_group_layout = wgpu.deviceCreateBindGroupLayout(demo.device, &wgpu.BindGroupLayoutDescriptor{
        .label = .fromSlice("texture_bind_group_layout"),
        .entry_count = 2,
        .entries = &[_]wgpu.BindGroupLayoutEntry{
            .{
                .binding = 0,
                .visibility = .fragmentStage,
                .sampler = .{ .type = .filtering },
            },
            .{
                .binding = 1,
                .visibility = .fragmentStage,
                .texture = .{ .sample_type = .float, .view_dimension = .@"2d" },
            },
        },
    });
    std.debug.assert(bind_group_layout != null);
    defer wgpu.bindGroupLayoutRelease(bind_group_layout);

    const bind_group = wgpu.deviceCreateBindGroup(demo.device, &wgpu.BindGroupDescriptor{
        .label = .fromSlice("texture_bind_group"),
        .layout = bind_group_layout,
        .entry_count = 2,
        .entries = &[_]wgpu.BindGroupEntry{
            .{ .binding = 0, .sampler = sampler },
            .{ .binding = 1, .texture_view = texture_view },
        },
    });
    std.debug.assert(bind_group != null);
    defer wgpu.bindGroupRelease(bind_group);

    // --- Create Pipeline ---
    const shader_module = try wgpu.loadShader(demo.device, "assets/rect.wgsl");
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

    const blend_state = wgpu.BlendState{
        .color = .{
            // This corresponds to the formula:
            // (SourceColor * SourceAlpha) + (DestinationColor * (1 - SourceAlpha))
            .operation = .add,
            .src_factor = .src_alpha,
            .dst_factor = .one_minus_src_alpha,
        },
        .alpha = .{
            // We want the final alpha to be the source's alpha.
            // Formula: (SourceAlpha * 1) + (DestinationAlpha * 0)
            .operation = .add,
            .src_factor = .one,
            .dst_factor = .zero,
        },
    };

    // For premultiplied alpha textures (note: stbi will automatically un-premultiply if we like; here for reference)
    // const premultiplied_blend_state = wgpu.BlendState{
    //     .color = .{
    //         // Formula: SourceColor + (DestinationColor * (1 - SourceAlpha))
    //         .operation = .add,
    //         .src_factor = .one,
    //         .dst_factor = .one_minus_src_alpha,
    //     },
    //     // ... alpha component is often the same
    // };

    const render_pipeline = wgpu.deviceCreateRenderPipeline(demo.device, &wgpu.RenderPipelineDescriptor{
        .label = .fromSlice("render_pipeline"),
        .layout = pipeline_layout,
        .vertex = .{
            .module = shader_module,
            .entry_point = .fromSlice("vs_main"),
        },
        .fragment = &wgpu.FragmentState{
            .module = shader_module,
            .entry_point = .fromSlice("fs_main"),
            .target_count = 1,
            .targets = &[_]wgpu.ColorTargetState{
                .{
                    .format = surface_capabilities.formats.?[0],
                    .blend = &blend_state,
                    .write_mask = .all,
                },
            },
        },
        .primitive = .{ .topology = .triangle_list },
        .multisample = .{ .count = 1, .mask = 0xFFFFFFFF },
    });
    std.debug.assert(render_pipeline != null);
    defer wgpu.renderPipelineRelease(render_pipeline);

    // --- Configure Surface ---
    demo.config = .{
        .device = demo.device,
        .usage = .renderAttachmentUsage,
        .format = surface_capabilities.formats.?[0],
        .present_mode = .fifo,
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

    // --- Main Loop ---
    main_loop: while (!glfw.windowShouldClose(window)) {
        glfw.pollEvents();

        var surface_texture: wgpu.SurfaceTexture = undefined;
        wgpu.surfaceGetCurrentTexture(demo.surface, &surface_texture);

        switch (surface_texture.status) {
            .success_optimal, .success_suboptimal => {},
            .timeout, .outdated, .lost => {
                if (surface_texture.texture != null) {
                    wgpu.textureRelease(surface_texture.texture);
                }
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
            .out_of_memory, .device_lost, .@"error" => {
                std.debug.panic("get_current_texture status={any}", .{surface_texture.status});
            },
            else => std.debug.print("get_current_texture unknown status={any}\n", .{surface_texture.status}),
        }
        std.debug.assert(surface_texture.texture != null);
        defer wgpu.textureRelease(surface_texture.texture);

        const frame = wgpu.textureCreateView(surface_texture.texture, null);
        std.debug.assert(frame != null);
        defer wgpu.textureViewRelease(frame);

        const command_encoder = wgpu.deviceCreateCommandEncoder(demo.device, &wgpu.CommandEncoderDescriptor{
            .label = .fromSlice("command_encoder"),
        });
        std.debug.assert(command_encoder != null);
        defer wgpu.commandEncoderRelease(command_encoder);

        const render_pass_encoder = wgpu.commandEncoderBeginRenderPass(command_encoder, &wgpu.RenderPassDescriptor{
            .label = .fromSlice("render_pass_encoder"),
            .color_attachment_count = 1,
            .color_attachments = &[_]wgpu.RenderPassColorAttachment{
                .{
                    .view = frame,
                    .load_op = .clear,
                    .store_op = .store,
                    .depth_slice = wgpu.undefined_depth_slice,
                    .clear_value = .{ .r = 0.0, .g = 1.0, .b = 0.0, .a = 1.0 },
                },
            },
        });
        std.debug.assert(render_pass_encoder != null);

        wgpu.renderPassEncoderSetPipeline(render_pass_encoder, render_pipeline);
        wgpu.renderPassEncoderSetBindGroup(render_pass_encoder, 0, bind_group, 0, null);
        wgpu.renderPassEncoderDraw(render_pass_encoder, 6, 1, 0, 0); // Draw 6 vertices for the quad
        wgpu.renderPassEncoderEnd(render_pass_encoder);
        wgpu.renderPassEncoderRelease(render_pass_encoder);

        const command_buffer = wgpu.commandEncoderFinish(command_encoder, &wgpu.CommandBufferDescriptor{
            .label = .fromSlice("command_buffer"),
        });
        std.debug.assert(command_buffer != null);
        defer wgpu.commandBufferRelease(command_buffer);

        wgpu.queueSubmit(queue, 1, &[_]wgpu.CommandBuffer{command_buffer});
        _ = wgpu.surfacePresent(demo.surface);
    }
}
