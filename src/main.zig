const std = @import("std");
const wgpu = @import("wgpu.zig");
const glfw = @import("glfw.zig");

test {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(wgpu);
    std.testing.refAllDecls(glfw);
}

const C = @cImport({
    @cInclude("stb_image.h");
});

/// A descriptor for creating and initializing a buffer in one step.
const frmwrk_buffer_init_descriptor = struct {
    label: []const u8,
    usage: wgpu.BufferUsage,
    content: ?*anyopaque,
    content_size: usize,
};

/// The wgpu log callback, adapted for the new binding's types.
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

    std.debug.print("[wgpu] [{s}] {s}\n", .{ level_str, text });
}

/// Sets up wgpu logging.
fn frmwrk_setup_logging(level: wgpu.LogLevel) void {
    _ = wgpu.setLogCallback(log_callback, null);
    wgpu.setLogLevel(level);
}

/// Loads a WGSL shader from a file.
fn frmwrk_load_shader_module(device: wgpu.Device, name: []const u8) !wgpu.ShaderModule {
    const buf = std.fs.cwd().readFileAlloc(std.heap.page_allocator, name, 8192) catch {
        return error.FailedToLoadShader;
    };
    defer std.heap.page_allocator.free(buf);

    var wgsl_descriptor = wgpu.ShaderSourceWGSL{
        .chain = .{ .s_type = .shader_source_wgsl },
        .code = .{ .data = buf.ptr, .length = buf.len },
    };

    const shader_module = wgpu.deviceCreateShaderModule(device, &wgpu.ShaderModuleDescriptor{
        .label = .{ .data = name.ptr, .length = name.len },
        .next_in_chain = @ptrCast(&wgsl_descriptor),
    });

    if (shader_module == null) {
        return error.FailedToCreateShaderModule;
    }

    return shader_module;
}

const COPY_BUFFER_ALIGNMENT = 4;

/// A helper to create and initialize a buffer.
fn frmwrk_device_create_buffer_init(device: wgpu.Device, descriptor: *const frmwrk_buffer_init_descriptor) wgpu.Buffer {
    if (descriptor.content_size == 0) {
        return wgpu.deviceCreateBuffer(device, &wgpu.BufferDescriptor{
            .label = .{ .data = descriptor.label.ptr, .length = descriptor.label.len },
            .size = 0,
            .usage = descriptor.usage,
            .mapped_at_creation = false,
        });
    }

    const unpadded_size = descriptor.content_size;
    const align_mask = COPY_BUFFER_ALIGNMENT - 1;
    const padded_size = @max((unpadded_size + align_mask) & ~align_mask, COPY_BUFFER_ALIGNMENT);

    const buffer = wgpu.deviceCreateBuffer(device, &wgpu.BufferDescriptor{
        .label = .{ .data = descriptor.label.ptr, .length = descriptor.label.len },
        .size = padded_size,
        .usage = descriptor.usage,
        .mapped_at_creation = true,
    });

    const mapped_range = wgpu.bufferGetMappedRange(buffer, 0, unpadded_size).?;
    @memcpy(mapped_range, descriptor.content.?[0..unpadded_size]);
    wgpu.bufferUnmap(buffer);

    return buffer;
}

// --- Printing helper functions adapted to new types ---
fn print_registry_report(report: wgpu.RegistryReport, comptime prefix: []const u8) void {
    std.debug.print(prefix ++ "num_allocated={d}\n", .{report.num_allocated});
    std.debug.print(prefix ++ "num_kept_from_user={d}\n", .{report.num_kept_from_user});
    std.debug.print(prefix ++ "num_released_from_user={d}\n", .{report.num_released_from_user});
    std.debug.print(prefix ++ "element_size={d}\n", .{report.element_size});
}

fn print_hub_report(report: wgpu.HubReport, comptime prefix: []const u8) void {
    print_registry_report(report.adapters, prefix ++ "adapter.");
    print_registry_report(report.devices, prefix ++ "devices.");
    print_registry_report(report.queues, prefix ++ "queues.");
    print_registry_report(report.pipeline_layouts, prefix ++ "pipeline_layouts.");
    print_registry_report(report.shader_modules, prefix ++ "shaderModules.");
    print_registry_report(report.bind_group_layouts, prefix ++ "bind_group_layouts.");
    print_registry_report(report.bind_groups, prefix ++ "bind_groups.");
    print_registry_report(report.command_buffers, prefix ++ "command_buffers.");
    print_registry_report(report.render_bundles, prefix ++ "render_bundles.");
    print_registry_report(report.render_pipelines, prefix ++ "render_pipelines.");
    print_registry_report(report.compute_pipelines, prefix ++ "compute_pipelines.");
    print_registry_report(report.pipeline_caches, prefix ++ "pipeline_caches.");
    print_registry_report(report.query_sets, prefix ++ "query_sets.");
    print_registry_report(report.textures, prefix ++ "textures.");
    print_registry_report(report.texture_views, prefix ++ "texture_views.");
    print_registry_report(report.samplers, prefix ++ "samplers.");
}

fn frmwrk_print_global_report(report: wgpu.GlobalReport) void {
    std.debug.print("struct WGPUGlobalReport {{\n", .{});
    print_registry_report(report.surfaces, "\tsurfaces.");
    print_hub_report(report.hub, "\thub.");
    std.debug.print("}}\n", .{});
}

fn frmwrk_print_adapter_info(adapter: wgpu.Adapter) void {
    var info = std.mem.zeroes(wgpu.AdapterInfo);
    _ = wgpu.adapterGetInfo(adapter, &info);
    std.debug.print("description: {s}\n", .{if (info.description.data) |d| d[0..info.description.length] else "(null)"});
    std.debug.print("vendor: {s}\n", .{if (info.vendor.data) |d| d[0..info.vendor.length] else "(null)"});
    std.debug.print("architecture: {s}\n", .{if (info.architecture.data) |d| d[0..info.architecture.length] else "(null)"});
    std.debug.print("device: {s}\n", .{if (info.device.data) |d| d[0..info.device.length] else "(null)"});
    std.debug.print("backend type: {any}\n", .{info.backend_type});
    std.debug.print("adapter type: {any}\n", .{info.adapter_type});
    std.debug.print("vendor_id: {x}\n", .{info.vendor_id});
    std.debug.print("device_id: {x}\n", .{info.device_id});
    wgpu.adapterInfoFreeMembers(info);
}

// --- Application State Struct ---
const Demo = struct {
    instance: wgpu.Instance = null,
    surface: wgpu.Surface = null,
    adapter: wgpu.Adapter = null,
    device: wgpu.Device = null,
    config: wgpu.SurfaceConfiguration = .{},
};

// --- Asynchronous Callbacks ---
fn handle_request_adapter(status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, message: wgpu.StringView, userdata1: ?*anyopaque, userdata2: ?*anyopaque) callconv(.c) void {
    _ = userdata2;
    if (status == .success) {
        const demo: *Demo = @ptrCast(@alignCast(userdata1.?));
        demo.adapter = adapter;
    } else {
        if (message.data) |data| {
            std.debug.print("request_adapter status={any} message={s}\n", .{ status, data[0..message.length] });
        }
    }
}

fn handle_request_device(status: wgpu.RequestDeviceStatus, device: wgpu.Device, message: wgpu.StringView, userdata1: ?*anyopaque, userdata2: ?*anyopaque) callconv(.c) void {
    _ = userdata2;
    if (status == .success) {
        const demo: *Demo = @ptrCast(@alignCast(userdata1.?));
        demo.device = device;
    } else {
        if (message.data) |data| {
            std.debug.print("request_device status={any} message={s}\n", .{ status, data[0..message.length] });
        }
    }
}

// --- GLFW Event Callbacks ---
fn handle_glfw_key(window: *glfw.Window, key: glfw.Key, scancode: i32, action: glfw.KeyState32, mods: glfw.Modifier) callconv(.c) void {
    _ = scancode;
    _ = mods;
    if (key == .r and (action == .press or action == .repeat)) {
        const demo: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(window) orelse return));
        if (demo.instance == null) return;

        var report: wgpu.GlobalReport = undefined;
        wgpu.generateReport(demo.instance, &report);
        frmwrk_print_global_report(report);
    }
}

fn handle_glfw_framebuffer_size(window: *glfw.Window, width: i32, height: i32) callconv(.c) void {
    if (width <= 0 and height <= 0) {
        return;
    }

    const demo: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(window) orelse return));
    if (demo.surface == null) return;

    demo.config.width = @intCast(width);
    demo.config.height = @intCast(height);

    wgpu.surfaceConfigure(demo.surface, &demo.config);
}

pub fn main() !void {
    frmwrk_setup_logging(.warn);

    // this call is required to force glfw to use X11 backend on wayland systems
    glfw.initHint(.{ .platform = .x11 });

    try glfw.init();
    defer glfw.deinit();

    var demo = Demo{};

    demo.instance = wgpu.createInstance(null);
    std.debug.assert(demo.instance != null);
    defer wgpu.instanceRelease(demo.instance);

    glfw.windowHint(.{ .client_api = .none });
    const window = try glfw.createWindow(640, 480, "textured-quad [wgpu + glfw]", null, null);
    defer glfw.destroyWindow(window);

    glfw.setWindowUserPointer(window, &demo);
    _ = glfw.setKeyCallback(window, handle_glfw_key);
    _ = glfw.setFramebufferSizeCallback(window, handle_glfw_framebuffer_size);

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

    // TODO: this doesn't seem like the right way to handle the future but it's from the examples
    _ = wgpu.instanceRequestAdapter(demo.instance, &wgpu.RequestAdapterOptions{
        .compatible_surface = demo.surface,
    }, .{ .callback = handle_request_adapter, .userdata1 = &demo });
    while (demo.adapter == null) {
        wgpu.instanceProcessEvents(demo.instance);
    }
    defer wgpu.adapterRelease(demo.adapter);
    frmwrk_print_adapter_info(demo.adapter);

    // TODO: this doesn't seem like the right way to handle the future but it's from the examples
    _ = wgpu.adapterRequestDevice(demo.adapter, null, .{ .callback = handle_request_device, .userdata1 = &demo });
    while (demo.device == null) {
        wgpu.instanceProcessEvents(demo.instance);
    }
    defer wgpu.deviceRelease(demo.device);

    const queue = wgpu.deviceGetQueue(demo.device);
    std.debug.assert(queue != null);
    defer wgpu.queueRelease(queue);

    // --- Load Texture ---
    var image_width: i32 = 0;
    var image_height: i32 = 0;
    var image_channels: i32 = 0;
    // Force 4 channels (RGBA) for alignment and format compatibility
    const image_data = C.stbi_load("assets/wgpu-logo.png", &image_width, &image_height, &image_channels, 4);
    const image_data_slice =
        if (image_data) |data| data[0 .. @as(u64, @intCast(image_width)) * @as(u64, @intCast(image_height)) * 4] else {
            std.debug.print("Failed to load image.\n", .{});
            return error.ImageLoadFailed;
        };
    defer C.stbi_image_free(image_data);
    std.debug.print("Loaded image with width={d} height={d} channels={d}\n", .{ image_width, image_height, image_channels });

    // --- Create Texture, Sampler, and upload data ---
    const texture_size = wgpu.Extent3D{
        .width = @intCast(image_width),
        .height = @intCast(image_height),
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
        image_data,
        image_data_slice.len,
        &wgpu.TexelCopyBufferLayout{
            .offset = 0,
            .bytes_per_row = @intCast(4 * image_width),
            .rows_per_image = @intCast(image_height),
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
    const shader_module = try frmwrk_load_shader_module(demo.device, "assets/rect.wgsl");
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

    // For premultiplied alpha textures (NOT what we need for stb, just for reference)
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
