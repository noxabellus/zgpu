const std = @import("std");
const wgpu = @import("wgpu.zig");
const glfw = @import("glfw.zig");

test {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(wgpu);
    std.testing.refAllDecls(glfw);
}

/// A descriptor for creating and initializing a buffer in one step.
const frmwrk_buffer_init_descriptor = struct {
    label: []const u8,
    usage: wgpu.BufferUsage, // Changed to new type
    content: ?*anyopaque,
    content_size: usize,
};

/// The wgpu log callback, adapted for the new binding's types.
fn log_callback(level: wgpu.LogLevel, message: wgpu.StringView, userdata: ?*anyopaque) callconv(.c) void {
    _ = userdata;

    // Use the new wgpu.LogLevel enum
    const level_str = switch (level) {
        .@"error" => "error",
        .warn => "warn",
        .info => "info",
        .debug => "debug",
        .trace => "trace",
        else => "unknown_level",
    };

    // message.data is now an optional pointer
    if (message.data) |data| {
        std.debug.print("[wgpu] [{s}] {s}\n", .{ level_str, data[0..message.length] });
    }
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

    // Use the new descriptor and chained struct types
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
    std.debug.print("backend type: {any}\n", .{info.backend_type}); // Printing the enum is more descriptive
    std.debug.print("adapter type: {any}\n", .{info.adapter_type}); // Printing the enum is more descriptive
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
    const window = glfw.createWindow(640, 480, "triangle [wgpu + glfw]", null, null) orelse {
        std.debug.panic("Failed to create GLFW window", .{});
    };
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

    _ = wgpu.instanceRequestAdapter(demo.instance, &wgpu.RequestAdapterOptions{
        .compatible_surface = demo.surface,
    }, .{ .callback = handle_request_adapter, .userdata1 = &demo });

    while (demo.adapter == null) {
        wgpu.instanceProcessEvents(demo.instance);
    }
    defer wgpu.adapterRelease(demo.adapter);
    frmwrk_print_adapter_info(demo.adapter);

    _ = wgpu.adapterRequestDevice(demo.adapter, null, .{ .callback = handle_request_device, .userdata1 = &demo });

    while (demo.device == null) {
        wgpu.instanceProcessEvents(demo.instance);
    }
    defer wgpu.deviceRelease(demo.device);

    const queue = wgpu.deviceGetQueue(demo.device);
    std.debug.assert(queue != null);
    defer wgpu.queueRelease(queue);

    const shader_module = try frmwrk_load_shader_module(demo.device, "shader.wgsl");
    defer wgpu.shaderModuleRelease(shader_module);

    const pipeline_layout = wgpu.deviceCreatePipelineLayout(demo.device, &wgpu.PipelineLayoutDescriptor{
        .label = .fromSlice("pipeline_layout"),
    });
    std.debug.assert(pipeline_layout != null);
    defer wgpu.pipelineLayoutRelease(pipeline_layout);

    var surface_capabilities: wgpu.SurfaceCapabilities = undefined;
    _ = wgpu.surfaceGetCapabilities(demo.surface, demo.adapter, &surface_capabilities);
    defer wgpu.surfaceCapabilitiesFreeMembers(surface_capabilities);

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
                    .write_mask = .all,
                },
            },
        },
        .primitive = .{ .topology = .triangle_list },
        .multisample = .{ .count = 1, .mask = 0xFFFFFFFF },
    });
    std.debug.assert(render_pipeline != null);
    defer wgpu.renderPipelineRelease(render_pipeline);

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
                    .depth_slice = std.math.maxInt(u32), // WGPU_DEPTH_SLICE_UNDEFINED
                    .clear_value = .{ .r = 0.0, .g = 1.0, .b = 0.0, .a = 1.0 },
                },
            },
        });
        std.debug.assert(render_pass_encoder != null);

        wgpu.renderPassEncoderSetPipeline(render_pass_encoder, render_pipeline);
        wgpu.renderPassEncoderDraw(render_pass_encoder, 3, 1, 0, 0);
        wgpu.renderPassEncoderEnd(render_pass_encoder);
        wgpu.renderPassEncoderRelease(render_pass_encoder); // Must release after ending

        const command_buffer = wgpu.commandEncoderFinish(command_encoder, &wgpu.CommandBufferDescriptor{
            .label = .fromSlice("command_buffer"),
        });
        std.debug.assert(command_buffer != null);
        defer wgpu.commandBufferRelease(command_buffer);

        wgpu.queueSubmit(queue, 1, &[_]wgpu.CommandBuffer{command_buffer});
        _ = wgpu.surfacePresent(demo.surface);
    }
}
