const std = @import("std");

const C = @cImport({
    @cInclude("webgpu.h");
    @cInclude("wgpu.h");
    @cDefine("GLFW_EXPOSE_NATIVE_X11", "");
    @cInclude("GLFW/glfw3.h");
    @cInclude("GLFW/glfw3native.h");
});

const frmwrk_buffer_init_descriptor = struct {
    label: []const u8,
    usage: C.WGPUBufferUsage,
    content: ?*anyopaque,
    content_size: usize,
};

fn log_callback(level: C.WGPULogLevel, message: C.WGPUStringView, userdata: ?*anyopaque) callconv(.c) void {
    _ = userdata;

    std.debug.print("[wgpu] [{s}] {s}\n", .{ switch (level) {
        C.WGPULogLevel_Error => "error",
        C.WGPULogLevel_Warn => "warn",
        C.WGPULogLevel_Info => "info",
        C.WGPULogLevel_Debug => "debug",
        C.WGPULogLevel_Trace => "trace",
        else => "unknown_level",
    }, message.data[0..message.length] });
}

fn frmwrk_setup_logging(level: C.WGPULogLevel) void {
    C.wgpuSetLogCallback(log_callback, null);
    C.wgpuSetLogLevel(level);
}

fn frmwrk_load_shader_module(device: C.WGPUDevice, name: []const u8) !C.WGPUShaderModule {
    const buf = std.fs.cwd().readFileAlloc(std.heap.page_allocator, name, 8192) catch {
        return error.FailedToLoadShader;
    };
    defer std.heap.page_allocator.free(buf);

    const shader_module = C.wgpuDeviceCreateShaderModule(device, &C.WGPUShaderModuleDescriptor{
        .label = .{ .data = name.ptr, .length = name.len },
        .nextInChain = @ptrCast(&C.WGPUShaderSourceWGSL{
            .chain = C.WGPUChainedStruct{
                .sType = C.WGPUSType_ShaderSourceWGSL,
            },
            .code = .{ .data = buf.ptr, .length = buf.len },
        }),
    });

    if (shader_module == null) {
        return error.FailedToCreateShaderModule;
    }

    return shader_module;
}

const COPY_BUFFER_ALIGNMENT = 4;

fn frmwrk_device_create_buffer_init(device: C.WGPUDevice, descriptor: *const frmwrk_buffer_init_descriptor) C.WGPUBuffer {
    if (descriptor.content_size == 0) {
        return C.wgpuDeviceCreateBuffer(device, &C.WGPUBufferDescriptor{
            .label = .{ .data = descriptor.label.ptr, .length = descriptor.label.len },
            .size = 0,
            .usage = descriptor.usage,
            .mappedAtCreation = false,
        });
    }

    const unpadded_size = descriptor.content_size;
    const align_mask = COPY_BUFFER_ALIGNMENT - 1;
    const padded_size = @max((unpadded_size + align_mask) & ~align_mask, COPY_BUFFER_ALIGNMENT);
    const buffer = C.wgpuDeviceCreateBuffer(device, &C.WGPUBufferDescriptor{
        .label = .{ .data = descriptor.label.ptr, .length = descriptor.label.len },
        .size = padded_size,
        .usage = descriptor.usage,
        .mappedAtCreation = true,
    });
    const buf = C.wgpuBufferGetMappedRange(buffer, 0, unpadded_size);
    @memcpy(buf, descriptor.content[0..unpadded_size]);
    C.wgpuBufferUnmap(buffer);

    return buffer;
}

fn print_registry_report(report: anytype, comptime prefix: anytype) void {
    std.debug.print(prefix ++ "numAllocated={d}\n", .{report.numAllocated});
    std.debug.print(prefix ++ "numKeptFromUser={d}\n", .{report.numKeptFromUser});
    std.debug.print(prefix ++ "numReleasedFromUser={d}\n", .{report.numReleasedFromUser});
    std.debug.print(prefix ++ "elementSize={d}\n", .{report.elementSize});
}

fn print_hub_report(report: anytype, comptime prefix: anytype) void {
    print_registry_report(report.adapters, prefix ++ "adapter.");
    print_registry_report(report.devices, prefix ++ "devices.");
    print_registry_report(report.queues, prefix ++ "queues.");
    print_registry_report(report.pipelineLayouts, prefix ++ "pipelineLayouts.");
    print_registry_report(report.shaderModules, prefix ++ "shaderModules.");
    print_registry_report(report.bindGroupLayouts, prefix ++ "bindGroupLayouts.");
    print_registry_report(report.bindGroups, prefix ++ "bindGroups.");
    print_registry_report(report.commandBuffers, prefix ++ "commandBuffers.");
    print_registry_report(report.renderBundles, prefix ++ "renderBundles.");
    print_registry_report(report.renderPipelines, prefix ++ "renderPipelines.");
    print_registry_report(report.computePipelines, prefix ++ "computePipelines.");
    print_registry_report(report.pipelineCaches, prefix ++ "pipelineCaches.");
    print_registry_report(report.querySets, prefix ++ "querySets.");
    print_registry_report(report.textures, prefix ++ "textures.");
    print_registry_report(report.textureViews, prefix ++ "textureViews.");
    print_registry_report(report.samplers, prefix ++ "samplers.");
}

fn frmwrk_print_global_report(report: C.WGPUGlobalReport) void {
    std.debug.print("struct WGPUGlobalReport {{\n", .{});
    print_registry_report(report.surfaces, "\tsurfaces.");
    print_hub_report(report.hub, "\thub.");
    std.debug.print("}}\n", .{});
}

fn frmwrk_print_adapter_info(adapter: C.WGPUAdapter) void {
    var info = std.mem.zeroes(C.WGPUAdapterInfo);
    // This function populates the `info` struct.
    _ = C.wgpuAdapterGetInfo(adapter, &info);

    // Safely print each string field, providing a fallback if the pointer is null.
    std.debug.print("description: {s}\n", .{if (info.description.data) |d| d[0..info.description.length] else "(null)"});
    std.debug.print("vendor: {s}\n", .{if (info.vendor.data) |d| d[0..info.vendor.length] else "(null)"});
    std.debug.print("architecture: {s}\n", .{if (info.architecture.data) |d| d[0..info.architecture.length] else "(null)"});
    std.debug.print("device: {s}\n", .{if (info.device.data) |d| d[0..info.device.length] else "(null)"});

    std.debug.print("backend type: {d}\n", .{info.backendType});
    std.debug.print("adapter type: {d}\n", .{info.adapterType});
    std.debug.print("vendorID: {x}\n", .{info.vendorID});
    std.debug.print("deviceID: {x}\n", .{info.deviceID});
    C.wgpuAdapterInfoFreeMembers(info);
}

const Demo = struct {
    instance: C.WGPUInstance,
    surface: C.WGPUSurface,
    adapter: C.WGPUAdapter,
    device: C.WGPUDevice,
    config: C.WGPUSurfaceConfiguration,
};

fn handle_request_adapter(status: C.WGPURequestAdapterStatus, adapter: C.WGPUAdapter, message: C.WGPUStringView, userdata1: ?*anyopaque, userdata2: ?*anyopaque) callconv(.c) void {
    _ = userdata2;
    if (status == C.WGPURequestAdapterStatus_Success) {
        const demo: *Demo = @ptrCast(@alignCast(userdata1.?));
        demo.adapter = adapter;
    } else {
        std.debug.print("request_adapter status={x} message={s}\n", .{ status, message.data[0..message.length] });
    }
}
fn handle_request_device(status: C.WGPURequestDeviceStatus, device: C.WGPUDevice, message: C.WGPUStringView, userdata1: ?*anyopaque, userdata2: ?*anyopaque) callconv(.c) void {
    _ = userdata2;
    if (status == C.WGPURequestDeviceStatus_Success) {
        const demo: *Demo = @ptrCast(@alignCast(userdata1.?));
        demo.device = device;
    } else {
        std.debug.print("request_device status={x} message={s}\n", .{ status, message.data[0..message.length] });
    }
}
fn handle_glfw_key(window: *C.GLFWwindow, key: c_int, scancode: c_int, action: c_int, mods: c_int) callconv(.c) void {
    _ = scancode;
    _ = mods;
    if (key == C.GLFW_KEY_R and (action == C.GLFW_PRESS or action == C.GLFW_REPEAT)) {
        const demo: *Demo = @ptrCast(@alignCast(C.glfwGetWindowUserPointer(window) orelse return));
        if (demo.instance == null)
            return;

        var report: C.WGPUGlobalReport = undefined;
        C.wgpuGenerateReport(demo.instance, &report);
        frmwrk_print_global_report(report);
    }
}

fn handle_glfw_framebuffer_size(window: *C.GLFWwindow, width: c_int, height: c_int) callconv(.c) void {
    if (width <= 0 and height <= 0) {
        return;
    }

    const demo: *Demo = @ptrCast(@alignCast(C.glfwGetWindowUserPointer(window) orelse return));

    demo.config.width = @intCast(width);
    demo.config.height = @intCast(height);

    C.wgpuSurfaceConfigure(demo.surface, &demo.config);
}

pub fn main() !void {
    frmwrk_setup_logging(C.WGPULogLevel_Warn);

    C.glfwInitHint(C.GLFW_PLATFORM, C.GLFW_PLATFORM_X11);

    if (C.glfwInit() == 0)
        return error.FailedToInitGLFW;

    var demo = std.mem.zeroes(Demo);
    demo.instance = C.wgpuCreateInstance(null);
    std.debug.assert(demo.instance != null);

    C.glfwWindowHint(C.GLFW_CLIENT_API, C.GLFW_NO_API);
    const window =
        C.glfwCreateWindow(640, 480, "triangle [wgpu-native + glfw]", null, null);
    std.debug.assert(window != null);

    C.glfwSetWindowUserPointer(window, &demo);
    _ = C.glfwSetKeyCallback(window, @ptrCast(&handle_glfw_key));
    _ = C.glfwSetFramebufferSizeCallback(window, @ptrCast(&handle_glfw_framebuffer_size));
    {
        const x11_display = C.glfwGetX11Display();
        const x11_window = C.glfwGetX11Window(window);

        std.debug.print("Attempting to use X11 backend.\n", .{});
        std.debug.print("X11 display:{?d}, window: {d}\n", .{ x11_display, x11_window });
        std.debug.assert(x11_display != null);
        std.debug.assert(x11_window != 0);

        demo.surface = C.wgpuInstanceCreateSurface(demo.instance, &C.WGPUSurfaceDescriptor{
            .nextInChain = @ptrCast(&C.WGPUSurfaceSourceXlibWindow{
                .chain = C.WGPUChainedStruct{
                    .sType = C.WGPUSType_SurfaceSourceXlibWindow,
                },
                .display = x11_display,
                .window = x11_window,
            }),
        });
    }
    std.debug.assert(demo.surface != null);

    const future = C.wgpuInstanceRequestAdapter(demo.instance, &C.WGPURequestAdapterOptions{
        .compatibleSurface = demo.surface,
    }, C.WGPURequestAdapterCallbackInfo{ .callback = handle_request_adapter, .userdata1 = &demo });
    while (demo.adapter == null) {
        std.debug.print("Waiting for adapter...\n", .{});
        C.wgpuInstanceProcessEvents(demo.instance);
    }
    std.debug.assert(demo.adapter != null);
    _ = future;

    frmwrk_print_adapter_info(demo.adapter);

    const future2 = C.wgpuAdapterRequestDevice(demo.adapter, null, C.WGPURequestDeviceCallbackInfo{ .callback = handle_request_device, .userdata1 = &demo });

    while (demo.device == null) {
        std.debug.print("Waiting for device...\n", .{});
        C.wgpuInstanceProcessEvents(demo.instance);
    }
    std.debug.assert(demo.device != null);
    _ = future2;

    const queue = C.wgpuDeviceGetQueue(demo.device);
    std.debug.assert(queue != null);

    const shader_module =
        try frmwrk_load_shader_module(demo.device, "shader.wgsl");

    const pipeline_layout = C.wgpuDeviceCreatePipelineLayout(demo.device, &C.WGPUPipelineLayoutDescriptor{
        .label = .{ .data = "pipeline_layout", .length = C.WGPU_STRLEN },
    });
    std.debug.assert(pipeline_layout != null);

    var surface_capabilities = std.mem.zeroes(C.WGPUSurfaceCapabilities);
    _ = C.wgpuSurfaceGetCapabilities(demo.surface, demo.adapter, &surface_capabilities);

    const render_pipeline = C.wgpuDeviceCreateRenderPipeline(demo.device, &C.WGPURenderPipelineDescriptor{
        .label = .{ .data = "render_pipeline", .length = C.WGPU_STRLEN },
        .layout = pipeline_layout,
        .vertex = C.WGPUVertexState{
            .module = shader_module,
            .entryPoint = .{ .data = "vs_main", .length = C.WGPU_STRLEN },
        },
        .fragment = &C.WGPUFragmentState{
            .module = shader_module,
            .entryPoint = .{ .data = "fs_main", .length = C.WGPU_STRLEN },
            .targetCount = 1,
            .targets = &[_]C.WGPUColorTargetState{
                C.WGPUColorTargetState{
                    .format = surface_capabilities.formats[0],
                    .writeMask = C.WGPUColorWriteMask_All,
                },
            },
        },
        .primitive = C.WGPUPrimitiveState{
            .topology = C.WGPUPrimitiveTopology_TriangleList,
        },
        .multisample = C.WGPUMultisampleState{
            .count = 1,
            .mask = 0xFFFFFFFF,
        },
    });
    std.debug.assert(render_pipeline != null);

    demo.config = C.WGPUSurfaceConfiguration{
        .device = demo.device,
        .usage = C.WGPUTextureUsage_RenderAttachment,
        .format = surface_capabilities.formats[0],
        .presentMode = C.WGPUPresentMode_Fifo,
        .alphaMode = surface_capabilities.alphaModes[0],
    };

    {
        var width: c_int = 0;
        var height: c_int = 0;
        C.glfwGetWindowSize(window, &width, &height);
        demo.config.width = @intCast(width);
        demo.config.height = @intCast(height);
    }

    C.wgpuSurfaceConfigure(demo.surface, &demo.config);

    main_loop: while (C.glfwWindowShouldClose(window) == 0) {
        C.glfwPollEvents();

        var surface_texture = std.mem.zeroes(C.WGPUSurfaceTexture);
        C.wgpuSurfaceGetCurrentTexture(demo.surface, &surface_texture);
        switch (surface_texture.status) {
            C.WGPUSurfaceGetCurrentTextureStatus_SuccessOptimal,
            C.WGPUSurfaceGetCurrentTextureStatus_SuccessSuboptimal,
            => {},

            C.WGPUSurfaceGetCurrentTextureStatus_Timeout,
            C.WGPUSurfaceGetCurrentTextureStatus_Outdated,
            C.WGPUSurfaceGetCurrentTextureStatus_Lost,
            => {
                // Skip this frame, and re-configure surface.
                if (surface_texture.texture != null) {
                    C.wgpuTextureRelease(surface_texture.texture);
                }
                var width: c_int = 0;
                var height: c_int = 0;
                C.glfwGetWindowSize(window, &width, &height);
                if (width != 0 and height != 0) {
                    demo.config.width = @intCast(width);
                    demo.config.height = @intCast(height);
                    C.wgpuSurfaceConfigure(demo.surface, &demo.config);
                }
                continue :main_loop;
            },
            C.WGPUSurfaceGetCurrentTextureStatus_OutOfMemory,
            C.WGPUSurfaceGetCurrentTextureStatus_DeviceLost,
            C.WGPUSurfaceGetCurrentTextureStatus_Force32,
            => {
                // Fatal error
                std.debug.panic("get_current_texture status={x}", .{surface_texture.status});
            },
            else => std.debug.print("get_current_texture unknown status={x}\n", .{surface_texture.status}),
        }

        std.debug.assert(surface_texture.texture != null);

        const frame =
            C.wgpuTextureCreateView(surface_texture.texture, null);
        std.debug.assert(frame != null);

        const command_encoder = C.wgpuDeviceCreateCommandEncoder(demo.device, &C.WGPUCommandEncoderDescriptor{
            .label = .{ .data = "command_encoder", .length = C.WGPU_STRLEN },
        });
        std.debug.assert(command_encoder != null);

        const render_pass_encoder =
            C.wgpuCommandEncoderBeginRenderPass(command_encoder, &C.WGPURenderPassDescriptor{
                .label = .{ .data = "render_pass_encoder", .length = C.WGPU_STRLEN },
                .colorAttachmentCount = 1,
                .colorAttachments = &[_]C.WGPURenderPassColorAttachment{
                    C.WGPURenderPassColorAttachment{
                        .view = frame,
                        .loadOp = C.WGPULoadOp_Clear,
                        .storeOp = C.WGPUStoreOp_Store,
                        .depthSlice = C.WGPU_DEPTH_SLICE_UNDEFINED,
                        .clearValue = C.WGPUColor{
                            .r = 0.0,
                            .g = 1.0,
                            .b = 0.0,
                            .a = 1.0,
                        },
                    },
                },
            });
        std.debug.assert(render_pass_encoder != null);

        C.wgpuRenderPassEncoderSetPipeline(render_pass_encoder, render_pipeline);
        C.wgpuRenderPassEncoderDraw(render_pass_encoder, 3, 1, 0, 0);
        C.wgpuRenderPassEncoderEnd(render_pass_encoder);
        C.wgpuRenderPassEncoderRelease(render_pass_encoder);

        const command_buffer = C.wgpuCommandEncoderFinish(command_encoder, &C.WGPUCommandBufferDescriptor{
            .label = .{ .data = "command_buffer", .length = C.WGPU_STRLEN },
        });
        std.debug.assert(command_buffer != null);

        C.wgpuQueueSubmit(queue, 1, &[_]C.WGPUCommandBuffer{command_buffer});
        _ = C.wgpuSurfacePresent(demo.surface);

        C.wgpuCommandBufferRelease(command_buffer);
        C.wgpuCommandEncoderRelease(command_encoder);
        C.wgpuTextureViewRelease(frame);
        C.wgpuTextureRelease(surface_texture.texture);
    }

    C.wgpuRenderPipelineRelease(render_pipeline);
    C.wgpuPipelineLayoutRelease(pipeline_layout);
    C.wgpuShaderModuleRelease(shader_module);
    C.wgpuSurfaceCapabilitiesFreeMembers(surface_capabilities);
    C.wgpuQueueRelease(queue);
    C.wgpuDeviceRelease(demo.device);
    C.wgpuAdapterRelease(demo.adapter);
    C.wgpuSurfaceRelease(demo.surface);
    C.glfwDestroyWindow(window);
    C.wgpuInstanceRelease(demo.instance);
    C.glfwTerminate();
}
