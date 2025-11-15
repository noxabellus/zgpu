const Application = @This();

const std = @import("std");
const builtin = @import("builtin");
const linalg = @import("linalg.zig");
const glfw = @import("glfw");
const wgpu = @import("wgpu");
const stbi = @import("stbi");

const log = std.log.scoped(.app);

pub const DEPTH_FORMAT = wgpu.TextureFormat.depth32_float;
pub const WGPU_LOG_LEVEL = wgpu.LogLevel.warn;

window: *glfw.Window,
gpu: struct {
    instance: wgpu.Instance,
    surface: wgpu.Surface,
    surface_format: wgpu.TextureFormat,
    frame_surface_texture: wgpu.SurfaceTexture,
    frame_surface_view: wgpu.TextureView,
    adapter: wgpu.Adapter,
    device: wgpu.Device,
    depth_texture: wgpu.Texture,
    depth_view: wgpu.TextureView,
    config: wgpu.SurfaceConfiguration,
    capabilities: wgpu.SurfaceCapabilities,
    queue: wgpu.Queue,
},

pub fn recreateDepthTexture(app: *Application) void {
    if (app.gpu.depth_view) |v| wgpu.textureViewRelease(v);
    if (app.gpu.depth_texture) |t| wgpu.textureRelease(t);

    app.gpu.depth_texture = wgpu.deviceCreateTexture(app.gpu.device, &wgpu.TextureDescriptor{
        .label = .fromSlice("depth_texture"),
        .size = .{
            .width = app.gpu.config.width,
            .height = app.gpu.config.height,
            .depth_or_array_layers = 1,
        },
        .mip_level_count = 1,
        .sample_count = 1,
        .dimension = .@"2d",
        .format = DEPTH_FORMAT,
        .usage = wgpu.TextureUsage.renderAttachmentUsage,
        .view_format_count = 1,
        .view_formats = &.{DEPTH_FORMAT},
    });

    app.gpu.depth_view = wgpu.textureCreateView(app.gpu.depth_texture, null);
}

pub fn getFramebufferSize(self: *Application) linalg.vec2i {
    var width: i32 = 0;
    var height: i32 = 0;
    glfw.getFramebufferSize(self.window, &width, &height);
    return .{ width, height };
}

pub fn beginFrame(self: *Application) ?wgpu.TextureView {
    wgpu.surfaceGetCurrentTexture(self.gpu.surface, &self.gpu.frame_surface_texture);
    switch (self.gpu.frame_surface_texture.status) {
        .success_optimal, .success_suboptimal => {},
        .timeout, .outdated, .lost => {
            if (self.gpu.frame_surface_texture.texture != null) wgpu.textureRelease(self.gpu.frame_surface_texture.texture);
            var width: i32 = 0;
            var height: i32 = 0;
            glfw.getWindowSize(self.window, &width, &height);
            if (width != 0 and height != 0) {
                self.gpu.config.width = @intCast(width);
                self.gpu.config.height = @intCast(height);
                wgpu.surfaceConfigure(self.gpu.surface, &self.gpu.config);
                self.recreateDepthTexture();
            }
            return null;
        },
        else => std.debug.panic("get_current_texture status={any}", .{self.gpu.frame_surface_texture.status}),
    }
    std.debug.assert(self.gpu.frame_surface_texture.texture != null);

    self.gpu.frame_surface_view = wgpu.textureCreateView(self.gpu.frame_surface_texture.texture, null);
    std.debug.assert(self.gpu.frame_surface_view != null);

    return self.gpu.frame_surface_view;
}

pub fn endFrame(self: *Application) void {
    _ = wgpu.surfacePresent(self.gpu.surface);

    wgpu.textureViewRelease(self.gpu.frame_surface_view);
    self.gpu.frame_surface_view = null;

    wgpu.textureRelease(self.gpu.frame_surface_texture.texture);
    @memset(std.mem.asBytes(&self.gpu.frame_surface_texture), 0);
}

pub fn init(gpa: std.mem.Allocator, name: [*:0]const u8) !*Application {
    log.info("application initializing...", .{});

    const backend = switch (builtin.os.tag) {
        .windows => if (glfw.isRunningInWine()) wgpu.InstanceBackend.vulkanBackend else wgpu.InstanceBackend.dx12Backend,
        .linux => wgpu.InstanceBackend.vulkanBackend,
        else => @compileError("unsupported os " ++ @tagName(builtin.os.tag)),
    };

    log.info("selected backends: {}", .{backend});

    stbi.init(gpa);
    errdefer stbi.deinit();

    log.info("stbi started", .{});

    if (comptime builtin.os.tag != .windows) {
        log.info("initializing glfw for x11 ...", .{});
        glfw.initHint(.{ .platform = .x11 });
    } else {
        log.info("initializing glfw for win32 ...", .{});
        glfw.initHint(.{ .platform = .win32 });
    }

    try glfw.init();
    errdefer glfw.deinit();

    log.info("glfw started", .{});

    var self = try gpa.create(Application);
    @memset(std.mem.asBytes(self), 0);

    log.info("created application structure allocation", .{});

    wgpu.setLogCallback(&struct {
        pub fn wgpu_logger(level: wgpu.LogLevel, message: wgpu.StringView, _: ?*anyopaque) callconv(.c) void {
            const msg = message.toSlice();
            const prefix = switch (level) {
                .@"error" => "error",
                .warn => "warning",
                .info => "info",
                .debug => "debug",
                .trace => "trace",
                else => "unknown",
            };
            std.debug.print("{s}(wgpu): {s}\n", .{ prefix, msg });
        }
    }.wgpu_logger, null);

    wgpu.setLogLevel(WGPU_LOG_LEVEL);

    log.info("setup wgpu logger", .{});

    self.gpu.instance = wgpu.createInstance(&wgpu.InstanceDescriptor{
        .next_in_chain = @ptrCast(&wgpu.InstanceExtras{
            .chain = .{ .s_type = .instance_extras },
            .backends = backend,
        }),
    });
    if (self.gpu.instance == null) return error.FailedToCreateWgpuInstance;
    log.info("created wgpu instance", .{});

    errdefer wgpu.instanceRelease(self.gpu.instance);
    glfw.windowHint(.{ .client_api = .none });
    glfw.windowHint(.{ .resizable = true });

    self.window = try glfw.createWindow(800, 600, name, null, null);
    errdefer glfw.destroyWindow(self.window);

    glfw.setWindowUserPointer(self.window, self);

    log.info("created glfw window", .{});

    _ = glfw.setFramebufferSizeCallback(self.window, &struct {
        fn handle_glfw_framebuffer_size(w: *glfw.Window, width: i32, height: i32) callconv(.c) void {
            if (width <= 0 and height <= 0) {
                log.warn("impossible window size reported by glfw {}x{}", .{ width, height });
                return;
            }

            const app: *Application = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse {
                log.warn("failed to get application from glfw window", .{});
                return;
            }));

            if (app.gpu.surface == null) {
                log.warn("failed to get application surface for resize", .{});
                return;
            }

            app.gpu.config.width = @intCast(width);
            app.gpu.config.height = @intCast(height);
            wgpu.surfaceConfigure(app.gpu.surface, &app.gpu.config);
            recreateDepthTexture(app);
        }
    }.handle_glfw_framebuffer_size);

    if (comptime builtin.os.tag != .windows) {
        log.info("initializing wgpu surface for x11", .{});
        const x11_display = glfw.getX11Display();
        const x11_window = glfw.getX11Window(self.window);
        self.gpu.surface = wgpu.instanceCreateSurface(self.gpu.instance, &wgpu.SurfaceDescriptor{
            .next_in_chain = @ptrCast(&wgpu.SurfaceSourceXlibWindow{
                .chain = .{ .s_type = .surface_source_xlib_window },
                .display = x11_display,
                .window = x11_window,
            }),
        });
    } else {
        log.info("initializing wgpu surface for win32", .{});
        const win32_hwnd = glfw.getWin32Window(self.window);
        const win32_hinstance = glfw.getWin32ModuleHandle();
        self.gpu.surface = wgpu.instanceCreateSurface(self.gpu.instance, &wgpu.SurfaceDescriptor{
            .next_in_chain = @ptrCast(&wgpu.SurfaceSourceWindowsHWND{
                .chain = .{ .s_type = .surface_source_windows_hwnd },
                .hwnd = win32_hwnd,
                .hinstance = win32_hinstance,
            }),
        });
    }

    if (self.gpu.surface == null) return error.FailedToCreateWgpuSurface;

    errdefer wgpu.surfaceRelease(self.gpu.surface);

    log.info("wgpu surface initialized", .{});

    _ = wgpu.instanceRequestAdapter(self.gpu.instance, &wgpu.RequestAdapterOptions{ .compatible_surface = self.gpu.surface }, .{ .callback = &struct {
        fn handle_request_adapter(status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
            _ = ud2;
            if (status == .success) {
                const app: *Application = @ptrCast(@alignCast(ud1.?));
                app.gpu.adapter = adapter;
            } else {
                log.err("request_adapter failed: {s}", .{msg.toSlice()});
            }
        }
    }.handle_request_adapter, .userdata1 = self });

    while (self.gpu.adapter == null) wgpu.instanceProcessEvents(self.gpu.instance);

    errdefer wgpu.adapterRelease(self.gpu.adapter);

    log.info("got wgpu adapter", .{});

    _ = wgpu.adapterRequestDevice(self.gpu.adapter, null, .{ .callback = &struct {
        fn handle_request_device(status: wgpu.RequestDeviceStatus, device: wgpu.Device, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
            _ = ud2;
            if (status == .success) {
                const app: *Application = @ptrCast(@alignCast(ud1.?));
                app.gpu.device = device;
            } else {
                log.err("request_device failed: {s}", .{msg.toSlice()});
            }
        }
    }.handle_request_device, .userdata1 = self });

    while (self.gpu.device == null) wgpu.instanceProcessEvents(self.gpu.instance);

    errdefer wgpu.deviceRelease(self.gpu.device);

    log.info("got wgpu device", .{});

    self.gpu.queue = wgpu.deviceGetQueue(self.gpu.device);
    errdefer wgpu.queueRelease(self.gpu.queue);

    log.info("got wgpu queue", .{});

    const cap_res = wgpu.surfaceGetCapabilities(self.gpu.surface, self.gpu.adapter, &self.gpu.capabilities);
    if (cap_res != .success) return error.FailedToGetWgpuSurfaceCapabilities;
    if (self.gpu.capabilities.format_count == 0) {
        return error.NoSurfaceFormatAvailable;
    }

    errdefer wgpu.surfaceCapabilitiesFreeMembers(self.gpu.capabilities);

    log.info("got wgpu surface capabilities", .{});

    self.gpu.surface_format =
        for (0..self.gpu.capabilities.format_count) |f| {
            const fmt = self.gpu.capabilities.formats.?[f];
            if (fmt.isSrgb()) break fmt;
        } else {
            return error.NoWgpuSurfaceSrgbFormatAvailable;
        };

    log.info("selected surface format {s}", .{@tagName(self.gpu.surface_format)});

    self.gpu.config = .{
        .device = self.gpu.device,
        .usage = .renderAttachmentUsage,
        .format = self.gpu.surface_format,
        .present_mode = .fifo,
        .alpha_mode = self.gpu.capabilities.alpha_modes.?[0],
    };

    {
        var width: i32 = 0;
        var height: i32 = 0;
        glfw.getWindowSize(self.window, &width, &height);
        log.info("got window size", .{});
        self.gpu.config.width = @intCast(width);
        self.gpu.config.height = @intCast(height);
    }

    wgpu.surfaceConfigure(self.gpu.surface, &self.gpu.config);

    log.info("configured wgpu surface", .{});

    self.recreateDepthTexture();

    log.info("application initialized", .{});

    return self;
}

pub fn deinit(self: *Application) void {
    log.info("application shutting down ...", .{});

    wgpu.surfaceCapabilitiesFreeMembers(self.gpu.capabilities);
    wgpu.queueRelease(self.gpu.queue);
    if (self.gpu.depth_view) |v| wgpu.textureViewRelease(v);
    if (self.gpu.depth_texture) |t| wgpu.textureRelease(t);

    wgpu.deviceRelease(self.gpu.device);
    wgpu.adapterRelease(self.gpu.adapter);
    wgpu.surfaceRelease(self.gpu.surface);
    wgpu.instanceRelease(self.gpu.instance);
    glfw.destroyWindow(self.window);

    glfw.deinit();
    stbi.deinit();

    self.* = undefined;

    log.info("application shutdown completed successfully", .{});
}
