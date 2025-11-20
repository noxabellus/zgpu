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
user_data: *anyopaque,
gpu: Gpu,

pub const Gpu = struct {
    instance: wgpu.Instance,
    surface: wgpu.Surface,
    surface_format: wgpu.TextureFormat,
    frame_surface_texture: wgpu.SurfaceTexture,
    adapter: wgpu.Adapter,
    device: wgpu.Device,
    depth_texture: wgpu.Texture,
    depth_view: wgpu.TextureView,
    config: wgpu.SurfaceConfiguration,
    capabilities: wgpu.SurfaceCapabilities,
    queue: wgpu.Queue,

    pub fn init(self: *Gpu) !void {
        const window = self.getApp().window;

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

        const backend = switch (builtin.os.tag) {
            .windows => if (glfw.isRunningInWine()) wgpu.InstanceBackend.vulkanBackend else wgpu.InstanceBackend.dx12Backend,
            .linux => wgpu.InstanceBackend.vulkanBackend,
            else => @compileError("unsupported os " ++ @tagName(builtin.os.tag)),
        };

        log.info("selected backends: {}", .{backend});

        self.instance = wgpu.createInstance(&wgpu.InstanceDescriptor{
            .next_in_chain = @ptrCast(&wgpu.InstanceExtras{
                .chain = .{ .s_type = .instance_extras },
                .backends = backend,
            }),
        });
        if (self.instance == null) return error.FailedToCreateWgpuInstance;
        log.info("created wgpu instance", .{});

        errdefer wgpu.instanceRelease(self.instance);

        _ = glfw.setFramebufferSizeCallback(window, &struct {
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
                app.gpu.recreateDepthTexture();
            }
        }.handle_glfw_framebuffer_size);

        if (comptime builtin.os.tag != .windows) {
            log.info("initializing wgpu surface for x11", .{});
            const x11_display = glfw.getX11Display();
            const x11_window = glfw.getX11Window(window);
            self.surface = wgpu.instanceCreateSurface(self.instance, &wgpu.SurfaceDescriptor{
                .next_in_chain = @ptrCast(&wgpu.SurfaceSourceXlibWindow{
                    .chain = .{ .s_type = .surface_source_xlib_window },
                    .display = x11_display,
                    .window = x11_window,
                }),
            });
        } else {
            log.info("initializing wgpu surface for win32", .{});
            const win32_hwnd = glfw.getWin32Window(window);
            const win32_hinstance = glfw.getWin32ModuleHandle();
            self.surface = wgpu.instanceCreateSurface(self.instance, &wgpu.SurfaceDescriptor{
                .next_in_chain = @ptrCast(&wgpu.SurfaceSourceWindowsHWND{
                    .chain = .{ .s_type = .surface_source_windows_hwnd },
                    .hwnd = win32_hwnd,
                    .hinstance = win32_hinstance,
                }),
            });
        }

        if (self.surface == null) return error.FailedToCreateWgpuSurface;

        errdefer wgpu.surfaceRelease(self.surface);

        log.info("wgpu surface initialized", .{});

        _ = wgpu.instanceRequestAdapter(self.instance, &wgpu.RequestAdapterOptions{ .compatible_surface = self.surface }, .{ .callback = &struct {
            fn handle_request_adapter(status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
                _ = ud2;
                if (status == .success) {
                    const gpu: *Gpu = @ptrCast(@alignCast(ud1.?));
                    gpu.adapter = adapter;
                } else {
                    log.err("request_adapter failed: {s}", .{msg.toSlice()});
                }
            }
        }.handle_request_adapter, .userdata1 = self });

        while (self.adapter == null) wgpu.instanceProcessEvents(self.instance);

        errdefer wgpu.adapterRelease(self.adapter);

        log.info("got wgpu adapter", .{});

        _ = wgpu.adapterRequestDevice(self.adapter, null, .{ .callback = &struct {
            fn handle_request_device(status: wgpu.RequestDeviceStatus, device: wgpu.Device, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
                _ = ud2;
                if (status == .success) {
                    const gpu: *Gpu = @ptrCast(@alignCast(ud1.?));
                    gpu.device = device;
                } else {
                    log.err("request_device failed: {s}", .{msg.toSlice()});
                }
            }
        }.handle_request_device, .userdata1 = self });

        while (self.device == null) wgpu.instanceProcessEvents(self.instance);

        errdefer wgpu.deviceRelease(self.device);

        log.info("got wgpu device", .{});

        self.queue = wgpu.deviceGetQueue(self.device);
        errdefer wgpu.queueRelease(self.queue);

        log.info("got wgpu queue", .{});

        const cap_res = wgpu.surfaceGetCapabilities(self.surface, self.adapter, &self.capabilities);
        if (cap_res != .success) return error.FailedToGetWgpuSurfaceCapabilities;
        if (self.capabilities.format_count == 0) {
            return error.NoSurfaceFormatAvailable;
        }

        errdefer wgpu.surfaceCapabilitiesFreeMembers(self.capabilities);

        log.info("got wgpu surface capabilities", .{});

        self.surface_format =
            for (0..self.capabilities.format_count) |f| {
                const fmt = self.capabilities.formats.?[f];
                if (fmt.isSrgb()) break fmt;
            } else {
                return error.NoWgpuSurfaceSrgbFormatAvailable;
            };

        log.info("selected surface format {s}", .{@tagName(self.surface_format)});

        self.config = .{
            .device = self.device,
            .usage = .renderAttachmentUsage,
            .format = self.surface_format,
            .present_mode = .fifo,
            .alpha_mode = self.capabilities.alpha_modes.?[0],
        };

        {
            var width: i32 = 0;
            var height: i32 = 0;
            glfw.getWindowSize(window, &width, &height);
            log.info("got window size", .{});
            self.config.width = @intCast(width);
            self.config.height = @intCast(height);
        }

        wgpu.surfaceConfigure(self.surface, &self.config);

        log.info("configured wgpu surface", .{});

        self.recreateDepthTexture();
    }

    pub fn deinit(self: *Gpu) void {
        wgpu.surfaceCapabilitiesFreeMembers(self.capabilities);
        wgpu.queueRelease(self.queue);
        if (self.depth_view) |v| wgpu.textureViewRelease(v);
        if (self.depth_texture) |t| wgpu.textureRelease(t);

        wgpu.deviceRelease(self.device);
        wgpu.adapterRelease(self.adapter);
        wgpu.surfaceRelease(self.surface);
        wgpu.instanceRelease(self.instance);
    }

    pub fn getApp(self: *Gpu) *Application {
        return @fieldParentPtr("gpu", self);
    }

    fn recreateDepthTexture(gpu: *Gpu) void {
        if (gpu.depth_view) |v| wgpu.textureViewRelease(v);
        if (gpu.depth_texture) |t| wgpu.textureRelease(t);

        gpu.depth_texture = wgpu.deviceCreateTexture(gpu.device, &wgpu.TextureDescriptor{
            .label = .fromSlice("depth_texture"),
            .size = .{
                .width = gpu.config.width,
                .height = gpu.config.height,
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

        gpu.depth_view = wgpu.textureCreateView(gpu.depth_texture, null);
    }

    pub fn loadShader(self: *Gpu, path: []const u8) !wgpu.ShaderModule {
        return wgpu.loadShader(self.device, path);
    }

    pub fn loadShaderText(self: *Gpu, name: []const u8, source: []const u8) !wgpu.ShaderModule {
        return wgpu.loadShaderText(self.device, name, source);
    }

    pub fn createPipelineLayout(self: *Gpu, descriptor: *const wgpu.PipelineLayoutDescriptor) wgpu.PipelineLayout {
        return wgpu.deviceCreatePipelineLayout(self.device, descriptor);
    }

    pub fn createPipeline(self: *Gpu, descriptor: *const wgpu.RenderPipelineDescriptor) wgpu.RenderPipeline {
        return wgpu.deviceCreateRenderPipeline(self.device, descriptor);
    }

    pub fn createBuffer(self: *Gpu, descriptor: *const wgpu.BufferDescriptor) wgpu.Buffer {
        return wgpu.deviceCreateBuffer(self.device, descriptor);
    }

    pub fn createTexture(self: *Gpu, descriptor: *const wgpu.TextureDescriptor) wgpu.Texture {
        return wgpu.deviceCreateTexture(self.device, descriptor);
    }

    pub fn createSampler(self: *Gpu, descriptor: *const wgpu.SamplerDescriptor) wgpu.Sampler {
        return wgpu.deviceCreateSampler(self.device, descriptor);
    }

    pub fn createBindGroup(self: *Gpu, descriptor: *const wgpu.BindGroupDescriptor) wgpu.BindGroup {
        return wgpu.deviceCreateBindGroup(self.device, descriptor);
    }

    pub fn createBindGroupLayout(self: *Gpu, descriptor: *const wgpu.BindGroupLayoutDescriptor) wgpu.BindGroupLayout {
        return wgpu.deviceCreateBindGroupLayout(self.device, descriptor);
    }

    pub fn writeTextureImage(self: *Gpu, info: wgpu.TexelCopyTextureInfo, image: *const stbi.Image) void {
        self.writeTexture(
            info,
            .{
                .bytes_per_row = image.bytes_per_row,
                .rows_per_image = image.height,
            },
            .{
                .width = image.width,
                .height = image.height,
                .depth_or_array_layers = 1,
            },
            image.data,
        );
    }

    pub fn writeTexture(
        self: *Gpu,
        info: wgpu.TexelCopyTextureInfo,
        layout: wgpu.TexelCopyBufferLayout,
        extent: wgpu.Extent3D,
        data: anytype,
    ) void {
        const bytes = asBytes(data);
        wgpu.queueWriteTexture(self.queue, &info, bytes.ptr, bytes.len, &layout, &extent);
    }

    fn asBytes(data: anytype) []const u8 {
        const T = @TypeOf(data);
        const T_info = @typeInfo(T);
        const bytes = switch (T_info) {
            .pointer => |p_info| switch (p_info.size) {
                .one => std.mem.asBytes(data),
                .many, .c => if (p_info.sentinel_ptr != null) std.mem.sliceAsBytes(std.mem.span(data)) else @compileError("C pointer and pointer-to-many without sentinel are not supported"),
                .slice => std.mem.sliceAsBytes(data),
            },
            else => @compileError("Require a pointer; got " ++ @typeName(T)),
        };
        return bytes;
    }

    pub fn writeBuffer(self: *Gpu, buffer: wgpu.Buffer, write_offset: u64, data: anytype) void {
        const bytes = asBytes(data);
        wgpu.queueWriteBuffer(self.queue, buffer, write_offset, bytes.ptr, bytes.len);
    }

    pub fn getCommandEncoder(self: *Gpu, name: []const u8) wgpu.CommandEncoder {
        return wgpu.deviceCreateCommandEncoder(self.device, &.{ .label = .fromSlice(name) });
    }

    pub fn cancelCommandEncoder(_: *Gpu, encoder: wgpu.CommandEncoder) void {
        wgpu.commandEncoderRelease(encoder);
    }

    pub fn finalizeCommandEncoder(_: *Gpu, encoder: wgpu.CommandEncoder) wgpu.CommandBuffer {
        defer wgpu.commandEncoderRelease(encoder);
        return wgpu.commandEncoderFinish(encoder, null);
    }

    pub fn submitCommands(self: *Gpu, commands: []const wgpu.CommandBuffer) void {
        wgpu.queueSubmit(self.queue, commands.len, commands.ptr);

        for (commands) |cmd| wgpu.commandBufferRelease(cmd);
    }

    pub fn getFramebufferSize(self: *Gpu) linalg.vec2i {
        var width: i32 = 0;
        var height: i32 = 0;
        glfw.getFramebufferSize(self.getApp().window, &width, &height);
        return .{ width, height };
    }

    pub fn beginFrame(self: *Gpu) ?wgpu.TextureView {
        wgpu.surfaceGetCurrentTexture(self.surface, &self.frame_surface_texture);
        switch (self.frame_surface_texture.status) {
            .success_optimal, .success_suboptimal => {},
            .timeout, .outdated, .lost => {
                if (self.frame_surface_texture.texture != null) wgpu.textureRelease(self.frame_surface_texture.texture);
                var width: i32 = 0;
                var height: i32 = 0;
                glfw.getWindowSize(self.getApp().window, &width, &height);
                if (width != 0 and height != 0) {
                    self.config.width = @intCast(width);
                    self.config.height = @intCast(height);
                    wgpu.surfaceConfigure(self.surface, &self.config);
                    self.recreateDepthTexture();
                }
                return null;
            },
            else => std.debug.panic("get_current_texture status={any}", .{self.frame_surface_texture.status}),
        }
        std.debug.assert(self.frame_surface_texture.texture != null);

        const frame_surface_view = wgpu.textureCreateView(self.frame_surface_texture.texture, null);
        std.debug.assert(frame_surface_view != null);

        return frame_surface_view;
    }

    pub fn endFrame(self: *Gpu, frame_surface_view: wgpu.TextureView) void {
        _ = wgpu.surfacePresent(self.surface);

        wgpu.textureViewRelease(frame_surface_view);

        wgpu.textureRelease(self.frame_surface_texture.texture);
        @memset(std.mem.asBytes(&self.frame_surface_texture), 0);
    }
};

pub fn init(gpa: std.mem.Allocator, name: [*:0]const u8) !*Application {
    log.info("application initializing...", .{});

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

    glfw.windowHint(.{ .client_api = .none });
    glfw.windowHint(.{ .resizable = true });

    self.window = try glfw.createWindow(800, 600, name, null, null);
    errdefer glfw.destroyWindow(self.window);

    glfw.setWindowUserPointer(self.window, self);

    log.info("created glfw window", .{});

    try self.gpu.init();

    log.info("application initialized", .{});

    return self;
}

pub fn deinit(self: *Application) void {
    log.info("application shutting down ...", .{});

    self.gpu.deinit();

    glfw.destroyWindow(self.window);

    glfw.deinit();
    stbi.deinit();

    self.* = undefined;

    log.info("application shutdown completed successfully", .{});
}
