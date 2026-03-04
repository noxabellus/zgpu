const Gpu = @This();
const std = @import("std");
const builtin = @import("builtin");
const wgpu = @import("wgpu");
const glfw = @import("glfw");
const stbi = @import("stbi");
const linalg = @import("linalg.zig");
const Application = @import("Application.zig");

const log = std.log.scoped(.gpu);

instance: *Instance,
surface: *Surface,
surface_format: TextureFormat,
frame_surface_texture: ?*Texture,
adapter: *Adapter,
device: *Device,
config: SurfaceConfiguration,
capabilities: SurfaceCapabilities,
queue: *Queue,

pub const WGPU_LOG_LEVEL = LogLevel.warn;

pub fn init(self: *Gpu) !void {
    const window = self.getApp().window;

    setLogCallback(&struct {
        pub fn wgpu_logger(level: LogLevel, message: StringView, _: ?*anyopaque) callconv(.c) void {
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

    setLogLevel(WGPU_LOG_LEVEL);

    log.info("setup wgpu logger", .{});

    const backend = switch (builtin.os.tag) {
        .windows => if (glfw.isRunningInWine()) InstanceBackend.vulkanBackend else InstanceBackend.dx12Backend,
        .linux => InstanceBackend.vulkanBackend,
        else => @compileError("unsupported os " ++ @tagName(builtin.os.tag)),
    };

    log.info("selected backends: {}", .{backend});

    self.instance =
        if (createInstance(&InstanceDescriptor{
            .next_in_chain = @ptrCast(&InstanceExtras{
                .chain = .{ .s_type = .instance_extras },
                .backends = backend,
            }),
        })) |x| x else return error.FailedToCreateWgpuInstance;
    log.info("created wgpu instance", .{});

    errdefer self.instance.release();

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

            app.gpu.config.width = @intCast(width);
            app.gpu.config.height = @intCast(height);
            app.gpu.surface.configure(&app.gpu.config);
        }
    }.handle_glfw_framebuffer_size);

    if (comptime builtin.os.tag != .windows) {
        log.info("initializing wgpu surface for x11", .{});
        const x11_display = glfw.getX11Display();
        const x11_window = glfw.getX11Window(window);
        self.surface = try self.instance.createSurface(&SurfaceDescriptor{
            .next_in_chain = @ptrCast(&SurfaceSourceXlibWindow{
                .chain = .{ .s_type = .surface_source_xlib_window },
                .display = x11_display,
                .window = x11_window,
            }),
        });
    } else {
        log.info("initializing wgpu surface for win32", .{});
        const win32_hwnd = glfw.getWin32Window(window);
        const win32_hinstance = glfw.getWin32ModuleHandle();
        self.surface = try self.instance.createSurface(&SurfaceDescriptor{
            .next_in_chain = @ptrCast(&SurfaceSourceWindowsHWND{
                .chain = .{ .s_type = .surface_source_windows_hwnd },
                .hwnd = win32_hwnd,
                .hinstance = win32_hinstance,
            }),
        });
    }

    errdefer self.surface.release();

    log.info("wgpu surface initialized", .{});

    const TempWaitStatus = struct {
        complete: bool = false,
        success: bool = false,
    };
    var wait_status: TempWaitStatus = .{};

    _ = self.instance.requestAdapter(&RequestAdapterOptions{ .compatible_surface = self.surface }, .{ .callback = &struct {
        fn handle_request_adapter(status: RequestAdapterStatus, adapter: *Adapter, msg: StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
            const out_status: *TempWaitStatus = @ptrCast(@alignCast(ud2.?));
            defer out_status.complete = true;
            if (status == .success) {
                const gpu: *Gpu = @ptrCast(@alignCast(ud1.?));
                gpu.adapter = adapter;
                out_status.success = true;
            } else {
                log.err("request_adapter failed: {s}", .{msg.toSlice()});
            }
        }
    }.handle_request_adapter, .userdata1 = self, .userdata2 = &wait_status });

    while (!wait_status.complete) self.instance.processEvents();
    if (!wait_status.success) return error.FailedToRequestWgpuAdapter;
    errdefer self.adapter.release();

    log.info("got wgpu adapter", .{});

    wait_status = .{};
    _ = self.adapter.requestDevice(null, .{ .callback = &struct {
        fn handle_request_device(status: RequestDeviceStatus, device: *Device, msg: StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
            const out_status: *TempWaitStatus = @ptrCast(@alignCast(ud2.?));
            defer out_status.complete = true;
            if (status == .success) {
                const gpu: *Gpu = @ptrCast(@alignCast(ud1.?));
                gpu.device = device;
                out_status.success = true;
            } else {
                log.err("request_device failed: {s}", .{msg.toSlice()});
            }
        }
    }.handle_request_device, .userdata1 = self, .userdata2 = &wait_status });

    while (!wait_status.complete) self.instance.processEvents();
    if (!wait_status.success) return error.FailedToRequestWgpuDevice;
    errdefer self.device.release();

    log.info("got wgpu device", .{});

    self.queue = try self.device.getQueue();
    errdefer self.queue.release();

    log.info("got wgpu queue", .{});

    self.capabilities = try self.surface.getCapabilities(self.adapter);
    errdefer self.capabilities.freeMembers();

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

    self.surface.configure(&self.config);

    log.info("configured wgpu surface", .{});
}

pub fn deinit(self: *Gpu) void {
    self.capabilities.freeMembers();
    self.queue.release();
    self.device.release();
    self.adapter.release();
    self.surface.release();
    self.instance.release();
}

pub fn getApp(self: *Gpu) *Application {
    return @alignCast(@fieldParentPtr("gpu", self));
}

// pub fn loadShader(self: *Gpu, path: []const u8) !ShaderModule {
//     return loadShader(self.device, path);
// }

// pub fn loadShaderText(self: *Gpu, name: []const u8, source: []const u8) !*ShaderModule {
//     return loadShaderText(self.device, name, source);
// }

// pub fn createPipelineLayout(self: *Gpu, descriptor: *const PipelineLayoutDescriptor) PipelineLayout {
//     return self.device.createPipelineLayout(descriptor);
// }

// pub fn createComputePipeline(self: *Gpu, descriptor: *const ComputePipelineDescriptor) ComputePipeline {
//     return self.device.createComputePipeline(descriptor);
// }

// pub fn createRenderPipeline(self: *Gpu, descriptor: *const RenderPipelineDescriptor) RenderPipeline {
//     return self.device.createRenderPipeline(descriptor);
// }

// pub fn createBuffer(self: *Gpu, descriptor: *const BufferDescriptor) Buffer {
//     return self.device.createBuffer(descriptor);
// }

// pub fn createTexture(self: *Gpu, descriptor: *const TextureDescriptor) Texture {
//     return self.device.createTexture(descriptor);
// }

// pub fn createSampler(self: *Gpu, descriptor: *const SamplerDescriptor) Sampler {
//     return self.device.createSampler(descriptor);
// }

// pub fn createBindGroup(self: *Gpu, descriptor: *const BindGroupDescriptor) BindGroup {
//     return self.device.createBindGroup(descriptor);
// }

// pub fn createBindGroupLayout(self: *Gpu, descriptor: *const BindGroupLayoutDescriptor) BindGroupLayout {
//     return self.device.createBindGroupLayout(descriptor);
// }

// pub fn writeTextureImage(self: *Gpu, info: *const TexelCopyTextureInfo, image: *const stbi.Image) void {
//     self.queue.writeTexture(
//         info,
//         image.data.ptr,
//         image.data.len,
//         &.{
//             .bytes_per_row = image.bytes_per_row,
//             .rows_per_image = image.height,
//         },
//         &.{
//             .width = image.width,
//             .height = image.height,
//             .depth_or_array_layers = 1,
//         },
//     );
// }

// pub fn writeTexture(
//     self: *Gpu,
//     info: TexelCopyTextureInfo,
//     layout: TexelCopyBufferLayout,
//     extent: Extent3D,
//     data: anytype,
// ) void {
//     const bytes = asBytes(data);
//     self.queue.writeTexture(&info, bytes.ptr, bytes.len, &layout, &extent);
// }

// fn asBytes(data: anytype) []const u8 {
//     const T = @TypeOf(data);
//     const T_info = @typeInfo(T);
//     const bytes = switch (T_info) {
//         .pointer => |p_info| switch (p_info.size) {
//             .one => std.mem.asBytes(data),
//             .many, .c => if (p_info.sentinel_ptr != null) std.mem.sliceAsBytes(std.mem.span(data)) else @compileError("C pointer and pointer-to-many without sentinel are not supported"),
//             .slice => std.mem.sliceAsBytes(data),
//         },
//         else => @compileError("Require a pointer; got " ++ @typeName(T)),
//     };
//     return bytes;
// }

// pub fn writeBuffer(self: *Gpu, buffer: Buffer, write_offset: u64, data: anytype) void {
//     const bytes = asBytes(data);
//     self.queue.writeBuffer(buffer, write_offset, bytes.ptr, bytes.len);
// }

// pub fn getCommandEncoder(self: *Gpu, name: []const u8) CommandEncoder {
//     return self.device.createCommandEncoder(&.{ .label = .fromSlice(name) });
// }

// pub fn submitCommands(self: *Gpu, commands: []const CommandBuffer) void {
//     self.queue.submit(commands.len, commands.ptr);

//     for (commands) |cmd| cmd.release();
// }

pub fn beginFrame(self: *Gpu) !?*TextureView {
    var status: SurfaceGetCurrentTextureStatus = undefined;
    const new_frame_surface_texture = self.surface.getCurrentTexture(&status);
    switch (status) {
        .success_optimal, .success_suboptimal => {},
        .timeout, .outdated, .lost => {
            if (self.frame_surface_texture) |old_tex| old_tex.release();
            var width: i32 = 0;
            var height: i32 = 0;
            glfw.getFramebufferSize(self.getApp().window, &width, &height);
            if (width != 0 and height != 0) {
                self.config.width = @intCast(width);
                self.config.height = @intCast(height);
                self.surface.configure(&self.config);
            }
            return null;
        },
        else => std.debug.panic("get_current_texture status={any}", .{status}),
    }
    self.frame_surface_texture = new_frame_surface_texture;
    const frame_surface_view = try self.frame_surface_texture.?.createView(null);

    return frame_surface_view;
}

pub fn endFrame(self: *Gpu, frame_surface_view: *TextureView) !void {
    try self.surface.present();

    frame_surface_view.release();

    self.frame_surface_texture.?.release();
    @memset(std.mem.asBytes(&self.frame_surface_texture.?), 0);
}

pub const BigBool = wgpu.BigBool;
pub const undefined_depth_slice = wgpu.undefined_depth_slice;
pub const whole_size = wgpu.whole_size;
pub const BufferUsage = wgpu.BufferUsage;
pub const TextureUsage = wgpu.TextureUsage;
pub const ShaderStage = wgpu.ShaderStage;
pub const ColorWriteMask = wgpu.ColorWriteMask;
pub const MapMode = wgpu.MapMode;
pub const InstanceBackend = wgpu.InstanceBackend;
pub const InstanceFlag = wgpu.InstanceFlag;
pub const SType = wgpu.SType;
pub const BackendType = wgpu.BackendType;
pub const AdapterType = wgpu.AdapterType;
pub const BlendOperation = wgpu.BlendOperation;
pub const BlendFactor = wgpu.BlendFactor;
pub const BufferBindingType = wgpu.BufferBindingType;
pub const CompilationMessageType = wgpu.CompilationMessageType;
pub const PrimitiveTopology = wgpu.PrimitiveTopology;
pub const IndexFormat = wgpu.IndexFormat;
pub const FrontFace = wgpu.FrontFace;
pub const CullMode = wgpu.CullMode;
pub const QueryType = wgpu.QueryType;
pub const TextureFormat = wgpu.TextureFormat;
pub const LoadOp = wgpu.LoadOp;
pub const StoreOp = wgpu.StoreOp;
pub const FeatureLevel = wgpu.FeatureLevel;
pub const PowerPreference = wgpu.PowerPreference;
pub const SamplerBindingType = wgpu.SamplerBindingType;
pub const AddressMode = wgpu.AddressMode;
pub const FilterMode = wgpu.FilterMode;
pub const MipmapFilterMode = wgpu.MipmapFilterMode;
pub const CompareFunction = wgpu.CompareFunction;
pub const StencilOperation = wgpu.StencilOperation;
pub const StorageTextureAccess = wgpu.StorageTextureAccess;
pub const TextureViewDimension = wgpu.TextureViewDimension;
pub const FeatureName = wgpu.FeatureName;
pub const WGSLLanguageFeatureName = wgpu.WGSLLanguageFeatureName;
pub const PresentMode = wgpu.PresentMode;
pub const CompositeAlphaMode = wgpu.CompositeAlphaMode;
pub const SurfaceGetCurrentTextureStatus = wgpu.SurfaceGetCurrentTextureStatus;
pub const TextureSampleType = wgpu.TextureSampleType;
pub const TextureAspect = wgpu.TextureAspect;
pub const VertexFormat = wgpu.VertexFormat;
pub const OptionalBool = wgpu.OptionalBool;
pub const CallbackMode = wgpu.CallbackMode;
pub const DeviceLostReason = wgpu.DeviceLostReason;
pub const ErrorType = wgpu.ErrorType;
pub const TextureDimension = wgpu.TextureDimension;
pub const VertexStepMode = wgpu.VertexStepMode;
pub const MapAsyncStatus = wgpu.MapAsyncStatus;
pub const CompilationInfoRequestStatus = wgpu.CompilationInfoRequestStatus;
pub const CreatePipelineAsyncStatus = wgpu.CreatePipelineAsyncStatus;
pub const PopErrorScopeStatus = wgpu.PopErrorScopeStatus;
pub const QueueWorkDoneStatus = wgpu.QueueWorkDoneStatus;
pub const RequestAdapterStatus = wgpu.RequestAdapterStatus;
pub const RequestDeviceStatus = wgpu.RequestDeviceStatus;
pub const BufferMapState = wgpu.BufferMapState;
pub const ErrorFilter = wgpu.ErrorFilter;
pub const Status = wgpu.Status;
pub const WaitStatus = wgpu.WaitStatus;
pub const NativeFeature = wgpu.NativeFeature;
pub const LogLevel = wgpu.LogLevel;
pub const Dx12Compiler = wgpu.Dx12Compiler;
pub const Gles3MinorVersion = wgpu.Gles3MinorVersion;
pub const PipelineStatisticName = wgpu.PipelineStatisticName;
pub const NativeQueryType = wgpu.NativeQueryType;
pub const DxcMaxShaderModel = wgpu.DxcMaxShaderModel;
pub const GLFenceBehaviour = wgpu.GLFenceBehaviour;
pub const NativeTextureFormat = wgpu.NativeTextureFormat;
pub const StringView = wgpu.StringView;
pub const Adapter = wgpu.Adapter;
pub const BindGroup = wgpu.BindGroup;
pub const BindGroupLayout = wgpu.BindGroupLayout;
pub const Buffer = wgpu.Buffer;
pub const CommandBuffer = wgpu.CommandBuffer;
pub const CommandEncoder = wgpu.CommandEncoder;
pub const ComputePassEncoder = wgpu.ComputePassEncoder;
pub const ComputePipeline = wgpu.ComputePipeline;
pub const Device = wgpu.Device;
pub const Instance = wgpu.Instance;
pub const PipelineLayout = wgpu.PipelineLayout;
pub const QuerySet = wgpu.QuerySet;
pub const Queue = wgpu.Queue;
pub const RenderBundle = wgpu.RenderBundle;
pub const RenderBundleEncoder = wgpu.RenderBundleEncoder;
pub const RenderPassEncoder = wgpu.RenderPassEncoder;
pub const RenderPipeline = wgpu.RenderPipeline;
pub const Sampler = wgpu.Sampler;
pub const ShaderModule = wgpu.ShaderModule;
pub const Surface = wgpu.Surface;
pub const Texture = wgpu.Texture;
pub const TextureView = wgpu.TextureView;
pub const ChainedStructOut = wgpu.ChainedStructOut;
pub const AdapterInfo = wgpu.AdapterInfo;
pub const ChainedStruct = wgpu.ChainedStruct;
pub const BindGroupEntry = wgpu.BindGroupEntry;
pub const BlendComponent = wgpu.BlendComponent;
pub const BufferBindingLayout = wgpu.BufferBindingLayout;
pub const BufferDescriptor = wgpu.BufferDescriptor;
pub const Color = wgpu.Color;
pub const CommandBufferDescriptor = wgpu.CommandBufferDescriptor;
pub const CommandEncoderDescriptor = wgpu.CommandEncoderDescriptor;
pub const CompilationMessage = wgpu.CompilationMessage;
pub const ComputePassTimestampWrites = wgpu.ComputePassTimestampWrites;
pub const ConstantEntry = wgpu.ConstantEntry;
pub const Extent3D = wgpu.Extent3D;
pub const Future = wgpu.Future;
pub const InstanceCapabilities = wgpu.InstanceCapabilities;
pub const Limits = wgpu.Limits;
pub const MultisampleState = wgpu.MultisampleState;
pub const Origin3D = wgpu.Origin3D;
pub const PipelineLayoutDescriptor = wgpu.PipelineLayoutDescriptor;
pub const PrimitiveState = wgpu.PrimitiveState;
pub const QuerySetDescriptor = wgpu.QuerySetDescriptor;
pub const QueueDescriptor = wgpu.QueueDescriptor;
pub const RenderBundleDescriptor = wgpu.RenderBundleDescriptor;
pub const RenderBundleEncoderDescriptor = wgpu.RenderBundleEncoderDescriptor;
pub const RenderPassDepthStencilAttachment = wgpu.RenderPassDepthStencilAttachment;
pub const RenderPassMaxDrawCount = wgpu.RenderPassMaxDrawCount;
pub const RenderPassTimestampWrites = wgpu.RenderPassTimestampWrites;
pub const RequestAdapterOptions = wgpu.RequestAdapterOptions;
pub const SamplerBindingLayout = wgpu.SamplerBindingLayout;
pub const SamplerDescriptor = wgpu.SamplerDescriptor;
pub const ShaderModuleDescriptor = wgpu.ShaderModuleDescriptor;
pub const ShaderSourceSPIRV = wgpu.ShaderSourceSPIRV;
pub const ShaderSourceWGSL = wgpu.ShaderSourceWGSL;
pub const StencilFaceState = wgpu.StencilFaceState;
pub const StorageTextureBindingLayout = wgpu.StorageTextureBindingLayout;
pub const SupportedFeatures = wgpu.SupportedFeatures;
pub const SupportedWGSLLanguageFeatures = wgpu.SupportedWGSLLanguageFeatures;
pub const SurfaceCapabilities = wgpu.SurfaceCapabilities;
pub const SurfaceConfiguration = wgpu.SurfaceConfiguration;
pub const SurfaceDescriptor = wgpu.SurfaceDescriptor;
pub const SurfaceSourceAndroidNativeWindow = wgpu.SurfaceSourceAndroidNativeWindow;
pub const SurfaceSourceMetalLayer = wgpu.SurfaceSourceMetalLayer;
pub const SurfaceSourceWaylandSurface = wgpu.SurfaceSourceWaylandSurface;
pub const SurfaceSourceWindowsHWND = wgpu.SurfaceSourceWindowsHWND;
pub const SurfaceSourceXCBWindow = wgpu.SurfaceSourceXCBWindow;
pub const SurfaceSourceXlibWindow = wgpu.SurfaceSourceXlibWindow;
pub const SurfaceTexture = wgpu.SurfaceTexture;
pub const TexelCopyBufferLayout = wgpu.TexelCopyBufferLayout;
pub const TextureBindingLayout = wgpu.TextureBindingLayout;
pub const TextureViewDescriptor = wgpu.TextureViewDescriptor;
pub const VertexAttribute = wgpu.VertexAttribute;
pub const BindGroupDescriptor = wgpu.BindGroupDescriptor;
pub const BindGroupLayoutEntry = wgpu.BindGroupLayoutEntry;
pub const BlendState = wgpu.BlendState;
pub const CompilationInfo = wgpu.CompilationInfo;
pub const ComputePassDescriptor = wgpu.ComputePassDescriptor;
pub const DepthStencilState = wgpu.DepthStencilState;
pub const DeviceLostCallbackInfo = wgpu.DeviceLostCallbackInfo;
pub const UncapturedErrorCallbackInfo = wgpu.UncapturedErrorCallbackInfo;
pub const DeviceDescriptor = wgpu.DeviceDescriptor;
pub const FutureWaitInfo = wgpu.FutureWaitInfo;
pub const InstanceDescriptor = wgpu.InstanceDescriptor;
pub const ProgrammableStageDescriptor = wgpu.ProgrammableStageDescriptor;
pub const RenderPassColorAttachment = wgpu.RenderPassColorAttachment;
pub const TexelCopyBufferInfo = wgpu.TexelCopyBufferInfo;
pub const TexelCopyTextureInfo = wgpu.TexelCopyTextureInfo;
pub const TextureDescriptor = wgpu.TextureDescriptor;
pub const VertexBufferLayout = wgpu.VertexBufferLayout;
pub const BindGroupLayoutDescriptor = wgpu.BindGroupLayoutDescriptor;
pub const ColorTargetState = wgpu.ColorTargetState;
pub const ComputePipelineDescriptor = wgpu.ComputePipelineDescriptor;
pub const RenderPassDescriptor = wgpu.RenderPassDescriptor;
pub const VertexState = wgpu.VertexState;
pub const FragmentState = wgpu.FragmentState;
pub const RenderPipelineDescriptor = wgpu.RenderPipelineDescriptor;
pub const BufferMapCallbackInfo = wgpu.BufferMapCallbackInfo;
pub const CompilationInfoCallbackInfo = wgpu.CompilationInfoCallbackInfo;
pub const CreateComputePipelineAsyncCallbackInfo = wgpu.CreateComputePipelineAsyncCallbackInfo;
pub const CreateRenderPipelineAsyncCallbackInfo = wgpu.CreateRenderPipelineAsyncCallbackInfo;
pub const PopErrorScopeCallbackInfo = wgpu.PopErrorScopeCallbackInfo;
pub const QueueWorkDoneCallbackInfo = wgpu.QueueWorkDoneCallbackInfo;
pub const RequestAdapterCallbackInfo = wgpu.RequestAdapterCallbackInfo;
pub const RequestDeviceCallbackInfo = wgpu.RequestDeviceCallbackInfo;
pub const Proc = wgpu.Proc;
pub const InstanceExtras = wgpu.InstanceExtras;
pub const DeviceExtras = wgpu.DeviceExtras;
pub const NativeLimits = wgpu.NativeLimits;
pub const PushConstantRange = wgpu.PushConstantRange;
pub const PipelineLayoutExtras = wgpu.PipelineLayoutExtras;
pub const SubmissionIndex = wgpu.SubmissionIndex;
pub const ShaderDefine = wgpu.ShaderDefine;
pub const ShaderSourceGLSL = wgpu.ShaderSourceGLSL;
pub const ShaderModuleDescriptorSpirV = wgpu.ShaderModuleDescriptorSpirV;
pub const RegistryReport = wgpu.RegistryReport;
pub const HubReport = wgpu.HubReport;
pub const GlobalReport = wgpu.GlobalReport;
pub const InstanceEnumerateAdapterOptions = wgpu.InstanceEnumerateAdapterOptions;
pub const BindGroupEntryExtras = wgpu.BindGroupEntryExtras;
pub const BindGroupLayoutEntryExtras = wgpu.BindGroupLayoutEntryExtras;
pub const QuerySetDescriptorExtras = wgpu.QuerySetDescriptorExtras;
pub const SurfaceConfigurationExtras = wgpu.SurfaceConfigurationExtras;
pub const SurfaceSourceSwapChainPanel = wgpu.SurfaceSourceSwapChainPanel;
pub const DeviceLostCallback = wgpu.DeviceLostCallback;
pub const UncapturedErrorCallback = wgpu.UncapturedErrorCallback;
pub const BufferMapCallback = wgpu.BufferMapCallback;
pub const CompilationInfoCallback = wgpu.CompilationInfoCallback;
pub const CreateComputePipelineAsyncCallback = wgpu.CreateComputePipelineAsyncCallback;
pub const CreateRenderPipelineAsyncCallback = wgpu.CreateRenderPipelineAsyncCallback;
pub const PopErrorScopeCallback = wgpu.PopErrorScopeCallback;
pub const QueueWorkDoneCallback = wgpu.QueueWorkDoneCallback;
pub const RequestAdapterCallback = wgpu.RequestAdapterCallback;
pub const RequestDeviceCallback = wgpu.RequestDeviceCallback;
pub const LogCallback = wgpu.LogCallback;
pub const createInstance = wgpu.createInstance;
pub const getInstanceCapabilities = wgpu.getInstanceCapabilities;
pub const getProcAddress = wgpu.getProcAddress;
pub const setLogCallback = wgpu.setLogCallback;
pub const setLogLevel = wgpu.setLogLevel;
pub const getVersion = wgpu.getVersion;
