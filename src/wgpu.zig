const std = @import("std");

pub const BigBool = enum(u32) { false = 0, true = 1 };

pub const undefined_depth_slice = std.math.maxInt(u32);

pub const BufferUsage = packed struct(u64) {
    map_read: bool = false,
    map_write: bool = false,
    copy_src: bool = false,
    copy_dst: bool = false,
    index: bool = false,
    vertex: bool = false,
    uniform: bool = false,
    storage: bool = false,
    indirect: bool = false,
    query_resolve: bool = false,
    _: u54 = 0, // Pad to 64 bits

    pub const none = BufferUsage{};

    pub const mapReadUsage = BufferUsage{ .map_read = true };
    pub const mapWriteUsage = BufferUsage{ .map_write = true };
    pub const copySrcUsage = BufferUsage{ .copy_src = true };
    pub const copyDstUsage = BufferUsage{ .copy_dst = true };
    pub const indexUsage = BufferUsage{ .index = true };
    pub const vertexUsage = BufferUsage{ .vertex = true };
    pub const uniformUsage = BufferUsage{ .uniform = true };
    pub const storageUsage = BufferUsage{ .storage = true };
    pub const indirectUsage = BufferUsage{ .indirect = true };
    pub const queryResolveUsage = BufferUsage{ .query_resolve = true };

    pub fn merge(self: BufferUsage, other: BufferUsage) BufferUsage {
        return @bitCast(@as(u64, @bitCast(self)) | @as(u64, @bitCast(other)));
    }
};

pub const TextureUsage = packed struct(u64) {
    copy_src: bool = false,
    copy_dst: bool = false,
    texture_binding: bool = false,
    storage_binding: bool = false,
    render_attachment: bool = false,
    _: u59 = 0, // Pad to 64 bits

    pub const none = TextureUsage{};

    pub const copySrcUsage = TextureUsage{ .copy_src = true };
    pub const copyDstUsage = TextureUsage{ .copy_dst = true };
    pub const textureBindingUsage = TextureUsage{ .texture_binding = true };
    pub const storageBindingUsage = TextureUsage{ .storage_binding = true };
    pub const renderAttachmentUsage = TextureUsage{ .render_attachment = true };

    pub fn merge(self: TextureUsage, other: TextureUsage) TextureUsage {
        return @bitCast(@as(u64, @bitCast(self)) | @as(u64, @bitCast(other)));
    }
};

pub const ShaderStage = packed struct(u64) {
    vertex: bool = false,
    fragment: bool = false,
    compute: bool = false,
    _: u61 = 0, // Pad to 64 bits

    pub const none = ShaderStage{};

    pub const vertexStage = ShaderStage{ .vertex = true };
    pub const fragmentStage = ShaderStage{ .fragment = true };
    pub const computeStage = ShaderStage{ .compute = true };

    pub fn merge(self: ShaderStage, other: ShaderStage) ShaderStage {
        return @bitCast(@as(u64, @bitCast(self)) | @as(u64, @bitCast(other)));
    }
};

pub const ColorWriteMask = packed struct(u64) {
    red: bool = false,
    green: bool = false,
    blue: bool = false,
    alpha: bool = false,
    _: u60 = 0, // Pad to 64 bits

    pub const none = ColorWriteMask{};
    pub const all = ColorWriteMask{ .red = true, .green = true, .blue = true, .alpha = true };

    pub const redMask = ColorWriteMask{ .red = true };
    pub const greenMask = ColorWriteMask{ .green = true };
    pub const blueMask = ColorWriteMask{ .blue = true };
    pub const alphaMask = ColorWriteMask{ .alpha = true };

    pub fn merge(self: ColorWriteMask, other: ColorWriteMask) ColorWriteMask {
        return @bitCast(@as(u64, @bitCast(self)) | @as(u64, @bitCast(other)));
    }
};

pub const MapMode = packed struct(u64) {
    read: bool = false,
    write: bool = false,
    _: u62 = 0, // Pad to 64 bits

    pub const none = MapMode{};
    pub const readMode = MapMode{ .read = true };
    pub const writeMode = MapMode{ .write = true };

    pub fn merge(self: MapMode, other: MapMode) MapMode {
        return @bitCast(@as(u64, @bitCast(self)) | @as(u64, @bitCast(other)));
    }
};

pub const InstanceBackend = packed struct {
    vulkan: bool = false,
    gl: bool = false,
    metal: bool = false,
    dx12: bool = false,
    dx11: bool = false,
    browser_web_gpu: bool = false,
    _: u58 = 0, // Pad to 64 bits

    pub const all = InstanceBackend{};
    pub const vulkanBackend = InstanceBackend{ .vulkan = true };
    pub const glBackend = InstanceBackend{ .gl = true };
    pub const metalBackend = InstanceBackend{ .metal = true };
    pub const dx12Backend = InstanceBackend{ .dx12 = true };
    pub const dx11Backend = InstanceBackend{ .dx11 = true };
    pub const browserWebGPU = InstanceBackend{ .browser_web_gpu = true };

    pub const primary = InstanceBackend{ .vulkan = true, .metal = true, .dx12 = true, .browser_web_gpu = true };
    pub const secondary = InstanceBackend{ .gl = true, .dx11 = true };

    pub fn merge(self: InstanceBackend, other: InstanceBackend) InstanceBackend {
        return @bitCast(@as(u64, @bitCast(self)) | @as(u64, @bitCast(other)));
    }
};

pub const InstanceFlag = packed struct {
    debug: bool = false,
    validation: bool = false,
    discard_hal_labels: bool = false,
    _: u61 = 0, // Pad to 64 bits

    pub const none = InstanceFlag{};
    pub const debugFlag = InstanceFlag{ .debug = true };
    pub const validationFlag = InstanceFlag{ .validation = true };
    pub const discardHalLabelsFlag = InstanceFlag{ .discard_hal_labels = true };

    pub fn merge(self: InstanceFlag, other: InstanceFlag) InstanceFlag {
        return @bitCast(@as(u64, @bitCast(self)) | @as(u64, @bitCast(other)));
    }
};

pub const SType = enum(u32) {
    undefined = 0,
    shader_source_spirv = 1,
    shader_source_wgsl = 2,
    render_pass_max_draw_count = 3,
    surface_source_metal_layer = 4,
    surface_source_windows_hwnd = 5,
    surface_source_xlib_window = 6,
    surface_source_wayland_surface = 7,
    surface_source_android_native_window = 8,
    surface_source_xcb_window = 9,
    device_extras = 196609,
    native_limits = 196610,
    pipeline_layout_extras = 196611,
    shader_source_glsl = 196612,
    instance_extras = 196614,
    bind_group_entry_extras = 196615,
    bind_group_layout_entry_extras = 196616,
    query_set_descriptor_extras = 196617,
    surface_configuration_extras = 196618,
    surface_source_swap_chain_panel = 196619,
};

pub const BackendType = enum(u32) {
    undefined = 0,
    null = 1,
    web_gpu = 2,
    d3d11 = 3,
    d3d12 = 4,
    metal = 5,
    vulkan = 6,
    open_gl = 7,
    open_gles = 8,
};

pub const AdapterType = enum(u32) {
    undefined = 0,
    discrete_gpu = 1,
    integrated_gpu = 2,
    cpu = 3,
    unknown = 4,
};

pub const BlendOperation = enum(u32) {
    undefined = 0,
    add = 1,
    subtract = 2,
    reverse_subtract = 3,
    min = 4,
    max = 5,
};

pub const BlendFactor = enum(u32) {
    undefined = 0,
    zero = 1,
    one = 2,
    src = 3,
    one_minus_src = 4,
    src_alpha = 5,
    one_minus_src_alpha = 6,
    dst = 7,
    one_minus_dst = 8,
    dst_alpha = 9,
    one_minus_dst_alpha = 10,
    src_alpha_saturated = 11,
    constant = 12,
    one_minus_constant = 13,
    src1 = 14,
    one_minus_src1 = 15,
    src1_alpha = 16,
    one_minus_src1_alpha = 17,
};

pub const BufferBindingType = enum(u32) {
    binding_not_used = 0,
    undefined = 1,
    uniform = 2,
    storage = 3,
    read_only_storage = 4,
};

pub const CompilationMessageType = enum(u32) {
    undefined = 0,
    @"error" = 1,
    warning = 2,
    info = 3,
};

pub const PrimitiveTopology = enum(u32) {
    undefined = 0,
    point_list = 1,
    line_list = 2,
    line_strip = 3,
    triangle_list = 4,
    triangle_strip = 5,
};

pub const IndexFormat = enum(u32) {
    undefined = 0,
    uint16 = 1,
    uint32 = 2,
};

pub const FrontFace = enum(u32) {
    undefined = 0,
    ccw = 1,
    cw = 2,
};

pub const CullMode = enum(u32) {
    undefined = 0,
    none = 1,
    front = 2,
    back = 3,
};

pub const QueryType = enum(u32) {
    undefined = 0,
    occlusion = 1,
    timestamp = 2,
};

pub const TextureFormat = enum(u32) {
    undefined = 0,
    r8_unorm = 1,
    r8_snorm = 2,
    r8_uint = 3,
    r8_sint = 4,
    r16_uint = 5,
    r16_sint = 6,
    r16_float = 7,
    rg8_unorm = 8,
    rg8_snorm = 9,
    rg8_uint = 10,
    rg8_sint = 11,
    r32_float = 12,
    r32_uint = 13,
    r32_sint = 14,
    rg16_uint = 15,
    rg16_sint = 16,
    rg16_float = 17,
    rgba8_unorm = 18,
    rgba8_unorm_srgb = 19,
    rgba8_snorm = 20,
    rgba8_uint = 21,
    rgba8_sint = 22,
    bgra8_unorm = 23,
    bgra8_unorm_srgb = 24,
    rgb10a2_uint = 25,
    rgb10a2_unorm = 26,
    rg11b10_ufloat = 27,
    rgb9e5_ufloat = 28,
    rg32_float = 29,
    rg32_uint = 30,
    rg32_sint = 31,
    rgba16_uint = 32,
    rgba16_sint = 33,
    rgba16_float = 34,
    rgba32_float = 35,
    rgba32_uint = 36,
    rgba32_sint = 37,
    stencil8 = 38,
    depth16_unorm = 39,
    depth24_plus = 40,
    depth24_plus_stencil8 = 41,
    depth32_float = 42,
    depth32_float_stencil8 = 43,
    bc1rgba_unorm = 44,
    bc1rgba_unorm_srgb = 45,
    bc2rgba_unorm = 46,
    bc2rgba_unorm_srgb = 47,
    bc3rgba_unorm = 48,
    bc3rgba_unorm_srgb = 49,
    bc4r_unorm = 50,
    bc4r_snorm = 51,
    bc5rg_unorm = 52,
    bc5rg_snorm = 53,
    bc6hrgb_ufloat = 54,
    bc6hrgb_float = 55,
    bc7rgba_unorm = 56,
    bc7rgba_unorm_srgb = 57,
    etc2rgb8_unorm = 58,
    etc2rgb8_unorm_srgb = 59,
    etc2rgb8a1_unorm = 60,
    etc2rgb8a1_unorm_srgb = 61,
    etc2rgba8_unorm = 62,
    etc2rgba8_unorm_srgb = 63,
    eacr11_unorm = 64,
    eacr11_snorm = 65,
    eacrg11_unorm = 66,
    eacrg11_snorm = 67,
    astc4x4_unorm = 68,
    astc4x4_unorm_srgb = 69,
    astc5x4_unorm = 70,
    astc5x4_unorm_srgb = 71,
    astc5x5_unorm = 72,
    astc5x5_unorm_srgb = 73,
    astc6x5_unorm = 74,
    astc6x5_unorm_srgb = 75,
    astc6x6_unorm = 76,
    astc6x6_unorm_srgb = 77,
    astc8x5_unorm = 78,
    astc8x5_unorm_srgb = 79,
    astc8x6_unorm = 80,
    astc8x6_unorm_srgb = 81,
    astc8x8_unorm = 82,
    astc8x8_unorm_srgb = 83,
    astc10x5_unorm = 84,
    astc10x5_unorm_srgb = 85,
    astc10x6_unorm = 86,
    astc10x6_unorm_srgb = 87,
    astc10x8_unorm = 88,
    astc10x8_unorm_srgb = 89,
    astc10x10_unorm = 90,
    astc10x10_unorm_srgb = 91,
    astc12x10_unorm = 92,
    astc12x10_unorm_srgb = 93,
    astc12x12_unorm = 94,
    astc12x12_unorm_srgb = 95,
};

pub const LoadOp = enum(u32) {
    undefined = 0,
    load = 1,
    clear = 2,
};

pub const StoreOp = enum(u32) {
    undefined = 0,
    store = 1,
    discard = 2,
};

pub const FeatureLevel = enum(u32) {
    undefined = 0,
    compatibility = 1,
    core = 2,
};

pub const PowerPreference = enum(u32) {
    undefined = 0,
    low_power = 1,
    high_performance = 2,
};

pub const SamplerBindingType = enum(u32) {
    binding_not_used = 0,
    undefined = 1,
    filtering = 2,
    non_filtering = 3,
    comparison = 4,
};

pub const AddressMode = enum(u32) {
    undefined = 0,
    clamp_to_edge = 1,
    repeat = 2,
    mirror_repeat = 3,
};

pub const FilterMode = enum(u32) {
    undefined = 0,
    nearest = 1,
    linear = 2,
};

pub const MipmapFilterMode = enum(u32) {
    undefined = 0,
    nearest = 1,
    linear = 2,
};

pub const CompareFunction = enum(u32) {
    undefined = 0,
    never = 1,
    less = 2,
    equal = 3,
    less_equal = 4,
    greater = 5,
    not_equal = 6,
    greater_equal = 7,
    always = 8,
};

pub const StencilOperation = enum(u32) {
    undefined = 0,
    keep = 1,
    zero = 2,
    replace = 3,
    invert = 4,
    increment_clamp = 5,
    decrement_clamp = 6,
    increment_wrap = 7,
    decrement_wrap = 8,
};

pub const StorageTextureAccess = enum(u32) {
    binding_not_used = 0,
    undefined = 1,
    write_only = 2,
    read_only = 3,
    read_write = 4,
};

pub const TextureViewDimension = enum(u32) {
    undefined = 0,
    @"1d" = 1,
    @"2d" = 2,
    @"2d_array" = 3,
    cube = 4,
    cube_array = 5,
    @"3d" = 6,
};

pub const FeatureName = enum(u32) {
    undefined = 0,
    depth_clip_control = 1,
    depth32_float_stencil8 = 2,
    timestamp_query = 3,
    texture_compression_bc = 4,
    texture_compression_bc_sliced3d = 5,
    texture_compression_etc2 = 6,
    texture_compression_astc = 7,
    texture_compression_astc_sliced3d = 8,
    indirect_first_instance = 9,
    shader_f16 = 10,
    rg11b10_ufloat_renderable = 11,
    bgra8_unorm_storage = 12,
    float32_filterable = 13,
    float32_blendable = 14,
    clip_distances = 15,
    dual_source_blending = 16,
};

pub const WGSLLanguageFeatureName = enum(u32) {
    readonly_and_readwrite_storage_textures = 1,
    packed4x8_integer_dot_product = 2,
    unrestricted_pointer_parameters = 3,
    pointer_composite_access = 4,
};

pub const PresentMode = enum(u32) {
    undefined = 0,
    fifo = 1,
    fifo_relaxed = 2,
    immediate = 3,
    mailbox = 4,
};

pub const CompositeAlphaMode = enum(u32) {
    auto = 0,
    @"opaque" = 1,
    premultiplied = 2,
    unpremultiplied = 3,
    inherit = 4,
};

pub const SurfaceGetCurrentTextureStatus = enum(u32) {
    undefined = 0,
    success_optimal = 1,
    success_suboptimal = 2,
    timeout = 3,
    outdated = 4,
    lost = 5,
    out_of_memory = 6,
    device_lost = 7,
    @"error" = 8,
};

pub const TextureSampleType = enum(u32) {
    binding_not_used = 0,
    undefined = 1,
    float = 2,
    unfilterable_float = 3,
    depth = 4,
    sint = 5,
    uint = 6,
};

pub const TextureAspect = enum(u32) {
    undefined = 0,
    all = 1,
    stencil_only = 2,
    depth_only = 3,
};

pub const VertexFormat = enum(u32) {
    undefined = 0,
    uint8 = 1,
    uint8x2 = 2,
    uint8x4 = 3,
    sint8 = 4,
    sint8x2 = 5,
    sint8x4 = 6,
    unorm8 = 7,
    unorm8x2 = 8,
    unorm8x4 = 9,
    snorm8 = 10,
    snorm8x2 = 11,
    snorm8x4 = 12,
    uint16 = 13,
    uint16x2 = 14,
    uint16x4 = 15,
    sint16 = 16,
    sint16x2 = 17,
    sint16x4 = 18,
    unorm16 = 19,
    unorm16x2 = 20,
    unorm16x4 = 21,
    snorm16 = 22,
    snorm16x2 = 23,
    snorm16x4 = 24,
    float16 = 25,
    float16x2 = 26,
    float16x4 = 27,
    float32 = 28,
    float32x2 = 29,
    float32x3 = 30,
    float32x4 = 31,
    uint32 = 32,
    uint32x2 = 33,
    uint32x3 = 34,
    uint32x4 = 35,
    sint32 = 36,
    sint32x2 = 37,
    sint32x3 = 38,
    sint32x4 = 39,
    unorm10_10_10_2 = 40,
    unorm8x4bgra = 41,
};

pub const OptionalBool = enum(u32) {
    false = 0,
    true = 1,
    undefined = 2,
};

pub const CallbackMode = enum(u32) {
    undefined = 0,
    wait_any_only = 1,
    allow_process_events = 2,
    allow_spontaneous = 3,
};

pub const DeviceLostReason = enum(u32) {
    unknown = 1,
    destroyed = 2,
    instance_dropped = 3,
    failed_creation = 4,
};

pub const ErrorType = enum(u32) {
    no_error = 1,
    validation = 2,
    out_of_memory = 3,
    internal = 4,
    unknown = 5,
};

pub const TextureDimension = enum(u32) {
    undefined = 0,
    @"1d" = 1,
    @"2d" = 2,
    @"3d" = 3,
};

pub const VertexStepMode = enum(u32) {
    vertex_buffer_not_used = 0,
    undefined = 1,
    vertex = 2,
    instance = 3,
};

pub const MapAsyncStatus = enum(u32) {
    success = 1,
    instance_dropped = 2,
    @"error" = 3,
    aborted = 4,
    unknown = 5,
};

pub const CompilationInfoRequestStatus = enum(u32) {
    success = 1,
    instance_dropped = 2,
    @"error" = 3,
    unknown = 4,
};

pub const CreatePipelineAsyncStatus = enum(u32) {
    success = 1,
    instance_dropped = 2,
    validation_error = 3,
    internal_error = 4,
    unknown = 5,
};

pub const PopErrorScopeStatus = enum(u32) {
    success = 1,
    instance_dropped = 2,
    empty_stack = 3,
};

pub const QueueWorkDoneStatus = enum(u32) {
    success = 1,
    instance_dropped = 2,
    @"error" = 3,
    unknown = 4,
};

pub const RequestAdapterStatus = enum(u32) {
    success = 1,
    instance_dropped = 2,
    unavailable = 3,
    @"error" = 4,
    unknown = 5,
};

pub const RequestDeviceStatus = enum(u32) {
    success = 1,
    instance_dropped = 2,
    @"error" = 3,
    unknown = 4,
};

pub const BufferMapState = enum(u32) {
    unmapped = 1,
    pending = 2,
    mapped = 3,
};

pub const ErrorFilter = enum(u32) {
    validation = 1,
    out_of_memory = 2,
    internal = 3,
};

pub const Status = enum(u32) {
    success = 1,
    @"error" = 2,
};

pub const WaitStatus = enum(u32) {
    success = 1,
    timed_out = 2,
    unsupported_timeout = 3,
    unsupported_count = 4,
    unsupported_mixed_sources = 5,
};

pub const NativeFeature = enum(u32) {
    push_constants = 196609,
    texture_adapter_specific_format_features = 196610,
    multi_draw_indirect = 196611,
    multi_draw_indirect_count = 196612,
    vertex_writable_storage = 196613,
    texture_binding_array = 196614,
    sampled_texture_and_storage_buffer_array_non_uniform_indexing = 196615,
    pipeline_statistics_query = 196616,
    storage_resource_binding_array = 196617,
    partially_bound_binding_array = 196618,
    texture_format16bit_norm = 196619,
    texture_compression_astc_hdr = 196620,
    mappable_primary_buffers = 196622,
    buffer_binding_array = 196623,
    uniform_buffer_and_storage_texture_array_non_uniform_indexing = 196624,
    spirv_shader_passthrough = 196631,
    vertex_attribute64bit = 196633,
    texture_format_nv12 = 196634,
    ray_tracing_acceleration_structure = 196635,
    ray_query = 196636,
    shader_f64 = 196637,
    shader_i16 = 196638,
    shader_primitive_index = 196639,
    shader_early_depth_test = 196640,
    subgroup = 196641,
    subgroup_vertex = 196642,
    subgroup_barrier = 196643,
    timestamp_query_inside_encoders = 196644,
    timestamp_query_inside_passes = 196645,
    shader_int64 = 196646,
};

pub const LogLevel = enum(u32) {
    off = 0,
    @"error" = 1,
    warn = 2,
    info = 3,
    debug = 4,
    trace = 5,
};

pub const Dx12Compiler = enum(u32) {
    undefined = 0,
    fxc = 1,
    dxc = 2,
};

pub const Gles3MinorVersion = enum(u32) {
    automatic = 0,
    version0 = 1,
    version1 = 2,
    version2 = 3,
};

pub const PipelineStatisticName = enum(u32) {
    vertex_shader_invocations = 0,
    clipper_invocations = 1,
    clipper_primitives_out = 2,
    fragment_shader_invocations = 3,
    compute_shader_invocations = 4,
};

pub const NativeQueryType = enum(u32) {
    pipeline_statistics = 196608,
};

pub const DxcMaxShaderModel = enum(u32) {
    v6_0 = 0,
    v6_1 = 1,
    v6_2 = 2,
    v6_3 = 3,
    v6_4 = 4,
    v6_5 = 5,
    v6_6 = 6,
    v6_7 = 7,
};

pub const GLFenceBehaviour = enum(u32) {
    normal = 0,
    auto_finish = 1,
};

pub const NativeTextureFormat = enum(u32) {
    r16_unorm = 196609,
    r16_snorm = 196610,
    rg16_unorm = 196611,
    rg16_snorm = 196612,
    rgba16_unorm = 196613,
    rgba16_snorm = 196614,
    nv12 = 196615,
};

pub const StringView = extern struct {
    data: ?[*]const u8 = null,
    length: usize = 0,

    pub fn fromSlice(slice: []const u8) StringView {
        return StringView{
            .data = slice.ptr,
            .length = slice.len,
        };
    }

    pub fn toSlice(self: StringView) []const u8 {
        if (self.data) |d| {
            return d[0..self.length];
        } else {
            return &.{};
        }
    }
};

pub const Adapter = ?*AdapterImpl;
pub const AdapterImpl = opaque {};

pub const BindGroup = ?*BindGroupImpl;
pub const BindGroupImpl = opaque {};

pub const BindGroupLayout = ?*BindGroupLayoutImpl;
pub const BindGroupLayoutImpl = opaque {};

pub const Buffer = ?*BufferImpl;
pub const BufferImpl = opaque {};

pub const CommandBuffer = ?*CommandBufferImpl;
pub const CommandBufferImpl = opaque {};

pub const CommandEncoder = ?*CommandEncoderImpl;
pub const CommandEncoderImpl = opaque {};

pub const ComputePassEncoder = ?*ComputePassEncoderImpl;
pub const ComputePassEncoderImpl = opaque {};

pub const ComputePipeline = ?*ComputePipelineImpl;
pub const ComputePipelineImpl = opaque {};

pub const Device = ?*DeviceImpl;
pub const DeviceImpl = opaque {};

pub const Instance = ?*InstanceImpl;
pub const InstanceImpl = opaque {};

pub const PipelineLayout = ?*PipelineLayoutImpl;
pub const PipelineLayoutImpl = opaque {};

pub const QuerySet = ?*QuerySetImpl;
pub const QuerySetImpl = opaque {};

pub const Queue = ?*QueueImpl;
pub const QueueImpl = opaque {};

pub const RenderBundle = ?*RenderBundleImpl;
pub const RenderBundleImpl = opaque {};

pub const RenderBundleEncoder = ?*RenderBundleEncoderImpl;
pub const RenderBundleEncoderImpl = opaque {};

pub const RenderPassEncoder = ?*RenderPassEncoderImpl;
pub const RenderPassEncoderImpl = opaque {};

pub const RenderPipeline = ?*RenderPipelineImpl;
pub const RenderPipelineImpl = opaque {};

pub const Sampler = ?*SamplerImpl;
pub const SamplerImpl = opaque {};

pub const ShaderModule = ?*ShaderModuleImpl;
pub const ShaderModuleImpl = opaque {};

pub const Surface = ?*SurfaceImpl;
pub const SurfaceImpl = opaque {};

pub const Texture = ?*TextureImpl;
pub const TextureImpl = opaque {};

pub const TextureView = ?*TextureViewImpl;
pub const TextureViewImpl = opaque {};

pub const ChainedStructOut = extern struct {
    next: ?*ChainedStructOut = null,
    s_type: SType = .undefined,
};

pub const AdapterInfo = extern struct {
    next_in_chain: ?*ChainedStructOut = null,
    vendor: StringView = .{},
    architecture: StringView = .{},
    device: StringView = .{},
    description: StringView = .{},
    backend_type: BackendType = .undefined,
    adapter_type: AdapterType = .undefined,
    vendor_id: u32 = 0,
    device_id: u32 = 0,
};

pub const ChainedStruct = extern struct {
    next: ?*const ChainedStruct = null,
    s_type: SType = .undefined,
};

pub const BindGroupEntry = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    binding: u32 = 0,
    buffer: Buffer = null,
    offset: u64 = 0,
    size: u64 = 0,
    sampler: Sampler = null,
    texture_view: TextureView = null,
};

pub const BlendComponent = extern struct {
    operation: BlendOperation = .undefined,
    src_factor: BlendFactor = .undefined,
    dst_factor: BlendFactor = .undefined,
};

pub const BufferBindingLayout = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    type: BufferBindingType = .binding_not_used,
    has_dynamic_offset: BigBool = .false,
    min_binding_size: u64 = 0,
};

pub const BufferDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    usage: BufferUsage = .none,
    size: u64 = 0,
    mapped_at_creation: BigBool = .false,
};

pub const Color = extern struct {
    r: f64 = 0.0,
    g: f64 = 0.0,
    b: f64 = 0.0,
    a: f64 = 0.0,
};

pub const CommandBufferDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
};

pub const CommandEncoderDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
};

pub const CompilationMessage = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    message: StringView = .{},
    type: CompilationMessageType = .undefined,
    line_num: u64 = 0,
    line_pos: u64 = 0,
    offset: u64 = 0,
    length: u64 = 0,
};

pub const ComputePassTimestampWrites = extern struct {
    query_set: QuerySet = null,
    beginning_of_pass_write_index: u32 = 0,
    end_of_pass_write_index: u32 = 0,
};

pub const ConstantEntry = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    key: StringView = .{},
    value: f64 = 0.0,
};

pub const Extent3D = extern struct {
    width: u32 = 0,
    height: u32 = 0,
    depth_or_array_layers: u32 = 0,
};

pub const Future = extern struct {
    id: u64 = 0,
};

pub const InstanceCapabilities = extern struct {
    next_in_chain: ?*ChainedStructOut = null,
    timed_wait_any_enable: BigBool = .false,
    timed_wait_any_max_count: usize = 0,
};

pub const Limits = extern struct {
    next_in_chain: ?*ChainedStructOut = null,
    max_texture_dimension1d: u32 = 0,
    max_texture_dimension2d: u32 = 0,
    max_texture_dimension3d: u32 = 0,
    max_texture_array_layers: u32 = 0,
    max_bind_groups: u32 = 0,
    max_bind_groups_plus_vertex_buffers: u32 = 0,
    max_bindings_per_bind_group: u32 = 0,
    max_dynamic_uniform_buffers_per_pipeline_layout: u32 = 0,
    max_dynamic_storage_buffers_per_pipeline_layout: u32 = 0,
    max_sampled_textures_per_shader_stage: u32 = 0,
    max_samplers_per_shader_stage: u32 = 0,
    max_storage_buffers_per_shader_stage: u32 = 0,
    max_storage_textures_per_shader_stage: u32 = 0,
    max_uniform_buffers_per_shader_stage: u32 = 0,
    max_uniform_buffer_binding_size: u64 = 0,
    max_storage_buffer_binding_size: u64 = 0,
    min_uniform_buffer_offset_alignment: u32 = 0,
    min_storage_buffer_offset_alignment: u32 = 0,
    max_vertex_buffers: u32 = 0,
    max_buffer_size: u64 = 0,
    max_vertex_attributes: u32 = 0,
    max_vertex_buffer_array_stride: u32 = 0,
    max_inter_stage_shader_variables: u32 = 0,
    max_color_attachments: u32 = 0,
    max_color_attachment_bytes_per_sample: u32 = 0,
    max_compute_workgroup_storage_size: u32 = 0,
    max_compute_invocations_per_workgroup: u32 = 0,
    max_compute_workgroup_size_x: u32 = 0,
    max_compute_workgroup_size_y: u32 = 0,
    max_compute_workgroup_size_z: u32 = 0,
    max_compute_workgroups_per_dimension: u32 = 0,
};

pub const MultisampleState = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    count: u32 = 0,
    mask: u32 = 0,
    alpha_to_coverage_enabled: BigBool = .false,
};

pub const Origin3D = extern struct {
    x: u32 = 0,
    y: u32 = 0,
    z: u32 = 0,
};

pub const PipelineLayoutDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    bind_group_layout_count: usize = 0,
    bind_group_layouts: ?[*]const BindGroupLayout = null,
};

pub const PrimitiveState = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    topology: PrimitiveTopology = .undefined,
    strip_index_format: IndexFormat = .undefined,
    front_face: FrontFace = .undefined,
    cull_mode: CullMode = .undefined,
    unclipped_depth: BigBool = .false,
};

pub const QuerySetDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    type: QueryType = .undefined,
    count: u32 = 0,
};

pub const QueueDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
};

pub const RenderBundleDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
};

pub const RenderBundleEncoderDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    color_format_count: usize = 0,
    color_formats: ?[*]const TextureFormat = null,
    depth_stencil_format: TextureFormat = .undefined,
    sample_count: u32 = 0,
    depth_read_only: BigBool = .false,
    stencil_read_only: BigBool = .false,
};

pub const RenderPassDepthStencilAttachment = extern struct {
    view: TextureView = null,
    depth_load_op: LoadOp = .undefined,
    depth_store_op: StoreOp = .undefined,
    depth_clear_value: f32 = 0.0,
    depth_read_only: BigBool = .false,
    stencil_load_op: LoadOp = .undefined,
    stencil_store_op: StoreOp = .undefined,
    stencil_clear_value: u32 = 0,
    stencil_read_only: BigBool = .false,
};

pub const RenderPassMaxDrawCount = extern struct {
    chain: ChainedStruct = .{},
    max_draw_count: u64 = 0,
};

pub const RenderPassTimestampWrites = extern struct {
    query_set: QuerySet = null,
    beginning_of_pass_write_index: u32 = 0,
    end_of_pass_write_index: u32 = 0,
};

pub const RequestAdapterOptions = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    feature_level: FeatureLevel = .undefined,
    power_preference: PowerPreference = .undefined,
    force_fallback_adapter: BigBool = .false,
    backend_type: BackendType = .undefined,
    compatible_surface: Surface = null,
};

pub const SamplerBindingLayout = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    type: SamplerBindingType = .binding_not_used,
};

pub const SamplerDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    address_mode_u: AddressMode = .undefined,
    address_mode_v: AddressMode = .undefined,
    address_mode_w: AddressMode = .undefined,
    mag_filter: FilterMode = .undefined,
    min_filter: FilterMode = .undefined,
    mipmap_filter: MipmapFilterMode = .undefined,
    lod_min_clamp: f32 = 0.0,
    lod_max_clamp: f32 = 0.0,
    compare: CompareFunction = .undefined,
    max_anisotropy: u16 = 0,
};

pub const ShaderModuleDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
};

pub const ShaderSourceSPIRV = extern struct {
    chain: ChainedStruct = .{},
    code_size: u32 = 0,
    code: ?[*]const u32 = null,
};

pub const ShaderSourceWGSL = extern struct {
    chain: ChainedStruct = .{},
    code: StringView = .{},
};

pub const StencilFaceState = extern struct {
    compare: CompareFunction = .undefined,
    fail_op: StencilOperation = .undefined,
    depth_fail_op: StencilOperation = .undefined,
    pass_op: StencilOperation = .undefined,
};

pub const StorageTextureBindingLayout = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    access: StorageTextureAccess = .binding_not_used,
    format: TextureFormat = .undefined,
    view_dimension: TextureViewDimension = .undefined,
};

pub const SupportedFeatures = extern struct {
    feature_count: usize = 0,
    features: ?[*]const FeatureName = null,
};

pub const SupportedWGSLLanguageFeatures = extern struct {
    feature_count: usize = 0,
    features: ?[*]const WGSLLanguageFeatureName = null,
};

pub const SurfaceCapabilities = extern struct {
    next_in_chain: ?*ChainedStructOut = null,
    usages: TextureUsage = .none,
    format_count: usize = 0,
    formats: ?[*]const TextureFormat = null,
    present_mode_count: usize = 0,
    present_modes: ?[*]const PresentMode = null,
    alpha_mode_count: usize = 0,
    alpha_modes: ?[*]const CompositeAlphaMode = null,
};

pub const SurfaceConfiguration = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    device: Device = null,
    format: TextureFormat = .undefined,
    usage: TextureUsage = .none,
    width: u32 = 0,
    height: u32 = 0,
    view_format_count: usize = 0,
    view_formats: ?[*]const TextureFormat = null,
    alpha_mode: CompositeAlphaMode = .auto,
    present_mode: PresentMode = .undefined,
};

pub const SurfaceDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
};

pub const SurfaceSourceAndroidNativeWindow = extern struct {
    chain: ChainedStruct = .{},
    window: ?*anyopaque = null,
};

pub const SurfaceSourceMetalLayer = extern struct {
    chain: ChainedStruct = .{},
    layer: ?*anyopaque = null,
};

pub const SurfaceSourceWaylandSurface = extern struct {
    chain: ChainedStruct = .{},
    display: ?*anyopaque = null,
    surface: ?*anyopaque = null,
};

pub const SurfaceSourceWindowsHWND = extern struct {
    chain: ChainedStruct = .{},
    hinstance: ?*anyopaque = null,
    hwnd: ?*anyopaque = null,
};

pub const SurfaceSourceXCBWindow = extern struct {
    chain: ChainedStruct = .{},
    connection: ?*anyopaque = null,
    window: u32 = 0,
};

pub const SurfaceSourceXlibWindow = extern struct {
    chain: ChainedStruct = .{},
    display: ?*anyopaque = null,
    window: u64 = 0,
};

pub const SurfaceTexture = extern struct {
    next_in_chain: ?*ChainedStructOut = null,
    texture: Texture = null,
    status: SurfaceGetCurrentTextureStatus = .undefined,
};

pub const TexelCopyBufferLayout = extern struct {
    offset: u64 = 0,
    bytes_per_row: u32 = 0,
    rows_per_image: u32 = 0,
};

pub const TextureBindingLayout = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    sample_type: TextureSampleType = .binding_not_used,
    view_dimension: TextureViewDimension = .undefined,
    multisampled: BigBool = .false,
};

pub const TextureViewDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    format: TextureFormat = .undefined,
    dimension: TextureViewDimension = .undefined,
    base_mip_level: u32 = 0,
    mip_level_count: u32 = 0,
    base_array_layer: u32 = 0,
    array_layer_count: u32 = 0,
    aspect: TextureAspect = .undefined,
    usage: TextureUsage = .none,
};

pub const VertexAttribute = extern struct {
    format: VertexFormat = .undefined,
    offset: u64 = 0,
    shaderLocation: u32 = 0,
};

pub const BindGroupDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    layout: BindGroupLayout = null,
    entry_count: usize = 0,
    entries: ?[*]const BindGroupEntry = null,
};

pub const BindGroupLayoutEntry = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    binding: u32 = 0,
    visibility: ShaderStage = .none,
    buffer: BufferBindingLayout = .{},
    sampler: SamplerBindingLayout = .{},
    texture: TextureBindingLayout = .{},
    storage_texture: StorageTextureBindingLayout = .{},
};

pub const BlendState = extern struct {
    color: BlendComponent = .{},
    alpha: BlendComponent = .{},
};

pub const CompilationInfo = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    message_count: usize = 0,
    messages: ?[*]const CompilationMessage = null,
};

pub const ComputePassDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    timestamp_writes: ?*const ComputePassTimestampWrites = null,
};

pub const DepthStencilState = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    format: TextureFormat = .undefined,
    depth_write_enabled: OptionalBool = .false,
    depth_compare: CompareFunction = .undefined,
    stencil_front: StencilFaceState = .{},
    stencil_back: StencilFaceState = .{},
    stencil_read_mask: u32 = 0,
    stencil_write_mask: u32 = 0,
    depth_bias: i32 = 0,
    depth_bias_slope_scale: f32 = 0.0,
    depth_bias_clamp: f32 = 0.0,
};

pub const DeviceLostCallbackInfo = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: DeviceLostCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const UncapturedErrorCallbackInfo = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    callback: UncapturedErrorCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const DeviceDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    required_feature_count: usize = 0,
    required_features: ?[*]const FeatureName = null,
    required_limits: ?[*]const Limits = null,
    default_queue: QueueDescriptor = .{},
    device_lost_callback_info: DeviceLostCallbackInfo = .{},
    uncaptured_error_callback_info: UncapturedErrorCallbackInfo = .{},
};

pub const FutureWaitInfo = extern struct {
    future: Future = .{},
    completed: BigBool = .false,
};

pub const InstanceDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    features: InstanceCapabilities = .{},
};

pub const ProgrammableStageDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    module: ShaderModule = null,
    entry_point: StringView = .{},
    constant_count: usize = 0,
    constants: ?[*]const ConstantEntry = null,
};

pub const RenderPassColorAttachment = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    view: TextureView = null,
    depth_slice: u32 = 0,
    resolve_target: TextureView = null,
    load_op: LoadOp = .undefined,
    store_op: StoreOp = .undefined,
    clear_value: Color = .{},
};

pub const TexelCopyBufferInfo = extern struct {
    layout: TexelCopyBufferLayout = .{},
    buffer: Buffer = null,
};

pub const TexelCopyTextureInfo = extern struct {
    texture: Texture = null,
    mip_level: u32 = 0,
    origin: Origin3D = .{},
    aspect: TextureAspect = .undefined,
};

pub const TextureDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    usage: TextureUsage = .none,
    dimension: TextureDimension = .undefined,
    size: Extent3D = .{},
    format: TextureFormat = .undefined,
    mip_level_count: u32 = 0,
    sample_count: u32 = 0,
    view_format_count: usize = 0,
    view_formats: ?[*]const TextureFormat = null,
};

pub const VertexBufferLayout = extern struct {
    step_mode: VertexStepMode = .vertex_buffer_not_used,
    array_stride: u64 = 0,
    attribute_count: usize = 0,
    attributes: ?[*]const VertexAttribute = null,
};

pub const BindGroupLayoutDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    entry_count: usize = 0,
    entries: ?[*]const BindGroupLayoutEntry = null,
};

pub const ColorTargetState = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    format: TextureFormat = .undefined,
    blend: ?*const BlendState = null,
    write_mask: ColorWriteMask = .none,
};

pub const ComputePipelineDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    layout: PipelineLayout = null,
    compute: ProgrammableStageDescriptor = .{},
};

pub const RenderPassDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    color_attachment_count: usize = 0,
    color_attachments: ?[*]const RenderPassColorAttachment = null,
    depth_stencil_attachment: ?*const RenderPassDepthStencilAttachment = null,
    occlusion_query_set: QuerySet = null,
    timestamp_writes: ?*const RenderPassTimestampWrites = null,
};

pub const VertexState = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    module: ShaderModule = null,
    entry_point: StringView = .{},
    constant_count: usize = 0,
    constants: ?[*]const ConstantEntry = null,
    buffer_count: usize = 0,
    buffers: ?[*]const VertexBufferLayout = null,
};

pub const FragmentState = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    module: ShaderModule = null,
    entry_point: StringView = .{},
    constant_count: usize = 0,
    constants: ?[*]const ConstantEntry = null,
    target_count: usize = 0,
    targets: ?[*]const ColorTargetState = null,
};

pub const RenderPipelineDescriptor = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    label: StringView = .{},
    layout: PipelineLayout = null,
    vertex: VertexState = .{},
    primitive: PrimitiveState = .{},
    depth_stencil: ?*const DepthStencilState = null,
    multisample: MultisampleState = .{},
    fragment: ?*const FragmentState = null,
};

pub const BufferMapCallbackInfo = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: BufferMapCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const CompilationInfoCallbackInfo = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: CompilationInfoCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const CreateComputePipelineAsyncCallbackInfo = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: CreateComputePipelineAsyncCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const CreateRenderPipelineAsyncCallbackInfo = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: CreateRenderPipelineAsyncCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const PopErrorScopeCallbackInfo = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: PopErrorScopeCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const QueueWorkDoneCallbackInfo = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: QueueWorkDoneCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const RequestAdapterCallbackInfo = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: RequestAdapterCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const RequestDeviceCallbackInfo = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: RequestDeviceCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const Proc = ?*const fn () callconv(.c) void;

pub const InstanceExtras = extern struct {
    chain: ChainedStruct = .{},
    backends: InstanceBackend = .all,
    flags: InstanceFlag = .none,
    dx12_shader_compiler: Dx12Compiler = .undefined,
    gles3_minor_version: Gles3MinorVersion = .automatic,
    gl_fence_behaviour: GLFenceBehaviour = .normal,
    dxil_path: StringView = .{},
    dxc_path: StringView = .{},
    dxc_max_shader_model: DxcMaxShaderModel = .v6_0,
};

pub const DeviceExtras = extern struct {
    chain: ChainedStruct = .{},
    trace_path: StringView = .{},
};

pub const NativeLimits = extern struct {
    chain: ChainedStructOut = .{},
    max_push_constant_size: u32 = 0,
    max_non_sampler_bindings: u32 = 0,
};

pub const PushConstantRange = extern struct {
    stages: ShaderStage = .none,
    start: u32 = 0,
    end: u32 = 0,
};

pub const PipelineLayoutExtras = extern struct {
    chain: ChainedStruct = .{},
    push_constant_range_count: usize = 0,
    push_constant_ranges: ?[*]const PushConstantRange = null,
};

pub const SubmissionIndex = u64;
pub const ShaderDefine = extern struct {
    name: StringView = .{},
    value: StringView = .{},
};

pub const ShaderSourceGLSL = extern struct {
    chain: ChainedStruct = .{},
    stage: ShaderStage = .none,
    code: StringView = .{},
    define_count: u32 = 0,
    defines: ?[*]ShaderDefine = null,
};

pub const ShaderModuleDescriptorSpirV = extern struct {
    label: StringView = .{},
    source_size: u32 = 0,
    source: ?*const u32 = null,
};

pub const RegistryReport = extern struct {
    num_allocated: usize = 0,
    num_kept_from_user: usize = 0,
    num_released_from_user: usize = 0,
    element_size: usize = 0,
};

pub const HubReport = extern struct {
    adapters: RegistryReport = .{},
    devices: RegistryReport = .{},
    queues: RegistryReport = .{},
    pipeline_layouts: RegistryReport = .{},
    shader_modules: RegistryReport = .{},
    bind_group_layouts: RegistryReport = .{},
    bind_groups: RegistryReport = .{},
    command_buffers: RegistryReport = .{},
    render_bundles: RegistryReport = .{},
    render_pipelines: RegistryReport = .{},
    compute_pipelines: RegistryReport = .{},
    pipeline_caches: RegistryReport = .{},
    query_sets: RegistryReport = .{},
    buffers: RegistryReport = .{},
    textures: RegistryReport = .{},
    texture_views: RegistryReport = .{},
    samplers: RegistryReport = .{},
};

pub const GlobalReport = extern struct {
    surfaces: RegistryReport = .{},
    hub: HubReport = .{},
};

pub const InstanceEnumerateAdapterOptions = extern struct {
    next_in_chain: ?*const ChainedStruct = null,
    backends: InstanceBackend = .all,
};

pub const BindGroupEntryExtras = extern struct {
    chain: ChainedStruct = .{},
    buffers: ?[*]const Buffer = null,
    buffer_count: usize = 0,
    samplers: ?[*]const Sampler = null,
    sampler_count: usize = 0,
    texture_views: ?[*]const TextureView = null,
    texture_view_count: usize = 0,
};

pub const BindGroupLayoutEntryExtras = extern struct {
    chain: ChainedStruct = .{},
    count: u32 = 0,
};

pub const QuerySetDescriptorExtras = extern struct {
    chain: ChainedStruct = .{},
    pipeline_statistics: ?[*]const PipelineStatisticName = null,
    pipeline_statistic_count: usize = 0,
};

pub const SurfaceConfigurationExtras = extern struct {
    chain: ChainedStruct = .{},
    desired_maximum_frame_latency: u32 = 0,
};

pub const SurfaceSourceSwapChainPanel = extern struct {
    chain: ChainedStruct = .{},
    panel_native: ?*anyopaque = null,
};

pub const DeviceLostCallback = ?*const fn (Device, DeviceLostReason, StringView, ?*anyopaque, ?*anyopaque) callconv(.c) void;
pub const UncapturedErrorCallback = ?*const fn (Device, ErrorType, StringView, ?*anyopaque, ?*anyopaque) callconv(.c) void;
pub const BufferMapCallback = ?*const fn (MapAsyncStatus, StringView, ?*anyopaque, ?*anyopaque) callconv(.c) void;
pub const CompilationInfoCallback = ?*const fn (CompilationInfoRequestStatus, *const CompilationInfo, ?*anyopaque, ?*anyopaque) callconv(.c) void;
pub const CreateComputePipelineAsyncCallback = ?*const fn (CreatePipelineAsyncStatus, ComputePipeline, StringView, ?*anyopaque, ?*anyopaque) callconv(.c) void;
pub const CreateRenderPipelineAsyncCallback = ?*const fn (CreatePipelineAsyncStatus, RenderPipeline, StringView, ?*anyopaque, ?*anyopaque) callconv(.c) void;
pub const PopErrorScopeCallback = ?*const fn (PopErrorScopeStatus, ErrorType, StringView, ?*anyopaque, ?*anyopaque) callconv(.c) void;
pub const QueueWorkDoneCallback = ?*const fn (QueueWorkDoneStatus, ?*anyopaque, ?*anyopaque) callconv(.c) void;
pub const RequestAdapterCallback = ?*const fn (RequestAdapterStatus, Adapter, StringView, ?*anyopaque, ?*anyopaque) callconv(.c) void;
pub const RequestDeviceCallback = ?*const fn (RequestDeviceStatus, Device, StringView, ?*anyopaque, ?*anyopaque) callconv(.c) void;
pub const LogCallback = ?*const fn (LogLevel, StringView, ?*anyopaque) callconv(.c) void;

/// `WGPUInstance wgpuCreateInstance(WGPU_NULLABLE WGPUInstanceDescriptor const * descriptor)`
pub const createInstance = @extern(*const fn (?*const InstanceDescriptor) callconv(.c) Instance, .{ .name = "wgpuCreateInstance" });
/// `WGPUStatus wgpuGetInstanceCapabilities(WGPUInstanceCapabilities * capabilities)`
pub const getInstanceCapabilities = @extern(*const fn (*InstanceCapabilities) callconv(.c) Status, .{ .name = "wgpuGetInstanceCapabilities" });
/// `WGPUProc wgpuGetProcAddress(WGPUStringView procName)`
pub const getProcAddress = @extern(*const fn (StringView) callconv(.c) Proc, .{ .name = "wgpuGetProcAddress" });
/// `void wgpuAdapterGetFeatures(WGPUAdapter adapter, WGPUSupportedFeatures * features)`
pub const adapterGetFeatures = @extern(*const fn (Adapter, *SupportedFeatures) callconv(.c) void, .{ .name = "wgpuAdapterGetFeatures" });
/// `WGPUStatus wgpuAdapterGetInfo(WGPUAdapter adapter, WGPUAdapterInfo * info)`
pub const adapterGetInfo = @extern(*const fn (Adapter, *AdapterInfo) callconv(.c) Status, .{ .name = "wgpuAdapterGetInfo" });
/// `WGPUStatus wgpuAdapterGetLimits(WGPUAdapter adapter, WGPULimits * limits)`
pub const adapterGetLimits = @extern(*const fn (Adapter, *Limits) callconv(.c) Status, .{ .name = "wgpuAdapterGetLimits" });
/// `WGPUBool wgpuAdapterHasFeature(WGPUAdapter adapter, WGPUFeatureName feature)`
pub const adapterHasFeature = @extern(*const fn (Adapter, FeatureName) callconv(.c) BigBool, .{ .name = "wgpuAdapterHasFeature" });
/// `WGPUFuture wgpuAdapterRequestDevice(WGPUAdapter adapter, WGPU_NULLABLE WGPUDeviceDescriptor const * descriptor, WGPURequestDeviceCallbackInfo callbackInfo)`
pub const adapterRequestDevice = @extern(*const fn (Adapter, ?*const DeviceDescriptor, RequestDeviceCallbackInfo) callconv(.c) Future, .{ .name = "wgpuAdapterRequestDevice" });
/// `void wgpuAdapterAddRef(WGPUAdapter adapter)`
pub const adapterAddRef = @extern(*const fn (Adapter) callconv(.c) void, .{ .name = "wgpuAdapterAddRef" });
/// `void wgpuAdapterRelease(WGPUAdapter adapter)`
pub const adapterRelease = @extern(*const fn (Adapter) callconv(.c) void, .{ .name = "wgpuAdapterRelease" });
/// `void wgpuAdapterInfoFreeMembers(WGPUAdapterInfo adapterInfo)`
pub const adapterInfoFreeMembers = @extern(*const fn (AdapterInfo) callconv(.c) void, .{ .name = "wgpuAdapterInfoFreeMembers" });
/// `void wgpuBindGroupSetLabel(WGPUBindGroup bindGroup, WGPUStringView label)`
pub const bindGroupSetLabel = @extern(*const fn (BindGroup, StringView) callconv(.c) void, .{ .name = "wgpuBindGroupSetLabel" });
/// `void wgpuBindGroupAddRef(WGPUBindGroup bindGroup)`
pub const bindGroupAddRef = @extern(*const fn (BindGroup) callconv(.c) void, .{ .name = "wgpuBindGroupAddRef" });
/// `void wgpuBindGroupRelease(WGPUBindGroup bindGroup)`
pub const bindGroupRelease = @extern(*const fn (BindGroup) callconv(.c) void, .{ .name = "wgpuBindGroupRelease" });
/// `void wgpuBindGroupLayoutSetLabel(WGPUBindGroupLayout bindGroupLayout, WGPUStringView label)`
pub const bindGroupLayoutSetLabel = @extern(*const fn (BindGroupLayout, StringView) callconv(.c) void, .{ .name = "wgpuBindGroupLayoutSetLabel" });
/// `void wgpuBindGroupLayoutAddRef(WGPUBindGroupLayout bindGroupLayout)`
pub const bindGroupLayoutAddRef = @extern(*const fn (BindGroupLayout) callconv(.c) void, .{ .name = "wgpuBindGroupLayoutAddRef" });
/// `void wgpuBindGroupLayoutRelease(WGPUBindGroupLayout bindGroupLayout)`
pub const bindGroupLayoutRelease = @extern(*const fn (BindGroupLayout) callconv(.c) void, .{ .name = "wgpuBindGroupLayoutRelease" });
/// `void wgpuBufferDestroy(WGPUBuffer buffer)`
pub const bufferDestroy = @extern(*const fn (Buffer) callconv(.c) void, .{ .name = "wgpuBufferDestroy" });
/// `void const * wgpuBufferGetConstMappedRange(WGPUBuffer buffer, size_t offset, size_t size)`
pub const bufferGetConstMappedRange = @extern(*const fn (Buffer, usize, usize) callconv(.c) ?*const anyopaque, .{ .name = "wgpuBufferGetConstMappedRange" });
/// `WGPUBufferMapState wgpuBufferGetMapState(WGPUBuffer buffer)`
pub const bufferGetMapState = @extern(*const fn (Buffer) callconv(.c) BufferMapState, .{ .name = "wgpuBufferGetMapState" });
/// `void * wgpuBufferGetMappedRange(WGPUBuffer buffer, size_t offset, size_t size)`
pub const bufferGetMappedRange = @extern(*const fn (Buffer, usize, usize) callconv(.c) ?*anyopaque, .{ .name = "wgpuBufferGetMappedRange" });
/// `uint64_t wgpuBufferGetSize(WGPUBuffer buffer)`
pub const bufferGetSize = @extern(*const fn (Buffer) callconv(.c) u64, .{ .name = "wgpuBufferGetSize" });
/// `WGPUBufferUsage wgpuBufferGetUsage(WGPUBuffer buffer)`
pub const bufferGetUsage = @extern(*const fn (Buffer) callconv(.c) BufferUsage, .{ .name = "wgpuBufferGetUsage" });
/// `WGPUFuture wgpuBufferMapAsync(WGPUBuffer buffer, WGPUMapMode mode, size_t offset, size_t size, WGPUBufferMapCallbackInfo callbackInfo)`
pub const bufferMapAsync = @extern(*const fn (Buffer, MapMode, usize, usize, BufferMapCallbackInfo) callconv(.c) Future, .{ .name = "wgpuBufferMapAsync" });
/// `void wgpuBufferSetLabel(WGPUBuffer buffer, WGPUStringView label)`
pub const bufferSetLabel = @extern(*const fn (Buffer, StringView) callconv(.c) void, .{ .name = "wgpuBufferSetLabel" });
/// `void wgpuBufferUnmap(WGPUBuffer buffer)`
pub const bufferUnmap = @extern(*const fn (Buffer) callconv(.c) void, .{ .name = "wgpuBufferUnmap" });
/// `void wgpuBufferAddRef(WGPUBuffer buffer)`
pub const bufferAddRef = @extern(*const fn (Buffer) callconv(.c) void, .{ .name = "wgpuBufferAddRef" });
/// `void wgpuBufferRelease(WGPUBuffer buffer)`
pub const bufferRelease = @extern(*const fn (Buffer) callconv(.c) void, .{ .name = "wgpuBufferRelease" });
/// `void wgpuCommandBufferSetLabel(WGPUCommandBuffer commandBuffer, WGPUStringView label)`
pub const commandBufferSetLabel = @extern(*const fn (CommandBuffer, StringView) callconv(.c) void, .{ .name = "wgpuCommandBufferSetLabel" });
/// `void wgpuCommandBufferAddRef(WGPUCommandBuffer commandBuffer)`
pub const commandBufferAddRef = @extern(*const fn (CommandBuffer) callconv(.c) void, .{ .name = "wgpuCommandBufferAddRef" });
/// `void wgpuCommandBufferRelease(WGPUCommandBuffer commandBuffer)`
pub const commandBufferRelease = @extern(*const fn (CommandBuffer) callconv(.c) void, .{ .name = "wgpuCommandBufferRelease" });
/// `WGPUComputePassEncoder wgpuCommandEncoderBeginComputePass(WGPUCommandEncoder commandEncoder, WGPU_NULLABLE WGPUComputePassDescriptor const * descriptor)`
pub const commandEncoderBeginComputePass = @extern(*const fn (CommandEncoder, ?*const ComputePassDescriptor) callconv(.c) ComputePassEncoder, .{ .name = "wgpuCommandEncoderBeginComputePass" });
/// `WGPURenderPassEncoder wgpuCommandEncoderBeginRenderPass(WGPUCommandEncoder commandEncoder, WGPURenderPassDescriptor const * descriptor)`
pub const commandEncoderBeginRenderPass = @extern(*const fn (CommandEncoder, *const RenderPassDescriptor) callconv(.c) RenderPassEncoder, .{ .name = "wgpuCommandEncoderBeginRenderPass" });
/// `void wgpuCommandEncoderClearBuffer(WGPUCommandEncoder commandEncoder, WGPUBuffer buffer, uint64_t offset, uint64_t size)`
pub const commandEncoderClearBuffer = @extern(*const fn (CommandEncoder, Buffer, u64, u64) callconv(.c) void, .{ .name = "wgpuCommandEncoderClearBuffer" });
/// `void wgpuCommandEncoderCopyBufferToBuffer(WGPUCommandEncoder commandEncoder, WGPUBuffer source, uint64_t sourceOffset, WGPUBuffer destination, uint64_t destinationOffset, uint64_t size)`
pub const commandEncoderCopyBufferToBuffer = @extern(*const fn (CommandEncoder, Buffer, u64, Buffer, u64, u64) callconv(.c) void, .{ .name = "wgpuCommandEncoderCopyBufferToBuffer" });
/// `void wgpuCommandEncoderCopyBufferToTexture(WGPUCommandEncoder commandEncoder, WGPUTexelCopyBufferInfo const * source, WGPUTexelCopyTextureInfo const * destination, WGPUExtent3D const * copySize)`
pub const commandEncoderCopyBufferToTexture = @extern(*const fn (CommandEncoder, *const TexelCopyBufferInfo, *const TexelCopyTextureInfo, *const Extent3D) callconv(.c) void, .{ .name = "wgpuCommandEncoderCopyBufferToTexture" });
/// `void wgpuCommandEncoderCopyTextureToBuffer(WGPUCommandEncoder commandEncoder, WGPUTexelCopyTextureInfo const * source, WGPUTexelCopyBufferInfo const * destination, WGPUExtent3D const * copySize)`
pub const commandEncoderCopyTextureToBuffer = @extern(*const fn (CommandEncoder, *const TexelCopyTextureInfo, *const TexelCopyBufferInfo, *const Extent3D) callconv(.c) void, .{ .name = "wgpuCommandEncoderCopyTextureToBuffer" });
/// `void wgpuCommandEncoderCopyTextureToTexture(WGPUCommandEncoder commandEncoder, WGPUTexelCopyTextureInfo const * source, WGPUTexelCopyTextureInfo const * destination, WGPUExtent3D const * copySize)`
pub const commandEncoderCopyTextureToTexture = @extern(*const fn (CommandEncoder, *const TexelCopyTextureInfo, *const TexelCopyTextureInfo, *const Extent3D) callconv(.c) void, .{ .name = "wgpuCommandEncoderCopyTextureToTexture" });
/// `WGPUCommandBuffer wgpuCommandEncoderFinish(WGPUCommandEncoder commandEncoder, WGPU_NULLABLE WGPUCommandBufferDescriptor const * descriptor)`
pub const commandEncoderFinish = @extern(*const fn (CommandEncoder, ?*const CommandBufferDescriptor) callconv(.c) CommandBuffer, .{ .name = "wgpuCommandEncoderFinish" });
/// `void wgpuCommandEncoderInsertDebugMarker(WGPUCommandEncoder commandEncoder, WGPUStringView markerLabel)`
pub const commandEncoderInsertDebugMarker = @extern(*const fn (CommandEncoder, StringView) callconv(.c) void, .{ .name = "wgpuCommandEncoderInsertDebugMarker" });
/// `void wgpuCommandEncoderPopDebugGroup(WGPUCommandEncoder commandEncoder)`
pub const commandEncoderPopDebugGroup = @extern(*const fn (CommandEncoder) callconv(.c) void, .{ .name = "wgpuCommandEncoderPopDebugGroup" });
/// `void wgpuCommandEncoderPushDebugGroup(WGPUCommandEncoder commandEncoder, WGPUStringView groupLabel)`
pub const commandEncoderPushDebugGroup = @extern(*const fn (CommandEncoder, StringView) callconv(.c) void, .{ .name = "wgpuCommandEncoderPushDebugGroup" });
/// `void wgpuCommandEncoderResolveQuerySet(WGPUCommandEncoder commandEncoder, WGPUQuerySet querySet, uint32_t firstQuery, uint32_t queryCount, WGPUBuffer destination, uint64_t destinationOffset)`
pub const commandEncoderResolveQuerySet = @extern(*const fn (CommandEncoder, QuerySet, u32, u32, Buffer, u64) callconv(.c) void, .{ .name = "wgpuCommandEncoderResolveQuerySet" });
/// `void wgpuCommandEncoderSetLabel(WGPUCommandEncoder commandEncoder, WGPUStringView label)`
pub const commandEncoderSetLabel = @extern(*const fn (CommandEncoder, StringView) callconv(.c) void, .{ .name = "wgpuCommandEncoderSetLabel" });
/// `void wgpuCommandEncoderWriteTimestamp(WGPUCommandEncoder commandEncoder, WGPUQuerySet querySet, uint32_t queryIndex)`
pub const commandEncoderWriteTimestamp = @extern(*const fn (CommandEncoder, QuerySet, u32) callconv(.c) void, .{ .name = "wgpuCommandEncoderWriteTimestamp" });
/// `void wgpuCommandEncoderAddRef(WGPUCommandEncoder commandEncoder)`
pub const commandEncoderAddRef = @extern(*const fn (CommandEncoder) callconv(.c) void, .{ .name = "wgpuCommandEncoderAddRef" });
/// `void wgpuCommandEncoderRelease(WGPUCommandEncoder commandEncoder)`
pub const commandEncoderRelease = @extern(*const fn (CommandEncoder) callconv(.c) void, .{ .name = "wgpuCommandEncoderRelease" });
/// `void wgpuComputePassEncoderDispatchWorkgroups(WGPUComputePassEncoder computePassEncoder, uint32_t workgroupCountX, uint32_t workgroupCountY, uint32_t workgroupCountZ)`
pub const computePassEncoderDispatchWorkgroups = @extern(*const fn (ComputePassEncoder, u32, u32, u32) callconv(.c) void, .{ .name = "wgpuComputePassEncoderDispatchWorkgroups" });
/// `void wgpuComputePassEncoderDispatchWorkgroupsIndirect(WGPUComputePassEncoder computePassEncoder, WGPUBuffer indirectBuffer, uint64_t indirectOffset)`
pub const computePassEncoderDispatchWorkgroupsIndirect = @extern(*const fn (ComputePassEncoder, Buffer, u64) callconv(.c) void, .{ .name = "wgpuComputePassEncoderDispatchWorkgroupsIndirect" });
/// `void wgpuComputePassEncoderEnd(WGPUComputePassEncoder computePassEncoder)`
pub const computePassEncoderEnd = @extern(*const fn (ComputePassEncoder) callconv(.c) void, .{ .name = "wgpuComputePassEncoderEnd" });
/// `void wgpuComputePassEncoderInsertDebugMarker(WGPUComputePassEncoder computePassEncoder, WGPUStringView markerLabel)`
pub const computePassEncoderInsertDebugMarker = @extern(*const fn (ComputePassEncoder, StringView) callconv(.c) void, .{ .name = "wgpuComputePassEncoderInsertDebugMarker" });
/// `void wgpuComputePassEncoderPopDebugGroup(WGPUComputePassEncoder computePassEncoder)`
pub const computePassEncoderPopDebugGroup = @extern(*const fn (ComputePassEncoder) callconv(.c) void, .{ .name = "wgpuComputePassEncoderPopDebugGroup" });
/// `void wgpuComputePassEncoderPushDebugGroup(WGPUComputePassEncoder computePassEncoder, WGPUStringView groupLabel)`
pub const computePassEncoderPushDebugGroup = @extern(*const fn (ComputePassEncoder, StringView) callconv(.c) void, .{ .name = "wgpuComputePassEncoderPushDebugGroup" });
/// `void wgpuComputePassEncoderSetBindGroup(WGPUComputePassEncoder computePassEncoder, uint32_t groupIndex, WGPU_NULLABLE WGPUBindGroup group, size_t dynamicOffsetCount, uint32_t const * dynamicOffsets)`
pub const computePassEncoderSetBindGroup = @extern(*const fn (ComputePassEncoder, u32, BindGroup, usize, ?[*]const u32) callconv(.c) void, .{ .name = "wgpuComputePassEncoderSetBindGroup" });
/// `void wgpuComputePassEncoderSetLabel(WGPUComputePassEncoder computePassEncoder, WGPUStringView label)`
pub const computePassEncoderSetLabel = @extern(*const fn (ComputePassEncoder, StringView) callconv(.c) void, .{ .name = "wgpuComputePassEncoderSetLabel" });
/// `void wgpuComputePassEncoderSetPipeline(WGPUComputePassEncoder computePassEncoder, WGPUComputePipeline pipeline)`
pub const computePassEncoderSetPipeline = @extern(*const fn (ComputePassEncoder, ComputePipeline) callconv(.c) void, .{ .name = "wgpuComputePassEncoderSetPipeline" });
/// `void wgpuComputePassEncoderAddRef(WGPUComputePassEncoder computePassEncoder)`
pub const computePassEncoderAddRef = @extern(*const fn (ComputePassEncoder) callconv(.c) void, .{ .name = "wgpuComputePassEncoderAddRef" });
/// `void wgpuComputePassEncoderRelease(WGPUComputePassEncoder computePassEncoder)`
pub const computePassEncoderRelease = @extern(*const fn (ComputePassEncoder) callconv(.c) void, .{ .name = "wgpuComputePassEncoderRelease" });
/// `WGPUBindGroupLayout wgpuComputePipelineGetBindGroupLayout(WGPUComputePipeline computePipeline, uint32_t groupIndex)`
pub const computePipelineGetBindGroupLayout = @extern(*const fn (ComputePipeline, u32) callconv(.c) BindGroupLayout, .{ .name = "wgpuComputePipelineGetBindGroupLayout" });
/// `void wgpuComputePipelineSetLabel(WGPUComputePipeline computePipeline, WGPUStringView label)`
pub const computePipelineSetLabel = @extern(*const fn (ComputePipeline, StringView) callconv(.c) void, .{ .name = "wgpuComputePipelineSetLabel" });
/// `void wgpuComputePipelineAddRef(WGPUComputePipeline computePipeline)`
pub const computePipelineAddRef = @extern(*const fn (ComputePipeline) callconv(.c) void, .{ .name = "wgpuComputePipelineAddRef" });
/// `void wgpuComputePipelineRelease(WGPUComputePipeline computePipeline)`
pub const computePipelineRelease = @extern(*const fn (ComputePipeline) callconv(.c) void, .{ .name = "wgpuComputePipelineRelease" });
/// `WGPUBindGroup wgpuDeviceCreateBindGroup(WGPUDevice device, WGPUBindGroupDescriptor const * descriptor)`
pub const deviceCreateBindGroup = @extern(*const fn (Device, *const BindGroupDescriptor) callconv(.c) BindGroup, .{ .name = "wgpuDeviceCreateBindGroup" });
/// `WGPUBindGroupLayout wgpuDeviceCreateBindGroupLayout(WGPUDevice device, WGPUBindGroupLayoutDescriptor const * descriptor)`
pub const deviceCreateBindGroupLayout = @extern(*const fn (Device, *const BindGroupLayoutDescriptor) callconv(.c) BindGroupLayout, .{ .name = "wgpuDeviceCreateBindGroupLayout" });
/// `WGPUBuffer wgpuDeviceCreateBuffer(WGPUDevice device, WGPUBufferDescriptor const * descriptor)`
pub const deviceCreateBuffer = @extern(*const fn (Device, *const BufferDescriptor) callconv(.c) Buffer, .{ .name = "wgpuDeviceCreateBuffer" });
/// `WGPUCommandEncoder wgpuDeviceCreateCommandEncoder(WGPUDevice device, WGPU_NULLABLE WGPUCommandEncoderDescriptor const * descriptor)`
pub const deviceCreateCommandEncoder = @extern(*const fn (Device, ?*const CommandEncoderDescriptor) callconv(.c) CommandEncoder, .{ .name = "wgpuDeviceCreateCommandEncoder" });
/// `WGPUComputePipeline wgpuDeviceCreateComputePipeline(WGPUDevice device, WGPUComputePipelineDescriptor const * descriptor)`
pub const deviceCreateComputePipeline = @extern(*const fn (Device, *const ComputePipelineDescriptor) callconv(.c) ComputePipeline, .{ .name = "wgpuDeviceCreateComputePipeline" });
/// `WGPUFuture wgpuDeviceCreateComputePipelineAsync(WGPUDevice device, WGPUComputePipelineDescriptor const * descriptor, WGPUCreateComputePipelineAsyncCallbackInfo callbackInfo)`
pub const deviceCreateComputePipelineAsync = @extern(*const fn (Device, *const ComputePipelineDescriptor, CreateComputePipelineAsyncCallbackInfo) callconv(.c) Future, .{ .name = "wgpuDeviceCreateComputePipelineAsync" });
/// `WGPUPipelineLayout wgpuDeviceCreatePipelineLayout(WGPUDevice device, WGPUPipelineLayoutDescriptor const * descriptor)`
pub const deviceCreatePipelineLayout = @extern(*const fn (Device, *const PipelineLayoutDescriptor) callconv(.c) PipelineLayout, .{ .name = "wgpuDeviceCreatePipelineLayout" });
/// `WGPUQuerySet wgpuDeviceCreateQuerySet(WGPUDevice device, WGPUQuerySetDescriptor const * descriptor)`
pub const deviceCreateQuerySet = @extern(*const fn (Device, *const QuerySetDescriptor) callconv(.c) QuerySet, .{ .name = "wgpuDeviceCreateQuerySet" });
/// `WGPURenderBundleEncoder wgpuDeviceCreateRenderBundleEncoder(WGPUDevice device, WGPURenderBundleEncoderDescriptor const * descriptor)`
pub const deviceCreateRenderBundleEncoder = @extern(*const fn (Device, *const RenderBundleEncoderDescriptor) callconv(.c) RenderBundleEncoder, .{ .name = "wgpuDeviceCreateRenderBundleEncoder" });
/// `WGPURenderPipeline wgpuDeviceCreateRenderPipeline(WGPUDevice device, WGPURenderPipelineDescriptor const * descriptor)`
pub const deviceCreateRenderPipeline = @extern(*const fn (Device, *const RenderPipelineDescriptor) callconv(.c) RenderPipeline, .{ .name = "wgpuDeviceCreateRenderPipeline" });
/// `WGPUFuture wgpuDeviceCreateRenderPipelineAsync(WGPUDevice device, WGPURenderPipelineDescriptor const * descriptor, WGPUCreateRenderPipelineAsyncCallbackInfo callbackInfo)`
pub const deviceCreateRenderPipelineAsync = @extern(*const fn (Device, *const RenderPipelineDescriptor, CreateRenderPipelineAsyncCallbackInfo) callconv(.c) Future, .{ .name = "wgpuDeviceCreateRenderPipelineAsync" });
/// `WGPUSampler wgpuDeviceCreateSampler(WGPUDevice device, WGPU_NULLABLE WGPUSamplerDescriptor const * descriptor)`
pub const deviceCreateSampler = @extern(*const fn (Device, ?*const SamplerDescriptor) callconv(.c) Sampler, .{ .name = "wgpuDeviceCreateSampler" });
/// `WGPUShaderModule wgpuDeviceCreateShaderModule(WGPUDevice device, WGPUShaderModuleDescriptor const * descriptor)`
pub const deviceCreateShaderModule = @extern(*const fn (Device, *const ShaderModuleDescriptor) callconv(.c) ShaderModule, .{ .name = "wgpuDeviceCreateShaderModule" });
/// `WGPUTexture wgpuDeviceCreateTexture(WGPUDevice device, WGPUTextureDescriptor const * descriptor)`
pub const deviceCreateTexture = @extern(*const fn (Device, *const TextureDescriptor) callconv(.c) Texture, .{ .name = "wgpuDeviceCreateTexture" });
/// `void wgpuDeviceDestroy(WGPUDevice device)`
pub const deviceDestroy = @extern(*const fn (Device) callconv(.c) void, .{ .name = "wgpuDeviceDestroy" });
/// `WGPUAdapterInfo wgpuDeviceGetAdapterInfo(WGPUDevice device)`
pub const deviceGetAdapterInfo = @extern(*const fn (Device) callconv(.c) AdapterInfo, .{ .name = "wgpuDeviceGetAdapterInfo" });
/// `void wgpuDeviceGetFeatures(WGPUDevice device, WGPUSupportedFeatures * features)`
pub const deviceGetFeatures = @extern(*const fn (Device, *SupportedFeatures) callconv(.c) void, .{ .name = "wgpuDeviceGetFeatures" });
/// `WGPUStatus wgpuDeviceGetLimits(WGPUDevice device, WGPULimits * limits)`
pub const deviceGetLimits = @extern(*const fn (Device, *Limits) callconv(.c) Status, .{ .name = "wgpuDeviceGetLimits" });
/// `WGPUFuture wgpuDeviceGetLostFuture(WGPUDevice device)`
pub const deviceGetLostFuture = @extern(*const fn (Device) callconv(.c) Future, .{ .name = "wgpuDeviceGetLostFuture" });
/// `WGPUQueue wgpuDeviceGetQueue(WGPUDevice device)`
pub const deviceGetQueue = @extern(*const fn (Device) callconv(.c) Queue, .{ .name = "wgpuDeviceGetQueue" });
/// `WGPUBool wgpuDeviceHasFeature(WGPUDevice device, WGPUFeatureName feature)`
pub const deviceHasFeature = @extern(*const fn (Device, FeatureName) callconv(.c) BigBool, .{ .name = "wgpuDeviceHasFeature" });
/// `WGPUFuture wgpuDevicePopErrorScope(WGPUDevice device, WGPUPopErrorScopeCallbackInfo callbackInfo)`
pub const devicePopErrorScope = @extern(*const fn (Device, PopErrorScopeCallbackInfo) callconv(.c) Future, .{ .name = "wgpuDevicePopErrorScope" });
/// `void wgpuDevicePushErrorScope(WGPUDevice device, WGPUErrorFilter filter)`
pub const devicePushErrorScope = @extern(*const fn (Device, ErrorFilter) callconv(.c) void, .{ .name = "wgpuDevicePushErrorScope" });
/// `void wgpuDeviceSetLabel(WGPUDevice device, WGPUStringView label)`
pub const deviceSetLabel = @extern(*const fn (Device, StringView) callconv(.c) void, .{ .name = "wgpuDeviceSetLabel" });
/// `void wgpuDeviceAddRef(WGPUDevice device)`
pub const deviceAddRef = @extern(*const fn (Device) callconv(.c) void, .{ .name = "wgpuDeviceAddRef" });
/// `void wgpuDeviceRelease(WGPUDevice device)`
pub const deviceRelease = @extern(*const fn (Device) callconv(.c) void, .{ .name = "wgpuDeviceRelease" });
/// `WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WGPUSurfaceDescriptor const * descriptor)`
pub const instanceCreateSurface = @extern(*const fn (Instance, *const SurfaceDescriptor) callconv(.c) Surface, .{ .name = "wgpuInstanceCreateSurface" });
/// `WGPUStatus wgpuInstanceGetWGSLLanguageFeatures(WGPUInstance instance, WGPUSupportedWGSLLanguageFeatures * features)`
pub const instanceGetWGSLLanguageFeatures = @extern(*const fn (Instance, *SupportedWGSLLanguageFeatures) callconv(.c) Status, .{ .name = "wgpuInstanceGetWGSLLanguageFeatures" });
/// `WGPUBool wgpuInstanceHasWGSLLanguageFeature(WGPUInstance instance, WGPUWGSLLanguageFeatureName feature)`
pub const instanceHasWGSLLanguageFeature = @extern(*const fn (Instance, WGSLLanguageFeatureName) callconv(.c) BigBool, .{ .name = "wgpuInstanceHasWGSLLanguageFeature" });
/// `void wgpuInstanceProcessEvents(WGPUInstance instance)`
pub const instanceProcessEvents = @extern(*const fn (Instance) callconv(.c) void, .{ .name = "wgpuInstanceProcessEvents" });
/// `WGPUFuture wgpuInstanceRequestAdapter(WGPUInstance instance, WGPU_NULLABLE WGPURequestAdapterOptions const * options, WGPURequestAdapterCallbackInfo callbackInfo)`
pub const instanceRequestAdapter = @extern(*const fn (Instance, ?*const RequestAdapterOptions, RequestAdapterCallbackInfo) callconv(.c) Future, .{ .name = "wgpuInstanceRequestAdapter" });
/// `WGPUWaitStatus wgpuInstanceWaitAny(WGPUInstance instance, size_t futureCount, WGPU_NULLABLE WGPUFutureWaitInfo * futures, uint64_t timeoutNS)`
pub const instanceWaitAny = @extern(*const fn (Instance, usize, [*]FutureWaitInfo, u64) callconv(.c) WaitStatus, .{ .name = "wgpuInstanceWaitAny" });
/// `void wgpuInstanceAddRef(WGPUInstance instance)`
pub const instanceAddRef = @extern(*const fn (Instance) callconv(.c) void, .{ .name = "wgpuInstanceAddRef" });
/// `void wgpuInstanceRelease(WGPUInstance instance)`
pub const instanceRelease = @extern(*const fn (Instance) callconv(.c) void, .{ .name = "wgpuInstanceRelease" });
/// `void wgpuPipelineLayoutSetLabel(WGPUPipelineLayout pipelineLayout, WGPUStringView label)`
pub const pipelineLayoutSetLabel = @extern(*const fn (PipelineLayout, StringView) callconv(.c) void, .{ .name = "wgpuPipelineLayoutSetLabel" });
/// `void wgpuPipelineLayoutAddRef(WGPUPipelineLayout pipelineLayout)`
pub const pipelineLayoutAddRef = @extern(*const fn (PipelineLayout) callconv(.c) void, .{ .name = "wgpuPipelineLayoutAddRef" });
/// `void wgpuPipelineLayoutRelease(WGPUPipelineLayout pipelineLayout)`
pub const pipelineLayoutRelease = @extern(*const fn (PipelineLayout) callconv(.c) void, .{ .name = "wgpuPipelineLayoutRelease" });
/// `void wgpuQuerySetDestroy(WGPUQuerySet querySet)`
pub const querySetDestroy = @extern(*const fn (QuerySet) callconv(.c) void, .{ .name = "wgpuQuerySetDestroy" });
/// `uint32_t wgpuQuerySetGetCount(WGPUQuerySet querySet)`
pub const querySetGetCount = @extern(*const fn (QuerySet) callconv(.c) u32, .{ .name = "wgpuQuerySetGetCount" });
/// `WGPUQueryType wgpuQuerySetGetType(WGPUQuerySet querySet)`
pub const querySetGetType = @extern(*const fn (QuerySet) callconv(.c) QueryType, .{ .name = "wgpuQuerySetGetType" });
/// `void wgpuQuerySetSetLabel(WGPUQuerySet querySet, WGPUStringView label)`
pub const querySetSetLabel = @extern(*const fn (QuerySet, StringView) callconv(.c) void, .{ .name = "wgpuQuerySetSetLabel" });
/// `void wgpuQuerySetAddRef(WGPUQuerySet querySet)`
pub const querySetAddRef = @extern(*const fn (QuerySet) callconv(.c) void, .{ .name = "wgpuQuerySetAddRef" });
/// `void wgpuQuerySetRelease(WGPUQuerySet querySet)`
pub const querySetRelease = @extern(*const fn (QuerySet) callconv(.c) void, .{ .name = "wgpuQuerySetRelease" });
/// `WGPUFuture wgpuQueueOnSubmittedWorkDone(WGPUQueue queue, WGPUQueueWorkDoneCallbackInfo callbackInfo)`
pub const queueOnSubmittedWorkDone = @extern(*const fn (Queue, QueueWorkDoneCallbackInfo) callconv(.c) Future, .{ .name = "wgpuQueueOnSubmittedWorkDone" });
/// `void wgpuQueueSetLabel(WGPUQueue queue, WGPUStringView label)`
pub const queueSetLabel = @extern(*const fn (Queue, StringView) callconv(.c) void, .{ .name = "wgpuQueueSetLabel" });
/// `void wgpuQueueSubmit(WGPUQueue queue, size_t commandCount, WGPUCommandBuffer const * commands)`
pub const queueSubmit = @extern(*const fn (Queue, usize, ?[*]const CommandBuffer) callconv(.c) void, .{ .name = "wgpuQueueSubmit" });
/// `void wgpuQueueWriteBuffer(WGPUQueue queue, WGPUBuffer buffer, uint64_t bufferOffset, void const * data, size_t size)`
pub const queueWriteBuffer = @extern(*const fn (Queue, Buffer, u64, ?*const anyopaque, usize) callconv(.c) void, .{ .name = "wgpuQueueWriteBuffer" });
/// `void wgpuQueueWriteTexture(WGPUQueue queue, WGPUTexelCopyTextureInfo const * destination, void const * data, size_t dataSize, WGPUTexelCopyBufferLayout const * dataLayout, WGPUExtent3D const * writeSize)`
pub const queueWriteTexture = @extern(*const fn (Queue, *const TexelCopyTextureInfo, ?*const anyopaque, usize, *const TexelCopyBufferLayout, *const Extent3D) callconv(.c) void, .{ .name = "wgpuQueueWriteTexture" });
/// `void wgpuQueueAddRef(WGPUQueue queue)`
pub const queueAddRef = @extern(*const fn (Queue) callconv(.c) void, .{ .name = "wgpuQueueAddRef" });
/// `void wgpuQueueRelease(WGPUQueue queue)`
pub const queueRelease = @extern(*const fn (Queue) callconv(.c) void, .{ .name = "wgpuQueueRelease" });
/// `void wgpuRenderBundleSetLabel(WGPURenderBundle renderBundle, WGPUStringView label)`
pub const renderBundleSetLabel = @extern(*const fn (RenderBundle, StringView) callconv(.c) void, .{ .name = "wgpuRenderBundleSetLabel" });
/// `void wgpuRenderBundleAddRef(WGPURenderBundle renderBundle)`
pub const renderBundleAddRef = @extern(*const fn (RenderBundle) callconv(.c) void, .{ .name = "wgpuRenderBundleAddRef" });
/// `void wgpuRenderBundleRelease(WGPURenderBundle renderBundle)`
pub const renderBundleRelease = @extern(*const fn (RenderBundle) callconv(.c) void, .{ .name = "wgpuRenderBundleRelease" });
/// `void wgpuRenderBundleEncoderDraw(WGPURenderBundleEncoder renderBundleEncoder, uint32_t vertexCount, uint32_t instanceCount, uint32_t firstVertex, uint32_t firstInstance)`
pub const renderBundleEncoderDraw = @extern(*const fn (RenderBundleEncoder, u32, u32, u32, u32) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderDraw" });
/// `void wgpuRenderBundleEncoderDrawIndexed(WGPURenderBundleEncoder renderBundleEncoder, uint32_t indexCount, uint32_t instanceCount, uint32_t firstIndex, int32_t baseVertex, uint32_t firstInstance)`
pub const renderBundleEncoderDrawIndexed = @extern(*const fn (RenderBundleEncoder, u32, u32, u32, i32, u32) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderDrawIndexed" });
/// `void wgpuRenderBundleEncoderDrawIndexedIndirect(WGPURenderBundleEncoder renderBundleEncoder, WGPUBuffer indirectBuffer, uint64_t indirectOffset)`
pub const renderBundleEncoderDrawIndexedIndirect = @extern(*const fn (RenderBundleEncoder, Buffer, u64) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderDrawIndexedIndirect" });
/// `void wgpuRenderBundleEncoderDrawIndirect(WGPURenderBundleEncoder renderBundleEncoder, WGPUBuffer indirectBuffer, uint64_t indirectOffset)`
pub const renderBundleEncoderDrawIndirect = @extern(*const fn (RenderBundleEncoder, Buffer, u64) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderDrawIndirect" });
/// `WGPURenderBundle wgpuRenderBundleEncoderFinish(WGPURenderBundleEncoder renderBundleEncoder, WGPU_NULLABLE WGPURenderBundleDescriptor const * descriptor)`
pub const renderBundleEncoderFinish = @extern(*const fn (RenderBundleEncoder, ?*const RenderBundleDescriptor) callconv(.c) RenderBundle, .{ .name = "wgpuRenderBundleEncoderFinish" });
/// `void wgpuRenderBundleEncoderInsertDebugMarker(WGPURenderBundleEncoder renderBundleEncoder, WGPUStringView markerLabel)`
pub const renderBundleEncoderInsertDebugMarker = @extern(*const fn (RenderBundleEncoder, StringView) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderInsertDebugMarker" });
/// `void wgpuRenderBundleEncoderPopDebugGroup(WGPURenderBundleEncoder renderBundleEncoder)`
pub const renderBundleEncoderPopDebugGroup = @extern(*const fn (RenderBundleEncoder) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderPopDebugGroup" });
/// `void wgpuRenderBundleEncoderPushDebugGroup(WGPURenderBundleEncoder renderBundleEncoder, WGPUStringView groupLabel)`
pub const renderBundleEncoderPushDebugGroup = @extern(*const fn (RenderBundleEncoder, StringView) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderPushDebugGroup" });
/// `void wgpuRenderBundleEncoderSetBindGroup(WGPURenderBundleEncoder renderBundleEncoder, uint32_t groupIndex, WGPU_NULLABLE WGPUBindGroup group, size_t dynamicOffsetCount, uint32_t const * dynamicOffsets)`
pub const renderBundleEncoderSetBindGroup = @extern(*const fn (RenderBundleEncoder, u32, BindGroup, usize, ?[*]const u32) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderSetBindGroup" });
/// `void wgpuRenderBundleEncoderSetIndexBuffer(WGPURenderBundleEncoder renderBundleEncoder, WGPUBuffer buffer, WGPUIndexFormat format, uint64_t offset, uint64_t size)`
pub const renderBundleEncoderSetIndexBuffer = @extern(*const fn (RenderBundleEncoder, Buffer, IndexFormat, u64, u64) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderSetIndexBuffer" });
/// `void wgpuRenderBundleEncoderSetLabel(WGPURenderBundleEncoder renderBundleEncoder, WGPUStringView label)`
pub const renderBundleEncoderSetLabel = @extern(*const fn (RenderBundleEncoder, StringView) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderSetLabel" });
/// `void wgpuRenderBundleEncoderSetPipeline(WGPURenderBundleEncoder renderBundleEncoder, WGPURenderPipeline pipeline)`
pub const renderBundleEncoderSetPipeline = @extern(*const fn (RenderBundleEncoder, RenderPipeline) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderSetPipeline" });
/// `void wgpuRenderBundleEncoderSetVertexBuffer(WGPURenderBundleEncoder renderBundleEncoder, uint32_t slot, WGPU_NULLABLE WGPUBuffer buffer, uint64_t offset, uint64_t size)`
pub const renderBundleEncoderSetVertexBuffer = @extern(*const fn (RenderBundleEncoder, u32, Buffer, u64, u64) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderSetVertexBuffer" });
/// `void wgpuRenderBundleEncoderAddRef(WGPURenderBundleEncoder renderBundleEncoder)`
pub const renderBundleEncoderAddRef = @extern(*const fn (RenderBundleEncoder) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderAddRef" });
/// `void wgpuRenderBundleEncoderRelease(WGPURenderBundleEncoder renderBundleEncoder)`
pub const renderBundleEncoderRelease = @extern(*const fn (RenderBundleEncoder) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderRelease" });
/// `void wgpuRenderPassEncoderBeginOcclusionQuery(WGPURenderPassEncoder renderPassEncoder, uint32_t queryIndex)`
pub const renderPassEncoderBeginOcclusionQuery = @extern(*const fn (RenderPassEncoder, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderBeginOcclusionQuery" });
/// `void wgpuRenderPassEncoderDraw(WGPURenderPassEncoder renderPassEncoder, uint32_t vertexCount, uint32_t instanceCount, uint32_t firstVertex, uint32_t firstInstance)`
pub const renderPassEncoderDraw = @extern(*const fn (RenderPassEncoder, u32, u32, u32, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderDraw" });
/// `void wgpuRenderPassEncoderDrawIndexed(WGPURenderPassEncoder renderPassEncoder, uint32_t indexCount, uint32_t instanceCount, uint32_t firstIndex, int32_t baseVertex, uint32_t firstInstance)`
pub const renderPassEncoderDrawIndexed = @extern(*const fn (RenderPassEncoder, u32, u32, u32, i32, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderDrawIndexed" });
/// `void wgpuRenderPassEncoderDrawIndexedIndirect(WGPURenderPassEncoder renderPassEncoder, WGPUBuffer indirectBuffer, uint64_t indirectOffset)`
pub const renderPassEncoderDrawIndexedIndirect = @extern(*const fn (RenderPassEncoder, Buffer, u64) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderDrawIndexedIndirect" });
/// `void wgpuRenderPassEncoderDrawIndirect(WGPURenderPassEncoder renderPassEncoder, WGPUBuffer indirectBuffer, uint64_t indirectOffset)`
pub const renderPassEncoderDrawIndirect = @extern(*const fn (RenderPassEncoder, Buffer, u64) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderDrawIndirect" });
/// `void wgpuRenderPassEncoderEnd(WGPURenderPassEncoder renderPassEncoder)`
pub const renderPassEncoderEnd = @extern(*const fn (RenderPassEncoder) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderEnd" });
/// `void wgpuRenderPassEncoderEndOcclusionQuery(WGPURenderPassEncoder renderPassEncoder)`
pub const renderPassEncoderEndOcclusionQuery = @extern(*const fn (RenderPassEncoder) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderEndOcclusionQuery" });
/// `void wgpuRenderPassEncoderExecuteBundles(WGPURenderPassEncoder renderPassEncoder, size_t bundleCount, WGPURenderBundle const * bundles)`
pub const renderPassEncoderExecuteBundles = @extern(*const fn (RenderPassEncoder, usize, ?[*]const RenderBundle) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderExecuteBundles" });
/// `void wgpuRenderPassEncoderInsertDebugMarker(WGPURenderPassEncoder renderPassEncoder, WGPUStringView markerLabel)`
pub const renderPassEncoderInsertDebugMarker = @extern(*const fn (RenderPassEncoder, StringView) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderInsertDebugMarker" });
/// `void wgpuRenderPassEncoderPopDebugGroup(WGPURenderPassEncoder renderPassEncoder)`
pub const renderPassEncoderPopDebugGroup = @extern(*const fn (RenderPassEncoder) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderPopDebugGroup" });
/// `void wgpuRenderPassEncoderPushDebugGroup(WGPURenderPassEncoder renderPassEncoder, WGPUStringView groupLabel)`
pub const renderPassEncoderPushDebugGroup = @extern(*const fn (RenderPassEncoder, StringView) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderPushDebugGroup" });
/// `void wgpuRenderPassEncoderSetBindGroup(WGPURenderPassEncoder renderPassEncoder, uint32_t groupIndex, WGPU_NULLABLE WGPUBindGroup group, size_t dynamicOffsetCount, uint32_t const * dynamicOffsets)`
pub const renderPassEncoderSetBindGroup = @extern(*const fn (RenderPassEncoder, u32, BindGroup, usize, ?[*]const u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetBindGroup" });
/// `void wgpuRenderPassEncoderSetBlendConstant(WGPURenderPassEncoder renderPassEncoder, WGPUColor const * color)`
pub const renderPassEncoderSetBlendConstant = @extern(*const fn (RenderPassEncoder, *const Color) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetBlendConstant" });
/// `void wgpuRenderPassEncoderSetIndexBuffer(WGPURenderPassEncoder renderPassEncoder, WGPUBuffer buffer, WGPUIndexFormat format, uint64_t offset, uint64_t size)`
pub const renderPassEncoderSetIndexBuffer = @extern(*const fn (RenderPassEncoder, Buffer, IndexFormat, u64, u64) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetIndexBuffer" });
/// `void wgpuRenderPassEncoderSetLabel(WGPURenderPassEncoder renderPassEncoder, WGPUStringView label)`
pub const renderPassEncoderSetLabel = @extern(*const fn (RenderPassEncoder, StringView) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetLabel" });
/// `void wgpuRenderPassEncoderSetPipeline(WGPURenderPassEncoder renderPassEncoder, WGPURenderPipeline pipeline)`
pub const renderPassEncoderSetPipeline = @extern(*const fn (RenderPassEncoder, RenderPipeline) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetPipeline" });
/// `void wgpuRenderPassEncoderSetScissorRect(WGPURenderPassEncoder renderPassEncoder, uint32_t x, uint32_t y, uint32_t width, uint32_t height)`
pub const renderPassEncoderSetScissorRect = @extern(*const fn (RenderPassEncoder, u32, u32, u32, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetScissorRect" });
/// `void wgpuRenderPassEncoderSetStencilReference(WGPURenderPassEncoder renderPassEncoder, uint32_t reference)`
pub const renderPassEncoderSetStencilReference = @extern(*const fn (RenderPassEncoder, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetStencilReference" });
/// `void wgpuRenderPassEncoderSetVertexBuffer(WGPURenderPassEncoder renderPassEncoder, uint32_t slot, WGPU_NULLABLE WGPUBuffer buffer, uint64_t offset, uint64_t size)`
pub const renderPassEncoderSetVertexBuffer = @extern(*const fn (RenderPassEncoder, u32, Buffer, u64, u64) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetVertexBuffer" });
/// `void wgpuRenderPassEncoderSetViewport(WGPURenderPassEncoder renderPassEncoder, float x, float y, float width, float height, float minDepth, float maxDepth)`
pub const renderPassEncoderSetViewport = @extern(*const fn (RenderPassEncoder, f32, f32, f32, f32, f32, f32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetViewport" });
/// `void wgpuRenderPassEncoderAddRef(WGPURenderPassEncoder renderPassEncoder)`
pub const renderPassEncoderAddRef = @extern(*const fn (RenderPassEncoder) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderAddRef" });
/// `void wgpuRenderPassEncoderRelease(WGPURenderPassEncoder renderPassEncoder)`
pub const renderPassEncoderRelease = @extern(*const fn (RenderPassEncoder) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderRelease" });
/// `WGPUBindGroupLayout wgpuRenderPipelineGetBindGroupLayout(WGPURenderPipeline renderPipeline, uint32_t groupIndex)`
pub const renderPipelineGetBindGroupLayout = @extern(*const fn (RenderPipeline, u32) callconv(.c) BindGroupLayout, .{ .name = "wgpuRenderPipelineGetBindGroupLayout" });
/// `void wgpuRenderPipelineSetLabel(WGPURenderPipeline renderPipeline, WGPUStringView label)`
pub const renderPipelineSetLabel = @extern(*const fn (RenderPipeline, StringView) callconv(.c) void, .{ .name = "wgpuRenderPipelineSetLabel" });
/// `void wgpuRenderPipelineAddRef(WGPURenderPipeline renderPipeline)`
pub const renderPipelineAddRef = @extern(*const fn (RenderPipeline) callconv(.c) void, .{ .name = "wgpuRenderPipelineAddRef" });
/// `void wgpuRenderPipelineRelease(WGPURenderPipeline renderPipeline)`
pub const renderPipelineRelease = @extern(*const fn (RenderPipeline) callconv(.c) void, .{ .name = "wgpuRenderPipelineRelease" });
/// `void wgpuSamplerSetLabel(WGPUSampler sampler, WGPUStringView label)`
pub const samplerSetLabel = @extern(*const fn (Sampler, StringView) callconv(.c) void, .{ .name = "wgpuSamplerSetLabel" });
/// `void wgpuSamplerAddRef(WGPUSampler sampler)`
pub const samplerAddRef = @extern(*const fn (Sampler) callconv(.c) void, .{ .name = "wgpuSamplerAddRef" });
/// `void wgpuSamplerRelease(WGPUSampler sampler)`
pub const samplerRelease = @extern(*const fn (Sampler) callconv(.c) void, .{ .name = "wgpuSamplerRelease" });
/// `WGPUFuture wgpuShaderModuleGetCompilationInfo(WGPUShaderModule shaderModule, WGPUCompilationInfoCallbackInfo callbackInfo)`
pub const shaderModuleGetCompilationInfo = @extern(*const fn (ShaderModule, CompilationInfoCallbackInfo) callconv(.c) Future, .{ .name = "wgpuShaderModuleGetCompilationInfo" });
/// `void wgpuShaderModuleSetLabel(WGPUShaderModule shaderModule, WGPUStringView label)`
pub const shaderModuleSetLabel = @extern(*const fn (ShaderModule, StringView) callconv(.c) void, .{ .name = "wgpuShaderModuleSetLabel" });
/// `void wgpuShaderModuleAddRef(WGPUShaderModule shaderModule)`
pub const shaderModuleAddRef = @extern(*const fn (ShaderModule) callconv(.c) void, .{ .name = "wgpuShaderModuleAddRef" });
/// `void wgpuShaderModuleRelease(WGPUShaderModule shaderModule)`
pub const shaderModuleRelease = @extern(*const fn (ShaderModule) callconv(.c) void, .{ .name = "wgpuShaderModuleRelease" });
/// `void wgpuSupportedFeaturesFreeMembers(WGPUSupportedFeatures supportedFeatures)`
pub const supportedFeaturesFreeMembers = @extern(*const fn (SupportedFeatures) callconv(.c) void, .{ .name = "wgpuSupportedFeaturesFreeMembers" });
/// `void wgpuSupportedWGSLLanguageFeaturesFreeMembers(WGPUSupportedWGSLLanguageFeatures supportedWGSLLanguageFeatures)`
pub const supportedWGSLLanguageFeaturesFreeMembers = @extern(*const fn (SupportedWGSLLanguageFeatures) callconv(.c) void, .{ .name = "wgpuSupportedWGSLLanguageFeaturesFreeMembers" });
/// `void wgpuSurfaceConfigure(WGPUSurface surface, WGPUSurfaceConfiguration const * config)`
pub const surfaceConfigure = @extern(*const fn (Surface, *const SurfaceConfiguration) callconv(.c) void, .{ .name = "wgpuSurfaceConfigure" });
/// `WGPUStatus wgpuSurfaceGetCapabilities(WGPUSurface surface, WGPUAdapter adapter, WGPUSurfaceCapabilities * capabilities)`
pub const surfaceGetCapabilities = @extern(*const fn (Surface, Adapter, *SurfaceCapabilities) callconv(.c) Status, .{ .name = "wgpuSurfaceGetCapabilities" });
/// `void wgpuSurfaceGetCurrentTexture(WGPUSurface surface, WGPUSurfaceTexture * surfaceTexture)`
pub const surfaceGetCurrentTexture = @extern(*const fn (Surface, *SurfaceTexture) callconv(.c) void, .{ .name = "wgpuSurfaceGetCurrentTexture" });
/// `WGPUStatus wgpuSurfacePresent(WGPUSurface surface)`
pub const surfacePresent = @extern(*const fn (Surface) callconv(.c) Status, .{ .name = "wgpuSurfacePresent" });
/// `void wgpuSurfaceSetLabel(WGPUSurface surface, WGPUStringView label)`
pub const surfaceSetLabel = @extern(*const fn (Surface, StringView) callconv(.c) void, .{ .name = "wgpuSurfaceSetLabel" });
/// `void wgpuSurfaceUnconfigure(WGPUSurface surface)`
pub const surfaceUnconfigure = @extern(*const fn (Surface) callconv(.c) void, .{ .name = "wgpuSurfaceUnconfigure" });
/// `void wgpuSurfaceAddRef(WGPUSurface surface)`
pub const surfaceAddRef = @extern(*const fn (Surface) callconv(.c) void, .{ .name = "wgpuSurfaceAddRef" });
/// `void wgpuSurfaceRelease(WGPUSurface surface)`
pub const surfaceRelease = @extern(*const fn (Surface) callconv(.c) void, .{ .name = "wgpuSurfaceRelease" });
/// `void wgpuSurfaceCapabilitiesFreeMembers(WGPUSurfaceCapabilities surfaceCapabilities)`
pub const surfaceCapabilitiesFreeMembers = @extern(*const fn (SurfaceCapabilities) callconv(.c) void, .{ .name = "wgpuSurfaceCapabilitiesFreeMembers" });
/// `WGPUTextureView wgpuTextureCreateView(WGPUTexture texture, WGPU_NULLABLE WGPUTextureViewDescriptor const * descriptor)`
pub const textureCreateView = @extern(*const fn (Texture, ?*const TextureViewDescriptor) callconv(.c) TextureView, .{ .name = "wgpuTextureCreateView" });
/// `void wgpuTextureDestroy(WGPUTexture texture)`
pub const textureDestroy = @extern(*const fn (Texture) callconv(.c) void, .{ .name = "wgpuTextureDestroy" });
/// `uint32_t wgpuTextureGetDepthOrArrayLayers(WGPUTexture texture)`
pub const textureGetDepthOrArrayLayers = @extern(*const fn (Texture) callconv(.c) u32, .{ .name = "wgpuTextureGetDepthOrArrayLayers" });
/// `WGPUTextureDimension wgpuTextureGetDimension(WGPUTexture texture)`
pub const textureGetDimension = @extern(*const fn (Texture) callconv(.c) TextureDimension, .{ .name = "wgpuTextureGetDimension" });
/// `WGPUTextureFormat wgpuTextureGetFormat(WGPUTexture texture)`
pub const textureGetFormat = @extern(*const fn (Texture) callconv(.c) TextureFormat, .{ .name = "wgpuTextureGetFormat" });
/// `uint32_t wgpuTextureGetHeight(WGPUTexture texture)`
pub const textureGetHeight = @extern(*const fn (Texture) callconv(.c) u32, .{ .name = "wgpuTextureGetHeight" });
/// `uint32_t wgpuTextureGetMipLevelCount(WGPUTexture texture)`
pub const textureGetMipLevelCount = @extern(*const fn (Texture) callconv(.c) u32, .{ .name = "wgpuTextureGetMipLevelCount" });
/// `uint32_t wgpuTextureGetSampleCount(WGPUTexture texture)`
pub const textureGetSampleCount = @extern(*const fn (Texture) callconv(.c) u32, .{ .name = "wgpuTextureGetSampleCount" });
/// `WGPUTextureUsage wgpuTextureGetUsage(WGPUTexture texture)`
pub const textureGetUsage = @extern(*const fn (Texture) callconv(.c) TextureUsage, .{ .name = "wgpuTextureGetUsage" });
/// `uint32_t wgpuTextureGetWidth(WGPUTexture texture)`
pub const textureGetWidth = @extern(*const fn (Texture) callconv(.c) u32, .{ .name = "wgpuTextureGetWidth" });
/// `void wgpuTextureSetLabel(WGPUTexture texture, WGPUStringView label)`
pub const textureSetLabel = @extern(*const fn (Texture, StringView) callconv(.c) void, .{ .name = "wgpuTextureSetLabel" });
/// `void wgpuTextureAddRef(WGPUTexture texture)`
pub const textureAddRef = @extern(*const fn (Texture) callconv(.c) void, .{ .name = "wgpuTextureAddRef" });
/// `void wgpuTextureRelease(WGPUTexture texture)`
pub const textureRelease = @extern(*const fn (Texture) callconv(.c) void, .{ .name = "wgpuTextureRelease" });
/// `void wgpuTextureViewSetLabel(WGPUTextureView textureView, WGPUStringView label)`
pub const textureViewSetLabel = @extern(*const fn (TextureView, StringView) callconv(.c) void, .{ .name = "wgpuTextureViewSetLabel" });
/// `void wgpuTextureViewAddRef(WGPUTextureView textureView)`
pub const textureViewAddRef = @extern(*const fn (TextureView) callconv(.c) void, .{ .name = "wgpuTextureViewAddRef" });
/// `void wgpuTextureViewRelease(WGPUTextureView textureView)`
pub const textureViewRelease = @extern(*const fn (TextureView) callconv(.c) void, .{ .name = "wgpuTextureViewRelease" });
/// `void wgpuGenerateReport(WGPUInstance instance, WGPUGlobalReport * report)`
pub const generateReport = @extern(*const fn (Instance, *GlobalReport) callconv(.c) void, .{ .name = "wgpuGenerateReport" });
/// `size_t wgpuInstanceEnumerateAdapters(WGPUInstance instance, WGPU_NULLABLE WGPUInstanceEnumerateAdapterOptions const * options, WGPUAdapter * adapters)`
pub const instanceEnumerateAdapters = @extern(*const fn (Instance, ?*const InstanceEnumerateAdapterOptions, [*]Adapter) callconv(.c) usize, .{ .name = "wgpuInstanceEnumerateAdapters" });
/// `WGPUSubmissionIndex wgpuQueueSubmitForIndex(WGPUQueue queue, size_t commandCount, WGPUCommandBuffer const * commands)`
pub const queueSubmitForIndex = @extern(*const fn (Queue, usize, ?[*]const CommandBuffer) callconv(.c) SubmissionIndex, .{ .name = "wgpuQueueSubmitForIndex" });
/// `WGPUBool wgpuDevicePoll(WGPUDevice device, WGPUBool wait, WGPU_NULLABLE WGPUSubmissionIndex const * submissionIndex)`
pub const devicePoll = @extern(*const fn (Device, BigBool, ?*const SubmissionIndex) callconv(.c) BigBool, .{ .name = "wgpuDevicePoll" });
/// `WGPUShaderModule wgpuDeviceCreateShaderModuleSpirV(WGPUDevice device, WGPUShaderModuleDescriptorSpirV const * descriptor)`
pub const deviceCreateShaderModuleSpirV = @extern(*const fn (Device, *const ShaderModuleDescriptorSpirV) callconv(.c) ShaderModule, .{ .name = "wgpuDeviceCreateShaderModuleSpirV" });
/// `void wgpuSetLogCallback(WGPULogCallback callback, void * userdata)`
pub const setLogCallback = @extern(*const fn (LogCallback, ?*anyopaque) callconv(.c) void, .{ .name = "wgpuSetLogCallback" });
/// `void wgpuSetLogLevel(WGPULogLevel level)`
pub const setLogLevel = @extern(*const fn (LogLevel) callconv(.c) void, .{ .name = "wgpuSetLogLevel" });
/// `uint32_t wgpuGetVersion(void)`
pub const getVersion = @extern(*const fn () callconv(.c) u32, .{ .name = "wgpuGetVersion" });
/// `void wgpuRenderPassEncoderSetPushConstants(WGPURenderPassEncoder encoder, WGPUShaderStage stages, uint32_t offset, uint32_t sizeBytes, void const * data)`
pub const renderPassEncoderSetPushConstants = @extern(*const fn (RenderPassEncoder, ShaderStage, u32, u32, ?*const anyopaque) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetPushConstants" });
/// `void wgpuComputePassEncoderSetPushConstants(WGPUComputePassEncoder encoder, uint32_t offset, uint32_t sizeBytes, void const * data)`
pub const computePassEncoderSetPushConstants = @extern(*const fn (ComputePassEncoder, u32, u32, ?*const anyopaque) callconv(.c) void, .{ .name = "wgpuComputePassEncoderSetPushConstants" });
/// `void wgpuRenderBundleEncoderSetPushConstants(WGPURenderBundleEncoder encoder, WGPUShaderStage stages, uint32_t offset, uint32_t sizeBytes, void const * data)`
pub const renderBundleEncoderSetPushConstants = @extern(*const fn (RenderBundleEncoder, ShaderStage, u32, u32, ?*const anyopaque) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderSetPushConstants" });
/// `void wgpuRenderPassEncoderMultiDrawIndirect(WGPURenderPassEncoder encoder, WGPUBuffer buffer, uint64_t offset, uint32_t count)`
pub const renderPassEncoderMultiDrawIndirect = @extern(*const fn (RenderPassEncoder, Buffer, u64, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderMultiDrawIndirect" });
/// `void wgpuRenderPassEncoderMultiDrawIndexedIndirect(WGPURenderPassEncoder encoder, WGPUBuffer buffer, uint64_t offset, uint32_t count)`
pub const renderPassEncoderMultiDrawIndexedIndirect = @extern(*const fn (RenderPassEncoder, Buffer, u64, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderMultiDrawIndexedIndirect" });
/// `void wgpuRenderPassEncoderMultiDrawIndirectCount(WGPURenderPassEncoder encoder, WGPUBuffer buffer, uint64_t offset, WGPUBuffer count_buffer, uint64_t count_buffer_offset, uint32_t max_count)`
pub const renderPassEncoderMultiDrawIndirectCount = @extern(*const fn (RenderPassEncoder, Buffer, u64, Buffer, u64, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderMultiDrawIndirectCount" });
/// `void wgpuRenderPassEncoderMultiDrawIndexedIndirectCount(WGPURenderPassEncoder encoder, WGPUBuffer buffer, uint64_t offset, WGPUBuffer count_buffer, uint64_t count_buffer_offset, uint32_t max_count)`
pub const renderPassEncoderMultiDrawIndexedIndirectCount = @extern(*const fn (RenderPassEncoder, Buffer, u64, Buffer, u64, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderMultiDrawIndexedIndirectCount" });
/// `void wgpuComputePassEncoderBeginPipelineStatisticsQuery(WGPUComputePassEncoder computePassEncoder, WGPUQuerySet querySet, uint32_t queryIndex)`
pub const computePassEncoderBeginPipelineStatisticsQuery = @extern(*const fn (ComputePassEncoder, QuerySet, u32) callconv(.c) void, .{ .name = "wgpuComputePassEncoderBeginPipelineStatisticsQuery" });
/// `void wgpuComputePassEncoderEndPipelineStatisticsQuery(WGPUComputePassEncoder computePassEncoder)`
pub const computePassEncoderEndPipelineStatisticsQuery = @extern(*const fn (ComputePassEncoder) callconv(.c) void, .{ .name = "wgpuComputePassEncoderEndPipelineStatisticsQuery" });
/// `void wgpuRenderPassEncoderBeginPipelineStatisticsQuery(WGPURenderPassEncoder renderPassEncoder, WGPUQuerySet querySet, uint32_t queryIndex)`
pub const renderPassEncoderBeginPipelineStatisticsQuery = @extern(*const fn (RenderPassEncoder, QuerySet, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderBeginPipelineStatisticsQuery" });
/// `void wgpuRenderPassEncoderEndPipelineStatisticsQuery(WGPURenderPassEncoder renderPassEncoder)`
pub const renderPassEncoderEndPipelineStatisticsQuery = @extern(*const fn (RenderPassEncoder) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderEndPipelineStatisticsQuery" });
/// `void wgpuComputePassEncoderWriteTimestamp(WGPUComputePassEncoder computePassEncoder, WGPUQuerySet querySet, uint32_t queryIndex)`
pub const computePassEncoderWriteTimestamp = @extern(*const fn (ComputePassEncoder, QuerySet, u32) callconv(.c) void, .{ .name = "wgpuComputePassEncoderWriteTimestamp" });
/// `void wgpuRenderPassEncoderWriteTimestamp(WGPURenderPassEncoder renderPassEncoder, WGPUQuerySet querySet, uint32_t queryIndex)`
pub const renderPassEncoderWriteTimestamp = @extern(*const fn (RenderPassEncoder, QuerySet, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderWriteTimestamp" });
