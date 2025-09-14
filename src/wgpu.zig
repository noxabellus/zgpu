const std = @import("std");

const RawFlags = u64;

pub const BigBool = enum(u32) { false = 0, true = 1 }; // wtf, wgpu?

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
        return @bitCast(@as(RawFlags, @bitCast(self)) | @as(RawFlags, @bitCast(other)));
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
        return @bitCast(@as(RawFlags, @bitCast(self)) | @as(RawFlags, @bitCast(other)));
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
        return @bitCast(@as(RawFlags, @bitCast(self)) | @as(RawFlags, @bitCast(other)));
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
        return @bitCast(@as(RawFlags, @bitCast(self)) | @as(RawFlags, @bitCast(other)));
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
        return @bitCast(@as(RawFlags, @bitCast(self)) | @as(RawFlags, @bitCast(other)));
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
        return @bitCast(@as(RawFlags, @bitCast(self)) | @as(RawFlags, @bitCast(other)));
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
        return @bitCast(@as(RawFlags, @bitCast(self)) | @as(RawFlags, @bitCast(other)));
    }
};

pub const SType = enum(c_uint) {
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

pub const BackendType = enum(c_uint) {
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

pub const AdapterType = enum(c_uint) {
    undefined = 0,
    discrete_gpu = 1,
    integrated_gpu = 2,
    cpu = 3,
    unknown = 4,
};

pub const BlendOperation = enum(c_uint) {
    undefined = 0,
    add = 1,
    subtract = 2,
    reverse_subtract = 3,
    min = 4,
    max = 5,
};

pub const BlendFactor = enum(c_uint) {
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

pub const BufferBindingType = enum(c_uint) {
    binding_not_used = 0,
    undefined = 1,
    uniform = 2,
    storage = 3,
    read_only_storage = 4,
};

pub const CompilationMessageType = enum(c_uint) {
    undefined = 0,
    @"error" = 1,
    warning = 2,
    info = 3,
};

pub const PrimitiveTopology = enum(c_uint) {
    undefined = 0,
    point_list = 1,
    line_list = 2,
    line_strip = 3,
    triangle_list = 4,
    triangle_strip = 5,
};

pub const IndexFormat = enum(c_uint) {
    undefined = 0,
    uint16 = 1,
    uint32 = 2,
};

pub const FrontFace = enum(c_uint) {
    undefined = 0,
    ccw = 1,
    cw = 2,
};

pub const CullMode = enum(c_uint) {
    undefined = 0,
    none = 1,
    front = 2,
    back = 3,
};

pub const QueryType = enum(c_uint) {
    undefined = 0,
    occlusion = 1,
    timestamp = 2,
};

pub const TextureFormat = enum(c_uint) {
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

pub const LoadOp = enum(c_uint) {
    undefined = 0,
    load = 1,
    clear = 2,
};

pub const StoreOp = enum(c_uint) {
    undefined = 0,
    store = 1,
    discard = 2,
};

pub const FeatureLevel = enum(c_uint) {
    undefined = 0,
    compatibility = 1,
    core = 2,
};

pub const PowerPreference = enum(c_uint) {
    undefined = 0,
    low_power = 1,
    high_performance = 2,
};

pub const SamplerBindingType = enum(c_uint) {
    binding_not_used = 0,
    undefined = 1,
    filtering = 2,
    non_filtering = 3,
    comparison = 4,
};

pub const AddressMode = enum(c_uint) {
    undefined = 0,
    clamp_to_edge = 1,
    repeat = 2,
    mirror_repeat = 3,
};

pub const FilterMode = enum(c_uint) {
    undefined = 0,
    nearest = 1,
    linear = 2,
};

pub const MipmapFilterMode = enum(c_uint) {
    undefined = 0,
    nearest = 1,
    linear = 2,
};

pub const CompareFunction = enum(c_uint) {
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

pub const StencilOperation = enum(c_uint) {
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

pub const StorageTextureAccess = enum(c_uint) {
    binding_not_used = 0,
    undefined = 1,
    write_only = 2,
    read_only = 3,
    read_write = 4,
};

pub const TextureViewDimension = enum(c_uint) {
    undefined = 0,
    @"1d" = 1,
    @"2d" = 2,
    @"2d_array" = 3,
    cube = 4,
    cube_array = 5,
    @"3d" = 6,
};

pub const FeatureName = enum(c_uint) {
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

pub const WGSLLanguageFeatureName = enum(c_uint) {
    readonly_and_readwrite_storage_textures = 1,
    packed4x8_integer_dot_product = 2,
    unrestricted_pointer_parameters = 3,
    pointer_composite_access = 4,
};

pub const PresentMode = enum(c_uint) {
    undefined = 0,
    fifo = 1,
    fifo_relaxed = 2,
    immediate = 3,
    mailbox = 4,
};

pub const CompositeAlphaMode = enum(c_uint) {
    auto = 0,
    @"opaque" = 1,
    premultiplied = 2,
    unpremultiplied = 3,
    inherit = 4,
};

pub const SurfaceGetCurrentTextureStatus = enum(c_uint) {
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

pub const TextureSampleType = enum(c_uint) {
    binding_not_used = 0,
    undefined = 1,
    float = 2,
    unfilterable_float = 3,
    depth = 4,
    sint = 5,
    uint = 6,
};

pub const TextureAspect = enum(c_uint) {
    undefined = 0,
    all = 1,
    stencil_only = 2,
    depth_only = 3,
};

pub const VertexFormat = enum(c_uint) {
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

pub const OptionalBool = enum(c_uint) {
    false = 0,
    true = 1,
    undefined = 2,
};

pub const CallbackMode = enum(c_uint) {
    undefined = 0,
    wait_any_only = 1,
    allow_process_events = 2,
    allow_spontaneous = 3,
};

pub const DeviceLostReason = enum(c_uint) {
    unknown = 1,
    destroyed = 2,
    instance_dropped = 3,
    failed_creation = 4,
};

pub const ErrorType = enum(c_uint) {
    no_error = 1,
    validation = 2,
    out_of_memory = 3,
    internal = 4,
    unknown = 5,
};

pub const TextureDimension = enum(c_uint) {
    undefined = 0,
    @"1d" = 1,
    @"2d" = 2,
    @"3d" = 3,
};

pub const VertexStepMode = enum(c_uint) {
    vertex_buffer_not_used = 0,
    undefined = 1,
    vertex = 2,
    instance = 3,
};

pub const MapAsyncStatus = enum(c_uint) {
    success = 1,
    instance_dropped = 2,
    @"error" = 3,
    aborted = 4,
    unknown = 5,
};

pub const CompilationInfoRequestStatus = enum(c_uint) {
    success = 1,
    instance_dropped = 2,
    @"error" = 3,
    unknown = 4,
};

pub const CreatePipelineAsyncStatus = enum(c_uint) {
    success = 1,
    instance_dropped = 2,
    validation_error = 3,
    internal_error = 4,
    unknown = 5,
};

pub const PopErrorScopeStatus = enum(c_uint) {
    success = 1,
    instance_dropped = 2,
    empty_stack = 3,
};

pub const QueueWorkDoneStatus = enum(c_uint) {
    success = 1,
    instance_dropped = 2,
    @"error" = 3,
    unknown = 4,
};

pub const RequestAdapterStatus = enum(c_uint) {
    success = 1,
    instance_dropped = 2,
    unavailable = 3,
    @"error" = 4,
    unknown = 5,
};

pub const RequestDeviceStatus = enum(c_uint) {
    success = 1,
    instance_dropped = 2,
    @"error" = 3,
    unknown = 4,
};

pub const BufferMapState = enum(c_uint) {
    unmapped = 1,
    pending = 2,
    mapped = 3,
};

pub const ErrorFilter = enum(c_uint) {
    validation = 1,
    out_of_memory = 2,
    internal = 3,
};

pub const Status = enum(c_uint) {
    success = 1,
    @"error" = 2,
};

pub const WaitStatus = enum(c_uint) {
    success = 1,
    timed_out = 2,
    unsupported_timeout = 3,
    unsupported_count = 4,
    unsupported_mixed_sources = 5,
};

pub const NativeFeature = enum(c_uint) {
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

pub const LogLevel = enum(c_uint) {
    off = 0,
    @"error" = 1,
    warn = 2,
    info = 3,
    debug = 4,
    trace = 5,
};

pub const Dx12Compiler = enum(c_uint) {
    undefined = 0,
    fxc = 1,
    dxc = 2,
};

pub const Gles3MinorVersion = enum(c_uint) {
    automatic = 0,
    version0 = 1,
    version1 = 2,
    version2 = 3,
};

pub const PipelineStatisticName = enum(c_uint) {
    vertex_shader_invocations = 0,
    clipper_invocations = 1,
    clipper_primitives_out = 2,
    fragment_shader_invocations = 3,
    compute_shader_invocations = 4,
};

pub const NativeQueryType = enum(c_uint) {
    pipeline_statistics = 196608,
};

pub const DxcMaxShaderModel = enum(c_uint) {
    v6_0 = 0,
    v6_1 = 1,
    v6_2 = 2,
    v6_3 = 3,
    v6_4 = 4,
    v6_5 = 5,
    v6_6 = 6,
    v6_7 = 7,
};

pub const GLFenceBehaviour = enum(c_uint) {
    normal = 0,
    auto_finish = 1,
};

pub const NativeTextureFormat = enum(c_uint) {
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
    sType: SType = .undefined,
};

pub const AdapterInfo = extern struct {
    nextInChain: ?*ChainedStructOut = null,
    vendor: StringView = .{},
    architecture: StringView = .{},
    device: StringView = .{},
    description: StringView = .{},
    backendType: BackendType = .undefined,
    adapterType: AdapterType = .undefined,
    vendorID: u32 = 0,
    deviceID: u32 = 0,
};

pub const ChainedStruct = extern struct {
    next: ?*const ChainedStruct = null,
    sType: SType = .undefined,
};

pub const BindGroupEntry = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    binding: u32 = 0,
    buffer: Buffer = null,
    offset: u64 = 0,
    size: u64 = 0,
    sampler: Sampler = null,
    textureView: TextureView = null,
};

pub const BlendComponent = extern struct {
    operation: BlendOperation = .undefined,
    srcFactor: BlendFactor = .undefined,
    dstFactor: BlendFactor = .undefined,
};

pub const BufferBindingLayout = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    type: BufferBindingType = .binding_not_used,
    hasDynamicOffset: BigBool = .false,
    minBindingSize: u64 = 0,
};

pub const BufferDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    usage: BufferUsage = .none,
    size: u64 = 0,
    mappedAtCreation: BigBool = .false,
};

pub const Color = extern struct {
    r: f64 = 0.0,
    g: f64 = 0.0,
    b: f64 = 0.0,
    a: f64 = 0.0,
};

pub const CommandBufferDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
};

pub const CommandEncoderDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
};

pub const CompilationMessage = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    message: StringView = .{},
    type: CompilationMessageType = .undefined,
    lineNum: u64 = 0,
    linePos: u64 = 0,
    offset: u64 = 0,
    length: u64 = 0,
};

pub const ComputePassTimestampWrites = extern struct {
    querySet: QuerySet = null,
    beginningOfPassWriteIndex: u32 = 0,
    endOfPassWriteIndex: u32 = 0,
};

pub const ConstantEntry = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    key: StringView = .{},
    value: f64 = 0.0,
};

pub const Extent3D = extern struct {
    width: u32 = 0,
    height: u32 = 0,
    depthOrArrayLayers: u32 = 0,
};

pub const Future = extern struct {
    id: u64 = 0,
};

pub const InstanceCapabilities = extern struct {
    nextInChain: ?*ChainedStructOut = null,
    timedWaitAnyEnable: BigBool = .false,
    timedWaitAnyMaxCount: usize = 0,
};

pub const Limits = extern struct {
    nextInChain: ?*ChainedStructOut = null,
    maxTextureDimension1D: u32 = 0,
    maxTextureDimension2D: u32 = 0,
    maxTextureDimension3D: u32 = 0,
    maxTextureArrayLayers: u32 = 0,
    maxBindGroups: u32 = 0,
    maxBindGroupsPlusVertexBuffers: u32 = 0,
    maxBindingsPerBindGroup: u32 = 0,
    maxDynamicUniformBuffersPerPipelineLayout: u32 = 0,
    maxDynamicStorageBuffersPerPipelineLayout: u32 = 0,
    maxSampledTexturesPerShaderStage: u32 = 0,
    maxSamplersPerShaderStage: u32 = 0,
    maxStorageBuffersPerShaderStage: u32 = 0,
    maxStorageTexturesPerShaderStage: u32 = 0,
    maxUniformBuffersPerShaderStage: u32 = 0,
    maxUniformBufferBindingSize: u64 = 0,
    maxStorageBufferBindingSize: u64 = 0,
    minUniformBufferOffsetAlignment: u32 = 0,
    minStorageBufferOffsetAlignment: u32 = 0,
    maxVertexBuffers: u32 = 0,
    maxBufferSize: u64 = 0,
    maxVertexAttributes: u32 = 0,
    maxVertexBufferArrayStride: u32 = 0,
    maxInterStageShaderVariables: u32 = 0,
    maxColorAttachments: u32 = 0,
    maxColorAttachmentBytesPerSample: u32 = 0,
    maxComputeWorkgroupStorageSize: u32 = 0,
    maxComputeInvocationsPerWorkgroup: u32 = 0,
    maxComputeWorkgroupSizeX: u32 = 0,
    maxComputeWorkgroupSizeY: u32 = 0,
    maxComputeWorkgroupSizeZ: u32 = 0,
    maxComputeWorkgroupsPerDimension: u32 = 0,
};

pub const MultisampleState = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    count: u32 = 0,
    mask: u32 = 0,
    alphaToCoverageEnabled: BigBool = .false,
};

pub const Origin3D = extern struct {
    x: u32 = 0,
    y: u32 = 0,
    z: u32 = 0,
};

pub const PipelineLayoutDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    bindGroupLayoutCount: usize = 0,
    bindGroupLayouts: ?[*]const BindGroupLayout = null,
};

pub const PrimitiveState = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    topology: PrimitiveTopology = .undefined,
    stripIndexFormat: IndexFormat = .undefined,
    frontFace: FrontFace = .undefined,
    cullMode: CullMode = .undefined,
    unclippedDepth: BigBool = .false,
};

pub const QuerySetDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    type: QueryType = .undefined,
    count: u32 = 0,
};

pub const QueueDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
};

pub const RenderBundleDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
};

pub const RenderBundleEncoderDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    colorFormatCount: usize = 0,
    colorFormats: ?[*]const TextureFormat = null,
    depthStencilFormat: TextureFormat = null,
    sampleCount: u32 = 0,
    depthReadOnly: BigBool = .false,
    stencilReadOnly: BigBool = .false,
};

pub const RenderPassDepthStencilAttachment = extern struct {
    view: TextureView = null,
    depthLoadOp: LoadOp = .undefined,
    depthStoreOp: StoreOp = .undefined,
    depthClearValue: f32 = 0.0,
    depthReadOnly: BigBool = .false,
    stencilLoadOp: LoadOp = .undefined,
    stencilStoreOp: StoreOp = .undefined,
    stencilClearValue: u32 = 0,
    stencilReadOnly: BigBool = .false,
};

pub const RenderPassMaxDrawCount = extern struct {
    chain: ChainedStruct = .{},
    maxDrawCount: u64 = 0,
};

pub const RenderPassTimestampWrites = extern struct {
    querySet: QuerySet = null,
    beginningOfPassWriteIndex: u32 = 0,
    endOfPassWriteIndex: u32 = 0,
};

pub const RequestAdapterOptions = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    featureLevel: FeatureLevel = .undefined,
    powerPreference: PowerPreference = .undefined,
    forceFallbackAdapter: BigBool = .false,
    backendType: BackendType = .undefined,
    compatibleSurface: Surface = null,
};

pub const SamplerBindingLayout = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    type: SamplerBindingType = .binding_not_used,
};

pub const SamplerDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    addressModeU: AddressMode = .undefined,
    addressModeV: AddressMode = .undefined,
    addressModeW: AddressMode = .undefined,
    magFilter: FilterMode = .undefined,
    minFilter: FilterMode = .undefined,
    mipmapFilter: MipmapFilterMode = .undefined,
    lodMinClamp: f32 = 0.0,
    lodMaxClamp: f32 = 0.0,
    compare: CompareFunction = .undefined,
    maxAnisotropy: u16 = 0,
};

pub const ShaderModuleDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
};

pub const ShaderSourceSPIRV = extern struct {
    chain: ChainedStruct = .{},
    codeSize: u32 = 0,
    code: ?[*]const u32 = null,
};

pub const ShaderSourceWGSL = extern struct {
    chain: ChainedStruct = .{},
    code: StringView = .{},
};

pub const StencilFaceState = extern struct {
    compare: CompareFunction = .undefined,
    failOp: StencilOperation = .undefined,
    depthFailOp: StencilOperation = .undefined,
    passOp: StencilOperation = .undefined,
};

pub const StorageTextureBindingLayout = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    access: StorageTextureAccess = .binding_not_used,
    format: TextureFormat = .undefined,
    viewDimension: TextureViewDimension = .undefined,
};

pub const SupportedFeatures = extern struct {
    featureCount: usize = 0,
    features: ?[*]const FeatureName = null,
};

pub const SupportedWGSLLanguageFeatures = extern struct {
    featureCount: usize = 0,
    features: ?[*]const WGSLLanguageFeatureName = null,
};

pub const SurfaceCapabilities = extern struct {
    nextInChain: ?*ChainedStructOut = null,
    usages: TextureUsage = .none,
    formatCount: usize = 0,
    formats: ?[*]const TextureFormat = null,
    presentModeCount: usize = 0,
    presentModes: ?[*]const PresentMode = null,
    alphaModeCount: usize = 0,
    alphaModes: ?[*]const CompositeAlphaMode = null,
};

pub const SurfaceConfiguration = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    device: Device = null,
    format: TextureFormat = .undefined,
    usage: TextureUsage = .none,
    width: u32 = 0,
    height: u32 = 0,
    viewFormatCount: usize = 0,
    viewFormats: ?[*]const TextureFormat = null,
    alphaMode: CompositeAlphaMode = .auto,
    presentMode: PresentMode = .undefined,
};

pub const SurfaceDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
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
    nextInChain: ?*ChainedStructOut = null,
    texture: Texture = null,
    status: SurfaceGetCurrentTextureStatus = .undefined,
};

pub const TexelCopyBufferLayout = extern struct {
    offset: u64 = 0,
    bytesPerRow: u32 = 0,
    rowsPerImage: u32 = 0,
};

pub const TextureBindingLayout = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    sampleType: TextureSampleType = .binding_not_used,
    viewDimension: TextureViewDimension = .undefined,
    multisampled: BigBool = .false,
};

pub const TextureViewDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    format: TextureFormat = .undefined,
    dimension: TextureViewDimension = .undefined,
    baseMipLevel: u32 = 0,
    mipLevelCount: u32 = 0,
    baseArrayLayer: u32 = 0,
    arrayLayerCount: u32 = 0,
    aspect: TextureAspect = .undefined,
    usage: TextureUsage = .none,
};

pub const VertexAttribute = extern struct {
    format: VertexFormat = .undefined,
    offset: u64 = 0,
    shaderLocation: u32 = 0,
};

pub const BindGroupDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    layout: BindGroupLayout = null,
    entryCount: usize = 0,
    entries: ?[*]const BindGroupEntry = null,
};

pub const BindGroupLayoutEntry = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    binding: u32 = 0,
    visibility: ShaderStage = .none,
    buffer: BufferBindingLayout = .{},
    sampler: SamplerBindingLayout = .{},
    texture: TextureBindingLayout = .{},
    storageTexture: StorageTextureBindingLayout = .{},
};

pub const BlendState = extern struct {
    color: BlendComponent = .{},
    alpha: BlendComponent = .{},
};

pub const CompilationInfo = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    messageCount: usize = 0,
    messages: ?[*]const CompilationMessage = null,
};

pub const ComputePassDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    timestampWrites: ?*const ComputePassTimestampWrites = null,
};

pub const DepthStencilState = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    format: TextureFormat = .undefined,
    depthWriteEnabled: OptionalBool = .false,
    depthCompare: CompareFunction = .undefined,
    stencilFront: StencilFaceState = .{},
    stencilBack: StencilFaceState = .{},
    stencilReadMask: u32 = 0,
    stencilWriteMask: u32 = 0,
    depthBias: i32 = 0,
    depthBiasSlopeScale: f32 = 0.0,
    depthBiasClamp: f32 = 0.0,
};

pub const DeviceLostCallbackInfo = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: DeviceLostCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const UncapturedErrorCallbackInfo = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    callback: UncapturedErrorCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const DeviceDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    requiredFeatureCount: usize = 0,
    requiredFeatures: ?[*]const FeatureName = null,
    requiredLimits: ?[*]const Limits = null,
    defaultQueue: QueueDescriptor = .{},
    deviceLostCallbackInfo: DeviceLostCallbackInfo = .{},
    uncapturedErrorCallbackInfo: UncapturedErrorCallbackInfo = .{},
};

pub const FutureWaitInfo = extern struct {
    future: Future = .{},
    completed: BigBool = .false,
};

pub const InstanceDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    features: InstanceCapabilities = .{},
};

pub const ProgrammableStageDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    module: ShaderModule = null,
    entryPoint: StringView = .{},
    constantCount: usize = 0,
    constants: ?[*]const ConstantEntry = null,
};

pub const RenderPassColorAttachment = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    view: TextureView = null,
    depthSlice: u32 = 0,
    resolveTarget: TextureView = null,
    loadOp: LoadOp = .undefined,
    storeOp: StoreOp = .undefined,
    clearValue: Color = .{},
};

pub const TexelCopyBufferInfo = extern struct {
    layout: TexelCopyBufferLayout = .{},
    buffer: Buffer = null,
};

pub const TexelCopyTextureInfo = extern struct {
    texture: Texture = null,
    mipLevel: u32 = 0,
    origin: Origin3D = .{},
    aspect: TextureAspect = .undefined,
};

pub const TextureDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    usage: TextureUsage = .none,
    dimension: TextureDimension = .undefined,
    size: Extent3D = .{},
    format: TextureFormat = .undefined,
    mipLevelCount: u32 = 0,
    sampleCount: u32 = 0,
    viewFormatCount: usize = 0,
    viewFormats: ?[*]const TextureFormat = null,
};

pub const VertexBufferLayout = extern struct {
    stepMode: VertexStepMode = .vertex_buffer_not_used,
    arrayStride: u64 = 0,
    attributeCount: usize = 0,
    attributes: ?[*]const VertexAttribute = null,
};

pub const BindGroupLayoutDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    entryCount: usize = 0,
    entries: ?[*]const BindGroupLayoutEntry = null,
};

pub const ColorTargetState = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    format: TextureFormat = .undefined,
    blend: ?*const BlendState = null,
    writeMask: ColorWriteMask = .none,
};

pub const ComputePipelineDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    layout: PipelineLayout = null,
    compute: ProgrammableStageDescriptor = .{},
};

pub const RenderPassDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    colorAttachmentCount: usize = 0,
    colorAttachments: ?[*]const RenderPassColorAttachment = null,
    depthStencilAttachment: ?*const RenderPassDepthStencilAttachment = null,
    occlusionQuerySet: QuerySet = null,
    timestampWrites: ?*const RenderPassTimestampWrites = null,
};

pub const VertexState = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    module: ShaderModule = null,
    entryPoint: StringView = .{},
    constantCount: usize = 0,
    constants: ?[*]const ConstantEntry = null,
    bufferCount: usize = 0,
    buffers: ?[*]const VertexBufferLayout = null,
};

pub const FragmentState = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    module: ShaderModule = null,
    entryPoint: StringView = .{},
    constantCount: usize = 0,
    constants: ?[*]const ConstantEntry = null,
    targetCount: usize = 0,
    targets: ?[*]const ColorTargetState = null,
};

pub const RenderPipelineDescriptor = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    label: StringView = .{},
    layout: PipelineLayout = null,
    vertex: VertexState = .{},
    primitive: PrimitiveState = .{},
    depthStencil: ?*const DepthStencilState = null,
    multisample: MultisampleState = .{},
    fragment: ?*const FragmentState = null,
};

pub const BufferMapCallbackInfo = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: BufferMapCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const CompilationInfoCallbackInfo = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: CompilationInfoCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const CreateComputePipelineAsyncCallbackInfo = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: CreateComputePipelineAsyncCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const CreateRenderPipelineAsyncCallbackInfo = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: CreateRenderPipelineAsyncCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const PopErrorScopeCallbackInfo = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: PopErrorScopeCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const QueueWorkDoneCallbackInfo = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: QueueWorkDoneCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const RequestAdapterCallbackInfo = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    mode: CallbackMode = .undefined,
    callback: RequestAdapterCallback = null,
    userdata1: ?*anyopaque = null,
    userdata2: ?*anyopaque = null,
};

pub const RequestDeviceCallbackInfo = extern struct {
    nextInChain: ?*const ChainedStruct = null,
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
    dx12ShaderCompiler: Dx12Compiler = .undefined,
    gles3MinorVersion: Gles3MinorVersion = .automatic,
    glFenceBehaviour: GLFenceBehaviour = .normal,
    dxilPath: StringView = .{},
    dxcPath: StringView = .{},
    dxcMaxShaderModel: DxcMaxShaderModel = .v6_0,
};

pub const DeviceExtras = extern struct {
    chain: ChainedStruct = .{},
    tracePath: StringView = .{},
};

pub const NativeLimits = extern struct {
    chain: ChainedStructOut = .{},
    maxPushConstantSize: u32 = 0,
    maxNonSamplerBindings: u32 = 0,
};

pub const PushConstantRange = extern struct {
    stages: ShaderStage = .none,
    start: u32 = 0,
    end: u32 = 0,
};

pub const PipelineLayoutExtras = extern struct {
    chain: ChainedStruct = .{},
    pushConstantRangeCount: usize = 0,
    pushConstantRanges: ?[*]const PushConstantRange = null,
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
    defineCount: u32 = 0,
    defines: ?[*]ShaderDefine = null,
};

pub const ShaderModuleDescriptorSpirV = extern struct {
    label: StringView = .{},
    sourceSize: u32 = 0,
    source: ?*const u32 = null,
};

pub const RegistryReport = extern struct {
    numAllocated: usize = 0,
    numKeptFromUser: usize = 0,
    numReleasedFromUser: usize = 0,
    elementSize: usize = 0,
};

pub const HubReport = extern struct {
    adapters: RegistryReport = .{},
    devices: RegistryReport = .{},
    queues: RegistryReport = .{},
    pipelineLayouts: RegistryReport = .{},
    shaderModules: RegistryReport = .{},
    bindGroupLayouts: RegistryReport = .{},
    bindGroups: RegistryReport = .{},
    commandBuffers: RegistryReport = .{},
    renderBundles: RegistryReport = .{},
    renderPipelines: RegistryReport = .{},
    computePipelines: RegistryReport = .{},
    pipelineCaches: RegistryReport = .{},
    querySets: RegistryReport = .{},
    buffers: RegistryReport = .{},
    textures: RegistryReport = .{},
    textureViews: RegistryReport = .{},
    samplers: RegistryReport = .{},
};

pub const GlobalReport = extern struct {
    surfaces: RegistryReport = .{},
    hub: HubReport = .{},
};

pub const InstanceEnumerateAdapterOptions = extern struct {
    nextInChain: ?*const ChainedStruct = null,
    backends: InstanceBackend = .all,
};

pub const BindGroupEntryExtras = extern struct {
    chain: ChainedStruct = .{},
    buffers: ?[*]const Buffer = null,
    bufferCount: usize = 0,
    samplers: ?[*]const Sampler = null,
    samplerCount: usize = 0,
    textureViews: ?[*]const TextureView = null,
    textureViewCount: usize = 0,
};

pub const BindGroupLayoutEntryExtras = extern struct {
    chain: ChainedStruct = .{},
    count: u32 = 0,
};

pub const QuerySetDescriptorExtras = extern struct {
    chain: ChainedStruct = .{},
    pipelineStatistics: ?[*]const PipelineStatisticName = null,
    pipelineStatisticCount: usize = 0,
};

pub const SurfaceConfigurationExtras = extern struct {
    chain: ChainedStruct = .{},
    desiredMaximumFrameLatency: u32 = 0,
};

pub const SurfaceSourceSwapChainPanel = extern struct {
    chain: ChainedStruct = .{},
    panelNative: ?*anyopaque = null,
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

// WGPUInstance wgpuCreateInstance(WGPU_NULLABLE WGPUInstanceDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const createInstance = @extern(*const fn (?*const InstanceDescriptor) callconv(.c) Instance, .{ .name = "wgpuCreateInstance" });
// WGPUStatus wgpuGetInstanceCapabilities(WGPUInstanceCapabilities * capabilities) WGPU_FUNCTION_ATTRIBUTE;
pub const getInstanceCapabilities = @extern(*const fn (*InstanceCapabilities) callconv(.c) Status, .{ .name = "wgpuGetInstanceCapabilities" });
// WGPUProc wgpuGetProcAddress(WGPUStringView procName) WGPU_FUNCTION_ATTRIBUTE;
pub const getProcAddress = @extern(*const fn (StringView) callconv(.c) Proc, .{ .name = "wgpuGetProcAddress" });
// void wgpuAdapterGetFeatures(WGPUAdapter adapter, WGPUSupportedFeatures * features) WGPU_FUNCTION_ATTRIBUTE;
pub const adapterGetFeatures = @extern(*const fn (Adapter, *SupportedFeatures) callconv(.c) void, .{ .name = "wgpuAdapterGetFeatures" });
// WGPUStatus wgpuAdapterGetInfo(WGPUAdapter adapter, WGPUAdapterInfo * info) WGPU_FUNCTION_ATTRIBUTE;
pub const adapterGetInfo = @extern(*const fn (Adapter, *AdapterInfo) callconv(.c) Status, .{ .name = "wgpuAdapterGetInfo" });
// WGPUStatus wgpuAdapterGetLimits(WGPUAdapter adapter, WGPULimits * limits) WGPU_FUNCTION_ATTRIBUTE;
pub const adapterGetLimits = @extern(*const fn (Adapter, *Limits) callconv(.c) Status, .{ .name = "wgpuAdapterGetLimits" });
// WGPUBool wgpuAdapterHasFeature(WGPUAdapter adapter, WGPUFeatureName feature) WGPU_FUNCTION_ATTRIBUTE;
pub const adapterHasFeature = @extern(*const fn (Adapter, FeatureName) callconv(.c) BigBool, .{ .name = "wgpuAdapterHasFeature" });
// WGPUFuture wgpuAdapterRequestDevice(WGPUAdapter adapter, WGPU_NULLABLE WGPUDeviceDescriptor const * descriptor, WGPURequestDeviceCallbackInfo callbackInfo) WGPU_FUNCTION_ATTRIBUTE;
pub const adapterRequestDevice = @extern(*const fn (Adapter, ?*const DeviceDescriptor, RequestDeviceCallbackInfo) callconv(.c) Future, .{ .name = "wgpuAdapterRequestDevice" });
// void wgpuAdapterAddRef(WGPUAdapter adapter) WGPU_FUNCTION_ATTRIBUTE;
pub const adapterAddRef = @extern(*const fn (Adapter) callconv(.c) void, .{ .name = "wgpuAdapterAddRef" });
// void wgpuAdapterRelease(WGPUAdapter adapter) WGPU_FUNCTION_ATTRIBUTE;
pub const adapterRelease = @extern(*const fn (Adapter) callconv(.c) void, .{ .name = "wgpuAdapterRelease" });
// void wgpuAdapterInfoFreeMembers(WGPUAdapterInfo adapterInfo) WGPU_FUNCTION_ATTRIBUTE;
pub const adapterInfoFreeMembers = @extern(*const fn (AdapterInfo) callconv(.c) void, .{ .name = "wgpuAdapterInfoFreeMembers" });
// void wgpuBindGroupSetLabel(WGPUBindGroup bindGroup, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const bindGroupSetLabel = @extern(*const fn (BindGroup, StringView) callconv(.c) void, .{ .name = "wgpuBindGroupSetLabel" });
// void wgpuBindGroupAddRef(WGPUBindGroup bindGroup) WGPU_FUNCTION_ATTRIBUTE;
pub const bindGroupAddRef = @extern(*const fn (BindGroup) callconv(.c) void, .{ .name = "wgpuBindGroupAddRef" });
// void wgpuBindGroupRelease(WGPUBindGroup bindGroup) WGPU_FUNCTION_ATTRIBUTE;
pub const bindGroupRelease = @extern(*const fn (BindGroup) callconv(.c) void, .{ .name = "wgpuBindGroupRelease" });
// void wgpuBindGroupLayoutSetLabel(WGPUBindGroupLayout bindGroupLayout, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const bindGroupLayoutSetLabel = @extern(*const fn (BindGroupLayout, StringView) callconv(.c) void, .{ .name = "wgpuBindGroupLayoutSetLabel" });
// void wgpuBindGroupLayoutAddRef(WGPUBindGroupLayout bindGroupLayout) WGPU_FUNCTION_ATTRIBUTE;
pub const bindGroupLayoutAddRef = @extern(*const fn (BindGroupLayout) callconv(.c) void, .{ .name = "wgpuBindGroupLayoutAddRef" });
// void wgpuBindGroupLayoutRelease(WGPUBindGroupLayout bindGroupLayout) WGPU_FUNCTION_ATTRIBUTE;
pub const bindGroupLayoutRelease = @extern(*const fn (BindGroupLayout) callconv(.c) void, .{ .name = "wgpuBindGroupLayoutRelease" });
// void wgpuBufferDestroy(WGPUBuffer buffer) WGPU_FUNCTION_ATTRIBUTE;
pub const bufferDestroy = @extern(*const fn (Buffer) callconv(.c) void, .{ .name = "wgpuBufferDestroy" });
// void const * wgpuBufferGetConstMappedRange(WGPUBuffer buffer, size_t offset, size_t size) WGPU_FUNCTION_ATTRIBUTE;
pub const bufferGetConstMappedRange = @extern(*const fn (Buffer, usize, usize) callconv(.c) ?*const anyopaque, .{ .name = "wgpuBufferGetConstMappedRange" });
// WGPUBufferMapState wgpuBufferGetMapState(WGPUBuffer buffer) WGPU_FUNCTION_ATTRIBUTE;
pub const bufferGetMapState = @extern(*const fn (Buffer) callconv(.c) BufferMapState, .{ .name = "wgpuBufferGetMapState" });
// void * wgpuBufferGetMappedRange(WGPUBuffer buffer, size_t offset, size_t size) WGPU_FUNCTION_ATTRIBUTE;
pub const bufferGetMappedRange = @extern(*const fn (Buffer, usize, usize) callconv(.c) ?*anyopaque, .{ .name = "wgpuBufferGetMappedRange" });
// uint64_t wgpuBufferGetSize(WGPUBuffer buffer) WGPU_FUNCTION_ATTRIBUTE;
pub const bufferGetSize = @extern(*const fn (Buffer) callconv(.c) u64, .{ .name = "wgpuBufferGetSize" });
// WGPUBufferUsage wgpuBufferGetUsage(WGPUBuffer buffer) WGPU_FUNCTION_ATTRIBUTE;
pub const bufferGetUsage = @extern(*const fn (Buffer) callconv(.c) BufferUsage, .{ .name = "wgpuBufferGetUsage" });
// WGPUFuture wgpuBufferMapAsync(WGPUBuffer buffer, WGPUMapMode mode, size_t offset, size_t size, WGPUBufferMapCallbackInfo callbackInfo) WGPU_FUNCTION_ATTRIBUTE;
pub const bufferMapAsync = @extern(*const fn (Buffer, MapMode, usize, usize, BufferMapCallbackInfo) callconv(.c) Future, .{ .name = "wgpuBufferMapAsync" });
// void wgpuBufferSetLabel(WGPUBuffer buffer, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const bufferSetLabel = @extern(*const fn (Buffer, StringView) callconv(.c) void, .{ .name = "wgpuBufferSetLabel" });
// void wgpuBufferUnmap(WGPUBuffer buffer) WGPU_FUNCTION_ATTRIBUTE;
pub const bufferUnmap = @extern(*const fn (Buffer) callconv(.c) void, .{ .name = "wgpuBufferUnmap" });
// void wgpuBufferAddRef(WGPUBuffer buffer) WGPU_FUNCTION_ATTRIBUTE;
pub const bufferAddRef = @extern(*const fn (Buffer) callconv(.c) void, .{ .name = "wgpuBufferAddRef" });
// void wgpuBufferRelease(WGPUBuffer buffer) WGPU_FUNCTION_ATTRIBUTE;
pub const bufferRelease = @extern(*const fn (Buffer) callconv(.c) void, .{ .name = "wgpuBufferRelease" });
// void wgpuCommandBufferSetLabel(WGPUCommandBuffer commandBuffer, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const commandBufferSetLabel = @extern(*const fn (CommandBuffer, StringView) callconv(.c) void, .{ .name = "wgpuCommandBufferSetLabel" });
// void wgpuCommandBufferAddRef(WGPUCommandBuffer commandBuffer) WGPU_FUNCTION_ATTRIBUTE;
pub const commandBufferAddRef = @extern(*const fn (CommandBuffer) callconv(.c) void, .{ .name = "wgpuCommandBufferAddRef" });
// void wgpuCommandBufferRelease(WGPUCommandBuffer commandBuffer) WGPU_FUNCTION_ATTRIBUTE;
pub const commandBufferRelease = @extern(*const fn (CommandBuffer) callconv(.c) void, .{ .name = "wgpuCommandBufferRelease" });
// WGPUComputePassEncoder wgpuCommandEncoderBeginComputePass(WGPUCommandEncoder commandEncoder, WGPU_NULLABLE WGPUComputePassDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderBeginComputePass = @extern(*const fn (CommandEncoder, ?*const ComputePassDescriptor) callconv(.c) ComputePassEncoder, .{ .name = "wgpuCommandEncoderBeginComputePass" });
// WGPURenderPassEncoder wgpuCommandEncoderBeginRenderPass(WGPUCommandEncoder commandEncoder, WGPURenderPassDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderBeginRenderPass = @extern(*const fn (CommandEncoder, *const RenderPassDescriptor) callconv(.c) RenderPassEncoder, .{ .name = "wgpuCommandEncoderBeginRenderPass" });
// void wgpuCommandEncoderClearBuffer(WGPUCommandEncoder commandEncoder, WGPUBuffer buffer, uint64_t offset, uint64_t size) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderClearBuffer = @extern(*const fn (CommandEncoder, Buffer, u64, u64) callconv(.c) void, .{ .name = "wgpuCommandEncoderClearBuffer" });
// void wgpuCommandEncoderCopyBufferToBuffer(WGPUCommandEncoder commandEncoder, WGPUBuffer source, uint64_t sourceOffset, WGPUBuffer destination, uint64_t destinationOffset, uint64_t size) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderCopyBufferToBuffer = @extern(*const fn (CommandEncoder, Buffer, u64, Buffer, u64, u64) callconv(.c) void, .{ .name = "wgpuCommandEncoderCopyBufferToBuffer" });
// void wgpuCommandEncoderCopyBufferToTexture(WGPUCommandEncoder commandEncoder, WGPUTexelCopyBufferInfo const * source, WGPUTexelCopyTextureInfo const * destination, WGPUExtent3D const * copySize) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderCopyBufferToTexture = @extern(*const fn (CommandEncoder, *const TexelCopyBufferInfo, *const TexelCopyTextureInfo, *const Extent3D) callconv(.c) void, .{ .name = "wgpuCommandEncoderCopyBufferToTexture" });
// void wgpuCommandEncoderCopyTextureToBuffer(WGPUCommandEncoder commandEncoder, WGPUTexelCopyTextureInfo const * source, WGPUTexelCopyBufferInfo const * destination, WGPUExtent3D const * copySize) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderCopyTextureToBuffer = @extern(*const fn (CommandEncoder, *const TexelCopyTextureInfo, *const TexelCopyBufferInfo, *const Extent3D) callconv(.c) void, .{ .name = "wgpuCommandEncoderCopyTextureToBuffer" });
// void wgpuCommandEncoderCopyTextureToTexture(WGPUCommandEncoder commandEncoder, WGPUTexelCopyTextureInfo const * source, WGPUTexelCopyTextureInfo const * destination, WGPUExtent3D const * copySize) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderCopyTextureToTexture = @extern(*const fn (CommandEncoder, *const TexelCopyTextureInfo, *const TexelCopyTextureInfo, *const Extent3D) callconv(.c) void, .{ .name = "wgpuCommandEncoderCopyTextureToTexture" });
// WGPUCommandBuffer wgpuCommandEncoderFinish(WGPUCommandEncoder commandEncoder, WGPU_NULLABLE WGPUCommandBufferDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderFinish = @extern(*const fn (CommandEncoder, ?*const CommandBufferDescriptor) callconv(.c) CommandBuffer, .{ .name = "wgpuCommandEncoderFinish" });
// void wgpuCommandEncoderInsertDebugMarker(WGPUCommandEncoder commandEncoder, WGPUStringView markerLabel) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderInsertDebugMarker = @extern(*const fn (CommandEncoder, StringView) callconv(.c) void, .{ .name = "wgpuCommandEncoderInsertDebugMarker" });
// void wgpuCommandEncoderPopDebugGroup(WGPUCommandEncoder commandEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderPopDebugGroup = @extern(*const fn (CommandEncoder) callconv(.c) void, .{ .name = "wgpuCommandEncoderPopDebugGroup" });
// void wgpuCommandEncoderPushDebugGroup(WGPUCommandEncoder commandEncoder, WGPUStringView groupLabel) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderPushDebugGroup = @extern(*const fn (CommandEncoder, StringView) callconv(.c) void, .{ .name = "wgpuCommandEncoderPushDebugGroup" });
// void wgpuCommandEncoderResolveQuerySet(WGPUCommandEncoder commandEncoder, WGPUQuerySet querySet, uint32_t firstQuery, uint32_t queryCount, WGPUBuffer destination, uint64_t destinationOffset) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderResolveQuerySet = @extern(*const fn (CommandEncoder, QuerySet, u32, u32, Buffer, u64) callconv(.c) void, .{ .name = "wgpuCommandEncoderResolveQuerySet" });
// void wgpuCommandEncoderSetLabel(WGPUCommandEncoder commandEncoder, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderSetLabel = @extern(*const fn (CommandEncoder, StringView) callconv(.c) void, .{ .name = "wgpuCommandEncoderSetLabel" });
// void wgpuCommandEncoderWriteTimestamp(WGPUCommandEncoder commandEncoder, WGPUQuerySet querySet, uint32_t queryIndex) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderWriteTimestamp = @extern(*const fn (CommandEncoder, QuerySet, u32) callconv(.c) void, .{ .name = "wgpuCommandEncoderWriteTimestamp" });
// void wgpuCommandEncoderAddRef(WGPUCommandEncoder commandEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderAddRef = @extern(*const fn (CommandEncoder) callconv(.c) void, .{ .name = "wgpuCommandEncoderAddRef" });
// void wgpuCommandEncoderRelease(WGPUCommandEncoder commandEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const commandEncoderRelease = @extern(*const fn (CommandEncoder) callconv(.c) void, .{ .name = "wgpuCommandEncoderRelease" });
// void wgpuComputePassEncoderDispatchWorkgroups(WGPUComputePassEncoder computePassEncoder, uint32_t workgroupCountX, uint32_t workgroupCountY, uint32_t workgroupCountZ) WGPU_FUNCTION_ATTRIBUTE;
pub const computePassEncoderDispatchWorkgroups = @extern(*const fn (ComputePassEncoder, u32, u32, u32) callconv(.c) void, .{ .name = "wgpuComputePassEncoderDispatchWorkgroups" });
// void wgpuComputePassEncoderDispatchWorkgroupsIndirect(WGPUComputePassEncoder computePassEncoder, WGPUBuffer indirectBuffer, uint64_t indirectOffset) WGPU_FUNCTION_ATTRIBUTE;
pub const computePassEncoderDispatchWorkgroupsIndirect = @extern(*const fn (ComputePassEncoder, Buffer, u64) callconv(.c) void, .{ .name = "wgpuComputePassEncoderDispatchWorkgroupsIndirect" });
// void wgpuComputePassEncoderEnd(WGPUComputePassEncoder computePassEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const computePassEncoderEnd = @extern(*const fn (ComputePassEncoder) callconv(.c) void, .{ .name = "wgpuComputePassEncoderEnd" });
// void wgpuComputePassEncoderInsertDebugMarker(WGPUComputePassEncoder computePassEncoder, WGPUStringView markerLabel) WGPU_FUNCTION_ATTRIBUTE;
pub const computePassEncoderInsertDebugMarker = @extern(*const fn (ComputePassEncoder, StringView) callconv(.c) void, .{ .name = "wgpuComputePassEncoderInsertDebugMarker" });
// void wgpuComputePassEncoderPopDebugGroup(WGPUComputePassEncoder computePassEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const computePassEncoderPopDebugGroup = @extern(*const fn (ComputePassEncoder) callconv(.c) void, .{ .name = "wgpuComputePassEncoderPopDebugGroup" });
// void wgpuComputePassEncoderPushDebugGroup(WGPUComputePassEncoder computePassEncoder, WGPUStringView groupLabel) WGPU_FUNCTION_ATTRIBUTE;
pub const computePassEncoderPushDebugGroup = @extern(*const fn (ComputePassEncoder, StringView) callconv(.c) void, .{ .name = "wgpuComputePassEncoderPushDebugGroup" });
// void wgpuComputePassEncoderSetBindGroup(WGPUComputePassEncoder computePassEncoder, uint32_t groupIndex, WGPU_NULLABLE WGPUBindGroup group, size_t dynamicOffsetCount, uint32_t const * dynamicOffsets) WGPU_FUNCTION_ATTRIBUTE;
pub const computePassEncoderSetBindGroup = @extern(*const fn (ComputePassEncoder, u32, BindGroup, usize, ?[*]const u32) callconv(.c) void, .{ .name = "wgpuComputePassEncoderSetBindGroup" });
// void wgpuComputePassEncoderSetLabel(WGPUComputePassEncoder computePassEncoder, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const computePassEncoderSetLabel = @extern(*const fn (ComputePassEncoder, StringView) callconv(.c) void, .{ .name = "wgpuComputePassEncoderSetLabel" });
// void wgpuComputePassEncoderSetPipeline(WGPUComputePassEncoder computePassEncoder, WGPUComputePipeline pipeline) WGPU_FUNCTION_ATTRIBUTE;
pub const computePassEncoderSetPipeline = @extern(*const fn (ComputePassEncoder, ComputePipeline) callconv(.c) void, .{ .name = "wgpuComputePassEncoderSetPipeline" });
// void wgpuComputePassEncoderAddRef(WGPUComputePassEncoder computePassEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const computePassEncoderAddRef = @extern(*const fn (ComputePassEncoder) callconv(.c) void, .{ .name = "wgpuComputePassEncoderAddRef" });
// void wgpuComputePassEncoderRelease(WGPUComputePassEncoder computePassEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const computePassEncoderRelease = @extern(*const fn (ComputePassEncoder) callconv(.c) void, .{ .name = "wgpuComputePassEncoderRelease" });
// WGPUBindGroupLayout wgpuComputePipelineGetBindGroupLayout(WGPUComputePipeline computePipeline, uint32_t groupIndex) WGPU_FUNCTION_ATTRIBUTE;
pub const computePipelineGetBindGroupLayout = @extern(*const fn (ComputePipeline, u32) callconv(.c) BindGroupLayout, .{ .name = "wgpuComputePipelineGetBindGroupLayout" });
// void wgpuComputePipelineSetLabel(WGPUComputePipeline computePipeline, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const computePipelineSetLabel = @extern(*const fn (ComputePipeline, StringView) callconv(.c) void, .{ .name = "wgpuComputePipelineSetLabel" });
// void wgpuComputePipelineAddRef(WGPUComputePipeline computePipeline) WGPU_FUNCTION_ATTRIBUTE;
pub const computePipelineAddRef = @extern(*const fn (ComputePipeline) callconv(.c) void, .{ .name = "wgpuComputePipelineAddRef" });
// void wgpuComputePipelineRelease(WGPUComputePipeline computePipeline) WGPU_FUNCTION_ATTRIBUTE;
pub const computePipelineRelease = @extern(*const fn (ComputePipeline) callconv(.c) void, .{ .name = "wgpuComputePipelineRelease" });
// WGPUBindGroup wgpuDeviceCreateBindGroup(WGPUDevice device, WGPUBindGroupDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreateBindGroup = @extern(*const fn (Device, *const BindGroupDescriptor) callconv(.c) BindGroup, .{ .name = "wgpuDeviceCreateBindGroup" });
// WGPUBindGroupLayout wgpuDeviceCreateBindGroupLayout(WGPUDevice device, WGPUBindGroupLayoutDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreateBindGroupLayout = @extern(*const fn (Device, *const BindGroupLayoutDescriptor) callconv(.c) BindGroupLayout, .{ .name = "wgpuDeviceCreateBindGroupLayout" });
// WGPUBuffer wgpuDeviceCreateBuffer(WGPUDevice device, WGPUBufferDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreateBuffer = @extern(*const fn (Device, *const BufferDescriptor) callconv(.c) Buffer, .{ .name = "wgpuDeviceCreateBuffer" });
// WGPUCommandEncoder wgpuDeviceCreateCommandEncoder(WGPUDevice device, WGPU_NULLABLE WGPUCommandEncoderDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreateCommandEncoder = @extern(*const fn (Device, ?*const CommandEncoderDescriptor) callconv(.c) CommandEncoder, .{ .name = "wgpuDeviceCreateCommandEncoder" });
// WGPUComputePipeline wgpuDeviceCreateComputePipeline(WGPUDevice device, WGPUComputePipelineDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreateComputePipeline = @extern(*const fn (Device, *const ComputePipelineDescriptor) callconv(.c) ComputePipeline, .{ .name = "wgpuDeviceCreateComputePipeline" });
// WGPUFuture wgpuDeviceCreateComputePipelineAsync(WGPUDevice device, WGPUComputePipelineDescriptor const * descriptor, WGPUCreateComputePipelineAsyncCallbackInfo callbackInfo) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreateComputePipelineAsync = @extern(*const fn (Device, *const ComputePipelineDescriptor, CreateComputePipelineAsyncCallbackInfo) callconv(.c) Future, .{ .name = "wgpuDeviceCreateComputePipelineAsync" });
// WGPUPipelineLayout wgpuDeviceCreatePipelineLayout(WGPUDevice device, WGPUPipelineLayoutDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreatePipelineLayout = @extern(*const fn (Device, *const PipelineLayoutDescriptor) callconv(.c) PipelineLayout, .{ .name = "wgpuDeviceCreatePipelineLayout" });
// WGPUQuerySet wgpuDeviceCreateQuerySet(WGPUDevice device, WGPUQuerySetDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreateQuerySet = @extern(*const fn (Device, *const QuerySetDescriptor) callconv(.c) QuerySet, .{ .name = "wgpuDeviceCreateQuerySet" });
// WGPURenderBundleEncoder wgpuDeviceCreateRenderBundleEncoder(WGPUDevice device, WGPURenderBundleEncoderDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreateRenderBundleEncoder = @extern(*const fn (Device, *const RenderBundleEncoderDescriptor) callconv(.c) RenderBundleEncoder, .{ .name = "wgpuDeviceCreateRenderBundleEncoder" });
// WGPURenderPipeline wgpuDeviceCreateRenderPipeline(WGPUDevice device, WGPURenderPipelineDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreateRenderPipeline = @extern(*const fn (Device, *const RenderPipelineDescriptor) callconv(.c) RenderPipeline, .{ .name = "wgpuDeviceCreateRenderPipeline" });
// WGPUFuture wgpuDeviceCreateRenderPipelineAsync(WGPUDevice device, WGPURenderPipelineDescriptor const * descriptor, WGPUCreateRenderPipelineAsyncCallbackInfo callbackInfo) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreateRenderPipelineAsync = @extern(*const fn (Device, *const RenderPipelineDescriptor, CreateRenderPipelineAsyncCallbackInfo) callconv(.c) Future, .{ .name = "wgpuDeviceCreateRenderPipelineAsync" });
// WGPUSampler wgpuDeviceCreateSampler(WGPUDevice device, WGPU_NULLABLE WGPUSamplerDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreateSampler = @extern(*const fn (Device, ?*const SamplerDescriptor) callconv(.c) Sampler, .{ .name = "wgpuDeviceCreateSampler" });
// WGPUShaderModule wgpuDeviceCreateShaderModule(WGPUDevice device, WGPUShaderModuleDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreateShaderModule = @extern(*const fn (Device, *const ShaderModuleDescriptor) callconv(.c) ShaderModule, .{ .name = "wgpuDeviceCreateShaderModule" });
// WGPUTexture wgpuDeviceCreateTexture(WGPUDevice device, WGPUTextureDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceCreateTexture = @extern(*const fn (Device, *const TextureDescriptor) callconv(.c) Texture, .{ .name = "wgpuDeviceCreateTexture" });
// void wgpuDeviceDestroy(WGPUDevice device) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceDestroy = @extern(*const fn (Device) callconv(.c) void, .{ .name = "wgpuDeviceDestroy" });
// WGPUAdapterInfo wgpuDeviceGetAdapterInfo(WGPUDevice device) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceGetAdapterInfo = @extern(*const fn (Device) callconv(.c) AdapterInfo, .{ .name = "wgpuDeviceGetAdapterInfo" });
// void wgpuDeviceGetFeatures(WGPUDevice device, WGPUSupportedFeatures * features) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceGetFeatures = @extern(*const fn (Device, *SupportedFeatures) callconv(.c) void, .{ .name = "wgpuDeviceGetFeatures" });
// WGPUStatus wgpuDeviceGetLimits(WGPUDevice device, WGPULimits * limits) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceGetLimits = @extern(*const fn (Device, *Limits) callconv(.c) Status, .{ .name = "wgpuDeviceGetLimits" });
// WGPUFuture wgpuDeviceGetLostFuture(WGPUDevice device) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceGetLostFuture = @extern(*const fn (Device) callconv(.c) Future, .{ .name = "wgpuDeviceGetLostFuture" });
// WGPUQueue wgpuDeviceGetQueue(WGPUDevice device) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceGetQueue = @extern(*const fn (Device) callconv(.c) Queue, .{ .name = "wgpuDeviceGetQueue" });
// WGPUBool wgpuDeviceHasFeature(WGPUDevice device, WGPUFeatureName feature) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceHasFeature = @extern(*const fn (Device, FeatureName) callconv(.c) BigBool, .{ .name = "wgpuDeviceHasFeature" });
// WGPUFuture wgpuDevicePopErrorScope(WGPUDevice device, WGPUPopErrorScopeCallbackInfo callbackInfo) WGPU_FUNCTION_ATTRIBUTE;
pub const devicePopErrorScope = @extern(*const fn (Device, PopErrorScopeCallbackInfo) callconv(.c) Future, .{ .name = "wgpuDevicePopErrorScope" });
// void wgpuDevicePushErrorScope(WGPUDevice device, WGPUErrorFilter filter) WGPU_FUNCTION_ATTRIBUTE;
pub const devicePushErrorScope = @extern(*const fn (Device, ErrorFilter) callconv(.c) void, .{ .name = "wgpuDevicePushErrorScope" });
// void wgpuDeviceSetLabel(WGPUDevice device, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceSetLabel = @extern(*const fn (Device, StringView) callconv(.c) void, .{ .name = "wgpuDeviceSetLabel" });
// void wgpuDeviceAddRef(WGPUDevice device) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceAddRef = @extern(*const fn (Device) callconv(.c) void, .{ .name = "wgpuDeviceAddRef" });
// void wgpuDeviceRelease(WGPUDevice device) WGPU_FUNCTION_ATTRIBUTE;
pub const deviceRelease = @extern(*const fn (Device) callconv(.c) void, .{ .name = "wgpuDeviceRelease" });
// WGPUSurface wgpuInstanceCreateSurface(WGPUInstance instance, WGPUSurfaceDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const instanceCreateSurface = @extern(*const fn (Instance, *const SurfaceDescriptor) callconv(.c) Surface, .{ .name = "wgpuInstanceCreateSurface" });
// WGPUStatus wgpuInstanceGetWGSLLanguageFeatures(WGPUInstance instance, WGPUSupportedWGSLLanguageFeatures * features) WGPU_FUNCTION_ATTRIBUTE;
pub const instanceGetWGSLLanguageFeatures = @extern(*const fn (Instance, *SupportedWGSLLanguageFeatures) callconv(.c) Status, .{ .name = "wgpuInstanceGetWGSLLanguageFeatures" });
// WGPUBool wgpuInstanceHasWGSLLanguageFeature(WGPUInstance instance, WGPUWGSLLanguageFeatureName feature) WGPU_FUNCTION_ATTRIBUTE;
pub const instanceHasWGSLLanguageFeature = @extern(*const fn (Instance, WGSLLanguageFeatureName) callconv(.c) BigBool, .{ .name = "wgpuInstanceHasWGSLLanguageFeature" });
// void wgpuInstanceProcessEvents(WGPUInstance instance) WGPU_FUNCTION_ATTRIBUTE;
pub const instanceProcessEvents = @extern(*const fn (Instance) callconv(.c) void, .{ .name = "wgpuInstanceProcessEvents" });
// WGPUFuture wgpuInstanceRequestAdapter(WGPUInstance instance, WGPU_NULLABLE WGPURequestAdapterOptions const * options, WGPURequestAdapterCallbackInfo callbackInfo) WGPU_FUNCTION_ATTRIBUTE;
pub const instanceRequestAdapter = @extern(*const fn (Instance, ?*const RequestAdapterOptions, RequestAdapterCallbackInfo) callconv(.c) Future, .{ .name = "wgpuInstanceRequestAdapter" });
// WGPUWaitStatus wgpuInstanceWaitAny(WGPUInstance instance, size_t futureCount, WGPU_NULLABLE WGPUFutureWaitInfo * futures, uint64_t timeoutNS) WGPU_FUNCTION_ATTRIBUTE;
pub const instanceWaitAny = @extern(*const fn (Instance, usize, [*]FutureWaitInfo, u64) callconv(.c) WaitStatus, .{ .name = "wgpuInstanceWaitAny" });
// void wgpuInstanceAddRef(WGPUInstance instance) WGPU_FUNCTION_ATTRIBUTE;
pub const instanceAddRef = @extern(*const fn (Instance) callconv(.c) void, .{ .name = "wgpuInstanceAddRef" });
// void wgpuInstanceRelease(WGPUInstance instance) WGPU_FUNCTION_ATTRIBUTE;
pub const instanceRelease = @extern(*const fn (Instance) callconv(.c) void, .{ .name = "wgpuInstanceRelease" });
// void wgpuPipelineLayoutSetLabel(WGPUPipelineLayout pipelineLayout, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const pipelineLayoutSetLabel = @extern(*const fn (PipelineLayout, StringView) callconv(.c) void, .{ .name = "wgpuPipelineLayoutSetLabel" });
// void wgpuPipelineLayoutAddRef(WGPUPipelineLayout pipelineLayout) WGPU_FUNCTION_ATTRIBUTE;
pub const pipelineLayoutAddRef = @extern(*const fn (PipelineLayout) callconv(.c) void, .{ .name = "wgpuPipelineLayoutAddRef" });
// void wgpuPipelineLayoutRelease(WGPUPipelineLayout pipelineLayout) WGPU_FUNCTION_ATTRIBUTE;
pub const pipelineLayoutRelease = @extern(*const fn (PipelineLayout) callconv(.c) void, .{ .name = "wgpuPipelineLayoutRelease" });
// void wgpuQuerySetDestroy(WGPUQuerySet querySet) WGPU_FUNCTION_ATTRIBUTE;
pub const querySetDestroy = @extern(*const fn (QuerySet) callconv(.c) void, .{ .name = "wgpuQuerySetDestroy" });
// uint32_t wgpuQuerySetGetCount(WGPUQuerySet querySet) WGPU_FUNCTION_ATTRIBUTE;
pub const querySetGetCount = @extern(*const fn (QuerySet) callconv(.c) u32, .{ .name = "wgpuQuerySetGetCount" });
// WGPUQueryType wgpuQuerySetGetType(WGPUQuerySet querySet) WGPU_FUNCTION_ATTRIBUTE;
pub const querySetGetType = @extern(*const fn (QuerySet) callconv(.c) QueryType, .{ .name = "wgpuQuerySetGetType" });
// void wgpuQuerySetSetLabel(WGPUQuerySet querySet, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const querySetSetLabel = @extern(*const fn (QuerySet, StringView) callconv(.c) void, .{ .name = "wgpuQuerySetSetLabel" });
// void wgpuQuerySetAddRef(WGPUQuerySet querySet) WGPU_FUNCTION_ATTRIBUTE;
pub const querySetAddRef = @extern(*const fn (QuerySet) callconv(.c) void, .{ .name = "wgpuQuerySetAddRef" });
// void wgpuQuerySetRelease(WGPUQuerySet querySet) WGPU_FUNCTION_ATTRIBUTE;
pub const querySetRelease = @extern(*const fn (QuerySet) callconv(.c) void, .{ .name = "wgpuQuerySetRelease" });
// WGPUFuture wgpuQueueOnSubmittedWorkDone(WGPUQueue queue, WGPUQueueWorkDoneCallbackInfo callbackInfo) WGPU_FUNCTION_ATTRIBUTE;
pub const queueOnSubmittedWorkDone = @extern(*const fn (Queue, QueueWorkDoneCallbackInfo) callconv(.c) Future, .{ .name = "wgpuQueueOnSubmittedWorkDone" });
// void wgpuQueueSetLabel(WGPUQueue queue, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const queueSetLabel = @extern(*const fn (Queue, StringView) callconv(.c) void, .{ .name = "wgpuQueueSetLabel" });
// void wgpuQueueSubmit(WGPUQueue queue, size_t commandCount, WGPUCommandBuffer const * commands) WGPU_FUNCTION_ATTRIBUTE;
pub const queueSubmit = @extern(*const fn (Queue, usize, ?[*]const CommandBuffer) callconv(.c) void, .{ .name = "wgpuQueueSubmit" });
// void wgpuQueueWriteBuffer(WGPUQueue queue, WGPUBuffer buffer, uint64_t bufferOffset, void const * data, size_t size) WGPU_FUNCTION_ATTRIBUTE;
pub const queueWriteBuffer = @extern(*const fn (Queue, Buffer, u64, ?*const anyopaque, usize) callconv(.c) void, .{ .name = "wgpuQueueWriteBuffer" });
// void wgpuQueueWriteTexture(WGPUQueue queue, WGPUTexelCopyTextureInfo const * destination, void const * data, size_t dataSize, WGPUTexelCopyBufferLayout const * dataLayout, WGPUExtent3D const * writeSize) WGPU_FUNCTION_ATTRIBUTE;
pub const queueWriteTexture = @extern(*const fn (Queue, *const TexelCopyTextureInfo, ?*const anyopaque, usize, *const TexelCopyBufferLayout, *const Extent3D) callconv(.c) void, .{ .name = "wgpuQueueWriteTexture" });
// void wgpuQueueAddRef(WGPUQueue queue) WGPU_FUNCTION_ATTRIBUTE;
pub const queueAddRef = @extern(*const fn (Queue) callconv(.c) void, .{ .name = "wgpuQueueAddRef" });
// void wgpuQueueRelease(WGPUQueue queue) WGPU_FUNCTION_ATTRIBUTE;
pub const queueRelease = @extern(*const fn (Queue) callconv(.c) void, .{ .name = "wgpuQueueRelease" });
// void wgpuRenderBundleSetLabel(WGPURenderBundle renderBundle, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleSetLabel = @extern(*const fn (RenderBundle, StringView) callconv(.c) void, .{ .name = "wgpuRenderBundleSetLabel" });
// void wgpuRenderBundleAddRef(WGPURenderBundle renderBundle) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleAddRef = @extern(*const fn (RenderBundle) callconv(.c) void, .{ .name = "wgpuRenderBundleAddRef" });
// void wgpuRenderBundleRelease(WGPURenderBundle renderBundle) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleRelease = @extern(*const fn (RenderBundle) callconv(.c) void, .{ .name = "wgpuRenderBundleRelease" });
// void wgpuRenderBundleEncoderDraw(WGPURenderBundleEncoder renderBundleEncoder, uint32_t vertexCount, uint32_t instanceCount, uint32_t firstVertex, uint32_t firstInstance) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderDraw = @extern(*const fn (RenderBundleEncoder, u32, u32, u32, u32) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderDraw" });
// void wgpuRenderBundleEncoderDrawIndexed(WGPURenderBundleEncoder renderBundleEncoder, uint32_t indexCount, uint32_t instanceCount, uint32_t firstIndex, int32_t baseVertex, uint32_t firstInstance) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderDrawIndexed = @extern(*const fn (RenderBundleEncoder, u32, u32, u32, i32, u32) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderDrawIndexed" });
// void wgpuRenderBundleEncoderDrawIndexedIndirect(WGPURenderBundleEncoder renderBundleEncoder, WGPUBuffer indirectBuffer, uint64_t indirectOffset) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderDrawIndexedIndirect = @extern(*const fn (RenderBundleEncoder, Buffer, u64) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderDrawIndexedIndirect" });
// void wgpuRenderBundleEncoderDrawIndirect(WGPURenderBundleEncoder renderBundleEncoder, WGPUBuffer indirectBuffer, uint64_t indirectOffset) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderDrawIndirect = @extern(*const fn (RenderBundleEncoder, Buffer, u64) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderDrawIndirect" });
// WGPURenderBundle wgpuRenderBundleEncoderFinish(WGPURenderBundleEncoder renderBundleEncoder, WGPU_NULLABLE WGPURenderBundleDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderFinish = @extern(*const fn (RenderBundleEncoder, ?*const RenderBundleDescriptor) callconv(.c) RenderBundle, .{ .name = "wgpuRenderBundleEncoderFinish" });
// void wgpuRenderBundleEncoderInsertDebugMarker(WGPURenderBundleEncoder renderBundleEncoder, WGPUStringView markerLabel) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderInsertDebugMarker = @extern(*const fn (RenderBundleEncoder, StringView) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderInsertDebugMarker" });
// void wgpuRenderBundleEncoderPopDebugGroup(WGPURenderBundleEncoder renderBundleEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderPopDebugGroup = @extern(*const fn (RenderBundleEncoder) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderPopDebugGroup" });
// void wgpuRenderBundleEncoderPushDebugGroup(WGPURenderBundleEncoder renderBundleEncoder, WGPUStringView groupLabel) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderPushDebugGroup = @extern(*const fn (RenderBundleEncoder, StringView) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderPushDebugGroup" });
// void wgpuRenderBundleEncoderSetBindGroup(WGPURenderBundleEncoder renderBundleEncoder, uint32_t groupIndex, WGPU_NULLABLE WGPUBindGroup group, size_t dynamicOffsetCount, uint32_t const * dynamicOffsets) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderSetBindGroup = @extern(*const fn (RenderBundleEncoder, u32, BindGroup, usize, ?[*]const u32) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderSetBindGroup" });
// void wgpuRenderBundleEncoderSetIndexBuffer(WGPURenderBundleEncoder renderBundleEncoder, WGPUBuffer buffer, WGPUIndexFormat format, uint64_t offset, uint64_t size) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderSetIndexBuffer = @extern(*const fn (RenderBundleEncoder, Buffer, IndexFormat, u64, u64) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderSetIndexBuffer" });
// void wgpuRenderBundleEncoderSetLabel(WGPURenderBundleEncoder renderBundleEncoder, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderSetLabel = @extern(*const fn (RenderBundleEncoder, StringView) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderSetLabel" });
// void wgpuRenderBundleEncoderSetPipeline(WGPURenderBundleEncoder renderBundleEncoder, WGPURenderPipeline pipeline) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderSetPipeline = @extern(*const fn (RenderBundleEncoder, RenderPipeline) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderSetPipeline" });
// void wgpuRenderBundleEncoderSetVertexBuffer(WGPURenderBundleEncoder renderBundleEncoder, uint32_t slot, WGPU_NULLABLE WGPUBuffer buffer, uint64_t offset, uint64_t size) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderSetVertexBuffer = @extern(*const fn (RenderBundleEncoder, u32, Buffer, u64, u64) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderSetVertexBuffer" });
// void wgpuRenderBundleEncoderAddRef(WGPURenderBundleEncoder renderBundleEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderAddRef = @extern(*const fn (RenderBundleEncoder) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderAddRef" });
// void wgpuRenderBundleEncoderRelease(WGPURenderBundleEncoder renderBundleEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const renderBundleEncoderRelease = @extern(*const fn (RenderBundleEncoder) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderRelease" });
// void wgpuRenderPassEncoderBeginOcclusionQuery(WGPURenderPassEncoder renderPassEncoder, uint32_t queryIndex) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderBeginOcclusionQuery = @extern(*const fn (RenderPassEncoder, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderBeginOcclusionQuery" });
// void wgpuRenderPassEncoderDraw(WGPURenderPassEncoder renderPassEncoder, uint32_t vertexCount, uint32_t instanceCount, uint32_t firstVertex, uint32_t firstInstance) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderDraw = @extern(*const fn (RenderPassEncoder, u32, u32, u32, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderDraw" });
// void wgpuRenderPassEncoderDrawIndexed(WGPURenderPassEncoder renderPassEncoder, uint32_t indexCount, uint32_t instanceCount, uint32_t firstIndex, int32_t baseVertex, uint32_t firstInstance) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderDrawIndexed = @extern(*const fn (RenderPassEncoder, u32, u32, u32, i32, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderDrawIndexed" });
// void wgpuRenderPassEncoderDrawIndexedIndirect(WGPURenderPassEncoder renderPassEncoder, WGPUBuffer indirectBuffer, uint64_t indirectOffset) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderDrawIndexedIndirect = @extern(*const fn (RenderPassEncoder, Buffer, u64) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderDrawIndexedIndirect" });
// void wgpuRenderPassEncoderDrawIndirect(WGPURenderPassEncoder renderPassEncoder, WGPUBuffer indirectBuffer, uint64_t indirectOffset) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderDrawIndirect = @extern(*const fn (RenderPassEncoder, Buffer, u64) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderDrawIndirect" });
// void wgpuRenderPassEncoderEnd(WGPURenderPassEncoder renderPassEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderEnd = @extern(*const fn (RenderPassEncoder) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderEnd" });
// void wgpuRenderPassEncoderEndOcclusionQuery(WGPURenderPassEncoder renderPassEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderEndOcclusionQuery = @extern(*const fn (RenderPassEncoder) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderEndOcclusionQuery" });
// void wgpuRenderPassEncoderExecuteBundles(WGPURenderPassEncoder renderPassEncoder, size_t bundleCount, WGPURenderBundle const * bundles) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderExecuteBundles = @extern(*const fn (RenderPassEncoder, usize, ?[*]const RenderBundle) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderExecuteBundles" });
// void wgpuRenderPassEncoderInsertDebugMarker(WGPURenderPassEncoder renderPassEncoder, WGPUStringView markerLabel) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderInsertDebugMarker = @extern(*const fn (RenderPassEncoder, StringView) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderInsertDebugMarker" });
// void wgpuRenderPassEncoderPopDebugGroup(WGPURenderPassEncoder renderPassEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderPopDebugGroup = @extern(*const fn (RenderPassEncoder) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderPopDebugGroup" });
// void wgpuRenderPassEncoderPushDebugGroup(WGPURenderPassEncoder renderPassEncoder, WGPUStringView groupLabel) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderPushDebugGroup = @extern(*const fn (RenderPassEncoder, StringView) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderPushDebugGroup" });
// void wgpuRenderPassEncoderSetBindGroup(WGPURenderPassEncoder renderPassEncoder, uint32_t groupIndex, WGPU_NULLABLE WGPUBindGroup group, size_t dynamicOffsetCount, uint32_t const * dynamicOffsets) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderSetBindGroup = @extern(*const fn (RenderPassEncoder, u32, BindGroup, usize, ?[*]const u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetBindGroup" });
// void wgpuRenderPassEncoderSetBlendConstant(WGPURenderPassEncoder renderPassEncoder, WGPUColor const * color) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderSetBlendConstant = @extern(*const fn (RenderPassEncoder, *const Color) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetBlendConstant" });
// void wgpuRenderPassEncoderSetIndexBuffer(WGPURenderPassEncoder renderPassEncoder, WGPUBuffer buffer, WGPUIndexFormat format, uint64_t offset, uint64_t size) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderSetIndexBuffer = @extern(*const fn (RenderPassEncoder, Buffer, IndexFormat, u64, u64) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetIndexBuffer" });
// void wgpuRenderPassEncoderSetLabel(WGPURenderPassEncoder renderPassEncoder, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderSetLabel = @extern(*const fn (RenderPassEncoder, StringView) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetLabel" });
// void wgpuRenderPassEncoderSetPipeline(WGPURenderPassEncoder renderPassEncoder, WGPURenderPipeline pipeline) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderSetPipeline = @extern(*const fn (RenderPassEncoder, RenderPipeline) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetPipeline" });
// void wgpuRenderPassEncoderSetScissorRect(WGPURenderPassEncoder renderPassEncoder, uint32_t x, uint32_t y, uint32_t width, uint32_t height) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderSetScissorRect = @extern(*const fn (RenderPassEncoder, u32, u32, u32, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetScissorRect" });
// void wgpuRenderPassEncoderSetStencilReference(WGPURenderPassEncoder renderPassEncoder, uint32_t reference) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderSetStencilReference = @extern(*const fn (RenderPassEncoder, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetStencilReference" });
// void wgpuRenderPassEncoderSetVertexBuffer(WGPURenderPassEncoder renderPassEncoder, uint32_t slot, WGPU_NULLABLE WGPUBuffer buffer, uint64_t offset, uint64_t size) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderSetVertexBuffer = @extern(*const fn (RenderPassEncoder, u32, Buffer, u64, u64) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetVertexBuffer" });
// void wgpuRenderPassEncoderSetViewport(WGPURenderPassEncoder renderPassEncoder, float x, float y, float width, float height, float minDepth, float maxDepth) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderSetViewport = @extern(*const fn (RenderPassEncoder, f32, f32, f32, f32, f32, f32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetViewport" });
// void wgpuRenderPassEncoderAddRef(WGPURenderPassEncoder renderPassEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderAddRef = @extern(*const fn (RenderPassEncoder) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderAddRef" });
// void wgpuRenderPassEncoderRelease(WGPURenderPassEncoder renderPassEncoder) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPassEncoderRelease = @extern(*const fn (RenderPassEncoder) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderRelease" });
// WGPUBindGroupLayout wgpuRenderPipelineGetBindGroupLayout(WGPURenderPipeline renderPipeline, uint32_t groupIndex) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPipelineGetBindGroupLayout = @extern(*const fn (RenderPipeline, u32) callconv(.c) BindGroupLayout, .{ .name = "wgpuRenderPipelineGetBindGroupLayout" });
// void wgpuRenderPipelineSetLabel(WGPURenderPipeline renderPipeline, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPipelineSetLabel = @extern(*const fn (RenderPipeline, StringView) callconv(.c) void, .{ .name = "wgpuRenderPipelineSetLabel" });
// void wgpuRenderPipelineAddRef(WGPURenderPipeline renderPipeline) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPipelineAddRef = @extern(*const fn (RenderPipeline) callconv(.c) void, .{ .name = "wgpuRenderPipelineAddRef" });
// void wgpuRenderPipelineRelease(WGPURenderPipeline renderPipeline) WGPU_FUNCTION_ATTRIBUTE;
pub const renderPipelineRelease = @extern(*const fn (RenderPipeline) callconv(.c) void, .{ .name = "wgpuRenderPipelineRelease" });
// void wgpuSamplerSetLabel(WGPUSampler sampler, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const samplerSetLabel = @extern(*const fn (Sampler, StringView) callconv(.c) void, .{ .name = "wgpuSamplerSetLabel" });
// void wgpuSamplerAddRef(WGPUSampler sampler) WGPU_FUNCTION_ATTRIBUTE;
pub const samplerAddRef = @extern(*const fn (Sampler) callconv(.c) void, .{ .name = "wgpuSamplerAddRef" });
// void wgpuSamplerRelease(WGPUSampler sampler) WGPU_FUNCTION_ATTRIBUTE;
pub const samplerRelease = @extern(*const fn (Sampler) callconv(.c) void, .{ .name = "wgpuSamplerRelease" });
// WGPUFuture wgpuShaderModuleGetCompilationInfo(WGPUShaderModule shaderModule, WGPUCompilationInfoCallbackInfo callbackInfo) WGPU_FUNCTION_ATTRIBUTE;
pub const shaderModuleGetCompilationInfo = @extern(*const fn (ShaderModule, CompilationInfoCallbackInfo) callconv(.c) Future, .{ .name = "wgpuShaderModuleGetCompilationInfo" });
// void wgpuShaderModuleSetLabel(WGPUShaderModule shaderModule, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const shaderModuleSetLabel = @extern(*const fn (ShaderModule, StringView) callconv(.c) void, .{ .name = "wgpuShaderModuleSetLabel" });
// void wgpuShaderModuleAddRef(WGPUShaderModule shaderModule) WGPU_FUNCTION_ATTRIBUTE;
pub const shaderModuleAddRef = @extern(*const fn (ShaderModule) callconv(.c) void, .{ .name = "wgpuShaderModuleAddRef" });
// void wgpuShaderModuleRelease(WGPUShaderModule shaderModule) WGPU_FUNCTION_ATTRIBUTE;
pub const shaderModuleRelease = @extern(*const fn (ShaderModule) callconv(.c) void, .{ .name = "wgpuShaderModuleRelease" });
// void wgpuSupportedFeaturesFreeMembers(WGPUSupportedFeatures supportedFeatures) WGPU_FUNCTION_ATTRIBUTE;
pub const supportedFeaturesFreeMembers = @extern(*const fn (SupportedFeatures) callconv(.c) void, .{ .name = "wgpuSupportedFeaturesFreeMembers" });
// void wgpuSupportedWGSLLanguageFeaturesFreeMembers(WGPUSupportedWGSLLanguageFeatures supportedWGSLLanguageFeatures) WGPU_FUNCTION_ATTRIBUTE;
pub const supportedWGSLLanguageFeaturesFreeMembers = @extern(*const fn (SupportedWGSLLanguageFeatures) callconv(.c) void, .{ .name = "wgpuSupportedWGSLLanguageFeaturesFreeMembers" });
// void wgpuSurfaceConfigure(WGPUSurface surface, WGPUSurfaceConfiguration const * config) WGPU_FUNCTION_ATTRIBUTE;
pub const surfaceConfigure = @extern(*const fn (Surface, *const SurfaceConfiguration) callconv(.c) void, .{ .name = "wgpuSurfaceConfigure" });
// WGPUStatus wgpuSurfaceGetCapabilities(WGPUSurface surface, WGPUAdapter adapter, WGPUSurfaceCapabilities * capabilities) WGPU_FUNCTION_ATTRIBUTE;
pub const surfaceGetCapabilities = @extern(*const fn (Surface, Adapter, *SurfaceCapabilities) callconv(.c) Status, .{ .name = "wgpuSurfaceGetCapabilities" });
// void wgpuSurfaceGetCurrentTexture(WGPUSurface surface, WGPUSurfaceTexture * surfaceTexture) WGPU_FUNCTION_ATTRIBUTE;
pub const surfaceGetCurrentTexture = @extern(*const fn (Surface, *SurfaceTexture) callconv(.c) void, .{ .name = "wgpuSurfaceGetCurrentTexture" });
// WGPUStatus wgpuSurfacePresent(WGPUSurface surface) WGPU_FUNCTION_ATTRIBUTE;
pub const surfacePresent = @extern(*const fn (Surface) callconv(.c) Status, .{ .name = "wgpuSurfacePresent" });
// void wgpuSurfaceSetLabel(WGPUSurface surface, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const surfaceSetLabel = @extern(*const fn (Surface, StringView) callconv(.c) void, .{ .name = "wgpuSurfaceSetLabel" });
// void wgpuSurfaceUnconfigure(WGPUSurface surface) WGPU_FUNCTION_ATTRIBUTE;
pub const surfaceUnconfigure = @extern(*const fn (Surface) callconv(.c) void, .{ .name = "wgpuSurfaceUnconfigure" });
// void wgpuSurfaceAddRef(WGPUSurface surface) WGPU_FUNCTION_ATTRIBUTE;
pub const surfaceAddRef = @extern(*const fn (Surface) callconv(.c) void, .{ .name = "wgpuSurfaceAddRef" });
// void wgpuSurfaceRelease(WGPUSurface surface) WGPU_FUNCTION_ATTRIBUTE;
pub const surfaceRelease = @extern(*const fn (Surface) callconv(.c) void, .{ .name = "wgpuSurfaceRelease" });
// void wgpuSurfaceCapabilitiesFreeMembers(WGPUSurfaceCapabilities surfaceCapabilities) WGPU_FUNCTION_ATTRIBUTE;
pub const surfaceCapabilitiesFreeMembers = @extern(*const fn (SurfaceCapabilities) callconv(.c) void, .{ .name = "wgpuSurfaceCapabilitiesFreeMembers" });
// WGPUTextureView wgpuTextureCreateView(WGPUTexture texture, WGPU_NULLABLE WGPUTextureViewDescriptor const * descriptor) WGPU_FUNCTION_ATTRIBUTE;
pub const textureCreateView = @extern(*const fn (Texture, ?*const TextureViewDescriptor) callconv(.c) TextureView, .{ .name = "wgpuTextureCreateView" });
// void wgpuTextureDestroy(WGPUTexture texture) WGPU_FUNCTION_ATTRIBUTE;
pub const textureDestroy = @extern(*const fn (Texture) callconv(.c) void, .{ .name = "wgpuTextureDestroy" });
// uint32_t wgpuTextureGetDepthOrArrayLayers(WGPUTexture texture) WGPU_FUNCTION_ATTRIBUTE;
pub const textureGetDepthOrArrayLayers = @extern(*const fn (Texture) callconv(.c) u32, .{ .name = "wgpuTextureGetDepthOrArrayLayers" });
// WGPUTextureDimension wgpuTextureGetDimension(WGPUTexture texture) WGPU_FUNCTION_ATTRIBUTE;
pub const textureGetDimension = @extern(*const fn (Texture) callconv(.c) TextureDimension, .{ .name = "wgpuTextureGetDimension" });
// WGPUTextureFormat wgpuTextureGetFormat(WGPUTexture texture) WGPU_FUNCTION_ATTRIBUTE;
pub const textureGetFormat = @extern(*const fn (Texture) callconv(.c) TextureFormat, .{ .name = "wgpuTextureGetFormat" });
// uint32_t wgpuTextureGetHeight(WGPUTexture texture) WGPU_FUNCTION_ATTRIBUTE;
pub const textureGetHeight = @extern(*const fn (Texture) callconv(.c) u32, .{ .name = "wgpuTextureGetHeight" });
// uint32_t wgpuTextureGetMipLevelCount(WGPUTexture texture) WGPU_FUNCTION_ATTRIBUTE;
pub const textureGetMipLevelCount = @extern(*const fn (Texture) callconv(.c) u32, .{ .name = "wgpuTextureGetMipLevelCount" });
// uint32_t wgpuTextureGetSampleCount(WGPUTexture texture) WGPU_FUNCTION_ATTRIBUTE;
pub const textureGetSampleCount = @extern(*const fn (Texture) callconv(.c) u32, .{ .name = "wgpuTextureGetSampleCount" });
// WGPUTextureUsage wgpuTextureGetUsage(WGPUTexture texture) WGPU_FUNCTION_ATTRIBUTE;
pub const textureGetUsage = @extern(*const fn (Texture) callconv(.c) TextureUsage, .{ .name = "wgpuTextureGetUsage" });
// uint32_t wgpuTextureGetWidth(WGPUTexture texture) WGPU_FUNCTION_ATTRIBUTE;
pub const textureGetWidth = @extern(*const fn (Texture) callconv(.c) u32, .{ .name = "wgpuTextureGetWidth" });
// void wgpuTextureSetLabel(WGPUTexture texture, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const textureSetLabel = @extern(*const fn (Texture, StringView) callconv(.c) void, .{ .name = "wgpuTextureSetLabel" });
// void wgpuTextureAddRef(WGPUTexture texture) WGPU_FUNCTION_ATTRIBUTE;
pub const textureAddRef = @extern(*const fn (Texture) callconv(.c) void, .{ .name = "wgpuTextureAddRef" });
// void wgpuTextureRelease(WGPUTexture texture) WGPU_FUNCTION_ATTRIBUTE;
pub const textureRelease = @extern(*const fn (Texture) callconv(.c) void, .{ .name = "wgpuTextureRelease" });
// void wgpuTextureViewSetLabel(WGPUTextureView textureView, WGPUStringView label) WGPU_FUNCTION_ATTRIBUTE;
pub const textureViewSetLabel = @extern(*const fn (TextureView, StringView) callconv(.c) void, .{ .name = "wgpuTextureViewSetLabel" });
// void wgpuTextureViewAddRef(WGPUTextureView textureView) WGPU_FUNCTION_ATTRIBUTE;
pub const textureViewAddRef = @extern(*const fn (TextureView) callconv(.c) void, .{ .name = "wgpuTextureViewAddRef" });
// void wgpuTextureViewRelease(WGPUTextureView textureView) WGPU_FUNCTION_ATTRIBUTE;
pub const textureViewRelease = @extern(*const fn (TextureView) callconv(.c) void, .{ .name = "wgpuTextureViewRelease" });
// void wgpuGenerateReport(WGPUInstance instance, WGPUGlobalReport * report);
pub const generateReport = @extern(*const fn (Instance, *GlobalReport) callconv(.c) void, .{ .name = "wgpuGenerateReport" });
// size_t wgpuInstanceEnumerateAdapters(WGPUInstance instance, WGPU_NULLABLE WGPUInstanceEnumerateAdapterOptions const * options, WGPUAdapter * adapters);
pub const instanceEnumerateAdapters = @extern(*const fn (Instance, ?*const InstanceEnumerateAdapterOptions, [*]Adapter) callconv(.c) usize, .{ .name = "wgpuInstanceEnumerateAdapters" });
// WGPUSubmissionIndex wgpuQueueSubmitForIndex(WGPUQueue queue, size_t commandCount, WGPUCommandBuffer const * commands);
pub const queueSubmitForIndex = @extern(*const fn (Queue, usize, ?[*]const CommandBuffer) callconv(.c) SubmissionIndex, .{ .name = "wgpuQueueSubmitForIndex" });
// WGPUBool wgpuDevicePoll(WGPUDevice device, WGPUBool wait, WGPU_NULLABLE WGPUSubmissionIndex const * submissionIndex);
pub const devicePoll = @extern(*const fn (Device, BigBool, ?*const SubmissionIndex) callconv(.c) BigBool, .{ .name = "wgpuDevicePoll" });
// WGPUShaderModule wgpuDeviceCreateShaderModuleSpirV(WGPUDevice device, WGPUShaderModuleDescriptorSpirV const * descriptor);
pub const deviceCreateShaderModuleSpirV = @extern(*const fn (Device, *const ShaderModuleDescriptorSpirV) callconv(.c) ShaderModule, .{ .name = "wgpuDeviceCreateShaderModuleSpirV" });
// void wgpuSetLogCallback(WGPULogCallback callback, void * userdata);
pub const setLogCallback = @extern(*const fn (LogCallback, ?*anyopaque) callconv(.c) void, .{ .name = "wgpuSetLogCallback" });
// void wgpuSetLogLevel(WGPULogLevel level);
pub const setLogLevel = @extern(*const fn (LogLevel) callconv(.c) void, .{ .name = "wgpuSetLogLevel" });
// uint32_t wgpuGetVersion(void);
pub const getVersion = @extern(*const fn () callconv(.c) u32, .{ .name = "wgpuGetVersion" });
// void wgpuRenderPassEncoderSetPushConstants(WGPURenderPassEncoder encoder, WGPUShaderStage stages, uint32_t offset, uint32_t sizeBytes, void const * data);
pub const renderPassEncoderSetPushConstants = @extern(*const fn (RenderPassEncoder, ShaderStage, u32, u32, ?*const anyopaque) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderSetPushConstants" });
// void wgpuComputePassEncoderSetPushConstants(WGPUComputePassEncoder encoder, uint32_t offset, uint32_t sizeBytes, void const * data);
pub const computePassEncoderSetPushConstants = @extern(*const fn (ComputePassEncoder, u32, u32, ?*const anyopaque) callconv(.c) void, .{ .name = "wgpuComputePassEncoderSetPushConstants" });
// void wgpuRenderBundleEncoderSetPushConstants(WGPURenderBundleEncoder encoder, WGPUShaderStage stages, uint32_t offset, uint32_t sizeBytes, void const * data);
pub const renderBundleEncoderSetPushConstants = @extern(*const fn (RenderBundleEncoder, ShaderStage, u32, u32, ?*const anyopaque) callconv(.c) void, .{ .name = "wgpuRenderBundleEncoderSetPushConstants" });
// void wgpuRenderPassEncoderMultiDrawIndirect(WGPURenderPassEncoder encoder, WGPUBuffer buffer, uint64_t offset, uint32_t count);
pub const renderPassEncoderMultiDrawIndirect = @extern(*const fn (RenderPassEncoder, Buffer, u64, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderMultiDrawIndirect" });
// void wgpuRenderPassEncoderMultiDrawIndexedIndirect(WGPURenderPassEncoder encoder, WGPUBuffer buffer, uint64_t offset, uint32_t count);
pub const renderPassEncoderMultiDrawIndexedIndirect = @extern(*const fn (RenderPassEncoder, Buffer, u64, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderMultiDrawIndexedIndirect" });
// void wgpuRenderPassEncoderMultiDrawIndirectCount(WGPURenderPassEncoder encoder, WGPUBuffer buffer, uint64_t offset, WGPUBuffer count_buffer, uint64_t count_buffer_offset, uint32_t max_count);
pub const renderPassEncoderMultiDrawIndirectCount = @extern(*const fn (RenderPassEncoder, Buffer, u64, Buffer, u64, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderMultiDrawIndirectCount" });
// void wgpuRenderPassEncoderMultiDrawIndexedIndirectCount(WGPURenderPassEncoder encoder, WGPUBuffer buffer, uint64_t offset, WGPUBuffer count_buffer, uint64_t count_buffer_offset, uint32_t max_count);
pub const renderPassEncoderMultiDrawIndexedIndirectCount = @extern(*const fn (RenderPassEncoder, Buffer, u64, Buffer, u64, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderMultiDrawIndexedIndirectCount" });
// void wgpuComputePassEncoderBeginPipelineStatisticsQuery(WGPUComputePassEncoder computePassEncoder, WGPUQuerySet querySet, uint32_t queryIndex);
pub const computePassEncoderBeginPipelineStatisticsQuery = @extern(*const fn (ComputePassEncoder, QuerySet, u32) callconv(.c) void, .{ .name = "wgpuComputePassEncoderBeginPipelineStatisticsQuery" });
// void wgpuComputePassEncoderEndPipelineStatisticsQuery(WGPUComputePassEncoder computePassEncoder);
pub const computePassEncoderEndPipelineStatisticsQuery = @extern(*const fn (ComputePassEncoder) callconv(.c) void, .{ .name = "wgpuComputePassEncoderEndPipelineStatisticsQuery" });
// void wgpuRenderPassEncoderBeginPipelineStatisticsQuery(WGPURenderPassEncoder renderPassEncoder, WGPUQuerySet querySet, uint32_t queryIndex);
pub const renderPassEncoderBeginPipelineStatisticsQuery = @extern(*const fn (RenderPassEncoder, QuerySet, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderBeginPipelineStatisticsQuery" });
// void wgpuRenderPassEncoderEndPipelineStatisticsQuery(WGPURenderPassEncoder renderPassEncoder);
pub const renderPassEncoderEndPipelineStatisticsQuery = @extern(*const fn (RenderPassEncoder) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderEndPipelineStatisticsQuery" });
// void wgpuComputePassEncoderWriteTimestamp(WGPUComputePassEncoder computePassEncoder, WGPUQuerySet querySet, uint32_t queryIndex);
pub const computePassEncoderWriteTimestamp = @extern(*const fn (ComputePassEncoder, QuerySet, u32) callconv(.c) void, .{ .name = "wgpuComputePassEncoderWriteTimestamp" });
// void wgpuRenderPassEncoderWriteTimestamp(WGPURenderPassEncoder renderPassEncoder, WGPUQuerySet querySet, uint32_t queryIndex);
pub const renderPassEncoderWriteTimestamp = @extern(*const fn (RenderPassEncoder, QuerySet, u32) callconv(.c) void, .{ .name = "wgpuRenderPassEncoderWriteTimestamp" });
