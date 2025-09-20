@group(0) @binding(0)
var<uniform> u_projection: mat4x4<f32>;

@group(0) @binding(1)
var t_sampler: sampler;

@group(0) @binding(2)
var t_texture: texture_2d_array<f32>;

// Updated MipInfo struct with layer index and padding
struct MipInfo {
    uv_rect: vec4<f32>, // x, y, width, height
    atlas_layer_index: u32,
    // Note: padding isn't declared in WGSL, but the host-side
    // struct must have it to ensure correct stride in the buffer.
};

const MAX_MIP_LEVELS = 12u;
struct ImageMipData {
    mips: array<MipInfo, MAX_MIP_LEVELS>,
};

@group(0) @binding(3)
var<storage, read> image_mip_table: array<ImageMipData>;

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) tex_coords: vec2<f32>,
    @location(2) color: vec4<f32>,
    @location(3) @interpolate(flat) encoded_params: u32,
};

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
    @location(1) color: vec4<f32>,
    @location(2) @interpolate(flat) encoded_params: u32,
};

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.clip_position = u_projection * vec4<f32>(in.position, 0.0, 1.0);
    out.tex_coords = in.tex_coords;
    out.color = in.color;
    out.encoded_params = in.encoded_params;
    return out;
}

const IMAGE_ID_MASK = 0x7FFFFFFFu;

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let image_id = in.encoded_params & IMAGE_ID_MASK;

    // 1. Calculate the required Level of Detail (LOD) using screen-space derivatives.
    let atlas_dims = vec2<f32>(textureDimensions(t_texture, 0).xy);
    let rect_mip0 = image_mip_table[image_id].mips[0u].uv_rect;
    let scale_texels = rect_mip0.zw * atlas_dims;

    let dx = dpdx(in.tex_coords * scale_texels);
    let dy = dpdy(in.tex_coords * scale_texels);
    let p = max(dot(dx, dx), dot(dy, dy));
    let lod = 0.5 * log2(max(p, 1e-8));

    // 2. Clamp the LOD and find the two nearest mip levels to blend between.
    let clamped_lod = clamp(lod, 0.0, f32(MAX_MIP_LEVELS - 1u) - 0.001);
    let lod_floor = floor(clamped_lod);
    let lod_ceil = ceil(clamped_lod);
    let lod_fract = fract(clamped_lod);

    // 3. Fetch the indirection info (UV rect and layer index) for both mip levels.
    let info_floor = image_mip_table[image_id].mips[u32(lod_floor)];
    let info_ceil = image_mip_table[image_id].mips[u32(lod_ceil)];

    // 4. Calculate the 2D texture coordinates for each sample.
    let coords_2d_floor = info_floor.uv_rect.xy + in.tex_coords * info_floor.uv_rect.zw;
    let coords_2d_ceil = info_ceil.uv_rect.xy + in.tex_coords * info_ceil.uv_rect.zw;

    // 5. Sample from both layers using the correct 5-argument function signature.
    // textureSampleLevel(texture, sampler, coords_2d, array_index, level)
    let color_floor = textureSampleLevel(t_texture, t_sampler, coords_2d_floor, info_floor.atlas_layer_index, 0.0);
    let color_ceil = textureSampleLevel(t_texture, t_sampler, coords_2d_ceil, info_ceil.atlas_layer_index, 0.0);

    // 6. Manually interpolate between the two samples to achieve trilinear filtering.
    let sampled_color = mix(color_floor, color_ceil, lod_fract);

    // 7. Apply tint and convert to pre-multiplied alpha for correct blending.
    var final_color = sampled_color * in.color;
    // you can't *= swizzles in WGSL, do it component-wise
    final_color.r *= final_color.a;
    final_color.g *= final_color.a;
    final_color.b *= final_color.a;

    return final_color;
}