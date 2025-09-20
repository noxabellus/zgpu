@group(0) @binding(0)
var<uniform> projection_matrix: mat4x4<f32>;

@group(0) @binding(1)
var linear_sampler: sampler;

@group(0) @binding(2)
var nearest_sampler: sampler;

@group(0) @binding(3)
var atlas_texture: texture_2d_array<f32>;

// Updated MipInfo struct with layer index and padding
struct MipInfo {
    uv_rect: vec4<f32>, // x, y, width, height
    atlas_layer_index: u32,
    // Note: padding isn't declared in WGSL, but the host-side
    // struct must have it to ensure correct stride in the buffer.
};

// Must be kept in sync with Atlas.zig
const MAX_MIP_LEVELS = 12u;
struct ImageMipData {
    mips: array<MipInfo, MAX_MIP_LEVELS>,
};

@group(0) @binding(4)
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
    out.clip_position = projection_matrix * vec4<f32>(in.position, 0.0, 1.0);
    out.tex_coords = in.tex_coords;
    out.color = in.color;
    out.encoded_params = in.encoded_params;
    return out;
}

// Must be kept in sync with Batch2D.zig
const IMAGE_ID_MASK = 0x7FFFFFFFu;
const IS_GLYPH_MASK = 0x80000000u;

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let image_id = in.encoded_params & IMAGE_ID_MASK;
    let is_glyph = (in.encoded_params & IS_GLYPH_MASK) != 0u;

    let atlas_dims = vec2<f32>(textureDimensions(atlas_texture, 0).xy);
    let rect_mip0 = image_mip_table[image_id].mips[0u].uv_rect;
    let scale_texels = rect_mip0.zw * atlas_dims;

    let dx = dpdx(in.tex_coords * scale_texels);
    let dy = dpdy(in.tex_coords * scale_texels);
    let p = max(dot(dx, dx), dot(dy, dy));
    let lod = 0.5 * log2(max(p, 1e-8));

    var sampled_color: vec4<f32>;

    if (is_glyph) {
        // --- NEAREST PATH (for text) ---
        // 1. Clamp the LOD and round to the nearest integer.
        let nearest_lod_index = u32(round(clamp(lod, 0.0, f32(MAX_MIP_LEVELS - 1u))));

        // 2. Fetch the indirection info for that single mip level.
        let info = image_mip_table[image_id].mips[nearest_lod_index];
        let coords_2d = info.uv_rect.xy + in.tex_coords * info.uv_rect.zw;

        // 3. Sample using the nearest sampler.
        sampled_color = textureSampleLevel(atlas_texture, nearest_sampler, coords_2d, info.atlas_layer_index, 0.0);
    } else {
        // --- LINEAR PATH (for images) ---
        // This is the original trilinear filtering logic.
        let clamped_lod = clamp(lod, 0.0, f32(MAX_MIP_LEVELS - 1u) - 0.001);
        let lod_floor = floor(clamped_lod);
        let lod_ceil = ceil(clamped_lod);
        let lod_fract = fract(clamped_lod);

        let info_floor = image_mip_table[image_id].mips[u32(lod_floor)];
        let info_ceil = image_mip_table[image_id].mips[u32(lod_ceil)];

        let coords_2d_floor = info_floor.uv_rect.xy + in.tex_coords * info_floor.uv_rect.zw;
        let coords_2d_ceil = info_ceil.uv_rect.xy + in.tex_coords * info_ceil.uv_rect.zw;

        let color_floor = textureSampleLevel(atlas_texture, linear_sampler, coords_2d_floor, info_floor.atlas_layer_index, 0.0);
        let color_ceil = textureSampleLevel(atlas_texture, linear_sampler, coords_2d_ceil, info_ceil.atlas_layer_index, 0.0);

        sampled_color = mix(color_floor, color_ceil, lod_fract);
    }

    // Apply tint and convert to pre-multiplied alpha for correct blending.
    var final_color = sampled_color * in.color;
    final_color.r *= final_color.a;
    final_color.g *= final_color.a;
    final_color.b *= final_color.a;

    return final_color;
}