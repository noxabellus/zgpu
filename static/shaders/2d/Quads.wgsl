@group(0) @binding(0) var<uniform> projection_matrix: mat4x4<f32>;
@group(0) @binding(1) var linear_sampler: sampler;
@group(0) @binding(2) var nearest_sampler: sampler;
@group(0) @binding(3) var atlas_texture: texture_2d_array<f32>;

struct MipInfo {
    uv_rect: vec4<f32>,
    atlas_layer_index: u32,
};

const MAX_MIP_LEVELS = 12u;
struct ImageMipData {
    mips: array<MipInfo, MAX_MIP_LEVELS>,
};

@group(0) @binding(4) var<storage, read> image_mip_table: array<ImageMipData>;

struct QuadInstance {
    @location(0) position: vec2<f32>,
    @location(1) size: vec2<f32>,
    @location(2) uv_min: vec2<f32>,
    @location(3) uv_max: vec2<f32>,
    @location(4) color: vec4<f32>,
    @location(5) radii: vec4<f32>, // tl, tr, br, bl
    @location(6) border_thickness: vec4<f32>, // top, right, bottom, left
    @location(7) edge_softness: f32,
    @location(8) encoded_params: u32,
};

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
    @location(1) color: vec4<f32>,
    @location(2) @interpolate(flat) encoded_params: u32,
    @location(3) local_pos: vec2<f32>,
    @location(4) size: vec2<f32>,
    @location(5) @interpolate(flat) radii: vec4<f32>,
    @location(6) @interpolate(flat) border_thickness: vec4<f32>,
    @location(7) @interpolate(flat) edge_softness: f32,
};

@vertex
fn vs_main(@builtin(vertex_index) v_idx: u32, instance: QuadInstance) -> VertexOutput {
    // Generate a unit quad
    var quad_uvs = array<vec2<f32>, 6>(
        vec2<f32>(0.0, 0.0), vec2<f32>(1.0, 0.0), vec2<f32>(1.0, 1.0),
        vec2<f32>(0.0, 0.0), vec2<f32>(1.0, 1.0), vec2<f32>(0.0, 1.0)
    );
    let uv = quad_uvs[v_idx];

    var out: VertexOutput;
    let world_pos = instance.position + uv * instance.size;
    
    out.clip_position = projection_matrix * vec4<f32>(world_pos, 0.0, 1.0);
    out.tex_coords = mix(instance.uv_min, instance.uv_max, uv);
    out.color = instance.color;
    out.encoded_params = instance.encoded_params;
    out.local_pos = uv * instance.size; // Pixel coordinates relative to top-left
    out.size = instance.size;
    out.radii = instance.radii;
    out.border_thickness = instance.border_thickness;
    out.edge_softness = instance.edge_softness;
    
    return out;
}

fn sdRoundRect(p: vec2<f32>, size: vec2<f32>, radii: vec4<f32>) -> f32 {
    let half_size = size * 0.5;
    let center_p = p - half_size;

    // Map screen quadrants to corner radii
    var r: f32;
    if (center_p.x > 0.0) {
        if (center_p.y > 0.0) { r = radii.z; } // Bottom-Right
        else                  { r = radii.y; } // Top-Right
    } else {
        if (center_p.y > 0.0) { r = radii.w; } // Bottom-Left
        else                  { r = radii.x; } // Top-Left
    }

    let q = abs(center_p) - half_size + r;
    return min(max(q.x, q.y), 0.0) + length(max(q, vec2<f32>(0.0))) - r;
}

const IMAGE_ID_MASK = 0x7FFFFFFFu;
const USE_NEAREST_MASK = 0x80000000u;

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    // 1. Evaluate SDF Geometry
    let d = sdRoundRect(in.local_pos, in.size, in.radii);
    var sdf_val = d;

    // 2. Smoothly Interpolated Border Thickness
    // border_thickness: x=top, y=right, z=bottom, w=left [cite: 28]
    if (any(in.border_thickness > vec4<f32>(0.0))) {
        // Calculate normalized coordinates from 0.0 to 1.0 across the quad
        let st = in.local_pos / in.size;

        // Calculate weights for each side. 
        // These are highest (1.0) at the edge and 0.0 at the opposite edge.
        let w_top    = 1.0 - st.y;
        let w_bottom = st.y;
        let w_left   = 1.0 - st.x;
        let w_right  = st.x;

        // Use a power function or smoothstep to sharpen the transition 
        // toward the corners, ensuring the correct thickness "wins" at the edge.
        let blend = pow(vec4<f32>(w_top, w_right, w_bottom, w_left), vec4<f32>(4.0));
        let total_w = blend.x + blend.y + blend.z + blend.w;
        
        // Final interpolated thickness
        let t = dot(in.border_thickness, blend) / total_w;

        // Apply thickness to the SDF
        sdf_val = abs(d + t * 0.5) - t * 0.5;
    }

    // Alpha Anti-Aliasing falloff
    let alpha_factor = 1.0 - smoothstep(-in.edge_softness, in.edge_softness, sdf_val);
    if (alpha_factor <= 0.0) { discard; }

    // 3. Sample Atlas (Identical to Triangles pipeline)
    let image_id = in.encoded_params & IMAGE_ID_MASK;
    let use_nearest = (in.encoded_params & USE_NEAREST_MASK) != 0u;

    let atlas_dims = vec2<f32>(textureDimensions(atlas_texture, 0).xy);
    let rect_mip0 = image_mip_table[image_id].mips[0u].uv_rect;
    let scale_texels = rect_mip0.zw * atlas_dims;

    let dx = dpdx(in.tex_coords * scale_texels);
    let dy = dpdy(in.tex_coords * scale_texels);
    let p = max(dot(dx, dx), dot(dy, dy));
    let lod = 0.5 * log2(max(p, 1e-8));

    var sampled_color: vec4<f32>;

    if (use_nearest) {
        let nearest_lod_index = u32(round(clamp(lod, 0.0, f32(MAX_MIP_LEVELS - 1u))));
        let info = image_mip_table[image_id].mips[nearest_lod_index];
        let coords_2d = info.uv_rect.xy + in.tex_coords * info.uv_rect.zw;
        sampled_color = textureSampleLevel(atlas_texture, nearest_sampler, coords_2d, info.atlas_layer_index, 0.0);
    } else {
        let clamped_lod = clamp(lod, 0.0, f32(MAX_MIP_LEVELS - 1u) - 0.001);
        let lod_fract = fract(clamped_lod);
        let info_floor = image_mip_table[image_id].mips[u32(floor(clamped_lod))];
        let info_ceil = image_mip_table[image_id].mips[u32(ceil(clamped_lod))];

        let coords_2d_floor = info_floor.uv_rect.xy + in.tex_coords * info_floor.uv_rect.zw;
        let coords_2d_ceil = info_ceil.uv_rect.xy + in.tex_coords * info_ceil.uv_rect.zw;

        let color_floor = textureSampleLevel(atlas_texture, linear_sampler, coords_2d_floor, info_floor.atlas_layer_index, 0.0);
        let color_ceil = textureSampleLevel(atlas_texture, linear_sampler, coords_2d_ceil, info_ceil.atlas_layer_index, 0.0);
        sampled_color = mix(color_floor, color_ceil, lod_fract);
    }

    // 4. Blend Alpha & Tint
    var final_color = sampled_color * in.color;
    final_color.a *= alpha_factor;
    final_color.r *= final_color.a;
    final_color.g *= final_color.a;
    final_color.b *= final_color.a;

    return final_color;
}