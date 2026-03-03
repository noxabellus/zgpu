@group(0) @binding(0) var<uniform> projection_matrix: mat4x4<f32>;
@group(0) @binding(1) var linear_sampler: sampler;
@group(0) @binding(2) var nearest_sampler: sampler;
@group(0) @binding(3) var non_filtering_sampler: sampler;
@group(0) @binding(4) var atlas_texture: texture_2d_array<f32>;
@group(0) @binding(5) var<storage, read> image_mip_table: array<ImageMipData>;

@group(1) @binding(0) var picking_texture: texture_2d<f32>;

struct MipInfo {
    uv_rect: vec4<f32>,
    atlas_layer_index: u32,
};

const MAX_MIP_LEVELS = 12u;
struct ImageMipData {
    mips: array<MipInfo, MAX_MIP_LEVELS>,
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

const colors = array<vec3<f32>, 4>(
    vec3<f32>(0.0, 0.0, 0.0),
    vec3<f32>(1.0, 0.0, 0.0),
    vec3<f32>(0.0, 1.0, 0.0),
    vec3<f32>(0.0, 0.0, 1.0)
);

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

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    // 1. Evaluate SDF Geometry
    let d = sdRoundRect(in.local_pos, in.size, in.radii);
    var sdf_val = d;

    // 2. Smoothly Interpolated Border Thickness
    // border_thickness: x=top, y=right, z=bottom, w=left
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

    let sampled_data = textureSample(picking_texture, non_filtering_sampler, in.tex_coords);
    let id = u32(sampled_data.a);
    let color = colors[id % 4u];
    
    return vec4<f32>(color, alpha_factor);
}