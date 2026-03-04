@group(0) @binding(0) var<uniform> projection_matrix: mat4x4<f32>;
@group(0) @binding(1) var linear_sampler: sampler;
@group(0) @binding(2) var nearest_sampler: sampler;
@group(0) @binding(3) var non_filtering_sampler: sampler;
@group(0) @binding(4) var atlas_texture: texture_2d_array<f32>;
@group(0) @binding(5) var<storage, read> image_mip_table: array<ImageMipData>;

struct CustomUniforms {
    mouse_pos: vec2<f32>,
    _padding: vec2<f32>,
};
@group(1) @binding(0) var<uniform> custom_uniforms: CustomUniforms;
@group(1) @binding(1) var picking_texture: texture_2d<f32>;

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

const colors = array<vec3<f32>, 32>(
    vec3<f32>(0.0, 0.0, 0.0),
    vec3<f32>(1.0, 0.0, 0.0),
    vec3<f32>(0.0, 1.0, 0.0),
    vec3<f32>(0.0, 0.0, 1.0),

    vec3<f32>(1.0, 1.0, 0.0),
    vec3<f32>(1.0, 0.0, 1.0),
    vec3<f32>(0.0, 1.0, 1.0),
    vec3<f32>(1.0, 1.0, 1.0),

    vec3<f32>(0.5, 0.0, 0.0),
    vec3<f32>(0.0, 0.5, 0.0),
    vec3<f32>(0.0, 0.0, 0.5),
    vec3<f32>(0.5, 0.5, 0.0),

    vec3<f32>(0.5, 0.0, 0.5),
    vec3<f32>(0.0, 0.5, 0.5),
    vec3<f32>(0.5, 0.5, 0.5),
    vec3<f32>(0.75, 0.0, 0.0),

    vec3<f32>(0.0, 0.75, 0.0),
    vec3<f32>(0.0, 0.0, 0.75),
    vec3<f32>(0.75, 0.75, 0.0),
    vec3<f32>(0.75, 0.0, 0.75),

    vec3<f32>(0.0, 0.75, 0.75),
    vec3<f32>(0.75, 0.75, 0.75),
    vec3<f32>(0.25, 0.0, 0.0),
    vec3<f32>(0.0, 0.25, 0.0),

    vec3<f32>(0.0, 0.0, 0.25),
    vec3<f32>(0.25, 0.25, 0.0),
    vec3<f32>(0.25, 0.0, 0.25),
    vec3<f32>(0.0, 0.25, 0.25),

    vec3<f32>(0.25, 0.25, 0.25),
    vec3<f32>(1.0, 0.5, 0.0),
    vec3<f32>(1.0, 0.0, 0.5),
    vec3<f32>(0.5, 1.0, 0.0)
);

fn sdRoundRect(p: vec2<f32>, size: vec2<f32>, radii: vec4<f32>) -> f32 {
    let half_size = size * 0.5;
    let center_p = p - half_size;

    var r: f32;
    if (center_p.x > 0.0) {
        if (center_p.y > 0.0) { r = radii.z; } 
        else                  { r = radii.y; } 
    } else {
        if (center_p.y > 0.0) { r = radii.w; } 
        else                  { r = radii.x; } 
    }

    let q = abs(center_p) - half_size + r;
    return min(max(q.x, q.y), 0.0) + length(max(q, vec2<f32>(0.0))) - r;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let d = sdRoundRect(in.local_pos, in.size, in.radii);
    var sdf_val = d;

    if (any(in.border_thickness > vec4<f32>(0.0))) {
        let st = in.local_pos / in.size;
        let w_top    = 1.0 - st.y;
        let w_bottom = st.y;
        let w_left   = 1.0 - st.x;
        let w_right  = st.x;

        let blend = pow(vec4<f32>(w_top, w_right, w_bottom, w_left), vec4<f32>(4.0));
        let total_w = blend.x + blend.y + blend.z + blend.w;
        let t = dot(in.border_thickness, blend) / total_w;
        sdf_val = abs(d + t * 0.5) - t * 0.5;
    }

    let alpha_factor = 1.0 - smoothstep(-in.edge_softness, in.edge_softness, sdf_val);
    if (alpha_factor <= 0.0) { discard; }

    let sampled_data = textureSample(picking_texture, non_filtering_sampler, in.tex_coords);
    let id = u32(sampled_data.a);
    var final_color = colors[id % 32u];
    
    // --- TEXTURE SPACE MOUSE LOGIC ---
    // Get the actual pixel dimensions of the underlying render target
    let tex_dims = vec2<f32>(textureDimensions(picking_texture));
    // Map the 0..1 UV coordinate back into the texture's native pixel space
    let tex_pixel_pos = in.tex_coords * tex_dims;
    
    // Compare against the uniform, which should now represent the mouse in the unscaled screen space
    let dist_to_mouse = distance(tex_pixel_pos, custom_uniforms.mouse_pos);
    
    if (abs(dist_to_mouse - 10.0) < 2.0) {
        final_color = vec3<f32>(1.0, 0.0, 0.0);
    }
    
    return vec4<f32>(final_color, alpha_factor);
}