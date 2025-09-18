struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
};

@vertex
fn vs_main(@builtin(vertex_index) in_vertex_index: u32) -> VertexOutput {
    var out: VertexOutput;
    // A hardcoded array of vertex positions for a quad (two triangles).
    let positions = array<vec2<f32>, 6>(
        vec2<f32>(-1.0, -1.0), // Triangle 1
        vec2<f32>( 1.0, -1.0),
        vec2<f32>(-1.0,  1.0),
        vec2<f32>(-1.0,  1.0), // Triangle 2
        vec2<f32>( 1.0, -1.0),
        vec2<f32>( 1.0,  1.0)
    );
    // A hardcoded array of UV coordinates that correspond to the vertex positions.
    let uvs = array<vec2<f32>, 6>(
        vec2<f32>(0.0, 1.0), // Corresponds to bottom-left
        vec2<f32>(1.0, 1.0), // Corresponds to bottom-right
        vec2<f32>(0.0, 0.0), // Corresponds to top-left
        vec2<f32>(0.0, 0.0), // Corresponds to top-left
        vec2<f32>(1.0, 1.0), // Corresponds to bottom-right
        vec2<f32>(1.0, 0.0)  // Corresponds to top-right
    );
    
    out.clip_position = vec4<f32>(positions[in_vertex_index], 0.0, 1.0);
    out.tex_coords = uvs[in_vertex_index];
    return out;
}

// These bindings must match the bind group layout and bind group in the Zig code.
@group(0) @binding(0)
var t_sampler: sampler;

@group(0) @binding(1)
var t_texture: texture_2d<f32>;

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    // Sample the texture at the interpolated UV coordinates.
    return textureSample(t_texture, t_sampler, in.tex_coords);
}