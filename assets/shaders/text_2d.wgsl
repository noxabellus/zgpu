/// Represents the output of the vertex shader and the input to the fragment shader.
struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
};

/// Uniforms that are constant for every vertex in a draw call.
struct Uniforms {
    resolution: vec2<f32>,
};

@group(0) @binding(2) var<uniform> u_uniforms: Uniforms;

@vertex
fn vs_main(
    @location(0) position: vec2<f32>,
    @location(1) tex_coords: vec2<f32>
) -> VertexOutput {
    var out: VertexOutput;
    // Convert pixel coordinates to normalized device coordinates (-1.0 to 1.0)
    let scaled_pos = position / u_uniforms.resolution * 2.0 - 1.0;
    // WGPU's NDC has Y pointing down, so we flip it.
    out.clip_position = vec4<f32>(scaled_pos.x, -scaled_pos.y, 0.0, 1.0);
    out.tex_coords = tex_coords;
    return out;
}

// --- Fragment Shader ---

@group(0) @binding(0) var t_sampler: sampler;
@group(0) @binding(1) var t_texture: texture_2d<f32>;

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    // Sample the single-channel font texture. The alpha value is in the .r component.
    let alpha = textureSample(t_texture, t_sampler, in.tex_coords).r;

    // Output white, using the sampled value as the final alpha.
    // This allows the blending stage to correctly composite the text.
    return vec4<f32>(1.0, 1.0, 1.0, alpha);
}