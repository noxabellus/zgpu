struct Uniforms {
    projection: mat4x4<f32>,
};

@group(0) @binding(0) var<uniform> u_uniforms: Uniforms;
@group(0) @binding(1) var s_sampler: sampler;
@group(0) @binding(2) var t_texture: texture_2d<f32>;

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) tex_coords: vec2<f32>,
    @location(2) color: vec4<f32>,
    // atlas_index is used by the CPU for batching and is not needed in the shader.
    @location(3) atlas_index: u32,
};

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
    @location(1) color: vec4<f32>,
};

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.clip_position = u_uniforms.projection * vec4<f32>(in.position, 0.0, 1.0);
    out.tex_coords = in.tex_coords;
    out.color = in.color;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let texture_color = textureSample(t_texture, s_sampler, in.tex_coords);
    return in.color * texture_color;
}