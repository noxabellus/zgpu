@group(0) @binding(0)
var<uniform> u_projection: mat4x4<f32>;

@group(0) @binding(1)
var t_sampler: sampler;

@group(0) @binding(2)
var t_texture: texture_2d<f32>;

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) tex_coords: vec2<f32>,
    @location(2) color: vec4<f32>,
    @location(3) @interpolate(flat) encoded_atlas_index: u32,
};

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
    @location(1) color: vec4<f32>,
};

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.clip_position = u_projection * vec4<f32>(in.position, 0.0, 1.0);
    out.tex_coords = in.tex_coords;
    out.color = in.color;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    var color = textureSample(t_texture, t_sampler, in.tex_coords) * in.color;

    // Here, color.rgb is in LINEAR space because the srgb texture sampler
    // has already converted it. This is the correct space to do math.
    // We now multiply by alpha to create the premultiplied-alpha color
    // that our blend state expects.
    color.r *= color.a;
    color.g *= color.a;
    color.b *= color.a;

    return color;
}