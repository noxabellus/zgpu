// This struct must match the VertexBufferLayout in the Zig code.
struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) tex_coords: vec2<f32>,
};

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) tex_coords: vec2<f32>,
};

// This struct must match the Uniforms struct in the Zig code.
struct Uniforms {
    resolution: vec2<f32>,
};

// This binding must match the bind group layout in the Zig code.
@group(0) @binding(2) var<uniform> u_uniforms: Uniforms;

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;

    // Convert pixel coordinates to normalized device coordinates (NDC),
    // then to clip space. WebGPU's clip space is X from -1 to 1 (left to right)
    // and Y from -1 to 1 (bottom to top).
    // Our pixel coordinates start at (0,0) in the top-left.
    // 1. Normalize position to 0.0-1.0 range: `in.position / u_uniforms.resolution`
    // 2. Convert to -1.0 to 1.0 range: `* 2.0 - 1.0`
    // 3. Flip the Y-axis because pixel Y is down, clip space Y is up.
    let normalized_pos = in.position / u_uniforms.resolution;
    let screen_space_pos = normalized_pos * vec2<f32>(2.0, -2.0) + vec2<f32>(-1.0, 1.0);
    
    out.clip_position = vec4<f32>(screen_space_pos, 0.0, 1.0);
    out.tex_coords = in.tex_coords;
    return out;
}

// These texture/sampler bindings are unchanged from the previous shader.
@group(0) @binding(0)
var t_sampler: sampler;

@group(0) @binding(1)
var t_texture: texture_2d<f32>;

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return textureSample(t_texture, t_sampler, in.tex_coords);
}