@group(0) @binding(0) var<uniform> view_projection: mat4x4<f32>;

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) normal: vec3<f32>,
    @location(2) color: vec4<f32>,
    @location(3) id: u32,
};

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) world_position: vec3<f32>,
    @location(1) normal: vec3<f32>,
    @location(2) color: vec4<f32>,
    @location(3) @interpolate(flat) id: u32,
}

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    out.clip_position = view_projection * vec4<f32>(in.position, 1.0);
    out.world_position = in.position;
    // TODO: For uniform scaling, we can just pass the normal through; Later, we may want to add a normal matrix.
    out.normal = in.normal;
    out.color = in.color;
    out.id = in.id;
    return out;
}

@fragment
fn fs_shaded(in: VertexOutput) -> @location(0) vec4<f32> {
    // Hardcoded directional light
    let light_dir = normalize(vec3<f32>(0.5, 1.0, -0.3));
    let ambient_strength = 0.3;
    
    // Normalize in the fragment shader for smoother interpolation
    let norm = normalize(in.normal);
    let diff = max(dot(norm, light_dir), 0.0);
    
    let lighting = ambient_strength + diff;
    let final_color = in.color.rgb * lighting;
    
    return vec4<f32>(final_color, in.color.a);
}

@fragment
fn fs_wireframe(in: VertexOutput) -> @location(0) vec4<f32> {
    return in.color;
}

@fragment
fn fs_picking(in: VertexOutput) -> @location(0) vec4<f32> {
    return vec4<f32>(in.world_position, f32(in.id));
}