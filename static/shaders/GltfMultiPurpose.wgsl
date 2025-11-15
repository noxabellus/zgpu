struct CameraUniform {
    view_proj: mat4x4<f32>,
};

@group(0) @binding(0)
var<uniform> u_camera: CameraUniform;

@group(1) @binding(0)
var t_diffuse: texture_2d<f32>;

@group(1) @binding(1)
var s_diffuse: sampler;

struct SkinUniforms {
    joint_matrices: array<mat4x4<f32>, 128>, // Must match MAX_JOINTS
};

@group(2) @binding(0)
var<uniform> u_skin: SkinUniforms;

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) normal: vec3<f32>,
    @location(2) color: vec4<f32>,
    @location(3) tex_coord: vec2<f32>,
    @location(4) joint_indices: vec4<u32>,
    @location(5) joint_weights: vec4<f32>,
};

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) normal: vec3<f32>,
    @location(1) color: vec4<f32>,
    @location(2) tex_coord: vec2<f32>,
};

@vertex
fn vs_main(model: VertexInput) -> VertexOutput {
    var out: VertexOutput;

    var skin_matrix = mat4x4<f32>(
        vec4<f32>(0.0, 0.0, 0.0, 0.0),
        vec4<f32>(0.0, 0.0, 0.0, 0.0),
        vec4<f32>(0.0, 0.0, 0.0, 0.0),
        vec4<f32>(0.0, 0.0, 0.0, 0.0),
    );
    
    // Add the influence of each of the 4 joints.
    // Note: This assumes joint_weights sum to 1.0, which they should.
    // If a primitive is not skinned, joint_indices will be 0 and weights will be (1,0,0,0),
    // effectively using the matrix of the root joint (which will be the node's world transform).
    let i0 = model.joint_indices[0];
    let i1 = model.joint_indices[1];
    let i2 = model.joint_indices[2];
    let i3 = model.joint_indices[3];

    let w0 = model.joint_weights[0];
    let w1 = model.joint_weights[1];
    let w2 = model.joint_weights[2];
    let w3 = model.joint_weights[3];

    skin_matrix += w0 * u_skin.joint_matrices[i0];
    skin_matrix += w1 * u_skin.joint_matrices[i1];
    skin_matrix += w2 * u_skin.joint_matrices[i2];
    skin_matrix += w3 * u_skin.joint_matrices[i3];
    
    // Transform position and normal by the final skin matrix.
    let world_position = skin_matrix * vec4<f32>(model.position, 1.0);
    out.clip_position = u_camera.view_proj * world_position;
    
    // Normals need to be transformed by the inverse transpose of the
    // upper 3x3 of the skin matrix to handle non-uniform scaling correctly.
    // For simplicity here, we assume uniform scaling and just use the 3x3.
    let normal_matrix = mat3x3<f32>(
        skin_matrix[0].xyz,
        skin_matrix[1].xyz,
        skin_matrix[2].xyz
    );
    out.normal = normalize(normal_matrix * model.normal);

    out.color = model.color;
    out.tex_coord = model.tex_coord;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    let light_dir = normalize(vec3<f32>(0.5, 1.0, 0.75));
    let light_color = vec3<f32>(1.0, 1.0, 1.0);
    let light_intensity = 1.0;
    let texture_color = textureSample(t_diffuse, s_diffuse, in.tex_coord);
    let base_color = texture_color.rgb * in.color.rgb;
    let normal = normalize(in.normal);
    let diffuse_strength = max(dot(normal, light_dir), 0.0);
    let diffuse_contribution = base_color * light_color * light_intensity * diffuse_strength;
    let ambient_color = vec3<f32>(0.1, 0.1, 0.1);
    let ambient_contribution = base_color * ambient_color;
    let final_color = ambient_contribution + diffuse_contribution;
    return vec4<f32>(final_color, texture_color.a);
}