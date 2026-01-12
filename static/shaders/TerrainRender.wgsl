struct CameraUniform {
    view_proj: mat4x4<f32>,
    cursor_pos: vec2<u32>,
};
@group(0) @binding(0) var<uniform> camera: CameraUniform;

struct PackedVertex {
    pos_norm: u32,
    data: u32,
};
@group(0) @binding(1) var<storage, read> verts: array<PackedVertex>;

struct VertexOutput {
    @builtin(position) clip_pos: vec4<f32>,
    @location(0) color: vec3<f32>,
};

const normals = array<vec3<f32>, 5>(
    vec3<f32>(0.0, 1.0, 0.0), // Up
    vec3<f32>(1.0, 0.0, 0.0), // Right
    vec3<f32>(-1.0, 0.0, 0.0), // Left
    vec3<f32>(0.0, 0.0, -1.0), // Back
    vec3<f32>(0.0, 0.0, 1.0)   // Front
);

@vertex
fn vs_main(@builtin(vertex_index) id: u32) -> VertexOutput {
    let p = verts[id];
    
    // Unpack Position
    let x = f32(p.pos_norm & 0xFFu);
    let y = f32((p.pos_norm >> 8u) & 0xFFu);
    let z = f32((p.pos_norm >> 16u) & 0xFFu);
    let n_idx = (p.pos_norm >> 24u) & 0xFFu;
    
    let world_pos = vec3<f32>(x, z, y); 
    
    // Lighting
    let normal = normals[n_idx];
    let light_dir = normalize(vec3<f32>(0.5, 1.0, -0.5));
    let diff = max(dot(normal, light_dir), 0.3);
    
    var base_color = vec3<f32>(0.2, 0.8, 0.2); // Grass
    if (n_idx != 0u) { base_color = vec3<f32>(0.5, 0.4, 0.3); } // Dirt

    // --- HIGHLIGHT LOGIC ---
    // Unpack Tile ID
    let tile_x = p.data & 0xFFFFu;
    let tile_y = (p.data >> 16u) & 0xFFFFu;

    if (tile_x == camera.cursor_pos.x && tile_y == camera.cursor_pos.y) {
        // Apply a brightness boost / yellow tint for selection
        base_color = base_color + vec3<f32>(0.3, 0.3, 0.0);
    }
    
    var out: VertexOutput;
    out.clip_pos = camera.view_proj * vec4<f32>(world_pos, 1.0);
    out.color = base_color * diff;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    return vec4<f32>(in.color, 1.0);
}