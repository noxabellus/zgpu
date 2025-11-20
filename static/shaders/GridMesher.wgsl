struct IndirectArgs {
    vertex_count: atomic<u32>,
    instance_count: u32,
    first_vertex: u32,
    first_instance: u32,
};
    
struct PackedVertex {
    // x,y,z (8 bits each) + normal_id (3 bits) + unused
    pos_norm: u32, 
    // material_id (16 bits) + ao (something else)
    data: u32, 
};
    
// Bindings
@group(0) @binding(0) var<storage, read_write> indirect_draw: IndirectArgs;
@group(0) @binding(1) var<storage, read_write> vertex_pool: array<PackedVertex>;
@group(0) @binding(2) var<storage, read> voxel_pool: array<u32>; // The raw voxel data
    
// Helper to pack data
fn pack_vertex(norm: u32, v: vec3<u32>) -> u32 {
    return (v.x) | (v.y << 8u) | (v.z << 16u) | (norm << 24u);
}

fn voxel_index(id: vec3<u32>) -> u32 {
    return id.x + (id.y * 16u) + (id.z * 256u);
}

fn get_voxel(id: vec3<u32>) -> u32 {
    return voxel_pool[voxel_index(id)];
}

struct FaceData {
    norm: u32,
    neighbor: vec3<i32>,
    v1: vec3<u32>,
    v2: vec3<u32>,
    v3: vec3<u32>,
    v4: vec3<u32>,
}

const face_data = array<FaceData, 6>(
    FaceData(0u, vec3<i32>(1, 0, 0), // +X
        vec3<u32>(1, 0, 0), vec3<u32>(1, 1, 0), vec3<u32>(1, 1, 1), vec3<u32>(1, 0, 1)),
    FaceData(1u, vec3<i32>(-1, 0, 0), // -X
        vec3<u32>(0, 0, 1), vec3<u32>(0, 1, 1), vec3<u32>(0, 1, 0), vec3<u32>(0, 0, 0)),
    FaceData(2u, vec3<i32>(0, 1, 0), // +Y
        vec3<u32>(0, 1, 0), vec3<u32>(0, 1, 1), vec3<u32>(1, 1, 1), vec3<u32>(1, 1, 0)),
    FaceData(3u, vec3<i32>(0, -1, 0), // -Y
        vec3<u32>(1, 0, 0), vec3<u32>(1, 0, 1), vec3<u32>(0, 0, 1), vec3<u32>(0, 0, 0)),
    FaceData(4u, vec3<i32>(0, 0, 1), // +Z
        vec3<u32>(1, 0, 1), vec3<u32>(1, 1, 1), vec3<u32>(0, 1, 1), vec3<u32>(0, 0, 1)),
    FaceData(5u, vec3<i32>(0, 0, -1), // -Z
        vec3<u32>(0, 0, 0), vec3<u32>(0, 1, 0), vec3<u32>(1, 1, 0), vec3<u32>(1, 0, 0)),
);
    
@compute @workgroup_size(4, 4, 4) // 64 threads per group
fn main(@builtin(global_invocation_id) id: vec3<u32>) {
    // Hardcoded for this demo: We are meshing Brick #0 at World Pos (0,0,0)
    // In a real engine, we'd look up the BrickMap here.
    
    if (id.x >= 16u || id.y >= 16u || id.z >= 16u) { return; }
    
    // Flatten index for 16^3 brick
    let voxel_data = get_voxel(id);
    
    // 0 is empty
    if (voxel_data == 0u) { return; }
    
    // SIMPLE FACE CULLING (Naive)
    // Real implementation: Check neighbor voxels in pool. 
    // For this demo, we just emit a cube for every solid voxel to prove the pipeline.
    
    // Allocate space for 36 vertices (6 faces * 2 tris * 3 verts)
    let start_idx = atomicAdd(&indirect_draw.vertex_count, 36u);
    

    for (var f = 0u; f < 6; f++) {
        let face = face_data[f];

        let norm = face.norm;

        if (!((face.neighbor.x == -1 && id.x ==  0u)
           || (face.neighbor.y == -1 && id.y ==  0u)
           || (face.neighbor.z == -1 && id.z ==  0u)
           || (face.neighbor.x ==  1 && id.x == 15u)
           || (face.neighbor.y ==  1 && id.y == 15u)
           || (face.neighbor.z ==  1 && id.z == 15u)
        )) {
            let neighbor_pos = vec3<u32>(u32(i32(id.x) + face.neighbor.x), u32(i32(id.y) + face.neighbor.y), u32(i32(id.z) + face.neighbor.z));

            if (get_voxel(neighbor_pos) != 0u) {
                continue; // Neighbor is solid, skip this face
            }
        }

        let v1 = pack_vertex(norm, id + face.v1);
        let v2 = pack_vertex(norm, id + face.v2);
        let v3 = pack_vertex(norm, id + face.v3);
        let v4 = pack_vertex(norm, id + face.v4);
        
        vertex_pool[start_idx + f * 6u + 0u] = PackedVertex(v1, voxel_data);
        vertex_pool[start_idx + f * 6u + 1u] = PackedVertex(v2, voxel_data);
        vertex_pool[start_idx + f * 6u + 2u] = PackedVertex(v3, voxel_data);

        vertex_pool[start_idx + f * 6u + 3u] = PackedVertex(v1, voxel_data);
        vertex_pool[start_idx + f * 6u + 4u] = PackedVertex(v3, voxel_data);
        vertex_pool[start_idx + f * 6u + 5u] = PackedVertex(v4, voxel_data);
    }
}