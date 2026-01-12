struct IndirectArgs {
    vertex_count: atomic<u32>,
    instance_count: u32,
    first_vertex: u32,
    first_instance: u32,
};

struct PackedVertex {
    pos_norm: u32,
    data: u32,
};

@group(0) @binding(0) var<storage, read_write> indirect: IndirectArgs;
@group(0) @binding(1) var<storage, read_write> verts: array<PackedVertex>;
@group(0) @binding(2) var<storage, read> map: array<u32>;

const SIZE = 64u;

fn get_idx(x: u32, y: u32) -> u32 {
    return x + (y * SIZE);
}

fn get_heights(x: u32, y: u32) -> vec4<u32> {
    if (x >= SIZE || y >= SIZE) { return vec4<u32>(0u); }
    let raw = map[get_idx(x, y)];
    return vec4<u32>(raw & 0xFFu, (raw >> 8u) & 0xFFu, (raw >> 16u) & 0xFFu, (raw >> 24u) & 0xFFu);
}

fn pack(x: u32, y: u32, z: u32, n: u32) -> u32 {
    return (x & 0xFFu) | ((y & 0xFFu) << 8u) | ((z & 0xFFu) << 16u) | ((n & 0xFFu) << 24u);
}

@compute @workgroup_size(8, 8, 1)
fn main(@builtin(global_invocation_id) id: vec3<u32>) {
    let x = id.x;
    let y = id.y;
    if (x >= SIZE || y >= SIZE) { return; }

    let h = get_heights(x, y);

    // --- ENCODE TILE ID ---
    // We pack X in lower 16 bits, Y in upper 16 bits.
    // This allows the fragment shader to know exactly which "logic tile" this geometry belongs to.
    let tile_id = x | (y << 16u);

    // --- 1. TOP SURFACE ---
    var idx = atomicAdd(&indirect.vertex_count, 6u);
    let n_up = 0u;
    
    // Note we pass 'tile_id' instead of color
    verts[idx+0] = PackedVertex(pack(x,   y,   h.x, n_up), tile_id);
    verts[idx+1] = PackedVertex(pack(x+1, y+1, h.z, n_up), tile_id);
    verts[idx+2] = PackedVertex(pack(x+1, y,   h.y, n_up), tile_id);
    
    verts[idx+3] = PackedVertex(pack(x,   y,   h.x, n_up), tile_id);
    verts[idx+4] = PackedVertex(pack(x,   y+1, h.w, n_up), tile_id);
    verts[idx+5] = PackedVertex(pack(x+1, y+1, h.z, n_up), tile_id);

    // --- 2. WALLS ---
    
    // Right (+X)
    if (x + 1 < SIZE) {
        let n_h = get_heights(x + 1, y);
        if (h.y > n_h.x || h.z > n_h.w) {
            idx = atomicAdd(&indirect.vertex_count, 6u);
            let n_right = 1u;
            verts[idx+0] = PackedVertex(pack(x+1, y,   h.y, n_right), tile_id);
            verts[idx+1] = PackedVertex(pack(x+1, y+1, h.z, n_right), tile_id);
            verts[idx+2] = PackedVertex(pack(x+1, y,   n_h.x, n_right), tile_id);
            verts[idx+3] = PackedVertex(pack(x+1, y,   n_h.x, n_right), tile_id);
            verts[idx+4] = PackedVertex(pack(x+1, y+1, h.z, n_right), tile_id);
            verts[idx+5] = PackedVertex(pack(x+1, y+1, n_h.w, n_right), tile_id);
        }
    }
    
    // Bottom (-Y)
    if (y > 0) {
        let n_h = get_heights(x, y - 1);
        if (h.x > n_h.w || h.y > n_h.z) {
             idx = atomicAdd(&indirect.vertex_count, 6u);
             let n_front = 4u;
             verts[idx+0] = PackedVertex(pack(x,   y, h.x, n_front), tile_id);
             verts[idx+1] = PackedVertex(pack(x+1, y, h.y, n_front), tile_id);
             verts[idx+2] = PackedVertex(pack(x,   y, n_h.w, n_front), tile_id);
             verts[idx+3] = PackedVertex(pack(x,   y, n_h.w, n_front), tile_id);
             verts[idx+4] = PackedVertex(pack(x+1, y, h.y, n_front), tile_id);
             verts[idx+5] = PackedVertex(pack(x+1, y, n_h.z, n_front), tile_id);
        }
    }

    // Left (-X)
    if (x > 0) {
        let n_h = get_heights(x - 1, y);
        if (h.x > n_h.y || h.w > n_h.z) {
            idx = atomicAdd(&indirect.vertex_count, 6u);
            let n_left = 2u;
            verts[idx+0] = PackedVertex(pack(x, y+1, h.w, n_left), tile_id);
            verts[idx+1] = PackedVertex(pack(x, y,   h.x, n_left), tile_id);
            verts[idx+2] = PackedVertex(pack(x, y+1, n_h.z, n_left), tile_id);
            verts[idx+3] = PackedVertex(pack(x, y+1, n_h.z, n_left), tile_id);
            verts[idx+4] = PackedVertex(pack(x, y,   h.x, n_left), tile_id);
            verts[idx+5] = PackedVertex(pack(x, y,   n_h.y, n_left), tile_id);
        }
    }

    // Top (+Y)
    if (y + 1 < SIZE) {
        let n_h = get_heights(x, y + 1);
        if (h.w > n_h.x || h.z > n_h.y) {
            idx = atomicAdd(&indirect.vertex_count, 6u);
            let n_back = 3u;
            verts[idx+0] = PackedVertex(pack(x+1, y+1, h.z, n_back), tile_id);
            verts[idx+1] = PackedVertex(pack(x,   y+1, h.w, n_back), tile_id);
            verts[idx+2] = PackedVertex(pack(x+1, y+1, n_h.y, n_back), tile_id);
            verts[idx+3] = PackedVertex(pack(x+1, y+1, n_h.y, n_back), tile_id);
            verts[idx+4] = PackedVertex(pack(x,   y+1, h.w, n_back), tile_id);
            verts[idx+5] = PackedVertex(pack(x,   y+1, n_h.x, n_back), tile_id);
        }
    }
}