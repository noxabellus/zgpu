const GRID_AXIS_DIVISOR: u32 = 128;
const PAGE_AXIS_DIVISOR: u32 = 16;
const VOXEME_AXIS_DIVISOR: u32 = 16;

const PAGE_TABLE_SIZE: u32 = GRID_AXIS_DIVISOR * GRID_AXIS_DIVISOR * GRID_AXIS_DIVISOR;
const VOXEMES_PER_PAGE: u32 = PAGE_AXIS_DIVISOR * PAGE_AXIS_DIVISOR * PAGE_AXIS_DIVISOR;
const VOXELS_PER_VOXEME: u32 = VOXEME_AXIS_DIVISOR * VOXEME_AXIS_DIVISOR * VOXEME_AXIS_DIVISOR;
const VOXELS_PER_PAGE: u32 = VOXELS_PER_VOXEME * VOXEMES_PER_PAGE;

const GRID_SCALE: f32 = 2048.0;
const INV_GRID_SCALE: f32 = 1.0 / GRID_SCALE;

const PAGE_SCALE: f32 = 16;
const INV_PAGE_SCALE: f32 = 1.0 / PAGE_SCALE;

const VOXEME_SCALE: f32 = 1.0;
const INV_VOXEME_SCALE: f32 = 1.0 / VOXEME_SCALE;

const VOXEL_SCALE: f32 = 0.0625;
const INV_VOXEL_SCALE: f32 = 1.0 / VOXEL_SCALE;

const GRID_AXIS_SHIFT: u32 = 7; // log2(128)
const GRID_AXIS_MASK: u32 = GRID_AXIS_DIVISOR - 1;

const PAGE_AXIS_SHIFT: u32 = 4; // log2(16)
const PAGE_AXIS_MASK: u32 = PAGE_AXIS_DIVISOR - 1;

const VOXEME_AXIS_SHIFT: u32 = 4; // log2(16)
const VOXEME_AXIS_MASK: u32 = VOXEME_AXIS_DIVISOR - 1;

const LOCAL_SHIFT: u32 = PAGE_AXIS_SHIFT + VOXEME_AXIS_SHIFT;

const MAX_PAGES: u32 = PAGE_TABLE_SIZE;
// const MAX_VOXEMES: u32 = MAX_PAGES * VOXEMES_PER_PAGE; // Note: this would be 1gb even if each voxeme/table was only 1 byte (theyre not)
// const MAX_VOXELS: u32 = MAX_VOXEMES * VOXELS_PER_VOXEME; // Note: 3.518437209*10¹³ (35 trillion) voxels total, definitely not storable

const SENTINEL_INDEX = 0xFFFFFFFFu; // u32 max

const MAX_MATERIALS: u32 = 4096; // u12 max

alias Color = u32; // RGBA8

alias MaterialFlags = u32;
// bit 0 (LSB): is_solid

struct MaterialData {
    color: Color,
    flags: MaterialFlags,
};

alias Voxel = u32; // u12 material_id + u20 data

fn getVoxelMaterialId(voxel: Voxel) -> u32 {
    return voxel & 0xFFFu; // lower 12 bits
}

struct PageData {
    // The coordinate of this page in the infinite 3D grid of pages.
    coord: vec3<i32>, // in grid space
    // The homogeneous voxel data for this page.
    // Even if the page contains voxemes, this is still the default voxel state for implicit voxemes.
    voxel: Voxel,
    /// index into voxeme table
    buffer_indirection: u32,
};

struct VoxemeData {
    // The coordinate of this voxeme within its page.
    coord: vec3<i32>, // in voxeme space
    // The homogeneous voxel data for this voxeme;
    // unlike page-level voxel data, this is only valid
    // if `buffer_indirection` is `sentinel_index`.
    voxel: Voxel,
    // index into voxel table
    buffer_indirection: u32,
}

alias CommandType = u32;
const SET_AABB: CommandType = 0; // coord0 = min, coord1 = max
const SET_PAGE: CommandType = 1; // coord0 = page coord, coord1 unused
const SET_VOXEME: CommandType = 2; // coord0 = page coord, coord1 = voxeme coord
const SET_VOXEL: CommandType = 3; // coord0 = global voxel coord, coord1 unused

struct GridCommand {
    coord0: vec3<i32>,
    command_type: CommandType,
    coord1: vec3<i32>,
    voxel: Voxel,
}

struct MeshDescriptor {
    coord: vec3<i32>, // in grid space
    index: u32,
    length: u32,
};

struct Vertex {
    pos: vec3<f32>,
    material_id: u32,
    norm: vec3<f32>,
};

struct GridCounters {
    page: atomic<u32>, // total number of pages allocated
    voxeme: atomic<u32>, // total number of voxemes allocated
    voxel: atomic<u32>, // total number of non-homogeneous voxemes (not individual voxels but sets of 4096)
    free_page: atomic<u32>, // total number of free pages
    free_voxeme: atomic<u32>, // total number of free voxemes
    free_voxel: atomic<u32>, // total number of free voxels
};

// Grid data
@group(0) @binding(0) var<storage, read> materials: array<MaterialData>;
@group(0) @binding(1) var<storage, read_write> page_table_mapping: array<u32>;
@group(0) @binding(2) var<storage, read_write> page_table: array<PageData>; // index via table
@group(0) @binding(4) var<storage, read_write> voxeme_table: array<VoxemeData>; // index via table
@group(0) @binding(5) var<storage, read_write> voxel_table: array<Voxel>; // index by voxeme buffer_indirection * VOXELS_PER_VOXEME + voxel local idx
@group(0) @binding(7) var<storage, read_write> free_page_list: array<u32>;
@group(0) @binding(8) var<storage, read_write> free_voxeme_list: array<u32>;
@group(0) @binding(9) var<storage, read_write> free_voxel_list: array<u32>;
@group(0) @binding(6) var<storage, read_write> grid_counters: GridCounters;

fn coordToPageTableIndex(coord: vec3<i32>) -> u32 {
    let wrap = bitcast<vec3<u32>>(coord) & vec3<u32>(GRID_AXIS_MASK);
    return wrap.x + (wrap.y << GRID_AXIS_SHIFT) + (wrap.z << (2u * GRID_AXIS_SHIFT));
}

fn worldToGlobalVoxel(world_pos: vec3<f32>) -> vec3<i32> {
    return vec3<i32>(floor(world_pos * vec3<f32>(INV_VOXEL_SCALE)));
}

fn globalVoxelToWorld(voxel_coord: vec3<i32>) -> vec3<f32> {
    return vec3<f32>(voxel_coord) * vec3<f32>(VOXEL_SCALE);
}

fn globalVoxelToPageCoord(voxel_coord: vec3<i32>) -> vec3<i32> {
    return voxel_coord >> LOCAL_SHIFT;
}

fn globalVoxelToVoxemeCoord(voxel_coord: vec3<i32>) -> vec3<i32> {
    return (voxel_coord >> VOXEME_AXIS_SHIFT) & PAGE_AXIS_MASK;
}

fn globalVoxelToLocalVoxelCoord(voxel_coord: vec3<i32>) -> vec3<i32> {
    return voxel_coord & VOXEME_AXIS_MASK;
}

fn localVoxemeCoordToIndex(local_coord: vec3<i32>) -> i32 {
    return local_coord.x + (local_coord.y << PAGE_AXIS_SHIFT) + (local_coord.z << (2u * PAGE_AXIS_SHIFT));
}

fn indexToLocalVoxemeCoord(index: i32) -> vec3<i32> {
    let x = index & PAGE_AXIS_MASK;
    let y = (index >> PAGE_AXIS_SHIFT) & PAGE_AXIS_MASK;
    let z = (index >> (2u * PAGE_AXIS_SHIFT)) & PAGE_AXIS_MASK;
    return vec3<i32>(x, y, z);
}

fn localVoxelCoordToIndex(local_coord: vec3<i32>) -> i32 {
    return local_coord.x + (local_coord.y << VOXEME_AXIS_SHIFT) + (local_coord.z << (2u * VOXEME_AXIS_SHIFT));
}

fn indexToLocalVoxelCoord(index: i32) -> vec3<i32> {
    let x = index & VOXEME_AXIS_MASK;
    let y = (index >> VOXEME_AXIS_SHIFT) & VOXEME_AXIS_MASK;
    let z = (index >> (2u * VOXEME_AXIS_SHIFT)) & VOXEME_AXIS_MASK;
    return vec3<i32>(x, y, z);
}



// COMMAND PROCESSING COMPUTE SHADER //

struct CommandCounters {
    dirty: atomic<u32>,
    removed: atomic<u32>,
}

@group(1) @binding(0) var<storage, read> commands: array<GridCommand>;
@group(1) @binding(1) var<storage, write> dirty_pages: array<vec3<i32>>; // for (re)generating meshes
@group(1) @binding(2) var<storage, write> removed_pages: array<vec3<i32>>; // for culling cached meshes
@group(1) @binding(3) var<storage, read_write> command_counters: CommandCounters;

@compute @workgroup_size(4, 4, 4) // TODO: determine wg size
fn main(@builtin(global_invocation_id) id: vec3<u32>) {
    let cmd_idx = id.x + (id.y * 4u) + (id.z * 16u); // wg size dependent
    if (cmd_idx >= arrayLength(&commands)) { return; }

    let command = commands[cmd_idx];

    switch (command) {
        case SET_AABB: {
            setAABB(command.coord0, command.coord1, command.voxel);
        },
        case SET_PAGE: {
            setPage(command.coord0, command.voxel);
        },
        case SET_VOXEME: {
            setVoxeme(command.coord0, command.coord1, command.voxel);
        },
        case SET_VOXEL: {
            setVoxel(command.coord0, command.voxel);
        },
        default: {
            // Invalid command
        }
    }
}

fn setAABB(min_coord: vec3<i32>, max_coord: vec3<i32>, voxel: Voxel) {
    // TODO
}

fn setPage(page_coord: vec3<i32>, voxel: Voxel) {
    let page_coord = globalVoxelToPageCoord(global_voxel_coord);
    let voxeme_coord = globalVoxelToVoxemeCoord(global_voxel_coord);

    let page_table_idx = coordToPageTableIndex(page_coord);
    var page_idx = page_table_mapping[page_table_idx];
    if (page_idx == SENTINEL_INDEX) {
        if (getVoxelMaterialId(voxel) == 0u) {
            // air to air, no-op
            return;
        }

        // new page
        let free_idx = atomicSub(&grid_counters.free_page, 1u);
        page_idx = free_page_list[free_idx];
        page_table_mapping[page_table_idx] = page_idx;

        page_table[page_idx] = PageData(page_coord, voxel, SENTINEL_INDEX);

        let dirty_index = atomicAdd(&command_counters.dirty, 1u);
        dirty_pages[dirty_index] = page_coord;
    } else {
        // page exists
        let page = page_table[page_idx];
        if (page.voxel == voxel) {
            // no change, no-op
            return;
        }

        if (getVoxelMaterialId(voxel) == 0u) {
            // air, remove page
            page_table_mapping[page_table_idx] = SENTINEL_INDEX;

            let removed_index = atomicAdd(&command_counters.removed, 1u);
            removed_pages[removed_index] = page_coord;

            let free_index = atomicAdd(&grid_counters.free_page, 1u);
            free_page_list[free_index] = page_idx;
        } else {
            // update page voxel
            page_table[page_idx].voxel = voxel;

            let dirty_index = atomicAdd(&command_counters.dirty, 1u);
            dirty_pages[dirty_index] = page_coord;
        }

        // free old voxemes
        if (page.buffer_indirection != SENTINEL_INDEX) {
            // free old voxels
            for (var v = 0u; v < VOXEMES_PER_PAGE; v++) {
                let voxeme_idx = page.buffer_indirection + v;
                let voxeme = voxeme_table[voxeme_idx];
                if (voxeme.buffer_indirection != SENTINEL_INDEX) {
                    let free_voxel_idx = atomicAdd(&grid_counters.free_voxel, 1u);
                    free_voxel_list[free_voxel_idx] = voxeme.buffer_indirection;
                }

                let free_voxeme_idx = atomicAdd(&grid_counters.free_voxeme, 1u);
                free_voxeme_list[free_voxeme_idx] = voxeme_idx;
            }

            let free_voxeme_idx = atomicAdd(&grid_counters.free_voxeme, 1u);
            free_voxeme_list[free_voxeme_idx] = page.buffer_indirection;
            page.buffer_indirection = SENTINEL_INDEX;
        }
    }

    // mark neighbors as dirty; they need to remesh as their edges may be invalid
    let dirty_index = atomicAdd(&command_counters.dirty, 6u);
    var i = dirty_index;
    for (var z = -1; z <= 1; z++) {
        for (var y = -1; y <= 1; y++) {
            for (var x = -1; x <= 1; x++) {
                if (abs(x) + abs(y) + abs(z) != 1) { continue; } // only direct neighbors

                let neighbor_coord = page_coord + vec3<i32>(x, y, z);
                dirty_pages[i] = neighbor_coord;

                i += 1;
            }
        }
    }
}

fn setVoxeme(page_coord: vec3<i32>, voxeme_coord: vec3<i32>, voxel: Voxel) {
    // TODO
}

fn setVoxel(global_voxel_coord: vec3<i32>, voxel: Voxel) {
    // TODO
}



// MESHING COMPUTE SHADER //

struct MeshCounters {
    descriptors: atomic<u32>,
    vertices: atomic<u32>,
    indices: atomic<u32>,
};

@group(1) @binding(0) var<storage, read> dirty_pages: array<vec3<i32>>;
@group(1) @binding(1) var<storage, write> mesh_descriptors: array<MeshDescriptor>;
@group(1) @binding(2) var<storage, write> mesh_vertices: array<Vertex>;
@group(1) @binding(3) var<storage, write> mesh_indices: array<u32>;
@group(1) @binding(4) var<storage, read_write> mesh_counters: MeshCounters;

@compute @workgroup_size(4, 4, 4) // TODO: determine wg size
fn main(@builtin(global_invocation_id) id: vec3<u32>) {
    // ... todo ...
}