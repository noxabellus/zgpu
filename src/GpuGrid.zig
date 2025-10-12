//! This is a GPU-focused voxel grid. We need to eliminate hash tables and dynamic allocation from the design.
//! This is a work in progress. The code here is focused around prototyping the data structures and procedures that will be needed gpu-side.
//! Thus, we are avoiding data types > 32 bits in atomic operations, and avoiding pointers.
//! basic idea:
//! pages of voxemes, ie chunks of bricks
//! each page is 16^3 voxemes
//! each voxeme is 16^3 voxels
//! use indirection tables to map from page space coordinate to page index
//! and from voxeme space coordinate to voxeme index

const Grid = @This();

const std = @import("std");

const log = std.log.scoped(.gpu_grid);

const linalg = @import("linalg.zig");

const vec2 = linalg.vec2;
const vec3 = linalg.vec3;
const vec3i = linalg.vec3i;
const vec3u = linalg.vec3u;

test {
    log.debug("semantic analysis for GpuGrid.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const axis_divisor = 16;
pub const axis_shift = 4; // log2(axis_divisor)
pub const total_shift = 8; // axis_shift + axis_shift
pub const axis_mask = 15; // axis_divisor - 1 aka 0b1111
pub const sub_unit_count = 4096; // 4096

pub const voxeme_scale = 1.0;
pub const voxel_scale = 0.0625; // 1/16
pub const inv_voxel_scale = 16.0; // 1/voxel_scale
pub const page_scale = 16.0; // 1 * 16

pub const PageCoord = packed struct(u32) {
    x: i11, // 2048 pages in x, y
    y: i11,
    z: i10, // 1024 pages in z

    pub const Unsigned = packed struct(u32) { x: u11, y: u11, z: u10 };
};

pub const LocalCoord = packed struct(u12) {
    x: u4,
    y: u4,
    z: u4,
};

pub const MaterialId = enum(u10) { none = 0, _ }; // 1023 possible materials

pub const max_materials = std.math.maxInt(std.meta.Tag(MaterialId));

pub const MaterialProperties = extern struct {
    color: vec3,
    is_opaque: bool,
};

pub const Visibility = packed struct(u6) {
    pos_x: bool,
    neg_x: bool,
    pos_y: bool,
    neg_y: bool,
    pos_z: bool,
    neg_z: bool,

    pub const all = Visibility{ .pos_x = true, .neg_x = true, .pos_y = true, .neg_y = true, .pos_z = true, .neg_z = true };
    pub const none = Visibility{ .pos_x = false, .neg_x = false, .pos_y = false, .neg_y = false, .pos_z = false, .neg_z = false };
};

pub const Voxel = packed struct(u32) {
    material_id: MaterialId,
    visibility: Visibility,
    state: u16,

    pub const empty = Voxel{ .material_id = .none, .visibility = .none, .state = 0 };
};

pub const PageTable = extern struct {
    pub const max_indirections = std.math.maxInt(u16);
    pub const indirection_mask = max_indirections - 1;
    pub const sentinel: u32 = std.math.maxInt(u32);
    pub const tombstone: u32 = sentinel - 1;

    indices: [max_indirections]u32 = [1]u32{sentinel} ** max_indirections,
    coordinates: [max_indirections]PageCoord = [1]PageCoord{.{ .x = 0, .y = 0, .z = 0 }} ** max_indirections,

    pub const empty = PageTable{};

    pub fn hash(coord: PageCoord) u32 {
        const ucoord: PageCoord.Unsigned = @bitCast(coord);
        const h = (@as(u32, ucoord.x) *% 92837111) ^ (@as(u32, ucoord.y) *% 689287499) ^ (@as(u32, ucoord.z) *% 283923481);
        return h & indirection_mask;
    }

    pub fn lookup(self: *const PageTable, coord: PageCoord) u32 {
        var h = hash(coord);
        for (0..max_indirections) |_| {
            // Step 1: Read the index FIRST.
            const index1 = @atomicLoad(u32, &self.indices[h], .acquire);

            if (index1 == sentinel) {
                return sentinel; // Chain ends here.
            }

            if (index1 != tombstone) {
                // Step 2: Read the payload (the coordinate).
                const coord_at_h = self.coordinates[h];

                // Step 3: Read the index AGAIN to verify.
                const index2 = @atomicLoad(u32, &self.indices[h], .acquire);

                // Step 4: Check for consistency.
                if (index1 == index2) {
                    // The index didn't change while we read the coordinate.
                    // The data is consistent. Now we can safely use it.
                    if (coord_at_h == coord) {
                        return index1; // Found it!
                    }
                }
                // If index1 != index2, a race happened. We just continue probing,
                // effectively treating it as a failed match, which is safe.
            }

            h = (h + 1) & indirection_mask;
        }

        return sentinel;
    }

    pub fn insert(self: *PageTable, coord: PageCoord, new_index: u32) u32 {
        var h = hash(coord);
        for (0..max_indirections) |_| {
            // --- Read Phase (Same logic as lookup) ---
            const index1 = @atomicLoad(u32, &self.indices[h], .acquire);

            // --- Write Phase ---
            if (index1 == sentinel or index1 == tombstone) {
                const result = @cmpxchgStrong(u32, &self.indices[h], index1, new_index, .acq_rel, .acquire);
                if (result == null) {
                    // We won the race to claim the index. Now we can safely write the coordinate.
                    // No other thread can be writing to `coordinates[h]` now.
                    self.coordinates[h] = coord;
                    return h;
                }
                continue; // CAS failed, retry.
            }

            // The slot is occupied. We need to verify the coordinate before attempting an update.
            const coord_at_h = self.coordinates[h];
            const index2 = @atomicLoad(u32, &self.indices[h], .acquire);

            if (index1 == index2) {
                // Consistent read. Now we can check if it's our key.
                if (coord_at_h == coord) {
                    const result = @cmpxchgStrong(u32, &self.indices[h], index1, new_index, .acq_rel, .acquire);
                    if (result == null) return h; // Update succeeded
                    continue; // Update failed, retry.
                }
            }

            // If inconsistent read or not our key, continue probing.
            h = (h + 1) & indirection_mask;
        }

        return sentinel; // Table full
    }

    pub fn remove(self: *PageTable, coord: PageCoord) void {
        var h = hash(coord);
        for (0..max_indirections) |_| {
            const index1 = @atomicLoad(u32, &self.indices[h], .acquire);
            if (index1 == sentinel) return;

            if (index1 != tombstone) {
                const coord_at_h = self.coordinates[h];
                const index2 = @atomicLoad(u32, &self.indices[h], .acquire);
                if (index1 == index2 and coord_at_h == coord) {
                    const result = @cmpxchgStrong(u32, &self.indices[h], index1, tombstone, .acq_rel, .acquire);
                    if (result == null) return; // Success!
                    continue; // CAS failed, retry.
                }
            }
            h = (h + 1) & indirection_mask;
        }
    }
};

pub const VoxemeTable = extern struct {
    pub const max_indirections = 4096;
    pub const indirection_mask = max_indirections - 1;
    pub const sentinel: u16 = std.math.maxInt(u16);
    pub const tombstone: u16 = sentinel - 1;

    pub const Value = packed struct(u32) {
        index: u16,
        coord: LocalCoord,
        _unused: u4 = 0,
        pub fn tombstone() Value {
            return Value{ .index = VoxemeTable.tombstone, .coord = .{ .x = 0, .y = 0, .z = 0 }, ._unused = 0 };
        }
    };

    values: [max_indirections]Value = [1]Value{.{ .coord = .{ .x = 0, .y = 0, .z = 0 }, .index = sentinel }} ** max_indirections,

    pub const empty = VoxemeTable{};

    pub fn hash(coord: LocalCoord) u16 {
        var h = (@as(u32, coord.x) *% 92837111) ^ (@as(u32, coord.y) *% 689287499) ^ (@as(u32, coord.z) *% 283923481);
        h ^= h >> 16;
        return @truncate(h & indirection_mask);
    }

    pub fn lookup(self: *const VoxemeTable, coord: LocalCoord) u16 {
        var h = hash(coord);
        for (0..max_indirections) |_| {
            // A single atomic load of the entire struct.
            const entry = @atomicLoad(Value, &self.values[h], .acquire);

            if (entry.index == sentinel) return sentinel;

            if (entry.index != tombstone and entry.coord == coord) {
                return entry.index;
            }

            h = (h + 1) & indirection_mask;
        }

        return sentinel;
    }

    pub fn insert(self: *VoxemeTable, coord: LocalCoord, new_index: u16) u16 {
        var h = hash(coord);
        const new_entry = Value{ .index = new_index, .coord = coord };

        for (0..max_indirections) |_| {
            // Atomically load the entire struct we expect to replace.
            const expected_entry = @atomicLoad(Value, &self.values[h], .acquire);

            // Case 1: Slot is claimable.
            if (expected_entry.index == sentinel or expected_entry.index == tombstone) {
                const result = @cmpxchgStrong(Value, &self.values[h], expected_entry, new_entry, .acq_rel, .acquire);
                if (result == null) return h;
                continue;
            }

            // Case 2: Slot is occupied by our key. Update it.
            if (expected_entry.coord == coord) {
                const result = @cmpxchgStrong(Value, &self.values[h], expected_entry, new_entry, .acq_rel, .acquire);
                if (result == null) return h;
                continue;
            }

            // Case 3: Collision.
            h = (h + 1) & indirection_mask;
        }

        @panic("VoxemeTable is full");
    }

    pub fn remove(self: *VoxemeTable, coord: LocalCoord) void {
        var h = hash(coord);
        const tombstone_entry = Value.tombstone();

        for (0..max_indirections) |_| {
            const expected_entry = @atomicLoad(Value, &self.values[h], .acquire);

            if (expected_entry.index == sentinel) return;

            if (expected_entry.index != tombstone and expected_entry.coord == coord) {
                const result = @cmpxchgStrong(Value, &self.values[h], expected_entry, tombstone_entry, .acq_rel, .acquire);
                if (result == null) return;
                continue;
            }

            h = (h + 1) & indirection_mask;
        }
    }
};

pub const Buffer = [sub_unit_count]Voxel; // 16*16*16 voxels in a voxeme
pub const buffer_sentinel = std.math.maxInt(u32); // sentinel for "no buffer", meaning homogeneous voxeme

pub const PageData = struct { // not really constructed, just a table layout for MultiArrayList
    voxel: Voxel,
    voxeme_indirection: VoxemeTable,
}; // each page is 16388 bytes

pub const VoxemeData = struct { // same as above
    voxel: Voxel,
    buffer_indirection: u32,
}; // each voxeme is 8 bytes

pub const DirtyPageBitSet = std.bit_set.ArrayBitSet(usize, PageTable.max_indirections); // 8192 bytes
pub const DirtyVoxemeBitSet = std.bit_set.ArrayBitSet(usize, PageTable.max_indirections * VoxemeTable.max_indirections); // 33553920 bytes (~32mb)

// the Grid struct itself is not extern as each address will need to be bound as gpu inputs

allocator: std.mem.Allocator,
page_indirection: PageTable = .empty, // ~512kb
pages: std.MultiArrayList(PageData) = .empty,
voxemes: std.MultiArrayList(VoxemeData) = .empty,
voxels: std.ArrayList(Buffer) = .empty,
dirty_page_set: DirtyPageBitSet = .initEmpty(), // pages that have been modified since last gpu sync
dirty_voxeme_set: DirtyVoxemeBitSet = .initEmpty(), // voxemes that have been modified since last gpu sync
material_properties: std.ArrayList(MaterialProperties) = .empty,

// Roughly ~66mb of initial allocations
pub fn init(allocator: std.mem.Allocator) !*Grid {
    var self = try allocator.create(Grid); // allocate self because the page table is large
    errdefer allocator.destroy(self);

    self.* = Grid{
        .allocator = allocator,
    };

    try self.pages.ensureTotalCapacity(allocator, 1024); // 16mb
    errdefer self.pages.deinit(allocator);

    try self.voxemes.ensureTotalCapacity(allocator, 1024); // 8kb
    errdefer self.voxemes.deinit(allocator);

    try self.voxels.ensureTotalCapacity(allocator, 1024); // 16mb
    errdefer self.voxels.deinit(allocator);

    try self.material_properties.ensureTotalCapacity(allocator, 1024);
    errdefer self.material_properties.deinit(allocator);

    // initialize the empty material
    self.material_properties.appendAssumeCapacity(.{
        .color = .{ 0.0, 0.0, 0.0 },
        .is_opaque = false,
    });

    return self;
}

pub fn deinit(self: *Grid) void {
    self.voxels.deinit(self.allocator);
    self.voxemes.deinit(self.allocator);
    self.pages.deinit(self.allocator);
    self.material_properties.deinit(self.allocator);
    self.allocator.destroy(self);
}

pub fn clear(self: *Grid, clear_materials: bool) void {
    self.page_indirection = .empty;
    self.pages.clearRetainingCapacity();
    self.voxemes.clearRetainingCapacity();
    self.voxels.clearRetainingCapacity();
    if (clear_materials) self.material_properties.clearRetainingCapacity();
    self.dirty_page_set = .initEmpty();
    self.dirty_voxeme_set = .initEmpty();
}

pub fn pageCount(self: *const Grid) usize {
    return self.pages.len;
}

pub fn voxemeCount(self: *const Grid) usize {
    return self.voxemes.len;
}

pub fn bufferCount(self: *const Grid) usize {
    return self.voxels.items.len;
}

pub fn voxelCount(self: *const Grid) usize {
    return self.bufferCount() * sub_unit_count;
}

pub fn materialCount(self: *const Grid) usize {
    return self.material_properties.items.len;
}

pub fn registerMaterial(self: *Grid, properties: MaterialProperties) !MaterialId {
    const index = self.materialCount();
    if (index >= max_materials) {
        return error.OutOfMaterials;
    }

    self.material_properties.appendAssumeCapacity(properties);

    return @enumFromInt(index);
}

pub fn getVoxel(world: *const Grid, global_voxel: vec3i) Voxel {
    // 2. Find which Page it's in.
    const page_coord = convert.globalVoxelToPageCoord(global_voxel);
    const page_hash_index = world.page_indirection.lookup(page_coord);
    if (page_hash_index == PageTable.sentinel) {
        return .empty; // Page doesn't exist.
    }
    // Now we have the actual index into the `pages` MultiArrayList.
    const page_index: u32 = world.page_indirection.indices[page_hash_index];

    // 3. Find which Voxeme it's in *within that page*.
    const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(global_voxel);

    // Get the specific VoxemeTable for our page.
    const voxeme_table: *const VoxemeTable = &world.pages.items(.voxeme_indirection)[page_index];

    const voxeme_hash_index = voxeme_table.lookup(local_voxeme_coord);
    if (voxeme_hash_index == VoxemeTable.sentinel) {
        // Voxeme doesn't exist, so return the page's homogeneous voxel data.
        return world.pages.items(.voxel)[page_index];
    }
    // Now we have the actual index into the global `voxemes` MultiArrayList.
    const voxeme_index: u16 = voxeme_table.values[voxeme_hash_index].index;

    // 4. Get the data for that voxeme.
    const voxeme_voxel = world.voxemes.items(.voxel)[voxeme_index];
    const buffer_handle = world.voxemes.items(.buffer_indirection)[voxeme_index];
    if (buffer_handle == buffer_sentinel) { // Check if it's homogeneous
        return voxeme_voxel; // It's a homogeneous voxeme.
    }

    // 5. It's a heterogeneous voxeme. Find the specific voxel.
    const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);
    const index_in_buffer = convert.localCoordToIndex(local_voxel_coord);

    // The buffer handle is an index into the final `voxels` ArrayList.
    const buffer = &world.voxels.items[buffer_handle];

    return buffer[index_in_buffer]; // Final Voxel!
}

pub fn setVoxel(self: *Grid, global_voxel: vec3i, new_material_id: MaterialId, new_state: u16) !void {
    // --- 1. FIND OR CREATE PAGE ---
    const page_coord = convert.globalVoxelToPageCoord(global_voxel);
    var page_hash_index = self.page_indirection.lookup(page_coord);
    var page_index: u32 = undefined;

    if (page_hash_index == PageTable.sentinel) {
        page_index = @intCast(self.pageCount());
        if (page_index >= PageTable.max_indirections) return error.OutOfPages;

        // Reserve the spot in the indirection table FIRST.
        page_hash_index = self.page_indirection.insert(page_coord, page_index);
        if (page_hash_index == PageTable.sentinel) @panic("PageTable full after check passed"); // Should not happen

        // NOW append the data.
        try self.pages.append(self.allocator, .{
            .voxel = Voxel.empty,
            .voxeme_indirection = .empty,
        });
    } else {
        page_index = self.page_indirection.indices[page_hash_index];
    }

    // --- 2. FIND OR CREATE VOXEME ---
    const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(global_voxel);
    const voxeme_table: *VoxemeTable = &self.pages.items(.voxeme_indirection)[page_index];
    var voxeme_hash_index = voxeme_table.lookup(local_voxeme_coord);

    if (voxeme_hash_index == VoxemeTable.sentinel) {
        // === BREAKING A HOMOGENEOUS PAGE (with your safe ordering) ===
        const page_homogeneous_voxel = self.pages.items(.voxel)[page_index];
        if (page_homogeneous_voxel.material_id == new_material_id and page_homogeneous_voxel.state == new_state) return;

        // 1. Reserve the spot in the page's voxeme table FIRST.
        const new_voxeme_index: u16 = @intCast(self.voxemeCount());
        voxeme_hash_index = voxeme_table.insert(local_voxeme_coord, new_voxeme_index);
        if (voxeme_hash_index == VoxemeTable.sentinel) return error.OutOfVoxemes;

        // 2. NOW we are committed. Allocate the buffer.
        const new_buffer = try self.voxels.addOne(self.allocator);
        @memset(new_buffer, page_homogeneous_voxel);

        const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);
        const index_in_buffer = convert.localCoordToIndex(local_voxel_coord);
        new_buffer[index_in_buffer] = .{
            .material_id = new_material_id,
            .visibility = .none, // Visibility must be recalculated.
            .state = new_state,
        };

        const buffer_handle: u32 = @intCast(self.voxels.items.len - 1);

        // 3. And finally, append the voxeme data.
        try self.voxemes.append(self.allocator, .{
            .voxel = Voxel.empty,
            .buffer_indirection = buffer_handle,
        });
    } else {
        // === MODIFYING AN EXISTING VOXEME ===
        const voxeme_index = voxeme_table.values[voxeme_hash_index].index;
        const voxeme_data = &self.voxemes.items(.voxel)[voxeme_index];
        const buffer_handle = &self.voxemes.items(.buffer_indirection)[voxeme_index];

        if (buffer_handle.* == buffer_sentinel) {
            // Break a homogeneous voxeme.
            if (voxeme_data.material_id == new_material_id and voxeme_data.state == new_state) return;

            const new_buffer = try self.voxels.addOne(self.allocator);
            @memset(new_buffer, voxeme_data.*);

            const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);
            const index_in_buffer = convert.localCoordToIndex(local_voxel_coord);
            new_buffer[index_in_buffer] = .{
                .material_id = new_material_id,
                .visibility = .none,
                .state = new_state,
            };

            buffer_handle.* = @intCast(self.voxels.items.len - 1);
            voxeme_data.* = Voxel.empty;
        } else {
            // Modify a heterogeneous voxeme.
            const buffer = &self.voxels.items[buffer_handle.*];
            const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);
            const index_in_buffer = convert.localCoordToIndex(local_voxel_coord);
            const voxel = &buffer[index_in_buffer];

            if (voxel.material_id == new_material_id and voxel.state == new_state) return;

            voxel.* = Voxel{
                .material_id = new_material_id,
                .visibility = .none, // Reset visibility for this voxel
                .state = new_state,
            };
        }
    }

    // --- 3. MARK DIRTY ---
    self.dirty_page_set.set(page_index);
}

/// namespace for voxel coordinate conversions
pub const convert = struct {
    // --- High-Level to Low-Level (Drilling Down) ---

    /// World-space position to the global, infinite voxel grid coordinate.
    pub inline fn worldToGlobalVoxel(pos: vec3) vec3i {
        return @as(vec3i, @intFromFloat(@floor(pos * comptime @as(vec3, @splat(inv_voxel_scale)))));
    }

    /// Global voxel coordinate to the containing Page's global coordinate.
    /// Used for hashing into the PageTable.
    pub inline fn globalVoxelToPageCoord(v: vec3i) PageCoord {
        // Fast floor division by (16*16) using an arithmetic right shift.
        const shifted = v >> @splat(total_shift);
        // Pack the i32 components into the smaller bitfields of PageCoord.
        return .{
            .x = @intCast(shifted[0]),
            .y = @intCast(shifted[1]),
            .z = @intCast(shifted[2]),
        };
    }

    /// Global voxel coordinate to the local coordinate of the voxeme *within its page*.
    /// Used for hashing into a specific page's VoxemeTable.
    pub inline fn globalVoxelToLocalVoxemeCoord(v: vec3i) LocalCoord {
        // 1. Get the global coordinate of the containing voxeme.
        const global_voxeme_coord = v >> comptime @splat(axis_shift);
        // 2. Get the local part of that coordinate within the page grid (fast modulo).
        const local_coord = global_voxeme_coord & comptime @as(vec3i, @splat(axis_mask));
        // 3. Pack into the struct.
        return .{
            .x = @intCast(local_coord[0]),
            .y = @intCast(local_coord[1]),
            .z = @intCast(local_coord[2]),
        };
    }

    /// Global voxel coordinate to the local coordinate of the voxel *within its voxeme*.
    /// Used for indexing into a dense Voxel Buffer.
    pub inline fn globalVoxelToLocalVoxelCoord(v: vec3i) LocalCoord {
        // Fast modulo by 16 to get the local coordinate.
        const base = v & comptime @as(vec3i, @splat(axis_mask));
        return LocalCoord{
            .x = @intCast(base[0]),
            .y = @intCast(base[1]),
            .z = @intCast(base[2]),
        };
    }

    // --- Local Coords to 1D Buffer Indices ---

    /// 3D local coordinate (0-15) to a 1D buffer index (0-4095).
    /// Works for both local voxemes within a page and local voxels within a voxeme.
    pub inline fn localCoordToIndex(local_coord: LocalCoord) u16 {
        // Standard 3D to 1D mapping: x + y*WIDTH + z*WIDTH*HEIGHT
        // Since WIDTH and HEIGHT are both 16, this is:
        // x + y * 16 + z * 256
        const x = @as(u32, local_coord.x);
        const y = @as(u32, local_coord.y);
        const z = @as(u32, local_coord.z);
        return @intCast(x + (y << axis_shift) + (z << total_shift));
    }

    // --- Low-Level to High-Level (Building Back Up) ---

    /// Reconstructs a global voxel coordinate from its constituent parts.
    /// Useful for debugging or world generation algorithms.
    pub inline fn partsToGlobalVoxel(
        page_coord: PageCoord,
        local_voxeme: LocalCoord,
        local_voxel: LocalCoord,
    ) vec3i {
        const page_base: vec3i = .{ @as(i32, page_coord.x), @as(i32, page_coord.y), @as(i32, page_coord.z) };
        const local_voxeme_base: vec3i = .{ @as(i32, local_voxeme.x), @as(i32, local_voxeme.y), @as(i32, local_voxeme.z) };
        const local_voxel_base: vec3i = .{ @as(i32, local_voxel.x), @as(i32, local_voxel.y), @as(i32, local_voxel.z) };

        return (page_base << comptime @splat(total_shift)) +
            (local_voxeme_base << comptime @splat(axis_shift)) +
            local_voxel_base;
    }

    /// Global voxel coordinate back to a world-space position.
    /// (SampleOffsets is a useful enum to get min/center/max of a voxel).
    pub inline fn globalVoxelToWorld(v: vec3i) vec3 {
        return @as(vec3, @floatFromInt(v)) * comptime @as(vec3, @splat(voxel_scale));
    }
};
