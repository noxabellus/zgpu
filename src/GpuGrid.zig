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

/// The Voxeme scale is the base unit for all other scales. A voxeme scale of 1.0
/// means each voxeme is 1m^3. This gives our "coarse voxels" a reasonable size for
/// pathfinding etc. The actual, small-voxel scale is derived by dividing this by
/// the voxeme_axis_divisor. The current divisor of 16 means that the smallest voxel
/// is 1/16m = 6.25cm, which is a quite small size and will allow for fairly
/// detailed geometry, at the cost of memory usage for detailed areas. Our sparse
/// data structures alleviate this cost where detail is sparse.
pub const voxeme_scale = 1.0;

/// Allow up to 65535 pages total in memory at a time. At the default sizes, this
/// would allow 1km^2 in memory for a world with z-depth of 256m. The in-memory
/// volume can be adjusted without changing level of detail by changing the
/// `page_axis_divisor`, but it is important to realize that this is the *working*
/// region of the world, where voxels can be modified in ~realtime. It is possible
/// to *visualize* larger worlds, by leaving generated meshes in memory while
/// refocusing the loaded pages to new areas. The actual live portion the player is
/// using does not need to be 1km^2 in general; thus, this leaves a large overhead
/// for loading pages during distant-land generation, and for simulation.
pub const max_pages = std.math.maxInt(u16);

//// The number of voxemes along each axis in a page. Changing this value does not
//// change level of detail in the world, but it does effect how much volume can be
//// loaded into memory at once. See `max_pages` for details.
pub const page_axis_divisor = 16;

//// The number of voxels along each axis in a voxeme. Setting this smaller leads to
//// decreased level of detail in the world, but less memory usage for detailed
//// areas. Setting it larger leads to more memory usage, but more detail.
pub const voxeme_axis_divisor = 16;

/// The size of a page in world units (meters).
pub const page_scale = voxeme_scale * @as(comptime_float, page_axis_divisor);
/// The size of a voxel in world units (meters).
pub const voxel_scale = voxeme_scale / @as(comptime_float, voxeme_axis_divisor);

/// The inverse of `page_scale`, useful for fast multiplication instead of division.
pub const inv_page_scale = 1.0 / page_scale;
/// The inverse of `voxel_scale`, useful for fast multiplication instead of division.
pub const inv_voxel_scale = 1.0 / voxel_scale;

/// `log2(page_axis_divisor)`; useful for optimizing coordinate conversions via bitwise shift.
pub const page_axis_shift = std.math.log2(page_axis_divisor);
/// `log2(voxeme_axis_divisor)`; useful for optimizing coordinate conversions via bitwise shift.
pub const voxeme_axis_shift = std.math.log2(voxeme_axis_divisor);
/// `page_axis_shift + voxeme_axis_shift`; useful for optimizing coordinate conversions via bitwise shift.
pub const total_shift = page_axis_shift + voxeme_axis_shift;

/// `page_axis_divisor - 1`; useful for fast modulo operations via bitwise &.
pub const page_axis_mask = page_axis_divisor - 1;
/// `voxeme_axis_divisor - 1`; useful for fast modulo operations via bitwise &.
pub const voxeme_axis_mask = voxeme_axis_divisor - 1;

/// `pow(page_axis_divisor, 3)`; total (maximum) number of voxemes in a page.
pub const voxemes_per_page = page_axis_divisor * page_axis_divisor * page_axis_divisor; // no comptime pow
/// `pow(voxeme_axis_divisor, 3)`; total (maximum) number of voxels in a voxeme.
pub const voxels_per_voxeme = voxeme_axis_divisor * voxeme_axis_divisor * voxeme_axis_divisor;
/// `voxemes_per_page * voxels_per_voxeme`; total (maximum) number of voxels in a page.
pub const voxels_per_page = voxemes_per_page * voxels_per_voxeme;

/// A coordinate identifying a page in the infinite 3D grid of pages.
pub const PageCoord = packed struct(u32) {
    x: i11, // 2048 pages in x, y
    y: i11,
    z: i10, // 1024 pages in z

    /// Unsigned version of `PageCoord`, useful for bitcasting before hashing.
    pub const Unsigned = packed struct(u32) { x: u11, y: u11, z: u10 };
};

/// A coordinate identifying a voxeme within a page, or a voxel within a voxeme.
pub const LocalCoord = packed struct(u12) {
    x: u4,
    y: u4,
    z: u4,
};

/// Unique identifier for a voxel material type.
/// Supports up to 4096 materials including the empty state. See `MaterialProperties`, `registerMaterial`.
pub const MaterialId = enum(u12) { none = 0, _ };

/// Maximum number of materials supported by the grid. (4096 including empty)
pub const max_materials = std.math.maxInt(std.meta.Tag(MaterialId));

/// Surface properties for a voxel material type. See `MaterialId`, `registerMaterial`.
pub const MaterialProperties = extern struct {
    /// The base color of this material type before lighting.
    color: Color,
    /// State flags for this material type, such as whether it is opaque (occludes other voxels, light, etc).
    flags: MaterialFlags,

    /// The properties for the "no material" material / default state.
    pub const none = MaterialProperties{
        .color = .none,
        .flags = .none,
    };
}; // 8 bytes

/// RGBA color with 8-bit channels.
pub const Color = packed struct(u32) {
    r: u8,
    g: u8,
    b: u8,
    a: u8 = 255, // this can be thought of as the "emissive" channel, or as the overall additive brightness

    pub const none = Color{ .r = 0, .g = 0, .b = 0, .a = 0 };
    pub const white = Color{ .r = 255, .g = 255, .b = 255, .a = 255 };
    pub const grey = Color{ .r = 128, .g = 128, .b = 128, .a = 255 };
    pub const black = Color{ .r = 0, .g = 0, .b = 0, .a = 255 };
    pub const red = Color{ .r = 255, .g = 0, .b = 0, .a = 255 };
    pub const green = Color{ .r = 0, .g = 255, .b = 0, .a = 255 };
    pub const blue = Color{ .r = 0, .g = 0, .b = 255, .a = 255 };
    pub const cyan = Color{ .r = 0, .g = 255, .b = 255, .a = 255 };
    pub const magenta = Color{ .r = 255, .g = 0, .b = 255, .a = 255 };
    pub const yellow = Color{ .r = 255, .g = 255, .b = 0, .a = 255 };

    /// Create a new color with the same RGB channels but a different alpha channel.
    pub fn withAlpha(self: Color, new_alpha: u8) Color {
        return Color{ .r = self.r, .g = self.g, .b = self.b, .a = new_alpha };
    }
};

/// State flags for a voxel material type. See `MaterialProperties`.
pub const MaterialFlags = packed struct(u32) {
    /// Whether this material occludes other voxels, light, etc.
    is_opaque: bool,
    _unused: u31 = 0,

    /// The flags for the "no material" material / default state.
    pub const none = MaterialFlags{ .is_opaque = false };
};

/// Offset coordinates for the 6 faces of a volume.
/// These are in the same order as `Visibility`.
pub const offsets = [6]vec3i{
    .{ 1, 0, 0 }, // +X
    .{ -1, 0, 0 }, // -X
    .{ 0, 1, 0 }, // +Y
    .{ 0, -1, 0 }, // -Y
    .{ 0, 0, 1 }, // +Z
    .{ 0, 0, -1 }, // -Z
};

/// Axis identifiers for the 3 axes in 3D space.
/// These convert to indices in `offsets` and `Visibility`; add 1 for negative directions.
pub const Axis = enum(u3) {
    x = 0,
    y = 2,
    z = 4,

    pub fn toIndex(self: Axis) u3 {
        return @intFromEnum(self);
    }

    pub fn fromIndex(index: u3) Axis {
        return @enumFromInt(index & 0b110);
    }
};

/// Axis and direction identifier.
pub const AxisDir = packed struct(u8) {
    axis: Axis,
    positive: bool,
    _unused: u4 = 0,

    pub fn toIndex(self: AxisDir) u3 {
        return self.axis.toIndex() + @intFromBool(!self.positive);
    }

    pub fn fromIndex(index: u3) AxisDir {
        return AxisDir{
            .axis = Axis.fromIndex(index),
            .positive = (index & 1) == 0,
        };
    }

    pub fn getOffset(self: AxisDir) vec3i {
        return offsets[self.toIndex()];
    }
};

/// Visibility flags for the 6 faces of a volume.
pub const Visibility = packed struct(u8) {
    pos_x: bool,
    neg_x: bool,
    pos_y: bool,
    neg_y: bool,
    pos_z: bool,
    neg_z: bool,
    _unused: u2 = 0,

    /// Completely unoccluded state.
    pub const all = Visibility{ .pos_x = true, .neg_x = true, .pos_y = true, .neg_y = true, .pos_z = true, .neg_z = true };

    /// Completely occluded state.
    pub const none = Visibility{ .pos_x = false, .neg_x = false, .pos_y = false, .neg_y = false, .pos_z = false, .neg_z = false };

    /// Get the visibility for a specific axis and direction by index.
    pub fn getIndex(self: Visibility, index: u3) bool {
        const bits: u6 = @bitCast(self);

        return (bits & (1 << index)) != 0;
    }

    /// Set the visibility for a specific axis and direction by index.
    pub fn setIndex(self: *Visibility, index: u3, value: bool) void {
        var bits: u6 = @bitCast(*self);
        if (value) {
            bits |= (1 << index);
        } else {
            bits &= ~(1 << index);
        }
        self.* = @bitCast(bits);
    }

    /// Get the visibility for a specific `AxisDir`.
    pub fn get(self: Visibility, axis_dir: AxisDir) bool {
        return self.getIndex(axis_dir.toIndex());
    }

    /// Set the visibility for a specific `AxisDir`.
    pub fn set(self: *Visibility, axis_dir: AxisDir, value: bool) void {
        self.setIndex(axis_dir.toIndex(), value);
    }
};

/// The main unit of state;
/// depending on context this may represent a volume at
/// `page_scale`, `voxeme_scale`, or `voxel_scale`.
pub const Voxel = packed struct(u32) {
    /// The material type of this voxel.
    material_id: MaterialId,
    /// Material-specific state data, such as orientation.
    state: u20 = 0,

    /// The "no voxel" state.
    pub const empty = Voxel{ .material_id = .none };
};

/// A spacial hash table mapping from `Coord` to index in a MultiArrayList.
pub fn SpacialHashTable(comptime Coord: type, comptime BitCoord: type, comptime max_indirections: comptime_int) type {
    return extern struct {
        const Self = @This();

        const CoordInt = std.meta.Int(.unsigned, @bitSizeOf(Coord));

        fn coordFromInt(value: u32) Coord {
            const bits: CoordInt = @truncate(value);
            return @bitCast(bits);
        }

        fn intFromCoord(coord: Coord) u32 {
            const bits: CoordInt = @bitCast(coord);
            return bits;
        }

        /// Mask for wrapping hash indices.
        pub const indirection_mask = max_indirections - 1;
        /// Marks an empty entry that has never been used.
        pub const sentinel: u32 = std.math.maxInt(u32);
        /// Marks a deleted entry that can be reused.
        pub const tombstone: u32 = sentinel - 1;

        /// The indices into the `pages` MultiArrayList for this page; or `sentinel` if empty, `tombstone` if deleted.
        indices: [max_indirections]u32,
        /// The coordinates for the slots in the table.
        coordinates: [max_indirections]u32,

        /// Initialize the table to empty state.
        pub fn init(self: *Self) void {
            @memset(&self.indices, Self.sentinel);
            @memset(&self.coordinates, 0);
        }

        /// Hash function for `PageCoord`.
        fn hash(coord: Coord) u32 {
            const bits: BitCoord = @bitCast(coord);
            const h = (@as(u32, bits.x) *% 92837111) ^ (@as(u32, bits.y) *% 689287499) ^ (@as(u32, bits.z) *% 283923481);
            return h & indirection_mask;
        }

        /// Look up the index bound to `coord` in the table, or `sentinel` if not found.
        /// This is a non-thread-safe operation; use only from the main thread on the front buffer.
        pub fn lookup(self: *const Self, coord: Coord) u32 {
            var h = hash(coord);
            for (0..max_indirections) |_| {
                const index = self.indices[h];
                if (index == sentinel) {
                    return sentinel; // Chain ends here.
                }
                if (index != tombstone and coordFromInt(self.coordinates[h]) == coord) {
                    return index; // Found it!
                }
                h = (h + 1) & indirection_mask;
            }
            return sentinel;
        }

        /// Insert a new index binding for `coord` into the table, or update an existing binding.
        /// This is a non-thread-safe operation; use only from the main thread on the front buffer.
        pub fn insert(self: *Self, coord: Coord, new_index: u32) bool {
            var h = hash(coord);
            for (0..max_indirections) |_| {
                const index = self.indices[h];
                if (index == sentinel or index == tombstone) {
                    // Empty or deleted slot; claim it.
                    self.indices[h] = new_index;
                    self.coordinates[h] = intFromCoord(coord);
                    return true;
                }
                if (index != tombstone and coordFromInt(self.coordinates[h]) == coord) {
                    // Existing binding; update it.
                    self.indices[h] = new_index;
                    return true;
                }
                h = (h + 1) & indirection_mask;
            }
            return false; // Table full
        }

        /// Remove the index binding for `coord` from the table, if it exists.
        /// This is a non-thread-safe operation; use only from the main thread on the front buffer.
        pub fn remove(self: *Self, coord: Coord) void {
            var h = hash(coord);
            for (0..max_indirections) |_| {
                const index = self.indices[h];
                if (index == sentinel) return;
                if (index != tombstone and self.coordinates[h] == coord) {
                    self.indices[h] = tombstone;
                    return;
                }
                h = (h + 1) & indirection_mask;
            }
        }

        /// Look up the index bound to `coord` in the table, or `sentinel` if not found.
        /// This is a thread safe, lock-free operation. It does have some overhead, however;
        /// so use the non-atomic version from the main thread.
        /// This is only needed for worker threads operation on the back buffer.
        pub fn lookup_atomic(self: *const Self, coord: Coord) u32 {
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
                        if (coordFromInt(coord_at_h) == coord) {
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

        /// Insert a new index binding for `coord` into the table, or update an existing binding.
        /// This is a thread safe, lock-free operation. It does have some overhead, however;
        /// so use the non-atomic version from the main thread.
        /// This is only needed for worker threads operating on the back buffer.
        pub fn insert_atomic(self: *Self, coord: Coord, new_index: u32) bool {
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
                        return true;
                    }
                    continue; // CAS failed, retry.
                }

                // The slot is occupied. We need to verify the coordinate before attempting an update.
                const coord_at_h = self.coordinates[h];
                const index2 = @atomicLoad(u32, &self.indices[h], .acquire);

                if (index1 == index2) {
                    // Consistent read. Now we can check if it's our key.
                    if (coordFromInt(coord_at_h) == coord) {
                        const result = @cmpxchgStrong(u32, &self.indices[h], index1, new_index, .acq_rel, .acquire);
                        if (result == null) return true; // Update succeeded
                        continue; // Update failed, retry.
                    }
                }

                // If inconsistent read or not our key, continue probing.
                h = (h + 1) & indirection_mask;
            }

            return false; // Table full
        }

        /// Remove the index binding for `coord` from the table, if it exists.
        /// This is a thread safe, lock-free operation. It does have some overhead, however;
        /// so use the non-atomic version from the main thread.
        /// This is only needed for worker threads operating on the back buffer.
        pub fn remove_atomic(self: *Self, coord: Coord) void {
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
}

/// A spacial hash table for pages.
pub const PageTable = SpacialHashTable(PageCoord, PageCoord.Unsigned, max_pages);

/// A spacial hash table for voxemes within a page.
pub const VoxemeTable = SpacialHashTable(LocalCoord, LocalCoord, voxemes_per_page);

/// A dense minimum-scale voxel state grid.
pub const VoxelBuffer = [voxels_per_voxeme]Voxel; // 16*16*16 voxels in a voxeme; 16kb
/// A dense minimum-scale voxel visibility grid.
pub const VisibilityBuffer = [voxels_per_voxeme]Visibility; // 4kb

/// A sparse, large-scale voxel grid supporting efficient GPU usage.
/// This data structure is never really constructed, it is just a table layout for MultiArrayList
pub const PageData = struct {
    /// The coordinate of this page in the infinite 3D grid of pages.
    coord: PageCoord,
    /// The homogeneous voxel data for this page.
    /// Even if the page contains voxemes, this is still the default voxel state for implicit voxemes.
    voxel: Voxel,
    /// Coarse visibility data for this page.
    visibility: Visibility,
    /// The indirection table mapping from `LocalCoord` to voxeme index in the `voxemes` MultiArrayList, within this page.
    voxeme_indirection: VoxemeTable,
}; // each page is 32780 bytes

/// Small-scale voxel grid metadata supporting efficient GPU usage.
/// This data structure is never really constructed, it is just a table layout for MultiArrayList
pub const VoxemeData = struct {
    /// The coordinate of this voxeme within its page.
    coord: LocalCoord,
    /// The homogeneous voxel data for this voxeme;
    /// unlike page-level voxel data, this is only valid
    /// if `buffer_indirection` is `BufferData.sentinel`.
    voxel: Voxel,
    /// Coarse visibility data for this voxeme.
    visibility: Visibility,
    /// Index into the `voxels` MultiArrayList for the detailed voxel data for this voxeme, if it is heterogeneous.
    /// Homogeneous voxemes have this set to `BufferData.sentinel`.
    buffer_indirection: u32,
}; // each voxeme is 12 bytes

/// Dense storage for the detailed voxel data of a heterogeneous voxeme, supporting efficient GPU usage.
/// This data structure is never really constructed, it is just a table layout for MultiArrayList
pub const BufferData = struct {
    /// The voxel state data for this buffer.
    voxel: VoxelBuffer,
    /// The visibility data for this buffer.
    visibility: VisibilityBuffer,

    /// Sentinel index value for "no buffer", meaning homogeneous voxeme.
    pub const sentinel = std.math.maxInt(u32);
}; // each buffer is 20480 bytes

// the Grid struct itself is not extern as each address will need to be bound as gpu inputs

/// Allocator used for all memory in the grid.
allocator: std.mem.Allocator,
/// The spacial hash table mapping from `PageCoord` to page index in the `pages` MultiArrayList.
page_indirection: PageTable, // ~512kb
/// The list of pages in the grid in SOA format.
pages: std.MultiArrayList(PageData),
/// The list of voxemes in the grid in SOA format.
voxemes: std.MultiArrayList(VoxemeData),
/// The list of voxel buffers in the grid in SOA format.
voxels: std.MultiArrayList(BufferData),
/// The list of registered material properties for this grid.
material_properties: std.ArrayList(MaterialProperties),
/// Tracks which pages need to be checked for homogenization, pruning, and visibility recalculation.
dirty_page_set: std.DynamicBitSetUnmanaged,
/// Tracks which voxemes need to be checked for homogenization, pruning, and visibility recalculation.
dirty_voxeme_set: std.DynamicBitSetUnmanaged,
/// The list of pages that are free to be reused.
/// Necessary because we cannot move or truly delete pages in the MultiArrayList.
page_free_list: std.ArrayList(u32),
/// The list of voxemes that are free to be reused.
/// Necessary because we cannot move or truly delete voxemes in the MultiArrayList.
voxeme_free_list: std.ArrayList(u32),
/// The list of buffers that are free to be reused.
/// Necessary because we cannot move or truly delete buffers in the MultiArrayList.
buffer_free_list: std.ArrayList(u32),

/// Create a new, empty grid.
/// This incurs several large allocations totalling around 85 megabytes.
pub fn init(allocator: std.mem.Allocator) !*Grid {
    var self = try allocator.create(Grid); // allocate self because the page table is large
    errdefer allocator.destroy(self);

    self.allocator = allocator;
    self.page_indirection.init();
    self.pages = .empty;
    self.voxemes = .empty;
    self.voxels = .empty;
    self.material_properties = .empty;
    self.page_free_list = .empty;
    self.voxeme_free_list = .empty;
    self.buffer_free_list = .empty;

    try self.pages.ensureTotalCapacity(allocator, 1024); // ~32mb
    errdefer self.pages.deinit(allocator);

    try self.voxemes.ensureTotalCapacity(allocator, 1024); // ~9kb
    errdefer self.voxemes.deinit(allocator);

    try self.voxels.ensureTotalCapacity(allocator, 1024); // ~20mb
    errdefer self.voxels.deinit(allocator);

    try self.material_properties.ensureTotalCapacity(allocator, max_materials); // ~32kb
    errdefer self.material_properties.deinit(allocator);

    try self.page_free_list.ensureTotalCapacity(allocator, 1024); // ~4kb
    errdefer self.page_free_list.deinit(allocator);

    try self.voxeme_free_list.ensureTotalCapacity(allocator, 1024); // ~2kb
    errdefer self.voxeme_free_list.deinit(allocator);

    try self.buffer_free_list.ensureTotalCapacity(allocator, 1024); // ~4kb
    errdefer self.buffer_free_list.deinit(allocator);

    self.dirty_page_set = try std.DynamicBitSetUnmanaged.initEmpty(allocator, max_pages); // ~8kb
    errdefer self.dirty_page_set.deinit(allocator);

    self.dirty_voxeme_set = try std.DynamicBitSetUnmanaged.initEmpty(allocator, max_pages * voxemes_per_page); // ~32mb
    errdefer self.dirty_voxeme_set.deinit(allocator);

    // initialize the empty material
    self.material_properties.appendAssumeCapacity(.none);

    return self;
}

/// Deinitialize the grid and free all associated memory.
pub fn deinit(self: *Grid) void {
    self.voxels.deinit(self.allocator);
    self.voxemes.deinit(self.allocator);
    self.pages.deinit(self.allocator);
    self.material_properties.deinit(self.allocator);
    self.page_free_list.deinit(self.allocator);
    self.buffer_free_list.deinit(self.allocator);
    self.voxeme_free_list.deinit(self.allocator);
    self.dirty_page_set.deinit(self.allocator);
    self.dirty_voxeme_set.deinit(self.allocator);
    self.allocator.destroy(self);
}

/// Clear the grid state but retain all allocated memory.
/// * Only clears registered materials if `clear_materials` is true.
pub fn clear(self: *Grid, clear_materials: bool) void {
    self.page_indirection.init();
    self.pages.clearRetainingCapacity();
    self.voxemes.clearRetainingCapacity();
    self.voxels.clearRetainingCapacity();
    self.page_free_list.clearRetainingCapacity();
    self.voxeme_free_list.clearRetainingCapacity();
    self.buffer_free_list.clearRetainingCapacity();
    if (clear_materials) self.material_properties.clearRetainingCapacity();
    self.dirty_page_set.unsetAll();
    self.dirty_voxeme_set.unsetAll();
}

/// The number of live pages in the grid.
pub fn pageCount(self: *const Grid) usize {
    return self.pages.len - self.page_free_list.items.len;
}

/// The number of live voxemes in the grid.
pub fn voxemeCount(self: *const Grid) usize {
    return self.voxemes.len - self.voxeme_free_list.items.len;
}

/// The number of live voxel buffers in the grid.
pub fn bufferCount(self: *const Grid) usize {
    return self.voxels.len - self.buffer_free_list.items.len;
}

/// The total number of live small voxels in the grid across all heterogeneous voxemes.
pub fn voxelCount(self: *const Grid) usize {
    return self.bufferCount() * voxels_per_voxeme;
}

/// The number of registered materials in the grid.
pub fn materialCount(self: *const Grid) usize {
    return self.material_properties.items.len;
}

/// Register a new material type for use in voxels.
/// Returns the MaterialId for the new material type.
pub fn registerMaterial(self: *Grid, properties: MaterialProperties) !MaterialId {
    const index = self.materialCount();
    if (index >= max_materials) {
        return error.OutOfMaterials;
    }

    self.material_properties.appendAssumeCapacity(properties);

    return @enumFromInt(index);
}

/// Get the properties for a material type by its MaterialId.
pub fn getMaterial(self: *const Grid, id: MaterialId) *const MaterialProperties {
    const index = @intFromEnum(id);
    return &self.material_properties.items[index];
}

/// Check if a voxel at the given global voxel coordinate is occluding.
pub fn isOpaqueVoxel(self: *const Grid, global_voxel: vec3i) bool {
    const vox = self.getVoxel(global_voxel);
    const mat = self.getMaterial(vox.material_id);
    return mat.flags.is_opaque;
}

/// Get the voxel data at the given global voxel coordinate.
/// This always returns a value; unloaded areas return `Voxel.empty`.
pub fn getVoxel(world: *const Grid, global_voxel: vec3i) Voxel {
    // Find which Page it's in.
    const page_coord = convert.globalVoxelToPageCoord(global_voxel);
    const page_index = world.page_indirection.lookup(page_coord);
    if (page_index == PageTable.sentinel) {
        return .empty; // Page doesn't exist.
    }

    // Find which Voxeme it's in *within that page*.
    const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(global_voxel);

    // Get the specific VoxemeTable for our page.
    const voxeme_table: *const VoxemeTable = &world.pages.items(.voxeme_indirection)[page_index];

    const voxeme_index = voxeme_table.lookup(local_voxeme_coord);
    if (voxeme_index == VoxemeTable.sentinel) {
        // Voxeme doesn't exist, so return the page's homogeneous voxel data.
        return world.pages.items(.voxel)[page_index];
    }

    // Get the data for that voxeme.
    const voxeme_voxel = world.voxemes.items(.voxel)[voxeme_index];
    const buffer_handle = world.voxemes.items(.buffer_indirection)[voxeme_index];
    if (buffer_handle == BufferData.sentinel) { // Check if it's homogeneous
        return voxeme_voxel; // It's a homogeneous voxeme.
    }

    // It's a heterogeneous voxeme. Find the specific voxel.
    const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);
    const index_in_buffer = convert.localVoxelCoordToIndex(local_voxel_coord);

    // The buffer handle is an index into the final `voxels` ArrayList.
    const buffer = &world.voxels.items(.voxel)[buffer_handle];

    return buffer[index_in_buffer]; // Final Voxel!
}

/// Set the voxel data at the given global voxel coordinate.
/// This may allocate memory if the relevant page or voxeme does not yet exist,
/// or is currently homogeneous and needs to be broken into a heterogeneous voxeme.
/// Marks the relevant voxeme and page as dirty for later processing.
pub fn setVoxel(self: *Grid, global_voxel: vec3i, new_voxel: Voxel) !void {
    // --- FIND OR CREATE PAGE ---
    const page_coord = convert.globalVoxelToPageCoord(global_voxel);
    const page_indirect_index = self.page_indirection.lookup(page_coord);
    var page_index: u32 = undefined;

    if (page_indirect_index == PageTable.sentinel) { // page not bound
        // === CREATING A NEW PAGE ===
        page_index = if (self.page_free_list.pop()) |idx| idx else @intCast(try self.pages.addOne(self.allocator));
        errdefer self.page_free_list.appendAssumeCapacity(page_index);

        // Reserve the spot in the indirection table FIRST.
        const success = self.page_indirection.insert(page_coord, page_index);
        if (!success) return error.OutOfPages;

        self.pages.items(.coord)[page_index] = page_coord;
        self.pages.items(.visibility)[page_index] = .none;
        self.pages.items(.voxel)[page_index] = .empty;
        self.pages.items(.voxeme_indirection)[page_index].init();
    } else { // page bound
        // === MODIFYING AN EXISTING PAGE ===
        page_index = @intCast(page_indirect_index);
    }

    // --- FIND OR CREATE VOXEME ---
    const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(global_voxel);
    const voxeme_table: *VoxemeTable = &self.pages.items(.voxeme_indirection)[page_index];
    const voxeme_indirect_index = voxeme_table.lookup(local_voxeme_coord);

    if (voxeme_indirect_index == VoxemeTable.sentinel) { // voxeme not bound
        // === BREAKING A HOMOGENEOUS PAGE ===
        const page_homogeneous_voxel = self.pages.items(.voxel)[page_index];
        if (page_homogeneous_voxel == new_voxel) return;

        // Reserve the spot in the page's voxeme table FIRST.
        const new_voxeme_index: u32 = if (self.voxeme_free_list.pop()) |idx| idx else @intCast(try self.voxemes.addOne(self.allocator));
        errdefer self.voxeme_free_list.appendAssumeCapacity(new_voxeme_index);

        const success = voxeme_table.insert(local_voxeme_coord, new_voxeme_index);
        if (!success) return error.OutOfVoxemes;

        // Now we are committed. Allocate the buffer.
        const new_buffer_index: u32 = if (self.buffer_free_list.pop()) |idx| idx else @intCast(try self.voxels.addOne(self.allocator));
        const new_voxels = &self.voxels.items(.voxel)[new_buffer_index];
        const new_visibility = &self.voxels.items(.visibility)[new_buffer_index];
        @memset(new_voxels, page_homogeneous_voxel);
        @memset(new_visibility, Visibility.none); // ensure its at least not `undefined`; TODO: is this necessary?

        const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);
        const index_in_buffer = convert.localVoxelCoordToIndex(local_voxel_coord);
        new_voxels[index_in_buffer] = new_voxel;

        // Append the voxeme data.
        self.voxemes.items(.coord)[new_voxeme_index] = local_voxeme_coord;
        self.voxemes.items(.visibility)[new_voxeme_index] = .none;
        self.voxemes.items(.voxel)[new_voxeme_index] = .empty; // not valid with buffer handle
        self.voxemes.items(.buffer_indirection)[new_voxeme_index] = new_buffer_index;
    } else { // voxeme bound
        // === MODIFYING AN EXISTING VOXEME ===
        const voxeme_data = &self.voxemes.items(.voxel)[voxeme_indirect_index];
        const buffer_handle = &self.voxemes.items(.buffer_indirection)[voxeme_indirect_index];

        if (buffer_handle.* == BufferData.sentinel) { // buffer not bound
            // Break a homogeneous voxeme.
            if (voxeme_data.* == new_voxel) return;

            const new_buffer_index: u32 = if (self.buffer_free_list.pop()) |idx| idx else @intCast(try self.voxels.addOne(self.allocator));
            const new_voxels = &self.voxels.items(.voxel)[new_buffer_index];
            const new_visibility = &self.voxels.items(.visibility)[new_buffer_index];
            @memset(new_voxels, voxeme_data.*);
            @memset(new_visibility, Visibility.none); // ensure its at least not `undefined`; TODO: is this necessary?

            const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);
            const index_in_buffer = convert.localVoxelCoordToIndex(local_voxel_coord);
            new_voxels[index_in_buffer] = new_voxel;

            buffer_handle.* = new_buffer_index;
            voxeme_data.* = Voxel.empty;
        } else { // buffer bound
            // Modify a heterogeneous voxeme.
            const buffer = &self.voxels.items(.voxel)[buffer_handle.*];
            const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);
            const index_in_buffer = convert.localVoxelCoordToIndex(local_voxel_coord);
            const voxel = &buffer[index_in_buffer];

            // no need to remap visibility here, that is done in a separate pass

            if (voxel.* == new_voxel) return;

            voxel.* = new_voxel;
        }
    }

    // --- MARK DIRTY ---
    const local_voxeme_idx = convert.localVoxemeCoordToIndex(local_voxeme_coord);
    const dirty_index = @as(usize, page_index) * voxemes_per_page + local_voxeme_idx;
    self.dirty_voxeme_set.set(dirty_index);
    self.dirty_page_set.set(page_index);

    for (offsets) |offset| {
        const neighbor_global_voxel = global_voxel + offset;
        const neighbor_page_coord = convert.globalVoxelToPageCoord(neighbor_global_voxel);
        const neighbor_page_index = self.page_indirection.lookup(neighbor_page_coord);
        if (neighbor_page_index != PageTable.sentinel) {
            self.dirty_page_set.set(@as(usize, neighbor_page_index));
            // Mark the neighbor voxeme dirty too, if it exists.
            const neighbor_local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(neighbor_global_voxel);
            const neighbor_voxeme_table: *VoxemeTable = &self.pages.items(.voxeme_indirection)[neighbor_page_index];
            const neighbor_voxeme_index = neighbor_voxeme_table.lookup(neighbor_local_voxeme_coord);
            if (neighbor_voxeme_index != VoxemeTable.sentinel) {
                const neighbor_local_voxeme_idx = convert.localVoxemeCoordToIndex(neighbor_local_voxeme_coord);
                const neighbor_dirty_index = @as(usize, neighbor_page_index) * voxemes_per_page + neighbor_local_voxeme_idx;
                self.dirty_voxeme_set.set(neighbor_dirty_index);
            }
        }
    }
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
        // A page's coordinate is the global voxel coordinate divided by the number of voxels
        // per page on each axis. This total shift is the combination of the page's
        // dimensions in voxemes and the voxeme's dimensions in voxels.
        // Fast floor division by (page_axis_divisor * voxeme_axis_divisor) using a shift.
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
        // Get the global coordinate of the containing voxeme by dividing by voxeme size.
        const global_voxeme_coord = v >> comptime @splat(voxeme_axis_shift);
        // Get the local part of that coordinate within the page grid using a fast modulo.
        // A page has `page_axis_divisor` voxemes along each axis.
        const local_coord = global_voxeme_coord & comptime @as(vec3i, @splat(page_axis_mask));
        // Pack into the struct.
        return .{
            .x = @intCast(local_coord[0]),
            .y = @intCast(local_coord[1]),
            .z = @intCast(local_coord[2]),
        };
    }

    /// Global voxel coordinate to the local coordinate of the voxel *within its voxeme*.
    /// Used for indexing into a dense Voxel Buffer.
    pub inline fn globalVoxelToLocalVoxelCoord(v: vec3i) LocalCoord {
        // A voxeme has `voxeme_axis_divisor` voxels along each axis.
        // Fast modulo by that dimension gets the local coordinate.
        const base = v & comptime @as(vec3i, @splat(voxeme_axis_mask));
        return LocalCoord{
            .x = @intCast(base[0]),
            .y = @intCast(base[1]),
            .z = @intCast(base[2]),
        };
    }

    // --- Local Coords to 1D Buffer Indices ---

    /// 3D local voxeme coordinate (0-15) to a 1D buffer index for a page's VoxemeTable.
    pub inline fn localVoxemeCoordToIndex(local_coord: LocalCoord) u32 {
        // Standard 3D to 1D mapping: x + y*WIDTH + z*WIDTH*HEIGHT
        // Page WIDTH and HEIGHT in voxemes are both `page_axis_divisor` (16).
        // x + y * 16 + z * 256
        const x = @as(u32, local_coord.x);
        const y = @as(u32, local_coord.y);
        const z = @as(u32, local_coord.z);
        return x + (y << page_axis_shift) + (z << (page_axis_shift * 2));
    }

    /// 3D local voxel coordinate (0-15) to a 1D buffer index for a voxeme's VoxelBuffer.
    pub inline fn localVoxelCoordToIndex(local_coord: LocalCoord) u32 {
        // Standard 3D to 1D mapping: x + y*WIDTH + z*WIDTH*HEIGHT
        // Voxeme WIDTH and HEIGHT in voxels are both `voxeme_axis_divisor` (16).
        // x + y * 16 + z * 256
        const x = @as(u32, local_coord.x);
        const y = @as(u32, local_coord.y);
        const z = @as(u32, local_coord.z);
        return x + (y << voxeme_axis_shift) + (z << (voxeme_axis_shift * 2));
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

        // Scale each coordinate up to the global voxel grid and add them together.
        // Page coord is scaled by total voxels per page axis.
        // Local voxeme coord is scaled by total voxels per voxeme axis.
        // Local voxel coord is the final offset.
        return (page_base << comptime @splat(total_shift)) +
            (local_voxeme_base << comptime @splat(voxeme_axis_shift)) +
            local_voxel_base;
    }

    /// Global voxel coordinate back to a world-space position.
    /// This gives the coordinate of the minimum corner of the voxel.
    pub inline fn globalVoxelToWorld(v: vec3i) vec3 {
        return @as(vec3, @floatFromInt(v)) * comptime @as(vec3, @splat(voxel_scale));
    }
};
