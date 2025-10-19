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
const vec4 = linalg.vec4;
const mat4 = linalg.mat4;
const vec3i = linalg.vec3i;
const vec3u = linalg.vec3u;
const aabb3 = linalg.aabb3;
const aabb3i = linalg.aabb3i;

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

/// The number of voxemes on a single face of a page.
pub const voxemes_per_page_face = page_axis_divisor * page_axis_divisor;

/// The number of voxels on a single face of a voxeme.
pub const voxels_per_voxeme_face = voxeme_axis_divisor * voxeme_axis_divisor;

/// A coordinate identifying a page in the infinite 3D grid of pages.
pub const PageCoord = packed struct(u32) {
    x: i11, // 2048 pages in x, y
    y: i11,
    z: i10, // 1024 pages in z

    /// Unsigned version of `PageCoord`, useful for bitcasting before hashing.
    pub const Unsigned = packed struct(u32) { x: u11, y: u11, z: u10 };

    pub fn toVec3i(self: PageCoord) vec3i {
        return vec3i{
            @intCast(self.x),
            @intCast(self.y),
            @intCast(self.z),
        };
    }

    pub fn fromVec3i(v: vec3i) PageCoord {
        return PageCoord{
            .x = @as(i11, @intCast(v[0])),
            .y = @as(i11, @intCast(v[1])),
            .z = @as(i10, @intCast(v[2])),
        };
    }

    pub fn add(self: PageCoord, other: PageCoord) PageCoord {
        return PageCoord{
            .x = self.x + other.x,
            .y = self.y + other.y,
            .z = self.z + other.z,
        };
    }

    pub fn addOffset(self: PageCoord, offset: vec3i) PageCoord {
        return PageCoord{
            .x = self.x + @as(i11, @intCast(offset[0])),
            .y = self.y + @as(i11, @intCast(offset[1])),
            .z = self.z + @as(i10, @intCast(offset[2])),
        };
    }
};

/// A coordinate identifying a voxeme within a page, or a voxel within a voxeme.
pub const LocalCoord = packed struct(u12) {
    x: u4,
    y: u4,
    z: u4,

    pub fn fromVec3i(v: vec3i) LocalCoord {
        return LocalCoord{
            .x = @intCast(v[0]),
            .y = @intCast(v[1]),
            .z = @intCast(v[2]),
        };
    }

    pub fn toVec3i(self: LocalCoord) vec3i {
        return vec3i{
            @intCast(self.x),
            @intCast(self.y),
            @intCast(self.z),
        };
    }

    pub fn add(self: LocalCoord, other: LocalCoord) LocalCoord {
        return LocalCoord{
            .x = self.x + other.x,
            .y = self.y + other.y,
            .z = self.z + other.z,
        };
    }

    pub fn addOffset(self: LocalCoord, offset: vec3i) LocalCoord {
        return LocalCoord{
            .x = @intCast(@as(i32, @intCast(self.x)) + offset[0]),
            .y = @intCast(@as(i32, @intCast(self.y)) + offset[1]),
            .z = @intCast(@as(i32, @intCast(self.z)) + offset[2]),
        };
    }
};

/// Unique identifier for a voxel material type.
/// Supports up to 4096 materials including the empty state. See `MaterialData`, `registerMaterial`.
pub const MaterialId = enum(u12) { none = 0, _ };

/// Maximum number of materials supported by the grid. (4096 including empty)
pub const max_materials = std.math.maxInt(std.meta.Tag(MaterialId));

/// Surface properties for a voxel material type. See `MaterialId`, `registerMaterial`.
/// This data structure is not passed to the gpu, it is just a table layout for MultiArrayList.
pub const MaterialData = struct {
    /// The base color of this material type before lighting.
    color: Color,
    /// State flags for this material type, such as whether it is opaque (occludes other voxels, light, etc).
    flags: MaterialFlags,

    /// The properties for the "no material" material / default state.
    pub const none = MaterialData{
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

/// State flags for a voxel material type. See `MaterialData`.
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

fn generatePageFace(axis_idx: u2, const_val: u4) [voxemes_per_page_face]LocalCoord {
    var face_coords: [voxemes_per_page_face]LocalCoord = undefined;
    var i = 0;
    var v = 0;
    @setEvalBranchQuota(voxemes_per_page);
    while (v < page_axis_divisor) : (v += 1) {
        var u = 0;
        while (u < page_axis_divisor) : (u += 1) {
            face_coords[i] = switch (axis_idx) {
                // +X/-X face, iterate over Y and Z
                0 => LocalCoord{ .x = const_val, .y = @intCast(u), .z = @intCast(v) },
                // +Y/-Y face, iterate over X and Z
                1 => LocalCoord{ .x = @intCast(u), .y = const_val, .z = @intCast(v) },
                // +Z/-Z face, iterate over X and Y
                2 => LocalCoord{ .x = @intCast(u), .y = @intCast(v), .z = const_val },
                else => unreachable,
            };
            i += 1;
        }
    }
    return face_coords;
}

fn generateVoxemeFace(axis_idx: u2, const_val: u4) [voxels_per_voxeme_face]LocalCoord {
    var face_coords: [voxels_per_voxeme_face]LocalCoord = undefined;
    var i = 0;
    var v = 0;
    @setEvalBranchQuota(voxels_per_voxeme);
    while (v < voxeme_axis_divisor) : (v += 1) {
        var u = 0;
        while (u < voxeme_axis_divisor) : (u += 1) {
            face_coords[i] = switch (axis_idx) {
                // +X/-X face, iterate over Y and Z
                0 => LocalCoord{ .x = const_val, .y = @intCast(u), .z = @intCast(v) },
                // +Y/-Y face, iterate over X and Z
                1 => LocalCoord{ .x = @intCast(u), .y = const_val, .z = @intCast(v) },
                // +Z/-Z face, iterate over X and Y
                2 => LocalCoord{ .x = @intCast(u), .y = @intCast(v), .z = const_val },
                else => unreachable,
            };
            i += 1;
        }
    }
    return face_coords;
}

/// Precomputed local coordinates for all voxemes on each of the 6 faces of a page.
/// The outer index corresponds to the `offsets` array index (0: +X, 1: -X, etc.).
pub const page_face_voxeme_coords = blk: {
    var coords: [6][voxemes_per_page_face]LocalCoord = undefined;

    // The face on the *current* page that borders the neighbor in that direction.
    coords[0] = generatePageFace(0, page_axis_mask); // +X face (x = 15)
    coords[1] = generatePageFace(0, 0); // -X face (x = 0)
    coords[2] = generatePageFace(1, page_axis_mask); // +Y face (y = 15)
    coords[3] = generatePageFace(1, 0); // -Y face (y = 0)
    coords[4] = generatePageFace(2, page_axis_mask); // +Z face (z = 15)
    coords[5] = generatePageFace(2, 0); // -Z face (z = 0)

    break :blk coords;
};

/// Precomputed local coordinates for all voxels on each of the 6 faces of a voxeme.
pub const voxeme_face_voxel_coords = blk: {
    var coords: [6][voxels_per_voxeme_face]LocalCoord = undefined;

    // The face on the *current* voxeme that borders the neighbor in that direction.
    coords[0] = generateVoxemeFace(0, voxeme_axis_mask); // +X face (x = 15)
    coords[1] = generateVoxemeFace(0, 0); // -X face (x = 0)
    coords[2] = generateVoxemeFace(1, voxeme_axis_mask); // +Y face (y = 15)
    coords[3] = generateVoxemeFace(1, 0); // -Y face (y = 0)
    coords[4] = generateVoxemeFace(2, voxeme_axis_mask); // +Z face (z = 15)
    coords[5] = generateVoxemeFace(2, 0); // -Z face (z = 0)

    break :blk coords;
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

    pub fn fromIndex(index: anytype) Axis {
        return @enumFromInt(@as(u32, @intCast(index)) & 0b110);
    }

    pub fn toComponentIndex(self: Axis) u2 {
        return switch (self) {
            .x => 0,
            .y => 1,
            .z => 2,
        };
    }
};

/// Axis and direction identifier.
pub const AxisDir = packed struct(u8) {
    axis: Axis,
    positive: bool,
    _unused: u4 = 0,

    pub const x_pos = AxisDir{ .axis = .x, .positive = true };
    pub const x_neg = AxisDir{ .axis = .x, .positive = false };
    pub const y_pos = AxisDir{ .axis = .y, .positive = true };
    pub const y_neg = AxisDir{ .axis = .y, .positive = false };
    pub const z_pos = AxisDir{ .axis = .z, .positive = true };
    pub const z_neg = AxisDir{ .axis = .z, .positive = false };

    pub fn toIndex(self: AxisDir) u3 {
        return self.axis.toIndex() + @intFromBool(!self.positive);
    }

    pub fn fromIndex(index: anytype) AxisDir {
        return AxisDir{
            .axis = Axis.fromIndex(index),
            .positive = (@as(u3, @intCast(index)) & 1) == 0,
        };
    }

    pub fn getOffset(self: AxisDir) vec3i {
        return offsets[self.toIndex()];
    }

    pub fn toComponentIndex(self: AxisDir) u2 {
        return self.axis.toComponentIndex();
    }

    pub const values = [_]AxisDir{
        .x_pos,
        .x_neg,
        .y_pos,
        .y_neg,
        .z_pos,
        .z_neg,
    };
};

/// Visibility flags for the 6 faces of a volume.
pub const Visibility = packed struct(u6) {
    pos_x: bool,
    neg_x: bool,
    pos_y: bool,
    neg_y: bool,
    pos_z: bool,
    neg_z: bool,

    /// Completely unoccluded state.
    pub const all = Visibility{ .pos_x = true, .neg_x = true, .pos_y = true, .neg_y = true, .pos_z = true, .neg_z = true };

    /// Completely occluded state.
    pub const none = Visibility{ .pos_x = false, .neg_x = false, .pos_y = false, .neg_y = false, .pos_z = false, .neg_z = false };

    /// Get the visibility for a specific axis and direction by index.
    pub fn getIndex(self: Visibility, index: u3) bool {
        const bits: u6 = @bitCast(self);

        return (bits & (@as(u6, 1) << index)) != 0;
    }

    /// Set the visibility for a specific axis and direction by index.
    pub fn setIndex(self: *Visibility, index: u3, value: bool) void {
        var bits: u6 = @bitCast(self.*);
        if (value) {
            bits |= (@as(u6, 1) << index);
        } else {
            bits &= ~(@as(u6, 1) << index);
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
/// This structure is not thread-safe and relies on the command queue architecture implemented in the update manager.
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
        /// The number of valid entries in the table.
        len: u32,

        /// Initialize the table to empty state.
        pub fn init(self: *Self) void {
            @memset(&self.indices, Self.sentinel);
            @memset(&self.coordinates, 0);
            self.len = 0;
        }

        /// Copy the table state from another table into this one
        pub fn copy(self: *Self, other: *const Self) void {
            @memcpy(&self.indices, &other.indices);
            @memcpy(&self.coordinates, &other.coordinates);
            self.len = other.len;
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
                    self.len += 1;
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
                if (index != tombstone and coordFromInt(self.coordinates[h]) == coord) {
                    self.indices[h] = tombstone;
                    self.len -= 1;
                    return;
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

/// A fixed-size bit set for tracking dirty state, etc.
/// This works for all of our bitsets, where std.StaticBitSet causes segfaults on larger sizes.
pub fn FixedBitSet(comptime N: comptime_int) type {
    if (N == 0) @compileError("FixedBitSet size cannot be zero");

    return extern struct {
        const Self = @This();
        const Word = u64;
        const word_bits = @bitSizeOf(Word);
        pub const num_words = (N + word_bits - 1) / word_bits;

        words: [num_words]Word,

        /// Sets the bit at the given index to 1.
        pub fn set(self: *Self, index: u32) void {
            std.debug.assert(index < N);
            const word_index = index / word_bits;
            const bit_index = index % word_bits;
            self.words[word_index] |= (@as(Word, 1) << @intCast(bit_index));
        }

        /// Sets the bit at the given index to 0.
        pub fn unset(self: *Self, index: u32) void {
            std.debug.assert(index < N);
            const word_index = index / word_bits;
            const bit_index = index % word_bits;
            self.words[word_index] &= ~(@as(Word, 1) << @intCast(bit_index));
        }

        /// Returns true if the bit at the given index is 1.
        pub fn isSet(self: *const Self, index: u32) bool {
            std.debug.assert(index < N);
            const word_index = index / word_bits;
            const bit_index = index % word_bits;
            return (self.words[word_index] & (@as(Word, 1) << @intCast(bit_index))) != 0;
        }

        /// Sets all bits to 1.
        pub fn setAll(self: *Self) void {
            @memset(&self.words, std.math.maxInt(Word));
        }

        /// Sets all bits to 0.
        pub fn unsetAll(self: *Self) void {
            @memset(&self.words, 0);
        }

        /// Copy state from another bit set into this one.
        pub fn copy(self: *Self, other: *const Self) void {
            @memcpy(&self.words, &other.words);
        }

        /// Counts the number of set bits.
        pub fn count(self: *const Self) u32 {
            var total: u32 = 0;
            for (self.words) |word| {
                total += @popCount(word);
            }
            return total;
        }

        /// An iterator that yields the index of each set bit.
        pub const Iterator = struct {
            words: []const Word,
            word_index: u32 = 0,
            current_word: Word = 0,

            pub fn next(self: *Iterator) ?u32 {
                while (self.current_word == 0) {
                    if (self.word_index >= num_words) return null;
                    self.current_word = self.words[self.word_index];
                    self.word_index += 1;
                }

                const bit_offset = @ctz(self.current_word);
                const bit_index = (self.word_index - 1) * word_bits + bit_offset;

                // Unset this bit in our temporary copy so we find the next one
                self.current_word &= self.current_word - 1;

                return bit_index;
            }
        };

        /// Returns an iterator over the set bits.
        pub fn iterator(self: *const Self) Iterator {
            return .{ .words = &self.words };
        }
    };
}

/// A dense minimum-scale voxel state grid.
pub const VoxelBuffer = [voxels_per_voxeme]Voxel; // 16*16*16 voxels in a voxeme; 16kb
/// A dense minimum-scale voxel visibility grid.
pub const VisibilityBuffer = [voxels_per_voxeme]Visibility; // 4kb

/// A sparse, large-scale voxel grid supporting efficient GPU usage.
/// This data structure is not passed to the gpu, it is just a table layout for MultiArrayList
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
    /// Tracks which voxemes need to be checked for homogenization, pruning, and visibility recalculation.
    dirty_voxeme_set: FixedBitSet(voxemes_per_page),
}; // each page is 33292 bytes

/// Small-scale voxel grid metadata supporting efficient GPU usage.
/// This data structure is not passed to the gpu, it is just a table layout for MultiArrayList
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
/// This data structure is not passed to the gpu, it is just a table layout for MultiArrayList
pub const BufferData = struct {
    /// The dirty bits for this buffer
    dirty_voxel_set: FixedBitSet(voxels_per_voxeme),
    /// The voxel state data for this buffer.
    voxel: VoxelBuffer,
    /// The visibility data for this buffer.
    visibility: VisibilityBuffer,

    /// Sentinel index value for "no buffer", meaning homogeneous voxeme.
    pub const sentinel = std.math.maxInt(u32);
}; // each buffer is 20992 bytes

/// Async command to modify the grid state.
/// These are generated on the main thread and applied on the manager thread.
pub const Command = union(enum) {
    set_aabb: struct {
        aabb: aabb3i,
        voxel: Voxel,
    },
    set_page: struct {
        coord: PageCoord,
        voxel: Voxel,
    },
    set_voxeme: struct {
        page_coord: PageCoord,
        local_coord: LocalCoord,
        voxel: Voxel,
    },
    set_voxel: struct {
        global_voxel: vec3i,
        voxel: Voxel,
    },
};

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
materials: std.MultiArrayList(MaterialData),
/// The list of pages that are free to be reused.
/// Necessary because we cannot move or truly delete pages in the MultiArrayList.
page_free_list: std.ArrayList(u32),
/// The list of voxemes that are free to be reused.
/// Necessary because we cannot move or truly delete voxemes in the MultiArrayList.
voxeme_free_list: std.ArrayList(u32),
/// The list of buffers that are free to be reused.
/// Necessary because we cannot move or truly delete buffers in the MultiArrayList.
buffer_free_list: std.ArrayList(u32),
/// Tracks which pages need to be checked for homogenization, pruning, and visibility recalculation.
dirty_page_set: FixedBitSet(max_pages), // 8kb

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
    self.materials = .empty;
    self.page_free_list = .empty;
    self.voxeme_free_list = .empty;
    self.buffer_free_list = .empty;

    try self.pages.ensureTotalCapacity(allocator, 1024); // ~32mb
    errdefer self.pages.deinit(allocator);

    try self.voxemes.ensureTotalCapacity(allocator, 1024); // ~9kb
    errdefer self.voxemes.deinit(allocator);

    try self.voxels.ensureTotalCapacity(allocator, 1024); // ~20mb
    errdefer self.voxels.deinit(allocator);

    try self.materials.ensureTotalCapacity(allocator, max_materials); // ~32kb
    errdefer self.materials.deinit(allocator);
    @memset(self.materials.items(.color), MaterialData.none.color);
    @memset(self.materials.items(.flags), MaterialData.none.flags);

    try self.page_free_list.ensureTotalCapacity(allocator, 1024); // ~4kb
    errdefer self.page_free_list.deinit(allocator);

    try self.voxeme_free_list.ensureTotalCapacity(allocator, 1024); // ~2kb
    errdefer self.voxeme_free_list.deinit(allocator);

    try self.buffer_free_list.ensureTotalCapacity(allocator, 1024); // ~4kb
    errdefer self.buffer_free_list.deinit(allocator);

    self.dirty_page_set.unsetAll();

    // initialize the empty material
    self.materials.appendAssumeCapacity(.none);

    return self;
}

/// Deinitialize the grid and free all associated memory.
pub fn deinit(self: *Grid) void {
    self.voxels.deinit(self.allocator);
    self.voxemes.deinit(self.allocator);
    self.pages.deinit(self.allocator);
    self.materials.deinit(self.allocator);
    self.page_free_list.deinit(self.allocator);
    self.buffer_free_list.deinit(self.allocator);
    self.voxeme_free_list.deinit(self.allocator);
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
    if (clear_materials) self.materials.clearRetainingCapacity();
    self.dirty_page_set.unsetAll();
}

/// Copy the grid state, allocating a new Grid.
pub fn clone(self: *Grid) !*Grid {
    const new_self = try self.allocator.create(Grid);
    errdefer self.allocator.destroy(new_self);

    new_self.allocator = self.allocator;

    new_self.page_indirection.copy(&self.page_indirection);

    new_self.voxemes = try self.voxemes.clone(self.allocator);
    errdefer new_self.voxemes.deinit(self.allocator);

    new_self.pages = try self.pages.clone(self.allocator);
    errdefer new_self.pages.deinit(self.allocator);

    new_self.voxels = try self.voxels.clone(self.allocator);
    errdefer new_self.voxels.deinit(self.allocator);

    new_self.materials = try self.materials.clone(self.allocator);
    errdefer new_self.materials.deinit(self.allocator);

    new_self.page_free_list = try self.page_free_list.clone(self.allocator);
    errdefer new_self.page_free_list.deinit(self.allocator);

    new_self.voxeme_free_list = try self.voxeme_free_list.clone(self.allocator);
    errdefer new_self.voxeme_free_list.deinit(self.allocator);

    new_self.buffer_free_list = try self.buffer_free_list.clone(self.allocator);
    errdefer new_self.buffer_free_list.deinit(self.allocator);

    new_self.dirty_page_set.copy(&self.dirty_page_set);

    return new_self;
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
    return self.materials.len;
}

/// Register a new material type for use in voxels.
/// Returns the MaterialId for the new material type.
pub fn registerMaterial(self: *Grid, properties: MaterialData) !MaterialId {
    const index = self.materialCount();
    if (index >= max_materials) {
        return error.OutOfMaterials;
    }

    self.materials.appendAssumeCapacity(properties);

    return @enumFromInt(index);
}

/// Get the properties for a material type by its MaterialId.
pub fn getMaterial(self: *const Grid, id: MaterialId) MaterialData {
    const index = @intFromEnum(id);
    return self.materials.get(index);
}

/// Get the opacity of a material by its MaterialId.
pub fn isOpaqueMaterial(self: *const Grid, id: MaterialId) bool {
    const index = @intFromEnum(id);
    return self.materials.items(.flags)[index].is_opaque;
}

/// Check if a voxel at the given global voxel coordinate is occluding.
pub fn isOpaqueVoxel(self: *const Grid, global_voxel: vec3i) bool {
    const vox = self.getVoxel(global_voxel);
    return self.isOpaqueMaterial(vox.material_id);
}

/// Get the voxel data at the given global voxel coordinate.
/// This always returns a value; unloaded areas return `Voxel.empty`.
/// Note that this does not return updated state for voxels that have been modified this frame.
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

/// Applies a list of commands to the grid. This is intended to be called by the Manager on the back buffer.
pub fn applyCommands(self: *Grid, commands: []const Command) !void {
    for (commands) |command| {
        switch (command) {
            .set_aabb => |cmd| try self.setAABB(cmd.aabb, cmd.voxel),
            .set_page => |cmd| try self.setPage(cmd.coord, cmd.voxel),
            .set_voxeme => |cmd| try self.setVoxeme(cmd.page_coord, cmd.local_coord, cmd.voxel),
            .set_voxel => |cmd| try self.setVoxel(cmd.global_voxel, cmd.voxel),
        }
    }
}

fn setAABB(self: *Grid, aabb: aabb3i, new_voxel: Voxel) !void {
    const min_v = aabb[0];
    const max_v = aabb[1];

    const min_page_coord = convert.globalVoxelToPageCoord(min_v);
    const max_page_coord_inclusive = convert.globalVoxelToPageCoord(max_v - vec3i{ 1, 1, 1 });

    var page_z = min_page_coord.z;
    while (page_z <= max_page_coord_inclusive.z) : (page_z += 1) {
        var page_y = min_page_coord.y;
        while (page_y <= max_page_coord_inclusive.y) : (page_y += 1) {
            var page_x = min_page_coord.x;
            while (page_x <= max_page_coord_inclusive.x) : (page_x += 1) {
                const page_coord = PageCoord{ .x = page_x, .y = page_y, .z = page_z };

                const page_min_voxel = convert.partsToGlobalVoxel(page_coord, .{ .x = 0, .y = 0, .z = 0 }, .{ .x = 0, .y = 0, .z = 0 });
                const page_max_voxel = page_min_voxel + @as(vec3i, @splat(page_axis_divisor * voxeme_axis_divisor));
                const page_aabb = aabb3i{ page_min_voxel, page_max_voxel };

                if (linalg.aabb_contains(aabb, page_aabb)) {
                    // Page is fully contained.
                    const page_index = try self.getOrCreatePage(page_coord, new_voxel) orelse continue;
                    try self.setPageCore(page_coord, page_index, new_voxel);
                    if (new_voxel == Voxel.empty) continue; // Page was freed.

                    // --- Optimized Dirty-Marking for Shell ---
                    self.dirty_page_set.set(page_index);
                    self.pages.items(.dirty_voxeme_set)[page_index].setAll();

                    const is_on_shell = [6]bool{
                        page_coord.x == max_page_coord_inclusive.x, // +X
                        page_coord.x == min_page_coord.x, // -X
                        page_coord.y == max_page_coord_inclusive.y, // +Y
                        page_coord.y == min_page_coord.y, // -Y
                        page_coord.z == max_page_coord_inclusive.z, // +Z
                        page_coord.z == min_page_coord.z, // -Z
                    };

                    for (is_on_shell, 0..) |on_shell, i| {
                        if (!on_shell) continue;

                        const neighbor_page_coord = page_coord.addOffset(offsets[i]);
                        const neighbor_page_index = self.page_indirection.lookup(neighbor_page_coord);
                        if (neighbor_page_index == PageTable.sentinel) continue;

                        self.dirty_page_set.set(neighbor_page_index);

                        const neighbor_face_index = i ^ 1;
                        const neighbor_dirty_set = &self.pages.items(.dirty_voxeme_set)[neighbor_page_index];
                        for (page_face_voxeme_coords[neighbor_face_index]) |local_coord| {
                            neighbor_dirty_set.set(convert.localVoxemeCoordToIndex(local_coord));
                        }
                    }
                } else {
                    // Page is partially contained, fall back to voxeme/voxel level.
                    var voxeme_z: u4 = 0;
                    while (voxeme_z < page_axis_divisor) : (voxeme_z += 1) {
                        var voxeme_y: u4 = 0;
                        while (voxeme_y < page_axis_divisor) : (voxeme_y += 1) {
                            var voxeme_x: u4 = 0;
                            while (voxeme_x < page_axis_divisor) : (voxeme_x += 1) {
                                const local_voxeme_coord = LocalCoord{ .x = voxeme_x, .y = voxeme_y, .z = voxeme_z };
                                const voxeme_min_voxel = page_min_voxel + (vec3i{ @intCast(voxeme_x), @intCast(voxeme_y), @intCast(voxeme_z) } << @splat(voxeme_axis_shift));
                                const voxeme_max_voxel = voxeme_min_voxel + @as(vec3i, @splat(voxeme_axis_divisor));
                                const voxeme_aabb = aabb3i{ voxeme_min_voxel, voxeme_max_voxel };

                                if (!linalg.aabb_overlapping(aabb, voxeme_aabb)) continue;

                                if (linalg.aabb_contains(aabb, voxeme_aabb)) {
                                    try self.setVoxeme(page_coord, local_voxeme_coord, new_voxel);
                                } else {
                                    var local_voxel_z: u4 = 0;
                                    while (local_voxel_z < voxeme_axis_divisor) : (local_voxel_z += 1) {
                                        var local_voxel_y: u4 = 0;
                                        while (local_voxel_y < voxeme_axis_divisor) : (local_voxel_y += 1) {
                                            var local_voxel_x: u4 = 0;
                                            while (local_voxel_x < voxeme_axis_divisor) : (local_voxel_x += 1) {
                                                const global_voxel_coord = voxeme_min_voxel + vec3i{ @intCast(local_voxel_x), @intCast(local_voxel_y), @intCast(local_voxel_z) };
                                                if (linalg.aabb_contains_point(aabb, global_voxel_coord)) {
                                                    try self.setVoxel(global_voxel_coord, new_voxel);
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Internal helper to change page state without triggering neighbor updates.
fn setPageCore(self: *Grid, page_coord: PageCoord, page_index: u32, new_voxel: Voxel) !void {
    const page_voxel = &self.pages.items(.voxel)[page_index];
    if (page_voxel.* == new_voxel and self.pages.items(.voxeme_indirection)[page_index].len == 0) return;

    // Free existing voxemes within this page
    const voxeme_table: *VoxemeTable = &self.pages.items(.voxeme_indirection)[page_index];
    for (voxeme_table.indices) |voxeme_index| {
        if (voxeme_index == VoxemeTable.sentinel or voxeme_index == VoxemeTable.tombstone) continue;
        const buffer_index_ptr = &self.voxemes.items(.buffer_indirection)[voxeme_index];
        if (buffer_index_ptr.* != BufferData.sentinel) {
            try self.buffer_free_list.append(self.allocator, buffer_index_ptr.*);
        }
        try self.voxeme_free_list.append(self.allocator, voxeme_index);
    }

    if (new_voxel != Voxel.empty) {
        page_voxel.* = new_voxel;
        voxeme_table.init(); // Clear the table
    } else {
        // Free page
        self.page_indirection.remove(page_coord);
        try self.page_free_list.append(self.allocator, page_index);
    }
}

fn setPage(self: *Grid, page_coord: PageCoord, new_voxel: Voxel) !void {
    const page_index = try self.getOrCreatePage(page_coord, new_voxel) orelse return;
    try self.setPageCore(page_coord, page_index, new_voxel);
    if (new_voxel == Voxel.empty) return;

    // --- MARK DIRTY ---
    self.dirty_page_set.set(page_index);
    self.pages.items(.dirty_voxeme_set)[page_index].setAll();

    for (offsets, 0..) |offset, i| {
        const neighbor_page_coord = page_coord.addOffset(offset);
        const neighbor_page_index = self.page_indirection.lookup(neighbor_page_coord);
        if (neighbor_page_index == PageTable.sentinel) continue;

        self.dirty_page_set.set(neighbor_page_index);

        const neighbor_face_index = i ^ 1;
        const neighbor_dirty_set = &self.pages.items(.dirty_voxeme_set)[neighbor_page_index];
        for (page_face_voxeme_coords[neighbor_face_index]) |local_coord| {
            neighbor_dirty_set.set(convert.localVoxemeCoordToIndex(local_coord));
        }
    }
}

fn setVoxeme(self: *Grid, page_coord: PageCoord, local_voxeme_coord: LocalCoord, new_voxel: Voxel) !void {
    const page_index = try self.getOrCreatePage(page_coord, new_voxel) orelse return;
    const voxeme_index = try self.getOrCreateVoxeme(page_index, local_voxeme_coord, new_voxel) orelse return;
    const voxeme = &self.voxemes.items(.voxel)[voxeme_index];

    if (voxeme.* == new_voxel) return;

    // free buffer
    const buffer_index = &self.voxemes.items(.buffer_indirection)[voxeme_index];
    if (buffer_index.* != BufferData.sentinel) {
        try self.buffer_free_list.append(self.allocator, buffer_index.*);
        buffer_index.* = BufferData.sentinel;
    }

    if (new_voxel != Voxel.empty) {
        voxeme.* = new_voxel;
    } else {
        // free voxeme
        const voxeme_table: *VoxemeTable = &self.pages.items(.voxeme_indirection)[page_index];
        voxeme_table.remove(local_voxeme_coord);
        try self.voxeme_free_list.append(self.allocator, voxeme_index);
    }

    // --- MARK DIRTY ---
    const local_voxeme_idx = convert.localVoxemeCoordToIndex(local_voxeme_coord);
    self.pages.items(.dirty_voxeme_set)[page_index].set(local_voxeme_idx);
    self.dirty_page_set.set(page_index);

    // --- MARK NEIGHBORS DIRTY ---
    const current_local_i = vec3i{ @intCast(local_voxeme_coord.x), @intCast(local_voxeme_coord.y), @intCast(local_voxeme_coord.z) };
    for (offsets) |offset| {
        const neighbor_local_i = current_local_i + offset;
        var neighbor_page_coord = page_coord;
        var neighbor_local_coord: LocalCoord = undefined;

        // Check X boundary
        if (neighbor_local_i[0] < 0) {
            neighbor_page_coord.x -= 1;
            neighbor_local_coord.x = page_axis_mask;
        } else if (neighbor_local_i[0] >= page_axis_divisor) {
            neighbor_page_coord.x += 1;
            neighbor_local_coord.x = 0;
        } else {
            neighbor_local_coord.x = @intCast(neighbor_local_i[0]);
        }
        // Check Y boundary
        if (neighbor_local_i[1] < 0) {
            neighbor_page_coord.y -= 1;
            neighbor_local_coord.y = page_axis_mask;
        } else if (neighbor_local_i[1] >= page_axis_divisor) {
            neighbor_page_coord.y += 1;
            neighbor_local_coord.y = 0;
        } else {
            neighbor_local_coord.y = @intCast(neighbor_local_i[1]);
        }
        // Check Z boundary
        if (neighbor_local_i[2] < 0) {
            neighbor_page_coord.z -= 1;
            neighbor_local_coord.z = page_axis_mask;
        } else if (neighbor_local_i[2] >= page_axis_divisor) {
            neighbor_page_coord.z += 1;
            neighbor_local_coord.z = 0;
        } else {
            neighbor_local_coord.z = @intCast(neighbor_local_i[2]);
        }

        const neighbor_page_index = self.page_indirection.lookup(neighbor_page_coord);
        if (neighbor_page_index != PageTable.sentinel) {
            self.dirty_page_set.set(neighbor_page_index);
            const neighbor_local_voxeme_idx = convert.localVoxemeCoordToIndex(neighbor_local_coord);
            self.pages.items(.dirty_voxeme_set)[neighbor_page_index].set(neighbor_local_voxeme_idx);
        }
    }
}

fn setVoxel(self: *Grid, global_voxel: vec3i, new_voxel: Voxel) !void {
    const page_coord = convert.globalVoxelToPageCoord(global_voxel);
    const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(global_voxel);

    const page_index = try self.getOrCreatePage(page_coord, new_voxel) orelse return;
    const voxeme_index = try self.getOrCreateVoxeme(page_index, local_voxeme_coord, new_voxel) orelse return;
    const buffer_index = try self.getOrCreateBuffer(voxeme_index, new_voxel) orelse return;

    const buffer = &self.voxels.items(.voxel)[buffer_index];
    const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);
    const index_in_buffer = convert.localVoxelCoordToIndex(local_voxel_coord);
    const voxel = &buffer[index_in_buffer];

    if (voxel.* == new_voxel) return;

    voxel.* = new_voxel;

    // --- MARK DIRTY ---
    const local_voxeme_idx = convert.localVoxemeCoordToIndex(local_voxeme_coord);
    self.voxels.items(.dirty_voxel_set)[buffer_index].set(index_in_buffer);
    self.pages.items(.dirty_voxeme_set)[page_index].set(local_voxeme_idx);
    self.dirty_page_set.set(page_index);

    for (offsets) |offset| {
        const neighbor_global_voxel = global_voxel + offset;
        const neighbor_parts = convert.globalVoxelToParts(neighbor_global_voxel);
        const neighbor_page_index = self.page_indirection.lookup(neighbor_parts.page_coord);

        if (neighbor_page_index != PageTable.sentinel) {
            self.dirty_page_set.set(neighbor_page_index);

            const neighbor_voxeme_table: *const VoxemeTable = &self.pages.items(.voxeme_indirection)[neighbor_page_index];
            const neighbor_voxeme_index = neighbor_voxeme_table.lookup(neighbor_parts.local_voxeme_coord);

            if (neighbor_voxeme_index != VoxemeTable.sentinel) {
                const neighbor_local_voxeme_idx = convert.localVoxemeCoordToIndex(neighbor_parts.local_voxeme_coord);
                self.pages.items(.dirty_voxeme_set)[neighbor_page_index].set(neighbor_local_voxeme_idx);
            }
        }
    }
}

fn getOrCreatePage(self: *Grid, page_coord: PageCoord, new_voxel: ?Voxel) !?u32 {
    const page_indirect_index = self.page_indirection.lookup(page_coord);

    if (page_indirect_index == PageTable.sentinel) {
        if (new_voxel == Voxel.empty) {
            return null;
        }

        const page_index: u32 = if (self.page_free_list.pop()) |idx| idx else @intCast(try self.pages.addOne(self.allocator));
        errdefer self.page_free_list.appendAssumeCapacity(page_index);

        const success = self.page_indirection.insert(page_coord, page_index);
        if (!success) return error.OutOfPages;

        self.pages.items(.coord)[page_index] = page_coord;
        self.pages.items(.visibility)[page_index] = .none;
        self.pages.items(.voxel)[page_index] = .empty;
        self.pages.items(.voxeme_indirection)[page_index].init();
        self.pages.items(.dirty_voxeme_set)[page_index].unsetAll();

        return page_index;
    } else {
        return @intCast(page_indirect_index);
    }
}

fn getOrCreateVoxeme(self: *Grid, page_index: u32, local_voxeme_coord: LocalCoord, new_voxel: ?Voxel) !?u32 {
    const voxeme_table: *VoxemeTable = &self.pages.items(.voxeme_indirection)[page_index];
    const voxeme_index = voxeme_table.lookup(local_voxeme_coord);

    if (voxeme_index == VoxemeTable.sentinel) {
        const page_homogeneous_voxel = self.pages.items(.voxel)[page_index];
        if (page_homogeneous_voxel == new_voxel) return null;

        const new_voxeme_index: u32 = if (self.voxeme_free_list.pop()) |idx| idx else @intCast(try self.voxemes.addOne(self.allocator));
        errdefer self.voxeme_free_list.appendAssumeCapacity(new_voxeme_index);

        const success = voxeme_table.insert(local_voxeme_coord, new_voxeme_index);
        if (!success) return error.OutOfVoxemes;

        self.voxemes.items(.coord)[new_voxeme_index] = local_voxeme_coord;
        self.voxemes.items(.visibility)[new_voxeme_index] = .none;
        self.voxemes.items(.voxel)[new_voxeme_index] = page_homogeneous_voxel;
        self.voxemes.items(.buffer_indirection)[new_voxeme_index] = BufferData.sentinel;

        return new_voxeme_index;
    } else {
        if (new_voxel) |nv| {
            const page_homogeneous_voxel = self.pages.items(.voxel)[page_index];
            if (page_homogeneous_voxel == nv) return null;
        }

        return voxeme_index;
    }
}

fn getOrCreateBuffer(self: *Grid, voxeme_index: u32, new_voxel: ?Voxel) !?u32 {
    const voxeme_data = &self.voxemes.items(.voxel)[voxeme_index];
    const buffer_handle = &self.voxemes.items(.buffer_indirection)[voxeme_index];

    if (buffer_handle.* == BufferData.sentinel) { // buffer not bound
        if (voxeme_data.* == new_voxel) return null;

        const new_buffer_index: u32 = if (self.buffer_free_list.pop()) |idx| idx else @intCast(try self.voxels.addOne(self.allocator));
        const new_voxels = &self.voxels.items(.voxel)[new_buffer_index];
        const new_visibility = &self.voxels.items(.visibility)[new_buffer_index];
        const new_dirty_bits = &self.voxels.items(.dirty_voxel_set)[new_buffer_index];
        @memset(new_voxels, voxeme_data.*);
        @memset(new_visibility, Visibility.none); // ensure its at least not `undefined`; TODO: is this necessary?
        new_dirty_bits.unsetAll();

        buffer_handle.* = new_buffer_index;
        voxeme_data.* = Voxel.empty;
    } else {
        if (voxeme_data.* == new_voxel) return null;
    }

    return buffer_handle.*;
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

    /// Deconstructs a global voxel coordinate into all its constituent parts.
    pub inline fn globalVoxelToParts(v: vec3i) struct {
        page_coord: PageCoord,
        local_voxeme_coord: LocalCoord,
        local_voxel_coord: LocalCoord,
    } {
        return .{
            .page_coord = globalVoxelToPageCoord(v),
            .local_voxeme_coord = globalVoxelToLocalVoxemeCoord(v),
            .local_voxel_coord = globalVoxelToLocalVoxelCoord(v),
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

    /// 1D buffer index for a page's VoxemeTable to a 3D local voxeme coordinate (0-15).
    pub inline fn indexToLocalVoxemeCoord(index: u32) LocalCoord {
        return .{
            .x = @intCast(index & page_axis_mask),
            .y = @intCast((index >> page_axis_shift) & page_axis_mask),
            .z = @intCast((index >> (page_axis_shift * 2)) & page_axis_mask),
        };
    }

    /// 1D buffer index for a voxeme's VoxelBuffer to a 3D local voxel coordinate (0-15).
    pub inline fn indexToLocalVoxelCoord(index: u32) LocalCoord {
        return .{
            .x = @intCast(index & voxeme_axis_mask),
            .y = @intCast((index >> voxeme_axis_shift) & voxeme_axis_mask),
            .z = @intCast((index >> (voxeme_axis_shift * 2)) & voxeme_axis_mask),
        };
    }

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

/// Descriptor holding raw pointers to mesh data for integration into the cache.
pub const MeshDescriptor = extern struct {
    coord: PageCoord,
    bounds: aabb3,
    vertex_count: u32,
    index_count: u32,

    position: [*]const vec3,
    normal: [*]const vec3,
    uv: [*]const vec2,
    material_id: [*]const u32, // u32 for direct GPU compatibility
    indices: [*]const u32,
};

/// A simplified MeshDescriptor for possible rendering.
pub const MeshView = extern struct {
    coord: PageCoord,
    bounds: aabb3,
    index_offset: u32,
    index_count: u32,
};

/// A cache for mesh data generated from the voxel grid.
/// This is used to store the mesh data generated during the update phase.
///
/// This is not an inherently thread-safe structure;
/// `Command` and `applyCommands` are provided to allow deferred modification of the cache.
///
/// Descriptors may be retrieved and read from multiple threads safely, so long as
/// descriptors are not held past a command application step.
///
/// In other words, their lifetime is the current frame lifetime.
pub const MeshCache = struct {
    test {
        std.testing.refAllDecls(@This());
    }

    /// The indirection table mapping from `PageCoord` to page index in the `instances` ArrayList.
    page_indirection: PageTable,
    /// The mesh data which is currently ready for render.
    vertices: std.MultiArrayList(VertexData),
    /// The index buffer which is currently ready for render.
    indices: std.ArrayList(u32),
    /// The mesh instances which are currently ready for render.
    meshes: std.MultiArrayList(MeshData),
    /// The list of free mesh instance indices for reuse.
    mesh_free_list: std.ArrayList(u32),

    /// The vertex data which is currently ready for render.
    /// This data structure is not passed to the gpu, it is just a table layout for MultiArrayList
    pub const VertexData = struct {
        position: vec3,
        normal: vec3,
        uv: vec2,
        material_id: u32, // stored as u32 for direct GPU compatibility (MaterialId is u12 but gets widened)
    };

    /// A mesh instance which is currently ready for render.
    pub const MeshData = struct {
        /// Memory management metadata for this mesh instance.
        info: packed struct {
            /// flag indicating that this instance is not in use and can be reused
            free: bool,

            /// The number of triangles in the allocated space for this mesh instance.
            allocated_index_count: u31, // u31 is still much larger than the theoretical maximum indices per page, so borrowing a bit for the free flag is fine
            /// The number of vertices in the allocated space for this mesh instance.
            allocated_vertex_count: u32,
        },

        /// The page coordinate for this mesh instance.
        coord: PageCoord,

        /// The world-space axis-aligned bounding box for this mesh instance.
        bounds: aabb3,

        /// The offset into the global vertex buffer for this mesh instance.
        vertex_offset: u32,
        /// The number of vertices in this mesh instance.
        vertex_count: u32,

        /// The offset into the global index buffer for this mesh instance.
        index_offset: u32,
        /// The number of triangles in this mesh instance.
        index_count: u32,
    };

    /// An iterator over all active mesh instances indices in the cache.
    pub const Iterator = struct {
        mesh_cache: *const MeshCache,
        index: u32,

        pub fn next(self: *Iterator) ?MeshView {
            var current_index: u32 = self.index;
            while (current_index < self.mesh_cache.meshes.len) : (current_index += 1) {
                if (!self.mesh_cache.meshes.items(.info)[current_index].free) break;
            } else { // while else branch is taken if we do not break out of the loop
                self.index = current_index;
                return null;
            }

            defer self.index = current_index + 1;

            return MeshView{
                .coord = self.mesh_cache.meshes.items(.coord)[current_index],
                .bounds = self.mesh_cache.meshes.items(.bounds)[current_index],
                .index_offset = self.mesh_cache.meshes.items(.index_offset)[current_index],
                .index_count = self.mesh_cache.meshes.items(.index_count)[current_index],
            };
        }
    };

    /// Commands for modifying the mesh cache asynchronously.
    pub const Command = union(enum) {
        /// Insert a new mesh instance.
        insert: MeshDescriptor,
        /// Remove an existing mesh instance by its page coordinate.
        remove: PageCoord,
    };

    /// Create a new, empty mesh cache.
    /// This incurs several large allocations.
    pub fn init(allocator: std.mem.Allocator) !*MeshCache {
        const self = try allocator.create(MeshCache); // allocate self because the structure is large
        errdefer allocator.destroy(self);

        self.page_indirection.init();

        self.vertices = .empty;
        try self.vertices.ensureTotalCapacity(allocator, std.math.maxInt(u16));
        errdefer self.vertices.deinit(allocator);

        self.indices = .empty;
        try self.indices.ensureTotalCapacity(allocator, std.math.maxInt(u16) * 3);
        errdefer self.indices.deinit(allocator);

        self.meshes = .empty;
        try self.meshes.ensureTotalCapacity(allocator, 1024);
        errdefer self.meshes.deinit(allocator);

        self.mesh_free_list = .empty;
        try self.mesh_free_list.ensureTotalCapacity(allocator, 1024);
        errdefer self.mesh_free_list.deinit(allocator);

        return self;
    }

    /// Deinitialize the cache, freeing all memory it owns.
    pub fn deinit(self: *MeshCache, allocator: std.mem.Allocator) void {
        self.vertices.deinit(allocator);
        self.indices.deinit(allocator);
        self.meshes.deinit(allocator);
        self.mesh_free_list.deinit(allocator);
        allocator.destroy(self);
    }

    /// Clear the cache state but retain all allocated memory.
    /// Caller should hold mutex lock when calling this.
    pub fn clear(self: *MeshCache) void {
        self.page_indirection.init();
        self.vertices.clearRetainingCapacity();
        self.indices.clearRetainingCapacity();
        self.meshes.clearRetainingCapacity();
        self.mesh_free_list.clearRetainingCapacity();
    }

    /// Get an iterator over all active mesh instances in the cache.
    pub fn iterator(self: *const MeshCache) Iterator {
        return Iterator{ .mesh_cache = self, .index = 0 };
    }

    /// Apply a set of commands to the cache.
    pub fn applyCommands(self: *MeshCache, allocator: std.mem.Allocator, commands: []const MeshCache.Command) !void {
        for (commands) |cmd| {
            switch (cmd) {
                .insert => |desc| try self.addInstance(allocator, desc),
                .remove => |coord| try self.removeInstance(allocator, coord),
            }
        }
    }

    /// Add a new mesh instance for the given page coordinate.
    /// This will first attempt to fit the instance into a free slot;
    /// if no free slots are available, a new instance is appended to the list.
    fn addInstance(self: *MeshCache, allocator: std.mem.Allocator, mesh_descriptor: MeshDescriptor) !void {
        // remove any existing instance first
        try self.removeInstance(allocator, mesh_descriptor.coord);

        var best_fit: ?struct { indirection: u32, vertices: u32, indices: u32 } = null;
        for (self.mesh_free_list.items, 0..) |free_index, instance_indirection| {
            const info = self.meshes.items(.info)[free_index];

            if (info.allocated_vertex_count >= mesh_descriptor.vertex_count and info.allocated_index_count >= mesh_descriptor.index_count) {
                if (best_fit) |bf| {
                    if (info.allocated_vertex_count < bf.vertices or info.allocated_index_count < bf.indices) {
                        best_fit = .{ .indirection = @intCast(instance_indirection), .vertices = info.allocated_vertex_count, .indices = info.allocated_index_count };
                    }
                } else {
                    best_fit = .{ .indirection = @intCast(instance_indirection), .vertices = info.allocated_vertex_count, .indices = info.allocated_index_count };
                }
            }
        }

        var resized_vertices: ?u32 = null;
        var resized_indices: ?u32 = null;
        var resized_instances = false;
        errdefer {
            if (resized_vertices) |old_len| self.vertices.shrinkRetainingCapacity(old_len);
            if (resized_indices) |old_len| self.indices.shrinkRetainingCapacity(old_len);
            if (resized_instances) self.meshes.orderedRemove(self.meshes.len - 1);
        }

        const index = if (best_fit) |bf| reuse: {
            const index = self.mesh_free_list.swapRemove(bf.indirection);

            self.meshes.items(.info)[index].free = false;

            self.meshes.items(.vertex_count)[index] = mesh_descriptor.vertex_count;
            self.meshes.items(.index_count)[index] = mesh_descriptor.index_count;
            break :reuse index;
        } else create: {
            const new_index: u32 = @intCast(try self.meshes.addOne(allocator));
            resized_instances = true;

            const vertex_offset: u32 = @intCast(self.vertices.len);
            const index_offset: u32 = @intCast(self.indices.items.len);

            self.meshes.items(.info)[new_index] = .{
                .free = false,
                .allocated_vertex_count = mesh_descriptor.vertex_count,
                .allocated_index_count = @intCast(mesh_descriptor.index_count),
            };

            self.meshes.items(.vertex_offset)[new_index] = vertex_offset;
            self.meshes.items(.vertex_count)[new_index] = mesh_descriptor.vertex_count;
            self.meshes.items(.index_offset)[new_index] = index_offset;
            self.meshes.items(.index_count)[new_index] = mesh_descriptor.index_count;

            try self.vertices.resize(allocator, vertex_offset + mesh_descriptor.vertex_count);
            resized_vertices = vertex_offset;

            try self.indices.resize(allocator, index_offset + mesh_descriptor.index_count);
            resized_indices = index_offset;

            break :create new_index;
        };

        self.meshes.items(.coord)[index] = mesh_descriptor.coord;
        self.meshes.items(.bounds)[index] = mesh_descriptor.bounds;

        const vertex_offset = self.meshes.items(.vertex_offset)[index];
        @memcpy(self.vertices.items(.position)[vertex_offset .. vertex_offset + mesh_descriptor.vertex_count], mesh_descriptor.position);
        @memcpy(self.vertices.items(.normal)[vertex_offset .. vertex_offset + mesh_descriptor.vertex_count], mesh_descriptor.normal);
        @memcpy(self.vertices.items(.uv)[vertex_offset .. vertex_offset + mesh_descriptor.vertex_count], mesh_descriptor.uv);
        @memcpy(self.vertices.items(.material_id)[vertex_offset .. vertex_offset + mesh_descriptor.vertex_count], mesh_descriptor.material_id);

        const index_offset = self.meshes.items(.index_offset)[index];
        const indices_slice = self.indices.items[index_offset .. index_offset + mesh_descriptor.index_count];
        for (indices_slice, 0..) |*dest_index, i| {
            dest_index.* = mesh_descriptor.indices[i] + vertex_offset;
        }

        // replace or append the indirection
        const success = self.page_indirection.insert(mesh_descriptor.coord, index);
        if (!success) return error.OutOfPages;
    }

    /// Remove a mesh instance by its page coordinate.
    /// This is a trivial operation, as the actual mesh data is not freed;
    /// only the instance is removed from the indirection table and its index added to the free list.
    fn removeInstance(self: *MeshCache, allocator: std.mem.Allocator, coord: PageCoord) !void {
        const instance_index = self.page_indirection.lookup(coord);
        if (instance_index == PageTable.sentinel) return; // not found

        self.page_indirection.remove(coord);
        try self.mesh_free_list.append(allocator, instance_index);
    }

    /// Get a mesh render descriptor by its page coordinate.
    /// The descriptor must not be held beyond a frame boundary.
    pub fn getView(self: *const MeshCache, coord: PageCoord) ?MeshView {
        const instance_index = self.page_indirection.lookup(coord);
        if (instance_index == PageTable.sentinel) return null; // not found

        std.debug.assert(!self.meshes.items(.info)[instance_index].free); // should never happen

        return .{
            .coord = coord,
            .bounds = self.meshes.items(.bounds)[instance_index],
            .index_offset = self.meshes.items(.index_offset)[instance_index],
            .index_count = self.meshes.items(.index_count)[instance_index],
        };
    }

    /// Get a mesh instance descriptor by its page coordinate.
    /// The descriptor must not be held beyond a frame boundary.
    pub fn getDescriptor(self: *const MeshCache, coord: PageCoord) ?MeshDescriptor {
        const instance_index = self.page_indirection.lookup(coord);

        if (instance_index == PageTable.sentinel) return null; // not found

        std.debug.assert(!self.meshes.items(.info)[instance_index].free); // should never happen

        return .{
            .coord = coord,
            .bounds = self.meshes.items(.bounds)[instance_index],
            .vertex_count = self.meshes.items(.vertex_count)[instance_index],
            .index_count = self.meshes.items(.index_count)[instance_index],
            .position = self.vertices.items(.position)[self.meshes.items(.vertex_offset)[instance_index]..].ptr,
            .normal = self.vertices.items(.normal)[self.meshes.items(.vertex_offset)[instance_index]..].ptr,
            .uv = self.vertices.items(.uv)[self.meshes.items(.vertex_offset)[instance_index]..].ptr,
            .material_id = self.vertices.items(.material_id)[self.meshes.items(.vertex_offset)[instance_index]..].ptr,
            .indices = self.indices.items[self.meshes.items(.index_offset)[instance_index]..].ptr,
        };
    }

    /// Defragment a mesh cache and copy it into a new one.
    pub fn clone(self: *MeshCache, temp_allocator: std.mem.Allocator, gpa: std.mem.Allocator) !*MeshCache {
        const new_cache = try MeshCache.init(gpa);
        errdefer new_cache.deinit(gpa);

        try self.defragment_and_copy(temp_allocator, gpa, new_cache);

        return new_cache;
    }

    /// Defragment if necessary
    pub fn maybeDefrag(self: *MeshCache, temp_allocator: std.mem.Allocator, gpa: std.mem.Allocator) !void {
        const free_count = self.mesh_free_list.items.len;
        const total_count = self.meshes.len;

        // if more than 25% of instances are free, defrag
        if (free_count * 4 > total_count) {
            return self.defragment(temp_allocator, gpa);
        }

        // sum the number of allocated and used vertices and indices
        var allocated_vertices: usize = 0;
        var allocated_indices: usize = 0;
        var used_vertices: usize = 0;
        var used_indices: usize = 0;
        for (self.mesh_free_list.items) |free_index| {
            const info = self.meshes.items(.info)[free_index];
            allocated_vertices += info.allocated_vertex_count;
            allocated_indices += info.allocated_index_count;
            used_vertices += self.meshes.items(.vertex_count)[free_index];
            used_indices += self.meshes.items(.index_count)[free_index];
        }

        // if more than 50% of allocated vertices or indices are unused, defrag
        if (allocated_vertices > used_vertices * 2 or allocated_indices > used_indices * 2) {
            return self.defragment(temp_allocator, gpa);
        }
    }

    /// Defragment the mesh instance list, removing all free instances and compacting the list.
    /// This also rebuilds the indirection table and compacts the vertex and index buffers.
    pub fn defragment(self: *MeshCache, temp_allocator: std.mem.Allocator, gpa: std.mem.Allocator) !void {
        return self.defragment_and_copy(temp_allocator, gpa, null);
    }

    inline fn defragment_and_copy(self: *MeshCache, temp_allocator: std.mem.Allocator, gpa: std.mem.Allocator, secondary_destination: ?*MeshCache) !void {
        var new_instances = std.MultiArrayList(MeshData).empty;
        try new_instances.ensureTotalCapacity(gpa, self.meshes.len - self.mesh_free_list.items.len);
        errdefer new_instances.deinit(gpa);

        var new_vertices = std.MultiArrayList(VertexData).empty;
        try new_vertices.ensureTotalCapacity(gpa, self.vertices.len);
        errdefer new_vertices.deinit(gpa);

        var new_indices = std.ArrayList(u32).empty;
        try new_indices.ensureTotalCapacity(gpa, self.indices.items.len);
        errdefer new_indices.deinit(gpa);

        const new_indirection = try temp_allocator.create(PageTable);
        defer temp_allocator.destroy(new_indirection);
        new_indirection.init();

        for (0..self.meshes.len) |old_index| {
            const old_info = self.meshes.items(.info)[old_index];

            if (old_info.free) continue; // skip free instances

            const instance_coord = self.meshes.items(.coord)[old_index];

            const new_index: u32 = @intCast(try new_instances.addOne(gpa));
            const success = new_indirection.insert(instance_coord, new_index);
            std.debug.assert(success); // this should never fail, it was already in a table

            const vertex_count = self.meshes.items(.vertex_count)[old_index];
            const old_vertex_offset = self.meshes.items(.vertex_offset)[old_index];
            const new_vertex_offset: u32 = @intCast(new_vertices.len);

            const index_count = self.meshes.items(.index_count)[old_index];
            const old_index_offset = self.meshes.items(.index_offset)[old_index];
            const new_index_offset: u32 = @intCast(new_indices.items.len);

            try new_vertices.resize(gpa, new_vertex_offset + vertex_count);
            try new_indices.resize(gpa, new_index_offset + index_count);

            @memcpy(new_vertices.items(.position)[new_vertex_offset .. new_vertex_offset + vertex_count], self.vertices.items(.position)[old_vertex_offset..].ptr);
            @memcpy(new_vertices.items(.normal)[new_vertex_offset .. new_vertex_offset + vertex_count], self.vertices.items(.normal)[old_vertex_offset..].ptr);
            @memcpy(new_vertices.items(.uv)[new_vertex_offset .. new_vertex_offset + vertex_count], self.vertices.items(.uv)[old_vertex_offset..].ptr);
            @memcpy(new_vertices.items(.material_id)[new_vertex_offset .. new_vertex_offset + vertex_count], self.vertices.items(.material_id)[old_vertex_offset..].ptr);
            @memcpy(new_indices.items[new_index_offset .. new_index_offset + index_count], self.indices.items[old_index_offset..].ptr);
            new_instances.items(.info)[new_index] = .{
                .free = false,
                .allocated_vertex_count = vertex_count,
                .allocated_index_count = @intCast(index_count),
            };
            new_instances.items(.coord)[new_index] = instance_coord;
            new_instances.items(.bounds)[new_index] = self.meshes.items(.bounds)[old_index];
            new_instances.items(.vertex_offset)[new_index] = new_vertex_offset;
            new_instances.items(.vertex_count)[new_index] = vertex_count;
            new_instances.items(.index_offset)[new_index] = new_index_offset;
            new_instances.items(.index_count)[new_index] = index_count;
        }

        inline for (0..2) |i| {
            const dest = if (comptime i > 0) secondary_destination orelse return else self;

            dest.meshes.deinit(gpa);
            dest.meshes = if (comptime i > 0) try new_instances.clone(gpa) else new_instances;

            dest.vertices.deinit(gpa);
            dest.vertices = if (comptime i > 0) try new_vertices.clone(gpa) else new_vertices;

            dest.indices.deinit(gpa);
            dest.indices = if (comptime i > 0) try new_indices.clone(gpa) else new_indices;

            dest.page_indirection.copy(new_indirection);

            dest.mesh_free_list.clearRetainingCapacity();
        }
    }
};

pub fn AsyncQueue(comptime T: type) type {
    return struct {
        const Self = @This();

        buffer: []T,
        top: u32,

        /// Initialize a new async queue with the given capacity.
        /// * Initialize before spawning threads using the queue.
        pub fn init(allocator: std.mem.Allocator, capacity: u32) !Self {
            return Self{
                .buffer = try allocator.alloc(T, capacity),
                .top = 0,
            };
        }

        /// Deinitialize the queue, freeing its buffer.
        /// * All threads must be joined and no longer using the queue before calling this.
        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            allocator.free(self.buffer);
            self.* = undefined; // debug safety
        }

        /// * Thread-safe
        pub fn enqueue(self: *Self, item: T) !void {
            const current_top: usize = @atomicRmw(u32, &self.top, .Add, 1, .monotonic);
            if (current_top >= self.buffer.len) return error.AsyncQueueFull;
            self.buffer[current_top] = item;
        }

        /// * Not thread-safe
        pub fn enqueue_sync(self: *Self, item: T) !void {
            if (self.top >= self.buffer.len) return error.AsyncQueueFull;
            self.buffer[self.top] = item;
            self.top += 1;
        }

        // * Not thread-safe
        pub fn dequeue_sync(self: *Self) ?T {
            if (self.top == 0) return null;
            self.top -= 1;
            return self.buffer[self.top];
        }
    };
}

/// The manager holds two Grids; a front buffer and a back buffer:
/// * The front buffer is the one currently being used for read and write operations.
/// * The back buffer is used for updates in parallel worker threads. These updates perform multiple operations:
///   - Attempts homogenization dirty voxemes and pages.
///   - Prunes empty voxemes and pages.
///   - Recalculates visibility for dirty voxemes and pages.
///   - Generates mesh instances for voxemes and pages.
///
/// This allows for safe, lock-free updates to the grid state while still allowing the main thread
/// to read and write voxel data without blocking.
///
/// To facilitate these processes, the Manager also holds a thread pool for parallel updates,
/// and a mesh cache for storing generated mesh data.
pub const Manager = struct {
    test {
        std.testing.refAllDecls(@This());
    }

    pub const SwapBuffers = struct {
        grid: *Grid,
        mesh_cache: *MeshCache,
        pub fn deinit(self: *SwapBuffers) void {
            self.mesh_cache.deinit(self.grid.allocator);
            self.grid.deinit();
        }
    };

    pub const PruneCommand = union(enum) {
        prune_page: PageCoord,
        prune_voxeme: struct {
            page_coord: PageCoord,
            local_coord: LocalCoord,
        },
    };

    /// The allocator used for all allocations, taken from the input Grid for consistent, concurrent-safe access.
    allocator: std.mem.Allocator,

    /// Both grids and mesh caches used by the double buffering system.
    state: [2]SwapBuffers,
    /// index of the front buffers in `state`
    front_index: u1,

    /// Grid command queue used by main thread to request work from the manager thread
    grid_command_queue: std.ArrayList(Grid.Command),

    /// Mesh cache command queue used by the manager thread
    mesh_command_queue: AsyncQueue(MeshCache.Command),
    /// Grid pruning command queue used by the manager thread
    prune_command_queue: AsyncQueue(PruneCommand),
    /// Queue for freed buffers from parallel jobs
    buffer_free_queue: AsyncQueue(u32),
    /// Queue for freed voxemes from parallel jobs
    voxeme_free_queue: AsyncQueue(u32),

    /// Arena allocator for temporary allocations during updates.
    update_arena: std.heap.ArenaAllocator,

    /// Synchronization mutex for the front buffer.
    sync_mutex: std.Thread.Mutex,
    /// Mutex for the command queue.
    cmd_mutex: std.Thread.Mutex,
    /// Signals the manager thread that there is update work to do in the front buffer.
    signal: std.Thread.Condition,
    /// The shutdown signal for the manager thread.
    frame_ready: std.atomic.Value(bool),
    running: std.atomic.Value(bool),
    /// The thread pool used for parallel updates.
    thread_pool: *std.Thread.Pool,
    /// The thread running the update manager loop.
    manager_thread: std.Thread,

    /// Create a new update manager for the given grid.
    /// This allocates a second grid as the back buffer.
    /// This does not take fully ownership of the given grid;
    /// the caller is still responsible for deinitializing it. See `deinit`.
    pub fn init(grid: *Grid, pool: *std.Thread.Pool) !*Manager {
        const self = try grid.allocator.create(Manager);
        errdefer grid.allocator.destroy(self);

        self.allocator = grid.allocator;

        const back_grid = try grid.clone();
        errdefer back_grid.deinit();

        const mesh_cache = try MeshCache.init(self.allocator);
        errdefer mesh_cache.deinit(self.allocator);

        const back_cache = try MeshCache.init(self.allocator);
        errdefer back_cache.deinit(self.allocator);

        self.state = .{
            .{ .grid = grid, .mesh_cache = mesh_cache },
            .{ .grid = back_grid, .mesh_cache = back_cache },
        };
        self.front_index = 0;

        self.sync_mutex = .{};
        self.cmd_mutex = .{};
        self.signal = .{};
        self.frame_ready = .init(false);
        self.running = .init(true);

        self.grid_command_queue = .empty;
        try self.grid_command_queue.ensureTotalCapacity(self.allocator, 1024);
        errdefer self.grid_command_queue.deinit(self.allocator);

        self.mesh_command_queue = try AsyncQueue(MeshCache.Command).init(self.allocator, max_pages); // TODO: calculate memory usage
        errdefer self.mesh_command_queue.deinit(self.allocator);

        self.prune_command_queue = try AsyncQueue(PruneCommand).init(self.allocator, max_pages * voxemes_per_page); // TODO: calculate memory usage
        errdefer self.prune_command_queue.deinit(self.allocator);

        self.buffer_free_queue = try AsyncQueue(u32).init(self.allocator, max_pages * voxemes_per_page);
        errdefer self.buffer_free_queue.deinit(self.allocator);

        self.voxeme_free_queue = try AsyncQueue(u32).init(self.allocator, max_pages * voxemes_per_page);
        errdefer self.voxeme_free_queue.deinit(self.allocator);

        self.update_arena = std.heap.ArenaAllocator.init(self.allocator);
        errdefer self.update_arena.deinit();

        self.thread_pool = pool;

        self.manager_thread = try std.Thread.spawn(.{
            .allocator = self.allocator,
        }, manager, .{self});

        return self;
    }

    pub fn front(self: *Manager) SwapBuffers {
        return self.state[self.front_index];
    }

    pub fn back(self: *Manager) SwapBuffers {
        return self.state[~self.front_index];
    }

    // Deinitialize the update manager, joining all managed threads and freeing the back buffer grid.
    // * Returns the front buffers, transferring full ownership back to the caller.
    pub fn deinit(self: *Manager) SwapBuffers {
        const out = self.front();

        {
            self.sync_mutex.lock();
            defer {
                self.sync_mutex.unlock();
            }

            self.running.store(false, .monotonic);
            self.signal.broadcast(); // wake the manager thread if it is waiting
        }

        self.manager_thread.join(); // manager will wait for jobs to finish

        var back_mut = self.back();
        back_mut.deinit();

        self.grid_command_queue.deinit(self.allocator);
        self.mesh_command_queue.deinit(self.allocator);
        self.prune_command_queue.deinit(self.allocator);
        self.buffer_free_queue.deinit(self.allocator);
        self.voxeme_free_queue.deinit(self.allocator);
        self.update_arena.deinit();

        const allocator = self.allocator;
        allocator.destroy(self);

        return out;
    }

    /// Queues a command for the manager thread to process. This is the main thread's primary write interface.
    pub fn queueCommand(self: *Manager, command: Grid.Command) !void {
        self.cmd_mutex.lock();
        defer self.cmd_mutex.unlock();
        try self.grid_command_queue.append(self.allocator, command);
    }

    /// Swaps the front and back buffers and signals the manager that a new frame of commands is ready.
    pub fn endFrame(self: *Manager) void {
        {
            self.sync_mutex.lock();
            defer {
                self.sync_mutex.unlock();
            }

            // Swap front and back buffers
            self.front_index = ~self.front_index;
            self.frame_ready.store(true, .monotonic);

            // Wake the manager thread if it is waiting
            self.signal.broadcast();
        }
    }

    pub fn manager(self: *Manager) void {
        var local_command_buffer: std.ArrayList(Grid.Command) = .empty;
        defer local_command_buffer.deinit(self.allocator);

        log.info("Grid.Manager thread: started", .{});

        while (self.running.load(.monotonic)) {
            self.sync_mutex.lock();
            defer self.sync_mutex.unlock();
            const buffers = wait_and_copy: {
                // Wait until a frame is ready or we're shutting down
                wait_loop: while (true) {
                    if (!self.frame_ready.load(.monotonic) and self.running.load(.monotonic)) {
                        self.signal.wait(&self.sync_mutex);
                        continue :wait_loop;
                    } else {
                        break :wait_loop;
                    }
                }

                self.frame_ready.store(false, .monotonic);

                if (!self.running.load(.monotonic)) {
                    log.info("Grid.Manager thread: shutting down\n", .{});
                    return;
                }

                {
                    self.cmd_mutex.lock();
                    defer self.cmd_mutex.unlock();

                    // Drain the queue into a local buffer so we can unlock quickly.
                    local_command_buffer.clearRetainingCapacity();
                    local_command_buffer.appendSlice(self.allocator, self.grid_command_queue.items) catch |err| {
                        log.err("Grid.Manager thread: failed to copy command buffer: {s}; exiting\n", .{@errorName(err)});
                        return;
                    };
                    self.grid_command_queue.clearRetainingCapacity();
                    break :wait_and_copy self.back();
                }
            };

            // 2. Apply all queued commands to the back grid. This populates the dirty sets.
            buffers.grid.applyCommands(local_command_buffer.items) catch |err| {
                log.err("Failed to apply grid commands: {s}; exiting", .{@errorName(err)});
                return;
            };

            const arena = self.update_arena.allocator();
            defer _ = self.update_arena.reset(.retain_capacity);

            var voxeme_wait_group = std.Thread.WaitGroup{};
            var page_wait_group = std.Thread.WaitGroup{};

            var page_it = buffers.grid.dirty_page_set.iterator();

            // 3. Process dirty voxemes for homogenization/pruning.
            while (page_it.next()) |page_index| {
                var voxeme_it = buffers.grid.pages.items(.dirty_voxeme_set)[page_index].iterator();
                while (voxeme_it.next()) |local_voxeme_1d_idx| {
                    self.thread_pool.spawnWg(&voxeme_wait_group, voxeme_job, .{
                        self,
                        arena,
                        buffers,
                        page_index,
                        local_voxeme_1d_idx,
                    });
                }
            }
            self.thread_pool.waitAndWork(&voxeme_wait_group);

            // Drain buffer free queue before next steps.
            while (self.buffer_free_queue.dequeue_sync()) |buffer_idx| {
                buffers.grid.buffer_free_list.append(self.allocator, buffer_idx) catch |err| {
                    log.err("Grid.Manager thread: failed to append to buffer free list: {s}; exiting", .{@errorName(err)});
                    return;
                };
            }

            // 4. Process dirty pages for homogenization/pruning.
            page_it = buffers.grid.dirty_page_set.iterator();
            while (page_it.next()) |index| {
                self.thread_pool.spawnWg(&page_wait_group, page_job, .{
                    self,
                    arena,
                    buffers,
                    @as(u32, @intCast(index)),
                });
            }
            self.thread_pool.waitAndWork(&page_wait_group);

            // Drain voxeme free queue
            while (self.voxeme_free_queue.dequeue_sync()) |voxeme_idx| {
                buffers.grid.voxeme_free_list.append(self.allocator, voxeme_idx) catch |err| {
                    log.err("Grid.Manager thread: failed to append to buffer free list: {s}; exiting", .{@errorName(err)});
                    return;
                };
            }

            // 5. Apply any generated pruning commands serially.
            while (self.prune_command_queue.dequeue_sync()) |cmd| {
                switch (cmd) {
                    .prune_page => |page_coord| {
                        const page_index = buffers.grid.page_indirection.lookup(page_coord);
                        std.debug.assert(page_index != PageTable.sentinel);

                        buffers.grid.page_indirection.remove(page_coord);
                        buffers.grid.page_free_list.append(self.allocator, page_index) catch |err| {
                            log.err("Grid.Manager thread: failed to append to page free list: {s}; exiting", .{@errorName(err)});
                            return;
                        };

                        self.mesh_command_queue.enqueue_sync(.{ .remove = page_coord }) catch |err| {
                            log.err("Grid.Manager thread: failed to enqueue mesh cache remove command: {s}; exiting", .{@errorName(err)});
                            return;
                        };
                    },
                    .prune_voxeme => |coords| {
                        const page_index = buffers.grid.page_indirection.lookup(coords.page_coord);
                        std.debug.assert(page_index != PageTable.sentinel);

                        const voxeme_indirection: *VoxemeTable = &buffers.grid.pages.items(.voxeme_indirection)[page_index];

                        const voxeme_index = voxeme_indirection.lookup(coords.local_coord);
                        std.debug.assert(voxeme_index != VoxemeTable.sentinel);

                        voxeme_indirection.remove(coords.local_coord);
                        buffers.grid.voxeme_free_list.append(self.allocator, voxeme_index) catch |err| {
                            log.err("Grid.Manager thread: failed to append to voxeme free list: {s}; exiting", .{@errorName(err)});
                            return;
                        };
                    },
                }
            }

            // 6. Recalculate visibility for dirty voxemes.
            voxeme_wait_group = std.Thread.WaitGroup{};
            page_it = buffers.grid.dirty_page_set.iterator();
            while (page_it.next()) |page_index| {
                var voxeme_it = buffers.grid.pages.items(.dirty_voxeme_set)[page_index].iterator();
                while (voxeme_it.next()) |local_voxeme_1d_idx| {
                    self.thread_pool.spawnWg(&voxeme_wait_group, voxeme_visibility_job, .{
                        self,
                        arena,
                        buffers,
                        page_index,
                        local_voxeme_1d_idx,
                    });
                }
            }
            self.thread_pool.waitAndWork(&voxeme_wait_group);

            // 7. Recalculate visibility for dirty pages.
            page_wait_group = std.Thread.WaitGroup{};
            page_it = buffers.grid.dirty_page_set.iterator();
            while (page_it.next()) |index| {
                self.thread_pool.spawnWg(&page_wait_group, page_visibility_job, .{
                    self,
                    arena,
                    buffers,
                    @as(u32, @intCast(index)),
                });
            }
            self.thread_pool.waitAndWork(&page_wait_group);

            // 8. Generate/update meshes for any dirty pages.
            page_wait_group = std.Thread.WaitGroup{};
            for (buffers.grid.page_indirection.indices) |page_index| {
                if (page_index == PageTable.sentinel or page_index == PageTable.tombstone) continue;
                const page_coord = buffers.grid.pages.items(.coord)[page_index];

                // Remesh if the page was directly dirtied, OR if it doesn't have a mesh yet.
                if (!buffers.grid.dirty_page_set.isSet(page_index) and buffers.mesh_cache.page_indirection.lookup(page_coord) != PageTable.sentinel) continue;

                self.thread_pool.spawnWg(&page_wait_group, mesh_job, .{
                    self,
                    arena,
                    buffers,
                    page_index,
                });
            }
            self.thread_pool.waitAndWork(&page_wait_group);

            // 9. Finalize: clear all dirty bits, apply mesh commands, and defrag.
            page_it = buffers.grid.dirty_page_set.iterator();
            while (page_it.next()) |page_index| {
                const dirty_set = &buffers.grid.pages.items(.dirty_voxeme_set)[page_index];
                const voxeme_indirection = &buffers.grid.pages.items(.voxeme_indirection)[page_index];

                dirty_set.unsetAll();

                for (voxeme_indirection.indices) |voxeme_idx| {
                    if (voxeme_idx == VoxemeTable.sentinel or voxeme_idx == VoxemeTable.tombstone) continue;
                    const buffer_idx = buffers.grid.voxemes.items(.buffer_indirection)[voxeme_idx];
                    if (buffer_idx == BufferData.sentinel) continue;
                    buffers.grid.voxels.items(.dirty_voxel_set)[buffer_idx].unsetAll();
                }
            }
            buffers.grid.dirty_page_set.unsetAll();

            buffers.mesh_cache.applyCommands(self.allocator, self.mesh_command_queue.buffer[0..self.mesh_command_queue.top]) catch |err| {
                log.err("Grid.Manager thread: Failed to apply mesh cache commands: {s}; exiting", .{@errorName(err)});
                return;
            };
            self.mesh_command_queue.top = 0; // Reset queue

            buffers.mesh_cache.maybeDefrag(arena, self.allocator) catch |err| {
                log.err("Grid.Manager thread: Failed to defragment mesh cache: {s}; exiting", .{@errorName(err)});
                return;
            };
        }
    }

    /// Homogenizes a voxeme if all its voxels are identical, and prunes it if its state matches its parent page.
    fn voxeme_job(self: *Manager, arena: std.mem.Allocator, buffers: SwapBuffers, page_index: u32, local_voxeme_1d_index: u32) void {
        _ = arena;

        const voxeme_coord = convert.indexToLocalVoxemeCoord(local_voxeme_1d_index);
        const voxeme_index = buffers.grid.pages.items(.voxeme_indirection)[page_index].lookup(voxeme_coord);
        if (voxeme_index == VoxemeTable.sentinel) return;

        const buffer_handle = buffers.grid.voxemes.items(.buffer_indirection)[voxeme_index];

        // try to homogenize
        if (buffer_handle != BufferData.sentinel) {
            const buffer_data = &buffers.grid.voxels.items(.voxel)[buffer_handle];

            var first_voxel: ?Voxel = null;
            var all_identical = true;

            for (buffer_data) |*voxel| {
                if (first_voxel == null) {
                    first_voxel = voxel.*;
                } else if (voxel.* != first_voxel.?) {
                    all_identical = false;
                    break;
                }
            }

            if (all_identical) {
                // Homogenize
                buffers.grid.voxemes.items(.voxel)[voxeme_index] = first_voxel.?;

                // Free the buffer
                buffers.grid.voxemes.items(.buffer_indirection)[voxeme_index] = BufferData.sentinel;
                self.buffer_free_queue.enqueue(buffer_handle) catch |err| {
                    log.err("Grid.Manager voxeme_job: failed to enqueue freed buffer: {s}", .{@errorName(err)});
                    return;
                };
            }
        }

        // try to prune
        const buffer_handle_2 = buffers.grid.voxemes.items(.buffer_indirection)[voxeme_index];
        if (buffer_handle_2 == BufferData.sentinel) {
            const voxeme_voxel = buffers.grid.voxemes.items(.voxel)[voxeme_index];
            const page_voxel = buffers.grid.pages.items(.voxel)[page_index];

            if (voxeme_voxel == page_voxel) {
                // Prune
                self.prune_command_queue.enqueue(.{
                    .prune_voxeme = .{
                        .page_coord = buffers.grid.pages.items(.coord)[page_index],
                        .local_coord = convert.indexToLocalVoxemeCoord(local_voxeme_1d_index),
                    },
                }) catch |err| {
                    log.err("Grid.Manager voxeme_job: failed to enqueue prune command: {s}", .{@errorName(err)});
                    return;
                };
            }
        }
    }

    /// Homogenizes a page if all its voxemes are identical, and prunes it if its state is empty.
    fn page_job(self: *Manager, arena: std.mem.Allocator, buffers: SwapBuffers, page_index: u32) void {
        _ = arena;

        const page_voxel = buffers.grid.pages.items(.voxel)[page_index];

        // try to homogenize
        var all_identical = true;

        var voxeme_1d_index: u32 = 0;
        while (voxeme_1d_index < voxemes_per_page) : (voxeme_1d_index += 1) {
            const voxeme_coord = convert.indexToLocalVoxemeCoord(voxeme_1d_index);
            const voxeme_index = buffers.grid.pages.items(.voxeme_indirection)[page_index].lookup(voxeme_coord);
            if (voxeme_index == VoxemeTable.sentinel) continue;
            const voxeme_voxel = buffers.grid.voxemes.items(.voxel)[voxeme_index];
            const buffer_handle = buffers.grid.voxemes.items(.buffer_indirection)[voxeme_index];
            if (buffer_handle != BufferData.sentinel or voxeme_voxel != page_voxel) {
                all_identical = false;
                break;
            }
        }

        if (!all_identical) return;

        if (page_voxel != Voxel.empty) {
            // Homogenize
            // Free all voxeme buffers
            var voxeme_1d_index_2: u32 = 0;
            while (voxeme_1d_index_2 < voxemes_per_page) : (voxeme_1d_index_2 += 1) {
                const voxeme_coord = convert.indexToLocalVoxemeCoord(voxeme_1d_index_2);
                const voxeme_index = buffers.grid.pages.items(.voxeme_indirection)[page_index].lookup(voxeme_coord);
                if (voxeme_index == VoxemeTable.sentinel) continue;
                const buffer_handle = buffers.grid.voxemes.items(.buffer_indirection)[voxeme_index];
                if (buffer_handle != BufferData.sentinel) {
                    buffers.grid.voxemes.items(.buffer_indirection)[voxeme_index] = BufferData.sentinel;
                    self.buffer_free_queue.enqueue(buffer_handle) catch |err| {
                        log.err("Grid.Manager page_job: failed to enqueue freed buffer: {s}", .{@errorName(err)});
                        return;
                    };
                }
                self.voxeme_free_queue.enqueue(voxeme_index) catch |err| {
                    log.err("Grid.Manager page_job: failed to enqueue freed voxeme: {s}", .{@errorName(err)});
                    return;
                };
            }
        } else {
            // Prune
            self.prune_command_queue.enqueue(.{ .prune_page = buffers.grid.pages.items(.coord)[page_index] }) catch |err| {
                log.err("Grid.Manager page_job: failed to enqueue prune command: {s}", .{@errorName(err)});
                return;
            };
        }
    }

    /// Recalculates the 6-face visibility for a specific voxeme and all of its heterogeneous voxels, if it has any.
    fn voxeme_visibility_job(self: *Manager, arena: std.mem.Allocator, buffers: SwapBuffers, page_index: u32, local_voxeme_1d_index: u32) void {
        _ = self;
        _ = arena;

        const page_coord = buffers.grid.pages.items(.coord)[page_index];
        const voxeme_coord = convert.indexToLocalVoxemeCoord(local_voxeme_1d_index);
        const voxeme_coord_i = voxeme_coord.toVec3i();
        const voxeme_index = buffers.grid.pages.items(.voxeme_indirection)[page_index].lookup(voxeme_coord);
        if (voxeme_index == VoxemeTable.sentinel) return; // TODO: cache implicit voxeme visibility in the case of homogeneous page

        // recalculate gross visibility based on neighbor voxeme homogeny
        var new_voxeme_vis = Visibility.all;

        for (AxisDir.values) |axis_dir| {
            const component_index = axis_dir.toComponentIndex();
            const offset_index = axis_dir.toIndex();
            const offset = offsets[offset_index];

            const neighbor_page_index = check_same_page: {
                // zig fmt: off
                if ((voxeme_coord_i[component_index] == 0 and !axis_dir.positive)
                or  (voxeme_coord_i[component_index] == voxeme_axis_divisor - 1 and axis_dir.positive)) {
                // zig fmt: on
                    // neighbor is in a different page
                    const neighbor_page_coord = page_coord.addOffset(offset);
                    break :check_same_page buffers.grid.page_indirection.lookup(neighbor_page_coord);
                } else {
                    // neighbor is in the same page
                    break :check_same_page page_index;
                }
            };

            const neighbor_is_opaque = check_opacity: {
                if (neighbor_page_index != PageTable.sentinel) {
                    var neighbor_voxeme_coord = voxeme_coord_i;
                    neighbor_voxeme_coord[component_index] = if (axis_dir.positive) 0 else voxeme_axis_divisor - 1;

                    const neighbor_voxeme_index = buffers.grid.pages.items(.voxeme_indirection)[neighbor_page_index].lookup(.fromVec3i(neighbor_voxeme_coord));

                    if (neighbor_voxeme_index != VoxemeTable.sentinel) {
                        const neighbor_buffer_handle = buffers.grid.voxemes.items(.buffer_indirection)[neighbor_voxeme_index];
                        if (neighbor_buffer_handle == BufferData.sentinel) {
                            const neighbor_homo_voxel = buffers.grid.voxemes.items(.voxel)[neighbor_voxeme_index];
                            break :check_opacity buffers.grid.isOpaqueMaterial(neighbor_homo_voxel.material_id);
                        } else {
                            break :check_opacity false;
                        }
                    } else {
                        const neighbor_homo_voxel = buffers.grid.pages.items(.voxel)[neighbor_page_index];
                        break :check_opacity buffers.grid.isOpaqueMaterial(neighbor_homo_voxel.material_id);
                    }
                } else {
                    break :check_opacity false;
                }
            };

            if (neighbor_is_opaque) {
                new_voxeme_vis.set(axis_dir, false);
            }
        }

        buffers.grid.voxemes.items(.visibility)[voxeme_index] = new_voxeme_vis;

        const buffer_handle = buffers.grid.voxemes.items(.buffer_indirection)[voxeme_index];
        if (buffer_handle != BufferData.sentinel) {
            // Recalculate visibility for all voxels in the buffer
            const buffer_data = &buffers.grid.voxels.items(.visibility)[buffer_handle];

            // TODO: this could be split up to better utilize thread pool
            for (0..voxels_per_voxeme) |voxel_1d_index| {
                const local_voxel_coord = convert.indexToLocalVoxelCoord(@intCast(voxel_1d_index));
                const global_voxel_coord = convert.partsToGlobalVoxel(page_coord, voxeme_coord, local_voxel_coord);

                var new_vis = Visibility.all;

                for (AxisDir.values) |axis_dir| {
                    const offset_index = axis_dir.toIndex();
                    const offset = offsets[offset_index];
                    const neighbor_global_coord = global_voxel_coord + offset;
                    if (buffers.grid.isOpaqueVoxel(neighbor_global_coord)) {
                        new_vis.set(axis_dir, false);
                    }
                }

                buffer_data[voxel_1d_index] = new_vis;
            }
        }
    }

    /// Recalculates the 6-face visibility for a specific page.
    fn page_visibility_job(self: *Manager, arena: std.mem.Allocator, buffers: SwapBuffers, page_index: u32) void {
        _ = self;
        _ = arena;
        const page_coord = buffers.grid.pages.items(.coord)[page_index];
        var new_page_vis = Visibility.all;
        for (AxisDir.values) |axis_dir| {
            const offset_index = axis_dir.toIndex();
            const offset = offsets[offset_index];
            const neighbor_page_coord = page_coord.addOffset(offset);
            const neighbor_page_index = buffers.grid.page_indirection.lookup(neighbor_page_coord);
            const neighbor_is_opaque =
                if (neighbor_page_index != PageTable.sentinel)
                    // zig fmt: off
                        buffers.grid.pages.items(.voxeme_indirection)[neighbor_page_index].len == 0
                    and buffers.grid.isOpaqueMaterial(buffers.grid.pages.items(.voxel)[neighbor_page_index].material_id)
                    // zig fmt: on
                else
                    false;

            if (neighbor_is_opaque) {
                new_page_vis.set(axis_dir, false);
            }
        }

        buffers.grid.pages.items(.visibility)[page_index] = new_page_vis;
    }

    /// Generates a mesh for a dirty page using a simple "naive cubes" algorithm.
    fn mesh_job(self: *Manager, arena: std.mem.Allocator, buffers: SwapBuffers, page_index: u32) void {
        var vertices: std.MultiArrayList(MeshCache.VertexData) = .empty;
        var indices: std.ArrayList(u32) = .empty;

        const page_coord = buffers.grid.pages.items(.coord)[page_index];
        pageMeshBasic(buffers.grid, arena, page_coord, &vertices, &indices) catch |err| {
            log.err("Grid.Manager mesh_job: failed to generate basic mesh for page {any}: {s}", .{ page_coord, @errorName(err) });
            return;
        };

        const min_global_voxel = convert.partsToGlobalVoxel(
            page_coord,
            .{ .x = 0, .y = 0, .z = 0 },
            .{ .x = 0, .y = 0, .z = 0 },
        );
        const max_global_voxel = convert.partsToGlobalVoxel(
            page_coord,
            .{ .x = page_axis_divisor - 1, .y = page_axis_divisor - 1, .z = page_axis_divisor - 1 },
            .{ .x = voxeme_axis_divisor - 1, .y = voxeme_axis_divisor - 1, .z = voxeme_axis_divisor - 1 },
        );
        const min_world = convert.globalVoxelToWorld(min_global_voxel);
        const max_world = convert.globalVoxelToWorld(max_global_voxel) + @as(vec3, @splat(voxel_scale));

        const descriptor = MeshDescriptor{
            .coord = page_coord,
            .bounds = .{ min_world, max_world },
            .vertex_count = @intCast(vertices.len),
            .index_count = @intCast(indices.items.len),

            .position = vertices.items(.position).ptr,
            .normal = vertices.items(.normal).ptr,
            .uv = vertices.items(.uv).ptr,
            .material_id = vertices.items(.material_id).ptr,
            .indices = indices.items.ptr,
        };

        self.mesh_command_queue.enqueue_sync(.{ .insert = descriptor }) catch |err| {
            log.err("Grid.Manager mesh_job: failed to enqueue mesh cache update command for page {any}: {s}", .{ page_coord, @errorName(err) });
            return;
        };
    }
};

const FaceData = struct {
    normal: vec3,
    // Vertices in counter-clockwise order for correct winding
    vertices: [4]vec3,
    uvs: [4]vec2 = .{ .{ 0, 0 }, .{ 1, 0 }, .{ 1, 1 }, .{ 0, 1 } },
};

// Data for the 6 faces of a unit cube.
// The order MUST match the bit order of the Visibility struct for the fast meshing path.
// 0=+X, 1=-X, 2=+Y, 3=-Y, 4=+Z, 5=-Z
const face_data = [_]FaceData{
    // 0: +X
    .{ .normal = .{ 1, 0, 0 }, .vertices = .{ .{ 1, 0, 0 }, .{ 1, 1, 0 }, .{ 1, 1, 1 }, .{ 1, 0, 1 } } },
    // 1: -X
    .{ .normal = .{ -1, 0, 0 }, .vertices = .{ .{ 0, 0, 1 }, .{ 0, 1, 1 }, .{ 0, 1, 0 }, .{ 0, 0, 0 } } },
    // 2: +Y
    .{ .normal = .{ 0, 1, 0 }, .vertices = .{ .{ 0, 1, 0 }, .{ 0, 1, 1 }, .{ 1, 1, 1 }, .{ 1, 1, 0 } } },
    // 3: -Y
    .{ .normal = .{ 0, -1, 0 }, .vertices = .{ .{ 1, 0, 0 }, .{ 1, 0, 1 }, .{ 0, 0, 1 }, .{ 0, 0, 0 } } },
    // 4: +Z
    .{ .normal = .{ 0, 0, 1 }, .vertices = .{ .{ 1, 0, 1 }, .{ 1, 1, 1 }, .{ 0, 1, 1 }, .{ 0, 0, 1 } } },
    // 5: -Z
    .{ .normal = .{ 0, 0, -1 }, .vertices = .{ .{ 0, 0, 0 }, .{ 0, 1, 0 }, .{ 1, 1, 0 }, .{ 1, 0, 0 } } },
};

/// Helper function to generate a quad for a single face.
fn addFace(
    gpa: std.mem.Allocator,
    vertices: *std.MultiArrayList(MeshCache.VertexData),
    indices: *std.ArrayList(u32),
    axis_dir: AxisDir,
    origin: vec3,
    scale: f32,
    material: MaterialId,
) !void {
    const face_index = axis_dir.toIndex();

    const base_vertex_index: u32 = @intCast(vertices.len);
    const data = &face_data[face_index];

    for (data.vertices, data.uvs) |v_offset, uv| {
        const pos = origin + v_offset * @as(vec3, @splat(scale));
        try vertices.append(gpa, .{
            .position = pos,
            .normal = data.normal,
            .uv = uv,
            .material_id = @intFromEnum(material), // Convert MaterialId (u12 enum) to u32 for GPU
        });
    }

    try indices.appendSlice(gpa, &.{
        base_vertex_index + 0, base_vertex_index + 1, base_vertex_index + 2,
        base_vertex_index + 0, base_vertex_index + 2, base_vertex_index + 3,
    });
}

/// Generates a simple, blocky, non-greedy mesh for a single Voxeme.
pub fn voxemeMeshBasic(self: *const Grid, gpa: std.mem.Allocator, page_index: u32, local_voxeme_coord: LocalCoord, vertices: *std.MultiArrayList(MeshCache.VertexData), indices: *std.ArrayList(u32)) !void {
    const page_coord = self.pages.items(.coord)[page_index];
    const voxeme_index = self.pages.items(.voxeme_indirection)[page_index].lookup(local_voxeme_coord);
    if (voxeme_index == VoxemeTable.sentinel) {
        log.err("voxemeMeshBasic called on non-existent voxeme at {any}, {any}", .{ page_coord, local_voxeme_coord });
        return;
    }

    const buffer_index = self.voxemes.items(.buffer_indirection)[voxeme_index];

    if (buffer_index == BufferData.sentinel) {
        // large-volume voxel
        const data = self.voxemes.items(.voxel)[voxeme_index];
        if (data.material_id == .none) {
            log.err("voxemeMeshBasic called on empty voxeme {any}, skipping", .{local_voxeme_coord});
            return;
        }

        const local_voxeme_origin = convert.partsToGlobalVoxel(page_coord, local_voxeme_coord, .{ .x = 0, .y = 0, .z = 0 });
        const world_voxeme_origin = convert.globalVoxelToWorld(local_voxeme_origin);

        const visibility = self.voxemes.items(.visibility)[voxeme_index];

        for (AxisDir.values) |axis_dir| {
            const visible_face = visibility.get(axis_dir);

            if (visible_face) {
                try addFace(
                    gpa,
                    vertices,
                    indices,
                    axis_dir,
                    world_voxeme_origin,
                    voxeme_scale,
                    data.material_id,
                );
            }
        }
    } else {
        // mesh the dense voxel data
        const data = self.voxels.items(.voxel)[buffer_index];
        const visibility = self.voxels.items(.visibility)[buffer_index];

        for (0..voxels_per_voxeme) |voxel_1d_index| {
            const voxel = data[voxel_1d_index];
            if (voxel.material_id == .none) continue;

            const vis = visibility[voxel_1d_index];

            const local_voxel_coord = convert.indexToLocalVoxelCoord(@intCast(voxel_1d_index));
            const global_voxel_coord = convert.partsToGlobalVoxel(page_coord, local_voxeme_coord, local_voxel_coord);
            const world_voxel_coord = convert.globalVoxelToWorld(global_voxel_coord);

            for (AxisDir.values) |axis_dir| {
                const visible_face = vis.get(axis_dir);

                if (visible_face) {
                    try addFace(
                        gpa,
                        vertices,
                        indices,
                        axis_dir,
                        world_voxel_coord,
                        voxel_scale,
                        voxel.material_id,
                    );
                }
            }
        }
    }
}

/// Generates a simple, blocky, non-greedy mesh for an entire Page.
pub fn pageMeshBasic(self: *const Grid, gpa: std.mem.Allocator, page_coord: PageCoord, vertices: *std.MultiArrayList(MeshCache.VertexData), indices: *std.ArrayList(u32)) !void {
    const page_index = self.page_indirection.lookup(page_coord);
    if (page_index == PageTable.sentinel) {
        log.err("pageMeshBasic called on non-existent page at {any}", .{page_coord});
        return;
    }

    const is_hetero = self.pages.items(.voxeme_indirection)[page_index].len != 0;
    const page_homo_voxel = self.pages.items(.voxel)[page_index];

    if (!is_hetero) {
        // large-volume voxel
        if (page_homo_voxel.material_id == .none) {
            log.err("pageMeshBasic detected residual empty page at {any}", .{page_coord});
            return;
        }
        const page_homo_vis = self.pages.items(.visibility)[page_index];

        const local_page_origin = convert.partsToGlobalVoxel(page_coord, .{ .x = 0, .y = 0, .z = 0 }, .{ .x = 0, .y = 0, .z = 0 });
        const world_page_origin = convert.globalVoxelToWorld(local_page_origin);

        for (AxisDir.values) |axis_dir| {
            const visible_face = page_homo_vis.get(axis_dir);
            if (visible_face) {
                try addFace(
                    gpa,
                    vertices,
                    indices,
                    axis_dir,
                    world_page_origin,
                    page_scale,
                    page_homo_voxel.material_id,
                );
            }
        }
    } else {
        for (0..voxemes_per_page) |voxeme_1d_index| {
            const local_voxeme_coord = convert.indexToLocalVoxemeCoord(@intCast(voxeme_1d_index));

            const voxeme_index = self.pages.items(.voxeme_indirection)[page_index].lookup(local_voxeme_coord);
            if (voxeme_index != VoxemeTable.sentinel) {
                try voxemeMeshBasic(self, gpa, page_index, local_voxeme_coord, vertices, indices);
            } else {
                if (page_homo_voxel.material_id == .none) continue;

                try implicitVoxemeMeshBasic(self, gpa, page_coord, local_voxeme_coord, vertices, indices);
            }
        }
    }
}

pub fn implicitVoxemeMeshBasic(self: *const Grid, gpa: std.mem.Allocator, page_coord: PageCoord, local_voxeme_coord: LocalCoord, vertices: *std.MultiArrayList(MeshCache.VertexData), indices: *std.ArrayList(u32)) !void {
    _ = self;
    _ = gpa;
    _ = page_coord;
    _ = local_voxeme_coord;
    _ = vertices;
    _ = indices;
    log.err("TODO: Skipping implicit voxeme", .{});
}
