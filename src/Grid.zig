//! A 3D Voxel grid space
//!
//! This is partitioned into Pages (chunks), which are further partitioned into Voxemes (bricks), which contain the actual Voxels.
//!
//! Pages and Voxemes are sparse, while Voxels are dense.
//!
//! We define a minimum voxel resolution of 1 Voxeme / 16^3 Voxels per cubic meter,
//! and one Page contains up to 16^3 Voxemes (4096^3 Voxels).

const Grid = @This();

const std = @import("std");
const linalg = @import("linalg.zig");

const vec2 = linalg.vec2;
const vec3 = linalg.vec3;
const vec3i = linalg.vec3i;

const log = std.log.scoped(.grid);

test {
    log.debug("semantic analysis for Grid.zig", .{});
    std.testing.refAllDecls(@This());
}

/// Number of Voxemes along one edge of a Page
pub const page_length: u32 = 16;
/// Number of Voxels along one edge of a Voxeme
pub const voxeme_length: u32 = 16;

/// Axis-size of one Voxeme in world units
pub const voxeme_scale: f32 = 1.0;
/// Axis-size of one Voxel in world units
pub const voxel_scale: f32 = voxeme_scale / @as(f32, @floatFromInt(voxeme_length));
/// Axis-size of one Page in world units
pub const page_scale: f32 = voxeme_scale * @as(f32, @floatFromInt(page_length));

/// Number of Voxels in one Voxeme
pub const voxels_per_voxeme = voxeme_length * voxeme_length * voxeme_length;

/// Maximum number of Voxemes in one Page
pub const voxemes_per_page = page_length * page_length * page_length;

/// Maxmimum number of Voxels in one Page
pub const voxels_per_page = voxels_per_voxeme * voxemes_per_page;

/// A 3D float vector representing a position in world space.
pub const Position = linalg.vec3;

/// A 3D integer vector representing a position in voxel space.
pub const VoxelCoord = linalg.vec3i;

/// A 3D integer vector representing a position in buffer space within a Voxeme.
pub const BufferCoord = linalg.vec3u;

/// A 3D integer vector representing a position in voxeme space.
pub const VoxemeCoord = linalg.vec3i;

/// A 3D integer vector representing a position in page space.
pub const PageCoord = linalg.vec3i;

/// Unique identifier for a material type in the voxel Grid's world. Allows for 1023 different materials.
pub const MaterialId = enum(u10) { none = 0, _ };

/// A buffer of Voxels, used to back a heterogeneous Voxeme.
pub const BufferData = [voxels_per_voxeme]Voxel;

/// The address of a Buffer of Voxels inside a MemoryPool (which has a minimum alignment of 8).
pub const Buffer = *align(8) BufferData;

/// Determines where to offset a position relative to a voxel coordinate along an axis.
pub const SampleOffset = enum {
    min,
    center,
    max,

    pub fn toInt(self: SampleOffset, scale: i32) i32 {
        return switch (self) {
            .min => 0,
            .center => @divExact(scale, 2),
            .max => scale,
        };
    }

    pub fn toFloat(self: SampleOffset, scale: f32) f32 {
        return switch (self) {
            .min => 0.0,
            .center => scale * 0.5,
            .max => scale,
        };
    }
};

/// A set of 3 SampleOffset, one for each axis.
pub const SampleOffsets = struct {
    x: SampleOffset,
    y: SampleOffset,
    z: SampleOffset,

    pub const min = SampleOffsets{ .x = .min, .y = .min, .z = .min };
    pub const center = SampleOffsets{ .x = .center, .y = .center, .z = .center };
    pub const max = SampleOffsets{ .x = .max, .y = .max, .z = .max };

    pub fn init(x: SampleOffset, y: SampleOffset, z: SampleOffset) SampleOffsets {
        return SampleOffsets{ .x = x, .y = y, .z = z };
    }

    pub fn toVec3i(self: SampleOffsets, scale: vec3i) vec3i {
        return vec3i{ self.x.toInt(scale[0]), self.y.toInt(scale[1]), self.z.toInt(scale[2]) };
    }

    pub fn toVec3(self: SampleOffsets, scale: vec3) vec3 {
        return vec3{ self.x.toFloat(scale[0]), self.y.toFloat(scale[1]), self.z.toFloat(scale[2]) };
    }
};

pub const convert = struct {
    test {
        log.debug("semantic analysis for Grid.convert", .{});
        std.testing.refAllDecls(@This());
    }

    // Storing the shift amount and mask at compile time makes the code cleaner
    // and ties the implementation directly to the constants.
    const page_shift = @ctz(page_length);
    const page_mask = page_length - 1;
    const voxeme_shift = @ctz(voxeme_length);
    const voxeme_mask = voxeme_length - 1;

    // The total shift from voxel-space to page-space.
    const total_shift = page_shift + voxeme_shift;

    // Assert that our bit-shifting logic is valid by ensuring the lengths are powers of two.
    comptime {
        std.debug.assert(std.math.isPowerOfTwo(page_length));
        std.debug.assert(std.math.isPowerOfTwo(voxeme_length));
    }

    /// world-space position to voxel-space coordinate
    pub inline fn worldToVoxel(pos: Position) VoxelCoord {
        return @as(VoxelCoord, @intFromFloat(@floor(pos / @as(Position, @splat(voxel_scale)))));
    }

    /// voxel-space coordinate to world-space position
    pub inline fn voxelToWorld(v: VoxelCoord, offsets: SampleOffsets) Position {
        return @as(Position, @floatFromInt(v)) * @as(Position, @splat(voxel_scale)) + offsets.toVec3(@splat(voxel_scale));
    }

    /// world-space position to voxeme-space coordinate
    pub inline fn worldToVoxeme(pos: Position) VoxemeCoord {
        return @as(VoxemeCoord, @intFromFloat(@floor(pos / @as(Position, @splat(voxeme_scale)))));
    }

    /// voxeme-space coordinate to world-space position
    pub inline fn voxemeToWorld(v: VoxemeCoord, offsets: SampleOffsets) Position {
        return @as(Position, @floatFromInt(v)) * @as(Position, @splat(voxeme_scale)) + offsets.toVec3(@splat(voxeme_scale));
    }

    /// world-space position to page-space coordinate
    pub inline fn worldToPage(pos: Position) PageCoord {
        return @as(PageCoord, @intFromFloat(@floor(pos / @as(Position, @splat(page_scale)))));
    }

    /// page-space coordinate to world-space position
    pub inline fn pageToWorld(v: PageCoord, offsets: SampleOffsets) Position {
        return @as(Position, @floatFromInt(v)) * @as(Position, @splat(page_scale)) + offsets.toVec3(@splat(page_scale));
    }

    /// voxel-space coordinate to voxeme-space coordinate
    pub inline fn voxelToVoxeme(v: VoxelCoord) VoxemeCoord {
        // Use an arithmetic right shift for fast floor division by a power of two.
        // This correctly handles negative coordinates.
        return v >> @splat(voxeme_shift);
    }

    /// voxel-space coordinate to local buffer coordinate within its containing Voxeme
    pub inline fn voxelToBuffer(v: VoxelCoord) BufferCoord {
        // Use a bitwise AND for a fast modulo operation by a power of two.
        // This also correctly handles negative coordinates.
        return @intCast(v & @as(VoxelCoord, @splat(voxeme_mask)));
    }

    /// buffer-space coordinate to index
    pub inline fn bufferToIndex(v: BufferCoord) usize {
        // This is a standard 3D to 1D index conversion.
        return v[0] + (v[1] * voxeme_length) + (v[2] * voxeme_length * voxeme_length);
    }

    /// index to buffer-space coordinate
    pub inline fn indexToBuffer(index: usize) BufferCoord {
        const L = voxeme_length;
        const L2 = voxeme_length * voxeme_length;

        // v[2] (z) is how many full 2D slices we have.
        const z = index / L2;
        // v[1] (y) is how many full rows are in the remainder.
        const y = (index % L2) / L;
        // v[0] (x) is the column in the current row.
        const x = index % L;

        return @intCast(@Vector(3, usize){ x, y, z });
    }

    /// buffer index to voxel-space coordinate
    pub inline fn indexToVoxel(v: VoxemeCoord, index: usize) VoxelCoord {
        return voxemeToVoxel(v, .min) + @as(VoxelCoord, @intCast(indexToBuffer(index)));
    }

    /// voxeme-space coordinate to voxel-space coordinate
    pub inline fn voxemeToVoxel(v: VoxemeCoord, offsets: SampleOffsets) VoxelCoord {
        return (v * @as(VoxemeCoord, @splat(voxeme_length))) + offsets.toVec3i(@splat(voxeme_length));
    }

    /// voxeme-space coordinate to page-space coordinate
    pub inline fn voxemeToPage(v: VoxemeCoord) PageCoord {
        return v >> @splat(page_shift);
    }

    /// page-space coordinate to voxeme-space coordinate
    pub inline fn pageToVoxeme(v: PageCoord, offsets: SampleOffsets) VoxemeCoord {
        return (v * @as(PageCoord, @splat(page_length))) + offsets.toVec3i(@splat(page_length));
    }

    /// voxel-space coordinate to page-space coordinate
    pub inline fn voxelToPage(v: VoxelCoord) PageCoord {
        // Directly convert from voxel to page space by combining the shifts.
        return v >> @splat(total_shift);
    }

    /// page-space coordinate to voxel-space coordinate
    pub inline fn pageToVoxel(v: PageCoord, page_offsets: SampleOffsets, voxeme_offsets: SampleOffsets) VoxelCoord {
        return voxemeToVoxel(pageToVoxeme(v, page_offsets), voxeme_offsets);
    }

    /// page-local 1D index to a 3D voxeme coordinate
    /// The index ranges from 0 to `voxemes_per_page - 1`.
    pub inline fn pageIndexToVoxemeCoord(page: PageCoord, index: usize) VoxemeCoord {
        const first_voxeme = pageToVoxeme(page, .min);
        const L = page_length;
        const x: i32 = @intCast(index % L);
        const y: i32 = @intCast((index / L) % L);
        const z: i32 = @intCast(index / (L * L));
        return first_voxeme + VoxemeCoord{ x, y, z };
    }
};

/// A sparse map of all existing Voxemes in a particular area, keyed by their VoxemeCoord.
pub const Page = struct {
    homogeneous: Voxel,
    visibility: Visibility = .all,
    heterogeneous: std.AutoHashMapUnmanaged(VoxemeCoord, Voxeme) = .empty,

    pub const empty = Page{ .homogeneous = .empty, .visibility = .all };

    pub fn deinit(self: *Page, gpa: std.mem.Allocator) void {
        self.heterogeneous.deinit(gpa);
        // debug safety: prevent use after free
        self.* = undefined;
    }

    pub fn isHeterogeneous(self: *const Page) bool {
        return self.heterogeneous.count() > 0;
    }
};

/// A Voxeme is a small, dense 3D grid of Voxels.
/// This is represented in our system as a tagged pointer, allowing homogenous Voxemes to avoid an allocation.
pub const Voxeme = packed struct(u64) {
    is_homogeneous: bool,
    is_dirty: bool,
    visibility: Visibility,
    _reserved: u8 = 0,
    payload: packed union {
        /// A homogeneous Voxeme is represented as a single MaterialId and state value, without allocating the array.
        /// This should never have value.material_id == .none/0, which would indicate a homogeneously empty Voxeme, which should be removed from the Page entirely.
        homogeneous: Voxel,
        /// the OS pointer is at most 48 bits; we store it here
        heterogeneous: u48,
    },

    /// Create a homogenous Voxeme containing the given Voxel.
    /// * Note that while creating a homogeneous Voxeme with an empty MaterialId is allowed, this is not a valid state for a Voxeme in a Page.
    pub fn homogeneous(voxel: Voxel) Voxeme {
        return Voxeme{
            .is_homogeneous = true,
            .is_dirty = false,
            .visibility = .all,
            .payload = .{ .homogeneous = voxel },
        };
    }

    /// Create a heterogeneous Voxeme containing the given pointer to an array of Voxels.
    pub fn heterogeneous(voxels: Buffer) Voxeme {
        return Voxeme{
            .is_homogeneous = false,
            .is_dirty = true,
            .visibility = .all,
            .payload = .{ .heterogeneous = @intCast(@intFromPtr(voxels)) },
        };
    }

    /// Get this Voxeme as a pointer to a heterogeneous Buffer.
    /// * Asserts that it is indeed heterogeneous
    pub fn asHeterogeneous(self: Voxeme) Buffer {
        std.debug.assert(!self.is_homogeneous);
        return @ptrFromInt(self.payload.heterogeneous);
    }
};

/// The axis along which a Voxel face lies.
pub const Axis = enum(u3) {
    x = 0,
    y = 2,
    z = 4,
};

/// Pre-calculated visibility flags for each face-neighbor of a Voxel.
pub const Visibility = packed struct(u6) {
    // Note: fields of packed structs are arranged in the provided order from LSB to MSB
    pos_x: bool,
    neg_x: bool,
    pos_y: bool,
    neg_y: bool,
    pos_z: bool,
    neg_z: bool,

    pub const all = Visibility{ .pos_x = true, .neg_x = true, .pos_y = true, .neg_y = true, .pos_z = true, .neg_z = true };
    pub const none = Visibility{ .pos_x = false, .neg_x = false, .pos_y = false, .neg_y = false, .pos_z = false, .neg_z = false };

    /// Gets the visibility state for a face by axis and direction.
    pub fn get(self: Visibility, axis: Axis, positive: bool) bool {
        const bits: u6 = @bitCast(self);
        const axis_offset: u3 = @intFromEnum(axis);
        const bit_index: u3 = if (positive) axis_offset else axis_offset + 1;
        const mask = @as(u6, 1) << bit_index;
        return (bits & mask) != 0;
    }

    /// Sets the visibility state for a face by axis and direction.
    pub fn set(self: anytype, axis: Axis, positive: bool, value: bool) void {
        const TInfo = @typeInfo(@TypeOf(self));
        comptime std.debug.assert(TInfo == .pointer and !TInfo.pointer.is_const);

        var bits: u6 = @bitCast(self.*);
        const axis_offset: u3 = @intFromEnum(axis);
        const bit_index: u3 = if (positive) axis_offset else axis_offset + 1;
        const mask = @as(u6, 1) << bit_index;
        if (value) {
            bits |= mask;
        } else {
            bits &= ~mask;
        }
        self.* = @bitCast(bits);
    }
};

pub const VolumeIterator = struct {
    size: i32,
    x_offset: i32 = 0,
    y_offset: i32 = 0,
    z_offset: i32 = 0,

    pub fn forVoxeme() VolumeIterator {
        return .{ .size = @intCast(voxeme_length) };
    }

    pub fn forPage() VolumeIterator {
        return .{ .size = @intCast(page_length) };
    }

    pub fn next(self: *VolumeIterator) ?vec3i {
        if (self.z_offset >= self.size) return null;

        const out = vec3i{ self.x_offset, self.y_offset, self.z_offset };

        self.x_offset += 1;
        if (self.x_offset >= self.size) {
            self.x_offset = 0;
            self.y_offset += 1;
            if (self.y_offset >= self.size) {
                self.y_offset = 0;
                self.z_offset += 1;
            }
        }

        return out;
    }
};

/// Given a local coordinate within a Page or Voxeme, iterates the coordinates of the bordering sub-units along a given axis.
/// * Note that this iterates the face of the *current* Page/Voxeme, not the neighboring one.
///   For example, iterating the positive X border of a Voxeme will yield coordinates with x == voxeme_length - 1.
pub const BorderIterator = struct {
    size: i32,
    axis: Axis,
    positive: bool,
    axis_a_offset: i32 = 0,
    axis_b_offset: i32 = 0,

    pub fn forVoxeme(axis: Axis, positive: bool) BorderIterator {
        return .{
            .size = @intCast(voxeme_length),
            .axis = axis,
            .positive = positive,
        };
    }

    pub fn forPage(axis: Axis, positive: bool) BorderIterator {
        return .{
            .size = @intCast(page_length),
            .axis = axis,
            .positive = positive,
        };
    }

    pub fn next(self: *BorderIterator) ?vec3i {
        if (self.axis_a_offset >= self.size) return null;

        const out = switch (self.axis) {
            .x => vec3i{
                if (self.positive) self.size - 1 else 0,
                self.axis_a_offset,
                self.axis_b_offset,
            },
            .y => vec3i{
                self.axis_a_offset,
                if (self.positive) self.size - 1 else 0,
                self.axis_b_offset,
            },
            .z => vec3i{
                self.axis_a_offset,
                self.axis_b_offset,
                if (self.positive) self.size - 1 else 0,
            },
        };

        self.axis_b_offset += 1;
        if (self.axis_b_offset >= self.size) {
            self.axis_b_offset = 0;
            self.axis_a_offset += 1;
        }

        return out;
    }
};

pub const Neighbor = struct {
    axis: Axis,
    positive: bool,
    coord_offset: vec3i,

    pub const coordinates = [_]vec3i{
        .{ 1, 0, 0 }, // pos_x
        .{ -1, 0, 0 }, // neg_x
        .{ 0, 1, 0 }, // pos_y
        .{ 0, -1, 0 }, // neg_y
        .{ 0, 0, 1 }, // pos_z
        .{ 0, 0, -1 }, // neg_z
    };

    pub fn get(axis: Axis, positive: bool) vec3i {
        const index = switch (axis) {
            .x => if (positive) 0 else 1,
            .y => if (positive) 2 else 3,
            .z => if (positive) 4 else 5,
        };
        return coordinates[index];
    }

    pub const Iterator = struct {
        index: usize = 0,

        pub fn next(self: *Iterator) ?Neighbor {
            if (self.index >= coordinates.len) return null;
            const axis = switch (self.index) {
                0, 1 => Axis.x,
                2, 3 => Axis.y,
                4, 5 => Axis.z,
                else => unreachable,
            };
            const positive = (self.index % 2) == 0;
            const coord_offset = coordinates[self.index];
            self.index += 1;
            return Neighbor{ .axis = axis, .positive = positive, .coord_offset = coord_offset };
        }
    };
};

/// Material properties associated with a MaterialId in a Grid.
pub const MaterialProperties = packed struct {
    is_opaque: bool,
    color: vec3,

    pub const none = MaterialProperties{ .is_opaque = false, .color = .{ 0, 0, 0 } };
};

/// A Lite voxel for input with setVoxel.
pub const LiteVoxel = struct {
    /// The material type of this voxel. A value of `none`/`0` indicates an empty voxel.
    material_id: MaterialId,
    /// Material-specific state data, e.g. amount of fluid, rich-state map key, etc.
    state: u16 = 0,

    pub const empty = LiteVoxel{ .material_id = .none };

    /// Equality comparison on LiteVoxels vs other LiteVoxels or Voxels.
    pub fn eql(self: LiteVoxel, other: anytype) bool {
        return self.material_id == other.material_id and self.state == other.state;
    }
};

/// The smallest unit of volume in our voxel Grid's world.
pub const Voxel = packed struct(u32) {
    /// The material type of this voxel. A value of `none`/`0` indicates an empty voxel.
    material_id: MaterialId,
    /// The visibility flags for each face-neighbor of this voxel.
    /// These are precalculated per frame in the update function.
    /// This accelerates meshing and raytracing greatly by avoiding redundant lookups that make incur a hashmap traversal.
    visibility: Visibility = .none,
    /// Material-specific state data, e.g. amount of fluid, rich-state map key, etc.
    state: u16 = 0,

    pub const empty = Voxel{ .material_id = .none };

    /// Equality comparison on Voxels vs LiteVoxels or Voxels without considering visibility.
    /// This is useful for determining if a structure can be converted to homogeneous.
    pub fn eql(self: Voxel, other: anytype) bool {
        return self.material_id == other.material_id and self.state == other.state;
    }

    /// Copy data from a LiteVoxel into this Voxel.
    /// Note that this does not modify visibility; the Voxeme must be set to dirty separately.
    pub fn set(self: *Voxel, lite: LiteVoxel) void {
        self.material_id = lite.material_id;
        self.state = lite.state;
    }

    /// Create a Voxel from a LiteVoxel, with visibility set to none.
    pub fn fromLite(lite: LiteVoxel) Voxel {
        return Voxel{ .material_id = lite.material_id, .visibility = .none, .state = lite.state };
    }

    /// Create a LiteVoxel from this Voxel, ignoring visibility.
    pub fn toLite(self: Voxel) LiteVoxel {
        return LiteVoxel{ .material_id = self.material_id, .state = self.state };
    }
};

/// Standard vertex format for rendering.
pub const Vertex = packed struct {
    pos: vec3,
    norm: vec3,
    uv: vec2,
    color: vec3,

    pub fn init(pos: vec3, norm: vec3, uv: vec2, color: vec3) Vertex {
        return .{ .pos = pos, .norm = norm, .uv = uv, .color = color };
    }
};

/// The memory pool used to allocate heterogeneous Buffers.
pool: std.heap.MemoryPool(BufferData),

/// A sparse map of all existing Pages in the Grid, keyed by their PageCoord.
pages: std.AutoHashMapUnmanaged(PageCoord, Page) = .empty,

/// A map of all material properties in this Grid, keyed by their MaterialId.
materials: std.AutoHashMapUnmanaged(MaterialId, MaterialProperties) = .empty,

/// A set of all Pages that need processing in the next `update` tick.
dirty_pages: std.AutoHashMapUnmanaged(PageCoord, void) = .empty,

/// Create a new empty Grid with a preheated pool for Buffers.
pub fn init(backing_allocator: std.mem.Allocator) !Grid {
    var out = Grid{
        .pool = try .initPreheated(backing_allocator, 4096),
    };

    try out.pages.ensureTotalCapacity(backing_allocator, 4096);
    try out.materials.ensureTotalCapacity(backing_allocator, std.math.maxInt(std.meta.Tag(MaterialId)));
    try out.dirty_pages.ensureTotalCapacity(backing_allocator, 1024);

    out.materials.putAssumeCapacity(.none, .none);

    return out;
}

/// Deinitialize the Grid, freeing all allocated memory.
pub fn deinit(self: *Grid) void {
    const gpa = self.allocator();

    // Deinitialize all Pages
    var it = self.pages.valueIterator();
    while (it.next()) |page| page.deinit(gpa);

    self.pages.deinit(gpa);
    self.materials.deinit(gpa);
    self.dirty_pages.deinit(gpa);

    // Destroying the pool will free all allocated Buffers
    self.pool.deinit();

    // debug safety: prevent use after free
    self.* = undefined;
}

/// Get the `backing_allocator` passed during `Grid.init`.
pub fn allocator(self: *const Grid) std.mem.Allocator {
    return self.pool.arena.child_allocator;
}

/// Allocates a new Buffer from the memory pool.
fn allocateBuffer(self: *Grid) !Buffer {
    return self.pool.create();
}

/// Frees a previously allocated Buffer back to the memory pool.
fn deallocateBuffer(self: *Grid, buffer: Buffer) void {
    self.pool.destroy(buffer);
}

/// Binds a new MaterialProperties to a new MaterialId in the Grid.
/// Returns the new MaterialId, or an error if the maximum number of materials has been reached. See MaterialId for max.
pub fn bindMaterial(self: *Grid, properties: MaterialProperties) !MaterialId {
    const len = self.materials.count();
    if (len >= @as(usize, std.math.maxInt(std.meta.Tag(MaterialId)))) {
        return error.TooManyMaterials;
    }

    const id: MaterialId = @enumFromInt(len);
    self.materials.putAssumeCapacity(id, properties);

    return id;
}

/// Get the MaterialProperties associated with a MaterialId, or `MaterialProperties.none` if not found.
pub fn getMaterialProperties(self: *const Grid, id: MaterialId) MaterialProperties {
    return self.materials.get(id) orelse MaterialProperties.none;
}

/// Determine if a given voxel-space coordinate is mapped (Not whether it exists).
pub fn isVoxelMapped(self: *const Grid, coord: VoxelCoord) bool {
    const page_coord = convert.voxelToPage(coord);
    return self.pages.contains(page_coord);
}

/// Loop all dirty Pages, precalculate Voxel visibility, homogenize, and prune extraneous data structures.
pub fn update(self: *Grid, frame_arena: std.mem.Allocator) !void {
    var pages_pruned: std.AutoHashMapUnmanaged(PageCoord, void) = .empty;
    defer pages_pruned.deinit(frame_arena);

    var voxemes_pruned: std.AutoHashMapUnmanaged(VoxemeCoord, void) = .empty;
    defer voxemes_pruned.deinit(frame_arena);

    try self.homogenizeAndPrune(frame_arena, &pages_pruned, &voxemes_pruned);
    try self.recalculateVisibility(frame_arena, &pages_pruned, &voxemes_pruned);

    var dirty_it = self.dirty_pages.keyIterator();
    while (dirty_it.next()) |page_coord_ptr| {
        const page = self.pages.getPtr(page_coord_ptr.*) orelse unreachable;

        var voxeme_it = page.heterogeneous.valueIterator();
        while (voxeme_it.next()) |voxeme| {
            voxeme.is_dirty = false;
        }
    }

    self.dirty_pages.clearRetainingCapacity();
}

/// Loop all dirty Pages and their neighbors to recalculate Voxel visibility.
pub fn recalculateVisibility(self: *Grid, frame_arena: std.mem.Allocator, pages_pruned: *const std.AutoHashMapUnmanaged(PageCoord, void), voxemes_pruned: *const std.AutoHashMapUnmanaged(VoxemeCoord, void)) !void {
    // we need to traverse all dirty pages and process them and potentially their neighbors
    // to ensure that visibility is correct across page boundaries

    // we need to track which voxemes or homogeneous pages bordering the dirty pages need to be updated
    var border_pages: std.AutoHashMapUnmanaged(PageCoord, Visibility) = .empty;
    defer border_pages.deinit(frame_arena);

    var border_voxemes: std.AutoHashMapUnmanaged(VoxemeCoord, Visibility) = .empty;
    defer border_voxemes.deinit(frame_arena);

    // first we can queue the neighbors of all deleted pages
    var page_pruned_it = pages_pruned.keyIterator();
    while (page_pruned_it.next()) |page_entry| {
        const page_coord = page_entry.*;

        // queue all 6 neighbors of this page, if they exist
        comptime var neighbor_it = Neighbor.Iterator{};
        inline while (comptime neighbor_it.next()) |neighbor| {
            const neighbor_page_coord = page_coord + neighbor.coord_offset;
            if (!self.dirty_pages.contains(neighbor_page_coord) and self.pages.contains(neighbor_page_coord)) {
                const border_page_gop = try border_pages.getOrPut(frame_arena, neighbor_page_coord);
                if (!border_page_gop.found_existing) border_page_gop.value_ptr.* = .none;
                border_page_gop.value_ptr.set(neighbor.axis, neighbor.positive, true);
            }
        }
    }

    // and queue the neighbors of all deleted voxemes
    var voxeme_pruned_it = voxemes_pruned.keyIterator();
    while (voxeme_pruned_it.next()) |voxeme_entry| {
        const voxeme_coord = voxeme_entry.*;

        // queue all 6 neighbors of this voxeme, and any neighboring pages, if they exist
        comptime var neighbor_it = Neighbor.Iterator{};
        inline while (comptime neighbor_it.next()) |neighbor| {
            const neighbor_voxeme_coord = voxeme_coord + neighbor.coord_offset;
            const neighbor_page_coord = convert.voxemeToPage(neighbor_voxeme_coord);
            if (!self.dirty_pages.contains(neighbor_page_coord)) {
                if (self.pages.getPtr(neighbor_page_coord)) |neighbor_page| {
                    const border_page_gop = try border_pages.getOrPut(frame_arena, neighbor_page_coord);
                    if (!border_page_gop.found_existing) border_page_gop.value_ptr.* = .none;
                    border_page_gop.value_ptr.set(neighbor.axis, neighbor.positive, true);

                    if (neighbor_page.heterogeneous.contains(neighbor_voxeme_coord)) {
                        const border_voxeme_gop = try border_voxemes.getOrPut(frame_arena, neighbor_voxeme_coord);
                        if (!border_voxeme_gop.found_existing) border_voxeme_gop.value_ptr.* = .none;
                        border_voxeme_gop.value_ptr.set(neighbor.axis, neighbor.positive, true);
                    }
                }
            }
        }
    }

    // next we must process all dirty pages
    var page_it = self.dirty_pages.keyIterator();
    while (page_it.next()) |page_entry| {
        const page_coord = page_entry.*;

        if (self.pages.getPtr(page_coord)) |page| {
            // start with no visibility; if any interior borders have visibility, change this for the respective edge
            var new_page_vis: Visibility = .none;

            if (!page.isHeterogeneous()) {
                // the entire page is a single homogeneous large-volume voxel
                const data = page.homogeneous;

                const local_is_solid = self.getMaterialProperties(data.material_id).is_opaque;

                // recalculate visibility for the single large-volume voxel in this page and within its neighbors
                comptime var neighbor_it = Neighbor.Iterator{};
                inline while (comptime neighbor_it.next()) |neighbor| {
                    const neighbor_page_coord = page_coord + neighbor.coord_offset;

                    if (self.pages.getPtr(neighbor_page_coord)) |neighbor_page| {
                        // queue neighbor page update if it hasn't been already
                        if (!self.dirty_pages.contains(neighbor_page_coord)) {
                            const neighbor_page_gop = try border_pages.getOrPut(frame_arena, neighbor_page_coord);
                            if (!neighbor_page_gop.found_existing) neighbor_page_gop.value_ptr.* = .none;
                            // if we are not solid, mark the opposing face visible in the queued neighbor
                            if (!local_is_solid) neighbor_page_gop.value_ptr.set(neighbor.axis, !neighbor.positive, true);
                        }

                        if (neighbor_page.isHeterogeneous()) {
                            // we need to traverse the bordering face voxemes of the neighbor, checking that they are completely solid across their volume face
                            const reference_voxeme_coord = convert.pageToVoxeme(neighbor_page_coord, .min);
                            var voxeme_border_it = BorderIterator.forPage(neighbor.axis, !neighbor.positive);
                            var face_is_solid = true;
                            while (voxeme_border_it.next()) |local_coord| {
                                const neighbor_voxeme_coord = reference_voxeme_coord + local_coord;

                                if (neighbor_page.heterogeneous.getPtr(neighbor_voxeme_coord)) |neighbor_voxeme| {
                                    // queue the neighbor voxeme for update if it hasn't been already
                                    if (!self.dirty_pages.contains(neighbor_page_coord)) {
                                        const neighbor_voxeme_gop = try border_voxemes.getOrPut(frame_arena, neighbor_voxeme_coord);
                                        if (!neighbor_voxeme_gop.found_existing) neighbor_voxeme_gop.value_ptr.* = .none;

                                        // if we are not solid, mark the opposing face visible in the queued neighbor
                                        if (!local_is_solid) neighbor_voxeme_gop.value_ptr.set(neighbor.axis, !neighbor.positive, true);
                                    }

                                    if (neighbor_voxeme.is_homogeneous) {
                                        const neighbor_data = neighbor_voxeme.payload.homogeneous;
                                        const neighbor_is_solid = self.getMaterialProperties(neighbor_data.material_id).is_opaque;
                                        if (!neighbor_is_solid) {
                                            face_is_solid = false;
                                        }
                                    } else {
                                        const neighbor_data = neighbor_voxeme.asHeterogeneous();

                                        const reference_voxel_coord = convert.voxemeToVoxel(neighbor_voxeme_coord, .min);
                                        var voxel_border_it = BorderIterator.forVoxeme(neighbor.axis, !neighbor.positive);

                                        while (voxel_border_it.next()) |local_voxel_coord| {
                                            const index = convert.bufferToIndex(@intCast(reference_voxel_coord + local_voxel_coord));
                                            const neighbor_voxel = &neighbor_data[index];
                                            const neighbor_is_solid = self.getMaterialProperties(neighbor_voxel.material_id).is_opaque;
                                            if (!neighbor_is_solid) {
                                                face_is_solid = false;
                                            }

                                            // update the neighbor voxel's visibility directly, if it isn't dirty itself
                                            if (!neighbor_voxeme.is_dirty) neighbor_voxel.visibility.set(neighbor.axis, !neighbor.positive, !local_is_solid);
                                        }
                                    }
                                } else {
                                    // no neighboring voxeme does not necessarily mean empty space;
                                    // the neighbor page could have a solid as its homogeneous voxel base
                                    const neighbor_is_solid = self.getMaterialProperties(neighbor_page.homogeneous.material_id).is_opaque;
                                    if (!neighbor_is_solid) {
                                        face_is_solid = false;
                                    }
                                }
                            }

                            if (!face_is_solid) {
                                new_page_vis.set(neighbor.axis, neighbor.positive, true);
                            }
                        } else {
                            // we can update our local visibility directly
                            const neighbor_data = neighbor_page.homogeneous;
                            const neighbor_is_solid = self.getMaterialProperties(neighbor_data.material_id).is_opaque;
                            if (!neighbor_is_solid) {
                                new_page_vis.set(neighbor.axis, neighbor.positive, true);
                            }

                            if (!self.dirty_pages.contains(neighbor_page_coord)) {
                                // queue neighbor page update if it hasn't been already
                                const neighbor_page_gop = try border_pages.getOrPut(frame_arena, neighbor_page_coord);
                                if (!neighbor_page_gop.found_existing) neighbor_page_gop.value_ptr.* = .none;

                                // if we are not solid, mark the opposing face visible in the queued neighbor
                                if (!local_is_solid) neighbor_page_gop.value_ptr.set(neighbor.axis, !neighbor.positive, true);
                            }
                        }
                    } else {
                        // we can update our local visibility directly
                        new_page_vis.set(neighbor.axis, neighbor.positive, true);
                    }
                }
            } else {
                // iterate all voxemes in this page
                var voxeme_it = page.heterogeneous.iterator();
                while (voxeme_it.next()) |voxeme_entry| {
                    const voxeme_coord = voxeme_entry.key_ptr.*;
                    const voxeme = voxeme_entry.value_ptr;

                    var new_voxeme_vis: Visibility = .none;
                    if (voxeme.is_homogeneous) {
                        const data = voxeme.payload.homogeneous;
                        const local_is_solid = self.getMaterialProperties(data.material_id).is_opaque;

                        // recalculate visibility for the single large-volume voxel in this voxeme
                        comptime var neighbor_it = Neighbor.Iterator{};
                        inline while (comptime neighbor_it.next()) |neighbor| {
                            const neighbor_voxeme_coord = voxeme_coord + neighbor.coord_offset;
                            const neighbor_page_coord = convert.voxemeToPage(neighbor_voxeme_coord);

                            if (self.pages.getPtr(neighbor_page_coord)) |neighbor_page| {
                                const neighbor_page_is_solid = self.getMaterialProperties(neighbor_page.homogeneous.material_id).is_opaque;
                                if (neighbor_page.isHeterogeneous()) {
                                    // we can update our local visibility directly
                                    if (!neighbor_page_is_solid) {
                                        new_voxeme_vis.set(neighbor.axis, neighbor.positive, true);
                                        new_page_vis.set(neighbor.axis, neighbor.positive, true);
                                    }
                                } else {
                                    if (neighbor_page.heterogeneous.getPtr(neighbor_voxeme_coord)) |neighbor_voxeme| {
                                        if (neighbor_voxeme.is_homogeneous) {
                                            const neighbor_data = neighbor_voxeme.payload.homogeneous;
                                            const neighbor_is_solid = self.getMaterialProperties(neighbor_data.material_id).is_opaque;
                                            if (!neighbor_is_solid) {
                                                new_voxeme_vis.set(neighbor.axis, neighbor.positive, true);
                                                new_page_vis.set(neighbor.axis, neighbor.positive, true);
                                            }
                                        } else {
                                            const neighbor_data = neighbor_voxeme.asHeterogeneous();

                                            // we cannot update our local visibility directly;
                                            // we need to traverse the bordering face of the neighbor and check that they are completely solid across the volume face
                                            var border_it = BorderIterator.forVoxeme(neighbor.axis, !neighbor.positive);
                                            const reference_voxel_coord = convert.voxemeToVoxel(neighbor_voxeme_coord, .min);
                                            var face_is_solid = true;
                                            while (border_it.next()) |local_coord| {
                                                const index = convert.bufferToIndex(@intCast(reference_voxel_coord + local_coord));
                                                const neighbor_voxel = &neighbor_data[index];
                                                const neighbor_is_solid = self.getMaterialProperties(neighbor_voxel.material_id).is_opaque;
                                                if (!neighbor_is_solid) {
                                                    face_is_solid = false;
                                                }

                                                // update the neighbor voxel's visibility directly, if it isn't dirty itself
                                                if (!neighbor_voxeme.is_dirty) neighbor_voxel.visibility.set(neighbor.axis, !neighbor.positive, !local_is_solid);
                                            }

                                            if (!face_is_solid) {
                                                new_voxeme_vis.set(neighbor.axis, neighbor.positive, true);
                                                new_page_vis.set(neighbor.axis, neighbor.positive, true);
                                            }
                                        }
                                    } else if (!neighbor_page_is_solid) {
                                        // no neighboring voxeme does not necessarily mean empty space;
                                        // the neighbor page could have a solid as its homogeneous voxel base
                                        new_voxeme_vis.set(neighbor.axis, neighbor.positive, true);
                                        new_page_vis.set(neighbor.axis, neighbor.positive, true);
                                    }
                                }
                            } else {
                                // we can update our local visibility directly
                                new_voxeme_vis.set(neighbor.axis, neighbor.positive, true);
                                new_page_vis.set(neighbor.axis, neighbor.positive, true);
                            }

                            // if the neighbor isn't dirty itself, but does exist, we need to update its visibilities too
                            if (!self.dirty_pages.contains(neighbor_page_coord)) {
                                if (self.pages.getPtr(neighbor_page_coord)) |neighbor_page| {
                                    // queue neighbor page update if it hasn't been already
                                    const neighbor_page_gop = try border_pages.getOrPut(frame_arena, neighbor_page_coord);
                                    if (!neighbor_page_gop.found_existing) neighbor_page_gop.value_ptr.* = .none;

                                    // if we are not solid, mark the opposing face visible in the queued neighbor
                                    if (!local_is_solid) neighbor_page_gop.value_ptr.set(neighbor.axis, !neighbor.positive, true);

                                    // if there is also a neighboring voxeme, we need to ensure it is updated as well
                                    if (neighbor_page.heterogeneous.getPtr(neighbor_voxeme_coord)) |neighbor_voxeme| {
                                        // queue neighbor voxeme update it if hasn't been already
                                        const neighbor_voxeme_gop = try border_voxemes.getOrPut(frame_arena, neighbor_voxeme_coord);
                                        if (!neighbor_voxeme_gop.found_existing) neighbor_voxeme_gop.value_ptr.* = .none;

                                        // if we are not solid, mark the opposing face visible in the queued neighbor
                                        if (!local_is_solid) neighbor_voxeme_gop.value_ptr.set(neighbor.axis, !neighbor.positive, true);

                                        if (!neighbor_voxeme.is_homogeneous) {
                                            // if the neighbor is dense voxel data, we can update the adjacent border voxels directly
                                            const neighbor_data = neighbor_voxeme.asHeterogeneous();

                                            // update the specific voxel in the buffer, marking the opposing face as visible if we are not solid
                                            var border_it = BorderIterator.forVoxeme(neighbor.axis, !neighbor.positive);
                                            const reference_voxel_coord = convert.voxemeToVoxel(neighbor_voxeme_coord, .min);
                                            while (border_it.next()) |local_coord| {
                                                const index = convert.bufferToIndex(@intCast(reference_voxel_coord + local_coord));
                                                neighbor_data[index].visibility.set(neighbor.axis, !neighbor.positive, !local_is_solid);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        const data = voxeme.asHeterogeneous();

                        // recalculate visibility for all voxels in this voxeme
                        for (data, 0..) |*voxel, i| {
                            const local_coord = convert.indexToVoxel(voxeme_coord, i);
                            const local_is_solid = self.getMaterialProperties(voxel.material_id).is_opaque;

                            var new_voxel_vis: Visibility = .none;

                            comptime var neighbor_it = Neighbor.Iterator{};
                            inline while (comptime neighbor_it.next()) |neighbor| {
                                const neighbor_voxel_coord = local_coord + neighbor.coord_offset;
                                const neighbor_page_coord = convert.voxelToPage(neighbor_voxel_coord);
                                const neighbor_voxeme_coord = convert.voxelToVoxeme(neighbor_voxel_coord);

                                // we can update our local visibility directly
                                const neighbor_is_solid = self.getMaterialProperties(self.getVoxel(neighbor_voxel_coord).material_id).is_opaque;
                                if (!neighbor_is_solid) {
                                    new_voxel_vis.set(neighbor.axis, neighbor.positive, true);
                                    new_voxeme_vis.set(neighbor.axis, neighbor.positive, true);
                                    new_page_vis.set(neighbor.axis, neighbor.positive, true);
                                }

                                // if the neighbor isn't dirty itself, but does exist, we need to update its visibilities too
                                if (!self.dirty_pages.contains(neighbor_page_coord)) {
                                    if (self.pages.getPtr(neighbor_page_coord)) |neighbor_page| {
                                        // queue neighbor page update if it hasn't been already
                                        const neighbor_page_gop = try border_pages.getOrPut(frame_arena, neighbor_page_coord);
                                        if (!neighbor_page_gop.found_existing) neighbor_page_gop.value_ptr.* = .none;

                                        // if we are not solid, mark the opposing face visible in the queued neighbor
                                        if (!local_is_solid) neighbor_page_gop.value_ptr.set(neighbor.axis, !neighbor.positive, true);

                                        // if there is also a neighboring voxeme, we need to ensure it is updated as well
                                        if (neighbor_page.heterogeneous.getPtr(neighbor_voxeme_coord)) |neighbor_voxeme| {
                                            // queue neighbor voxeme update it if hasn't been already
                                            const neighbor_voxeme_gop = try border_voxemes.getOrPut(frame_arena, neighbor_voxeme_coord);
                                            if (!neighbor_voxeme_gop.found_existing) neighbor_voxeme_gop.value_ptr.* = .none;

                                            // if we are not solid, mark the opposing face visible in the queued neighbor
                                            if (!local_is_solid) neighbor_voxeme_gop.value_ptr.set(neighbor.axis, !neighbor.positive, true);

                                            if (!neighbor_voxeme.is_homogeneous) {
                                                // if the neighbor is dense voxel data, we can update the adjacent voxel directly
                                                const neighbor_data = neighbor_voxeme.asHeterogeneous();

                                                // update the specific voxel in the buffer, marking the opposing face as visible if we are not solid
                                                const index = convert.bufferToIndex(convert.voxelToBuffer(neighbor_voxel_coord));
                                                neighbor_data[index].visibility.set(neighbor.axis, !neighbor.positive, !local_is_solid);
                                            }
                                        }
                                    }
                                }
                            }

                            // update this voxel's visibility directly
                            voxel.visibility = new_voxel_vis;
                        }
                    }

                    // update this voxeme's visibility directly
                    voxeme.visibility = new_voxeme_vis;
                }
            }

            // update this page's visibility directly
            page.visibility = new_page_vis;
        }
    }

    var border_voxeme_it = border_voxemes.iterator();
    while (border_voxeme_it.next()) |entry| {
        const voxeme_coord = entry.key_ptr.*;
        const new_vis = entry.value_ptr.*;

        const page = self.pages.getPtr(convert.voxemeToPage(voxeme_coord)) orelse unreachable; // shouldnt have been put in the map if it didnt exist
        const voxeme = page.heterogeneous.getPtr(voxeme_coord) orelse unreachable; // same as above

        voxeme.visibility = new_vis;
    }

    var border_page_it = border_pages.iterator();
    while (border_page_it.next()) |entry| {
        const page_coord = entry.key_ptr.*;
        const new_vis = entry.value_ptr.*;

        const page = self.pages.getPtr(page_coord) orelse unreachable; // shouldnt have been put in the map if it didnt exist
        page.visibility = new_vis;
    }
}

/// Loop all dirty Pages and homogenize and prune any that can be.
pub fn homogenizeAndPrune(self: *Grid, frame_arena: std.mem.Allocator, pages_pruned: *std.AutoHashMapUnmanaged(PageCoord, void), voxemes_pruned: *std.AutoHashMapUnmanaged(VoxemeCoord, void)) !void {
    var pages_to_prune = try std.ArrayList(PageCoord).initCapacity(frame_arena, self.dirty_pages.count());
    defer pages_to_prune.deinit(frame_arena);

    var page_it = self.dirty_pages.keyIterator();
    page_loop: while (page_it.next()) |page_entry| {
        const page_coord = page_entry.*;
        const page = self.pages.getPtr(page_coord) orelse unreachable; // pages are never removed from the live set until after this loop

        if (!page.isHeterogeneous()) {
            // simple case: check if the page is empty
            if (page.homogeneous.material_id == .none) {
                try pages_to_prune.append(frame_arena, page_coord);
            }

            continue :page_loop;
        }

        var voxemes_to_prune = try std.ArrayList(VoxemeCoord).initCapacity(frame_arena, page.heterogeneous.count());
        defer voxemes_to_prune.deinit(frame_arena);

        var voxeme_it = page.heterogeneous.iterator();
        while (voxeme_it.next()) |voxeme_entry| {
            const voxeme_coord = voxeme_entry.key_ptr.*;
            const voxeme = voxeme_entry.value_ptr;

            var can_absorb = true;

            if (voxeme.is_homogeneous) {
                const homogeneous_data = voxeme.payload.homogeneous;

                if (homogeneous_data.eql(page.homogeneous)) {
                    // This voxeme is redundant, mark for pruning
                    try voxemes_to_prune.append(frame_arena, voxeme_coord);
                }
            } else if (voxeme.is_dirty) {
                const heterogeneous_data = voxeme.asHeterogeneous();

                // Check if all voxels in the buffer are the same
                const first_voxel = heterogeneous_data[0];
                var all_same = true;

                check_loop: for (heterogeneous_data) |*voxel| {
                    if (!voxel.eql(page.homogeneous)) {
                        can_absorb = false; // at least one voxel is different from the page's homogeneous voxel
                    }

                    if (!voxel.eql(first_voxel)) {
                        all_same = false;
                        break :check_loop;
                    }
                }

                if (can_absorb) {
                    // This voxeme could be homogenized, but it is redundant; mark for pruning
                    try voxemes_to_prune.append(frame_arena, voxeme_coord);
                } else if (all_same) {
                    // free the old buffer
                    self.deallocateBuffer(heterogeneous_data);

                    // We can convert this voxeme to homogeneous
                    voxeme.* = Voxeme.homogeneous(first_voxel);
                    voxeme.is_dirty = true; // re-mark dirty so that it can be recalculated
                }
            }
        }

        for (voxemes_to_prune.items) |voxeme_coord| {
            const voxeme = page.heterogeneous.getPtr(voxeme_coord) orelse unreachable;

            if (!voxeme.is_homogeneous) {
                // Free the buffer back to the pool
                const buffer = voxeme.asHeterogeneous();
                self.deallocateBuffer(buffer);
            }

            // Remove the voxeme from the page
            _ = page.heterogeneous.remove(voxeme_coord);

            // report the pruning
            try voxemes_pruned.put(frame_arena, voxeme_coord, {});
        }

        // If the page is now empty, cleanup
        if (page.heterogeneous.count() == 0) {
            // remove heterogeneous state
            page.heterogeneous.deinit(self.allocator());
            page.heterogeneous = .empty;

            // if the page's homogeneous voxel data is empty, we can remove the page entirely
            if (page.homogeneous.material_id == .none) {
                try pages_to_prune.append(frame_arena, page_coord);
            }
        }
    }

    for (pages_to_prune.items) |page_coord| {
        const page = self.pages.getPtr(page_coord) orelse unreachable;

        // Heterogeneous pages should have been cleaned up already
        std.debug.assert(!page.isHeterogeneous());

        // Remove the page from the grid
        _ = self.pages.remove(page_coord);

        // Also remove from dirty pages
        _ = self.dirty_pages.remove(page_coord);

        // report the pruning
        try pages_pruned.put(frame_arena, page_coord, {});
    }
}

/// Get the Voxel at the given voxel-space coordinate, if it exists.
pub fn getVoxel(self: *const Grid, coord: VoxelCoord) Voxel {
    const page_coord = convert.voxelToPage(coord);
    const voxeme_coord = convert.voxelToVoxeme(coord);
    const local_voxel_coord = convert.voxelToBuffer(coord);

    const page = self.pages.get(page_coord) orelse return .empty;
    if (!page.isHeterogeneous()) {
        return page.homogeneous;
    }

    const voxeme = page.heterogeneous.get(voxeme_coord) orelse return page.homogeneous;
    if (voxeme.is_homogeneous) {
        return voxeme.payload.homogeneous;
    } else {
        const buffer = voxeme.asHeterogeneous();
        const index = convert.bufferToIndex(local_voxel_coord);
        return buffer[index];
    }
}

fn markDirty(self: *Grid, page_coord: PageCoord) !void {
    try self.dirty_pages.put(self.allocator(), page_coord, {});
}

/// Delete the Voxel at the given voxel-space coordinate.
pub fn delVoxel(self: *Grid, coord: VoxelCoord) !void {
    if (!self.isVoxelMapped(coord)) return;

    const page_coord = convert.voxelToPage(coord);
    const voxeme_coord = convert.voxelToVoxeme(coord);

    const local_voxel_coord = convert.voxelToBuffer(coord);
    const index = convert.bufferToIndex(local_voxel_coord);

    const page = self.pages.getPtr(page_coord) orelse unreachable;
    if (!page.isHeterogeneous()) {
        const old_voxel = page.homogeneous;
        std.debug.assert(old_voxel.material_id != .none); // sanity check: shouldn't be any empty pages

        const new_buffer = try self.allocateBuffer();

        // Initialize the new buffer to old_voxel
        @memset(new_buffer, old_voxel);

        // Set the specific voxel to empty
        new_buffer[index].set(.empty);

        // Insert the new heterogeneous voxeme
        try page.heterogeneous.put(self.allocator(), voxeme_coord, Voxeme.heterogeneous(new_buffer));
    } else {
        const voxeme = page.heterogeneous.getPtr(voxeme_coord) orelse unreachable;

        if (voxeme.is_homogeneous) {
            // We need to recreate the voxeme as a heterogeneous one without this voxel
            const old_voxel = voxeme.payload.homogeneous;
            std.debug.assert(old_voxel.material_id != .none); // sanity check: shouldn't be any empty voxemes

            const new_buffer = try self.allocateBuffer();

            // Initialize the new buffer to old_voxel
            @memset(new_buffer, old_voxel);

            // Set the specific voxel to empty
            new_buffer[index].set(.empty);

            // Replace the voxeme in the page with a dirty heterogeneous one
            voxeme.* = Voxeme.heterogeneous(new_buffer);
        } else {
            // we can just set the voxel to empty in the existing buffer
            const buffer = voxeme.asHeterogeneous();
            if (buffer[index].material_id == .none) {
                return; // No change needed
            }

            buffer[index].set(.empty);

            // Mark dirty so that it can be checked for homogenization later
            voxeme.is_dirty = true;
        }
    }

    // if we made it here, something in the page was changed
    try self.markDirty(page_coord);
}

/// Set the Voxel at the given voxel-space coordinate.
pub fn setVoxel(self: *Grid, coord: VoxelCoord, value: LiteVoxel) !void {
    if (value.material_id == .none) {
        return self.delVoxel(coord);
    }

    const page_coord = convert.voxelToPage(coord);
    const voxeme_coord = convert.voxelToVoxeme(coord);

    const page_gop = try self.pages.getOrPut(self.allocator(), page_coord);
    if (!page_gop.found_existing) page_gop.value_ptr.* = .empty;

    const page = page_gop.value_ptr;
    var was_modified = false;

    if (!page.isHeterogeneous()) {
        const old_voxel = page.homogeneous;
        if (value.eql(old_voxel)) {
            return; // No change needed
        }

        const new_buffer = try self.allocateBuffer();

        // Initialize the new buffer to old_voxel
        @memset(new_buffer, old_voxel);

        // Set the specific voxel
        const local_voxel_coord = convert.voxelToBuffer(coord);
        const index = convert.bufferToIndex(local_voxel_coord);
        new_buffer[index].set(value);

        // Insert the new heterogeneous voxeme
        try page.heterogeneous.put(self.allocator(), voxeme_coord, Voxeme.heterogeneous(new_buffer));
        was_modified = true;
    } else {
        const voxeme_gop = try page.heterogeneous.getOrPut(self.allocator(), voxeme_coord);

        if (!voxeme_gop.found_existing) {
            // This space was previously implicitly filled with page.homogeneous
            const old_voxel = page.homogeneous;
            if (value.eql(old_voxel)) return;

            const new_buffer = try self.allocateBuffer();
            // Initialize the new buffer to the page's homogeneous material
            @memset(new_buffer, old_voxel);

            // Set the voxeme to a heterogeneous one with the new buffer and dirty flag set
            voxeme_gop.value_ptr.* = Voxeme.heterogeneous(new_buffer);

            // Now we can set the specific voxel
            const local_voxel_coord = convert.voxelToBuffer(coord);
            const index = convert.bufferToIndex(local_voxel_coord);
            new_buffer[index].set(value);
            was_modified = true;
        } else {
            const voxeme = voxeme_gop.value_ptr;

            if (voxeme.is_homogeneous) {
                const old_voxel = voxeme.payload.homogeneous;
                if (value.eql(old_voxel)) {
                    return; // No change needed
                }

                // We need to recreate the voxeme as a heterogeneous one
                const new_buffer = try self.allocateBuffer();

                // Initialize the new buffer to old_voxel
                @memset(new_buffer, old_voxel);

                // Set the specific voxel to the new value
                const local_voxel_coord = convert.voxelToBuffer(coord);
                const index = convert.bufferToIndex(local_voxel_coord);
                new_buffer[index].set(value);

                // Replace the voxeme in the page with a dirty heterogeneous one
                voxeme.* = Voxeme.heterogeneous(new_buffer);
                was_modified = true;
            } else {
                // we can just set the voxel in the existing buffer
                const buffer = voxeme.asHeterogeneous();

                const local_voxel_coord = convert.voxelToBuffer(coord);
                const index = convert.bufferToIndex(local_voxel_coord);

                if (buffer[index].eql(value)) {
                    return; // No change needed
                }

                buffer[index].set(value);

                // Mark dirty so that it can be checked for homogenization later
                voxeme.is_dirty = true;
                was_modified = true;
            }
        }
    }

    if (was_modified) {
        try self.markDirty(page_coord);
    }
}

// --- Meshing Helpers ---

const VoxelGetter = struct {
    buffer: ?Buffer,
    homogeneous_voxel: Voxel,

    fn get(self: @This(), x: u32, y: u32, z: u32) Voxel {
        if (self.buffer) |b| {
            return b[convert.bufferToIndex(.{ x, y, z })];
        } else {
            return self.homogeneous_voxel;
        }
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
    vertices: *std.ArrayList(Vertex),
    indices: *std.ArrayList(u32),
    axis: Axis,
    positive: bool,
    origin: Position,
    scale: f32,
    color: vec3,
) !void {
    log.debug("      adding face: axis={}, positive={}, origin={}, scale={}", .{ axis, positive, origin, scale });

    const face_index = @intFromEnum(axis) + if (positive) @as(usize, 0) else 1;

    const base_vertex_index: u32 = @intCast(vertices.items.len);
    const data = &face_data[face_index];

    for (data.vertices, data.uvs) |v_offset, uv| {
        const pos = origin + v_offset * @as(vec3, @splat(scale));
        try vertices.append(gpa, Vertex.init(pos, data.normal, uv, color));
    }

    try indices.appendSlice(gpa, &.{
        base_vertex_index + 0, base_vertex_index + 1, base_vertex_index + 2,
        base_vertex_index + 0, base_vertex_index + 2, base_vertex_index + 3,
    });
}

/// Generates a simple, blocky, non-greedy mesh for a single Voxeme.
pub fn voxemeMeshBasic(self: *const Grid, gpa: std.mem.Allocator, page: *const Page, voxeme_coord: VoxemeCoord, vertices: *std.ArrayList(Vertex), indices: *std.ArrayList(u32)) !void {
    std.debug.assert(page.isHeterogeneous());

    if (page.heterogeneous.getPtr(voxeme_coord)) |voxeme| {
        log.debug("Meshing voxeme at {d}", .{voxeme_coord});

        if (voxeme.is_homogeneous) {
            log.debug("  - simple homogeneous voxel data", .{});

            // large-volume voxel
            const data = voxeme.payload.homogeneous;
            if (data.material_id == .none) return;

            const material = self.getMaterialProperties(data.material_id);

            const local_voxeme_origin = convert.voxemeToVoxel(voxeme_coord, .min);
            const world_voxeme_origin = convert.voxelToWorld(local_voxeme_origin, .min);
            const color = material.color;

            comptime var neighbor_it = Neighbor.Iterator{};
            inline while (comptime neighbor_it.next()) |neighbor| {
                const visible_face = data.visibility.get(neighbor.axis, neighbor.positive);

                if (visible_face) {
                    log.debug("    - adding face for voxeme {d}", .{voxeme_coord});
                    try addFace(
                        gpa,
                        vertices,
                        indices,
                        neighbor.axis,
                        neighbor.positive,
                        world_voxeme_origin,
                        voxeme_scale,
                        color,
                    );
                } else {
                    log.debug("    - skipping face for voxeme {d}: axis={}, positive={}", .{ voxeme_coord, neighbor.axis, neighbor.positive });
                }
            }
        } else {
            log.debug("  - dense voxel data", .{});

            // mesh the dense voxel data
            const buffer = voxeme.asHeterogeneous();

            for (buffer, 0..) |voxel, index| {
                if (voxel.material_id == .none) continue;

                const material = self.getMaterialProperties(voxel.material_id);

                const voxel_coord = convert.indexToVoxel(voxeme_coord, index);
                const world_voxel_coord = convert.voxelToWorld(voxel_coord, .min);
                const color = material.color;

                comptime var neighbor_it = Neighbor.Iterator{};
                inline while (comptime neighbor_it.next()) |neighbor| {
                    const visible_face = voxel.visibility.get(neighbor.axis, neighbor.positive);

                    if (visible_face) {
                        log.debug("    - adding face for voxel {d}", .{voxel_coord});
                        try addFace(
                            gpa,
                            vertices,
                            indices,
                            neighbor.axis,
                            neighbor.positive,
                            world_voxel_coord,
                            voxel_scale,
                            color,
                        );
                    } else {
                        log.debug("    - skipping face for voxel {d}: axis={}, positive={}", .{ voxel_coord, neighbor.axis, neighbor.positive });
                    }
                }
            }
        }
    } else {
        std.debug.panic("voxemeMeshBasic called on non-existent voxeme\n", .{});
    }
}

/// Generates a simple, blocky, non-greedy mesh for an entire Page.
pub fn pageMeshBasic(self: *const Grid, gpa: std.mem.Allocator, page_coord: PageCoord, vertices: *std.ArrayList(Vertex), indices: *std.ArrayList(u32)) !void {
    const page = self.pages.getPtr(page_coord) orelse return;
    if (!page.isHeterogeneous()) {
        // large-volume voxel
        const data = page.homogeneous;
        if (data.material_id == .none) return;

        const material = self.getMaterialProperties(data.material_id);

        const local_page_origin = convert.pageToVoxel(page_coord, .min, .min);
        const world_page_origin = convert.voxelToWorld(local_page_origin, .min);
        const color = material.color;

        comptime var neighbor_it = Neighbor.Iterator{};
        inline while (comptime neighbor_it.next()) |neighbor| {
            const visible_face = data.visibility.get(neighbor.axis, neighbor.positive);
            if (visible_face) {
                try addFace(
                    gpa,
                    vertices,
                    indices,
                    neighbor.axis,
                    neighbor.positive,
                    world_page_origin,
                    page_scale,
                    color,
                );
            }
        }
    } else {
        // mesh all live voxemes in the page
        var live_voxeme_it = page.heterogeneous.keyIterator();
        while (live_voxeme_it.next()) |voxeme_coord_ptr| {
            try self.voxemeMeshBasic(gpa, page, voxeme_coord_ptr.*, vertices, indices);
        }

        // mesh implicit voxemes if this is page has non-empty homogeneous data
        if (page.homogeneous.material_id != .none) {
            const material = self.getMaterialProperties(page.homogeneous.material_id);
            const color = material.color;

            var implicit_voxeme_it = VolumeIterator.forPage();
            while (implicit_voxeme_it.next()) |voxeme_coord| {
                if (page.heterogeneous.contains(voxeme_coord)) continue; // already meshed

                const local_voxeme_origin = convert.voxemeToVoxel(voxeme_coord, .min);
                const world_voxeme_origin = convert.voxelToWorld(local_voxeme_origin, .min);

                comptime var neighbor_it = Neighbor.Iterator{};
                inline while (comptime neighbor_it.next()) |neighbor| {
                    // hmmm we don't have per-voxeme visibility data here...we'll have to look it up from the coordinates
                    const neighbor_voxeme_coord = voxeme_coord + neighbor.coord_offset;

                    const neighbor_page_coord = convert.voxemeToPage(neighbor_voxeme_coord);

                    if (@reduce(.And, neighbor_page_coord == page_coord)) {
                        // neighbor is in the same page, so we can try to get visibility if it is not also implicit
                        if (page.heterogeneous.get(neighbor_voxeme_coord)) |neighbor_voxeme| {
                            if (neighbor_voxeme.is_homogeneous) {
                                const neighbor_is_solid = self.getMaterialProperties(neighbor_voxeme.payload.homogeneous.material_id).is_opaque;
                                if (!neighbor_is_solid) {
                                    try addFace(
                                        gpa,
                                        vertices,
                                        indices,
                                        neighbor.axis,
                                        neighbor.positive,
                                        world_voxeme_origin,
                                        voxeme_scale,
                                        color,
                                    );
                                } else {
                                    // neighbor is solid, so face is not visible
                                }
                            } else {
                                // we need to do the full visibility check here...at least we don't have to update the neighbor, so we can early-exit
                                // precaching the visibility data for implicit voxemes in the update would be better
                                const neighbor_data = neighbor_voxeme.asHeterogeneous();
                                var face_is_solid = true;
                                var border_it = BorderIterator.forVoxeme(neighbor.axis, neighbor.positive);
                                const reference_voxel_coord = convert.voxemeToVoxel(neighbor_voxeme_coord, .min);
                                while (border_it.next()) |local_coord| {
                                    const index = convert.bufferToIndex(@intCast(reference_voxel_coord + local_coord));
                                    const neighbor_voxel = &neighbor_data[index];
                                    const neighbor_is_solid = self.getMaterialProperties(neighbor_voxel.material_id).is_opaque;
                                    if (!neighbor_is_solid) {
                                        face_is_solid = false;
                                        break;
                                    }
                                }

                                if (!face_is_solid) {
                                    try addFace(
                                        gpa,
                                        vertices,
                                        indices,
                                        neighbor.axis,
                                        neighbor.positive,
                                        world_voxeme_origin,
                                        voxeme_scale,
                                        color,
                                    );
                                } else {
                                    // neighbor is solid, so face is not visible
                                }
                            }
                        } else {
                            // its implicit, so it has the same visibility as us, no face needed
                        }
                    } else if (page.homogeneous.visibility.get(neighbor.axis, neighbor.positive)) { // if our whole page face is not visible, and we were processing a border implicit voxeme, we can early-out
                        if (self.pages.getPtr(neighbor_page_coord)) |neighbor_page| {
                            if (!neighbor_page.isHeterogeneous()) {
                                const neighbor_is_solid = self.getMaterialProperties(neighbor_page.homogeneous.material_id).is_opaque;

                                if (!neighbor_is_solid) {
                                    try addFace(
                                        gpa,
                                        vertices,
                                        indices,
                                        neighbor.axis,
                                        neighbor.positive,
                                        world_voxeme_origin,
                                        voxeme_scale,
                                        color,
                                    );
                                } else {
                                    // neighbor is solid, so face is not visible
                                }
                            } else {
                                // neighbor is heterogeneous, so we need to go down a level if possible
                                if (neighbor_page.heterogeneous.get(neighbor_voxeme_coord)) |neighbor_voxeme| {
                                    if (neighbor_voxeme.is_homogeneous) {
                                        // we can just test the material
                                        const neighbor_is_solid = self.getMaterialProperties(neighbor_voxeme.payload.homogeneous.material_id).is_opaque;
                                        if (!neighbor_is_solid) {
                                            try addFace(
                                                gpa,
                                                vertices,
                                                indices,
                                                neighbor.axis,
                                                neighbor.positive,
                                                world_voxeme_origin,
                                                voxeme_scale,
                                                color,
                                            );
                                        } else {
                                            // neighbor is solid, so face is not visible
                                        }
                                    } else {
                                        // we must do the full visibility check here...at least we don't have to update the neighbor, so we can early-exit

                                        const neighbor_data = neighbor_voxeme.asHeterogeneous();
                                        var face_is_solid = true;
                                        var border_it = BorderIterator.forVoxeme(neighbor.axis, neighbor.positive);
                                        const reference_voxel_coord = convert.voxemeToVoxel(neighbor_voxeme_coord, .min);
                                        while (border_it.next()) |local_coord| {
                                            const index = convert.bufferToIndex(@intCast(reference_voxel_coord + local_coord));
                                            const neighbor_voxel = &neighbor_data[index];
                                            const neighbor_is_solid = self.getMaterialProperties(neighbor_voxel.material_id).is_opaque;
                                            if (!neighbor_is_solid) {
                                                face_is_solid = false;
                                                break;
                                            }
                                        }

                                        if (!face_is_solid) {
                                            try addFace(
                                                gpa,
                                                vertices,
                                                indices,
                                                neighbor.axis,
                                                neighbor.positive,
                                                world_voxeme_origin,
                                                voxeme_scale,
                                                color,
                                            );
                                        } else {
                                            // neighbor is solid, so face is not visible
                                        }
                                    }
                                } else {
                                    // check against the neighbor page's homogeneous data
                                    const neighbor_is_solid = self.getMaterialProperties(neighbor_page.homogeneous.material_id).is_opaque;
                                    if (!neighbor_is_solid) {
                                        try addFace(
                                            gpa,
                                            vertices,
                                            indices,
                                            neighbor.axis,
                                            neighbor.positive,
                                            world_voxeme_origin,
                                            voxeme_scale,
                                            color,
                                        );
                                    } else {
                                        // neighbor is solid, so face is not visible
                                    }
                                }
                            }
                        } else {
                            // no neighbor page, so face is visible
                            try addFace(
                                gpa,
                                vertices,
                                indices,
                                neighbor.axis,
                                neighbor.positive,
                                world_voxeme_origin,
                                voxeme_scale,
                                color,
                            );
                        }
                    }
                }
            }
        }
    }
}

/// Generates a mesh for all pages within the rectangular volume defined by start_coord and end_coord (inclusive).
pub fn worldMeshBasic(
    self: *const Grid,
    gpa: std.mem.Allocator,
    start_coord: PageCoord,
    end_coord: PageCoord,
    vertices: *std.ArrayList(Vertex),
    indices: *std.ArrayList(u32),
) !void {
    // Determine the iteration bounds, ensuring we handle cases where start > end.
    const min_x = @min(start_coord[0], end_coord[0]);
    const max_x = @max(start_coord[0], end_coord[0]);
    const min_y = @min(start_coord[1], end_coord[1]);
    const max_y = @max(start_coord[1], end_coord[1]);
    const min_z = @min(start_coord[2], end_coord[2]);
    const max_z = @max(start_coord[2], end_coord[2]);

    var z = min_z;
    while (z <= max_z) : (z += 1) {
        var y = min_y;
        while (y <= max_y) : (y += 1) {
            var x = min_x;
            while (x <= max_x) : (x += 1) {
                const current_page_coord = PageCoord{ x, y, z };
                try self.pageMeshBasic(gpa, current_page_coord, vertices, indices);
            }
        }
    }
}
