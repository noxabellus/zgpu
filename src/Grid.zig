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
    is_dirty: bool,
    is_heterogeneous: bool,
    homogeneous: Voxel,
    heterogeneous: std.AutoHashMapUnmanaged(VoxemeCoord, Voxeme) = .empty,

    pub const empty = Page{ .is_dirty = false, .is_heterogeneous = false, .homogeneous = .empty };

    pub fn deinit(self: *Page, gpa: std.mem.Allocator) void {
        if (self.is_heterogeneous) {
            self.heterogeneous.deinit(gpa);
        }
        // debug safety: prevent use after free
        self.* = undefined;
    }
};

/// A Voxeme is a small, dense 3D grid of Voxels.
/// This is represented in our system as a tagged pointer, allowing homogenous Voxemes to avoid an allocation.
pub const Voxeme = packed struct(u64) {
    is_homogeneous: bool,
    is_dirty: bool,
    _reserved: u14 = 0,
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
            .payload = .{ .homogeneous = voxel },
        };
    }

    /// Create a heterogeneous Voxeme containing the given pointer to an array of Voxels.
    pub fn heterogeneous(voxels: Buffer) Voxeme {
        return Voxeme{
            .is_homogeneous = false,
            .is_dirty = true,
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

    pub const Axis = enum(u3) {
        x = 0,
        y = 2,
        z = 4,
    };

    /// Gets the visibility state for a face by axis and direction.
    pub fn get(self: Visibility, axis: Axis, positive: bool) bool {
        const bits: u6 = @bitCast(self);
        const axis_offset: u3 = @intFromEnum(axis);
        const bit_index: u3 = if (positive) axis_offset else axis_offset + 1;
        const mask = @as(u6, 1) << bit_index;
        return (bits & mask) != 0;
    }

    /// Sets the visibility state for a face by axis and direction.
    pub fn set(self: *align(4:10:4) Visibility, axis: Axis, positive: bool, value: bool) void {
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

    pub const neighbor_coordinates = [_]VoxelCoord{
        .{ 1, 0, 0 }, // pos_x
        .{ -1, 0, 0 }, // neg_x
        .{ 0, 1, 0 }, // pos_y
        .{ 0, -1, 0 }, // neg_y
        .{ 0, 0, 1 }, // pos_z
        .{ 0, 0, -1 }, // neg_z
    };

    pub const Neighbor = struct {
        axis: Axis,
        positive: bool,
        coord_offset: VoxelCoord,
    };

    pub const NeighborIterator = struct {
        index: usize = 0,

        pub fn next(self: *NeighborIterator) ?Neighbor {
            if (self.index >= neighbor_coordinates.len) return null;
            const axis = switch (self.index) {
                0, 1 => Axis.x,
                2, 3 => Axis.y,
                4, 5 => Axis.z,
                else => unreachable,
            };
            const positive = (self.index % 2) == 0;
            const coord_offset = neighbor_coordinates[self.index];
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
    pub fn set(self: *Voxel, Lite: LiteVoxel) void {
        self.material_id = Lite.material_id;
        self.state = Lite.state;
    }

    /// Create a Voxel from a LiteVoxel, with visibility set to none.
    pub fn fromLite(Lite: LiteVoxel) Voxel {
        return Voxel{ .material_id = Lite.material_id, .visibility = .none, .state = Lite.state };
    }

    /// Create a LiteVoxel from this Voxel, ignoring visibility.
    pub fn toLite(self: Voxel) LiteVoxel {
        return LiteVoxel{ .material_id = self.material_id, .state = self.state };
    }
};

// --- Meshing Helpers ---

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

/// Utility for mesh building/general voxel polling that can read from either a buffer or a homogeneous voxel.
pub const VoxelGetter = struct {
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
    origin: Position,
    scale: f32,
    face_index: usize,
    color: vec3,
) !void {
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

/// The memory pool used to allocate heterogeneous Buffers.
pool: std.heap.MemoryPool(BufferData),

/// A sparse map of all existing Pages in the Grid, keyed by their PageCoord.
pages: std.AutoHashMapUnmanaged(PageCoord, Page) = .empty,

/// A map of all material properties in this Grid, keyed by their MaterialId.
materials: std.AutoHashMapUnmanaged(MaterialId, MaterialProperties) = .empty,

/// Create a new empty Grid with a preheated pool for Buffers.
pub fn init(backing_allocator: std.mem.Allocator) !Grid {
    var out = Grid{
        .pool = try .initPreheated(backing_allocator, 4096),
    };

    try out.pages.ensureTotalCapacity(backing_allocator, 4096);
    try out.materials.ensureTotalCapacity(backing_allocator, std.math.maxInt(std.meta.Tag(MaterialId)));

    out.materials.putAssumeCapacity(.none, .none);

    return out;
}

/// Deinitialize the Grid, freeing all allocated memory.
pub fn deinit(self: *Grid) void {
    const gpa = self.allocator();

    // Deinitialize all Pages
    var it = self.pages.valueIterator();
    while (it.next()) |page| page.deinit(gpa);

    // Free the Page hashmap
    self.pages.deinit(gpa);

    // Free the MaterialProperties hashmap
    self.materials.deinit(gpa);

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

/// Determine if a given voxeme-space coordinate is mapped.
pub fn isVoxemeMapped(self: *const Grid, coord: VoxemeCoord) bool {
    const page_coord = convert.voxemeToPage(coord);

    const page = self.pages.getPtr(page_coord) orelse return false;
    return page.is_heterogeneous and page.heterogeneous.contains(coord);
}

/// Determine if a given page-space coordinate is mapped.
pub fn isPageMapped(self: *const Grid, coord: PageCoord) bool {
    return self.pages.contains(coord);
}

/// Loop all Pages and for each dirty Page, precalculates Voxel visibility, then homogenizes and prunes extraneous data structures.
/// Loop all Pages and for each dirty Page, precalculates Voxel visibility, then homogenizes and prunes extraneous data structures.
pub fn update(self: *Grid, frame_arena: std.mem.Allocator) !void {
    const gpa = self.allocator();

    // --- Pass 1: Collect coordinates of all dirty pages ---
    var dirty_pages = std.ArrayList(PageCoord).empty;
    defer dirty_pages.deinit(frame_arena);

    var page_key_it = self.pages.keyIterator();
    while (page_key_it.next()) |page_coord_ptr| {
        if (self.pages.get(page_coord_ptr.*).?.is_dirty) {
            try dirty_pages.append(frame_arena, page_coord_ptr.*);
        }
    }

    // If no pages are dirty, we're done.
    if (dirty_pages.items.len == 0) return;

    // --- Pass 2: Process the collected dirty pages ---
    var pages_to_remove = std.ArrayList(PageCoord).empty;
    defer pages_to_remove.deinit(frame_arena);
    var voxemes_to_remove = std.ArrayList(VoxemeCoord).empty;
    defer voxemes_to_remove.deinit(frame_arena);

    for (dirty_pages.items) |page_coord| {
        // The page must exist, as we just found it.
        const page = self.pages.getPtr(page_coord).?;

        // --- Stage A: Pre-calculate Visibility ---
        // --- Step 1: Pre-calculate Visibility for the entire dirty page ---
        // This must be done before homogenization, as homogenization might deallocate the very buffers we need to read from.
        if (page.is_heterogeneous) {
            // Iterate Sparsely: only process voxemes that actually exist in the page.
            var voxeme_it = page.heterogeneous.iterator();
            while (voxeme_it.next()) |voxeme_entry| {
                const voxeme_coord = voxeme_entry.key_ptr.*;
                const voxeme = voxeme_entry.value_ptr.*;

                // Visibility is only calculated for individual voxels, which are only in heterogeneous voxemes.
                if (voxeme.is_homogeneous) continue;

                const buffer = voxeme.asHeterogeneous();
                const start_voxel_coord = convert.voxemeToVoxel(voxeme_coord, .min);

                // Iterate Densely: loop through all voxels within this specific voxeme.
                var z: u32 = 0;
                while (z < voxeme_length) : (z += 1) {
                    var y: u32 = 0;
                    while (y < voxeme_length) : (y += 1) {
                        var x: u32 = 0;
                        while (x < voxeme_length) : (x += 1) {
                            const local_u_coord = BufferCoord{ x, y, z };
                            const current_coord = start_voxel_coord + VoxelCoord{ @intCast(x), @intCast(y), @intCast(z) };

                            const index = convert.bufferToIndex(local_u_coord);
                            const voxel_ptr = &buffer[index];

                            if (!self.getMaterialProperties(voxel_ptr.material_id).is_opaque) {
                                voxel_ptr.visibility = .all;
                                continue;
                            }

                            comptime var vis_iterator = Visibility.NeighborIterator{};
                            inline while (comptime vis_iterator.next()) |info| {
                                const neighbor_voxel = self.getVoxel(current_coord + info.coord_offset);
                                if (!self.getMaterialProperties(neighbor_voxel.material_id).is_opaque) {
                                    voxel_ptr.visibility.set(info.axis, info.positive, true);
                                } else {
                                    voxel_ptr.visibility.set(info.axis, info.positive, false);
                                }
                            }
                        }
                    }
                }
            }
        }

        // --- Stage B: Homogenize ---
        page.is_dirty = false;
        if (!page.is_heterogeneous) {
            if (page.homogeneous.material_id == .none) {
                // It's possible for a page to be dirty and homogeneous if it was just
                // created via setVoxel in an empty region and needs to be cleaned up.
                try pages_to_remove.append(frame_arena, page_coord);
            }
            continue;
        }

        voxemes_to_remove.clearRetainingCapacity();
        var voxeme_it = page.heterogeneous.iterator();
        while (voxeme_it.next()) |voxeme_entry| {
            const voxeme_coord = voxeme_entry.key_ptr.*;
            const voxeme = voxeme_entry.value_ptr;

            if (!voxeme.is_dirty) continue;
            voxeme.is_dirty = false;
            if (voxeme.is_homogeneous) continue;

            const buffer = voxeme.asHeterogeneous();
            const reference_voxel = buffer[0];
            var is_now_homogeneous = true;
            for (buffer[1..]) |v| {
                if (!v.eql(reference_voxel)) {
                    is_now_homogeneous = false;
                    break;
                }
            }

            if (is_now_homogeneous) {
                if (reference_voxel.material_id == .none) {
                    self.deallocateBuffer(buffer);
                    try voxemes_to_remove.append(frame_arena, voxeme_coord);
                } else {
                    voxeme.* = Voxeme.homogeneous(reference_voxel);
                    self.deallocateBuffer(buffer);
                }
            }
        }

        for (voxemes_to_remove.items) |voxeme_coord| {
            _ = page.heterogeneous.remove(voxeme_coord);
        }

        voxemes_to_remove.clearRetainingCapacity();
        var prune_it = page.heterogeneous.iterator();
        while (prune_it.next()) |entry| {
            const voxeme = entry.value_ptr.*;
            if (voxeme.is_homogeneous and voxeme.payload.homogeneous.eql(page.homogeneous)) {
                try voxemes_to_remove.append(frame_arena, entry.key_ptr.*);
            }
        }
        for (voxemes_to_remove.items) |coord| {
            _ = page.heterogeneous.remove(coord);
        }

        if (page.heterogeneous.count() == 0) {
            page.heterogeneous.deinit(gpa);
            page.is_heterogeneous = false;
            if (page.homogeneous.material_id == .none) {
                try pages_to_remove.append(frame_arena, page_coord);
            }
        } else if (page.heterogeneous.count() == voxemes_per_page) {
            var first_voxel: ?Voxel = null;
            var all_same = true;
            var check_it = page.heterogeneous.valueIterator();
            while (check_it.next()) |voxeme| {
                if (!voxeme.is_homogeneous) {
                    all_same = false;
                    break;
                }
                const current_voxel = voxeme.payload.homogeneous;
                if (first_voxel) |fv| {
                    if (!current_voxel.eql(fv)) {
                        all_same = false;
                        break;
                    }
                } else {
                    first_voxel = current_voxel;
                }
            }

            if (all_same) {
                if (first_voxel) |new_homogeneous_voxel| {
                    if (new_homogeneous_voxel.material_id != .none) {
                        page.heterogeneous.deinit(gpa);
                        page.is_heterogeneous = false;
                        page.homogeneous = new_homogeneous_voxel;
                    }
                }
            }
        }
    }

    // --- Pass 3: Prune any pages that became empty ---
    for (pages_to_remove.items) |page_coord_to_remove| {
        if (self.pages.fetchRemove(page_coord_to_remove)) |removed_entry| {
            var removed_mut = removed_entry.value;
            removed_mut.deinit(gpa);
        }
    }
}

/// Get the Voxeme at the given voxeme-space coordinate, if it exists.
pub fn getVoxeme(self: *const Grid, coord: VoxemeCoord) ?*Voxeme {
    const page_coord = convert.voxemeToPage(coord);
    const page = self.pages.getPtr(page_coord) orelse return null;
    if (!page.is_heterogeneous) return null;
    return page.heterogeneous.getPtr(coord);
}

/// Get a mutable pointer to the Voxel at a given coordinate, if it exists within a heterogeneous buffer.
/// This will return null for voxels in homogeneous pages or voxemes, as they have no unique memory location.
pub fn getVoxelPtr(self: *Grid, coord: VoxelCoord) ?*Voxel {
    const page_coord = convert.voxelToPage(coord);
    const page = self.pages.getPtr(page_coord) orelse return null;
    // Can only get a pointer if the page is heterogeneous.
    if (!page.is_heterogeneous) return null;

    const voxeme_coord = convert.voxelToVoxeme(coord);
    const voxeme = page.heterogeneous.get(voxeme_coord) orelse return null;
    // Can only get a pointer if the voxeme is heterogeneous.
    if (voxeme.is_homogeneous) return null;

    const buffer = voxeme.asHeterogeneous();
    const local_voxel_coord = convert.voxelToBuffer(coord);
    const index = convert.bufferToIndex(local_voxel_coord);
    return &buffer[index];
}

/// Get the Voxel at the given voxel-space coordinate, if it exists.
pub fn getVoxel(self: *Grid, coord: VoxelCoord) Voxel {
    const page_coord = convert.voxelToPage(coord);
    const voxeme_coord = convert.voxelToVoxeme(coord);
    const local_voxel_coord = convert.voxelToBuffer(coord);

    const page = self.pages.getPtr(page_coord) orelse return .empty;
    if (!page.is_heterogeneous) {
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

/// Delete the Voxel at the given voxel-space coordinate.
pub fn delVoxel(self: *Grid, coord: VoxelCoord) !void {
    if (!self.isVoxelMapped(coord)) return;

    const page_coord = convert.voxelToPage(coord);
    const voxeme_coord = convert.voxelToVoxeme(coord);

    const local_voxel_coord = convert.voxelToBuffer(coord);
    const index = convert.bufferToIndex(local_voxel_coord);

    const page = self.pages.getPtr(page_coord) orelse unreachable;
    if (!page.is_heterogeneous) {
        const old_voxel = page.homogeneous;
        std.debug.assert(old_voxel.material_id != .none); // sanity check: shouldn't be any empty pages

        page.is_heterogeneous = true;
        page.heterogeneous = .empty; // re-initialize to empty

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
    page.is_dirty = true;
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

    if (!page.is_heterogeneous) {
        const old_voxel = page.homogeneous;
        if (value.eql(old_voxel)) {
            return; // No change needed
        }

        page.is_heterogeneous = true;
        page.heterogeneous = .empty; // re-initialize to empty

        const new_buffer = try self.allocateBuffer();

        // Initialize the new buffer to old_voxel
        @memset(new_buffer, old_voxel);

        // Set the specific voxel
        const local_voxel_coord = convert.voxelToBuffer(coord);
        const index = convert.bufferToIndex(local_voxel_coord);
        new_buffer[index].set(value);

        // Insert the new heterogeneous voxeme
        try page.heterogeneous.put(self.allocator(), voxeme_coord, Voxeme.heterogeneous(new_buffer));
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
            }
        }
    }

    page.is_dirty = true;
}

/// Generates a simple, blocky, non-greedy mesh for a single Voxeme.
/// This function has two paths:
/// 1. A fast path for heterogeneous voxemes that uses pre-calculated visibility flags.
/// 2. A slower path for homogeneous (or implicitly homogeneous) voxemes that checks the 6 neighboring voxemes.
pub fn voxemeMeshBasic(self: *const Grid, gpa: std.mem.Allocator, voxeme_coord: VoxemeCoord, vertices: *std.ArrayList(Vertex), indices: *std.ArrayList(u32)) !void {
    // getVoxel and getVoxeme are behaviorally const but are not marked as such.
    // We use @constCast to call them, assuming read-only operations are safe.
    var mut_self = @constCast(self);
    const voxeme_ptr = self.getVoxeme(voxeme_coord);

    // Fast path for heterogeneous voxemes using pre-calculated visibility flags.
    if (voxeme_ptr) |v_ptr| {
        if (!v_ptr.is_homogeneous) {
            const buffer = v_ptr.asHeterogeneous();
            const start_voxel_coord = convert.voxemeToVoxel(voxeme_coord, .min);

            var z: u32 = 0;
            while (z < voxeme_length) : (z += 1) {
                var y: u32 = 0;
                while (y < voxeme_length) : (y += 1) {
                    var x: u32 = 0;
                    while (x < voxeme_length) : (x += 1) {
                        const index = convert.bufferToIndex(.{ x, y, z });
                        const voxel = buffer[index];

                        const material_props = self.getMaterialProperties(voxel.material_id);
                        if (!material_props.is_opaque) {
                            continue;
                        }

                        const vis_bits: u6 = @bitCast(voxel.visibility);
                        if (vis_bits == 0) continue; // Quick skip if no faces are visible

                        const voxel_world_pos = convert.voxelToWorld(start_voxel_coord + VoxelCoord{ @intCast(x), @intCast(y), @intCast(z) }, .min);
                        const color = material_props.color;

                        // Iterate through the 6 faces, adding a quad for each visible one.
                        for (0..6) |face_index| {
                            if ((vis_bits >> @as(u3, @intCast(face_index))) & 1 == 1) {
                                try addFace(gpa, vertices, indices, voxel_world_pos, voxel_scale, face_index, color);
                            }
                        }
                    }
                }
            }
            return; // Finished with the fast path.
        }
    }

    // Slow path: For homogeneous voxemes or empty space within a page that resolves to a solid color.
    // This is taken if getVoxeme returns null (implicit homogeneous) or a homogeneous voxeme.
    const first_voxel_coord = convert.voxemeToVoxel(voxeme_coord, .min);
    const sample_voxel = mut_self.getVoxel(first_voxel_coord);

    const material_props = self.getMaterialProperties(sample_voxel.material_id);
    // If the entire voxeme is effectively empty/non-opaque, there's nothing to mesh.
    if (!material_props.is_opaque) {
        return;
    }

    const color = material_props.color;
    // The voxeme is solid. Mesh it as one large cube, checking 6 neighbors for visibility.
    const voxeme_world_pos = convert.voxemeToWorld(voxeme_coord, .min);
    const neighbor_offsets = [_]VoxelCoord{ .{ 1, 0, 0 }, .{ -1, 0, 0 }, .{ 0, 1, 0 }, .{ 0, -1, 0 }, .{ 0, 0, 1 }, .{ 0, 0, -1 } };

    for (neighbor_offsets, 0..) |offset, face_index| {
        // To check the neighbor, we get a voxel from the adjacent voxeme.
        const neighbor_voxeme_coord = voxeme_coord + offset;
        const sample_voxel_in_neighbor = mut_self.getVoxel(convert.voxemeToVoxel(neighbor_voxeme_coord, .min));

        if (!self.getMaterialProperties(sample_voxel_in_neighbor.material_id).is_opaque) {
            // Neighbor is not opaque, so this face is visible.
            try addFace(gpa, vertices, indices, voxeme_world_pos, voxeme_scale, face_index, color);
        }
    }
}

test "coordinate conversions" {
    // Calculate the length of one side of a page, *not the total volume*
    const page_length_in_voxels: i32 = @intCast(page_length * voxeme_length);

    try std.testing.expectEqual(@as(VoxemeCoord, .{ 0, 0, 0 }), convert.voxelToVoxeme(.{ 0, 0, 0 }));
    try std.testing.expectEqual(@as(BufferCoord, .{ 0, 0, 0 }), convert.voxelToBuffer(.{ 0, 0, 0 }));

    try std.testing.expectEqual(@as(VoxemeCoord, .{ 0, 0, 0 }), convert.voxelToVoxeme(.{ 15, 5, 1 }));
    try std.testing.expectEqual(@as(BufferCoord, .{ 15, 5, 1 }), convert.voxelToBuffer(.{ 15, 5, 1 }));

    try std.testing.expectEqual(@as(VoxemeCoord, .{ 1, 2, 3 }), convert.voxelToVoxeme(.{ 16, 32, 48 }));
    try std.testing.expectEqual(@as(BufferCoord, .{ 0, 0, 0 }), convert.voxelToBuffer(.{ 16, 32, 48 }));

    // Test negative coordinates, the most common failure point
    try std.testing.expectEqual(@as(VoxemeCoord, .{ -1, -1, -1 }), convert.voxelToVoxeme(.{ -1, -1, -1 }));
    try std.testing.expectEqual(@as(BufferCoord, .{ 15, 15, 15 }), convert.voxelToBuffer(.{ -1, -1, -1 }));

    try std.testing.expectEqual(@as(VoxemeCoord, .{ -1, -1, -1 }), convert.voxelToVoxeme(.{ -16, -16, -16 }));
    try std.testing.expectEqual(@as(BufferCoord, .{ 0, 0, 0 }), convert.voxelToBuffer(.{ -16, -16, -16 }));

    try std.testing.expectEqual(@as(VoxemeCoord, .{ -2, -2, -2 }), convert.voxelToVoxeme(.{ -17, -17, -17 }));
    try std.testing.expectEqual(@as(BufferCoord, .{ 15, 15, 15 }), convert.voxelToBuffer(.{ -17, -17, -17 }));

    // Test voxel -> page conversions using the corrected length
    try std.testing.expectEqual(@as(PageCoord, .{ 0, 0, 0 }), convert.voxelToPage(.{ 0, 0, 0 }));
    try std.testing.expectEqual(@as(PageCoord, .{ 0, 0, 0 }), convert.voxelToPage(.{ page_length_in_voxels - 1, page_length_in_voxels - 1, page_length_in_voxels - 1 }));
    try std.testing.expectEqual(@as(PageCoord, .{ 1, 1, 1 }), convert.voxelToPage(.{ page_length_in_voxels, page_length_in_voxels, page_length_in_voxels }));
    try std.testing.expectEqual(@as(PageCoord, .{ -1, -1, -1 }), convert.voxelToPage(.{ -1, -1, -1 }));
    try std.testing.expectEqual(@as(PageCoord, .{ -2, -2, -2 }), convert.voxelToPage(.{ -page_length_in_voxels - 1, -page_length_in_voxels - 1, -page_length_in_voxels - 1 }));

    // Test buffer indexing
    try std.testing.expectEqual(@as(usize, 0), convert.bufferToIndex(.{ 0, 0, 0 }));
    try std.testing.expectEqual(@as(usize, 15), convert.bufferToIndex(.{ 15, 0, 0 }));
    try std.testing.expectEqual(@as(usize, 16), convert.bufferToIndex(.{ 0, 1, 0 }));
    try std.testing.expectEqual(@as(usize, 256), convert.bufferToIndex(.{ 0, 0, 1 }));
    try std.testing.expectEqual(@as(usize, voxels_per_voxeme - 1), convert.bufferToIndex(.{ 15, 15, 15 }));

    // Test world <-> voxel round trip
    const v_coord1: VoxelCoord = .{ 10, 20, 30 };
    const w_pos1 = convert.voxelToWorld(v_coord1, .center);
    const v_coord1_roundtrip = convert.worldToVoxel(w_pos1);
    try std.testing.expectEqual(v_coord1, v_coord1_roundtrip);

    const v_coord2: VoxelCoord = .{ -5, -80, 12 };
    const w_pos2 = convert.voxelToWorld(v_coord2, .min); // use min offset
    const v_coord2_roundtrip = convert.worldToVoxel(w_pos2);
    try std.testing.expectEqual(v_coord2, v_coord2_roundtrip);
}

test "Grid API" {
    const gpa = std.testing.allocator;

    var grid = try Grid.init(gpa);
    defer grid.deinit();

    var frame_arena = std.heap.ArenaAllocator.init(gpa);
    defer frame_arena.deinit();

    const stone_mat = try grid.bindMaterial(.{ .is_opaque = true, .color = .{ 0.5, 0.5, 0.5 } });
    const dirt_mat = try grid.bindMaterial(.{ .is_opaque = true, .color = .{ 0.6, 0.4, 0.2 } });

    const stone: LiteVoxel = .{ .material_id = stone_mat, .state = 10 };
    const dirt: LiteVoxel = .{ .material_id = dirt_mat, .state = 20 };

    const coord1 = VoxelCoord{ 5, 5, 5 };
    const coord2 = VoxelCoord{ 6, 6, 6 };
    const voxeme_coord = convert.voxelToVoxeme(coord1);
    const page_coord = convert.voxelToPage(coord1);

    // --- 1. Set Voxel and verify it creates a HETEROGENEOUS voxeme ---
    try grid.setVoxel(coord1, stone);

    try std.testing.expect(grid.isVoxelMapped(coord1));
    try std.testing.expectEqual(stone, grid.getVoxel(coord1).toLite());
    try std.testing.expectEqual(LiteVoxel.empty, grid.getVoxel(coord2).toLite());

    var voxeme = grid.getVoxeme(voxeme_coord) orelse return error.TestFailed;
    var page = grid.pages.getPtr(page_coord) orelse return error.TestFailed;
    try std.testing.expect(!voxeme.is_homogeneous);

    // --- 2. Verify that a partially full voxeme does NOT homogenize ---
    try grid.update(frame_arena.allocator());

    // The voxeme should STILL be heterogeneous because it contains empty voxels.
    voxeme = grid.getVoxeme(voxeme_coord) orelse return error.TestFailed;
    try std.testing.expect(!voxeme.is_homogeneous);
    try std.testing.expect(!voxeme.is_dirty); // Dirty flag should be cleared though.

    // --- 3. Manually fill a voxeme to test TRUE homogenization (Hetero -> Homo) ---
    const buffer = voxeme.asHeterogeneous();
    // Fill the entire buffer with the 'stone' voxel.
    @memset(buffer, Voxel.fromLite(stone));

    // Mark it as dirty so update() will process it.
    voxeme.is_dirty = true;
    page.is_dirty = true;
    try grid.update(frame_arena.allocator());

    // NOW it should be homogeneous.
    voxeme = grid.getVoxeme(voxeme_coord) orelse return error.TestFailed;
    try std.testing.expect(voxeme.is_homogeneous);
    try std.testing.expectEqual(stone, voxeme.payload.homogeneous.toLite());

    // --- 4. Test De-homogenization (Homo -> Hetero) on setVoxel ---
    // Change one voxel in the now-homogeneous voxeme.
    try grid.setVoxel(coord2, dirt);
    voxeme = grid.getVoxeme(voxeme_coord) orelse return error.TestFailed;
    try std.testing.expect(!voxeme.is_homogeneous); // Should be heterogeneous again.

    // Check that the change was applied correctly and other voxels retain the old value.
    try std.testing.expectEqual(dirt, grid.getVoxel(coord2).toLite());
    try std.testing.expectEqual(stone, grid.getVoxel(coord1).toLite());

    // --- 5. Test Homogenization to Empty and Pruning ---
    // Manually clear the entire buffer to test removal.
    const new_buffer = voxeme.asHeterogeneous();
    @memset(new_buffer, Voxel.empty);

    // Mark as dirty and run update.
    voxeme.is_dirty = true;
    page.is_dirty = true;
    try grid.update(frame_arena.allocator());

    // The voxeme and its containing page should now be completely gone.
    try std.testing.expect(!grid.isVoxemeMapped(voxeme_coord));
    try std.testing.expect(!grid.isPageMapped(page_coord));
}

test "Page Homogenization Lifecycle" {
    const gpa = std.testing.allocator;
    var grid = try Grid.init(gpa);
    defer grid.deinit();

    var frame_arena = std.heap.ArenaAllocator.init(gpa);
    defer frame_arena.deinit();

    const stone_mat = try grid.bindMaterial(.{ .is_opaque = true, .color = .{ 0.5, 0.5, 0.5 } });
    const dirt_mat = try grid.bindMaterial(.{ .is_opaque = true, .color = .{ 0.6, 0.4, 0.2 } });

    const stone: LiteVoxel = .{ .material_id = stone_mat, .state = 10 };
    const dirt: LiteVoxel = .{ .material_id = dirt_mat };
    const page_coord = PageCoord{ 0, 0, 0 };

    // --- 1. Test Homogenization to Solid ---
    // Manually create a page and fill it with 4096 identical, homogeneous voxemes
    // to simulate a state that is ready for page-level homogenization.
    {
        const page_gop = try grid.pages.getOrPut(gpa, page_coord);
        page_gop.value_ptr.* = .{
            .is_dirty = true,
            .is_heterogeneous = true,
            .homogeneous = .fromLite(stone), // This value doesn't matter yet, but will be the result
            .heterogeneous = .{},
        };
        const page_ptr = page_gop.value_ptr;

        for (0..voxemes_per_page) |i| {
            const voxeme_coord = convert.pageIndexToVoxemeCoord(page_coord, i);
            try page_ptr.heterogeneous.put(gpa, voxeme_coord, Voxeme.homogeneous(.fromLite(stone)));
        }

        try grid.update(frame_arena.allocator());

        // Assert that the page is now a single homogeneous entity.
        const page = grid.pages.getPtr(page_coord) orelse return error.TestFailed;
        try std.testing.expect(!page.is_heterogeneous);
        try std.testing.expectEqual(stone, page.homogeneous.toLite());
    }

    // --- 2. Test De-homogenization on Edit ---
    // Modify one voxel within the now-solid page.
    {
        const mod_coord = convert.pageToVoxel(page_coord, .center, .center);
        try grid.setVoxel(mod_coord, dirt);

        // Assert that the page has become heterogeneous and contains exactly one override voxeme.
        const page = grid.pages.getPtr(page_coord) orelse return error.TestFailed;
        try std.testing.expect(page.is_heterogeneous);
        try std.testing.expectEqual(@as(usize, 1), page.heterogeneous.count());
    }

    // --- 3. Test Implicit Get after De-homogenization ---
    // Verify that reads from the modified page return the correct explicit or implicit values.
    {
        const mod_coord = convert.pageToVoxel(page_coord, .center, .center);
        const unmod_coord = convert.pageToVoxel(page_coord, .min, .min);

        // The modified voxel should be dirt.
        try std.testing.expectEqual(dirt, grid.getVoxel(mod_coord).toLite());
        // An unmodified voxel should still be the page's implicit material (stone).
        try std.testing.expectEqual(stone, grid.getVoxel(unmod_coord).toLite());
    }

    // --- 4. Test Re-homogenization on Undo ---
    // "Undo" the modification by setting the voxel back to stone.
    {
        const mod_coord = convert.pageToVoxel(page_coord, .center, .center);
        try grid.setVoxel(mod_coord, stone);

        // The page is still heterogeneous, but the one override voxeme is now homogeneous stone.
        // After homogenization, the override should be removed and the page should become pure again.
        try grid.update(frame_arena.allocator());

        const page = grid.pages.getPtr(page_coord) orelse return error.TestFailed;
        try std.testing.expect(!page.is_heterogeneous);
        try std.testing.expectEqual(stone, page.homogeneous.toLite());
    }

    // --- 5. Test Pruning of Empty Page ---
    // Create a new, simple page, delete its only content, and ensure it gets removed.
    {
        const prune_page_coord = PageCoord{ 1, 1, 1 };
        const prune_voxel_coord = convert.pageToVoxel(prune_page_coord, .min, .min);

        // This creates a heterogeneous page with an implicit 'empty' material and one 'stone' voxeme.
        try grid.setVoxel(prune_voxel_coord, stone);
        try std.testing.expect(grid.isPageMapped(prune_page_coord));

        // Delete the only solid voxel, leaving an empty heterogeneous page.
        try grid.delVoxel(prune_voxel_coord);

        // Homogenization should find the page has no overrides and its implicit material is 'empty',
        // so it should schedule the page for removal.
        try grid.update(frame_arena.allocator());

        try std.testing.expect(!grid.isPageMapped(prune_page_coord));
    }
}

test "Visibility Precalculation" {
    const gpa = std.testing.allocator;
    var grid = try Grid.init(gpa);
    defer grid.deinit();

    var frame_arena = std.heap.ArenaAllocator.init(gpa);
    defer frame_arena.deinit();

    // --- Setup Materials ---
    const stone_mat = try grid.bindMaterial(.{ .is_opaque = true, .color = .{ 0.5, 0.5, 0.5 } });
    const glass_mat = try grid.bindMaterial(.{ .is_opaque = false, .color = .{ 0.8, 0.9, 1.0 } });
    const stone: LiteVoxel = .{ .material_id = stone_mat };
    const glass: LiteVoxel = .{ .material_id = glass_mat };

    // Helper to make checking visibility less verbose
    const VisCheck = struct {
        px: bool,
        nx: bool,
        py: bool,
        ny: bool,
        pz: bool,
        nz: bool,

        fn check(self: @This(), v: Voxel) !void {
            try std.testing.expectEqual(self.px, v.visibility.pos_x);
            try std.testing.expectEqual(self.nx, v.visibility.neg_x);
            try std.testing.expectEqual(self.py, v.visibility.pos_y);
            try std.testing.expectEqual(self.ny, v.visibility.neg_y);
            try std.testing.expectEqual(self.pz, v.visibility.pos_z);
            try std.testing.expectEqual(self.nz, v.visibility.neg_z);
        }
    };

    // --- 1. Test isolated voxel (all faces visible) ---
    {
        const coord = VoxelCoord{ 0, 0, 0 };
        try grid.setVoxel(coord, stone);
        try grid.update(frame_arena.allocator());
        const v = grid.getVoxel(coord);
        try (VisCheck{ .px = true, .nx = true, .py = true, .ny = true, .pz = true, .nz = true }).check(v);
        try grid.delVoxel(coord); // clean up for next test
        try grid.update(frame_arena.allocator());
    }

    // --- 2. Test direct occlusion between two adjacent voxels ---
    {
        const coord_a = VoxelCoord{ 1, 0, 0 };
        const coord_b = VoxelCoord{ 2, 0, 0 };
        try grid.setVoxel(coord_a, stone);
        try grid.setVoxel(coord_b, stone);
        try grid.update(frame_arena.allocator());

        // Voxel A should be occluded on its +X face by Voxel B
        const v_a = grid.getVoxel(coord_a);
        try (VisCheck{ .px = false, .nx = true, .py = true, .ny = true, .pz = true, .nz = true }).check(v_a);

        // Voxel B should be occluded on its -X face by Voxel A
        const v_b = grid.getVoxel(coord_b);
        try (VisCheck{ .px = true, .nx = false, .py = true, .ny = true, .pz = true, .nz = true }).check(v_b);

        try grid.delVoxel(coord_a);
        try grid.delVoxel(coord_b);
        try grid.update(frame_arena.allocator());
    }

    // --- 3. Test fully enclosed voxel (no faces visible) ---
    {
        const center = VoxelCoord{ 5, 5, 5 };
        try grid.setVoxel(center, stone);
        try grid.setVoxel(center + VoxelCoord{ 1, 0, 0 }, stone);
        try grid.setVoxel(center + VoxelCoord{ -1, 0, 0 }, stone);
        try grid.setVoxel(center + VoxelCoord{ 0, 1, 0 }, stone);
        try grid.setVoxel(center + VoxelCoord{ 0, -1, 0 }, stone);
        try grid.setVoxel(center + VoxelCoord{ 0, 0, 1 }, stone);
        try grid.setVoxel(center + VoxelCoord{ 0, 0, -1 }, stone);
        try grid.update(frame_arena.allocator());

        const v = grid.getVoxel(center);
        try (VisCheck{ .px = false, .nx = false, .py = false, .ny = false, .pz = false, .nz = false }).check(v);
    }

    // --- 4. Test non-opaque voxel (always all faces visible) ---
    {
        // Use the same enclosed setup as test 3, but make the center voxel glass
        const center = VoxelCoord{ 5, 5, 5 };
        try grid.setVoxel(center, glass);
        try grid.update(frame_arena.allocator());

        const v = grid.getVoxel(center);
        // Even though it's enclosed by opaque stone, its own visibility should be .all
        try (VisCheck{ .px = true, .nx = true, .py = true, .ny = true, .pz = true, .nz = true }).check(v);
    }

    // --- 5. Test boundary crossing visibility ---
    {
        // Voxeme Boundary (voxeme_length = 16)
        const v_coord1 = VoxelCoord{ 15, 10, 10 };
        const v_coord2 = VoxelCoord{ 16, 10, 10 };
        std.debug.assert(@reduce(.Or, convert.voxelToVoxeme(v_coord1) != convert.voxelToVoxeme(v_coord2)));
        try grid.setVoxel(v_coord1, stone);
        try grid.setVoxel(v_coord2, stone);

        // Page Boundary (page_length_in_voxels = 16*16 = 256)
        const p_coord1 = VoxelCoord{ 255, 10, 10 };
        const p_coord2 = VoxelCoord{ 256, 10, 10 };
        std.debug.assert(@reduce(.Or, convert.voxelToPage(p_coord1) != convert.voxelToPage(p_coord2)));
        try grid.setVoxel(p_coord1, stone);
        try grid.setVoxel(p_coord2, stone);

        try grid.update(frame_arena.allocator());

        // Check voxeme boundary occlusion
        try (VisCheck{ .px = false, .nx = true, .py = true, .ny = true, .pz = true, .nz = true }).check(grid.getVoxel(v_coord1));
        try (VisCheck{ .px = true, .nx = false, .py = true, .ny = true, .pz = true, .nz = true }).check(grid.getVoxel(v_coord2));

        // Check page boundary occlusion
        try (VisCheck{ .px = false, .nx = true, .py = true, .ny = true, .pz = true, .nz = true }).check(grid.getVoxel(p_coord1));
        try (VisCheck{ .px = true, .nx = false, .py = true, .ny = true, .pz = true, .nz = true }).check(grid.getVoxel(p_coord2));
    }

    // --- 6. Test Heterogeneous -> Homogeneous visibility ---
    {
        // First, create a fully solid, homogeneous page of stone at (1,1,1)
        const solid_page_coord = PageCoord{ 1, 1, 1 };
        const page_gop = try grid.pages.getOrPut(gpa, solid_page_coord);
        page_gop.value_ptr.* = .{
            .is_dirty = false, // Not dirty, already homogeneous
            .is_heterogeneous = false,
            .homogeneous = .fromLite(stone),
            .heterogeneous = .{},
        };

        // Now, place a single voxel just outside that page, at the -X boundary.
        const test_coord = convert.pageToVoxel(solid_page_coord, .min, .min) - VoxelCoord{ 1, 0, 0 };
        try grid.setVoxel(test_coord, stone);
        try grid.update(frame_arena.allocator());

        // This voxel's +X face should be occluded by the neighboring homogeneous page.
        const v = grid.getVoxel(test_coord);
        try (VisCheck{ .px = false, .nx = true, .py = true, .ny = true, .pz = true, .nz = true }).check(v);
    }
}

test "voxeme meshing" {
    const gpa = std.testing.allocator;

    var grid = try Grid.init(gpa);
    defer grid.deinit();

    var frame_arena = std.heap.ArenaAllocator.init(gpa);
    defer frame_arena.deinit();
    const fa = frame_arena.allocator();

    var vertices = std.ArrayList(Vertex).empty;
    defer vertices.deinit(gpa);
    var indices = std.ArrayList(u32).empty;
    defer indices.deinit(gpa);

    const stone_mat = try grid.bindMaterial(.{ .is_opaque = true, .color = .{ 0.5, 0.5, 0.5 } });
    const stone: LiteVoxel = .{ .material_id = stone_mat };

    // --- Case 1: Single Voxel (Heterogeneous Path) ---
    // A single voxel should have 6 visible faces.
    {
        vertices.clearRetainingCapacity();
        indices.clearRetainingCapacity();

        const coord = VoxelCoord{ 0, 0, 0 };
        const voxeme_coord = convert.voxelToVoxeme(coord);
        try grid.setVoxel(coord, stone);
        try grid.update(fa);

        try grid.voxemeMeshBasic(gpa, voxeme_coord, &vertices, &indices);

        try std.testing.expectEqual(@as(usize, 24), vertices.items.len); // 6 faces * 4 vertices
        try std.testing.expectEqual(@as(usize, 36), indices.items.len); // 6 faces * 6 indices

        try grid.delVoxel(coord);
        try grid.update(fa);
    }

    // --- Case 2: Two adjacent voxels in same voxeme (Heterogeneous Path) ---
    // Two adjacent voxels should hide one face each, for a total of 10 visible faces.
    {
        vertices.clearRetainingCapacity();
        indices.clearRetainingCapacity();

        const coord1 = VoxelCoord{ 1, 1, 1 };
        const coord2 = VoxelCoord{ 2, 1, 1 }; // +X neighbor
        const voxeme_coord = convert.voxelToVoxeme(coord1);
        std.debug.assert(@reduce(.And, voxeme_coord == convert.voxelToVoxeme(coord2)));

        try grid.setVoxel(coord1, stone);
        try grid.setVoxel(coord2, stone);
        try grid.update(fa);

        try grid.voxemeMeshBasic(gpa, voxeme_coord, &vertices, &indices);

        try std.testing.expectEqual(@as(usize, 40), vertices.items.len); // 10 faces * 4 vertices
        try std.testing.expectEqual(@as(usize, 60), indices.items.len); // 10 faces * 6 indices

        try grid.delVoxel(coord1);
        try grid.delVoxel(coord2);
        try grid.update(fa);
    }

    // --- Case 3: Homogeneous voxeme, isolated (Homogeneous Path) ---
    // A full, solid voxeme should be meshed as one large cube with 6 faces.
    {
        vertices.clearRetainingCapacity();
        indices.clearRetainingCapacity();

        const voxeme_coord = VoxemeCoord{ 5, 5, 5 };
        const page_coord = convert.voxemeToPage(voxeme_coord);

        // Manually create a homogeneous voxeme
        const page_gop = try grid.pages.getOrPut(gpa, page_coord);
        if (!page_gop.found_existing) page_gop.value_ptr.* = .empty;
        page_gop.value_ptr.is_heterogeneous = true; // Make page hetero to hold the voxeme
        _ = try page_gop.value_ptr.heterogeneous.getOrPutValue(gpa, voxeme_coord, Voxeme.homogeneous(.fromLite(stone)));
        page_gop.value_ptr.is_dirty = true;

        try grid.voxemeMeshBasic(gpa, voxeme_coord, &vertices, &indices);

        try std.testing.expectEqual(@as(usize, 24), vertices.items.len); // 6 faces * 4 vertices
        try std.testing.expectEqual(@as(usize, 36), indices.items.len); // 6 faces * 6 indices
    }

    // --- Case 4: Homogeneous voxeme, occluded (Homogeneous Path) ---
    // Two adjacent homogeneous voxemes. We mesh one, it should have one face occluded.
    {
        vertices.clearRetainingCapacity();
        indices.clearRetainingCapacity();

        const v_coord1 = VoxemeCoord{ 10, 10, 10 };
        const v_coord2 = VoxemeCoord{ 11, 10, 10 }; // +X neighbor

        try insertHomoVoxeme(&grid, gpa, v_coord1, stone);
        try insertHomoVoxeme(&grid, gpa, v_coord2, stone);

        // Mesh the first voxeme
        try grid.voxemeMeshBasic(gpa, v_coord1, &vertices, &indices);

        // It should have 5 faces, as its +X face is occluded
        try std.testing.expectEqual(@as(usize, 20), vertices.items.len); // 5 faces * 4 vertices
        try std.testing.expectEqual(@as(usize, 30), indices.items.len); // 5 faces * 6 indices
    }
}

fn insertHomoVoxeme(g: *Grid, gpa: std.mem.Allocator, v_coord: VoxemeCoord, lite: LiteVoxel) !void {
    const p_coord = convert.voxemeToPage(v_coord);
    const p_gop = try g.pages.getOrPut(gpa, p_coord);
    if (!p_gop.found_existing) p_gop.value_ptr.* = .empty;
    p_gop.value_ptr.is_heterogeneous = true;
    _ = try p_gop.value_ptr.heterogeneous.getOrPutValue(gpa, v_coord, Voxeme.homogeneous(.fromLite(lite)));
}
