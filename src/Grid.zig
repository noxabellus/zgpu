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

/// Unique identifier for a material type in the voxel Grid's world.
pub const MaterialId = enum(u16) { none = 0, _ };

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
};

/// A sparse map of all existing Voxemes in a particular area, keyed by their VoxemeCoord.
pub const Page = struct {
    is_dirty: bool,
    voxemes: std.AutoHashMapUnmanaged(VoxemeCoord, Voxeme),

    pub const empty = Page{ .is_dirty = false, .voxemes = .empty };
};

/// A Voxeme is a small, dense 3D grid of Voxels.
/// This is represented in our system as a tagged pointer, allowing homogenous Voxemes to avoid an allocation.
pub const Voxeme = packed struct(u64) {
    is_homogeneous: bool,
    is_dirty: bool = false,
    _reserved: u14 = 0,
    payload: packed union {
        /// A homogenous Voxeme is represented as a single MaterialId and state value, without allocating the array.
        /// This should never have value.material_id == 0, which would indicate a homogeneously empty Voxeme, which should be removed from the Page entirely.
        homogeneous: Voxel,
        /// the OS pointer is at most 48 bits; we store it here
        heterogeneous: u48,
    },

    /// Create a homogenous Voxeme containing the given Voxel.
    /// * Asserts `voxel.material_id != .none`
    pub fn homogeneous(voxel: Voxel) Voxeme {
        std.debug.assert(voxel.material_id != .none);
        return Voxeme{
            .is_homogeneous = true,
            .payload = .{ .homogeneous = voxel },
        };
    }

    /// Create a heterogeneous Voxeme containing the given pointer to an array of Voxels.
    pub fn heterogeneous(voxels: Buffer) Voxeme {
        return Voxeme{
            .is_homogeneous = false,
            .payload = .{ .heterogeneous = @intCast(@intFromPtr(voxels)) },
        };
    }

    /// Attempt to get this Voxeme as a homogeneous Voxel.
    pub fn asHomogeneous(self: Voxeme) ?Voxel {
        if (!self.is_homogeneous) return null;
        return self.payload.homogeneous;
    }

    /// Attempt to get this Voxeme as a pointer to a heterogeneous Buffer.
    pub fn asHeterogeneous(self: Voxeme) ?Buffer {
        if (self.is_homogeneous) return null;
        return @ptrFromInt(self.payload.heterogeneous);
    }

    /// Get this Voxeme as a homogeneous Voxel.
    /// * Asserts that it is indeed homogeneous
    pub fn forceHomogeneous(self: Voxeme) Voxel {
        std.debug.assert(self.is_homogeneous);
        return self.payload.homogeneous;
    }

    /// Get this Voxeme as a pointer to a heterogeneous Buffer.
    /// * Asserts that it is indeed heterogeneous
    pub fn forceHeterogeneous(self: Voxeme) Buffer {
        std.debug.assert(!self.is_homogeneous);
        return @ptrFromInt(self.payload.heterogeneous);
    }
};

/// The smallest unit of volume in our voxel Grid's world.
pub const Voxel = packed struct(u32) {
    /// The material type of this voxel. A value of `none`/`0` indicates an empty voxel.
    material_id: MaterialId,
    /// Material-specific state data, e.g. amount of fluid, rich-state map key, etc.
    state: u16 = 0,

    pub const empty = Voxel{ .material_id = .none };
};

/// The memory pool used to allocate heterogeneous Buffers.
pool: std.heap.MemoryPool(BufferData),

/// A sparse map of all existing Pages in the Grid, keyed by their PageCoord.
pages: std.AutoHashMapUnmanaged(PageCoord, Page),

/// Create a new empty Grid with a preheated pool for Buffers.
pub fn init(backing_allocator: std.mem.Allocator) !Grid {
    return Grid{
        .pool = try std.heap.MemoryPool(BufferData).initPreheated(backing_allocator, 4096),
        .pages = .empty,
    };
}

/// Deinitialize the Grid, freeing all allocated memory.
pub fn deinit(self: *Grid) void {
    const gpa = self.allocator();

    // Deinitialize all Pages
    var it = self.pages.valueIterator();
    while (it.next()) |page| {
        page.voxemes.deinit(gpa);
        // debug safety: prevent use after free
        page.* = undefined;
    }

    // Free the Page hashmap
    self.pages.deinit(gpa);

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

/// Determine if a given voxel-space coordinate is mapped (Not whether it exists).
pub fn isVoxelMapped(self: *const Grid, coord: VoxelCoord) bool {
    const page_coord = convert.voxelToPage(coord);
    const voxeme_coord = convert.voxelToVoxeme(coord);

    const page = self.pages.getPtr(page_coord) orelse return false;
    return page.voxemes.contains(voxeme_coord);
}

/// Determine if a given voxeme-space coordinate is mapped.
pub fn isVoxemeMapped(self: *const Grid, coord: VoxemeCoord) bool {
    const page_coord = convert.voxemeToPage(coord);

    const page = self.pages.getPtr(page_coord) orelse return false;
    return page.voxemes.contains(coord);
}

/// Determine if a given page-space coordinate is mapped.
pub fn isPageMapped(self: *const Grid, coord: PageCoord) bool {
    return self.pages.contains(coord);
}

/// Loop all Pages and for each dirty Page, loop all its Voxemes and for each dirty Voxeme,
/// check if it can be converted to a homogeneous one, and do so if possible.
pub fn homogenize(self: *Grid, frame_arena: std.mem.Allocator) !void {
    var page_it = self.pages.valueIterator();
    var to_remove = std.ArrayList(VoxemeCoord).empty;
    defer to_remove.deinit(frame_arena); // even though we expect to be passed an arena, this is a cheap robustness measure

    while (page_it.next()) |page| {
        if (!page.is_dirty) continue;

        page.is_dirty = false;

        var voxeme_it = page.voxemes.iterator();
        while (voxeme_it.next()) |voxeme_entry| {
            const voxeme_coord = voxeme_entry.key_ptr.*;
            const voxeme = voxeme_entry.value_ptr;

            if (!voxeme.is_dirty) continue;

            voxeme.is_dirty = false;

            if (voxeme.is_homogeneous) {
                log.warn("is_dirty flag set on homogeneous voxeme", .{});
                continue;
            }

            const buffer = voxeme.forceHeterogeneous();

            // The value that the entire voxeme must match.
            const reference_voxel = buffer[0];
            var is_homogeneous = true;

            // Check if all other voxels match the first one.
            for (buffer[1..]) |v| {
                if (v != reference_voxel) {
                    is_homogeneous = false;
                    break;
                }
            }

            if (is_homogeneous) {
                // The entire buffer is uniform. Now decide what to do.
                if (reference_voxel.material_id == .none) {
                    // The voxeme is entirely empty, so remove it.
                    self.deallocateBuffer(buffer);
                    try to_remove.append(frame_arena, voxeme_coord);
                } else {
                    // The voxeme is entirely filled with a single material and state configuration. Convert it.
                    const old_buffer = buffer;
                    voxeme.* = Voxeme.homogeneous(reference_voxel);
                    self.deallocateBuffer(old_buffer);
                }
            }
        }
    }

    while (to_remove.pop()) |voxeme_coord| {
        const page_coord = convert.voxemeToPage(voxeme_coord);
        const page = self.pages.getPtr(page_coord) orelse unreachable;
        _ = page.voxemes.remove(voxeme_coord);
        if (page.voxemes.count() == 0) {
            // Page is now empty, remove it too
            var kv = self.pages.fetchRemove(page_coord).?;
            kv.value.voxemes.deinit(self.allocator());
        }
    }
}

/// Get the Voxeme at the given voxeme-space coordinate, if it exists.
pub fn getVoxeme(self: *Grid, coord: VoxemeCoord) ?*Voxeme {
    const page_coord = convert.voxemeToPage(coord);
    const page = self.pages.getPtr(page_coord) orelse return null;
    return page.voxemes.getPtr(coord);
}

/// Get the Voxel at the given voxel-space coordinate, if it exists.
pub fn getVoxel(self: *Grid, coord: VoxelCoord) Voxel {
    const page_coord = convert.voxelToPage(coord);
    const voxeme_coord = convert.voxelToVoxeme(coord);
    const local_voxel_coord = convert.voxelToBuffer(coord);

    const page = self.pages.getPtr(page_coord) orelse return Voxel{ .material_id = .none, .state = 0 };
    const voxeme = page.voxemes.get(voxeme_coord) orelse return Voxel{ .material_id = .none, .state = 0 };

    if (voxeme.is_homogeneous) {
        return voxeme.payload.homogeneous;
    } else {
        const buffer = voxeme.forceHeterogeneous();
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
    const voxeme = page.voxemes.getPtr(voxeme_coord) orelse unreachable;

    if (voxeme.is_homogeneous) {
        // We need to recreate the voxeme as a heterogeneous one without this voxel
        const old_voxel = voxeme.payload.homogeneous;
        const new_buffer = try self.allocateBuffer();

        // Initialize the new buffer to old_voxel
        @memset(new_buffer, old_voxel);

        // Set the specific voxel to empty
        new_buffer[index] = .empty;

        // Replace the voxeme in the page
        voxeme.* = Voxeme.heterogeneous(new_buffer);

        // If all other voxels are deleted, this voxeme should be pruned, so we still need to mark it.
        voxeme.is_dirty = true;
        page.is_dirty = true;
    } else {
        // we can just set the voxel to empty in the existing buffer
        const buffer = voxeme.forceHeterogeneous();
        if (buffer[index] == Voxel.empty) {
            return; // No change needed
        }

        buffer[index] = .empty;

        // Mark dirty so that it can be checked for homogenization later
        voxeme.is_dirty = true;
        page.is_dirty = true;
    }
}

/// Set the Voxel at the given voxel-space coordinate.
pub fn setVoxel(self: *Grid, coord: VoxelCoord, value: Voxel) !void {
    if (value.material_id == .none) {
        return self.delVoxel(coord);
    }

    const page_coord = convert.voxelToPage(coord);
    const voxeme_coord = convert.voxelToVoxeme(coord);

    const page_gop = try self.pages.getOrPut(self.allocator(), page_coord);
    if (!page_gop.found_existing) page_gop.value_ptr.* = .empty;

    const page = page_gop.value_ptr;

    const voxeme_gop = try page.voxemes.getOrPut(self.allocator(), voxeme_coord);
    if (!voxeme_gop.found_existing) {
        const new_buffer = try self.allocateBuffer();
        // Initialize the new buffer to empty
        @memset(new_buffer, .empty);

        // Set the voxeme to a heterogeneous one with the new buffer
        voxeme_gop.value_ptr.* = Voxeme.heterogeneous(new_buffer);

        // Now we can set the specific voxel
        const local_voxel_coord = convert.voxelToBuffer(coord);

        const index = convert.bufferToIndex(local_voxel_coord);
        new_buffer[index] = value;

        // If all other voxels are set, this voxeme should be homogenized, so we still need to mark it.
        voxeme_gop.value_ptr.is_dirty = true;
        page.is_dirty = true;
    } else {
        const voxeme = voxeme_gop.value_ptr;

        if (voxeme.is_homogeneous) {
            const old_voxel = voxeme.forceHomogeneous();
            if (value == old_voxel) {
                return; // No change needed
            }

            // We need to recreate the voxeme as a heterogeneous one
            const new_buffer = try self.allocateBuffer();

            // Initialize the new buffer to old_voxel
            @memset(new_buffer, old_voxel);

            // Set the specific voxel to the new value
            const local_voxel_coord = convert.voxelToBuffer(coord);
            const index = convert.bufferToIndex(local_voxel_coord);
            new_buffer[index] = value;

            // Replace the voxeme in the page
            voxeme.* = Voxeme.heterogeneous(new_buffer);

            // If all other voxels are set, this voxeme should be homogenized, so we still need to mark it.
            voxeme.is_dirty = true;
            page.is_dirty = true;
        } else {
            // we can just set the voxel in the existing buffer
            const buffer = voxeme.forceHeterogeneous();

            const local_voxel_coord = convert.voxelToBuffer(coord);
            const index = convert.bufferToIndex(local_voxel_coord);

            if (buffer[index] == value) {
                return; // No change needed
            }

            buffer[index] = value;

            // Mark dirty so that it can be checked for homogenization later
            voxeme.is_dirty = true;
            page.is_dirty = true;
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

    const stone: Voxel = .{ .material_id = @enumFromInt(1), .state = 10 };
    const dirt: Voxel = .{ .material_id = @enumFromInt(2), .state = 20 };

    const coord1 = VoxelCoord{ 5, 5, 5 };
    const coord2 = VoxelCoord{ 6, 6, 6 };
    const voxeme_coord = convert.voxelToVoxeme(coord1);
    const page_coord = convert.voxelToPage(coord1);

    // == 1. Set Voxel and verify it creates a HETEROGENEOUS voxeme ==
    try grid.setVoxel(coord1, stone);

    try std.testing.expect(grid.isVoxelMapped(coord1));
    try std.testing.expectEqual(stone, grid.getVoxel(coord1));
    try std.testing.expectEqual(Voxel.empty, grid.getVoxel(coord2));

    var voxeme = grid.getVoxeme(voxeme_coord) orelse return error.TestFailed;
    var page = grid.pages.getPtr(page_coord) orelse return error.TestFailed;
    try std.testing.expect(!voxeme.is_homogeneous);

    // == 2. Verify that a partially full voxeme does NOT homogenize ==
    try grid.homogenize(frame_arena.allocator());

    // The voxeme should STILL be heterogeneous because it contains empty voxels.
    voxeme = grid.getVoxeme(voxeme_coord) orelse return error.TestFailed;
    try std.testing.expect(!voxeme.is_homogeneous);
    try std.testing.expect(!voxeme.is_dirty); // Dirty flag should be cleared though.

    // == 3. Manually fill a voxeme to test TRUE homogenization (Hetero -> Homo) ==
    const buffer = voxeme.forceHeterogeneous();
    // Fill the entire buffer with the 'stone' voxel.
    @memset(buffer, stone);

    // Mark it as dirty so homogenize() will process it.
    voxeme.is_dirty = true;
    page.is_dirty = true;
    try grid.homogenize(frame_arena.allocator());

    // NOW it should be homogeneous.
    voxeme = grid.getVoxeme(voxeme_coord) orelse return error.TestFailed;
    try std.testing.expect(voxeme.is_homogeneous);
    try std.testing.expectEqual(stone, voxeme.forceHomogeneous());

    // == 4. Test De-homogenization (Homo -> Hetero) on setVoxel ==
    // Change one voxel in the now-homogeneous voxeme.
    try grid.setVoxel(coord2, dirt);
    voxeme = grid.getVoxeme(voxeme_coord) orelse return error.TestFailed;
    try std.testing.expect(!voxeme.is_homogeneous); // Should be heterogeneous again.

    // Check that the change was applied correctly and other voxels retain the old value.
    try std.testing.expectEqual(dirt, grid.getVoxel(coord2));
    try std.testing.expectEqual(stone, grid.getVoxel(coord1));

    // == 5. Test Homogenization to Empty and Pruning ==
    // Manually clear the entire buffer to test removal.
    const new_buffer = voxeme.forceHeterogeneous();
    @memset(new_buffer, Voxel.empty);

    // Mark as dirty and run homogenize.
    voxeme.is_dirty = true;
    page.is_dirty = true;
    try grid.homogenize(frame_arena.allocator());

    // The voxeme and its containing page should now be completely gone.
    try std.testing.expect(!grid.isVoxemeMapped(voxeme_coord));
    try std.testing.expect(!grid.isPageMapped(page_coord));
}
