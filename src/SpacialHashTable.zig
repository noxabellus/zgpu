const std = @import("std");
const log = std.log.scoped(.spacial_hash_table);
const linalg = @import("linalg.zig");
const vec3i = linalg.vec3i;

/// A spacial hash table mapping from `vec3i` to index in a MultiArrayList.
/// This structure is not thread-safe and relies on the command queue architecture implemented in the update manager.
pub fn new(comptime max_indirections: comptime_int) type {
    return extern struct {
        const Self = @This();

        /// Mask for wrapping hash indices.
        pub const indirection_mask = max_indirections - 1;
        /// Marks an empty entry that has never been used.
        pub const sentinel: u32 = std.math.maxInt(u32);
        /// Marks a deleted entry that can be reused.
        pub const tombstone: u32 = sentinel - 1;

        /// The indices into the `pages` MultiArrayList for this page; or `sentinel` if empty, `tombstone` if deleted.
        indices: [max_indirections]u32,
        /// The coordinates for the slots in the table.
        coordinates: [max_indirections]vec3i,
        /// The number of valid entries in the table.
        len: u32,

        /// Initialize the table to empty state.
        pub fn init(self: *Self) void {
            @memset(&self.indices, Self.sentinel);
            @memset(&self.coordinates, vec3i{ 0, 0, 0 });
            self.len = 0;
        }

        /// Copy the table state from another table into this one
        pub fn copy(self: *Self, other: *const Self) void {
            @memcpy(&self.indices, &other.indices);
            @memcpy(&self.coordinates, &other.coordinates);
            self.len = other.len;
        }

        /// Hash function for `PageCoord`.
        fn hash(coord: vec3i) u32 {
            const h = (@as(u32, @bitCast(coord[0])) *% 92837111) ^ (@as(u32, @bitCast(coord[1])) *% 689287499) ^ (@as(u32, @bitCast(coord[2])) *% 283923481);
            return h & indirection_mask;
        }

        /// Look up the index bound to `coord` in the table, or `sentinel` if not found.
        /// This is a non-thread-safe operation; use only from the main thread on the front buffer.
        pub fn lookup(self: *const Self, coord: vec3i) u32 {
            var h = hash(coord);
            for (0..max_indirections) |_| {
                const index = self.indices[h];
                if (index == sentinel) {
                    return sentinel; // Chain ends here.
                }
                if (index != tombstone and @reduce(.And, self.coordinates[h] == coord)) {
                    return index; // Found it!
                }
                h = (h + 1) & indirection_mask;
            }
            return sentinel;
        }

        /// Insert a new index binding for `coord` into the table, or update an existing binding.
        /// This is a non-thread-safe operation; use only from the main thread on the front buffer.
        pub fn insert(self: *Self, coord: vec3i, new_index: u32) bool {
            var h = hash(coord);
            for (0..max_indirections) |_| {
                const index = self.indices[h];
                if (index == sentinel or index == tombstone) {
                    // Empty or deleted slot; claim it.
                    self.indices[h] = new_index;
                    self.coordinates[h] = coord;
                    self.len += 1;
                    return true;
                }
                if (index != tombstone and @reduce(.And, self.coordinates[h] == coord)) {
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
        pub fn remove(self: *Self, coord: vec3i) void {
            var h = hash(coord);
            for (0..max_indirections) |_| {
                const index = self.indices[h];
                if (index == sentinel) return;
                if (index != tombstone and @reduce(.And, self.coordinates[h] == coord)) {
                    self.indices[h] = tombstone;
                    self.len -= 1;
                    return;
                }
                h = (h + 1) & indirection_mask;
            }
        }
    };
}
