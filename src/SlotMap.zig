const SlotMap = @This();

const std = @import("std");

pub const Ref = packed struct {
    index: usize,
    generation: Generation,

    pub const invalid = @This(){
        .index = 0,
        .generation = .invalid,
    };
};

pub const Generation = enum(u64) {
    invalid = 0,
    _,
};

pub fn new(comptime T: type) type {
    return struct {
        const Self = @This();

        // S is the "Slot Index" type, used to index into sparse arrays.
        const S = usize;
        // V is the "Value Index" type, used to index into dense arrays.
        const V = usize;

        const Slot = union(enum) {
            /// The slot is in use and stores the index of the value in the dense `values` array.
            occupied: V,
            /// The slot is free and stores the index of the next free slot in the freelist chain.
            free: ?S,
        };

        // --- Slot-Indexed Data (Sparse) ---
        // These arrays grow but their elements never move. Indexed by `Ref.index`.
        slots: std.ArrayListUnmanaged(Slot) = .empty,
        generations: std.ArrayListUnmanaged(Generation) = .empty,

        // --- Value-Indexed Data (Dense) ---
        // These arrays are kept tightly packed. Indexed by `Slot.occupied`.
        data: std.ArrayListUnmanaged(T) = .empty,
        value_to_slot: std.ArrayListUnmanaged(S) = .empty,

        /// The number of active elements in the SlotMap.
        len: usize = 0,
        freelist_head: ?S = null,

        pub const empty = Self{};

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.slots.deinit(allocator);
            self.generations.deinit(allocator);
            self.data.deinit(allocator);
            self.value_to_slot.deinit(allocator);
        }

        pub fn clear(self: *Self) void {
            // This is a simple clear; a "clearRetainingCapacity" could be implemented
            // by rebuilding the freelist from all existing slots.
            self.slots.clearRetainingCapacity();
            self.generations.clearRetainingCapacity();
            self.data.clearRetainingCapacity();
            self.value_to_slot.clearRetainingCapacity();
            self.len = 0;
            self.freelist_head = null;
        }

        pub fn create(self: *Self, allocator: std.mem.Allocator) !struct { Ref, *T } {
            const slot_index: S = if (self.freelist_head) |head| from_freelist: {
                // Pop a slot from the freelist.
                self.freelist_head = switch (self.slots.items[head]) {
                    .free => |next| next,
                    .occupied => unreachable, // Should not be occupied if it's on the freelist.
                };
                break :from_freelist head;
            } else fresh_slot: {
                // No free slots, so create a new one.
                const new_index = self.slots.items.len;
                try self.slots.append(allocator, undefined); // Placeholder, will be set below.
                try self.generations.append(allocator, .invalid);
                self.generations.items[new_index] = @enumFromInt(1);
                break :fresh_slot new_index;
            };

            // The value will be placed at the end of the dense array.
            const value_index = self.len;

            // Reserve space in the dense arrays.
            try self.data.append(allocator, undefined);
            try self.value_to_slot.append(allocator, undefined);

            // Link everything together.
            self.slots.items[slot_index] = .{ .occupied = value_index };
            self.value_to_slot.items[value_index] = slot_index;

            self.len += 1;

            return .{
                .{ .index = slot_index, .generation = self.generations.items[slot_index] },
                &self.data.items[value_index],
            };
        }

        pub fn remove(self: *Self, ref: Ref) ?T {
            const slot_index = ref.index;

            // Basic validation.
            if (slot_index >= self.slots.items.len) return null;
            if (self.generations.items[slot_index] != ref.generation) return null;

            const destroyed_value_index = switch (self.slots.items[slot_index]) {
                .occupied => |v_idx| v_idx,
                .free => return null, // Already removed.
            };

            // Increment generation to invalidate all old refs to this slot.
            const g = &self.generations.items[slot_index];
            g.* = @enumFromInt(@intFromEnum(g.*) + 1);
            if (g.* == .invalid) { // Wrap around behavior
                g.* = @enumFromInt(1);
            }

            // Put the now-unused slot onto the freelist.
            self.slots.items[slot_index] = .{ .free = self.freelist_head };
            self.freelist_head = slot_index;

            // Get the value to return before we overwrite it.
            const out = self.data.items[destroyed_value_index];
            self.len -= 1;
            const last_value_index = self.len;

            // To keep the value array dense, swap the last element into the place
            // of the element being removed.
            if (destroyed_value_index != last_value_index) {
                // Move the last element's data.
                self.data.items[destroyed_value_index] = self.data.items[last_value_index];
                self.value_to_slot.items[destroyed_value_index] = self.value_to_slot.items[last_value_index];

                // Find out which slot the last element belonged to and update it.
                const last_elements_slot_index = self.value_to_slot.items[destroyed_value_index];
                self.slots.items[last_elements_slot_index] = .{ .occupied = destroyed_value_index };
            }

            // Shrink the dense arrays.
            _ = self.data.pop();
            _ = self.value_to_slot.pop();

            return out;
        }

        pub fn isValid(self: *const Self, ref: Ref) bool {
            const slot_index = ref.index;
            if (slot_index >= self.slots.items.len) return false;
            return self.generations.items[slot_index] == ref.generation and switch (self.slots.items[slot_index]) {
                .occupied => true,
                .free => false,
            };
        }

        pub fn get(self: *const Self, ref: Ref) ?*T {
            if (!self.isValid(ref)) return null;

            const slot_index = ref.index;

            return switch (self.slots.items[slot_index]) {
                .occupied => |value_index| &self.data.items[value_index],
                .free => null, // This ref points to a slot that has been freed.
            };
        }

        pub fn valueIndexToRef(self: *const Self, value_index: V) ?Ref {
            if (value_index >= self.len) return null;
            const slot_index = self.value_to_slot.items[value_index];
            return .{
                .index = slot_index,
                .generation = self.generations.items[slot_index],
            };
        }

        pub fn values(self: anytype) CopyPtrMutability(@TypeOf(self), .slice, T) {
            return self.data.items;
        }

        pub const RefIterator = struct {
            map: *const Self,
            length: usize,
            index: usize = 0,

            pub fn next(self: *RefIterator) ?Ref {
                if (self.index >= self.length) return null;
                const v_idx = self.index;
                self.index += 1;
                return self.map.valueIndexToRef(v_idx).?;
            }
        };

        pub fn refIterator(self: *const Self) RefIterator {
            return RefIterator{
                .map = self,
                .length = self.len,
            };
        }
    };
}

/// Same as `CopyPtrAttrs`, but does not copy the allowzero or alignment attributes.
pub fn CopyPtrMutability(
    comptime source: type,
    comptime size: std.builtin.Type.Pointer.Size,
    comptime child: type,
) type {
    const info = @typeInfo(source).pointer;
    return @Type(std.builtin.Type{
        .pointer = .{
            .size = size,
            .is_const = info.is_const,
            .is_volatile = info.is_volatile,
            .is_allowzero = false,
            .alignment = @alignOf(child),
            .address_space = info.address_space,
            .child = child,
            .sentinel_ptr = null,
        },
    });
}

const TestMap = SlotMap.new(u32);

test "basic lifecycle: create, get, remove" {
    const allocator = std.testing.allocator;
    var sm = TestMap.empty;
    defer sm.deinit(allocator);

    try std.testing.expect(sm.len == 0);

    // Create an element
    const create_res = try sm.create(allocator);
    const ref = create_res.@"0";
    const val_ptr = create_res.@"1";
    val_ptr.* = 123;

    try std.testing.expect(sm.len == 1);
    try std.testing.expect(ref.generation != .invalid);

    // Get the element
    const got_val_ptr = sm.get(ref) orelse return error.TestExpectedValue;
    try std.testing.expect(got_val_ptr.* == 123);

    // Remove the element
    const removed_val = sm.remove(ref) orelse return error.TestExpectedValue;
    try std.testing.expect(removed_val == 123);
    try std.testing.expect(sm.len == 0);

    // Ensure the old ref is now invalid
    try std.testing.expect(sm.get(ref) == null);
}

test "generation invalidation and reuse" {
    const allocator = std.testing.allocator;
    var sm = TestMap.empty;
    defer sm.deinit(allocator);

    // Create and remove an element to free up a slot
    const old_ref = (try sm.create(allocator)).@"0";
    _ = sm.remove(old_ref);

    // The old ref must be invalid now
    try std.testing.expect(sm.get(old_ref) == null);

    // Create a new element, which should reuse the slot
    const new_ref = (try sm.create(allocator)).@"0";

    // Critical check: The new ref must have the same index but a new, higher generation.
    // This prevents the "zombie entity" problem.
    try std.testing.expect(new_ref.index == old_ref.index);
    try std.testing.expect(@intFromEnum(new_ref.generation) > @intFromEnum(old_ref.generation));
    try std.testing.expect(!std.meta.eql(new_ref, old_ref));

    // The new ref must be valid
    try std.testing.expect(sm.get(new_ref) != null);
}

test "swap-and-pop behavior remains valid for swapped elements" {
    const allocator = std.testing.allocator;
    var sm = TestMap.empty;
    defer sm.deinit(allocator);

    const val_a = 10;
    const val_b = 20;
    const val_c = 30;

    const ref_a = (try sm.create(allocator)).@"0";
    (sm.get(ref_a).?.*) = val_a;
    const ref_b = (try sm.create(allocator)).@"0";
    (sm.get(ref_b).?.*) = val_b;
    const ref_c = (try sm.create(allocator)).@"0";
    (sm.get(ref_c).?.*) = val_c;

    try std.testing.expect(sm.len == 3);
    try std.testing.expectEqualSlices(u32, sm.values(), &.{ val_a, val_b, val_c });

    // Remove the *first* element. This will trigger a swap-and-pop,
    // moving element C into element A's old value slot.
    const removed_val = sm.remove(ref_a) orelse return error.TestExpectedValue;
    try std.testing.expect(removed_val == val_a);
    try std.testing.expect(sm.len == 2);

    // Critical check: The handle for the swapped element (C) must still be valid.
    const c_ptr = sm.get(ref_c) orelse return error.TestExpectedValue;
    try std.testing.expect(c_ptr.* == val_c);

    // The handle for B should also still be valid.
    const b_ptr = sm.get(ref_b) orelse return error.TestExpectedValue;
    try std.testing.expect(b_ptr.* == val_b);

    // The handle for the removed element (A) must be invalid.
    try std.testing.expect(sm.get(ref_a) == null);

    // Check the dense value array contents after the swap
    try std.testing.expectEqualSlices(u32, sm.values(), &.{ val_c, val_b });
}

test "remove last element (no-swap path)" {
    const allocator = std.testing.allocator;
    var sm = TestMap.empty;
    defer sm.deinit(allocator);

    const ref_a = (try sm.create(allocator)).@"0";
    (sm.get(ref_a).?.*) = 11;
    const ref_b = (try sm.create(allocator)).@"0";
    (sm.get(ref_b).?.*) = 22;

    // Remove the last element, which should not perform a swap.
    _ = sm.remove(ref_b);

    try std.testing.expect(sm.len == 1);
    // The handle for A must still be valid.
    try std.testing.expect(sm.get(ref_a).?.* == 11);
    // The handle for B must be invalid.
    try std.testing.expect(sm.get(ref_b) == null);
}

test "clear functionality" {
    const allocator = std.testing.allocator;
    var sm = TestMap.empty;
    defer sm.deinit(allocator);

    _ = try sm.create(allocator);
    _ = try sm.create(allocator);
    try std.testing.expect(sm.len == 2);

    sm.clear();
    try std.testing.expect(sm.len == 0);
    try std.testing.expect(sm.values().len == 0);

    // Should be able to add new items after clearing
    const ref = (try sm.create(allocator)).@"0";
    (sm.get(ref).?.*) = 99;
    try std.testing.expect(sm.len == 1);
    try std.testing.expect(sm.get(ref).?.* == 99);
}

test "invalid ref access" {
    const allocator = std.testing.allocator;
    var sm = TestMap.empty;
    defer sm.deinit(allocator);

    // Create one item to ensure arrays aren't empty
    const valid_ref = (try sm.create(allocator)).@"0";

    // 1. Ref with out-of-bounds index
    const bad_index_ref = Ref{ .index = 999, .generation = valid_ref.generation };
    try std.testing.expect(sm.get(bad_index_ref) == null);

    // 2. Ref with valid index but wrong generation
    const bad_gen_ref = Ref{ .index = valid_ref.index, .generation = @enumFromInt(999) };
    try std.testing.expect(sm.get(bad_gen_ref) == null);

    // 3. The globally invalid ref
    try std.testing.expect(sm.get(Ref.invalid) == null);
}
