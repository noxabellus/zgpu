const std = @import("std");
const linalg = @import("../linalg.zig");
const vec3 = linalg.vec3;
const vec3i = linalg.vec3i;
const Voxel = Grid.Voxel;
const VoxemeTable = Grid.VoxemeTable;
const PageCoord = Grid.PageCoord;
const LocalCoord = Grid.LocalCoord;
const MaterialId = Grid.MaterialId;
const PageTable = Grid.PageTable;

const Grid = @import("../GpuGrid.zig");
const convert = Grid.convert;

// A helper function to make the validation reusable and clear.
fn expectInVoxelVolume(world_pos: vec3, global_voxel: vec3i) !void {
    const voxel_min_corner = convert.globalVoxelToWorld(global_voxel);
    const voxel_max_corner = convert.globalVoxelToWorld(global_voxel + @as(vec3i, @splat(1)));

    // Assert that the original world position is within the [min, max) volume of the converted voxel.
    inline for (.{ 0, 1, 2 }) |i| {
        try std.testing.expect(world_pos[i] >= voxel_min_corner[i]);
        try std.testing.expect(world_pos[i] < voxel_max_corner[i]);
    }
}

test "coordinate conversions round-trip" {
    // === Test Case 1: Negative Coordinates ===
    {
        const world_pos: vec3 = .{ -34.5, 17.2, 120.9 };

        // 1a. Top-down Conversion from world space
        const global_voxel = convert.worldToGlobalVoxel(world_pos);
        try std.testing.expectEqual(vec3i{ -552, 275, 1934 }, global_voxel);

        // 1b. *** NEW: Validate the world-space mapping ***
        try expectInVoxelVolume(world_pos, global_voxel);

        // 1c. Deconstruct into integer parts
        const page_coord = convert.globalVoxelToPageCoord(global_voxel);
        const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(global_voxel);
        const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);

        try std.testing.expectEqual(PageCoord{ .x = -3, .y = 1, .z = 7 }, page_coord);
        try std.testing.expectEqual(LocalCoord{ .x = 13, .y = 1, .z = 8 }, local_voxeme_coord);
        try std.testing.expectEqual(LocalCoord{ .x = 8, .y = 3, .z = 14 }, local_voxel_coord);

        // 1d. Bottom-up Reconstruction (integer round trip)
        const reconstructed_global_voxel = convert.partsToGlobalVoxel(page_coord, local_voxeme_coord, local_voxel_coord);
        try std.testing.expectEqual(global_voxel, reconstructed_global_voxel);
    }

    // === Test Case 2: Positive Coordinates ===
    {
        const world_pos: vec3 = .{ 552.1, 275.9, 1934.3 };

        const global_voxel = convert.worldToGlobalVoxel(world_pos);
        try std.testing.expectEqual(vec3i{ 8833, 4414, 30948 }, global_voxel);
        try expectInVoxelVolume(world_pos, global_voxel);

        const page_coord = convert.globalVoxelToPageCoord(global_voxel);
        const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(global_voxel);
        const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);

        try std.testing.expectEqual(PageCoord{ .x = 34, .y = 17, .z = 120 }, page_coord);
        try std.testing.expectEqual(LocalCoord{ .x = 8, .y = 3, .z = 14 }, local_voxeme_coord);
        try std.testing.expectEqual(LocalCoord{ .x = 1, .y = 14, .z = 4 }, local_voxel_coord);

        const reconstructed_global_voxel = convert.partsToGlobalVoxel(page_coord, local_voxeme_coord, local_voxel_coord);
        try std.testing.expectEqual(global_voxel, reconstructed_global_voxel);
    }

    // === Test Case 3: Edge Case at Origin ===
    {
        const world_pos: vec3 = .{ 0.0, 0.0, 0.0 };

        const global_voxel = convert.worldToGlobalVoxel(world_pos);
        try std.testing.expectEqual(vec3i{ 0, 0, 0 }, global_voxel);
        try expectInVoxelVolume(world_pos, global_voxel);

        const page_coord = convert.globalVoxelToPageCoord(global_voxel);
        const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(global_voxel);
        const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);

        try std.testing.expectEqual(PageCoord{ .x = 0, .y = 0, .z = 0 }, page_coord);
        try std.testing.expectEqual(LocalCoord{ .x = 0, .y = 0, .z = 0 }, local_voxeme_coord);
        try std.testing.expectEqual(LocalCoord{ .x = 0, .y = 0, .z = 0 }, local_voxel_coord);

        const reconstructed_global_voxel = convert.partsToGlobalVoxel(page_coord, local_voxeme_coord, local_voxel_coord);
        try std.testing.expectEqual(global_voxel, reconstructed_global_voxel);
    }

    // === Test Case 4: Edge Case at a Negative Boundary ===
    {
        // A point just barely in the negative space.
        const world_pos: vec3 = .{ -0.001, -0.001, -0.001 };

        const global_voxel = convert.worldToGlobalVoxel(world_pos);
        try std.testing.expectEqual(vec3i{ -1, -1, -1 }, global_voxel);
        try expectInVoxelVolume(world_pos, global_voxel);

        const page_coord = convert.globalVoxelToPageCoord(global_voxel);
        const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(global_voxel);
        const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);

        try std.testing.expectEqual(PageCoord{ .x = -1, .y = -1, .z = -1 }, page_coord);
        try std.testing.expectEqual(LocalCoord{ .x = 15, .y = 15, .z = 15 }, local_voxeme_coord);
        try std.testing.expectEqual(LocalCoord{ .x = 15, .y = 15, .z = 15 }, local_voxel_coord);

        const reconstructed_global_voxel = convert.partsToGlobalVoxel(page_coord, local_voxeme_coord, local_voxel_coord);
        try std.testing.expectEqual(global_voxel, reconstructed_global_voxel);
    }

    // === Test Case 5: localCoordToIndex (remains the same) ===
    {
        const index_coord = LocalCoord{ .x = 1, .y = 2, .z = 3 };
        // Expected: 1 + (2 * 16) + (3 * 256) = 1 + 32 + 768 = 801
        try std.testing.expectEqual(@as(u16, 801), convert.localVoxelCoordToIndex(index_coord));
    }
}

test "setVoxel in empty world creates all structures" {
    const gpa = std.testing.allocator;

    var grid = try Grid.init(gpa);
    defer grid.deinit();

    const stone_mat = try grid.registerMaterial(.{
        .color = .{ 0.5, 0.5, 0.5 },
        .flags = .{ .is_opaque = true },
    });

    // ARRANGE: The grid is empty.
    try std.testing.expectEqual(@as(usize, 0), grid.pageCount());
    try std.testing.expectEqual(@as(usize, 0), grid.voxemeCount());
    try std.testing.expectEqual(@as(usize, 0), grid.bufferCount());

    // ACT: Set a single voxel at the origin.
    const global_voxel = vec3i{ 0, 0, 0 };
    try grid.setVoxel(global_voxel, .{ .material_id = stone_mat });

    // ASSERT: Check that all structures were created correctly.
    try std.testing.expectEqual(@as(usize, 1), grid.pageCount());
    try std.testing.expectEqual(@as(usize, 1), grid.voxemeCount());
    try std.testing.expectEqual(@as(usize, 1), grid.bufferCount());

    // Assert Page was created and linked
    const page_coord = convert.globalVoxelToPageCoord(global_voxel);
    const page_hash_index = grid.page_indirection.lookup(page_coord);
    try std.testing.expect(page_hash_index != PageTable.sentinel);
    const page_index = grid.page_indirection.indices[page_hash_index];
    try std.testing.expectEqual(@as(u32, 0), page_index);

    // Assert Voxeme was created and linked
    const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(global_voxel);
    const voxeme_table = &grid.pages.items(.voxeme_indirection)[page_index];
    const voxeme_index = voxeme_table.lookup(local_voxeme_coord);
    try std.testing.expect(voxeme_index != VoxemeTable.sentinel);
    try std.testing.expectEqual(@as(u16, 0), voxeme_index);

    // Assert Buffer was created and linked
    const buffer_handle = grid.voxemes.items(.buffer_indirection)[voxeme_index];
    try std.testing.expect(buffer_handle != Grid.BufferData.sentinel);
    try std.testing.expectEqual(@as(u32, 0), buffer_handle);

    // Assert the Voxel data is correct
    const buffer = &grid.voxels.items(.voxel)[buffer_handle];
    const local_voxel_coord = convert.globalVoxelToLocalVoxelCoord(global_voxel);
    const index_in_buffer = convert.localVoxelCoordToIndex(local_voxel_coord);

    // The set voxel should be stone
    try std.testing.expectEqual(stone_mat, buffer[index_in_buffer].material_id);
    // A different voxel in the same buffer should be empty (from the page's original material)
    try std.testing.expectEqual(MaterialId.none, buffer[1].material_id);

    // Assert the page was marked dirty
    try std.testing.expect(grid.dirty_page_set.isSet(page_index));

    // Assert the voxeme was marked dirty
    const local_voxeme_idx = Grid.convert.localVoxemeCoordToIndex(local_voxeme_coord);
    const dirty_voxeme_index = @as(u64, page_index) * Grid.voxemes_per_page + local_voxeme_idx;
    try std.testing.expect(grid.dirty_voxeme_set.isSet(dirty_voxeme_index));
}

test "setVoxel breaks homogeneous voxeme" {
    const gpa = std.testing.allocator;

    var grid = try Grid.init(gpa);
    defer grid.deinit();

    const stone_mat = try grid.registerMaterial(.{
        .color = .{ 0.5, 0.5, 0.5 },
        .flags = .{ .is_opaque = true },
    });

    const dirt_mat = try grid.registerMaterial(.{
        .color = .{ 0.6, 0.4, 0.2 },
        .flags = .{ .is_opaque = true },
    });

    // ARRANGE: Create a state with a solid, homogeneous voxeme.
    // Easiest way is to set one voxel, then manually edit the grid state.
    try grid.setVoxel(vec3i{ 0, 0, 0 }, .{ .material_id = stone_mat });

    // Manually "homogenize" the voxeme to be solid stone.
    grid.voxemes.items(.voxel)[0] = Voxel{ .material_id = stone_mat, .state = 0 };
    grid.voxemes.items(.buffer_indirection)[0] = Grid.BufferData.sentinel;
    grid.voxels.clearRetainingCapacity(); // Clear the old buffer
    grid.dirty_page_set.unset(0); // Clear dirty flag

    try std.testing.expectEqual(@as(usize, 0), grid.bufferCount());
    const initial_voxeme_count = grid.voxemeCount();

    // ACT: Set a different voxel within the same voxeme to a new material.
    const breaking_voxel = vec3i{ 1, 1, 1 };
    try grid.setVoxel(breaking_voxel, .{ .material_id = dirt_mat });

    // ASSERT: Check that the break was handled correctly.
    try std.testing.expectEqual(initial_voxeme_count, grid.voxemeCount()); // No new voxeme created
    try std.testing.expectEqual(@as(usize, 1), grid.bufferCount()); // A new buffer was created

    const buffer_handle = grid.voxemes.items(.buffer_indirection)[0];
    try std.testing.expect(buffer_handle != Grid.BufferData.sentinel);

    const buffer = &grid.voxels.items(.voxel)[buffer_handle];

    // The new voxel should be dirt.
    const index_in_buffer = convert.localVoxelCoordToIndex(convert.globalVoxelToLocalVoxelCoord(breaking_voxel));
    try std.testing.expectEqual(dirt_mat, buffer[index_in_buffer].material_id);

    // A different voxel should still be the old homogeneous material (stone).
    try std.testing.expectEqual(stone_mat, buffer[0].material_id);

    // Assert the page was marked dirty
    try std.testing.expect(grid.dirty_page_set.isSet(0));

    // Assert the voxeme was marked dirty
    const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(breaking_voxel);
    const local_voxeme_idx = Grid.convert.localVoxemeCoordToIndex(local_voxeme_coord);
    const dirty_voxeme_index = local_voxeme_idx;
    try std.testing.expect(grid.dirty_voxeme_set.isSet(dirty_voxeme_index));
}

test "setVoxel modifies existing heterogeneous voxeme" {
    const gpa = std.testing.allocator;

    var grid = try Grid.init(gpa);
    defer grid.deinit();

    const stone_mat = try grid.registerMaterial(.{
        .color = .{ 0.5, 0.5, 0.5 },
        .flags = .{ .is_opaque = true },
    });

    const dirt_mat = try grid.registerMaterial(.{
        .color = .{ 0.6, 0.4, 0.2 },
        .flags = .{ .is_opaque = true },
    });

    // ARRANGE: Create a heterogeneous state by setting one voxel.
    try grid.setVoxel(vec3i{ 5, 5, 5 }, .{ .material_id = stone_mat });
    grid.dirty_page_set.unset(0); // Clear dirty flag for a clean test

    const initial_page_count = grid.pageCount();
    const initial_voxeme_count = grid.voxemeCount();
    const initial_buffer_count = grid.bufferCount();

    // ACT: Set the same voxel to a different material.
    try grid.setVoxel(vec3i{ 5, 5, 5 }, .{ .material_id = dirt_mat });

    // ASSERT: No new allocations should have occurred.
    try std.testing.expectEqual(initial_page_count, grid.pageCount());
    try std.testing.expectEqual(initial_voxeme_count, grid.voxemeCount());
    try std.testing.expectEqual(initial_buffer_count, grid.bufferCount());

    // Assert the voxel was correctly modified in the original buffer.
    const voxel = grid.getVoxel(.{ 5, 5, 5 });
    try std.testing.expectEqual(dirt_mat, voxel.material_id);

    // Assert the page was marked dirty
    try std.testing.expect(grid.dirty_page_set.isSet(0));

    // Assert the voxeme was marked dirty
    const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(vec3i{ 5, 5, 5 });
    const local_voxeme_idx = Grid.convert.localVoxemeCoordToIndex(local_voxeme_coord);
    const dirty_voxeme_index = local_voxeme_idx;
    try std.testing.expect(grid.dirty_voxeme_set.isSet(dirty_voxeme_index));
}

test "setVoxel no-op does not allocate or dirty" {
    const gpa = std.testing.allocator;

    var grid = try Grid.init(gpa);
    defer grid.deinit();

    const stone_mat = try grid.registerMaterial(.{
        .color = .{ 0.5, 0.5, 0.5 },
        .flags = .{ .is_opaque = true },
    });

    // ARRANGE: Create a state.
    try grid.setVoxel(vec3i{ 10, 10, 10 }, .{ .material_id = stone_mat });

    // Record initial state and clear dirty flags.
    const initial_page_count = grid.pageCount();
    const initial_voxeme_count = grid.voxemeCount();
    const initial_buffer_count = grid.bufferCount();

    const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(vec3i{ 10, 10, 10 });
    const local_voxeme_idx = Grid.convert.localVoxemeCoordToIndex(local_voxeme_coord);
    const dirty_voxeme_index = local_voxeme_idx;

    grid.dirty_page_set.unset(0);
    grid.dirty_voxeme_set.unset(dirty_voxeme_index);

    // ACT: Set the voxel to the exact same material and state.
    try grid.setVoxel(vec3i{ 10, 10, 10 }, .{ .material_id = stone_mat });

    // ASSERT: Nothing should have changed.
    try std.testing.expectEqual(initial_page_count, grid.pageCount());
    try std.testing.expectEqual(initial_voxeme_count, grid.voxemeCount());
    try std.testing.expectEqual(initial_buffer_count, grid.bufferCount());

    // Most importantly, the page should NOT be marked dirty.
    try std.testing.expect(!grid.dirty_page_set.isSet(0));

    // Assert the voxeme was not marked dirty
    try std.testing.expect(!grid.dirty_voxeme_set.isSet(dirty_voxeme_index));
}
