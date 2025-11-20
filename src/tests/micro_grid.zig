const std = @import("std");
const log = std.log.scoped(.micro_grid_tests);

const linalg = @import("../linalg.zig");
const vec3 = linalg.vec3;
const vec3i = linalg.vec3i;
const Voxel = Grid.Voxel;
const VoxemeTable = Grid.VoxemeTable;
const MaterialId = Grid.MaterialId;
const PageTable = Grid.PageTable;

const Grid = @import("../MicroGrid.zig");
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

        try std.testing.expectEqual(vec3i{ -3, 1, 7 }, page_coord);
        try std.testing.expectEqual(vec3i{ 13, 1, 8 }, local_voxeme_coord);
        try std.testing.expectEqual(vec3i{ 8, 3, 14 }, local_voxel_coord);

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

        try std.testing.expectEqual(vec3i{ 34, 17, 120 }, page_coord);
        try std.testing.expectEqual(vec3i{ 8, 3, 14 }, local_voxeme_coord);
        try std.testing.expectEqual(vec3i{ 1, 14, 4 }, local_voxel_coord);

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

        try std.testing.expectEqual(vec3i{ 0, 0, 0 }, page_coord);
        try std.testing.expectEqual(vec3i{ 0, 0, 0 }, local_voxeme_coord);
        try std.testing.expectEqual(vec3i{ 0, 0, 0 }, local_voxel_coord);

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

        try std.testing.expectEqual(vec3i{ -1, -1, -1 }, page_coord);
        try std.testing.expectEqual(vec3i{ 15, 15, 15 }, local_voxeme_coord);
        try std.testing.expectEqual(vec3i{ 15, 15, 15 }, local_voxel_coord);

        const reconstructed_global_voxel = convert.partsToGlobalVoxel(page_coord, local_voxeme_coord, local_voxel_coord);
        try std.testing.expectEqual(global_voxel, reconstructed_global_voxel);
    }

    // === Test Case 5: localCoordToIndex (remains the same) ===
    {
        const index_coord = vec3i{ 1, 2, 3 };
        // Expected: 1 + (2 * 16) + (3 * 256) = 1 + 32 + 768 = 801
        try std.testing.expectEqual(@as(u16, 801), convert.localVoxelCoordToIndex(index_coord));
    }
}

test "Axis and offsets correctness" {
    // 1. Verify the static offsets array is correct.
    try std.testing.expectEqual(vec3i{ 1, 0, 0 }, Grid.offsets[0]); // +X
    try std.testing.expectEqual(vec3i{ -1, 0, 0 }, Grid.offsets[1]); // -X
    try std.testing.expectEqual(vec3i{ 0, 1, 0 }, Grid.offsets[2]); // +Y
    try std.testing.expectEqual(vec3i{ 0, -1, 0 }, Grid.offsets[3]); // -Y
    try std.testing.expectEqual(vec3i{ 0, 0, 1 }, Grid.offsets[4]); // +Z
    try std.testing.expectEqual(vec3i{ 0, 0, -1 }, Grid.offsets[5]); // -Z

    // 2. Test Axis enum conversions
    try std.testing.expectEqual(@as(u3, 0), Grid.Axis.x.toIndex());
    try std.testing.expectEqual(@as(u3, 2), Grid.Axis.y.toIndex());
    try std.testing.expectEqual(@as(u3, 4), Grid.Axis.z.toIndex());

    try std.testing.expectEqual(Grid.Axis.x, Grid.Axis.fromIndex(0));
    try std.testing.expectEqual(Grid.Axis.x, Grid.Axis.fromIndex(1)); // LSB is ignored for direction
    try std.testing.expectEqual(Grid.Axis.y, Grid.Axis.fromIndex(2));
    try std.testing.expectEqual(Grid.Axis.y, Grid.Axis.fromIndex(3));
    try std.testing.expectEqual(Grid.Axis.z, Grid.Axis.fromIndex(4));
    try std.testing.expectEqual(Grid.Axis.z, Grid.Axis.fromIndex(5));

    // 3. Test AxisDir struct conversions and helpers
    const dirs = [_]Grid.AxisDir{
        .{ .axis = .x, .positive = true }, // 0
        .{ .axis = .x, .positive = false }, // 1
        .{ .axis = .y, .positive = true }, // 2
        .{ .axis = .y, .positive = false }, // 3
        .{ .axis = .z, .positive = true }, // 4
        .{ .axis = .z, .positive = false }, // 5
    };

    inline for (dirs, 0..) |dir, i| {
        const index: u3 = @intCast(i);
        // Test toIndex()
        try std.testing.expectEqual(index, dir.toIndex());
        // Test fromIndex() round-trip
        try std.testing.expectEqual(dir, Grid.AxisDir.fromIndex(index));
        // Test getOffset()
        try std.testing.expectEqual(Grid.offsets[index], dir.getOffset());
    }
}

test "setVoxel in empty world creates all structures" {
    const old_log_level = std.testing.log_level;
    defer std.testing.log_level = old_log_level;
    std.testing.log_level = .debug;

    const gpa = std.heap.page_allocator;

    var grid = try Grid.init(gpa);
    defer grid.deinit();

    const stone_mat = try grid.registerMaterial(.{
        .color = .grey,
        .flags = .{ .is_opaque = true },
    });

    // ARRANGE: The grid is empty.
    try std.testing.expectEqual(@as(usize, 0), grid.pageCount());
    try std.testing.expectEqual(@as(usize, 0), grid.voxemeCount());
    try std.testing.expectEqual(@as(usize, 0), grid.bufferCount());

    // ACT: Set a single voxel at the origin.
    const global_voxel = vec3i{ 0, 0, 0 };
    try grid.applyCommands(&[_]Grid.Command{
        .{ .set_voxel = .{ .global_voxel = global_voxel, .voxel = .{ .material_id = stone_mat } } },
    });

    // ASSERT: Check that all structures were created correctly.
    try std.testing.expectEqual(@as(usize, 1), grid.pageCount());
    try std.testing.expectEqual(@as(usize, 1), grid.voxemeCount());
    try std.testing.expectEqual(@as(usize, 1), grid.bufferCount());

    // Assert Page was created and linked
    const page_coord = convert.globalVoxelToPageCoord(global_voxel);
    const page_index = grid.page_indirection.lookup(page_coord);
    try std.testing.expect(page_index != Grid.sentinel_index);
    try std.testing.expectEqual(@as(u32, 0), page_index);

    // Assert Voxeme was created and linked
    const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(global_voxel);
    const voxeme_table = &grid.pages.items(.voxeme_indirection)[page_index];
    const voxeme_index = voxeme_table.lookup(local_voxeme_coord);
    try std.testing.expect(voxeme_index != Grid.sentinel_index);
    try std.testing.expectEqual(@as(u16, 0), voxeme_index);

    // Assert Buffer was created and linked
    const buffer_handle = grid.voxemes.items(.buffer_indirection)[voxeme_index];
    try std.testing.expect(buffer_handle != Grid.sentinel_index);
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
    try std.testing.expect(grid.pages.items(.dirty_voxeme_set)[page_index].isSet(local_voxeme_idx));
}

test "setVoxel breaks homogeneous voxeme" {
    const gpa = std.testing.allocator;

    var grid = try Grid.init(gpa);
    defer grid.deinit();

    const stone_mat = try grid.registerMaterial(.{
        .color = .grey,
        .flags = .{ .is_opaque = true },
    });

    const dirt_mat = try grid.registerMaterial(.{
        .color = .{ .r = 0.6 * 255, .g = 0.4 * 255, .b = 0.2 * 255 },
        .flags = .{ .is_opaque = true },
    });

    // ARRANGE: Create a state with a solid, homogeneous voxeme.
    // Easiest way is to set one voxel, then manually edit the grid state.
    try grid.applyCommands(&[_]Grid.Command{
        .{ .set_voxel = .{ .global_voxel = .{ 0, 0, 0 }, .voxel = .{ .material_id = stone_mat } } },
    });

    // Manually "homogenize" the voxeme to be solid stone.
    grid.voxemes.items(.voxel)[0] = Voxel{ .material_id = stone_mat, .state = 0 };
    grid.voxemes.items(.buffer_indirection)[0] = Grid.sentinel_index;
    grid.voxels.clearRetainingCapacity(); // Clear the old buffer
    grid.dirty_page_set.unset(0); // Clear dirty flag

    try std.testing.expectEqual(@as(usize, 0), grid.bufferCount());
    const initial_voxeme_count = grid.voxemeCount();

    // ACT: Set a different voxel within the same voxeme to a new material.
    const breaking_voxel = vec3i{ 1, 1, 1 };
    try grid.applyCommands(&[_]Grid.Command{
        .{ .set_voxel = .{ .global_voxel = breaking_voxel, .voxel = .{ .material_id = dirt_mat } } },
    });

    // ASSERT: Check that the break was handled correctly.
    try std.testing.expectEqual(initial_voxeme_count, grid.voxemeCount()); // No new voxeme created
    try std.testing.expectEqual(@as(usize, 1), grid.bufferCount()); // A new buffer was created

    const buffer_handle = grid.voxemes.items(.buffer_indirection)[0];
    try std.testing.expect(buffer_handle != Grid.sentinel_index);

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
    try std.testing.expect(grid.pages.items(.dirty_voxeme_set)[0].isSet(local_voxeme_idx));
}

test "setVoxel modifies existing heterogeneous voxeme" {
    const gpa = std.testing.allocator;

    var grid = try Grid.init(gpa);
    defer grid.deinit();

    const stone_mat = try grid.registerMaterial(.{
        .color = .grey,
        .flags = .{ .is_opaque = true },
    });

    const dirt_mat = try grid.registerMaterial(.{
        .color = .{ .r = 0.6 * 255, .g = 0.4 * 255, .b = 0.2 * 255 },
        .flags = .{ .is_opaque = true },
    });

    // ARRANGE: Create a heterogeneous state by setting one voxel.
    try grid.applyCommands(&[_]Grid.Command{
        .{ .set_voxel = .{ .global_voxel = .{ 5, 5, 5 }, .voxel = .{ .material_id = stone_mat } } },
    });
    grid.dirty_page_set.unset(0); // Clear dirty flag for a clean test

    const initial_page_count = grid.pageCount();
    const initial_voxeme_count = grid.voxemeCount();
    const initial_buffer_count = grid.bufferCount();

    // ACT: Set the same voxel to a different material.
    try grid.applyCommands(&[_]Grid.Command{
        .{ .set_voxel = .{ .global_voxel = .{ 5, 5, 5 }, .voxel = .{ .material_id = dirt_mat } } },
    });

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
    try std.testing.expect(grid.pages.items(.dirty_voxeme_set)[0].isSet(local_voxeme_idx));
}

test "setVoxel no-op does not allocate or dirty" {
    const gpa = std.testing.allocator;

    var grid = try Grid.init(gpa);
    defer grid.deinit();

    const stone_mat = try grid.registerMaterial(.{
        .color = .grey,
        .flags = .{ .is_opaque = true },
    });

    // ARRANGE: Create a state.
    try grid.applyCommands(&[_]Grid.Command{
        .{ .set_voxel = .{ .global_voxel = .{ 10, 10, 10 }, .voxel = .{ .material_id = stone_mat } } },
    });

    // Record initial state and clear dirty flags.
    const initial_page_count = grid.pageCount();
    const initial_voxeme_count = grid.voxemeCount();
    const initial_buffer_count = grid.bufferCount();

    const local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(vec3i{ 10, 10, 10 });
    const local_voxeme_idx = Grid.convert.localVoxemeCoordToIndex(local_voxeme_coord);

    grid.dirty_page_set.unset(0);
    grid.pages.items(.dirty_voxeme_set)[0].unsetAll();

    // ACT: Set the voxel to the exact same material and state.
    try grid.applyCommands(&[_]Grid.Command{
        .{ .set_voxel = .{ .global_voxel = .{ 10, 10, 10 }, .voxel = .{ .material_id = stone_mat } } },
    });

    // ASSERT: Nothing should have changed.
    try std.testing.expectEqual(initial_page_count, grid.pageCount());
    try std.testing.expectEqual(initial_voxeme_count, grid.voxemeCount());
    try std.testing.expectEqual(initial_buffer_count, grid.bufferCount());

    // Most importantly, the page should NOT be marked dirty.
    try std.testing.expect(!grid.dirty_page_set.isSet(0));

    // Assert the voxeme was not marked dirty
    try std.testing.expect(!grid.pages.items(.dirty_voxeme_set)[0].isSet(local_voxeme_idx));
}

test "setVoxel dirties neighbors across page boundary" {
    const gpa = std.testing.allocator;

    var grid = try Grid.init(gpa);
    defer grid.deinit();

    const stone_mat = try grid.registerMaterial(.{
        .color = .grey,
        .flags = .{ .is_opaque = true },
    });

    const dirt_mat = try grid.registerMaterial(.{
        .color = .yellow,
        .flags = .{ .is_opaque = true },
    });

    // ARRANGE: Create two adjacent pages by setting a voxel in each,
    // right on the boundary we want to test.
    // A page is 256x256x256 voxels.
    // Voxel at {255, 0, 0} is the last voxel in Page {0,0,0} along the +X axis.
    const primary_coord = vec3i{ 255, 0, 0 };
    // Voxel at {256, 0, 0} is the first voxel in Page {1,0,0} along the +X axis.
    const neighbor_coord = vec3i{ 256, 0, 0 };

    // Create the initial structures.
    try grid.applyCommands(&[_]Grid.Command{
        .{ .set_voxel = .{ .global_voxel = primary_coord, .voxel = .{ .material_id = stone_mat } } },
        .{ .set_voxel = .{ .global_voxel = neighbor_coord, .voxel = .{ .material_id = stone_mat } } },
    });

    // --- Get indices for the primary voxel's containers ---
    const primary_page_coord = convert.globalVoxelToPageCoord(primary_coord);
    const primary_page_index = grid.page_indirection.lookup(primary_page_coord);
    try std.testing.expect(primary_page_index != Grid.sentinel_index);

    const primary_local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(primary_coord);
    const primary_dirty_voxeme_idx = convert.localVoxemeCoordToIndex(primary_local_voxeme_coord);

    // --- Get indices for the neighbor voxel's containers ---
    const neighbor_page_coord = convert.globalVoxelToPageCoord(neighbor_coord);
    const neighbor_page_index = grid.page_indirection.lookup(neighbor_page_coord);
    try std.testing.expect(neighbor_page_index != Grid.sentinel_index);
    try std.testing.expect(primary_page_index != neighbor_page_index); // Ensure they are in different pages

    const neighbor_local_voxeme_coord = convert.globalVoxelToLocalVoxemeCoord(neighbor_coord);
    const neighbor_dirty_voxeme_idx = convert.localVoxemeCoordToIndex(neighbor_local_voxeme_coord);

    // --- Clear dirty flags to ensure a clean test state ---
    grid.dirty_page_set.unsetAll();
    grid.pages.items(.dirty_voxeme_set)[primary_page_index].unsetAll();
    grid.pages.items(.dirty_voxeme_set)[neighbor_page_index].unsetAll();

    // Verify clean state
    try std.testing.expect(!grid.dirty_page_set.isSet(primary_page_index));
    try std.testing.expect(!grid.dirty_page_set.isSet(neighbor_page_index));
    try std.testing.expect(!grid.pages.items(.dirty_voxeme_set)[primary_page_index].isSet(primary_dirty_voxeme_idx));
    try std.testing.expect(!grid.pages.items(.dirty_voxeme_set)[neighbor_page_index].isSet(neighbor_dirty_voxeme_idx));

    // ACT: Set the primary voxel, which should trigger dirtying its neighbors.
    try grid.applyCommands(&[_]Grid.Command{
        .{ .set_voxel = .{ .global_voxel = primary_coord, .voxel = .{ .material_id = dirt_mat } } },
    });

    // ASSERT:
    // 1. The primary page and voxeme should be dirty.
    try std.testing.expect(grid.dirty_page_set.isSet(primary_page_index));
    try std.testing.expect(grid.pages.items(.dirty_voxeme_set)[primary_page_index].isSet(primary_dirty_voxeme_idx));

    // 2. The neighbor page and voxeme across the boundary should also be dirty.
    try std.testing.expect(grid.dirty_page_set.isSet(neighbor_page_index));
    try std.testing.expect(grid.pages.items(.dirty_voxeme_set)[neighbor_page_index].isSet(neighbor_dirty_voxeme_idx));

    // 3. A neighbor in a direction that doesn't cross a page boundary (-X) should
    // still dirty its containing page (which is the primary page).
    const neg_x_neighbor_coord = primary_coord - vec3i{ 1, 0, 0 }; // {254,0,0}
    const neg_x_page_coord = convert.globalVoxelToPageCoord(neg_x_neighbor_coord);
    try std.testing.expectEqual(primary_page_coord, neg_x_page_coord); // Same page

    // 4. A neighbor in a direction where the page doesn't exist (+Y) should
    // not cause a crash or dirty anything extra. The total number of dirty pages
    // should be 2: the primary page and the existing neighbor page.
    try std.testing.expectEqual(@as(usize, 2), grid.dirty_page_set.count());
}

test "manager creates sphere mesh across four pages" {
    // --- Create Grid and Generate Mesh ---
    var arena_state = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_state.deinit();
    // const frame_arena = arena_state.allocator();

    std.log.debug("Starting sphere mesh test...", .{});

    // 1. Initialize the manager and its dependencies
    var pool: std.Thread.Pool = undefined;
    try pool.init(.{ .allocator = std.testing.allocator, .n_jobs = 4 });
    defer pool.deinit();
    const grid = try Grid.init(std.testing.allocator);
    // Note: We don't defer grid.deinit() here because manager.deinit() returns ownership to us.

    var manager = try Grid.Manager.init(grid, &pool);

    // 2. Define the sphere material and voxel state
    const solid_mat = try manager.front().grid.registerMaterial(.{ .color = .red, .flags = .{ .is_opaque = true } });
    const sphere_voxel = Grid.Voxel{ .material_id = solid_mat };

    // 3. Define the sphere geometry.
    // The boundary between pages is at global voxel coordinates that are multiples of
    // (page_axis_divisor * voxeme_axis_divisor) = 16 * 16 = 256.
    // Placing the center at (256, 256, 128) will make it perfectly intersect the corner
    // of pages (0,0,0), (1,0,0), (0,1,0), and (1,1,0).
    const center_voxel = vec3i{ 256, 256, 128 };
    const radius = 10;
    const radius_sq = radius * radius;

    const min_bound = center_voxel - @as(vec3i, @splat(radius));
    const max_bound = center_voxel + @as(vec3i, @splat(radius));

    // 4. Queue commands to create the sphere
    var z = min_bound[2];
    while (z <= max_bound[2]) : (z += 1) {
        var y = min_bound[1];
        while (y <= max_bound[1]) : (y += 1) {
            var x = min_bound[0];
            while (x <= max_bound[0]) : (x += 1) {
                const current_voxel = vec3i{ x, y, z };
                const offset = current_voxel - center_voxel;
                const dist_sq = linalg.len_sq(offset);

                if (dist_sq <= radius_sq) {
                    try manager.queueCommand(.{ .set_voxel = .{
                        .global_voxel = current_voxel,
                        .voxel = sphere_voxel,
                    } });
                }
            }
        }
    }

    std.log.debug("Queued sphere voxel commands.", .{});

    // 5. Signal the manager to process the commands and wait for completion.
    // manager.deinit() joins the worker thread, ensuring all work is finished.
    std.log.debug("Signaling manager to process queued commands...", .{});
    manager.endFrame();
    std.log.debug("Manager signaled 1; sleeping ..", .{});
    std.Thread.sleep(std.time.ns_per_s);
    std.log.debug("Waking up and signaling manager to swap buffers again", .{});
    manager.endFrame();
    std.log.debug("Manager signaled 2; front buffer should contain meshes", .{});
    defer {
        std.log.debug("Deinitializing manager...", .{});
        var final_state = manager.deinit();
        std.log.debug("Manager deinitialized, final state obtained.", .{});
        final_state.deinit(); // Clean up the final grid and mesh cache
    }

    const mesh_cache = manager.front().mesh_cache;

    // 6. Validate that the correct pages have generated meshes.
    const affected_pages = [_]vec3i{
        Grid.convert.globalVoxelToPageCoord(.{ 255, 255, 128 }), // Page (0,0,0)
        Grid.convert.globalVoxelToPageCoord(.{ 256, 255, 128 }), // Page (1,0,0)
        Grid.convert.globalVoxelToPageCoord(.{ 255, 256, 128 }), // Page (0,1,0)
        Grid.convert.globalVoxelToPageCoord(.{ 256, 256, 128 }), // Page (1,1,0)
    };

    // Sanity check our coordinate math
    try std.testing.expectEqual(vec3i{ 0, 0, 0 }, affected_pages[0]);
    try std.testing.expectEqual(vec3i{ 1, 0, 0 }, affected_pages[1]);
    try std.testing.expectEqual(vec3i{ 0, 1, 0 }, affected_pages[2]);
    try std.testing.expectEqual(vec3i{ 1, 1, 0 }, affected_pages[3]);

    try std.testing.expectEqual(4, mesh_cache.meshes.len);

    for (affected_pages) |coord| {
        const view = mesh_cache.getView(coord);
        try std.testing.expect(view != null);
        if (view) |v| {
            std.log.debug("Found mesh for page {any}: indices={}", .{ coord, v.index_count });
            try std.testing.expect(v.index_count > 0);
        }
    }

    // 7. Validate that an unaffected page has no mesh.
    std.log.debug("Checking for no mesh in unaffected page...", .{});
    const unaffected_page = vec3i{ 5, 5, 5 };
    try std.testing.expect(mesh_cache.getView(unaffected_page) == null);

    try std.testing.expectEqual(7608, mesh_cache.vertices.len);
    try std.testing.expectEqual(11412, mesh_cache.indices.items.len);

    std.log.debug("Sphere mesh test passed.", .{});
}
