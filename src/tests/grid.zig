const std = @import("std");
const log = std.log.scoped(.grid_test);

const Grid = @import("../Grid.zig");
const VoxemeCoord = Grid.VoxemeCoord;
const BufferCoord = Grid.BufferCoord;
const PageCoord = Grid.PageCoord;
const VoxelCoord = Grid.VoxelCoord;
const Voxeme = Grid.Voxeme;
const Voxel = Grid.Voxel;
const Vertex = Grid.Vertex;
const LiteVoxel = Grid.LiteVoxel;

test "coordinate conversions" {
    // Calculate the length of one side of a page, *not the total volume*
    const page_length_in_voxels: i32 = @intCast(Grid.page_length * Grid.voxeme_length);

    try std.testing.expectEqual(@as(VoxemeCoord, .{ 0, 0, 0 }), Grid.convert.voxelToVoxeme(.{ 0, 0, 0 }));
    try std.testing.expectEqual(@as(BufferCoord, .{ 0, 0, 0 }), Grid.convert.voxelToBuffer(.{ 0, 0, 0 }));

    try std.testing.expectEqual(@as(VoxemeCoord, .{ 0, 0, 0 }), Grid.convert.voxelToVoxeme(.{ 15, 5, 1 }));
    try std.testing.expectEqual(@as(BufferCoord, .{ 15, 5, 1 }), Grid.convert.voxelToBuffer(.{ 15, 5, 1 }));

    try std.testing.expectEqual(@as(VoxemeCoord, .{ 1, 2, 3 }), Grid.convert.voxelToVoxeme(.{ 16, 32, 48 }));
    try std.testing.expectEqual(@as(BufferCoord, .{ 0, 0, 0 }), Grid.convert.voxelToBuffer(.{ 16, 32, 48 }));

    // Test negative coordinates, the most common failure point
    try std.testing.expectEqual(@as(VoxemeCoord, .{ -1, -1, -1 }), Grid.convert.voxelToVoxeme(.{ -1, -1, -1 }));
    try std.testing.expectEqual(@as(BufferCoord, .{ 15, 15, 15 }), Grid.convert.voxelToBuffer(.{ -1, -1, -1 }));

    try std.testing.expectEqual(@as(VoxemeCoord, .{ -1, -1, -1 }), Grid.convert.voxelToVoxeme(.{ -16, -16, -16 }));
    try std.testing.expectEqual(@as(BufferCoord, .{ 0, 0, 0 }), Grid.convert.voxelToBuffer(.{ -16, -16, -16 }));

    try std.testing.expectEqual(@as(VoxemeCoord, .{ -2, -2, -2 }), Grid.convert.voxelToVoxeme(.{ -17, -17, -17 }));
    try std.testing.expectEqual(@as(BufferCoord, .{ 15, 15, 15 }), Grid.convert.voxelToBuffer(.{ -17, -17, -17 }));

    // Test voxel -> page conversions using the corrected length
    try std.testing.expectEqual(@as(PageCoord, .{ 0, 0, 0 }), Grid.convert.voxelToPage(.{ 0, 0, 0 }));
    try std.testing.expectEqual(@as(PageCoord, .{ 0, 0, 0 }), Grid.convert.voxelToPage(.{ page_length_in_voxels - 1, page_length_in_voxels - 1, page_length_in_voxels - 1 }));
    try std.testing.expectEqual(@as(PageCoord, .{ 1, 1, 1 }), Grid.convert.voxelToPage(.{ page_length_in_voxels, page_length_in_voxels, page_length_in_voxels }));
    try std.testing.expectEqual(@as(PageCoord, .{ -1, -1, -1 }), Grid.convert.voxelToPage(.{ -1, -1, -1 }));
    try std.testing.expectEqual(@as(PageCoord, .{ -2, -2, -2 }), Grid.convert.voxelToPage(.{ -page_length_in_voxels - 1, -page_length_in_voxels - 1, -page_length_in_voxels - 1 }));

    // Test buffer indexing
    try std.testing.expectEqual(@as(usize, 0), Grid.convert.bufferToIndex(.{ 0, 0, 0 }));
    try std.testing.expectEqual(@as(usize, 15), Grid.convert.bufferToIndex(.{ 15, 0, 0 }));
    try std.testing.expectEqual(@as(usize, 16), Grid.convert.bufferToIndex(.{ 0, 1, 0 }));
    try std.testing.expectEqual(@as(usize, 256), Grid.convert.bufferToIndex(.{ 0, 0, 1 }));
    try std.testing.expectEqual(@as(usize, Grid.voxels_per_voxeme - 1), Grid.convert.bufferToIndex(.{ 15, 15, 15 }));

    // Test world <-> voxel round trip
    const v_coord1: VoxelCoord = .{ 10, 20, 30 };
    const w_pos1 = Grid.convert.voxelToWorld(v_coord1, .center);
    const v_coord1_roundtrip = Grid.convert.worldToVoxel(w_pos1);
    try std.testing.expectEqual(v_coord1, v_coord1_roundtrip);

    const v_coord2: VoxelCoord = .{ -5, -80, 12 };
    const w_pos2 = Grid.convert.voxelToWorld(v_coord2, .min); // use min offset
    const v_coord2_roundtrip = Grid.convert.worldToVoxel(w_pos2);
    try std.testing.expectEqual(v_coord2, v_coord2_roundtrip);
}

test "end to end" {
    const gpa = std.testing.allocator;

    // --- Create Grid and Generate Mesh ---
    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();
    const frame_arena = arena_state.allocator();

    var grid = try Grid.init(gpa);
    defer grid.deinit();

    // --- DEFINE MATERIALS ---
    const stone_mat = try grid.bindMaterial(.{ .is_opaque = true, .color = .{ 0.5, 0.55, 0.6 } });
    const stone: Grid.LiteVoxel = .{ .material_id = stone_mat };

    // Define a second material for dirt
    const dirt_mat = try grid.bindMaterial(.{ .is_opaque = true, .color = .{ 0.6, 0.4, 0.2 } });
    const dirt: Grid.LiteVoxel = .{ .material_id = dirt_mat };

    // --- GENERATE A SPHERE ---
    const radius: i32 = 32;
    const radius_sq = @as(f32, @floatFromInt(radius * radius));
    {
        var z: i32 = -radius;
        while (z <= radius) : (z += 1) {
            var y: i32 = -radius;
            while (y <= radius) : (y += 1) {
                var x: i32 = -radius;
                while (x <= radius) : (x += 1) {
                    const xf = @as(f32, @floatFromInt(x));
                    const yf = @as(f32, @floatFromInt(y));
                    const zf = @as(f32, @floatFromInt(z));

                    // Check if the point is within the sphere's radius
                    if (xf * xf + yf * yf + zf * zf <= radius_sq) {
                        // Use dirt for the top half, stone for the bottom half
                        if (y <= 0) {
                            try grid.setVoxel(.{ x + radius, y + radius, z + radius }, stone);
                        } else {
                            try grid.setVoxel(.{ x + radius, y + radius, z + radius }, dirt);
                        }
                    }
                }
            }
        }
    }

    log.info("generated sphere with radius {d} voxels", .{radius});

    // Update the grid to calculate visibility, etc.
    try grid.update(frame_arena);

    log.info("grid update complete", .{});

    {
        var page_it = grid.pages.iterator();
        while (page_it.next()) |page_ptrs| {
            const page_pos = page_ptrs.key_ptr.*;
            const page = page_ptrs.value_ptr.*;

            log.info("page at {d}: hetero: {any}, homo: {any}", .{ page_pos, page.isHeterogeneous(), page.homogeneous });

            var voxeme_it = page.heterogeneous.iterator();
            while (voxeme_it.next()) |voxeme_ptrs| {
                const voxeme_pos = voxeme_ptrs.key_ptr.*;
                const voxeme_level_1 = voxeme_ptrs.value_ptr.*;

                log.info("  voxeme at {}: homo: {any}", .{ voxeme_pos, voxeme_level_1.is_homogeneous });

                if (voxeme_level_1.is_homogeneous) {
                    log.info("    {any} ", .{voxeme_level_1.payload.homogeneous});
                } else {
                    log.info("    skipping display of dense heterogeneous collection", .{});
                }
            }
        }
    }

    // Generate the mesh for the world containing our sphere
    var vertices = std.ArrayList(Grid.Vertex).empty;
    defer vertices.deinit(gpa);
    var indices = std.ArrayList(u32).empty;
    defer indices.deinit(gpa);

    try grid.worldMeshBasic(gpa, .{ 0, 0, 0 }, .{ 1, 1, 1 }, &vertices, &indices);

    log.info("generated mesh with {d} vertices and {d} indices", .{ vertices.items.len, indices.items.len });

    try std.testing.expectEqual(77016, vertices.items.len);
    try std.testing.expectEqual(115524, indices.items.len);
}
