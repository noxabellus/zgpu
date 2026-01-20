const std = @import("std");
const log = std.log.scoped(.micro_grid_tests);

const linalg = @import("../linalg.zig");
const vec3 = linalg.vec3;
const vec3i = linalg.vec3i;
const vec3u = linalg.vec3u;

const Grid = @import("../GPUMicroGrid.zig");

test "GPUMicroGrid: create and destroy" {
    const allocator = std.testing.allocator;

    var grid = try Grid.init(allocator);
    defer grid.deinit();
}

test "GPUMicroGrid: basic commands" {
    const allocator = std.testing.allocator;

    var grid = try Grid.init(allocator);
    defer grid.deinit();

    try grid.runCommands(&.{
        .{ .set_page = .{
            .coord = vec3i{ 0, 0, 0 },
            .voxel = Grid.Voxel{
                .material_id = @enumFromInt(201),
                .state = 10,
            },
        } },
        .{ .set_voxeme = .{
            .page_coord = vec3i{ 0, 0, 0 },
            .voxeme_coord = vec3u{ 1, 2, 3 },
            .voxel = Grid.Voxel{
                .material_id = @enumFromInt(202),
                .state = 20,
            },
        } },
        .{ .set_voxel = .{
            .coord = vec3i{ 1, 2, 3 },
            .voxel = Grid.Voxel{
                .material_id = @enumFromInt(203),
                .state = 30,
            },
        } },
    });
}
