const std = @import("std");
const log = std.log.scoped(.tests);

pub const grid = @import("tests/grid.zig");
pub const gpu_grid = @import("tests/gpu_grid.zig");

test {
    log.debug("semantic analysis for tests.zig", .{});
    std.testing.refAllDecls(@This());
}
