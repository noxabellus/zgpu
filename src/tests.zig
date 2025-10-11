const std = @import("std");
const log = std.log.scoped(.tests);

pub const grid = @import("tests/grid.zig");

test {
    log.debug("semantic analysis for tests.zig", .{});
    std.testing.refAllDecls(@This());
}
