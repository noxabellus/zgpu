const std = @import("std");
const log = std.log.scoped(.tests);

pub const micro_grid = @import("tests/micro_grid.zig");

test {
    log.debug("semantic analysis for tests.zig", .{});
    std.testing.refAllDecls(@This());
}
