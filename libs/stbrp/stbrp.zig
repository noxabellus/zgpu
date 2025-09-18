const std = @import("std");
const log = std.log.scoped(.stbrp);

test {
    log.debug("semantic analysis for stbrp.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const Node = extern struct {
    x: i32 = 0,
    y: i32 = 0,
    next: ?*Node = null,
};

pub const Context = extern struct {
    width: i32 = 0,
    height: i32 = 0,
    @"align": i32 = 0,
    init_mode: i32 = 0,
    heuristic: i32 = 0,
    num_nodes: i32 = 0,
    active_head: ?*Node = null,
    free_head: ?*Node = null,
    extra: [2]Node = .{ .{}, .{} },
};

pub const Rect = extern struct {
    id: i32 = 0,
    w: i32 = 0,
    h: i32 = 0,
    x: i32 = 0,
    y: i32 = 0,
    was_packed: BigBool = .False,
};

pub const initTarget = @extern(*const fn (context: *Context, width: i32, height: i32, nodes: [*]Node, num_nodes: i32) callconv(.c) void, .{ .name = "stbrp_init_target" });
pub const packRects = @extern(*const fn (context: *Context, rects: [*]Rect, num_rects: i32) callconv(.c) i32, .{ .name = "stbrp_pack_rects" });
pub const setupAllowOutOfMem = @extern(*const fn (context: *Context, allow_out_of_mem: i32) callconv(.c) void, .{ .name = "stbrp_setup_allow_out_of_mem" });
pub const setupHeuristic = @extern(*const fn (context: *Context, heuristic: Heuristic) callconv(.c) void, .{ .name = "stbrp_setup_heuristic" });

pub const Heuristic = enum(i32) {
    default = 0,
    bl_sort_height = 0,
    bf_sort_height = 1,
};

pub const BigBool = enum(i32) {
    False,
    _,

    const True: BigBool = @enumFromInt(1);

    pub fn from(x: bool) BigBool {
        return if (x) .True else .False;
    }

    pub fn to(self: BigBool) bool {
        return switch (self) {
            .False => false,
            else => true,
        };
    }
};
