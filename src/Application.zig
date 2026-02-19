const Application = @This();

const std = @import("std");
const builtin = @import("builtin");
const linalg = @import("linalg.zig");
const glfw = @import("glfw");
const wgpu = @import("wgpu");
const stbi = @import("stbi");

const Gpu = @import("Gpu.zig");

const log = std.log.scoped(.app);

window: *glfw.Window,
user_data: *anyopaque,
gpu: Gpu,

pub fn init(gpa: std.mem.Allocator, name: [*:0]const u8) !*Application {
    log.info("application initializing...", .{});
    var timer = try std.time.Timer.start();

    stbi.init(gpa);
    errdefer stbi.deinit();

    log.info("stbi started", .{});

    if (comptime builtin.os.tag != .windows) {
        log.info("initializing glfw for x11 ...", .{});
        glfw.initHint(.{ .platform = .x11 });
    } else {
        log.info("initializing glfw for win32 ...", .{});
        glfw.initHint(.{ .platform = .win32 });
    }

    try glfw.init();
    errdefer glfw.deinit();

    log.info("glfw started", .{});

    var self = try gpa.create(Application);
    @memset(std.mem.asBytes(self), 0);

    log.info("created application structure allocation", .{});

    glfw.windowHint(.{ .client_api = .none });
    glfw.windowHint(.{ .resizable = true });

    self.window = try glfw.createWindow(800, 600, name, null, null);
    errdefer glfw.destroyWindow(self.window);

    glfw.setWindowUserPointer(self.window, self);

    log.info("created glfw window", .{});

    try self.gpu.init();

    const elapsed = timer.read();
    const ms = @as(f64, @floatFromInt(elapsed)) / std.time.ns_per_ms;

    log.info("application initialized in {d:.3}ms", .{ms});

    return self;
}

pub fn deinit(self: *Application) void {
    log.info("application shutting down ...", .{});

    self.gpu.deinit();

    glfw.destroyWindow(self.window);

    glfw.deinit();
    stbi.deinit();

    self.* = undefined;

    log.info("application shutdown completed successfully", .{});
}
