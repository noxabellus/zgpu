//! An abstraction layer for rendering *interactive* UI layouts based on the clay layout engine, rendered with our Batch2D library.

const Ui = @This();

const std = @import("std");
const clay = @import("clay");
const glfw = @import("glfw");

const Batch2D = @import("Batch2D.zig");
const AssetCache = @import("AssetCache.zig");
const InputState = @import("InputState.zig");
const ClayBackend = @import("ClayBackend.zig");

const log = std.log.scoped(.ui);

test {
    log.debug("semantic analysis for Ui.zig", .{});
    std.testing.refAllDecls(@This());
}

allocator: std.mem.Allocator,
backend: *ClayBackend,
clay_context: *clay.Context,
clay_memory: []const u8,
render_commands: ?[]clay.RenderCommand = null,
inputs: InputState = .{},

pub fn init(allocator: std.mem.Allocator, renderer: *Batch2D, asset_cache: *AssetCache) !Ui {
    // Init Clay
    const min_memory_size = clay.minMemorySize();
    const clay_memory = try allocator.alloc(u8, min_memory_size);
    errdefer allocator.free(clay_memory);

    const clay_arena = clay.createArenaWithCapacityAndMemory(clay_memory);
    const clay_context = clay.initialize(
        clay_arena,
        clay.Dimensions{ .w = 0, .h = 0 },
        clay.ErrorHandler{
            .error_handler_function = reportClayError,
            .user_data = null,
        },
    );
    defer clay.setCurrentContext(null);

    const backend = try ClayBackend.init(allocator, renderer, asset_cache);

    return Ui{
        .allocator = allocator,
        .backend = backend,
        .clay_context = clay_context,
        .clay_memory = clay_memory,
    };
}

pub fn deinit(self: *Ui) void {
    self.allocator.free(self.clay_memory);
    self.backend.deinit(self.allocator);
    self.* = undefined;
}

/// Call this at any time in the update stage to begin declaring the ui layout. Caller must ensure `Ui.endLayout` is called before the next `Ui.beginLayout` and/or `Ui.render`.
/// Note: it is acceptable to run the layout code multiple times per frame if needed, but any queued render commands will be discarded when calling this function.
pub fn beginLayout(self: *Ui, dimensions: Batch2D.Vec2, mouse_down: bool, mouse_position: ?Batch2D.Vec2, scroll_delta: Batch2D.Vec2, delta_ms: f32) void {
    self.render_commands = null; // Discard any existing render commands

    clay.setCurrentContext(self.clay_context);

    if (mouse_position) |pos| {
        clay.setPointerState(vec2ToClay(pos), mouse_down);
    } else {
        clay.setPointerState(.{ .x = -1, .y = -1 }, false);
    }

    clay.updateScrollContainers(false, vec2ToClay(scroll_delta), delta_ms);

    clay.setLayoutDimensions(vec2ToDims(dimensions));

    clay.beginLayout();
}

pub fn endLayout(self: *Ui) void {
    const render_commands = clay.endLayout();
    self.render_commands = render_commands;

    clay.setCurrentContext(null);
}

/// Must call this between `Batch2D.beginFrame()` and `Batch2D.endFrame()`.
pub fn render(self: *Ui) !void {
    clay.setCurrentContext(self.clay_context);
    defer clay.setCurrentContext(null);

    if (self.render_commands) |commands| {
        try self.backend.render(commands);
    } else {
        log.warn("Ui.render called without any render commands (did you forget to call Ui.beginLayout/Ui.endLayout?)", .{});
    }
}

// --- Helper functions ---

fn vec2FromClay(vec: clay.Vector2) Batch2D.Vec2 {
    return .{ .x = vec.x, .y = vec.y };
}

fn vec2ToClay(vec: Batch2D.Vec2) clay.Vector2 {
    return .{ .x = vec.x, .y = vec.y };
}

fn vec2FromDims(dims: clay.Dimensions) Batch2D.Vec2 {
    return .{ .x = dims.w, .y = dims.h };
}

fn vec2ToDims(vec: Batch2D.Vec2) clay.Dimensions {
    return .{ .w = vec.x, .h = vec.y };
}

fn reportClayError(data: clay.ErrorData) callconv(.c) void {
    std.log.scoped(.clay).err("{s} - {s}", .{ @tagName(data.error_type), data.error_text.toSlice() });
}
