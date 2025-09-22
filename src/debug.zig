const std = @import("std");
const Batch2D = @import("Batch2D.zig");

const FRAME_AVG_LEN = 144;
const FRAME_TIME_BASE_MS = 1000.0 / 144.0;
const FRAME_TIME_BASE_NS = FRAME_TIME_BASE_MS * @as(comptime_float, std.time.ns_per_ms); // Start with a base of 6ms

pub var frame_time: u64 = @as(comptime_int, @intFromFloat(FRAME_TIME_BASE_NS));
pub var frame_ms: f64 = 6.0;
pub var frame_ms_buf: [FRAME_AVG_LEN]f64 = [1]f64{6.0} ** FRAME_AVG_LEN;
pub var frame_index: usize = 0;

pub var frame_fps: f64 = 144.0;

pub var avg_ms: f64 = FRAME_TIME_BASE_MS;
pub var min_ms: f64 = std.math.inf(f64);
pub var max_ms: f64 = -std.math.inf(f64);
pub var avg_fps: f64 = 144.0;

var timer: *std.time.Timer = undefined;

pub fn start(user_timer: *std.time.Timer) f64 {
    timer = user_timer;
    const startup_time = timer.lap();
    return @as(f64, @floatFromInt(startup_time)) / std.time.ns_per_ms;
}

pub fn lap() void {
    frame_time = timer.lap();
    frame_ms = @as(f64, @floatFromInt(frame_time)) / std.time.ns_per_ms;
    frame_fps = 1000.0 / frame_ms;

    // Update rolling average
    frame_ms_buf[frame_index % FRAME_AVG_LEN] = frame_ms;
    frame_index += 1;

    min_ms = std.math.inf(f64);
    max_ms = -std.math.inf(f64);
    avg_ms = 0.0;

    for (frame_ms_buf) |ms| {
        min_ms = @min(min_ms, ms);
        max_ms = @max(max_ms, ms);
        avg_ms += ms;
    }

    avg_ms /= @as(f64, FRAME_AVG_LEN);
}

/// Draws a bar chart for all frames in the buffer, with each bar scaled by its size relative to the max frame time
pub fn drawFpsChart(renderer: *Batch2D, chart_pos: Batch2D.Vec2) !void {
    const bar_width: f32 = 4.0;
    const chart_height: f32 = 100.0;
    const chart_color: Batch2D.Color = .{ .r = 0, .g = 0, .b = 0, .a = 0.5 };

    const reference_ms = @max(16.67, max_ms);

    const chart_width = @as(f32, FRAME_AVG_LEN) * bar_width;
    try renderer.drawQuad(chart_pos, .{ .x = chart_width, .y = chart_height }, chart_color);

    var x: f32 = chart_pos.x;
    for (frame_ms_buf) |ms| {
        const height = @as(f32, @floatCast(ms)) / @as(f32, @floatCast(reference_ms)) * chart_height;
        try renderer.drawQuad(.{ .x = x, .y = chart_pos.y + (chart_height - height) }, .{ .x = bar_width - 1, .y = height }, chart_color);
        x += bar_width;
    }

    //  string: []const u8, font_id: AssetCache.FontId, font_size: AssetCache.FontSize, line_spacing_override: ?u16, pos: Vec2, tint: Color
    try renderer.formatText(
        "FPS: {d:.2} Frame:{d:.2}ms Avg: {d:.2}ms Min: {d:.2}ms Max: {d:.2}ms",
        0,
        16,
        null,
        .{ .x = chart_pos.x + 5, .y = chart_pos.y + 5 },
        .{ .r = 1, .g = 1, .b = 1, .a = 1 },
        .{ avg_fps, frame_ms, avg_ms, min_ms, max_ms },
    );
}
