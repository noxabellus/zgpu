//! Slider widget for f32 values.

const Slider = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");
const BindingState = @import("../BindingState.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

const log = std.log.scoped(.slider_widget);

test {
    log.debug("semantic analysis for widgets/Slider.zig", .{});
    std.testing.refAllDecls(@This());
}

const KeyRepeatState = struct {
    timer: ?std.time.Timer = null,
    initial_delay: u64 = 350 * std.time.ns_per_ms,
    nth_delay: u64 = 25 * std.time.ns_per_ms,
    state: enum { none, first, nth } = .none,
    active_key: ?BindingState.Key = null,

    fn advance(self: *@This()) void {
        self.state = switch (self.state) {
            .none => .first,
            .first => .nth,
            else => self.state,
        };
    }
};

pub fn For(comptime T: type) type {
    const TInfo = @typeInfo(T);

    if ((TInfo == .int and TInfo.int.bits > 64) or (TInfo == .float and TInfo.float.bits > 64)) {
        @compileError("SliderWidget supports a maximum of 64-bits on operand types");
    }

    return switch (TInfo) {
        .float => struct {
            const Self = @This();

            id: Ui.ElementId,
            min: T,
            max: T,
            value: *T,

            key_repeat: KeyRepeatState = .{},

            // Style properties
            track_color: Ui.Color,
            handle_color: Ui.Color,
            handle_size: f32,

            pub const Config = struct {
                value: *T,
                min: T = 0.0,
                max: T = 1.0,
                track_color: Ui.Color = Ui.Color.init(200, 200, 200, 255),
                handle_color: Ui.Color = Ui.Color.init(100, 100, 100, 255),
                handle_size: f32 = 16.0,
            };

            pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*Self {
                const self = try ui.gpa.create(Self);
                errdefer ui.gpa.destroy(self);

                config.value.* = std.math.clamp(config.value.*, config.min, config.max);

                self.* = Self{
                    .id = id,
                    .min = config.min,
                    .max = config.max,
                    .value = config.value,
                    .track_color = config.track_color,
                    .handle_color = config.handle_color,
                    .handle_size = config.handle_size,
                };
                self.key_repeat.timer = try std.time.Timer.start();

                return self;
            }

            pub fn deinit(self: *Self, ui: *Ui) void {
                ui.gpa.destroy(self);
            }

            fn performKeyAction(self: *Self, key: BindingState.Key) !bool {
                const step = (self.max - self.min) * 0.01;
                var new_value = self.value.*;

                switch (key) {
                    .left, .down => new_value -= step,
                    .right, .up => new_value += step,
                    else => return false,
                }

                new_value = std.math.clamp(new_value, self.min, self.max);

                defer self.value.* = new_value;
                return self.value.* != new_value;
            }

            fn updateValueFromMouse(self: *Self, info: Ui.Event.Info, mouse_pos: vec2) !bool {
                const bb = info.bounding_box;
                const relative_x = mouse_pos[0] - bb.x;
                const proportion = std.math.clamp(relative_x / bb.width, 0.0, 1.0);
                const new_value = self.min + proportion * (self.max - self.min);

                defer self.value.* = new_value;
                return self.value.* != new_value;
            }

            pub fn render(self: *Self, ui: *Ui, command: Ui.RenderCommand) !void {
                const bb = command.bounding_box;

                const track_height: f32 = 4.0;
                const track_y = bb.y + (bb.height - track_height) / 2.0;
                try ui.renderer.drawRoundedRect(.{ bb.x, track_y }, .{ bb.width, track_height }, .all(track_height / 2.0), self.track_color);

                const value_proportion: f32 = @floatCast((self.value.* - self.min) / (self.max - self.min));
                const handle_x = bb.x + (value_proportion * (bb.width - self.handle_size));
                const handle_y = bb.y + (bb.height - self.handle_size) / 2.0;

                try ui.renderer.drawRoundedRect(.{ handle_x, handle_y }, .{ self.handle_size, self.handle_size }, .all(self.handle_size / 2.0), self.handle_color);
            }
        },

        .int => |int_info| switch (int_info.signedness) {
            .signed => struct {
                const Self = @This();

                id: Ui.ElementId,
                min: T,
                max: T,
                value: *T,

                key_repeat: KeyRepeatState = .{},

                // Style properties
                track_color: Ui.Color,
                handle_color: Ui.Color,
                handle_size: f32,

                pub const Config = struct {
                    value: *T,
                    min: T = -50,
                    max: T = 50,
                    track_color: Ui.Color = Ui.Color.init(200, 200, 200, 255),
                    handle_color: Ui.Color = Ui.Color.init(100, 100, 100, 255),
                    handle_size: f32 = 16.0,
                };

                pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*Self {
                    const self = try ui.gpa.create(Self);
                    errdefer ui.gpa.destroy(self);

                    config.value.* = std.math.clamp(config.value.*, config.min, config.max);

                    self.* = Self{
                        .id = id,
                        .min = config.min,
                        .max = config.max,
                        .value = config.value,
                        .track_color = config.track_color,
                        .handle_color = config.handle_color,
                        .handle_size = config.handle_size,
                    };
                    self.key_repeat.timer = try std.time.Timer.start();

                    return self;
                }

                pub fn deinit(self: *Self, ui: *Ui) void {
                    ui.gpa.destroy(self);
                }

                fn performKeyAction(self: *Self, key: BindingState.Key) !bool {
                    const step: T = 1;
                    var new_value = self.value.*;

                    switch (key) {
                        .left, .down => new_value -= step,
                        .right, .up => new_value += step,
                        else => return false,
                    }

                    new_value = std.math.clamp(new_value, self.min, self.max);

                    defer self.value.* = new_value;
                    return self.value.* != new_value;
                }

                fn updateValueFromMouse(self: *Self, info: Ui.Event.Info, mouse_pos: vec2) !bool {
                    const bb = info.bounding_box;
                    const relative_x = mouse_pos[0] - bb.x;
                    const proportion = std.math.clamp(relative_x / bb.width, 0.0, 1.0);

                    const range = @as(f32, @floatFromInt(self.max - self.min));
                    const float_offset = std.math.round(proportion * range);
                    const new_value = self.min + @as(T, @intFromFloat(float_offset));

                    defer self.value.* = new_value;
                    return self.value.* != new_value;
                }

                pub fn render(self: *Self, ui: *Ui, command: Ui.RenderCommand) !void {
                    const bb = command.bounding_box;

                    const track_height: f32 = 4.0;
                    const track_y = bb.y + (bb.height - track_height) / 2.0;
                    try ui.renderer.drawRoundedRect(.{ bb.x, track_y }, .{ bb.width, track_height }, .all(track_height / 2.0), self.track_color);

                    const value_proportion = if (self.max - self.min == 0)
                        0.0
                    else
                        @as(f32, @floatFromInt(self.value.* - self.min)) / @as(f32, @floatFromInt(self.max - self.min));

                    const handle_x = bb.x + (value_proportion * (bb.width - self.handle_size));
                    const handle_y = bb.y + (bb.height - self.handle_size) / 2.0;

                    try ui.renderer.drawRoundedRect(.{ handle_x, handle_y }, .{ self.handle_size, self.handle_size }, .all(self.handle_size / 2.0), self.handle_color);
                }
            },

            .unsigned => struct {
                const Self = @This();
                id: Ui.ElementId,
                min: T,
                max: T,
                value: *T,

                key_repeat: KeyRepeatState = .{},

                // Style properties
                track_color: Ui.Color,
                handle_color: Ui.Color,
                handle_size: f32,

                pub const Config = struct {
                    value: *T,
                    min: T = 0,
                    max: T = 100,
                    track_color: Ui.Color = Ui.Color.init(200, 200, 200, 255),
                    handle_color: Ui.Color = Ui.Color.init(100, 100, 100, 255),
                    handle_size: f32 = 16.0,
                };

                pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*Self {
                    const self = try ui.gpa.create(Self);
                    errdefer ui.gpa.destroy(self);

                    config.value.* = std.math.clamp(config.value.*, config.min, config.max);

                    self.* = Self{
                        .id = id,
                        .min = config.min,
                        .max = config.max,
                        .value = config.value,
                        .track_color = config.track_color,
                        .handle_color = config.handle_color,
                        .handle_size = config.handle_size,
                    };
                    self.key_repeat.timer = try std.time.Timer.start();

                    return self;
                }

                pub fn deinit(self: *Self, ui: *Ui) void {
                    ui.gpa.destroy(self);
                }

                fn performKeyAction(self: *Self, key: BindingState.Key) !bool {
                    const step: T = 1;
                    var new_value = self.value.*;

                    switch (key) {
                        .left, .down => {
                            if (new_value > self.min) {
                                new_value -= step;
                            } else {
                                new_value = self.min;
                            }
                        },
                        .right, .up => {
                            if (new_value < self.max) {
                                new_value += step;
                            } else {
                                new_value = self.max;
                            }
                        },
                        else => return false,
                    }

                    new_value = std.math.clamp(new_value, self.min, self.max);

                    defer self.value.* = new_value;
                    return self.value.* != new_value;
                }

                fn updateValueFromMouse(self: *Self, info: Ui.Event.Info, mouse_pos: vec2) !bool {
                    const bb = info.bounding_box;
                    const relative_x = mouse_pos[0] - bb.x;
                    const proportion = std.math.clamp(relative_x / bb.width, 0.0, 1.0);

                    const range = @as(f32, @floatFromInt(self.max - self.min));
                    const float_offset = std.math.round(proportion * range);
                    const new_value = self.min + @as(T, @intFromFloat(float_offset));

                    defer self.value.* = new_value;
                    return self.value.* != new_value;
                }

                pub fn render(self: *Self, ui: *Ui, command: Ui.RenderCommand) !void {
                    const bb = command.bounding_box;

                    const track_height: f32 = 4.0;
                    const track_y = bb.y + (bb.height - track_height) / 2.0;
                    try ui.renderer.drawRoundedRect(.{ bb.x, track_y }, .{ bb.width, track_height }, .all(track_height / 2.0), self.track_color);

                    const value_proportion = if (self.max - self.min == 0)
                        0.0
                    else
                        @as(f32, @floatFromInt(self.value.* - self.min)) / @as(f32, @floatFromInt(self.max - self.min));

                    const handle_x = bb.x + (value_proportion * (bb.width - self.handle_size));
                    const handle_y = bb.y + (bb.height - self.handle_size) / 2.0;

                    try ui.renderer.drawRoundedRect(.{ handle_x, handle_y }, .{ self.handle_size, self.handle_size }, .all(self.handle_size / 2.0), self.handle_color);
                }
            },
        },
        else => @compileError("Type " ++ @typeName(T) ++ " not supported in Slider Widget"),
    };
}

/// Configure an open element as a slider widget; works with floats and integers of all signs and sizes up to 64 bits.
pub fn slider(ui: *Ui, comptime T: type, config: Slider.For(T).Config) !bool {
    const S = Slider.For(T);

    const id = ui.open_ids.items[ui.open_ids.items.len - 1];
    const gop = try ui.widget_states.getOrPut(ui.gpa, id.id);
    const self = if (!gop.found_existing) create_new: {
        const ptr = try S.init(ui, id, config);
        gop.value_ptr.* = Ui.Widget{
            .user_data = ptr,
            .render = @ptrCast(&S.render),
            .deinit = @ptrCast(&S.deinit),
            .seen_this_frame = true,
        };

        break :create_new ptr;
    } else reuse_existing: {
        gop.value_ptr.seen_this_frame = true;

        const ptr: *S = @ptrCast(@alignCast(gop.value_ptr.user_data));

        // Update config properties in case they change frame-to-frame.
        ptr.value.* = std.math.clamp(ptr.value.*, config.min, config.max);
        ptr.min = config.min;
        ptr.max = config.max;
        ptr.track_color = config.track_color;
        ptr.handle_color = config.handle_color;
        ptr.handle_size = config.handle_size;

        break :reuse_existing ptr;
    };

    var changed = false;
    if (ui.getEvent(id, .mouse_down)) |event| {
        changed = changed or try self.updateValueFromMouse(event.info, event.data.mouse_down.mouse_position);
    }

    if (ui.getEvent(id, .drag)) |event| {
        changed = changed or try self.updateValueFromMouse(event.info, event.data.drag.mouse_position);
    }

    if (ui.getEvent(id, .key_down)) |event| key_down: {
        const key = event.data.key_down.key;
        if (key != .left and key != .down and key != .right and key != .up) break :key_down;

        self.key_repeat.state = .none;
        self.key_repeat.active_key = key;

        changed = changed or try self.performKeyAction(key);

        self.key_repeat.advance();
        self.key_repeat.timer.?.reset();
    }

    if (ui.getEvent(id, .key)) |event| key: {
        const key = event.data.key;
        if (self.key_repeat.active_key != key.key) break :key;

        const delay = switch (self.key_repeat.state) {
            .none => break :key,
            .first => self.key_repeat.initial_delay,
            .nth => self.key_repeat.nth_delay,
        };

        if (self.key_repeat.timer.?.read() < delay) break :key;

        changed = changed or try self.performKeyAction(key.key);
        self.key_repeat.advance();
        self.key_repeat.timer.?.reset();
    }

    if (ui.getEvent(id, .key_up)) |event| {
        if (self.key_repeat.active_key == event.data.key.key) {
            self.key_repeat.state = .none;
            self.key_repeat.active_key = null;
        }
    }

    return changed;
}
