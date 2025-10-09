//! Slider widget for f32 values.

const SliderWidget = @This();

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
            current_value: T,

            key_repeat: KeyRepeatState = .{},

            // Style properties
            track_color: Ui.Color,
            handle_color: Ui.Color,
            handle_size: f32,

            pub const Config = struct {
                min: T = 0.0,
                max: T = 1.0,
                default: T = 0.5,
                track_color: Ui.Color = Ui.Color.init(200, 200, 200, 255),
                handle_color: Ui.Color = Ui.Color.init(100, 100, 100, 255),
                handle_size: f32 = 16.0,
            };

            pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*Self {
                const self = try ui.gpa.create(Self);
                errdefer ui.gpa.destroy(self);

                self.* = Self{
                    .id = id,
                    .min = config.min,
                    .max = config.max,
                    .current_value = std.math.clamp(config.default, config.min, config.max),
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

            pub fn bindEvents(self: *Self, ui: *Ui) !void {
                try ui.addListener(self.id, .mouse_down, Self, onMouseDown, self);
                try ui.addListener(self.id, .drag, Self, onDrag, self);
                try ui.addListener(self.id, .key_down, Self, onKeyDown, self);
                try ui.addListener(self.id, .key, Self, onKey, self);
                try ui.addListener(self.id, .key_up, Self, onKeyUp, self);
            }

            pub fn unbindEvents(self: *Self, ui: *Ui) void {
                ui.removeListener(self.id, .mouse_down, Self, onMouseDown);
                ui.removeListener(self.id, .drag, Self, onDrag);
                ui.removeListener(self.id, .key_down, Self, onKeyDown);
                ui.removeListener(self.id, .key, Self, onKey);
                ui.removeListener(self.id, .key_up, Self, onKeyUp);
            }

            pub fn onGet(self: *Self, _: *Ui) *const T {
                return &self.current_value;
            }

            pub fn onSet(self: *Self, _: *Ui, new_value: *const T) !void {
                self.current_value = std.math.clamp(new_value.*, self.min, self.max);
            }

            pub fn onMouseDown(self: *Self, ui: *Ui, info: Ui.Event.Info, mouse_down_data: Ui.Event.Payload(.mouse_down)) !void {
                try self.updateValueFromMouse(ui, info, mouse_down_data.mouse_position);
            }

            pub fn onDrag(self: *Self, ui: *Ui, info: Ui.Event.Info, drag_data: Ui.Event.Payload(.drag)) !void {
                try self.updateValueFromMouse(ui, info, drag_data.mouse_position);
            }

            pub fn onKeyDown(self: *Self, ui: *Ui, _: Ui.Event.Info, key_data: Ui.Event.Payload(.key_down)) !void {
                const key = key_data.key;
                if (key != .left and key != .down and key != .right and key != .up) return;

                self.key_repeat.state = .none;
                self.key_repeat.active_key = key;

                try self.performKeyAction(ui, key);

                self.key_repeat.advance();
                self.key_repeat.timer.?.reset();
            }

            pub fn onKey(self: *Self, ui: *Ui, _: Ui.Event.Info, key_data: Ui.Event.Payload(.key)) !void {
                const key = key_data.key;
                if (self.key_repeat.active_key != key) return;

                const delay = switch (self.key_repeat.state) {
                    .none => return,
                    .first => self.key_repeat.initial_delay,
                    .nth => self.key_repeat.nth_delay,
                };

                if (self.key_repeat.timer.?.read() < delay) return;

                try self.performKeyAction(ui, key);
                self.key_repeat.advance();
                self.key_repeat.timer.?.reset();
            }

            pub fn onKeyUp(self: *Self, _: *Ui, _: Ui.Event.Info, key_data: Ui.Event.Payload(.key_up)) !void {
                if (self.key_repeat.active_key == key_data.key) {
                    self.key_repeat.state = .none;
                    self.key_repeat.active_key = null;
                }
            }

            fn performKeyAction(self: *Self, ui: *Ui, key: BindingState.Key) !void {
                const step = (self.max - self.min) * 0.01;
                var new_value = self.current_value;

                switch (key) {
                    .left, .down => new_value -= step,
                    .right, .up => new_value += step,
                    else => return,
                }

                new_value = std.math.clamp(new_value, self.min, self.max);

                if (new_value != self.current_value) {
                    self.current_value = new_value;
                    try ui.pushEvent(self.id, .{ .float_change = self.current_value }, self);
                }
            }

            fn updateValueFromMouse(self: *Self, ui: *Ui, info: Ui.Event.Info, mouse_pos: vec2) !void {
                const bb = info.bounding_box;
                const relative_x = mouse_pos[0] - bb.x;
                const proportion = std.math.clamp(relative_x / bb.width, 0.0, 1.0);
                const new_value = self.min + proportion * (self.max - self.min);

                if (new_value != self.current_value) {
                    self.current_value = new_value;
                    try ui.pushEvent(self.id, .{ .float_change = self.current_value }, self);
                }
            }

            pub fn render(self: *Self, ui: *Ui, command: Ui.RenderCommand) !void {
                const bb = command.bounding_box;

                const track_height: f32 = 4.0;
                const track_y = bb.y + (bb.height - track_height) / 2.0;
                try ui.renderer.drawRoundedRect(.{ bb.x, track_y }, .{ bb.width, track_height }, .all(track_height / 2.0), self.track_color);

                const value_proportion: f32 = @floatCast((self.current_value - self.min) / (self.max - self.min));
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
                current_value: T,

                key_repeat: KeyRepeatState = .{},

                // Style properties
                track_color: Ui.Color,
                handle_color: Ui.Color,
                handle_size: f32,

                pub const Config = struct {
                    min: T = -50,
                    max: T = 50,
                    default: T = 0,
                    track_color: Ui.Color = Ui.Color.init(200, 200, 200, 255),
                    handle_color: Ui.Color = Ui.Color.init(100, 100, 100, 255),
                    handle_size: f32 = 16.0,
                };

                pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*Self {
                    const self = try ui.gpa.create(Self);
                    errdefer ui.gpa.destroy(self);

                    self.* = Self{
                        .id = id,
                        .min = config.min,
                        .max = config.max,
                        .current_value = std.math.clamp(config.default, config.min, config.max),
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

                pub fn bindEvents(self: *Self, ui: *Ui) !void {
                    try ui.addListener(self.id, .mouse_down, Self, onMouseDown, self);
                    try ui.addListener(self.id, .drag, Self, onDrag, self);
                    try ui.addListener(self.id, .key_down, Self, onKeyDown, self);
                    try ui.addListener(self.id, .key, Self, onKey, self);
                    try ui.addListener(self.id, .key_up, Self, onKeyUp, self);
                }

                pub fn unbindEvents(self: *Self, ui: *Ui) void {
                    ui.removeListener(self.id, .mouse_down, Self, onMouseDown);
                    ui.removeListener(self.id, .drag, Self, onDrag);
                    ui.removeListener(self.id, .key_down, Self, onKeyDown);
                    ui.removeListener(self.id, .key, Self, onKey);
                    ui.removeListener(self.id, .key_up, Self, onKeyUp);
                }

                pub fn onGet(self: *Self, _: *Ui) *const T {
                    return &self.current_value;
                }

                pub fn onSet(self: *Self, _: *Ui, new_value: *const T) !void {
                    self.current_value = std.math.clamp(new_value.*, self.min, self.max);
                }

                pub fn onMouseDown(self: *Self, ui: *Ui, info: Ui.Event.Info, mouse_down_data: Ui.Event.Payload(.mouse_down)) !void {
                    try self.updateValueFromMouse(ui, info, mouse_down_data.mouse_position);
                }

                pub fn onDrag(self: *Self, ui: *Ui, info: Ui.Event.Info, drag_data: Ui.Event.Payload(.drag)) !void {
                    try self.updateValueFromMouse(ui, info, drag_data.mouse_position);
                }

                pub fn onKeyDown(self: *Self, ui: *Ui, _: Ui.Event.Info, key_data: Ui.Event.Payload(.key_down)) !void {
                    const key = key_data.key;
                    if (key != .left and key != .down and key != .right and key != .up) return;

                    self.key_repeat.state = .none;
                    self.key_repeat.active_key = key;

                    try self.performKeyAction(ui, key);

                    self.key_repeat.advance();
                    self.key_repeat.timer.?.reset();
                }

                pub fn onKey(self: *Self, ui: *Ui, _: Ui.Event.Info, key_data: Ui.Event.Payload(.key)) !void {
                    const key = key_data.key;
                    if (self.key_repeat.active_key != key) return;

                    const delay = switch (self.key_repeat.state) {
                        .none => return,
                        .first => self.key_repeat.initial_delay,
                        .nth => self.key_repeat.nth_delay,
                    };

                    if (self.key_repeat.timer.?.read() < delay) return;

                    try self.performKeyAction(ui, key);
                    self.key_repeat.advance();
                    self.key_repeat.timer.?.reset();
                }

                pub fn onKeyUp(self: *Self, _: *Ui, _: Ui.Event.Info, key_data: Ui.Event.Payload(.key_up)) !void {
                    if (self.key_repeat.active_key == key_data.key) {
                        self.key_repeat.state = .none;
                        self.key_repeat.active_key = null;
                    }
                }

                fn performKeyAction(self: *Self, ui: *Ui, key: BindingState.Key) !void {
                    const step: T = 1;
                    var new_value = self.current_value;

                    switch (key) {
                        .left, .down => new_value -= step,
                        .right, .up => new_value += step,
                        else => return,
                    }

                    new_value = std.math.clamp(new_value, self.min, self.max);

                    if (new_value != self.current_value) {
                        self.current_value = new_value;
                        try ui.pushEvent(self.id, .{ .int_change = self.current_value }, self);
                    }
                }

                fn updateValueFromMouse(self: *Self, ui: *Ui, info: Ui.Event.Info, mouse_pos: vec2) !void {
                    const bb = info.bounding_box;
                    const relative_x = mouse_pos[0] - bb.x;
                    const proportion = std.math.clamp(relative_x / bb.width, 0.0, 1.0);

                    const range = @as(f32, @floatFromInt(self.max - self.min));
                    const float_offset = std.math.round(proportion * range);
                    const new_value = self.min + @as(T, @intFromFloat(float_offset));

                    if (new_value != self.current_value) {
                        self.current_value = new_value;
                        try ui.pushEvent(self.id, .{ .int_change = self.current_value }, self);
                    }
                }

                pub fn render(self: *Self, ui: *Ui, command: Ui.RenderCommand) !void {
                    const bb = command.bounding_box;

                    const track_height: f32 = 4.0;
                    const track_y = bb.y + (bb.height - track_height) / 2.0;
                    try ui.renderer.drawRoundedRect(.{ bb.x, track_y }, .{ bb.width, track_height }, .all(track_height / 2.0), self.track_color);

                    const value_proportion = if (self.max - self.min == 0)
                        0.0
                    else
                        @as(f32, @floatFromInt(self.current_value - self.min)) / @as(f32, @floatFromInt(self.max - self.min));

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
                current_value: T,

                key_repeat: KeyRepeatState = .{},

                // Style properties
                track_color: Ui.Color,
                handle_color: Ui.Color,
                handle_size: f32,

                pub const Config = struct {
                    min: T = 0,
                    max: T = 100,
                    default: T = 50,
                    track_color: Ui.Color = Ui.Color.init(200, 200, 200, 255),
                    handle_color: Ui.Color = Ui.Color.init(100, 100, 100, 255),
                    handle_size: f32 = 16.0,
                };

                pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*Self {
                    const self = try ui.gpa.create(Self);
                    errdefer ui.gpa.destroy(self);

                    self.* = Self{
                        .id = id,
                        .min = config.min,
                        .max = config.max,
                        .current_value = std.math.clamp(config.default, config.min, config.max),
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

                pub fn bindEvents(self: *Self, ui: *Ui) !void {
                    try ui.addListener(self.id, .mouse_down, Self, onMouseDown, self);
                    try ui.addListener(self.id, .drag, Self, onDrag, self);
                    try ui.addListener(self.id, .key_down, Self, onKeyDown, self);
                    try ui.addListener(self.id, .key, Self, onKey, self);
                    try ui.addListener(self.id, .key_up, Self, onKeyUp, self);
                }

                pub fn unbindEvents(self: *Self, ui: *Ui) void {
                    ui.removeListener(self.id, .mouse_down, Self, onMouseDown);
                    ui.removeListener(self.id, .drag, Self, onDrag);
                    ui.removeListener(self.id, .key_down, Self, onKeyDown);
                    ui.removeListener(self.id, .key, Self, onKey);
                    ui.removeListener(self.id, .key_up, Self, onKeyUp);
                }

                pub fn onGet(self: *Self, _: *Ui) *const T {
                    return &self.current_value;
                }

                pub fn onSet(self: *Self, _: *Ui, new_value: *const T) !void {
                    self.current_value = std.math.clamp(new_value.*, self.min, self.max);
                }

                pub fn onMouseDown(self: *Self, ui: *Ui, info: Ui.Event.Info, mouse_down_data: Ui.Event.Payload(.mouse_down)) !void {
                    try self.updateValueFromMouse(ui, info, mouse_down_data.mouse_position);
                }

                pub fn onDrag(self: *Self, ui: *Ui, info: Ui.Event.Info, drag_data: Ui.Event.Payload(.drag)) !void {
                    try self.updateValueFromMouse(ui, info, drag_data.mouse_position);
                }

                pub fn onKeyDown(self: *Self, ui: *Ui, _: Ui.Event.Info, key_data: Ui.Event.Payload(.key_down)) !void {
                    const key = key_data.key;
                    if (key != .left and key != .down and key != .right and key != .up) return;

                    self.key_repeat.state = .none;
                    self.key_repeat.active_key = key;

                    try self.performKeyAction(ui, key);

                    self.key_repeat.advance();
                    self.key_repeat.timer.?.reset();
                }

                pub fn onKey(self: *Self, ui: *Ui, _: Ui.Event.Info, key_data: Ui.Event.Payload(.key)) !void {
                    const key = key_data.key;
                    if (self.key_repeat.active_key != key) return;

                    const delay = switch (self.key_repeat.state) {
                        .none => return,
                        .first => self.key_repeat.initial_delay,
                        .nth => self.key_repeat.nth_delay,
                    };

                    if (self.key_repeat.timer.?.read() < delay) return;

                    try self.performKeyAction(ui, key);
                    self.key_repeat.advance();
                    self.key_repeat.timer.?.reset();
                }

                pub fn onKeyUp(self: *Self, _: *Ui, _: Ui.Event.Info, key_data: Ui.Event.Payload(.key_up)) !void {
                    if (self.key_repeat.active_key == key_data.key) {
                        self.key_repeat.state = .none;
                        self.key_repeat.active_key = null;
                    }
                }

                fn performKeyAction(self: *Self, ui: *Ui, key: BindingState.Key) !void {
                    const step: T = 1;
                    var new_value = self.current_value;

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
                        else => return,
                    }

                    new_value = std.math.clamp(new_value, self.min, self.max);

                    if (new_value != self.current_value) {
                        self.current_value = new_value;
                        try ui.pushEvent(self.id, .{ .uint_change = self.current_value }, self);
                    }
                }

                fn updateValueFromMouse(self: *Self, ui: *Ui, info: Ui.Event.Info, mouse_pos: vec2) !void {
                    const bb = info.bounding_box;
                    const relative_x = mouse_pos[0] - bb.x;
                    const proportion = std.math.clamp(relative_x / bb.width, 0.0, 1.0);

                    const range = @as(f32, @floatFromInt(self.max - self.min));
                    const float_offset = std.math.round(proportion * range);
                    const new_value = self.min + @as(T, @intFromFloat(float_offset));

                    if (new_value != self.current_value) {
                        self.current_value = new_value;
                        try ui.pushEvent(self.id, .{ .uint_change = self.current_value }, self);
                    }
                }

                pub fn render(self: *Self, ui: *Ui, command: Ui.RenderCommand) !void {
                    const bb = command.bounding_box;

                    const track_height: f32 = 4.0;
                    const track_y = bb.y + (bb.height - track_height) / 2.0;
                    try ui.renderer.drawRoundedRect(.{ bb.x, track_y }, .{ bb.width, track_height }, .all(track_height / 2.0), self.track_color);

                    const value_proportion = if (self.max - self.min == 0)
                        0.0
                    else
                        @as(f32, @floatFromInt(self.current_value - self.min)) / @as(f32, @floatFromInt(self.max - self.min));

                    const handle_x = bb.x + (value_proportion * (bb.width - self.handle_size));
                    const handle_y = bb.y + (bb.height - self.handle_size) / 2.0;

                    try ui.renderer.drawRoundedRect(.{ handle_x, handle_y }, .{ self.handle_size, self.handle_size }, .all(self.handle_size / 2.0), self.handle_color);
                }
            },
        },
        else => @compileError("Type " ++ @typeName(T) ++ " not supported in SliderWidget"),
    };
}
