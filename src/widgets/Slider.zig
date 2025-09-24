//! Slider widget for f32 values.

const SliderWidget = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");

const log = std.log.scoped(.slider_widget);

test {
    log.debug("semantic analysis for widgets/Slider.zig", .{});
    std.testing.refAllDecls(@This());
}

pub fn For(comptime T: type) type {
    const TInfo = @typeInfo(T);

    return switch (TInfo) {
        .float => |float_info| struct {
            const Self = @This();

            id: Ui.ElementId,
            min: T,
            max: T,
            current_value: T,

            // Style properties
            track_color: Ui.Color,
            handle_color: Ui.Color,
            handle_size: f32,

            // Interaction state
            is_dragging: bool = false,

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

                return self;
            }

            pub fn deinit(self: *Self, ui: *Ui) void {
                ui.gpa.destroy(self);
            }

            pub fn bindEvents(self: *Self, ui: *Ui) !void {
                try ui.addListener(self.id, .mouse_down, Self, onMouseDown, self);
                try ui.addListener(self.id, .drag, Self, onDrag, self);
                try ui.addListener(self.id, .mouse_up, Self, onMouseUp, self);
            }

            pub fn unbindEvents(self: *Self, ui: *Ui) void {
                ui.removeListener(self.id, .mouse_down, Self, onMouseDown);
                ui.removeListener(self.id, .drag, Self, onDrag);
                ui.removeListener(self.id, .mouse_up, Self, onMouseUp);
            }

            /// Called when the user presses the mouse button over the slider.
            pub fn onMouseDown(self: *Self, ui: *Ui, info: Ui.Event.Info, mouse_down_data: Ui.Event.Payload(.mouse_down)) !void {
                self.is_dragging = true;
                try self.updateValueFromMouse(ui, info, mouse_down_data.mouse_position);
            }

            /// Called when the user drags the mouse after clicking on the slider.
            pub fn onDrag(self: *Self, ui: *Ui, info: Ui.Event.Info, drag_data: Ui.Event.Payload(.drag)) !void {
                if (self.is_dragging) {
                    try self.updateValueFromMouse(ui, info, drag_data.mouse_position);
                }
            }

            /// Called when the user releases the mouse button.
            pub fn onMouseUp(self: *Self, _: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.mouse_up)) !void {
                self.is_dragging = false;
            }

            /// Calculates the new slider value based on mouse position and fires an event if it changed.
            fn updateValueFromMouse(self: *Self, ui: *Ui, info: Ui.Event.Info, mouse_pos: Ui.Vec2) !void {
                const bb = info.bounding_box;
                // For a horizontal slider, we only care about the x position.
                const relative_x = mouse_pos.x - bb.x;
                const proportion = std.math.clamp(relative_x / bb.width, 0.0, 1.0);
                const new_value = self.min + proportion * (self.max - self.min);

                if (new_value != self.current_value) {
                    self.current_value = new_value;
                    // Push a value_change event to notify the application.

                    try ui.pushEvent(self.id, if (float_info.bits == 64) Ui.Event.Data{ .f64_change = self.current_value } else Ui.Event.Data{ .f32_change = self.current_value }, self);
                }
            }

            /// The rendering function for the slider, called by the UI system.
            pub fn render(self: *Self, ui: *Ui, command: Ui.RenderCommand) !void {
                const bb = command.bounding_box;

                // Draw the track (a thin, rounded rectangle).
                const track_height: f32 = 4.0;
                const track_y = bb.y + (bb.height - track_height) / 2.0;
                try ui.renderer.drawRoundedRect(.{ .x = bb.x, .y = track_y }, .{ .x = bb.width, .y = track_height }, .all(track_height / 2.0), self.track_color);

                // Calculate the handle's position based on the current value.
                const value_proportion: f32 = @floatCast((self.current_value - self.min) / (self.max - self.min));
                const handle_x = bb.x + (value_proportion * (bb.width - self.handle_size));
                const handle_y = bb.y + (bb.height - self.handle_size) / 2.0;

                // Draw the handle (a circle or rounded square).
                try ui.renderer.drawRoundedRect(.{ .x = handle_x, .y = handle_y }, .{ .x = self.handle_size, .y = self.handle_size }, .all(self.handle_size / 2.0), self.handle_color);
            }
        },

        .int => |int_info| switch (int_info.signedness) {
            .signed => struct {
                const Self = @This();

                id: Ui.ElementId,
                min: T,
                max: T,
                current_value: T,

                // Style properties
                track_color: Ui.Color,
                handle_color: Ui.Color,
                handle_size: f32,

                // Interaction state
                is_dragging: bool = false,

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

                    return self;
                }

                pub fn deinit(self: *Self, ui: *Ui) void {
                    ui.gpa.destroy(self);
                }

                pub fn bindEvents(self: *Self, ui: *Ui) !void {
                    try ui.addListener(self.id, .mouse_down, Self, onMouseDown, self);
                    try ui.addListener(self.id, .drag, Self, onDrag, self);
                    try ui.addListener(self.id, .mouse_up, Self, onMouseUp, self);
                }

                pub fn unbindEvents(self: *Self, ui: *Ui) void {
                    ui.removeListener(self.id, .mouse_down, Self, onMouseDown);
                    ui.removeListener(self.id, .drag, Self, onDrag);
                    ui.removeListener(self.id, .mouse_up, Self, onMouseUp);
                }

                /// Called when the user presses the mouse button over the slider.
                pub fn onMouseDown(self: *Self, ui: *Ui, info: Ui.Event.Info, mouse_down_data: Ui.Event.Payload(.mouse_down)) !void {
                    self.is_dragging = true;
                    try self.updateValueFromMouse(ui, info, mouse_down_data.mouse_position);
                }

                /// Called when the user drags the mouse after clicking on the slider.
                pub fn onDrag(self: *Self, ui: *Ui, info: Ui.Event.Info, drag_data: Ui.Event.Payload(.drag)) !void {
                    if (self.is_dragging) {
                        try self.updateValueFromMouse(ui, info, drag_data.mouse_position);
                    }
                }

                /// Called when the user releases the mouse button.
                pub fn onMouseUp(self: *Self, _: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.mouse_up)) !void {
                    self.is_dragging = false;
                }

                /// Calculates the new slider value based on mouse position and fires an event if it changed.
                fn updateValueFromMouse(self: *Self, ui: *Ui, info: Ui.Event.Info, mouse_pos: Ui.Vec2) !void {
                    const bb = info.bounding_box;
                    const relative_x = mouse_pos.x - bb.x;
                    const proportion = std.math.clamp(relative_x / bb.width, 0.0, 1.0);

                    const range = @as(f32, @floatFromInt(self.max - self.min));
                    const float_offset = std.math.round(proportion * range);
                    const new_value = self.min + @as(T, @intFromFloat(float_offset));

                    if (new_value != self.current_value) {
                        self.current_value = new_value;
                        try ui.pushEvent(self.id, .{ .int_change = self.current_value }, self);
                    }
                }

                /// The rendering function for the slider, called by the UI system.
                pub fn render(self: *Self, ui: *Ui, command: Ui.RenderCommand) !void {
                    const bb = command.bounding_box;

                    // Draw the track
                    const track_height: f32 = 4.0;
                    const track_y = bb.y + (bb.height - track_height) / 2.0;
                    try ui.renderer.drawRoundedRect(.{ .x = bb.x, .y = track_y }, .{ .x = bb.width, .y = track_height }, .all(track_height / 2.0), self.track_color);

                    // Calculate the handle's position
                    const value_proportion = if (self.max - self.min == 0)
                        0.0
                    else
                        @as(f32, @floatFromInt(self.current_value - self.min)) / @as(f32, @floatFromInt(self.max - self.min));

                    const handle_x = bb.x + (value_proportion * (bb.width - self.handle_size));
                    const handle_y = bb.y + (bb.height - self.handle_size) / 2.0;

                    // Draw the handle
                    try ui.renderer.drawRoundedRect(.{ .x = handle_x, .y = handle_y }, .{ .x = self.handle_size, .y = self.handle_size }, .all(self.handle_size / 2.0), self.handle_color);
                }
            },

            .unsigned => struct {
                const Self = @This();
                id: Ui.ElementId,
                min: T,
                max: T,
                current_value: T,

                // Style properties
                track_color: Ui.Color,
                handle_color: Ui.Color,
                handle_size: f32,

                // Interaction state
                is_dragging: bool = false,

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

                    return self;
                }

                pub fn deinit(self: *Self, ui: *Ui) void {
                    ui.gpa.destroy(self);
                }

                pub fn bindEvents(self: *Self, ui: *Ui) !void {
                    try ui.addListener(self.id, .mouse_down, Self, onMouseDown, self);
                    try ui.addListener(self.id, .drag, Self, onDrag, self);
                    try ui.addListener(self.id, .mouse_up, Self, onMouseUp, self);
                }

                pub fn unbindEvents(self: *Self, ui: *Ui) void {
                    ui.removeListener(self.id, .mouse_down, Self, onMouseDown);
                    ui.removeListener(self.id, .drag, Self, onDrag);
                    ui.removeListener(self.id, .mouse_up, Self, onMouseUp);
                }

                /// Called when the user presses the mouse button over the slider.
                pub fn onMouseDown(self: *Self, ui: *Ui, info: Ui.Event.Info, mouse_down_data: Ui.Event.Payload(.mouse_down)) !void {
                    self.is_dragging = true;
                    try self.updateValueFromMouse(ui, info, mouse_down_data.mouse_position);
                }

                /// Called when the user drags the mouse after clicking on the slider.
                pub fn onDrag(self: *Self, ui: *Ui, info: Ui.Event.Info, drag_data: Ui.Event.Payload(.drag)) !void {
                    if (self.is_dragging) {
                        try self.updateValueFromMouse(ui, info, drag_data.mouse_position);
                    }
                }

                /// Called when the user releases the mouse button.
                pub fn onMouseUp(self: *Self, _: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.mouse_up)) !void {
                    self.is_dragging = false;
                }

                /// Calculates the new slider value based on mouse position and fires an event if it changed.
                fn updateValueFromMouse(self: *Self, ui: *Ui, info: Ui.Event.Info, mouse_pos: Ui.Vec2) !void {
                    const bb = info.bounding_box;
                    const relative_x = mouse_pos.x - bb.x;
                    const proportion = std.math.clamp(relative_x / bb.width, 0.0, 1.0);

                    const range = @as(f32, @floatFromInt(self.max - self.min));
                    const float_offset = std.math.round(proportion * range);
                    const new_value = self.min + @as(T, @intFromFloat(float_offset));

                    if (new_value != self.current_value) {
                        self.current_value = new_value;
                        try ui.pushEvent(self.id, .{ .uint_change = self.current_value }, self);
                    }
                }

                /// The rendering function for the slider, called by the UI system.
                pub fn render(self: *Self, ui: *Ui, command: Ui.RenderCommand) !void {
                    const bb = command.bounding_box;

                    // Draw the track
                    const track_height: f32 = 4.0;
                    const track_y = bb.y + (bb.height - track_height) / 2.0;
                    try ui.renderer.drawRoundedRect(.{ .x = bb.x, .y = track_y }, .{ .x = bb.width, .y = track_height }, .all(track_height / 2.0), self.track_color);

                    // Calculate the handle's position
                    const value_proportion = if (self.max - self.min == 0)
                        0.0
                    else
                        @as(f32, @floatFromInt(self.current_value - self.min)) / @as(f32, @floatFromInt(self.max - self.min));

                    const handle_x = bb.x + (value_proportion * (bb.width - self.handle_size));
                    const handle_y = bb.y + (bb.height - self.handle_size) / 2.0;

                    // Draw the handle
                    try ui.renderer.drawRoundedRect(.{ .x = handle_x, .y = handle_y }, .{ .x = self.handle_size, .y = self.handle_size }, .all(self.handle_size / 2.0), self.handle_color);
                }
            },
        },
        else => @compileError("Type " ++ @typeName(T) ++ " not supported in SliderWidget"),
    };
}
