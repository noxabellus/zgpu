//! Slider widget for numeric values.

const Slider = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");
const BindingState = @import("../BindingState.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

const log = std.log.scoped(.slider_widget);

test {
    log.debug("semantic analysis for widgets/FSlider.zig", .{});
    std.testing.refAllDecls(@This());
}

pub fn State(comptime T: type) type {
    const T_info = @typeInfo(T);
    return struct {
        value: *T,
        config: Config,
        theme: Theme,
        key_repeat: Ui.KeyRepeatState,

        pub const Config = struct {
            min: T = if (T_info == .int and T_info.int.signedness == .signed) -50 else 0,
            max: T = if (T_info == .float) 1.0 else if (T_info.int.signedness == .signed) 50 else 100,
            step: T = if (T_info == .float) 0.01 else 1,
            disabled: bool = false,
        };

        fn performKeyAction(self: *@This(), key: BindingState.Key) !bool {
            log.info("Slider.performKeyAction: {s}", .{@tagName(key)});
            var new_value = self.value.*;

            if (comptime @typeInfo(T) == .float) {
                switch (key) {
                    .left, .down => if (new_value > self.config.min) {
                        new_value -= self.config.step;
                    },
                    .right, .up => if (new_value < self.config.max) {
                        new_value += self.config.step;
                    },
                    else => return false,
                }
            } else {
                switch (key) {
                    .left, .down => if (new_value > self.config.min) {
                        new_value = new_value -| self.config.step;
                    },
                    .right, .up => if (new_value < self.config.max) {
                        new_value = new_value +| self.config.step;
                    },
                    else => return false,
                }
            }

            new_value = std.math.clamp(new_value, self.config.min, self.config.max);

            defer self.value.* = new_value;
            return self.value.* != new_value;
        }

        fn updateValueFromMouse(self: *@This(), info: Ui.Event.Info, mouse_pos: vec2) !bool {
            const bb = info.bounding_box;
            const relative_x = mouse_pos[0] - bb.x;
            const proportion = std.math.clamp(relative_x / bb.width, 0.0, 1.0);

            const new_value =
                if (comptime T_info == .float)
                    self.config.min + proportion * (self.config.max - self.config.min)
                else int: {
                    const range = @as(f32, @floatFromInt(self.config.max - self.config.min));
                    const float_offset = std.math.round(proportion * range);
                    break :int self.config.min + @as(T, @intFromFloat(float_offset));
                };

            defer self.value.* = new_value;
            return self.value.* != new_value;
        }

        pub fn render(self: *@This(), ui: *Ui, command: Ui.RenderCommand) !void {
            const bb = command.bounding_box;

            const track_color = command.render_data.custom.background_color;
            const corner_radius = command.render_data.custom.corner_radius;

            const track_y = bb.y + (bb.height - self.theme.slider_track_size) / 2.0;
            try ui.renderer.drawRoundedRect(
                .{ bb.x, track_y },
                .{ bb.width, self.theme.slider_track_size },
                .{
                    .bottom_left = self.theme.slider_track_radius.bottom_left,
                    .bottom_right = self.theme.slider_track_radius.bottom_right,
                    .top_left = self.theme.slider_track_radius.top_left,
                    .top_right = self.theme.slider_track_radius.top_right,
                },
                .{
                    .r = track_color[0],
                    .g = track_color[1],
                    .b = track_color[2],
                    .a = track_color[3],
                },
            );

            try ui.renderer.drawRoundedRectLine(
                .{ bb.x, track_y },
                .{ bb.width, self.theme.slider_track_size },
                .{
                    .bottom_left = self.theme.slider_track_radius.bottom_left,
                    .bottom_right = self.theme.slider_track_radius.bottom_right,
                    .top_left = self.theme.slider_track_radius.top_left,
                    .top_right = self.theme.slider_track_radius.top_right,
                },
                .{
                    .bottom = @floatFromInt(self.theme.border_width.bottom),
                    .left = @floatFromInt(self.theme.border_width.left),
                    .right = @floatFromInt(self.theme.border_width.right),
                    .top = @floatFromInt(self.theme.border_width.top),
                },
                self.theme.border_color,
            );

            const value_proportion: f32 =
                if (comptime T_info == .float)
                    @floatCast((self.value.* - self.config.min) / (self.config.max - self.config.min))
                else if (self.config.max - self.config.min == 0)
                    0.0
                else
                    @as(f32, @floatFromInt(self.value.* - self.config.min)) / @as(f32, @floatFromInt(self.config.max - self.config.min));

            const handle_x = bb.x + (value_proportion * (bb.width - self.theme.slider_handle_size));
            const handle_y = bb.y + (bb.height - self.theme.slider_handle_size) / 2.0;

            try ui.renderer.drawRoundedRect(
                .{ handle_x, handle_y },
                .{ self.theme.slider_handle_size, self.theme.slider_handle_size },
                .{
                    .bottom_left = corner_radius.bottom_left,
                    .bottom_right = corner_radius.bottom_right,
                    .top_left = corner_radius.top_left,
                    .top_right = corner_radius.top_right,
                },
                self.theme.slider_handle_color,
            );
        }
    };
}

pub const Theme = struct {
    slider_handle_color: Ui.Color = Ui.Color.init(100, 100, 100, 255),
    slider_handle_size: f32 = 16.0,
    slider_track_size: f32 = 8.0,
    slider_track_radius: Ui.CornerRadius = .all(4),
    border_width: Ui.BorderWidth = .all(0),
    border_color: Ui.Color = .black,

    pub const BINDING_SET = Ui.Theme.Binding.Set.create(Theme);
};

pub fn enumSlider(comptime T: type, ui: *Ui, id: Ui.ElementId, value: *T, config: struct { disabled: bool = false }) !bool {
    const state, _ = try ui.getOrCreateSharedWidgetState(usize, id);

    state.* = inline for (comptime std.meta.fieldNames(T), 0..) |field_name, i| {
        if (@field(T, field_name) == value.*) break i;
    } else unreachable;

    if (try slider(usize, ui, id, state, .{ .min = 0, .max = std.meta.fieldNames(T).len - 1, .disabled = config.disabled })) {
        inline for (comptime std.meta.fieldNames(T), 0..) |field_name, i| {
            if (i == state.*) {
                value.* = @field(T, field_name);
                return true;
            }
        }
        unreachable;
    }
    return false;
}

pub fn slider(comptime T: type, ui: *Ui, id: Ui.ElementId, value: *T, config: State(T).Config) !bool {
    const self, const new = try ui.getOrCreateWidget(State(T), id);

    self.value = value;
    self.config = config;
    self.theme = .{};
    if (new) {
        self.key_repeat = .{ .timer = try .start() };
    }

    try ui.openElement(id);
    defer ui.endElement();

    const state = if (config.disabled) Ui.ActionState.disabled else null;
    try ui.applyThemeState(&Theme.BINDING_SET, .widget, state orelse ui.getActionState(), &self.theme);

    try ui.configureElement(.{
        .sizing = .{ .w = .grow, .h = .fixed(self.theme.slider_handle_size) },
        .border_width = .all(0),
        .type = .render_widget,
        .state = state,
        .event_flags = .{
            .click = true,
            .drag = true,
            .focus = true,
            .keyboard = true,
        },
    });

    try Ui.widgets.menuNavigable(ui);

    if (ui.disabled()) return false;

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
        self.key_repeat.timer.reset();
    }

    if (ui.getEvent(id, .key)) |event| key: {
        const data = event.data.key;
        if (self.key_repeat.active_key != data.key) break :key;

        const delay = switch (self.key_repeat.state) {
            .none => break :key,
            .first => self.key_repeat.initial_delay,
            .nth => self.key_repeat.nth_delay,
        };

        if (self.key_repeat.timer.read() < delay) break :key;

        changed = changed or try self.performKeyAction(data.key);
        self.key_repeat.advance();
        self.key_repeat.timer.reset();
    }

    if (ui.getEvent(id, .key_up)) |event| {
        if (self.key_repeat.active_key == event.data.key_up.key) {
            self.key_repeat.state = .none;
            self.key_repeat.active_key = null;
        }
    }

    return changed;
}
