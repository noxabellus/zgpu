//! Radio button widget for selecting one of multiple enum values.

const RadioButton = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

const log = std.log.scoped(.radio_button_widget);

test {
    log.debug("semantic analysis for widgets/RadioButton.zig", .{});
    std.testing.refAllDecls(@This());
}

value: usize,
selected: *usize,
theme: Theme,

pub const Theme = struct {
    radio_size: Ui.Sizing = .{ .w = .fixed(16), .h = .fixed(16) },
    radio_mark_color: Ui.Color = .black,
    radio_mark_ratio: f32 = 0.5,

    pub const BINDING_SET = Ui.Theme.Binding.Set.create(Theme);
};

pub fn render(self: *RadioButton, ui: *Ui, command: Ui.RenderCommand) !void {
    const bb = command.bounding_box;

    // Draw the box
    try ui.renderer.drawRoundedRect(
        .{ bb.x, bb.y },
        .{ bb.width, bb.height },
        .{
            .top_left = command.render_data.custom.corner_radius.top_left,
            .top_right = command.render_data.custom.corner_radius.top_right,
            .bottom_left = command.render_data.custom.corner_radius.bottom_left,
            .bottom_right = command.render_data.custom.corner_radius.bottom_right,
        },
        .{
            .r = command.render_data.custom.background_color[0],
            .g = command.render_data.custom.background_color[1],
            .b = command.render_data.custom.background_color[2],
            .a = command.render_data.custom.background_color[3],
        },
    );

    // If selected, draw the inner dot
    if (self.selected.* == self.value) {
        const padding = (1.0 - self.theme.radio_mark_ratio) * 0.5;

        try ui.renderer.drawRoundedRect(
            .{ bb.x + bb.width * padding, bb.y + bb.height * padding },
            .{ bb.width * self.theme.radio_mark_ratio, bb.height * self.theme.radio_mark_ratio },
            .{
                .top_left = command.render_data.rectangle.corner_radius.top_left * self.theme.radio_mark_ratio,
                .top_right = command.render_data.rectangle.corner_radius.top_right * self.theme.radio_mark_ratio,
                .bottom_left = command.render_data.rectangle.corner_radius.bottom_left * self.theme.radio_mark_ratio,
                .bottom_right = command.render_data.rectangle.corner_radius.bottom_right * self.theme.radio_mark_ratio,
            },
            self.theme.radio_mark_color,
        );
    }
}

pub const Config = struct {
    disabled: bool = false,
};

pub fn enumRadioButton(comptime T: type, ui: *Ui, id: Ui.ElementId, selected: *T, value: T, config: Config) !bool {
    const state, _ = try ui.getOrCreateSharedWidgetState(struct {
        selected: usize,
        value: usize,
    }, id);

    state.* = .{
        .selected = inline for (comptime std.meta.fieldNames(T), 0..) |field_name, i| {
            if (@field(T, field_name) == selected.*) break i;
        } else unreachable,
        .value = inline for (comptime std.meta.fieldNames(T), 0..) |field_name, i| {
            if (@field(T, field_name) == value) break i;
        } else unreachable,
    };

    if (try radioButton(ui, id, &state.selected, state.value, config)) {
        inline for (comptime std.meta.fieldNames(T), 0..) |field_name, i| {
            if (i == state.selected) {
                selected.* = @field(T, field_name);
                return true;
            }
        }
        unreachable;
    }
    return false;
}

pub fn radioButton(ui: *Ui, id: Ui.ElementId, selected: *usize, value: usize, config: Config) !bool {
    const self, _ = try ui.getOrCreateWidget(RadioButton, id);
    self.selected = selected;
    self.value = value;
    self.theme = .{};

    try ui.openElement(id);
    defer ui.endElement();

    const state = if (config.disabled) Ui.ActionState.disabled else null;
    try ui.applyThemeState(&Theme.BINDING_SET, .widget, state orelse ui.getActionState(), &self.theme);

    try ui.configureElement(.{
        .sizing = self.theme.radio_size,
        .type = .render_widget,
        .state = state,
        .event_flags = .{
            .activate = true,
            .focus = true,
        },
    });

    try ui.menuNavigable();

    if (ui.disabled()) return false;

    if (ui.getEvent(id, .activate_end)) |_| {
        // Update the shared state to this button's value.
        if (self.selected.* != self.value) {
            self.selected.* = self.value;

            return true;
        }
    }

    return false;
}
