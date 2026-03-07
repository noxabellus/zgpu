//! Checkbox widget for bool values.

const Checkbox = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

const log = std.log.scoped(.checkbox_widget);

test {
    log.debug("semantic analysis for widgets/Checkbox.zig", .{});
    std.testing.refAllDecls(@This());
}

value: *bool,
theme: Theme,

pub const Mark = enum {
    check,
    block,
};

pub const Theme = struct {
    checkbox_size: Ui.Sizing = .{ .w = .fixed(16), .h = .fixed(16) },
    checkbox_mark_color: Ui.Color = .black,
    checkbox_mark_ratio: f32 = 0.5,
    checkbox_mark: Mark = .check,

    pub const BINDING_SET = Ui.Theme.Binding.Set.create(Theme);
};

/// The rendering function for the checkbox, called by the UI system.
pub fn render(self: *Checkbox, ui: *Ui, command: Ui.RenderCommand) !void {
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

    // If checked, draw mark
    if (self.value.*) {
        switch (self.theme.checkbox_mark) {
            .check => {
                const cx = bb.x + bb.width * 0.5;
                const cy = bb.y + bb.height * 0.5;
                const sw = (self.theme.checkbox_mark_ratio * bb.width) * 0.4;
                const sh = (self.theme.checkbox_mark_ratio * bb.height) * 0.4;
                const p1 = vec2{ cx - sw, cy };
                const p2 = vec2{ cx - sw * 0.1, cy + sh };
                const p3 = vec2{ cx + sw, cy - sh };
                const line_width = (@min(bb.width, bb.height) * self.theme.checkbox_mark_ratio) * 0.25;

                try ui.renderer.drawLine(p1, p2, line_width, self.theme.checkbox_mark_color);
                try ui.renderer.drawLine(p2 - vec2{ line_width * 0.5, 0 }, p3, line_width, self.theme.checkbox_mark_color);
            },

            .block => {
                const padding = (1.0 - self.theme.checkbox_mark_ratio) * 0.5;

                try ui.renderer.drawRoundedRect(
                    .{ bb.x + bb.width * padding, bb.y + bb.height * padding },
                    .{ bb.width * self.theme.checkbox_mark_ratio, bb.height * self.theme.checkbox_mark_ratio },
                    .{
                        .top_left = command.render_data.rectangle.corner_radius.top_left * self.theme.checkbox_mark_ratio,
                        .top_right = command.render_data.rectangle.corner_radius.top_right * self.theme.checkbox_mark_ratio,
                        .bottom_left = command.render_data.rectangle.corner_radius.bottom_left * self.theme.checkbox_mark_ratio,
                        .bottom_right = command.render_data.rectangle.corner_radius.bottom_right * self.theme.checkbox_mark_ratio,
                    },
                    self.theme.checkbox_mark_color,
                );
            },
        }
    }
}

/// Configure an open element as a checkbox widget for boolean values.
pub fn checkbox(ui: *Ui, id: Ui.ElementId, value: *bool) !bool {
    try ui.openSection(id);
    defer ui.endSection();

    const self, _ = try ui.getOrCreateWidget(Checkbox, id);
    self.value = value;
    self.theme = .{};
    try ui.applyTheme(&Theme.BINDING_SET, .widget, &self.theme);

    try ui.configureSection(.{
        .sizing = self.theme.checkbox_size,
        .type = .render_widget,
        .event_flags = .{
            .activate = true,
            .focus = true,
        },
    });

    if (ui.getEvent(id, .activate_end)) |_| {
        self.value.* = !self.value.*;
        return true;
    }

    return false;
}
