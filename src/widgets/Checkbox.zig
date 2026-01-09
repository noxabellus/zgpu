//! Checkbox widget for bool values.

const CheckboxWidget = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

const log = std.log.scoped(.checkbox_widget);

test {
    log.debug("semantic analysis for widgets/Checkbox.zig", .{});
    std.testing.refAllDecls(@This());
}

id: Ui.ElementId,
current_value: bool,

// Style properties
box_color: Ui.Color,
check_color: Ui.Color,
size: f32,

pub const Config = struct {
    default: bool = false,
    box_color: Ui.Color = Ui.Color.init(200, 200, 200, 255),
    check_color: Ui.Color = Ui.Color.init(50, 50, 50, 255),
    size: f32 = 16.0,
};

pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*CheckboxWidget {
    const self = try ui.gpa.create(CheckboxWidget);
    errdefer ui.gpa.destroy(self);

    self.* = CheckboxWidget{
        .id = id,
        .current_value = config.default,
        .box_color = config.box_color,
        .check_color = config.check_color,
        .size = config.size,
    };

    return self;
}

pub fn deinit(self: *CheckboxWidget, ui: *Ui) void {
    ui.gpa.destroy(self);
}

pub fn bindEvents(self: *CheckboxWidget, ui: *Ui) !void {
    try ui.addListener(self.id, .activate_end, CheckboxWidget, onActivate, self);
}

pub fn unbindEvents(self: *CheckboxWidget, ui: *Ui) void {
    ui.removeListener(self.id, .activate_end, CheckboxWidget, onActivate);
}

pub fn onGet(self: *CheckboxWidget, _: *Ui) *const bool {
    return &self.current_value;
}

pub fn onSet(self: *CheckboxWidget, _: *Ui, new_value: *const bool) !void {
    self.current_value = new_value.*;
}

/// Called when the user clicks the checkbox or activates it with the keyboard.
fn onActivate(self: *CheckboxWidget, ui: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.activate_end)) !void {
    self.current_value = !self.current_value;
    try ui.pushEvent(self.id, .{ .bool_change = self.current_value }, self);
}

/// The rendering function for the checkbox, called by the UI system.
pub fn render(self: *CheckboxWidget, ui: *Ui, command: Ui.RenderCommand) !void {
    const bb = command.bounding_box;

    // Center the checkbox within its bounding box
    const box_pos = vec2{
        bb.x + (bb.width - self.size) / 2.0,
        bb.y + (bb.height - self.size) / 2.0,
    };
    const box_size = vec2{ self.size, self.size };
    const radius = self.size * 0.15; // A small corner radius

    // Draw the box
    try ui.renderer.drawRoundedRect(box_pos, box_size, .all(radius), self.box_color);

    // If checked, draw the checkmark
    if (self.current_value) {
        // Simple checkmark using two lines
        const p1 = vec2{ box_pos[0] + self.size * 0.2, box_pos[1] + self.size * 0.5 };
        const p2 = vec2{ box_pos[0] + self.size * 0.45, box_pos[1] + self.size * 0.75 };
        const p3 = vec2{ box_pos[0] + self.size * 0.8, box_pos[1] + self.size * 0.25 };
        const line_width = self.size * 0.125;

        try ui.renderer.drawLine(p1, p2, line_width, self.check_color);
        try ui.renderer.drawLine(p2, p3, line_width, self.check_color);
    }
}
