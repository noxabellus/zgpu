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

id: Ui.ElementId,
value: *bool,

// Style properties
box_color: Ui.Color,
check_color: Ui.Color,
size: f32,

pub const Config = struct {
    value: *bool,
    box_color: Ui.Color = Ui.Color.init(200, 200, 200, 255),
    check_color: Ui.Color = Ui.Color.init(50, 50, 50, 255),
    size: f32 = 16.0,
};

pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*Checkbox {
    const self = try ui.gpa.create(Checkbox);
    errdefer ui.gpa.destroy(self);

    self.* = Checkbox{
        .id = id,
        .value = config.value,
        .box_color = config.box_color,
        .check_color = config.check_color,
        .size = config.size,
    };

    return self;
}

pub fn deinit(self: *Checkbox, ui: *Ui) void {
    ui.gpa.destroy(self);
}

/// The rendering function for the checkbox, called by the UI system.
pub fn render(self: *Checkbox, ui: *Ui, command: Ui.RenderCommand) !void {
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
    if (self.value.*) {
        // Simple checkmark using two lines
        const p1 = vec2{ box_pos[0] + self.size * 0.2, box_pos[1] + self.size * 0.5 };
        const p2 = vec2{ box_pos[0] + self.size * 0.45, box_pos[1] + self.size * 0.75 };
        const p3 = vec2{ box_pos[0] + self.size * 0.8, box_pos[1] + self.size * 0.25 };
        const line_width = self.size * 0.125;

        try ui.renderer.drawLine(p1, p2, line_width, self.check_color);
        try ui.renderer.drawLine(p2, p3, line_width, self.check_color);
    }
}

/// Configure an open element as a checkbox widget for boolean values.
pub fn checkbox(ui: *Ui, config: Config) !bool {
    const id = ui.open_ids.items[ui.open_ids.items.len - 1];
    const gop = try ui.widget_states.getOrPut(ui.gpa, id.id);
    const self = if (!gop.found_existing) create_new: {
        const ptr = try Checkbox.init(ui, id, config);
        gop.value_ptr.* = Ui.Widget{
            .user_data = ptr,
            .render = @ptrCast(&Checkbox.render),
            .deinit = @ptrCast(&Checkbox.deinit),
            .seen_this_frame = true,
        };

        break :create_new ptr;
    } else reuse_existing: {
        gop.value_ptr.seen_this_frame = true;

        const ptr: *Checkbox = @ptrCast(@alignCast(gop.value_ptr.user_data));
        // Update config properties in case they change frame-to-frame.
        ptr.box_color = config.box_color;
        ptr.check_color = config.check_color;
        ptr.size = config.size;

        break :reuse_existing ptr;
    };

    if (ui.getEvent(id, .activate_end)) |_| {
        self.value.* = !self.value.*;
        return true;
    }

    return false;
}
