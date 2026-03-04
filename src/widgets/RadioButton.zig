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

pub fn For(comptime T: type) type {
    const TInfo = @typeInfo(T);

    if (TInfo != .@"enum") {
        @compileError("RadioButtonWidget only supports enum types, but got " ++ @typeName(T));
    }

    if (std.meta.fieldNames(T).len == 0) {
        @compileError("RadioButtonWidget requires a non-empty enum.");
    }

    return struct {
        const Self = @This();

        id: Ui.ElementId,
        group_id: Ui.ElementId,
        value: T,
        state: *T,

        // Style properties
        circle_color: Ui.Color,
        dot_color: Ui.Color,
        size: f32,

        pub const Config = struct {
            /// The ElementId shared by all radio buttons in this group.
            group_id: Ui.ElementId,
            /// The specific enum value this button represents.
            value: T,
            /// The enum value to control.
            state: *T,
            /// The color of the button's outer circle.
            circle_color: Ui.Color = Ui.Color.fromLinearU8(200, 200, 200, 255),
            /// The color of the inner dot when the button is selected.
            dot_color: Ui.Color = Ui.Color.fromLinearU8(50, 50, 50, 255),
            /// The diameter of the radio button.
            size: f32 = 16.0,
        };

        pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*Self {
            const self = try ui.gpa.create(Self);
            errdefer ui.gpa.destroy(self);

            self.* = Self{
                .id = id,
                .group_id = config.group_id,
                .value = config.value,
                .state = config.state,
                .circle_color = config.circle_color,
                .dot_color = config.dot_color,
                .size = config.size,
            };

            return self;
        }

        pub fn deinit(self: *Self, ui: *Ui) void {
            ui.gpa.destroy(self);
        }

        /// The rendering function for the radio button, called by the UI system.
        pub fn render(self: *Self, ui: *Ui, command: Ui.RenderCommand) !void {
            // Get a pointer to the shared state for this group to check if we are selected.
            const is_selected = (self.state.* == self.value);

            const bb = command.bounding_box;

            // Center the radio button within its bounding box
            const circle_pos = vec2{
                bb.x + (bb.width - self.size) / 2.0,
                bb.y + (bb.height - self.size) / 2.0,
            };
            const circle_size = vec2{ self.size, self.size };
            const radius = self.size / 2.0;

            // Draw the outer circle using a rounded rect
            try ui.renderer.drawRoundedRect(circle_pos, circle_size, .all(radius), self.circle_color);

            // If selected, draw the inner dot
            if (is_selected) {
                const dot_size_val = self.size * 0.6;
                const dot_pos = vec2{
                    bb.x + (bb.width - dot_size_val) / 2.0,
                    bb.y + (bb.height - dot_size_val) / 2.0,
                };
                const dot_size = vec2{ dot_size_val, dot_size_val };
                const dot_radius = dot_size_val / 2.0;
                try ui.renderer.drawRoundedRect(dot_pos, dot_size, .all(dot_radius), self.dot_color);
            }
        }
    };
}

/// Configure an open element as a radio button widget for enum values.
/// All radio buttons sharing the same `group_id` in the config will be linked.
pub fn radioButton(ui: *Ui, comptime T: type, config: RadioButton.For(T).Config) !bool {
    const R = RadioButton.For(T);

    const id = ui.open_ids.items[ui.open_ids.items.len - 1];
    const gop = try ui.widget_states.getOrPut(ui.gpa, id.id);
    const self = if (!gop.found_existing) create_new: {
        const ptr = try R.init(ui, id, config);
        gop.value_ptr.* = Ui.Widget{
            .user_data = ptr,
            .render = @ptrCast(&R.render),
            .deinit = @ptrCast(&R.deinit),
            .seen_this_frame = true,
        };

        break :create_new ptr;
    } else reuse_existing: {
        gop.value_ptr.seen_this_frame = true;

        const ptr: *R = @ptrCast(@alignCast(gop.value_ptr.user_data));

        // Update config properties in case they change frame-to-frame.
        ptr.circle_color = config.circle_color;
        ptr.dot_color = config.dot_color;
        ptr.size = config.size;

        break :reuse_existing ptr;
    };

    if (ui.getEvent(id, .activate_end)) |_| {
        // Update the shared state to this button's value.
        if (self.state.* != self.value) {
            self.state.* = self.value;

            return true;
        }
    }

    return false;
}
