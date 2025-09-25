//! RadioButton widget for selecting one of multiple enum values.

const RadioButtonWidget = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");

const log = std.log.scoped(.radio_button_widget);

test {
    log.debug("semantic analysis for widgets/RadioButton.zig", .{});
    std.testing.refAllDecls(@This());
}

pub fn For(comptime T: type) type {
    const TInfo = @typeInfo(T);
    comptime {
        if (TInfo != .@"enum") {
            @compileError("RadioButtonWidget only supports enum types, but got " ++ @typeName(T));
        }
        if (std.meta.fieldNames(T).len == 0) {
            @compileError("RadioButtonWidget requires a non-empty enum.");
        }
    }

    return struct {
        const Self = @This();

        id: Ui.ElementId,
        group_id: Ui.ElementId,
        value: T,

        // Style properties
        circle_color: Ui.Color,
        dot_color: Ui.Color,
        size: f32,

        pub const Config = struct {
            /// The ElementId shared by all radio buttons in this group.
            group_id: Ui.ElementId,
            /// The specific enum value this button represents.
            value: T,
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
                .circle_color = config.circle_color,
                .dot_color = config.dot_color,
                .size = config.size,
            };

            return self;
        }

        pub fn deinit(self: *Self, ui: *Ui) void {
            ui.gpa.destroy(self);
        }

        pub fn bindEvents(self: *Self, ui: *Ui) !void {
            // A radio button is "activated" by a click or keyboard interaction.
            // `activate_end` is the correct event as it fires on release for both.
            try ui.addListener(self.id, .activate_end, Self, onActivate, self);
        }

        pub fn unbindEvents(self: *Self, ui: *Ui) void {
            ui.removeListener(self.id, .activate_end, Self, onActivate);
        }

        /// Returns the enum value this specific button represents, not the group's current value.
        pub fn onGet(self: *Self, _: *Ui) *const T {
            return &self.value;
        }

        /// Setting the value of an individual radio button is not supported. This is a no-op.
        /// To change the selected button programmatically, get the shared state pointer via
        /// `ui.getOrPutSharedWidgetState` and modify its value directly.
        pub fn onSet(_: *Self, _: *Ui, _: *const T) !void {}

        /// Called when the user clicks the radio button or activates it with the keyboard.
        fn onActivate(self: *Self, ui: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.activate_end)) !void {
            // Get the default value for the group if it doesn't exist yet.
            // As per the requirement, this is the first field in the enum declaration.
            const default_value = comptime @field(T, std.meta.fieldNames(T)[0]);

            // Get a pointer to the shared state for this group. This will create it with
            // the default value if it's the first button in the group to be activated.
            const shared_state_ptr = try ui.getOrPutSharedWidgetState(T, self.group_id, default_value);

            // Update the shared state to this button's value.
            if (shared_state_ptr.* != self.value) {
                shared_state_ptr.* = self.value;

                // Push a value_change event to notify the application.
                const int_value: std.meta.Tag(T) = @intFromEnum(self.value);
                try ui.pushEvent(
                    self.group_id,
                    if (comptime @typeInfo(std.meta.Tag(T)).int.signedness == .signed)
                        Ui.Event.Data{ .int_change = int_value }
                    else
                        Ui.Event.Data{ .uint_change = int_value },
                    self,
                );
            }
        }

        /// The rendering function for the radio button, called by the UI system.
        pub fn render(self: *Self, ui: *Ui, command: Ui.RenderCommand) !void {
            // Get the default value for the group if it doesn't exist yet.
            const default_value = comptime @field(T, std.meta.fieldNames(T)[0]);

            // Get a pointer to the shared state for this group to check if we are selected.
            const shared_state_ptr = try ui.getOrPutSharedWidgetState(T, self.group_id, default_value);
            const is_selected = (shared_state_ptr.* == self.value);

            const bb = command.bounding_box;

            // Center the radio button within its bounding box
            const circle_pos = Ui.Vec2{
                .x = bb.x + (bb.width - self.size) / 2.0,
                .y = bb.y + (bb.height - self.size) / 2.0,
            };
            const circle_size = Ui.Vec2{ .x = self.size, .y = self.size };
            const radius = self.size / 2.0;

            // Draw the outer circle using a rounded rect
            try ui.renderer.drawRoundedRect(circle_pos, circle_size, .all(radius), self.circle_color);

            // If selected, draw the inner dot
            if (is_selected) {
                const dot_size_val = self.size * 0.6;
                const dot_pos = Ui.Vec2{
                    .x = bb.x + (bb.width - dot_size_val) / 2.0,
                    .y = bb.y + (bb.height - dot_size_val) / 2.0,
                };
                const dot_size = Ui.Vec2{ .x = dot_size_val, .y = dot_size_val };
                const dot_radius = dot_size_val / 2.0;
                try ui.renderer.drawRoundedRect(dot_pos, dot_size, .all(dot_radius), self.dot_color);
            }
        }
    };
}
