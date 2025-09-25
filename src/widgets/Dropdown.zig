//! Dropdown widget for selecting one of multiple enum values.

const DropdownWidget = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");

const log = std.log.scoped(.dropdown_widget);

test {
    log.debug("semantic analysis for widgets/Dropdown.zig", .{});
    std.testing.refAllDecls(@This());
}

pub fn For(comptime T: type) type {
    const TInfo = @typeInfo(T);
    comptime {
        if (TInfo != .@"enum") {
            @compileError("DropdownWidget only supports enum types, but got " ++ @typeName(T));
        }
        if (std.meta.fields(T).len == 0) {
            @compileError("DropdownWidget requires a non-empty enum.");
        }
    }

    return struct {
        const Self = @This();

        id: Ui.ElementId,
        current_value: T,
        is_open: bool,

        // Style properties
        box_color: Ui.Color,
        box_color_hover: Ui.Color,
        text_color: Ui.Color,
        font_id: Ui.FontId,
        font_size: u16,

        panel_color: Ui.Color,
        option_color_hover: Ui.Color,

        pub const Config = struct {
            /// The default enum value to be selected.
            default: T,
            /// The color of the main dropdown box.
            box_color: Ui.Color = Ui.Color.fromLinearU8(224, 215, 210, 255),
            /// The color of the main dropdown box when hovered.
            box_color_hover: Ui.Color = Ui.Color.fromLinearU8(238, 227, 225, 255),
            /// The color of the text for the selected value.
            text_color: Ui.Color = Ui.Color.fromLinearU8(61, 26, 5, 255),
            /// The font to use for the text.
            font_id: Ui.FontId,
            /// The font size for the text.
            font_size: u16 = 16,
            /// The background color of the floating options panel.
            panel_color: Ui.Color = Ui.Color.fromLinearU8(244, 235, 230, 255),
            /// The background color of an option when it is hovered.
            option_color_hover: Ui.Color = Ui.Color.fromLinearU8(224, 215, 210, 255),
        };

        pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*Self {
            const self = try ui.gpa.create(Self);
            errdefer ui.gpa.destroy(self);

            self.* = Self{
                .id = id,
                .current_value = config.default,
                .is_open = false,
                .box_color = config.box_color,
                .box_color_hover = config.box_color_hover,
                .text_color = config.text_color,
                .font_id = config.font_id,
                .font_size = config.font_size,
                .panel_color = config.panel_color,
                .option_color_hover = config.option_color_hover,
            };

            return self;
        }

        pub fn deinit(self: *Self, ui: *Ui) void {
            self.unbindEvents(ui);
            ui.gpa.destroy(self);
        }

        pub fn bindEvents(self: *Self, ui: *Ui) !void {
            try ui.addListener(self.id, .activate_end, Self, EventHandler.toggleOpen, self);

            inline for (comptime std.meta.fields(T)) |field| {
                const option_id = try optionId(self, ui, field.name);
                const field_value = @field(T, field.name);
                try ui.addListener(option_id, .activate_end, Self, EventHandler.select(field_value), self);
            }
        }

        pub fn unbindEvents(self: *Self, ui: *Ui) void {
            ui.removeListener(self.id, .activate_end, Self, EventHandler.toggleOpen);

            inline for (comptime std.meta.fields(T)) |field| {
                if (optionId(self, ui, field.name)) |option_id| {
                    const field_value = @field(T, field.name);
                    ui.removeListener(option_id, .activate_end, Self, EventHandler.select(field_value));
                } else |err| {
                    log.err("Error generating option ID for unbinding: {s}", .{@errorName(err)});
                }
            }
        }

        pub fn onGet(self: *Self, _: *Ui) *const T {
            return &self.current_value;
        }

        pub fn onSet(self: *Self, _: *Ui, new_value: *const T) !void {
            self.current_value = new_value.*;
        }

        pub fn declare(self: *Self, ui: *Ui) !void {
            // --- Configure the main dropdown box ---
            try ui.configureElement(.{
                .layout = .{
                    .sizing = .{ .w = .fixed(150), .h = .fixed(30) },
                    .padding = .axes(10, 0),
                    .child_alignment = .center,
                },
                .background_color = if (ui.hovered()) self.box_color_hover else self.box_color,
                .corner_radius = .all(4),
                .border = .{ .width = .all(1), .color = if (ui.focused()) Ui.Color.blue else Ui.Color.black },
                .widget = false,
                .state = .flags(.{ .activate = true, .focus = true }),
            });

            try ui.text(@tagName(self.current_value), .{
                .font_id = self.font_id,
                .font_size = self.font_size,
                .color = self.text_color,
            });

            // --- Declare the floating options panel (if open) ---
            if (self.is_open) {
                const panel_id = try self.panelId(ui);
                try ui.openElement(.{
                    .id = panel_id,
                    .layout = .{
                        .sizing = .{ .w = .grow, .h = .fit },
                        .direction = .top_to_bottom,
                    },
                    .floating = .{
                        .attach_to = .element_with_id,
                        .parentId = self.id.id,
                        .attach_points = .{ .parent = .left_top, .element = .left_top },
                        .z_index = 10,
                    },
                    .background_color = self.panel_color,
                    .border = .{ .width = .all(1), .color = Ui.Color.black },
                    .corner_radius = .all(4),
                });
                defer ui.closeElement();

                inline for (comptime std.meta.fieldNames(T)) |field_name| {
                    const option_id = try optionId(self, ui, field_name);

                    try ui.beginElement(option_id);
                    defer ui.closeElement();

                    try ui.configureElement(.{
                        .layout = .{
                            .sizing = .{ .w = .grow, .h = .fixed(30) },
                            .padding = .axes(10, 0),
                            .child_alignment = .center,
                        },
                        .background_color = if (ui.hovered()) self.option_color_hover else .transparent,
                        .widget = false,
                        .state = .flags(.{ .activate = true }),
                    });

                    try ui.text(field_name, .{
                        .font_id = self.font_id,
                        .font_size = self.font_size,
                        .color = self.text_color,
                    });
                }
            }

            // --- Per-frame logic to handle closing the dropdown ---
            // This logic is placed after all elements are declared for the frame,
            // because it uses a geometric check for the click event.
            if (self.is_open and ui.bindings.getAction(.primary_mouse) == .pressed) {
                const mouse_pos = ui.bindings.getMousePosition();
                var click_is_internal = false;

                // Check if click is inside the main dropdown box
                const main_box_data = ui.getElementBounds(self.id);
                if (main_box_data != null and Ui.boxContains(main_box_data.?, mouse_pos)) {
                    click_is_internal = true;
                }

                // Check if click is inside the floating panel
                if (!click_is_internal) {
                    const panel_id = try self.panelId(ui);
                    const panel_data = ui.getElementBounds(panel_id);
                    if (panel_data != null and Ui.boxContains(panel_data.?, mouse_pos)) {
                        click_is_internal = true;
                    }
                }

                if (!click_is_internal) {
                    self.is_open = false;
                }
            }
        }

        pub fn render(_: *Self, _: *Ui, _: Ui.RenderCommand) !void {
            // This widget is rendered entirely via its `declare` method.
        }

        fn panelId(self: *Self, ui: *Ui) !Ui.ElementId {
            return Ui.ElementId.fromSlice(try std.fmt.allocPrint(ui.frame_arena, "{x}_dropdown_panel", .{self.id.id}));
        }

        fn optionId(self: *Self, ui: *Ui, name: []const u8) !Ui.ElementId {
            return Ui.ElementId.fromSlice(try std.fmt.allocPrint(ui.frame_arena, "{x}_option_{s}", .{ self.id.id, name }));
        }

        const EventHandler = struct {
            pub fn toggleOpen(self: *Self, _: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.activate_end)) !void {
                self.is_open = !self.is_open;
            }

            pub fn select(comptime value: T) (fn (self: *Self, ui: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.activate_end)) anyerror!void) {
                return struct {
                    pub fn handler(self: *Self, ui: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.activate_end)) !void {
                        if (self.current_value != value) {
                            self.current_value = value;
                            const int_value: std.meta.Tag(T) = @intFromEnum(value);
                            try ui.pushEvent(
                                self.id,
                                if (comptime @typeInfo(std.meta.Tag(T)).int.signedness == .signed)
                                    Ui.Event.Data{ .int_change = int_value }
                                else
                                    Ui.Event.Data{ .uint_change = int_value },
                                self,
                            );
                        }
                        self.is_open = false;
                    }
                }.handler;
            }
        };
    };
}
