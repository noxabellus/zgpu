//! Dropdown widget for selecting one of multiple enum values.

const Dropdown = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");

const log = std.log.scoped(.dropdown_widget);

test {
    log.debug("semantic analysis for widgets/Dropdown.zig", .{});
    std.testing.refAllDecls(@This());
}

pub fn For(comptime T: type) type {
    const TInfo = @typeInfo(T);

    if (TInfo != .@"enum") {
        @compileError("DropdownWidget only supports enum types, but got " ++ @typeName(T));
    }

    if (std.meta.fields(T).len == 0) {
        @compileError("DropdownWidget requires a non-empty enum.");
    }

    const field_names = comptime std.meta.fieldNames(T);

    return struct {
        const Self = @This();

        id: Ui.ElementId,
        value: *T,
        is_open: bool,
        highlighted_index: ?usize,

        // Style properties
        box_color: Ui.Color,
        box_color_hover: Ui.Color,
        border_color: Ui.Color,
        border_color_hover: Ui.Color,
        border_width: Ui.BorderWidth,
        corner_radius: Ui.CornerRadius,
        text_color: Ui.Color,
        font_id: Ui.FontId,
        font_size: u16,

        panel_color: Ui.Color,
        option_color_hover: Ui.Color,

        pub const Config = struct {
            /// The default enum value to be selected.
            value: *T,
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
            /// The color of the element border.
            border_color: Ui.Color = Ui.Color.fromLinearU8(200, 190, 185, 255),
            /// The color of the element border when hovered or focused.
            border_color_hover: Ui.Color = Ui.Color.fromLinearU8(150, 140, 135, 255),
            /// The corner radius for the dropdown and options panel.
            corner_radius: Ui.CornerRadius = .all(4.0),
            /// The width of the border for the element.
            border_width: Ui.BorderWidth = .all(1),
        };

        pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*Self {
            const self = try ui.gpa.create(Self);
            errdefer ui.gpa.destroy(self);

            self.* = Self{
                .id = id,
                .value = config.value,
                .is_open = false,
                .highlighted_index = null,
                .box_color = config.box_color,
                .box_color_hover = config.box_color_hover,
                .text_color = config.text_color,
                .font_id = config.font_id,
                .font_size = config.font_size,
                .panel_color = config.panel_color,
                .option_color_hover = config.option_color_hover,
                .border_color = config.border_color,
                .border_color_hover = config.border_color_hover,
                .corner_radius = config.corner_radius,
                .border_width = config.border_width,
            };

            return self;
        }

        pub fn deinit(self: *Self, ui: *Ui) void {
            ui.gpa.destroy(self);
        }

        pub fn declare(self: *Self, ui: *Ui) !void {
            // --- Configure the main dropdown box ---
            try ui.configureElement(.{
                .layout = .{
                    .sizing = .{ .w = .fixed(150), .h = .fixed(30) },
                    .padding = .axes(10, 0),
                    .child_alignment = .center,
                },
                .background_color = if (ui.hovered() or ui.focused()) self.box_color_hover else self.box_color,
                .corner_radius = self.corner_radius,
                .border = .{ .width = self.border_width, .color = if (ui.focused() or ui.hovered()) self.border_color_hover else self.border_color },
                .widget = false,
                .state = .flags(.{ .activate = true, .focus = true, .keyboard = true }),
            });

            try ui.text(@tagName(self.value.*), .{
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
                    .border = .{ .width = self.border_width, .color = if (ui.focused() or ui.hovered()) self.border_color_hover else self.border_color },
                    .corner_radius = self.corner_radius,
                });
                defer ui.endElement();

                inline for (comptime std.meta.fields(T), 0..) |_, i| {
                    const field_name = comptime std.meta.fields(T)[i].name;
                    const option_id = try optionId(self, ui, field_name);

                    try ui.beginElement(option_id);
                    defer ui.endElement();

                    if (ui.hovered()) {
                        self.highlighted_index = i;
                    }

                    try ui.configureElement(.{
                        .layout = .{
                            .sizing = .{ .w = .grow, .h = .fixed(30) },
                            .padding = .axes(10, 0),
                            .child_alignment = .center,
                        },
                        .background_color = if (self.highlighted_index == i) self.option_color_hover else .transparent,
                        .widget = false,
                        .corner_radius = self.corner_radius,
                        .state = .flags(.{ .activate = true }),
                    });

                    try ui.text(field_name, .{
                        .font_id = self.font_id,
                        .font_size = self.font_size,
                        .color = self.text_color,
                    });
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

        fn selectValue(self: *Self, ui: *Ui, value: T) !bool {
            self.highlighted_index = null;

            defer self.is_open = false;
            if (self.is_open) {
                ui.popFocusScope();
            }

            defer self.value.* = value;
            return self.value.* == value;
        }

        fn selectHighlighted(self: *Self, ui: *Ui) !bool {
            if (self.highlighted_index) |idx| {
                inline for (comptime field_names, 0..) |field_name, i| {
                    if (i == idx) {
                        const selected_value = @field(T, field_name);
                        // selectValue will close the dropdown and pop the focus scope
                        return self.selectValue(ui, selected_value);
                    }
                }
            }

            // If we get here, nothing was highlighted. Just close.
            self.is_open = false;
            self.highlighted_index = null;
            ui.popFocusScope();
            return false;
        }
    };
}

/// Configure an open element as a dropdown selection widget for enum values.
pub fn dropdown(ui: *Ui, comptime T: type, config: Dropdown.For(T).Config) !bool {
    const D = Dropdown.For(T);

    const field_names = comptime std.meta.fieldNames(T);
    const num_options = comptime field_names.len;

    const id = ui.open_ids.items[ui.open_ids.items.len - 1];
    const gop = try ui.widget_states.getOrPut(ui.gpa, id.id);
    const self = if (!gop.found_existing) create_new: {
        const ptr = try D.init(ui, id, config);
        gop.value_ptr.* = Ui.Widget{
            .user_data = ptr,
            .render = @ptrCast(&D.render),
            .deinit = @ptrCast(&D.deinit),
            .seen_this_frame = true,
        };

        break :create_new ptr;
    } else reuse_existing: {
        gop.value_ptr.seen_this_frame = true;

        const ptr: *D = @ptrCast(@alignCast(gop.value_ptr.user_data));

        // Update config properties in case they change frame-to-frame.
        ptr.box_color = config.box_color;
        ptr.box_color_hover = config.box_color_hover;
        ptr.text_color = config.text_color;
        ptr.font_id = config.font_id;
        ptr.font_size = config.font_size;
        ptr.panel_color = config.panel_color;
        ptr.option_color_hover = config.option_color_hover;
        ptr.border_color = config.border_color;
        ptr.border_color_hover = config.border_color_hover;
        ptr.corner_radius = config.corner_radius;
        ptr.border_width = config.border_width;

        break :reuse_existing ptr;
    };

    // Crucially, the widget declares its own elements, including the floating panel if open.
    try self.declare(ui);

    var changed = false;

    if (ui.getEvent(self.id, .activate_end)) |_| {
        if (self.is_open) {
            // If dropdown is open, activation selects the highlighted item.
            changed = changed or try self.selectHighlighted(ui);
        } else {
            // If dropdown is closed, activation opens it.
            self.is_open = true;
            try ui.pushFocusScope(self.id);

            // When opening, set highlight to the current selection
            inline for (field_names, 0..) |field_name, i| {
                const value = comptime @field(T, field_name);
                if (value == self.value.*) {
                    self.highlighted_index = i;
                    break;
                }
            }
        }
    }

    if (ui.getEvent(self.id, .key_down)) |event| key_down: {
        if (!self.is_open) {
            // When closed and focused, allow arrow keys to open the dropdown.
            // Activation (Enter/Click) is handled by onActivate.
            switch (event.data.key_down.key) {
                .up, .down => {
                    self.is_open = true;
                    try ui.pushFocusScope(self.id);

                    // When opening, set highlight to the current selection
                    inline for (field_names, 0..) |field_name, i| {
                        const value = comptime @field(T, field_name);
                        if (value == self.value.*) {
                            self.highlighted_index = i;
                            break;
                        }
                    }
                },
                else => {},
            }
            break :key_down;
        }

        // When open, handle navigation and selection.
        switch (event.data.key_down.key) {
            .up => {
                if (self.highlighted_index) |idx| {
                    self.highlighted_index = (idx + num_options - 1) % num_options;
                } else {
                    self.highlighted_index = num_options - 1;
                }
            },
            .down => {
                if (self.highlighted_index) |idx| {
                    self.highlighted_index = (idx + 1) % num_options;
                } else {
                    self.highlighted_index = 0;
                }
            },
            .space => {
                // Space is a common activation key, handle it directly for convenience.
                changed = changed or try self.selectHighlighted(ui);
            },
            // .enter is handled by the 'activate_end' event.
            // .escape is handled by the 'scoped_focus_close' event.
            else => {},
        }
    }

    if (ui.getEvent(self.id, .focus_lost)) |_| {
        // When the dropdown loses focus, always close the panel and pop the scope.
        if (self.is_open) {
            self.is_open = false;
            self.highlighted_index = null;
            ui.popFocusScope();
        }
    }

    if (ui.getEvent(self.id, .scoped_focus_change)) |event| scoped_focus_change: {
        if (!self.is_open) break :scoped_focus_change;

        switch (event.data.scoped_focus_change) {
            .prev => { // Equivalent to Shift+Tab
                if (self.highlighted_index) |idx| {
                    self.highlighted_index = (idx + num_options - 1) % num_options;
                } else {
                    self.highlighted_index = num_options - 1;
                }
            },
            .next => { // Equivalent to Tab
                if (self.highlighted_index) |idx| {
                    self.highlighted_index = (idx + 1) % num_options;
                } else {
                    self.highlighted_index = 0;
                }
            },
        }
    }

    if (ui.getEvent(self.id, .scoped_focus_close)) |_| {
        // This handler is only called if this widget owns the scope.
        // It should close the dropdown.
        if (self.is_open) {
            self.is_open = false;
            self.highlighted_index = null;
            ui.popFocusScope();
        }
    }

    inline for (field_names) |field_name| {
        const field_value = comptime @field(T, field_name);
        const option_id = try D.optionId(self, ui, field_name);
        if (ui.getEvent(option_id, .activate_end)) |_| {
            changed = changed or try self.selectValue(ui, field_value);
        }
    }

    return changed;
}
