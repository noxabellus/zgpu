//! Dropdown widget for selecting one of multiple enum values.

const Dropdown = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");

const log = std.log.scoped(.dropdown_widget);

test {
    log.debug("semantic analysis for widgets/Dropdown.zig", .{});
    std.testing.refAllDecls(@This());
}

highlighted_index: ?usize,

pub const Theme = struct {
    corner_radius: Ui.CornerRadius = .{},
    pub const BINDING_SET = Ui.Theme.Binding.Set.create(Theme);
};

fn panelId(ui: *Ui, id: Ui.ElementId) !Ui.ElementId {
    return Ui.ElementId.fromSlice(try std.fmt.allocPrint(ui.frame_arena, "{s}_dropdown_panel", .{id.string_id.toSlice()}));
}

fn optionId(ui: *Ui, id: Ui.ElementId, name: []const u8) !Ui.ElementId {
    return Ui.ElementId.fromSlice(try std.fmt.allocPrint(ui.frame_arena, "{s}_option_{s}", .{ id.string_id.toSlice(), name }));
}

fn selectValue(self: *Dropdown, ui: *Ui, selected: *usize, value: usize) !bool {
    ui.popFocusScope();
    const changed = selected.* != value;
    selected.* = value;
    self.highlighted_index = null;
    return changed;
}

pub const Config = struct {
    disabled: bool = false,
};

pub fn enumDropdown(comptime T: type, ui: *Ui, id: Ui.ElementId, selected: *T, config: Config) !bool {
    const state, _ = try ui.getOrCreateSharedWidgetState(usize, id);

    state.* = inline for (comptime std.meta.fieldNames(T), 0..) |field_name, i| {
        if (@field(T, field_name) == selected.*) break i;
    } else unreachable;

    if (try dropdown(ui, id, state, std.meta.fieldNames(T), config)) {
        inline for (comptime std.meta.fieldNames(T), 0..) |field_name, i| {
            if (i == state.*) {
                selected.* = @field(T, field_name);
                return true;
            }
        }
        unreachable;
    }
    return false;
}

pub fn dropdown(ui: *Ui, id: Ui.ElementId, selected: *usize, options: []const []const u8, config: Config) !bool {
    const self, const is_new = try ui.getOrCreateWidget(Dropdown, id);

    if (is_new) self.highlighted_index = null;

    try ui.beginElement(id, .{
        .sizing = .{ .w = .grow, .h = .fit },
        .child_alignment = .center,
        .type = .layout_widget,
        .state = if (config.disabled) .disabled else null,
        .event_flags = .{ .activate = true, .focus = true, .keyboard = true },
    });
    defer ui.endElement();

    var theme = Theme{};
    try ui.applyTheme(&Theme.BINDING_SET, .widget, &theme);

    try ui.text(options[selected.*], .{ .alignment = .center });

    if (ui.disabled()) {
        self.highlighted_index = null;
        return false;
    }

    try ui.menuNavigable();

    if (self.highlighted_index) |hi| {
        const panel_id = try panelId(ui, id);
        try ui.beginElement(panel_id, .{
            .state = .standard,
            .sizing = .{ .w = .grow, .h = .fit },
            .padding = .all(0),
            .child_gap = 0,
            .direction = .top_to_bottom,
            .floating = .{
                .attach_to = .element_with_id,
                .parentId = id.id,
                .attach_points = .{ .parent = .left_top, .element = .left_top },
                .z_index = 10,
            },
            .type = .layout_widget,
        });
        defer ui.endElement();

        for (options, 0..) |field_name, i| {
            const option_id = try optionId(ui, id, field_name);

            try ui.beginElement(option_id, .{
                .sizing = .{ .w = .grow, .h = .fit },
                .child_alignment = .center,
                .event_flags = .{ .activate = true },
                .state = if (hi == i) .focus else null,
                .border_width = .all(0),
                .type = .layout_widget,
                .corner_radius = if (i == 0)
                    Ui.CornerRadius{ .top_left = theme.corner_radius.top_left, .top_right = theme.corner_radius.top_right }
                else if (i == options.len - 1)
                    Ui.CornerRadius{ .bottom_left = theme.corner_radius.bottom_left, .bottom_right = theme.corner_radius.bottom_right }
                else
                    .all(0),
            });
            defer ui.endElement();

            try ui.text(field_name, .{
                .alignment = .center,
                .state = if (hi == i) .focus else null,
            });

            if (ui.getEvent(option_id, .activate_end)) |_| {
                return self.selectValue(ui, selected, i);
            }
        }
    }

    if (ui.getEvent(id, .activate_end)) |_| {
        if (self.highlighted_index) |hi| {
            // If dropdown is open, activation selects the highlighted item.
            return self.selectValue(ui, selected, hi);
        } else {
            // If dropdown is closed, activation opens it.
            try ui.pushFocusScope(id);

            // When opening, set highlight to the current selection
            self.highlighted_index = selected.*;
        }
    }

    if (ui.getEvent(id, .key_down)) |event| key_down: {
        if (self.highlighted_index == null) {
            // When closed and focused, allow arrow keys to open the dropdown.
            // Activation (Enter/Click) is handled by onActivate.
            switch (event.data.key_down.key) {
                .up, .down => {
                    try ui.pushFocusScope(id);

                    // When opening, set highlight to the current selection
                    self.highlighted_index = selected.*;
                },
                else => {},
            }
            break :key_down;
        }

        // When open, handle navigation and selection.
        switch (event.data.key_down.key) {
            .up => {
                if (self.highlighted_index) |idx| {
                    self.highlighted_index = (idx + options.len - 1) % options.len;
                } else {
                    self.highlighted_index = options.len - 1;
                }
            },
            .down => {
                if (self.highlighted_index) |idx| {
                    self.highlighted_index = (idx + 1) % options.len;
                } else {
                    self.highlighted_index = 0;
                }
            },
            .space => {
                // Space is a common activation key, handle it directly for convenience.
                return self.selectValue(ui, selected, self.highlighted_index.?);
            },
            // .enter is handled by the 'activate_end' event.
            // .escape is handled by the 'scoped_focus_close' event.
            else => {},
        }
    }

    if (ui.getEvent(id, .focus_lost)) |_| {
        // When the dropdown loses focus, always close the panel and pop the scope.
        if (self.highlighted_index != null) {
            self.highlighted_index = null;
            ui.popFocusScope();
        }
    }

    if (ui.getEvent(id, .scoped_focus_change)) |event| scoped_focus_change: {
        if (self.highlighted_index == null) break :scoped_focus_change;

        switch (event.data.scoped_focus_change) {
            .prev => { // Equivalent to Shift+Tab
                if (self.highlighted_index) |idx| {
                    self.highlighted_index = (idx + options.len - 1) % options.len;
                } else {
                    self.highlighted_index = options.len - 1;
                }
            },
            .next => { // Equivalent to Tab
                if (self.highlighted_index) |idx| {
                    self.highlighted_index = (idx + 1) % options.len;
                } else {
                    self.highlighted_index = 0;
                }
            },
        }
    }

    if (ui.getEvent(id, .scoped_focus_close)) |_| {
        // This handler is only called if this widget owns the scope.
        // It should close the dropdown.
        if (self.highlighted_index != null) {
            self.highlighted_index = null;
            ui.popFocusScope();
        }
    }

    return false;
}
