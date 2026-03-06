//! Dropdown widget for selecting one of multiple enum values.

const Dropdown = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");

const log = std.log.scoped(.dropdown_widget);

test {
    log.debug("semantic analysis for widgets/Dropdown.zig", .{});
    std.testing.refAllDecls(@This());
}

value: *usize,
options: []const []const u8,
highlighted_index: ?usize,
theme: Theme,

pub const Theme = struct {
    corner_radius: Ui.CornerRadius = .{},
};

pub fn deinit(self: *Dropdown, ui: *Ui) void {
    ui.gpa.destroy(self);
}

pub fn render(_: *Dropdown, _: *Ui, _: Ui.RenderCommand) !void {
    // This widget is rendered entirely via its `declare` method.
}

fn panelId(ui: *Ui, id: Ui.ElementId) !Ui.ElementId {
    return Ui.ElementId.fromSlice(try std.fmt.allocPrint(ui.frame_arena, "{s}_dropdown_panel", .{id.string_id.toSlice()}));
}

fn optionId(ui: *Ui, id: Ui.ElementId, name: []const u8) !Ui.ElementId {
    return Ui.ElementId.fromSlice(try std.fmt.allocPrint(ui.frame_arena, "{s}_option_{s}", .{ id.string_id.toSlice(), name }));
}

fn selectValue(self: *Dropdown, ui: *Ui, value: usize) !bool {
    ui.popFocusScope();
    const changed = self.value.* != value;
    log.info("selectValue: {d}, {d}, {any}", .{ self.value.*, value, changed });
    self.value.* = value;
    self.highlighted_index = null;
    return changed;
}

pub fn enumDropdown(ui: *Ui, id: Ui.ElementId, comptime T: type, selected: *T) !bool {
    var state: usize = inline for (comptime std.meta.fieldNames(T), 0..) |field_name, i| {
        if (@field(T, field_name) == selected.*) break i;
    } else unreachable;

    if (try dropdown(ui, id, &state, std.meta.fieldNames(T))) {
        inline for (comptime std.meta.fieldNames(T), 0..) |field_name, i| {
            if (i == state) {
                log.info("enumDropdown changed {any} -> {any}", .{ selected.*, @field(T, field_name) });
                selected.* = @field(T, field_name);
                return true;
            }
        }
        unreachable;
    }
    return false;
}

pub const THEME_BINDING_SET = Ui.Theme.Binding.Set.create(Theme);

pub fn dropdown(ui: *Ui, id: Ui.ElementId, selected: *usize, options: []const []const u8) !bool {
    const self, const is_new = try ui.getOrCreateWidget(Dropdown, id);

    self.value = selected;
    self.options = options;
    self.theme = .{};
    if (is_new) self.highlighted_index = null;

    try ui.beginSection(id, .{
        .sizing = .{ .w = .grow, .h = .fit },
        .child_alignment = .center,
        .type = .layout_widget,
        .event_flags = .{ .activate = true, .focus = true, .keyboard = true },
    });
    defer ui.endSection();

    try ui.applyTheme(&THEME_BINDING_SET, .widget, &self.theme);

    try ui.textSection(options[self.value.*], .{ .alignment = .center });

    if (self.highlighted_index) |hi| {
        const panel_id = try panelId(ui, id);
        try ui.beginSection(panel_id, .{
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
        defer ui.endSection();

        for (options, 0..) |field_name, i| {
            const option_id = try optionId(ui, id, field_name);

            try ui.beginSection(option_id, .{
                .sizing = .{ .w = .grow, .h = .fit },
                .child_alignment = .center,
                .event_flags = .{ .activate = true },
                .state = if (hi == i) .focus else null,
                .border_width = .all(0),
                .type = .layout_widget,
                .corner_radius = if (i == 0)
                    Ui.CornerRadius{ .top_left = self.theme.corner_radius.top_left, .top_right = self.theme.corner_radius.top_right }
                else if (i == options.len - 1)
                    Ui.CornerRadius{ .bottom_left = self.theme.corner_radius.bottom_left, .bottom_right = self.theme.corner_radius.bottom_right }
                else
                    .all(0),
            });
            defer ui.endSection();

            try ui.textSection(field_name, .{
                .alignment = .center,
                .state = if (hi == i) .focus else null,
            });

            if (ui.getEvent(option_id, .activate_end)) |_| {
                log.info("activated {s}", .{field_name});
                return self.selectValue(ui, i);
            }
        }
    }

    if (ui.getEvent(id, .activate_end)) |_| {
        log.info("activated main", .{});
        if (self.highlighted_index) |hi| {
            log.info("Selecting {d}", .{hi});
            // If dropdown is open, activation selects the highlighted item.
            return self.selectValue(ui, hi);
        } else {
            log.info("Opening", .{});
            // If dropdown is closed, activation opens it.
            try ui.pushFocusScope(id);

            // When opening, set highlight to the current selection
            self.highlighted_index = self.value.*;
        }
    }

    if (ui.getEvent(id, .key_down)) |event| key_down: {
        log.info("keydown {any}", .{event});
        if (self.highlighted_index == null) {
            // When closed and focused, allow arrow keys to open the dropdown.
            // Activation (Enter/Click) is handled by onActivate.
            switch (event.data.key_down.key) {
                .up, .down => {
                    try ui.pushFocusScope(id);

                    // When opening, set highlight to the current selection
                    self.highlighted_index = self.value.*;
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
                return self.selectValue(ui, self.highlighted_index.?);
            },
            // .enter is handled by the 'activate_end' event.
            // .escape is handled by the 'scoped_focus_close' event.
            else => {},
        }
    }

    if (ui.getEvent(id, .focus_lost)) |_| {
        log.info(".focus_lost", .{});

        // When the dropdown loses focus, always close the panel and pop the scope.
        if (self.highlighted_index != null) {
            self.highlighted_index = null;
            ui.popFocusScope();
        }
    }

    if (ui.getEvent(id, .scoped_focus_change)) |event| scoped_focus_change: {
        log.info(".scope_focus_change", .{});

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
        log.info(".scope_focus_closed", .{});
        // This handler is only called if this widget owns the scope.
        // It should close the dropdown.
        if (self.highlighted_index != null) {
            self.highlighted_index = null;
            ui.popFocusScope();
        }
    }

    return false;
}
