//! Menu widget subsystem.

const Menu = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

const log = std.log.scoped(.menu_widget);

test {
    log.debug("semantic analysis for widgets/Menu.zig", .{});
    std.testing.refAllDecls(@This());
}

// --- Theme ---

pub const Theme = struct {
    menu_separator_color: Ui.Color = Ui.Color.fromLinearU8(200, 200, 200, 255),
    menu_separator_height: f32 = 1.0,

    background_color: Ui.Color = .white,

    pub const BINDING_SET = Ui.Theme.Binding.Set.create(Theme);
};

pub const StandardTheme = struct {
    background_color: Ui.Color = .white,

    pub const BINDING_SET = Ui.Theme.Binding.Set.create(StandardTheme);
};

pub const Config = struct {
    disabled: bool = false,
};

pub const NavigableItem = struct {
    id: Ui.ElementId,
    is_widget: bool,
    disabled: bool = false,
};

// --- Internal State ---

navigable_items: std.ArrayList(NavigableItem),
submenu_map: std.AutoHashMapUnmanaged(u32, Ui.ElementId),
highlighted_index: ?usize,
hovered_submenu_candidate: ?Ui.ElementId,
hover_timer: std.time.Timer,

pub fn deinit(self: *Menu, ui: *Ui) void {
    self.navigable_items.deinit(ui.gpa);
    self.submenu_map.deinit(ui.gpa);
}

fn navigate(ui: *Ui, panel_id: Ui.ElementId, state: *Menu, dir: enum { next, prev }) void {
    const items = state.navigable_items.items;
    if (items.len == 0) return;

    var current_idx: ?usize = null;

    // First, check if global focus is currently resting on one of our widgets
    if (ui.state.focusedId()) |focused_id| {
        for (items, 0..) |it, i| {
            if (it.is_widget and it.id.id == focused_id.id) {
                current_idx = i;
                break;
            }
        }
    }
    // Otherwise fall back to the visual highlight
    if (current_idx == null) current_idx = state.highlighted_index;

    const new_idx: usize = switch (dir) {
        .next => if (current_idx) |idx| (idx + 1) % items.len else 0,
        .prev => if (current_idx) |idx| (idx + items.len - 1) % items.len else items.len - 1,
    };

    const target = items[new_idx];
    if (target.is_widget) {
        state.highlighted_index = null; // Clear visual highlight
        ui.state.focused_id = .{ // Grant actual focus to the widget
            .id = target.id,
            .bounding_box = ui.getElementBounds(target.id) orelse .{},
            .state = .flags(.{ .focus = true, .keyboard = true, .activate = true }),
        };
    } else {
        state.highlighted_index = new_idx; // Set visual highlight
        ui.state.focused_id = .{ // Steal actual focus back to the menu panel
            .id = panel_id,
            .bounding_box = ui.getElementBounds(panel_id) orelse .{},
            .state = .flags(.{ .focus = true, .keyboard = true, .activate = true }),
        };
    }
}

// --- Widget API ---

pub fn begin(ui: *Ui, id: Ui.ElementId) !bool {
    var is_open = false;
    var current_level: usize = 0;
    var info: Ui.OverlayState.OverlayInfo = undefined;

    for (ui.overlay_state.stack.items, 0..) |overlay_info, i| {
        if (overlay_info.id.id == id.id) {
            is_open = true;
            current_level = i;
            info = overlay_info;
            break;
        }
    }

    if (!is_open) return false;

    log.debug("Menu({s})", .{id.string_id.toSlice()});

    const self, const is_new = try ui.getOrCreateSharedWidgetState(Menu, id);
    if (is_new) {
        self.navigable_items = .empty;
        self.submenu_map = .empty;
        self.highlighted_index = 0;
        self.hovered_submenu_candidate = null;
        self.hover_timer = try std.time.Timer.start();
    }

    const is_first_frame = info.last_size[0] == 0;
    if (is_first_frame) {
        self.highlighted_index = 0;
    }

    // --- Layout and Positioning ---
    const viewport = ui.dimensions;
    var final_offset = info.position;
    var attach_points = Ui.FloatingAttachPoints{ .parent = .left_top, .element = .left_top };
    var attach_to: Ui.FloatingAttachToElement = .root;

    if (info.trigger_item_id) |trigger_id| {
        attach_to = .element_with_id;
        attach_points = .{ .parent = .right_top, .element = .left_top };

        if (!is_first_frame) {
            if (ui.getElementBounds(trigger_id)) |bb| {
                const right_edge_x = bb.x + bb.width;
                const goes_off_right = right_edge_x + info.last_size[0] > viewport[0];

                // Check if the bottom of the menu will exceed the viewport height
                const bottom_edge_y = bb.y + info.last_size[1];
                const goes_off_bottom = bottom_edge_y > viewport[1];

                if (goes_off_right and goes_off_bottom) {
                    attach_points = .{ .parent = .left_bottom, .element = .right_bottom };
                } else if (goes_off_right) {
                    attach_points = .{ .parent = .left_top, .element = .right_top };
                } else if (goes_off_bottom) {
                    attach_points = .{ .parent = .right_bottom, .element = .left_bottom };
                }
            }
        }
    } else {
        if (!is_first_frame) {
            final_offset[0] = std.math.clamp(final_offset[0], 0, viewport[0] - info.last_size[0]);
            final_offset[1] = std.math.clamp(final_offset[1], 0, viewport[1] - info.last_size[1]);
        }
    }

    const applied_offset = if (is_first_frame) vec2{ -99999.0, -99999.0 } else if (attach_to == .element_with_id) vec2{ 0, 0 } else final_offset;

    try ui.openElement(id);

    var theme = Theme{};
    try ui.applyThemeState(&Theme.BINDING_SET, .widget, ui.getActionState(), &theme);
    var standard_theme = StandardTheme{};
    try ui.applyThemeState(&StandardTheme.BINDING_SET, .widget, if (ui.disabled()) .disabled else .standard, &standard_theme);

    try ui.configureElement(.{
        .sizing = .{ .w = .fit, .h = .fit },
        .direction = .top_to_bottom,
        .child_gap = 0,
        .padding_left = 0,
        .padding_right = 0,
        .padding_top = 0,
        .padding_bottom = 0,
        .background_color = standard_theme.background_color,
        .floating = .{
            .attach_to = attach_to,
            .parentId = (info.trigger_item_id orelse Ui.ElementId{}).id,
            .offset = applied_offset,
            .attach_points = attach_points,
            .z_index = @intCast(Ui.OverlayState.OVERLAY_Z_INDEX_BASE + current_level),
        },
        .type = .layout_widget,
        .event_flags = .{ .focus = true, .keyboard = true, .activate = true },
    });

    // --- Process Input ---
    if (ui.getEvent(id, .scoped_focus_change)) |event| {
        navigate(ui, id, self, switch (event.data.scoped_focus_change) {
            .next => .next,
            .prev => .prev,
        });
    }

    if (ui.getEvent(id, .key_down)) |event| {
        const key = event.data.key_down.key;
        const items = self.navigable_items.items;

        if (items.len > 0) {
            switch (key) {
                .down => navigate(ui, id, self, .next),
                .up => navigate(ui, id, self, .prev),
                .right => {
                    if (self.highlighted_index) |idx| {
                        if (idx < items.len) {
                            const item_id = items[idx].id;
                            if (self.submenu_map.get(item_id.id)) |child_id| {
                                ui.openOverlay(child_id, id, item_id, .{ 0, 0 });
                            }
                        }
                    }
                },
                .left => {
                    if (info.parent_overlay_id != null) ui.closeTopOverlay();
                },
                else => {},
            }
        }
    }

    if (ui.getEvent(id, .activate_end)) |_| {
        if (self.highlighted_index) |idx| {
            if (idx < self.navigable_items.items.len) {
                const item_id = self.navigable_items.items[idx].id;
                if (self.submenu_map.get(item_id.id)) |child_id| {
                    ui.openOverlay(child_id, id, item_id, .{ 0, 0 });
                } else {
                    try ui.pushEvent(item_id, .activate_begin, null);
                    try ui.pushEvent(item_id, .{ .activate_end = .{ .end_element = item_id, .modifiers = .{} } }, null);
                    ui.closeAllOverlays();
                }
            }
        }
    }

    if (ui.getEvent(id, .scoped_focus_close)) |_| {
        ui.closeTopOverlay();
    }

    self.navigable_items.clearRetainingCapacity();
    self.submenu_map.clearRetainingCapacity();

    return true;
}

pub fn end(ui: *Ui) void {
    const menu_id = ui.open_ids.items[ui.open_ids.items.len - 1];

    if (ui.shared_widget_states.getPtr(menu_id.id)) |entry| {
        if (entry.state_type == @as(*const anyopaque, @typeName(Menu))) {
            const state: *Menu = @ptrCast(@alignCast(entry.data));

            if (ui.isHovered(menu_id)) {
                var hovered_nav_idx: ?usize = null;
                for (state.navigable_items.items, 0..) |it, i| {
                    if (!it.disabled and ui.isHovered(it.id)) {
                        hovered_nav_idx = i;
                        break;
                    }
                }

                if (hovered_nav_idx) |idx| {
                    if (state.highlighted_index != idx) {
                        state.highlighted_index = idx;

                        var current_level: usize = 0;
                        for (ui.overlay_state.stack.items, 0..) |info, i| {
                            if (info.id.id == menu_id.id) {
                                current_level = i;
                                break;
                            }
                        }

                        if (ui.overlay_state.stack.items.len > current_level + 1) {
                            const hovered_id = state.navigable_items.items[idx].id;
                            var deeper_is_ours = false;
                            if (state.submenu_map.get(hovered_id.id)) |child_id| {
                                deeper_is_ours = ui.overlay_state.stack.items[current_level + 1].id.id == child_id.id;
                            }

                            if (!deeper_is_ours) {
                                ui.overlay_state.close_request_level = current_level + 1;
                            }
                        }
                    }

                    // Steal focus back to the menu panel (unless we are hovering an embedded navigable widget)
                    if (!state.navigable_items.items[idx].is_widget) {
                        if (ui.getElementBounds(menu_id)) |bb| {
                            const current_focus = ui.state.focusedIdValue() orelse 0;
                            if (current_focus != menu_id.id) {
                                ui.state.focused_id = .{
                                    .id = menu_id,
                                    .bounding_box = bb,
                                    .state = .flags(.{ .focus = true, .keyboard = true, .activate = true }),
                                };
                            }
                        }
                    }
                } else {
                    // Hovering the menu, but NOT over a valid navigable item
                    // Clear the highlight and close any open submenus.
                    if (state.highlighted_index != null) {
                        state.highlighted_index = null;

                        var current_level: usize = 0;
                        for (ui.overlay_state.stack.items, 0..) |info, i| {
                            if (info.id.id == menu_id.id) {
                                current_level = i;
                                break;
                            }
                        }
                        if (ui.overlay_state.stack.items.len > current_level + 1) {
                            ui.overlay_state.close_request_level = current_level + 1;
                        }
                    }
                }
            }
        }
    }

    ui.endElement();
}

pub fn item(ui: *Ui, id: Ui.ElementId, label: []const u8, config: Config) !bool {
    const menu_id = ui.open_ids.items[ui.open_ids.items.len - 1];
    const state, _ = try ui.getOrCreateSharedWidgetState(Menu, menu_id);

    const index = state.navigable_items.items.len;
    try state.navigable_items.append(ui.gpa, .{ .id = id, .is_widget = false, .disabled = config.disabled });

    const is_highlighted = state.highlighted_index == index;

    try ui.openElement(id);
    defer ui.endElement();

    var theme = Theme{};
    const action_state: Ui.ActionState = if (config.disabled) .disabled else if (is_highlighted) .focus else ui.getActionState();
    try ui.applyThemeState(&Theme.BINDING_SET, .widget, action_state, &theme);

    try ui.configureElement(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .child_alignment = .center,
        .state = action_state,
        .background_color = if (action_state == .standard) .transparent else theme.background_color,
        .border_left = 0,
        .border_right = 0,
        .border_bottom = 0,
        .border_between_children = 0,
        .border_top = 0,
        .event_flags = .{ .activate = true, .hover = true, .click = true },
        .type = .layout_widget,
    });
    try ui.text(label, .{});

    if (ui.getEvent(id, .activate_end)) |_| {
        ui.closeAllOverlays();
        return true;
    }
    return false;
}

pub const EmbedConfig = struct {
    sizing: Ui.Sizing = .{ .w = .grow, .h = .fit },
};

pub fn beginEmbeddedLayout(ui: *Ui, section_id: Ui.ElementId, config: EmbedConfig) !void {
    var standard_theme = StandardTheme{};
    try ui.openElement(section_id);
    try ui.applyThemeState(&StandardTheme.BINDING_SET, .widget, if (ui.disabled()) .disabled else .standard, &standard_theme);
    try ui.configureElement(.{
        .sizing = config.sizing,
        .type = .layout_widget,
        .background_color = standard_theme.background_color,
        .direction = .top_to_bottom,
        .border_left = 0,
        .border_right = 0,
        .border_bottom = 0,
        .border_between_children = 0,
        .border_top = 0,
    });
}

pub fn endEmbeddedLayout(ui: *Ui) void {
    ui.endElement();
}

pub fn subMenu(ui: *Ui, self_id: Ui.ElementId, label: []const u8, child_menu_id: Ui.ElementId, config: Config) !bool {
    log.debug("Submenu({s} -> {s})", .{ self_id.string_id.toSlice(), child_menu_id.string_id.toSlice() });

    const menu_id = ui.open_ids.items[ui.open_ids.items.len - 1];
    const state, _ = try ui.getOrCreateSharedWidgetState(Menu, menu_id);

    const index = state.navigable_items.items.len;
    try state.navigable_items.append(ui.gpa, .{ .id = self_id, .is_widget = false, .disabled = config.disabled });
    try state.submenu_map.put(ui.gpa, self_id.id, child_menu_id);

    try ui.openElement(self_id);
    defer ui.endElement();

    const is_highlighted = state.highlighted_index == index;

    var is_child_open = false;
    for (ui.overlay_state.stack.items) |info| {
        if (info.id.id == child_menu_id.id) {
            is_child_open = true;
            break;
        }
    }

    var theme = Theme{};
    const eff_highlighted = is_highlighted or is_child_open;
    const action_state: Ui.ActionState = if (config.disabled) .disabled else if (eff_highlighted) .focus else ui.getActionState();
    try ui.applyThemeState(&Theme.BINDING_SET, .widget, action_state, &theme);

    try ui.configureElement(.{
        .sizing = .{ .w = .grow, .h = .fit },
        .direction = .left_to_right,
        .child_alignment = .center,
        .border_left = 0,
        .border_right = 0,
        .border_bottom = 0,
        .border_between_children = 0,
        .border_top = 0,
        .background_color = if (action_state == .standard) .transparent else theme.background_color,
        .state = action_state,
        .event_flags = .{ .activate = true, .hover = true },
        .type = .layout_widget,
    });

    const str = try std.fmt.allocPrint(ui.frame_arena, "{s} >", .{label});
    try ui.text(str, .{});

    if (ui.hovered() and !config.disabled) {
        if (!is_child_open) {
            if (state.hovered_submenu_candidate) |candidate| {
                if (candidate.id != self_id.id) {
                    state.hovered_submenu_candidate = self_id;
                    state.hover_timer.reset();
                } else if (state.hover_timer.read() > 200 * std.time.ns_per_ms) {
                    ui.openOverlay(child_menu_id, menu_id, self_id, .{ 0, 0 });
                    state.hovered_submenu_candidate = null;
                }
            } else {
                state.hovered_submenu_candidate = self_id;
                state.hover_timer.reset();
            }
        }
    } else if (state.hovered_submenu_candidate) |candidate| {
        if (candidate.id == self_id.id) {
            state.hovered_submenu_candidate = null;
        }
    }

    if (ui.getEvent(self_id, .activate_end)) |_| {
        ui.openOverlay(child_menu_id, menu_id, self_id, .{ 0, 0 });
    }

    return is_child_open;
}

pub fn separator(ui: *Ui, id: Ui.ElementId) !void {
    try ui.openElement(id);
    defer ui.endElement();

    var theme = Theme{};
    try ui.applyThemeState(&Theme.BINDING_SET, .widget, .standard, &theme);

    try ui.configureElement(.{
        .sizing = .{ .w = .grow, .h = .fixed(theme.menu_separator_height) },
        .background_color = theme.menu_separator_color,
        .type = .layout_widget,
    });
}

pub fn navigable(ui: *Ui) !void {
    std.debug.assert(ui.open_ids.items.len >= 1);
    const element_id = ui.open_ids.items[ui.open_ids.items.len - 1];

    var i = ui.open_ids.items.len;
    while (i > 0) {
        i -= 1;
        const ancestor_id = ui.open_ids.items[i];

        if (ui.shared_widget_states.getPtr(ancestor_id.id)) |entry| {
            if (entry.state_type == @as(*const anyopaque, @typeName(Menu))) {
                const state_ptr: *Menu = @ptrCast(@alignCast(entry.data));
                try state_ptr.navigable_items.append(ui.gpa, .{ .id = element_id, .is_widget = true, .disabled = false });
                return;
            }
        }
    }
}
