//! Text input widget state management and event handling.

const TextInput = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");
const Batch2D = @import("../Batch2D.zig");
const BindingState = @import("../BindingState.zig");

const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

const log = std.log.scoped(.text_input_widget);

test {
    log.debug("semantic analysis for widgets/TextInput.zig", .{});
    std.testing.refAllDecls(@This());
}

id: Ui.ElementId,
text: *std.ArrayList(u8),
allocator: std.mem.Allocator,

carets: std.ArrayList(Caret), // = .empty,
spatial_actions: std.ArrayList(SpatialAction), // = .empty,

column_select_start_pos: ?u32, // = null,
drag_additive: u32, // = 0,

theme: Theme,

pub const Theme = struct {
    /// The RGBA color of selection highlights.
    text_selection_color: Ui.Color = .{ .r = 0.75, .g = 0, .b = 0.75, .a = 0.5 },
    /// The RGBA color of selection carets.
    text_caret_color: Ui.Color = .{ .r = 1, .a = 1 },

    pub const BINDING_SET = Ui.Theme.Binding.Set.create(Theme);
};

const SpatialAction = union(enum) {
    mouse_down: struct { location: vec2, modifiers: BindingState.Modifiers },
    mouse_up: void,
    drag: struct { location: vec2, modifiers: BindingState.Modifiers },
    move_up: BindingState.Modifiers,
    move_down: BindingState.Modifiers,
    home: BindingState.Modifiers,
    end: BindingState.Modifiers,
};

// pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*TextInput {
//     const self = try ui.gpa.create(TextInput);
//     errdefer ui.gpa.destroy(self);

//     self.* = TextInput{
//         .id = id,
//         .text = config.text,
//         .allocator = config.allocator,
//         .selection_color = config.selection_color,
//         .caret_color = config.caret_color,
//     };

//     // Start the cursor at the end of whatever text initially exists
//     const text_len: u32 = @intCast(config.text.items.len);
//     try self.carets.append(ui.gpa, .{ .start = text_len, .end = text_len });

//     return self;
// }

pub fn deinit(self: *TextInput, ui: *Ui) void {
    self.carets.deinit(ui.gpa);
    self.spatial_actions.deinit(ui.gpa);
}

const Caret = struct {
    start: u32,
    end: u32,

    fn min(self: Caret) u32 {
        return @min(self.start, self.end);
    }
    fn max(self: Caret) u32 {
        return @max(self.start, self.end);
    }
    fn hasSelection(self: Caret) bool {
        return self.start != self.end;
    }
};

const SelectionRenderData = struct {
    carets: *const std.ArrayList(Caret),
};

/// Sorts carets by position and merges any that are overlapping or adjacent.
fn sortAndMergeCarets(caret_list: *std.ArrayList(Caret)) !void {
    if (caret_list.items.len <= 1) return;

    // Sort by start position
    std.mem.sort(Caret, caret_list.items, {}, struct {
        fn lt(_: void, a: Caret, b: Caret) bool {
            return a.min() < b.min();
        }
    }.lt);

    // Merge overlapping selections
    var i: usize = 1;
    while (i < caret_list.items.len) {
        const prev_max = caret_list.items[i - 1].max();
        const curr_min = caret_list.items[i].min();

        if (prev_max >= curr_min) {
            // Overlap detected, merge curr into prev
            const prev_min = caret_list.items[i - 1].min();
            const curr_max = caret_list.items[i].max();
            caret_list.items[i - 1].start = prev_min;
            caret_list.items[i - 1].end = @max(prev_max, curr_max);
            _ = caret_list.orderedRemove(i);
            // Do not increment i, as the new element at i needs to be checked against i-1
        } else {
            i += 1;
        }
    }
}

/// Rebuilds the text string by applying a given modification at each caret.
/// Mutates the caller's text buffer directly.
fn applyTextModification(
    self: *TextInput,
    ui: *Ui,
    action: enum { insert, delete, backspace },
    payload: []const u8,
) !void {
    try sortAndMergeCarets(&self.carets);

    var new_text = std.ArrayList(u8).empty;
    var new_carets = std.ArrayList(Caret).empty;
    errdefer {
        new_text.deinit(ui.gpa);
        new_carets.deinit(ui.gpa);
    }

    const source_text = self.text.items;

    var last_idx: u32 = 0;
    for (self.carets.items) |caret| {
        if (caret.hasSelection()) {
            // Copy text before the current selection
            try new_text.appendSlice(ui.gpa, source_text[last_idx..caret.min()]);
            // For inserts (like paste), add the payload here. For deletes, do nothing.
            if (action == .insert) {
                try new_text.appendSlice(ui.gpa, payload);
            }
            // Advance the cursor in the old text string past the selection
            last_idx = caret.max();
        } else { // No selection, just a cursor.
            switch (action) {
                .insert => {
                    // Copy up to the cursor, insert payload, and update last_idx
                    try new_text.appendSlice(ui.gpa, source_text[last_idx..caret.end]);
                    try new_text.appendSlice(ui.gpa, payload);
                    last_idx = caret.end;
                },
                .delete => {
                    // Copy up to the cursor
                    try new_text.appendSlice(ui.gpa, source_text[last_idx..caret.end]);
                    // Advance last_idx past the next UTF-8 character to effectively delete it
                    if (caret.end < source_text.len) {
                        var delete_end = caret.end + 1;
                        while (delete_end < source_text.len and (source_text[delete_end] & 0b1100_0000) == 0b1000_0000) : (delete_end += 1) {}
                        last_idx = delete_end;
                    } else {
                        last_idx = caret.end;
                    }
                },
                .backspace => {
                    // Find the start of the previous UTF-8 character
                    var delete_start = if (caret.end > 0) caret.end - 1 else 0;
                    while (delete_start > 0 and (source_text[delete_start] & 0b1100_0000) == 0b1000_0000) : (delete_start -= 1) {}
                    // Copy up to that point
                    try new_text.appendSlice(ui.gpa, source_text[last_idx..delete_start]);
                    // Advance last_idx past the cursor to effectively delete the character
                    last_idx = caret.end;
                },
            }
        }
        // Add a new, collapsed caret at the end of the modified section
        const new_pos: u32 = @intCast(new_text.items.len);
        try new_carets.append(ui.gpa, .{ .start = new_pos, .end = new_pos });
    }

    // Append any remaining text after the last caret
    try new_text.appendSlice(ui.gpa, source_text[last_idx..]);

    // Write directly back to the user's string buffer
    self.text.clearRetainingCapacity();
    try self.text.appendSlice(self.allocator, new_text.items);
    new_text.deinit(ui.gpa);

    self.carets.deinit(ui.gpa);
    self.carets = new_carets;
}

/// Finds the index of the start of the next word.
fn findNextWordBreak(text: []const u8, start_index: u32) u32 {
    if (start_index >= text.len) return @intCast(text.len);
    var idx: u32 = start_index;

    // Skip any initial whitespace
    while (idx < text.len and std.ascii.isWhitespace(text[idx])) : (idx += 1) {}
    // Skip non-whitespace characters
    while (idx < text.len and !std.ascii.isWhitespace(text[idx])) : (idx += 1) {}

    return idx;
}

/// Finds the index of the start of the previous word.
fn findPrevWordBreak(text: []const u8, start_index: u32) u32 {
    if (start_index == 0) return 0;
    var idx: u32 = start_index - 1;

    // Skip any initial whitespace (backwards)
    while (idx > 0 and std.ascii.isWhitespace(text[idx])) : (idx -= 1) {}
    // Skip non-whitespace characters (backwards)
    while (idx > 0 and !std.ascii.isWhitespace(text[idx])) : (idx -= 1) {}

    // If we landed on whitespace, move forward one to be at the start of the word.
    if (idx > 0 and std.ascii.isWhitespace(text[idx])) {
        idx += 1;
    }

    return idx;
}

pub fn render(self: *TextInput, ui: *Ui, command: Ui.RenderCommand) !void {
    const data = command.render_data.custom;
    const color = Ui.clayColorToBatchColor(data.background_color);
    const radius = Batch2D.CornerRadius{
        .top_left = data.corner_radius.top_left,
        .top_right = data.corner_radius.top_right,
        .bottom_right = data.corner_radius.bottom_right,
        .bottom_left = data.corner_radius.bottom_left,
    };

    // Draw the background
    try ui.renderer.drawRoundedRect(
        .{ command.bounding_box.x, command.bounding_box.y },
        .{ command.bounding_box.width, command.bounding_box.height },
        radius,
        color,
    );

    // =========================================================================
    // SPATIAL ACTION RESOLUTION
    // This executes *after* Ui.endLayout(), so the Clay tree is valid
    // and querying it will no longer crash.
    // =========================================================================
    for (self.spatial_actions.items) |action| {
        switch (action) {
            .mouse_down => |d| {
                if (ui.getCharacterIndexAtOffset(self.id, d.location)) |index| {
                    if (d.modifiers.alt) {
                        self.column_select_start_pos = index;
                        if (d.modifiers.shift) {
                            self.drag_additive = @intCast(self.carets.items.len + 1);
                        } else {
                            self.carets.clearRetainingCapacity();
                            self.drag_additive = 1;
                        }
                        try self.carets.append(ui.gpa, .{ .start = index, .end = index });
                    } else if (d.modifiers.shift and self.carets.items.len > 0) {
                        self.carets.items[self.carets.items.len - 1].end = index;
                    } else {
                        self.carets.clearRetainingCapacity();
                        try self.carets.append(ui.gpa, .{ .start = index, .end = index });
                    }
                }
            },
            .mouse_up => {
                try sortAndMergeCarets(&self.carets);
                self.column_select_start_pos = null;
                self.drag_additive = 0;
            },
            .drag => |d| {
                if (self.column_select_start_pos) |start_index| {
                    // --- COLUMN SELECTION mode ---
                    const start_offset = ui.getCharacterOffset(self.id, start_index) orelse continue;
                    const current_offset = ui.getCharacterOffsetAtPoint(self.id, d.location) orelse continue;

                    const rect_x1 = @min(start_offset.offset.x, current_offset.offset.x);
                    const rect_x2 = @max(start_offset.offset.x, current_offset.offset.x);
                    const rect_y1 = @min(start_offset.offset.y, current_offset.offset.y);
                    const rect_y2 = @max(start_offset.offset.y, current_offset.offset.y) + current_offset.line_height;

                    const base = @min(self.drag_additive, self.carets.items.len);
                    self.carets.shrinkRetainingCapacity(base);

                    var scan_index: u32 = 0;
                    while (scan_index < self.text.items.len) {
                        const char_offset = ui.getCharacterOffset(self.id, scan_index) orelse {
                            scan_index += 1;
                            continue;
                        };

                        const line_y = char_offset.offset.y;
                        const line_h = char_offset.line_height;

                        if (line_y + line_h > rect_y1 and line_y < rect_y2) {
                            const start_char_opt = ui.getCharacterIndexAtOffset(self.id, .{ rect_x1, line_y });
                            const end_char_opt = ui.getCharacterIndexAtOffset(self.id, .{ rect_x2, line_y });

                            if (start_char_opt != null and end_char_opt != null) {
                                const sel_start = @min(start_char_opt.?, end_char_opt.?);
                                const sel_end = @max(start_char_opt.?, end_char_opt.?);
                                try self.carets.append(ui.gpa, .{ .start = sel_start, .end = sel_end });
                            }
                        }

                        var next_scan_index = scan_index;
                        while (next_scan_index < self.text.items.len) {
                            const next_char_offset = ui.getCharacterOffset(self.id, next_scan_index) orelse break;
                            if (next_char_offset.offset.y > line_y) break;
                            next_scan_index += 1;
                        }

                        if (next_scan_index == scan_index) break;
                        scan_index = next_scan_index;
                    }
                } else {
                    // --- NORMAL selection mode ---
                    if (ui.getCharacterIndexAtOffset(self.id, d.location)) |offset| {
                        if (self.carets.items.len > 0) {
                            self.carets.items[self.carets.items.len - 1].end = offset;
                        }
                    }
                }
            },
            .move_up, .move_down => |action_mod| {
                const is_up = action == .move_up;
                var new_carets_to_add = std.ArrayList(Caret).empty;
                defer new_carets_to_add.deinit(ui.gpa);

                for (self.carets.items) |*caret| {
                    const start_offset_res = ui.getCharacterOffset(self.id, caret.start) orelse continue;
                    const end_offset_res = ui.getCharacterOffset(self.id, caret.end) orelse continue;

                    const target_y_offset = if (is_up) -start_offset_res.line_height else start_offset_res.line_height;

                    if (action_mod.alt) {
                        const target_start_loc = vec2{ start_offset_res.offset.x, start_offset_res.offset.y + target_y_offset };
                        const new_start_res = ui.getCharacterIndexAtOffset(self.id, target_start_loc) orelse continue;

                        const target_end_loc = vec2{ end_offset_res.offset.x, end_offset_res.offset.y + target_y_offset };
                        const new_end_res = ui.getCharacterIndexAtOffset(self.id, target_end_loc) orelse continue;

                        try new_carets_to_add.append(ui.gpa, .{ .start = new_start_res, .end = new_end_res });
                    } else if (action_mod.shift) {
                        const target_end_loc = vec2{ end_offset_res.offset.x, end_offset_res.offset.y + target_y_offset };
                        const new_end_res = ui.getCharacterIndexAtOffset(self.id, target_end_loc) orelse continue;
                        caret.end = new_end_res;
                    } else {
                        const target_end_loc = vec2{ end_offset_res.offset.x, end_offset_res.offset.y + target_y_offset };
                        const new_end_res = ui.getCharacterIndexAtOffset(self.id, target_end_loc) orelse continue;
                        caret.start = new_end_res;
                        caret.end = new_end_res;
                    }
                }
                try self.carets.appendSlice(ui.gpa, new_carets_to_add.items);
                try sortAndMergeCarets(&self.carets);
            },
            .home => |action_mod| {
                for (self.carets.items) |*caret| {
                    if (action_mod.ctrl) {
                        caret.end = 0;
                    } else if (ui.getCharacterOffset(self.id, caret.end)) |offset| {
                        const target_loc = vec2{ 0, offset.offset.y };
                        if (ui.getCharacterIndexAtOffset(self.id, target_loc)) |target_index| caret.end = target_index;
                    }
                    if (!action_mod.shift) caret.start = caret.end;
                }
            },
            .end => |action_mod| {
                for (self.carets.items) |*caret| {
                    if (action_mod.ctrl) {
                        caret.end = @intCast(self.text.items.len);
                    } else {
                        if (ui.getCharacterOffset(self.id, caret.end)) |current_offset_res| {
                            const end_of_line_target_loc = vec2{ 999999.0, current_offset_res.offset.y };
                            if (ui.getCharacterIndexAtOffset(self.id, end_of_line_target_loc)) |end_of_line_res| {
                                var last_char_idx = end_of_line_res;
                                const check_offset_res = ui.getCharacterOffset(self.id, last_char_idx);
                                if (check_offset_res != null and check_offset_res.?.offset.y != current_offset_res.offset.y and last_char_idx > 0) {
                                    var prev_char_start = last_char_idx - 1;
                                    while (prev_char_start > 0 and (self.text.items[prev_char_start] & 0b1100_0000) == 0b1000_0000) {
                                        prev_char_start -= 1;
                                    }
                                    last_char_idx = prev_char_start;
                                }
                                caret.end = last_char_idx;
                            }
                        }
                    }
                    if (!action_mod.shift) caret.start = caret.end;
                }
            },
        }
    }
    self.spatial_actions.clearRetainingCapacity();

    // =========================================================================
    // DRAWING
    // =========================================================================
    if (ui.state.focusedIdValue() != command.id) {
        return;
    }

    const caret_list = self.carets;
    const caret_width: f32 = 1.5;

    for (caret_list.items) |caret| {
        // Draw selection highlight
        if (caret.hasSelection()) {
            const start_idx = caret.min();
            const end_idx = caret.max();

            var i = start_idx;
            while (i < end_idx) {
                const start_of_line = i;

                const start_offset = ui.getCharacterOffset(.fromRawId(command.id), start_of_line) orelse {
                    i += 1;
                    continue;
                };

                // Find the end of the current line within the selection
                var end_of_line = i;
                while (end_of_line + 1 < end_idx) {
                    const next_offset = ui.getCharacterOffset(.fromRawId(command.id), end_of_line + 1) orelse break;
                    if (next_offset.offset.y != start_offset.offset.y) {
                        break;
                    }
                    end_of_line += 1;
                }

                const end_offset = ui.getCharacterOffset(.fromRawId(command.id), end_of_line) orelse {
                    return error.InvalidLineOffset;
                };
                const next_char_after_end_offset = ui.getCharacterOffset(.fromRawId(command.id), end_of_line + 1);

                const rect_x = command.bounding_box.x + start_offset.offset.x;
                const rect_y = command.bounding_box.y + start_offset.offset.y;
                const rect_h = start_offset.line_height;

                // Determine the width of the selection rectangle for this line
                const rect_w = if (next_char_after_end_offset != null and next_char_after_end_offset.?.offset.y == end_offset.offset.y)
                    // The selection ends mid-line
                    next_char_after_end_offset.?.offset.x - start_offset.offset.x
                else
                    // The selection covers the rest of the line
                    end_offset.offset.x - start_offset.offset.x;

                try ui.renderer.drawRect(.{ rect_x, rect_y }, .{ rect_w, rect_h }, self.theme.text_selection_color);

                i = end_of_line + 1;
            }
        }

        // Draw caret at the end position
        if (ui.getCharacterOffset(.fromRawId(command.id), caret.end)) |caret_offset| {
            const caret_x = command.bounding_box.x + caret_offset.offset.x;
            const caret_y = command.bounding_box.y + caret_offset.offset.y;
            try ui.renderer.drawRect(.{ caret_x, caret_y }, .{ caret_width, caret_offset.line_height }, self.theme.text_caret_color);
        }
    }
}

pub const Config = struct {
    sizing: ?Ui.Sizing = null,
    disabled: bool = false,
};

/// Configure an open element as a text input widget, applying mutative state to the passed-in config text buffer.
pub fn textInput(ui: *Ui, id: Ui.ElementId, allocator: std.mem.Allocator, text: *std.ArrayList(u8), config: Config) !bool {
    const self, const new = try ui.getOrCreateWidget(TextInput, id);

    if (new) {
        self.id = id;
        self.allocator = allocator;
        self.text = text;
        self.column_select_start_pos = null;
        self.drag_additive = 0;
        self.carets = .empty;
        self.spatial_actions = .empty;
    } else if (self.text != text) {
        self.text = text;
        self.allocator = allocator;
        self.column_select_start_pos = null;
        self.drag_additive = 0;
        self.carets.clearRetainingCapacity();
        self.spatial_actions.clearRetainingCapacity();
    } else {
        std.debug.assert(self.allocator.ptr == allocator.ptr);
    }

    if (self.carets.items.len == 0) {
        // Start the cursor at the end of whatever text initially exists
        const text_len: u32 = @intCast(self.text.items.len);
        try self.carets.append(ui.gpa, .{ .start = text_len, .end = text_len });
    }

    self.theme = .{};

    try ui.openElement(id);
    defer ui.endElement();

    try ui.configureElement(.{
        .sizing = config.sizing,
        .clip = .{
            .vertical = true,
            .child_offset = ui.scrollOffset(),
        },
        .type = .render_widget,
        .state = if (config.disabled) .disabled else null,
        .event_flags = .{
            .click = true,
            .focus = true,
            .text = true,
            .drag = true,
        },
    });

    try ui.applyTheme(&Theme.BINDING_SET, .widget, &self.theme);

    try ui.text(self.text.items, .{});

    if (ui.disabled()) return false;

    try Ui.widgets.menuNavigable(ui);

    var changed = false;

    if (ui.getEvent(self.id, .mouse_down)) |event| {
        const mouse_down_data = event.data.mouse_down;
        const location = vec2{
            mouse_down_data.mouse_position[0] - event.info.bounding_box.x,
            mouse_down_data.mouse_position[1] - event.info.bounding_box.y,
        };
        try self.spatial_actions.append(ui.gpa, .{ .mouse_down = .{ .location = location, .modifiers = mouse_down_data.modifiers } });
    }

    if (ui.getEvent(self.id, .mouse_up)) |_| {
        try self.spatial_actions.append(ui.gpa, .mouse_up);
    }

    if (ui.getEvent(self.id, .drag)) |event| {
        const drag_data = event.data.drag;
        const location = vec2{
            drag_data.mouse_position[0] - event.info.bounding_box.x,
            drag_data.mouse_position[1] - event.info.bounding_box.y,
        };
        try self.spatial_actions.append(ui.gpa, .{ .drag = .{ .location = location, .modifiers = drag_data.modifiers } });
    }

    // Double clicking operates on pure string content mapping (not spatial layouts), so we handle it immediately.
    if (ui.getEvent(self.id, .double_clicked)) |_| {
        if (self.carets.items.len > 0) {
            const caret = &self.carets.items[self.carets.items.len - 1];
            caret.start = findPrevWordBreak(self.text.items, caret.end);
            caret.end = findNextWordBreak(self.text.items, caret.end);
            try sortAndMergeCarets(&self.carets);
        }
    }

    if (ui.getEvent(self.id, .text)) |event| {
        const text_data = event.data.text;

        switch (text_data) {
            .command => |cmd| switch (cmd.action) {
                // Layout-dependent actions are queued up to process inside render()
                .move_up => try self.spatial_actions.append(ui.gpa, .{ .move_up = cmd.modifiers }),
                .move_down => try self.spatial_actions.append(ui.gpa, .{ .move_down = cmd.modifiers }),
                .home => try self.spatial_actions.append(ui.gpa, .{ .home = cmd.modifiers }),
                .end => try self.spatial_actions.append(ui.gpa, .{ .end = cmd.modifiers }),

                // Text/string manipulation actions are evaluated immediately
                .copy => copy_action: {
                    try sortAndMergeCarets(&self.carets);
                    var has_selection = false;
                    for (self.carets.items) |c| {
                        if (c.hasSelection()) {
                            has_selection = true;
                        }
                    }
                    if (!has_selection) break :copy_action;

                    var clipboard_list = std.ArrayList(u8).empty;
                    var first = true;
                    for (self.carets.items) |caret| {
                        if (!caret.hasSelection()) continue;
                        if (!first) try clipboard_list.append(ui.frame_arena, '\n');
                        first = false;

                        const selection = self.text.items[caret.min()..caret.max()];
                        try clipboard_list.appendSlice(ui.frame_arena, selection);
                    }

                    try ui.bindings.setClipboard(clipboard_list.items);
                },
                .paste => {
                    const clipboard = ui.bindings.getClipboard();
                    try self.applyTextModification(ui, .insert, clipboard);
                    changed = true;
                },
                .delete => {
                    if (cmd.modifiers.ctrl) {
                        for (self.carets.items) |*caret| {
                            if (!caret.hasSelection()) {
                                caret.end = findNextWordBreak(self.text.items, caret.end);
                            }
                        }
                    }
                    try self.applyTextModification(ui, .delete, "");
                    changed = true;
                },
                .backspace => {
                    if (cmd.modifiers.ctrl) {
                        for (self.carets.items) |*caret| {
                            if (!caret.hasSelection()) {
                                caret.end = findPrevWordBreak(self.text.items, caret.end);
                            }
                        }
                    }
                    try self.applyTextModification(ui, .backspace, "");
                    changed = true;
                },
                .newline => {
                    try self.applyTextModification(ui, .insert, "\n");
                    changed = true;
                },
                .select_all => {
                    self.carets.clearRetainingCapacity();
                    try self.carets.append(ui.gpa, .{ .start = 0, .end = @intCast(self.text.items.len) });
                },
                .move_left => {
                    for (self.carets.items) |*caret| {
                        if (caret.hasSelection() and !cmd.modifiers.shift) {
                            caret.end = caret.min();
                        } else if (cmd.modifiers.ctrl) {
                            caret.end = findPrevWordBreak(self.text.items, caret.end);
                        } else if (caret.end > 0) {
                            var new_pos = caret.end - 1;
                            while (new_pos > 0 and (self.text.items[new_pos] & 0b1100_0000) == 0b1000_0000) : (new_pos -= 1) {}
                            caret.end = new_pos;
                        }
                        if (!cmd.modifiers.shift) caret.start = caret.end;
                    }
                    try sortAndMergeCarets(&self.carets);
                },
                .move_right => {
                    for (self.carets.items) |*caret| {
                        if (caret.hasSelection() and !cmd.modifiers.shift) {
                            caret.end = caret.max();
                        } else if (cmd.modifiers.ctrl) {
                            caret.end = findNextWordBreak(self.text.items, caret.end);
                        } else if (caret.end < self.text.items.len) {
                            var new_pos = caret.end + 1;
                            while (new_pos < self.text.items.len and (self.text.items[new_pos] & 0b1100_0000) == 0b1000_0000) : (new_pos += 1) {}
                            caret.end = new_pos;
                        }
                        if (!cmd.modifiers.shift) caret.start = caret.end;
                    }
                    try sortAndMergeCarets(&self.carets);
                },
            },
            .chars => |chars| {
                var buffer = std.ArrayList(u8).empty;
                for (chars) |char_cmd| {
                    const width = try std.unicode.utf8CodepointSequenceLength(char_cmd.codepoint);
                    const buf = try ui.frame_arena.alloc(u8, width);
                    _ = try std.unicode.utf8Encode(char_cmd.codepoint, buf);
                    try buffer.appendSlice(ui.frame_arena, buf);
                }
                try self.applyTextModification(ui, .insert, buffer.items);
                changed = true;
            },
        }
    }

    return changed;
}
