//! Text input widget state management and event handling.

const TextInputWidget = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");

const log = std.log.scoped(.text_input_widget);

test {
    log.debug("semantic analysis for widgets/TextInput.zig", .{});
    std.testing.refAllDecls(@This());
}

id: Ui.ElementId,
selection_render_data: SelectionRenderData,

text_buffers: [2]std.ArrayList(u8) = .{ .empty, .empty },
carets: std.ArrayList(Caret) = .empty,

current_buffer_idx: u2 = 0,
text_was_modified_last_frame: bool = false,

pub const Config = struct {
    /// The RGBA color of the font to render, conventionally specified as 0-255.
    color: Ui.Color = .{ .r = 0, .g = 0, .b = 0, .a = 255 },
    /// Identifies the font to use.
    font_id: Ui.FontId = 0, // The debug view will pass fontId = 0 for its internal text.
    /// Controls the size of the font.
    font_size: u16 = 20, // Handled by the function provided to Clay_MeasureText.
    /// Controls extra horizontal spacing between characters.
    letter_spacing: u16 = 0, // TODO: Not yet handled by the function provided to Clay_MeasureText.
    /// Additional vertical space between wrapped lines of text
    line_height: u16 = 0,
    /// Controls how text "wraps", that is how it is broken into multiple lines when there is insufficient horizontal space.
    wrap_mode: Ui.TextElementConfigWrapMode = .words,
    /// Controls how wrapped lines of text are horizontally aligned within the outer text bounding box.
    alignment: Ui.TextAlignment = .left,

    pub fn toFull(self: *const Config) Ui.TextElementConfig {
        return .{
            .color = self.color,
            .font_id = self.font_id,
            .font_size = self.font_size,
            .letter_spacing = self.letter_spacing,
            .line_height = self.line_height,
            .wrap_mode = self.wrap_mode,
            .alignment = self.alignment,
            .user_data = null,
        };
    }
};

pub fn init(ui: *Ui, id: Ui.ElementId, initial_text: []const u8) !*TextInputWidget {
    const self = try ui.gpa.create(TextInputWidget);
    errdefer ui.gpa.destroy(self);

    self.* = TextInputWidget{
        .id = id,
        .selection_render_data = .{ .carets = &self.carets },
    };

    self.text_buffers[0] = try std.ArrayList(u8).initCapacity(ui.gpa, initial_text.len);
    self.text_buffers[0].appendSliceAssumeCapacity(initial_text);
    errdefer self.text_buffers[0].deinit(ui.gpa);

    self.text_buffers[1] = try self.text_buffers[0].clone(ui.gpa);
    errdefer self.text_buffers[1].deinit(ui.gpa);

    try self.carets.append(ui.gpa, .{ .start = 0, .end = 0 });

    return self;
}

pub fn deinit(self: *TextInputWidget, ui: *Ui) void {
    self.text_buffers[0].deinit(ui.gpa);
    self.text_buffers[1].deinit(ui.gpa);
    self.carets.deinit(ui.gpa);
    ui.gpa.destroy(self);
}

pub fn bindEvents(self: *TextInputWidget, ui: *Ui) !void {
    try ui.addListener(self.id, .mouse_down, TextInputWidget, TextInputWidget.onMouseDown, self);
    try ui.addListener(self.id, .mouse_up, TextInputWidget, TextInputWidget.onMouseUp, self);
    try ui.addListener(self.id, .drag, TextInputWidget, TextInputWidget.onDrag, self);
    try ui.addListener(self.id, .text, TextInputWidget, TextInputWidget.onText, self);
}

pub fn unbindEvents(self: *TextInputWidget, ui: *Ui) void {
    ui.removeListener(self.id, .mouse_down, TextInputWidget, TextInputWidget.onMouseDown);
    ui.removeListener(self.id, .mouse_up, TextInputWidget, TextInputWidget.onMouseUp);
    ui.removeListener(self.id, .drag, TextInputWidget, TextInputWidget.onDrag);
    ui.removeListener(self.id, .text, TextInputWidget, TextInputWidget.onText);
}

pub fn swapBuffers(self: *TextInputWidget, ui: *Ui) !void {
    if (self.text_was_modified_last_frame) {
        // Swap to the buffer that was modified last frame, making it current.
        self.current_buffer_idx = 1 - self.current_buffer_idx;
        self.text_was_modified_last_frame = false;

        // Sync the 'next' buffer to be a fresh copy of the 'current' one,
        // so subsequent edits in this frame start from the correct state.
        var target_buffer = self.nextTextArray();
        target_buffer.deinit(ui.gpa);
        target_buffer.* = try self.currentTextArray().clone(ui.gpa);
    }
}

pub fn currentText(self: *TextInputWidget) []const u8 {
    return self.currentTextArray().items;
}

fn currentTextArray(self: *TextInputWidget) *std.ArrayList(u8) {
    return &self.text_buffers[self.current_buffer_idx];
}
fn nextTextArray(self: *TextInputWidget) *std.ArrayList(u8) {
    return &self.text_buffers[1 - self.current_buffer_idx];
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
/// This reads from the 'current' buffer and writes the result to the 'next' buffer.
fn applyTextModification(
    self: *TextInputWidget,
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

    const source_text = self.currentText();

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

    // Apply changes to the 'next' buffer and set the modified flag
    var target_buffer = self.nextTextArray();
    target_buffer.deinit(ui.gpa);
    target_buffer.* = new_text;

    self.carets.deinit(ui.gpa);
    self.carets = new_carets;

    self.text_was_modified_last_frame = true;

    try ui.pushEvent(self.id, .{ .text_change = new_text.items }, self);
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

pub fn renderTextInput(self: *TextInputWidget, ui: *Ui, command: Ui.RenderCommand) !void {
    const caret_list = self.selection_render_data.carets;

    const selection_color = Ui.Color.init(0.75, 0, 0.75, 0.5);
    const caret_width: f32 = 1.5;

    if (ui.state.focusedIdValue() != command.id) {
        log.debug("skipping rendering carets for unfocused TextInput", .{});
        return;
    } else {
        log.debug("rendering carets for focused TextInput", .{});
    }

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

                try ui.renderer.drawRect(.{ .x = rect_x, .y = rect_y }, .{ .x = rect_w, .y = rect_h }, selection_color);

                i = end_of_line + 1;
            }
        }

        // Draw caret at the end position
        if (ui.getCharacterOffset(.fromRawId(command.id), caret.end)) |caret_offset| {
            const caret_x = command.bounding_box.x + caret_offset.offset.x;
            const caret_y = command.bounding_box.y + caret_offset.offset.y;
            try ui.renderer.drawRect(.{ .x = caret_x, .y = caret_y }, .{ .x = caret_width, .y = caret_offset.line_height }, .{ .r = 1, .a = 1 });
        }
    }
}

var is_dragging_text = false;

pub fn onMouseDown(self: *TextInputWidget, ui: *Ui, info: Ui.Event.Info, mouse_down_data: Ui.Event.Payload(.mouse_down)) !void {
    log.debug("TextInput received mouse down event: {any}", .{mouse_down_data});

    is_dragging_text = true;

    const location = Ui.Vec2{
        .x = mouse_down_data.mouse_position.x - info.bounding_box.x,
        .y = mouse_down_data.mouse_position.y - info.bounding_box.y,
    };

    if (ui.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), location)) |index| {
        if (mouse_down_data.modifiers.alt) {
            // Alt-click: Add a new caret
            try self.carets.append(ui.gpa, .{ .start = index, .end = index });
        } else if (mouse_down_data.modifiers.shift and self.carets.items.len > 0) {
            // Shift-click: Extend selection of the last caret
            self.carets.items[self.carets.items.len - 1].end = index;
        } else {
            // Normal click: Clear all and create one new caret
            self.carets.clearRetainingCapacity();
            try self.carets.append(ui.gpa, .{ .start = index, .end = index });
        }
    }
}

pub fn onMouseUp(self: *TextInputWidget, _: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.mouse_up)) !void {
    log.debug("TextInput received mouse up event", .{});

    if (is_dragging_text) {
        is_dragging_text = false;
        try sortAndMergeCarets(&self.carets);
    }
}

pub fn onDrag(self: *TextInputWidget, ui: *Ui, info: Ui.Event.Info, drag_data: Ui.Event.Payload(.drag)) !void {
    log.debug("TextInput received drag event: {any}", .{drag_data});

    const location = Ui.Vec2{
        .x = drag_data.mouse_position.x - info.bounding_box.x,
        .y = drag_data.mouse_position.y - info.bounding_box.y,
    };

    const offset = ui.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), location);
    if (offset != null and self.carets.items.len > 0) {
        self.carets.items[self.carets.items.len - 1].end = offset.?;
    }
}

pub fn onText(self: *TextInputWidget, ui: *Ui, _: Ui.Event.Info, text_data: Ui.Event.Payload(.text)) !void {
    log.debug("TextInput received text event: {any}", .{text_data});

    switch (text_data) {
        .command => |cmd| switch (cmd.action) {
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

                    const selection = self.currentText()[caret.min()..caret.max()];
                    try clipboard_list.appendSlice(ui.frame_arena, selection);
                }

                try ui.bindings.setClipboard(clipboard_list.items);
                log.debug(" -> copied '{s}' to clipboard", .{clipboard_list.items});
            },
            .paste => {
                const clipboard = ui.bindings.getClipboard();
                log.debug("  -> paste: '{s}'", .{clipboard});
                try self.applyTextModification(ui, .insert, clipboard);
            },
            .delete => {
                if (cmd.modifiers.ctrl) {
                    for (self.carets.items) |*caret| {
                        if (!caret.hasSelection()) {
                            caret.end = findNextWordBreak(self.currentText(), caret.end);
                        }
                    }
                }
                try self.applyTextModification(ui, .delete, "");
            },
            .backspace => {
                if (cmd.modifiers.ctrl) {
                    for (self.carets.items) |*caret| {
                        if (!caret.hasSelection()) {
                            caret.end = findPrevWordBreak(self.currentText(), caret.end);
                        }
                    }
                }
                try self.applyTextModification(ui, .backspace, "");
            },
            .newline => {
                try self.applyTextModification(ui, .insert, "\n");
            },
            .select_all => {
                self.carets.clearRetainingCapacity();
                try self.carets.append(ui.gpa, .{ .start = 0, .end = @intCast(self.currentText().len) });
            },
            .move_left, .move_right, .move_up, .move_down, .home, .end => {
                var new_carets_to_add = std.ArrayList(Caret).empty;

                for (self.carets.items) |*caret| {
                    switch (cmd.action) {
                        .move_left => {
                            if (caret.hasSelection() and !cmd.modifiers.shift) {
                                caret.end = caret.min();
                            } else if (cmd.modifiers.ctrl) {
                                caret.end = findPrevWordBreak(self.currentText(), caret.end);
                            } else if (caret.end > 0) {
                                var new_pos = caret.end - 1;
                                while (new_pos > 0 and (self.currentText()[new_pos] & 0b1100_0000) == 0b1000_0000) : (new_pos -= 1) {}
                                caret.end = new_pos;
                            }
                            if (!cmd.modifiers.shift) caret.start = caret.end;
                        },
                        .move_right => {
                            if (caret.hasSelection() and !cmd.modifiers.shift) {
                                caret.end = caret.max();
                            } else if (cmd.modifiers.ctrl) {
                                caret.end = findNextWordBreak(self.currentText(), caret.end);
                            } else if (caret.end < self.currentText().len) {
                                var new_pos = caret.end + 1;
                                while (new_pos < self.currentText().len and (self.currentText()[new_pos] & 0b1100_0000) == 0b1000_0000) : (new_pos += 1) {}
                                caret.end = new_pos;
                            }
                            if (!cmd.modifiers.shift) caret.start = caret.end;
                        },
                        .move_up, .move_down => {
                            const start_offset_res = ui.getCharacterOffset(Ui.ElementId.fromSlice("TextInputTest"), caret.start) orelse continue;
                            const end_offset_res = ui.getCharacterOffset(Ui.ElementId.fromSlice("TextInputTest"), caret.end) orelse continue;

                            const target_y_offset = if (cmd.action == .move_up)
                                -start_offset_res.line_height
                            else
                                start_offset_res.line_height;

                            if (cmd.modifiers.alt) {
                                // Alt: Duplicate selection to the next line
                                const target_start_loc = Ui.Vec2{ .x = start_offset_res.offset.x, .y = start_offset_res.offset.y + target_y_offset };
                                const new_start_res = ui.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), target_start_loc) orelse continue;

                                const target_end_loc = Ui.Vec2{ .x = end_offset_res.offset.x, .y = end_offset_res.offset.y + target_y_offset };
                                const new_end_res = ui.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), target_end_loc) orelse continue;

                                try new_carets_to_add.append(ui.frame_arena, .{ .start = new_start_res, .end = new_end_res });
                            } else if (cmd.modifiers.shift) {
                                // Shift: Extend selection to the next line
                                const target_end_loc = Ui.Vec2{ .x = end_offset_res.offset.x, .y = end_offset_res.offset.y + target_y_offset };
                                const new_end_res = ui.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), target_end_loc) orelse continue;

                                caret.end = new_end_res;
                            } else {
                                // No modifiers: Move caret, collapsing selection
                                const target_end_loc = Ui.Vec2{ .x = end_offset_res.offset.x, .y = end_offset_res.offset.y + target_y_offset };
                                const new_end_res = ui.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), target_end_loc) orelse continue;
                                caret.start = new_end_res;
                                caret.end = new_end_res;
                            }
                        },
                        .home => {
                            if (cmd.modifiers.ctrl) {
                                caret.end = 0;
                            } else if (ui.getCharacterOffset(Ui.ElementId.fromSlice("TextInputTest"), caret.end)) |offset| {
                                const target_loc = Ui.Vec2{ .x = 0, .y = offset.offset.y };
                                if (ui.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), target_loc)) |target_index| caret.end = target_index;
                            }
                            if (!cmd.modifiers.shift) caret.start = caret.end;
                        },
                        .end => {
                            if (cmd.modifiers.ctrl) {
                                caret.end = @intCast(self.currentText().len);
                            } else {
                                if (ui.getCharacterOffset(Ui.ElementId.fromSlice("TextInputTest"), caret.end)) |current_offset_res| {
                                    const end_of_line_target_loc = Ui.Vec2{ .x = 999999.0, .y = current_offset_res.offset.y };
                                    if (ui.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), end_of_line_target_loc)) |end_of_line_res| {
                                        var last_char_idx = end_of_line_res;
                                        const check_offset_res = ui.getCharacterOffset(Ui.ElementId.fromSlice("TextInputTest"), last_char_idx);
                                        if (check_offset_res != null and check_offset_res.?.offset.y != current_offset_res.offset.y and last_char_idx > 0) {
                                            var prev_char_start = last_char_idx - 1;
                                            while (prev_char_start > 0 and (self.currentText()[prev_char_start] & 0b1100_0000) == 0b1000_0000) {
                                                prev_char_start -= 1;
                                            }
                                            last_char_idx = prev_char_start;
                                        }
                                        caret.end = last_char_idx;
                                    }
                                }
                            }
                            if (!cmd.modifiers.shift) caret.start = caret.end;
                        },
                        else => {},
                    }
                }

                try self.carets.appendSlice(ui.gpa, new_carets_to_add.items);
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
        },
    }
}
