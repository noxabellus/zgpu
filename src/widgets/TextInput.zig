//! Text input widget state management and event handling.

const TextInputWidget = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");

const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

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
column_select_start_pos: ?u32 = null,
drag_additive: u32 = 0,

pub const Config = struct {
    /// The default value the text input will be initialized to.
    default_text: []const u8 = "[Type here]",
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

pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*TextInputWidget {
    const self = try ui.gpa.create(TextInputWidget);
    errdefer ui.gpa.destroy(self);

    self.* = TextInputWidget{
        .id = id,
        .selection_render_data = .{ .carets = &self.carets },
    };

    self.text_buffers[0] = try std.ArrayList(u8).initCapacity(ui.gpa, config.default_text.len);
    self.text_buffers[0].appendSliceAssumeCapacity(config.default_text);
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
    try ui.addListener(self.id, .double_clicked, TextInputWidget, TextInputWidget.onDoubleClicked, self);
    try ui.addListener(self.id, .drag, TextInputWidget, TextInputWidget.onDrag, self);
    try ui.addListener(self.id, .text, TextInputWidget, TextInputWidget.onText, self);
}

pub fn unbindEvents(self: *TextInputWidget, ui: *Ui) void {
    ui.removeListener(self.id, .mouse_down, TextInputWidget, TextInputWidget.onMouseDown);
    ui.removeListener(self.id, .mouse_up, TextInputWidget, TextInputWidget.onMouseUp);
    ui.removeListener(self.id, .double_clicked, TextInputWidget, TextInputWidget.onDoubleClicked);
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

pub fn render(self: *TextInputWidget, ui: *Ui, command: Ui.RenderCommand) !void {
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

                try ui.renderer.drawRect(.{ rect_x, rect_y }, .{ rect_w, rect_h }, selection_color);

                i = end_of_line + 1;
            }
        }

        // Draw caret at the end position
        if (ui.getCharacterOffset(.fromRawId(command.id), caret.end)) |caret_offset| {
            const caret_x = command.bounding_box.x + caret_offset.offset.x;
            const caret_y = command.bounding_box.y + caret_offset.offset.y;
            try ui.renderer.drawRect(.{ caret_x, caret_y }, .{ caret_width, caret_offset.line_height }, .{ .r = 1, .a = 1 });
        }
    }
}

pub fn onGet(self: *TextInputWidget, _: *Ui, _: Ui.Event.Info) *const []const u8 {
    return &self.currentTextArray().items;
}

pub fn onSet(self: *TextInputWidget, ui: *Ui, new_text: *const []const u8) !void {
    var target_buffer = self.nextTextArray();
    target_buffer.deinit(ui.gpa);
    target_buffer.* = try std.ArrayList(u8).initCapacity(ui.gpa, new_text.len);
    try target_buffer.appendSlice(ui.gpa, new_text.*);

    self.text_was_modified_last_frame = true;

    self.carets.clearRetainingCapacity();
    try self.carets.append(ui.gpa, .{ .start = 0, .end = @intCast(new_text.len) });
}

pub fn onDoubleClicked(self: *TextInputWidget, _: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.double_clicked)) !void {
    log.info("TextInput received double click event", .{});

    if (self.carets.items.len == 0) return;

    const caret = &self.carets.items[self.carets.items.len - 1];
    caret.start = findPrevWordBreak(self.currentText(), caret.end);
    caret.end = findNextWordBreak(self.currentText(), caret.end);

    try sortAndMergeCarets(&self.carets);
}

pub fn onMouseDown(self: *TextInputWidget, ui: *Ui, info: Ui.Event.Info, mouse_down_data: Ui.Event.Payload(.mouse_down)) !void {
    log.info("TextInput received mouse down event: {any}", .{mouse_down_data});

    const location = vec2{
        mouse_down_data.mouse_position[0] - info.bounding_box.x,
        mouse_down_data.mouse_position[1] - info.bounding_box.y,
    };

    if (ui.getCharacterIndexAtOffset(self.id, location)) |index| {
        if (mouse_down_data.modifiers.alt) {
            // START a column select drag.
            self.column_select_start_pos = index;
            if (mouse_down_data.modifiers.shift) {
                log.info("Alt+Shift drag additive", .{});

                self.drag_additive = @intCast(self.carets.items.len + 1);
            } else {
                log.info("Alt drag new", .{});
                self.carets.clearRetainingCapacity();
                self.drag_additive = 1;
            }
            try self.carets.append(ui.gpa, .{ .start = index, .end = index });
        } else if (mouse_down_data.modifiers.shift and self.carets.items.len > 0) {
            log.info("Shift click extend", .{});
            self.carets.items[self.carets.items.len - 1].end = index;
        } else {
            log.info("Normal click", .{});
            self.carets.clearRetainingCapacity();
            try self.carets.append(ui.gpa, .{ .start = index, .end = index });
        }
    }
}

pub fn onMouseUp(self: *TextInputWidget, _: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.mouse_up)) !void {
    log.info("TextInput received mouse up event", .{});

    try sortAndMergeCarets(&self.carets);
    self.column_select_start_pos = null;
    self.drag_additive = 0;
}

pub fn onDrag(self: *TextInputWidget, ui: *Ui, info: Ui.Event.Info, drag_data: Ui.Event.Payload(.drag)) !void {
    log.info("drag event: {any}", .{drag_data});

    const location = vec2{
        drag_data.mouse_position[0] - info.bounding_box.x,
        drag_data.mouse_position[1] - info.bounding_box.y,
    };

    // Branch on the interaction mode
    if (self.column_select_start_pos) |start_index| {
        log.info("Column drag event", .{});
        // --- We are in COLUMN SELECTION mode ---
        const start_offset = ui.getCharacterOffset(self.id, start_index) orelse return;
        const current_offset = ui.getCharacterOffsetAtPoint(self.id, location) orelse return;

        // Define the selection rectangle in text-space coordinates
        const rect_x1 = @min(start_offset.offset.x, current_offset.offset.x);
        const rect_x2 = @max(start_offset.offset.x, current_offset.offset.x);
        const rect_y1 = @min(start_offset.offset.y, current_offset.offset.y);
        const rect_y2 = @max(start_offset.offset.y, current_offset.offset.y) + current_offset.line_height;

        // Clear previous carets, we're recalculating them from scratch each drag frame
        const base = @min(self.drag_additive, self.carets.items.len);
        self.carets.shrinkRetainingCapacity(base);

        // This is tricky: we need to iterate over *visual lines* in the text box.
        // We can approximate this by stepping through the text and checking character offsets.
        var scan_index: u32 = 0;
        while (scan_index < self.currentText().len) {
            const char_offset = ui.getCharacterOffset(self.id, scan_index) orelse {
                scan_index += 1;
                continue;
            };

            const line_y = char_offset.offset.y;
            const line_h = char_offset.line_height;

            // Is this line within our vertical selection rectangle?
            if (line_y + line_h > rect_y1 and line_y < rect_y2) {
                // Yes. Now find the start and end character indices for this line's selection.
                const start_char_opt = ui.getCharacterIndexAtOffset(self.id, .{ rect_x1, line_y });
                const end_char_opt = ui.getCharacterIndexAtOffset(self.id, .{ rect_x2, line_y });

                if (start_char_opt != null and end_char_opt != null) {
                    // We found a valid selection range for this line. Add a new caret.
                    // Ensure start <= end
                    const sel_start = @min(start_char_opt.?, end_char_opt.?);
                    const sel_end = @max(start_char_opt.?, end_char_opt.?);
                    self.carets.append(ui.gpa, .{ .start = sel_start, .end = sel_end }) catch |err| {
                        log.err("Failed to append caret during column select: {any}", .{err});
                        return;
                    };
                }
            }

            // Find the start of the next line to continue scanning
            var next_scan_index = scan_index;
            while (next_scan_index < self.currentText().len) {
                const next_char_offset = ui.getCharacterOffset(self.id, next_scan_index) orelse break;
                if (next_char_offset.offset.y > line_y) {
                    break; // Found start of next line
                }
                next_scan_index += 1; // This is slow, but necessary without a better line API
            }

            if (next_scan_index == scan_index) {
                // We are at the end of the text
                break;
            }
            scan_index = next_scan_index;
        }
    } else {
        // --- We are in NORMAL selection mode ---
        log.info("TextInput received drag event: {any}", .{drag_data});
        const offset = ui.getCharacterIndexAtOffset(self.id, location);
        if (offset != null and self.carets.items.len > 0) {
            self.carets.items[self.carets.items.len - 1].end = offset.?;
        }
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
                            const start_offset_res = ui.getCharacterOffset(self.id, caret.start) orelse continue;
                            const end_offset_res = ui.getCharacterOffset(self.id, caret.end) orelse continue;

                            const target_y_offset = if (cmd.action == .move_up)
                                -start_offset_res.line_height
                            else
                                start_offset_res.line_height;

                            if (cmd.modifiers.alt) {
                                // Alt: Duplicate selection to the next line
                                const target_start_loc = vec2{ start_offset_res.offset.x, start_offset_res.offset.y + target_y_offset };
                                const new_start_res = ui.getCharacterIndexAtOffset(self.id, target_start_loc) orelse continue;

                                const target_end_loc = vec2{ end_offset_res.offset.x, end_offset_res.offset.y + target_y_offset };
                                const new_end_res = ui.getCharacterIndexAtOffset(self.id, target_end_loc) orelse continue;

                                try new_carets_to_add.append(ui.frame_arena, .{ .start = new_start_res, .end = new_end_res });
                            } else if (cmd.modifiers.shift) {
                                // Shift: Extend selection to the next line
                                const target_end_loc = vec2{ end_offset_res.offset.x, end_offset_res.offset.y + target_y_offset };
                                const new_end_res = ui.getCharacterIndexAtOffset(self.id, target_end_loc) orelse continue;

                                caret.end = new_end_res;
                            } else {
                                // No modifiers: Move caret, collapsing selection
                                const target_end_loc = vec2{ end_offset_res.offset.x, end_offset_res.offset.y + target_y_offset };
                                const new_end_res = ui.getCharacterIndexAtOffset(self.id, target_end_loc) orelse continue;
                                caret.start = new_end_res;
                                caret.end = new_end_res;
                            }
                        },
                        .home => {
                            if (cmd.modifiers.ctrl) {
                                caret.end = 0;
                            } else if (ui.getCharacterOffset(self.id, caret.end)) |offset| {
                                const target_loc = vec2{ 0, offset.offset.y };
                                if (ui.getCharacterIndexAtOffset(self.id, target_loc)) |target_index| caret.end = target_index;
                            }
                            if (!cmd.modifiers.shift) caret.start = caret.end;
                        },
                        .end => {
                            if (cmd.modifiers.ctrl) {
                                caret.end = @intCast(self.currentText().len);
                            } else {
                                if (ui.getCharacterOffset(self.id, caret.end)) |current_offset_res| {
                                    const end_of_line_target_loc = vec2{ 999999.0, current_offset_res.offset.y };
                                    if (ui.getCharacterIndexAtOffset(self.id, end_of_line_target_loc)) |end_of_line_res| {
                                        var last_char_idx = end_of_line_res;
                                        const check_offset_res = ui.getCharacterOffset(self.id, last_char_idx);
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
