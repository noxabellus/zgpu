//! ScrollArea widget that provides a structured scrolling region with non-overlapping scrollbars.

const ScrollArea = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");
const Batch2D = @import("../Batch2D.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

const log = std.log.scoped(.scrollarea_widget);

test {
    log.debug("semantic analysis for widgets/ScrollArea.zig", .{});
    std.testing.refAllDecls(@This());
}

const CLIP_MASK: u32 = 0x11111111;
const VBAR_MASK: u32 = 0x22222222;
const HBAR_MASK: u32 = 0x33333333;

const SCROLL_TOLERANCE: f32 = 1.0;

pub const Config = struct {
    sizing: Ui.Sizing = .{ .w = .grow, .h = .grow },
    direction: Ui.LayoutDirection = .left_to_right,
    child_alignment: Ui.ChildAlignment = .{},
    horizontal: bool = false,
    vertical: bool = true,
    disabled: bool = false,
};

pub const Theme = struct {
    scrollbar_track_color: Ui.Color = Ui.Color.fromLinearU8(240, 240, 240, 255),
    scrollbar_thumb_color: Ui.Color = Ui.Color.fromLinearU8(180, 180, 180, 255),

    scrollbar_size: f32 = 8.0,
    scrollbar_radius: f32 = 4.0,
    scrollbar_min_thumb_size: f32 = 8.0,

    scrollbar_border_color: Ui.Color = .black,
    scrollbar_border_size: u16 = 0,

    padding_right: u16 = 0,
    padding_bottom: u16 = 0,

    pub const BINDING_SET = Ui.Theme.Binding.Set.create(Theme);
};

id: Ui.ElementId,
config: Config,
theme: Theme,

v_drag_start_mouse_pos: vec2,
v_drag_start_scroll_offset: f32,
v_thumb_rect: Ui.BoundingBox,

h_drag_start_mouse_pos: vec2,
h_drag_start_scroll_offset: f32,
h_thumb_rect: Ui.BoundingBox,

pub fn getClipId(id: Ui.ElementId) Ui.ElementId {
    var out = id;
    out.id ^= CLIP_MASK;
    return out;
}

pub fn begin(ui: *Ui, id: Ui.ElementId, config: Config) !void {
    const self, const new = try ui.getOrCreateWidget(ScrollArea, id);
    self.id = id;
    self.config = config;
    self.theme = .{};
    if (new) {
        self.v_drag_start_mouse_pos = .{ 0, 0 };
        self.v_drag_start_scroll_offset = 0;
        self.v_thumb_rect = .{ .x = 0, .y = 0, .width = 0, .height = 0 };

        self.h_drag_start_mouse_pos = .{ 0, 0 };
        self.h_drag_start_scroll_offset = 0;
        self.h_thumb_rect = .{ .x = 0, .y = 0, .width = 0, .height = 0 };
    }

    const action_state = if (config.disabled) .disabled else ui.getActionState();
    try ui.applyThemeState(&Theme.BINDING_SET, .widget, action_state, &self.theme);

    // 1. Outer Container: Left-to-Right
    try ui.beginElement(id, .{
        .sizing = self.config.sizing,
        .direction = .left_to_right,
        .child_gap = 0,
        .event_flags = .{ .wheel = true },
    });

    // 2. Inner Left Column: Top-to-Bottom
    var left_col_id = id;
    left_col_id.id ^= 0x44444444; // Generate unique inner ID

    try ui.beginSection(left_col_id, .{
        .sizing = .{ .w = .grow, .h = .grow },
        .direction = .top_to_bottom,
        .child_gap = 0,
    });

    // 3. The true Clip Container
    var clip_id = id;
    clip_id.id ^= CLIP_MASK;

    try ui.openSection(clip_id);
    try ui.configureSection(.{
        .sizing = .{ .w = .grow, .h = .grow },
        .direction = self.config.direction,
        .child_alignment = self.config.child_alignment,
        .clip = .{ .horizontal = self.config.horizontal, .vertical = self.config.vertical, .child_offset = ui.scrollOffset() },
    });
}

pub fn end(ui: *Ui, id: Ui.ElementId) void {
    _end(ui, id) catch |err| {
        log.err("Failed to end ScrollArea: {s}", .{@errorName(err)});
    };
}

fn _end(ui: *Ui, id: Ui.ElementId) !void {
    const self, _ = try ui.getOrCreateWidget(ScrollArea, id);

    var clip_id = id;
    clip_id.id ^= CLIP_MASK;

    // End Clip Container
    ui.endSection();

    // Fetch scroll data. In immediate mode, it's common that this is
    // populated from the last frame's layout pass or earlier in the tree.
    const scroll_data_opt = ui.getElementScrollData(clip_id);

    if (scroll_data_opt) |scroll_data| {
        if (ui.getEvent(self.id, .wheel)) |event| {
            // Only manually scroll if the mouse isn't over the actual clip region.
            // If it IS over the clip region, Clay natively handles it.
            if (!ui.isHovered(clip_id)) {
                // Note: Clay applies an internal multiplier to wheel deltas based on time/config. This works well enough for now.
                const scroll_speed = 10.0;

                if (self.config.vertical) {
                    const content_size = scroll_data.content_dimensions[1];
                    const viewport_size = scroll_data.scroll_container_dimensions[1];

                    if (content_size > viewport_size) {
                        const max_scroll = content_size - viewport_size;
                        var new_scroll = scroll_data.scroll_position.y + (event.data.wheel.delta[1] * scroll_speed);
                        new_scroll = std.math.clamp(new_scroll, -max_scroll, 0);
                        scroll_data.scroll_position.y = new_scroll;
                    }
                }

                if (self.config.horizontal) {
                    const content_size = scroll_data.content_dimensions[0];
                    const viewport_size = scroll_data.scroll_container_dimensions[0];

                    if (content_size > viewport_size) {
                        const max_scroll = content_size - viewport_size;
                        var new_scroll = scroll_data.scroll_position.x + (event.data.wheel.delta[0] * scroll_speed);
                        new_scroll = std.math.clamp(new_scroll, -max_scroll, 0);
                        scroll_data.scroll_position.x = new_scroll;
                    }
                }
            }
        }
    }

    var hbar_visible = false;

    // Render Horizontal Scrollbar (if needed) at the bottom of the left column
    if (self.config.horizontal) {
        if (scroll_data_opt) |scroll_data| {
            if (scroll_data.content_dimensions[0] - scroll_data.scroll_container_dimensions[0] > SCROLL_TOLERANCE) {
                hbar_visible = true;
                try self.doHorizontalScrollbar(ui, scroll_data);
            }
        }
    }

    // End Inner Left Column
    ui.endSection();

    // Render Vertical Scrollbar (if needed) on the right side of the outer container
    if (self.config.vertical) {
        if (scroll_data_opt) |scroll_data| {
            if (scroll_data.content_dimensions[1] - scroll_data.scroll_container_dimensions[1] > SCROLL_TOLERANCE) {
                try self.doVerticalScrollbar(ui, scroll_data, hbar_visible);
            }
        }
    }

    // End Outer Container
    ui.endElement();
}

fn doVerticalScrollbar(self: *ScrollArea, ui: *Ui, scroll_data: Ui.ScrollContainerData, hbar_visible: bool) !void {
    var vbar_id = self.id;
    vbar_id.id ^= VBAR_MASK;

    const vbar, _ = try ui.getOrCreateWidget(VBar, vbar_id);
    vbar.area_id = self.id;

    try ui.beginSection(.localID("VBAR"), .{
        .sizing = .grow,
        .child_gap = 0,
        .padding_left = self.theme.padding_right,
        .padding_bottom = if (hbar_visible) @as(u16, @intFromFloat(@round(@as(f32, @floatFromInt(self.theme.padding_bottom)) + self.theme.scrollbar_size))) else null,
    });
    defer ui.endSection();

    try ui.openElement(vbar_id);
    defer ui.endElement();

    try ui.configureElement(.{
        .sizing = .{ .w = .fixed(self.theme.scrollbar_size), .h = .grow },
        .type = .render_widget,
        .border_color = self.theme.scrollbar_border_color,
        .border_left = self.theme.scrollbar_border_size,
        .border_right = self.theme.scrollbar_border_size,
        .border_top = self.theme.scrollbar_border_size,
        .border_bottom = self.theme.scrollbar_border_size,
        .radius_top_left = self.theme.scrollbar_radius,
        .radius_top_right = self.theme.scrollbar_radius,
        .radius_bottom_left = self.theme.scrollbar_radius,
        .radius_bottom_right = self.theme.scrollbar_radius,
        .event_flags = .{ .drag = true, .click = true, .hover = true },
    });

    if (self.config.disabled) return;

    const content_size = scroll_data.content_dimensions[1];
    const viewport_size = scroll_data.scroll_container_dimensions[1];
    if (content_size <= viewport_size) return;

    if (ui.getEvent(vbar_id, .mouse_down)) |event| {
        const mouse_pos = event.data.mouse_down.mouse_position;

        if (!Ui.boxContains(self.v_thumb_rect, mouse_pos) and self.v_thumb_rect.width > 0) {
            const track_start = event.info.bounding_box.y;
            const track_size = event.info.bounding_box.height;
            const max_scroll = content_size - viewport_size;
            const thumb_size = @max(self.theme.scrollbar_min_thumb_size, (viewport_size / content_size) * track_size);
            const track_scrollable = track_size - thumb_size;

            var new_thumb_pos = mouse_pos[1] - (thumb_size / 2.0) - track_start;
            new_thumb_pos = std.math.clamp(new_thumb_pos, 0, track_scrollable);

            // Clay scroll positions are negative
            const new_scroll = -(new_thumb_pos / track_scrollable) * max_scroll;
            scroll_data.scroll_position.y = new_scroll;
        }

        self.v_drag_start_mouse_pos = mouse_pos;
        self.v_drag_start_scroll_offset = scroll_data.scroll_position.y;
    }

    if (ui.getEvent(vbar_id, .drag)) |event| {
        const track_size = event.info.bounding_box.height;
        const max_scroll = content_size - viewport_size;
        const thumb_size = @max(self.theme.scrollbar_min_thumb_size, (viewport_size / content_size) * track_size);
        const track_scrollable = track_size - thumb_size;

        if (track_scrollable > 0) {
            const mouse_delta = event.data.drag.mouse_position[1] - self.v_drag_start_mouse_pos[1];
            const scroll_delta = (mouse_delta / track_scrollable) * max_scroll;

            var new_scroll = self.v_drag_start_scroll_offset - scroll_delta;
            new_scroll = std.math.clamp(new_scroll, -max_scroll, 0);

            scroll_data.scroll_position.y = new_scroll;
        }
    }
}

fn doHorizontalScrollbar(self: *ScrollArea, ui: *Ui, scroll_data: Ui.ScrollContainerData) !void {
    var hbar_id = self.id;
    hbar_id.id ^= HBAR_MASK;

    const hbar, _ = try ui.getOrCreateWidget(HBar, hbar_id);
    hbar.area_id = self.id;

    try ui.beginSection(.localID("HBAR"), .{
        .sizing = .grow,
        .child_gap = 0,
    });
    defer ui.endSection();

    try ui.openElement(hbar_id);
    defer ui.endElement();

    try ui.configureElement(.{
        .sizing = .{ .w = .grow, .h = .fixed(self.theme.scrollbar_size) },
        .type = .render_widget,
        .border_color = self.theme.scrollbar_border_color,
        .border_left = self.theme.scrollbar_border_size,
        .border_right = self.theme.scrollbar_border_size,
        .border_top = self.theme.scrollbar_border_size,
        .border_bottom = self.theme.scrollbar_border_size,
        .radius_top_left = self.theme.scrollbar_radius,
        .radius_top_right = self.theme.scrollbar_radius,
        .radius_bottom_left = self.theme.scrollbar_radius,
        .radius_bottom_right = self.theme.scrollbar_radius,
        .event_flags = .{ .drag = true, .click = true, .hover = true },
    });

    if (self.config.disabled) return;

    const content_size = scroll_data.content_dimensions[0];
    const viewport_size = scroll_data.scroll_container_dimensions[0];
    if (content_size <= viewport_size) return;

    if (ui.getEvent(hbar_id, .mouse_down)) |event| {
        const mouse_pos = event.data.mouse_down.mouse_position;

        if (!Ui.boxContains(self.h_thumb_rect, mouse_pos) and self.h_thumb_rect.height > 0) {
            const track_start = event.info.bounding_box.x;
            const track_size = event.info.bounding_box.width;
            const max_scroll = content_size - viewport_size;
            const thumb_size = @max(self.theme.scrollbar_min_thumb_size, (viewport_size / content_size) * track_size);
            const track_scrollable = track_size - thumb_size;

            var new_thumb_pos = mouse_pos[0] - (thumb_size / 2.0) - track_start;
            new_thumb_pos = std.math.clamp(new_thumb_pos, 0, track_scrollable);

            const new_scroll = -(new_thumb_pos / track_scrollable) * max_scroll;
            scroll_data.scroll_position.x = new_scroll;
        }

        self.h_drag_start_mouse_pos = mouse_pos;
        self.h_drag_start_scroll_offset = scroll_data.scroll_position.x;
    }

    if (ui.getEvent(hbar_id, .drag)) |event| {
        const track_size = event.info.bounding_box.width;
        const max_scroll = content_size - viewport_size;
        const thumb_size = @max(self.theme.scrollbar_min_thumb_size, (viewport_size / content_size) * track_size);
        const track_scrollable = track_size - thumb_size;

        if (track_scrollable > 0) {
            const mouse_delta = event.data.drag.mouse_position[0] - self.h_drag_start_mouse_pos[0];
            const scroll_delta = (mouse_delta / track_scrollable) * max_scroll;

            var new_scroll = self.h_drag_start_scroll_offset - scroll_delta;
            new_scroll = std.math.clamp(new_scroll, -max_scroll, 0);

            scroll_data.scroll_position.x = new_scroll;
        }
    }
}

const VBar = struct {
    area_id: Ui.ElementId = undefined,

    pub fn render(ptr: *anyopaque, ui: *Ui, command: Ui.RenderCommand) !void {
        const self: *VBar = @ptrCast(@alignCast(ptr));
        const area = ui.getWidget(ScrollArea, self.area_id).?;

        var clip_id = self.area_id;
        clip_id.id ^= CLIP_MASK;

        const scroll_data = ui.getElementScrollData(clip_id) orelse return;

        const bb = command.bounding_box;
        const content_size = scroll_data.content_dimensions[1];
        const viewport_size = scroll_data.scroll_container_dimensions[1];

        if (content_size <= viewport_size) return;

        const max_scroll = content_size - viewport_size;
        const track_size = bb.height;
        const thumb_size = @max(area.theme.scrollbar_min_thumb_size, (viewport_size / content_size) * track_size);
        const track_scrollable = track_size - thumb_size;

        const current_scroll = scroll_data.scroll_position.y;
        const scroll_ratio = std.math.clamp(-current_scroll / max_scroll, 0.0, 1.0);
        const thumb_offset = scroll_ratio * track_scrollable;
        const border_f: f32 = @floatFromInt(area.theme.scrollbar_border_size);

        area.v_thumb_rect = .{
            .x = bb.x,
            .y = bb.y + thumb_offset,
            .width = bb.width,
            .height = thumb_size,
        };

        try ui.renderer.drawRoundedRect(
            .{ bb.x, bb.y },
            .{ bb.width, bb.height },
            .all(area.theme.scrollbar_radius),
            area.theme.scrollbar_track_color,
        );

        try ui.renderer.drawRoundedRect(
            .{ area.v_thumb_rect.x + border_f, area.v_thumb_rect.y + border_f },
            .{ area.v_thumb_rect.width - border_f * 2, area.v_thumb_rect.height - border_f * 2 },
            .all(area.theme.scrollbar_radius - border_f),
            area.theme.scrollbar_thumb_color,
        );
    }
};

const HBar = struct {
    area_id: Ui.ElementId = undefined,

    pub fn render(ptr: *anyopaque, ui: *Ui, command: Ui.RenderCommand) !void {
        const self: *HBar = @ptrCast(@alignCast(ptr));
        const area = ui.getWidget(ScrollArea, self.area_id).?;

        var clip_id = self.area_id;
        clip_id.id ^= CLIP_MASK;

        const scroll_data = ui.getElementScrollData(clip_id) orelse return;

        const bb = command.bounding_box;
        const content_size = scroll_data.content_dimensions[0];
        const viewport_size = scroll_data.scroll_container_dimensions[0];

        if (content_size <= viewport_size) return;

        const max_scroll = content_size - viewport_size;
        const track_size = bb.width;
        const thumb_size = @max(area.theme.scrollbar_min_thumb_size, (viewport_size / content_size) * track_size);
        const track_scrollable = track_size - thumb_size;

        const current_scroll = scroll_data.scroll_position.x;
        const scroll_ratio = std.math.clamp(-current_scroll / max_scroll, 0.0, 1.0);
        const thumb_offset = scroll_ratio * track_scrollable;
        const border_f: f32 = @floatFromInt(area.theme.scrollbar_border_size);

        area.h_thumb_rect = .{ .x = bb.x + thumb_offset, .y = bb.y, .width = thumb_size, .height = bb.height };

        try ui.renderer.drawRoundedRect(
            .{ bb.x, bb.y },
            .{ bb.width, bb.height },
            .all(area.theme.scrollbar_radius),
            area.theme.scrollbar_track_color,
        );

        try ui.renderer.drawRoundedRect(
            .{ area.h_thumb_rect.x + border_f, area.h_thumb_rect.y + border_f },
            .{ area.h_thumb_rect.width - border_f * 2, area.h_thumb_rect.height - border_f * 2 },
            .all(area.theme.scrollbar_radius - border_f),
            area.theme.scrollbar_thumb_color,
        );
    }
};
