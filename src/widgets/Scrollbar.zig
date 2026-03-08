//! Scrollbar widget for interacting with scroll containers.

const Scrollbar = @This();

const std = @import("std");
const Ui = @import("../Ui.zig");
const Batch2D = @import("../Batch2D.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

const log = std.log.scoped(.scrollbar_widget);

test {
    log.debug("semantic analysis for widgets/Scrollbar.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const Axis = enum { vertical, horizontal };

pub const Config = struct {
    target_id: Ui.ElementId,
    axis: Axis = .vertical,
    length: ?Ui.SizingAxis = null,
    offset: vec2 = .{ 0, 0 },
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

    pub const BINDING_SET = Ui.Theme.Binding.Set.create(Theme);
};

id: Ui.ElementId,
target_id: Ui.ElementId,
axis: Axis,
theme: Theme,

drag_start_mouse_pos: vec2,
drag_start_scroll_offset: f32,

// Calculated during render, cached for hit-testing next frame
thumb_rect: Ui.BoundingBox,

pub fn render(self: *@This(), ui: *Ui, command: Ui.RenderCommand) !void {
    const bb = command.bounding_box;

    // Fetch fresh scroll data to draw thumb correctly
    const scroll_data = ui.getElementScrollData(self.target_id) orelse return;

    const content_size = if (self.axis == .vertical)
        scroll_data.content_dimensions[1]
    else
        scroll_data.content_dimensions[0];

    const viewport_size = if (self.axis == .vertical)
        scroll_data.scroll_container_dimensions[1]
    else
        scroll_data.scroll_container_dimensions[0];

    if (content_size <= viewport_size) return; // Nothing to scroll

    const max_scroll = content_size - viewport_size;
    const track_size = if (self.axis == .vertical) bb.height else bb.width;
    const thumb_size = @max(self.theme.scrollbar_min_thumb_size, (viewport_size / content_size) * track_size);
    const track_scrollable = track_size - thumb_size;

    const current_scroll = if (self.axis == .vertical) scroll_data.scroll_position.y else scroll_data.scroll_position.x;
    // Clay scroll is negative, so we use -current_scroll to get a positive ratio
    const scroll_ratio = std.math.clamp(-current_scroll / max_scroll, 0.0, 1.0);

    const thumb_offset = scroll_ratio * track_scrollable;

    if (ui.getEvent(self.id, .wheel)) |event| {
        log.info("Scrollbar wheel: {any}, position: {any}", .{ event.data.wheel, scroll_data.scroll_position });
        // Note: Clay applies an internal multiplier to wheel deltas based on time/config. This works well enough for now.
        const scroll_speed = 10.0;

        switch (self.axis) {
            .horizontal => {
                const delta = if (event.data.wheel.delta[1] != 0.0) event.data.wheel.delta[1] else event.data.wheel.delta[0];
                log.info("delta: {d:0.2}", .{delta});
                var new_scroll = scroll_data.scroll_position.x + (delta * scroll_speed);
                new_scroll = std.math.clamp(new_scroll, -max_scroll, 0);
                scroll_data.scroll_position.x = new_scroll;
                log.info("Scrollbar position: {any}", .{scroll_data.scroll_position});
            },

            .vertical => {
                const delta = if (event.data.wheel.delta[0] != 0.0) event.data.wheel.delta[0] else event.data.wheel.delta[1];
                log.info("delta: {d:0.2}", .{delta});
                var new_scroll = scroll_data.scroll_position.y + (delta * scroll_speed);
                new_scroll = std.math.clamp(new_scroll, -max_scroll, 0);
                scroll_data.scroll_position.y = new_scroll;
                log.info("Scrollbar position: {any}", .{scroll_data.scroll_position});
            },
        }
    }

    // Cache the thumb rect for hit-testing in the next frame
    if (self.axis == .vertical) {
        self.thumb_rect = .{
            .x = bb.x,
            .y = bb.y + thumb_offset,
            .width = bb.width,
            .height = thumb_size,
        };
    } else {
        self.thumb_rect = .{
            .x = bb.x + thumb_offset,
            .y = bb.y,
            .width = thumb_size,
            .height = bb.height,
        };
    }

    const border_f: f32 = @floatFromInt(self.theme.scrollbar_border_size);

    // Draw track
    try ui.renderer.drawRoundedRect(
        .{ bb.x, bb.y },
        .{ bb.width, bb.height },
        .all(self.theme.scrollbar_radius),
        self.theme.scrollbar_track_color,
    );

    // Draw thumb
    try ui.renderer.drawRoundedRect(
        .{ self.thumb_rect.x + border_f, self.thumb_rect.y + border_f },
        .{ self.thumb_rect.width - border_f * 2, self.thumb_rect.height - border_f * 2 },
        .all(self.theme.scrollbar_radius - border_f),
        self.theme.scrollbar_thumb_color,
    );
}

pub fn scrollbar(ui: *Ui, id: Ui.ElementId, config: Config) !void {
    const scroll_data = ui.getElementScrollData(config.target_id) orelse return;

    const content_size = if (config.axis == .vertical)
        scroll_data.content_dimensions[1]
    else
        scroll_data.content_dimensions[0];

    const viewport_size = if (config.axis == .vertical)
        scroll_data.scroll_container_dimensions[1]
    else
        scroll_data.scroll_container_dimensions[0];

    // Do not draw or process logic if the content fits entirely in the viewport
    if (content_size <= viewport_size) {
        return;
    }

    const self, const new = try ui.getOrCreateWidget(Scrollbar, id);
    self.id = id;
    self.target_id = config.target_id;
    self.axis = config.axis;
    self.theme = .{};

    if (new) {
        self.drag_start_mouse_pos = .{ 0, 0 };
        self.drag_start_scroll_offset = 0;
        self.thumb_rect = .{ .x = 0, .y = 0, .width = 0, .height = 0 };
    }

    try ui.openElement(id);
    defer ui.endElement();

    // Theme resolution based on interactive state
    const action_state = if (config.disabled) .disabled else ui.getActionState();
    try ui.applyThemeState(&Theme.BINDING_SET, .widget, action_state, &self.theme);

    const sizing: Ui.Sizing = if (config.axis == .vertical)
        .{ .w = .fixed(self.theme.scrollbar_size), .h = config.length orelse .fixed(viewport_size) }
    else
        .{ .w = config.length orelse .fixed(viewport_size), .h = .fixed(self.theme.scrollbar_size) };

    const attach_points: Ui.FloatingAttachPoints = if (config.axis == .vertical)
        .{ .parent = .right_top, .element = .right_top }
    else
        .{ .parent = .left_bottom, .element = .left_bottom };

    try ui.configureElement(.{
        .sizing = sizing,
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
        .floating = .{
            .offset = config.offset,
            .attach_to = .element_with_id,
            .parentId = config.target_id.id,
            .attach_points = attach_points,
            .z_index = 50, // Float above container content
        },
        .event_flags = .{ .drag = true, .click = true, .hover = true, .wheel = true },
    });

    if (config.disabled) return;

    if (ui.getEvent(id, .mouse_down)) |event| {
        const mouse_pos = event.data.mouse_down.mouse_position;

        // If the click is outside the thumb (on the track), jump the thumb center to the mouse
        if (!Ui.boxContains(self.thumb_rect, mouse_pos) and self.thumb_rect.width > 0) {
            const track_start = if (config.axis == .vertical) event.info.bounding_box.y else event.info.bounding_box.x;
            const track_size = if (config.axis == .vertical) event.info.bounding_box.height else event.info.bounding_box.width;

            const max_scroll = content_size - viewport_size;
            const thumb_size = @max(self.theme.scrollbar_min_thumb_size, (viewport_size / content_size) * track_size);
            const track_scrollable = track_size - thumb_size;

            const click_pos = if (config.axis == .vertical) mouse_pos[1] else mouse_pos[0];
            var new_thumb_pos = click_pos - (thumb_size / 2.0) - track_start;
            new_thumb_pos = std.math.clamp(new_thumb_pos, 0, track_scrollable);

            // Clay scroll positions are negative
            const new_scroll = -(new_thumb_pos / track_scrollable) * max_scroll;

            if (config.axis == .vertical) {
                scroll_data.scroll_position.y = new_scroll;
            } else {
                scroll_data.scroll_position.x = new_scroll;
            }
        }

        // Lock in the start position for future drag events
        self.drag_start_mouse_pos = mouse_pos;
        self.drag_start_scroll_offset = if (config.axis == .vertical) scroll_data.scroll_position.y else scroll_data.scroll_position.x;
    }

    if (ui.getEvent(id, .drag)) |event| {
        const max_scroll = content_size - viewport_size;
        const track_size = if (config.axis == .vertical) event.info.bounding_box.height else event.info.bounding_box.width;
        const thumb_size = @max(self.theme.scrollbar_min_thumb_size, (viewport_size / content_size) * track_size);
        const track_scrollable = track_size - thumb_size;

        if (track_scrollable > 0) {
            const mouse_delta = if (config.axis == .vertical)
                event.data.drag.mouse_position[1] - self.drag_start_mouse_pos[1]
            else
                event.data.drag.mouse_position[0] - self.drag_start_mouse_pos[0];

            // Map the mouse delta back to a scroll delta
            const scroll_delta = (mouse_delta / track_scrollable) * max_scroll;

            // Apply delta (subtract because moving thumb right/down makes content move left/up which is negative)
            var new_scroll = self.drag_start_scroll_offset - scroll_delta;
            new_scroll = std.math.clamp(new_scroll, -max_scroll, 0);

            if (config.axis == .vertical) {
                scroll_data.scroll_position.y = new_scroll;
            } else {
                scroll_data.scroll_position.x = new_scroll;
            }
        }
    }
}
