//! An abstraction layer for rendering *interactive* UI layouts based on the clay layout engine, rendered with our Batch2D library.

const Ui = @This();

const std = @import("std");
const clay = @import("clay");

const linalg = @import("linalg.zig");
const Batch2D = @import("Batch2D.zig");
const AssetCache = @import("AssetCache.zig");
const BindingState = @import("BindingState.zig");

const vec2 = linalg.vec2;

const log = std.log.scoped(.ui);

test {
    log.debug("semantic analysis for Ui.zig", .{});
    std.testing.refAllDecls(@This());
}

const FrameElementInfo = struct {
    widget: bool,
    state: ElementStateFlags,
    parent_id: ?ElementId,
};

pub const widgets = struct {
    pub const button = Widget.Button.button;
    pub const checkbox = Widget.Checkbox.checkbox;
    pub const dropdown = Widget.Dropdown.dropdown;
    pub const enumDropdown = Widget.Dropdown.enumDropdown;
    pub const image = Widget.Image.image;
    pub const beginMenu = Widget.Menu.begin;
    pub const endMenu = Widget.Menu.end;
    pub const subMenu = Widget.Menu.subMenu;
    pub const menuItem = Widget.Menu.item;
    pub const menuSeparator = Widget.Menu.separator;
    pub const menuNavigable = Widget.Menu.navigable;
    pub const beginMenuEmbeddedLayout = Widget.Menu.beginEmbeddedLayout;
    pub const endMenuEmbeddedLayout = Widget.Menu.endEmbeddedLayout;
    pub const radioButton = Widget.RadioButton.radioButton;
    pub const enumRadioButton = Widget.RadioButton.enumRadioButton;
    pub const beginScrollArea = Widget.ScrollArea.begin;
    pub const endScrollArea = Widget.ScrollArea.end;
    pub const getScrollAreaClipId = Widget.ScrollArea.getClipId;
    pub const scrollbar = Widget.Scrollbar.scrollbar;
    pub const shaderRect = Widget.ShaderRect.shaderRect;
    pub const slider = Widget.Slider.slider;
    pub const enumSlider = Widget.Slider.enumSlider;
    pub const textInput = Widget.TextInput.textInput;
};

pub const Widget = struct {
    user_data: *anyopaque,
    render: ?*const fn (*anyopaque, *Ui, RenderCommand) anyerror!void,
    deinit: *const fn (*anyopaque, *Ui) void,
    seen_this_frame: bool,

    pub const Button = @import("widgets/Button.zig");
    pub const Checkbox = @import("widgets/Checkbox.zig");
    pub const Dropdown = @import("widgets/Dropdown.zig");
    pub const Image = @import("widgets/Image.zig");
    pub const Menu = @import("widgets/Menu.zig");
    pub const RadioButton = @import("widgets/RadioButton.zig");
    pub const ScrollArea = @import("widgets/ScrollArea.zig");
    pub const Scrollbar = @import("widgets/Scrollbar.zig");
    pub const ShaderRect = @import("widgets/ShaderRect.zig");
    pub const Slider = @import("widgets/Slider.zig");
    pub const TextInput = @import("widgets/TextInput.zig");

    test {
        log.debug("semantic analysis for Ui.Widgets", .{});
        std.testing.refAllDecls(@This());
    }
};

pub const SharedWidgetState = struct {
    data: *anyopaque,
    deinit: *const fn (*anyopaque, *Ui) void,
    state_type: *const anyopaque,
    seen_this_frame: bool,
};

pub const KeyRepeatState = struct {
    timer: std.time.Timer,
    initial_delay: u64 = 350 * std.time.ns_per_ms,
    nth_delay: u64 = 25 * std.time.ns_per_ms,
    state: enum { none, first, nth } = .none,
    active_key: ?BindingState.Key = null,

    pub fn advance(self: *@This()) void {
        self.state = switch (self.state) {
            .none => .first,
            .first => .nth,
            else => self.state,
        };
    }
};

pub const Color = Batch2D.Color;
pub const FontId = Batch2D.FontId;

// --- Common Colors ---
pub const black = Color.fromLinearU8(0, 0, 0, 255);
pub const white = Color.fromLinearU8(255, 255, 255, 255);
pub const transparent = Color.fromLinearU8(0, 0, 0, 0);

pub const default_bindings = .{
    .primary_mouse = BindingState.InputBinding{ .mouse = .{ .bind_point = .button_1 } },
    .focus_next = BindingState.InputBinding{ .key = .{ .bind_point = .tab } },
    .focus_prev = BindingState.InputBinding{ .key = .{ .bind_point = .tab, .modifiers = .shiftMod } },
    .activate_focused = BindingState.InputBinding{ .key = .{ .bind_point = .enter } },
    .close_focus_scope = BindingState.InputBinding{ .key = .{ .bind_point = .escape } },
};

gpa: std.mem.Allocator,
frame_arena: std.mem.Allocator,
renderer: *Batch2D,
asset_cache: *AssetCache,
bindings: *BindingState,

clay_context: *clay.Context,
clay_memory: []const u8,

render_commands: ?[]clay.RenderCommand = null,
events: std.ArrayList(Event) = .empty,

user_data: ?*anyopaque = null,

open_layout: bool = false,
dimensions: vec2 = .{ 0, 0 },

// A stack of widget themes
theme_stack: std.ArrayList(*const Theme) = .empty,
theme_match_levels: std.StringHashMapUnmanaged(Theme.MatchLevels) = .empty,

// A stack of all elements currently under the mouse, populated during layout. The last element is the top-most.
hovered_element_stack: std.ArrayList(StateProperty) = .empty,
// Map from navigation index to element ID for quick lookup during keyboard navigation
navigable_elements: std.AutoHashMapUnmanaged(u32, struct { id: ElementId, state: ElementStateFlags }) = .empty,
// Map from ElementId.id to navigation index for reverse lookup during event generation
reverse_navigable_elements: std.AutoHashMapUnmanaged(u32, u32) = .empty,
// Stack of currently open element's user-provided stable IDs
open_ids: std.ArrayList(ElementId) = .empty,

// Scroll states for the current frame
current_scroll_states: std.AutoHashMapUnmanaged(u32, ScrollProperty) = .empty,
// Scroll states from the last frame
last_scroll_states: std.AutoHashMapUnmanaged(u32, ScrollProperty) = .empty,

// States for rich-interaction widgets
widget_states: std.AutoHashMapUnmanaged(u32, Widget) = .empty,

// events from widgets triggered this frame, to be dispatched on the next frame
widget_events: std.ArrayList(Event) = .empty,

// shared states for widgets like radio groups
shared_widget_states: std.AutoHashMapUnmanaged(u32, SharedWidgetState) = .empty,

// --- Overlay System State ---
overlay_state: OverlayState = .{},

// --- Interaction State ---
focus_scope_stack: std.ArrayList(ElementId) = .empty,
disabled_stack: std.ArrayList(bool) = .empty,

// Stores information about every element declared in the current frame.
frame_element_info: std.AutoHashMapUnmanaged(u32, FrameElementInfo) = .empty,

state: ElementState = .{},
last_state: ElementState = .{},
wheel_delta: vec2 = .{ 0, 0 },
char_input: []const BindingState.Char = &.{},

text_repeat: struct {
    timer: std.time.Timer = undefined,
    initial_delay: u64 = 350 * std.time.ns_per_ms,
    nth_delay: u64 = 50 * std.time.ns_per_ms,
    state: enum {
        none,
        first,
        nth,
        fn advance(self: *@This()) void {
            self.* = switch (self.*) {
                .none => .first,
                .first => .nth,
                else => self.*,
            };
        }
    } = .none,
} = .{},

click_state: struct {
    timer: std.time.Timer = undefined,
    // Double-click state
    last_click_time: u64 = 0,
    last_click_element_id: ?u32 = null,
    double_click_threshold_ms: u64 = 250,
    // Drag-to-click differentiation state
    drag_state: struct {
        start_pos: vec2 = .{ 0, 0 },
        start_time: u64 = 0,
        is_dragging: bool = false,
    } = .{},
    drag_time_threshold_ms: u64 = 150,
    drag_dist_threshold: f32 = 5.0,
} = .{},

pub fn init(gpa: std.mem.Allocator, frame_arena: std.mem.Allocator, renderer: *Batch2D, asset_cache: *AssetCache, bindings: *BindingState) !*Ui {
    const self = try gpa.create(Ui);
    errdefer gpa.destroy(self);

    // Init Clay
    const min_memory_size = clay.minMemorySize();
    const clay_memory = try gpa.alloc(u8, min_memory_size);
    errdefer gpa.free(clay_memory);

    const clay_arena = clay.createArenaWithCapacityAndMemory(clay_memory);
    const clay_context = clay.initialize(
        clay_arena,
        clay.Dimensions{ .w = 0, .h = 0 },
        clay.ErrorHandler{
            .error_handler_function = reportClayError,
            .user_data = self,
        },
    );
    defer clay.setCurrentContext(null);

    // Register our text measurement function with Clay. Clay will call this
    // function during layout to determine the size of text elements.
    clay.setMeasureTextFunction(
        *Ui,
        self,
        measureTextCallback,
    );

    self.* = Ui{
        .gpa = gpa,
        .frame_arena = frame_arena,
        .renderer = renderer,
        .asset_cache = asset_cache,
        .bindings = bindings,
        .clay_context = clay_context,
        .clay_memory = clay_memory,
    };

    self.click_state.timer = try .start();
    self.text_repeat.timer = try .start();

    // Ensure required ui input bindings are registered
    if (!bindings.hasBinding(.primary_mouse)) try bindings.bind(.primary_mouse, default_bindings.primary_mouse);
    if (!bindings.hasBinding(.focus_next)) try bindings.bind(.focus_next, default_bindings.focus_next);
    if (!bindings.hasBinding(.focus_prev)) try bindings.bind(.focus_prev, default_bindings.focus_prev);
    if (!bindings.hasBinding(.activate_focused)) try bindings.bind(.activate_focused, default_bindings.activate_focused);
    if (!bindings.hasBinding(.close_focus_scope)) try bindings.bind(.close_focus_scope, default_bindings.close_focus_scope);

    return self;
}

pub fn deinit(self: *Ui) void {
    var state_it = self.widget_states.valueIterator();
    while (state_it.next()) |vtable| {
        vtable.deinit(vtable.user_data, self);
    }

    var shared_it = self.shared_widget_states.valueIterator();
    while (shared_it.next()) |entry| {
        entry.deinit(entry.data, self);
    }

    self.events.deinit(self.gpa);
    self.theme_stack.deinit(self.gpa);
    self.theme_match_levels.deinit(self.gpa);
    self.hovered_element_stack.deinit(self.gpa);
    self.navigable_elements.deinit(self.gpa);
    self.reverse_navigable_elements.deinit(self.gpa);
    self.open_ids.deinit(self.gpa);
    self.current_scroll_states.deinit(self.gpa);
    self.last_scroll_states.deinit(self.gpa);
    self.focus_scope_stack.deinit(self.gpa);
    self.frame_element_info.deinit(self.gpa);
    self.disabled_stack.deinit(self.gpa);
    self.overlay_state.deinit(self.gpa);

    self.widget_states.deinit(self.gpa);
    self.widget_events.deinit(self.gpa);

    self.gpa.free(self.clay_memory);
    self.gpa.destroy(self);
}

pub fn wantsMouse(self: *Ui, root_element_style: enum(u1) { containerized = 0, fullscreen = 1 }) bool {
    clay.setCurrentContext(self.clay_context);
    defer clay.setCurrentContext(null);

    return clay.getPointerOverIds().len > @intFromEnum(root_element_style);
}

/// Call this at any time in the update stage to begin declaring the ui layout. Caller must ensure `Ui.endLayout` is called before the next `Ui.beginLayout` and/or `Ui.render`.
/// Note: it is acceptable to run the layout code multiple times per frame, but all state will be discarded when calling this function.
pub fn beginLayout(self: *Ui, allow_mouse: bool, dimensions: vec2, delta_ms: f32) !void {
    log.debug("beginLayout", .{});
    self.render_commands = null;

    self.theme_stack.clearRetainingCapacity();
    self.hovered_element_stack.clearRetainingCapacity();
    self.navigable_elements.clearRetainingCapacity();
    self.reverse_navigable_elements.clearRetainingCapacity();
    self.open_ids.clearRetainingCapacity();
    self.frame_element_info.clearRetainingCapacity();
    self.disabled_stack.clearRetainingCapacity();

    self.last_state = self.state; // Save the last frame's state for event generation
    self.state = .{}; // Reset current state; will be populated during layout and event generation
    // The active and focused elements persist unless an event changes them.
    self.state.active_id = self.last_state.active_id;
    self.state.focused_id = self.last_state.focused_id;

    if (allow_mouse) {
        self.wheel_delta = self.bindings.consumeScrollDelta();
    } else {
        self.wheel_delta = .{ 0, 0 };
    }

    self.char_input = self.bindings.consumeCharInput();

    const last_scrolls = self.last_scroll_states;
    self.last_scroll_states = self.current_scroll_states;
    self.current_scroll_states = last_scrolls;
    self.current_scroll_states.clearRetainingCapacity();

    var shared_it = self.shared_widget_states.iterator();
    while (shared_it.next()) |entry| {
        entry.value_ptr.seen_this_frame = false;
    }

    clay.setCurrentContext(self.clay_context);

    clay.setLayoutDimensions(vec2ToDims(dimensions));
    self.dimensions = dimensions;

    // Although we inform Clay of current mouse button state, it is only used in debug mode or for drag scrolling.
    clay.setPointerState(vec2ToClay(self.bindings.getMousePosition()), allow_mouse and self.bindings.getAction(.primary_mouse).isDown());

    clay.updateScrollContainers(
        false, // never use drag scrolling
        vec2ToClay(self.wheel_delta),
        delta_ms,
    );

    clay.beginLayout();

    self.open_layout = true;
}

/// End the current layout declaration and finalize the render commands and events.
/// Must be called after `Ui.beginLayout` and before `Ui.render`.
pub fn endLayout(self: *Ui) !void {
    log.debug("endLayout", .{});

    std.debug.assert(self.clay_context == clay.getCurrentContext());
    defer clay.setCurrentContext(null);

    self.render_commands = clay.endLayout();

    var state_to_remove = std.ArrayList(u32).empty;
    var widget_it = self.widget_states.iterator();
    while (widget_it.next()) |kv| {
        if (!kv.value_ptr.seen_this_frame) {
            try state_to_remove.append(self.frame_arena, kv.key_ptr.*);
        }
    }

    for (state_to_remove.items) |key| {
        if (self.widget_states.fetchRemove(key)) |removed_entry| {
            removed_entry.value.deinit(removed_entry.value.user_data, self);
        }
    }

    var shared_to_remove = std.ArrayList(u32).empty;

    var shared_it = self.shared_widget_states.iterator();
    while (shared_it.next()) |entry| {
        if (!entry.value_ptr.seen_this_frame) {
            try shared_to_remove.append(self.frame_arena, entry.key_ptr.*);
        }
    }

    for (shared_to_remove.items) |key| {
        if (self.shared_widget_states.fetchRemove(key)) |removed_entry| {
            const state = removed_entry.value;
            state.deinit(state.data, self);
        }
    }

    for (self.overlay_state.stack.items) |*overlay_info| {
        if (self.getElementBounds(overlay_info.id)) |bb| {
            overlay_info.last_size = .{ bb.width, bb.height };
        }
    }

    self.open_layout = false;

    try self.generateEvents();
}

pub fn getEvent(self: *Ui, element_id: ElementId, event_tag: Event.Tag) ?Event {
    for (self.events.items) |event| { // TODO: change to hashmap
        if (event.data == event_tag and event.info.element_id.id == element_id.id) {
            return event;
        }
    }
    return null;
}

/// After calling `Ui.beginLayout` and `Ui.endLayout`, this function issues the generated draw calls to Batch2D to render the UI.
/// User should call this between `Batch2D.beginFrame()` and `Batch2D.endFrame()`.
pub fn render(self: *Ui) !void {
    std.debug.assert(!self.open_layout);

    clay.setCurrentContext(self.clay_context);
    defer clay.setCurrentContext(null);

    try self.draw();
}

pub fn setDebugMode(self: *Ui, enabled: bool) void {
    clay.setCurrentContext(self.clay_context);
    defer clay.setCurrentContext(null);

    clay.setDebugModeEnabled(enabled);
}

// --- Element Interface ---

fn _openElement(self: *Ui, id: ElementId) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

    try self.open_ids.append(self.gpa, id);

    log.debug("openElement({s})", .{id.string_id.toSlice()});

    clay.beginElement();
}

fn _configureElement(self: *Ui, declaration: HeadlessElementConfig) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

    const id = self.open_ids.items[self.open_ids.items.len - 1];

    log.debug("configureElement({s})", .{id.string_id.toSlice()});

    const full = declaration.attachHead(id);
    clay.configureElement(full.toClay());

    try self.handleStateSetup(full);
}

fn _endElement(self: *Ui) void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

    const id = self.open_ids.pop().?;
    log.debug("endElement({s})", .{id.string_id.toSlice()});

    clay.closeElement();
}

fn _text(self: *Ui, str: []const u8, config: TextConfig) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    std.debug.assert(self.open_ids.items.len > 0);

    clay.text(str, config.toClay());
}

/// Determine if the currently-open element is hovered by the mouse.
/// * This is for styling logic, not event handling; use the generated event stream for that.
pub fn hovered(self: *Ui) bool {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    std.debug.assert(self.open_ids.items.len > 0);

    return clay.pointerOver(self.open_ids.items[self.open_ids.items.len - 1]);
}

/// Get the current scroll offset of the currently-open scroll container element.
/// * If the current element is not a scroll container, returns {0,0}.
/// * This is for styling logic, not event handling; use the generated event stream for that.
pub fn scrollOffset(self: *Ui) vec2 {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    std.debug.assert(self.open_ids.items.len > 0);

    return vec2FromClay(clay.getScrollOffset());
}

/// Determine if the currently open element is focused for keyboard input.
/// * This is for styling logic, not event handling; use the generated event stream for that.
pub fn focused(self: *Ui) bool {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    std.debug.assert(self.open_ids.items.len > 0);

    const focused_id = self.state.focusedIdValue() orelse return false;
    const current_id = self.open_ids.items[self.open_ids.items.len - 1].id;

    return focused_id == current_id;
}

/// Determine if the currently open element is the active element, meaning it is currently being clicked or interacted with.
/// * This is for styling logic, not event handling; use the generated event stream for that.
pub fn active(self: *Ui) bool {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    std.debug.assert(self.open_ids.items.len > 0);

    const active_id = self.state.activeIdValue() orelse return false;
    const current_id = self.open_ids.items[self.open_ids.items.len - 1].id;

    return active_id == current_id;
}

/// Hacky helper for menus, checks last frame state rather than current like other functions.
pub fn activated(self: *Ui, id: ElementId) bool {
    return self.last_state.activeIdValue() == id.id;
}

/// Returns true if the current UI scope is disabled.
pub fn disabled(self: *Ui) bool {
    if (self.disabled_stack.items.len == 0) return false;
    return self.disabled_stack.items[self.disabled_stack.items.len - 1];
}

/// Get the currently active element's ID, or null if there is no active element.
pub fn activeId(self: *Ui) ?ElementId {
    return self.state.activeId();
}

/// Get the active element ID from the last frame, or null if there was no active element.
pub fn lastActiveId(self: *Ui) ?ElementId {
    return self.last_state.activeId();
}

/// Get the currently focused element's ID, or null if there is no focused element.
pub fn focusedId(self: *Ui) ?ElementId {
    return self.state.focusedId();
}

/// Get the focused element ID from the last frame, or null if there was no focused element.
pub fn lastFocusedId(self: *Ui) ?ElementId {
    return self.last_state.focusedId();
}

pub fn isHovered(self: *Ui, id: ElementId) bool {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    return clay.pointerOver(id);
}

pub fn isActive(self: *Ui, id: ElementId) bool {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    const active_id = self.activeId() orelse return false;
    return id.id == active_id.id;
}

pub fn isFocused(self: *Ui, id: ElementId) bool {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    const focused_id = self.focusedId() orelse return false;
    return id.id == focused_id.id;
}

/// Pushes an element ID onto the focus scope stack.
/// While a scope is active, focus navigation events (Tab, Shift+Tab) will be sent
/// to the element at the top of the stack instead of performing global focus changes.
pub fn pushFocusScope(self: *Ui, id: ElementId) !void {
    try self.focus_scope_stack.append(self.gpa, id);
}

/// Pops the top element ID from the focus scope stack.
pub fn popFocusScope(self: *Ui) void {
    _ = self.focus_scope_stack.pop();
}

/// Pushes a disabled state to a stack.
pub fn pushDisabled(self: *Ui, is_disabled: bool) !void {
    try self.disabled_stack.append(self.gpa, is_disabled);
}

pub fn popDisabled(self: *Ui) void {
    _ = self.disabled_stack.pop();
}

// --- Overlay API ---

pub const OverlayState = struct {
    pub const OVERLAY_Z_INDEX_BASE: i16 = 1000;

    pub const OverlayInfo = struct {
        id: ElementId,
        parent_overlay_id: ?ElementId, // The panel that owns this overlay (keeps stack alive)
        trigger_item_id: ?ElementId, // The item to attach the visual position to
        position: vec2,
        last_size: vec2 = .{ 0, 0 },
    };

    stack: std.ArrayList(OverlayInfo) = .empty,
    open_request: ?OverlayInfo = null,
    close_request_level: ?usize = null,
    just_opened_id: ?u32 = null,

    fn deinit(self: *OverlayState, gpa: std.mem.Allocator) void {
        self.stack.deinit(gpa);
    }
};

pub fn openOverlay(self: *Ui, id: ElementId, parent_overlay_id: ?ElementId, trigger_item_id: ?ElementId, position: vec2) void {
    self.overlay_state.open_request = .{
        .id = id,
        .parent_overlay_id = parent_overlay_id,
        .trigger_item_id = trigger_item_id,
        .position = position,
    };
}

pub fn closeTopOverlay(self: *Ui) void {
    if (self.overlay_state.stack.items.len > 0) {
        self.overlay_state.close_request_level = self.overlay_state.stack.items.len - 1;
    }
}

pub fn closeAllOverlays(self: *Ui) void {
    if (self.overlay_state.stack.items.len > 0) {
        self.overlay_state.close_request_level = 0;
    }
}

fn handleOverlayState(self: *Ui) !void {
    const os = &self.overlay_state;
    os.just_opened_id = null;

    // --- 1. Process deferred close requests ---
    if (os.close_request_level) |level| {
        while (os.stack.items.len > level) {
            const closed_overlay = os.stack.pop() orelse break;
            self.popFocusScope();
            try self.pushEvent(closed_overlay.id, .{ .menu_closed = .{ .level = @intCast(os.stack.items.len) } }, null);
        }

        if (os.stack.items.len > 0) {
            const parent_overlay = os.stack.items[os.stack.items.len - 1];
            self.state.focused_id = .{
                .id = parent_overlay.id,
                .bounding_box = clay.getElementData(parent_overlay.id).bounding_box,
                .state = .flags(.{ .focus = true, .keyboard = true, .activate = true }),
            };
        } else {
            self.state.focused_id = null;
        }
        os.close_request_level = null;
    }

    // --- 2. Process deferred open requests ---
    if (os.open_request) |request| {
        var open_at_level: usize = 0;
        if (request.parent_overlay_id) |parent_id| {
            // Find parent level in stack to ensure we nest correctly
            for (os.stack.items, 0..) |info, i| {
                if (info.id.id == parent_id.id) {
                    open_at_level = i + 1;
                    break;
                }
            }
        }

        while (os.stack.items.len > open_at_level) {
            const closed = os.stack.pop() orelse break;
            self.popFocusScope();
            try self.pushEvent(closed.id, .{ .menu_closed = .{ .level = @intCast(os.stack.items.len) } }, null);
        }

        try os.stack.append(self.gpa, request);
        os.just_opened_id = request.id.id;
        try self.pushFocusScope(request.id);
        try self.pushEvent(request.id, .menu_opened, null);

        self.state.focused_id = .{
            .id = request.id,
            .bounding_box = .{},
            .state = .flags(.{ .focus = true, .keyboard = true, .activate = true }),
        };
        os.open_request = null;
    }

    // --- 3. Handle "click outside" to close all overlays ---
    if (self.bindings.getAction(.primary_mouse) == .pressed and os.stack.items.len > 0) {
        var click_inside = false;
        const mouse_pos = self.bindings.getMousePosition();

        for (os.stack.items) |info| {
            const data = clay.getElementData(info.id);
            if (data.found and boxContains(data.bounding_box, mouse_pos)) {
                click_inside = true;
                break;
            }
        }

        if (!click_inside) {
            self.closeAllOverlays();
        }
    }
}

// End Overlay API //

/// Get the offset data for a character within the only text element inside the element with the given id.
pub fn getCharacterOffset(self: *Ui, id: ElementId, char_index: u32) ?CharacterOffset {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    return clay.getCharacterOffset(id, char_index);
}

pub fn getCharacterIndexAtOffset(self: *Ui, id: ElementId, offset: vec2) ?u32 {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    return clay.getCharacterIndexAtOffset(id, vec2ToClay(offset));
}

pub fn getCharacterOffsetAtPoint(self: *Ui, id: ElementId, point: vec2) ?CharacterOffset {
    const index = self.getCharacterIndexAtOffset(id, point) orelse return null;
    return self.getCharacterOffset(id, index);
}

pub fn getElementBounds(self: *Ui, id: ElementId) ?BoundingBox {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    const data = clay.getElementData(id);
    return if (data.found) data.bounding_box else null;
}

pub const ScrollContainerData = struct {
    /// Pointer to the internal scroll position (mutable)
    /// Modifying this will change the actual scroll position
    scroll_position: *clay.Vector2, // NOTE: clay.Vector2 has a different alignment than vec2, so we can't pointer cast safely
    /// Bounding box of the scroll container
    scroll_container_dimensions: vec2,
    /// Dimensions of the inner content, including parent padding
    content_dimensions: vec2,
    /// Original scroll config
    config: ClipElementConfig,
};

pub fn getElementScrollData(self: *Ui, id: ElementId) ?ScrollContainerData {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

    const data = clay.getScrollContainerData(id);
    return if (data.found) .{
        .scroll_position = data.scroll_position,
        .scroll_container_dimensions = vec2FromDims(data.scroll_container_dimensions),
        .content_dimensions = vec2FromDims(data.content_dimensions),
        .config = .fromClay(data.config),
    } else null;
}

pub fn pushEvent(self: *Ui, id: ElementId, data: Event.Data, user_data: ?*anyopaque) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

    // NOTE: we do not check if element_data was found here,
    // because this allows dispatching arbitrary events for non-existent elements.
    // The bounding box will be {0,0,0,0} in this case.
    const element_data = clay.getElementData(id);

    try self.widget_events.append(self.gpa, Event{
        .info = Event.Info{
            .element_id = id,
            .bounding_box = element_data.bounding_box,
            .user_data = user_data,
        },
        .data = data,
    });
}

/// Programmatically sets the value of a shared widget state, such as a radio button group.
/// If the state does not yet exist for the given group_id, it will be created.
pub fn setSharedWidgetState(self: *Ui, group_id: ElementId, comptime T: type, state: T) !void {
    const key = group_id.id;

    if (self.shared_widget_states.getPtr(key)) |entry| {
        // State already exists, so we just need to update it.
        // First, ensure the types match to prevent corruption.
        if (entry.state_type != @as(*const anyopaque, @typeName(T))) {
            return error.InvalidWidgetStateType;
        }

        // Cast the generic pointer to the correct type and set the value.
        const state_ptr: *T = @ptrCast(@alignCast(entry.data));
        state_ptr.* = state;
    } else {
        // The state does not exist. We will create it. This is useful for setting
        // the initial state of a group before any of its widgets are declared.
        const ptr = try self.gpa.create(T);
        errdefer self.gpa.destroy(ptr); // In case the put fails.
        ptr.* = state;

        const new_entry = SharedWidgetState{
            .data = ptr,
            .deinit = @ptrCast(&struct {
                pub fn destructor(state_ptr: *T, ui: *Ui) void {
                    ui.gpa.destroy(state_ptr);
                }
            }.destructor),
            .state_type = @typeName(T),
            // We mark this as 'seen' because we are intentionally creating it. This
            // prevents it from being garbage collected at the end of the frame if
            // no widgets from its group happen to be declared.
            .seen_this_frame = true,
        };
        try self.shared_widget_states.put(self.gpa, key, new_entry);
    }
}

pub fn getOrCreateSharedWidgetState(self: *Ui, comptime T: type, group_id: ElementId) !struct { *T, bool } {
    const key = group_id.id;

    const gop = try self.shared_widget_states.getOrPut(self.gpa, key);

    if (!gop.found_existing) {
        const ptr = try self.gpa.create(T);

        gop.value_ptr.* = SharedWidgetState{
            .data = ptr,
            .deinit = @ptrCast(&struct {
                pub fn destructor(state_ptr: *T, ui: *Ui) void {
                    ui.gpa.destroy(state_ptr);
                    if (comptime std.meta.hasMethod(T, "deinit")) state_ptr.deinit(ui);
                }
            }.destructor),
            .state_type = @typeName(T),
            .seen_this_frame = true,
        };

        return .{ ptr, true };
    } else {
        gop.value_ptr.seen_this_frame = true;

        if (gop.value_ptr.state_type != @as(*const anyopaque, @typeName(T))) {
            return error.SharedStateWrongType;
        }

        return .{ @ptrCast(@alignCast(gop.value_ptr.data)), false };
    }
}

pub fn getOrPutSharedWidgetState(self: *Ui, comptime T: type, group_id: ElementId, default_value: T) !*T {
    const ptr, const new = try self.getOrCreateSharedWidgetState(T, group_id);
    if (new) ptr.* = default_value;
    return ptr;
}

// --- Structures ---

pub const Event = struct {
    info: Info,
    data: Data,

    pub const Tag = std.meta.Tag(Data);

    pub fn Payload(comptime tag: Tag) type {
        @setEvalBranchQuota(10_000);
        return std.meta.TagPayload(Data, tag);
    }

    pub const Info = struct {
        element_id: ElementId,
        bounding_box: BoundingBox,
        user_data: ?*anyopaque,
    };

    pub const Data = union(enum) {
        hover_begin: struct { mouse_position: vec2, modifiers: BindingState.Modifiers },
        hovering: struct { mouse_position: vec2, modifiers: BindingState.Modifiers },
        hover_end: struct { mouse_position: vec2, modifiers: BindingState.Modifiers },

        mouse_down: struct { mouse_position: vec2, modifiers: BindingState.Modifiers },
        mouse_up: struct { mouse_position: vec2, end_element: ?ElementId, modifiers: BindingState.Modifiers },
        clicked: struct { mouse_position: vec2, modifiers: BindingState.Modifiers },
        double_clicked: struct { mouse_position: vec2, modifiers: BindingState.Modifiers },

        drag_begin: struct { mouse_position: vec2, modifiers: BindingState.Modifiers },
        drag: struct { mouse_position: vec2, modifiers: BindingState.Modifiers },
        drag_end: struct { mouse_position: vec2, modifiers: BindingState.Modifiers },

        text: union(enum) {
            chars: []const BindingState.Char,
            command: struct {
                action: enum { delete, backspace, newline, copy, paste, select_all, move_left, move_right, move_up, move_down, home, end },
                modifiers: BindingState.Modifiers,
            },
        },

        wheel: struct { delta: vec2, modifiers: BindingState.Modifiers },

        focus_gained: void,
        focusing: void,
        focus_lost: void,

        activate_begin: void,
        activating: void,
        activate_end: struct { end_element: ?ElementId, modifiers: BindingState.Modifiers },

        key_down: struct {
            key: BindingState.Key,
            modifiers: BindingState.Modifiers,
        },
        key: struct {
            key: BindingState.Key,
            modifiers: BindingState.Modifiers,
        },
        key_up: struct {
            key: BindingState.Key,
            modifiers: BindingState.Modifiers,
        },

        scroll: struct {
            old_offset: vec2,
            new_offset: vec2,
            delta: vec2,
        },

        scoped_focus_change: enum { next, prev },
        scoped_focus_close: void,

        menu_opened: void,
        menu_closed: struct { level: u32 },
    };
};

pub const ElementState = struct {
    hovered: ?StateProperty = null,
    active_id: ?StateProperty = null,
    focused_id: ?StateProperty = null,

    pub fn hoveredId(self: ElementState) ?ElementId {
        return (self.hovered orelse return null).id;
    }

    pub fn activeId(self: ElementState) ?ElementId {
        return (self.active_id orelse return null).id;
    }

    pub fn focusedId(self: ElementState) ?ElementId {
        return (self.focused_id orelse return null).id;
    }

    pub fn hoveredIdValue(self: ElementState) ?u32 {
        return (self.hovered orelse return null).id.id;
    }

    pub fn activeIdValue(self: ElementState) ?u32 {
        return (self.active_id orelse return null).id.id;
    }

    pub fn focusedIdValue(self: ElementState) ?u32 {
        return (self.focused_id orelse return null).id.id;
    }
};

pub const StateProperty = struct {
    id: ElementId,
    bounding_box: BoundingBox,
    state: ElementStateFlags,
};

pub const ScrollProperty = struct {
    offset: vec2,
    elem_state: StateProperty,
};

pub const ElementStateFlags = packed struct(usize) {
    event_flags: Flags,
    user_data: u48 = 0,

    pub const Flags = packed struct(u16) {
        hover: bool = false,
        wheel: bool = false,
        click: bool = false,
        drag: bool = false,
        focus: bool = false,
        activate: bool = false,
        text: bool = false,
        keyboard: bool = false,
        scroll: bool = false,
        change: bool = false,

        _reserved: u6 = 0,

        pub const none = Flags{};
        pub const hoverFlag = Flags{ .hover = true };
        pub const wheelFlag = Flags{ .wheel = true };
        pub const clickFlag = Flags{ .click = true };
        pub const dragFlag = Flags{ .drag = true };
        pub const focusFlag = Flags{ .focus = true };
        pub const activateFlag = Flags{ .activate = true };
        pub const textFlag = Flags{ .text = true };
        pub const keyboardFlag = Flags{ .keyboard = true };
        pub const scrollFlag = Flags{ .scroll = true };
        pub const changeFlag = Flags{ .change = true };
        pub const all = Flags{
            .hover = true,
            .wheel = true,
            .click = true,
            .drag = true,
            .focus = true,
            .activate = true,
            .text = true,
            .keyboard = true,
            .scroll = true,
            .change = true,
        };

        pub fn merge(a: Flags, b: Flags) Flags {
            return Flags{
                .hover = a.hover or b.hover,
                .wheel = a.wheel or b.wheel,
                .click = a.click or b.click,
                .drag = a.drag or b.drag,
                .focus = a.focus or b.focus,
                .activate = a.activate or b.activate,
                .text = a.text or b.text,
                .keyboard = a.keyboard or b.keyboard,
                .scroll = a.scroll or b.scroll,
            };
        }

        pub fn takesInput(self: Flags) bool {
            return self.hover or self.wheel or self.click or self.drag or self.focus or self.activate or self.text or self.keyboard or self.scroll;
        }

        pub fn usesMouse(self: Flags) bool {
            return self.hover or self.wheel or self.click or self.drag or self.scroll;
        }
    };

    pub const none = ElementStateFlags{ .event_flags = .none };
    pub const hoverFlag = ElementStateFlags{ .event_flags = .hoverFlag };
    pub const wheelFlag = ElementStateFlags{ .event_flags = .wheelFlag };
    pub const clickFlag = ElementStateFlags{ .event_flags = .clickFlag };
    pub const dragFlag = ElementStateFlags{ .event_flags = .dragFlag };
    pub const focusFlag = ElementStateFlags{ .event_flags = .focusFlag };
    pub const activateFlag = ElementStateFlags{ .event_flags = .activateFlag };
    pub const textFlag = ElementStateFlags{ .event_flags = .textFlag };
    pub const keyboardFlag = ElementStateFlags{ .event_flags = .keyboardFlag };
    pub const scrollFlag = ElementStateFlags{ .event_flags = .scrollFlag };
    pub const changeFlag = ElementStateFlags{ .event_flags = .changeFlag };
    pub const all = ElementStateFlags{ .event_flags = .all };

    pub fn flags(f: Flags) ElementStateFlags {
        return ElementStateFlags{ .event_flags = f };
    }

    pub fn custom(f: Flags, user_data: ?*anyopaque) ElementStateFlags {
        return ElementStateFlags{
            .event_flags = f,
            .user_data = @intCast(@intFromPtr(user_data)),
        };
    }

    fn fromClay(data: ?*anyopaque) ElementStateFlags {
        if (data) |ptr| {
            return @bitCast(@as(usize, @intFromPtr(ptr)));
        } else {
            return .none;
        }
    }

    fn toClay(self: ElementStateFlags) ?*anyopaque {
        // This converts directly to the nullable pointer so there's no risk from the runtime safety check here.
        return @ptrFromInt(@as(usize, @bitCast(self)));
    }

    pub fn takesInput(self: ElementStateFlags) bool {
        return self.event_flags.takesInput();
    }

    pub fn usesMouse(self: ElementStateFlags) bool {
        return self.event_flags.usesMouse();
    }

    pub fn getUserData(self: ElementStateFlags) ?*anyopaque {
        // This converts directly to the nullable pointer so there's no risk from the runtime safety check here.
        return @ptrFromInt(self.user_data);
    }
};

pub const ElementId = clay.ElementId;
pub const BoundingBox = clay.BoundingBox;
pub const SizingMinMax = clay.SizingMinMax;
pub const SizingConstraint = clay.SizingConstraint;
pub const SizingType = clay.SizingType;
pub const SizingAxis = clay.SizingAxis;
pub const Sizing = clay.Sizing;
pub const Padding = clay.Padding;
pub const TextElementConfigWrapMode = clay.TextElementConfigWrapMode;
pub const TextAlignment = clay.TextAlignment;
pub const FloatingAttachPointType = clay.FloatingAttachPointType;
pub const FloatingAttachPoints = clay.FloatingAttachPoints;
pub const FloatingAttachToElement = clay.FloatingAttachToElement;
pub const FloatingClipToElement = clay.FloatingClipToElement;
pub const PointerCaptureMode = clay.PointerCaptureMode;
pub const RenderCommandType = clay.RenderCommandType;
pub const CornerRadius = clay.CornerRadius;
pub const LayoutDirection = clay.LayoutDirection;
pub const LayoutAlignmentX = clay.LayoutAlignmentX;
pub const LayoutAlignmentY = clay.LayoutAlignmentY;
pub const ChildAlignment = clay.ChildAlignment;
pub const BorderWidth = clay.BorderWidth;
pub const LayoutConfig = clay.LayoutConfig;
pub const CharacterOffset = clay.CharacterOffset;
pub const RenderCommand = clay.RenderCommand;

pub const AspectRatioElementConfig = f32;
pub const ImageElementConfig = ?AssetCache.ImageId;
pub const CustomElementConfig = ?*anyopaque;

pub const TextConfig = struct {
    /// The RGBA color of the font to render, conventionally specified as 0-255.
    color: Color = .{ .r = 0, .g = 0, .b = 0, .a = 1.0 },
    /// Identifies the font to use.
    font_id: AssetCache.FontId = 0, // The debug view will pass fontId = 0 for its internal text.
    /// Controls the size of the font.
    font_size: u16 = 20, // Handled by the function provided to Clay_MeasureText.
    /// Controls extra horizontal spacing between characters.
    letter_spacing: u16 = 0, // TODO: Not yet handled by the function provided to Clay_MeasureText.
    /// Additional vertical space between wrapped lines of text
    line_height: u16 = 0,
    /// Controls how text "wraps", that is how it is broken into multiple lines when there is insufficient horizontal space.
    wrap_mode: TextElementConfigWrapMode = .words,
    /// Controls how wrapped lines of text are horizontally aligned within the outer text bounding box.
    alignment: TextAlignment = .left,
    /// A pointer that will be transparently passed through to the resulting render command.
    user_data: ?*anyopaque = null,

    fn toClay(self: TextConfig) clay.TextElementConfig {
        return clay.TextElementConfig{
            .color = colorToClay(self.color),
            .font_id = self.font_id,
            .font_size = self.font_size,
            .letter_spacing = self.letter_spacing,
            .line_height = self.line_height,
            .wrap_mode = self.wrap_mode,
            .alignment = self.alignment,
            .user_data = self.user_data,
        };
    }
};

pub const FloatingElementConfig = struct {
    /// Offsets this floating element by these x,y coordinates from its attachPoints
    offset: vec2 = .{ 0, 0 },
    /// Expands the boundaries of the outer floating element without affecting children
    expand: vec2 = .{ 0, 0 },
    /// When using CLAY_ATTACH_TO_ELEMENT_WITH_ID, attaches to element with this ID
    parentId: u32 = 0,
    /// Z-index controls stacking order (ascending)
    z_index: i16 = 0,
    /// Controls attachment points between floating element and its parent
    attach_points: FloatingAttachPoints = .{},
    /// Controls whether pointer events are captured or pass through to elements underneath
    pointer_capture_mode: PointerCaptureMode = .capture,
    /// Controls which element this floating element is attached to
    attach_to: FloatingAttachToElement = .none,
    /// Controls whether or not a floating element is clipped to the same clipping rectangle as the element it's attached to.
    clip_to: FloatingClipToElement = .none,

    fn toClay(self: FloatingElementConfig) clay.FloatingElementConfig {
        return clay.FloatingElementConfig{
            .offset = vec2ToClay(self.offset),
            .expand = vec2ToDims(self.expand),
            .parentId = self.parentId,
            .z_index = self.z_index,
            .attach_points = self.attach_points,
            .pointer_capture_mode = self.pointer_capture_mode,
            .attach_to = self.attach_to,
            .clip_to = self.clip_to,
        };
    }
};

pub const BorderElementConfig = struct {
    /// Color of all borders with width > 0
    color: Color = .{ .r = 0, .g = 0, .b = 0, .a = 255 },
    /// Widths of individual borders
    width: BorderWidth = .{},

    fn toClay(self: BorderElementConfig) clay.BorderElementConfig {
        return clay.BorderElementConfig{
            .color = colorToClay(self.color),
            .width = self.width,
        };
    }
};

pub const ClipElementConfig = struct {
    /// Whether to enable horizontal scrolling
    horizontal: bool = false,
    /// Whether to enable vertical scrolling
    vertical: bool = false,
    // Offsets the x,y positions of all child elements.
    child_offset: vec2 = .{ 0, 0 },

    fn fromClay(in: clay.ClipElementConfig) ClipElementConfig {
        return ClipElementConfig{
            .horizontal = in.horizontal,
            .vertical = in.vertical,
            .child_offset = vec2FromClay(in.child_offset),
        };
    }

    fn toClay(self: ClipElementConfig) clay.ClipElementConfig {
        return clay.ClipElementConfig{
            .horizontal = self.horizontal,
            .vertical = self.vertical,
            .child_offset = vec2ToClay(self.child_offset),
        };
    }
};

pub const HeadlessElementConfig = struct {
    /// Controls various settings that affect the size and position of an element, as well as the sizes and positions of any child elements.
    layout: LayoutConfig = .{},
    /// Controls the background color of the resulting element.
    /// By convention specified as 0-255, but interpretation is up to the renderer.
    /// If no other config is specified, `.background_color` will generate a `RECTANGLE` render command, otherwise it will be passed as a property to `IMAGE` or `CUSTOM` render commands.
    background_color: Color = .{},
    /// Controls the "radius", or corner rounding of elements, including rectangles, borders and images.
    corner_radius: CornerRadius = .{},
    // Controls settings related to aspect ratio scaling.
    aspect_ratio: AspectRatioElementConfig = 0,
    /// Controls settings related to image elements.
    image: ImageElementConfig = null,
    /// Controls whether and how an element "floats", which means it layers over the top of other elements in z order, and doesn't affect the position and size of siblings or parent elements.
    /// Note: in order to activate floating, `.floating.attachTo` must be set to something other than the default value.
    floating: FloatingElementConfig = .{},
    /// Whether or not this element will be treated as a widget.
    type: ElementType = .content,
    /// Controls whether an element should clip its contents and allow scrolling rather than expanding to contain them.
    clip: ClipElementConfig = .{},
    /// Controls settings related to element borders, and will generate BORDER render command
    border: BorderElementConfig = .{},
    /// A pointer that will be transparently passed through to resulting render command
    state: ElementStateFlags = .none,

    fn attachHead(value: *const HeadlessElementConfig, head: ElementId) ElementConfig {
        var out: ElementConfig = undefined;

        out.id = head;

        inline for (comptime std.meta.fieldNames(HeadlessElementConfig)) |field| {
            @field(out, field) = @field(value, field);
        }

        return out;
    }
};

pub const ActionState = enum(u8) {
    standard,
    focus,
    hover,
    active,
    disabled,

    pub fn isOneOf(base: ActionState, elems: []const ActionState) bool {
        return std.mem.indexOfScalar(ActionState, elems, base) != null;
    }

    pub fn match(element: ActionState, selector: ActionState) ?Theme.Match {
        if (element == selector) return .exact;
        switch (element) {
            .standard => return null,
            .focus => if (selector.isOneOf(&.{ .standard, .hover, .active })) return .inherit,
            .hover => if (selector.isOneOf(&.{ .standard, .focus, .active })) return .inherit,
            .active => if (selector.isOneOf(&.{ .standard, .focus, .hover })) return .inherit,
            .disabled => if (selector == .standard) return .inherit,
        }
        return null;
    }
};

pub fn getActionState(self: *Ui) ActionState {
    return if (self.disabled()) .disabled else if (self.active()) .active else if (self.hovered()) .hover else if (self.focused()) .focus else .standard;
}

pub const Theme = struct {
    allocator: std.mem.Allocator,
    map: Map = .empty,

    pub fn init(allocator: std.mem.Allocator) Theme {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Theme) void {
        var it = self.map.valueIterator();
        while (it.next()) |prop_set| prop_set.deinit(self.allocator);
        self.map.deinit(self.allocator);
        self.map = .empty;
    }

    pub fn set(self: *Theme, comptime name: []const u8, kind: Kind, state: ActionState, value: anytype) !void {
        const name_gop = try self.map.getOrPut(self.allocator, name);
        const prop_set = name_gop.value_ptr;
        if (!name_gop.found_existing) prop_set.* = .empty;

        try prop_set.append(self.allocator, .{ .kind = kind, .state = state, .data = .create(value) });
    }

    pub fn setAll(self: *Theme, kind: Kind, state: ActionState, ksvs: anytype) !void {
        inline for (comptime std.meta.fieldNames(@TypeOf(ksvs))) |name| {
            try self.set(name, kind, state, @field(ksvs, name));
        }
    }

    pub fn apply(
        self: *const Theme,
        allocator: std.mem.Allocator,
        match_levels: *std.StringHashMapUnmanaged(MatchLevels),
        layout: *const Binding.Set,
        kind: Kind,
        state: ActionState,
        out: *anyopaque,
    ) !void {
        for (&layout.names, &layout.bindings) |name, *binding| {
            if (self.map.getPtr(name)) |property_set| {
                for (property_set.items) |property| {
                    const new_lvls = MatchLevels{
                        .kind = kind.match(property.kind) orelse continue,
                        .state = state.match(property.state) orelse continue,
                    };
                    const match_gop = try match_levels.getOrPut(allocator, name);
                    const ext_lvls = match_gop.value_ptr;

                    if (match_gop.found_existing) {
                        if (ext_lvls.kind == .exact and ext_lvls.state == .exact and new_lvls.kind != .exact and new_lvls.state != .exact) continue;
                    }

                    ext_lvls.* = new_lvls;

                    property.data.apply(binding, out);
                }
            }
        }
    }

    pub const Map = std.StringHashMapUnmanaged(std.ArrayList(Property));

    pub const Property = struct {
        kind: Kind = .content,
        state: ActionState = .standard,
        data: Data = .{},
    };

    pub const Match = enum { exact, inherit };
    pub const MatchLevels = struct { kind: Match = .inherit, state: Match = .inherit };

    pub const Kind = enum {
        content,
        widget,

        pub fn match(element: Kind, selector: Kind) ?Match {
            if (element == selector) return .exact;
            return switch (element) {
                .content => null,
                .widget => .inherit,
            };
        }
    };

    pub const Data = struct {
        size: u8 = 0,
        value: [Data.MAX_SIZE]u8 = [1]u8{0} ** Data.MAX_SIZE,

        pub const MAX_SIZE = 32;

        pub fn create(value: anytype) Data {
            var out = Data{
                .size = @sizeOf(@TypeOf(value)),
            };
            @memcpy(out.value[0..out.size], std.mem.asBytes(&value));
            return out;
        }

        pub fn apply(self: *const Data, binding: *const Binding, out: *anyopaque) void {
            std.debug.assert(self.size == binding.size);
            @memcpy(
                @as([*]u8, @ptrCast(out))[binding.offset .. binding.offset + binding.size],
                self.value[0..binding.size],
            );
        }
    };

    pub const Binding = struct {
        size: u16 = 0,
        offset: u16 = 0,

        pub const Set = struct {
            count: u8 = 0,
            names: [MAX_BINDINGS][]const u8 = [1][]const u8{""} ** Set.MAX_BINDINGS,
            bindings: [MAX_BINDINGS]Binding = [1]Binding{.{}} ** Set.MAX_BINDINGS,

            pub const ELEM_DECL_BIND_SET = Binding.Set.custom(HeadlessElementConfig, .{
                "background_color",
                .{ "border_color", "border.color" },
                .{ "border_left", "border.width.left" },
                .{ "border_right", "border.width.right" },
                .{ "border_top", "border.width.top" },
                .{ "border_bottom", "border.width.bottom" },
                .{ "border_between_children", "border.width.between_children" },
                .{ "radius_top_left", "corner_radius.top_left" },
                .{ "radius_top_right", "corner_radius.top_right" },
                .{ "radius_bottom_left", "corner_radius.bottom_left" },
                .{ "radius_bottom_right", "corner_radius.bottom_right" },
                .{ "padding_left", "layout.padding.left" },
                .{ "padding_right", "layout.padding.right" },
                .{ "padding_top", "layout.padding.top" },
                .{ "padding_bottom", "layout.padding.bottom" },
                .{ "child_gap", "layout.child_gap" },
                .{ "child_alignment", "layout.child_alignment" },
                .{ "child_direction", "layout.direction" },
            });

            pub const TEXT_CONF_BIND_SET = Binding.Set.custom(TextConfig, .{
                .{ "text_color", "color" },
                "font_id",
                "font_size",
                "letter_spacing",
                "line_height",
                .{ "text_wrap_mode", "wrap_mode" },
                .{ "text_alignment", "alignment" },
            });

            pub const MAX_BINDINGS = 32;

            pub fn find(self: *const Set, search_name: []const u8) ?*const Binding {
                for (self.names, 0..) |bound_name, i| {
                    if (std.mem.eql(u8, search_name, bound_name)) {
                        return &self.bindings[i];
                    }
                }
                return null;
            }

            pub inline fn create(comptime T: type) Set {
                comptime {
                    if (T == TextConfig) return TEXT_CONF_BIND_SET;
                    if (T == HeadlessElementConfig) return ELEM_DECL_BIND_SET;
                    std.debug.assert(T != ElementConfig);

                    const fields = std.meta.fieldNames(T);

                    var out = Set{ .count = fields.len };

                    for (fields, 0..) |field_name, i| {
                        out.names[i] = field_name;
                        out.bindings[i] = Binding.create(T, field_name);
                    }

                    return out;
                }
            }

            pub inline fn custom(comptime T: type, comptime Fset: anytype) Set {
                comptime {
                    var out = Set{ .count = Fset.len };

                    for (0..Fset.len) |i| {
                        const x = Fset[i];
                        if (@typeInfo(@TypeOf(x)) == .@"struct") {
                            out.names[i] = x[0];
                            out.bindings[i] = .custom(T, x[1]);
                        } else {
                            out.names[i] = x;
                            out.bindings[i] = .create(T, x);
                        }
                    }

                    return out;
                }
            }
        };

        pub inline fn create(comptime T: type, comptime F: []const u8) Binding {
            comptime return .{
                .size = @sizeOf(@FieldType(T, F)),
                .offset = @offsetOf(T, F),
            };
        }

        pub inline fn custom(comptime T: type, comptime Fs: []const u8) Binding {
            comptime {
                var X = T;
                var offset = 0;
                var it = std.mem.tokenizeScalar(u8, Fs, '.');
                while (it.next()) |field_name| {
                    offset += @offsetOf(X, field_name);
                    X = @FieldType(X, field_name);
                }
                return .{
                    .size = @sizeOf(X),
                    .offset = offset,
                };
            }
        }
    };
};

pub fn pushTheme(self: *Ui, theme: *const Theme) !void {
    try self.theme_stack.append(self.gpa, theme);
}

pub fn popTheme(self: *Ui) ?*const Theme {
    return self.theme_stack.pop();
}

pub fn applyThemeState(self: *Ui, binding_set: *const Theme.Binding.Set, kind: Theme.Kind, state: ActionState, out: *anyopaque) !void {
    self.theme_match_levels.clearRetainingCapacity();

    for (self.theme_stack.items) |theme| {
        try theme.apply(self.gpa, &self.theme_match_levels, binding_set, kind, state, out);
    }
}

pub fn applyTheme(self: *Ui, binding_set: *const Theme.Binding.Set, kind: Theme.Kind, out: *anyopaque) !void {
    return self.applyThemeState(binding_set, kind, self.getActionState(), out);
}

pub fn openElement(self: *Ui, id: ElementId) !void {
    try self._openElement(id);
}

pub fn configureElement(self: *Ui, decl: ElementDeclaration) !void {
    var config = HeadlessElementConfig{};

    try self.applyThemeState(
        &Theme.Binding.Set.ELEM_DECL_BIND_SET,
        if (decl.type == .content) .content else .widget,
        if (decl.state) |st| st else self.getActionState(),
        &config,
    );

    if (decl.state == .disabled) {
        try self.pushDisabled(true);
    } else {
        try self.pushDisabled(self.disabled());
    }

    decl.apply(&config);

    try self._configureElement(config);
}

pub fn beginElement(self: *Ui, id: ElementId, decl: ElementDeclaration) !void {
    try self._openElement(id);
    try self.configureElement(decl);
}

pub fn endElement(self: *Ui) void {
    self._endElement();
    self.popDisabled();
}

pub fn openSection(self: *Ui, id: ElementId) !void {
    try self._openElement(id);
}

pub fn configureSection(self: *Ui, decl: ElementDeclaration) !void {
    var config = HeadlessElementConfig{};

    decl.apply(&config);

    const SectionTheme = struct {
        child_gap: u16 = 0,
        const BINDING_SET = Theme.Binding.Set.create(@This());
    };

    var section_theme = SectionTheme{};
    try self.applyThemeState(
        &SectionTheme.BINDING_SET,
        if (decl.type == .content) .content else .widget,
        if (decl.state) |st| st else self.getActionState(),
        &section_theme,
    );
    config.layout.child_gap = section_theme.child_gap;

    if (decl.state == .disabled) {
        try self.pushDisabled(true);
    } else {
        try self.pushDisabled(self.disabled());
    }

    try self._configureElement(config);
}

pub fn beginSection(self: *Ui, id: ElementId, decl: ElementDeclaration) !void {
    try self._openElement(id);
    try self.configureSection(decl);
}

pub fn endSection(self: *Ui) void {
    self._endElement();
    self.popDisabled();
}

pub fn text(self: *Ui, content: []const u8, decl: TextDeclaration) !void {
    var config = TextConfig{};

    const parent_id: ElementId = if (self.open_ids.items.len > 1)
        self.open_ids.items[self.open_ids.items.len - 2]
    else
        return error.TextSectionMustBeInContentSection;
    const parent_info = self.frame_element_info.get(parent_id.id) orelse return error.Unexpected;

    try self.applyTheme(&Theme.Binding.Set.TEXT_CONF_BIND_SET, if (parent_info.widget) .widget else .content, &config);

    decl.apply(&config);

    return self._text(content, config);
}

pub fn getWidget(self: *Ui, comptime T: type, id: ElementId) ?*T {
    const widget = self.widget_states.get(id.id) orelse return null;
    return @ptrCast(@alignCast(widget.user_data));
}

pub fn getOrCreateWidget(self: *Ui, comptime T: type, id: ElementId) !struct { *T, bool } {
    const gop = try self.widget_states.getOrPut(self.gpa, id.id);
    if (!gop.found_existing) {
        const ptr = try self.gpa.create(T);
        gop.value_ptr.* = Ui.Widget{
            .user_data = ptr,
            .render = if (comptime std.meta.hasMethod(T, "render")) @ptrCast(&T.render) else null,
            .deinit = @ptrCast(&struct {
                pub fn deinit(x: *T, y: *Ui) void {
                    defer y.gpa.destroy(x);
                    if (comptime std.meta.hasMethod(T, "deinit")) x.deinit(y);
                }
            }.deinit),
            .seen_this_frame = true,
        };

        return .{ ptr, true };
    } else {
        gop.value_ptr.seen_this_frame = true;

        return .{ @ptrCast(@alignCast(gop.value_ptr.user_data)), false };
    }
}

pub const ElementDeclaration = struct {
    sizing: ?Sizing = null,
    padding_left: ?u16 = null,
    padding_right: ?u16 = null,
    padding_top: ?u16 = null,
    padding_bottom: ?u16 = null,
    child_gap: ?u16 = null,
    child_alignment: ?ChildAlignment = null,
    direction: ?LayoutDirection = null,
    background_color: ?Color = null,
    radius_top_left: ?f32 = null,
    radius_top_right: ?f32 = null,
    radius_bottom_left: ?f32 = null,
    radius_bottom_right: ?f32 = null,
    border_left: ?u16 = null,
    border_right: ?u16 = null,
    border_top: ?u16 = null,
    border_bottom: ?u16 = null,
    border_between_children: ?u16 = null,
    border_color: ?Color = null,
    state: ?ActionState = null,
    floating: FloatingElementConfig = .{},
    aspect_ratio: AspectRatioElementConfig = 0,
    type: ElementType = .content,
    clip: ClipElementConfig = .{},
    event_flags: ElementStateFlags.Flags = .{},
    userdata: ?*anyopaque = null,
    image: ImageElementConfig = null,

    pub fn apply(decl: *const ElementDeclaration, config: *HeadlessElementConfig) void {
        config.aspect_ratio = decl.aspect_ratio;
        config.image = decl.image;
        config.floating = decl.floating;
        config.type = decl.type;
        config.clip = decl.clip;
        config.state = .custom(if (decl.state != .disabled) decl.event_flags else .{}, decl.userdata);

        if (decl.sizing) |x| config.layout.sizing = x;
        if (decl.padding_left) |x| config.layout.padding.left = x;
        if (decl.padding_right) |x| config.layout.padding.right = x;
        if (decl.padding_top) |x| config.layout.padding.top = x;
        if (decl.padding_bottom) |x| config.layout.padding.bottom = x;
        if (decl.child_gap) |x| config.layout.child_gap = x;
        if (decl.child_alignment) |x| config.layout.child_alignment = x;
        if (decl.direction) |x| config.layout.direction = x;
        if (decl.background_color) |x| config.background_color = x;
        if (decl.radius_top_left) |x| config.corner_radius.top_left = x;
        if (decl.radius_top_right) |x| config.corner_radius.top_right = x;
        if (decl.radius_bottom_left) |x| config.corner_radius.bottom_left = x;
        if (decl.radius_bottom_right) |x| config.corner_radius.bottom_right = x;
        if (decl.border_left) |x| config.border.width.left = x;
        if (decl.border_right) |x| config.border.width.right = x;
        if (decl.border_top) |x| config.border.width.top = x;
        if (decl.border_bottom) |x| config.border.width.bottom = x;
        if (decl.border_between_children) |x| config.border.width.between_children = x;
        if (decl.border_color) |x| config.border.color = x;
    }
};

pub const TextDeclaration = struct {
    color: ?Color = null,
    font_id: ?AssetCache.FontId = null,
    font_size: ?u16 = null,
    letter_spacing: ?u16 = null,
    line_height: ?u16 = null,
    wrap_mode: ?TextElementConfigWrapMode = null,
    alignment: ?TextAlignment = null,
    state: ?ActionState = null,
    user_data: ?*anyopaque = null,

    pub fn apply(decl: *const TextDeclaration, config: *TextConfig) void {
        config.user_data = decl.user_data;

        if (decl.color) |x| config.color = x;
        if (decl.font_id) |x| config.font_id = x;
        if (decl.font_size) |x| config.font_size = x;
        if (decl.letter_spacing) |x| config.letter_spacing = x;
        if (decl.line_height) |x| config.line_height = x;
        if (decl.wrap_mode) |x| config.wrap_mode = x;
        if (decl.alignment) |x| config.alignment = x;
    }
};

pub const ElementType = enum {
    content,
    render_widget,
    layout_widget,
};

pub const ElementConfig = struct {
    id: ElementId = .{},

    /// Controls various settings that affect the size and position of an element, as well as the sizes and positions of any child elements.
    layout: LayoutConfig = .{},
    // Controls settings related to aspect ratio scaling.
    aspect_ratio: AspectRatioElementConfig = 0,
    /// Controls settings related to image elements.
    image: ImageElementConfig = null,
    /// Controls whether and how an element "floats", which means it layers over the top of other elements in z order, and doesn't affect the position and size of siblings or parent elements.
    /// Note: in order to activate floating, `.floating.attachTo` must be set to something other than the default value.
    floating: FloatingElementConfig = .{},
    /// Whether or not this element will be treated as a widget.
    type: ElementType = .content,
    /// Controls whether an element should clip its contents and allow scrolling rather than expanding to contain them.
    clip: ClipElementConfig = .{},
    /// A pointer that will be transparently passed through to resulting render command
    state: ElementStateFlags = .none,

    /// Controls the background color of the resulting element.
    /// By convention specified as 0-255, but interpretation is up to the renderer.
    /// If no other config is specified, `.background_color` will generate a `RECTANGLE` render command, otherwise it will be passed as a property to `IMAGE` or `CUSTOM` render commands.
    background_color: Color = .{},
    /// Controls the "radius", or corner rounding of elements, including rectangles, borders and images.
    corner_radius: CornerRadius = .{},
    /// Controls settings related to element borders, and will generate BORDER render command
    border: BorderElementConfig = .{},

    fn toClay(self: ElementConfig) clay.ElementDeclaration {
        return clay.ElementDeclaration{
            .id = self.id,
            .layout = self.layout,
            .background_color = colorToClay(self.background_color),
            .corner_radius = self.corner_radius,
            .aspect_ratio = .{ .aspect_ratio = self.aspect_ratio },
            .image = imageToClay(self.image),
            .floating = self.floating.toClay(),
            .custom = .{ .custom_data = @ptrFromInt(@intFromBool(self.type == .render_widget)) },
            .clip = self.clip.toClay(),
            .border = self.border.toClay(),
            .user_data = self.state.toClay(),
        };
    }
};

// --- Helper functions ---

fn vec2FromClay(vec: clay.Vector2) vec2 {
    return .{ vec.x, vec.y };
}

fn vec2ToClay(vec: vec2) clay.Vector2 {
    return .{ .x = vec[0], .y = vec[1] };
}

fn vec2FromDims(dims: clay.Dimensions) vec2 {
    return .{ dims.w, dims.h };
}

fn vec2ToDims(vec: vec2) clay.Dimensions {
    return .{ .w = vec[0], .h = vec[1] };
}

fn colorToClay(color: Batch2D.Color) clay.Color {
    return clay.Color{ color.r, color.g, color.b, color.a };
}

fn imageToClay(image: ImageElementConfig) clay.ImageElementConfig {
    return clay.ImageElementConfig{
        // must add one to avoid null pointer discard in clay
        .image_data = if (image) |id| @intCast(id + 1) else 0,
    };
}

fn decodeImageId(id: usize) AssetCache.ImageId {
    return @intCast(id - 1);
}

pub fn boxContains(box: BoundingBox, point: vec2) bool {
    return point[0] >= box.x and point[0] < box.x + box.width and point[1] >= box.y and point[1] < box.y + box.height;
}

pub fn boxesIntersect(a: BoundingBox, b: BoundingBox) bool {
    return a.x < b.x + b.width and a.x + a.width > b.x and a.y < b.y + b.height and a.y + a.height > b.y;
}

pub fn boxContainsBox(outer: BoundingBox, inner: BoundingBox) bool {
    return inner.x >= outer.x and inner.x + inner.width <= outer.x + outer.width and inner.y >= outer.y and inner.y + inner.height <= outer.y + outer.height;
}

pub fn clampToBox(box: BoundingBox, point: vec2) vec2 {
    return .{
        std.math.clamp(point[0], box.x, box.x + box.width),
        std.math.clamp(point[1], box.y, box.y + box.height),
    };
}

pub fn relativizeToBox(box: BoundingBox, point: vec2) vec2 {
    return .{
        point[0] - box.x,
        point[1] - box.y,
    };
}

/// Converts a Clay color ([4]f32, 0-255) to a Batch2D color (struct, 0.0-1.0).
/// As per Clay's convention, a color of {0,0,0,0} is treated as "no color" or "untinted",
/// which we map to white for image rendering.
pub fn clayColorToBatchColor(c: clay.Color) Batch2D.Color {
    // Clay convention: {0,0,0,0} means no tint, which is white in multiplicative blending.
    // zig fmt: off
    if (std.math.approxEqAbs(f32, c[0], 0, std.math.floatEps(f32))
    and std.math.approxEqAbs(f32, c[1], 0, std.math.floatEps(f32))
    and std.math.approxEqAbs(f32, c[2], 0, std.math.floatEps(f32))
    and std.math.approxEqAbs(f32, c[3], 0, std.math.floatEps(f32))) {
        return .white;
    }
    // zig fmt: on

    return .{
        .r = c[0],
        .g = c[1],
        .b = c[2],
        .a = c[3],
    };
}

// --- Backend implementation --- //

fn handleStateSetup(self: *Ui, declaration: ElementConfig) !void {
    // Capture element info for this frame.
    const parent_id: ?ElementId = if (self.open_ids.items.len > 1)
        self.open_ids.items[self.open_ids.items.len - 2]
    else
        null;

    try self.frame_element_info.put(self.gpa, declaration.id.id, .{
        .widget = declaration.type != .content,
        .state = declaration.state,
        .parent_id = parent_id,
    });

    // Add navigable elements to the map in the order they are declared
    if (declaration.state.event_flags.focus) {
        const index = self.navigable_elements.count();
        try self.navigable_elements.put(self.gpa, index, .{ .id = declaration.id, .state = declaration.state });
        try self.reverse_navigable_elements.put(self.gpa, declaration.id.id, index);
    }

    // If the element is hovered, push it to our stack of hovered elements.
    if (clay.hovered()) {
        const hovered_data = clay.getElementData(declaration.id);
        if (hovered_data.found) {
            try self.hovered_element_stack.append(self.gpa, .{
                .id = declaration.id,
                .bounding_box = hovered_data.bounding_box,
                .state = declaration.state,
            });
        }
    }

    // If the element supports scrolling, save its scroll state for the current frame, bound by its id.
    if (declaration.state.event_flags.scroll) {
        const offset = declaration.clip.child_offset;
        const scrolled_data = clay.getElementData(declaration.id);
        if (scrolled_data.found) {
            try self.current_scroll_states.put(self.gpa, declaration.id.id, .{
                .offset = offset,
                .elem_state = .{
                    .id = declaration.id,
                    .bounding_box = scrolled_data.bounding_box,
                    .state = declaration.state,
                },
            });
        }
    }
}

/// The error reporting function that Clay calls when it encounters an error.
/// This function is registered in `init`.
fn reportClayError(data: clay.ErrorData) callconv(.c) void {
    std.log.scoped(.clay).err("{s} - {s}", .{ @tagName(data.error_type), data.error_text.toSlice() });
    const self: *Ui = @ptrCast(@alignCast(data.user_data));
    if (self.open_ids.items.len > 0) {
        for (self.open_ids.items) |id| {
            std.log.scoped(.clay).err("{s}", .{id.string_id.toSlice()});
        }
    }
}

/// The callback function that Clay uses to measure text.
/// This function is registered in `init`.
fn measureTextCallback(
    text_slice: []const u8,
    config: *clay.TextElementConfig,
    backend_ptr: *Ui,
) clay.Dimensions {
    if (backend_ptr.asset_cache.fonts.items.len <= config.font_id) {
        log.err("Invalid font_id {d} used in text element.", .{config.font_id});
        return .{ .w = 0, .h = 0 };
    }

    const line_spacing_override = if (config.line_height > 0) config.line_height else null;

    // Use the Batch2D's own text measurement logic for perfect consistency.
    const dimensions = backend_ptr.renderer.measureText(
        text_slice,
        @intCast(config.font_id),
        config.font_size,
        line_spacing_override,
    );

    if (dimensions) |dims| {
        return vec2ToDims(dims);
    } else {
        return .{ .w = 0, .h = 0 };
    }
}

// Helper to determine if an element is a widget that should receive true focus.
// It now uses our internal frame_element_info map.
fn isWidgetNavigable(self: *Ui, id: ElementId) bool {
    const info = self.frame_element_info.get(id.id) orelse return false;
    return info.state.event_flags.focus;
}

/// Processes the current UI state against the last frame's state to generate interaction events.
fn generateEvents(self: *Ui) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

    self.events.clearRetainingCapacity();
    try self.events.appendSlice(self.gpa, self.widget_events.items);
    self.widget_events.clearRetainingCapacity();

    const mouse_pos = self.bindings.getMousePosition();
    const primary_mouse_action = self.bindings.getAction(.primary_mouse);
    const activate_action = self.bindings.getAction(.activate_focused);
    const focus_next_action = self.bindings.getAction(.focus_next);
    const focus_prev_action = self.bindings.getAction(.focus_prev);
    const close_scope_action = self.bindings.getAction(.close_focus_scope);

    const modifiers = self.bindings.input_state.getModifiers();

    const now = self.click_state.timer.read();

    // The top-most element in the stack is the one we consider "hovered" for this frame.
    if (self.hovered_element_stack.items.len > 0) {
        self.state.hovered = self.hovered_element_stack.items[self.hovered_element_stack.items.len - 1];
    }

    // --- Handle Overlay State Changes ---
    try self.handleOverlayState();

    // --- Handle Text Input Events ---
    // This is processed before activation to give it priority.
    var processed_text_input = false;
    if (self.state.focused_id) |focused_elem| {
        if (focused_elem.state.event_flags.text) {
            // Handle character input
            if (self.char_input.len > 0) {
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = focused_elem.id,
                        .bounding_box = focused_elem.bounding_box,
                        .user_data = focused_elem.state.getUserData(),
                    },
                    .data = .{ .text = .{ .chars = self.char_input } },
                });
                processed_text_input = true;
            }

            // Handle special command keys by accessing the raw input state.
            const delete_down = self.bindings.input_state.getKey(.delete).isDown();
            const backspace_down = self.bindings.input_state.getKey(.backspace).isDown();
            const enter_down = self.bindings.input_state.getKey(.enter).isDown();
            const l_arrow_down = self.bindings.input_state.getKey(.left).isDown();
            const r_arrow_down = self.bindings.input_state.getKey(.right).isDown();
            const u_arrow_down = self.bindings.input_state.getKey(.up).isDown();
            const d_arrow_down = self.bindings.input_state.getKey(.down).isDown();
            const a_down = self.bindings.input_state.getKey(.a).isDown();
            const home_down = self.bindings.input_state.getKey(.home).isDown();
            const end_down = self.bindings.input_state.getKey(.end).isDown();

            const copy_down = self.bindings.input_state.getKey(.c).isDown();
            const paste_down = self.bindings.input_state.getKey(.v).isDown();

            // zig fmt: off
            if (delete_down or backspace_down or enter_down or (modifiers.ctrl and (copy_down or paste_down))
            or l_arrow_down or r_arrow_down or u_arrow_down or d_arrow_down or home_down or end_down
            or (modifiers.ctrl and a_down)) special_text: {
            // zig fmt: on
                switch (self.text_repeat.state) {
                    .none => {},
                    .first => if (self.text_repeat.timer.read() < self.text_repeat.initial_delay) {
                        break :special_text;
                    },
                    .nth => if (self.text_repeat.timer.read() < self.text_repeat.nth_delay) {
                        break :special_text;
                    },
                }

                self.text_repeat.state.advance();

                processed_text_input = true;

                self.text_repeat.timer.reset();

                if (delete_down) {
                    try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .text = .{ .command = .{ .action = .delete, .modifiers = modifiers } } },
                    });
                } else if (backspace_down) {
                    try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .text = .{ .command = .{ .action = .backspace, .modifiers = modifiers } } },
                    });
                } else if (enter_down) {
                    try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .text = .{ .command = .{ .action = .newline, .modifiers = modifiers } } },
                    });
                } else if (copy_down) {
                    try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .text = .{ .command = .{ .action = .copy, .modifiers = modifiers } } },
                    });
                } else if (paste_down) {
                    try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .text = .{ .command = .{ .action = .paste, .modifiers = modifiers } } },
                    });
                } else if (l_arrow_down) {
                    try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .text = .{ .command = .{ .action = .move_left, .modifiers = modifiers } } },
                    });
                } else if (r_arrow_down) {
                    try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .text = .{ .command = .{ .action = .move_right, .modifiers = modifiers } } },
                    });
                } else if (u_arrow_down) {
                    try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .text = .{ .command = .{ .action = .move_up, .modifiers = modifiers } } },
                    });
                } else if (d_arrow_down) {
                    try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .text = .{ .command = .{ .action = .move_down, .modifiers = modifiers } } },
                    });
                } else if (home_down) {
                    try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .text = .{ .command = .{ .action = .home, .modifiers = modifiers } } },
                    });
                } else if (end_down) {
                    try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .text = .{ .command = .{ .action = .end, .modifiers = modifiers } } },
                    });
                } else if (a_down) {
                    try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .text = .{ .command = .{ .action = .select_all, .modifiers = modifiers } } },
                    });
                }
            } else {
                // No special event, so reset the repeat state.
                self.text_repeat.state = .none;
            }
        }
    }

    // --- Handle Keyboard Events ---
    if (self.state.focused_id) |focused_elem| {
        if (focused_elem.state.event_flags.keyboard) {
            inline for (comptime std.meta.fieldNames(BindingState.Key)) |field_name| {
                const key: BindingState.Key = @field(BindingState.Key, field_name);
                const key_state = self.bindings.input_state.getKey(key);

                switch (key_state) {
                    .none => {},
                    .pressed => try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .key_down = .{ .key = key, .modifiers = modifiers } },
                    }),
                    .held => try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .key = .{ .key = key, .modifiers = modifiers } },
                    }),
                    .released => try self.events.append(self.gpa, .{
                        .info = .{
                            .element_id = focused_elem.id,
                            .bounding_box = focused_elem.bounding_box,
                            .user_data = focused_elem.state.getUserData(),
                        },
                        .data = .{ .key_up = .{ .key = key, .modifiers = modifiers } },
                    }),
                }
            }
        }
    }

    // --- Handle Hover Events ---
    // This logic compares the hovered element from the last frame to the current one.
    if (self.last_state.hovered) |last_hovered| {
        if (last_hovered.state.event_flags.hover) {
            if (last_hovered.id.id != self.state.hoveredIdValue()) { // Mouse moved out of the previously hovered element.
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = last_hovered.id,
                        .bounding_box = last_hovered.bounding_box,
                        .user_data = last_hovered.state.getUserData(),
                    },
                    .data = .{ .hover_end = .{ .mouse_position = clampToBox(last_hovered.bounding_box, mouse_pos), .modifiers = modifiers } },
                });
            } else { // Mouse remains over the previously hovered element.
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = last_hovered.id,
                        .bounding_box = last_hovered.bounding_box,
                        .user_data = last_hovered.state.getUserData(),
                    },
                    .data = .{ .hovering = .{ .mouse_position = mouse_pos, .modifiers = modifiers } },
                });
            }
        }
    }

    if (self.state.hoveredIdValue() != self.last_state.hoveredIdValue()) {
        if (self.state.hovered) |new_hovered| { // A new element is now hovered.
            if (new_hovered.state.event_flags.hover) {
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = new_hovered.id,
                        .bounding_box = new_hovered.bounding_box,
                        .user_data = new_hovered.state.getUserData(),
                    },
                    .data = .{ .hover_begin = .{ .mouse_position = mouse_pos, .modifiers = modifiers } },
                });
            }
        }
    }

    // --- Handle Active State and Focus-on-click ---
    // Determine the current active element based on continuous input state. Mouse input takes precedence.
    if (primary_mouse_action == .pressed) {
        // Iterate through the hovered stack from top to bottom to find the correct event target.
        for (self.hovered_element_stack.items, 0..) |_, i| {
            const hovered_state = self.hovered_element_stack.items[self.hovered_element_stack.items.len - 1 - i];

            // The first clickable, draggable or activatable element we find becomes active.
            if (hovered_state.state.event_flags.click or hovered_state.state.event_flags.activate or hovered_state.state.event_flags.drag) {
                self.state.active_id = hovered_state;
                break;
            }
        }
    }

    // An interaction is mouse-driven if the button is down AND we have a locked-in active element.
    // This correctly handles continuation (.held) frames.
    const is_mouse_activating = primary_mouse_action.isDown() and self.state.active_id != null;
    // If mouse isn't activating, check keyboard.
    if (!is_mouse_activating) {
        // Only consider keyboard activation if we haven't just processed text input.
        // This prevents, for example, the space key from both typing a space and activating the element.
        if (activate_action.isDown() and self.state.focused_id != null and !processed_text_input) {
            if (self.state.focused_id.?.state.event_flags.activate) {
                self.state.active_id = self.state.focused_id.?;
            } else {
                self.state.active_id = null;
            }
        } else {
            // If the mouse is down on a draggable element, we are dragging, so don't clear the active id.
            var is_dragging = false;
            if (primary_mouse_action.isDown() and self.state.active_id != null) {
                const active_elem = self.state.active_id.?;
                is_dragging = active_elem.state.event_flags.drag;
            }

            if (!is_dragging) {
                self.state.active_id = null;
            }
        }
    }

    // Handle focus changes on mouse press
    if (primary_mouse_action == .pressed) {
        var new_focus_target: ?StateProperty = null;

        // 1. Search the hovered stack for a focusable element
        for (self.hovered_element_stack.items, 0..) |_, i| {
            const hovered_state = self.hovered_element_stack.items[self.hovered_element_stack.items.len - 1 - i];
            if (hovered_state.state.event_flags.focus) {
                new_focus_target = hovered_state;
                break;
            }
        }

        if (new_focus_target) |target| {
            // We clicked directly on a focusable element (or a visual child of one). Shift focus.
            self.state.focused_id = target;
        } else {
            // We clicked on something unfocusable.
            // Only clear focus if the click was OUTSIDE the currently focused element's logical hierarchy.
            var click_was_inside_current_focus = false;

            if (self.state.focused_id) |current_focus| {
                if (self.hovered_element_stack.items.len > 0) {
                    const top_hovered = self.hovered_element_stack.items[self.hovered_element_stack.items.len - 1];
                    var walker_id: ?ElementId = top_hovered.id;

                    // Walk up the lexical tree to see if the focused element is an ancestor
                    while (walker_id) |current_id| {
                        if (current_id.id == current_focus.id.id) {
                            click_was_inside_current_focus = true;
                            break;
                        }
                        const info = self.frame_element_info.get(current_id.id) orelse break;
                        walker_id = info.parent_id;
                    }
                }
            }

            if (!click_was_inside_current_focus) {
                self.state.focused_id = null;
            }
        }
    }

    // --- Generate Mouse Release Events ---
    if (primary_mouse_action == .released and self.last_state.active_id != null) {
        const last_active = self.last_state.active_id.?;

        // Always generate mouse_up if the element was clickable
        if (last_active.state.event_flags.click) {
            try self.events.append(self.gpa, .{
                .info = .{
                    .element_id = last_active.id,
                    .bounding_box = last_active.bounding_box,
                    .user_data = last_active.state.getUserData(),
                },
                .data = .{ .mouse_up = .{ .mouse_position = mouse_pos, .end_element = self.state.hoveredId(), .modifiers = modifiers } },
            });
        }

        if (self.click_state.drag_state.is_dragging) {
            // This was a drag, so generate drag_end and no click.
            if (last_active.state.event_flags.drag) {
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = last_active.id,
                        .bounding_box = last_active.bounding_box,
                        .user_data = last_active.state.getUserData(),
                    },
                    .data = .{ .drag_end = .{ .mouse_position = mouse_pos, .modifiers = modifiers } },
                });
            }
            self.click_state.drag_state.is_dragging = false;
            // A drag action resets any pending double-click.
            self.click_state.last_click_element_id = null;
        } else if (last_active.state.event_flags.click and last_active.id.id == self.state.hoveredIdValue()) {
            // Now, proceed with click/double-click event generation.
            try self.events.append(self.gpa, .{
                .info = .{
                    .element_id = last_active.id,
                    .bounding_box = last_active.bounding_box,
                    .user_data = last_active.state.getUserData(),
                },
                .data = .{ .clicked = .{ .mouse_position = mouse_pos, .modifiers = modifiers } },
            });

            const double_click_threshold_ns = self.click_state.double_click_threshold_ms * std.time.ns_per_ms;

            if (self.click_state.last_click_element_id == last_active.id.id and
                (now - self.click_state.last_click_time) <= double_click_threshold_ns)
            {
                // Double click registered
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = last_active.id,
                        .bounding_box = last_active.bounding_box,
                        .user_data = last_active.state.getUserData(),
                    },
                    .data = .{ .double_clicked = .{ .mouse_position = mouse_pos, .modifiers = modifiers } },
                });

                // Reset to prevent a third rapid click from triggering another double-click
                self.click_state.last_click_element_id = null;
            } else {
                // First click, record the state
                self.click_state.last_click_element_id = last_active.id.id;
                self.click_state.last_click_time = now;
            }
        } else {
            // Mouse was released on a different element or a non-clickable one; reset double-click tracking.
            self.click_state.last_click_element_id = null;
        }
    }

    // --- Generate Activation and Mouse Down Events from State Changes ---
    if (self.state.activeIdValue() != self.last_state.activeIdValue()) {
        if (self.last_state.active_id) |last_active| {
            if (last_active.state.event_flags.activate) {
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = last_active.id,
                        .bounding_box = last_active.bounding_box,
                        .user_data = last_active.state.getUserData(),
                    },
                    .data = .{ .activate_end = .{ .end_element = self.state.hoveredId(), .modifiers = modifiers } },
                });
            }
        }
        if (self.state.active_id) |new_active| {
            if (is_mouse_activating and new_active.state.event_flags.click) {
                log.debug("Generating mouse_down for element {any} with modifiers: {any}", .{ new_active.id, modifiers });
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = new_active.id,
                        .bounding_box = new_active.bounding_box,
                        .user_data = new_active.state.getUserData(),
                    },
                    .data = .{ .mouse_down = .{ .mouse_position = mouse_pos, .modifiers = modifiers } },
                });
            }
            // If the new active element is draggable, initialize the drag state.
            if (is_mouse_activating and new_active.state.event_flags.drag) {
                self.click_state.drag_state.is_dragging = false;
                self.click_state.drag_state.start_pos = mouse_pos;
                self.click_state.drag_state.start_time = now;
            }

            if (new_active.state.event_flags.activate) {
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = new_active.id,
                        .bounding_box = new_active.bounding_box,
                        .user_data = new_active.state.getUserData(),
                    },
                    .data = .activate_begin,
                });
            }
        }
    } else if (self.state.active_id) |active_elem| { // Element remains active from last frame.
        // Handle drag state progression
        if (primary_mouse_action.isDown() and active_elem.state.event_flags.drag) {
            if (!self.click_state.drag_state.is_dragging) {
                // Check if drag thresholds (time or distance) have been met.
                const elapsed_time = now - self.click_state.drag_state.start_time;
                const drag_time_threshold_ns = self.click_state.drag_time_threshold_ms * std.time.ns_per_ms;

                const dx = mouse_pos[0] - self.click_state.drag_state.start_pos[0];
                const dy = mouse_pos[1] - self.click_state.drag_state.start_pos[1];
                const dist_sq = dx * dx + dy * dy;
                const dist_threshold_sq = self.click_state.drag_dist_threshold * self.click_state.drag_dist_threshold;

                if (elapsed_time > drag_time_threshold_ns or dist_sq > dist_threshold_sq) {
                    self.click_state.drag_state.is_dragging = true;
                    try self.events.append(self.gpa, .{
                        .info = .{ .element_id = active_elem.id, .bounding_box = active_elem.bounding_box, .user_data = active_elem.state.getUserData() },
                        .data = .{ .drag_begin = .{ .mouse_position = mouse_pos, .modifiers = modifiers } },
                    });
                }
            }

            if (self.click_state.drag_state.is_dragging) {
                try self.events.append(self.gpa, .{
                    .info = .{ .element_id = active_elem.id, .bounding_box = active_elem.bounding_box, .user_data = active_elem.state.getUserData() },
                    .data = .{ .drag = .{ .mouse_position = mouse_pos, .modifiers = modifiers } },
                });
            }
        }

        // Handle 'activating' event for any held active element (mouse or keyboard).
        if (active_elem.state.event_flags.activate) {
            try self.events.append(self.gpa, .{
                .info = .{
                    .element_id = active_elem.id,
                    .bounding_box = active_elem.bounding_box,
                    .user_data = active_elem.state.getUserData(),
                },
                .data = .activating,
            });
        }
    }

    // If no mouse buttons are being interacted with, check if enough time has passed to invalidate a pending double-click.
    if (self.click_state.last_click_element_id != null and !primary_mouse_action.isDown()) {
        const double_click_threshold_ns = self.click_state.double_click_threshold_ms * std.time.ns_per_ms;
        if (now - self.click_state.last_click_time > double_click_threshold_ns) {
            self.click_state.last_click_element_id = null;
        }
    }

    // --- Handle Focus Scope Closing ---
    if (close_scope_action == .released) {
        if (self.focus_scope_stack.items.len > 0) {
            // A scope is active. Send the close event to the owner.
            const scope_owner_id = self.focus_scope_stack.items[self.focus_scope_stack.items.len - 1];

            const bounding_box = clay.getElementData(scope_owner_id).bounding_box;
            var user_data: ?*anyopaque = null;
            if (self.reverse_navigable_elements.get(scope_owner_id.id)) |nav_index| {
                if (self.navigable_elements.get(nav_index)) |nav_entry| {
                    user_data = nav_entry.state.getUserData();
                }
            }

            try self.events.append(self.gpa, .{
                .info = .{
                    .element_id = scope_owner_id,
                    .bounding_box = bounding_box,
                    .user_data = user_data,
                },
                .data = .scoped_focus_close,
            });
        } else {
            // No scope active. Clear global focus.
            if (self.state.focused_id != null) {
                self.state.focused_id = null;
            }
        }
    }

    // --- Handle Keyboard Focus Events ---
    if (self.focus_scope_stack.items.len > 0) {
        // A scope is active. Send a special event to that widget instead of doing global navigation.
        if (focus_next_action == .released or focus_prev_action == .released) {
            const scope_owner_id = self.focus_scope_stack.items[self.focus_scope_stack.items.len - 1];

            // The bounding box can be retrieved from Clay's post-layout data.
            // If the element wasn't laid out, this will be zero-initialized, which is fine.
            const bounding_box = clay.getElementData(scope_owner_id).bounding_box;

            // To get the user_data, we need to find the ElementState that was
            // declared during layout. The navigable_elements map is the best
            // place to look this up by ID. An element that can own a focus scope
            // must be focusable itself, and therefore will be in this map.
            var user_data: ?*anyopaque = null;
            if (self.reverse_navigable_elements.get(scope_owner_id.id)) |nav_index| {
                if (self.navigable_elements.get(nav_index)) |nav_entry| {
                    user_data = nav_entry.state.getUserData();
                }
            }

            // Prioritize 'prev' over 'next' in case of ambiguity from the binding system.
            const event_data: Event.Data = if (focus_prev_action == .released)
                .{ .scoped_focus_change = .prev }
            else
                .{ .scoped_focus_change = .next };

            try self.events.append(self.gpa, .{
                .info = .{
                    .element_id = scope_owner_id,
                    .bounding_box = bounding_box,
                    .user_data = user_data,
                },
                .data = event_data,
            });
        }
    } else if (self.navigable_elements.count() > 0) {
        // No scope active, perform global focus navigation as before.
        if (focus_prev_action == .released) {
            var current_index: ?u32 = null;
            if (self.state.focusedId()) |focused_id| {
                current_index = self.reverse_navigable_elements.get(focused_id.id);
            }

            const count = @as(u32, @intCast(self.navigable_elements.count()));
            const next_index: u32 = if (current_index) |idx| (idx + count - 1) % count else count - 1;

            const next = self.navigable_elements.get(next_index).?;
            const element_data = clay.getElementData(next.id);
            if (element_data.found) {
                self.state.focused_id = .{
                    .id = next.id,
                    .bounding_box = element_data.bounding_box,
                    .state = next.state,
                };
            }
        } else if (focus_next_action == .released) {
            var current_index: ?u32 = null;
            if (self.state.focusedId()) |focused_id| {
                current_index = self.reverse_navigable_elements.get(focused_id.id);
            }

            const count = @as(u32, @intCast(self.navigable_elements.count()));
            const next_index: u32 = if (current_index) |idx| (idx + 1) % count else 0;

            const next = self.navigable_elements.get(next_index).?;
            const element_data = clay.getElementData(next.id);
            if (element_data.found) {
                self.state.focused_id = .{
                    .id = next.id,
                    .bounding_box = element_data.bounding_box,
                    .state = next.state,
                };
            }
        }
    }

    // --- Generate Focus Change Events ---
    if (self.state.focusedIdValue() != self.last_state.focusedIdValue()) {
        if (self.last_state.focused_id) |last_focused| {
            try self.events.append(self.gpa, .{
                .info = .{
                    .element_id = last_focused.id,
                    .bounding_box = last_focused.bounding_box,
                    .user_data = last_focused.state.getUserData(),
                },
                .data = .focus_lost,
            });
        }
        if (self.state.focused_id) |new_focused| {
            try self.events.append(self.gpa, .{
                .info = .{
                    .element_id = new_focused.id,
                    .bounding_box = new_focused.bounding_box,
                    .user_data = new_focused.state.getUserData(),
                },
                .data = .focus_gained,
            });
        }
    }

    if (self.state.focused_id) |focused_elem| {
        try self.events.append(self.gpa, .{
            .info = .{
                .element_id = focused_elem.id,
                .bounding_box = focused_elem.bounding_box,
                .user_data = focused_elem.state.getUserData(),
            },
            .data = .focusing,
        });
    }

    // --- Handle Wheel Events ---
    if (@reduce(.Or, self.wheel_delta != vec2{ 0, 0 })) {
        // Iterate from the top-most hovered element downwards to find a wheel target.
        for (self.hovered_element_stack.items, 0..) |_, i| {
            const hovered_state = self.hovered_element_stack.items[self.hovered_element_stack.items.len - 1 - i];
            if (hovered_state.state.event_flags.wheel) {
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = hovered_state.id,
                        .bounding_box = hovered_state.bounding_box,
                        .user_data = hovered_state.state.getUserData(),
                    },
                    .data = .{ .wheel = .{ .delta = self.wheel_delta, .modifiers = modifiers } },
                });
                // Once a wheel container is found, stop propagating the event.
                break;
            }
        }
    }

    // Handle Scroll Events
    var scroll_it = self.current_scroll_states.iterator();
    while (scroll_it.next()) |entry| {
        const curr_state = entry.value_ptr.*;
        const last_state = self.last_scroll_states.get(entry.key_ptr.*) orelse continue;
        if (@reduce(.Or, curr_state.offset != last_state.offset)) {
            if (curr_state.elem_state.state.event_flags.scroll) {
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = curr_state.elem_state.id,
                        .bounding_box = curr_state.elem_state.bounding_box,
                        .user_data = curr_state.elem_state.state.getUserData(),
                    },
                    .data = .{ .scroll = .{ .old_offset = last_state.offset, .new_offset = curr_state.offset, .delta = curr_state.offset - last_state.offset } },
                });
            }
        }
    }
}

/// Processes the current array of RenderCommands from Clay and issues draw calls to Batch2D.
fn draw(self: *Ui) !void {
    const render_commands = self.render_commands orelse {
        log.debug("Ui.render called without any render commands (did you forget to call Ui.beginLayout/Ui.endLayout?)", .{});
        return;
    };

    var scissor_count: usize = 0;

    for (render_commands) |cmd| {
        const bb = cmd.bounding_box;
        const pos = vec2{ bb.x, bb.y };
        const size = vec2{ bb.width, bb.height };

        switch (cmd.command_type) {
            .none => {},
            .rectangle => {
                const data = cmd.render_data.rectangle;
                const color = clayColorToBatchColor(data.background_color);
                const radius = Batch2D.CornerRadius{
                    .top_left = data.corner_radius.top_left,
                    .top_right = data.corner_radius.top_right,
                    .bottom_right = data.corner_radius.bottom_right,
                    .bottom_left = data.corner_radius.bottom_left,
                };
                try self.renderer.drawRoundedRect(pos, size, radius, color);
            },
            .border => {
                const data = cmd.render_data.border;
                const color = clayColorToBatchColor(data.color);
                const r = Batch2D.CornerRadius{
                    .top_left = data.corner_radius.top_left,
                    .top_right = data.corner_radius.top_right,
                    .bottom_right = data.corner_radius.bottom_right,
                    .bottom_left = data.corner_radius.bottom_left,
                };
                const w = Batch2D.BorderWidth{
                    .top = @floatFromInt(data.width.top),
                    .right = @floatFromInt(data.width.right),
                    .bottom = @floatFromInt(data.width.bottom),
                    .left = @floatFromInt(data.width.left),
                };

                try self.renderer.drawRoundedRectLine(pos, size, r, w, color);
            },
            .text => {
                const data = cmd.render_data.text;
                if (self.asset_cache.fonts.items.len <= data.font_id) {
                    log.err("Invalid font_id {d} used in text element.", .{data.font_id});
                    continue;
                }
                const text_slice = data.string_contents.chars[0..@intCast(data.string_contents.length)];
                const color = clayColorToBatchColor(data.text_color);
                const line_spacing_override = if (data.line_height > 0) data.line_height else null;

                // Clay provides the final bounding box after alignment. We can just draw at its top-left corner.
                try self.renderer.drawText(
                    text_slice,
                    @intCast(data.font_id),
                    data.font_size,
                    line_spacing_override,
                    pos,
                    color,
                );
            },
            .image => {
                const data = cmd.render_data.image;
                const tint = clayColorToBatchColor(data.background_color);

                const image_id: AssetCache.ImageId = decodeImageId(data.image_data);

                try self.renderer.drawRoundedTexturedQuad(image_id, pos, size, .{
                    .top_left = data.corner_radius.top_left,
                    .top_right = data.corner_radius.top_right,
                    .bottom_right = data.corner_radius.bottom_right,
                    .bottom_left = data.corner_radius.bottom_left,
                }, null, tint);
            },
            .scissor_start => {
                try self.renderer.scissorStart(pos, size);
                scissor_count += 1;
            },
            .scissor_end => {
                if (scissor_count == 0) {
                    log.err("Received dangling scissorEnd command from clay", .{});
                    continue;
                }
                scissor_count -= 1;
                try self.renderer.scissorEnd();
            },
            .custom => {
                const widget = self.widget_states.get(cmd.id) orelse {
                    log.warn("No widget state found for custom render command with id {x}", .{cmd.id});
                    continue;
                };

                const f = widget.render orelse {
                    log.warn("Widget state with id {x} has no render function", .{cmd.id});
                    continue;
                };

                try f(widget.user_data, self, cmd);
            },
        }
    }
}
