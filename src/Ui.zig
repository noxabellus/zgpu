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
    state: ElementState,
    parent_id: ?ElementId,
};

pub const Widget = struct {
    user_data: *anyopaque,
    render: *const fn (*anyopaque, *Ui, RenderCommand) anyerror!void,
    unbind: *const fn (*anyopaque, *Ui) void,
    deinit: *const fn (*anyopaque, *Ui) void,
    get: *const fn (*anyopaque, *Ui) *const anyopaque,
    set: *const fn (*anyopaque, *Ui, *const anyopaque) void,
    state_type: *const anyopaque,
    seen_this_frame: bool,

    pub const Checkbox = @import("widgets/Checkbox.zig");
    pub const Slider = @import("widgets/Slider.zig");
    pub const TextInput = @import("widgets/TextInput.zig");
    pub const RadioButton = @import("widgets/RadioButton.zig");
    pub const Dropdown = @import("widgets/Dropdown.zig");

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

event_dispatch: EventDispatch = .empty,

user_data: ?*anyopaque = null,

open_layout: bool = false,

// A stack of all elements currently under the mouse, populated during layout. The last element is the top-most.
hovered_element_stack: std.ArrayList(StateElement) = .empty,
// Map from navigation index to element ID for quick lookup during keyboard navigation
navigable_elements: std.AutoHashMapUnmanaged(u32, struct { id: ElementId, state: ElementState }) = .empty,
// Map from ElementId.id to navigation index for reverse lookup during event generation
reverse_navigable_elements: std.AutoHashMapUnmanaged(u32, u32) = .empty,
// Stack of currently open element's user-provided stable IDs
open_ids: std.ArrayList(ElementId) = .empty,

// Scroll states for the current frame
current_scroll_states: std.AutoHashMapUnmanaged(u32, ScrollState) = .empty,
// Scroll states from the last frame
last_scroll_states: std.AutoHashMapUnmanaged(u32, ScrollState) = .empty,

// States for rich-interaction widgets
widget_states: std.AutoHashMapUnmanaged(u32, Widget) = .empty,

// Deferred state changes for widgets
deferred_widget_states_current: std.AutoHashMapUnmanaged(u32, *anyopaque) = .empty,
deferred_widget_arena_current: std.heap.ArenaAllocator,

deferred_widget_states_last: std.AutoHashMapUnmanaged(u32, *anyopaque) = .empty,
deferred_widget_arena_last: std.heap.ArenaAllocator,

// events from widgets triggered this frame, to be dispatched on the next frame
widget_events: std.ArrayList(Event) = .empty,

// shared states for widgets like radio groups
shared_widget_states: std.AutoHashMapUnmanaged(u32, SharedWidgetState) = .empty,

// --- Menu System State ---
menu_state: MenuState = .{},
// A map from a menu item's ID to the ID of the submenu it opens.
// This is rebuilt every frame during layout.
menu_item_to_submenu_map: std.AutoHashMapUnmanaged(u32, ElementId) = .empty,

// --- Interaction State ---
focus_scope_stack: std.ArrayList(ElementId) = .empty,

// Stores information about every element declared in the current frame.
frame_element_info: std.AutoHashMapUnmanaged(u32, FrameElementInfo) = .empty,

state: State = .{},
last_state: State = .{},
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
            .user_data = null,
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
        .deferred_widget_arena_current = std.heap.ArenaAllocator.init(gpa),
        .deferred_widget_arena_last = std.heap.ArenaAllocator.init(gpa),
    };

    self.click_state.timer = try .start();
    self.text_repeat.timer = try .start();
    self.menu_state.hover_timer = try .start();

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
    self.hovered_element_stack.deinit(self.gpa);
    self.navigable_elements.deinit(self.gpa);
    self.reverse_navigable_elements.deinit(self.gpa);
    self.open_ids.deinit(self.gpa);
    self.event_dispatch.deinit(self.gpa);
    self.current_scroll_states.deinit(self.gpa);
    self.last_scroll_states.deinit(self.gpa);
    self.focus_scope_stack.deinit(self.gpa);
    self.frame_element_info.deinit(self.gpa);
    self.menu_state.deinit(self.gpa);
    self.menu_item_to_submenu_map.deinit(self.gpa);

    self.widget_states.deinit(self.gpa);
    self.deferred_widget_states_current.deinit(self.gpa);
    self.deferred_widget_arena_current.deinit();
    self.deferred_widget_states_last.deinit(self.gpa);
    self.deferred_widget_arena_last.deinit();
    self.widget_events.deinit(self.gpa);

    self.gpa.free(self.clay_memory);
    self.gpa.destroy(self);
}

/// Call this at any time in the update stage to begin declaring the ui layout. Caller must ensure `Ui.endLayout` is called before the next `Ui.beginLayout` and/or `Ui.render`.
/// Note: it is acceptable to run the layout code multiple times per frame, but all state will be discarded when calling this function.
pub fn beginLayout(self: *Ui, dimensions: vec2, delta_ms: f32) !void {
    self.render_commands = null;

    self.events.clearRetainingCapacity();
    try self.events.appendSlice(self.gpa, self.widget_events.items);
    self.widget_events.clearRetainingCapacity();

    self.hovered_element_stack.clearRetainingCapacity();
    self.navigable_elements.clearRetainingCapacity();
    self.reverse_navigable_elements.clearRetainingCapacity();
    self.open_ids.clearRetainingCapacity();
    self.frame_element_info.clearRetainingCapacity();
    self.menu_state.hovered_submenu_candidate = null;
    // This map is declarative and needs to be rebuilt each frame.
    self.menu_item_to_submenu_map.clearRetainingCapacity();

    self.last_state = self.state; // Save the last frame's state for event generation
    self.state = .{}; // Reset current state; will be populated during layout and event generation
    // The active and focused elements persist unless an event changes them.
    self.state.active_id = self.last_state.active_id;
    self.state.focused_id = self.last_state.focused_id;

    self.wheel_delta = self.bindings.consumeScrollDelta();
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

    // Although we inform Clay of current mouse button state, it is only used in debug mode or for drag scrolling.
    clay.setPointerState(vec2ToClay(self.bindings.getMousePosition()), self.bindings.getAction(.primary_mouse).isDown());

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
            removed_entry.value.unbind(removed_entry.value.user_data, self);
            removed_entry.value.deinit(removed_entry.value.user_data, self);
        }
    }

    const old_states = self.deferred_widget_states_last;
    const old_arena = self.deferred_widget_arena_last;

    self.deferred_widget_states_last = self.deferred_widget_states_current;
    self.deferred_widget_arena_last = self.deferred_widget_arena_current;

    self.deferred_widget_states_current = old_states;
    self.deferred_widget_arena_current = old_arena;

    self.deferred_widget_states_current.clearRetainingCapacity();
    _ = self.deferred_widget_arena_current.reset(.retain_capacity);

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

    self.open_layout = false;

    try self.generateEvents();
}

/// Iterates through the generated events and calls any registered listeners.
pub fn dispatchEvents(self: *Ui) !void {
    std.debug.assert(!self.open_layout);

    clay.setCurrentContext(self.clay_context);
    defer clay.setCurrentContext(null);

    for (self.events.items) |event| {
        const event_tag: Event.Tag = event.data;
        const key = EventDispatch.Key{ .element_id = event.info.element_id.id, .event_tag = event_tag };
        log.debug("Attempting to dispatch event {s} for element {any}", .{ @tagName(event_tag), event.info.element_id });

        if (self.event_dispatch.listeners.get(key)) |listener_list| {
            log.debug("Got {d} listeners", .{listener_list.items.len});
            // This inline for loop will generate a compile-time branch for each possible event type,
            // ensuring that the payload is correctly typed when calling the listener function.
            inline for (comptime std.meta.fieldNames(Event.Tag)) |field_name| inline_loop: {
                const comptime_tag = comptime @field(Event.Tag, field_name);
                if (event_tag == comptime_tag) {
                    const payload = @field(event.data, field_name);

                    for (listener_list.items) |*listener| {
                        const typed_fn = @as(*const EventDispatch.Handler(anyopaque, comptime_tag), @ptrCast(@alignCast(listener.func)));
                        log.debug("dispatching event {s} to element {any}", .{ @tagName(comptime_tag), event.info.element_id });
                        try typed_fn(
                            listener.user_data,
                            self,
                            event.info,
                            payload,
                        );
                        log.debug("dispatched successfully", .{});
                    }

                    break :inline_loop;
                }
            }
        } else {
            log.debug("No listeners for this event on this element", .{});
        }
    }
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

/// Registers a function to be called when a specific event occurs on a given element.
pub fn addListener(
    self: *Ui,
    element_id: ElementId,
    comptime event_tag: Event.Tag,
    comptime T: type,
    listener_fn: *const EventDispatch.Handler(T, event_tag),
    user_data: *T,
) !void {
    const key = EventDispatch.Key{ .element_id = element_id.id, .event_tag = event_tag };

    var gop = try self.event_dispatch.listeners.getOrPut(self.gpa, key);
    if (!gop.found_existing) {
        // Initialize the ArrayList if this is a new key.
        gop.value_ptr.* = .empty;
    }

    try gop.value_ptr.append(self.gpa, .{
        .func = listener_fn,
        .user_data = user_data,
    });
}

/// Removes the registered listener that uses the given function pointer for a specific event on a given element.
pub fn removeListener(
    self: *Ui,
    element_id: ElementId,
    comptime event_tag: Event.Tag,
    comptime T: type,
    listener_fn: *const EventDispatch.Handler(T, event_tag),
) void {
    const key = EventDispatch.Key{ .element_id = element_id.id, .event_tag = event_tag };
    if (self.event_dispatch.listeners.getPtr(key)) |listener_list| {
        var i = listener_list.items.len;
        while (i > 0) {
            i -= 1;
            if (listener_list.items[i].func == @as(*const anyopaque, @ptrCast(listener_fn))) {
                _ = listener_list.orderedRemove(i);
            }
        }
        // If the list is now empty, we can remove the key from the hash map to save memory.
        if (listener_list.items.len == 0) {
            listener_list.deinit(self.gpa);
            const success = self.event_dispatch.listeners.remove(key);
            std.debug.assert(success);
        }
    }
}

// --- Element Interface ---

/// Create a new, unconfigured element.
/// * Must be followed by calls to `Ui.configureElement` and `Ui.closeElement`.
/// * See also `Ui.openElement`, `Ui.elem`.
pub fn beginElement(self: *Ui, id: ElementId) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

    try self.open_ids.append(self.gpa, id);

    clay.beginElement();
}

/// Configure the current element opened with `Ui.beginElement`.
/// * See also `Ui.openElement`, `Ui.elem`.
pub fn configureElement(self: *Ui, declaration: HeadlessElementDeclaration) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

    const id = self.open_ids.items[self.open_ids.items.len - 1];
    const full = attachHead(ElementDeclaration, declaration, id);
    clay.configureElement(full.toClay());

    try self.handleStateSetup(full);
}

/// Create a new element with the given declaration.
/// * Must be followed by a call to `Ui.closeElement`.
/// * Note that functions like `Ui.hovered` and `Ui.scrollOffset` will not work inside the passed declaration; use `Ui.beginElement` and `Ui.configureElement` instead.
/// * See also `Ui.elem`.
pub fn openElement(self: *Ui, declaration: ElementDeclaration) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

    try self.open_ids.append(self.gpa, declaration.id);

    clay.beginElement();
    clay.configureElement(declaration.toClay());

    try self.handleStateSetup(declaration);
}

/// Close the current element opened with `Ui.beginElement` or `Ui.openElement`.
/// * Note that if you opened with `Ui.openElement`, you should also call `Ui.configureElement` before this function.
/// * See also `Ui.elem`.
pub fn closeElement(self: *Ui) void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

    _ = self.open_ids.pop().?;

    clay.closeElement();
}

/// Create a new element with the given declaration, and immediately close it.
/// * Note that functions like `Ui.hovered` and `Ui.scrollOffset` will not work inside this declaration; use `Ui.beginElement`, `Ui.configureElement` and `Ui.closeElement` instead.
pub fn elem(self: *Ui, declaration: ElementDeclaration) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

    try self.openElement(declaration);
    self.closeElement();
}

/// Create a new text element with the given string and configuration.
/// * This element type cannot have children, so there is no need to call `Ui.closeElement`.
/// * This is not intended to work with `Ui.hovered` or `Ui.scrollOffset`; text elements should remain "dumb".
/// * This cannot be a top-level element.
pub fn text(self: *Ui, str: []const u8, config: TextElementConfig) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    std.debug.assert(self.open_ids.items.len > 0);

    clay.text(str, config.toClay());
}

/// Determine if the currently-open element is hovered by the mouse.
/// * This is for styling logic, not event handling; use the generated event stream for that.
pub fn hovered(self: *Ui) bool {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    std.debug.assert(self.open_ids.items.len > 0);

    return clay.hovered();
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

// Widget api //

/// Configure an open element as a checkbox widget for boolean values.
pub fn bindCheckbox(self: *Ui, config: Widget.Checkbox.Config) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    std.debug.assert(self.open_ids.items.len > 0);

    const id = self.open_ids.items[self.open_ids.items.len - 1];
    const gop = try self.widget_states.getOrPut(self.gpa, id.id);
    const widget = if (!gop.found_existing) create_new: {
        const ptr = try Widget.Checkbox.init(self, id, config);
        gop.value_ptr.* = Widget{
            .user_data = ptr,
            .get = @ptrCast(&Widget.Checkbox.onGet),
            .set = @ptrCast(&Widget.Checkbox.onSet),
            .state_type = @typeName(bool),
            .render = @ptrCast(&Widget.Checkbox.render),
            .unbind = @ptrCast(&Widget.Checkbox.unbindEvents),
            .deinit = @ptrCast(&Widget.Checkbox.deinit),
            .seen_this_frame = true,
        };

        try ptr.bindEvents(self);

        break :create_new ptr;
    } else reuse_existing: {
        gop.value_ptr.seen_this_frame = true;

        const ptr: *Widget.Checkbox = @ptrCast(@alignCast(gop.value_ptr.user_data));
        // Update config properties in case they change frame-to-frame.
        ptr.box_color = config.box_color;
        ptr.check_color = config.check_color;
        ptr.size = config.size;

        break :reuse_existing ptr;
    };

    if (self.deferred_widget_states_last.get(id.id)) |deferred_state| {
        try widget.onSet(self, @ptrCast(@alignCast(deferred_state)));
    }
}

/// Configure an open element as a dropdown selection widget for enum values.
pub fn bindDropdown(self: *Ui, comptime T: type, config: Widget.Dropdown.For(T).Config) !void {
    const Dropdown = Widget.Dropdown.For(T);

    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    std.debug.assert(self.open_ids.items.len > 0);

    const id = self.open_ids.items[self.open_ids.items.len - 1];
    const gop = try self.widget_states.getOrPut(self.gpa, id.id);
    const widget = if (!gop.found_existing) create_new: {
        const ptr = try Dropdown.init(self, id, config);
        gop.value_ptr.* = Widget{
            .user_data = ptr,
            .get = @ptrCast(&Dropdown.onGet),
            .set = @ptrCast(&Dropdown.onSet),
            .state_type = @typeName(T),
            .render = @ptrCast(&Dropdown.render),
            .unbind = @ptrCast(&Dropdown.unbindEvents),
            .deinit = @ptrCast(&Dropdown.deinit),
            .seen_this_frame = true,
        };

        try ptr.bindEvents(self);

        break :create_new ptr;
    } else reuse_existing: {
        gop.value_ptr.seen_this_frame = true;

        const ptr: *Dropdown = @ptrCast(@alignCast(gop.value_ptr.user_data));

        // Update config properties in case they change frame-to-frame.
        ptr.box_color = config.box_color;
        ptr.box_color_hover = config.box_color_hover;
        ptr.text_color = config.text_color;
        ptr.font_id = config.font_id;
        ptr.font_size = config.font_size;
        ptr.panel_color = config.panel_color;
        ptr.option_color_hover = config.option_color_hover;

        break :reuse_existing ptr;
    };

    if (self.deferred_widget_states_last.get(id.id)) |deferred_state| {
        try widget.onSet(self, @ptrCast(@alignCast(deferred_state)));
    }

    // Crucially, the widget declares its own elements, including the floating panel if open.
    try widget.declare(self);
}

/// Configure an open element as a radio button widget for enum values.
/// All radio buttons sharing the same `group_id` in the config will be linked.
pub fn bindRadioButton(self: *Ui, comptime T: type, config: Widget.RadioButton.For(T).Config) !void {
    const RadioButton = Widget.RadioButton.For(T);

    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    std.debug.assert(self.open_ids.items.len > 0);

    const id = self.open_ids.items[self.open_ids.items.len - 1];
    const gop = try self.widget_states.getOrPut(self.gpa, id.id);
    if (!gop.found_existing) {
        const ptr = try RadioButton.init(self, id, config);
        gop.value_ptr.* = Widget{
            .user_data = ptr,
            .get = @ptrCast(&RadioButton.onGet),
            .set = @ptrCast(&RadioButton.onSet),
            .state_type = @typeName(T),
            .render = @ptrCast(&RadioButton.render),
            .unbind = @ptrCast(&RadioButton.unbindEvents),
            .deinit = @ptrCast(&RadioButton.deinit),
            .seen_this_frame = true,
        };

        try ptr.bindEvents(self);
    } else {
        gop.value_ptr.seen_this_frame = true;

        const ptr: *RadioButton = @ptrCast(@alignCast(gop.value_ptr.user_data));

        // Update config properties in case they change frame-to-frame.
        ptr.circle_color = config.circle_color;
        ptr.dot_color = config.dot_color;
        ptr.size = config.size;
    }

    // We must "see" the shared state during the layout phase to prevent it
    // from being garbage collected by endLayout(). We don't need to do anything
    // with the returned pointer here; just calling the function is enough.
    const default_value = comptime @field(T, std.meta.fieldNames(T)[0]);
    _ = try self.getOrPutSharedWidgetState(T, config.group_id, default_value);
}

/// Configure an open element as a slider widget; works with floats and integers of all signs and sizes up to 64 bits.
pub fn bindSlider(self: *Ui, comptime T: type, config: Widget.Slider.For(T).Config) !void {
    const Slider = Widget.Slider.For(T);

    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    std.debug.assert(self.open_ids.items.len > 0);

    const id = self.open_ids.items[self.open_ids.items.len - 1];
    const gop = try self.widget_states.getOrPut(self.gpa, id.id);
    const widget = if (!gop.found_existing) create_new: {
        const ptr = try Slider.init(self, id, config);
        gop.value_ptr.* = Widget{
            .user_data = ptr,
            .get = @ptrCast(&Slider.onGet),
            .set = @ptrCast(&Slider.onSet),
            .state_type = @typeName(T),
            .render = @ptrCast(&Slider.render),
            .unbind = @ptrCast(&Slider.unbindEvents),
            .deinit = @ptrCast(&Slider.deinit),
            .seen_this_frame = true,
        };

        try ptr.bindEvents(self);

        break :create_new ptr;
    } else reuse_existing: {
        gop.value_ptr.seen_this_frame = true;

        const ptr: *Slider = @ptrCast(@alignCast(gop.value_ptr.user_data));

        // Update config properties in case they change frame-to-frame.
        ptr.min = config.min;
        ptr.max = config.max;
        ptr.track_color = config.track_color;
        ptr.handle_color = config.handle_color;
        ptr.handle_size = config.handle_size;

        break :reuse_existing ptr;
    };

    if (self.deferred_widget_states_last.get(id.id)) |deferred_state| {
        try widget.onSet(self, @ptrCast(@alignCast(deferred_state)));
    }
}

/// Configure an open element as a text input widget
pub fn bindTextInput(self: *Ui, config: Widget.TextInput.Config) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    std.debug.assert(self.open_ids.items.len > 0);

    const id = self.open_ids.items[self.open_ids.items.len - 1];
    const gop = try self.widget_states.getOrPut(self.gpa, id.id);
    const widget = if (!gop.found_existing) create_new: {
        const ptr = try Widget.TextInput.init(self, id, config);
        gop.value_ptr.* = Widget{
            .user_data = ptr,
            .render = @ptrCast(&Widget.TextInput.render),
            .get = @ptrCast(&Widget.TextInput.onGet),
            .set = @ptrCast(&Widget.TextInput.onSet),
            .state_type = @typeName([]const u8),
            .unbind = @ptrCast(&Widget.TextInput.unbindEvents),
            .deinit = @ptrCast(&Widget.TextInput.deinit),
            .seen_this_frame = true,
        };

        try ptr.bindEvents(self);

        break :create_new ptr;
    } else reuse_existing: {
        gop.value_ptr.seen_this_frame = true;
        const ptr: *Widget.TextInput = @ptrCast(@alignCast(gop.value_ptr.user_data));
        try ptr.swapBuffers(self);
        break :reuse_existing ptr;
    };

    if (self.deferred_widget_states_last.get(id.id)) |deferred_state| {
        try widget.onSet(self, @ptrCast(@alignCast(deferred_state)));
    }

    try self.text(widget.currentText(), config.toFull());
}

// --- Menu Structures ---

pub const MenuItemConfig = struct {
    font_id: FontId = 0,
    font_size: u16 = 14,
    height: f32 = 24.0,
    padding: Padding = .{ .left = 8, .right = 8, .top = 0, .bottom = 0 },
    corner_radius: CornerRadius = .all(3),
    text_color: Color = black,
    text_color_highlighted: Color = white,
    background_color: Color = transparent,
    background_color_highlighted: Color = Color.fromLinearU8(0, 120, 215, 255),
};

pub const MenuConfig = struct {
    background_color: Color = Color.fromLinearU8(240, 240, 240, 255),
    border_color: Color = Color.fromLinearU8(128, 128, 128, 255),
    border_width: BorderWidth = .all(1),
    corner_radius: CornerRadius = .all(4),
    padding: Padding = .all(4),
};

// --- Menu API ---

pub fn openMenu(self: *Ui, id: ElementId, config: MenuState.OpenMenuConfig) void {
    self.menu_state.open_request = .{
        .id = id,
        .parent_menu_id = config.parent_menu_id,
        .parent_item_id = config.parent_item_id,
        .position = config.position,
    };
}

pub fn closeTopMenu(self: *Ui) void {
    if (self.menu_state.stack.items.len > 0) {
        self.menu_state.close_request_level = self.menu_state.stack.items.len - 1;
    }
}

pub fn closeAllMenus(self: *Ui) void {
    if (self.menu_state.stack.items.len > 0) {
        self.menu_state.close_request_level = 0;
    }
}

pub fn beginMenu(self: *Ui, id: ElementId, config: MenuConfig) !bool {
    var is_open = false;
    var maybe_current_level: ?usize = null;
    var maybe_info: ?MenuState.OpenMenuInfo = null;

    for (self.menu_state.stack.items, 0..) |menu_info, i| {
        if (menu_info.id.id == id.id) {
            is_open = true;
            maybe_current_level = i;
            maybe_info = menu_info;
            break;
        }
    }

    if (!is_open) {
        return false;
    }

    const current_level = maybe_current_level.?;
    const info = maybe_info.?;

    const final_offset = info.position;
    var attach_to_element = false;

    if (info.parent_item_id) |item_id| {
        // This is a submenu. We need to find its trigger item's position.
        const item_data = clay.getElementData(item_id);
        if (item_data.found) {
            // The position should be relative to the viewport, not the parent menu.
            // Clay handles attaching to an element correctly if we provide the
            // element's ID and use attach points. The offset can be zero.
            attach_to_element = true;
        }
    }

    try self.openElement(.{
        .id = id,
        .layout = .{
            .sizing = .{ .w = .fit, .h = .fit },
            .direction = .top_to_bottom,
            .padding = config.padding,
        },
        .floating = .{
            // If it's a submenu, attach to the ITEM. If not, attach to the root.
            .attach_to = if (attach_to_element) .element_with_id else .root,
            // The parentId for attachment is the ITEM's ID.
            .parentId = (info.parent_item_id orelse ElementId{}).id,
            // If it's a root menu (context menu), use the specified position.
            .offset = if (attach_to_element) .{ 0, 0 } else final_offset,
            // For submenus, attach our top-left to the parent item's top-right.
            .attach_points = .{ .parent = if (attach_to_element) .right_top else .left_top, .element = .left_top },
            .z_index = @intCast(MenuState.MENU_Z_INDEX_BASE + current_level),
        },
        .background_color = config.background_color,
        .border = .{ .width = config.border_width, .color = config.border_color },
        .corner_radius = config.corner_radius,
        .state = .flags(.{ .focus = true, .keyboard = true, .activate = true }),
    });

    // Get (or create) and clear the navigable item list for this specific menu.
    var gop = try self.menu_state.navigable_items.getOrPut(self.gpa, id.id);
    if (!gop.found_existing) {
        gop.value_ptr.* = .empty;
    }
    gop.value_ptr.clearRetainingCapacity();

    return true;
}

pub fn endMenu(self: *Ui) void {
    const menu_id = self.open_ids.items[self.open_ids.items.len - 1];
    self.closeElement();

    // If this menu was just opened, automatically highlight its first item for better keyboard UX.
    if (self.menu_state.just_opened_menu_id == menu_id.id) {
        if (self.menu_state.navigable_items.get(menu_id.id)) |list| {
            if (list.items.len > 0) {
                // Find the menu's level in the stack to set the highlight correctly.
                for (self.menu_state.stack.items, 0..) |menu_info, level| {
                    if (menu_info.id.id == menu_id.id) {
                        self.menu_state.setHighlighted(self.gpa, level, list.items[0]);
                        break;
                    }
                }
            }
        }
    }
}

/// Creates a menu item. Application logic should be handled by listening for the `.activate_end` or `.clicked` event on the provided `id`.
pub fn menuItem(self: *Ui, id: ElementId, label: []const u8, config: MenuItemConfig) !void {
    // Get the current menu's ID from the open_ids stack.
    const menu_id = self.open_ids.items[self.open_ids.items.len - 1];
    // Add this item to its parent menu's navigable list.
    if (self.menu_state.navigable_items.getPtr(menu_id.id)) |list| {
        try list.append(self.gpa, id);
    }

    const level = self.menu_state.stack.items.len - 1;
    const is_highlighted = self.menu_state.highlighted_path.items.len > level and self.menu_state.highlighted_path.items[level].id == id.id;

    try self.openElement(.{
        .id = id,
        .layout = .{
            .sizing = .{ .w = .grow, .h = .fixed(config.height) },
            .padding = config.padding,
            .child_alignment = .center,
        },
        .background_color = if (is_highlighted) config.background_color_highlighted else config.background_color,
        .corner_radius = config.corner_radius,
        .state = .flags(.{ .activate = true, .hover = true }),
    });
    defer self.closeElement();

    if (self.hovered()) {
        self.menu_state.setHighlighted(self.gpa, level, id);
        // If we hover a menu item, it should take precedence over any focused widget inside the menu.
        if (self.state.focused_id != null) {
            // A more complex check could verify the focused element is a child of the current menu,
            // but for now, simply clearing focus when hovering any menu item is effective and intuitive.
            self.state.focused_id = null;
        }
    }

    try self.text(label, .{
        .font_id = config.font_id,
        .font_size = config.font_size,
        .color = if (is_highlighted) config.text_color_highlighted else config.text_color,
    });
}

/// Create a link to a submenu inside the currently open menu.
/// * It is not necessary to render the submenu directly, instead simply call `Ui.beginMenu` separately, with the same child menu ID provided here.
/// * The return value indicating whether the menu is open is for styling the current menu.
pub fn subMenu(self: *Ui, self_id: ElementId, label: []const u8, child_menu_id: ElementId, config: MenuItemConfig) !bool {
    const parent_menu_id = self.open_ids.items[self.open_ids.items.len - 1];
    // Add this item to its parent menu's navigable list.
    if (self.menu_state.navigable_items.getPtr(parent_menu_id.id)) |list| {
        try list.append(self.gpa, self_id);
    }

    // Register the relationship between this item and the submenu it opens.
    try self.menu_item_to_submenu_map.put(self.gpa, self_id.id, child_menu_id);

    const level = self.menu_state.stack.items.len - 1;
    const is_highlighted = self.menu_state.highlighted_path.items.len > level and self.menu_state.highlighted_path.items[level].id == self_id.id;

    var is_child_open = false;
    if (self.menu_state.stack.items.len > level + 1) {
        if (self.menu_state.stack.items[level + 1].parent_item_id) |parent_item| {
            if (parent_item.id == self_id.id) {
                is_child_open = true;
            }
        }
    }

    try self.openElement(.{
        .id = self_id,
        .layout = .{
            .sizing = .{ .w = .grow, .h = .fixed(config.height) },
            .direction = .left_to_right,
            .child_alignment = .center,
            .child_gap = 10,
            .padding = config.padding,
        },
        .background_color = if (is_highlighted or is_child_open) config.background_color_highlighted else config.background_color,
        .corner_radius = config.corner_radius,
        .state = .flags(.{ .activate = true, .hover = true }),
    });
    defer self.closeElement();

    if (self.hovered()) {
        self.menu_state.setHighlighted(self.gpa, level, self_id);
        // Also clear widget focus when hovering a submenu trigger.
        if (self.state.focused_id != null) {
            self.state.focused_id = null;
        }

        if (self.menu_state.hovered_submenu_candidate) |candidate| {
            if (candidate.id != self_id.id) {
                self.menu_state.hovered_submenu_candidate = self_id;
                self.menu_state.hover_timer.reset();
            }
        } else {
            self.menu_state.hovered_submenu_candidate = self_id;
            self.menu_state.hover_timer.reset();
        }
    }

    const text_color = if (is_highlighted or is_child_open) config.text_color_highlighted else config.text_color;
    try self.text(label, .{
        .font_id = config.font_id,
        .font_size = config.font_size,
        .color = text_color,
    });
    try self.elem(.{ .layout = .{ .sizing = .{ .w = .grow, .h = .grow } } }); // Spacer
    try self.text(">", .{
        .font_id = config.font_id,
        .font_size = config.font_size,
        .color = text_color,
    }); // Arrow

    if (self.activated(self_id)) {
        // Pass the item_id as the parent_item_id.
        // The parent_menu_id is the menu we are currently inside.
        self.openMenu(child_menu_id, .{ .parent_menu_id = parent_menu_id, .parent_item_id = self_id });
    }

    return is_child_open;
}

/// Registers the currently open element as a navigable target within the current menu.
/// This allows keyboard focus to move from menu items into custom embedded widgets.
pub fn menuNavigable(self: *Ui) !void {
    // This function must be called from within an open menu (i.e., inside a beginMenu/endMenu block).
    std.debug.assert(self.open_ids.items.len >= 1);

    // The menu stack must have at least one menu open.
    if (self.menu_state.stack.items.len == 0) {
        log.warn("menuNavigable called, but no menu is currently open in the menu stack.", .{});
        return;
    }

    const element_id = self.open_ids.items[self.open_ids.items.len - 1];

    // The current menu is always the last one on the menu_state stack.
    // This is robust and not dependent on the layout nesting depth.
    const menu_info = self.menu_state.stack.items[self.menu_state.stack.items.len - 1];
    const menu_id = menu_info.id;

    // Add this item to its parent menu's navigable list.
    // The list is guaranteed to exist because it's created in `beginMenu`.
    if (self.menu_state.navigable_items.getPtr(menu_id.id)) |list| {
        try list.append(self.gpa, element_id);
    } else {
        // This case should ideally not be hit if used correctly.
        log.err("Could not find navigable item list for menu {any}", .{menu_id});
    }
}

pub fn menuSeparator(self: *Ui) !void {
    try self.elem(.{
        .layout = .{
            .sizing = .{ .w = .grow, .h = .fixed(1) },
            .padding = .{ .top = 4, .bottom = 4 },
        },
        .background_color = Color.fromLinearU8(200, 200, 200, 255),
    });
}

// End Menu API //

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

pub fn pushEvent(self: *Ui, id: ElementId, data: Event.Data, user_data: ?*anyopaque) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

    // NOTE: we do not check if element_data was found here,
    // because this allows dispatching arbitrary events for non-existent elements,
    // which is useful for shared-state widgets like radio groups:
    // dispatching on the id of the group instead of the element allows for easier event handling on the user side.
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

pub fn getWidgetState(self: *Ui, id: ElementId, comptime T: type) !?T {
    const widget_vtable = self.widget_states.get(id.id) orelse return null;

    if (widget_vtable.state_type != @as(*const anyopaque, @typeName(T))) {
        return null;
    }

    const ptr: *const T = @ptrCast(@alignCast(widget_vtable.get(widget_vtable.user_data, self)));
    return ptr.*;
}

pub fn setWidgetState(self: *Ui, id: ElementId, comptime T: type, state: T) !void {
    if (self.widget_states.get(id.id)) |widget_vtable| {
        if (widget_vtable.state_type != @as(*const anyopaque, @typeName(T))) {
            return error.InvalidWidgetStateType;
        }

        widget_vtable.set(widget_vtable.user_data, self, @ptrCast(&state));
    } else if (self.open_layout) {
        const ptr = try self.deferred_widget_arena_current.allocator().create(T);
        ptr.* = state;
        try self.deferred_widget_states_current.put(self.gpa, id.id, @ptrCast(ptr));
    } else {
        const ptr = try self.deferred_widget_arena_last.allocator().create(T);
        ptr.* = state;
        try self.deferred_widget_states_last.put(self.gpa, id.id, @ptrCast(ptr));
    }
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

pub fn getOrPutSharedWidgetState(self: *Ui, comptime T: type, group_id: ElementId, default_value: T) !*T {
    const key = group_id.id;

    const gop = try self.shared_widget_states.getOrPut(self.gpa, key);

    if (!gop.found_existing) {
        const ptr = try self.gpa.create(T);
        ptr.* = default_value;

        gop.value_ptr.* = SharedWidgetState{
            .data = ptr,
            .deinit = @ptrCast(&struct {
                pub fn destructor(state_ptr: *T, ui: *Ui) void {
                    ui.gpa.destroy(state_ptr);
                }
            }.destructor),
            .state_type = @typeName(T),
            .seen_this_frame = true,
        };

        return ptr;
    } else {
        gop.value_ptr.seen_this_frame = true;

        if (gop.value_ptr.state_type != @as(*const anyopaque, @typeName(T))) {
            return error.SharedStateWrongType;
        }

        return @ptrCast(@alignCast(gop.value_ptr.data));
    }
}

// --- Structures ---

/// Manages registration and dispatching of UI events to listeners.
pub const EventDispatch = struct {
    /// Key combines element ID and event type for efficient lookup.
    pub const Key = struct {
        element_id: u32,
        event_tag: Event.Tag,
    };

    pub const Listener = struct {
        /// Store the function pointer as a generic pointer.
        func: *const anyopaque,
        user_data: *anyopaque,
    };

    /// The value is a list of listeners for a given key.
    pub const ListenerList = std.ArrayList(Listener);

    pub fn Handler(comptime T: type, comptime event_tag: Event.Tag) type {
        return fn (user_data: *T, ui: *Ui, info: Event.Info, payload: Event.Payload(event_tag)) anyerror!void;
    }

    /// The main storage for all listeners.
    listeners: std.HashMapUnmanaged(Key, ListenerList, struct {
        pub fn hash(_: @This(), self: Key) u64 {
            var hasher = std.hash.Wyhash.init(0);
            std.hash.autoHash(&hasher, self);
            return hasher.final();
        }

        pub fn eql(_: @This(), self: Key, other: Key) bool {
            return self.element_id == other.element_id and self.event_tag == other.event_tag;
        }
    }, 80) = .empty,

    pub const empty = EventDispatch{};

    pub fn deinit(self: *EventDispatch, allocator: std.mem.Allocator) void {
        var it = self.listeners.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(allocator);
        }
        self.listeners.deinit(allocator);
    }
};

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

        text_change: []const u8,
        bool_change: bool,
        float_change: f64,
        int_change: i64,
        uint_change: u64,

        menu_opened: void,
        menu_closed: struct { level: u32 },
    };
};

pub const State = struct {
    hovered: ?StateElement = null,
    active_id: ?StateElement = null,
    focused_id: ?StateElement = null,

    pub fn hoveredId(self: State) ?ElementId {
        return (self.hovered orelse return null).id;
    }

    pub fn activeId(self: State) ?ElementId {
        return (self.active_id orelse return null).id;
    }

    pub fn focusedId(self: State) ?ElementId {
        return (self.focused_id orelse return null).id;
    }

    pub fn hoveredIdValue(self: State) ?u32 {
        return (self.hovered orelse return null).id.id;
    }

    pub fn activeIdValue(self: State) ?u32 {
        return (self.active_id orelse return null).id.id;
    }

    pub fn focusedIdValue(self: State) ?u32 {
        return (self.focused_id orelse return null).id.id;
    }
};

pub const MenuState = struct {
    pub const MENU_Z_INDEX_BASE: i16 = 1000;
    pub const SUBMENU_OPEN_DELAY_NS: u64 = 200 * std.time.ns_per_ms;

    pub const OpenMenuInfo = struct {
        id: ElementId, // id of the menu panel being opened
        parent_menu_id: ?ElementId, // id of the parent menu panel
        parent_item_id: ?ElementId, // id of the menu item that triggered this menu
        // attach_to is now redundant, we derive it from parent_item_id
        position: vec2, // screen-relative position to open the menu at
    };

    pub const OpenMenuConfig = struct {
        parent_menu_id: ?ElementId = null, // id of the parent menu panel
        parent_item_id: ?ElementId = null, // id of the menu item that triggered this menu
        position: vec2 = .{ 0, 0 },
    };

    stack: std.ArrayList(OpenMenuInfo) = .empty,
    highlighted_path: std.ArrayList(ElementId) = .empty,
    // A map from a menu panel's ID to a list of its navigable item IDs.
    navigable_items: std.AutoHashMapUnmanaged(u32, std.ArrayList(ElementId)) = .empty,

    open_request: ?OpenMenuInfo = null,
    close_request_level: ?usize = null,

    hover_timer: std.time.Timer = undefined,
    hovered_submenu_candidate: ?ElementId = null,
    // Transient state to signal that a menu was opened in the current event processing phase.
    // This is used to auto-highlight the first item during the next layout pass.
    just_opened_menu_id: ?u32 = null,

    fn deinit(self: *MenuState, gpa: std.mem.Allocator) void {
        self.stack.deinit(gpa);
        self.highlighted_path.deinit(gpa);
        // Deinitialize the hash map and all the lists it contains.
        var it = self.navigable_items.iterator();
        while (it.next()) |entry| {
            var mutable_list = entry.value_ptr.*;
            mutable_list.deinit(gpa);
        }
        self.navigable_items.deinit(gpa);
    }

    fn setHighlighted(self: *MenuState, allocator: std.mem.Allocator, level: usize, id: ElementId) void {
        // If we're setting a highlight at `level`, the path must become exactly `level + 1` long.
        // First, ensure the path is at least that long.
        if (self.highlighted_path.items.len <= level) {
            self.highlighted_path.resize(allocator, level + 1) catch |e| {
                log.err("failed to resize highlighted path: {s}", .{@errorName(e)});
                return;
            };
        } else {
            // If the path is already longer, it means a deeper submenu was highlighted.
            // We must truncate it to the correct new length before setting the new item.
            self.highlighted_path.shrinkRetainingCapacity(level + 1);
        }

        // Now that the ArrayList has the correct size, we can safely set the item.
        self.highlighted_path.items[level] = id;
    }
};

pub const StateElement = struct {
    id: ElementId,
    bounding_box: BoundingBox,
    state: ElementState,
};

pub const ScrollState = struct {
    offset: vec2,
    elem_state: StateElement,
};

pub const ElementState = packed struct(usize) {
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

    pub const none = ElementState{ .event_flags = .none };
    pub const hoverFlag = ElementState{ .event_flags = .hoverFlag };
    pub const wheelFlag = ElementState{ .event_flags = .wheelFlag };
    pub const clickFlag = ElementState{ .event_flags = .clickFlag };
    pub const dragFlag = ElementState{ .event_flags = .dragFlag };
    pub const focusFlag = ElementState{ .event_flags = .focusFlag };
    pub const activateFlag = ElementState{ .event_flags = .activateFlag };
    pub const textFlag = ElementState{ .event_flags = .textFlag };
    pub const keyboardFlag = ElementState{ .event_flags = .keyboardFlag };
    pub const scrollFlag = ElementState{ .event_flags = .scrollFlag };
    pub const changeFlag = ElementState{ .event_flags = .changeFlag };
    pub const all = ElementState{ .event_flags = .all };

    pub fn flags(f: Flags) ElementState {
        return ElementState{ .event_flags = f };
    }

    pub fn custom(f: Flags, user_data: ?*anyopaque) ElementState {
        return ElementState{
            .event_flags = f,
            .user_data = @intCast(@intFromPtr(user_data)),
        };
    }

    fn fromClay(data: ?*anyopaque) ElementState {
        if (data) |ptr| {
            return @bitCast(@as(usize, @intFromPtr(ptr)));
        } else {
            return .none;
        }
    }

    fn toClay(self: ElementState) ?*anyopaque {
        // This converts directly to the nullable pointer so there's no risk from the runtime safety check here.
        return @ptrFromInt(@as(usize, @bitCast(self)));
    }

    pub fn takesInput(self: ElementState) bool {
        return self.event_flags.takesInput();
    }

    pub fn usesMouse(self: ElementState) bool {
        return self.event_flags.usesMouse();
    }

    pub fn getUserData(self: ElementState) ?*anyopaque {
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

pub const TextElementConfig = struct {
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

    fn toClay(self: TextElementConfig) clay.TextElementConfig {
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

    fn toClay(self: ClipElementConfig) clay.ClipElementConfig {
        return clay.ClipElementConfig{
            .horizontal = self.horizontal,
            .vertical = self.vertical,
            .child_offset = vec2ToClay(self.child_offset),
        };
    }
};

pub fn Headless(comptime T: type) type {
    comptime return switch (T) {
        ElementDeclaration => HeadlessElementDeclaration,
        else => @compileError("Unsupported type for Headless"),
    };
}

fn attachHead(comptime T: type, value: Headless(T), head: ElementId) T {
    var out: T = undefined;

    out.id = head;

    inline for (comptime std.meta.fieldNames(Headless(T))) |field| {
        @field(out, field) = @field(value, field);
    }

    return out;
}

fn detachHead(comptime T: type, value: T) Headless(T) {
    var out: Headless(T) = undefined;

    inline for (comptime std.meta.fieldNames(Headless(T))) |field| {
        @field(out, field) = @field(value, field);
    }

    return out;
}

pub const HeadlessElementDeclaration = struct {
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
    widget: bool = false,
    /// Controls whether an element should clip its contents and allow scrolling rather than expanding to contain them.
    clip: ClipElementConfig = .{},
    /// Controls settings related to element borders, and will generate BORDER render command
    border: BorderElementConfig = .{},
    /// A pointer that will be transparently passed through to resulting render command
    state: ElementState = .none,
};

pub const ElementDeclaration = struct {
    id: ElementId = .{},
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
    widget: bool = false,
    /// Controls whether an element should clip its contents and allow scrolling rather than expanding to contain them.
    clip: ClipElementConfig = .{},
    /// Controls settings related to element borders, and will generate BORDER render command
    border: BorderElementConfig = .{},
    /// A pointer that will be transparently passed through to resulting render command
    state: ElementState = .none,

    fn toClay(self: ElementDeclaration) clay.ElementDeclaration {
        return clay.ElementDeclaration{
            .id = self.id,
            .layout = self.layout,
            .background_color = colorToClay(self.background_color),
            .corner_radius = self.corner_radius,
            .aspect_ratio = .{ .aspect_ratio = self.aspect_ratio },
            .image = imageToClay(self.image),
            .floating = self.floating.toClay(),
            .custom = .{ .custom_data = @ptrFromInt(@intFromBool(self.widget)) },
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
fn clayColorToBatchColor(c: clay.Color) Batch2D.Color {
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

fn handleStateSetup(self: *Ui, declaration: ElementDeclaration) !void {
    // Capture element info for this frame.
    const parent_id: ?ElementId = if (self.open_ids.items.len > 1)
        self.open_ids.items[self.open_ids.items.len - 2]
    else
        null;

    try self.frame_element_info.put(self.gpa, declaration.id.id, .{
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
            try self.current_scroll_states.put(self.gpa, declaration.id.id, .{ .offset = offset, .elem_state = .{
                .id = declaration.id,
                .bounding_box = scrolled_data.bounding_box,
                .state = declaration.state,
            } });
        }
    }
}

/// The error reporting function that Clay calls when it encounters an error.
/// This function is registered in `init`.
fn reportClayError(data: clay.ErrorData) callconv(.c) void {
    std.log.scoped(.clay).err("{s} - {s}", .{ @tagName(data.error_type), data.error_text.toSlice() });
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

// A unified helper to perform navigation within a menu.
fn navigateMenu(self: *Ui, menu_id: ElementId, direction: enum { next, prev, up, down }) !void {
    const level = self.menu_state.stack.items.len - 1;
    const items_list = self.menu_state.navigable_items.get(menu_id.id) orelse return;
    if (items_list.items.len == 0) return;

    // --- Step 1: Find the index of the currently active item. ---
    var current_idx: ?usize = null;

    // First, check if a specific WIDGET inside the menu has focus. This takes precedence.
    if (self.state.focusedId()) |focused_id| {
        // We walk up the parent tree from the focused widget to find which of our
        // navigable items is its ancestor. This handles cases like a slider inside a menu.
        // Crucially, this check will FAIL if the focused element is just the menu panel itself,
        // because the panel is not in the `items_list`. This is the key to solving the bug.
        for (items_list.items, 0..) |navigable_item_id, i| {
            var is_match = false;
            var walker_id: ?ElementId = focused_id;
            while (walker_id) |current_id| {
                if (current_id.id == navigable_item_id.id) {
                    is_match = true;
                    break;
                }
                // Stop walking if we hit the menu panel itself without a match.
                if (current_id.id == menu_id.id) break;

                const info = self.frame_element_info.get(current_id.id) orelse break;
                walker_id = info.parent_id;
            }
            if (is_match) {
                current_idx = i;
                break;
            }
        }
    }

    // If no specific widget was focused, we fall back to using the visual highlight as our starting point.
    // This will be the case when the menu is first opened.
    if (current_idx == null and self.menu_state.highlighted_path.items.len > level) {
        const highlighted_id = self.menu_state.highlighted_path.items[level];
        for (items_list.items, 0..) |item_id, i| {
            if (item_id.id == highlighted_id.id) {
                current_idx = i;
                break;
            }
        }
    }

    // --- Step 2: Calculate the next index based on direction. ---
    // This calculation now works correctly on the first press because `current_idx`
    // will be `0` (from the highlighted path), not `null`.
    const new_idx: usize = switch (direction) {
        .next, .down => if (current_idx) |idx| (idx + 1) % items_list.items.len else 0,
        .prev, .up => if (current_idx) |idx| (idx + items_list.items.len - 1) % items_list.items.len else items_list.items.len - 1,
    };

    const new_target_id = items_list.items[new_idx];

    // --- Step 3: Activate the new target appropriately (unchanged from before). ---
    if (self.isWidgetNavigable(new_target_id)) {
        // The new target is a widget that requires true focus.
        // 1. Clear the visual menu item highlight.
        self.menu_state.highlighted_path.shrinkRetainingCapacity(level);
        // 2. Give the widget global focus.
        const info = self.frame_element_info.get(new_target_id.id).?;
        const element_data = clay.getElementData(new_target_id);
        if (element_data.found) {
            self.state.focused_id = .{
                .id = new_target_id,
                .bounding_box = element_data.bounding_box,
                .state = info.state,
            };
        }
    } else {
        // The new target is a standard menu item.
        // 1. Clear global focus in case we just navigated off a widget.
        self.state.focused_id = null;
        // 2. Set the visual highlight on the new menu item.
        self.menu_state.setHighlighted(self.gpa, level, new_target_id);
    }
}

fn onMenuCloseScope(_: *anyopaque, self: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.scoped_focus_close)) !void {
    // This event is sent when 'Escape' is pressed and a focus scope is active.
    // For a menu, this should close the current sub-menu level.
    self.closeTopMenu();
}

fn onMenuActivate(_: *anyopaque, self: *Ui, info: Ui.Event.Info, _: Ui.Event.Payload(.activate_end)) !void {
    // This handler fires when the menu panel itself is "activated" (e.g., by pressing Enter).
    // It finds the currently highlighted item and performs the appropriate action.
    const level = self.menu_state.stack.items.len - 1;
    if (self.menu_state.highlighted_path.items.len > level) {
        const highlighted_id = self.menu_state.highlighted_path.items[level];
        // Check if the highlighted item is a submenu trigger.
        if (self.menu_item_to_submenu_map.get(highlighted_id.id)) |child_menu_id| {
            // It's a submenu. Open it directly.
            const parent_menu_id = info.element_id; // The event came from the parent menu.
            self.openMenu(child_menu_id, .{ .parent_menu_id = parent_menu_id, .parent_item_id = highlighted_id });
        } else {
            // It's a regular menu item. Push an event for the application to handle...
            try self.pushEvent(highlighted_id, .activate_begin, null);
            try self.pushEvent(highlighted_id, .{ .activate_end = .{ .end_element = highlighted_id, .modifiers = .{} } }, null);
            // ...and close all menus.
            self.closeAllMenus();
        }
    }
}

fn onMenuFocusChange(_: *anyopaque, self: *Ui, info: Ui.Event.Info, payload: Ui.Event.Payload(.scoped_focus_change)) !void {
    // This handler manages Tab and Shift+Tab navigation within a menu.
    try navigateMenu(self, info.element_id, switch (payload) {
        .next => .next,
        .prev => .prev,
    });
}

fn onMenuKeyDown(_: *anyopaque, self: *Ui, info: Ui.Event.Info, key_data: Ui.Event.Payload(.key_down)) !void {
    switch (key_data.key) {
        .down => try navigateMenu(self, info.element_id, .down),
        .up => try navigateMenu(self, info.element_id, .up),
        .right => {
            // Right arrow should open a submenu if a submenu item is highlighted.
            const level = self.menu_state.stack.items.len - 1;
            if (self.menu_state.highlighted_path.items.len > level) {
                const highlighted_id = self.menu_state.highlighted_path.items[level];
                if (self.menu_item_to_submenu_map.get(highlighted_id.id)) |child_menu_id| {
                    const parent_menu_id = info.element_id;
                    self.openMenu(child_menu_id, .{
                        .parent_menu_id = parent_menu_id,
                        .parent_item_id = highlighted_id,
                    });
                }
            }
        },
        .left => {
            // If we are in a submenu (level > 0), close this menu and go back to the parent.
            if (self.menu_state.stack.items.len - 1 > 0) {
                self.closeTopMenu();
            }
        },
        else => {},
    }
}

/// Manages the menu stack, processes open/close requests, and handles global menu interactions like "click outside".
fn handleMenuState(self: *Ui) !void {
    const ms = &self.menu_state;

    // Reset transient state at the start of each frame's processing.
    ms.just_opened_menu_id = null;

    // --- 1. Process deferred close requests ---
    if (ms.close_request_level) |level| {
        while (ms.stack.items.len > level) {
            // When popping a menu, we must remove its listeners and its navigable item list.
            const closed_menu = ms.stack.pop() orelse break;
            self.removeListener(closed_menu.id, .key_down, anyopaque, &onMenuKeyDown);
            self.removeListener(closed_menu.id, .scoped_focus_change, anyopaque, &onMenuFocusChange);
            self.removeListener(closed_menu.id, .scoped_focus_close, anyopaque, &onMenuCloseScope);
            self.removeListener(closed_menu.id, .activate_end, anyopaque, &onMenuActivate);
            self.popFocusScope();

            // Clean up the navigable items list for the closed menu to prevent memory leaks.
            if (ms.navigable_items.fetchRemove(closed_menu.id.id)) |removed_list| {
                var mutable_list = removed_list.value; // thank you zig very helpful
                mutable_list.deinit(self.gpa);
            }

            // Fire the public event notifying the application that a menu was closed.
            try self.pushEvent(closed_menu.id, .{ .menu_closed = .{ .level = @intCast(ms.stack.items.len) } }, null);
        }

        // After closing a menu, explicitly restore focus to the new top-most menu.
        if (ms.stack.items.len > 0) {
            const parent_menu = ms.stack.items[ms.stack.items.len - 1];
            self.state.focused_id = .{
                .id = parent_menu.id,
                .bounding_box = clay.getElementData(parent_menu.id).bounding_box,
                .state = .flags(.{ .focus = true, .keyboard = true, .activate = true }),
            };
        } else {
            // If all menus were closed, clear focus.
            self.state.focused_id = null;
        }

        // After closing menus, the highlighted path might be longer than the new
        // menu stack. We must truncate it to match.
        if (ms.highlighted_path.items.len > ms.stack.items.len) {
            ms.highlighted_path.shrinkRetainingCapacity(ms.stack.items.len);
        }
        ms.close_request_level = null;
    }

    // --- 2. Process deferred open requests ---
    if (ms.open_request) |request| {
        var open_at_level: usize = 0;
        if (request.parent_menu_id) |parent_menu| {
            // Find the level of the parent menu.
            var found_parent = false;
            for (ms.stack.items, 0..) |menu_info, i| {
                if (menu_info.id.id == parent_menu.id) {
                    open_at_level = i + 1; // The new menu will be one level deeper.
                    found_parent = true;
                    break;
                }
            }
            if (!found_parent) {
                // The parent menu must have closed in the same frame.
                // Abort the open request to prevent a detached menu.
                ms.open_request = null;
                return;
            }
        } else {
            // No parent menu, so it's a root menu (e.g., context menu).
            open_at_level = 0;
        }

        // Close any menus deeper than the level we're opening at
        while (ms.stack.items.len > open_at_level) {
            // Also remove listeners and navigable item lists when closing menus here.
            const closed_menu = ms.stack.pop() orelse break;
            self.removeListener(closed_menu.id, .key_down, anyopaque, &onMenuKeyDown);
            self.removeListener(closed_menu.id, .scoped_focus_change, anyopaque, &onMenuFocusChange);
            self.removeListener(closed_menu.id, .scoped_focus_close, anyopaque, &onMenuCloseScope);
            self.removeListener(closed_menu.id, .activate_end, anyopaque, &onMenuActivate);
            self.popFocusScope();

            if (ms.navigable_items.fetchRemove(closed_menu.id.id)) |removed_list| {
                var mutable_list = removed_list.value; // thank you zig very helpful
                mutable_list.deinit(self.gpa);
            }

            // Fire the public event here as well
            try self.pushEvent(closed_menu.id, .{ .menu_closed = .{ .level = @intCast(ms.stack.items.len) } }, null);
        }

        // When opening a menu, we must truncate the highlighted path to the new
        // parent level. This removes highlights from any deeper submenus that
        // were just closed.
        if (ms.highlighted_path.items.len > open_at_level) {
            ms.highlighted_path.shrinkRetainingCapacity(open_at_level);
        }

        // Push the new menu
        try ms.stack.append(self.gpa, request);
        ms.just_opened_menu_id = request.id.id; // Signal that this menu just opened.
        try self.pushFocusScope(request.id);

        // Fire the public event notifying the application that a menu was opened.
        try self.pushEvent(request.id, .menu_opened, null);

        // Add the listeners now that the menu is officially open.
        // These will persist until the menu is closed.
        try self.addListener(request.id, .key_down, anyopaque, &onMenuKeyDown, undefined);
        try self.addListener(request.id, .scoped_focus_change, anyopaque, &onMenuFocusChange, undefined);
        try self.addListener(request.id, .scoped_focus_close, anyopaque, &onMenuCloseScope, undefined);
        try self.addListener(request.id, .activate_end, anyopaque, &onMenuActivate, undefined);

        // Set focus to the new menu.
        self.state.focused_id = .{
            .id = request.id,
            .bounding_box = .{}, // BBox is not known yet, but that's okay.
            .state = .flags(.{ .focus = true, .keyboard = true, .activate = true }),
        };

        ms.open_request = null;
    }

    // --- 3. Handle hover-to-open for submenus ---
    if (ms.hovered_submenu_candidate) |candidate_id| {
        if (ms.hover_timer.read() > MenuState.SUBMENU_OPEN_DELAY_NS) {
            // Activate the candidate item to trigger its opening logic
            try self.pushEvent(candidate_id, .activate_begin, null);
            try self.pushEvent(candidate_id, .{ .activate_end = .{ .end_element = candidate_id, .modifiers = .{} } }, null);
            ms.hovered_submenu_candidate = null;
        }
    } else {
        ms.hover_timer.reset();
    }

    // --- 4. Handle "click outside" to close all menus ---
    if (self.bindings.getAction(.primary_mouse) == .pressed and ms.stack.items.len > 0) {
        var click_is_inside_a_menu_panel = false;
        const mouse_pos = self.bindings.getMousePosition();

        // Iterate through all open menus and check if the mouse position is inside any of their bounding boxes.
        for (ms.stack.items) |menu_info| {
            const menu_data = clay.getElementData(menu_info.id);
            if (menu_data.found) {
                if (boxContains(menu_data.bounding_box, mouse_pos)) {
                    click_is_inside_a_menu_panel = true;
                    break;
                }
            }
        }

        // If the click was not inside any menu's bounds, close them all.
        if (!click_is_inside_a_menu_panel) {
            self.closeAllMenus();
        }
    }
}

/// Processes the current UI state against the last frame's state to generate interaction events.
fn generateEvents(self: *Ui) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

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

    // --- Handle Menu State Changes ---
    try self.handleMenuState();

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
        var clear_focus = true;
        for (self.hovered_element_stack.items, 0..) |_, i| {
            const hovered_state = self.hovered_element_stack.items[self.hovered_element_stack.items.len - 1 - i];
            if (hovered_state.state.event_flags.focus) {
                self.state.focused_id = hovered_state;
                clear_focus = false;
                break;
            }
        }
        if (clear_focus) {
            self.state.focused_id = null;
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
            // This was not a drag. Check if it's a regular menu item click, which should close the menu.
            var is_regular_menu_item = false;
            var menu_it = self.menu_state.navigable_items.valueIterator();
            is_in_any_menu: while (menu_it.next()) |item_list| {
                for (item_list.items) |item_id| {
                    if (item_id.id == last_active.id.id) {
                        // It is a menu item. Now check if it's a submenu trigger.
                        if (!self.menu_item_to_submenu_map.contains(last_active.id.id)) {
                            is_regular_menu_item = true;
                        }
                        break :is_in_any_menu;
                    }
                }
            }

            if (is_regular_menu_item) {
                self.closeAllMenus();
            }

            // Now, proceed with click/double-click event generation.
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

                // Clay borders are drawn inset. Our drawRoundedRectLine function does this.
                // However, Clay supports different widths per side, which our function does not.
                // We will draw each side as a separate component.

                // Top bar
                try self.renderer.drawQuad(.{ pos[0] + r.top_left, pos[1] }, .{ size[0] - r.top_left - r.top_right, @floatFromInt(data.width.top) }, color);
                // Bottom bar
                try self.renderer.drawQuad(.{ pos[0] + r.bottom_left, pos[1] + size[1] - @as(f32, @floatFromInt(data.width.bottom)) }, .{ size[0] - r.bottom_left - r.bottom_right, @floatFromInt(data.width.bottom) }, color);
                // Left bar
                try self.renderer.drawQuad(.{ pos[0], pos[1] + r.top_left }, .{ @floatFromInt(data.width.left), size[1] - r.top_left - r.bottom_left }, color);
                // Right bar
                try self.renderer.drawQuad(.{ pos[0] + size[0] - @as(f32, @floatFromInt(data.width.right)), pos[1] + r.top_right }, .{ @floatFromInt(data.width.right), size[1] - r.top_right - r.bottom_right }, color);

                const pi = std.math.pi;
                // Top-left corner
                try self.renderer.drawArcLine(.{ pos[0] + r.top_left, pos[1] + r.top_left }, r.top_left, pi, 1.5 * pi, @max(@as(f32, @floatFromInt(data.width.top)), @as(f32, @floatFromInt(data.width.left))), color);
                // Top-right corner
                try self.renderer.drawArcLine(.{ pos[0] + size[0] - r.top_right, pos[1] + r.top_right }, r.top_right, 1.5 * pi, 2.0 * pi, @max(@as(f32, @floatFromInt(data.width.top)), @as(f32, @floatFromInt(data.width.right))), color);
                // Bottom-right corner
                try self.renderer.drawArcLine(.{ pos[0] + size[0] - r.bottom_right, pos[1] + size[1] - r.bottom_right }, r.bottom_right, 0, 0.5 * pi, @max(@as(f32, @floatFromInt(data.width.bottom)), @as(f32, @floatFromInt(data.width.right))), color);
                // Bottom-left corner
                try self.renderer.drawArcLine(.{ pos[0] + r.bottom_left, pos[1] + size[1] - r.bottom_left }, r.bottom_left, 0.5 * pi, pi, @max(@as(f32, @floatFromInt(data.width.bottom)), @as(f32, @floatFromInt(data.width.left))), color);
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
            .scissor_start => try self.renderer.scissorStart(pos, size),
            .scissor_end => try self.renderer.scissorEnd(),
            .custom => {
                const widget = self.widget_states.get(cmd.id) orelse {
                    log.warn("No widget state found for custom render command with id {x}", .{cmd.id});
                    continue;
                };

                try widget.render(widget.user_data, self, cmd);
            },
        }
    }
}
