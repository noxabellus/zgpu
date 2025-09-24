//! An abstraction layer for rendering *interactive* UI layouts based on the clay layout engine, rendered with our Batch2D library.

const Ui = @This();

const std = @import("std");
const clay = @import("clay");

const Batch2D = @import("Batch2D.zig");
const AssetCache = @import("AssetCache.zig");
const BindingState = @import("BindingState.zig");

const log = std.log.scoped(.ui);

test {
    log.debug("semantic analysis for Ui.zig", .{});
    std.testing.refAllDecls(@This());
}

/// Manages registration and dispatching of UI events to listeners.
const EventDispatch = struct {
    /// Key combines element ID and event type for efficient lookup.
    const Key = struct {
        element_id: u32,
        event_tag: Event.Tag,
    };

    const Listener = struct {
        /// Store the function pointer as a generic pointer.
        func: *const anyopaque,
        user_data: *anyopaque,
    };

    /// The value is a list of listeners for a given key.
    const ListenerList = std.ArrayList(Listener);

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

pub const Vec2 = Batch2D.Vec2;
pub const Color = Batch2D.Color;

pub const default_bindings = .{
    .primary_mouse = BindingState.InputBinding{ .mouse = .{ .bind_point = .button_1 } },
    .focus_next = BindingState.InputBinding{ .key = .{ .bind_point = .tab } },
    .focus_prev = BindingState.InputBinding{ .key = .{ .bind_point = .tab, .modifiers = .shiftMod } },
    .activate_focused = BindingState.InputBinding{ .key = .{ .bind_point = .enter } },
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

custom_element_renderer: ?*const fn (*Ui, ?*anyopaque, clay.RenderCommand) anyerror!void = null,
user_data: ?*anyopaque = null,

state: State = .{},
last_state: State = .{},
wheel_delta: Vec2 = .{},
char_input: []const BindingState.Char = &.{},

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
    };

    self.text_repeat.timer = try .start();

    // Ensure required ui input bindings are registered
    if (!bindings.hasBinding(.primary_mouse)) try bindings.bind(.primary_mouse, default_bindings.primary_mouse);
    if (!bindings.hasBinding(.focus_next)) try bindings.bind(.focus_next, default_bindings.focus_next);
    if (!bindings.hasBinding(.focus_prev)) try bindings.bind(.focus_prev, default_bindings.focus_prev);
    if (!bindings.hasBinding(.activate_focused)) try bindings.bind(.activate_focused, default_bindings.activate_focused);

    return self;
}

pub fn deinit(self: *Ui) void {
    self.events.deinit(self.gpa);
    self.hovered_element_stack.deinit(self.gpa);
    self.navigable_elements.deinit(self.gpa);
    self.reverse_navigable_elements.deinit(self.gpa);
    self.open_ids.deinit(self.gpa);
    self.event_dispatch.deinit(self.gpa);

    self.gpa.free(self.clay_memory);
    self.gpa.destroy(self);
}

/// Call this at any time in the update stage to begin declaring the ui layout. Caller must ensure `Ui.endLayout` is called before the next `Ui.beginLayout` and/or `Ui.render`.
/// Note: it is acceptable to run the layout code multiple times per frame, but all state will be discarded when calling this function.
pub fn beginLayout(self: *Ui, dimensions: Batch2D.Vec2, delta_ms: f32) void {
    self.render_commands = null;

    self.events.clearRetainingCapacity();
    self.hovered_element_stack.clearRetainingCapacity();
    self.navigable_elements.clearRetainingCapacity();
    self.reverse_navigable_elements.clearRetainingCapacity();
    self.open_ids.clearRetainingCapacity();

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
}

/// End the current layout declaration and finalize the render commands and events.
/// Must be called after `Ui.beginLayout` and before `Ui.render`.
pub fn endLayout(self: *Ui) !void {
    std.debug.assert(self.clay_context == clay.getCurrentContext());
    defer clay.setCurrentContext(null);

    const render_commands = clay.endLayout();
    self.render_commands = render_commands;

    try self.generateEvents();
}

/// Iterates through the generated events and calls any registered listeners.
pub fn dispatchEvents(self: *Ui) !void {
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
            if (listener_list.items[i].func == listener_fn) {
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
    const full = declaration.toFull(id);
    clay.configureElement(full.toClay());

    try self.handleStateSetup(full);
}

/// Create a new element with the given declaration.
/// * Must be followed by a call to `Ui.closeElement`.
/// * Note that functions like `Ui.hovered` and `Ui.scrollOffset` will not work inside the passed declaration; use `Ui.beginElement` and `Ui.configureElement` instead.
/// * See also `Ui.elem`.
pub fn openElement(self: *Ui, declaration: ElementDeclarationWithId) !void {
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
pub fn elem(self: *Ui, declaration: ElementDeclarationWithId) !void {
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
pub fn scrollOffset(self: *Ui) Vec2 {
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

/// Get the offset data for a character within the only text element inside the element with the given id.
pub fn getCharacterOffset(self: *Ui, id: ElementId, char_index: u32) ?CharacterOffset {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    return clay.getCharacterOffset(id, char_index);
}

pub fn getCharacterIndexAtOffset(self: *Ui, id: ElementId, offset: Vec2) ?u32 {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);
    return clay.getCharacterIndexAtOffset(id, vec2ToClay(offset));
}

// --- Structures ---

pub const Event = struct {
    info: Info,
    data: Data,

    pub const Tag = std.meta.Tag(Data);

    pub fn Payload(comptime tag: Tag) type {
        return std.meta.TagPayload(Data, tag);
    }

    pub const Info = struct {
        element_id: ElementId,
        bounding_box: BoundingBox,
        user_data: ?*anyopaque,
    };

    pub const Data = union(enum) {
        hover_begin: struct { mouse_position: Vec2, modifiers: BindingState.Modifiers },
        hovering: struct { mouse_position: Vec2, modifiers: BindingState.Modifiers },
        hover_end: struct { mouse_position: Vec2, modifiers: BindingState.Modifiers },

        mouse_down: struct { mouse_position: Vec2, modifiers: BindingState.Modifiers },
        mouse_up: struct { mouse_position: Vec2, end_element: ?ElementId, modifiers: BindingState.Modifiers },
        clicked: struct { mouse_position: Vec2, modifiers: BindingState.Modifiers },

        drag: struct { mouse_position: Vec2, modifiers: BindingState.Modifiers },

        text: union(enum) {
            chars: []const BindingState.Char,
            command: struct {
                action: enum { delete, backspace, newline, copy, paste, select_all, move_left, move_right, move_up, move_down, home, end },
                modifiers: BindingState.Modifiers,
            },
        },

        wheel: struct { delta: Vec2, modifiers: BindingState.Modifiers },

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
            old_offset: Vec2,
            new_offset: Vec2,
            delta: Vec2,
        },
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

pub const StateElement = struct {
    id: ElementId,
    bounding_box: BoundingBox,
    state: ElementState,
};

pub const ScrollState = struct {
    offset: Vec2,
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

        _reserved: u7 = 0,

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
    color: Color = .{ .r = 0, .g = 0, .b = 0, .a = 255 },
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
    offset: Vec2 = .{},
    /// Expands the boundaries of the outer floating element without affecting children
    expand: Vec2 = .{},
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
    child_offset: Vec2 = .{ .x = 0, .y = 0 },

    fn toClay(self: ClipElementConfig) clay.ClipElementConfig {
        return clay.ClipElementConfig{
            .horizontal = self.horizontal,
            .vertical = self.vertical,
            .child_offset = vec2ToClay(self.child_offset),
        };
    }
};

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
    /// Used to create CUSTOM render commands, usually to render element types not supported by default.
    custom: CustomElementConfig = null,
    /// Controls whether an element should clip its contents and allow scrolling rather than expanding to contain them.
    clip: ClipElementConfig = .{},
    /// Controls settings related to element borders, and will generate BORDER render command
    border: BorderElementConfig = .{},
    /// A pointer that will be transparently passed through to resulting render command
    state: ElementState = .none,

    fn toFull(self: HeadlessElementDeclaration, id: ElementId) ElementDeclarationWithId {
        return ElementDeclarationWithId{
            .id = id,
            .layout = self.layout,
            .background_color = self.background_color,
            .corner_radius = self.corner_radius,
            .aspect_ratio = self.aspect_ratio,
            .image = self.image,
            .floating = self.floating,
            .custom = self.custom,
            .clip = self.clip,
            .border = self.border,
            .state = self.state,
        };
    }
};

pub const ElementDeclarationWithId = struct {
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
    /// Used to create CUSTOM render commands, usually to render element types not supported by default.
    custom: CustomElementConfig = null,
    /// Controls whether an element should clip its contents and allow scrolling rather than expanding to contain them.
    clip: ClipElementConfig = .{},
    /// Controls settings related to element borders, and will generate BORDER render command
    border: BorderElementConfig = .{},
    /// A pointer that will be transparently passed through to resulting render command
    state: ElementState = .none,

    fn toHeadless(self: ElementDeclarationWithId) HeadlessElementDeclaration {
        return HeadlessElementDeclaration{
            .layout = self.layout,
            .background_color = self.background_color,
            .corner_radius = self.corner_radius,
            .aspect_ratio = self.aspect_ratio,
            .image = self.image,
            .floating = self.floating,
            .custom = self.custom,
            .clip = self.clip,
            .border = self.border,
            .state = self.state,
        };
    }

    fn toClay(self: ElementDeclarationWithId) clay.ElementDeclaration {
        return clay.ElementDeclaration{
            .id = self.id,
            .layout = self.layout,
            .background_color = colorToClay(self.background_color),
            .corner_radius = self.corner_radius,
            .aspect_ratio = .{ .aspect_ratio = self.aspect_ratio },
            .image = imageToClay(self.image),
            .floating = self.floating.toClay(),
            .custom = .{ .custom_data = self.custom },
            .clip = self.clip.toClay(),
            .border = self.border.toClay(),
            .user_data = self.state.toClay(),
        };
    }
};

// --- Helper functions ---

fn vec2FromClay(vec: clay.Vector2) Batch2D.Vec2 {
    return .{ .x = vec.x, .y = vec.y };
}

fn vec2ToClay(vec: Batch2D.Vec2) clay.Vector2 {
    return .{ .x = vec.x, .y = vec.y };
}

fn vec2FromDims(dims: clay.Dimensions) Batch2D.Vec2 {
    return .{ .x = dims.w, .y = dims.h };
}

fn vec2ToDims(vec: Batch2D.Vec2) clay.Dimensions {
    return .{ .w = vec.x, .h = vec.y };
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

fn srgbToLinear(c: f32) f32 {
    if (c <= 0.04045) {
        return c / 12.92;
    } else {
        return std.math.pow(f32, (c + 0.055) / 1.055, 2.4);
    }
}

fn linearToSrgb(c: f32) f32 {
    if (c <= 0.0031308) {
        return c * 12.92;
    } else {
        return 1.055 * std.math.pow(f32, c, 1.0 / 2.4) - 0.055;
    }
}

fn decodeImageId(id: usize) AssetCache.ImageId {
    return @intCast(id - 1);
}

pub fn boxContains(box: BoundingBox, point: Vec2) bool {
    return point.x >= box.x and point.x < box.x + box.width and point.y >= box.y and point.y < box.y + box.height;
}

pub fn boxesIntersect(a: BoundingBox, b: BoundingBox) bool {
    return a.x < b.x + b.width and a.x + a.width > b.x and a.y < b.y + b.height and a.y + a.height > b.y;
}

pub fn boxContainsBox(outer: BoundingBox, inner: BoundingBox) bool {
    return inner.x >= outer.x and inner.x + inner.width <= outer.x + outer.width and inner.y >= outer.y and inner.y + inner.height <= outer.y + outer.height;
}

pub fn clampToBox(box: BoundingBox, point: Vec2) Vec2 {
    return .{
        .x = std.math.clamp(point.x, box.x, box.x + box.width),
        .y = std.math.clamp(point.y, box.y, box.y + box.height),
    };
}

pub fn relativizeToBox(box: BoundingBox, point: Vec2) Vec2 {
    return .{
        .x = point.x - box.x,
        .y = point.y - box.y,
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
        .r = srgbToLinear(c[0] / 255.0),
        .g = srgbToLinear(c[1] / 255.0),
        .b = srgbToLinear(c[2] / 255.0),
        .a = srgbToLinear(c[3] / 255.0),
    };
}

// --- Backend implementation --- //

fn handleStateSetup(self: *Ui, declaration: ElementDeclarationWithId) !void {
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
        return .{ .w = dims.x, .h = dims.y };
    } else {
        return .{ .w = 0, .h = 0 };
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

    const modifiers = self.bindings.input_state.getModifiers();

    // The top-most element in the stack is the one we consider "hovered" for this frame.
    if (self.hovered_element_stack.items.len > 0) {
        self.state.hovered = self.hovered_element_stack.items[self.hovered_element_stack.items.len - 1];
    }

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
    var is_mouse_activating = false;
    if (primary_mouse_action.isDown()) {
        var found_click_target = false;

        // Iterate through the hovered stack from top to bottom to find the correct event target.
        for (self.hovered_element_stack.items, 0..) |_, i| {
            const hovered_state = self.hovered_element_stack.items[self.hovered_element_stack.items.len - 1 - i];

            // The first clickable, draggable or activatable element we find becomes active.
            if (!found_click_target and (hovered_state.state.event_flags.click or hovered_state.state.event_flags.activate or hovered_state.state.event_flags.drag)) {
                self.state.active_id = hovered_state;
                is_mouse_activating = true;
                found_click_target = true;
                break;
            }
        }
    }

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

    // --- Generate Drag Event ---
    if (primary_mouse_action.isDown() and self.state.active_id != null) {
        const active_elem = self.state.active_id.?;
        if (active_elem.state.event_flags.drag) {
            try self.events.append(self.gpa, .{
                .info = .{
                    .element_id = active_elem.id,
                    .bounding_box = active_elem.bounding_box,
                    .user_data = active_elem.state.getUserData(),
                },
                .data = .{ .drag = .{ .mouse_position = mouse_pos, .modifiers = modifiers } },
            });
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
        if (last_active.state.event_flags.click) {
            try self.events.append(self.gpa, .{
                .info = .{
                    .element_id = last_active.id,
                    .bounding_box = last_active.bounding_box,
                    .user_data = last_active.state.getUserData(),
                },
                .data = .{ .mouse_up = .{ .mouse_position = mouse_pos, .end_element = self.state.hoveredId(), .modifiers = modifiers } },
            });

            if (last_active.id.id == self.state.hoveredIdValue()) {
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = last_active.id,
                        .bounding_box = last_active.bounding_box,
                        .user_data = last_active.state.getUserData(),
                    },
                    .data = .{ .clicked = .{ .mouse_position = mouse_pos, .modifiers = modifiers } },
                });
            }
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
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = new_active.id,
                        .bounding_box = new_active.bounding_box,
                        .user_data = new_active.state.getUserData(),
                    },
                    .data = .{ .mouse_down = .{ .mouse_position = mouse_pos, .modifiers = modifiers } },
                });
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
    } else if (self.state.active_id) |active_elem| {
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

    // --- Handle Keyboard Focus Events ---
    if (self.navigable_elements.count() > 0) {
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
    if (self.wheel_delta.x != 0 or self.wheel_delta.y != 0) {
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
        if (curr_state.offset.x != last_state.offset.x or curr_state.offset.y != last_state.offset.y) {
            if (curr_state.elem_state.state.event_flags.scroll) {
                try self.events.append(self.gpa, .{
                    .info = .{
                        .element_id = curr_state.elem_state.id,
                        .bounding_box = curr_state.elem_state.bounding_box,
                        .user_data = curr_state.elem_state.state.getUserData(),
                    },
                    .data = .{ .scroll = .{ .old_offset = last_state.offset, .new_offset = curr_state.offset, .delta = .{
                        .x = curr_state.offset.x - last_state.offset.x,
                        .y = curr_state.offset.y - last_state.offset.y,
                    } } },
                });
            }
        }
    }
}

/// Processes the current array of RenderCommands from Clay and issues draw calls to Batch2D.
fn draw(self: *Ui) !void {
    clay.setCurrentContext(self.clay_context); // Needed for text measurement callbacks.
    defer clay.setCurrentContext(null);

    const render_commands = self.render_commands orelse {
        log.debug("Ui.render called without any render commands (did you forget to call Ui.beginLayout/Ui.endLayout?)", .{});
        return;
    };

    for (render_commands) |cmd| {
        const bb = cmd.bounding_box;
        const pos = Batch2D.Vec2{ .x = bb.x, .y = bb.y };
        const size = Batch2D.Vec2{ .x = bb.width, .y = bb.height };

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
                try self.renderer.drawQuad(.{ .x = pos.x + r.top_left, .y = pos.y }, .{ .x = size.x - r.top_left - r.top_right, .y = @floatFromInt(data.width.top) }, color);
                // Bottom bar
                try self.renderer.drawQuad(.{ .x = pos.x + r.bottom_left, .y = pos.y + size.y - @as(f32, @floatFromInt(data.width.bottom)) }, .{ .x = size.x - r.bottom_left - r.bottom_right, .y = @floatFromInt(data.width.bottom) }, color);
                // Left bar
                try self.renderer.drawQuad(.{ .x = pos.x, .y = pos.y + r.top_left }, .{ .x = @floatFromInt(data.width.left), .y = size.y - r.top_left - r.bottom_left }, color);
                // Right bar
                try self.renderer.drawQuad(.{ .x = pos.x + size.x - @as(f32, @floatFromInt(data.width.right)), .y = pos.y + r.top_right }, .{ .x = @floatFromInt(data.width.right), .y = size.y - r.top_right - r.bottom_right }, color);

                const pi = std.math.pi;
                // Top-left corner
                try self.renderer.drawArcLine(.{ .x = pos.x + r.top_left, .y = pos.y + r.top_left }, r.top_left, pi, 1.5 * pi, @max(@as(f32, @floatFromInt(data.width.top)), @as(f32, @floatFromInt(data.width.left))), color);
                // Top-right corner
                try self.renderer.drawArcLine(.{ .x = pos.x + size.x - r.top_right, .y = pos.y + r.top_right }, r.top_right, 1.5 * pi, 2.0 * pi, @max(@as(f32, @floatFromInt(data.width.top)), @as(f32, @floatFromInt(data.width.right))), color);
                // Bottom-right corner
                try self.renderer.drawArcLine(.{ .x = pos.x + size.x - r.bottom_right, .y = pos.y + size.y - r.bottom_right }, r.bottom_right, 0, 0.5 * pi, @max(@as(f32, @floatFromInt(data.width.bottom)), @as(f32, @floatFromInt(data.width.right))), color);
                // Bottom-left corner
                try self.renderer.drawArcLine(.{ .x = pos.x + r.bottom_left, .y = pos.y + size.y - r.bottom_left }, r.bottom_left, 0.5 * pi, pi, @max(@as(f32, @floatFromInt(data.width.bottom)), @as(f32, @floatFromInt(data.width.left))), color);
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
            .custom => if (self.custom_element_renderer) |func| {
                try func(self, self.user_data, cmd);
            } else {
                log.err("Encountered CUSTOM render command but no custom renderer is set up; element will be discarded.", .{});
            },
        }
    }
}
