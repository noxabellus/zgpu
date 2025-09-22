//! An abstraction layer for rendering *interactive* UI layouts based on the clay layout engine, rendered with our Batch2D library.

const Ui = @This();

const std = @import("std");
const clay = @import("clay");
const glfw = @import("glfw");
const stbtt = @import("stbtt");

const Batch2D = @import("Batch2D.zig");
const AssetCache = @import("AssetCache.zig");
const InputState = @import("InputState.zig");

const log = std.log.scoped(.ui);

test {
    log.debug("semantic analysis for Ui.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const Vec2 = Batch2D.Vec2;
pub const Color = Batch2D.Color;

allocator: std.mem.Allocator,

renderer: *Batch2D,
asset_cache: *AssetCache,
inputs: *InputState,

clay_context: *clay.Context,
clay_memory: []const u8,

render_commands: ?[]clay.RenderCommand = null,
events: std.ArrayList(Event) = .empty,

custom_element_renderer: ?*const fn (*Ui, ?*anyopaque, clay.RenderCommand) anyerror!void = null,
user_data: ?*anyopaque = null,

state: State = .{},
last_state: State = .{},
wheel_delta: Vec2 = .{},

// A stack of all elements currently under the mouse, populated during layout. The last element is the top-most.
hovered_element_stack: std.ArrayList(StateElement) = .empty,
// Map from navigation index to element ID for quick lookup during keyboard navigation
navigable_elements: std.AutoHashMapUnmanaged(u32, struct { id: ElementId, state: ElementState }) = .empty,
// Map from ElementId.id to navigation index for reverse lookup during event generation
reverse_navigable_elements: std.AutoHashMapUnmanaged(u32, u32) = .empty,
// Stack of currently open element's user-provided stable IDs
open_ids: std.ArrayList(ElementId) = .empty,

pub fn init(allocator: std.mem.Allocator, renderer: *Batch2D, asset_cache: *AssetCache, inputs: *InputState) !*Ui {
    const self = try allocator.create(Ui);
    errdefer allocator.destroy(self);

    // Init Clay
    const min_memory_size = clay.minMemorySize();
    const clay_memory = try allocator.alloc(u8, min_memory_size);
    errdefer allocator.free(clay_memory);

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
        .allocator = allocator,
        .renderer = renderer,
        .asset_cache = asset_cache,
        .inputs = inputs,
        .clay_context = clay_context,
        .clay_memory = clay_memory,
    };

    return self;
}

pub fn deinit(self: *Ui) void {
    self.events.deinit(self.allocator);
    self.hovered_element_stack.deinit(self.allocator);
    self.navigable_elements.deinit(self.allocator);
    self.reverse_navigable_elements.deinit(self.allocator);
    self.open_ids.deinit(self.allocator);

    self.allocator.free(self.clay_memory);
    self.allocator.destroy(self);
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

    self.wheel_delta = self.inputs.consumeScrollDelta();

    clay.setCurrentContext(self.clay_context);

    clay.setLayoutDimensions(vec2ToDims(dimensions));

    // Although we inform Clay of current mouse button state, it is only used in debug mode or for drag scrolling.
    clay.setPointerState(vec2ToClay(self.inputs.getMousePosition()), self.inputs.getMouseButton(.left).isDown());

    clay.updateScrollContainers(
        false, // never use drag scrolling
        vec2ToClay(self.wheel_delta),
        delta_ms,
    );

    clay.beginLayout();
}

/// End the current layout declaration and finalize the render commands and events.
/// Must be called after `Ui.beginLayout` and before `Ui.render`.
pub fn endLayout(self: *Ui) ![]const Event {
    std.debug.assert(self.clay_context == clay.getCurrentContext());
    defer clay.setCurrentContext(null);

    const render_commands = clay.endLayout();
    self.render_commands = render_commands;

    try self.generateEvents();

    return self.events.items;
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

// --- Element Interface ---

/// Create a new, unconfigured element.
/// * Must be followed by calls to `Ui.configureElement` and `Ui.closeElement`.
/// * See also `Ui.openElement`, `Ui.elem`.
pub fn beginElement(self: *Ui, id: ElementId) !void {
    std.debug.assert(clay.getCurrentContext() == self.clay_context);

    try self.open_ids.append(self.allocator, id);

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

    try self.open_ids.append(self.allocator, declaration.id);

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

// --- Structures ---

pub const Event = struct {
    element_id: ElementId,
    bounding_box: BoundingBox,
    user_data: ?*anyopaque,
    data: Data,

    pub const Data = union(enum) {
        hover_begin: struct { mouse_position: Vec2 },
        hovering: struct { mouse_position: Vec2 },
        hover_end: struct { mouse_position: Vec2 },

        mouse_down: struct { mouse_position: Vec2 },
        mouse_up: struct { mouse_position: Vec2, end_element: ?ElementId },
        clicked: struct { mouse_position: Vec2 },

        wheel: struct { delta: Vec2 },

        focus_gained: void,
        focusing: void,
        focus_lost: void,

        activate_begin: void,
        activating: void,
        activate_end: struct { end_element: ?ElementId },
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

pub const ElementState = packed struct(usize) {
    event_flags: Flags,
    user_data: u48 = 0,

    pub const Flags = packed struct(u16) {
        hover: bool = false,
        wheel: bool = false,
        click: bool = false,
        focus: bool = false,
        activate: bool = false,

        _reserved: u11 = 0,

        pub const none = Flags{};
        pub const hoverable = Flags{ .hover = true };
        pub const scrollable = Flags{ .wheel = true };
        pub const clickable = Flags{ .click = true };
        pub const focusable = Flags{ .focus = true };
        pub const activatable = Flags{ .activate = true };
        pub const all = Flags{
            .hover = true,
            .wheel = true,
            .click = true,
            .focus = true,
            .activate = true,
        };

        pub fn merge(a: Flags, b: Flags) Flags {
            return Flags{
                .hover = a.hover or b.hover,
                .wheel = a.wheel or b.wheel,
                .click = a.click or b.click,
                .focus = a.focus or b.focus,
                .activate = a.activate or b.activate,
            };
        }

        pub fn takesInput(self: Flags) bool {
            return self.hover or self.wheel or self.click or self.focus or self.activate;
        }

        pub fn usesMouse(self: Flags) bool {
            return self.hover or self.wheel or self.click;
        }
    };

    pub const none = ElementState{ .event_flags = .none };
    pub const hoverable = ElementState{ .event_flags = .hoverable };
    pub const scrollable = ElementState{ .event_flags = .scrollable };
    pub const clickable = ElementState{ .event_flags = .clickable };
    pub const focusable = ElementState{ .event_flags = .focusable };
    pub const activatable = ElementState{ .event_flags = .activatable };
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
        try self.navigable_elements.put(self.allocator, index, .{ .id = declaration.id, .state = declaration.state });
        try self.reverse_navigable_elements.put(self.allocator, declaration.id.id, index);
    }

    // If the element is hovered, push it to our stack of hovered elements.
    if (clay.hovered()) {
        const hovered_data = clay.getElementData(declaration.id);
        if (hovered_data.found) {
            try self.hovered_element_stack.append(self.allocator, .{
                .id = declaration.id,
                .bounding_box = hovered_data.bounding_box,
                .state = declaration.state,
            });
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

    const font_info = &backend_ptr.asset_cache.fonts.items[config.font_id].info;
    const line_spacing_override = if (config.line_height > 0) config.line_height else null;

    // Use the Batch2D's own text measurement logic for perfect consistency.
    const dimensions = Batch2D.measureText(
        text_slice,
        font_info,
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

    const mouse_pos = self.inputs.getMousePosition();
    const left_button = self.inputs.getMouseButton(.left);
    const enter_key = self.inputs.getKey(.enter);

    // The top-most element in the stack is the one we consider "hovered" for this frame.
    if (self.hovered_element_stack.items.len > 0) {
        self.state.hovered = self.hovered_element_stack.items[self.hovered_element_stack.items.len - 1];
    }

    // --- Handle Hover Events ---
    // This logic compares the hovered element from the last frame to the current one.
    if (self.last_state.hovered) |last_hovered| {
        if (last_hovered.state.event_flags.hover) {
            if (last_hovered.id.id != self.state.hoveredIdValue()) { // Mouse moved out of the previously hovered element.
                try self.events.append(self.allocator, .{
                    .element_id = last_hovered.id,
                    .bounding_box = last_hovered.bounding_box,
                    .user_data = last_hovered.state.getUserData(),
                    .data = .{ .hover_end = .{ .mouse_position = clampToBox(last_hovered.bounding_box, mouse_pos) } },
                });
            } else { // Mouse remains over the previously hovered element.
                try self.events.append(self.allocator, .{
                    .element_id = last_hovered.id,
                    .bounding_box = last_hovered.bounding_box,
                    .user_data = last_hovered.state.getUserData(),
                    .data = .{ .hovering = .{ .mouse_position = mouse_pos } },
                });
            }
        }
    }

    if (self.state.hoveredIdValue() != self.last_state.hoveredIdValue()) {
        if (self.state.hovered) |new_hovered| { // A new element is now hovered.
            if (new_hovered.state.event_flags.hover) {
                try self.events.append(self.allocator, .{
                    .element_id = new_hovered.id,
                    .bounding_box = new_hovered.bounding_box,
                    .user_data = new_hovered.state.getUserData(),
                    .data = .{ .hover_begin = .{ .mouse_position = mouse_pos } },
                });
            }
        }
    }

    // --- Handle Active State and Focus-on-click ---
    // Determine the current active element based on continuous input state. Mouse input takes precedence.
    var is_mouse_activating = false;
    if (left_button.isDown()) {
        var found_click_target = false;

        // Iterate through the hovered stack from top to bottom to find the correct event target.
        for (self.hovered_element_stack.items, 0..) |_, i| {
            const hovered_state = self.hovered_element_stack.items[self.hovered_element_stack.items.len - 1 - i];

            // The first clickable or activatable element we find becomes active.
            if (!found_click_target and (hovered_state.state.event_flags.click or hovered_state.state.event_flags.activate)) {
                self.state.active_id = hovered_state;
                is_mouse_activating = true;
                found_click_target = true;
                break;
            }
        }
    }

    // If mouse isn't activating, check keyboard.
    if (!is_mouse_activating) {
        if (enter_key.isDown() and self.state.focused_id != null) {
            if (self.state.focused_id.?.state.event_flags.activate) {
                self.state.active_id = self.state.focused_id.?;
            } else {
                self.state.active_id = null;
            }
        } else {
            self.state.active_id = null;
        }
    }

    // Handle focus changes on mouse press
    if (left_button == .pressed) {
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
    if (left_button == .released and self.last_state.active_id != null) {
        const last_active = self.last_state.active_id.?;
        if (last_active.state.event_flags.click) {
            try self.events.append(self.allocator, .{
                .element_id = last_active.id,
                .bounding_box = last_active.bounding_box,
                .user_data = last_active.state.getUserData(),
                .data = .{ .mouse_up = .{ .mouse_position = mouse_pos, .end_element = self.state.hoveredId() } },
            });

            if (last_active.id.id == self.state.hoveredIdValue()) {
                try self.events.append(self.allocator, .{
                    .element_id = last_active.id,
                    .bounding_box = last_active.bounding_box,
                    .user_data = last_active.state.getUserData(),
                    .data = .{ .clicked = .{ .mouse_position = mouse_pos } },
                });
            }
        }
    }

    // --- Generate Activation and Mouse Down Events from State Changes ---
    if (self.state.activeIdValue() != self.last_state.activeIdValue()) {
        if (self.last_state.active_id) |last_active| {
            if (last_active.state.event_flags.activate) {
                try self.events.append(self.allocator, .{
                    .element_id = last_active.id,
                    .bounding_box = last_active.bounding_box,
                    .user_data = last_active.state.getUserData(),
                    .data = .{ .activate_end = .{ .end_element = self.state.hoveredId() } },
                });
            }
        }
        if (self.state.active_id) |new_active| {
            if (is_mouse_activating and new_active.state.event_flags.click) {
                try self.events.append(self.allocator, .{
                    .element_id = new_active.id,
                    .bounding_box = new_active.bounding_box,
                    .user_data = new_active.state.getUserData(),
                    .data = .{ .mouse_down = .{ .mouse_position = mouse_pos } },
                });
            }
            if (new_active.state.event_flags.activate) {
                try self.events.append(self.allocator, .{
                    .element_id = new_active.id,
                    .bounding_box = new_active.bounding_box,
                    .user_data = new_active.state.getUserData(),
                    .data = .activate_begin,
                });
            }
        }
    } else if (self.state.active_id) |active_elem| {
        if (active_elem.state.event_flags.activate) {
            try self.events.append(self.allocator, .{
                .element_id = active_elem.id,
                .bounding_box = active_elem.bounding_box,
                .user_data = active_elem.state.getUserData(),
                .data = .activating,
            });
        }
    }

    // --- Handle Keyboard Focus Events ---
    if (self.inputs.getKey(.tab) == .released and self.navigable_elements.count() > 0) {
        var current_index: ?u32 = null;
        if (self.state.focusedId()) |focused_id| {
            current_index = self.reverse_navigable_elements.get(focused_id.id);
        }

        const count = @as(u32, @intCast(self.navigable_elements.count()));
        var next_index: u32 = 0;
        if (self.inputs.getModifiers().shift) { // Shift+Tab: move focus backward.
            next_index = if (current_index) |idx| (idx + count - 1) % count else count - 1;
        } else { // Tab: move focus forward.
            next_index = if (current_index) |idx| (idx + 1) % count else 0;
        }

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

    // --- Generate Focus Change Events ---
    if (self.state.focusedIdValue() != self.last_state.focusedIdValue()) {
        if (self.last_state.focused_id) |last_focused| {
            try self.events.append(self.allocator, .{
                .element_id = last_focused.id,
                .bounding_box = last_focused.bounding_box,
                .user_data = last_focused.state.getUserData(),
                .data = .focus_lost,
            });
        }
        if (self.state.focused_id) |new_focused| {
            try self.events.append(self.allocator, .{
                .element_id = new_focused.id,
                .bounding_box = new_focused.bounding_box,
                .user_data = new_focused.state.getUserData(),
                .data = .focus_gained,
            });
        }
    }

    if (self.state.focused_id) |focused_elem| {
        try self.events.append(self.allocator, .{
            .element_id = focused_elem.id,
            .bounding_box = focused_elem.bounding_box,
            .user_data = focused_elem.state.getUserData(),
            .data = .focusing,
        });
    }

    // --- Handle Wheel Events ---
    if (self.wheel_delta.x != 0 or self.wheel_delta.y != 0) {
        // Iterate from the top-most hovered element downwards to find a wheel target.
        for (self.hovered_element_stack.items, 0..) |_, i| {
            const hovered_state = self.hovered_element_stack.items[self.hovered_element_stack.items.len - 1 - i];
            if (hovered_state.state.event_flags.wheel) {
                try self.events.append(self.allocator, .{
                    .element_id = hovered_state.id,
                    .bounding_box = hovered_state.bounding_box,
                    .user_data = hovered_state.state.getUserData(),
                    .data = .{ .wheel = .{ .delta = self.wheel_delta } },
                });
                // Once a wheel container is found, stop propagating the event.
                break;
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
                const font_info = &self.asset_cache.fonts.items[data.font_id].info;
                const text_slice = data.string_contents.chars[0..@intCast(data.string_contents.length)];
                const color = clayColorToBatchColor(data.text_color);
                const line_spacing_override = if (data.line_height > 0) data.line_height else null;

                // Clay provides the final bounding box after alignment. We can just draw at its top-left corner.
                try self.renderer.drawText(
                    text_slice,
                    font_info,
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
