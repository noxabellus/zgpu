// WIP: rearchitecting clay in ideomatic zig
const std = @import("std");

pub const vec2 = @Vector(2, f32);

pub const CharacterOffset = struct {
    // The x,y offset of the character relative to the top-left corner of the text element's content area.
    offset: vec2,
    // The height of the line the character is on. Useful for rendering a caret.
    line_height: f32,
    // Indicates whether a text element was found with the provided ID and the index was valid.
    found: bool,
};

pub const CharacterIndexResult = struct {
    // The zero-based index of the character in the original string that is closest to the provided offset.
    index: u32,
    // Indicates whether a text element was found and an index could be determined.
    found: bool,
};

pub const EnumBackingType = u8;

/// Clay String representation, not guaranteed to be null terminated
pub const String = struct {
    /// Set this boolean to true if the `chars: [*]const u8` data underlying this string will live for the entire lifetime of the program.
    /// This will automatically be set for strings created with CLAY_STRING, as the macro requires a string literal.
    is_statically_allocated: bool = true,
    slice: []const u8 = "",

    /// Converts a Zig comptime string slice to a Clay_String, see `fromRuntimeSlice` form non-comptime strings.
    pub fn fromSlice(comptime string: []const u8) String {
        return .{
            .is_statically_allocated = true,
            .chars = @ptrCast(@constCast(string)),
            .length = @intCast(string.len),
        };
    }

    /// Converts a Zig string slice to a Clay_String
    pub fn fromRuntimeSlice(string: []const u8) String {
        return .{ .is_statically_allocated = false, .chars = string };
    }
};

/// Clay StringSlice is used to represent non-owning string slices
/// Includes a baseChars field which points to the string this slice is derived from
pub const StringSlice = struct {
    value: []const u8,
    /// Pointer to the source string that this slice was derived from
    base: *const String,
};

/// Represents an RGBA color where components are in 0-255 range
/// order: r, g, b, a
pub const Color = [4]f32;

pub const BoundingBox = struct {
    /// X, Y coordinate of the top-left corner
    position: vec2 = .{ 0, 0 },
    /// Width, Height of the bounding box
    size: vec2 = .{ 0, 0 },
};

pub const SizingMinMax = struct {
    /// Element won't shrink below this size even if content is smaller
    min: f32 = 0,
    /// Content will wrap/overflow if larger than this size
    max: f32 = 0,
};

pub const SizingConstraint = union {
    /// Min/max sizing constraints in pixels
    minmax: SizingMinMax,
    /// Percentage of parent container size (0.0-1.0)
    percent: f32,
};

pub const SizingType = enum(EnumBackingType) {
    /// (default) Wraps tightly to the size of element's contents
    fit = 0,
    /// Expands to fill available space, sharing with other GROW elements
    grow = 1,
    /// Clamps size to a percent of parent (0.0-1.0 range)
    percent = 2,
    /// Clamps size to an exact size in pixels
    fixed = 3,
};

/// Controls the sizing of an element along one axis inside its parent container
pub const SizingAxis = struct {
    /// Size constraints
    size: SizingConstraint = .{ .minmax = .{} },
    /// How the element takes up space (fit, grow, fixed, percent)
    type: SizingType = .fit,

    /// Element will grow to fill available space
    pub const grow = SizingAxis{ .type = .grow, .size = .{ .minmax = .{ .min = 0, .max = 0 } } };

    /// Element will size to fit its child elements
    pub const fit = SizingAxis{ .type = .fit, .size = .{ .minmax = .{ .min = 0, .max = 0 } } };

    /// Element will grow to fill available space with min/max constraints
    pub fn growMinMax(size_minmax: SizingMinMax) SizingAxis {
        return .{ .type = .grow, .size = .{ .minmax = size_minmax } };
    }

    /// Element will fit its contents with min/max constraints
    pub fn fitMinMax(size_minmax: SizingMinMax) SizingAxis {
        return .{ .type = .fit, .size = .{ .minmax = size_minmax } };
    }

    /// Creates a fixed size element (min = max = size)
    pub fn fixed(size: f32) SizingAxis {
        return .{ .type = .fixed, .size = .{ .minmax = .{ .max = size, .min = size } } };
    }

    /// Creates a sizing that's a percentage of parent size (0.0-1.0)
    pub fn percent(size_percent: f32) SizingAxis {
        return .{ .type = .percent, .size = .{ .percent = size_percent } };
    }
};

pub const Sizing = struct {
    /// Width
    w: SizingAxis = .{},
    /// Height
    h: SizingAxis = .{},

    /// Grow to fill available space in both dimensions
    pub const grow = Sizing{ .grow, .grow };

    /// will size to fit its child elements in both dimensions
    pub const fit = Sizing{ .fit, .fit };
};

/// Controls "padding" in pixels, which is a gap between the element's bounding box
/// and where its children will be placed
pub const Padding = struct {
    /// Padding on left side
    left: u16 = 0,
    /// Padding on right side
    right: u16 = 0,
    /// Padding on top side
    top: u16 = 0,
    /// Padding on bottom side
    bottom: u16 = 0,

    pub const xy = @compileError("renamed to axes"); // TODO: remove this in v0.3.0

    /// Padding with vertical and horizontal values
    pub fn axes(top_bottom: u16, left_right: u16) Padding {
        return .{
            .top = top_bottom,
            .bottom = top_bottom,
            .left = left_right,
            .right = left_right,
        };
    }

    /// Equal padding on all sides
    pub fn all(size: u16) Padding {
        return Padding{
            .left = size,
            .right = size,
            .top = size,
            .bottom = size,
        };
    }
};

pub const TextElementConfigWrapMode = enum(EnumBackingType) {
    /// (default) Breaks text on whitespace characters
    words = 0,
    /// Don't break on space characters, only on newlines
    new_lines = 1,
    /// Disable text wrapping entirely
    none = 2,
};

pub const TextAlignment = enum(EnumBackingType) {
    /// (default) Aligns text to the left edge
    left = 0,
    /// Aligns text to the center
    center = 1,
    /// Aligns text to the right edge
    right = 2,
};

pub const TextElementConfig = struct {
    /// A pointer that will be transparently passed through to the resulting render command.
    user_data: ?*anyopaque = null,
    /// The RGBA color of the font to render, conventionally specified as 0-255.
    color: Color = .{ 0, 0, 0, 255 },
    /// An integer transparently passed to Clay_MeasureText to identify the font to use.
    /// The debug view will pass fontId = 0 for its internal text.
    font_id: u16 = 0,
    /// Controls the size of the font. Handled by the function provided to Clay_MeasureText.
    font_size: u16 = 20,
    /// Controls extra horizontal spacing between characters. Handled by the function provided to Clay_MeasureText.
    letter_spacing: u16 = 0,
    /// Additional vertical space between wrapped lines of text
    line_height: u16 = 0,
    /// Controls how text "wraps", that is how it is broken into multiple lines when there is insufficient horizontal space.
    wrap_mode: TextElementConfigWrapMode = .words,
    /// Controls how wrapped lines of text are horizontally aligned within the outer text bounding box.
    alignment: TextAlignment = .left,
};

pub const FloatingAttachPointType = enum(EnumBackingType) {
    left_top = 0,
    left_center = 1,
    left_bottom = 2,
    center_top = 3,
    center_center = 4,
    center_bottom = 5,
    right_top = 6,
    right_center = 7,
    right_bottom = 8,
};

pub const FloatingAttachPoints = struct {
    /// Controls the origin point on the floating element that attaches to its parent
    element: FloatingAttachPointType = .left_top,
    /// Controls the origin point on the parent element that the floating element attaches to
    parent: FloatingAttachPointType = .left_top,
};

pub const FloatingAttachToElement = enum(EnumBackingType) {
    /// (default) Disables floating for this element
    none = 0,
    /// Attaches to parent, positioned based on attachPoints and offset
    parent = 1,
    /// Attaches to element with specific ID (specified with parentId field)
    element_with_id = 2,
    /// Attaches to the root of the layout (similar to absolute positioning)
    root = 3,
};

pub const FloatingClipToElement = enum(EnumBackingType) {
    /// (default) - The floating element does not inherit clipping.
    none = 0,
    /// The floating element is clipped to the same clipping rectangle as the element it's attached to.
    attached_parent = 1,
};

/// Controls how pointer events are handled by floating elements
pub const PointerCaptureMode = enum(EnumBackingType) {
    /// (default) Captures pointer events and doesn't pass through to elements underneath
    capture = 0,
    /// Transparently passes through pointer events to elements underneath
    passthrough = 1,
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
    attach_points: FloatingAttachPoints = .{ .element = .left_top, .parent = .left_top },
    /// Controls whether pointer events are captured or pass through to elements underneath
    pointer_capture_mode: PointerCaptureMode = .capture,
    /// Controls which element this floating element is attached to
    attach_to: FloatingAttachToElement = .none,
    /// Controls whether or not a floating element is clipped to the same clipping rectangle as the element it's attached to.
    clip_to: FloatingClipToElement = .none,
};

pub const RenderCommandType = enum(EnumBackingType) {
    /// This command type should be skipped
    none = 0,
    /// Draw a solid color rectangle
    rectangle = 1,
    /// Draw a colored border inset into the bounding box
    border = 2,
    /// Draw text
    text = 3,
    /// Draw an image
    image = 4,
    /// Begin clipping (scissor) - render only content within the boundingBox
    scissor_start = 5,
    /// End clipping - resume rendering elements without restriction
    scissor_end = 6,
    /// Custom implementation based on the render command's customData
    custom = 7,
};

pub const PointerDataInteractionState = enum(EnumBackingType) {
    /// A mouse click or touch occurred this frame
    pressed_this_frame = 0,
    /// Mouse button or touch is currently held down
    pressed = 1,
    /// Mouse button or touch was released this frame
    released_this_frame = 2,
    /// Mouse button or touch is not currently down
    released = 3,
};

pub const PointerData = struct {
    /// Position of the mouse/touch relative to the root of the layout
    position: vec2 = .{},
    state: PointerDataInteractionState = .released,
};

/// Controls corner rounding of elements (rectangles, borders, images)
pub const CornerRadius = struct {
    top_left: f32 = 0,
    top_right: f32 = 0,
    bottom_left: f32 = 0,
    bottom_right: f32 = 0,

    /// Sets all corners to the same radius
    pub fn all(radius: f32) CornerRadius {
        return .{
            .top_left = radius,
            .top_right = radius,
            .bottom_left = radius,
            .bottom_right = radius,
        };
    }
};

/// Identifies a UI element for interaction and lookups
pub const ElementId = struct {
    /// The resulting hash generated from the other fields
    id: u32 = 0,
    /// Numerical offset applied after computing the hash
    offset: u32 = 0,
    /// Base hash value to start from (e.g., parent element ID for local IDs)
    base_id: u32 = 0,
    /// The string ID to hash
    string_id: String = .{},

    /// Creates an ElementId with no metadata from the given raw ID.
    pub fn fromRawId(id: u32) ElementId {
        return .{ .id = id, .offset = 0, .base_id = 0, .string_id = .{} };
    }

    /// Creates a global element ID from a string
    pub fn fromSlice(string: []const u8) ElementId {
        return hash.string(.fromRuntimeSlice(string), 0, 0); // TODO move hashing to zig side for performance
    }

    /// Creates a global element ID with an index component for use in loops
    /// Equivalent to `ID("prefix0")`, `ID("prefix1")`, etc. without string allocations
    pub fn indexed(string: []const u8, index: u32) ElementId {
        return hash.string(.fromRuntimeSlice(string), index, 0);
    }

    /// Creates a local element ID from a string
    /// Local IDs are scoped to the current parent element
    pub fn localID(ctx: *Context, string: []const u8) ElementId {
        return hash.string(.fromRuntimeSlice(string), 0, ctx.getParentElementId());
    }

    /// Creates a local element ID from a string with index
    /// Local IDs are scoped to the current parent element
    pub fn indexedLocal(ctx: *Context, string: []const u8, index: u32) ElementId {
        return hash.string(.fromRuntimeSlice(string), index, ctx.getParentElementId());
    }

    /// Creates a global element ID from a source location (@src())
    /// Useful for auto-generating unique IDs based on code location
    pub fn fromSrc(comptime src: std.builtin.SourceLocation) ElementId {
        return hash.string(.fromSlice(src.module ++ ":" ++ src.file ++ ":" ++ std.fmt.comptimePrint("{}", .{src.column})), 0, 0);
    }

    /// Creates a global element ID from a source location (@src()) with an index
    /// Useful for auto-generating unique IDs based on code location in loops
    pub fn indexedFromSrc(comptime src: std.builtin.SourceLocation, index: u32) ElementId {
        return hash.string(.fromSlice(src.module ++ ":" ++ src.file ++ ":" ++ std.fmt.comptimePrint("{}", .{src.column})), index, 0);
    }
};

/// Represents a single render command to be processed by a renderer
pub const RenderCommand = struct {
    /// Rectangular box that fully encloses this UI element
    bounding_box: BoundingBox,
    /// Data specific to this command's type
    render_data: RenderData,
    /// Pointer passed through from the original element declaration
    user_data: ?*anyopaque,
    /// ID of this element
    id: u32,
    /// Z-order for correct drawing (commands are already sorted in ascending order)
    z_index: i16,
    /// Specifies how to handle rendering of this command
    command_type: RenderCommandType,
};

pub const LayoutDirection = enum(EnumBackingType) {
    /// (default) Lays out children from left to right with increasing x
    left_to_right = 0,
    /// Lays out children from top to bottom with increasing y
    top_to_bottom = 1,
};

pub const LayoutAlignmentX = enum(EnumBackingType) {
    /// (default) Aligns children to the left, offset by padding.left
    left = 0,
    /// Aligns children to the right, offset by padding.right
    right = 1,
    /// Aligns children horizontally to the center
    center = 2,
};

pub const LayoutAlignmentY = enum(EnumBackingType) {
    /// (default) Aligns children to the top, offset by padding.top
    top = 0,
    /// Aligns children to the bottom, offset by padding.bottom
    bottom = 1,
    /// Aligns children vertically to the center
    center = 2,
};

pub const ChildAlignment = struct {
    x: LayoutAlignmentX = .left,
    y: LayoutAlignmentY = .top,

    /// Centers children on both axes
    pub const center = ChildAlignment{ .center, .center };
};

pub const LayoutConfig = struct {
    /// Controls sizing of this element inside its parent container
    sizing: Sizing = .{},
    /// Controls gap between element bounds and where children are placed
    padding: Padding = .{},
    /// Controls gap between child elements along layout axis
    child_gap: u16 = 0,
    /// Controls how child elements are aligned on each axis
    child_alignment: ChildAlignment = .{},
    /// Controls the direction of children's layout
    direction: LayoutDirection = .left_to_right,
};

pub const BorderWidth = struct {
    /// Width of left border in pixels
    left: u16 = 0,
    /// Width of right border in pixels
    right: u16 = 0,
    /// Width of top border in pixels
    top: u16 = 0,
    /// Width of bottom border in pixels
    bottom: u16 = 0,
    /// Width of borders between child elements
    between_children: u16 = 0,

    /// Creates borders on all outer edges (not between children)
    pub fn all(width: u16) BorderWidth {
        return .{
            .left = width,
            .right = width,
            .top = width,
            .bottom = width,
            .between_children = 0,
        };
    }

    /// Creates borders on all edges, including between children
    pub fn between(width: u16) BorderWidth {
        return .{
            .left = width,
            .right = width,
            .top = width,
            .bottom = width,
            .between_children = width,
        };
    }
};

pub const BorderElementConfig = struct {
    /// Color of all borders with width > 0
    color: Color = .{ 0, 0, 0, 255 },
    /// Widths of individual borders
    width: BorderWidth = .{},
};

pub const TextRenderData = struct {
    /// Text to be rendered
    string_contents: StringSlice,
    /// Color of the text (0-255 range)
    text_color: Color,
    /// Font identifier passed to the text measurement function
    font_id: u16,
    /// Size of the font
    font_size: u16,
    /// Extra space between characters
    letter_spacing: u16,
    /// Height of this line of text
    line_height: u16,
};

pub const RectangleRenderData = struct {
    /// Fill color for the rectangle
    background_color: Color,
    /// Corner rounding for the rectangle
    corner_radius: CornerRadius,
};

pub const ImageRenderData = struct {
    /// Tint color for the image (0,0,0,0 = untinted)
    background_color: Color,
    /// Corner rounding for the image
    corner_radius: CornerRadius,
    /// Transparent pointer to image data
    image_data: usize,
};

pub const CustomRenderData = struct {
    /// Background color passed from the element declaration
    background_color: Color,
    /// Corner rounding for the custom element
    corner_radius: CornerRadius,
    /// Transparent pointer from the element declaration
    custom_data: ?*anyopaque,
};

pub const AspectRatioElementConfig = struct {
    /// A float representing the target "Aspect ratio" for an element, which is its final width divided by its final height.
    aspect_ratio: f32 = 0,
};

pub const ImageElementConfig = struct {
    /// Transparent pointer to image data
    image_data: usize = 0,
};

/// Render command data for scissor (clipping) commands
pub const ClipRenderData = struct {
    /// Whether to clip/scroll horizontally
    horizontal: bool,
    /// Whether to clip/scroll vertically
    vertical: bool,
};

pub const BorderRenderData = struct {
    /// Color of all borders
    color: Color,
    /// Corner rounding for the borders
    corner_radius: CornerRadius,
    /// Widths of individual borders
    width: BorderWidth,
};

pub const RenderData = union {
    rectangle: RectangleRenderData,
    text: TextRenderData,
    image: ImageRenderData,
    custom: CustomRenderData,
    border: BorderRenderData,
    scroll: ClipRenderData,
};

/// Configuration for custom elements
pub const CustomElementConfig = struct {
    /// Transparent pointer for passing custom data to the renderer
    custom_data: ?*anyopaque = null,
};

/// Data representing the current internal state of a scrolling element
pub const ScrollContainerData = struct {
    /// Pointer to the internal scroll position (mutable)
    /// Modifying this will change the actual scroll position
    scroll_position: *vec2,
    /// Bounding box of the scroll container
    scroll_container_dimensions: vec2,
    /// Dimensions of the inner content, including parent padding
    content_dimensions: vec2,
    /// Original scroll config
    config: ClipElementConfig,
    /// Whether a scroll container was found with the provided ID
    found: bool,
};

/// Bounding box and other data for a specific UI element
pub const ElementData = struct {
    /// Rectangle enclosing this element, position relative to layout root
    bounding_box: BoundingBox,
    /// Whether an element with the provided ID was found
    found: bool,
};

/// Controls the axes on which an element can scroll
pub const ClipElementConfig = struct {
    /// Whether to enable horizontal scrolling
    horizontal: bool = false,
    /// Whether to enable vertical scrolling
    vertical: bool = false,
    // Offsets the x,y positions of all child elements. Used primarily for scrolling containers.
    child_offset: vec2 = .{ 0, 0 },
};

/// Shared configuration properties for multiple element types
pub const SharedElementConfig = struct {
    /// Background color of the element
    background_color: Color,
    /// Corner rounding of the element
    corner_radius: CornerRadius,
    /// Transparent pointer passed to render commands
    user_data: ?*anyopaque,
};

/// Element configuration type identifiers
pub const ElementConfigType = std.meta.Tag(ElementConfig);

pub const ElementConfigRaw = union {
    text_element: *TextElementConfig,
    aspect_ratio_element: *AspectRatioElementConfig,
    image_element: *ImageElementConfig,
    floating_element: *FloatingElementConfig,
    custom_element: *CustomElementConfig,
    clip_element: *ClipElementConfig,
    border_element: *BorderElementConfig,
    shared_element: *SharedElementConfig,
};

pub const ElementConfig = union(enum(u8)) {
    text_element: *TextElementConfig,
    aspect_ratio_element: *AspectRatioElementConfig,
    image_element: *ImageElementConfig,
    floating_element: *FloatingElementConfig,
    custom_element: *CustomElementConfig,
    clip_element: *ClipElementConfig,
    border_element: *BorderElementConfig,
    shared_element: *SharedElementConfig,
};

pub const ElementDeclaration = struct {
    /// Element IDs have two main use cases.
    ///
    /// Firstly, tagging an element with an ID allows you to query information about the element later, such as its mouseover state or dimensions.
    ///
    /// Secondly, IDs are visually useful when attempting to read and modify UI code, as well as when using the built-in debug tools.
    id: ElementId = .{ .base_id = 0, .id = 0, .offset = 0, .string_id = .{ .chars = &.{}, .length = 0, .is_statically_allocated = false } },
    /// Controls various settings that affect the size and position of an element, as well as the sizes and positions of any child elements.
    layout: LayoutConfig = .{},
    /// Controls the background color of the resulting element.
    /// By convention specified as 0-255, but interpretation is up to the renderer.
    /// If no other config is specified, `.background_color` will generate a `RECTANGLE` render command, otherwise it will be passed as a property to `IMAGE` or `CUSTOM` render commands.
    background_color: Color = .{ 0, 0, 0, 0 },
    /// Controls the "radius", or corner rounding of elements, including rectangles, borders and images.
    corner_radius: CornerRadius = .{},
    // Controls settings related to aspect ratio scaling.
    aspect_ratio: AspectRatioElementConfig = .{},
    /// Controls settings related to image elements.
    image: ImageElementConfig = .{},
    /// Controls whether and how an element "floats", which means it layers over the top of other elements in z order, and doesn't affect the position and size of siblings or parent elements.
    /// Note: in order to activate floating, `.floating.attachTo` must be set to something other than the default value.
    floating: FloatingElementConfig = .{},
    /// Used to create CUSTOM render commands, usually to render element types not supported by Clay.
    custom: CustomElementConfig = .{},
    /// Controls whether an element should clip its contents and allow scrolling rather than expanding to contain them.
    clip: ClipElementConfig = .{},
    /// Controls settings related to element borders, and will generate BORDER render command
    border: BorderElementConfig = .{},
    /// A pointer that will be transparently passed through to resulting render command
    user_data: ?*anyopaque = null,
};

const DebugElementData = packed struct {
    collision: bool = false,
    collapsed: bool = false,
};

const WrappedTextLine = struct {
    dimensions: vec2 = .{ 0, 0 },
    line: String = .{},
};

const TextElementData = struct {
    text: String = .{},
    preferred_dimensions: vec2 = .{},
    index: u32 = 0,
    wrapped_lines: []WrappedTextLine = &.{},
};

const LayoutElementChildren = []u32;

const LayoutElementPayload = union {
    children: LayoutElementChildren,
    text_element_data: *TextElementData,
};

const LayoutElement = struct {
    payload: LayoutElementPayload = .{ .children = &.{} },
    dimensions: vec2 = .{},
    min_dimensions: vec2 = .{},
    layout_config: *LayoutConfig,
    element_configs: []ElementConfig = &.{},
    id: u32 = 0,
};

const ScrollContainerDataInternal = struct {
    layout_element: *LayoutElement,
    bounding_box: BoundingBox = .{},
    content_size: vec2 = .{ 0, 0 },
    scroll_origin: vec2 = .{ 0, 0 },
    pointer_origin: vec2 = .{ 0, 0 },
    scroll_momentum: vec2 = .{ 0, 0 },
    scroll_position: vec2 = .{ 0, 0 },
    previous_delta: vec2 = .{ 0, 0 },
    momentum_time: f32 = 0.0,
    element_id: u32 = 0,
    open_this_frame: bool = false,
    pointer_scroll_active: bool = false,
};

const LayoutElementHashMapItem = struct { // todo get this struct into a single cache line
    bounding_box: BoundingBox = .{},
    element_id: ElementId = .{},
    on_hover_function: ?*const fn (elementId: ElementId, pointerInfo: PointerData, userData: ?*anyopaque) void = null,
    hover_function_user_data: ?*anyopaque = null,
    next_index: u32 = 0,
    generation: u32 = 0,
    id_alias: u32 = 0,
    layout_element: *LayoutElement,
    debug_data: *DebugElementData,
};

const MeasuredWord = struct {
    start_offset: u32 = 0,
    length: u32 = 0,
    width: f32 = 0,
    next: u32 = 0,
};

const MeasureTextCacheItem = struct {
    unwrapped_dimensions: vec2 = .{ 0, 0 },
    measured_words_start_index: u32 = 0,
    min_width: f32 = 0,
    contains_newlines: bool = false,
    id: u32 = 0,
    next_index: u32 = 0,
    generation: u32 = 0,
};

const LayoutElementTreeNode = struct {
    layout_element: *LayoutElement,
    position: vec2 = .{ 0, 0 },
    next_child_offset: vec2 = .{ 0, 0 },
};

const LayoutElementTreeRoot = struct {
    layout_element_index: u32 = 0,
    parent_id: u32 = 0,
    clip_element_id: u32 = 0,
    z_index: i16 = 0,
    pointer_offset: vec2 = .{ 0, 0 },
};

pub const Context = struct {
    internal_arena: std.heap.ArenaAllocator,

    max_element_count: u32 = DEFAULT_MAX_ELEMENT_COUNT,
    max_measure_text_cache_word_count: u32 = DEFAULT_MAX_MEASURE_TEXT_WORD_CACHE_COUNT,
    warnings_enabled: bool = true,
    pointer_info: PointerData = .{},
    layout_dimensions: vec2 = .{ 0, 0 },
    dynamic_element_index_base_hash: ElementId = .{},
    dynamic_element_index: u32 = 0,
    debug_mode_enabled: bool = false,
    disable_culling: bool = false,
    external_scroll_handling_enabled: bool = false,
    debug_selected_element_id: u32 = 0,
    generation: u32 = 0,
    measure_text_callback: ?*const fn ([]const u8, *TextElementConfig, user_data: ?*anyopaque) vec2 = null,
    measure_text_user_data: ?*anyopaque = null,
    query_scroll_offset_user_data: ?*anyopaque = null,
    // Layout Elements / Render Commands
    layout_elements: std.ArrayList(LayoutElement) = .empty,
    render_commands: std.ArrayList(RenderCommand) = .empty,
    open_layout_element_stack: std.ArrayList(u32) = .empty,
    layout_element_children: std.ArrayList(u32) = .empty,
    layout_element_children_buffer: std.ArrayList(u32) = .empty,
    text_element_data: std.ArrayList(TextElementData) = .empty,
    aspect_ratio_element_indexes: std.ArrayList(u32) = .empty,
    reusable_element_index_buffer: std.ArrayList(u32) = .empty,
    layout_element_clip_element_ids: std.ArrayList(u32) = .empty,
    // Configs
    layout_configs: std.ArrayList(LayoutConfig) = .empty,
    element_configs: std.ArrayList(ElementConfig) = .empty,
    text_element_configs: std.ArrayList(TextElementConfig) = .empty,
    aspect_ratio_element_configs: std.ArrayList(AspectRatioElementConfig) = .empty,
    image_element_configs: std.ArrayList(ImageElementConfig) = .empty,
    floating_element_configs: std.ArrayList(FloatingElementConfig) = .empty,
    clip_element_configs: std.ArrayList(ClipElementConfig) = .empty,
    custom_element_configs: std.ArrayList(CustomElementConfig) = .empty,
    border_element_configs: std.ArrayList(BorderElementConfig) = .empty,
    shared_element_configs: std.ArrayList(SharedElementConfig) = .empty,
    // Misc Data Structures
    layout_element_id_strings: std.ArrayList(String) = .empty,
    wrapped_text_lines: std.ArrayList(WrappedTextLine) = .empty,
    layout_element_tree_node_array: std.ArrayList(LayoutElementTreeNode) = .empty,
    layout_element_tree_roots: std.ArrayList(LayoutElementTreeRoot) = .empty,
    layout_elements_hash_map: std.AutoHashMapUnmanaged(u32, LayoutElementHashMapItem) = .empty,
    measure_text_hash_map: std.AutoHashMapUnmanaged(u32, MeasureTextCacheItem) = .empty,
    measured_words: std.ArrayList(MeasuredWord) = .empty,
    measured_words_free_list: std.ArrayList(u32) = .empty,
    open_clip_element_stack: std.ArrayList(u32) = .empty,
    pointer_over_ids: std.ArrayList(ElementId) = .empty,
    scroll_container_datas: std.ArrayList(ScrollContainerDataInternal) = .empty,
    tree_node_visited: std.ArrayList(bool) = .empty,
    dynamic_string_data: std.ArrayList(u8) = .empty,
    debug_element_data: std.ArrayList(DebugElementData) = .empty,

    pub fn init(allocator: std.mem.Allocator) error{OutOfMemory}!*Context {
        const self = try allocator.create(Context);
        self.* = .{
            .internal_arena = .init(allocator),
        };
        return self;
    }

    pub fn deinit(self: *Context) void {
        const allocator = self.internal_arena.child_allocator;
        self.internal_arena.deinit();
        allocator.destroy(self);
    }
};

fn addMeasuredWord(ctx: *Context, word: MeasuredWord, prev: *MeasuredWord) error{OutOfMemory}!*MeasuredWord {
    const index, const out =
        if (ctx.measured_words_free_list.pop()) |index| .{
            index,
            &ctx.measured_words[index],
        } else .{
            ctx.measured_words.items.len,
            try ctx.measured_words.addOneBounded(),
        };

    out.* = word;
    prev.next = index;

    return out;
}

fn writeStringToCharBuffer(buffer: *std.ArrayList(u8), string: String) error{OutOfMemory}!String {
    const out = buffer.addManyAsSliceBounded(string.slice.len);
    @memcpy(out, string.slice);
    return String{ .is_statically_allocated = false, .slice = out };
}

fn getOpenLayoutElement(ctx: *Context) error{InvalidLayout}!*LayoutElement {
    return if (ctx.open_layout_element_stack.items.len > 0)
        &ctx.layout_elements[ctx.open_layout_element_stack.items[ctx.open_layout_element_stack.items.len - 1]]
    else
        error.InvalidLayout;
}

fn getParentElementId(ctx: *Context) error{InvalidLayout}!u32 {
    return if (ctx.open_layout_element_stack.items.len > 1)
        ctx.layout_elements[ctx.open_layout_element_stack.items[ctx.open_layout_element_stack.items.len - 2]].id
    else
        error.InvalidLayout;
}

fn storeLayoutConfig(ctx: *Context, config: LayoutConfig) error{OutOfMemory}!*LayoutConfig {
    const out = try ctx.layout_configs.addOneBounded();
    out.* = config;
    return out;
}

fn storeTextElementConfig(ctx: *Context, config: TextElementConfig) error{OutOfMemory}!*TextElementConfig {
    const out = try ctx.text_element_configs.addOneBounded();
    out.* = config;
    return out;
}

fn storeAspectRatioElementConfig(ctx: *Context, config: AspectRatioElementConfig) error{OutOfMemory}!*AspectRatioElementConfig {
    const out = try ctx.aspect_ratio_element_configs.addOneBounded();
    out.* = config;
    return out;
}

fn storeImageElementConfig(ctx: *Context, config: ImageElementConfig) error{OutOfMemory}!*ImageElementConfig {
    const out = try ctx.image_element_configs.addOneBounded();
    out.* = config;
    return out;
}

fn storeFloatingElementConfig(ctx: *Context, config: FloatingElementConfig) error{OutOfMemory}!*FloatingElementConfig {
    const out = try ctx.floating_element_configs.addOneBounded();
    out.* = config;
    return out;
}

fn storeCustomElementConfig(ctx: *Context, config: CustomElementConfig) error{OutOfMemory}!*CustomElementConfig {
    const out = try ctx.custom_element_configs.addOneBounded();
    out.* = config;
    return out;
}

fn storeClipElementConfig(ctx: *Context, config: ClipElementConfig) error{OutOfMemory}!*ClipElementConfig {
    const out = try ctx.clip_element_configs.addOneBounded();
    out.* = config;
    return out;
}

fn storeBorderElementConfig(ctx: *Context, config: BorderElementConfig) error{OutOfMemory}!*BorderElementConfig {
    const out = try ctx.border_element_configs.addOneBounded();
    out.* = config;
    return out;
}

fn storeSharedElementConfig(ctx: *Context, config: SharedElementConfig) error{OutOfMemory}!*SharedElementConfig {
    const out = try ctx.shared_element_configs.addOneBounded();
    out.* = config;
    return out;
}

fn attachElementConfig(ctx: *Context, config: ElementConfig) error{ OutOfMemory, InvalidLayout }!*ElementConfig {
    const el = try getOpenLayoutElement(ctx);
    const out = try ctx.element_configs.addOneBounded();
    out.* = config;
    el.element_configs.len += 1; // this is ok because we point into this section of the list
    return out;
}

fn findElementConfigWithType(element: *LayoutElement, config_type: ElementConfigType) ?*ElementConfig {
    for (element.element_configs) |*config| {
        if (config.* == config_type) {
            return config;
        }
    }
    return null;
}



fn measureTextCached(ctx: *Context, text: *const String, config: *const TextElementConfig) error{MissingMeasureTextCallback}!*MeasureTextCacheItem {
    const callback = ctx.measure_text_callback orelse {
        ctx.boolean_warnings.text_measurement_function_not_set = true;
        return error.MissingMeasureTextCallback;
    };
    const id = hash.stringConfig(text, config);
    if (ctx.measure_text_hash_map.getPtr(id)) |item| {
        if (item.id)
    }
}



pub const DEFAULT_MAX_ELEMENT_COUNT = 8192;
pub const DEFAULT_MAX_MEASURE_TEXT_WORD_CACHE_COUNT = 16384;

pub const hash = struct {
    pub fn number(offset: u32, seed: u32) ElementId {
        var out = seed;

        out += (offset + 48);
        out += (out << 10);
        out ^= (out >> 6);

        out += (out << 3);
        out ^= (out >> 11);
        out += (out << 15);

        return ElementId{ // Reserve the hash result of zero as "null id"
            .id = out + 1,
            .offset = offset,
            .base_id = seed,
            .string_id = .{},
        };
    }

    pub fn string(key: String, offset: u32, seed: u32) ElementId {
        var out: u32 = 0;
        var base: u32 = seed;

        for (0..key.length) |i| {
            base += key.chars[i];
            base += (base << 10);
            base ^= (base >> 6);
        }
        out = base;
        out += offset;
        out += (out << 10);
        out ^= (out >> 6);

        out += (out << 3);
        base += (base << 3);
        out ^= (out >> 11);
        base ^= (base >> 11);
        out += (out << 15);
        base += (base << 15);

        return ElementId{ // Reserve the hash result of zero as "null id"
            .id = out + 1,
            .offset = offset,
            .baseId = base + 1,
            .stringId = key,
        };
    }

    pub fn data(value: []const u8) u64 {
        var out: u64 = 0;

        for (value) |b| {
            out += b;
            out += (out << 10);
            out ^= (out >> 6);
        }

        return out;
    }

    pub fn stringConfig(text: *const String, config: *const TextElementConfig) u32 {
        var out: u32 = 0;
        if (text.is_statically_allocated) {
            out += text.chars;
            out += (out << 10);
            out ^= (out >> 6);
            out += text.length;
            out += (out << 10);
            out ^= (out >> 6);
        } else {
            out = @intCast(data(text.chars[0..text.length]) % std.math.maxInt(u32));
        }

        out += config.font_id;
        out += (out << 10);
        out ^= (out >> 6);

        out += config.font_size;
        out += (out << 10);
        out ^= (out >> 6);

        out += config.letter_spacing;
        out += (out << 10);
        out ^= (out >> 6);

        out += (out << 3);
        out ^= (out >> 11);
        out += (out << 15);

        return out + 1; // Reserve the hash result of zero as "null id"
    }
};
