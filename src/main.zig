//! An example of the new UI wrapper over Ui.

const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");

const wgpu = @import("wgpu");
const stbi = @import("stbi");
const glfw = @import("glfw");
const clay = @import("clay");

pub const debug = @import("debug.zig");
pub const Batch2D = @import("Batch2D.zig");
pub const AssetCache = @import("AssetCache.zig");
pub const InputState = @import("InputState.zig");
pub const BindingState = @import("BindingState.zig");
pub const Ui = @import("Ui.zig");

test {
    log.debug("semantic analysis for main.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const std_options = std.Options{
    .log_level = .info,
};

const MSAA_SAMPLE_COUNT: u32 = 4;

// --- Global Application State ---

// WGPU and Window Management
const Demo = struct {
    instance: wgpu.Instance = null,
    surface: wgpu.Surface = null,
    adapter: wgpu.Adapter = null,
    device: wgpu.Device = null,
    config: wgpu.SurfaceConfiguration = .{},
    renderer: *Batch2D = undefined,
    msaa_texture: wgpu.Texture = null,
    msaa_view: wgpu.TextureView = null,
};

var FONT_ID_BODY: AssetCache.FontId = undefined;
var FONT_ID_TITLE: AssetCache.FontId = undefined;
var FONT_ID_MONO: AssetCache.FontId = undefined;

var zig_logo_image_id: AssetCache.ImageId = undefined;
var wgpu_logo_image_id: AssetCache.ImageId = undefined;

const COLOR_LIGHT = Ui.Color.init(244, 235, 230, 255);
const COLOR_LIGHT_HOVER = Ui.Color.init(224, 215, 210, 255);
const COLOR_BUTTON_HOVER = Ui.Color.init(238, 227, 225, 255);
const COLOR_BROWN = Ui.Color.init(61, 26, 5, 255);
const COLOR_RED = Ui.Color.init(168, 66, 28, 255);
const COLOR_RED_HOVER = Ui.Color.init(148, 46, 8, 255);
const COLOR_ORANGE = Ui.Color.init(225, 138, 50, 255);
const COLOR_BLUE = Ui.Color.init(111, 173, 162, 255);
const COLOR_TEAL = Ui.Color.init(111, 173, 162, 255);
const COLOR_BLUE_DARK = Ui.Color.init(2, 32, 82, 255);
const COLOR_NONE = Ui.Color.init(0, 0, 0, 255);
const COLOR_WHITE = Ui.Color.init(255, 255, 255, 255);

const test_text_default = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla feugiat convallis viverra.\nNulla luctus odio arcu. Cras pellentesque vitae lorem vel egestas.\n";

// --- Double-buffered text state for safe immediate-mode UI updates ---
var text_buffers: [2]std.ArrayList(u8) = .{ .empty, .empty };
var current_buffer_idx: u2 = 0;
var text_was_modified_last_frame: bool = false;

fn currentText() *std.ArrayList(u8) {
    return &text_buffers[current_buffer_idx];
}
fn nextText() *std.ArrayList(u8) {
    return &text_buffers[1 - current_buffer_idx];
}
// ---

var carets: std.ArrayList(Caret) = .empty;

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
var selection_render_data = SelectionRenderData{
    .carets = &carets,
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
    gpa: std.mem.Allocator,
    action: enum { insert, delete, backspace },
    payload: []const u8,
) !void {
    try sortAndMergeCarets(&carets);

    var new_text = std.ArrayList(u8).empty;
    var new_carets = std.ArrayList(Caret).empty;
    errdefer {
        new_text.deinit(gpa);
        new_carets.deinit(gpa);
    }

    const source_text = currentText().items;

    var last_idx: u32 = 0;
    for (carets.items) |caret| {
        if (caret.hasSelection()) {
            // Copy text before the current selection
            try new_text.appendSlice(gpa, source_text[last_idx..caret.min()]);
            // For inserts (like paste), add the payload here. For deletes, do nothing.
            if (action == .insert) {
                try new_text.appendSlice(gpa, payload);
            }
            // Advance the cursor in the old text string past the selection
            last_idx = caret.max();
        } else { // No selection, just a cursor.
            switch (action) {
                .insert => {
                    // Copy up to the cursor, insert payload, and update last_idx
                    try new_text.appendSlice(gpa, source_text[last_idx..caret.end]);
                    try new_text.appendSlice(gpa, payload);
                    last_idx = caret.end;
                },
                .delete => {
                    // Copy up to the cursor
                    try new_text.appendSlice(gpa, source_text[last_idx..caret.end]);
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
                    try new_text.appendSlice(gpa, source_text[last_idx..delete_start]);
                    // Advance last_idx past the cursor to effectively delete the character
                    last_idx = caret.end;
                },
            }
        }
        // Add a new, collapsed caret at the end of the modified section
        const new_pos: u32 = @intCast(new_text.items.len);
        try new_carets.append(gpa, .{ .start = new_pos, .end = new_pos });
    }

    // Append any remaining text after the last caret
    try new_text.appendSlice(gpa, source_text[last_idx..]);

    // Apply changes to the 'next' buffer and set the modified flag
    var target_buffer = nextText();
    target_buffer.deinit(gpa);
    target_buffer.* = new_text;

    carets.deinit(gpa);
    carets = new_carets;

    text_was_modified_last_frame = true;
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

fn createLayout(ui: *Ui) !void {
    try ui.openElement(.{
        .id = .fromSlice("OuterContainer"),
        .layout = .{
            .sizing = .grow,
            .direction = .top_to_bottom,
            .child_alignment = .center,
        },
        .background_color = COLOR_LIGHT,
    });
    defer ui.closeElement();

    {
        try ui.beginElement(.fromSlice("TextInputTest"));
        defer ui.closeElement();

        try ui.configureElement(.{
            .layout = .{
                .sizing = .{ .w = .fixed(300 + 5 * 2 + 2 * 2), .h = .fixed(16 * 6 + 5 * 2 + 2 * 2) },
                .padding = .all(5),
            },
            .background_color = COLOR_WHITE,
            .border = .{
                .width = .all(2),
                .color = COLOR_BROWN,
            },
            .corner_radius = .all(5),
            .clip = .{
                .vertical = true,
                .child_offset = ui.scrollOffset(),
            },
            .custom = &selection_render_data,
            .state = .flags(.{
                .click = true,
                .focus = true,
                .text = true,
                .hover = true,
            }),
        });

        try ui.text(currentText().items, .{
            .alignment = .left,
            .font_id = FONT_ID_MONO,
            .font_size = 16,
            .color = COLOR_BLUE,
            .line_height = 20,
        });
    }
}

pub fn main() !void {
    var debug_timer = try std.time.Timer.start();
    var timer = try std.time.Timer.start();
    var last_frame_time = timer.read();

    const gpa = std.heap.page_allocator;

    try carets.append(gpa, .{ .start = 32, .end = 32 });

    if (comptime builtin.os.tag != .windows) {
        glfw.initHint(.{ .platform = .x11 });
    } else {
        glfw.initHint(.{ .platform = .win32 });
    }

    try glfw.init();
    defer glfw.deinit();

    stbi.init(gpa);
    defer stbi.deinit();

    var demo = Demo{};

    const instance_extras = wgpu.InstanceExtras{
        .chain = .{ .s_type = .instance_extras },
        .backends = switch (builtin.os.tag) {
            .windows => if (glfw.isRunningInWine()) wgpu.InstanceBackend.vulkanBackend else wgpu.InstanceBackend.dx12Backend,
            else => wgpu.InstanceBackend.vulkanBackend,
        },
    };
    demo.instance = wgpu.createInstance(&wgpu.InstanceDescriptor{ .next_in_chain = @ptrCast(&instance_extras) });
    std.debug.assert(demo.instance != null);
    defer wgpu.instanceRelease(demo.instance);

    glfw.windowHint(.{ .client_api = .none });
    const window = try glfw.createWindow(1000, 1000, "zgpu example", null, null);
    defer glfw.destroyWindow(window);
    glfw.setWindowUserPointer(window, &demo);

    // --- GLFW Callbacks ---
    _ = glfw.setFramebufferSizeCallback(window, &struct {
        fn handle_glfw_framebuffer_size(w: *glfw.Window, width: i32, height: i32) callconv(.c) void {
            if (width <= 0 and height <= 0) return;
            const d: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse return));
            if (d.surface == null) return;
            d.config.width = @intCast(width);
            d.config.height = @intCast(height);
            wgpu.surfaceConfigure(d.surface, &d.config);
            createOrResizeMsaaTexture(d);
        }
    }.handle_glfw_framebuffer_size);

    // --- WGPU Surface, Adapter, Device Setup ---
    if (comptime builtin.os.tag != .windows) {
        const x11_display = glfw.getX11Display();
        const x11_window = glfw.getX11Window(window);
        var xlib_source = wgpu.SurfaceSourceXlibWindow{ .chain = .{ .s_type = .surface_source_xlib_window }, .display = x11_display, .window = x11_window };
        demo.surface = wgpu.instanceCreateSurface(demo.instance, &wgpu.SurfaceDescriptor{ .next_in_chain = @ptrCast(&xlib_source) });
    } else {
        const win32_hwnd = glfw.getWin32Window(window);
        const win32_hinstance = glfw.getWin32ModuleHandle();
        var win32_source = wgpu.SurfaceSourceWindowsHWND{ .chain = .{ .s_type = .surface_source_windows_hwnd }, .hwnd = win32_hwnd, .hinstance = win32_hinstance };
        demo.surface = wgpu.instanceCreateSurface(demo.instance, &wgpu.SurfaceDescriptor{ .next_in_chain = @ptrCast(&win32_source) });
    }
    std.debug.assert(demo.surface != null);
    defer wgpu.surfaceRelease(demo.surface);

    _ = wgpu.instanceRequestAdapter(demo.instance, &wgpu.RequestAdapterOptions{ .compatible_surface = demo.surface }, .{ .callback = &struct {
        fn handle_request_adapter(status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, msg: wgpu.StringView, ud1: ?*anyopaque, _: ?*anyopaque) callconv(.c) void {
            if (status == .success) {
                const d: *Demo = @ptrCast(@alignCast(ud1.?));
                d.adapter = adapter;
            } else {
                log.err("request_adapter failed: {s}", .{msg.toSlice()});
            }
        }
    }.handle_request_adapter, .userdata1 = &demo });
    while (demo.adapter == null) wgpu.instanceProcessEvents(demo.instance);
    defer wgpu.adapterRelease(demo.adapter);

    _ = wgpu.adapterRequestDevice(demo.adapter, null, .{ .callback = &struct {
        fn handle_request_device(status: wgpu.RequestDeviceStatus, device: wgpu.Device, msg: wgpu.StringView, ud1: ?*anyopaque, _: ?*anyopaque) callconv(.c) void {
            if (status == .success) {
                const d: *Demo = @ptrCast(@alignCast(ud1.?));
                d.device = device;
            } else {
                log.err("request_device failed: {s}", .{msg.toSlice()});
            }
        }
    }.handle_request_device, .userdata1 = &demo });
    while (demo.device == null) wgpu.instanceProcessEvents(demo.instance);
    defer {
        if (demo.msaa_view != null) wgpu.textureViewRelease(demo.msaa_view);
        if (demo.msaa_texture != null) wgpu.textureRelease(demo.msaa_texture);
        wgpu.deviceRelease(demo.device);
    }

    const queue = wgpu.deviceGetQueue(demo.device);
    defer wgpu.queueRelease(queue);
    var surface_capabilities: wgpu.SurfaceCapabilities = undefined;
    _ = wgpu.surfaceGetCapabilities(demo.surface, demo.adapter, &surface_capabilities);
    defer wgpu.surfaceCapabilitiesFreeMembers(surface_capabilities);

    const surface_format = surface_capabilities.formats.?[0];
    demo.config = .{
        .device = demo.device,
        .usage = .renderAttachmentUsage,
        .format = surface_format,
        .present_mode = .immediate,
        .alpha_mode = surface_capabilities.alpha_modes.?[0],
    };

    var window_width: i32 = 0;
    var window_height: i32 = 0;
    {
        glfw.getFramebufferSize(window, &window_width, &window_height);
        demo.config.width = @intCast(window_width);
        demo.config.height = @intCast(window_height);
    }
    wgpu.surfaceConfigure(demo.surface, &demo.config);
    createOrResizeMsaaTexture(&demo);

    // --- Application and UI Library Initialization ---
    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();

    const arena = arena_state.allocator();

    // Init Asset Cache
    var asset_cache = AssetCache.init(gpa);
    defer asset_cache.deinit();

    // Init Batch Renderer
    demo.renderer = try Batch2D.init(gpa, demo.device, queue, surface_format, &asset_cache, MSAA_SAMPLE_COUNT);
    defer demo.renderer.deinit();

    var inputs = InputState{};
    inputs.listenAllGlfw(window);

    var bindings = BindingState.init(gpa, &inputs);
    defer bindings.deinit();

    try bindings.bind(.toggle_debugger, .{ .key = .{ .bind_point = .d, .modifiers = .ctrlMod } });
    try bindings.bind(.dump_atlas, .{ .key = .{ .bind_point = .a, .modifiers = .altMod } });

    // Init Ui
    var ui = try Ui.init(gpa, demo.renderer, &asset_cache, &bindings);
    ui.custom_element_renderer = &struct {
        pub fn selection_renderer(g: *Ui, _: ?*anyopaque, command: clay.RenderCommand) !void {
            const data = @as(*const SelectionRenderData, @ptrCast(@alignCast(command.render_data.custom.custom_data.?)));
            const caret_list = data.carets;

            const selection_color = Ui.Color.init(0.75, 0, 0.75, 0.5);
            const caret_width: f32 = 1.5;

            clay.setCurrentContext(g.clay_context);
            defer clay.setCurrentContext(null);

            if (g.state.focusedIdValue() != command.id) return;

            for (caret_list.items) |caret| {
                // Draw selection highlight
                if (caret.hasSelection()) {
                    const start_idx = caret.min();
                    const end_idx = caret.max();

                    var i = start_idx;
                    while (i < end_idx) {
                        const start_of_line = i;

                        const start_offset = clay.getCharacterOffset(.fromRawId(command.id), start_of_line);
                        if (!start_offset.found) {
                            i += 1;
                            continue;
                        }

                        // Find the end of the current line within the selection
                        var end_of_line = i;
                        while (end_of_line + 1 < end_idx) {
                            const next_offset = clay.getCharacterOffset(.fromRawId(command.id), end_of_line + 1);
                            if (!next_offset.found or next_offset.offset.y != start_offset.offset.y) {
                                break;
                            }
                            end_of_line += 1;
                        }

                        const end_offset = clay.getCharacterOffset(.fromRawId(command.id), end_of_line);
                        const next_char_after_end_offset = clay.getCharacterOffset(.fromRawId(command.id), end_of_line + 1);

                        const rect_x = command.bounding_box.x + start_offset.offset.x;
                        const rect_y = command.bounding_box.y + start_offset.offset.y;
                        const rect_h = start_offset.line_height;

                        // Determine the width of the selection rectangle for this line
                        const rect_w = if (next_char_after_end_offset.found and next_char_after_end_offset.offset.y == end_offset.offset.y)
                            // The selection ends mid-line
                            next_char_after_end_offset.offset.x - start_offset.offset.x
                        else
                            // The selection covers the rest of the line
                            end_offset.offset.x - start_offset.offset.x;

                        try g.renderer.drawRect(.{ .x = rect_x, .y = rect_y }, .{ .x = rect_w, .y = rect_h }, selection_color);

                        i = end_of_line + 1;
                    }
                }

                // Draw caret at the end position
                const caret_offset = clay.getCharacterOffset(.fromRawId(command.id), caret.end);
                if (caret_offset.found) {
                    const caret_x = command.bounding_box.x + caret_offset.offset.x;
                    const caret_y = command.bounding_box.y + caret_offset.offset.y;
                    try g.renderer.drawRect(.{ .x = caret_x, .y = caret_y }, .{ .x = caret_width, .y = caret_offset.line_height }, .{ .r = 1, .a = 1 });
                }
            }
        }
    }.selection_renderer;
    defer ui.deinit();

    // --- Load Assets ---
    FONT_ID_BODY = try asset_cache.loadFont("assets/fonts/Quicksand-Semibold.ttf");
    FONT_ID_TITLE = try asset_cache.loadFont("assets/fonts/Calistoga-Regular.ttf");
    FONT_ID_MONO = try asset_cache.loadFont("assets/fonts/dejavu/DejaVuSansMono.ttf");

    zig_logo_image_id = try asset_cache.loadImage("assets/images/zig-mark.png", true);
    wgpu_logo_image_id = try asset_cache.loadImage("assets/images/wgpu-logo.png", true);

    try demo.renderer.preAtlasAllImages();
    try text_buffers[0].appendSlice(gpa, test_text_default);
    text_buffers[1] = try text_buffers[0].clone(gpa);
    defer text_buffers[0].deinit(gpa);
    defer text_buffers[1].deinit(gpa);

    const startup_time = debug.start(&debug_timer);
    log.info("startup_time={d}ms", .{startup_time});

    var debug_mode_enabled = false;
    var is_dragging_text = false;
    main_loop: while (!glfw.windowShouldClose(window)) {
        glfw.pollEvents();
        _ = arena_state.reset(.free_all);

        // --- Handle Text Buffer Swap ---
        if (text_was_modified_last_frame) {
            // Swap to the buffer that was modified last frame, making it current.
            current_buffer_idx = 1 - current_buffer_idx;
            text_was_modified_last_frame = false;

            // Sync the 'next' buffer to be a fresh copy of the 'current' one,
            // so subsequent edits in this frame start from the correct state.
            var target_buffer = nextText();
            target_buffer.deinit(gpa);
            target_buffer.* = try currentText().clone(gpa);
        }

        // --- Calculate Delta Time ---
        const current_time = timer.read();
        const delta_time_ns = current_time - last_frame_time;
        last_frame_time = current_time;
        const delta_time_f32 = @as(f32, @floatFromInt(delta_time_ns)) / std.time.ns_per_s;

        // --- Handle Input ---
        if (bindings.get(.toggle_debugger) == .pressed) {
            debug_mode_enabled = !debug_mode_enabled;
            ui.setDebugMode(debug_mode_enabled);
        }

        if (bindings.get(.dump_atlas) == .pressed) {
            try demo.renderer.atlas.debugWriteAllAtlasesToPng("debug_atlas");
            log.info("finished writing debug_atlas_*.png", .{});
        }

        // --- Update Clay UI ---
        glfw.getFramebufferSize(window, &window_width, &window_height);

        inputs.collectAllGlfw(window);

        // Generate the UI layout and cache rendering commands
        {
            ui.beginLayout(
                .{ .x = @floatFromInt(window_width), .y = @floatFromInt(window_height) },
                delta_time_f32,
            );

            try createLayout(ui);

            const events = try ui.endLayout();

            for (events) |event| {
                switch (event.data) {
                    .hover_begin => |hover_begin_data| {
                        log.info("hover_begin id={any} loc={f}", .{ event.element_id, hover_begin_data.mouse_position });
                    },
                    .hover_end => |hover_end_data| {
                        log.info("hover_end id={any} loc={f}", .{ event.element_id, hover_end_data.mouse_position });
                    },
                    .hovering => |hovering_data| {
                        if (is_dragging_text) {
                            if (event.element_id.id == Ui.ElementId.fromSlice("TextInputTest").id) {
                                const location = clay.Vector2{
                                    .x = hovering_data.mouse_position.x - event.bounding_box.x,
                                    .y = hovering_data.mouse_position.y - event.bounding_box.y,
                                };

                                clay.setCurrentContext(ui.clay_context);
                                defer clay.setCurrentContext(null);

                                const offset = clay.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), location);
                                if (offset.found and carets.items.len > 0) {
                                    carets.items[carets.items.len - 1].end = offset.index;
                                }
                            }
                        }
                    },
                    .mouse_down => |mouse_down_data| {
                        log.info("mouse_down id={any} loc={f}", .{ event.element_id, mouse_down_data.mouse_position });

                        if (event.element_id.id == Ui.ElementId.fromSlice("TextInputTest").id) {
                            is_dragging_text = true;

                            const location = clay.Vector2{
                                .x = mouse_down_data.mouse_position.x - event.bounding_box.x,
                                .y = mouse_down_data.mouse_position.y - event.bounding_box.y,
                            };
                            clay.setCurrentContext(ui.clay_context);
                            defer clay.setCurrentContext(null);
                            const offset = clay.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), location);
                            if (offset.found) {
                                const modifiers = inputs.getModifiers();
                                if (modifiers.alt) {
                                    // Alt-click: Add a new caret
                                    try carets.append(gpa, .{ .start = offset.index, .end = offset.index });
                                } else if (modifiers.shift and carets.items.len > 0) {
                                    // Shift-click: Extend selection of the last caret
                                    carets.items[carets.items.len - 1].end = offset.index;
                                } else {
                                    // Normal click: Clear all and create one new caret
                                    carets.clearRetainingCapacity();
                                    try carets.append(gpa, .{ .start = offset.index, .end = offset.index });
                                }
                            }
                        }
                    },
                    .mouse_up => |mouse_up_data| {
                        log.info("mouse_up id={any} loc={f} end_id={any}", .{ event.element_id, mouse_up_data.mouse_position, mouse_up_data.end_element });
                        if (is_dragging_text) {
                            is_dragging_text = false;
                            try sortAndMergeCarets(&carets);
                        }
                    },
                    .clicked => |clicked_data| {
                        log.info("clicked id={any} loc={f}", .{ event.element_id, clicked_data.mouse_position });
                    },
                    .wheel => |wheel_data| {
                        log.info("wheel id={any} delta={f}", .{ event.element_id, wheel_data.delta });
                    },
                    .focus_gained => {
                        log.info("focus_gained id={any}", .{event.element_id});
                    },
                    .focusing => {},
                    .focus_lost => {
                        log.info("focus_lost id={any}", .{event.element_id});
                    },
                    .activate_begin => {
                        log.info("activate_begin id={any}", .{event.element_id});
                    },
                    .activating => {},
                    .activate_end => |activate_end_data| {
                        log.info("activate_end id={any} end_element={any}", .{ event.element_id, activate_end_data.end_element });
                    },
                    .text => |text_data| {
                        log.info("text id={any} cmds={any}", .{ event.element_id, text_data });
                        if (event.element_id.id == Ui.ElementId.fromSlice("TextInputTest").id) {
                            switch (text_data) {
                                .command => |cmd| switch (cmd.action) {
                                    .copy => copy_action: {
                                        try sortAndMergeCarets(&carets);
                                        var has_selection = false;
                                        for (carets.items) |c| {
                                            if (c.hasSelection()) {
                                                has_selection = true;
                                            }
                                        }
                                        if (!has_selection) break :copy_action;

                                        var clipboard_list = std.ArrayList(u8).empty;

                                        var first = true;
                                        for (carets.items) |caret| {
                                            if (!caret.hasSelection()) continue;
                                            if (!first) try clipboard_list.append(arena, '\n');
                                            first = false;

                                            const selection = currentText().items[caret.min()..caret.max()];
                                            try clipboard_list.appendSlice(arena, selection);
                                        }

                                        const selection_z = try clipboard_list.toOwnedSliceSentinel(arena, 0);
                                        glfw.setClipboardString(window, selection_z);
                                        log.info(" -> copied '{s}' to clipboard", .{selection_z});
                                    },
                                    .paste => pasting: {
                                        const clipboard = std.mem.span(glfw.getClipboardString(window) orelse break :pasting);
                                        log.info("  -> paste: '{s}'", .{clipboard});
                                        try applyTextModification(gpa, .insert, clipboard);
                                    },
                                    .delete => {
                                        if (cmd.modifiers.ctrl) {
                                            for (carets.items) |*caret| {
                                                if (!caret.hasSelection()) {
                                                    caret.end = findNextWordBreak(currentText().items, caret.end);
                                                }
                                            }
                                        }
                                        try applyTextModification(gpa, .delete, "");
                                    },
                                    .backspace => {
                                        if (cmd.modifiers.ctrl) {
                                            for (carets.items) |*caret| {
                                                if (!caret.hasSelection()) {
                                                    caret.end = findPrevWordBreak(currentText().items, caret.end);
                                                }
                                            }
                                        }
                                        try applyTextModification(gpa, .backspace, "");
                                    },
                                    .newline => {
                                        try applyTextModification(gpa, .insert, "\n");
                                    },
                                    .select_all => {
                                        carets.clearRetainingCapacity();
                                        try carets.append(gpa, .{ .start = 0, .end = @intCast(currentText().items.len) });
                                    },
                                    .move_left, .move_right, .move_up, .move_down, .home, .end => {
                                        var new_carets_to_add = std.ArrayList(Caret).empty;

                                        for (carets.items) |*caret| {
                                            switch (cmd.action) {
                                                .move_left => {
                                                    if (caret.hasSelection() and !cmd.modifiers.shift) {
                                                        caret.end = caret.min();
                                                    } else if (cmd.modifiers.ctrl) {
                                                        caret.end = findPrevWordBreak(currentText().items, caret.end);
                                                    } else if (caret.end > 0) {
                                                        var new_pos = caret.end - 1;
                                                        while (new_pos > 0 and (currentText().items[new_pos] & 0b1100_0000) == 0b1000_0000) : (new_pos -= 1) {}
                                                        caret.end = new_pos;
                                                    }
                                                    if (!cmd.modifiers.shift) caret.start = caret.end;
                                                },
                                                .move_right => {
                                                    if (caret.hasSelection() and !cmd.modifiers.shift) {
                                                        caret.end = caret.max();
                                                    } else if (cmd.modifiers.ctrl) {
                                                        caret.end = findNextWordBreak(currentText().items, caret.end);
                                                    } else if (caret.end < currentText().items.len) {
                                                        var new_pos = caret.end + 1;
                                                        while (new_pos < currentText().items.len and (currentText().items[new_pos] & 0b1100_0000) == 0b1000_0000) : (new_pos += 1) {}
                                                        caret.end = new_pos;
                                                    }
                                                    if (!cmd.modifiers.shift) caret.start = caret.end;
                                                },
                                                .move_up, .move_down => {
                                                    clay.setCurrentContext(ui.clay_context);
                                                    defer clay.setCurrentContext(null);

                                                    const start_offset_res = clay.getCharacterOffset(Ui.ElementId.fromSlice("TextInputTest"), caret.start);
                                                    const end_offset_res = clay.getCharacterOffset(Ui.ElementId.fromSlice("TextInputTest"), caret.end);

                                                    if (!start_offset_res.found or !end_offset_res.found) continue;

                                                    const target_y_offset = if (cmd.action == .move_up)
                                                        -start_offset_res.line_height
                                                    else
                                                        start_offset_res.line_height;

                                                    if (cmd.modifiers.alt) {
                                                        // Alt: Duplicate selection to the next line
                                                        const target_start_loc = clay.Vector2{ .x = start_offset_res.offset.x, .y = start_offset_res.offset.y + target_y_offset };
                                                        const new_start_res = clay.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), target_start_loc);

                                                        const target_end_loc = clay.Vector2{ .x = end_offset_res.offset.x, .y = end_offset_res.offset.y + target_y_offset };
                                                        const new_end_res = clay.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), target_end_loc);

                                                        if (new_start_res.found and new_end_res.found) {
                                                            try new_carets_to_add.append(arena, .{ .start = new_start_res.index, .end = new_end_res.index });
                                                        }
                                                    } else if (cmd.modifiers.shift) {
                                                        // Shift: Extend selection to the next line
                                                        const target_end_loc = clay.Vector2{ .x = end_offset_res.offset.x, .y = end_offset_res.offset.y + target_y_offset };
                                                        const new_end_res = clay.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), target_end_loc);

                                                        if (new_end_res.found) {
                                                            caret.end = new_end_res.index;
                                                        }
                                                    } else {
                                                        // No modifiers: Move caret, collapsing selection
                                                        const target_end_loc = clay.Vector2{ .x = end_offset_res.offset.x, .y = end_offset_res.offset.y + target_y_offset };
                                                        const new_end_res = clay.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), target_end_loc);
                                                        if (new_end_res.found) {
                                                            caret.start = new_end_res.index;
                                                            caret.end = new_end_res.index;
                                                        }
                                                    }
                                                },
                                                .home => {
                                                    if (cmd.modifiers.ctrl) {
                                                        caret.end = 0;
                                                    } else {
                                                        clay.setCurrentContext(ui.clay_context);
                                                        defer clay.setCurrentContext(null);
                                                        const offset = clay.getCharacterOffset(Ui.ElementId.fromSlice("TextInputTest"), caret.end);
                                                        if (offset.found) {
                                                            const target_loc = clay.Vector2{ .x = 0, .y = offset.offset.y };
                                                            const target_offset = clay.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), target_loc);
                                                            if (target_offset.found) caret.end = target_offset.index;
                                                        }
                                                    }
                                                    if (!cmd.modifiers.shift) caret.start = caret.end;
                                                },
                                                .end => {
                                                    if (cmd.modifiers.ctrl) {
                                                        caret.end = @intCast(currentText().items.len);
                                                    } else {
                                                        clay.setCurrentContext(ui.clay_context);
                                                        defer clay.setCurrentContext(null);
                                                        const current_offset_res = clay.getCharacterOffset(Ui.ElementId.fromSlice("TextInputTest"), caret.end);
                                                        if (current_offset_res.found) {
                                                            const end_of_line_target_loc = clay.Vector2{ .x = 999999.0, .y = current_offset_res.offset.y };
                                                            const end_of_line_res = clay.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), end_of_line_target_loc);
                                                            if (end_of_line_res.found) {
                                                                var last_char_idx = end_of_line_res.index;
                                                                const check_offset_res = clay.getCharacterOffset(Ui.ElementId.fromSlice("TextInputTest"), last_char_idx);
                                                                if (check_offset_res.found and check_offset_res.offset.y != current_offset_res.offset.y and last_char_idx > 0) {
                                                                    var prev_char_start = last_char_idx - 1;
                                                                    while (prev_char_start > 0 and (currentText().items[prev_char_start] & 0b1100_0000) == 0b1000_0000) {
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

                                        try carets.appendSlice(gpa, new_carets_to_add.items);
                                        try sortAndMergeCarets(&carets);
                                    },
                                },
                                .chars => |chars| {
                                    var buffer = std.ArrayList(u8).empty;
                                    for (chars) |char_cmd| {
                                        const width = try std.unicode.utf8CodepointSequenceLength(char_cmd.codepoint);
                                        const buf = try arena.alloc(u8, width);
                                        _ = try std.unicode.utf8Encode(char_cmd.codepoint, buf);
                                        try buffer.appendSlice(arena, buf);
                                    }
                                    try applyTextModification(gpa, .insert, buffer.items);
                                },
                            }
                        }
                    },
                }
            }
        }

        // --- WGPU Frame Rendering ---
        var surface_texture: wgpu.SurfaceTexture = undefined;
        wgpu.surfaceGetCurrentTexture(demo.surface, &surface_texture);
        switch (surface_texture.status) {
            .success_optimal, .success_suboptimal => {},
            .timeout, .outdated, .lost => {
                if (surface_texture.texture != null) wgpu.textureRelease(surface_texture.texture);
                glfw.getFramebufferSize(window, &window_width, &window_height);
                if (window_width != 0 and window_height != 0) {
                    demo.config.width = @intCast(window_width);
                    demo.config.height = @intCast(window_height);
                    wgpu.surfaceConfigure(demo.surface, &demo.config);
                    createOrResizeMsaaTexture(&demo);
                }
                continue :main_loop;
            },
            else => std.debug.panic("get_current_texture status={any}", .{surface_texture.status}),
        }
        std.debug.assert(surface_texture.texture != null);
        defer wgpu.textureRelease(surface_texture.texture);

        const frame_view = wgpu.textureCreateView(surface_texture.texture, null);
        std.debug.assert(frame_view != null);
        defer wgpu.textureViewRelease(frame_view);

        // --- Queue all draws to screen buffer ---
        {
            const proj = ortho(0, @floatFromInt(demo.config.width), @floatFromInt(demo.config.height), 0, -1, 1);
            demo.renderer.beginFrame(proj, demo.config.width, demo.config.height);

            try ui.render();

            try debug.drawFpsChart(demo.renderer, .{});

            try demo.renderer.endFrame();
        }

        // --- WGPU Command Submission ---
        const encoder = wgpu.deviceCreateCommandEncoder(demo.device, &.{ .label = .fromSlice("main_encoder") });
        defer wgpu.commandEncoderRelease(encoder);

        const render_target_view = if (demo.msaa_view != null) demo.msaa_view else frame_view;
        const resolve_target_view = if (demo.msaa_view != null) frame_view else null;
        const clear_color = wgpu.Color{ .r = 0.1, .g = 0.1, .b = 0.1, .a = 1.0 };

        const render_pass = wgpu.commandEncoderBeginRenderPass(encoder, &wgpu.RenderPassDescriptor{
            .color_attachment_count = 1,
            .color_attachments = &[_]wgpu.RenderPassColorAttachment{.{
                .view = render_target_view,
                .resolve_target = resolve_target_view,
                .load_op = .clear,
                .store_op = .store,
                .clear_value = clear_color,
            }},
        });

        try demo.renderer.render(render_pass);

        wgpu.renderPassEncoderEnd(render_pass);
        wgpu.renderPassEncoderRelease(render_pass);

        const cmd = wgpu.commandEncoderFinish(encoder, null);
        defer wgpu.commandBufferRelease(cmd);

        wgpu.queueSubmit(queue, 1, &.{cmd});

        _ = wgpu.surfacePresent(demo.surface);

        debug.lap();
    }
}

/// (Re)creates or resizes the MSAA texture used for rendering, based on the current swap chain size.
fn createOrResizeMsaaTexture(d: *Demo) void {
    if (d.msaa_view != null) wgpu.textureViewRelease(d.msaa_view);
    if (d.msaa_texture != null) wgpu.textureRelease(d.msaa_texture);

    if (MSAA_SAMPLE_COUNT <= 1) {
        d.msaa_texture = null;
        d.msaa_view = null;
        return;
    }

    const msaa_descriptor = wgpu.TextureDescriptor{
        .label = .fromSlice("msaa_texture"),
        .size = .{ .width = d.config.width, .height = d.config.height, .depth_or_array_layers = 1 },
        .mip_level_count = 1,
        .sample_count = MSAA_SAMPLE_COUNT,
        .dimension = .@"2d",
        .format = d.config.format,
        .usage = wgpu.TextureUsage{ .render_attachment = true },
    };

    d.msaa_texture = wgpu.deviceCreateTexture(d.device, &msaa_descriptor);
    std.debug.assert(d.msaa_texture != null);
    d.msaa_view = wgpu.textureCreateView(d.msaa_texture, null);
    std.debug.assert(d.msaa_view != null);
}

/// orthographic projection matrix helper
fn ortho(left: f32, right: f32, bottom: f32, top: f32, near: f32, far: f32) [16]f32 {
    var mat: [16]f32 = std.mem.zeroes([16]f32);
    mat[0] = 2.0 / (right - left);
    mat[5] = 2.0 / (top - bottom);
    mat[10] = -2.0 / (far - near);
    mat[12] = -(right + left) / (right - left);
    mat[13] = -(top + bottom) / (top - bottom);
    mat[14] = -(far + near) / (far - near);
    mat[15] = 1.0;
    return mat;
}
