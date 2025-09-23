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
var test_text = std.ArrayList(u8).empty;

const TextInputState = struct {
    start: u32 = 32,
    end: u32 = 32,

    fn min(self: *TextInputState) u32 {
        return @min(self.start, self.end);
    }
    fn max(self: *TextInputState) u32 {
        return @max(self.start, self.end);
    }
    fn hasSelection(self: *TextInputState) bool {
        return self.start != self.end;
    }
    fn deleteSelection(self: *TextInputState) !void {
        if (!self.hasSelection()) return;

        const start = self.min();
        const end = self.max();
        const delete_count = end - start;
        const remaining_after = test_text.items.len - end;

        if (remaining_after > 0) {
            const src = test_text.items.ptr + end;
            const dest = test_text.items.ptr + start;
            @memmove(dest[0..remaining_after], src[0..remaining_after]);
        }
        test_text.shrinkRetainingCapacity(test_text.items.len - delete_count);

        self.start = start;
        self.end = start;
    }
};
var text_input_state: TextInputState = .{};

const SelectionRenderData = struct {
    state: *TextInputState,
};
var selection_render_data = SelectionRenderData{
    .state = &text_input_state,
};

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
            .custom = &selection_render_data,
            .state = .flags(.{
                .wheel = true,
                .click = true,
                .focus = true,
                .text = true,
                .hover = true, // needed for drag selection
            }),
        });

        try ui.text(test_text.items, .{
            .alignment = .left,
            .font_id = FONT_ID_BODY,
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
            const state = data.state;

            const selection_color = Ui.Color.init(0.75, 0, 0.75, 0.5);
            const caret_width: f32 = 1.5;

            clay.setCurrentContext(g.clay_context);
            defer clay.setCurrentContext(null);

            if (g.state.focusedIdValue() != command.id) return;

            // Draw selection highlight
            if (state.hasSelection()) {
                const start_idx = state.min();
                const end_idx = state.max();

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
            const caret_offset = clay.getCharacterOffset(.fromRawId(command.id), state.end);
            if (caret_offset.found) {
                const caret_x = command.bounding_box.x + caret_offset.offset.x;
                const caret_y = command.bounding_box.y + caret_offset.offset.y;
                try g.renderer.drawRect(.{ .x = caret_x, .y = caret_y }, .{ .x = caret_width, .y = caret_offset.line_height }, .{ .r = 1, .a = 1 });
            }
        }
    }.selection_renderer;
    defer ui.deinit();

    // --- Load Assets ---
    FONT_ID_BODY = try asset_cache.loadFont("assets/fonts/Quicksand-Semibold.ttf");
    FONT_ID_TITLE = try asset_cache.loadFont("assets/fonts/Calistoga-Regular.ttf");

    zig_logo_image_id = try asset_cache.loadImage("assets/images/zig-mark.png", true);
    wgpu_logo_image_id = try asset_cache.loadImage("assets/images/wgpu-logo.png", true);

    try demo.renderer.preAtlasAllImages();
    try test_text.appendSlice(gpa, test_text_default);

    const startup_time = debug.start(&debug_timer);
    log.info("startup_time={d}ms", .{startup_time});

    var debug_mode_enabled = false;
    var is_dragging_text = false;
    main_loop: while (!glfw.windowShouldClose(window)) {
        glfw.pollEvents();
        _ = arena_state.reset(.free_all);

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
                                if (offset.found) {
                                    text_input_state.end = offset.index;
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
                                if (inputs.getModifiers().shift) {
                                    text_input_state.end = offset.index;
                                } else {
                                    text_input_state.start = offset.index;
                                    text_input_state.end = offset.index;
                                }
                            }
                        }
                    },
                    .mouse_up => |mouse_up_data| {
                        log.info("mouse_up id={any} loc={f} end_id={any}", .{ event.element_id, mouse_up_data.mouse_position, mouse_up_data.end_element });
                        is_dragging_text = false;
                    },
                    .clicked => |clicked_data| {
                        log.info("clicked id={any} loc={f}", .{ event.element_id, clicked_data.mouse_position });
                    },
                    .wheel => |wheel_data| {
                        log.info("wheel id={any} delta={f}", .{ event.element_id, wheel_data.delta });

                        if (event.element_id.id == Ui.ElementId.fromSlice("TextInputTest").id) {
                            var new_caret_pos = text_input_state.end;
                            if (wheel_data.delta.y < 0) {
                                if (new_caret_pos > 0) new_caret_pos -= 1;
                            } else if (wheel_data.delta.y > 0) {
                                if (new_caret_pos < @as(u32, @intCast(test_text.items.len))) new_caret_pos += 1;
                            }
                            text_input_state.end = new_caret_pos;

                            if (!inputs.getModifiers().shift) {
                                text_input_state.start = new_caret_pos;
                            }
                            log.info("  -> new selection=[{d}..{d}]/{d}", .{ text_input_state.min(), text_input_state.max(), test_text.items.len });
                        }
                    },
                    .focus_gained => {
                        log.info("focus_gained id={any}", .{event.element_id});
                    },
                    .focusing => {
                        // log.info("focusing id={any}", .{event.element_id});
                    },
                    .focus_lost => {
                        log.info("focus_lost id={any}", .{event.element_id});
                    },
                    .activate_begin => {
                        log.info("activate_begin id={any}", .{event.element_id});
                    },
                    .activating => {
                        // log.info("activating id={any}", .{event.element_id});
                    },
                    .activate_end => |activate_end_data| {
                        log.info("activate_end id={any} end_element={any}", .{ event.element_id, activate_end_data.end_element });
                    },
                    .text => |text_data| {
                        log.info("text id={any} cmds={any}", .{ event.element_id, text_data });
                        if (event.element_id.id == Ui.ElementId.fromSlice("TextInputTest").id) {
                            switch (text_data) {
                                .command => |cmd| switch (cmd.action) {
                                    .copy => {
                                        if (text_input_state.hasSelection()) {
                                            const selection = test_text.items[text_input_state.min()..text_input_state.max()];
                                            const selection_z = try arena.dupeZ(u8, selection);
                                            glfw.setClipboardString(window, selection_z);
                                            log.info(" -> copied '{s}' to clipboard", .{selection});
                                        }
                                    },
                                    .paste => pasting: {
                                        try text_input_state.deleteSelection();
                                        const clipboard = std.mem.span(glfw.getClipboardString(window) orelse break :pasting);
                                        log.info("  -> paste at {d}: '{s}'", .{ text_input_state.end, clipboard });
                                        try test_text.insertSlice(gpa, text_input_state.end, clipboard);
                                        const new_pos = @as(u32, @intCast(text_input_state.end + clipboard.len));
                                        text_input_state.start = new_pos;
                                        text_input_state.end = new_pos;
                                    },
                                    .delete => {
                                        if (text_input_state.hasSelection()) {
                                            try text_input_state.deleteSelection();
                                        } else if (cmd.modifiers.ctrl) {
                                            // Ctrl+Delete: Delete to the next word break
                                            if (text_input_state.end < test_text.items.len) {
                                                var delete_end = text_input_state.end;
                                                const text = test_text.items;

                                                // Skip any initial whitespace
                                                while (delete_end < text.len and std.ascii.isWhitespace(text[delete_end])) : (delete_end += 1) {}
                                                // Skip non-whitespace characters
                                                while (delete_end < text.len and !std.ascii.isWhitespace(text[delete_end])) : (delete_end += 1) {}

                                                const delete_len = delete_end - text_input_state.end;
                                                if (delete_len > 0) {
                                                    log.info("  -> ctrl+delete at {d}, deleting {d} bytes", .{ text_input_state.end, delete_len });
                                                    const remaining_after = test_text.items.len - delete_end;
                                                    if (remaining_after > 0) {
                                                        const src = test_text.items.ptr + delete_end;
                                                        const dest = test_text.items.ptr + text_input_state.end;
                                                        @memmove(dest[0..remaining_after], src[0..remaining_after]);
                                                    }
                                                    test_text.shrinkRetainingCapacity(test_text.items.len - delete_len);
                                                }
                                            }
                                        } else if (text_input_state.end < test_text.items.len) {
                                            var delete_end = text_input_state.end + 1;
                                            while (delete_end < test_text.items.len and (test_text.items[delete_end] & 0b1100_0000) == 0b1000_0000) : (delete_end += 1) {}
                                            const delete_len = delete_end - text_input_state.end;

                                            log.info("  -> delete at {d}, deleting {d} bytes", .{ text_input_state.end, delete_len });

                                            const remaining_after = test_text.items.len - delete_end;
                                            if (remaining_after > 0) {
                                                const src = test_text.items.ptr + delete_end;
                                                const dest = test_text.items.ptr + text_input_state.end;
                                                @memmove(dest[0..remaining_after], src[0..remaining_after]);
                                            }
                                            test_text.shrinkRetainingCapacity(test_text.items.len - delete_len);
                                        }
                                    },
                                    .backspace => {
                                        if (text_input_state.hasSelection()) {
                                            try text_input_state.deleteSelection();
                                        } else if (cmd.modifiers.ctrl) {
                                            // Ctrl+Backspace: Delete to the previous word break
                                            if (text_input_state.end > 0) {
                                                var delete_start = text_input_state.end;
                                                const text = test_text.items;

                                                delete_start -= 1;
                                                while (delete_start > 0 and std.ascii.isWhitespace(text[delete_start])) : (delete_start -= 1) {}
                                                while (delete_start > 0 and !std.ascii.isWhitespace(text[delete_start])) : (delete_start -= 1) {}

                                                if (std.ascii.isWhitespace(text[delete_start])) {
                                                    delete_start += 1;
                                                }

                                                const delete_len = text_input_state.end - delete_start;

                                                if (delete_len > 0) {
                                                    log.info("  -> ctrl+backspace at {d}, deleting {d} bytes from {d}", .{ text_input_state.end, delete_len, delete_start });

                                                    const remaining_after = test_text.items.len - text_input_state.end;
                                                    if (remaining_after > 0) {
                                                        const src = test_text.items.ptr + text_input_state.end;
                                                        const dest = test_text.items.ptr + delete_start;
                                                        @memmove(dest[0..remaining_after], src[0..remaining_after]);
                                                    }
                                                    test_text.shrinkRetainingCapacity(test_text.items.len - delete_len);

                                                    text_input_state.start = delete_start;
                                                    text_input_state.end = delete_start;
                                                }
                                            }
                                        } else if (text_input_state.end > 0) {
                                            var delete_start = text_input_state.end - 1;
                                            while (delete_start > 0 and (test_text.items[delete_start] & 0b1100_0000) == 0b1000_0000) : (delete_start -= 1) {}
                                            const delete_len = text_input_state.end - delete_start;

                                            log.info("  -> backspace at {d}, deleting {d} bytes", .{ text_input_state.end, delete_len });

                                            const remaining_after = test_text.items.len - text_input_state.end;
                                            if (remaining_after > 0) {
                                                const src = test_text.items.ptr + text_input_state.end;
                                                const dest = test_text.items.ptr + delete_start;
                                                @memmove(dest[0..remaining_after], src[0..remaining_after]);
                                            }
                                            test_text.shrinkRetainingCapacity(test_text.items.len - delete_len);

                                            text_input_state.start = delete_start;
                                            text_input_state.end = delete_start;
                                        }
                                    },
                                    .newline => {
                                        try text_input_state.deleteSelection();
                                        const newline = "\n";
                                        log.info("  -> newline at {d}", .{text_input_state.end});
                                        try test_text.insertSlice(gpa, text_input_state.end, newline);
                                        const new_pos: u32 = @intCast(text_input_state.end + newline.len);
                                        text_input_state.start = new_pos;
                                        text_input_state.end = new_pos;
                                    },
                                    .move_left => {
                                        if (text_input_state.hasSelection() and !cmd.modifiers.shift) {
                                            const new_pos = text_input_state.min();
                                            text_input_state.start = new_pos;
                                            text_input_state.end = new_pos;
                                        } else if (cmd.modifiers.ctrl) {
                                            // Ctrl+Left: Move to the previous word break
                                            if (text_input_state.end > 0) {
                                                var new_pos = text_input_state.end - 1;
                                                const text = test_text.items;

                                                while (new_pos > 0 and std.ascii.isWhitespace(text[new_pos])) : (new_pos -= 1) {}
                                                while (new_pos > 0 and !std.ascii.isWhitespace(text[new_pos])) : (new_pos -= 1) {}

                                                if (std.ascii.isWhitespace(text[new_pos])) {
                                                    new_pos += 1;
                                                }

                                                text_input_state.end = new_pos;
                                                if (!cmd.modifiers.shift) {
                                                    text_input_state.start = new_pos;
                                                }
                                            }
                                        } else {
                                            if (text_input_state.end > 0) {
                                                var new_pos = text_input_state.end - 1;
                                                // move to the beginning of the utf-8 sequence
                                                while (new_pos > 0 and (test_text.items[new_pos] & 0b1100_0000) == 0b1000_0000) : (new_pos -= 1) {}
                                                text_input_state.end = new_pos;
                                                if (!cmd.modifiers.shift) {
                                                    text_input_state.start = new_pos;
                                                }
                                            }
                                        }
                                    },
                                    .move_right => {
                                        if (text_input_state.hasSelection() and !cmd.modifiers.shift) {
                                            const new_pos = text_input_state.max();
                                            text_input_state.start = new_pos;
                                            text_input_state.end = new_pos;
                                        } else if (cmd.modifiers.ctrl) {
                                            // Ctrl+Right: Move to the next word break
                                            if (text_input_state.end < test_text.items.len) {
                                                var new_pos = text_input_state.end;
                                                const text = test_text.items;

                                                while (new_pos < text.len and !std.ascii.isWhitespace(text[new_pos])) : (new_pos += 1) {}
                                                while (new_pos < text.len and std.ascii.isWhitespace(text[new_pos])) : (new_pos += 1) {}

                                                text_input_state.end = new_pos;
                                                if (!cmd.modifiers.shift) {
                                                    text_input_state.start = new_pos;
                                                }
                                            }
                                        } else {
                                            if (text_input_state.end < test_text.items.len) {
                                                var new_pos = text_input_state.end + 1;
                                                // move to the start of the next utf-8 sequence
                                                while (new_pos < test_text.items.len and (test_text.items[new_pos] & 0b1100_0000) == 0b1000_0000) : (new_pos += 1) {}
                                                text_input_state.end = new_pos;
                                                if (!cmd.modifiers.shift) {
                                                    text_input_state.start = new_pos;
                                                }
                                            }
                                        }
                                    },
                                    .move_up => {
                                        clay.setCurrentContext(ui.clay_context);
                                        defer clay.setCurrentContext(null);

                                        const offset = clay.getCharacterOffset(Ui.ElementId.fromSlice("TextInputTest"), text_input_state.end);
                                        if (offset.found) {
                                            const target_location = clay.Vector2{
                                                .x = offset.offset.x,
                                                .y = offset.offset.y - offset.line_height,
                                            };
                                            const target_offset = clay.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), target_location);
                                            if (target_offset.found) {
                                                text_input_state.end = target_offset.index;
                                                if (!cmd.modifiers.shift) {
                                                    text_input_state.start = target_offset.index;
                                                }
                                            }
                                        }
                                    },
                                    .move_down => {
                                        clay.setCurrentContext(ui.clay_context);
                                        defer clay.setCurrentContext(null);

                                        const offset = clay.getCharacterOffset(Ui.ElementId.fromSlice("TextInputTest"), text_input_state.end);
                                        if (offset.found) {
                                            const target_location = clay.Vector2{
                                                .x = offset.offset.x,
                                                .y = offset.offset.y + offset.line_height,
                                            };
                                            const target_offset = clay.getCharacterIndexAtOffset(Ui.ElementId.fromSlice("TextInputTest"), target_location);
                                            if (target_offset.found) {
                                                text_input_state.end = target_offset.index;
                                                if (!cmd.modifiers.shift) {
                                                    text_input_state.start = target_offset.index;
                                                }
                                            }
                                        }
                                    },
                                },
                                .chars => |chars| {
                                    try text_input_state.deleteSelection();
                                    for (chars) |char_cmd| {
                                        _ = char_cmd.modifiers;
                                        const width = try std.unicode.utf8CodepointSequenceLength(char_cmd.codepoint);
                                        const buf = try arena.alloc(u8, width);
                                        _ = try std.unicode.utf8Encode(char_cmd.codepoint, buf);

                                        log.info("  -> char input: '{s}'", .{buf});
                                        try test_text.insertSlice(gpa, text_input_state.end, buf);
                                        text_input_state.end += @intCast(width);
                                    }
                                    text_input_state.start = text_input_state.end;
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
