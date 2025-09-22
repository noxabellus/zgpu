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
var caret_index: u32 = 32;

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
                .sizing = .{ .w = .fixed(300 + 5 * 2), .h = .fixed(16 * 6 + 5 * 2 + 5) },
                .padding = .all(5),
            },
            .background_color = COLOR_WHITE,
            .border = .{
                .width = .all(2),
                .color = COLOR_BROWN,
            },
            .corner_radius = .all(5),

            .custom = &caret_index, // TODO: in future we will need a more advanced facility for dispatch on kinds of custom elements
            .state = .flags(.{
                .wheel = true,
                .click = true,
                .focus = true,
                .text = true,
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
        pub fn caret_renderer(g: *Ui, _: ?*anyopaque, command: clay.RenderCommand) !void {
            const caret_width: f32 = 1.5;

            const character_offset = clay.getCharacterOffset(Ui.ElementId{ .id = command.id }, @as(*const u32, @ptrCast(@alignCast(command.render_data.custom.custom_data.?))).*);
            std.debug.assert(character_offset.found);

            const caret_x = command.bounding_box.x + character_offset.offset.x;
            const caret_y = command.bounding_box.y + character_offset.offset.y;

            try g.renderer.drawRect(.{ .x = caret_x, .y = caret_y }, .{ .x = caret_width, .y = character_offset.line_height }, .{ .r = 1, .a = 1 });
        }
    }.caret_renderer;
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
                        _ = hovering_data; // too noisey rn
                        // log.info("hovering id={any} loc={f}", .{ event.element_id, hovering_data.mouse_position });
                    },
                    .mouse_down => |mouse_down_data| {
                        log.info("mouse_down id={any} loc={f}", .{ event.element_id, mouse_down_data.mouse_position });
                    },
                    .mouse_up => |mouse_up_data| {
                        log.info("mouse_up id={any} loc={f} end_id={any}", .{ event.element_id, mouse_up_data.mouse_position, mouse_up_data.end_element });
                    },
                    .clicked => |clicked_data| {
                        log.info("clicked id={any} loc={f}", .{ event.element_id, clicked_data.mouse_position });

                        const id = Ui.ElementId.fromSlice("TextInputTest");

                        if (event.element_id.id == id.id) {
                            const location = clay.Vector2{
                                .x = clicked_data.mouse_position.x - event.bounding_box.x,
                                .y = clicked_data.mouse_position.y - event.bounding_box.y,
                            };

                            log.info("  -> clicked local={d:.2}, {d:.2}", .{ location.x, location.y });

                            clay.setCurrentContext(ui.clay_context);
                            defer clay.setCurrentContext(null);

                            const offset = clay.getCharacterIndexAtOffset(id, location);

                            log.info("  -> offset result: index={d} found={any}", .{ offset.index, offset.found });

                            if (offset.found) caret_index = offset.index;
                        }
                    },
                    .wheel => |wheel_data| {
                        log.info("wheel id={any} delta={f}", .{ event.element_id, wheel_data.delta });

                        if (event.element_id.id == Ui.ElementId.fromSlice("TextInputTest").id) {
                            if (wheel_data.delta.y < 0) {
                                if (caret_index > 0) {
                                    caret_index -= 1;
                                }
                            } else if (wheel_data.delta.y > 0) {
                                if (caret_index < @as(u32, @intCast(test_text.items.len))) {
                                    caret_index += 1;
                                }
                            }

                            log.info("  -> new caret_index={d}/{d}", .{ caret_index, test_text.items.len });
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
                                .command => |cmd| switch (cmd) {
                                    .backspace => if (caret_index > 0) {
                                        // Find the start of the previous UTF-8 codepoint
                                        var delete_start = caret_index - 1;
                                        while (delete_start > 0 and (test_text.items[delete_start] & 0b1100_0000) == 0b1000_0000) : (delete_start -= 1) {}
                                        const delete_len = caret_index - delete_start;

                                        log.info("  -> backspace at {d}, deleting {d} bytes", .{ caret_index, delete_len });

                                        for (delete_start..caret_index) |i| {
                                            _ = test_text.orderedRemove(i);
                                        }
                                        caret_index = delete_start;
                                    },
                                    .newline => {
                                        const newline = "\n";
                                        log.info("  -> newline at {d}", .{caret_index});
                                        try test_text.insertSlice(gpa, caret_index, newline);
                                        caret_index += newline.len;
                                    },
                                },
                                .chars => |chars| if (event.element_id.id == Ui.ElementId.fromSlice("TextInputTest").id) {
                                    var input = std.ArrayList(u8).empty;

                                    for (chars) |char_cmd| {
                                        _ = char_cmd.modifiers; // currently ignoring these, could be used for various IME states etc

                                        const width = try std.unicode.utf8CodepointSequenceLength(char_cmd.codepoint);
                                        const buf = try input.addManyAsSlice(arena, width);
                                        _ = try std.unicode.utf8Encode(char_cmd.codepoint, buf);

                                        log.info("  -> char input: '{s}'", .{buf});

                                        // Insert at caret position
                                        try test_text.insertSlice(gpa, caret_index, buf);
                                        caret_index += width;
                                    }
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
