//! An example of the new UI wrapper over ui.

const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");

const wgpu = @import("wgpu");
const stbi = @import("stbi");
const glfw = @import("glfw");

pub const debug = @import("../debug.zig");
pub const Batch2D = @import("../Batch2D.zig");
pub const AssetCache = @import("../AssetCache.zig");
pub const InputState = @import("../InputState.zig");
pub const BindingState = @import("../BindingState.zig");
pub const Ui = @import("../Ui.zig");
pub const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

test {
    log.debug("semantic analysis for examples/bare.zig", .{});
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

fn createLayout(_: *Ui) !void {}

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

    const frame_arena = arena_state.allocator();

    // Init Asset Cache
    var asset_cache = AssetCache.init(gpa);
    defer asset_cache.deinit();

    // Init Batch Renderer
    demo.renderer = try Batch2D.init(gpa, demo.device, queue, surface_format, &asset_cache, MSAA_SAMPLE_COUNT);
    defer demo.renderer.deinit();

    var inputs = InputState.init(window);
    inputs.listenAllGlfw();

    var bindings = BindingState.init(gpa, &inputs);
    defer bindings.deinit();

    try bindings.bind(.toggle_debugger, .{ .key = .{ .bind_point = .d, .modifiers = .ctrlMod } });
    try bindings.bind(.dump_atlas, .{ .key = .{ .bind_point = .a, .modifiers = .altMod } });
    try bindings.bind(.open_context_menu, .{ .mouse = .{ .bind_point = .button_2 } });

    // Init Ui
    var ui = try Ui.init(gpa, frame_arena, demo.renderer, &asset_cache, &bindings);
    defer ui.deinit();

    _ = try asset_cache.loadFont("assets/fonts/quicksand/regular.ttf");

    try demo.renderer.preAtlasAllImages();

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
        if (bindings.getAction(.toggle_debugger) == .pressed) {
            debug_mode_enabled = !debug_mode_enabled;
            ui.setDebugMode(debug_mode_enabled);
        }

        if (bindings.getAction(.dump_atlas) == .pressed) {
            try demo.renderer.atlas.debugWriteAllAtlasesToPng("debug_atlas");
            log.info("finished writing debug_atlas_*.png", .{});
        }

        // --- Context menu ---
        if (bindings.getAction(.open_context_menu) == .released) {
            log.info("Right click released, opening context menu.", .{});
            ui.openMenu(.fromSlice("ContextMenuRoot"), .{
                .position = inputs.getMousePosition(),
            });
        }

        // --- Update Clay UI ---
        glfw.getFramebufferSize(window, &window_width, &window_height);

        inputs.collectAllGlfw();

        // Generate the UI layout and cache rendering commands
        {
            try ui.beginLayout(
                .{ @floatFromInt(window_width), @floatFromInt(window_height) },
                delta_time_f32,
            );

            try createLayout(ui);

            try ui.endLayout();
        }

        try ui.dispatchEvents();

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
            const proj = linalg.mat4_ortho(0, @floatFromInt(demo.config.width), @floatFromInt(demo.config.height), 0, -1, 1);
            demo.renderer.beginFrame(proj, demo.config.width, demo.config.height);

            try ui.render();

            try debug.drawFpsChart(demo.renderer, .{ 0, 0 });

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

        // block to create 10fps for debugging
        // const fps_target_ns = std.time.ns_per_s / 10;
        // while (debug_timer.read() < fps_target_ns) {}

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
