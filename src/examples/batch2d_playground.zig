//! Test of Batch2d

const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");

const wgpu = @import("wgpu");
const stbi = @import("stbi");
const stbtt = @import("stbtt");
const glfw = @import("glfw");
const clay = @import("clay");

const Batch2D = @import("../Batch2D.zig");
const AssetCache = @import("../AssetCache.zig");
const debug = @import("../debug.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

test {
    log.debug("semantic analysis for examples/batch2d_playground.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const std_options = std.Options{
    .log_level = .info,
};

const MSAA_SAMPLE_COUNT: u32 = 4;

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

var tts_buf: [1024]u8 = undefined;

pub fn main() !void {
    var timer = try std.time.Timer.start();

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

    const instance_extras = wgpu.InstanceExtras{ .chain = .{ .s_type = .instance_extras }, .backends = switch (builtin.os.tag) {
        .windows => if (glfw.isRunningInWine()) wgpu.InstanceBackend.vulkanBackend else wgpu.InstanceBackend.dx12Backend,
        else => wgpu.InstanceBackend.vulkanBackend,
    } };
    demo.instance = wgpu.createInstance(&wgpu.InstanceDescriptor{ .next_in_chain = @ptrCast(&instance_extras) });
    std.debug.assert(demo.instance != null);
    defer wgpu.instanceRelease(demo.instance);

    glfw.windowHint(.{ .client_api = .none });
    const window = try glfw.createWindow(640, 480, "zgpu", null, null);
    defer glfw.destroyWindow(window);
    glfw.setWindowUserPointer(window, &demo);

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
        fn handle_request_adapter(status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
            _ = ud2;
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
        fn handle_request_device(status: wgpu.RequestDeviceStatus, device: wgpu.Device, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
            _ = ud2;
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

    {
        var width: i32 = 0;
        var height: i32 = 0;
        glfw.getWindowSize(window, &width, &height);
        demo.config.width = @intCast(width);
        demo.config.height = @intCast(height);
    }
    wgpu.surfaceConfigure(demo.surface, &demo.config);
    createOrResizeMsaaTexture(&demo);

    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();

    var asset_cache = AssetCache.init(gpa);
    defer asset_cache.deinit();

    // Load fonts via the asset cache
    const roboto_id = try asset_cache.loadFont("assets/fonts/roboto/regular.ttf");
    const calistoga_id = try asset_cache.loadFont("assets/fonts/calistoga/regular.ttf");
    const quicksand_id = try asset_cache.loadFont("assets/fonts/quicksand/semibold.ttf");

    // Load images via the asset cache
    const LOGO_ID = try asset_cache.loadImage("assets/images/wgpu-logo.png", true);
    const BANNER_ID = try asset_cache.loadImage("assets/images/ribbon-banner.png", true);
    const EMBLEM_ID = try asset_cache.loadImage("assets/images/ribbon-emblem.png", true);
    const BOW_ID = try asset_cache.loadImage("assets/images/tiny-bow-icon.png", true);
    const PIXELART_ID = try asset_cache.loadImage("assets/images/ribbon-emblem-pixelart.png", false);

    demo.renderer = try Batch2D.init(gpa, demo.device, queue, surface_format, &asset_cache, MSAA_SAMPLE_COUNT);
    defer demo.renderer.deinit();

    const startup_ms = debug.start(&timer);

    log.info("startup completed in {d} ms", .{startup_ms});

    // --- Main Loop ---
    main_loop: while (!glfw.windowShouldClose(window)) {
        glfw.pollEvents();
        _ = arena_state.reset(.free_all);

        if (glfw.getKey(window, .a) == .press) {
            try demo.renderer.atlas.debugWriteAllAtlasesToPng("debug_atlas");
            log.info("finished writing debug_atlas_*.png", .{});
        }

        var surface_texture: wgpu.SurfaceTexture = undefined;
        wgpu.surfaceGetCurrentTexture(demo.surface, &surface_texture);
        switch (surface_texture.status) {
            .success_optimal, .success_suboptimal => {},
            .timeout, .outdated, .lost => {
                if (surface_texture.texture != null) wgpu.textureRelease(surface_texture.texture);
                var width: i32 = 0;
                var height: i32 = 0;
                glfw.getWindowSize(window, &width, &height);
                if (width != 0 and height != 0) {
                    demo.config.width = @intCast(width);
                    demo.config.height = @intCast(height);
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

        const proj = linalg.mat4_ortho(0, @floatFromInt(demo.config.width), @floatFromInt(demo.config.height), 0, -1, 1);

        demo.renderer.beginFrame(proj, demo.config.width, demo.config.height);

        const tint = Batch2D.Color{ .r = 1, .g = 1, .b = 1, .a = 1 };

        // Draw the WGPU logo at the cursor
        var cursor_x: f64 = 0;
        var cursor_y: f64 = 0;
        glfw.getCursorPos(window, &cursor_x, &cursor_y);
        const logo_img = asset_cache.images.items[LOGO_ID];
        const image_scale = 0.2;
        const quad_width: f32 = @as(f32, @floatFromInt(logo_img.content.width)) * image_scale;
        const quad_height: f32 = @as(f32, @floatFromInt(logo_img.content.height)) * image_scale;
        const quad_pos = vec2{ @as(f32, @floatCast(cursor_x)) - quad_width / 2.0, @as(f32, @floatCast(cursor_y)) - quad_height / 2.0 };
        const quad_size = vec2{ quad_width, quad_height };
        try demo.renderer.drawTexturedQuad(LOGO_ID, quad_pos, quad_size, null, tint);

        const banner_img = asset_cache.images.items[BANNER_ID];
        try demo.renderer.drawTexturedQuad(BANNER_ID, .{ 100, 200 }, .{ @floatFromInt(@divFloor(banner_img.content.width, 4)), @floatFromInt(@divFloor(banner_img.content.height, 4)) }, null, tint);

        const emblem_img = asset_cache.images.items[EMBLEM_ID];
        try demo.renderer.drawTexturedQuad(EMBLEM_ID, .{ 500, 50 }, .{ @floatFromInt(emblem_img.content.width), @floatFromInt(emblem_img.content.height) }, null, tint);

        const bow_img = asset_cache.images.items[BOW_ID];
        try demo.renderer.drawTexturedQuad(BOW_ID, .{ 400, 375 }, .{ @floatFromInt(bow_img.content.width), @floatFromInt(bow_img.content.height) }, null, tint);

        var tts_fba = std.heap.FixedBufferAllocator.init(&tts_buf);

        const fps_text = try std.fmt.allocPrint(tts_fba.allocator(), "FPS: {d:0.1} ({d:0.3}ms) / {d:0.1} ({d:0.3}ms) / {d:0.3}ms : {d:0.3}ms", .{ debug.avg_fps, debug.avg_ms, debug.frame_fps, debug.frame_ms, debug.min_ms, debug.max_ms });

        try debug.drawFpsChart(demo.renderer, .{ 18, 18 });

        // --- Draw Text ---

        // Draw with Roboto, 12px
        try demo.renderer.drawText(fps_text, roboto_id, 16, null, .{ 0, 0 }, .black);

        // Draw with Calistoga, 48px
        try demo.renderer.drawText("WGPU Batch Renderer", calistoga_id, 48, null, .{ 10, 60 }, .cyan);

        // Draw with Quicksand, 32px
        try demo.renderer.drawText("Newline\nin\nText", quicksand_id, 32, 20, .{ 20, 120 }, .magenta);

        // --- Draw Primitives ---

        // Draw a solid red quad
        try demo.renderer.drawQuad(.{ 250, 150 }, .{ 50, 50 }, .red);

        // Draw a solid green triangle
        try demo.renderer.drawTriangle(.{ 320, 150 }, .{ 370, 300 }, .{ 270, 300 }, .green);

        // Draw a thick blue line
        try demo.renderer.drawLine(.{ 250, 120 }, .{ 370, 320 }, 5.0, .blue);

        // Draw a yellow circle
        try demo.renderer.drawCircle(.{ 100, 100 }, 30.0, .yellow);

        // Draw a cyan triangle strip
        const strip_verts = &[_]vec2{
            .{ 400, 150 },
            .{ 420, 180 },
            .{ 440, 150 },
            .{ 460, 180 },
            .{ 480, 150 },
        };
        try demo.renderer.drawSolidTriangleStrip(strip_verts, .{ .r = 0, .g = 1, .b = 1, .a = 1 });

        // Draw a filled, half-pie shape. Angles are in radians (Pi = 180 degrees).
        try demo.renderer.drawArc(
            .{ 150.0, 150.0 }, // center
            80.0, // radius
            0.0, // start_angle (0 is the 3 o'clock position)
            std.math.pi, // end_angle (Pi is the 9 o'clock position)
            .red,
        );

        // Draw just the outline of an arc, like a progress bar.
        try demo.renderer.drawArcLine(
            .{ 400.0, 150.0 }, // center
            70.0, // radius
            0.0, // start_angle
            1.5 * std.math.pi, // end_angle (270 degrees)
            15.0, // thickness
            .blue,
        );

        // Draw a smaller, more acute filled arc.
        try demo.renderer.drawArc(
            .{ 150.0, 400.0 }, // center
            90.0, // radius
            0.0, // start_angle
            std.math.pi / 4.0, // end_angle (45 degrees)
            .green,
        );

        // Draw from -45 to +45 degrees, testing the angle normalization logic.
        try demo.renderer.drawArcLine(
            .{ 400.0, 400.0 }, // center
            75.0, // radius
            -std.math.pi / 4.0, // start_angle (-45 degrees)
            std.math.pi / 4.0, // end_angle (+45 degrees)
            8.0, // thickness
            .yellow,
        );

        // A thin white outline around the blue rectangle.
        try demo.renderer.drawRectLine(.{ 50, 50 }, .{ 150, 100 }, 2.0, .white);

        // A thicker red outline on a different rectangle.
        try demo.renderer.drawRectLine(.{ 250, 50 }, .{ 200, 75 }, 8.0, .red);

        // A green rounded rectangle with a moderate corner radius.
        try demo.renderer.drawRoundedRect(.{ 50, 200 }, .{ 200, 100 }, .all(20.0), .green);

        // A rounded square that becomes a circle because radius >= size/2.
        try demo.renderer.drawRoundedRect(.{ 300, 200 }, .{ 100, 100 }, .all(50.0), .yellow);

        // A "pill" shape where the radius is clamped to half the height.
        try demo.renderer.drawRoundedRect(.{ 50, 350 }, .{ 350, 100 }, .all(100.0), Batch2D.Color.red.withAlpha(0.8));

        // A thin outline for the green rounded rectangle.
        try demo.renderer.drawRoundedRectLine(.{ 50, 200 }, .{ 200, 100 }, .all(20.0), 3.0, .white);

        // A thick outline for the "pill" shape.
        try demo.renderer.drawRoundedRectLine(.{ 50, 350 }, .{ 350, 100 }, .all(100.0), 10.0, .blue);

        // An outlined circle made from a rounded rect line.
        try demo.renderer.drawRoundedRectLine(.{ 300, 200 }, .{ 100, 100 }, .all(50.0), 5.0, .magenta);

        // A case where thickness is large relative to the radius. The path_radius will
        // be clamped, resulting in a shape with a rounded outer edge but a sharp inner corner.
        try demo.renderer.drawRoundedRectLine(.{ 50, 0 }, .{ 150, 100 }, .all(15.0), 25.0, .yellow);

        // Test of per-corner radius values
        try demo.renderer.drawRoundedRectLine(.{ 350, 100 }, .{ 150, 100 }, .{
            .top_left = 30.0,
            .top_right = 10.0,
            .bottom_right = 50.0,
            .bottom_left = 5.0,
        }, 5.0, .magenta);

        // --- Scissor Test ---
        try demo.renderer.scissorStart(.{ 450, 250 }, .{ 150, 150 });
        {
            // This quad is fully inside the scissor rect and will be drawn normally.
            try demo.renderer.drawQuad(.{ 460, 260 }, .{ 50, 50 }, .blue);

            // This quad is partially overlapping the scissor rect and will be clipped.
            try demo.renderer.drawQuad(.{ 550, 350 }, .{ 400, 400 }, .red);

            // This quad is completely outside the scissor rect and will not be visible.
            try demo.renderer.drawQuad(.{ 200, 200 }, .{ 50, 50 }, .green);
        }
        try demo.renderer.scissorEnd();

        // Draw a white outline around the scissor area to visualize it
        try demo.renderer.drawRectLine(.{ 450, 250 }, .{ 150, 150 }, 1.0, .white);

        // draw a rounded rect with a texture
        try demo.renderer.drawRoundedTexturedQuad(PIXELART_ID, .{ 500, 300 }, .{ 128, 128 }, .all(20.0), null, tint);

        // --- End Frame ---

        try demo.renderer.endFrame();

        const encoder = wgpu.deviceCreateCommandEncoder(demo.device, &.{ .label = .fromSlice("main_encoder") });
        defer wgpu.commandEncoderRelease(encoder);

        const render_target_view = if (demo.msaa_view != null) demo.msaa_view else frame_view;
        const resolve_target_view = if (demo.msaa_view != null) frame_view else null;
        const clear_color = wgpu.Color{ .r = 0.1, .g = 0.1, .b = 0.1, .a = 1 };

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
