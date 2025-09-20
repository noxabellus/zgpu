//! Test of Batch2d

const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");

const wgpu = @import("wgpu");
const stbi = @import("stbi");
const stbtt = @import("stbtt");
const glfw = @import("glfw");

const Batch2D = @import("Batch2D.zig");
const Atlas = @import("Atlas.zig");
const MultiAtlas = @import("MultiAtlas.zig");
const AssetCache = @import("AssetCache.zig");

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

pub fn main() !void {
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
    demo.config = .{ .device = demo.device, .usage = .renderAttachmentUsage, .format = surface_format, .present_mode = .fifo, .alpha_mode = surface_capabilities.alpha_modes.?[0] };
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
    const roboto_idx = try asset_cache.loadFont("assets/fonts/roboto/regular.ttf");
    const calistoga_idx = try asset_cache.loadFont("assets/fonts/Calistoga-Regular.ttf");
    const quicksand_idx = try asset_cache.loadFont("assets/fonts/Quicksand-Semibold.ttf");

    // Load images via the asset cache
    const LOGO_ID = try asset_cache.loadImage("assets/images/wgpu-logo.png");
    const BANNER_ID = try asset_cache.loadImage("assets/images/ribbon-banner.png");
    const EMBLEM_ID = try asset_cache.loadImage("assets/images/ribbon-emblem.png");
    const BOW_ID = try asset_cache.loadImage("assets/images/tiny-bow-icon.png");

    demo.renderer = try Batch2D.init(gpa, demo.device, queue, surface_format, MSAA_SAMPLE_COUNT);
    defer demo.renderer.deinit();

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

        const proj = ortho(0, @floatFromInt(demo.config.width), @floatFromInt(demo.config.height), 0, -1, 1);

        // Create the context struct for this frame.
        const provider_user_context = AssetCache.ProviderUserContext{
            .asset_cache = &asset_cache,
            .frame_allocator = arena_state.allocator(),
        };

        const provider_ctx = Batch2D.ProviderContext{
            .provider = AssetCache.dataProvider,
            // a generic API requires a non-const pointer; this cast is still safe, as the provider we're using does not mutate the context.
            .user_context = @constCast(&provider_user_context),
        };
        demo.renderer.beginFrame(proj, provider_ctx);

        const tint = Batch2D.Color{ .r = 1, .g = 1, .b = 1, .a = 1 };

        // Draw the WGPU logo at the cursor
        var cursor_x: f64 = 0;
        var cursor_y: f64 = 0;
        glfw.getCursorPos(window, &cursor_x, &cursor_y);
        const logo_img = asset_cache.images.items[LOGO_ID];
        const image_scale = 0.2;
        const quad_width: f32 = @as(f32, @floatFromInt(logo_img.width)) * image_scale;
        const quad_height: f32 = @as(f32, @floatFromInt(logo_img.height)) * image_scale;
        const quad_pos = Batch2D.Vec2{ .x = @as(f32, @floatCast(cursor_x)) - quad_width / 2.0, .y = @as(f32, @floatCast(cursor_y)) - quad_height / 2.0 };
        const quad_size = Batch2D.Vec2{ .x = quad_width, .y = quad_height };
        try demo.renderer.drawTexturedQuad(LOGO_ID, true, quad_pos, quad_size, tint);

        const banner_img = asset_cache.images.items[BANNER_ID];
        try demo.renderer.drawTexturedQuad(BANNER_ID, true, .{ .x = 100, .y = 200 }, .{ .x = @floatFromInt(@divFloor(banner_img.width, 4)), .y = @floatFromInt(@divFloor(banner_img.height, 4)) }, tint);

        const emblem_img = asset_cache.images.items[EMBLEM_ID];
        try demo.renderer.drawTexturedQuad(EMBLEM_ID, true, .{ .x = 500, .y = 50 }, .{ .x = @floatFromInt(emblem_img.width), .y = @floatFromInt(emblem_img.height) }, tint);

        const bow_img = asset_cache.images.items[BOW_ID];
        try demo.renderer.drawTexturedQuad(BOW_ID, true, .{ .x = 400, .y = 375 }, .{ .x = @floatFromInt(bow_img.width), .y = @floatFromInt(bow_img.height) }, tint);

        const text_to_draw = "WGPU Batch Renderer";

        // Draw with Roboto, 40px
        try demo.renderer.drawText(text_to_draw, &asset_cache.fonts.items[roboto_idx].info, roboto_idx, 40.0, .{ .x = 0, .y = 0 }, .{ .r = 1, .g = 1, .b = 1, .a = 1 });

        // Draw with Calistoga, 48px
        try demo.renderer.drawText(text_to_draw, &asset_cache.fonts.items[calistoga_idx].info, calistoga_idx, 48.0, .{ .x = 10, .y = 60 }, .{ .r = 0, .g = 1, .b = 1, .a = 1 });

        // Draw with Quicksand, 32px
        try demo.renderer.drawText(text_to_draw, &asset_cache.fonts.items[quicksand_idx].info, quicksand_idx, 32.0, .{ .x = 20, .y = 120 }, .{ .r = 1, .g = 0, .b = 1, .a = 1 });

        try demo.renderer.endFrame();

        const encoder = wgpu.deviceCreateCommandEncoder(demo.device, &.{ .label = .fromSlice("main_encoder") });
        defer wgpu.commandEncoderRelease(encoder);

        const render_target_view = if (demo.msaa_view != null) demo.msaa_view else frame_view;
        const resolve_target_view = if (demo.msaa_view != null) frame_view else null;
        const clear_color = wgpu.Color{ .b = 0.5, .a = 1 };

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
    }
}
