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

const ImageId = MultiAtlas.ImageId;
const LOGO_ID: ImageId = 1;

const LoadedFont = struct {
    info: stbtt.FontInfo,
    data: []const u8,
};

pub const AppContext = struct {
    allocator: std.mem.Allocator,
    logo_image: stbi.Image,
    fonts: std.ArrayList(LoadedFont),
    frame_arena: *std.heap.ArenaAllocator,
};

// The dataProvider returns a single InputImage.
// It now checks the ImageId to see if it's a glyph or a standard image.
fn dataProvider(image_id: ImageId, user_context: ?*anyopaque) ?Atlas.InputImage {
    const app: *AppContext = @ptrCast(@alignCast(user_context.?));
    const frame_allocator = app.frame_arena.allocator();

    if ((image_id & Batch2D.GLYPH_ID_FLAG) != 0) {
        // This is a glyph request.
        const decoded = Batch2D.decodeGlyphId(image_id);
        if (decoded.font_index >= app.fonts.items.len) {
            log.err("invalid font index {d} in glyph id", .{decoded.font_index});
            return null;
        }
        const font = &app.fonts.items[decoded.font_index];
        const scale = stbtt.scaleForPixelHeight(&font.info, decoded.pixel_height);

        var w: i32 = 0;
        var h: i32 = 0;
        var xoff: i32 = 0;
        var yoff: i32 = 0;
        const grayscale_pixels = stbtt.getCodepointBitmap(&font.info, 0, scale, @intCast(decoded.char_code), &w, &h, &xoff, &yoff);

        if (grayscale_pixels == null or w == 0 or h == 0) return null;
        defer stbtt.freeBitmap(grayscale_pixels.?, null);

        const GLYPH_PADDING: u32 = 2;

        const padded_w: u32 = @as(u32, @intCast(w)) + (GLYPH_PADDING * 2);
        const padded_h: u32 = @as(u32, @intCast(h)) + (GLYPH_PADDING * 2);
        const master_rgba_padded_pixels = (frame_allocator.alloc(u8, padded_w * padded_h * 4) catch return null);

        // 1. Clear the entire buffer to WHITE {255, 255, 255, 0}.
        //    The memset makes RGB white. We will then clear alpha manually.
        @memset(master_rgba_padded_pixels, 0xFF);
        for (0..padded_w * padded_h) |i| {
            master_rgba_padded_pixels[i * 4 + 3] = 0;
        }

        // 2. Copy the glyph data, setting only the Alpha channel over our white background.
        const original_pitch: usize = @intCast(w);
        const padded_pitch: usize = padded_w * 4;
        for (0..@as(usize, @intCast(h))) |row| {
            const src_row = grayscale_pixels.?[(row * original_pitch)..];
            const dst_row_start_idx = ((row + GLYPH_PADDING) * padded_pitch) + (GLYPH_PADDING * 4);
            for (0..original_pitch) |col| {
                const alpha = src_row[col];
                const dst_pixel_start = dst_row_start_idx + (col * 4);

                // RGB is already 255 from the memset above. We only need to write alpha.
                master_rgba_padded_pixels[dst_pixel_start + 3] = alpha; // A
            }
        }
        return Atlas.InputImage{
            .pixels = master_rgba_padded_pixels,
            .width = padded_w,
            .height = padded_h,
            .format = .rgba,
        };
    } else {
        return Atlas.InputImage{
            .pixels = app.logo_image.data,
            .width = app.logo_image.width,
            .height = app.logo_image.height,
            .format = .rgba,
        };
    }

    return null;
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
    var app_context = AppContext{
        .allocator = gpa,
        .logo_image = try stbi.Image.loadFromFile("assets/images/wgpu-logo.png", 4),
        .fonts = .empty,
        .frame_arena = &arena_state,
    };
    defer app_context.logo_image.deinit();
    defer {
        for (app_context.fonts.items) |font| {
            gpa.free(font.data);
        }
        app_context.fonts.deinit(app_context.allocator);
    }

    // Load fonts
    const font_paths = &[_][]const u8{
        "assets/fonts/roboto/regular.ttf",
        "assets/fonts/Calistoga-Regular.ttf",
        "assets/fonts/Quicksand-Semibold.ttf",
    };
    for (font_paths) |path| {
        const font_data = try std.fs.cwd().readFileAlloc(gpa, path, 10 * 1024 * 1024);
        var font_info = stbtt.FontInfo{};
        const success = stbtt.initFont(&font_info, font_data.ptr, 0).to();
        std.debug.assert(success);
        try app_context.fonts.append(app_context.allocator, .{ .data = font_data, .info = font_info });
    }

    demo.renderer = try Batch2D.init(gpa, demo.device, queue, surface_format, MSAA_SAMPLE_COUNT);
    defer demo.renderer.deinit();

    // --- Main Loop ---
    main_loop: while (!glfw.windowShouldClose(window)) {
        glfw.pollEvents();
        _ = app_context.frame_arena.reset(.free_all);

        // --- DEBUG DUMP ---
        // It will run once and then panic, which is fine for debugging.
        if (glfw.getKey(window, .a) == .press) {
            try demo.renderer.glyph_atlas.debugWriteAllAtlasesToPng("debug_glyph_atlas");
            try demo.renderer.image_atlas.debugWriteAllAtlasesToPng("debug_image_atlas");
            log.warn("Wrote atlases to disk, exiting.", .{});
            std.process.exit(0);
        }
        // --- END DEBUG DUMP ---

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
        const provider_ctx = Batch2D.ProviderContext{ .provider = dataProvider, .user_context = &app_context };
        demo.renderer.beginFrame(proj, provider_ctx);

        var cursor_x: f64 = 0;
        var cursor_y: f64 = 0;
        glfw.getCursorPos(window, &cursor_x, &cursor_y);
        const image_scale = 0.2;
        const quad_width: f32 = @as(f32, @floatFromInt(app_context.logo_image.width)) * image_scale;
        const quad_height: f32 = @as(f32, @floatFromInt(app_context.logo_image.height)) * image_scale;
        const quad_pos = Batch2D.Vec2{ .x = @as(f32, @floatCast(cursor_x)) - quad_width / 2.0, .y = @as(f32, @floatCast(cursor_y)) - quad_height / 2.0 };
        const quad_size = Batch2D.Vec2{ .x = quad_width, .y = quad_height };
        const tint = Batch2D.Color{ .r = 1, .g = 1, .b = 1, .a = 1 };
        try demo.renderer.drawTexturedQuad(LOGO_ID, quad_pos, quad_size, tint);

        const text_to_draw = "WGPU Batch Renderer";

        // Draw with Roboto, 40px
        try demo.renderer.drawText(text_to_draw, &app_context.fonts.items[0].info, 0, 40.0, .{ .x = 0, .y = 0 }, .{ .r = 1, .g = 1, .b = 1, .a = 1 });

        // Draw with Calistoga, 48px
        try demo.renderer.drawText(text_to_draw, &app_context.fonts.items[1].info, 1, 48.0, .{ .x = 10, .y = 60 }, .{ .r = 0, .g = 1, .b = 1, .a = 1 });

        // Draw with Quicksand, 32px
        try demo.renderer.drawText(text_to_draw, &app_context.fonts.items[2].info, 2, 32.0, .{ .x = 20, .y = 120 }, .{ .r = 1, .g = 0, .b = 1, .a = 1 });

        try demo.renderer.endFrame();

        const encoder = wgpu.deviceCreateCommandEncoder(demo.device, &.{ .label = .fromSlice("main_encoder") });
        defer wgpu.commandEncoderRelease(encoder);

        const render_target_view = if (demo.msaa_view != null) demo.msaa_view else frame_view;
        const resolve_target_view = if (demo.msaa_view != null) frame_view else null;
        const clear_color = Batch2D.Color{ .b = 0.1, .a = 1 };

        const render_pass = wgpu.commandEncoderBeginRenderPass(encoder, &.{
            .color_attachment_count = 1,
            .color_attachments = &.{.{
                .view = render_target_view,
                .resolve_target = resolve_target_view,
                .load_op = .clear,
                .store_op = .store,
                .clear_value = .{ .r = clear_color.r, .g = clear_color.g, .b = clear_color.b, .a = clear_color.a },
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
