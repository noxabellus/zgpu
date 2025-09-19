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

// Demo struct and other helpers are unchanged...
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
const FONT_ID_BASE: ImageId = 0x1000;
fn glyphId(char: u21) ImageId {
    return FONT_ID_BASE + char;
}
pub const AppContext = struct {
    allocator: std.mem.Allocator,
    logo_image: stbi.Image,
    font_info: stbtt.FontInfo,
    font_data: []const u8,
    font_scale: f32,
    frame_arena: *std.heap.ArenaAllocator,
};
fn dataProvider(image_id: ImageId, user_context: ?*anyopaque) ?Atlas.InputImage {
    const app: *AppContext = @ptrCast(@alignCast(user_context.?));

    if (image_id == LOGO_ID) {
        return Atlas.InputImage{
            .pixels = app.logo_image.data,
            .width = app.logo_image.width,
            .height = app.logo_image.height,
            .format = .rgba,
        };
    } else if (image_id >= FONT_ID_BASE and image_id < FONT_ID_BASE + 0x10000) {
        const char_code = @as(u21, @intCast(image_id - FONT_ID_BASE));
        var w: i32 = 0;
        var h: i32 = 0;
        var xoff: i32 = 0;
        var yoff: i32 = 0;

        const grayscale_pixels = stbtt.getCodepointBitmap(
            &app.font_info,
            0,
            app.font_scale,
            @intCast(char_code),
            &w,
            &h,
            &xoff,
            &yoff,
        );

        if (grayscale_pixels == null or w == 0 or h == 0) {
            return null;
        }
        defer stbtt.freeBitmap(grayscale_pixels.?, null);

        const padded_w: u32 = @intCast(w + 2);
        const padded_h: u32 = @intCast(h + 2);

        const padded_bitmap = app.frame_arena.allocator().alloc(u8, padded_w * padded_h) catch {
            log.err("out of memory for padded glyph bitmap", .{});
            return null;
        };
        @memset(padded_bitmap, 0);

        const original_pitch: usize = @intCast(w);
        const padded_pitch: usize = @intCast(padded_w);
        for (0..@as(usize, @intCast(h))) |row| {
            const src = grayscale_pixels.?[row * original_pitch .. (row + 1) * original_pitch];
            const dst = padded_bitmap[(row + 1) * padded_pitch + 1 .. (row + 1) * padded_pitch + 1 + original_pitch];
            @memcpy(dst, src);
        }

        return Atlas.InputImage{
            .pixels = padded_bitmap,
            .width = padded_w,
            .height = padded_h,
            .format = .grayscale,
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

pub fn main() !void {
    const gpa = std.heap.page_allocator;

    // --- WGPU, GLFW, STB Initialization (unchanged) ---
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
    var app_context = AppContext{ .allocator = gpa, .logo_image = try stbi.Image.loadFromFile("assets/images/wgpu-logo.png", 4), .font_data = try std.fs.cwd().readFileAlloc(gpa, "assets/fonts/roboto/regular.ttf", 10 * 1024 * 1024), .font_info = .{}, .font_scale = 0.0, .frame_arena = &arena_state };
    defer app_context.logo_image.deinit();
    defer gpa.free(app_context.font_data);
    std.debug.assert(stbtt.initFont(&app_context.font_info, app_context.font_data.ptr, 0) != 0);
    app_context.font_scale = stbtt.scaleForPixelHeight(&app_context.font_info, 40.0);
    demo.renderer = try Batch2D.init(gpa, demo.device, queue, surface_format, MSAA_SAMPLE_COUNT);
    defer demo.renderer.deinit();

    // --- Main Loop ---
    main_loop: while (!glfw.windowShouldClose(window)) {
        glfw.pollEvents();
        _ = app_context.frame_arena.reset(.free_all);

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
        demo.renderer.beginFrame(proj);

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

        const text_pos = Batch2D.Vec2{ .x = 0.0, .y = 0.0 };
        try demo.renderer.drawText("WGpu Test", &app_context.font_info, app_context.font_scale, text_pos, tint);

        const prepare_context = Batch2D.PrepareContext{ .provider = dataProvider, .provider_context = &app_context };
        try demo.renderer.prepare(prepare_context);

        const encoder = wgpu.deviceCreateCommandEncoder(demo.device, &.{ .label = .fromSlice("main_encoder") });
        defer wgpu.commandEncoderRelease(encoder);

        const render_target_view = if (demo.msaa_view != null) demo.msaa_view else frame_view;
        const resolve_target_view = if (demo.msaa_view != null) frame_view else null;
        const clear_color = Batch2D.Color{ .r = 0, .g = 0, .b = 1, .a = 1 };

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
