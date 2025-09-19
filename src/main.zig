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

/// The number of samples to use for MSAA. Set to 1 to disable.
const MSAA_SAMPLE_COUNT: u32 = 4;

/// --- Application State Struct ---
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

/// Helper to create or resize the MSAA texture and view.
fn createOrResizeMsaaTexture(d: *Demo) void {
    if (d.msaa_view != null) wgpu.textureViewRelease(d.msaa_view);
    if (d.msaa_texture != null) wgpu.textureRelease(d.msaa_texture);

    if (MSAA_SAMPLE_COUNT <= 1) { // Don't create if MSAA is off
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

/// --- Data Provider and Image Management ---
const ImageId = MultiAtlas.ImageId;

const LOGO_ID: ImageId = 1;
const FONT_ID_BASE: ImageId = 0x1000;

fn glyphId(char: u21) ImageId {
    return FONT_ID_BASE + char;
}

const AppContext = struct {
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
    } else if (image_id >= FONT_ID_BASE and image_id < FONT_ID_BASE + 0x10000) { // Increased range for safety
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

        // --- PADDING LOGIC ---
        // Create a 1-pixel transparent border around the glyph to prevent texture bleeding.
        const padded_w: u32 = @intCast(w + 2);
        const padded_h: u32 = @intCast(h + 2);

        const padded_bitmap = app.frame_arena.allocator().alloc(u8, padded_w * padded_h) catch {
            log.err("out of memory for padded glyph bitmap", .{});
            return null;
        };
        // Zero-initialize to create the transparent border.
        @memset(padded_bitmap, 0);

        // Copy the original glyph data into the center of the padded buffer.
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

/// Pre-caches all images and glyphs needed for a frame.
/// This function iterates through all drawable elements and calls `multi_atlas.query`
/// for them. The goal is not to get the location, but to populate the
/// multi_atlas's `pending_chains` list with everything that is currently missing.
fn precacheRequiredImages(
    renderer: *Batch2D,
    app_context: *AppContext,
) void {
    // 1. Precache the logo
    _ = renderer.multi_atlas.query(LOGO_ID, dataProvider, app_context) catch |err| {
        // We expect ImageNotYetPacked, so we can ignore it.
        // Any other error would be a real problem.
        if (err != error.ImageNotYetPacked) {
            log.err("unexpected error during logo precache: {any}", .{err});
        }
    };

    // 2. Precache the text glyphs
    const text_to_render = "WGpu Test";
    for (text_to_render) |char| {
        const char_code = @as(u21, @intCast(char));

        // Only query for glyphs that actually have a visual representation.
        var ix0: i32 = 0;
        var iy0: i32 = 0;
        var ix1: i32 = 0;
        var iy1: i32 = 0;
        stbtt.getCodepointBitmapBox(&app_context.font_info, @intCast(char_code), app_context.font_scale, app_context.font_scale, &ix0, &iy0, &ix1, &iy1);

        if ((ix1 - ix0) > 0 and (iy1 - iy0) > 0) {
            _ = renderer.multi_atlas.query(glyphId(char_code), dataProvider, app_context) catch |err| {
                if (err != error.ImageNotYetPacked) {
                    log.err("unexpected error during glyph precache: {any}", .{err});
                }
            };
        }
    }
}

/// --- Main Drawing Logic ---
/// Encapsulates all drawing calls for a single frame using the robust two-pass text layout system.
fn recordDrawCommands(
    renderer: *Batch2D,
    app_context: *AppContext,
    window: *glfw.Window,
) !void {
    // --- Draw the Logo ---
    const logo_location = renderer.multi_atlas.query(LOGO_ID, dataProvider, app_context) catch |err| {
        return err; // Propagate ImageNotYetPacked
    };

    var cursor_x: f64 = 0;
    var cursor_y: f64 = 0;
    glfw.getCursorPos(window, &cursor_x, &cursor_y);

    const image_scale = 0.2;
    const quad_width: f32 = @as(f32, @floatFromInt(app_context.logo_image.width)) * image_scale;
    const quad_height: f32 = @as(f32, @floatFromInt(app_context.logo_image.height)) * image_scale;

    const pos = Batch2D.Vec2{
        .x = @as(f32, @floatCast(cursor_x)) - quad_width / 2.0,
        .y = @as(f32, @floatCast(cursor_y)) - quad_height / 2.0,
    };
    const size = Batch2D.Vec2{ .x = quad_width, .y = quad_height };
    const tint = Batch2D.Color{ .r = 1, .g = 1, .b = 1, .a = 1 };

    try renderer.drawTexturedQuad(logo_location, pos, size, tint);

    // --- Draw the Text ---
    const text_to_render = "WGpu Test";
    const x_start: f32 = 0.0;
    const y_start: f32 = 0.0; // The desired top Y coordinate of the text block

    // --- PASS 1: Calculate the precise ascent for this specific string ---
    // This logic is correct and mirrors the principle used in wgpu_demo.zig.
    // It finds the highest point any glyph reaches to perfectly calculate the baseline position.
    var max_y1_unscaled: i32 = 0;
    for (text_to_render) |char| {
        const char_code = @as(u21, @intCast(char));
        var x0: i32 = 0;
        var y0: i32 = 0;
        var x1: i32 = 0;
        var y1: i32 = 0;
        _ = stbtt.getCodepointBox(&app_context.font_info, @intCast(char_code), &x0, &y0, &x1, &y1);
        if (y1 > max_y1_unscaled) {
            max_y1_unscaled = y1;
        }
    }
    const final_ascent = @as(f32, @floatFromInt(max_y1_unscaled)) * app_context.font_scale;
    const baseline_y = y_start + final_ascent;

    // --- PASS 2: Generate and draw the glyph quads ---
    // CORRECTED IMPLEMENTATION: This section is modified to use stbtt.getCodepointBitmapBox
    // for accurate, scaled pixel coordinates, just like the proven wgpu_demo.zig implementation
    // relies on for its final positioning.
    var xpos = x_start;
    var prev_char: u21 = 0;

    for (text_to_render) |char| {
        const char_code = @as(u21, @intCast(char));

        // Get kerning adjustment
        if (prev_char != 0) {
            const kern = stbtt.getCodepointKernAdvance(&app_context.font_info, prev_char, char_code);
            xpos += @as(f32, @floatFromInt(kern)) * app_context.font_scale;
        }

        // Get horizontal metrics for advancing the cursor.
        var adv: i32 = 0;
        var lsb: i32 = 0; // lsb is not used here, getCodepointBitmapBox provides the final x offset.
        stbtt.getCodepointHMetrics(&app_context.font_info, @intCast(char_code), &adv, &lsb);

        // Get the glyph's final bounding box in scaled pixel coordinates.
        // This is the key change to ensure the quad we draw matches the bitmap
        // generated by the dataProvider.
        var ix0: i32 = 0;
        var iy0: i32 = 0;
        var ix1: i32 = 0;
        var iy1: i32 = 0;
        stbtt.getCodepointBitmapBox(
            &app_context.font_info,
            @intCast(char_code),
            app_context.font_scale,
            app_context.font_scale,
            &ix0,
            &iy0,
            &ix1,
            &iy1,
        );

        const char_width = @as(f32, @floatFromInt(ix1 - ix0));
        const char_height = @as(f32, @floatFromInt(iy1 - iy0));

        // Draw the character ONLY if it has a visual representation.
        if (char_width > 0 and char_height > 0) {
            // iy0 is the pixel offset from the baseline to the top of the bitmap.
            // ix0 is the pixel offset from the cursor to the left of the bitmap.
            const char_pos = Batch2D.Vec2{
                .x = xpos + @as(f32, @floatFromInt(ix0)),
                .y = baseline_y + @as(f32, @floatFromInt(iy0)),
            };
            const char_size = Batch2D.Vec2{
                .x = char_width,
                .y = char_height,
            };

            const glyph_location = renderer.multi_atlas.query(glyphId(char_code), dataProvider, app_context) catch |err| {
                return err;
            };
            try renderer.drawTexturedQuad(glyph_location, char_pos, char_size, tint);
        }

        // Advance cursor position for the next character.
        xpos += @as(f32, @floatFromInt(adv)) * app_context.font_scale;
        prev_char = char_code;
    }
}

/// Creates a 4x4 orthographic projection matrix.
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

    // --- WGPU, GLFW, STB Initialization (Largely unchanged) ---
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
            .windows => if (glfw.isRunningInWine())
                wgpu.InstanceBackend.vulkanBackend
            else
                wgpu.InstanceBackend.dx12Backend,
            else => wgpu.InstanceBackend.vulkanBackend,
        },
    };
    demo.instance = wgpu.createInstance(&wgpu.InstanceDescriptor{
        .next_in_chain = @ptrCast(&instance_extras),
    });
    std.debug.assert(demo.instance != null);
    defer wgpu.instanceRelease(demo.instance);

    glfw.windowHint(.{ .client_api = .none });
    const window = try glfw.createWindow(640, 480, "gpu-quad [zig / Renderer]", null, null);
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

    _ = wgpu.instanceRequestAdapter(demo.instance, &wgpu.RequestAdapterOptions{ .compatible_surface = demo.surface }, .{
        .callback = &struct {
            fn handle_request_adapter(status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
                _ = ud2;
                if (status == .success) {
                    const d: *Demo = @ptrCast(@alignCast(ud1.?));
                    d.adapter = adapter;
                } else {
                    log.err("request_adapter failed: {s}", .{msg.toSlice()});
                }
            }
        }.handle_request_adapter,
        .userdata1 = &demo,
    });
    while (demo.adapter == null) wgpu.instanceProcessEvents(demo.instance);
    defer wgpu.adapterRelease(demo.adapter);

    _ = wgpu.adapterRequestDevice(demo.adapter, null, .{
        .callback = &struct {
            fn handle_request_device(status: wgpu.RequestDeviceStatus, device: wgpu.Device, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
                _ = ud2;
                if (status == .success) {
                    const d: *Demo = @ptrCast(@alignCast(ud1.?));
                    d.device = device;
                } else {
                    log.err("request_device failed: {s}", .{msg.toSlice()});
                }
            }
        }.handle_request_device,
        .userdata1 = &demo,
    });
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
        .present_mode = .fifo,
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

    // --- Initialize Renderer and AppContext ---
    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();

    var app_context = AppContext{
        .allocator = gpa,
        .logo_image = try stbi.Image.loadFromFile("assets/images/wgpu-logo.png", 4),
        .font_data = try std.fs.cwd().readFileAlloc(gpa, "assets/fonts/roboto/regular.ttf", 10 * 1024 * 1024),
        .font_info = .{},
        .font_scale = 0.0,
        .frame_arena = &arena_state,
    };
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

        // --- Drawing with the Renderer ---
        const proj = ortho(0, @floatFromInt(demo.config.width), @floatFromInt(demo.config.height), 0, -1, 1);
        demo.renderer.beginFrame(proj);

        // --- PASS 1: Pre-cache all required images ---
        // This populates the pending_chains list with everything we need for this frame.
        precacheRequiredImages(demo.renderer, &app_context);

        // --- PASS 2: Flush if necessary ---
        // If the pre-cache pass found any missing images, we generate their mipmaps
        // and upload them to the GPU in a single batch.
        if (demo.renderer.multi_atlas.pending_chains.items.len > 0) {
            // Mipmap Generation (this logic is moved from the old loop)
            const multi_atlas = demo.renderer.multi_atlas;
            for (multi_atlas.pending_chains.items) |*item| {
                if (item.chain.items.len > 1) continue;
                const base_image = item.chain.items[0];
                if (base_image.format != .rgba) continue;

                var current_w = base_image.width;
                var current_h = base_image.height;
                var last_pixels = base_image.pixels;

                while (@max(current_w, current_h) > 1 and item.chain.items.len < multi_atlas.mip_level_count) {
                    const next_w = @max(1, current_w / 2);
                    const next_h = @max(1, current_h / 2);

                    const resized_pixels = try app_context.frame_arena.allocator().alloc(u8, next_w * next_h * 4);
                    stbi.stbir_resize_uint8_srgb(
                        last_pixels.ptr,
                        @intCast(current_w),
                        @intCast(current_h),
                        0,
                        resized_pixels.ptr,
                        @intCast(next_w),
                        @intCast(next_h),
                        0,
                        4,
                    );

                    try item.chain.append(Atlas.InputImage{
                        .pixels = resized_pixels,
                        .width = next_w,
                        .height = next_h,
                        .format = .rgba,
                    });

                    current_w = next_w;
                    current_h = next_h;
                    last_pixels = resized_pixels;
                }
            }

            // Now that mip chains are complete, flush them all to the GPU.
            try multi_atlas.flush();
        }

        // --- PASS 3: Draw the frame ---
        // At this point, all required images are guaranteed to be in the atlas.
        // This section should no longer fail with `ImageNotYetPacked`.

        recordDrawCommands(demo.renderer, &app_context, window) catch |err| {
            // This should now be an unrecoverable error.
            log.err("unrecoverable error during recordDrawCommands: {any}", .{err});
            return err;
        };

        const render_target_view = if (demo.msaa_view != null) demo.msaa_view else frame_view;
        const resolve_target_view = if (demo.msaa_view != null) frame_view else null;
        const clear_color = Batch2D.Color{ .r = 0, .g = 0, .b = 1, .a = 1 };
        try demo.renderer.endFrame(render_target_view, resolve_target_view, clear_color);

        _ = wgpu.surfacePresent(demo.surface);
    }
}
