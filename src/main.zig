const std = @import("std");
const log = std.log.scoped(.main);

const builtin = @import("builtin");

const wgpu = @import("wgpu.zig");
const glfw = @import("glfw.zig");
const stbi = @import("stbi.zig");

pub const std_options = std.Options{
    .log_level = .warn,
};

test {
    log.debug("semantic analysis for main.zig", .{});
    std.testing.refAllDecls(@This());
}

/// --- Application State Struct ---
/// This struct holds all the core WGPU objects that we need to manage throughout the
/// application's lifecycle. It's passed around so that different parts of the code
/// (like window callbacks) can access the graphics state.
const Demo = struct {
    /// The top-level WGPU object, representing a connection to the WGPU implementation.
    instance: wgpu.Instance = null,
    /// The platform-native surface (e.g., a window) that we will draw to.
    surface: wgpu.Surface = null,
    /// A handle to a physical graphics device (e.g., a specific GPU).
    adapter: wgpu.Adapter = null,
    /// A logical connection to the adapter, used for creating resources like buffers and textures.
    device: wgpu.Device = null,
    /// Configuration for the surface, defining how it's presented (size, format, etc.).
    config: wgpu.SurfaceConfiguration = .{},
};

// --- Vertex and Uniform Data Structs ---
// `extern struct` ensures a C-compatible memory layout, which is important when passing
// data to the GPU, as the graphics driver is typically a C/C++ library.

/// Defines the layout of data for a single vertex. Each vertex in our quad
/// will have a 2D position and a 2D texture coordinate.
const Vertex = extern struct {
    /// The (x, y) position in screen space.
    position: [2]f32,
    /// The (u, v) texture coordinates.
    tex_coords: [2]f32,
};

/// Defines the layout for our uniform data. Uniforms are variables that are constant
/// for all vertices/fragments in a single draw call.
const Uniforms = extern struct {
    /// The current resolution of the window, used in the shader to convert
    /// from pixel coordinates to normalized device coordinates.
    resolution: [2]f32,
};

pub fn main() !void {
    // --- WGPU Logging Setup ---
    // WGPU can provide detailed logging information. We set up a callback function
    // that bridges WGPU's C-style logging to Zig's standard log.
    _ = wgpu.setLogCallback(&struct {
        fn log_callback(level: wgpu.LogLevel, message: wgpu.StringView, userdata: ?*anyopaque) callconv(.c) void {
            _ = userdata;

            const level_str = switch (level) {
                .@"error" => "error",
                .warn => "warn",
                .info => "info",
                .debug => "debug",
                .trace => "trace",
                else => "unknown_level",
            };

            const text = message.toSlice();

            // Filter out noisy messages that we don't care about for this demo.
            inline for (&.{
                "Suboptimal present of frame", // Suboptimal frame presentation is *desirable* when resizing the window, to prevent stuttering.
            }) |ignore_pattern| {
                if (std.mem.indexOf(u8, text, ignore_pattern) != null) return;
            }

            // Use a scoped logger to prefix messages with "wgpu".
            const wgpu_log = std.log.scoped(.wgpu);
            switch (level) {
                .@"error" => wgpu_log.err("{s}", .{text}),
                .warn => wgpu_log.warn("{s}", .{text}),
                .info => wgpu_log.info("{s}", .{text}),
                .debug => wgpu_log.debug("{s}", .{text}),
                else => wgpu_log.debug("[{s}] {s}", .{ level_str, text }),
            }
        }
    }.log_callback, null);

    // Set the maximum log level that WGPU will report.
    wgpu.setLogLevel(.warn);

    // --- GLFW Initialization ---
    // GLFW needs a hint about which platform backend to use (e.g., X11 on Linux, Win32 on Windows).
    if (comptime builtin.os.tag != .windows) {
        glfw.initHint(.{ .platform = .x11 });
    } else {
        glfw.initHint(.{ .platform = .win32 });
    }

    // Initialize the GLFW library.
    try glfw.init();
    // Ensure GLFW is de-initialized when the main function exits.
    defer glfw.deinit();

    // --- Core WGPU and Window Setup ---
    // Create an instance of our application state struct.
    var demo = Demo{};
    // Create the top-level WGPU instance. This is the entry point to the API.
    demo.instance = wgpu.createInstance(null);
    std.debug.assert(demo.instance != null);
    // Ensure the instance is released when the main function exits.
    defer wgpu.instanceRelease(demo.instance);

    const initial_width: i32 = 640;
    const initial_height: i32 = 480;

    // Tell GLFW not to create a graphics context (like OpenGL), because WGPU will manage that.
    glfw.windowHint(.{ .client_api = .none });
    // Create the window that we will draw into.
    const window = try glfw.createWindow(initial_width, initial_height, "gpu-quad [zig / wgpu + glfw + stb_image]", null, null);
    // Ensure the window is destroyed when the main function exits.
    defer glfw.destroyWindow(window);

    // --- GLFW Callbacks ---
    // Store a pointer to our `Demo` state struct within the GLFW window object. This allows
    // our C-style callbacks to retrieve the application state.
    glfw.setWindowUserPointer(window, &demo);

    // Set a callback for key presses.
    _ = glfw.setKeyCallback(window, &struct {
        fn handle_glfw_key(w: *glfw.Window, key: glfw.Key, scancode: i32, action: glfw.KeyState32, mods: glfw.Modifier) callconv(.c) void {
            _ = scancode;
            _ = mods;
            // When 'R' is pressed, generate and print a WGPU report for debugging memory usage.
            if (key == .r and (action == .press or action == .repeat)) {
                // Retrieve the application state from the window pointer.
                const st: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse return));
                if (st.instance == null) return;
                var report: wgpu.GlobalReport = undefined;
                wgpu.generateReport(st.instance, &report);
                wgpu.printGlobalReport(report);
            }
        }
    }.handle_glfw_key);

    // Set a callback for when the window's framebuffer is resized.
    _ = glfw.setFramebufferSizeCallback(window, &struct {
        fn handle_glfw_framebuffer_size(w: *glfw.Window, width: i32, height: i32) callconv(.c) void {
            if (width <= 0 and height <= 0) return;
            // Retrieve the application state.
            const st: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse return));
            if (st.surface == null) return;
            // Update the surface configuration with the new dimensions.
            st.config.width = @intCast(width);
            st.config.height = @intCast(height);
            // Re-configure the surface to apply the new size.
            wgpu.surfaceConfigure(st.surface, &st.config);
        }
    }.handle_glfw_framebuffer_size);

    // --- WGPU Surface Creation (Platform-Specific) ---
    // A surface is the part of a window that WGPU can draw to. Creating it requires
    // platform-specific native window handles, which we get from GLFW.
    if (comptime builtin.os.tag != .windows) {
        // On Linux/X11
        const x11_display = glfw.getX11Display();
        const x11_window = glfw.getX11Window(window);
        std.debug.assert(x11_display != null and x11_window != 0);
        var xlib_source = wgpu.SurfaceSourceXlibWindow{ .chain = .{ .s_type = .surface_source_xlib_window }, .display = x11_display, .window = x11_window };
        demo.surface = wgpu.instanceCreateSurface(demo.instance, &wgpu.SurfaceDescriptor{ .next_in_chain = @ptrCast(&xlib_source) });
    } else {
        // On Windows
        // TODO: glfw binding for windows is currently missing in the example's `glfw.zig`.
        // const win32_hwnd = glfw.getWin32Window(window);
        // std.debug.assert(win32_hwnd != null);
        // var win32_source = wgpu.SurfaceSourceWindowsHWND{ .chain = .{ .s_type = .surface_source_win32_window }, .hwnd = win32_hwnd, .hinstance = null };
        // demo.surface = wgpu.instanceCreateSurface(demo.instance, &wgpu.SurfaceDescriptor{ .next_in_chain = @ptrCast(&win32_source) });
        @panic("NYI");
    }
    std.debug.assert(demo.surface != null);
    // Ensure the surface is released on exit.
    defer wgpu.surfaceRelease(demo.surface);

    // --- WGPU Adapter and Device Setup (Asynchronous) ---
    // 1. Request an Adapter: An adapter represents a physical GPU. We request one
    // that is compatible with our drawing surface.
    _ = wgpu.instanceRequestAdapter(demo.instance, &wgpu.RequestAdapterOptions{ .compatible_surface = demo.surface }, .{
        .callback = &struct {
            fn handle_request_adapter(status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, message: wgpu.StringView, userdata1: ?*anyopaque, userdata2: ?*anyopaque) callconv(.c) void {
                _ = userdata2;
                if (status == .success) {
                    const state: *Demo = @ptrCast(@alignCast(userdata1.?));
                    state.adapter = adapter; // On success, store the adapter in our state.
                } else {
                    if (message.data) |data| std.debug.print("request_adapter status={any} message={s}\n", .{ status, data[0..message.length] });
                }
            }
        }.handle_request_adapter,
        .userdata1 = &demo,
    });
    // This is an asynchronous operation. For this simple example, we poll until the callback has completed.
    while (demo.adapter == null) wgpu.instanceProcessEvents(demo.instance);
    defer wgpu.adapterRelease(demo.adapter);

    // Print information about the selected GPU.
    wgpu.printAdapterInfo(demo.adapter);

    // 2. Request a Device: A device is our logical connection to the GPU, used to create resources.
    _ = wgpu.adapterRequestDevice(demo.adapter, null, .{
        .callback = &struct {
            fn handle_request_device(status: wgpu.RequestDeviceStatus, device: wgpu.Device, message: wgpu.StringView, userdata1: ?*anyopaque, userdata2: ?*anyopaque) callconv(.c) void {
                _ = userdata2;
                if (status == .success) {
                    const state: *Demo = @ptrCast(@alignCast(userdata1.?));
                    state.device = device; // On success, store the device in our state.
                } else {
                    if (message.data) |data| std.debug.print("request_device status={any} message={s}\n", .{ status, data[0..message.length] });
                }
            }
        }.handle_request_device,
        .userdata1 = &demo,
    });
    // This is also async, so we poll to wait for completion.
    while (demo.device == null) wgpu.instanceProcessEvents(demo.instance);
    defer wgpu.deviceRelease(demo.device);

    // 3. Get the Command Queue: The queue is where we submit command buffers for the GPU to execute.
    const queue = wgpu.deviceGetQueue(demo.device);
    std.debug.assert(queue != null);
    defer wgpu.queueRelease(queue);

    // --- GPU Resource Creation ---

    // A quad is made of two triangles, so we need 6 vertices.
    const vertex_count = 6;
    const vertex_data_size = vertex_count * @sizeOf(Vertex);

    // --- Create Vertex and Staging Buffers ---
    // The vertex buffer will live on the GPU and is the destination for our vertex data.
    // It's not directly writable by the CPU, making it faster for the GPU to access during rendering.
    const vertex_buffer = wgpu.deviceCreateBuffer(demo.device, &wgpu.BufferDescriptor{
        .label = .fromSlice("vertex_buffer"),
        // Usage: It will be used as a vertex buffer and as a destination for copy operations.
        .usage = wgpu.BufferUsage{ .vertex = true, .copy_dst = true },
        .size = vertex_data_size,
    });
    std.debug.assert(vertex_buffer != null);
    defer wgpu.bufferRelease(vertex_buffer);

    // The staging buffer is a temporary buffer that is writable by the CPU.
    // We will write our vertex data here each frame and then command the GPU
    // to copy it to the main vertex buffer. This CPU-writable -> GPU-copyable -> GPU-local
    // pattern is common for frequently updated data.
    const staging_buffer = wgpu.deviceCreateBuffer(demo.device, &wgpu.BufferDescriptor{
        .label = .fromSlice("staging_buffer"),
        // Usage: It can be mapped for writing from the CPU and used as a source for copy operations.
        .usage = wgpu.BufferUsage{ .copy_src = true, .map_write = true },
        .size = vertex_data_size,
    });
    std.debug.assert(staging_buffer != null);
    defer wgpu.bufferRelease(staging_buffer);

    // --- Create Uniform Buffer ---
    // This buffer will hold our `Uniforms` struct.
    const uniform_buffer = wgpu.deviceCreateBuffer(demo.device, &wgpu.BufferDescriptor{
        .label = .fromSlice("uniform_buffer"),
        // Usage: It will be used as a uniform buffer in shaders and can be written to.
        .usage = wgpu.BufferUsage{ .uniform = true, .copy_dst = true },
        .size = @sizeOf(Uniforms),
    });
    std.debug.assert(uniform_buffer != null);
    defer wgpu.bufferRelease(uniform_buffer);

    // --- Load Texture ---
    // Use stb_image to load a PNG from the disk into CPU memory.
    var image = try stbi.Image8.fromPath("assets/wgpu-logo.png", .rgba);
    defer image.deinit();
    std.debug.print("Loaded image: {any}\n", .{image.info});

    // Create a WGPU texture on the GPU.
    const texture_size = wgpu.Extent3D{ .width = image.info.width, .height = image.info.height, .depth_or_array_layers = 1 };

    const texture = wgpu.deviceCreateTexture(demo.device, &wgpu.TextureDescriptor{
        .label = .fromSlice("texture"),
        .size = texture_size,
        .mip_level_count = 1,
        .sample_count = 1,
        .dimension = .@"2d",
        .format = .rgba8_unorm_srgb, // sRGB format is important for correct color display.
        // Usage: It can be bound in a shader and can be a destination for data copies.
        .usage = wgpu.TextureUsage{ .texture_binding = true, .copy_dst = true },
    });
    std.debug.assert(texture != null);
    defer wgpu.textureRelease(texture);

    // Upload the image data from the CPU buffer to the GPU texture.
    wgpu.queueWriteTexture(queue, &wgpu.TexelCopyTextureInfo{ .texture = texture }, image.buffer.ptr, image.buffer.len, &wgpu.TexelCopyBufferLayout{ .bytes_per_row = 4 * image.info.width, .rows_per_image = image.info.height }, &texture_size);

    // A TextureView describes how the shader will access the texture (e.g., which mip levels).
    const texture_view = wgpu.textureCreateView(texture, null);
    std.debug.assert(texture_view != null);
    defer wgpu.textureViewRelease(texture_view);
    // A Sampler describes how the shader will sample the texture (e.g., using linear filtering for smoothness).
    const sampler = wgpu.deviceCreateSampler(demo.device, &wgpu.SamplerDescriptor{
        .label = .fromSlice("sampler"),
        .mag_filter = .linear, // Use linear filtering when magnifying the texture.
        .min_filter = .linear, // Use linear filtering when minifying the texture.
        .mipmap_filter = .linear, // Use linear filtering between mip levels.

        // NOTE: this does nothing without generating mip levels, which we aren't doing in this example yet.
        // A clamp value > 1 enables anisotropic filtering.
        // The device hardware will clamp this to its maximum supported level (often 16).
        .max_anisotropy = 16, // this requires all filters to be .linear
    });
    std.debug.assert(sampler != null);
    defer wgpu.samplerRelease(sampler);

    // --- Create Bind Group Layout and Bind Group ---
    // A BindGroupLayout defines the "signature" of a group of resources that will be
    // available to a shader. It's like a function signature or an interface.
    const bind_group_layout = wgpu.deviceCreateBindGroupLayout(demo.device, &wgpu.BindGroupLayoutDescriptor{
        .label = .fromSlice("bind_group_layout"),
        .entry_count = 3,
        // Define the resources: a sampler, a texture, and a uniform buffer.
        .entries = &[_]wgpu.BindGroupLayoutEntry{
            .{ .binding = 0, .visibility = .fragmentStage, .sampler = .{ .type = .filtering } }, // Binding 0: a sampler for the fragment shader.
            .{ .binding = 1, .visibility = .fragmentStage, .texture = .{ .sample_type = .float, .view_dimension = .@"2d" } }, // Binding 1: a texture for the fragment shader.
            .{ .binding = 2, .visibility = .vertexStage, .buffer = .{ .type = .uniform } }, // Binding 2: a uniform buffer for the vertex shader.
        },
    });
    std.debug.assert(bind_group_layout != null);
    defer wgpu.bindGroupLayoutRelease(bind_group_layout);

    // A BindGroup is an instance of a BindGroupLayout. It binds the actual resources
    // (our sampler, texture view, and uniform buffer) to the slots defined in the layout.
    const bind_group = wgpu.deviceCreateBindGroup(demo.device, &wgpu.BindGroupDescriptor{
        .label = .fromSlice("bind_group"),
        .layout = bind_group_layout,
        .entry_count = 3,
        .entries = &[_]wgpu.BindGroupEntry{
            .{ .binding = 0, .sampler = sampler },
            .{ .binding = 1, .texture_view = texture_view },
            .{ .binding = 2, .buffer = uniform_buffer, .offset = 0, .size = @sizeOf(Uniforms) },
        },
    });
    std.debug.assert(bind_group != null);
    defer wgpu.bindGroupRelease(bind_group);

    // --- Create Render Pipeline ---
    // Load our WGSL shader code from a file into a shader module.
    const shader_module = try wgpu.loadShader(demo.device, "assets/screenspace_2d.wgsl");
    defer wgpu.shaderModuleRelease(shader_module);

    // A PipelineLayout defines which bind groups the pipeline will use.
    const pipeline_layout = wgpu.deviceCreatePipelineLayout(demo.device, &wgpu.PipelineLayoutDescriptor{
        .label = .fromSlice("pipeline_layout"),
        .bind_group_layout_count = 1,
        .bind_group_layouts = &[_]wgpu.BindGroupLayout{bind_group_layout},
    });
    std.debug.assert(pipeline_layout != null);
    defer wgpu.pipelineLayoutRelease(pipeline_layout);

    // Query the surface for its supported capabilities (e.g., formats, present modes).
    var surface_capabilities: wgpu.SurfaceCapabilities = undefined;
    _ = wgpu.surfaceGetCapabilities(demo.surface, demo.adapter, &surface_capabilities);
    defer wgpu.surfaceCapabilitiesFreeMembers(surface_capabilities);

    // Define how colors will be blended with the background (standard alpha blending).
    const blend_state = wgpu.BlendState{
        .color = .{ .operation = .add, .src_factor = .src_alpha, .dst_factor = .one_minus_src_alpha },
        .alpha = .{ .operation = .add, .src_factor = .one, .dst_factor = .zero },
    };
    // Define the layout of our vertex buffer for the vertex shader.
    const vertex_buffer_layout = wgpu.VertexBufferLayout{
        .array_stride = @sizeOf(Vertex),
        .step_mode = .vertex,
        .attribute_count = 2,
        .attributes = &[_]wgpu.VertexAttribute{
            .{ .shaderLocation = 0, .offset = @offsetOf(Vertex, "position"), .format = .float32x2 },
            .{ .shaderLocation = 1, .offset = @offsetOf(Vertex, "tex_coords"), .format = .float32x2 },
        },
    };

    // The RenderPipeline is a massive object that configures almost the entire rendering state:
    // shaders, vertex formats, blending, culling, etc. It's expensive to create, so you
    // typically create all needed pipelines at initialization.
    const render_pipeline = wgpu.deviceCreateRenderPipeline(demo.device, &wgpu.RenderPipelineDescriptor{
        .label = .fromSlice("render_pipeline"),
        // The bind groups it will use.
        .layout = pipeline_layout,
        // Vertex shader configuration.
        .vertex = .{
            .module = shader_module,
            .entry_point = .fromSlice("vs_main"),
            .buffer_count = 1,
            .buffers = &.{vertex_buffer_layout},
        },
        // Fragment shader and output target config.
        .fragment = &wgpu.FragmentState{
            .module = shader_module,
            .entry_point = .fromSlice("fs_main"),
            .target_count = 1,
            .targets = &[_]wgpu.ColorTargetState{.{ .format = surface_capabilities.formats.?[0], .blend = &blend_state, .write_mask = .all }},
        },
        .primitive = .{ .topology = .triangle_list }, // How to interpret vertices (as triangles).
        .multisample = .{ .count = 1, .mask = 0xFFFFFFFF }, // No multisampling.
    });
    std.debug.assert(render_pipeline != null);
    defer wgpu.renderPipelineRelease(render_pipeline);

    // --- Configure Surface ---
    // Now that we have a device and have queried capabilities, we can configure the surface.
    demo.config = .{
        .device = demo.device,
        .usage = .renderAttachmentUsage, // The surface textures will be used for rendering.
        .format = surface_capabilities.formats.?[0], // Use the preferred format.
        .present_mode = .fifo, // V-sync mode.
        .alpha_mode = surface_capabilities.alpha_modes.?[0],
    };
    {
        // Set the initial size of the surface configuration.
        var width: i32 = 0;
        var height: i32 = 0;
        glfw.getWindowSize(window, &width, &height);
        demo.config.width = @intCast(width);
        demo.config.height = @intCast(height);
    }
    wgpu.surfaceConfigure(demo.surface, &demo.config);

    // --- Main Loop ---
    main_loop: while (!glfw.windowShouldClose(window)) {
        // Handle window events (input, closing the window, etc.).
        glfw.pollEvents();

        // --- Regenerate and Upload Mesh Data via Staging Buffer ---
        // 1. Asynchronously request to map the staging buffer for writing from the CPU.
        _ = wgpu.bufferMapAsync(staging_buffer, .writeMode, 0, vertex_data_size, .{ .callback = &struct {
            fn callback(status: wgpu.MapAsyncStatus, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
                _ = msg;
                _ = ud1;
                _ = ud2;
                if (status != .success) std.debug.print("Failed to map staging buffer: {any}\n", .{status});
            }
        }.callback });

        // 2. Poll the device to wait for the mapping to complete.
        // In a complex engine, you would use the async callback to avoid blocking the main thread.
        // For this example, polling is simpler and effectively synchronizes the operation.
        _ = wgpu.devicePoll(demo.device, .true, null);

        // 3. Get a CPU pointer to the mapped memory and write the new vertex data into it.
        const mapped_range = wgpu.bufferGetMappedRange(staging_buffer, 0, vertex_data_size);
        if (mapped_range) |ptr| {
            // Cast the raw pointer to a pointer of our Vertex struct.
            const data: [*]Vertex = @ptrCast(@alignCast(ptr));
            // Get the current cursor position from GLFW.
            var cursor_x: f64 = 0;
            var cursor_y: f64 = 0;
            glfw.getCursorPos(window, &cursor_x, &cursor_y);
            // Define the size of the quad.
            const quad_width: f32 = @as(f32, @floatFromInt(image.info.width)) * 0.25;
            const quad_height: f32 = @as(f32, @floatFromInt(image.info.height)) * 0.25;
            // Calculate the quad's corners based on the cursor position.
            const x1: f32 = @as(f32, @floatCast(cursor_x)) - quad_width / 2.0;
            const y1: f32 = @as(f32, @floatCast(cursor_y)) - quad_height / 2.0;
            const x2: f32 = @as(f32, @floatCast(cursor_x)) + quad_width / 2.0;
            const y2: f32 = @as(f32, @floatCast(cursor_y)) + quad_height / 2.0;

            // Define the 6 vertices for the two triangles that make up the quad.
            const vertices = [_]Vertex{
                .{ .position = .{ x1, y1 }, .tex_coords = .{ 0.0, 0.0 } }, // Top-left
                .{ .position = .{ x2, y1 }, .tex_coords = .{ 1.0, 0.0 } }, // Top-right
                .{ .position = .{ x1, y2 }, .tex_coords = .{ 0.0, 1.0 } }, // Bottom-left
                .{ .position = .{ x1, y2 }, .tex_coords = .{ 0.0, 1.0 } }, // Bottom-left
                .{ .position = .{ x2, y1 }, .tex_coords = .{ 1.0, 0.0 } }, // Top-right
                .{ .position = .{ x2, y2 }, .tex_coords = .{ 1.0, 1.0 } }, // Bottom-right
            };
            // Copy the vertex data into the mapped staging buffer.
            @memcpy(data[0..vertex_count], &vertices);
            // Unmap the buffer, giving control back to the GPU.
            wgpu.bufferUnmap(staging_buffer);
        }

        // --- Update Uniforms ---
        // Update our uniform data with the current window resolution.
        const uniforms = Uniforms{ .resolution = .{ @floatFromInt(demo.config.width), @floatFromInt(demo.config.height) } };
        // Write the new uniform data to the uniform buffer. queueWriteBuffer is a convenient
        // way to update small amounts of data without the map/unmap dance.
        wgpu.queueWriteBuffer(queue, uniform_buffer, 0, &uniforms, @sizeOf(Uniforms));

        // --- Acquire Frame and Handle Resizing ---
        // Get the next available texture from the swap chain to draw on.
        var surface_texture: wgpu.SurfaceTexture = undefined;
        wgpu.surfaceGetCurrentTexture(demo.surface, &surface_texture);
        // The surface might be "lost" or "outdated" (e.g., after a resize). We must handle this.
        switch (surface_texture.status) {
            .success_optimal, .success_suboptimal => {}, // All good.
            .timeout, .outdated, .lost => {
                // The surface is no longer valid. We need to release the old texture,
                // get the new window size, and re-configure the surface.
                if (surface_texture.texture != null) wgpu.textureRelease(surface_texture.texture);
                var width: i32 = 0;
                var height: i32 = 0;
                glfw.getWindowSize(window, &width, &height);
                if (width != 0 and height != 0) {
                    demo.config.width = @intCast(width);
                    demo.config.height = @intCast(height);
                    wgpu.surfaceConfigure(demo.surface, &demo.config);
                }
                // Skip the rest of this frame's rendering and try again.
                continue :main_loop;
            },
            .out_of_memory, .device_lost, .@"error" => std.debug.panic("get_current_texture status={any}", .{surface_texture.status}),
            else => std.debug.print("get_current_texture unknown status={any}\n", .{surface_texture.status}),
        }
        std.debug.assert(surface_texture.texture != null);
        // Ensure the acquired texture is released at the end of the loop scope.
        defer wgpu.textureRelease(surface_texture.texture);

        // Create a view of the swap chain texture, which will be our render target.
        const frame = wgpu.textureCreateView(surface_texture.texture, null);
        std.debug.assert(frame != null);
        defer wgpu.textureViewRelease(frame);

        // --- Command Encoding ---
        // Create a command encoder to record GPU commands.
        const command_encoder = wgpu.deviceCreateCommandEncoder(demo.device, &wgpu.CommandEncoderDescriptor{ .label = .fromSlice("command_encoder") });
        std.debug.assert(command_encoder != null);
        defer wgpu.commandEncoderRelease(command_encoder);

        // 4. Before the render pass, encode a command to copy the data from the staging
        // buffer to the actual vertex buffer. This copy happens entirely on the GPU.
        wgpu.commandEncoderCopyBufferToBuffer(command_encoder, staging_buffer, 0, vertex_buffer, 0, vertex_data_size);

        // Begin a render pass. This is a block of drawing commands that render to a specific target.
        const render_pass_encoder = wgpu.commandEncoderBeginRenderPass(command_encoder, &wgpu.RenderPassDescriptor{
            .label = .fromSlice("render_pass_encoder"),
            .color_attachment_count = 1,
            // Configure the color attachment (our `frame` from the swap chain).
            .color_attachments = &[_]wgpu.RenderPassColorAttachment{.{ .view = frame, .load_op = .clear, .store_op = .store, .clear_value = .{ .g = 1.0, .a = 1.0 } }},
        });
        std.debug.assert(render_pass_encoder != null);

        // --- Record Render Commands ---
        wgpu.renderPassEncoderSetPipeline(render_pass_encoder, render_pipeline); // Set the active pipeline.
        wgpu.renderPassEncoderSetBindGroup(render_pass_encoder, 0, bind_group, 0, null); // Set the active bind group.
        wgpu.renderPassEncoderSetVertexBuffer(render_pass_encoder, 0, vertex_buffer, 0, vertex_data_size); // Set the active vertex buffer.
        wgpu.renderPassEncoderDraw(render_pass_encoder, vertex_count, 1, 0, 0); // Draw our 6 vertices.

        // End the render pass.
        wgpu.renderPassEncoderEnd(render_pass_encoder);
        wgpu.renderPassEncoderRelease(render_pass_encoder);

        // Finish encoding and create a command buffer.
        const command_buffer = wgpu.commandEncoderFinish(command_encoder, &wgpu.CommandBufferDescriptor{ .label = .fromSlice("command_buffer") });
        std.debug.assert(command_buffer != null);
        defer wgpu.commandBufferRelease(command_buffer);

        // --- Submit to GPU and Present ---
        // Submit the command buffer to the queue for execution.
        wgpu.queueSubmit(queue, 1, &[_]wgpu.CommandBuffer{command_buffer});
        // Present the rendered frame to the screen.
        _ = wgpu.surfacePresent(demo.surface);
    }
}
