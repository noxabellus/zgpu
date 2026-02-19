const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");

const wgpu = @import("wgpu");
const glfw = @import("glfw");
const gltf = @import("gltf");
const stbi = @import("stbi");
const nfd = @import("nfd");

const Gpu = @import("../Gpu.zig");
const Application = @import("../Application.zig");
const RenderTexture = @import("../RenderTexture.zig");
const Compositor = @import("../Compositor.zig");
const Camera = @import("../Camera.zig");
const debug = @import("../debug.zig");
const Batch2D = @import("../Batch2D.zig");
const AssetCache = @import("../AssetCache.zig");
const InputState = @import("../InputState.zig");
const BindingState = @import("../BindingState.zig");
const Model = @import("../Model.zig");
const Ui = @import("../Ui.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;
const vec3 = linalg.vec3;
const vec4 = linalg.vec4;
const vec4u = linalg.vec4u;
const quat = linalg.quat;
const mat4 = linalg.mat4;

pub const std_options = std.Options{
    .log_level = .info,
};

test {
    log.debug("semantic analysis for examples/gltf.zig", .{});
    std.testing.refAllDecls(@This());
}

const Demo = struct {
    app: *Application,

    camera: Camera,

    model: ?Model = null,
    anim_state: ?Model.AnimationState = null,
    anim_index: u32 = 0,
    anim_time: f32 = 0.0,

    gltf_color: RenderTexture,
    gltf_depth: RenderTexture,
    ui_msaa: RenderTexture,
    ui_color: RenderTexture,
    compositor: Compositor,
};

var FONT_ID_BODY: AssetCache.FontId = undefined;
var FONT_ID_TITLE: AssetCache.FontId = undefined;
var FONT_ID_MONO: AssetCache.FontId = undefined;

const COLOR_LIGHT = Ui.Color.fromLinearU8(244, 235, 230, 255);
const COLOR_LIGHT_HOVER = Ui.Color.fromLinearU8(224, 215, 210, 255);
const COLOR_BUTTON_HOVER = Ui.Color.fromLinearU8(238, 227, 225, 255);
const COLOR_BROWN = Ui.Color.fromLinearU8(61, 26, 5, 255);
const COLOR_GREYBROWN = Ui.Color.fromLinearU8(54, 48, 33, 255);
const COLOR_TAN = Ui.Color.fromLinearU8(128, 113, 77, 255);
const COLOR_PHOSPHOR = Ui.Color.fromLinearU8(0xff, 0xd4, 0x77, 0xff);
const COLOR_RED = Ui.Color.fromLinearU8(168, 66, 28, 255);
const COLOR_RED_HOVER = Ui.Color.fromLinearU8(148, 46, 8, 255);
const COLOR_ORANGE = Ui.Color.fromLinearU8(225, 138, 50, 255);
const COLOR_BLUE = Ui.Color.fromLinearU8(111, 173, 162, 255);
const COLOR_TEAL = Ui.Color.fromLinearU8(111, 173, 162, 255);
const COLOR_BLUE_DARK = Ui.Color.fromLinearU8(2, 32, 82, 255);
const COLOR_NONE = Ui.Color.fromLinearU8(0, 0, 0, 255);
const COLOR_WHITE = Ui.Color.fromLinearU8(255, 255, 255, 255);
const COLOR_BLACK = Ui.Color.fromLinearU8(0, 0, 0, 255);

const shader_text = @embedFile("shaders/GltfMultiPurpose.wgsl");

const UI_MSAA_SAMPLES = 4;
const DEPTH_FORMAT = wgpu.TextureFormat.depth32_float;

pub fn main() !void {
    var timer = try std.time.Timer.start();

    const gpa = std.heap.page_allocator;

    // --- Application and UI Library Initialization ---
    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();

    const frame_arena = arena_state.allocator();

    // Init Asset Cache
    var asset_cache = AssetCache.init(gpa);
    defer asset_cache.deinit();

    const app = try Application.init(gpa, "zgpu gltf example");
    defer app.deinit();

    const renderer = try Batch2D.init(
        gpa,
        app.gpu.device,
        app.gpu.queue,
        app.gpu.surface_format,
        &asset_cache,
        UI_MSAA_SAMPLES,
    );
    defer renderer.deinit();

    var demo = Demo{
        .app = app,
        .camera = Camera.fromLookAt(
            vec3{ 10.0, 2.0, 0.0 },
            vec3{ 0, 1.0, 0.0 },
            vec3{ 0.0, 1.0, 0.0 },
        ),
        .gltf_color = try RenderTexture.init(&app.gpu, .{
            .label = "gltf_color",
            .width = app.gpu.config.width,
            .height = app.gpu.config.height,
            .format = app.gpu.surface_format,
            .usage = .{ .render_attachment = true, .texture_binding = true },
            .sample_count = 1,
        }),
        .gltf_depth = try RenderTexture.init(&app.gpu, .{
            .label = "gltf_depth",
            .width = app.gpu.config.width,
            .height = app.gpu.config.height,
            .format = DEPTH_FORMAT,
            .usage = .{ .render_attachment = true },
            .sample_count = 1,
        }),
        .ui_msaa = try RenderTexture.init(&app.gpu, .{
            .label = "ui_msaa",
            .width = app.gpu.config.width,
            .height = app.gpu.config.height,
            .format = app.gpu.surface_format,
            .usage = .{ .render_attachment = true },
            .sample_count = UI_MSAA_SAMPLES,
        }),
        .ui_color = try RenderTexture.init(&app.gpu, .{
            .label = "ui_color",
            .width = app.gpu.config.width,
            .height = app.gpu.config.height,
            .format = app.gpu.surface_format,
            .usage = .{ .render_attachment = true, .texture_binding = true },
            .sample_count = 1,
        }),
        .compositor = try Compositor.init(&app.gpu),
    };
    defer demo.gltf_color.deinit();
    defer demo.gltf_depth.deinit();
    defer demo.ui_msaa.deinit();
    defer demo.ui_color.deinit();
    defer demo.compositor.deinit();

    app.user_data = &demo;

    const camera_buffer = app.gpu.createBuffer(&.{
        .usage = .{ .uniform = true, .copy_dst = true },
        .size = @sizeOf(Camera.Uniform),
    });
    defer wgpu.bufferRelease(camera_buffer);

    _ = glfw.setCursorPosCallback(app.window, struct {
        pub fn mouse_handler(w: *glfw.Window, xpos: f64, ypos: f64) callconv(.c) void {
            const a: *Application = @ptrCast(@alignCast(glfw.getWindowUserPointer(w)));
            const d: *Demo = @ptrCast(@alignCast(a.user_data));
            d.camera.handle_mouse_move(w, vec2{ @floatCast(xpos), @floatCast(ypos) });
        }
    }.mouse_handler);

    const camera_bind_group_layout = app.gpu.createBindGroupLayout(&.{
        .label = .fromSlice("camera_bind_group_layout"),
        .entry_count = 1,
        .entries = &.{
            .{
                .binding = 0,
                .visibility = wgpu.ShaderStage.vertexStage,
                .buffer = .{ .type = .uniform },
            },
        },
    });
    defer wgpu.bindGroupLayoutRelease(camera_bind_group_layout);

    const texture_bind_group_layout = app.gpu.createBindGroupLayout(&.{
        .label = .fromSlice("texture_bind_group_layout"),
        .entry_count = 2,
        .entries = &.{
            .{ .binding = 0, .visibility = wgpu.ShaderStage.fragmentStage, .texture = .{ .sample_type = .float, .view_dimension = .@"2d" } },
            .{ .binding = 1, .visibility = wgpu.ShaderStage.fragmentStage, .sampler = .{ .type = .filtering } },
        },
    });
    defer wgpu.bindGroupLayoutRelease(texture_bind_group_layout);
    const skin_uniform_buffer = app.gpu.createBuffer(&.{
        .label = .fromSlice("skin_uniform_buffer"),
        .usage = wgpu.BufferUsage.uniformUsage.merge(.copyDstUsage),
        .size = @sizeOf(Model.SkinUniforms),
    });
    defer wgpu.bufferRelease(skin_uniform_buffer);

    const skin_bind_group_layout = app.gpu.createBindGroupLayout(&.{
        .label = .fromSlice("skin_bind_group_layout"),
        .entry_count = 1,
        .entries = &.{
            .{
                .binding = 0,
                .visibility = wgpu.ShaderStage.vertexStage,
                .buffer = .{ .type = .uniform },
            },
        },
    });
    defer wgpu.bindGroupLayoutRelease(skin_bind_group_layout);

    const skin_bind_group = app.gpu.createBindGroup(&.{
        .label = .fromSlice("skin_bind_group"),
        .layout = skin_bind_group_layout,
        .entry_count = 1,
        .entries = &.{
            .{
                .binding = 0,
                .buffer = skin_uniform_buffer,
                .size = @sizeOf(Model.SkinUniforms),
            },
        },
    });
    defer wgpu.bindGroupRelease(skin_bind_group);

    const camera_bind_group = app.gpu.createBindGroup(&.{
        .label = .fromSlice("camera_bind_group"),
        .layout = camera_bind_group_layout,
        .entry_count = 1,
        .entries = &.{
            .{
                .binding = 0,
                .buffer = camera_buffer,
                .offset = 0,
                .size = @sizeOf(Camera.Uniform),
            },
        },
    });
    defer wgpu.bindGroupRelease(camera_bind_group);

    // Init Ui

    var inputs = InputState.init(demo.app.window);
    inputs.listenAllGlfw();

    var bindings = BindingState.init(gpa, &inputs);
    defer bindings.deinit();

    try bindings.bind(.toggle_debugger, .{ .key = .{ .bind_point = .d, .modifiers = .ctrlMod } });
    try bindings.bind(.dump_atlas, .{ .key = .{ .bind_point = .a, .modifiers = .altMod } });
    try bindings.bind(.open_context_menu, .{ .mouse = .{ .bind_point = .button_2 } });

    var ui = try Ui.init(gpa, frame_arena, renderer, &asset_cache, &bindings);
    defer ui.deinit();

    FONT_ID_BODY = try asset_cache.loadFont("assets/fonts/quicksand/semibold.ttf", .linear);
    FONT_ID_TITLE = try asset_cache.loadFont("assets/fonts/calistoga/regular.ttf", .linear);
    FONT_ID_MONO = try asset_cache.loadFont("assets/fonts/dejavu/sans-mono.ttf", .linear);

    // LOAD MODEL //

    demo.model = if (try nfd.openDialog("glb,gltf", ".")) |p| try Model.loadGltf(gpa, &app.gpu, p, texture_bind_group_layout) else null;
    defer if (demo.model) |*m| m.deinit();

    try ui.addListener(
        .fromSlice("animIndexSlider"),
        .uint_change,
        Demo,
        &struct {
            pub fn slider_value_listener(d: *Demo, _: *Ui, _: Ui.Event.Info, new_value: Ui.Event.Payload(.uint_change)) anyerror!void {
                if (d.model) |*model| {
                    d.anim_index = @intCast(new_value);

                    if (d.anim_state) |*st| st.deinit();

                    d.anim_state = try Model.AnimationState.init(gpa, model, d.anim_index);
                    d.anim_time = 0.0;
                }
            }
        }.slider_value_listener,
        &demo,
    );
    const anim_speed = 2.0; // Playback speed multiplier
    demo.anim_state = if (demo.model != null and demo.model.?.animations.len > 0) try Model.AnimationState.init(gpa, &demo.model.?, demo.anim_index) else none: {
        app.gpu.writeBuffer(skin_uniform_buffer, 0, &[1]mat4{linalg.mat4_identity} ** 128);

        break :none null;
    };
    defer if (demo.anim_state) |*x| x.deinit();

    const shader_module = try app.gpu.loadShaderText("GltfMultiPurpose.wgsl", shader_text);
    defer wgpu.shaderModuleRelease(shader_module);

    const pipeline_layout = app.gpu.createPipelineLayout(&.{
        .label = .fromSlice("pipeline_layout"),
        .bind_group_layout_count = 3,
        .bind_group_layouts = &.{
            camera_bind_group_layout,
            texture_bind_group_layout,
            skin_bind_group_layout,
        },
    });
    defer wgpu.pipelineLayoutRelease(pipeline_layout);

    const render_pipeline = app.gpu.createPipeline(&wgpu.RenderPipelineDescriptor{
        .label = .fromSlice("main_render_pipeline"),
        .layout = pipeline_layout,
        .vertex = .{
            .module = shader_module,
            .entry_point = .fromSlice("vs_main"),
            .buffer_count = 1,
            .buffers = &.{
                wgpu.VertexBufferLayout{
                    .array_stride = @sizeOf(Model.Vertex),
                    .step_mode = .vertex,
                    .attribute_count = 6,
                    .attributes = &.{
                        .{ .shaderLocation = 0, .offset = @offsetOf(Model.Vertex, "position"), .format = .float32x3 },
                        .{ .shaderLocation = 1, .offset = @offsetOf(Model.Vertex, "normal"), .format = .float32x3 },
                        .{ .shaderLocation = 2, .offset = @offsetOf(Model.Vertex, "color"), .format = .float32x4 },
                        .{ .shaderLocation = 3, .offset = @offsetOf(Model.Vertex, "tex_coord"), .format = .float32x2 },
                        .{ .shaderLocation = 4, .offset = @offsetOf(Model.Vertex, "joint_indices"), .format = .uint32x4 },
                        .{ .shaderLocation = 5, .offset = @offsetOf(Model.Vertex, "joint_weights"), .format = .float32x4 },
                    },
                },
            },
        },
        .primitive = .{
            .topology = .triangle_list,
            .strip_index_format = .undefined,
            .cull_mode = .back,
            .front_face = .ccw,
        },
        .depth_stencil = &wgpu.DepthStencilState{
            .format = DEPTH_FORMAT,
            .depth_write_enabled = .true,
            .depth_compare = .less,
            .stencil_front = .{},
            .stencil_back = .{},
            .stencil_read_mask = 0,
            .stencil_write_mask = 0,
            .depth_bias = 0,
            .depth_bias_slope_scale = 0.0,
            .depth_bias_clamp = 0.0,
        },
        .fragment = &wgpu.FragmentState{
            .module = shader_module,
            .entry_point = .fromSlice("fs_main"),
            .target_count = 1,
            .targets = &.{
                wgpu.ColorTargetState{
                    .format = app.gpu.surface_format,
                    .blend = null,
                    .write_mask = .all,
                },
            },
        },
    });
    defer wgpu.renderPipelineRelease(render_pipeline);

    const startup_ms = debug.start(&timer);
    log.info("startup completed in {d} ms", .{startup_ms});

    var last_frame_time = glfw.getTime();
    main_loop: while (!glfw.windowShouldClose(app.window)) {
        const current_time = glfw.getTime();
        const delta_time: f32 = @floatCast(current_time - last_frame_time);
        last_frame_time = current_time;

        defer debug.lap();

        glfw.pollEvents();
        _ = arena_state.reset(.retain_capacity);

        // --- Update Animation ---
        if (demo.model) |*model| {
            if (demo.anim_state) |*st| {
                demo.anim_time += delta_time * anim_speed;
                const anim = &model.animations[demo.anim_index];
                if (demo.anim_time > anim.duration) {
                    demo.anim_time -= anim.duration;
                    @memset(st.channel_key_cache, 0); // Reset key cache on loop
                }

                st.updateAnimation(
                    model,
                    demo.anim_index,
                    demo.anim_time,
                );

                app.gpu.writeBuffer(skin_uniform_buffer, 0, st.matrices);
            }
        }

        // --- Process keyboard input ---
        demo.camera.update(app.window, delta_time);

        inputs.collectAllGlfw();

        // Generate the UI layout and cache rendering commands
        {
            try ui.beginLayout(
                .{ @floatFromInt(app.gpu.config.width), @floatFromInt(app.gpu.config.height) },
                delta_time,
            );

            {
                try ui.openElement(.{
                    .id = .fromSlice("OuterContainer"),
                    .layout = .{
                        .sizing = .{
                            .w = .fit,
                            .h = .grow,
                        },
                        .direction = .top_to_bottom,
                        .child_alignment = .center,
                        .padding = .all(10),
                    },
                });
                defer ui.closeElement();

                {
                    try ui.openElement(.{
                        .id = .fromSlice("MenuContainer"),
                        .layout = .{
                            .sizing = .{
                                .w = .fit,
                                .h = .fit,
                            },
                            .direction = .top_to_bottom,
                            .child_alignment = .center,
                            .child_gap = 10,
                            .padding = .all(10),
                        },
                        .background_color = COLOR_GREYBROWN,
                        .corner_radius = .all(10),
                    });
                    defer ui.closeElement();

                    {
                        try ui.beginElement(.fromSlice("TitleText"));
                        defer ui.closeElement();

                        try ui.configureElement(.{
                            .layout = .{
                                .sizing = .fit,
                            },
                        });

                        try ui.text("Animation", .{
                            .font_id = FONT_ID_TITLE,
                            .font_size = 32,
                            .color = COLOR_PHOSPHOR,
                        });
                    }

                    if (demo.model) |*model| {
                        if (model.animations.len > 0) {
                            {
                                try ui.beginElement(.fromSlice("NameText"));
                                defer ui.closeElement();

                                try ui.configureElement(.{
                                    .layout = .{
                                        .sizing = .fit,
                                    },
                                });

                                try ui.text(model.animations[demo.anim_index].name orelse "[unnamed]", .{
                                    .font_id = FONT_ID_BODY,
                                    .font_size = 16,
                                    .color = COLOR_PHOSPHOR,
                                });
                            }

                            {
                                try ui.beginElement(.fromSlice("animIndexSlider"));
                                defer ui.closeElement();

                                try ui.configureElement(.{
                                    .layout = .{
                                        .sizing = .{ .w = .fixed(300), .h = .fixed(20) },
                                    },
                                    .widget = true,
                                    .state = .flags(.{
                                        .click = true,
                                        .drag = true,
                                        .focus = true,
                                        .keyboard = true,
                                    }),
                                });

                                try ui.bindSlider(u32, .{
                                    .default = 0,
                                    .min = 0,
                                    .max = if (demo.model != null and demo.model.?.animations.len > 0) @intCast(demo.model.?.animations.len - 1) else 0,
                                    .track_color = if (ui.focused()) COLOR_TAN else COLOR_BROWN,
                                    .handle_color = COLOR_PHOSPHOR,
                                });
                            }
                        }
                    }
                }
            }

            try ui.endLayout();
        }

        try ui.dispatchEvents();

        {
            const frame_view = app.gpu.beginFrame() orelse continue :main_loop;
            defer app.gpu.endFrame(frame_view);

            // 1. Sync Render Targets to current window size
            const w = app.gpu.config.width;
            const h = app.gpu.config.height;
            try demo.gltf_color.resize(&app.gpu, w, h);
            try demo.gltf_depth.resize(&app.gpu, w, h);
            try demo.ui_msaa.resize(&app.gpu, w, h);
            try demo.ui_color.resize(&app.gpu, w, h);

            const encoder = app.gpu.getCommandEncoder("main_encoder");

            // --- Pass 1: GLTF Model (NO MSAA, Depth) ---
            demo.camera.calculateViewProj(app.window);
            app.gpu.writeBuffer(camera_buffer, 0, &demo.camera.getUniform());

            {
                // Always begin the pass to clear the background, even if no model is loaded.
                const render_pass = wgpu.commandEncoderBeginRenderPass(encoder, &wgpu.RenderPassDescriptor{
                    .label = .fromSlice("main_render_pass"),
                    .color_attachment_count = 1,
                    .color_attachments = &[_]wgpu.RenderPassColorAttachment{.{
                        .view = demo.gltf_color.view,
                        .resolve_target = null,
                        .load_op = .clear,
                        .store_op = .store,
                        .clear_value = wgpu.Color{ .r = 0.1, .g = 0.2, .b = 0.3, .a = 1 },
                    }},
                    .depth_stencil_attachment = &wgpu.RenderPassDepthStencilAttachment{
                        .view = demo.gltf_depth.view,
                        .depth_load_op = .clear,
                        .depth_store_op = .store,
                        .depth_clear_value = 1.0,
                        .depth_read_only = .False,
                    },
                });
                defer wgpu.renderPassEncoderRelease(render_pass);

                if (demo.model) |*model| {
                    wgpu.renderPassEncoderSetPipeline(render_pass, render_pipeline);
                    wgpu.renderPassEncoderSetBindGroup(render_pass, 0, camera_bind_group, 0, null);
                    for (model.primitives) |primitive| {
                        wgpu.renderPassEncoderSetBindGroup(render_pass, 1, primitive.texture_bind_group, 0, null);
                        wgpu.renderPassEncoderSetBindGroup(render_pass, 2, skin_bind_group, 0, null);
                        wgpu.renderPassEncoderSetVertexBuffer(render_pass, 0, primitive.vertex_buffer, 0, wgpu.whole_size);
                        wgpu.renderPassEncoderSetIndexBuffer(render_pass, primitive.index_buffer, primitive.index_format, 0, wgpu.whole_size);
                        wgpu.renderPassEncoderDrawIndexed(render_pass, primitive.index_count, 1, 0, 0, 0);
                    }
                }
                wgpu.renderPassEncoderEnd(render_pass);
            }

            // --- Pass 2: UI (MSAA resolving to UI Color) ---
            {
                const proj = linalg.mat4_ortho(0, @floatFromInt(w), @floatFromInt(h), 0, -1, 1);
                renderer.beginFrame(proj, w, h);
                try ui.render();
                try debug.drawFpsChart(renderer, .{ 0, 0 });
                try renderer.endFrame();

                const render_pass = wgpu.commandEncoderBeginRenderPass(encoder, &wgpu.RenderPassDescriptor{
                    .label = .fromSlice("ui_render_pass"),
                    .color_attachment_count = 1,
                    .color_attachments = &[_]wgpu.RenderPassColorAttachment{.{
                        .view = demo.ui_msaa.view,
                        .resolve_target = demo.ui_color.view,
                        .load_op = .clear,
                        .store_op = .store,
                        // IMPORTANT: Clear to transparent black so it alpha blends nicely over the GLTF layer
                        .clear_value = wgpu.Color{ .r = 0, .g = 0, .b = 0, .a = 0 },
                    }},
                });
                defer wgpu.renderPassEncoderRelease(render_pass);
                try renderer.render(render_pass);
                wgpu.renderPassEncoderEnd(render_pass);
            }

            // --- Pass 3: Composition ---

            // Draw the base 3D scene (Clears the swapchain)
            demo.compositor.draw(&app.gpu, encoder, &demo.gltf_color, frame_view, .clear, wgpu.Color{ .r = 0, .g = 0, .b = 0, .a = 1 });

            // Draw the anti-aliased UI over it (Loads the swapchain and Alpha Blends)
            demo.compositor.draw(&app.gpu, encoder, &demo.ui_color, frame_view, .load, wgpu.Color{ .r = 0, .g = 0, .b = 0, .a = 0 });

            // --- WGPU Command Submission ---
            const cmd = app.gpu.finalizeCommandEncoder(encoder);
            app.gpu.submitCommands(&.{cmd});
        }
    }
}
