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
const EnvironmentLight = @import("../EnvironmentLight.zig");
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
    light: EnvironmentLight = .{},

    model: ?Model = null,
    animator: Model.Animator = .{},
    anim_state: Model.AnimationState = .{},

    gltf_color: RenderTexture,
    gltf_depth: RenderTexture,
    ui_msaa: RenderTexture,
    ui_color: RenderTexture,
    compositor: Compositor,

    path_to_load: error{DialogFailed}!?[]const u8 = null,
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
const SCALE_DIVISOR = 3;

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

    try nfd.initAsync();
    defer nfd.deinitAsync();

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
            .width = @divFloor(app.gpu.config.width, SCALE_DIVISOR),
            .height = @divFloor(app.gpu.config.height, SCALE_DIVISOR),
            .format = app.gpu.surface_format,
            .usage = .{ .render_attachment = true, .texture_binding = true },
            .sample_count = 1,
        }),
        .gltf_depth = try RenderTexture.init(&app.gpu, .{
            .label = "gltf_depth",
            .width = @divFloor(app.gpu.config.width, SCALE_DIVISOR),
            .height = @divFloor(app.gpu.config.height, SCALE_DIVISOR),
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
    defer demo.anim_state.deinit();

    app.user_data = &demo;

    _ = glfw.setCursorPosCallback(app.window, struct {
        pub fn mouse_handler(w: *glfw.Window, xpos: f64, ypos: f64) callconv(.c) void {
            const a: *Application = @ptrCast(@alignCast(glfw.getWindowUserPointer(w)));
            const d: *Demo = @ptrCast(@alignCast(a.user_data));
            d.camera.handle_mouse_move(w, vec2{ @floatCast(xpos), @floatCast(ypos) });
        }
    }.mouse_handler);

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

    defer if (demo.model) |*m| m.deinit();

    try ui.addListener(
        .fromSlice("animIndexSlider"),
        .uint_change,
        Demo,
        &struct {
            pub fn slider_value_listener(d: *Demo, _: *Ui, _: Ui.Event.Info, new_value: Ui.Event.Payload(.uint_change)) anyerror!void {
                d.anim_state.setIndex(@intCast(new_value));
            }
        }.slider_value_listener,
        &demo,
    );

    try ui.addListener(
        .fromSlice("LoadButton"),
        .activate_end,
        Demo,
        &struct {
            pub fn load_button_listener(d: *Demo, _: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.activate_end)) anyerror!void {
                try nfd.openDialogAsync("glb,gltf", ".", &d.path_to_load);
            }
        }.load_button_listener,
        &demo,
    );

    try ui.addListener(
        .fromSlice("UnloadButton"),
        .activate_end,
        Demo,
        &struct {
            pub fn unload_button_listener(d: *Demo, _: *Ui, _: Ui.Event.Info, _: Ui.Event.Payload(.activate_end)) anyerror!void {
                if (d.model) |*m| m.deinit();
                d.model = null;

                d.anim_state.clearCache();
                d.anim_state.clearMatrices();
                d.anim_state.sync(&d.app.gpu);
            }
        }.unload_button_listener,
        &demo,
    );

    try ui.addListener(
        .fromSlice("lightBrightnessSlider"),
        .float_change,
        Demo,
        &struct {
            pub fn brightness_slider_listener(d: *Demo, _: *Ui, _: Ui.Event.Info, new_value: Ui.Event.Payload(.float_change)) anyerror!void {
                d.light.config.brightness = @floatCast(new_value);
                std.debug.print("New Light Values: {any}\n", .{d.light.config});
            }
        }.brightness_slider_listener,
        &demo,
    );

    const shader_module = try app.gpu.loadShaderText("GltfMultiPurpose.wgsl", shader_text);
    defer wgpu.shaderModuleRelease(shader_module);

    const pipeline_layout = app.gpu.createPipelineLayout(&.{
        .label = .fromSlice("pipeline_layout"),
        .bind_group_layout_count = 4,
        .bind_group_layouts = &.{
            Camera.getBindGroupLayout(&app.gpu),
            Model.getTextureBindGroupLayout(&app.gpu),
            Model.AnimationState.getBindGroupLayout(&app.gpu),
            EnvironmentLight.getBindGroupLayout(&app.gpu),
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
            .buffers = &.{Model.Vertex.buffer_layout},
        },
        .primitive = .{
            .topology = .triangle_list,
            .cull_mode = .back,
            .front_face = .ccw,
        },
        .depth_stencil = &wgpu.DepthStencilState{
            .format = DEPTH_FORMAT,
            .depth_write_enabled = .true,
            .depth_compare = .less,
        },
        .fragment = &wgpu.FragmentState{
            .module = shader_module,
            .entry_point = .fromSlice("fs_main"),
            .target_count = 1,
            .targets = &.{
                wgpu.ColorTargetState{
                    .format = app.gpu.surface_format,
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

        if (demo.path_to_load) |maybe_path| {
            if (maybe_path) |p| {
                // Deinit old model if it exists
                if (demo.model) |*m| m.deinit();

                demo.model = try Model.loadGltf(gpa, &demo.app.gpu, p);

                demo.anim_state.setIndex(0);
                demo.anim_state.clearMatrices();
                demo.anim_state.sync(&demo.app.gpu);

                try ui.setWidgetState(.fromSlice("animIndexSlider"), u32, 0);
            }
        } else |err| {
            log.err("Failed to load model: {s}\n", .{@errorName(err)});
        }
        demo.path_to_load = null;

        // --- Update Animation ---
        if (demo.model) |*model| {
            demo.animator.updateAnimation(
                model,
                &demo.anim_state,
                delta_time,
            );

            demo.anim_state.sync(&demo.app.gpu);
        }

        // --- Process keyboard input ---
        demo.camera.update(app.window, delta_time);

        inputs.collectAllGlfw();

        try ui.dispatchEvents();

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
                        try ui.openElement(.{
                            .id = .fromSlice("ButtonRow"),
                            .layout = .{
                                .sizing = .{
                                    .w = .grow,
                                    .h = .fit,
                                },
                                .direction = .left_to_right,
                                .child_alignment = .{
                                    .x = .left,
                                    .y = .center,
                                },
                                .child_gap = 10,
                            },
                        });
                        defer ui.closeElement();

                        {
                            try ui.beginElement(.fromSlice("LoadButton"));
                            defer ui.closeElement();

                            try ui.configureElement(.{
                                .layout = .{
                                    .sizing = .{
                                        .w = .fit,
                                        .h = .fit,
                                    },
                                    .child_alignment = .center,
                                    .padding = .axes(10, 5),
                                },
                                .background_color = if (ui.active()) COLOR_PHOSPHOR else if (ui.hovered()) COLOR_TAN else if (ui.focused()) COLOR_TAN else COLOR_BROWN,
                                .border = .{
                                    .width = .all(1),
                                    .color = COLOR_PHOSPHOR,
                                },
                                .corner_radius = .all(5),
                                .state = .flags(.{
                                    .click = true,
                                    .focus = true,
                                    .activate = true,
                                }),
                            });

                            try ui.text("Load Model", .{
                                .font_id = FONT_ID_BODY,
                                .font_size = 16,
                                .color = if (!ui.active()) COLOR_PHOSPHOR else COLOR_GREYBROWN,
                            });
                        }

                        {
                            try ui.beginElement(.fromSlice("UnloadButton"));
                            defer ui.closeElement();

                            try ui.configureElement(.{
                                .layout = .{
                                    .sizing = .{
                                        .w = .fit,
                                        .h = .fit,
                                    },
                                    .child_alignment = .center,
                                    .padding = .axes(10, 5),
                                },
                                .background_color = if (demo.model != null) if (ui.active()) COLOR_RED_HOVER else if (ui.hovered()) COLOR_RED else if (ui.focused()) COLOR_RED else COLOR_BROWN else COLOR_TAN,
                                .border = .{
                                    .width = .all(1),
                                    .color = if (demo.model != null) COLOR_PHOSPHOR else COLOR_BROWN,
                                },
                                .corner_radius = .all(5),
                                .state = if (demo.model != null) .flags(.{
                                    .click = true,
                                    .focus = true,
                                    .activate = true,
                                }) else .flags(.{}),
                            });

                            try ui.text("Unload Model", .{
                                .font_id = FONT_ID_BODY,
                                .font_size = 16,
                                .color = if (demo.model != null) if (!ui.active()) COLOR_PHOSPHOR else COLOR_GREYBROWN else COLOR_BROWN,
                            });
                        }
                    }

                    if (demo.model) |*model| {
                        if (model.animations.len > 0) {
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

                            {
                                try ui.beginElement(.fromSlice("NameText"));
                                defer ui.closeElement();

                                try ui.configureElement(.{
                                    .layout = .{
                                        .sizing = .fit,
                                    },
                                });

                                try ui.text(model.animations[demo.anim_state.index].name orelse "[unnamed]", .{
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

                    {
                        {
                            try ui.beginElement(.fromSlice("TitleText2"));
                            defer ui.closeElement();

                            try ui.configureElement(.{
                                .layout = .{
                                    .sizing = .fit,
                                },
                            });

                            try ui.text("Light", .{
                                .font_id = FONT_ID_TITLE,
                                .font_size = 32,
                                .color = COLOR_PHOSPHOR,
                            });
                        }

                        {
                            try ui.beginElement(.fromSlice("BrightnessText"));
                            defer ui.closeElement();

                            try ui.configureElement(.{
                                .layout = .{
                                    .sizing = .fit,
                                },
                            });

                            var buf: [128]u8 = undefined;
                            try ui.text(try std.fmt.bufPrint(&buf, "{d:0.2}", .{demo.light.config.brightness}), .{
                                .font_id = FONT_ID_BODY,
                                .font_size = 16,
                                .color = COLOR_PHOSPHOR,
                            });
                        }

                        {
                            try ui.beginElement(.fromSlice("lightBrightnessSlider"));
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

                            try ui.bindSlider(f32, .{
                                .default = demo.light.config.brightness,
                                .min = 0.0,
                                .max = 3.0,

                                .track_color = if (ui.focused()) COLOR_TAN else COLOR_BROWN,
                                .handle_color = COLOR_PHOSPHOR,
                            });
                        }
                    }
                }
            }

            try ui.endLayout();
        }

        {
            const frame_view = app.gpu.beginFrame() orelse continue :main_loop;
            defer app.gpu.endFrame(frame_view);

            // Sync Render Targets to current window size
            const w = app.gpu.config.width;
            const h = app.gpu.config.height;
            try demo.gltf_color.resize(&app.gpu, @divFloor(app.gpu.config.width, SCALE_DIVISOR), @divFloor(app.gpu.config.height, SCALE_DIVISOR));
            try demo.gltf_depth.resize(&app.gpu, @divFloor(app.gpu.config.width, SCALE_DIVISOR), @divFloor(app.gpu.config.height, SCALE_DIVISOR));
            try demo.ui_msaa.resize(&app.gpu, w, h);
            try demo.ui_color.resize(&app.gpu, w, h);

            const encoder = app.gpu.getCommandEncoder("main_encoder");

            // --- Pass 1: GLTF Model (NO MSAA, Depth) ---
            demo.camera.calculateViewProj(app.window);
            demo.camera.sync(&app.gpu);

            demo.light.sync(&app.gpu);

            {
                const render_pass = wgpu.commandEncoderBeginRenderPass(encoder, &wgpu.RenderPassDescriptor{
                    .label = .fromSlice("main_render_pass"),
                    .color_attachment_count = 1,
                    .color_attachments = &[_]wgpu.RenderPassColorAttachment{.{
                        .view = demo.gltf_color.view,
                        .load_op = .clear,
                        .store_op = .store,
                        .clear_value = wgpu.Color{ .r = 0.1, .g = 0.2, .b = 0.3, .a = 1 },
                    }},
                    .depth_stencil_attachment = &wgpu.RenderPassDepthStencilAttachment{
                        .view = demo.gltf_depth.view,
                        .depth_load_op = .clear,
                        .depth_store_op = .store,
                        .depth_clear_value = 1.0,
                    },
                });
                defer wgpu.renderPassEncoderRelease(render_pass);

                if (demo.model) |*model| {
                    wgpu.renderPassEncoderSetPipeline(render_pass, render_pipeline);
                    for (model.primitives) |primitive| {
                        wgpu.renderPassEncoderSetBindGroup(render_pass, 0, demo.camera.bind_group, 0, null);
                        wgpu.renderPassEncoderSetBindGroup(render_pass, 1, primitive.texture_bind_group, 0, null);
                        wgpu.renderPassEncoderSetBindGroup(render_pass, 2, demo.anim_state.bind_group, 0, null);
                        wgpu.renderPassEncoderSetBindGroup(render_pass, 3, demo.light.bind_group, 0, null);
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
                        .clear_value = wgpu.Color{ .r = 0, .g = 0, .b = 0, .a = 0 },
                    }},
                });
                defer wgpu.renderPassEncoderRelease(render_pass);
                try renderer.render(render_pass);
                wgpu.renderPassEncoderEnd(render_pass);
            }

            // --- Pass 3: Composition ---

            demo.compositor.draw(&app.gpu, encoder, &demo.gltf_color, frame_view, .clear, .{ .r = 0, .g = 0, .b = 0, .a = 1 });
            demo.compositor.draw(&app.gpu, encoder, &demo.ui_color, frame_view, .load, .{});

            // --- WGPU Command Submission ---
            const cmd = app.gpu.finalizeCommandEncoder(encoder);
            app.gpu.submitCommands(&.{cmd});
        }
    }
}
