const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");

const nfd = @import("nfd");

const Gpu = @import("../Gpu.zig");
const Application = @import("../Application.zig");
const RenderTexture = @import("../RenderTexture.zig");
const Compositor = @import("../Compositor.zig");
const Camera = @import("../Camera.zig");
const EnvironmentLight = @import("../EnvironmentLight.zig");
const debug = @import("../debug.zig");
const Batch2D = @import("../Batch2D.zig");
const Batch3D = @import("../Batch3D.zig");
const AssetCache = @import("../AssetCache.zig");
const BindingState = @import("../BindingState.zig");
const Model = @import("../Model.zig");
const Ui = @import("../Ui.zig");
const widgets = Ui.widgets;
const linalg = @import("../linalg.zig");
const vec2u = linalg.vec2u;
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
    log.debug("semantic analysis for examples/3d_sandbox.zig", .{});
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
    picking: RenderTexture,
    picking_depth: RenderTexture,
    compositor: Compositor,

    path_to_load: error{DialogFailed}!?[]const u8 = null,
};

var FONT_ID_BODY: AssetCache.FontId = undefined;
var FONT_ID_TITLE: AssetCache.FontId = undefined;
var FONT_ID_MONO: AssetCache.FontId = undefined;
var IMAGE_ID_PIP: AssetCache.ImageId = undefined;

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

const UI_MSAA_SAMPLES = 4;
const DEPTH_FORMAT = Gpu.TextureFormat.depth32_float;

const MouseUniforms = extern struct {
    mouse_pos: [2]f32,
    _padding: [2]f32 = .{ 0.0, 0.0 }, // Explicit padding to reach 16 bytes
};

const PickingUniforms = struct {
    value: Value,
    uniform_buffer: ?*Gpu.Buffer = null,
    bind_group: ?*Gpu.BindGroup = null,

    pub const Value = extern struct {
        object_id: u32,
        _padding: [3]u32 = .{ 0, 0, 0 }, // Explicit padding to reach 16 bytes
    };

    var BIND_GROUP_LAYOUT: ?*Gpu.BindGroupLayout = null;
    pub fn getBindGroupLayout(gpu: *Gpu) !*Gpu.BindGroupLayout {
        if (BIND_GROUP_LAYOUT) |p| return p;

        BIND_GROUP_LAYOUT = try gpu.device.createBindGroupLayout(&.{
            .label = .fromSlice("PickingUniforms:bind_group_layout"),
            .entry_count = 1,
            .entries = &.{
                .{
                    .binding = 0,
                    .visibility = Gpu.ShaderStage.fragmentStage,
                    .buffer = .{ .type = .uniform },
                },
            },
        });

        return BIND_GROUP_LAYOUT.?;
    }

    pub fn init(object_id: u32) PickingUniforms {
        return PickingUniforms{
            .value = .{
                .object_id = object_id,
            },
        };
    }

    pub fn deinit(self: *PickingUniforms) void {
        if (self.uniform_buffer) |p| p.release();
        if (self.bind_group) |p| p.release();
    }

    pub fn sync(self: *PickingUniforms, gpu: *Gpu) !void {
        if (self.uniform_buffer == null) {
            self.uniform_buffer = try gpu.device.createBuffer(&.{
                .usage = .{ .uniform = true, .copy_dst = true },
                .size = @sizeOf(PickingUniforms.Value),
            });
        }

        if (self.bind_group == null) {
            self.bind_group = try gpu.device.createBindGroup(&.{
                .label = .fromSlice("PickingUniforms:bind_group"),
                .layout = try getBindGroupLayout(gpu),
                .entry_count = 1,
                .entries = &.{
                    .{
                        .binding = 0,
                        .buffer = self.uniform_buffer,
                        .offset = 0,
                        .size = @sizeOf(PickingUniforms.Value),
                    },
                },
            });
        }

        gpu.queue.writeBuffer(self.uniform_buffer.?, 0, &self.value, @sizeOf(PickingUniforms.Value));
    }
};

pub fn main() !void {
    @setEvalBranchQuota(10_000);

    var timer = try std.time.Timer.start();

    const app = try Application.init("zgpu");
    defer app.deinit();

    // Init Asset Cache
    var asset_cache = AssetCache.init(app.generalAllocator());
    defer asset_cache.deinit();

    try nfd.initAsync();
    defer nfd.deinitAsync();

    const b2d = try Batch2D.init(
        app.permanentAllocator(),
        &app.gpu,
        .rgba8_unorm_srgb,
        &asset_cache,
        UI_MSAA_SAMPLES,
    );
    defer b2d.deinit();

    const pwp_shader = try app.gpu.device.loadShaderText("static/shaders/2d/PickerWorldPos.wgsl", @embedFile("shaders/2d/PickerWorldPos.wgsl"));
    defer pwp_shader.release();

    const pid_shader = try app.gpu.device.loadShaderText("static/shaders/2d/PickerId.wgsl", @embedFile("shaders/2d/PickerId.wgsl"));
    defer pid_shader.release();

    const b3d = try Batch3D.init(
        app.permanentAllocator(),
        &app.gpu,
        .rgba8_unorm_srgb,
        .rgba32_float,
        DEPTH_FORMAT,
        1,
    );
    defer b3d.deinit();

    var demo = Demo{
        .app = app,
        .camera = Camera.fromLookAt(
            vec3{ 10.0, 2.0, 0.0 },
            vec3{ 0, 1.0, 0.0 },
            vec3{ 0.0, 1.0, 0.0 },
            @floatFromInt(vec2u{ app.gpu.config.width, app.gpu.config.height }),
        ),
        .gltf_color = try RenderTexture.init(&app.gpu, .{
            .label = "gltf_color",
            .width = @divFloor(app.gpu.config.width, SCALE_DIVISOR),
            .height = @divFloor(app.gpu.config.height, SCALE_DIVISOR),
            .format = .rgba8_unorm_srgb,
            .usage = .{ .render_attachment = true, .texture_binding = true, .copy_src = true },
        }),
        .gltf_depth = try RenderTexture.init(&app.gpu, .{
            .label = "gltf_depth",
            .width = @divFloor(app.gpu.config.width, SCALE_DIVISOR),
            .height = @divFloor(app.gpu.config.height, SCALE_DIVISOR),
            .format = DEPTH_FORMAT,
            .usage = .{ .render_attachment = true },
        }),
        .ui_msaa = try RenderTexture.init(&app.gpu, .{
            .label = "ui_msaa",
            .width = app.gpu.config.width,
            .height = app.gpu.config.height,
            .format = .rgba8_unorm_srgb,
            .usage = .{ .render_attachment = true },
            .sample_count = UI_MSAA_SAMPLES,
        }),
        .ui_color = try RenderTexture.init(&app.gpu, .{
            .label = "ui_color",
            .width = app.gpu.config.width,
            .height = app.gpu.config.height,
            .format = .rgba8_unorm_srgb,
            .usage = .{ .render_attachment = true, .texture_binding = true },
        }),
        .picking = try RenderTexture.init(&app.gpu, .{
            .label = "picking_texture",
            .width = @divFloor(app.gpu.config.width, SCALE_DIVISOR),
            .height = @divFloor(app.gpu.config.height, SCALE_DIVISOR),
            .format = .rgba32_float,
            .usage = .{ .render_attachment = true, .texture_binding = true, .copy_src = true },
        }),
        .picking_depth = try RenderTexture.init(&app.gpu, .{
            .label = "picking_depth",
            .width = @divFloor(app.gpu.config.width, SCALE_DIVISOR),
            .height = @divFloor(app.gpu.config.height, SCALE_DIVISOR),
            .format = DEPTH_FORMAT,
            .usage = .{ .render_attachment = true },
        }),
        .compositor = try Compositor.init(&app.gpu, null),
    };
    defer demo.gltf_color.deinit();
    defer demo.gltf_depth.deinit();
    defer demo.ui_msaa.deinit();
    defer demo.ui_color.deinit();
    defer demo.picking.deinit();
    defer demo.picking_depth.deinit();
    defer demo.compositor.deinit();
    defer demo.anim_state.deinit();

    app.user_data = &demo;

    // Init Ui

    var bindings = BindingState.init(app.permanentAllocator(), &app.input_state);
    defer bindings.deinit();

    try bindings.bind(.toggle_debugger, .{ .key = .{ .bind_point = .d, .modifiers = .ctrlMod } });
    try bindings.bind(.dump_atlas, .{ .key = .{ .bind_point = .a, .modifiers = .altMod } });
    try bindings.bind(.open_context_menu, .{ .mouse = .{ .bind_point = .button_2 } });

    var ui = try Ui.init(app.permanentAllocator(), app.frameAllocator(), b2d, &asset_cache, &bindings);
    defer ui.deinit();

    FONT_ID_BODY = try asset_cache.loadFont("assets/fonts/pixels/Habbo.ttf", .nearest);
    FONT_ID_TITLE = try asset_cache.loadFont("assets/fonts/pixels/ModernDOS9x16.ttf", .nearest);
    FONT_ID_MONO = try asset_cache.loadFont("assets/fonts/pixels/PublicPixel.ttf", .nearest);

    IMAGE_ID_PIP = try b2d.createDynamicTexture(app.gpu.config.width, app.gpu.config.height);

    defer if (demo.model) |*m| m.deinit();

    var draw_fps = true;

    const skinned_mesh_lit_shader = try app.gpu.device.loadShaderText("static/shaders/GltfMultiPurpose.wgsl", @embedFile("shaders/GltfMultiPurpose.wgsl"));
    defer skinned_mesh_lit_shader.release();

    const skinned_mesh_picking_shader = try app.gpu.device.loadShaderText("static/shaders/picking/SkinnedMesh.wgsl", @embedFile("shaders/picking/SkinnedMesh.wgsl"));
    defer skinned_mesh_picking_shader.release();

    const skinned_mesh_lit_pipeline_layout = try app.gpu.device.createPipelineLayout(&.{
        .label = .fromSlice("skinned_mesh_lit_pipeline_layout:pipeline_layout"),
        .bind_group_layout_count = 4,
        .bind_group_layouts = &.{
            try Camera.getBindGroupLayout(&app.gpu),
            try Model.getTextureBindGroupLayout(&app.gpu),
            try Model.AnimationState.getBindGroupLayout(&app.gpu),
            try EnvironmentLight.getBindGroupLayout(&app.gpu),
        },
    });
    defer skinned_mesh_lit_pipeline_layout.release();

    const skinned_mesh_picking_pipeline_layout = try app.gpu.device.createPipelineLayout(&.{
        .label = .fromSlice("skinned_mesh_picking_pipeline_layout:pipeline_layout"),
        .bind_group_layout_count = 3,
        .bind_group_layouts = &.{
            try Camera.getBindGroupLayout(&app.gpu),
            try Model.AnimationState.getBindGroupLayout(&app.gpu),
            try PickingUniforms.getBindGroupLayout(&app.gpu),
        },
    });
    defer skinned_mesh_picking_pipeline_layout.release();

    const skinned_mesh_lit_pipeline = try app.gpu.device.createRenderPipeline(&Gpu.RenderPipelineDescriptor{
        .label = .fromSlice("skinned_mesh_lit_pipeline:render_pipeline"),
        .layout = skinned_mesh_lit_pipeline_layout,
        .vertex = .{
            .module = skinned_mesh_lit_shader,
            .entry_point = .fromSlice("vs_main"),
            .buffer_count = 1,
            .buffers = &.{Model.Vertex.buffer_layout},
        },
        .primitive = .{
            .topology = .triangle_list,
            .cull_mode = .none,
            .front_face = .ccw,
        },
        .depth_stencil = &Gpu.DepthStencilState{
            .format = DEPTH_FORMAT,
            .depth_write_enabled = .true,
            .depth_compare = .less,
        },
        .fragment = &Gpu.FragmentState{
            .module = skinned_mesh_lit_shader,
            .entry_point = .fromSlice("fs_main"),
            .target_count = 1,
            .targets = &.{
                Gpu.ColorTargetState{
                    .format = .rgba8_unorm_srgb,
                },
            },
        },
    });
    defer skinned_mesh_lit_pipeline.release();

    const skinned_mesh_picking_pipeline = try app.gpu.device.createRenderPipeline(&Gpu.RenderPipelineDescriptor{
        .label = .fromSlice("skinned_mesh_picking_pipeline:render_pipeline"),
        .layout = skinned_mesh_picking_pipeline_layout,
        .vertex = .{
            .module = skinned_mesh_picking_shader,
            .entry_point = .fromSlice("vs_main"),
            .buffer_count = 1,
            .buffers = &.{Model.Vertex.buffer_layout},
        },
        .primitive = .{
            .topology = .triangle_list,
            .cull_mode = .none,
            .front_face = .ccw,
        },
        .depth_stencil = &Gpu.DepthStencilState{
            .format = DEPTH_FORMAT,
            .depth_write_enabled = .true,
            .depth_compare = .less,
        },
        .fragment = &Gpu.FragmentState{
            .module = skinned_mesh_picking_shader,
            .entry_point = .fromSlice("fs_picking"),
            .target_count = 1,
            .targets = &.{
                Gpu.ColorTargetState{
                    .format = .rgba32_float,
                },
            },
        },
    });
    defer skinned_mesh_picking_pipeline.release();

    var picking_uniforms = PickingUniforms.init(1);
    defer picking_uniforms.deinit();
    try picking_uniforms.sync(&app.gpu);

    const readback_buffer = try app.gpu.device.createBuffer(&.{
        .label = .fromSlice("Picking Readback Buffer"),
        .size = 256, // 4 * @sizeOf(f32)
        .usage = .{ .map_read = true, .copy_dst = true },
    });
    defer readback_buffer.release();

    var camera_captured_mouse = false;

    const BufferDebugMode = enum { None, Framebuffer, @"Picking Position", @"Picking Id" };
    var buffer_debug_mode: BufferDebugMode = .None;
    var text_buffer: std.ArrayList(u8) = .empty;
    try text_buffer.appendSlice(app.generalAllocator(), "Test text input");
    var dummy_slider_value: f32 = 0.0;
    var anim_index_slider: u32 = 0;

    var theme = Ui.Theme.init(app.generalAllocator());
    defer theme.deinit();

    try theme.setAll(.content, .standard, .{
        .background_color = COLOR_GREYBROWN,
        .border_color = COLOR_PHOSPHOR,
        .text_color = COLOR_PHOSPHOR,
        .font_id = FONT_ID_BODY,
        .font_size = @as(u16, 16),

        .padding = Ui.Padding.all(10),
        .border_width = Ui.BorderWidth.all(1),
        .corner_radius = Ui.CornerRadius.all(8),
        .child_gap = @as(u16, 10),
    });

    try theme.setAll(.widget, .standard, .{
        .background_color = COLOR_BROWN,
        .checkbox_mark = Ui.Widget.Checkbox.Mark.block,
        .checkbox_mark_color = COLOR_PHOSPHOR,
        .radio_mark_color = COLOR_PHOSPHOR,
        .slider_handle_color = COLOR_PHOSPHOR,
        .text_selection_color = COLOR_PHOSPHOR.withAlpha(0.1),
        .text_caret_color = COLOR_PHOSPHOR,
    });

    try theme.setAll(.widget, .active, .{
        .background_color = COLOR_TAN,
    });

    try theme.setAll(.content, .disabled, .{
        .background_color = COLOR_TAN,
        .text_color = COLOR_BROWN,
        .border_color = COLOR_BROWN,
    });

    const startup_ms = debug.start(&timer);
    log.info("startup completed in {d} ms", .{startup_ms});

    var last_frame_time = std.time.milliTimestamp();
    main_loop: while (app.beginFrame()) {
        const current_time = std.time.milliTimestamp();
        const delta_time: f32 = @as(f32, @floatFromInt(current_time - last_frame_time)) / 1000.0;
        last_frame_time = current_time;

        defer debug.lap();

        const w = app.gpu.config.width;
        const h = app.gpu.config.height;
        const screen_ratio = @as(f32, @floatFromInt(w)) / @as(f32, @floatFromInt(h));

        if (demo.path_to_load) |maybe_path| {
            if (maybe_path) |p| {
                // Deinit old model if it exists
                if (demo.model) |*m| m.deinit();

                demo.model = try Model.loadGltf(app.generalAllocator(), &demo.app.gpu, p);

                demo.anim_state.setIndex(0);
                demo.anim_state.clearMatrices();
                try demo.anim_state.sync(&demo.app.gpu);
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

            try demo.anim_state.sync(&demo.app.gpu);
        }

        // Sync Render Targets to current window size
        try demo.gltf_color.resize(&app.gpu, @divFloor(w, SCALE_DIVISOR), @divFloor(h, SCALE_DIVISOR));
        try demo.gltf_depth.resize(&app.gpu, @divFloor(w, SCALE_DIVISOR), @divFloor(h, SCALE_DIVISOR));
        try demo.picking.resize(&app.gpu, @divFloor(w, SCALE_DIVISOR), @divFloor(h, SCALE_DIVISOR));
        try demo.picking_depth.resize(&app.gpu, @divFloor(w, SCALE_DIVISOR), @divFloor(h, SCALE_DIVISOR));
        try demo.ui_msaa.resize(&app.gpu, w, h);
        try demo.ui_color.resize(&app.gpu, w, h);

        // --- Process events ---

        const ui_wants_mouse = ui.wantsMouse(.fullscreen);
        if (camera_captured_mouse or !ui_wants_mouse) {
            camera_captured_mouse = demo.camera.update(&app.input_state, @floatFromInt(vec2u{ app.gpu.config.width, app.gpu.config.height }), delta_time);
        }

        const mouse_pos = app.input_state.getMousePosition();
        const mouse_uniforms = std.mem.asBytes(&MouseUniforms{
            .mouse_pos = .{ mouse_pos[0] / SCALE_DIVISOR, mouse_pos[1] / SCALE_DIVISOR },
        });

        // Generate the UI layout and cache rendering commands
        {
            try ui.beginLayout(
                !camera_captured_mouse,
                .{ @floatFromInt(w), @floatFromInt(h) },
                delta_time,
            );
            defer ui.endLayout() catch |err| {
                log.err("Failed to end UI layout: {s}\n", .{@errorName(err)});
            };

            try ui.pushTheme(&theme);

            { // main window
                try ui.beginSection(.fromSlice("OuterContainer"), .{
                    .sizing = .{ .w = .fit, .h = .grow },
                    .direction = .top_to_bottom,
                    .child_alignment = .center,
                    .padding = .all(10),
                });
                defer ui.endSection();

                {
                    try ui.openElement(.fromSlice("MenuContainer"));
                    defer ui.endElement();

                    try ui.configureElement(.{
                        .sizing = .{ .w = .fit, .h = .fitMinMax(.{ .min = 0, .max = @floatFromInt(h - 20) }) },
                        .direction = .top_to_bottom,
                        .child_alignment = .{ .x = .center, .y = .top },
                        .clip = .{ .child_offset = ui.scrollOffset(), .vertical = true },
                    });

                    {
                        try ui.beginSection(.fromSlice("ButtonRow"), .{
                            .sizing = .{ .w = .grow, .h = .fit },
                            .direction = .left_to_right,
                            .child_alignment = .{ .x = .left, .y = .center },
                        });
                        defer ui.endSection();

                        if (try widgets.button(ui, .fromSlice("LoadButton"), "Load Model", .{})) {
                            try nfd.openDialogAsync("glb,gltf", ".", &demo.path_to_load);
                        }

                        if (try widgets.button(ui, .fromSlice("UnloadButton"), "Unload Model", .{ .disabled = demo.model == null })) {
                            if (demo.model) |*m| m.deinit();
                            demo.model = null;

                            demo.anim_state.clearCache();
                            demo.anim_state.clearMatrices();
                            try demo.anim_state.sync(&demo.app.gpu);
                        }
                    }

                    if (demo.model) |*model| {
                        if (model.animations.len > 0) {
                            try ui.text("Animation", .{
                                .font_id = FONT_ID_TITLE,
                                .font_size = 32,
                            });

                            try ui.text(model.animations[demo.anim_state.index].name orelse "[unnamed]", .{});

                            if (try widgets.slider(u32, ui, .fromSlice("animIndexSlider"), &anim_index_slider, .{
                                .min = 0,
                                .max = if (demo.model != null and demo.model.?.animations.len > 0) @intCast(demo.model.?.animations.len - 1) else 0,
                            })) {
                                demo.anim_state.setIndex(anim_index_slider);
                            }
                        }
                    }

                    {
                        try ui.text("Debug Options", .{
                            .font_id = FONT_ID_TITLE,
                            .font_size = 32,
                        });

                        // Random text input
                        if (try widgets.textInput(
                            ui,
                            .fromSlice("TextInputTest"),
                            app.generalAllocator(),
                            &text_buffer,
                            .{ .sizing = .{ .w = .fixed(300), .h = .fixed(16 * 6) } },
                        )) {
                            std.debug.print("Text input changed:\n{s}\n", .{text_buffer.items});
                        }

                        { // Fps Overlay
                            try ui.beginSection(.fromSlice("DebugFPSOverlayControlSection"), .{
                                .sizing = .fit,
                                .direction = .left_to_right,
                                .child_alignment = .center,
                            });
                            defer ui.endSection();

                            try ui.text("Show FPS Overlay", .{});

                            _ = try widgets.checkbox(ui, .fromSlice("fpsOverlayToggle"), &draw_fps, .{});
                        }

                        { // Light brightness
                            try ui.beginSection(.fromSlice("SliderRow"), .{
                                .sizing = .{ .w = .grow, .h = .fit },
                                .direction = .left_to_right,
                                .child_alignment = .{ .x = .left, .y = .center },
                            });
                            defer ui.endSection();

                            try ui.text("Light", .{});

                            var buf: [4]u8 = undefined;
                            try ui.text(try std.fmt.bufPrint(&buf, "{d:0.2}", .{demo.light.config.brightness}), .{ .font_id = FONT_ID_MONO });

                            _ = try widgets.slider(f32, ui, .fromSlice("lightBrightnessSlider"), &demo.light.config.brightness, .{ .min = 0.0, .max = 3.0 });
                        }

                        { // Buffer Debug
                            {
                                try ui.beginSection(.fromSlice("DropdownRow"), .{
                                    .sizing = .{ .w = .grow, .h = .fit },
                                    .direction = .left_to_right,
                                    .child_alignment = .{ .x = .left, .y = .center },
                                });
                                defer ui.endSection();

                                try ui.text("Buffer Debug Mode", .{});

                                _ = try widgets.enumDropdown(BufferDebugMode, ui, .fromSlice("BufferDebugModeDropdown"), &buffer_debug_mode, .{});
                            }

                            {
                                try ui.beginSection(.fromSlice("RadioRow"), .{
                                    .sizing = .{ .w = .grow, .h = .fit },
                                    .direction = .top_to_bottom,
                                    .child_alignment = .{ .x = .left, .y = .center },
                                });
                                defer ui.endSection();

                                inline for (comptime std.meta.fieldNames(BufferDebugMode)) |mode_name| {
                                    try ui.beginSection(.fromSlice("RadiowSubRow_" ++ mode_name), .{
                                        .sizing = .{ .w = .grow, .h = .fit },
                                        .direction = .left_to_right,
                                        .child_alignment = .{ .x = .left, .y = .center },
                                    });
                                    defer ui.endSection();

                                    _ = try widgets.enumRadioButton(BufferDebugMode, ui, .fromSlice(mode_name ++ "_radio"), &buffer_debug_mode, @field(BufferDebugMode, mode_name), .{});

                                    try ui.text(mode_name, .{});
                                }
                            }

                            {
                                try ui.beginSection(.fromSlice("EnumSliderRow"), .{
                                    .sizing = .{ .w = .grow, .h = .fit },
                                    .direction = .left_to_right,
                                    .child_alignment = .{ .x = .left, .y = .center },
                                });
                                defer ui.endSection();

                                try ui.text(@tagName(buffer_debug_mode), .{});

                                _ = try widgets.enumSlider(BufferDebugMode, ui, .fromSlice("buffer_debug_slider"), &buffer_debug_mode, .{});
                            }

                            switch (buffer_debug_mode) {
                                .None => {},

                                .Framebuffer => {
                                    try widgets.image(ui, .fromSlice("PictureInPictureTest"), .{
                                        .image_id = IMAGE_ID_PIP,
                                        .aspect_ratio = screen_ratio,
                                    });
                                },

                                .@"Picking Position" => {
                                    try widgets.shaderRect(ui, .fromSlice("ShaderTest1"), .{
                                        .shader = pwp_shader,
                                        .texture_bindings = &.{
                                            .{ .sample_type = .unfilterable_float, .view_dimension = .@"2d", .multisampled = .False },
                                        },
                                        .textures = &.{demo.picking.view.?},
                                        .uniforms = mouse_uniforms,
                                        .sizing = .{ .w = .fixed(300), .h = .fixed(300 / screen_ratio) },
                                    });
                                },

                                .@"Picking Id" => {
                                    try widgets.shaderRect(ui, .fromSlice("ShaderTest2"), .{
                                        .shader = pid_shader,
                                        .texture_bindings = &.{
                                            .{ .sample_type = .unfilterable_float, .view_dimension = .@"2d", .multisampled = .False },
                                        },
                                        .textures = &.{demo.picking.view.?},
                                        .uniforms = mouse_uniforms,
                                        .sizing = .{ .w = .fixed(300), .h = .fixed(300 / screen_ratio) },
                                    });
                                },
                            }
                        }
                    }
                }
            }

            { // menus
                try ui.pushTheme(&theme);
                defer _ = ui.popTheme();

                if (bindings.getAction(.open_context_menu) == .released) {
                    log.info("Right click released, opening context menu.", .{});
                    ui.openOverlay(.fromSlice("ContextMenuRoot"), null, null, mouse_pos);
                }

                // Root Context Menu
                if (try widgets.beginMenu(ui, .fromSlice("ContextMenuRoot"))) {
                    defer widgets.endMenu(ui);

                    if (try widgets.menuItem(ui, .fromSlice("CopyMenuItem"), "Copy", .{})) {
                        std.debug.print("ContextMenu - Copy\n", .{});
                    }
                    if (try widgets.menuItem(ui, .fromSlice("PasteMenuItem"), "Paste", .{})) {
                        std.debug.print("ContextMenu - Paste\n", .{});
                    }

                    _ = ui.popTheme(); // Revert back to the global menu theme

                    try widgets.menuSeparator(ui, .fromSlice("ContextMenuSep1"));

                    _ = try widgets.subMenu(ui, .fromSlice("MoreOptionsMenuItem"), "More Options...", .fromSlice("SubMenu1"), .{});

                    try widgets.menuSeparator(ui, .fromSlice("ContextMenuSep2"));

                    // Embedded Widget Test
                    {
                        try widgets.beginMenuEmbeddedLayout(ui, .fromSlice("MenuSliderContainer"), .{});
                        defer widgets.endMenuEmbeddedLayout(ui);

                        try ui.text("Opacity", .{});

                        if (try widgets.slider(f32, ui, .fromSlice("MenuSlider"), &dummy_slider_value, .{})) std.debug.print("dummy_slider_value: {d:0.2}\n", .{dummy_slider_value});
                    }
                }

                // First Level Submenu
                if (try widgets.beginMenu(ui, .fromSlice("SubMenu1"))) {
                    defer widgets.endMenu(ui);

                    if (try widgets.menuItem(ui, .fromSlice("OptionAMenuItem"), "Option A", .{})) std.debug.print("OptionAMenuItem\n", .{});
                    if (try widgets.menuItem(ui, .fromSlice("OptionBMenuItem"), "Option B", .{})) std.debug.print("OptionBMenuItem\n", .{});

                    _ = try widgets.subMenu(ui, .fromSlice("EvenDeeperMenuItem"), "Even Deeper...", .fromSlice("SubMenu2"), .{});
                }

                // Second Level Submenu
                if (try widgets.beginMenu(ui, .fromSlice("SubMenu2"))) {
                    defer widgets.endMenu(ui);

                    if (try widgets.menuItem(ui, .fromSlice("FinalOptionMenuItem"), "Final Option!", .{})) std.debug.print("FinalOptionMenuItem\n", .{});
                }
            }
        }

        {
            const frame_view = try app.gpu.beginFrame() orelse continue :main_loop;
            defer {
                app.gpu.endFrame(frame_view) catch |err| {
                    log.err("Failed to present frame: {s}\n", .{@errorName(err)});
                };
            }

            const encoder = try app.gpu.device.createCommandEncoder(&.{ .label = .fromSlice("main_encoder") });
            defer encoder.release();

            // --- Pass 1: GLTF Model (NO MSAA, Depth) ---
            try demo.camera.sync(&app.gpu);

            try demo.light.sync(&app.gpu);

            {
                const render_pass = try encoder.beginRenderPass(&Gpu.RenderPassDescriptor{
                    .label = .fromSlice("main_render_pass"),
                    .color_attachment_count = 1,
                    .color_attachments = &[_]Gpu.RenderPassColorAttachment{.{
                        .view = demo.gltf_color.view,
                        .load_op = .clear,
                        .store_op = .store,
                        .clear_value = Gpu.Color{ .r = 0.1, .g = 0.1, .b = 0.1, .a = 1 },
                    }},
                    .depth_stencil_attachment = &Gpu.RenderPassDepthStencilAttachment{
                        .view = demo.gltf_depth.view,
                        .depth_load_op = .clear,
                        .depth_store_op = .store,
                        .depth_clear_value = 1.0,
                    },
                });
                defer render_pass.release();

                if (demo.model) |*model| {
                    render_pass.setPipeline(skinned_mesh_lit_pipeline);
                    for (model.primitives) |primitive| {
                        render_pass.setBindGroup(0, demo.camera.bind_group, &.{});
                        render_pass.setBindGroup(1, primitive.texture_bind_group, &.{});
                        render_pass.setBindGroup(2, demo.anim_state.bind_group, &.{});
                        render_pass.setBindGroup(3, demo.light.bind_group, &.{});
                        render_pass.setVertexBuffer(0, primitive.vertex_buffer, 0, Gpu.whole_size);
                        if (primitive.index_buffer) |ib| {
                            render_pass.setIndexBuffer(ib, primitive.index_format, 0, Gpu.whole_size);
                            render_pass.drawIndexed(primitive.index_count, 1, 0, 0, 0);
                        } else {
                            render_pass.draw(primitive.vertex_count, 1, 0, 0);
                        }
                    }
                }

                {
                    b3d.beginFrame(demo.camera.view_proj);
                    try b3d.drawCylinder(linalg.mat4_scale(.{ 0.1, 2, 0.1 }), 3, .{ .r = 1, .g = 0, .b = 1, .a = 1 }, 2);
                    try b3d.drawCylinder(linalg.mat4_scale(.{ 0.5, 1, 0.5 }), 6, .{ .r = 0, .g = 1, .b = 1, .a = 1 }, 3);
                    try b3d.endFrame();
                    try b3d.render(render_pass);
                }

                render_pass.end();
            }

            {
                const picking_pass = try encoder.beginRenderPass(&Gpu.RenderPassDescriptor{
                    .label = .fromSlice("picking_render_pass"),
                    .color_attachment_count = 1,
                    .color_attachments = &[_]Gpu.RenderPassColorAttachment{.{
                        .view = demo.picking.view,
                        .load_op = .clear,
                        .store_op = .store,
                        .clear_value = Gpu.Color{ .r = 0, .g = 0, .b = 0, .a = 0 },
                    }},
                    .depth_stencil_attachment = &Gpu.RenderPassDepthStencilAttachment{
                        .view = demo.picking_depth.view,
                        .depth_load_op = .clear,
                        .depth_store_op = .store,
                        .depth_clear_value = 1.0,
                    },
                });
                defer picking_pass.release();

                try b3d.renderPicking(picking_pass);

                if (demo.model) |*model| {
                    picking_pass.setPipeline(skinned_mesh_picking_pipeline);
                    for (model.primitives) |primitive| {
                        picking_pass.setBindGroup(0, demo.camera.bind_group, &.{});
                        picking_pass.setBindGroup(1, demo.anim_state.bind_group, &.{});
                        picking_pass.setBindGroup(2, picking_uniforms.bind_group, &.{});
                        picking_pass.setVertexBuffer(0, primitive.vertex_buffer, 0, Gpu.whole_size);
                        if (primitive.index_buffer) |ib| {
                            picking_pass.setIndexBuffer(ib, primitive.index_format, 0, Gpu.whole_size);
                            picking_pass.drawIndexed(primitive.index_count, 1, 0, 0, 0);
                        } else {
                            picking_pass.draw(primitive.vertex_count, 1, 0, 0);
                        }
                    }
                }

                picking_pass.end();
            }

            const clicked = !ui_wants_mouse and app.input_state.getMouseButton(.left) == .released;

            if (clicked) {
                const m_pos = app.input_state.getMousePosition();
                const tx = @as(u32, @intFromFloat(@max(0, m_pos[0] / SCALE_DIVISOR)));
                const ty = @as(u32, @intFromFloat(@max(0, m_pos[1] / SCALE_DIVISOR)));

                // Ensure we don't read out of bounds
                if (tx < demo.picking.desc.width and ty < demo.picking.desc.height) {
                    encoder.copyTextureToBuffer(
                        &.{ // Source (Texture)
                            .texture = demo.picking.texture,
                            .mip_level = 0,
                            .origin = .{ .x = tx, .y = ty, .z = 0 },
                        },
                        &.{ // Destination (Buffer)
                            .buffer = readback_buffer,
                            .layout = .{
                                .offset = 0,
                                .bytes_per_row = 256,
                                .rows_per_image = 1,
                            },
                        },
                        &.{ .width = 1, .height = 1, .depth_or_array_layers = 1 },
                    );
                }
            }

            try b2d.updateDynamicTexture(encoder, IMAGE_ID_PIP, demo.gltf_color.texture.?, demo.gltf_color.desc.width, demo.gltf_color.desc.height);

            // --- Pass 2: UI (MSAA resolving to UI Color) ---
            {
                const proj = linalg.mat4_ortho(0, @floatFromInt(w), @floatFromInt(h), 0, -1, 1);
                b2d.beginFrame(proj, w, h);
                if (draw_fps) try debug.drawFpsChart(b2d, .{ 0, 0 });
                try ui.render();
                try b2d.endFrame();

                const render_pass = try encoder.beginRenderPass(&Gpu.RenderPassDescriptor{
                    .label = .fromSlice("ui_render_pass"),
                    .color_attachment_count = 1,
                    .color_attachments = &[_]Gpu.RenderPassColorAttachment{.{
                        .view = demo.ui_msaa.view,
                        .resolve_target = demo.ui_color.view,
                        .load_op = .clear,
                        .store_op = .store,
                        .clear_value = Gpu.Color{ .r = 0, .g = 0, .b = 0, .a = 0 },
                    }},
                });
                defer render_pass.release();
                try b2d.render(render_pass);
                render_pass.end();
            }

            // --- Pass 3: Composition ---

            try demo.compositor.draw(&app.gpu, encoder, &demo.gltf_color, frame_view, .clear, .{ .r = 0, .g = 0, .b = 0, .a = 1 });
            try demo.compositor.draw(&app.gpu, encoder, &demo.ui_color, frame_view, .load, .{});

            // --- WGPU Command Submission ---
            const cmd = try encoder.finish(null);
            app.gpu.queue.submit(&.{cmd});

            if (clicked) {
                _ = readback_buffer.mapAsync(.{ .read = true }, 0, 16, .{
                    .callback = struct {
                        pub fn picker_read_callback(status: Gpu.MapAsyncStatus, _: Gpu.StringView, userdata1: ?*anyopaque, _: ?*anyopaque) callconv(.c) void {
                            if (status == .success) {
                                const buf: *Gpu.Buffer = @ptrCast(@alignCast(userdata1));
                                const data = buf.getConstMappedRange(0, 16);
                                const values: []const f32 = @as([*]const f32, @ptrCast(@alignCast(data)))[0..4];

                                std.debug.print("Picked object value: {any}\n", .{values});
                                buf.unmap();
                            }
                        }
                    }.picker_read_callback,
                    .userdata1 = readback_buffer,
                });
            }
        }
    }
}
