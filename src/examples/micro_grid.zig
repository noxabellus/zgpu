//! Test of Voxel Grid

const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");

const wgpu = @import("wgpu");
const glfw = @import("glfw");

const debug = @import("../debug.zig");
const linalg = @import("../linalg.zig");
const Grid = @import("../MicroGrid.zig");
const Application = @import("../Application.zig");

const vec2 = linalg.vec2;
const vec3 = linalg.vec3;
const vec3i = linalg.vec3i;
const mat4 = linalg.mat4;

pub const std_options = std.Options{
    .log_level = .info,
};

test {
    log.debug("semantic analysis for examples/micro_grid.zig", .{});
    std.testing.refAllDecls(@This());
}

const Demo = struct {
    app: *Application,

    // --- Camera state for fly controls (Z-up coordinate system) ---
    camera_pos: vec3 = .{ 8.125, 28.125, 2 }, // Start outside the sphere (X, Y, Z with Z-up)
    camera_front: vec3 = .{ 0.0, -1.0, 0.0 }, // Looking towards the sphere
    camera_up: vec3 = .{ 0.0, 0.0, 1.0 }, // Z is up
    camera_right: vec3 = .{ 0.0, 0.0, 0.0 }, // will be calculated

    yaw: f32 = -90.0,
    pitch: f32 = 0.0,

    first_mouse: bool = true,
    last_x: f64 = 400.0,
    last_y: f64 = 300.0,

    // Timing
    delta_time: f32 = 0.0,
    last_frame: f32 = 0.0,
};

// --- Mouse movement callback ---
fn handle_mouse_move(w: *glfw.Window, xpos: f64, ypos: f64) callconv(.c) void {
    const app: *Application = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse return));
    const d: *Demo = @ptrCast(@alignCast(app.user_data));

    const btn_state = glfw.getMouseButton(w, .button_3);
    if (btn_state != .repeat and btn_state != .press) {
        d.first_mouse = true;
        return;
    }

    if (d.first_mouse) {
        d.last_x = xpos;
        d.last_y = ypos;
        d.first_mouse = false;
    }

    var x_offset = d.last_x - xpos;
    var y_offset = d.last_y - ypos; // reversed since y-coordinates go from top to bottom
    d.last_x = xpos;
    d.last_y = ypos;

    const sensitivity: f64 = 0.05;
    x_offset *= sensitivity;
    y_offset *= sensitivity;

    d.yaw += @floatCast(x_offset);
    d.pitch += @floatCast(y_offset);

    // constrain pitch
    if (d.pitch > 89.0) {
        d.pitch = 89.0;
    }
    if (d.pitch < -89.0) {
        d.pitch = -89.0;
    }

    var front: vec3 = undefined;
    const yaw_rad = linalg.deg_to_rad * d.yaw;
    const pitch_rad = linalg.deg_to_rad * d.pitch;
    // Z-up coordinate system: X-right, Y-forward, Z-up
    front[0] = std.math.cos(yaw_rad) * std.math.cos(pitch_rad);
    front[1] = std.math.sin(yaw_rad) * std.math.cos(pitch_rad);
    front[2] = std.math.sin(pitch_rad);
    d.camera_front = linalg.normalize(front);
    // Also re-calculate the Right and Up vector (world up is Z-axis)
    d.camera_right = linalg.normalize(linalg.vec3_cross(d.camera_front, .{ 0.0, 0.0, 1.0 }));
    d.camera_up = linalg.normalize(linalg.vec3_cross(d.camera_right, d.camera_front));
}

// A struct for our camera's view-projection matrix uniform.
const CameraUniform = extern struct {
    view_proj: mat4,
};

// The WGSL shader source code.
// This now includes basic directional lighting using the vertex normal.
const shader_text =
    \\struct CameraUniform {
    \\    view_proj: mat4x4<f32>,
    \\};
    \\@group(0) @binding(0)
    \\var<uniform> u_camera: CameraUniform;
    \\
    \\@group(0) @binding(1)
    \\var<storage, read> material_colors: array<u32>;
    \\
    \\struct VertexOutput {
    \\    @builtin(position) clip_position: vec4<f32>,
    \\    @location(0) norm: vec3<f32>,
    \\    @location(1) color: vec3<f32>,
    \\};
    \\
    \\fn unpackColor(packed: u32) -> vec3<f32> {
    \\    let r = f32((packed >> 0u) & 0xFFu) / 255.0;
    \\    let g = f32((packed >> 8u) & 0xFFu) / 255.0;
    \\    let b = f32((packed >> 16u) & 0xFFu) / 255.0;
    \\    return vec3<f32>(r, g, b);
    \\}
    \\
    \\@vertex
    \\fn vs_main(
    \\    @location(0) pos: vec3<f32>,
    \\    @location(1) norm: vec3<f32>,
    \\    @location(2) uv: vec2<f32>,
    \\    @location(3) material_id: u32,
    \\) -> VertexOutput {
    \\    var out: VertexOutput;
    \\    out.clip_position = u_camera.view_proj * vec4<f32>(pos, 1.0);
    \\    out.norm = norm;
    \\    // Look up material color from the storage buffer
    \\    out.color = unpackColor(material_colors[material_id]);
    \\    return out;
    \\}
    \\
    \\@fragment
    \\fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    \\    // Basic directional lighting
    \\    let light_dir = normalize(vec3<f32>(0.5, 1.0, 0.75));
    \\    let ambient = 0.2;
    \\    let diffuse = max(dot(in.norm, light_dir), 0.0) * 0.8;
    \\    let intensity = ambient + diffuse;
    \\    return vec4<f32>(in.color * intensity, 1.0);
    \\}
;

pub fn main() !void {
    var timer = try std.time.Timer.start();

    var tsa = std.heap.ThreadSafeAllocator{
        .child_allocator = std.heap.page_allocator,
    };
    const gpa = tsa.allocator();

    if (comptime builtin.os.tag != .windows) {
        glfw.initHint(.{ .platform = .x11 });
    } else {
        glfw.initHint(.{ .platform = .win32 });
    }

    try glfw.init();
    defer glfw.deinit();

    const app = try Application.init(gpa, "zgpu micro grid example");
    defer app.deinit();

    var demo = Demo{ .app = app };

    app.user_data = &demo;

    // Initial camera vector calculations (Z-up: world up is {0, 0, 1})
    demo.camera_right = linalg.normalize(linalg.vec3_cross(demo.camera_front, .{ 0.0, 0.0, 1.0 }));
    demo.camera_up = linalg.normalize(linalg.vec3_cross(demo.camera_right, demo.camera_front));

    _ = glfw.setCursorPosCallback(app.window, handle_mouse_move);

    // --- CREATE UNIFORM BUFFER & BIND GROUP ---
    const camera_buffer = app.gpu.createBuffer(&.{
        .label = .fromSlice("camera_buffer"),
        .usage = .{
            .uniform = true,
            .copy_dst = true,
        },
        .size = @sizeOf(CameraUniform),
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(camera_buffer);

    const camera_bind_group_layout = app.gpu.createBindGroupLayout(&.{
        .label = .fromSlice("camera_bind_group_layout"),
        .entry_count = 2,
        .entries = &.{
            .{
                .binding = 0,
                .visibility = .vertexStage,
                .buffer = .{ .type = .uniform, .has_dynamic_offset = .False, .min_binding_size = 0 },
            },
            .{
                .binding = 1,
                .visibility = .vertexStage,
                .buffer = .{ .type = .read_only_storage, .has_dynamic_offset = .False, .min_binding_size = 0 },
            },
        },
    });
    defer wgpu.bindGroupLayoutRelease(camera_bind_group_layout);

    // --- Create Render Pipeline ---
    const shader_module = try app.gpu.loadShaderText("grid.wgsl", shader_text);
    defer wgpu.shaderModuleRelease(shader_module);

    const pipeline_layout = app.gpu.createPipelineLayout(&.{
        .label = .fromSlice("pipeline_layout"),
        .bind_group_layout_count = 1,
        .bind_group_layouts = &.{camera_bind_group_layout},
    });
    defer wgpu.pipelineLayoutRelease(pipeline_layout);

    // Separate vertex buffer layouts for each attribute (Structure of Arrays)
    const vertex_buffer_layouts = [_]wgpu.VertexBufferLayout{
        // Buffer 0: positions (vec3)
        .{
            .array_stride = @sizeOf(vec3),
            .step_mode = .vertex,
            .attribute_count = 1,
            .attributes = &[_]wgpu.VertexAttribute{
                .{ .shaderLocation = 0, .offset = 0, .format = .float32x3 },
            },
        },
        // Buffer 1: normals (vec3)
        .{
            .array_stride = @sizeOf(vec3),
            .step_mode = .vertex,
            .attribute_count = 1,
            .attributes = &[_]wgpu.VertexAttribute{
                .{ .shaderLocation = 1, .offset = 0, .format = .float32x3 },
            },
        },
        // Buffer 2: uvs (vec2)
        .{
            .array_stride = @sizeOf(vec2),
            .step_mode = .vertex,
            .attribute_count = 1,
            .attributes = &[_]wgpu.VertexAttribute{
                .{ .shaderLocation = 2, .offset = 0, .format = .float32x2 },
            },
        },
        // Buffer 3: material_ids (u32)
        .{
            .array_stride = @sizeOf(u32),
            .step_mode = .vertex,
            .attribute_count = 1,
            .attributes = &[_]wgpu.VertexAttribute{
                .{ .shaderLocation = 3, .offset = 0, .format = .uint32 },
            },
        },
    };

    const color_target_state = wgpu.ColorTargetState{
        .format = app.gpu.surface_format,
        .blend = null,
        .write_mask = .all,
    };

    const fragment_state = wgpu.FragmentState{
        .module = shader_module,
        .entry_point = .fromSlice("fs_main"),
        .target_count = 1,
        .targets = &.{color_target_state},
    };

    const render_pipeline = app.gpu.createPipeline(&wgpu.RenderPipelineDescriptor{
        .label = .fromSlice("render_pipeline"),
        .layout = pipeline_layout,
        .vertex = .{
            .module = shader_module,
            .entry_point = .fromSlice("vs_main"),
            .buffer_count = vertex_buffer_layouts.len,
            .buffers = &vertex_buffer_layouts,
        },
        .primitive = .{
            .topology = .triangle_list,
            .strip_index_format = .undefined,
            .cull_mode = .back, // Cull back faces for solid objects
            .front_face = .ccw,
        },
        .depth_stencil = &wgpu.DepthStencilState{
            .format = .depth32_float,
            .depth_write_enabled = .true,
            .depth_compare = .less,
        },
        .fragment = &fragment_state,
    });
    defer wgpu.renderPipelineRelease(render_pipeline);

    // --- Create Grid and Generate Mesh ---
    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();
    // const frame_arena = arena_state.allocator();

    log.info("Starting sphere mesh test...", .{});

    // 1. Initialize the manager and its dependencies
    var pool: std.Thread.Pool = undefined;
    try pool.init(.{ .allocator = gpa });
    defer pool.deinit();

    const grid = try Grid.init(gpa);
    // Note: We don't defer grid.deinit() here because manager.deinit() returns ownership to us.

    var manager = try Grid.Manager.init(grid, &pool);

    // 2. Define the sphere materials
    const stone_mat = try manager.front().grid.registerMaterial(.{ .color = .{ .r = 128, .g = 128, .b = 128, .a = 255 }, .flags = .{ .is_opaque = true } });
    const dirt_mat = try manager.front().grid.registerMaterial(.{ .color = .{ .r = 139, .g = 90, .b = 43, .a = 255 }, .flags = .{ .is_opaque = true } });

    // 3. Define the sphere geometry.

    var commands: std.ArrayList(Grid.Command) = .empty;
    defer commands.deinit(gpa);

    for (0..4) |x| {
        for (0..4) |y| {
            for (0..4) |z| {
                try commands.append(gpa, .{
                    .set_page = .{
                        .coord = vec3i{ @as(i32, @intCast(x)) - 2, @as(i32, @intCast(y)) - 2, @as(i32, @intCast(z)) - 4 },
                        .voxel = .{ .material_id = if ((x + y + z) % 2 == 0) stone_mat else dirt_mat },
                    },
                });
            }
        }
    }

    for (0..2) |x| {
        for (0..2) |y| {
            try commands.append(gpa, .{
                .set_voxeme = .{
                    .page_coord = vec3i{ @intCast(x), @intCast(y), 0 },
                    .local_coord = vec3i{ 0, 0, 0 },
                    .voxel = .{ .material_id = if ((x + y) % 2 == 0) stone_mat else dirt_mat },
                },
            });
        }
    }

    const center_voxel = vec3i{ 128, 128, 32 };
    const radius = 32;
    const radius_sq = radius * radius;

    // 4. Queue commands to create the sphere with two materials
    const min_bound = center_voxel - @as(vec3i, @splat(radius));
    const max_bound = center_voxel + @as(vec3i, @splat(radius));
    var z = min_bound[2];
    while (z <= max_bound[2]) : (z += 1) {
        var y = min_bound[1];
        while (y <= max_bound[1]) : (y += 1) {
            var x = min_bound[0];
            while (x <= max_bound[0]) : (x += 1) {
                const current_voxel = vec3i{ x, y, z };
                const offset = current_voxel - center_voxel;
                const dist_sq = linalg.len_sq(offset);

                if (dist_sq <= radius_sq) {
                    // Use stone for bottom half (z < center), dirt for top half (z >= center)
                    const material_id = if (z < center_voxel[2]) stone_mat else dirt_mat;
                    const sphere_voxel = Grid.Voxel{ .material_id = material_id };

                    try commands.append(gpa, .{ .set_voxel = .{
                        .global_voxel = current_voxel,
                        .voxel = sphere_voxel,
                    } });
                }
            }
        }
    }

    try manager.queueCommands(commands.items);

    log.info("Queued sphere voxel {} commands.", .{commands.items.len});

    manager.endFrame();
    var time = try std.time.Timer.start();
    std.Thread.sleep(std.time.ns_per_ms);
    manager.endFrame();
    const elapsed = time.read();
    log.info("Waited {d}ms for grid update to finish", .{@as(f64, @floatFromInt(elapsed)) / @as(f64, @floatFromInt(std.time.ns_per_ms))});
    defer {
        var final_state = manager.deinit();
        final_state.deinit(); // Clean up the final grid and mesh cache
    }

    const mesh_cache = manager.front().mesh_cache;
    const front_grid = manager.front().grid;

    log.info("grid update complete", .{});

    // --- Create Buffers from Mesh Data ---
    // Calculate total buffer sizes from mesh cache
    const total_vertices = mesh_cache.vertices.len;
    const total_indices = mesh_cache.indices.items.len;

    log.info("Creating GPU buffers: {} vertices, {} indices, {} meshes", .{ total_vertices, total_indices, mesh_cache.meshes.len });

    // Create separate vertex buffers for each attribute (Structure of Arrays)
    const position_buffer = app.gpu.createBuffer(&.{
        .label = .fromSlice("position_buffer"),
        .usage = .{
            .vertex = true,
            .copy_dst = true,
        },
        .size = total_vertices * @sizeOf(vec3),
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(position_buffer);

    const normal_buffer = app.gpu.createBuffer(&.{
        .label = .fromSlice("normal_buffer"),
        .usage = .{
            .vertex = true,
            .copy_dst = true,
        },
        .size = total_vertices * @sizeOf(vec3),
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(normal_buffer);

    const uv_buffer = app.gpu.createBuffer(&.{
        .label = .fromSlice("uv_buffer"),
        .usage = .{
            .vertex = true,
            .copy_dst = true,
        },
        .size = total_vertices * @sizeOf(vec2),
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(uv_buffer);

    const material_id_buffer = app.gpu.createBuffer(&.{
        .label = .fromSlice("material_id_buffer"),
        .usage = .{
            .vertex = true,
            .copy_dst = true,
        },
        .size = total_vertices * @sizeOf(u32),
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(material_id_buffer);

    const index_buffer = app.gpu.createBuffer(&.{
        .label = .fromSlice("index_buffer"),
        .usage = .{
            .index = true,
            .copy_dst = true,
        },
        .size = total_indices * @sizeOf(u32),
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(index_buffer);

    // Upload mesh data directly from the MultiArrayList (no expensive struct packing needed!)
    if (total_vertices > 0) {
        const positions = mesh_cache.vertices.items(.position);
        const normals = mesh_cache.vertices.items(.normal);
        const uvs = mesh_cache.vertices.items(.uv);
        const material_ids = mesh_cache.vertices.items(.material_id);

        // Upload all vertex attribute arrays directly - no conversion needed!
        app.gpu.writeBuffer(position_buffer, 0, positions);
        app.gpu.writeBuffer(normal_buffer, 0, normals);
        app.gpu.writeBuffer(uv_buffer, 0, uvs);
        app.gpu.writeBuffer(material_id_buffer, 0, material_ids);
    }

    if (total_indices > 0) {
        app.gpu.writeBuffer(index_buffer, 0, mesh_cache.indices.items);
    }

    log.info("GPU buffers uploaded", .{});

    // --- CREATE MATERIAL COLORS BUFFER ---
    const material_colors = front_grid.materials.items(.color);
    const material_colors_buffer = app.gpu.createBuffer(&.{
        .label = .fromSlice("material_colors_buffer"),
        .usage = .{
            .storage = true,
            .copy_dst = true,
        },
        .size = material_colors.len * @sizeOf(Grid.Color),
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(material_colors_buffer);

    app.gpu.writeBuffer(material_colors_buffer, 0, material_colors);

    // --- CREATE BIND GROUP WITH MATERIAL COLORS ---
    const camera_bind_group = app.gpu.createBindGroup(&.{
        .label = .fromSlice("camera_bind_group"),
        .layout = camera_bind_group_layout,
        .entry_count = 2,
        .entries = &.{
            .{
                .binding = 0,
                .buffer = camera_buffer,
                .offset = 0,
                .size = @sizeOf(CameraUniform),
            },
            .{
                .binding = 1,
                .buffer = material_colors_buffer,
                .offset = 0,
                .size = material_colors.len * @sizeOf(Grid.Color),
            },
        },
    });
    defer wgpu.bindGroupRelease(camera_bind_group);

    const startup_ms = debug.start(&timer);
    log.info("startup completed in {d} ms", .{startup_ms});

    var frame_timer = try std.time.Timer.start();

    // --- Main Loop ---
    main_loop: while (!glfw.windowShouldClose(app.window)) {
        glfw.pollEvents();
        _ = arena_state.reset(.free_all);
        defer debug.lap();

        // --- Delta time calculation ---
        const ns_since_start = frame_timer.read();
        const current_frame = @as(f32, @floatFromInt(ns_since_start)) / std.time.ns_per_s;
        demo.delta_time = current_frame - demo.last_frame;
        demo.last_frame = current_frame;

        // --- Process keyboard input ---
        const camera_speed = 10.0 * demo.delta_time; // Adjust speed as needed
        if (glfw.getKey(app.window, .w) == .press) {
            demo.camera_pos = demo.camera_pos + demo.camera_front * @as(vec3, @splat(camera_speed));
        }
        if (glfw.getKey(app.window, .s) == .press) {
            demo.camera_pos = demo.camera_pos - demo.camera_front * @as(vec3, @splat(camera_speed));
        }
        if (glfw.getKey(app.window, .a) == .press) {
            demo.camera_pos = demo.camera_pos - demo.camera_right * @as(vec3, @splat(camera_speed));
        }
        if (glfw.getKey(app.window, .d) == .press) {
            demo.camera_pos = demo.camera_pos + demo.camera_right * @as(vec3, @splat(camera_speed));
        }
        if (glfw.getKey(app.window, .e) == .press) {
            demo.camera_pos = demo.camera_pos + demo.camera_up * @as(vec3, @splat(camera_speed));
        }
        if (glfw.getKey(app.window, .q) == .press) {
            demo.camera_pos = demo.camera_pos - demo.camera_up * @as(vec3, @splat(camera_speed));
        }

        // --- Calculate the view-projection matrix from fly camera state ---
        var camera_uniform: CameraUniform = undefined;
        {
            var width: i32 = 0;
            var height: i32 = 0;
            glfw.getFramebufferSize(app.window, &width, &height);
            const aspect = if (height == 0) 1.0 else @as(f32, @floatFromInt(width)) / @as(f32, @floatFromInt(height));

            const proj = linalg.mat4_perspective(linalg.deg_to_rad * 60.0, aspect, 0.1, 1000.0);

            const view = linalg.mat4_look_at(
                demo.camera_pos,
                demo.camera_pos + demo.camera_front,
                demo.camera_up,
            );

            camera_uniform.view_proj = linalg.mat4_mul(proj, view);
        }
        app.gpu.writeBuffer(camera_buffer, 0, &camera_uniform);

        {
            const frame_view = app.gpu.beginFrame() orelse continue :main_loop;
            defer app.gpu.endFrame(frame_view);

            const encoder = app.gpu.getCommandEncoder("main_encoder");

            {
                const render_pass = wgpu.commandEncoderBeginRenderPass(encoder, &wgpu.RenderPassDescriptor{
                    .label = .fromSlice("main_render_pass"),
                    .color_attachment_count = 1,
                    .color_attachments = &[_]wgpu.RenderPassColorAttachment{.{
                        .view = frame_view,
                        .resolve_target = null,
                        .load_op = .clear,
                        .store_op = .store,
                        .clear_value = wgpu.Color{ .r = 0.1, .g = 0.2, .b = 0.3, .a = 1 },
                    }},
                    .depth_stencil_attachment = &wgpu.RenderPassDepthStencilAttachment{
                        .view = app.gpu.depth_view,
                        .depth_load_op = .clear,
                        .depth_store_op = .store,
                        .depth_clear_value = 1.0,
                        .depth_read_only = .False,
                        .stencil_load_op = .undefined,
                        .stencil_store_op = .undefined,
                    },
                });
                defer wgpu.renderPassEncoderRelease(render_pass);

                // --- SET BIND GROUP & DRAW ---
                wgpu.renderPassEncoderSetPipeline(render_pass, render_pipeline);
                wgpu.renderPassEncoderSetBindGroup(render_pass, 0, camera_bind_group, 0, null);
                wgpu.renderPassEncoderSetIndexBuffer(render_pass, index_buffer, .uint32, 0, total_indices * @sizeOf(u32));

                // Set all vertex buffers (Structure of Arrays layout)
                wgpu.renderPassEncoderSetVertexBuffer(render_pass, 0, position_buffer, 0, total_vertices * @sizeOf(vec3));
                wgpu.renderPassEncoderSetVertexBuffer(render_pass, 1, normal_buffer, 0, total_vertices * @sizeOf(vec3));
                wgpu.renderPassEncoderSetVertexBuffer(render_pass, 2, uv_buffer, 0, total_vertices * @sizeOf(vec2));
                wgpu.renderPassEncoderSetVertexBuffer(render_pass, 3, material_id_buffer, 0, total_vertices * @sizeOf(u32));

                // Iterate through all mesh views and draw them
                var mesh_iter = mesh_cache.iterator();
                while (mesh_iter.next()) |view| {
                    // Draw this mesh instance
                    wgpu.renderPassEncoderDrawIndexed(render_pass, view.index_count, // indexCount
                        1, // instanceCount
                        view.index_offset, // firstIndex
                        0, // baseVertex
                        0 // firstInstance
                    );
                }

                wgpu.renderPassEncoderEnd(render_pass);
            }

            const cmd = app.gpu.finalizeCommandEncoder(encoder);

            app.gpu.submitCommands(&.{cmd});
        }
    }
}
