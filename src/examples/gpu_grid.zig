//! Test of Voxel Grid

const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");

const wgpu = @import("wgpu");
const glfw = @import("glfw");

const debug = @import("../debug.zig");
const linalg = @import("../linalg.zig");
const Application = @import("../Application.zig");
const Camera = @import("../Camera.zig");

const vec2 = linalg.vec2;
const vec3 = linalg.vec3;
const vec3i = linalg.vec3i;
const vec4 = linalg.vec4;
const mat4 = linalg.mat4;

pub const std_options = std.Options{
    .log_level = .info,
};

test {
    log.debug("semantic analysis for examples/gpu_grid.zig", .{});
    std.testing.refAllDecls(@This());
}

const Demo = struct {
    app: *Application,
    camera: Camera = .{},
};

const Color = packed struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
};

// --- Constants ---
const MAX_VERTS = 1000000; // 1 Million vertices max for this test
const VOXEL_POOL_SIZE = 10; // Space for 10 bricks (40KB)

// --- Compute Shader: The Mesher ---
const mesher_shader_text = @embedFile("shaders/GridMesher.wgsl");

// --- Render Shader: Vertex Pulling ---
const render_shader_text = @embedFile("shaders/GridRender.wgsl");

// Helper for Indirect Draw Args
const IndirectDrawArgs = extern struct {
    vertex_count: u32,
    instance_count: u32,
    first_vertex: u32,
    first_instance: u32,
};

pub fn main() !void {
    var timer = try std.time.Timer.start();

    var tsa = std.heap.ThreadSafeAllocator{
        .child_allocator = std.heap.page_allocator,
    };
    const gpa = tsa.allocator();

    const app = try Application.init(gpa, "zgpu micro grid example");
    defer app.deinit();

    var demo = Demo{ .app = app };

    app.user_data = &demo;

    // Initial camera vector calculations (Z-up: world up is {0, 0, 1})
    demo.camera.right = linalg.normalize(linalg.vec3_cross(demo.camera.front, .{ 0.0, 0.0, 1.0 }));
    demo.camera.up = linalg.normalize(linalg.vec3_cross(demo.camera.right, demo.camera.front));

    _ = glfw.setCursorPosCallback(app.window, struct {
        pub fn mouse_handler(w: *glfw.Window, xpos: f64, ypos: f64) callconv(.c) void {
            const a: *Application = @ptrCast(@alignCast(glfw.getWindowUserPointer(w)));
            const d: *Demo = @ptrCast(@alignCast(a.user_data));
            d.camera.handle_mouse_move(w, vec2{ @floatCast(xpos), @floatCast(ypos) });
        }
    }.mouse_handler);

    // ------------------------------------------------------------------------
    // 1. COMPUTE PIPELINE SETUP
    // ------------------------------------------------------------------------

    // A. The Voxel Pool (Input Data)
    // 1 Brick = 16*16*16 u32s.
    const voxel_buffer_size = 16 * 16 * 16 * @sizeOf(u32) * VOXEL_POOL_SIZE;
    const voxel_pool_buffer = app.gpu.createBuffer(&.{
        .label = .fromSlice("voxel_pool"),
        .usage = .{ .storage = true, .copy_dst = true },
        .size = voxel_buffer_size,
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(voxel_pool_buffer);

    // Fill Brick 0 with Checkerboard pattern
    {
        var initial_voxels = try gpa.alloc(u32, 16 * 16 * 16);
        defer gpa.free(initial_voxels);
        for (0..16) |z| {
            for (0..16) |y| {
                for (0..16) |x| {
                    const i = x + (y * 16) + (z * 256);
                    // Checkerboard pattern
                    if ((x + y + z) % 2 == 0) {
                        initial_voxels[i] = 1; // Material 1
                    } else {
                        initial_voxels[i] = 2; // Material 2
                    }
                }
            }
        }
        app.gpu.writeBuffer(voxel_pool_buffer, 0, initial_voxels);
    }

    // B. The Output Geometry Buffer (Vertex Pool)
    // PackedVertex is 2 * u32 = 8 bytes.
    const vertex_pool_buffer = app.gpu.createBuffer(&.{
        .label = .fromSlice("vertex_pool"),
        .usage = .{ .storage = true, .vertex = false }, // Accessed as Storage in both VS and CS
        .size = MAX_VERTS * 8,
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(vertex_pool_buffer);

    // C. The Indirect Draw Buffer
    // This holds the atomic counter for vertices
    const indirect_buffer = app.gpu.createBuffer(&.{
        .label = .fromSlice("indirect_args"),
        .usage = .{ .storage = true, .indirect = true, .copy_dst = true },
        .size = @sizeOf(IndirectDrawArgs),
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(indirect_buffer);

    // D. Compute Bind Groups
    const compute_module = try app.gpu.loadShaderText("GridMesher.wgsl", mesher_shader_text);
    defer wgpu.shaderModuleRelease(compute_module);

    const compute_bg_layout = app.gpu.createBindGroupLayout(&.{
        .entry_count = 3,
        .entries = &.{
            .{ .binding = 0, .visibility = .computeStage, .buffer = .{ .type = .storage, .min_binding_size = @sizeOf(IndirectDrawArgs) } }, // Indirect
            .{ .binding = 1, .visibility = .computeStage, .buffer = .{ .type = .storage, .min_binding_size = 0 } }, // Vertex Out
            .{ .binding = 2, .visibility = .computeStage, .buffer = .{ .type = .read_only_storage, .min_binding_size = 0 } }, // Voxel In
        },
    });

    const compute_pipeline_layout = app.gpu.createPipelineLayout(&.{
        .bind_group_layout_count = 1,
        .bind_group_layouts = &.{compute_bg_layout},
    });

    const compute_pipeline = app.gpu.createComputePipeline(&.{
        .layout = compute_pipeline_layout,
        .compute = .{
            .module = compute_module,
            .entry_point = .fromSlice("main"),
        },
    });
    defer wgpu.computePipelineRelease(compute_pipeline);

    const compute_bind_group = app.gpu.createBindGroup(&.{
        .layout = compute_bg_layout,
        .entry_count = 3,
        .entries = &.{
            .{ .binding = 0, .buffer = indirect_buffer, .size = @sizeOf(IndirectDrawArgs) },
            .{ .binding = 1, .buffer = vertex_pool_buffer, .size = MAX_VERTS * 8 },
            .{ .binding = 2, .buffer = voxel_pool_buffer, .size = voxel_buffer_size },
        },
    });

    // ------------------------------------------------------------------------
    // 2. RENDER PIPELINE SETUP
    // ------------------------------------------------------------------------

    const camera_buffer = app.gpu.createBuffer(&.{
        .usage = .{ .uniform = true, .copy_dst = true },
        .size = @sizeOf(Camera.Uniform),
    });
    defer wgpu.bufferRelease(camera_buffer);

    const render_module = try app.gpu.loadShaderText("GridRender.wgsl", render_shader_text);

    const render_bg_layout = app.gpu.createBindGroupLayout(&.{
        .entry_count = 2,
        .entries = &.{
            .{ .binding = 0, .visibility = .vertexStage, .buffer = .{ .type = .uniform } },
            // Note: Vertex Pool is ReadOnly Storage here
            .{ .binding = 1, .visibility = .vertexStage, .buffer = .{ .type = .read_only_storage } },
        },
    });

    const render_pipeline_layout = app.gpu.createPipelineLayout(&.{
        .bind_group_layout_count = 1,
        .bind_group_layouts = &.{render_bg_layout},
    });

    const render_pipeline = app.gpu.createPipeline(&wgpu.RenderPipelineDescriptor{
        .layout = render_pipeline_layout,
        .vertex = .{
            .module = render_module,
            .entry_point = .fromSlice("vs_main"),
            .buffer_count = 0, // Vertex Pulling! No standard attributes.
            .buffers = null,
        },
        .primitive = .{ .topology = .triangle_list, .cull_mode = .back },
        .depth_stencil = &wgpu.DepthStencilState{
            .format = .depth32_float,
            .depth_write_enabled = .true,
            .depth_compare = .less,
        },
        .fragment = &.{
            .module = render_module,
            .entry_point = .fromSlice("fs_main"),
            .target_count = 1,
            .targets = &.{.{ .format = app.gpu.surface_format, .write_mask = .all }},
        },
    });

    const render_bind_group = app.gpu.createBindGroup(&.{
        .layout = render_bg_layout,
        .entry_count = 2,
        .entries = &.{
            .{ .binding = 0, .buffer = camera_buffer, .size = @sizeOf(Camera.Uniform) },
            .{ .binding = 1, .buffer = vertex_pool_buffer, .size = MAX_VERTS * 8 },
        },
    });

    const startup_ms = debug.start(&timer);
    log.info("startup completed in {d} ms", .{startup_ms});

    var frame_timer = try std.time.Timer.start();

    // --- Main Loop ---
    main_loop: while (!glfw.windowShouldClose(app.window)) {
        glfw.pollEvents();
        // _ = arena_state.reset(.free_all);
        defer debug.lap();

        // --- Delta time calculation ---
        const ns_since_last = frame_timer.lap();
        const delta_time = @as(f32, @floatFromInt(ns_since_last)) / std.time.ns_per_s;

        // --- Process keyboard input ---
        demo.camera.update(app.window, delta_time);

        {
            const frame_view = app.gpu.beginFrame() orelse continue :main_loop;
            defer app.gpu.endFrame(frame_view);

            const encoder = app.gpu.getCommandEncoder("main_encoder");

            // --- Update camera uniform ---
            app.gpu.writeBuffer(camera_buffer, 0, &demo.camera.calculateUniform(app.window));

            // --- STEP A: RESET INDIRECT BUFFER ---
            // We must reset the vertex_count to 0 whenever we remesh.
            // For this test, we remesh every frame to prove it's fast.
            const reset_data = IndirectDrawArgs{ .vertex_count = 0, .instance_count = 1, .first_vertex = 0, .first_instance = 0 };
            app.gpu.writeBuffer(indirect_buffer, 0, &reset_data); // Note: In prod, use copyBufferToBuffer or clearBuffer (Not sure how this is supposed to work if we need to set instance count)

            // --- STEP B: COMPUTE PASS (MESHING) ---
            {
                const cpass = wgpu.commandEncoderBeginComputePass(encoder, &.{ .label = .fromSlice("mesher") });
                wgpu.computePassEncoderSetPipeline(cpass, compute_pipeline);
                wgpu.computePassEncoderSetBindGroup(cpass, 0, compute_bind_group, 0, null);
                // Dispatch 4x4x4 workgroups. Since workgroup_size is 4x4x4 (64 threads),
                // this covers 16x16x16 voxels exactly (1 Brick).
                wgpu.computePassEncoderDispatchWorkgroups(cpass, 4, 4, 4);
                wgpu.computePassEncoderEnd(cpass);
                wgpu.computePassEncoderRelease(cpass);
            }

            // --- STEP C: RENDER PASS ---
            {
                const rpass = wgpu.commandEncoderBeginRenderPass(encoder, &.{
                    .color_attachment_count = 1,
                    .color_attachments = &[_]wgpu.RenderPassColorAttachment{.{ .view = frame_view, .load_op = .clear, .store_op = .store, .clear_value = .{ .r = 0.1, .g = 0.1, .b = 0.1, .a = 1.0 } }},
                    .depth_stencil_attachment = &.{ .view = app.gpu.depth_view, .depth_load_op = .clear, .depth_store_op = .store, .depth_clear_value = 1.0 },
                });

                wgpu.renderPassEncoderSetPipeline(rpass, render_pipeline);
                wgpu.renderPassEncoderSetBindGroup(rpass, 0, render_bind_group, 0, null);

                // THE MAGIC: Draw Indirect using the buffer filled by Compute
                wgpu.renderPassEncoderDrawIndirect(rpass, indirect_buffer, 0);

                wgpu.renderPassEncoderEnd(rpass);
                wgpu.renderPassEncoderRelease(rpass);
            }

            const cmd = app.gpu.finalizeCommandEncoder(encoder);

            app.gpu.submitCommands(&.{cmd});
        }
    }
}
