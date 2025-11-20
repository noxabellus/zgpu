//! Test of Voxel Grid

const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");

const wgpu = @import("wgpu");
const glfw = @import("glfw");

const debug = @import("../debug.zig");
const linalg = @import("../linalg.zig");
const Application = @import("../Application.zig");

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

const Color = packed struct {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
};

// A struct for our camera's view-projection matrix uniform.
const CameraUniform = extern struct {
    view_proj: mat4,
};

// --- Constants ---
const MAX_VERTS = 1000000; // 1 Million vertices max for this test
const VOXEL_POOL_SIZE = 10; // Space for 10 bricks (40KB)

// --- Compute Shader: The Mesher ---
const compute_shader_text =
    \\struct IndirectArgs {
    \\    vertex_count: atomic<u32>,
    \\    instance_count: u32,
    \\    first_vertex: u32,
    \\    first_instance: u32,
    \\};
    \\
    \\struct PackedVertex {
    \\    // x,y,z (8 bits each) + normal_id (3 bits) + unused
    \\    pos_norm: u32, 
    \\    // material_id (16 bits) + ao (something else)
    \\    data: u32, 
    \\};
    \\
    \\// Bindings
    \\@group(0) @binding(0) var<storage, read_write> indirect_draw: IndirectArgs;
    \\@group(0) @binding(1) var<storage, read_write> vertex_pool: array<PackedVertex>;
    \\@group(0) @binding(2) var<storage, read> voxel_pool: array<u32>; // The raw voxel data
    \\
    \\// Helper to pack data
    \\fn pack_vertex(x: u32, y: u32, z: u32, norm: u32) -> u32 {
    \\    return (x) | (y << 8u) | (z << 16u) | (norm << 24u);
    \\}
    \\
    \\@compute @workgroup_size(4, 4, 4) // 64 threads per group
    \\fn main(@builtin(global_invocation_id) id: vec3<u32>) {
    \\    // Hardcoded for this demo: We are meshing Brick #0 at World Pos (0,0,0)
    \\    // In a real engine, we'd look up the BrickMap here.
    \\    
    \\    if (id.x >= 16u || id.y >= 16u || id.z >= 16u) { return; }
    \\
    \\    // Flatten index for 16^3 brick
    \\    let voxel_index = id.x + (id.y * 16u) + (id.z * 256u);
    \\    let voxel_data = voxel_pool[voxel_index];
    \\
    \\    // 0 is empty
    \\    if (voxel_data == 0u) { return; }
    \\
    \\    // SIMPLE FACE CULLING (Naive)
    \\    // Real implementation: Check neighbor voxels in pool. 
    \\    // For this demo, we just emit a cube for every solid voxel to prove the pipeline.
    \\    
    \\    // Allocate space for 36 vertices (6 faces * 2 tris * 3 verts)
    \\    // In atomic operations, we reserve space in the global buffer
    \\    let start_idx = atomicAdd(&indirect_draw.vertex_count, 36u);
    \\
    \\    // Geometry Generation (emit a standard cube scaled to voxel size)
    \\    // We are generating triangle lists.
    \\    // NOTE: Standard Rasterization requires explicit triangles.
    \\    
    \\    // Define relative coords (0..15)
    \\    let x = id.x; let y = id.y; let z = id.z;
    \\    let mat = voxel_data;
    \\
    \\    // Construct a few faces (simplified for brevity - just showing Z+ face logic)
    \\    // A real mesher loops 6 faces. Here is a "Blocky Point" representation for testing
    \\    // Actually, let's emit one full quad for +Z face to demonstrate.
    \\    
    \\    // Z+ Face
    \\    let v1 = pack_vertex(x, y, z+1u, 4u);      // 0,0,1
    \\    let v2 = pack_vertex(x+1u, y, z+1u, 4u);   // 1,0,1
    \\    let v3 = pack_vertex(x+1u, y+1u, z+1u, 4u);// 1,1,1
    \\    let v4 = pack_vertex(x, y+1u, z+1u, 4u);   // 0,1,1
    \\    
    \\    // Write 6 vertices (2 triangles)
    \\    // Tri 1
    \\    vertex_pool[start_idx + 0u] = PackedVertex(v1, mat);
    \\    vertex_pool[start_idx + 1u] = PackedVertex(v2, mat);
    \\    vertex_pool[start_idx + 2u] = PackedVertex(v3, mat);
    \\    // Tri 2
    \\    vertex_pool[start_idx + 3u] = PackedVertex(v1, mat);
    \\    vertex_pool[start_idx + 4u] = PackedVertex(v3, mat);
    \\    vertex_pool[start_idx + 5u] = PackedVertex(v4, mat);
    \\    
    \\    // Note: You would repeat this for -Z, +X, -X, +Y, -Y checking neighbors.
    \\}
;

// --- Render Shader: Vertex Pulling ---
const render_shader_text =
    \\struct CameraUniform {
    \\    view_proj: mat4x4<f32>,
    \\};
    \\@group(0) @binding(0) var<uniform> u_camera: CameraUniform;
    \\
    \\struct PackedVertex {
    \\    pos_norm: u32,
    \\    data: u32,
    \\};
    \\
    \\// We access the buffer generated by Compute Shader as ReadOnly here
    \\@group(0) @binding(1) var<storage, read> vertex_pool: array<PackedVertex>;
    \\
    \\struct VertexOutput {
    \\    @builtin(position) clip_position: vec4<f32>,
    \\    @location(0) norm: vec3<f32>,
    \\    @location(1) color: vec3<f32>,
    \\};
    \\
    \\@vertex
    \\fn vs_main(@builtin(vertex_index) v_idx: u32) -> VertexOutput {
    \\    let packed = vertex_pool[v_idx];
    \\    
    \\    // Unpack Position (8 bits per axis)
    \\    let lx = f32((packed.pos_norm >> 0u) & 0xFFu);
    \\    let ly = f32((packed.pos_norm >> 8u) & 0xFFu);
    \\    let lz = f32((packed.pos_norm >> 16u) & 0xFFu);
    \\    let norm_id = (packed.pos_norm >> 24u) & 0x7u;
    \\    
    \\    // Constraints: 6.25cm voxel size = 0.0625 meters
    \\    let scale = 0.0625;
    \\    let world_pos = vec3<f32>(lx, ly, lz) * scale;
    \\    
    \\    // Fake normals based on ID (simplified)
    \\    var normal = vec3<f32>(0.0, 0.0, 1.0);
    \\    
    \\    // Material Coloring
    \\    let mat_id = packed.data; // simplified
    \\    var color = vec3<f32>(0.8, 0.8, 0.8);
    \\    if (mat_id == 1u) { color = vec3<f32>(1.0, 0.2, 0.2); } // Red
    \\    if (mat_id == 2u) { color = vec3<f32>(0.2, 1.0, 0.2); } // Green
    \\    
    \\    var out: VertexOutput;
    \\    out.clip_position = u_camera.view_proj * vec4<f32>(world_pos, 1.0);
    \\    out.norm = normal;
    \\    out.color = color;
    \\    return out;
    \\}
    \\
    \\@fragment
    \\fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    \\    // Simple lighting
    \\    return vec4<f32>(in.color, 1.0);
    \\}
;

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
    demo.camera_right = linalg.normalize(linalg.vec3_cross(demo.camera_front, .{ 0.0, 0.0, 1.0 }));
    demo.camera_up = linalg.normalize(linalg.vec3_cross(demo.camera_right, demo.camera_front));

    _ = glfw.setCursorPosCallback(app.window, handle_mouse_move);

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
    const compute_module = try app.gpu.loadShaderText("mesher.wgsl", compute_shader_text);
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
        .size = @sizeOf(CameraUniform),
    });
    defer wgpu.bufferRelease(camera_buffer);

    const render_module = try app.gpu.loadShaderText("render.wgsl", render_shader_text);

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
            .{ .binding = 0, .buffer = camera_buffer, .size = @sizeOf(CameraUniform) },
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

            // --- STEP A: RESET INDIRECT BUFFER ---
            // We must reset the vertex_count to 0 every frame (or whenever we remesh).
            // For this test, we remesh every frame to prove it's fast.
            const reset_data = IndirectDrawArgs{ .vertex_count = 0, .instance_count = 1, .first_vertex = 0, .first_instance = 0 };
            app.gpu.writeBuffer(indirect_buffer, 0, &reset_data); // Note: In prod, use copyBufferToBuffer or clearBuffer

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
