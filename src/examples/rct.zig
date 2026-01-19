//! Terrain Test (RCT Style)

const std = @import("std");
const log = std.log.scoped(.main);
const wgpu = @import("wgpu");
const glfw = @import("glfw");
const debug = @import("../debug.zig");
const linalg = @import("../linalg.zig");
const Application = @import("../Application.zig");
const Camera = @import("../Camera.zig");

const vec2 = linalg.vec2;
const vec2d = linalg.vec2d;
const vec2i = linalg.vec2i;
const vec2u = linalg.vec2u;
const vec3 = linalg.vec3;
const vec3i = linalg.vec3i;
const vec4 = linalg.vec4;
const mat4 = linalg.mat4;

pub const std_options = std.Options{
    .log_level = .info,
};

test {
    log.debug("semantic analysis for examples/rct.zig", .{});
    std.testing.refAllDecls(@This());
}

// --- Constants ---
const MAP_SIZE = 64; // 64x64 map
const MAX_VERTS = MAP_SIZE * MAP_SIZE * 10; // Approx buffer for walls + tops

const mesher_shader_text = @embedFile("shaders/TerrainMesher.wgsl");
const render_shader_text = @embedFile("shaders/TerrainRender.wgsl");

const IndirectDrawArgs = extern struct {
    vertex_count: u32,
    instance_count: u32,
    first_vertex: u32,
    first_instance: u32,
};

const Demo = struct {
    app: *Application,
    camera: Camera,
    map_dirty: bool = true, // Start dirty to generate initial mesh

    // CPU side copy of map to modify
    map_data: []Cell,
    cursor_pos_world: vec2u = .{ 0xFFFFFFFF, 0xFFFFFFFF }, // OOB by default

    drag_active: bool = false,
    drag_target: vec2u = .{ 0, 0 },
    drag_start_y: f64 = 0.0,
    drag_start_val: Cell = .{},
};

const CameraUniform = extern struct {
    view_proj: mat4,
    cursor_pos_world: vec2u,
};

const Corner = enum(u2) {
    BottomLeft = 0,
    BottomRight = 1,
    TopRight = 2,
    TopLeft = 3,
};

const Cell = extern struct {
    corners: [4]u8 align(4) = [1]u8{0} ** 4,

    pub fn getCorner(self: Cell, corner: Corner) u8 {
        return self.corners[@intFromEnum(corner)];
    }

    pub fn setCorner(self: *Cell, corner: Corner, value: u8) void {
        self.corners[@intFromEnum(corner)] = value;
    }

    pub fn eql(self: Cell, other: Cell) bool {
        return std.mem.eql(u8, &self.corners, &other.corners);
    }
};

fn gridIndex(i: usize, j: usize) usize {
    return i + j * MAP_SIZE;
}

const MAP_HEIGHT: f32 = @floatFromInt(std.math.maxInt(u8));

fn wave(i: f32, j: f32) u8 {
    const f = @abs(std.math.cos((i + 1) / 4) * std.math.sin((j + 5) / 4));

    return @intFromFloat(@floor(f * 15));
}

pub fn main() !void {
    var tsa = std.heap.ThreadSafeAllocator{ .child_allocator = std.heap.page_allocator };
    const gpa = tsa.allocator();

    const app = try Application.init(gpa, "RCT-Style Terrain");
    defer app.deinit();

    // Initialize Map Data (CPU)
    // Format: 0xAABBCCDD -> AA=Corner3, BB=Corner2, CC=Corner1, DD=Corner0
    var map_data = try gpa.alloc(Cell, MAP_SIZE * MAP_SIZE);
    defer gpa.free(map_data);

    // Init with some interesting terrain (sine wave hills)
    for (0..MAP_SIZE) |i| {
        for (0..MAP_SIZE) |j| {
            const fx: f32 = @floatFromInt(i);
            const fy: f32 = @floatFromInt(j);

            map_data[gridIndex(i, j)].setCorner(.TopLeft, wave(fx + 0.0, fy + 0.0));
            map_data[gridIndex(i, j)].setCorner(.TopRight, wave(fx + 1.0, fy + 0.0));
            map_data[gridIndex(i, j)].setCorner(.BottomLeft, wave(fx + 0.0, fy + 1.0));
            map_data[gridIndex(i, j)].setCorner(.BottomRight, wave(fx + 1.0, fy + 1.0));
        }
    }

    var demo = Demo{
        .app = app,
        .camera = Camera.fromLookAt(.{ -10.0, 30.0, 20.0 }, .{ 32.0, 0.0, 32.0 }, .{ 0.0, 1.0, 0.0 }), // Adjusted for isometric-ish view
        .map_data = map_data,
    };
    app.user_data = &demo;

    // --- Input Handling ---
    _ = glfw.setCursorPosCallback(app.window, struct {
        pub fn mouse(w: *glfw.Window, x: f64, y: f64) callconv(.c) void {
            const a: *Application = @ptrCast(@alignCast(glfw.getWindowUserPointer(w)));
            const d: *Demo = @ptrCast(@alignCast(a.user_data));
            d.camera.handle_mouse_move(w, vec2{ @floatCast(x), @floatCast(y) });
        }
    }.mouse);

    // --- GPU Resources ---

    // 1. Terrain Map Buffer (Storage, CopyDst)
    const map_buffer = app.gpu.createBuffer(&.{
        .label = .fromSlice("terrain_map"),
        .usage = .{ .storage = true, .copy_dst = true },
        .size = @sizeOf(u32) * MAP_SIZE * MAP_SIZE,
        .mapped_at_creation = .False,
    });
    defer wgpu.bufferRelease(map_buffer);

    // 2. Vertex Pool (Output)
    const vertex_pool = app.gpu.createBuffer(&.{
        .label = .fromSlice("vertex_pool"),
        .usage = .{ .storage = true, .vertex = false },
        .size = MAX_VERTS * 8, // 8 bytes per PackedVertex
    });
    defer wgpu.bufferRelease(vertex_pool);

    // 3. Indirect Args
    const indirect_buffer = app.gpu.createBuffer(&.{
        .usage = .{ .storage = true, .indirect = true, .copy_dst = true },
        .size = @sizeOf(IndirectDrawArgs),
    });
    defer wgpu.bufferRelease(indirect_buffer);

    // 4. Uniforms
    const camera_buffer = app.gpu.createBuffer(&.{
        .usage = .{ .uniform = true, .copy_dst = true },
        .size = @sizeOf(CameraUniform),
    });
    defer wgpu.bufferRelease(camera_buffer);

    // --- Pipelines ---

    // Compute (Mesher)
    const cs_mod = try app.gpu.loadShaderText("TerrainMesher.wgsl", mesher_shader_text);
    defer wgpu.shaderModuleRelease(cs_mod);

    const cs_layout = app.gpu.createBindGroupLayout(&.{
        .entry_count = 3,
        .entries = &.{
            .{ .binding = 0, .visibility = .computeStage, .buffer = .{ .type = .storage } }, // Indirect
            .{ .binding = 1, .visibility = .computeStage, .buffer = .{ .type = .storage } }, // Verts
            .{ .binding = 2, .visibility = .computeStage, .buffer = .{ .type = .read_only_storage } }, // Map
        },
    });
    const cs_pipe_layout = app.gpu.createPipelineLayout(&.{ .bind_group_layout_count = 1, .bind_group_layouts = &.{cs_layout} });
    const cs_pipeline = app.gpu.createComputePipeline(&.{
        .layout = cs_pipe_layout,
        .compute = .{ .module = cs_mod, .entry_point = .fromSlice("main") },
    });
    const cs_bind = app.gpu.createBindGroup(&.{
        .layout = cs_layout,
        .entry_count = 3,
        .entries = &.{
            .{ .binding = 0, .buffer = indirect_buffer, .size = @sizeOf(IndirectDrawArgs) },
            .{ .binding = 1, .buffer = vertex_pool, .size = MAX_VERTS * 8 },
            .{ .binding = 2, .buffer = map_buffer, .size = @sizeOf(u32) * MAP_SIZE * MAP_SIZE },
        },
    });

    // Render
    const vs_mod = try app.gpu.loadShaderText("TerrainRender.wgsl", render_shader_text);
    const rs_layout = app.gpu.createBindGroupLayout(&.{
        .entry_count = 2,
        .entries = &.{
            .{ .binding = 0, .visibility = .vertexStage, .buffer = .{ .type = .uniform } },
            .{ .binding = 1, .visibility = .vertexStage, .buffer = .{ .type = .read_only_storage } },
        },
    });
    const rs_pipe_layout = app.gpu.createPipelineLayout(&.{ .bind_group_layout_count = 1, .bind_group_layouts = &.{rs_layout} });
    const rs_pipeline = app.gpu.createPipeline(&wgpu.RenderPipelineDescriptor{
        .layout = rs_pipe_layout,
        .vertex = .{ .module = vs_mod, .entry_point = .fromSlice("vs_main") },
        .primitive = .{ .topology = .triangle_list, .cull_mode = .back },
        .depth_stencil = &wgpu.DepthStencilState{ .format = .depth32_float, .depth_write_enabled = .true, .depth_compare = .less },
        .fragment = &.{ .module = vs_mod, .entry_point = .fromSlice("fs_main"), .target_count = 1, .targets = &.{.{ .format = app.gpu.surface_format }} },
    });
    const rs_bind = app.gpu.createBindGroup(&.{
        .layout = rs_layout,
        .entry_count = 2,
        .entries = &.{
            .{ .binding = 0, .buffer = camera_buffer, .size = @sizeOf(CameraUniform) },
            .{ .binding = 1, .buffer = vertex_pool, .size = MAX_VERTS * 8 },
        },
    });

    // --- Loop ---
    var frame_timer = try std.time.Timer.start();
    while (!glfw.windowShouldClose(app.window)) {
        glfw.pollEvents();
        const dt = @as(f32, @floatFromInt(frame_timer.lap())) / std.time.ns_per_s;
        demo.camera.update(app.window, dt);

        const frame = app.gpu.beginFrame() orelse continue;
        defer app.gpu.endFrame(frame);
        const encoder = app.gpu.getCommandEncoder("main");

        demo.camera.calculateViewProj(demo.app.window);

        // Update Camera
        app.gpu.writeBuffer(camera_buffer, 0, &CameraUniform{
            .view_proj = demo.camera.view_proj,
            .cursor_pos_world = demo.cursor_pos_world,
        });

        var mouse_pos = vec2d{ 0.0, 0.0 };
        var window_size = vec2i{ 0, 0 };
        glfw.getCursorPos(app.window, &mouse_pos[0], &mouse_pos[1]);
        glfw.getWindowSize(app.window, &window_size[0], &window_size[1]);
        // Determine hovered tile (only relevant if not currently dragging)
        if (!demo.drag_active) {
            if (get_hovered_tile(&demo, @floatCast(mouse_pos[0]), @floatCast(mouse_pos[1]), @floatFromInt(window_size[0]), @floatFromInt(window_size[1]))) |tile| {
                demo.cursor_pos_world = tile;
            } else {
                demo.cursor_pos_world = .{ 0xFFFFFFFF, 0xFFFFFFFF };
            }
        }

        const left_mouse = glfw.getMouseButton(app.window, .left);

        if (demo.drag_active) {
            // --- STATE: DRAGGING ---
            if (left_mouse == .release) {
                demo.drag_active = false;
            } else {
                // Calculate Delta
                // Screen Y is positive down. Dragging UP (Negative Y) should increase height.
                // Factor 0.25 means 4 pixels of mouse movement = 1 unit of height
                const sensitivity = 0.05;
                const delta_y = demo.drag_start_y - mouse_pos[1];
                const height_diff: i32 = @intFromFloat(delta_y * sensitivity);

                // Apply diff to the original starting value
                var new_packed: Cell = .{};

                // Unpack, add diff, clamp, repack for all 4 corners
                inline for (comptime std.meta.fieldNames(Corner)) |corner_name| {
                    const corner = comptime @field(Corner, corner_name);
                    const base_h: i32 = @intCast(demo.drag_start_val.getCorner(corner));

                    // Add delta and clamp to u8 range
                    const new_h = std.math.clamp(base_h + height_diff, 0, std.math.maxInt(u8));

                    // Repack
                    new_packed.setCorner(corner, @intCast(new_h));
                }

                // Update Map Data
                const tile_idx = demo.drag_target[0] + demo.drag_target[1] * MAP_SIZE;

                // Optimization: Only mark dirty if value actually changed
                if (!demo.map_data[tile_idx].eql(new_packed)) {
                    demo.map_data[tile_idx] = new_packed;
                    demo.map_dirty = true;
                }
            }
        } else {
            // --- STATE: IDLE (Check for click) ---
            if (left_mouse == .press and demo.cursor_pos_world[0] != 0xFFFFFFFF) {
                demo.drag_active = true;
                demo.drag_target = demo.cursor_pos_world;
                demo.drag_start_y = mouse_pos[1];

                const tile_idx = demo.drag_target[0] + demo.drag_target[1] * MAP_SIZE;
                demo.drag_start_val = demo.map_data[tile_idx];
            }
        }

        // --- CONDITIONALLY RUN MESHING ---
        if (demo.map_dirty) {
            // 1. Upload new map data
            app.gpu.writeBuffer(map_buffer, 0, demo.map_data);

            // 2. Reset Indirect Counter
            const reset_args = IndirectDrawArgs{ .vertex_count = 0, .instance_count = 1, .first_vertex = 0, .first_instance = 0 };
            app.gpu.writeBuffer(indirect_buffer, 0, &reset_args);

            // 3. Dispatch Compute
            // Ensure we wait for the buffer write before computing (WebGPU handles this implicitly usually, but barriers in Vulkan matters)
            const cpass = wgpu.commandEncoderBeginComputePass(encoder, &.{ .label = .fromSlice("mesher") });
            wgpu.computePassEncoderSetPipeline(cpass, cs_pipeline);
            wgpu.computePassEncoderSetBindGroup(cpass, 0, cs_bind, 0, null);
            // Dispatch 8x8 threads per group -> 64x64 map = 8x8 groups
            wgpu.computePassEncoderDispatchWorkgroups(cpass, MAP_SIZE / 8, MAP_SIZE / 8, 1);
            wgpu.computePassEncoderEnd(cpass);
            wgpu.computePassEncoderRelease(cpass);

            demo.map_dirty = false;
        }

        // --- RENDER PASS (Always runs) ---
        {
            const rpass = wgpu.commandEncoderBeginRenderPass(encoder, &.{
                .color_attachment_count = 1,
                .color_attachments = &[_]wgpu.RenderPassColorAttachment{.{
                    .view = frame,
                    .load_op = .clear,
                    .store_op = .store,
                    .clear_value = .{ .r = 0.2, .g = 0.5, .b = 0.8, .a = 1.0 }, // Nice sky blue
                }},
                .depth_stencil_attachment = &.{ .view = app.gpu.depth_view, .depth_load_op = .clear, .depth_store_op = .store, .depth_clear_value = 1.0 },
            });
            wgpu.renderPassEncoderSetPipeline(rpass, rs_pipeline);
            wgpu.renderPassEncoderSetBindGroup(rpass, 0, rs_bind, 0, null);
            wgpu.renderPassEncoderDrawIndirect(rpass, indirect_buffer, 0);
            wgpu.renderPassEncoderEnd(rpass);
            wgpu.renderPassEncoderRelease(rpass);
        }

        const cmd = app.gpu.finalizeCommandEncoder(encoder);
        app.gpu.submitCommands(&.{cmd});
    }
}

fn get_heights(map: []Cell, x: usize, y: usize) [4]f32 {
    if (x >= 64 or y >= 64) return .{ 0, 0, 0, 0 };
    const raw = map[x + y * 64];
    return .{
        @floatFromInt(raw.getCorner(.BottomLeft)),
        @floatFromInt(raw.getCorner(.BottomRight)),
        @floatFromInt(raw.getCorner(.TopRight)),
        @floatFromInt(raw.getCorner(.TopLeft)),
    };
}

// Möller-Trumbore intersection
fn intersect_triangle(ro: vec3, rd: vec3, v0: vec3, v1: vec3, v2: vec3) ?f32 {
    const EPSILON = 0.0000001;
    const edge1 = v1 - v0;
    const edge2 = v2 - v0;
    const h = linalg.vec3_cross(rd, edge2);
    const a = linalg.dot(edge1, h);

    if (a > -EPSILON and a < EPSILON) return null; // Ray parallel to triangle

    const f = 1.0 / a;
    const s = ro - v0;
    const u = f * linalg.dot(s, h);
    if (u < 0.0 or u > 1.0) return null;

    const q = linalg.vec3_cross(s, edge1);
    const v = f * linalg.dot(rd, q);
    if (v < 0.0 or u + v > 1.0) return null;

    const t = f * linalg.dot(edge2, q);
    if (t > EPSILON) return t;

    return null;
}

fn get_hovered_tile(demo: *Demo, mouse_x: f32, mouse_y: f32, screen_w: f32, screen_h: f32) ?vec2u {
    // 1. Calculate Normalized Device Coordinates (-1 to 1)
    const ndc_x = (2.0 * mouse_x) / screen_w - 1.0;
    const ndc_y = 1.0 - (2.0 * mouse_y) / screen_h;

    // 2. Unproject to create Ray
    const inv_vp = linalg.mat4_inverse(demo.camera.view_proj) orelse {
        log.err("Failed to invert view-proj matrix", .{});
        return null;
    };

    // Near plane point
    var p_near = linalg.mat4_apply(inv_vp, vec4{ ndc_x, ndc_y, 0.0, 1.0 });
    p_near = p_near / @as(vec4, @splat(p_near[3]));

    // Far plane point
    var p_far = linalg.mat4_apply(inv_vp, vec4{ ndc_x, ndc_y, 1.0, 1.0 });
    p_far = p_far / @as(vec4, @splat(p_far[3]));

    const ray_origin = vec3{ p_near[0], p_near[1], p_near[2] };
    const ray_dir = linalg.normalize(vec3{ p_far[0] - p_near[0], p_far[1] - p_near[1], p_far[2] - p_near[2] });

    // 3. Iterate all tiles
    var min_dist: f32 = std.math.floatMax(f32);
    var hit_tile: ?vec2u = null;

    // Helper to reduce code duplication for the 4 walls + top
    const CheckInfo = struct {
        dist: *f32,
        hit: *?vec2u,
        x: usize,
        y: usize,
        ro: vec3,
        rd: vec3,

        fn check_quad(self: *const @This(), v0: vec3, v1: vec3, v2: vec3, v3: vec3) void {
            // Check Tri 1 (v0, v1, v2)
            if (intersect_triangle(self.ro, self.rd, v0, v1, v2)) |d| {
                if (d < self.dist.*) {
                    self.dist.* = d;
                    self.hit.* = .{ @intCast(self.x), @intCast(self.y) };
                }
            }
            // Check Tri 2 (v0, v2, v3)
            if (intersect_triangle(self.ro, self.rd, v0, v2, v3)) |d| {
                if (d < self.dist.*) {
                    self.dist.* = d;
                    self.hit.* = .{ @intCast(self.x), @intCast(self.y) };
                }
            }
        }
    };

    for (0..64) |y| {
        for (0..64) |x| {
            const h = get_heights(demo.map_data, x, y);

            // Get neighbors for wall checks (using wrapping math which get_heights handles as OOB 0)
            const h_south = get_heights(demo.map_data, x, y -% 1);
            const h_north = get_heights(demo.map_data, x, y +% 1);
            const h_west = get_heights(demo.map_data, x -% 1, y);
            const h_east = get_heights(demo.map_data, x +% 1, y);

            const fx = @as(f32, @floatFromInt(x));
            const fy = @as(f32, @floatFromInt(y));

            // Tile Corners (Top Surface)
            // H Indices: 0=BL, 1=BR, 2=TR, 3=TL
            const c_bl = vec3{ fx, h[0], fy };
            const c_br = vec3{ fx + 1.0, h[1], fy };
            const c_tr = vec3{ fx + 1.0, h[2], fy + 1.0 };
            const c_tl = vec3{ fx, h[3], fy + 1.0 };

            const checker = CheckInfo{ .dist = &min_dist, .hit = &hit_tile, .x = x, .y = y, .ro = ray_origin, .rd = ray_dir };

            // 1. Top Surface
            checker.check_quad(c_bl, c_br, c_tr, c_tl);

            // 2. Vertical Walls
            // Only generate a wall if the current edge is higher than the neighbor's edge.
            // This prevents selecting a tile by clicking a wall that is technically "underground".

            // South Wall (y) - Shared with Neighbor(y-1) North Edge
            // Current BL/BR vs Neighbor TL/TR
            if (h[0] > h_south[3] or h[1] > h_south[2]) {
                const w_bl = vec3{ fx, h_south[3], fy }; // Neighbor TL
                const w_br = vec3{ fx + 1.0, h_south[2], fy }; // Neighbor TR
                checker.check_quad(w_bl, w_br, c_br, c_bl); // Wall from neighbor up to current
            }

            // North Wall (y+1) - Shared with Neighbor(y+1) South Edge
            // Current TL/TR vs Neighbor BL/BR
            if (h[3] > h_north[0] or h[2] > h_north[1]) {
                const w_bl = vec3{ fx, h_north[0], fy + 1.0 }; // Neighbor BL
                const w_br = vec3{ fx + 1.0, h_north[1], fy + 1.0 }; // Neighbor BR
                checker.check_quad(c_tl, c_tr, w_br, w_bl);
            }

            // West Wall (x) - Shared with Neighbor(x-1) East Edge
            // Current BL/TL vs Neighbor BR/TR
            if (h[0] > h_west[1] or h[3] > h_west[2]) {
                const w_bl = vec3{ fx, h_west[1], fy }; // Neighbor BR
                const w_tl = vec3{ fx, h_west[2], fy + 1.0 }; // Neighbor TR
                checker.check_quad(w_bl, c_bl, c_tl, w_tl);
            }

            // East Wall (x+1) - Shared with Neighbor(x+1) West Edge
            // Current BR/TR vs Neighbor BL/TL
            if (h[1] > h_east[0] or h[2] > h_east[3]) {
                const w_br = vec3{ fx + 1.0, h_east[0], fy }; // Neighbor BL
                const w_tr = vec3{ fx + 1.0, h_east[3], fy + 1.0 }; // Neighbor TL
                checker.check_quad(c_br, w_br, w_tr, c_tr);
            }
        }
    }
    return hit_tile;
}
