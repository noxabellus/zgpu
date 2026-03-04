const Camera = @This();

const std = @import("std");

const Gpu = @import("Gpu.zig");
const InputState = @import("InputState.zig");
const linalg = @import("linalg.zig");
const vec2 = linalg.vec2;
const vec3 = linalg.vec3;
const mat4 = linalg.mat4;

pos: vec3,
front: vec3,
up: vec3,
right: vec3,
world_up: vec3,
world_forward: vec3,

yaw: f32 = 0.0,
pitch: f32 = 0.0,
roll: f32 = 0.0,

first_mouse: bool = true,
last_mouse: vec2 = .{ 0.0, 0.0 },
sensitivity: f32 = 0.1,
speed: f32 = 10.0,
roll_speed: f32 = 60.0,

view_proj: mat4 = linalg.mat4_identity,

buffer: ?*Gpu.Buffer = null,
bind_group: ?*Gpu.BindGroup = null,

pub const Uniform = mat4;

var BIND_GROUP_LAYOUT: ?*Gpu.BindGroupLayout = null;
pub fn getBindGroupLayout(gpu: *Gpu) !*Gpu.BindGroupLayout {
    if (BIND_GROUP_LAYOUT) |p| return p;

    BIND_GROUP_LAYOUT = try gpu.device.createBindGroupLayout(&.{
        .label = .fromSlice("Camera:bind_group_layout"),
        .entry_count = 1,
        .entries = &.{
            .{
                .binding = 0,
                .visibility = Gpu.ShaderStage.vertexStage,
                .buffer = .{ .type = .uniform },
            },
        },
    });

    return BIND_GROUP_LAYOUT.?;
}

pub fn fromFrontUp(pos: vec3, front: vec3, up: vec3, world_up: vec3, size: vec2) Camera {
    var self = Camera{
        .pos = pos,
        .front = linalg.normalize(front),
        .up = linalg.normalize(up),
        .right = undefined,
        .world_up = world_up,
        .world_forward = deduceWorldForward(world_up),
    };

    self.deriveState(size);

    return self;
}

pub fn fromLookAt(pos: vec3, target: vec3, world_up: vec3, size: vec2) Camera {
    const front = linalg.normalize(target - pos);

    var self = Camera{
        .pos = pos,
        .front = front,
        // We initialize 'up' to world_up. deriveState() will immediately
        // orthogonalize this relative to 'front' to create the true local 'up'.
        .up = linalg.normalize(world_up),
        .right = undefined,
        .world_up = world_up,
        .world_forward = deduceWorldForward(world_up),
    };

    // Calculates valid Right/Up vectors and back-calculates Euler angles (Yaw/Pitch/Roll)
    self.deriveState(size);

    return self;
}

pub fn deinit(self: *Camera) void {
    if (self.buffer) |p| Gpu.bufferRelease(p);
    if (self.bind_group) |p| Gpu.bindGroupRelease(p);
}

pub fn sync(self: *Camera, gpu: *Gpu) !void {
    if (self.buffer == null) {
        self.buffer = try gpu.device.createBuffer(&.{
            .usage = .{ .uniform = true, .copy_dst = true },
            .size = @sizeOf(Camera.Uniform),
        });
    }
    if (self.bind_group == null) {
        self.bind_group = try gpu.device.createBindGroup(&.{
            .label = .fromSlice("Camera:bind_group"),
            .layout = try getBindGroupLayout(gpu),
            .entry_count = 1,
            .entries = &.{
                .{
                    .binding = 0,
                    .buffer = self.buffer,
                    .offset = 0,
                    .size = @sizeOf(Camera.Uniform),
                },
            },
        });
    }

    gpu.queue.writeBuffer(self.buffer.?, 0, &self.view_proj, @sizeOf(Camera.Uniform));
}

pub fn deduceWorldForward(world_up: vec3) vec3 {
    const axis =
        if (@abs(world_up[0]) > 0.99)
            vec3{ 0.0, 0.0, -1.0 }
        else
            vec3{ -1.0, 0.0, 0.0 };

    const right = linalg.normalize(linalg.vec3_cross(axis, world_up));
    const forward = linalg.normalize(linalg.vec3_cross(world_up, right));

    return forward;
}

pub fn deriveState(self: *Camera, size: vec2) void {
    // Ensure our basis is orthogonal and normalized based on inputs
    self.right = linalg.normalize(linalg.vec3_cross(self.front, self.up));
    self.up = linalg.normalize(linalg.vec3_cross(self.right, self.front));

    // --- Derive Yaw and Pitch (Same as before) ---
    const sin_p = std.math.clamp(linalg.dot(self.front, self.world_up), -1.0, 1.0);
    const pitch_rad = std.math.asin(sin_p);
    self.pitch = linalg.rad_to_deg * pitch_rad;

    const basis_left = linalg.normalize(linalg.vec3_cross(self.world_up, self.world_forward));
    const y_val = linalg.dot(self.front, basis_left);
    const x_val = linalg.dot(self.front, self.world_forward);
    const yaw_rad = std.math.atan2(y_val, x_val);
    self.yaw = linalg.rad_to_deg * yaw_rad;

    // --- Derive Roll ---
    // We compare our actual 'Right' vector to what the 'Right' vector WOULD be
    // if roll were zero (the "horizon right").

    // Reconstruct the "No-Roll" basis vectors
    // (Handle edge case where front is straight up/down)
    var ref_right = linalg.vec3_cross(self.front, self.world_up);
    if (linalg.len(ref_right) < 0.001) {
        // If looking straight up, use world_forward/right as reference
        ref_right = linalg.vec3_cross(self.front, self.world_forward);
    }
    ref_right = linalg.normalize(ref_right);

    // The "No-Roll" Up vector
    const ref_up = linalg.normalize(linalg.vec3_cross(ref_right, self.front));

    // Project our ACTUAL Right vector onto the Reference basis.
    // This gives us the cos/sin of the roll angle.
    // x (cos) = dot(actual_right, ref_right)
    // y (sin) = dot(actual_right, ref_up)
    const roll_cos = linalg.dot(self.right, ref_right);
    const roll_sin = linalg.dot(self.right, ref_up);

    self.roll = linalg.rad_to_deg * std.math.atan2(-roll_sin, roll_cos);

    self.calculateViewProj(size);
}

pub fn update(self: *Camera, input_state: *const InputState, size: vec2, delta_time: f32) bool {
    if (!input_state.getMouseButton(.middle).isDown()) {
        // If the middle mouse isn't held, we don't want to process keyboard input
        self.first_mouse = true; // Reset mouse movement tracking for when we do start moving again
        return false;
    }

    if (input_state.getMouseButton(.middle) != .pressed and self.first_mouse) {
        // drag must start in valid region
        return false;
    }

    const frame_speed: vec3 = @splat(self.speed * delta_time);
    const roll_step = self.roll_speed * delta_time;

    const pos = input_state.getMousePosition();
    if (self.first_mouse) {
        self.last_mouse = pos;
        self.first_mouse = false;
    }

    var delta = self.last_mouse - pos;
    self.last_mouse = pos;

    delta *= @splat(self.sensitivity);

    self.yaw += delta[0];
    self.pitch += delta[1];

    if (self.pitch > 89.0) self.pitch = 89.0;
    if (self.pitch < -89.0) self.pitch = -89.0;

    const yaw_rad = linalg.deg_to_rad * self.yaw;
    const pitch_rad = linalg.deg_to_rad * self.pitch;
    const roll_rad = linalg.deg_to_rad * self.roll;

    // --- Calculate Front (Direction) ---
    // This depends only on Yaw and Pitch
    const cos_yaw = std.math.cos(yaw_rad);
    const sin_yaw = std.math.sin(yaw_rad);
    const cos_pitch = std.math.cos(pitch_rad);
    const sin_pitch = std.math.sin(pitch_rad);

    const basis_left = linalg.normalize(linalg.vec3_cross(self.world_up, self.world_forward));

    const fwd_contribution = self.world_forward * @as(vec3, @splat(cos_yaw * cos_pitch));
    const left_contribution = basis_left * @as(vec3, @splat(sin_yaw * cos_pitch));
    const up_contribution = self.world_up * @as(vec3, @splat(sin_pitch));

    self.front = linalg.normalize(fwd_contribution + left_contribution + up_contribution);

    // --- Calculate the "Reference" Basis (Zero Roll) ---
    // This represents the camera as if it were perfectly level with the horizon.
    const ref_right = linalg.normalize(linalg.vec3_cross(self.front, self.world_up));
    const ref_up = linalg.normalize(linalg.vec3_cross(ref_right, self.front));

    // --- Apply Roll Rotation ---
    // We rotate the Reference Up/Right vectors around the Front vector by 'roll_rad'.
    // We can use a standard 2D rotation matrix formula here because ref_right and ref_up
    // form a 2D plane perpendicular to 'front'.

    const cos_roll = std.math.cos(roll_rad);
    const sin_roll = std.math.sin(-roll_rad);

    const r_cos = ref_right * @as(vec3, @splat(cos_roll));
    const r_sin = ref_up * @as(vec3, @splat(sin_roll));

    self.right = linalg.normalize(r_cos + r_sin);

    // We could calculate Up using rotation too, but cross product ensures orthogonality errors don't drift
    self.up = linalg.normalize(linalg.vec3_cross(self.right, self.front));

    if (input_state.getKey(.w).isDown()) {
        self.pos += self.front * frame_speed;
    }
    if (input_state.getKey(.s).isDown()) {
        self.pos -= self.front * frame_speed;
    }
    if (input_state.getKey(.d).isDown()) {
        self.pos += self.right * frame_speed;
    }
    if (input_state.getKey(.a).isDown()) {
        self.pos -= self.right * frame_speed;
    }
    if (input_state.getKey(.left_shift).isDown()) {
        // Move along the LOCAL up vector now, not world up
        self.pos += self.up * frame_speed;
    }
    if (input_state.getKey(.left_control).isDown()) {
        self.pos -= self.up * frame_speed;
    }

    if (input_state.getKey(.q).isDown()) {
        self.roll -= roll_step;
        self.recalcBasis();
    }
    if (input_state.getKey(.e).isDown()) {
        self.roll += roll_step;
        self.recalcBasis();
    }

    self.calculateViewProj(size);

    return true;
}

// Helper to apply rotation without mouse movement input
fn recalcBasis(self: *Camera) void {
    // This is essentially the logic from handle_mouse_move, but without changing yaw/pitch
    const roll_rad = linalg.deg_to_rad * self.roll;

    // Get the stable "Horizon" basis
    const ref_right = linalg.normalize(linalg.vec3_cross(self.front, self.world_up));
    const ref_up = linalg.normalize(linalg.vec3_cross(ref_right, self.front));

    const cos_roll = std.math.cos(roll_rad);
    const sin_roll = std.math.sin(-roll_rad);

    const r_cos = ref_right * @as(vec3, @splat(cos_roll));
    const r_sin = ref_up * @as(vec3, @splat(sin_roll));

    self.right = linalg.normalize(r_cos + r_sin);
    self.up = linalg.normalize(linalg.vec3_cross(self.right, self.front));
}

pub fn calculateViewProj(self: *Camera, size: vec2) void {
    const aspect = if (size[1] == 0) 1.0 else size[0] / size[1];

    const proj = linalg.mat4_perspective(
        linalg.deg_to_rad * 60.0,
        aspect,
        0.1,
        1000.0,
    );

    const view = linalg.mat4_look_at(
        self.pos,
        self.pos + self.front,
        self.up,
    );

    self.view_proj = linalg.mat4_mul(proj, view);
}
