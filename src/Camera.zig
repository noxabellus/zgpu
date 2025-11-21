const Camera = @This();

const std = @import("std");
const glfw = @import("glfw");

const Application = @import("Application.zig");
const linalg = @import("linalg.zig");
const vec2 = linalg.vec2;
const vec3 = linalg.vec3;
const mat4 = linalg.mat4;

// --- Camera state for fly controls (Z-up coordinate system) ---
pos: vec3 = .{ 8.125, 28.125, 2 }, // Start outside the sphere (X, Y, Z with Z-up)
front: vec3 = .{ 0.0, -1.0, 0.0 }, // Looking towards the sphere
up: vec3 = .{ 0.0, 0.0, 1.0 }, // Z is up
right: vec3 = .{ 0.0, 0.0, 0.0 }, // will be calculated

yaw: f32 = -90.0,
pitch: f32 = 0.0,

first_mouse: bool = true,
last_mouse: vec2 = .{ 400.0, 300.0 },
sensitivity: f32 = 0.1,
speed: f32 = 10.0,

// A struct for our camera's view-projection matrix uniform.
pub const Uniform = extern struct {
    view_proj: mat4,
};

// --- Mouse movement callback ---
pub fn handle_mouse_move(self: *Camera, w: *glfw.Window, pos: vec2) callconv(.c) void {
    const btn_state = glfw.getMouseButton(w, .button_3);
    if (btn_state != .repeat and btn_state != .press) {
        self.first_mouse = true;
        return;
    }

    if (self.first_mouse) {
        self.last_mouse = pos;
        self.first_mouse = false;
    }

    var delta = self.last_mouse - pos;
    self.last_mouse = pos;

    delta *= @splat(self.sensitivity);

    self.yaw += delta[0];
    self.pitch += delta[1];

    // constrain pitch
    if (self.pitch > 89.0) {
        self.pitch = 89.0;
    }
    if (self.pitch < -89.0) {
        self.pitch = -89.0;
    }

    var front: vec3 = undefined;
    const yaw_rad = linalg.deg_to_rad * self.yaw;
    const pitch_rad = linalg.deg_to_rad * self.pitch;
    // Z-up coordinate system: X-right, Y-forward, Z-up
    front[0] = std.math.cos(yaw_rad) * std.math.cos(pitch_rad);
    front[1] = std.math.sin(yaw_rad) * std.math.cos(pitch_rad);
    front[2] = std.math.sin(pitch_rad);
    self.front = linalg.normalize(front);
    // Also re-calculate the Right and Up vector (world up is Z-axis)
    self.right = linalg.normalize(linalg.vec3_cross(self.front, .{ 0.0, 0.0, 1.0 }));
    self.up = linalg.normalize(linalg.vec3_cross(self.right, self.front));
}

pub fn update(self: *Camera, window: *glfw.Window, delta: f32, uniform: *Uniform) void {
    const frame_speed: vec3 = @splat(self.speed * delta);
    if (glfw.getKey(window, .w) == .press) {
        self.pos += self.front * frame_speed;
    }
    if (glfw.getKey(window, .d) == .press) {
        self.pos += self.right * frame_speed;
    }
    if (glfw.getKey(window, .e) == .press) {
        self.pos += self.up * frame_speed;
    }

    if (glfw.getKey(window, .s) == .press) {
        self.pos -= self.front * frame_speed;
    }
    if (glfw.getKey(window, .a) == .press) {
        self.pos -= self.right * frame_speed;
    }
    if (glfw.getKey(window, .q) == .press) {
        self.pos -= self.up * frame_speed;
    }

    // --- Calculate the view-projection matrix from fly camera state ---
    {
        var width: i32 = 0;
        var height: i32 = 0;
        glfw.getFramebufferSize(window, &width, &height);
        const aspect = if (height == 0) 1.0 else @as(f32, @floatFromInt(width)) / @as(f32, @floatFromInt(height));

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

        uniform.view_proj = linalg.mat4_mul(proj, view);
    }
}
