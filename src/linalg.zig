const linalg = @This();

const std = @import("std");
const log = std.log.scoped(.linalg);

test {
    log.debug("semantic analysis for linalg.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const mat4 = [16]f32;
pub const mat3 = [9]f32;
pub const vec2 = @Vector(2, f32);
pub const vec3 = @Vector(3, f32);
pub const vec4 = @Vector(4, f32);
pub const vec2i = @Vector(2, i32);
pub const vec3i = @Vector(3, i32);
pub const vec4i = @Vector(4, i32);
pub const vec2u = @Vector(2, u32);
pub const vec3u = @Vector(3, u32);
pub const vec4u = @Vector(4, u32);
pub const vec2b = @Vector(2, bool);
pub const vec3b = @Vector(3, bool);
pub const vec4b = @Vector(4, bool);

/// orthographic projection matrix helper
pub fn mat4_ortho(left: f32, right: f32, bottom: f32, top: f32, near: f32, far: f32) mat4 {
    var mat: mat4 = std.mem.zeroes(mat4);
    mat[0] = 2.0 / (right - left);
    mat[5] = 2.0 / (top - bottom);
    mat[10] = -2.0 / (far - near);
    mat[12] = -(right + left) / (right - left);
    mat[13] = -(top + bottom) / (top - bottom);
    mat[14] = -(far + near) / (far - near);
    mat[15] = 1.0;
    return mat;
}
