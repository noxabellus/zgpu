const linalg = @This();

const std = @import("std");
const log = std.log.scoped(.linalg);

test {
    log.debug("semantic analysis for linalg.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const mat4 = [4]vec4;
pub const mat3 = [3]vec3;

pub const aabb2 = [2]vec2;
pub const aabb2i = [2]vec2i;
pub const aabb2u = [2]vec2u;
pub const aabb3 = [2]vec3;
pub const aabb3i = [2]vec3i;
pub const aabb3u = [2]vec3u;

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

pub const deg_to_rad = std.math.rad_per_deg;
pub const rad_to_deg = std.math.deg_per_rad;

/// check if a point is inside the AABB
pub fn aabb_contains_point(aabb: anytype, point: @typeInfo(@TypeOf(aabb)).array.child) bool {
    const less = point < aabb[0];
    const greater = point > aabb[1];

    return !(@reduce(.Or, less) or @reduce(.Or, greater));
}

/// get the smallest AABB that contains both a and b
pub fn aabb_union(a: anytype, b: @TypeOf(a)) @TypeOf(a) {
    return .{
        @min(a[0], b[0]),
        @max(a[1], b[1]),
    };
}

/// get the overlapping region of a and b, or null if they don't overlap.
pub fn aabb_intersect(a: anytype, b: @TypeOf(a)) ?@TypeOf(a) {
    const intersect_min = @max(a[0], b[0]);
    const intersect_max = @min(a[1], b[1]);

    // An invalid AABB means there was no overlap.
    if (@reduce(.And, intersect_min <= intersect_max)) {
        return .{ intersect_min, intersect_max };
    } else {
        return null;
    }
}

/// check if a and b have any overlap
pub fn aabb_overlapping(a: anytype, b: @TypeOf(a)) bool {
    const less_than = a[1] < b[0];
    const greater_than = a[0] > b[1];

    return !(@reduce(.Or, less_than) or @reduce(.Or, greater_than));
}

/// check if b is completely inside a
pub fn aabb_contains(a: anytype, b: @TypeOf(a)) bool {
    const less_than = a[0] <= b[0];
    const greater_than = a[1] >= b[1];
    return @reduce(.And, less_than) and @reduce(.And, greater_than);
}

pub fn numComponents(comptime T: type) usize {
    return @typeInfo(T).vector.len;
}

pub fn dot(a: anytype, b: @TypeOf(a)) @typeInfo(@TypeOf(a)).vector.child {
    var result: @typeInfo(@TypeOf(a)).vector.child = 0.0;
    inline for (0..comptime numComponents(@TypeOf(a))) |i| {
        result += a[i] * b[i];
    }
    return result;
}

pub fn len(v: anytype) @typeInfo(@TypeOf(v)).vector.child {
    return std.math.sqrt(dot(v, v));
}

pub fn len_sq(v: anytype) @typeInfo(@TypeOf(v)).vector.child {
    return dot(v, v);
}

pub fn normalize(v: anytype) @TypeOf(v) {
    const l = len(v);
    if (l == 0) return v;
    var result: @TypeOf(v) = undefined;
    inline for (0..comptime numComponents(@TypeOf(v))) |i| {
        result[i] = v[i] / l;
    }
    return result;
}

pub fn cross(a: vec3, b: vec3) vec3 {
    return .{
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    };
}

// --- Matrix Operations ---

/// Returns a 4x4 identity matrix.
pub fn mat4_identity() mat4 {
    return mat4{
        .{ 1, 0, 0, 0 },
        .{ 0, 1, 0, 0 },
        .{ 0, 0, 1, 0 },
        .{ 0, 0, 0, 1 },
    };
}

/// Multiplies two 4x4 matrices (self * other).
pub fn mat4_mul(m1: mat4, m2: mat4) mat4 {
    var out: mat4 = @splat(@splat(0.0));
    comptime var c: usize = 0;
    inline while (c < 4) : (c += 1) {
        comptime var r: usize = 0;
        inline while (r < 4) : (r += 1) {
            // zig fmt: off
            out[c][r] 
                = m1[0][r] * m2[c][0]
                + m1[1][r] * m2[c][1]
                + m1[2][r] * m2[c][2]
                + m1[3][r] * m2[c][3];
            // zig fmt: on
        }
    }
    return out;
}

/// Creates a view matrix that looks from `eye` towards `center`.
pub fn mat4_look_at(eye: vec3, center: vec3, up: vec3) mat4 {
    const f = normalize(center - eye);
    const s = normalize(cross(f, up));
    const u = cross(s, f);

    // Note: The view matrix is the inverse of the camera's transformation matrix.
    // This results in the basis vectors (s, u, -f) being laid out in rows
    // of the conceptual matrix, which means they are spread across the columns
    // in our column-major memory layout.
    return mat4{
        // Column 0
        .{ s[0], u[0], -f[0], 0 },
        // Column 1
        .{ s[1], u[1], -f[1], 0 },
        // Column 2
        .{ s[2], u[2], -f[2], 0 },
        // Column 3
        .{ -dot(s, eye), -dot(u, eye), dot(f, eye), 1 },
    };
}

/// Creates a perspective projection matrix.
///
/// This implementation is specifically for APIs like Vulkan, Metal, and WGPU
/// that use a depth range of [0, 1], unlike OpenGL's [-1, 1].
///
/// Parameters:
/// - `fovy_rad`: Vertical field of view in radians.
/// - `aspect`: Aspect ratio of the viewport (width / height).
/// - `z_near`: Distance to the near clipping plane (must be positive).
/// - `z_far`: Distance to the far clipping plane (must be positive).
pub fn mat4_perspective(fovy_rad: f32, aspect: f32, z_near: f32, z_far: f32) mat4 {
    const f = 1.0 / std.math.tan(fovy_rad / 2.0);
    const nf = z_near - z_far;

    return mat4{
        // Column 0
        .{ f / aspect, 0, 0, 0 },
        // Column 1
        .{ 0, f, 0, 0 },
        // Column 2
        .{ 0, 0, z_far / nf, -1 },
        // Column 3
        .{ 0, 0, (z_far * z_near) / nf, 0 },
    };
}

/// Creates an orthographic projection matrix.
pub fn mat4_ortho(left: f32, right: f32, bottom: f32, top: f32, near: f32, far: f32) mat4 {
    const rml = right - left;
    const tmb = top - bottom;
    const fmn = far - near;

    var res = mat4_identity();
    res[0][0] = 2.0 / rml;
    res[1][1] = 2.0 / tmb;
    res[2][2] = -1.0 / fmn; // Use -1 for [0, 1] depth range
    res[3][0] = -(right + left) / rml;
    res[3][1] = -(top + bottom) / tmb;
    res[3][2] = -near / fmn;
    return res;
}
