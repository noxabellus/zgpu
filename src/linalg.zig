const linalg = @This();

const std = @import("std");
const log = std.log.scoped(.linalg);

test {
    log.debug("semantic analysis for linalg.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const quat = vec4;
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

/// Get the number of elements in a vector type
pub fn numComponents(comptime T: type) usize {
    return @typeInfo(T).vector.len;
}

/// Convert a value to a vector type, or leave it in place and coerce if it is already a vector
pub fn upcast(comptime T: type, v: anytype) T {
    const U = @TypeOf(v);
    const U_info = @typeInfo(U);
    return if (comptime U_info == .vector) v else @splat(v);
}

/// N-dimensional dot product on vectors
pub fn dot(a: anytype, b: @TypeOf(a)) @typeInfo(@TypeOf(a)).vector.child {
    var result: @typeInfo(@TypeOf(a)).vector.child = 0.0;
    inline for (0..comptime numComponents(@TypeOf(a))) |i| {
        result += a[i] * b[i];
    }
    return result;
}

/// get the magnitude of an n-dimensional vector
pub fn len(v: anytype) @typeInfo(@TypeOf(v)).vector.child {
    return std.math.sqrt(dot(v, v));
}

/// get the square of the magnitude of an n-dimensional vector
pub fn len_sq(v: anytype) @typeInfo(@TypeOf(v)).vector.child {
    return dot(v, v);
}

/// N-dimensional vector normalization
pub fn normalize(v: anytype) @TypeOf(v) {
    const l = len(v);
    if (l == 0) return v;
    var result: @TypeOf(v) = undefined;
    inline for (0..comptime numComponents(@TypeOf(v))) |i| {
        result[i] = v[i] / l;
    }
    return result;
}

/// Cross product for vector3 only
pub fn vec3_cross(a: vec3, b: vec3) vec3 {
    return .{
        a[1] * b[2] - a[2] * b[1],
        a[2] * b[0] - a[0] * b[2],
        a[0] * b[1] - a[1] * b[0],
    };
}

/// Linear interpolate two N-dimensional vectors by at an N-dimensional vector or scalar delta
pub fn lerp(a: anytype, b: @TypeOf(a), t: anytype) @TypeOf(a) {
    const tv = upcast(@TypeOf(a), t);
    return a + (b - a) * tv;
}

/// Compare two vectors for approximate equality, with an absolute tolerance
pub fn approxEqAbs(a: anytype, b: @TypeOf(a), epsilon: anytype) bool {
    const T = @TypeOf(a);
    const S = @typeInfo(T).vector.child;
    const eps = upcast(T, epsilon);
    inline for (0..comptime numComponents(T)) |i| {
        if (!std.math.approxEqAbs(S, a[i], b[i], eps[i])) {
            return false;
        }
    }
    return true;
}

/// Create a new N-dimensional vector from another vector or a scalar
pub fn ExtrapolateComponent(comptime N: usize, comptime T: type) type {
    const T_info = @typeInfo(T);
    return @Vector(N, if (T_info == .vector) T_info.vector.child else T);
}

/// Extract the first three components of a vector, or splat a scalar to three components
pub fn xyz(v: anytype) ExtrapolateComponent(3, @TypeOf(v)) {
    const T = @TypeOf(v);
    const T_info = @typeInfo(T);

    var out: ExtrapolateComponent(3, T) = undefined;
    if (comptime T_info == .vector) {
        if (T_info.vector.len < 3) @compileError("linalg.xyz requires at least 3 components or a scalar");
        inline for (0..3) |i| {
            out[i] = v[i];
        }
    } else {
        inline for (0..3) |i| {
            out[i] = v;
        }
    }

    return out;
}

/// A 4x4 identity matrix.
pub const mat4_identity =
    mat4{
        .{ 1, 0, 0, 0 },
        .{ 0, 1, 0, 0 },
        .{ 0, 0, 1, 0 },
        .{ 0, 0, 0, 1 },
    };

/// A quaternion representing no rotation.
pub const quat_identity = quat{ 0, 0, 0, 1 };

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
    const s = normalize(vec3_cross(f, up));
    const u = vec3_cross(s, f);

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

    var res = mat4_identity;
    res[0][0] = 2.0 / rml;
    res[1][1] = 2.0 / tmb;
    res[2][2] = -1.0 / fmn; // Use -1 for [0, 1] depth range
    res[3][0] = -(right + left) / rml;
    res[3][1] = -(top + bottom) / tmb;
    res[3][2] = -near / fmn;
    return res;
}

/// Creates a 4x4 matrix from translation, rotation and scale components
pub fn mat4_compose(t: vec3, r: quat, s: vec3) mat4 {
    return linalg.mat4_mul(linalg.mat4_mul(linalg.mat4_translate(t), linalg.mat4_from_quat(r)), linalg.mat4_scale(s));
}
/// Creates a 4x4 translation matrix.
pub fn mat4_translate(translation: vec3) mat4 {
    var m = mat4_identity;
    m[3] = .{ translation[0], translation[1], translation[2], 1.0 };
    return m;
}

/// Creates a 4x4 scaling matrix.
pub fn mat4_scale(s: vec3) mat4 {
    var m = mat4_identity;
    m[0][0] = s[0];
    m[1][1] = s[1];
    m[2][2] = s[2];
    return m;
}

/// Creates a 4x4 rotation matrix from a quaternion.
/// The quaternion is assumed to have components (x, y, z, w).
pub fn mat4_from_quat(q: quat) mat4 {
    const nq = normalize(q);
    const x = nq[0];
    const y = nq[1];
    const z = nq[2];
    const w = nq[3];

    const xx = x * x;
    const yy = y * y;
    const zz = z * z;
    const xy = x * y;
    const xz = x * z;
    const yz = y * z;
    const wx = w * x;
    const wy = w * y;
    const wz = w * z;

    var out: mat4 = undefined;

    // Column 0
    out[0][0] = 1.0 - 2.0 * (yy + zz);
    out[0][1] = 2.0 * (xy + wz);
    out[0][2] = 2.0 * (xz - wy);
    out[0][3] = 0.0;

    // Column 1
    out[1][0] = 2.0 * (xy - wz);
    out[1][1] = 1.0 - 2.0 * (xx + zz);
    out[1][2] = 2.0 * (yz + wx);
    out[1][3] = 0.0;

    // Column 2
    out[2][0] = 2.0 * (xz + wy);
    out[2][1] = 2.0 * (yz - wx);
    out[2][2] = 1.0 - 2.0 * (xx + yy);
    out[2][3] = 0.0;

    // Column 3
    out[3] = .{ 0.0, 0.0, 0.0, 1.0 };

    return out;
}

/// Cubic hermite interpolation on 3-dimensional vectors; delta may be either another vec3 or a scalar
pub fn vec3_interp_cubic(v1: vec3, tangent1: vec3, v2: vec3, tangent2: vec3, t: anytype) vec3 {
    const tv = upcast(quat, t);

    const t2 = tv * tv;
    const t3 = tv * tv * tv;

    const one: vec3 = @splat(1);
    const two: vec3 = @splat(2);
    const three: vec3 = @splat(3);

    // zig fmt: off
    return (two * t3 - three * t2 + one) * v1
         + (t3 - two * t2 + tv) * tangent1
         + (-two * t3 + three * t2) * v2
         + (t3 - t2) * tangent2
         ;
    // zig fmt: on
}

/// Spherical linear interpolation on 3-dimensional vectors; delta may be either another vec3 or a scalar
pub fn quat_slerp(q1: quat, q2: quat, t: anytype) quat {
    const tv = upcast(quat, t);

    // Ensure quaternions are normalized
    const temp_q1 = normalize(q1);
    var temp_q2 = normalize(q2);

    var d = dot(temp_q1, temp_q2);

    // If dot product is negative, negate one quaternion to take the shortest path
    if (d < 0.0) {
        temp_q2 = -temp_q2;
        d = -d; // Recalculate d product (now positive)
    }

    // Handle near-parallel quaternions to avoid division by zero
    const DOT_THRESHOLD = 0.9995;
    if (d > DOT_THRESHOLD) {
        // Linear interpolation (LERP) as an approximation
        return normalize(temp_q1 + tv * (temp_q2 - temp_q1));
    }

    const theta = std.math.acos(d);
    const sin_theta = std.math.sin(theta);

    var out: quat = undefined;

    inline for (0..4) |i| {
        const s0 = std.math.sin((1.0 - tv[i]) * theta) / sin_theta;
        const s1 = std.math.sin(tv[i] * theta) / sin_theta;
        out[i] = s0 * temp_q1[i] + s1 * temp_q2[i];
    }

    return out;
}

/// Cubic hermite interpolation on quaternions; delta may be either another quat or a scalar
pub fn quat_interp_cubic(q1: quat, outTangent1: quat, q2: quat, inTangent2: quat, t: anytype) quat {
    const tv = upcast(quat, t);

    const t2 = tv * tv;
    const t3 = t2 * tv;

    const one: quat = @splat(1);
    const two: quat = @splat(2);
    const three: quat = @splat(3);

    const h00: quat = two * t3 - three * t2 + one;
    const h10: quat = t3 - two * t2 + t;
    const h01: quat = -two * t3 + three * t2;
    const h11: quat = t3 - t2;

    const p0 = q1 * h00;
    const m0 = outTangent1 * h10;
    const p1 = q2 * h01;
    const m1 = inTangent2 * h11;

    return normalize(p0 + m0 + p1 + m1);
}
