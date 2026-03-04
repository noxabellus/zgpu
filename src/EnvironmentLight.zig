pub const EnvironmentLight = @This();

const Gpu = @import("Gpu.zig");
const linalg = @import("linalg.zig");
const vec3 = linalg.vec3;

pub const Uniform = extern struct {
    brightness: f32 = 1.0,
    direction: vec3 = linalg.normalize(vec3{ 0.5, 1.0, 0.75 }),
    color: vec3 = vec3{ 1.0, 1.0, 1.0 },
    ambient_color: vec3 = vec3{ 0.1, 0.1, 0.1 },
};

config: Uniform = .{},
buffer: ?*Gpu.Buffer = null,
bind_group: ?*Gpu.BindGroup = null,

var BIND_GROUP_LAYOUT: ?*Gpu.BindGroupLayout = null;
pub fn getBindGroupLayout(gpu: *Gpu) !*Gpu.BindGroupLayout {
    if (BIND_GROUP_LAYOUT) |p| return p;

    BIND_GROUP_LAYOUT = try gpu.device.createBindGroupLayout(&.{
        .label = .fromSlice("EnvironmentLight:bind_group_layout"),
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

pub fn deinit(self: *EnvironmentLight) void {
    if (self.buffer) |p| p.release();
    if (self.bind_group) |p| p.release();
}

pub fn sync(self: *EnvironmentLight, gpu: *Gpu) !void {
    if (self.buffer == null) {
        self.buffer = try gpu.device.createBuffer(&.{
            .usage = .{ .uniform = true, .copy_dst = true },
            .size = @sizeOf(EnvironmentLight.Uniform),
        });
    }
    if (self.bind_group == null) {
        self.bind_group = try gpu.device.createBindGroup(&.{
            .label = .fromSlice("EnvironmentLight:bind_group"),
            .layout = try getBindGroupLayout(gpu),
            .entry_count = 1,
            .entries = &.{
                .{
                    .binding = 0,
                    .buffer = self.buffer,
                    .offset = 0,
                    .size = @sizeOf(EnvironmentLight.Uniform),
                },
            },
        });
    }

    gpu.queue.writeBuffer(self.buffer.?, 0, &self.config, @sizeOf(Uniform));
}
