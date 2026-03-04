const std = @import("std");

pub const example = @import("examples/playground.zig");

pub const main = example.main;

pub const std_options = if (@hasDecl(example, "std_options")) example.std_options else std.Options{
    .log_level = .info,
};

pub const Application = @import("Application.zig");
pub const AssetCache = @import("AssetCache.zig");
pub const Atlas = @import("Atlas.zig");
pub const Batch2D = @import("Batch2D.zig");
pub const Batch3D = @import("Batch3D.zig");
pub const BindingState = @import("BindingState.zig");
pub const Camera = @import("Camera.zig");
pub const Compositor = @import("Compositor.zig");
pub const debug = @import("debug.zig");
pub const ecs = @import("ecs.zig");
pub const EnvironmentLight = @import("EnvironmentLight.zig");
pub const Gpu = @import("Gpu.zig");
pub const InputState = @import("InputState.zig");
pub const linalg = @import("linalg.zig");
pub const Model = @import("Model.zig");
pub const RenderTexture = @import("RenderTexture.zig");
pub const SlotMap = @import("SlotMap.zig");
pub const Ui = @import("Ui.zig");

test {
    std.testing.refAllDecls(@This());
}
