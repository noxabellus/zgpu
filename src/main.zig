const std = @import("std");

pub const example = @import("examples/gltf.zig");

pub const main = example.main;

pub const std_options = if (@hasDecl(example, "std_options")) example.std_options else std.Options{
    .log_level = .info,
};

pub const AssetCache = @import("AssetCache.zig");
pub const Atlas = @import("Atlas.zig");
pub const Batch2D = @import("Batch2D.zig");
pub const BindingState = @import("BindingState.zig");
pub const debug = @import("debug.zig");
pub const MicroGrid = @import("MicroGrid.zig");
pub const InputState = @import("InputState.zig");
pub const linalg = @import("linalg.zig");
pub const Ui = @import("Ui.zig");
pub const SlotMap = @import("SlotMap.zig");
pub const ecs = @import("ecs.zig");
pub const tests = @import("tests.zig");

test {
    std.testing.refAllDecls(@This());
}
