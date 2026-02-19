const RenderTexture = @This();

const std = @import("std");
const wgpu = @import("wgpu");
const Gpu = @import("Gpu.zig");

texture: wgpu.Texture,
view: wgpu.TextureView,
desc: Descriptor,

pub const Descriptor = struct {
    label: []const u8 = "RenderTexture",
    width: u32,
    height: u32,
    format: wgpu.TextureFormat,
    usage: wgpu.TextureUsage = .{ .render_attachment = true, .texture_binding = true },
    sample_count: u32 = 1,
    mip_level_count: u32 = 1,
};

/// Creates a new texture and its default view.
pub fn init(gpu: *Gpu, desc: Descriptor) !RenderTexture {
    var self = RenderTexture{
        .texture = null,
        .view = null,
        .desc = desc,
    };

    try self.sync(gpu);

    return self;
}

/// Cleans up the WGPU resources and invalidates the structure.
pub fn deinit(self: *RenderTexture) void {
    self.release();
    self.* = undefined;
}

/// Helper to recreate the texture when the window is resized.
pub fn resize(self: *RenderTexture, gpu: *Gpu, new_width: u32, new_height: u32) !void {
    if (self.desc.width == new_width and self.desc.height == new_height and self.texture != null and self.view != null) return;

    self.desc.width = new_width;
    self.desc.height = new_height;

    self.release();

    try self.sync(gpu);
}

fn sync(self: *RenderTexture, gpu: *Gpu) !void {
    self.texture = wgpu.deviceCreateTexture(gpu.device, &wgpu.TextureDescriptor{
        .label = .fromSlice(self.desc.label),
        .size = .{
            .width = self.desc.width,
            .height = self.desc.height,
            .depth_or_array_layers = 1,
        },
        .mip_level_count = self.desc.mip_level_count,
        .sample_count = self.desc.sample_count,
        .dimension = .@"2d",
        .format = self.desc.format,
        .usage = self.desc.usage,
    });
    if (self.texture == null) return error.FailedToCreateTexture;
    errdefer wgpu.textureRelease(self.texture);

    self.view = wgpu.textureCreateView(self.texture, &wgpu.TextureViewDescriptor{
        .label = .fromSlice(self.desc.label),
        .format = self.desc.format,
        .dimension = .@"2d",
        .base_mip_level = 0,
        .mip_level_count = self.desc.mip_level_count,
        .base_array_layer = 0,
        .array_layer_count = 1,
        .aspect = .all,
    });
    if (self.view == null) return error.FailedToCreateTextureView;
}

/// Cleans up the WGPU resources.
fn release(self: *RenderTexture) void {
    if (self.view != null) wgpu.textureViewRelease(self.view);
    self.view = null;

    if (self.texture != null) wgpu.textureRelease(self.texture);
    self.texture = null;
}
