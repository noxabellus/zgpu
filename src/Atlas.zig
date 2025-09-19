//! Manages a single GPU texture as a dynamic atlas for images and glyphs.
//!
//! This module is responsible for finding space for new images, uploading their
//! pixel data to the GPU, and reporting when it is full. It is designed to be
//! a single "page" in a larger multi-atlas system. All functions use a
//! C-style API, taking a pointer to the Atlas state struct.

const Atlas = @This();

const std = @import("std");
const wgpu = @import("wgpu");
const stbrp = @import("stbrp");

const log = std.log.scoped(.atlas);

test {
    log.debug("semantic analysis for Atlas.zig", .{});
    std.testing.refAllDecls(@This());
}

/// Describes the pixel format of an image being submitted to the atlas.
/// This allows the atlas to handle data from various sources generically.
pub const PixelFormat = enum {
    /// Single-channel 8-bit data, typically alpha from a font glyph.
    /// Will be expanded to RGBA { 255, 255, 255, alpha } before uploading.
    grayscale,
    /// Four-channel 8-bit RGBA data, typically from a loaded image file.
    rgba,
};

/// A generic description of an image to be packed into the atlas.
pub const InputImage = struct {
    pixels: []const u8,
    width: u32,
    height: u32,
    format: PixelFormat,
};

/// The result of a packing operation, indicating how many of the submitted
/// images were successfully packed and uploaded.
pub const PackResult = struct {
    packed_count: usize,
};

// --- State ---

/// The primary state struct for a single texture atlas page.
/// A pointer to this struct is passed to all functions in this module.
allocator: std.mem.Allocator,
device: wgpu.Device,
queue: wgpu.Queue,

texture: wgpu.Texture,
view: wgpu.TextureView,
width: u32,
height: u32,

pack_context: stbrp.Context,
pack_nodes: []stbrp.Node,

// --- Private Helper Functions ---

/// Private helper to upload a single image, handling pixel format conversion.
fn uploadImage(
    self: *Atlas,
    image: InputImage,
    x: u32,
    y: u32,
) !void {
    const copy_destination = wgpu.TexelCopyTextureInfo{
        .texture = self.texture,
        .mip_level = 0,
        .origin = .{ .x = x, .y = y, .z = 0 },
    };

    const copy_size = wgpu.Extent3D{
        .width = image.width,
        .height = image.height,
        .depth_or_array_layers = 1,
    };

    switch (image.format) {
        .rgba => {
            // For RGBA, we can upload the data directly.
            const layout = wgpu.TexelCopyBufferLayout{
                .bytes_per_row = image.width * 4,
                .rows_per_image = image.height,
            };
            wgpu.queueWriteTexture(self.queue, &copy_destination, image.pixels.ptr, image.pixels.len, &layout, &copy_size);
        },
        .grayscale => {
            // For grayscale, we must expand it to RGBA on the CPU first.
            // We create a temporary buffer for the expanded pixel data.
            const rgba_data = try self.allocator.alloc(u8, image.width * image.height * 4);
            defer self.allocator.free(rgba_data);

            for (image.pixels, 0..) |alpha, i| {
                rgba_data[i * 4 + 0] = 255; // R
                rgba_data[i * 4 + 1] = 255; // G
                rgba_data[i * 4 + 2] = 255; // B
                rgba_data[i * 4 + 3] = alpha; // A
            }

            const layout = wgpu.TexelCopyBufferLayout{
                .bytes_per_row = image.width * 4,
                .rows_per_image = image.height,
            };
            wgpu.queueWriteTexture(self.queue, &copy_destination, rgba_data.ptr, rgba_data.len, &layout, &copy_size);
        },
    }
}

// --- Public API ---

/// Creates and initializes a new Atlas.
///
/// - `allocator`: Used to allocate internal data for the packing algorithm.
/// - `device`, `queue`: WGPU objects for creating the texture and uploading data.
/// - `width`, `height`: The dimensions of the atlas texture. A larger atlas
///   reduces the need for flushes but uses more VRAM. Must be power-of-two.
pub fn init(
    allocator: std.mem.Allocator,
    device: wgpu.Device,
    queue: wgpu.Queue,
    width: u32,
    height: u32,
) !*Atlas {
    log.info("initializing atlas with size {d}x{d}", .{ width, height });
    const self = try allocator.create(Atlas);
    errdefer allocator.destroy(self);

    // STB Rect Pack documentation suggests the number of nodes should be equal
    // to the width of the packing space.
    const num_nodes = width;
    const pack_nodes = try allocator.alloc(stbrp.Node, num_nodes);
    errdefer allocator.free(pack_nodes);

    // Create the GPU texture that will store all our images and glyphs.
    // The format is always RGBA, as we will convert grayscale images before upload.
    const texture_descriptor = wgpu.TextureDescriptor{
        .label = .fromSlice("texture_atlas"),
        .size = .{ .width = width, .height = height, .depth_or_array_layers = 1 },
        .mip_level_count = 1,
        .sample_count = 1,
        .dimension = .@"2d",
        .format = .rgba8_unorm_srgb,
        .usage = wgpu.TextureUsage{ .texture_binding = true, .copy_dst = true },
    };
    const texture = wgpu.deviceCreateTexture(device, &texture_descriptor);
    if (texture == null) {
        log.err("failed to create WGPU texture for atlas", .{});
        allocator.free(pack_nodes);
        allocator.destroy(self);
        return error.WgpuTextureCreationFailed;
    }
    errdefer wgpu.textureRelease(texture);

    const view = wgpu.textureCreateView(texture, null);
    if (view == null) {
        log.err("failed to create WGPU texture view for atlas", .{});
        wgpu.textureRelease(texture);
        allocator.free(pack_nodes);
        allocator.destroy(self);
        return error.WgpuTextureViewCreationFailed;
    }

    // Initialize our state
    self.* = .{
        .allocator = allocator,
        .device = device,
        .queue = queue,
        .texture = texture,
        .view = view,
        .width = width,
        .height = height,
        .pack_context = .{},
        .pack_nodes = pack_nodes,
    };

    // Finally, initialize the stb_rect_pack context.
    stbrp.initTarget(&self.pack_context, @intCast(width), @intCast(height), self.pack_nodes.ptr, @intCast(self.pack_nodes.len));

    return self;
}

/// Releases all GPU and CPU resources used by the Atlas.
pub fn deinit(self: *Atlas) void {
    log.info("deinitializing atlas", .{});
    wgpu.textureViewRelease(self.view);
    wgpu.textureRelease(self.texture);
    self.allocator.free(self.pack_nodes);
    self.allocator.destroy(self);
}

/// Attempts to pack and upload a batch of images into the atlas.
///
/// - `self`: A pointer to the Atlas state.
/// - `images`: A slice of `InputImage` structs to add to the atlas.
/// - `out_rects`: A slice of `stbrp.Rect` of the same length as `images`.
///   On return, this will be filled with the positions of the packed images.
///   The caller is responsible for converting these pixel coordinates to UVs.
///
/// Returns a `PackResult` indicating how many images were successfully packed.
/// If `result.packed_count < images.len`, the atlas is considered full.
pub fn packAndUpload(
    self: *Atlas,
    images: []const InputImage,
    out_rects: []stbrp.Rect,
) !PackResult {
    if (images.len == 0) return .{ .packed_count = 0 };
    std.debug.assert(images.len == out_rects.len);

    // 1. Populate the rects array with dimensions for the packer.
    for (images, 0..) |image, i| {
        out_rects[i] = .{
            .id = @intCast(i),
            .w = @intCast(image.width),
            .h = @intCast(image.height),
        };
    }

    // 2. Run the packing algorithm.
    _ = stbrp.packRects(&self.pack_context, out_rects.ptr, @intCast(out_rects.len));

    // 3. Iterate through the results, upload packed images, and count successes.
    var packed_count: usize = 0;
    for (out_rects) |rect| {
        if (rect.was_packed.to()) {
            const image = images[@intCast(rect.id)];
            try uploadImage(self, image, @intCast(rect.x), @intCast(rect.y));
            packed_count += 1;
        }
    }

    if (packed_count != images.len) {
        log.warn("atlas is full. packed {d}/{d} images.", .{ packed_count, images.len });
    } else {
        log.debug("packed {d} images successfully.", .{packed_count});
    }

    return .{ .packed_count = packed_count };
}
