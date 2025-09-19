//! Manages a single GPU texture as a dynamic atlas for images and glyphs.
//!
//! This module is responsible for finding space for new images, uploading their
//! pixel data to the GPU, and reporting when it is full. It is designed to be
//! a single "page" in a larger multi-atlas system. All functions use a
//! C-style API, taking a pointer to the Atlas state struct.

const Atlas = @This();

const std = @import("std");
const wgpu = @import("wgpu");
const stbi = @import("stbi");
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

/// Represents one image and all of its smaller mipmap levels.
pub const InputMipChain = struct {
    mips: []const InputImage,
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
mip_level_count: u32,

pack_context: stbrp.Context,
pack_nodes: []stbrp.Node,

// --- Private Helper Functions ---

/// Private helper to upload a single image, handling pixel format conversion.
fn uploadImage(
    self: *Atlas,
    image: InputImage,
    x: u32,
    y: u32,
    mip_level: u32,
) !void {
    const copy_destination = wgpu.TexelCopyTextureInfo{
        .texture = self.texture,
        .mip_level = mip_level,
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
    mip_level_count: u32,
) !*Atlas {
    log.info("initializing atlas with size {d}x{d} and {d} mip levels", .{ width, height, mip_level_count });
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
        .mip_level_count = mip_level_count,
        .sample_count = 1,
        .dimension = .@"2d",
        .format = .rgba8_unorm_srgb,
        .usage = wgpu.TextureUsage{ .texture_binding = true, .copy_dst = true, .copy_src = true }, // need copy_src for debug readback
    };
    const texture = wgpu.deviceCreateTexture(device, &texture_descriptor);
    if (texture == null) {
        log.err("failed to create WGPU texture for atlas", .{});
        allocator.free(pack_nodes);
        allocator.destroy(self);
        return error.WgpuTextureCreationFailed;
    }
    errdefer wgpu.textureRelease(texture);

    const texture_view_descriptor = wgpu.TextureViewDescriptor{
        .label = .fromSlice("atlas_texture_view"),
        .format = .rgba8_unorm_srgb,
        .dimension = .@"2d",
        .base_mip_level = 0,
        .mip_level_count = mip_level_count, // Explicitly include all mips
        .base_array_layer = 0,
        .array_layer_count = 1,
    };

    const view = wgpu.textureCreateView(texture, &texture_view_descriptor);
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
        .mip_level_count = mip_level_count,
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

/// Attempts to pack and upload a batch of images (with their mip chains) into the atlas.
///
/// - `self`: A pointer to the Atlas state.
/// - `chains`: A slice of `InputMipChain` structs to add to the atlas.
/// - `out_rects`: A slice of `stbrp.Rect` of the same length as `chains`.
///   On return, this will be filled with the positions of the packed images at mip level 0.
///
/// Returns a `PackResult` indicating how many images were successfully packed.
/// If `result.packed_count < chains.len`, the atlas is considered full.
pub fn packAndUpload(
    self: *Atlas,
    chains: []const InputMipChain,
    out_rects: []stbrp.Rect,
) !PackResult {
    if (chains.len == 0) return .{ .packed_count = 0 };
    std.debug.assert(chains.len == out_rects.len);

    // 1. Populate the rects array with dimensions for the packer using only mip level 0.
    for (chains, 0..) |chain, i| {
        const base_image = chain.mips[0];
        out_rects[i] = .{
            .id = @intCast(i),
            .w = @intCast(base_image.width),
            .h = @intCast(base_image.height),
        };
    }

    // 2. Run the packing algorithm on mip level 0.
    _ = stbrp.packRects(&self.pack_context, out_rects.ptr, @intCast(out_rects.len));

    // 3. Iterate through the results, upload the full mip chain for packed images, and count successes.
    var packed_count: usize = 0;
    for (out_rects) |rect| {
        if (rect.was_packed.to()) {
            const chain = chains[@intCast(rect.id)];
            std.debug.assert(chain.mips.len <= self.mip_level_count);

            // Upload each mip level, calculating its position from the base level rect.
            for (chain.mips, 0..) |mip_image, mip_level| {
                const divisor: u32 = @as(u32, 1) << @intCast(mip_level);
                const mip_x = @as(u32, @intCast(rect.x)) / divisor;
                const mip_y = @as(u32, @intCast(rect.y)) / divisor;
                try uploadImage(self, mip_image, mip_x, mip_y, @intCast(mip_level));
            }
            packed_count += 1;
        }
    }

    if (packed_count != chains.len) {
        log.warn("atlas is full. packed {d}/{d} images.", .{ packed_count, chains.len });
    } else {
        log.debug("packed {d} images successfully.", .{packed_count});
    }

    return .{ .packed_count = packed_count };
}

/// (DEBUG) Writes the current content of the GPU texture atlas to a PNG file.
/// This is a slow, blocking operation intended only for debugging.
pub fn debugWriteToPng(self: *Atlas, filename: []const u8) !void {
    log.warn("writing atlas to '{s}' for debugging...", .{filename});

    const bytes_per_pixel = 4;
    const buffer_size = self.width * self.height * bytes_per_pixel;

    // WGPU requires buffer-texture copies to have bytes_per_row aligned to 256.
    const buffer_stride = std.mem.alignForward(u32, self.width * bytes_per_pixel, 256);
    const aligned_buffer_size = buffer_stride * self.height;

    // 1. Create a mappable buffer on the GPU to be the destination for the texture copy.
    const readback_buffer = wgpu.deviceCreateBuffer(self.device, &.{
        .label = .fromSlice("debug_readback_buffer"),
        .usage = wgpu.BufferUsage{ .map_read = true, .copy_dst = true },
        .size = aligned_buffer_size,
    });
    if (readback_buffer == null) return error.DebugBufferCreationFailed;
    defer wgpu.bufferRelease(readback_buffer);

    // 2. Create a command encoder to issue the copy command.
    const encoder = wgpu.deviceCreateCommandEncoder(self.device, &.{ .label = .fromSlice("debug_readback_encoder") });
    defer wgpu.commandEncoderRelease(encoder);

    // 3. Command: Copy from the atlas texture to our new buffer.
    wgpu.commandEncoderCopyTextureToBuffer(
        encoder,
        &.{ .texture = self.texture, .mip_level = 0, .origin = .{} },
        &.{ .buffer = readback_buffer, .layout = .{ .offset = 0, .bytes_per_row = buffer_stride, .rows_per_image = self.height } },
        &.{ .width = self.width, .height = self.height, .depth_or_array_layers = 1 },
    );

    const cmd = wgpu.commandEncoderFinish(encoder, null);
    defer wgpu.commandBufferRelease(cmd);

    // 4. Submit the command and wait for it to complete.
    wgpu.queueSubmit(self.queue, 1, &.{cmd});

    // 5. Asynchronously map the buffer and wait for the callback.
    var map_finished = false;
    _ = wgpu.bufferMapAsync(readback_buffer, .readMode, 0, aligned_buffer_size, .{
        .callback = &struct {
            fn handle_map(status: wgpu.MapAsyncStatus, _: wgpu.StringView, u: ?*anyopaque, _: ?*anyopaque) callconv(.c) void {
                if (status == .success) {
                    const finished_flag: *bool = @ptrCast(@alignCast(u.?));
                    finished_flag.* = true;
                } else {
                    log.err("failed to map debug readback buffer: {any}", .{status});
                }
            }
        }.handle_map,
        .userdata1 = &map_finished,
    });

    // This is a simple, blocking wait. In a real app, you'd want a better way to handle this.
    while (!map_finished) {
        // Yielding or sleeping would be better, but for a debug dump this is fine.
        _ = wgpu.devicePoll(self.device, .from(true), null);
    }

    // 6. Get the mapped data.
    const mapped_range: [*]const u8 = @ptrCast(@alignCast(wgpu.bufferGetMappedRange(readback_buffer, 0, aligned_buffer_size) orelse {
        log.err("mapped range for debug buffer was null", .{});
        return;
    }));

    // 7. The data is aligned. We need to create a new, contiguous buffer for stbi_write.
    const cpu_pixels = try self.allocator.alloc(u8, buffer_size);
    defer self.allocator.free(cpu_pixels);

    for (0..self.height) |row| {
        const src_start = row * buffer_stride;
        const dst_start = row * self.width * bytes_per_pixel;
        const row_bytes = self.width * bytes_per_pixel;
        @memcpy(cpu_pixels[dst_start .. dst_start + row_bytes], mapped_range[src_start .. src_start + row_bytes]);
    }

    // 8. Write to PNG.
    const image = stbi.Image{
        .data = cpu_pixels,
        .width = self.width,
        .height = self.height,
        .num_components = 4,
        .bytes_per_component = 1,
        .bytes_per_row = self.width * 4,
        .is_hdr = false,
    };

    const filename_z = try self.allocator.dupeZ(u8, filename);

    image.writeToFile(filename_z, .png) catch |err| {
        log.err("failed to write atlas to png: {any}", .{err});
    };

    // 9. Unmap and clean up.
    wgpu.bufferUnmap(readback_buffer);
    log.info("successfully wrote atlas to '{s}'", .{filename});
}
