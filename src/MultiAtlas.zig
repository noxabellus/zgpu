//! Manages a collection of dynamically-growing texture atlases.
//!
//! This module acts as a caching layer between the renderer and the underlying
//! GPU textures. The renderer asks for an image's location via an `ImageId`.
//! If the image is not yet on the GPU, this module uses a callback to get its
//! pixel data, finds a free spot for it in one of its `Atlas` pages (creating
//! a new page if necessary), uploads the data, and caches the location for
//! future frames.

const MultiAtlas = @This();

const std = @import("std");
const wgpu = @import("wgpu");
const stbrp = @import("stbrp");
const stbi = @import("stbi");
const Atlas = @import("Atlas.zig");

const log = std.log.scoped(.multi_atlas);

test {
    log.debug("semantic analysis for MultiAtlas.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const ImageId = usize;

pub const ImageLocation = struct {
    atlas_index: usize,
    uv_rect: [4]f32,
};

pub const DataProvider = *const fn (image_id: ImageId, user_context: ?*anyopaque) ?Atlas.InputImage;

pub const ProviderContext = @import("Batch2D.zig").ProviderContext;

const PendingItem = struct {
    id: ImageId,
    chain: []const Atlas.InputImage, // owned by MultiAtlas
};

allocator: std.mem.Allocator,
device: wgpu.Device,
queue: wgpu.Queue,
atlases: std.ArrayList(*Atlas),
cache: std.AutoHashMapUnmanaged(ImageId, ImageLocation),
pending_items: std.ArrayList(PendingItem),
rect_buffer: std.ArrayList(stbrp.Rect),
atlas_width: u32,
atlas_height: u32,
mip_level_count: u32,

pub fn init(
    allocator: std.mem.Allocator,
    device: wgpu.Device,
    queue: wgpu.Queue,
    atlas_width: u32,
    atlas_height: u32,
    mip_level_count: u32,
) !*MultiAtlas {
    const self = try allocator.create(MultiAtlas);
    errdefer allocator.destroy(self);

    self.* = .{
        .allocator = allocator,
        .device = device,
        .queue = queue,
        .atlases = .empty,
        .cache = .empty,
        .pending_items = .empty,
        .rect_buffer = .empty,
        .atlas_width = atlas_width,
        .atlas_height = atlas_height,
        .mip_level_count = mip_level_count,
    };

    try self.addNewAtlas();
    log.info("multi-atlas system initialized with {d} mip levels.", .{mip_level_count});

    return self;
}

pub fn deinit(self: *MultiAtlas) void {
    for (self.pending_items.items) |item| {
        for (item.chain) |mip| self.allocator.free(mip.pixels);
        self.allocator.free(item.chain);
    }
    self.pending_items.deinit(self.allocator);

    for (self.atlases.items) |atlas| atlas.deinit();
    self.atlases.deinit(self.allocator);

    self.cache.deinit(self.allocator);
    self.rect_buffer.deinit(self.allocator);
    self.allocator.destroy(self);

    log.info("multi-atlas system deinitialized.", .{});
}

pub fn query(
    self: *MultiAtlas,
    id: ImageId,
    context: ProviderContext,
) !ImageLocation {
    if (self.cache.get(id)) |location| {
        return location;
    }

    for (self.pending_items.items) |item| {
        if (item.id == id) return error.ImageNotYetPacked;
    }

    const source_image = context.provider(id, context.user_context) orelse {
        log.err("data provider returned null for image id {any}", .{id});
        return error.InvalidImageId;
    };

    const chain = try generateMipChain(self, source_image);

    self.pending_items.append(self.allocator, .{ .id = id, .chain = chain }) catch |err| {
        for (chain) |mip| self.allocator.free(mip.pixels);
        self.allocator.free(chain);
        return err;
    };

    return error.ImageNotYetPacked;
}

pub fn flush(self: *MultiAtlas, context: ProviderContext) !void {
    if (self.pending_items.items.len == 0) return;
    log.debug("flushing {d} pending images...", .{self.pending_items.items.len});
    _ = context;

    while (self.pending_items.items.len > 0) {
        const current_atlas_idx = self.atlases.items.len - 1;
        const current_atlas = self.atlases.items[current_atlas_idx];

        const pending_count = self.pending_items.items.len;
        var chain_batch = try self.allocator.alloc(Atlas.InputMipChain, pending_count);
        defer self.allocator.free(chain_batch);

        for (self.pending_items.items, 0..) |item, i| {
            chain_batch[i] = .{ .mips = item.chain };
        }

        try self.rect_buffer.resize(self.allocator, pending_count);

        const result = try current_atlas.packAndUpload(chain_batch, self.rect_buffer.items);

        if (result.packed_count > 0) {
            var packed_indices = std.ArrayList(usize){};
            defer packed_indices.deinit(self.allocator);

            const UV_INSET: f32 = 0.0;

            for (self.rect_buffer.items) |rect| {
                if (!rect.was_packed.to()) continue;

                const original_index: usize = @intCast(rect.id);
                const item = self.pending_items.items[original_index];

                const u_0 = (@as(f32, @floatFromInt(rect.x)) + UV_INSET) / @as(f32, @floatFromInt(self.atlas_width));
                const v_0 = (@as(f32, @floatFromInt(rect.y)) + UV_INSET) / @as(f32, @floatFromInt(self.atlas_height));

                const u_1 = (@as(f32, @floatFromInt(rect.x + rect.w)) - UV_INSET) / @as(f32, @floatFromInt(self.atlas_width));
                const v_1 = (@as(f32, @floatFromInt(rect.y + rect.h)) - UV_INSET) / @as(f32, @floatFromInt(self.atlas_height));

                const location = ImageLocation{
                    .atlas_index = current_atlas_idx,
                    .uv_rect = .{ u_0, v_0, u_1, v_1 },
                };

                try self.cache.put(self.allocator, item.id, location);
                try packed_indices.append(self.allocator, original_index);
            }

            std.mem.sort(usize, packed_indices.items, {}, std.sort.desc(usize));
            for (packed_indices.items) |idx| {
                const item_to_remove = self.pending_items.items[idx];
                for (item_to_remove.chain) |mip| {
                    self.allocator.free(mip.pixels);
                }
                self.allocator.free(item_to_remove.chain);
                _ = self.pending_items.swapRemove(idx);
            }
        }

        if (self.pending_items.items.len > 0) {
            if (result.packed_count == 0) {
                const first_pending = self.pending_items.items[0].chain[0];
                log.err("single image ({d}x{d}) is too large to fit in a {d}x{d} atlas!", .{
                    first_pending.width,
                    first_pending.height,
                    self.atlas_width,
                    self.atlas_height,
                });
                return error.ImageTooLargeForAtlas;
            }
            try self.addNewAtlas();
        }
    }

    self.pending_items.clearRetainingCapacity();
    self.rect_buffer.clearRetainingCapacity();
}

pub fn getTextureView(self: *MultiAtlas, atlas_index: usize) ?wgpu.TextureView {
    if (atlas_index >= self.atlases.items.len) return null;
    return self.atlases.items[atlas_index].view;
}

pub fn debugWriteAllAtlasesToPng(self: *MultiAtlas, base_filename: []const u8) !void {
    var file_buffer: [128]u8 = undefined;
    for (self.atlases.items, 0..) |atlas, i| {
        const filename = try std.fmt.bufPrint(&file_buffer, "{s}_{d}.png", .{ base_filename, i });
        try atlas.debugWriteToPng(filename);
    }
}

fn addNewAtlas(self: *MultiAtlas) !void {
    const new_atlas = try Atlas.init(
        self.allocator,
        self.device,
        self.queue,
        self.atlas_width,
        self.atlas_height,
        self.mip_level_count,
    );
    try self.atlases.append(self.allocator, new_atlas);
}

// Private helper to generate a mip chain from a single source image.
fn generateMipChain(self: *MultiAtlas, source: Atlas.InputImage) ![]const Atlas.InputImage {
    var chain = std.ArrayList(Atlas.InputImage).empty;
    errdefer {
        for (chain.items) |item| self.allocator.free(item.pixels);
        chain.deinit(self.allocator);
    }

    const source_copy = try self.allocator.dupe(u8, source.pixels);

    chain.append(self.allocator, .{
        .pixels = source_copy,
        .width = source.width,
        .height = source.height,
        .format = source.format,
    }) catch |err| {
        self.allocator.free(source_copy);
        return err;
    };

    var current_w = source.width;
    var current_h = source.height;
    var last_pixels = source_copy;

    // This loop generates smaller mip levels until the image is 1x1,
    // or until we've generated the number of mips requested during initialization.
    while (@max(current_w, current_h) > 1 and chain.items.len < self.mip_level_count) {
        const next_w = @max(1, current_w / 2);
        const next_h = @max(1, current_h / 2);

        const resized_pixels = try self.allocator.alloc(u8, next_w * next_h * 4);
        stbi.stbir_resize_uint8_srgb(last_pixels.ptr, @intCast(current_w), @intCast(current_h), 0, resized_pixels.ptr, @intCast(next_w), @intCast(next_h), 0, 4);

        try chain.append(self.allocator, .{
            .pixels = resized_pixels,
            .width = next_w,
            .height = next_h,
            .format = .rgba,
        });

        current_w = next_w;
        current_h = next_h;
        last_pixels = resized_pixels;
    }

    return chain.toOwnedSlice(self.allocator);
}
