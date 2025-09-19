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
const Atlas = @import("Atlas.zig");

const log = std.log.scoped(.multi_atlas);

test {
    log.debug("semantic analysis for MultiAtlas.zig", .{});
    std.testing.refAllDecls(@This());
}

/// A unique, stable identifier for a logical image or glyph.
/// For a UI system like Clay, this would be the pointer/word-sized value
/// it uses to identify an image resource. For fonts, it could be a hash
/// of the glyph, font, and size.
pub const ImageId = usize;

/// The physical location of a packed image on the GPU.
/// This is the data structure the renderer cares about.
pub const ImageLocation = struct {
    /// The index of the Atlas page (and thus the WGPU texture) this image resides in.
    /// The renderer uses this to select the correct wgpu.BindGroup.
    atlas_index: usize,

    /// The UV coordinates [u0, v0, u1, v1] within that atlas.
    uv_rect: [4]f32,
};

/// A function pointer that the MultiAtlas uses to request image data on demand.
/// When a new `ImageId` is queried, this callback is invoked to provide the
/// raw pixel data and dimensions for that image.
/// - `image_id`: The ID that was requested.
/// - `user_context`: A user-provided pointer, passed through from the `query` call.
/// Returns an `Atlas.InputImage` on success, or `null` if the ID is invalid.
pub const DataProvider = *const fn (image_id: ImageId, user_context: ?*anyopaque) ?Atlas.InputImage;

allocator: std.mem.Allocator,
device: wgpu.Device,
queue: wgpu.Queue,

/// We own and manage a list of individual atlas pages.
atlases: std.array_list.Managed(*Atlas),

/// The central cache that maps a user-provided ID to a physical location.
cache: std.AutoHashMap(ImageId, ImageLocation),

/// A temporary list of images that were requested this frame but
/// haven't been packed and uploaded yet.
pending_images: std.array_list.Managed(struct {
    id: ImageId,
    data: Atlas.InputImage,
}),

/// A reusable buffer for getting packing results from individual atlases.
rect_buffer: std.array_list.Managed(stbrp.Rect),

/// The dimensions to use for any *new* atlas pages we create.
atlas_width: u32,
atlas_height: u32,

/// Initializes a new MultiAtlas manager.
///
/// - `allocator`: Used for all internal memory management.
/// - `device`, `queue`: WGPU objects passed down to create Atlas pages.
/// - `atlas_width`, `atlas_height`: The dimensions to use for each atlas texture page.
pub fn init(
    allocator: std.mem.Allocator,
    device: wgpu.Device,
    queue: wgpu.Queue,
    atlas_width: u32,
    atlas_height: u32,
) !*MultiAtlas {
    const self = try allocator.create(MultiAtlas);
    errdefer allocator.destroy(self);

    self.* = .{
        .allocator = allocator,
        .device = device,
        .queue = queue,
        .atlases = .init(allocator),
        .cache = .init(allocator),
        .pending_images = .init(allocator),
        .rect_buffer = .init(allocator),
        .atlas_width = atlas_width,
        .atlas_height = atlas_height,
    };

    // A MultiAtlas must start with at least one page.
    try self.addNewAtlas();
    log.info("multi-atlas system initialized.", .{});

    return self;
}

/// Deinitializes the MultiAtlas, releasing all owned Atlas pages and memory.
pub fn deinit(self: *MultiAtlas) void {
    for (self.atlases.items) |atlas| {
        atlas.deinit();
    }
    self.atlases.deinit();
    self.cache.deinit();
    self.pending_images.deinit();
    self.rect_buffer.deinit();
    self.allocator.destroy(self);
    log.info("multi-atlas system deinitialized.", .{});
}

/// The main query function for the renderer. It attempts to find the location
/// of an image.
///
/// If the image is already packed, its `ImageLocation` is returned immediately.
/// If not, it's added to a pending list to be processed by `flush()`,
/// and `error.ImageNotYetPacked` is returned.
pub fn query(
    self: *MultiAtlas,
    id: ImageId,
    provider: DataProvider,
    provider_context: ?*anyopaque,
) !ImageLocation {
    // 1. Fast path: Check the cache first for an instant hit.
    if (self.cache.get(id)) |location| {
        return location;
    }

    // 2. Slow path: This is a new image.
    // Use the provider callback to get the image pixel data on demand.
    const image_data = provider(id, provider_context) orelse {
        log.err("data provider returned null for image id {any}", .{id});
        return error.InvalidImageId;
    };

    // Add it to the list of work to be done in flush().
    try self.pending_images.append(.{ .id = id, .data = image_data });

    // 3. Signal to the renderer that the image is not ready *yet*.
    // The renderer must call flush() and then query again later this frame.
    return error.ImageNotYetPacked;
}

/// Processes all pending images, packing them into atlases.
/// If the current atlas is full, a new one is automatically created.
/// This should be called by the renderer once per frame after the "discovery" pass.
pub fn flush(self: *MultiAtlas) !void {
    if (self.pending_images.items.len == 0) return;
    log.debug("flushing {d} pending images...", .{self.pending_images.items.len});

    // We keep trying to pack the remaining pending images until the list is empty.
    while (self.pending_images.items.len > 0) {
        const current_atlas_idx = self.atlases.items.len - 1;
        const current_atlas = self.atlases.items[current_atlas_idx];

        // Prepare the inputs for the Atlas.packAndUpload call.
        const pending_count = self.pending_images.items.len;
        var image_batch = try self.allocator.alloc(Atlas.InputImage, pending_count);
        defer self.allocator.free(image_batch);

        for (self.pending_images.items, 0..) |item, i| {
            image_batch[i] = item.data;
        }

        try self.rect_buffer.resize(pending_count);

        // Attempt to pack the batch into the current atlas.
        const result = try current_atlas.packAndUpload(image_batch, self.rect_buffer.items);

        if (result.packed_count > 0) {
            // Success! Some images were packed. Update our cache and remove
            // them from the pending list.
            var packed_indices = std.array_list.Managed(usize).init(self.allocator);
            defer packed_indices.deinit();

            for (self.rect_buffer.items[0..result.packed_count]) |rect| {
                if (!rect.was_packed.to()) continue;

                // The rect's ID corresponds to its original index in the batch.
                const original_index: usize = @intCast(rect.id);
                const item = self.pending_images.items[original_index];

                // Calculate UVs.
                const u_0 = @as(f32, @floatFromInt(rect.x)) / @as(f32, @floatFromInt(self.atlas_width));
                const v_0 = @as(f32, @floatFromInt(rect.y)) / @as(f32, @floatFromInt(self.atlas_height));
                const u_1 = @as(f32, @floatFromInt(rect.x + rect.w)) / @as(f32, @floatFromInt(self.atlas_width));
                const v_1 = @as(f32, @floatFromInt(rect.y + rect.h)) / @as(f32, @floatFromInt(self.atlas_height));

                // Create the final location object and add it to the cache.
                const location = ImageLocation{
                    .atlas_index = current_atlas_idx,
                    .uv_rect = .{ u_0, v_0, u_1, v_1 },
                };

                try self.cache.put(item.id, location);
                try packed_indices.append(original_index);
            }

            // To efficiently remove the packed items, we sort indices descending
            // and remove one by one from the back.
            std.mem.sort(usize, packed_indices.items, {}, std.sort.desc(usize));
            for (packed_indices.items) |idx| {
                _ = self.pending_images.swapRemove(idx);
            }
        }

        // If we still have pending images after the attempt, the atlas is full.
        // We must create a new one and let the loop try again.
        if (self.pending_images.items.len > 0) {
            if (result.packed_count == 0) {
                // This is a critical error: an image is too large to fit in a brand new empty atlas.
                const first_pending = self.pending_images.items[0];
                log.err("single image ({d}x{d}) is too large to fit in a {d}x{d} atlas!", .{
                    first_pending.data.width, first_pending.data.height, self.atlas_width, self.atlas_height,
                });
                return error.ImageTooLargeForAtlas;
            }
            try self.addNewAtlas();
        }
    }

    // Reset for the next frame.
    self.pending_images.clearRetainingCapacity();
    self.rect_buffer.clearRetainingCapacity();
}

/// Returns the WGPU texture view for a given atlas page. The renderer
/// will use this to create/update its bind groups.
pub fn getTextureView(self: *MultiAtlas, atlas_index: usize) ?wgpu.TextureView {
    if (atlas_index >= self.atlases.items.len) return null;
    return self.atlases.items[atlas_index].view;
}

/// Private helper to create a new, empty Atlas page and add it to our list.
fn addNewAtlas(self: *MultiAtlas) !void {
    log.info("atlas page {d} is full. creating new page...", .{self.atlases.items.len -| 1});
    const new_atlas = try Atlas.init(
        self.allocator,
        self.device,
        self.queue,
        self.atlas_width,
        self.atlas_height,
    );
    try self.atlases.append(new_atlas);
}
