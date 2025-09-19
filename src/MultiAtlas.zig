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

pub const ProviderResult = struct {
    chain: []const Atlas.InputImage,
};

pub const DataProvider = *const fn (image_id: ImageId, user_context: ?*anyopaque) ?ProviderResult;

// Renamed from PrepareContext for clarity.
pub const ProviderContext = @import("Batch2D.zig").ProviderContext;

const PendingItem = struct {
    id: ImageId,
    chain: []const Atlas.InputImage,
};

allocator: std.mem.Allocator,
device: wgpu.Device,
queue: wgpu.Queue,
atlases: std.array_list.Managed(*Atlas),
cache: std.AutoHashMap(ImageId, ImageLocation),
pending_items: std.array_list.Managed(PendingItem),
rect_buffer: std.array_list.Managed(stbrp.Rect),
atlas_width: u32,
atlas_height: u32,
pub const mip_level_count: u32 = 12; // Make this public for sampler setup

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
        .pending_items = .init(allocator),
        .rect_buffer = .init(allocator),
        .atlas_width = atlas_width,
        .atlas_height = atlas_height,
    };

    try self.addNewAtlas();
    log.info("multi-atlas system initialized.", .{});

    return self;
}

pub fn deinit(self: *MultiAtlas) void {
    for (self.atlases.items) |atlas| atlas.deinit();
    self.pending_items.deinit();
    self.atlases.deinit();
    self.cache.deinit();
    self.rect_buffer.deinit();
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

    const result = context.provider(id, context.user_context) orelse {
        log.err("data provider returned null for image id {any}", .{id});
        return error.InvalidImageId;
    };

    try self.pending_items.append(.{ .id = id, .chain = result.chain });

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

        try self.rect_buffer.resize(pending_count);

        const result = try current_atlas.packAndUpload(chain_batch, self.rect_buffer.items);

        if (result.packed_count > 0) {
            var packed_indices = std.array_list.Managed(usize).init(self.allocator);
            defer packed_indices.deinit();

            for (self.rect_buffer.items) |rect| {
                if (!rect.was_packed.to()) continue;

                const original_index: usize = @intCast(rect.id);
                const item = self.pending_items.items[original_index];

                var u_offset: f32 = 0.0;
                var v_offset: f32 = 0.0;
                var w_adjust: f32 = 0.0;
                var h_adjust: f32 = 0.0;

                if (item.chain[0].format == .grayscale) {
                    u_offset = 1.0;
                    v_offset = 1.0;
                    w_adjust = -1.0;
                    h_adjust = -1.0;
                }

                const u_0 = (@as(f32, @floatFromInt(rect.x)) + u_offset) / @as(f32, @floatFromInt(self.atlas_width));
                const v_0 = (@as(f32, @floatFromInt(rect.y)) + v_offset) / @as(f32, @floatFromInt(self.atlas_height));
                const u_1 = (@as(f32, @floatFromInt(rect.x + rect.w)) + w_adjust) / @as(f32, @floatFromInt(self.atlas_width));
                const v_1 = (@as(f32, @floatFromInt(rect.y + rect.h)) + h_adjust) / @as(f32, @floatFromInt(self.atlas_height));

                const location = ImageLocation{
                    .atlas_index = current_atlas_idx,
                    .uv_rect = .{ u_0, v_0, u_1, v_1 },
                };

                try self.cache.put(item.id, location);
                try packed_indices.append(original_index);
            }

            std.mem.sort(usize, packed_indices.items, {}, std.sort.desc(usize));
            for (packed_indices.items) |idx| {
                _ = self.pending_items.swapRemove(idx);
            }
        }

        if (self.pending_items.items.len > 0) {
            if (result.packed_count == 0) {
                const first_pending = self.pending_items.items[0].chain[0];
                log.err("single image ({d}x{d}) is too large to fit in a {d}x{d} atlas!", .{
                    first_pending.width, first_pending.height, self.atlas_width, self.atlas_height,
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

fn addNewAtlas(self: *MultiAtlas) !void {
    const new_atlas = try Atlas.init(
        self.allocator,
        self.device,
        self.queue,
        self.atlas_width,
        self.atlas_height,
        mip_level_count,
    );
    try self.atlases.append(new_atlas);
}
