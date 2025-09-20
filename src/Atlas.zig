//! Manages a dynamically-growing texture array containing atlased images and their mip levels.

const Atlas = @This();

const std = @import("std");
const wgpu = @import("wgpu");
const stbrp = @import("stbrp");
const stbi = @import("stbi");

const log = std.log.scoped(.multi_atlas);

pub const ImageId = u32;

pub const MAX_MIP_LEVELS = 12;

pub const MipInfo = extern struct {
    uv_rect: [4]f32,
    atlas_layer_index: u32,
    _p1: u32,
    _p2: u32,
    _p3: u32,
};

pub const ImageMipData = extern struct {
    mips: [MAX_MIP_LEVELS]MipInfo,
};

pub const CachedImageInfo = struct {
    indirection_table_index: u32,
};

pub const DataProvider = *const fn (image_id: ImageId, user_context: ?*anyopaque) ?InputImage;

pub const ProviderContext = struct {
    provider: DataProvider,
    user_context: ?*anyopaque,
};

pub const PixelFormat = enum {
    grayscale,
    rgba,
};

pub const InputImage = struct {
    pixels: []const u8,
    width: u32,
    height: u32,
    format: PixelFormat,
};

const PendingItem = struct {
    id: ImageId,
    chain: []const InputImage,
};

allocator: std.mem.Allocator,
device: wgpu.Device,
queue: wgpu.Queue,
texture: wgpu.Texture,
view: wgpu.TextureView,
pack_contexts: std.ArrayList(stbrp.Context),
pack_nodes: std.ArrayList([]stbrp.Node),
cache: std.AutoHashMapUnmanaged(ImageId, CachedImageInfo),
pending_items: std.ArrayList(PendingItem),
atlas_width: u32,
atlas_height: u32,
mip_level_count: u32,
indirection_table: std.ArrayList(ImageMipData),

pub fn init(
    allocator: std.mem.Allocator,
    device: wgpu.Device,
    queue: wgpu.Queue,
    atlas_width: u32,
    atlas_height: u32,
    mip_level_count: u32,
) !*Atlas {
    const self = try allocator.create(Atlas);
    errdefer allocator.destroy(self);

    self.* = .{
        .allocator = allocator,
        .device = device,
        .queue = queue,
        .texture = undefined,
        .view = undefined,
        .pack_contexts = .empty,
        .pack_nodes = .empty,
        .cache = .empty,
        .pending_items = .empty,
        .atlas_width = atlas_width,
        .atlas_height = atlas_height,
        .mip_level_count = mip_level_count,
        .indirection_table = .empty,
    };

    try self.createTextureArray(2);
    log.info("multi-atlas system initialized.", .{});
    return self;
}

pub fn deinit(self: *Atlas) void {
    for (self.pending_items.items) |item| {
        for (item.chain) |mip| self.allocator.free(mip.pixels);
        self.allocator.free(item.chain);
    }
    self.pending_items.deinit(self.allocator);

    wgpu.textureViewRelease(self.view);
    wgpu.textureRelease(self.texture);

    for (self.pack_nodes.items) |nodes| self.allocator.free(nodes);
    self.pack_nodes.deinit(self.allocator);
    self.pack_contexts.deinit(self.allocator);

    self.cache.deinit(self.allocator);
    self.indirection_table.deinit(self.allocator);
    self.allocator.destroy(self);
    log.info("multi-atlas system deinitialized.", .{});
}

pub fn query(
    self: *Atlas,
    id: ImageId,
    generate_mips: bool,
    context: ProviderContext,
) !CachedImageInfo {
    if (self.cache.get(id)) |info| {
        return info;
    }

    for (self.pending_items.items) |item| {
        if (item.id == id) return error.ImageNotYetPacked;
    }

    const source_image = context.provider(id, context.user_context) orelse {
        log.err("data provider returned null for image id {any}", .{id});
        return error.InvalidImageId;
    };

    var max_mips: u32 = if (generate_mips) self.mip_level_count else 1;
    if (source_image.width <= 4 or source_image.height <= 4) {
        max_mips = 1;
    }

    const chain = try self.generateMipChain(source_image, max_mips);

    // Try to append. If it fails, clean up the chain and propagate the error.
    if (self.pending_items.append(self.allocator, .{ .id = id, .chain = chain })) {
        // On success, return the intended error to signal the image is now pending.
        return error.ImageNotYetPacked;
    } else |err| {
        for (chain) |mip| self.allocator.free(mip.pixels);
        self.allocator.free(chain);
        return err;
    }
}

pub fn flush(self: *Atlas, context: ProviderContext) !bool {
    if (self.pending_items.items.len == 0) return false;
    log.debug("flushing {d} pending images...", .{self.pending_items.items.len});
    _ = context;

    var atlas_recreated = false;
    var pending_idx: usize = 0;
    while (pending_idx < self.pending_items.items.len) {
        const item = self.pending_items.items[pending_idx];

        const gop = try self.cache.getOrPut(self.allocator, item.id);
        if (!gop.found_existing) {
            const new_index: u32 = @intCast(self.indirection_table.items.len);
            try self.indirection_table.append(self.allocator, std.mem.zeroes(ImageMipData));
            gop.value_ptr.* = .{ .indirection_table_index = new_index };
        }
        const cache_info = gop.value_ptr.*;
        const ind_entry = &self.indirection_table.items[@as(usize, @intCast(cache_info.indirection_table_index))];

        var mips_packed = std.ArrayList(bool).initCapacity(self.allocator, item.chain.len) catch @panic("oom");
        @memset(mips_packed.items, false);
        defer mips_packed.deinit(self.allocator);

        for (item.chain, 0..) |*mip, mip_level| {
            var rect = stbrp.Rect{ .id = 0, .w = @intCast(mip.width), .h = @intCast(mip.height) };
            var is_packed = false;
            var packed_layer_idx: u32 = 0;

            for (self.pack_contexts.items, 0..) |*pack_ctx, layer_idx| {
                _ = stbrp.packRects(pack_ctx, @ptrCast(&rect), 1);
                if (rect.was_packed.to()) {
                    try self.uploadImage(mip.*, @intCast(rect.x), @intCast(rect.y), @intCast(layer_idx));
                    is_packed = true;
                    packed_layer_idx = @intCast(layer_idx);
                    break;
                }
            }

            if (!is_packed) {
                const old_layer_count: u32 = @intCast(self.pack_contexts.items.len);
                try self.growTextureArray(old_layer_count * 2);
                atlas_recreated = true;
                const new_layer_idx = old_layer_count;
                _ = stbrp.packRects(&self.pack_contexts.items[new_layer_idx], @ptrCast(&rect), 1);
                std.debug.assert(rect.was_packed.to());
                try self.uploadImage(mip.*, @intCast(rect.x), @intCast(rect.y), new_layer_idx);
                is_packed = true;
                packed_layer_idx = new_layer_idx;
            }

            const u_min = @as(f32, @floatFromInt(rect.x)) / @as(f32, @floatFromInt(self.atlas_width));
            const v_min = @as(f32, @floatFromInt(rect.y)) / @as(f32, @floatFromInt(self.atlas_height));
            const u_max = @as(f32, @floatFromInt(rect.x + rect.w)) / @as(f32, @floatFromInt(self.atlas_width));
            const v_max = @as(f32, @floatFromInt(rect.y + rect.h)) / @as(f32, @floatFromInt(self.atlas_height));
            ind_entry.mips[mip_level] = .{
                .uv_rect = .{ u_min, v_min, u_max - u_min, v_max - v_min },
                .atlas_layer_index = packed_layer_idx,
                ._p1 = 0,
                ._p2 = 0,
                ._p3 = 0,
            };
        }

        if (item.chain.len == 1) {
            const mip0_info = ind_entry.mips[0];
            for (&ind_entry.mips) |*mip| mip.* = mip0_info;
        }
        pending_idx += 1;
    }

    for (self.pending_items.items) |item| {
        for (item.chain) |mip| self.allocator.free(mip.pixels);
        self.allocator.free(item.chain);
    }
    self.pending_items.clearRetainingCapacity();

    return atlas_recreated;
}

fn createTextureArray(self: *Atlas, layer_count: u32) !void {
    const texture_descriptor = wgpu.TextureDescriptor{
        .label = .fromSlice("texture_atlas_array"),
        .size = .{ .width = self.atlas_width, .height = self.atlas_height, .depth_or_array_layers = layer_count },
        .mip_level_count = 1,
        .sample_count = 1,
        .dimension = .@"2d",
        .format = .rgba8_unorm_srgb,
        .usage = wgpu.TextureUsage{ .texture_binding = true, .copy_dst = true, .copy_src = true },
    };
    self.texture = wgpu.deviceCreateTexture(self.device, &texture_descriptor);
    std.debug.assert(self.texture != null);

    const view_descriptor = wgpu.TextureViewDescriptor{
        .label = .fromSlice("atlas_array_texture_view"),
        .format = .rgba8_unorm_srgb,
        .dimension = .@"2d_array",
        .base_array_layer = 0,
        .array_layer_count = layer_count,
    };
    self.view = wgpu.textureCreateView(self.texture, &view_descriptor);
    std.debug.assert(self.view != null);

    try self.pack_contexts.resize(self.allocator, layer_count);
    const old_nodes_len = self.pack_nodes.items.len;
    try self.pack_nodes.resize(self.allocator, layer_count);

    const num_nodes_per_layer: u32 = self.atlas_width;
    for (old_nodes_len..layer_count) |i| {
        self.pack_nodes.items[i] = try self.allocator.alloc(stbrp.Node, num_nodes_per_layer);
        stbrp.initTarget(&self.pack_contexts.items[i], @intCast(self.atlas_width), @intCast(self.atlas_height), self.pack_nodes.items[i].ptr, @intCast(num_nodes_per_layer));
    }
}

fn growTextureArray(self: *Atlas, new_layer_count: u32) !void {
    log.warn("growing atlas texture array to {d} layers. this may cause a stutter.", .{new_layer_count});
    const old_texture = self.texture;
    const old_view = self.view;
    const old_layer_count: u32 = @intCast(self.pack_contexts.items.len);

    try self.createTextureArray(new_layer_count);

    const encoder = wgpu.deviceCreateCommandEncoder(self.device, &.{ .label = .fromSlice("atlas_copy_encoder") });
    defer wgpu.commandEncoderRelease(encoder);

    wgpu.commandEncoderCopyTextureToTexture(encoder, &.{ .texture = old_texture }, &.{ .texture = self.texture }, &.{ .width = self.atlas_width, .height = self.atlas_height, .depth_or_array_layers = old_layer_count });

    const cmd = wgpu.commandEncoderFinish(encoder, null);
    defer wgpu.commandBufferRelease(cmd);
    wgpu.queueSubmit(self.queue, 1, &.{cmd});

    wgpu.textureViewRelease(old_view);
    wgpu.textureRelease(old_texture);
}

fn uploadImage(self: *Atlas, image: InputImage, x: u32, y: u32, layer: u32) !void {
    const copy_destination = wgpu.TexelCopyTextureInfo{
        .texture = self.texture,
        .origin = .{ .x = x, .y = y, .z = layer },
    };
    const copy_size = wgpu.Extent3D{
        .width = image.width,
        .height = image.height,
        .depth_or_array_layers = 1,
    };
    const layout = wgpu.TexelCopyBufferLayout{
        .bytes_per_row = image.width * 4,
        .rows_per_image = image.height,
    };
    wgpu.queueWriteTexture(self.queue, &copy_destination, image.pixels.ptr, image.pixels.len, &layout, &copy_size);
}

fn generateMipChain(self: *Atlas, source: InputImage, max_levels: u32) ![]const InputImage {
    if (max_levels <= 1) {
        const slice = try self.allocator.alloc(InputImage, 1);
        slice[0] = .{ .pixels = try self.allocator.dupe(u8, source.pixels), .width = source.width, .height = source.height, .format = source.format };
        return slice;
    }
    var chain = std.ArrayList(InputImage).empty;
    errdefer {
        for (chain.items) |item| self.allocator.free(item.pixels);
        chain.deinit(self.allocator);
    }
    const source_copy = try self.allocator.dupe(u8, source.pixels);
    errdefer self.allocator.free(source_copy);
    try chain.append(self.allocator, .{ .pixels = source_copy, .width = source.width, .height = source.height, .format = source.format });

    var current_w = source.width;
    var current_h = source.height;
    var last_pixels = source_copy;
    while (@max(current_w, current_h) > 1 and chain.items.len < max_levels) {
        const next_w = @max(1, current_w / 2);
        const next_h = @max(1, current_h / 2);
        const resized_pixels = try self.allocator.alloc(u8, next_w * next_h * 4);
        stbi.stbir_resize_uint8_srgb(last_pixels.ptr, @intCast(current_w), @intCast(current_h), 0, resized_pixels.ptr, @intCast(next_w), @intCast(next_h), 0, 4);
        try chain.append(self.allocator, .{ .pixels = resized_pixels, .width = next_w, .height = next_h, .format = .rgba });
        current_w = next_w;
        current_h = next_h;
        last_pixels = resized_pixels;
    }
    return chain.toOwnedSlice(self.allocator);
}

pub fn debugWriteAllAtlasesToPng(self: *Atlas, base_filename: []const u8) !void {
    var file_buffer: [128]u8 = undefined;
    for (0..self.pack_contexts.items.len) |i| {
        const filename = try std.fmt.bufPrint(&file_buffer, "zig-out/{s}_{d}.png", .{ base_filename, i });
        try debugWriteLayerToPng(self.device, self.queue, self.allocator, self.texture, @intCast(i), self.atlas_width, self.atlas_height, filename);
    }
}

pub fn debugWriteLayerToPng(
    device: wgpu.Device,
    queue: wgpu.Queue,
    allocator: std.mem.Allocator,
    texture: wgpu.Texture,
    layer: u32,
    width: u32,
    height: u32,
    filename: []const u8,
) !void {
    log.warn("writing atlas layer {d} to '{s}' for debugging...", .{ layer, filename });

    const bytes_per_pixel = 4;
    const buffer_stride = std.mem.alignForward(u32, width * bytes_per_pixel, 256);
    const aligned_buffer_size = buffer_stride * height;

    const readback_buffer = wgpu.deviceCreateBuffer(device, &.{
        .label = .fromSlice("debug_readback_buffer"),
        .usage = wgpu.BufferUsage{ .map_read = true, .copy_dst = true },
        .size = aligned_buffer_size,
    });
    if (readback_buffer == null) return error.DebugBufferCreationFailed;
    defer wgpu.bufferRelease(readback_buffer);

    const encoder = wgpu.deviceCreateCommandEncoder(device, &.{ .label = .fromSlice("debug_readback_encoder") });
    defer wgpu.commandEncoderRelease(encoder);

    wgpu.commandEncoderCopyTextureToBuffer(
        encoder,
        &.{ .texture = texture, .mip_level = 0, .origin = .{ .z = layer } },
        &.{ .buffer = readback_buffer, .layout = .{ .offset = 0, .bytes_per_row = buffer_stride, .rows_per_image = height } },
        &.{ .width = width, .height = height, .depth_or_array_layers = 1 },
    );

    const cmd = wgpu.commandEncoderFinish(encoder, null);
    defer wgpu.commandBufferRelease(cmd);
    wgpu.queueSubmit(queue, 1, &.{cmd});

    var map_finished = false;
    _ = wgpu.bufferMapAsync(readback_buffer, .readMode, 0, aligned_buffer_size, .{
        .callback = &struct {
            fn handle_map(status: wgpu.MapAsyncStatus, _: wgpu.StringView, u: ?*anyopaque, _: ?*anyopaque) callconv(.c) void {
                if (status == .success) {
                    const finished_flag: *bool = @ptrCast(@alignCast(u.?));
                    finished_flag.* = true;
                }
            }
        }.handle_map,
        .userdata1 = &map_finished,
    });

    while (!map_finished) {
        _ = wgpu.devicePoll(device, .from(true), null);
    }

    const mapped_range: [*]const u8 = @ptrCast(@alignCast(wgpu.bufferGetMappedRange(readback_buffer, 0, aligned_buffer_size).?));
    const cpu_pixels = try allocator.alloc(u8, width * height * 4);
    defer allocator.free(cpu_pixels);

    for (0..height) |row| {
        const src_start = row * buffer_stride;
        const dst_start = row * width * bytes_per_pixel;
        const row_bytes = width * bytes_per_pixel;
        @memcpy(cpu_pixels[dst_start .. dst_start + row_bytes], mapped_range[src_start .. src_start + row_bytes]);
    }

    const image = stbi.Image{
        .data = cpu_pixels,
        .width = width,
        .height = height,
        .num_components = 4,
        .bytes_per_component = 1,
        .bytes_per_row = width * 4,
        .is_hdr = false,
    };
    const filename_z = try allocator.dupeZ(u8, filename);
    defer allocator.free(filename_z);
    image.writeToFile(filename_z, .png) catch |err| {
        log.err("failed to write atlas to png: {any}", .{err});
    };
    wgpu.bufferUnmap(readback_buffer);
}
