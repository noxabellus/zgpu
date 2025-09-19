//! Simple asset loading and caching; currently supports images and fonts.
//! Acts as a provider for the MultiAtlas used by Batch2D.

const std = @import("std");
const stbi = @import("stbi");
const stbtt = @import("stbtt");

const Atlas = @import("Atlas.zig");
const Batch2D = @import("Batch2D.zig");

const log = std.log.scoped(.asset_cache);

pub const ImageId = @import("MultiAtlas.zig").ImageId;

pub const LoadedFont = struct {
    info: stbtt.FontInfo,
    data: []const u8,
};

/// The context object that our dataProvider expects to receive.
/// It bundles all application-specific data needed for asset lookups.
pub const ProviderUserContext = struct {
    asset_cache: *AssetCache,
    frame_allocator: std.mem.Allocator,
};

/// Manages loading and caching of assets like images and fonts.
pub const AssetCache = @This();

allocator: std.mem.Allocator,
fonts: std.ArrayList(LoadedFont),
images: std.ArrayList(stbi.Image),
image_map: std.StringHashMapUnmanaged(ImageId),

pub fn init(allocator: std.mem.Allocator) AssetCache {
    return .{
        .allocator = allocator,
        .fonts = .empty,
        .images = .empty,
        .image_map = .empty,
    };
}

pub fn deinit(self: *AssetCache) void {
    // Free fonts
    for (self.fonts.items) |font| {
        self.allocator.free(font.data);
    }
    self.fonts.deinit(self.allocator);

    // Free images
    var image_iter = self.image_map.iterator();
    while (image_iter.next()) |entry| {
        // Free the key (path) which was duplicated
        self.allocator.free(entry.key_ptr.*);
    }
    for (self.images.items) |*image| {
        image.deinit();
    }
    self.images.deinit(self.allocator);
    self.image_map.deinit(self.allocator);
}

/// Loads a font from a file path and returns its index.
pub fn loadFont(self: *AssetCache, path: []const u8) !u32 {
    const font_data = try std.fs.cwd().readFileAlloc(self.allocator, path, 10 * 1024 * 1024);
    errdefer self.allocator.free(font_data);

    var font_info = stbtt.FontInfo{};
    const success = stbtt.initFont(&font_info, font_data.ptr, 0).to();
    std.debug.assert(success);

    const font_index: u32 = @intCast(self.fonts.items.len);
    try self.fonts.append(self.allocator, .{ .data = font_data, .info = font_info });
    log.info("loaded font '{s}' with index {d}", .{ path, font_index });
    return font_index;
}

/// Loads an image from a file path and returns a unique ImageId.
/// Avoids loading the same image twice.
pub fn loadImage(self: *AssetCache, path: []const u8) !ImageId {
    if (self.image_map.get(path)) |existing_id| {
        return existing_id;
    }

    const file_data = try std.fs.cwd().readFileAlloc(self.allocator, path, 10 * 1024 * 1024); // 10MB max
    defer self.allocator.free(file_data);

    var image = try stbi.Image.loadFromMemory(file_data, 4);
    errdefer image.deinit();

    const image_id: ImageId = self.images.items.len;
    try self.images.append(self.allocator, image);

    try self.image_map.put(self.allocator, try self.allocator.dupe(u8, path), image_id);

    log.info("loaded image '{s}' with id {d}", .{ path, image_id });
    return image_id;
}

/// The data provider callback required by the MultiAtlas.
/// This function is called by the renderer when it needs pixel data for an ImageId.
pub fn dataProvider(image_id: ImageId, user_context: ?*anyopaque) ?Atlas.InputImage {
    // Cast the opaque pointer to our specific context struct.
    const ctx: *const ProviderUserContext = @ptrCast(@alignCast(user_context.?));
    const cache = ctx.asset_cache;
    const frame_allocator = ctx.frame_allocator;

    if ((image_id & Batch2D.GLYPH_ID_FLAG) != 0) { // This is a glyph request.
        const decoded = Batch2D.decodeGlyphId(image_id);
        if (decoded.font_index >= cache.fonts.items.len) {
            log.err("invalid font index {d} in glyph id", .{decoded.font_index});
            return null;
        }
        const font = &cache.fonts.items[decoded.font_index];
        const scale = stbtt.scaleForPixelHeight(&font.info, decoded.pixel_height);

        var w: i32 = 0;
        var h: i32 = 0;
        var xoff: i32 = 0;
        var yoff: i32 = 0;
        const grayscale_pixels = stbtt.getCodepointBitmap(&font.info, 0, scale, @intCast(decoded.char_code), &w, &h, &xoff, &yoff);

        if (grayscale_pixels == null or w == 0 or h == 0) return null;
        defer stbtt.freeBitmap(grayscale_pixels.?, null);

        const GLYPH_PADDING: u32 = 2;

        const padded_w: u32 = @as(u32, @intCast(w)) + (GLYPH_PADDING * 2);
        const padded_h: u32 = @as(u32, @intCast(h)) + (GLYPH_PADDING * 2);
        const master_rgba_padded_pixels = (frame_allocator.alloc(u8, padded_w * padded_h * 4) catch return null);

        @memset(master_rgba_padded_pixels, 0xFF);
        for (0..padded_w * padded_h) |i| {
            master_rgba_padded_pixels[i * 4 + 3] = 0;
        }

        const original_pitch: usize = @intCast(w);
        const padded_pitch: usize = padded_w * 4;
        for (0..@as(usize, @intCast(h))) |row| {
            const src_row = grayscale_pixels.?[(row * original_pitch)..];
            const dst_row_start_idx = ((row + GLYPH_PADDING) * padded_pitch) + (GLYPH_PADDING * 4);
            for (0..original_pitch) |col| {
                const alpha = src_row[col];
                const dst_pixel_start = dst_row_start_idx + (col * 4);
                master_rgba_padded_pixels[dst_pixel_start + 3] = alpha;
            }
        }
        return Atlas.InputImage{
            .pixels = master_rgba_padded_pixels,
            .width = padded_w,
            .height = padded_h,
            .format = .rgba,
        };
    } else { // This is a standard image request. The ID is the index in our images list.
        const image_index = @as(usize, @intCast(image_id));
        if (image_index >= cache.images.items.len) {
            log.err("invalid image id {d}", .{image_index});
            return null;
        }
        const image = &cache.images.items[image_index];
        return Atlas.InputImage{
            .pixels = image.data,
            .width = image.width,
            .height = image.height,
            .format = .rgba,
        };
    }
}
