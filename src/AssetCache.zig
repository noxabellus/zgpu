//! Simple asset loading and caching; currently supports images and fonts.
//! Acts as a provider for the Atlas used by Batch2D.

const std = @import("std");
const stbi = @import("stbi");
const stbtt = @import("stbtt");
const Atlas = @import("Atlas.zig");

const log = std.log.scoped(.asset_cache);

test {
    log.debug("semantic analysis for AssetCache.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const FontId = u8; // Max 256 fonts.

pub const FontSize = u16; // Max 65535 pixel height.

/// A packed struct representing the components of a glyph or special (e.g. white pixel) ImageID.
/// This allows for type-safe, error-free conversion to and from a u64 ImageID via @bitCast.
pub const GlyphId = packed struct(u64) {
    char_code: u21,
    font_size: FontSize,
    font_id: FontId,
    _reserved: u18,
    /// If true, this ID refers to a glyph or special ID that requires nearest-neighbor filtering.
    /// If false, it's a standard image ID.
    is_glyph_or_special: bool,
};

/// Encodes a GlyphId struct into a u64 ImageID.
pub fn encodeGlyphId(id_struct: GlyphId) Atlas.ImageId {
    return @bitCast(id_struct);
}

/// Decodes a u64 ImageID back into a GlyphId struct.
pub fn decodeGlyphId(id: Atlas.ImageId) GlyphId {
    return @bitCast(id);
}

/// A font index reserved for special, non-font IDs (like the white pixel).
pub const SPECIAL_ID_font_id: FontId = 0xFF;

/// The canonical ID for a single white pixel, used for drawing solid-colored shapes.
pub const WHITE_PIXEL_ID: Atlas.ImageId = encodeGlyphId(.{
    .char_code = 0,
    .font_size = 0,
    .font_id = SPECIAL_ID_font_id,
    ._reserved = 0,
    .is_glyph_or_special = true,
});

pub const LoadedFont = struct {
    info: stbtt.FontInfo,
    data: []const u8,
};

// NOTE: ProviderUserContext has been removed.

/// Manages loading and caching of assets like images and fonts.
pub const AssetCache = @This();

allocator: std.mem.Allocator,
fonts: std.ArrayList(LoadedFont),
images: std.ArrayList(stbi.Image),
image_map: std.StringHashMapUnmanaged(Atlas.ImageId),

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
pub fn loadFont(self: *AssetCache, path: []const u8) !FontId {
    std.debug.assert(self.fonts.items.len < std.math.maxInt(FontId));

    const font_data = try std.fs.cwd().readFileAlloc(self.allocator, path, 10 * 1024 * 1024);
    errdefer self.allocator.free(font_data);

    var font_info = stbtt.FontInfo{};
    const success = stbtt.initFont(&font_info, font_data.ptr, 0).to();
    std.debug.assert(success);

    const font_id: FontId = @intCast(self.fonts.items.len);
    try self.fonts.append(self.allocator, .{ .data = font_data, .info = font_info });
    log.info("loaded font '{s}' with index {d}", .{ path, font_id });
    return font_id;
}

/// Loads an image from a file path and returns a unique Atlas.ImageId.
/// Avoids loading the same image twice.
pub fn loadImage(self: *AssetCache, path: []const u8) !Atlas.ImageId {
    if (self.image_map.get(path)) |existing_id| {
        return existing_id;
    }

    const file_data = try std.fs.cwd().readFileAlloc(self.allocator, path, 10 * 1024 * 1024); // 10MB max
    defer self.allocator.free(file_data);

    var image = try stbi.Image.loadFromMemory(file_data, 4);
    errdefer image.deinit();

    const image_id: Atlas.ImageId = @intCast(self.images.items.len);
    try self.images.append(self.allocator, image);

    try self.image_map.put(self.allocator, try self.allocator.dupe(u8, path), image_id);

    log.info("loaded image '{s}' with id {d}", .{ path, image_id });
    return image_id;
}

/// The data provider callback required by the Atlas.
/// This function is called by the renderer when it needs pixel data for an Atlas.ImageId.
pub fn dataProvider(image_id: Atlas.ImageId, context: Atlas.ProviderContext) ?Atlas.InputImage {
    // Cast the opaque user_context pointer to our specific AssetCache struct.
    const cache: *const AssetCache = @ptrCast(@alignCast(context.user_context.?));
    const frame_allocator = context.frame_allocator;
    const decoded = decodeGlyphId(image_id);

    if (decoded.is_glyph_or_special) { // This is a glyph or special ID request.
        if (decoded.font_id == SPECIAL_ID_font_id) {
            // This is a special, non-font ID. The white pixel is char_code 0.
            if (decoded.char_code == 0) {
                const white_pixel = (frame_allocator.alloc(u8, 4) catch return null);
                @memcpy(white_pixel, &[_]u8{ 255, 255, 255, 255 });
                return Atlas.InputImage{
                    .pixels = white_pixel,
                    .width = 1,
                    .height = 1,
                    .format = .rgba,
                };
            }
            log.err("unknown special id {any}", .{decoded});
            return null;
        }

        // --- Otherwise, it's a regular glyph request ---
        if (decoded.font_id >= cache.fonts.items.len) {
            log.err("invalid font index {d} in glyph id", .{decoded.font_id});
            return null;
        }
        const font = &cache.fonts.items[decoded.font_id];
        const pixel_height_f32 = @as(f32, @floatFromInt(decoded.font_size));
        const scale = stbtt.scaleForPixelHeight(&font.info, pixel_height_f32);

        var w: i32 = 0;
        var h: i32 = 0;
        var xoff: i32 = 0;
        var yoff: i32 = 0;
        const grayscale_pixels = stbtt.getCodepointBitmap(&font.info, scale, scale, @intCast(decoded.char_code), &w, &h, &xoff, &yoff);

        if (grayscale_pixels == null or w == 0 or h == 0) return null;
        defer stbtt.freeBitmap(grayscale_pixels.?, null);

        const padded_w: u32 = @as(u32, @intCast(w)) + (@import("Batch2D.zig").GLYPH_PADDING * 2);
        const padded_h: u32 = @as(u32, @intCast(h)) + (@import("Batch2D.zig").GLYPH_PADDING * 2);
        const master_rgba_padded_pixels = (frame_allocator.alloc(u8, padded_w * padded_h * 4) catch return null);

        @memset(master_rgba_padded_pixels, 0x00);
        // Set RGB to 255 and alpha to 0 by default, then fill alpha from grayscale.
        for (0..padded_w * padded_h) |i| {
            const base = i * 4;
            master_rgba_padded_pixels[base + 0] = 255;
            master_rgba_padded_pixels[base + 1] = 255;
            master_rgba_padded_pixels[base + 2] = 255;
            master_rgba_padded_pixels[base + 3] = 0;
        }

        const original_pitch: usize = @intCast(w);
        const padded_pitch: usize = padded_w * 4;
        const glyph_padding = @import("Batch2D.zig").GLYPH_PADDING;
        for (0..@as(usize, @intCast(h))) |row| {
            const src_row = grayscale_pixels.?[(row * original_pitch)..];
            const dst_row_start_idx = ((row + glyph_padding) * padded_pitch) + (glyph_padding * 4);
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
