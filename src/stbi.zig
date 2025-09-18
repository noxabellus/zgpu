const std = @import("std");
const log = std.log.scoped(.stbi);

test {
    log.debug("semantic analysis for stb_image.zig", .{});
    std.testing.refAllDecls(@This());
}

/// Describes the number and configuration of channels in an image.
pub const Channels = enum(i32) {
    default = 0,
    grey = 1,
    grey_alpha = 2,
    rgb = 3,
    rgba = 4,

    pub fn asInt(self: Channels) u32 {
        return @intCast(@intFromEnum(self));
    }
};

/// Callback-api vtable representing a file-like object api for custom I/O.
pub const IoCallbacks = extern struct {
    read: *const fn (?*anyopaque, [*c]u8, i32) callconv(.c) i32,
    skip: *const fn (?*anyopaque, i32) callconv(.c) void,
    eof: *const fn (?*anyopaque) callconv(.c) i32,
};

/// Buffer of 8-bit image data.
pub const RawImageBuffer8 = [*]u8;
/// Buffer of 16-bit image data.
pub const RawImageBuffer16 = [*]u16;
/// Buffer of 32-bit float image data.
pub const RawImageBufferF = [*]f32;

/// Slice of 8-bit image data.
pub const ImageBuffer8 = []u8;
/// Slice of 16-bit image data.
pub const ImageBuffer16 = []u16;
/// Slice of 32-bit float image data.
pub const ImageBufferF = []f32;

// Flags and settings //

/// Set the gamma factor for HDR to LDR conversion, when loading HDR images. Default value is `2.2`.
pub const hdrToLdrGamma = @extern(*const fn (gamma: f32) callconv(.c) void, .{ .name = "stbi_hdr_to_ldr_gamma" });
/// Set the scale factor for HDR to LDR conversion, when loading HDR images. Default value is `1.0`.
pub const hdrToLdrScale = @extern(*const fn (scale: f32) callconv(.c) void, .{ .name = "stbi_hdr_to_ldr_scale" });
/// Set the gamma factor for LDR to HDR conversion, when loading LDR images. Default value is `1.0 / 2.2`.
pub const ldrToHdrGamma = @extern(*const fn (gamma: f32) callconv(.c) void, .{ .name = "stbi_ldr_to_hdr_gamma" });
/// Set the scale factor for LDR to HDR conversion, when loading LDR images. Default value is `1.0`.
pub const ldrToHdrScale = @extern(*const fn (scale: f32) callconv(.c) void, .{ .name = "stbi_ldr_to_hdr_scale" });

/// Sets the global flag indicating whether image data should be flipped along the y axis when loaded. Default is `false`.
pub fn setFlipVerticallyOnLoad(should_flip: bool) void {
    stbi_set_flip_vertically_on_load(.from(should_flip));
}

/// Sets the thread-local flag indicating whether image data should be flipped along the y axis when loaded. Default is `false`.
pub fn setFlipVerticallyOnLoadThread(should_flip: bool) void {
    stbi_set_flip_vertically_on_load_thread(.from(should_flip));
}

/// Sets the global flag indicating whether image data should be **un**-premultiplied when loaded. Default is `false`.
pub fn setUnpremultiplyOnLoad(should_unpremultiply: bool) void {
    stbi_set_unpremultiply_on_load(.from(should_unpremultiply));
}

/// Sets the thread-local flag indicating whether image data should be **un**-premultiplied when loaded. Default is `false`.
pub fn setUnpremultiplyOnLoadThread(should_unpremultiply: bool) void {
    stbi_set_unpremultiply_on_load_thread(.from(should_unpremultiply));
}

/// Sets the global flag indicating whether iPhone-formatted PNGs should be converted to standard RGB format when loaded. Default is `false`.
pub fn convertIphonePngToRgb(should_convert: bool) void {
    stbi_convert_iphone_png_to_rgb(.from(should_convert));
}

/// Sets the thread-local flag indicating whether iPhone-formatted PNGs should be converted to standard RGB format when loaded. Default is `false`.
pub fn convertIphonePngToRgbThread(should_convert: bool) void {
    stbi_convert_iphone_png_to_rgb_thread(.from(should_convert));
}

// Path api //

/// Returns true if the image is a HDR image.
pub fn isHdrFromPath(filename: []const u8) bool {
    const filename_z = dupeZ(filename);
    return stbi_is_hdr(filename_z).to();
}

/// Returns true if the image is a 16-bit per channel image.
pub fn is16BitFromPath(filename: []const u8) bool {
    const filename_z = dupeZ(filename);
    return stbi_is_16_bit(filename_z).to();
}

// Memory api //

pub fn is16BitFromMemory(buffer: []const u8) bool {
    return stbi_is_16_bit_from_memory(buffer.ptr, @intCast(buffer.len)).to();
}

pub fn isHdrFromMemory(buffer: []const u8) bool {
    return stbi_is_hdr_from_memory(buffer.ptr, @intCast(buffer.len)).to();
}

// Callback api //

pub fn is16BitFromCallbacks(clbk: *const IoCallbacks, user: ?*anyopaque) bool {
    return stbi_is_16_bit_from_callbacks(clbk, user).to();
}

pub fn isHdrFromCallbacks(clbk: *const IoCallbacks, user: ?*anyopaque) bool {
    return stbi_is_hdr_from_callbacks(clbk, user).to();
}

// Image definitions //

/// Image description metadata.
pub const Info = struct {
    /// width of the image in pixels
    width: u32,
    /// height of the image in pixels
    height: u32,
    /// number and configuration of channels in the image
    channels: Channels,

    /// Retrieves information about an image without loading the full image data.
    pub fn fromPath(filename: []const u8) !Info {
        const filename_z = dupeZ(filename);

        var width: i32 = 0;
        var height: i32 = 0;
        var channels_in_file: Channels = .default;

        if (!stbi_info(filename_z, &width, &height, &channels_in_file).to()) {
            log.debug("failed to load image: {s}", .{failureReason() orelse "unknown error"});
            return error.FailedToLoadImage;
        }

        return .{
            .width = @intCast(width),
            .height = @intCast(height),
            .channels = channels_in_file,
        };
    }

    /// Retrieves information about an image in a memory buffer without loading the full image data.
    pub fn fromMemory(buffer: []const u8) !Info {
        var width: i32 = 0;
        var height: i32 = 0;
        var channels_in_file: Channels = .default;

        if (!stbi_info_from_memory(buffer, @intCast(buffer.len), &width, &height, &channels_in_file).to()) {
            log.debug("failed to load image: {s}", .{failureReason() orelse "unknown error"});
            return error.FailedToLoadImage;
        }

        return .{
            .width = @intCast(width),
            .height = @intCast(height),
            .channels = channels_in_file,
        };
    }

    /// Give a callback vtable, and optionally a userdata pointer, retrieves information about an image without loading the full image data.
    pub fn fromCallbacks(clbk: *const IoCallbacks, user: ?*anyopaque) !Info {
        var width: i32 = 0;
        var height: i32 = 0;
        var channels_in_file: Channels = .default;

        if (!stbi_info_from_callbacks(clbk, user, &width, &height, &channels_in_file).to()) {
            log.debug("failed to load image: {s}", .{failureReason() orelse "unknown error"});
            return error.FailedToLoadImage;
        }

        return .{
            .width = @intCast(width),
            .height = @intCast(height),
            .channels = channels_in_file,
        };
    }
};

/// Image with 8-bit channels.
pub const Image8 = struct {
    info: Info,
    buffer: ImageBuffer8,

    /// Deallocates the image buffer, freeing the memory.
    pub fn deinit(self: *Image8) void {
        stbi_image_free(self.buffer.ptr);
        self.* = undefined;
    }

    /// Loads an 8bpp image from the provided file path, converting where necessary to the desired number of channels.
    pub fn fromPath(filename: []const u8, desired_channels: Channels) !Image8 {
        const filename_z = dupeZ(filename);

        var width: i32 = 0;
        var height: i32 = 0;
        var channels_in_file: Channels = .default;

        const buffer = stbi_load(filename_z, &width, &height, &channels_in_file, desired_channels) orelse {
            log.debug("failed to load image: {s}", .{failureReason() orelse "unknown error"});
            return error.FailedToLoadImage;
        };

        const buffer_info = Info{
            .width = @intCast(width),
            .height = @intCast(height),
            .channels = if (desired_channels != .default) desired_channels else channels_in_file,
        };

        return Image8{
            .info = buffer_info,
            .buffer = buffer[0 .. buffer_info.width * buffer_info.height * buffer_info.channels.asInt()],
        };
    }

    /// Loads an 8bpp image from a memory buffer, converting where necessary to the desired number of channels.
    pub fn fromMemory(buffer: []const u8, desired_channels: Channels) ?Image8 {
        var width: i32 = 0;
        var height: i32 = 0;
        var channels_in_file: Channels = .default;

        const img_buffer = stbi_load_from_memory(buffer.ptr, @intCast(buffer.len), &width, &height, &channels_in_file, desired_channels) orelse {
            return null;
        };

        const buffer_info = Info{
            .width = @intCast(width),
            .height = @intCast(height),
            .channels = if (desired_channels != .default) desired_channels else channels_in_file,
        };

        return Image8{
            .info = buffer_info,
            .buffer = img_buffer,
        };
    }

    /// Given a callback vtable, and optionally a userdata pointer, loads an 8bpp image, converting where necessary to the desired number of channels.
    pub fn fromCallbacks(clbk: *const IoCallbacks, user: ?*anyopaque, desired_channels: Channels) !Image8 {
        var width: i32 = 0;
        var height: i32 = 0;
        var channels_in_file: Channels = .default;

        const img_buffer = stbi_load_from_callbacks(clbk, user, &width, &height, &channels_in_file, desired_channels) orelse {
            log.debug("failed to load image: {s}", .{failureReason() orelse "unknown error"});
            return error.FailedToLoadImage;
        };

        const buffer_info = Info{
            .width = @intCast(width),
            .height = @intCast(height),
            .channels = if (desired_channels != .default) desired_channels else channels_in_file,
        };

        return Image8{
            .info = buffer_info,
            .buffer = img_buffer[0 .. buffer_info.width * buffer_info.height * buffer_info.channels.asInt()],
        };
    }
};

/// Image with 16-bit channels.
pub const Image16 = struct {
    info: Info,
    buffer: ImageBuffer16,

    /// Deallocates the image buffer, freeing the memory.
    pub fn deinit(self: *Image16) void {
        stbi_image_free(self.buffer.ptr);
        self.* = undefined;
    }

    /// Loads a 16bpp image from the provided file path, converting where necessary to the desired number of channels.
    pub fn fromPath(filename: []const u8, desired_channels: Channels) !Image16 {
        const filename_z = dupeZ(filename);

        var width: i32 = 0;
        var height: i32 = 0;
        var channels_in_file: Channels = .default;

        const buffer = stbi_load_16(filename_z, &width, &height, &channels_in_file, desired_channels) orelse {
            log.debug("failed to load image: {s}", .{failureReason() orelse "unknown error"});
            return error.FailedToLoadImage;
        };
        return Image16{
            .info = .{
                .width = @intCast(width),
                .height = @intCast(height),
                .channels = if (desired_channels != .default) desired_channels else channels_in_file,
            },
            .buffer = buffer,
        };
    }

    /// Loads a 16bpp image from a memory buffer, converting where necessary to the desired number of channels.
    pub fn fromMemory(buffer: []const u8, desired_channels: Channels) ?Image16 {
        var width: i32 = 0;
        var height: i32 = 0;
        var channels_in_file: Channels = .default;

        const img_buffer = stbi_load_16_from_memory(buffer.ptr, @intCast(buffer.len), &width, &height, &channels_in_file, desired_channels) orelse {
            return null;
        };

        const buffer_info = Info{
            .width = @intCast(width),
            .height = @intCast(height),
            .channels = if (desired_channels != .default) desired_channels else channels_in_file,
        };

        return Image16{
            .info = buffer_info,
            .buffer = img_buffer[0 .. buffer_info.width * buffer_info.height * buffer_info.channels.asInt()],
        };
    }

    /// Given a callback vtable, and optionally a userdata pointer, loads a 16bpp image, converting where necessary to the desired number of channels.
    pub fn fromCallbacks(clbk: *const IoCallbacks, user: ?*anyopaque, desired_channels: Channels) ?Image16 {
        var width: i32 = 0;
        var height: i32 = 0;
        var channels_in_file: Channels = .default;

        const img_buffer = stbi_load_16_from_callbacks(clbk, user, &width, &height, &channels_in_file, desired_channels) orelse {
            return null;
        };

        const buffer_info = Info{
            .width = @intCast(width),
            .height = @intCast(height),
            .channels = if (desired_channels != .default) desired_channels else channels_in_file,
        };

        return Image16{
            .info = buffer_info,
            .buffer = img_buffer[0 .. buffer_info.width * buffer_info.height * buffer_info.channels.asInt()],
        };
    }
};

/// Image with 32-bit float channels.
pub const ImageF = struct {
    info: Info,
    buffer: ImageBufferF,

    /// Deallocates the image buffer, freeing the memory.
    pub fn deinit(self: *ImageF) void {
        stbi_image_free(self.buffer.ptr);
        self.* = undefined;
    }

    /// Loads a 32bpp float image from the provided file path, converting where necessary to the desired number of channels.
    pub fn fromPath(filename: []const u8, desired_channels: Channels) !ImageF {
        const filename_z = dupeZ(filename);

        var width: i32 = 0;
        var height: i32 = 0;
        var channels_in_file: Channels = .default;

        const buffer = stbi_loadf(filename_z, &width, &height, &channels_in_file, desired_channels) orelse {
            log.debug("failed to load image: {s}", .{failureReason() orelse "unknown error"});
            return error.FailedToLoadImage;
        };

        const buffer_info = Info{
            .width = @intCast(width),
            .height = @intCast(height),
            .channels = if (desired_channels != .default) desired_channels else channels_in_file,
        };

        return ImageF{
            .info = buffer_info,
            .buffer = buffer[0 .. buffer_info.width * buffer_info.height * buffer_info.channels.asInt()],
        };
    }

    /// Loads a 32bpp float image from a memory buffer, converting where necessary to the desired number of channels.
    pub fn fromMemory(buffer: [*]const u8, desired_channels: Channels) !ImageF {
        var width: i32 = 0;
        var height: i32 = 0;
        var channels_in_file: Channels = .default;

        const img_buffer = stbi_loadf_from_memory(buffer.ptr, @intCast(buffer.len), &width, &height, &channels_in_file, desired_channels) orelse {
            log.debug("failed to load image: {s}", .{failureReason() orelse "unknown error"});
            return error.FailedToLoadImage;
        };

        const buffer_info = Info{
            .width = @intCast(width),
            .height = @intCast(height),
            .channels = if (desired_channels != .default) desired_channels else channels_in_file,
        };

        return ImageF{
            .info = buffer_info,
            .buffer = img_buffer[0 .. buffer_info.width * buffer_info.height * buffer_info.channels.asInt()],
        };
    }

    /// Given a callback vtable, and optionally a userdata pointer, loads a 32bpp float image, converting where necessary to the desired number of channels.
    pub fn fromCallbacks(clbk: *const IoCallbacks, user: ?*anyopaque, desired_channels: Channels) !ImageF {
        var width: i32 = 0;
        var height: i32 = 0;
        var channels_in_file: Channels = .default;

        const img_buffer = stbi_loadf_from_callbacks(clbk, user, &width, &height, &channels_in_file, desired_channels) orelse {
            log.debug("failed to load image: {s}", .{failureReason() orelse "unknown error"});
            return error.FailedToLoadImage;
        };

        const buffer_info = Info{
            .width = @intCast(width),
            .height = @intCast(height),
            .channels = if (desired_channels != .default) desired_channels else channels_in_file,
        };

        return ImageF{
            .info = buffer_info,
            .buffer = img_buffer[0 .. buffer_info.width * buffer_info.height * buffer_info.channels.asInt()],
        };
    }
};

// ZLib decoding functions //

pub fn decodeZLib_MallocGuess(buffer: []const u8, initial_size: i32) !ImageBuffer8 {
    var outlen: i32 = 0;
    const buf = stbi_zlib_decode_malloc_guesssize(buffer.ptr, @intCast(buffer.len), initial_size, &outlen) orelse {
        log.debug("failed to load image: {s}", .{failureReason() orelse "unknown error"});
        return error.FailedToLoadImage;
    };
    return buf[0..@intCast(outlen)];
}

pub fn decodeZLib_MallocGuess_Header(buffer: []const u8, initial_size: i32, parse_header: bool) !ImageBuffer8 {
    var outlen: i32 = 0;
    const buf = stbi_zlib_decode_malloc_guesssize_headerflag(buffer.ptr, @intCast(buffer.len), initial_size, &outlen, if (parse_header) 1 else 0) orelse {
        log.debug("failed to load image: {s}", .{failureReason() orelse "unknown error"});
        return error.FailedToLoadImage;
    };
    return buf[0..@intCast(outlen)];
}

pub fn decodeZLib_Malloc(buffer: []const u8) !ImageBuffer8 {
    var outlen: i32 = 0;
    const buf = stbi_zlib_decode_malloc(buffer.ptr, @intCast(buffer.len), &outlen) orelse {
        log.debug("failed to load image: {s}", .{failureReason() orelse "unknown error"});
        return error.FailedToLoadImage;
    };
    return buf[0..@intCast(outlen)];
}

pub fn decodeZLib_Buffer(obuffer: ImageBuffer8, ibuffer: []const u8) bool {
    return stbi_zlib_decode_buffer(obuffer.ptr, @intCast(obuffer.len), ibuffer.ptr, @intCast(ibuffer.len)).to();
}

pub fn decodeZLib_Malloc_NoHeader(buffer: []const u8) !ImageBuffer8 {
    var outlen: i32 = 0;
    const buf = stbi_zlib_decode_noheader_malloc(buffer.ptr, @intCast(buffer.len), &outlen) orelse {
        log.debug("failed to load image: {s}", .{failureReason() orelse "unknown error"});
        return error.FailedToLoadImage;
    };
    return buf[0..@intCast(outlen)];
}

pub fn decodeZLib_Buffer_NoHeader(obuffer: ImageBuffer8, ibuffer: []const u8) bool {
    return stbi_zlib_decode_noheader_buffer(obuffer.ptr, @intCast(obuffer.len), ibuffer.ptr, @intCast(ibuffer.len)).to();
}

// unused & utilities //

/// If an image fails to load, this will give a descriptive error message.
pub fn failureReason() ?[]const u8 {
    return if (stbi_failure_reason()) |reason_z| std.mem.span(reason_z) else null;
}

/// Frees any `RawImageBufferX` and `ImageBufferX` value allocated by stb_image.
/// * Usually, you would use `ImageX.deinit` instead, but the zlib functions for example return raw buffers.
/// * This will not compile for other types.
pub fn free_buffer(img: anytype) void {
    switch (@TypeOf(img)) {
        RawImageBuffer8 => stbi_image_free(img),
        RawImageBuffer16 => stbi_image_free(img),
        RawImageBufferF => stbi_image_free(img),
        ImageBuffer8 => stbi_image_free(img.ptr),
        ImageBuffer16 => stbi_image_free(img.ptr),
        ImageBufferF => stbi_image_free(img.ptr),
        else => @compileError("invalid type passed to freeImage"),
    }
}

/// This is not yet wrapped; the interface is terrible and theres no real use case for it in my personal plans.
pub const loadGifFromMemory = @extern(*const fn (buffer: [*]const u8, delays: [*]*i32, len: i32, width: ?*i32, height: ?*i32, z: ?*i32, channels: ?*Channels, desired_channels: Channels) callconv(.c) ?RawImageBuffer8, .{ .name = "stbi_load_gif_from_memory" });

// TODO: we could probably just use the fromFile apis instead of duping the filename
threadlocal var filename_transform_buff: [1024]u8 = undefined;
fn dupeZ(filename: []const u8) [*:0]const u8 {
    var fba = std.heap.FixedBufferAllocator.init(&filename_transform_buff);
    const filename_z = fba.allocator().dupeZ(u8, filename) catch @panic("failed to allocate null-terminated filename for stb_image");
    return filename_z.ptr;
}

const BigBool = enum(i32) {
    False,
    _,

    const True: BigBool = @enumFromInt(1);

    pub fn from(x: bool) BigBool {
        return if (x) .True else .False;
    }

    pub fn to(self: BigBool) bool {
        return switch (self) {
            .False => false,
            else => true,
        };
    }
};

// extern C functions //

extern fn stbi_image_free(retval_from_stbi_load: *anyopaque) callconv(.c) void;
extern fn stbi_failure_reason() callconv(.c) ?[*:0]const u8;
extern fn stbi_set_flip_vertically_on_load(flag_true_if_should_flip: BigBool) callconv(.c) void;
extern fn stbi_set_flip_vertically_on_load_thread(flag_true_if_should_flip: BigBool) callconv(.c) void;
extern fn stbi_set_unpremultiply_on_load(flag_true_if_should_unpremultiply: BigBool) callconv(.c) void;
extern fn stbi_set_unpremultiply_on_load_thread(flag_true_if_should_unpremultiply: BigBool) callconv(.c) void;
extern fn stbi_convert_iphone_png_to_rgb(flag_true_if_should_convert: BigBool) callconv(.c) void;
extern fn stbi_convert_iphone_png_to_rgb_thread(flag_true_if_should_convert: BigBool) callconv(.c) void;
extern fn stbi_load(filename: [*:0]const u8, width: ?*i32, height: ?*i32, channels_in_file: ?*Channels, desired_channels: Channels) ?RawImageBuffer8;
extern fn stbi_load_16(filename: [*:0]const u8, width: ?*i32, height: ?*i32, channels_in_file: ?*Channels, desired_channels: Channels) ?RawImageBuffer16;
extern fn stbi_loadf(filename: [*:0]const u8, width: ?*i32, height: ?*i32, channels_in_file: ?*Channels, desired_channels: Channels) ?RawImageBufferF;
extern fn stbi_info(filename: [*:0]const u8, width: ?*i32, height: ?*i32, channels_in_file: ?*Channels) BigBool;
extern fn stbi_is_hdr(filename: [*:0]const u8) BigBool;
extern fn stbi_is_16_bit(filename: [*:0]const u8) BigBool;
extern fn stbi_load_from_memory(buffer: [*]const u8, len: i32, width: ?*i32, height: ?*i32, channels_in_file: ?*Channels, desired_channels: Channels) callconv(.c) ?RawImageBuffer8;
extern fn stbi_load_16_from_memory(buffer: [*]const u8, len: i32, width: ?*i32, height: ?*i32, channels_in_file: ?*Channels, desired_channels: Channels) callconv(.c) ?RawImageBuffer16;
extern fn stbi_loadf_from_memory(buffer: [*]const u8, len: i32, width: ?*i32, height: ?*i32, channels_in_file: ?*Channels, desired_channels: Channels) callconv(.c) ?RawImageBufferF;
extern fn stbi_info_from_memory(buffer: [*]const u8, len: i32, width: ?*i32, height: ?*i32, channels_in_file: ?*Channels) callconv(.c) BigBool;
extern fn stbi_is_hdr_from_memory(buffer: [*]const u8, len: i32) callconv(.c) BigBool;
extern fn stbi_is_16_bit_from_memory(buffer: [*]const u8, len: i32) callconv(.c) BigBool;
extern fn stbi_load_from_callbacks(clbk: *const IoCallbacks, user: ?*anyopaque, width: ?*i32, height: ?*i32, channels_in_file: ?*Channels, desired_channels: Channels) callconv(.c) ?RawImageBuffer8;
extern fn stbi_load_16_from_callbacks(clbk: *const IoCallbacks, user: ?*anyopaque, width: ?*i32, height: ?*i32, channels_in_file: ?*Channels, desired_channels: Channels) callconv(.c) ?RawImageBuffer16;
extern fn stbi_loadf_from_callbacks(clbk: *const IoCallbacks, user: ?*anyopaque, width: ?*i32, height: ?*i32, channels_in_file: ?*Channels, desired_channels: Channels) callconv(.c) ?RawImageBufferF;
extern fn stbi_is_hdr_from_callbacks(clbk: *const IoCallbacks, user: ?*anyopaque) callconv(.c) BigBool;
extern fn stbi_is_16_bit_from_callbacks(clbk: *const IoCallbacks, user: ?*anyopaque) callconv(.c) BigBool;
extern fn stbi_info_from_callbacks(clbk: *const IoCallbacks, user: ?*anyopaque, width: ?*i32, height: ?*i32, channels_in_file: ?*Channels) callconv(.c) BigBool;
extern fn stbi_zlib_decode_malloc_guesssize(buffer: [*]const u8, len: i32, initial_size: i32, outlen: ?*i32) callconv(.c) ?RawImageBuffer8;
extern fn stbi_zlib_decode_malloc_guesssize_headerflag(buffer: [*]const u8, len: i32, initial_size: i32, outlen: ?*i32, parse_header: i32) callconv(.c) ?RawImageBuffer8;
extern fn stbi_zlib_decode_malloc(buffer: [*]const u8, len: i32, outlen: ?*i32) callconv(.c) ?RawImageBuffer8;
extern fn stbi_zlib_decode_buffer(obuffer: RawImageBuffer8, olen: i32, ibuffer: [*]const u8, ilen: i32) callconv(.c) BigBool;
extern fn stbi_zlib_decode_noheader_malloc(buffer: [*]const u8, len: i32, outlen: ?*i32) callconv(.c) ?RawImageBuffer8;
extern fn stbi_zlib_decode_noheader_buffer(obuffer: RawImageBuffer8, olen: i32, ibuffer: [*]const u8, ilen: i32) callconv(.c) BigBool;
