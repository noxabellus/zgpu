//! Contains shared structs for atlas management and debug helpers.
const std = @import("std");
const wgpu = @import("wgpu");
const stbi = @import("stbi");
const log = std.log.scoped(.atlas);

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
