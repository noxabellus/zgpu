const std = @import("std");

pub fn CopyPtrMutability(
    comptime source: type,
    comptime size: std.builtin.Type.Pointer.Size,
    comptime child: type,
) type {
    const info = @typeInfo(source).pointer;
    return @Pointer(
        size,
        .{
            .@"const" = info.is_const,
            .@"volatile" = info.is_volatile,
            .@"allowzero" = false,
            .@"align" = @alignOf(child),
            .@"addrspace" = info.address_space,
        },
        child,
        null,
    );
}
