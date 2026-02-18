const std = @import("std");
const nfd = @import("nfd");

pub fn main() !void {
    {
        std.debug.print("Opening file dialog...\n", .{});

        const x = try nfd.openDialog("glb,gltf", ".");
        if (x) |path| {
            std.debug.print("Selected file: {s}\n", .{path});
        } else {
            std.debug.print("No file selected\n", .{});
        }
    }

    {
        std.debug.print("Opening multi-file dialog...\n", .{});

        const x = try nfd.openDialogMultiple("glb,gltf", ".");
        if (x) |paths| {
            std.debug.print("Selected files:\n", .{});
            for (paths) |path| {
                std.debug.print("- {s}\n", .{path});
            }
        } else {
            std.debug.print("No files selected\n", .{});
        }
    }

    {
        std.debug.print("Opening save file dialog...\n", .{});

        const x = try nfd.saveDialog("glb,gltf", ".");
        if (x) |path| {
            std.debug.print("Selected save file: {s}\n", .{path});
        } else {
            std.debug.print("No save file selected\n", .{});
        }
    }

    {
        std.debug.print("Opening folder dialog...\n", .{});

        const x = try nfd.folderDialog(".");
        if (x) |path| {
            std.debug.print("Selected folder: {s}\n", .{path});
        } else {
            std.debug.print("No folder selected\n", .{});
        }
    }
}
