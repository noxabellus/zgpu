const std = @import("std");

var in_buf: [32000]u8 = undefined;
var out_buf: [32000]u8 = undefined;

pub fn openDialog(filterList: ?[]const u8, defaultPath: ?[]const u8) !?[]const u8 {
    var inFba = std.heap.FixedBufferAllocator.init(&in_buf);
    var outFba = std.heap.FixedBufferAllocator.init(&out_buf);

    const arg0 = if (filterList) |x| (inFba.allocator().dupeZ(u8, x) catch return error.OutOfMemory).ptr else null;
    const arg1 = if (defaultPath) |x| (inFba.allocator().dupeZ(u8, x) catch return error.OutOfMemory).ptr else null;

    var ptr: ?[*:0]const u8 = null;
    switch (NFD_OpenDialog(arg0, arg1, &ptr)) {
        .err => {
            const msg = NFD_GetError() orelse "Unknown error";
            std.log.err("Dialog error: {s}\n", .{std.mem.span(msg)});
            return error.DialogFailed;
        },
        .cancel => return null,
        .okay => {
            defer NFDi_Free(ptr);

            const path = ptr.?;
            const dup = outFba.allocator().dupeZ(u8, std.mem.span(path)) catch return error.OutOfMemory;

            return dup;
        },
    }
}

pub fn openDialogMultiple(filterList: ?[]const u8, defaultPath: ?[]const u8) !?[]const []const u8 {
    var inFba = std.heap.FixedBufferAllocator.init(&in_buf);
    var outFba = std.heap.FixedBufferAllocator.init(&out_buf);

    const arg0 = if (filterList) |x| (inFba.allocator().dupeZ(u8, x) catch return error.OutOfMemory).ptr else null;
    const arg1 = if (defaultPath) |x| (inFba.allocator().dupeZ(u8, x) catch return error.OutOfMemory).ptr else null;

    var paths: nfdpathset_t = undefined;
    switch (NFD_OpenDialogMultiple(arg0, arg1, &paths)) {
        .err => {
            const msg = NFD_GetError() orelse "Unknown error";
            std.log.err("Dialog error: {s}\n", .{std.mem.span(msg)});
            return error.DialogFailed;
        },
        .cancel => return null,
        .okay => {
            defer NFD_PathSet_Free(&paths);

            var result = try outFba.allocator().alloc([]const u8, paths.count);
            for (0..paths.count) |i| {
                const path = NFD_PathSet_GetPath(&paths, i) orelse continue;
                result[i] = try outFba.allocator().dupeZ(u8, std.mem.span(path));
            }

            return result;
        },
    }
}

pub fn saveDialog(filterList: ?[]const u8, defaultPath: ?[]const u8) !?[]const u8 {
    var inFba = std.heap.FixedBufferAllocator.init(&in_buf);
    var outFba = std.heap.FixedBufferAllocator.init(&out_buf);

    const arg0 = if (filterList) |x| (inFba.allocator().dupeZ(u8, x) catch return error.OutOfMemory).ptr else null;
    const arg1 = if (defaultPath) |x| (inFba.allocator().dupeZ(u8, x) catch return error.OutOfMemory).ptr else null;

    var ptr: ?[*:0]const u8 = null;
    switch (NFD_SaveDialog(arg0, arg1, &ptr)) {
        .err => {
            const msg = NFD_GetError() orelse "Unknown error";
            std.log.err("Dialog error: {s}\n", .{std.mem.span(msg)});
            return error.DialogFailed;
        },
        .cancel => return null,
        .okay => {
            defer NFDi_Free(ptr);

            const path = ptr.?;
            const dup = outFba.allocator().dupeZ(u8, std.mem.span(path)) catch return error.OutOfMemory;

            return dup;
        },
    }
}

pub fn folderDialog(defaultPath: ?[]const u8) !?[]const u8 {
    var inFba = std.heap.FixedBufferAllocator.init(&in_buf);
    var outFba = std.heap.FixedBufferAllocator.init(&out_buf);

    const arg0 = if (defaultPath) |x| (inFba.allocator().dupeZ(u8, x) catch return error.OutOfMemory).ptr else null;

    var ptr: ?[*:0]const u8 = null;
    switch (NFD_PickFolder(arg0, &ptr)) {
        .err => {
            const msg = NFD_GetError() orelse "Unknown error";
            std.log.err("Dialog error: {s}\n", .{std.mem.span(msg)});
            return error.DialogFailed;
        },
        .cancel => return null,
        .okay => {
            defer NFDi_Free(ptr);

            const path = ptr.?;
            const dup = outFba.allocator().dupeZ(u8, std.mem.span(path)) catch return error.OutOfMemory;

            return dup;
        },
    }
}

/// opaque data structure -- see NFD_PathSet_*
const nfdpathset_t = extern struct {
    buf: *u8,
    /// byte offsets into buf
    indices: [*]usize,
    /// number of indices into buf
    count: usize,
};

const nfdresult_t = enum(c_int) {
    /// programmatic error
    err,
    /// user pressed okay, or successful return
    okay,
    /// user pressed cancel
    cancel,
};

extern fn NFDi_Free(ptr: ?[*:0]const u8) void;

/// single file open dialog
extern fn NFD_OpenDialog(
    filterList: ?[*:0]const u8,
    defaultPath: ?[*:0]const u8,
    outPath: *?[*:0]const u8,
) nfdresult_t;

/// multiple file open dialog
extern fn NFD_OpenDialogMultiple(
    filterList: ?[*:0]const u8,
    defaultPath: ?[*:0]const u8,
    outPaths: *nfdpathset_t,
) nfdresult_t;

/// save dialog
extern fn NFD_SaveDialog(
    filterList: ?[*:0]const u8,
    defaultPath: ?[*:0]const u8,
    outPath: *?[*:0]const u8,
) nfdresult_t;

/// select folder dialog */
extern fn NFD_PickFolder(
    defaultPath: ?[*:0]const u8,
    outPath: *?[*:0]const u8,
) nfdresult_t;

/// get last error -- set when nfdresult_t returns NFD_ERROR
extern fn NFD_GetError(
    // void
) ?[*:0]const u8;

/// get the number of entries stored in pathSet
extern fn NFD_PathSet_GetCount(
    pathSet: *const nfdpathset_t,
) usize;

/// Get the UTF-8 path at offset index
extern fn NFD_PathSet_GetPath(
    pathSet: *const nfdpathset_t,
    index: usize,
) ?[*:0]const u8;

/// Free the pathSet
extern fn NFD_PathSet_Free(
    pathSet: *nfdpathset_t,
) void;
