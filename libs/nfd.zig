const std = @import("std");

var in_buf: [32000]u8 = undefined;
var out_buf: [32000]u8 = undefined;
var queue_buf: [32000]u8 = undefined;
var thread: std.Thread = undefined;
var shutdown_async: std.atomic.Value(bool) = .init(false);
var queue_fba: std.heap.FixedBufferAllocator = undefined;
var queue_lock: std.Thread.Mutex = .{};
var queue: std.ArrayList(Event) = .empty;

const EventKind = union(enum) {
    open,
    openMultiple,
    save,
    folder,
};

const Event = struct {
    kind: EventKind,
    filterList: ?[]const u8 = null,
    defaultPath: ?[]const u8 = null,
    out: *error{DialogFailed}!?[]const u8,
};

pub fn initAsync() !void {
    queue_fba = std.heap.FixedBufferAllocator.init(&queue_buf);
    thread = try std.Thread.spawn(.{}, asyncExecutor, .{});
}

fn asyncExecutor() void {
    while (!shutdown_async.load(.acquire)) {
        {
            queue_lock.lock();
            defer queue_lock.unlock();

            if (queue.pop()) |event| {
                std.debug.print("Processing async dialog event: {s}\n", .{@tagName(event.kind)});
                switch (event.kind) {
                    .open => {
                        event.out.* = openDialog(event.filterList, event.defaultPath);
                    },
                    .openMultiple => {
                        event.out.* = openDialogMultiple(event.filterList, event.defaultPath);
                    },
                    .save => {
                        event.out.* = saveDialog(event.filterList, event.defaultPath);
                    },
                    .folder => {
                        event.out.* = folderDialog(event.defaultPath);
                    },
                }
            }
        }

        std.Thread.sleep(std.time.ns_per_ms * 10);
    }
}

pub fn deinitAsync() void {
    shutdown_async.store(true, .release);
    thread.join();
}

pub fn openDialogAsync(filterList: ?[]const u8, defaultPath: ?[]const u8, result: *error{DialogFailed}!?[]const u8) !void {
    queue_lock.lock();
    defer queue_lock.unlock();

    try queue.append(queue_fba.allocator(), Event{
        .kind = .open,
        .filterList = filterList,
        .defaultPath = defaultPath,
        .out = result,
    });
}

pub fn openDialogMultipleAsync(filterList: ?[]const u8, defaultPath: ?[]const u8, result: *error{DialogFailed}!?[]const u8) !void {
    queue_lock.lock();
    defer queue_lock.unlock();

    try queue.append(queue_fba.allocator(), Event{
        .kind = .openMultiple,
        .filterList = filterList,
        .defaultPath = defaultPath,
        .out = result,
    });
}

pub fn saveDialogAsync(filterList: ?[]const u8, defaultPath: ?[]const u8, result: *error{DialogFailed}!?[]const u8) !void {
    queue_lock.lock();
    defer queue_lock.unlock();

    try queue.append(queue_fba.allocator(), Event{
        .kind = .save,
        .filterList = filterList,
        .defaultPath = defaultPath,
        .out = result,
    });
}

pub fn folderDialogAsync(defaultPath: ?[]const u8, result: *error{DialogFailed}!?[]const u8) !void {
    queue_lock.lock();
    defer queue_lock.unlock();

    try queue.append(queue_fba.allocator(), Event{
        .kind = .folder,
        .defaultPath = defaultPath,
        .out = result,
    });
}

pub fn openDialog(filterList: ?[]const u8, defaultPath: ?[]const u8) error{DialogFailed}!?[]const u8 {
    var inFba = std.heap.FixedBufferAllocator.init(&in_buf);
    var outFba = std.heap.FixedBufferAllocator.init(&out_buf);

    const arg0 = if (filterList) |x| (inFba.allocator().dupeZ(u8, x) catch return error.DialogFailed).ptr else null;
    const arg1 = if (defaultPath) |x| (inFba.allocator().dupeZ(u8, x) catch return error.DialogFailed).ptr else null;

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
            const dup = outFba.allocator().dupeZ(u8, std.mem.span(path)) catch return error.DialogFailed;

            return dup;
        },
    }
}

pub fn openDialogMultiple(filterList: ?[]const u8, defaultPath: ?[]const u8) error{DialogFailed}!?[]const u8 {
    var inFba = std.heap.FixedBufferAllocator.init(&in_buf);
    var outFba = std.heap.FixedBufferAllocator.init(&out_buf);

    const arg0 = if (filterList) |x| (inFba.allocator().dupeZ(u8, x) catch return error.DialogFailed).ptr else null;
    const arg1 = if (defaultPath) |x| (inFba.allocator().dupeZ(u8, x) catch return error.DialogFailed).ptr else null;

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

            var result = std.ArrayList(u8).empty;
            for (0..paths.count) |i| {
                const path = NFD_PathSet_GetPath(&paths, i) orelse continue;
                result.appendSlice(outFba.allocator(), std.mem.span(path)) catch return error.DialogFailed;
                if (i != paths.count - 1) {
                    result.append(outFba.allocator(), ';') catch return error.DialogFailed;
                }
            }

            return result.items;
        },
    }
}

pub fn saveDialog(filterList: ?[]const u8, defaultPath: ?[]const u8) error{DialogFailed}!?[]const u8 {
    var inFba = std.heap.FixedBufferAllocator.init(&in_buf);
    var outFba = std.heap.FixedBufferAllocator.init(&out_buf);

    const arg0 = if (filterList) |x| (inFba.allocator().dupeZ(u8, x) catch return error.DialogFailed).ptr else null;
    const arg1 = if (defaultPath) |x| (inFba.allocator().dupeZ(u8, x) catch return error.DialogFailed).ptr else null;

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
            const dup = outFba.allocator().dupeZ(u8, std.mem.span(path)) catch return error.DialogFailed;

            return dup;
        },
    }
}

pub fn folderDialog(defaultPath: ?[]const u8) error{DialogFailed}!?[]const u8 {
    var inFba = std.heap.FixedBufferAllocator.init(&in_buf);
    var outFba = std.heap.FixedBufferAllocator.init(&out_buf);

    const arg0 = if (defaultPath) |x| (inFba.allocator().dupeZ(u8, x) catch return error.DialogFailed).ptr else null;

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
            const dup = outFba.allocator().dupeZ(u8, std.mem.span(path)) catch return error.DialogFailed;

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
