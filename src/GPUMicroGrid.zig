const Grid = @This();

const std = @import("std");

const linalg = @import("./linalg.zig");
const aabb3i = linalg.aabb3i;
const vec3 = linalg.vec3;
const vec3i = linalg.vec3i;
const vec3u = linalg.vec3u;

const GRID_AXIS_DIVISOR = 128;
const PAGE_AXIS_DIVISOR = 16;
const VOXEME_AXIS_DIVISOR = 16;

const PAGE_TABLE_SIZE = GRID_AXIS_DIVISOR * GRID_AXIS_DIVISOR * GRID_AXIS_DIVISOR;
const VOXEMES_PER_PAGE = PAGE_AXIS_DIVISOR * PAGE_AXIS_DIVISOR * PAGE_AXIS_DIVISOR;
const VOXELS_PER_VOXEME = VOXEME_AXIS_DIVISOR * VOXEME_AXIS_DIVISOR * VOXEME_AXIS_DIVISOR;
const VOXELS_PER_PAGE = VOXELS_PER_VOXEME * VOXEMES_PER_PAGE;

const GRID_SCALE = 2048.0;
const INV_GRID_SCALE = 1.0 / GRID_SCALE;

const PAGE_SCALE = 16;
const INV_PAGE_SCALE = 1.0 / PAGE_SCALE;

const VOXEME_SCALE = 1.0;
const INV_VOXEME_SCALE = 1.0 / VOXEME_SCALE;

const VOXEL_SCALE = 0.0625;
const INV_VOXEL_SCALE = 1.0 / VOXEL_SCALE;

const GRID_AXIS_SHIFT = 7; // log2(128)
const GRID_AXIS_MASK = GRID_AXIS_DIVISOR - 1;

const PAGE_AXIS_SHIFT = 4; // log2(16)
const PAGE_AXIS_MASK = PAGE_AXIS_DIVISOR - 1;

const VOXEME_AXIS_SHIFT = 4; // log2(16)
const VOXEME_AXIS_MASK = VOXEME_AXIS_DIVISOR - 1;

const LOCAL_SHIFT = PAGE_AXIS_SHIFT + VOXEME_AXIS_SHIFT;

const SENTINEL_INDEX = std.math.maxInt(u32);

const MAX_MATERIALS = std.math.maxInt(u12);

pub const Color = packed struct(u32) {
    r: u8,
    g: u8,
    b: u8,
    _: u8 = 0,
}; // RGB_8

pub const MaterialFlags = packed struct(u32) {
    // bit 0 (LSB): is_occluding; whether or not the material is occluding
    is_occluding: bool = false,
    _: u31 = 0,
};

pub const MaterialData = extern struct {
    color: Color = Color{ .r = 255, .g = 255, .b = 255 },
    flags: MaterialFlags = .{},
};

pub const MaterialId = enum(u12) { empty = 0, _ };

pub const Voxel = packed struct(u32) {
    material_id: MaterialId = .empty,
    state: u20 = 0,

    pub const empty = Voxel{
        .material_id = .empty,
        .state = 0,
    };
};

pub const PageData = extern struct {
    // The coordinate of this page in the infinite 3D grid of pages.
    coord: vec3i, // in grid space
    // The homogeneous voxel data for this page.
    // Even if the page contains voxemes, this is still the default voxel state for implicit voxemes.
    voxel: Voxel = .empty,
    /// index into voxeme map
    map_indirection: u32 = SENTINEL_INDEX,
};

pub const VoxemeData = extern struct {
    // The coordinate of this voxeme within its page.
    coord: vec3u, // in voxeme space
    // The homogeneous voxel data for this voxeme;
    // unlike page-level voxel data, this is only valid
    // if `buffer_indirection` is `sentinel_index`.
    voxel: Voxel = .empty,
    // index into voxel buffers
    buffer_indirection: u32 = SENTINEL_INDEX,
};

pub const GridCommand = union(enum) {
    set_aabb: struct {
        bounds: aabb3i,
        voxel: Voxel,
    },
    set_page: struct {
        coord: vec3i, // in grid space
        voxel: Voxel,
    },
    set_voxeme: struct {
        page_coord: vec3i, // in grid space
        voxeme_coord: vec3u, // in voxeme space
        voxel: Voxel,
    },
    set_voxel: struct {
        coord: vec3i, // in global voxel space
        voxel: Voxel,
    },
};

pub const MeshDescriptor = extern struct {
    coord: vec3i, // in grid space
    index: u32,
    length: u32,
};

pub const Vertex = extern struct {
    pos: vec3, // in world space
    material_id: u32,
    norm: vec3,
};

pub const AtomicValue = std.atomic.Value;

pub fn AtomicSet(comptime T: type) type {
    return struct {
        mutex: std.Thread.Mutex = .{},
        set: std.AutoArrayHashMapUnmanaged(T, void) = .empty,

        pub fn deinit(self: *AtomicSet(T), allocator: std.mem.Allocator) void {
            self.mutex.lock();
            self.set.deinit(allocator);
        }

        pub fn put(self: *AtomicSet(T), allocator: std.mem.Allocator, value: T) !void {
            self.mutex.lock();
            defer self.mutex.unlock();
            _ = try self.set.put(allocator, value, {});
        }

        pub fn remove(self: *AtomicSet(T), value: T) void {
            self.mutex.lock();
            defer self.mutex.unlock();
            _ = self.set.remove(value);
        }

        pub fn contains(self: *AtomicSet(T), value: T) bool {
            self.mutex.lock();
            defer self.mutex.unlock();
            return self.set.contains(value);
        }

        pub fn count(self: *AtomicSet(T)) usize {
            self.mutex.lock();
            defer self.mutex.unlock();
            return self.set.count();
        }

        pub fn take(self: *AtomicSet(T), out: []T) !void {
            self.mutex.lock();
            defer self.mutex.unlock();
            const keys = self.set.keys();
            if (keys.len > out.len) {
                @branchHint(.cold);
                return error.OutOfMemory;
            }
            @memcpy(out[0..keys.len], keys);
            self.set.clearRetainingCapacity();
        }
    };
}

pub fn AtomicStack(comptime T: type) type {
    return struct {
        mutex: std.Thread.Mutex = .{},
        array: std.ArrayList(T) = .empty,

        pub fn deinit(self: *AtomicStack(T), allocator: std.mem.Allocator) void {
            self.mutex.lock();
            self.array.deinit(allocator);
        }

        pub fn push(self: *AtomicStack(T), allocator: std.mem.Allocator, value: T) !void {
            self.mutex.lock();
            defer self.mutex.unlock();
            try self.array.append(allocator, value);
        }

        pub fn pop(self: *AtomicStack(T)) ?T {
            self.mutex.lock();
            defer self.mutex.unlock();
            return self.array.pop();
        }

        pub fn len(self: *AtomicStack(T)) usize {
            self.mutex.lock();
            defer self.mutex.unlock();
            return self.array.len;
        }

        pub fn take(self: *AtomicStack(T), out: []T) !void {
            self.mutex.lock();
            defer self.mutex.unlock();
            if (self.array.items.len > out.len) {
                @branchHint(.cold);
                return error.OutOfMemory;
            }
            @memcpy(out, self.array.items);
            self.array.clearRetainingCapacity();
        }
    };
}

pub const MAX_PAGES = 2_097_152;
pub const MAX_VOXEME_BUFFERS = 32_768;
pub const MAX_VOXEL_BUFFERS = 262_144;

allocator: std.mem.Allocator,

materials: []MaterialData,
material_count: AtomicValue(u32) = .init(0),

page_map: []u32,
page_buffer: []PageData,

voxeme_maps: [][VOXEMES_PER_PAGE]u32,
voxeme_buffer: []VoxemeData,

voxel_buffers: [][VOXELS_PER_VOXEME]Voxel,

free_pages: AtomicStack(u32) = .{},
free_voxeme_maps: AtomicStack(u32) = .{},
free_voxeme_buffers: AtomicStack(u32) = .{},
free_voxel_buffers: AtomicStack(u32) = .{},

page_count: AtomicValue(u32) = .init(0),
voxeme_count: AtomicValue(u32) = .init(0),
voxel_count: AtomicValue(u32) = .init(0), // (not individual voxels but sets of 4096)

commands: AtomicStack(GridCommand) = .{},
dirty_pages: AtomicSet(u32) = .{},
removed_pages: AtomicStack(u32) = .{},

pub fn init(allocator: std.mem.Allocator) !Grid {
    const materials = try allocator.alloc(MaterialData, MAX_MATERIALS);
    errdefer allocator.free(materials);

    const page_map = try allocator.alloc(u32, PAGE_TABLE_SIZE);
    errdefer allocator.free(page_map);

    const page_buffer = try allocator.alloc(PageData, MAX_PAGES);
    errdefer allocator.free(page_buffer);

    const voxeme_maps = try allocator.alloc([VOXEMES_PER_PAGE]u32, MAX_VOXEME_BUFFERS);
    errdefer allocator.free(voxeme_maps);

    const voxeme_buffer = try allocator.alloc(VoxemeData, MAX_VOXEME_BUFFERS);
    errdefer allocator.free(voxeme_buffer);

    const voxel_buffers = try allocator.alloc([VOXELS_PER_VOXEME]Voxel, MAX_VOXEL_BUFFERS);
    errdefer allocator.free(voxel_buffers);

    @memset(materials, .{});
    @memset(page_map, SENTINEL_INDEX);
    @memset(page_buffer, .{ .coord = .{ 0, 0, 0 } });
    @memset(voxeme_maps, [1]u32{SENTINEL_INDEX} ** VOXEMES_PER_PAGE);
    @memset(voxeme_buffer, .{ .coord = .{ 0, 0, 0 } });
    @memset(voxel_buffers, [1]Voxel{.empty} ** VOXELS_PER_VOXEME);

    return Grid{
        .allocator = allocator,
        .materials = materials,
        .page_map = page_map,
        .page_buffer = page_buffer,
        .voxeme_maps = voxeme_maps,
        .voxeme_buffer = voxeme_buffer,
        .voxel_buffers = voxel_buffers,
    };
}

pub fn deinit(self: *Grid) void {
    self.allocator.free(self.materials);
    self.allocator.free(self.page_map);
    self.allocator.free(self.page_buffer);
    self.allocator.free(self.voxeme_maps);
    self.allocator.free(self.voxeme_buffer);
    self.allocator.free(self.voxel_buffers);
    self.free_pages.deinit(self.allocator);
    self.free_voxeme_maps.deinit(self.allocator);
    self.free_voxeme_buffers.deinit(self.allocator);
    self.free_voxel_buffers.deinit(self.allocator);
    self.commands.deinit(self.allocator);
    self.dirty_pages.deinit(self.allocator);
    self.removed_pages.deinit(self.allocator);
    self.* = undefined;
}

pub fn gridToPageMap(coord: vec3i) u32 {
    const wrap = @as(vec3u, @bitCast(coord)) & @as(vec3u, @splat(GRID_AXIS_MASK));
    return wrap[0] + (wrap[1] << GRID_AXIS_SHIFT) + (wrap[2] << (2 * GRID_AXIS_SHIFT));
}

pub fn worldToGlobalVoxel(world_pos: vec3) vec3i {
    return @intFromFloat(@floor(world_pos * @as(vec3, @splat(INV_VOXEL_SCALE))));
}

pub fn globalVoxelToWorld(voxel_coord: vec3i) vec3 {
    return vec3(voxel_coord) * vec3(VOXEL_SCALE);
}

pub fn globalVoxelToPageCoord(voxel_coord: vec3i) vec3i {
    return voxel_coord >> @splat(LOCAL_SHIFT);
}

pub fn globalVoxelToVoxemeCoord(voxel_coord: vec3i) vec3u {
    return (@as(vec3u, @bitCast(voxel_coord)) >> @splat(VOXEME_AXIS_SHIFT)) & @as(vec3u, @splat(PAGE_AXIS_MASK));
}

pub fn globalVoxelToLocalVoxelCoord(voxel_coord: vec3i) vec3u {
    return @as(vec3u, @bitCast(voxel_coord)) & @as(vec3u, @splat(VOXEME_AXIS_MASK));
}

pub fn localVoxemeCoordToIndex(local_coord: vec3u) u32 {
    return local_coord[0] + (local_coord[1] << PAGE_AXIS_SHIFT) + (local_coord[2] << (2 * PAGE_AXIS_SHIFT));
}

pub fn indexToLocalVoxemeCoord(index: u32) vec3u {
    const x = index & PAGE_AXIS_MASK;
    const y = (index >> PAGE_AXIS_SHIFT) & PAGE_AXIS_MASK;
    const z = (index >> (2 * PAGE_AXIS_SHIFT)) & PAGE_AXIS_MASK;
    return vec3u{ x, y, z };
}

pub fn localVoxelCoordToIndex(local_coord: vec3u) u32 {
    return local_coord[0] + (local_coord[1] << VOXEME_AXIS_SHIFT) + (local_coord[2] << (2 * VOXEME_AXIS_SHIFT));
}

pub fn indexToLocalVoxelCoord(index: u32) vec3u {
    const x = index & VOXEME_AXIS_MASK;
    const y = (index >> VOXEME_AXIS_SHIFT) & VOXEME_AXIS_MASK;
    const z = (index >> (2 * VOXEME_AXIS_SHIFT)) & VOXEME_AXIS_MASK;
    return vec3u{ x, y, z };
}

fn allocatePage(self: *Grid) !u32 {
    if (self.free_pages.pop()) |idx| {
        @branchHint(.likely);
        return idx;
    } else {
        const new_idx = self.page_count.fetchAdd(1, .seq_cst);
        if (new_idx >= MAX_PAGES) {
            @branchHint(.cold);
            return error.OutOfMemory;
        }
        return new_idx;
    }
}

fn allocateVoxemeMap(self: *Grid) !u32 {
    if (self.free_voxeme_maps.pop()) |idx| {
        @branchHint(.likely);
        return idx;
    } else {
        const new_idx = self.voxeme_count.fetchAdd(1, .seq_cst);
        if (new_idx >= MAX_VOXEME_BUFFERS) {
            @branchHint(.cold);
            return error.OutOfMemory;
        }
        return new_idx;
    }
}

fn allocateVoxeme(self: *Grid) !u32 {
    if (self.free_voxeme_buffers.pop()) |idx| {
        @branchHint(.likely);
        return idx;
    } else {
        const new_idx = self.voxeme_count.fetchAdd(1, .seq_cst);
        if (new_idx >= MAX_VOXEME_BUFFERS) {
            @branchHint(.cold);
            return error.OutOfMemory;
        }
        return new_idx;
    }
}

fn allocateVoxelBuffer(self: *Grid) !u32 {
    if (self.free_voxel_buffers.pop()) |idx| {
        @branchHint(.likely);
        return idx;
    } else {
        const new_idx = self.voxel_count.fetchAdd(1, .seq_cst);
        if (new_idx >= MAX_VOXEL_BUFFERS) {
            @branchHint(.cold);
            return error.OutOfMemory;
        }
        return new_idx;
    }
}

fn freePageVoxemesAndVoxels(self: *Grid, page_idx: u32) !void {
    const voxeme_map_idx = self.page_buffer[page_idx].map_indirection;

    if (self.page_buffer[page_idx].map_indirection == SENTINEL_INDEX) return;

    const voxeme_map = &self.voxeme_maps[voxeme_map_idx];

    for (0..VOXEMES_PER_PAGE) |i| {
        const voxeme_buffer_idx = voxeme_map[i];

        if (voxeme_buffer_idx == SENTINEL_INDEX) continue;

        try self.freeVoxemeVoxels(voxeme_buffer_idx);

        try self.free_voxeme_buffers.push(self.allocator, voxeme_buffer_idx);
    }

    try self.free_voxeme_maps.push(self.allocator, voxeme_map_idx);
}

fn freeVoxemeVoxels(self: *Grid, voxeme_buffer_idx: u32) !void {
    const voxeme = self.voxeme_buffer[voxeme_buffer_idx];

    if (voxeme.buffer_indirection == SENTINEL_INDEX) return;

    try self.free_voxel_buffers.push(self.allocator, voxeme.buffer_indirection);
}

pub fn runCommands(self: *Grid, commands: []const GridCommand) !void {
    for (commands) |command| {
        switch (command) {
            .set_aabb => |_| {
                @panic("TODO");
            },
            .set_page => |cmd| {
                try self.setPage(cmd.coord, cmd.voxel);
            },
            .set_voxeme => |cmd| {
                try self.setVoxeme(cmd.page_coord, cmd.voxeme_coord, cmd.voxel);
            },
            .set_voxel => |cmd| {
                try self.setVoxel(cmd.coord, cmd.voxel);
            },
        }
    }
}

fn setPage(self: *Grid, coord: vec3i, voxel: Voxel) !void {
    const page_table_idx = gridToPageMap(coord);

    var page_buffer_idx = self.page_map[page_table_idx];

    if (page_buffer_idx == SENTINEL_INDEX) { // new page
        if (voxel.material_id == .empty) { // empty to empty, no-op
            return;
        }
        page_buffer_idx = try self.allocatePage();

        self.page_map[page_table_idx] = page_buffer_idx;
        self.page_buffer[page_buffer_idx] = PageData{
            .coord = coord,
            .voxel = voxel,
        };

        try self.dirty_pages.put(self.allocator, page_buffer_idx);
    } else if (voxel.material_id == .empty) { // non-empty to empty, deallocate
        try self.freePageVoxemesAndVoxels(page_buffer_idx);

        self.page_map[page_table_idx] = SENTINEL_INDEX;
        try self.free_pages.push(self.allocator, page_buffer_idx);

        try self.removed_pages.push(self.allocator, page_buffer_idx);
    } else if (self.page_buffer[page_buffer_idx].map_indirection != SENTINEL_INDEX or voxel.material_id != self.page_buffer[page_buffer_idx].voxel.material_id) { // non-empty unmatched/non-homogeneous to non-empty homogeneous, update voxel
        try self.freePageVoxemesAndVoxels(page_buffer_idx);

        self.page_buffer[page_buffer_idx].voxel = voxel;

        try self.dirty_pages.put(self.allocator, page_buffer_idx);
    } else { // same material, no-op
        return;
    }

    comptime var offset = vec3i{ -1, -1, -1 };
    inline while (offset[0] <= 1) : (offset[0] += 1) {
        offset[1] = -1;
        inline while (offset[1] <= 1) : (offset[1] += 1) {
            offset[2] = -1;
            inline while (offset[2] <= 1) : (offset[2] += 1) {
                if (@reduce(.Add, @abs(offset)) != 1) continue;

                const neighbor_coord = coord + offset;
                const neighbor_page_map_idx = gridToPageMap(neighbor_coord);
                const neighbor_page_idx = self.page_map[neighbor_page_map_idx];
                if (neighbor_page_idx != SENTINEL_INDEX) {
                    try self.dirty_pages.put(self.allocator, neighbor_page_idx);
                }
            }
        }
    }
}

fn setVoxeme(self: *Grid, page_coord: vec3i, voxeme_coord: vec3u, voxel: Voxel) !void {
    const page_map_idx = gridToPageMap(page_coord);

    var page_buffer_idx = self.page_map[page_map_idx];
    if (page_buffer_idx == SENTINEL_INDEX) {
        if (voxel.material_id == .empty) { // empty to empty, no-op
            return;
        }

        // allocate new page and voxeme data
        page_buffer_idx = try self.allocatePage();
        const voxeme_map_idx = try self.allocateVoxemeMap();
        const voxeme_buffer_idx = try self.allocateVoxeme();
        const local_voxeme_idx = localVoxemeCoordToIndex(voxeme_coord);

        @memset(&self.voxeme_maps[voxeme_map_idx], SENTINEL_INDEX);

        self.page_map[page_map_idx] = page_buffer_idx;
        self.page_buffer[page_buffer_idx] = PageData{
            .coord = page_coord,
            .map_indirection = voxeme_map_idx,
        };
        self.voxeme_maps[voxeme_map_idx][local_voxeme_idx] = voxeme_buffer_idx;
        self.voxeme_buffer[voxeme_buffer_idx] = VoxemeData{
            .coord = voxeme_coord,
            .voxel = voxel,
        };

        try self.dirty_pages.put(self.allocator, page_buffer_idx);
    } else if (self.page_buffer[page_buffer_idx].map_indirection == SENTINEL_INDEX) {
        if (voxel == self.page_buffer[page_buffer_idx].voxel) {
            // same voxel data, no-op
            return;
        }

        // convert homogeneous page to heterogeneous page
        const voxeme_map_idx = try self.allocateVoxemeMap();
        const voxeme_buffer_idx = try self.allocateVoxeme();

        @memset(&self.voxeme_maps[voxeme_map_idx], SENTINEL_INDEX);

        // populate existing voxeme data
        const local_voxeme_idx = localVoxemeCoordToIndex(voxeme_coord);

        self.page_buffer[page_buffer_idx].map_indirection = voxeme_map_idx;
        self.voxeme_maps[voxeme_map_idx][local_voxeme_idx] = voxeme_buffer_idx;
        self.voxeme_buffer[voxeme_buffer_idx] = VoxemeData{
            .coord = voxeme_coord,
            .voxel = voxel,
        };

        try self.dirty_pages.put(self.allocator, page_buffer_idx);
    } else {
        const voxeme_map_idx = self.page_buffer[page_buffer_idx].map_indirection;
        const local_voxeme_idx = localVoxemeCoordToIndex(voxeme_coord);
        var voxeme_buffer_idx = self.voxeme_maps[voxeme_map_idx][local_voxeme_idx];

        if (voxeme_buffer_idx == SENTINEL_INDEX) {
            if (voxel == self.page_buffer[page_buffer_idx].voxel) {
                // same voxel data, no-op
                return;
            }

            // allocate new voxeme
            voxeme_buffer_idx = try self.allocateVoxeme();

            self.voxeme_maps[voxeme_map_idx][local_voxeme_idx] = voxeme_buffer_idx;
            self.voxeme_buffer[voxeme_buffer_idx] = VoxemeData{
                .coord = voxeme_coord,
                .voxel = voxel,
            };

            try self.dirty_pages.put(self.allocator, page_buffer_idx);
        } else if (voxel == self.page_buffer[page_buffer_idx].voxel) {
            // same voxel data as page, deallocate existing voxeme
            try self.freeVoxemeVoxels(voxeme_buffer_idx);

            self.voxeme_maps[voxeme_map_idx][local_voxeme_idx] = SENTINEL_INDEX;

            try self.free_voxeme_buffers.push(self.allocator, voxeme_buffer_idx);
            try self.dirty_pages.put(self.allocator, page_buffer_idx);
        } else if (self.voxeme_buffer[voxeme_buffer_idx].buffer_indirection != SENTINEL_INDEX or voxel != self.voxeme_buffer[voxeme_buffer_idx].voxel) {
            // update existing voxeme
            try self.freeVoxemeVoxels(voxeme_buffer_idx);

            self.voxeme_buffer[voxeme_buffer_idx].voxel = voxel;

            try self.dirty_pages.put(self.allocator, page_buffer_idx);
        } else {
            // same voxel data, no-op
            return;
        }
    }
}

fn setVoxel(self: *Grid, coord: vec3i, voxel: Voxel) !void {
    const page_coord = globalVoxelToPageCoord(coord);
    const voxeme_coord = globalVoxelToVoxemeCoord(coord);
    const local_voxel_coord = globalVoxelToLocalVoxelCoord(coord);

    const page_map_idx = gridToPageMap(page_coord);

    var page_buffer_idx = self.page_map[page_map_idx];
    if (page_buffer_idx == SENTINEL_INDEX) {
        if (voxel.material_id == .empty) { // empty to empty, no-op
            return;
        }

        // allocate new page, voxeme, and voxel data
        page_buffer_idx = try self.allocatePage();
        const voxeme_map_idx = try self.allocateVoxemeMap();
        const voxeme_buffer_idx = try self.allocateVoxeme();
        const voxel_buffer_idx = try self.allocateVoxelBuffer();
        const local_voxeme_idx = localVoxemeCoordToIndex(voxeme_coord);
        const local_voxel_idx = localVoxelCoordToIndex(local_voxel_coord);

        @memset(&self.voxeme_maps[voxeme_map_idx], SENTINEL_INDEX);
        @memset(&self.voxel_buffers[voxel_buffer_idx], .empty);

        self.page_map[page_map_idx] = page_buffer_idx;
        self.page_buffer[page_buffer_idx] = PageData{
            .coord = page_coord,
            .map_indirection = voxeme_map_idx,
        };
        self.voxeme_maps[voxeme_map_idx][local_voxeme_idx] = voxeme_buffer_idx;
        self.voxeme_buffer[voxeme_buffer_idx] = VoxemeData{
            .coord = voxeme_coord,
            .buffer_indirection = voxel_buffer_idx,
        };
        self.voxel_buffers[voxel_buffer_idx][local_voxel_idx] = voxel;

        try self.dirty_pages.put(self.allocator, page_buffer_idx);
    } else if (self.page_buffer[page_buffer_idx].map_indirection == SENTINEL_INDEX) {
        if (voxel == self.page_buffer[page_buffer_idx].voxel) {
            // same voxel data, no-op
            return;
        }

        const local_voxeme_idx = localVoxemeCoordToIndex(voxeme_coord);
        const local_voxel_idx = localVoxelCoordToIndex(local_voxel_coord);

        const voxeme_map_idx = try self.allocateVoxemeMap();
        const voxeme_buffer_idx = try self.allocateVoxeme();
        const voxel_buffer_idx = try self.allocateVoxelBuffer();

        @memset(&self.voxeme_maps[voxeme_map_idx], SENTINEL_INDEX);
        @memset(&self.voxel_buffers[voxel_buffer_idx], .empty);

        // populate existing voxeme and voxel data
        self.page_buffer[page_buffer_idx].map_indirection = voxeme_map_idx;
        self.voxeme_maps[voxeme_map_idx][local_voxeme_idx] = voxeme_buffer_idx;
        self.voxeme_buffer[voxeme_buffer_idx] = VoxemeData{
            .coord = voxeme_coord,
            .buffer_indirection = voxel_buffer_idx,
        };
        self.voxel_buffers[voxel_buffer_idx][local_voxel_idx] = voxel;
        try self.dirty_pages.put(self.allocator, page_buffer_idx);
    } else {
        const voxeme_map_idx = self.page_buffer[page_buffer_idx].map_indirection;
        const local_voxeme_idx = localVoxemeCoordToIndex(voxeme_coord);
        var voxeme_buffer_idx = self.voxeme_maps[voxeme_map_idx][local_voxeme_idx];

        if (voxeme_buffer_idx == SENTINEL_INDEX) {
            if (voxel == self.page_buffer[page_buffer_idx].voxel) {
                // same voxel data, no-op
                return;
            }

            const local_voxel_idx = localVoxelCoordToIndex(local_voxel_coord);

            voxeme_buffer_idx = try self.allocateVoxeme();
            const voxel_buffer_idx = try self.allocateVoxelBuffer();

            @memset(&self.voxel_buffers[voxel_buffer_idx], .empty);

            self.voxeme_maps[voxeme_map_idx][local_voxeme_idx] = voxeme_buffer_idx;
            self.voxeme_buffer[voxeme_buffer_idx] = VoxemeData{
                .coord = voxeme_coord,
                .buffer_indirection = voxel_buffer_idx,
            };
            self.voxel_buffers[voxel_buffer_idx][local_voxel_idx] = voxel;

            try self.dirty_pages.put(self.allocator, page_buffer_idx);
        } else {
            const voxeme = &self.voxeme_buffer[voxeme_buffer_idx];

            if (voxeme.buffer_indirection == SENTINEL_INDEX) {
                if (voxel == voxeme.voxel) {
                    // same voxel data, no-op
                    return;
                }

                // convert homogeneous voxeme to heterogeneous voxeme
                const voxel_buffer_idx = try self.allocateVoxelBuffer();
                @memset(&self.voxel_buffers[voxel_buffer_idx], .empty);

                voxeme.buffer_indirection = voxel_buffer_idx;
            }

            const local_voxel_idx = localVoxelCoordToIndex(local_voxel_coord);
            self.voxel_buffers[voxeme.buffer_indirection][local_voxel_idx] = voxel;

            try self.dirty_pages.put(self.allocator, page_buffer_idx);
        }
    }
}
