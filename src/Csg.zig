const Csg = @This();

const std = @import("std");
const bvh = @import("bvh.zig");
const slot_map = @import("slot_map.zig");

const linalg = @import("linalg.zig");
const aabb3 = linalg.aabb3;
const vec2 = linalg.vec2;
const vec3 = linalg.vec3;
const vec4 = linalg.vec4;

const epsilon = std.math.floatEps(f32);

test {
    std.testing.refAllDecls(@This());
}

pub const Vertex = vec3;
pub const VertexIndex = u32;

pub const Category = enum {
    inside,
    outside,
    aligned,
    rev_aligned,
};

pub const Operation = enum {
    additive,
    subtractive,
    intersecting,
};

pub const QuantizedVec3 = struct {
    x: i32,
    y: i32,
    z: i32,

    // If 1.0 = 1 meter, 1000.0 quantizes to the nearest millimeter.
    pub const resolution: f32 = 1000.0;

    pub fn quantize(v: vec3) QuantizedVec3 {
        return .{
            .x = @as(i32, @intFromFloat(@round(v[0] * resolution))),
            .y = @as(i32, @intFromFloat(@round(v[1] * resolution))),
            .z = @as(i32, @intFromFloat(@round(v[2] * resolution))),
        };
    }

    pub fn toVec3(self: QuantizedVec3) vec3 {
        return vec3{
            @as(f32, @floatFromInt(self.x)) / resolution,
            @as(f32, @floatFromInt(self.y)) / resolution,
            @as(f32, @floatFromInt(self.z)) / resolution,
        };
    }
};

pub const VertexPool = struct {
    allocator: std.mem.Allocator,
    // Stores the actual contiguous vertex data
    vertices: std.ArrayList(vec3) = .empty,

    // Maps a quantized coordinate to a vertex index
    index_map: std.AutoHashMapUnmanaged(QuantizedVec3, u32) = .empty,

    pub fn internVertex(self: *VertexPool, position: vec3) !u32 {
        const q_pos = QuantizedVec3.quantize(position);

        const gop = try self.index_map.getOrPut(self.allocator, q_pos);

        if (!gop.found_existing) {
            errdefer _ = self.index_map.remove(q_pos);

            const new_index = @as(u32, @intCast(self.vertices.items.len));
            gop.value_ptr.* = new_index;

            try self.vertices.append(self.allocator, position);
        }

        return gop.value_ptr.*;
    }
};

pub const Edge = packed struct {
    a: VertexIndex,
    b: VertexIndex,
};

pub const Plane = struct {
    normal: vec3,
    distance: f32,

    pub fn eql(self: Plane, other: Plane) bool {
        return linalg.approxEqAbs(self.normal, other.normal, epsilon) and std.math.approxEqAbs(f32, self.distance, other.distance, epsilon);
    }

    pub fn basePoly(self: Plane, pool: *VertexPool, source: Brush.Ref, edge_length: f32) !Polygon {
        const out = .{
            .plane = self,
            .category = .aligned,
            .edges = try std.ArrayList(Edge).initCapacity(pool.allocator, 4),
            .source_brush = source,
        };

        for (0..4) |i| {
            const angle = @as(f32, @floatFromInt(i)) * std.math.pi / 2;
            const dir = vec3{
                .x = @cos(angle),
                .y = @sin(angle),
                .z = 0,
            };
            const vertex_offset = dir * edge_length;

            const ax = self.normal * self.distance + vertex_offset;
            const bx = self.normal * self.distance - vertex_offset;

            try out.edges.append(pool.allocator, .{
                .a = try pool.internVertex(ax),
                .b = try pool.internVertex(bx),
            });
        }

        return out;
    }
};

pub const Polygon = struct {
    plane: Plane,
    category: Category,
    edges: std.ArrayList(Edge) = .empty,
    source_brush: Brush.Ref,
};

pub const NGon = struct {
    wind: Winding,
    vertices: std.ArrayList(VertexIndex) = .empty,

    pub const Winding = enum {
        outside,
        inside,
    };

    pub const Row = struct { plane: Plane, ngons: std.ArrayList(NGon) };
};

pub const Mesh = struct {
    vertices: []Vertex = &.{},
    indices: []VertexIndex = &.{},

    pub fn deinit(self: *Mesh, allocator: std.mem.Allocator) void {
        allocator.free(self.vertices);
        allocator.free(self.indices);
    }
};

pub const Node = union(enum) {
    leaf: Brush.Ref,
    operator: Operator,

    pub const Pool = std.heap.MemoryPool(Data);

    pub const Data = struct {
        world: *Csg,
        parent: ParentRef,
        value: Node,
    };

    pub const ParentRef = union(enum) { none, world, node: *Node };

    pub fn getWorld(self: *Node) *Csg {
        return self.getData().world;
    }

    pub fn getParent(self: *Node) ParentRef {
        return self.getData().parent;
    }

    fn setParent(self: *Node, parent: Node.ParentRef) void {
        const data = self.getData();
        switch (parent) {
            .none => {},
            .world => {
                std.debug.assert(data.parent == .none);
            },
            .node => |node| {
                std.debug.assert(data.parent == .none);
                std.debug.assert(data.world == node.getWorld());
            },
        }
        data.parent = parent;
    }

    pub fn containsBrush(self: *Node, target: Brush.Ref) bool {
        return switch (self.*) {
            .leaf => |ref| ref == target,
            .operator => |*op| op.left.containsBrush(target) or op.right.containsBrush(target),
        };
    }

    pub fn replace(toReplace: *Node, replacement: *Node) !void {
        const world = toReplace.getWorld();
        std.debug.assert(world == replacement.getWorld());

        if (toReplace == replacement) return;

        const parent = toReplace.getParent();
        replacement.setParent(parent);

        const out: **Node = extract: switch (parent) {
            .none => {
                std.mem.swap(Node, toReplace, replacement);

                try replacement.remove();

                return;
            },
            .world => {
                std.debug.assert(world.root_node == toReplace);
                break :extract &world.root_node;
            },
            .node => |node| {
                std.debug.assert(node.operator.left == toReplace or node.operator.right == toReplace);
                break :extract if (node.operator.left == toReplace)
                    &node.operator.left
                else
                    &node.operator.right;
            },
        };

        if (out.* != replacement) {
            try out.*.deinit();
            out.*.destroy();
        }

        out.* = replacement;
    }

    pub fn take(toRemove: *Node) !void {
        toRemove.setParent(.none);

        switch (toRemove.getParent()) {
            .none => {},
            .world => |csg| {
                std.debug.assert(csg.root_node == toRemove);
                csg.root_node = null;
            },
            .node => |parent| {
                std.debug.assert(parent.operator.left == toRemove or parent.operator.right == toRemove);
                const sibling = if (parent.operator.left == toRemove) parent.operator.right else parent.operator.left;
                sibling.setParent(.none);
                try parent.replace(sibling);
            },
        }
    }

    pub fn remove(toRemove: *Node) !void {
        try toRemove.take();

        try toRemove.deinit();
        toRemove.destroy();
    }

    fn deinit(self: *Node) !void {
        const world = self.getWorld();

        switch (self.*) {
            .leaf => |ref| {
                const brush = world.brush_map.remove(ref).?;
                _ = (try world.brush_bvh.remove(brush.id)).?;
                try brush.deinit(world);
            },
            .operator => |*op| {
                try op.left.deinit();
                try op.right.deinit();
            },
        }
    }

    fn destroy(self: *Node) void {
        const data = self.getData();
        data.world.node_pool.destroy(data);
    }

    fn getData(self: *Node) *Data {
        return @fieldParentPtr("value", self);
    }
};

pub const Brush = struct {
    id: Id,
    operation: Operation = .additive,
    planes: std.ArrayList(Plane) = .empty,
    mesh_cache: ?*Mesh = null,

    pub const SlotMap = slot_map.new(Brush);
    pub const BVH = bvh.Octree(Ref);
    pub const Id = BVH.Id;
    pub const Ref = slot_map.Ref;
    pub const Bounds = BVH.Bounds;

    pub fn deinit(self: *Brush, world: *Csg) void {
        if (self.mesh_cache) |m| m.deinit(world.brush_bvh.allocator);
        self.planes.deinit(world.brush_bvh.allocator);
    }
};

pub const Operator = struct {
    operation: Operation,
    left: *Node,
    right: *Node,
};

brush_bvh: Brush.BVH,
brush_map: Brush.SlotMap,
node_pool: Node.Pool,
root_node: ?*Node = null,

pub fn init(allocator: std.mem.Allocator) Csg {
    return .{ .brush_bvh = Brush.BVH.init(allocator) };
}

pub fn deinit(self: *Csg) void {
    self.brush_map.deinit(self.brush_bvh.allocator);
    self.brush_bvh.deinit();
    self.* = undefined;
}

pub fn setRootNode(self: *Csg, node: *Node) ?*Node {
    const old_root = self.root_node;
    if (old_root) |old| old.setParent(.none);

    node.setParent(.world);
    self.root_node = node;

    return old_root;
}

pub fn createOperator(world: *Csg, operation: Operation, left: *Node, right: *Node) !*Node {
    const data = try world.node_pool.create(world.brush_bvh.allocator);
    const node = &data.value;

    left.setParent(.{ .node = node });
    right.setParent(.{ .node = node });

    data.* = .{
        .world = world,
        .parent = .none,
        .value = .{ .operator = .{ .operation = operation, .left = left, .right = right } },
    };

    return node;
}

pub fn createBrush(world: *Csg, bounds: Brush.Bounds) !struct { Brush.Ref, *Node } {
    const ref, const ptr = try world.brush_map.create(world.brush_bvh.allocator);
    errdefer _ = world.brush_map.remove(ref);
    ptr.* = Brush{ .id = try world.brush_bvh.insert(bounds, ref) };
    const node = try world.node_pool.create(world.brush_bvh.allocator);
    node.* = .{
        .world = world,
        .parent = .none,
        .value = .{ .leaf = ref },
    };
    return .{ ref, node };
}

pub fn getOverlappingBrushes(world: *Csg, bounds: Brush.Bounds, allocator: std.mem.Allocator, out: *std.ArrayList(Brush.BVH.ColumnRef)) !void {
    try world.brush_bvh.queryBounds(allocator, out, bounds);
}

pub fn getBounds(world: *Csg, ref: Brush.Ref) Brush.Bounds {
    const brush = world.brush_map.get(ref).?;
    return world.brush_bvh.getBounds(brush.id).?;
}

pub fn setBounds(world: *Csg, ref: Brush.Ref, new_bounds: Brush.Bounds) !void {
    const brush = world.brush_map.get(ref).?;
    try world.brush_bvh.updateBounds(brush.id, new_bounds);
}

pub fn getOperation(world: *Csg, ref: Brush.Ref) Operation {
    const brush = world.brush_map.get(ref).?;
    return brush.operation;
}

pub fn setOperation(world: *Csg, ref: Brush.Ref, operation: Operation) void {
    const brush = world.brush_map.get(ref).?;
    brush.operation = operation;
}

pub fn getPlanes(world: *Csg, ref: Brush.Ref) []const Plane {
    const brush = world.brush_map.get(ref).?;
    return brush.planes.items;
}

pub fn mutPlanes(world: *Csg, ref: Brush.Ref) *std.ArrayList(Plane) {
    const brush = world.brush_map.get(ref).?;
    return &brush.planes;
}

pub fn remeshBrush(world: *Csg, ref: Brush.Ref) !void {
    const brush = world.brush_map.get(ref).?;
    const brush_bounds = world.brush_bvh.getBounds(brush.id).?;

    var arena = std.heap.ArenaAllocator.init(world.brush_bvh.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var intersectingBrushes: std.ArrayList(Brush.Ref) = .empty;
    var pool = VertexPool{ .allocator = allocator };
    var generatedPolygons: std.ArrayList(Polygon) = .empty;
    var keptPolygons: std.ArrayList(Polygon) = .empty;
    var groups: std.ArrayList(Polygon) = .empty;
    var ngon_sets: std.MultiArrayList(NGon.Row) = .empty;
    var vertices: std.ArrayList(Vertex) = .empty;
    var indices: std.ArrayList(VertexIndex) = .empty;
    var map: std.AutoHashMapUnmanaged(VertexIndex, VertexIndex) = .empty;

    // Build the local tree (optimizing away branches that don't touch this brush)
    const localTreeRoot = try world.buildPerBrushTree(brush_bounds, world.root_node, allocator, &intersectingBrushes) orelse @panic("Failed to build per-brush tree at root");

    // Generate initial geometry via Analytic Intersections
    // (Instead of recursive slicing, find exact overlapping areas)
    for (intersectingBrushes.items) |ref2| {
        if (ref == ref2) continue;

        // Calculate precise overlap polygons on the planes of the active brush
        world.calculateAnalyticIntersection(&pool, &generatedPolygons, ref, ref2);
    }

    // Determine if polygons are Inside, Outside, or Surface
    for (generatedPolygons.items) |*poly| {
        poly.category = world.routePolygon(localTreeRoot, poly, pool.vertices.items);
    }

    // Discard polygons that are buried or floating in space
    for (generatedPolygons.items) |*poly| {
        if (poly.category == .aligned or poly.category == .rev_aligned) {
            try keptPolygons.append(allocator, poly);
        }
    }

    // Group polygons' edges by plane and category
    sort_loop: for (keptPolygons.items) |poly| {
        for (groups.items) |group| {
            // FIXME: This is incorrect.
            // when we have a hole in a face, we can end up with two sets of overlapping coplanar edges which do not have the same source brush or category,
            // those overlapping edges should be removed below, but this will prevent that. we should just be grouping by plane, it seems. but we need to know category later...?
            // probably need to refactor this and the next two steps into a single phase that removes edges while grouping by plane and flipping etc
            if (group.category == poly.category and group.plane.eql(poly.plane) and group.source_brush == poly.source_brush) {
                try group.edges.appendSlice(allocator, poly.edges);
                continue :sort_loop;
            }
        }

        try groups.append(allocator, poly);
    }

    // Clean up edges
    for (groups.items) |poly| {
        const edges = &poly.edges;

        var i: usize = 0;
        while (i < edges.items.len - 1) {
            const slice = edges.items[i + 1 ..];

            const base = edges.items[i];

            if (for (slice, 0..) |edge, e| if (edge.a == base.b and edge.b == base.a) break e else null) |j| {
                // We found a pair of edges that are exact opposites (A->B and B->A).
                // This means they are self-cancelling internal edges between two coplanar polygons, and can be removed.
                _ = edges.orderedRemove(i + j);
                _ = edges.orderedRemove(i);
            } else if (for (slice, 0..) |edge, e| if (edge == base) break e else null) |j| {
                // We can remove the duplicate edge at j
                _ = edges.orderedRemove(i + j);
            } else {
                // Keep it and move on.
                i += 1;
            }
        }
    }

    // Whatever edges are left form the perfect outer boundary/boundaries
    // of the combined shape. Stitch them into N-gons.
    for (groups) |poly| {
        for (poly.edges) |edge| try map.put(allocator, edge.a, edge.b);
        defer map.clearRetainingCapacity();

        const start = poly.edges.items[0].a;
        var current: ?u32 = poly.edges.items[0].b;

        var ngon = NGon{
            .wind = switch (poly.category) {
                .aligned => .outside,
                .rev_aligned => .inside,
                else => std.debug.panic("Only aligned polygons can be turned into ngons, found category `{any}`", .{poly.category}),
            },
        };

        try ngon.vertices.append(allocator, start);

        while (current) |c| {
            if (c == start) break;
            try ngon.vertices.append(allocator, c);
            current = map.get(c);
        } else {
            std.debug.panic("Failed to stitch ngon for poly `{any}`: edge list is not a closed loop", .{poly});
        }

        if (poly.category == .rev_aligned) {
            std.mem.reverse(VertexIndex, ngon.vertices.items);
        }

        for (ngon_sets.items(.plane), ngon_sets.items(.ngons)) |plane, *set| {
            if (plane.eql(poly.plane)) {
                try set.append(allocator, ngon);
                break;
            }
        } else {
            var x = try std.ArrayList(NGon).initCapacity(allocator, 1);
            x.appendAssumeCapacity(ngon);
            try ngon_sets.append(allocator, .{ .plane = poly.plane, .ngons = x });
        }
    }

    // Triangulate
    try triangulateNgonSets(allocator, ngon_sets.items(.ngons), &vertices, &indices);

    if (vertices.items.len == 0 or indices.items.len == 0) {
        brush.mesh = null;
    } else {
        brush.mesh = Mesh{
            .vertices = try world.brush_bvh.allocator.dupe(Vertex, vertices.items),
            .indices = try world.brush_bvh.allocator.dupe(VertexIndex, indices.items),
        };
    }
}

fn routePolygon(world: *Csg, node: *Node, poly: *Polygon, vertices: []const Vertex) Category {
    switch (node.*) {
        .leaf => |leaf| {
            if (poly.source_brush == leaf) {
                // If the polygon came from this brush, it is definitely on the surface.
                return .aligned;
            }

            const brush = world.brush_map.get(leaf).?;

            // Check for coplanar surfaces (Aligned / Reverse-Aligned)
            for (brush.planes.items) |plane| {
                if (poly.plane.eql(plane)) {
                    return .aligned;
                }

                if (poly.plane.eql(-plane)) {
                    return .rev_aligned;
                }
            }

            // If it's not on the surface, it must be completely Inside or Outside.
            // Since the poly doesn't straddle any boundaries, we can just test ONE vertex.
            const test_point_idx = poly.edges.items[0].a;
            const test_point = vertices[test_point_idx];

            // A brush is a convex hull. If a point is in front of *any* single plane
            // of the hull, it is outside the volume.
            for (brush.planes.items) |plane| {
                const distance = linalg.dot(plane.normal, test_point) + plane.distance;
                if (distance > epsilon) return .outside;
            }

            // If the point was behind ALL planes, it is buried inside the volume.
            return .inside;
        },
        .operator => |*op| {
            const lcat: Category =
                if (!op.left.containsBrush(poly.source_brush))
                    world.routePolygon(op.left, poly, vertices)
                else
                    // The result of traversing its own branch is always Aligned.
                    .aligned;

            const rcat: Category =
                if (!op.right.containsBrush(poly.source_brush))
                    world.routePolygon(op.right, poly, vertices)
                else
                    .aligned;

            return switch (op.operation) {
                .additive => // (Left + Right)
                if (lcat == .inside or rcat == .inside)
                    // If it's inside either shape, it's inside the combined shape.
                    .inside
                else if ((lcat == .aligned and rcat == .outside) or
                    // If it's the surface of one, but outside the other, it stays the surface.
                    (rcat == .aligned and lcat == .outside))
                    .aligned
                else
                    // Everything else is floating in space (Outside).
                    .outside,

                .subtractive => // (Left - Right)
                if (lcat == .inside and rcat == .outside)
                    // If it's inside the Left, but outside the Right, it's still inside.
                    .inside
                else if (lcat == .aligned and rcat == .outside)
                    // Left surface that is outside the Right shape is kept.
                    .aligned
                else if (rcat == .aligned and lcat == .inside)
                    // Right surface that is inside the Left shape becomes the new
                    // interior wall. We return ReverseAligned so the normals flip inward!
                    .rev_aligned
                else // Everything else is either floating in space (Outside) or buried (Inside).
                    .outside,

                .intersecting => // (Left * Right)
                if (lcat == .inside and rcat == .inside)
                    // It is only inside if it is inside BOTH shapes.
                    .inside
                else if (lcat == .aligned and rcat == .inside)
                    // Left surface is kept only if it's inside the Right shape.
                    .aligned
                else if (rcat == .aligned and lcat == .inside)
                    // Right surface is kept only if it's inside the Left shape.
                    .aligned
                else
                    // Everything else is either floating in space (Outside) or buried (Inside).
                    .outside,
            };
        },
    }
}

// Recursively walk the global tree from the root, cloning nodes;
// while collapsing branches that don't intersect the active brush's bounding box.
// NOTE: nodes created this way should NOT be `deinit`'d, and the allocator provided should thus be an arena.
fn buildPerBrushTree(world: *Csg, bounds: Brush.Bounds, node: *Node, allocator: std.mem.Allocator, intersecting_brushes: *std.ArrayList(Brush.Ref)) !?*Node {
    switch (node.*) {
        .leaf => |ref| {
            const brush = world.brush_map.get(ref).?;

            const brush_bounds = world.brush_bvh.getBounds(brush.id).?;
            if (!brush_bounds.intersects(bounds)) {
                return null;
            }

            try intersecting_brushes.append(world.brush_bvh.allocator, ref);

            return node;
        },
        .operator => |*op| {
            const left_node = try world.buildPerBrushTree(bounds, op.left, allocator, intersecting_brushes);
            const right_node = try world.buildPerBrushTree(bounds, op.right, allocator, intersecting_brushes);

            if (left_node == null) {
                if (right_node != null) {
                    return switch (op.operation) {
                        .additive => right_node, // (Empty + Right) -> Just Right
                        .subtractive => null, // (Empty - Right) -> Empty
                        .intersecting => null, // (Empty * Right) -> Empty
                    };
                }

                return null;
            } else if (right_node == null) {
                return switch (op.operation) {
                    .additive => left_node, // (Left + Empty) -> Just Left
                    .subtractive => left_node, // (Left - Empty) -> Just Left
                    .intersecting => null, // (Left * Empty) -> Empty
                };
            }

            if (left_node.? == op.left and right_node.? == op.right) {
                // If both branches are unchanged, we can just reuse this node and avoid an unnecessary allocation.
                return node;
            }

            // Both sides contain brushes that overlap our target area.
            // We must keep the operation node intact.
            const new_op = try allocator.create(Node);
            new_op.* = .{
                .operator = .{
                    .operation = op.operation,
                    .left = left_node.?,
                    .right = right_node.?,
                },
            };

            return new_op;
        },
    }
}

fn calculateAnalyticIntersection(world: *Csg, pool: *VertexPool, out_polys: *std.ArrayList(Polygon), a: Brush.Ref, b: Brush.Ref) void {
    const brush_a = world.brush_map.get(a).?;
    const bounds_a = world.brush_bvh.getBounds(brush_a.id).?;

    const brush_b = world.brush_map.get(b).?;
    const bounds_b = world.brush_bvh.getBounds(brush_b.id).?;

    const start_len = out_polys.items.len;
    errdefer {
        for (out_polys.items[start_len..]) |*poly| poly.edges.deinit(pool.allocator);
        out_polys.shrinkRetainingCapacity(start_len);
    }

    // Find the parts of Brush A that are inside Brush B
    for (brush_a.planes.items) |plane| {
        var poly = try plane.basePoly(pool, a, bounds_a.diagonalLength());
        errdefer poly.edges.deinit(pool.allocator);

        try clipPolygonAgainstBrush(pool, &poly, brush_b);

        if (poly.edges.items.len >= 3) {
            try out_polys.append(world.brush_bvh.allocator, poly);
        } else {
            poly.edges.deinit(pool.allocator);
        }
    }

    // Find the parts of Brush B that are inside Brush A
    for (brush_b.planes.items) |plane| {
        var poly = try plane.basePoly(pool, b, bounds_b.diagonalLength());
        errdefer poly.edges.deinit(pool.allocator);

        try clipPolygonAgainstBrush(pool, &poly, brush_a);

        if (poly.edges.items.len >= 3) {
            try out_polys.append(world.brush_bvh.allocator, poly);
        } else {
            poly.edges.deinit(pool.allocator);
        }
    }
}

fn clipPolygonAgainstBrush(pool: *VertexPool, in_out: *Polygon, brush: *Brush) !void {
    // Slice the polygon against every plane of the convex hull
    for (brush.planes.items) |plane| {
        try slicePolygonByPlane(pool, in_out, plane);
        // If the polygon was completely outside this plane, it doesn't
        // intersect the brush at all. We can abort early.
        if (in_out.edges.items.len == 0) break;
    }
}

fn slicePolygonByPlane(pool: *VertexPool, in_out: *Polygon, plane: Plane) !void {
    var i: usize = 0;
    while (i < in_out.edges.items.len) {
        const edge = in_out.edges.items[i];
        const current_vert = pool.vertices.items[edge.a];
        const next_vert = pool.vertices.items[edge.b];

        const dist_current: f32 = linalg.dot(plane.normal, current_vert) + plane.distance;
        const dist_next: f32 = linalg.dot(plane.normal, next_vert) + plane.distance;

        const current_is_inside = dist_current <= epsilon;
        const next_is_inside = dist_next <= epsilon;

        if (!current_is_inside) {
            _ = in_out.edges.orderedRemove(i);
            if (current_is_inside != next_is_inside) {
                const t = dist_current / (dist_current - dist_next);
                const intersect_vert = linalg.lerp(current_vert, next_vert, t);
                const intersect_idx = try pool.internVertex(intersect_vert);
                try in_out.edges.insert(i, .{ .a = edge.a, .b = intersect_idx });
                i += 1;
            }
        } else {
            i += 1;
            if (current_is_inside != next_is_inside) {
                const t = dist_current / (dist_current - dist_next);
                const intersect_vert = linalg.lerp(current_vert, next_vert, t);
                const intersect_idx = try pool.internVertex(intersect_vert);
                try in_out.edges.insert(i, .{ .a = intersect_idx, .b = edge.b });
                i += 1;
            }
        }
    }
}

fn triangulateNgonSets(pool: *VertexPool, out_vertices: *std.ArrayList(Vertex), out_indices: *std.ArrayList(VertexIndex), sets: []const std.ArrayList(NGon)) !void {
    _ = .{ pool, sets, out_vertices, out_indices };
    @panic("TODO");
}
