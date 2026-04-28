//! A collection of abstractions providing spacial partitioning (quadtree, octree, etc) //

const bounding_volumes = @This();

const std = @import("std");
const util = @import("util.zig");
const linalg = @import("linalg.zig");

/// Configuration input for a `BoundingVolumeHierarchy`; produces a sensible standard precision float based Octree set by default.
pub const BVHContext = struct {
    /// The type of data, if any, stored in each volume node of the BVH-tree.
    T: type = void,
    /// The number of dimensions in the K-dimensional space.
    K: comptime_int = 3,
    /// The scalar type used for coordinate axes and distance calculations. Must be floating point.
    Scalar: type = f32,
    /// The identity type for values inserted into the tree. BVHId is the default and template for substitutes.
    Id: type = BVHId,
    /// The maximum number of objects a leaf node can hold before it subdivides if it can.
    max_objects_per_node: comptime_int = 16,
    /// The minimum node size (in world units) before the BVH will stop subdividing. Defaults to 1 unit.
    minimum_node_size: comptime_float = 1.0,

    /// Computes the number of children each branch node can have (2^K).
    fn splitCount(self: BVHContext) comptime_int {
        comptime return 1 << self.K;
    }

    /// Computes the epsilon value for the context's Scalar type
    fn floatEps(self: BVHContext) self.Scalar {
        comptime return std.math.floatEps(self.Scalar);
    }
};

/// A unique identifier for an object stored in the BVH tree.
/// This is a simple integer newtype, but can be replaced to include more information
pub const BVHId = enum(usize) {
    _, // open enumeration allows the conversions to work

    /// Creates a BVHId from an integer value.
    pub fn create(
        value: usize,
        // data for user convenience,
        // not used by the default ID implementation
        info: extern struct {
            bounds: *const anyopaque,
            element: *const anyopaque,
        },
    ) callconv(.c) BVHId {
        _ = info;
        return @enumFromInt(value);
    }

    /// Converts the BVHId back to its integer representation.
    pub fn toInteger(self: BVHId) usize {
        return @intFromEnum(self);
    }
};

/// A point in K-dimensional space.
pub fn BVHPoint(comptime Ctx: BVHContext) type {
    return @Vector(Ctx.K, Ctx.Scalar);
}

/// An index of a node within its parent's children array.
pub fn BVHNodeIndex(comptime Ctx: BVHContext) type {
    return std.math.IntFittingRange(0, Ctx.splitCount());
}

/// Axis-aligned bounding box in K-dimensional space.
pub fn BVHBounds(comptime Ctx: BVHContext) type {
    return struct {
        const Bounds = @This();
        const Point = BVHPoint(Ctx);

        /// The smallest coordinate values in each dimension.
        min: Point,
        /// The largest coordinate values in each dimension.
        max: Point,

        /// Calculates the diagonal length of the bounds, which can be used as a rough measure of its size.
        pub fn diagonalLength(self: *const Bounds) Ctx.Scalar {
            return linalg.len(self.size());
        }

        /// Calculates the union of two bounds, returning a new bounds that encompasses both.
        pub fn merge(a: *const Bounds, b: *const Bounds) Bounds {
            return Bounds{
                .min = @min(a.min, b.min),
                .max = @max(a.max, b.max),
            };
        }

        /// Calculates the size of the bounds in each dimension.
        pub fn size(self: *const Bounds) Point {
            return self.max - self.min;
        }

        /// Determines if the bounds can be subdivided further based on the minimum node size.
        pub fn canSubdivide(self: *const Bounds) bool {
            return !linalg.any(self.size() <= comptime linalg.splat(Point, Ctx.minimum_node_size + Ctx.floatEps()), Ctx.K);
        }

        /// Calculates the volume of the bounds.
        pub fn volume(self: *const Bounds) Ctx.Scalar {
            return @reduce(.Mul, self.size());
        }

        /// Calculates the center point of the bounds.
        pub fn center(self: *const Bounds) Point {
            return self.min + self.size() * linalg.splat(Point, 0.5);
        }

        /// Expands a bounds on all sides by the given amount, returning a new bounds.
        pub fn inflate(self: *const Bounds, amount: Ctx.Scalar) Bounds {
            const v_amount = linalg.splat(Point, amount);
            return Bounds{
                .min = self.min - v_amount,
                .max = self.max + v_amount,
            };
        }

        /// Checks if this bounds contains the given k-dimensional point.
        pub fn containsPoint(self: *const Bounds, point: Point) bool {
            return !linalg.any(point < self.min) and !linalg.any(point > self.max);
        }

        /// Checks if this bounds completely contains another bounds.
        pub fn contains(self: *const Bounds, other: *const Bounds) bool {
            return !linalg.any(other.min < self.min) and !linalg.any(other.max > self.max);
        }

        /// Checks if this bounds intersects with another bounds.
        pub fn intersects(self: *const Bounds, other: *const Bounds) bool {
            return !linalg.any(self.max < other.min) and !linalg.any(self.min > other.max);
        }

        /// Calculates a new bounds containing this bounds, aligned to a power-of-two multiple of the `minimum_node_size`.
        pub fn alignToScaledGrid(self: *const Bounds) Bounds {
            const needed_size = @reduce(.Max, self.size());

            var new_size: Ctx.Scalar = Ctx.minimum_node_size;
            while (new_size < needed_size) new_size *= 2;

            const cen = self.center();
            const half_size = linalg.splat(Point, new_size * 0.5);
            return Bounds{
                .min = cen - half_size,
                .max = cen + half_size,
            };
        }

        /// Computes the bounding box for a child node given its index in the parent's children array.
        ///
        /// The index is interpreted as a bitfield, where each bit represents which half of the parent's
        /// bounds the child occupies in that dimension.
        /// For example, in a 3D octree, index 0 (000) represents the child in the min-min-min octant,
        /// index 1 (001) represents the min-min-max octant, index 2 (010) represents the min-max-min octant,
        /// and so on, up to index 7 (111) which represents the max-max-max octant.
        /// This function asserts that the index is valid (i.e., less than 2^K).
        pub fn computeChildBounds(parent_bounds: Bounds, index: usize) Bounds {
            // sanity check: index must be in range
            std.debug.assert(index < Ctx.splitCount());

            var min = parent_bounds.min;
            var max = parent_bounds.max;

            inline for (comptime 0..Ctx.K) |i| {
                const bit = (index >> i) & 1;
                const mid = (min[i] + max[i]) * 0.5;

                if (bit == 0) {
                    max[i] = mid;
                } else {
                    min[i] = mid;
                }
            }

            return Bounds{
                .min = min,
                .max = max,
            };
        }
    };
}

pub fn Octree(comptime T: type) type {
    return BoundingVolumeHierarchy(BVHContext{
        .T = T,
        .K = 3,
        .Scalar = f32,
        .Id = BVHId,
        .max_objects_per_node = 16,
        .minimum_node_size = 1.0,
    });
}
pub fn Quadtree(comptime T: type) type {
    return BoundingVolumeHierarchy(BVHContext{
        .T = T,
        .K = 2,
        .Scalar = f32,
        .Id = BVHId,
        .max_objects_per_node = 16,
        .minimum_node_size = 1.0,
    });
}

/// A space-partitioning data structure that organizes elements and bounding volumes
/// within a K-dimensional space.
///
/// Note that this is different from a K-D tree or an object-partitioning structure:
/// - K-D trees are sort of like binary trees that split space with alternating planes,
///   and are often used for nearest-neighbor searches.
///
/// - Object-partitioning structures (like BSP trees) split objects into groups based on their spatial relationships.
///   This is a much more robust solution to the problem of organizing data,
///   but it is also much more complex and computationally expensive.
///
/// - This structure takes an initial volume and recursively, evenly subdivides it into smaller volumes,
///   creating a tree-like structure where each node represents a volume in space
///   and each level of the tree represents a finer subdivision of space.
///
/// #### Memory management
/// - The tree itself should be created with an allocator that can easily reuse freed memory.
///
///   If you are using an arena, this may conflict with order of deinit, as arena operations must correspond
///   to stack-like push/pop operations; which often will not be the case when manipulating the tree.
///
///   An arena can of course still be desirable in the situation where the entire tree is created and destroyed,
///   and the implementation is compatible with this, just be aware of the concerns.
///
/// - Internally, the tree uses `std.heap.MemoryPool` backed by the provided allocator to make
///   node allocations and deallocations fast.
///
/// - The actual nodes of the tree contain an optimized shortlist to store their elemental bounds and data.
///   Where we cannot optimally divide the space into shortlists, a `std.MultiArrayList` is employed to store the overflow.
///   When required, the `MultiArrayList` is backed by the base allocator.
///
///   Additionally, a `std.AutoHashmap` is used to relate IDs to their container nodes. This is also backed by the base allocator.
///
/// - Memory management by the data type element `T` is the responsibility of the user,
///   and is generally not recommended for performance reasons. Ideally, `T` should be POD, or even `void`.
///
/// - The BoundingVolumeHierarchy itself requires a stable base address.
///   If you relocate the hierarchy with live nodes in it, you must update the root node's parent reference.
pub fn BoundingVolumeHierarchy(comptime Ctx: BVHContext) type {
    std.debug.assert(Ctx.K >= 1);
    std.debug.assert(Ctx.max_objects_per_node >= 1);
    std.debug.assert(Ctx.minimum_node_size > 0.0);

    return struct {
        const Volume = @This();
        pub const Id = Ctx.Id;
        pub const Node = BVHNode(Ctx);
        pub const Bounds = BVHBounds(Ctx);
        pub const Point = BVHPoint(Ctx);
        pub const ColumnRef = BVHColumnRef(Ctx);
        pub const NodePool = std.heap.memory_pool.Managed(Node);
        pub const NodeIndex = BVHNodeIndex(Ctx);
        pub const ElementMap = std.AutoHashMap(Ctx.Id, struct { *Node, usize });

        /// general purpose/temporary allocator for non-node allocations
        allocator: std.mem.Allocator,
        /// backed by the above allocator, makes node allocations fast
        pool: NodePool,
        /// A map from element IDs to the nodes that contain them, along with their index; for fast removal and updates
        id_to_node: ElementMap,
        /// The root node of the tree, or `null` if the tree is empty
        root: ?*Node = null,
        /// Source of fresh unique IDs for inserted elements
        fresh_id: usize = 0,

        /// Creates a new empty tree backed by the given allocator.
        pub fn init(allocator: std.mem.Allocator) Volume {
            return Volume{
                .allocator = allocator,
                .pool = NodePool.init(allocator),
                .id_to_node = ElementMap.init(allocator),
            };
        }

        /// Destroys the tree and all its nodes, freeing all associated memory.
        pub fn deinit(self: *Volume) void {
            // we don't need to destroy the nodes themselves, just the pool and their data members
            if (self.root) |root| root.free(self.allocator);

            self.pool.deinit();
            self.id_to_node.deinit();

            self.* = undefined;
        }

        /// Clears the tree, removing all nodes and freeing their memory.
        ///
        /// Slightly more efficient than deinit+init, as this retains the pool and map memory.
        pub fn clear(self: *Volume) !void {
            if (self.root) |root| root.free(self.allocator);

            _ = self.pool.reset(.retain_capacity);
            self.root = null;
            self.fresh_id = 0;
            self.id_to_node.clearRetainingCapacity();
        }

        //                          //
        // === Partitioning API === //
        //                          //

        /// Ensures the tree has a root node covering the given bounds, creating one if necessary.
        pub fn ensureBounds(self: *Volume, bounds: Bounds) error{OutOfMemory}!void {
            _ = try self.initRoot(bounds);
        }

        /// Queries the tree for all elements whose bounding volumes intersect with the given bounds.
        pub fn queryBounds(self: *const Volume, allocator: std.mem.Allocator, out: *std.ArrayList(ColumnRef), bounds: Bounds) error{OutOfMemory}!void {
            if (self.root) |root| try root.query_bounds(allocator, out, bounds);
        }

        /// Inserts an element with the given bounding volume into the tree.
        /// * Returns a unique ID for the inserted element, which can be used for removal or updates.
        pub fn insert(self: *Volume, bounds: Bounds, element: Ctx.T) error{OutOfMemory}!Ctx.Id {
            const root = try self.initRoot(bounds);

            const id = Ctx.Id.create(self.fresh_id, .{ .bounds = @ptrCast(&bounds), .element = @ptrCast(&element) });
            self.fresh_id += 1;

            const ref = root.insert(
                self.allocator,
                &self.pool,
                &self.id_to_node,
                id,
                bounds,
                element,
            ) catch |err| {
                std.debug.assert(err == error.OutOfMemory);
                return error.OutOfMemory;
            };

            try self.id_to_node.put(id, ref);

            return id;
        }

        /// Removes the element with the given ID from the tree.
        /// * Returns the removed element, if any.
        pub fn remove(self: *Volume, id: Ctx.Id) error{OutOfMemory}!?Ctx.T {
            const kv = self.id_to_node.fetchRemove(id) orelse {
                @branchHint(.unlikely);
                return null;
            };

            return try kv.value[0].remove(self.allocator, &self.pool, &self.id_to_node, kv.value[1]);
        }

        /// Updates the bounding volume of the element with the given ID.
        pub fn updateBounds(self: *Volume, id: Ctx.Id, new_bounds: Bounds) error{ OutOfMemory, InvalidId }!void {
            const entry = self.id_to_node.getEntry(id) orelse {
                @branchHint(.unlikely);
                return error.InvalidId;
            };

            const node = entry.value.*[0];
            const element_index = entry.value.*[1];

            // If the object's new bounds still fit within its current node,
            // we can update its bounds in-place, which is very fast.
            if (node.bounds.contains(&new_bounds)) {
                node.element_table.bounds_slice()[element_index] = new_bounds;
                return;
            }

            // If the object has moved outside its current node, a simple in-place update is not possible.

            // We must relocate the object. Because we were initially starting from an arbitrary node,
            // it would be complex and inefficient to try to reinsert by moving upward first.
            // Let the tree do what it was designed to do.

            // Remove the object from the tree.
            const element = (try self.remove(id)).?;

            // Ensure the tree's root can contain the new bounds, expanding the tree upwards if needed.
            const root = try self.initRoot(new_bounds);

            // Re-insert the object starting from the root. This will find the correct new position
            // for the element in a top-down manner.
            const container = try root.insert(
                self.allocator,
                &self.pool,
                &self.id_to_node,
                id,
                new_bounds,
                element,
            );

            // Update the ID-to-node map to point to the object's new container and index.
            try self.id_to_node.put(id, container);
        }

        /// Gets a temporary pointer to the element with the given ID.
        pub fn getElement(self: *const Volume, id: Ctx.Id) ?*const Ctx.T {
            const node = self.id_to_node.get(id) orelse {
                @branchHint(.unlikely);
                return null;
            };

            return node.getElement(id);
        }

        /// Gets the bounds of the element with the given ID.
        pub fn getBounds(self: *const Volume, id: Ctx.Id) ?Bounds {
            const node = self.id_to_node.get(id) orelse return null;

            return node.getBounds(id);
        }

        //                                   //
        // === Extra utilities/debugging === //
        //                                   //

        pub fn drawVolumeHierarchy(self: *const Volume) void {
            if (self.root) |root| {
                root.drawNode();
            }
        }

        //                                //
        // === Implementation details === //
        //                                //

        /// Allocates and initializes the root node of the tree with the given bounds.
        fn initRoot(self: *Volume, bounds: Bounds) error{OutOfMemory}!*Node {
            if (self.root) |x| {
                var root = x;

                // We are a root node, but we do not contain the new bounds;
                // we need to create a new root that contains both the old root and the new bounds;
                // however, we cannot simply take the union and align it:
                // we need each level of tree in between, in order to ensure our current divisions remain valid.

                // Loop to grow the tree upwards until the new object fits.
                while (!root.bounds.contains(&bounds)) {
                    const old_root = root;
                    const old_root_center = old_root.bounds.center();

                    // Determine which octant the old_root will become.
                    var child_index: usize = 0;
                    inline for (comptime 0..Ctx.K) |i| {
                        // If the new object is on the "high" side of the old center...
                        if (bounds.min[i] >= old_root_center[i]) {
                            // ...then the old root must be in the "low" half of the new parent.
                            // The bit for this axis is 0. We do nothing.
                        } else {
                            // ...otherwise, the old root must be in the "high" half.
                            // The bit for this axis is 1.
                            child_index |= (1 << i);
                        }
                    }

                    // Calculate the new larger, aligned parent bounds.
                    const old_size_vec = old_root.bounds.size();
                    var new_min = old_root.bounds.min;
                    var new_max = old_root.bounds.max;

                    inline for (comptime 0..Ctx.K) |i| {
                        const bit = (child_index >> i) & 1;
                        const old_size = old_size_vec[i];
                        if (bit == 0) { // Old root is in the low half.
                            new_max[i] = new_min[i] + old_size * 2.0;
                        } else { // Old root is in the high half.
                            new_min[i] = new_max[i] - old_size * 2.0;
                        }
                    }

                    // Create the new root and re-parent the old one.
                    const new_root = try self.pool.create();
                    new_root.* = Node{
                        .parent = .{ .root = self },
                        .bounds = .{ .min = new_min, .max = new_max },
                        .element_table = .empty,
                        .children = [1]?*Node{null} ** Ctx.splitCount(),
                        .content = .branch, // The new root is always a branch.
                    };
                    new_root.children[child_index] = old_root;
                    old_root.parent = .{ .nested = .{ .node = new_root, .index = @intCast(child_index) } };

                    root = new_root;
                }

                self.root = root;
            } else { // no root yet, let's create it
                const root_node = try self.pool.create();

                root_node.* = Node{
                    .bounds = bounds.alignToScaledGrid(),
                    .parent = .{ .root = self },
                    .element_table = .empty,
                    .children = [1]?*Node{null} ** Ctx.splitCount(),
                    .content = .leaf,
                };

                self.root = root_node;
            }

            return self.root.?;
        }
    };
}

/// A bounding volume node in a K-dimensional space.
pub fn BVHNode(comptime Ctx: BVHContext) type {
    return struct {
        const Node = @This();
        const Volume = BoundingVolumeHierarchy(Ctx);
        const Point = BVHPoint(Ctx);
        const Bounds = BVHBounds(Ctx);
        const Table = BVHTable(Ctx);
        const ColumnRef = BVHColumnRef(Ctx);
        const NodeIndex = BVHNodeIndex(Ctx);

        /// The parent of this node, or `null` if this is the current root node.
        parent: union(enum) { nested: struct { node: *Node, index: NodeIndex }, root: *Volume },
        /// The content kind for this node, indicating whether `children` is populated or if this is a leaf node.
        content: enum(u1) { leaf, branch },
        /// The bounding box of this node.
        bounds: Bounds,
        /// The table of elements contained in this node, along with their bounding boxes.
        element_table: Table = .empty,
        /// A branch node can have up to 2^K child nodes.
        children: [Ctx.splitCount()]?*Node = [1]?*Node{null} ** Ctx.splitCount(),

        /// Gets an element by id from this node's table, if it exists here.
        fn getElement(self: anytype, id: Ctx.Id) ?util.CopyPtrMutability(@TypeOf(self), .one, Ctx.T) {
            const idx = self.element_table.find_by_id(id) orelse return null;
            return self.element_table.elements_slice()[idx];
        }

        /// Gets an element's bounds by id from this node's table, if it exists here.
        fn getBounds(self: *const Node, id: Ctx.Id) ?Bounds {
            const idx = self.element_table.find_by_id(id) orelse return null;
            return self.element_table.bounds_slice()[idx];
        }

        /// Recursively queries this node and its children for all elements whose bounding volumes intersect with the given bounds.
        fn query_bounds(self: *const Node, allocator: std.mem.Allocator, out: *std.ArrayList(ColumnRef), bounds: Bounds) error{OutOfMemory}!void {
            if (!self.bounds.intersects(&bounds)) {
                // ma'am, this is a wendy's
                return;
            }

            // check our own table
            var it = self.element_table.iterator();
            while (it.next()) |entry| {
                if (entry.bounds.intersects(&bounds)) {
                    try out.append(allocator, entry);
                }
            }

            // recurse into children
            if (self.content == .branch) {
                inline for (comptime 0..Ctx.splitCount()) |i| {
                    if (self.children[i]) |child| {
                        try child.query_bounds(allocator, out, bounds);
                    }
                }
            }
        }

        /// Inserts an element with the given bounding box into this node.
        /// If this node is a leaf and exceeds `max_objects_per_node`, it will subdivide.
        /// If this node is a branch, it will attempt to insert into one of its children.
        /// This function asserts that the element's bounds are contained within this node's bounds.
        /// `allocator` must be the same as the one used to create the tree.
        fn insert(
            self: *Node,
            allocator: std.mem.Allocator,
            pool: *Volume.NodePool,
            map: *Volume.ElementMap,
            id: Ctx.Id,
            bounds: Bounds,
            element: Ctx.T,
        ) error{ OutOfMemory, OutOfBounds }!struct { *Node, usize } {
            if (!self.bounds.contains(&bounds)) {
                @branchHint(.unlikely);
                return error.OutOfBounds;
            }

            // if we are a branch, try to insert into a descendant node
            switch (self.content) {
                .branch => {
                    // sanity check: we should only be a branch if we can subdivide
                    std.debug.assert(self.bounds.canSubdivide());

                    // find a single child that fully contains the bounds
                    insert_recursive: {
                        var fit: ?NodeIndex = null;
                        var fit_bounds: Bounds = undefined;

                        inline for (comptime 0..Ctx.splitCount()) |i| {
                            const child_bounds = self.bounds.computeChildBounds(i);
                            if (child_bounds.contains(&bounds)) {
                                if (fit != null) {
                                    // if the element fits in multiple children, (ie, a point at the boundary)
                                    // we cannot insert it into any of them, just break all the way out to the switch block and skip the fit check at the end
                                    break :insert_recursive;
                                } else {
                                    fit = @intCast(i);
                                    fit_bounds = child_bounds;
                                }
                            }
                        }

                        // if it fit in exactly one child, recurse into that child
                        if (fit) |child_index| {
                            // get the child node or create on demand
                            const child = if (self.children[child_index]) |c| c else create_child: {
                                const child_node = try pool.create();

                                child_node.* = Node{
                                    .parent = .{ .nested = .{ .node = self, .index = child_index } },
                                    .content = .leaf,
                                    .bounds = fit_bounds,
                                    .element_table = .empty,
                                    .children = [1]?*Node{null} ** Ctx.splitCount(),
                                };

                                self.children[child_index] = child_node;

                                break :create_child child_node;
                            };

                            return child.insert(allocator, pool, map, id, bounds, element);
                        }
                    }

                    const index = self.element_table.count();

                    // it didnt fit, so we will have to insert it here; doesn't matter if it allocates
                    _ = try self.element_table.append(allocator, id, bounds, element);

                    return .{ self, index };
                },
                .leaf => {
                    const index = self.element_table.count();
                    if (try self.element_table.try_append(allocator, id, bounds, element)) {
                        // we were able to add it with no growth, so we're done
                        return .{ self, index };
                    }

                    // this would cause our table to upgrade;
                    // we want to subdivide now if possible, so that we don't create an extra allocation

                    if (!self.bounds.canSubdivide()) {
                        // we are at max depth, so we cannot subdivide; just add it to our table
                        _ = try self.element_table.append(allocator, id, bounds, element);
                        return .{ self, index };
                    }

                    // no getting out of it now! subdivide and try again

                    try self.subdivide(allocator, pool, map);

                    return self.insert(allocator, pool, map, id, bounds, element);
                },
            }
        }

        /// Takes the node's element table, then subdivides the node into children by creating them on demand as it reinserts the elements into them.
        fn subdivide(
            self: *Node,
            allocator: std.mem.Allocator,
            pool: *Volume.NodePool,
            map: *Volume.ElementMap,
        ) error{OutOfMemory}!void {
            std.debug.assert(self.content == .leaf);

            // take the table so we can reinsert its elements
            var old_table = self.element_table;
            defer old_table.deinit(allocator);

            // clear our state to become a branch
            self.element_table = .empty;
            self.content = .branch;

            var i: usize = 0;
            while (i < old_table.count()) : (i += 1) {
                const id = old_table.ids_slice()[i];
                const elem = old_table.elements_slice()[i];
                const bounds = old_table.bounds_slice()[i];

                const success = map.remove(id);
                std.debug.assert(success);

                // this will create children as needed
                const new_ref = self.insert(allocator, pool, map, id, bounds, elem) catch |err| {
                    std.debug.assert(err == error.OutOfMemory);
                    return error.OutOfMemory;
                };

                try map.put(id, new_ref);
            }
        }

        /// Removes an element with the given ID from this node. Handles the consequences of removal, such as removing this child from its parent if it becomes empty.
        fn remove(
            self: *Node,
            allocator: std.mem.Allocator,
            pool: *Volume.NodePool,
            map: *Volume.ElementMap,
            element_index: usize,
        ) error{OutOfMemory}!Ctx.T {
            // store a copy of the element to return later
            const element = self.element_table.elements_slice()[element_index];

            const maybe_state_edge = self.element_table.remove(allocator, element_index);

            // the id will have already been removed from the map by the caller

            switch (self.content) {
                .leaf => if (maybe_state_edge) |edge| {
                    // if there was an state edge, we need to handle it.
                    switch (edge) {
                        .downgraded => {
                            // big leaf downgraded but still has elements, nothing else to do at this level

                            // if we have a parent, let it know our size has changed, so it can consider collapsing
                            if (self.parent == .nested) try self.parent.nested.node.try_collapse(allocator, pool, map);
                        },
                        .emptied => switch (self.parent) {
                            // we are now empty, we should destroy ourself and remove ourself from our parent
                            .nested => |ref| {
                                // store the parent for the next step
                                const parent_node = ref.node;

                                // remove ourself from the parent and destroy
                                ref.node.children[ref.index] = null;
                                self.destroy(allocator, pool);

                                // let the parent know its size has changed, so it can consider collapsing
                                try parent_node.try_collapse(allocator, pool, map);
                            },
                            // we are the root node, so just clear the tree
                            .root => |volume| try volume.clear(),
                        },
                    }
                } else {
                    // if we have a parent, let it know our size has changed, so it can consider collapsing
                    if (self.parent == .nested) try self.parent.nested.node.try_collapse(allocator, pool, map);
                },
                .branch => if (maybe_state_edge) |edge| {
                    switch (edge) {
                        .downgraded => {
                            // big leaf downgraded but still has elements, nothing else to do at this level

                            // we should consider collapsing; the parent does not because we are a branch
                            try self.try_collapse(allocator, pool, map);
                        },
                        .emptied => {
                            const num_children = Ctx.splitCount() - std.mem.count(?*Node, &self.children, &.{@as(?*Node, null)});

                            if (num_children == 0) {
                                // we are now empty, we should destroy ourself and remove ourself from our parent
                                switch (self.parent) {
                                    .nested => |ref| {
                                        // store the parent for the last step
                                        const parent_node = ref.node;

                                        // remove ourself from the parent and destroy
                                        ref.node.children[ref.index] = null;
                                        self.destroy(allocator, pool);

                                        // let the parent know its size has changed, so it can consider collapsing
                                        try parent_node.try_collapse(allocator, pool, map);
                                    },
                                    // we are the root node, so just clear the tree
                                    .root => |volume| try volume.clear(),
                                }
                            } else if (num_children == 1) {
                                // we have exactly one child and no elements; we can replace ourself with that child in our parent
                                const only_child: *Node = inline for (comptime 0..Ctx.splitCount()) |i| {
                                    if (self.children[i]) |child| {
                                        break child;
                                    }
                                } else unreachable;

                                switch (self.parent) {
                                    .nested => |ref| {
                                        // store the parent for the last step
                                        const parent_node = ref.node;

                                        // reparent our only child to our parent
                                        only_child.parent = .{ .nested = ref };
                                        // preserve our bounds
                                        only_child.bounds = self.bounds;
                                        // destroy ourself
                                        self.destroy(allocator, pool);
                                        // ovewrite ourself in our parent with our repurposed only child
                                        ref.node.children[ref.index] = only_child;

                                        // let the parent know its size has changed, so it can consider collapsing
                                        try parent_node.try_collapse(allocator, pool, map);
                                    },
                                    .root => |volume| {
                                        // we are the root node, so just make our only child the new root
                                        only_child.parent = .{ .root = volume };
                                        volume.root = only_child;
                                        self.destroy(allocator, pool);
                                    },
                                }
                            } else {
                                // we still have multiple children, but we are empty, so we should consider collapsing
                                try self.try_collapse(allocator, pool, map);

                                // let our parent know if we collapsed, so it can consider collapsing as well
                                if (self.content == .leaf) try self.parent.nested.node.try_collapse(allocator, pool, map);
                            }
                        },
                    }
                } else {
                    // in this case, we need to consider collapsing; the parent does not because we are a branch
                    try self.try_collapse(allocator, pool, map);
                },
            }

            return element;
        }

        /// If this is a branch node, checks if it can collapse back into a leaf node.
        /// This happens if the total number of elements in all children is less than or equal to `max_objects_per_node`.
        /// If it can collapse, it collects all elements from its children, destroys the children, and becomes a leaf node.
        fn try_collapse(
            self: *Node,
            allocator: std.mem.Allocator,
            pool: *Volume.NodePool,
            map: *Volume.ElementMap,
        ) error{OutOfMemory}!void {
            if (self.content != .branch) return;

            var total: usize = self.element_table.count();
            inline for (comptime 0..Ctx.splitCount()) |i| {
                if (self.children[i]) |child| {
                    total += child.element_table.count();
                }
            }

            if (total > Ctx.max_objects_per_node) {
                // too many elements to collapse
                return;
            }

            inline for (comptime 0..Ctx.splitCount()) |i| {
                if (self.children[i]) |child| {
                    // add the children to our collection and replace the map binding
                    var it = child.element_table.iterator();
                    while (it.next()) |entry| {
                        const index = self.element_table.count();
                        _ = try self.element_table.append(
                            allocator,
                            entry.id,
                            entry.bounds,
                            entry.element.*,
                        );

                        try map.put(entry.id, .{ self, @intCast(index) });
                    }

                    // destroy the child node
                    child.destroy(allocator, pool);
                    self.children[i] = null;
                }
            }
        }

        /// Destroys this node and all of its descendants, and frees any memory managed in the subtree, including the Node pool memory.
        /// Called by the tree when the node is being removed normally.
        fn destroy(self: *Node, allocator: std.mem.Allocator, pool: *Volume.NodePool) void {
            self.element_table.deinit(allocator);

            inline for (comptime 0..Ctx.splitCount()) |i| {
                if (self.children[i]) |child| child.destroy(allocator, pool);
            }

            pool.destroy(self);
        }

        /// Deallocates memory managed by this node; ie, the MultiArrayList.
        /// In the case of branches, it recursively calls free on the children.
        /// Does not free the node itself; this is an optimal-teardown operation for when the entire tree is being destroyed.
        fn free(self: *Node, allocator: std.mem.Allocator) void {
            self.element_table.deinit(allocator);

            inline for (comptime 0..Ctx.splitCount()) |i| {
                if (self.children[i]) |child| child.free(allocator);
            }

            // in release modes this doesn't do anything, but in debug it will catch use-after-free bugs
            self.* = undefined;
        }
    };
}

/// A reference into a BVHTable
pub fn BVHColumnRef(comptime Ctx: BVHContext) type {
    return struct {
        id: Ctx.Id,
        element: *Ctx.T,
        bounds: BVHBounds(Ctx),
    };
}

/// An iterator over the elements in a BVHTable.
pub fn BVHColumnIterator(comptime Ctx: BVHContext) type {
    return struct {
        const Iterator = @This();
        const ColumnRef = BVHColumnRef(Ctx);
        const Bounds = BVHBounds(Ctx);

        /// Current index in the iteration.
        index: usize,
        /// Total number of elements to iterate over.
        count: usize,
        /// Slice of IDs in the table.
        ids: [*]const Ctx.Id,
        /// Slice of elements in the table.
        elements: [*]Ctx.T,
        /// Slice of bounds in the table.
        bounds: [*]const Bounds,

        /// Advances the iterator and returns the next element, or `null` if the end is reached.
        pub fn next(self: *Iterator) ?ColumnRef {
            if (self.index >= self.count) {
                return null;
            }

            const item = ColumnRef{
                .id = self.ids[self.index],
                .element = &self.elements[self.index],
                .bounds = self.bounds[self.index],
            };

            self.index += 1;

            return item;
        }
    };
}

/// A table of elements and their bounding boxes stored in a BVH node.
/// This table can be a fixed-size array (standard leaf) or dynamically sized  (big leaf).
pub fn BVHTable(comptime Ctx: BVHContext) type {
    return union(enum) {
        const Table = @This();
        const Bounds = BVHBounds(Ctx);
        pub const Column = struct {
            id: Ctx.Id,
            element: Ctx.T,
            bounds: Bounds,
        };

        pub const ColumnRef = BVHColumnRef(Ctx);
        pub const Iterator = BVHColumnIterator(Ctx);

        /// Standard leaf node with a fixed-size parallel array for elements.
        standard_leaf: struct {
            len: usize = 0,
            ids: [Ctx.max_objects_per_node]Ctx.Id = undefined,
            elements: [Ctx.max_objects_per_node]Ctx.T = undefined,
            bounds: [Ctx.max_objects_per_node]Bounds = undefined,
        },

        /// If we are at > max_objects_per_node, and we are a branch or we are at max_depth;
        /// this is where we store additional elements after the `standard_leaf` is full.
        big_leaf: std.MultiArrayList(Column),

        /// Default state for a new table.
        pub const empty = Table{ .standard_leaf = .{} };

        /// Creates an iterator over the elements in the table.
        pub fn iterator(self: *const Table) Iterator {
            return switch (self.*) {
                .standard_leaf => |*leaf| Iterator{
                    .index = 0,
                    .count = leaf.len,
                    .ids = @constCast(&leaf.ids),
                    .elements = @constCast(&leaf.elements),
                    .bounds = @constCast(&leaf.bounds),
                },
                .big_leaf => |*big| Iterator{
                    .index = 0,
                    .count = big.len,
                    .ids = big.items(.id).ptr,
                    .elements = big.items(.element).ptr,
                    .bounds = big.items(.bounds).ptr,
                },
            };
        }

        /// Returns the number of elements in the table.
        pub fn count(self: *const Table) usize {
            return switch (self.*) {
                .standard_leaf => |*leaf| leaf.len,
                .big_leaf => |*big| big.len,
            };
        }

        /// Get a temporary slice of the elements in the table.
        /// Copies pointer mutability and other properties from the table type.
        fn ids_slice(self: anytype) util.CopyPtrMutability(@TypeOf(self), .slice, Ctx.Id) {
            return switch (self.*) {
                .standard_leaf => |*leaf| &leaf.ids,
                .big_leaf => |*big| big.items(.id),
            };
        }

        /// Get a temporary slice of the elements in the table.
        /// Copies pointer mutability and other properties from the table type.
        fn elements_slice(self: anytype) util.CopyPtrMutability(@TypeOf(self), .slice, Ctx.T) {
            return switch (self.*) {
                .standard_leaf => |*leaf| &leaf.elements,
                .big_leaf => |*big| big.items(.element),
            };
        }

        /// Get a temporary slice of the bounds in the table.
        /// Copies pointer mutability and other properties from the table type.
        fn bounds_slice(self: anytype) util.CopyPtrMutability(@TypeOf(self), .slice, Bounds) {
            return switch (self.*) {
                .standard_leaf => |*leaf| &leaf.bounds,
                .big_leaf => |*big| big.items(.bounds),
            };
        }

        /// Deinitializes the table, freeing any allocated memory.
        fn deinit(self: *Table, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .standard_leaf => {}, // nothing to do
                .big_leaf => |*big| big.deinit(allocator),
            }
        }

        /// Appends an element to the table, upgrading to a big leaf if necessary.
        /// * Returns whether the table was upgraded during the append operation.
        fn append(self: *Table, allocator: std.mem.Allocator, id: Ctx.Id, bounds: Bounds, element: Ctx.T) error{OutOfMemory}!bool {
            if (!try self.try_append(allocator, id, bounds, element)) {
                try self.upgrade(allocator);

                const success = try self.try_append(allocator, id, bounds, element);
                std.debug.assert(success); // sanity check: upgrade must have made space

                return true;
            }

            return false;
        }

        /// Finds an element by its ID, returning the index.
        fn find_by_id(self: *const Table, id: Ctx.Id) ?usize {
            const ids = self.ids_slice();
            for (ids, 0..) |it, idx| {
                if (it == id) return idx;
            }
        }

        const RemovalState = enum(u1) { downgraded, emptied };

        /// Removes the element at the given index from the table.
        /// * Returns an optional state enum indicating if the table was downgraded or emptied during the remove operation.
        fn remove(self: *Table, allocator: std.mem.Allocator, index: usize) ?RemovalState {
            switch (self.*) {
                .standard_leaf => |*leaf| {
                    std.debug.assert(index < leaf.len);
                    leaf.len -= 1;
                    // swap remove
                    if (index != leaf.len) {
                        leaf.ids[index] = leaf.ids[leaf.len];
                        leaf.elements[index] = leaf.elements[leaf.len];
                        leaf.bounds[index] = leaf.bounds[leaf.len];
                    }
                },
                .big_leaf => |*big| {
                    std.debug.assert(index < big.len);

                    big.swapRemove(index);

                    if (big.len <= Ctx.max_objects_per_node) {
                        self.downgrade(allocator);
                        return if (big.len == 0) .emptied else .downgraded;
                    }
                },
            }

            return if (self.count() == 0) .emptied else null;
        }

        /// Attempts to append an element to the table without upgrading.
        /// * Returns `true` if the append succeeded, `false` if the standard_leaf table is full.
        fn try_append(self: *Table, allocator: std.mem.Allocator, id: Ctx.Id, bounds: Bounds, element: Ctx.T) error{OutOfMemory}!bool {
            switch (self.*) {
                .standard_leaf => |*leaf| {
                    if (leaf.len < Ctx.max_objects_per_node) {
                        leaf.ids[leaf.len] = id;
                        leaf.elements[leaf.len] = element;
                        leaf.bounds[leaf.len] = bounds;
                        leaf.len += 1;
                        return true;
                    } else {
                        return false;
                    }
                },
                .big_leaf => |*big| {
                    try big.append(allocator, .{
                        .id = id,
                        .element = element,
                        .bounds = bounds,
                    });

                    return true;
                },
            }
        }

        /// Upgrades a standard leaf to a big leaf, migrating existing elements.
        /// * Undefined behavior to call this on a big leaf.
        /// * Incurs an allocation, which may fail.
        fn upgrade(self: *Table, allocator: std.mem.Allocator) error{OutOfMemory}!void {
            switch (self.*) {
                .standard_leaf => |*leaf| {
                    var big = std.MultiArrayList(Column).empty;
                    errdefer big.deinit(allocator);

                    var i: usize = 0;
                    while (i < leaf.len) : (i += 1) {
                        try big.append(allocator, .{
                            .id = leaf.ids[i],
                            .element = leaf.elements[i],
                            .bounds = leaf.bounds[i],
                        });
                        i += 1;
                    }

                    self.* = Table{ .big_leaf = big };
                },
                .big_leaf => unreachable, // already a big leaf, nothing to do, shouldn't be called
            }
        }

        /// Downgrades a big leaf to a standard leaf, migrating existing elements.
        /// * Undefined behavior to call this on a standard leaf.
        /// * This will cause an out of bounds access error if the big leaf has more than `max_objects_per_node` elements.
        fn downgrade(self: *Table, allocator: std.mem.Allocator) void {
            switch (self.*) {
                .standard_leaf => unreachable, // already a standard leaf, nothing to do, shouldn't be called
                .big_leaf => |*big| {
                    var leaf = @FieldType(Table, "standard_leaf"){
                        .len = 0,
                        .ids = undefined,
                        .elements = undefined,
                        .bounds = undefined,
                    };

                    var i: usize = 0;
                    while (i < big.len) : (i += 1) {
                        leaf.ids[i] = big.items(.id)[i];
                        leaf.elements[i] = big.items(.element)[i];
                        leaf.bounds[i] = big.items(.bounds)[i];
                        leaf.len += 1;
                        i += 1;
                    }

                    big.deinit(allocator);
                    self.* = Table{ .standard_leaf = leaf };
                },
            }
        }
    };
}
