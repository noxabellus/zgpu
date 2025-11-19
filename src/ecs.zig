//! # Game Engine Architecture Plan: A Deterministic, Parallel ECS
//!
//! Relevant GDD: https://atet.site/technical-depot
//!
//! ## 1. High-Level Philosophy
//!
//! The core architecture is built on a **data-oriented** paradigm using an Entity
//! Component System (ECS).
//!
//! The guiding principles are:
//!
//! *   **Separation of State and Logic:** The `ecs.World` is the single source of
//!     truth for all game state. `Systems` contain all game logic and are
//!     stateless.
//! *   **Determinism:** The simulation will produce the exact same output given the
//!     same input and starting conditions. This is achieved through explicit
//!     ordering and a command-based mutation model.
//! *   **Parallelism by Default:** The design is structured to leverage multi-core
//!     processors by default, partitioning work into independent jobs.
//! *   **Clarity and Intent:** The flow of data and the reasons for state changes
//!     are designed to be explicit, favoring a "choreographed" model of system
//!     interaction over a tightly coupled "orchestrated" one.
//!
//! ## 2. Core Architectural Components
//!
//! * **`ecs.World`**: The container for all game state. Manages entities,
//!   components, and their memory. Acts as the central "database."
//! * **`Scheduler`**: The "brain" of the engine loop. Responsible for identifying
//!   work, dispatching jobs to worker threads, and ensuring the deterministic
//!   application of results.
//! * **`Systems`**: Stateless units of logic (e.g., `PhysicsSystem`,
//!   `PerceptionSystem`) that operate on components. During parallel execution,
//!   they have read-only access to the main world state.
//! * **`CommandBuffer`**: A **long-term, cross-tick** communication channel.
//!   Systems write their desired *world mutations* here. These are applied at the
//!   end of the tick. This is our primary tool for thread-safe determinism.
//! * **`Event Stream`**: A **short-term, intra-tick** communication channel. This
//!   is a job-local "log" that allows systems *within the same job* to communicate
//!   in a deterministic sequence, enabling complex cause-and-effect chains within a
//!   single tick.
//!
//! ## 3. The Tick Lifecycle (Gather-Process-Scatter)
//!
//! A single simulation tick proceeds in three distinct phases, orchestrated by the `Scheduler`.
//!
//! #### **Phase 1: Pre-Tick Sync (Main Thread)**
//!
//! 1.  **Interpolation Sync:** For every component type marked as `interpolated`,
//!     the `Scheduler` runs a built-in process to copy the data from the
//!     `current_state` buffer to the `previous_state` buffer. This provides a
//!     stable snapshot for rendering interpolation.
//!
//! #### **Phase 2: Gather & Process (Parallel Execution)**
//!
//! 1.  **Gather (Main Thread):**
//!     * The `Scheduler` identifies all top-level hierarchy roots (entities without
//!         a `Parent` component). These are the "Points of Interest" (POIs) that
//!         form our work units.
//!     *   **For determinism, this list of roots is sorted** by `EntityId`.
//!     *   For each root, a "job" is created.
//!
//! 2.  **Process (Worker Threads):**
//!     *   Jobs are dispatched to a `std.Thread.Pool`.
//!     *   Each job is provided with two new, empty communication channels:
//!         *   A `CommandBuffer` for world mutations.
//!         *   An `Event Stream` for same-tick events.
//!     * Within each job, a predefined sequence of `Systems` is executed in a
//!         fixed, deterministic order.
//!     *   A `System`:
//!         * Reads data from the read-only `*const World`.
//!         * Performs its logic.
//!         * Publishes events (e.g., `ThreatSpotted`) to the job's local
//!             **`Event Stream`**.
//!         * Consumes events from the **`Event Stream`** published by earlier
//!             systems.
//!         * Writes desired world mutations (e.g., `AddComponent`, `DestroyEntity`)
//!             to the job's **`CommandBuffer`**.
//!
//! #### **Phase 3: Scatter (Main Thread)**
//!
//! 1.  **Synchronization:** The `Scheduler` waits for all jobs in the thread pool
//!     to complete.
//! 2.  **Apply Mutations:** The `Scheduler` collects the `CommandBuffer` from each
//!     job. It iterates through the buffers **in the same sorted order from the
//!     Gather step** and applies every command in sequence to the `ecs.World`.
//!
//! This completes the tick. The world state is now updated and ready for the next
//! tick's Pre-Tick Sync.
//!
//! ## 4. Key Architectural Solutions
//!
//! This architecture directly solves the challenges laid out in the GDD and the
//! technical discussions.
//!
//! #### **Spatial Hierarchy**
//!
//! *   **Implementation:** Achieved via `Parent` (points to parent entity) and
//!     `Children` (list of child entities) components.
//! *   **Note:** The `Children` component will require an `on_remove` hook during
//!     component registration to de-initialize its internal `ArrayList`, preventing
//!     memory leaks. This hook mechanism has been implement as a general feature for any
//!     component managing its own resources; we grab the `deinit` method through comptime reflection.
//!
//! #### **State Mutation & Determinism (The `CommandBuffer`)**
//!
//! *   **Problem:** How to safely mutate the world from multiple threads without
//!     race conditions while ensuring deterministic outcomes.
//! *   **Solution:** Systems do not mutate the world directly. They queue their
//!     *intent* in a `CommandBuffer`. These commands are applied serially and in a
//!     fixed order by the main thread, guaranteeing that the final state is always
//!     the same.
//!
//! #### **Intra-Tick Communication (The `Event Stream`)**
//!
//! *   **Problem:** Fulfilling the GDD's requirement for immediate, same-tick
//!     cause-and-effect chains (e.g., the agent cognitive loop) without violating
//!     thread safety.
//! *   **Solution:** The job-local `Event Stream`. This is a "choreographed" model
//!     where systems publish events without knowing the consumers. Subsequent
//!     systems in the deterministic execution order can then read this stream and
//!     react. This provides total decoupling and predictable, ordered reactions,
//!     perfectly suiting the GDD's agent logic.
//!
//! #### **Fixed-Rate Simulation & Rendering**
//!
//! *   **Problem:** Decoupling the simulation rate (e.g., 30 TPS) from the display
//!     rate (e.g., 60/144 FPS) for smooth visuals.
//! *   **Solution:** A main loop based on a time accumulator will run the simulation in fixed steps.
//! *   **Interpolation:** The `Scheduler` will automatically manage `current_state`
//!     and `previous_state` buffers for designated components. The renderer will be
//!     given an `alpha` value (0.0-1.0) to interpolate between these two states,
//!     ensuring smooth motion regardless of framerate.
//!
//! ## 5. Future Evolution & Considerations
//!
//! The current architecture is a robust foundation (Phase 1) with clear paths for
//! future enhancement.
//!
//! *   **Phase 2: Advanced Scheduling:** The initial hierarchy-based job
//!     partitioning can be evolved. By having systems declare their component
//!     access (`read`/`write`), the `Scheduler` can build a dependency graph and
//!     unlock a higher degree of parallelism, allowing non-conflicting systems to
//!     run concurrently on the same data set.
//! *   **Phase 3: Triple Buffering:** If profiling reveals that the simulation and
//!     render threads are stalling each other, the double-buffer interpolation
//!     model can be upgraded to a full triple-buffer system. This would involve a
//!     `simulation`, `render`, and `history` world state, rotated via pointers to
//!     achieve maximum CPU/GPU parallelism. This comes at the cost of increased
//!     memory usage and one extra frame of input latency, and should only be
//!     implemented if proven necessary.
//! *   **Director AI & Analysis:** The `Event Stream` model is highly beneficial
//!     for a "Director" AI. The Director can be a special system that consumes the
//!     event streams from all POIs after a tick, providing it with a rich,
//!     structured log of everything significant that happened, which it can use to
//!     make strategic decisions.
//!
//! ## 6. Notes
//!
//! * The current type names system is suboptimal as we're hashing the full name; but, for the underlying system its sort of necessary.
//!   If we were to move away from supporting the direct-access API (registerComponetType), we could use just the pointer to the type name, because theyre always unique.
//!   A better system would supply a TypeInfo and have you register that first. Then we'd never need to deal with names at all.
const ecs = @This();
const std = @import("std");
const SlotMap = @import("SlotMap.zig");

const log = std.log.scoped(.ecs);

// --- Core Identifiers ---

/// EntityId represents a unique, stable handle to an entity in the world.
/// It is a "stable" handle because it remains valid even if other entities are
/// created or destroyed. It wraps a SlotMap.Ref, which provides this guarantee.
pub const EntityId = packed struct {
    ref: SlotMap.Ref,

    pub fn wrap(ref: SlotMap.Ref) EntityId {
        return EntityId{ .ref = ref };
    }
};

/// Used to reference component types and storage buffers.
pub const ComponentId = packed struct {
    ref: SlotMap.Ref,

    pub fn wrap(ref: SlotMap.Ref) ComponentId {
        return ComponentId{ .ref = ref };
    }
};

// --- Query filters ---

/// A type-level filter that specifies if a component is excluded in a query.
pub const Exclude = struct { type };

/// ComponentFilter specifies whether a component is required or excluded in a query.
pub const ComponentFilter = union(enum) {
    /// indicates that a component is required to be present on entities for a given query
    required: ComponentId,
    /// indicates that a component is not allowed to be present on entities in a given query
    excluded: ComponentId,

    /// Shortcut for creating a new filter.
    pub fn require(id: ComponentId) ComponentFilter {
        return ComponentFilter{ .required = id };
    }

    /// Shortcut for creating a new filter.
    pub fn exclude(id: ComponentId) ComponentFilter {
        return ComponentFilter{ .excluded = id };
    }

    /// Create a ComponentFilter from a type or type filter.
    pub fn fromType(world: *const World, comptime x: anytype) error{InvalidComponentId}!ComponentFilter {
        if (@TypeOf(x) == Exclude) {
            const id = world.getExistingComponentId(x[0]) orelse return error.InvalidComponentId;
            return .exclude(id);
        } else {
            const id = world.getExistingComponentId(x) orelse return error.InvalidComponentId;
            return .require(id);
        }
    }
};

// --- Configuration Constants ---

/// The default minimum capacity to allocate for component buffers.
pub const min_component_alloc: u32 = 1024;

/// The theoretical maximum number of entities supported.
/// Used to determine the bit-width of indices.
pub const max_entities: u32 = 1 << 31;

// --- Internal Index Types ---

/// PseudoIndex is an internal, recyclable index assigned to an entity upon creation.
/// This is the index used to access the "sparse" arrays within each component buffer.
/// It is the value stored within the EntityMap (a SlotMap).
const PseudoIndex = u32;

/// TrueIndex is the actual index into the dense `storage` array.
/// It's 31 bits to leave one bit for the `enabled` flag in the packed struct.
const TrueIndex = u31;

/// IndexEntry is the element type of our "sparse" array. It stores whether
/// an entity has this component and, if so, where to find it in the "dense" array.
const IndexEntry = packed struct(u32) {
    enabled: bool,
    index: TrueIndex, // The index into the `storage` and `reverse_indices` arrays.

    pub const disabled = IndexEntry{
        .enabled = false,
        .index = 0, // Index is irrelevant when disabled.
    };

    pub fn enable(index: anytype) IndexEntry {
        return IndexEntry{ .enabled = true, .index = @intCast(index) };
    }
};

// --- Core Data Structures ---

/// EntityMap is the central manager for entity identity and lifecycle.
/// It maps the stable, external `EntityId` (via its `SlotMap.Ref`) to a transient,
/// internal `PseudoIndex`. This provides a layer of indirection that allows
//  `PseudoIndex` values to be recycled safely.
pub const EntityMap = SlotMap.new(PseudoIndex);

/// ComponentMap maps `ComponentId` (via its `SlotMap.Ref`) to a type-erased
/// `BufferStorage`. This allows the `World` to manage multiple component types
/// without knowing their concrete types at compile time.
pub const ComponentMap = SlotMap.new(BufferStorage);

/// World is the top-level container for the entire ECS.
/// It holds the allocator, the entity manager, and a type-erased list of
/// all component storage buffers.
pub const World = struct {
    /// The allocator used for all dynamic memory operations within the ECS.
    allocator: std.mem.Allocator,

    /// A slotmap that provides stable entity IDs and maps them to internal pseudo-indices.
    entities: EntityMap = .empty,

    /// A map from component type names to their assigned ComponentId.
    /// This allows for dynamic registration and lookup of component types by name.
    component_name_to_id: std.StringHashMapUnmanaged(ComponentId) = .empty,

    /// A map of all component buffers. Each buffer stores all instances
    /// of a single component type. The BufferStorage provides a uniform,
    /// type-erased interface to interact with these different buffers.
    component_buffers: ComponentMap = .empty,

    /// Creates a new, empty world backed by the provided allocator.
    pub fn init(allocator: std.mem.Allocator) World {
        return World{
            .allocator = allocator,
        };
    }

    /// Deinitializes the world, freeing all component buffers and entities.
    pub fn deinit(self: *World) void {
        var it = self.component_name_to_id.keyIterator();
        while (it.next()) |key_ptr| {
            self.allocator.free(key_ptr.*);
        }

        for (self.component_buffers.values()) |*buffer| {
            buffer.deinit(self.allocator);
        }

        self.component_name_to_id.deinit(self.allocator);
        self.component_buffers.deinit(self.allocator);
        self.entities.deinit(self.allocator);

        self.* = undefined;
    }

    /// Convert a pseduo-index back to a stable EntityId.
    /// * Note that this is unsafe, because it gets the generation of the entity slot and constructs an EntityId without validation.
    pub fn pseudoToId(self: *const World, pseudo: PseudoIndex) EntityId {
        const gen = self.entities.generations.items[pseudo];

        return .wrap(.{
            .index = @intCast(pseudo),
            .generation = gen,
        });
    }

    /// Get a ComponentId from a component name.
    /// Returns an error if the component type is not registered.
    pub fn getComponentIdByName(self: *const World, component_name: []const u8) error{InvalidComponentId}!ComponentId {
        return self.component_name_to_id.get(component_name) orelse error.InvalidComponentId;
    }

    /// Register a component type with the world, allocating its storage buffer.
    /// Returns a `ComponentId` that can be used to reference this component type.
    /// If the component type is already registered, returns the existing `ComponentId`.
    /// This is a convenience wrapper around `registerComponentType` that uses the type name as the component name.
    /// Unlike `register..`, this will not destroy existing components.
    pub fn bindComponentId(self: *World, comptime Component: type) error{OutOfMemory}!ComponentId {
        if (self.getExistingComponentId(Component)) |existing_id| {
            return existing_id;
        }

        return self.registerComponentType(@typeName(Component), Component);
    }

    /// Get an existing `ComponentId` for a component type, if it is registered.
    pub fn getExistingComponentId(self: *const World, comptime Component: type) ?ComponentId {
        return self.component_name_to_id.get(@typeName(Component));
    }

    /// Register a component type with the world, allocating its storage buffer.
    /// This is a lower level function than `bindComponentId`. While `bindComponentId` is preferred for type-based interfaces, this allows custom names for components.
    /// Returns a `ComponentId` that can be used to reference this component type.
    /// If the component type is already registered, it is replaced and all existing components of that type are destroyed.
    pub fn registerComponentType(self: *World, component_name: []const u8, comptime Component: type) error{OutOfMemory}!ComponentId {
        const gop = try self.component_name_to_id.getOrPut(self.allocator, component_name);

        if (gop.found_existing) {
            log.warn("replacing existing component type {s}; destroying outdated components", .{component_name});
            const reused_id = gop.value_ptr.*;
            const reused_buffer = self.getBufferById(reused_id) catch unreachable;
            reused_buffer.deinit(self.allocator);

            reused_buffer.* = try BufferStorage.init(Component, self.allocator);

            return reused_id;
        } else {
            const ref, const buffer_storage = try self.component_buffers.create(self.allocator);
            errdefer _ = self.component_buffers.remove(ref);

            const owned_name = try self.allocator.dupe(u8, component_name);
            errdefer self.allocator.free(owned_name);

            const id = ComponentId.wrap(ref);

            gop.key_ptr.* = owned_name;
            gop.value_ptr.* = id;

            buffer_storage.* = try BufferStorage.init(Component, self.allocator);
            errdefer buffer_storage.deinit(self.allocator);

            return id;
        }
    }

    /// Get the number of active entities in the world. O(1)
    pub fn count(self: *const World) usize {
        return self.entities.len;
    }

    /// Get the number of active component types in the world. O(1)
    pub fn countComponentTypes(self: *const World) usize {
        return self.component_buffers.len;
    }

    /// Get the number of components of a given type in the world. O(1)
    pub fn countComponentsById(self: *const World, component_id: ComponentId) error{InvalidComponentId}!usize {
        const buffer = try self.getBufferById(component_id);
        return buffer.count();
    }

    /// Get the number of components of a given type in the world. O(1)
    pub fn countComponentsByType(self: *const World, comptime Component: type) error{InvalidComponentId}!usize {
        const id = self.getExistingComponentId(Component) orelse return error.InvalidComponentId;
        return self.countComponentsById(id);
    }

    /// Get the total number of components across all types in the world.
    /// This is O(N) in the number of component types.
    pub fn countAllComponents(self: *const World) usize {
        var total: usize = 0;
        for (self.component_buffers.data()) |buffer| {
            total += buffer.count();
        }
        return total;
    }

    /// Creates a new entity in the world and returns its stable `EntityId`.
    /// Expects a tuple struct or array of components to initialize the entity with.
    /// This will not register component types; use `bindComponentId` or `registerComponentType` first.
    pub fn addEntity(self: *World, prototype: anytype) error{ OutOfMemory, InvalidComponentId }!EntityId {
        const entity_id = try self.addEmptyEntity();
        errdefer self.delEntity(entity_id);

        inline for (0..qtu.lenOfT(@TypeOf(prototype))) |i| {
            const component = prototype[i];
            const id = self.getExistingComponentId(@TypeOf(component)) orelse return error.InvalidComponentId;
            const buffer = try self.getBufferById(id);

            try buffer.setComponent(self.allocator, @intCast(entity_id.ref.index), if (comptime @sizeOf(@TypeOf(component)) > 0) &component else null);
        }

        return entity_id;
    }

    /// Creates a new entity in the world and returns its stable `EntityId`.
    pub fn addEmptyEntity(self: *World) error{OutOfMemory}!EntityId {
        const ref, const pseudo_index_ptr = try self.entities.create(self.allocator);

        pseudo_index_ptr.* = @intCast(ref.index);

        return .wrap(ref);
    }

    /// Destroys an entity and removes all its components.
    /// If the entity does not exist, this is a no-op.
    pub fn delEntity(self: *World, entity: EntityId) void {
        const pseudo = self.entities.remove(entity.ref) orelse return;

        // Remove this entity's components from all component buffers.
        for (self.component_buffers.values()) |*buffer| {
            buffer.delComponent(pseudo);
        }
    }

    /// Gets a component for a given entity.
    pub fn getComponent(self: *const World, entity: EntityId, comptime Component: type) error{ EntityNotFound, InvalidComponentId, DisabledComponent }!*Component {
        const pseudo = try self.getPseudoIndexById(entity);
        const id = self.getExistingComponentId(Component) orelse return error.InvalidComponentId;
        const buffer = try self.getBufferById(id);
        const maybe_opaque_comp = try buffer.getComponent(pseudo);

        return @ptrCast(@alignCast(maybe_opaque_comp));
    }

    /// Sets a component for a given entity.
    /// This will not register component types; use `bindComponentId` or `registerComponentType` first.
    pub fn setComponent(self: *World, entity: EntityId, component: anytype) error{ EntityNotFound, InvalidComponentId, OutOfMemory }!void {
        const pseudo = try self.getPseudoIndexById(entity);
        const id = self.getExistingComponentId(@TypeOf(component)) orelse return error.InvalidComponentId;
        const buffer = try self.getBufferById(id);

        try buffer.setComponent(self.allocator, pseudo, if (comptime @sizeOf(@TypeOf(component)) > 0) &component else null);
    }

    /// Removes a component from a given entity, returning it.
    pub fn takeComponent(self: *World, entity: EntityId, comptime Component: type) error{ EntityNotFound, InvalidComponentId }!?Component {
        const pseudo = try self.getPseudoIndexById(entity);
        const id = self.getExistingComponentId(Component) orelse return error.InvalidComponentId;
        const buffer = try self.getBufferById(id);

        var out: Component = undefined;
        if (buffer.vtable.takeComponent(&buffer.buffer, pseudo, &out)) {
            return out;
        } else {
            return null;
        }
    }

    /// Removes a component from a given entity, deinitializing it if necessary.
    pub fn delComponent(self: *World, entity: EntityId, comptime Component: type) error{ EntityNotFound, InvalidComponentId }!void {
        const pseudo = try self.getPseudoIndexById(entity);
        const id = self.getExistingComponentId(Component) orelse return error.InvalidComponentId;
        const buffer = try self.getBufferById(id);

        buffer.delComponent(pseudo);
    }

    /// Creates a query iterator for the given type-based component filters.
    /// Expects a tuple or array of types/type filters (`ecs.Exclude` etc).
    pub fn query(self: *const World, comptime Q: anytype) error{ InvalidComponentId, InvalidQuery }!TypedQuery(Q) {
        const M = qtu.lenOf(Q);

        var filters: [M]ComponentFilter = undefined;
        inline for (0..M) |i| {
            filters[i] = try ComponentFilter.fromType(self, Q[i]);
        }

        return .{ .inner = try self.queryById(filters) };
    }

    /// Gets a component for a given entity.
    /// Returns null if the entity does not have this component.
    pub fn getComponentById(self: *const World, entity: EntityId, component_id: ComponentId) error{ EntityNotFound, InvalidComponentId, DisabledComponent }!*anyopaque {
        const pseudo = try self.getPseudoIndexById(entity);
        const buffer = try self.getBufferById(component_id);

        return buffer.getComponent(pseudo);
    }

    /// Sets a component for a given entity.
    /// If the entity already has this component, it is replaced.
    pub fn setComponentById(self: *World, entity: EntityId, component_id: ComponentId, component_data: ?*const anyopaque) error{ EntityNotFound, InvalidComponentId, OutOfMemory }!void {
        const pseudo = try self.getPseudoIndexById(entity);
        const buffer = try self.getBufferById(component_id);

        try buffer.setComponent(self.allocator, pseudo, component_data);
    }

    /// Takes a component from a given entity, writing it to `out`.
    /// Returns true if the component was present and written to `out`, false if the entity did not have this component.
    pub fn takeComponentById(self: *World, entity: EntityId, component_id: ComponentId, out: *anyopaque) error{ EntityNotFound, InvalidComponentId }!bool {
        const pseudo = try self.getPseudoIndexById(entity);
        const buffer = try self.getBufferById(component_id);

        return buffer.vtable.takeComponent(&buffer.buffer, pseudo, out);
    }

    /// Removes a component from a given entity.
    /// If the entity does not have this component, this is a no-op.
    pub fn delComponentById(self: *World, entity: EntityId, component_id: ComponentId) error{ EntityNotFound, InvalidComponentId }!void {
        const pseudo = try self.getPseudoIndexById(entity);
        const buffer = try self.getBufferById(component_id);

        buffer.delComponent(pseudo);
    }

    /// Get a component buffer handle for a given component ID.
    pub fn getBufferById(self: *const World, component_id: ComponentId) error{InvalidComponentId}!*BufferStorage {
        return self.component_buffers.get(component_id.ref) orelse return error.InvalidComponentId;
    }

    /// Gets the `PseudoIndex` for a given `EntityId`.
    /// Returns an error if the entity does not exist.
    pub fn getPseudoIndexById(self: *const World, entity: EntityId) error{EntityNotFound}!PseudoIndex {
        const pseudo_index = self.entities.get(entity.ref) orelse {
            return error.EntityNotFound;
        };

        return pseudo_index.*;
    }

    /// Creates a query iterator for the given component filters.
    /// Input should be a tuple struct or array of `ComponentFilter`.
    pub fn queryById(self: *const World, q: anytype) error{ InvalidComponentId, InvalidQuery }!Query(qtu.queryM(@TypeOf(q), ComponentFilter)) {
        const M = qtu.queryM(@TypeOf(q), null);
        const Q = comptime Query(M);

        var it = Q{ .world = self };

        // driver buffer should be the shortest one for minimal indirect lookups
        var shortest_buf_index: usize = 0;
        var shortest_buf_count: usize = std.math.maxInt(usize);

        for (0..M) |i| {
            it.component_filters[i] = q[i];

            switch (q[i]) {
                .excluded => |id| {
                    it.buffers[i] = try self.getBufferById(id);
                },
                .required => |id| {
                    it.buffers[i] = try self.getBufferById(id);
                    if (it.buffers[i].count() < shortest_buf_count) {
                        shortest_buf_count = it.buffers[i].count();
                        shortest_buf_index = i;
                    }
                },
            }
        }

        it.mode = if (shortest_buf_count == std.math.maxInt(usize)) .all else .{ .driver = @intCast(shortest_buf_index) };

        return it;
    }
};

/// BufferStorage allows for type-erased manipulation
/// of a specific `ComponentBuffer(T)`. It consists of a raw pointer to the
/// buffer's memory (`ptr`) and a pointer to a vtable that contains functions
/// specific to that component type. This allows the World to treat all
/// component buffers identically without knowing their concrete types.
pub const BufferStorage = struct {
    buffer: [@sizeOf(ComponentBuffer(void))]u8 align(@alignOf(ComponentBuffer(void))),
    vtable: *const BufferVTable,

    /// Create a type-erased BufferStorage given a type and an allocator.
    pub fn init(comptime Component: type, allocator: std.mem.Allocator) error{OutOfMemory}!BufferStorage {
        const buffer = try ComponentBuffer(Component).init(allocator);
        errdefer buffer.deinit(allocator);

        var out = BufferStorage{
            .buffer = undefined,
            .vtable = &ComponentBuffer(Component).vtable,
        };

        @memcpy(&out.buffer, std.mem.asBytes(&buffer));

        return out;
    }

    /// Deinitializes the component buffer, freeing its internal arrays.
    pub inline fn deinit(self: *BufferStorage, allocator: std.mem.Allocator) void {
        self.vtable.deinit(&self.buffer, allocator);
        self.* = undefined;
    }

    /// Gets the number of components in this buffer.
    pub inline fn count(self: *const BufferStorage) usize {
        return self.vtable.count(&self.buffer);
    }

    /// Gets a pointer to a component for a given `PseudoIndex`.
    /// Returns null if the entity does not have this component.
    pub inline fn getComponent(self: *const BufferStorage, index: PseudoIndex) error{DisabledComponent}!*anyopaque {
        return self.vtable.getComponent(&self.buffer, index);
    }

    /// Adds a component for a new entity.
    pub inline fn setComponent(self: *BufferStorage, allocator: std.mem.Allocator, index: PseudoIndex, component_data: ?*const anyopaque) error{OutOfMemory}!void {
        return self.vtable.setComponent(&self.buffer, allocator, index, component_data);
    }

    /// Removes a component for a given `PseudoIndex`.
    pub inline fn delComponent(self: *BufferStorage, index: PseudoIndex) void {
        self.vtable.delComponent(&self.buffer, index);
    }

    /// Gets a component and its entity `PseudoIndex` for a given dense index.
    pub inline fn getIndex(self: *const BufferStorage, dense_index: TrueIndex) struct { *anyopaque, PseudoIndex } {
        return self.vtable.getIndex(&self.buffer, dense_index);
    }
};

/// BufferVTable (Virtual Method Table) defines the set of operations that
/// can be performed on a component buffer. Each `ComponentBuffer(T)` generates
/// its own static instance of this struct with pointers to its concrete,
/// type-specific functions. This is the mechanism that enables type erasure.
pub const BufferVTable = struct {
    getComponent: *const fn (self: *const anyopaque, index: PseudoIndex) error{DisabledComponent}!*anyopaque,
    setComponent: *const fn (self: *anyopaque, allocator: std.mem.Allocator, index: PseudoIndex, data: ?*const anyopaque) error{OutOfMemory}!void,
    delComponent: *const fn (self: *anyopaque, index: PseudoIndex) void,
    takeComponent: *const fn (self: *anyopaque, index: PseudoIndex, out: *anyopaque) bool,
    count: *const fn (self: *const anyopaque) usize,
    deinit: *const fn (self: *anyopaque, allocator: std.mem.Allocator) void,
    getIndex: *const fn (self: *const anyopaque, dense_index: TrueIndex) struct { *anyopaque, PseudoIndex },
};

/// This function is a "generic" that generates a concrete `ComponentBuffer` struct
/// for a given component type `T`. This struct implements the sparse set data structure.
pub fn ComponentBuffer(comptime Component: type) type {
    return struct {
        const Buffer = @This();

        // --- Core Sparse Set Arrays ---

        /// `indices`: This is the SPARSE array. It is indexed by `PseudoIndex`.
        /// It provides a fast, O(1) lookup to check if an entity has this component
        /// and to find its `TrueIndex` in the dense `storage` array if it does.
        /// This array grows but never shrinks to maintain index stability.
        indices: std.ArrayListUnmanaged(IndexEntry),

        /// `reverse_indices`: This is the REVERSE MAP. It is a dense array, parallel
        /// to `storage`. It is indexed by `TrueIndex` and stores the `PseudoIndex`
        /// of the entity that owns the component at that `TrueIndex`.
        /// Its purpose is to make deletion an O(1) operation during a swap-remove.
        reverse_indices: std.ArrayListUnmanaged(PseudoIndex),

        /// `storage`: This is the DENSE array. It stores the actual component data
        /// contiguously in memory. This layout is critical for cache performance
        /// during iteration (e.g., in the Gather step of a job).
        storage: std.ArrayListUnmanaged(Component),

        /// A static vtable instance for this specific `ComponentBuffer(T)` type.
        /// The function pointers are cast to be generic for the `BufferVTable` struct.
        pub const vtable = BufferVTable{
            .setComponent = @ptrCast(&Buffer.setComponent),
            .getComponent = @ptrCast(&Buffer.getComponent),
            .delComponent = @ptrCast(&Buffer.delComponent),
            .takeComponent = @ptrCast(&Buffer.takeComponent),
            .count = @ptrCast(&Buffer.count),
            .deinit = @ptrCast(&Buffer.deinit),
            .getIndex = @ptrCast(&Buffer.getIndex),
        };

        /// Initializes a new component buffer at minimum capacity.
        pub fn init(allocator: std.mem.Allocator) error{OutOfMemory}!Buffer {
            var indices = try std.ArrayListUnmanaged(IndexEntry).initCapacity(allocator, min_component_alloc);
            errdefer indices.deinit(allocator);

            var reverse_indices = try std.ArrayListUnmanaged(PseudoIndex).initCapacity(allocator, min_component_alloc);
            errdefer reverse_indices.deinit(allocator);

            var storage: ?std.ArrayListUnmanaged(Component) = if (comptime @sizeOf(Component) > 0) try .initCapacity(allocator, min_component_alloc) else null;
            errdefer storage.deinit(allocator);

            return Buffer{
                .indices = indices,
                .reverse_indices = reverse_indices,
                .storage = storage orelse .empty,
            };
        }

        /// Deinitializes the component buffer, freeing its internal arrays.
        pub fn deinit(self: *Buffer, allocator: std.mem.Allocator) void {
            self.indices.deinit(allocator);
            self.reverse_indices.deinit(allocator);
            if (comptime @sizeOf(Component) > 0) self.storage.deinit(allocator);
            self.* = undefined;
        }

        /// Gets the number of active components in this buffer.
        pub fn count(self: *const Buffer) usize {
            return self.reverse_indices.items.len;
        }

        /// Gets a component and the associated entity `PseudoIndex` for a given `TrueIndex`.
        /// * Does not perform bounds checking; caller must ensure `dense_index < count()`.
        pub fn getIndex(self: *const Buffer, dense_index: TrueIndex) struct { *Component, PseudoIndex } {
            const pseudo_index = self.reverse_indices.items[dense_index];
            const component: *Component = if (comptime @sizeOf(Component) > 0) &self.storage.items[dense_index] else @ptrCast(@alignCast(@constCast(self))); // doesnt matter what we return for ZSTs, just needs to be non-null

            return .{ component, pseudo_index };
        }

        /// Gets a pointer to a component for a given `PseudoIndex`.
        /// Returns an error if the entity does not have this component, null if the component is active but is a ZST.
        pub fn getComponent(self: *const Buffer, index: PseudoIndex) error{DisabledComponent}!*Component {
            if (index >= self.indices.items.len) return error.DisabledComponent;

            // A direct, O(1) lookup into the sparse array.
            const entry = self.indices.items[index];
            if (!entry.enabled) return error.DisabledComponent;

            // A second direct, O(1) lookup into the dense array.
            return if (comptime @sizeOf(Component) > 0) &self.storage.items[entry.index] else @ptrCast(@alignCast(@constCast(self))); // doesnt matter what we return for ZSTs, just needs to be non-null
        }

        /// Adds a component for a new entity.
        pub fn setComponent(self: *Buffer, allocator: std.mem.Allocator, pseudo_index: PseudoIndex, component_data: ?*const Component) error{OutOfMemory}!void {
            // Ensure the sparse array is large enough to hold the new PseudoIndex.
            if (pseudo_index >= self.indices.items.len) {
                const old_len = self.indices.items.len;

                // Grow the sparse array to accommodate the new PseudoIndex.
                try self.indices.resize(allocator, pseudo_index + 1);

                // New entries are initialized as disabled.
                @memset(self.indices.items[old_len..], IndexEntry.disabled);
            }

            // if the component already exists for this entity, replace it and exit early.
            const entry = self.indices.items[pseudo_index];
            if (entry.enabled) {
                if (comptime @sizeOf(Component) > 0) {
                    if (component_data) |data| {
                        self.storage.items[entry.index] = data.*;
                    } else {
                        @memset(std.mem.asBytes(&self.storage.items[entry.index]), 0);
                    }
                }
                return;
            }

            // Add an entry to the sparse array. Its `TrueIndex` points to the end of the current dense storage.
            self.indices.items[pseudo_index] = .enable(self.storage.items.len);
            errdefer self.indices.items[pseudo_index] = .disabled;

            if (comptime @sizeOf(Component) > 0) {
                // Add a placeholder for the new component data at the end of the dense array.
                const component_storage = try self.storage.addOne(allocator);
                errdefer _ = self.storage.pop();

                // Copy the actual component data into the newly allocated slot.
                if (component_data) |data| {
                    component_storage.* = data.*;
                } else {
                    @memset(std.mem.asBytes(component_storage), 0);
                }
            } else {
                // Zero-sized types (ZSTs) do not require storage space; ensures there wasn't a bug.
                std.debug.assert(component_data == null);
            }

            // Add the new entity's PseudoIndex to the reverse map. This keeps `reverse_indices`
            // and `storage` perfectly in sync. `reverse_indices[N]` now correctly maps to `storage[N]`.
            try self.reverse_indices.append(allocator, pseudo_index);
            errdefer _ = self.reverse_indices.pop();
        }

        /// Removes a component for a given `PseudoIndex`, deinitializing it if necessary.
        pub fn delComponent(self: *Buffer, index: PseudoIndex) void {
            var old_component: Component = undefined;
            const success = self.takeComponent(index, &old_component);

            if (comptime std.meta.hasMethod(Component, "deinit")) {
                if (success) {
                    old_component.deinit();
                }
            }
        }

        /// Takes a component for a given `PseudoIndex` using a swap-remove operation, writing it to the provided pointer.
        ///
        /// * Note that this does not deinitialize the component; the caller is responsible for that if necessary.
        pub fn takeComponent(self: *Buffer, index: PseudoIndex, out: *Component) bool {
            // Safety check: ensure the index is valid.
            if (index >= self.indices.items.len) return false;

            // Get the sparse entry for the entity we are removing the component from.
            const entry = &self.indices.items[index];

            // Safety check: ensure we are not trying to remove a component that isn't there.
            if (!entry.enabled) return false;

            // Get the index in the dense `storage` array where the component lives.
            const index_to_remove: TrueIndex = entry.index;
            // Mark the component as disabled for this entity in the sparse array.
            entry.* = .disabled;

            // Perform the swap-remove on the dense storage. This moves the *last* component
            // in the array into the slot we just vacated, patching the "hole".
            out.* = if (comptime @sizeOf(Component) > 0) self.storage.swapRemove(index_to_remove) else std.mem.zeroes(Component); // yes i'd like a fully zeroed type of zero size please

            // Perform a parallel swap-remove on the reverse_indices array.
            _ = self.reverse_indices.swapRemove(index_to_remove);

            // Check if we actually moved anything. If the item we removed was already
            // the last one, the storage length is now equal to the old index, and no update is needed.
            if (index_to_remove < self.storage.items.len) {
                // After the swap-remove, the element that was at the end of `reverse_indices`
                // is now at `index_to_remove`. This is the PseudoIndex of the entity whose
                // component was just moved in the `storage` array.
                const moved_pseudo_index = self.reverse_indices.items[index_to_remove];

                // Update the sparse entry for the entity that was moved. Its component now
                // lives at the `index_to_remove`, so we update its `TrueIndex` to point there.
                self.indices.items[moved_pseudo_index].index = index_to_remove;
            }

            return true;
        }
    };
}

/// The TypedQuery struct wraps an id-based Query and provides a type-safe interface.
pub fn TypedQuery(comptime Q: anytype) type {
    const M = qtu.lenOf(Q);

    return struct {
        inner: Query(M),

        pub const Element = create_element_type: {
            var fields: [M]std.builtin.Type.StructField = undefined;

            var field_index: comptime_int = 0;

            for (0..M) |i| {
                const T = qtu.typeOf(Q, i);
                if (@TypeOf(T) == Exclude) continue;

                fields[field_index] = std.builtin.Type.StructField{
                    .name = std.fmt.comptimePrint("{}", .{i}),
                    .type = *T,
                    .default_value_ptr = null,
                    .is_comptime = false,
                    .alignment = @alignOf(*T),
                };
                field_index += 1;
            }

            break :create_element_type @Type(.{
                .@"struct" = std.builtin.Type.Struct{
                    .layout = .auto,
                    .backing_integer = null,
                    .decls = &.{},
                    .is_tuple = true,
                    .fields = fields[0..field_index],
                },
            });
        };

        pub fn next(self: *@This()) ?Element {
            const inner = self.inner.next() orelse return null;

            var out: Element = undefined;
            comptime var field_index: comptime_int = 0;

            inline for (0..M) |i| {
                const T = qtu.typeOf(Q, i);
                if (comptime @TypeOf(T) == Exclude) continue;

                out[field_index] = @ptrCast(@alignCast(inner[field_index]));
                comptime field_index += 1;
            }

            return out;
        }
    };
}

/// The Query struct is a generic iterator that yields entities matching a set of component filters.
pub fn Query(comptime M: comptime_int) type {
    return struct {
        const Q = @This();

        /// Designates which mode a query is operating in, either an optimized driver mode, or an all-encompassing mode.
        pub const QMode = enum { all, driver };

        /// used for reconstructing EntityIds or for driverless queries
        world: *const World,
        /// indicates
        mode: union(QMode) {
            all,
            /// the index of the "driver" buffer, which is iterated over directly
            driver: usize,
        } = undefined,
        /// pointers to the component buffers being queried
        buffers: [M]*BufferStorage = undefined,
        /// the filter (required/excluded) for each component buffer
        component_filters: [M]ComponentFilter = undefined,

        /// accumulator storing the actual selection of components for the current entity
        selection: [M]*anyopaque = undefined,
        /// accumulator for the current index within the driver buffer being processed
        index: TrueIndex = 0,

        /// accumulator for the current entity being processed, intended for user-level access within iterations
        entity: ?EntityId = null,

        /// Recursively attempts to build the next valid selection of components.
        fn build_selection(self: *Q) error{OutOfBounds}!?[]*anyopaque {
            if (comptime M == 0) {
                // no filters means all entities match, but we have no output capability;
                // just set entity and return empty slice

                while (self.index < self.world.entities.generations.items.len) {
                    const e_pseudo: PseudoIndex = @intCast(self.index);
                    const gen = self.world.entities.generations.items[e_pseudo];

                    // A generation of 0 means the slot has never been used.
                    // A non-zero generation means it's either live or on the free list.
                    // We can verify it's live by trying to "get" it.
                    if (@intFromEnum(gen) > 0 and self.world.entities.get(.{ .index = e_pseudo, .generation = gen }) != null) {
                        // Found a live entity, break from the while loop to process it.
                        break;
                    }

                    // This slot is not a live entity, so check the next one.
                    self.index += 1;
                }

                if (self.index >= self.world.entities.generations.items.len) return error.OutOfBounds;

                const e_pseudo: PseudoIndex = @intCast(self.index);

                self.entity = EntityId.wrap(.{ .index = e_pseudo, .generation = self.world.entities.generations.items[e_pseudo] });

                self.index += 1;

                return &.{};
            } else {
                switch (self.mode) {
                    // fast driver mode that iterates over the smallest component buffer
                    // this results in the fewest indirect lookups
                    .driver => |driver_index| {
                        const driver_buffer = self.buffers[driver_index];

                        if (self.index >= driver_buffer.count()) return error.OutOfBounds;

                        const driver_component, const e_pseudo = driver_buffer.getIndex(self.index);

                        self.entity = EntityId.wrap(.{ .index = e_pseudo, .generation = self.world.entities.generations.items[e_pseudo] });

                        self.index += 1;

                        var selection_index: usize = 0;

                        for (0..M) |i| {
                            if (i == driver_index) {
                                self.selection[selection_index] = driver_component;
                                selection_index += 1;
                            } else {
                                switch (self.component_filters[i]) {
                                    .excluded => exclusion: {
                                        const other_buffer = self.buffers[i];
                                        _ = other_buffer.getComponent(e_pseudo) catch break :exclusion;
                                        return self.build_selection();
                                    },
                                    .required => {
                                        const other_buffer = self.buffers[i];
                                        const component = other_buffer.getComponent(e_pseudo) catch {
                                            return self.build_selection();
                                        };
                                        self.selection[selection_index] = component;
                                        selection_index += 1;
                                    },
                                }
                            }
                        }

                        return self.selection[0..selection_index];
                    },

                    // Builds the next valid selection of components by iterating over the entity list and constructing ids.
                    // This is less efficient than the driver mode, as it requires checking all entities.
                    .all => {
                        while (self.index < self.world.entities.generations.items.len) {
                            const e_pseudo: PseudoIndex = @intCast(self.index);
                            const gen = self.world.entities.generations.items[e_pseudo];

                            // A generation of 0 means the slot has never been used.
                            // A non-zero generation means it's either live or on the free list.
                            // We can verify it's live by trying to "get" it.
                            if (@intFromEnum(gen) > 0 and self.world.entities.get(.{ .index = e_pseudo, .generation = gen }) != null) {
                                // Found a live entity, break from the while loop to process it.
                                break;
                            }

                            // This slot is not a live entity, so check the next one.
                            self.index += 1;
                        }

                        if (self.index >= self.world.entities.generations.items.len) return error.OutOfBounds;

                        const e_pseudo: PseudoIndex = @intCast(self.index);

                        const entity = EntityId.wrap(.{ .index = e_pseudo, .generation = self.world.entities.generations.items[e_pseudo] });
                        self.entity = entity;
                        self.index += 1;

                        var selection_index: usize = 0;

                        for (0..M) |i| {
                            switch (self.component_filters[i]) {
                                .excluded => exclusion: {
                                    _ = self.world.getComponentById(entity, self.component_filters[i].excluded) catch break :exclusion;
                                    return self.build_selection();
                                },
                                .required => {
                                    const component = self.world.getComponentById(entity, self.component_filters[i].required) catch {
                                        return self.build_selection();
                                    };
                                    self.selection[selection_index] = component;
                                    selection_index += 1;
                                },
                            }
                        }

                        return self.selection[0..selection_index];
                    },
                }
            }
        }

        /// Advances the iterator to the next matching entity and returns its components.
        /// After calling this function, the iterator's `entity` field will be set to the current entity the components belong to.
        pub fn next(self: *Q) ?[]*anyopaque {
            while (self.build_selection()) |maybe_selected| {
                if (maybe_selected) |selected| return selected;
            } else |_| {
                return null;
            }
        }
    };
}

const qtu = ecs.query_type_utils;
pub const query_type_utils = struct {
    pub fn lenOfT(comptime Q: type) comptime_int {
        return switch (@typeInfo(Q)) {
            .array => |info| info.len,
            .@"struct" => |info| info.fields.len,
            else => @compileError("Expected an array or struct, got " ++ @typeName(Q)),
        };
    }

    pub fn lenOf(comptime Q: anytype) comptime_int {
        return switch (@typeInfo(@TypeOf(Q))) {
            .array => |info| info.len,
            .@"struct" => |info| info.fields.len,
            else => @compileError("Expected an array or struct, got " ++ @typeName(Q)),
        };
    }

    pub fn resultOf(comptime Q: anytype, comptime i: comptime_int) type {
        return switch (@typeInfo(@TypeOf(Q))) {
            .array => |info| info.child,
            .@"struct" => |info| info.fields[i].type,
            else => @compileError("Expected an array or struct, got " ++ @typeName(Q)),
        };
    }

    pub fn typeOf(comptime Q: anytype, comptime i: comptime_int) resultOf(Q, i) {
        return Q[i];
    }

    pub fn queryM(comptime Q: type, comptime expect: ?type) comptime_int {
        comptime {
            const Q_info = @typeInfo(Q);

            switch (Q_info) {
                else => @compileError("Query must be a tuple struct or array containing ComponentFilter; got " ++ @typeName(Q)),

                .array => |info| {
                    if (expect) |e| {
                        if (info.child != e) @compileError("Query must be a tuple struct or array containing ComponentFilter; got " ++ @typeName(Q));
                    }

                    return info.len;
                },

                .@"struct" => |info| {
                    if (info.is_tuple) {
                        for (info.fields) |field| {
                            if (expect) |e| {
                                if (field.type != e) @compileError("Query must be a tuple struct or array containing ComponentFilter; got " ++ @typeName(Q));
                            }
                        }

                        return info.fields.len;
                    } else {
                        @compileError("Query must be a tuple struct or array containing ComponentFilter; got " ++ @typeName(Q));
                    }
                },
            }
        }
    }
};

test "ecs basic integration" {
    const testing = std.testing;
    const linalg = @import("linalg.zig");

    const allocator = testing.allocator;

    // --- Component Definitions ---
    const Position = struct {
        vec: linalg.vec3,
    };

    const Velocity = struct {
        vec: linalg.vec3,
    };

    const IsVisible = struct {}; // A Zero-Sized Type (ZST) for tagging.

    // --- Test Setup ---
    var world = ecs.World.init(allocator);
    defer world.deinit();

    // --- Component Registration ---
    _ = try world.bindComponentId(Position);
    _ = try world.bindComponentId(Velocity);
    _ = try world.bindComponentId(IsVisible);

    // --- Entity & Component Creation ---
    // Entity 'a': Position + Velocity
    const a = try world.addEmptyEntity();
    try world.setComponent(a, Position{ .vec = .{ 1.0, 2.0, 3.0 } });
    try world.setComponent(a, Velocity{ .vec = .{ 0.1, 0.0, 0.0 } });

    // Entity 'b': Position + IsVisible
    const b = try world.addEmptyEntity();
    try world.setComponent(b, Position{ .vec = .{ 4.0, 5.0, 6.0 } });
    try world.setComponent(b, IsVisible{});

    // Entity 'c': Position + Velocity + IsVisible
    const c = try world.addEmptyEntity();
    try world.setComponent(c, Position{ .vec = .{ 7.0, 8.0, 9.0 } });
    try world.setComponent(c, Velocity{ .vec = .{ 0.0, 0.2, 0.0 } });
    try world.setComponent(c, IsVisible{});

    // Entity 'd': Velocity only
    const d = try world.addEmptyEntity();
    try world.setComponent(d, Velocity{ .vec = .{ 0.0, 0.0, 0.3 } });

    // --- Query 1: Position AND IsVisible ---
    {
        // Expected: finds entities 'b' and 'c'
        var found_count: usize = 0;
        var found_b = false;
        var found_c = false;

        var it = try world.query(.{ Position, IsVisible });
        while (it.next()) |components| {
            found_count += 1;
            const pos = components[0];
            const entity = it.inner.entity.?;

            if (std.meta.eql(entity, b)) {
                found_b = true;
                try testing.expect(linalg.approxEqAbs(linalg.vec3{ 4.0, 5.0, 6.0 }, pos.vec, 1e-6));
            } else if (std.meta.eql(entity, c)) {
                found_c = true;
                try testing.expect(linalg.approxEqAbs(linalg.vec3{ 7.0, 8.0, 9.0 }, pos.vec, 1e-6));
            } else {
                // Fail if any other entity is found
                try testing.expect(false);
            }
        }
        try testing.expectEqual(@as(usize, 2), found_count);
        try testing.expect(found_b);
        try testing.expect(found_c);
    }

    // --- Query 2: Position AND Velocity ---
    {
        // Expected: finds entities 'a' and 'c'
        var found_count: usize = 0;
        var found_a = false;
        var found_c = false;

        var it = try world.query(.{ Position, Velocity });
        while (it.next()) |components| {
            found_count += 1;
            const pos = components[0];
            const vel = components[1];
            const entity = it.inner.entity.?;

            if (std.meta.eql(entity, a)) {
                found_a = true;
                try testing.expect(linalg.approxEqAbs(linalg.vec3{ 1.0, 2.0, 3.0 }, pos.vec, 1e-6));
                try testing.expect(linalg.approxEqAbs(linalg.vec3{ 0.1, 0.0, 0.0 }, vel.vec, 1e-6));
            } else if (std.meta.eql(entity, c)) {
                found_c = true;
                try testing.expect(linalg.approxEqAbs(linalg.vec3{ 7.0, 8.0, 9.0 }, pos.vec, 1e-6));
                try testing.expect(linalg.approxEqAbs(linalg.vec3{ 0.0, 0.2, 0.0 }, vel.vec, 1e-6));
            } else {
                try testing.expect(false);
            }
        }
        try testing.expectEqual(@as(usize, 2), found_count);
        try testing.expect(found_a);
        try testing.expect(found_c);
    }

    // --- Query 3: Position AND Velocity BUT NOT IsVisible ---
    {
        // Expected: finds only entity 'a'
        var found_count: usize = 0;
        var found_a = false;

        var it = try world.query(.{ Position, Velocity, ecs.Exclude{IsVisible} });
        while (it.next()) |components| {
            found_count += 1;
            const pos = components[0];
            const vel = components[1];
            const entity = it.inner.entity.?;

            if (std.meta.eql(entity, a)) {
                found_a = true;
                try testing.expect(linalg.approxEqAbs(linalg.vec3{ 1.0, 2.0, 3.0 }, pos.vec, 1e-6));
                try testing.expect(linalg.approxEqAbs(linalg.vec3{ 0.1, 0.0, 0.0 }, vel.vec, 1e-6));
            } else {
                try testing.expect(false);
            }
        }
        try testing.expectEqual(@as(usize, 1), found_count);
        try testing.expect(found_a);
    }

    // --- Query 4: Exclusion only, no driver components ---
    {
        // Expected: finds only entity 'd'
        var found_count: usize = 0;
        var found_d = false;

        var it = try world.query(.{ ecs.Exclude{Position}, ecs.Exclude{IsVisible} });
        while (it.next()) |_| {
            found_count += 1;
            const entity = it.inner.entity.?;

            if (std.meta.eql(entity, d)) {
                found_d = true;
            } else {
                try testing.expect(false);
            }
        }
        try testing.expectEqual(@as(usize, 1), found_count);
        try testing.expect(found_d);
    }

    // --- Query 5: All entities (no filters) ---
    {
        // Expected: finds all entities: 'a', 'b', 'c', and 'd'
        var found = std.AutoHashMap(ecs.EntityId, void).init(allocator);
        defer found.deinit();

        var it = try world.query(.{});
        while (it.next()) |_| {
            try found.put(it.inner.entity.?, {});
        }

        try testing.expectEqual(@as(usize, 4), found.count());
        try testing.expect(found.contains(a));
        try testing.expect(found.contains(b));
        try testing.expect(found.contains(c));
        try testing.expect(found.contains(d));
    }
}

test "ecs lifecycle and edge cases" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // --- Component Definitions ---
    const Position = struct { x: f32, y: f32 };
    const Velocity = struct { dx: f32, dy: f32 };

    // --- Test Setup ---
    var world = ecs.World.init(allocator);
    defer world.deinit();

    _ = try world.bindComponentId(Position);
    _ = try world.bindComponentId(Velocity);

    // --- Entity Creation & `addEntity` with prototype test ---
    const a = try world.addEntity(.{
        Position{ .x = 10, .y = 10 },
        Velocity{ .dx = 1, .dy = 0 },
    });
    const b = try world.addEntity(.{Position{ .x = 20, .y = 20 }});
    const c = try world.addEntity(.{Position{ .x = 30, .y = 30 }});

    // --- Initial State Verification ---
    try testing.expectEqual(@as(usize, 3), world.count());
    try testing.expectEqual(@as(usize, 3), try world.countComponentsByType(Position));
    try testing.expectEqual(@as(usize, 1), try world.countComponentsByType(Velocity));

    // --- Test Component Deletion ---
    const removed_pos = try world.takeComponent(c, Position);
    try testing.expect(removed_pos != null);
    try testing.expectEqualDeep(Position{ .x = 30, .y = 30 }, removed_pos.?);
    try testing.expectEqual(@as(usize, 2), try world.countComponentsByType(Position));

    // After removing Position from 'c', a query for Position should not find it.
    {
        var found_count: usize = 0;
        var it = try world.query(.{Position});
        while (it.next() != null) {
            const entity = it.inner.entity.?;
            try testing.expect(!std.meta.eql(entity, c));
            found_count += 1;
        }
        try testing.expectEqual(@as(usize, 2), found_count);
    }

    // --- Test Entity Deletion ---
    world.delEntity(b);
    try testing.expectEqual(@as(usize, 2), world.count());
    try testing.expectEqual(@as(usize, 1), try world.countComponentsByType(Position)); // Only entity 'a' has a position now

    // --- Test Entity ID Invalidation & Recycling ---
    // Try to get a component from the now-deleted entity 'b'. Should fail.
    try testing.expectError(error.EntityNotFound, world.getComponent(b, Position));

    // Create a new entity 'd'. It might recycle the slot from 'b'.
    const d = try world.addEntity(.{Position{ .x = 40, .y = 40 }});

    // The new entity 'd' should NOT be equal to the old 'b', because the generation number has been bumped.
    try testing.expect(!std.meta.eql(d, b));
    try testing.expectEqual(@as(usize, 2), try world.countComponentsByType(Position)); // 'a' and 'd'

    // Verify the query finds 'a' and 'd', but not 'b' or 'c'.
    {
        var found_a = false;
        var found_d = false;
        var it = try world.query(.{Position});
        while (it.next()) |comps| {
            const entity = it.inner.entity.?;
            if (std.meta.eql(entity, a)) {
                found_a = true;
                try testing.expectEqual(10, comps[0].x);
            } else if (std.meta.eql(entity, d)) {
                found_d = true;
                try testing.expectEqual(40, comps[0].x);
            } else {
                log.err("Query found unexpected entity!\n", .{});
                try testing.expect(false);
            }
        }
        try testing.expect(found_a);
        try testing.expect(found_d);
    }

    // Remove a component from entity 'a' to make the next query empty.
    try world.delComponent(a, Velocity);

    // --- Test Empty/No-Result Query ---
    // No entities have both Position and Velocity anymore.
    {
        var it = try world.query(.{ Position, Velocity });
        try testing.expect(it.next() == null);
    }
}

test "zero sized components that manage memory" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // --- Component Definition ---
    const ManagedTester = struct {
        var flag: i32 = 0;

        pub fn deinit(_: *@This()) void {
            flag += 1;
        }
    };

    // --- Test Setup ---
    var world = ecs.World.init(allocator);
    defer world.deinit();

    _ = try world.bindComponentId(ManagedTester);

    // --- Entity Creation & Component Assignment ---
    const e1 = try world.addEmptyEntity();

    const comp1 = ManagedTester{};
    try world.setComponent(e1, comp1);

    // --- Verify Component Exists ---
    {
        _ = try world.getComponent(e1, ManagedTester);
        try testing.expectEqual(0, ManagedTester.flag);
    }

    // --- Remove Component and Verify `deinit` Called ---
    {
        try world.delComponent(e1, ManagedTester);
        try testing.expectEqual(1, ManagedTester.flag); // deinit should have incremented flag
    }
}

test "sized components that manage memory" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // --- Component Definition ---
    const ManagedTester = struct {
        var flag: i32 = 0;

        value: usize,

        pub fn deinit(_: *@This()) void {
            flag += 1;
        }
    };

    // --- Test Setup ---
    var world = ecs.World.init(allocator);
    defer world.deinit();

    const managed_tester_id = try world.bindComponentId(ManagedTester);

    // --- Entity Creation & Component Assignment ---
    const e1 = try world.addEmptyEntity();

    const comp1 = ManagedTester{ .value = 100 };
    try world.setComponent(e1, comp1);

    // --- Verify Component Exists ---
    {
        const ptr = try world.getComponent(e1, ManagedTester);
        try testing.expectEqual(100, ptr.value);
        try testing.expectEqual(0, ManagedTester.flag);
    }

    // --- Remove Component and Verify `deinit` Called ---
    {
        try world.delComponentById(e1, managed_tester_id);
        try testing.expectEqual(1, ManagedTester.flag); // deinit should have incremented flag
    }
}
