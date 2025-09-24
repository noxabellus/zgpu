//! An abstraction layer over InputState to handle custom key bindings,
//! allowing actions to be queried via a user-defined enum.

const BindingState = @This();

const std = @import("std");
const InputState = @import("InputState.zig");

pub const Vec2 = InputState.Vec2; // TODO: linalg library
pub const Action = InputState.Action;
pub const Char = InputState.Char;
pub const Modifiers = InputState.Modifiers;
pub const Key = InputState.Key;
pub const MouseButton = InputState.MouseButton;

const log = std.log.scoped(.input_bindings);

test {
    log.debug("semantic analysis for BindingState.zig", .{});
    std.testing.refAllDecls(@This());
}

allocator: std.mem.Allocator,
input_state: *InputState,
bindings: std.AutoHashMapUnmanaged(*const anyopaque, InputBinding),

pub fn init(allocator: std.mem.Allocator, input_state: *InputState) BindingState {
    return .{
        .allocator = allocator,
        .input_state = input_state,
        .bindings = .{},
    };
}

pub fn deinit(self: *BindingState) void {
    self.bindings.deinit(self.allocator);
}

/// Registers a new input binding with a compile-time known enum literal.
/// The enum literal acts as a unique, type-safe identifier for the action.
/// Example: `try bindings.registerBinding(.jump, .{.key = .{.key = .space}})`
pub fn bind(self: *BindingState, comptime name: @Type(.enum_literal), binding: InputBinding) !void {
    try self.bindings.put(self.allocator, @tagName(name), binding);
}

/// Determine if a given input has a binding in this BindingState.
pub fn hasBinding(self: *const BindingState, comptime name: @Type(.enum_literal)) bool {
    return self.bindings.contains(@tagName(name));
}

/// Get the InputBinding associated with a given enum literal, if it exists.
pub fn getBinding(self: *const BindingState, comptime name: @Type(.enum_literal)) ?InputBinding {
    return self.bindings.get(@tagName(name));
}

/// Retrieves the current action state of a registered binding.
/// The enum literal must correspond to a previously registered binding.
/// Example: `const jump_action = bindings.getBinding(.jump)`
pub fn getAction(self: *const BindingState, comptime name: @Type(.enum_literal)) Action {
    const binding = self.bindings.get(@tagName(name)) orelse {
        return .none;
    };

    return binding.query(self.input_state);
}

/// Get the mouse position from the underlying InputState.
pub fn getMousePosition(self: *const BindingState) Vec2 {
    return self.input_state.getMousePosition();
}

/// Consume the mouse wheel delta for this frame from the underlying InputState.
pub fn consumeScrollDelta(self: *BindingState) Vec2 {
    return self.input_state.consumeScrollDelta();
}

/// Consume the text entry buffer for this frame from the underlying InputState.
pub fn consumeCharInput(self: *BindingState) []const Char {
    return self.input_state.consumeCharInput();
}

/// Represents a specific input configuration that can be bound to an action.
pub const InputBinding = union(enum) {
    key: KeyBinding,
    mouse: MouseBinding,

    pub const KeyBinding = struct {
        bind_point: InputState.Key,
        /// If null, no modifiers are required. Otherwise, all specified modifiers must be active.
        modifiers: ?InputState.Modifiers = null,
    };

    pub const MouseBinding = struct {
        bind_point: InputState.MouseButton,
        /// If null, no modifiers are required. Otherwise, all specified modifiers must be active.
        modifiers: ?InputState.Modifiers = null,
    };

    pub fn getAction(self: InputBinding, input_state: *InputState) Action {
        return switch (self) {
            .key => |kb| input_state.getKey(kb.bind_point),
            .mouse => |mb| input_state.getMouseButton(mb.bind_point),
        };
    }

    pub fn getModifiers(self: InputBinding) ?InputState.Modifiers {
        return switch (self) {
            .key => |kb| kb.modifiers,
            .mouse => |mb| mb.modifiers,
        };
    }

    pub fn checkModifiers(self: InputBinding, input_state: *InputState) bool {
        const required_mods = self.getModifiers() orelse return true;
        const current_mods = input_state.getModifiers();

        // This logic ensures that *at least* the required modifiers are active.
        // It doesn't preclude other modifiers from also being active.
        // zig fmt: off
        return (!required_mods.shift or current_mods.shift)
           and (!required_mods.ctrl or current_mods.ctrl)
           and (!required_mods.alt or current_mods.alt)
           and (!required_mods.super or current_mods.super)
           and (!required_mods.caps_lock or current_mods.caps_lock)
           and (!required_mods.num_lock or current_mods.num_lock);
        // zig fmt: on
    }

    pub fn query(self: InputBinding, input_state: *InputState) Action {
        // First, check if the required modifiers are met.
        if (!self.checkModifiers(input_state)) return .none;

        // Then get the state of the primary input.
        return self.getAction(input_state);
    }
};
