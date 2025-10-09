//! A simple structure to hold the current input state for an application, including keyboard and mouse information.
//! Includes helper functions to collect input state from a glfw window.

const InputState = @This();

const std = @import("std");
const glfw = @import("glfw");

const log = std.log.scoped(.input_state);

test {
    log.debug("semantic analysis for InputState.zig", .{});
    std.testing.refAllDecls(@This());
}

const vec2 = @import("linalg.zig").vec2;

const MIN_KEY_CODE = 32; // The lowest key code we care about (space). See Key enum.
const KEY_ARRAY_SIZE = 349; // The size of the key state arrays. Must be at least one more than the highest key code in the Key enum.
const CHAR_ARRAY_SIZE = 16; // The size of the character input buffer. This can be adjusted as needed.
const MOUSE_ARRAY_SIZE = 8; // The size of the mouse button state arrays. GLFW currently defines 8 mouse buttons.

pub const Key = enum(std.math.IntFittingRange(0, KEY_ARRAY_SIZE)) {
    // NOTE: this definition must be kept in sync with libs/glfw.zig
    space = 32,
    apostrophe = 39,
    comma = 44,
    minus = 45,
    period = 46,
    slash = 47,
    @"0" = 48,
    @"1" = 49,
    @"2" = 50,
    @"3" = 51,
    @"4" = 52,
    @"5" = 53,
    @"6" = 54,
    @"7" = 55,
    @"8" = 56,
    @"9" = 57,
    semicolon = 59,
    equal = 61,
    a = 65,
    b = 66,
    c = 67,
    d = 68,
    e = 69,
    f = 70,
    g = 71,
    h = 72,
    i = 73,
    j = 74,
    k = 75,
    l = 76,
    m = 77,
    n = 78,
    o = 79,
    p = 80,
    q = 81,
    r = 82,
    s = 83,
    t = 84,
    u = 85,
    v = 86,
    w = 87,
    x = 88,
    y = 89,
    z = 90,
    left_bracket = 91,
    backslash = 92,
    right_bracket = 93,
    grave_accent = 96,
    world_1 = 161,
    world_2 = 162,
    escape = 256,
    enter = 257,
    tab = 258,
    backspace = 259,
    insert = 260,
    delete = 261,
    right = 262,
    left = 263,
    down = 264,
    up = 265,
    page_up = 266,
    page_down = 267,
    home = 268,
    end = 269,
    caps_lock = 280,
    scroll_lock = 281,
    num_lock = 282,
    print_screen = 283,
    pause = 284,
    f1 = 290,
    f2 = 291,
    f3 = 292,
    f4 = 293,
    f5 = 294,
    f6 = 295,
    f7 = 296,
    f8 = 297,
    f9 = 298,
    f10 = 299,
    f11 = 300,
    f12 = 301,
    f13 = 302,
    f14 = 303,
    f15 = 304,
    f16 = 305,
    f17 = 306,
    f18 = 307,
    f19 = 308,
    f20 = 309,
    f21 = 310,
    f22 = 311,
    f23 = 312,
    f24 = 313,
    f25 = 314,
    kp_0 = 320,
    kp_1 = 321,
    kp_2 = 322,
    kp_3 = 323,
    kp_4 = 324,
    kp_5 = 325,
    kp_6 = 326,
    kp_7 = 327,
    kp_8 = 328,
    kp_9 = 329,
    kp_decimal = 330,
    kp_divide = 331,
    kp_multiply = 332,
    kp_subtract = 333,
    kp_add = 334,
    kp_enter = 335,
    kp_equal = 336,
    left_shift = 340,
    left_control = 341,
    left_alt = 342,
    left_super = 343,
    right_shift = 344,
    right_control = 345,
    right_alt = 346,
    right_super = 347,
    menu = 348,

    pub fn isCharProducingKey(key: Key) bool {
        switch (key) {
            .space,
            .apostrophe,
            .comma,
            .minus,
            .period,
            .slash,
            .@"0",
            .@"1",
            .@"2",
            .@"3",
            .@"4",
            .@"5",
            .@"6",
            .@"7",
            .@"8",
            .@"9",
            .semicolon,
            .equal,
            .a,
            .b,
            .c,
            .d,
            .e,
            .f,
            .g,
            .h,
            .i,
            .j,
            .k,
            .l,
            .m,
            .n,
            .o,
            .p,
            .q,
            .r,
            .s,
            .t,
            .u,
            .v,
            .w,
            .x,
            .y,
            .z,
            .left_bracket,
            .backslash,
            .right_bracket,
            .grave_accent,
            .kp_0,
            .kp_1,
            .kp_2,
            .kp_3,
            .kp_4,
            .kp_5,
            .kp_6,
            .kp_7,
            .kp_8,
            .kp_9,
            .kp_decimal,
            .kp_divide,
            .kp_multiply,
            .kp_subtract,
            .kp_add,
            .kp_enter,
            .kp_equal,
            => return true,
            else => return false,
        }
    }

    pub fn validateKeyCode(code: usize) bool {
        const key_names = comptime std.meta.fieldNames(Key);
        comptime var keys: [KEY_ARRAY_SIZE]bool = [1]bool{false} ** KEY_ARRAY_SIZE;
        inline for (key_names) |name| {
            keys[@intFromEnum(@field(Key, name))] = true;
        }

        if (code < 0 or code >= KEY_ARRAY_SIZE) {
            return false;
        }

        return keys[code];
    }

    pub fn fromKeyCode(code: usize) ?Key {
        if (!validateKeyCode(code)) {
            return null;
        }

        return @enumFromInt(code);
    }

    pub fn fromGlfw(code: glfw.Key) ?Key {
        return @enumFromInt(@intFromEnum(code));
    }
};

pub const Action = enum(u2) {
    none,
    released,
    pressed,
    held,

    pub fn isDown(self: Action) bool {
        return self == .pressed or self == .held;
    }

    pub fn isUp(self: Action) bool {
        return self == .none or self == .released;
    }

    pub fn fromBooleans(is_down: bool, was_down: bool) Action {
        if (is_down) {
            if (was_down) {
                return .held;
            } else {
                return .pressed;
            }
        } else {
            if (was_down) {
                return .released;
            } else {
                return .none;
            }
        }
    }
};

pub const Focus = enum(u2) {
    none,
    retained,
    gained,
    lost,

    pub fn isFocused(self: Focus) bool {
        return self == .retained or self == .gained;
    }

    pub fn isBlurred(self: Focus) bool {
        return self == .none or self == .lost;
    }

    pub fn fromBooleans(has_focus: bool, had_focus: bool) Focus {
        if (has_focus) {
            if (had_focus) {
                return .retained;
            } else {
                return .gained;
            }
        } else {
            if (had_focus) {
                return .lost;
            } else {
                return .none;
            }
        }
    }
};

pub const Modifiers = packed struct(u6) {
    ctrl: bool = false,
    shift: bool = false,
    alt: bool = false,
    super: bool = false,
    caps_lock: bool = false,
    num_lock: bool = false,

    pub const noMod = Modifiers{};
    pub const ctrlMod = Modifiers{ .ctrl = true };
    pub const shiftMod = Modifiers{ .shift = true };
    pub const altMod = Modifiers{ .alt = true };
    pub const superMod = Modifiers{ .super = true };
    pub const capsLockMod = Modifiers{ .caps_lock = true };
    pub const numLockMod = Modifiers{ .num_lock = true };
    pub const allMods = Modifiers{
        .ctrl = true,
        .shift = true,
        .alt = true,
        .super = true,
        .caps_lock = true,
        .num_lock = true,
    };

    pub fn merge(self: Modifiers, other: Modifiers) Modifiers {
        return Modifiers{
            .ctrl = self.ctrl or other.ctrl,
            .shift = self.shift or other.shift,
            .alt = self.alt or other.alt,
            .super = self.super or other.super,
            .caps_lock = self.caps_lock or other.caps_lock,
            .num_lock = self.num_lock or other.num_lock,
        };
    }

    pub fn fromKeys(keys: []const bool) Modifiers {
        return Modifiers{
            .ctrl = keys[@intFromEnum(Key.left_control)] or keys[@intFromEnum(Key.right_control)],
            .shift = keys[@intFromEnum(Key.left_shift)] or keys[@intFromEnum(Key.right_shift)],
            .alt = keys[@intFromEnum(Key.left_alt)] or keys[@intFromEnum(Key.right_alt)],
            .super = keys[@intFromEnum(Key.left_super)] or keys[@intFromEnum(Key.right_super)],
            .caps_lock = keys[@intFromEnum(Key.caps_lock)],
            .num_lock = keys[@intFromEnum(Key.num_lock)],
        };
    }

    pub fn fromGlfw(mods: glfw.Modifier) Modifiers {
        return Modifiers{
            .ctrl = mods.control,
            .shift = mods.shift,
            .alt = mods.alt,
            .super = mods.super,
            .caps_lock = mods.caps_lock,
            .num_lock = mods.num_lock,
        };
    }
};

pub const MouseButton = enum(u3) {
    button_1 = 0,
    button_2 = 1,
    button_3 = 2,
    button_4 = 3,
    button_5 = 4,
    button_6 = 5,
    button_7 = 6,
    button_8 = 7,

    pub const left = MouseButton.button_1;
    pub const right = MouseButton.button_2;
    pub const middle = MouseButton.button_3;
    // TODO: double check which is the standard forward/back buttons
    pub const forward = MouseButton.button_4;
    pub const back = MouseButton.button_5;

    pub fn fromGlfw(code: glfw.MouseButton) MouseButton {
        return @enumFromInt(@intFromEnum(code));
    }
};

/// Represents a unicode character input event
pub const Char = packed struct(u27) {
    codepoint: u21,
    modifiers: Modifiers,
};

window: *glfw.Window,

have_keyboard_focus: bool = false,
keyboard_focus_last: bool = false,

have_mouse_focus: bool = false,
mouse_focus_last: bool = false,

mouse_buttons: [MOUSE_ARRAY_SIZE]bool = [1]bool{false} ** MOUSE_ARRAY_SIZE,
mouse_buttons_last: [MOUSE_ARRAY_SIZE]bool = [1]bool{false} ** MOUSE_ARRAY_SIZE,

keys: [KEY_ARRAY_SIZE]bool = [1]bool{false} ** KEY_ARRAY_SIZE,
keys_last: [KEY_ARRAY_SIZE]bool = [1]bool{false} ** KEY_ARRAY_SIZE,

modifiers: Modifiers = .{},

mouse_position: vec2 = .{ 0, 0 },

scroll_delta: vec2 = .{ 0, 0 },
scroll_multiplier: f32 = 6.0, // Scroll delta from glfw tends to be small, this multiplier can be adjusted as needed but 6 is a good default

chars: []Char = &.{}, // slice into chars_storage
_chars_storage: [CHAR_ARRAY_SIZE]Char = undefined, // Input character accumulation buffer

/// Create an InputState for a given glfw window.
pub fn init(window: *glfw.Window) InputState {
    return InputState{
        .window = window,
    };
}

/// Gets the current scroll delta for the frame, and resets it to zero.
pub fn consumeScrollDelta(self: *InputState) vec2 {
    defer self.scroll_delta = vec2{ 0, 0 };

    return .{
        self.scroll_delta[0] * self.scroll_multiplier,
        self.scroll_delta[1] * self.scroll_multiplier,
    };
}

/// Gets the current character input events for the frame, and resets the buffer.
pub fn consumeCharInput(self: *InputState) []const Char {
    const input = self.chars;
    self.chars = &.{};
    return input;
}

/// Returns the current state of keyboard focus for the window.
pub fn getKeyboardFocus(self: *const InputState) Focus {
    return Focus.fromBooleans(self.have_keyboard_focus, self.keyboard_focus_last);
}

/// Returns the current state of mouse focus (whether the mouse is over the window).
pub fn getMouseFocus(self: *const InputState) Focus {
    return Focus.fromBooleans(self.have_mouse_focus, self.mouse_focus_last);
}

/// Returns the current mouse position relative to the top-left corner of the window.
/// Note that this always returns a value, but the coordinates provided may not be within the window bounds.
/// Use `InputState.getMouseFocus` to determine if the mouse is over the window.
pub fn getMousePosition(self: *const InputState) vec2 {
    return self.mouse_position;
}

/// Returns the current state of the specified mouse button.
pub fn getMouseButton(self: *const InputState, button: MouseButton) Action {
    const is_down = self.mouse_buttons[@intFromEnum(button)];
    const was_down = self.mouse_buttons_last[@intFromEnum(button)];

    return Action.fromBooleans(is_down, was_down);
}

/// Returns the current state of the specified key.
pub fn getKey(self: *const InputState, key: Key) Action {
    const is_down = self.keys[@intFromEnum(key)];
    const was_down = self.keys_last[@intFromEnum(key)];

    return Action.fromBooleans(is_down, was_down);
}

/// Returns the currently active modifiers.
/// If the window does not have keyboard focus, all modifiers will be false.
pub fn getModifiers(self: *const InputState) Modifiers {
    return self.modifiers;
}

/// Note that this function sets up multiple direct callbacks to collect scroll and character input states, so it can only be used once per glfw Window, and once per thread.
/// If you need to collect scroll input in multiple places, use `InputState.accumulateScroll` and `InputState.accumulateChar` instead.
/// Additionally, this function does not track state like mouse position or keys pressed; you must call `InputState.collectAllGlfw` each frame to gather that information.
/// See also `InputState.listenMouseScrollGlfw` and `InputState.listenCharInputGlfw`.
pub fn listenAllGlfw(self: *InputState) void {
    self.listenMouseScrollGlfw();
    self.listenCharInputGlfw();
}

/// Note that this function sets up a direct callback to collect scroll input, so it can only be used once per glfw Window, and once per thread.
/// If you need to collect scroll input in multiple places, use `InputState.accumulateScroll` instead.
/// See also `InputState.listenAllGlfw` to collect character input as well.
pub fn listenMouseScrollGlfw(self: *InputState) void {
    direct_scroll_delta = &self.scroll_delta;
    _ = glfw.setScrollCallback(self.window, @ptrCast(&directScrollGlfwCallback));
}

/// Note that this function sets up a direct callback to collect codepoint input, so it can only be used once per glfw Window, and once per thread.
/// If you need to collect character input in multiple places, use `InputState.accumulateChar` instead.
/// See also `InputState.listenAllGlfw` to collect mouse scroll input as well.
pub fn listenCharInputGlfw(self: *InputState) void {
    direct_char_input = .{
        .storage = &self._chars_storage,
        .view = &self.chars,
    };
    _ = glfw.setCharModsCallback(self.window, @ptrCast(&directCharGlfwCallback));
}

/// Manually add to the scroll offset for the frame. This can be used in addition to `InputState.listenMouseScrollGlfw` if needed.
/// See also `InputState.listenAllGlfw`, `InputState.listenMouseScrollGlfw` for event listeners that collect this input.
pub fn accumulateScroll(self: *InputState, additional_offset: vec2) void {
    self.scroll_delta[0] += additional_offset[0];
    self.scroll_delta[1] += additional_offset[1];
}

/// Manually add a character input event. This can be used in addition to `InputState.listenCharInputGlfw` if needed.
/// See also `InputState.listenAllGlfw`, `InputState.listenCharInputGlfw` for event listeners that collect this input.
pub fn accumulateChar(self: *InputState, char: Char) void {
    const index = self.chars.len % CHAR_ARRAY_SIZE;
    self._chars_storage[index] = char;
    self.chars = self._chars_storage[0 .. index + 1];
}

/// Manually set the state of keyboard focus for the window.
/// See also `InputState.collectAllGlfw`.
pub fn setKeyboardFocus(self: *InputState, has_focus: bool) void {
    self.keyboard_focus_last = self.have_keyboard_focus;
    self.have_keyboard_focus = has_focus;
}

/// Set the clipboard value.
pub fn setClipboard(self: *InputState, text: []const u8) !void {
    _ = clipboard_temp.reset(.retain_capacity);

    const text_z = try clipboard_temp.allocator().dupeZ(u8, text);

    glfw.setClipboardString(self.window, text_z);
}
threadlocal var clipboard_temp: std.heap.ArenaAllocator = .init(std.heap.page_allocator);

/// Get the clipboard value.
pub fn getClipboard(self: *InputState) []const u8 {
    return if (glfw.getClipboardString(self.window)) |str| std.mem.span(str) else &.{};
}

/// Manually set the state of mouse focus (whether the mouse is over the window).
/// See also `InputState.collectAllGlfw`.
pub fn setMouseFocus(self: *InputState, has_focus: bool) void {
    self.mouse_focus_last = self.have_mouse_focus;
    self.have_mouse_focus = has_focus;
}

/// Manually set the mouse position for the window.
/// See also `InputState.collectAllGlfw`.
pub fn setMousePosition(self: *InputState, position: vec2) void {
    // we always want the mouse position, even if the window doesn't have mouse focus
    // this allows us to continue drag interactions etc, even if the mouse has moved outside the window
    self.mouse_position = position;
}

/// Manually set the state of all mouse buttons.
/// See also `InputState.collectAllGlfw`.
pub fn setMouseButtons(self: *InputState, active_buttons: []const MouseButton) void {
    self.mouse_buttons_last = self.mouse_buttons;

    self.mouse_buttons = [1]bool{false} ** MOUSE_ARRAY_SIZE;

    // unlike with keys, we always want to collect mouse button states, even if the window doesn't have mouse focus
    // this allows us to continue drag interactions etc, even if the mouse has moved outside the window
    for (active_buttons) |button_state| {
        self.mouse_buttons[@intFromEnum(button_state)] = true;
    }
}

/// Manually set the state of all keyboard keys.
/// See also `InputState.collectAllGlfw`.
pub fn setKeys(self: *InputState, active_keys: []const Key) void {
    self.keys_last = self.keys;

    self.keys = [1]bool{false} ** KEY_ARRAY_SIZE;

    if (self.have_keyboard_focus) {
        for (active_keys) |key_state| {
            self.keys[@intFromEnum(key_state)] = true;
        }
    }

    self.modifiers = Modifiers.fromKeys(&self.keys);
}

/// * Collects:
/// * whether the window has keyboard focus
/// * mouse position and whether the mouse is over the window (mouse focus)
/// * mouse button and keyboard key states from the window
///
/// Note that this function does not handle scroll or character input, which must be collected via callbacks.
///
/// See also `InputState.listenAllGlfw` or `InputState.listenMouseScrollGlfw`/`InputState.listenCharInputGlfw`;
/// or use `InputState.accumulateScroll` and `InputState.accumulateChar` to manually add these inputs.
///
/// If you need finer-grained control, see `InputState.collectKeyboardFocusGlfw`, `InputState.collectMouseStateGlfw`, `InputState.collectButtonsGlfw` and `InputState.collectKeysGlfw`;
/// or use `InputState.setKeyboardFocus`, `InputState.setMousePosition`, `InputState.setMouseButtons` and `InputState.setKeys` to manually set these states.
pub fn collectAllGlfw(self: *InputState) void {
    self.collectKeyboardFocusGlfw();
    self.collectMouseStateGlfw();
    self.collectButtonsGlfw();
    self.collectKeysGlfw();
}

/// Collects whether the specified glfw window has keyboard focus.
/// See also `InputState.collectAllGlfw`.
pub fn collectKeyboardFocusGlfw(self: *InputState) void {
    self.keyboard_focus_last = self.have_keyboard_focus;
    self.have_keyboard_focus = glfw.getWindowAttrib(self.window, .focused) != 0;
}

/// Collects mouse position and whether the specified glfw window has mouse focus.
/// See also `InputState.collectAllGlfw`.
pub fn collectMouseStateGlfw(self: *InputState) void {
    var window_w: i32 = 0;
    var window_h: i32 = 0;
    glfw.getFramebufferSize(self.window, &window_w, &window_h);

    var mouse_x: f64 = 0;
    var mouse_y: f64 = 0;
    glfw.getCursorPos(self.window, &mouse_x, &mouse_y);

    self.mouse_focus_last = self.have_mouse_focus;
    self.have_mouse_focus = mouse_x >= 0 and mouse_y >= 0 and mouse_x <= @as(f64, @floatFromInt(window_w)) and mouse_y <= @as(f64, @floatFromInt(window_h));
    self.mouse_position = .{ @as(f32, @floatCast(mouse_x)), @as(f32, @floatCast(mouse_y)) };
}

/// Collects mouse button and keyboard key states from the specified glfw window.
/// See also `InputState.collectAllGlfw`.
pub fn collectButtonsGlfw(self: *InputState) void {
    self.mouse_buttons_last = self.mouse_buttons;

    // unlike with keys, we always want to collect mouse button states, even if the window doesn't have mouse focus
    // this allows us to continue drag interactions etc, even if the mouse has moved outside the window
    for (&self.mouse_buttons, 0..) |*button_state, code| {
        button_state.* = glfw.getMouseButton(self.window, @enumFromInt(code)).isDown();
    }
}

/// Collects keyboard key states from the specified glfw window.
/// See also `InputState.collectAllGlfw`.
pub fn collectKeysGlfw(self: *InputState) void {
    self.keys_last = self.keys;

    if (self.have_keyboard_focus) {
        for (&self.keys, 0..) |*key_state, code| {
            if (Key.validateKeyCode(code)) {
                key_state.* = glfw.getKey(self.window, @enumFromInt(code)).isDown();
            }
        }
    } else {
        self.keys = [1]bool{false} ** KEY_ARRAY_SIZE;
    }

    self.modifiers = Modifiers.fromKeys(&self.keys);
}

threadlocal var direct_scroll_delta: *vec2 = undefined;
fn directScrollGlfwCallback(_: *glfw.Window, x_offset: f64, y_offset: f64) callconv(.c) void {
    direct_scroll_delta.* += .{ @floatCast(x_offset), @floatCast(y_offset) };
}

threadlocal var direct_char_input: struct { storage: *[CHAR_ARRAY_SIZE]Char, view: *[]Char } = undefined;
fn directCharGlfwCallback(_: *glfw.Window, codepoint: u32, modifiers: glfw.Modifier) callconv(.c) void {
    if (direct_char_input.view.len < direct_char_input.storage.len) {
        const index = direct_char_input.view.len % CHAR_ARRAY_SIZE;
        direct_char_input.storage[index] = Char{
            .codepoint = @truncate(codepoint),
            .modifiers = .fromGlfw(modifiers),
        };
        direct_char_input.view.* = direct_char_input.storage[0 .. index + 1];
    }
}
