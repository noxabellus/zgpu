const std = @import("std");
const log = std.log.scoped(.glfw);

test {
    log.debug("semantic analysis for glfw.zig", .{});
    std.testing.refAllDecls(@This());
}

pub const Proc = ?*const fn () callconv(.c) void;

pub const Monitor = opaque {};
pub const Window = opaque {};
pub const Cursor = opaque {};

pub const AllocateCallback = ?*const fn (size: usize, userdata: ?*anyopaque) callconv(.c) ?*anyopaque;
pub const ReallocateCallback = ?*const fn (block: ?*anyopaque, new_size: usize, userdata: ?*anyopaque) callconv(.c) ?*anyopaque;
pub const DeallocateCallback = ?*const fn (block: ?*anyopaque, userdata: ?*anyopaque) callconv(.c) void;

pub const ErrorCallback = ?*const fn (ErrorCode, description: ?[*:0]const u8) callconv(.c) void;

pub const WindowPosCallback = ?*const fn (*Window, x_pos: i32, y_pos: i32) callconv(.c) void;
pub const WindowSizeCallback = ?*const fn (*Window, width: i32, height: i32) callconv(.c) void;
pub const WindowCloseCallback = ?*const fn (*Window) callconv(.c) void;
pub const WindowRefreshCallback = ?*const fn (*Window) callconv(.c) void;
pub const WindowFocusCallback = ?*const fn (*Window, FocusEvent) callconv(.c) void;
pub const WindowIconifyCallback = ?*const fn (*Window, MinimizeEvent) callconv(.c) void;
pub const WindowMaximizeCallback = ?*const fn (*Window, MaximizeEvent) callconv(.c) void;
pub const FramebufferSizeCallback = ?*const fn (*Window, width: i32, height: i32) callconv(.c) void;
pub const WindowContentScaleCallback = ?*const fn (*Window, x_scale: f32, y_scale: f32) callconv(.c) void;
pub const MouseButtonCallback = ?*const fn (*Window, MouseButton, KeyState32, Modifier) callconv(.c) void;
pub const CursorPosCallback = ?*const fn (*Window, x_position: f64, y_position: f64) callconv(.c) void;
pub const CursorEnterCallback = ?*const fn (*Window, CursorEvent) callconv(.c) void;
pub const ScrollCallback = ?*const fn (*Window, x_offset: f64, y_offset: f64) callconv(.c) void;
pub const KeyCallback = ?*const fn (window: *Window, key: Key, scancode: i32, action: KeyState32, mods: Modifier) callconv(.c) void;
pub const CharCallback = ?*const fn (*Window, codepoint: u32) callconv(.c) void;
pub const CharModsCallback = ?*const fn (*Window, codepoint: u32, Modifier) callconv(.c) void;
pub const DropCallback = ?*const fn (*Window, dropped_path_count: i32, dropped_paths: ?[*][*:0]const u8) callconv(.c) void;
pub const MonitorCallback = ?*const fn (*Monitor, ConnectionEvent) callconv(.c) void;
pub const JoystickCallback = ?*const fn (joystick_id: i32, ConnectionEvent) callconv(.c) void;

pub const VideoMode = extern struct {
    width: i32 = 0,
    height: i32 = 0,
    redBits: i32 = 0,
    greenBits: i32 = 0,
    blueBits: i32 = 0,
    refreshRate: i32 = 0,
};

pub const GammaRamp = extern struct {
    red: ?[*]u16 = null,
    green: ?[*]u16 = null,
    blue: ?[*]u16 = null,
    size: u32 = 0,
};

pub const Image = extern struct {
    width: i32 = 0,
    height: i32 = 0,
    pixels: ?[*]u8 = null,
};

pub const GamepadState = extern struct {
    buttons: [15]u8 = [1]u8{0} ** 15,
    axes: [6]f32 = [1]f32{0.0} ** 6,
};

pub const Allocator = extern struct {
    allocate: AllocateCallback = null,
    reallocate: ReallocateCallback = null,
    deallocate: DeallocateCallback = null,
    user: ?*anyopaque = null,
};

extern fn glfwInit() callconv(.c) i32;
pub fn init() !void {
    if (glfwInit() != 1) return error.FailedToInitGLFW;
}

pub const deinit = @extern(*const fn () callconv(.c) void, .{ .name = "glfwTerminate" });

extern fn glfwInitHint(hint: i32, value: i32) callconv(.c) void;
pub fn initHint(hint: InitHint) void {
    const tag: InitHintTag = hint;
    switch (hint) {
        .platform => |pf| glfwInitHint(@intFromEnum(tag), @intFromEnum(pf)),
        .angle_platform_type => |pt| glfwInitHint(@intFromEnum(tag), @intFromEnum(pt)),
        .joystick_hat_buttons => |v| glfwInitHint(@intFromEnum(tag), @intFromBool(v)),
        .cocoa_chdir_resources => |v| glfwInitHint(@intFromEnum(tag), @intFromBool(v)),
        .cocoa_menubar => |v| glfwInitHint(@intFromEnum(tag), @intFromBool(v)),
        .wayland_libdecor => |dc| glfwInitHint(@intFromEnum(tag), @intFromEnum(dc)),
        .x11_xcb_vulkan_surface => |v| glfwInitHint(@intFromEnum(tag), @intFromBool(v)),
    }
}

pub const initAllocator = @extern(*const fn (allocator: *const Allocator) callconv(.c) void, .{ .name = "glfwInitAllocator" });
pub const getVersion = @extern(*const fn (major: *i32, minor: *i32, rev: *i32) callconv(.c) void, .{ .name = "glfwGetVersion" });
pub const getVersionString = @extern(*const fn () callconv(.c) [*:0]const u8, .{ .name = "glfwGetVersionString" });
pub const getError = @extern(*const fn (description: ?*[*:0]const u8) callconv(.c) i32, .{ .name = "glfwGetError" });
pub const setErrorCallback = @extern(*const fn (callback: ErrorCallback) callconv(.c) ErrorCallback, .{ .name = "glfwSetErrorCallback" });
pub const getPlatform = @extern(*const fn () callconv(.c) i32, .{ .name = "glfwGetPlatform" });
pub const platformSupported = @extern(*const fn (platform: i32) callconv(.c) i32, .{ .name = "glfwPlatformSupported" });
pub const getMonitors = @extern(*const fn (count: *i32) callconv(.c) ?[*]*Monitor, .{ .name = "glfwGetMonitors" });
pub const getPrimaryMonitor = @extern(*const fn () callconv(.c) ?*Monitor, .{ .name = "glfwGetPrimaryMonitor" });
pub const getMonitorPos = @extern(*const fn (monitor: *Monitor, xpos: *i32, ypos: *i32) callconv(.c) void, .{ .name = "glfwGetMonitorPos" });
pub const getMonitorWorkarea = @extern(*const fn (monitor: *Monitor, xpos: *i32, ypos: *i32, width: *i32, height: *i32) callconv(.c) void, .{ .name = "glfwGetMonitorWorkarea" });
pub const getMonitorPhysicalSize = @extern(*const fn (monitor: *Monitor, widthMM: *i32, heightMM: *i32) callconv(.c) void, .{ .name = "glfwGetMonitorPhysicalSize" });
pub const getMonitorContentScale = @extern(*const fn (monitor: *Monitor, xscale: *f32, yscale: *f32) callconv(.c) void, .{ .name = "glfwGetMonitorContentScale" });
pub const getMonitorName = @extern(*const fn (monitor: *Monitor) callconv(.c) ?[*:0]const u8, .{ .name = "glfwGetMonitorName" });
pub const setMonitorUserPointer = @extern(*const fn (monitor: *Monitor, pointer: ?*anyopaque) callconv(.c) void, .{ .name = "glfwSetMonitorUserPointer" });
pub const getMonitorUserPointer = @extern(*const fn (monitor: *Monitor) callconv(.c) ?*anyopaque, .{ .name = "glfwGetMonitorUserPointer" });
pub const setMonitorCallback = @extern(*const fn (callback: MonitorCallback) callconv(.c) MonitorCallback, .{ .name = "glfwSetMonitorCallback" });
pub const getVideoModes = @extern(*const fn (monitor: *Monitor, count: *i32) callconv(.c) ?[*]const VideoMode, .{ .name = "glfwGetVideoModes" });
pub const getVideoMode = @extern(*const fn (monitor: *Monitor) callconv(.c) ?*const VideoMode, .{ .name = "glfwGetVideoMode" });
pub const setGamma = @extern(*const fn (monitor: *Monitor, gamma: f32) callconv(.c) void, .{ .name = "glfwSetGamma" });
pub const getGammaRamp = @extern(*const fn (monitor: *Monitor) callconv(.c) ?*const GammaRamp, .{ .name = "glfwGetGammaRamp" });
pub const setGammaRamp = @extern(*const fn (monitor: *Monitor, ramp: ?*const GammaRamp) callconv(.c) void, .{ .name = "glfwSetGammaRamp" });
pub const defaultWindowHints = @extern(*const fn () callconv(.c) void, .{ .name = "glfwDefaultWindowHints" });

extern fn glfwWindowHint(hint: WindowHintTag, value: i32) callconv(.c) void;
extern fn glfwWindowHintString(hint: WindowHintTag, value: ?[*:0]const u8) callconv(.c) void;
pub fn windowHint(hint: WindowHint) void {
    const tag: WindowHintTag = hint;
    switch (hint) {
        .focused => |x| glfwWindowHint(tag, @intFromBool(x)),
        .iconified => |x| glfwWindowHint(tag, @intFromBool(x)),
        .resizable => |x| glfwWindowHint(tag, @intFromBool(x)),
        .visible => |x| glfwWindowHint(tag, @intFromBool(x)),
        .decorated => |x| glfwWindowHint(tag, @intFromBool(x)),
        .auto_iconify => |x| glfwWindowHint(tag, @intFromBool(x)),
        .floating => |x| glfwWindowHint(tag, @intFromBool(x)),
        .maximized => |x| glfwWindowHint(tag, @intFromBool(x)),
        .center_cursor => |x| glfwWindowHint(tag, @intFromBool(x)),
        .transparent_framebuffer => |x| glfwWindowHint(tag, @intFromBool(x)),
        .hovered => |x| glfwWindowHint(tag, @intFromBool(x)),
        .focus_on_show => |x| glfwWindowHint(tag, @intFromBool(x)),
        .mouse_passthrough => |x| glfwWindowHint(tag, @intFromBool(x)),
        .position_x => |x| glfwWindowHint(tag, x),
        .position_y => |x| glfwWindowHint(tag, x),
        .red_bits => |x| glfwWindowHint(tag, x),
        .green_bits => |x| glfwWindowHint(tag, x),
        .blue_bits => |x| glfwWindowHint(tag, x),
        .alpha_bits => |x| glfwWindowHint(tag, x),
        .depth_bits => |x| glfwWindowHint(tag, x),
        .stencil_bits => |x| glfwWindowHint(tag, x),
        .accum_red_bits => |x| glfwWindowHint(tag, x),
        .accum_green_bits => |x| glfwWindowHint(tag, x),
        .accum_blue_bits => |x| glfwWindowHint(tag, x),
        .accum_alpha_bits => |x| glfwWindowHint(tag, x),
        .aux_buffers => |x| glfwWindowHint(tag, x),
        .stereo => |x| glfwWindowHint(tag, @intFromBool(x)),
        .samples => |x| glfwWindowHint(tag, x),
        .srgb_capable => |x| glfwWindowHint(tag, @intFromBool(x)),
        .refresh_rate => |x| glfwWindowHint(tag, x),
        .doublebuffer => |x| glfwWindowHint(tag, @intFromBool(x)),
        .client_api => |x| glfwWindowHint(tag, @intFromEnum(x)),
        .context_version_major => |x| glfwWindowHint(tag, x),
        .context_version_minor => |x| glfwWindowHint(tag, x),
        .context_revision => |x| glfwWindowHint(tag, x),
        .context_robustness => |x| glfwWindowHint(tag, @intFromEnum(x)),
        .opengl_forward_compat => |x| glfwWindowHint(tag, @intFromBool(x)),
        .context_debug => |x| glfwWindowHint(tag, @intFromBool(x)),
        .opengl_profile => |x| glfwWindowHint(tag, @intFromEnum(x)),
        .context_release_behavior => |x| glfwWindowHint(tag, @intFromEnum(x)),
        .context_no_error => |x| glfwWindowHint(tag, @intFromBool(x)),
        .context_creation_api => |x| glfwWindowHint(tag, @intFromEnum(x)),
        .scale_to_monitor => |x| glfwWindowHint(tag, @intFromBool(x)),
        .scale_framebuffer => |x| glfwWindowHint(tag, @intFromBool(x)),
        .cocoa_retina_framebuffer => |x| glfwWindowHint(tag, @intFromBool(x)),
        .cocoa_frame_name => |x| glfwWindowHintString(tag, x),
        .cocoa_graphics_switching => |x| glfwWindowHint(tag, @intFromBool(x)),
        .x11_class_name => |x| glfwWindowHintString(tag, x),
        .x11_instance_name => |x| glfwWindowHintString(tag, x),
        .win32_keyboard_menu => |x| glfwWindowHint(tag, @intFromBool(x)),
        .win32_showdefault => |x| glfwWindowHint(tag, @intFromBool(x)),
        .wayland_app_id => |x| glfwWindowHintString(tag, x),
    }
}

// pub const createWindow = @extern(*const fn (width: i32, height: i32, title: [*:0]const u8, monitor: ?*Monitor, share: ?*Window) callconv(.c) ?*Window, .{ .name = "glfwCreateWindow" });
extern fn glfwCreateWindow(width: i32, height: i32, title: [*:0]const u8, monitor: ?*Monitor, share: ?*Window) callconv(.c) ?*Window;
pub fn createWindow(width: i32, height: i32, title: [*:0]const u8, monitor: ?*Monitor, share: ?*Window) !*Window {
    const window = glfwCreateWindow(width, height, title, monitor, share);
    return window orelse error.FailedToCreateWindow;
}

pub const destroyWindow = @extern(*const fn (window: *Window) callconv(.c) void, .{ .name = "glfwDestroyWindow" });

extern fn glfwWindowShouldClose(window: *Window) callconv(.c) i32;
pub fn windowShouldClose(window: *Window) bool {
    return glfwWindowShouldClose(window) == 1;
}

extern fn glfwSetWindowShouldClose(window: *Window, value: i32) callconv(.c) void;
pub fn setWindowShouldClose(window: *Window, value: bool) void {
    glfwSetWindowShouldClose(window, if (value) 1 else 0);
}

pub const getWindowTitle = @extern(*const fn (window: *Window) callconv(.c) ?[*:0]const u8, .{ .name = "glfwGetWindowTitle" });
pub const setWindowTitle = @extern(*const fn (window: *Window, title: [*:0]const u8) callconv(.c) void, .{ .name = "glfwSetWindowTitle" });
pub const setWindowIcon = @extern(*const fn (window: *Window, count: i32, images: ?[*]const Image) callconv(.c) void, .{ .name = "glfwSetWindowIcon" });
pub const getWindowPos = @extern(*const fn (window: *Window, xpos: *i32, ypos: *i32) callconv(.c) void, .{ .name = "glfwGetWindowPos" });
pub const setWindowPos = @extern(*const fn (window: *Window, xpos: i32, ypos: i32) callconv(.c) void, .{ .name = "glfwSetWindowPos" });
pub const getWindowSize = @extern(*const fn (window: *Window, width: *i32, height: *i32) callconv(.c) void, .{ .name = "glfwGetWindowSize" });
pub const setWindowSizeLimits = @extern(*const fn (window: *Window, minwidth: i32, minheight: i32, maxwidth: i32, maxheight: i32) callconv(.c) void, .{ .name = "glfwSetWindowSizeLimits" });
pub const setWindowAspectRatio = @extern(*const fn (window: *Window, numer: i32, denom: i32) callconv(.c) void, .{ .name = "glfwSetWindowAspectRatio" });
pub const setWindowSize = @extern(*const fn (window: *Window, width: i32, height: i32) callconv(.c) void, .{ .name = "glfwSetWindowSize" });
pub const getFramebufferSize = @extern(*const fn (window: *Window, width: *i32, height: *i32) callconv(.c) void, .{ .name = "glfwGetFramebufferSize" });
pub const getWindowFrameSize = @extern(*const fn (window: *Window, left: *i32, top: *i32, right: *i32, bottom: *i32) callconv(.c) void, .{ .name = "glfwGetWindowFrameSize" });
pub const getWindowContentScale = @extern(*const fn (window: *Window, xscale: *f32, yscale: *f32) callconv(.c) void, .{ .name = "glfwGetWindowContentScale" });
pub const getWindowOpacity = @extern(*const fn (window: *Window) callconv(.c) f32, .{ .name = "glfwGetWindowOpacity" });
pub const setWindowOpacity = @extern(*const fn (window: *Window, opacity: f32) callconv(.c) void, .{ .name = "glfwSetWindowOpacity" });
pub const iconifyWindow = @extern(*const fn (window: *Window) callconv(.c) void, .{ .name = "glfwIconifyWindow" });
pub const restoreWindow = @extern(*const fn (window: *Window) callconv(.c) void, .{ .name = "glfwRestoreWindow" });
pub const maximizeWindow = @extern(*const fn (window: *Window) callconv(.c) void, .{ .name = "glfwMaximizeWindow" });
pub const showWindow = @extern(*const fn (window: *Window) callconv(.c) void, .{ .name = "glfwShowWindow" });
pub const hideWindow = @extern(*const fn (window: *Window) callconv(.c) void, .{ .name = "glfwHideWindow" });
pub const focusWindow = @extern(*const fn (window: *Window) callconv(.c) void, .{ .name = "glfwFocusWindow" });
pub const requestWindowAttention = @extern(*const fn (window: *Window) callconv(.c) void, .{ .name = "glfwRequestWindowAttention" });
pub const getWindowMonitor = @extern(*const fn (window: *Window) callconv(.c) ?*Monitor, .{ .name = "glfwGetWindowMonitor" });
pub const setWindowMonitor = @extern(*const fn (window: *Window, monitor: ?*Monitor, xpos: i32, ypos: i32, width: i32, height: i32, refreshRate: i32) callconv(.c) void, .{ .name = "glfwSetWindowMonitor" });
pub const getWindowAttrib = @extern(*const fn (window: *Window, attrib: i32) callconv(.c) i32, .{ .name = "glfwGetWindowAttrib" });
pub const setWindowAttrib = @extern(*const fn (window: *Window, attrib: i32, value: i32) callconv(.c) void, .{ .name = "glfwSetWindowAttrib" });
pub const setWindowUserPointer = @extern(*const fn (window: *Window, pointer: ?*anyopaque) callconv(.c) void, .{ .name = "glfwSetWindowUserPointer" });
pub const getWindowUserPointer = @extern(*const fn (window: *Window) callconv(.c) ?*anyopaque, .{ .name = "glfwGetWindowUserPointer" });
pub const setWindowPosCallback = @extern(*const fn (window: *Window, callback: WindowPosCallback) callconv(.c) WindowPosCallback, .{ .name = "glfwSetWindowPosCallback" });
pub const setWindowSizeCallback = @extern(*const fn (window: *Window, callback: WindowSizeCallback) callconv(.c) WindowSizeCallback, .{ .name = "glfwSetWindowSizeCallback" });
pub const setWindowCloseCallback = @extern(*const fn (window: *Window, callback: WindowCloseCallback) callconv(.c) WindowCloseCallback, .{ .name = "glfwSetWindowCloseCallback" });
pub const setWindowRefreshCallback = @extern(*const fn (window: *Window, callback: WindowRefreshCallback) callconv(.c) WindowRefreshCallback, .{ .name = "glfwSetWindowRefreshCallback" });
pub const setWindowFocusCallback = @extern(*const fn (window: *Window, callback: WindowFocusCallback) callconv(.c) WindowFocusCallback, .{ .name = "glfwSetWindowFocusCallback" });
pub const setWindowIconifyCallback = @extern(*const fn (window: *Window, callback: WindowIconifyCallback) callconv(.c) WindowIconifyCallback, .{ .name = "glfwSetWindowIconifyCallback" });
pub const setWindowMaximizeCallback = @extern(*const fn (window: *Window, callback: WindowMaximizeCallback) callconv(.c) WindowMaximizeCallback, .{ .name = "glfwSetWindowMaximizeCallback" });
pub const setFramebufferSizeCallback = @extern(*const fn (window: *Window, callback: FramebufferSizeCallback) callconv(.c) FramebufferSizeCallback, .{ .name = "glfwSetFramebufferSizeCallback" });
pub const setWindowContentScaleCallback = @extern(*const fn (window: *Window, callback: WindowContentScaleCallback) callconv(.c) WindowContentScaleCallback, .{ .name = "glfwSetWindowContentScaleCallback" });
pub const pollEvents = @extern(*const fn () callconv(.c) void, .{ .name = "glfwPollEvents" });
pub const waitEvents = @extern(*const fn () callconv(.c) void, .{ .name = "glfwWaitEvents" });
pub const waitEventsTimeout = @extern(*const fn (timeout: f64) callconv(.c) void, .{ .name = "glfwWaitEventsTimeout" });
pub const postEmptyEvent = @extern(*const fn () callconv(.c) void, .{ .name = "glfwPostEmptyEvent" });

extern fn glfwGetInputMode(window: *Window, mode: InputModeKind) callconv(.c) i32;
pub fn getInputMode(window: *Window, mode_kind: InputModeKind) InputMode {
    const raw = glfwGetInputMode(window, mode_kind);
    return switch (mode_kind) {
        .cursor => InputMode{ .cursor = @enumFromInt(raw) },
        .sticky_keys => InputMode{ .sticky_keys = raw != 0 },
        .sticky_mouse_buttons => InputMode{ .sticky_mouse_buttons = raw != 0 },
        .lock_key_mods => InputMode{ .lock_key_mods = raw != 0 },
        .raw_mouse_motion => InputMode{ .raw_mouse_motion = raw != 0 },
    };
}

extern fn glfwSetInputMode(window: *Window, mode: InputModeKind, value: i32) callconv(.c) void;
pub fn setInputMode(window: *Window, mode: InputMode) void {
    const kind: InputModeKind = mode;

    switch (mode) {
        .cursor => |c| glfwSetInputMode(window, kind, @intFromEnum(c)),
        .sticky_keys => |v| glfwSetInputMode(window, kind, @intFromBool(v)),
        .sticky_mouse_buttons => |v| glfwSetInputMode(window, kind, @intFromBool(v)),
        .lock_key_mods => |v| glfwSetInputMode(window, kind, @intFromBool(v)),
        .raw_mouse_motion => |v| glfwSetInputMode(window, kind, @intFromBool(v)),
    }
}

extern fn glfwRawMouseMotionSupported() callconv(.c) i32;
pub fn rawMouseMotionSupported() bool {
    return glfwRawMouseMotionSupported() == 1;
}

pub const getKeyName = @extern(*const fn (key: i32, scancode: i32) callconv(.c) ?[*:0]const u8, .{ .name = "glfwGetKeyName" });
pub const getKeyScancode = @extern(*const fn (key: i32) callconv(.c) i32, .{ .name = "glfwGetKeyScancode" });
pub const getKey = @extern(*const fn (window: *Window, key: i32) callconv(.c) KeyState32, .{ .name = "glfwGetKey" });

pub const getMouseButton = @extern(*const fn (window: *Window, button: i32) callconv(.c) KeyState32, .{ .name = "glfwGetMouseButton" });

pub const getCursorPos = @extern(*const fn (window: *Window, xpos: *f64, ypos: *f64) callconv(.c) void, .{ .name = "glfwGetCursorPos" });
pub const setCursorPos = @extern(*const fn (window: *Window, xpos: f64, ypos: f64) callconv(.c) void, .{ .name = "glfwSetCursorPos" });
pub const createCursor = @extern(*const fn (Image: *const Image, xhot: i32, yhot: i32) callconv(.c) ?*Cursor, .{ .name = "glfwCreateCursor" });
pub const createStandardCursor = @extern(*const fn (shape: i32) callconv(.c) ?*Cursor, .{ .name = "glfwCreateStandardCursor" });
pub const destroyCursor = @extern(*const fn (cursor: ?*Cursor) callconv(.c) void, .{ .name = "glfwDestroyCursor" });
pub const setCursor = @extern(*const fn (window: *Window, cursor: ?*Cursor) callconv(.c) void, .{ .name = "glfwSetCursor" });

pub const setKeyCallback = @extern(*const fn (window: *Window, callback: KeyCallback) callconv(.c) KeyCallback, .{ .name = "glfwSetKeyCallback" });

pub const setCharCallback = @extern(*const fn (window: *Window, callback: CharCallback) callconv(.c) CharCallback, .{ .name = "glfwSetCharCallback" });
pub const setCharModsCallback = @extern(*const fn (window: *Window, callback: CharModsCallback) callconv(.c) CharModsCallback, .{ .name = "glfwSetCharModsCallback" });
pub const setMouseButtonCallback = @extern(*const fn (window: *Window, callback: MouseButtonCallback) callconv(.c) MouseButtonCallback, .{ .name = "glfwSetMouseButtonCallback" });
pub const setCursorPosCallback = @extern(*const fn (window: *Window, callback: CursorPosCallback) callconv(.c) CursorPosCallback, .{ .name = "glfwSetCursorPosCallback" });
pub const setCursorEnterCallback = @extern(*const fn (window: *Window, callback: CursorEnterCallback) callconv(.c) CursorEnterCallback, .{ .name = "glfwSetCursorEnterCallback" });
pub const setScrollCallback = @extern(*const fn (window: *Window, callback: ScrollCallback) callconv(.c) ScrollCallback, .{ .name = "glfwSetScrollCallback" });
pub const setDropCallback = @extern(*const fn (window: *Window, callback: DropCallback) callconv(.c) DropCallback, .{ .name = "glfwSetDropCallback" });

extern fn glfwJoystickPresent(jid: i32) callconv(.c) i32;
pub fn joystickPresent(jid: i32) bool {
    return glfwJoystickPresent(jid) == 1;
}

pub const getJoystickAxes = @extern(*const fn (jid: i32, count: *i32) callconv(.c) ?[*]const f32, .{ .name = "glfwGetJoystickAxes" });
pub const getJoystickButtons = @extern(*const fn (jid: i32, count: *i32) callconv(.c) ?[*]const KeyState8, .{ .name = "glfwGetJoystickButtons" });
pub const getJoystickHats = @extern(*const fn (jid: i32, count: *i32) callconv(.c) ?[*]const KeyState8, .{ .name = "glfwGetJoystickHats" });
pub const getJoystickName = @extern(*const fn (jid: i32) callconv(.c) ?[*:0]const u8, .{ .name = "glfwGetJoystickName" });
pub const getJoystickGUID = @extern(*const fn (jid: i32) callconv(.c) ?[*:0]const u8, .{ .name = "glfwGetJoystickGUID" });
pub const setJoystickUserPointer = @extern(*const fn (jid: i32, pointer: ?*anyopaque) callconv(.c) void, .{ .name = "glfwSetJoystickUserPointer" });
pub const getJoystickUserPointer = @extern(*const fn (jid: i32) callconv(.c) ?*anyopaque, .{ .name = "glfwGetJoystickUserPointer" });

extern fn glfwJoystickIsGamepad(jid: i32) callconv(.c) i32;
pub fn joystickIsGamepad(jid: i32) bool {
    return glfwJoystickIsGamepad(jid) == 1;
}

pub const setJoystickCallback = @extern(*const fn (callback: JoystickCallback) callconv(.c) JoystickCallback, .{ .name = "glfwSetJoystickCallback" });

extern fn glfwUpdateGamepadMappings(string: [*c]const u8) callconv(.c) i32;
pub fn updateGamepadMappings(string: [*c]const u8) bool {
    return glfwUpdateGamepadMappings(string) == 1;
}

pub const getGamepadName = @extern(*const fn (jid: i32) callconv(.c) ?[*:0]const u8, .{ .name = "glfwGetGamepadName" });

extern fn glfwGetGamepadState(jid: i32, state: *GamepadState) callconv(.c) i32;
pub fn getGamepadState(jid: i32, state: *GamepadState) bool {
    return glfwGetGamepadState(jid, state) == 1;
}

pub const setClipboardString = @extern(*const fn (window: *Window, string: [*c]const u8) callconv(.c) void, .{ .name = "glfwSetClipboardString" });
pub const getClipboardString = @extern(*const fn (window: *Window) callconv(.c) ?[*:0]const u8, .{ .name = "glfwGetClipboardString" });
pub const getTime = @extern(*const fn () callconv(.c) f64, .{ .name = "glfwGetTime" });
pub const setTime = @extern(*const fn (time: f64) callconv(.c) void, .{ .name = "glfwSetTime" });
pub const getTimerValue = @extern(*const fn () callconv(.c) u64, .{ .name = "glfwGetTimerValue" });
pub const getTimerFrequency = @extern(*const fn () callconv(.c) u64, .{ .name = "glfwGetTimerFrequency" });
pub const makeContextCurrent = @extern(*const fn (window: *Window) callconv(.c) void, .{ .name = "glfwMakeContextCurrent" });
pub const getCurrentContext = @extern(*const fn () callconv(.c) ?*Window, .{ .name = "glfwGetCurrentContext" });
pub const swapBuffers = @extern(*const fn (window: *Window) callconv(.c) void, .{ .name = "glfwSwapBuffers" });
pub const swapInterval = @extern(*const fn (interval: i32) callconv(.c) void, .{ .name = "glfwSwapInterval" });

extern fn glfwExtensionSupported(extension: [*:0]const u8) callconv(.c) i32;
pub fn extensionSupported(extension: [*:0]const u8) bool {
    return glfwExtensionSupported(extension) == 1;
}

pub const getProcAddress = @extern(*const fn (procname: [*:0]const u8) callconv(.c) Proc, .{ .name = "glfwGetProcAddress" });

extern fn glfwVulkanSupported() callconv(.c) i32;
pub fn vulkanSupported() bool {
    return glfwVulkanSupported() == 1;
}

pub const getRequiredInstanceExtensions = @extern(*const fn (count: *u32) callconv(.c) ?[*][*:0]const u8, .{ .name = "glfwGetRequiredInstanceExtensions" });
pub const getX11Display = @extern(*const fn () callconv(.c) ?*anyopaque, .{ .name = "glfwGetX11Display" });
pub const getX11Adapter = @extern(*const fn (monitor: *Monitor) callconv(.c) u64, .{ .name = "glfwGetX11Adapter" });
pub const getX11Monitor = @extern(*const fn (monitor: *Monitor) callconv(.c) u64, .{ .name = "glfwGetX11Monitor" });
pub const getX11Window = @extern(*const fn (window: *Window) callconv(.c) u64, .{ .name = "glfwGetX11Window" });
pub const setX11SelectionString = @extern(*const fn (string: ?[*:0]const u8) callconv(.c) void, .{ .name = "glfwSetX11SelectionString" });
pub const getX11SelectionString = @extern(*const fn () callconv(.c) ?[*:0]const u8, .{ .name = "glfwGetX11SelectionString" });

pub const version = std.SemanticVersion{ .major = 3, .minor = 4, .patch = 0 };

// TODO: no way to make these the same type without forcing all event handlers to comptime so that we can wrap and convert
pub const KeyState8 = enum(u8) {
    release = 0,
    press = 1,
    repeat = 2,
};

pub const KeyState32 = enum(i32) {
    release = 0,
    press = 1,
    repeat = 2,
};

pub fn KeyStateInverse(comptime T: type) type {
    return switch (T) {
        KeyState8 => KeyState32,
        KeyState32 => KeyState8,
        else => @compileError("Invalid type for KeyStateInverse"),
    };
}

pub fn castKeyState(value: anytype) KeyStateInverse(@TypeOf(value)) {
    return @enumFromInt(@intFromEnum(value));
}

pub const InputModeKind = enum(i32) {
    cursor = 0x00033001,
    sticky_keys = 0x00033002,
    sticky_mouse_buttons = 0x00033003,
    lock_key_mods = 0x00033004,
    raw_mouse_motion = 0x00033005,
};

pub const CursorMode = enum(i32) {
    normal = 0x00034001,
    hidden = 0x00034002,
    disabled = 0x00034003,
    captured = 0x00034004,
};

pub const InputMode = union(InputModeKind) {
    cursor: CursorMode,
    sticky_keys: bool,
    sticky_mouse_buttons: bool,
    lock_key_mods: bool,
    raw_mouse_motion: bool,
};

pub const Hat = enum(i32) {
    centered = 0,
    up = 1,
    right = 2,
    down = 4,
    left = 8,
    right_up = 3,
    right_down = 6,
    left_up = 9,
    left_down = 12,
};

pub const Key = enum(i32) {
    unknown = -1,
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
    KEY_MEN,
};

pub const Modifier = packed struct(i32) {
    shift: bool = false,
    control: bool = false,
    alt: bool = false,
    super: bool = false,
    caps_lock: bool = false,
    num_lock: bool = false,
    _padding: u26 = 0, // pad to 32 bits

    pub const none = Modifier{};

    pub const shiftMod = Modifier{ .shift = true };
    pub const controlMod = Modifier{ .control = true };
    pub const altMod = Modifier{ .alt = true };
    pub const superMod = Modifier{ .super = true };
    pub const capsLockMod = Modifier{ .caps_lock = true };
    pub const numLockMod = Modifier{ .num_lock = true };

    pub fn merge(self: Modifier, other: Modifier) Modifier {
        return Modifier{
            .shift = self.shift or other.shift,
            .control = self.control or other.control,
            .alt = self.alt or other.alt,
            .super = self.super or other.super,
            .caps_lock = self.caps_lock or other.caps_lock,
            .num_lock = self.num_lock or other.num_lock,
        };
    }
};

pub const MouseButton = enum(i32) {
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
};

pub const Joystick = enum(i32) {
    joystick_1 = 0,
    joystick_2 = 1,
    joystick_3 = 2,
    joystick_4 = 3,
    joystick_5 = 4,
    joystick_6 = 5,
    joystick_7 = 6,
    joystick_8 = 7,
    joystick_9 = 8,
    joystick_10 = 9,
    joystick_11 = 10,
    joystick_12 = 11,
    joystick_13 = 12,
    joystick_14 = 13,
    joystick_15 = 14,
    joystick_16 = 15,
};

pub const GamepadButton = enum(i32) {
    a = 0,
    b = 1,
    x = 2,
    y = 3,
    left_bumper = 4,
    right_bumper = 5,
    back = 6,
    start = 7,
    guide = 8,
    left_thumb = 9,
    right_thumb = 10,
    dpad_up = 11,
    dpad_right = 12,
    dpad_down = 13,
    dpad_left = 14,

    pub const cross = GamepadButton.a;
    pub const circle = GamepadButton.b;
    pub const square = GamepadButton.x;
    pub const triangle = GamepadButton.y;
};

pub const GamepadAxis = enum(i32) {
    left_x = 0,
    left_y = 1,
    right_x = 2,
    right_y = 3,
    left_trigger = 4,
    right_trigger = 5,
};

pub const CursorEvent = enum(i32) {
    enter = 1,
    leave = 0,
};

pub const FocusEvent = enum(i32) {
    focused = 1,
    unfocus = 0,
};

pub const MinimizeEvent = enum(i32) {
    minimized = 1,
    restored = 0,
};

pub const MaximizeEvent = enum(i32) {
    maximized = 1,
    restored = 0,
};

pub const ConnectionEvent = enum(i32) {
    disconnected = 0x00040001,
    connected = 0x00040002,
};

pub const ErrorCode = enum(i32) {
    no_error = 0,
    not_initialized = 0x00010001,
    no_current_context = 0x00010002,
    invalid_enum = 0x00010003,
    invalid_value = 0x00010004,
    out_of_memory = 0x00010005,
    api_unavailable = 0x00010006,
    version_unavailable = 0x00010007,
    platform_error = 0x00010008,
    format_unavailable = 0x00010009,
    no_window_context = 0x0001000A,
    cursor_unavailable = 0x0001000B,
    feature_unavailable = 0x0001000C,
    feature_unimplemented = 0x0001000D,
    platform_unavailable = 0x0001000E,
};

pub const PlatformId = enum(i32) {
    any = 0x00060000,
    win32 = 0x00060001,
    cocoa = 0x00060002,
    wayland = 0x00060003,
    x11 = 0x00060004,
    null = 0x00060005,
};

pub const WaylandLibDecorPreference = enum(i32) {
    prefer = 0x00038001,
    disable = 0x00038002,
};

pub const WindowHintTag = enum(i32) {
    focused = 0x00020001,
    iconified = 0x00020002,
    resizable = 0x00020003,
    visible = 0x00020004,
    decorated = 0x00020005,
    auto_iconify = 0x00020006,
    floating = 0x00020007,
    maximized = 0x00020008,
    center_cursor = 0x00020009,
    transparent_framebuffer = 0x0002000A,
    hovered = 0x0002000B,
    focus_on_show = 0x0002000C,
    mouse_passthrough = 0x0002000D,
    position_x = 0x0002000E,
    position_y = 0x0002000F,
    red_bits = 0x00021001,
    green_bits = 0x00021002,
    blue_bits = 0x00021003,
    alpha_bits = 0x00021004,
    depth_bits = 0x00021005,
    stencil_bits = 0x00021006,
    accum_red_bits = 0x00021007,
    accum_green_bits = 0x00021008,
    accum_blue_bits = 0x00021009,
    accum_alpha_bits = 0x0002100A,
    aux_buffers = 0x0002100B,
    stereo = 0x0002100C,
    samples = 0x0002100D,
    srgb_capable = 0x0002100E,
    refresh_rate = 0x0002100F,
    doublebuffer = 0x00021010,
    client_api = 0x00022001,
    context_version_major = 0x00022002,
    context_version_minor = 0x00022003,
    context_revision = 0x00022004,
    context_robustness = 0x00022005,
    opengl_forward_compat = 0x00022006,
    context_debug = 0x00022007,
    opengl_profile = 0x00022008,
    context_release_behavior = 0x00022009,
    context_no_error = 0x0002200A,
    context_creation_api = 0x0002200B,
    scale_to_monitor = 0x0002200C,
    scale_framebuffer = 0x0002200D,
    cocoa_retina_framebuffer = 0x00023001,
    cocoa_frame_name = 0x00023002,
    cocoa_graphics_switching = 0x00023003,
    x11_class_name = 0x00024001,
    x11_instance_name = 0x00024002,
    win32_keyboard_menu = 0x00025001,
    win32_showdefault = 0x00025002,
    wayland_app_id = 0x00026001,
};

pub const ClientApi = enum(i32) {
    none = 0,
    opengl = 0x00030001,
    opengl_es = 0x00030002,
};

pub const ContextRobustness = enum(i32) {
    none = 0,
    no_reset_notification = 0x00031001,
    lose_context_on_reset = 0x00031002,
};

pub const OpenGlProfile = enum(i32) {
    any = 0,
    core = 0x00032001,
    compat = 0x00032002,
};

pub const ContextReleaseBehavior = enum(i32) {
    any = 0,
    flush = 0x00035001,
    none = 0x00035002,
};

pub const ContextCreationApi = enum(i32) {
    native = 0x00036001,
    egl = 0x00036002,
    osmesa = 0x00036003,
};
pub const AnglePlatformType = enum(i32) {
    none = 0x00037001,
    opengl = 0x00037002,
    opengles = 0x00037003,
    d3d9 = 0x00037004,
    d3d11 = 0x00037005,
    vulkan = 0x00037007,
    metal = 0x00037008,
};
pub const WindowHint = union(WindowHintTag) {
    focused: bool,
    iconified: bool,
    resizable: bool,
    visible: bool,
    decorated: bool,
    auto_iconify: bool,
    floating: bool,
    maximized: bool,
    center_cursor: bool,
    transparent_framebuffer: bool,
    hovered: bool,
    focus_on_show: bool,
    mouse_passthrough: bool,
    position_x: i32,
    position_y: i32,
    red_bits: i32,
    green_bits: i32,
    blue_bits: i32,
    alpha_bits: i32,
    depth_bits: i32,
    stencil_bits: i32,
    accum_red_bits: i32,
    accum_green_bits: i32,
    accum_blue_bits: i32,
    accum_alpha_bits: i32,
    aux_buffers: i32,
    stereo: bool,
    samples: i32,
    srgb_capable: bool,
    refresh_rate: i32,
    doublebuffer: bool,
    client_api: ClientApi,
    context_version_major: i32,
    context_version_minor: i32,
    context_revision: i32,
    context_robustness: ContextRobustness,
    opengl_forward_compat: bool,
    context_debug: bool,
    opengl_profile: OpenGlProfile,
    context_release_behavior: ContextReleaseBehavior,
    context_no_error: bool,
    context_creation_api: ContextCreationApi,
    scale_to_monitor: bool,
    scale_framebuffer: bool,
    cocoa_retina_framebuffer: bool,
    cocoa_frame_name: ?[*:0]const u8,
    cocoa_graphics_switching: bool,
    x11_class_name: ?[*:0]const u8,
    x11_instance_name: ?[*:0]const u8,
    win32_keyboard_menu: bool,
    win32_showdefault: bool,
    wayland_app_id: ?[*:0]const u8,
};

pub const InitHintTag = enum(i32) {
    platform = 0x00050003,
    angle_platform_type = 0x00050002,
    joystick_hat_buttons = 0x00050001,
    cocoa_chdir_resources = 0x00051001,
    cocoa_menubar = 0x00051002,
    wayland_libdecor = 0x00053001,
    x11_xcb_vulkan_surface = 0x00052001,
};

pub const InitHint = union(InitHintTag) {
    platform: PlatformId,
    angle_platform_type: AnglePlatformType,
    joystick_hat_buttons: bool,
    cocoa_chdir_resources: bool,
    cocoa_menubar: bool,
    wayland_libdecor: WaylandLibDecorPreference,
    x11_xcb_vulkan_surface: bool,
};

pub const dont_care = -1;
pub const any_position = 0x80000000;

pub const StandardCursor = enum(i32) {
    arrow = 0x00036001,
    ibeam = 0x00036002,
    crosshair = 0x00036003,
    pointing_hand = 0x00036004,
    resize_ew = 0x00036005,
    resize_ns = 0x00036006,
    resize_nwse = 0x00036007,
    resize_nesw = 0x00036008,
    resize_all = 0x00036009,
    not_allowed = 0x0003600A,

    pub const hresize = StandardCursor.resize_ew;
    pub const vresize = StandardCursor.resize_ns;
    pub const hand = StandardCursor.pointing_hand;
};
