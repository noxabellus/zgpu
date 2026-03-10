const Timer = @This();

const std = @import("std");

started: std.Io.Timestamp,
previous: std.Io.Timestamp,

pub fn start(io: std.Io) Timer {
    const current = std.Io.Clock.real.now(io);
    return Timer{ .started = current, .previous = current };
}

pub fn read(self: *Timer, io: std.Io) i96 {
    const current = self.sample(io);
    return self.started.durationTo(current).toNanoseconds();
}

pub fn reset(self: *Timer, io: std.Io) void {
    const current = self.sample(io);
    self.started = current;
}

pub fn lap(self: *Timer, io: std.Io) i96 {
    const current = self.sample(io);
    defer self.started = current;
    return self.started.durationTo(current).toNanoseconds();
}

fn sample(self: *Timer, io: std.Io) std.Io.Timestamp {
    const current = std.Io.Clock.real.now(io);
    if (current.nanoseconds > self.started.nanoseconds) {
        self.previous = current;
    }
    return self.previous;
}
