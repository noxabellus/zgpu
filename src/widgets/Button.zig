pub const Button = @This();

const Ui = @import("../Ui.zig");

pub fn button(ui: *Ui, id: Ui.ElementId, text: []const u8) !bool {
    try ui.beginElement(id, .{
        .sizing = .fit,
        .child_alignment = .center,
        .type = .layout_widget,
        .event_flags = .{ .click = true, .focus = true, .activate = true },
    });
    defer ui.endElement();

    try ui.text(text, .{});

    if (ui.getEvent(id, .activate_end)) |_| {
        return true;
    }

    return false;
}
