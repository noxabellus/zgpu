pub const Button = @This();

const Ui = @import("../Ui.zig");

pub const Config = struct {
    disabled: bool = false,
};

pub fn button(ui: *Ui, id: Ui.ElementId, text: []const u8, config: Config) !bool {
    try ui.beginElement(id, .{
        .sizing = .fit,
        .child_alignment = .center,
        .type = .layout_widget,
        .state = if (config.disabled) .disabled else null,
        .event_flags = .{ .click = true, .focus = true, .activate = true },
    });
    defer ui.endElement();

    try ui.text(text, .{});

    if (ui.disabled()) return false;

    try Ui.widgets.menuNavigable(ui);

    if (ui.getEvent(id, .activate_end)) |_| {
        return true;
    }

    return false;
}
