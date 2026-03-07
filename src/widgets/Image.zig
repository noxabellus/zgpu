pub const Image = @This();

const Ui = @import("../Ui.zig");
const Batch2D = @import("../Batch2D.zig");

pub const Config = struct {
    image_id: Batch2D.ImageId,
    sizing: Ui.Sizing = .grow,
    aspect_ratio: Ui.AspectRatioElementConfig = 0,
};

pub fn image(ui: *Ui, id: Ui.ElementId, config: Config) !void {
    try ui.beginElement(id, .{
        .image = config.image_id,
        .sizing = config.sizing,
        .aspect_ratio = config.aspect_ratio,
        .background_color = .{},
    });
    ui.endElement();
}
