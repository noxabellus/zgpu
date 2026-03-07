//! Custom shader widget.

const ShaderRect = @This();

const std = @import("std");

const Ui = @import("../Ui.zig");
const Gpu = @import("../Gpu.zig");
const Batch2D = @import("../Batch2D.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

const log = std.log.scoped(.shader_rect);

test {
    log.debug("semantic analysis for widgets/ShaderRect.zig", .{});
    std.testing.refAllDecls(@This());
}

shader_id: u32,
identity: PipelineIdentity,
state: State,

pub const State = struct {
    count: usize,
    textures: [Batch2D.MAX_CUSTOM_TEXTURES]?*Gpu.TextureView,
    uniform_size: usize,
    uniform_data: [1024]u8,

    pub fn toSlices(self: *const State) struct { []const *Gpu.TextureView, []const u8 } {
        return .{ @ptrCast(self.textures[0..self.count]), self.uniform_data[0..self.uniform_size] };
    }

    pub fn fromSlices(textures: []const *Gpu.TextureView, uniform_data: []const u8) State {
        var s = State{
            .count = textures.len,
            .textures = [1]?*Gpu.TextureView{null} ** Batch2D.MAX_CUSTOM_TEXTURES,
            .uniform_size = uniform_data.len,
            .uniform_data = [1]u8{0} ** 1024,
        };
        for (textures, 0..) |tex, i| {
            s.textures[i] = tex;
        }
        @memcpy(s.uniform_data[0..uniform_data.len], uniform_data);
        return s;
    }
};

pub const TexBindingIdentity = packed struct(u8) {
    sample_type: u3 = 0,
    view_dimension: u3 = 0,
    multisampled: bool = false,
    _unused: u1 = 0,

    pub fn fromLayout(layout: Gpu.TextureBindingLayout) TexBindingIdentity {
        return TexBindingIdentity{
            .sample_type = @intCast(@intFromEnum(layout.sample_type)),
            .view_dimension = @intCast(@intFromEnum(layout.view_dimension)),
            .multisampled = layout.multisampled.to(),
        };
    }
};

pub const PipelineIdentity = struct {
    shader_ptr: *Gpu.ShaderModule,
    texture_bindings: [Batch2D.MAX_CUSTOM_TEXTURES]TexBindingIdentity,
    uniform_size: usize,

    pub fn init(shader_ptr: *Gpu.ShaderModule, texture_bindings: []const Gpu.TextureBindingLayout, uniforms: []const u8) PipelineIdentity {
        var id = PipelineIdentity{
            .shader_ptr = shader_ptr,
            .texture_bindings = [1]TexBindingIdentity{.{}} ** Batch2D.MAX_CUSTOM_TEXTURES,
            .uniform_size = uniforms.len,
        };

        for (texture_bindings, 0..) |binding, i| {
            id.texture_bindings[i] = TexBindingIdentity.fromLayout(binding);
        }

        return id;
    }

    pub fn eql(a: *const PipelineIdentity, b: *const PipelineIdentity) bool {
        if (a.shader_ptr != b.shader_ptr) return false;
        if (a.uniform_size != b.uniform_size) return false;
        for (a.texture_bindings, 0..) |binding, i| {
            if (binding != b.texture_bindings[i]) return false;
        }
        return true;
    }
};

pub const Config = struct {
    shader: *Gpu.ShaderModule,
    texture_bindings: []const Gpu.TextureBindingLayout,
    textures: []const *Gpu.TextureView = &.{},
    uniforms: []const u8 = &.{},
    sizing: Ui.Sizing = .grow,
    aspect_ratio: f32 = 0,
};

pub fn render(self: *ShaderRect, ui: *Ui, command: Ui.RenderCommand) !void {
    const bb = command.bounding_box;
    const rad = command.render_data.rectangle.corner_radius;

    const textures, const custom_uniforms = self.state.toSlices();

    try ui.renderer.customPipelineStart(self.shader_id, textures, custom_uniforms);
    try ui.renderer.drawRoundedRect(
        .{ bb.x, bb.y },
        .{ bb.width, bb.height },
        .{
            .top_left = rad.top_left,
            .top_right = rad.top_right,
            .bottom_left = rad.bottom_left,
            .bottom_right = rad.bottom_right,
        },
        .{},
    );
    try ui.renderer.customPipelineEnd(self.shader_id);
}

pub const PipelineCache = struct {
    map: std.AutoHashMapUnmanaged(PipelineIdentity, u32) = .empty,

    pub fn deinit(self: *PipelineCache, ui: *Ui) void {
        self.map.deinit(ui.gpa);
    }
};

pub fn shaderRect(ui: *Ui, id: Ui.ElementId, config: ShaderRect.Config) !void {
    const self, _ = try ui.getOrCreateWidget(ShaderRect, id);
    const pipeline_cache, const new_cache = try ui.getOrCreateSharedWidgetState(PipelineCache, .fromSlice("ShaderRectPipelineCache"));
    if (new_cache) pipeline_cache.* = .{};

    if (config.texture_bindings.len > Batch2D.MAX_CUSTOM_TEXTURES) {
        return error.TooManyTextures;
    }

    const pipeline_identity = PipelineIdentity.init(config.shader, config.texture_bindings, config.uniforms);

    const gop = try pipeline_cache.map.getOrPut(ui.gpa, pipeline_identity);
    if (!gop.found_existing) {
        gop.value_ptr.* = try ui.renderer.createCustomPipeline(
            "Ui:ShaderRect:custom_shader",
            .quad,
            config.shader,
            config.texture_bindings,
            config.uniforms.len,
        );
    }

    self.shader_id = gop.value_ptr.*;
    self.identity = pipeline_identity;
    self.state = .fromSlices(config.textures, config.uniforms);

    try ui.beginElement(id, .{
        .type = .render_widget,
        .sizing = config.sizing,
        .aspect_ratio = config.aspect_ratio,
    });
    ui.endElement();
}
