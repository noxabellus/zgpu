//! Custom shader widget.

const ShaderRect = @This();

const std = @import("std");
const wgpu = @import("wgpu");

const Ui = @import("../Ui.zig");
const Batch2D = @import("../Batch2D.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;

const log = std.log.scoped(.shader_rect);

test {
    log.debug("semantic analysis for widgets/ShaderRect.zig", .{});
    std.testing.refAllDecls(@This());
}

id: Ui.ElementId,
shader_id: u32,
identity: PipelineIdentity,
state: State,

pub const State = struct {
    count: usize,
    textures: [Batch2D.MAX_CUSTOM_TEXTURES]wgpu.TextureView,

    pub fn toSlice(self: *const State) []const wgpu.TextureView {
        return self.textures[0..self.count];
    }

    pub fn fromSlice(textures: []const wgpu.TextureView) State {
        var s = State{
            .count = textures.len,
            .textures = [1]wgpu.TextureView{null} ** Batch2D.MAX_CUSTOM_TEXTURES,
        };
        for (textures, 0..) |tex, i| {
            s.textures[i] = tex;
        }
        return s;
    }
};

pub const TexBindingIdentity = packed struct(u8) {
    sample_type: u3 = 0,
    view_dimension: u3 = 0,
    multisampled: bool = false,
    _unused: u1 = 0,

    pub fn fromLayout(layout: wgpu.TextureBindingLayout) TexBindingIdentity {
        return TexBindingIdentity{
            .sample_type = @intCast(@intFromEnum(layout.sample_type)),
            .view_dimension = @intCast(@intFromEnum(layout.view_dimension)),
            .multisampled = layout.multisampled.to(),
        };
    }
};

pub const PipelineIdentity = struct {
    shader_ptr: wgpu.ShaderModule,
    texture_bindings: [Batch2D.MAX_CUSTOM_TEXTURES]TexBindingIdentity,

    pub fn init(shader_ptr: wgpu.ShaderModule, texture_bindings: []const wgpu.TextureBindingLayout) PipelineIdentity {
        var id = PipelineIdentity{
            .shader_ptr = shader_ptr,
            .texture_bindings = [1]TexBindingIdentity{.{}} ** Batch2D.MAX_CUSTOM_TEXTURES,
        };

        for (texture_bindings, 0..) |binding, i| {
            id.texture_bindings[i] = TexBindingIdentity.fromLayout(binding);
        }

        return id;
    }

    pub fn eql(a: *const PipelineIdentity, b: *const PipelineIdentity) bool {
        if (a.shader_ptr != b.shader_ptr) return false;
        for (a.texture_bindings, 0..) |binding, i| {
            if (binding != b.texture_bindings[i]) return false;
        }
        return true;
    }
};

pub const Config = struct {
    shader: wgpu.ShaderModule,
    texture_bindings: []const wgpu.TextureBindingLayout,
    textures: []const wgpu.TextureView = &.{},
};

var pipeline_cache: std.AutoHashMap(PipelineIdentity, u32) = std.AutoHashMap(PipelineIdentity, u32).init(std.heap.page_allocator);

pub fn init(ui: *Ui, id: Ui.ElementId, config: Config) !*ShaderRect {
    const self = try ui.gpa.create(ShaderRect);
    errdefer ui.gpa.destroy(self);

    if (config.texture_bindings.len > Batch2D.MAX_CUSTOM_TEXTURES) {
        return error.TooManyTextures;
    }

    const pipeline_identity = PipelineIdentity.init(config.shader, config.texture_bindings);

    const shader_id = if (pipeline_cache.get(pipeline_identity)) |sid| sid else create_new_pipeline: {
        const shader_id, _, _ = try ui.renderer.createCustomPipeline("Ui:ShaderRect:custom_shader", .quad, config.shader, config.texture_bindings, void);
        try pipeline_cache.put(pipeline_identity, shader_id);
        break :create_new_pipeline shader_id;
    };

    self.* = ShaderRect{
        .id = id,
        .shader_id = shader_id,
        .identity = pipeline_identity,
        .state = .fromSlice(config.textures),
    };

    return self;
}

pub fn deinit(self: *ShaderRect, ui: *Ui) void {
    ui.gpa.destroy(self);
}

pub fn bindEvents(self: *ShaderRect, ui: *Ui) !void {
    _ = .{ self, ui };
}

pub fn unbindEvents(self: *ShaderRect, ui: *Ui) void {
    _ = .{ self, ui };
}

pub fn onGet(self: *ShaderRect, _: *Ui) *const State {
    return &self.state;
}

pub fn onSet(self: *ShaderRect, _: *Ui, new_value: *const State) !void {
    self.state = new_value.*;
}

/// The rendering function for the checkbox, called by the UI system.
pub fn render(self: *ShaderRect, ui: *Ui, command: Ui.RenderCommand) !void {
    const bb = command.bounding_box;
    const rad = command.render_data.rectangle.corner_radius;

    try ui.renderer.customPipelineStart(self.shader_id, self.state.toSlice(), {});
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
