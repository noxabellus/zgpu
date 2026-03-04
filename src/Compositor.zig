const Compositor = @This();
const std = @import("std");
const Gpu = @import("Gpu.zig");
const RenderTexture = @import("RenderTexture.zig");

pipeline: *Gpu.RenderPipeline,
bind_group_layout: *Gpu.BindGroupLayout,
sampler: *Gpu.Sampler,

var BLIT_SHADER_MODULE: ?*Gpu.ShaderModule = null;
pub fn getBlitShader(gpu: *Gpu) !*Gpu.ShaderModule {
    if (BLIT_SHADER_MODULE == null) {
        BLIT_SHADER_MODULE = try gpu.device.loadShaderText("shaders/BlitCompositor.wgsl", @embedFile("shaders/BlitCompositor.wgsl"));
    }
    return BLIT_SHADER_MODULE.?;
}

pub fn init(gpu: *Gpu, custom_shader: ?*Gpu.ShaderModule) !Compositor {
    // Load the shader
    const blit_shader = try getBlitShader(gpu);
    const shader = if (custom_shader) |cs| cs else blit_shader;

    // Create the Sampler
    const sampler = try gpu.device.createSampler(&Gpu.SamplerDescriptor{
        .label = .fromSlice("compositor_sampler"),
        .address_mode_u = .clamp_to_edge,
        .address_mode_v = .clamp_to_edge,
        .address_mode_w = .clamp_to_edge,
        .mag_filter = .nearest,
        .min_filter = .nearest,
        .mipmap_filter = .nearest,
    });

    // Create Bind Group Layout (Texture @ 0, Sampler @ 1)
    const bgl_entries = [_]Gpu.BindGroupLayoutEntry{
        .{
            .binding = 0,
            .visibility = .{ .fragment = true },
            .texture = .{
                .sample_type = .float,
                .view_dimension = .@"2d",
                .multisampled = .False,
            },
        },
        .{
            .binding = 1,
            .visibility = .{ .fragment = true },
            .sampler = .{ .type = .filtering },
        },
    };

    const bind_group_layout = try gpu.device.createBindGroupLayout(&Gpu.BindGroupLayoutDescriptor{
        .label = .fromSlice("compositor_bgl"),
        .entry_count = bgl_entries.len,
        .entries = &bgl_entries,
    });

    // Create Pipeline Layout
    const pipeline_layout = try gpu.device.createPipelineLayout(&Gpu.PipelineLayoutDescriptor{
        .label = .fromSlice("compositor_pipeline_layout"),
        .bind_group_layout_count = 1,
        .bind_group_layouts = &.{bind_group_layout},
    });
    defer pipeline_layout.release();

    // Create Render Pipeline
    const pipeline = try gpu.device.createRenderPipeline(&Gpu.RenderPipelineDescriptor{
        .label = .fromSlice("compositor_pipeline"),
        .layout = pipeline_layout,
        .vertex = .{
            .module = blit_shader,
            .entry_point = .fromSlice("vs_main"),
            .buffer_count = 0,
            .buffers = null,
        },
        .fragment = &Gpu.FragmentState{
            .module = shader,
            .entry_point = .fromSlice("fs_main"),
            .target_count = 1,
            .targets = &[_]Gpu.ColorTargetState{
                .{
                    .format = gpu.surface_format, // MUST match the swapchain format!
                    .blend = &Gpu.BlendState{
                        .color = .{
                            .operation = .add,
                            .src_factor = .src_alpha,
                            .dst_factor = .one_minus_src_alpha,
                        },
                        .alpha = .{
                            .operation = .add,
                            .src_factor = .one,
                            .dst_factor = .one_minus_src_alpha,
                        },
                    },
                    .write_mask = .all,
                },
            },
        },
        .primitive = .{
            .topology = .triangle_list,
            .front_face = .ccw,
            .cull_mode = .none,
        },
        .multisample = .{
            .count = 1, // Swapchain is 1 sample. We resolve MSAA before this step.
            .mask = 0xFFFFFFFF,
            .alpha_to_coverage_enabled = .False,
        },
    });

    return Compositor{
        .pipeline = pipeline,
        .bind_group_layout = bind_group_layout,
        .sampler = sampler,
    };
}

pub fn deinit(self: *Compositor) void {
    self.pipeline.release();
    self.bind_group_layout.release();
    self.sampler.release();
}

pub fn draw(
    self: *Compositor,
    gpu: *Gpu,
    encoder: *Gpu.CommandEncoder,
    source: *const RenderTexture,
    destination_view: *Gpu.TextureView,
    load_op: Gpu.LoadOp,
    clear_color: Gpu.Color,
) !void {
    // Create the temporary bind group for this frame/resize-state
    const bg_entries = [_]Gpu.BindGroupEntry{
        .{ .binding = 0, .texture_view = source.view },
        .{ .binding = 1, .sampler = self.sampler },
    };

    const bind_group = try gpu.device.createBindGroup(&Gpu.BindGroupDescriptor{
        .label = .fromSlice("compositor_bind_group"),
        .layout = self.bind_group_layout,
        .entry_count = bg_entries.len,
        .entries = &bg_entries,
    });
    defer bind_group.release(); // Release immediately after encoding

    // Setup the render pass targeting the swapchain

    const pass = try encoder.beginRenderPass(&Gpu.RenderPassDescriptor{
        .label = .fromSlice("compositor_pass"),
        .color_attachment_count = 1,
        .color_attachments = &[_]Gpu.RenderPassColorAttachment{
            .{
                .view = destination_view,
                .load_op = load_op,
                .store_op = .store,
                .clear_value = clear_color,
            },
        },
    });
    defer pass.release();

    // Draw the fullscreen triangle
    pass.setPipeline(self.pipeline);
    pass.setBindGroup(0, bind_group, &.{});
    pass.draw(3, 1, 0, 0); // 3 vertices, 1 instance
    pass.end();
}
