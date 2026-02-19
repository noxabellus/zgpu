const Compositor = @This();
const std = @import("std");
const wgpu = @import("wgpu");
const Gpu = @import("Gpu.zig");
const RenderTexture = @import("RenderTexture.zig");

pipeline: wgpu.RenderPipeline,
bind_group_layout: wgpu.BindGroupLayout,
sampler: wgpu.Sampler,

pub fn init(gpu: *Gpu) !Compositor {
    // Load the shader
    // TODO: custom shaders so we can do post-processing effects. for now just blit.
    const shader = try gpu.loadShaderText("shaders/BlitCompositor.wgsl", @embedFile("shaders/BlitCompositor.wgsl"));
    defer wgpu.shaderModuleRelease(shader);

    // Create the Sampler
    const sampler = gpu.createSampler(&wgpu.SamplerDescriptor{
        .label = .fromSlice("compositor_sampler"),
        .address_mode_u = .clamp_to_edge,
        .address_mode_v = .clamp_to_edge,
        .address_mode_w = .clamp_to_edge,
        .mag_filter = .linear,
        .min_filter = .linear,
        .mipmap_filter = .linear,
    });

    // Create Bind Group Layout (Texture @ 0, Sampler @ 1)
    const bgl_entries = [_]wgpu.BindGroupLayoutEntry{
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

    const bind_group_layout = gpu.createBindGroupLayout(&wgpu.BindGroupLayoutDescriptor{
        .label = .fromSlice("compositor_bgl"),
        .entry_count = bgl_entries.len,
        .entries = &bgl_entries,
    });

    // Create Pipeline Layout
    const pipeline_layout = gpu.createPipelineLayout(&wgpu.PipelineLayoutDescriptor{
        .label = .fromSlice("compositor_pipeline_layout"),
        .bind_group_layout_count = 1,
        .bind_group_layouts = &.{bind_group_layout},
    });
    defer wgpu.pipelineLayoutRelease(pipeline_layout);

    // Create Render Pipeline
    const pipeline = gpu.createPipeline(&wgpu.RenderPipelineDescriptor{
        .label = .fromSlice("compositor_pipeline"),
        .layout = pipeline_layout,
        .vertex = .{
            .module = shader,
            .entry_point = .fromSlice("vs_main"),
            .buffer_count = 0,
            .buffers = null,
        },
        .fragment = &wgpu.FragmentState{
            .module = shader,
            .entry_point = .fromSlice("fs_main"),
            .target_count = 1,
            .targets = &[_]wgpu.ColorTargetState{
                .{
                    .format = gpu.surface_format, // MUST match the swapchain format!
                    .blend = &wgpu.BlendState{
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
    wgpu.renderPipelineRelease(self.pipeline);
    wgpu.bindGroupLayoutRelease(self.bind_group_layout);
    wgpu.samplerRelease(self.sampler);
}

pub fn draw(
    self: *Compositor,
    gpu: *Gpu,
    encoder: wgpu.CommandEncoder,
    source: *const RenderTexture,
    destination_view: wgpu.TextureView,
    load_op: wgpu.LoadOp,
    clear_color: wgpu.Color,
) void {
    // Create the temporary bind group for this frame/resize-state
    const bg_entries = [_]wgpu.BindGroupEntry{
        .{ .binding = 0, .texture_view = source.view },
        .{ .binding = 1, .sampler = self.sampler },
    };

    const bind_group = gpu.createBindGroup(&wgpu.BindGroupDescriptor{
        .label = .fromSlice("compositor_bind_group"),
        .layout = self.bind_group_layout,
        .entry_count = bg_entries.len,
        .entries = &bg_entries,
    });
    defer wgpu.bindGroupRelease(bind_group); // Release immediately after encoding

    // Setup the render pass targeting the swapchain

    const pass = wgpu.commandEncoderBeginRenderPass(encoder, &wgpu.RenderPassDescriptor{
        .label = .fromSlice("compositor_pass"),
        .color_attachment_count = 1,
        .color_attachments = &[_]wgpu.RenderPassColorAttachment{
            .{
                .view = destination_view,
                .load_op = load_op,
                .store_op = .store,
                .clear_value = clear_color,
            },
        },
    });
    defer wgpu.renderPassEncoderRelease(pass);

    // Draw the fullscreen triangle
    wgpu.renderPassEncoderSetPipeline(pass, self.pipeline);
    wgpu.renderPassEncoderSetBindGroup(pass, 0, bind_group, 0, null);
    wgpu.renderPassEncoderDraw(pass, 3, 1, 0, 0); // 3 vertices, 1 instance
    wgpu.renderPassEncoderEnd(pass);
}
