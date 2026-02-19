const Model = @This();

const std = @import("std");
const log = std.log.scoped(.model);

const gltf = @import("gltf");
const wgpu = @import("wgpu");
const stbi = @import("stbi");

const Gpu = @import("Gpu.zig");
const linalg = @import("linalg.zig");
const vec2 = linalg.vec2;
const vec3 = linalg.vec3;
const vec4 = linalg.vec4;
const vec4u = linalg.vec4u;
const mat4 = linalg.mat4;
const quat = linalg.quat;

primitives: []MeshPrimitive,
allocator: std.mem.Allocator,
joints: []Joint,
/// This buffer holds all child indices for all joints contiguously.
/// Each Joint refers to its children via a start index and count into this buffer.
child_buffer: []u32,
skins: []Skin,
animations: []Animation,

pub fn deinit(self: *Model) void {
    for (self.primitives) |p| {
        wgpu.bufferRelease(p.vertex_buffer);
        wgpu.bufferRelease(p.index_buffer);
        wgpu.bindGroupRelease(p.texture_bind_group);
        wgpu.samplerRelease(p.sampler);
        wgpu.textureViewRelease(p.texture_view);
        wgpu.textureRelease(p.texture);
    }
    self.allocator.free(self.primitives);

    for (self.skins) |s| self.allocator.free(s.inverse_bind_matrices);
    self.allocator.free(self.skins);

    for (self.animations) |*a| {
        if (a.name) |n| self.allocator.free(n);

        for (a.samplers) |s| {
            self.allocator.free(s.input);
            self.allocator.free(s.output);
        }
        self.allocator.free(a.samplers);
        self.allocator.free(a.translation_channels);
        self.allocator.free(a.rotation_channels);
        self.allocator.free(a.scale_channels);
    }
    self.allocator.free(self.animations);
    self.allocator.free(self.child_buffer);
    self.allocator.free(self.joints);
}

/// Loads a glTF model from the given path, creating WGPU buffers and optimized runtime structures.
pub fn loadGltf(
    allocator: std.mem.Allocator,
    gpu: *Gpu,
    model_path: []const u8,
    texture_bind_group_layout: wgpu.BindGroupLayout,
) !Model {
    stbi.setFlipVerticallyOnLoad(false);

    const gltf_file_buffer = try std.fs.cwd().readFileAllocOptions(model_path, allocator, .unlimited, .@"4", null);
    defer allocator.free(gltf_file_buffer);

    var gltf_data = gltf.init(allocator);
    defer gltf_data.deinit();
    try gltf_data.parse(gltf_file_buffer);

    var binary_buffers = std.ArrayList([]align(4) const u8).empty;
    defer {
        for (binary_buffers.items) |b| allocator.free(b);
        binary_buffers.deinit(allocator);
    }
    if (gltf_data.glb_binary) |bin| {
        const owned_bin = try allocator.alignedAlloc(u8, .@"4", bin.len);
        @memcpy(owned_bin, bin);
        try binary_buffers.append(allocator, owned_bin);
    } else {
        const model_dir = std.fs.path.dirname(model_path).?;
        for (gltf_data.data.buffers) |buffer_info| {
            const bin_path = try std.fs.path.join(allocator, &.{ model_dir, buffer_info.uri.? });
            defer allocator.free(bin_path);
            const bin_buffer = try std.fs.cwd().readFileAllocOptions(bin_path, allocator, .unlimited, .@"4", null);
            try binary_buffers.append(allocator, bin_buffer);
        }
    }

    var primitive_list = std.ArrayList(MeshPrimitive).empty;
    errdefer primitive_list.deinit(allocator);

    const ColorIterator = union(enum) {
        none,
        float: gltf.AccessorIterator(f32),
        u16: gltf.AccessorIterator(u16),
        u8: gltf.AccessorIterator(u8),
    };

    var joint_to_mesh_skin = std.AutoHashMap(gltf.Index, struct { mesh_idx: u32, skin_idx: ?u32 }).init(allocator);
    defer joint_to_mesh_skin.deinit();

    for (gltf_data.data.nodes, 0..) |node, joint_idx| {
        if (node.mesh) |mesh_idx| {
            try joint_to_mesh_skin.put(@intCast(joint_idx), .{
                .mesh_idx = @intCast(mesh_idx),
                .skin_idx = if (node.skin) |i| @intCast(i) else null,
            });
        }
    }

    for (gltf_data.data.meshes, 0..) |mesh, mesh_idx| {
        var maybe_skin_index: ?u32 = null;
        {
            var iter = joint_to_mesh_skin.iterator();
            while (iter.next()) |entry| {
                if (entry.value_ptr.*.mesh_idx == mesh_idx) {
                    maybe_skin_index = entry.value_ptr.*.skin_idx;
                    break;
                }
            }
        }

        for (mesh.primitives) |primitive| {
            var positions_accessor_idx: ?gltf.Index = null;
            var normals_accessor_idx: ?gltf.Index = null;
            var colors_accessor_idx: ?gltf.Index = null;
            var texcoords_accessor_idx: ?gltf.Index = null;
            var joints_accessor_idx: ?gltf.Index = null;
            var weights_accessor_idx: ?gltf.Index = null;

            for (primitive.attributes) |attr| {
                switch (attr) {
                    .position => |idx| positions_accessor_idx = idx,
                    .normal => |idx| normals_accessor_idx = idx,
                    .color => |idx| colors_accessor_idx = idx,
                    .texcoord => |idx| texcoords_accessor_idx = idx,
                    .joints => |idx| joints_accessor_idx = idx,
                    .weights => |idx| weights_accessor_idx = idx,
                    else => {},
                }
            }

            if (positions_accessor_idx == null or normals_accessor_idx == null) {
                log.warn("Primitive in mesh '{s}' is missing positions or normals. Skipping.", .{mesh.name orelse "unnamed"});
                continue;
            }

            const pos_acc = gltf_data.data.accessors[positions_accessor_idx.?];
            var vertices = try std.ArrayList(Vertex).initCapacity(allocator, pos_acc.count);
            defer vertices.deinit(allocator);

            var pos_iter = pos_acc.iterator(
                f32,
                &gltf_data,
                binary_buffers.items[gltf_data.data.buffer_views[pos_acc.buffer_view.?].buffer],
            );
            var norm_iter = gltf_data.data.accessors[normals_accessor_idx.?].iterator(
                f32,
                &gltf_data,
                binary_buffers.items[gltf_data.data.buffer_views[gltf_data.data.accessors[normals_accessor_idx.?].buffer_view.?].buffer],
            );

            var color_iter: ColorIterator = .none;
            if (colors_accessor_idx) |color_idx| {
                const color_acc = gltf_data.data.accessors[color_idx];
                const color_buffer_view = gltf_data.data.buffer_views[color_acc.buffer_view.?];
                const color_binary_data = binary_buffers.items[color_buffer_view.buffer];
                switch (color_acc.component_type) {
                    .float => color_iter = .{
                        .float = color_acc.iterator(f32, &gltf_data, color_binary_data),
                    },
                    .unsigned_short => color_iter = .{
                        .u16 = color_acc.iterator(u16, &gltf_data, color_binary_data),
                    },
                    .unsigned_byte => color_iter = .{
                        .u8 = color_acc.iterator(u8, &gltf_data, color_binary_data),
                    },
                    else => log.warn("Unsupported vertex color format: {any}", .{color_acc.component_type}),
                }
            }

            var maybe_uv_iter: ?gltf.AccessorIterator(f32) = null;
            if (texcoords_accessor_idx) |uv_idx| {
                const uv_acc = gltf_data.data.accessors[uv_idx];
                const uv_buffer_view = gltf_data.data.buffer_views[uv_acc.buffer_view.?];
                const uv_binary_data = binary_buffers.items[uv_buffer_view.buffer];
                maybe_uv_iter = uv_acc.iterator(f32, &gltf_data, uv_binary_data);
            }

            var maybe_joint_iter_u8: ?gltf.AccessorIterator(u8) = null;
            var maybe_joint_iter_u16: ?gltf.AccessorIterator(u16) = null;
            if (joints_accessor_idx) |joint_idx| {
                const j_acc = gltf_data.data.accessors[joint_idx];
                const j_bv = gltf_data.data.buffer_views[j_acc.buffer_view.?];
                const j_bin = binary_buffers.items[j_bv.buffer];
                switch (j_acc.component_type) {
                    .unsigned_byte => maybe_joint_iter_u8 = j_acc.iterator(u8, &gltf_data, j_bin),
                    .unsigned_short => maybe_joint_iter_u16 = j_acc.iterator(u16, &gltf_data, j_bin),
                    else => log.warn("Unsupported joint index format: {any}", .{j_acc.component_type}),
                }
            }
            var maybe_weight_iter: ?gltf.AccessorIterator(f32) = null;
            if (weights_accessor_idx) |weight_idx| {
                const w_acc = gltf_data.data.accessors[weight_idx];
                const w_bv = gltf_data.data.buffer_views[w_acc.buffer_view.?];
                const w_bin = binary_buffers.items[w_bv.buffer];
                maybe_weight_iter = w_acc.iterator(f32, &gltf_data, w_bin);
            }

            while (pos_iter.next()) |pos_slice| {
                const norm_slice = norm_iter.next().?;
                const pos_vec: vec3 = .{ pos_slice[0], pos_slice[1], pos_slice[2] };
                const norm_vec: vec3 = .{ norm_slice[0], norm_slice[1], norm_slice[2] };
                const color_vec: vec4 = switch (color_iter) {
                    .none => .{ 1.0, 1.0, 1.0, 1.0 },
                    .float => |*iter| blk: {
                        const s = iter.next().?;
                        break :blk if (s.len == 3)
                            .{ s[0], s[1], s[2], 1.0 }
                        else
                            .{ s[0], s[1], s[2], s[3] };
                    },
                    .u16 => |*iter| blk: {
                        const s = iter.next().?;
                        const n: f32 = 1.0 / 65535.0;
                        break :blk if (s.len == 3)
                            .{
                                @as(f32, @floatFromInt(s[0])) * n,
                                @as(f32, @floatFromInt(s[1])) * n,
                                @as(f32, @floatFromInt(s[2])) * n,
                                1.0,
                            }
                        else
                            .{
                                @as(f32, @floatFromInt(s[0])) * n,
                                @as(f32, @floatFromInt(s[1])) * n,
                                @as(f32, @floatFromInt(s[2])) * n,
                                @as(f32, @floatFromInt(s[3])) * n,
                            };
                    },
                    .u8 => |*iter| blk: {
                        const s = iter.next().?;
                        const n: f32 = 1.0 / 255.0;
                        break :blk if (s.len == 3)
                            .{
                                @as(f32, @floatFromInt(s[0])) * n,
                                @as(f32, @floatFromInt(s[1])) * n,
                                @as(f32, @floatFromInt(s[2])) * n,
                                1.0,
                            }
                        else
                            .{
                                @as(f32, @floatFromInt(s[0])) * n,
                                @as(f32, @floatFromInt(s[1])) * n,
                                @as(f32, @floatFromInt(s[2])) * n,
                                @as(f32, @floatFromInt(s[3])) * n,
                            };
                    },
                };
                const uv_vec: vec2 = if (maybe_uv_iter) |*uv_iter| blk: {
                    const uv_slice = uv_iter.next().?;
                    break :blk .{ uv_slice[0], uv_slice[1] };
                } else .{ 0.0, 0.0 };

                const joint_vec: vec4u = if (maybe_joint_iter_u8) |*iter| blk: {
                    const s = iter.next().?;
                    break :blk .{ @intCast(s[0]), @intCast(s[1]), @intCast(s[2]), @intCast(s[3]) };
                } else if (maybe_joint_iter_u16) |*iter| blk: {
                    const s = iter.next().?;
                    break :blk .{ @intCast(s[0]), @intCast(s[1]), @intCast(s[2]), @intCast(s[3]) };
                } else .{ 0, 0, 0, 0 };

                const weight_vec: vec4 = if (maybe_weight_iter) |*iter| blk: {
                    const s = iter.next().?;
                    break :blk .{ s[0], s[1], s[2], s[3] };
                } else .{ 1.0, 0.0, 0.0, 0.0 };

                vertices.appendAssumeCapacity(.{
                    .position = pos_vec,
                    .normal = norm_vec,
                    .color = color_vec,
                    .tex_coord = uv_vec,
                    .joint_indices = joint_vec,
                    .joint_weights = weight_vec,
                });
            }

            const vertex_buffer_bytes = std.mem.sliceAsBytes(vertices.items);
            const vbo = gpu.createBuffer(&.{
                .label = .fromSlice(mesh.name orelse "anonymous_mesh"),
                .usage = .{ .vertex = true, .copy_dst = true },
                .size = vertex_buffer_bytes.len,
            });
            gpu.writeBuffer(vbo, 0, vertex_buffer_bytes);
            const idx_acc = gltf_data.data.accessors[primitive.indices.?];
            var ibo: wgpu.Buffer = undefined;
            const index_count: u32 = @intCast(idx_acc.count);
            var index_format: wgpu.IndexFormat = undefined;
            switch (idx_acc.component_type) {
                .unsigned_short, .unsigned_integer => {
                    index_format = if (idx_acc.component_type == .unsigned_short) .uint16 else .uint32;
                    if (idx_acc.component_type == .unsigned_short) {
                        var indices = try std.ArrayList(u16).initCapacity(allocator, idx_acc.count);
                        defer indices.deinit(allocator);
                        var idx_iter = idx_acc.iterator(
                            u16,
                            &gltf_data,
                            binary_buffers.items[gltf_data.data.buffer_views[idx_acc.buffer_view.?].buffer],
                        );
                        while (idx_iter.next()) |idx_slice| for (idx_slice) |idx| indices.appendAssumeCapacity(idx);
                        const index_buffer_bytes = std.mem.sliceAsBytes(indices.items);
                        ibo = gpu.createBuffer(&.{
                            .label = .fromSlice("index_buffer"),
                            .usage = .{ .index = true, .copy_dst = true },
                            .size = index_buffer_bytes.len,
                        });
                        gpu.writeBuffer(ibo, 0, index_buffer_bytes);
                    } else {
                        var indices = try std.ArrayList(u32).initCapacity(allocator, idx_acc.count);
                        defer indices.deinit(allocator);
                        var idx_iter = idx_acc.iterator(
                            u32,
                            &gltf_data,
                            binary_buffers.items[gltf_data.data.buffer_views[idx_acc.buffer_view.?].buffer],
                        );
                        while (idx_iter.next()) |idx_slice| for (idx_slice) |idx| indices.appendAssumeCapacity(idx);
                        const index_buffer_bytes = std.mem.sliceAsBytes(indices.items);
                        ibo = gpu.createBuffer(&.{
                            .label = .fromSlice("index_buffer"),
                            .usage = .{ .index = true, .copy_dst = true },
                            .size = index_buffer_bytes.len,
                        });
                        gpu.writeBuffer(ibo, 0, index_buffer_bytes);
                    }
                },
                else => return error.UnsupportedIndexFormat,
            }
            var prim_texture: wgpu.Texture = undefined;
            var prim_texture_view: wgpu.TextureView = undefined;
            var maybe_image: ?stbi.Image = null;
            defer if (maybe_image) |*img| img.deinit();
            if (primitive.material) |mat_idx| {
                const material = gltf_data.data.materials[mat_idx];
                if (material.metallic_roughness.base_color_texture) |tex_info| {
                    const texture_def = gltf_data.data.textures[tex_info.index];
                    if (texture_def.source) |img_idx| {
                        const image_def = gltf_data.data.images[img_idx];
                        if (image_def.buffer_view) |bv_idx| {
                            const buffer_view = gltf_data.data.buffer_views[bv_idx];
                            const image_binary_data = binary_buffers.items[buffer_view.buffer];
                            const image_slice = image_binary_data[buffer_view.byte_offset..][0..buffer_view.byte_length];
                            maybe_image = try stbi.Image.loadFromMemory(image_slice, 4);
                        }
                    }
                }
            }
            if (maybe_image) |image| {
                prim_texture = gpu.createTexture(&.{
                    .label = .fromSlice("gltf_texture"),
                    .size = .{
                        .width = image.width,
                        .height = image.height,
                        .depth_or_array_layers = 1,
                    },
                    .mip_level_count = 1,
                    .sample_count = 1,
                    .dimension = .@"2d",
                    .format = .rgba8_unorm_srgb,
                    .usage = .{
                        .texture_binding = true,
                        .copy_dst = true,
                    },
                });
                prim_texture_view = wgpu.textureCreateView(prim_texture, null);
                gpu.writeTextureImage(.{ .texture = prim_texture }, &image);
            } else {
                prim_texture = gpu.createTexture(&.{
                    .label = .fromSlice("fallback_texture"),
                    .size = .{
                        .width = 1,
                        .height = 1,
                        .depth_or_array_layers = 1,
                    },
                    .mip_level_count = 1,
                    .sample_count = 1,
                    .dimension = .@"2d",
                    .format = .rgba8_unorm_srgb,
                    .usage = .{
                        .texture_binding = true,
                        .copy_dst = true,
                    },
                });
                prim_texture_view = wgpu.textureCreateView(prim_texture, null);
                const white_pixel: u32 = 0xFFFFFFFF;
                gpu.writeTexture(
                    .{ .texture = prim_texture },
                    .{
                        .bytes_per_row = 4,
                        .rows_per_image = 1,
                    },
                    .{
                        .width = 1,
                        .height = 1,
                        .depth_or_array_layers = 1,
                    },
                    &white_pixel,
                );
            }
            const prim_sampler = gpu.createSampler(&.{
                .address_mode_u = .repeat,
                .address_mode_v = .repeat,
                .mag_filter = .linear,
                .min_filter = .linear,
            });
            const prim_bind_group = gpu.createBindGroup(&.{
                .layout = texture_bind_group_layout,
                .entry_count = 2,
                .entries = &.{
                    .{ .binding = 0, .texture_view = prim_texture_view },
                    .{ .binding = 1, .sampler = prim_sampler },
                },
            });
            try primitive_list.append(allocator, .{
                .vertex_buffer = vbo,
                .index_buffer = ibo,
                .index_count = index_count,
                .index_format = index_format,
                .texture = prim_texture,
                .texture_view = prim_texture_view,
                .sampler = prim_sampler,
                .texture_bind_group = prim_bind_group,
                .skin_index = maybe_skin_index,
            });
        }
    }

    // --- Load Skins ---
    var skins = try std.ArrayList(Skin).initCapacity(allocator, gltf_data.data.skins.len);
    for (gltf_data.data.skins) |gltf_skin| {
        const ibm_acc = gltf_data.data.accessors[gltf_skin.inverse_bind_matrices.?];
        const ibm_bv = gltf_data.data.buffer_views[ibm_acc.buffer_view.?];
        const ibm_bin = binary_buffers.items[ibm_bv.buffer];
        var ibm_iter = ibm_acc.iterator(f32, &gltf_data, ibm_bin);
        var ibms = try std.ArrayList(mat4).initCapacity(allocator, ibm_acc.count);
        errdefer ibms.deinit(allocator);
        while (ibm_iter.next()) |mat_slice| {
            try ibms.append(allocator, std.mem.bytesAsValue(mat4, mat_slice[0..16]).*);
        }
        skins.appendAssumeCapacity(.{
            .inverse_bind_matrices = try ibms.toOwnedSlice(allocator),
            .joints = try allocator.dupe(usize, gltf_skin.joints),
        });
    }

    // --- Process Joints into Optimized Scene Graph ---
    var optimized_joints = std.ArrayList(Joint).empty;
    var all_children = std.ArrayList(u32).empty;
    defer {
        optimized_joints.deinit(allocator);
        all_children.deinit(allocator);
    }
    for (gltf_data.data.nodes) |node| {
        const children_start_idx: u32 = @intCast(all_children.items.len);
        for (node.children) |child_idx| {
            try all_children.append(allocator, @intCast(child_idx));
        }
        const children_count: u32 = @intCast(all_children.items.len - children_start_idx);
        try optimized_joints.append(allocator, .{
            .parent = if (node.parent) |p| @intCast(p) else null,
            .children_start = children_start_idx,
            .children_count = children_count,
            .default_translation = node.translation,
            .default_rotation = node.rotation,
            .default_scale = node.scale,
        });
    }

    // --- Process Animations into Optimized Format ---
    var animations = try std.ArrayList(Animation).initCapacity(allocator, gltf_data.data.animations.len);
    errdefer animations.deinit(allocator);

    for (gltf_data.data.animations, 0..) |gltf_anim, anim_index| {
        var anim_samplers = try std.ArrayList(AnimationSampler).initCapacity(allocator, gltf_anim.samplers.len);
        errdefer anim_samplers.deinit(allocator);

        for (gltf_anim.samplers, 0..) |gltf_sampler, sampler_index| {
            const time_acc = gltf_data.data.accessors[gltf_sampler.input];
            const time_bv = gltf_data.data.buffer_views[time_acc.buffer_view.?];
            const time_bin = binary_buffers.items[time_bv.buffer];
            var time_iter = time_acc.iterator(f32, &gltf_data, time_bin);
            if (gltf_sampler.interpolation != .linear) {
                log.warn("non-linear interpolation mode specified for model {s} animation {s}/{} sampler {}", .{
                    model_path,
                    gltf_anim.name orelse "unnamed",
                    anim_index,
                    sampler_index,
                });
            }
            var times = try std.ArrayList(f32).initCapacity(allocator, time_acc.count);
            errdefer times.deinit(allocator);
            while (time_iter.next()) |s| for (s) |t| try times.append(allocator, t);

            const val_acc = gltf_data.data.accessors[gltf_sampler.output];
            const val_bv = gltf_data.data.buffer_views[val_acc.buffer_view.?];
            const val_bin = binary_buffers.items[val_bv.buffer];
            var val_iter = val_acc.iterator(f32, &gltf_data, val_bin);
            var values = try std.ArrayList(vec4).initCapacity(allocator, val_acc.count);
            errdefer values.deinit(allocator);
            while (val_iter.next()) |s| {
                switch (s.len) {
                    3 => try values.append(allocator, .{ s[0], s[1], s[2], 0.0 }),
                    4 => try values.append(allocator, .{ s[0], s[1], s[2], s[3] }),
                    else => return error.UnsupportedAnimationFormat,
                }
            }
            anim_samplers.appendAssumeCapacity(.{
                .input = try times.toOwnedSlice(allocator),
                .output = try values.toOwnedSlice(allocator),
                .interpolation = gltf_sampler.interpolation,
            });
        }

        var translation_channels = std.ArrayList(TranslationChannel).empty;
        defer translation_channels.deinit(allocator);
        var rotation_channels = std.ArrayList(RotationChannel).empty;
        defer rotation_channels.deinit(allocator);
        var scale_channels = std.ArrayList(ScaleChannel).empty;
        defer scale_channels.deinit(allocator);

        for (gltf_anim.channels) |gltf_channel| {
            switch (gltf_channel.target.property) {
                .translation => try translation_channels.append(allocator, .{
                    .sampler_index = @intCast(gltf_channel.sampler),
                    .target_joint = @intCast(gltf_channel.target.node),
                }),
                .rotation => try rotation_channels.append(allocator, .{
                    .sampler_index = @intCast(gltf_channel.sampler),
                    .target_joint = @intCast(gltf_channel.target.node),
                }),
                .scale => try scale_channels.append(allocator, .{
                    .sampler_index = @intCast(gltf_channel.sampler),
                    .target_joint = @intCast(gltf_channel.target.node),
                }),
                else => {},
            }
        }

        var max_time: f32 = 0;
        for (anim_samplers.items) |sampler| {
            if (sampler.input.len > 0 and sampler.input[sampler.input.len - 1] > max_time) {
                max_time = sampler.input[sampler.input.len - 1];
            }
        }

        try animations.append(allocator, .{
            .name = if (gltf_anim.name) |n| try allocator.dupe(u8, n) else null,
            .samplers = try anim_samplers.toOwnedSlice(allocator),
            .duration = max_time,
            .translation_channels = try translation_channels.toOwnedSlice(allocator),
            .rotation_channels = try rotation_channels.toOwnedSlice(allocator),
            .scale_channels = try scale_channels.toOwnedSlice(allocator),
        });
    }

    return Model{
        .primitives = try primitive_list.toOwnedSlice(allocator),
        .allocator = allocator,
        .joints = try optimized_joints.toOwnedSlice(allocator),
        .child_buffer = try all_children.toOwnedSlice(allocator),
        .skins = try skins.toOwnedSlice(allocator),
        .animations = try animations.toOwnedSlice(allocator),
    };
}

pub const MAX_JOINTS = 128;

pub const Vertex = extern struct {
    position: vec3,
    normal: vec3,
    color: vec4,
    tex_coord: vec2,
    joint_indices: vec4u,
    joint_weights: vec4,
};

pub const MeshPrimitive = struct {
    vertex_buffer: wgpu.Buffer,
    index_buffer: wgpu.Buffer,
    index_count: u32,
    index_format: wgpu.IndexFormat,
    texture: wgpu.Texture,
    texture_view: wgpu.TextureView,
    sampler: wgpu.Sampler,
    texture_bind_group: wgpu.BindGroup,
    skin_index: ?u32,
};

pub const Skin = struct {
    inverse_bind_matrices: []mat4,
    joints: []gltf.Index,
};

pub const AnimationSampler = struct {
    input: []f32, // Keyframe times
    output: []vec4, // Keyframe values (translation, rotation, or scale)
    interpolation: gltf.Interpolation,
};

pub const Joint = struct {
    parent: ?u32,
    children_start: u32,
    children_count: u32,
    default_translation: vec3,
    default_rotation: quat,
    default_scale: vec3,
};

pub const TranslationChannel = struct { sampler_index: u32, target_joint: u32 };
pub const RotationChannel = struct { sampler_index: u32, target_joint: u32 };
pub const ScaleChannel = struct { sampler_index: u32, target_joint: u32 };

pub const SkinUniforms = [MAX_JOINTS]mat4;

pub const Animation = struct {
    name: ?[]const u8,
    samplers: []AnimationSampler,
    duration: f32,
    translation_channels: []TranslationChannel,
    rotation_channels: []RotationChannel,
    scale_channels: []ScaleChannel,
};

/// An intermediate struct to hold TRS components before composing matrices.
pub const JointPose = struct {
    translation: vec3,
    rotation: vec4,
    scale: vec3,
};

/// State management holding pre-allocated buffers for animation, avoiding per-frame allocations.
pub const AnimationState = struct {
    allocator: std.mem.Allocator,

    poses: []JointPose,
    matrices: []mat4,
    local_transforms: []mat4,
    global_transforms: []mat4,
    channel_key_cache: []u32,

    pub fn init(allocator: std.mem.Allocator, model: *const Model, anim_idx: u32) !AnimationState {
        const joint_count = model.joints.len;
        const anim = &model.animations[anim_idx];
        const channel_count = anim.translation_channels.len + anim.rotation_channels.len + anim.scale_channels.len;
        const out = AnimationState{
            .poses = try allocator.alloc(JointPose, joint_count),
            .local_transforms = try allocator.alloc(mat4, joint_count),
            .global_transforms = try allocator.alloc(mat4, joint_count),
            .matrices = try allocator.alloc(mat4, joint_count),
            .channel_key_cache = try allocator.alloc(u32, channel_count),
            .allocator = allocator,
        };
        @memset(out.channel_key_cache, 0);
        return out;
    }

    pub fn deinit(self: *AnimationState) void {
        self.allocator.free(self.poses);
        self.allocator.free(self.local_transforms);
        self.allocator.free(self.global_transforms);
        self.allocator.free(self.channel_key_cache);
    }

    pub fn updateAnimation(
        self: *AnimationState,
        model: *const Model,
        anim_idx: u32,
        time: f32,
    ) void {
        const animation = &model.animations[anim_idx];

        for (model.joints, 0..) |joint, i| {
            self.poses[i] = .{
                .translation = joint.default_translation,
                .rotation = joint.default_rotation,
                .scale = joint.default_scale,
            };
        }

        var key_cache_offset: u32 = 0;
        for (animation.translation_channels, 0..) |channel, i| {
            const sampler = &animation.samplers[channel.sampler_index];
            const cache_idx = key_cache_offset + @as(u32, @intCast(i));
            var current_key: usize = self.channel_key_cache[cache_idx];
            while (current_key < sampler.input.len - 1 and sampler.input[current_key + 1] < time) current_key += 1;
            self.channel_key_cache[cache_idx] = @intCast(current_key);
            const prev_key = current_key;
            const next_key = if (current_key == sampler.input.len - 1) 0 else current_key + 1;
            const time_diff = sampler.input[next_key] - sampler.input[prev_key];
            const factor = if (time_diff > 0) (time - sampler.input[prev_key]) / time_diff else 0;
            const clamped_factor = @min(@max(factor, 0.0), 1.0);
            const prev_val = sampler.output[prev_key];
            const next_val = sampler.output[next_key];
            self.poses[channel.target_joint].translation = linalg.lerp(linalg.xyz(prev_val), linalg.xyz(next_val), clamped_factor);
        }
        key_cache_offset += @intCast(animation.translation_channels.len);

        for (animation.rotation_channels, 0..) |channel, i| {
            const sampler = &animation.samplers[channel.sampler_index];
            const cache_idx = key_cache_offset + @as(u32, @intCast(i));
            var current_key: usize = self.channel_key_cache[cache_idx];
            while (current_key < sampler.input.len - 1 and sampler.input[current_key + 1] < time) current_key += 1;
            self.channel_key_cache[cache_idx] = @intCast(current_key);
            const prev_key = current_key;
            const next_key = if (current_key == sampler.input.len - 1) 0 else current_key + 1;
            const time_diff = sampler.input[next_key] - sampler.input[prev_key];
            const factor = if (time_diff > 0) (time - sampler.input[prev_key]) / time_diff else 0;
            const clamped_factor = @min(@max(factor, 0.0), 1.0);
            const prev_val = sampler.output[prev_key];
            const next_val = sampler.output[next_key];
            self.poses[channel.target_joint].rotation = linalg.quat_slerp(prev_val, next_val, clamped_factor);
        }
        key_cache_offset += @intCast(animation.rotation_channels.len);

        for (animation.scale_channels, 0..) |channel, i| {
            const sampler = &animation.samplers[channel.sampler_index];
            const cache_idx = key_cache_offset + @as(u32, @intCast(i));
            var current_key: usize = self.channel_key_cache[cache_idx];
            while (current_key < sampler.input.len - 1 and sampler.input[current_key + 1] < time) current_key += 1;
            self.channel_key_cache[cache_idx] = @intCast(current_key);
            const prev_key = current_key;
            const next_key = if (current_key == sampler.input.len - 1) 0 else current_key + 1;
            const time_diff = sampler.input[next_key] - sampler.input[prev_key];
            const factor = if (time_diff > 0) (time - sampler.input[prev_key]) / time_diff else 0;
            const clamped_factor = @min(@max(factor, 0.0), 1.0);
            const prev_val = sampler.output[prev_key];
            const next_val = sampler.output[next_key];
            self.poses[channel.target_joint].scale = linalg.lerp(linalg.xyz(prev_val), linalg.xyz(next_val), clamped_factor);
        }

        for (self.poses, 0..) |p, i| {
            self.local_transforms[i] = linalg.mat4_compose(p.translation, p.rotation, p.scale);
        }

        for (model.joints, 0..) |joint, i| {
            if (joint.parent == null) {
                self.updateJointHierarchyRecursive(@intCast(i), linalg.mat4_identity, model);
            }
        }

        if (model.skins.len > 0) {
            const skin = &model.skins[0];
            for (skin.joints, 0..) |joint_joint_idx, joint_idx| {
                if (joint_idx >= self.matrices.len) continue;
                const global_joint_transform = self.global_transforms[joint_joint_idx];
                const ibm = skin.inverse_bind_matrices[joint_idx];
                self.matrices[joint_idx] = linalg.mat4_mul(global_joint_transform, ibm);
            }
        }
    }

    fn updateJointHierarchyRecursive(
        self: *AnimationState,
        joint_idx: u32,
        parent_transform: mat4,
        model: *const Model,
    ) void {
        const joint = &model.joints[joint_idx];
        const local_transform = self.local_transforms[joint_idx];
        const global_transform = linalg.mat4_mul(parent_transform, local_transform);
        self.global_transforms[joint_idx] = global_transform;

        const children_slice = model.child_buffer[joint.children_start..][0..joint.children_count];
        for (children_slice) |child_idx| {
            self.updateJointHierarchyRecursive(child_idx, global_transform, model);
        }
    }
};
