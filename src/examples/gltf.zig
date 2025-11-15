const std = @import("std");
const log = std.log.scoped(.main);
const builtin = @import("builtin");
const path = std.fs.path;

const wgpu = @import("wgpu");
const glfw = @import("glfw");
const gltf = @import("gltf");
const stbi = @import("stbi");

const debug = @import("../debug.zig");
const linalg = @import("../linalg.zig");
const vec2 = linalg.vec2;
const vec3 = linalg.vec3;
const vec4 = linalg.vec4;
const vec4u = linalg.vec4u;
const quat = linalg.quat;
const mat4 = linalg.mat4;

pub const std_options = std.Options{
    .log_level = .info,
};

test {
    log.debug("semantic analysis for examples/gltf.zig", .{});
    std.testing.refAllDecls(@This());
}

const DEPTH_FORMAT = wgpu.TextureFormat.depth32_float;
const MAX_JOINTS = 128;

const Demo = struct {
    instance: wgpu.Instance = null,
    surface: wgpu.Surface = null,
    adapter: wgpu.Adapter = null,
    device: wgpu.Device = null,
    depth_texture: wgpu.Texture = null,
    depth_view: wgpu.TextureView = null,
    config: wgpu.SurfaceConfiguration = .{},
};

const Vertex = extern struct {
    position: vec3,
    normal: vec3,
    color: vec4,
    tex_coord: vec2,
    joint_indices: vec4u,
    joint_weights: vec4,
};

const MeshPrimitive = struct {
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

const Skin = struct {
    inverse_bind_matrices: []mat4,
    joints: []gltf.Index,
};

const AnimationSampler = struct {
    input: []f32, // Keyframe times
    output: []vec4, // Keyframe values (translation, rotation, or scale)
    interpolation: gltf.Interpolation,
};

pub const SceneNode = struct {
    parent: ?u32,
    children_start: u32,
    children_count: u32,
    default_translation: vec3,
    default_rotation: quat,
    default_scale: vec3,
};

const TranslationChannel = struct { sampler_index: u32, target_node: u32 };
const RotationChannel = struct { sampler_index: u32, target_node: u32 };
const ScaleChannel = struct { sampler_index: u32, target_node: u32 };

pub const Animation = struct {
    name: ?[]const u8,
    samplers: []AnimationSampler,
    duration: f32,
    translation_channels: []TranslationChannel,
    rotation_channels: []RotationChannel,
    scale_channels: []ScaleChannel,
};

// The top-level optimized model struct
pub const Model = struct {
    primitives: []MeshPrimitive,
    allocator: std.mem.Allocator,
    nodes: []SceneNode,
    // This buffer holds all child indices for all nodes contiguously.
    // Each SceneNode refers to its children via a start index and count into this buffer.
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
        self.allocator.free(self.nodes);
    }
};

const CameraUniform = extern struct {
    view_proj: mat4,
};

const SkinUniforms = extern struct {
    joint_matrices: [MAX_JOINTS]mat4,
};

const shader_text = @embedFile("shaders/GltfMultiPurpose.wgsl");

fn createDepthTexture(d: *Demo) void {
    if (d.depth_view) |v| wgpu.textureViewRelease(v);
    if (d.depth_texture) |t| wgpu.textureRelease(t);
    const depth_texture_descriptor = wgpu.TextureDescriptor{
        .label = .fromSlice("depth_texture"),
        .size = .{
            .width = d.config.width,
            .height = d.config.height,
            .depth_or_array_layers = 1,
        },
        .mip_level_count = 1,
        .sample_count = 1,
        .dimension = .@"2d",
        .format = DEPTH_FORMAT,
        .usage = wgpu.TextureUsage.renderAttachmentUsage,
        .view_format_count = 1,
        .view_formats = &.{DEPTH_FORMAT},
    };
    d.depth_texture = wgpu.deviceCreateTexture(d.device, &depth_texture_descriptor);
    d.depth_view = wgpu.textureCreateView(d.depth_texture, null);
}

/// Loads a glTF model from the given path, creating WGPU buffers and optimized runtime structures.
fn loadGltfModel(
    allocator: std.mem.Allocator,
    device: wgpu.Device,
    queue: wgpu.Queue,
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
        const model_dir = path.dirname(model_path).?;
        for (gltf_data.data.buffers) |buffer_info| {
            const bin_path = try path.join(allocator, &.{ model_dir, buffer_info.uri.? });
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

    var node_to_mesh_skin = std.AutoHashMap(gltf.Index, struct { mesh_idx: u32, skin_idx: ?u32 }).init(allocator);
    defer node_to_mesh_skin.deinit();

    for (gltf_data.data.nodes, 0..) |node, node_idx| {
        if (node.mesh) |mesh_idx| {
            try node_to_mesh_skin.put(@intCast(node_idx), .{
                .mesh_idx = @intCast(mesh_idx),
                .skin_idx = if (node.skin) |i| @intCast(i) else null,
            });
        }
    }

    for (gltf_data.data.meshes, 0..) |mesh, mesh_idx| {
        var maybe_skin_index: ?u32 = null;
        {
            var iter = node_to_mesh_skin.iterator();
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
            const vbo = wgpu.deviceCreateBuffer(device, &.{
                .label = wgpu.StringView.fromSlice(mesh.name orelse "anonymous_mesh"),
                .usage = .{ .vertex = true, .copy_dst = true },
                .size = vertex_buffer_bytes.len,
            });
            wgpu.queueWriteBuffer(queue, vbo, 0, vertex_buffer_bytes.ptr, vertex_buffer_bytes.len);
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
                        ibo = wgpu.deviceCreateBuffer(device, &.{
                            .label = .fromSlice("index_buffer"),
                            .usage = .{ .index = true, .copy_dst = true },
                            .size = index_buffer_bytes.len,
                        });
                        wgpu.queueWriteBuffer(queue, ibo, 0, index_buffer_bytes.ptr, index_buffer_bytes.len);
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
                        ibo = wgpu.deviceCreateBuffer(device, &.{
                            .label = .fromSlice("index_buffer"),
                            .usage = .{ .index = true, .copy_dst = true },
                            .size = index_buffer_bytes.len,
                        });
                        wgpu.queueWriteBuffer(queue, ibo, 0, index_buffer_bytes.ptr, index_buffer_bytes.len);
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
                const texture_descriptor = wgpu.TextureDescriptor{
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
                };
                prim_texture = wgpu.deviceCreateTexture(device, &texture_descriptor);
                prim_texture_view = wgpu.textureCreateView(prim_texture, null);
                wgpu.queueWriteTexture(
                    queue,
                    &.{ .texture = prim_texture },
                    image.data.ptr,
                    image.data.len,
                    &.{
                        .bytes_per_row = image.bytes_per_row,
                        .rows_per_image = image.height,
                    },
                    &.{
                        .width = image.width,
                        .height = image.height,
                        .depth_or_array_layers = 1,
                    },
                );
            } else {
                const texture_descriptor = wgpu.TextureDescriptor{
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
                };
                prim_texture = wgpu.deviceCreateTexture(device, &texture_descriptor);
                prim_texture_view = wgpu.textureCreateView(prim_texture, null);
                const white_pixel: u32 = 0xFFFFFFFF;
                wgpu.queueWriteTexture(
                    queue,
                    &.{ .texture = prim_texture },
                    &white_pixel,
                    4,
                    &.{
                        .bytes_per_row = 4,
                        .rows_per_image = 1,
                    },
                    &.{
                        .width = 1,
                        .height = 1,
                        .depth_or_array_layers = 1,
                    },
                );
            }
            const prim_sampler = wgpu.deviceCreateSampler(device, &.{
                .address_mode_u = .repeat,
                .address_mode_v = .repeat,
                .mag_filter = .linear,
                .min_filter = .linear,
            });
            const prim_bind_group = wgpu.deviceCreateBindGroup(device, &.{
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

    // --- Process Nodes into Optimized Scene Graph ---
    var optimized_nodes = std.ArrayList(SceneNode).empty;
    var all_children = std.ArrayList(u32).empty;
    defer {
        optimized_nodes.deinit(allocator);
        all_children.deinit(allocator);
    }
    for (gltf_data.data.nodes) |gltf_node| {
        const children_start_idx: u32 = @intCast(all_children.items.len);
        for (gltf_node.children) |child_idx| {
            try all_children.append(allocator, @intCast(child_idx));
        }
        const children_count: u32 = @intCast(all_children.items.len - children_start_idx);
        try optimized_nodes.append(allocator, .{
            .parent = if (gltf_node.parent) |p| @intCast(p) else null,
            .children_start = children_start_idx,
            .children_count = children_count,
            .default_translation = gltf_node.translation,
            .default_rotation = gltf_node.rotation,
            .default_scale = gltf_node.scale,
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
                    .target_node = @intCast(gltf_channel.target.node),
                }),
                .rotation => try rotation_channels.append(allocator, .{
                    .sampler_index = @intCast(gltf_channel.sampler),
                    .target_node = @intCast(gltf_channel.target.node),
                }),
                .scale => try scale_channels.append(allocator, .{
                    .sampler_index = @intCast(gltf_channel.sampler),
                    .target_node = @intCast(gltf_channel.target.node),
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
            .name = gltf_anim.name,
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
        .nodes = try optimized_nodes.toOwnedSlice(allocator),
        .child_buffer = try all_children.toOwnedSlice(allocator),
        .skins = try skins.toOwnedSlice(allocator),
        .animations = try animations.toOwnedSlice(allocator),
    };
}

// --- Animation Update Logic ---

// State object to hold pre-allocated buffers for animation, avoiding per-frame allocations.
const AnimationState = struct {
    local_transforms: []mat4,
    global_transforms: []mat4,
    channel_key_cache: []u32,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, model: *const Model, anim_idx: u32) !AnimationState {
        const node_count = model.nodes.len;
        const anim = &model.animations[anim_idx];
        const channel_count = anim.translation_channels.len + anim.rotation_channels.len + anim.scale_channels.len;
        const out = AnimationState{
            .local_transforms = try allocator.alloc(mat4, node_count),
            .global_transforms = try allocator.alloc(mat4, node_count),
            .channel_key_cache = try allocator.alloc(u32, channel_count),
            .allocator = allocator,
        };
        @memset(out.channel_key_cache, 0);
        return out;
    }
    pub fn deinit(self: *AnimationState) void {
        self.allocator.free(self.local_transforms);
        self.allocator.free(self.global_transforms);
        self.allocator.free(self.channel_key_cache);
    }
};

// An intermediate struct to hold TRS components before composing the matrix.
const NodePose = struct {
    translation: vec3,
    rotation: vec4,
    scale: vec3,
};

// Helper function for efficient global transform calculation.
fn updateNodeHierarchyRecursive(
    node_idx: u32,
    parent_transform: mat4,
    model: *const Model,
    state: *AnimationState,
) void {
    const node = &model.nodes[node_idx];
    const local_transform = state.local_transforms[node_idx];
    const global_transform = linalg.mat4_mul(parent_transform, local_transform);
    state.global_transforms[node_idx] = global_transform;

    const children_slice = model.child_buffer[node.children_start..][0..node.children_count];
    for (children_slice) |child_idx| {
        updateNodeHierarchyRecursive(child_idx, global_transform, model, state);
    }
}

fn updateAnimation(
    arena: std.mem.Allocator,
    model: *const Model,
    anim_idx: u32,
    time: f32,
    state: *AnimationState,
    out_matrices: []mat4,
) void {
    const animation = &model.animations[anim_idx];

    var poses = arena.alloc(NodePose, model.nodes.len) catch unreachable;
    defer arena.free(poses);

    for (model.nodes, 0..) |node, i| {
        poses[i] = .{
            .translation = node.default_translation,
            .rotation = node.default_rotation,
            .scale = node.default_scale,
        };
    }

    var key_cache_offset: u32 = 0;
    for (animation.translation_channels, 0..) |channel, i| {
        const sampler = &animation.samplers[channel.sampler_index];
        const cache_idx = key_cache_offset + @as(u32, @intCast(i));
        var current_key: usize = state.channel_key_cache[cache_idx];
        while (current_key < sampler.input.len - 1 and sampler.input[current_key + 1] < time) current_key += 1;
        state.channel_key_cache[cache_idx] = @intCast(current_key);
        const prev_key = current_key;
        const next_key = if (current_key == sampler.input.len - 1) 0 else current_key + 1;
        const time_diff = sampler.input[next_key] - sampler.input[prev_key];
        const factor = if (time_diff > 0) (time - sampler.input[prev_key]) / time_diff else 0;
        const clamped_factor = @min(@max(factor, 0.0), 1.0);
        const prev_val = sampler.output[prev_key];
        const next_val = sampler.output[next_key];
        poses[channel.target_node].translation = linalg.lerp(linalg.xyz(prev_val), linalg.xyz(next_val), clamped_factor);
    }
    key_cache_offset += @intCast(animation.translation_channels.len);

    for (animation.rotation_channels, 0..) |channel, i| {
        const sampler = &animation.samplers[channel.sampler_index];
        const cache_idx = key_cache_offset + @as(u32, @intCast(i));
        var current_key: usize = state.channel_key_cache[cache_idx];
        while (current_key < sampler.input.len - 1 and sampler.input[current_key + 1] < time) current_key += 1;
        state.channel_key_cache[cache_idx] = @intCast(current_key);
        const prev_key = current_key;
        const next_key = if (current_key == sampler.input.len - 1) 0 else current_key + 1;
        const time_diff = sampler.input[next_key] - sampler.input[prev_key];
        const factor = if (time_diff > 0) (time - sampler.input[prev_key]) / time_diff else 0;
        const clamped_factor = @min(@max(factor, 0.0), 1.0);
        const prev_val = sampler.output[prev_key];
        const next_val = sampler.output[next_key];
        poses[channel.target_node].rotation = linalg.quat_slerp(prev_val, next_val, clamped_factor);
    }
    key_cache_offset += @intCast(animation.rotation_channels.len);

    for (animation.scale_channels, 0..) |channel, i| {
        const sampler = &animation.samplers[channel.sampler_index];
        const cache_idx = key_cache_offset + @as(u32, @intCast(i));
        var current_key: usize = state.channel_key_cache[cache_idx];
        while (current_key < sampler.input.len - 1 and sampler.input[current_key + 1] < time) current_key += 1;
        state.channel_key_cache[cache_idx] = @intCast(current_key);
        const prev_key = current_key;
        const next_key = if (current_key == sampler.input.len - 1) 0 else current_key + 1;
        const time_diff = sampler.input[next_key] - sampler.input[prev_key];
        const factor = if (time_diff > 0) (time - sampler.input[prev_key]) / time_diff else 0;
        const clamped_factor = @min(@max(factor, 0.0), 1.0);
        const prev_val = sampler.output[prev_key];
        const next_val = sampler.output[next_key];
        poses[channel.target_node].scale = linalg.lerp(linalg.xyz(prev_val), linalg.xyz(next_val), clamped_factor);
    }

    for (poses, 0..) |p, i| {
        state.local_transforms[i] = linalg.mat4_compose(p.translation, p.rotation, p.scale);
    }

    for (model.nodes, 0..) |node, i| {
        if (node.parent == null) {
            updateNodeHierarchyRecursive(@intCast(i), linalg.mat4_identity, model, state);
        }
    }

    if (model.skins.len > 0) {
        const skin = &model.skins[0];
        for (skin.joints, 0..) |joint_node_idx, joint_idx| {
            if (joint_idx >= out_matrices.len) continue;
            const global_joint_transform = state.global_transforms[joint_node_idx];
            const ibm = skin.inverse_bind_matrices[joint_idx];
            out_matrices[joint_idx] = linalg.mat4_mul(global_joint_transform, ibm);
        }
    }
}

pub fn main() !void {
    var timer = try std.time.Timer.start();
    const gpa = std.heap.page_allocator;
    stbi.init(gpa);
    defer stbi.deinit();

    if (comptime builtin.os.tag != .windows) {
        glfw.initHint(.{ .platform = .x11 });
    } else {
        glfw.initHint(.{ .platform = .win32 });
    }
    try glfw.init();
    defer glfw.deinit();
    var demo = Demo{};
    const instance_extras = wgpu.InstanceExtras{
        .chain = .{ .s_type = .instance_extras },
        .backends = switch (builtin.os.tag) {
            .windows => if (glfw.isRunningInWine()) wgpu.InstanceBackend.vulkanBackend else wgpu.InstanceBackend.dx12Backend,
            else => wgpu.InstanceBackend.vulkanBackend,
        },
    };
    wgpu.setLogCallback(&struct {
        pub fn wgpu_logger(level: wgpu.LogLevel, message: wgpu.StringView, _: ?*anyopaque) callconv(.c) void {
            const msg = message.toSlice();
            const prefix = switch (level) {
                .@"error" => "wgpu error: ",
                .warn => "wgpu warn: ",
                .info => "wgpu info: ",
                .debug => "wgpu debug: ",
                .trace => "wgpu trace: ",
                else => "wgpu unknown: ",
            };
            std.debug.print("{s}{s}\n", .{ prefix, msg });
        }
    }.wgpu_logger, null);
    wgpu.setLogLevel(.warn);
    demo.instance = wgpu.createInstance(&wgpu.InstanceDescriptor{ .next_in_chain = @ptrCast(&instance_extras) });
    std.debug.assert(demo.instance != null);
    defer wgpu.instanceRelease(demo.instance);
    glfw.windowHint(.{ .client_api = .none });
    const window = try glfw.createWindow(800, 600, "zgpu glTF Animation", null, null);
    defer glfw.destroyWindow(window);
    glfw.setWindowUserPointer(window, &demo);
    _ = glfw.setFramebufferSizeCallback(window, &struct {
        fn handle_glfw_framebuffer_size(w: *glfw.Window, width: i32, height: i32) callconv(.c) void {
            if (width <= 0 and height <= 0) return;
            const d: *Demo = @ptrCast(@alignCast(glfw.getWindowUserPointer(w) orelse return));
            if (d.surface == null) return;
            d.config.width = @intCast(width);
            d.config.height = @intCast(height);
            wgpu.surfaceConfigure(d.surface, &d.config);
            createDepthTexture(d);
        }
    }.handle_glfw_framebuffer_size);
    if (comptime builtin.os.tag != .windows) {
        const x11_display = glfw.getX11Display();
        const x11_window = glfw.getX11Window(window);
        var xlib_source = wgpu.SurfaceSourceXlibWindow{
            .chain = .{ .s_type = .surface_source_xlib_window },
            .display = x11_display,
            .window = x11_window,
        };
        demo.surface = wgpu.instanceCreateSurface(demo.instance, &wgpu.SurfaceDescriptor{ .next_in_chain = @ptrCast(&xlib_source) });
    } else {
        const win32_hwnd = glfw.getWin32Window(window);
        const win32_hinstance = glfw.getWin32ModuleHandle();
        var win32_source = wgpu.SurfaceSourceWindowsHWND{
            .chain = .{ .s_type = .surface_source_windows_hwnd },
            .hwnd = win32_hwnd,
            .hinstance = win32_hinstance,
        };
        demo.surface = wgpu.instanceCreateSurface(demo.instance, &wgpu.SurfaceDescriptor{ .next_in_chain = @ptrCast(&win32_source) });
    }
    std.debug.assert(demo.surface != null);
    defer wgpu.surfaceRelease(demo.surface);
    _ = wgpu.instanceRequestAdapter(demo.instance, &wgpu.RequestAdapterOptions{ .compatible_surface = demo.surface }, .{ .callback = &struct {
        fn handle_request_adapter(status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
            _ = ud2;
            if (status == .success) {
                const d: *Demo = @ptrCast(@alignCast(ud1.?));
                d.adapter = adapter;
            } else {
                log.err("request_adapter failed: {s}", .{msg.toSlice()});
            }
        }
    }.handle_request_adapter, .userdata1 = &demo });
    while (demo.adapter == null) wgpu.instanceProcessEvents(demo.instance);
    defer wgpu.adapterRelease(demo.adapter);
    _ = wgpu.adapterRequestDevice(demo.adapter, null, .{ .callback = &struct {
        fn handle_request_device(status: wgpu.RequestDeviceStatus, device: wgpu.Device, msg: wgpu.StringView, ud1: ?*anyopaque, ud2: ?*anyopaque) callconv(.c) void {
            _ = ud2;
            if (status == .success) {
                const d: *Demo = @ptrCast(@alignCast(ud1.?));
                d.device = device;
            } else {
                log.err("request_device failed: {s}", .{msg.toSlice()});
            }
        }
    }.handle_request_device, .userdata1 = &demo });
    while (demo.device == null) wgpu.instanceProcessEvents(demo.instance);
    defer wgpu.deviceRelease(demo.device);
    defer if (demo.depth_view) |v| wgpu.textureViewRelease(v);
    defer if (demo.depth_texture) |t| wgpu.textureRelease(t);
    const queue = wgpu.deviceGetQueue(demo.device);
    defer wgpu.queueRelease(queue);
    var surface_capabilities: wgpu.SurfaceCapabilities = undefined;
    _ = wgpu.surfaceGetCapabilities(demo.surface, demo.adapter, &surface_capabilities);
    defer wgpu.surfaceCapabilitiesFreeMembers(surface_capabilities);
    const surface_format: wgpu.TextureFormat = .bgra8_unorm_srgb;
    demo.config = .{
        .device = demo.device,
        .usage = .renderAttachmentUsage,
        .format = surface_format,
        .present_mode = .fifo,
        .alpha_mode = surface_capabilities.alpha_modes.?[0],
    };
    {
        var width: i32 = 0;
        var height: i32 = 0;
        glfw.getWindowSize(window, &width, &height);
        demo.config.width = @intCast(width);
        demo.config.height = @intCast(height);
    }
    wgpu.surfaceConfigure(demo.surface, &demo.config);
    createDepthTexture(&demo);

    const camera_buffer = wgpu.deviceCreateBuffer(demo.device, &.{
        .label = .fromSlice("camera_buffer"),
        .usage = wgpu.BufferUsage.uniformUsage.merge(.copyDstUsage),
        .size = @sizeOf(CameraUniform),
    });
    defer wgpu.bufferRelease(camera_buffer);
    const camera_bind_group_layout = wgpu.deviceCreateBindGroupLayout(demo.device, &.{
        .label = .fromSlice("camera_bind_group_layout"),
        .entry_count = 1,
        .entries = &.{
            .{
                .binding = 0,
                .visibility = wgpu.ShaderStage.vertexStage,
                .buffer = .{ .type = .uniform },
            },
        },
    });
    defer wgpu.bindGroupLayoutRelease(camera_bind_group_layout);
    const texture_bind_group_layout = wgpu.deviceCreateBindGroupLayout(demo.device, &.{
        .label = .fromSlice("texture_bind_group_layout"),
        .entry_count = 2,
        .entries = &.{
            .{ .binding = 0, .visibility = wgpu.ShaderStage.fragmentStage, .texture = .{ .sample_type = .float, .view_dimension = .@"2d" } },
            .{ .binding = 1, .visibility = wgpu.ShaderStage.fragmentStage, .sampler = .{ .type = .filtering } },
        },
    });
    defer wgpu.bindGroupLayoutRelease(texture_bind_group_layout);
    const skin_uniform_buffer = wgpu.deviceCreateBuffer(demo.device, &.{
        .label = .fromSlice("skin_uniform_buffer"),
        .usage = wgpu.BufferUsage.uniformUsage.merge(.copyDstUsage),
        .size = @sizeOf(SkinUniforms),
    });
    defer wgpu.bufferRelease(skin_uniform_buffer);
    const skin_bind_group_layout = wgpu.deviceCreateBindGroupLayout(demo.device, &.{
        .label = .fromSlice("skin_bind_group_layout"),
        .entry_count = 1,
        .entries = &.{
            .{
                .binding = 0,
                .visibility = wgpu.ShaderStage.vertexStage,
                .buffer = .{ .type = .uniform },
            },
        },
    });
    defer wgpu.bindGroupLayoutRelease(skin_bind_group_layout);
    const skin_bind_group = wgpu.deviceCreateBindGroup(demo.device, &.{
        .label = .fromSlice("skin_bind_group"),
        .layout = skin_bind_group_layout,
        .entry_count = 1,
        .entries = &.{
            .{
                .binding = 0,
                .buffer = skin_uniform_buffer,
                .size = @sizeOf(SkinUniforms),
            },
        },
    });
    defer wgpu.bindGroupRelease(skin_bind_group);
    const camera_bind_group = wgpu.deviceCreateBindGroup(demo.device, &.{
        .label = .fromSlice("camera_bind_group"),
        .layout = camera_bind_group_layout,
        .entry_count = 1,
        .entries = &.{
            .{
                .binding = 0,
                .buffer = camera_buffer,
                .offset = 0,
                .size = @sizeOf(CameraUniform),
            },
        },
    });
    defer wgpu.bindGroupRelease(camera_bind_group);

    var model = try loadGltfModel(gpa, demo.device, queue, "assets/models/greenman.glb", texture_bind_group_layout);
    defer model.deinit();

    const anim_index = 2;
    var anim_state = try AnimationState.init(gpa, &model, anim_index);
    defer anim_state.deinit();

    const shader_module = try wgpu.loadShaderText(demo.device, "gltf_shader.wgsl", shader_text);
    defer wgpu.shaderModuleRelease(shader_module);
    const pipeline_layout = wgpu.deviceCreatePipelineLayout(demo.device, &.{
        .label = .fromSlice("pipeline_layout"),
        .bind_group_layout_count = 3,
        .bind_group_layouts = &.{
            camera_bind_group_layout,
            texture_bind_group_layout,
            skin_bind_group_layout,
        },
    });
    defer wgpu.pipelineLayoutRelease(pipeline_layout);
    const vertex_attributes = [_]wgpu.VertexAttribute{
        .{ .shaderLocation = 0, .offset = @offsetOf(Vertex, "position"), .format = .float32x3 },
        .{ .shaderLocation = 1, .offset = @offsetOf(Vertex, "normal"), .format = .float32x3 },
        .{ .shaderLocation = 2, .offset = @offsetOf(Vertex, "color"), .format = .float32x4 },
        .{ .shaderLocation = 3, .offset = @offsetOf(Vertex, "tex_coord"), .format = .float32x2 },
        .{ .shaderLocation = 4, .offset = @offsetOf(Vertex, "joint_indices"), .format = .uint32x4 },
        .{ .shaderLocation = 5, .offset = @offsetOf(Vertex, "joint_weights"), .format = .float32x4 },
    };
    const vertex_buffer_layout = wgpu.VertexBufferLayout{
        .array_stride = @sizeOf(Vertex),
        .step_mode = .vertex,
        .attribute_count = vertex_attributes.len,
        .attributes = &vertex_attributes,
    };
    const color_target_state = wgpu.ColorTargetState{
        .format = surface_format,
        .blend = null,
        .write_mask = .all,
    };
    const fragment_state = wgpu.FragmentState{
        .module = shader_module,
        .entry_point = .fromSlice("fs_main"),
        .target_count = 1,
        .targets = &.{color_target_state},
    };
    const depth_stencil_state = wgpu.DepthStencilState{
        .format = DEPTH_FORMAT,
        .depth_write_enabled = .true,
        .depth_compare = .less,
        .stencil_front = .{},
        .stencil_back = .{},
        .stencil_read_mask = 0,
        .stencil_write_mask = 0,
        .depth_bias = 0,
        .depth_bias_slope_scale = 0.0,
        .depth_bias_clamp = 0.0,
    };
    const render_pipeline = wgpu.deviceCreateRenderPipeline(demo.device, &wgpu.RenderPipelineDescriptor{
        .label = .fromSlice("render_pipeline"),
        .layout = pipeline_layout,
        .vertex = .{
            .module = shader_module,
            .entry_point = .fromSlice("vs_main"),
            .buffer_count = 1,
            .buffers = &.{vertex_buffer_layout},
        },
        .primitive = .{
            .topology = .triangle_list,
            .strip_index_format = .undefined,
            .cull_mode = .back,
            .front_face = .ccw,
        },
        .depth_stencil = &depth_stencil_state,
        .multisample = .{
            .count = 1,
            .mask = 0xFFFFFFFF,
            .alpha_to_coverage_enabled = .False,
        },
        .fragment = &fragment_state,
    });
    defer wgpu.renderPipelineRelease(render_pipeline);

    var arena_state = std.heap.ArenaAllocator.init(gpa);
    defer arena_state.deinit();

    const startup_ms = debug.start(&timer);
    log.info("startup completed in {d} ms", .{startup_ms});

    var animation_time: f32 = 0.0;
    var last_frame_time = glfw.getTime();
    main_loop: while (!glfw.windowShouldClose(window)) {
        const current_time = glfw.getTime();
        const delta_time: f32 = @floatCast(current_time - last_frame_time);
        last_frame_time = current_time;
        glfw.pollEvents();
        _ = arena_state.reset(.retain_capacity);

        var camera_uniform: CameraUniform = undefined;
        {
            var width: i32 = 0;
            var height: i32 = 0;
            glfw.getFramebufferSize(window, &width, &height);
            const aspect = if (height == 0) 1.0 else @as(f32, @floatFromInt(width)) / @as(f32, @floatFromInt(height));
            const proj = linalg.mat4_perspective(
                linalg.deg_to_rad * 45.0,
                aspect,
                0.1,
                100.0,
            );
            const view = linalg.mat4_look_at(
                .{ 2.5, 2.0, 3.0 },
                .{ 0.0, 1.0, 0.0 },
                .{ 0.0, 1.0, 0.0 },
            );
            camera_uniform.view_proj = linalg.mat4_mul(proj, view);
        }
        wgpu.queueWriteBuffer(queue, camera_buffer, 0, &camera_uniform, @sizeOf(CameraUniform));

        if (model.animations.len > anim_index) {
            animation_time += delta_time;
            const anim = &model.animations[anim_index];
            if (animation_time > anim.duration) {
                animation_time -= anim.duration;
                @memset(anim_state.channel_key_cache, 0); // Reset key cache on loop
            }

            var skin_uniforms: SkinUniforms = .{ .joint_matrices = undefined };

            updateAnimation(
                arena_state.allocator(),
                &model,
                anim_index,
                animation_time,
                &anim_state,
                &skin_uniforms.joint_matrices,
            );

            wgpu.queueWriteBuffer(queue, skin_uniform_buffer, 0, &skin_uniforms, @sizeOf(SkinUniforms));
        }

        var surface_texture: wgpu.SurfaceTexture = undefined;
        wgpu.surfaceGetCurrentTexture(demo.surface, &surface_texture);
        switch (surface_texture.status) {
            .success_optimal, .success_suboptimal => {},
            .timeout, .outdated, .lost => {
                if (surface_texture.texture != null) wgpu.textureRelease(surface_texture.texture);
                var width: i32 = 0;
                var height: i32 = 0;
                glfw.getWindowSize(window, &width, &height);
                if (width != 0 and height != 0) {
                    demo.config.width = @intCast(width);
                    demo.config.height = @intCast(height);
                    wgpu.surfaceConfigure(demo.surface, &demo.config);
                    createDepthTexture(&demo);
                }
                continue :main_loop;
            },
            else => std.debug.panic("get_current_texture status={any}", .{surface_texture.status}),
        }
        std.debug.assert(surface_texture.texture != null);
        defer wgpu.textureRelease(surface_texture.texture);
        const frame_view = wgpu.textureCreateView(surface_texture.texture, null);
        std.debug.assert(frame_view != null);
        defer wgpu.textureViewRelease(frame_view);
        const encoder = wgpu.deviceCreateCommandEncoder(demo.device, &.{ .label = .fromSlice("main_encoder") });
        defer wgpu.commandEncoderRelease(encoder);
        var depth_stencil_attachment = wgpu.RenderPassDepthStencilAttachment{
            .view = demo.depth_view,
            .depth_load_op = .clear,
            .depth_store_op = .store,
            .depth_clear_value = 1.0,
            .depth_read_only = .False,
            .stencil_load_op = .undefined,
            .stencil_store_op = .undefined,
        };
        const render_pass = wgpu.commandEncoderBeginRenderPass(encoder, &wgpu.RenderPassDescriptor{
            .label = .fromSlice("main_render_pass"),
            .color_attachment_count = 1,
            .color_attachments = &[_]wgpu.RenderPassColorAttachment{.{
                .view = frame_view,
                .resolve_target = null,
                .load_op = .clear,
                .store_op = .store,
                .clear_value = wgpu.Color{ .r = 0.1, .g = 0.2, .b = 0.3, .a = 1 },
            }},
            .depth_stencil_attachment = &depth_stencil_attachment,
        });
        wgpu.renderPassEncoderSetPipeline(render_pass, render_pipeline);
        wgpu.renderPassEncoderSetBindGroup(render_pass, 0, camera_bind_group, 0, null);
        for (model.primitives) |primitive| {
            wgpu.renderPassEncoderSetBindGroup(render_pass, 1, primitive.texture_bind_group, 0, null);
            if (primitive.skin_index != null) {
                wgpu.renderPassEncoderSetBindGroup(render_pass, 2, skin_bind_group, 0, null);
            }
            wgpu.renderPassEncoderSetVertexBuffer(render_pass, 0, primitive.vertex_buffer, 0, wgpu.whole_size);
            wgpu.renderPassEncoderSetIndexBuffer(render_pass, primitive.index_buffer, primitive.index_format, 0, wgpu.whole_size);
            wgpu.renderPassEncoderDrawIndexed(render_pass, primitive.index_count, 1, 0, 0, 0);
        }
        wgpu.renderPassEncoderEnd(render_pass);
        wgpu.renderPassEncoderRelease(render_pass);
        const cmd = wgpu.commandEncoderFinish(encoder, null);
        defer wgpu.commandBufferRelease(cmd);
        wgpu.queueSubmit(queue, 1, &.{cmd});
        _ = wgpu.surfacePresent(demo.surface);
        debug.lap();
    }
}
