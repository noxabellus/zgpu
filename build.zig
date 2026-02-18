/// build.zig
const std = @import("std");

fn addSystemCFlags(b: *std.Build, mod: *std.Build.Module) void {
    addSystemCFlagsFromEnv(b, mod, "NIX_CFLAGS_COMPILE");
    addSystemCFlagsFromEnv(b, mod, "CFLAGS");
}

fn addSystemCFlagsFromEnv(b: *std.Build, mod: *std.Build.Module, env_var: []const u8) void {
    if (std.process.getEnvVarOwned(b.allocator, env_var)) |cflags| {
        defer b.allocator.free(cflags);

        var it = std.mem.splitAny(u8, cflags, " \n");
        while (it.next()) |flag| {
            if (flag.len == 0) continue;

            if (std.mem.startsWith(u8, flag, "-I")) {
                const path = flag[2..];
                // Handle both -I/path and -I /path
                if (path.len > 0) {
                    addIncludePath(b, mod, path);
                } else if (it.next()) |next_path| {
                    addIncludePath(b, mod, next_path);
                }
            } else if (std.mem.eql(u8, flag, "-isystem")) {
                if (it.next()) |path| {
                    addIncludePath(b, mod, path);
                } else {
                    std.debug.print("Warning: -isystem flag without a path\n", .{});
                }
            } else {
                // std.debug.print("Warning: ignoring unsupported {s} flag: {s}\n", .{env_var, flag});
            }
        }
    } else |err| {
        if (err != error.EnvironmentVariableNotFound) {
            std.debug.print("Warning: could not read {s}: {s}; if you are not on nix, ignore this.\n", .{ env_var, @errorName(err) });
        }
    }
}

/// Helper to add a path, detecting if it's absolute or relative.
fn addIncludePath(b: *std.Build, mod: *std.Build.Module, path: []const u8) void {
    if (std.fs.path.isAbsolute(path)) {
        // std.debug.print("Adding absolute system include path: {s}\n", .{path});
        mod.addIncludePath(.{ .cwd_relative = path });
    } else {
        // This case is unlikely with Nix but good to have.
        // std.debug.print("Adding relative system include path: {s}\n", .{path});
        mod.addIncludePath(b.path(path));
    }
}

pub fn build(b: *std.Build) void {
    // std.debug.print("build.zig started\n", .{});
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    if (target.result.cpu.arch != .x86_64) {
        std.debug.panic("Cannot build for architecture `{s}`", .{@tagName(target.result.cpu.arch)});
    }

    const run_step = b.step("run", "Run the app");
    const check_step = b.step("check", "Semantic analysis");
    const test_step = b.step("test", "Run tests");
    const install_step = b.default_step;

    const is_windows = target.result.os.tag == .windows;
    if (!is_windows and target.result.os.tag != .linux) std.debug.panic("Cannot build for os `{s}`", .{@tagName(target.result.os.tag)});

    // --- Dependencies ---
    const wgpu_dep =
        if (is_windows)
            b.lazyDependency("wgpu_native_windows_x64", .{}).?
        else
            b.lazyDependency("wgpu_native_linux_x64", .{}).?;

    const stb_dep = b.dependency("stb", .{});
    const glfw_dep = b.dependency("glfw", .{});
    const clay_dep = b.dependency("clay", .{});
    const nfd_dep = b.dependency("nfd", .{});

    // --- WGPU Module ---
    const wgpu_mod = b.createModule(.{
        .root_source_file = b.path("libs/wgpu.zig"),
        .target = target,
        .optimize = optimize,
    });

    const wgpu_test = b.addTest(.{ .root_module = wgpu_mod });
    check_step.dependOn(&wgpu_test.step);
    test_step.dependOn(&b.addRunArtifact(wgpu_test).step);

    wgpu_mod.addLibraryPath(wgpu_dep.path("lib/"));
    wgpu_mod.linkSystemLibrary("wgpu_native", .{});

    // --- STB_IMAGE Module ---
    const stbi_mod = b.createModule(.{
        .root_source_file = b.path("libs/stbi/stbi.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const stbi_test = b.addTest(.{ .root_module = stbi_mod });
    check_step.dependOn(&stbi_test.step);
    test_step.dependOn(&b.addRunArtifact(stbi_test).step);

    stbi_mod.addIncludePath(stb_dep.path("."));
    stbi_mod.addCSourceFile(.{
        .file = b.path("libs/stbi/stbi.c"),
        .flags = &.{ "-std=c99", "-fno-sanitize=undefined" },
    });

    // --- STB_TRUETYPE Module ---
    const stbtt_mod = b.createModule(.{
        .root_source_file = b.path("libs/stbtt/stbtt.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const stbtt_test = b.addTest(.{ .root_module = stbtt_mod });
    check_step.dependOn(&stbtt_test.step);
    test_step.dependOn(&b.addRunArtifact(stbtt_test).step);

    stbtt_mod.addIncludePath(stb_dep.path("."));
    stbtt_mod.addCSourceFile(.{
        .file = b.path("libs/stbtt/stbtt.c"),
        .flags = &.{ "-std=c99", "-fno-sanitize=undefined" },
    });

    // --- STB_RECT_PACK Module ---
    const stbrp_mod = b.createModule(.{
        .root_source_file = b.path("libs/stbrp/stbrp.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const stbrp_test = b.addTest(.{ .root_module = stbrp_mod });
    check_step.dependOn(&stbrp_test.step);
    test_step.dependOn(&b.addRunArtifact(stbrp_test).step);

    stbrp_mod.addIncludePath(stb_dep.path("."));
    stbrp_mod.addCSourceFile(.{
        .file = b.path("libs/stbrp/stbrp.c"),
        .flags = &.{ "-std=c99", "-fno-sanitize=undefined" },
    });

    const clay_mod = b.createModule(.{
        .root_source_file = b.path("libs/clay/clay.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const clay_test = b.addTest(.{ .root_module = clay_mod });
    check_step.dependOn(&clay_test.step);
    test_step.dependOn(&b.addRunArtifact(clay_test).step);

    clay_mod.addIncludePath(clay_dep.path("."));
    clay_mod.addCSourceFile(.{
        .file = b.path("libs/clay/clay.c"),
        .flags = &.{ "-std=c99", "-fno-sanitize=undefined" },
    });

    // --- tilefiledialogs module ---
    const nfd_mod = b.createModule(.{
        .root_source_file = b.path("libs/nfd.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    const nfd_test = b.addTest(.{ .root_module = nfd_mod });
    check_step.dependOn(&nfd_test.step);
    test_step.dependOn(&b.addRunArtifact(nfd_test).step);

    nfd_mod.addIncludePath(nfd_dep.path("src/include"));
    nfd_mod.addIncludePath(nfd_dep.path("src/"));
    nfd_mod.addCSourceFile(.{
        .file = nfd_dep.path("src/nfd_common.c"),
        .flags = &.{ "-std=c99", "-fno-sanitize=undefined" },
    });

    // --- GLFW Module (Zig bindings) ---
    const glfw_mod = b.createModule(.{
        .root_source_file = b.path("libs/glfw.zig"),
        .target = target,
        .optimize = optimize,
    });

    // --- GLFW Static Library (compiling the C source) ---
    const glfw_lib = b.addLibrary(.{
        .name = "glfw",
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
        .linkage = .static,
    });

    glfw_mod.linkLibrary(glfw_lib);

    glfw_lib.addIncludePath(glfw_dep.path("include"));
    glfw_lib.addCSourceFiles(.{
        .files = &.{
            "context.c",     "init.c",          "input.c",
            "monitor.c",     "vulkan.c",        "window.c",
            "platform.c",    "null_init.c",     "null_monitor.c",
            "null_window.c", "null_joystick.c",
        },
        .root = glfw_dep.path("src"),
    });
    // --- Gltf loader module ---
    const gltf_mod = b.createModule(.{
        .root_source_file = b.path("libs/gltf.zig"),
        .target = target,
        .optimize = optimize,
    });

    // --- Main Executable ---
    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "zgpu",
        .root_module = exe_mod,
    });

    const exe_test = b.addTest(.{ .root_module = exe_mod });
    check_step.dependOn(&exe_test.step);
    test_step.dependOn(&b.addRunArtifact(exe_test).step);

    exe_mod.addImport("wgpu", wgpu_mod);
    exe_mod.addImport("stbi", stbi_mod);
    exe_mod.addImport("stbtt", stbtt_mod);
    exe_mod.addImport("stbrp", stbrp_mod);
    exe_mod.addImport("clay", clay_mod);
    exe_mod.addImport("nfd", nfd_mod);
    exe_mod.addImport("glfw", glfw_mod);
    exe_mod.addImport("gltf", gltf_mod);
    exe_mod.addAnonymousImport("shaders/GltfMultiPurpose.wgsl", .{
        .root_source_file = b.path("static/shaders/GltfMultiPurpose.wgsl"),
    });
    exe_mod.addAnonymousImport("shaders/Renderer.wgsl", .{
        .root_source_file = b.path("static/shaders/Renderer.wgsl"),
    });
    exe_mod.addAnonymousImport("shaders/GridMesher.wgsl", .{
        .root_source_file = b.path("static/shaders/GridMesher.wgsl"),
    });
    exe_mod.addAnonymousImport("shaders/GridRender.wgsl", .{
        .root_source_file = b.path("static/shaders/GridRender.wgsl"),
    });
    exe_mod.addAnonymousImport("shaders/TerrainMesher.wgsl", .{
        .root_source_file = b.path("static/shaders/TerrainMesher.wgsl"),
    });
    exe_mod.addAnonymousImport("shaders/TerrainRender.wgsl", .{
        .root_source_file = b.path("static/shaders/TerrainRender.wgsl"),
    });

    b.installArtifact(exe);

    // --- Run Step ---
    const run_cmd = b.addRunArtifact(exe);
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    run_step.dependOn(&run_cmd.step);

    // --- Platform-specifics ---
    if (is_windows) {
        glfw_lib.root_module.addCMacro("_GLFW_WIN32", "");
        glfw_lib.addCSourceFiles(.{
            .files = &.{
                "win32_init.c",     "win32_joystick.c", "win32_monitor.c", "win32_thread.c",
                "win32_time.c",     "win32_window.c",   "wgl_context.c",   "egl_context.c",
                "osmesa_context.c", "win32_module.c",
            },
            .root = glfw_dep.path("src"),
        });

        exe.linkSystemLibrary("dxgi");
        exe.linkSystemLibrary("d3d12");

        exe.linkSystemLibrary("gdi32");
        exe.linkSystemLibrary("shell32");
        exe.linkSystemLibrary("user32");
        exe.linkSystemLibrary("kernel32");
        exe.linkSystemLibrary("ntdll");

        const dll_needed = wgpu_dep.path("lib/wgpu_native.dll");
        const install = b.addInstallFile(dll_needed, "bin/wgpu_native.dll");
        install_step.dependOn(&install.step);

        exe.linkSystemLibrary("ole32");
        nfd_mod.addCSourceFile(.{
            .file = nfd_dep.path("src/nfd_win.cpp"),
            .flags = &.{"-fno-sanitize=undefined"},
        });
    } else {
        addSystemCFlags(b, glfw_lib.root_module);

        glfw_lib.root_module.addCMacro("_GLFW_X11", "");
        glfw_lib.addCSourceFiles(.{
            .files = &.{
                "x11_init.c",       "x11_monitor.c",    "x11_window.c",  "xkb_unicode.c",
                "posix_thread.c",   "posix_time.c",     "glx_context.c", "egl_context.c",
                "osmesa_context.c", "linux_joystick.c", "posix_poll.c",  "posix_module.c",
            },
            .root = glfw_dep.path("src"),
        });

        // for wgpu
        exe.linkSystemLibrary("m");
        exe.linkSystemLibrary("dl");

        // for glfw
        exe.linkSystemLibrary("X11");
        exe.linkSystemLibrary("Xrandr");
        exe.linkSystemLibrary("Xinerama");
        exe.linkSystemLibrary("Xi");
        exe.linkSystemLibrary("Xcursor");

        // for nfd
        exe.linkSystemLibrary("gtk-3");
        exe.linkSystemLibrary("glib-2.0");
        exe.linkSystemLibrary("gobject-2.0");

        nfd_mod.addCSourceFile(.{
            .file = nfd_dep.path("src/nfd_gtk.c"),
            .flags = &.{ "-std=c99", "-fno-sanitize=undefined" },
        });

        addSystemCFlags(b, nfd_mod);
    }
}
