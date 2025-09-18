const std = @import("std");
pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const run_step = b.step("run", "Run the app");
    const check_step = b.step("check", "Semantic analysis");
    const test_step = b.step("test", "Run tests");

    const wgpu_dep = switch (target.result.os.tag) {
        .linux => b.lazyDependency("wgpu_native_linux_x64", .{}).?,
        else => @panic("NYI"),
    };
    const stb_dep = b.dependency("stb", .{});

    const wgpu_mod = b.createModule(.{
        .root_source_file = b.path("libs/wgpu.zig"),
        .target = target,
        .optimize = optimize,
    });

    const wgpu_test = b.addTest(.{
        .root_module = wgpu_mod,
    });

    check_step.dependOn(&wgpu_test.step);
    test_step.dependOn(&b.addRunArtifact(wgpu_test).step);

    wgpu_mod.addLibraryPath(wgpu_dep.path("lib/"));
    wgpu_mod.linkSystemLibrary("wgpu_native", .{});
    wgpu_mod.linkSystemLibrary("m", .{});
    wgpu_mod.linkSystemLibrary("dl", .{});

    const stbi_mod = b.createModule(.{
        .root_source_file = b.path("libs/stbi/stbi.zig"),
        .target = target,
        .optimize = optimize,
    });

    const stbi_test = b.addTest(.{
        .root_module = stbi_mod,
    });

    check_step.dependOn(&stbi_test.step);
    test_step.dependOn(&b.addRunArtifact(stbi_test).step);

    stbi_mod.addIncludePath(stb_dep.path("."));
    stbi_mod.addCSourceFile(.{
        .file = b.path("libs/stbi/stbi.c"),
        .flags = if (optimize == .Debug) &.{
            "-std=c99",
            "-fno-sanitize=undefined",
            "-g",
            "-O0",
        } else &.{
            "-std=c99",
            "-fno-sanitize=undefined",
            "-O2",
        },
    });

    const glfw_mod = b.createModule(.{
        .root_source_file = b.path("libs/glfw.zig"),
        .target = target,
        .optimize = optimize,
    });

    const glfw_test = b.addTest(.{
        .root_module = glfw_mod,
    });

    check_step.dependOn(&glfw_test.step);
    test_step.dependOn(&b.addRunArtifact(glfw_test).step);

    glfw_mod.linkSystemLibrary("glfw", .{});

    if (target.result.os.tag == .emscripten) {
        wgpu_mod.addCMacro("WGPU_EMSCRIPTEN", @panic("NYI"));
        stbi_mod.addIncludePath(.{
            .cwd_relative = b.pathJoin(&.{ b.sysroot.?, "/include" }),
        });
    } else {
        wgpu_mod.link_libc = true;
        stbi_mod.link_libc = true;
        glfw_mod.link_libc = true; // TODO: is this needed?
    }

    const exe_mod = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "wgpu_zig",
        .root_module = exe_mod,
    });

    exe_mod.addImport("wgpu", wgpu_mod);
    exe_mod.addImport("stbi", stbi_mod);
    exe_mod.addImport("glfw", glfw_mod);

    const exe_unit_tests = b.addTest(.{
        .root_module = exe_mod,
    });

    check_step.dependOn(&exe_unit_tests.step);
    test_step.dependOn(&b.addRunArtifact(exe_unit_tests).step);

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    run_step.dependOn(&run_cmd.step);
}
