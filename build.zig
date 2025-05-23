const std = @import("std");

fn addOutputProgram(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.Mode,
    name: []const u8,
    root_source_file_path: []const u8,
) void {
    var step_name_buf: [256]u8 = undefined;
    var step_desc_buf: [256]u8 = undefined;
    const exe = b.addExecutable(.{
        .name = name,
        // In this case the main source file is merely a path, however, in more
        // complicated build scripts, this could be a generated file.
        .root_source_file = b.path(root_source_file_path),
        .target = target,
        .optimize = optimize,
    });

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a RunStep in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step(
        std.fmt.bufPrint(step_name_buf[0..], "run-{s}", .{name}) catch unreachable,
        std.fmt.bufPrint(step_desc_buf[0..], "Run {s}", .{name}) catch unreachable,
    );
    run_step.dependOn(&run_cmd.step);

    // Creates a step for unit testing.
    const unit_tests = b.addTest(.{
        .root_source_file = b.path(root_source_file_path),
        .target = target,
        .optimize = optimize,
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step(
        std.fmt.bufPrint(step_name_buf[0..], "test-{s}", .{name}) catch unreachable,
        std.fmt.bufPrint(step_desc_buf[0..], "Run unit tests for {s}", .{name}) catch unreachable,
    );
    test_step.dependOn(&run_unit_tests.step);
}

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    addOutputProgram(b, target, optimize, "8086asm", "src/disassembler.zig");
    addOutputProgram(b, target, optimize, "8086sim", "src/simulator.zig");
}
