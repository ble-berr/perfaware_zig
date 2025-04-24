const std = @import("std");

pub fn compile(
    input_file_path: []const u8,
    output_file_path: []const u8,
    allocator: std.mem.Allocator,
) !void {
    const argv = [_][]const u8{
        "nasm",
        "-O0",
        "-w-prefix-lock",
        "--before",
        "bits 16",
        "-o",
        output_file_path,
        input_file_path,
    };
    var nasm = std.process.Child.init(argv[0..], allocator);

    switch (try nasm.spawnAndWait()) {
        .Exited => |exit_code| if (exit_code != 0) return error.NasmError,
        else => return error.NasmError,
    }
}
