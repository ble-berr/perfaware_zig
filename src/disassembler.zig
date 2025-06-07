const decode = @import("decode.zig");
const std = @import("std");

fn segmentPrefixMnemonic(opt_prefix: ?decode.SegmentRegister) []const u8 {
    return switch (opt_prefix orelse return "") {
        .es => "es:",
        .cs => "cs:",
        .ss => "ss:",
        .ds => "ds:",
    };
}

fn printOperand(
    writer: anytype,
    operand: decode.InstructionOperand,
    segment_override: ?decode.SegmentRegister,
    instruction_position: usize,
) !void {
    switch (operand) {
        .none => {},
        .register => |register| try writer.writeAll(@tagName(register)),
        .direct_address => |direct_address| try std.fmt.format(
            writer,
            "{s} {s}[{d}]",
            .{
                if (direct_address.width) |w| @tagName(w) else "",
                segmentPrefixMnemonic(segment_override),
                direct_address.address,
            },
        ),
        .effective_address => |ea| try std.fmt.format(
            writer,
            "{s} {s}[{s} {d:1}]",
            .{
                if (ea.width) |w| @tagName(w) else "",
                segmentPrefixMnemonic(segment_override),
                @tagName(ea.base),
                @as(i16, @bitCast(ea.offset)),
            },
        ),
        .immediate_byte => |byte| try std.fmt.format(writer, "byte {d}", .{byte}),
        .immediate_word => |word| try std.fmt.format(writer, "word {d}", .{word}),
        // NOTE(benjamin): NASM does not account for the instruction length so
        // we must add it here (short jumps are always encoded using 2 bytes).
        .short_jump => |jump| try std.fmt.format(writer, "${d:1}", .{@as(i16, jump) + 2}),
        // NOTE(benjamin): Hacky way to satisfy NASM, obviously assumes that we
        // have only just decoded the instruction.
        .near_jump => |jump| try std.fmt.format(writer, "{d}", .{jump + @as(i16, @intCast(instruction_position))}),
        .far_jump => |jump| try std.fmt.format(writer, "{d}:{d}", .{ jump.sp, jump.ip }),
        .segment => |segment| try writer.writeAll(@tagName(segment)),
        .one => try writer.writeAll("1"),
    }
}

fn printInstruction(
    writer: anytype,
    instruction: decode.Instruction,
    prefixes: decode.InstructionPrefixes,
    instruction_position: usize,
) !void {
    if (prefixes.lock) {
        try writer.writeAll("lock ");
    }

    if (prefixes.repeat) |repeat| {
        try std.fmt.format(writer, "{s} ", .{@tagName(repeat)});
    }

    try writer.writeAll(@tagName(instruction.type));

    switch (instruction.type) {
        .illegal => {},
        // NOTE(benjamin): src and dst are inverted in the assembler output for
        // "out"
        .out => {
            try writer.writeAll(" ");
            try printOperand(writer, instruction.src, prefixes.segment, 0);

            try writer.writeAll(", ");
            try printOperand(writer, instruction.dst, prefixes.segment, 0);
        },
        else => {
            if (instruction.dst != .none) {
                try writer.writeAll(" ");
                try printOperand(writer, instruction.dst, prefixes.segment, instruction_position);

                if (instruction.src != .none) {
                    try writer.writeAll(", ");
                    try printOperand(writer, instruction.src, prefixes.segment, instruction_position);
                }
            }
        }
    }
}

pub fn disassembleProgram(writer: anytype, program: []const u8) !void {
    var pos: usize = 0;
    while (pos < program.len) {
        const window = program[pos..];
        const decoded = try decode.decodeNext(window);

        pos += decoded.instruction.length;

        try printInstruction(writer, decoded.instruction, decoded.prefixes, pos);
        try writer.writeAll(" ;");
        for (window[0..decoded.instruction.length]) |byte| {
            try std.fmt.format(writer, " 0x{x:0>2}", .{byte});
        }
        try writer.writeAll("\n");
    }
}

// NOTE(benjamin): Code segment is only 64KiB long. This should be more
// than enough for the course so we'll ignore the possibility of not having
// enough space.
var program_buf = [_]u8{0} ** (64 * 1024);

pub fn main() !void {
    const program_len = try std.io.getStdIn().readAll(program_buf[0..]);

    try disassembleProgram(std.io.getStdOut().writer(), program_buf[0..program_len]);
}

fn test_decode(reference_file_path: []const u8, file_format: enum { @"asm", bin }) !void {
    if (!@import("builtin").is_test) {
        @compileError("test_decode can only be used for testing.");
    }

    const nasm = @import("nasm.zig");

    var allocator = std.testing.allocator;

    var cwd = blk: {
        const cwd_path = try std.process.getCwdAlloc(allocator);
        defer allocator.free(cwd_path);
        break :blk try std.fs.openDirAbsolute(cwd_path, .{});
    };
    defer cwd.close();

    cwd.makeDir("tests") catch {};

    var filename = std.fs.path.basename(reference_file_path);
    if (std.mem.lastIndexOfScalar(u8, filename, '.')) |dot_index| {
        filename = filename[0..dot_index];
    }

    const reference_program = blk: {
        switch (file_format) {
            .bin => {
                var file = try cwd.openFile(reference_file_path, .{ .mode = .read_only });
                defer file.close();

                break :blk try file.readToEndAlloc(allocator, std.math.maxInt(usize));
            },
            .@"asm" => {
                const bin_file_path = try std.fmt.allocPrint(
                    allocator,
                    "tests/{s}_ref.bin",
                    .{filename},
                );
                errdefer allocator.free(bin_file_path);

                try nasm.compile(reference_file_path, bin_file_path, allocator);

                var file = try cwd.openFile(bin_file_path, .{ .mode = .read_only });
                defer file.close();
                allocator.free(bin_file_path);

                break :blk try file.readToEndAlloc(allocator, std.math.maxInt(usize));
            },
        }
    };
    defer allocator.free(reference_program);

    // Disassemble the reference program and recompile it with NASM
    const rebuilt_program = blk: {
        const test_asm_file_path = try std.fmt.allocPrint(
            allocator,
            "tests/{s}_test.asm",
            .{filename},
        );
        defer allocator.free(test_asm_file_path);

        {
            const test_asm_file = try cwd.createFile(test_asm_file_path, .{ .truncate = true });
            defer test_asm_file.close();

            try disassembleProgram(test_asm_file.writer(), reference_program);
        }

        const test_bin_file_path = try std.fmt.allocPrint(
            allocator,
            "tests/{s}_test.bin",
            .{filename},
        );
        defer allocator.free(test_bin_file_path);

        try nasm.compile(test_asm_file_path, test_bin_file_path, allocator);

        var file = try cwd.openFile(test_bin_file_path, .{ .mode = .read_only });
        defer file.close();

        break :blk try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    };
    defer allocator.free(rebuilt_program);

    try std.testing.expectEqualSlices(u8, reference_program, rebuilt_program);
}

test "listing_0037_decode" {
    try test_decode("course_material/perfaware/part1/listing_0037_single_register_mov.asm", .@"asm");
}
test "listing_0038_decode" {
    try test_decode("course_material/perfaware/part1/listing_0038_many_register_mov.asm", .@"asm");
}
test "listing_0039_decode" {
    try test_decode("course_material/perfaware/part1/listing_0039_more_movs.asm", .@"asm");
}
test "listing_0040_decode" {
    try test_decode("course_material/perfaware/part1/listing_0040_challenge_movs.asm", .@"asm");
}
test "listing_0041_decode" {
    try test_decode("course_material/perfaware/part1/listing_0041_add_sub_cmp_jnz.asm", .@"asm");
}
test "listing_0042_decode" {
    try test_decode("course_material/perfaware/part1/listing_0042_completionist_decode.asm", .@"asm");
}
test "listing_0043_decode" {
    try test_decode("course_material/perfaware/part1/listing_0043_immediate_movs.asm", .@"asm");
}
test "listing_0044_decode" {
    try test_decode("course_material/perfaware/part1/listing_0044_register_movs.asm", .@"asm");
}
test "listing_0045_decode" {
    try test_decode("course_material/perfaware/part1/listing_0045_challenge_register_movs.asm", .@"asm");
}
test "listing_0046_decode" {
    try test_decode("course_material/perfaware/part1/listing_0046_add_sub_cmp.asm", .@"asm");
}
test "listing_0047_decode" {
    try test_decode("course_material/perfaware/part1/listing_0047_challenge_flags.asm", .@"asm");
}

test "jumps_decode" {
    try test_decode("testfiles/asm/jumps.asm", .@"asm");
}
test "hlt_decode" {
    try test_decode("testfiles/bin/hlt", .bin);
}
test "string_decode" {
    try test_decode("testfiles/bin/string", .bin);
}
