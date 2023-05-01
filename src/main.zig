const machine = @import("machine.zig");
const decode = @import("decode.zig");
const std = @import("std");

const Instruction = decode.Instruction;
const Register = decode.Register;
const SegmentRegister = decode.SegmentRegister;

const Emulator = struct {
    const Ptr = union(decode.OperandWidth) {
        byte: *u8,
        word: *u16,
    };

    const Value = union(decode.OperandWidth) {
        byte: u8,
        word: u16,
    };

    fn getRegister(register: Register) Ptr {
        return switch (register) {
            .al => .{ .byte = &machine.byte_registers[0] },
            .ah => .{ .byte = &machine.byte_registers[1] },
            .cl => .{ .byte = &machine.byte_registers[2] },
            .ch => .{ .byte = &machine.byte_registers[3] },
            .dl => .{ .byte = &machine.byte_registers[4] },
            .dh => .{ .byte = &machine.byte_registers[5] },
            .bl => .{ .byte = &machine.byte_registers[6] },
            .bh => .{ .byte = &machine.byte_registers[7] },
            else => |r| .{
                .word = &machine.word_registers[@truncate(u3, @enumToInt(r))],
            },
        };
    }

    fn getDestination(dst_operand: decode.InstructionOperand) !Ptr {
        return switch (dst_operand) {
            .register => |r| getRegister(r),
            .segment => |sr| .{ .word = &machine.segment_registers[@enumToInt(sr)] },
            .immediate_byte, .immediate_word, .one => unreachable,
            else => error.UnsupportedDestination,
        };
    }

    fn getSource(src_operand: decode.InstructionOperand) !Value {
        return switch (src_operand) {
            .register => |r| switch (getRegister(r)) {
                .byte => |b| .{ .byte = b.* },
                .word => |w| .{ .word = w.* },
            },
            .segment => |sr| .{ .word = machine.segment_registers[@enumToInt(sr)] },
            .immediate_byte => |byte| .{ .byte = byte },
            .immediate_word => |word| .{ .word = word },
            .one => .{ .byte = 1 },
            else => error.UnsupportedInstruction,
        };
    }

    fn processMov(instruction: Instruction) !void {
        const dst = try getDestination(instruction.dst.?);
        const src = try getSource(instruction.src.?);

        // Output debug info
        {
            const dst_name: []const u8 = switch (instruction.dst.?) {
                .register => |r| @tagName(r),
                .segment => |s| @tagName(s),
                else => unreachable,
            };

            const dst_val: u16 = switch (dst) {
                .byte => |b| b.*,
                .word => |w| w.*,
            };
            const src_val: u16 = switch (src) {
                .byte => |b| b,
                .word => |w| w,
            };

            std.debug.print("{s}: 0x{x}->0x{x}\n", .{ dst_name, dst_val, src_val });
        }

        switch (dst) {
            .byte => |dst_byte| switch (src) {
                .byte => |src_byte| dst_byte.* = src_byte,
                .word => return error.WordToByte,
            },
            .word => |dst_word| switch (src) {
                .byte => |src_byte| dst_word.* = src_byte,
                .word => |src_word| dst_word.* = src_word,
            },
        }
    }

    fn processSub(instruction: Instruction) !void {
        const dst = try getDestination(instruction.dst.?);
        const src = try getSource(instruction.src.?);

        // Output debug info
        {
            const dst_name: []const u8 = switch (instruction.dst.?) {
                .register => |r| @tagName(r),
                .segment => |s| @tagName(s),
                else => unreachable,
            };

            const dst_val: u16 = switch (dst) {
                .byte => |b| b.*,
                .word => |w| w.*,
            };
            const src_val: u16 = switch (src) {
                .byte => |b| b,
                .word => |w| w,
            };

            std.debug.print("{s}: 0x{x}->0x{x}\n", .{ dst_name, dst_val, dst_val - src_val });
        }

        switch (dst) {
            .byte => |dst_byte| switch (src) {
                .byte => |src_byte| dst_byte.* -= src_byte,
                .word => return error.WordToByte,
            },
            .word => |dst_word| switch (src) {
                .byte => |src_byte| dst_word.* -= src_byte,
                .word => |src_word| dst_word.* -= src_word,
            },
        }
    }

    fn processAdd(instruction: Instruction) !void {
        const dst = try getDestination(instruction.dst.?);
        const src = try getSource(instruction.src.?);

        // Output debug info
        {
            const dst_name: []const u8 = switch (instruction.dst.?) {
                .register => |r| @tagName(r),
                .segment => |s| @tagName(s),
                else => unreachable,
            };

            const dst_val: u16 = switch (dst) {
                .byte => |b| b.*,
                .word => |w| w.*,
            };
            const src_val: u16 = switch (src) {
                .byte => |b| b,
                .word => |w| w,
            };

            std.debug.print("{s}: 0x{x}->0x{x}\n", .{ dst_name, dst_val, dst_val + src_val });
        }

        switch (dst) {
            .byte => |dst_byte| switch (src) {
                .byte => |src_byte| dst_byte.* += src_byte,
                .word => return error.WordToByte,
            },
            .word => |dst_word| switch (src) {
                .byte => |src_byte| dst_word.* += src_byte,
                .word => |src_word| dst_word.* += src_word,
            },
        }
    }

    fn processInstruction(instruction: Instruction) !void {
        return switch (instruction.type) {
            .mov => processMov(instruction),
            .sub => processSub(instruction),
            .add => processAdd(instruction),
            else => error.UnsupportedInstruction,
        };
    }

    fn dumpMemory(writer: anytype) !void {
        try writer.writeAll("===== sim8086 memdump =====\n");
        for (0..machine.word_registers.len) |i| {
            try std.fmt.format(writer, "{s}: 0x{x:0>4} ({1d})\n", .{
                @tagName(Register.fromInt(.word, @truncate(u3, i))),
                machine.word_registers[i],
            });
        }
        try writer.writeAll("---\n");
        for (0..machine.segment_registers.len) |i| {
            try std.fmt.format(writer, "{s}: 0x{x:0>4} ({1d})\n", .{
                @tagName(@intToEnum(SegmentRegister, i)),
                machine.segment_registers[i],
            });
        }
        try writer.writeAll("===========================\n");
    }
};

fn segmentPrefixMnemonic(opt_prefix: ?SegmentRegister) []const u8 {
    return switch (opt_prefix orelse return "") {
        .es => "es:",
        .cs => "cs:",
        .ss => "ss:",
        .ds => "ds:",
    };
}

fn printOperand(
    operand: decode.InstructionOperand,
    segment_override: ?SegmentRegister,
    writer: anytype,
) !void {
    switch (operand) {
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
                @bitCast(i16, ea.offset),
            },
        ),
        .immediate_byte => |byte| try std.fmt.format(writer, "byte {d}", .{byte}),
        .immediate_word => |word| try std.fmt.format(writer, "word {d}", .{word}),
        // NOTE(benjamin): 'nasm' does not account for the instruction
        // length so we must add it here.
        .short_jump => |jump| try std.fmt.format(writer, "${d:1}", .{@as(i16, jump) + 2}),
        // Hacky way to satisfy NASM, obviously assumes that we have only just decoded the instruction.
        .near_jump => |jump| try std.fmt.format(writer, "{d}", .{jump + @intCast(i16, machine.instruction_pointer)}),
        .far_jump => |jump| try std.fmt.format(writer, "{d}:{d}", .{ jump.sp, jump.ip }),
        .segment => |segment| try writer.writeAll(@tagName(segment)),
        .one => try writer.writeAll("1"),
    }
}

fn printInstruction(
    instruction: Instruction,
    prefixes: decode.InstructionPrefixes,
    writer: anytype,
) !void {
    if (prefixes.lock) {
        try writer.writeAll("lock ");
    }

    if (prefixes.repeat) |repeat| {
        try std.fmt.format(writer, "{s} ", .{@tagName(repeat)});
    }

    try writer.writeAll(@tagName(instruction.type));

    if (instruction.type == .out) {
        try writer.writeAll(" ");
        try printOperand(instruction.src.?, prefixes.segment, writer);

        try writer.writeAll(", ");
        try printOperand(instruction.dst.?, prefixes.segment, writer);
    } else {
        if (instruction.dst) |dst| {
            try writer.writeAll(" ");
            try printOperand(dst, prefixes.segment, writer);

            if (instruction.src) |src| {
                try writer.writeAll(", ");
                try printOperand(src, prefixes.segment, writer);
            }
        }
    }
}

fn decodeProgram(reader: anytype, writer: anytype, emulate: bool) !void {
    machine.reset();

    const program_len = try reader.readAll(machine.memory[0..]);
    const code_segment = blk: {
        const code_segment_address = machine.segment_registers[@enumToInt(SegmentRegister.cs)];
        break :blk machine.memory[code_segment_address .. code_segment_address + 0xffff];
    };

    while (true) {
        if (code_segment.len < machine.instruction_pointer) {
            return error.ProgramOverflow;
        } else if (machine.instruction_pointer == program_len) {
            // implicit halt for development convenience.
            return;
        }

        var window = code_segment[machine.instruction_pointer..];

        var decoded = try decode.decodeNext(window);

        machine.instruction_pointer += decoded.instruction.length;

        if (emulate) {
            Emulator.processInstruction(decoded.instruction) catch |err| {
                std.debug.print("{s}: {s} {s} {s}\n", .{
                    @errorName(err),
                    @tagName(decoded.instruction.type),
                    if (decoded.instruction.dst) |dst| @tagName(dst) else "(null)",
                    if (decoded.instruction.src) |src| @tagName(src) else "(null)",
                });
                return err;
            };
        } else {
            try printInstruction(decoded.instruction, decoded.prefixes, writer);
            try writer.writeAll(" ;");
            for (window[0..decoded.instruction.length]) |byte| {
                try std.fmt.format(writer, " 0x{x:0>2}", .{byte});
            }
            try writer.writeAll("\n");
        }
    }
}

fn test_decode(reference_file_path: []const u8, file_format: enum { @"asm", bin }) !void {
    if (!@import("builtin").is_test) {
        @compileError("test_decode can only be used for testing.");
    }

    const nasm = @import("nasm.zig");

    var allocator = std.testing.allocator;

    var cwd = blk: {
        var cwd_path = try std.process.getCwdAlloc(allocator);
        defer allocator.free(cwd_path);
        break :blk try std.fs.openDirAbsolute(cwd_path, .{});
    };
    defer cwd.close();

    cwd.makeDir("tests") catch {};

    var filename = std.fs.path.basename(reference_file_path);
    if (std.mem.lastIndexOfScalar(u8, filename, '.')) |dot_index| {
        filename = filename[0..dot_index];
    }

    const reference_bin_file = switch (file_format) {
        .@"asm" => compile_asm: {
            const bin_file_path = try std.fmt.allocPrint(
                allocator,
                "tests/{s}_ref.bin",
                .{filename},
            );
            defer allocator.free(bin_file_path);

            try nasm.compile(reference_file_path, bin_file_path, allocator);

            break :compile_asm try cwd.openFile(bin_file_path, .{ .mode = .read_only });
        },
        .bin => try cwd.openFile(reference_file_path, .{ .mode = .read_only }),
    };
    defer reference_bin_file.close();

    const test_bin_file = test_bin_file: {
        const test_asm_file_path = try std.fmt.allocPrint(
            allocator,
            "tests/{s}_test.asm",
            .{filename},
        );
        defer allocator.free(test_asm_file_path);

        {
            const test_asm_file = try cwd.createFile(test_asm_file_path, .{ .truncate = true });
            defer test_asm_file.close();

            try decodeProgram(reference_bin_file.reader(), test_asm_file.writer(), false);
        }

        const test_bin_file_path = try std.fmt.allocPrint(
            allocator,
            "tests/{s}_test.bin",
            .{filename},
        );
        defer allocator.free(test_bin_file_path);

        try nasm.compile(test_asm_file_path, test_bin_file_path, allocator);

        break :test_bin_file try cwd.openFile(test_bin_file_path, .{ .mode = .read_only });
    };
    defer test_bin_file.close();

    // compare files
    try reference_bin_file.seekTo(0);

    const reference_buf = try reference_bin_file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(reference_buf);

    const test_buf = try test_bin_file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(test_buf);

    try std.testing.expectEqualSlices(u8, reference_buf, test_buf);
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

fn simulate_test_program(program_file_path: []const u8) !void {
    if (!@import("builtin").is_test) {
        @compileError("test_simulation can only be used for testing.");
    }
    const allocator = std.testing.allocator;

    var cwd = blk: {
        var cwd_path = try std.process.getCwdAlloc(allocator);
        defer allocator.free(cwd_path);
        break :blk try std.fs.openDirAbsolute(cwd_path, .{});
    };
    defer cwd.close();

    var program_file = try cwd.openFile(program_file_path, .{ .mode = .read_only });

    try decodeProgram(program_file.reader(), std.io.null_writer, true);
}

test "listing_0044_simulate" {
    try simulate_test_program("course_material/perfaware/part1/listing_0044_register_movs");

    const expected_registers = [machine.word_registers.len]u16{
        0x0004, // ax
        0x0002, // cx
        0x0001, // dx
        0x0003, // bx
        0x0001, // sp
        0x0002, // bp
        0x0003, // si
        0x0004, // di
    };

    try std.testing.expectEqualSlices(u16, &expected_registers, &machine.word_registers);
}

test "listing_0045_simulate" {
    try simulate_test_program("course_material/perfaware/part1/listing_0045_challenge_register_movs");

    const expected_registers = [machine.word_registers.len]u16{
        0x4411, // ax
        0x6677, // cx
        0x7788, // dx
        0x3344, // bx
        0x4411, // sp
        0x3344, // bp
        0x6677, // si
        0x7788, // di
    };

    const expected_segment_registers = [machine.segment_registers.len]u16{
        0x6677, // es
        0x0000, // cs
        0x4411, // ss
        0x3344, // ds
    };

    try std.testing.expectEqualSlices(u16, &expected_registers, &machine.word_registers);
    try std.testing.expectEqualSlices(u16, &expected_segment_registers, &machine.segment_registers);
}

test "sub_simulate" {
    const program = [_]Instruction{
        .{
            .length = 0,
            .type = .mov,
            .dst = .{ .register = .ax },
            .src = .{ .immediate_word = 0xfedc },
        },
        .{
            .length = 0,
            .type = .sub,
            .dst = .{ .register = .ax },
            .src = .{ .immediate_word = 0x7654 },
        },
        .{
            .length = 0,
            .type = .sub,
            .dst = .{ .register = .al },
            .src = .{ .immediate_byte = 0x67 },
        },
        .{
            .length = 0,
            .type = .sub,
            .dst = .{ .register = .ah },
            .src = .{ .immediate_byte = 0x45 },
        },
    };

    machine.reset();
    for (program) |instruction| {
        try Emulator.processInstruction(instruction);
    }

    try std.testing.expectEqual(@as(u16, 0x4321), machine.word_registers[0]);
}

test "add_simulate" {
    const program = [_]Instruction{
        .{
            .length = 0,
            .type = .mov,
            .dst = .{ .register = .ax },
            .src = .{ .immediate_word = 0x4321 },
        },
        .{
            .length = 0,
            .type = .add,
            .dst = .{ .register = .ax },
            .src = .{ .immediate_word = 0x2008 },
        },
        .{
            .length = 0,
            .type = .add,
            .dst = .{ .register = .al },
            .src = .{ .immediate_byte = 0x60 },
        },
        .{
            .length = 0,
            .type = .add,
            .dst = .{ .register = .ah },
            .src = .{ .immediate_byte = 0x04 },
        },
    };

    machine.reset();
    for (program) |instruction| {
        try Emulator.processInstruction(instruction);
    }

    try std.testing.expectEqual(@as(u16, 0x6789), machine.word_registers[0]);
}

pub fn main() !void {
    var stdout = std.io.getStdOut().writer();
    var stdin = std.io.getStdIn().reader();

    const argv = std.os.argv;
    if (2 <= argv.len and argv[1][0] == 'e') {
        try decodeProgram(stdin, stdout, true);
        try Emulator.dumpMemory(stdout);
    } else {
        try decodeProgram(stdin, stdout, false);
    }
}
