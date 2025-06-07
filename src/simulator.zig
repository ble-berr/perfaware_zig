const machine = @import("machine.zig");
const decode = @import("decode.zig");
const std = @import("std");

const Instruction = decode.Instruction;
const Register = decode.Register;
const SegmentRegister = decode.SegmentRegister;

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
            .word = &machine.word_registers[@as(u3, @truncate(@intFromEnum(r)))],
        },
    };
}

fn getDestination(dst_operand: decode.InstructionOperand) !Ptr {
    return switch (dst_operand) {
        .none => unreachable,
        .register => |r| getRegister(r),
        .segment => |sr| .{ .word = &machine.segment_registers[@intFromEnum(sr)] },
        .immediate_byte, .immediate_word, .one => unreachable,
        else => error.UnsupportedDestination,
    };
}

fn getSource(src_operand: decode.InstructionOperand) !Value {
    return switch (src_operand) {
        .none => unreachable,
        .register => |r| switch (getRegister(r)) {
            .byte => |b| .{ .byte = b.* },
            .word => |w| .{ .word = w.* },
        },
        .segment => |sr| .{ .word = machine.segment_registers[@intFromEnum(sr)] },
        .immediate_byte => |byte| .{ .byte = byte },
        .immediate_word => |word| .{ .word = word },
        .one => .{ .byte = 1 },
        else => error.UnsupportedInstruction,
    };
}

fn processMov(instruction: Instruction) !void {
    const dst = try getDestination(instruction.dst);
    const src = try getSource(instruction.src);

    // Output debug info
    {
        const dst_name: []const u8 = switch (instruction.dst) {
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

// TODO(benjamin): overflow and auxiliary carry flags
fn processSub(instruction: Instruction) !void {
    const dst = try getDestination(instruction.dst);
    const src = try getSource(instruction.src);

    // Output debug info
    {
        const dst_name: []const u8 = switch (instruction.dst) {
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
        .byte => |dst_byte| {
            const src_byte = switch (src) {
                .byte => |b| b,
                .word => return error.WordToByte,
            };

            machine.flags.carry = (dst_byte.* < src_byte);

            dst_byte.* -= src_byte;

            machine.flags.zero = (dst_byte.* == 0);
            machine.flags.sign = ((dst_byte.* & 0x80) != 0);
            machine.flags.parity = ((@popCount(dst_byte.*) % 2) != 0);
        },
        .word => |dst_word| {
            const src_word = switch (src) {
                .byte => |b| @as(u16, b),
                .word => |w| w,
            };

            machine.flags.carry = (dst_word.* < src_word);

            dst_word.* -= src_word;

            machine.flags.zero = (dst_word.* == 0);
            machine.flags.sign = ((dst_word.* & 0x80) != 0);
            machine.flags.parity = ((@popCount(dst_word.*) % 2) != 0);
        },
    }
}

// TODO(benjamin): overflow and auxiliary carry flags
fn processAdd(instruction: Instruction) !void {
    const dst = try getDestination(instruction.dst);
    const src = try getSource(instruction.src);

    // Output debug info
    {
        const dst_name: []const u8 = switch (instruction.dst) {
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
        .byte => |dst_byte| {
            const src_byte = switch (src) {
                .byte => |b| b,
                .word => return error.WordToByte,
            };

            machine.flags.carry = (dst_byte.* > src_byte);

            dst_byte.* += src_byte;

            machine.flags.zero = (dst_byte.* == 0);
            machine.flags.sign = ((dst_byte.* & 0x80) != 0);
            machine.flags.parity = ((@popCount(dst_byte.*) % 2) != 0);
        },
        .word => |dst_word| {
            const src_word = switch (src) {
                .byte => |b| @as(u16, b),
                .word => |w| w,
            };

            machine.flags.carry = (dst_word.* > src_word);

            dst_word.* += src_word;

            machine.flags.zero = (dst_word.* == 0);
            machine.flags.sign = ((dst_word.* & 0x8000) != 0);
            machine.flags.parity = ((@popCount(dst_word.* & 0x00ff) % 2) != 0);
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

fn dumpRegister(writer: anytype, register: decode.Register) !void {
    try std.fmt.format(writer, "{s}: 0x{x:0>4} ({1d})\n", .{
        @tagName(register),
        machine.word_registers[@as(u3, @truncate(@intFromEnum(register)))],
    });
}

fn dumpSegmentRegister(writer: anytype, register: decode.SegmentRegister) !void {
    try std.fmt.format(
        writer,
        "{s}: 0x{x:0>4} ({1d})\n",
        .{ @tagName(register), machine.segment_registers[@intFromEnum(register)] },
    );
}

fn dumpMemory(writer: anytype) !void {
    try writer.writeAll("===== 8086 memdump =====\n");

    try writer.writeAll("-- Registers --\n");
    try dumpRegister(writer, .ax);
    try dumpRegister(writer, .bx);
    try dumpRegister(writer, .cx);
    try dumpRegister(writer, .dx);
    try dumpRegister(writer, .sp);
    try dumpRegister(writer, .bp);
    try dumpRegister(writer, .si);
    try dumpRegister(writer, .di);

    try writer.writeAll("-- Flags --\n");
    if (machine.flags.trap) {
        try writer.writeAll("T");
    }
    if (machine.flags.direction) {
        try writer.writeAll("D");
    }
    if (machine.flags.interrupt_enable) {
        try writer.writeAll("I");
    }
    if (machine.flags.overflow) {
        try writer.writeAll("O");
    }
    if (machine.flags.sign) {
        try writer.writeAll("S");
    }
    if (machine.flags.zero) {
        try writer.writeAll("Z");
    }
    if (machine.flags.auxiliary_carry) {
        try writer.writeAll("A");
    }
    if (machine.flags.parity) {
        try writer.writeAll("P");
    }
    if (machine.flags.carry) {
        try writer.writeAll("C");
    }
    try writer.writeAll("\n");

    try writer.writeAll("-- Segment Registers --\n");
    try dumpSegmentRegister(writer, .es);
    try dumpSegmentRegister(writer, .cs);
    try dumpSegmentRegister(writer, .ss);
    try dumpSegmentRegister(writer, .ds);

    try writer.writeAll("========================\n");
}

fn simulateProgram(reader: anytype) !void {
    machine.reset();

    const program_len = try reader.readAll(machine.memory[0..]);
    const code_segment = blk: {
        const code_segment_address = machine.segment_registers[@intFromEnum(SegmentRegister.cs)];
        break :blk machine.memory[code_segment_address .. code_segment_address + 0xffff];
    };

    while (true) {
        if (code_segment.len < machine.instruction_pointer) {
            return error.ProgramOverflow;
        } else if (machine.instruction_pointer == program_len) {
            // implicit halt for development convenience.
            return;
        }

        const window = code_segment[machine.instruction_pointer..];

        const decoded = try decode.decodeNext(window);

        machine.instruction_pointer += decoded.instruction.length;

        processInstruction(decoded.instruction) catch |err| {
            std.debug.print("{s}: {s} {s} {s}\n", .{
                @errorName(err),
                @tagName(decoded.instruction.type),
                @tagName(decoded.instruction.dst),
                @tagName(decoded.instruction.src),
            });
            return err;
        };
    }
}

fn simulate_test_program(program_file_path: []const u8) !void {
    if (!@import("builtin").is_test) {
        @compileError("simulate_test_program can only be used for testing.");
    }

    var program_file = blk: {
        const cwd_path = try std.process.getCwdAlloc(std.testing.allocator);
        defer std.testing.allocator.free(cwd_path);
        var cwd = try std.fs.openDirAbsolute(cwd_path, .{});
        defer cwd.close();
        break :blk try cwd.openFile(program_file_path, .{ .mode = .read_only });
    };
    defer program_file.close();

    try simulateProgram(program_file.reader());
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
        try processInstruction(instruction);
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
        try processInstruction(instruction);
    }

    try std.testing.expectEqual(@as(u16, 0x6789), machine.word_registers[0]);
}

pub fn main() !void {
    try simulateProgram(std.io.getStdIn().reader());
    try dumpMemory(std.io.getStdOut().writer());
}
