const machine = @import("machine.zig");
const decode = @import("decode.zig");
const std = @import("std");

const Instruction = decode.Instruction;
const SegmentRegister = decode.SegmentRegister;

const Ptr = union(decode.OperandWidth) {
    byte: *u8,
    word: *u16,

    fn value(ptr: Ptr) u16 {
        return switch (ptr) {
            .byte => |b| b.*,
            .word => |w| w.*,
        };
    }
};

fn ptrFromOperand(op: decode.InstructionOperand) Ptr {
    return switch (op) {
        .none => unreachable,
        .register => |r| switch (r) {
            .al => .{ .byte = &machine.byte_registers[0] },
            .ah => .{ .byte = &machine.byte_registers[1] },
            .cl => .{ .byte = &machine.byte_registers[2] },
            .ch => .{ .byte = &machine.byte_registers[3] },
            .dl => .{ .byte = &machine.byte_registers[4] },
            .dh => .{ .byte = &machine.byte_registers[5] },
            .bl => .{ .byte = &machine.byte_registers[6] },
            .bh => .{ .byte = &machine.byte_registers[7] },
            else => |wr| .{
                .word = &machine.word_registers[@as(u3, @truncate(@intFromEnum(wr)))],
            },
        },
        .direct_address => unreachable, // TODO: implement
        .effective_address => unreachable, // TODO: implement
        .immediate_byte => unreachable, // illegal
        .immediate_word => unreachable, // illegal
        .short_jump => unreachable, // use another function?
        .near_jump => unreachable, // use another function?
        .far_jump => unreachable, // use another function?
        .segment => |sr| .{ .word = &machine.segment_registers[@intFromEnum(sr)] },
    };
}

fn valueFromOperand(op: decode.InstructionOperand) u16 {
    return switch (op) {
        .none => unreachable,
        .register => |r| switch (r) {
            .al => machine.byte_registers[0],
            .ah => machine.byte_registers[1],
            .cl => machine.byte_registers[2],
            .ch => machine.byte_registers[3],
            .dl => machine.byte_registers[4],
            .dh => machine.byte_registers[5],
            .bl => machine.byte_registers[6],
            .bh => machine.byte_registers[7],
            else => |wr| machine.word_registers[@as(u3, @truncate(@intFromEnum(wr)))],
        },
        .direct_address => unreachable, // TODO: implement
        .effective_address => unreachable, // TODO: implement
        .immediate_byte => |b| b,
        .immediate_word => |w| w,
        .short_jump => unreachable, // use another function?
        .near_jump => unreachable, // use another function?
        .far_jump => unreachable, // use another function?
        .segment => |sr| machine.segment_registers[@intFromEnum(sr)],
    };
}

fn processMov(instruction: Instruction) !void {
    const dst = ptrFromOperand(instruction.dst);
    const src = valueFromOperand(instruction.src);

    switch (dst) {
        .byte => |p| p.* = @truncate(src),
        .word => |p| p.* = src,
    }
}

fn processSub(instruction: Instruction) !void {
    const dst = ptrFromOperand(instruction.dst);
    const dst_val = dst.value();
    const src = valueFromOperand(instruction.src);
    const result = @as(u32, dst_val) -% src;

    switch (dst) {
        .byte => |ptr| {
            ptr.* = @truncate(result);

            machine.flags.carry = (dst_val < src); // ??????
            machine.flags.auxiliary_carry = false; // TODO
            machine.flags.zero = (ptr.* == 0);
            machine.flags.sign = (ptr.* > 0x80);
            machine.flags.parity = ((@popCount(ptr.*) % 2) == 0);
            machine.flags.overflow = (result > 0xff);
        },
        .word => |ptr| {
            ptr.* = @truncate(result);

            machine.flags.carry = (dst_val < src); // ??????
            machine.flags.auxiliary_carry = false; // TODO
            machine.flags.zero = (ptr.* == 0);
            machine.flags.sign = (ptr.* > 0x8000);
            machine.flags.parity = ((@popCount(ptr.* & 0xff) % 2) == 0);
            machine.flags.overflow = (result > 0xffff);
        },
    }
}

fn processAdd(instruction: Instruction) !void {
    const dst = ptrFromOperand(instruction.dst);
    const dst_val = dst.value();
    const src = valueFromOperand(instruction.src);
    const result = @as(u32, dst_val) +% src;

    switch (dst) {
        .byte => |ptr| {
            ptr.* = @truncate(result);

            machine.flags.carry = (dst_val > src); // ??????
            machine.flags.auxiliary_carry = false; // TODO
            machine.flags.zero = (ptr.* == 0);
            machine.flags.sign = (ptr.* > 0x80);
            machine.flags.parity = ((@popCount(ptr.*) % 2) == 0);
            machine.flags.overflow = (result > 0xff);
        },
        .word => |ptr| {
            ptr.* = @truncate(result);

            machine.flags.carry = (dst_val > src); // ??????
            machine.flags.auxiliary_carry = false; // TODO
            machine.flags.zero = (ptr.* == 0);
            machine.flags.sign = (ptr.* > 0x8000);
            machine.flags.parity = ((@popCount(ptr.* & 0xff) % 2) == 0);
            machine.flags.overflow = (result > 0xffff);
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

fn dumpSegmentRegister(writer: anytype, register: SegmentRegister) !void {
    try std.fmt.format(writer, "{s}: 0x{x:0>4} ({1d})\n", .{
        @tagName(register),
        machine.segment_registers[@intFromEnum(register)],
    });
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
    const code_segment_start = machine.segment_registers[@intFromEnum(SegmentRegister.cs)];
    const code_segment = machine.memory[code_segment_start .. code_segment_start + 0xffff];

    while (true) {
        if (code_segment.len < machine.instruction_pointer) {
            return error.ProgramOverflow;
        } else if (machine.instruction_pointer == program_len) {
            // implicit halt for development convenience.
            return;
        }

        const window = code_segment[machine.instruction_pointer..];

        const instruction = try decode.decodeNext(window);

        machine.instruction_pointer += instruction.length;

        processInstruction(instruction) catch |err| {
            std.debug.print("{s}: {s} {s} {s}\n", .{
                @errorName(err),
                @tagName(instruction.type),
                @tagName(instruction.dst),
                @tagName(instruction.src),
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
