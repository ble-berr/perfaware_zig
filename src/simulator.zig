const decode = @import("decode.zig");
const std = @import("std");
const dis = @import("disassembler.zig");

const printInstruction = dis.printInstruction;

const Instruction = decode.Instruction;
const SegmentRegister = decode.SegmentRegister;

var stdout: std.fs.File.Writer = undefined;

const Machine = struct {
    const Flags = struct {
        trap: bool,
        direction: bool,
        interrupt_enable: bool,
        overflow: bool,
        sign: bool,
        zero: bool,
        auxiliary_carry: bool,
        parity: bool,
        carry: bool,
    };

    memory: [1024 * 1024]u8,
    instruction_pointer: u16,
    registers: [8]u16,
    segment_registers: [4]u16,
    flags: Flags,

    fn reset(m: *Machine) void {
        m.*.memory = .{0} ** m.memory.len;
        m.*.registers = .{0} ** m.registers.len;
        m.*.segment_registers = .{0} ** m.segment_registers.len;
        m.*.instruction_pointer = 0;
        m.*.flags = .{
            .trap = false,
            .direction = false,
            .interrupt_enable = false,
            .overflow = false,
            .sign = false,
            .zero = false,
            .auxiliary_carry = false,
            .parity = false,
            .carry = false,
        };
    }

    fn ptrFromRegister(m: *Machine, r: decode.Register) Ptr {
        const halves: *[8]u8 = @ptrCast(&m.*.registers);

        return switch (r) {
            .ax => .{ .word = &m.*.registers[0] },
            .al => .{ .byte = &halves.*[0] },
            .ah => .{ .byte = &halves.*[1] },
            .cx => .{ .word = &m.*.registers[1] },
            .cl => .{ .byte = &halves.*[2] },
            .ch => .{ .byte = &halves.*[3] },
            .dx => .{ .word = &m.*.registers[2] },
            .dl => .{ .byte = &halves.*[4] },
            .dh => .{ .byte = &halves.*[5] },
            .bx => .{ .word = &m.*.registers[3] },
            .bl => .{ .byte = &halves.*[6] },
            .bh => .{ .byte = &halves.*[7] },
            .sp => .{ .word = &m.*.registers[4] },
            .bp => .{ .word = &m.*.registers[5] },
            .si => .{ .word = &m.*.registers[6] },
            .di => .{ .word = &m.*.registers[7] },
        };
    }
};

var machine: Machine = undefined;

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
        .register => |r| machine.ptrFromRegister(r),
        .direct_address => unreachable, // TODO
        .effective_address => unreachable, // TODO
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
        .register => |r| machine.ptrFromRegister(r).value(),
        .direct_address => unreachable, // TODO
        .effective_address => unreachable, // TODO
        .immediate_byte => |b| b,
        .immediate_word => |w| w,
        .short_jump => unreachable, // use another function?
        .near_jump => unreachable, // use another function?
        .far_jump => unreachable, // use another function?
        .segment => |sr| machine.segment_registers[@intFromEnum(sr)],
    };
}

fn printChange(op: decode.InstructionOperand, before: u16, after: u16) !void {
    var name: []const u8 = undefined;
    var width: decode.OperandWidth = undefined;

    switch (op) {
        .register => |register| switch (register) {
            .al, .ah, .cl, .ch, .dl, .dh, .bl, .bh => |r| {
                name = @tagName(r);
                width = .byte;
            },
            .ax, .cx, .dx, .bx, .sp, .bp, .si, .di => |r| {
                name = @tagName(r);
                width = .word;
            }
        },
        .segment => |sr| {
            name = @tagName(sr);
            width = .word;
        },
        else => unreachable, // TODO
    }

    switch (width) {
        .byte => try std.fmt.format(stdout, "{s}:{x:0>2}->{x:0>2}", .{
            name,
            @as(u8, @truncate(before)),
            @as(u8, @truncate(after)),
        }),
        .word => try std.fmt.format(stdout, "{s}:{x:0>4}->{x:0>4}", .{
            name, before, after,
        }),
    }
}

fn printFlags(flags: Machine.Flags) !void {
    if (flags.trap) {
        try stdout.writeAll("T");
    }
    if (flags.direction) {
        try stdout.writeAll("D");
    }
    if (flags.interrupt_enable) {
        try stdout.writeAll("I");
    }
    if (flags.overflow) {
        try stdout.writeAll("O");
    }
    if (flags.sign) {
        try stdout.writeAll("S");
    }
    if (flags.zero) {
        try stdout.writeAll("Z");
    }
    if (flags.auxiliary_carry) {
        try stdout.writeAll("A");
    }
    if (flags.parity) {
        try stdout.writeAll("P");
    }
    if (flags.carry) {
        try stdout.writeAll("C");
    }
}

fn printFlagChange(before: Machine.Flags, after: Machine.Flags) !void {
    try stdout.writeAll(" flags:");
    try printFlags(before);
    try stdout.writeAll("->");
    try printFlags(after);
}

fn processMov(instruction: Instruction) !void {
    const dst = ptrFromOperand(instruction.dst);
    const src = valueFromOperand(instruction.src);

    try stdout.writeAll(" ; ");
    try printChange(instruction.dst, dst.value(), src);

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

    try stdout.writeAll(" ; ");
    try printChange(instruction.dst, dst.value(), src);

    const prev_flags = machine.flags;

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

    try printFlagChange(prev_flags, machine.flags);
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
    try printInstruction(stdout, instruction, machine.instruction_pointer);
    switch (instruction.type) {
        .mov => try processMov(instruction),
        .sub => try processSub(instruction),
        .add => try processAdd(instruction),
        else => return error.UnsupportedInstruction,
    }
    try stdout.writeAll("\n");
}

fn dumpRegister(writer: anytype, register: decode.Register) !void {
    try std.fmt.format(writer, "{s}: 0x{x:0>4} ({1d})\n", .{
        @tagName(register),
        machine.ptrFromRegister(register).value(),
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
    try printFlags(machine.flags);
    try writer.writeAll("\n");

    try writer.writeAll("-- Segment Registers --\n");
    try dumpSegmentRegister(writer, .es);
    try dumpSegmentRegister(writer, .cs);
    try dumpSegmentRegister(writer, .ss);
    try dumpSegmentRegister(writer, .ds);

    try writer.writeAll("========================\n");
}

fn runProgram(program_reader: anytype) !void {
    machine.reset();

    const program_len = try program_reader.readAll(machine.memory[0..]);
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

fn testFromFile(program_file_path: []const u8) !void {
    if (!@import("builtin").is_test) {
        @compileError("testFromFile can only be used for testing.");
    }

    const out = std.io.getStdErr();
    stdout = out.writer();

    try std.fmt.format(stdout, ";;; {s}\n", .{program_file_path});

    var program_file = blk: {
        const cwd_path = try std.process.getCwdAlloc(std.testing.allocator);
        defer std.testing.allocator.free(cwd_path);
        var cwd = try std.fs.openDirAbsolute(cwd_path, .{});
        defer cwd.close();
        break :blk try cwd.openFile(program_file_path, .{ .mode = .read_only });
    };
    defer program_file.close();

    try runProgram(program_file.reader());
    try dumpMemory(stdout);
}

test "listing_0044_simulate" {
    try testFromFile("course_material/perfaware/part1/listing_0044_register_movs");

    const expected_registers = [machine.registers.len]u16{
        0x0004, // ax
        0x0002, // cx
        0x0001, // dx
        0x0003, // bx
        0x0001, // sp
        0x0002, // bp
        0x0003, // si
        0x0004, // di
    };

    try std.testing.expectEqualSlices(u16, &expected_registers, &machine.registers);
}

test "listing_0045_simulate" {
    try testFromFile("course_material/perfaware/part1/listing_0045_challenge_register_movs");

    const expected_registers = [machine.registers.len]u16{
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

    try std.testing.expectEqualSlices(u16, &expected_registers, &machine.registers);
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
    try std.io.getStdErr().writer().writeAll(";;; test sub\n");
    for (program) |instruction| {
        try processInstruction(instruction);
    }

    try std.testing.expectEqual(@as(u16, 0x4321), machine.registers[0]);
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
    try std.io.getStdErr().writer().writeAll(";;; test add\n");
    for (program) |instruction| {
        try processInstruction(instruction);
    }

    try std.testing.expectEqual(@as(u16, 0x6789), machine.registers[0]);
}

pub fn main() !void {
    stdout = std.io.getStdOut().writer();
    try runProgram(std.io.getStdIn().reader());
    try dumpMemory(stdout);
}
