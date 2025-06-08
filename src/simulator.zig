const decode = @import("decode.zig");
const std = @import("std");
const dis = @import("disassembler.zig");

const printInstruction = dis.printInstruction;

const Instruction = decode.Instruction;
const SegmentRegister = decode.SegmentRegister;

const Machine = struct {
    const Flags = struct {
        trap: bool = false,
        direction: bool = false,
        interrupt_enable: bool = false,
        overflow: bool = false,
        sign: bool = false,
        zero: bool = false,
        auxiliary_carry: bool = false,
        parity: bool = false,
        carry: bool = false,
    };

    const LogBuf = std.ArrayListAligned(u8, null);
    const LogWriter = std.io.Writer(*LogBuf, error{}, logWrite);

    memory: [1024 * 1024]u8 = .{0} ** (1024 * 1024),
    instruction_pointer: u16 = 0,
    registers: [8]u16 = .{0} ** (8),
    segment_registers: [4]u16 = .{0} ** (4),
    flags: Flags = .{},
    logbuf: LogBuf = LogBuf.init(std.heap.page_allocator),

    fn logWrite(logbuf: *LogBuf, bytes: []const u8) error{}!usize {
        logbuf.*.appendSlice(bytes) catch {};
        return bytes.len;
    }

    fn logger(m: *Machine) LogWriter {
        return .{ .context = &m.*.logbuf };
    }

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
        m.*.logbuf.clearRetainingCapacity();
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

    fn logState(m: *Machine) void {
        const writer = m.logger();

        writer.writeAll("===== 8086 memdump =====\n") catch {};

        writer.writeAll("-- Registers --\n") catch {};
        dumpRegister(writer, .ax) catch {};
        dumpRegister(writer, .bx) catch {};
        dumpRegister(writer, .cx) catch {};
        dumpRegister(writer, .dx) catch {};
        dumpRegister(writer, .sp) catch {};
        dumpRegister(writer, .bp) catch {};
        dumpRegister(writer, .si) catch {};
        dumpRegister(writer, .di) catch {};

        writer.writeAll("-- Flags --\n") catch {};
        printFlags(machine.flags) catch {};
        writer.writeAll("\n") catch {};

        writer.writeAll("-- Segment Registers --\n") catch {};
        dumpSegmentRegister(writer, .es) catch {};
        dumpSegmentRegister(writer, .cs) catch {};
        dumpSegmentRegister(writer, .ss) catch {};
        dumpSegmentRegister(writer, .ds) catch {};

        writer.writeAll("========================\n") catch {};
    }
};

var machine: Machine = .{};

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
        .byte => try std.fmt.format(machine.logger(), "{s}:0x{x:0>2}->0x{x:0>2}", .{
            name,
            @as(u8, @truncate(before)),
            @as(u8, @truncate(after)),
        }),
        .word => try std.fmt.format(machine.logger(), "{s}:0x{x:0>4}->0x{x:0>4}", .{
            name, before, after,
        }),
    }
}

fn printFlags(flags: Machine.Flags) !void {
    if (flags.carry) {
        try machine.logger().writeAll("C");
    }
    if (flags.parity) {
        try machine.logger().writeAll("P");
    }
    if (flags.auxiliary_carry) {
        try machine.logger().writeAll("A");
    }
    if (flags.zero) {
        try machine.logger().writeAll("Z");
    }
    if (flags.sign) {
        try machine.logger().writeAll("S");
    }
    if (flags.overflow) {
        try machine.logger().writeAll("O");
    }
    if (flags.interrupt_enable) {
        try machine.logger().writeAll("I");
    }
    if (flags.direction) {
        try machine.logger().writeAll("D");
    }
    if (flags.trap) {
        try machine.logger().writeAll("T");
    }
}

fn printFlagChange(before: Machine.Flags, after: Machine.Flags) !void {
    try machine.logger().writeAll(" flags:");
    try printFlags(before);
    try machine.logger().writeAll("->");
    try printFlags(after);
}

fn simMov(instruction: Instruction) !void {
    const dst = ptrFromOperand(instruction.dst);
    const src = valueFromOperand(instruction.src);

    try machine.logger().writeAll(" ; ");
    try printChange(instruction.dst, dst.value(), src);

    switch (dst) {
        .byte => |p| p.* = @truncate(src),
        .word => |p| p.* = src,
    }
}

fn simSub(instruction: Instruction) !void {
    const dst = ptrFromOperand(instruction.dst);
    const dst_val = dst.value();
    const src = valueFromOperand(instruction.src);
    var result = dst_val -% src;

    try machine.logger().writeAll(" ; ");
    try printChange(instruction.dst, dst.value(), @truncate(result));

    const prev_flags = machine.flags;

    var neg_limit: u16 = undefined;
    switch (dst) {
        .byte => |ptr| {
            ptr.* = @truncate(result);
            neg_limit = 0x80;
            result &= 0xff;
        },
        .word => |ptr| {
            ptr.* = result;
            neg_limit = 0x8000;
        },
    }

    const dst_sign = dst_val >= neg_limit;
    const src_sign = src >= neg_limit;
    const result_sign = result >= neg_limit;

    machine.flags.zero = result == 0;
    machine.flags.sign = result_sign;
    machine.flags.parity = (@popCount(result & 0xff) % 2) == 0;
    machine.flags.carry = result_sign and !dst_sign; // check this some more
    machine.flags.auxiliary_carry = false; // TODO
    if (src_sign) {
        machine.flags.overflow = !dst_sign and result_sign;
    } else {
        machine.flags.overflow = dst_sign and !result_sign;
    }

    try printFlagChange(prev_flags, machine.flags);
}

fn simCmp(instruction: Instruction) !void {
    const dst = ptrFromOperand(instruction.dst);
    const dst_val = dst.value();
    const src = valueFromOperand(instruction.src);
    var result = dst_val -% src;

    try machine.logger().writeAll(" ; ");

    const prev_flags = machine.flags;

    var neg_limit: u16 = undefined;
    switch (dst) {
        .byte => {
            neg_limit = 0x80;
            result &= 0xff;
        },
        .word => {
            neg_limit = 0x8000;
        },
    }

    const dst_sign = dst_val >= neg_limit;
    const src_sign = src >= neg_limit;
    const result_sign = result >= neg_limit;

    machine.flags.zero = result == 0;
    machine.flags.sign = result_sign;
    machine.flags.parity = (@popCount(result & 0xff) % 2) == 0;
    machine.flags.carry = result_sign and !dst_sign; // check this some more
    machine.flags.auxiliary_carry = false; // TODO
    if (src_sign) {
        machine.flags.overflow = !dst_sign and result_sign;
    } else {
        machine.flags.overflow = dst_sign and !result_sign;
    }

    try printFlagChange(prev_flags, machine.flags);
}

fn simAdd(instruction: Instruction) !void {
    const dst = ptrFromOperand(instruction.dst);
    const dst_val = dst.value();
    const src = valueFromOperand(instruction.src);
    var result = dst_val +% src;

    try machine.logger().writeAll(" ; ");
    try printChange(instruction.dst, dst.value(), @truncate(result));

    const prev_flags = machine.flags;

    var neg_limit: u16 = undefined;
    switch (dst) {
        .byte => |ptr| {
            ptr.* = @truncate(result);
            neg_limit = 0x80;
            result &= 0xff;
        },
        .word => |ptr| {
            ptr.* = result;
            neg_limit = 0x8000;
        },
    }

    const dst_sign = dst_val >= neg_limit;
    const src_sign = src >= neg_limit;
    const result_sign = result >= neg_limit;

    machine.flags.zero = result == 0;
    machine.flags.sign = result_sign;
    machine.flags.parity = (@popCount(result & 0xff) % 2) == 0;
    machine.flags.carry = result_sign and !dst_sign; // check this some more
    machine.flags.auxiliary_carry = false; // TODO
    if (src_sign) {
        machine.flags.overflow = dst_sign and !result_sign;
    } else {
        machine.flags.overflow = !dst_sign and result_sign;
    }

    try printFlagChange(prev_flags, machine.flags);
}

fn simInstruction(instruction: Instruction) !void {
    try printInstruction(machine.logger(), instruction, machine.instruction_pointer);
    switch (instruction.type) {
        .mov => try simMov(instruction),
        .sub => try simSub(instruction),
        .add => try simAdd(instruction),
        .cmp => try simCmp(instruction),
        else => return error.UnsupportedInstruction,
    }
    try machine.logger().writeAll("\n");
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

        simInstruction(instruction) catch |err| {
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

fn testFromFile(path: []const u8, expect: Machine) !void {
    if (!@import("builtin").is_test) {
        @compileError("testFromFile can only be used for testing.");
    }

    try std.fmt.format(machine.logger(), ";;; {s}\n", .{path});

    var program_file = blk: {
        const cwd_path = try std.process.getCwdAlloc(std.testing.allocator);
        defer std.testing.allocator.free(cwd_path);
        var cwd = try std.fs.openDirAbsolute(cwd_path, .{});
        defer cwd.close();
        break :blk try cwd.openFile(path, .{ .mode = .read_only });
    };
    defer program_file.close();

    errdefer {
        machine.logState();
        std.io.getStdErr().writeAll(machine.logbuf.items) catch {};
    }
    try runProgram(program_file.reader());
    try expectMachine(expect);
}

fn expectMachine(expect: Machine) !void {
    try std.testing.expectEqualSlices(u16, &expect.registers, &machine.registers);
    try std.testing.expectEqualSlices(u16, &expect.segment_registers, &machine.segment_registers);
    try std.testing.expectEqualDeep(expect.flags, machine.flags);
}

test "listing_0044_simulate" {
    const expect: Machine = .{
        .registers = .{
            0x0004, // ax
            0x0002, // cx
            0x0001, // dx
            0x0003, // bx
            0x0001, // sp
            0x0002, // bp
            0x0003, // si
            0x0004, // di
        },
        .logbuf = undefined,
    };
    try testFromFile("course_material/perfaware/part1/listing_0044_register_movs", expect);
}

test "listing_0045_simulate" {
    const expect: Machine = .{
        .registers = .{
            0x4411, // ax
            0x6677, // cx
            0x7788, // dx
            0x3344, // bx
            0x4411, // sp
            0x3344, // bp
            0x6677, // si
            0x7788, // di
        },
        .segment_registers = .{
            0x6677, // es
            0x0000, // cs
            0x4411, // ss
            0x3344, // ds
        },
        .logbuf = undefined,
    };
    try testFromFile("course_material/perfaware/part1/listing_0045_challenge_register_movs", expect);
}

test "listing_0046_simulate" {
    const expect: Machine = .{
        .registers = .{
            0x0000, // ax
            0x0f01, // cx
            0x0000, // dx
            0xe102, // bx
            0x03e6, // sp
            0x0000, // bp
            0x0000, // si
            0x0000, // di
        },
        .flags = .{
            .parity = true,
            .zero = true,
        },
        .logbuf = undefined,
    };
    try testFromFile("course_material/perfaware/part1/listing_0046_add_sub_cmp", expect);
}

test "listing_0047_simulate" {
    const expect: Machine = .{
        .registers = .{
            0x0000, // ax
            0x0000, // cx
            0x000a, // dx
            0x9ca5, // bx
            0x0063, // sp
            0x0062, // bp
            0x0000, // si
            0x0000, // di
        },
        .flags = .{
            .carry = true,
            .parity = true,
            .auxiliary_carry = true,
            .sign = true,
        },
        .logbuf = undefined,
    };
    try testFromFile("course_material/perfaware/part1/listing_0047_challenge_flags", expect);
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
        try simInstruction(instruction);
    }

    const expect: Machine = .{
        .registers = .{
            0x4321, // ax
            0x0000, // cx
            0x0000, // dx
            0x0000, // bx
            0x0000, // sp
            0x0000, // bp
            0x0000, // si
            0x0000, // di
        },
        .logbuf = undefined,
    };
    expectMachine(expect) catch |err| {
        std.io.getStdErr().writeAll(machine.logbuf.items) catch {};
        return err;
    };
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
        try simInstruction(instruction);
    }

    const expect: Machine = .{
        .registers = .{
            0x6789, // ax
            0x0000, // cx
            0x0000, // dx
            0x0000, // bx
            0x0000, // sp
            0x0000, // bp
            0x0000, // si
            0x0000, // di
        },
        .logbuf = undefined,
    };
    expectMachine(expect) catch |err| {
        std.io.getStdErr().writeAll(machine.logbuf.items) catch {};
        return err;
    };
}

pub fn main() !void {
    try runProgram(std.io.getStdIn().reader());
    machine.logState();
}
