const machine = @import("machine.zig");
const std = @import("std");

const Emulator = struct {
    const Ptr = union(OperandWidth) {
        byte: *u8,
        word: *u16,
    };

    const Value = union(OperandWidth) {
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

    fn processMov(instruction: Instruction) !void {
        const dst: Ptr = switch (instruction.dst.?) {
            .register => |r| getRegister(r),
            .segment => |sr| .{ .word = &machine.segment_registers[@enumToInt(sr)] },
            .immediate => unreachable,
            else => return error.UnsupportedInstruction,
        };

        const src: Value = switch (instruction.src.?) {
            .register => |r| switch (getRegister(r)) {
                .byte => |b| .{ .byte = b.* },
                .word => |w| .{ .word = w.* },
            },
            .segment => |sr| .{ .word = machine.segment_registers[@enumToInt(sr)] },
            .immediate => |iv| switch (iv.width orelse @as(OperandWidth, dst)) {
                .byte => .{ .byte = @intCast(u8, iv.value) },
                .word => .{ .word = iv.value },
            },
            else => return error.UnsupportedInstruction,
        };

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
                .word => return Error.IllegalInstruction,
            },
            .word => |dst_word| switch (src) {
                .byte => |src_byte| dst_word.* = src_byte,
                .word => |src_word| dst_word.* = src_word,
            },
        }
    }

    fn processInstruction(instruction: Instruction) !void {
        return switch (instruction.type) {
            .mov => processMov(instruction),
            else => error.UnsupportedInstruction,
        };
    }

    fn dumpMemory(writer: anytype) !void {
        try writer.writeAll("===== 8086 =====\n");
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
        try writer.writeAll("================\n");
    }
};

const OperandWidth = enum {
    byte,
    word,
};

const Register = enum(u4) {
    al,
    cl,
    dl,
    bl,
    ah,
    ch,
    dh,
    bh,
    ax,
    cx,
    dx,
    bx,
    sp,
    bp,
    si,
    di,

    fn fromInt(width: OperandWidth, value: u3) Register {
        return switch (width) {
            .byte => @intToEnum(Register, value),
            .word => @intToEnum(Register, @as(u4, value) | 8),
        };
    }
};

const EacBase = enum {
    @"bx + si",
    @"bx + di",
    @"bp + si",
    @"bp + di",
    si,
    di,
    bp,
    bx,
};

const EffectiveAddress = struct {
    base: EacBase,
    offset: u16,
    width: ?OperandWidth,
};

const DirectAddress = struct {
    address: u16,
    width: ?OperandWidth,
};

const ImmediateValue = struct {
    value: u16,
    width: ?OperandWidth,
};

const SegmentRegister = enum(u2) {
    es, // extra segment
    cs, // code segment
    ss, // stack segment
    ds, // data segment
};

const InstructionOperand = union(enum) {
    register: Register,
    direct_address: DirectAddress,
    effective_address: EffectiveAddress,
    immediate: ImmediateValue,
    short_jump: i8,
    near_jump: i16,
    far_jump: struct { ip: i16, sp: u16 },
    segment: SegmentRegister,

    fn mnemonic(operand: InstructionOperand) []const u8 {
        return switch (operand) {
            .register => "register",
            .direct_address => "direct_address",
            .effective_address => "effective_address",
            .immediate => "immediate",
            .short_jump => "short_jump",
            .near_jump => "near_jump",
            .far_jump => "far_jump",
            .segment => "segment",
        };
    }
};

const InstructionType = enum {
    hlt,
    mov,
    add,
    @"or",
    adc,
    sbb,
    @"and",
    sub,
    xor,
    cmp,
    jo,
    jno,
    jb,
    jnb,
    je,
    jne,
    jbe,
    jnbe,
    js,
    jns,
    jp,
    jnp,
    jl,
    jnl,
    jle,
    jnle,
    loopne,
    loope,
    loop,
    jcxz,
    inc,
    dec,
    call,
    @"call far",
    jmp,
    @"jmp far",
    push,
    pop,
    @"test",
    xchg,
    in,
    out,
    xlat,
    lea,
    les,
    lds,
    daa,
    das,
    aaa,
    aas,
    aam,
    aad,
    cbw,
    cwd,
    wait,
    pushf,
    popf,
    sahf,
    lahf,
    ret,
    retf,
    int,
    into,
    iret,
    cmc,
    clc,
    stc,
    cli,
    sti,
    cld,
    std,
    not,
    neg,
    mul,
    imul,
    div,
    idiv,
    rol,
    ror,
    rcl,
    rcr,
    shl,
    shr,
    sar,
    int3,
    // TODO(benjamin): The reference does not use the b/w suffix to
    // differentiate byte and word string operations.
    movsb,
    cmpsb,
    stosb,
    lodsb,
    scasb,
    movsw,
    cmpsw,
    stosw,
    lodsw,
    scasw,
};

const Instruction = struct {
    length: u8,
    type: InstructionType,
    dst: ?InstructionOperand,
    src: ?InstructionOperand,
};

const RepeatPrefix = enum {
    repz,
    repnz,
};

// NOTE(benjamin): still not clear on which prefixes can be combined, if any. A
// first skim of the 8086 user's manual had me believe that only the latest
// prefix would be in effect. Reading it again it seems to indicate that
// interrupts will make the CPU forget about all prefixes but the last.
const InstructionPrefixes = struct {
    lock: bool,
    repeat: ?RepeatPrefix,
    segment: ?SegmentRegister,
};

const Error = error{
    IllegalInstruction,
    IncompleteProgram,
};

fn segmentPrefixMnemonic(opt_prefix: ?SegmentRegister) []const u8 {
    return if (opt_prefix) |prefix| {
        return switch (prefix) {
            .es => "es:",
            .cs => "cs:",
            .ss => "ss:",
            .ds => "ds:",
        };
    } else "";
}

fn printOperand(
    operand: InstructionOperand,
    segment_override: ?SegmentRegister,
    writer: anytype,
) !void {
    switch (operand) {
        .register => |register| {
            try writer.writeAll(@tagName(register));
        },
        .direct_address => |direct_address| {
            const segment_prefix = segmentPrefixMnemonic(segment_override);

            try std.fmt.format(writer, "{s} {s}[{d}]", .{
                if (direct_address.width) |w| @tagName(w) else "",
                segment_prefix,
                direct_address.address,
            });
        },
        .effective_address => |ea| {
            const segment_prefix = segmentPrefixMnemonic(segment_override);

            try std.fmt.format(writer, "{s} {s}[{s} {d:1}]", .{
                if (ea.width) |w| @tagName(w) else "",
                segment_prefix,
                @tagName(ea.base),
                @bitCast(i16, ea.offset),
            });
        },
        .immediate => |immediate| {
            try std.fmt.format(writer, "{s} {d}", .{
                if (immediate.width) |w| @tagName(w) else "",
                immediate.value,
            });
        },
        .short_jump => |jump| {
            // NOTE(benjamin): 'nasm' does not account for the instruction
            // length so we must add it here.
            try std.fmt.format(writer, "${d:1}", .{@as(i16, jump) + 2});
        },
        .near_jump => |jump| {
            try std.fmt.format(writer, "{d}", .{jump});
        },
        .far_jump => |jump| {
            try std.fmt.format(writer, "{d}:{d}", .{ jump.sp, jump.ip });
        },
        .segment => |segment| {
            try writer.writeAll(@tagName(segment));
        },
    }
}

fn printInstruction(
    instruction: Instruction,
    prefixes: InstructionPrefixes,
    writer: anytype,
) !void {
    if (prefixes.lock) {
        try writer.writeAll("lock ");
    }

    if (prefixes.repeat) |repeat| {
        switch (repeat) {
            .repz => try writer.writeAll("repz "),
            .repnz => try writer.writeAll("repnz "),
        }
    }

    try writer.writeAll(@tagName(instruction.type));

    if (instruction.dst) |dst| {
        try writer.writeAll(" ");
        try printOperand(dst, prefixes.segment, writer);

        if (instruction.src) |src| {
            try writer.writeAll(", ");
            try printOperand(src, prefixes.segment, writer);
        }
    }
}

const ModByte = struct {
    mod: u2,
    a: u3,
    b: u3,
};

fn parseModByte(mod_byte: u8) ModByte {
    return .{
        .mod = @truncate(u2, (mod_byte & 0o300) >> 6),
        .a = @truncate(u3, (mod_byte & 0o070) >> 3),
        .b = @truncate(u3, (mod_byte & 0o007) >> 0),
    };
}

const ModOperand = struct {
    length: u8,
    operand: InstructionOperand,
};

fn make16(low: u8, high: u8) u16 {
    return @as(u16, low) | (@as(u16, high) << 8);
}

fn extend8(byte: u8) u16 {
    if (128 <= byte) {
        return @as(u16, byte) | 0xff00;
    } else {
        return byte;
    }
}

fn getModOperand(mod_byte: ModByte, width: OperandWidth, byte_stream: []const u8) !ModOperand {
    switch (mod_byte.mod) {
        0 => switch (mod_byte.b) {
            6 => {
                if (byte_stream.len < 2) {
                    return Error.IncompleteProgram;
                }
                return ModOperand{
                    .length = 2,
                    .operand = .{
                        .direct_address = .{
                            .address = make16(byte_stream[0], byte_stream[1]),
                            .width = width,
                        },
                    },
                };
            },
            else => return ModOperand{
                .length = 0,
                .operand = .{
                    .effective_address = .{
                        .base = @intToEnum(EacBase, mod_byte.b),
                        .offset = 0,
                        .width = width,
                    },
                },
            },
        },
        1 => {
            if (byte_stream.len < 1) {
                return Error.IncompleteProgram;
            }
            return ModOperand{
                .length = 1,
                .operand = .{
                    .effective_address = .{
                        .base = @intToEnum(EacBase, mod_byte.b),
                        .offset = extend8(byte_stream[0]),
                        .width = width,
                    },
                },
            };
        },
        2 => {
            if (byte_stream.len < 2) {
                return Error.IncompleteProgram;
            }
            return ModOperand{
                .length = 2,
                .operand = .{
                    .effective_address = .{
                        .base = @intToEnum(EacBase, mod_byte.b),
                        .offset = make16(byte_stream[0], byte_stream[1]),
                        .width = width,
                    },
                },
            };
        },
        3 => return ModOperand{
            .length = 0,
            .operand = .{ .register = Register.fromInt(width, mod_byte.b) },
        },
    }
}

const ImmediateValueDecode = struct {
    operand: ImmediateValue,
    length: u8,
};

fn getImmediateOperand(
    width: OperandWidth,
    byte_stream: []const u8,
) !ImmediateValueDecode {
    switch (width) {
        .byte => {
            if (byte_stream.len < 1) {
                return Error.IncompleteProgram;
            }
            return ImmediateValueDecode{ .length = 1, .operand = .{
                .value = byte_stream[0],
                .width = .byte,
            } };
        },
        .word => {
            if (byte_stream.len < 2) {
                return Error.IncompleteProgram;
            }
            return ImmediateValueDecode{
                .length = 2,
                .operand = .{
                    .value = make16(byte_stream[0], byte_stream[1]),
                    .width = .word,
                },
            };
        },
    }
}

fn decodeRegisterRM(
    instruction_type: InstructionType,
    direction: enum { to_rm, from_rm },
    width: OperandWidth,
    byte_stream: []const u8,
) !Instruction {
    var instruction = Instruction{
        .length = 2,
        .type = instruction_type,
        .dst = undefined,
        .src = undefined,
    };

    if (byte_stream.len < instruction.length) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    const mod_operand = try getModOperand(mod_byte, width, byte_stream[2..]);

    instruction.length += mod_operand.length;

    switch (direction) {
        .to_rm => {
            instruction.dst = mod_operand.operand;
            instruction.src = .{ .register = Register.fromInt(width, mod_byte.a) };
        },
        .from_rm => {
            instruction.dst = .{ .register = Register.fromInt(width, mod_byte.a) };
            instruction.src = mod_operand.operand;
        },
    }

    return instruction;
}

fn decodeRegisterImmediate(
    instruction_type: InstructionType,
    width: OperandWidth,
    byte_stream: []const u8,
) !Instruction {
    var instruction = Instruction{
        .length = switch (width) {
            .byte => 2,
            .word => 3,
        },
        .type = instruction_type,
        .dst = undefined,
        .src = undefined,
    };

    if (byte_stream.len < instruction.length) {
        return Error.IncompleteProgram;
    }

    instruction.dst = .{ .register = @intToEnum(Register, byte_stream[0] & 0x0f) };

    instruction.src = .{
        .immediate = .{
            .value = switch (width) {
                .byte => byte_stream[1],
                .word => make16(byte_stream[1], byte_stream[2]),
            },
            .width = width,
        },
    };

    return instruction;
}

fn decodeMemImmediate(
    instruction_type: InstructionType,
    width: OperandWidth,
    byte_stream: []const u8,
) !Instruction {
    var instruction = Instruction{
        .length = 2,
        .type = instruction_type,
        .dst = undefined,
        .src = undefined,
    };

    if (byte_stream.len < instruction.length) {
        return Error.IncompleteProgram;
    }

    {
        const mod_byte = parseModByte(byte_stream[1]);
        var mod_operand = try getModOperand(mod_byte, width, byte_stream[2..]);

        instruction.length += mod_operand.length;
        instruction.dst = mod_operand.operand;
    }

    {
        const immediate = try getImmediateOperand(width, byte_stream[instruction.length..]);

        instruction.length += immediate.length;
        instruction.src = .{ .immediate = immediate.operand };
    }

    return instruction;
}

fn decodeMovAccMem(
    comptime direction: enum { to_acc, from_acc },
    width: OperandWidth,
    byte_stream: []const u8,
) !Instruction {
    if (byte_stream.len < 3) {
        return Error.IncompleteProgram;
    }

    var instruction = Instruction{
        .length = 3,
        .type = .mov,
        .dst = undefined,
        .src = undefined,
    };
    const direct_address = DirectAddress{ .address = make16(byte_stream[1], byte_stream[2]), .width = width };

    switch (direction) {
        .to_acc => {
            instruction.dst = .{
                .register = switch (width) {
                    .byte => .al,
                    .word => .ax,
                },
            };
            instruction.src = .{ .direct_address = direct_address };
        },
        .from_acc => {
            instruction.dst = .{ .direct_address = direct_address };
            instruction.src = .{
                .register = switch (width) {
                    .byte => .al,
                    .word => .ax,
                },
            };
        },
    }

    return instruction;
}

fn decodeAccImmediate(
    instruction_type: InstructionType,
    accumulator: Register,
    immediate_width: OperandWidth,
    byte_stream: []const u8,
) !Instruction {
    var instruction = Instruction{
        .length = undefined,
        .type = instruction_type,
        .dst = .{ .register = accumulator },
        .src = .{ .immediate = .{
            .value = undefined,
            .width = immediate_width,
        } },
    };

    switch (immediate_width) {
        .byte => {
            if (byte_stream.len < 2) {
                return Error.IncompleteProgram;
            }
            instruction.length = 2;
            instruction.src.?.immediate.value = byte_stream[1];
        },
        .word => {
            if (byte_stream.len < 3) {
                return Error.IncompleteProgram;
            }

            instruction.length = 3;
            instruction.src.?.immediate.value = make16(byte_stream[1], byte_stream[2]);
        },
    }

    return instruction;
}

fn decodeOpRmImmediate(width: OperandWidth, byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    const mod_operand = try getModOperand(mod_byte, width, byte_stream[2..]);
    const immediate = try getImmediateOperand(
        width,
        byte_stream[(2 + mod_operand.length)..],
    );

    return Instruction{
        .length = 2 + mod_operand.length + immediate.length,
        .type = switch (mod_byte.a) {
            0 => .add,
            1 => .@"or",
            2 => .adc,
            3 => .sbb,
            4 => .@"and",
            5 => .sub,
            6 => .xor,
            7 => .cmp,
        },
        .dst = mod_operand.operand,
        .src = .{ .immediate = immediate.operand },
    };
}

fn decodeOpRmExtended(width: OperandWidth, byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 3) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    const mod_operand = try getModOperand(mod_byte, width, byte_stream[2..]);
    const immediate_value = switch (width) {
        .byte => @as(u16, byte_stream[2 + mod_operand.length]),
        .word => extend8(byte_stream[2 + mod_operand.length]),
    };

    return Instruction{
        .length = 3 + mod_operand.length,
        .type = switch (mod_byte.a) {
            0 => .add,
            1 => .@"or",
            2 => .adc,
            3 => .sbb,
            4 => .@"and",
            5 => .sub,
            6 => .xor,
            7 => .cmp,
        },
        .dst = mod_operand.operand,
        .src = .{ .immediate = .{ .value = immediate_value, .width = width } },
    };
}

fn decodeShortLabelJump(instruction_type: InstructionType, byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    return Instruction{
        .length = 2,
        .type = instruction_type,
        .dst = .{ .short_jump = @bitCast(i8, byte_stream[1]) },
        .src = null,
    };
}

fn decodeNearLabelJump(instruction_type: InstructionType, byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 3) {
        return Error.IncompleteProgram;
    }

    return Instruction{
        .length = 3,
        .type = instruction_type,
        .dst = .{ .near_jump = @bitCast(i16, make16(byte_stream[1], byte_stream[2])) },
        .src = null,
    };
}

fn decodeFarLabelJump(instruction_type: InstructionType, byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 5) {
        return Error.IncompleteProgram;
    }

    return Instruction{
        .length = 5,
        .type = instruction_type,
        .dst = .{ .far_jump = .{
            .ip = @bitCast(i16, make16(byte_stream[1], byte_stream[2])),
            .sp = make16(byte_stream[3], byte_stream[4]),
        } },
        .src = null,
    };
}

fn decodeGroup2Word(byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    const mod_operand = try getModOperand(mod_byte, .word, byte_stream[2..]);

    return Instruction{
        .length = 2 + mod_operand.length,
        // TODO(benjamin): Check for invalid mod values maybe?
        .type = switch (mod_byte.a) {
            0 => .inc,
            1 => .dec,
            2 => .call,
            3 => .@"call far",
            4 => .jmp,
            5 => .@"jmp far",
            6 => .push,
            7 => return Error.IllegalInstruction,
        },
        .dst = mod_operand.operand,
        .src = null,
    };
}

fn decodePopRM16(byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    if (mod_byte.a != 0) {
        return Error.IllegalInstruction;
    }

    const mod_operand = try getModOperand(mod_byte, .word, byte_stream[2..]);

    return Instruction{
        .length = 2 + mod_operand.length,
        .type = .pop,
        .dst = mod_operand.operand,
        .src = null,
    };
}

fn decodeAddressObject(
    instruction_type: InstructionType,
    byte_stream: []const u8,
) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    var mod_operand = try getModOperand(mod_byte, .word, byte_stream[2..]);
    switch (mod_operand.operand) {
        .register => return Error.IllegalInstruction,
        // workaround for nasm dissasembly output
        .direct_address => |*da| da.*.width = null,
        // workaround for nasm dissasembly output
        .effective_address => |*ea| ea.*.width = null,
        else => unreachable,
    }

    return Instruction{
        .length = 2 + mod_operand.length,
        .type = instruction_type,
        .dst = .{ .register = Register.fromInt(.word, mod_byte.a) },
        .src = mod_operand.operand,
    };
}

fn decodeGroup2Byte(byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    const instruction_type: InstructionType = switch (mod_byte.a) {
        0 => .inc,
        1 => .dec,
        2...7 => return Error.IllegalInstruction,
    };
    const mod_operand = try getModOperand(mod_byte, .byte, byte_stream[2..]);

    return Instruction{
        .length = 2 + mod_operand.length,
        .type = instruction_type,
        .dst = mod_operand.operand,
        .src = null,
    };
}

fn decodeGroup1(width: OperandWidth, byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    const mod_operand = try getModOperand(mod_byte, width, byte_stream[2..]);

    const instruction_type: InstructionType = switch (mod_byte.a) {
        0 => .@"test",
        1 => return Error.IllegalInstruction,
        2 => .not,
        3 => .neg,
        4 => .mul,
        5 => .imul,
        6 => .div,
        7 => .idiv,
    };

    var instruction = Instruction{
        .length = 2 + mod_operand.length,
        .type = instruction_type,
        .dst = mod_operand.operand,
        .src = undefined,
    };

    if (instruction_type != .@"test") {
        instruction.src = null;
        return instruction;
    }

    const immediate_operand = try getImmediateOperand(width, byte_stream[instruction.length..]);

    instruction.length += immediate_operand.length;
    instruction.src = .{ .immediate = immediate_operand.operand };
    return instruction;
}

fn decodeShift(
    width: OperandWidth,
    src_operand: enum { one, cl },
    byte_stream: []const u8,
) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    const mod_operand = try getModOperand(mod_byte, width, byte_stream[2..]);

    return Instruction{
        .length = 2 + mod_operand.length,
        .type = switch (mod_byte.a) {
            0 => .rol,
            1 => .ror,
            2 => .rcl,
            3 => .rcr,
            4 => .shl,
            5 => .shr,
            6 => return Error.IllegalInstruction,
            7 => .sar,
        },
        .dst = mod_operand.operand,
        .src = switch (src_operand) {
            .one => .{ .immediate = .{ .value = 1, .width = null } },
            .cl => .{ .register = .cl },
        },
    };
}

fn decodeRetImmediate(kind: enum { intrasegment, intersegment }, byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 3) {
        return Error.IncompleteProgram;
    }

    return Instruction{
        .length = 3,
        .type = switch (kind) {
            .intrasegment => .ret,
            .intersegment => .retf,
        },
        .dst = .{ .immediate = .{
            .value = make16(byte_stream[1], byte_stream[2]),
            .width = .word,
        } },
        .src = null,
    };
}

fn decodeIntImmediate(byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    return Instruction{
        .length = 2,
        .type = .int,
        .dst = .{ .immediate = .{
            .value = byte_stream[1],
            .width = .byte,
        } },
        .src = null,
    };
}

fn decodeSegmentRM(
    instruction_type: InstructionType,
    direction: enum { to_rm, from_rm },
    byte_stream: []const u8,
) !Instruction {
    var instruction = Instruction{
        .length = 2,
        .type = instruction_type,
        .dst = undefined,
        .src = undefined,
    };

    if (byte_stream.len < instruction.length) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    if (4 <= mod_byte.a) {
        return Error.IllegalInstruction;
    }
    const mod_operand = try getModOperand(mod_byte, .word, byte_stream[2..]);

    instruction.length += mod_operand.length;

    switch (direction) {
        .to_rm => {
            instruction.dst = mod_operand.operand;
            instruction.src = .{ .segment = @intToEnum(SegmentRegister, mod_byte.a) };
        },
        .from_rm => {
            instruction.dst = .{ .segment = @intToEnum(SegmentRegister, mod_byte.a) };
            instruction.src = mod_operand.operand;
        },
    }

    return instruction;
}

fn decodeInstruction(byte_stream: []const u8) !union(enum) {
    instruction: Instruction,
    lock: void,
    repeat: RepeatPrefix,
    segment: SegmentRegister,
} {
    if (byte_stream.len < 1) {
        return Error.IncompleteProgram;
    }

    const instruction: Instruction = switch (byte_stream[0]) {
        0x00 => try decodeRegisterRM(.add, .to_rm, .byte, byte_stream),
        0x01 => try decodeRegisterRM(.add, .to_rm, .word, byte_stream),
        0x02 => try decodeRegisterRM(.add, .from_rm, .byte, byte_stream),
        0x03 => try decodeRegisterRM(.add, .from_rm, .word, byte_stream),
        0x04 => try decodeAccImmediate(.add, .al, .byte, byte_stream),
        0x05 => try decodeAccImmediate(.add, .ax, .word, byte_stream),
        0x06 => Instruction{ .length = 1, .type = .push, .dst = .{ .segment = .es }, .src = null },
        0x07 => Instruction{ .length = 1, .type = .pop, .dst = .{ .segment = .es }, .src = null },

        0x08 => try decodeRegisterRM(.@"or", .to_rm, .byte, byte_stream),
        0x09 => try decodeRegisterRM(.@"or", .to_rm, .word, byte_stream),
        0x0a => try decodeRegisterRM(.@"or", .from_rm, .byte, byte_stream),
        0x0b => try decodeRegisterRM(.@"or", .from_rm, .word, byte_stream),
        0x0c => try decodeAccImmediate(.@"or", .al, .byte, byte_stream),
        0x0d => try decodeAccImmediate(.@"or", .ax, .word, byte_stream),
        0x0e => Instruction{ .length = 1, .type = .push, .dst = .{ .segment = .cs }, .src = null },
        0x0f => return Error.IllegalInstruction,

        0x10 => try decodeRegisterRM(.adc, .to_rm, .byte, byte_stream),
        0x11 => try decodeRegisterRM(.adc, .to_rm, .word, byte_stream),
        0x12 => try decodeRegisterRM(.adc, .from_rm, .byte, byte_stream),
        0x13 => try decodeRegisterRM(.adc, .from_rm, .word, byte_stream),
        0x14 => try decodeAccImmediate(.adc, .al, .byte, byte_stream),
        0x15 => try decodeAccImmediate(.adc, .ax, .word, byte_stream),
        0x16 => Instruction{ .length = 1, .type = .push, .dst = .{ .segment = .ss }, .src = null },
        0x17 => Instruction{ .length = 1, .type = .pop, .dst = .{ .segment = .ss }, .src = null },

        0x18 => try decodeRegisterRM(.sbb, .to_rm, .byte, byte_stream),
        0x19 => try decodeRegisterRM(.sbb, .to_rm, .word, byte_stream),
        0x1a => try decodeRegisterRM(.sbb, .from_rm, .byte, byte_stream),
        0x1b => try decodeRegisterRM(.sbb, .from_rm, .word, byte_stream),
        0x1c => try decodeAccImmediate(.sbb, .al, .byte, byte_stream),
        0x1d => try decodeAccImmediate(.sbb, .ax, .word, byte_stream),
        0x1e => Instruction{ .length = 1, .type = .push, .dst = .{ .segment = .ds }, .src = null },
        0x1f => Instruction{ .length = 1, .type = .pop, .dst = .{ .segment = .ds }, .src = null },

        0x20 => try decodeRegisterRM(.@"and", .to_rm, .byte, byte_stream),
        0x21 => try decodeRegisterRM(.@"and", .to_rm, .word, byte_stream),
        0x22 => try decodeRegisterRM(.@"and", .from_rm, .byte, byte_stream),
        0x23 => try decodeRegisterRM(.@"and", .from_rm, .word, byte_stream),
        0x24 => try decodeAccImmediate(.@"and", .al, .byte, byte_stream),
        0x25 => try decodeAccImmediate(.@"and", .ax, .word, byte_stream),
        0x26 => return .{ .segment = .es },
        0x27 => Instruction{ .length = 1, .type = .daa, .dst = null, .src = null },

        0x28 => try decodeRegisterRM(.sub, .to_rm, .byte, byte_stream),
        0x29 => try decodeRegisterRM(.sub, .to_rm, .word, byte_stream),
        0x2a => try decodeRegisterRM(.sub, .from_rm, .byte, byte_stream),
        0x2b => try decodeRegisterRM(.sub, .from_rm, .word, byte_stream),
        0x2c => try decodeAccImmediate(.sub, .al, .byte, byte_stream),
        0x2d => try decodeAccImmediate(.sub, .ax, .word, byte_stream),
        0x2e => return .{ .segment = .cs },
        0x2f => Instruction{ .length = 1, .type = .das, .dst = null, .src = null },

        0x30 => try decodeRegisterRM(.xor, .to_rm, .byte, byte_stream),
        0x31 => try decodeRegisterRM(.xor, .to_rm, .word, byte_stream),
        0x32 => try decodeRegisterRM(.xor, .from_rm, .byte, byte_stream),
        0x33 => try decodeRegisterRM(.xor, .from_rm, .word, byte_stream),
        0x34 => try decodeAccImmediate(.xor, .al, .byte, byte_stream),
        0x35 => try decodeAccImmediate(.xor, .ax, .word, byte_stream),
        0x36 => return .{ .segment = .ss },
        0x37 => Instruction{ .length = 1, .type = .aaa, .dst = null, .src = null },

        0x38 => try decodeRegisterRM(.cmp, .to_rm, .byte, byte_stream),
        0x39 => try decodeRegisterRM(.cmp, .to_rm, .word, byte_stream),
        0x3a => try decodeRegisterRM(.cmp, .from_rm, .byte, byte_stream),
        0x3b => try decodeRegisterRM(.cmp, .from_rm, .word, byte_stream),
        0x3c => try decodeAccImmediate(.cmp, .al, .byte, byte_stream),
        0x3d => try decodeAccImmediate(.cmp, .ax, .word, byte_stream),
        0x3e => return .{ .segment = .ds },
        0x3f => Instruction{ .length = 1, .type = .aas, .dst = null, .src = null },

        0x40...0x47 => Instruction{
            .length = 1,
            .type = .inc,
            .dst = .{ .register = Register.fromInt(.word, @truncate(u3, byte_stream[0])) },
            .src = null,
        },
        0x48...0x4f => Instruction{
            .length = 1,
            .type = .dec,
            .dst = .{ .register = Register.fromInt(.word, @truncate(u3, byte_stream[0])) },
            .src = null,
        },

        0x50...0x57 => Instruction{
            .length = 1,
            .type = .push,
            .dst = .{ .register = Register.fromInt(.word, @truncate(u3, byte_stream[0])) },
            .src = null,
        },
        0x58...0x5f => Instruction{
            .length = 1,
            .type = .pop,
            .dst = .{ .register = Register.fromInt(.word, @truncate(u3, byte_stream[0])) },
            .src = null,
        },

        0x60...0x6f => return Error.IllegalInstruction,

        0x70 => try decodeShortLabelJump(.jo, byte_stream),
        0x71 => try decodeShortLabelJump(.jno, byte_stream),
        0x72 => try decodeShortLabelJump(.jb, byte_stream),
        0x73 => try decodeShortLabelJump(.jnb, byte_stream),
        0x74 => try decodeShortLabelJump(.je, byte_stream),
        0x75 => try decodeShortLabelJump(.jne, byte_stream),
        0x76 => try decodeShortLabelJump(.jbe, byte_stream),
        0x77 => try decodeShortLabelJump(.jnbe, byte_stream),
        0x78 => try decodeShortLabelJump(.js, byte_stream),
        0x79 => try decodeShortLabelJump(.jns, byte_stream),
        0x7a => try decodeShortLabelJump(.jp, byte_stream),
        0x7b => try decodeShortLabelJump(.jnp, byte_stream),
        0x7c => try decodeShortLabelJump(.jl, byte_stream),
        0x7d => try decodeShortLabelJump(.jnl, byte_stream),
        0x7e => try decodeShortLabelJump(.jle, byte_stream),
        0x7f => try decodeShortLabelJump(.jnle, byte_stream),

        0x80 => try decodeOpRmImmediate(.byte, byte_stream),
        0x81 => try decodeOpRmImmediate(.word, byte_stream),

        0x82 => try decodeOpRmExtended(.byte, byte_stream), // Duplicates 0x80?
        0x83 => try decodeOpRmExtended(.word, byte_stream),

        // Order shouldn't matter for these but we'll follow the convention
        // from table 4-13 of the user's manual (9800722-03).
        0x84 => try decodeRegisterRM(.@"test", .to_rm, .byte, byte_stream),
        0x85 => try decodeRegisterRM(.@"test", .to_rm, .word, byte_stream),
        0x86 => try decodeRegisterRM(.xchg, .from_rm, .byte, byte_stream),
        0x87 => try decodeRegisterRM(.xchg, .from_rm, .word, byte_stream),

        0x88 => try decodeRegisterRM(.mov, .to_rm, .byte, byte_stream),
        0x89 => try decodeRegisterRM(.mov, .to_rm, .word, byte_stream),
        0x8a => try decodeRegisterRM(.mov, .from_rm, .byte, byte_stream),
        0x8b => try decodeRegisterRM(.mov, .from_rm, .word, byte_stream),

        0x8c => try decodeSegmentRM(.mov, .to_rm, byte_stream),
        0x8d => try decodeAddressObject(.lea, byte_stream),
        0x8e => try decodeSegmentRM(.mov, .from_rm, byte_stream),

        0x8f => try decodePopRM16(byte_stream),

        // NOTE(benjamin): 0x90 is NOP (xchg ax, ax).
        0x90...0x97 => Instruction{
            .length = 1,
            .type = .xchg,
            .dst = .{ .register = .ax },
            .src = .{ .register = Register.fromInt(.word, @truncate(u3, byte_stream[0])) },
        },

        0x98 => Instruction{ .length = 1, .type = .cbw, .dst = null, .src = null },
        0x99 => Instruction{ .length = 1, .type = .cwd, .dst = null, .src = null },
        0x9a => try decodeFarLabelJump(.call, byte_stream),
        0x9b => Instruction{ .length = 1, .type = .wait, .dst = null, .src = null },
        0x9c => Instruction{ .length = 1, .type = .pushf, .dst = null, .src = null },
        0x9d => Instruction{ .length = 1, .type = .popf, .dst = null, .src = null },
        0x9e => Instruction{ .length = 1, .type = .sahf, .dst = null, .src = null },
        0x9f => Instruction{ .length = 1, .type = .lahf, .dst = null, .src = null },

        0xa0 => try decodeMovAccMem(.to_acc, .byte, byte_stream),
        0xa1 => try decodeMovAccMem(.to_acc, .word, byte_stream),
        0xa2 => try decodeMovAccMem(.from_acc, .byte, byte_stream),
        0xa3 => try decodeMovAccMem(.from_acc, .word, byte_stream),

        0xa4 => Instruction{ .length = 1, .type = .movsb, .dst = null, .src = null },
        0xa5 => Instruction{ .length = 1, .type = .movsw, .dst = null, .src = null },
        0xa6 => Instruction{ .length = 1, .type = .cmpsb, .dst = null, .src = null },
        0xa7 => Instruction{ .length = 1, .type = .cmpsw, .dst = null, .src = null },

        0xa8 => try decodeAccImmediate(.@"test", .al, .byte, byte_stream),
        0xa9 => try decodeAccImmediate(.@"test", .ax, .word, byte_stream),

        0xaa => Instruction{ .length = 1, .type = .stosb, .dst = null, .src = null },
        0xab => Instruction{ .length = 1, .type = .stosw, .dst = null, .src = null },
        0xac => Instruction{ .length = 1, .type = .lodsb, .dst = null, .src = null },
        0xad => Instruction{ .length = 1, .type = .lodsw, .dst = null, .src = null },
        0xae => Instruction{ .length = 1, .type = .scasb, .dst = null, .src = null },
        0xaf => Instruction{ .length = 1, .type = .scasw, .dst = null, .src = null },

        0xb0...0xb7 => try decodeRegisterImmediate(.mov, .byte, byte_stream),
        0xb8...0xbf => try decodeRegisterImmediate(.mov, .word, byte_stream),

        0xc0...0xc1 => return Error.IllegalInstruction,

        0xc2 => try decodeRetImmediate(.intrasegment, byte_stream),
        0xc3 => Instruction{ .length = 1, .type = .ret, .dst = null, .src = null },

        0xc4 => try decodeAddressObject(.les, byte_stream),
        0xc5 => try decodeAddressObject(.lds, byte_stream),

        0xc6 => try decodeMemImmediate(.mov, .byte, byte_stream),
        0xc7 => try decodeMemImmediate(.mov, .word, byte_stream),

        0xc8...0xc9 => return Error.IllegalInstruction,

        0xca => try decodeRetImmediate(.intersegment, byte_stream),
        0xcb => Instruction{ .length = 1, .type = .retf, .dst = null, .src = null },

        0xcc => Instruction{
            .length = 1,
            .type = .int3,
            .dst = null,
            .src = null,
        },
        0xcd => try decodeIntImmediate(byte_stream),

        0xce => Instruction{ .length = 1, .type = .into, .dst = null, .src = null },
        0xcf => Instruction{ .length = 1, .type = .iret, .dst = null, .src = null },

        0xd0 => try decodeShift(.byte, .one, byte_stream),
        0xd1 => try decodeShift(.word, .one, byte_stream),
        0xd2 => try decodeShift(.byte, .cl, byte_stream),
        0xd3 => try decodeShift(.word, .cl, byte_stream),

        // TODO(benjamin): assert (byte_stream[1] == 0x0a) ?
        0xd4 => Instruction{ .length = 2, .type = .aam, .dst = null, .src = null },
        0xd5 => Instruction{ .length = 2, .type = .aad, .dst = null, .src = null },

        0xd6 => return Error.IllegalInstruction,
        0xd7 => Instruction{ .length = 1, .type = .xlat, .dst = null, .src = null },

        // NOTE(benjamin): unsupported by nasm.
        0xd8...0xdf => return error.InstructionNotImplemented,

        0xe0 => try decodeShortLabelJump(.loopne, byte_stream),
        0xe1 => try decodeShortLabelJump(.loope, byte_stream),
        0xe2 => try decodeShortLabelJump(.loop, byte_stream),
        0xe3 => try decodeShortLabelJump(.jcxz, byte_stream),
        0xe4 => try decodeAccImmediate(.in, .al, .byte, byte_stream),
        0xe5 => try decodeAccImmediate(.in, .ax, .byte, byte_stream),
        0xe6 => try decodeAccImmediate(.out, .al, .byte, byte_stream),
        0xe7 => try decodeAccImmediate(.out, .ax, .byte, byte_stream),

        0xe8 => try decodeNearLabelJump(.call, byte_stream),
        0xe9 => try decodeNearLabelJump(.jmp, byte_stream),
        0xea => try decodeFarLabelJump(.jmp, byte_stream),
        0xeb => try decodeShortLabelJump(.jmp, byte_stream),

        0xec => Instruction{
            .length = 1,
            .type = .in,
            .dst = .{ .register = .al },
            .src = .{ .register = .dx },
        },
        0xed => Instruction{
            .length = 1,
            .type = .in,
            .dst = .{ .register = .ax },
            .src = .{ .register = .dx },
        },
        0xee => Instruction{
            .length = 1,
            .type = .out,
            .dst = .{ .register = .al },
            .src = .{ .register = .dx },
        },
        0xef => Instruction{
            .length = 1,
            .type = .out,
            .dst = .{ .register = .ax },
            .src = .{ .register = .dx },
        },

        0xf0 => return .{ .lock = {} },
        0xf1 => return Error.IllegalInstruction,
        0xf2 => return .{ .repeat = .repnz },
        0xf3 => return .{ .repeat = .repz },

        0xf4 => Instruction{ .length = 1, .type = .hlt, .dst = null, .src = null },
        0xf5 => Instruction{ .length = 1, .type = .cmc, .dst = null, .src = null },

        0xf6 => try decodeGroup1(.byte, byte_stream),
        0xf7 => try decodeGroup1(.word, byte_stream),

        0xf8 => Instruction{ .length = 1, .type = .clc, .dst = null, .src = null },
        0xf9 => Instruction{ .length = 1, .type = .stc, .dst = null, .src = null },
        0xfa => Instruction{ .length = 1, .type = .cli, .dst = null, .src = null },
        0xfb => Instruction{ .length = 1, .type = .sti, .dst = null, .src = null },
        0xfc => Instruction{ .length = 1, .type = .cld, .dst = null, .src = null },
        0xfd => Instruction{ .length = 1, .type = .std, .dst = null, .src = null },

        0xfe => try decodeGroup2Byte(byte_stream),

        // 0xff is described as Group2 in the manual
        0xff => try decodeGroup2Word(byte_stream),
    };

    return .{ .instruction = instruction };
}

fn decodeProgram(reader: anytype, writer: anytype, emulate: bool) !void {
    const program_len = try reader.readAll(machine.memory[0..]);
    const code_segment = blk: {
        const code_segment_address = machine.segment_registers[@enumToInt(SegmentRegister.cs)];
        break :blk machine.memory[code_segment_address .. code_segment_address + 0xffff];
    };

    while (true) {
        const pos = machine.instruction_pointer;
        if (machine.memory.len <= pos) {
            return error.MemoryOverflow;
        }

        if (pos == program_len) {
            return;
        } else if (program_len < pos) {
            return error.ProgramOverflow;
        }

        var window = code_segment[pos..];

        var prefix = InstructionPrefixes{ .lock = false, .repeat = null, .segment = null };
        var instruction: Instruction = decode: for (0..window.len) |i| {
            var decoded = decodeInstruction(window[i..]) catch |err| {
                std.debug.print("{s}: 0x{x:0>2}\n", .{ @errorName(err), window[i] });
                return err;
            };

            switch (decoded) {
                .lock => prefix.lock = true,
                .repeat => |val| prefix.repeat = val,
                .segment => |seg| prefix.segment = seg,
                .instruction => |*inst| {
                    inst.length += @intCast(u8, i);
                    break :decode inst.*;
                },
            }
        } else return Error.IncompleteProgram;

        machine.instruction_pointer += instruction.length;

        if (instruction.type == .out) {
            std.mem.swap(?InstructionOperand, &instruction.src, &instruction.dst);
        }
        if (instruction.dst) |*dst| {
            switch (dst.*) {
                // TODO(benjamin): adding to the jump here is a hack for the
                // assembly output and needs to be corrected.
                .near_jump => |*jump| jump.* += @intCast(i16, machine.instruction_pointer),
                else => {},
            }
        }

        if (emulate) {
            Emulator.processInstruction(instruction) catch |err| {
                std.debug.print("{s}: {s} {s} {s}\n", .{
                    @errorName(err),
                    @tagName(instruction.type),
                    if (instruction.dst) |dst| dst.mnemonic() else "(null)",
                    if (instruction.src) |src| src.mnemonic() else "(null)",
                });
                return err;
            };
        } else {
            try printInstruction(instruction, prefix, writer);
            try writer.writeAll(" ;");
            for (window[0..instruction.length]) |byte| {
                try std.fmt.format(writer, " 0x{x:0>2}", .{byte});
            }
            try writer.writeAll("\n");
        }
    }
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
