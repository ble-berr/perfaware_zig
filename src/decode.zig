const std = @import("std");

pub const OperandWidth = enum(u2) {
    byte = 1,
    word = 2,
};

// NOTE(benjamin): values chosen to allow direct casting of 3 bit values in the
// instruction stream when account for the width bit.
pub const Register = enum(u4) {
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

    pub fn fromInt(width: OperandWidth, value: u3) Register {
        return switch (width) {
            .byte => @enumFromInt(value),
            .word => @enumFromInt(@as(u4, value) | 8),
        };
    }
};

// NOTE(benjamin): Ordered according to their respective values within a mod
// byte, allowing for direct conversion via @enumFromInt.
const EacBase = enum(u3) {
    @"bx + si",
    @"bx + di",
    @"bp + si",
    @"bp + di",
    si,
    di,
    bp,
    bx,
};

pub const EffectiveAddress = struct {
    base: EacBase,
    offset: u16,
    width: OperandWidth,
};

pub const DirectAddress = struct {
    address: u16,
    width: OperandWidth,
};

pub const SegmentRegister = enum(u2) {
    es, // extra segment
    cs, // code segment
    ss, // stack segment
    ds, // data segment
};

pub const InstructionOperand = union(enum) {
    none: void,
    register: Register,
    direct_address: DirectAddress,
    effective_address: EffectiveAddress,
    immediate_byte: u8,
    immediate_word: u16,
    short_jump: i8,
    near_jump: i16,
    far_jump: struct { ip: i16, sp: u16 },
    segment: SegmentRegister,
};

pub const InstructionType = enum {
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
    illegal,
    incomplete, // Valid instruction but too few bytes left in the program.
};

// NOTE(benjamin): still not clear on which prefixes can be combined, if any. A
// first skim of the 8086 user's manual had me believe that only the latest
// prefix would be in effect. Reading it again it seems to indicate that
// interrupts will make the CPU forget about all prefixes but the last.
pub const Instruction = struct {
    lock: bool = false,
    repeat: ?RepeatPrefix = null,
    segment: ?SegmentRegister = null,
    length: u8,
    type: InstructionType,
    dst: InstructionOperand = .none,
    src: InstructionOperand = .none,
};

pub const RepeatPrefix = enum {
    repz,
    repnz,
};

pub const Error = error{
    IncompleteProgram,
};

const ModByte = struct {
    mod: u2,
    a: u3,
    b: u3,
};

fn parseModByte(mod_byte: u8) ModByte {
    return .{
        .mod = @truncate((mod_byte & 0o300) >> 6),
        .a = @truncate((mod_byte & 0o070) >> 3),
        .b = @truncate((mod_byte & 0o007) >> 0),
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
        0 => if (mod_byte.b == 6) {
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
        } else {
            return ModOperand{
                .length = 0,
                .operand = .{
                    .effective_address = .{
                        .base = @enumFromInt(mod_byte.b),
                        .offset = 0,
                        .width = width,
                    },
                },
            };
        },
        1 => {
            if (byte_stream.len < 1) {
                return Error.IncompleteProgram;
            }
            return ModOperand{
                .length = 1,
                .operand = .{
                    .effective_address = .{
                        .base = @enumFromInt(mod_byte.b),
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
                        .base = @enumFromInt(mod_byte.b),
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

fn getImmediateOperand(width: OperandWidth, byte_stream: []const u8) !InstructionOperand {
    if (byte_stream.len < @intFromEnum(width)) {
        return Error.IncompleteProgram;
    }
    return switch (width) {
        .byte => .{ .immediate_byte = byte_stream[0] },
        .word => .{ .immediate_word = make16(byte_stream[0], byte_stream[1]) },
    };
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
    switch (width) {
        .byte => {
            if (byte_stream.len < 2) {
                return Error.IncompleteProgram;
            }
            return Instruction{
                .length = 2,
                .type = instruction_type,
                .dst = .{ .register = @enumFromInt(@as(u4, @truncate(byte_stream[0]))) },
                .src = .{ .immediate_byte = byte_stream[1] },
            };
        },
        .word => {
            if (byte_stream.len < 3) {
                return Error.IncompleteProgram;
            }
            return Instruction{
                .length = 3,
                .type = instruction_type,
                .dst = .{ .register = @enumFromInt(@as(u4, @truncate(byte_stream[0]))) },
                .src = .{ .immediate_word = make16(byte_stream[1], byte_stream[2]) },
            };
        },
    }
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
        const mod_operand = try getModOperand(mod_byte, width, byte_stream[2..]);

        instruction.length += mod_operand.length;
        instruction.dst = mod_operand.operand;
    }

    instruction.src = try getImmediateOperand(width, byte_stream[instruction.length..]);
    instruction.length += @intFromEnum(width);


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
    return Instruction{
        .length = @intFromEnum(immediate_width) + 1,
        .type = instruction_type,
        .dst = .{ .register = accumulator },
        .src = try getImmediateOperand(immediate_width, byte_stream[1..]),
    };
}

fn decodeOpRmImmediate(width: OperandWidth, byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    const mod_operand = try getModOperand(mod_byte, width, byte_stream[2..]);

    return Instruction{
        .length = 2 + mod_operand.length + @intFromEnum(width),
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
        .src = try getImmediateOperand(width, byte_stream[(2 + mod_operand.length)..]),
    };
}

fn decodeOpRmExtended(width: OperandWidth, byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 3) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    const mod_operand = try getModOperand(mod_byte, width, byte_stream[2..]);

    return Instruction{
        // byte0 + mod_byte + mod_operand + signed_byte
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
        .src = switch (width) {
            .byte => .{ .immediate_byte = byte_stream[2 + mod_operand.length] },
            .word => .{ .immediate_word = extend8(byte_stream[2 + mod_operand.length]) },
        },
    };
}

fn decodeShortLabelJump(instruction_type: InstructionType, byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    return Instruction{
        .length = 2,
        .type = instruction_type,
        .dst = .{ .short_jump = @bitCast(byte_stream[1]) },
    };
}

fn decodeNearLabelJump(instruction_type: InstructionType, byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 3) {
        return Error.IncompleteProgram;
    }

    return Instruction{
        .length = 3,
        .type = instruction_type,
        .dst = .{ .near_jump = @bitCast(make16(byte_stream[1], byte_stream[2])) },
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
            .ip = @bitCast(make16(byte_stream[1], byte_stream[2])),
            .sp = make16(byte_stream[3], byte_stream[4]),
        } },
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
            7 => .illegal,
        },
        .dst = mod_operand.operand,
    };
}

fn decodePopRM16(byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    const mod_operand = try getModOperand(mod_byte, .word, byte_stream[2..]);

    return Instruction{
        .length = 2 + mod_operand.length,
        .type = if (mod_byte.a == 0) .pop else .illegal,
        .dst = mod_operand.operand,
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
    const mod_operand = try getModOperand(mod_byte, .word, byte_stream[2..]);

     var inst = Instruction{
        .length = 2 + mod_operand.length,
        .type = instruction_type,
        .dst = .{ .register = Register.fromInt(.word, mod_byte.a) },
        .src = mod_operand.operand,
    };

    switch (inst.src) {
        .direct_address, .effective_address => {},
        else => inst.type = .illegal,
    }

    return inst;
}

fn decodeGroup2Byte(byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parseModByte(byte_stream[1]);
    const instruction_type: InstructionType = switch (mod_byte.a) {
        0 => .inc,
        1 => .dec,
        2...7 => .illegal,
    };
    const mod_operand = try getModOperand(mod_byte, .byte, byte_stream[2..]);

    return Instruction{
        .length = 2 + mod_operand.length,
        .type = instruction_type,
        .dst = mod_operand.operand,
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
        1 => .illegal,
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
    };

    if (instruction_type == .@"test") {
        instruction.src = try getImmediateOperand(width, byte_stream[instruction.length..]);
        instruction.length += @intFromEnum(width);
    }

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
            6 => .illegal,
            7 => .sar,
        },
        .dst = mod_operand.operand,
        .src = switch (src_operand) {
            .one => .{ .immediate_byte = 1 },
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
        .dst = .{ .immediate_word = make16(byte_stream[1], byte_stream[2]) },
    };
}

fn decodeIntImmediate(byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 2) {
        return Error.IncompleteProgram;
    }

    return Instruction{
        .length = 2,
        .type = .int,
        .dst = .{ .immediate_byte = byte_stream[1] },
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
        instruction.type = .illegal;
    }
    const mod_operand = try getModOperand(mod_byte, .word, byte_stream[2..]);

    instruction.length += mod_operand.length;

    switch (direction) {
        .to_rm => {
            instruction.dst = mod_operand.operand;
            instruction.src = .{ .segment = @enumFromInt(mod_byte.a) };
        },
        .from_rm => {
            instruction.dst = .{ .segment = @enumFromInt(mod_byte.a) };
            instruction.src = mod_operand.operand;
        },
    }

    return instruction;
}

fn decodeInstruction(byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 1) {
        return Error.IncompleteProgram;
    }

    return switch (byte_stream[0]) {
        0x00 => try decodeRegisterRM(.add, .to_rm, .byte, byte_stream),
        0x01 => try decodeRegisterRM(.add, .to_rm, .word, byte_stream),
        0x02 => try decodeRegisterRM(.add, .from_rm, .byte, byte_stream),
        0x03 => try decodeRegisterRM(.add, .from_rm, .word, byte_stream),
        0x04 => try decodeAccImmediate(.add, .al, .byte, byte_stream),
        0x05 => try decodeAccImmediate(.add, .ax, .word, byte_stream),
        0x06 => Instruction{ .length = 1, .type = .push, .dst = .{ .segment = .es } },
        0x07 => Instruction{ .length = 1, .type = .pop, .dst = .{ .segment = .es } },

        0x08 => try decodeRegisterRM(.@"or", .to_rm, .byte, byte_stream),
        0x09 => try decodeRegisterRM(.@"or", .to_rm, .word, byte_stream),
        0x0a => try decodeRegisterRM(.@"or", .from_rm, .byte, byte_stream),
        0x0b => try decodeRegisterRM(.@"or", .from_rm, .word, byte_stream),
        0x0c => try decodeAccImmediate(.@"or", .al, .byte, byte_stream),
        0x0d => try decodeAccImmediate(.@"or", .ax, .word, byte_stream),
        0x0e => Instruction{ .length = 1, .type = .push, .dst = .{ .segment = .cs } },
        0x0f => Instruction{ .length = 1, .type = .illegal },

        0x10 => try decodeRegisterRM(.adc, .to_rm, .byte, byte_stream),
        0x11 => try decodeRegisterRM(.adc, .to_rm, .word, byte_stream),
        0x12 => try decodeRegisterRM(.adc, .from_rm, .byte, byte_stream),
        0x13 => try decodeRegisterRM(.adc, .from_rm, .word, byte_stream),
        0x14 => try decodeAccImmediate(.adc, .al, .byte, byte_stream),
        0x15 => try decodeAccImmediate(.adc, .ax, .word, byte_stream),
        0x16 => Instruction{ .length = 1, .type = .push, .dst = .{ .segment = .ss } },
        0x17 => Instruction{ .length = 1, .type = .pop, .dst = .{ .segment = .ss } },

        0x18 => try decodeRegisterRM(.sbb, .to_rm, .byte, byte_stream),
        0x19 => try decodeRegisterRM(.sbb, .to_rm, .word, byte_stream),
        0x1a => try decodeRegisterRM(.sbb, .from_rm, .byte, byte_stream),
        0x1b => try decodeRegisterRM(.sbb, .from_rm, .word, byte_stream),
        0x1c => try decodeAccImmediate(.sbb, .al, .byte, byte_stream),
        0x1d => try decodeAccImmediate(.sbb, .ax, .word, byte_stream),
        0x1e => Instruction{ .length = 1, .type = .push, .dst = .{ .segment = .ds } },
        0x1f => Instruction{ .length = 1, .type = .pop, .dst = .{ .segment = .ds } },

        0x20 => try decodeRegisterRM(.@"and", .to_rm, .byte, byte_stream),
        0x21 => try decodeRegisterRM(.@"and", .to_rm, .word, byte_stream),
        0x22 => try decodeRegisterRM(.@"and", .from_rm, .byte, byte_stream),
        0x23 => try decodeRegisterRM(.@"and", .from_rm, .word, byte_stream),
        0x24 => try decodeAccImmediate(.@"and", .al, .byte, byte_stream),
        0x25 => try decodeAccImmediate(.@"and", .ax, .word, byte_stream),
        0x26 => unreachable, // segment ES
        0x27 => Instruction{ .length = 1, .type = .daa },

        0x28 => try decodeRegisterRM(.sub, .to_rm, .byte, byte_stream),
        0x29 => try decodeRegisterRM(.sub, .to_rm, .word, byte_stream),
        0x2a => try decodeRegisterRM(.sub, .from_rm, .byte, byte_stream),
        0x2b => try decodeRegisterRM(.sub, .from_rm, .word, byte_stream),
        0x2c => try decodeAccImmediate(.sub, .al, .byte, byte_stream),
        0x2d => try decodeAccImmediate(.sub, .ax, .word, byte_stream),
        0x2e => unreachable, // segment CS
        0x2f => Instruction{ .length = 1, .type = .das },

        0x30 => try decodeRegisterRM(.xor, .to_rm, .byte, byte_stream),
        0x31 => try decodeRegisterRM(.xor, .to_rm, .word, byte_stream),
        0x32 => try decodeRegisterRM(.xor, .from_rm, .byte, byte_stream),
        0x33 => try decodeRegisterRM(.xor, .from_rm, .word, byte_stream),
        0x34 => try decodeAccImmediate(.xor, .al, .byte, byte_stream),
        0x35 => try decodeAccImmediate(.xor, .ax, .word, byte_stream),
        0x36 => unreachable, // segment SS
        0x37 => Instruction{ .length = 1, .type = .aaa },

        0x38 => try decodeRegisterRM(.cmp, .to_rm, .byte, byte_stream),
        0x39 => try decodeRegisterRM(.cmp, .to_rm, .word, byte_stream),
        0x3a => try decodeRegisterRM(.cmp, .from_rm, .byte, byte_stream),
        0x3b => try decodeRegisterRM(.cmp, .from_rm, .word, byte_stream),
        0x3c => try decodeAccImmediate(.cmp, .al, .byte, byte_stream),
        0x3d => try decodeAccImmediate(.cmp, .ax, .word, byte_stream),
        0x3e => unreachable, // segment DS
        0x3f => Instruction{ .length = 1, .type = .aas },

        0x40...0x47 => Instruction{
            .length = 1,
            .type = .inc,
            .dst = .{ .register = Register.fromInt(.word, @truncate(byte_stream[0])) },
        },
        0x48...0x4f => Instruction{
            .length = 1,
            .type = .dec,
            .dst = .{ .register = Register.fromInt(.word, @truncate(byte_stream[0])) },
        },

        0x50...0x57 => Instruction{
            .length = 1,
            .type = .push,
            .dst = .{ .register = Register.fromInt(.word, @truncate(byte_stream[0])) },
        },
        0x58...0x5f => Instruction{
            .length = 1,
            .type = .pop,
            .dst = .{ .register = Register.fromInt(.word, @truncate(byte_stream[0])) },
        },

        0x60...0x6f => Instruction{ .length = 1, .type = .illegal },

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
        0x86 => try decodeRegisterRM(.xchg, .to_rm, .byte, byte_stream),
        0x87 => try decodeRegisterRM(.xchg, .to_rm, .word, byte_stream),

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
            .src = .{ .register = Register.fromInt(.word, @truncate(byte_stream[0])) },
        },

        0x98 => Instruction{ .length = 1, .type = .cbw },
        0x99 => Instruction{ .length = 1, .type = .cwd },
        0x9a => try decodeFarLabelJump(.call, byte_stream),
        0x9b => Instruction{ .length = 1, .type = .wait },
        0x9c => Instruction{ .length = 1, .type = .pushf },
        0x9d => Instruction{ .length = 1, .type = .popf },
        0x9e => Instruction{ .length = 1, .type = .sahf },
        0x9f => Instruction{ .length = 1, .type = .lahf },

        0xa0 => try decodeMovAccMem(.to_acc, .byte, byte_stream),
        0xa1 => try decodeMovAccMem(.to_acc, .word, byte_stream),
        0xa2 => try decodeMovAccMem(.from_acc, .byte, byte_stream),
        0xa3 => try decodeMovAccMem(.from_acc, .word, byte_stream),

        0xa4 => Instruction{ .length = 1, .type = .movsb },
        0xa5 => Instruction{ .length = 1, .type = .movsw },
        0xa6 => Instruction{ .length = 1, .type = .cmpsb },
        0xa7 => Instruction{ .length = 1, .type = .cmpsw },

        0xa8 => try decodeAccImmediate(.@"test", .al, .byte, byte_stream),
        0xa9 => try decodeAccImmediate(.@"test", .ax, .word, byte_stream),

        0xaa => Instruction{ .length = 1, .type = .stosb },
        0xab => Instruction{ .length = 1, .type = .stosw },
        0xac => Instruction{ .length = 1, .type = .lodsb },
        0xad => Instruction{ .length = 1, .type = .lodsw },
        0xae => Instruction{ .length = 1, .type = .scasb },
        0xaf => Instruction{ .length = 1, .type = .scasw },

        0xb0...0xb7 => try decodeRegisterImmediate(.mov, .byte, byte_stream),
        0xb8...0xbf => try decodeRegisterImmediate(.mov, .word, byte_stream),

        0xc0...0xc1 => Instruction{ .length = 1, .type = .illegal },

        0xc2 => try decodeRetImmediate(.intrasegment, byte_stream),
        0xc3 => Instruction{ .length = 1, .type = .ret },

        0xc4 => try decodeAddressObject(.les, byte_stream),
        0xc5 => try decodeAddressObject(.lds, byte_stream),

        0xc6 => try decodeMemImmediate(.mov, .byte, byte_stream),
        0xc7 => try decodeMemImmediate(.mov, .word, byte_stream),

        0xc8...0xc9 => Instruction{ .length = 1, .type = .illegal },

        0xca => try decodeRetImmediate(.intersegment, byte_stream),
        0xcb => Instruction{ .length = 1, .type = .retf },

        0xcc => Instruction{ .length = 1, .type = .int3 },
        0xcd => try decodeIntImmediate(byte_stream),

        0xce => Instruction{ .length = 1, .type = .into },
        0xcf => Instruction{ .length = 1, .type = .iret },

        0xd0 => try decodeShift(.byte, .one, byte_stream),
        0xd1 => try decodeShift(.word, .one, byte_stream),
        0xd2 => try decodeShift(.byte, .cl, byte_stream),
        0xd3 => try decodeShift(.word, .cl, byte_stream),

        // TODO(benjamin): assert (byte_stream[1] == 0x0a) ?
        0xd4 => Instruction{ .length = 2, .type = .aam },
        0xd5 => Instruction{ .length = 2, .type = .aad },

        0xd6 => Instruction{ .length = 1, .type = .illegal },
        0xd7 => Instruction{ .length = 1, .type = .xlat },

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

        0xf0 => unreachable, // lock
        0xf1 => Instruction{ .length = 1, .type = .illegal },
        0xf2 => unreachable, // repnz
        0xf3 => unreachable, // repz

        0xf4 => Instruction{ .length = 1, .type = .hlt },
        0xf5 => Instruction{ .length = 1, .type = .cmc },

        0xf6 => try decodeGroup1(.byte, byte_stream),
        0xf7 => try decodeGroup1(.word, byte_stream),

        0xf8 => Instruction{ .length = 1, .type = .clc },
        0xf9 => Instruction{ .length = 1, .type = .stc },
        0xfa => Instruction{ .length = 1, .type = .cli },
        0xfb => Instruction{ .length = 1, .type = .sti },
        0xfc => Instruction{ .length = 1, .type = .cld },
        0xfd => Instruction{ .length = 1, .type = .std },

        0xfe => try decodeGroup2Byte(byte_stream),

        // 0xff is described as Group2 in the manual
        0xff => try decodeGroup2Word(byte_stream),
    };
}

test "illegal_0f" {
    const program: [1]u8 = .{0x0f};
    const instruction = try decodeNext(program[0..]);
    try std.testing.expectEqual(instruction.type, InstructionType.illegal);
}

pub fn decodeNext(byte_stream: []const u8) !Instruction {
    var lock: bool = false;
    var repeat: ?RepeatPrefix = null;
    var segment: ?SegmentRegister = null;
    // Shouldn't exceed 3
    var prefix_bytes: u8 = 0;

    for (byte_stream, 0..) |b, i| {
        switch (b) {
            0x26 => segment = .es,
            0x2e => segment = .cs,
            0x36 => segment = .ss,
            0x3e => segment = .ds,
            0xf0 => lock = true,
            0xf2 => repeat = .repnz,
            0xf3 => repeat = .repz,
            else => {
                prefix_bytes = @intCast(i);
                break;
            },
        }
    } else {
        return Error.IncompleteProgram;
    }

    var instruction = try decodeInstruction(byte_stream[prefix_bytes..]);

    instruction.lock = lock;
    instruction.repeat = repeat;
    instruction.segment = segment;
    instruction.length += prefix_bytes;

    return instruction;
}
