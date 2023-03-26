const std = @import("std");

const OperandWidth = enum {
    byte,
    word,
};

const Register = enum {
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
    sum_bx_si,
    sum_bx_di,
    sum_bp_si,
    sum_bp_di,
    si,
    di,
    bp,
    bx,
};

const EffectiveAddress = struct {
    base: EacBase,
    offset: u16,
    width: OperandWidth,
};

const DirectAddress = struct {
    address: u16,
    width: OperandWidth,
};

const ImmediateValue = struct {
    value: u16,
    width: OperandWidth,
};

const SegmentRegister = enum {
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
    jump: i8,
    segment: SegmentRegister,
};

const InstructionType = enum {
    hlt,
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
    jmp,
    push,
    pop,
    @"test",
    xchg,
    in,
    out,
    xlat,
};

const Instruction = struct {
    length: usize,
    type: InstructionType,
    dst: ?InstructionOperand,
    src: ?InstructionOperand,
};

const Error = error{
    IllegalInstruction,
    IncompleteProgram,
    UnsupportedOperation,
};

// TODO(benjamin): some comptime magic to generate these
fn instructionMnemonic(instruction: InstructionType) []const u8 {
    return switch (instruction) {
        .hlt => "hlt",
        .movsb => "movsb",
        .cmpsb => "cmpsb",
        .stosb => "stosb",
        .lodsb => "lodsb",
        .scasb => "scasb",
        .movsw => "movsw",
        .cmpsw => "cmpsw",
        .stosw => "stosw",
        .lodsw => "lodsw",
        .scasw => "scasw",
        .mov => "mov",
        .add => "add",
        .@"or" => "or",
        .adc => "adc",
        .sbb => "sbb",
        .@"and" => "and",
        .sub => "sub",
        .xor => "xor",
        .cmp => "cmp",
        .jo => "jo",
        .jno => "jno",
        .jb => "jb",
        .jnb => "jnb",
        .je => "je",
        .jne => "jne",
        .jbe => "jbe",
        .jnbe => "jnbe",
        .js => "js",
        .jns => "jns",
        .jp => "jp",
        .jnp => "jnp",
        .jl => "jl",
        .jnl => "jnl",
        .jle => "jle",
        .jnle => "jnle",
        .loopne => "loopne",
        .loope => "loope",
        .loop => "loop",
        .jcxz => "jcxz",
        .inc => "inc",
        .dec => "dec",
        .call => "call",
        .jmp => "jmp",
        .push => "push",
        .pop => "pop",
        .@"test" => "test",
        .xchg => "xchg",
        .in => "in",
        .out => "out",
        .xlat => "xlat",
    };
}

fn registerMnemonic(register: Register) []const u8 {
    return switch (register) {
        .al => "al",
        .cl => "cl",
        .dl => "dl",
        .bl => "bl",
        .ah => "ah",
        .ch => "ch",
        .dh => "dh",
        .bh => "bh",
        .ax => "ax",
        .cx => "cx",
        .dx => "dx",
        .bx => "bx",
        .sp => "sp",
        .bp => "bp",
        .si => "si",
        .di => "di",
    };
}

fn eacBaseMnemonic(base: EacBase) []const u8 {
    return switch (base) {
        .sum_bx_si => "bx + si",
        .sum_bx_di => "bx + di",
        .sum_bp_si => "bp + si",
        .sum_bp_di => "bp + di",
        .si => "si",
        .di => "di",
        .bp => "bp",
        .bx => "bx",
    };
}

fn widthMnemonic(width: OperandWidth) []const u8 {
    return switch (width) {
        .byte => "byte",
        .word => "word",
    };
}

fn segmentMnemonic(segment: SegmentRegister) []const u8 {
    return switch (segment) {
        .es => "es",
        .cs => "cs",
        .ss => "ss",
        .ds => "ds",
    };
}

fn printOperand(operand: InstructionOperand, writer: anytype) !void {
    switch (operand) {
        .register => |register| {
            try writer.writeAll(registerMnemonic(register));
        },
        .direct_address => |direct_address| {
            try std.fmt.format(writer, "{s} [{d}]", .{
                widthMnemonic(direct_address.width),
                direct_address.address,
            });
        },
        .effective_address => |ea| {
            try std.fmt.format(writer, "{s} [{s} {d:1}]", .{
                widthMnemonic(ea.width),
                eacBaseMnemonic(ea.base),
                @bitCast(i16, ea.offset),
            });
        },
        .immediate => |immediate| {
            try std.fmt.format(writer, "{s} {d}", .{
                widthMnemonic(immediate.width),
                immediate.value,
            });
        },
        .jump => |jump| {
            // NOTE(benjamin): 'nasm' does not account for the instruction
            // length so we must add it here. Note that we're assuming that
            // only short jumps are supported and so are hardcoding an
            // instruction length of 2.
            try std.fmt.format(writer, "${d:1}", .{@as(i16, jump) + 2});
        },
        .segment => |segment| {
            try writer.writeAll(segmentMnemonic(segment));
        },
    }
}

fn printInstruction(instruction: Instruction, writer: anytype) !void {
    try writer.writeAll(instructionMnemonic(instruction.type));

    if (instruction.dst) |dst| {
        try writer.writeAll(" ");
        try printOperand(dst, writer);

        if (instruction.src) |src| {
            try writer.writeAll(", ");
            try printOperand(src, writer);
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
        .mod = @intCast(u2, (mod_byte & 0o300) >> 6),
        .a = @intCast(u3, (mod_byte & 0o070) >> 3),
        .b = @intCast(u3, (mod_byte & 0o007) >> 0),
    };
}

const ModOperand = struct {
    length: usize,
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
    length: u16,
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
        .dst = .{ .jump = @bitCast(i8, byte_stream[1]) },
        .src = null,
    };
}

fn decodeGroup2(byte_stream: []const u8) !Instruction {
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
            2, 3 => .call,
            4, 5 => .jmp,
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

fn decodeInstruction(byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 1) {
        return Error.IncompleteProgram;
    }

    return switch (byte_stream[0]) {
        0x00 => decodeRegisterRM(.add, .to_rm, .byte, byte_stream),
        0x01 => decodeRegisterRM(.add, .to_rm, .word, byte_stream),
        0x02 => decodeRegisterRM(.add, .from_rm, .byte, byte_stream),
        0x03 => decodeRegisterRM(.add, .from_rm, .word, byte_stream),
        0x04 => decodeAccImmediate(.add, .al, .byte, byte_stream),
        0x05 => decodeAccImmediate(.add, .ax, .word, byte_stream),
        0x06 => Instruction{ .length = 1, .type = .push, .dst = .{ .segment = .es }, .src = null },
        0x07 => Instruction{ .length = 1, .type = .pop, .dst = .{ .segment = .es }, .src = null },

        0x08 => decodeRegisterRM(.@"or", .to_rm, .byte, byte_stream),
        0x09 => decodeRegisterRM(.@"or", .to_rm, .word, byte_stream),
        0x0a => decodeRegisterRM(.@"or", .from_rm, .byte, byte_stream),
        0x0b => decodeRegisterRM(.@"or", .from_rm, .word, byte_stream),
        0x0c => decodeAccImmediate(.@"or", .al, .byte, byte_stream),
        0x0d => decodeAccImmediate(.@"or", .ax, .word, byte_stream),
        0x0e => Instruction{ .length = 1, .type = .push, .dst = .{ .segment = .cs }, .src = null },
        0x0f => Error.IllegalInstruction,

        0x10 => decodeRegisterRM(.adc, .to_rm, .byte, byte_stream),
        0x11 => decodeRegisterRM(.adc, .to_rm, .word, byte_stream),
        0x12 => decodeRegisterRM(.adc, .from_rm, .byte, byte_stream),
        0x13 => decodeRegisterRM(.adc, .from_rm, .word, byte_stream),
        0x14 => decodeAccImmediate(.adc, .al, .byte, byte_stream),
        0x15 => decodeAccImmediate(.adc, .ax, .word, byte_stream),
        0x16 => Instruction{ .length = 1, .type = .push, .dst = .{ .segment = .ss }, .src = null },
        0x17 => Instruction{ .length = 1, .type = .pop, .dst = .{ .segment = .ss }, .src = null },

        0x18 => decodeRegisterRM(.sbb, .to_rm, .byte, byte_stream),
        0x19 => decodeRegisterRM(.sbb, .to_rm, .word, byte_stream),
        0x1a => decodeRegisterRM(.sbb, .from_rm, .byte, byte_stream),
        0x1b => decodeRegisterRM(.sbb, .from_rm, .word, byte_stream),
        0x1c => decodeAccImmediate(.sbb, .al, .byte, byte_stream),
        0x1d => decodeAccImmediate(.sbb, .ax, .word, byte_stream),
        0x1e => Instruction{ .length = 1, .type = .push, .dst = .{ .segment = .ds }, .src = null },
        0x1f => Instruction{ .length = 1, .type = .pop, .dst = .{ .segment = .ds }, .src = null },

        0x20 => decodeRegisterRM(.@"and", .to_rm, .byte, byte_stream),
        0x21 => decodeRegisterRM(.@"and", .to_rm, .word, byte_stream),
        0x22 => decodeRegisterRM(.@"and", .from_rm, .byte, byte_stream),
        0x23 => decodeRegisterRM(.@"and", .from_rm, .word, byte_stream),
        0x24 => decodeAccImmediate(.@"and", .al, .byte, byte_stream),
        0x25 => decodeAccImmediate(.@"and", .ax, .word, byte_stream),

        0x26...0x27 => error.InstructionNotImplemented,

        0x28 => decodeRegisterRM(.sub, .to_rm, .byte, byte_stream),
        0x29 => decodeRegisterRM(.sub, .to_rm, .word, byte_stream),
        0x2a => decodeRegisterRM(.sub, .from_rm, .byte, byte_stream),
        0x2b => decodeRegisterRM(.sub, .from_rm, .word, byte_stream),
        0x2c => decodeAccImmediate(.sub, .al, .byte, byte_stream),
        0x2d => decodeAccImmediate(.sub, .ax, .word, byte_stream),

        0x2e...0x2f => error.InstructionNotImplemented,

        0x30 => decodeRegisterRM(.xor, .to_rm, .byte, byte_stream),
        0x31 => decodeRegisterRM(.xor, .to_rm, .word, byte_stream),
        0x32 => decodeRegisterRM(.xor, .from_rm, .byte, byte_stream),
        0x33 => decodeRegisterRM(.xor, .from_rm, .word, byte_stream),
        0x34 => decodeAccImmediate(.xor, .al, .byte, byte_stream),
        0x35 => decodeAccImmediate(.xor, .ax, .word, byte_stream),

        0x36...0x37 => error.InstructionNotImplemented,

        0x38 => decodeRegisterRM(.cmp, .to_rm, .byte, byte_stream),
        0x39 => decodeRegisterRM(.cmp, .to_rm, .word, byte_stream),
        0x3a => decodeRegisterRM(.cmp, .from_rm, .byte, byte_stream),
        0x3b => decodeRegisterRM(.cmp, .from_rm, .word, byte_stream),
        0x3c => decodeAccImmediate(.cmp, .al, .byte, byte_stream),
        0x3d => decodeAccImmediate(.cmp, .ax, .word, byte_stream),

        0x3e...0x3f => error.InstructionNotImplemented,

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

        0x60...0x6f => Error.IllegalInstruction,

        0x70 => decodeShortLabelJump(.jo, byte_stream),
        0x71 => decodeShortLabelJump(.jno, byte_stream),
        0x72 => decodeShortLabelJump(.jb, byte_stream),
        0x73 => decodeShortLabelJump(.jnb, byte_stream),
        0x74 => decodeShortLabelJump(.je, byte_stream),
        0x75 => decodeShortLabelJump(.jne, byte_stream),
        0x76 => decodeShortLabelJump(.jbe, byte_stream),
        0x77 => decodeShortLabelJump(.jnbe, byte_stream),
        0x78 => decodeShortLabelJump(.js, byte_stream),
        0x79 => decodeShortLabelJump(.jns, byte_stream),
        0x7a => decodeShortLabelJump(.jp, byte_stream),
        0x7b => decodeShortLabelJump(.jnp, byte_stream),
        0x7c => decodeShortLabelJump(.jl, byte_stream),
        0x7d => decodeShortLabelJump(.jnl, byte_stream),
        0x7e => decodeShortLabelJump(.jle, byte_stream),
        0x7f => decodeShortLabelJump(.jnle, byte_stream),

        0x80 => decodeOpRmImmediate(.byte, byte_stream),
        0x81 => decodeOpRmImmediate(.word, byte_stream),

        0x82 => decodeOpRmExtended(.byte, byte_stream), // Duplicates 0x80?
        0x83 => decodeOpRmExtended(.word, byte_stream),

        // Order shouldn't matter for these but we'll follow the convention
        // from table 4-13 of the user's manual (9800722-03).
        0x84 => decodeRegisterRM(.@"test", .to_rm, .byte, byte_stream),
        0x85 => decodeRegisterRM(.@"test", .to_rm, .word, byte_stream),
        0x86 => decodeRegisterRM(.xchg, .from_rm, .byte, byte_stream),
        0x87 => decodeRegisterRM(.xchg, .from_rm, .word, byte_stream),

        0x88 => decodeRegisterRM(.mov, .to_rm, .byte, byte_stream),
        0x89 => decodeRegisterRM(.mov, .to_rm, .word, byte_stream),
        0x8a => decodeRegisterRM(.mov, .from_rm, .byte, byte_stream),
        0x8b => decodeRegisterRM(.mov, .from_rm, .word, byte_stream),

        0x8c...0x8e => error.InstructionNotImplemented,

        0x8f => decodePopRM16(byte_stream),

        // NOTE(benjamin): 0x90 is NOP (xchg ax, ax).
        0x90...0x97 => Instruction{
            .length = 1,
            .type = .xchg,
            .dst = .{ .register = .ax },
            .src = .{ .register = Register.fromInt(.word, @truncate(u3, byte_stream[0])) },
        },

        0x98...0x9f => error.InstructionNotImplemented,

        0xa0 => decodeMovAccMem(.to_acc, .byte, byte_stream),
        0xa1 => decodeMovAccMem(.to_acc, .word, byte_stream),
        0xa2 => decodeMovAccMem(.from_acc, .byte, byte_stream),
        0xa3 => decodeMovAccMem(.from_acc, .word, byte_stream),

        0xa4 => Instruction{ .length = 1, .type = .movsb, .dst = null, .src = null },
        0xa5 => Instruction{ .length = 1, .type = .movsw, .dst = null, .src = null },
        0xa6 => Instruction{ .length = 1, .type = .cmpsb, .dst = null, .src = null },
        0xa7 => Instruction{ .length = 1, .type = .cmpsw, .dst = null, .src = null },

        0xa8...0xa9 => error.InstructionNotImplemented,

        0xaa => Instruction{ .length = 1, .type = .stosb, .dst = null, .src = null },
        0xab => Instruction{ .length = 1, .type = .stosw, .dst = null, .src = null },
        0xac => Instruction{ .length = 1, .type = .lodsb, .dst = null, .src = null },
        0xad => Instruction{ .length = 1, .type = .lodsw, .dst = null, .src = null },
        0xae => Instruction{ .length = 1, .type = .scasb, .dst = null, .src = null },
        0xaf => Instruction{ .length = 1, .type = .scasw, .dst = null, .src = null },

        0xb0...0xb7 => decodeRegisterImmediate(.mov, .byte, byte_stream),
        0xb8...0xbf => decodeRegisterImmediate(.mov, .word, byte_stream),

        0xc0...0xc5 => error.InstructionNotImplemented,

        0xc6 => decodeMemImmediate(.mov, .byte, byte_stream),
        0xc7 => decodeMemImmediate(.mov, .word, byte_stream),

        0xc8...0xd6 => error.InstructionNotImplemented,

        0xd7 => Instruction{ .length = 1, .type = .xlat, .dst = null, .src = null },

        0xd8...0xdf => error.InstructionNotImplemented,

        0xe0 => decodeShortLabelJump(.loopne, byte_stream),
        0xe1 => decodeShortLabelJump(.loope, byte_stream),
        0xe2 => decodeShortLabelJump(.loop, byte_stream),
        0xe3 => decodeShortLabelJump(.jcxz, byte_stream),
        0xe4 => decodeAccImmediate(.in, .al, .byte, byte_stream),
        0xe5 => decodeAccImmediate(.in, .ax, .byte, byte_stream),
        0xe6 => decodeAccImmediate(.out, .al, .byte, byte_stream),
        0xe7 => decodeAccImmediate(.out, .ax, .byte, byte_stream),

        0xe8...0xea => error.InstructionNotImplemented,

        0xeb => decodeShortLabelJump(.jmp, byte_stream),
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

        0xf0...0xf3 => error.InstructionNotImplemented,

        0xf4 => Instruction{ .length = 1, .type = .hlt, .dst = null, .src = null },

        0xf5...0xfe => error.InstructionNotImplemented,

        // 0xff is described as Group2 in the manual
        0xff => decodeGroup2(byte_stream),
    };
}

fn decodeProgram(reader: anytype, writer: anytype) !void {
    var stream_buf: [512]u8 = undefined;

    var stream_len: usize = try reader.read(stream_buf[0..]);
    var stream_pos: usize = 0;
    var eof: bool = false;

    while (stream_pos < stream_len) {
        if (!eof) {
            // Longest instructions are 6 bytes long, there should be no risk of
            // outrunning the buffer if we append under 6.
            const remaining = stream_len - stream_pos;
            if (remaining < 6) {
                for (0..remaining) |i| {
                    stream_buf[i] = stream_buf[stream_pos + i];
                }
                stream_len = remaining + try reader.read(stream_buf[remaining..]);
                eof = (stream_len == remaining);
                stream_pos = 0;
            }
        }
        const instruction = decodeInstruction(stream_buf[stream_pos..stream_len]) catch |err| {
            std.debug.print("{s}: 0x{x:0>2}\n", .{ @errorName(err), stream_buf[stream_pos] });
            return err;
        };

        try printInstruction(instruction, writer);
        try writer.writeAll(" ;");
        for (stream_buf[stream_pos..(stream_pos + instruction.length)]) |byte| {
            try std.fmt.format(writer, " 0x{x:0>2}", .{byte});
        }
        try writer.writeAll("\n");

        stream_pos += instruction.length;
    }
}

pub fn main() !void {
    var stdout = std.io.getStdOut().writer();
    var stdin = std.io.getStdIn().reader();

    try decodeProgram(stdin, stdout);
}
