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
    width: ?OperandWidth,
};

const DirectAddress = struct {
    address: u16,
    width: ?OperandWidth,
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

const InstructionPrefix = enum {
    lock,
    repz,
    repnz,
    // segment overrides
    es,
    cs,
    ss,
    ds,
};

const InstructionOperand = union(enum) {
    register: Register,
    direct_address: DirectAddress,
    effective_address: EffectiveAddress,
    immediate: ImmediateValue,
    short_jump: i8,
    near_jump: u16,
    far_jump: struct { ip: u16, sp: u16 },
    segment: SegmentRegister,
};

const InstructionType = enum {
    hlt,

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
};

const Instruction = struct {
    length: u8,
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
        .lea => "lea",
        .les => "les",
        .lds => "lds",
        .daa => "daa",
        .das => "das",
        .aaa => "aaa",
        .aas => "aas",
        .aam => "aam",
        .aad => "aad",
        .cbw => "cbw",
        .cwd => "cwd",
        .wait => "wait",
        .pushf => "pushf",
        .popf => "popf",
        .sahf => "sahf",
        .lahf => "lahf",
        .ret => "ret",
        .int => "int",
        .into => "into",
        .iret => "iret",
        .cmc => "cmc",
        .clc => "clc",
        .stc => "stc",
        .cli => "cli",
        .sti => "sti",
        .cld => "cld",
        .std => "std",
        .not => "not",
        .neg => "neg",
        .mul => "mul",
        .imul => "imul",
        .div => "div",
        .idiv => "idiv",
        .rol => "rol",
        .ror => "ror",
        .rcl => "rcl",
        .rcr => "rcr",
        .shl => "shl",
        .shr => "shr",
        .sar => "sar",
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

fn segmentPrefixMnemonic(opt_prefix: ?InstructionPrefix) []const u8 {
    return if (opt_prefix) |prefix| {
        return switch (prefix) {
            .es => "es:",
            .cs => "cs:",
            .ss => "ss:",
            .ds => "ds:",
            else => "",
        };
    } else "";
}

fn printOperand(
    operand: InstructionOperand,
    opt_prefix: ?InstructionPrefix,
    writer: anytype,
) !void {
    switch (operand) {
        .register => |register| {
            try writer.writeAll(registerMnemonic(register));
        },
        .direct_address => |direct_address| {
            const segment_prefix = segmentPrefixMnemonic(opt_prefix);

            try std.fmt.format(writer, "{s} {s}[{d}]", .{
                if (direct_address.width) |w| widthMnemonic(w) else "",
                segment_prefix,
                direct_address.address,
            });
        },
        .effective_address => |ea| {
            const segment_prefix = segmentPrefixMnemonic(opt_prefix);

            try std.fmt.format(writer, "{s} {s}[{s} {d:1}]", .{
                if (ea.width) |w| widthMnemonic(w) else "",
                segment_prefix,
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
            try writer.writeAll(segmentMnemonic(segment));
        },
    }
}

fn printInstruction(
    instruction: Instruction,
    opt_prefix: ?InstructionPrefix,
    writer: anytype,
) !void {
    if (opt_prefix) |prefix| {
        switch (prefix) {
            .lock => try writer.writeAll("lock "),
            .repz => try writer.writeAll("repz "),
            .repnz => try writer.writeAll("repnz "),
            else => {},
        }
    }

    try writer.writeAll(instructionMnemonic(instruction.type));

    if (instruction.dst) |dst| {
        try writer.writeAll(" ");
        try printOperand(dst, opt_prefix, writer);

        if (instruction.src) |src| {
            try writer.writeAll(", ");
            try printOperand(src, opt_prefix, writer);
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
        .length = 2,
        .type = instruction_type,
        .dst = .{ .near_jump = make16(byte_stream[1], byte_stream[2]) },
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
            .ip = make16(byte_stream[1], byte_stream[2]),
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
    const mod_operand = try getModOperand(mod_byte, .word, byte_stream[2..]);

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
            .one => .{ .immediate = .{ .value = 1, .width = .byte } },
            .cl => .{ .register = .cl },
        },
    };
}

fn decodeRetImmediate(byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 3) {
        return Error.IncompleteProgram;
    }

    return Instruction{
        .length = 3,
        .type = .ret,
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
    prefix: InstructionPrefix,
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
        0x26 => return .{ .prefix = .es },
        0x27 => Instruction{ .length = 1, .type = .daa, .dst = null, .src = null },

        0x28 => try decodeRegisterRM(.sub, .to_rm, .byte, byte_stream),
        0x29 => try decodeRegisterRM(.sub, .to_rm, .word, byte_stream),
        0x2a => try decodeRegisterRM(.sub, .from_rm, .byte, byte_stream),
        0x2b => try decodeRegisterRM(.sub, .from_rm, .word, byte_stream),
        0x2c => try decodeAccImmediate(.sub, .al, .byte, byte_stream),
        0x2d => try decodeAccImmediate(.sub, .ax, .word, byte_stream),
        0x2e => return .{ .prefix = .cs },
        0x2f => Instruction{ .length = 1, .type = .das, .dst = null, .src = null },

        0x30 => try decodeRegisterRM(.xor, .to_rm, .byte, byte_stream),
        0x31 => try decodeRegisterRM(.xor, .to_rm, .word, byte_stream),
        0x32 => try decodeRegisterRM(.xor, .from_rm, .byte, byte_stream),
        0x33 => try decodeRegisterRM(.xor, .from_rm, .word, byte_stream),
        0x34 => try decodeAccImmediate(.xor, .al, .byte, byte_stream),
        0x35 => try decodeAccImmediate(.xor, .ax, .word, byte_stream),
        0x36 => return .{ .prefix = .ss },
        0x37 => Instruction{ .length = 1, .type = .aaa, .dst = null, .src = null },

        0x38 => try decodeRegisterRM(.cmp, .to_rm, .byte, byte_stream),
        0x39 => try decodeRegisterRM(.cmp, .to_rm, .word, byte_stream),
        0x3a => try decodeRegisterRM(.cmp, .from_rm, .byte, byte_stream),
        0x3b => try decodeRegisterRM(.cmp, .from_rm, .word, byte_stream),
        0x3c => try decodeAccImmediate(.cmp, .al, .byte, byte_stream),
        0x3d => try decodeAccImmediate(.cmp, .ax, .word, byte_stream),
        0x3e => return .{ .prefix = .ds },
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

        // NOTE(benjamin): intrasegment.
        0xc2 => try decodeRetImmediate(byte_stream),
        0xc3 => Instruction{ .length = 1, .type = .ret, .dst = null, .src = null },

        0xc4 => try decodeAddressObject(.les, byte_stream),
        0xc5 => try decodeAddressObject(.lds, byte_stream),

        0xc6 => try decodeMemImmediate(.mov, .byte, byte_stream),
        0xc7 => try decodeMemImmediate(.mov, .word, byte_stream),

        0xc8...0xc9 => return Error.IllegalInstruction,
        // NOTE(benjamin): intersegment.
        0xca => try decodeRetImmediate(byte_stream),
        0xcb => Instruction{ .length = 1, .type = .ret, .dst = null, .src = null },

        0xcc => Instruction{
            .length = 1,
            .type = .int,
            .dst = .{ .immediate = .{ .value = 3, .width = .byte } },
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
        0xd4 => Instruction{ .length = 1, .type = .aam, .dst = null, .src = null },
        0xd5 => Instruction{ .length = 1, .type = .aad, .dst = null, .src = null },

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

        0xf0 => return .{ .prefix = .lock },
        0xf1 => return Error.IllegalInstruction,
        0xf2 => return .{ .prefix = .repnz },
        0xf3 => return .{ .prefix = .repz },

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

        // Store only the last prefix to the instruction, if any.
        var prefix: ?InstructionPrefix = null;
        var instruction: Instruction = while (true) {
            const decoded = decodeInstruction(stream_buf[stream_pos..stream_len]) catch |err| {
                std.debug.print("{s}: 0x{x:0>2}\n", .{ @errorName(err), stream_buf[stream_pos] });
                return err;
            };

            switch (decoded) {
                .instruction => |val| break val,
                .prefix => |val| {
                    try std.fmt.format(writer, "; 0x{x:0>2}\n", .{stream_buf[stream_pos]});
                    prefix = val;
                    stream_pos += 1;
                },
            }
        };

        if (instruction.type == .out) {
            std.mem.swap(?InstructionOperand, &instruction.src, &instruction.dst);
        }

        try printInstruction(instruction, prefix, writer);
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
