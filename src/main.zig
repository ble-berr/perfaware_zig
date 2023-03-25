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

const InstructionOperand = union(enum) {
    register: Register,
    direct_address: DirectAddress,
    effective_address: EffectiveAddress,
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
                ea.offset,
            });
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

fn parse_mod_byte(mod_byte: u8) ModByte {
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

fn get_mod_operand(mod_byte: ModByte, width: OperandWidth, byte_stream: []const u8) !ModOperand {
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
                        .offset = extend8(byte_stream[1]),
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

fn decode_r_rm(
    instruction_type: InstructionType,
    direction: enum { to_rm, from_rm },
    width: OperandWidth,
    byte_stream: []const u8,
) !Instruction {
    var instruction = Instruction{
        .length = 2,
        .type = instruction_type,
        .src = undefined,
        .dst = undefined,
    };

    if (byte_stream.len < instruction.length) {
        return Error.IncompleteProgram;
    }

    const mod_byte = parse_mod_byte(byte_stream[1]);
    const mod_operand = try get_mod_operand(mod_byte, width, byte_stream[2..]);

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

fn decodeInstruction(byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 1) {
        return Error.IncompleteProgram;
    }

    return switch (byte_stream[0]) {
        0x88 => decode_r_rm(.mov, .to_rm, .byte, byte_stream),
        0x89 => decode_r_rm(.mov, .to_rm, .word, byte_stream),
        0x8a => decode_r_rm(.mov, .from_rm, .byte, byte_stream),
        0x8b => decode_r_rm(.mov, .from_rm, .word, byte_stream),

        0xa4 => Instruction{ .length = 1, .type = .movsb, .dst = null, .src = null },
        0xa5 => Instruction{ .length = 1, .type = .movsw, .dst = null, .src = null },
        0xa6 => Instruction{ .length = 1, .type = .cmpsb, .dst = null, .src = null },
        0xa7 => Instruction{ .length = 1, .type = .cmpsw, .dst = null, .src = null },

        0xaa => Instruction{ .length = 1, .type = .stosb, .dst = null, .src = null },
        0xab => Instruction{ .length = 1, .type = .stosw, .dst = null, .src = null },
        0xac => Instruction{ .length = 1, .type = .lodsb, .dst = null, .src = null },
        0xad => Instruction{ .length = 1, .type = .lodsw, .dst = null, .src = null },
        0xae => Instruction{ .length = 1, .type = .scasb, .dst = null, .src = null },
        0xaf => Instruction{ .length = 1, .type = .scasw, .dst = null, .src = null },

        0xf4 => Instruction{ .length = 1, .type = .hlt, .dst = null, .src = null },

        else => Error.IllegalInstruction,
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
            std.debug.print("{s}: 0x{x}\n", .{ @errorName(err), stream_buf[stream_pos] });
            return err;
        };

        try printInstruction(instruction, writer);
        try writer.writeAll(" ;");
        for (stream_buf[stream_pos..(stream_pos + instruction.length)]) |byte| {
            try std.fmt.format(writer, " 0x{x}", .{byte});
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
