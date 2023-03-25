const std = @import("std");

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
};

const Instruction = struct {
    length: usize,
    type: InstructionType,
};

// TODO(benjamin): some comptime magic to generate this
fn instructionMnemonic(instruction_type: InstructionType) []const u8 {
    return switch (instruction_type) {
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
    };
}

fn printInstruction(instruction: Instruction, writer: anytype) !void {
    try writer.writeAll(instructionMnemonic(instruction.type));
}

fn decodeInstruction(byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 1) {
        return error.IncompleteProgram;
    }
    switch (byte_stream[0]) {
        0xa4 => return Instruction{ .length = 1, .type = .movsb },
        0xa5 => return Instruction{ .length = 1, .type = .movsw },
        0xa6 => return Instruction{ .length = 1, .type = .cmpsb },
        0xa7 => return Instruction{ .length = 1, .type = .cmpsw },

        0xaa => return Instruction{ .length = 1, .type = .stosb },
        0xab => return Instruction{ .length = 1, .type = .stosw },
        0xac => return Instruction{ .length = 1, .type = .lodsb },
        0xad => return Instruction{ .length = 1, .type = .lodsw },
        0xae => return Instruction{ .length = 1, .type = .scasb },
        0xaf => return Instruction{ .length = 1, .type = .scasw },

        0xf4 => return Instruction{ .length = 1, .type = .hlt },
        else => return error.IllegalInstruction,
    }
    return 0;
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
