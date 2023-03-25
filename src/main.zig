const std = @import("std");

const InstructionType = enum {
    hlt,
};

const Instruction = struct {
    length: usize,
    type: InstructionType,
};

fn print_instruction(instruction: Instruction, writer: anytype) !void {
    switch (instruction.type) {
        .hlt => try writer.writeAll("hlt\n"),
    }
}

fn decode_instruction(byte_stream: []const u8) !Instruction {
    if (byte_stream.len < 1) {
        return error.IncompleteProgram;
    }
    switch (byte_stream[0]) {
        0xf4 => return Instruction{ .length = 1, .type = .hlt },
        else => return error.IllegalInstruction,
    }
    return 0;
}

fn decode_program(reader: anytype, writer: anytype) !void {
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
        const instruction = decode_instruction(stream_buf[stream_pos..stream_len]) catch |err| {
            std.debug.print("{s}: 0x{x}\n", .{ @errorName(err), stream_buf[stream_pos] });
            return err;
        };
        try print_instruction(instruction, writer);
        stream_pos += instruction.length;
    }
}

pub fn main() !void {
    var stdout = std.io.getStdOut().writer();
    var stdin = std.io.getStdIn().reader();

    try decode_program(stdin, stdout);
}
