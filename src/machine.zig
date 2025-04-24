// 1MiB of addressable memory
pub var memory = [_]u8{0} ** (1024 * 1024);
pub var instruction_pointer: u16 = 0;

pub var word_registers = [8]u16{ 0, 0, 0, 0, 0, 0, 0, 0 };
pub const byte_registers = @as(*[8]u8, @ptrCast(&word_registers));

pub var segment_registers = [4]u16{ 0, 0, 0, 0 };

pub const Flags = struct {
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

pub var flags = Flags{
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

pub fn reset() void {
    for (0..memory.len) |i| {
        memory[i] = 0;
    }

    instruction_pointer = 0;

    for (0..word_registers.len) |i| {
        word_registers[i] = 0;
    }

    for (0..segment_registers.len) |i| {
        segment_registers[i] = 0;
    }

    flags = Flags{
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
