// 1MiB of addressable memory
pub var memory = [_]u8{0} ** (1024 * 1024);
pub var instruction_pointer: u16 = 0;

pub var word_registers = [8]u16{ 0, 0, 0, 0, 0, 0, 0, 0 };
pub const byte_registers: *[8]u8 = @ptrCast(&word_registers);

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
    memory = .{0} ** memory.len;
    word_registers = .{0} ** word_registers.len;
    segment_registers = .{0} ** segment_registers.len;
    instruction_pointer = 0;
    flags = .{
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
