// 1MiB of addressable memory
pub var memory = [_]u8{0} ** (1024 * 1024);
pub var instruction_pointer: u16 = 0;

pub var word_registers = [8]u16{ 0, 0, 0, 0, 0, 0, 0, 0 };
pub const byte_registers = @ptrCast(*[8]u8, &word_registers);

pub var segment_registers = [4]u16{ 0, 0, 0, 0 };
