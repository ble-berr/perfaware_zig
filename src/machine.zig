// 1MiB of addressable memory
pub var memory = [_]u8{0} ** (1024 * 1024);
pub var instruction_pointer: u16 = 0;

pub var segment_registers = [4]u16{ 0, 0, 0, 0 };
