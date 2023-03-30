// 1MiB of addressable memory
pub var memory = [_]u8{0} ** (1024 * 1024);
pub var code_segment_pointer: u32 = 0;
pub var instruction_pointer: u32 = 0;
