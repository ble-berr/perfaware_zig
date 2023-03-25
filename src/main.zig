const std = @import("std");

pub fn main() !void {
    var stdout = std.io.getStdOut();
    try stdout.writeAll("hello world\n");
}
