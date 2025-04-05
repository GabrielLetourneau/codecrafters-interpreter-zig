const std = @import("std");

pub fn format(number: f64, writer: anytype) !void {
    try writer.print("{d}", .{number});
    if (number == @trunc(number)) try writer.writeAll(".0");
}
