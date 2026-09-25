const std = @import("std");

pub fn format(number: f64, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    try writer.print("{d}", .{number});
    if (number == @trunc(number)) try writer.writeAll(".0");
}
