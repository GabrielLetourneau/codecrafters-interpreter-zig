const std = @import("std");

pub const NodeTag = enum(u8) {
    // Primary expressions, no data
    nil,
    true,
    false,

    // Primary Expressions, with data
    number,
};

pub const Data = union {
    number: f64,
};

pub const Node = struct {
    ast: Ast,
    tag_index: u32,
    data_index: u32,

    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.ast.tags[self.tag_index]) {
            .nil, .true, .false => try writer.writeAll(@tagName(self.ast.tags[self.tag_index])),
            .number => {
                const literal_number = self.ast.data[self.data_index].number;
                try writer.print("{d}", .{literal_number});
                if (literal_number == @trunc(literal_number)) try writer.writeAll(".0");
            },
        }
    }
};

tags: []const NodeTag,
data: []const Data,

const Ast = @This();

pub fn head(self: Ast) ?Node {
    if (self.tags.len == 0) return null;
    return .{ .ast = self, .tag_index = 0, .data_index = 0 };
}
