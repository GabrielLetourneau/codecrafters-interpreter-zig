const std = @import("std");

pub const NodeTag = enum(u8) {
    nil,
    true,
    false,
};

pub const Node = struct {
    ast: Ast,
    tag_index: u32,

    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll(@tagName(self.ast.tags[self.tag_index]));
    }
};

tags: []const NodeTag,

const Ast = @This();

pub fn head(self: Ast) ?Node {
    if (self.tags.len == 0) return null;
    return .{ .ast = self, .tag_index = 0 };
}
