const std = @import("std");

pub const NodeShape = enum { primary, unary, literal, binary };

pub const NodeTag = enum(u8) {
    // Primary expressions, no data
    nil,
    true,
    false,

    // Unary expressions, no data
    group = 0x10,
    not,
    unary_minus,

    // Literal expressions
    number = 0x20,
    string,

    // Binary expressions
    multiply = 0x30,
    divide,
    add,
    substract,
    greater,
    greater_equal,
    less,
    less_equal,
    equal,
    not_equal,

    const Self = @This();

    fn shape(self: Self) NodeShape {
        return @enumFromInt(@intFromEnum(self) >> 4);
    }

    fn shortString(self: Self) []const u8 {
        return switch (self) {
            .not => "!",
            .unary_minus, .substract => "-",
            .multiply => "*",
            .divide => "/",
            .add => "+",
            .greater => ">",
            .greater_equal => ">=",
            .less => "<",
            .less_equal => "<=",
            .equal => "==",
            .not_equal => "!=",
            else => @tagName(self),
        };
    }
};

pub const Data = union {
    empty: void,
    index: usize, // index to tags (left child of binary), or string_ends (string literal)
    number: f64,
};

tags: []const NodeTag, // whole tree, in postfix order
data: []const Data, // relevant data in same order as tags
string_ends: []const usize, // pointer to end of string; start of string is preceding entry
strings: []const u8, // internalized strings

const Ast = @This();

pub fn deinit(self: Ast, allocator: std.mem.Allocator) void {
    allocator.free(self.strings);
    allocator.free(self.string_ends);
    allocator.free(self.data);
    allocator.free(self.tags);
}

pub const Node = struct {
    ast: Ast,
    index: usize,

    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const tag = self.ast.tags[self.index];
        const short_string = tag.shortString();
        switch (tag.shape()) {
            .primary => try writer.writeAll(short_string),
            .unary => try writer.print("({s} {s})", .{ short_string, self.onlyChild() }),
            .literal => switch (tag) {
                .number => {
                    const literal_number = self.number();
                    try writer.print("{d}", .{literal_number});
                    if (literal_number == @trunc(literal_number)) try writer.writeAll(".0");
                },
                .string => try writer.writeAll(self.string()),
                else => unreachable,
            },
            .binary => try writer.print("({s} {s} {s})", .{ short_string, self.leftChild(), self.rightChild() }),
        }
    }

    fn data(self: Self) Data {
        return self.ast.data[self.index];
    }

    fn onlyChild(self: Self) Self {
        return self.rightChild();
    }

    fn number(self: Self) f64 {
        return self.data().number;
    }

    fn string(self: Self) []const u8 {
        const end_index = self.data().index;
        const start =
            if (end_index == 0) 0 else self.ast.string_ends[end_index - 1];
        const end = self.ast.string_ends[end_index];
        return self.ast.strings[start..end];
    }

    fn leftChild(self: Self) Self {
        return .{
            .ast = self.ast,
            .index = self.data().index,
        };
    }

    fn rightChild(self: Self) Self {
        return .{
            .ast = self.ast,
            .index = self.index - 1,
        };
    }
};

pub fn root(self: Ast) ?Node {
    if (self.tags.len == 0) return null;

    return .{
        .ast = self,
        .index = self.tags.len - 1,
    };
}
