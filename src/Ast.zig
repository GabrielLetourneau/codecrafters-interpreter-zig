const std = @import("std");

pub const NodeShape = enum { primary, unary, literal, binary };

pub const NodeTag = enum(u8) {
    // Primary expressions, no data
    nil,
    true,
    false,
    undefined,

    // Unary expressions, no data
    group = 0x10,
    not,
    unary_minus,

    // Statements, child expression, no data
    discard,
    print,

    // Literal expressions
    number = 0x20,
    string,

    // Statement, data
    alloc_frame, // data is frame variable count
    var_decl, // data is variable index
    variable, // data is variable index

    // Statements, child expression, data
    var_decl_init,
    assignment,

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
string_starts: []const usize, // pointer to start of string; end of string is followin entry
strings: []const u8, // internalized strings

const Ast = @This();

pub fn deinit(self: Ast, allocator: std.mem.Allocator) void {
    allocator.free(self.strings);
    allocator.free(self.string_starts);
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
                .number => try @import("number.zig").format(self.number(), writer),
                .string => try writer.writeAll(self.string()),
                else => unreachable,
            },
            .binary => try writer.print("({s} {s} {s})", .{ short_string, self.leftChild(), self.rightChild() }),
        }
    }

    pub fn dataIndex(self: Self) usize {
        return self.data().index;
    }

    pub fn string(self: Self) []const u8 {
        const index = self.data().index;
        const start = self.ast.string_starts[index];
        const end = self.ast.string_starts[index + 1];
        return self.ast.strings[start..end];
    }

    pub fn number(self: Self) f64 {
        return self.data().number;
    }

    fn data(self: Self) Data {
        return self.ast.data[self.index];
    }

    fn onlyChild(self: Self) Self {
        return self.rightChild();
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

pub fn node(self: Ast, index: usize) Node {
    return .{ .ast = self, .index = index };
}

pub fn root(self: Ast) ?Node {
    if (self.tags.len == 0) return null;

    return .{
        .ast = self,
        .index = self.tags.len - 1,
    };
}
