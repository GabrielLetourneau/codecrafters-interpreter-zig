const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

pub const RootSymbol = enum { program, expression };

pub const NodeTag = enum(u8) {
    // Primary expressions: no child, no data
    nil,
    true,
    false,
    empty,

    // Unary expressions: one child, no data
    group,
    not,
    unary_minus,
    print,
    @"return",
    block,

    // Data expressions: no child, data
    number,
    string,
    var_decl,
    variable,
    parameter,

    // Bindings: one child, identifier data
    var_decl_init,
    assignment,
    fun_decl,

    // Binary expressions: data points to left-hand child, immediate child is right-hand child
    declarations, // lhs is .empty or declaration; rhs is single declaration
    arguments,
    call,
    fun_def,
    @"if",
    @"else",
    @"while",
    for_init_cond,
    for_preamble,
    @"for",
    multiply,
    divide,
    add,
    substract,
    greater,
    greater_equal,
    less,
    less_equal,
    equal,
    not_equal,
    @"or",
    @"and",

    const Self = @This();

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
string_starts: []const usize, // pointer to start of string; end of string is following entry
strings: []const u8, // internalized strings

const Ast = @This();

pub fn deinitStrings(self: Ast, allocator: Allocator) void {
    allocator.free(self.strings);
    allocator.free(self.string_starts);
}

pub fn deinit(self: Ast, allocator: Allocator) void {
    allocator.free(self.data);
    allocator.free(self.tags);
}

pub const Node = struct {
    ast: *const Ast,
    tag_index: usize,

    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.tag()) {
            .nil, .true, .false => try writer.writeAll(self.tag().shortString()),
            .empty => {},
            .group, .not, .unary_minus, .print => try writer.print("({s} {s})", .{ self.tag().shortString(), self.onlyChild() }),
            .block => try writer.print("{{{s}\n}}", .{self.onlyChild()}),
            .number => try @import("number.zig").format(self.number(), writer),
            .string => try writer.writeAll(self.string()),
            .var_decl, .variable => try writer.print("({s} {d})", .{ self.tag().shortString(), self.identifier() }),
            .var_decl_init, .assignment => try writer.print("({s} {d} {s})", .{ self.tag().shortString(), self.identifier(), self.onlyChild() }),
            .declarations => try writer.print("{s}\n{s}", .{ self.leftChild(), self.rightChild() }),
            .@"if", .@"while" => try writer.print(
                "{s} ({s}) {s}",
                .{
                    self.tag().shortString(),
                    self.leftChild(),
                    self.rightChild(),
                },
            ),
            .@"else" => {
                const if_statement = self.leftChild();
                try writer.print(
                    "if ({s}) {s} else {s}",
                    .{ if_statement.leftChild(), if_statement.rightChild(), self.rightChild() },
                );
            },
            .@"for" => {
                const for_preamble = self.leftChild();
                const for_cond_init = for_preamble.leftChild();
                try writer.print(
                    "for ({s}; {s}; {s}) {s}",
                    .{
                        for_cond_init.leftChild(),
                        for_cond_init.rightChild(),
                        for_preamble.rightChild(),
                        self.rightChild(),
                    },
                );
            },
            else => try writer.print("({s} {s} {s})", .{ self.tag().shortString(), self.leftChild(), self.rightChild() }),
        }
    }

    pub fn tag(self: Self) NodeTag {
        return self.ast.tags[self.tag_index];
    }

    pub fn identifier(self: Self) usize {
        assert(switch (self.tag()) {
            .var_decl, .variable, .parameter, .var_decl_init, .assignment, .fun_decl => true,
            else => false,
        });

        return self.data().index;
    }

    pub fn stringIndex(self: Self) usize {
        assert(self.tag() == .string);

        return self.data().index;
    }

    pub fn number(self: Self) f64 {
        assert(self.tag() == .number);

        return self.data().number;
    }

    pub fn onlyChild(self: Self) Self {
        assert(switch (self.tag()) {
            .group, .not, .unary_minus, .print, .@"return", .block, .parameter, .var_decl_init, .assignment, .fun_decl => true,
            else => false,
        });

        return .{
            .ast = self.ast,
            .tag_index = self.tag_index - 1,
        };
    }

    pub fn leftChild(self: Self) Self {
        assert(@intFromEnum(self.tag()) >= @intFromEnum(NodeTag.declarations));

        return .{
            .ast = self.ast,
            .tag_index = self.data().index,
        };
    }

    pub fn rightChild(self: Self) Self {
        assert(@intFromEnum(self.tag()) >= @intFromEnum(NodeTag.declarations));

        return .{
            .ast = self.ast,
            .tag_index = self.tag_index - 1,
        };
    }

    fn string(self: Self) []const u8 {
        const index_data = self.stringIndex();
        const start = self.ast.string_starts[index_data];
        const end = self.ast.string_starts[index_data + 1];
        return self.ast.strings[start..end];
    }

    fn data(self: Self) Data {
        return self.ast.data[self.tag_index];
    }
};

pub fn root(self: *const Ast) ?Node {
    if (self.tags.len == 0) return null;

    return .{
        .ast = self,
        .tag_index = self.tags.len - 1,
    };
}
