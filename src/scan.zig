const std = @import("std");

pub const TokenTag = enum {
    // Single-character tokens.
    left_paren,
    right_paren,

    // end of unit
    eof,
};

pub const Literal = union {
    empty: void,
};

const empty_literal: Literal = .{ .empty = {} };

pub const Token = struct {
    tag: TokenTag,
    lexeme: []const u8,
    literal: Literal,

    pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        for (@tagName(self.tag)) |char| {
            try writer.writeByte(std.ascii.toUpper(char));
        }
        try writer.print(" {s} null", .{self.lexeme});
    }
};

pub fn scan(source: []const u8, allocator: std.mem.Allocator) ![]const Token {
    var scanner: Scanner = .{ .source = source, .tokens_list = std.ArrayList(Token).init(allocator) };
    defer scanner.tokens_list.deinit();

    try scanner.scan();

    return scanner.tokens_list.toOwnedSlice();
}

const Scanner = struct {
    source: []const u8,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,
    tokens_list: std.ArrayList(Token),

    const Self = @This();

    fn scan(self: *Self) !void {
        try self.addEmptyToken(.left_paren);
        try self.addEmptyToken(.left_paren);
        try self.addEmptyToken(.right_paren);
        try self.tokens_list.append(.{ .tag = .eof, .lexeme = "", .literal = empty_literal });
    }

    fn addEmptyToken(self: *Self, tag: TokenTag) !void {
        return self.addToken(tag, empty_literal);
    }

    fn addToken(self: *Self, tag: TokenTag, literal: Literal) !void {
        self.current += 1;
        const lexeme = self.source[self.start..self.current];
        self.start = self.current;
        try self.tokens_list.append(.{ .tag = tag, .lexeme = lexeme, .literal = literal });
    }
};
