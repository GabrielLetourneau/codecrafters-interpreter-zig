const std = @import("std");

pub const TokenTag = enum {
    // Single-character tokens.
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,

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
        while (self.current < self.source.len) {
            switch (self.source[self.current]) {
                '(' => try self.addEmptyToken(.left_paren),
                ')' => try self.addEmptyToken(.right_paren),
                '{' => try self.addEmptyToken(.left_brace),
                '}' => try self.addEmptyToken(.right_brace),
                ',' => try self.addEmptyToken(.comma),
                '.' => try self.addEmptyToken(.dot),
                '-' => try self.addEmptyToken(.minus),
                '+' => try self.addEmptyToken(.plus),
                '/' => try self.addEmptyToken(.slash),
                '*' => try self.addEmptyToken(.star),
                else => return error.UnexpectedCharacter,
            }
        }

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

fn testScan(source: []const u8, output: []const u8) !void {
    const allocator = std.testing.allocator;
    const tokens = try scan(source, allocator);
    defer allocator.free(tokens);

    var expected_lines = std.mem.splitScalar(u8, output, '\n');
    var index: usize = 0;
    while (expected_lines.next()) |expected_line| {
        // We shouldn’t have more tokens than expected
        try std.testing.expect(index < tokens.len);
        const token_string = try std.fmt.allocPrint(allocator, "{s}", .{tokens[index]});
        defer allocator.free(token_string);
        std.debug.print("expected: {s}, actual: {s}\n", .{ expected_line, token_string });
        try std.testing.expectEqualStrings(expected_line, token_string);
        index += 1;
    }

    // Fails if we haven’t had all expected tokens.
    try std.testing.expectEqual(index, tokens.len);
}

test "scan single character tokens" {
    try testScan("", "EOF  null");
    try testScan("(()",
        \\LEFT_PAREN ( null
        \\LEFT_PAREN ( null
        \\RIGHT_PAREN ) null
        \\EOF  null
    );
    try testScan("{{}}",
        \\LEFT_BRACE { null
        \\LEFT_BRACE { null
        \\RIGHT_BRACE } null
        \\RIGHT_BRACE } null
        \\EOF  null
    );
    try testScan("({*.,+*})",
        \\LEFT_PAREN ( null
        \\LEFT_BRACE { null
        \\STAR * null
        \\DOT . null
        \\COMMA , null
        \\PLUS + null
        \\STAR * null
        \\RIGHT_BRACE } null
        \\RIGHT_PAREN ) null
        \\EOF  null
    );
}
