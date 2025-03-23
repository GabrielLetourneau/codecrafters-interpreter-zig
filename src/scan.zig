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

    // One or two character tokens.
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    greater_equal,
    less,
    less_equal,

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

pub const Error = struct {
    line: usize,
    char: u8,

    pub fn format(self: Error, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("[line {d}] Error: Unexpected character: {c}", .{ self.line, self.char });
    }
};

pub const ScanResult = struct {
    tokens: []const Token,
    errors: []const Error,
};

pub fn scan(source: []const u8, allocator: std.mem.Allocator) !ScanResult {
    var scanner: Scanner = Scanner.init(source, allocator);
    defer scanner.deinit();

    try scanner.scan();

    return .{
        .tokens = try scanner.tokens_list.toOwnedSlice(allocator),
        .errors = try scanner.errors_list.toOwnedSlice(allocator),
    };
}

const Scanner = struct {
    source: []const u8,
    start: usize = 0,
    current: usize = 0,
    line: usize = 1,
    tokens_list: std.ArrayListUnmanaged(Token),
    errors_list: std.ArrayListUnmanaged(Error),
    allocator: std.mem.Allocator,

    const Self = @This();

    fn init(source: []const u8, allocator: std.mem.Allocator) Scanner {
        return .{
            .source = source,
            .tokens_list = .{},
            .errors_list = .{},
            .allocator = allocator,
        };
    }

    fn deinit(self: *Self) void {
        self.tokens_list.deinit(self.allocator);
        self.errors_list.deinit(self.allocator);
    }

    fn scan(self: *Self) !void {
        while (self.current < self.source.len) {
            const char = self.source[self.current];
            self.current += 1;
            switch (char) {
                '(' => try self.addEmptyToken(.left_paren),
                ')' => try self.addEmptyToken(.right_paren),
                '{' => try self.addEmptyToken(.left_brace),
                '}' => try self.addEmptyToken(.right_brace),
                ',' => try self.addEmptyToken(.comma),
                '.' => try self.addEmptyToken(.dot),
                '-' => try self.addEmptyToken(.minus),
                '+' => try self.addEmptyToken(.plus),
                ';' => try self.addEmptyToken(.semicolon),
                '/' => if (self.match('/')) {
                    if (std.mem.indexOfScalar(u8, self.source[self.current..], '\n')) |new_line_index| {
                        self.current += new_line_index + 1;
                    } else self.current = self.source.len;

                    self.start = self.current;
                } else try self.addEmptyToken(.slash),
                '*' => try self.addEmptyToken(.star),
                '!' => try self.addEqualOperator(.bang, .bang_equal),
                '=' => try self.addEqualOperator(.equal, .equal_equal),
                '>' => try self.addEqualOperator(.greater, .greater_equal),
                '<' => try self.addEqualOperator(.less, .less_equal),
                '\t', ' ', '\n' => {
                    self.start = self.current;
                },
                else => {
                    try self.errors_list.append(self.allocator, .{ .line = 1, .char = char });
                    self.start = self.current;
                },
            }
        }

        try self.tokens_list.append(self.allocator, .{ .tag = .eof, .lexeme = "", .literal = empty_literal });
    }

    fn addEmptyToken(self: *Self, tag: TokenTag) !void {
        return self.addToken(tag, empty_literal);
    }

    fn addToken(self: *Self, tag: TokenTag, literal: Literal) !void {
        const lexeme = self.source[self.start..self.current];
        self.start = self.current;
        try self.tokens_list.append(self.allocator, .{ .tag = tag, .lexeme = lexeme, .literal = literal });
    }

    fn addEqualOperator(self: *Self, short_tag: TokenTag, long_tag: TokenTag) !void {
        if (self.match('=')) {
            try self.addEmptyToken(long_tag);
        } else {
            try self.addEmptyToken(short_tag);
        }
    }

    fn match(self: *Self, char: u8) bool {
        if (self.current < self.source.len and self.source[self.current] == char) {
            self.current += 1;
            return true;
        } else return false;
    }
};

fn testScan(source: []const u8, output: []const u8, errors: []const u8) !void {
    const allocator = std.testing.allocator;
    const result = try scan(source, allocator);
    defer {
        allocator.free(result.tokens);
        allocator.free(result.errors);
    }

    try matchResultSet(Token, result.tokens, output, allocator);
    try matchResultSet(Error, result.errors, errors, allocator);
}

fn matchResultSet(ResultType: type, results: []const ResultType, output: []const u8, allocator: std.mem.Allocator) !void {
    if (output.len == 0) {
        try std.testing.expectEqual(0, results.len);
        return;
    }

    var expected_lines = std.mem.splitScalar(u8, output, '\n');
    var index: usize = 0;
    while (expected_lines.next()) |expected_line| {
        // We shouldn’t have more results than expected
        try std.testing.expect(index < results.len);
        const token_string = try std.fmt.allocPrint(allocator, "{s}", .{results[index]});
        defer allocator.free(token_string);
        std.debug.print("expected: {s}, actual: {s}\n", .{ expected_line, token_string });
        try std.testing.expectEqualStrings(expected_line, token_string);
        index += 1;
    }

    // Fails if we haven’t had all expected results.
    try std.testing.expectEqual(index, results.len);
}

test "scan single character tokens" {
    try testScan("", "EOF  null", "");
    try testScan("(()",
        \\LEFT_PAREN ( null
        \\LEFT_PAREN ( null
        \\RIGHT_PAREN ) null
        \\EOF  null
    , "");
    try testScan("{{}}",
        \\LEFT_BRACE { null
        \\LEFT_BRACE { null
        \\RIGHT_BRACE } null
        \\RIGHT_BRACE } null
        \\EOF  null
    , "");
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
    , "");
    try testScan(",.$(#",
        \\COMMA , null
        \\DOT . null
        \\LEFT_PAREN ( null
        \\EOF  null
    ,
        \\[line 1] Error: Unexpected character: $
        \\[line 1] Error: Unexpected character: #
    );
}

test "scan one or two character tokens" {
    try testScan("={===}",
        \\EQUAL = null
        \\LEFT_BRACE { null
        \\EQUAL_EQUAL == null
        \\EQUAL = null
        \\RIGHT_BRACE } null
        \\EOF  null
    , "");
    try testScan("!!===",
        \\BANG ! null
        \\BANG_EQUAL != null
        \\EQUAL_EQUAL == null
        \\EOF  null
    , "");
}

test "scan slash vs comment" {
    try testScan("()// Comment",
        \\LEFT_PAREN ( null
        \\RIGHT_PAREN ) null
        \\EOF  null
    , "");
    try testScan("/()",
        \\SLASH / null
        \\LEFT_PAREN ( null
        \\RIGHT_PAREN ) null
        \\EOF  null
    , "");
}

test "scan whitespace" {
    try testScan("(\t )",
        \\LEFT_PAREN ( null
        \\RIGHT_PAREN ) null
        \\EOF  null
    , "");
    try testScan("{\n }\n((+",
        \\LEFT_BRACE { null
        \\RIGHT_BRACE } null
        \\LEFT_PAREN ( null
        \\LEFT_PAREN ( null
        \\PLUS + null
        \\EOF  null
    , "");
}
