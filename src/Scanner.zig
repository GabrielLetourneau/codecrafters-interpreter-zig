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

    // Literals.
    identifier,
    string,
    number,
};

pub const Token = struct {
    tag: TokenTag,
    lexeme: []const u8,
    literal: union {
        empty: void,
        string: []const u8,
        number: f64,
    },

    pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        for (@tagName(self.tag)) |char| {
            try writer.writeByte(std.ascii.toUpper(char));
        }
        try writer.print(" {s} ", .{self.lexeme});
        switch (self.tag) {
            .string => try writer.writeAll(self.literal.string),
            .number => {
                const literal_number = self.literal.number;
                try writer.print("{d}", .{literal_number});
                if (literal_number == @trunc(literal_number)) try writer.writeAll(".0");
            },
            else => try writer.writeAll("null"),
        }
    }
};

pub const Error = struct {
    line: usize,
    @"error": union(enum) { char: u8, unterminated_string: void },

    pub fn format(self: Error, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("[line {d}] Error: ", .{self.line});
        switch (self.@"error") {
            .char => |char| try writer.print("Unexpected character: {c}", .{char}),
            .unterminated_string => try writer.writeAll("Unterminated string."),
        }
    }
};

pub const Result = union(enum) {
    token: Token,
    @"error": Error,
};

source: []const u8,
start: usize = 0,
current: usize = 0,
line: usize = 1,
next_result: ?Result = null,

const Self = @This();

pub fn next(self: *Self) ?Result {
    while (self.current < self.source.len and self.next_result == null) {
        const char = self.source[self.current];
        self.current += 1;
        defer self.start = self.current;
        switch (char) {
            '(' => self.addEmptyToken(.left_paren),
            ')' => self.addEmptyToken(.right_paren),
            '{' => self.addEmptyToken(.left_brace),
            '}' => self.addEmptyToken(.right_brace),
            ',' => self.addEmptyToken(.comma),
            '.' => self.addEmptyToken(.dot),
            '-' => self.addEmptyToken(.minus),
            '+' => self.addEmptyToken(.plus),
            ';' => self.addEmptyToken(.semicolon),
            '/' => if (self.match('/')) self.comment() else self.addEmptyToken(.slash),
            '*' => self.addEmptyToken(.star),
            '!' => self.addEqualOperator(.bang, .bang_equal),
            '=' => self.addEqualOperator(.equal, .equal_equal),
            '>' => self.addEqualOperator(.greater, .greater_equal),
            '<' => self.addEqualOperator(.less, .less_equal),
            '\t', ' ' => {},
            '\n' => self.line += 1,
            '"' => self.string(),
            '0'...'9' => self.number(),
            'a'...'z', 'A'...'Z', '_' => self.identifier(),
            else => self.unexpected_char(char),
        }
    }

    const result = self.next_result;
    self.next_result = null;
    return result;
}

fn addEmptyToken(self: *Self, tag: TokenTag) void {
    const lexeme = self.source[self.start..self.current];
    self.addToken(.{ .tag = tag, .lexeme = lexeme, .literal = .{ .empty = {} } });
}

fn addEqualOperator(self: *Self, short_tag: TokenTag, long_tag: TokenTag) void {
    if (self.match('=')) {
        self.addEmptyToken(long_tag);
    } else {
        self.addEmptyToken(short_tag);
    }
}

fn comment(self: *Self) void {
    if (std.mem.indexOfScalarPos(u8, self.source, self.current, '\n')) |new_line_index| {
        self.line += 1;
        self.current = new_line_index + 1;
    } else self.current = self.source.len;
}

fn string(self: *Self) void {
    if (std.mem.indexOfScalarPos(u8, self.source, self.current, '"')) |end_of_string_index| {
        const lexeme = self.source[self.current - 1 .. end_of_string_index + 1];
        const literal = self.source[self.current..end_of_string_index];
        self.addToken(.{ .tag = .string, .lexeme = lexeme, .literal = .{ .string = literal } });
        self.line += std.mem.count(u8, literal, "\n");
        self.current = end_of_string_index + 1;
    } else {
        self.addError(.{ .line = self.line, .@"error" = .unterminated_string });
        self.current = self.source.len;
    }
}

fn number(self: *Self) void {
    var end_of_number =
        std.mem.indexOfNonePos(u8, self.source, self.current, "0123456789") orelse self.source.len;

    self.current = end_of_number;

    if (self.match('.')) {
        if (std.mem.indexOfNonePos(u8, self.source, self.current, "0123456789")) |end_of_fraction| {
            if (end_of_fraction > self.current) {
                end_of_number = end_of_fraction;
                self.current = end_of_fraction;
            }
        } else {
            end_of_number = self.source.len;
            self.current = self.source.len;
        }
    }

    const lexeme = self.source[self.start..end_of_number];
    const value = std.fmt.parseFloat(f64, lexeme) catch unreachable;

    self.addToken(.{ .tag = .number, .lexeme = lexeme, .literal = .{ .number = value } });
}

fn identifier(self: *Self) void {
    while (self.current < self.source.len) {
        const char = self.source[self.current];
        if (char >= 'a' and char <= 'z' or
            char >= 'A' and char <= 'Z' or
            char >= '0' and char <= '9' or
            char == '_')
        {
            self.current += 1;
        } else break;
    }

    const lexeme = self.source[self.start..self.current];
    self.addToken(.{ .tag = .identifier, .lexeme = lexeme, .literal = .{ .empty = {} } });
}

fn unexpected_char(self: *Self, char: u8) void {
    self.addError(.{ .line = self.line, .@"error" = .{ .char = char } });
}

fn addToken(self: *Self, token: Token) void {
    self.next_result = .{ .token = token };
}

fn addError(self: *Self, @"error": Error) void {
    self.next_result = .{ .@"error" = @"error" };
}

fn match(self: *Self, char: u8) bool {
    if (self.current < self.source.len and self.source[self.current] == char) {
        self.current += 1;
        return true;
    } else return false;
}

fn testScan(source: []const u8, output: []const u8, errors: []const u8) !void {
    var output_iterator = std.mem.splitScalar(u8, output, '\n');
    var errors_iterator = std.mem.splitScalar(u8, errors, '\n');
    var scanner: Self = .{ .source = source };
    const testing = std.testing;
    const allocator = std.testing.allocator;

    while (scanner.next()) |result| {
        switch (result) {
            .token => |token| {
                if (output_iterator.next()) |expected| {
                    const actual = try std.fmt.allocPrint(allocator, "{s}", .{token});
                    defer allocator.free(actual);
                    try testing.expectEqualSlices(u8, expected, actual);
                } else try testing.expect(false);
            },
            .@"error" => |@"error"| {
                if (errors_iterator.next()) |expected| {
                    const actual = try std.fmt.allocPrint(allocator, "{s}", .{@"error"});
                    defer allocator.free(actual);
                    try testing.expectEqualSlices(u8, expected, actual);
                } else try testing.expect(false);
            },
        }
    }

    try std.testing.expectEqual(0, output_iterator.rest().len);
    try std.testing.expectEqual(0, errors_iterator.rest().len);
}

test "scan single character tokens" {
    try testScan("", "", "");
    try testScan("(()",
        \\LEFT_PAREN ( null
        \\LEFT_PAREN ( null
        \\RIGHT_PAREN ) null
    , "");
    try testScan("{{}}",
        \\LEFT_BRACE { null
        \\LEFT_BRACE { null
        \\RIGHT_BRACE } null
        \\RIGHT_BRACE } null
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
    , "");
    try testScan(",.$(#",
        \\COMMA , null
        \\DOT . null
        \\LEFT_PAREN ( null
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
    , "");
    try testScan("!!===",
        \\BANG ! null
        \\BANG_EQUAL != null
        \\EQUAL_EQUAL == null
    , "");
}

test "scan slash vs comment" {
    try testScan("()// Comment",
        \\LEFT_PAREN ( null
        \\RIGHT_PAREN ) null
    , "");
    try testScan("/()",
        \\SLASH / null
        \\LEFT_PAREN ( null
        \\RIGHT_PAREN ) null
    , "");
}

test "scan whitespace" {
    try testScan("(\t )",
        \\LEFT_PAREN ( null
        \\RIGHT_PAREN ) null
    , "");
    try testScan("{\n }\n((+",
        \\LEFT_BRACE { null
        \\RIGHT_BRACE } null
        \\LEFT_PAREN ( null
        \\LEFT_PAREN ( null
        \\PLUS + null
    , "");
}

test "scan multi-line errors" {
    try testScan("# (\n)\t@",
        \\LEFT_PAREN ( null
        \\RIGHT_PAREN ) null
    ,
        \\[line 1] Error: Unexpected character: #
        \\[line 2] Error: Unexpected character: @
    );
}

test "scan string literals" {
    try testScan("\"foo baz\"",
        \\STRING "foo baz" foo baz
    , "");
    try testScan("\"bar", "",
        \\[line 1] Error: Unterminated string.
    );
}

test "scan number" {
    try testScan("42",
        \\NUMBER 42 42.0
    , "");
    try testScan("1234.1234",
        \\NUMBER 1234.1234 1234.1234
    , "");
}

test "scan identifier" {
    try testScan("foo bar _hello _1234",
        \\IDENTIFIER foo null
        \\IDENTIFIER bar null
        \\IDENTIFIER _hello null
        \\IDENTIFIER _1234 null
    , "");
}
