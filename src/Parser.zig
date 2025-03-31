const std = @import("std");
const Ast = @import("Ast.zig");
const Scanner = @import("Scanner.zig");

scanner: *Scanner,
allocator: std.mem.Allocator,
tags_list: std.ArrayListUnmanaged(Ast.NodeTag) = .{},
data_list: std.ArrayListUnmanaged(Ast.Data) = .{},
next_token: ?Scanner.Token = null,

const Self = @This();

pub fn deinit(self: *Self) void {
    self.data_list.deinit(self.allocator);
    self.tags_list.deinit(self.allocator);
}

pub fn parse(self: *Self) !?Ast.Node {
    try self.expression();
    if (self.tags_list.items.len > 0) {
        const indexes = self.lastIndexes().lhs_reference;
        return .{
            .ast = .{
                .tags = self.tags_list.items,
                .data = self.data_list.items,
            },
            .tag_index = indexes.tag_index,
            .data_index = indexes.data_index,
        };
    } else return null;
}

fn expression(self: *Self) std.mem.Allocator.Error!void {
    try self.term();
}

fn term(self: *Self) !void {
    try self.factor();

    var lhs_reference = self.lastIndexes();

    while (true) {
        const tag: Ast.NodeTag = blk: {
            if (self.match(.plus) != null) break :blk .add;
            if (self.match(.minus) != null) break :blk .substract;
            return;
        };

        try self.factor();

        try self.addData(tag, lhs_reference);

        lhs_reference = self.lastIndexes();
    }
}

fn factor(self: *Self) !void {
    try self.unary();

    var lhs_reference = self.lastIndexes();

    while (true) {
        const tag: Ast.NodeTag = blk: {
            if (self.match(.star) != null) break :blk .multiply;
            if (self.match(.slash) != null) break :blk .divide;
            return;
        };

        try self.unary();

        try self.addData(tag, lhs_reference);

        lhs_reference = self.lastIndexes();
    }
}

fn unary(self: *Self) !void {
    const tag: Ast.NodeTag = blk: {
        if (self.match(.bang) != null) break :blk .not;
        if (self.match(.minus) != null) break :blk .unary_minus;

        try self.primary();
        return;
    };

    try self.unary();

    try self.addEmpty(tag);
}

fn primary(self: *Self) !void {
    if (self.match(.nil) != null) {
        try self.addEmpty(.nil);
    } else if (self.match(.true) != null) {
        try self.addEmpty(.true);
    } else if (self.match(.false) != null) {
        try self.addEmpty(.false);
    } else if (self.match(.number)) |token| {
        try self.addData(.number, .{ .number = token.literal.number });
    } else if (self.match(.string)) |token| {
        try self.addData(.string, .{ .string = token.literal.string });
    } else if (self.match(.left_paren) != null) {
        try self.expression();
        _ = self.match(.right_paren);
        try self.addEmpty(.group);
    }
}

fn match(self: *Self, token_tag: Scanner.TokenTag) ?Scanner.Token {
    const token = blk: while (true) {
        if (self.next_token) |token|
            break :blk token;
        if (self.scanner.next()) |result| {
            switch (result) {
                .token => |scanned_token| self.next_token = scanned_token,
                .@"error" => {},
            }
        } else return null;
    };

    if (token.tag == token_tag) {
        self.next_token = null;
        return token;
    }
    return null;
}

fn addEmpty(self: *Self, tag: Ast.NodeTag) !void {
    try self.tags_list.append(self.allocator, tag);
}

fn addData(self: *Self, tag: Ast.NodeTag, data: Ast.Data) !void {
    try self.tags_list.append(self.allocator, tag);
    errdefer _ = self.tags_list.pop();
    try self.data_list.append(self.allocator, data);
}

fn lastIndexes(self: Self) Ast.Data {
    return .{
        .lhs_reference = .{
            .tag_index = @intCast(self.tags_list.items.len - 1),
            .data_index = @intCast(@max(self.data_list.items.len, 1) - 1),
        },
    };
}

fn testParse(source: []const u8, parsed: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;

    var scanner: Scanner = .{ .source = source };
    var parser: Self = .{ .scanner = &scanner, .allocator = allocator };
    defer parser.deinit();

    if (try parser.parse()) |node| {
        const actual = try std.fmt.allocPrint(allocator, "{s}", .{node});
        defer allocator.free(actual);

        try testing.expectEqualStrings(parsed, actual);
    } else try testing.expect(false);
}

test "parse primary expressions" {
    try testParse("nil", "nil");
    try testParse("true", "true");
    try testParse("false", "false");
    try testParse("42.47", "42.47");
    try testParse("\"hello\"", "hello");
    try testParse("(\"foo\")", "(group foo)");
}

test "parse unary expressions" {
    try testParse("!true", "(! true)");
}

test "parse binary expressions" {
    try testParse("16 * 38 / 58", "(/ (* 16.0 38.0) 58.0)");
    try testParse("52 + 80 - 94", "(- (+ 52.0 80.0) 94.0)");
    try testParse("(-92 + 90) * (60 * 99) / (39 + 51)", "(/ (* (group (+ (- 92.0) 90.0)) (group (* 60.0 99.0))) (group (+ 39.0 51.0)))");
}
