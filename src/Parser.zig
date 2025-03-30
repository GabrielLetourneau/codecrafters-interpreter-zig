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

pub fn parse(self: *Self) !void {
    try self.primary();
}

pub fn ast(self: Self) Ast {
    return .{ .tags = self.tags_list.items, .data = self.data_list.items };
}

fn primary(self: *Self) !void {
    if (self.match(.nil) != null) {
        try self.addTag(.nil);
    } else if (self.match(.true) != null) {
        try self.addTag(.true);
    } else if (self.match(.false) != null) {
        try self.addTag(.false);
    } else if (self.match(.number)) |token| {
        try self.addTagAndData(.number, .{ .number = token.literal.number });
    } else if (self.match(.string)) |token| {
        try self.addTagAndData(.string, .{ .string = token.literal.string });
    } else if (self.match(.left_paren) != null) {
        try self.addTag(.group);
        try self.primary();
        _ = self.match(.right_paren);
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

fn addTag(self: *Self, node_tag: Ast.NodeTag) !void {
    try self.tags_list.append(self.allocator, node_tag);
}

fn addTagAndData(self: *Self, node_tag: Ast.NodeTag, data: Ast.Data) !void {
    try self.addTag(node_tag);
    try self.data_list.append(self.allocator, data);
}

fn testParse(source: []const u8, parsed: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;

    var scanner: Scanner = .{ .source = source };
    var parser: Self = .{ .scanner = &scanner, .allocator = allocator };
    defer parser.deinit();

    try parser.parse();

    if (parser.ast().head()) |node| {
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
