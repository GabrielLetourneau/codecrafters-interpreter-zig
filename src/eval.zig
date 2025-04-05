const std = @import("std");
const Allocator = std.mem.Allocator;

const Ast = @import("Ast.zig");

pub const ValueTag = enum {
    nil,
    true,
    false,
};

pub const Value = union(ValueTag) {
    nil: void,
    true: void,
    false: void,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll(@tagName(self));
    }
};

pub fn evaluate(allocator: Allocator, ast: Ast) !Value {
    var runtime: Runtime = .{
        .ast = ast,
        .allocator = allocator,
        .tags_stack = .{},
    };
    defer runtime.tags_stack.deinit(allocator);

    return try runtime.evaluate();
}

const Runtime = struct {
    ast: Ast,
    allocator: Allocator,
    tags_stack: std.ArrayListUnmanaged(ValueTag),

    const Self = @This();

    fn evaluate(self: *Self) !Value {
        for (self.ast.tags) |op| switch (op) {
            .nil => try self.pushTag(.nil),
            .true => try self.pushTag(.true),
            .false => try self.pushTag(.false),
            else => {},
        };

        return self.popValue();
    }

    fn pushTag(self: *Self, tag: ValueTag) !void {
        try self.tags_stack.append(self.allocator, tag);
    }

    fn popValue(self: *Self) Value {
        return switch (self.tags_stack.pop().?) {
            .nil => .{ .nil = {} },
            .true => .{ .true = {} },
            .false => .{ .false = {} },
        };
    }
};

fn testEvaluate(source: []const u8, expected: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;
    const parsing = @import("parsing.zig");

    const ast = try parsing.parse(allocator, source);
    defer ast.deinit(allocator);

    if (try evaluate(allocator, ast)) |value| {
        const evaluated = try std.fmt.allocPrint(allocator, "{s}", .{value});
        defer allocator.free(evaluated);

        try testing.expectEqualStrings(evaluated, expected);
    } else try testing.expectEqualStrings("", expected);
}

test "evaluate literals" {
    try testEvaluate("nil", "nil");
    try testEvaluate("true", "true");
    try testEvaluate("false", "false");
}
