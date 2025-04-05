const std = @import("std");
const Allocator = std.mem.Allocator;

const Ast = @import("Ast.zig");

pub const ValueTag = enum {
    nil,
    true,
    false,
    number,
    string,
};

pub const Value = union(ValueTag) {
    nil: void,
    true: void,
    false: void,
    number: f64,
    string: []const u8,

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .nil, .true, .false => try writer.writeAll(@tagName(self)),
            .number => |number| try writer.print("{d}", .{number}),
            .string => |string| try writer.writeAll(string),
        }
    }
};

pub fn evaluate(allocator: Allocator, ast: Ast) !Value {
    var runtime: Runtime = .{
        .ast = ast,
        .allocator = allocator,
        .tags_stack = .{},
        .data_stack = .{},
    };
    defer {
        runtime.data_stack.deinit(allocator);
        runtime.tags_stack.deinit(allocator);
    }

    return try runtime.evaluate();
}

const Runtime = struct {
    ast: Ast,
    allocator: Allocator,
    tags_stack: std.ArrayListUnmanaged(ValueTag),
    data_stack: std.ArrayListUnmanaged(Data),

    const Self = @This();

    const Data = union {
        number: f64,
        string: usize,
    };

    fn evaluate(self: *Self) !Value {
        for (self.ast.tags, 0..) |op, op_index| switch (op) {
            .nil => try self.pushEmpty(.nil),
            .true => try self.pushEmpty(.true),
            .false => try self.pushEmpty(.false),
            .number => try self.pushData(.number, .{ .number = self.ast.node(op_index).data().number }),
            .string => try self.pushData(.string, .{ .string = self.ast.node(op_index).data().index }),
            .grouping => {},
            else => {},
        };

        return self.popValue();
    }

    fn pushEmpty(self: *Self, tag: ValueTag) !void {
        try self.tags_stack.append(self.allocator, tag);
    }

    fn pushData(self: *Self, tag: ValueTag, data: Data) !void {
        try self.tags_stack.append(self.allocator, tag);
        try self.data_stack.append(self.allocator, data);
    }

    fn popValue(self: *Self) Value {
        return switch (self.tags_stack.pop().?) {
            .nil => .{ .nil = {} },
            .true => .{ .true = {} },
            .false => .{ .false = {} },
            .number => .{ .number = self.data_stack.pop().?.number },
            .string => blk: {
                const index = self.data_stack.pop().?.string;
                const string = self.ast.string(index);
                break :blk .{ .string = string };
            },
        };
    }
};

fn testEvaluate(source: []const u8, expected: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;
    const parsing = @import("parsing.zig");

    const ast = try parsing.parse(allocator, source);
    defer ast.deinit(allocator);

    const value = try evaluate(allocator, ast);
    const evaluated = try std.fmt.allocPrint(allocator, "{s}", .{value});
    defer allocator.free(evaluated);

    try testing.expectEqualStrings(evaluated, expected);
}

test "evaluate literals" {
    try testEvaluate("nil", "nil");
    try testEvaluate("true", "true");
    try testEvaluate("false", "false");
    try testEvaluate("\"hello world!\"", "hello world!");
    try testEvaluate("10.40", "10.4");
    try testEvaluate("10", "10");
}
