const std = @import("std");
const Allocator = std.mem.Allocator;

const Ast = @import("Ast.zig");

pub const ValueTag = enum {
    nil,
    true,
    false,
    number,
    string,
    object,
};

pub const Value = union(ValueTag) {
    nil: void,
    true: void,
    false: void,
    number: f64,
    string: usize,
    object: *HeapObject,
};

pub const HeapObject = struct {
    tag: HeapObjectTag,
    ref_count: u32 = 1,
    left: HeapData,
    right: HeapData,
};

pub const HeapObjectTag = enum {
    string_string,
    string_compound,
    compound_string,
    compound_compound,
};

pub const HeapData = union {
    empty: void,
    index: usize,
    object: *HeapObject,
};

ast: Ast,
allocator: Allocator,
tags_stack: std.ArrayListUnmanaged(ValueTag),
data_stack: std.ArrayListUnmanaged(StackData),
heap: std.heap.MemoryPool(HeapObject),

const Self = @This();

const StackData = union {
    number: f64,
    string: usize,
    object: *HeapObject,
};

pub fn init(allocator: Allocator, ast: Ast) Self {
    return .{
        .ast = ast,
        .allocator = allocator,
        .tags_stack = .{},
        .data_stack = .{},
        .heap = std.heap.MemoryPool(HeapObject).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.heap.deinit();
    self.data_stack.deinit(self.allocator);
    self.tags_stack.deinit(self.allocator);
}

pub fn evaluate(self: *Self) !RuntimeValue {
    for (self.ast.tags, 0..) |op, op_index| switch (op) {
        .nil => try self.pushEmpty(.nil),
        .true => try self.pushEmpty(.true),
        .false => try self.pushEmpty(.false),

        .group => {},
        .not => {
            const result: ValueTag = switch (self.popValue()) {
                .nil, .false => .true,
                else => .false,
            };
            try self.pushEmpty(result);
        },
        .unary_minus => {
            const argument = self.popValue().number;
            try self.pushData(.number, .{ .number = -argument });
        },

        .number => try self.pushData(.number, .{ .number = self.ast.node(op_index).data().number }),
        .string => try self.pushData(.string, .{ .string = self.ast.node(op_index).data().index }),

        .multiply => try self.binary(multiply),
        .divide => try self.binary(divide),
        .add => try self.binary(add),
        .substract => try self.binary(substract),
        .greater => try self.binary(greater),
        .greater_equal => try self.binary(greater_equal),
        .less => try self.binary(less),
        .less_equal => try self.binary(less_equal),

        else => {},
    };

    const value = self.popValue();
    return .{ .runtime = self, .value = value };
}

fn binary(self: *Self, operator: fn (*Self, Value, Value) error{OutOfMemory}!void) !void {
    const right = self.popValue();
    const left = self.popValue();
    try operator(self, left, right);
}

fn multiply(self: *Self, left: Value, right: Value) !void {
    try self.pushData(.number, .{ .number = left.number * right.number });
}

fn divide(self: *Self, left: Value, right: Value) !void {
    try self.pushData(.number, .{ .number = left.number / right.number });
}

fn add(self: *Self, left: Value, right: Value) !void {
    switch (left) {
        .number => |number| try self.pushData(.number, .{ .number = number + right.number }),
        .string => |left_index| {
            const object_ptr = try self.heap.create();
            object_ptr.*.left = .{ .index = left_index };
            switch (right) {
                .string => |right_index| {
                    object_ptr.*.tag = .string_string;
                    object_ptr.*.right = .{ .index = right_index };
                },
                .object => |right_compound| {
                    object_ptr.*.tag = .string_compound;
                    object_ptr.*.right = .{ .object = right_compound };
                },
                else => {},
            }
            try self.pushData(.object, .{ .object = object_ptr });
        },
        .object => |left_compound| {
            const object_ptr = try self.heap.create();
            object_ptr.*.left = .{ .object = left_compound };
            switch (right) {
                .string => |right_index| {
                    object_ptr.*.tag = .compound_string;
                    object_ptr.*.right = .{ .index = right_index };
                },
                .object => |right_compound| {
                    object_ptr.*.tag = .compound_compound;
                    object_ptr.*.right = .{ .object = right_compound };
                },
                else => {},
            }
            try self.pushData(.object, .{ .object = object_ptr });
        },
        else => {},
    }
}

fn greater(self: *Self, left: Value, right: Value) !void {
    const result = left.number > right.number;
    try self.pushEmpty(if (result) .true else .false);
}

fn greater_equal(self: *Self, left: Value, right: Value) !void {
    const result = left.number >= right.number;
    try self.pushEmpty(if (result) .true else .false);
}

fn less(self: *Self, left: Value, right: Value) !void {
    const result = left.number < right.number;
    try self.pushEmpty(if (result) .true else .false);
}

fn less_equal(self: *Self, left: Value, right: Value) !void {
    const result = left.number <= right.number;
    try self.pushEmpty(if (result) .true else .false);
}

fn substract(self: *Self, left: Value, right: Value) !void {
    try self.pushData(.number, .{ .number = left.number - right.number });
}

fn pushEmpty(self: *Self, tag: ValueTag) !void {
    try self.tags_stack.append(self.allocator, tag);
}

fn pushData(self: *Self, tag: ValueTag, data: StackData) !void {
    try self.tags_stack.append(self.allocator, tag);
    try self.data_stack.append(self.allocator, data);
}

fn popValue(self: *Self) Value {
    return switch (self.tags_stack.pop().?) {
        .nil => .{ .nil = {} },
        .true => .{ .true = {} },
        .false => .{ .false = {} },
        .number => .{ .number = self.data_stack.pop().?.number },
        .string => .{ .string = self.data_stack.pop().?.string },
        .object => .{ .object = self.data_stack.pop().?.object },
    };
}

pub const RuntimeValue = struct {
    runtime: *const Self,
    value: Value,

    pub fn format(self: RuntimeValue, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.value) {
            .nil, .true, .false => try writer.writeAll(@tagName(self.value)),
            .number => |number| try writer.print("{d}", .{number}),
            .string => |index| {
                const string = self.runtime.ast.string(index);
                try writer.writeAll(string);
            },
            .object => |object| {
                const leftValue: Value = switch (object.tag) {
                    .string_string, .string_compound => .{ .string = object.left.index },
                    .compound_string, .compound_compound => .{ .object = object.left.object },
                };
                const rightValue: Value = switch (object.tag) {
                    .string_string, .compound_string => .{ .string = object.right.index },
                    .string_compound, .compound_compound => .{ .object = object.right.object },
                };
                const left: RuntimeValue = .{ .runtime = self.runtime, .value = leftValue };
                const right: RuntimeValue = .{ .runtime = self.runtime, .value = rightValue };
                try writer.print("{s}{s}", .{ left, right });
            },
        }
    }
};

fn testEvaluate(source: []const u8, expected: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;
    const parsing = @import("parsing.zig");

    const ast = try parsing.parse(allocator, source);
    defer ast.deinit(allocator);

    var runtime = Self.init(allocator, ast);
    defer runtime.deinit();

    const value = try runtime.evaluate();
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

test "evaluate unary expressions" {
    try testEvaluate("(\"hello world!\")", "hello world!");
    try testEvaluate("-73", "-73");
    try testEvaluate("!true", "false");
    try testEvaluate("!10.40", "false");
    try testEvaluate("!((false))", "true");
}

test "evaluate binary expressions" {
    try testEvaluate("42 / 5", "8.4");
    try testEvaluate("18 * 3 / (3 * 6)", "3");
    try testEvaluate("(10.40 * 2) / 2", "10.4");
    try testEvaluate("70 - 65", "5");
    try testEvaluate("69 - 93", "-24");
    try testEvaluate("10.40 - 2", "8.4");
    try testEvaluate("23 + 28 - (-(61 - 99))", "13");
    try testEvaluate("\"hello\" + \" world!\"", "hello world!");
    try testEvaluate("57 > -65", "true");
    try testEvaluate("(54 - 67) >= -(114 / 57 + 11)", "true");
}
