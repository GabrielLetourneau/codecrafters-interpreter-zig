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
    ref_count: u32,
    left: HeapData,
    right: HeapData,

    fn string(self: *HeapObject) []const u8 {
        return self.left.string_ptr[0..self.right.string_len];
    }
};

pub const HeapObjectTag = enum {
    string,
};

pub const HeapData = union {
    string_ptr: [*]const u8,
    string_len: usize,
};

ast: Ast,
allocator: Allocator,
tags_stack: std.ArrayListUnmanaged(ValueTag),
data_stack: std.ArrayListUnmanaged(StackData),
heap: std.heap.MemoryPool(HeapObject),
left: ?Value = null,
right: ?Value = null,

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
    self.freeArguments();
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
            self.popValue();
            const result: ValueTag = switch (self.right.?) {
                .nil, .false => .true,
                else => .false,
            };
            try self.pushEmpty(result);
        },
        .unary_minus => {
            self.popValue();
            const result = -self.right.?.number;
            try self.pushData(.number, .{ .number = result });
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
        .equal => try self.binary(equal),
        .not_equal => try self.binary(not_equal),
    };

    self.popValue();
    return .{ .runtime = self, .value = self.right.? };
}

fn pushEmpty(self: *Self, tag: ValueTag) !void {
    defer self.freeArguments();
    try self.tags_stack.append(self.allocator, tag);
}

fn pushData(self: *Self, tag: ValueTag, data: StackData) !void {
    defer self.freeArguments();
    try self.tags_stack.append(self.allocator, tag);
    try self.data_stack.append(self.allocator, data);
    if (tag == .object) {
        data.object.*.ref_count += 1;
    }
}

fn freeArguments(self: *Self) void {
    if (self.right) |value| {
        switch (value) {
            .object => |object| {
                self.decrementRef(object);
            },
            else => {},
        }
        self.right = null;
    }
    if (self.left) |value| {
        switch (value) {
            .object => |object| {
                self.decrementRef(object);
            },
            else => {},
        }
        self.left = null;
    }
}

fn decrementRef(self: *Self, object: *HeapObject) void {
    object.*.ref_count -= 1;
    if (object.ref_count == 0) {
        switch (object.tag) {
            .string => self.allocator.free(object.string()),
        }

        self.heap.destroy(object);
    }
}

fn popValue(self: *Self) void {
    const value: Value = switch (self.tags_stack.pop().?) {
        .nil => .{ .nil = {} },
        .true => .{ .true = {} },
        .false => .{ .false = {} },
        .number => .{ .number = self.data_stack.pop().?.number },
        .string => .{ .string = self.data_stack.pop().?.string },
        .object => .{ .object = self.data_stack.pop().?.object },
    };

    if (self.right == null) {
        self.right = value;
    } else self.left = value;
}

fn binary(self: *Self, operator: fn (*Self) error{OutOfMemory}!void) !void {
    self.popValue();
    self.popValue();
    try operator(self);
}

fn multiply(self: *Self) !void {
    try self.pushData(.number, .{ .number = self.left.?.number * self.right.?.number });
}

fn divide(self: *Self) !void {
    try self.pushData(.number, .{ .number = self.left.?.number / self.right.?.number });
}

fn add(self: *Self) !void {
    switch (self.left.?) {
        .number => |number| try self.pushData(.number, .{ .number = number + self.right.?.number }),
        .string, .object => {
            const left_string = self.maybe_string(self.left.?) orelse return;
            const right_string = self.maybe_string(self.right.?) orelse return;

            const string = try self.allocator.alloc(u8, left_string.len + right_string.len);
            errdefer self.allocator.free(string);

            @memcpy(string[0..left_string.len], left_string);
            @memcpy(string[left_string.len..], right_string);

            const object = try self.heap.create();
            errdefer self.heap.destroy(object);

            object.tag = .string;
            object.ref_count = 0;
            object.left = .{ .string_ptr = string.ptr };
            object.right = .{ .string_len = string.len };
            try self.pushData(.object, .{ .object = object });
        },
        else => {},
    }
}

fn substract(self: *Self) !void {
    try self.pushData(.number, .{ .number = self.left.?.number - self.right.?.number });
}

fn greater(self: *Self) !void {
    const result = self.left.?.number > self.right.?.number;
    try self.pushEmpty(if (result) .true else .false);
}

fn greater_equal(self: *Self) !void {
    const result = self.left.?.number >= self.right.?.number;
    try self.pushEmpty(if (result) .true else .false);
}

fn less(self: *Self) !void {
    const result = self.left.?.number < self.right.?.number;
    try self.pushEmpty(if (result) .true else .false);
}

fn less_equal(self: *Self) !void {
    const result = self.left.?.number <= self.right.?.number;
    try self.pushEmpty(if (result) .true else .false);
}

fn equal(self: *Self) !void {
    const result = self.isEqual();
    try self.pushEmpty(if (result) .true else .false);
}

fn not_equal(self: *Self) !void {
    const result = !self.isEqual();
    try self.pushEmpty(if (result) .true else .false);
}

fn isEqual(self: Self) bool {
    return switch (self.left.?) {
        .nil => self.right.? == .nil,
        .true => self.right.? == .true,
        .false => self.right.? == .false,
        .number => |left_number| switch (self.right.?) {
            .number => |right_number| left_number == right_number,
            else => false,
        },
        .string, .object => {
            const left_string = self.maybe_string(self.left.?) orelse return false;
            const right_string = self.maybe_string(self.right.?) orelse return false;
            return std.mem.eql(u8, left_string, right_string);
        },
    };
}

fn maybe_string(self: Self, value: Value) ?[]const u8 {
    return switch (value) {
        .string => |index| self.ast.string(index),
        .object => |object| switch (object.tag) {
            .string => object.string(),
        },
        else => null,
    };
}

pub const RuntimeValue = struct {
    runtime: *const Self,
    value: Value,

    pub fn format(self: RuntimeValue, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self.value) {
            .nil, .true, .false => try writer.writeAll(@tagName(self.value)),
            .number => |number| try writer.print("{d}", .{number}),
            .string, .object => if (self.runtime.maybe_string(self.value)) |string|
                try writer.writeAll(string),
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

    try testing.expectEqualStrings(expected, evaluated);
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
    try testEvaluate("\"a\" + \"b\" + \"c\"", "abc");
    try testEvaluate("\"a\" + \"b\" + (\"c\" + \"d\")", "abcd");
    try testEvaluate("57 > -65", "true");
    try testEvaluate("(54 - 67) >= -(114 / 57 + 11)", "true");
    try testEvaluate("\"hello\" == \"world\"", "false");
    try testEvaluate("nil != false", "true");
}
