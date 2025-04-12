const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.io.AnyWriter;

const Ast = @import("Ast.zig");

pub const Value = union(enum) {
    nil: void,
    true: void,
    false: void,
    number: f64,
    internal_string: []const u8,
    heap_string: *HeapObject,

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .nil, .true, .false => try writer.writeAll(@tagName(self)),
            .number => |number| try writer.print("{d}", .{number}),
            .internal_string, .heap_string => try writer.writeAll(self.string()),
        }
    }

    fn string(self: Value) []const u8 {
        return switch (self) {
            .internal_string => |internal_string| internal_string,
            .heap_string => |object| object.string(),
            else => unreachable,
        };
    }
};

const ValueTag = std.meta.Tag(Value);

pub const HeapObjectTag = enum {
    nil,
    true,
    false,
    number,
    internal_string,
    heap_string,
    string_buffer,
    string_prefix,
};

pub const HeapObject = struct {
    tag: HeapObjectTag,
    ref_count: u32,
    data: HeapData,

    fn string(self: HeapObject) []const u8 {
        return switch (self.tag) {
            .internal_string => self.data.internal_string,
            .heap_string => self.data.heap_string.string(),
            .string_buffer => self.data.string_buffer,
            .string_prefix => self.data.string_prefix.object.data.string_buffer[0..self.data.string_prefix.len],
            else => unreachable,
        };
    }

    fn value(self: *HeapObject) Value {
        return switch (self.tag) {
            .nil => .{ .nil = {} },
            .true => .{ .true = {} },
            .false => .{ .false = {} },
            .number => .{ .number = self.data.number },
            .internal_string => .{ .internal_string = self.data.internal_string },
            .heap_string => .{ .heap_string = self.data.heap_string },
            .string_buffer, .string_prefix => unreachable,
        };
    }

    fn setValue(self: *HeapObject, new_value: Value) void {
        switch (new_value) {
            .nil => {
                self.tag = .nil;
                self.data = .{ .empty = {} };
            },
            .true => {
                self.tag = .true;
                self.data = .{ .empty = {} };
            },
            .false => {
                self.tag = .false;
                self.data = .{ .empty = {} };
            },
            .number => |number| {
                self.tag = .number;
                self.data = .{ .number = number };
            },
            .internal_string => |internal_string| {
                self.tag = .internal_string;
                self.data = .{ .internal_string = internal_string };
            },
            .heap_string => |object| {
                self.tag = .heap_string;
                self.data = .{ .heap_string = object };
                object.ref_count += 1;
            },
        }
    }
};

pub const HeapData = union(enum) {
    empty: void,
    number: f64,
    internal_string: []const u8,
    heap_string: *HeapObject,
    string_buffer: []u8,
    string_prefix: struct { object: *HeapObject, len: usize },
};

allocator: Allocator,
ast: Ast,
out: Writer,
heap: std.heap.MemoryPool(HeapObject),
variables_stack: std.ArrayListUnmanaged(?*HeapObject),
tags_stack: std.ArrayListUnmanaged(ValueTag),
data_stack: std.ArrayListUnmanaged(StackData),
left: ?Value = null,
right: ?Value = null,

const Self = @This();

const StackData = union {
    number: f64,
    string_ptr: [*]const u8,
    string_len: usize,
    object: *HeapObject,
};

pub fn init(allocator: Allocator, ast: Ast, out: Writer) Self {
    return .{
        .allocator = allocator,
        .ast = ast,
        .out = out,
        .tags_stack = .{},
        .data_stack = .{},
        .variables_stack = .{},
        .heap = std.heap.MemoryPool(HeapObject).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.freeArguments();

    while (self.tags_stack.items.len > 0) {
        self.popValue();
        self.freeArguments();
    }
    self.data_stack.deinit(self.allocator);
    self.tags_stack.deinit(self.allocator);

    while (self.variables_stack.pop()) |maybe_variable|
        if (maybe_variable) |variable|
            self.decrementRef(variable);
    self.variables_stack.deinit(self.allocator);

    self.heap.deinit();
}

pub fn run(self: *Self) !void {
    for (self.ast.tags, 0..) |op, op_index| {
        defer self.freeArguments();
        switch (op) {
            .nil => try self.pushValue(.nil),
            .true => try self.pushValue(.true),
            .false => try self.pushValue(.false),
            .undefined => return error.Semantics,

            .group => {},
            .not => {
                self.popValue();
                const result: Value = switch (self.right.?) {
                    .nil, .false => .true,
                    else => .false,
                };
                try self.pushValue(result);
            },
            .unary_minus => {
                self.popValue();
                const result = -(try self.rightNumber());
                try self.pushValue(.{ .number = result });
            },
            .discard => self.popValue(),
            .print => {
                self.popValue();
                try self.out.print("{s}\n", .{self.right.?});
            },

            .number => try self.pushValue(.{ .number = self.ast.node(op_index).number() }),
            .string => try self.pushValue(.{ .internal_string = self.ast.node(op_index).string() }),
            .alloc_frame => {
                const frame_size = self.ast.node(op_index).dataIndex();
                try self.variables_stack.appendNTimes(self.allocator, null, frame_size);
            },
            .var_decl => {
                const variable_index = self.ast.node(op_index).dataIndex();
                try self.set(variable_index, .{ .nil = {} });
            },
            .identifier => {
                const variable_index = self.ast.node(op_index).dataIndex();
                const variable = self.variables_stack.items[variable_index] orelse return error.Semantics;
                try self.pushValue(variable.value());
            },

            .var_decl_init => {
                self.popValue();
                const variable_index = self.ast.node(op_index).dataIndex();
                try self.set(variable_index, self.right.?);
            },

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
        }
    }
}

pub fn evaluate(self: *Self) !Value {
    try self.run();

    self.popValue();
    return self.right.?;
}

fn pushValue(self: *Self, value: Value) !void {
    try self.tags_stack.append(self.allocator, std.meta.activeTag(value));
    switch (value) {
        .nil, .true, .false => {},
        .number => |number| try self.data_stack.append(self.allocator, .{ .number = number }),
        .internal_string => |string| {
            try self.data_stack.append(self.allocator, .{ .string_ptr = string.ptr });
            try self.data_stack.append(self.allocator, .{ .string_len = string.len });
        },
        .heap_string => |object| {
            try self.data_stack.append(self.allocator, .{ .object = object });
            object.*.ref_count += 1;
        },
    }
}

fn leftNumber(self: Self) !f64 {
    return switch (self.left.?) {
        .number => |number| number,
        else => error.Semantics,
    };
}

fn rightNumber(self: Self) !f64 {
    return switch (self.right.?) {
        .number => |number| number,
        else => error.Semantics,
    };
}

fn freeArguments(self: *Self) void {
    if (self.right) |value| {
        switch (value) {
            .heap_string => |object| {
                self.decrementRef(object);
            },
            else => {},
        }
        self.right = null;
    }
    if (self.left) |value| {
        switch (value) {
            .heap_string => |object| {
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
            .string_buffer => self.allocator.free(object.data.string_buffer),
            .string_prefix => self.decrementRef(object.data.string_prefix.object),
            else => {},
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
        .internal_string => blk: {
            const len = self.data_stack.pop().?.string_len;
            const ptr = self.data_stack.pop().?.string_ptr;
            break :blk .{ .internal_string = ptr[0..len] };
        },
        .heap_string => .{ .heap_string = self.data_stack.pop().?.object },
    };

    if (self.right == null) {
        self.right = value;
    } else self.left = value;
}

fn binary(self: *Self, operator: fn (*Self) error{ OutOfMemory, Semantics }!void) !void {
    self.popValue();
    self.popValue();
    try operator(self);
}

fn multiply(self: *Self) !void {
    try self.pushValue(.{ .number = try self.leftNumber() * try self.rightNumber() });
}

fn divide(self: *Self) !void {
    try self.pushValue(.{ .number = try self.leftNumber() / try self.rightNumber() });
}

fn add(self: *Self) !void {
    sw: switch (self.left.?) {
        .number => |number| try self.pushValue(.{ .number = number + try self.rightNumber() }),
        .internal_string => |left_string| {
            const right_string = switch (self.right.?) {
                .internal_string, .heap_string => self.right.?.string(),
                else => return error.Semantics,
            };

            const buffer = try self.allocator.alloc(u8, left_string.len + right_string.len);
            errdefer self.allocator.free(buffer);

            @memcpy(buffer[0..left_string.len], left_string);
            @memcpy(buffer[left_string.len..], right_string);

            const object = try self.heap.create();
            errdefer self.heap.destroy(object);

            object.tag = .string_buffer;
            object.ref_count = 0;
            object.data = .{ .string_buffer = buffer };
            try self.pushValue(.{ .heap_string = object });
        },
        .heap_string => |left_object| {
            var buffer = switch (left_object.tag) {
                .string_prefix => continue :sw .{ .internal_string = left_object.string() },
                .string_buffer => left_object.data.string_buffer,
                else => unreachable,
            };

            const right_string = switch (self.right.?) {
                .internal_string, .heap_string => self.right.?.string(),
                else => return error.Semantics,
            };

            const left_len = buffer.len;
            buffer = try self.allocator.realloc(buffer, left_len + right_string.len);
            @memcpy(buffer[left_len..], right_string);

            const object = try self.heap.create();
            errdefer self.heap.destroy(object);

            object.tag = .string_buffer;
            object.ref_count = 1;
            object.data = .{ .string_buffer = buffer };
            try self.pushValue(.{ .heap_string = object });

            left_object.tag = .string_prefix;
            left_object.data = .{ .string_prefix = .{ .object = object, .len = left_len } };
        },
        else => return error.Semantics,
    }
}

fn substract(self: *Self) !void {
    try self.pushValue(.{ .number = try self.leftNumber() - try self.rightNumber() });
}

fn greater(self: *Self) !void {
    const result = try self.leftNumber() > try self.rightNumber();
    try self.pushValue(if (result) .true else .false);
}

fn greater_equal(self: *Self) !void {
    const result = try self.leftNumber() >= try self.rightNumber();
    try self.pushValue(if (result) .true else .false);
}

fn less(self: *Self) !void {
    const result = try self.leftNumber() < try self.rightNumber();
    try self.pushValue(if (result) .true else .false);
}

fn less_equal(self: *Self) !void {
    const result = try self.leftNumber() <= try self.rightNumber();
    try self.pushValue(if (result) .true else .false);
}

fn equal(self: *Self) !void {
    const result = self.isEqual();
    try self.pushValue(if (result) .true else .false);
}

fn not_equal(self: *Self) !void {
    const result = !self.isEqual();
    try self.pushValue(if (result) .true else .false);
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
        .internal_string, .heap_string => switch (self.right.?) {
            .internal_string, .heap_string => std.mem.eql(u8, self.left.?.string(), self.right.?.string()),
            else => false,
        },
    };
}

fn setVariable(self: *Self, variable_index: usize, value: Value) !void {
    if (self.variables_stack.items[variable_index] != null) return error.Semantics;

    const variable = try self.heap.create();
    variable.ref_count = 1;
    variable.setValue(self.right.?);
    self.variables_stack.items[variable_index] = variable;
}

fn testEvaluate(source: []const u8, expected: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;
    const parsing = @import("parsing.zig");

    const ast = try parsing.parse(allocator, source, .expression);
    defer ast.deinit(allocator);

    var runtime = Self.init(allocator, ast, undefined);
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

fn testSemanticsError(source: []const u8) !void {
    const result = testEvaluate(source, "");
    try std.testing.expectError(error.Semantics, result);
}

test "semantics errors" {
    try testSemanticsError("-\"foo\"");
    try testSemanticsError("-(\"hello\" + \" world!\")");
    try testSemanticsError("\"foo\" * 42");
    try testSemanticsError("true / 2");
    try testSemanticsError("\"quz\" + 2");
    try testSemanticsError("2 + a");
}

fn testRun(source: []const u8, expected: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;
    const parsing = @import("parsing.zig");

    const ast = try parsing.parse(allocator, source, .program);
    defer ast.deinit(allocator);

    var out_buffer = std.ArrayList(u8).init(allocator);
    defer out_buffer.deinit();

    var runtime = Self.init(allocator, ast, out_buffer.writer().any());
    defer runtime.deinit();

    try runtime.run();

    try testing.expectEqualStrings(expected, out_buffer.items);
}

test "run statements" {
    try testRun("print \"Hello, World!\";", "Hello, World!\n");
    try testRun("print 12 + 24;", "36\n");
    try testRun("print true;", "true\n");
    try testRun(
        \\var bar = 99;
        \\var foo = 99;
        \\print bar + foo;
        \\var quz = 99;
        \\print bar + foo + quz;
    ,
        \\198
        \\297
        \\
    );
}
