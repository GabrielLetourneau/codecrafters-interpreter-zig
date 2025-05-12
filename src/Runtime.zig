const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.io.AnyWriter;

const Bytecode = @import("Bytecode.zig");

pub const Value = union(enum) {
    nil: void,
    true: void,
    false: void,
    clock: void,
    number: f64,
    internal_string: []const u8,
    heap_string: *HeapObject,
    jump_target: usize,
    function: FunctionDefinition,

    pub fn format(self: Value, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .nil, .true, .false, .clock => try writer.writeAll(@tagName(self)),
            .number => |number| try writer.print("{d}", .{number}),
            .internal_string, .heap_string => try writer.writeAll(self.string()),
            .jump_target => try writer.writeAll("<jmp>"),
            .function => |function| {
                try writer.print("<fn {s}>", .{function.name.string()});
            },
        }
    }

    fn string(self: Value) []const u8 {
        return switch (self) {
            .internal_string => |internal_string| internal_string,
            .heap_string => |object| object.string(),
            else => unreachable,
        };
    }

    fn truthy(self: Value) bool {
        return switch (self) {
            .nil, .false => false,
            else => true,
        };
    }

    fn checkedNumber(self: Value) !f64 {
        return switch (self) {
            .number => |number| number,
            else => error.Semantics,
        };
    }
};

const ValueTag = std.meta.Tag(Value);

pub const FunctionDefinition = struct {
    op_index: usize,
    name: *HeapObject,
};

pub const HeapObjectTag = enum {
    nil,
    true,
    false,
    clock,
    number,
    internal_string,
    heap_string,
    string_buffer,
    string_prefix,
    function,
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
            .clock => .{ .clock = {} },
            .number => .{ .number = self.data.number },
            .internal_string => .{ .internal_string = self.data.internal_string },
            .heap_string => .{ .heap_string = self.data.heap_string },
            .string_buffer, .string_prefix => unreachable,
            .function => .{ .function = self.data.function },
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
            .clock => {
                self.tag = .clock;
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
            .jump_target => unreachable,
            .function => |function| {
                self.tag = .function;
                self.data = .{ .function = function };
            },
        }
    }
};

pub const HeapData = union {
    empty: void,
    number: f64,
    internal_string: []const u8,
    heap_string: *HeapObject,
    string_buffer: []u8,
    string_prefix: struct { object: *HeapObject, len: usize },
    function: FunctionDefinition,
};

allocator: Allocator,
out: Writer,

heap: std.heap.MemoryPool(HeapObject),

variables_stack: std.ArrayListUnmanaged(*HeapObject),

tags_stack: std.ArrayListUnmanaged(ValueTag),
data_stack: std.ArrayListUnmanaged(StackData),

const Self = @This();

const StackData = union {
    number: f64,
    string_ptr: [*]const u8,
    string_len: usize,
    object: *HeapObject,
    op_index: usize,
    identifier: usize,
};

pub fn init(allocator: Allocator, out: Writer) Self {
    return .{
        .allocator = allocator,
        .out = out,
        .tags_stack = .{},
        .data_stack = .{},
        .variables_stack = .{},
        .heap = std.heap.MemoryPool(HeapObject).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    while (self.tags_stack.items.len > 0) {
        self.free(self.pop());
    }
    self.data_stack.deinit(self.allocator);
    self.tags_stack.deinit(self.allocator);

    while (self.variables_stack.pop()) |variable|
        self.decrementRef(variable);
    self.variables_stack.deinit(self.allocator);

    self.heap.deinit();
}

pub fn run(self: *Self, start: Bytecode.Instruction) !void {
    var inst = start;
    while (!inst.finished()) : (inst = inst.next()) {
        sw: switch (inst.op()) {
            .free_frame => {
                var frame_size = inst.size();
                while (frame_size != 0) : (frame_size -= 1)
                    self.decrementRef(self.variables_stack.pop().?);
            },
            .branch_uncond => {
                inst = inst.target();
                if (inst.finished()) {
                    break;
                } else continue :sw inst.op();
            },

            .nil => try self.push(.nil),
            .true => try self.push(.true),
            .false => try self.push(.false),
            .undefined => return error.Semantics,
            .number => try self.push(.{ .number = inst.number() }),
            .string => try self.push(.{ .internal_string = inst.string() }),
            .variable => {
                const variable_index = inst.variable();
                const variable = self.variables_stack.items[variable_index];
                try self.push(variable.value());
            },
            .clock => try self.push(.clock),

            .not => {
                const value = self.pop();
                defer self.free(value);

                const result: Value = if (value.truthy()) .{ .false = {} } else .{ .true = {} };
                try self.push(result);
            },
            .unary_minus => {
                const value = self.pop();
                defer self.free(value);

                const result = -(try value.checkedNumber());
                try self.push(.{ .number = result });
            },
            .assign => {
                const value = self.pop();
                defer self.free(value);

                const variable = self.variables_stack.items[inst.variable()];

                const maybe_existing_heap_string = switch (variable.tag) {
                    .heap_string => variable.data.heap_string,
                    else => null,
                };

                variable.setValue(value);

                if (maybe_existing_heap_string) |heap_string|
                    self.decrementRef(heap_string);

                try self.push(value);
            },
            .def_fun => {
                const name_value = self.pop();
                const name_object = try self.heap.create();
                errdefer self.heap.destroy(name_object);
                name_object.ref_count = 1;
                name_object.setValue(name_value);

                try self.push(.{ .function = .{
                    .op_index = inst.target().op_index,
                    .name = name_object,
                } });
            },

            .@"or" => {
                const value = self.pop();
                defer self.free(value);

                if (value.truthy()) {
                    try self.push(value); // replaces popped value
                    inst = inst.target();
                    continue :sw inst.op();
                }
            },
            .@"and" => {
                const value = self.pop();
                defer self.free(value);

                if (!value.truthy()) {
                    try self.push(value); // replaces popped value
                    inst = inst.target();
                    continue :sw inst.op();
                }
            },

            .alloc => {
                const value = self.pop();
                defer self.free(value);

                const variable = try self.heap.create();
                errdefer self.heap.destroy(variable);
                variable.ref_count = 1;
                variable.setValue(value);
                try self.variables_stack.append(self.allocator, variable);
            },
            .discard => self.free(self.pop()),
            .print => {
                const value = self.pop();
                defer self.free(value);

                try self.out.print("{s}\n", .{value});
            },
            .branch_cond_not => {
                const value = self.pop();
                defer self.free(value);

                if (!value.truthy()) {
                    inst = inst.target();
                    if (inst.finished()) {
                        break;
                    } else continue :sw inst.op();
                }
            },
            .call => {
                const value = self.pop();
                defer self.free(value);

                if (inst.size() != 0) return error.Semantics;

                switch (value) {
                    .clock => {
                        const time_in_nanoseconds: f64 = @floatFromInt(std.time.nanoTimestamp());
                        const time_in_seconds: f64 = time_in_nanoseconds / @as(f64, @floatFromInt(std.time.ns_per_s));
                        try self.push(.{ .number = time_in_seconds });
                    },
                    .function => |function| {
                        try self.push(.{ .jump_target = inst.next().op_index });
                        inst = .{ .bytecode = inst.bytecode, .op_index = function.op_index };
                        continue :sw inst.op();
                    },
                    else => return error.Semantics,
                }
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

            .@"return" => {
                const return_value = self.pop();
                defer self.free(return_value);

                const return_target = self.pop();

                try self.push(return_value);

                inst = .{ .bytecode = inst.bytecode, .op_index = return_target.jump_target };
                continue :sw inst.op();
            },
        }
    }
}

pub fn evaluate(self: *Self, start: Bytecode.Instruction) !Value {
    try self.run(start);

    return self.pop();
}

pub fn free(self: *Self, value: Value) void {
    switch (value) {
        .heap_string => |object| {
            self.decrementRef(object);
        },
        .function => |function| {
            self.decrementRef(function.name);
        },
        else => {},
    }
}

fn decrementRef(self: *Self, object: *HeapObject) void {
    object.*.ref_count -= 1;
    if (object.ref_count == 0) {
        const object_copy = object.*;
        self.heap.destroy(object);

        switch (object_copy.tag) {
            .heap_string => self.decrementRef(object_copy.data.heap_string),
            .string_buffer => self.allocator.free(object_copy.data.string_buffer),
            .string_prefix => self.decrementRef(object_copy.data.string_prefix.object),
            else => {},
        }
    }
}

fn push(self: *Self, value: Value) !void {
    try self.tags_stack.append(self.allocator, std.meta.activeTag(value));
    switch (value) {
        .nil, .true, .false, .clock => {},
        .number => |number| try self.data_stack.append(self.allocator, .{ .number = number }),
        .internal_string => |string| {
            try self.data_stack.append(self.allocator, .{ .string_ptr = string.ptr });
            try self.data_stack.append(self.allocator, .{ .string_len = string.len });
        },
        .heap_string => |object| {
            try self.data_stack.append(self.allocator, .{ .object = object });
            object.*.ref_count += 1;
        },
        .jump_target => |op_index| try self.data_stack.append(self.allocator, .{ .op_index = op_index }),
        .function => |function| {
            try self.data_stack.append(self.allocator, .{ .op_index = function.op_index });
            try self.data_stack.append(self.allocator, .{ .object = function.name });
        },
    }
}

fn pop(self: *Self) Value {
    return switch (self.tags_stack.pop().?) {
        .nil => .{ .nil = {} },
        .true => .{ .true = {} },
        .false => .{ .false = {} },
        .clock => .{ .clock = {} },
        .number => .{ .number = self.data_stack.pop().?.number },
        .internal_string => blk: {
            const len = self.data_stack.pop().?.string_len;
            const ptr = self.data_stack.pop().?.string_ptr;
            break :blk .{ .internal_string = ptr[0..len] };
        },
        .heap_string => .{ .heap_string = self.data_stack.pop().?.object },
        .jump_target => .{ .jump_target = self.data_stack.pop().?.op_index },
        .function => blk: {
            const name = self.data_stack.pop().?.object;
            name.ref_count += 1;
            const op_index = self.data_stack.pop().?.op_index;
            break :blk .{ .function = .{ .op_index = op_index, .name = name } };
        },
    };
}

fn binary(self: *Self, operator: fn (*Self, Value, Value) error{ OutOfMemory, Semantics }!void) !void {
    const right = self.pop();
    defer self.free(right);
    const left = self.pop();
    defer self.free(left);
    try operator(self, left, right);
}

fn multiply(self: *Self, left: Value, right: Value) !void {
    try self.push(.{ .number = try left.checkedNumber() * try right.checkedNumber() });
}

fn divide(self: *Self, left: Value, right: Value) !void {
    try self.push(.{ .number = try left.checkedNumber() / try right.checkedNumber() });
}

fn add(self: *Self, left: Value, right: Value) !void {
    sw: switch (left) {
        .number => |number| try self.push(.{ .number = number + try right.checkedNumber() }),
        .internal_string => |left_string| {
            const right_string = switch (right) {
                .internal_string, .heap_string => right.string(),
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
            try self.push(.{ .heap_string = object });
        },
        .heap_string => |left_object| {
            var buffer = switch (left_object.tag) {
                .string_prefix => continue :sw .{ .internal_string = left_object.string() },
                .string_buffer => left_object.data.string_buffer,
                else => unreachable,
            };

            const right_string = switch (right) {
                .internal_string, .heap_string => right.string(),
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
            try self.push(.{ .heap_string = object });

            left_object.tag = .string_prefix;
            left_object.data = .{ .string_prefix = .{ .object = object, .len = left_len } };
        },
        else => return error.Semantics,
    }
}

fn substract(self: *Self, left: Value, right: Value) !void {
    try self.push(.{ .number = try left.checkedNumber() - try right.checkedNumber() });
}

fn greater(self: *Self, left: Value, right: Value) !void {
    const result = try left.checkedNumber() > try right.checkedNumber();
    try self.push(if (result) .true else .false);
}

fn greater_equal(self: *Self, left: Value, right: Value) !void {
    const result = try left.checkedNumber() >= try right.checkedNumber();
    try self.push(if (result) .true else .false);
}

fn less(self: *Self, left: Value, right: Value) !void {
    const result = try left.checkedNumber() < try right.checkedNumber();
    try self.push(if (result) .true else .false);
}

fn less_equal(self: *Self, left: Value, right: Value) !void {
    const result = try left.checkedNumber() <= try right.checkedNumber();
    try self.push(if (result) .true else .false);
}

fn equal(self: *Self, left: Value, right: Value) !void {
    const result = isEqual(left, right);
    try self.push(if (result) .true else .false);
}

fn not_equal(self: *Self, left: Value, right: Value) !void {
    const result = !isEqual(left, right);
    try self.push(if (result) .true else .false);
}

fn isEqual(left: Value, right: Value) bool {
    return switch (left) {
        .nil => right == .nil,
        .true => right == .true,
        .false => right == .false,
        .clock => right == .clock,
        .number => |left_number| switch (right) {
            .number => |right_number| left_number == right_number,
            else => false,
        },
        .internal_string, .heap_string => switch (right) {
            .internal_string, .heap_string => std.mem.eql(u8, left.string(), right.string()),
            else => false,
        },
        .jump_target => unreachable,
        .function => |left_function| switch (right) {
            .function => |right_function| left_function.op_index == right_function.op_index,
            else => false,
        },
    };
}

fn testEvaluate(source: []const u8, expected: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;
    const parsing = @import("parsing.zig");
    const gen = @import("gen.zig");

    const bytecode = blk: {
        const ast = try parsing.parse(allocator, source, .expression);
        defer ast.deinit(allocator);
        errdefer ast.deinitStrings(allocator);

        break :blk try gen.generate(allocator, ast.root().?, .expression);
    };
    defer bytecode.deinit(allocator);

    var runtime = Self.init(allocator, undefined);
    defer runtime.deinit();

    const value = try runtime.evaluate(bytecode.startOp().?);
    defer runtime.free(value);

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
    const gen = @import("gen.zig");

    const bytecode = blk: {
        const ast = try parsing.parse(allocator, source, .program);
        defer ast.deinit(allocator);
        errdefer ast.deinitStrings(allocator);

        break :blk try gen.generate(allocator, ast.root().?, .program);
    };
    defer bytecode.deinit(allocator);

    var out_buffer = std.ArrayList(u8).init(allocator);
    defer out_buffer.deinit();

    var runtime = Self.init(allocator, out_buffer.writer().any());
    defer runtime.deinit();

    try runtime.run(bytecode.startOp().?);

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
    try testRun(
        \\var bar;
        \\print bar;
    ,
        \\nil
        \\
    );
    try testRun(
        \\var baz = "before";
        \\print baz;
        \\var baz = "after";
        \\print baz;
    ,
        \\before
        \\after
        \\
    );
    try testRun(
        \\var quz;
        \\var hello;
        \\
        \\quz = hello = 16 + 34 * 92;
        \\print quz;
        \\print hello;
    ,
        \\3144
        \\3144
        \\
    );
    try testRun(
        \\{
        \\    var world = "before";
        \\    print world;        
        \\}
        \\{
        \\    var world = "after";
        \\    print world;
        \\}
    ,
        \\before
        \\after
        \\
    );
    try testRun(
        \\{
        \\    var world = "before";
        \\    {
        \\        var world = "after";
        \\        print world;
        \\    }
        \\    print world;
        \\}
    ,
        \\after
        \\before
        \\
    );
}

test "control flow" {
    try testRun(
        \\var stage = "unknown";
        \\var age = 50;
        \\if (age < 18) { stage = "child"; }
        \\if (age >= 18) { stage = "adult"; }
        \\print stage;
        \\
        \\var isAdult = age >= 18;
        \\if (isAdult) { print "eligible for voting: true"; }
        \\if (!isAdult) { print "eligible for voting: false"; }
    ,
        \\adult
        \\eligible for voting: true
        \\
    );
    try testRun(
        \\if (true) print "if branch"; else print "else branch";
        \\if (false) print "if branch"; else if (false) print "else-if branch";
    ,
        \\if branch
        \\
    );
    try testRun(
        \\if (false or "ok") print "baz";
        \\if (nil or "ok") print "baz";
        \\
        \\if (false or false) print "world";
        \\if (true or "world") print "world";
        \\
        \\if (24 or "bar") print "bar";
        \\if ("bar" or "bar") print "bar";
    ,
        \\baz
        \\baz
        \\world
        \\bar
        \\bar
        \\
    );
    try testRun(
        \\print false and 1;
        \\print true and 1;
        \\print 23 and "hello" and false;
        \\
        \\print 23 and true;
        \\print 23 and "hello" and 23;
    ,
        \\false
        \\1
        \\false
        \\true
        \\23
        \\
    );
    try testRun(
        \\var foo = 0;
        \\while (foo < 3) print foo = foo + 1;
    ,
        \\1
        \\2
        \\3
        \\
    );
    try testRun(
        \\for (var baz = 0; baz < 3;) print baz = baz + 1;
    ,
        \\1
        \\2
        \\3
        \\
    );
    try testRun(
        \\var world = 0;
        \\for (; world < 2; world = world + 1) print world;
    ,
        \\0
        \\1
        \\
    );
    try testRun(
        \\ var foo = "after";
        \\ {
        \\   var foo = "before";
        \\
        \\   for (var foo = 0; foo < 1; foo = foo + 1) {
        \\     print foo;
        \\     var foo = -1;
        \\     print foo;
        \\   }
        \\ }
    ,
        \\0
        \\-1
        \\
    );
}

test "functions" {
    try testRun("print (clock() > 1746902501);", "true\n");
    try testRun(
        \\fun foo() { return 10; }
        \\print foo();
    ,
        \\10
        \\
    );
    try testRun(
        \\fun foo() {}
        \\print foo;
    ,
        \\<fn foo>
        \\
    );
    try testRun(
        \\fun cumulative_sum() {
        \\    var n = 10;  // Fixed value
        \\    var total = 0;
        \\    var i = 1;
        \\    while (i <= n) {
        \\        total = total + i;
        \\        i = i + 1;
        \\    }
        \\    print "The cumulative sum from 1 to 10 is: ";
        \\    print total;
        \\}
        \\
        \\cumulative_sum();
    ,
        \\The cumulative sum from 1 to 10 is: 
        \\55
        \\
    );
}
