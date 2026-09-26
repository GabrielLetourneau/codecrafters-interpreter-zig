const std = @import("std");
const Allocator = std.mem.Allocator;
const Writer = std.Io.Writer;

const Bytecode = @import("Bytecode.zig");
const Ast = @import("Ast.zig");

pub const Value = union(enum) {
    nil: void,
    true: void,
    false: void,
    clock: void,
    number: f64,
    internal_string: []const u8,
    heap_string: *HeapObject,
    jump_target: usize,
    function: *HeapObject,
    instance: *HeapObject,
    class: *HeapObject,

    pub fn inContext(self: Value, bytecode: *const Bytecode) ValueInContext {
        return .{ .bytecode = bytecode, .value = self };
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
            else => error.Runtime,
        };
    }
};

const ValueTag = std.meta.Tag(Value);

pub const ValueInContext = struct {
    bytecode: *const Bytecode,
    value: Value,

    pub fn format(self: ValueInContext, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        const value = self.value;
        switch (value) {
            .nil, .true, .false, .clock => try writer.writeAll(@tagName(value)),
            .number => |number| try writer.print("{d}", .{number}),
            .internal_string, .heap_string => try writeStringValue(value, writer),
            .jump_target => |target| try writer.print("<jmp {d}>", .{target}),
            .function => |object| {
                const bytecode = self.bytecode;
                const string_start_index = bytecode.function_names[object.data.function.function_index];
                try writer.print("<fn {s}>", .{bytecode.stringAtIndex(string_start_index)});
            },
            .instance => |object| {
                const class_index = object.data.instance_body.class.data.class_body.class_index;
                const name_index = self.bytecode.class_defs[class_index].name_index;
                try writer.print("{s} instance", .{self.bytecode.stringAtIndex(name_index)});
            },
            .class => |object| {
                const name_index = self.bytecode.class_defs[object.data.class_body.class_index].name_index;
                try writer.writeAll(self.bytecode.stringAtIndex(name_index));
            },
        }
    }
};

fn writeStringValue(value: Value, writer: *std.Io.Writer) std.Io.Writer.Error!void {
    switch (value) {
        .internal_string => |string| try writer.writeAll(string),
        .heap_string => |object| {
            const size = object.data.string_header.size;
            if (size == 0) return;
            const tail_count: usize = if (size % 8 == 0) 8 else size % 8;
            try writeChain(writer, object.data.string_header.last.?, tail_count);
        },
        else => unreachable,
    }
}

fn writeChain(writer: *std.Io.Writer, part: *HeapObject, part_count: usize) std.Io.Writer.Error!void {
    if (part.data.string_part.prev) |prev| try writeChain(writer, prev, 8);
    try writer.writeAll(part.data.string_part.data[0..part_count]);
}

pub const Function = struct {
    function_index: usize,
    capture: ?*HeapObject,
};

pub const HeapObjectTag = enum {
    nil,
    true,
    false,
    clock,
    number,
    string_header,
    string_part,
    function,
    capture,
    variable,
    member,
    member_list,
    class_body,
    instance_body,
};

const HeapVariable = union {
    empty: void,
    number: f64,
    internal_string: []const u8,
    object: *HeapObject,
};

fn objectValue(tag: ValueTag, object: *HeapObject) Value {
    return switch (tag) {
        .heap_string => .{ .heap_string = object },
        .function => .{ .function = object },
        .instance => .{ .instance = object },
        .class => .{ .class = object },
        else => unreachable,
    };
}

pub const HeapObject = struct {
    tag: HeapObjectTag,
    value_tag: ValueTag,
    ref_count: u32,
    data: HeapData,

    fn value(self: *HeapObject) Value {
        return switch (self.tag) {
            .nil => .{ .nil = {} },
            .true => .{ .true = {} },
            .false => .{ .false = {} },
            .clock => .{ .clock = {} },
            .number => .{ .number = self.data.number },
            .string_header => .{ .heap_string = self },
            .string_part, .capture, .member, .member_list => unreachable,
            .function => .{ .function = self },
            .class_body => .{ .class = self },
            .instance_body => .{ .instance = self },
            .variable => switch (self.value_tag) {
                .nil => .{ .nil = {} },
                .true => .{ .true = {} },
                .false => .{ .false = {} },
                .clock => .{ .clock = {} },
                .number => .{ .number = self.data.variable.number },
                .internal_string => .{ .internal_string = self.data.variable.internal_string },
                .heap_string, .function, .instance, .class => objectValue(self.value_tag, self.data.variable.object),
                .jump_target => unreachable,
            },
        };
    }

    fn storeValue(self: *HeapObject, new_value: Value) void {
        self.value_tag = std.meta.activeTag(new_value);
        switch (new_value) {
            .nil, .true, .false, .clock => self.data = .{ .variable = .{ .empty = {} } },
            .number => |number| self.data = .{ .variable = .{ .number = number } },
            .internal_string => |string| self.data = .{ .variable = .{ .internal_string = string } },
            .heap_string, .function, .instance, .class => |object| {
                self.data = .{ .variable = .{ .object = object } };
                object.ref_count += 1;
            },
            .jump_target => unreachable,
        }
    }
};

pub const HeapData = union {
    empty: void,
    number: f64,
    internal_string: []const u8,
    string_header: struct {
        size: usize,
        last: ?*HeapObject,
    },
    string_part: struct {
        prev: ?*HeapObject,
        data: [8]u8,
    },
    function: Function,
    capture: struct {
        variable: *HeapObject,
        next: ?*HeapObject,
    },
    variable: HeapVariable,
    member: struct {
        name_index: usize,
        value: *HeapObject,
    },
    member_list: struct {
        member: *HeapObject,
        next: ?*HeapObject,
    },
    class_body: struct {
        class_index: usize,
        methods: ?*HeapObject,
    },
    instance_body: struct {
        class: *HeapObject,
        members: ?*HeapObject,
    },
};

allocator: Allocator,
io: std.Io,
out: *Writer,

heap: std.heap.MemoryPool(HeapObject) = .empty,

variables_stack: std.ArrayListUnmanaged(*HeapObject),

tags_stack: std.ArrayListUnmanaged(ValueTag),
data_stack: std.ArrayListUnmanaged(StackData),

nil_singleton: HeapObject = .{ .tag = .nil, .value_tag = .nil, .ref_count = 1, .data = .{ .empty = {} } },
true_singleton: HeapObject = .{ .tag = .true, .value_tag = .nil, .ref_count = 1, .data = .{ .empty = {} } },
false_singleton: HeapObject = .{ .tag = .false, .value_tag = .nil, .ref_count = 1, .data = .{ .empty = {} } },
clock_singleton: HeapObject = .{ .tag = .clock, .value_tag = .nil, .ref_count = 1, .data = .{ .empty = {} } },

const Self = @This();

const StackData = union {
    number: f64,
    string_ptr: [*]const u8,
    string_len: usize,
    object: *HeapObject,
    op_index: usize,
};

pub fn init(allocator: Allocator, io: std.Io, out: *Writer) Self {
    return .{
        .allocator = allocator,
        .io = io,
        .out = out,
        .tags_stack = .empty,
        .data_stack = .empty,
        .variables_stack = .empty,
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

    self.heap.deinit(self.allocator);
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
            .undefined => return error.Runtime,
            .number => try self.push(.{ .number = inst.number() }),
            .string => try self.push(.{ .internal_string = inst.string() }),
            .variable => {
                const variable_index = inst.variable();
                const variable = self.variableAtIndex(variable_index);
                try self.push(variable.value());
            },
            .clock => try self.push(.clock),
            .def_fun => {
                const function = try self.heap.create(self.allocator);
                errdefer self.heap.destroy(function);
                function.* = .{
                    .tag = .function,
                    .value_tag = .nil,
                    .ref_count = 0,
                    .data = .{ .function = .{ .function_index = inst.functionIndex(), .capture = null } },
                };
                try self.push(.{ .function = function });
            },
            .def_class => {
                const class_object = try self.heap.create(self.allocator);
                errdefer self.heap.destroy(class_object);
                class_object.* = .{
                    .tag = .class_body,
                    .value_tag = .nil,
                    .ref_count = 0,
                    .data = .{ .class_body = .{ .class_index = inst.classIndex(), .methods = null } },
                };

                try self.push(.{ .class = class_object });
            },
            .store_method => {
                const closure = self.pop();
                errdefer self.free(closure);
                const closure_object = closure.function;

                if (self.tags_stack.items[self.tags_stack.items.len - 1] != .class)
                    return error.Runtime;
                const class_object = self.data_stack.items[self.data_stack.items.len - 1].object;

                const class_def = inst.bytecode.class_defs[class_object.data.class_body.class_index];
                const name_index = inst.bytecode.class_methods[class_def.methods_start + inst.methodSlot()].name_index;

                const member = try self.heap.create(self.allocator);
                errdefer self.heap.destroy(member);
                member.* = .{
                    .tag = .member,
                    .value_tag = .nil,
                    .ref_count = 0,
                    .data = .{ .member = .{ .name_index = name_index, .value = closure_object } },
                };
                closure_object.ref_count += 1;

                const methods = &class_object.data.class_body.methods;
                const link = try self.heap.create(self.allocator);
                errdefer self.heap.destroy(link);
                link.* = .{
                    .tag = .member_list,
                    .value_tag = .nil,
                    .ref_count = 0,
                    .data = .{ .member_list = .{ .member = member, .next = methods.* } },
                };
                member.ref_count += 1;
                if (methods.*) |old_head|
                    old_head.ref_count += 1;
                methods.* = link;
                link.ref_count += 1;

                self.decrementRef(closure_object);
            },

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
                const variable = self.variableAtIndex(inst.variable());

                self.releaseVariableValue(variable);
                variable.storeValue(value);

                try self.push(value);
            },
            .capture => {
                var value = self.pop();
                defer self.free(value);

                const captured_variable = self.variableAtIndex(inst.variable());
                captured_variable.ref_count += 1;

                const capture = try self.heap.create(self.allocator);
                errdefer self.heap.destroy(capture);
                capture.* = .{
                    .tag = .capture,
                    .value_tag = .nil,
                    .ref_count = 0,
                    .data = .{ .capture = .{ .variable = captured_variable, .next = value.function.data.function.capture } },
                };
                if (value.function.data.function.capture) |old_head|
                    old_head.ref_count += 1;
                value.function.data.function.capture = capture;
                capture.ref_count += 1;

                try self.push(value);
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

            .alloc => try self.allocVariable(),
            .discard => self.free(self.pop()),
            .print => {
                const value = self.pop();
                defer self.free(value);

                try self.out.print("{f}\n", .{value.inContext(inst.bytecode)});
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

                switch (value) {
                    .clock => {
                        if (inst.size() != 0) return error.Runtime;
                        const timestamp = std.Io.Timestamp.now(self.io, .real);
                        const time_in_seconds: f64 = @as(f64, @floatFromInt(timestamp.toMicroseconds())) / @as(f64, @floatFromInt(std.time.us_per_s));
                        try self.push(.{ .number = time_in_seconds });
                    },
                    .function => |function| {
                        const function_def = inst.bytecode.function_defs[function.data.function.function_index];

                        if (inst.size() != function_def.param_count) return error.Runtime;

                        var maybe_capture = function.data.function.capture;
                        while (maybe_capture) |capture| {
                            const capture_data = capture.data.capture;
                            const variable = capture_data.variable;
                            variable.ref_count += 1;
                            try self.variables_stack.append(self.allocator, variable);
                            maybe_capture = capture_data.next;
                        }

                        const constructor_instance: ?*HeapObject = if (function_def.is_initializer) blk: {
                            const this_variable = self.variables_stack.items[self.variables_stack.items.len - 1];
                            break :blk switch (this_variable.value()) {
                                .instance => |instance| instance,
                                else => null,
                            };
                        } else null;

                        for (0..function_def.param_count) |_|
                            try self.allocVariable();

                        try self.push(.{ .jump_target = inst.next().op_index });
                        if (constructor_instance) |instance|
                            try self.push(.{ .instance = instance });

                        inst = .{ .bytecode = inst.bytecode, .op_index = function_def.op_index };
                        continue :sw inst.op();
                    },
                    .class => |class_object| {
                        var init_function: ?*HeapObject = null;
                        var init_index: ?usize = null;
                        var methods_node = class_object.data.class_body.methods;
                        while (methods_node) |link| {
                            const member = link.data.member_list.member;
                            const function_index = member.data.member.value.data.function.function_index;
                            if (inst.bytecode.function_defs[function_index].is_initializer) {
                                init_function = member.data.member.value;
                                init_index = function_index;
                                break;
                            }
                            methods_node = link.data.member_list.next;
                        }

                        const param_count: usize = if (init_index) |function_index|
                            inst.bytecode.function_defs[function_index].param_count
                        else
                            0;
                        if (inst.size() != param_count) return error.Runtime;

                        const instance = try self.heap.create(self.allocator);
                        errdefer self.heap.destroy(instance);
                        instance.* = .{
                            .tag = .instance_body,
                            .value_tag = .nil,
                            .ref_count = 0,
                            .data = .{ .instance_body = .{ .class = class_object, .members = null } },
                        };
                        class_object.ref_count += 1;

                        if (init_index) |function_index| {
                            try self.bindMethod(instance, init_function.?);
                            const bound = self.pop();
                            defer self.free(bound);

                            var maybe_capture = bound.function.data.function.capture;
                            while (maybe_capture) |capture| {
                                const variable = capture.data.capture.variable;
                                variable.ref_count += 1;
                                try self.variables_stack.append(self.allocator, variable);
                                maybe_capture = capture.data.capture.next;
                            }

                            for (0..param_count) |_|
                                try self.allocVariable();

                            try self.push(.{ .jump_target = inst.next().op_index });
                            try self.push(.{ .instance = instance });

                            inst = .{ .bytecode = inst.bytecode, .op_index = inst.bytecode.function_defs[function_index].op_index };
                            continue :sw inst.op();
                        }

                        try self.push(.{ .instance = instance });
                    },
                    else => return error.Runtime,
                }
            },
            .get => {
                const object = self.pop();
                defer self.free(object);

                const instance = switch (object) {
                    .instance => |instance| instance,
                    else => return error.Runtime,
                };

                const name_index = inst.nameIndex();
                var member_node = instance.data.instance_body.members;
                while (member_node) |link| {
                    const member = link.data.member_list.member;
                    if (member.data.member.name_index == name_index) {
                        try self.push(member.data.member.value.value());
                        break;
                    }
                    member_node = link.data.member_list.next;
                } else {
                    const class_object = instance.data.instance_body.class;
                    var method_node = class_object.data.class_body.methods;
                    while (method_node) |link| {
                        const member = link.data.member_list.member;
                        if (member.data.member.name_index == name_index) {
                            try self.bindMethod(instance, member.data.member.value);
                            break;
                        }
                        method_node = link.data.member_list.next;
                    } else {
                        return error.Runtime;
                    }
                }
            },
            .set => {
                const object = self.pop();
                defer self.free(object);

                const instance = switch (object) {
                    .instance => |instance| instance,
                    else => return error.Runtime,
                };

                const value = self.pop();
                defer self.free(value);

                const name_index = inst.nameIndex();
                const members = &instance.data.instance_body.members;

                var member_node = members.*;
                while (member_node) |link| {
                    const member = link.data.member_list.member;
                    if (member.data.member.name_index == name_index) {
                        const boxed = try self.box(value);
                        self.decrementRef(member.data.member.value);
                        member.data.member.value = boxed;
                        boxed.ref_count += 1;
                        try self.push(value);
                        break;
                    }
                    member_node = link.data.member_list.next;
                } else {
                    const boxed = try self.box(value);

                    const member = try self.heap.create(self.allocator);
                    errdefer self.heap.destroy(member);
                    member.* = .{
                        .tag = .member,
                        .value_tag = .nil,
                        .ref_count = 0,
                        .data = .{ .member = .{ .name_index = name_index, .value = boxed } },
                    };
                    boxed.ref_count += 1;

                    const link = try self.heap.create(self.allocator);
                    errdefer self.heap.destroy(link);
                    link.* = .{
                        .tag = .member_list,
                        .value_tag = .nil,
                        .ref_count = 0,
                        .data = .{ .member_list = .{ .member = member, .next = members.* } },
                    };
                    member.ref_count += 1;
                    if (members.*) |old_head|
                        old_head.ref_count += 1;
                    members.* = link;
                    link.ref_count += 1;

                    try self.push(value);
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

                const top = self.pop();

                for (0..inst.size()) |_| {
                    const variable = self.variables_stack.pop().?;
                    self.decrementRef(variable);
                }

                var return_target: usize = undefined;
                switch (top) {
                    .instance => {
                        return_target = self.pop().jump_target;
                        try self.push(top);
                        self.free(top);
                    },
                    .jump_target => {
                        return_target = top.jump_target;
                        try self.push(return_value);
                    },
                    else => unreachable,
                }

                inst = .{ .bytecode = inst.bytecode, .op_index = return_target };
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
        .heap_string, .function, .instance, .class => |object| {
            self.decrementRef(object);
        },
        else => {},
    }
}

fn decrementRef(self: *Self, object: *HeapObject) void {
    object.ref_count -= 1;
    if (object.ref_count == 0) {
        const object_copy = object.*;
        self.heap.destroy(object);

        switch (object_copy.tag) {
            .string_header => if (object_copy.data.string_header.last) |last|
                self.decrementRef(last),
            .string_part => if (object_copy.data.string_part.prev) |prev|
                self.decrementRef(prev),
            .function => if (object_copy.data.function.capture) |capture|
                self.decrementRef(capture),
            .capture => {
                if (object_copy.data.capture.next) |next|
                    self.decrementRef(next);
                self.decrementRef(object_copy.data.capture.variable);
            },
            .member => self.decrementRef(object_copy.data.member.value),
            .member_list => {
                self.decrementRef(object_copy.data.member_list.member);
                if (object_copy.data.member_list.next) |link|
                    self.decrementRef(link);
            },
            .class_body => if (object_copy.data.class_body.methods) |methods|
                self.decrementRef(methods),
            .instance_body => {
                self.decrementRef(object_copy.data.instance_body.class);
                if (object_copy.data.instance_body.members) |members|
                    self.decrementRef(members);
            },
            .variable => self.releaseVariableObject(object_copy),
            else => {},
        }
    }
}

fn releaseVariableValue(self: *Self, variable: *HeapObject) void {
    switch (variable.value_tag) {
        .heap_string, .function, .instance, .class => self.decrementRef(variable.data.variable.object),
        else => {},
    }
}

fn releaseVariableObject(self: *Self, variable: HeapObject) void {
    switch (variable.value_tag) {
        .heap_string, .function, .instance, .class => self.decrementRef(variable.data.variable.object),
        else => {},
    }
}

fn allocVariable(self: *Self) !void {
    const value = self.pop();
    errdefer self.free(value);

    const variable = try self.heap.create(self.allocator);
    errdefer self.heap.destroy(variable);
    variable.* = .{ .tag = .variable, .value_tag = .nil, .ref_count = 1, .data = .{ .variable = .{ .empty = {} } } };
    variable.storeValue(value);

    try self.variables_stack.append(self.allocator, variable);
    self.free(value);
}

fn variableAtIndex(self: Self, variable_index: usize) *HeapObject {
    const variable_items = self.variables_stack.items;
    return variable_items[variable_items.len - variable_index];
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
        .heap_string, .function, .instance, .class => |object| try self.pushObject(object),
        .jump_target => |target| try self.data_stack.append(self.allocator, .{ .op_index = target }),
    }
}

fn pushObject(self: *Self, object: *HeapObject) !void {
    try self.data_stack.append(self.allocator, .{ .object = object });
    object.ref_count += 1;
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
        .function => .{ .function = self.data_stack.pop().?.object },
        .instance => .{ .instance = self.data_stack.pop().?.object },
        .class => .{ .class = self.data_stack.pop().?.object },
    };
}

fn binary(self: *Self, operator: fn (*Self, Value, Value) error{ OutOfMemory, Runtime }!void) !void {
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
    switch (left) {
        .number => |number| try self.push(.{ .number = number + try right.checkedNumber() }),
        .internal_string, .heap_string => try self.addString(left, right),
        else => return error.Runtime,
    }
}

fn addString(self: *Self, left: Value, right: Value) !void {
    switch (right) {
        .internal_string, .heap_string => {},
        else => return error.Runtime,
    }

    const header = try self.heap.create(self.allocator);
    errdefer self.heap.destroy(header);
    header.* = .{
        .tag = .string_header,
        .value_tag = .nil,
        .ref_count = 0,
        .data = .{ .string_header = .{ .size = 0, .last = null } },
    };
    errdefer if (header.data.string_header.last) |last|
        self.decrementRef(last);

    switch (left) {
        .internal_string => |string| try self.appendBytes(header, string),
        .heap_string => |object| {
            // Share the left chain so appending stays amortized O(1).
            const left_last = object.data.string_header.last;
            if (left_last) |last|
                last.ref_count += 1;
            header.data.string_header.last = left_last;
            header.data.string_header.size = object.data.string_header.size;
        },
        else => unreachable,
    }
    switch (right) {
        .internal_string => |string| try self.appendBytes(header, string),
        .heap_string => |object| try self.appendChain(header, object),
        else => unreachable,
    }

    try self.push(.{ .heap_string = header });
}

fn appendChain(self: *Self, header: *HeapObject, object: *HeapObject) error{OutOfMemory}!void {
    const size = object.data.string_header.size;
    if (size == 0) return;
    const tail = object.data.string_header.last.?;
    const tail_count: usize = if (size % 8 == 0) 8 else size % 8;
    try self.appendChainParts(header, tail, tail_count);
}

fn appendChainParts(self: *Self, header: *HeapObject, part: *HeapObject, count: usize) error{OutOfMemory}!void {
    if (part.data.string_part.prev) |prev|
        try self.appendChainParts(header, prev, 8);
    try self.appendBytes(header, part.data.string_part.data[0..count]);
}

fn appendBytes(self: *Self, header: *HeapObject, bytes: []const u8) error{OutOfMemory}!void {
    var size = header.data.string_header.size;
    var chunk = bytes;
    if (chunk.len == 0) return;

    if (size != 0) {
        const used = if (size % 8 == 0) 8 else size % 8;
        if (used < 8) {
            const tail = header.data.string_header.last.?;
            if (tail.ref_count == 1) {
                // Merge into the exclusively-held partial tail: safe because other
                // readers are bounded by their own string header sizes.
                const take = @min(8 - used, chunk.len);
                @memcpy(tail.data.string_part.data[used..][0..take], chunk[0..take]);
                size += take;
                chunk = chunk[take..];
                header.data.string_header.size = size;
            } else {
                // The partial tail is shared; absorb it into a fresh node rather
                // than stranding a partial node mid-chain.
                return self.absorbPartialTail(header, chunk);
            }
            if (chunk.len == 0) return;
        }
    }

    var tail = header.data.string_header.last;
    while (chunk.len != 0) {
        const count = @min(8, chunk.len);
        const part = try self.heap.create(self.allocator);
        errdefer self.heap.destroy(part);
        var data = [_]u8{0} ** 8;
        @memcpy(data[0..count], chunk[0..count]);
        part.* = .{
            .tag = .string_part,
            .value_tag = .nil,
            .ref_count = 0,
            .data = .{ .string_part = .{ .prev = tail, .data = data } },
        };
        if (tail) |old| {
            old.ref_count += 1; // inbound prev edge from `part`
            old.ref_count -= 1; // the header no longer points at `old`
        }
        part.ref_count += 1; // inbound header.last edge
        header.data.string_header.last = part;
        size += count;
        header.data.string_header.size = size;
        tail = part;
        chunk = chunk[count..];
    }
}

fn absorbPartialTail(self: *Self, header: *HeapObject, bytes: []const u8) error{OutOfMemory}!void {
    const size = header.data.string_header.size;
    const used = if (size % 8 == 0) 8 else size % 8;
    const tail = header.data.string_header.last.?;

    const old_prev = tail.data.string_part.prev;
    const take = @min(8 - used, bytes.len);
    var data = [_]u8{0} ** 8;
    @memcpy(data[0..used], tail.data.string_part.data[0..used]);
    @memcpy(data[used..][0..take], bytes[0..take]);

    const part = try self.heap.create(self.allocator);
    part.* = .{
        .tag = .string_part,
        .value_tag = .nil,
        .ref_count = 1,
        .data = .{ .string_part = .{ .prev = old_prev, .data = data } },
    };

    // The fresh part inherits the tail's left edge and the header's last edge.
    if (old_prev) |prev|
        prev.ref_count += 1;
    tail.ref_count -= 1;

    header.data.string_header.last = part;
    header.data.string_header.size = size + take;

    if (bytes[take..].len != 0)
        try self.appendBytes(header, bytes[take..]);
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
    const result = isEqual(left, right);
    try self.push(if (!result) .true else .false);
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
        .internal_string => |left_internal| switch (right) {
            .internal_string => |right_internal| std.mem.eql(u8, left_internal, right_internal),
            .heap_string => |right_heap| heapInternalStringsEqual(right_heap, left_internal),
            else => false,
        },
        .heap_string => |left_heap| switch (right) {
            .internal_string => |right_internal| heapInternalStringsEqual(left_heap, right_internal),
            .heap_string => |right_heap| heapStringsEqual(left_heap, right_heap),
            else => false,
        },
        .instance => |left_instance| switch (right) {
            .instance => |right_instance| left_instance == right_instance,
            else => false,
        },
        .class => |left_class_object| switch (right) {
            .class => |right_class_object| left_class_object == right_class_object,
            else => false,
        },
        .jump_target => unreachable,
        .function => |left_function| switch (right) {
            .function => |right_function| left_function.data.function.function_index == right_function.data.function.function_index,
            else => false,
        },
    };
}

fn heapStringsEqual(left: *HeapObject, right: *HeapObject) bool {
    if (left == right)
        return true;
    const left_size = left.data.string_header.size;
    if (left_size != right.data.string_header.size)
        return false;

    var left_part: ?*HeapObject = left.data.string_header.last;
    var right_part: ?*HeapObject = right.data.string_header.last;
    var count: usize = if (left_size % 8 == 0) 8 else left_size % 8;
    while (left_part) |lp| {
        const rp = right_part.?;
        if (!std.mem.eql(u8, lp.data.string_part.data[0..count], rp.data.string_part.data[0..count]))
            return false;
        left_part = lp.data.string_part.prev;
        right_part = rp.data.string_part.prev;
        count = 8;
    }
    return true;
}

fn heapInternalStringsEqual(heap: *HeapObject, internal: []const u8) bool {
    const heap_size = heap.data.string_header.size;
    if (heap_size != internal.len)
        return false;

    var pos: usize = internal.len;
    var part: ?*HeapObject = heap.data.string_header.last;
    var count: usize = if (heap_size % 8 == 0) 8 else heap_size % 8;
    while (part) |p| {
        if (!std.mem.eql(u8, p.data.string_part.data[0..count], internal[pos - count .. pos]))
            return false;
        part = p.data.string_part.prev;
        pos -= count;
        count = 8;
    }
    return true;
}

fn box(self: *Self, value: Value) error{OutOfMemory}!*HeapObject {
    return switch (value) {
        .nil => &self.nil_singleton,
        .true => &self.true_singleton,
        .false => &self.false_singleton,
        .clock => &self.clock_singleton,
        .number => |number| blk: {
            const object = try self.heap.create(self.allocator);
            object.* = .{ .tag = .number, .value_tag = .nil, .ref_count = 0, .data = .{ .number = number } };
            break :blk object;
        },
        .internal_string => |string| blk: {
            const object = try self.heap.create(self.allocator);
            errdefer self.heap.destroy(object);
            object.* = .{ .tag = .string_header, .value_tag = .nil, .ref_count = 0, .data = .{ .string_header = .{ .size = 0, .last = null } } };
            errdefer if (object.data.string_header.last) |last|
                self.decrementRef(last);
            try self.appendBytes(object, string);
            break :blk object;
        },
        .heap_string, .function, .instance, .class => |object| object,
        .jump_target => unreachable,
    };
}

fn bindMethod(self: *Self, instance: *HeapObject, stored: *HeapObject) error{OutOfMemory}!void {
    const function_index = stored.data.function.function_index;
    const stored_chain = stored.data.function.capture;

    const this_variable = try self.heap.create(self.allocator);
    errdefer self.heap.destroy(this_variable);
    this_variable.* = .{ .tag = .variable, .value_tag = .nil, .ref_count = 1, .data = .{ .variable = .{ .empty = {} } } };
    this_variable.storeValue(.{ .instance = instance });

    const this_capture = try self.heap.create(self.allocator);
    errdefer self.heap.destroy(this_capture);
    this_capture.* = .{ .tag = .capture, .value_tag = .nil, .ref_count = 0, .data = .{ .capture = .{ .variable = this_variable, .next = null } } };

    var new_capture: ?*HeapObject = this_capture;
    var maybe_stored = stored_chain;
    while (maybe_stored) |capture_node| {
        const variable = capture_node.data.capture.variable;
        variable.ref_count += 1;

        const copied = try self.heap.create(self.allocator);
        errdefer self.heap.destroy(copied);
        copied.* = .{ .tag = .capture, .value_tag = .nil, .ref_count = 0, .data = .{ .capture = .{ .variable = variable, .next = new_capture } } };
        new_capture.?.ref_count += 1;
        new_capture = copied;
        maybe_stored = capture_node.data.capture.next;
    }

    const bound = try self.heap.create(self.allocator);
    errdefer self.heap.destroy(bound);
    bound.* = .{ .tag = .function, .value_tag = .nil, .ref_count = 0, .data = .{ .function = .{ .function_index = function_index, .capture = new_capture } } };
    new_capture.?.ref_count += 1;

    try self.push(.{ .function = bound });
}

fn testBuildBytecode(source: []const u8, root_symbol: Ast.RootSymbol) !Bytecode {
    const allocator = std.testing.allocator;
    const parsing = @import("parsing.zig");
    const gen = @import("gen.zig");

    const ast = try parsing.parse(allocator, source, root_symbol);
    defer ast.deinit(allocator);
    errdefer ast.deinitStrings(allocator);

    return gen.generate(allocator, ast.root().?, root_symbol);
}

fn testEvaluate(source: []const u8, expected: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;

    const bytecode = try testBuildBytecode(source, .expression);
    defer bytecode.deinit(allocator);

    var buffer: [1024]u8 = undefined;
    var w: std.Io.Writer = .fixed(&buffer);
    var runtime = Self.init(allocator, std.testing.io, &w);
    defer runtime.deinit();

    const value = try runtime.evaluate(bytecode.startOp().?);
    defer runtime.free(value);

    const evaluated = try std.fmt.allocPrint(allocator, "{f}", .{value.inContext(&bytecode)});
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

fn testRuntimeError(source: []const u8) !void {
    const result = testEvaluate(source, "");
    try std.testing.expectError(error.Runtime, result);
}

test "runtime errors" {
    try testRuntimeError("-\"foo\"");
    try testRuntimeError("-(\"hello\" + \" world!\")");
    try testRuntimeError("\"foo\" * 42");
    try testRuntimeError("true / 2");
    try testRuntimeError("\"quz\" + 2");
    try testRuntimeError("2 + a");
}

fn testRun(source: []const u8, expected: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;

    var write_state = std.Io.Writer.Allocating.init(allocator);
    defer write_state.deinit();

    try testRunBinary(source, &write_state.writer);

    try testing.expectEqualStrings(expected, write_state.writer.buffered());
}

fn testRunError(source: []const u8, comptime expected_error: anyerror) !void {
    const testing = std.testing;

    var black_hole_buffer: [4096]u8 = undefined;
    var black_hole: std.Io.Writer.Discarding = .init(&black_hole_buffer);

    try testing.expectError(expected_error, testRunBinary(source, &black_hole.writer));
}

fn testRunBinary(source: []const u8, writer: *std.Io.Writer) !void {
    const testing = std.testing;
    const allocator = testing.allocator;

    const bytecode = try testBuildBytecode(source, .program);
    defer bytecode.deinit(allocator);

    var runtime = Self.init(allocator, std.testing.io, writer);
    defer runtime.deinit();

    try runtime.run(bytecode.startOp().?);
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
    try testRunError(
        \\{
        \\  var hello = "outer hello";
        \\  {
        \\    var hello = "inner hello";
        \\    print hello;
        \\  }
        \\  print hello;
        \\}
        \\print hello;
    , error.Runtime);
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
    try testRun(
        \\fun foo(a) { print a; }
        \\foo(10);
    ,
        \\10
        \\
    );
    try testRun(
        \\fun calculateGrade(score, bonus) {
        \\  var finalScore = score + bonus;
        \\
        \\  if (finalScore >= 90) {
        \\    print "A";
        \\  } else if (finalScore >= 80) {
        \\    print "B";
        \\  } else if (finalScore >= 70) {
        \\    print "C";
        \\  } else if (finalScore >= 60) {
        \\    print "D";
        \\  } else {
        \\    print "F";
        \\  }
        \\}
        \\
        \\var score = 81;
        \\var bonus = 3;
        \\print "Grade for given score is: ";
        \\calculateGrade(score, bonus);
    ,
        \\Grade for given score is: 
        \\B
        \\
    );
    try testRun(
        \\fun fib(n) {
        \\  if (n < 2) return n;
        \\  return fib(n - 2) + fib(n - 1);
        \\}
        \\
        \\var start = clock();
        \\print fib(20) == 6765;
        \\print (clock() - start) < 5; // 5 seconds
        \\
    ,
        \\true
        \\true
        \\
    );
    try testRun(
        \\var globalGreeting = "Hello";
        \\
        \\fun makeGreeter() {
        \\  fun greet(name) {
        \\    print globalGreeting + " " + name;
        \\  }
        \\  return greet;
        \\}
        \\
        \\var sayHello = makeGreeter();
        \\sayHello("Bob");
    ,
        \\Hello Bob
        \\
    );
    try testRun(
        \\fun returnArg(arg) {
        \\  return arg;
        \\}
        \\
        \\fun returnFunCallWithArg(func, arg) {
        \\  return returnArg(func)(arg);
        \\}
        \\
        \\fun printArg(arg) {
        \\  print arg;
        \\}
        \\
        \\returnFunCallWithArg(printArg, "foo");
    ,
        \\foo
        \\
    );
}

test "declaration semantics" {
    // Re-declaring a global is allowed (jlox dynamic globals).
    try testRun(
        \\var a = "value";
        \\var a = a;
        \\print a;
    ,
        \\value
        \\
    );
    // A local variable may not reference itself in its own initializer.
    try testRunError(
        \\var a = "outer";
        \\{ var a = a; }
    ,
        error.Semantics,
    );
    try testRunError(
        \\fun returnArg(arg) { return arg; }
        \\var b = "global";
        \\{
        \\    var a = "first";
        \\    var b = returnArg(b);
        \\    print b;
        \\}
        \\var b = b + " updated";
    ,
        error.Semantics,
    );
    try testRunError(
        \\fun outer() {
        \\    var a = "outer";
        \\    fun inner() {
        \\        var a = a;
        \\        print a;
        \\    }
        \\    inner();
        \\}
        \\outer();
    ,
        error.Semantics,
    );
}

test "misplaced return statements" {
    // A return statement inside a function is fine; at the top level it is not.
    try testRunError(
        \\fun foo() {
        \\  if (true) {
        \\    return "early return";
        \\  }
        \\
        \\  for (var i = 0; i < 10; i = i + 1) {
        \\    return "loop return";
        \\  }
        \\}
        \\
        \\if (true) {
        \\  return "conditional return";
        \\}
    ,
        error.Semantics,
    );
    try testRunError(
        \\{
        \\  return "not allowed in a block either";
        \\}
    ,
        error.Semantics,
    );
    try testRunError(
        \\fun allowed() {
        \\  if (true) {
        \\    return "this is fine";
        \\  }
        \\  return;
        \\}
        \\
        \\
        \\fun outer() {
        \\  fun inner() {
        \\    return "ok";
        \\  }
        \\
        \\  return "also ok";
        \\}
        \\
        \\if (true) {
        \\  fun nested() {
        \\    return;
        \\  }
        \\
        \\  return "not ok";
        \\}
    ,
        error.Semantics,
    );
}

test "class declarations" {
    try testRun(
        \\class Robot {}
        \\class Wizard {}
        \\print Robot;
        \\print Wizard;
        \\print "Both classes successfully printed";
    ,
        \\Robot
        \\Wizard
        \\Both classes successfully printed
        \\
    );
    try testRun(
        \\class Robot {}
        \\print Robot == Robot;
        \\var c = Robot;
        \\print c;
    ,
        \\true
        \\Robot
        \\
    );
}

test "class instances" {
    try testRun(
        \\class Robot {}
        \\var r1 = Robot();
        \\var r2 = Robot();
        \\print r1;
        \\print r2;
    ,
        \\Robot instance
        \\Robot instance
        \\
    );
    try testRun(
        \\class Robot {}
        \\class Wizard {}
        \\var r = Robot();
        \\var w = Wizard();
        \\print r;
        \\print w;
    ,
        \\Robot instance
        \\Wizard instance
        \\
    );
    try testRun(
        \\class Robot {}
        \\var c = Robot;
        \\var r = c();
        \\print r;
    ,
        \\Robot instance
        \\
    );
    try testRun(
        \\class Robot {}
        \\var a = Robot();
        \\var b = a;
        \\print a == b;
    ,
        \\true
        \\
    );
    try testRun(
        \\class Robot {}
        \\print Robot() == Robot();
    ,
        \\false
        \\
    );
    try testRun(
        \\class Robot {}
        \\var r = Robot();
        \\r = Robot();
        \\print r;
    ,
        \\Robot instance
        \\
    );
    try testRun(
        \\class Robot {}
        \\if (Robot()) print "magic";
        \\if (!Robot()) print "not magic";
        \\if (nil) print "unreachable";
    ,
        \\magic
        \\
    );
    try testRun(
        \\class Robot {}
        \\{
        \\    var r = Robot();
        \\    print r;
        \\}
        \\print "scope exited";
    ,
        \\Robot instance
        \\scope exited
        \\
    );
    try testRunError("class Robot {}\nRobot(1);", error.Runtime);
}

test "constructors" {
    try testRun(
        \\class Default { init() { this.x = "bar"; this.y = 91; } }
        \\print Default().x;
        \\print Default().y;
    ,
        \\bar
        \\91
        \\
    );
    try testRun(
        \\class Pair { init(a, b) { this.a = a; this.b = b; } }
        \\var p = Pair(1, 2);
        \\print p.a;
        \\print p.b;
        \\print Pair(3, 4).a + Pair(5, 6).b;
    ,
        \\1
        \\2
        \\9
        \\
    );
    try testRun(
        \\class Foo { init() { this.v = "ok"; return; } }
        \\print Foo().v;
    ,
        \\ok
        \\
    );
    try testRun(
        \\class Foo { init() { return nil; } }
        \\print Foo();
    ,
        \\Foo instance
        \\
    );
    try testRun(
        \\class Foo { init() { fun helper() { return 7; } this.v = helper(); } }
        \\print Foo().v;
    ,
        \\7
        \\
    );
    try testRun(
        \\class Maker {
        \\  init(name) {
        \\    var prefix = "> ";
        \\    fun greet() { return prefix + name; }
        \\    this.greet = greet;
        \\  }
        \\}
        \\print Maker("Bob").greet();
    ,
        \\> Bob
        \\
    );
    try testRun(
        \\class Counter {
        \\  init(start) { this.count = start; }
        \\  add(amount) { this.count = this.count + amount; }
        \\}
        \\var c = Counter(5);
        \\c.add(3);
        \\print c.count;
    ,
        \\8
        \\
    );
    try testRun(
        \\class Pair { init(a, b) { this.sum = a + b; } }
        \\class Box { init(inner) { this.inner = inner; } }
        \\var box = Box(Pair(1, 2));
        \\print box.inner.sum;
    ,
        \\3
        \\
    );
    try testRun(
        \\class Foo { init(n) { if (n > 0) Foo(n - 1); this.n = n; } }
        \\print Foo(3).n;
    ,
        \\3
        \\
    );
    try testRun(
        \\class Robot { init(id) { this.id = id; } }
        \\var c = Robot;
        \\var r = c(42);
        \\print r.id;
    ,
        \\42
        \\
    );
    try testRun(
        \\class Counter {
        \\  init(start) { if (start < 0) this.count = 0; else this.count = start; }
        \\}
        \\var instance = Counter(5);
        \\print instance.init(28).count;
        \\print instance.count;
    ,
        \\28
        \\28
        \\
    );
    try testRunError(
        \\class Foo { init() { return 10; } }
    ,
        error.Semantics,
    );
    try testRunError(
        \\class Foo { init() { if (true) return 5; } }
    ,
        error.Semantics,
    );
    try testRunError(
        \\class Foo { init() { return this; } }
    ,
        error.Semantics,
    );
    try testRunError(
        \\class Foo { init(a) {} }
        \\Foo();
    ,
        error.Runtime,
    );
    try testRunError(
        \\class Foo { init(a) {} }
        \\Foo(1, 2);
    ,
        error.Runtime,
    );
}

test "property access" {
    try testRun(
        \\class Spaceship {}
        \\var falcon = Spaceship();
        \\
        \\falcon.name = "Millennium Falcon";
        \\falcon.speed = 75.5;
        \\
        \\print "Ship details:";
        \\print falcon.name;
        \\print falcon.speed;
    ,
        \\Ship details:
        \\Millennium Falcon
        \\75.5
        \\
    );
    try testRun(
        \\class Robot {}
        \\var a = Robot();
        \\a.speed = 10;
        \\a.speed = a.speed + 20;
        \\print a.speed;
        \\var b = Robot();
        \\b.speed = 20;
        \\print a.speed == b.speed;
    ,
        \\30
        \\false
        \\
    );
    try testRun(
        \\class Robot {}
        \\var a = Robot();
        \\var b = Robot();
        \\a.copilot = b;
        \\b.name = "R2";
        \\print a.copilot.name;
    ,
        \\R2
        \\
    );
    try testRunError(
        \\class Robot {}
        \\var a = Robot();
        \\print a.unknown;
    ,
        error.Runtime,
    );
    try testRunError(
        \\var x = 5;
        \\x.foo = 1;
    ,
        error.Runtime,
    );
}

test "instance methods" {
    try testRun(
        \\class Robot {
        \\  beep() {
        \\    print "Beep boop!";
        \\  }
        \\}
        \\
        \\var r2d2 = Robot();
        \\r2d2.beep();
        \\
        \\Robot().beep();
    ,
        \\Beep boop!
        \\Beep boop!
        \\
    );
    try testRun(
        \\class Point {
        \\  initialize(x, y) {
        \\    this.x = x;
        \\    this.y = y;
        \\  }
        \\  sum() {
        \\    return this.x + this.y;
        \\  }
        \\}
        \\var p = Point();
        \\p.initialize(3, 4);
        \\print p.sum();
        \\var q = Point();
        \\q.initialize(10, 20);
        \\print q.sum();
        \\print p.sum();
    ,
        \\7
        \\30
        \\7
        \\
    );
    try testRun(
        \\class Robot {
        \\  beep() {
        \\    print "Beep boop!";
        \\  }
        \\}
        \\var r2d2 = Robot();
        \\var beep = r2d2.beep;
        \\beep();
        \\var still = r2d2.beep;
        \\print beep == still;
    ,
        \\Beep boop!
        \\true
        \\
    );
    try testRun(
        \\class Calc {
        \\  add(a, b) {
        \\    return a + b;
        \\  }
        \\}
        \\var c = Calc();
        \\print c.add(2, 3);
    ,
        \\5
        \\
    );
    try testRun(
        \\class Foo {
        \\  returnSelf() {
        \\    return Foo;
        \\  }
        \\}
        \\print Foo().returnSelf();
    ,
        \\Foo
        \\
    );
    try testRun(
        \\var owner = "droid";
        \\class Robot {
        \\  describe() {
        \\    print owner;
        \\  }
        \\}
        \\Robot().describe();
        \\owner = "planet";
        \\Robot().describe();
    ,
        \\droid
        \\planet
        \\
    );
    try testRun(
        \\fun makeRobot() {
        \\  var owner = "droid";
        \\  class Robot {
        \\    describe() {
        \\      print owner;
        \\    }
        \\  }
        \\  return Robot;
        \\}
        \\var r = makeRobot();
        \\r().describe();
    ,
        \\droid
        \\
    );
    try testRun(
        \\var a = "alpha";
        \\var b = "beta";
        \\class Pair {
        \\  first() {
        \\    print a;
        \\  }
        \\  second() {
        \\    print b;
        \\  }
        \\}
        \\var p = Pair();
        \\p.first();
        \\p.second();
    ,
        \\alpha
        \\beta
        \\
    );
    try testRun(
        \\var wrap = "[";
        \\class Wrapper {
        \\  wrapValue(v) {
        \\    return wrap + v + "]";
        \\  }
        \\}
        \\print Wrapper().wrapValue("hi");
    ,
        \\[hi]
        \\
    );
    try testRunError(
        \\class Robot {}
        \\var r = Robot();
        \\r.beep();
    ,
        error.Runtime,
    );
    try testRunError(
        \\class Robot {
        \\  beep() {
        \\    print "Beep boop!";
        \\  }
        \\}
        \\var r = Robot();
        \\r.beep(1);
    ,
        error.Runtime,
    );
    try testRunError(
        \\print this;
    ,
        error.Semantics,
    );
    try testRun(
        \\class Animal {
        \\  makeSound() {
        \\    print this.sound;
        \\  }
        \\
        \\  identify() {
        \\    print this.species;
        \\  }
        \\}
        \\
        \\var dog = Animal();
        \\dog.sound = "Woof";
        \\dog.species = "Dog";
        \\
        \\var cat = Animal();
        \\cat.sound = "Meow";
        \\cat.species = "Cat";
        \\
        \\// The this keyword should be bound to the
        \\// class instance that the method is called on
        \\cat.makeSound = dog.makeSound;
        \\dog.identify = cat.identify;
        \\
        \\cat.makeSound(); // expect: Woof
        \\dog.identify(); // expect: Cat
    ,
        \\Woof
        \\Cat
        \\
    );
}
