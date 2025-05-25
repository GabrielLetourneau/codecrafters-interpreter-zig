const std = @import("std");
const assert = std.debug.assert;

pub const OpCode = enum(u8) {
    // No effect on evaluation stack
    free_frame,
    branch_uncond,

    // Pushes value onto stack
    nil,
    true,
    false,
    undefined,
    number,
    string,

    variable,
    clock,
    def_fun,

    // Unary operation; pops one value, pushes one
    not,
    unary_minus,
    assign,
    capture,

    // Logical operator; pops one value, may push it back
    @"or",
    @"and",

    // Consuming operation; pops one value, pushes none
    alloc,
    discard,
    print,
    branch_cond_not,
    call,

    // Binary operation; pops two values, pushes one
    multiply = 0x30,
    divide,
    add,
    substract,
    greater,
    greater_equal,
    less,
    less_equal,
    equal,
    not_equal,
    @"return",
};

pub const Data = union {
    empty: void,
    index: usize, // index to ops, or string_starts (string literal); or identifier; or frame size
    number: f64,
};

pub const FunctionDefinition = struct {
    op_index: usize,
    param_count: usize,
};

ops: []const OpCode, // op codes, program starts at 0
data: []const Data, // relevant data in same order as ops
string_starts: []const usize, // pointer to start of string; end of string is following entry
strings: []const u8, // internalized strings
function_defs: []const FunctionDefinition, // function definitions
function_names: []const usize, // indexes of function name string starts

const Bytecode = @This();

pub fn deinit(self: Bytecode, allocator: std.mem.Allocator) void {
    allocator.free(self.function_names);
    allocator.free(self.function_defs);
    allocator.free(self.strings);
    allocator.free(self.string_starts);
    allocator.free(self.data);
    allocator.free(self.ops);
}

pub fn format(bytecode: Bytecode, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
    for (0..bytecode.ops.len) |op_index| {
        const inst: Instruction = .{ .bytecode = &bytecode, .op_index = op_index };
        try writer.print("{any}\n", .{inst});
    }
}

pub const Instruction = struct {
    bytecode: *const Bytecode,
    op_index: usize,

    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("{d}: {s}", .{ self.op_index, @tagName(self.op()) });
        switch (self.op()) {
            .free_frame, .branch_uncond, .variable, .assign, .capture, .@"or", .@"and", .branch_cond_not, .call, .@"return" => try writer.print(" {d}", .{self.index()}),
            .number => try writer.print(" {d}", .{self.number()}),
            .string => try writer.print(" {s}", .{self.string()}),
            .def_fun => {
                const function_def = self.function();
                try writer.print(" {s} {d} {d}", .{
                    self.string(),
                    function_def.param_count,
                    function_def.op_index,
                });
            },
            else => {},
        }
    }

    pub fn op(self: Self) OpCode {
        return self.bytecode.ops[self.op_index];
    }

    pub fn size(self: Self) usize {
        assert(switch (self.op()) {
            .free_frame, .call, .@"return" => true,
            else => false,
        });

        return self.index();
    }

    pub fn variable(self: Self) usize {
        assert(switch (self.op()) {
            .variable, .assign, .capture => true,
            else => false,
        });

        return self.index();
    }

    pub fn number(self: Self) f64 {
        assert(self.op() == .number);

        return self.data().number;
    }

    pub fn string(self: Self) []const u8 {
        const start_index = switch (self.op()) {
            .string => self.index(),
            .def_fun => self.bytecode.function_names[self.index()],
            else => unreachable,
        };

        return self.bytecode.stringAtIndex(start_index);
    }

    pub fn next(self: Self) Instruction {
        return .{
            .bytecode = self.bytecode,
            .op_index = self.op_index + 1,
        };
    }

    pub fn target(self: Self) Instruction {
        assert(switch (self.op()) {
            .branch_uncond, .@"or", .@"and", .branch_cond_not => true,
            else => false,
        });

        return .{
            .bytecode = self.bytecode,
            .op_index = self.index(),
        };
    }

    pub fn function(self: Self) FunctionDefinition {
        assert(self.op() == .def_fun);

        return self.bytecode.function_defs[self.index()];
    }

    pub fn functionIndex(self: Self) usize {
        assert(self.op() == .def_fun);

        return self.index();
    }

    pub fn finished(self: Self) bool {
        return self.op_index >= self.bytecode.ops.len;
    }

    fn index(self: Self) usize {
        return self.data().index;
    }

    fn data(self: Self) Data {
        return self.bytecode.data[self.op_index];
    }
};

pub fn startOp(self: *const Bytecode) ?Instruction {
    if (self.ops.len == 0) return null;

    return .{
        .bytecode = self,
        .op_index = 0,
    };
}

pub fn stringAtIndex(self: *const Bytecode, start_index: usize) []const u8 {
    const start = self.string_starts[start_index];
    const end = self.string_starts[start_index + 1];
    return self.strings[start..end];
}
