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

    // Unary operation; pops one value, pushes one
    not,
    unary_minus,
    assign,
    def_fun,

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

ops: []const OpCode, // op codes, program starts at 0
data: []const Data, // relevant data in same order as ops
string_starts: []const usize, // pointer to start of string; end of string is following entry
strings: []const u8, // internalized strings

const Bytecode = @This();

pub fn deinit(self: Bytecode, allocator: std.mem.Allocator) void {
    allocator.free(self.strings);
    allocator.free(self.string_starts);
    allocator.free(self.data);
    allocator.free(self.ops);
}

pub const Instruction = struct {
    bytecode: *const Bytecode,
    op_index: usize,

    const Self = @This();

    pub fn op(self: Self) OpCode {
        return self.bytecode.ops[self.op_index];
    }

    pub fn size(self: Self) usize {
        assert(switch (self.op()) {
            .free_frame, .call => true,
            else => false,
        });

        return self.index();
    }

    pub fn variable(self: Self) usize {
        assert(switch (self.op()) {
            .assign, .variable => true,
            else => false,
        });

        return self.index();
    }

    pub fn number(self: Self) f64 {
        assert(self.op() == .number);

        return self.data().number;
    }

    pub fn string(self: Self) []const u8 {
        assert(self.op() == .string);

        const start_index = self.index();
        const start = self.bytecode.string_starts[start_index];
        const end = self.bytecode.string_starts[start_index + 1];
        return self.bytecode.strings[start..end];
    }

    pub fn next(self: Self) Instruction {
        return .{
            .bytecode = self.bytecode,
            .op_index = self.op_index + 1,
        };
    }

    pub fn target(self: Self) Instruction {
        assert(switch (self.op()) {
            .branch_uncond, .def_fun, .@"or", .@"and", .branch_cond_not => true,
            else => false,
        });

        return .{
            .bytecode = self.bytecode,
            .op_index = self.index(),
        };
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
