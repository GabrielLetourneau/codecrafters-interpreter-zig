const std = @import("std");
const Allocator = std.mem.Allocator;

const Bytecode = @import("Bytecode.zig");
const Ast = @import("Ast.zig");

pub fn generate(allocator: Allocator, root: Ast.Node, run_mode: Ast.RootSymbol) !Bytecode {
    var generator: Generator = .{
        .allocator = allocator,
    };
    defer {
        generator.returns.deinit(allocator);
        generator.captures.deinit(allocator);
        generator.frame_variables.deinit(allocator);
    }
    errdefer {
        generator.function_names_list.deinit(allocator);
        generator.function_defs_list.deinit(allocator);
        generator.data_list.deinit(allocator);
        generator.ops_list.deinit(allocator);
    }

    switch (run_mode) {
        .expression => try generator.expression(root),
        .program => try generator.program(root),
    }

    const ops = try generator.ops_list.toOwnedSlice(allocator);
    errdefer allocator.free(ops);

    const data = try generator.data_list.toOwnedSlice(allocator);
    errdefer allocator.free(data);

    const function_defs = try generator.function_defs_list.toOwnedSlice(allocator);
    errdefer allocator.free(function_defs);

    const function_names = try generator.function_names_list.toOwnedSlice(allocator);
    errdefer allocator.free(function_names);

    return .{
        .ops = ops,
        .data = data,
        .function_defs = function_defs,
        .function_names = function_names,
        .string_starts = root.ast.string_starts,
        .strings = root.ast.strings,
    };
}

const Generator = struct {
    allocator: Allocator,

    ops_list: std.ArrayListUnmanaged(Bytecode.OpCode) = .{},
    data_list: std.ArrayListUnmanaged(Bytecode.Data) = .{},

    function_defs_list: std.ArrayListUnmanaged(Bytecode.FunctionDefinition) = .{},
    function_names_list: std.ArrayListUnmanaged(usize) = .{},

    frame_variables: std.ArrayListUnmanaged(usize) = .{},
    captures: std.ArrayListUnmanaged(usize) = .{},
    returns: std.ArrayListUnmanaged(usize) = .{},

    block_base: usize = 0,
    function_base: struct {
        variable_base: usize,
        capture_base: usize,
        return_base: usize,
    } = .{
        .variable_base = 0,
        .capture_base = 0,
        .return_base = 0,
    },

    const Self = @This();

    fn program(self: *Self, root: Ast.Node) !void {
        try self.block(root);
    }

    fn statement(self: *Self, node: Ast.Node) !void {
        switch (node.tag()) {
            .empty => {},
            .print => {
                try self.expression(node.onlyChild());
                try self.addEmpty(.print);
            },
            .@"return" => {
                try self.expression(node.onlyChild());
                const return_op_index = self.nextOpIndex();
                const frame_height = self.frame_variables.items.len - self.function_base.variable_base;
                try self.addIndexed(.@"return", frame_height);
                try self.returns.append(self.allocator, return_op_index);
            },
            .block => try self.block(node.onlyChild()),

            .var_decl => {
                try self.addEmpty(.nil);
                if (try self.getOrPutVariable(node.identifier())) |variable| {
                    try self.addIndexed(.assign, variable);
                    try self.addEmpty(.discard);
                } else try self.addEmpty(.alloc);
            },
            .parameter => {
                try self.statement(node.onlyChild());
                _ = try self.getOrPutVariable(node.identifier());
                self.function_defs_list.items[self.function_defs_list.items.len - 1].param_count += 1;
            },
            .var_decl_init => {
                try self.expression(node.onlyChild());
                if (try self.getOrPutVariable(node.identifier())) |variable| {
                    try self.addIndexed(.assign, variable);
                    try self.addEmpty(.discard);
                } else try self.addEmpty(.alloc);
            },

            .fun_decl => {
                const jump_op_index = try self.addForwardBranch(.branch_uncond);

                const maybe_variable = try self.getOrPutVariable(node.identifier());

                const old_function_base = self.function_base;
                self.function_base = .{
                    .variable_base = self.frame_variables.items.len,
                    .capture_base = self.captures.items.len,
                    .return_base = self.returns.items.len,
                };
                const new_function_base = self.function_base;

                const fun_start_index = self.nextOpIndex();

                const function_index = self.function_defs_list.items.len;
                try self.function_defs_list.append(self.allocator, .{
                    .op_index = fun_start_index,
                    .param_count = 0,
                });
                try self.function_names_list.append(self.allocator, node.identifier());

                try self.block(node.onlyChild());
                try self.addEmpty(.nil);
                const final_return_index = self.nextOpIndex();
                try self.addIndexed(.@"return", 0);

                self.setBranchTargetHere(jump_op_index);

                try self.addIndexed(.def_fun, function_index);
                if (maybe_variable) |variable| {
                    try self.addIndexed(.assign, variable);
                } else {
                    try self.addEmpty(.alloc);
                    try self.addIndexed(.variable, 1);
                }

                self.function_base = old_function_base;

                const capture_count = self.captures.items.len - new_function_base.capture_base;

                var capture_top = new_function_base.capture_base;
                for (new_function_base.capture_base..self.captures.items.len) |capture_index| {
                    const captured_variable = self.captures.items[capture_index];
                    const variable = try self.getOrCaptureVariable(captured_variable, &capture_top);
                    try self.addIndexed(.capture, variable);
                }
                self.captures.shrinkRetainingCapacity(capture_top);

                for (new_function_base.return_base..self.returns.items.len) |return_index| {
                    const return_op_index = self.returns.items[return_index];
                    self.data_list.items[return_op_index].index += capture_count;
                }
                self.data_list.items[final_return_index].index = capture_count;
                self.returns.shrinkRetainingCapacity(new_function_base.return_base);

                try self.addIndexed(.assign, maybe_variable orelse 1);
                try self.addEmpty(.discard);
            },

            .declarations => {
                try self.statement(node.leftChild());
                try self.statement(node.rightChild());
            },
            .fun_def => {
                try self.statement(node.leftChild());
                try self.statement(node.rightChild().onlyChild());
            },
            .@"if" => {
                try self.expression(node.leftChild());

                const branch_op_index = try self.addForwardBranch(.branch_cond_not);

                try self.block(node.rightChild());

                self.setBranchTargetHere(branch_op_index);
            },
            .@"else" => {
                const if_node = node.leftChild();

                try self.expression(if_node.leftChild());

                const if_branch_op_index = try self.addForwardBranch(.branch_cond_not);

                try self.block(if_node.rightChild());

                const else_branch_op_index = try self.addForwardBranch(.branch_uncond);

                self.setBranchTargetHere(if_branch_op_index);

                try self.block(node.rightChild());

                self.setBranchTargetHere(else_branch_op_index);
            },
            .@"while" => {
                const cond_target = self.nextOpIndex();
                try self.expression(node.leftChild());

                const branch_op_index = try self.addForwardBranch(.branch_cond_not);

                try self.block(node.rightChild());

                try self.addIndexed(.branch_uncond, cond_target);

                self.setBranchTargetHere(branch_op_index);
            },
            .@"for" => {
                const preamble_node = node.leftChild();
                const cond_init_node = preamble_node.leftChild();

                try self.statement(cond_init_node.leftChild());

                const cond_target = self.nextOpIndex();
                const cond_node = cond_init_node.rightChild();
                const maybe_branch_op_index: ?usize =
                    if (cond_node.tag() == .empty) null else blk: {
                        try self.expression(cond_node);
                        break :blk try self.addForwardBranch(.branch_cond_not);
                    };

                try self.block(node.rightChild());
                try self.statement(preamble_node.rightChild());

                try self.addIndexed(.branch_uncond, cond_target);

                if (maybe_branch_op_index) |branch_op_index| {
                    self.setBranchTargetHere(branch_op_index);
                }
            },

            else => {
                try self.expression(node);
                try self.addEmpty(.discard);
            },
        }
    }

    fn block(self: *Self, node: Ast.Node) error{OutOfMemory}!void {
        const old_frame_base = self.block_base;
        self.block_base = self.frame_variables.items.len;

        try self.statement(node);

        const variable_count = self.frame_variables.items.len - self.block_base;
        if (variable_count > 0) {
            try self.addIndexed(.free_frame, variable_count);
            self.frame_variables.shrinkRetainingCapacity(self.block_base);
        }

        self.block_base = old_frame_base;
    }

    fn expression(self: *Self, node: Ast.Node) error{OutOfMemory}!void {
        switch (node.tag()) {
            .nil => try self.addEmpty(.nil),
            .true => try self.addEmpty(.true),
            .false => try self.addEmpty(.false),
            .empty => {},

            .group => try self.expression(node.onlyChild()),
            .not => {
                try self.expression(node.onlyChild());
                try self.addEmpty(.not);
            },
            .unary_minus => {
                try self.expression(node.onlyChild());
                try self.addEmpty(.unary_minus);
            },

            .number => try self.addData(.number, .{ .number = node.number() }),
            .string => try self.addIndexed(.string, node.stringIndex()),
            .variable => {
                const identifier = node.identifier();
                if (identifier == 0) { // clock
                    try self.addEmpty(.clock);
                } else if (self.findVariableIndex(node.identifier(), 0)) |variable_index| {
                    const variable = try self.getOrCaptureVariable(variable_index, &self.captures.items.len);
                    try self.addIndexed(.variable, variable);
                } else {
                    try self.addEmpty(.undefined);
                }
            },

            .assignment => {
                try self.expression(node.onlyChild());

                if (self.findVariableIndex(node.identifier(), 0)) |variable_index| {
                    const variable = try self.getOrCaptureVariable(variable_index, &self.captures.items.len);
                    try self.addIndexed(.assign, variable);
                } else {
                    try self.addEmpty(.discard);
                    try self.addEmpty(.undefined);
                }
            },

            .call => {
                var arguments = node.rightChild();
                var arguments_count: usize = 0;
                while (arguments.tag() != .empty) : (arguments = arguments.leftChild()) {
                    try self.expression(arguments.rightChild());
                    arguments_count += 1;
                }

                try self.expression(node.leftChild());

                try self.addIndexed(.call, arguments_count);
            },

            .multiply => try self.binary(node, .multiply),
            .divide => try self.binary(node, .divide),
            .add => try self.binary(node, .add),
            .substract => try self.binary(node, .substract),
            .greater => try self.binary(node, .greater),
            .greater_equal => try self.binary(node, .greater_equal),
            .less => try self.binary(node, .less),
            .less_equal => try self.binary(node, .less_equal),
            .equal => try self.binary(node, .equal),
            .not_equal => try self.binary(node, .not_equal),

            .@"or" => {
                try self.expression(node.leftChild());

                const branch_op_index = try self.addForwardBranch(.@"or");

                try self.expression(node.rightChild());

                self.setBranchTargetHere(branch_op_index);
            },
            .@"and" => {
                try self.expression(node.leftChild());

                const branch_op_index = try self.addForwardBranch(.@"and");

                try self.expression(node.rightChild());

                self.setBranchTargetHere(branch_op_index);
            },

            else => unreachable,
        }
    }

    fn binary(self: *Self, node: Ast.Node, op: Bytecode.OpCode) !void {
        try self.expression(node.leftChild());
        try self.expression(node.rightChild());
        try self.addEmpty(op);
    }

    fn nextOpIndex(self: Self) usize {
        return self.ops_list.items.len;
    }

    fn addEmpty(self: *Self, op: Bytecode.OpCode) !void {
        try self.addData(op, .{ .empty = {} });
    }

    fn addIndexed(self: *Self, op: Bytecode.OpCode, index: usize) !void {
        try self.addData(op, .{ .index = index });
    }

    fn addForwardBranch(self: *Self, op: Bytecode.OpCode) !usize {
        const index = self.data_list.items.len;
        try self.addData(op, undefined);
        return index;
    }

    fn setBranchTargetHere(self: *Self, branch_op_index: usize) void {
        self.data_list.items[branch_op_index] = .{ .index = self.nextOpIndex() };
    }

    fn addData(self: *Self, op: Bytecode.OpCode, data: Bytecode.Data) !void {
        try self.ops_list.append(self.allocator, op);
        try self.data_list.append(self.allocator, data);
    }

    fn getOrPutVariable(self: *Self, identifier: usize) !?usize {
        if (self.findVariableIndex(identifier, self.block_base)) |variable|
            return self.frame_variables.items.len - variable;

        try self.frame_variables.append(self.allocator, identifier);
        return null;
    }

    fn getOrCaptureVariable(self: *Self, variable_index: usize, capture_top_ptr: *usize) !usize {
        if (variable_index >= self.function_base.variable_base)
            return self.frame_variables.items.len - variable_index;

        const local_frame_height = self.frame_variables.items.len - self.function_base.variable_base;

        const capture_top = capture_top_ptr.*;
        for (self.function_base.capture_base..capture_top) |capture_index|
            if (self.captures.items[capture_index] == variable_index)
                return capture_index + 1 - self.function_base.capture_base + local_frame_height;

        if (capture_top == self.captures.items.len) {
            try self.captures.append(self.allocator, variable_index);
        } else {
            self.captures.items[capture_top] = variable_index;
            capture_top_ptr.* += 1;
        }

        return capture_top + 1 - self.function_base.capture_base + local_frame_height;
    }

    fn findVariableIndex(self: Self, identifier: usize, base_index: usize) ?usize {
        var variable_index = self.frame_variables.items.len;
        while (variable_index > base_index) {
            variable_index -= 1;
            if (self.frame_variables.items[variable_index] == identifier)
                return variable_index;
        } else return null;
    }
};

fn testParse(source: []const u8, parsed: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;

    const ast = try generate(allocator, source, .expression);
    defer ast.deinit(allocator);

    if (ast.root()) |node| {
        const actual = try std.fmt.allocPrint(allocator, "{s}", .{node});
        defer allocator.free(actual);

        try testing.expectEqualStrings(parsed, actual);
    } else try testing.expect(false);
}
