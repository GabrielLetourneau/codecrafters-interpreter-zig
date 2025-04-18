const std = @import("std");
const Allocator = std.mem.Allocator;

const Ast = @import("Ast.zig");
const Scanner = @import("Scanner.zig");

pub const RootSymbol = enum { program, expression };

pub fn parse(allocator: Allocator, file_contents: []const u8, root_symbol: RootSymbol) !Ast {
    var parser: Parser = .{
        .scanner = .{ .source = file_contents },
        .allocator = allocator,
    };
    defer {
        parser.frame_variables.deinit(allocator);
        parser.identifiers.deinit(allocator);
        parser.start_index_of_strings.deinit(allocator);
    }
    errdefer {
        parser.string_buffer.deinit(allocator);
        parser.string_indexes_list.deinit(allocator);
        parser.data_list.deinit(allocator);
        parser.tags_list.deinit(allocator);
    }

    try parser.string_indexes_list.append(allocator, 0);

    switch (root_symbol) {
        .program => try parser.program(),
        .expression => try parser.expression(),
    }

    const tags = try parser.tags_list.toOwnedSlice(allocator);
    errdefer allocator.free(tags);

    const data = try parser.data_list.toOwnedSlice(allocator);
    errdefer allocator.free(data);

    const string_starts = try parser.string_indexes_list.toOwnedSlice(allocator);
    errdefer allocator.free(string_starts);

    const strings = try parser.string_buffer.toOwnedSlice(allocator);

    return .{
        .tags = tags,
        .data = data,
        .string_starts = string_starts,
        .strings = strings,
    };
}

const Parser = struct {
    scanner: Scanner,
    allocator: Allocator,

    tags_list: std.ArrayListUnmanaged(Ast.NodeTag) = .{},
    data_list: std.ArrayListUnmanaged(Ast.Data) = .{},

    string_indexes_list: std.ArrayListUnmanaged(usize) = .{},
    string_buffer: std.ArrayListUnmanaged(u8) = .{},
    start_index_of_strings: std.StringHashMapUnmanaged(usize) = std.StringHashMapUnmanaged(usize).empty,

    identifiers: std.StringArrayHashMapUnmanaged(void) = std.StringArrayHashMapUnmanaged(void).empty,
    frame_variables: std.ArrayListUnmanaged(usize) = .{},
    current_frame_base: usize = 0,

    next_token: ?Scanner.Token = null,

    const Self = @This();

    fn program(self: *Self) !void {

        // global variable frame
        try self.addData(.alloc_frame, .{ .index = 0 });

        while (self.nextToken()) |_| {
            try self.declaration();
        }

        // global frame variable count
        self.data_list.items[0] = .{ .index = self.frame_variables.items.len };
    }

    fn declaration(self: *Self) error{ OutOfMemory, Syntax }!void {
        if (self.match(.@"var")) |_| {
            const identifier = self.match(.identifier) orelse return error.Syntax;
            const identifier_index_result = try self.identifiers.getOrPut(self.allocator, identifier.lexeme);
            const identifier_index = identifier_index_result.index;

            var variable_index: usize = undefined;
            if (self.findVariableIndex(identifier_index, self.current_frame_base)) |index| {
                variable_index = index;
            } else {
                try self.frame_variables.append(self.allocator, identifier_index);
                variable_index = self.frame_variables.items.len - 1;
            }

            if (self.match(.equal)) |_| {
                try self.expression();
                if (self.match(.semicolon) == null) return error.Syntax;
                try self.addData(.var_decl_init, .{ .index = variable_index });
            } else {
                if (self.match(.semicolon) == null) return error.Syntax;
                try self.addData(.var_decl, .{ .index = variable_index });
            }
        } else try self.statement();
    }

    fn statement(self: *Self) !void {
        if (self.match(.left_brace)) |_| { // block
            const old_frame_base = self.current_frame_base;
            self.current_frame_base = self.frame_variables.items.len;

            const alloc_frame_op_index = self.tags_list.items.len;
            try self.addData(.alloc_frame, undefined); // push new frame, full size will be found later

            while (self.match(.right_brace) == null)
                try self.declaration();

            const frame_size = self.frame_variables.items.len - self.current_frame_base;
            self.data_list.items[alloc_frame_op_index] = .{ .index = frame_size };

            try self.addData(.free_frame, .{ .index = frame_size }); // pop frame
            self.frame_variables.shrinkRetainingCapacity(self.current_frame_base);
            self.current_frame_base = old_frame_base;
        } else if (self.match(.@"if")) |_| {
            if (self.match(.left_paren) == null) return error.Syntax;
            try self.expression();
            if (self.match(.right_paren) == null) return error.Syntax;

            const if_branch_op_index = self.tags_list.items.len;
            try self.addData(.branch_cond_false, undefined);

            // if shadow
            try self.statement();

            if (self.match(.@"else")) |_| {
                // at end of if shadow, unconditionnaly jump to past else shadow
                const else_branch_op_index = self.tags_list.items.len;
                try self.addData(.branch_uncond, undefined);

                // if branch target starts here
                const if_target_index = self.tags_list.items.len;
                self.data_list.items[if_branch_op_index] = .{ .index = if_target_index };

                // else shadow
                try self.statement();

                const else_target_index = self.tags_list.items.len;
                self.data_list.items[else_branch_op_index] = .{ .index = else_target_index };
            } else {
                // if branch target starts here
                const if_target_index = self.tags_list.items.len;
                self.data_list.items[if_branch_op_index] = .{ .index = if_target_index };
            }
        } else if (self.match(.print)) |_| { // print statement
            try self.expression();
            if (self.match(.semicolon) == null) return error.Syntax;
            try self.addEmpty(.print);
        } else { // expression statement
            try self.expression();
            if (self.match(.semicolon) == null) return error.Syntax;
            try self.addEmpty(.discard);
        }
    }

    fn expression(self: *Self) error{ OutOfMemory, Syntax }!void {
        try self.assignment();
    }

    fn assignment(self: *Self) !void {
        try self.equality();

        if (self.match(.equal) != null) {
            const left_index = self.tags_list.items.len - 1;
            if (self.tags_list.items[left_index] != .variable)
                return error.Syntax;

            // Back off from the variable op, we will push an assignment on top of the right-hand side
            _ = self.tags_list.pop();
            const variable_index = self.data_list.pop().?.index;

            try self.assignment();

            try self.addData(.assignment, .{ .index = variable_index });
        }
    }

    fn equality(self: *Self) !void {
        return self.binary(comparison, &.{
            .{ .token_tag = .equal_equal, .node_tag = .equal },
            .{ .token_tag = .bang_equal, .node_tag = .not_equal },
        });
    }

    fn comparison(self: *Self) !void {
        return self.binary(term, &.{
            .{ .token_tag = .greater, .node_tag = .greater },
            .{ .token_tag = .greater_equal, .node_tag = .greater_equal },
            .{ .token_tag = .less, .node_tag = .less },
            .{ .token_tag = .less_equal, .node_tag = .less_equal },
        });
    }

    fn term(self: *Self) !void {
        return self.binary(factor, &.{
            .{ .token_tag = .plus, .node_tag = .add },
            .{ .token_tag = .minus, .node_tag = .substract },
        });
    }

    fn factor(self: *Self) !void {
        return self.binary(unary, &.{
            .{ .token_tag = .star, .node_tag = .multiply },
            .{ .token_tag = .slash, .node_tag = .divide },
        });
    }

    fn binary(
        self: *Self,
        constituent: fn (*Self) error{ OutOfMemory, Syntax }!void,
        comptime tag_mappings: []const struct { token_tag: Scanner.TokenTag, node_tag: Ast.NodeTag },
    ) error{ OutOfMemory, Syntax }!void {
        try constituent(self);
        var leftChildIndex = self.tags_list.items.len - 1;

        while (true) {
            const tag: Ast.NodeTag = blk: {
                inline for (tag_mappings) |tag_mapping|
                    if (self.match(tag_mapping.token_tag) != null) break :blk tag_mapping.node_tag;
                return;
            };

            try constituent(self);
            try self.addData(tag, .{ .index = leftChildIndex });
            leftChildIndex = self.tags_list.items.len - 1;
        }
    }

    fn unary(self: *Self) !void {
        const tag: Ast.NodeTag = blk: {
            if (self.match(.bang) != null) break :blk .not;
            if (self.match(.minus) != null) break :blk .unary_minus;

            try self.primary();
            return;
        };

        try self.unary();

        try self.addEmpty(tag);
    }

    fn primary(self: *Self) !void {
        if (self.match(.nil) != null) {
            try self.addEmpty(.nil);
        } else if (self.match(.true) != null) {
            try self.addEmpty(.true);
        } else if (self.match(.false) != null) {
            try self.addEmpty(.false);
        } else if (self.match(.number)) |token| {
            try self.addData(.number, .{ .number = token.literal.number });
        } else if (self.match(.string)) |token| {
            const string = token.literal.string;
            const start_index_result = try self.start_index_of_strings.getOrPut(self.allocator, string);
            if (!start_index_result.found_existing) {
                const start = self.string_buffer.items.len;
                try self.string_buffer.appendSlice(self.allocator, string);
                const end = self.string_buffer.items.len;

                start_index_result.key_ptr.* = self.string_buffer.items[start..];
                start_index_result.value_ptr.* = self.string_indexes_list.items.len - 1;

                try self.string_indexes_list.append(self.allocator, end);
            }
            const string_index = start_index_result.value_ptr.*;
            try self.addData(.string, .{ .index = string_index });
        } else if (self.match(.left_paren) != null) {
            try self.expression();

            if (self.match(.right_paren) == null)
                return error.Syntax;

            try self.addEmpty(.group);
        } else if (self.match(.identifier)) |identifier| {
            if (self.identifiers.getIndex(identifier.lexeme)) |identifier_index| {
                if (self.findVariableIndex(identifier_index, 0)) |variable_index| {
                    try self.addData(.variable, .{ .index = variable_index });
                    return;
                }
            }
            try self.addEmpty(.undefined);
        } else return error.Syntax;
    }

    fn match(self: *Self, token_tag: Scanner.TokenTag) ?Scanner.Token {
        const token = self.nextToken() orelse return null;

        if (token.tag == token_tag) {
            self.next_token = null;
            return token;
        }
        return null;
    }

    fn nextToken(self: *Self) ?Scanner.Token {
        while (true) {
            if (self.next_token) |token|
                return token;
            if (self.scanner.next()) |result| {
                switch (result) {
                    .token => |scanned_token| self.next_token = scanned_token,
                    .@"error" => {},
                }
            } else return null;
        }
    }

    fn addEmpty(self: *Self, tag: Ast.NodeTag) !void {
        try self.addData(tag, .{ .empty = {} });
    }

    fn addData(self: *Self, tag: Ast.NodeTag, data: Ast.Data) !void {
        try self.tags_list.append(self.allocator, tag);
        try self.data_list.append(self.allocator, data);
    }

    fn findVariableIndex(self: Self, identifier_index: usize, base_index: usize) ?usize {
        var variable_index = self.frame_variables.items.len;
        while (variable_index > base_index) {
            variable_index -= 1;
            if (self.frame_variables.items[variable_index] == identifier_index)
                return variable_index;
        } else return null;
    }
};

fn testParse(source: []const u8, parsed: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;

    const ast = try parse(allocator, source, .expression);
    defer ast.deinit(allocator);

    if (ast.root()) |node| {
        const actual = try std.fmt.allocPrint(allocator, "{s}", .{node});
        defer allocator.free(actual);

        try testing.expectEqualStrings(parsed, actual);
    } else try testing.expect(false);
}

test "parse primary expressions" {
    try testParse("nil", "nil");
    try testParse("true", "true");
    try testParse("false", "false");
    try testParse("42.47", "42.47");
    try testParse("\"hello\"", "hello");
    try testParse("(\"foo\")", "(group foo)");
}

test "parse unary expressions" {
    try testParse("!true", "(! true)");
}

test "parse binary expressions" {
    try testParse("16 * 38 / 58", "(/ (* 16.0 38.0) 58.0)");
    try testParse("52 + 80 - 94", "(- (+ 52.0 80.0) 94.0)");
    try testParse("(-92 + 90) * (60 * 99) / (39 + 51)", "(/ (* (group (+ (- 92.0) 90.0)) (group (* 60.0 99.0))) (group (+ 39.0 51.0)))");
    try testParse("83 < 99 < 115", "(< (< 83.0 99.0) 115.0)");
    try testParse("\"baz\" == \"baz\"", "(== baz baz)");
}

test "syntax error" {
    try std.testing.expectError(error.Syntax, testParse("(72 +)", ""));
}
