const std = @import("std");
const Allocator = std.mem.Allocator;

const Ast = @import("Ast.zig");
const Scanner = @import("Scanner.zig");

pub fn parse(allocator: Allocator, file_contents: []const u8, root_symbol: Ast.RootSymbol) !Ast {
    var parser: Parser = .{
        .scanner = .{ .source = file_contents },
        .allocator = allocator,
    };
    defer {
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

    next_token: ?Scanner.Token = null,

    const Self = @This();

    fn program(self: *Self) !void {
        _ = try self.getStringStartIndex("clock");

        try self.addEmpty(.empty);

        while (self.nextToken()) |_| {
            const lhs_index = self.lastNodeIndex();

            try self.declaration();

            try self.addIndexed(.declarations, lhs_index);
        }
    }

    fn declaration(self: *Self) error{ OutOfMemory, Syntax }!void {
        if (try self.funDeclaration() or
            try self.varDeclaration())
            return;

        try self.statement();
    }

    fn funDeclaration(self: *Self) !bool {
        if (self.match(.fun) == null)
            return false;

        const identifier = self.match(.identifier) orelse
            return error.Syntax;
        const identifier_index = try self.getStringStartIndex(identifier.lexeme);

        if (self.match(.left_paren) == null)
            return error.Syntax;

        try self.addEmpty(.empty);

        while (self.match(.identifier)) |param_identifier| {
            const param_identifier_index = try self.getStringStartIndex(param_identifier.lexeme);
            try self.addIndexed(.parameter, param_identifier_index);

            if (self.match(.right_paren)) |_|
                break;

            if (self.match(.comma) == null)
                return error.Syntax;
        } else if (self.match(.right_paren) == null)
            return error.Syntax;

        const lhs_index = self.lastNodeIndex();

        if (!try self.block())
            return error.Syntax;

        try self.addIndexed(.fun_def, lhs_index);

        try self.addIndexed(.fun_decl, identifier_index);

        return true;
    }

    fn varDeclaration(self: *Self) !bool {
        if (self.match(.@"var") == null)
            return false;

        const identifier = self.match(.identifier) orelse return error.Syntax;
        const identifier_index = try self.getStringStartIndex(identifier.lexeme);

        if (self.match(.equal)) |_| {
            try self.expression();
            if (self.match(.semicolon) == null) return error.Syntax;
            try self.addIndexed(.var_decl_init, identifier_index);
        } else {
            if (self.match(.semicolon) == null) return error.Syntax;
            try self.addIndexed(.var_decl, identifier_index);
        }

        return true;
    }

    fn block(self: *Self) !bool {
        if (self.match(.left_brace) == null)
            return false;

        try self.addEmpty(.empty);

        while (self.match(.right_brace) == null) {
            const lhs_index = self.lastNodeIndex();

            try self.declaration();

            try self.addIndexed(.declarations, lhs_index);
        }

        try self.addEmpty(.block);

        return true;
    }

    fn statement(self: *Self) !void {
        if (try self.block())
            return;

        if (self.match(.@"if")) |_| {
            if (self.match(.left_paren) == null) return error.Syntax;
            try self.expression();
            if (self.match(.right_paren) == null) return error.Syntax;
            var lhs_index = self.lastNodeIndex();

            // if shadow
            try self.statement();

            try self.addIndexed(.@"if", lhs_index);

            if (self.match(.@"else")) |_| {
                lhs_index = self.lastNodeIndex();

                try self.statement();

                try self.addIndexed(.@"else", lhs_index);
            }
        } else if (self.match(.@"while")) |_| {
            if (self.match(.left_paren) == null) return error.Syntax;
            try self.expression();
            if (self.match(.right_paren) == null) return error.Syntax;
            const lhs_index = self.lastNodeIndex();

            // while shadow
            try self.statement();

            try self.addIndexed(.@"while", lhs_index);
        } else if (self.match(.@"for")) |_| {
            if (self.match(.left_paren) == null) return error.Syntax;

            if (self.match(.semicolon)) |_| {
                try self.addEmpty(.empty);
            } else if (!try self.varDeclaration())
                try self.expressionStatement();
            var lhs_index = self.lastNodeIndex();

            if (self.match(.semicolon)) |_| {
                try self.addEmpty(.empty);
            } else {
                try self.expression();
                if (self.match(.semicolon) == null) return error.Syntax;
            }

            try self.addIndexed(.for_init_cond, lhs_index);
            lhs_index = self.lastNodeIndex();

            if (self.match(.right_paren)) |_| {
                try self.addEmpty(.empty);
            } else {
                try self.expression();
                if (self.match(.right_paren) == null) return error.Syntax;
            }

            try self.addIndexed(.for_preamble, lhs_index);
            lhs_index = self.lastNodeIndex();

            // for shadow
            try self.statement();

            try self.addIndexed(.@"for", lhs_index);
            try self.addEmpty(.block); // simplifies handling of variable declaration in code gen
        } else if (self.match(.print)) |_| { // print statement
            try self.expression();
            if (self.match(.semicolon) == null) return error.Syntax;
            try self.addEmpty(.print);
        } else if (self.match(.@"return")) |_| {
            if (self.match(.semicolon)) |_| {
                try self.addEmpty(.nil);
            } else {
                try self.expression();
                if (self.match(.semicolon) == null) return error.Syntax;
            }
            try self.addEmpty(.@"return");
        } else try self.expressionStatement();
    }

    fn expressionStatement(self: *Self) !void {
        try self.expression();
        if (self.match(.semicolon) == null) return error.Syntax;
    }

    fn expression(self: *Self) error{ OutOfMemory, Syntax }!void {
        try self.assignment();
    }

    fn assignment(self: *Self) !void {
        try self.@"or"();

        if (self.match(.equal) != null) {
            const lhs_index = self.lastNodeIndex();
            if (self.tags_list.items[lhs_index] != .variable)
                return error.Syntax;

            // Cancel off the variable node, we will push an assignment on top of the right-hand side
            _ = self.tags_list.pop();
            const variable_index = self.data_list.pop().?.index;

            try self.assignment();

            try self.addIndexed(.assignment, variable_index);
        }
    }

    fn @"or"(self: *Self) !void {
        try self.@"and"();

        while (self.match(.@"or")) |_| {
            const lhs_index = self.lastNodeIndex();

            try self.@"and"();

            try self.addIndexed(.@"or", lhs_index);
        }
    }

    fn @"and"(self: *Self) !void {
        try self.equality();

        while (self.match(.@"and")) |_| {
            const lhs_index = self.lastNodeIndex();

            try self.equality();

            try self.addIndexed(.@"and", lhs_index);
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

        while (true) {
            const tag: Ast.NodeTag = blk: {
                inline for (tag_mappings) |tag_mapping|
                    if (self.match(tag_mapping.token_tag) != null) break :blk tag_mapping.node_tag;
                return;
            };
            const lhs_index = self.lastNodeIndex();

            try constituent(self);

            try self.addIndexed(tag, lhs_index);
        }
    }

    fn unary(self: *Self) !void {
        const tag: Ast.NodeTag = blk: {
            if (self.match(.bang) != null) break :blk .not;
            if (self.match(.minus) != null) break :blk .unary_minus;

            try self.call();
            return;
        };

        try self.unary();

        try self.addEmpty(tag);
    }

    fn call(self: *Self) !void {
        try self.primary();

        while (self.match(.left_paren)) |_| {
            const lhs_index = self.lastNodeIndex();

            try self.addEmpty(.empty);

            if (self.match(.right_paren) == null) {
                while (true) {
                    const arguments_lhs_index = self.lastNodeIndex();

                    try self.expression();

                    try self.addIndexed(.arguments, arguments_lhs_index);

                    if (self.match(.comma) == null) break;
                }

                if (self.match(.right_paren) == null) return error.Syntax;
            }

            try self.addIndexed(.call, lhs_index);
        }
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
            const string_start_index = try self.getStringStartIndex(token.literal.string);
            try self.addIndexed(.string, string_start_index);
        } else if (self.match(.left_paren) != null) {
            try self.expression();

            if (self.match(.right_paren) == null)
                return error.Syntax;

            try self.addEmpty(.group);
        } else if (self.match(.identifier)) |identifier| {
            const identifier_index = try self.getStringStartIndex(identifier.lexeme);
            try self.addIndexed(.variable, identifier_index);
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

    fn lastNodeIndex(self: Self) usize {
        return self.tags_list.items.len - 1;
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

    fn addIndexed(self: *Self, tag: Ast.NodeTag, index: usize) !void {
        try self.addData(tag, .{ .index = index });
    }

    fn getStringStartIndex(self: *Self, string: []const u8) !usize {
        const start_index_result = try self.start_index_of_strings.getOrPut(self.allocator, string);
        if (!start_index_result.found_existing) {
            try self.string_buffer.appendSlice(self.allocator, string);
            const end = self.string_buffer.items.len;

            start_index_result.key_ptr.* = string;
            start_index_result.value_ptr.* = self.string_indexes_list.items.len - 1;

            try self.string_indexes_list.append(self.allocator, end);
        }
        return start_index_result.value_ptr.*;
    }
};

fn testParse(source: []const u8, parsed: []const u8) !void {
    const testing = std.testing;
    const allocator = testing.allocator;

    const ast = try parse(allocator, source, .expression);
    defer {
        ast.deinitStrings(allocator);
        ast.deinit(allocator);
    }

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
    try testParse("!!35", "(! (! 35.0))");
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
