const std = @import("std");

pub const NodeTag = enum(u8) {
    // Primary expressions, no data
    nil,
    true,
    false,

    // Primary expressions, with data
    number,
    string,

    // Unary expressions, no data
    group,
    not,
    unary_minus,

    // Binary expressions
    multiply,
    divide,
    add,
    substract,
    greater,
    greater_equal,
    less,
    less_equal,
    equal,
    not_equal,
};

pub const Data = union {
    number: f64,
    string: []const u8,
    lhs_reference: struct { tag_index: u32, data_index: u32 },
};

const Ast = @This();

pub const Node = struct {
    ast: Ast,
    tag_index: u32,
    data_index: u32,

    const Self = @This();

    pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const tag = self.ast.tags[self.tag_index];
        switch (tag) {
            .nil, .true, .false => try writer.writeAll(@tagName(tag)),
            .number => {
                const literal_number = self.ast.data[self.data_index].number;
                try writer.print("{d}", .{literal_number});
                if (literal_number == @trunc(literal_number)) try writer.writeAll(".0");
            },
            .string => try writer.writeAll(self.ast.data[self.data_index].string),
            .group => try writer.print("(group {s})", .{self.onlyChild()}),
            .not => try writer.print("(! {s})", .{self.onlyChild()}),
            .unary_minus => try writer.print("(- {s})", .{self.onlyChild()}),
            .multiply => try writer.print("(* {s} {s})", .{ self.lhs(), self.rhs() }),
            .divide => try writer.print("(/ {s} {s})", .{ self.lhs(), self.rhs() }),
            .add => try writer.print("(+ {s} {s})", .{ self.lhs(), self.rhs() }),
            .substract => try writer.print("(- {s} {s})", .{ self.lhs(), self.rhs() }),
            .greater => try writer.print("(> {s} {s})", .{ self.lhs(), self.rhs() }),
            .greater_equal => try writer.print("(>= {s} {s})", .{ self.lhs(), self.rhs() }),
            .less => try writer.print("(< {s} {s})", .{ self.lhs(), self.rhs() }),
            .less_equal => try writer.print("(<= {s} {s})", .{ self.lhs(), self.rhs() }),
            .equal => try writer.print("(== {s} {s})", .{ self.lhs(), self.rhs() }),
            .not_equal => try writer.print("(!= {s} {s})", .{ self.lhs(), self.rhs() }),
        }
    }

    fn onlyChild(self: Self) Self {
        return .{
            .ast = self.ast,
            .tag_index = self.tag_index - 1,
            .data_index = self.data_index,
        };
    }

    fn lhs(self: Self) Self {
        const lhs_reference = self.ast.data[self.data_index].lhs_reference;
        return .{
            .ast = self.ast,
            .tag_index = lhs_reference.tag_index,
            .data_index = lhs_reference.data_index,
        };
    }

    fn rhs(self: Self) Self {
        return .{ .ast = self.ast, .tag_index = self.tag_index - 1, .data_index = self.data_index - 1 };
    }
};

tags: []const NodeTag,
data: []const Data,
