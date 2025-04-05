const std = @import("std");
const Allocator = std.mem.Allocator;

const Scanner = @import("Scanner.zig");
const parsing = @import("parsing.zig");
const eval = @import("eval.zig");

pub fn main() !void {
    var gpa: std.heap.DebugAllocator(.{}) = .{};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const command_tag, const file_contents = blk: {
        const args = try std.process.argsAlloc(allocator);
        defer std.process.argsFree(allocator, args);

        if (args.len < 3) {
            std.debug.print("Usage: ./your_program.sh <command> <filename>\n", .{});
            std.process.exit(1);
        }

        const command = args[1];
        const filename = args[2];

        const command_tag: CommandTag =
            if (std.meta.stringToEnum(CommandTag, command)) |actual| actual else {
                std.debug.print("Unknown command: {s}\n", .{command});
                std.process.exit(1);
            };

        const file_contents = try std.fs.cwd().readFileAlloc(allocator, filename, std.math.maxInt(usize));

        break :blk .{ command_tag, file_contents };
    };
    defer allocator.free(file_contents);

    switch (command_tag) {
        .tokenize => try scan(file_contents),
        .parse => try parse(file_contents, allocator),
        .evaluate => try evaluate(file_contents, allocator),
    }
}

const CommandTag = enum { tokenize, parse, evaluate };

fn scan(file_contents: []const u8) !void {
    var scanner: Scanner = .{ .source = file_contents };

    var has_errors = false;

    const out = std.io.getStdOut().writer();
    const err = std.io.getStdErr().writer();

    while (scanner.next()) |result| {
        switch (result) {
            .token => |token| try out.print("{s}\n", .{token}),
            .@"error" => |@"error"| {
                try err.print("{s}\n", .{@"error"});
                has_errors = true;
            },
        }
    }

    try out.writeAll("EOF  null\n");

    if (has_errors)
        std.process.exit(65);
}

fn parse(file_contents: []const u8, allocator: Allocator) !void {
    const ast = parsing.parse(allocator, file_contents) catch |err| switch (err) {
        error.Syntax => {
            try std.io.getStdErr().writer().writeAll("Syntax error\n");
            std.process.exit(65);
        },
        else => return,
    };
    defer ast.deinit(allocator);

    if (ast.root()) |head_node| {
        const out = std.io.getStdOut().writer();
        try out.print("{s}\n", .{head_node});
    }
}

fn evaluate(file_contents: []const u8, allocator: Allocator) !void {
    const ast = parsing.parse(allocator, file_contents) catch |err| switch (err) {
        error.Syntax => {
            try std.io.getStdErr().writer().writeAll("Syntax error\n");
            std.process.exit(65);
        },
        else => return,
    };
    defer ast.deinit(allocator);

    const value = try eval.evaluate(allocator, ast);
    const out = std.io.getStdOut().writer();
    try out.print("{s}\n", .{value});
}
