const std = @import("std");
const Scanner = @import("Scanner.zig");
const Parser = @import("Parser.zig");

pub fn main() !void {
    var gpa: std.heap.DebugAllocator(.{}) = .{};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

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
    defer allocator.free(file_contents);

    var scanner: Scanner = .{ .source = file_contents };

    switch (command_tag) {
        .tokenize => try scan(&scanner),
        .parse => try parse(&scanner, allocator),
    }
}

const CommandTag = enum { tokenize, parse };

fn scan(scanner: *Scanner) !void {
    var hasErrors = false;

    const out = std.io.getStdOut().writer();
    const err = std.io.getStdErr().writer();

    while (scanner.next()) |result| {
        switch (result) {
            .token => |token| try out.print("{s}\n", .{token}),
            .@"error" => |@"error"| {
                try err.print("{s}\n", .{@"error"});
                hasErrors = true;
            },
        }
    }

    try out.writeAll("EOF  null\n");

    if (hasErrors)
        std.process.exit(65);
}

fn parse(scanner: *Scanner, allocator: std.mem.Allocator) !void {
    var parser: Parser = .{ .scanner = scanner, .allocator = allocator };
    defer parser.deinit();

    const maybe_head_node = parser.parse() catch |err| switch (err) {
        error.Syntax => {
            try std.io.getStdErr().writer().writeAll("Syntax error\n");
            std.process.exit(65);
        },
        else => return,
    };

    if (maybe_head_node) |head_node| {
        const out = std.io.getStdOut().writer();
        try out.print("{s}\n", .{head_node});
    }
}
