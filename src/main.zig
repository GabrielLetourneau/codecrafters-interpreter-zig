const std = @import("std");
const Allocator = std.mem.Allocator;

const Scanner = @import("Scanner.zig");
const parsing = @import("parsing.zig");
const Ast = @import("Ast.zig");
const generate = @import("gen.zig").generate;
const Bytecode = @import("Bytecode.zig");
const Runtime = @import("Runtime.zig");

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

    switch (command_tag) {
        .tokenize => try scan(allocator, file_contents),
        .parse => try parse(allocator, file_contents),
        .evaluate => try evaluate(allocator, file_contents),
        .run => try run(allocator, file_contents),
    }
}

const CommandTag = enum { tokenize, parse, evaluate, run };

fn scan(allocator: Allocator, file_contents: []const u8) !void {
    defer allocator.free(file_contents);

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

fn parse_or_exit(allocator: Allocator, file_contents: []const u8, root_symbol: Ast.RootSymbol) !Ast {
    defer allocator.free(file_contents);
    return parsing.parse(allocator, file_contents, root_symbol) catch |err| switch (err) {
        error.Syntax => {
            try std.io.getStdErr().writer().writeAll("Syntax error\n");
            std.process.exit(65);
        },
        else => return err,
    };
}

fn parse(allocator: Allocator, file_contents: []const u8) !void {
    const ast = try parse_or_exit(allocator, file_contents, .expression);
    defer {
        ast.deinitStrings(allocator);
        ast.deinit(allocator);
    }

    const root = ast.root() orelse return;

    const out = std.io.getStdOut().writer();
    try out.print("{s}\n", .{root});
}

fn generate_or_exit(allocator: Allocator, file_contents: []const u8, root_symbol: Ast.RootSymbol) !?Bytecode {
    const ast = try parse_or_exit(allocator, file_contents, root_symbol);
    defer ast.deinit(allocator);
    errdefer ast.deinitStrings(allocator);

    const root = ast.root() orelse return null;

    return try generate(allocator, root, root_symbol);
}

fn evaluate(allocator: Allocator, file_contents: []const u8) !void {
    const bytecode = (try generate_or_exit(allocator, file_contents, .expression)) orelse return;
    defer bytecode.deinit(allocator);

    const start = bytecode.startOp() orelse return;

    var runtime: Runtime = Runtime.init(allocator, undefined);
    defer runtime.deinit();

    const value = runtime.evaluate(start) catch |err| switch (err) {
        error.Semantics => {
            try std.io.getStdErr().writer().writeAll("Semantics error\n");
            std.process.exit(70);
        },
        else => return,
    };
    defer runtime.free(value);

    const out = std.io.getStdOut().writer();
    try out.print("{s}\n", .{value});
}

fn run(allocator: Allocator, file_contents: []const u8) !void {
    const bytecode = (try generate_or_exit(allocator, file_contents, .program)) orelse return;
    defer bytecode.deinit(allocator);

    const start = bytecode.startOp() orelse return;

    const out = std.io.getStdOut().writer().any();

    var runtime: Runtime = Runtime.init(allocator, out);
    defer runtime.deinit();

    runtime.run(start) catch |err| switch (err) {
        error.Semantics => {
            try std.io.getStdErr().writer().writeAll("Semantics error\n");
            std.process.exit(70);
        },
        else => return,
    };
}
