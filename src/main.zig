const std = @import("std");
const Allocator = std.mem.Allocator;

const Scanner = @import("Scanner.zig");
const parsing = @import("parsing.zig");
const Ast = @import("Ast.zig");
const generate = @import("gen.zig").generate;
const Bytecode = @import("Bytecode.zig");
const Runtime = @import("Runtime.zig");

pub fn main(init: std.process.Init) !void {
    var gpa: std.heap.DebugAllocator(.{}) = .{};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();
    const io = init.io;

    var stdout_buffer: [1024]u8 = undefined;
    var stdout_file_writer = std.Io.File.Writer.init(.stdout(), io, &stdout_buffer);

    var stderr_buffer: [1024]u8 = undefined;
    var stderr_file_writer = std.Io.File.Writer.init(.stderr(), io, &stderr_buffer);

    const command_tag, const file_contents = blk: {
        const args = try init.minimal.args.toSlice(init.arena.allocator());

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

        const file_contents = try std.Io.Dir.cwd().readFileAlloc(io, filename, allocator, .unlimited);

        break :blk .{ command_tag, file_contents };
    };

    switch (command_tag) {
        .tokenize => try scan(allocator, &stdout_file_writer, &stderr_file_writer, file_contents),
        .parse => try parse(allocator, &stdout_file_writer, &stderr_file_writer, file_contents),
        .evaluate => try evaluate(allocator, io, &stdout_file_writer, &stderr_file_writer, file_contents),
        .run => try run(allocator, io, &stdout_file_writer, &stderr_file_writer, file_contents),
    }

    try stdout_file_writer.flush();
    try stderr_file_writer.flush();
}

const CommandTag = enum { tokenize, parse, evaluate, run };

fn scan(
    allocator: Allocator,
    stdout_file_writer: *std.Io.File.Writer,
    stderr_file_writer: *std.Io.File.Writer,
    file_contents: []const u8,
) !void {
    defer allocator.free(file_contents);

    var scanner: Scanner = .{ .source = file_contents };

    var has_errors = false;

    const out = &stdout_file_writer.interface;
    const err = &stderr_file_writer.interface;

    while (scanner.next()) |result| {
        switch (result) {
            .token => |token| try out.print("{f}\n", .{token}),
            .@"error" => |@"error"| {
                try err.print("{f}\n", .{@"error"});
                has_errors = true;
            },
        }
    }

    try out.writeAll("EOF  null\n");

    if (has_errors) {
        try stdout_file_writer.flush();
        try stderr_file_writer.flush();
        std.process.exit(65);
    }
}

fn parse_or_exit(
    allocator: Allocator,
    stderr_file_writer: *std.Io.File.Writer,
    file_contents: []const u8,
    root_symbol: Ast.RootSymbol,
) !Ast {
    defer allocator.free(file_contents);
    return parsing.parse(allocator, file_contents, root_symbol) catch |err| switch (err) {
        error.Syntax => {
            try stderr_file_writer.interface.writeAll("Syntax error\n");
            try stderr_file_writer.flush();
            std.process.exit(65);
        },
        else => return err,
    };
}

fn parse(
    allocator: Allocator,
    stdout_file_writer: *std.Io.File.Writer,
    stderr_file_writer: *std.Io.File.Writer,
    file_contents: []const u8,
) !void {
    const ast = try parse_or_exit(allocator, stderr_file_writer, file_contents, .expression);
    defer {
        ast.deinitStrings(allocator);
        ast.deinit(allocator);
    }

    const root = ast.root() orelse return;

    try stdout_file_writer.interface.print("{f}\n", .{root});
}

fn generate_or_exit(
    allocator: Allocator,
    stderr_file_writer: *std.Io.File.Writer,
    file_contents: []const u8,
    root_symbol: Ast.RootSymbol,
) !?Bytecode {
    const ast = try parse_or_exit(allocator, stderr_file_writer, file_contents, root_symbol);
    defer ast.deinit(allocator);
    errdefer ast.deinitStrings(allocator);

    const root = ast.root() orelse return null;

    return generate(allocator, root, root_symbol) catch |err| switch (err) {
        error.Semantics => {
            try stderr_file_writer.interface.writeAll("Semantics error\n");
            try stderr_file_writer.flush();
            std.process.exit(65);
        },
        else => return err,
    };
}

fn evaluate(
    allocator: Allocator,
    io: std.Io,
    stdout_file_writer: *std.Io.File.Writer,
    stderr_file_writer: *std.Io.File.Writer,
    file_contents: []const u8,
) !void {
    const bytecode = (try generate_or_exit(allocator, stderr_file_writer, file_contents, .expression)) orelse return;
    defer bytecode.deinit(allocator);

    const start = bytecode.startOp() orelse return;

    var runtime: Runtime = Runtime.init(allocator, io, &stdout_file_writer.interface);
    defer runtime.deinit();

    const value = runtime.evaluate(start) catch |err| switch (err) {
        error.Runtime => {
            try stdout_file_writer.flush();
            try stderr_file_writer.interface.writeAll("Runtime error\n");
            try stderr_file_writer.flush();
            std.process.exit(70);
        },
        else => return,
    };
    defer runtime.free(value);

    try stdout_file_writer.interface.print("{f}\n", .{value.inContext(&bytecode)});
}

fn run(
    allocator: Allocator,
    io: std.Io,
    stdout_file_writer: *std.Io.File.Writer,
    stderr_file_writer: *std.Io.File.Writer,
    file_contents: []const u8,
) !void {
    const bytecode = (try generate_or_exit(allocator, stderr_file_writer, file_contents, .program)) orelse return;
    defer bytecode.deinit(allocator);

    const start = bytecode.startOp() orelse return;

    var runtime: Runtime = Runtime.init(allocator, io, &stdout_file_writer.interface);
    defer runtime.deinit();

    runtime.run(start) catch |err| switch (err) {
        error.Runtime => {
            try stdout_file_writer.flush();
            try stderr_file_writer.interface.writeAll("Runtime error\n");
            try stderr_file_writer.flush();
            std.process.exit(70);
        },
        else => return,
    };
}
