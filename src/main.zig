const std = @import("std");
const scan = @import("scan.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 3) {
        std.debug.print("Usage: ./your_program.sh tokenize <filename>\n", .{});
        std.process.exit(1);
    }

    const command = args[1];
    const filename = args[2];

    if (!std.mem.eql(u8, command, "tokenize")) {
        std.debug.print("Unknown command: {s}\n", .{command});
        std.process.exit(1);
    }

    const file_contents = try std.fs.cwd().readFileAlloc(allocator, filename, std.math.maxInt(usize));

    const scan_result = try scan.scan(file_contents, allocator);

    const out = std.io.getStdOut().writer();
    for (scan_result.tokens) |token| {
        try out.print("{s}\n", .{token});
    }

    const err = std.io.getStdErr().writer();
    for (scan_result.errors) |@"error"| {
        try err.print("{s}\n", .{@"error"});
    }

    if (scan_result.errors.len > 0) std.process.exit(65);
}
