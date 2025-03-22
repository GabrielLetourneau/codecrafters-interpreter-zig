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
    defer allocator.free(file_contents);

    const tokens: []const scan.Token = try scan.scan(file_contents, allocator);

    const out = std.io.getStdOut().writer();
    for (tokens) |token| {
        try out.print("{s}\n", .{token});
    }
}
