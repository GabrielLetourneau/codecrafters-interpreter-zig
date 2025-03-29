const std = @import("std");
const Scanner = @import("Scanner.zig");

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

    var scanner: Scanner = .{ .source = file_contents };
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
