const std = @import("std");

test {
    std.testing.refAllDecls(@import("Runtime.zig"));
    std.testing.refAllDecls(@import("parsing.zig"));
    std.testing.refAllDecls(@import("Scanner.zig"));
}
