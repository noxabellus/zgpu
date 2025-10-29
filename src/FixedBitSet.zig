const std = @import("std");

/// A fixed-size bit set for tracking dirty state, etc.
/// This works for all of our bitsets, where std.StaticBitSet causes segfaults on larger sizes.
pub fn new(comptime N: comptime_int) type {
    if (N == 0) @compileError("FixedBitSet size cannot be zero");

    return extern struct {
        const Self = @This();
        const Word = u64;
        const word_bits = @bitSizeOf(Word);
        pub const num_words = (N + word_bits - 1) / word_bits;

        words: [num_words]Word,

        /// Sets the bit at the given index to 1.
        pub fn set(self: *Self, index: u32) void {
            std.debug.assert(index < N);
            const word_index = index / word_bits;
            const bit_index = index % word_bits;
            self.words[0..][word_index] |= (@as(Word, 1) << @intCast(bit_index));
        }

        /// Sets the bit at the given index to 0.
        pub fn unset(self: *Self, index: u32) void {
            std.debug.assert(index < N);
            const word_index = index / word_bits;
            const bit_index = index % word_bits;
            self.words[0..][word_index] &= ~(@as(Word, 1) << @intCast(bit_index));
        }

        /// Returns true if the bit at the given index is 1.
        pub fn isSet(self: *const Self, index: u32) bool {
            std.debug.assert(index < N);
            const word_index = index / word_bits;
            const bit_index = index % word_bits;
            return (self.words[0..][word_index] & (@as(Word, 1) << @intCast(bit_index))) != 0;
        }

        /// Sets all bits to 1.
        pub fn setAll(self: *Self) void {
            @memset(&self.words, std.math.maxInt(Word));
        }

        /// Sets all bits to 0.
        pub fn unsetAll(self: *Self) void {
            @memset(&self.words, 0);
        }

        /// Copy state from another bit set into this one.
        pub fn copy(self: *Self, other: *const Self) void {
            @memcpy(&self.words, &other.words);
        }

        /// Counts the number of set bits.
        pub fn count(self: *const Self) u32 {
            var total: u32 = 0;
            for (self.words) |word| {
                total += @popCount(word);
            }
            return total;
        }

        /// An iterator that yields the index of each set bit.
        pub const Iterator = struct {
            words: []const Word,
            word_index: u32 = 0,
            current_word: Word = 0,

            pub fn next(self: *Iterator) ?u32 {
                while (self.current_word == 0) {
                    if (self.word_index >= num_words) return null;
                    self.current_word = self.words[self.word_index];
                    self.word_index += 1;
                }

                const bit_offset = @ctz(self.current_word);
                const bit_index = (self.word_index - 1) * word_bits + bit_offset;

                // Unset this bit in our temporary copy so we find the next one
                self.current_word &= self.current_word - 1;

                return bit_index;
            }
        };

        /// Returns an iterator over the set bits.
        pub fn iterator(self: *const Self) Iterator {
            return .{ .words = self.words[0..] };
        }
    };
}
