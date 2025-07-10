use super::Parser;

type CharTest = fn(u8) -> bool;

impl<'a> Parser<'a> {
    /// Consume the specified tag if it appears next in the input,
    /// othewise consume nothing.
    /// Return `true` if the tag was found and consumed.
    pub(super) fn tag(&mut self, tag: &[u8]) -> bool {
        if self.input.len() < self.pos + tag.len() {
            return false;
        }

        if &self.input[self.pos..(self.pos + tag.len())] != tag {
            return false;
        }

        self.pos += tag.len();

        true
    }

    /// Consume the specified char (only once) if it appears next in the input,
    /// othewise consume nothing.
    /// Return `true` if the charater was found and consumed.
    pub(super) fn char(&mut self, expect: u8) -> bool {
        if self.pos >= self.input.len() {
            return false;
        }

        let c = self.input[self.pos];
        if c != expect {
            return false;
        };
        self.pos += 1;

        true
    }

    /// Consume as many chacters as possible with match the specified test (for which `test` function returns true.
    /// Return the number of charcters consumed.
    pub(super) fn repeat(&mut self, test: CharTest) -> usize {
        let start = self.pos;
        let mut pos = start;
        let len = self.input.len();
        while pos < len {
            let c = self.input[pos];
            if !test(c) {
                break;
            };
            pos += 1;
        }
        self.pos = pos;

        pos - start
    }

    /// Same as `repeat` but returns a tupple containing the number of characters consumed and the first non-consumed character which ended the input matching.
    /// If the end of input is reached consuming all remaining bytes, a `0` is returned as next byte.
    /// Return the number of charcters consumed.
    pub(super) fn repeat_and_peek(&mut self, test: CharTest) -> (usize, u8) {
        let start = self.pos;
        let mut pos = start;
        let len = self.input.len();
        let mut peek;

        loop {
            if pos < len {
                peek = self.input[pos];
                if !test(peek) {
                    break;
                };
                pos += 1;
            } else {
                peek = 0;
                break;
            }
        }
        self.pos = pos;

        (pos - start, peek)
    }

    /// Peek at the next byte in the input without consuming it.
    /// Return '0' if the end of the input has been reached.
    pub(super) fn peek(&mut self) -> u8 {
        *self.input.get(self.pos).unwrap_or(&0)
    }

    pub(super) fn is_eoi(&self) -> bool {
        self.pos >= self.input.len()
    }
}

#[inline]
pub(super) fn is_digit(c: u8) -> bool {
    c.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[track_caller]
    fn do_test_tag(input: &str, tag: &str, expected: bool) {
        let mut p = Parser::new(input);
        assert_eq!(p.tag(tag.as_bytes()), expected);

        match expected {
            true => assert_eq!(p.pos, tag.len()),
            false => assert_eq!(p.pos, 0),
        }
    }

    #[test]
    fn test_tag() {
        do_test_tag("foobar", "foo", true);
        do_test_tag("foobar", "fooo", false);
    }

    #[track_caller]
    fn do_test_char(input: &str, c: u8, pass: bool) {
        let mut p = Parser::new(input);
        p.pos = 1;

        match pass {
            true => {
                assert_eq!(p.char(c), true);
                assert_eq!(p.pos, 2);
            }
            false => {
                assert_eq!(p.char(c), false);
                assert_eq!(p.pos, 1);
            }
        }
    }

    #[test]
    fn test_char() {
        do_test_char("-f-", b'f', true);
        do_test_char("-f-", b'x', false);
    }

    #[track_caller]
    fn do_test_repeat(input: &str, test: CharTest, expect_count: usize) {
        let mut p = Parser::new(input);
        p.pos = 1;

        assert_eq!(p.repeat(test), expect_count);
        assert_eq!(p.pos, 1 + expect_count);
    }

    #[test]
    fn test_repeat() {
        do_test_repeat("-abcabcabc-", |c| c.is_ascii_alphanumeric(), 9);
        do_test_repeat("-abcabcabc-", |c| c == b'a', 1);
        do_test_repeat("-abcabcabc-", |c| c == b'x', 0);
    }

    #[track_caller]
    fn do_test_repeat_and_peek(input: &str, test: CharTest, expect_count: usize, expect_peek: u8) {
        let mut p = Parser::new(input);
        p.pos = 1;

        assert_eq!(p.repeat_and_peek(test), (expect_count, expect_peek));
        assert_eq!(p.pos, 1 + expect_count);
    }

    #[test]
    fn test_repeat_and_peek() {
        do_test_repeat_and_peek("-abcabcabc--", |c| c.is_ascii_alphanumeric(), 9, b'-');
        do_test_repeat_and_peek("-abcabcabc", |c| c.is_ascii_alphanumeric(), 9, 0);
        do_test_repeat_and_peek("-abcabcabc--", |c| c == b'a', 1, b'b');
        do_test_repeat_and_peek("-abcabcabc--", |c| c == b'x', 0, b'a');
    }

    #[track_caller]
    fn do_test_peek(input: &str, start: usize, expect: u8) {
        let mut p = Parser::new(input);
        p.pos = start;

        assert_eq!(p.peek(), expect);
        assert_eq!(p.pos, start);
    }

    #[test]
    fn test_peek() {
        do_test_peek("abc", 0, b'a');
        do_test_peek("abc", 2, b'c');
        do_test_peek("abc", 3, 0);
        do_test_peek("abc", 8, 0);
    }

    #[track_caller]
    fn do_test_eoi(input: &str, start: usize, eoi: bool) {
        let mut p = Parser::new(input);
        p.pos = start;

        assert_eq!(p.is_eoi(), eoi);
        assert_eq!(p.pos, start);
    }

    #[test]
    fn test_eoi() {
        do_test_eoi("abc", 0, false);
        do_test_eoi("abc", 2, false);
        do_test_eoi("abc", 3, true);
        do_test_eoi("abc", 8, true);
    }
}
