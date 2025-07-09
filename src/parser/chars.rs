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
