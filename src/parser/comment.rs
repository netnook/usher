use super::{ParseResult, Parser, SyntaxError};

impl<'a> Parser<'a> {
    /// Consume as much whitespace and comments as possible.  Return an error
    /// if neither is found.
    pub(super) fn req_whitespace_comments(&mut self) -> ParseResult<()> {
        let start = self.pos;
        self.whitespace_comments();
        if self.pos > start {
            Ok(())
        } else {
            Err(SyntaxError::new(start, "Expected whitespace or comment."))
        }
    }

    /// Consume as much whitespace and comments as possible.
    pub(super) fn whitespace_comments(&mut self) {
        loop {
            self.whitespace();
            if !self.comment() {
                break;
            }
        }
    }

    /// Consume a single comment (including trailing CRLF) if it appears next in the input,
    /// otherwise consumes nothing.
    /// Return true if a comment was found and consumed.
    pub(super) fn comment(&mut self) -> bool {
        if !self.char(b'#') {
            return false;
        };

        let (_, ending) = self.repeat_and_peek(|b| b != b'\n' && b != b'\r');

        // ending must be CR, LF or EOI
        if ending == b'\r' {
            self.pos += 1;
            self.char(b'\n');
        } else if ending == b'\n' {
            self.pos += 1;
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[track_caller]
    fn do_test_comment(input: &str, expect_match: bool, expect_end: usize) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(p.comment(), expect_match);
        assert_eq!(p.pos, expect_end);
    }

    #[test]
    fn test_comment() {
        do_test_comment("- # starts too late", false, 1);
        do_test_comment("-# a comment \r\r\rsome more", true, 14);
        do_test_comment("-# a comment \n\n\nsome more", true, 14);
        do_test_comment("-# a comment \n\r\nsome more", true, 14);
        do_test_comment("-# a comment \r\n\rsome more", true, 15);
        do_test_comment("-# a comment \n#another comment", true, 14);
    }

    #[track_caller]
    fn do_test_whitespace_comments(input: &str, expect_len: usize, expect_next: u8) {
        let mut p = Parser::new(input);
        p.pos = 1;
        p.whitespace_comments();
        assert_eq!(p.pos, expect_len + 1);
        assert_eq!(p.peek(), expect_next);
    }

    #[test]
    fn test_whitespace_comments() {
        do_test_whitespace_comments("-not a comment-", 0, b'n');
        do_test_whitespace_comments("-", 0, 0);
        do_test_whitespace_comments("-# a comment", 11, 0);
        do_test_whitespace_comments("-# a comment\n-", 12, b'-');
        do_test_whitespace_comments(
            "-# first comment\n# second comment\nnot a comment",
            33,
            b'n',
        );
        do_test_whitespace_comments(
            "-  \n # first comment\n  \r\n  \n  \r # second comment  \n  x not a comment",
            52,
            b'x',
        );
    }

    #[track_caller]
    fn do_test_req_whitespace_comments(input: &str, expect_len: usize, expect_next: u8) {
        let mut p = Parser::new(input);
        p.pos = 1;

        if expect_len > 0 {
            assert!(p.req_whitespace_comments().is_ok());
            assert_eq!(p.pos, expect_len + 1);
        } else {
            assert!(p.req_whitespace_comments().is_err());
            assert_eq!(p.pos, 1);
        }
        assert_eq!(p.peek(), expect_next);
    }

    #[test]
    fn test_req_whitespace_comments() {
        do_test_req_whitespace_comments("-not a comment-", 0, b'n');
        do_test_req_whitespace_comments("-", 0, 0);
        do_test_req_whitespace_comments("-# a comment", 11, 0);
        do_test_req_whitespace_comments("-# a comment\n-", 12, b'-');
        do_test_req_whitespace_comments(
            "-# first comment\n# second comment\nnot a comment",
            33,
            b'n',
        );
        do_test_req_whitespace_comments(
            "-  \n # first comment\n  \r\n  \n  \r # second comment  \n  x not a comment",
            52,
            b'x',
        );
    }
}
