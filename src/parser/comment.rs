use super::{ParseResult, Parser, SyntaxError, chars::WhitespaceDetailed};

impl<'a> Parser<'a> {
    /// Consume as much whitespace and comments as possible.  Return an error
    /// if neither is found.
    pub(super) fn req_whitespace_comments(&mut self) -> ParseResult<()> {
        if self.whitespace_comments() {
            Ok(())
        } else {
            Err(SyntaxError::ExpectedWhitespaceOrComment { pos: self.pos })
        }
    }

    /// Consume as much whitespace and comments as possible.
    // TODO: replace all occurrences with `whitespace_comments_detailed` ? - check performance
    pub(super) fn whitespace_comments(&mut self) -> bool {
        let start = self.pos;
        loop {
            self.whitespace();
            if !self.comment() {
                break;
            }
        }
        self.pos > start
    }

    /// Consume as much whitespace and comments as possible.
    pub(super) fn whitespace_comments_detailed(&mut self) -> WhitespaceDetailed {
        let mut result = WhitespaceDetailed {
            newline: false,
            any: false,
        };

        loop {
            let d = self.whitespace_detailed();
            result.newline |= d.newline;
            result.any |= d.any;
            if self.comment() {
                result.newline = true;
                result.any = true;
            } else {
                break;
            }
        }

        result
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
        let r = p.whitespace_comments();
        assert_eq!(r, expect_len > 0);
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
    fn do_test_whitespace_comments_detailed(
        input: &str,
        expected_end: isize,
        expected_newline: bool,
    ) {
        let mut parser = Parser::new(input);
        parser.pos = 1;
        let actual = parser.whitespace_comments_detailed();
        // assert_eq!(p.pos, expect_len + 1);
        // assert_eq!(p.peek(), expect_next);
        if expected_end > 0 {
            assert_eq!(
                parser.pos, expected_end as usize,
                "assert actual_end ({}) == expected_end ({expected_end})",
                parser.pos
            );
        } else {
            let actual_remain = input.len() - parser.pos;
            let expected_remain = -expected_end as usize;
            assert_eq!(
                actual_remain, expected_remain,
                "assert actual_remain ({actual_remain}) == expected_remaining ({expected_remain})"
            );
        }
        assert_eq!(
            actual,
            WhitespaceDetailed {
                newline: expected_newline,
                any: expected_end != 1
            }
        )
    }

    #[test]
    fn test_whitespace_comments_detailed() {
        do_test_whitespace_comments_detailed("-not a comment-", 1, false);
        do_test_whitespace_comments_detailed("-", 1, false);
        do_test_whitespace_comments_detailed("-# a comment", 0, true);
        do_test_whitespace_comments_detailed("-# a comment\n-", -1, true);
        do_test_whitespace_comments_detailed(
            "-# first comment\n# second comment\nnot a comment",
            -13,
            true,
        );
        do_test_whitespace_comments_detailed(
            "-  \n # first comment\n  \r\n  \n  \r # second comment  \n  x not a comment",
            -15,
            true,
        );
    }

    #[track_caller]
    fn do_test_req_whitespace_comments_ok(input: &str, expected_end: isize) {
        let mut parser = Parser::new(input);
        parser.pos = 1;

        assert!(parser.req_whitespace_comments().is_ok());

        if expected_end > 0 {
            assert_eq!(
                parser.pos, expected_end as usize,
                "assert actual_end ({}) == expected_end ({expected_end})",
                parser.pos
            );
        } else {
            let actual_remain = input.len() - parser.pos;
            let expected_remain = -expected_end as usize;
            assert_eq!(
                actual_remain, expected_remain,
                "assert actual_remain ({actual_remain}) == expected_remaining ({expected_remain})"
            );
        }
    }

    #[track_caller]
    fn do_test_req_whitespace_comments_err(input: &str) {
        let mut p = Parser::new(input);
        p.pos = 1;

        assert_eq!(
            p.req_whitespace_comments()
                .expect_err("expected syntax error"),
            SyntaxError::ExpectedWhitespaceOrComment { pos: 1 }
        );
        assert_eq!(p.pos, 1);
    }

    #[test]
    fn test_req_whitespace_comments() {
        do_test_req_whitespace_comments_err("-not a comment-");
        do_test_req_whitespace_comments_err("-");
        do_test_req_whitespace_comments_ok("-# a comment", 0);
        do_test_req_whitespace_comments_ok("-# a comment\n-", -1);
        do_test_req_whitespace_comments_ok(
            "-# first comment\n# second comment\nnot a comment",
            -13,
        );
        do_test_req_whitespace_comments_ok(
            "-  \n # first comment\n  \r\n  \n  \r # second comment  \n  x not a comment",
            -15,
        );
    }
}
