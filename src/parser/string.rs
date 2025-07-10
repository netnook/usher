use super::{ParseResult, Parser, SyntaxError};
use crate::lang::Value;

const MISSING_END_QUOTE: &str = "Missing closing double quote to end string.";
const CRLF_IN_STRING_NOT_ALLOWED: &str = "Invalid characters CR or LF in string.";
const INVALID_ESCAPE: &str = "Invalid escape sequence.";

impl<'a> Parser<'a> {
    /// Consume a string if next on input and return it.
    /// Otherwise consume nothing and return `None`
    // FIXME - handle interpolation !!!
    pub(super) fn string(&mut self) -> ParseResult<Option<Value>> {
        let start = self.pos;

        if !self.char(b'\"') {
            return Ok(None);
        };

        let mut result = String::new();

        loop {
            let from = self.pos;

            let (_count, peek) =
                self.repeat_and_peek(|c| c != b'\n' && c != b'\r' && c != b'\"' && c != b'\\');

            if peek == b'"' {
                let tmp = String::from_utf8_lossy(&self.input[from..self.pos]);
                result.push_str(&tmp);
                self.pos += 1;
                break;
            }

            if peek == b'\r' || peek == b'\n' {
                return Err(SyntaxError::new(self.pos, CRLF_IN_STRING_NOT_ALLOWED));
            }

            if peek == b'\\' {
                let tmp = String::from_utf8_lossy(&self.input[from..self.pos]);
                result.push_str(&tmp);
                self.pos += 1;

                match self.peek() {
                    b'"' => result.push('"'),
                    b'\\' => result.push('\\'),
                    b'r' => result.push('\r'),
                    b'n' => result.push('\n'),
                    _ => {
                        return Err(SyntaxError::new(self.pos - 1, INVALID_ESCAPE));
                    }
                }

                self.pos += 1;
                continue;
            }

            return Err(SyntaxError::new(start, MISSING_END_QUOTE));
        }

        Ok(Some(Value::Str(result)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::s;
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn do_test_string_ok(input: &str, expected: Option<Value>, len: usize) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(expected, p.string().expect("parse ok"));
        assert_eq!(len + 1, p.pos);
    }

    #[track_caller]
    fn do_test_string_err(input: &str, err_pos: usize, err_msg: &'static str) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(
            SyntaxError {
                pos: err_pos,
                msg: err_msg
            },
            p.string().expect_err("expected error")
        );
    }

    #[test]
    fn test_strings() {
        do_test_string_ok(r#"_"one"_"#, Some(s("one")), 5);
        do_test_string_ok(r#"_"one two"_"#, Some(s("one two")), 9);
        do_test_string_ok(r#"_"\"aa\"\\\r\nbb\""_"#, Some(s("\"aa\"\\\r\nbb\"")), 18);

        do_test_string_ok(r#"_"true"_"#, Some(s("true")), 6);
        do_test_string_ok(r#"_"true"_"#, Some(s("true")), 6);

        do_test_string_ok(r#"_one"_"#, None, 0);

        do_test_string_err(r#"_"one"#, 1, MISSING_END_QUOTE);
        do_test_string_err("_\"on\re", 4, CRLF_IN_STRING_NOT_ALLOWED);
        do_test_string_err(r#"_"aa\xaa""#, 4, INVALID_ESCAPE);
    }
}
