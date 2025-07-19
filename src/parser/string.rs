use super::{ParseResult, Parser, SyntaxError};
use crate::lang::Value;

pub(super) const MISSING_END_QUOTE: &str = "Missing closing double quote to end string.";
pub(super) const CRLF_IN_STRING_NOT_ALLOWED: &str = "Invalid characters CR or LF in string.";
pub(super) const INVALID_ESCAPE: &str = "Invalid escape sequence.";

impl<'a> Parser<'a> {
    /// Consume a string if next on input and return it.
    /// Otherwise consume nothing and return `None`
    // FIXME - handle interpolation !!!
    // FIXME - handle non-ascii chars !!!
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
    use crate::parser::tests::{do_test_parser_err, do_test_parser_none, do_test_parser_some, s};

    #[test]
    fn test_strings() {
        do_test_parser_some(Parser::string, r#"_"one"_"#, s("one"), -1);
        do_test_parser_some(Parser::string, r#"_"one two"_"#, s("one two"), -1);
        do_test_parser_some(
            Parser::string,
            r#"_"\"aa\"\\\r\nbb\""_"#,
            s("\"aa\"\\\r\nbb\""),
            -1,
        );

        do_test_parser_some(Parser::string, r#"_"true"_"#, s("true"), -1);
        do_test_parser_some(Parser::string, r#"_"true"_"#, s("true"), -1);

        do_test_parser_none(Parser::string, r#"_one"_"#);

        do_test_parser_err(Parser::string, r#"_"one"#, 1, MISSING_END_QUOTE);
        do_test_parser_err(Parser::string, "_\"on\re", 4, CRLF_IN_STRING_NOT_ALLOWED);
        do_test_parser_err(Parser::string, r#"_"aa\xaa""#, 4, INVALID_ESCAPE);
    }
}
