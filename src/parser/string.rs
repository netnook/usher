use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, InterpolatedStr, Literal, Span, Value};

pub(super) const MISSING_END_QUOTE: &str = "Missing closing double quote to end string.";
pub(super) const CRLF_IN_STRING_NOT_ALLOWED: &str = "Invalid characters CR or LF in string.";
pub(super) const INVALID_ESCAPE: &str = "Invalid escape sequence.";
pub(super) const INVALID_EXPRESSION: &str = "Invalid expression.";
pub(super) const MISSING_MATCHING_CLOSING_BRACE: &str =
    "Expected closing brace '}' after interpolation expression.";

impl<'a> Parser<'a> {
    /// Consume a string if next on input and return it.
    /// Otherwise consume nothing and return `None`
    // FIXME - handle non-ascii chars !!!
    pub(super) fn string(&mut self) -> ParseResult<Option<AstNode>> {
        let start = self.pos;

        if !self.char(b'\"') {
            return Ok(None);
        };

        let mut interpolator_result = Vec::new();
        let mut literal = String::new();
        let mut literal_start = self.pos;

        loop {
            let loop_start = self.pos;

            let (_count, peek) = self.repeat_and_peek(|c| {
                c != b'\n' && c != b'\r' && c != b'\"' && c != b'\\' && c != b'{'
            });

            if peek == b'\r' || peek == b'\n' {
                return Err(SyntaxError::new(self.pos, CRLF_IN_STRING_NOT_ALLOWED));
            }

            if peek == b'\\' {
                let tmp = String::from_utf8_lossy(&self.input[loop_start..self.pos]);
                literal.push_str(&tmp);
                self.pos += 1;

                match self.peek() {
                    b'"' => literal.push('"'),
                    b'\\' => literal.push('\\'),
                    b'r' => literal.push('\r'),
                    b'n' => literal.push('\n'),
                    b'{' => literal.push('{'),
                    b'}' => literal.push('}'),
                    _ => {
                        return Err(SyntaxError::new(self.pos - 1, INVALID_ESCAPE));
                    }
                }

                self.pos += 1;
                continue;
            }

            if peek == b'"' {
                let tmp = String::from_utf8_lossy(&self.input[loop_start..self.pos]);
                literal.push_str(&tmp);
                self.pos += 1;

                if interpolator_result.is_empty() {
                    return Ok(Some(AstNode::Literal(Literal::new(
                        Value::Str(literal),
                        Span::new(start, self.pos - start),
                    ))));
                } else {
                    if !literal.is_empty() {
                        interpolator_result.push(AstNode::Literal(Literal::new(
                            Value::Str(literal),
                            Span::new(literal_start, self.pos - 1 - literal_start),
                        )));
                    }
                    return Ok(Some(AstNode::InterpolatedStr(InterpolatedStr {
                        parts: interpolator_result,
                    })));
                }
            }

            if peek == b'{' {
                let part_start = self.pos;

                if part_start > literal_start {
                    let tmp = String::from_utf8_lossy(&self.input[literal_start..self.pos]);
                    literal.push_str(&tmp);
                    interpolator_result.push(AstNode::Literal(Literal::new(
                        Value::Str(literal),
                        Span::new(literal_start, self.pos - literal_start),
                    )));
                }
                self.pos += 1;

                self.linespace();
                let savepoint = self.pos;
                let Some(expr) = self.expression()? else {
                    return Err(SyntaxError::new(savepoint, INVALID_EXPRESSION));
                };

                interpolator_result.push(expr);

                self.linespace();

                if !self.char(b'}') {
                    return Err(SyntaxError::new(part_start, MISSING_MATCHING_CLOSING_BRACE));
                }

                literal = String::new();
                literal_start = self.pos;

                continue;
            }

            return Err(SyntaxError::new(start, MISSING_END_QUOTE));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::*;

    #[track_caller]
    fn do_test_strings(input: &'static str, expected: impl Into<AstNode>, expected_end: isize) {
        do_test_parser_some(Parser::string, input, expected.into(), expected_end);
    }
    #[track_caller]
    fn do_test_strings_err(
        input: &'static str,
        expected_err_pos: usize,
        expected_err_msg: &'static str,
    ) {
        do_test_parser_err(Parser::string, input, expected_err_pos, expected_err_msg);
    }

    #[test]
    fn test_strings() {
        do_test_strings(r#"_""_"#, s!(""), -1);
        do_test_strings(r#"_"one"_"#, s!("one"), -1);
        do_test_strings(r#"_"one two"_"#, s!("one two"), -1);
        do_test_strings(r#"_"\"aa\"\\\r\nbb\""_"#, s!("\"aa\"\\\r\nbb\""), -1);
        do_test_strings(r#"_"\"aa\"\\\r\nb\{\}b\""_"#, s!("\"aa\"\\\r\nb{}b\""), -1);

        do_test_strings(r#"_"true"_"#, s!("true"), -1);
        do_test_strings(r#"_"true"_"#, s!("true"), -1);

        do_test_strings(r#"_"{ foo }"_"#, _interp![id!("foo")], -1);
        do_test_strings(
            r#"_"ab{ foo -45 }cd{ 35 }"_"#,
            _interp![s!("ab"), sub(id!("foo"), i!(45)), s!("cd"), i!(35)],
            -1,
        );

        do_test_parser_none(Parser::string, r#"_one"_"#);

        do_test_strings_err(r#"_"one"#, 1, MISSING_END_QUOTE);
        do_test_strings_err("_\"on\re", 4, CRLF_IN_STRING_NOT_ALLOWED);
        do_test_strings_err(r#"_"aa\xaa""#, 4, INVALID_ESCAPE);
        do_test_strings_err(r#"_"aa{aa"_"#, 4, MISSING_MATCHING_CLOSING_BRACE);
        do_test_strings_err(r#"_"aa{,}"_"#, 5, INVALID_EXPRESSION);
    }
}
