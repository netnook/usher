use super::{ParseResult, Parser, SyntaxError};
use crate::lang::Value;

const MISSING_END_QUOTE: &str = "Missing closing double quote to end string.";

impl<'a> Parser<'a> {
    /// Consume a string if next on input and return it.
    /// Otherwise consume nothing and return `None`
    // FIXME - handle escaping !!!
    // FIXME - handle interpolation !!!
    pub(super) fn string(&mut self) -> ParseResult<Option<Value>> {
        let start = self.pos;

        if !self.char(b'\"') {
            return Ok(None);
        };

        self.repeat(|c| c != b'\"');

        if !self.char(b'\"') {
            return Err(SyntaxError::new(start, MISSING_END_QUOTE));
        };

        let end = self.pos;

        let s = String::from_utf8_lossy(&self.input[start + 1..end - 1]).to_string();

        Ok(Some(Value::Str(s)))
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
    fn do_test_string_err(input: &str) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(
            SyntaxError {
                pos: 1,
                msg: MISSING_END_QUOTE
            },
            p.string().expect_err("expected error")
        );
    }

    #[test]
    fn test_strings() {
        do_test_string_ok(r#"_"one"_"#, Some(s("one")), 5);
        do_test_string_ok(r#"_"one two"_"#, Some(s("one two")), 9);
        do_test_string_ok(r#"_"true"_"#, Some(s("true")), 6);
        do_test_string_ok(r#"_"true"_"#, Some(s("true")), 6);
        do_test_string_ok(r#"_one"_"#, None, 0);

        do_test_string_err(r#"_"one"#);
    }
}
