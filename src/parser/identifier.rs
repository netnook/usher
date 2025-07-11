use super::{
    ParseResult, Parser, RESERVED_KEYWORDS, SyntaxError,
    chars::{is_alpha, is_alphanumeric},
};
use crate::lang::Identifier;

pub(super) const KEYWORD_RESERVED: &str = "Keyword reserved and may not be used as identifier.";

impl<'a> Parser<'a> {
    /// Consume a identifie if next on input and return it.
    /// Otherwise consume nothing and return `None`
    pub(super) fn identifier(&mut self) -> ParseResult<Option<Identifier>> {
        let start = self.pos;

        if self.repeat(is_alpha) < 1 {
            return Ok(None);
        }

        self.repeat(|c| is_alphanumeric(c) || c == b'_');

        let end = self.pos;

        let ident = String::from_utf8_lossy(&self.input[start..end]);

        if RESERVED_KEYWORDS.contains(&&ident[..]) {
            return Err(SyntaxError::new(start, KEYWORD_RESERVED));
        }

        Ok(Some(Identifier::new(&ident)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::ident;
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn do_test_ident_ok(input: &str, expected: Option<Identifier>, len: usize) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(expected, p.identifier().expect("parse ok"));
        assert_eq!(len + 1, p.pos);
    }

    #[track_caller]
    fn do_test_ident_err(input: &str, err_pos: usize, err_msg: &'static str) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(
            SyntaxError {
                pos: err_pos,
                msg: err_msg
            },
            p.identifier().expect_err("expected error")
        );
    }

    #[test]
    fn test_identifiers() {
        do_test_ident_ok("-one_TwO33_-", Some(ident("one_TwO33_")), 10);
        do_test_ident_ok("-one_TwO33_ ", Some(ident("one_TwO33_")), 10);
        do_test_ident_ok("-one_TwO33_", Some(ident("one_TwO33_")), 10);
        do_test_ident_ok("-o-", Some(ident("o")), 1);
        do_test_ident_ok("-1-", None, 0);
        do_test_ident_ok("-_-", None, 0);

        do_test_ident_err("-print-", 1, KEYWORD_RESERVED);
    }
}
