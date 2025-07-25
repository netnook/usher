use super::{
    ParseResult, Parser, RESERVED_KEYWORDS, SyntaxError,
    chars::{is_alpha, is_alphanumeric},
};
use crate::lang::Identifier;

pub(super) const KEYWORD_RESERVED: &str = "Keyword reserved and may not be used as identifier.";

impl<'a> Parser<'a> {
    /// Consume a identifier if next on input and return it.  Checks that identifier is
    /// not one of the reserved words.
    /// Otherwise consume nothing and return `None`
    // FIXME - handle non-ascii chars !!!
    pub(super) fn identifier(&mut self) -> ParseResult<Option<Identifier>> {
        let start = self.pos;

        let Some(ident) = self.unchecked_identifier() else {
            return Ok(None);
        };

        let ident = String::from_utf8_lossy(ident).to_string();

        if RESERVED_KEYWORDS.contains(&ident.as_str()) {
            return Err(SyntaxError::new(start, KEYWORD_RESERVED));
        }

        Ok(Some(Identifier::new(ident)))
    }

    /// Consume a identifier if next on input and return it.
    /// Otherwise consume nothing and return `None`
    // FIXME - handle non-ascii chars !!!
    pub(super) fn unchecked_identifier(&mut self) -> Option<&[u8]> {
        let start = self.pos;

        if self.repeat(is_alpha) < 1 {
            return None;
        }

        self.repeat(|c| is_alphanumeric(c) || c == b'_');

        let end = self.pos;

        Some(&self.input[start..end])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::{do_test_parser_err, do_test_parser_none, do_test_parser_some, id};

    #[test]
    fn test_identifiers() {
        do_test_parser_some(Parser::identifier, "-one_TwO33_-", id("one_TwO33_"), -1);
        do_test_parser_some(Parser::identifier, "-one_TwO33_ ", id("one_TwO33_"), -1);
        do_test_parser_some(Parser::identifier, "-one_TwO33_", id("one_TwO33_"), 0);
        do_test_parser_some(Parser::identifier, "-o-", id("o"), -1);

        do_test_parser_none(Parser::identifier, "-1-");
        do_test_parser_none(Parser::identifier, "-_-");

        do_test_parser_err(Parser::identifier, "-print-", 1, KEYWORD_RESERVED);
    }
}
