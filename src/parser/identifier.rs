use super::{
    KEYWORDS, ParseResult, Parser, RESERVED_NAMES, SyntaxError,
    chars::{is_alpha, is_alphanumeric},
};
use crate::lang::{Identifier, Span};

pub(super) const KEYWORD_RESERVED: &str = "Keywords may not be used as identifier.";
pub(super) const NAME_RESERVED: &str = "Reserved name cannot be used for declarations.";

impl<'a> Parser<'a> {
    /// Consume a identifier if next on input and return it.  Checks that identifier is
    /// not one of the reserved words.
    /// Otherwise consume nothing and return `None`
    // FIXME - handle non-ascii chars !!!
    pub(super) fn declaration_identifier(&mut self) -> ParseResult<Option<Identifier>> {
        let start = self.pos;

        let Some(ident) = self.unchecked_identifier() else {
            return Ok(None);
        };

        // let ident = String::from_utf8_lossy(ident).to_string();

        if KEYWORDS.contains(&ident) {
            return Err(SyntaxError::new(start, KEYWORD_RESERVED));
        }
        if RESERVED_NAMES.contains(&ident) {
            return Err(SyntaxError::new(start, NAME_RESERVED));
        }

        Ok(Some(Identifier::new(
            ident.to_string(),
            Span::new(start, ident.len()),
        )))
    }

    /// Consume a identifier if next on input and return it.
    /// Otherwise consume nothing and return `None`
    // FIXME - handle non-ascii chars !!!
    pub(super) fn unchecked_identifier(&mut self) -> Option<&str> {
        let start = self.pos;

        if self.repeat(is_alpha) < 1 {
            return None;
        }

        self.repeat(|c| is_alphanumeric(c) || c == b'_');

        let end = self.pos;

        let ident = std::str::from_utf8(&self.input[start..end]).expect("to str ok");

        Some(ident)
        // Some(&self.input[start..end])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::{
        do_test_parser_err, do_test_parser_none_t, do_test_parser_some_t, id,
    };

    #[test]
    fn test_identifiers() {
        let f = Parser::declaration_identifier;
        do_test_parser_some_t(f, "-one_TwO33_-", id("one_TwO33_").spanned(1, 10), -1);
        do_test_parser_some_t(f, "-one_TwO33_ ", id("one_TwO33_").spanned(1, 10), -1);
        do_test_parser_some_t(f, "-one_TwO33_", id("one_TwO33_").spanned(1, 10), 0);
        do_test_parser_some_t(f, "-o-", id("o").spanned(1, 1), -1);

        do_test_parser_none_t(f, "-1-");
        do_test_parser_none_t(f, "-_-");

        do_test_parser_err(f, "-print-", 1, NAME_RESERVED);
        do_test_parser_err(f, "-for-", 1, KEYWORD_RESERVED);
    }
}
