use super::{
    KEYWORDS, ParseResult, Parser, RESERVED_NAMES, SyntaxError,
    chars::{is_alpha, is_alphanumeric},
};
use crate::lang::{Identifier, Span, Var};

#[derive(Debug)]
pub struct UncheckedIdentifier<'a>(pub(crate) &'a str, pub(crate) Span);

impl<'a> UncheckedIdentifier<'a> {
    pub(super) fn check_not_reserved_name(&self) -> ParseResult<()> {
        let id = self.0;
        if RESERVED_NAMES.contains(&id) {
            return Err(SyntaxError::ReservedName {
                got: id.to_string(),
                span: self.1,
            });
        }

        Ok(())
    }

    pub(super) fn check_not_keyword(&self) -> ParseResult<()> {
        let id = self.0;
        if KEYWORDS.contains(&id) {
            return Err(SyntaxError::ReservedKeyword {
                got: id.to_string(),
                span: self.1,
            });
        }
        Ok(())
    }
}

impl<'a> Parser<'a> {
    /// Consume a identifier if next on input and return it.  Checks that identifier is
    /// not one of the reserved words.
    /// Otherwise consume nothing and return `None`
    // FIXME - handle non-ascii chars !!!
    pub(super) fn declaration_identifier(&mut self) -> ParseResult<Option<Var>> {
        let Some(id) = self.unchecked_identifier() else {
            return Ok(None);
        };

        id.check_not_keyword()?;
        id.check_not_reserved_name()?;

        Ok(Some(Var::new(Identifier::new(id.0.to_string(), id.1))))
    }

    /// Consume a identifier if next on input and return it.
    /// Otherwise consume nothing and return `None`
    // FIXME - handle non-ascii chars !!!
    pub(super) fn unchecked_identifier(&mut self) -> Option<UncheckedIdentifier<'a>> {
        let start = self.pos;

        if self.repeat(is_alpha) < 1 {
            return None;
        }

        self.repeat(|c| is_alphanumeric(c) || c == b'_');

        let end = self.pos;

        let ident = std::str::from_utf8(&self.input[start..end]).expect("to str ok");

        Some(UncheckedIdentifier(ident, Span::start_end(start, end)))
    }

    /// Consume a identifier if next on input and return it.
    /// Otherwise consume nothing and return `None`
    // FIXME - handle non-ascii chars !!!
    pub(super) fn identifier_str(&mut self) -> Option<&'a str> {
        let start = self.pos;

        if self.repeat(is_alpha) < 1 {
            return None;
        }

        self.repeat(|c| is_alphanumeric(c) || c == b'_');

        let end = self.pos;

        let ident = std::str::from_utf8(&self.input[start..end]).expect("to str ok");

        Some(ident)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::{
        do_test_parser_err, do_test_parser_none_t, do_test_parser_some_t, id, var,
    };

    #[test]
    fn test_identifiers() {
        let f = Parser::declaration_identifier;
        do_test_parser_some_t(f, "-one_TwO33_-", var(id("one_TwO33_").spanned(1, 10)), -1);
        do_test_parser_some_t(f, "-one_TwO33_ ", var(id("one_TwO33_").spanned(1, 10)), -1);
        do_test_parser_some_t(f, "-one_TwO33_", var(id("one_TwO33_").spanned(1, 10)), 0);
        do_test_parser_some_t(f, "-o-", var(id("o").spanned(1, 1)), -1);

        do_test_parser_none_t(f, "-1-");
        do_test_parser_none_t(f, "-_-");

        do_test_parser_err(
            f,
            "-print-",
            SyntaxError::ReservedName {
                got: "print".to_string(),
                span: Span::new(1, 5),
            },
        );
        do_test_parser_err(
            f,
            "-for-",
            SyntaxError::ReservedKeyword {
                got: "for".to_string(),
                span: Span::new(1, 3),
            },
        );
    }
}
