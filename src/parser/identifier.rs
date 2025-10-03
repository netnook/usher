use super::{
    KEYWORDS, ParseResult, Parser, RESERVED_NAMES, SyntaxError,
    chars::{is_alpha, is_alphanumeric},
};
use crate::lang::{Identifier, Span, Var};

#[derive(Debug)]
pub struct UncheckedIdentifier<'a>(pub(crate) &'a str, pub(crate) Span);

impl<'a> Parser<'a> {
    /// Consume a identifier if next on input and return it.  Checks that identifier is
    /// not one of the reserved words.
    /// Otherwise consume nothing and return `None`
    // FIXME - handle non-ascii chars !!!
    pub(super) fn declaration_identifier(&mut self) -> ParseResult<Option<Var>> {
        let Some(UncheckedIdentifier(id, span)) = self.unchecked_identifier() else {
            return Ok(None);
        };

        // let ident = String::from_utf8_lossy(ident).to_string();

        if KEYWORDS.contains(&id) {
            return Err(SyntaxError::ReservedKeyword {
                got: id.to_string(),
                span,
            });
        }
        if RESERVED_NAMES.contains(&id) {
            return Err(SyntaxError::ReservedName {
                got: id.to_string(),
                span,
            });
        }

        Ok(Some(Var::new(Identifier::new(id.to_string(), span))))
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
