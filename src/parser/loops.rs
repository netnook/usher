use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, For};

impl<'a> Parser<'a> {
    // "for" ident "in" expr block
    // "for" ident, ident "in" expr block
    pub(super) fn for_stmt(&mut self) -> ParseResult<AstNode> {
        // already passed "for" when called

        self.req_whitespace_comments()?;

        let Some(loop_item) = self.declaration_identifier()? else {
            return Err(SyntaxError::ExpectedVariableIdentifier { pos: self.pos });
        };

        let mut loop_info = None;

        self.whitespace_comments();

        if self.char(b',') {
            self.whitespace_comments();
            loop_info = self.declaration_identifier()?;

            if loop_info.is_none() {
                return Err(SyntaxError::ExpectedVariableIdentifier { pos: self.pos });
            };

            self.whitespace_comments();
        }

        if self.unchecked_identifier() != Some("in") {
            return Err(SyntaxError::LoopExpectedInKeyword { pos: self.pos });
        };

        self.whitespace_comments();

        let Some(iterable) = self.expression()? else {
            return Err(SyntaxError::ExpectsExpression { pos: self.pos });
        };

        self.whitespace_comments();

        let Some(block) = self.block()? else {
            return Err(SyntaxError::ExpectedBlock { pos: self.pos });
        };

        Ok(AstNode::For(For {
            iterable: iterable.into(),
            loop_item,
            loop_info,
            block,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lang::Span, parser::tests::*};

    #[track_caller]
    fn do_test_for_ok(input: &'static str, expected: For, expected_end: isize) {
        do_test_parser_ok(Parser::stmt, input, Some(expected.into()), expected_end);
    }

    #[track_caller]
    fn do_test_for_err(input: &'static str, expected_err: SyntaxError) {
        do_test_parser_err(Parser::stmt, input, expected_err);
    }

    #[test]
    fn test_for() {
        do_test_for_ok(
            " for a in x { 1 } ",
            _for(var("a"), None, var("x"), _block![i(1)]),
            -1,
        );
        do_test_for_ok(
            " for a, b in x { 1 } ",
            _for(var("a"), Some(var("b")), var("x"), _block![i(1)]),
            -1,
        );
        do_test_for_ok(
            " for a,b in x{1} ",
            _for(var("a"), Some(var("b")), var("x"), _block![i(1)]),
            -1,
        );
        do_test_for_ok(
            " for #comment\n a #comment\n , #comment\n b #comment\n in #comment\n x #comment\n {1} ",
            _for(var("a"), Some(var("b")), var("x"), _block![i(1)]),
            -1,
        );

        do_test_for_err(
            " for ,a in x { 1 } ",
            SyntaxError::ExpectedVariableIdentifier { pos: 5 },
        );
        do_test_for_err(
            " for a{ in x { 1 } ",
            SyntaxError::LoopExpectedInKeyword { pos: 6 },
        );
        do_test_for_err(
            " for a,{ in x { 1 } ",
            SyntaxError::ExpectedVariableIdentifier { pos: 7 },
        );
        do_test_for_err(
            " for a,b { in x { 1 } ",
            SyntaxError::LoopExpectedInKeyword { pos: 9 },
        );
        do_test_for_err(
            " for a in ; x { 1 } ",
            SyntaxError::ExpectsExpression { pos: 10 },
        );
        do_test_for_err(
            " for a in x ; { 1 } ",
            SyntaxError::ExpectedBlock { pos: 12 },
        );
        do_test_for_err(
            " for else in x { 1 } ",
            SyntaxError::ReservedKeyword {
                got: "else".to_string(),
                span: Span::new(5, 4),
            },
        );
        do_test_for_err(
            " for a, print in x { 1 } ",
            SyntaxError::ReservedName {
                got: "print".to_string(),
                span: Span::new(8, 5),
            },
        );
    }
}
