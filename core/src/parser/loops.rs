use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, Break, For, Span};

impl<'a> Parser<'a> {
    // "for" ident "in" expr block
    // "for" ident, ident "in" expr block
    pub(super) fn for_stmt(&mut self, span: Span) -> ParseResult<AstNode> {
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

        let Some("in") = self.identifier_str() else {
            return Err(SyntaxError::LoopExpectedInKeyword { pos: self.pos });
        };

        self.whitespace_comments();

        let Some(iterable) = self.simple_expression()? else {
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
            span: span.extended(self.pos),
        }))
    }

    pub(super) fn break_stmt(&mut self, span: Span) -> ParseResult<AstNode> {
        // already passed "break" when called

        let start = self.pos;

        let linespace_count = self.linespace();

        if linespace_count == 0 {
            return Ok(AstNode::Break(Break { value: None, span }));
        }

        if self.is_eoi() {
            self.pos = start;
            return Ok(AstNode::Break(Break { value: None, span }));
        }

        let peek = self.peek();
        if peek == b'\n' || peek == b'\r' || peek == b'#' {
            self.pos = start;
            return Ok(AstNode::Break(Break { value: None, span }));
        }

        let Some(expr) = self.complex_expression()? else {
            return Err(SyntaxError::ExpectsExpression { pos: self.pos });
        };

        Ok(AstNode::Break(Break {
            span: Span::merge(span, expr.span()),
            value: Some(Box::new(expr)),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::Span,
        parser::{
            stmt::tests::{do_test_stmt_err, do_test_stmt_ok},
            tests::*,
        },
    };

    #[test]
    fn test_for() {
        do_test_stmt_ok(
            " for a in x { 1 } ",
            _for(var("a"), None, var("x"), _block![i(1)]),
            -1,
        );
        do_test_stmt_ok(
            " for a, b in x { 1 } ",
            _for(var("a"), Some(var("b")), var("x"), _block![i(1)]),
            -1,
        );
        do_test_stmt_ok(
            " for a,b in x{1} ",
            _for(var("a"), Some(var("b")), var("x"), _block![i(1)]),
            -1,
        );
        do_test_stmt_ok(
            " for #comment\n a #comment\n , #comment\n b #comment\n in #comment\n x #comment\n {1} ",
            _for(var("a"), Some(var("b")), var("x"), _block![i(1)]),
            -1,
        );

        do_test_stmt_err(
            " for ,a in x { 1 } ",
            SyntaxError::ExpectedVariableIdentifier { pos: 5 },
        );
        do_test_stmt_err(
            " for a{ in x { 1 } ",
            SyntaxError::LoopExpectedInKeyword { pos: 6 },
        );
        do_test_stmt_err(
            " for a,{ in x { 1 } ",
            SyntaxError::ExpectedVariableIdentifier { pos: 7 },
        );
        do_test_stmt_err(
            " for a,b { in x { 1 } ",
            SyntaxError::LoopExpectedInKeyword { pos: 9 },
        );
        do_test_stmt_err(
            " for a in ; x { 1 } ",
            SyntaxError::ExpectsExpression { pos: 10 },
        );
        do_test_stmt_err(
            " for a in x ; { 1 } ",
            SyntaxError::ExpectedBlock { pos: 12 },
        );
        do_test_stmt_err(
            " for else in x { 1 } ",
            SyntaxError::ReservedKeyword {
                got: "else".to_string(),
                span: Span::new(5, 4),
            },
        );
        do_test_stmt_err(
            " for a, print in x { 1 } ",
            SyntaxError::ReservedName {
                got: "print".to_string(),
                span: Span::new(8, 5),
            },
        );
    }

    #[test]
    fn test_break() {
        do_test_stmt_ok(r" break ", brk!(), -1);
        do_test_stmt_ok(" break 42 ", brk!(i(42)), -1);
        do_test_stmt_ok(" break 42 + 3 ", brk!(add(i(42), i(3))), -1);
        do_test_stmt_ok(" break 42 \n +3 ", brk!(i(42)), 9);
        do_test_stmt_err(" break continue", SyntaxError::ExpectsExpression { pos: 7 });
    }
}
