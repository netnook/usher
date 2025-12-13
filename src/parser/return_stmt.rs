use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, ReturnStmt, Span};

impl<'a> Parser<'a> {
    pub(super) fn return_stmt(&mut self, span: Span) -> ParseResult<AstNode> {
        // already passed "return" when called

        let start = self.pos;

        let linespace_count = self.linespace();

        if linespace_count == 0 {
            return Ok(AstNode::ReturnStmt(ReturnStmt { value: None, span }));
        }

        if self.is_eoi() {
            self.pos = start;
            return Ok(AstNode::ReturnStmt(ReturnStmt { value: None, span }));
        }

        let peek = self.peek();
        if peek == b'\n' || peek == b'\r' || peek == b'#' {
            self.pos = start;
            return Ok(AstNode::ReturnStmt(ReturnStmt { value: None, span }));
        }

        let Some(expr) = self.expression()? else {
            return Err(SyntaxError::ExpectsExpression { pos: self.pos });
        };

        Ok(AstNode::ReturnStmt(ReturnStmt {
            span: Span::merge(span, expr.span()),
            value: Some(expr.into()),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{
        stmt::tests::{do_test_stmt_err, do_test_stmt_ok},
        tests::*,
    };

    #[test]
    fn test_return() {
        do_test_stmt_ok(r" return ", ret!(), -1);
        do_test_stmt_ok(" return 42 ", ret!(i(42)), -1);
        do_test_stmt_ok(" return 42 + 3 ", ret!(add(i(42), i(3))), -1);
        do_test_stmt_ok(" return 42 \n +3 ", ret!(i(42)), 10);
        do_test_stmt_err(" return { 42 } ", SyntaxError::ExpectsExpression { pos: 8 });
    }
}
