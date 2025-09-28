use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, ReturnStmt};

impl<'a> Parser<'a> {
    pub(super) fn return_stmt(&mut self) -> ParseResult<AstNode> {
        // already passed "return" when called

        let start = self.pos;

        let linespace_count = self.linespace();

        if linespace_count == 0 {
            return Ok(AstNode::ReturnStmt(ReturnStmt { value: None }));
        }

        if self.is_eoi() {
            self.pos = start;
            return Ok(AstNode::ReturnStmt(ReturnStmt { value: None }));
        }

        let peek = self.peek();
        if peek == b'\n' || peek == b'\r' || peek == b'#' {
            self.pos = start;
            return Ok(AstNode::ReturnStmt(ReturnStmt { value: None }));
        }

        let Some(expr) = self.expression()? else {
            return Err(SyntaxError::ExpectsExpression { pos: self.pos });
        };

        Ok(AstNode::ReturnStmt(ReturnStmt {
            value: Some(expr.into()),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::*;

    #[track_caller]
    fn do_test_return_ok(input: &'static str, expected: AstNode, expected_end: isize) {
        do_test_parser_ok(Parser::stmt, input, Some(expected), expected_end);
    }

    #[track_caller]
    fn do_test_return_err(input: &'static str, expected_err: SyntaxError) {
        do_test_parser_err(Parser::stmt, input, expected_err);
    }

    #[test]
    fn test_return() {
        do_test_return_ok(r" return ", _ret!(), -1);
        do_test_return_ok(" return 42 ", _ret!(i(42)), -1);
        do_test_return_ok(" return 42 + 3 ", _ret!(add(i(42), i(3))), -1);
        do_test_return_ok(" return 42 \n +3 ", _ret!(i(42)), 10);
        do_test_return_err(" return { 42 } ", SyntaxError::ExpectsExpression { pos: 8 });
    }
}
