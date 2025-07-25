use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, Block};

pub(crate) const MISSING_BLOCK_END: &str = "Missing closing brace to end block.";
pub(crate) const EXPECTED_NEW_LINE_AFTER_STMT: &str = "Expected new line after statement.";
pub(crate) const EXPECTED_STATEMENT: &str = "Expected statement.";
pub(crate) const EXPECTED_EXPRESSION_ON_RHS: &str = "Expected expression on RHS of assignment.";
pub(crate) const INVALID_LHS_OF_ASSIGNMENT: &str = "Invalid LHS of assignment.";

impl<'a> Parser<'a> {
    pub(super) fn stmt(&mut self) -> ParseResult<Option<AstNode>> {
        if let Some(res) = self.if_stmt()? {
            return Ok(Some(res));
        }
        if let Some(res) = self.for_stmt()? {
            return Ok(Some(res));
        }
        if let Some(res) = self.var_stmt()? {
            return Ok(Some(res));
        }
        if let Some(res) = self.assignment_or_expression()? {
            return Ok(Some(res));
        }
        // FIXME: binary run exppression
        // FIXME: function declaration statement

        Ok(None)
    }

    pub(super) fn block(&mut self) -> ParseResult<Option<Block>> {
        let start = self.pos;

        if !self.char(b'{') {
            return Ok(None);
        }

        let mut stmts = Vec::new();

        let mut first = true;
        loop {
            let details = self.whitespace_comments_detailed();

            if self.char(b'}') {
                break;
            }

            if self.is_eoi() {
                return Err(SyntaxError::new(start, MISSING_BLOCK_END));
            }

            if first {
                first = false;
            } else if !details.newline {
                return Err(SyntaxError::new(self.pos, EXPECTED_NEW_LINE_AFTER_STMT));
            }

            let Some(stmt) = self.stmt()? else {
                return Err(SyntaxError::new(self.pos, EXPECTED_STATEMENT));
            };

            stmts.push(stmt);
        }

        Ok(Some(Block { stmts }))
    }

    pub(super) fn assignment_or_expression(&mut self) -> ParseResult<Option<AstNode>> {
        let start = self.pos;

        let Some(expr) = self.expression()? else {
            return Ok(None);
        };

        let savepos = self.pos;
        self.linespace();

        if self.char(b'=') {
            self.whitespace_comments();

            let lhs = expr;
            let pass = matches!(
                lhs,
                AstNode::Identifier(_)
                    | AstNode::PropertyOf {
                        from: _,
                        property: _
                    }
                    | AstNode::IndexOf { from: _, index: _ }
            );
            if !pass {
                return Err(SyntaxError {
                    pos: start,
                    msg: INVALID_LHS_OF_ASSIGNMENT,
                });
            }

            let Some(rhs) = self.expression()? else {
                return Err(SyntaxError {
                    pos: self.pos,
                    msg: EXPECTED_EXPRESSION_ON_RHS,
                });
            };

            Ok(Some(AstNode::Assignment(lhs.into(), rhs.into())))
        } else {
            self.pos = savepos;
            Ok(Some(expr))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::*;

    #[track_caller]
    fn do_test_block_ok(input: &'static str, expected: Block, expected_end: isize) {
        do_test_parser_ok(Parser::block, input, Some(expected), expected_end);
    }
    #[track_caller]
    fn do_test_block_err(
        input: &'static str,
        expected_err_pos: usize,
        expected_err_msg: &'static str,
    ) {
        do_test_parser_err(Parser::block, input, expected_err_pos, expected_err_msg);
    }

    #[test]
    fn test_block() {
        do_test_block_ok(" {1} ", _block![i(1)], -1);
        do_test_block_ok(" { 1 } ", _block![i(1)], -1);
        do_test_block_ok(" { 1 \n 2 \n 3 } ", _block![i(1), i(2), i(3)], -1);
        do_test_block_ok(" { \n 1 \n 2 \n 3 \n } ", _block![i(1), i(2), i(3)], -1);
        do_test_block_ok(" { #foo\n 1 #bar\n #baz \n 2 } ", _block![i(1), i(2)], -1);

        do_test_block_err(" { 1 ", 1, MISSING_BLOCK_END);
        do_test_block_err(" { 1 2 } ", 5, EXPECTED_NEW_LINE_AFTER_STMT);
        do_test_block_err(" { 1 \n ; } ", 7, EXPECTED_STATEMENT);
    }

    #[track_caller]
    fn do_test_assign_or_expr_ok(input: &'static str, expected: AstNode, expected_end: isize) {
        do_test_parser_ok(
            Parser::assignment_or_expression,
            input,
            Some(expected),
            expected_end,
        );
    }

    #[test]
    fn test_assignment_or_expr() {
        do_test_assign_or_expr_ok(" a ", id("a").into(), -1);
        do_test_assign_or_expr_ok(" a = 1 + 2 ", assign(id("a"), add(i(1), i(2))), -1);
        do_test_assign_or_expr_ok(
            " a[3].b.c = 1 + 2 ",
            assign(
                prop_of(prop_of(index_of(id("a"), i(3)), "b"), "c"),
                add(i(1), i(2)),
            ),
            -1,
        );
    }

    #[track_caller]
    fn do_test_assign_or_expr_err(
        input: &'static str,
        expected_err_pos: usize,
        expected_err_msg: &'static str,
    ) {
        do_test_parser_err(
            Parser::assignment_or_expression,
            input,
            expected_err_pos,
            expected_err_msg,
        );
    }

    #[test]
    fn test_assign_or_expr_err() {
        do_test_assign_or_expr_err(" a = ;", 5, EXPECTED_EXPRESSION_ON_RHS);
        do_test_assign_or_expr_err(" a + b = 3", 1, INVALID_LHS_OF_ASSIGNMENT);
    }
}
