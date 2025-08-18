use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, ForStmt};

const EXPECTED_VARIABLE: &str = "Expected loop variable after 'for'.";
const EXPECTED_2ND_VARIABLE: &str = "Expected second loop variable after comma.";
const EXPECTED_IN: &str = "Expected 'in' after variable(s).";
const EXPECTED_EXPRESSION: &str = "Expected expression after 'in'.";
const EXPECTED_BLOCK: &str = "Expected loop block after expression.";

impl<'a> Parser<'a> {
    // "for" ident "in" expr block
    // "for" ident, ident "in" expr block
    pub(super) fn for_stmt(&mut self) -> ParseResult<AstNode> {
        // already passed "for" when called

        self.req_whitespace_comments()?;

        let Some(loop_var_1) = self.declaration_identifier()? else {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_VARIABLE,
            });
        };

        let mut loop_var_2 = None;

        self.whitespace_comments();

        if self.char(b',') {
            self.whitespace_comments();
            loop_var_2 = self.declaration_identifier()?;

            if loop_var_2.is_none() {
                return Err(SyntaxError {
                    pos: self.pos,
                    msg: EXPECTED_2ND_VARIABLE,
                });
            };

            self.whitespace_comments();
        }

        if self.unchecked_identifier() != Some("in") {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_IN,
            });
        };

        self.whitespace_comments();

        let Some(loop_expr) = self.expression()? else {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_EXPRESSION,
            });
        };

        self.whitespace_comments();

        let Some(block) = self.block()? else {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_BLOCK,
            });
        };

        Ok(AstNode::ForStmt(ForStmt {
            loop_var_1,
            loop_var_2,
            loop_expr: loop_expr.into(),
            block,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{
        identifier::{KEYWORD_RESERVED, NAME_RESERVED},
        tests::*,
    };

    #[track_caller]
    fn do_test_for_ok(input: &'static str, expected: ForStmt, expected_end: isize) {
        do_test_parser_ok(Parser::stmt, input, Some(expected.into()), expected_end);
    }

    #[track_caller]
    fn do_test_for_err(
        input: &'static str,
        expected_err_pos: usize,
        expected_err_msg: &'static str,
    ) {
        do_test_parser_err(Parser::stmt, input, expected_err_pos, expected_err_msg);
    }

    #[test]
    fn test_for() {
        do_test_for_ok(
            " for a in x { 1 } ",
            _for(id!("a"), None, id!("x"), _block![i!(1)]),
            -1,
        );
        do_test_for_ok(
            " for a, b in x { 1 } ",
            _for(id!("a"), Some(id!("b")), id!("x"), _block![i!(1)]),
            -1,
        );
        do_test_for_ok(
            " for a,b in x{1} ",
            _for(id!("a"), Some(id!("b")), id!("x"), _block![i!(1)]),
            -1,
        );
        do_test_for_ok(
            " for #comment\n a #comment\n , #comment\n b #comment\n in #comment\n x #comment\n {1} ",
            _for(id!("a"), Some(id!("b")), id!("x"), _block![i!(1)]),
            -1,
        );

        do_test_for_err(" for ,a in x { 1 } ", 5, EXPECTED_VARIABLE);
        do_test_for_err(" for a{ in x { 1 } ", 6, EXPECTED_IN);
        do_test_for_err(" for a,{ in x { 1 } ", 7, EXPECTED_2ND_VARIABLE);
        do_test_for_err(" for a,b { in x { 1 } ", 9, EXPECTED_IN);
        do_test_for_err(" for a in ; x { 1 } ", 10, EXPECTED_EXPRESSION);
        do_test_for_err(" for a in x ; { 1 } ", 12, EXPECTED_BLOCK);
        do_test_for_err(" for else in x { 1 } ", 5, KEYWORD_RESERVED);
        do_test_for_err(" for a, print in x { 1 } ", 8, NAME_RESERVED);
    }
}
