use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, Block};

pub(crate) const MISSING_BLOCK_END: &str = "Missing closing brace to end block.";
pub(crate) const EXPECTED_NEW_LINE_AFTER_STMT: &str = "Expected new line after statement.";
pub(crate) const EXPECTED_STATEMENT: &str = "Expected statement.";

impl<'a> Parser<'a> {
    // stmt*
    pub(super) fn stmts(&mut self) -> ParseResult<Vec<AstNode>> {
        let mut stmts = Vec::new();

        loop {
            let Some(stmt) = self.stmt()? else {
                break;
            };
            stmts.push(stmt);

            let save = self.pos;
            if self.req_whitespace_comments().is_err() {
                self.pos = save;
                break;
            };
        }

        Ok(stmts)
    }

    pub(super) fn stmt(&mut self) -> ParseResult<Option<AstNode>> {
        if let Some(res) = self.if_stmt()? {
            return Ok(Some(res));
        }
        if let Some(res) = self.expression()? {
            return Ok(Some(res));
        }

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
}
