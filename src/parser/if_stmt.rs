use super::{ParseResult, Parser, SyntaxError, comment::EXPECTED_WS_OR_COMMENT};
use crate::lang::{AstNode, ConditionalBlock, IfElseStmt};

const EXPECTED_CONDITION_EXPRESSION: &str = "Expected condition expression following if/else.";
const EXPECTED_BLOCK_OR_IF: &str = "Expected else block or 'if' keyword.";

impl<'a> Parser<'a> {
    // if_expr = { "if" ~ expr ~ block ~ if_else_if* ~ if_else? }
    // if_else_if = { "else" ~ "if" ~ expr ~ block }
    // if_else    = { "else" ~ block }
    pub(super) fn if_stmt(&mut self) -> ParseResult<AstNode> {
        // already passed "if" when called

        self.req_whitespace_comments()?;

        let cb = self.conditional_block()?;

        let mut conditional_blocks = vec![cb];
        let mut else_block = None;

        self.pos = loop {
            let savepoint = self.pos;

            self.whitespace_comments();

            if self.unchecked_identifier() != Some("else") {
                break savepoint;
            };

            let ws = self.whitespace_comments();

            let savepoint = self.pos;
            if self.unchecked_identifier() == Some("if") {
                if !ws {
                    return Err(SyntaxError {
                        pos: savepoint,
                        msg: EXPECTED_WS_OR_COMMENT,
                    });
                }
                self.req_whitespace_comments()?;
                conditional_blocks.push(self.conditional_block()?);
            } else {
                self.pos = savepoint;
                else_block = self.block()?;
                if else_block.is_none() {
                    return Err(SyntaxError {
                        pos: savepoint,
                        msg: EXPECTED_BLOCK_OR_IF,
                    });
                }
                break self.pos;
            };
        };

        Ok(AstNode::IfElseStmt(IfElseStmt {
            conditional_blocks,
            else_block,
        }))
    }

    // if_condition_block
    fn conditional_block(&mut self) -> ParseResult<ConditionalBlock> {
        let Some(condition) = self.expression()? else {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_CONDITION_EXPRESSION,
            });
        };
        self.whitespace_comments();

        let block = self.req(Self::block, "Expected block")?;

        Ok(ConditionalBlock { condition, block })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::IfElseStmt;
    use crate::parser::comment::EXPECTED_WS_OR_COMMENT;
    use crate::parser::stmt::EXPECTED_NEW_LINE_AFTER_STMT;
    use crate::parser::tests::*;

    #[track_caller]
    fn do_test_if_ok(input: &'static str, expected: IfElseStmt, expected_end: isize) {
        do_test_parser_ok(Parser::stmt, input, Some(expected.into()), expected_end);
    }

    #[track_caller]
    fn do_test_if_err(
        input: &'static str,
        expected_err_pos: usize,
        expected_err_msg: &'static str,
    ) {
        do_test_parser_err(Parser::stmt, input, expected_err_pos, expected_err_msg);
    }

    #[test]
    fn test_if() {
        do_test_if_ok(r" if x { 1 } ", _if!(cond(id!("x") => _block![i!(1)])), -1);
        do_test_if_ok(
            " if x { 1 } else if y { 2 } ",
            _if!(cond(id!("x") => _block![i!(1)]), cond(id!("y") => _block![i!(2)])),
            -1,
        );
        do_test_if_ok(
            " if x { 1 } else { 2 } ",
            _if!(cond(id!("x") => _block![i!(1)]), else(_block![i!(2)])),
            -1,
        );
        do_test_if_ok(
            " if x { 1 } else if y { 2 } else { 3 } ",
            _if!(
                cond(id!("x") => _block![i!(1)]),
                cond(id!("y") => _block![i!(2)]),
                else(_block![i!(3)])
            ),
            -1,
        );
        do_test_if_ok(
            " if x { 1 } else if y { 2 } else if z { 3 } else { 4 } ",
            _if!(
                cond(id!("x") => _block![i!(1)]),
                cond(id!("y") => _block![i!(2)]),
                cond(id!("z") => _block![i!(3)]),
                else(_block![i!(4)])
            ),
            -1,
        );
        do_test_if_ok(
            " if x{1}else if y{2}else{3} ",
            _if!(
                cond(id!("x") => _block![i!(1)]),
                cond(id!("y") => _block![i!(2)]),
                else(_block![i!(3)])
            ),
            -1,
        );
        do_test_if_ok(
            " if x { 1 } else { 2 } else { 3 } ",
            _if!(cond(id!("x") => _block![i!(1)]), else(_block![i!(2)])),
            -12,
        );
        do_test_if_err(
            " if, { 1 } else if y { 2 } else { 3 } ",
            3,
            EXPECTED_WS_OR_COMMENT,
        );
        do_test_if_err(
            " if x { 1; } else if y { 2 } else { 3 } ",
            9,
            EXPECTED_NEW_LINE_AFTER_STMT,
        );
        do_test_if_err(
            " if x { 1 } else ify { 2 } else { 3 } ",
            17,
            EXPECTED_BLOCK_OR_IF,
        );
        do_test_if_err(
            " if x { 1 } else if y { 2  else { 3 } ",
            27,
            EXPECTED_NEW_LINE_AFTER_STMT,
        );
    }
}
