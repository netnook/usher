use super::{ParseResult, Parser, SyntaxError};
use crate::{
    lang::{AstNode, ConditionalBlock, IfElse, Span},
    parser::identifier::UncheckedIdentifier,
};

impl<'a> Parser<'a> {
    // if_expr = { "if" ~ expr ~ block ~ if_else_if* ~ if_else? }
    // if_else_if = { "else" ~ "if" ~ expr ~ block }
    // if_else    = { "else" ~ block }
    pub(super) fn if_stmt(&mut self, span: Span) -> ParseResult<AstNode> {
        // already passed "if" when called

        self.req_whitespace_comments()?;

        let cb = self.conditional_block()?;

        let mut conditional_blocks = vec![cb];
        let mut else_block = None;

        self.pos = loop {
            let savepoint = self.pos;

            self.whitespace_comments();

            let Some(UncheckedIdentifier("else", _)) = self.unchecked_identifier() else {
                break savepoint;
            };

            let ws = self.whitespace_comments();

            let savepoint = self.pos;
            if let Some(UncheckedIdentifier("if", _)) = self.unchecked_identifier() {
                if !ws {
                    return Err(SyntaxError::ExpectedWhitespaceOrComment { pos: savepoint });
                }
                self.req_whitespace_comments()?;
                conditional_blocks.push(self.conditional_block()?);
            } else {
                self.pos = savepoint;
                else_block = self.block()?;
                if else_block.is_none() {
                    return Err(SyntaxError::ExpectedBlockOrIf { pos: savepoint });
                }
                break self.pos;
            };
        };

        Ok(AstNode::IfElse(IfElse {
            conditional_blocks,
            else_block,
            span: span.extended(self.pos),
        }))
    }

    // if_condition_block
    fn conditional_block(&mut self) -> ParseResult<ConditionalBlock> {
        let Some(condition) = self.expression()? else {
            return Err(SyntaxError::ExpectedConditionExpression { pos: self.pos });
        };
        self.whitespace_comments();

        let Some(block) = self.block()? else {
            return Err(SyntaxError::ExpectedBlock { pos: self.pos });
        };

        Ok(ConditionalBlock { condition, block })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::IfElse;
    use crate::parser::tests::*;

    #[track_caller]
    fn do_test_if_ok(input: &'static str, expected: IfElse, expected_end: isize) {
        do_test_parser_ok(Parser::stmt, input, Some(expected.into()), expected_end);
    }

    #[track_caller]
    fn do_test_if_err(input: &'static str, expected_err: SyntaxError) {
        do_test_parser_err(Parser::stmt, input, expected_err);
    }

    #[test]
    fn test_if() {
        do_test_if_ok(r" if x { 1 } ", _if!(cond(var("x") => _block![i(1)])), -1);
        do_test_if_ok(
            " if x { 1 } else if y { 2 } ",
            _if!(cond(var("x") => _block![i(1)]), cond(var("y") => _block![i(2)])),
            -1,
        );
        do_test_if_ok(
            " if x { 1 } else { 2 } ",
            _if!(cond(var("x") => _block![i(1)]), else(_block![i(2)])),
            -1,
        );
        do_test_if_ok(
            " if x { 1 } else if y { 2 } else { 3 } ",
            _if!(
                cond(var("x") => _block![i(1)]),
                cond(var("y") => _block![i(2)]),
                else(_block![i(3)])
            ),
            -1,
        );
        do_test_if_ok(
            " if x { 1 } else if y { 2 } else if z { 3 } else { 4 } ",
            _if!(
                cond(var("x") => _block![i(1)]),
                cond(var("y") => _block![i(2)]),
                cond(var("z") => _block![i(3)]),
                else(_block![i(4)])
            ),
            -1,
        );
        do_test_if_ok(
            " if x{1}else if y{2}else{3} ",
            _if!(
                cond(var("x") => _block![i(1)]),
                cond(var("y") => _block![i(2)]),
                else(_block![i(3)])
            ),
            -1,
        );
        do_test_if_ok(
            r" if x {
                1
                if a { 10 } else { 11 }
            } else if y {
                2
                if a { 12 } else if b { 13 }
            } else {
                3
                if a { 14 } else if b { 15 } else { 16 }
            } ",
            _if!(
                cond(var("x") => _block![
                    i(1),
                    _if!(
                        cond(var("a") => _block![i(10)]),
                        else(_block!(i(11)))
                    )
                ]),
                cond(var("y") => _block![
                    i(2),
                    _if!(
                        cond(var("a") => _block![i(12)]),
                        cond(var("b") => _block![i(13)])
                    )
                ]),
                else(_block![
                    i(3),
                    _if!(
                        cond(var("a") => _block![i(14)]),
                        cond(var("b") => _block![i(15)]),
                        else(_block![i(16)])
                    )
                ])
            ),
            -1,
        );
        do_test_if_ok(
            " if x { 1 } else { 2 } else { 3 } ",
            _if!(cond(var("x") => _block![i(1)]), else(_block![i(2)])),
            -12,
        );
        do_test_if_err(
            " if, { 1 } else if y { 2 } else { 3 } ",
            SyntaxError::ExpectedWhitespaceOrComment { pos: 3 },
        );
        do_test_if_err(
            " if x { 1; } else if y { 2 } else { 3 } ",
            SyntaxError::ExpectedNewLineAfterStmt { pos: 9 },
        );
        do_test_if_err(
            " if x { 1 } else ify { 2 } else { 3 } ",
            SyntaxError::ExpectedBlockOrIf { pos: 17 },
        );
        do_test_if_err(
            " if x { 1 } else if y { 2  else { 3 } ",
            SyntaxError::ExpectedNewLineAfterStmt { pos: 27 },
        );
    }
}
