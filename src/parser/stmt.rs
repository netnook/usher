use super::{ParseResult, Parser, SyntaxError};
use crate::{
    lang::{Assignment, AstNode, Block, Break, Continue, End, Span},
    parser::identifier::UncheckedIdentifier,
};

impl<'a> Parser<'a> {
    pub(super) fn stmt(&mut self) -> ParseResult<Option<AstNode>> {
        let start = self.pos;

        if let Some(UncheckedIdentifier(id, span)) = self.unchecked_identifier() {
            match id {
                "if" => {
                    return Ok(Some(self.if_stmt(span)?));
                }
                "for" => {
                    return Ok(Some(self.for_stmt(span)?));
                }
                "var" => {
                    return Ok(Some(self.var_stmt(span)?));
                }
                "break" => {
                    return Ok(Some(Break::new(span).into()));
                }
                "end" => {
                    return Ok(Some(End::new(span).into()));
                }
                "continue" => {
                    return Ok(Some(Continue::new(span).into()));
                }
                "return" => {
                    return Ok(Some(self.return_stmt()?));
                }
                _ => {
                    self.pos = start;
                }
            }
        }

        if let Some(blk) = self.block()? {
            return Ok(Some(blk.into()));
        }

        // FIXME: binary run exppression
        if let Some(res) = self.assignment_or_expression()? {
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
                return Err(SyntaxError::MissingClosingBrace { pos: start });
            }

            if first {
                first = false;
            } else if !details.newline {
                return Err(SyntaxError::ExpectedNewLineAfterStmt { pos: self.pos });
            }

            let Some(stmt) = self.stmt()? else {
                return Err(SyntaxError::ExpectedStmt { pos: self.pos });
            };

            stmts.push(stmt);
        }

        Ok(Some(Block {
            stmts,
            span: Span::start_end(start, self.pos),
        }))
    }

    pub(super) fn assignment_or_expression(&mut self) -> ParseResult<Option<AstNode>> {
        // FIXME check assignment does not assign to resered name on LHS

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
                AstNode::Var(_) | AstNode::PropertyOf(_) | AstNode::IndexOf(_)
            );
            if !pass {
                return Err(SyntaxError::AssignmentInvalidLHS { span: lhs.span() });
            }

            let Some(rhs) = self.expression()? else {
                return Err(SyntaxError::ExpectsExpression { pos: self.pos });
            };

            Ok(Some(AstNode::Assignment(Assignment {
                lhs: lhs.into(),
                rhs: rhs.into(),
            })))
        } else {
            self.pos = savepos;
            Ok(Some(expr))
        }
    }
}

#[cfg(test)]
pub(super) mod tests {
    use super::*;
    use crate::parser::tests::*;

    #[track_caller]
    fn do_test_block_ok(input: &'static str, expected: Block, expected_end: isize) {
        do_test_parser_ok(Parser::stmt, input, Some(expected.into()), expected_end);
    }

    #[track_caller]
    fn do_test_block_err(input: &'static str, expected_err: SyntaxError) {
        do_test_parser_err(Parser::block, input, expected_err);
    }

    #[test]
    fn test_block() {
        do_test_block_ok(" {1} ", _block![i(1)], -1);
        do_test_block_ok(" { 1 } ", _block![i(1)], -1);
        do_test_block_ok(" { 1 \n 2 \n 3 } ", _block![i(1), i(2), i(3)], -1);
        do_test_block_ok(" { \n 1 \n 2 \n 3 \n } ", _block![i(1), i(2), i(3)], -1);
        do_test_block_ok(" { #foo\n 1 #bar\n #baz \n 2 } ", _block![i(1), i(2)], -1);

        do_test_block_err(" { 1 ", SyntaxError::MissingClosingBrace { pos: 1 });
        do_test_block_err(
            " { 1 2 } ",
            SyntaxError::ExpectedNewLineAfterStmt { pos: 5 },
        );
        do_test_block_err(" { 1 \n ; } ", SyntaxError::ExpectedStmt { pos: 7 });
    }

    #[track_caller]
    fn do_test_assign_or_expr_ok<T: Into<AstNode>>(
        input: &'static str,
        expected: T,
        expected_end: isize,
    ) {
        do_test_parser_ok(
            Parser::assignment_or_expression,
            input,
            Some(expected.into()),
            expected_end,
        );
    }

    #[test]
    fn test_assignment_or_expr() {
        do_test_assign_or_expr_ok(" a ", var("a"), -1);
        do_test_assign_or_expr_ok(" a = 1 + 2 ", assign(var("a"), add(i(1), i(2))), -1);
        do_test_assign_or_expr_ok(
            " a[3].b.c = 1 + 2 ",
            assign(
                prop_of(prop_of(index_of(var("a"), i(3)), "b"), "c"),
                add(i(1), i(2)),
            ),
            -1,
        );
    }

    #[track_caller]
    fn do_test_assign_or_expr_err(input: &'static str, expected_err: SyntaxError) {
        do_test_parser_err(Parser::assignment_or_expression, input, expected_err);
    }

    #[test]
    fn test_assign_or_expr_err() {
        do_test_assign_or_expr_err(" a = ;", SyntaxError::ExpectsExpression { pos: 5 });
        do_test_assign_or_expr_err(
            " a + b = 3",
            SyntaxError::AssignmentInvalidLHS {
                span: Span::new(1, 5),
            },
        );
    }

    #[track_caller]
    pub(crate) fn do_test_stmt_ok<T: Into<AstNode>>(
        input: &'static str,
        expected: T,
        expected_end: isize,
    ) {
        do_test_parser_ok(Parser::stmt, input, Some(expected.into()), expected_end);
    }

    #[test]
    fn test_stmt() {
        do_test_stmt_ok(" if true { 2 } ", _if!(cond(b(true) => _block![i(2)])), -1);
        do_test_stmt_ok(
            " for a in b { 2 } ",
            _for(var("a"), None, var("b"), _block![i(2)]),
            -1,
        );
        do_test_stmt_ok(" var a = 1 ", decl(var("a"), i(1)), -1);
        do_test_stmt_ok(" a = 1 ", assign(var("a"), i(1)), -1);
        do_test_stmt_ok(" a + 2 ", add(var("a"), i(2)), -1);

        // check that vars starting with keywords are not mistaken for those keywords
        do_test_stmt_ok(" iffy + 2 ", add(var("iffy"), i(2)), -1);
        do_test_stmt_ok(" for_me + 2 ", add(var("for_me"), i(2)), -1);
        do_test_stmt_ok(" vario + 2 ", add(var("vario"), i(2)), -1);
        do_test_stmt_ok(" break ", _break(), -1);
        do_test_stmt_ok(" continue ", _continue(), -1);
        do_test_stmt_ok(" end ", _end(), -1);
    }
}
