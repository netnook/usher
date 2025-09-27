use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, Declaration};

const EXPECTED_IDENTIFIER: &str = "Expected identifier after 'var'.";
const EXPECTED_EQUAL: &str =
    "Unexpected character 'x'. Expected '=' to follow varable name in declaration.";
const EXPECTED_EXPRESSION: &str = "Expected expression.";

impl<'a> Parser<'a> {
    // "var" identifier = expr
    pub(super) fn var_stmt(&mut self) -> ParseResult<AstNode> {
        // already passed "var" when called

        self.req_whitespace_comments()?;

        let Some(var) = self.declaration_identifier()? else {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_IDENTIFIER,
            });
        };

        self.linespace();

        if !self.char(b'=') {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_EQUAL,
            });
        }

        self.whitespace_comments();

        let Some(value) = self.expression()? else {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_EXPRESSION,
            });
        };

        Ok(AstNode::Declaration(Declaration {
            var,
            value: value.into(),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::comment::EXPECTED_WS_OR_COMMENT;
    use crate::parser::identifier::{KEYWORD_RESERVED, NAME_RESERVED};
    use crate::parser::tests::*;

    #[track_caller]
    fn do_test_var_ok(input: &'static str, expected: Declaration, expected_end: isize) {
        do_test_parser_ok(Parser::stmt, input, Some(expected.into()), expected_end);
    }

    #[track_caller]
    fn do_test_var_err(
        input: &'static str,
        expected_err_pos: usize,
        expected_err_msg: &'static str,
    ) {
        do_test_parser_err(Parser::stmt, input, expected_err_pos, expected_err_msg);
    }

    #[test]
    fn test_var() {
        do_test_var_ok(" var a=x+2 ", decl(var("a"), add(var("x"), i(2))), -1);
        do_test_var_ok(" var a = x + 2 ", decl(var("a"), add(var("x"), i(2))), -1);
        do_test_var_ok(" var a = x + 2 ", decl(var("a"), add(var("x"), i(2))), -1);
        do_test_var_ok(
            " var # comment \n a = # comment \n x + 2 ",
            decl(var("a"), add(var("x"), i(2))),
            -1,
        );

        do_test_var_err(" var,=x+2 ", 4, EXPECTED_WS_OR_COMMENT);
        do_test_var_err(" var a # comment \n = 1 ", 7, EXPECTED_EQUAL);
        do_test_var_err(" var a +  = 1 ", 7, EXPECTED_EQUAL);
        do_test_var_err(" var a = ; 1 ", 9, EXPECTED_EXPRESSION);
        do_test_var_err(" var print = 1 ", 5, NAME_RESERVED);
        do_test_var_err(" var else = 1 ", 5, KEYWORD_RESERVED);
    }
}
