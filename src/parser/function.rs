use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, FunctionDef};

const EXPECTED_OPEN_PARENS: &str = "Expected '('.";
const EXPECTED_PARAM_IDENT: &str = "Expected parameter name.";
const EXPECTED_COMMA_OR_CLOSE: &str = "Expected ',' or ')'.";
const EXPECTED_BODY: &str = "Expected function body.";

impl<'a> Parser<'a> {
    // "function(" ident,* ")"
    pub(super) fn anonymous_function(&mut self) -> ParseResult<Option<AstNode>> {
        let start = self.pos;

        if self.unchecked_identifier() != Some(b"function") {
            self.pos = start;
            return Ok(None);
        }

        self.linespace();

        if !self.char(b'(') {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_OPEN_PARENS,
            });
        }

        let mut params = Vec::new();

        loop {
            self.whitespace_comments();

            if self.char(b')') {
                break;
            }

            let Some(param) = self.identifier()? else {
                return Err(SyntaxError {
                    pos: self.pos,
                    msg: EXPECTED_PARAM_IDENT,
                });
            };

            params.push(param);

            self.whitespace_comments();

            if self.char(b',') {
                self.whitespace_comments();
                continue;
            }

            if self.char(b')') {
                break;
            }

            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_COMMA_OR_CLOSE,
            });
        }

        self.whitespace_comments();

        let Some(body) = self.block()? else {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_BODY,
            });
        };

        Ok(Some(AstNode::FunctionDef(FunctionDef {
            name: None,
            params,
            body,
        })))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::*;

    #[track_caller]
    fn do_test_anon_func_ok(input: &'static str, expected: FunctionDef, expected_end: isize) {
        do_test_parser_ok(
            Parser::anonymous_function,
            input,
            Some(expected.into()),
            expected_end,
        );
    }

    #[track_caller]
    fn do_test_anon_func_err(
        input: &'static str,
        expected_err_pos: usize,
        expected_err_msg: &'static str,
    ) {
        do_test_parser_err(
            Parser::anonymous_function,
            input,
            expected_err_pos,
            expected_err_msg,
        );
    }

    #[test]
    fn test_anon_func() {
        do_test_anon_func_ok(r" function(){} ", _func!(_block![]), -1);
        do_test_anon_func_ok(r" function ( ) { } ", _func!(_block![]), -1);
        do_test_anon_func_ok(" function(a) { 1 } ", _func!(p["a"], _block![i(1)]), -1);
        do_test_anon_func_ok(
            " function (a,b, cd) { 1 } ",
            _func!(p[ "a", "b", "cd"], _block![i(1)]),
            -1,
        );
        do_test_anon_func_ok(
            " function (a,#foo\nb   #foo\n, cd  #foo\n) #foo\n { 1 } ",
            _func!(p[ "a", "b", "cd"], _block![i(1)]),
            -1,
        );

        do_test_anon_func_err(" function #foo\n () { } ", 10, EXPECTED_OPEN_PARENS);
    }
}
