use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, FunctionDef, KeyValue, Param};

const EXPECTED_OPEN_PARENS: &str = "Expected '('.";
const EXPECTED_PARAM_IDENT: &str = "Expected parameter name.";
const EXPECTED_COMMA_OR_CLOSE: &str = "Expected ',' or ')'.";
const EXPECTED_BODY: &str = "Expected function body.";

impl<'a> Parser<'a> {
    // "function" name? "(" param,* ")"
    pub(super) fn function_expr(&mut self) -> ParseResult<Option<AstNode>> {
        let start = self.pos;

        if self.unchecked_identifier() != Some(b"function") {
            self.pos = start;
            return Ok(None);
        }

        self.linespace();

        let name = self.identifier()?;
        if name.is_some() {
            self.linespace();
        }

        let params = self.function_params()?;

        self.whitespace_comments();

        let Some(body) = self.block()? else {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_BODY,
            });
        };

        Ok(Some(AstNode::FunctionDef(FunctionDef {
            name,
            params,
            body,
        })))
    }

    fn function_params(&mut self) -> ParseResult<Vec<Param>> {
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

            let ref_pos = self.pos;
            let Some(expr) = self.expression()? else {
                return Err(SyntaxError {
                    pos: self.pos,
                    msg: EXPECTED_PARAM_IDENT,
                });
            };

            match expr {
                AstNode::Identifier(identifier) => {
                    params.push(Param {
                        name: identifier,
                        value: None,
                    });
                }
                AstNode::KeyValue(KeyValue { key, value }) => {
                    params.push(Param {
                        name: key,
                        value: Some(*value),
                    });
                }
                _ => {
                    return Err(SyntaxError {
                        pos: ref_pos,
                        msg: EXPECTED_PARAM_IDENT,
                    });
                }
            }

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

        Ok(params)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{expression::EXPECTED_IDENT_ON_KV_LHS, tests::*};

    #[track_caller]
    fn do_test_func_ok(input: &'static str, expected: FunctionDef, expected_end: isize) {
        do_test_parser_ok(
            Parser::function_expr,
            input,
            Some(expected.into()),
            expected_end,
        );
    }

    #[track_caller]
    fn do_test_func_err(
        input: &'static str,
        expected_err_pos: usize,
        expected_err_msg: &'static str,
    ) {
        do_test_parser_err(
            Parser::function_expr,
            input,
            expected_err_pos,
            expected_err_msg,
        );
    }

    #[test]
    fn test_anon_func() {
        do_test_func_ok(r" function(){} ", _func!(_block![]), -1);
        do_test_func_ok(r" function ( ) { } ", _func!(_block![]), -1);

        do_test_func_ok(r" function foo(){} ", _func!(n = "foo", _block![]), -1);
        do_test_func_ok(r" function bar ( ) { } ", _func!(n = "bar", _block![]), -1);
        do_test_func_ok(
            " function(a) { 1 } ",
            _func!(p = [_param!("a")], _block![i(1)]),
            -1,
        );
        do_test_func_ok(
            " function(a:1) { 1 } ",
            _func!(p = [_param!("a"=> i(1))], _block![i(1)]),
            -1,
        );
        do_test_func_ok(
            " function(a:1, b) { 1 } ",
            _func!(p = [_param!("a"=>i(1)), _param!("b")], _block![i(1)]),
            -1,
        );
        do_test_func_ok(
            " function (a,b, cd) { 1 } ",
            _func!(
                p = [_param!("a"), _param!("b"), _param!("cd")],
                _block![i(1)]
            ),
            -1,
        );
        do_test_func_ok(
            " function (a,#foo\nb   #foo\n, cd  #foo\n) #foo\n { 1 } ",
            _func!(
                p = [_param!("a"), _param!("b"), _param!("cd")],
                _block![i(1)]
            ),
            -1,
        );

        do_test_func_err(" function #foo\n () { } ", 10, EXPECTED_OPEN_PARENS);
        do_test_func_err(r#" function("a") { } "#, 10, EXPECTED_PARAM_IDENT);
        do_test_func_err(r#" function("a":42) { } "#, 10, EXPECTED_IDENT_ON_KV_LHS);
    }
}
