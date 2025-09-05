use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, FunctionDef, KeyValue, Param};

const EXPECTED_OPEN_PARENS: &str = "Expected '('.";
const EXPECTED_PARAM_IDENT: &str = "Expected parameter name.";
const EXPECTED_COMMA_OR_CLOSE: &str = "Expected ',' or ')'.";
const EXPECTED_BODY: &str = "Expected function body.";

impl<'a> Parser<'a> {
    // "function" name? "(" param,* ")"
    /// Already passed "function" when called
    pub(super) fn function_def(&mut self) -> ParseResult<FunctionDef> {
        self.linespace();

        let name = self.declaration_identifier()?;
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

        Ok(FunctionDef { name, params, body })
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
                        default_value: None,
                    });
                }
                AstNode::KeyValue(KeyValue { key, value }) => {
                    params.push(Param {
                        name: key,
                        default_value: Some(*value),
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
    use crate::parser::{
        expression::{
            EXPECTED_IDENT_ON_KV_LHS,
            tests::{do_test_expr_err, do_test_expr_ok},
        },
        tests::*,
    };

    #[test]
    fn test_func() {
        do_test_expr_ok(r" function(){} ", _func!(_block![]), -1);
        do_test_expr_ok(r" function ( ) { } ", _func!(_block![]), -1);

        do_test_expr_ok(r" function foo(){} ", _func!(name("foo"), _block![]), -1);
        do_test_expr_ok(
            r" function bar ( ) { } ",
            _func!(name("bar"), _block![]),
            -1,
        );
        do_test_expr_ok(" function(a) { 1 } ", _func!(param("a"), _block![i(1)]), -1);
        do_test_expr_ok(
            " function(a:1) { 1 } ",
            _func!(param("a", i(1)), _block![i(1)]),
            -1,
        );
        do_test_expr_ok(
            " function(a:1, b) { 1 } ",
            _func!(param("a", i(1)), param("b"), _block![i(1)]),
            -1,
        );
        do_test_expr_ok(
            " function (a,b, cd) { 1 } ",
            _func!(param("a"), param("b"), param("cd"), _block![i(1)]),
            -1,
        );
        do_test_expr_ok(
            " function (a,#foo\nb   #foo\n, cd  #foo\n) #foo\n { 1 } ",
            _func!(param("a"), param("b"), param("cd"), _block![i(1)]),
            -1,
        );
        do_test_expr_ok(
            " function  meme (a,#foo\nb :8  #foo\n, cd  #foo\n) #foo\n { 1 } ",
            _func!(
                name("meme"),
                param("a"),
                param("b", i(8)),
                param("cd"),
                _block![i(1)]
            ),
            -1,
        );

        do_test_expr_err(" function #foo\n () { } ", 10, EXPECTED_OPEN_PARENS);
        do_test_expr_err(r#" function("a") { } "#, 10, EXPECTED_PARAM_IDENT);
        do_test_expr_err(r#" function("a":42) { } "#, 10, EXPECTED_IDENT_ON_KV_LHS);

        // FIXME: validation - the following should not be allowed
        // do_test_expr_err(" 3 + function() { true }", 5, "foo");

        // do_test_stmt_ok(
        //     " var a = function() { true } ",
        //     _func!(
        //         p = [_param!("a"), _param!("b"), _param!("cd")],
        //         _block![i(1)]
        //     )
        //     .into(),
        //     -1,
        // );
        // do_test_stmt_err(
        //     " var a = function named() { true }",
        //     18,
        //     "named not allowed in assingment",
        // );
    }
}
