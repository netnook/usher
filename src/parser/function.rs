use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{
    AstNode, Context, EvalStop, FunctionDef, InternalProgramError, KeyValueBuilder, Param, Var,
};

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
            return Err(SyntaxError::FunctionExpectedBody { pos: self.pos });
        };

        Ok(FunctionDef { name, params, body })
    }

    fn function_params(&mut self) -> ParseResult<Vec<Param>> {
        if !self.char(b'(') {
            return Err(SyntaxError::FunctionExpectedOpenParens { pos: self.pos });
        }

        let mut params = Vec::new();

        loop {
            self.whitespace_comments();

            if self.char(b')') {
                break;
            }

            let ref_pos = self.pos;
            let Some(expr) = self.expression()? else {
                return Err(SyntaxError::FunctionExpectedParamIdent { pos: self.pos });
            };

            match expr {
                AstNode::Var(var) => {
                    params.push(Param::Required(var));
                }
                AstNode::KeyValue(KeyValueBuilder { key, value }) => {
                    // FIXME: check value tree to see that it only contains constant "compatible" expressions
                    let mut ctxt = Context::default();
                    let value = match value.eval(&mut ctxt) {
                        Ok(val) => Ok(val),
                        Err(EvalStop::Return(val)) => Ok(val),
                        Err(EvalStop::Error(err)) => {
                            Err(SyntaxError::ConstantEvalError { cause: err })
                        }
                        Err(EvalStop::Break(span)) => Err(SyntaxError::ConstantEvalError {
                            cause: InternalProgramError::BreakWithoutLoop { span },
                        }),
                        Err(EvalStop::Continue(span)) => Err(SyntaxError::ConstantEvalError {
                            cause: InternalProgramError::ContinueWithoutLoop { span },
                        }),
                        Err(EvalStop::Throw) => todo!(),
                    }?;
                    params.push(Param::Optional(Var::new(key), value));
                }
                _ => {
                    return Err(SyntaxError::FunctionExpectedParamIdent { pos: ref_pos });
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

            return Err(SyntaxError::FunctionExpectedCommaOrCloseParens { pos: self.pos });
        }

        Ok(params)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::Span,
        parser::{
            expression::tests::{do_test_expr_err, do_test_expr_ok},
            tests::*,
        },
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
            _func!(param("a", 1.to_value()), _block![i(1)]),
            -1,
        );
        do_test_expr_ok(
            " function(a:1, b) { 1 } ",
            _func!(param("a", 1.to_value()), param("b"), _block![i(1)]),
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
                param("b", 8.to_value()),
                param("cd"),
                _block![i(1)]
            ),
            -1,
        );

        do_test_expr_err(
            " function #foo\n () { } ",
            SyntaxError::FunctionExpectedOpenParens { pos: 10 },
        );
        do_test_expr_err(
            r#" function("a") { } "#,
            SyntaxError::FunctionExpectedParamIdent { pos: 10 },
        );
        do_test_expr_err(
            r#" function("a":42) { } "#,
            SyntaxError::KeyValueExpectsIdentOnLHS {
                span: Span::new(10, 3),
                pos: 13,
            },
        );

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
