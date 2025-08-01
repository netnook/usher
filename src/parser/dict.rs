use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, DictBuilder, Value};

pub(crate) const MISSING_CLOSE: &str = "Missing closing ')'.";
pub(crate) const EXPECTED_OPEN: &str = "Expected opening '('.";
pub(crate) const EXPECTED_KEY_EXPRESSION_OR_CLOSE: &str = "Expected expression or ')'.";
pub(crate) const EXPECTED_COMMA_OR_CLOSE: &str = "Expected ',' or ')'.";
pub(crate) const EXPECTED_COLON: &str = "Expected ':' after key and before value.";
pub(crate) const EXPECTED_VALUE_EXPRESSION: &str = "Expected expression for value.";
pub(crate) const INVALID_KEY_EXPRESSION: &str =
    "Invalid key expression. Only string, integer, bool or identifier allowed.";

impl<'a> Parser<'a> {
    /// Consume an object if next on input and return it.
    /// Otherwise consume nothing and return `None`
    ///
    /// Already passed "dict" when called
    /// "(" entry*  ")"
    /// entry = expr ":" expr ","?
    pub(super) fn dict(&mut self) -> ParseResult<DictBuilder> {
        self.linespace();
        if !self.char(b'(') {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_OPEN,
            });
        };

        let open_pos = self.pos - 1;
        self.whitespace_comments();

        let mut entries = Vec::new();
        loop {
            if self.char(b')') {
                break;
            }

            let Some(key_expr) = self.key()? else {
                if self.is_eoi() {
                    return Err(SyntaxError {
                        pos: open_pos,
                        msg: MISSING_CLOSE,
                    });
                } else {
                    return Err(SyntaxError {
                        pos: self.pos,
                        msg: EXPECTED_KEY_EXPRESSION_OR_CLOSE,
                    });
                }
            };

            self.whitespace_comments();

            if !self.char(b':') {
                return Err(SyntaxError::new(self.pos, EXPECTED_COLON));
            };

            self.whitespace_comments();

            let Some(value_expr) = self.expression()? else {
                return Err(SyntaxError {
                    pos: self.pos,
                    msg: EXPECTED_VALUE_EXPRESSION,
                });
            };

            entries.push((key_expr, value_expr));

            self.whitespace_comments();

            if self.char(b',') {
                self.whitespace_comments();
                continue;
            }

            if self.char(b')') {
                break;
            }

            if self.is_eoi() {
                return Err(SyntaxError {
                    pos: open_pos,
                    msg: MISSING_CLOSE,
                });
            }

            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_COMMA_OR_CLOSE,
            });
        }

        Ok(DictBuilder::new(entries))
    }

    fn key(&mut self) -> ParseResult<Option<AstNode>> {
        let start = self.pos;
        let r = self.expression()?;

        match r {
            Some(v @ AstNode::This) => Ok(Some(v)),
            Some(v @ AstNode::Identifier(_)) => Ok(Some(v)),
            Some(v @ AstNode::Value(Value::Str(_))) => Ok(Some(v)),
            Some(v @ AstNode::Value(Value::Integer(_))) => Ok(Some(v)),
            Some(v @ AstNode::Value(Value::Bool(_))) => Ok(Some(v)),
            Some(_) => Err(SyntaxError {
                pos: start,
                msg: INVALID_KEY_EXPRESSION,
            }),
            None => {
                self.pos = start;
                Ok(None)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{
        expression::tests::{do_test_expr_err, do_test_expr_ok},
        string::MISSING_END_QUOTE,
        tests::*,
    };

    #[test]
    fn test_dict_ok() {
        let expect = DictBuilder::new(vec![
            (id("a").into(), i(1).into()),
            (id("b").into(), nil().into()),
            (id("c").into(), b(true).into()),
            (id("the_d").into(), s("bar").into()),
        ]);
        do_test_expr_ok(
            r#" dict(a:1,b:nil,c:true,the_d:"bar") "#,
            expect.clone(),
            -1,
        );
        do_test_expr_ok(
            r#" dict ( a : 1 , b : nil , c : true , the_d : "bar" , ) "#,
            expect.clone(),
            -1,
        );
        do_test_expr_ok(r#" dict() "#, DictBuilder::new(Vec::new()), -1);
        do_test_expr_ok(r#" dict(   ) "#, DictBuilder::new(Vec::new()), -1);

        do_test_expr_ok(
            r#" dict( a: dict( aa:1, ab:2, ac: dict()), b:3) "#,
            DictBuilder::new(vec![
                (
                    id("a").into(),
                    DictBuilder::new(vec![
                        (id("aa").into(), i(1).into()),
                        (id("ab").into(), i(2).into()),
                        (id("ac").into(), DictBuilder::new(Vec::new()).into()),
                    ])
                    .into(),
                ),
                (id("b").into(), i(3).into()),
            ]),
            -1,
        );
    }

    #[test]
    fn test_dict_err() {
        do_test_expr_err(r#" dict("#, 5, MISSING_CLOSE);
        do_test_expr_err(r#" dict(a:1, b:1"#, 5, MISSING_CLOSE);
        do_test_expr_err(r#" dict(a:1, b:1, "#, 5, MISSING_CLOSE);

        do_test_expr_err(r#" dict( ; ) "#, 7, EXPECTED_KEY_EXPRESSION_OR_CLOSE);
        do_test_expr_err(r#" dict( , ) "#, 7, EXPECTED_KEY_EXPRESSION_OR_CLOSE);
        do_test_expr_err(r#" dict( a:1 , , ) "#, 13, EXPECTED_KEY_EXPRESSION_OR_CLOSE);
        do_test_expr_err(r#" dict( a:1 ; ) "#, 11, EXPECTED_COMMA_OR_CLOSE);
        do_test_expr_err(
            r#" dict(a:1, b:1, - "#,
            16,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_expr_err(r#" dict( a:1 ; ) "#, 11, EXPECTED_COMMA_OR_CLOSE);
        do_test_expr_err(r#" dict( 1;1  ) "#, 8, EXPECTED_COLON);
        do_test_expr_err(r#" dict( nil:1 ; ) "#, 7, INVALID_KEY_EXPRESSION);
        do_test_expr_err(r#" dict( a:1, ] ) "#, 12, EXPECTED_KEY_EXPRESSION_OR_CLOSE);

        do_test_expr_err(r#" dict( a:1 , b 2, ) "#, 15, EXPECTED_COLON);

        do_test_expr_err(r#" dict( a:1 , b: ;, ) "#, 16, EXPECTED_VALUE_EXPRESSION);

        do_test_expr_err(r#" dict(a:1, b:"aaa ) "#, 13, MISSING_END_QUOTE);
        do_test_expr_err(" dict #comment \n (a:1) ", 6, EXPECTED_OPEN);
    }
}
