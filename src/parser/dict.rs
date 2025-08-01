use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, DictBuilder, Value};

pub(crate) const MISSING_CLOSE: &str = "Missing closing ')'.";
pub(crate) const EXPECTED_OPEN: &str = "Expected opening '('.";
pub(crate) const EXPECTED_KEY_EXPRESSION_OR_CLOSE: &str = "Expected expression or ')'.";
pub(crate) const EXPECTED_COMMA_OR_CLOSE: &str = "Expected ',' or ')'.";
pub(crate) const EXPECTED_KEY_VALUE_PAIR: &str = "Expected key value pair separated by ':'.";
pub(crate) const INVALID_KEY_EXPRESSION: &str =
    "Invalid key expression. Only string, integer, bool or identifier allowed.";

impl<'a> Parser<'a> {
    /// Consume an object if next on input and return it.
    /// Otherwise consume nothing and return `None`
    ///
    /// Already passed "dict" when called
    /// "(" kv*  ")"
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

            let kv_start = self.pos;
            let Some(n) = self.expression()? else {
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

            let AstNode::KeyValue(kv) = n else {
                return Err(SyntaxError {
                    pos: kv_start,
                    msg: EXPECTED_KEY_VALUE_PAIR,
                });
            };

            let k = kv.key.as_ref();

            match k {
                AstNode::This => {}
                AstNode::Identifier(_) => {}
                AstNode::Value(Value::Str(_)) => {}
                AstNode::Value(Value::Integer(_)) => {}
                AstNode::Value(Value::Bool(_)) => {}
                _ => {
                    return Err(SyntaxError {
                        pos: kv_start,
                        msg: INVALID_KEY_EXPRESSION,
                    });
                }
            };

            entries.push(kv);

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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{
        expression::{
            EXPECTED_EXPRESSION,
            tests::{do_test_expr_err, do_test_expr_ok},
        },
        string::MISSING_END_QUOTE,
        tests::*,
    };

    #[test]
    fn test_dict_ok() {
        let expect = DictBuilder::new(vec![
            kv(id("a"), i(1)),
            kv(id("b"), nil()),
            kv(id("c"), b(true)),
            kv(id("the_d"), s("bar")),
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
                kv(
                    id("a"),
                    DictBuilder::new(vec![
                        kv(id("aa"), i(1)),
                        kv(id("ab"), i(2)),
                        kv(id("ac"), DictBuilder::new(Vec::new())),
                    ]),
                ),
                kv(id("b"), i(3)),
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
        do_test_expr_err(r#" dict( 1;1  ) "#, 7, EXPECTED_KEY_VALUE_PAIR);
        do_test_expr_err(r#" dict( nil:1 ; ) "#, 7, INVALID_KEY_EXPRESSION);
        do_test_expr_err(r#" dict( a:1, ] ) "#, 12, EXPECTED_KEY_EXPRESSION_OR_CLOSE);

        do_test_expr_err(r#" dict( a:1 , b 2, ) "#, 13, EXPECTED_KEY_VALUE_PAIR);

        do_test_expr_err(r#" dict( a:1 , b: ;, ) "#, 16, EXPECTED_EXPRESSION);

        do_test_expr_err(r#" dict(a:1, b:"aaa ) "#, 13, MISSING_END_QUOTE);
        do_test_expr_err(" dict #comment \n (a:1) ", 6, EXPECTED_OPEN);
    }
}
