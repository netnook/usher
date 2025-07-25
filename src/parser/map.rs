use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, MapBuilder, Value};

pub(crate) const MISSING_END_PARENS: &str = "Missing closing ')'.";
pub(crate) const EXPECTED_OPEN_PARENS: &str = "Expected opening '('.";
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
    /// Already passed "map" when called
    /// "(" entry*  ")"
    /// entry = expr ":" expr ","?
    pub(super) fn map(&mut self) -> ParseResult<MapBuilder> {
        self.linespace();
        if !self.char(b'(') {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_OPEN_PARENS,
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
                        msg: MISSING_END_PARENS,
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
                    msg: MISSING_END_PARENS,
                });
            }

            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_COMMA_OR_CLOSE,
            });
        }

        Ok(MapBuilder::new(entries))
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
    fn test_map_ok() {
        let expect = MapBuilder::new(vec![
            (id("a").into(), i(1).into()),
            (id("b").into(), nil().into()),
            (id("c").into(), b(true).into()),
            (id("the_d").into(), s("bar").into()),
        ]);
        do_test_expr_ok(r#" map(a:1,b:nil,c:true,the_d:"bar") "#, expect.clone(), -1);
        do_test_expr_ok(
            r#" map ( a : 1 , b : nil , c : true , the_d : "bar" , ) "#,
            expect.clone(),
            -1,
        );
        do_test_expr_ok(r#" map() "#, MapBuilder::new(Vec::new()), -1);
        do_test_expr_ok(r#" map(   ) "#, MapBuilder::new(Vec::new()), -1);

        do_test_expr_ok(
            r#" map( a: map( aa:1, ab:2, ac: map()), b:3) "#,
            MapBuilder::new(vec![
                (
                    id("a").into(),
                    MapBuilder::new(vec![
                        (id("aa").into(), i(1).into()),
                        (id("ab").into(), i(2).into()),
                        (id("ac").into(), MapBuilder::new(Vec::new()).into()),
                    ])
                    .into(),
                ),
                (id("b").into(), i(3).into()),
            ]),
            -1,
        );
    }

    #[test]
    fn test_map_err() {
        do_test_expr_err(r#" map("#, 4, MISSING_END_PARENS);
        do_test_expr_err(r#" map(a:1, b:1"#, 4, MISSING_END_PARENS);
        do_test_expr_err(r#" map(a:1, b:1, "#, 4, MISSING_END_PARENS);

        do_test_expr_err(r#" map( ; ) "#, 6, EXPECTED_KEY_EXPRESSION_OR_CLOSE);
        do_test_expr_err(r#" map( , ) "#, 6, EXPECTED_KEY_EXPRESSION_OR_CLOSE);
        do_test_expr_err(r#" map( a:1 , , ) "#, 12, EXPECTED_KEY_EXPRESSION_OR_CLOSE);
        do_test_expr_err(r#" map( a:1 ; ) "#, 10, EXPECTED_COMMA_OR_CLOSE);
        do_test_expr_err(r#" map(a:1, b:1, - "#, 15, EXPECTED_KEY_EXPRESSION_OR_CLOSE);
        do_test_expr_err(r#" map( a:1 ; ) "#, 10, EXPECTED_COMMA_OR_CLOSE);
        do_test_expr_err(r#" map( 1-1:1 ) "#, 6, INVALID_KEY_EXPRESSION);
        do_test_expr_err(r#" map( 1 1 ; ) "#, 8, EXPECTED_COLON);
        do_test_expr_err(r#" map( ;:1 ) "#, 6, EXPECTED_KEY_EXPRESSION_OR_CLOSE);
        do_test_expr_err(r#" map( a:1, [ ) "#, 11, EXPECTED_KEY_EXPRESSION_OR_CLOSE);

        do_test_expr_err(r#" map( a:1 , b 2, ) "#, 14, EXPECTED_COLON);

        do_test_expr_err(r#" map( a:1 , b: ;, ) "#, 15, EXPECTED_VALUE_EXPRESSION);

        do_test_expr_err(r#" map(a:1, b:"aaa ) "#, 12, MISSING_END_QUOTE);
        do_test_expr_err(" map #comment \n (a:1) ", 5, EXPECTED_OPEN_PARENS);
    }
}
