use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, DictBuilder, Span};

impl<'a> Parser<'a> {
    /// Consume an object if next on input and return it.
    /// Otherwise consume nothing and return `None`
    ///
    /// Already passed "dict" when called
    /// "(" kv*  ")"
    pub(super) fn dict(&mut self, start: usize) -> ParseResult<DictBuilder> {
        self.linespace();
        if !self.char(b'(') {
            return Err(SyntaxError::DictExpectedOpenParens { pos: self.pos });
        };

        let open_pos = self.pos - 1;
        self.whitespace_comments();

        let mut entries = Vec::new();
        loop {
            if self.char(b')') {
                break;
            }

            let Some(n) = self.expression()? else {
                if self.is_eoi() {
                    return Err(SyntaxError::DictMissingCloseParens { pos: open_pos });
                } else {
                    return Err(SyntaxError::DictExpectedKeyOrCloseParens { pos: self.pos });
                }
            };

            let AstNode::KeyValue(kv) = n else {
                return Err(SyntaxError::DictExpectedKeyValuePair { span: n.span() });
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
                return Err(SyntaxError::DictMissingCloseParens { pos: open_pos });
            }

            return Err(SyntaxError::DictExpectedCommaOrCloseParens { pos: self.pos });
        }

        Ok(DictBuilder {
            entries,
            span: Span::start_end(start, self.pos),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{
        expression::tests::{do_test_expr_err, do_test_expr_ok},
        tests::*,
    };

    #[test]
    fn test_dict_ok() {
        let expect = dict_builder(vec![
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
        do_test_expr_ok(r#" dict() "#, dict_builder(Vec::new()), -1);
        do_test_expr_ok(r#" dict(   ) "#, dict_builder(Vec::new()), -1);

        do_test_expr_ok(
            r#" dict( a: dict( aa:1, ab:2, ac: dict()), b:3) "#,
            dict_builder(vec![
                kv(
                    id("a"),
                    dict_builder(vec![
                        kv(id("aa"), i(1)),
                        kv(id("ab"), i(2)),
                        kv(id("ac"), dict_builder(Vec::new())),
                    ]),
                ),
                kv(id("b"), i(3)),
            ]),
            -1,
        );
    }

    #[test]
    fn test_dict_err() {
        do_test_expr_err(r#" dict("#, SyntaxError::DictMissingCloseParens { pos: 5 });
        do_test_expr_err(
            r#" dict(a:1, b:1"#,
            SyntaxError::DictMissingCloseParens { pos: 5 },
        );
        do_test_expr_err(
            r#" dict(a:1, b:1, "#,
            SyntaxError::DictMissingCloseParens { pos: 5 },
        );

        do_test_expr_err(
            r#" dict( ; ) "#,
            SyntaxError::DictExpectedKeyOrCloseParens { pos: 7 },
        );
        do_test_expr_err(
            r#" dict( , ) "#,
            SyntaxError::DictExpectedKeyOrCloseParens { pos: 7 },
        );
        do_test_expr_err(
            r#" dict( a:1 , , ) "#,
            SyntaxError::DictExpectedKeyOrCloseParens { pos: 13 },
        );
        do_test_expr_err(
            r#" dict( a:1 ; ) "#,
            SyntaxError::DictExpectedCommaOrCloseParens { pos: 11 },
        );
        do_test_expr_err(
            r#" dict(a:1, b:1, - "#,
            SyntaxError::DictExpectedKeyOrCloseParens { pos: 16 },
        );
        do_test_expr_err(
            r#" dict( a:1 ; ) "#,
            SyntaxError::DictExpectedCommaOrCloseParens { pos: 11 },
        );
        do_test_expr_err(
            r#" dict( 12;1  ) "#,
            SyntaxError::DictExpectedKeyValuePair {
                span: Span::new(7, 2),
            },
        );
        do_test_expr_err(
            r#" dict( a:1, ] ) "#,
            SyntaxError::DictExpectedKeyOrCloseParens { pos: 12 },
        );
        do_test_expr_err(
            r#" dict( a:1 , b 2, ) "#,
            SyntaxError::DictExpectedKeyValuePair {
                span: Span::new(13, 1),
            },
        );
        do_test_expr_err(
            " dict #comment \n (a:1) ",
            SyntaxError::DictExpectedOpenParens { pos: 6 },
        );

        do_test_expr_err(
            r#" dict( a:1 , b: ;, ) "#,
            SyntaxError::ExpectsExpression { pos: 16 },
        );
        do_test_expr_err(
            r#" dict( nil:1 ; ) "#,
            SyntaxError::KeyValueExpectsIdentOnLHS {
                span: Span::new(7, 3),
                pos: 10,
            },
        );
        do_test_expr_err(
            r#" dict(a:1, b:"aaa ) "#,
            SyntaxError::StringMissingCloseQuote { pos: 13 },
        );
    }
}
