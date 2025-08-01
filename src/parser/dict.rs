use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, DictBuilder};

pub(crate) const MISSING_CLOSE: &str = "Missing closing ')'.";
pub(crate) const EXPECTED_OPEN: &str = "Expected opening '('.";
pub(crate) const EXPECTED_KEY_EXPRESSION_OR_CLOSE: &str = "Expected expression or ')'.";
pub(crate) const EXPECTED_COMMA_OR_CLOSE: &str = "Expected ',' or ')'.";
pub(crate) const EXPECTED_COLON: &str = "Expected ':' after key and before value.";
pub(crate) const EXPECTED_VALUE_EXPRESSION: &str = "Expected expression for value.";

impl<'a> Parser<'a> {
    /// Consume an object if next on input and return it.
    /// Otherwise consume nothing and return `None`
    ///
    /// "dict(" entry*  ")"
    /// entry = expr ":" expr ","?
    pub(super) fn dict(&mut self) -> ParseResult<Option<DictBuilder>> {
        let start = self.pos;
        let Some(id) = self.unchecked_identifier() else {
            return Ok(None);
        };
        if id != b"dict" {
            self.pos = start;
            return Ok(None);
        };

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

        Ok(Some(DictBuilder::new(entries)))
    }

    fn key(&mut self) -> ParseResult<Option<AstNode>> {
        let start = self.pos;
        let r = {
            #[allow(clippy::if_same_then_else)]
            #[allow(clippy::manual_map)]
            if self.nil().is_some() {
                None
            } else if self.boolean().is_some() {
                None
            } else if let Some(id) = self.identifier()? {
                Some(id.into())
            } else if let Some(s) = self.string()? {
                Some(s)
            } else if let Some(i) = self.integer() {
                Some(i.into())
            } else {
                None
            }
        };

        match r {
            Some(r) => Ok(Some(r)),
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
    use crate::parser::{string::MISSING_END_QUOTE, tests::*};

    #[test]
    fn test_dict_ok() {
        let expect = DictBuilder::new(vec![
            (id("a").into(), i(1).into()),
            (id("b").into(), nil().into()),
            (id("c").into(), b(true).into()),
            (id("the_d").into(), s("bar").into()),
        ]);
        do_test_parser_some(
            Parser::dict,
            r#"-dict(a:1,b:nil,c:true,the_d:"bar")-"#,
            expect.clone(),
            -1,
        );
        do_test_parser_some(
            Parser::dict,
            r#"-dict ( a : 1 , b : nil , c : true , the_d : "bar" , )-"#,
            expect.clone(),
            -1,
        );
        do_test_parser_some(
            Parser::dict,
            r#"-dict()-"#,
            DictBuilder::new(Vec::new()),
            -1,
        );
        do_test_parser_some(
            Parser::dict,
            r#"-dict(   )-"#,
            DictBuilder::new(Vec::new()),
            -1,
        );

        do_test_parser_some(
            Parser::dict,
            r#"-dict( a: dict( aa:1, ab:2, ac: dict()), b:3)-"#,
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
    fn test_dict_none() {
        do_test_parser_none(Parser::dict, "--");
    }

    #[test]
    fn test_dict_err() {
        do_test_parser_err(Parser::dict, r#"-dict("#, 5, MISSING_CLOSE);
        do_test_parser_err(Parser::dict, r#"-dict(a:1, b:1"#, 5, MISSING_CLOSE);
        do_test_parser_err(Parser::dict, r#"-dict(a:1, b:1, "#, 5, MISSING_CLOSE);

        do_test_parser_err(
            Parser::dict,
            r#"-dict( ; )-"#,
            7,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::dict,
            r#"-dict( , )-"#,
            7,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::dict,
            r#"-dict( a:1 , , )-"#,
            13,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::dict,
            r#"-dict( a:1 ; )-"#,
            11,
            EXPECTED_COMMA_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::dict,
            r#"-dict(a:1, b:1, - "#,
            16,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::dict,
            r#"-dict( a:1 ; )-"#,
            11,
            EXPECTED_COMMA_OR_CLOSE,
        );
        do_test_parser_err(Parser::dict, r#"-dict( 1.1:1 ; )-"#, 8, EXPECTED_COLON);
        do_test_parser_err(
            Parser::dict,
            r#"-dict( nil:1 ; )-"#,
            7,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::dict,
            r#"-dict( a:1, ( )-"#,
            12,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );

        do_test_parser_err(Parser::dict, r#"-dict( a:1 , b 2, )-"#, 15, EXPECTED_COLON);

        do_test_parser_err(
            Parser::dict,
            r#"-dict( a:1 , b: ;, )-"#,
            16,
            EXPECTED_VALUE_EXPRESSION,
        );

        do_test_parser_err(
            Parser::dict,
            r#"-dict(a:1, b:"aaa )-"#,
            13,
            MISSING_END_QUOTE,
        );
        do_test_parser_err(Parser::dict, "-dict #comment \n (a:1)-", 6, EXPECTED_OPEN);
    }
}
