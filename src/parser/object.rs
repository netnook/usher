use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, ObjectBuilder};

const MISSING_END_BRACKET: &str = "Missing closing '}'.";
const EXPECTED_KEY_EXPRESSION_OR_CLOSE: &str = "Expected expression or '}'.";
const EXPECTED_COMMA_OR_CLOSE: &str = "Expected ',' or '}'.";
const EXPECTED_COLON: &str = "Expected ':' after key and before value.";
const EXPECTED_VALUE_EXPRESSION: &str = "Expected expression for value.";

impl<'a> Parser<'a> {
    /// Consume an object if next on input and return it.
    /// Otherwise consume nothing and return `None`
    ///
    /// object = "{" entry*  "}"
    /// entry = expr ":" expr ","?
    pub(super) fn object(&mut self) -> ParseResult<Option<ObjectBuilder>> {
        let start = self.pos;
        if !self.char(b'{') {
            return Ok(None);
        };
        self.whitespace_comments();

        let mut entries = Vec::new();
        loop {
            if self.char(b'}') {
                break;
            }

            let Some(key_expr) = self.key()? else {
                if self.is_eoi() {
                    return Err(SyntaxError {
                        pos: start,
                        msg: MISSING_END_BRACKET,
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

            if self.char(b'}') {
                break;
            }

            if self.is_eoi() {
                return Err(SyntaxError {
                    pos: start,
                    msg: MISSING_END_BRACKET,
                });
            }

            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_COMMA_OR_CLOSE,
            });
        }

        Ok(Some(ObjectBuilder::new(entries)))
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
    fn test_object_ok() {
        let expect = ObjectBuilder::new(vec![
            (id("a").into(), i(1).into()),
            (id("b").into(), nil().into()),
            (id("c").into(), b(true).into()),
            (id("the_d").into(), s("bar").into()),
        ]);
        do_test_parser_some(
            Parser::object,
            r#"-{a:1,b:nil,c:true,the_d:"bar"}-"#,
            expect.clone(),
            -1,
        );
        do_test_parser_some(
            Parser::object,
            r#"-{ a : 1 , b : nil , c : true , the_d : "bar" , }-"#,
            expect.clone(),
            -1,
        );
        do_test_parser_some(
            Parser::object,
            r#"-{}-"#,
            ObjectBuilder::new(Vec::new()),
            -1,
        );
        do_test_parser_some(
            Parser::object,
            r#"-{   }-"#,
            ObjectBuilder::new(Vec::new()),
            -1,
        );

        do_test_parser_some(
            Parser::object,
            r#"-{ a: { aa:1, ab:2, ac: {}}, b:3}-"#,
            ObjectBuilder::new(vec![
                (
                    id("a").into(),
                    ObjectBuilder::new(vec![
                        (id("aa").into(), i(1).into()),
                        (id("ab").into(), i(2).into()),
                        (id("ac").into(), ObjectBuilder::new(Vec::new()).into()),
                    ])
                    .into(),
                ),
                (id("b").into(), i(3).into()),
            ]),
            -1,
        );
    }

    #[test]
    fn test_object_none() {
        do_test_parser_none(Parser::object, "--");
    }

    #[test]
    fn test_object_err() {
        do_test_parser_err(Parser::object, r#"-{"#, 1, MISSING_END_BRACKET);
        do_test_parser_err(Parser::object, r#"-{a:1, b:1"#, 1, MISSING_END_BRACKET);
        do_test_parser_err(Parser::object, r#"-{a:1, b:1, "#, 1, MISSING_END_BRACKET);

        do_test_parser_err(
            Parser::object,
            r#"-{ ; }-"#,
            3,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::object,
            r#"-{ , }-"#,
            3,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::object,
            r#"-{ a:1 , , }-"#,
            9,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(Parser::object, r#"-{ a:1 ; }-"#, 7, EXPECTED_COMMA_OR_CLOSE);
        do_test_parser_err(
            Parser::object,
            r#"-{a:1, b:1, - "#,
            12,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(Parser::object, r#"-{ a:1 ; }-"#, 7, EXPECTED_COMMA_OR_CLOSE);
        do_test_parser_err(Parser::object, r#"-{ 1.1:1 ; }-"#, 4, EXPECTED_COLON);
        do_test_parser_err(
            Parser::object,
            r#"-{ nil:1 ; }-"#,
            3,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::object,
            r#"-{ a:1, [ }-"#,
            8,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );

        do_test_parser_err(Parser::object, r#"-{ a:1 , b 2, }-"#, 11, EXPECTED_COLON);

        do_test_parser_err(
            Parser::object,
            r#"-{ a:1 , b: ;, }-"#,
            12,
            EXPECTED_VALUE_EXPRESSION,
        );

        do_test_parser_err(Parser::object, r#"-{a:1, b:"aaa }-"#, 9, MISSING_END_QUOTE);
    }
}
