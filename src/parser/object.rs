use super::{ParseResult, Parser, SyntaxError};
use crate::lang::ObjectBuilder;

const MISSING_END_BRACKET: &str = "Missing closing '}'.";
const EXPECTED_EXPRESSION_OR_CLOSE: &str = "Expected expression or '}'.";
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

            let Some(key_expr) = self.expression()? else {
                if self.is_eoi() {
                    return Err(SyntaxError {
                        pos: start,
                        msg: MISSING_END_BRACKET,
                    });
                } else {
                    return Err(SyntaxError {
                        pos: self.pos,
                        msg: EXPECTED_EXPRESSION_OR_CLOSE,
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{string::MISSING_END_QUOTE, tests::*};

    #[test]
    fn test_object_ok() {
        let expect = ObjectBuilder::new(vec![
            (ident("a").into(), i(1).into()),
            (ident("b").into(), nil().into()),
            (ident("c").into(), b(true).into()),
            (ident("the_d").into(), s("bar").into()),
        ]);
        do_test_parser_some(
            Parser::object,
            r#"-{a:1,b:nil,c:true,the_d:"bar"}-"#,
            expect.clone(),
            1,
        );
        do_test_parser_some(
            Parser::object,
            r#"-{ a : 1 , b : nil , c : true , the_d : "bar" , }-"#,
            expect.clone(),
            1,
        );
        do_test_parser_some(Parser::object, r#"-{}-"#, ObjectBuilder::new(Vec::new()), 1);
        do_test_parser_some(
            Parser::object,
            r#"-{   }-"#,
            ObjectBuilder::new(Vec::new()),
            1,
        );

        do_test_parser_some(
            Parser::object,
            r#"-{ a: { aa:1, ab:2, ac: {}}, b:3}-"#,
            ObjectBuilder::new(vec![
                (
                    ident("a").into(),
                    ObjectBuilder::new(vec![
                        (ident("aa").into(), i(1).into()),
                        (ident("ab").into(), i(2).into()),
                        (ident("ac").into(), ObjectBuilder::new(Vec::new()).into()),
                    ])
                    .into(),
                ),
                (ident("b").into(), i(3).into()),
            ]),
            1,
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
            EXPECTED_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::object,
            r#"-{ , }-"#,
            3,
            EXPECTED_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::object,
            r#"-{ a:1 , , }-"#,
            9,
            EXPECTED_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(Parser::object, r#"-{ a:1 ; }-"#, 7, EXPECTED_COMMA_OR_CLOSE);
        do_test_parser_err(
            Parser::object,
            r#"-{a:1, b:1, - "#,
            12,
            EXPECTED_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(Parser::object, r#"-{ a:1 ; }-"#, 7, EXPECTED_COMMA_OR_CLOSE);
        // do_test_parser_err(Parser::object,r#"-{ a:1 [ }-"#, 7, EXPECTED_EXPRESSION_OR_CLOSE); // FIXME: don't allow list expressions as keys

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
