use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, MapBuilder};

pub(crate) const MISSING_END_PARENS: &str = "Missing closing ')'.";
pub(crate) const EXPECTED_OPEN_PARENS: &str = "Expected opening '('.";
pub(crate) const EXPECTED_KEY_EXPRESSION_OR_CLOSE: &str = "Expected expression or ')'.";
pub(crate) const EXPECTED_COMMA_OR_CLOSE: &str = "Expected ',' or ')'.";
pub(crate) const EXPECTED_COLON: &str = "Expected ':' after key and before value.";
pub(crate) const EXPECTED_VALUE_EXPRESSION: &str = "Expected expression for value.";

impl<'a> Parser<'a> {
    /// Consume an object if next on input and return it.
    /// Otherwise consume nothing and return `None`
    ///
    /// "map(" entry*  ")"
    /// entry = expr ":" expr ","?
    pub(super) fn map(&mut self) -> ParseResult<Option<MapBuilder>> {
        let start = self.pos;
        let Some(id) = self.unchecked_identifier() else {
            return Ok(None);
        };
        if id != b"map" {
            self.pos = start;
            return Ok(None);
        };

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

        Ok(Some(MapBuilder::new(entries)))
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
    fn test_map_ok() {
        let expect = MapBuilder::new(vec![
            (id("a").into(), i(1).into()),
            (id("b").into(), nil().into()),
            (id("c").into(), b(true).into()),
            (id("the_d").into(), s("bar").into()),
        ]);
        do_test_parser_some(
            Parser::map,
            r#"-map(a:1,b:nil,c:true,the_d:"bar")-"#,
            expect.clone(),
            -1,
        );
        do_test_parser_some(
            Parser::map,
            r#"-map ( a : 1 , b : nil , c : true , the_d : "bar" , )-"#,
            expect.clone(),
            -1,
        );
        do_test_parser_some(Parser::map, r#"-map()-"#, MapBuilder::new(Vec::new()), -1);
        do_test_parser_some(
            Parser::map,
            r#"-map(   )-"#,
            MapBuilder::new(Vec::new()),
            -1,
        );

        do_test_parser_some(
            Parser::map,
            r#"-map( a: map( aa:1, ab:2, ac: map()), b:3)-"#,
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
    fn test_map_none() {
        do_test_parser_none(Parser::map, "--");
    }

    #[test]
    fn test_map_err() {
        do_test_parser_err(Parser::map, r#"-map("#, 4, MISSING_END_PARENS);
        do_test_parser_err(Parser::map, r#"-map(a:1, b:1"#, 4, MISSING_END_PARENS);
        do_test_parser_err(Parser::map, r#"-map(a:1, b:1, "#, 4, MISSING_END_PARENS);

        do_test_parser_err(
            Parser::map,
            r#"-map( ; )-"#,
            6,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::map,
            r#"-map( , )-"#,
            6,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::map,
            r#"-map( a:1 , , )-"#,
            12,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::map,
            r#"-map( a:1 ; )-"#,
            10,
            EXPECTED_COMMA_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::map,
            r#"-map(a:1, b:1, - "#,
            15,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::map,
            r#"-map( a:1 ; )-"#,
            10,
            EXPECTED_COMMA_OR_CLOSE,
        );
        do_test_parser_err(Parser::map, r#"-map( 1.1:1 ; )-"#, 7, EXPECTED_COLON);
        do_test_parser_err(
            Parser::map,
            r#"-map( nil:1 ; )-"#,
            6,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );
        do_test_parser_err(
            Parser::map,
            r#"-map( a:1, [ )-"#,
            11,
            EXPECTED_KEY_EXPRESSION_OR_CLOSE,
        );

        do_test_parser_err(Parser::map, r#"-map( a:1 , b 2, )-"#, 14, EXPECTED_COLON);

        do_test_parser_err(
            Parser::map,
            r#"-map( a:1 , b: ;, )-"#,
            15,
            EXPECTED_VALUE_EXPRESSION,
        );

        do_test_parser_err(Parser::map, r#"-map(a:1, b:"aaa )-"#, 12, MISSING_END_QUOTE);
        do_test_parser_err(
            Parser::map,
            "-map #comment \n (a:1)-",
            5,
            EXPECTED_OPEN_PARENS,
        );
    }
}
