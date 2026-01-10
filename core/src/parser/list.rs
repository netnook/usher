use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{ListBuilder, Span};

impl<'a> Parser<'a> {
    /// Consume a list arguments if next on input and return it.
    /// "[" expr,* ,? "]"
    pub(super) fn list(&mut self) -> ParseResult<Option<ListBuilder>> {
        let start = self.pos;
        if !self.char(b'[') {
            return Ok(None);
        };

        self.whitespace_comments();

        let mut entries = Vec::new();
        loop {
            if self.char(b']') {
                break;
            }

            let Some(expr) = self.simple_expression()? else {
                if self.is_eoi() {
                    return Err(SyntaxError::MissingClosingBracket { pos: start });
                } else {
                    return Err(SyntaxError::ListExpectedExpressionOrCloseBracket {
                        pos: self.pos,
                    });
                }
            };

            entries.push(expr);
            self.whitespace_comments();

            if self.char(b',') {
                self.whitespace_comments();
                continue;
            }

            if self.char(b']') {
                break;
            }

            if self.is_eoi() {
                return Err(SyntaxError::MissingClosingBracket { pos: start });
            }

            return Err(SyntaxError::ListExpectedCommaOrCloseBracket { pos: self.pos });
        }

        Ok(Some(ListBuilder::new(
            entries,
            Span::start_end(start, self.pos),
        )))
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
    fn test_list_ok() {
        let expect = list_builder!(i(1), i(2), i(3), i(4));
        do_test_expr_ok(" [1,2,3,4] ", expect.clone(), -1);
        do_test_expr_ok(" [1,2,3,4,] ", expect.clone(), -1);
        do_test_expr_ok(" [ 1 , 2 , 3 , 4 ] ", expect.clone(), -1);
        do_test_expr_ok(r#" [] "#, list_builder!(), -1);
        do_test_expr_ok(r#" [  ] "#, list_builder!(), -1);
        do_test_expr_ok(
            " [ # comment \n 1 , # comment \n 2 # comment \n , # comment \n\n 3 # comment \n , # comment \n 4 ] ",
            expect.clone(),
            -1,
        );
        do_test_expr_ok(
            " [# comment \n1# comment \n,2,# comment \n# comment \n\n3# comment \n,# comment \n4,] ",
            expect,
            -1,
        );

        let expect = list_builder!(i(1), nil(), b(false), s("foo"));
        do_test_expr_ok(r#" [1,nil,false,"foo"] "#, expect.clone(), -1);
        do_test_expr_ok(r#" [1, nil, false, "foo"] "#, expect.clone(), -1);
        do_test_expr_ok(r#" [1, nil, false, "foo", ] "#, expect.clone(), -1);
        do_test_expr_ok(" [ # comment\n # comment \n \n ] ", list_builder!(), -1);
        do_test_expr_ok(
            " [[1, 1], 2, [3, 3, 3,], 4,] ",
            list_builder!(
                list_builder!(i(1), i(1)),
                i(2),
                list_builder!(i(3), i(3), i(3)),
                i(4)
            ),
            -1,
        );
    }

    #[test]
    fn test_list_err() {
        do_test_expr_err(
            r#" [1,2,3,4"#,
            SyntaxError::MissingClosingBracket { pos: 1 },
        );
        do_test_expr_err(r#" [1,2,3,"#, SyntaxError::MissingClosingBracket { pos: 1 });
        do_test_expr_err(
            r#" [1 2]-"#,
            SyntaxError::ListExpectedCommaOrCloseBracket { pos: 4 },
        );
        do_test_expr_err(
            r#" [1, 2, 3, 4 } "#,
            SyntaxError::ListExpectedCommaOrCloseBracket { pos: 13 },
        );
        do_test_expr_err(
            r#" [1, 2, 3, 4, } "#,
            SyntaxError::ListExpectedExpressionOrCloseBracket { pos: 14 },
        );
        do_test_expr_err(
            r#" [ , ]-"#,
            SyntaxError::ListExpectedExpressionOrCloseBracket { pos: 3 },
        );
        do_test_expr_err(
            r#" [1, 2, 3, "bad-string"#,
            SyntaxError::StringMissingCloseQuote { pos: 11 },
        );
    }
}
