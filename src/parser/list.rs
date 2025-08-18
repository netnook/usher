use super::{ParseResult, Parser, SyntaxError};
use crate::lang::ListBuilder;

const MISSING_CLOSE: &str = "Missing closing ']'.";
const EXPECTED_EXPRESSION_OR_CLOSE: &str = "Expected expression or ']'.";
const EXPECTED_COMMA_OR_CLOSE: &str = "Expected ',' or ']'.";

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

            let Some(expr) = self.expression()? else {
                if self.is_eoi() {
                    return Err(SyntaxError {
                        pos: start,
                        msg: MISSING_CLOSE,
                    });
                } else {
                    return Err(SyntaxError {
                        pos: self.pos,
                        msg: EXPECTED_EXPRESSION_OR_CLOSE,
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
                return Err(SyntaxError {
                    pos: start,
                    msg: MISSING_CLOSE,
                });
            }

            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_COMMA_OR_CLOSE,
            });
        }

        Ok(Some(ListBuilder::new(entries)))
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
    fn test_list_ok() {
        let expect = ListBuilder::new(vec![i!(1).into(), i!(2).into(), i!(3).into(), i!(4).into()]);
        do_test_expr_ok(" [1,2,3,4] ", expect.clone(), -1);
        do_test_expr_ok(" [1,2,3,4,] ", expect.clone(), -1);
        do_test_expr_ok(" [ 1 , 2 , 3 , 4 ] ", expect.clone(), -1);
        do_test_expr_ok(r#" [] "#, ListBuilder::new(Vec::new()), -1);
        do_test_expr_ok(r#" [  ] "#, ListBuilder::new(Vec::new()), -1);
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

        let expect = ListBuilder::new(vec![
            i!(1).into(),
            nil!().into(),
            b!(false).into(),
            s!("foo").into(),
        ]);
        do_test_expr_ok(r#" [1,nil,false,"foo"] "#, expect.clone(), -1);
        do_test_expr_ok(r#" [1, nil, false, "foo"] "#, expect.clone(), -1);
        do_test_expr_ok(r#" [1, nil, false, "foo", ] "#, expect.clone(), -1);
        do_test_expr_ok(
            " [ # comment\n # comment \n \n ] ",
            ListBuilder::new(Vec::new()),
            -1,
        );
        do_test_expr_ok(
            " [[1, 1], 2, [3, 3, 3,], 4,] ",
            ListBuilder::new(vec![
                ListBuilder::new(vec![i!(1).into(), i!(1).into()]).into(),
                i!(2).into(),
                ListBuilder::new(vec![i!(3).into(), i!(3).into(), i!(3).into()]).into(),
                i!(4).into(),
            ]),
            -1,
        );
    }

    #[test]
    fn test_list_err() {
        do_test_expr_err(r#" [1,2,3,4"#, 1, MISSING_CLOSE);
        do_test_expr_err(r#" [1,2,3,"#, 1, MISSING_CLOSE);
        do_test_expr_err(r#" [1 2]-"#, 4, EXPECTED_COMMA_OR_CLOSE);
        do_test_expr_err(r#" [1, 2, 3, 4 } "#, 13, EXPECTED_COMMA_OR_CLOSE);
        do_test_expr_err(r#" [1, 2, 3, 4, } "#, 14, EXPECTED_EXPRESSION_OR_CLOSE);
        do_test_expr_err(r#" [ , ]-"#, 3, EXPECTED_EXPRESSION_OR_CLOSE);
        do_test_expr_err(r#" [1, 2, 3, "bad-string"#, 11, MISSING_END_QUOTE);
    }
}
