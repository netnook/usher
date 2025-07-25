use super::{ParseResult, Parser, SyntaxError};
use crate::lang::ListBuilder;

const MISSING_END_PARENS: &str = "Missing closing ')'.";
const EXPECTED_OPEN_PARENS: &str = "Expected opening '('.";
const EXPECTED_EXPRESSION_OR_CLOSE: &str = "Expected expression or ')'.";
const EXPECTED_COMMA_OR_CLOSE: &str = "Expected ',' or ')'.";

impl<'a> Parser<'a> {
    /// Consume a list arguments if next on input and return it.
    /// Already passed "list" when called
    /// "(" expr,* ,? ")"
    pub(super) fn list(&mut self) -> ParseResult<ListBuilder> {
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

            let Some(expr) = self.expression()? else {
                if self.is_eoi() {
                    return Err(SyntaxError {
                        pos: open_pos,
                        msg: MISSING_END_PARENS,
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

        Ok(ListBuilder::new(entries))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::AstNode,
        parser::{
            expression::tests::{do_test_expr_err, do_test_expr_ok},
            string::MISSING_END_QUOTE,
            tests::*,
        },
    };

    #[test]
    fn test_list_ok() {
        let expect: AstNode =
            ListBuilder::new(vec![i(1).into(), i(2).into(), i(3).into(), i(4).into()]).into();
        do_test_expr_ok(" list(1,2,3,4) ", expect.clone(), -1);
        do_test_expr_ok(" list(1,2,3,4,) ", expect.clone(), -1);
        do_test_expr_ok(" list( 1 , 2 , 3 , 4 ) ", expect.clone(), -1);
        do_test_expr_ok(" list ( 1 , 2 , 3 , 4 ) ", expect.clone(), -1);
        do_test_expr_ok(r#" list() "#, ListBuilder::new(Vec::new()), -1);
        do_test_expr_ok(r#" list(  ) "#, ListBuilder::new(Vec::new()), -1);
        do_test_expr_ok(
            " list( # comment \n 1 , # comment \n 2 # comment \n , # comment \n\n 3 # comment \n , # comment \n 4 ) ",
            expect.clone(),
            -1,
        );
        do_test_expr_ok(
            " list(# comment \n1# comment \n,2,# comment \n# comment \n\n3# comment \n,# comment \n4,) ",
            expect,
            -1,
        );

        let expect = ListBuilder::new(vec![
            i(1).into(),
            nil().into(),
            b(false).into(),
            s("foo").into(),
        ]);
        do_test_expr_ok(r#" list(1,nil,false,"foo") "#, expect.clone(), -1);
        do_test_expr_ok(r#" list(1, nil, false, "foo") "#, expect.clone(), -1);
        do_test_expr_ok(r#" list(1, nil, false, "foo", ) "#, expect.clone(), -1);
        do_test_expr_ok(
            " list( # comment\n # comment \n \n ) ",
            ListBuilder::new(Vec::new()),
            -1,
        );
        do_test_expr_ok(
            " list(list(1, 1), 2, list(3, 3, 3,), 4,) ",
            ListBuilder::new(vec![
                ListBuilder::new(vec![i(1).into(), i(1).into()]).into(),
                i(2).into(),
                ListBuilder::new(vec![i(3).into(), i(3).into(), i(3).into()]).into(),
                i(4).into(),
            ]),
            -1,
        );
    }

    #[test]
    fn test_list_err() {
        do_test_expr_err(r#" list(1,2,3,4"#, 5, MISSING_END_PARENS);
        do_test_expr_err(r#" list(1,2,3,"#, 5, MISSING_END_PARENS);
        do_test_expr_err(r#" list(1 2) "#, 8, EXPECTED_COMMA_OR_CLOSE);
        do_test_expr_err(" list #comment \n(1,2) ", 6, EXPECTED_OPEN_PARENS);
        do_test_expr_err(r#" list(1, 2, 3, 4 } "#, 17, EXPECTED_COMMA_OR_CLOSE);
        do_test_expr_err(r#" list(1, 2, 3, 4, } "#, 18, EXPECTED_EXPRESSION_OR_CLOSE);
        do_test_expr_err(r#" list( , ) "#, 7, EXPECTED_EXPRESSION_OR_CLOSE);
        do_test_expr_err(r#" list(1, 2, 3, "bad string"#, 15, MISSING_END_QUOTE);
    }
}
