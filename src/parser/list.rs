use super::{ParseResult, Parser, SyntaxError};
use crate::lang::List;

const MISSING_END_BRACKET: &str = "Missing closing ']'.";
const EXPECTED_EXPRESSION_OR_CLOSE: &str = "Expected expression or ']'.";

impl<'a> Parser<'a> {
    /// Consume a list if next on input and return it.
    /// Otherwise consume nothing and return `None`
    // list = { "[" expr* "]"}
    pub(super) fn list(&mut self) -> ParseResult<Option<List>> {
        let start = self.pos;
        if !self.char(b'[') {
            return Ok(None);
        };
        self.whitespace_comments();

        let mut list = Vec::new();
        loop {
            if self.char(b']') {
                break;
            }

            let Some(expr) = self.expression()? else {
                if self.is_eoi() {
                    return Err(SyntaxError {
                        pos: start,
                        msg: MISSING_END_BRACKET,
                    });
                } else {
                    return Err(SyntaxError {
                        pos: start,
                        msg: EXPECTED_EXPRESSION_OR_CLOSE,
                    });
                }
            };

            list.push(expr);
            self.whitespace_comments(); // FIXME: allow comments

            if self.char(b',') {
                self.whitespace_comments(); // FIXME: allow comments
            }
        }

        Ok(Some(List::new(list)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::AstNode,
        parser::{SyntaxError, string::MISSING_END_QUOTE, tests::*},
    };
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn do_test_list_ok(input: &str, expected: List, remaining: usize) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(expected, p.list().expect("parse ok").expect("some result"));
        assert_eq!(input.len() - remaining, p.pos);
    }

    #[test]
    fn test_list_ok() {
        let expect = List::new(vec![val(i(1)), val(i(2)), val(i(3)), val(i(4))]);
        do_test_list_ok("-[1 2 3 4]-", expect.clone(), 1);
        do_test_list_ok("-[1,2,3,4]-", expect.clone(), 1);
        do_test_list_ok("-[ 1 , 2 , 3 , 4 ]-", expect.clone(), 1);
        do_test_list_ok(
            "-[ # comment \n 1 # comment \n 2 # comment \n # comment \n\n 3 # comment \n , # comment \n 4 ]-",
            expect.clone(),
            1,
        );
        do_test_list_ok(
            "-[# comment \n1# comment \n2# comment \n# comment \n\n3# comment \n,# comment \n4]-",
            expect,
            1,
        );

        let expect = List::new(vec![val(i(1)), val(nil()), val(b(false)), val(s("foo"))]);
        do_test_list_ok(r#"-[1 nil false "foo"]-"#, expect.clone(), 1);
        do_test_list_ok(r#"-[1, nil, false, "foo"]-"#, expect.clone(), 1);
        do_test_list_ok(r#"-[1 nil, false "foo",]-"#, expect.clone(), 1);
        do_test_list_ok(r#"-[]-"#, List::new(Vec::new()), 1);

        do_test_list_ok(
            "-[[1 1] 2 [3 3 3] 4]-",
            List::new(vec![
                AstNode::List(List::new(vec![val(i(1)), val(i(1))])),
                val(i(2)),
                AstNode::List(List::new(vec![val(i(3)), val(i(3)), val(i(3))])),
                val(i(4)),
            ]),
            1,
        );
    }

    #[test]
    fn test_list_none() {
        let mut p = Parser::new("--");
        p.pos = 1;
        assert_eq!(p.list(), Ok(None));
        assert_eq!(p.pos, 1);
    }

    #[track_caller]
    fn do_test_list_err(input: &str, err_pos: usize, err_msg: &'static str) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(
            SyntaxError {
                pos: err_pos,
                msg: err_msg
            },
            p.list().expect_err("expected error")
        );
    }

    #[test]
    fn test_list_err() {
        do_test_list_err(r#"-[1 2 3 4"#, 1, MISSING_END_BRACKET);
        do_test_list_err(r#"-[1 2 3 4 var "#, 1, EXPECTED_EXPRESSION_OR_CLOSE);
        do_test_list_err(r#"-[1 2 3 4 "bad-string"#, 10, MISSING_END_QUOTE);
    }
}
