use super::{ParseResult, Parser, SyntaxError};
use crate::lang::ListBuilder;

const MISSING_END_BRACKET: &str = "Missing closing ']'.";
const EXPECTED_EXPRESSION_OR_CLOSE: &str = "Expected expression or ']'.";

impl<'a> Parser<'a> {
    /// Consume a list if next on input and return it.
    /// Otherwise consume nothing and return `None`
    // list = { "[" expr* "]"}
    // FIXME: require comma separator, but make it optional after last entry
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
                        msg: MISSING_END_BRACKET,
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
            }
        }

        Ok(Some(ListBuilder::new(entries)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{SyntaxError, string::MISSING_END_QUOTE, tests::*};
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn do_test_list_ok(input: &str, expected: ListBuilder, remaining: usize) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(expected, p.list().expect("parse ok").expect("some result"));
        assert_eq!(input.len() - remaining, p.pos);
    }

    #[test]
    fn test_list_ok() {
        let expect = ListBuilder::new(vec![i(1).into(), i(2).into(), i(3).into(), i(4).into()]);
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

        let expect = ListBuilder::new(vec![
            i(1).into(),
            nil().into(),
            b(false).into(),
            s("foo").into(),
        ]);
        do_test_list_ok(r#"-[1 nil false "foo"]-"#, expect.clone(), 1);
        do_test_list_ok(r#"-[1, nil, false, "foo"]-"#, expect.clone(), 1);
        do_test_list_ok(r#"-[1 nil, false "foo",]-"#, expect.clone(), 1);
        do_test_list_ok(r#"-[]-"#, ListBuilder::new(Vec::new()), 1);

        do_test_list_ok(
            "-[[1 1] 2 [3 3 3] 4]-",
            ListBuilder::new(vec![
                ListBuilder::new(vec![i(1).into(), i(1).into()]).into(),
                i(2).into(),
                ListBuilder::new(vec![i(3).into(), i(3).into(), i(3).into()]).into(),
                i(4).into(),
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
        do_test_list_err(r#"-[1 2 3 4 } "#, 10, EXPECTED_EXPRESSION_OR_CLOSE);
        do_test_list_err(r#"-[1 2 3 4 "bad-string"#, 10, MISSING_END_QUOTE);
    }
}
