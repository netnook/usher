use super::{ParseResult, Parser, SyntaxError};
use crate::lang::Program;

pub(crate) const EXPECTED_NEW_LINE_AFTER_STMT: &str = "Expected new line after statement.";
pub(crate) const UNEXPECTED_CHAR: &str = "Unexpected character.";

impl<'a> Parser<'a> {
    // stmt*
    pub(super) fn program(&mut self) -> ParseResult<Program<'a>> {
        let mut stmts = Vec::new();

        let mut first = true;
        loop {
            let details = self.whitespace_comments_detailed();

            if self.is_eoi() {
                break;
            }

            if first {
                first = false;
            } else if !details.newline {
                return Err(SyntaxError::new(self.pos, EXPECTED_NEW_LINE_AFTER_STMT));
            }

            let Some(stmt) = self.stmt()? else {
                return Err(SyntaxError::new(self.pos, UNEXPECTED_CHAR));
            };

            stmts.push(stmt);
        }

        Ok(Program {
            source: self.input_str,
            stmts,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lang::Program, parser::tests::*};
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn do_test_program_ok(input: &'static str, expected: Program) {
        let mut parser = Parser::new(input);
        parser.pos = 1;

        let actual = parser.program().expect("parser should succeed").print();
        let expected = expected.print();

        assert_eq!(actual, expected, "assert actual (left) == expected (right)");

        let expected_end = input.len();
        assert_eq!(
            parser.pos, expected_end,
            "assert actual_end ({}) == expected_end ({expected_end})",
            parser.pos
        );
    }

    #[track_caller]
    fn do_test_program_err(
        input: &'static str,
        expected_err_pos: usize,
        expected_err_msg: &'static str,
    ) {
        let mut parser = Parser::new(input);
        parser.pos = 1;

        let actual = parser.program().expect_err("parser should error");

        assert_eq!(
            actual,
            SyntaxError {
                pos: expected_err_pos,
                msg: expected_err_msg
            },
            "assert actual (left) == expected (right)"
        );
    }

    #[test]
    fn test_program() {
        do_test_program_ok(" 1 ", _prog![i!(1)]);
        do_test_program_ok(" 1 ", _prog![i!(1)]);
        do_test_program_ok("  1 \n 2 \n 3 ", _prog![i!(1), i!(2), i!(3)]);
        do_test_program_ok(" \n 1 \n 2 \n 3 \n ", _prog![i!(1), i!(2), i!(3)]);
        do_test_program_ok(" #foo\n 1 #bar\n #baz \n 2 ", _prog![i!(1), i!(2)]);
        do_test_program_ok(" var a = 1 ", _prog![var(id!("a"), i!(1))]);

        do_test_program_err(" 1 2 ", 3, EXPECTED_NEW_LINE_AFTER_STMT);
        do_test_program_err(" 1 \n ; ", 5, UNEXPECTED_CHAR);
    }
}
