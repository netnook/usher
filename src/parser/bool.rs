use super::Parser;
use crate::lang::Value;

impl<'a> Parser<'a> {
    /// Consume a boolean if next on input and return it.
    /// Otherwise consume nothing and return `None`
    pub(super) fn boolean(&mut self) -> Option<Value> {
        let start = self.pos;
        match self.unchecked_identifier() {
            Some(b"true") => Some(Value::Bool(true)),
            Some(b"false") => Some(Value::Bool(false)),
            _ => {
                self.pos = start;
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::{b, do_test_opt_parser_none, do_test_opt_parser_some};

    #[test]
    fn test_boolean() {
        do_test_opt_parser_some(Parser::boolean, "-true-", b(true), 1);
        do_test_opt_parser_some(Parser::boolean, "-false-", b(false), 1);
        do_test_opt_parser_some(Parser::boolean, "-true", b(true), 0);

        do_test_opt_parser_none(Parser::boolean, "-tru-");
        do_test_opt_parser_none(Parser::boolean, "-falsey");
        do_test_opt_parser_none(Parser::boolean, "-");
    }
}
