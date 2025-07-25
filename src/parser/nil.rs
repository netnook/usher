use super::Parser;
use crate::lang::Value;

impl<'a> Parser<'a> {
    /// Consume a nil literal if next on input and return it.
    /// Otherwise consume nothing and return `None`
    pub(super) fn nil(&mut self) -> Option<Value> {
        let start = self.pos;
        match self.unchecked_identifier() {
            Some(b"nil") => Some(Value::Nil),
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
    use crate::parser::tests::{do_test_opt_parser_none, do_test_opt_parser_some, nil};

    #[test]
    fn test_nil() {
        do_test_opt_parser_some(Parser::nil, "-nil-", nil(), 1);
        do_test_opt_parser_some(Parser::nil, "-nil", nil(), 0);

        do_test_opt_parser_none(Parser::nil, "-ni-");
        do_test_opt_parser_none(Parser::nil, "-nil2-");
        do_test_opt_parser_none(Parser::nil, "-");
    }
}
