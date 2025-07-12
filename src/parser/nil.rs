use super::Parser;
use crate::lang::Value;

impl<'a> Parser<'a> {
    /// Consume a nil literal if next on input and return it.
    /// Otherwise consume nothing and return `None`
    pub(super) fn nil(&mut self) -> Option<Value> {
        if self.tag(b"nil") {
            return Some(Value::Nil);
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::{do_test_opt_parser_none, do_test_opt_parser_some, nil};

    #[test]
    fn test_nil() {
        do_test_opt_parser_some(Parser::nil, "_nil_", nil(), 1);
        do_test_opt_parser_some(Parser::nil, "_nil", nil(), 0);

        do_test_opt_parser_none(Parser::nil, "_ni_");
        do_test_opt_parser_none(Parser::nil, "_");
    }
}
