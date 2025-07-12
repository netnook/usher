use super::Parser;
use crate::lang::Value;

impl<'a> Parser<'a> {
    /// Consume a boolean if next on input and return it.
    /// Otherwise consume nothing and return `None`
    pub(super) fn boolean(&mut self) -> Option<Value> {
        if self.tag(b"true") {
            return Some(Value::Bool(true));
        }
        if self.tag(b"false") {
            return Some(Value::Bool(false));
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::{b, do_test_opt_parser_none, do_test_opt_parser_some};

    #[test]
    fn test_boolean() {
        do_test_opt_parser_some(Parser::boolean, "_true_", b(true), 1);
        do_test_opt_parser_some(Parser::boolean, "_false_", b(false), 1);
        do_test_opt_parser_some(Parser::boolean, "_true", b(true), 0);

        do_test_opt_parser_none(Parser::boolean, "_tru_");
        do_test_opt_parser_none(Parser::boolean, "_");
    }
}
