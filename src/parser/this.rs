use super::Parser;
use crate::lang::AstNode;

impl<'a> Parser<'a> {
    /// Consume a "this" tag if next on input and return it.
    /// Otherwise consume nothing and return `None`
    pub(super) fn this(&mut self) -> Option<AstNode> {
        let start = self.pos;
        match self.unchecked_identifier() {
            Some(b"this") => Some(AstNode::This),
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
    use crate::parser::tests::{do_test_opt_parser_none, do_test_opt_parser_some, this};

    #[test]
    fn test_this() {
        do_test_opt_parser_some(Parser::this, "-this-", this(), 1);
        do_test_opt_parser_some(Parser::this, "-this", this(), 0);

        do_test_opt_parser_none(Parser::this, "-thiss-");
        do_test_opt_parser_none(Parser::this, "-");
    }
}
