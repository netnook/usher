use super::Parser;
use crate::lang::AstNode;

impl<'a> Parser<'a> {
    /// Consume a "this" tag if next on input and return it.
    /// Otherwise consume nothing and return `None`
    pub(super) fn this(&mut self) -> Option<AstNode> {
        if self.tag(b"this") {
            return Some(AstNode::This);
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::{do_test_opt_parser_none, do_test_opt_parser_some, this};

    #[test]
    fn test_this() {
        do_test_opt_parser_some(Parser::this, "_this_", this(), 1);
        do_test_opt_parser_some(Parser::this, "_this", this(), 0);

        do_test_opt_parser_none(Parser::this, "_thi_");
        do_test_opt_parser_none(Parser::this, "_");
    }
}
