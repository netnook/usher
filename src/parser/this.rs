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
    use crate::parser::tests::this;
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn do_test_this(input: &str, expected: Option<AstNode>, len: usize) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(expected, p.this());
        assert_eq!(len + 1, p.pos);
    }

    #[test]
    fn test_this() {
        do_test_this("_this_", Some(this()), 4);
        do_test_this("_this", Some(this()), 4);

        do_test_this("_thi_", None, 0);
        do_test_this("_", None, 0);
    }
}
