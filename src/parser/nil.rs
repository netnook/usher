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
    use crate::parser::tests::nil;
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn do_test_nil(input: &str, expected: Option<Value>, len: usize) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(expected, p.nil());
        assert_eq!(len + 1, p.pos);
    }

    #[test]
    fn test_nil() {
        do_test_nil("_nil_", Some(nil()), 3);
        do_test_nil("_nil", Some(nil()), 3);

        do_test_nil("_ni_", None, 0);
        do_test_nil("_", None, 0);
    }
}
