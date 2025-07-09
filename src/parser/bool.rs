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
    use crate::parser::tests::b;
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn do_test_boolean(input: &str, expected: Option<Value>, len: usize) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(expected, p.boolean());
        assert_eq!(len + 1, p.pos);
    }

    #[test]
    fn test_boolean() {
        do_test_boolean("_true_", Some(b(true)), 4);
        do_test_boolean("_false_", Some(b(false)), 5);
        do_test_boolean("_true", Some(b(true)), 4);

        do_test_boolean("_tru_", None, 0);
        do_test_boolean("_", None, 0);
    }
}
