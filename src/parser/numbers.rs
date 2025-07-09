use super::{Parser, chars::is_digit};
use crate::lang::Value;

impl<'a> Parser<'a> {
    /// Consume an integer if next on input and return it.
    /// Otherwise consume nothing and return `None`
    pub(super) fn integer(&mut self) -> Option<Value> {
        let start = self.pos;
        self.char(b'-'); // optional -> ignore error
        if self.repeat(is_digit) < 1 {
            self.pos = start;
            return None;
        };

        let v = String::from_utf8_lossy(&self.input[start..self.pos]);

        let v = v.parse().expect("str to int");

        Some(Value::Integer(v))
    }

    /// Consume a float if next on input and return it.
    /// Otherwise consume nothing and return `None`
    pub(super) fn float(&mut self) -> Option<Value> {
        let start = self.pos;
        self.char(b'-'); // optional -> ignore error
        if self.repeat(is_digit) < 1 {
            self.pos = start;
            return None;
        };
        if !self.char(b'.') {
            self.pos = start;
            return None;
        };
        self.repeat(is_digit);

        let v = String::from_utf8_lossy(&self.input[start..self.pos]);

        let v = v.parse().expect("str to float");

        Some(Value::Float(v))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::tests::i;
    use pretty_assertions::assert_eq;

    #[track_caller]
    fn do_test_integer(input: &str, expected: Option<Value>, len: usize) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(expected, p.integer());
        assert_eq!(len + 1, p.pos);
    }

    #[test]
    fn test_integers() {
        do_test_integer("_1_", Some(i(1)), 1);
        do_test_integer("_1", Some(i(1)), 1);
        do_test_integer("_-1_", Some(i(-1)), 2);
        do_test_integer("_0_", Some(i(0)), 1);
        do_test_integer("_1289_", Some(i(1289)), 4);
        do_test_integer("_-1289_", Some(i(-1289)), 5);
        do_test_integer("_-0_", Some(i(0)), 2);
        do_test_integer("_000_", Some(i(0)), 3);
        do_test_integer("_007_", Some(i(7)), 3);
        do_test_integer("_-007_", Some(i(-7)), 4);

        do_test_integer("_1.1_", Some(i(1)), 1);
        do_test_integer("_-1.1_", Some(i(-1)), 2);

        do_test_integer("_-x.0_", None, 0);
        do_test_integer("_x", None, 0);
        do_test_integer("_", None, 0);
    }

    #[track_caller]
    fn do_test_floats(input: &str, expected: Option<Value>, len: usize) {
        let mut p = Parser::new(input);
        p.pos = 1;
        assert_eq!(expected, p.float());
        assert_eq!(len + 1, p.pos);
    }

    #[test]
    fn test_floats() {
        do_test_floats("_1.0_", Some(Value::Float(1.0)), 3);
        do_test_floats("_-1.0_", Some(Value::Float(-1.0)), 4);
        do_test_floats("_0._", Some(Value::Float(0.0)), 2);
        do_test_floats("_1289.0_", Some(Value::Float(1289.0)), 6);
        do_test_floats("_-1289.0_", Some(Value::Float(-1289.0)), 7);
        do_test_floats("_-0.000_", Some(Value::Float(0.0)), 6);
        do_test_floats("_000.00_", Some(Value::Float(0.0)), 6);
        do_test_floats("_007.123099_", Some(Value::Float(7.123099)), 10);
        do_test_floats("_-007.123099_", Some(Value::Float(-7.123099)), 11);

        do_test_floats("_10_", None, 0);
        do_test_floats("_1", None, 0);
        do_test_floats("_x1.1", None, 0);
    }
}
