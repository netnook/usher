use super::{Parser, chars::is_digit};
use crate::lang::Value;

impl<'a> Parser<'a> {
    /// Consume an integer if next on input and return it.
    /// Otherwise consume nothing and return `None`
    pub(super) fn integer(&mut self) -> Option<Value> {
        let start = self.pos;

        self.char(b'-'); // optional
        if self.repeat(is_digit) < 1 {
            self.pos = start;
            return None;
        };

        let v = String::from_utf8_lossy(&self.input[start..self.pos]);

        let v = v.parse().expect("parsed str to int should never fail");

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
    use crate::parser::tests::{do_test_opt_parser_none, do_test_opt_parser_some, i};

    #[test]
    fn test_integers() {
        do_test_opt_parser_some(Parser::integer, "_1_", i(1), 1);
        do_test_opt_parser_some(Parser::integer, "_1", i(1), 0);
        do_test_opt_parser_some(Parser::integer, "_-1_", i(-1), 1);
        do_test_opt_parser_some(Parser::integer, "_0_", i(0), 1);
        do_test_opt_parser_some(Parser::integer, "_1289_", i(1289), 1);
        do_test_opt_parser_some(Parser::integer, "_-1289_", i(-1289), 1);
        do_test_opt_parser_some(Parser::integer, "_-0_", i(0), 1);
        do_test_opt_parser_some(Parser::integer, "_000_", i(0), 1);
        do_test_opt_parser_some(Parser::integer, "_007_", i(7), 1);
        do_test_opt_parser_some(Parser::integer, "_-007_", i(-7), 1);

        do_test_opt_parser_some(Parser::integer, "_1.1_", i(1), 3);
        do_test_opt_parser_some(Parser::integer, "_-1.1_", i(-1), 3);

        do_test_opt_parser_none(Parser::integer, "_-x.0_");
        do_test_opt_parser_none(Parser::integer, "_x");
        do_test_opt_parser_none(Parser::integer, "_");
    }

    #[test]
    fn test_floats() {
        do_test_opt_parser_some(Parser::float, "_1.0_", Value::Float(1.0), 1);
        do_test_opt_parser_some(Parser::float, "_-1.0_", Value::Float(-1.0), 1);
        do_test_opt_parser_some(Parser::float, "_0._", Value::Float(0.0), 1);
        do_test_opt_parser_some(Parser::float, "_1289.0_", Value::Float(1289.0), 1);
        do_test_opt_parser_some(Parser::float, "_-1289.0_", Value::Float(-1289.0), 1);
        do_test_opt_parser_some(Parser::float, "_-0.000_", Value::Float(0.0), 1);
        do_test_opt_parser_some(Parser::float, "_000.00_", Value::Float(0.0), 1);
        do_test_opt_parser_some(Parser::float, "_007.123099_", Value::Float(7.123099), 1);
        do_test_opt_parser_some(Parser::float, "_-007.123099_", Value::Float(-7.123099), 1);

        do_test_opt_parser_none(Parser::float, "_10_");
        do_test_opt_parser_none(Parser::float, "_1");
        do_test_opt_parser_none(Parser::float, "_x1.1");
    }
}
