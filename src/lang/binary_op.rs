use super::{AstNode, Context, InternalProgramError, Span, Value};
use crate::lang::{Eval, bad_type_error_op, value::ValueType};
use std::fmt::Display;

#[derive(PartialEq, Debug, Clone)]
pub enum BinaryOpCode {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    LessOrEqual,
    Less,
    And,
    Or,
}

impl Display for BinaryOpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.op_name())
    }
}

impl BinaryOpCode {
    pub fn op_name(&self) -> &'static str {
        match self {
            BinaryOpCode::Add => "addition",
            BinaryOpCode::Sub => "subtraction",
            BinaryOpCode::Mul => "multiplication",
            BinaryOpCode::Div => "division",
            BinaryOpCode::Mod => "modulo",
            BinaryOpCode::Equal => "equal-to",
            BinaryOpCode::NotEqual => "not-equal-to",
            BinaryOpCode::Greater => "greater-than",
            BinaryOpCode::GreaterOrEqual => "greater-or-equal-to",
            BinaryOpCode::LessOrEqual => "less-or-equal-to",
            BinaryOpCode::Less => "less-than",
            BinaryOpCode::And => "logical-and",
            BinaryOpCode::Or => "logical-or",
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct BinaryOp {
    pub(crate) op: BinaryOpCode,
    pub(crate) lhs: Box<AstNode>,
    pub(crate) rhs: Box<AstNode>,
    pub(crate) span: Span,
}

impl BinaryOp {
    pub fn span(&self) -> Span {
        Span::merge(self.lhs.span(), self.rhs.span())
    }
}
impl Eval for BinaryOp {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        // Do the logical short-circuiting ops first, taking care to only evaluate rhs
        // if necessary
        match self.op {
            BinaryOpCode::And => {
                let lhs = self.lhs.eval(ctxt)?;
                let Value::Bool(lhs) = lhs else {
                    return bad_type_error_op!(self, LHS, ValueType::Boolean, lhs.value_type());
                };

                if !lhs {
                    return Ok(Value::Bool(false));
                }

                let rhs = self.rhs.eval(ctxt)?;
                let Value::Bool(rhs) = rhs else {
                    return bad_type_error_op!(self, RHS, ValueType::Boolean, rhs.value_type());
                };

                return Ok(Value::Bool(rhs));
            }
            BinaryOpCode::Or => {
                let lhs = self.lhs.eval(ctxt)?;
                let Value::Bool(lhs) = lhs else {
                    return bad_type_error_op!(self, LHS, ValueType::Boolean, lhs.value_type());
                };

                if lhs {
                    return Ok(Value::Bool(true));
                }

                let rhs = self.rhs.eval(ctxt)?;
                let Value::Bool(rhs) = rhs else {
                    return bad_type_error_op!(self, RHS, ValueType::Boolean, rhs.value_type());
                };

                return Ok(Value::Bool(rhs));
            }
            _ => {}
        };

        let lhs = self.lhs.eval(ctxt)?;
        let rhs = self.rhs.eval(ctxt)?;

        macro_rules! maths_op {
            ($op:tt,  $lhs:expr, $rhs:expr) => {
                match ($lhs, $rhs) {
                    (Value::Integer(lhs), Value::Integer(rhs)) => Value::Integer(lhs $op rhs),
                    (Value::Float(lhs), Value::Float(rhs)) => Value::Float(lhs $op rhs),
                    (Value::Integer(lhs), Value::Float(rhs)) => Value::Float((lhs as f64) $op rhs),
                    (Value::Float(lhs), Value::Integer(rhs)) => Value::Float(lhs $op (rhs as f64)),
                    (Value::Integer(_), rhs) => return bad_type_error_op!(self, RHS, ValueType::Number, rhs.value_type()),
                    (Value::Float(_), rhs) => return bad_type_error_op!(self, RHS, ValueType::Number, rhs.value_type()),
                    (lhs, _) => return bad_type_error_op!(self, LHS, ValueType::Number, lhs.value_type()),

                }
            };
        }

        macro_rules! greater_lesser_op {
            ($op:tt,  $lhs:expr, $rhs:expr) => {
                match ($lhs, $rhs) {
                    (Value::Integer(lhs), Value::Integer(rhs)) => Value::Bool(lhs $op rhs),
                    (Value::Float(lhs), Value::Float(rhs)) => Value::Bool(lhs $op rhs),
                    (Value::Integer(lhs), Value::Float(rhs)) => Value::Bool((lhs as f64) $op rhs),
                    (Value::Float(lhs), Value::Integer(rhs)) => Value::Bool(lhs $op (rhs as f64)),
                    (Value::Integer(_), rhs) => return bad_type_error_op!(self, RHS, ValueType::Number, rhs.value_type()),
                    (Value::Float(_), rhs) => return bad_type_error_op!(self, RHS, ValueType::Number, rhs.value_type()),
                    (lhs, _) => return bad_type_error_op!(self, LHS, ValueType::Number, lhs.value_type()),
                }
            };
        }

        macro_rules! equals_op {
            ($op:tt,  $lhs:expr, $rhs:expr) => {
                match ($lhs, $rhs) {
                    (Value::Integer(lhs), Value::Integer(rhs)) => Value::Bool(lhs $op rhs),
                    (Value::Float(lhs), Value::Float(rhs)) => Value::Bool(lhs $op rhs),
                    (Value::Integer(lhs), Value::Float(rhs)) => Value::Bool((lhs as f64) $op rhs),
                    (Value::Float(lhs), Value::Integer(rhs)) => Value::Bool(lhs $op (rhs as f64)),
                    (Value::Str(lhs), Value::Str(rhs)) => Value::Bool(lhs $op rhs),
                    (Value::Bool(lhs), Value::Bool(rhs)) => Value::Bool(lhs $op rhs),
                    (Value::Nil, Value::Nil) => Value::Bool(true $op true),
                    (_, _) => Value::Bool(true $op false),
                }
            };
        }

        let result = match self.op {
            BinaryOpCode::Add => maths_op!(+, lhs, rhs),
            BinaryOpCode::Sub => maths_op!(-, lhs, rhs),
            BinaryOpCode::Mul => maths_op!(*, lhs, rhs),
            BinaryOpCode::Div => maths_op!(/, lhs, rhs),
            BinaryOpCode::Mod => maths_op!(%, lhs, rhs),

            BinaryOpCode::Greater => greater_lesser_op!(>, lhs, rhs),
            BinaryOpCode::GreaterOrEqual => greater_lesser_op!(>=, lhs, rhs),
            BinaryOpCode::LessOrEqual => greater_lesser_op!(<=, lhs, rhs),
            BinaryOpCode::Less => greater_lesser_op!(<, lhs, rhs),

            BinaryOpCode::Equal => equals_op!(==,lhs, rhs),
            BinaryOpCode::NotEqual => equals_op!(!=,lhs, rhs),

            // Both of these should have been handled in the boolean short-circuit handler above
            BinaryOpCode::And => unreachable!(),
            BinaryOpCode::Or => unreachable!(),
        };

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::{Context, Eval, Literal};
    use crate::parser::tests::*;

    use super::BinaryOp;

    #[track_caller]
    fn do_test_eval_ok(op: BinaryOp, expected: Literal) {
        assert_eq!(op.eval(&mut Context::new()).unwrap(), expected.val);
    }

    #[track_caller]
    fn do_test_eval_err(op: BinaryOp) {
        op.eval(&mut Context::new())
            .expect_err("expected error but none occured");
    }

    #[test]
    fn test_eval_maths() {
        do_test_eval_ok(add(i(3), i(4)), i(3 + 4));
        do_test_eval_ok(sub(i(3), i(4)), i(3 - 4));
        do_test_eval_ok(mul(i(3), i(4)), i(3 * 4));
        do_test_eval_ok(div(i(13), i(5)), i(13 / 5));
        do_test_eval_ok(modulo(i(13), i(5)), i(13 % 5));

        do_test_eval_ok(add(f(3.1), f(4.2)), f(3.1 + 4.2));
        do_test_eval_ok(sub(f(3.1), f(4.2)), f(3.1 - 4.2));
        do_test_eval_ok(mul(f(3.1), f(4.2)), f(3.1 * 4.2));
        do_test_eval_ok(div(f(3.1), f(4.2)), f(3.1 / 4.2));
        do_test_eval_ok(modulo(f(3.1), f(1.2)), f(3.1 % 1.2));

        // do_test_eval_ok(add(s("a"), s("b")), s("ab"));
        do_test_eval_err(add(b(true), b(false)));
    }

    #[test]
    fn test_eval_comparison() {
        do_test_eval_ok(greater(i(3), i(4)), b(false));
        do_test_eval_ok(greater(i(4), i(4)), b(false));
        do_test_eval_ok(greater(i(4), i(3)), b(true));

        do_test_eval_ok(greater_equal(i(3), i(4)), b(false));
        do_test_eval_ok(greater_equal(i(4), i(4)), b(true));
        do_test_eval_ok(greater_equal(i(4), i(3)), b(true));

        do_test_eval_ok(less_equal(i(3), i(4)), b(true));
        do_test_eval_ok(less_equal(i(4), i(4)), b(true));
        do_test_eval_ok(less_equal(i(4), i(3)), b(false));

        do_test_eval_ok(less(i(3), i(4)), b(true));
        do_test_eval_ok(less(i(4), i(4)), b(false));
        do_test_eval_ok(less(i(4), i(3)), b(false));

        do_test_eval_ok(greater(f(3.1), f(4.1)), b(false));
        do_test_eval_ok(greater(f(4.1), f(4.1)), b(false));
        do_test_eval_ok(greater(f(4.1), f(3.1)), b(true));

        do_test_eval_ok(greater_equal(f(3.1), f(4.1)), b(false));
        do_test_eval_ok(greater_equal(f(4.1), f(4.1)), b(true));
        do_test_eval_ok(greater_equal(f(4.1), f(3.1)), b(true));

        do_test_eval_ok(less_equal(f(3.1), f(4.1)), b(true));
        do_test_eval_ok(less_equal(f(4.1), f(4.1)), b(true));
        do_test_eval_ok(less_equal(f(4.1), f(3.1)), b(false));

        do_test_eval_ok(less(f(3.1), f(4.1)), b(true));
        do_test_eval_ok(less(f(4.1), f(4.1)), b(false));
        do_test_eval_ok(less(f(4.1), f(3.1)), b(false));

        do_test_eval_ok(greater(i(3), f(4.0)), b(false));
        do_test_eval_ok(greater(i(3), f(3.0)), b(false));
        do_test_eval_ok(greater(i(3), f(2.0)), b(true));
        do_test_eval_ok(less_equal(f(2.9), i(3)), b(true));
        do_test_eval_ok(less_equal(f(3.0), i(3)), b(true));
        do_test_eval_ok(less_equal(f(3.1), i(3)), b(false));

        do_test_eval_err(less_equal(i(3), b(false)));
        do_test_eval_err(greater(i(3), s("3")));
    }

    #[test]
    fn test_eval_equal() {
        do_test_eval_ok(equal(i(3), i(4)), b(false));
        do_test_eval_ok(equal(i(3), i(3)), b(true));
        do_test_eval_ok(not_equal(i(3), i(4)), b(true));
        do_test_eval_ok(not_equal(i(3), i(3)), b(false));

        do_test_eval_ok(equal(s("aaa"), s("bbb")), b(false));
        do_test_eval_ok(equal(s("aaa"), s("aaa")), b(true));
        do_test_eval_ok(not_equal(s("aaa"), s("bbb")), b(true));
        do_test_eval_ok(not_equal(s("aaa"), s("aaa")), b(false));

        do_test_eval_ok(equal(b(true), b(false)), b(false));
        do_test_eval_ok(equal(b(false), b(false)), b(true));
        do_test_eval_ok(not_equal(b(true), b(false)), b(true));
        do_test_eval_ok(not_equal(b(false), b(false)), b(false));

        do_test_eval_ok(equal(nil(), nil()), b(true));
        do_test_eval_ok(not_equal(nil(), nil()), b(false));

        do_test_eval_ok(equal(nil(), b(true)), b(false));

        do_test_eval_ok(equal(i(3), s("3")), b(false));
        do_test_eval_ok(equal(i(3), b(false)), b(false));
        do_test_eval_ok(equal(i(3), nil()), b(false));
        do_test_eval_ok(not_equal(i(3), s("3")), b(true));
        do_test_eval_ok(not_equal(i(3), b(false)), b(true));
        do_test_eval_ok(not_equal(i(3), nil()), b(true));

        do_test_eval_ok(equal(s("aaa"), i(3)), b(false));
        do_test_eval_ok(equal(s("aaa"), b(false)), b(false));
        do_test_eval_ok(equal(s("aaa"), nil()), b(false));
        do_test_eval_ok(not_equal(s("aaa"), i(3)), b(true));
        do_test_eval_ok(not_equal(s("aaa"), b(false)), b(true));
        do_test_eval_ok(not_equal(s("aaa"), nil()), b(true));

        do_test_eval_ok(equal(b(true), i(3)), b(false));
        do_test_eval_ok(equal(b(true), s("aaa")), b(false));
        do_test_eval_ok(equal(b(true), nil()), b(false));
        do_test_eval_ok(not_equal(b(true), i(3)), b(true));
        do_test_eval_ok(not_equal(b(true), s("aaa")), b(true));
        do_test_eval_ok(not_equal(b(true), nil()), b(true));

        do_test_eval_ok(equal(nil(), i(3)), b(false));
        do_test_eval_ok(equal(nil(), s("aaa")), b(false));
        do_test_eval_ok(equal(nil(), b(true)), b(false));
        do_test_eval_ok(not_equal(nil(), i(3)), b(true));
        do_test_eval_ok(not_equal(nil(), s("aaa")), b(true));
        do_test_eval_ok(not_equal(nil(), b(true)), b(true));
    }

    #[test]
    fn test_eval_and() {
        do_test_eval_ok(and(b(false), b(false)), b(false));
        do_test_eval_ok(and(b(false), b(true)), b(false));
        do_test_eval_ok(and(b(true), b(false)), b(false));
        do_test_eval_ok(and(b(true), b(true)), b(true));

        do_test_eval_ok(and(b(false), s("x")), b(false));

        do_test_eval_err(and(s("x"), b(false)));
        do_test_eval_err(and(b(true), s("x")));
    }

    #[test]
    fn test_eval_or() {
        do_test_eval_ok(or(b(false), b(false)), b(false));
        do_test_eval_ok(or(b(false), b(true)), b(true));
        do_test_eval_ok(or(b(true), b(false)), b(true));
        do_test_eval_ok(or(b(true), b(true)), b(true));

        do_test_eval_ok(or(b(true), s("x")), b(true));

        do_test_eval_err(or(s("x"), b(false)));
        do_test_eval_err(or(b(false), s("x")));
    }
}
