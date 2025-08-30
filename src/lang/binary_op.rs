use std::fmt::Display;

use crate::lang::value::ValueType;

use super::{AstNode, Context, InternalProgramError, Span, Value};

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

macro_rules! bad_type_error {
    ($op:expr, LHS, $expected:expr, $actual:expr) => {
        return Err(InternalProgramError {
            msg: format!(
                "Expected {} but got {} on LHS of {} operation",
                $expected, $actual, $op.op,
            ),
            span: $op.lhs.span().clone(),
        })
    };
    ($op:expr, RHS, $expected:expr, $actual:expr) => {
        return Err(InternalProgramError {
            msg: format!(
                "Expected {} but got {} on RHS of {} operation",
                $expected, $actual, $op.op,
            ),
            span: $op.rhs.span().clone(),
        })
    };
}

impl BinaryOp {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        // Do the logical short-circuiting ops first, taking care to only evaluate rhs
        // if necessary
        match self.op {
            BinaryOpCode::And => {
                let lhs = self.lhs.eval(ctxt)?;
                let Value::Bool(lhs) = lhs else {
                    bad_type_error!(self, LHS, ValueType::Boolean, lhs.value_type());
                };

                if !lhs {
                    return Ok(Value::Bool(false));
                }

                let rhs = self.rhs.eval(ctxt)?;
                let Value::Bool(rhs) = rhs else {
                    bad_type_error!(self, RHS, ValueType::Boolean, rhs.value_type());
                };

                return Ok(Value::Bool(rhs));
            }
            BinaryOpCode::Or => {
                let lhs = self.lhs.eval(ctxt)?;
                let Value::Bool(lhs) = lhs else {
                    bad_type_error!(self, LHS, ValueType::Boolean, lhs.value_type());
                };

                if lhs {
                    return Ok(Value::Bool(true));
                }

                let rhs = self.rhs.eval(ctxt)?;
                let Value::Bool(rhs) = rhs else {
                    bad_type_error!(self, RHS, ValueType::Boolean, rhs.value_type());
                };

                return Ok(Value::Bool(rhs));
            }
            _ => {}
        };

        let lhs = self.lhs.eval(ctxt)?;
        let rhs = self.rhs.eval(ctxt)?;

        match (lhs, rhs) {
            (Value::Integer(lhs), Value::Integer(rhs)) => Ok(match self.op {
                BinaryOpCode::Add => Value::Integer(lhs + rhs),
                BinaryOpCode::Sub => Value::Integer(lhs - rhs),
                BinaryOpCode::Mul => Value::Integer(lhs * rhs),
                BinaryOpCode::Div => Value::Integer(lhs / rhs),
                BinaryOpCode::Mod => Value::Integer(lhs % rhs),
                _ => todo!(),
            }),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(match self.op {
                BinaryOpCode::Add => Value::Float(lhs + rhs),
                BinaryOpCode::Sub => Value::Float(lhs - rhs),
                BinaryOpCode::Mul => Value::Float(lhs * rhs),
                BinaryOpCode::Div => Value::Float(lhs / rhs),
                BinaryOpCode::Mod => Value::Float(lhs % rhs),
                _ => todo!(),
            }),
            (Value::Bool(_lhs), Value::Bool(_rhs)) => {
                // todo!()
                match self.op {
                    BinaryOpCode::Add => Err(self.cannot_perform_op(ValueType::Boolean)),
                    BinaryOpCode::Sub => todo!(),
                    BinaryOpCode::Mul => todo!(),
                    BinaryOpCode::Div => todo!(),
                    BinaryOpCode::Mod => todo!(),
                    BinaryOpCode::Equal => todo!(),
                    BinaryOpCode::NotEqual => todo!(),
                    BinaryOpCode::Greater => todo!(),
                    BinaryOpCode::GreaterOrEqual => todo!(),
                    BinaryOpCode::LessOrEqual => todo!(),
                    BinaryOpCode::Less => todo!(),
                    BinaryOpCode::And => todo!(),
                    BinaryOpCode::Or => todo!(),
                }
            }
            (lhs, rhs) => Err(InternalProgramError {
                msg: format!(
                    "Incompatible lhs ({}) and rhs ({}) for {} operation",
                    lhs.value_type(),
                    rhs.value_type(),
                    self.op,
                ),
                span: self.span.clone(),
            }),
        }

        // FIXME: finish eval impl of BinaryOp
    }

    fn cannot_perform_op(&self, arg_type: ValueType) -> InternalProgramError {
        InternalProgramError {
            msg: format!("Cannot perform {} on {}", self.op, arg_type),
            span: self.span.clone(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::{Context, Literal};
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
    fn test_eval_int() {
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
