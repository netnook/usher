use crate::lang::{AstNode, Context, InternalProgramError, Span, Value, value::ValueType};
use std::fmt::Display;

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOpCode {
    Not,
    Negative,
}

impl Display for UnaryOpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.op_name())
    }
}

impl UnaryOpCode {
    pub fn op_name(&self) -> &'static str {
        match self {
            UnaryOpCode::Not => "not",
            UnaryOpCode::Negative => "negative",
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnaryOp {
    pub(crate) op: UnaryOpCode,
    pub(crate) on: Box<AstNode>,
    pub(crate) span: Span,
}

macro_rules! bad_type_error {
    ($op:expr, $expected:expr, $actual:expr) => {
        return Err(InternalProgramError {
            msg: format!(
                "Expected {} argument for {} operation but got {}",
                $expected, $op.op, $actual,
            ),
            span: $op.span().clone(),
        })
    };
}

impl UnaryOp {
    pub fn span(&self) -> Span {
        Span::merge(self.span, self.on.span())
    }

    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        let on = self.on.eval(ctxt)?;

        let result = match self.op {
            UnaryOpCode::Not => match on {
                Value::Bool(v) => Value::Bool(!v),
                on => {
                    bad_type_error!(self, ValueType::Boolean, on.value_type())
                }
            },
            UnaryOpCode::Negative => match on {
                Value::Integer(v) => Value::Integer(-v),
                Value::Float(v) => Value::Float(-v),
                on => {
                    bad_type_error!(self, "number", on.value_type())
                }
            },
        };

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::UnaryOp;
    use crate::lang::{Context, Literal};
    use crate::parser::tests::*;

    #[track_caller]
    fn do_test_eval_ok(op: UnaryOp, expected: Literal) {
        assert_eq!(op.eval(&mut Context::new()).unwrap(), expected.val);
    }

    #[track_caller]
    fn do_test_eval_err(op: UnaryOp) {
        op.eval(&mut Context::new())
            .expect_err("expected error but none occured");
    }

    #[test]
    fn test_not() {
        do_test_eval_ok(not(b(true)), b(false));
        do_test_eval_ok(not(b(false)), b(true));

        do_test_eval_err(not(s("a")));
        do_test_eval_err(not(i(1)));
        do_test_eval_err(not(f(1.)));
        do_test_eval_err(not(nil()));
    }

    #[test]
    fn test_neg() {
        do_test_eval_ok(neg(i(42)), i(-42));
        do_test_eval_ok(neg(i(-42)), i(42));
        do_test_eval_ok(neg(i(0)), i(0));
        do_test_eval_ok(neg(f(42.)), f(-42.));
        do_test_eval_ok(neg(f(-42.2)), f(42.2));
        do_test_eval_ok(neg(f(0.)), f(0.));

        do_test_eval_err(neg(s("a")));
        do_test_eval_err(neg(b(true)));
        do_test_eval_err(neg(nil()));
    }
}
