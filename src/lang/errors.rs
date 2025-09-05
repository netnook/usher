use crate::lang::{Identifier, Span, value::ValueType};

#[derive(PartialEq, Debug)]
pub struct ProgramError {
    pub(crate) msg: String,
    pub(crate) line_no: usize,
    pub(crate) char_no: usize,
    pub(crate) line: String,
}

#[derive(PartialEq, Debug)]
pub enum InternalProgramError {
    ExpectedFunction {
        got: ValueType,
        span: Span,
    },
    NoSuchMethod {
        name: Identifier,
        from: ValueType,
        span: Span,
    },
    MethodNotApplicable {
        name: String,
        to: ValueType,
        span: Span,
    },
    SuffixOperatorDoesNotSupportOperand {
        op: &'static str,
        got: ValueType,
        span: Span,
    },
    BadValueType {
        expected: ValueType,
        actual: ValueType,
        span: Span,
    },
    BadValueTypeOp {
        op: &'static str,
        expected: ValueType,
        actual: ValueType,
        span: Span,
    },
    IndexOutOfRange {
        index: isize,
        len: usize,
        span: Span,
    },
    CannotAssignToLHS {
        span: Span,
    },
    CannotConvertToString {
        typ: ValueType,
        span: Span,
    },
}

macro_rules! bad_type_error_op {
    ($op:expr, LHS, $expected:expr, $actual:expr) => {
        Err(InternalProgramError::BadValueTypeOp {
            op: $op.op.op_name(),
            expected: $expected,
            actual: $actual,
            span: $op.lhs.span().clone(),
        })
    };
    ($op:expr, RHS, $expected:expr, $actual:expr) => {
        Err(InternalProgramError::BadValueTypeOp {
            op: $op.op.op_name(),
            expected: $expected,
            actual: $actual,
            span: $op.rhs.span().clone(),
        })
    };
    ($op:expr, $expected:expr, $actual:expr) => {
        Err(InternalProgramError::BadValueTypeOp {
            op: $op.op.op_name(),
            expected: $expected,
            actual: $actual,
            span: $op.span().clone(),
        })
    };
}
pub(crate) use bad_type_error_op;

impl InternalProgramError {
    pub fn message(&self) -> String {
        match self {
            InternalProgramError::ExpectedFunction { got, span: _ } => {
                format!("Expected function but got {got}")
            }
            InternalProgramError::NoSuchMethod {
                name,
                from,
                span: _,
            } => {
                format!("Cannot find method {name} on {from}", name = name.name)
            }
            InternalProgramError::MethodNotApplicable { name, to, span: _ } => {
                format!("Method '{name}' not applicable to type {to}")
            }
            InternalProgramError::SuffixOperatorDoesNotSupportOperand { op, got, span: _ } => {
                format!("'{op}' operator cannot be applied to a {got}")
            }
            InternalProgramError::BadValueType {
                expected,
                actual,
                span: _,
            } => {
                format!("Bad type. Expected '{expected}' but got {actual}.",)
            }
            InternalProgramError::BadValueTypeOp {
                op,
                expected,
                actual,
                span: _,
            } => {
                format!("Operator {op} expected {expected} but got {actual}.",)
            }
            InternalProgramError::IndexOutOfRange {
                index,
                len,
                span: _,
            } => {
                format!("Index out of range.  Got {index}, length: {len}")
            }
            InternalProgramError::CannotAssignToLHS { span: _ } => {
                "LHS is not assignable.".to_string()
            }
            InternalProgramError::CannotConvertToString { typ, span: _ } => {
                format!("Cannot convert a {typ} to a string.")
            }
        }
    }

    pub(crate) fn span(&self) -> &Span {
        match self {
            InternalProgramError::ExpectedFunction { got: _, span } => span,
            InternalProgramError::NoSuchMethod {
                name: _,
                from: _,
                span,
            } => span,
            InternalProgramError::MethodNotApplicable {
                name: _,
                to: _,
                span,
            } => span,
            InternalProgramError::SuffixOperatorDoesNotSupportOperand {
                op: _,
                got: _,
                span,
            } => span,
            InternalProgramError::BadValueType {
                expected: _,
                actual: _,
                span,
            } => span,
            InternalProgramError::BadValueTypeOp {
                op: _,
                expected: _,
                actual: _,
                span,
            } => span,
            InternalProgramError::IndexOutOfRange {
                index: _,
                len: _,
                span,
            } => span,
            InternalProgramError::CannotAssignToLHS { span } => span,
            InternalProgramError::CannotConvertToString { typ: _, span } => span,
        }
    }
    // pub fn
}
