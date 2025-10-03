use crate::{
    lang::{Dict, EvalStop, Identifier, KeyValue, Span, value::ValueType},
    parser::{SourceRef, position},
};
use std::{cell::RefCell, fmt::Display, rc::Rc};
use thiserror::Error;

#[derive(PartialEq, Debug)]
pub struct EvalError<'a> {
    pub file: &'a str,
    pub source: &'a str,
    pub error: InternalProgramError,
}

impl<'a> EvalError<'a> {
    pub fn span(&self) -> Span {
        *self.error.span()
    }

    pub fn find_source_position(&self) -> SourceRef<'a> {
        position::find_source_position(self.file, self.source, self.span())
    }
}

#[derive(Error, PartialEq, Debug)]
pub enum InternalProgramError {
    #[error("Expected function but got {got}")]
    ExpectedFunction { got: ValueType, span: Span },
    #[error("No such method '{name}' on {from}.")]
    NoSuchMethod {
        name: String,
        from: ValueType,
        span: Span,
    },
    #[error("Method '{name}' not applicable to type {to}")]
    MethodNotApplicable {
        name: String,
        to: ValueType,
        span: Span,
    },
    #[error("'{op}' operator cannot be applied to a {got}")]
    SuffixOperatorDoesNotSupportOperand {
        op: &'static str,
        got: ValueType,
        span: Span,
    },
    #[error("Bad type. Expected '{expected}' but got {actual}.")]
    BadValueType {
        expected: ValueType,
        actual: ValueType,
        span: Span,
    },
    #[error("Operator {op} expected {expected} but got {actual}.")]
    BadValueTypeOp {
        op: &'static str,
        expected: ValueType,
        actual: ValueType,
        span: Span,
    },
    #[error("Index out of range.  Got {index}, length: {len}")]
    IndexOutOfRange {
        index: isize,
        len: usize,
        span: Span,
    },
    #[error("LHS is not assignable.")]
    CannotAssignToLHS { span: Span },
    #[error("Cannot convert a {typ} to a string.")]
    CannotConvertToString { typ: ValueType, span: Span },
    #[error("Cannot iterate on {got}.")]
    CannotLoopOnValue { got: ValueType, span: Span },
    #[error("This not available in current scope.")]
    ThisNotAvailable,
    #[error("Property '{prop}' not found. The following properties are available: {available}", prop = prop.key, available = from)]
    NoSuchProperty {
        prop: Identifier,
        from: PropertyList,
        span: Span,
    },
}

#[derive(Debug, PartialEq)]
pub enum PropertyList {
    Dict(Rc<RefCell<Dict>>),
    KeyValue(Rc<KeyValue>),
}

impl Display for PropertyList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PropertyList::Dict(v) => {
                let mut first = true;
                let v = v.borrow();
                let mut keys: Vec<_> = v.keys().collect::<Vec<_>>();
                keys.sort();
                for key in keys {
                    match first {
                        true => first = false,
                        false => f.write_str(", ")?,
                    }
                    f.write_str(&key.0)?
                }
                Ok(())
            }
            PropertyList::KeyValue(_) => f.write_str("key, value"),
        }
    }
}

macro_rules! bad_type_error_op {
    ($op:expr, LHS, $expected:expr, $actual:expr) => {
        InternalProgramError::BadValueTypeOp {
            op: $op.op.op_name(),
            expected: $expected,
            actual: $actual,
            span: $op.lhs.span().clone(),
        }
        .into()
    };
    ($op:expr, RHS, $expected:expr, $actual:expr) => {
        InternalProgramError::BadValueTypeOp {
            op: $op.op.op_name(),
            expected: $expected,
            actual: $actual,
            span: $op.rhs.span().clone(),
        }
        .into()
    };
    ($op:expr, $expected:expr, $actual:expr) => {
        InternalProgramError::BadValueTypeOp {
            op: $op.op.op_name(),
            expected: $expected,
            actual: $actual,
            span: $op.span().clone(),
        }
        .into()
    };
}
pub(crate) use bad_type_error_op;

impl InternalProgramError {
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
            InternalProgramError::CannotLoopOnValue { got: _, span } => span,
            InternalProgramError::ThisNotAvailable => todo!(),
            InternalProgramError::NoSuchProperty {
                prop: _,
                from: _,
                span,
            } => span,
        }
    }

    pub(crate) fn into_stop(self) -> EvalStop {
        self.into()
    }
}
