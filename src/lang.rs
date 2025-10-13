mod binary_op;
mod block;
mod catch;
mod context;
mod dict;
mod errors;
mod function;
mod if_else;
mod list;
mod literal;
mod loops;
mod member;
mod node;
mod program;
mod string;
mod unary_op;
mod value;
mod var;
mod visitor;

pub use binary_op::{BinaryOp, BinaryOpCode};
pub use block::Block;
pub use catch::ChainCatch;
pub use context::{Context, Output};
pub use dict::DictBuilder;
pub(crate) use errors::bad_type_error_op;
pub use errors::{EvalError, InternalProgramError};
pub use function::{Arg, FunctionCall, FunctionDef, Param, ReturnStmt};
pub use if_else::{ConditionalBlock, IfElse};
pub use list::ListBuilder;
pub use literal::Literal;
pub use loops::{Break, Continue, For};
pub use member::{IndexOf, PropertyOf};
pub use node::AstNode;
pub use program::Program;
use std::rc::Rc;
pub use string::InterpolatedStr;
pub use unary_op::{UnaryOp, UnaryOpCode};
pub use value::{KeyValue, Value};
pub use var::{Assignment, Declaration, This, Var};
pub(crate) use visitor::{Accept, Visitor, VisitorResult, accept_default};

#[allow(unused_imports)]
pub(crate) use value::{Dict, List};

const THIS: &str = "this";

trait Eval {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop>;
}

#[derive(Debug, PartialEq)]
pub enum EvalStop {
    Error(InternalProgramError),
    Return(Value),
    Break(Span),
    Continue(Span),
    Throw,
}

impl From<InternalProgramError> for EvalStop {
    fn from(value: InternalProgramError) -> Self {
        Self::Error(value)
    }
}
impl<T> From<InternalProgramError> for Result<T, EvalStop> {
    fn from(value: InternalProgramError) -> Self {
        Err(value.into())
    }
}

trait Setter {
    fn set(&self, ctxt: &mut Context, value: Value) -> Result<(), EvalStop>;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    pub(crate) start: usize,
    pub(crate) len: usize,
}

impl Span {
    pub(crate) fn new(start: usize, len: usize) -> Self {
        Self { start, len }
    }
    pub(crate) fn start_end(start: usize, end: usize) -> Self {
        Self::new(start, end - start)
    }
    pub(crate) fn end(&self) -> usize {
        self.start + self.len
    }
    pub(crate) fn merge(mut a: Self, b: Self) -> Self {
        let start = a.start.min(b.start);
        let end = a.end().max(b.end());
        a.start = start;
        a.len = end - start;
        a
    }

    pub(crate) fn extended(mut self, pos: usize) -> Self {
        let start = self.start.min(pos);
        let end = self.end().max(pos);
        self.start = start;
        self.len = end - start;
        self
    }
}

#[derive(PartialEq, Eq, Clone)]
pub struct Identifier {
    pub(crate) key: Key,
    pub(crate) span: Span,
}

impl core::fmt::Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            write!(f, r#"Ident("{}")"#, self.key.0)
        } else {
            f.debug_struct("Identifier")
                .field("key", &self.key)
                .field("span", &self.span)
                .finish()
        }
    }
}

impl Identifier {
    pub(crate) fn new(name: String, span: Span) -> Self {
        Self {
            key: Key::new(name),
            span,
        }
    }

    pub(crate) fn as_string(&self) -> String {
        (*self.key.0).clone()
    }
}

#[derive(PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub struct Key(Rc<String>);

impl core::fmt::Debug for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl core::fmt::Display for Key {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Key {
    pub(crate) fn new(name: String) -> Self {
        Self(Rc::new(name))
    }

    fn is_this(&self) -> bool {
        *self.0 == THIS
    }

    fn as_string(&self) -> String {
        (*self.0).clone()
    }

    fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<&str> for Key {
    fn from(value: &str) -> Self {
        Self::new(value.to_string())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct KeyValueBuilder {
    pub(crate) key: Identifier,
    pub(crate) value: Box<AstNode>,
}

impl KeyValueBuilder {
    pub(crate) fn span(&self) -> Span {
        Span::merge(self.key.span, self.value.span())
    }
}

impl Eval for KeyValueBuilder {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        Ok(KeyValue::new(self.key.key.clone(), self.value.eval(ctxt)?).into())
    }
}

accept_default!(KeyValueBuilder, value:node,);

#[derive(PartialEq, Clone)]
pub struct End {
    pub(crate) span: Span,
}

impl End {
    pub(crate) fn new(span: Span) -> Self {
        Self { span }
    }
}

impl Eval for End {
    fn eval(&self, _: &mut Context) -> Result<Value, EvalStop> {
        Ok(Value::End)
    }
}

accept_default!(End);

impl core::fmt::Debug for End {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            write!(f, "End")
        } else {
            f.debug_struct("End").field("span", &self.span).finish()
        }
    }
}

#[cfg(test)]
mod tests {}
