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
pub use context::Context;
pub use dict::DictBuilder;
pub(crate) use errors::bad_type_error_op;
pub use errors::{InternalProgramError, ProgramError};
pub use function::{Arg, BuiltInFunc, FunctionCall, FunctionDef, Param, ReturnStmt};
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
    Break,
    Continue,
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
    // FIXME: Rc<String>
    pub(crate) name: String,
    pub(crate) span: Span,
}

impl core::fmt::Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            write!(f, r#"Ident("{}")"#, self.name)
        } else {
            f.debug_struct("Identifier")
                .field("name", &self.name)
                .field("span", &self.span)
                .finish()
        }
    }
}

impl Identifier {
    pub(crate) const fn new(name: String, span: Span) -> Self {
        Self { name, span }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct KeyValueBuilder {
    pub(crate) key: Identifier,
    pub(crate) value: Box<AstNode>,
}

impl Eval for KeyValueBuilder {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        Ok(Value::KeyValue(Rc::new(KeyValue::new(
            self.key.name.clone(),
            self.value.eval(ctxt)?,
        ))))
    }
}

accept_default!(KeyValueBuilder, value:node,);

#[derive(PartialEq, Debug, Clone)]
pub struct End {}

impl End {
    pub(crate) fn new() -> Self {
        Self {}
    }
}

impl Eval for End {
    fn eval(&self, _: &mut Context) -> Result<Value, EvalStop> {
        Ok(Value::End)
    }
}

accept_default!(End);

#[cfg(test)]
mod tests {}
