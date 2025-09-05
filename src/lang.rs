mod binary_op;
mod block;
mod dict;
mod errors;
mod function;
mod if_else;
mod list;
mod loops;
mod member;
mod string;
mod unary_op;
mod value;
mod var;

use crate::{
    find_source_position,
    lang::{
        loops::{Break, Continue},
        value::ValueType,
    },
};
pub use binary_op::{BinaryOp, BinaryOpCode};
pub use block::Block;
pub use dict::DictBuilder;
pub(crate) use errors::bad_type_error_op;
pub use errors::{InternalProgramError, ProgramError};
pub use function::{FunctionDef, Param};
pub use if_else::{ConditionalBlock, IfElseStmt};
pub use list::ListBuilder;
pub use loops::ForStmt;
pub use member::{IndexOf, PropertyOf};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
pub use string::InterpolatedStr;
pub use unary_op::{UnaryOp, UnaryOpCode};
pub use value::Value;
pub use var::{Assignment, Declaration};

#[derive(Debug, Default)]
pub struct Context {
    pub inner: Rc<RefCell<ContextInner>>,
}

const THIS: &str = "this";

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&mut self, ident: &Identifier) -> Option<Value> {
        self.inner.borrow().get(&ident.name)
    }

    pub fn set(&mut self, ident: &Identifier, value: Value) {
        if ident.name == THIS {
            panic!("should not be able to modify 'this'")
        }
        self.inner.borrow_mut().set(&ident.name, value);
    }

    pub fn declare(&mut self, ident: &Identifier, value: Value) {
        if ident.name == THIS {
            panic!("should not be able to declare 'this'")
        }
        self.inner.borrow_mut().declare(&ident.name, value);
    }

    fn get_this(&self) -> Option<Value> {
        self.inner.borrow().get(THIS)
    }

    fn declare_this(&self, value: Value) {
        self.inner.borrow_mut().declare(THIS, value);
    }

    pub fn reset(&mut self) {
        self.inner.borrow_mut().reset();
    }

    fn new_child(&self) -> Self {
        Self {
            inner: Rc::new(RefCell::new(ContextInner {
                parent: Some(Rc::clone(&self.inner)),
                vars: HashMap::default(),
            })),
        }
    }
}

#[derive(Debug, Default)]
pub struct ContextInner {
    pub parent: Option<Rc<RefCell<ContextInner>>>,
    pub vars: HashMap<String, Value>,
}

impl ContextInner {
    fn get(&self, ident: &str) -> Option<Value> {
        if let Some(v) = self.vars.get(ident) {
            return Some(v.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().get(ident);
        }
        None
    }

    fn set(&mut self, ident: &str, value: Value) {
        if let Some(v) = self.do_set(ident, value) {
            self.vars.insert(ident.to_string(), v);
        }
    }

    fn do_set(&mut self, ident: &str, value: Value) -> Option<Value> {
        if self.vars.contains_key(ident) {
            self.vars.insert(ident.to_string(), value);
            return None;
        }
        if let Some(parent) = &self.parent {
            return parent.borrow_mut().do_set(ident, value);
        }
        Some(value)
    }

    fn declare(&mut self, ident: &str, value: Value) {
        self.vars.insert(ident.to_string(), value);
    }

    fn reset(&mut self) {
        self.vars.clear();
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Program<'a> {
    pub source: &'a str,
    pub stmts: Vec<AstNode>,
}

impl<'a> Program<'a> {
    pub(crate) fn run(&self) -> Result<Value, ProgramError> {
        match self.do_run() {
            Ok(v) => Ok(v),
            Err(EvalStop::Return(v)) => Ok(v),
            Err(EvalStop::Error(e)) => {
                let info = find_source_position(self.source, e.span().start);
                Err(ProgramError {
                    msg: e.message(),
                    line_no: info.0.line,
                    char_no: info.0.char,
                    line: info.1.to_string(),
                })
            }
            Err(v) => panic!("unexpected program response {v:?}. This is a bug!"),
        }
    }
    fn do_run(&self) -> Result<Value, EvalStop> {
        let mut ctxt = Context::new();
        let mut res = Value::Nil;
        for stmt in &self.stmts {
            res = stmt.eval(&mut ctxt)?;
        }
        Ok(res)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    This,
    Identifier(Identifier),
    Literal(Literal),
    InterpolatedStr(InterpolatedStr),
    ListBuilder(ListBuilder),
    DictBuilder(DictBuilder),
    PropertyOf(PropertyOf),
    IndexOf(IndexOf),
    UnaryOp(UnaryOp),
    BinaryOp(BinaryOp),
    ChainCatch(ChainCatch),
    Block(Block),
    IfElseStmt(IfElseStmt),
    ForStmt(ForStmt),
    Declaration(Declaration),
    FunctionDef(Rc<FunctionDef>),
    FunctionCall(FunctionCall),
    ReturnStmt(ReturnStmt),
    Assignment(Assignment),
    KeyValue(KeyValue),
    Break,
    Continue,
    End,
}

trait Eval {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop>;
}

#[derive(Debug)]
pub enum EvalStop {
    Error(InternalProgramError),
    Return(Value),
    Break,
    Continue,
    End,
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

impl AstNode {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        match self {
            AstNode::Declaration(v) => v.eval(ctxt),
            AstNode::Literal(v) => v.eval(ctxt),
            AstNode::FunctionCall(v) => v.eval(ctxt),
            AstNode::Identifier(v) => v.eval(ctxt),
            AstNode::InterpolatedStr(v) => v.eval(ctxt),
            AstNode::BinaryOp(v) => v.eval(ctxt),
            AstNode::UnaryOp(v) => v.eval(ctxt),
            AstNode::DictBuilder(v) => v.eval(ctxt),
            AstNode::PropertyOf(v) => v.eval(ctxt),
            AstNode::ListBuilder(v) => v.eval(ctxt),
            AstNode::IndexOf(v) => v.eval(ctxt),
            AstNode::Block(v) => v.eval(ctxt),
            AstNode::FunctionDef(v) => v.eval(ctxt),
            AstNode::Assignment(v) => v.eval(ctxt),
            AstNode::IfElseStmt(v) => v.eval(ctxt),
            AstNode::ForStmt(v) => v.eval(ctxt),
            AstNode::Break => Break::eval(ctxt),
            AstNode::Continue => Continue::eval(ctxt),
            AstNode::ReturnStmt(v) => v.eval(ctxt),
            // FIXME: finish eval
            // AstNode::This => todo!(),
            // AstNode::ChainCatch(chain_catch) => todo!(),
            // AstNode::KeyValue(key_value) => todo!(),
            // AstNode::End => todo!(),
            n => todo!("eval not implemented for {n:?}"),
        }
    }

    pub fn as_assignable(&'_ self) -> Option<Assignable<'_>> {
        match self {
            AstNode::Identifier(v) => Some(Assignable::Identifier(v)),
            AstNode::PropertyOf(v) => Some(Assignable::PropertyOf(v)),
            AstNode::IndexOf(v) => Some(Assignable::IndexOf(v)),
            _ => None,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            AstNode::Identifier(v) => v.span,
            AstNode::Literal(v) => v.span,
            AstNode::BinaryOp(v) => v.span(),
            AstNode::UnaryOp(v) => v.span(),
            // FIXME: finish pos
            // AstNode::Declaration(v) => v.eval(ctxt),
            // AstNode::FunctionCall(v) => v.eval(ctxt),
            // AstNode::InterpolatedStr(v) => v.eval(ctxt),
            // AstNode::ListBuilder(list_builder) => todo!(),
            // AstNode::DictBuilder(dict_builder) => todo!(),
            // AstNode::PropertyOf(property_of) => todo!(),
            // AstNode::IndexOf(index_of) => todo!(),
            // AstNode::ChainCatch(chain_catch) => todo!(),
            // AstNode::Block(block) => todo!(),
            // AstNode::IfElseStmt(if_else_stmt) => todo!(),
            // AstNode::ForStmt(for_stmt) => todo!(),
            // AstNode::FunctionDef(function_def) => todo!(),
            // AstNode::ReturnStmt(return_stmt) => todo!(),
            // AstNode::Assignment(assignment) => todo!(),
            // AstNode::KeyValue(key_value) => todo!(),
            n => todo!("span() not implemented for {n:?}"),
        }
    }
}

impl From<Identifier> for AstNode {
    fn from(value: Identifier) -> Self {
        Self::Identifier(value)
    }
}

impl From<Literal> for AstNode {
    fn from(value: Literal) -> Self {
        Self::Literal(value)
    }
}

impl From<Block> for AstNode {
    fn from(value: Block) -> Self {
        Self::Block(value)
    }
}

impl From<ListBuilder> for AstNode {
    fn from(value: ListBuilder) -> Self {
        Self::ListBuilder(value)
    }
}

impl From<DictBuilder> for AstNode {
    fn from(value: DictBuilder) -> Self {
        Self::DictBuilder(value)
    }
}

impl From<IfElseStmt> for AstNode {
    fn from(value: IfElseStmt) -> Self {
        Self::IfElseStmt(value)
    }
}
impl From<ForStmt> for AstNode {
    fn from(value: ForStmt) -> Self {
        Self::ForStmt(value)
    }
}
impl From<Declaration> for AstNode {
    fn from(value: Declaration) -> Self {
        Self::Declaration(value)
    }
}
impl From<Assignment> for AstNode {
    fn from(value: Assignment) -> Self {
        Self::Assignment(value)
    }
}
impl From<FunctionDef> for AstNode {
    fn from(value: FunctionDef) -> Self {
        Self::FunctionDef(Rc::new(value))
    }
}
impl From<FunctionCall> for AstNode {
    fn from(value: FunctionCall) -> Self {
        Self::FunctionCall(value)
    }
}
impl From<ReturnStmt> for AstNode {
    fn from(value: ReturnStmt) -> Self {
        Self::ReturnStmt(value)
    }
}
impl From<BinaryOp> for AstNode {
    fn from(value: BinaryOp) -> Self {
        Self::BinaryOp(value)
    }
}
impl From<UnaryOp> for AstNode {
    fn from(value: UnaryOp) -> Self {
        Self::UnaryOp(value)
    }
}
impl From<PropertyOf> for AstNode {
    fn from(value: PropertyOf) -> Self {
        Self::PropertyOf(value)
    }
}
impl From<IndexOf> for AstNode {
    fn from(value: IndexOf) -> Self {
        Self::IndexOf(value)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Assignable<'a> {
    Identifier(&'a Identifier),
    PropertyOf(&'a PropertyOf),
    IndexOf(&'a IndexOf),
}

impl<'a> Assignable<'a> {
    pub fn set(&self, ctxt: &mut Context, value: Value) -> Result<(), EvalStop> {
        match self {
            Assignable::Identifier(v) => v.set(ctxt, value),
            Assignable::PropertyOf(v) => v.set(ctxt, value),
            Assignable::IndexOf(v) => v.set(ctxt, value),
        }
    }
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
        let end = a.end().min(b.end());
        a.start = start;
        a.len = end - start;
        a
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Literal {
    pub(crate) val: Value,
    pub(crate) span: Span,
}

impl Literal {
    pub fn new(val: Value, span: Span) -> Self {
        Self { val, span }
    }
}
impl Eval for Literal {
    fn eval(&self, _: &mut Context) -> Result<Value, EvalStop> {
        Ok(self.val.clone())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier {
    pub(crate) name: String,
    pub(crate) span: Span,
}

impl Identifier {
    pub(crate) const fn new(name: String, span: Span) -> Self {
        Self { name, span }
    }
}
impl Eval for Identifier {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let value = ctxt
            .get(self)
            .or_else(|| BuiltInFunc::by_name(&self.name).map(|f| f.into()))
            .unwrap_or(Value::Nil);
        Ok(value)
    }
}
impl Setter for Identifier {
    fn set(&self, ctxt: &mut Context, value: Value) -> Result<(), EvalStop> {
        ctxt.set(self, value);
        Ok(())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ChainCatch {
    pub(crate) inner: Box<AstNode>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BuiltInFunc {
    Print,
    Add,
}

impl BuiltInFunc {
    pub fn by_name(key: &str) -> Option<BuiltInFunc> {
        match key {
            "print" => Some(BuiltInFunc::Print),
            "add" => Some(BuiltInFunc::Add),
            _ => None,
        }
    }

    pub fn call(
        &self,
        ctxt: &mut Context,
        params: Vec<Value>,
        span: &Span,
    ) -> Result<Value, EvalStop> {
        match self {
            BuiltInFunc::Print => {
                for p in params {
                    println!("got {}", p.as_string()?);
                }
                Ok(Value::Nil)
            }
            BuiltInFunc::Add => {
                let this = ctxt.get_this().unwrap_or(Value::Nil);
                match this {
                    Value::List(list) => {
                        let mut list = list.borrow_mut();
                        for v in params {
                            list.add(v);
                        }
                        Ok(Value::Nil)
                    }
                    _ => InternalProgramError::MethodNotApplicable {
                        name: "add".to_string(),
                        to: this.value_type(),
                        span: *span,
                    }
                    .into(),
                }
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionCall {
    pub(crate) on: Box<AstNode>,
    pub(crate) method: Option<Identifier>,
    pub(crate) args: Vec<Arg>,
    pub(crate) span: Span,
}

impl Eval for FunctionCall {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let on = self.on.eval(ctxt)?;

        // get the method
        let (this, func) = match &self.method {
            Some(method_name) => {
                let method = match &on {
                    Value::Dict(on) => {
                        on.borrow()
                            .get(&method_name.name)
                            .map(|v| match v {
                                Value::Func(f) => Ok(f),
                                other => InternalProgramError::ExpectedFunction {
                                    got: other.value_type(),
                                    span: Span::new(0, 0),
                                }
                                .into(),
                            })
                            .transpose()?
                            .or_else(|| BuiltInFunc::by_name(&method_name.name).map(|f| f.into()))
                            .ok_or_else(|| {
                                InternalProgramError::NoSuchMethod {
                                    name: method_name.clone(),
                                    from: ValueType::Dict,
                                    span: Span::new(0, 0), // FIXME: fix span
                                }
                                .into_stop()
                            })?
                    }
                    Value::List(_) => {
                        BuiltInFunc::by_name(&method_name.name)
                            .map(|f| f.into())
                            .ok_or_else(|| {
                                InternalProgramError::NoSuchMethod {
                                    name: method_name.clone(),
                                    from: ValueType::List,
                                    span: Span::new(0, 0), // FIXME: fix span
                                }
                                .into_stop()
                            })?
                    }
                    _ => {
                        return InternalProgramError::NoSuchMethod {
                            name: method_name.clone(),
                            from: on.value_type(),
                            span: Span::new(0, 0), // FIXME: fix span
                        }
                        .into();
                    }
                };
                (on, method)
            }
            None => {
                let func = match on {
                    Value::Func(func) => func,
                    _ => {
                        return InternalProgramError::ExpectedFunction {
                            got: on.value_type(),
                            span: Span::new(0, 0),
                        }
                        .into();
                    }
                };
                (Value::Nil, func)
            }
        };

        // evaluate the args
        let mut args = Vec::with_capacity(self.args.len());
        for arg_def in &self.args {
            args.push(arg_def.eval(ctxt)?);
        }

        // FIXME: normal function call should have a new context, but what about closures ?
        let mut context = Context::new();
        context.declare_this(this);

        match func.call(&mut context, args, &self.span) {
            v @ Ok(_) => v,
            e @ Err(EvalStop::Error(_)) => e,
            Err(EvalStop::Return(value)) => Ok(value),
            Err(EvalStop::Break) => todo!(),
            Err(EvalStop::Continue) => todo!(),
            Err(EvalStop::End) => todo!(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Arg {
    pub(crate) name: Option<Identifier>,
    pub(crate) value: AstNode,
}

impl Arg {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        if self.name.is_some() {
            todo!("named arg not handleld");
        }
        self.value.eval(ctxt)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ReturnStmt {
    pub(crate) value: Option<Box<AstNode>>,
}

impl Eval for ReturnStmt {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let val = match &self.value {
            Some(v) => v.eval(ctxt)?,
            None => Value::Nil,
        };

        Err(EvalStop::Return(val))
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct KeyValue {
    pub(crate) key: Identifier,
    pub(crate) value: Box<AstNode>,
}

#[cfg(test)]
mod tests {
    mod programs;
}
