mod binary_op;
mod block;
mod context;
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
    lang::{
        loops::{Break, Continue},
        value::ValueType,
    },
    parser::error::find_source_position,
};
pub use binary_op::{BinaryOp, BinaryOpCode};
pub use block::Block;
pub use context::Context;
pub use dict::DictBuilder;
pub(crate) use errors::bad_type_error_op;
pub use errors::{InternalProgramError, ProgramError};
pub use function::{FunctionDef, Param};
pub use if_else::{ConditionalBlock, IfElseStmt};
pub use list::ListBuilder;
pub use loops::ForStmt;
pub use member::{IndexOf, PropertyOf};
use std::rc::Rc;
pub use string::InterpolatedStr;
pub use unary_op::{UnaryOp, UnaryOpCode};
pub use value::Value;
pub use var::{Assignment, Declaration};

const THIS: &str = "this";

#[derive(PartialEq, Clone)]
pub struct Program<'a> {
    pub source: &'a str,
    pub stmts: Vec<AstNode>,
}

impl<'a> core::fmt::Debug for Program<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            f.write_str("Program { ")?;
            f.debug_list().entries(&self.stmts).finish()?;
            f.write_str(" }")
        } else {
            f.debug_struct("Program")
                .field("stmts", &self.stmts)
                .field("source", &self.source)
                .finish()
        }
    }
}

impl<'a> Program<'a> {
    pub fn run(&self) -> Result<Value, ProgramError> {
        match self.do_run() {
            Ok(v) => Ok(v),
            Err(EvalStop::Return(v)) => Ok(v),
            Err(EvalStop::Error(e)) => {
                let info = find_source_position(self.source, e.span().start);
                Err(ProgramError {
                    msg: format!("{e}"),
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

#[derive(PartialEq, Clone)]
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

macro_rules! node_debug_1 {
    ($m:expr, $f:expr, $($name:ident),+) => {
        match $m {
            $(
              AstNode::$name(v) => {v.fmt($f)?; true}
            )+
            _ => false
        }
    };
}

macro_rules! node_debug_2 {
    ($m:expr, $f:expr, $($name:ident),+) => {
        match $m {
            $(
              AstNode::$name(v) => {
                  $f.write_str(concat!(stringify!($name), "("))?;
                  v.fmt($f)?;
                  $f.write_str(" )")?;
                  true
              }
            )+
            _ => false
        }
    };
}

macro_rules! node_debug_3 {
    ($m:expr, $f:expr, $($name:ident),+) => {
        match $m {
            $(
              AstNode::$name => { write!($f, stringify!($name))?; true }
            )+
            _ => false
        }
    };
}

impl core::fmt::Debug for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            if node_debug_1!(
                self,
                f,
                Literal,
                Identifier,
                InterpolatedStr,
                ListBuilder,
                DictBuilder,
                PropertyOf,
                IndexOf,
                UnaryOp,
                BinaryOp,
                ChainCatch,
                Block,
                IfElseStmt,
                ForStmt,
                Declaration,
                FunctionDef,
                FunctionCall,
                ReturnStmt,
                Assignment,
                KeyValue
            ) {
                return Ok(());
            }
            if node_debug_3!(self, f, This, Break, Continue, End) {
                return Ok(());
            }
            panic!()
        } else {
            if node_debug_2!(
                self,
                f,
                Literal,
                Identifier,
                InterpolatedStr,
                ListBuilder,
                DictBuilder,
                PropertyOf,
                IndexOf,
                UnaryOp,
                BinaryOp,
                ChainCatch,
                Block,
                IfElseStmt,
                ForStmt,
                Declaration,
                FunctionDef,
                FunctionCall,
                ReturnStmt,
                Assignment,
                KeyValue
            ) {
                return Ok(());
            }
            if node_debug_3!(self, f, This, Break, Continue, End) {
                return Ok(());
            }
            panic!()
        }
    }
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

#[derive(PartialEq, Clone)]
pub struct Literal {
    pub(crate) val: Value,
    pub(crate) span: Span,
}

impl core::fmt::Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            write!(f, "Literal({:-#?})", self.val)
        } else {
            f.debug_struct("Literal")
                .field("val", &self.val)
                .field("span", &self.span)
                .finish()
        }
    }
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

#[derive(PartialEq, Eq, Clone)]
pub struct Identifier {
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

#[derive(PartialEq, Clone)]
pub struct ChainCatch {
    pub(crate) inner: Box<AstNode>,
}

impl core::fmt::Debug for ChainCatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            f.write_str("ChainCatch { ")?;
            self.inner.fmt(f)?;
            f.write_str(" }")
        } else {
            f.debug_struct("ChainCatch")
                .field("inner", &self.inner)
                .finish()
        }
    }
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

#[derive(PartialEq, Clone)]
pub struct FunctionCall {
    pub(crate) on: Box<AstNode>,
    pub(crate) method: Option<Identifier>,
    pub(crate) args: Vec<Arg>,
    pub(crate) span: Span,
}

impl core::fmt::Debug for FunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            let mut w = f.debug_struct("FunctionCall");
            w.field("on", &self.on);
            if let Some(method) = &self.method {
                w.field("method", &method.name);
            }
            if !self.args.is_empty() {
                w.field("args", &self.args);
            }
            w.finish()
        } else {
            f.debug_struct("FunctionCall")
                .field("on", &self.on)
                .field("method", &self.method)
                .field("args", &self.args)
                .field("span", &self.span)
                .finish()
        }
    }
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
                                    name: method_name.name.clone(),
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
                                    name: method_name.name.clone(),
                                    from: ValueType::List,
                                    span: Span::new(0, 0), // FIXME: fix span
                                }
                                .into_stop()
                            })?
                    }
                    _ => {
                        return InternalProgramError::NoSuchMethod {
                            name: method_name.name.clone(),
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

#[derive(PartialEq, Clone)]
pub struct Arg {
    pub(crate) name: Option<Identifier>,
    pub(crate) value: AstNode,
}

impl core::fmt::Debug for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            if let Some(name) = &self.name {
                name.name.fmt(f)?;
                f.write_str(": ")?;
                self.value.fmt(f)
            } else {
                self.value.fmt(f)
            }
        } else {
            f.debug_struct("Arg")
                .field("name", &self.name)
                .field("value", &self.value)
                .finish()
        }
    }
}
impl Arg {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        if self.name.is_some() {
            todo!("named arg not handleld");
        }
        self.value.eval(ctxt)
    }
}

#[derive(PartialEq, Clone)]
pub struct ReturnStmt {
    pub(crate) value: Option<Box<AstNode>>,
}

impl core::fmt::Debug for ReturnStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            match &self.value {
                Some(v) => {
                    f.write_str("Return { ")?;
                    v.fmt(f)?;
                    f.write_str(" }")
                }
                None => write!(f, "Return"),
            }
        } else {
            f.debug_struct("ReturnStmt")
                .field("value", &self.value)
                .finish()
        }
    }
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
mod tests {}
