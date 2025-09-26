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
mod program;
mod string;
mod unary_op;
mod value;
mod var;

pub use binary_op::{BinaryOp, BinaryOpCode};
pub use block::Block;
pub use context::Context;
pub use dict::DictBuilder;
pub(crate) use errors::bad_type_error_op;
pub use errors::{InternalProgramError, ProgramError};
pub use function::{FunctionDef, Param};
pub use if_else::{ConditionalBlock, IfElse};
pub use list::ListBuilder;
pub use loops::{Break, Continue, For};
pub use member::{IndexOf, PropertyOf};
pub use program::Program;
use std::rc::Rc;
pub use string::InterpolatedStr;
pub use unary_op::{UnaryOp, UnaryOpCode};
use value::ValueType;
pub use value::{KeyValue, Value};
pub use var::{Assignment, Declaration};

#[allow(unused_imports)]
pub(crate) use value::{Dict, List};

const THIS: &str = "this";

#[derive(PartialEq, Clone)]
pub enum AstNode {
    This(This),
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
    IfElse(IfElse),
    For(For),
    Declaration(Declaration),
    FunctionDef(Rc<FunctionDef>),
    FunctionCall(FunctionCall),
    ReturnStmt(ReturnStmt),
    Assignment(Assignment),
    KeyValue(KeyValueBuilder),
    Break(Break),
    Continue(Continue),
    End(End),
}

macro_rules! node_debug_1 {
    ($m:expr, $f:expr, $($name:ident),+) => {
        match $m {
            $(
              AstNode::$name(v) => v.fmt($f),
            )+
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
                  $f.write_str(" )")
              }
            )+
        }
    };
}

impl core::fmt::Debug for AstNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            node_debug_1!(
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
                IfElse,
                For,
                Declaration,
                FunctionDef,
                FunctionCall,
                ReturnStmt,
                Assignment,
                KeyValue,
                This,
                Break,
                Continue,
                End
            )
        } else {
            node_debug_2!(
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
                IfElse,
                For,
                Declaration,
                FunctionDef,
                FunctionCall,
                ReturnStmt,
                Assignment,
                KeyValue,
                This,
                Break,
                Continue,
                End
            )
        }
    }
}

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
            AstNode::IfElse(v) => v.eval(ctxt),
            AstNode::For(v) => v.eval(ctxt),
            AstNode::ReturnStmt(v) => v.eval(ctxt),
            AstNode::This(v) => v.eval(ctxt),
            AstNode::Break(v) => v.eval(ctxt),
            AstNode::Continue(v) => v.eval(ctxt),
            AstNode::End(v) => v.eval(ctxt),
            AstNode::KeyValue(v) => v.eval(ctxt),
            AstNode::ChainCatch(v) => v.eval(ctxt),
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

pub(crate) enum VisitorResult<T> {
    Stop(T),
    Continue,
}

pub(crate) trait Accept<T> {
    fn accept(&self, visitor: &mut impl Visitor<T>) -> VisitorResult<T>;
}

#[allow(unused_variables)]
pub(crate) trait Visitor<T>: Sized {
    fn go(&mut self, n: &AstNode) -> Option<T> {
        match self.visit_node(n) {
            VisitorResult::Stop(v) => Some(v),
            VisitorResult::Continue => None,
        }
    }

    fn visit_node(&mut self, n: &AstNode) -> VisitorResult<T> {
        match n {
            AstNode::This(v) => self.visit_this(v),
            AstNode::Identifier(v) => self.visit_identifier(v),
            AstNode::Literal(v) => self.visit_literal(v),
            AstNode::InterpolatedStr(v) => self.visit_interpolated_str(v),
            AstNode::ListBuilder(v) => self.visit_list_builder(v),
            AstNode::DictBuilder(v) => self.visit_dict_builder(v),
            AstNode::PropertyOf(v) => self.visit_property_of(v),
            AstNode::IndexOf(v) => self.visit_index_of(v),
            AstNode::UnaryOp(v) => self.visit_unary_op(v),
            AstNode::BinaryOp(v) => self.visit_binary_op(v),
            AstNode::ChainCatch(v) => self.visit_chain_catch(v),
            AstNode::Block(v) => self.visit_block(v),
            AstNode::IfElse(v) => self.visit_if_else(v),
            AstNode::For(v) => self.visit_for(v),
            AstNode::Declaration(v) => self.visit_declaration(v),
            AstNode::FunctionDef(v) => self.visit_function_def(v),
            AstNode::FunctionCall(v) => self.visit_function_call(v),
            AstNode::ReturnStmt(v) => self.visit_return_stmt(v),
            AstNode::Assignment(v) => self.visit_assignment(v),
            AstNode::KeyValue(v) => self.visit_key_value(v),
            AstNode::Break(v) => self.visit_break(v),
            AstNode::Continue(v) => self.visit_continue(v),
            AstNode::End(v) => self.visit_end(v),
        }
    }

    fn visit_this(&mut self, v: &This) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_identifier(&mut self, v: &Identifier) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_literal(&mut self, v: &Literal) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_interpolated_str(&mut self, v: &InterpolatedStr) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_list_builder(&mut self, v: &ListBuilder) -> VisitorResult<T> {
        v.accept(self)
    }
    fn visit_dict_builder(&mut self, v: &DictBuilder) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_property_of(&mut self, v: &PropertyOf) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_index_of(&mut self, v: &IndexOf) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_unary_op(&mut self, v: &UnaryOp) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_binary_op(&mut self, v: &BinaryOp) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_chain_catch(&mut self, v: &ChainCatch) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_block(&mut self, v: &Block) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_if_else(&mut self, v: &IfElse) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_for(&mut self, v: &For) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_declaration(&mut self, v: &Declaration) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_function_def(&mut self, v: &Rc<FunctionDef>) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_function_call(&mut self, v: &FunctionCall) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_return_stmt(&mut self, v: &ReturnStmt) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_assignment(&mut self, v: &Assignment) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_key_value(&mut self, v: &KeyValueBuilder) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_break(&mut self, v: &Break) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_continue(&mut self, v: &Continue) -> VisitorResult<T> {
        v.accept(self)
    }

    fn visit_end(&mut self, v: &End) -> VisitorResult<T> {
        v.accept(self)
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

impl From<IfElse> for AstNode {
    fn from(value: IfElse) -> Self {
        Self::IfElse(value)
    }
}
impl From<For> for AstNode {
    fn from(value: For) -> Self {
        Self::For(value)
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
impl From<Break> for AstNode {
    fn from(value: Break) -> Self {
        Self::Break(value)
    }
}
impl From<Continue> for AstNode {
    fn from(value: Continue) -> Self {
        Self::Continue(value)
    }
}
impl From<End> for AstNode {
    fn from(value: End) -> Self {
        Self::End(value)
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

impl<T> Accept<T> for Literal {
    fn accept(&self, _: &mut impl Visitor<T>) -> VisitorResult<T> {
        VisitorResult::Continue
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

impl Eval for Identifier {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let value = ctxt
            .get(self)
            .or_else(|| BuiltInFunc::by_name(&self.name).map(|f| f.into()))
            .unwrap_or(Value::Nil);
        Ok(value)
    }
}

impl<T> Accept<T> for Identifier {
    fn accept(&self, _: &mut impl Visitor<T>) -> VisitorResult<T> {
        VisitorResult::Continue
    }
}

impl Setter for Identifier {
    fn set(&self, ctxt: &mut Context, value: Value) -> Result<(), EvalStop> {
        ctxt.set(self, value);
        Ok(())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct This {}

impl This {
    pub(crate) fn new() -> Self {
        Self {}
    }
}

impl Eval for This {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        match ctxt.get_this() {
            Some(this) => Ok(this),
            None => Err(EvalStop::Error(InternalProgramError::ThisNotAvailable)),
        }
    }
}

impl<T> Accept<T> for This {
    fn accept(&self, _: &mut impl Visitor<T>) -> VisitorResult<T> {
        VisitorResult::Continue
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

impl Eval for ChainCatch {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        // FIXME: the chain catch ? should be replaced during assemble stage with a catch thing !!!
        match self.inner.eval(ctxt) {
            Err(EvalStop::Throw) => Ok(Value::Nil),
            other => other,
            // ok @ Ok(_) => ok,
            // e @ Err(_) => e,
        }
    }
}

impl<T> Accept<T> for ChainCatch {
    fn accept(&self, visitor: &mut impl Visitor<T>) -> VisitorResult<T> {
        if let v @ VisitorResult::Stop(_) = visitor.visit_node(&self.inner) {
            return v;
        }
        VisitorResult::Continue
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
            Err(EvalStop::Throw) => todo!(),
        }
    }
}

impl<T> Accept<T> for FunctionCall {
    fn accept(&self, visitor: &mut impl Visitor<T>) -> VisitorResult<T> {
        match visitor.visit_node(&self.on) {
            v @ VisitorResult::Stop(_) => return v,
            VisitorResult::Continue => {}
        }
        if let Some(method) = &self.method {
            match visitor.visit_identifier(method) {
                v @ VisitorResult::Stop(_) => return v,
                VisitorResult::Continue => {}
            }
        }
        for arg in &self.args {
            if let Some(name) = &arg.name {
                match visitor.visit_identifier(name) {
                    v @ VisitorResult::Stop(_) => return v,
                    VisitorResult::Continue => {}
                }
            }
            match visitor.visit_node(&arg.value) {
                v @ VisitorResult::Stop(_) => return v,
                VisitorResult::Continue => {}
            }
        }
        VisitorResult::Continue
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

impl<T> Accept<T> for ReturnStmt {
    fn accept(&self, visitor: &mut impl Visitor<T>) -> VisitorResult<T> {
        if let Some(value) = &self.value {
            match visitor.visit_node(value) {
                v @ VisitorResult::Stop(_) => return v,
                VisitorResult::Continue => {}
            }
        }
        VisitorResult::Continue
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

impl<T> Accept<T> for KeyValueBuilder {
    fn accept(&self, visitor: &mut impl Visitor<T>) -> VisitorResult<T> {
        if let v @ VisitorResult::Stop(_) = visitor.visit_identifier(&self.key) {
            return v;
        }
        if let v @ VisitorResult::Stop(_) = visitor.visit_node(&self.value) {
            return v;
        }
        VisitorResult::Continue
    }
}

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

impl<T> Accept<T> for End {
    fn accept(&self, _: &mut impl Visitor<T>) -> VisitorResult<T> {
        VisitorResult::Continue
    }
}

#[cfg(test)]
mod tests {}
