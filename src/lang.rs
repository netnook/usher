mod binary_op;
mod block;
mod dict;
mod function;
mod list;
mod unary_op;
mod value;

pub use binary_op::{BinaryOp, BinaryOpCode};
pub use block::Block;
pub use dict::{DictBuilder, PropertyOf};
pub use function::{FunctionDef, Param};
pub use list::{IndexOf, ListBuilder};
use std::{collections::HashMap, rc::Rc};
pub use unary_op::{UnaryOp, UnaryOpCode};
pub use value::Value;

use crate::find_source_position;

#[derive(Debug, Clone)]
pub struct Context {
    pub vars: HashMap<String, Value>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    pub fn set(&mut self, ident: &Identifier, value: Value) {
        self.vars.insert(ident.name.clone(), value);
    }
    pub fn get(&mut self, ident: &Identifier) -> Value {
        self.vars.get(&ident.name).cloned().unwrap_or(Value::Nil)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Program<'a> {
    pub source: &'a str,
    pub stmts: Vec<AstNode>,
}

impl<'a> Program<'a> {
    pub(crate) fn run(&self) -> Result<(), ProgramError> {
        match self.do_run() {
            Ok(v) => Ok(v),
            Err(e) => {
                let info = find_source_position(self.source, e.span.start);
                Err(ProgramError {
                    msg: e.msg,
                    line_no: info.0.line,
                    char_no: info.0.char,
                    line: info.1.to_string(),
                })
            }
        }
    }
    fn do_run(&self) -> Result<(), InternalProgramError> {
        let mut ctxt = Context::new();
        for stmt in &self.stmts {
            stmt.eval(&mut ctxt)?;
        }
        Ok(())
    }
}

#[derive(PartialEq, Debug)]
pub struct ProgramError {
    pub(crate) msg: String,
    pub(crate) line_no: usize,
    pub(crate) char_no: usize,
    pub(crate) line: String,
}

#[derive(PartialEq, Debug)]
pub struct InternalProgramError {
    pub(crate) msg: String,
    pub(crate) span: Span,
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
    fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError>;
}

impl AstNode {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
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
            // FIXME: finish eval
            // AstNode::ChainCatch(chain_catch) => todo!(),
            // AstNode::IfElseStmt(if_else_stmt) => todo!(),
            // AstNode::ForStmt(for_stmt) => todo!(),
            // AstNode::ReturnStmt(return_stmt) => todo!(),
            // AstNode::Assignment(assignment) => todo!(),
            // AstNode::KeyValue(key_value) => todo!(),
            n => todo!("eval not implemented for {n:?}"),
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
    pub fn eval(&self, _: &mut Context) -> Result<Value, InternalProgramError> {
        Ok(self.val.clone())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier {
    pub(crate) name: String,
    pub(crate) span: Span,
}

impl Identifier {
    pub(crate) fn new(name: String, span: Span) -> Self {
        Self { name, span }
    }
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        if self.name == "print" {
            Ok(Value::BuiltInFunc(BuiltInFunc::Print))
        } else {
            Ok(ctxt.get(self))
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ChainCatch {
    pub(crate) inner: Box<AstNode>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct InterpolatedStr {
    pub(crate) parts: Vec<AstNode>,
}

impl InterpolatedStr {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        if self.parts.len() == 1 {
            self.parts.first().expect("should be there").eval(ctxt)
        } else {
            let mut res = String::new();
            for p in &self.parts {
                let val = p.eval(ctxt)?;
                let s = val.as_string().map_err(|e| InternalProgramError {
                    msg: format!("Error interpolating string: {}", e.msg),
                    span: p.span(),
                })?;
                res.push_str(&format!("{s}"));
            }
            Ok(Value::Str(res))
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct IfElseStmt {
    pub(crate) conditional_blocks: Vec<ConditionalBlock>,
    pub(crate) else_block: Option<Block>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ConditionalBlock {
    pub(crate) condition: AstNode,
    pub(crate) block: Block,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ForStmt {
    pub(crate) loop_var_1: Identifier,
    pub(crate) loop_var_2: Option<Identifier>,
    pub(crate) loop_expr: Box<AstNode>,
    pub(crate) block: Block,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BuiltInFunc {
    Print,
}

impl BuiltInFunc {
    pub fn call(
        &self,
        _ctxt: &mut Context,
        params: Vec<Value>,
    ) -> Result<Value, InternalProgramError> {
        match self {
            BuiltInFunc::Print => {
                for p in params {
                    println!("got {}", p.as_string()?);
                }
                Ok(Value::Nil)
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionCall {
    pub(crate) on: Box<AstNode>,
    pub(crate) args: Vec<Arg>,
}

impl FunctionCall {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        // get the function
        let maybe_func = self.on.eval(ctxt)?;

        // evaluate the args
        let mut args = Vec::with_capacity(self.args.len());
        for arg_def in &self.args {
            args.push(arg_def.eval(ctxt)?);
        }

        // call the function
        // func.call(ctxt, args)
        match maybe_func {
            Value::Func(func) => func.call(ctxt, args),
            Value::BuiltInFunc(func) => func.call(ctxt, args),
            _ => {
                todo!()
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Arg {
    pub(crate) name: Option<Identifier>,
    pub(crate) value: AstNode,
}

impl Arg {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
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

#[derive(PartialEq, Debug, Clone)]
pub struct Declaration {
    pub(crate) ident: Identifier,
    pub(crate) value: Box<AstNode>,
}

impl Declaration {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        let value = self.value.eval(ctxt)?;
        ctxt.set(&self.ident, value);
        Ok(Value::Nil)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Assignment {
    pub(crate) lhs: Box<AstNode>,
    pub(crate) rhs: Box<AstNode>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct KeyValue {
    pub(crate) key: Identifier,
    pub(crate) value: Box<AstNode>,
}

#[cfg(test)]
mod tests {}
