mod value;

use std::collections::HashMap;
pub use value::Value;

#[derive(Debug, Clone)]
pub struct Context {
    pub vars: HashMap<Identifier, Value>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    pub fn set(&mut self, ident: &Identifier, value: Value) {
        self.vars.insert(ident.clone(), value);
    }
    pub fn get(&mut self, ident: &Identifier) -> Value {
        self.vars.get(ident).cloned().unwrap_or(Value::Nil)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Program {
    pub stmts: Vec<AstNode>,
}

impl Program {
    pub(crate) fn run(&self) -> Result<(), ProgramError> {
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
    pub(crate) pos: usize,
}

#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    This,
    Identifier(Identifier),
    Value(Value),
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
    FunctionDef(FunctionDef),
    FunctionCall(FunctionCall),
    ReturnStmt(ReturnStmt),
    Assignment(Assignment),
    KeyValue(KeyValue),
    Break,
    Continue,
    End,
}

impl AstNode {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, ProgramError> {
        match self {
            AstNode::Declaration(v) => v.eval(ctxt),
            AstNode::Value(v) => v.eval(ctxt),
            AstNode::FunctionCall(v) => v.eval(ctxt),
            AstNode::Identifier(v) => v.eval(ctxt),
            AstNode::InterpolatedStr(v) => v.eval(ctxt),
            // AstNode::ListBuilder(list_builder) => todo!(),
            // AstNode::DictBuilder(dict_builder) => todo!(),
            // AstNode::PropertyOf(property_of) => todo!(),
            // AstNode::IndexOf(index_of) => todo!(),
            // AstNode::UnaryOp(unary_op) => todo!(),
            // AstNode::BinaryOp(binary_op) => todo!(),
            // AstNode::ChainCatch(chain_catch) => todo!(),
            // AstNode::Block(block) => todo!(),
            // AstNode::IfElseStmt(if_else_stmt) => todo!(),
            // AstNode::ForStmt(for_stmt) => todo!(),
            // AstNode::FunctionDef(function_def) => todo!(),
            // AstNode::ReturnStmt(return_stmt) => todo!(),
            // AstNode::Assignment(assignment) => todo!(),
            // AstNode::KeyValue(key_value) => todo!(),
            n => todo!("eval not implemented for {n:?}"),
        }
    }

    pub fn pos(&self) -> usize {
        match self {
            AstNode::Identifier(v) => v.pos,
            // AstNode::Declaration(v) => v.eval(ctxt),
            // AstNode::Value(v) => v.eval(ctxt),
            // AstNode::FunctionCall(v) => v.eval(ctxt),
            // AstNode::InterpolatedStr(v) => v.eval(ctxt),
            // AstNode::ListBuilder(list_builder) => todo!(),
            // AstNode::DictBuilder(dict_builder) => todo!(),
            // AstNode::PropertyOf(property_of) => todo!(),
            // AstNode::IndexOf(index_of) => todo!(),
            // AstNode::UnaryOp(unary_op) => todo!(),
            // AstNode::BinaryOp(binary_op) => todo!(),
            // AstNode::ChainCatch(chain_catch) => todo!(),
            // AstNode::Block(block) => todo!(),
            // AstNode::IfElseStmt(if_else_stmt) => todo!(),
            // AstNode::ForStmt(for_stmt) => todo!(),
            // AstNode::FunctionDef(function_def) => todo!(),
            // AstNode::ReturnStmt(return_stmt) => todo!(),
            // AstNode::Assignment(assignment) => todo!(),
            // AstNode::KeyValue(key_value) => todo!(),
            n => todo!("pos() not implemented for {n:?}"),
        }
    }
}

impl From<Identifier> for AstNode {
    fn from(value: Identifier) -> Self {
        Self::Identifier(value)
    }
}

impl From<Value> for AstNode {
    fn from(value: Value) -> Self {
        Self::Value(value)
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
        Self::FunctionDef(value)
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Identifier {
    pub(crate) name: String,
    pub(crate) pos: usize,
}

impl Identifier {
    pub(crate) fn new(name: String, pos: usize) -> Self {
        Self { name, pos }
    }
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, ProgramError> {
        if self.name == "print" {
            Ok(Value::BuiltInFunc(BuiltInFunc::Print))
        } else {
            Ok(ctxt.get(self))
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnaryOp {
    pub(crate) op: UnaryOpCode,
    pub(crate) on: Box<AstNode>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct BinaryOp {
    pub(crate) op: BinaryOpCode,
    pub(crate) lhs: Box<AstNode>,
    pub(crate) rhs: Box<AstNode>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ChainCatch {
    pub(crate) inner: Box<AstNode>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct PropertyOf {
    pub(crate) from: Box<AstNode>,
    pub(crate) property: Identifier,
}

#[derive(PartialEq, Debug, Clone)]
pub struct IndexOf {
    pub(crate) from: Box<AstNode>,
    pub(crate) index: Box<AstNode>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct InterpolatedStr {
    pub(crate) parts: Vec<AstNode>,
}

impl InterpolatedStr {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, ProgramError> {
        if self.parts.len() == 1 {
            self.parts.first().expect("should be there").eval(ctxt)
        } else {
            let mut res = String::new();
            for p in &self.parts {
                let val = p.eval(ctxt)?;
                let s = val.as_string().map_err(|e| ProgramError {
                    msg: format!("Error interpolating string: {}", e.msg),
                    pos: p.pos(),
                })?;
                res.push_str(&format!("{s}"));
            }
            Ok(Value::Str(res))
        }
    }
}
#[derive(PartialEq, Debug, Clone)]
pub struct ListBuilder {
    pub(crate) entries: Vec<AstNode>,
}

impl ListBuilder {
    pub(crate) fn new(entries: Vec<AstNode>) -> Self {
        Self { entries }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct DictBuilder {
    pub(crate) entries: Vec<KeyValue>,
}

impl DictBuilder {
    pub(crate) fn new(entries: Vec<KeyValue>) -> Self {
        Self { entries }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOpCode {
    Not,
    Negative,
}

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
pub struct Block {
    pub(crate) stmts: Vec<AstNode>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionDef {
    pub(crate) name: Option<Identifier>,
    pub(crate) params: Vec<Param>,
    pub(crate) body: Block,
}

impl FunctionDef {
    pub fn call(&self, ctxt: &mut Context, params: Vec<Value>) -> Result<Value, ProgramError> {
        let _ = params;
        let _ = ctxt;
        let _ = Value::Func(self.clone());
        todo!()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum BuiltInFunc {
    Print,
}

impl BuiltInFunc {
    pub fn call(&self, _ctxt: &mut Context, params: Vec<Value>) -> Result<Value, ProgramError> {
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
pub struct Param {
    pub(crate) name: Identifier,
    pub(crate) value: Option<AstNode>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionCall {
    pub(crate) on: Box<AstNode>,
    pub(crate) args: Vec<Arg>,
}

impl FunctionCall {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, ProgramError> {
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
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, ProgramError> {
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
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, ProgramError> {
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
