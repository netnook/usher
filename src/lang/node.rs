use crate::lang::{
    Assignment, BinaryOp, Block, Break, ChainCatch, Context, Continue, Declaration, DictBuilder,
    End, Eval, EvalStop, For, FunctionCall, FunctionDef, IfElse, IndexOf, InterpolatedStr,
    KeyValueBuilder, ListBuilder, Literal, PropertyOf, ReturnStmt, Setter, Span, This, UnaryOp,
    Value, Var,
};
use std::rc::Rc;

#[derive(PartialEq, Clone)]
pub enum AstNode {
    This(This),
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
    Var(Var),
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
                Var,
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
                Var,
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

impl AstNode {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        match self {
            AstNode::Declaration(v) => v.eval(ctxt),
            AstNode::Literal(v) => v.eval(ctxt),
            AstNode::FunctionCall(v) => v.eval(ctxt),
            AstNode::InterpolatedStr(v) => v.eval(ctxt),
            AstNode::BinaryOp(v) => v.eval(ctxt),
            AstNode::UnaryOp(v) => v.eval(ctxt),
            AstNode::DictBuilder(v) => v.eval(ctxt),
            AstNode::PropertyOf(v) => v.eval(ctxt),
            AstNode::ListBuilder(v) => v.eval(ctxt),
            AstNode::IndexOf(v) => v.eval(ctxt),
            AstNode::Block(v) => v.eval(ctxt),
            AstNode::FunctionDef(v) => v.eval(ctxt),
            AstNode::Var(v) => v.eval(ctxt),
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

    pub(crate) fn as_assignable(&'_ self) -> Option<Assignable<'_>> {
        match self {
            AstNode::Var(v) => Some(Assignable::Var(v)),
            AstNode::PropertyOf(v) => Some(Assignable::PropertyOf(v)),
            AstNode::IndexOf(v) => Some(Assignable::IndexOf(v)),
            _ => None,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            AstNode::Var(v) => v.span(),
            AstNode::Literal(v) => v.span,
            AstNode::BinaryOp(v) => v.span(),
            AstNode::UnaryOp(v) => v.span(),
            AstNode::Break(v) => v.span,
            AstNode::Continue(v) => v.span,
            AstNode::End(v) => v.span,
            AstNode::This(_) => todo!(),
            AstNode::InterpolatedStr(_) => todo!(),
            AstNode::ListBuilder(_) => todo!(),
            AstNode::DictBuilder(_) => todo!(),
            AstNode::PropertyOf(_) => todo!(),
            AstNode::IndexOf(_) => todo!(),
            AstNode::ChainCatch(_) => todo!(),
            AstNode::Block(_) => todo!(),
            AstNode::IfElse(_) => todo!(),
            AstNode::For(_) => todo!(),
            AstNode::Declaration(_) => todo!(),
            AstNode::FunctionDef(_) => todo!(),
            AstNode::FunctionCall(_) => todo!(),
            AstNode::ReturnStmt(_) => todo!(),
            AstNode::Assignment(_) => todo!(),
            AstNode::KeyValue(_) => todo!(),
        }
    }
}

impl From<Var> for AstNode {
    fn from(value: Var) -> Self {
        Self::Var(value)
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
pub(crate) enum Assignable<'a> {
    Var(&'a Var),
    PropertyOf(&'a PropertyOf),
    IndexOf(&'a IndexOf),
}

impl<'a> Assignable<'a> {
    pub fn set(&self, ctxt: &mut Context, value: Value) -> Result<(), EvalStop> {
        match self {
            Assignable::Var(v) => v.set(ctxt, value),
            Assignable::PropertyOf(v) => v.set(ctxt, value),
            Assignable::IndexOf(v) => v.set(ctxt, value),
        }
    }
}
