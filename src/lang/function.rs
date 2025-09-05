use crate::lang::{AstNode, Block, Context, Eval, EvalStop, Identifier, Span, Value, value::Func};
use std::rc::Rc;

#[derive(PartialEq, Debug)]
pub struct FunctionDef {
    pub(crate) name: Option<Identifier>,
    pub(crate) params: Vec<Param>,
    pub(crate) body: Block,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Param {
    pub(crate) name: Identifier,
    pub(crate) value: Option<AstNode>,
}

impl Eval for Rc<FunctionDef> {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let f = Value::Func(Func::Func(Rc::clone(self)));
        if let Some(name) = &self.name {
            ctxt.set(name, f.clone());
        };
        Ok(f)
    }
}

impl FunctionDef {
    pub fn call(
        &self,
        ctxt: &mut Context,
        params: Vec<Value>,
        span: &Span,
    ) -> Result<Value, EvalStop> {
        let _ = ctxt;
        let _ = params;
        let _ = span;
        todo!()
    }
}

#[cfg(test)]
mod tests {}
