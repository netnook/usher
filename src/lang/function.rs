use crate::lang::{AstNode, Block, Context, Eval, Identifier, InternalProgramError, Value};
use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
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
    fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        if let Some(name) = &self.name {
            ctxt.set(name, Value::Func(Rc::clone(self)));
        };
        Ok(Value::Func(Rc::clone(self)))
    }
}

impl FunctionDef {
    pub fn call(
        &self,
        ctxt: &mut Context,
        params: Vec<Value>,
    ) -> Result<Value, InternalProgramError> {
        let _ = params;
        let _ = ctxt;
        todo!()
    }
}

#[cfg(test)]
mod tests {}
