use crate::lang::{AstNode, Block, Context, Eval, EvalStop, Identifier, Span, Value, value::Func};
use std::rc::Rc;

#[derive(PartialEq)]
pub struct FunctionDef {
    pub(crate) name: Option<Identifier>,
    pub(crate) params: Vec<Param>,
    pub(crate) body: Block,
}

impl core::fmt::Debug for FunctionDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            let mut w = f.debug_struct("FunctionDef");
            if let Some(name) = &self.name {
                w.field("name", &name.name);
            }
            if !self.params.is_empty() {
                w.field("params", &self.params);
            }
            w.field("body", &self.body);
            w.finish()
        } else {
            f.debug_struct("FunctionDef")
                .field("name", &self.name)
                .field("params", &self.params)
                .field("body", &self.body)
                .finish()
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct Param {
    pub(crate) name: Identifier,
    pub(crate) default_value: Option<AstNode>,
}

impl core::fmt::Debug for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            if let Some(default_value) = &self.default_value {
                self.name.name.fmt(f)?;
                f.write_str(": ")?;
                default_value.fmt(f)
            } else {
                self.name.name.fmt(f)
            }
        } else {
            f.debug_struct("Param")
                .field("name", &self.name)
                .field("default_value", &self.default_value)
                .finish()
        }
    }
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
