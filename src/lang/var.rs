use crate::lang::{AstNode, Context, Eval, Identifier, InternalProgramError, Value};

#[derive(PartialEq, Debug, Clone)]
pub struct Declaration {
    pub(crate) ident: Identifier,
    pub(crate) value: Box<AstNode>,
}

impl Eval for Declaration {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
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

impl Eval for Assignment {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        let rhs = self.rhs.eval(ctxt)?;
        let Some(assignable) = self.lhs.as_assignable() else {
            return Err(InternalProgramError {
                msg: "LHS is not assignable.".to_string(),
                span: self.lhs.span(),
            });
        };

        assignable.set(ctxt, rhs)?;

        Ok(Value::Nil)
    }
}

#[cfg(test)]
mod tests {}
