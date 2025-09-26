use crate::lang::{
    Accept, AstNode, Context, Eval, EvalStop, Identifier, InternalProgramError, Value, Visitor,
    VisitorResult, accept_default,
};

#[derive(PartialEq, Clone)]
pub struct Declaration {
    pub(crate) ident: Identifier,
    pub(crate) value: Box<AstNode>,
}

impl core::fmt::Debug for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            f.debug_struct("Declaration")
                .field("ident", &self.ident.name)
                .field("value", &self.value)
                .finish()
        } else {
            f.debug_struct("Declaration")
                .field("ident", &self.ident)
                .field("value", &self.value)
                .finish()
        }
    }
}

impl Eval for Declaration {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let value = self.value.eval(ctxt)?;
        ctxt.declare(&self.ident, value);
        Ok(Value::Nil)
    }
}

accept_default!(Declaration, ident:identifier, value:node,);

#[derive(PartialEq, Debug, Clone)]
pub struct Assignment {
    pub(crate) lhs: Box<AstNode>,
    pub(crate) rhs: Box<AstNode>,
}

impl Eval for Assignment {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let rhs = self.rhs.eval(ctxt)?;
        let Some(assignable) = self.lhs.as_assignable() else {
            return InternalProgramError::CannotAssignToLHS {
                span: self.lhs.span(),
            }
            .into();
        };

        assignable.set(ctxt, rhs)?;

        Ok(Value::Nil)
    }
}

accept_default!(Assignment, lhs:node, rhs:node,);

#[cfg(test)]
mod tests {}
