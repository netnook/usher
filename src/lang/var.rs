use crate::lang::{
    Accept, AstNode, BuiltInFunc, Context, Eval, EvalStop, Identifier, InternalProgramError,
    Setter, Span, Value, Visitor, VisitorResult, accept_default,
};

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

accept_default!(This);

#[derive(PartialEq, Clone)]
pub struct Var {
    pub(crate) ident: Identifier,
}

impl core::fmt::Debug for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            write!(f, r#"Var("{}")"#, self.ident.name)
        } else {
            f.debug_struct("Var").field("ident", &self.ident).finish()
        }
    }
}

impl Var {
    pub(crate) const fn new(name: Identifier) -> Self {
        Self { ident: name }
    }

    pub(crate) fn span(&self) -> Span {
        self.ident.span
    }
}

impl Eval for Var {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let value = ctxt
            .get(&self.ident)
            .or_else(|| BuiltInFunc::by_name(&self.ident.name).map(|f| f.into()))
            .unwrap_or(Value::Nil);
        Ok(value)
    }
}

accept_default!(Var);

impl Setter for Var {
    fn set(&self, ctxt: &mut Context, value: Value) -> Result<(), EvalStop> {
        ctxt.set(&self.ident, value);
        Ok(())
    }
}

#[derive(PartialEq, Clone)]
pub struct Declaration {
    pub(crate) var: Var,
    pub(crate) value: Box<AstNode>,
}

impl core::fmt::Debug for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            f.debug_struct("Declaration")
                .field("var", &self.var.ident.name)
                .field("value", &self.value)
                .finish()
        } else {
            f.debug_struct("Declaration")
                .field("var", &self.var)
                .field("value", &self.value)
                .finish()
        }
    }
}

impl Eval for Declaration {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let value = self.value.eval(ctxt)?;
        // FIXME: move declare method to Var
        ctxt.declare(&self.var.ident, value);
        Ok(Value::Nil)
    }
}

accept_default!(Declaration, var:var, value:node,);

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
