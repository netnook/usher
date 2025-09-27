use crate::lang::{
    Accept, AstNode, BuiltInFunc, Context, Eval, EvalStop, Identifier, InternalProgramError,
    Setter, Span, Value, Visitor, VisitorResult, accept_default,
};

#[derive(PartialEq, Clone)]
pub struct Var {
    pub(crate) name: Identifier,
    pub(crate) span: Span,
}

impl core::fmt::Debug for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            write!(f, r#"Var("{}")"#, self.name.name)
        } else {
            f.debug_struct("Var")
                .field("name", &self.name)
                .field("span", &self.span)
                .finish()
        }
    }
}

impl Var {
    pub(crate) const fn new(name: Identifier, span: Span) -> Self {
        Self { name, span }
    }
}

impl Eval for Var {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let value = ctxt
            .get(&self.name)
            .or_else(|| BuiltInFunc::by_name(&self.name.name).map(|f| f.into()))
            .unwrap_or(Value::Nil);
        Ok(value)
    }
}

accept_default!(Var);

impl Setter for Var {
    fn set(&self, ctxt: &mut Context, value: Value) -> Result<(), EvalStop> {
        ctxt.set(&self.name, value);
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
                .field("var", &self.var.name.name)
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
        ctxt.declare(&self.var.name, value);
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
