use crate::lang::{
    Accept, AstNode, Context, Eval, EvalStop, Identifier, InternalProgramError, Key, Setter, Span,
    THIS, Value, Visitor, VisitorResult, accept_default,
};
use std::cell::LazyCell;

#[derive(PartialEq, Clone, Debug)]
pub struct This {
    pub(crate) span: Span,
}

impl This {
    pub(crate) fn new(span: Span) -> Self {
        Self { span }
    }

    pub(crate) fn key() -> Key {
        #[allow(clippy::declare_interior_mutable_const)]
        const THIS_KEY: LazyCell<Key> = LazyCell::new(|| Key::new(THIS.to_string()));
        #[allow(clippy::borrow_interior_mutable_const)]
        (*THIS_KEY).clone()
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }

    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        self.span = Span::zero();
    }
}

impl Eval for This {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        match ctxt.get_this() {
            Some(this) => Ok(this),
            None => Err(EvalStop::Error(InternalProgramError::ThisNotAvailable {
                span: self.span,
            })),
        }
    }
}

accept_default!(This);

#[derive(PartialEq, Clone, Debug)]
pub struct Var {
    pub(crate) ident: Identifier,
}

impl Var {
    pub(crate) const fn new(name: Identifier) -> Self {
        Self { ident: name }
    }

    pub(crate) fn span(&self) -> Span {
        self.ident.span
    }

    pub(crate) fn declare(&self, ctxt: &mut Context, value: Value) -> Result<(), EvalStop> {
        ctxt.declare(self.ident.key.clone(), value).map_err(|_| {
            InternalProgramError::NameAlreadyDeclared {
                name: self.ident.key.to_string(),
                span: self.ident.span,
            }
        })?;
        Ok(())
    }

    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        self.ident.reset_spans();
    }
}

impl Eval for Var {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        if let Some(value) = ctxt.get(&self.ident.key) {
            return Ok(value);
        }
        // if let Some(func) = resolve_function(&self.ident.key) {
        //     return Ok(Value::Func(Func::BuiltIn(func)));
        // }
        Err(InternalProgramError::UndeclaredVariable {
            name: self.ident.key.as_string(),
            span: self.span(),
        }
        .into())
    }
}

accept_default!(Var);

impl Setter for Var {
    fn set(&self, ctxt: &mut Context, value: Value) -> Result<(), EvalStop> {
        ctxt.set(&self.ident.key, value).map_err(|_| {
            InternalProgramError::UndeclaredVariable {
                name: self.ident.as_string(),
                span: self.ident.span,
            }
            .into()
        })
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Declaration {
    pub(crate) var: Var,
    pub(crate) value: Box<AstNode>, // FIXME: this should be called `expr` rather than `value`
    pub(crate) span: Span,
}
impl Declaration {
    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        self.span = Span::zero();
        self.var.reset_spans();
        self.value.reset_spans();
    }
}

impl Eval for Declaration {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let value = self.value.eval(ctxt)?;
        self.var.declare(ctxt, value)?;
        Ok(Value::Nil)
    }
}
accept_default!(Declaration, var:var, value:node,);

#[derive(PartialEq, Debug, Clone)]
pub struct Assignment {
    pub(crate) lhs: Box<AstNode>,
    pub(crate) rhs: Box<AstNode>,
}

impl Assignment {
    pub(crate) fn span(&self) -> Span {
        Span::merge(self.lhs.span(), self.rhs.span())
    }

    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        // self.span = Span::zero();
        self.lhs.reset_spans();
        self.rhs.reset_spans();
    }
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
