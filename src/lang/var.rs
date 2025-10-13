use std::cell::LazyCell;

use crate::lang::{
    Accept, AstNode, Context, Eval, EvalStop, Identifier, InternalProgramError, Key, Setter, Span,
    THIS, Value, Visitor, VisitorResult, accept_default, builtin_functions::resolve_function,
    value::Func,
};

#[derive(PartialEq, Clone)]
pub struct This {
    pub(crate) span: Span,
}

impl core::fmt::Debug for This {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            write!(f, "This")
        } else {
            f.debug_struct("This").field("span", &self.span).finish()
        }
    }
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

#[derive(PartialEq, Clone)]
pub struct Var {
    pub(crate) ident: Identifier,
}

impl core::fmt::Debug for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            write!(f, r#"Var("{}")"#, self.ident.key)
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

    pub(crate) fn declare(&self, ctxt: &mut Context, value: Value) {
        ctxt.declare(self.ident.key.clone(), value)
    }
}

impl Eval for Var {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        if let Some(value) = ctxt.get(&self.ident.key) {
            return Ok(value);
        }
        if let Some(func) = resolve_function(&self.ident.key) {
            return Ok(Value::Func(Func::BuiltIn(func)));
        }
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
        if !ctxt.contains_key(&self.ident.key) {
            return Err(InternalProgramError::UndeclaredVariable {
                name: self.ident.as_string(),
                span: self.ident.span,
            }
            .into());
        }
        ctxt.set(&self.ident.key, value);
        Ok(())
    }
}

#[derive(PartialEq, Clone)]
pub struct Declaration {
    pub(crate) var: Var,
    pub(crate) value: Box<AstNode>,
    pub(crate) span: Span,
}

impl core::fmt::Debug for Declaration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            f.debug_struct("Declaration")
                .field("var", &self.var.ident.key)
                .field("value", &self.value)
                .finish()
        } else {
            f.debug_struct("Declaration")
                .field("var", &self.var)
                .field("value", &self.value)
                .field("span", &self.span)
                .finish()
        }
    }
}

impl Eval for Declaration {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let value = self.value.eval(ctxt)?;
        self.var.declare(ctxt, value);
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
