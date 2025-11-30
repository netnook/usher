use crate::lang::{
    Accept, AstNode, Context, Eval, EvalStop, InternalProgramError, Value, Visitor, VisitorResult,
    accept_default,
};

#[derive(PartialEq, Clone)]
pub struct CatchMissingOptionalProperty {
    pub(crate) inner: Box<AstNode>,
}

impl core::fmt::Debug for CatchMissingOptionalProperty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            f.write_str("CatchMissingOptionalProperty { ")?;
            self.inner.fmt(f)?;
            f.write_str(" }")
        } else {
            f.debug_struct("CatchMissingOptionalProperty")
                .field("inner", &self.inner)
                .finish()
        }
    }
}

impl Eval for CatchMissingOptionalProperty {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        match self.inner.eval(ctxt) {
            Err(EvalStop::Error(InternalProgramError::MissingOptionalProperty)) => Ok(Value::Nil),
            other => other,
        }
    }
}

accept_default!(CatchMissingOptionalProperty, inner:node,);
