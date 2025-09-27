use crate::lang::{
    Accept, AstNode, Context, Eval, EvalStop, Value, Visitor, VisitorResult, accept_default,
};

#[derive(PartialEq, Clone)]
pub struct ChainCatch {
    pub(crate) inner: Box<AstNode>,
}

impl core::fmt::Debug for ChainCatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            f.write_str("ChainCatch { ")?;
            self.inner.fmt(f)?;
            f.write_str(" }")
        } else {
            f.debug_struct("ChainCatch")
                .field("inner", &self.inner)
                .finish()
        }
    }
}

impl Eval for ChainCatch {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        match self.inner.eval(ctxt) {
            Err(EvalStop::Throw) => Ok(Value::Nil),
            other => other,
        }
    }
}

accept_default!(ChainCatch, inner:node,);
