use crate::lang::{
    Accept, AstNode, Context, Eval, EvalStop, InternalProgramError, Value, Visitor, VisitorResult,
    accept_default,
};

#[derive(PartialEq, Clone, Debug)]
pub struct CatchMissingOptionalProperty {
    pub(crate) inner: Box<AstNode>,
}

impl CatchMissingOptionalProperty {
    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        self.inner.reset_spans();
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
