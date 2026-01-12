use crate::lang::{
    Accept, Context, Eval, EvalStop, Span, Value, Visitor, VisitorResult, accept_default,
};

#[derive(PartialEq, Clone, Debug)]
pub struct Literal {
    pub(crate) val: Value,
    pub(crate) span: Span,
}

impl Literal {
    pub fn new(val: Value, span: Span) -> Self {
        Self { val, span }
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }

    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        self.span = Span::zero();
    }
}

impl Eval for Literal {
    fn eval(&self, _: &mut Context) -> Result<Value, EvalStop> {
        Ok(self.val.deep_clone())
    }
}

accept_default!(Literal);
