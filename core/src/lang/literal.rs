use crate::lang::{
    Accept, Context, Eval, EvalStop, Span, Value, Visitor, VisitorResult, accept_default,
};

#[derive(PartialEq, Clone)]
pub struct Literal {
    pub(crate) val: Value,
    pub(crate) span: Span,
}

impl core::fmt::Debug for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            write!(f, "Literal({})", self.val)
        } else {
            f.debug_struct("Literal")
                .field("val", &self.val)
                .field("span", &self.span)
                .finish()
        }
    }
}

impl Literal {
    pub fn new(val: Value, span: Span) -> Self {
        Self { val, span }
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }
}

impl Eval for Literal {
    fn eval(&self, _: &mut Context) -> Result<Value, EvalStop> {
        Ok(self.val.deep_clone())
    }
}

accept_default!(Literal);
