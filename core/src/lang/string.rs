use crate::lang::{
    Accept, AstNode, Context, Eval, EvalStop, Span, Value, Visitor, VisitorResult, accept_default,
};

#[derive(PartialEq, Clone, Debug)]
pub struct InterpolatedStr {
    pub(crate) parts: Vec<AstNode>,
    pub(crate) span: Span,
}

impl InterpolatedStr {
    pub(crate) fn span(&self) -> Span {
        self.span
    }

    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        self.span = Span::zero();
        for p in &mut self.parts {
            p.reset_spans();
        }
    }
}

impl Eval for InterpolatedStr {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        if self.parts.len() == 1 {
            match self.parts.first().expect("should be there").eval(ctxt)? {
                string @ Value::Str(_) => Ok(string),
                other => {
                    let res = other
                        .as_string()
                        .expect("write value to string should succeed");
                    Ok(Value::Str(res.into()))
                }
            }
        } else {
            let mut res = String::new();

            for p in &self.parts {
                let val = p.eval(ctxt)?;
                val.write_string(&mut res)
                    .expect("write value to string should succeed");
            }

            Ok(Value::Str(res.into()))
        }
    }
}

accept_default!(InterpolatedStr, parts:vec:node,);

#[cfg(test)]
mod tests {}
