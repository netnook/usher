use crate::lang::{
    Accept, AstNode, Context, Eval, EvalStop, Span, Value, Visitor, VisitorResult, accept_default,
};

#[derive(PartialEq, Clone)]
pub struct InterpolatedStr {
    pub(crate) parts: Vec<AstNode>,
    pub(crate) span: Span,
}

impl InterpolatedStr {
    pub(crate) fn span(&self) -> Span {
        self.span
    }
}

impl core::fmt::Debug for InterpolatedStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            f.write_str("InterpolatedStr {")?;
            f.debug_list().entries(&self.parts).finish()?;
            f.write_str("}")
        } else {
            f.debug_struct("InterpolatedStr")
                .field("parts", &self.parts)
                .field("span", &self.span)
                .finish()
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
