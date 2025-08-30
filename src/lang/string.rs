use crate::lang::{AstNode, Context, Eval, InternalProgramError, Value};

#[derive(PartialEq, Debug, Clone)]
pub struct InterpolatedStr {
    pub(crate) parts: Vec<AstNode>,
}

impl Eval for InterpolatedStr {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        if self.parts.len() == 1 {
            self.parts.first().expect("should be there").eval(ctxt)
        } else {
            let mut res = String::new();
            for p in &self.parts {
                let val = p.eval(ctxt)?;
                let s = val.as_string().map_err(|e| InternalProgramError {
                    msg: format!("Error interpolating string: {}", e.msg),
                    span: p.span(),
                })?;
                res.push_str(&format!("{s}"));
            }
            Ok(Value::Str(res))
        }
    }
}

#[cfg(test)]
mod tests {}
