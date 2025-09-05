use crate::lang::{AstNode, Context, Eval, EvalStop, Value};
use std::rc::Rc;

#[derive(PartialEq, Clone)]
pub struct InterpolatedStr {
    pub(crate) parts: Vec<AstNode>,
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
                .finish()
        }
    }
}

impl Eval for InterpolatedStr {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        if self.parts.len() == 1 {
            self.parts.first().expect("should be there").eval(ctxt)
        } else {
            let mut res = String::new();
            for p in &self.parts {
                let val = p.eval(ctxt)?;
                let s = val.as_string()?;
                res.push_str(&format!("{s}"));
            }
            Ok(Value::Str(Rc::new(res)))
        }
    }
}

#[cfg(test)]
mod tests {}
