use crate::lang::{AstNode, Context, Eval, EvalError, EvalStop, InternalProgramError, Value};

#[derive(PartialEq, Clone)]
pub struct Program<'a> {
    pub file: &'a str,
    pub source: &'a str,
    pub stmts: Vec<AstNode>,
}

impl<'a> core::fmt::Debug for Program<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            f.write_str("Program { ")?;
            f.debug_list().entries(&self.stmts).finish()?;
            f.write_str(" }")
        } else {
            f.debug_struct("Program")
                .field("stmts", &self.stmts)
                .field("source", &self.source)
                .finish()
        }
    }
}

impl<'a> Program<'a> {
    pub fn eval(&self) -> Result<Value, EvalError<'a>> {
        let mut ctxt = Context::default();
        self.eval_with_context(&mut ctxt)
    }

    pub fn eval_with_context(&self, ctxt: &mut Context) -> Result<Value, EvalError<'a>> {
        self.internal_eval(ctxt)
    }

    fn internal_eval(&self, ctxt: &mut Context) -> Result<Value, EvalError<'a>> {
        match self.do_internal_eval(ctxt) {
            Ok(v) => Ok(v),
            Err(EvalStop::Return(v, _)) => Ok(v),
            Err(EvalStop::Error(e)) => Err(EvalError {
                file: self.file,
                source: self.source,
                error: e,
            }),
            Err(v) => panic!("unexpected program response {v:?}. This is a bug!"),
        }
    }

    fn do_internal_eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let mut res = Value::Nil;
        for stmt in &self.stmts {
            res = stmt.eval(ctxt).map_err(|e| match e {
                EvalStop::Break(_, span) => {
                    EvalStop::Error(InternalProgramError::BreakWithoutLoop { span })
                }
                EvalStop::Continue(span) => {
                    EvalStop::Error(InternalProgramError::ContinueWithoutLoop { span })
                }
                e => e,
            })?;
        }
        Ok(res)
    }
}
