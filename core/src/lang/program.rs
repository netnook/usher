use crate::lang::{AstNode, Context, Eval, EvalError, EvalStop, InternalProgramError, Value};

#[derive(PartialEq, Clone, Debug)]
pub struct Program<'a> {
    pub file: &'a str,
    pub source: &'a str,
    pub stmts: Vec<AstNode>,
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

    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        for s in &mut self.stmts {
            s.reset_spans();
        }
    }
}
