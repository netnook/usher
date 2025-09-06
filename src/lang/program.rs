use crate::{
    lang::{AstNode, Context, EvalStop, ProgramError, Value},
    parser::error::find_source_position,
};

#[derive(PartialEq, Clone)]
pub struct Program<'a> {
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
    pub fn run(&self) -> Result<Value, ProgramError> {
        match self.do_run() {
            Ok(v) => Ok(v),
            Err(EvalStop::Return(v)) => Ok(v),
            Err(EvalStop::Error(e)) => {
                let info = find_source_position(self.source, e.span().start);
                Err(ProgramError {
                    msg: format!("{e}"),
                    line_no: info.0.line,
                    char_no: info.0.char,
                    line: info.1.to_string(),
                })
            }
            Err(v) => panic!("unexpected program response {v:?}. This is a bug!"),
        }
    }

    fn do_run(&self) -> Result<Value, EvalStop> {
        let mut ctxt = Context::new();
        let mut res = Value::Nil;
        for stmt in &self.stmts {
            res = stmt.eval(&mut ctxt)?;
        }
        Ok(res)
    }
}
