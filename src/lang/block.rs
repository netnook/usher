use crate::lang::{AstNode, Context, InternalProgramError, Span, Value};

#[derive(PartialEq, Debug, Clone)]
pub struct Block {
    pub(crate) stmts: Vec<AstNode>,
    pub(crate) span: Span,
}

impl Block {
    pub fn eval(&self, _ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        todo!()
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_block_eval() {}
}
