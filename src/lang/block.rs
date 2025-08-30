use crate::lang::{AstNode, Context, InternalProgramError, Span, Value};

#[derive(PartialEq, Debug, Clone)]
pub struct Block {
    pub(crate) stmts: Vec<AstNode>,
    pub(crate) span: Span,
}

impl Block {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        // FIXME: need new context to separate scope !!!
        let mut result = Value::Nil;
        for stmt in &self.stmts {
            result = stmt.eval(ctxt)?;
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        lang::{Block, Context, Value},
        parser::tests::{_block, add, i},
    };

    #[track_caller]
    fn do_test_block(block: &Block, expect: Value) {
        let mut ctxt = Context::new();
        let actual = block.eval(&mut ctxt).expect("return ok");
        assert_eq!(actual, expect);
    }

    #[test]
    fn test_block_eval() {
        let mut block = _block!();

        do_test_block(&block, Value::Nil);

        block.stmts.push(i(42).into());
        do_test_block(&block, Value::Integer(42));

        block.stmts.push(add(i(42), i(7)).into());
        do_test_block(&block, Value::Integer(49));
    }
}
