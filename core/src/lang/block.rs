use crate::lang::{
    Accept, AstNode, Context, Eval, EvalStop, Span, Value, Visitor, VisitorResult, accept_default,
};

#[derive(PartialEq, Clone, Debug)]
pub struct Block {
    pub(crate) stmts: Vec<AstNode>,
    pub(crate) span: Span,
}

impl Eval for Block {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let mut child_ctxt = ctxt.new_scope();
        self.eval_with_context(&mut child_ctxt)
    }
}

impl Block {
    pub(super) fn eval_with_context(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let mut result = Value::Nil;
        for stmt in &self.stmts {
            result = stmt.eval(ctxt)?;
        }
        Ok(result)
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }

    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        self.span = Span::zero();
        for s in &mut self.stmts {
            s.reset_spans();
        }
    }
}

accept_default!(Block, stmts:vec:node,);

#[cfg(test)]
mod tests {
    use crate::{
        lang::{Block, Context, Eval, Value},
        parser::tests::{_block, ToValue, add, assign, decl, i, var},
    };

    #[track_caller]
    fn do_test_block(block: &Block, expect: Value) {
        let mut ctxt = Context::default();
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

    #[test]
    fn test_block_eval_ctxt() {
        let mut ctxt = Context::default();
        ctxt.declare("a".into(), "a-initial".to_value()).unwrap();
        ctxt.declare("b".into(), "b-initial".to_value()).unwrap();

        let mut block = _block!();
        block.stmts.push(decl(var("a"), i(1)).into());
        block.stmts.push(assign(var("a"), i(42)).into());
        block.stmts.push(assign(var("b"), i(42)).into());

        block.eval(&mut ctxt).expect("return ok");
        assert_eq!(ctxt.get(&"a".into()), Some("a-initial".to_value()));
        assert_eq!(ctxt.get(&"b".into()), Some(42.to_value()));
    }
}
