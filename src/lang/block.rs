use crate::lang::{Accept, AstNode, Context, Eval, EvalStop, Span, Value, Visitor, VisitorResult};

#[derive(PartialEq, Clone)]
pub struct Block {
    pub(crate) stmts: Vec<AstNode>,
    pub(crate) span: Span,
}

impl core::fmt::Debug for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            f.write_str("Block {")?;
            f.debug_list().entries(&self.stmts).finish()?;
            f.write_str("}")
        } else {
            f.debug_struct("Block")
                .field("stmts", &self.stmts)
                .field("span", &self.span)
                .finish()
        }
    }
}

impl Eval for Block {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let mut child_ctxt = ctxt.new_child();
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
}

impl<T> Accept<T> for Block {
    fn accept(&self, visitor: &mut impl Visitor<T>) -> VisitorResult<T> {
        for stmt in &self.stmts {
            match visitor.visit_node(stmt) {
                v @ VisitorResult::Stop(_) => return v,
                VisitorResult::Continue => {}
            }
        }
        VisitorResult::Continue
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        lang::{Block, Context, Eval, Value},
        parser::tests::{_block, ToValue, add, assign, i, id, var},
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

    #[test]
    fn test_block_eval_ctxt() {
        let mut ctxt = Context::new();
        ctxt.set(&id("a"), "a-initial".to_value());
        ctxt.set(&id("b"), "b-initial".to_value());

        let mut block = _block!();
        block.stmts.push(var(id("a"), i(1)).into());
        block.stmts.push(assign(id("a"), i(42)).into());
        block.stmts.push(assign(id("b"), i(42)).into());

        block.eval(&mut ctxt).expect("return ok");
        assert_eq!(ctxt.get(&id("a")), Some("a-initial".to_value()));
        assert_eq!(ctxt.get(&id("b")), Some(42.to_value()));
    }
}
