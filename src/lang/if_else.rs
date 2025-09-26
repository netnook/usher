use crate::lang::{
    AstNode, Block, Context, Eval, EvalStop, InternalProgramError, Value, value::ValueType,
};

#[derive(PartialEq, Clone)]
pub struct IfElse {
    pub(crate) conditional_blocks: Vec<ConditionalBlock>,
    pub(crate) else_block: Option<Block>,
}

impl core::fmt::Debug for IfElse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            let mut w = f.debug_struct("IfElse");
            for cb in &self.conditional_blocks {
                w.field("conditional", cb);
            }
            if let Some(else_block) = &self.else_block {
                w.field("else", else_block);
            }
            w.finish()
        } else {
            f.debug_struct("IfElse")
                .field("conditional_blocks", &self.conditional_blocks)
                .field("else_block", &self.else_block)
                .finish()
        }
    }
}

impl Eval for IfElse {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        for cb in &self.conditional_blocks {
            let cond = cb.condition.eval(ctxt)?;
            let cond = match cond {
                Value::Bool(v) => v,
                other => {
                    return InternalProgramError::BadValueType {
                        expected: ValueType::Boolean,
                        actual: other.value_type(),
                        span: cb.condition.span(),
                    }
                    .into();
                }
            };
            if !cond {
                continue;
            }
            return cb.block.eval(ctxt);
        }

        if let Some(else_block) = &self.else_block {
            return else_block.eval(ctxt);
        }

        Ok(Value::Nil)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ConditionalBlock {
    pub(crate) condition: AstNode,
    pub(crate) block: Block,
}

#[cfg(test)]
mod tests {
    use crate::lang::{Context, Eval, Value};

    #[test]
    fn test_if_else() {
        use crate::parser::tests::*;

        let mut ctxt = Context::new();
        ctxt.set(&id("t"), Value::Bool(true));
        ctxt.set(&id("f"), Value::Bool(false));

        let stmt = _if!(
            cond(id("t") => _block![i(1)]),
            cond(id("t") => _block![i(2)]),
            else(_block![i(3)])
        );
        let actual = stmt.eval(&mut ctxt).expect("a value");
        assert_eq!(actual, Value::Integer(1));

        let stmt = _if!(
            cond(id("f") => _block![i(1)]),
            cond(id("t") => _block![i(2)]),
            else(_block![i(3)])
        );
        let actual = stmt.eval(&mut ctxt).expect("a value");
        assert_eq!(actual, Value::Integer(2));

        let stmt = _if!(
            cond(id("f") => _block![i(1)]),
            cond(id("f") => _block![i(2)]),
            else(_block![i(3)])
        );
        let actual = stmt.eval(&mut ctxt).expect("a value");
        assert_eq!(actual, Value::Integer(3));
    }
}
