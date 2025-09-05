use crate::lang::{AstNode, Block, Context, Eval, InternalProgramError, Value, value::ValueType};

#[derive(PartialEq, Debug, Clone)]
pub struct IfElseStmt {
    pub(crate) conditional_blocks: Vec<ConditionalBlock>,
    pub(crate) else_block: Option<Block>,
}

impl Eval for IfElseStmt {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        let mut child_ctxt = ctxt.new_child();

        for cb in &self.conditional_blocks {
            let cond = cb.condition.eval(ctxt)?;
            let cond = match cond {
                Value::Bool(v) => v,
                other => {
                    return Err(InternalProgramError::BadValueType {
                        expected: ValueType::Boolean,
                        actual: other.value_type(),
                        span: cb.condition.span(),
                    });
                }
            };
            if !cond {
                continue;
            }
            return cb.block.eval(&mut child_ctxt);
        }

        if let Some(else_block) = &self.else_block {
            return else_block.eval(&mut child_ctxt);
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
