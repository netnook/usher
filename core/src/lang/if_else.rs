use crate::lang::{
    AstNode, Block, Context, Eval, EvalStop, InternalProgramError, Span, Value, accept_default,
    value::ValueType,
    visitor::{Accept, Visitor, VisitorResult},
};

#[derive(PartialEq, Clone, Debug)]
pub struct IfElse {
    pub(crate) conditional_blocks: Vec<ConditionalBlock>,
    pub(crate) else_block: Option<Block>,
    pub(crate) span: Span,
}
impl IfElse {
    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        self.span = Span::zero();
        for cb in &mut self.conditional_blocks {
            cb.reset_spans();
        }
        if let Some(b) = self.else_block.as_mut() {
            b.reset_spans()
        }
    }
}

accept_default!(IfElse, conditional_blocks:vec:conditional_block, else_block:opt:block,);

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
impl ConditionalBlock {
    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        self.condition.reset_spans();
        self.block.reset_spans();
    }
}

accept_default!(ConditionalBlock, condition:node, block:block,);

#[cfg(test)]
mod tests {
    use crate::lang::{Context, Eval, Value};

    #[test]
    fn test_if_else() {
        use crate::parser::tests::*;

        let mut ctxt = Context::default();
        ctxt.declare("t".into(), Value::Bool(true)).unwrap();
        ctxt.declare("f".into(), Value::Bool(false)).unwrap();

        let stmt = _if!(
            cond(var("t") => _block![i(1)]),
            cond(var("t") => _block![i(2)]),
            else(_block![i(3)])
        );
        let actual = stmt.eval(&mut ctxt).expect("a value");
        assert_eq!(actual, Value::Integer(1));

        let stmt = _if!(
            cond(var("f") => _block![i(1)]),
            cond(var("t") => _block![i(2)]),
            else(_block![i(3)])
        );
        let actual = stmt.eval(&mut ctxt).expect("a value");
        assert_eq!(actual, Value::Integer(2));

        let stmt = _if!(
            cond(var("f") => _block![i(1)]),
            cond(var("f") => _block![i(2)]),
            else(_block![i(3)])
        );
        let actual = stmt.eval(&mut ctxt).expect("a value");
        assert_eq!(actual, Value::Integer(3));
    }
}
