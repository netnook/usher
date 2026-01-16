use super::{Context, Value};
use crate::lang::{
    Accept, AstNode, Block, Dict, Eval, EvalStop, InternalProgramError, Span, Var, Visitor,
    VisitorResult, accept_default,
};

#[derive(PartialEq, Clone, Debug)]
pub struct For {
    pub(crate) iterable: Box<AstNode>,
    pub(crate) loop_item_a: Var,
    pub(crate) loop_item_b: Option<Var>,
    pub(crate) loop_item_c: Option<Var>,
    pub(crate) block: Block,
    pub(crate) span: Span,
}
impl For {
    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        self.span = Span::zero();
        self.iterable.reset_spans();
        self.loop_item_a.reset_spans();
        self.loop_item_b.as_mut().map(|v| v.reset_spans());
        self.loop_item_c.as_mut().map(|v| v.reset_spans());
        self.block.reset_spans();
    }
}

impl Eval for For {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let iterable = self.iterable.eval(ctxt)?;
        let mut result = Value::Nil;

        match iterable {
            Value::List(list) => {
                let mut iter = list.iter();

                while let Some((idx, val)) = iter.next() {
                    let mut child_ctxt = ctxt.new_scope();
                    self.loop_item_a.declare(&mut child_ctxt, val.ref_clone())?;
                    if let Some(loop_index) = &self.loop_item_b {
                        loop_index.declare(&mut child_ctxt, Value::Integer(idx))?;
                    }
                    if let Some(loop_info) = &self.loop_item_c {
                        let mut dict = Dict::new();
                        dict.set("first".into(), iter.is_first().into());
                        dict.set("last".into(), iter.is_last().into());
                        loop_info.declare(&mut child_ctxt, dict.into())?;
                    }
                    result = match self.block.eval_with_context(&mut child_ctxt) {
                        Ok(v) => v,
                        Err(EvalStop::Break(value, _)) => return Ok(value),
                        Err(EvalStop::Continue(_)) => Value::Nil,
                        other @ Err(_) => return other,
                    };
                }
            }
            Value::Dict(dict) => {
                let Some(loop_index) = &self.loop_item_b else {
                    return Err(InternalProgramError::LoopOnDictMissingValueDeclartion {
                        span: self.span,
                    }
                    .into_stop());
                };
                let mut iter = dict.iter();

                while let Some((key, val)) = iter.next() {
                    let mut child_ctxt = ctxt.new_scope();
                    self.loop_item_a.declare(&mut child_ctxt, key.into())?;
                    loop_index.declare(&mut child_ctxt, val.clone())?;
                    if let Some(loop_info) = &self.loop_item_c {
                        let mut dict = Dict::new();
                        dict.set("first".into(), iter.is_first().into());
                        dict.set("last".into(), iter.is_last().into());
                        loop_info.declare(&mut child_ctxt, dict.into())?;
                    }
                    result = match self.block.eval_with_context(&mut child_ctxt) {
                        Ok(v) => v,
                        Err(EvalStop::Break(value, _)) => return Ok(value),
                        Err(EvalStop::Continue(_)) => Value::Nil,
                        other @ Err(_) => return other,
                    };
                }
            }
            _ => {
                return InternalProgramError::CannotLoopOnValue {
                    got: iterable.value_type(),
                    span: self.iterable.span(),
                }
                .into();
            }
        }

        Ok(result)
    }
}

accept_default!(For, iterable:node, loop_item_a:var, loop_item_b:opt:var, loop_item_c:opt:var, block:block,);

#[derive(PartialEq, Clone, Debug)]
pub struct Break {
    pub(crate) value: Option<Box<AstNode>>,
    pub(crate) span: Span,
}

impl Break {
    pub(crate) fn span(&self) -> Span {
        self.span
    }

    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        self.span = Span::zero();
        if let Some(v) = self.value.as_mut() {
            v.reset_spans()
        }
    }
}

impl Eval for Break {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let value = match &self.value {
            Some(v) => v.eval(ctxt)?,
            None => Value::Nil,
        };

        Err(EvalStop::Break(value, self.span))
    }
}

accept_default!(Break, value:opt:node,);

#[derive(PartialEq, Clone, Debug)]
pub struct Continue {
    pub(crate) span: Span,
}

impl Continue {
    pub(crate) fn new(span: Span) -> Self {
        Self { span }
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }

    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        self.span = Span::zero();
    }
}

impl Eval for Continue {
    fn eval(&self, _: &mut Context) -> Result<Value, EvalStop> {
        Err(EvalStop::Continue(self.span))
    }
}

accept_default!(Continue);

#[cfg(test)]
mod tests {
    use crate::lang::{Context, Eval, Value, value::List};

    #[test]
    fn test_if_else() {
        use crate::parser::tests::*;

        let mut list = List::new();
        list.push(Value::Integer(1));
        list.push(Value::Integer(2));
        list.push(Value::Integer(4));
        let mut ctxt = Context::default();
        ctxt.declare("l".into(), list.into()).unwrap();
        ctxt.declare("r".into(), Value::Integer(0)).unwrap();

        let stmt = _for!(
            var("i");
            var("l");
            _block![assign(var("r"), add(var("r"), var("i")))]
        );
        let actual = stmt.eval(&mut ctxt).expect("a value");
        assert_eq!(ctxt.get(&"r".into()), Some(Value::Integer(7)));
        assert_eq!(actual, Value::Nil); // assignment (last stmt in loop) returns Nil

        let stmt = _for!(
            var("i");
            var("l");
            _block![assign(var("r"), add(var("r"), var("i"))), var("r")]
        );
        let actual = stmt.eval(&mut ctxt).expect("a value");
        assert_eq!(ctxt.get(&"r".into()), Some(Value::Integer(14)));
        assert_eq!(actual, Value::Integer(14)); // last stmt in loop "recalls" value r and leads to loop result
    }
}
