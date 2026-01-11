use super::{Context, Value};
use crate::lang::{
    Accept, AstNode, Block, Eval, EvalStop, InternalProgramError, Span, Var, Visitor,
    VisitorResult, accept_default,
};

#[derive(PartialEq, Clone)]
pub struct For {
    pub(crate) iterable: Box<AstNode>,
    pub(crate) loop_item: Var,
    pub(crate) loop_info: Option<Var>,
    pub(crate) block: Block,
    pub(crate) span: Span,
}

impl core::fmt::Debug for For {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            let mut w = f.debug_struct("For");
            w.field("iterable", &self.iterable);
            w.field("loop_item", &self.loop_item);
            if let Some(loop_info) = &self.loop_info {
                w.field("loop_info", &loop_info.ident.key);
            }
            w.field("block", &self.block);
            w.finish()
        } else {
            f.debug_struct("For")
                .field("iterable", &self.iterable)
                .field("loop_item", &self.loop_item)
                .field("loop_info", &self.loop_info)
                .field("block", &self.block)
                .field("span", &self.span)
                .finish()
        }
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
                    self.loop_item.declare(&mut child_ctxt, val.ref_clone())?;
                    // FIXME: this loop_info should not be the index, but a custom object with .index, .first, .last properties
                    if let Some(loop_info) = &self.loop_info {
                        loop_info.declare(&mut child_ctxt, Value::Integer(idx))?;
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
                let Some(loop_info) = &self.loop_info else {
                    return Err(InternalProgramError::LoopOnDictMissingValueDeclartion {
                        span: self.span,
                    }
                    .into_stop());
                };
                let mut iter = dict.iter();

                while let Some((key, val)) = iter.next() {
                    let mut child_ctxt = ctxt.new_scope();
                    // FIXME: dict should always have vars for key and value, and an optional third as loop_info (with .index, .first, .last properties)
                    self.loop_item.declare(&mut child_ctxt, key.into())?;
                    loop_info.declare(&mut child_ctxt, val.clone())?;
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

accept_default!(For, iterable:node, loop_item:var, loop_info:opt:var, block:block,);

#[derive(PartialEq, Clone)]
pub struct Break {
    pub(crate) value: Option<Box<AstNode>>,
    pub(crate) span: Span,
}

impl core::fmt::Debug for Break {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            match &self.value {
                Some(v) => {
                    f.write_str("Break { ")?;
                    v.fmt(f)?;
                    f.write_str(" }")
                }
                None => write!(f, "Break"),
            }
        } else {
            f.debug_struct("Break")
                .field("value", &self.value)
                .field("span", &self.span)
                .finish()
        }
    }
}

impl Break {
    pub(crate) fn span(&self) -> Span {
        self.span
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

#[derive(PartialEq, Clone)]
pub struct Continue {
    pub(crate) span: Span,
}

impl core::fmt::Debug for Continue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            write!(f, "Continue")
        } else {
            f.debug_struct("Continue")
                .field("span", &self.span)
                .finish()
        }
    }
}

impl Continue {
    pub(crate) fn new(span: Span) -> Self {
        Self { span }
    }

    pub(crate) fn span(&self) -> Span {
        self.span
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

        let stmt = _for(
            var("i"),
            None,
            var("l"),
            _block![assign(var("r"), add(var("r"), var("i")))],
        );
        let actual = stmt.eval(&mut ctxt).expect("a value");
        assert_eq!(ctxt.get(&"r".into()), Some(Value::Integer(7)));
        assert_eq!(actual, Value::Nil); // assignment (last stmt in loop) returns Nil

        let stmt = _for(
            var("i"),
            None,
            var("l"),
            _block![assign(var("r"), add(var("r"), var("i"))), var("r")],
        );
        let actual = stmt.eval(&mut ctxt).expect("a value");
        assert_eq!(ctxt.get(&"r".into()), Some(Value::Integer(14)));
        assert_eq!(actual, Value::Integer(14)); // last stmt in loop "recalls" value r and leads to loop result
    }
}
