use super::{Context, Value};
use crate::lang::{
    Accept, AstNode, Block, Eval, EvalStop, InternalProgramError, KeyValue, Span, Var, Visitor,
    VisitorResult, accept_default,
};

#[derive(PartialEq, Clone)]
pub struct For {
    pub(crate) iterable: Box<AstNode>,
    pub(crate) loop_item: Var,
    pub(crate) loop_info: Option<Var>,
    pub(crate) block: Block,
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
                .finish()
        }
    }
}

impl Eval for For {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let iterable = self.iterable.eval(ctxt)?;
        let mut result = Value::Nil;

        let mut child_ctxt = ctxt.new_scope();

        match iterable {
            Value::List(list) => {
                // FIXME: what happen if list is modified during iteration ???
                let list = list.borrow();
                for val in list.iter() {
                    child_ctxt.reset();
                    self.loop_item.declare(&mut child_ctxt, val);
                    result = match self.block.eval_with_context(&mut child_ctxt) {
                        Ok(v) => v,
                        Err(EvalStop::Break) => return Ok(Value::Nil),
                        Err(EvalStop::Continue) => Value::Nil,
                        other @ Err(_) => return other,
                    };
                }
            }
            Value::Dict(dict) => {
                // FIXME: what happen if list is modified during iteration ???
                let dict = dict.borrow();
                for (k, val) in dict.iter() {
                    let loop_val = KeyValue::new(k.clone(), val.ref_clone()).into();
                    child_ctxt.reset();
                    self.loop_item.declare(&mut child_ctxt, loop_val);
                    result = match self.block.eval_with_context(&mut child_ctxt) {
                        Ok(v) => v,
                        Err(EvalStop::Break) => return Ok(Value::Nil),
                        Err(EvalStop::Continue) => Value::Nil,
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

        // FIXME: break result value ?
        Ok(result)
    }
}

accept_default!(For, iterable:node, loop_item:var, loop_info:opt:var, block:block,);

#[derive(PartialEq, Clone)]
pub struct Break {
    pub(crate) span: Span,
}

impl core::fmt::Debug for Break {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            write!(f, "Break")
        } else {
            f.debug_struct("Break").field("span", &self.span).finish()
        }
    }
}

impl Break {
    pub(crate) fn new(span: Span) -> Self {
        Self { span }
    }
}

impl Eval for Break {
    fn eval(&self, _: &mut Context) -> Result<Value, EvalStop> {
        Err(EvalStop::Break)
    }
}

accept_default!(Break);

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
}

impl Eval for Continue {
    fn eval(&self, _: &mut Context) -> Result<Value, EvalStop> {
        Err(EvalStop::Continue)
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
        list.add(Value::Integer(1));
        list.add(Value::Integer(2));
        list.add(Value::Integer(4));
        let mut ctxt = Context::default();
        ctxt.set(&"l".into(), list.into());
        ctxt.set(&"r".into(), Value::Integer(0));
        // ctxt.set(&id("f"), Value::Bool(false));

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
