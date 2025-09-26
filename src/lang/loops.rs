use std::rc::Rc;

use super::{Context, Value};
use crate::lang::{AstNode, Block, Eval, EvalStop, Identifier, InternalProgramError, KeyValue};

#[derive(PartialEq, Clone)]
pub struct ForStmt {
    pub(crate) loop_var_1: Identifier,
    pub(crate) loop_var_2: Option<Identifier>,
    pub(crate) loop_expr: Box<AstNode>,
    pub(crate) block: Block,
}

impl core::fmt::Debug for ForStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            let mut w = f.debug_struct("ForStmt");
            w.field("loop_var_1", &self.loop_var_1.name);
            if let Some(loop_var_2) = &self.loop_var_2 {
                w.field("loop_var_2", &loop_var_2.name);
            }
            w.field("loop_expr", &self.loop_expr);
            w.field("block", &self.block);
            w.finish()
        } else {
            f.debug_struct("ForStmt")
                .field("loop_var_1", &self.loop_var_1)
                .field("loop_var_2", &self.loop_var_2)
                .field("loop_expr", &self.loop_expr)
                .field("block", &self.block)
                .finish()
        }
    }
}

impl Eval for ForStmt {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let loop_expr = self.loop_expr.eval(ctxt)?;
        let mut result = Value::Nil;

        let mut child_ctxt = ctxt.new_child();

        match loop_expr {
            Value::List(list) => {
                // FIXME: what happen if list is modified during iteration ???
                let list = list.borrow();
                for val in list.iter() {
                    child_ctxt.reset();
                    child_ctxt.declare(&self.loop_var_1, val);
                    result = match self.block.eval_with_context(&mut child_ctxt) {
                        Ok(v) => v,
                        Err(EvalStop::Break) => todo!(),
                        Err(EvalStop::Continue) => Value::Nil,
                        other @ Err(_) => return other,
                    };
                }
            }
            Value::Dict(dict) => {
                // FIXME: what happen if list is modified during iteration ???
                let dict = dict.borrow();
                for (k, val) in dict.iter() {
                    let loop_val = Value::KeyValue(Rc::new(KeyValue::new(k.clone(), val.clone())));
                    child_ctxt.reset();
                    child_ctxt.declare(&self.loop_var_1, loop_val);
                    // child_ctxt.declare(&self.loop_var_1);
                    result = match self.block.eval_with_context(&mut child_ctxt) {
                        Ok(v) => v,
                        Err(EvalStop::Break) => todo!(),
                        Err(EvalStop::Continue) => Value::Nil,
                        other @ Err(_) => return other,
                    };
                }
            }
            _ => {
                return InternalProgramError::CannotLoopOnValue {
                    got: loop_expr.value_type(),
                    span: self.loop_expr.span(),
                }
                .into();
            }
        }

        // FIXME: break result value ?
        Ok(result)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Break {}

impl Break {
    pub(crate) fn new() -> Self {
        Self {}
    }
}

impl Eval for Break {
    fn eval(&self, _: &mut Context) -> Result<Value, EvalStop> {
        todo!()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Continue {}

impl Continue {
    pub(crate) fn new() -> Self {
        Self {}
    }
}

impl Eval for Continue {
    fn eval(&self, _: &mut Context) -> Result<Value, EvalStop> {
        Err(EvalStop::Continue)
    }
}

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
        let mut ctxt = Context::new();
        ctxt.set(&id("l"), list.into());
        ctxt.set(&id("r"), Value::Integer(0));
        // ctxt.set(&id("f"), Value::Bool(false));

        let stmt = _for(
            id("i"),
            None,
            id("l"),
            _block![assign(id("r"), add(id("r"), id("i")))],
        );
        let actual = stmt.eval(&mut ctxt).expect("a value");
        assert_eq!(ctxt.get(&id("r")), Some(Value::Integer(7)));
        assert_eq!(actual, Value::Nil); // assignment (last stmt in loop) returns Nil

        let stmt = _for(
            id("i"),
            None,
            id("l"),
            _block![assign(id("r"), add(id("r"), id("i"))), id("r")],
        );
        let actual = stmt.eval(&mut ctxt).expect("a value");
        assert_eq!(ctxt.get(&id("r")), Some(Value::Integer(14)));
        assert_eq!(actual, Value::Integer(14)); // last stmt in loop "recalls" value r and leads to loop result
    }
}
