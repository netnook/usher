use super::Value;
use crate::lang::{AstNode, Block, Eval, Identifier};

#[derive(PartialEq, Debug, Clone)]
pub struct ForStmt {
    pub(crate) loop_var_1: Identifier,
    pub(crate) loop_var_2: Option<Identifier>,
    pub(crate) loop_expr: Box<AstNode>,
    pub(crate) block: Block,
}

impl Eval for ForStmt {
    fn eval(&self, ctxt: &mut super::Context) -> Result<Value, super::InternalProgramError> {
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
                    result = self.block.eval(&mut child_ctxt)?;
                }
            }
            Value::Dict(_) => todo!(),
            _ => {
                return Err(crate::lang::InternalProgramError::CannotLoopOnValue {
                    got: loop_expr.value_type(),
                    span: self.loop_expr.span(),
                });
            }
        }

        // FIXME: break result value ?
        Ok(result)
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Break {}

impl Break {
    pub(crate) fn eval(_ctxt: &mut super::Context) -> Result<Value, super::InternalProgramError> {
        todo!()
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
