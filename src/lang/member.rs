use crate::lang::{
    AstNode, Context, Eval, Identifier, InternalProgramError, Setter, Span, Value,
    value::{DictCell, ListCell, ValueType},
};

#[derive(PartialEq, Debug, Clone)]
pub struct PropertyOf {
    pub(crate) of: Box<AstNode>,
    pub(crate) property: Identifier,
    pub(crate) span: Span,
}

impl PropertyOf {
    fn of(&self, ctxt: &mut Context) -> Result<DictCell, InternalProgramError> {
        let of = self.of.eval(ctxt)?;

        let Value::Dict(of) = of else {
            return Err(InternalProgramError {
                msg: format!(
                    "property-of operator can only be applied to {} but got {} LHS",
                    ValueType::Dict,
                    of.value_type()
                ),
                span: self.of.span(),
            });
        };

        Ok(of)
    }
}

impl Eval for PropertyOf {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        let of = self.of(ctxt)?;

        let result = of.borrow().get(&self.property.name).unwrap_or(Value::Nil);

        Ok(result)
    }
}

impl Setter for PropertyOf {
    fn set(&self, ctxt: &mut Context, value: Value) -> Result<(), InternalProgramError> {
        let of = self.of(ctxt)?;

        of.borrow_mut().set(self.property.name.clone(), value);

        Ok(())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct IndexOf {
    pub(crate) of: Box<AstNode>,
    pub(crate) index: Box<AstNode>,
    pub(crate) span: Span,
}

impl IndexOf {
    fn of(&self, ctxt: &mut Context) -> Result<ListCell, InternalProgramError> {
        let of = self.of.eval(ctxt)?;

        let Value::List(of) = of else {
            return Err(InternalProgramError {
                msg: format!(
                    "index-of operator can only be applied to {} but got {} LHS",
                    ValueType::List,
                    of.value_type()
                ),
                span: self.of.span(),
            });
        };

        Ok(of)
    }

    fn index(&self, ctxt: &mut Context) -> Result<isize, InternalProgramError> {
        let index = self.index.eval(ctxt)?;

        let Value::Integer(index) = index else {
            return Err(InternalProgramError {
                msg: format!(
                    "index-of operator requires a {} index but got {}.",
                    ValueType::Integer,
                    index.value_type()
                ),
                span: self.index.span(),
            });
        };

        Ok(index)
    }
}

impl Eval for IndexOf {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        let of = self.of(ctxt)?;

        let index = self.index(ctxt)?;

        let of = of.borrow();

        // FIXME: combine index validation for eval and set ?
        let index = {
            if index.is_negative() || index as usize >= of.len() {
                return Err(InternalProgramError {
                    msg: format!("index {} out of range", index),
                    span: self.index.span(),
                });
            }
            index as usize
        };

        let result = of.get(index).expect("index in range");

        Ok(result)
    }
}

impl Setter for IndexOf {
    fn set(&self, ctxt: &mut Context, value: Value) -> Result<(), InternalProgramError> {
        let of = self.of(ctxt)?;

        let index = self.index(ctxt)?;

        let mut of = of.borrow_mut();

        // FIXME: combine index validation for eval and set ?
        let index = {
            if index.is_negative() || index as usize >= of.len() {
                return Err(InternalProgramError {
                    msg: format!("index {} out of range", index),
                    span: self.index.span(),
                });
            }
            index as usize
        };

        of.set(index, value);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::{
        AstNode, Context, Value,
        value::{Dict, List},
    };

    #[test]
    fn test_property_of_eval() {
        use crate::parser::tests::*;

        let mut ctxt = {
            let mut ctxt = Context::new();
            let mut d = Dict::new();
            d.set("a".to_string(), 1.to_value());
            d.set("b".to_string(), "bbb".to_value());
            ctxt.set(&id("dict"), d.into());
            ctxt
        };

        let prop_a: AstNode = prop_of(id("dict"), "a").into();
        let prop_b: AstNode = prop_of(id("dict"), "b").into();
        let prop_c: AstNode = prop_of(id("dict"), "c").into();

        assert_eq!(prop_a.eval(&mut ctxt).unwrap(), 1.to_value());
        assert_eq!(prop_b.eval(&mut ctxt).unwrap(), "bbb".to_value());
        assert_eq!(prop_c.eval(&mut ctxt).unwrap(), Value::Nil);
    }
    #[test]
    fn test_index_of_eval() {
        use crate::parser::tests::*;

        let mut ctxt = {
            let mut ctxt = Context::new();
            let mut l = List::new();
            l.add(7.to_value());
            l.add("aaa".to_value());
            l.add(8.to_value());
            ctxt.set(&id("list"), l.into());
            ctxt
        };

        let i0: AstNode = index_of(id("list"), i(0)).into();
        let i1: AstNode = index_of(id("list"), sub(i(4), i(3))).into();
        let i9: AstNode = index_of(id("list"), i(9)).into();

        assert_eq!(i0.eval(&mut ctxt).unwrap(), 7.to_value());
        assert_eq!(i1.eval(&mut ctxt).unwrap(), "aaa".to_value());
        // assert_eq!(i9.eval(&mut ctxt).unwrap(), Value::Nil); // FIXME: or should this error ????
        assert!(
            i9.eval(&mut ctxt)
                .is_err_and(|e| e.msg.contains("out of range"))
        ); // FIXME: or should this error ????
    }
}
