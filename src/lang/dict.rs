use crate::lang::{
    AstNode, Context, Eval, Identifier, InternalProgramError, KeyValue, Setter, Span, Value,
    value::{Dict, DictCell, ValueType},
};

#[derive(PartialEq, Debug, Clone)]
pub struct DictBuilder {
    pub(crate) entries: Vec<KeyValue>,
    pub(crate) span: Span,
}

impl Eval for DictBuilder {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        let mut dict = Dict::new();

        for e in &self.entries {
            let key = e.key.name.clone();
            let value = e.value.eval(ctxt)?;
            dict.set(key, value);
        }

        Ok(dict.into())
    }
}

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

#[cfg(test)]
mod tests {
    use crate::lang::{AstNode, Context, Eval, Value, value::Dict};

    #[test]
    fn test_dict_builder_eval() {
        use crate::parser::tests::*;
        let d = dict(vec![kv(id("a"), i(1)), kv(id("b"), add(i(1), i(3)))]);
        let actual = d.eval(&mut Context::new()).expect("a value");
        let mut expected = Dict::new();
        expected.set("a".to_string(), 1.to_value());
        expected.set("b".to_string(), 4.to_value());
        assert_eq!(actual, expected.into());
    }

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
}
