use crate::lang::{
    AstNode, Context, Identifier, InternalProgramError, KeyValue, Span, Value,
    value::{Dict, ValueType},
};

#[derive(PartialEq, Debug, Clone)]
pub struct DictBuilder {
    pub(crate) entries: Vec<KeyValue>,
    pub(crate) span: Span,
}

impl DictBuilder {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
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
    pub(crate) from: Box<AstNode>,
    pub(crate) property: Identifier,
    pub(crate) span: Span,
}

impl PropertyOf {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        // FIXME: returning an owned value won't work iw we want to be able to assign prop
        // to object and have result in conmext !!
        let from = self.from.eval(ctxt)?;

        let Value::Dict(from) = from else {
            return Err(InternalProgramError {
                msg: format!(
                    "Property-of operator can only be applied to {} but got {} LHS",
                    ValueType::Dict,
                    from.value_type()
                ),
                span: self.from.span(),
            });
        };

        let result = from.get(&self.property.name).unwrap_or(Value::Nil);

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::AstNode;
    use crate::lang::{Context, Value, value::Dict};

    #[test]
    fn test_dict_builder_eval() {
        use crate::parser::tests::*;
        let d = dict(vec![kv(id("a"), i(1)), kv(id("b"), add(i(1), i(3)))]);
        let actual = d.eval(&mut Context::new()).expect("a value");
        let mut expected = Dict::new();
        expected.set("a".to_string(), Value::Integer(1));
        expected.set("b".to_string(), Value::Integer(4));
        assert_eq!(actual, expected.into());
    }

    #[test]
    fn test_property_of_eval() {
        use crate::parser::tests::*;

        let mut ctxt = {
            let mut ctxt = Context::new();
            let mut d = Dict::new();
            d.set("a".to_string(), Value::Integer(1));
            d.set("b".to_string(), Value::Str("bbb".to_string()));
            // let id = "dict".into();
            // ctxt.set(&id, d.into());
            ctxt.set(&id("dict"), d.into());
            ctxt
        };

        let prop_a: AstNode = prop_of(id("dict"), "a").into();
        let prop_b: AstNode = prop_of(id("dict"), "b").into();
        let prop_c: AstNode = prop_of(id("dict"), "c").into();

        assert_eq!(prop_a.eval(&mut ctxt).unwrap(), Value::Integer(1));
        assert_eq!(
            prop_b.eval(&mut ctxt).unwrap(),
            Value::Str("bbb".to_string())
        );
        assert_eq!(prop_c.eval(&mut ctxt).unwrap(), Value::Nil);
    }
}
