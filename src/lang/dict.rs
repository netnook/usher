use crate::lang::{Context, Eval, EvalStop, KeyValue, Span, Value, value::Dict};

#[derive(PartialEq, Debug, Clone)]
pub struct DictBuilder {
    pub(crate) entries: Vec<KeyValue>,
    pub(crate) span: Span,
}

impl Eval for DictBuilder {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let mut dict = Dict::new();

        for e in &self.entries {
            let key = e.key.name.clone();
            let value = e.value.eval(ctxt)?;
            dict.set(key, value);
        }

        Ok(dict.into())
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::{Context, Eval, value::Dict};

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
}
