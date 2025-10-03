use crate::lang::{
    Accept, Context, Eval, EvalStop, KeyValueBuilder, Span, Value, Visitor, VisitorResult,
    accept_default, value::Dict,
};

#[derive(PartialEq, Clone)]
pub struct DictBuilder {
    pub(crate) entries: Vec<KeyValueBuilder>,
    pub(crate) span: Span,
}

impl core::fmt::Debug for DictBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            f.write_str("DictBuilder ")?;
            let mut m = f.debug_map();
            for e in &self.entries {
                m.entry(&e.key.key, &e.value);
            }
            m.finish()
        } else {
            f.debug_struct("DictBuilder")
                .field("entries", &self.entries)
                .field("span", &self.span)
                .finish()
        }
    }
}

impl Eval for DictBuilder {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let mut dict = Dict::new();

        for e in &self.entries {
            let key = e.key.key.clone();
            let value = e.value.eval(ctxt)?;
            dict.set(key, value);
        }

        Ok(dict.into())
    }
}

accept_default!(DictBuilder, entries:vec:kv,);

#[cfg(test)]
mod tests {
    use crate::lang::{Context, Eval, value::Dict};

    #[test]
    fn test_dict_builder_eval() {
        use crate::parser::tests::*;
        let d = dict_builder(vec![kv(id("a"), i(1)), kv(id("b"), add(i(1), i(3)))]);
        let actual = d.eval(&mut Context::default()).expect("a value");
        let mut expected = Dict::new();
        expected.set("a".into(), 1.to_value());
        expected.set("b".into(), 4.to_value());
        assert_eq!(actual, expected.into());
    }
}
