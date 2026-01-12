use crate::lang::{
    Accept, Context, Eval, EvalStop, KeyValueBuilder, Span, Value, Visitor, VisitorResult,
    accept_default, value::Dict,
};

#[derive(PartialEq, Clone, Debug)]
pub struct DictBuilder {
    pub(crate) entries: Vec<KeyValueBuilder>,
    pub(crate) span: Span,
}
impl DictBuilder {
    pub(crate) fn span(&self) -> Span {
        self.span
    }

    #[cfg(test)]
    pub(crate) fn reset_spans(&mut self) {
        self.span = Span::zero();
        for e in &mut self.entries {
            e.reset_spans();
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
