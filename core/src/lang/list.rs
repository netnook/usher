use crate::lang::{
    Accept, AstNode, Context, Eval, EvalStop, Span, Value, Visitor, VisitorResult, accept_default,
};

#[derive(PartialEq, Clone, Debug)]
pub struct ListBuilder {
    pub(crate) entries: Vec<AstNode>,
    pub(crate) span: Span,
}

impl ListBuilder {
    pub(crate) fn new(entries: Vec<AstNode>, span: Span) -> Self {
        Self { entries, span }
    }

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

impl Eval for ListBuilder {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let mut list = Vec::new();

        for v in &self.entries {
            let value = v.eval(ctxt)?;
            list.push(value);
        }

        Ok(list.into())
    }
}

accept_default!(ListBuilder, entries:vec:node,);

#[cfg(test)]
mod tests {
    use crate::lang::{Context, Eval, value::List};

    #[test]
    fn test_list_builder_eval() {
        use crate::parser::tests::*;
        let d = list_builder!(s("a"), i(1), add(i(1), i(3)));
        let actual = d.eval(&mut Context::default()).expect("a value");
        let expected: List = vec!["a".to_value(), 1.to_value(), 4.to_value()].into();
        assert_eq!(actual, expected.into());
    }
}
