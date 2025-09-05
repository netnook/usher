use crate::lang::{AstNode, Context, Eval, EvalStop, Span, Value, value::List};

#[derive(PartialEq, Clone)]
pub struct ListBuilder {
    pub(crate) entries: Vec<AstNode>,
    pub(crate) span: Span,
}

impl core::fmt::Debug for ListBuilder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            f.write_str("ListBuilder {")?;
            f.debug_list().entries(&self.entries).finish()?;
            f.write_str("}")
        } else {
            f.debug_struct("ListBuilder ")
                .field("entries", &self.entries)
                .field("span", &self.span)
                .finish()
        }
    }
}

impl ListBuilder {
    pub(crate) fn new(entries: Vec<AstNode>, span: Span) -> Self {
        Self { entries, span }
    }
}

impl Eval for ListBuilder {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let mut list = List::new();

        for v in &self.entries {
            let value = v.eval(ctxt)?;
            list.add(value);
        }

        Ok(list.into())
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::{Context, Eval, value::List};

    #[test]
    fn test_list_builder_eval() {
        use crate::parser::tests::*;
        let d = list!(s("a"), i(1), add(i(1), i(3)));
        let actual = d.eval(&mut Context::new()).expect("a value");
        let mut expected = List::new();
        expected.add("a".to_value());
        expected.add(1.to_value());
        expected.add(4.to_value());
        assert_eq!(actual, expected.into());
    }
}
