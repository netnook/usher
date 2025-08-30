use crate::lang::{
    AstNode, Context, InternalProgramError, Span, Value,
    value::{List, ValueType},
};

#[derive(PartialEq, Debug, Clone)]
pub struct ListBuilder {
    pub(crate) entries: Vec<AstNode>,
    pub(crate) span: Span,
}

impl ListBuilder {
    pub(crate) fn new(entries: Vec<AstNode>, span: Span) -> Self {
        Self { entries, span }
    }
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        let mut list = List::new();

        for v in &self.entries {
            let value = v.eval(ctxt)?;
            list.add(value);
        }

        Ok(list.into())
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct IndexOf {
    pub(crate) from: Box<AstNode>,
    pub(crate) index: Box<AstNode>,
    pub(crate) span: Span,
}

impl IndexOf {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, InternalProgramError> {
        // FIXME: returning an owned value won't work iw we want to be able to assign prop
        // to object and have result in conmext !!
        let from = self.from.eval(ctxt)?;

        let Value::List(from) = from else {
            return Err(InternalProgramError {
                msg: format!(
                    "index-of operator can only be applied to {} but got {} LHS",
                    ValueType::List,
                    from.value_type()
                ),
                span: self.from.span(),
            });
        };

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

        let index = {
            if index.is_negative() || index as usize >= from.len() {
                return Err(InternalProgramError {
                    msg: format!("index {} out of range", index),
                    span: self.index.span(),
                });
            }
            index as usize
        };

        let result = from.get(index).expect("index in range");

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::{AstNode, Context, Value, value::List};

    #[test]
    fn test_list_builder_eval() {
        use crate::parser::tests::*;
        let d = list!(s("a"), i(1), add(i(1), i(3)));
        let actual = d.eval(&mut Context::new()).expect("a value");
        let mut expected = List::new();
        expected.add(Value::Str("a".to_string()));
        expected.add(Value::Integer(1));
        expected.add(Value::Integer(4));
        assert_eq!(actual, expected.into());
    }

    #[test]
    fn test_index_of_eval() {
        use crate::parser::tests::*;

        let mut ctxt = {
            let mut ctxt = Context::new();
            let mut l = List::new();
            l.add(Value::Integer(7));
            l.add(Value::Str("aaa".to_string()));
            l.add(Value::Integer(8));
            ctxt.set(&id("list"), l.into());
            ctxt
        };

        let i0: AstNode = index_of(id("list"), i(0)).into();
        let i1: AstNode = index_of(id("list"), sub(i(4), i(3))).into();
        let i9: AstNode = index_of(id("list"), i(9)).into();

        assert_eq!(i0.eval(&mut ctxt).unwrap(), Value::Integer(7));
        assert_eq!(i1.eval(&mut ctxt).unwrap(), Value::Str("aaa".to_string()));
        // assert_eq!(i9.eval(&mut ctxt).unwrap(), Value::Nil); // FIXME: or should this error ????
        assert!(
            i9.eval(&mut ctxt)
                .is_err_and(|e| e.msg.contains("out of range"))
        ); // FIXME: or should this error ????
    }
}
