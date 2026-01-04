use super::errors::PropertyList;
use crate::lang::{
    Accept, AstNode, Context, Eval, EvalStop, Identifier, InternalProgramError, Setter, Span,
    Value, Visitor, VisitorResult, accept_default,
    value::{List, ValueType},
};

#[derive(PartialEq, Clone)]
pub struct PropertyOf {
    pub(crate) of: Box<AstNode>,
    pub(crate) property: Identifier,
    pub(crate) optional_property: bool,
    pub(crate) span: Span,
}
impl PropertyOf {
    pub(crate) fn span(&self) -> Span {
        Span::merge(self.of.span(), self.span)
    }
}

impl core::fmt::Debug for PropertyOf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            let mut w = f.debug_struct("PropertyOf");
            w.field("of", &self.of);
            w.field("property", &self.property);
            if self.optional_property {
                w.field("optional_property", &self.optional_property);
            }
            w.finish()
        } else {
            f.debug_struct("PropertyOf")
                .field("of", &self.of)
                .field("property", &self.property)
                .field("optional_property", &self.optional_property)
                .field("span", &self.span)
                .finish()
        }
    }
}

accept_default!(PropertyOf, of:node,);

impl Eval for PropertyOf {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let of = self.of.eval(ctxt)?;

        let result = match of {
            Value::Dict(of) => {
                let v = of.borrow().get(&self.property.key);
                let Some(v) = v else {
                    if self.optional_property {
                        return InternalProgramError::MissingOptionalProperty.into();
                    } else {
                        return Err(InternalProgramError::NoSuchProperty {
                            prop: self.property.clone(),
                            from: PropertyList::Dict(of),
                            span: self.property.span,
                        }
                        .into_stop());
                    }
                };
                v
            }
            Value::KeyValue(of) => match self.property.key.0.as_str() {
                "key" => Value::Str(of.key.0.clone()),
                "value" => of.value.ref_clone(),
                _ => {
                    if self.optional_property {
                        return InternalProgramError::MissingOptionalProperty.into();
                    } else {
                        return Err(InternalProgramError::NoSuchProperty {
                            prop: self.property.clone(),
                            from: PropertyList::KeyValue(of),
                            span: self.property.span,
                        }
                        .into_stop());
                    }
                }
            },
            _ => {
                return InternalProgramError::SuffixOperatorDoesNotSupportOperand {
                    op: "property-of",
                    got: of.value_type(),
                    span: self.of.span(),
                }
                .into();
            }
        };

        Ok(result)
    }
}

impl Setter for PropertyOf {
    fn set(&self, ctxt: &mut Context, value: Value) -> Result<(), EvalStop> {
        let of = self.of.eval(ctxt)?;

        match of {
            Value::Dict(of) => of.borrow_mut().set(self.property.key.clone(), value),
            _ => {
                return InternalProgramError::SuffixOperatorDoesNotSupportOperand {
                    op: "property-of",
                    got: of.value_type(),
                    span: self.of.span(),
                }
                .into();
            }
        };

        Ok(())
    }
}

#[derive(PartialEq, Clone)]
pub struct IndexOf {
    pub(crate) of: Box<AstNode>,
    pub(crate) index: Box<AstNode>,
    pub(crate) optional_property: bool,
    pub(crate) span: Span,
}

impl core::fmt::Debug for IndexOf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            let mut w = f.debug_struct("IndexOf");
            w.field("of", &self.of);
            w.field("index", &self.index);
            if self.optional_property {
                w.field("optional_property", &self.optional_property);
            }
            w.finish()
        } else {
            f.debug_struct("IndexOf")
                .field("of", &self.of)
                .field("index", &self.index)
                .field("optional_property", &self.optional_property)
                .field("span", &self.span)
                .finish()
        }
    }
}

impl IndexOf {
    fn of(&self, ctxt: &mut Context) -> Result<List, EvalStop> {
        let of = self.of.eval(ctxt)?;

        let Value::List(of) = of else {
            return InternalProgramError::SuffixOperatorDoesNotSupportOperand {
                op: "index-of",
                got: of.value_type(),
                span: self.of.span(),
            }
            .into();
        };

        Ok(of)
    }

    fn index(&self, ctxt: &mut Context) -> Result<isize, EvalStop> {
        let index = self.index.eval(ctxt)?;

        let Value::Integer(index) = index else {
            return InternalProgramError::BadValueType {
                expected: ValueType::Integer,
                actual: index.value_type(),
                span: self.index.span(),
            }
            .into();
        };

        Ok(index)
    }

    pub(crate) fn span(&self) -> Span {
        Span::merge(self.of.span(), self.span)
    }
}

impl Eval for IndexOf {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let of = self.of(ctxt)?;

        let index = self.index(ctxt)?;

        let result = of.get(index).map_err(|e| {
            if self.optional_property {
                InternalProgramError::MissingOptionalProperty.into()
            } else {
                InternalProgramError::IndexOutOfRange {
                    index,
                    len: e.len,
                    span: self.index.span(),
                }
                .into_stop()
            }
        })?;

        Ok(result)
    }
}

accept_default!(IndexOf, of:node, index:node,);

impl Setter for IndexOf {
    fn set(&self, ctxt: &mut Context, value: Value) -> Result<(), EvalStop> {
        let mut of = self.of(ctxt)?;

        let index = self.index(ctxt)?;

        of.set(index, value).map_err(|e| {
            InternalProgramError::IndexOutOfRange {
                index,
                len: e.len,
                span: self.index.span(),
            }
            .into_stop()
        })?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::{
        AstNode, Context, EvalStop, InternalProgramError, Span,
        errors::PropertyList,
        value::{Dict, List},
    };

    #[test]
    fn test_property_of_eval() {
        use crate::parser::tests::*;

        let mut ctxt = {
            let mut ctxt = Context::default();
            let mut d = Dict::new();
            d.set("a".into(), 1.to_value());
            d.set("b".into(), "bbb".to_value());
            ctxt.declare("dict".into(), d.into()).unwrap();
            ctxt
        };

        let prop_a: AstNode = prop_of(var("dict"), "a").into();
        let prop_b: AstNode = prop_of(var("dict"), "b").into();
        let prop_c: AstNode = prop_of(var("dict"), "c").into();

        assert_eq!(prop_a.eval(&mut ctxt).unwrap(), 1.to_value());
        assert_eq!(prop_b.eval(&mut ctxt).unwrap(), "bbb".to_value());
        assert_eq!(
            prop_c.eval(&mut ctxt).unwrap_err(),
            InternalProgramError::NoSuchProperty {
                prop: id("c"),
                from: PropertyList::Dict(dict!("a" => 1, "b" => "bbb").into()),
                span: Span::new(999, 9999)
            }
            .into_stop()
        );
    }
    #[test]
    fn test_index_of_eval() {
        use crate::parser::tests::*;

        let mut ctxt = {
            let mut ctxt = Context::default();
            let mut l = List::new();
            l.push(7.to_value());
            l.push("aaa".to_value());
            l.push(8.to_value());
            ctxt.declare("list".into(), l.into()).unwrap();
            ctxt
        };

        let i0: AstNode = index_of(var("list"), i(0)).into();
        let i1: AstNode = index_of(var("list"), sub(i(4), i(3))).into();
        let i9: AstNode = index_of(var("list"), i(9)).into();

        assert_eq!(i0.eval(&mut ctxt).unwrap(), 7.to_value());
        assert_eq!(i1.eval(&mut ctxt).unwrap(), "aaa".to_value());
        assert!(i9.eval(&mut ctxt).is_err_and(|e| matches!(
            e,
            EvalStop::Error(InternalProgramError::IndexOutOfRange {
                index: 9,
                len: 3,
                span: _
            })
        )));
    }
}
