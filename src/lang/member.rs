use super::errors::PropertyList;
use crate::lang::{
    Accept, AstNode, Context, Eval, EvalStop, Identifier, InternalProgramError, Setter, Span,
    Value, Visitor, VisitorResult, accept_default,
    value::{ListCell, ValueType},
};
use std::rc::Rc;

#[derive(PartialEq, Clone)]
pub struct PropertyOf {
    pub(crate) of: Box<AstNode>,
    pub(crate) property: Identifier,
    pub(crate) throw_on_missing_prop: bool,
    pub(crate) span: Span,
}

impl core::fmt::Debug for PropertyOf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            let mut w = f.debug_struct("PropertyOf");
            w.field("of", &self.of);
            w.field("property", &self.property);
            if self.throw_on_missing_prop {
                w.field("throw_on_missing_prop", &self.throw_on_missing_prop);
            }
            w.finish()
        } else {
            f.debug_struct("PropertyOf")
                .field("of", &self.of)
                .field("property", &self.property)
                .field("throw_on_missing_prop", &self.throw_on_missing_prop)
                .field("span", &self.span)
                .finish()
        }
    }
}

accept_default!(PropertyOf, of:node, property:identifier,);

impl Eval for PropertyOf {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let of = self.of.eval(ctxt)?;

        let result = match of {
            Value::Dict(of) => {
                let v = of.borrow().get(&self.property.name);
                let Some(v) = v else {
                    if self.throw_on_missing_prop {
                        return Err(EvalStop::Throw);
                    } else {
                        return Err(InternalProgramError::NoSuchProperty {
                            prop: self.property.clone(),
                            from: PropertyList::Dict(of),
                            span: self.span,
                        }
                        .into_stop());
                    }
                };
                v
            }
            Value::KeyValue(of) => match self.property.name.as_ref() {
                "key" => Value::Str(Rc::new(of.key.clone())),
                "value" => of.value.clone(),
                _ => {
                    if self.throw_on_missing_prop {
                        // FIXME: do we really want to allow ? operator on a key/value object?
                        return Err(EvalStop::Throw);
                    } else {
                        return Err(InternalProgramError::NoSuchProperty {
                            prop: self.property.clone(),
                            from: PropertyList::KeyValue(of),
                            span: self.span,
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
            Value::Dict(of) => of.borrow_mut().set(self.property.name.clone(), value),
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
    pub(crate) throw_on_missing_prop: bool,
    pub(crate) span: Span,
}

impl core::fmt::Debug for IndexOf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            let mut w = f.debug_struct("IndexOf");
            w.field("of", &self.of);
            w.field("index", &self.index);
            if self.throw_on_missing_prop {
                w.field("throw_on_missing_prop", &self.throw_on_missing_prop);
            }
            w.finish()
        } else {
            f.debug_struct("IndexOf")
                .field("of", &self.of)
                .field("index", &self.index)
                .field("throw_on_missing_prop", &self.throw_on_missing_prop)
                .field("span", &self.span)
                .finish()
        }
    }
}

impl IndexOf {
    fn of(&self, ctxt: &mut Context) -> Result<ListCell, EvalStop> {
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
}

impl Eval for IndexOf {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let of = self.of(ctxt)?;

        let index = self.index(ctxt)?;

        let of = of.borrow();

        // FIXME: combine index validation for eval and set ?
        let index = {
            if index.is_negative() || index as usize >= of.len() {
                if self.throw_on_missing_prop {
                    return Err(EvalStop::Throw);
                } else {
                    return InternalProgramError::IndexOutOfRange {
                        index,
                        len: of.len(),
                        span: self.index.span(),
                    }
                    .into();
                }
            }
            index as usize
        };

        let result = of.get(index).expect("index in range");

        Ok(result)
    }
}

accept_default!(IndexOf, of:node, index:node,);

impl Setter for IndexOf {
    fn set(&self, ctxt: &mut Context, value: Value) -> Result<(), EvalStop> {
        let of = self.of(ctxt)?;

        let index = self.index(ctxt)?;

        let mut of = of.borrow_mut();

        // FIXME: combine index validation for eval and set ?
        let index = {
            if index.is_negative() || index as usize >= of.len() {
                return InternalProgramError::IndexOutOfRange {
                    index,
                    len: of.len(),
                    span: self.index.span(),
                }
                .into();
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
        AstNode, Context, EvalStop, InternalProgramError, Span,
        errors::PropertyList,
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
        assert!(i9.eval(&mut ctxt).is_err_and(|e| matches!(
            e,
            EvalStop::Error(InternalProgramError::IndexOutOfRange {
                index: _,
                len: _,
                span: _
            })
        ))); // FIXME: or should this error ????
    }
}
