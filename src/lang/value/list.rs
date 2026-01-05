use crate::lang::{
    Context, EvalStop, FunctionCall, InternalProgramError, Key, Value,
    function::{MethodResolver, MethodType},
};
use std::{
    cell::RefCell,
    fmt::{Display, Write},
    rc::Rc,
};
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
#[error("Index out of range.")]
pub struct IndexOutOfRangeError {
    pub(crate) len: usize,
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct List {
    content: Rc<RefCell<Vec<Value>>>,
}

impl List {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, index: isize) -> Result<Value, IndexOutOfRangeError> {
        if index < 0 {
            return Err({
                IndexOutOfRangeError {
                    len: self.content.borrow().len(),
                }
            });
        }
        match self.content.borrow().get(index as usize) {
            Some(v) => Ok(v.clone()),
            None => Err({
                IndexOutOfRangeError {
                    len: self.content.borrow().len(),
                }
            }),
        }
    }

    pub fn set(&mut self, index: isize, value: Value) -> Result<(), IndexOutOfRangeError> {
        let mut content = self.content.borrow_mut();
        if index.is_negative() || index as usize >= content.len() {
            return Err(IndexOutOfRangeError { len: content.len() });
        }
        content[index as usize] = value;
        Ok(())
    }

    pub fn remove(&mut self, index: isize) -> Result<Value, IndexOutOfRangeError> {
        let mut content = self.content.borrow_mut();
        if index.is_negative() || index as usize >= content.len() {
            return Err(IndexOutOfRangeError { len: content.len() });
        }
        Ok(content.remove(index as usize))
    }

    pub fn push(&mut self, value: Value) {
        let mut content = self.content.borrow_mut();
        content.push(value);
    }

    pub fn len(&self) -> usize {
        self.content.borrow().len()
    }

    pub fn is_empty(&self) -> bool {
        self.content.borrow().is_empty()
    }

    pub(crate) fn shallow_clone(&self) -> Self {
        self.content.borrow().clone().into()
    }

    pub(crate) fn deep_clone(&self) -> Self {
        let mut result = Self::new();
        let content = self.content.borrow();
        for e in &*content {
            result.push(e.deep_clone());
        }
        result
    }
}

impl From<Vec<Value>> for List {
    fn from(value: Vec<Value>) -> Self {
        Self {
            content: Rc::new(RefCell::new(value)),
        }
    }
}

impl Display for List {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.write_char('[')?;

        let mut first = true;
        for v in &*self.content.borrow() {
            if !first {
                fmt.write_char(',')?;
            } else {
                first = false;
            }
            v.fmt(fmt)?;
        }

        fmt.write_char(']')?;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Default)]
pub struct ListIter {
    list: List,
    next: isize,
}

impl ListIter {
    pub fn new(list: List) -> Self {
        Self { list, next: 0 }
    }

    pub fn next(&mut self) -> Option<(isize, Value)> {
        let val = self.list.get(self.next);

        match val {
            Ok(val) => {
                let r = Some((self.next, val));
                self.next += 1;
                r
            }
            Err(_e) => None,
        }
    }
}

impl MethodResolver for List {
    fn resolve_method(&self, key: &Key) -> Option<MethodType<Self>> {
        match key.as_str() {
            "push" => Some(list_push),
            "remove" => Some(list_remove),
            _ => None,
        }
    }
}

fn list_push(call: &FunctionCall, this: List, ctxt: &mut Context) -> Result<Value, EvalStop> {
    let args = call
        .args
        .iter()
        .map(|a| a.eval(ctxt))
        .collect::<Result<Vec<Value>, EvalStop>>()?;

    let mut list = this.content.borrow_mut();
    for arg in args {
        list.push(arg);
    }

    Ok(Value::Nil)
}

fn list_remove(call: &FunctionCall, mut this: List, ctxt: &mut Context) -> Result<Value, EvalStop> {
    // FIXME: have an arg spec as a struct and a macro which can "decode" the args into that struct ?
    let mut iter = call.args.iter();
    let idx_arg = call.required_positional_arg("index", iter.next())?;
    let idx_val = call.require_integer("index", idx_arg, ctxt)?;

    if let Some(arg) = iter.next() {
        return Err(
            InternalProgramError::FunctionCallUnexpectedArgument { span: arg.span() }.into(),
        );
    };

    match this.remove(idx_val) {
        Ok(v) => Ok(v),
        Err(e) => Err(InternalProgramError::IndexOutOfRange {
            index: idx_val,
            len: e.len,
            span: idx_arg.span(),
        }
        .into_stop()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::Value;
    use crate::parser::tests::ToValue;

    #[test]
    fn test_ref_clone() {
        {
            let mut l = List::new();
            l.push(42.into());
            l.push(43.into());
            let mut a = Value::List(l);
            let b = a.ref_clone();
            assert_eq!(a, b);

            let Value::List(l) = &mut a else { panic!() };
            l.push(45.into());
            assert_eq!(a, b);
        }
        {
            let mut sub = List::new();
            sub.push(42.to_value());
            sub.push(43.to_value());
            let mut l = List::new();
            l.push(44.to_value());
            l.push(sub.into());
            let mut a = Value::List(l);
            let b = a.ref_clone();
            assert_eq!(a, b);

            let Value::List(l) = &mut a else { panic!() };
            let Value::List(sub) = &mut l.get(1).unwrap() else {
                panic!()
            };
            sub.push(45.into());
            assert_eq!(a, b); // a and b still differ after sub list modified
        }
    }

    #[test]
    fn test_shallow_clone() {
        {
            let mut sub = List::new();
            sub.push(42.to_value());
            sub.push(43.to_value());
            let mut l = List::new();
            l.push(44.to_value());
            l.push(sub.into());
            let mut a = Value::List(l);
            let b = a.shallow_clone();
            assert_eq!(a, b);

            let Value::List(l) = &mut a else { panic!() };
            let Value::List(sub) = &mut l.get(1).unwrap() else {
                panic!()
            };
            sub.push(45.into());
            assert_eq!(a, b); // a and b still same after sub list modified (shallow copy only)

            let Value::List(l) = &mut a else { panic!() };
            l.push(46.into());
            assert_ne!(a, b); // and and b differ after top list modified
        }
    }

    #[test]
    fn test_deep_clone() {
        {
            let mut l = List::new();
            l.push(42.into());
            l.push(43.into());
            let mut a = Value::List(l);
            let b = a.deep_clone();
            assert_eq!(a, b);

            let Value::List(l) = &mut a else { panic!() };
            l.push(45.into());
            assert_ne!(a, b);
        }
        {
            let mut sub = List::new();
            sub.push(42.to_value());
            sub.push(43.to_value());
            let mut l = List::new();
            l.push(44.to_value());
            l.push(sub.into());
            let mut a = Value::List(l);
            let b = a.deep_clone();
            assert_eq!(a, b);

            let Value::List(l) = &mut a else { panic!() };
            let Value::List(sub) = &mut l.get(1).unwrap() else {
                panic!()
            };
            sub.push(45.into());
            assert_ne!(a, b);
        }
    }

    #[test]
    fn test_list() {
        let mut l = List::new();

        assert_eq!(l.get(0), Err(IndexOutOfRangeError { len: 0 }));
        assert_eq!(l.get(1), Err(IndexOutOfRangeError { len: 0 }));
        assert_eq!(l.len(), 0);

        l.push(42.to_value());
        assert_eq!(l.get(0), Ok(42.to_value()));
        assert_eq!(l.get(1), Err(IndexOutOfRangeError { len: 1 }));
        assert_eq!(l.len(), 1);

        l.push("aaa".to_value());
        assert_eq!(l.get(0), Ok(42.to_value()));
        assert_eq!(l.get(1), Ok("aaa".to_value()));
        assert_eq!(l.len(), 2);
    }

    #[test]
    fn test_list_remove() {
        let a = "aaa".to_value();
        let b = "bbb".to_value();
        let c = "ccc".to_value();
        let mut l: List = vec![a.clone(), b.clone(), c.clone()].into();

        assert_eq!(l.remove(-1), Err(IndexOutOfRangeError { len: 3 }));
        assert_eq!(l.remove(5), Err(IndexOutOfRangeError { len: 3 }));
        assert_eq!(l.len(), 3);

        assert_eq!(l.remove(1), Ok(b.clone()));
        assert_eq!(l.len(), 2);
        assert_eq!(*l.content.borrow(), vec![a.clone(), c.clone()]);

        assert_eq!(l.remove(1), Ok(c.clone()));
        assert_eq!(l.len(), 1);
        assert_eq!(*l.content.borrow(), vec![a.clone()]);

        assert_eq!(l.remove(0), Ok(a));
        assert_eq!(l.len(), 0);
        assert_eq!(*l.content.borrow(), vec![]);

        assert_eq!(l.remove(0), Err(IndexOutOfRangeError { len: 0 }));
    }

    #[test]
    fn test_display() {
        {
            let val = Value::List(List::new());
            let expected = "[]";
            assert_eq!(format!("{val}"), expected);
            assert_eq!(val.as_string().unwrap(), expected);
        };
        {
            let mut list = List::new();
            list.push(Value::Integer(1));
            list.push(Value::Nil);
            let list_a = List::new();
            list.push(list_a.into());
            let mut list_b = List::new();
            list_b.push("bar".to_value());
            list.push(list_b.into());

            let val = Value::List(list);
            let expected = r#"[1,nil,[],["bar"]]"#;

            assert_eq!(format!("{val}"), expected);
            assert_eq!(val.as_string().unwrap(), expected);
        }
    }
}
