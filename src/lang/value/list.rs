use crate::lang::{
    Context, EvalStop, FunctionCall, Key, Value,
    function::{MethodResolver, MethodType},
};
use std::{
    cell::RefCell,
    fmt::{Display, Write},
    rc::Rc,
};

pub type ListCell = Rc<RefCell<List>>;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct List {
    pub content: Vec<Value>,
}

impl List {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, index: usize) -> Option<Value> {
        self.content.get(index).cloned()
    }

    pub fn set(&mut self, index: usize, value: Value) {
        // FIXME: should there be an error if index out of range, or grow automatically
        self.content[index] = value;
    }

    pub fn add(&mut self, value: Value) {
        self.content.push(value);
    }

    pub fn len(&self) -> usize {
        self.content.len()
    }

    pub fn is_empty(&self) -> bool {
        self.content.is_empty()
    }

    // FIXME: why my own Iter implementation rather that collection::Iter ..
    pub fn iter<'a>(&'a self) -> ListIter<'a> {
        ListIter {
            list: self,
            next: 0,
        }
    }

    pub(crate) fn shallow_clone(&self) -> Self {
        let mut result = Self::new();
        for e in &self.content {
            result.add(e.ref_clone());
        }
        result
    }

    pub(crate) fn deep_clone(&self) -> Self {
        let mut result = Self::new();
        for e in &self.content {
            result.add(e.deep_clone());
        }
        result
    }
}

impl From<Vec<Value>> for List {
    fn from(value: Vec<Value>) -> Self {
        Self { content: value }
    }
}

impl From<List> for ListCell {
    fn from(value: List) -> Self {
        Rc::new(RefCell::new(value))
    }
}

impl Display for List {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.write_char('[')?;

        let mut first = true;
        for v in &self.content {
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

#[derive(Debug)]
pub struct ListIter<'a> {
    list: &'a List,
    next: usize,
}

impl<'a> Iterator for ListIter<'a> {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        if self.next >= self.list.len() {
            return Option::None;
        }

        let r = self.list.get(self.next);
        self.next += 1;
        Some(r.unwrap_or(Value::Nil))
    }
}

impl MethodResolver for ListCell {
    fn resolve_method(&self, key: &Key) -> Option<MethodType<Self>> {
        match key.as_str() {
            "add" => Some(list_add),
            _ => None,
        }
    }
}

fn list_add(
    call: &FunctionCall,
    this: Rc<RefCell<List>>,
    ctxt: &mut Context,
) -> Result<Value, EvalStop> {
    let mut list = this.borrow_mut();

    let args = call
        .args
        .iter()
        .map(|a| a.eval(ctxt))
        .collect::<Result<Vec<Value>, EvalStop>>()?;

    for arg in args {
        list.add(arg);
    }

    Ok(Value::Nil)
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
            l.add(42.into());
            l.add(43.into());
            let mut a = Value::List(l.into());
            let b = a.ref_clone();
            assert_eq!(a, b);

            let Value::List(l) = &mut a else { panic!() };
            l.borrow_mut().add(45.into());
            assert_eq!(a, b);
        }
        {
            let mut sub = List::new();
            sub.add(42.to_value());
            sub.add(43.to_value());
            let mut l = List::new();
            l.add(44.to_value());
            l.add(sub.into());
            let mut a = Value::List(l.into());
            let b = a.ref_clone();
            assert_eq!(a, b);

            let Value::List(l) = &mut a else { panic!() };
            let Value::List(sub) = &mut l.borrow().get(1).unwrap() else {
                panic!()
            };
            sub.borrow_mut().add(45.into());
            assert_eq!(a, b); // a and b still differ after sub list modified
        }
    }

    #[test]
    fn test_shallow_clone() {
        {
            let mut sub = List::new();
            sub.add(42.to_value());
            sub.add(43.to_value());
            let mut l = List::new();
            l.add(44.to_value());
            l.add(sub.into());
            let mut a = Value::List(l.into());
            let b = a.shallow_clone();
            assert_eq!(a, b);

            let Value::List(l) = &mut a else { panic!() };
            let Value::List(sub) = &mut l.borrow().get(1).unwrap() else {
                panic!()
            };
            sub.borrow_mut().add(45.into());
            assert_eq!(a, b); // a and b still same after sub list modified (shallow copy only)

            let Value::List(l) = &mut a else { panic!() };
            l.borrow_mut().add(46.into());
            assert_ne!(a, b); // and and b differ after top list modified
        }
    }

    #[test]
    fn test_deep_clone() {
        {
            let mut l = List::new();
            l.add(42.into());
            l.add(43.into());
            let mut a = Value::List(l.into());
            let b = a.deep_clone();
            assert_eq!(a, b);

            let Value::List(l) = &mut a else { panic!() };
            l.borrow_mut().add(45.into());
            assert_ne!(a, b);
        }
        {
            let mut sub = List::new();
            sub.add(42.to_value());
            sub.add(43.to_value());
            let mut l = List::new();
            l.add(44.to_value());
            l.add(sub.into());
            let mut a = Value::List(l.into());
            let b = a.deep_clone();
            assert_eq!(a, b);

            let Value::List(l) = &mut a else { panic!() };
            let Value::List(sub) = &mut l.borrow().get(1).unwrap() else {
                panic!()
            };
            sub.borrow_mut().add(45.into());
            assert_ne!(a, b);
        }
    }

    #[test]
    fn test_list() {
        let mut l = List::new();

        assert_eq!(l.get(0), None);
        assert_eq!(l.get(1), None);
        assert_eq!(l.len(), 0);

        l.add(42.to_value());
        assert_eq!(l.get(0), Some(42.to_value()));
        assert_eq!(l.get(1), None);
        assert_eq!(l.len(), 1);

        l.add("aaa".to_value());
        assert_eq!(l.get(0), Some(42.to_value()));
        assert_eq!(l.get(1), Some("aaa".to_value()));
        assert_eq!(l.len(), 2);
    }

    #[test]
    fn test_display() {
        {
            let val = Value::List(List::new().into());
            let expected = "[]";
            assert_eq!(format!("{val}"), expected);
            assert_eq!(val.as_string(), expected);
        };
        {
            let mut list = List::new();
            list.add(Value::Integer(1));
            list.add(Value::Nil);
            let list_a = List::new();
            list.add(list_a.into());
            let mut list_b = List::new();
            list_b.add("bar".to_value());
            list.add(list_b.into());

            let val = Value::List(list.into());
            let expected = r#"[1,nil,[],["bar"]]"#;

            assert_eq!(format!("{val}"), expected);
            assert_eq!(val.as_string(), expected);
        }
    }
}
