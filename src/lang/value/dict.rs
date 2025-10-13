use crate::lang::{
    Key, Value,
    function::{MethodResolver, MethodType},
};
use std::{
    cell::RefCell,
    collections::{
        HashMap,
        hash_map::{Iter, Keys},
    },
    rc::Rc,
};

pub type DictCell = Rc<RefCell<Dict>>;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Dict {
    pub content: HashMap<Key, Value>,
}

impl Dict {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, key: &Key) -> Option<Value> {
        self.content.get(key).cloned()
    }

    pub fn set(&mut self, key: Key, value: Value) {
        self.content.insert(key, value);
    }

    #[allow(dead_code)] // FIXME: remove later
    pub fn remove(&mut self, key: &Key) -> Option<Value> {
        self.content.remove(key)
    }

    pub fn iter(&self) -> Iter<'_, Key, Value> {
        self.content.iter()
    }

    pub fn keys(&'_ self) -> Keys<'_, Key, Value> {
        self.content.keys()
    }

    pub(crate) fn shallow_clone(&self) -> Self {
        let mut result = Self::new();
        for (k, v) in &self.content {
            result.set(k.clone(), v.ref_clone());
        }
        result
    }

    pub(crate) fn deep_clone(&self) -> Self {
        let mut result = Self::new();
        for (k, v) in &self.content {
            result.set(k.clone(), v.deep_clone());
        }
        result
    }
}

impl From<Dict> for DictCell {
    fn from(value: Dict) -> Self {
        Rc::new(RefCell::new(value))
    }
}

impl MethodResolver for DictCell {
    fn resolve_method(&self, _key: &Key) -> Option<MethodType<Self>> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::Value;
    use crate::lang::value::list::List;
    use crate::lang::value::tests::{do_test_as_string, do_test_debug_str};
    use crate::parser::tests::ToValue;

    #[test]
    fn test_ref_clone() {
        {
            let mut d = Dict::new();
            d.set("a".into(), 42.into());
            d.set("b".into(), 43.into());
            let mut a = Value::Dict(d.into());
            let b = a.ref_clone();
            assert_eq!(a, b);

            let Value::Dict(l) = &mut a else { panic!() };
            l.borrow_mut().set("c".into(), 45.into());
            assert_eq!(a, b);
        }
        {
            let mut sub = Dict::new();
            sub.set("a".into(), 42.to_value());
            sub.set("b".into(), 43.to_value());
            let mut l = Dict::new();
            l.set("c".into(), 44.to_value());
            l.set("d".into(), sub.into());
            let mut a = Value::Dict(l.into());
            let b = a.ref_clone();
            assert_eq!(a, b);

            let Value::Dict(l) = &mut a else { panic!() };
            let Value::Dict(sub) = &mut l.borrow().get(&"d".into()).unwrap() else {
                panic!()
            };
            sub.borrow_mut().set("e".into(), 45.into());
            assert_eq!(a, b); // a and b still differ after sub list modified
        }
    }

    #[test]
    fn test_shallow_clone() {
        {
            let mut sub = Dict::new();
            sub.set("a".into(), 42.to_value());
            sub.set("b".into(), 43.to_value());
            let mut l = Dict::new();
            l.set("c".into(), 44.to_value());
            l.set("d".into(), sub.into());
            let mut a = Value::Dict(l.into());
            let b = a.shallow_clone();
            assert_eq!(a, b);

            let Value::Dict(l) = &mut a else { panic!() };
            let Value::Dict(sub) = &mut l.borrow().get(&"d".into()).unwrap() else {
                panic!()
            };
            sub.borrow_mut().set("e".into(), 45.into());
            assert_eq!(a, b); // a and b still differ after sub list modified

            let Value::Dict(l) = &mut a else { panic!() };
            l.borrow_mut().set("c".into(), 45.into());
            assert_ne!(a, b);
        }
    }

    #[test]
    fn test_deep_clone() {
        {
            let mut d = Dict::new();
            d.set("a".into(), 42.into());
            d.set("b".into(), 43.into());
            let mut a = Value::Dict(d.into());
            let b = a.deep_clone();
            assert_eq!(a, b);

            let Value::Dict(l) = &mut a else { panic!() };
            l.borrow_mut().set("c".into(), 45.into());
            assert_ne!(a, b);
        }
        {
            let mut sub = Dict::new();
            sub.set("a".into(), 42.to_value());
            sub.set("b".into(), 43.to_value());
            let mut l = Dict::new();
            l.set("c".into(), 44.to_value());
            l.set("d".into(), sub.into());
            let mut a = Value::Dict(l.into());
            let b = a.deep_clone();
            assert_eq!(a, b);

            let Value::Dict(l) = &mut a else { panic!() };
            let Value::Dict(sub) = &mut l.borrow().get(&"d".into()).unwrap() else {
                panic!()
            };
            sub.borrow_mut().set("e".into(), 45.into());
            assert_ne!(a, b);
        }
    }

    #[test]
    fn test_value_display() {
        do_test_as_string(Value::Dict(Dict::new().into()), "dict()");
        {
            let mut dict = Dict::new();
            dict.set("one".into(), Value::Integer(1));
            dict.set("nil".into(), Value::Nil);
            let dict_a = Dict::new();
            dict.set("dict_a".into(), dict_a.into());
            let mut dict_b = Dict::new();
            dict_b.set("foo".into(), "bar".to_value());
            dict.set("dict_b".into(), dict_b.into());

            let str = format!("{}", Value::Dict(dict.into()).as_string().unwrap());
            assert!(str.starts_with("dict("));
            assert!(str.ends_with(")"));
            assert!(str.contains(r#""one":1"#));
            assert!(str.contains(r#""nil":nil"#));
            assert!(str.contains(r#""dict_a":dict()"#));
            assert!(str.contains(r#""dict_b":dict("foo":"bar")"#));
            assert_eq!(str.chars().filter(|c| *c == ',').count(), 3);
            assert_eq!(
                str.len(),
                r#"dict("one":1,"nil":nil,"dict_a":dict(),"dict_b":dict("foo":"bar"))"#.len()
            );
        }
    }

    #[test]
    fn test_dict() {
        let mut d = Dict::new();

        let key = "foo".into();

        assert_eq!(d.get(&key), None);

        d.set(key.clone(), 42.to_value());
        assert_eq!(d.get(&key), Some(42.to_value()));

        d.set(key.clone(), "aaa".to_value());
        assert_eq!(d.get(&key), Some("aaa".to_value()));

        assert_eq!(d.remove(&key), Some("aaa".to_value()));
        assert_eq!(d.get(&key), None);
    }

    #[test]
    fn test_debug_str() {
        {
            let mut dict = Dict::new();
            dict.set("one".into(), 1.to_value());
            dict.set("two".into(), Value::Nil);
            do_test_debug_str(dict.to_value(), r#"dict(one:1,two:nil)"#);
        }
        {
            let mut dict = Dict::new();
            dict.set("one".into(), Value::Integer(1));
            dict.set("nil".into(), Value::Nil);
            let dict_a = Dict::new();
            dict.set("dict_a".into(), dict_a.into());
            let mut dict_b = Dict::new();
            dict_b.set("foo".into(), "bar".to_value());
            dict.set("dict_b".into(), dict_b.into());
            let mut list_a = List::new();
            list_a.add(42.to_value());
            list_a.add(43.to_value());
            dict.set("list_a".into(), list_a.into());
            do_test_debug_str(
                dict.to_value(),
                r#"dict(dict_a:dict(),dict_b:dict(foo:"bar"),list_a:[42,43],nil:nil,one:1)"#,
            );
        }
    }
}
