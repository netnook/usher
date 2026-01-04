use crate::lang::{
    Context, EvalStop, FunctionCall, InternalProgramError, Key, Value,
    function::{MethodResolver, MethodType},
};
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Display, Write},
    rc::Rc,
};

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Dict {
    content: Rc<RefCell<HashMap<Key, Value>>>,
}

impl Dict {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, key: &Key) -> Option<Value> {
        self.content.borrow().get(key).cloned()
    }

    pub fn set(&mut self, key: Key, value: Value) -> Option<Value> {
        self.content.borrow_mut().insert(key, value)
    }

    pub fn remove(&mut self, key: &Key) -> Option<Value> {
        self.content.borrow_mut().remove(key)
    }

    pub(crate) fn keys(&'_ self) -> Vec<Key> {
        self.content.borrow().keys().cloned().collect()
    }

    pub(crate) fn shallow_clone(&self) -> Self {
        self.content.borrow().clone().into()
    }

    pub(crate) fn deep_clone(&self) -> Self {
        let mut result = HashMap::new();
        let content = self.content.borrow();
        for (k, v) in &*content {
            result.insert(k.clone(), v.deep_clone());
        }
        result.into()
    }
}

impl From<HashMap<Key, Value>> for Dict {
    fn from(value: HashMap<Key, Value>) -> Self {
        Self {
            content: Rc::new(RefCell::new(value)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct DictIter {
    dict: Dict,
    keys: Vec<Key>,
    next: usize,
}

impl DictIter {
    pub fn new(dict: Dict) -> Self {
        // FIXME: terrible.  Switch to ordermap under the hood to that we don't have
        // to play around and sort keys for stability
        let mut keys: Vec<Key> = dict.content.borrow().keys().cloned().collect();
        keys.sort();
        Self {
            dict,
            keys,
            next: 0,
        }
    }

    pub fn next(&mut self) -> Option<(&Key, Value)> {
        let key = self.keys.get(self.next);
        let key = match key {
            Some(k) => {
                self.next += 1;
                k
            }
            None => return None,
        };

        self.dict.get(key).map(|v| (key, v))
    }
}

impl MethodResolver for Dict {
    fn resolve_method(&self, key: &Key) -> Option<MethodType<Self>> {
        match key.as_str() {
            "add" => Some(dict_add),
            "remove" => Some(dict_remove),
            _ => None,
        }
    }
}

fn dict_add(call: &FunctionCall, mut this: Dict, ctxt: &mut Context) -> Result<Value, EvalStop> {
    // FIXME: have an arg spec as a struct and a macro which can "decode" the args into that struct ?
    let mut iter = call.args.iter();
    let val_arg = call.required_positional_arg("value", iter.next())?;
    let val_val = call.require_key_value("value", val_arg, ctxt)?;

    if let Some(arg) = iter.next() {
        return Err(
            InternalProgramError::FunctionCallUnexpectedArgument { span: arg.span() }.into(),
        );
    };

    let old_value = this.set(val_val.key.clone(), val_val.value.ref_clone());

    Ok(old_value.unwrap_or(Value::Nil))
}

fn dict_remove(call: &FunctionCall, mut this: Dict, ctxt: &mut Context) -> Result<Value, EvalStop> {
    // FIXME: have an arg spec as a struct and a macro which can "decode" the args into that struct ?
    let mut iter = call.args.iter();
    let key_arg = call.required_positional_arg("key", iter.next())?;
    // FIXME: key could be integer
    let key_val = call.require_string("key", key_arg, ctxt)?;

    if let Some(arg) = iter.next() {
        return Err(
            InternalProgramError::FunctionCallUnexpectedArgument { span: arg.span() }.into(),
        );
    };

    let old_value = this.remove(&(&key_val).into());

    Ok(old_value.unwrap_or(Value::Nil))
}

impl Display for Dict {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.write_str("dict(")?;

        // make the output sortable for testing
        let content = self.content.borrow();
        let mut keys: Vec<&Key> = content.keys().collect();
        keys.sort();

        let mut first = true;
        for k in keys {
            match first {
                true => first = false,
                false => fmt.write_char(',')?,
            }
            fmt.write_char('"')?;
            fmt.write_str(k.as_str())?;
            fmt.write_str("\":")?;

            match self.get(k) {
                Some(v) => Display::fmt(&v, fmt)?,
                None => fmt.write_str("?none?")?,
            }
        }

        fmt.write_char(')')?;

        Ok(())
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
            let mut d = Dict::new();
            d.set("a".into(), 42.into());
            d.set("b".into(), 43.into());
            let mut a = Value::Dict(d);
            let b = a.ref_clone();
            assert_eq!(a, b);

            let Value::Dict(l) = &mut a else { panic!() };
            l.set("c".into(), 45.into());
            assert_eq!(a, b);
        }
        {
            let mut sub = Dict::new();
            sub.set("a".into(), 42.to_value());
            sub.set("b".into(), 43.to_value());
            let mut l = Dict::new();
            l.set("c".into(), 44.to_value());
            l.set("d".into(), sub.into());
            let mut a = Value::Dict(l);
            let b = a.ref_clone();
            assert_eq!(a, b);

            let Value::Dict(l) = &mut a else { panic!() };
            let Value::Dict(sub) = &mut l.get(&"d".into()).unwrap() else {
                panic!()
            };
            sub.set("e".into(), 45.into());
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
            let mut a = Value::Dict(l);
            let b = a.shallow_clone();
            assert_eq!(a, b);

            let Value::Dict(l) = &mut a else { panic!() };
            let Value::Dict(sub) = &mut l.get(&"d".into()).unwrap() else {
                panic!()
            };
            sub.set("e".into(), 45.into());
            assert_eq!(a, b); // a and b still differ after sub list modified

            let Value::Dict(l) = &mut a else { panic!() };
            l.set("c".into(), 45.into());
            assert_ne!(a, b);
        }
    }

    #[test]
    fn test_deep_clone() {
        {
            let mut d = Dict::new();
            d.set("a".into(), 42.into());
            d.set("b".into(), 43.into());
            let mut a = Value::Dict(d);
            let b = a.deep_clone();
            assert_eq!(a, b);

            let Value::Dict(l) = &mut a else { panic!() };
            l.set("c".into(), 45.into());
            assert_ne!(a, b);
        }
        {
            let mut sub = Dict::new();
            sub.set("a".into(), 42.to_value());
            sub.set("b".into(), 43.to_value());
            let mut l = Dict::new();
            l.set("c".into(), 44.to_value());
            l.set("d".into(), sub.into());
            let mut a = Value::Dict(l);
            let b = a.deep_clone();
            assert_eq!(a, b);

            let Value::Dict(l) = &mut a else { panic!() };
            let Value::Dict(sub) = &mut l.get(&"d".into()).unwrap() else {
                panic!()
            };
            sub.set("e".into(), 45.into());
            assert_ne!(a, b);
        }
    }

    #[test]
    fn test_dict() {
        let mut d = Dict::new();

        let key = "foo".into();

        assert_eq!(d.get(&key), None);

        assert_eq!(d.set(key.clone(), 42.to_value()), None);
        assert_eq!(d.get(&key), Some(42.to_value()));

        assert_eq!(d.set(key.clone(), "aaa".to_value()), Some(42.to_value()));
        assert_eq!(d.get(&key), Some("aaa".to_value()));

        assert_eq!(d.remove(&key), Some("aaa".to_value()));
        assert_eq!(d.get(&key), None);
    }

    #[test]
    fn test_display() {
        {
            let val = Value::Dict(Dict::new());
            let expected = "dict()";
            assert_eq!(format!("{val}"), expected);
            assert_eq!(val.as_string().unwrap(), expected);
        };
        {
            let mut dict = Dict::new();
            dict.set("one".into(), Value::Integer(1));
            dict.set("nil".into(), Value::Nil);
            let dict_a = Dict::new();
            dict.set("dict_a".into(), dict_a.into());
            let mut dict_b = Dict::new();
            dict_b.set("foo".into(), "bar".to_value());
            dict.set("dict_b".into(), dict_b.into());

            let expected = r#"dict("dict_a":dict(),"dict_b":dict("foo":"bar"),"nil":nil,"one":1)"#;
            let val = Value::Dict(dict);
            assert_eq!(format!("{val}"), expected);
            assert_eq!(val.as_string().unwrap(), expected);
        }
    }
}
