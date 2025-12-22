use crate::lang::{
    Context, EvalStop, FunctionCall, InternalProgramError, Key, Value,
    function::{MethodResolver, MethodType},
};
use std::{
    cell::RefCell,
    collections::{
        HashMap,
        hash_map::{Iter, Keys},
    },
    fmt::{Display, Write},
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

    pub fn set(&mut self, key: Key, value: Value) -> Option<Value> {
        self.content.insert(key, value)
    }

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

#[derive(Debug, Clone, PartialEq, Default)]
pub struct DictIter {
    dict: DictCell,
    keys: Vec<Key>,
    next: usize,
}

impl DictIter {
    pub fn new(dict: DictCell) -> Self {
        // FIXME: terrible.  Switch to ordermap under the hood to that we don't have
        // to play around and sort keys for stability
        let mut keys: Vec<Key> = dict.borrow().keys().cloned().collect();
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

        let dict = self.dict.borrow();
        dict.get(key).map(|v| (key, v))
    }
}

impl MethodResolver for DictCell {
    fn resolve_method(&self, key: &Key) -> Option<MethodType<Self>> {
        match key.as_str() {
            "add" => Some(dict_add),
            "remove" => Some(dict_remove),
            _ => None,
        }
    }
}

fn dict_add(
    call: &FunctionCall,
    this: Rc<RefCell<Dict>>,
    ctxt: &mut Context,
) -> Result<Value, EvalStop> {
    // FIXME: have an arg spec as a struct and a macro which can "decode" the args into that struct ?
    let mut iter = call.args.iter();
    let val_arg = call.required_positional_arg("value", iter.next())?;
    let val_val = call.require_key_value("value", val_arg, ctxt)?;

    if let Some(arg) = iter.next() {
        return Err(
            InternalProgramError::FunctionCallUnexpectedArgument { span: arg.span() }.into(),
        );
    };

    let mut dict = this.borrow_mut();

    let old_value = dict.set(val_val.key.clone(), val_val.value.ref_clone());

    Ok(old_value.unwrap_or(Value::Nil))
}

fn dict_remove(
    call: &FunctionCall,
    this: Rc<RefCell<Dict>>,
    ctxt: &mut Context,
) -> Result<Value, EvalStop> {
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

    let mut dict = this.borrow_mut();

    let old_value = dict.remove(&(&key_val).into());

    Ok(old_value.unwrap_or(Value::Nil))
}

impl Display for Dict {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.write_str("dict(")?;

        // make the output sortable for testing
        let mut keys: Vec<&Key> = self.content.keys().collect();
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
            let val = Value::Dict(Dict::new().into());
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
            let val = Value::Dict(dict.into());
            assert_eq!(format!("{val}"), expected);
            assert_eq!(val.as_string().unwrap(), expected);
        }
    }
}
