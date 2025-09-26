use crate::lang::{Context, EvalStop};

use super::{BuiltInFunc, FunctionDef, InternalProgramError, Span};
use std::{
    borrow::Cow,
    cell::RefCell,
    collections::HashMap,
    fmt::{Display, Write},
    rc::Rc,
};

#[derive(PartialEq, Debug, Clone)]
pub enum ValueType {
    Function,
    String,
    Integer,
    Float,
    Number,
    Boolean,
    List,
    Dict,
    Nil,
    // FIXME: special ??? think of a better name
    Special,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.type_name())
    }
}

impl ValueType {
    pub fn type_name(&self) -> &'static str {
        match self {
            ValueType::Function => "function",
            ValueType::String => "string",
            ValueType::Integer => "integer",
            ValueType::Float => "float",
            ValueType::Number => "number",
            ValueType::Boolean => "boolean",
            ValueType::List => "list",
            ValueType::Dict => "dict",
            ValueType::Nil => "nil",
            ValueType::Special => "special",
        }
    }
}

#[derive(PartialEq, Clone)]
pub enum Value {
    Func(Func),
    Str(StringCell),
    Integer(isize),
    Float(f64),
    Bool(bool),
    List(ListCell),
    Dict(DictCell),
    Nil,
    End,
}

// FIXME: debug vs display vs .as_string -> merge into 1 if possible
impl core::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Func(_) => todo!(),
            Value::Str(v) => {
                f.write_char('"')?;
                f.write_str(v)?;
                f.write_char('"')?;
            }
            Value::Integer(v) => write!(f, "{v}")?,
            Value::Float(v) => write!(f, "{v}")?,
            Value::Bool(v) => write!(f, "{v}")?,
            Value::List(v) => {
                f.write_char('[')?;
                let mut first = true;
                for v in &v.borrow().content {
                    match first {
                        true => first = false,
                        false => f.write_char(',')?,
                    }
                    v.fmt(f)?;
                }
                f.write_char(']')?;
            }
            Value::Dict(v) => {
                f.write_str("dict(")?;

                // make the output sortable for testing
                let v = v.borrow();
                let mut keys: Vec<&String> = v.content.keys().collect();
                keys.sort();

                let mut first = true;
                for k in keys {
                    match first {
                        true => first = false,
                        false => f.write_char(',')?,
                    }
                    f.write_str(k)?;
                    f.write_char(':')?;
                    match v.get(k) {
                        Some(v) => v.fmt(f)?,
                        None => f.write_str("?none?")?,
                    }
                }

                f.write_char(')')?;
            }
            Value::Nil => f.write_str("nil")?,
            Value::End => f.write_str("end")?,
        }
        Ok(())
    }
}

impl Value {
    pub fn as_string(&'_ self) -> Result<Cow<'_, str>, EvalStop> {
        Ok(match self {
            Value::Func(_) => {
                let mut result = String::new();
                self.write_to(&mut result)?;
                Cow::Owned(result)
            }
            Value::Str(v) => Cow::Borrowed(v),
            Value::Integer(v) => Cow::Owned(format!("{v}")),
            Value::Float(v) => Cow::Owned(format!("{v}")),
            Value::Bool(v) => match v {
                true => Cow::Borrowed("true"),
                false => Cow::Borrowed("false"),
            },
            Value::List(_) => {
                let mut result = String::new();
                self.write_to(&mut result)?;
                Cow::Owned(result)
            }
            Value::Dict(_) => {
                let mut result = String::new();
                self.write_to(&mut result)?;
                Cow::Owned(result)
            }
            Value::Nil => Cow::Borrowed("nil"),
            Value::End => Cow::Borrowed("end"),
        })
    }

    fn write_to(&self, into: &mut String) -> Result<(), EvalStop> {
        match self {
            Value::Func(_) => {
                return InternalProgramError::CannotConvertToString {
                    typ: self.value_type(),
                    span: Span::new(0, 0), // FIXME: correct pos
                }
                .into();
            }
            Value::Str(v) => {
                // FIXME: in write_to, str is quoted.  In as_string above it is not.  Unify ?
                into.push('"');
                into.push_str(v);
                into.push('"');
            }
            Value::Integer(v) => into.push_str(&format!("{v}")),
            Value::Float(v) => into.push_str(&format!("{v}")),
            Value::Bool(v) => match v {
                true => into.push_str("true"),
                false => into.push_str("false"),
            },
            Value::List(v) => {
                into.push('[');

                let mut first = true;
                for v in &v.borrow().content {
                    if !first {
                        into.push(',');
                    } else {
                        first = false;
                    }
                    v.write_to(into)?;
                }

                into.push(']');
            }
            Value::Dict(v) => {
                into.push_str("dict(");

                let mut first = true;
                for (k, v) in &v.borrow().content {
                    if !first {
                        into.push(',');
                    } else {
                        first = false;
                    }
                    into.push('"');
                    into.push_str(k);
                    into.push('"');
                    into.push(':');
                    v.write_to(into)?;
                }

                into.push(')');
            }
            Value::Nil => into.push_str("nil"),
            Value::End => into.push_str("end"),
        }
        Ok(())
    }

    pub fn value_type(&self) -> ValueType {
        match self {
            Value::Func(_) => ValueType::Function,
            Value::Str(_) => ValueType::String,
            Value::Integer(_) => ValueType::Integer,
            Value::Float(_) => ValueType::Float,
            Value::Bool(_) => ValueType::Boolean,
            Value::List(_) => ValueType::List,
            Value::Dict(_) => ValueType::Dict,
            Value::Nil => ValueType::Nil,
            Value::End => ValueType::Special,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Func {
    Func(Rc<FunctionDef>),
    BuiltInFunc(BuiltInFunc),
}

impl Func {
    pub fn call(
        &self,
        ctxt: &mut Context,
        params: Vec<Value>,
        span: &Span,
    ) -> Result<Value, EvalStop> {
        match self {
            Func::Func(f) => f.call(ctxt, params, span),
            Func::BuiltInFunc(f) => f.call(ctxt, params, span),
        }
    }
}

impl From<BuiltInFunc> for Func {
    fn from(value: BuiltInFunc) -> Self {
        Self::BuiltInFunc(value)
    }
}

pub type StringCell = Rc<String>;

impl From<Dict> for DictCell {
    fn from(value: Dict) -> Self {
        Rc::new(RefCell::new(value))
    }
}

impl From<Dict> for Value {
    fn from(value: Dict) -> Self {
        Self::Dict(value.into())
    }
}

impl From<List> for ListCell {
    fn from(value: List) -> Self {
        Rc::new(RefCell::new(value))
    }
}

impl From<List> for Value {
    fn from(value: List) -> Self {
        Self::List(value.into())
    }
}

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
        // should there be an error if index out of range, or grow automatically
        self.content[index] = value;
    }

    pub fn add(&mut self, value: Value) {
        self.content.push(value);
    }

    pub fn len(&self) -> usize {
        self.content.len()
    }

    pub fn iter<'a>(&'a self) -> ListIter<'a> {
        ListIter {
            list: self,
            next: 0,
        }
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

pub type DictCell = Rc<RefCell<Dict>>;

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Dict {
    pub content: HashMap<String, Value>,
}

impl Dict {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&self, key: &str) -> Option<Value> {
        self.content.get(key).cloned()
    }

    pub fn set(&mut self, key: String, value: Value) {
        self.content.insert(key, value);
    }

    #[allow(dead_code)] // FIXME: remove later
    pub fn remove(&mut self, key: &str) -> Option<Value> {
        self.content.remove(key)
    }
}

impl From<BuiltInFunc> for Value {
    fn from(value: BuiltInFunc) -> Self {
        Self::Func(Func::BuiltInFunc(value))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::BuiltInFunc;
    use crate::lang::Value;
    use crate::parser::tests::ToValue;

    #[track_caller]
    fn do_test_as_string(val: Value, expected: &str) {
        assert_eq!(format!("{}", val.as_string().unwrap()), expected);
    }

    #[test]
    fn test_value_display() {
        // strings
        do_test_as_string("the-string".to_value(), "the-string");
        do_test_as_string("the-s\"tring".to_value(), "the-s\"tring");

        // integers
        do_test_as_string(000.to_value(), "0");
        do_test_as_string(10000.to_value(), "10000");
        do_test_as_string((-10000).to_value(), "-10000");

        // floats
        do_test_as_string(000.00.to_value(), "0");
        do_test_as_string(10000.0.to_value(), "10000");
        do_test_as_string((-10000.0).to_value(), "-10000");
        do_test_as_string(10000.012340.to_value(), "10000.01234");
        do_test_as_string((-10000.012340).to_value(), "-10000.01234");

        // bool
        do_test_as_string(true.to_value(), "true");
        do_test_as_string(false.to_value(), "false");

        // nil
        do_test_as_string(Value::Nil, "nil");

        // end
        do_test_as_string(Value::End, "end");

        assert!(
            Value::Func(Func::BuiltInFunc(BuiltInFunc::Print))
                .as_string()
                .is_err()
        );

        {
            let mut list = List::new();
            list.add(Value::Integer(1));
            list.add(Value::Nil);
            let list_a = List::new();
            list.add(list_a.into());
            let mut list_b = List::new();
            list_b.add("bar".to_value());
            list.add(list_b.into());

            let str = format!("{}", Value::List(list.into()).as_string().unwrap());
            assert_eq!(str, r#"[1,nil,[],["bar"]]"#);
        }
        do_test_as_string(Value::Dict(Dict::new().into()), "dict()");

        {
            let mut dict = Dict::new();
            dict.set("one".to_string(), Value::Integer(1));
            dict.set("nil".to_string(), Value::Nil);
            let dict_a = Dict::new();
            dict.set("dict_a".to_string(), dict_a.into());
            let mut dict_b = Dict::new();
            dict_b.set("foo".to_string(), "bar".to_value());
            dict.set("dict_b".to_string(), dict_b.into());

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
        do_test_as_string(Value::Dict(Dict::new().into()), "dict()");
    }

    #[test]
    fn test_dict() {
        let mut d = Dict::new();

        assert_eq!(d.get("foo"), None);

        d.set("foo".to_string(), 42.to_value());
        assert_eq!(d.get("foo"), Some(42.to_value()));

        d.set("foo".to_string(), "aaa".to_value());
        assert_eq!(d.get("foo"), Some("aaa".to_value()));

        assert_eq!(d.remove("foo"), Some("aaa".to_value()));
        assert_eq!(d.get("foo"), None);
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
}
