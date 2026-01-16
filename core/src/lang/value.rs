mod dict;
mod list;
mod string;

use super::InternalProgramError;
use crate::lang::{
    Key,
    function::{FunctionInst, FunctionType},
};
pub use dict::Dict;
pub use list::List;
use std::{
    fmt::{Display, Write},
    rc::Rc,
};
pub use string::StringCell;

#[derive(PartialEq, Debug, Clone, Eq)]
pub enum ValueType {
    Function,
    String,
    Integer,
    Float,
    Number,
    Boolean,
    List,
    Dict,
    KeyValue,
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
            ValueType::KeyValue => "key-value",
            ValueType::Nil => "nil",
            ValueType::Special => "special",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
#[allow(clippy::enum_variant_names)]
pub enum Value {
    Func(Func),
    Str(StringCell),
    Integer(isize),
    Float(f64),
    Bool(bool),
    List(List),
    Dict(Dict),
    KeyValue(KeyValueCell),
    Nil,
    End,
}

impl Value {
    pub(crate) fn as_string(&'_ self) -> Result<String, std::fmt::Error> {
        let mut s = String::new();
        self.write_string(&mut s)?;
        Ok(s)
    }

    pub(crate) fn write_string(&'_ self, into: &mut String) -> Result<(), std::fmt::Error> {
        match self {
            Value::Str(v) => write!(into, "{v}"),
            other => write!(into, "{other}"),
        }
    }
}

impl Display for Value {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Func(v) => Display::fmt(v, fmt),
            Value::Str(v) => {
                write!(fmt, "{:?}", v.as_str())?;
                Ok(())
            }
            Value::Integer(v) => Display::fmt(v, fmt),
            Value::Float(v) => Display::fmt(v, fmt),
            Value::Bool(v) => Display::fmt(v, fmt),
            Value::List(v) => Display::fmt(v, fmt),
            Value::Dict(v) => Display::fmt(v, fmt),
            Value::KeyValue(v) => Display::fmt(v.as_ref(), fmt),
            Value::Nil => fmt.write_str("nil"),
            Value::End => fmt.write_str("end"),
        }
    }
}

impl Value {
    // FIXME: deprecate
    pub fn ref_clone(&self) -> Self {
        match self {
            Value::Func(v) => Value::Func(v.clone()),
            Value::Str(v) => Value::Str(v.clone()),
            Value::Integer(v) => Value::Integer(*v),
            Value::Float(v) => Value::Float(*v),
            Value::Bool(v) => Value::Bool(*v),
            Value::List(v) => Value::List(v.clone()),
            Value::Dict(v) => Value::Dict(v.clone()),
            Value::KeyValue(v) => Value::KeyValue(Rc::clone(v)),
            Value::Nil => Value::Nil,
            Value::End => Value::End,
        }
    }

    pub fn shallow_clone(&self) -> Self {
        match self {
            Value::Func(v) => Value::Func(v.clone()),
            Value::Str(v) => Value::Str(v.shallow_clone()),
            Value::Integer(v) => Value::Integer(*v),
            Value::Float(v) => Value::Float(*v),
            Value::Bool(v) => Value::Bool(*v),
            Value::List(v) => Value::List(v.shallow_clone()),
            Value::Dict(v) => Value::Dict(v.shallow_clone()),
            Value::KeyValue(v) => Value::KeyValue(v.shallow_clone().into()),
            Value::Nil => Value::Nil,
            Value::End => Value::End,
        }
    }

    pub fn deep_clone(&self) -> Self {
        match self {
            Value::Func(v) => Value::Func(v.clone()),
            Value::Str(v) => Value::Str(v.deep_clone()),
            Value::Integer(v) => Value::Integer(*v),
            Value::Float(v) => Value::Float(*v),
            Value::Bool(v) => Value::Bool(*v),
            Value::List(v) => Value::List(v.deep_clone()),
            Value::Dict(v) => Value::Dict(v.deep_clone()),
            Value::KeyValue(v) => Value::KeyValue(v.deep_clone().into()),
            Value::Nil => Value::Nil,
            Value::End => Value::End,
        }
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
            Value::KeyValue(_) => ValueType::KeyValue,
            Value::Nil => ValueType::Nil,
            Value::End => ValueType::Special,
        }
    }
}

impl From<Dict> for Value {
    fn from(value: Dict) -> Self {
        Self::Dict(value)
    }
}

impl From<Vec<Value>> for Value {
    fn from(value: Vec<Value>) -> Self {
        Self::List(value.into())
    }
}

impl From<List> for Value {
    fn from(value: List) -> Self {
        Self::List(value)
    }
}

impl From<KeyValue> for Value {
    fn from(value: KeyValue) -> Self {
        Self::KeyValue(Rc::new(value))
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::Str(value.into())
    }
}
impl From<&Key> for Value {
    fn from(value: &Key) -> Self {
        Value::Str(value.into())
    }
}
impl From<Key> for Value {
    fn from(value: Key) -> Self {
        Value::Str(value.into())
    }
}

impl From<isize> for Value {
    fn from(value: isize) -> Self {
        Value::Integer(value)
    }
}
impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Nillable<T> {
    Nil,
    Some(T),
}

#[derive(Debug, Clone)]
pub enum Func {
    FuncInst(FunctionInst),
    BuiltIn(FunctionType),
}

impl PartialEq for Func {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl Display for Func {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Func::FuncInst(_) => fmt.write_str("<function>"),
            Func::BuiltIn(_) => fmt.write_str("<builtin-function>"),
        }
    }
}

pub type KeyValueCell = Rc<KeyValue>;

#[derive(Debug, Clone, PartialEq)]
pub struct KeyValue {
    pub key: Key,
    pub value: Value,
}

impl KeyValue {
    pub(crate) fn new(key: Key, value: Value) -> Self {
        Self { key, value }
    }

    pub(crate) fn shallow_clone(&self) -> Self {
        Self {
            key: self.key.clone(),
            value: self.value.ref_clone(),
        }
    }

    pub(crate) fn deep_clone(&self) -> Self {
        Self {
            key: self.key.clone(),
            value: self.value.deep_clone(),
        }
    }
}

impl Display for KeyValue {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.write_str("(\"")?;
        fmt.write_str(self.key.as_str())?;
        fmt.write_str("\":")?;
        self.value.fmt(fmt)?;
        fmt.write_char(')')?;
        Ok(())
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::lang::Value;
    use crate::parser::tests::ToValue;

    #[test]
    fn test_ref_clone() {
        {
            let a = Value::Str("string".into());
            let b = a.ref_clone();
            assert_eq!(a, b);
        }
        {
            let mut a = Value::Integer(42);
            let b = a.ref_clone();
            assert_eq!(a, b);

            let Value::Integer(v) = &mut a else { panic!() };
            *v = 24;
            assert_ne!(a, b);
        }
        {
            let mut a = Value::Float(42.1);
            let b = a.ref_clone();
            assert_eq!(a, b);

            let Value::Float(v) = &mut a else { panic!() };
            *v = 24.2;
            assert_ne!(a, b);
        }
        {
            let mut a = Value::Bool(true);
            let b = a.ref_clone();
            assert_eq!(a, b);

            let Value::Bool(v) = &mut a else { panic!() };
            *v = false;
            assert_ne!(a, b);
        }
        {
            let a = KeyValue::new("k".into(), "v".into());
            let a = Value::KeyValue(Rc::new(a));
            let b = a.ref_clone();
            assert_eq!(a, b);
        }
        {
            let a = Value::Nil;
            let b = a.ref_clone();
            assert_eq!(a, b);
        }
        {
            let a = Value::End;
            let b = a.ref_clone();
            assert_eq!(a, b);
        }
    }

    #[test]
    fn test_shallow_clone() {
        {
            let a = Value::Str("string".into());
            let b = a.shallow_clone();
            assert_eq!(a, b);
        }
        {
            let mut a = Value::Integer(42);
            let b = a.shallow_clone();
            assert_eq!(a, b);

            let Value::Integer(v) = &mut a else { panic!() };
            *v = 24;
            assert_ne!(a, b);
        }
        {
            let mut a = Value::Float(42.1);
            let b = a.shallow_clone();
            assert_eq!(a, b);

            let Value::Float(v) = &mut a else { panic!() };
            *v = 24.2;
            assert_ne!(a, b);
        }
        {
            let mut a = Value::Bool(true);
            let b = a.shallow_clone();
            assert_eq!(a, b);

            let Value::Bool(v) = &mut a else { panic!() };
            *v = false;
            assert_ne!(a, b);
        }
        {
            let a = KeyValue::new("k".into(), "v".into());
            let a = Value::KeyValue(Rc::new(a));
            let b = a.shallow_clone();
            assert_eq!(a, b);
        }
        {
            let a = Value::Nil;
            let b = a.shallow_clone();
            assert_eq!(a, b);
        }
        {
            let a = Value::End;
            let b = a.shallow_clone();
            assert_eq!(a, b);
        }
    }

    #[test]
    fn test_deep_clone() {
        {
            let a = Value::Str("string".into());
            let b = a.deep_clone();
            assert_eq!(a, b);
        }
        {
            let mut a = Value::Integer(42);
            let b = a.deep_clone();
            assert_eq!(a, b);

            let Value::Integer(v) = &mut a else { panic!() };
            *v = 24;
            assert_ne!(a, b);
        }
        {
            let mut a = Value::Float(42.1);
            let b = a.deep_clone();
            assert_eq!(a, b);

            let Value::Float(v) = &mut a else { panic!() };
            *v = 24.2;
            assert_ne!(a, b);
        }
        {
            let mut a = Value::Bool(true);
            let b = a.deep_clone();
            assert_eq!(a, b);

            let Value::Bool(v) = &mut a else { panic!() };
            *v = false;
            assert_ne!(a, b);
        }
        {
            let a = KeyValue::new("k".into(), "v".into());
            let a = Value::KeyValue(Rc::new(a));
            let b = a.deep_clone();
            assert_eq!(a, b);
        }
        {
            let a = Value::Nil;
            let b = a.deep_clone();
            assert_eq!(a, b);
        }
        {
            let a = Value::End;
            let b = a.deep_clone();
            assert_eq!(a, b);
        }
    }

    #[test]
    fn test_value_display() {
        // integers
        assert_eq!(format!("{}", 000.to_value()), "0");
        assert_eq!(format!("{}", 10000.to_value()), "10000");
        assert_eq!(format!("{}", (-10000).to_value()), "-10000");

        // floats
        assert_eq!(format!("{}", 000.00.to_value()), "0");
        assert_eq!(format!("{}", 10000.0.to_value()), "10000");
        assert_eq!(format!("{}", (-10000.0).to_value()), "-10000");
        assert_eq!(format!("{}", 10000.012340.to_value()), "10000.01234");
        assert_eq!(format!("{}", (-10000.012340).to_value()), "-10000.01234");

        // bool
        assert_eq!(format!("{}", true.to_value()), "true");
        assert_eq!(format!("{}", false.to_value()), "false");

        // nil
        assert_eq!(format!("{}", Value::Nil), "nil");

        // end
        assert_eq!(format!("{}", Value::End), "end");
    }
}
