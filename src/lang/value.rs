mod dict;
mod list;
mod string;

use super::{FunctionDef, InternalProgramError, Span};
use crate::lang::{EvalStop, Key, function::FunctionType};
pub use dict::{Dict, DictCell};
pub use list::{List, ListCell};
use std::{
    borrow::Cow,
    cell::RefCell,
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

#[derive(PartialEq, Clone)]
#[allow(clippy::enum_variant_names)]
pub enum Value {
    Func(Func),
    Str(StringCell),
    Integer(isize),
    Float(f64),
    Bool(bool),
    List(ListCell),
    Dict(DictCell),
    KeyValue(KeyValueCell),
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
                let mut keys: Vec<&Key> = v.content.keys().collect();
                keys.sort();

                let mut first = true;
                for k in keys {
                    match first {
                        true => first = false,
                        false => f.write_char(',')?,
                    }
                    f.write_str(k.as_str())?;
                    f.write_char(':')?;
                    match v.get(k) {
                        Some(v) => v.fmt(f)?,
                        None => f.write_str("?none?")?,
                    }
                }

                f.write_char(')')?;
            }
            Value::KeyValue(kv) => {
                f.write_str("(")?;
                f.write_str(kv.key.as_str())?;
                f.write_char(':')?;
                kv.value.fmt(f)?;
                f.write_str(")")?;
            }
            Value::Nil => f.write_str("nil")?,
            Value::End => f.write_str("end")?,
        }
        Ok(())
    }
}

impl Value {
    pub fn ref_clone(&self) -> Self {
        match self {
            Value::Func(v) => Value::Func(v.clone()),
            Value::Str(v) => Value::Str(Rc::clone(v)),
            Value::Integer(v) => Value::Integer(*v),
            Value::Float(v) => Value::Float(*v),
            Value::Bool(v) => Value::Bool(*v),
            Value::List(v) => Value::List(Rc::clone(v)),
            Value::Dict(v) => Value::Dict(Rc::clone(v)),
            Value::KeyValue(v) => Value::KeyValue(Rc::clone(v)),
            Value::Nil => Value::Nil,
            Value::End => Value::End,
        }
    }

    pub fn shallow_clone(&self) -> Self {
        match self {
            Value::Func(v) => Value::Func(v.clone()),
            Value::Str(v) => Value::Str(Rc::new(v.as_str().to_string())),
            Value::Integer(v) => Value::Integer(*v),
            Value::Float(v) => Value::Float(*v),
            Value::Bool(v) => Value::Bool(*v),
            Value::List(v) => Value::List(v.borrow().shallow_clone().into()),
            Value::Dict(v) => Value::Dict(v.borrow().shallow_clone().into()),
            Value::KeyValue(v) => Value::KeyValue(v.shallow_clone().into()),
            Value::Nil => Value::Nil,
            Value::End => Value::End,
        }
    }

    pub fn deep_clone(&self) -> Self {
        match self {
            Value::Func(v) => Value::Func(v.clone()),
            Value::Str(v) => Value::Str(Rc::new(v.as_str().to_string())),
            Value::Integer(v) => Value::Integer(*v),
            Value::Float(v) => Value::Float(*v),
            Value::Bool(v) => Value::Bool(*v),
            Value::List(v) => Value::List(v.borrow().deep_clone().into()),
            Value::Dict(v) => Value::Dict(v.borrow().deep_clone().into()),
            Value::KeyValue(v) => Value::KeyValue(v.deep_clone().into()),
            Value::Nil => Value::Nil,
            Value::End => Value::End,
        }
    }

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
            Value::KeyValue(_) => {
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
                    into.push_str(k.as_str());
                    into.push('"');
                    into.push(':');
                    v.write_to(into)?;
                }

                into.push(')');
            }
            Value::KeyValue(v) => {
                into.push_str("(\"");
                into.push_str(v.key.as_str());
                into.push_str("\":");
                v.value.write_to(into)?;
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
            Value::KeyValue(_) => ValueType::KeyValue,
            Value::Nil => ValueType::Nil,
            Value::End => ValueType::Special,
        }
    }
}

impl From<Dict> for Value {
    fn from(value: Dict) -> Self {
        Self::Dict(value.into())
    }
}

impl From<Vec<Value>> for Value {
    fn from(value: Vec<Value>) -> Self {
        Self::List(Rc::new(RefCell::new(value.into())))
    }
}
impl From<List> for Value {
    fn from(value: List) -> Self {
        Self::List(value.into())
    }
}

impl From<KeyValue> for Value {
    fn from(value: KeyValue) -> Self {
        Self::KeyValue(Rc::new(value))
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::Str(Rc::new(value.to_string()))
    }
}

impl From<isize> for Value {
    fn from(value: isize) -> Self {
        Value::Integer(value)
    }
}

#[derive(Debug, Clone)]
pub enum Func {
    FuncDef(Rc<FunctionDef>),
    BuiltIn(FunctionType),
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::FuncDef(left), Self::FuncDef(right)) => left == right,
            _ => false,
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

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::lang::Value;
    use crate::lang::builtin_functions::resolve_function;
    use crate::parser::tests::{_block, _func, ToValue};

    #[test]
    fn test_ref_clone() {
        assert_eq!(
            Value::Func(Func::FuncDef(Rc::new(_func!(_block![])))).ref_clone(),
            Value::Func(Func::FuncDef(Rc::new(_func!(_block![]))))
        );

        {
            let a = Value::Str(Rc::new("string".to_string()));
            let b = a.ref_clone();
            assert_eq!(a, b);

            let Value::Str(mut v) = a else { panic!() };
            if Rc::get_mut(&mut v).is_some() {
                panic!("expected none");
            };
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
        assert_eq!(
            Value::Func(Func::FuncDef(Rc::new(_func!(_block![])))).shallow_clone(),
            Value::Func(Func::FuncDef(Rc::new(_func!(_block![]))))
        );

        {
            let a = Value::Str(Rc::new("string".to_string()));
            let b = a.shallow_clone();
            assert_eq!(a, b);

            let Value::Str(mut v) = a else { panic!() };
            Rc::get_mut(&mut v).unwrap().push_str("xxx");
            let a = Value::Str(v);
            assert_ne!(a, b);
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
        assert_eq!(
            Value::Func(Func::FuncDef(Rc::new(_func!(_block![])))).deep_clone(),
            Value::Func(Func::FuncDef(Rc::new(_func!(_block![]))))
        );

        {
            let a = Value::Str(Rc::new("string".to_string()));
            let b = a.deep_clone();
            assert_eq!(a, b);

            let Value::Str(mut v) = a else { panic!() };
            Rc::get_mut(&mut v).unwrap().push_str("xxx");
            let a = Value::Str(v);
            assert_ne!(a, b);
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
            Value::Func(Func::BuiltIn(resolve_function(&"print".into()).unwrap()))
                .as_string()
                .is_err()
        );
    }

    #[test]
    fn test_debug_str() {
        // strings
        do_test_debug_str("abc".to_value(), r#""abc""#);
        do_test_debug_str(123.to_value(), r#"123"#);
        do_test_debug_str(12.3.to_value(), r#"12.3"#);
        do_test_debug_str(true.to_value(), r#"true"#);
        do_test_debug_str(false.to_value(), r#"false"#);
        do_test_debug_str(Value::Nil, r#"nil"#);
        do_test_debug_str(Value::End, r#"end"#);
        do_test_debug_str(
            KeyValue::new("a".into(), 123.to_value()).to_value(),
            r#"(a:123)"#,
        );
    }

    #[track_caller]
    pub fn do_test_as_string(val: Value, expected: &str) {
        assert_eq!(format!("{}", val.as_string().unwrap()), expected);
    }

    #[track_caller]
    pub fn do_test_debug_str(val: impl Into<Value>, expected: &str) {
        let val = val.into();
        let actual = format!("{val:?}");
        assert_eq!(actual, expected);
    }
}
