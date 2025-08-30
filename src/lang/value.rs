use super::{BuiltInFunc, FunctionDef, InternalProgramError, Span};
use std::{borrow::Cow, collections::HashMap, fmt::Display};

#[derive(PartialEq, Debug, Clone)]
pub enum ValueType {
    Function,
    BuiltInFunction,
    String,
    Integer,
    Float,
    Boolean,
    Dict,
    Nil,
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
            ValueType::BuiltInFunction => "built-in-function",
            ValueType::String => "string",
            ValueType::Integer => "integer",
            ValueType::Float => "float",
            ValueType::Boolean => "boolean",
            ValueType::Dict => "dict",
            ValueType::Nil => "nil",
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Func(FunctionDef),
    BuiltInFunc(BuiltInFunc),
    Str(String),
    Integer(isize),
    Float(f64),
    Bool(bool),
    Dict(Dict),
    Nil,
}

impl Value {
    pub fn as_string(&'_ self) -> Result<Cow<'_, str>, InternalProgramError> {
        Ok(match self {
            Value::Func(_) => {
                let mut result = String::new();
                self.write_to(&mut result)?;
                Cow::Owned(result)
            }
            Value::BuiltInFunc(_) => {
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
            Value::Dict(_) => {
                let mut result = String::new();
                self.write_to(&mut result)?;
                Cow::Owned(result)
            }
            Value::Nil => Cow::Borrowed("nil"),
        })
    }

    fn write_to(&self, into: &mut String) -> Result<(), InternalProgramError> {
        match self {
            Value::Func(_) => {
                return Err(InternalProgramError {
                    msg: "Cannot convert a function to a string.".to_string(),
                    // FIXME: correct pos
                    span: Span::new(0, 0),
                });
            }
            Value::BuiltInFunc(_) => {
                return Err(InternalProgramError {
                    msg: "Cannot convert a function to a string.".to_string(),
                    // FIXME: correct pos
                    span: Span::new(0, 0),
                });
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
            Value::Dict(v) => {
                into.push_str("dict(");

                let mut first = true;
                for (k, v) in &v.content {
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
        }
        Ok(())
    }

    pub fn value_type(&self) -> ValueType {
        match self {
            Value::Func(_) => ValueType::Function,
            Value::BuiltInFunc(_) => ValueType::BuiltInFunction,
            Value::Str(_) => ValueType::String,
            Value::Integer(_) => ValueType::Integer,
            Value::Float(_) => ValueType::Float,
            Value::Bool(_) => ValueType::Boolean,
            Value::Dict(_) => ValueType::Dict,
            Value::Nil => ValueType::Nil,
        }
    }
}

impl From<Dict> for Value {
    fn from(value: Dict) -> Self {
        Self::Dict(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dict {
    pub content: HashMap<String, Value>,
}

impl Dict {
    pub fn new() -> Self {
        Self {
            content: HashMap::new(),
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lang::Block;
    use crate::lang::BuiltInFunc;
    use crate::lang::FunctionDef;
    use crate::lang::Value;

    #[track_caller]
    fn do_test_as_string(val: Value, expected: &str) {
        assert_eq!(format!("{}", val.as_string().unwrap()), expected);
    }

    #[test]
    fn test_value_display() {
        // strings
        do_test_as_string(Value::Str("the-string".to_string()), "the-string");
        do_test_as_string(Value::Str("the-s\"tring".to_string()), "the-s\"tring");

        // integers
        do_test_as_string(Value::Integer(000), "0");
        do_test_as_string(Value::Integer(10000), "10000");
        do_test_as_string(Value::Integer(-10000), "-10000");

        // floats
        do_test_as_string(Value::Float(000.00), "0");
        do_test_as_string(Value::Float(10000.0), "10000");
        do_test_as_string(Value::Float(-10000.0), "-10000");
        do_test_as_string(Value::Float(10000.012340), "10000.01234");
        do_test_as_string(Value::Float(-10000.012340), "-10000.01234");

        // bool
        do_test_as_string(Value::Bool(true), "true");
        do_test_as_string(Value::Bool(false), "false");

        // nil
        do_test_as_string(Value::Nil, "nil");

        assert!(Value::BuiltInFunc(BuiltInFunc::Print).as_string().is_err());
        assert!(
            Value::Func(FunctionDef {
                name: None,
                params: Vec::new(),
                body: Block { stmts: Vec::new() }
            })
            .as_string()
            .is_err()
        );

        {
            let mut dict = Dict::new();
            dict.set("one".to_string(), Value::Integer(1));
            dict.set("nil".to_string(), Value::Nil);
            let dict_a = Dict::new();
            dict.set("dict_a".to_string(), dict_a.into());
            let mut dict_b = Dict::new();
            dict_b.set("foo".to_string(), Value::Str("bar".to_string()));
            dict.set("dict_b".to_string(), dict_b.into());

            let str = format!("{}", Value::Dict(dict).as_string().unwrap());
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
        do_test_as_string(Value::Dict(Dict::new()), "dict()");
    }

    #[test]
    fn test_dict() {
        let mut d = Dict::new();

        assert_eq!(d.get("foo"), None);

        d.set("foo".to_string(), Value::Integer(42));
        assert_eq!(d.get("foo"), Some(Value::Integer(42)));

        d.set("foo".to_string(), Value::Str("aaa".to_string()));
        assert_eq!(d.get("foo"), Some(Value::Str("aaa".to_string())));

        assert_eq!(d.remove("foo"), Some(Value::Str("aaa".to_string())));
        assert_eq!(d.get("foo"), None);
    }
}
