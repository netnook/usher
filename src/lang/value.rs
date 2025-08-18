use super::{BuiltInFunc, FunctionDef, InternalProgramError, Span};
use std::borrow::Cow;

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Func(FunctionDef),
    BuiltInFunc(BuiltInFunc),
    Str(String),
    Integer(isize),
    Float(f64),
    Bool(bool),
    Nil,
}

impl Value {
    pub fn as_string(&self) -> Result<Cow<str>, InternalProgramError> {
        Ok(match self {
            Value::Func(_) => {
                return Err(InternalProgramError {
                    msg: "Cannot convert a function to a string.".to_string(),
                    // FIXME: correct pos
                    pos: Span::new(0, 0),
                });
            }
            Value::BuiltInFunc(_) => {
                return Err(InternalProgramError {
                    msg: "Cannot convert a function to a string.".to_string(),
                    // FIXME: correct pos
                    pos: Span::new(0, 0),
                });
            }
            Value::Str(v) => Cow::Borrowed(v),
            Value::Integer(v) => Cow::Owned(format!("{v}")),
            Value::Float(v) => Cow::Owned(format!("{v}")),
            Value::Bool(v) => match v {
                true => Cow::Borrowed("true"),
                false => Cow::Borrowed("false"),
            },
            Value::Nil => Cow::Borrowed("nil"),
        })
    }
}

#[cfg(test)]
mod tests {
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
    }
}
