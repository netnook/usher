use std::fmt::Display;

#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    Value(Value),
    List(List),
}

#[derive(PartialEq, Debug, Clone)]
pub struct List {
    expressions: Vec<AstNode>,
}

impl List {
    pub(crate) fn new(expressions: Vec<AstNode>) -> Self {
        Self { expressions }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Str(String),
    Integer(isize),
    Float(f64),
    Bool(bool),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Str(v) => write!(f, "{v}"),
            Value::Integer(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Bool(v) => match v {
                true => write!(f, "true"),
                false => write!(f, "false"),
            },
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::Value;

    #[test]
    fn test_value_display() {
        // strings
        assert_eq!(
            format!("{}", Value::Str("the-string".to_string())),
            "the-string"
        );
        assert_eq!(
            format!("{}", Value::Str("the-s\"tring".to_string())),
            "the-s\"tring"
        );

        // integers
        assert_eq!(format!("{}", Value::Integer(000)), "0");
        assert_eq!(format!("{}", Value::Integer(10000)), "10000");
        assert_eq!(format!("{}", Value::Integer(-10000)), "-10000");

        // floats
        assert_eq!(format!("{}", Value::Float(000.00)), "0");
        assert_eq!(format!("{}", Value::Float(10000.0)), "10000");
        assert_eq!(format!("{}", Value::Float(-10000.0)), "-10000");
        assert_eq!(format!("{}", Value::Float(10000.012340)), "10000.01234");
        assert_eq!(format!("{}", Value::Float(-10000.012340)), "-10000.01234");

        // bool
        assert_eq!(format!("{}", Value::Bool(true)), "true");
        assert_eq!(format!("{}", Value::Bool(false)), "false");

        // nil
        assert_eq!(format!("{}", Value::Nil), "nil");
    }
}
