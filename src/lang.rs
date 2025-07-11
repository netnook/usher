use std::fmt::Display;

#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    Identifier(Identifier),
    Value(Value),
    ListBuilder(ListBuilder),
    ObjectBuilder(ObjectBuilder),
}

impl From<Identifier> for AstNode {
    fn from(value: Identifier) -> Self {
        Self::Identifier(value)
    }
}

impl From<Value> for AstNode {
    fn from(value: Value) -> Self {
        Self::Value(value)
    }
}

impl From<ListBuilder> for AstNode {
    fn from(value: ListBuilder) -> Self {
        Self::ListBuilder(value)
    }
}

impl From<ObjectBuilder> for AstNode {
    fn from(value: ObjectBuilder) -> Self {
        Self::ObjectBuilder(value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    name: String,
}

impl Identifier {
    pub(crate) fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ListBuilder {
    entries: Vec<AstNode>,
}

impl ListBuilder {
    pub(crate) fn new(entries: Vec<AstNode>) -> Self {
        Self { entries }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ObjectBuilder {
    entries: Vec<(AstNode, AstNode)>,
}

impl ObjectBuilder {
    pub(crate) fn new(entries: Vec<(AstNode, AstNode)>) -> Self {
        Self { entries }
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
