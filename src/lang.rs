use std::fmt::Display;

#[derive(PartialEq, Debug, Clone)]
pub enum AstNode {
    This,
    Identifier(Identifier),
    Value(Value),
    ListBuilder(ListBuilder),
    ObjectBuilder(ObjectBuilder),
    PropertyOf {
        from: Box<AstNode>,
        property: Identifier,
    },
    IndexOf {
        from: Box<AstNode>,
        index: Box<AstNode>,
    },
    UnaryOp {
        op: UnaryOp,
        on: Box<AstNode>,
    },
    BinaryOp {
        op: BinaryOp,
        lhs: Box<AstNode>,
        rhs: Box<AstNode>,
    },
    ChainCatch(Box<AstNode>),
    IfElseStmt(IfElseStmt),
    ForStmt(ForStmt),
    DeclarationStmt(DeclarationStmt),
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

impl From<IfElseStmt> for AstNode {
    fn from(value: IfElseStmt) -> Self {
        Self::IfElseStmt(value)
    }
}
impl From<ForStmt> for AstNode {
    fn from(value: ForStmt) -> Self {
        Self::ForStmt(value)
    }
}
impl From<DeclarationStmt> for AstNode {
    fn from(value: DeclarationStmt) -> Self {
        Self::DeclarationStmt(value)
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

#[derive(PartialEq, Debug, Clone)]
pub enum UnaryOp {
    Not,
    Negative,
}

#[derive(PartialEq, Debug, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    LessOrEqual,
    Less,
    And,
    Or,
}

#[derive(PartialEq, Debug, Clone)]
pub struct IfElseStmt {
    pub conditional_blocks: Vec<ConditionalBlock>,
    pub else_block: Option<Block>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ConditionalBlock {
    pub condition: AstNode,
    pub block: Block,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ForStmt {
    pub loop_var_1: Identifier,
    pub loop_var_2: Option<Identifier>,
    pub loop_expr: Box<AstNode>,
    pub block: Block,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Block {
    pub stmts: Vec<AstNode>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct DeclarationStmt {
    pub ident: Identifier,
    pub value: Box<AstNode>,
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
