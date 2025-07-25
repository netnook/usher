mod bool;
mod chars;
mod comment;
mod declaration_stmt;
mod error;
mod expression;
mod for_stmt;
mod identifier;
mod if_stmt;
mod list;
mod nil;
mod numbers;
mod object;
mod stmt;
mod string;
mod this;
mod utils;

use error::{ParseError, build_parse_error};

// FIXME: add all necessary keywords
pub(crate) const RESERVED_KEYWORDS: [&str; 9] = [
    "print", "if", "else", "for", "in", "var", "true", "false", "nil",
];

pub fn parse(input: &str) -> Result<(), ParseError> {
    let mut p = Parser::new(input);

    match p.program() {
        Ok(p) => p,
        Err(se) => return Err(build_parse_error(input, se)),
    };

    Ok(())
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct SyntaxError {
    pos: usize,
    msg: &'static str,
}

impl SyntaxError {
    fn new(pos: usize, msg: &'static str) -> Self {
        Self { pos, msg }
    }
}

type ParseResult<T> = Result<T, SyntaxError>;

#[derive(Debug)]
pub struct Parser<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    fn program(&mut self) -> ParseResult<()> {
        // // FIXME dummy code
        self.stmts().unwrap();

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    mod nested_types;

    use super::SyntaxError;
    use crate::{
        lang::{AstNode, BinaryOp, Block, DeclarationStmt, ForStmt, Identifier, UnaryOp, Value},
        parser::Parser,
    };
    use pretty_assertions::assert_eq;

    pub(crate) fn s(v: &str) -> Value {
        Value::Str(v.to_string())
    }
    pub(crate) fn i(v: isize) -> Value {
        Value::Integer(v)
    }
    pub(crate) fn b(v: bool) -> Value {
        Value::Bool(v)
    }
    pub(crate) fn nil() -> Value {
        Value::Nil
    }
    pub(crate) fn this() -> AstNode {
        AstNode::This
    }
    pub(crate) fn id(s: &str) -> Identifier {
        Identifier::new(s)
    }
    pub(crate) fn prop_of(from: impl Into<AstNode>, prop: &str) -> AstNode {
        AstNode::PropertyOf {
            from: from.into().into(),
            property: id(prop),
        }
    }
    pub(crate) fn index_of(from: impl Into<AstNode>, index: impl Into<AstNode>) -> AstNode {
        AstNode::IndexOf {
            from: from.into().into(),
            index: index.into().into(),
        }
    }
    pub(crate) fn chain_catch(from: impl Into<AstNode>) -> AstNode {
        AstNode::ChainCatch(from.into().into())
    }
    pub(crate) fn neg(on: impl Into<AstNode>) -> AstNode {
        AstNode::UnaryOp {
            on: on.into().into(),
            op: UnaryOp::Negative,
        }
    }
    pub(crate) fn not(on: impl Into<AstNode>) -> AstNode {
        AstNode::UnaryOp {
            op: UnaryOp::Not,
            on: on.into().into(),
        }
    }
    pub(crate) fn add(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOp::Add, lhs, rhs)
    }
    pub(crate) fn sub(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOp::Sub, lhs, rhs)
    }
    pub(crate) fn mul(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOp::Mul, lhs, rhs)
    }
    pub(crate) fn div(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOp::Div, lhs, rhs)
    }
    pub(crate) fn modulo(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOp::Mod, lhs, rhs)
    }
    pub(crate) fn equal(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOp::Equal, lhs, rhs)
    }
    pub(crate) fn not_equal(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOp::NotEqual, lhs, rhs)
    }
    pub(crate) fn greater(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOp::Greater, lhs, rhs)
    }
    pub(crate) fn greater_equal(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOp::GreaterOrEqual, lhs, rhs)
    }
    pub(crate) fn less(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOp::Less, lhs, rhs)
    }
    pub(crate) fn less_equal(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOp::LessOrEqual, lhs, rhs)
    }
    pub(crate) fn and(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOp::And, lhs, rhs)
    }
    pub(crate) fn or(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOp::Or, lhs, rhs)
    }
    pub(crate) fn binary(
        op: BinaryOp,
        lhs: impl Into<AstNode>,
        rhs: impl Into<AstNode>,
    ) -> AstNode {
        AstNode::BinaryOp {
            op,
            lhs: lhs.into().into(),
            rhs: rhs.into().into(),
        }
    }

    pub(crate) fn var(ident: Identifier, value: impl Into<AstNode>) -> DeclarationStmt {
        DeclarationStmt {
            ident,
            value: value.into().into(),
        }
    }
    pub(crate) fn assign(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        AstNode::Assignment(lhs.into().into(), rhs.into().into())
    }

    pub(crate) fn _for(
        ident1: Identifier,
        ident2: Option<Identifier>,
        expr: impl Into<AstNode>,
        block: Block,
    ) -> ForStmt {
        ForStmt {
            loop_var_1: ident1,
            loop_var_2: ident2,
            loop_expr: expr.into().into(),
            block,
        }
    }
    macro_rules! _if {
        ($(_cond($cond:expr, $block:expr)),+) => {{
            let conditional_blocks = vec![
                $(
                    ConditionalBlock {
                        condition: $cond.into(),
                        block: $block.into(),
                    }
                ),+
            ];
            IfElseStmt{
                conditional_blocks,
                else_block: None,
            }
        }};
        ($(_cond($cond:expr, $block:expr)),+ , _else($else_block:expr)) => {{
            let stmt =_if!($(_cond($cond, $block)),*);
            IfElseStmt {
                conditional_blocks : stmt.conditional_blocks ,
                else_block: $else_block.into(),
            }
        }};
    }
    pub(crate) use _if;

    macro_rules! _block{
        ($($stmt:expr),+) => {{
            use crate::lang::Block;
            Block{
                stmts:vec![$($stmt.into()),+]
            }
        }};
    }
    pub(crate) use _block;

    #[track_caller]
    pub(crate) fn do_test_parser_ok<'a, F, T>(
        func: F,
        input: &'static str,
        expected: Option<T>,
        expected_end: isize,
    ) where
        F: FnOnce(&mut Parser<'a>) -> Result<Option<T>, SyntaxError>,
        T: PartialEq<T> + std::fmt::Debug,
    {
        let mut parser = Parser::new(input);
        parser.pos = 1;

        let actual = func(&mut parser).expect("parser should succeed");

        assert_eq!(actual, expected, "assert actual (left) == expected (right)");

        if expected_end > 0 {
            assert_eq!(
                parser.pos, expected_end as usize,
                "assert actual_end ({}) == expected_end ({expected_end})",
                parser.pos
            );
        } else {
            let actual_remain = input.len() - parser.pos;
            let expected_remain = -expected_end as usize;
            assert_eq!(
                actual_remain, expected_remain,
                "assert actual_remain ({actual_remain}) == expected_remaining ({expected_remain})"
            );
        }
    }

    #[track_caller]
    pub(crate) fn do_test_parser_some<'a, F, T>(
        func: F,
        input: &'static str,
        expected: T,
        expected_end: isize,
    ) where
        F: FnOnce(&mut Parser<'a>) -> Result<Option<T>, SyntaxError>,
        T: PartialEq<T> + std::fmt::Debug,
    {
        do_test_parser_ok(func, input, Some(expected), expected_end);
    }

    #[track_caller]
    pub(crate) fn do_test_parser_none<'a, F, T>(func: F, input: &'static str)
    where
        F: FnOnce(&mut Parser<'a>) -> Result<Option<T>, SyntaxError>,
        T: PartialEq<T> + std::fmt::Debug,
    {
        do_test_parser_ok(func, input, None, 1);
    }

    #[track_caller]
    pub(crate) fn do_test_parser_err<'a, F, T>(
        func: F,
        input: &'static str,
        expected_err_pos: usize,
        expected_err_msg: &'static str,
    ) where
        F: FnOnce(&mut Parser<'a>) -> Result<Option<T>, SyntaxError>,
        T: PartialEq<T> + std::fmt::Debug,
    {
        let mut parser = Parser::new(input);
        parser.pos = 1;

        let actual = func(&mut parser).expect_err("parser should error");

        assert_eq!(
            actual,
            SyntaxError {
                pos: expected_err_pos,
                msg: expected_err_msg
            },
            "assert actual (left) == expected (right)"
        );
    }

    #[track_caller]
    pub(crate) fn do_test_opt_parser<'a, F, T>(
        func: F,
        input: &'static str,
        expected: Option<T>,
        expected_remain: usize,
    ) where
        F: FnOnce(&mut Parser<'a>) -> Option<T>,
        T: PartialEq<T> + std::fmt::Debug,
    {
        let mut parser = Parser::new(input);
        parser.pos = 1;

        let actual = func(&mut parser);

        assert_eq!(actual, expected, "assert actual (left) == expected (right)");

        let actual_remain = input.len() - parser.pos;
        assert_eq!(
            actual_remain, expected_remain,
            "assert actual_remain ({actual_remain}) == expected_remaining ({expected_remain})"
        );
    }

    #[track_caller]
    pub(crate) fn do_test_opt_parser_some<'a, F, T>(
        func: F,
        input: &'static str,
        expected: T,
        expected_remain: usize,
    ) where
        F: FnOnce(&mut Parser<'a>) -> Option<T>,
        T: PartialEq<T> + std::fmt::Debug,
    {
        do_test_opt_parser(func, input, Some(expected), expected_remain);
    }

    #[track_caller]
    pub(crate) fn do_test_opt_parser_none<'a, F, T>(func: F, input: &'static str)
    where
        F: FnOnce(&mut Parser<'a>) -> Option<T>,
        T: PartialEq<T> + std::fmt::Debug,
    {
        do_test_opt_parser(func, input, None, input.len() - 1);
    }
}
