mod chars;
mod comment;
mod declaration_stmt;
mod dict;
mod error;
mod expression;
mod for_stmt;
mod function;
mod identifier;
mod if_stmt;
mod list;
mod numbers;
mod program;
mod return_stmt;
mod stmt;
mod string;
mod utils;

use crate::lang::Program;
use error::{ParseError, build_parse_error};

// FIXME: add all necessary keywords
pub(crate) const RESERVED_KEYWORDS: [&str; 19] = [
    "print", "if", "else", "for", "in", "break", "continue", "return", "function", "var", "true",
    "false", "nil", "end", "dict", "switch", "case", "defer", "error",
];

pub fn parse(input: &str) -> Result<Program, ParseError> {
    let mut p = Parser::new(input);

    let p = match p.program() {
        Ok(p) => p,
        Err(se) => return Err(build_parse_error(input, se)),
    };

    Ok(p)
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
}

#[cfg(test)]
mod tests {
    mod nested_types;
    mod printing;
    mod programs;

    use super::SyntaxError;
    use crate::{
        lang::{
            Assignment, AstNode, BinaryOp, BinaryOpCode, Block, ChainCatch, Declaration, ForStmt,
            Identifier, IndexOf, KeyValue, PropertyOf, UnaryOp, UnaryOpCode, Value,
        },
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
        Identifier::new(s.to_string())
    }
    pub(crate) fn kv(key: impl Into<AstNode>, value: impl Into<AstNode>) -> KeyValue {
        KeyValue {
            key: key.into().into(),
            value: value.into().into(),
        }
    }
    pub(crate) fn prop_of(from: impl Into<AstNode>, prop: &str) -> AstNode {
        AstNode::PropertyOf(PropertyOf {
            from: from.into().into(),
            property: id(prop),
        })
    }
    pub(crate) fn index_of(from: impl Into<AstNode>, index: impl Into<AstNode>) -> AstNode {
        AstNode::IndexOf(IndexOf {
            from: from.into().into(),
            index: index.into().into(),
        })
    }
    pub(crate) fn chain_catch(from: impl Into<AstNode>) -> AstNode {
        AstNode::ChainCatch(ChainCatch {
            inner: from.into().into(),
        })
    }
    pub(crate) fn neg(on: impl Into<AstNode>) -> AstNode {
        AstNode::UnaryOp(UnaryOp {
            on: on.into().into(),
            op: UnaryOpCode::Negative,
        })
    }
    pub(crate) fn not(on: impl Into<AstNode>) -> AstNode {
        AstNode::UnaryOp(UnaryOp {
            op: UnaryOpCode::Not,
            on: on.into().into(),
        })
    }
    pub(crate) fn add(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOpCode::Add, lhs, rhs)
    }
    pub(crate) fn sub(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOpCode::Sub, lhs, rhs)
    }
    pub(crate) fn mul(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOpCode::Mul, lhs, rhs)
    }
    pub(crate) fn div(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOpCode::Div, lhs, rhs)
    }
    pub(crate) fn modulo(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOpCode::Mod, lhs, rhs)
    }
    pub(crate) fn equal(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOpCode::Equal, lhs, rhs)
    }
    pub(crate) fn not_equal(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOpCode::NotEqual, lhs, rhs)
    }
    pub(crate) fn greater(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOpCode::Greater, lhs, rhs)
    }
    pub(crate) fn greater_equal(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOpCode::GreaterOrEqual, lhs, rhs)
    }
    pub(crate) fn less(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOpCode::Less, lhs, rhs)
    }
    pub(crate) fn less_equal(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOpCode::LessOrEqual, lhs, rhs)
    }
    pub(crate) fn and(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOpCode::And, lhs, rhs)
    }
    pub(crate) fn or(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        binary(BinaryOpCode::Or, lhs, rhs)
    }
    pub(crate) fn binary(
        op: BinaryOpCode,
        lhs: impl Into<AstNode>,
        rhs: impl Into<AstNode>,
    ) -> AstNode {
        AstNode::BinaryOp(BinaryOp {
            op,
            lhs: lhs.into().into(),
            rhs: rhs.into().into(),
        })
    }

    pub(crate) fn var(ident: Identifier, value: impl Into<AstNode>) -> Declaration {
        Declaration {
            ident,
            value: value.into().into(),
        }
    }
    pub(crate) fn assign(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> AstNode {
        AstNode::Assignment(Assignment {
            lhs: lhs.into().into(),
            rhs: rhs.into().into(),
        })
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
    // pub(crate) fn ret(value: Option<impl Into<AstNode>>) -> AstNode {
    //     AstNode::ReturnStmt(ReturnStmt {
    //         value: value.map(|v| v.into().into()),
    //     })
    // }
    macro_rules! _ret {
        ($val:expr) => {{
            // use crate::lang::ConditionalBlock;
            // use crate::lang::ConditionalBlock;
            // use crate::lang::IfElseStmt;
            // let conditional_blocks = vec![
            //     $(
            //         ConditionalBlock {
            //             condition: $cond.into(),
            //             block: $block.into(),
            //         }
            //     ),+
            // ];
            AstNode::ReturnStmt(ReturnStmt {
                value: Some(Box::new($val.into())),
            })
        }};
        () => {{ AstNode::ReturnStmt(ReturnStmt { value: None }) }};
    }
    pub(crate) use _ret;

    macro_rules! _if {
        ($(_cond($cond:expr, $block:expr)),+) => {{
            use crate::lang::ConditionalBlock;
            use crate::lang::IfElseStmt;
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
        ($($stmt:expr),*) => {{
            use crate::lang::Block;
            Block{
                stmts:vec![$($stmt.into()),*]
            }
        }};
    }
    pub(crate) use _block;

    macro_rules! _interp {
        ($($v:expr),+) => {{
            use crate::lang::InterpolatedStr;
            let parts= vec![
                $($v.into()),+
            ];
            AstNode::InterpolatedStr(InterpolatedStr{parts})
        }};
    }
    pub(crate) use _interp;

    macro_rules! _prog{
        ($($stmt:expr),+) => {{
            use crate::lang::Program;
            Program{
                stmts:vec![$($stmt.into()),+]
            }
        }};
    }
    pub(crate) use _prog;

    macro_rules! _func {
        (p[$($param:expr),*], $body:expr) => {{
            use crate::lang::FunctionDef;
            use crate::lang::Identifier;
            let params = vec![
                $(Identifier::new($param.to_string())),*
            ];
            FunctionDef {
                name: None,
                params,
                body: $body,
            }
        }};
        ($body:expr) => {{
            use crate::lang::FunctionDef;
            FunctionDef {
                name: None,
                params: Vec::new(),
                body: $body,
            }
        }};
    }
    pub(crate) use _func;

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
