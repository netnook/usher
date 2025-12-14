mod chars;
mod comment;
mod declaration;
mod dict;
pub mod error;
mod expression;
mod function;
mod identifier;
mod if_else;
mod list;
mod loops;
mod numbers;
pub(crate) mod position;
mod program;
mod return_stmt;
mod stmt;
mod string;
mod validation;

use crate::lang::Program;
use error::ParseError;
pub use error::SyntaxError;
pub use position::SourceRef;

pub(crate) const KEYWORDS: [&str; 28] = [
    "if", "else", "for", "in", "break", "continue", "return", "function", "var", "true", "false",
    "nil", "end", "dict", "switch", "case", "defer", "exit", "throw", "catch", "finally", "raise",
    "iif", "drop", "unlet", "let", "const", "debug",
];

pub(crate) const RESERVED_NAMES: [&str; 3] = ["print", "error", "std"];

pub fn parse<'a>(filename: &'a str, input: &'a str) -> Result<Program<'a>, ParseError<'a>> {
    let mut p = Parser::new(filename, input);

    let program = match p.program() {
        Ok(p) => p,
        Err(err) => {
            return Err(ParseError {
                file: filename,
                source: input,
                cause: err.into(),
            });
        }
    };

    // if let Err(err) = Validator::validate(&program) {
    //     return Err(ParseError {
    //         file: filename,
    //         source: input,
    //         cause: err.into(),
    //     });
    // }

    Ok(program)
}

type ParseResult<T> = Result<T, SyntaxError>;

#[derive(Debug)]
pub struct Parser<'a> {
    filename: &'a str,
    input_str: &'a str,
    input: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(filename: &'a str, input: &'a str) -> Self {
        Self {
            filename,
            input_str: input,
            input: input.as_bytes(),
            pos: 0,
        }
    }
}

#[cfg(test)]
pub mod tests {
    mod nested_types;
    mod spans;

    use std::{cell::RefCell, rc::Rc};

    use crate::{
        lang::{
            Assignment, AstNode, BinaryOp, BinaryOpCode, Block, Break,
            CatchMissingOptionalProperty, Continue, Declaration, Dict, DictBuilder, End, For,
            FunctionCall, FunctionDef, Identifier, IfElse, IndexOf, InterpolatedStr, KeyValue,
            KeyValueBuilder, List, ListBuilder, Literal, PropertyOf, ReturnStmt, Span, This,
            UnaryOp, UnaryOpCode, Value, Var,
        },
        parser::{Parser, SyntaxError},
    };
    use pretty_assertions::assert_eq;

    pub trait ToValue {
        fn to_value(self) -> Value;
    }

    impl ToValue for &str {
        fn to_value(self) -> Value {
            Value::Str(Rc::new(self.to_string()))
        }
    }
    impl ToValue for isize {
        fn to_value(self) -> Value {
            Value::Integer(self)
        }
    }
    impl ToValue for f64 {
        fn to_value(self) -> Value {
            Value::Float(self)
        }
    }
    impl ToValue for bool {
        fn to_value(self) -> Value {
            Value::Bool(self)
        }
    }
    impl ToValue for KeyValue {
        fn to_value(self) -> Value {
            Value::KeyValue(Rc::new(self))
        }
    }
    impl ToValue for List {
        fn to_value(self) -> Value {
            Value::List(Rc::new(RefCell::new(self)))
        }
    }
    impl ToValue for Dict {
        fn to_value(self) -> Value {
            Value::Dict(Rc::new(RefCell::new(self)))
        }
    }

    pub fn s(val: &str) -> Literal {
        Literal::new(val.to_value(), Span::new(999, 9999))
    }
    pub fn i(val: isize) -> Literal {
        Literal::new(Value::Integer(val), Span::new(999, 9999))
    }
    pub fn f(val: f64) -> Literal {
        Literal::new(Value::Float(val), Span::new(999, 9999))
    }
    pub fn b(val: bool) -> Literal {
        Literal::new(Value::Bool(val), Span::new(999, 9999))
    }
    pub fn nil() -> Literal {
        Literal::new(Value::Nil, Span::new(999, 9999))
    }
    macro_rules! dict{
        ($($key:expr => $value:expr),*) => {{
            use crate::lang::Dict;
            let mut d = Dict::new();
            $(
                d.set($key.into(), $value.to_value());
            )*
            d
        }};
    }
    pub(crate) use dict;

    pub(crate) fn this() -> This {
        This::new(Span::new(999, 9999))
    }
    pub fn id(val: &str) -> Identifier {
        Identifier::new(val.to_string(), Span::new(999, 9999))
    }
    pub fn var(id: impl Into<Identifier>) -> Var {
        Var::new(id.into())
    }

    pub(crate) fn kv(key: impl Into<Identifier>, value: impl Into<AstNode>) -> KeyValueBuilder {
        KeyValueBuilder {
            key: key.into(),
            value: value.into().into(),
        }
    }
    pub(crate) fn dict_builder(entries: Vec<KeyValueBuilder>) -> DictBuilder {
        DictBuilder {
            entries,
            span: Span::new(999, 9999),
        }
    }
    macro_rules! list{
        ($($entry:expr),*) => {{
            use crate::lang::ListBuilder;
            use crate::lang::Span;
            ListBuilder {
                entries: vec![$($entry.into()),*],
                span: Span::new(999, 9999),
            }
        }};
    }
    pub(crate) use list;

    pub(crate) fn prop_of(of: impl Into<AstNode>, prop: impl Into<Identifier>) -> PropertyOf {
        PropertyOf {
            of: of.into().into(),
            property: prop.into(),
            span: Span::new(999, 9999),
            optional_property: false,
        }
    }

    impl PropertyOf {
        pub(crate) fn with_optional_property(mut self, v: bool) -> PropertyOf {
            self.optional_property = v;
            self
        }
    }

    pub(crate) fn index_of(of: impl Into<AstNode>, index: impl Into<AstNode>) -> IndexOf {
        IndexOf {
            of: of.into().into(),
            index: index.into().into(),
            optional_property: false,
            span: Span::new(999, 9999),
        }
    }

    impl IndexOf {
        pub(crate) fn with_optional_property(mut self, v: bool) -> IndexOf {
            self.optional_property = v;
            self
        }
    }

    pub(crate) fn catch_missing_optional_property(from: impl Into<AstNode>) -> AstNode {
        AstNode::CatchMissingOptionalProperty(CatchMissingOptionalProperty {
            inner: from.into().into(),
        })
    }
    pub(crate) fn neg(on: impl Into<AstNode>) -> UnaryOp {
        UnaryOp {
            on: on.into().into(),
            op: UnaryOpCode::Negative,
            span: Span::new(999, 9999),
        }
    }
    pub(crate) fn not(on: impl Into<AstNode>) -> UnaryOp {
        UnaryOp {
            op: UnaryOpCode::Not,
            on: on.into().into(),
            span: Span::new(999, 9999),
        }
    }
    pub(crate) fn add(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> BinaryOp {
        binary(BinaryOpCode::Add, lhs, rhs)
    }
    pub(crate) fn sub(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> BinaryOp {
        binary(BinaryOpCode::Sub, lhs, rhs)
    }
    pub(crate) fn mul(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> BinaryOp {
        binary(BinaryOpCode::Mul, lhs, rhs)
    }
    pub(crate) fn div(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> BinaryOp {
        binary(BinaryOpCode::Div, lhs, rhs)
    }
    pub(crate) fn modulo(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> BinaryOp {
        binary(BinaryOpCode::Mod, lhs, rhs)
    }
    pub(crate) fn equal(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> BinaryOp {
        binary(BinaryOpCode::Equal, lhs, rhs)
    }
    pub(crate) fn not_equal(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> BinaryOp {
        binary(BinaryOpCode::NotEqual, lhs, rhs)
    }
    pub(crate) fn greater(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> BinaryOp {
        binary(BinaryOpCode::Greater, lhs, rhs)
    }
    pub(crate) fn greater_equal(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> BinaryOp {
        binary(BinaryOpCode::GreaterOrEqual, lhs, rhs)
    }
    pub(crate) fn less(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> BinaryOp {
        binary(BinaryOpCode::Less, lhs, rhs)
    }
    pub(crate) fn less_equal(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> BinaryOp {
        binary(BinaryOpCode::LessOrEqual, lhs, rhs)
    }
    pub(crate) fn and(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> BinaryOp {
        binary(BinaryOpCode::And, lhs, rhs)
    }
    pub(crate) fn or(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> BinaryOp {
        binary(BinaryOpCode::Or, lhs, rhs)
    }
    pub(crate) fn binary(
        op: BinaryOpCode,
        lhs: impl Into<AstNode>,
        rhs: impl Into<AstNode>,
    ) -> BinaryOp {
        BinaryOp {
            op,
            lhs: lhs.into().into(),
            rhs: rhs.into().into(),
        }
    }

    pub(crate) fn decl(var: Var, value: impl Into<AstNode>) -> Declaration {
        Declaration {
            var,
            value: value.into().into(),
            span: Span::new(999, 9999),
        }
    }
    pub(crate) fn assign(lhs: impl Into<AstNode>, rhs: impl Into<AstNode>) -> Assignment {
        Assignment {
            lhs: lhs.into().into(),
            rhs: rhs.into().into(),
        }
    }
    pub(crate) fn _for(
        loop_item: Var,
        loop_info: Option<Var>,
        iterable: impl Into<AstNode>,
        block: Block,
    ) -> For {
        For {
            loop_item,
            loop_info,
            iterable: iterable.into().into(),
            block,
            span: Span::new(999, 9999),
        }
    }
    pub(crate) fn _continue() -> Continue {
        Continue::new(Span::new(999, 9999))
    }
    pub(crate) fn _end() -> End {
        End::new(Span::new(999, 9999))
    }

    macro_rules! brk {
        ($val:expr) => {{
            use crate::lang::Break;
            use crate::lang::Span;
            Break {
                value: Some(Box::new($val.into())),
                span: Span::new(999, 9999),
            }
        }};
        () => {{
            use crate::lang::Break;
            use crate::lang::Span;
            Break {
                value: None,
                span: Span::new(999, 9999),
            }
        }};
    }
    pub(crate) use brk;

    macro_rules! ret {
        ($val:expr) => {{
            use crate::lang::ReturnStmt;
            use crate::lang::Span;
            ReturnStmt {
                value: Some(Box::new($val.into())),
                span: Span::new(999, 9999),
            }
        }};
        () => {{
            use crate::lang::ReturnStmt;
            use crate::lang::Span;
            ReturnStmt {
                value: None,
                span: Span::new(999, 9999),
            }
        }};
    }
    pub(crate) use ret;

    macro_rules! _if {
        ($(cond($cond:expr => $block:expr)),+) => {{
            use crate::lang::ConditionalBlock;
            use crate::lang::IfElse;
            use crate::lang::Span;
            let conditional_blocks = vec![
                $(
                    ConditionalBlock {
                        condition: $cond.into(),
                        block: $block.into(),
                    }
                ),+
            ];
            IfElse {
                conditional_blocks,
                else_block: None,
                span: Span::new(999,9999),
            }
        }};
        ($(cond($cond:expr => $block:expr)),+ , else($else_block:expr)) => {{
            use crate::lang::IfElse;
            use crate::lang::Span;
            let stmt =_if!($(cond($cond => $block)),*);
            IfElse {
                conditional_blocks : stmt.conditional_blocks ,
                else_block: $else_block.into(),
                span: Span::new(999,9999),
            }
        }};
    }
    pub(crate) use _if;

    macro_rules! _block{
        ($($stmt:expr),*) => {{
            use crate::lang::Block;
            use crate::lang::Span;
            Block{
                stmts: vec![$($stmt.into()),*],
                span: Span::new(999, 9999),
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
            InterpolatedStr{parts, span: Span::new(999,9999)}
        }};
    }
    pub(crate) use _interp;

    macro_rules! _prog{
        ($($stmt:expr),+) => {{
            use crate::lang::Program;
            Program{
                file: "",
                source: "",
                stmts:vec![$($stmt.into()),+]
            }
        }};
    }
    pub(crate) use _prog;

    macro_rules! _func {
        (name($name:expr), $($rest:tt)*) => {{
            let f = _func!($($rest)*);
            FunctionDef {
                name: Some(var($name)),
                ..f
            }
        }};
        (param($($pp:tt)*), $($rest:tt)*) => {{
            let mut f = _func!($($rest)*);
            f.params.insert(0, _func!(@param $($pp)*));
            f
        }};
        (@param $name:expr, $val:expr) => {{
            use crate::lang::Param;
            Param::Optional(var($name), $val.into())
        }};
        (@param $name:expr) => {{
            use crate::lang::Param;
            Param::Required(var($name))
        }};
        ($body:expr) => {{
            use crate::lang::FunctionDef;
            use crate::lang::Span;
            FunctionDef {
                name: None,
                params: Vec::new(),
                body: $body,
                span: Span::new(999, 9999),
            }
        }};
    }
    pub(crate) use _func;

    macro_rules! _function_call{
        ($function:expr, $($arg:expr),*) => {{
            use crate::lang::FunctionCall;
            use crate::lang::FunctionCallVariant;
            FunctionCall {
                variant: FunctionCallVariant::FunctionCall {
                    function: $function.into(),
                },
                args: vec![$($arg),*],
                span: Span::new(999, 9999),
            }
        }};
    }
    pub(crate) use _function_call;

    macro_rules! _method_call{
        ($on:expr, $function:expr, $($arg:expr),*) => {{
            use crate::lang::FunctionCall;
            use crate::lang::FunctionCallVariant;
            FunctionCall {
                variant: FunctionCallVariant::MethodCall {
                    on: Box::new($on.into()),
                    function: $function.into(),
                },
                args: vec![$($arg),*],
                span: Span::new(999, 9999),
            }
        }};
    }
    pub(crate) use _method_call;

    macro_rules! _anon_call{
        ($on:expr, $($arg:expr),*) => {{
            use crate::lang::FunctionCall;
            use crate::lang::FunctionCallVariant;
            FunctionCall {
                variant: FunctionCallVariant::AnonymousCall {
                    on: Box::new($on.into()),
                },
                args: vec![$($arg),*],
                span: Span::new(999, 9999),
            }
        }};
    }
    pub(crate) use _anon_call;

    macro_rules! arg {
        ($name:expr, $value:expr) => {{
            use crate::lang::Arg;
            Arg {
                name: Some($name.into()),
                value: $value.into(),
            }
        }};
        ($value:expr) => {{
            use crate::lang::Arg;
            Arg {
                name: None,
                value: $value.into(),
            }
        }};
    }
    pub(crate) use arg;

    #[track_caller]
    pub(crate) fn do_test_parser_ok<'a, F>(
        func: F,
        input: &'static str,
        expected: Option<AstNode>,
        expected_end: isize,
    ) where
        F: FnOnce(&mut Parser<'a>) -> Result<Option<AstNode>, SyntaxError>,
    {
        let mut parser = Parser::new("dummy", input);
        parser.pos = 1;

        let actual = func(&mut parser)
            .expect("parser should succeed")
            .map(|a| format!("{a:-#?}"));
        let expected = expected.map(|e| format!("{e:-#?}"));

        match (actual, expected) {
            (Some(actual), Some(expected)) => {
                assert_eq!(actual, expected, "assert actual (left) == expected (right)");
            }
            (actual, expected) => {
                assert_eq!(actual, expected, "assert actual (left) == expected (right)");
            }
        };

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
    pub(crate) fn do_test_parser_ok_t<'a, F, T>(
        func: F,
        input: &'static str,
        expected: Option<T>,
        expected_end: isize,
    ) where
        F: FnOnce(&mut Parser<'a>) -> Result<Option<T>, SyntaxError>,
        T: PartialEq<T> + std::fmt::Debug,
    {
        let mut parser = Parser::new("dummy", input);
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
    pub(crate) fn do_test_parser_some<'a, F>(
        func: F,
        input: &'static str,
        expected: AstNode,
        expected_end: isize,
    ) where
        F: FnOnce(&mut Parser<'a>) -> Result<Option<AstNode>, SyntaxError>,
    {
        do_test_parser_ok(func, input, Some(expected), expected_end);
    }

    #[track_caller]
    pub(crate) fn do_test_parser_some_t<'a, F, T>(
        func: F,
        input: &'static str,
        expected: T,
        expected_end: isize,
    ) where
        F: FnOnce(&mut Parser<'a>) -> Result<Option<T>, SyntaxError>,
        T: PartialEq<T> + std::fmt::Debug,
    {
        do_test_parser_ok_t(func, input, Some(expected), expected_end);
    }

    #[track_caller]
    pub(crate) fn do_test_parser_none<'a, F>(func: F, input: &'static str)
    where
        F: FnOnce(&mut Parser<'a>) -> Result<Option<AstNode>, SyntaxError>,
    {
        do_test_parser_ok(func, input, None, 1);
    }

    #[track_caller]
    pub(crate) fn do_test_parser_none_t<'a, F, T>(func: F, input: &'static str)
    where
        F: FnOnce(&mut Parser<'a>) -> Result<Option<T>, SyntaxError>,
        T: PartialEq<T> + std::fmt::Debug,
    {
        do_test_parser_ok_t(func, input, None, 1);
    }

    #[track_caller]
    pub(crate) fn do_test_parser_err<'a, F, T>(
        func: F,
        input: &'static str,
        expected_err: SyntaxError,
    ) where
        F: FnOnce(&mut Parser<'a>) -> Result<Option<T>, SyntaxError>,
        T: PartialEq<T> + std::fmt::Debug,
    {
        let mut parser = Parser::new("dummy", input);
        parser.pos = 1;

        let actual = func(&mut parser).expect_err("parser should error");

        assert_eq!(
            actual, expected_err,
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
        let mut parser = Parser::new("dummy", input);
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

    impl From<&str> for Identifier {
        fn from(value: &str) -> Self {
            Identifier::new(value.to_string(), Span::new(999, 9999))
        }
    }

    macro_rules! with_span {
        ($type:ident) => {
            impl $type {
                pub fn spanned(mut self, start: usize, len: usize) -> Self {
                    self.span = Span::new(start, len);
                    self
                }
            }
        };
    }

    with_span!(UnaryOp);
    with_span!(Literal);
    with_span!(Identifier);
    with_span!(DictBuilder);
    with_span!(PropertyOf);
    with_span!(ListBuilder);
    with_span!(IndexOf);
    with_span!(Block);
    with_span!(FunctionCall);
    with_span!(Break);
    with_span!(Continue);
    with_span!(End);
    with_span!(This);
    with_span!(InterpolatedStr);
    with_span!(IfElse);
    with_span!(For);
    with_span!(Declaration);
    with_span!(ReturnStmt);
    with_span!(FunctionDef);
}
