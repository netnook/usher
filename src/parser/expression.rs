use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{
    Arg, AstNode, BinaryOp, BinaryOpCode, ChainCatch, FunctionCall, Identifier, IndexOf, KeyValue,
    PropertyOf, UnaryOp, UnaryOpCode, Value,
};

pub(crate) const EXPECTED_EXPRESSION: &str = "Expected expression.";
pub(crate) const EXPECTED_COSING_PARENS: &str = "Expected closing parenthesis ')'";
pub(crate) const EXPECTED_COSING_BRACKET: &str = "Expected closing bracket ']'";
pub(crate) const EXPECTED_IDENTIFIER: &str = "Expected identifier.";
pub(crate) const EXPECTED_IDENT_ON_KV_LHS: &str = "Expected identifier on LHS of key:value pair.";
pub(crate) const MISSING_CALL_CLOSE: &str =
    "Expected closing parenthesis ')' after function call arguments.";
pub(crate) const EXPECTED_CALL_ARG_OR_CLOSE: &str =
    "Expected function call argument or closing parenthesis ')'.";
pub(crate) const EXPECTED_CALL_COMMA_OR_CLOSE: &str =
    "Expected comma or closing parenthesis ')' for function call arguments.";

impl<'a> Parser<'a> {
    /// Consume an expression or nothing.
    pub(super) fn expression(&mut self) -> ParseResult<Option<AstNode>> {
        if let Some(v) = self.key_value_expression()? {
            return Ok(Some(v));
        }

        Ok(None)
    }

    // Key value expression
    fn key_value_expression(&mut self) -> ParseResult<Option<AstNode>> {
        let start = self.pos;

        let Some(lhs) = self.logical_expression()? else {
            return Ok(None);
        };

        let savepos = self.pos;

        self.linespace();

        if !self.char(b':') {
            self.pos = savepos;
            return Ok(Some(lhs));
        }

        let AstNode::Identifier(id) = lhs else {
            return Err(SyntaxError {
                pos: start,
                msg: EXPECTED_IDENT_ON_KV_LHS,
            });
        };

        self.whitespace_comments();

        let Some(rhs) = self.logical_expression()? else {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_EXPRESSION,
            });
        };

        Ok(Some(AstNode::KeyValue(KeyValue {
            key: id,
            value: rhs.into(),
        })))
    }

    // Logical and (&&), or (||) expression
    fn logical_expression(&mut self) -> ParseResult<Option<AstNode>> {
        self.infix_expression(Self::logical_op, Self::comparison_expression)
    }

    fn logical_op(&mut self) -> Option<BinaryOpCode> {
        if self.tag(b"&&") {
            Some(BinaryOpCode::And)
        } else if self.tag(b"||") {
            Some(BinaryOpCode::Or)
        } else {
            None
        }
    }

    // Addition/subtraction expression
    fn comparison_expression(&mut self) -> ParseResult<Option<AstNode>> {
        let Some(mut lhs) = self.addsub_expression()? else {
            return Ok(None);
        };

        let savepoint = self.pos;

        self.linespace();

        let op = if self.tag(b"==") {
            BinaryOpCode::Equal
        } else if self.tag(b"!=") {
            BinaryOpCode::NotEqual
        } else if self.tag(b">=") {
            BinaryOpCode::GreaterOrEqual
        } else if self.tag(b"<=") {
            BinaryOpCode::LessOrEqual
        } else if self.char(b'>') {
            BinaryOpCode::Greater
        } else if self.char(b'<') {
            BinaryOpCode::Less
        } else {
            self.pos = savepoint;
            return Ok(Some(lhs));
        };

        self.whitespace_comments();

        let Some(rhs) = self.addsub_expression()? else {
            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_EXPRESSION,
            });
        };

        lhs = AstNode::BinaryOp(BinaryOp {
            op,
            lhs: lhs.into(),
            rhs: rhs.into(),
        });

        Ok(Some(lhs))
    }

    // Addition/subtraction expression
    fn addsub_expression(&mut self) -> ParseResult<Option<AstNode>> {
        self.infix_expression(Self::addsub_op, Self::muldiv_expression)
    }

    fn addsub_op(&mut self) -> Option<BinaryOpCode> {
        if self.char(b'+') {
            Some(BinaryOpCode::Add)
        } else if self.char(b'-') {
            Some(BinaryOpCode::Sub)
        } else {
            None
        }
    }

    // Multiplication/division/modulo expression
    fn muldiv_expression(&mut self) -> ParseResult<Option<AstNode>> {
        self.infix_expression(Self::muldiv_op, Self::prefix_expression)
    }

    fn muldiv_op(&mut self) -> Option<BinaryOpCode> {
        if self.char(b'*') {
            Some(BinaryOpCode::Mul)
        } else if self.char(b'/') {
            Some(BinaryOpCode::Div)
        } else if self.char(b'%') {
            Some(BinaryOpCode::Mod)
        } else {
            None
        }
    }

    fn infix_expression<O, N>(
        &mut self,
        mut operator_fn: O,
        mut next_fn: N,
    ) -> ParseResult<Option<AstNode>>
    where
        O: FnMut(&mut Self) -> Option<BinaryOpCode>,
        N: FnMut(&mut Self) -> ParseResult<Option<AstNode>>,
    {
        let Some(mut lhs) = next_fn(self)? else {
            return Ok(None);
        };

        self.pos = loop {
            let savepoint = self.pos;

            self.linespace();

            let Some(op) = operator_fn(self) else {
                break savepoint;
            };

            self.whitespace_comments();

            let Some(rhs) = next_fn(self)? else {
                return Err(SyntaxError {
                    pos: self.pos,
                    msg: EXPECTED_EXPRESSION,
                });
            };

            lhs = AstNode::BinaryOp(BinaryOp {
                op,
                lhs: lhs.into(),
                rhs: rhs.into(),
            })
        };

        Ok(Some(lhs))
    }

    // Prefix expression
    fn prefix_expression(&mut self) -> ParseResult<Option<AstNode>> {
        let start = self.pos;

        let mut prefixes = Vec::new();

        loop {
            if self.char(b'!') {
                prefixes.push(UnaryOpCode::Not);
                continue;
            };
            if self.char(b'-') {
                prefixes.push(UnaryOpCode::Negative);
                continue;
            };
            break;
        }

        let Some(mut node) = self.expression_chain()? else {
            self.pos = start;
            return Ok(None);
        };

        while let Some(op) = prefixes.pop() {
            node = AstNode::UnaryOp(UnaryOp {
                op,
                on: node.into(),
            });
        }

        Ok(Some(node))
    }

    // Primary expression with chained operations:
    // - property access (dot operator),
    // - index op or
    // - function call
    // Consumes and returns node or consumes nothing.
    fn expression_chain(&mut self) -> ParseResult<Option<AstNode>> {
        let Some(mut node) = self.primary_expression()? else {
            return Ok(None);
        };

        loop {
            let savepoint = self.pos;
            self.linespace();

            if let Some(property) = self.property_of()? {
                node = AstNode::PropertyOf(PropertyOf {
                    from: node.into(),
                    property,
                });
                continue;
            }

            if let Some(index) = self.index_of()? {
                node = AstNode::IndexOf(IndexOf {
                    from: node.into(),
                    index: index.into(),
                });
                continue;
            }

            if let Some(args) = self.call_of()? {
                node = AstNode::FunctionCall(FunctionCall {
                    on: node.into(),
                    args,
                });
                continue;
            }

            if self.chain_catch() {
                node = AstNode::ChainCatch(ChainCatch { inner: node.into() });
                continue;
            }

            self.pos = savepoint;
            break;
        }

        Ok(Some(node))
    }

    // First element in an expression chain:
    //   - literals,
    //   - list or object
    //   - parenthesised expression
    fn primary_expression(&mut self) -> ParseResult<Option<AstNode>> {
        let start = self.pos;
        match self.unchecked_identifier() {
            Some(b"this") => return Ok(Some(AstNode::This)),
            Some(b"nil") => return Ok(Some(AstNode::Value(Value::Nil))),
            Some(b"dict") => return Ok(Some(AstNode::DictBuilder(self.dict()?))),
            Some(b"true") => return Ok(Some(AstNode::Value(Value::Bool(true)))),
            Some(b"false") => return Ok(Some(AstNode::Value(Value::Bool(false)))),
            Some(b"function") => return Ok(Some(AstNode::FunctionDef(self.function_def()?))),
            // to error if function def added to something, for example
            _ => {
                self.pos = start;
            }
        }
        // FIXME: combine this with above so as not to have to re-parse identifier
        if let Some(v) = self.identifier()? {
            return Ok(Some(AstNode::Identifier(v)));
        }
        if let Some(v) = self.list()? {
            return Ok(Some(AstNode::ListBuilder(v)));
        }
        if let Some(v) = self.string()? {
            return Ok(Some(v));
        }
        if let Some(v) = self.float() {
            return Ok(Some(AstNode::Value(v)));
        }
        if let Some(v) = self.integer() {
            return Ok(Some(AstNode::Value(v)));
        }
        if let Some(v) = self.parens_expression()? {
            return Ok(Some(v));
        }
        Ok(None)
    }

    // Parenthesised expression
    // "(" expr ")"
    fn parens_expression(&mut self) -> ParseResult<Option<AstNode>> {
        if !self.char(b'(') {
            return Ok(None);
        };
        self.whitespace_comments();

        let Some(expr) = self.expression()? else {
            return Err(super::SyntaxError {
                pos: self.pos,
                msg: EXPECTED_EXPRESSION,
            });
        };

        self.whitespace_comments();

        if !self.char(b')') {
            return Err(super::SyntaxError {
                pos: self.pos,
                msg: EXPECTED_COSING_PARENS,
            });
        };

        Ok(Some(expr))
    }

    fn property_of(&mut self) -> ParseResult<Option<Identifier>> {
        if !self.char(b'.') {
            return Ok(None);
        }

        self.whitespace_comments();

        let Some(ident) = self.unchecked_identifier() else {
            return Err(SyntaxError::new(self.pos, EXPECTED_IDENTIFIER));
        };

        let ident = String::from_utf8_lossy(ident).to_string();

        Ok(Some(Identifier { name: ident }))
    }

    fn index_of(&mut self) -> ParseResult<Option<AstNode>> {
        if !self.char(b'[') {
            return Ok(None);
        }

        self.whitespace_comments();

        let Some(index) = self.expression()? else {
            return Err(SyntaxError::new(self.pos, EXPECTED_EXPRESSION));
        };

        self.whitespace_comments();

        if !self.char(b']') {
            return Err(SyntaxError::new(self.pos, EXPECTED_COSING_BRACKET));
        }

        Ok(Some(index))
    }

    fn call_of(&mut self) -> ParseResult<Option<Vec<Arg>>> {
        let start = self.pos;
        if !self.char(b'(') {
            return Ok(None);
        };

        self.whitespace_comments();

        let mut args = Vec::new();
        loop {
            if self.char(b')') {
                break;
            }

            let Some(expr) = self.expression()? else {
                if self.is_eoi() {
                    return Err(SyntaxError {
                        pos: start,
                        msg: MISSING_CALL_CLOSE,
                    });
                } else {
                    return Err(SyntaxError {
                        pos: self.pos,
                        msg: EXPECTED_CALL_ARG_OR_CLOSE,
                    });
                }
            };

            let arg = match expr {
                AstNode::KeyValue(KeyValue { key, value }) => Arg {
                    name: Some(key),
                    value: *value,
                },
                other => Arg {
                    name: None,
                    value: other,
                },
            };
            args.push(arg);
            self.whitespace_comments();

            if self.char(b',') {
                self.whitespace_comments();
                continue;
            }

            if self.char(b')') {
                break;
            }

            if self.is_eoi() {
                return Err(SyntaxError {
                    pos: start,
                    msg: MISSING_CALL_CLOSE,
                });
            }

            return Err(SyntaxError {
                pos: self.pos,
                msg: EXPECTED_CALL_COMMA_OR_CLOSE,
            });
        }

        Ok(Some(args))
    }

    fn chain_catch(&mut self) -> bool {
        self.char(b'?')
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::parser::{dict::EXPECTED_OPEN, string::MISSING_END_QUOTE, tests::*};

    #[track_caller]
    pub(crate) fn do_test_expr_ok(
        input: &'static str,
        expected: impl Into<AstNode>,
        expected_end: isize,
    ) {
        do_test_parser_ok(
            Parser::expression,
            input,
            Some(expected.into()),
            expected_end,
        );
    }

    #[track_caller]
    pub(crate) fn do_test_expr_err(
        input: &'static str,
        expected_err_pos: usize,
        expected_err_msg: &'static str,
    ) {
        do_test_parser_err(
            Parser::expression,
            input,
            expected_err_pos,
            expected_err_msg,
        );
    }

    #[test]
    fn test_expr_ok() {
        do_test_expr_ok(" foo ", id("foo"), -1);
        do_test_expr_ok(" foo.bar ", prop_of(id("foo"), "bar"), -1);
        do_test_expr_ok(" foo[\"bar\"] ", index_of(id("foo"), s("bar")), -1);
        do_test_expr_ok(" foo[bar] ", index_of(id("foo"), id("bar")), -1);
        do_test_expr_ok(" foo() ", _call!(id("foo"),), -1);
        do_test_expr_ok(
            r#" foo.bar(a,"33",c:7*2) "#,
            _call!(
                prop_of(id("foo"), "bar"),
                arg(id("a")),
                arg(s("33")),
                arg(id("c"), mul(i(7), i(2))),
            ),
            -1,
        );
        do_test_expr_ok(" foo? ", chain_catch(id("foo")), -1);
        do_test_expr_ok(" foo.map ", prop_of(id("foo"), "map"), -1);
        do_test_expr_ok(
            " aa.bb[cc].dd[\"ee\"]?[66] ",
            index_of(
                chain_catch(index_of(
                    prop_of(index_of(prop_of(id("aa"), "bb"), id("cc")), "dd"),
                    s("ee"),
                )),
                i(66),
            ),
            -1,
        );
        do_test_expr_ok(
            " aa . bb [ cc ] . dd [ \"ee\" ] ? [ 66 ]  ",
            index_of(
                chain_catch(index_of(
                    prop_of(index_of(prop_of(id("aa"), "bb"), id("cc")), "dd"),
                    s("ee"),
                )),
                i(66),
            ),
            -2,
        );
        do_test_expr_ok(
            " aa . #comment\n bb [ #comment\n cc #comment\n ] . #comment\n dd [ \"ee\" ] ? [ 66 ]  ",
            index_of(
                chain_catch(index_of(
                    prop_of(index_of(prop_of(id("aa"), "bb"), id("cc")), "dd"),
                    s("ee"),
                )),
                i(66),
            ),
            -2,
        );
        do_test_expr_ok(" aa #comment\n . foo  ", id("aa"), 3);
        do_test_expr_ok(" aa #comment\n [1]  ", id("aa"), 3);
        do_test_expr_ok(" aa #comment\n ?  ", id("aa"), 3);
        do_test_expr_ok(
            " foo.bar(#comment\n a#comment\n ,#comment\n 7#comment\n ,#comment\n c :#comment\n 7*2) ",
            _call!(
                prop_of(id("foo"), "bar"),
                arg(id("a")),
                arg(i(7)),
                arg(id("c"), mul(i(7), i(2))),
            ),
            -1,
        );
        // unary prefix expressions
        do_test_expr_ok(" -foo ", neg(id("foo")), -1);
        do_test_expr_ok(" !foo ", not(id("foo")), -1);
        do_test_expr_ok(
            " -!-!!--foo.bar ",
            neg(not(neg(not(not(neg(neg(prop_of(id("foo"), "bar")))))))),
            -1,
        );

        // math infix expressions
        do_test_expr_ok(" a + b ", add(id("a"), id("b")), -1);
        do_test_expr_ok(" a - b ", sub(id("a"), id("b")), -1);
        do_test_expr_ok(" a * b ", mul(id("a"), id("b")), -1);
        do_test_expr_ok(" a / b ", div(id("a"), id("b")), -1);
        do_test_expr_ok(" a % b ", modulo(id("a"), id("b")), -1);

        do_test_expr_ok(" a+b ", add(id("a"), id("b")), -1);
        do_test_expr_ok(
            " a + #comment\n \n b - #comment\n \n c ",
            sub(add(id("a"), id("b")), id("c")),
            -1,
        );
        do_test_expr_ok(
            " a * #comment\n \n b / #comment\n \n c ",
            div(mul(id("a"), id("b")), id("c")),
            -1,
        );
        do_test_expr_ok(
            " a + b * c - d / e ",
            sub(add(id("a"), mul(id("b"), id("c"))), div(id("d"), id("e"))),
            -1,
        );

        // comparison expressions
        do_test_expr_ok(" a == b ", equal(id("a"), id("b")), -1);
        do_test_expr_ok(" a != b ", not_equal(id("a"), id("b")), -1);
        do_test_expr_ok(" a < b ", less(id("a"), id("b")), -1);
        do_test_expr_ok(" a > b ", greater(id("a"), id("b")), -1);
        do_test_expr_ok(" a >= b ", greater_equal(id("a"), id("b")), -1);
        do_test_expr_ok(" a <= b ", less_equal(id("a"), id("b")), -1);
        do_test_expr_ok(" a<=b ", less_equal(id("a"), id("b")), -1);
        do_test_expr_ok(
            " a * b == c+d ",
            equal(mul(id("a"), id("b")), add(id("c"), id("d"))),
            -1,
        );

        do_test_expr_ok(" a == b ", equal(id("a"), id("b")), -1);
        do_test_expr_ok(" a != b ", not_equal(id("a"), id("b")), -1);
        do_test_expr_ok(" a < b ", less(id("a"), id("b")), -1);
        do_test_expr_ok(" a > b ", greater(id("a"), id("b")), -1);
        do_test_expr_ok(" a >= b ", greater_equal(id("a"), id("b")), -1);
        do_test_expr_ok(" a <= b ", less_equal(id("a"), id("b")), -1);
        do_test_expr_ok(" a == #comment\n \n b ", equal(id("a"), id("b")), -1);
        do_test_expr_ok(
            " a * b == c + d ",
            equal(mul(id("a"), id("b")), add(id("c"), id("d"))),
            -1,
        );

        // logical ops
        do_test_expr_ok(" a && b ", and(id("a"), id("b")), -1);
        do_test_expr_ok(" a || b ", or(id("a"), id("b")), -1);
        do_test_expr_ok(" a||b ", or(id("a"), id("b")), -1);
        do_test_expr_ok(" a || #comment \n b ", or(id("a"), id("b")), -1);
        do_test_expr_ok(" a && b || c ", or(and(id("a"), id("b")), id("c")), -1);
        do_test_expr_ok(
            " a < b || c+d ",
            or(less(id("a"), id("b")), add(id("c"), id("d"))),
            -1,
        );

        // early ending (bad expressions)
        do_test_expr_ok(" a=!b ", id("a"), 2);
        do_test_expr_ok(" a=>b ", id("a"), 2);
        do_test_expr_ok(" a + b #comment\n \n - c ", add(id("a"), id("b")), 6);
        do_test_expr_ok(" a * b #comment\n \n / c ", mul(id("a"), id("b")), 6);
        do_test_expr_ok(" a # comment \n == b ", id("a"), 2);
        do_test_expr_ok(" a # comment \n && b ", id("a"), 2);
        do_test_expr_ok(" a <= b == c ", less_equal(id("a"), id("b")), 7);

        do_test_expr_ok(" (1>2)+3 ", add(greater(i(1), i(2)), i(3)), -1);
        do_test_expr_ok(" 1>2+3 ", greater(i(1), add(i(2), i(3))), -1);

        do_test_expr_ok(
            r#" foo?.bar["baz" + 7] + 32 * 7 >= b.c * (x + y % z) || (3>4)+7 "#,
            or(
                greater_equal(
                    add(
                        index_of(prop_of(chain_catch(id("foo")), "bar"), add(s("baz"), i(7))),
                        mul(i(32), i(7)),
                    ),
                    mul(
                        prop_of(id("b"), "c"),
                        add(id("x"), modulo(id("y"), id("z"))),
                    ),
                ),
                add(greater(i(3), i(4)), i(7)),
            ),
            -1,
        );

        do_test_expr_err(" dict.foo ", 5, EXPECTED_OPEN);
        do_test_expr_err(" foo(]) ", 5, EXPECTED_CALL_ARG_OR_CLOSE);
        do_test_expr_err(" foo(a,,) ", 7, EXPECTED_CALL_ARG_OR_CLOSE);
        do_test_expr_err(" foo(a b) ", 7, EXPECTED_CALL_COMMA_OR_CLOSE);
        do_test_expr_err(" foo(a", 4, MISSING_CALL_CLOSE);
        do_test_expr_err(" foo( ", 4, MISSING_CALL_CLOSE);
    }

    #[test]
    fn test_parens_expr_ok() {
        do_test_expr_ok(" (1) ", i(1), -1);
        do_test_expr_ok(" (((1))) ", i(1), -1);
        do_test_expr_ok(" (((1)))", i(1), 0);
    }

    #[test]
    fn test_parens_expr_none() {
        do_test_parser_none(Parser::parens_expression, "  ");
    }

    #[test]
    fn test_parens_expr_err() {
        do_test_expr_err(" (((1)] ", 6, EXPECTED_COSING_PARENS);
        do_test_expr_err(" (((1", 5, EXPECTED_COSING_PARENS);
        do_test_expr_err(" (;", 2, EXPECTED_EXPRESSION);
        do_test_expr_err(" (((\"..", 4, MISSING_END_QUOTE);
    }

    #[test]
    fn test_this() {
        do_test_expr_ok(" this ", this(), -1);
        do_test_expr_ok(" this", this(), 0);

        do_test_expr_ok(" thiss ", id("thiss"), -1);
    }

    #[test]
    fn test_nil() {
        do_test_expr_ok(" nil ", nil(), -1);
        do_test_expr_ok(" nil", nil(), 0);

        do_test_expr_ok(" nil2 ", id("nil2"), -1);
    }

    #[test]
    fn test_boolean() {
        do_test_expr_ok(" true ", b(true), -1);
        do_test_expr_ok(" false ", b(false), -1);
        do_test_expr_ok(" true", b(true), 0);

        do_test_expr_ok(" tru ", id("tru"), -1);
        do_test_expr_ok(" falsey", id("falsey"), 0);
        do_test_parser_none(Parser::expression, " ");
    }
}
