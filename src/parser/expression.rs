use super::{ParseResult, Parser, SyntaxError, chars::is_digit};
use crate::{
    lang::{
        Accept, Arg, AstNode, BinaryOp, BinaryOpCode, CatchMissingOptionalProperty, End,
        FunctionCall, FunctionCallVariant, Identifier, IndexOf, KeyValueBuilder, Literal,
        PropertyOf, Span, This, UnaryOp, UnaryOpCode, Value, Var, Visitor, VisitorResult,
    },
    parser::identifier::UncheckedIdentifier,
};
use std::rc::Rc;

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
        let Some(lhs) = self.logical_expression()? else {
            return Ok(None);
        };

        let savepos = self.pos;

        self.linespace();

        if !self.char(b':') {
            self.pos = savepos;
            return Ok(Some(lhs));
        }

        let AstNode::Var(id) = lhs else {
            return Err(SyntaxError::KeyValueExpectsIdentOnLHS {
                span: lhs.span(),
                pos: self.pos - 1,
            });
        };

        self.whitespace_comments();

        let Some(rhs) = self.logical_expression()? else {
            return Err(SyntaxError::ExpectsExpression { pos: self.pos });
        };

        Ok(Some(AstNode::KeyValue(KeyValueBuilder {
            key: id.ident,
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
            return Err(SyntaxError::ExpectsExpression { pos: self.pos });
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
                return Err(SyntaxError::ExpectsExpression { pos: self.pos });
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

        let mut ops = Vec::new();

        loop {
            if self.char(b'!') {
                ops.push(UnaryOp {
                    op: UnaryOpCode::Not,
                    on: Box::new(End::new(Span::new(0, 0)).into()), // dummy
                    span: Span::new(self.pos - 1, 1),
                });
                continue;
            };
            if self.char(b'-') {
                if self.peek_and_test(is_digit) {
                    self.pos -= 1;
                    break;
                }
                ops.push(UnaryOp {
                    op: UnaryOpCode::Negative,
                    on: Box::new(End::new(Span::new(0, 0)).into()), // dummy
                    span: Span::new(self.pos - 1, 1),
                });
                continue;
            };
            break;
        }

        let Some(mut node) = self.expression_chain()? else {
            self.pos = start;
            return Ok(None);
        };

        while let Some(mut op) = ops.pop() {
            *op.on = node;
            node = AstNode::UnaryOp(op);
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

            if let Some((index, span)) = self.index_of()? {
                let optional_property = self.optional_operator();
                node = AstNode::IndexOf(IndexOf {
                    of: node.into(),
                    index: index.into(),
                    optional_property,
                    span: span.extended(self.pos),
                });
                continue;
            }

            if let Some((args, span)) = self.call_of()? {
                node = match node {
                    // FIXME: what if property-of has the `throw_on_missing_prop` flag set !!!
                    AstNode::PropertyOf(node) => AstNode::FunctionCall(FunctionCall {
                        variant: FunctionCallVariant::MethodCall {
                            on: node.of,
                            function: node.property,
                        },
                        args,
                        span: Span::merge(node.span, span),
                    }),
                    AstNode::CatchMissingOptionalProperty(_) => {
                        return Err(SyntaxError::UnexpectedParseState {
                            details: "Found expression chain with call on CatchMissingOptionalProperty - please report a bug!".to_string(),
                            span,
                        });
                    }
                    AstNode::Var(var) => AstNode::FunctionCall(FunctionCall {
                        span: Span::merge(var.span(), span),
                        variant: FunctionCallVariant::FunctionCall {
                            function: var.ident,
                        },
                        args,
                    }),
                    _ => AstNode::FunctionCall(FunctionCall {
                        variant: FunctionCallVariant::AnonymousCall { on: node.into() },
                        args,
                        span,
                    }),
                };
                continue;
            }

            // dot '.' operator can appear on new line
            self.whitespace_comments();
            if let Some((property, span)) = self.property_of()? {
                let optional_property = self.optional_operator();
                node = AstNode::PropertyOf(PropertyOf {
                    of: node.into(),
                    property,
                    span: span.extended(self.pos),
                    optional_property,
                });
                continue;
            }

            self.pos = savepoint;
            break;
        }

        struct ContainsOptionalProperty;
        impl Visitor<bool> for ContainsOptionalProperty {
            fn visit_property_of(&mut self, v: &PropertyOf) -> VisitorResult<bool> {
                if v.optional_property {
                    return VisitorResult::Stop(true);
                }
                v.accept(self)
            }
            fn visit_index_of(&mut self, v: &IndexOf) -> VisitorResult<bool> {
                if v.optional_property {
                    return VisitorResult::Stop(true);
                }
                v.accept(self)
            }
        }

        let contains_optional_property = ContainsOptionalProperty.go(&node).unwrap_or(false);
        if contains_optional_property {
            node = AstNode::CatchMissingOptionalProperty(CatchMissingOptionalProperty {
                inner: node.into(),
            });
        }

        Ok(Some(node))
    }

    // First element in an expression chain:
    //   - literals,
    //   - list or object
    //   - parenthesised expression
    fn primary_expression(&mut self) -> ParseResult<Option<AstNode>> {
        let start = self.pos;

        if let Some(UncheckedIdentifier(name, span)) = self.unchecked_identifier() {
            match name {
                "this" => {
                    return Ok(Some(AstNode::This(This::new(span))));
                }
                "nil" => {
                    return Ok(Some(AstNode::Literal(Literal::new(Value::Nil, span))));
                }
                "end" => {
                    return Ok(Some(AstNode::Literal(Literal::new(Value::End, span))));
                }
                "dict" => {
                    return Ok(Some(AstNode::DictBuilder(self.dict(start)?)));
                }
                "true" => {
                    return Ok(Some(AstNode::Literal(Literal::new(
                        Value::Bool(true),
                        span,
                    ))));
                }
                "false" => {
                    return Ok(Some(AstNode::Literal(Literal::new(
                        Value::Bool(false),
                        span,
                    ))));
                }
                "function" => {
                    return Ok(Some(AstNode::FunctionDef(Rc::new(
                        self.function_def(span)?,
                    ))));
                }
                id => {
                    // FIXME: where is the ident checked to make sure that it is valid/permitted ?
                    return Ok(Some(AstNode::Var(Var::new(Identifier::new(
                        id.to_string(),
                        span,
                    )))));
                }
            }
        }
        if let Some(v) = self.list()? {
            return Ok(Some(AstNode::ListBuilder(v)));
        }
        if let Some(v) = self.string()? {
            return Ok(Some(v));
        }
        if let Some(v) = self.float() {
            return Ok(Some(AstNode::Literal(v)));
        }
        if let Some(v) = self.integer() {
            return Ok(Some(AstNode::Literal(v)));
        }
        if let Some(v) = self.parens_expression()? {
            return Ok(Some(v));
        }
        Ok(None)
    }

    // Parenthesised expression
    // "(" expr ")"
    fn parens_expression(&mut self) -> ParseResult<Option<AstNode>> {
        let start = self.pos;
        if !self.char(b'(') {
            return Ok(None);
        };
        self.whitespace_comments();

        let Some(expr) = self.expression()? else {
            return Err(SyntaxError::ExpectsExpression { pos: self.pos });
        };

        self.whitespace_comments();

        if self.is_eoi() {
            return Err(SyntaxError::MissingClosingParens { pos: start });
        };

        if !self.char(b')') {
            return Err(SyntaxError::ExpectedClosingParens {
                got: self.peek() as char,
                pos: self.pos,
            });
        };

        Ok(Some(expr))
    }

    fn property_of(&mut self) -> ParseResult<Option<(Identifier, Span)>> {
        let start = self.pos;
        if !self.char(b'.') {
            return Ok(None);
        }

        self.whitespace_comments();

        let Some(UncheckedIdentifier(ident, span)) = self.unchecked_identifier() else {
            return Err(SyntaxError::PropertyOfExpectedIdentifier { pos: self.pos });
        };

        Ok(Some((
            Identifier::new(ident.to_string(), span),
            Span::start_end(start, self.pos),
        )))
    }

    fn index_of(&mut self) -> ParseResult<Option<(AstNode, Span)>> {
        let start = self.pos;
        if !self.char(b'[') {
            return Ok(None);
        }

        self.whitespace_comments();

        let Some(index) = self.expression()? else {
            return Err(SyntaxError::ExpectsExpression { pos: self.pos });
        };

        self.whitespace_comments();

        if !self.char(b']') {
            return Err(SyntaxError::IndexOfExpectedClosingBracket {
                got: self.peek() as char,
                pos: self.pos,
            });
        }

        Ok(Some((index, Span::start_end(start, self.pos))))
    }

    fn call_of(&mut self) -> ParseResult<Option<(Vec<Arg>, Span)>> {
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
                    return Err(SyntaxError::MissingClosingParens { pos: start });
                } else {
                    return Err(SyntaxError::FunctionCallExpectedArgOrClosingParens {
                        pos: self.pos,
                    });
                }
            };

            let arg = match expr {
                AstNode::KeyValue(KeyValueBuilder { key, value }) => Arg {
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
                return Err(SyntaxError::MissingClosingParens { pos: start });
            }

            return Err(SyntaxError::FunctionCallExpectedCommaOrCloseParens { pos: self.pos });
        }

        Ok(Some((args, Span::start_end(start, self.pos))))
    }

    fn optional_operator(&mut self) -> bool {
        self.char(b'?')
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::parser::tests::*;

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
    pub(crate) fn do_test_expr_err(input: &'static str, expected_err: SyntaxError) {
        do_test_parser_err(Parser::expression, input, expected_err);
    }

    #[test]
    fn test_expr_ok() {
        do_test_expr_ok(" foo ", var("foo"), -1);
        do_test_expr_ok(" foo.bar ", prop_of(var("foo"), "bar"), -1);
        do_test_expr_ok(" foo[\"bar\"] ", index_of(var("foo"), s("bar")), -1);
        do_test_expr_ok(" foo[bar] ", index_of(var("foo"), var("bar")), -1);
        do_test_expr_ok(" foo() ", _function_call!("foo",), -1);
        do_test_expr_ok(
            r#" foo.bar(a,"33",c:7*2) "#,
            _method_call!(
                var("foo"),
                "bar",
                arg!(var("a")),
                arg!(s("33")),
                arg!(id("c"), mul(i(7), i(2)))
            ),
            -1,
        );
        do_test_expr_ok(" foo.map ", prop_of(var("foo"), "map"), -1);
        do_test_expr_ok(
            " foo.bar? ",
            catch_missing_optional_property(
                prop_of(var("foo"), "bar").with_missing_prop_to_nil(true),
            ),
            -1,
        );
        do_test_expr_ok(
            " foo.bar?.baz ",
            catch_missing_optional_property(prop_of(
                prop_of(var("foo"), "bar").with_missing_prop_to_nil(true),
                "baz",
            )),
            -1,
        );
        do_test_expr_ok(
            " foo[0]? ",
            catch_missing_optional_property(
                index_of(var("foo"), i(0)).with_optional_property(true),
            ),
            -1,
        );
        do_test_expr_ok(
            " foo[0]?[1] ",
            catch_missing_optional_property(index_of(
                index_of(var("foo"), i(0)).with_optional_property(true),
                i(1),
            )),
            -1,
        );
        do_test_expr_ok(
            " aa.bb[cc].dd[\"ee\"]?[66] ",
            catch_missing_optional_property(index_of(
                index_of(
                    prop_of(index_of(prop_of(var("aa"), "bb"), var("cc")), "dd"),
                    s("ee"),
                )
                .with_optional_property(true),
                i(66),
            )),
            -1,
        );
        do_test_expr_ok(
            " aa . bb [ cc ] . dd [ \"ee\" ]? [ 66 ]  ",
            catch_missing_optional_property(index_of(
                index_of(
                    prop_of(index_of(prop_of(var("aa"), "bb"), var("cc")), "dd"),
                    s("ee"),
                )
                .with_optional_property(true),
                i(66),
            )),
            -2,
        );
        do_test_expr_ok(
            " aa . #comment\n bb [ #comment\n cc #comment\n ] . #comment\n dd [ \"ee\" ]? [ 66 ]  ",
            catch_missing_optional_property(index_of(
                index_of(
                    prop_of(index_of(prop_of(var("aa"), "bb"), var("cc")), "dd"),
                    s("ee"),
                )
                .with_optional_property(true),
                i(66),
            )),
            -2,
        );
        do_test_expr_ok(" aa #comment\n . foo  ", prop_of(var("aa"), "foo"), -2);
        do_test_expr_ok(" aa #comment\n [1]  ", var("aa"), 3);
        do_test_expr_ok(" aa #comment\n ?  ", var("aa"), 3);
        do_test_expr_ok(
            " foo(#comment\n a#comment\n ,#comment\n 7#comment\n ,#comment\n c :#comment\n 7*2) ",
            _function_call!(
                "foo",
                arg!(var("a")),
                arg!(i(7)),
                arg!(id("c"), mul(i(7), i(2)))
            ),
            -1,
        );
        do_test_expr_ok(
            " foo.bar().baz(#comment\n a#comment\n ,#comment\n 7#comment\n ,#comment\n c :#comment\n 7*2) ",
            _method_call!(
                _method_call!(var("foo"), "bar",),
                "baz",
                arg!(var("a")),
                arg!(i(7)),
                arg!(id("c"), mul(i(7), i(2)))
            ),
            -1,
        );
        do_test_expr_ok(
            r#" foo.bar[1]()("x") "#,
            _anon_call!(
                _anon_call!(index_of(prop_of(var("foo"), "bar"), i(1)),),
                arg!(s("x"))
            ),
            -1,
        );
        // unary prefix expressions
        do_test_expr_ok(" -foo ", neg(var("foo")), -1);
        do_test_expr_ok(" !foo ", not(var("foo")), -1);
        do_test_expr_ok(
            " -!-!!--foo.bar ",
            neg(not(neg(not(not(neg(neg(prop_of(var("foo"), "bar")))))))),
            -1,
        );

        // math infix expressions
        do_test_expr_ok(" a + b ", add(var("a"), var("b")), -1);
        do_test_expr_ok(" a - b ", sub(var("a"), var("b")), -1);
        do_test_expr_ok(" a * b ", mul(var("a"), var("b")), -1);
        do_test_expr_ok(" a / b ", div(var("a"), var("b")), -1);
        do_test_expr_ok(" a % b ", modulo(var("a"), var("b")), -1);

        do_test_expr_ok(" a+b ", add(var("a"), var("b")), -1);
        do_test_expr_ok(
            " a + #comment\n \n b - #comment\n \n c ",
            sub(add(var("a"), var("b")), var("c")),
            -1,
        );
        do_test_expr_ok(
            " a * #comment\n \n b / #comment\n \n c ",
            div(mul(var("a"), var("b")), var("c")),
            -1,
        );
        do_test_expr_ok(
            " a + b * c - d / e ",
            sub(
                add(var("a"), mul(var("b"), var("c"))),
                div(var("d"), var("e")),
            ),
            -1,
        );

        // comparison expressions
        do_test_expr_ok(" a == b ", equal(var("a"), var("b")), -1);
        do_test_expr_ok(" a != b ", not_equal(var("a"), var("b")), -1);
        do_test_expr_ok(" a < b ", less(var("a"), var("b")), -1);
        do_test_expr_ok(" a > b ", greater(var("a"), var("b")), -1);
        do_test_expr_ok(" a >= b ", greater_equal(var("a"), var("b")), -1);
        do_test_expr_ok(" a <= b ", less_equal(var("a"), var("b")), -1);
        do_test_expr_ok(" a<=b ", less_equal(var("a"), var("b")), -1);
        do_test_expr_ok(
            " a * b == c+d ",
            equal(mul(var("a"), var("b")), add(var("c"), var("d"))),
            -1,
        );

        do_test_expr_ok(" a == b ", equal(var("a"), var("b")), -1);
        do_test_expr_ok(" a != b ", not_equal(var("a"), var("b")), -1);
        do_test_expr_ok(" a < b ", less(var("a"), var("b")), -1);
        do_test_expr_ok(" a > b ", greater(var("a"), var("b")), -1);
        do_test_expr_ok(" a >= b ", greater_equal(var("a"), var("b")), -1);
        do_test_expr_ok(" a <= b ", less_equal(var("a"), var("b")), -1);
        do_test_expr_ok(" a == #comment\n \n b ", equal(var("a"), var("b")), -1);
        do_test_expr_ok(
            " a * b == c + d ",
            equal(mul(var("a"), var("b")), add(var("c"), var("d"))),
            -1,
        );

        // logical ops
        do_test_expr_ok(" a && b ", and(var("a"), var("b")), -1);
        do_test_expr_ok(" a || b ", or(var("a"), var("b")), -1);
        do_test_expr_ok(" a||b ", or(var("a"), var("b")), -1);
        do_test_expr_ok(" a || #comment \n b ", or(var("a"), var("b")), -1);
        do_test_expr_ok(" a && b || c ", or(and(var("a"), var("b")), var("c")), -1);
        do_test_expr_ok(
            " a < b || c+d ",
            or(less(var("a"), var("b")), add(var("c"), var("d"))),
            -1,
        );

        // early ending (bad expressions)
        do_test_expr_ok(" a=!b ", var("a"), 2);
        do_test_expr_ok(" a=>b ", var("a"), 2);
        do_test_expr_ok(" a + b #comment\n \n - c ", add(var("a"), var("b")), 6);
        do_test_expr_ok(" a * b #comment\n \n / c ", mul(var("a"), var("b")), 6);
        do_test_expr_ok(" a # comment \n == b ", var("a"), 2);
        do_test_expr_ok(" a # comment \n && b ", var("a"), 2);
        do_test_expr_ok(" a <= b == c ", less_equal(var("a"), var("b")), 7);

        do_test_expr_ok(" (1>2)+3 ", add(greater(i(1), i(2)), i(3)), -1);
        do_test_expr_ok(" 1>2+3 ", greater(i(1), add(i(2), i(3))), -1);

        do_test_expr_ok(
            r#" foo.bar?["baz" + 7] + 32 * 7 >= b.c * (x + y % z) || (3>4)+7 "#,
            or(
                greater_equal(
                    add(
                        catch_missing_optional_property(index_of(
                            prop_of(var("foo"), "bar").with_missing_prop_to_nil(true),
                            add(s("baz"), i(7)),
                        )),
                        mul(i(32), i(7)),
                    ),
                    mul(
                        prop_of(var("b"), "c"),
                        add(var("x"), modulo(var("y"), var("z"))),
                    ),
                ),
                add(greater(i(3), i(4)), i(7)),
            ),
            -1,
        );

        do_test_expr_err(" dict.foo ", SyntaxError::DictExpectedOpenParens { pos: 5 });
        do_test_expr_err(
            " foo(]) ",
            SyntaxError::FunctionCallExpectedArgOrClosingParens { pos: 5 },
        );
        do_test_expr_err(
            " foo(a,,) ",
            SyntaxError::FunctionCallExpectedArgOrClosingParens { pos: 7 },
        );
        do_test_expr_err(
            " foo(a b) ",
            SyntaxError::FunctionCallExpectedCommaOrCloseParens { pos: 7 },
        );
        do_test_expr_err(" foo(a", SyntaxError::MissingClosingParens { pos: 4 });
        do_test_expr_err(" foo( ", SyntaxError::MissingClosingParens { pos: 4 });
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
        do_test_expr_err(
            " (((1)] ",
            SyntaxError::ExpectedClosingParens { got: ']', pos: 6 },
        );
        do_test_expr_err(" (((1", SyntaxError::MissingClosingParens { pos: 3 });
        do_test_expr_err(" (;", SyntaxError::ExpectsExpression { pos: 2 });
        do_test_expr_err(" (((\"..", SyntaxError::StringMissingCloseQuote { pos: 4 });
    }

    #[test]
    fn test_this() {
        do_test_expr_ok(" this ", this(), -1);
        do_test_expr_ok(" this", this(), 0);

        do_test_expr_ok(" thiss ", var("thiss"), -1);
    }

    #[test]
    fn test_nil() {
        do_test_expr_ok(" nil ", nil(), -1);
        do_test_expr_ok(" nil", nil(), 0);
    }

    #[test]
    fn test_numbers() {
        do_test_expr_ok(" 1234 ", i(1234), -1);
        do_test_expr_ok(" -1234 ", i(-1234), -1);
        do_test_expr_ok(" 12.34 ", f(12.34), -1);
        do_test_expr_ok(" -12.34 ", f(-12.34), -1);
    }

    #[test]
    fn test_boolean() {
        do_test_expr_ok(" tru ", var("tru"), -1);
        do_test_expr_ok(" falsey", var("falsey"), 0);
        do_test_parser_none(Parser::expression, " ");
    }
}
