use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, Identifier, PrefixOp};

const EXPECTED_EXPRESSION: &str = "Expected expression.";
const EXPECTED_COSING_PARENS: &str = "Expected closing parenthesis ')'";
const EXPECTED_COSING_BRACKET: &str = "Expected closing bracket ']'";
const EXPECTED_IDENTIFIER: &str = "Expected identifier.";

impl<'a> Parser<'a> {
    /// Consume an expression or nothing.
    pub(super) fn expression(&mut self) -> ParseResult<Option<AstNode>> {
        // FIXME: this is a dummy implementation to enable other parsers

        if let Some(v) = self.prefix_expression()? {
            return Ok(Some(v));
        }

        Ok(None)
    }

    // Prefix expression
    fn prefix_expression(&mut self) -> ParseResult<Option<AstNode>> {
        let start = self.pos;

        let mut prefixes = Vec::new();

        loop {
            if self.char(b'!') {
                prefixes.push(PrefixOp::Not);
                continue;
            };
            if self.char(b'-') {
                prefixes.push(PrefixOp::Negative);
                continue;
            };
            break;
        }

        let Some(mut node) = self.expression_chain()? else {
            self.pos = start;
            return Ok(None);
        };

        while let Some(op) = prefixes.pop() {
            node = AstNode::PrefixOp {
                of: node.into(),
                op,
            };
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
                node = AstNode::PropertyOf {
                    from: node.into(),
                    property,
                };
                continue;
            }

            if let Some(index) = self.index_of()? {
                node = AstNode::IndexOf {
                    from: node.into(),
                    index: index.into(),
                };
                continue;
            }

            if self.chain_catch() {
                node = AstNode::ChainCatch(node.into());
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
        if let Some(v) = self.this() {
            return Ok(Some(v));
        }
        if let Some(v) = self.string()? {
            return Ok(Some(AstNode::Value(v)));
        }
        if let Some(v) = self.float() {
            return Ok(Some(AstNode::Value(v)));
        }
        if let Some(v) = self.integer() {
            return Ok(Some(AstNode::Value(v)));
        }
        if let Some(v) = self.boolean() {
            return Ok(Some(AstNode::Value(v)));
        }
        if let Some(v) = self.nil() {
            return Ok(Some(AstNode::Value(v)));
        }
        if let Some(v) = self.identifier()? {
            return Ok(Some(AstNode::Identifier(v)));
        }
        if let Some(v) = self.list()? {
            return Ok(Some(AstNode::ListBuilder(v)));
        }
        if let Some(v) = self.object()? {
            return Ok(Some(AstNode::ObjectBuilder(v)));
        }
        if let Some(v) = self.parens_expression()? {
            // FIXME: this in the wrong place for this!
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

        let Some(ident) = self.identifier()? else {
            return Err(SyntaxError::new(self.pos, EXPECTED_IDENTIFIER));
        };

        Ok(Some(ident))
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

    fn chain_catch(&mut self) -> bool {
        self.char(b'?')
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{string::MISSING_END_QUOTE, tests::*};

    #[test]
    fn test_expr_ok() {
        do_test_parser_some(Parser::expression, "-foo-", ident("foo").into(), -1);
        do_test_parser_some(
            Parser::expression,
            "-foo.bar-",
            prop_of(ident("foo").into(), "bar"),
            -1,
        );
        do_test_parser_some(
            Parser::expression,
            "-foo[\"bar\"]-",
            index_of(ident("foo").into(), s("bar").into()),
            -1,
        );
        do_test_parser_some(
            Parser::expression,
            "-foo[bar]-",
            index_of(ident("foo").into(), ident("bar").into()),
            -1,
        );
        do_test_parser_some(
            Parser::expression,
            "-foo?-",
            chain_catch(ident("foo").into()),
            -1,
        );
        do_test_parser_some(
            Parser::expression,
            "-aa.bb[cc].dd[\"ee\"]?[66]-",
            index_of(
                chain_catch(index_of(
                    prop_of(
                        index_of(prop_of(ident("aa").into(), "bb"), ident("cc").into()),
                        "dd",
                    ),
                    s("ee").into(),
                )),
                i(66).into(),
            ),
            -1,
        );
        do_test_parser_some(
            Parser::expression,
            "-aa . bb [ cc ] . dd [ \"ee\" ] ? [ 66 ] -",
            index_of(
                chain_catch(index_of(
                    prop_of(
                        index_of(prop_of(ident("aa").into(), "bb"), ident("cc").into()),
                        "dd",
                    ),
                    s("ee").into(),
                )),
                i(66).into(),
            ),
            -2,
        );
        do_test_parser_some(
            Parser::expression,
            "-aa . #comment\n bb [ #comment\n cc #comment\n ] . #comment\n dd [ \"ee\" ] ? [ 66 ] -",
            index_of(
                chain_catch(index_of(
                    prop_of(
                        index_of(prop_of(ident("aa").into(), "bb"), ident("cc").into()),
                        "dd",
                    ),
                    s("ee").into(),
                )),
                i(66).into(),
            ),
            -2,
        );
        do_test_parser_some(
            Parser::expression,
            "-aa #comment\n . foo -",
            ident("aa").into(),
            3,
        );
        do_test_parser_some(
            Parser::expression,
            "-aa #comment\n [1] -",
            ident("aa").into(),
            3,
        );
        do_test_parser_some(
            Parser::expression,
            "-aa #comment\n ? -",
            ident("aa").into(),
            3,
        );
        do_test_parser_some(Parser::expression, "--foo-", neg(ident("foo").into()), -1);
        do_test_parser_some(Parser::expression, "-!foo-", not(ident("foo").into()), -1);
        do_test_parser_some(
            Parser::expression,
            "--!-!!--foo.bar-",
            neg(not(neg(not(not(neg(neg(prop_of(
                ident("foo").into(),
                "bar",
            )))))))),
            -1,
        );
    }

    #[test]
    fn test_parens_expr_ok() {
        do_test_parser_some(Parser::parens_expression, "-(1)-", i(1).into(), -1);
        do_test_parser_some(Parser::parens_expression, "-(((1)))-", i(1).into(), -1);
        do_test_parser_some(Parser::parens_expression, "-(((1)))", i(1).into(), 0);
    }

    #[test]
    fn test_parens_expr_none() {
        do_test_parser_none(Parser::parens_expression, "--");
    }

    #[test]
    fn test_parens_expr_err() {
        do_test_parser_err(
            Parser::parens_expression,
            "-(((1)]-",
            6,
            EXPECTED_COSING_PARENS,
        );
        do_test_parser_err(
            Parser::parens_expression,
            "-(((1",
            5,
            EXPECTED_COSING_PARENS,
        );
        do_test_parser_err(Parser::parens_expression, "-(;", 2, EXPECTED_EXPRESSION);
        do_test_parser_err(Parser::parens_expression, "-(((\"..", 4, MISSING_END_QUOTE);
    }
}
