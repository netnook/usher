use super::{ParseResult, Parser};
use crate::lang::AstNode;

const EXPECTED_EXPRESSION: &str = "Expected expression.";
const EXPECTED_COSING_PARENS: &str = "Expected closing parenthesis ')'";

impl<'a> Parser<'a> {
    /// Consume an expression or nothing.
    pub(super) fn expression(&mut self) -> ParseResult<Option<AstNode>> {
        // FIXME: this is a dummy implementation to enable other parsers

        if let Some(v) = self.primary_expression()? {
            return Ok(Some(v));
        }

        Ok(None)
    }

    // Primary expression - literals, list, object or parenthesised expression
    // Consumes and returns node or consumes nothing.
    pub(super) fn primary_expression(&mut self) -> ParseResult<Option<AstNode>> {
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
            return Ok(Some(v));
        }
        Ok(None)
    }

    // Parenthesised expression
    // "(" expr ")"
    pub(super) fn parens_expression(&mut self) -> ParseResult<Option<AstNode>> {
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{string::MISSING_END_QUOTE, tests::*};

    #[test]
    fn test_parens_expr_ok() {
        do_test_parser_some(Parser::parens_expression, "-(1)-", i(1).into(), 1);
        do_test_parser_some(Parser::parens_expression, "-(((1)))-", i(1).into(), 1);
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
