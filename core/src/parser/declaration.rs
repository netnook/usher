use super::{ParseResult, Parser, SyntaxError};
use crate::lang::{AstNode, Declaration, Span};

impl<'a> Parser<'a> {
    // "var" identifier = expr
    pub(super) fn var_stmt(&mut self, span: Span) -> ParseResult<AstNode> {
        // already passed "var" when called

        self.req_whitespace_comments()?;

        let Some(var) = self.declaration_identifier()? else {
            return Err(SyntaxError::ExpectedVariableIdentifier { pos: self.pos });
        };

        self.linespace();

        if !self.char(b'=') {
            return Err(SyntaxError::DeclarationExpectedEquals {
                got: self.peek() as char,
                pos: self.pos,
            });
        }

        self.whitespace_comments();

        let Some(value) = self.complex_expression()? else {
            return Err(SyntaxError::DeclarationExpectedExpression { pos: self.pos });
        };

        Ok(AstNode::Declaration(Declaration {
            var,
            value: value.into(),
            span: span.extended(self.pos),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::Span,
        parser::{
            stmt::tests::{do_test_stmt_err, do_test_stmt_ok},
            tests::*,
        },
    };

    #[test]
    fn test_var() {
        do_test_stmt_ok(" var a=x+2 ", decl(var("a"), add(var("x"), i(2))), -1);
        do_test_stmt_ok(" var a = x + 2 ", decl(var("a"), add(var("x"), i(2))), -1);
        do_test_stmt_ok(" var a = x + 2 ", decl(var("a"), add(var("x"), i(2))), -1);
        do_test_stmt_ok(
            " var # comment \n a = # comment \n x + 2 ",
            decl(var("a"), add(var("x"), i(2))),
            -1,
        );

        do_test_stmt_err(
            " var,=x+2 ",
            SyntaxError::ExpectedWhitespaceOrComment { pos: 4 },
        );
        do_test_stmt_err(
            " var ,=x+2 ",
            SyntaxError::ExpectedVariableIdentifier { pos: 5 },
        );
        do_test_stmt_err(
            " var a # comment \n = 1 ",
            SyntaxError::DeclarationExpectedEquals { got: '#', pos: 7 },
        );
        do_test_stmt_err(
            " var a +  = 1 ",
            SyntaxError::DeclarationExpectedEquals { got: '+', pos: 7 },
        );
        do_test_stmt_err(
            " var a = ; 1 ",
            SyntaxError::DeclarationExpectedExpression { pos: 9 },
        );
        do_test_stmt_err(
            " var print = 1 ",
            SyntaxError::ReservedName {
                got: "print".to_string(),
                span: Span::new(5, 5),
            },
        );
        do_test_stmt_err(
            " var else = 1 ",
            SyntaxError::ReservedKeyword {
                got: "else".to_string(),
                span: Span::new(5, 4),
            },
        );
    }
}
