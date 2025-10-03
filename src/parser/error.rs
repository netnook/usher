use crate::{
    lang::Span,
    parser::{SourceRef, position},
};
use thiserror::Error;

#[derive(PartialEq)]
pub struct ParseError<'a> {
    pub file: &'a str,
    pub source: &'a str,
    pub error: SyntaxError,
}

impl<'a> ParseError<'a> {
    pub fn span(&self) -> Span {
        self.error.span()
    }

    pub fn find_source_position(&self) -> SourceRef<'a> {
        position::find_source_position(self.file, self.source, self.span())
    }
}

impl<'a> core::fmt::Debug for ParseError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            f.debug_struct("ParseError")
                .field("file", &self.file)
                .field("error", &self.error)
                .finish_non_exhaustive()
        } else {
            f.debug_struct("ParseError")
                .field("file", &self.file)
                .field("source", &self.source)
                .field("error", &self.error)
                .finish()
        }
    }
}

#[derive(Error, PartialEq, Debug)]
pub enum SyntaxError {
    #[error("Expected identifier after 'var'.")]
    ExpectedVariableIdentifier { pos: usize },
    #[error("Unexpected character '{got}'. Expected '=' to follow varable name in declaration.")]
    DeclarationExpectedEquals { got: char, pos: usize },
    #[error("Expected expression.")]
    DeclarationExpectedExpression { pos: usize },
    #[error("Keywords may not be used as identifier.")]
    ReservedKeyword { got: String, span: Span },
    #[error("Reserved name cannot be used for declarations.")]
    ReservedName { got: String, span: Span },
    #[error("Expected whitespace or comment.")]
    ExpectedWhitespaceOrComment { pos: usize },
    #[error("Expected opening '('.")]
    DictExpectedOpenParens { pos: usize },
    #[error("Missing closing ')'.")]
    DictMissingCloseParens { pos: usize },
    #[error("Expected expression or ')'.")]
    DictExpectedKeyOrCloseParens { pos: usize },
    #[error("Expected key value pair separated by ':'.")]
    DictExpectedKeyValuePair { span: Span },
    #[error("Expected ',' or ')'.")]
    DictExpectedCommaOrCloseParens { pos: usize },
    #[error("Expected identifier on LHS of key:value pair.")]
    KeyValueExpectsIdentOnLHS { span: Span, pos: usize },
    #[error("Expected expression.")]
    ExpectsExpression { pos: usize },
    #[error("Missing closing parens")]
    MissingClosingParens { pos: usize },
    #[error("Expected closing parenthesis ')'")]
    ExpectedClosingParens { got: char, pos: usize },
    #[error("Expected identifier.")]
    PropertyOfExpectedIdentifier { pos: usize },
    #[error("Expected closing bracket ']'")]
    IndexOfExpectedClosingBracket { got: char, pos: usize },
    #[error("Expected function call argument or closing parenthesis ')'.")]
    FunctionCallExpectedArgOrClosingParens { pos: usize },
    #[error("Expected comma or closing parenthesis ')' for function call arguments.")]
    FunctionCallExpectedCommaOrCloseParens { pos: usize },
    #[error("Invalid characters CR or LF in string.")]
    StringNotAllowdCRLF { pos: usize },
    #[error("Invalid escape sequence.")]
    StringInvalidEscape { pos: usize },
    #[error("Missing closing brace '}}'.")]
    StringMissingCloseBrace { pos: usize },
    #[error("Expected closing brace '}}' after interpolation expression but found {got}.")]
    StringExpectedCloseBrace { got: char, pos: usize },
    #[error("Missing closing double quote to end string.")]
    StringMissingCloseQuote { pos: usize },
    #[error("Expected block.")]
    ExpectedBlock { pos: usize },
    #[error("Unexpected character. Expected new line after statement.")]
    ExpectedNewLineAfterStmt { pos: usize },
    #[error("Expected statement.")]
    ExpectedStmt { pos: usize },
    #[error("Expected else block or 'if' keyword.")]
    ExpectedBlockOrIf { pos: usize },
    #[error("Expected condition expression following if/else.")]
    ExpectedConditionExpression { pos: usize },
    #[error("Missing closing brace to end block.")]
    MissingClosingBrace { pos: usize },
    #[error("Invalid LHS of assignment.")]
    AssignmentInvalidLHS { span: Span },
    #[error("Expected 'in' after variable(s).")]
    LoopExpectedInKeyword { pos: usize },
    #[error("Missing closing ']'.")]
    MissingClosingBracket { pos: usize },
    #[error("Expected expression or ']'.")]
    ListExpectedExpressionOrCloseBracket { pos: usize },
    #[error("Expected ',' or ']'.")]
    ListExpectedCommaOrCloseBracket { pos: usize },
    #[error("Expected function body.")]
    FunctionExpectedBody { pos: usize },
    #[error("Expected '('.")]
    FunctionExpectedOpenParens { pos: usize },
    #[error("Expected parameter name.")]
    FunctionExpectedParamIdent { pos: usize },
    #[error("Expected ',' or ')'.")]
    FunctionExpectedCommaOrCloseParens { pos: usize },
}

impl SyntaxError {
    fn span(&self) -> Span {
        match self {
            SyntaxError::ExpectedVariableIdentifier { pos } => Span::new(*pos, 1),
            SyntaxError::DeclarationExpectedEquals { got: _, pos } => Span::new(*pos, 1),
            SyntaxError::DeclarationExpectedExpression { pos } => Span::new(*pos, 1),
            SyntaxError::ReservedKeyword { got: _, span } => *span,
            SyntaxError::ReservedName { got: _, span } => *span,
            SyntaxError::ExpectedWhitespaceOrComment { pos } => Span::new(*pos, 1),
            SyntaxError::DictExpectedOpenParens { pos } => Span::new(*pos, 1),
            SyntaxError::DictMissingCloseParens { pos } => Span::new(*pos, 1),
            SyntaxError::DictExpectedKeyOrCloseParens { pos } => Span::new(*pos, 1),
            SyntaxError::DictExpectedKeyValuePair { span } => *span,
            SyntaxError::DictExpectedCommaOrCloseParens { pos } => Span::new(*pos, 1),
            SyntaxError::KeyValueExpectsIdentOnLHS { span, pos: _ } => *span,
            SyntaxError::ExpectsExpression { pos } => Span::new(*pos, 1),
            SyntaxError::MissingClosingParens { pos } => Span::new(*pos, 1),
            SyntaxError::ExpectedClosingParens { got: _, pos } => Span::new(*pos, 1),
            SyntaxError::PropertyOfExpectedIdentifier { pos } => Span::new(*pos, 1),
            SyntaxError::IndexOfExpectedClosingBracket { got: _, pos } => Span::new(*pos, 1),
            SyntaxError::FunctionCallExpectedArgOrClosingParens { pos } => Span::new(*pos, 1),
            SyntaxError::FunctionCallExpectedCommaOrCloseParens { pos } => Span::new(*pos, 1),
            SyntaxError::StringNotAllowdCRLF { pos } => Span::new(*pos, 1),
            SyntaxError::StringInvalidEscape { pos } => Span::new(*pos, 1),
            SyntaxError::StringMissingCloseBrace { pos } => Span::new(*pos, 1),
            SyntaxError::StringExpectedCloseBrace { got: _, pos } => Span::new(*pos, 1),
            SyntaxError::StringMissingCloseQuote { pos } => Span::new(*pos, 1),
            SyntaxError::ExpectedBlock { pos } => Span::new(*pos, 1),
            SyntaxError::ExpectedNewLineAfterStmt { pos } => Span::new(*pos, 1),
            SyntaxError::ExpectedStmt { pos } => Span::new(*pos, 1),
            SyntaxError::ExpectedBlockOrIf { pos } => Span::new(*pos, 1),
            SyntaxError::ExpectedConditionExpression { pos } => Span::new(*pos, 1),
            SyntaxError::MissingClosingBrace { pos } => Span::new(*pos, 1),
            SyntaxError::AssignmentInvalidLHS { span } => *span,
            SyntaxError::LoopExpectedInKeyword { pos } => Span::new(*pos, 1),
            SyntaxError::MissingClosingBracket { pos } => Span::new(*pos, 1),
            SyntaxError::ListExpectedExpressionOrCloseBracket { pos } => Span::new(*pos, 1),
            SyntaxError::ListExpectedCommaOrCloseBracket { pos } => Span::new(*pos, 1),
            SyntaxError::FunctionExpectedBody { pos } => Span::new(*pos, 1),
            SyntaxError::FunctionExpectedOpenParens { pos } => Span::new(*pos, 1),
            SyntaxError::FunctionExpectedParamIdent { pos } => Span::new(*pos, 1),
            SyntaxError::FunctionExpectedCommaOrCloseParens { pos } => Span::new(*pos, 1),
        }
    }
}
