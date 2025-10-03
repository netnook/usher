use crate::lang::Span;
use thiserror::Error;

#[derive(PartialEq)]
pub struct ParseError<'a> {
    pub file: &'a str,
    pub source: &'a str,
    pub error: SyntaxError,
}

impl<'a> ParseError<'a> {
    pub(crate) fn start(&self) -> usize {
        self.error.start()
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
    fn start(&self) -> usize {
        match self {
            SyntaxError::ExpectedVariableIdentifier { pos } => *pos,
            SyntaxError::DeclarationExpectedEquals { got: _, pos } => *pos,
            SyntaxError::DeclarationExpectedExpression { pos } => *pos,
            SyntaxError::ReservedKeyword { got: _, span } => span.start,
            SyntaxError::ReservedName { got: _, span } => span.start,
            SyntaxError::ExpectedWhitespaceOrComment { pos } => *pos,
            SyntaxError::DictExpectedOpenParens { pos } => *pos,
            SyntaxError::DictMissingCloseParens { pos } => *pos,
            SyntaxError::DictExpectedKeyOrCloseParens { pos } => *pos,
            SyntaxError::DictExpectedKeyValuePair { span } => span.start,
            SyntaxError::DictExpectedCommaOrCloseParens { pos } => *pos,
            SyntaxError::KeyValueExpectsIdentOnLHS { span, pos: _ } => span.start,
            SyntaxError::ExpectsExpression { pos } => *pos,
            SyntaxError::MissingClosingParens { pos } => *pos,
            SyntaxError::ExpectedClosingParens { got: _, pos } => *pos,
            SyntaxError::PropertyOfExpectedIdentifier { pos } => *pos,
            SyntaxError::IndexOfExpectedClosingBracket { got: _, pos } => *pos,
            SyntaxError::FunctionCallExpectedArgOrClosingParens { pos } => *pos,
            SyntaxError::FunctionCallExpectedCommaOrCloseParens { pos } => *pos,
            SyntaxError::StringNotAllowdCRLF { pos } => *pos,
            SyntaxError::StringInvalidEscape { pos } => *pos,
            SyntaxError::StringMissingCloseBrace { pos } => *pos,
            SyntaxError::StringExpectedCloseBrace { got: _, pos } => *pos,
            SyntaxError::StringMissingCloseQuote { pos } => *pos,
            SyntaxError::ExpectedBlock { pos } => *pos,
            SyntaxError::ExpectedNewLineAfterStmt { pos } => *pos,
            SyntaxError::ExpectedStmt { pos } => *pos,
            SyntaxError::ExpectedBlockOrIf { pos } => *pos,
            SyntaxError::ExpectedConditionExpression { pos } => *pos,
            SyntaxError::MissingClosingBrace { pos } => *pos,
            SyntaxError::AssignmentInvalidLHS { span } => span.start,
            SyntaxError::LoopExpectedInKeyword { pos } => *pos,
            SyntaxError::MissingClosingBracket { pos } => *pos,
            SyntaxError::ListExpectedExpressionOrCloseBracket { pos } => *pos,
            SyntaxError::ListExpectedCommaOrCloseBracket { pos } => *pos,
            SyntaxError::FunctionExpectedBody { pos } => *pos,
            SyntaxError::FunctionExpectedOpenParens { pos } => *pos,
            SyntaxError::FunctionExpectedParamIdent { pos } => *pos,
            SyntaxError::FunctionExpectedCommaOrCloseParens { pos } => *pos,
        }
    }
}
