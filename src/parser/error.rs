use crate::lang::Span;
use thiserror::Error;

#[derive(PartialEq)]
pub struct ParseError<'a> {
    pub file: &'a str,
    pub source: &'a str,
    pub error: SyntaxError,
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

impl<'a> ParseError<'a> {
    pub fn to_display(&self) -> String {
        let (sp, line) = find_source_position(self.source, self.error.start());

        let mut result = String::new();
        result.push_str(&format!(
            "Syntax error in {file} at line {line}, char {char}:\n",
            file = self.file,
            line = sp.line + 1,
            char = sp.char + 1
        ));

        let line_num = format!("{}", sp.line + 1);

        result.push_str(&" ".repeat(line_num.len()));
        result.push_str(" |\n");

        result.push_str(&line_num);
        result.push_str(" | ");
        result.push_str(line);
        result.push('\n');

        result.push_str(&" ".repeat(line_num.len()));
        result.push_str(" | ");

        result.reserve(sp.char);
        for _ in 0..sp.char {
            result.push(' ');
        }
        // result.push_str("^ ");
        result.push_str("└ ");
        result.push_str(&format!("{}", self.error));
        result.push('\n');

        result
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

#[derive(Debug, PartialEq, Eq)]
pub struct SourcePos {
    pub line: usize,
    pub char: usize,
}

// FIXME: inline this method.
pub(super) fn build_parse_error<'a>(
    file: &'a str,
    source: &'a str,
    se: SyntaxError,
) -> ParseError<'a> {
    ParseError {
        file,
        source,
        error: se,
    }
}

pub(crate) fn find_source_position(source: &str, pos: usize) -> (SourcePos, &str) {
    let mut counter = 0;

    let mut line_no = 0;
    let mut char_no = 0;
    let mut line_start = 0;
    let mut line_end = source.len();

    let mut last = 0;
    for c in source.as_bytes() {
        counter += 1;

        let c = *c;

        if counter <= pos {
            if c == b'\r' {
                line_no += 1;
                char_no = 0;
                line_start = counter;
            } else if c == b'\n' {
                if last != b'\r' {
                    line_no += 1;
                    char_no = 0;
                }
                line_start = counter;
            } else {
                char_no += 1;
            }
        } else if c == b'\r' || c == b'\n' {
            line_end = counter - 1;
            break;
        }

        last = c;
    }
    (
        SourcePos {
            line: line_no,
            char: char_no,
        },
        &source[line_start..line_end],
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn sp(line: usize, char: usize) -> SourcePos {
        SourcePos { line, char }
    }

    #[test]
    fn test_build_parse_error() {
        const INPUT: &str =
            "line 0 \n line 1 \r\n line 2 \n\n\n line 5 \r\r\r line 8 \r\n\n\r\n line 11 ";

        assert_eq!(
            find_source_position(INPUT, INPUT.find("line 0").unwrap()),
            (sp(0, 0), "line 0 ")
        );
        assert_eq!(
            find_source_position(INPUT, INPUT.find("line 1").unwrap()),
            (sp(1, 1), " line 1 ")
        );
        assert_eq!(
            find_source_position(INPUT, INPUT.find("line 2").unwrap()),
            (sp(2, 1), " line 2 ")
        );
        assert_eq!(
            find_source_position(INPUT, INPUT.find(" line 5").unwrap()),
            (sp(5, 0), " line 5 ")
        );
        assert_eq!(
            find_source_position(INPUT, INPUT.find(" line 5").unwrap() + 7),
            (sp(5, 7), " line 5 ")
        );
        assert_eq!(
            find_source_position(INPUT, INPUT.find(" line 5").unwrap() + 8),
            (sp(5, 8), " line 5 ")
        );
        assert_eq!(
            find_source_position(INPUT, INPUT.find(" line 11").unwrap()),
            (sp(11, 0), " line 11 ")
        );
    }
}
