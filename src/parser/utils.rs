use super::{ParseResult, Parser, SyntaxError};

impl<'a> Parser<'a> {
    /// Return the `Some` value of the `inner` parser or a `SyntaxError`
    pub(super) fn req<P, R>(&mut self, mut inner: P, msg: &'static str) -> ParseResult<R>
    where
        P: FnMut(&mut Self) -> ParseResult<Option<R>>,
    {
        let start = self.pos;

        match inner(self)? {
            Some(res) => Ok(res),
            None => Err(SyntaxError::new(start, msg)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lang::Value,
        parser::{Parser, SyntaxError},
    };

    #[test]
    fn test_req_with_result() {
        let mut p = Parser::new(r#"-"xxx"-"#);
        p.pos = 1;

        assert_eq!(
            p.req(Parser::string, "the-error"),
            Ok(Value::Str("xxx".to_string()).into())
        );
        assert_eq!(p.pos, 6);
    }

    #[test]
    fn test_req_with_none() {
        let mut p = Parser::new(r#"-xxxx-"#);
        p.pos = 1;

        assert_eq!(
            p.req(Parser::string, "the-error"),
            Err(SyntaxError::new(1, "the-error"))
        );
    }

    #[test]
    fn test_req_with_inner_error() {
        let mut p = Parser::new(r#"-"bad-string-no-closing-quote"#);
        p.pos = 1;

        assert_eq!(
            p.req(Parser::string, "the-error"),
            Err(SyntaxError::new(
                1,
                crate::parser::string::MISSING_END_QUOTE
            ))
        );
    }
}
