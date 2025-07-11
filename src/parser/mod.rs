mod bool;
mod chars;
mod comment;
mod nil;
mod numbers;
mod string;

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError<'a> {
    pub line_no: usize,
    pub char_no: usize,
    pub line: &'a str,
    pub msg: &'static str,
}

pub fn parse(input: &str) -> Result<(), ParseError> {
    let mut p = Parser::new(input);

    p.program().expect("FIXME");

    todo!();
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct SyntaxError {
    pos: usize,
    msg: &'static str,
}

impl SyntaxError {
    fn new(pos: usize, msg: &'static str) -> Self {
        Self { pos, msg }
    }
}

type ParseResult<T> = Result<T, SyntaxError>;

#[derive(Debug)]
pub struct Parser<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    fn program(&mut self) -> ParseResult<()> {
        // FIXME dummy code
        self.char(b'a');
        self.tag(b"aaa");
        self.repeat(|_| false);
        self.integer();
        self.float();
        self.boolean();
        self.nil();
        self.string().unwrap();
        self.is_eoi();
        self.is_eoi();
        self.whitespace();
        self.linespace();
        self.comment();
        self.whitespace_comments();
        let _ = self.req_whitespace_comments();

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::Value;

    pub(crate) fn s(v: &str) -> Value {
        Value::Str(v.to_string())
    }
    pub(crate) fn i(v: isize) -> Value {
        Value::Integer(v)
    }
    pub(crate) fn b(v: bool) -> Value {
        Value::Bool(v)
    }
    pub(crate) fn nil() -> Value {
        Value::Nil
    }
}
