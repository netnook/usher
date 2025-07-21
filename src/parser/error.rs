use super::SyntaxError;

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError<'a> {
    pub line_no: usize,
    pub char_no: usize,
    pub line: &'a str,
    pub msg: &'static str,
}

pub(super) fn build_parse_error(source: &str, se: SyntaxError) -> ParseError {
    let mut counter = 0;

    let mut line_no = 0;
    let mut char_no = 0;
    let mut line_start = 0;
    let mut line_end = source.len();

    let mut last = 0;
    for c in source.as_bytes() {
        counter += 1;

        let c = *c;

        if counter <= se.pos {
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

    ParseError {
        line_no,
        char_no,
        line: &source[line_start..line_end],
        msg: se.msg,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn se(pos: usize) -> SyntaxError {
        SyntaxError { pos, msg: "foo" }
    }
    fn pe<'a>(line_no: usize, char_no: usize, line: &'a str, msg: &'static str) -> ParseError<'a> {
        ParseError {
            line_no,
            char_no,
            line,
            msg,
        }
    }

    #[test]
    fn test_build_parse_error() {
        const INPUT: &str =
            "line 0 \n line 1 \r\n line 2 \n\n\n line 5 \r\r\r line 8 \r\n\n\r\n line 11 ";
        assert_eq!(
            build_parse_error(INPUT, se(INPUT.find("line 0").unwrap())),
            pe(0, 0, "line 0 ", "foo")
        );

        assert_eq!(
            build_parse_error(INPUT, se(INPUT.find("line 1").unwrap())),
            pe(1, 1, " line 1 ", "foo")
        );

        assert_eq!(
            build_parse_error(INPUT, se(INPUT.find("line 2").unwrap())),
            pe(2, 1, " line 2 ", "foo")
        );
        assert_eq!(
            build_parse_error(INPUT, se(INPUT.find(" line 5").unwrap())),
            pe(5, 0, " line 5 ", "foo")
        );
        assert_eq!(
            build_parse_error(INPUT, se(INPUT.find(" line 5").unwrap() + 7)),
            pe(5, 7, " line 5 ", "foo")
        );
        assert_eq!(
            build_parse_error(INPUT, se(INPUT.find(" line 5").unwrap() + 8)),
            pe(5, 8, " line 5 ", "foo")
        );
        assert_eq!(
            build_parse_error(INPUT, se(INPUT.find(" line 11").unwrap())),
            pe(11, 0, " line 11 ", "foo")
        );
    }
}
