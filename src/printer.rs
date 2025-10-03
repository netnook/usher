use std::io::Write;

use crate::{lang::EvalError, parser::error::ParseError};

#[allow(dead_code)] // FIXME: should this be exported
pub fn print_parse_error_to_string(err: ParseError<'_>) -> String {
    let mut buf = Vec::new();
    print_parse_error(&mut buf, err).expect("write to string should not fail");
    String::from_utf8_lossy(&buf[..]).to_string()
}

pub(crate) fn print_parse_error(
    mut w: impl Write,
    err: ParseError<'_>,
) -> Result<(), std::io::Error> {
    let (sp, line) = find_source_position(err.source, err.start());

    writeln!(
        w,
        "Syntax error in {file} at line {line}, char {char}:",
        file = err.file,
        line = sp.line + 1,
        char = sp.char + 1
    )?;

    let line_num = format!("{}", sp.line + 1);
    let padding = " ".repeat(line_num.len());
    let space = " ".repeat(sp.char);

    writeln!(w, "{padding} |")?;
    writeln!(w, "{line_num} | {line}")?;
    writeln!(w, "{padding} | {space}└ {error}", error = err.error)?;
    // writeln!(w, "{padding} | {space}^ {error}", error = err.error)?;

    Ok(())
}

#[allow(dead_code)] // FIXME: should this be exported
pub fn print_eval_error_to_string(err: EvalError) -> String {
    let mut buf = Vec::new();
    print_eval_error(&mut buf, err).expect("write to string should not fail");
    String::from_utf8_lossy(&buf[..]).to_string()
}

pub(crate) fn print_eval_error(mut w: impl Write, err: EvalError) -> Result<(), std::io::Error> {
    let (sp, line) = find_source_position(err.source, err.start());

    writeln!(
        w,
        "Evaluation error in {file} at line {line}, char {char}:",
        file = err.file,
        line = sp.line + 1,
        char = sp.char + 1
    )?;

    let line_num = format!("{}", sp.line + 1);
    let padding = " ".repeat(line_num.len());
    let space = " ".repeat(sp.char);

    writeln!(w, "{padding} |")?;
    writeln!(w, "{line_num} | {line}")?;
    writeln!(w, "{padding} | {space}└ {error}", error = err.error)?;
    // writeln!(w, "{padding} | {space}^ {error}", error = err.error)?;

    Ok(())
}

#[derive(Debug, PartialEq, Eq)]
pub struct SourcePos {
    pub line: usize,
    pub char: usize,
}

fn find_source_position(source: &str, pos: usize) -> (SourcePos, &str) {
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
