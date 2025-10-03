use std::{error::Error, io::Write};

use crate::{
    lang::{EvalError, Span},
    parser::error::ParseError,
};

#[allow(dead_code)] // FIXME: should this be exported
pub fn print_parse_error_to_string(err: ParseError<'_>) -> String {
    let mut buf = Vec::new();
    print_parse_error(&mut buf, err).expect("write to string should not fail");
    String::from_utf8_lossy(&buf[..]).to_string()
}

pub(crate) fn print_parse_error(w: impl Write, err: ParseError<'_>) -> Result<(), std::io::Error> {
    let info = find_source_position(err.file, err.source, err.span());
    print_error(w, "Syntax error", info, err.error)?;

    Ok(())
}

#[allow(dead_code)] // FIXME: should this be exported
pub fn print_eval_error_to_string(err: EvalError) -> String {
    let mut buf = Vec::new();
    print_eval_error(&mut buf, err).expect("write to string should not fail");
    String::from_utf8_lossy(&buf[..]).to_string()
}

pub(crate) fn print_eval_error(w: impl Write, err: EvalError) -> Result<(), std::io::Error> {
    let info = find_source_position(err.file, err.source, err.span());
    print_error(w, "Evaluation error", info, err.error)?;

    Ok(())
}

fn print_error(
    mut w: impl Write,
    title: &'static str,
    info: SourceRef,
    err: impl Error,
) -> Result<(), std::io::Error> {
    match info {
        SourceRef::SingleLine {
            file,
            line,
            line_idx,
            start_char_idx,
            end_char_idx,
        } => {
            writeln!(
                w,
                "{title} in {file} at line {line}, char {char}:",
                line = line_idx + 1,
                char = start_char_idx + 1
            )?;

            let line_num = format!("{}", line_idx + 1);
            let padding = " ".repeat(line_num.len());
            let space = " ".repeat(start_char_idx);
            let underline = "^".repeat(end_char_idx - start_char_idx);

            writeln!(w, "{padding} |")?;
            writeln!(w, "{line_num} | {line}")?;
            writeln!(w, "{padding} | {space}{underline} {err}")?;
        }
        SourceRef::MultiLine {
            file,
            start_line,
            start_line_idx,
            start_char_idx,
            end_line,
            end_line_idx,
            end_char_idx: _,
        } => {
            writeln!(
                w,
                "Syntax error in {file} at line {line}, char {char}:",
                line = start_line_idx + 1,
                char = start_char_idx + 1
            )?;

            let start_line_num = format!("{:4}", start_line_idx + 1);
            let end_line_num = format!("{:4}", end_line_idx + 1);
            let padding = " ".repeat(start_line_num.len().max(end_line_num.len()));
            let start_line_space = " ".repeat(start_char_idx);
            let end_line_space = " ".repeat(start_char_idx);
            let underline = "^^^ FIXME".to_string();

            writeln!(w, "{padding} |")?;
            writeln!(w, "{start_line_num} | {start_line}")?;
            writeln!(w, "{padding} | {start_line_space}{underline}")?;
            writeln!(w, "{end_line_num} | {end_line}")?;
            writeln!(w, "{padding} | {end_line_space}{underline}")?;
            writeln!(w, "{padding} | -- {err}")?;
        }
    }

    Ok(())
}

#[derive(Debug, PartialEq)]
enum SourceRef<'a> {
    SingleLine {
        file: &'a str,
        line: &'a str,
        line_idx: usize,
        start_char_idx: usize,
        end_char_idx: usize,
    },
    MultiLine {
        file: &'a str,
        start_line: &'a str,
        start_line_idx: usize,
        start_char_idx: usize,
        end_line: &'a str,
        end_line_idx: usize,
        end_char_idx: usize,
    },
}

fn find_source_position<'a>(file: &'a str, source: &'a str, span: Span) -> SourceRef<'a> {
    let bytes = source.as_bytes();

    let span_start = scan_to_pos(bytes, 0, span.start);
    let span_end = {
        if span.len == 0 {
            ScanToPosResult {
                line_count: 0,
                ..span_start
            }
        } else {
            scan_to_pos(bytes, span.start, span.end())
        }
    };

    if span_end.line_count == 0 {
        let line_end = scan_to_newline(bytes, span.end());
        SourceRef::SingleLine {
            file,
            line: &source[span_start.last_line_start..line_end],
            line_idx: span_start.line_count,
            start_char_idx: span.start - span_start.last_line_start,
            end_char_idx: span.end() - span_start.last_line_start,
        }
    } else {
        let start_line_end = scan_to_newline(bytes, span.start);
        let end_line_end = scan_to_newline(bytes, span.end());
        SourceRef::MultiLine {
            file,
            start_line: &source[span_start.last_line_start..start_line_end],
            start_line_idx: span_start.line_count,
            start_char_idx: span.start - span_start.last_line_start,
            end_line: &source[span_end.last_line_start..end_line_end],
            end_line_idx: span_start.line_count + span_end.line_count,
            end_char_idx: span.end() - span_end.last_line_start,
        }
    }
}

fn scan_to_pos(input: &[u8], start_pos: usize, end_pos: usize) -> ScanToPosResult {
    // let mut pos = start_pos;

    let mut line_count = 0;
    let mut last_line_start = 0;

    let mut previous_c = 0;

    for pos in start_pos.. {
        if pos >= input.len() {
            break;
        }
        if pos >= end_pos {
            break;
        }

        let c = input[pos];

        if c == b'\r' {
            line_count += 1;
            last_line_start = pos + 1;
        } else if c == b'\n' {
            if previous_c != b'\r' {
                line_count += 1;
            }
            last_line_start = pos + 1;
        }

        previous_c = c;
    }

    ScanToPosResult {
        line_count,
        last_line_start,
    }
}

#[derive(Clone, Debug)]
struct ScanToPosResult {
    line_count: usize,
    last_line_start: usize,
}

fn scan_to_newline(input: &[u8], start_pos: usize) -> usize {
    for pos in start_pos.. {
        if pos >= input.len() {
            return pos;
        }

        let c = input[pos];

        if c == b'\r' || c == b'\n' {
            return pos;
        }
    }
    unreachable!()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    // FIXME: fix these tests
    // fn sp(line: usize, char: usize) -> SourceRef {
    //     SourceRef { line, char }
    // }

    // #[test]
    // fn test_build_parse_error() {
    //     const INPUT: &str =
    //         "line 0 \n line 1 \r\n line 2 \n\n\n line 5 \r\r\r line 8 \r\n\n\r\n line 11 ";

    //     assert_eq!(
    //         find_source_position(INPUT, INPUT.find("line 0").unwrap()),
    //         (sp(0, 0), "line 0 ")
    //     );
    //     assert_eq!(
    //         find_source_position(INPUT, INPUT.find("line 1").unwrap()),
    //         (sp(1, 1), " line 1 ")
    //     );
    //     assert_eq!(
    //         find_source_position(INPUT, INPUT.find("line 2").unwrap()),
    //         (sp(2, 1), " line 2 ")
    //     );
    //     assert_eq!(
    //         find_source_position(INPUT, INPUT.find(" line 5").unwrap()),
    //         (sp(5, 0), " line 5 ")
    //     );
    //     assert_eq!(
    //         find_source_position(INPUT, INPUT.find(" line 5").unwrap() + 7),
    //         (sp(5, 7), " line 5 ")
    //     );
    //     assert_eq!(
    //         find_source_position(INPUT, INPUT.find(" line 5").unwrap() + 8),
    //         (sp(5, 8), " line 5 ")
    //     );
    //     assert_eq!(
    //         find_source_position(INPUT, INPUT.find(" line 11").unwrap()),
    //         (sp(11, 0), " line 11 ")
    //     );
    // }
}
