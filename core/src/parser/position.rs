use crate::lang::Span;

#[derive(Debug, PartialEq)]
pub enum SourceRef<'a> {
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

pub(crate) fn find_source_position<'a>(
    file: &'a str,
    source: &'a str,
    span: Span,
) -> SourceRef<'a> {
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

    #[test]
    fn test_find_source_position() {
        const FILE: &str = "the-file";
        const INPUT: &str =
            "line 0 \n line 1 \r\n line 2 \n\n\n line 5 \r\r\r line 8 \r\n\n\r\n line 11 ";

        assert_eq!(
            find_source_position(
                FILE,
                INPUT,
                Span::new(INPUT.find("line 0").unwrap(), "line 0".len())
            ),
            SourceRef::SingleLine {
                file: FILE,
                line: "line 0 ",
                line_idx: 0,
                start_char_idx: 0,
                end_char_idx: 6,
            }
        );
        assert_eq!(
            find_source_position(FILE, INPUT, Span::new(INPUT.find("line 1").unwrap() + 1, 1)),
            SourceRef::SingleLine {
                file: FILE,
                line: " line 1 ",
                line_idx: 1,
                start_char_idx: 2,
                end_char_idx: 3,
            }
        );
        assert_eq!(
            find_source_position(FILE, INPUT, Span::new(INPUT.find("ne 5").unwrap(), 4)),
            SourceRef::SingleLine {
                file: FILE,
                line: " line 5 ",
                line_idx: 5,
                start_char_idx: 3,
                end_char_idx: 7,
            }
        );
        assert_eq!(
            find_source_position(FILE, INPUT, Span::new(INPUT.find("ne 5").unwrap(), 7)),
            SourceRef::MultiLine {
                file: FILE,
                start_line: " line 5 ",
                start_line_idx: 5,
                start_char_idx: 3,
                end_line: "",
                end_line_idx: 7,
                end_char_idx: 0,
            }
        );
        assert_eq!(
            find_source_position(FILE, INPUT, Span::new(INPUT.find("ne 5").unwrap(), 9)),
            SourceRef::MultiLine {
                file: FILE,
                start_line: " line 5 ",
                start_line_idx: 5,
                start_char_idx: 3,
                end_line: " line 8 ",
                end_line_idx: 8,
                end_char_idx: 1,
            }
        );
        assert_eq!(
            find_source_position(FILE, INPUT, Span::new(INPUT.find("11").unwrap(), 3)),
            SourceRef::SingleLine {
                file: FILE,
                line: " line 11 ",
                line_idx: 11,
                start_char_idx: 6,
                end_char_idx: 9,
            }
        );
    }
}
