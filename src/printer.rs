use crate::{
    lang::EvalError,
    parser::{
        SourceRef,
        error::{ParseError, ParseErrorCause},
    },
};
use std::{error::Error, io::Write};

pub fn print_parse_error(w: impl Write, err: ParseError<'_>) -> Result<(), std::io::Error> {
    let info = err.find_source_position();
    let title = match err.cause {
        ParseErrorCause::SyntaxError(_) => "Syntax error",
        ParseErrorCause::SemanticError(_) => "Semantic error",
    };
    print_error(w, title, info, err.cause)?;

    Ok(())
}

pub fn print_eval_error(w: impl Write, err: EvalError) -> Result<(), std::io::Error> {
    let info = err.find_source_position();
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
