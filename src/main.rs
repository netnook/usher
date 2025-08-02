use parser::error::find_source_position;
use std::fs;

mod lang;
mod parser;

fn main() {
    let mut args = std::env::args();

    // Skip usher binary itself
    if args.next().is_none() {
        eprintln!("Missing ARGS");
        std::process::exit(100)
    };

    // Get first arg
    let Some(file) = args.next() else {
        eprintln!("Expected 1 argument: FILE");
        std::process::exit(100)
    };

    // Check no other args
    if args.next().is_some() {
        eprintln!("Expected 1 argument: FILE");
        std::process::exit(100)
    }

    let script = match fs::read_to_string(file) {
        Ok(script) => script,
        Err(e) => {
            eprintln!("Error loading script: {e}");
            std::process::exit(101)
        }
    };

    let prog = match parser::parse(&script) {
        Ok(prog) => prog,
        Err(e) => {
            eprintln!();
            eprintln!(
                "Error parsing script at line {}, char {}: {}",
                e.line_no, e.char_no, e.msg
            );
            eprintln!();
            eprintln!("> {}", e.line);
            eprintln!("> {}^", " ".repeat(e.char_no));

            std::process::exit(102)
        }
    };

    match prog.run() {
        Ok(_) => {}
        Err(e) => {
            eprintln!(
                "Program error at line {}, char {}: {}",
                e.line_no, e.char_no, e.msg
            );
            eprintln!();
            eprintln!("> {}", e.line);
            eprintln!("> {}^", " ".repeat(e.char_no));
            std::process::exit(1)
        }
    }
}
