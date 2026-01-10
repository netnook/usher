use std::fs;
use usher::parser;
use usher::printer;

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

    let script = match fs::read_to_string(&file) {
        Ok(script) => script,
        Err(e) => {
            eprintln!("Error loading script: {e}");
            std::process::exit(101)
        }
    };

    let prog = match parser::parse(&file, &script) {
        Ok(prog) => prog,
        Err(e) => {
            if printer::print_parse_error(std::io::stderr(), e).is_err() {
                // Error writing to stderr
                std::process::exit(99)
            }
            std::process::exit(102)
        }
    };

    // println!("{prog:#?}");

    match prog.eval() {
        Ok(_) => {}
        Err(e) => {
            if printer::print_eval_error(std::io::stderr(), e).is_err() {
                // Error writing to stderr
                std::process::exit(99)
            }
            std::process::exit(1)
        }
    }
}
