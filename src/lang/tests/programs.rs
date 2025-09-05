use crate::parser::{self};
use pretty_assertions::assert_eq;

#[test]
fn test_prog1() {
    do_test_program_ok(include_str!("prog1.usher"));
}

#[test]
fn test_prog2() {
    do_test_program_ok(include_str!("prog2.usher"));
}

#[track_caller]
fn do_test_program_ok(src: &str) {
    let (input, expected) = src.split_once("---").expect("split ok");

    let prog = parser::parse(input).expect("parse ok");

    let printed = match prog.run() {
        Ok(ok) => ok.print(),
        Err(err) => err.print(),
    };

    assert_eq!(printed.trim(), expected.trim());
}
