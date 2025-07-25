use crate::parser::{self};
use pretty_assertions::assert_eq;

#[test]
fn test_programs() {
    let prog1 = include_str!("test_prog_1.usher");
    do_test_program_ok(prog1);
}

fn do_test_program_ok(src: &str) {
    let (input, expected) = src.split_once("---").expect("split ok");

    let actual = parser::parse(input).expect("parse ok");

    let printed = actual.print();

    assert_eq!(printed.trim(), expected.trim());
}
