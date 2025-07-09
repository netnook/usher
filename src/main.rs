mod lang;
mod parser;

use lang::Value;

fn main() {
    // FIXME silence unused code warnings
    let _ = Value::Str("x".to_string());
    parser::parse("FIXME").expect("FIXME");
}
