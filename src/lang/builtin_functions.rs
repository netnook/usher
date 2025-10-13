use crate::lang::{Context, EvalStop, FunctionCall, Key, Output, Value, function::FunctionType};
use std::io::Write;

pub fn resolve_function(key: &Key) -> Option<FunctionType> {
    match key.as_str() {
        "print" => Some(builtin_print),
        "eprint" => Some(builtin_eprint),
        _ => None,
    }
}

fn builtin_print(call: &FunctionCall, ctxt: &mut Context) -> Result<Value, EvalStop> {
    let output = ctxt.get_stdout();
    builtin_do_print(call, ctxt, output)
}

fn builtin_eprint(call: &FunctionCall, ctxt: &mut Context) -> Result<Value, EvalStop> {
    let output = ctxt.get_stderr();
    builtin_do_print(call, ctxt, output)
}

fn builtin_do_print(
    call: &FunctionCall,
    ctxt: &mut Context,
    mut output: Output,
) -> Result<Value, EvalStop> {
    let mut first = true;

    let args = call
        .args
        .iter()
        .map(|a| a.eval(ctxt))
        .collect::<Result<Vec<Value>, EvalStop>>()?;

    for arg in args {
        if first {
            first = false;
        } else {
            // FIXME: default should be no separator, and have a named arg "sep=', '" for nicer formatting
            output.write_all(b", ").expect("FIXME")
        }
        let arg = arg.as_string()?;
        output.write_all(arg.as_bytes()).expect("FIXME")
    }

    output.write_all(b"\n").expect("FIXME");
    Ok(Value::Nil)
}
