use super::InternalProgramError;
use crate::lang::{
    Arg, Context, EvalStop, FunctionCall, Key, Output, Value,
    function::{FunctionType, MaybeNil, args_struct},
};
use std::io::Write;

pub fn resolve_function(key: &Key) -> Option<FunctionType> {
    match key.as_str() {
        "print" => Some(builtin_print),
        "eprint" => Some(builtin_eprint),
        #[cfg(test)]
        "test_next_id" => Some(builtin_test_next_id),
        _ => None,
    }
}

fn builtin_print(call: &FunctionCall, ctxt: &mut Context) -> Result<Value, EvalStop> {
    let output = ctxt.get_stdout();
    builtin_do_print(call, ctxt, output, |e| {
        EvalStop::Error(InternalProgramError::StdoutWriteError {
            cause: format!("{e}"),
            span: call.span,
        })
    })
}

fn builtin_eprint(call: &FunctionCall, ctxt: &mut Context) -> Result<Value, EvalStop> {
    let output = ctxt.get_stderr();
    builtin_do_print(call, ctxt, output, |e| {
        EvalStop::Error(InternalProgramError::StderrWriteError {
            cause: format!("{e}"),
            span: call.span,
        })
    })
}

fn builtin_do_print<T>(
    call: &FunctionCall,
    ctxt: &mut Context,
    mut output: Output,
    error_mapper: T,
) -> Result<Value, EvalStop>
where
    T: Fn(std::io::Error) -> EvalStop,
{
    args_struct!(
        Args,
        arg(sep, -, string|nil, optional),
        arg(values, *, any)
    );

    let args = Args::new(call, ctxt)?;

    let mut first = true;
    for val in args.values {
        if first {
            first = false;
        } else {
            let s = match &args.sep {
                Some(MaybeNil::Some(sep)) => sep.as_str(),
                Some(MaybeNil::Nil) => "",
                None => ", ",
            };
            if !s.is_empty() {
                output.write_all(s.as_bytes()).map_err(&error_mapper)?;
            }
        }

        let arg = val
            .as_string()
            .expect("write value to string should succeed");

        output.write_all(arg.as_bytes()).map_err(&error_mapper)?;
    }

    output.write_all(b"\n").map_err(error_mapper)?;
    Ok(Value::Nil)
}

#[cfg(test)]
fn builtin_test_next_id(_call: &FunctionCall, ctxt: &mut Context) -> Result<Value, EvalStop> {
    let key = Key::new("test_next_id_val".to_string());

    match ctxt.get(&key) {
        Some(v @ Value::Integer(next_id)) => {
            ctxt.set(&key, Value::Integer(next_id + 1)).unwrap();
            Ok(v)
        }
        _ => {
            let v = Value::Integer(0);
            ctxt.declare(key, Value::Integer(1)).unwrap();
            Ok(v)
        }
    }
}
