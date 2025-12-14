use crate::lang::{Context, EvalStop, FunctionCall, Key, Output, Value, function::FunctionType};
use std::{io::Write, rc::Rc};

use super::{InternalProgramError, value::ValueType};

pub fn resolve_function(key: &Key) -> Option<FunctionType> {
    match key.as_str() {
        "print" => Some(builtin_print),
        "eprint" => Some(builtin_eprint),
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
    let mut first = true;

    let mut values = Vec::with_capacity(call.args.len());
    let mut sep = None;

    for a in &call.args {
        let val = a.value.eval(ctxt)?;
        if let Some(name) = &a.name {
            if name.key.as_str() == "sep" {
                sep = match val {
                    Value::Str(s) => Some(s),
                    Value::Nil => Some(Rc::new(String::new())),
                    _ => {
                        return Err(InternalProgramError::BadValueType {
                            expected: ValueType::String,
                            actual: val.value_type(),
                            span: a.span(),
                        }
                        .into_stop());
                    }
                };
            } else {
                return Err(InternalProgramError::FunctionCallNoSuchParameter {
                    name: name.as_string(),
                    span: a.span(),
                }
                .into_stop());
            }
        } else {
            values.push(val);
        }
    }

    for val in values {
        if first {
            first = false;
        } else {
            let s = match &sep {
                Some(sep) => sep.as_str(),
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
