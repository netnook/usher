use super::InternalProgramError;
use crate::lang::{
    Context, EvalStop, FunctionCall, Key, Value,
    function::{MethodResolver, MethodType},
    value::ValueType,
};
use std::rc::Rc;

pub type StringCell = Rc<String>;

impl MethodResolver for StringCell {
    fn resolve_method(&self, key: &Key) -> Option<MethodType<Self>> {
        match key.as_str() {
            "len" => Some(str_len),
            "split" => Some(str_split),
            _ => None,
        }
    }
}

fn str_len(call: &FunctionCall, this: StringCell, _ctxt: &mut Context) -> Result<Value, EvalStop> {
    if let Some(arg) = call.args.first() {
        return Err(
            InternalProgramError::FunctionCallUnexpectedArgument { span: arg.span() }.into(),
        );
    }
    Ok(Value::Integer(this.len() as isize))
}

fn str_split(call: &FunctionCall, this: StringCell, ctxt: &mut Context) -> Result<Value, EvalStop> {
    let mut on = None;
    for (idx, arg) in call.args.iter().enumerate() {
        match idx {
            0 => {
                // FIXME: refactor this argument handling into a common utility
                let Some(on_arg) = call.args.first() else {
                    return Err(InternalProgramError::FunctionCallMissingRequiredArgument {
                        name: "on".to_string(),
                        span: call.span,
                    }
                    .into());
                };
                let on_val = on_arg.eval(ctxt)?;
                let Value::Str(on_val) = on_val else {
                    return Err(InternalProgramError::FunctionCallBadArgType {
                        name: "on".to_string(),
                        expected: ValueType::String,
                        actual: on_val.value_type(),
                        span: on_arg.span(),
                    }
                    .into());
                };
                on = Some(on_val);
            }
            _ => {
                return Err(InternalProgramError::FunctionCallUnexpectedArgument {
                    span: arg.span(),
                }
                .into());
            }
        }
    }

    let Some(on) = on else {
        return Err(InternalProgramError::FunctionCallMissingRequiredArgument {
            name: "on".to_string(),
            span: call.span,
        }
        .into());
    };

    let mut result = Vec::new();
    for part in this.split(on.as_str()) {
        result.push(part.into());
    }
    Ok(result.into())
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::lang::Value;
    use crate::lang::value::tests::{do_test_as_string, do_test_debug_str};
    use crate::parser::tests::ToValue;

    #[test]
    fn test_ref_clone() {
        {
            let a = Value::Str(Rc::new("string".to_string()));
            let b = a.ref_clone();
            assert_eq!(a, b);

            let Value::Str(mut v) = a else { panic!() };
            if Rc::get_mut(&mut v).is_some() {
                panic!("expected none");
            };
        }
    }

    #[test]
    fn test_shallow_clone() {
        {
            let a = Value::Str(Rc::new("string".to_string()));
            let b = a.shallow_clone();
            assert_eq!(a, b);

            let Value::Str(mut v) = a else { panic!() };
            Rc::get_mut(&mut v).unwrap().push_str("xxx");
            let a = Value::Str(v);
            assert_ne!(a, b);
        }
    }

    #[test]
    fn test_deep_clone() {
        {
            let a = Value::Str(Rc::new("string".to_string()));
            let b = a.deep_clone();
            assert_eq!(a, b);

            let Value::Str(mut v) = a else { panic!() };
            Rc::get_mut(&mut v).unwrap().push_str("xxx");
            let a = Value::Str(v);
            assert_ne!(a, b);
        }
    }

    #[test]
    fn test_value_display() {
        do_test_as_string("the-string".to_value(), "the-string");
        do_test_as_string("the-s\"tring".to_value(), "the-s\"tring");
    }

    #[test]
    fn test_debug_str() {
        do_test_debug_str("abc".to_value(), r#""abc""#);
    }
}
