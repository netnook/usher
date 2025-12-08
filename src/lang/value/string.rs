use super::InternalProgramError;
use crate::lang::{
    Context, EvalStop, FunctionCall, Key, Value,
    function::{MethodResolver, MethodType},
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
    // FIXME: have an arg spec as a struct and a macro which can "decode" the args into that struct ?
    let mut on = None;
    for (idx, arg) in call.args.iter().enumerate() {
        match idx {
            0 => {
                let on_arg = call.required_positional_arg("on", call.args.first())?;
                let on_val = call.require_string("on", on_arg, ctxt)?;
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
    fn test_display() {
        {
            let val: Value = "the-string".into();
            let expected1 = "the-string";
            let expected2 = "\"the-string\"";
            assert_eq!(val.as_string().unwrap(), expected1);
            assert_eq!(format!("{val}"), expected2);
        }
        {
            let val: Value = "the-s\"tring".into();
            let expected1 = "the-s\"tring";
            let expected2 = "\"the-s\"tring\"";
            assert_eq!(val.as_string().unwrap(), expected1);
            assert_eq!(format!("{val}"), expected2);
        }
    }
}
