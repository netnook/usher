use super::InternalProgramError;
use crate::lang::{
    Arg, Context, EvalStop, FunctionCall, Key, Value,
    function::{MethodResolver, MethodType, args_struct},
};
use std::{fmt::Display, ops::Deref, rc::Rc};

#[derive(Debug, PartialEq, Default, Clone)]
pub struct StringCell {
    pub(crate) content: Rc<String>,
}

impl StringCell {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.content.len()
    }

    pub(crate) fn as_str(&self) -> &str {
        self.content.as_str()
    }

    pub(crate) fn shallow_clone(&self) -> Self {
        Self {
            // FIXME: are strings immutable, in which case the clone is un-necessary
            content: Rc::new(self.content.as_str().to_string()),
        }
    }

    pub(crate) fn deep_clone(&self) -> Self {
        Self {
            // FIXME: are strings immutable, in which case the clone is un-necessary
            content: Rc::new(self.content.as_str().to_string()),
        }
    }
}

impl Deref for StringCell {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.content.as_str()
    }
}

impl From<&Key> for StringCell {
    fn from(value: &Key) -> Self {
        Self {
            content: value.0.clone(),
        }
    }
}
impl From<Key> for StringCell {
    fn from(value: Key) -> Self {
        Self { content: value.0 }
    }
}

impl From<&str> for StringCell {
    fn from(value: &str) -> Self {
        Self {
            content: Rc::new(value.to_string()),
        }
    }
}

impl From<String> for StringCell {
    fn from(value: String) -> Self {
        Self {
            content: Rc::new(value),
        }
    }
}

impl Display for StringCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.content)
        // todo!()
    }
}

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
    args_struct!(Args, arg(on, 0, string, required));

    let args = Args::new(call, ctxt)?;

    let mut result = Vec::new();
    for part in this.split(args.on.as_str()) {
        result.push(part.into());
    }
    Ok(result.into())
}

#[cfg(test)]
pub mod tests {
    use crate::lang::Value;

    #[test]
    fn test_ref_clone() {
        {
            let a = Value::Str("string".into());
            let b = a.ref_clone();
            assert_eq!(a, b);
        }
    }

    #[test]
    fn test_shallow_clone() {
        {
            let a = Value::Str("string".into());
            let b = a.shallow_clone();
            assert_eq!(a, b);
        }
    }

    #[test]
    fn test_deep_clone() {
        {
            let a = Value::Str("string".into());
            let b = a.deep_clone();
            assert_eq!(a, b);
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
