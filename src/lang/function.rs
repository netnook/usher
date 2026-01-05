use crate::lang::{
    AstNode, Block, Context, Eval, EvalStop, Identifier, InternalProgramError, Key, KeyValue, Span,
    Value, Var, accept_default,
    builtin_functions::resolve_function,
    value::{Func, StringCell, ValueType},
    visitor::{Accept, Visitor, VisitorResult},
};
use std::rc::Rc;

#[derive(PartialEq)]
pub struct FunctionDef {
    pub(crate) name: Option<Var>,
    pub(crate) params: Vec<Param>,
    pub(crate) body: Block,
    pub(crate) span: Span,
}

impl core::fmt::Debug for FunctionDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            let mut w = f.debug_struct("FunctionDef");
            if let Some(name) = &self.name {
                w.field("name", &name.ident.key);
            }
            if !self.params.is_empty() {
                w.field("params", &self.params);
            }
            w.field("body", &self.body);
            w.finish()
        } else {
            f.debug_struct("FunctionDef")
                .field("name", &self.name)
                .field("params", &self.params)
                .field("body", &self.body)
                .field("span", &self.span)
                .finish()
        }
    }
}

accept_default!(FunctionDef, name:opt:var, params:vec:param, body:block,);

#[derive(PartialEq, Clone)]
pub enum Param {
    Required(Var),
    Optional(Var, Value),
    OtherPositional(Var),
    OtherNamed(Var),
}

impl Param {
    pub(crate) fn key(&self) -> &Key {
        match self {
            Param::Required(var) => &var.ident.key,
            Param::Optional(var, _) => &var.ident.key,
            Param::OtherPositional(var) => &var.ident.key,
            Param::OtherNamed(var) => &var.ident.key,
        }
    }
}

impl core::fmt::Debug for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            match self {
                Param::Required(var) => var.ident.key.fmt(f),
                Param::Optional(var, default_value) => {
                    var.ident.key.fmt(f)?;
                    f.write_str(": ")?;
                    write!(f, "{default_value}")
                }
                Param::OtherPositional(var) => {
                    f.write_str("*")?;
                    var.ident.key.fmt(f)
                }
                Param::OtherNamed(var) => {
                    f.write_str("**")?;
                    var.ident.key.fmt(f)
                }
            }
        } else {
            match self {
                Param::Required(var) => f.debug_tuple("Param::Required").field(var).finish(),
                Param::Optional(var, default_value) => f
                    .debug_tuple("Param::Optional")
                    .field(var)
                    .field(default_value)
                    .finish(),
                Param::OtherPositional(var) => {
                    f.debug_tuple("Param::OtherPositional").field(var).finish()
                }
                Param::OtherNamed(var) => f.debug_tuple("Param::OtherNamed").field(var).finish(),
            }
        }
    }
}

impl<T> Accept<T> for Param {
    fn accept(&self, visitor: &mut impl Visitor<T>) -> VisitorResult<T> {
        match self {
            Param::Required(var) => visitor.visit_var(var),
            Param::Optional(var, _) => {
                match visitor.visit_var(var) {
                    s @ VisitorResult::Stop(_) => return s,
                    VisitorResult::Continue => {}
                };
                VisitorResult::Continue
            }
            Param::OtherPositional(var) => visitor.visit_var(var),
            Param::OtherNamed(var) => visitor.visit_var(var),
        }
    }
}

impl Eval for Rc<FunctionDef> {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let f = Value::Func(Func::FuncDef(Rc::clone(self)));
        if let Some(name) = &self.name {
            ctxt.declare(name.ident.key.clone(), f.ref_clone())
                .map_err(|_| InternalProgramError::NameAlreadyDeclared {
                    name: name.ident.as_string(),
                    span: name.ident.span,
                })?
        };
        Ok(f)
    }
}

impl FunctionDef {
    pub(crate) fn params(&self) -> &[Param] {
        &self.params[..]
    }

    pub fn call(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        self.body.eval_with_context(ctxt)
    }
}

pub(crate) type FunctionType =
    fn(call: &FunctionCall, ctxt: &mut Context) -> Result<Value, EvalStop>;

pub(crate) type MethodType<T> =
    fn(call: &FunctionCall, this: T, ctxt: &mut Context) -> Result<Value, EvalStop>;

pub(crate) trait MethodResolver: Sized {
    fn resolve_method(&self, key: &Key) -> Option<MethodType<Self>>;
}

#[derive(PartialEq, Clone)]
pub struct FunctionCall {
    pub(crate) variant: FunctionCallVariant,
    pub(crate) args: Vec<Arg>,
    pub(crate) span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionCallVariant {
    MethodCall {
        on: Box<AstNode>,
        function: Identifier,
    },
    FunctionCall {
        function: Identifier,
    },
    AnonymousCall {
        on: Box<AstNode>,
    },
}

impl core::fmt::Debug for FunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            let mut w = f.debug_struct("FunctionCall");
            match &self.variant {
                FunctionCallVariant::MethodCall { on, function } => {
                    w.field("on", on);
                    w.field("function", &function.key);
                }
                FunctionCallVariant::FunctionCall { function } => {
                    w.field("function", &function.key);
                }
                FunctionCallVariant::AnonymousCall { on } => {
                    w.field("on", on);
                }
            }
            if !self.args.is_empty() {
                w.field("args", &self.args);
            }
            w.finish()
        } else {
            f.debug_struct("FunctionCall")
                .field("variant", &self.variant)
                .field("args", &self.args)
                .field("span", &self.span)
                .finish()
        }
    }
}

impl Eval for FunctionCall {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        match &self.variant {
            FunctionCallVariant::MethodCall { on, function } => {
                let on = on.eval(ctxt)?;
                let on_value_type = on.value_type();
                match on {
                    Value::Dict(dict) => {
                        let r = dict.get(&function.key);
                        match r {
                            Some(Value::Func(func)) => {
                                return self.call_func(func, ctxt, Some(Value::Dict(dict)));
                            }
                            Some(v) => {
                                return Err(InternalProgramError::ExpectedFunction {
                                    got: v.value_type(),
                                    span: function.span,
                                }
                                .into_stop());
                            }
                            None => {}
                        }
                        if let Some(func) = dict.resolve_method(&function.key) {
                            return func(self, dict, ctxt);
                        }
                    }
                    Value::List(list) => {
                        if let Some(func) = list.resolve_method(&function.key) {
                            return func(self, list, ctxt);
                        }
                    }
                    Value::Str(string) => {
                        if let Some(func) = string.resolve_method(&function.key) {
                            return func(self, string, ctxt);
                        }
                    }
                    _ => {}
                }

                // TODO: better error - list all methods ?
                Err(InternalProgramError::NoSuchMethod {
                    name: function.as_string(),
                    from: on_value_type,
                    span: function.span,
                }
                .into_stop())
            }

            FunctionCallVariant::FunctionCall { function } => {
                if let Some(maybe_func) = ctxt.get(&function.key) {
                    let Value::Func(func) = maybe_func else {
                        return Err(InternalProgramError::ExpectedFunction {
                            got: maybe_func.value_type(),
                            span: function.span,
                        }
                        .into_stop());
                    };

                    return self.call_func(func, ctxt, None);
                };

                if let Some(func) = resolve_function(&function.key) {
                    let func = Func::BuiltIn(func);
                    return self.call_func(func, ctxt, None);
                }

                Err(InternalProgramError::NoSuchFunction {
                    name: function.as_string(),
                    span: function.span,
                }
                .into_stop())
            }
            FunctionCallVariant::AnonymousCall { on } => {
                let maybe_func = on.eval(ctxt)?;

                let Value::Func(func) = maybe_func else {
                    return Err(InternalProgramError::ExpectedCallable {
                        got: maybe_func.value_type(),
                        span: on.span(),
                    }
                    .into_stop());
                };

                self.call_func(func, ctxt, None)
            }
        }
    }
}

impl FunctionCall {
    fn call_func(
        &self,
        func: Func,
        ctxt: &mut Context,
        this: Option<Value>,
    ) -> Result<Value, EvalStop> {
        match func {
            Func::FuncDef(func) => {
                // FIXME: normal function call should have a new context, but what about closures ?
                let mut call_context =
                    self.build_call_context(ctxt, func.params(), this, &self.span)?;

                let result = func.call(&mut call_context);
                Self::map_function_call_result(result)
            }
            Func::BuiltIn(func) => {
                let result = func(self, ctxt);
                Self::map_function_call_result(result)
            }
        }
    }

    fn map_function_call_result(value: Result<Value, EvalStop>) -> Result<Value, EvalStop> {
        match value {
            v @ Ok(_) => v,
            e @ Err(EvalStop::Error(_)) => e,
            Err(EvalStop::Return(value, _)) => Ok(value),
            Err(EvalStop::Break(_, span)) => {
                Err(EvalStop::Error(InternalProgramError::BreakWithoutLoop {
                    span,
                }))
            }
            Err(EvalStop::Continue(span)) => {
                Err(EvalStop::Error(InternalProgramError::ContinueWithoutLoop {
                    span,
                }))
            }
        }
    }

    fn build_call_context(
        &self,
        ctxt: &mut Context,
        params: &[Param],
        this: Option<Value>,
        call_span: &Span,
    ) -> Result<Context, EvalStop> {
        let mut call_context = ctxt.new_function_call_ctxt();

        if let Some(this) = this {
            call_context
                .declare_this(this)
                .expect("'this' should not already be declared");
        }

        let mut positional_mode = true;
        for (i, arg) in self.args.iter().enumerate() {
            if positional_mode {
                let Some(param) = params.get(i) else {
                    return Err(InternalProgramError::FunctionCallUnexpectedArgument {
                        span: arg.span(),
                    }
                    .into());
                };

                if arg.name.is_some() {
                    positional_mode = false;
                } else {
                    call_context
                        .declare(param.key().clone(), arg.value.eval(ctxt)?)
                        .expect("variable should not already be declared");
                }
            }

            if !positional_mode {
                let Some(arg_name) = &arg.name else {
                    return Err(
                        InternalProgramError::FunctionCallPositionalArgAfterNamedArg {
                            span: arg.span(),
                        }
                        .into(),
                    );
                };

                if !params.iter().any(|p| p.key() == &arg_name.key) {
                    return Err(InternalProgramError::FunctionCallNoSuchParameter {
                        name: arg_name.key.as_string(),
                        span: arg_name.span,
                    }
                    .into());
                };

                if call_context.contains_key(&arg_name.key) {
                    return Err(InternalProgramError::FunctionCallParamAlreadySet {
                        name: arg_name.key.as_string(),
                        span: arg_name.span,
                    }
                    .into());
                }

                call_context
                    .declare(arg_name.key.clone(), arg.value.eval(ctxt)?)
                    .expect("variable should not already be declared");
            }
        }

        // fill in optional args, check required args
        for param in params {
            match param {
                Param::Required(var) => {
                    if !call_context.contains_key(&var.ident.key) {
                        return Err(InternalProgramError::FunctionCallMissingRequiredArgument {
                            name: var.ident.key.as_string(),
                            span: *call_span,
                        }
                        .into());
                    }
                }
                Param::Optional(var, val) => {
                    if !call_context.contains_key(&var.ident.key) {
                        call_context
                            .declare(var.ident.key.clone(), val.deep_clone())
                            .expect("variable should not already be declared");
                    }
                }
                Param::OtherPositional(_) => {}
                Param::OtherNamed(_) => {}
            }
        }

        Ok(call_context)
    }

    pub(crate) fn required_positional_arg<'a>(
        &self,
        name: &str,
        arg: Option<&'a Arg>,
    ) -> Result<&'a Arg, EvalStop> {
        let Some(arg) = arg else {
            return Err(InternalProgramError::FunctionCallMissingRequiredArgument {
                name: name.to_string(),
                span: self.span,
            }
            .into());
        };
        Ok(arg)
    }

    pub(crate) fn require_string(
        &self,
        name: &str,
        arg: &Arg,
        ctxt: &mut Context,
    ) -> Result<StringCell, EvalStop> {
        let val = arg.eval(ctxt)?;
        let Value::Str(val) = val else {
            return Err(InternalProgramError::FunctionCallBadArgType {
                name: name.to_string(),
                expected: ValueType::String,
                actual: val.value_type(),
                span: arg.span(),
            }
            .into());
        };
        Ok(val)
    }

    pub(crate) fn require_integer(
        &self,
        name: &str,
        arg: &Arg,
        ctxt: &mut Context,
    ) -> Result<isize, EvalStop> {
        let val = arg.eval(ctxt)?;
        let Value::Integer(val) = val else {
            return Err(InternalProgramError::FunctionCallBadArgType {
                name: name.to_string(),
                expected: ValueType::Integer,
                actual: val.value_type(),
                span: arg.span(),
            }
            .into());
        };
        Ok(val)
    }

    pub(crate) fn require_key_value(
        &self,
        name: &str,
        arg: &Arg,
        ctxt: &mut Context,
    ) -> Result<Rc<KeyValue>, EvalStop> {
        let val = arg.eval(ctxt)?;
        let Value::KeyValue(val) = val else {
            return Err(InternalProgramError::FunctionCallBadArgType {
                name: name.to_string(),
                expected: ValueType::KeyValue,
                actual: val.value_type(),
                span: arg.span(),
            }
            .into());
        };
        Ok(val)
    }
}

impl<T> Accept<T> for FunctionCall {
    fn accept(&self, visitor: &mut impl Visitor<T>) -> VisitorResult<T> {
        match &self.variant {
            FunctionCallVariant::MethodCall { on, function: _ } => match visitor.visit_node(on) {
                v @ VisitorResult::Stop(_) => return v,
                VisitorResult::Continue => {}
            },
            FunctionCallVariant::FunctionCall { function: _ } => {}
            FunctionCallVariant::AnonymousCall { on } => match visitor.visit_node(on) {
                v @ VisitorResult::Stop(_) => return v,
                VisitorResult::Continue => {}
            },
        }
        for arg in &self.args {
            match visitor.visit_arg(arg) {
                v @ VisitorResult::Stop(_) => return v,
                VisitorResult::Continue => {}
            }
        }
        VisitorResult::Continue
    }
}

#[derive(PartialEq, Clone)]
pub struct Arg {
    pub(crate) name: Option<Identifier>,
    pub(crate) value: AstNode,
}

accept_default!(Arg, value:node,);

impl core::fmt::Debug for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            if let Some(name) = &self.name {
                name.key.fmt(f)?;
                f.write_str(": ")?;
                self.value.fmt(f)
            } else {
                self.value.fmt(f)
            }
        } else {
            f.debug_struct("Arg")
                .field("name", &self.name)
                .field("value", &self.value)
                .finish()
        }
    }
}

impl Arg {
    pub fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        if let Some(name) = &self.name {
            let val = self.value.eval(ctxt)?;
            Ok(Value::KeyValue(Rc::new(KeyValue {
                key: name.key.clone(),
                value: val,
            })))
        } else {
            self.value.eval(ctxt)
        }
    }

    pub(crate) fn span(&self) -> Span {
        let span = self.value.span();
        match &self.name {
            Some(n) => Span::merge(span, n.span),
            None => span,
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct ReturnStmt {
    pub(crate) value: Option<Box<AstNode>>,
    pub(crate) span: Span,
}

impl core::fmt::Debug for ReturnStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            match &self.value {
                Some(v) => {
                    f.write_str("Return { ")?;
                    v.fmt(f)?;
                    f.write_str(" }")
                }
                None => write!(f, "Return"),
            }
        } else {
            f.debug_struct("ReturnStmt")
                .field("value", &self.value)
                .finish()
        }
    }
}

impl Eval for ReturnStmt {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let val = match &self.value {
            Some(v) => v.eval(ctxt)?,
            None => Value::Nil,
        };

        Err(EvalStop::Return(val, self.span))
    }
}

accept_default!(ReturnStmt, value:opt:node,);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::Span,
        parser::tests::{arg, i, id, nil, var},
    };

    #[test]
    fn test_build_call_context_params_none_args_none() -> Result<(), EvalStop> {
        let params = Vec::new();
        let call = function_call(Vec::new());
        let mut ctxt = prepare_ctxt();

        let ctxt = call.build_call_context(&mut ctxt, &params[..], None, &Span::new(88, 888))?;

        assert_eq!(ctxt.get_this(), None);
        assert_eq!(ctxt.size(), 0);

        Ok(())
    }

    #[test]
    fn test_build_call_context_params_required_args_positional() -> Result<(), EvalStop> {
        let params = [Param::Required(var("aa")), Param::Required(var("bb"))];
        let call = function_call(vec![arg!(i(1)), arg!(i(2))]);
        let mut ctxt = prepare_ctxt();

        let ctxt = call.build_call_context(&mut ctxt, &params[..], None, &Span::new(88, 888))?;

        assert_eq!(ctxt.get_this(), None);
        assert_eq!(ctxt.size(), 2);
        assert_eq!(ctxt.get(&"aa".into()), Some(1.into()));
        assert_eq!(ctxt.get(&"bb".into()), Some(2.into()));

        Ok(())
    }

    #[test]
    fn test_build_call_context_params_required_args_named() -> Result<(), EvalStop> {
        let params = [Param::Required(var("aa")), Param::Required(var("bb"))];
        let call = function_call(vec![arg!("bb", i(1)), arg!("aa", i(2))]);
        let mut ctxt = prepare_ctxt();

        let ctxt = call.build_call_context(&mut ctxt, &params[..], None, &Span::new(88, 888))?;

        assert_eq!(ctxt.get_this(), None);
        assert_eq!(ctxt.size(), 2);
        assert_eq!(ctxt.get(&"aa".into()), Some(2.into()));
        assert_eq!(ctxt.get(&"bb".into()), Some(1.into()));

        Ok(())
    }

    #[test]
    fn test_build_call_context_params_optional_args_none() -> Result<(), EvalStop> {
        let params = [
            Param::Optional(var("aa"), "x".into()),
            Param::Optional(var("bb"), "y".into()),
        ];
        let call = function_call(vec![]);
        let mut ctxt = prepare_ctxt();

        let ctxt = call.build_call_context(&mut ctxt, &params[..], None, &Span::new(88, 888))?;

        assert_eq!(ctxt.get_this(), None);
        assert_eq!(ctxt.size(), 2);
        assert_eq!(ctxt.get(&"aa".into()), Some("x".into()));
        assert_eq!(ctxt.get(&"bb".into()), Some("y".into()));

        Ok(())
    }

    #[test]
    fn test_build_call_context_params_optional_args_positional() -> Result<(), EvalStop> {
        let params = [
            Param::Optional(var("aa"), "x".into()),
            Param::Optional(var("bb"), "y".into()),
        ];
        let call = function_call(vec![arg!(i(1)), arg!(i(2))]);
        let mut ctxt = prepare_ctxt();

        let ctxt = call.build_call_context(&mut ctxt, &params[..], None, &Span::new(88, 888))?;

        assert_eq!(ctxt.get_this(), None);
        assert_eq!(ctxt.size(), 2);
        assert_eq!(ctxt.get(&"aa".into()), Some(1.into()));
        assert_eq!(ctxt.get(&"bb".into()), Some(2.into()));

        Ok(())
    }

    #[test]
    fn test_build_call_context_params_optional_args_explicit_nil() -> Result<(), EvalStop> {
        let params = [
            Param::Optional(var("aa"), "x".into()),
            Param::Optional(var("bb"), "y".into()),
        ];
        let call = function_call(vec![arg!(nil())]);
        let mut ctxt = prepare_ctxt();

        let ctxt = call.build_call_context(&mut ctxt, &params[..], None, &Span::new(88, 888))?;

        assert_eq!(ctxt.get_this(), None);
        assert_eq!(ctxt.size(), 2);
        assert_eq!(ctxt.get(&"aa".into()), Some(nil().val));
        assert_eq!(ctxt.get(&"bb".into()), Some("y".into()));

        Ok(())
    }

    #[test]
    fn test_build_call_context_mixed() -> Result<(), EvalStop> {
        let params = [
            Param::Required(var("aa")),
            Param::Required(var("bb")),
            Param::Required(var("cc")),
            Param::Optional(var("dd"), "x".into()),
            Param::Required(var("ee")),
            Param::Optional(var("ff"), "z".into()),
        ];
        let call = function_call(vec![
            arg!(i(1)),
            arg!(i(2)),
            arg!("ee", i(3)),
            arg!("ff", i(4)),
            arg!("cc", i(5)),
        ]);
        let mut ctxt = prepare_ctxt();

        let ctxt = call.build_call_context(&mut ctxt, &params[..], None, &Span::new(88, 888))?;

        assert_eq!(ctxt.get_this(), None);
        assert_eq!(ctxt.size(), 6);
        assert_eq!(ctxt.get(&"aa".into()), Some(1.into()));
        assert_eq!(ctxt.get(&"bb".into()), Some(2.into()));
        assert_eq!(ctxt.get(&"cc".into()), Some(5.into()));
        assert_eq!(ctxt.get(&"dd".into()), Some("x".into()));
        assert_eq!(ctxt.get(&"ee".into()), Some(3.into()));
        assert_eq!(ctxt.get(&"ff".into()), Some(4.into()));

        Ok(())
    }

    #[test]
    fn test_build_call_context_error_too_many_args() {
        let params = [
            Param::Optional(var("aa"), "x".into()),
            Param::Optional(var("bb"), "y".into()),
        ];
        let call = function_call(vec![
            arg!(i(1).spanned(5, 5)),
            arg!(i(2).spanned(6, 6)),
            arg!(i(3).spanned(7, 7)),
            arg!(i(4).spanned(8, 8)),
        ]);
        let mut ctxt = prepare_ctxt();

        let err = call
            .build_call_context(&mut ctxt, &params[..], None, &Span::new(88, 888))
            .err()
            .expect("expect error");

        let EvalStop::Error(InternalProgramError::FunctionCallUnexpectedArgument { span }) = err
        else {
            panic!("unexpected error: {err:#?}");
        };

        assert_eq!(span, Span::new(7, 7));
    }

    #[test]
    fn test_build_call_context_error_positional_after_named() {
        let params = [
            Param::Optional(var("aa"), "x".into()),
            Param::Optional(var("bb"), "y".into()),
            Param::Optional(var("cc"), "z".into()),
        ];
        let call = function_call(vec![
            arg!("bb", i(1).spanned(5, 5)),
            arg!(i(2).spanned(6, 6)),
        ]);
        let mut ctxt = prepare_ctxt();

        let err = call
            .build_call_context(&mut ctxt, &params[..], None, &Span::new(88, 888))
            .err()
            .expect("expect error");

        let EvalStop::Error(InternalProgramError::FunctionCallPositionalArgAfterNamedArg { span }) =
            err
        else {
            panic!("unexpected error: {err:#?}");
        };

        assert_eq!(span, Span::new(6, 6));
    }

    #[test]
    fn test_build_call_context_error_no_such_parameter() {
        let params = [
            Param::Optional(var("aa"), "x".into()),
            Param::Optional(var("bb"), "y".into()),
            Param::Optional(var("cc"), "z".into()),
        ];
        let call = function_call(vec![
            arg!("bb", i(1).spanned(5, 5)),
            arg!(id("xx").spanned(7, 7), i(1).spanned(6, 6)),
        ]);
        let mut ctxt = prepare_ctxt();

        let err = call
            .build_call_context(&mut ctxt, &params[..], None, &Span::new(88, 888))
            .err()
            .expect("expect error");

        let EvalStop::Error(InternalProgramError::FunctionCallNoSuchParameter { span, name }) = err
        else {
            panic!("unexpected error: {err:#?}");
        };

        assert_eq!(name, "xx".to_string());
        assert_eq!(span, Span::new(7, 7));
    }

    #[test]
    fn test_build_call_context_error_parameter_already_set() {
        let params = [
            Param::Optional(var("aa"), "x".into()),
            Param::Optional(var("bb"), "y".into()),
            Param::Optional(var("cc"), "z".into()),
        ];
        let call = function_call(vec![
            arg!(i(1)),
            arg!(i(2)),
            arg!(id("aa").spanned(7, 7), i(7)),
        ]);
        let mut ctxt = prepare_ctxt();

        let err = call
            .build_call_context(&mut ctxt, &params[..], None, &Span::new(88, 888))
            .err()
            .expect("expect error");

        let EvalStop::Error(InternalProgramError::FunctionCallParamAlreadySet { span, name }) = err
        else {
            panic!("unexpected error: {err:#?}");
        };

        assert_eq!(name, "aa".to_string());
        assert_eq!(span, Span::new(7, 7));
    }

    #[test]
    fn test_build_call_context_error_missing_arg() {
        let params = [
            Param::Required(var("aa")),
            Param::Required(var("bb")),
            Param::Optional(var("cc"), "z".into()),
        ];
        let call = function_call(vec![arg!(i(1)), arg!(id("cc").spanned(7, 7), i(7))]);
        let mut ctxt = prepare_ctxt();

        let err = call
            .build_call_context(&mut ctxt, &params[..], None, &Span::new(88, 888))
            .err()
            .expect("expect error");

        let EvalStop::Error(InternalProgramError::FunctionCallMissingRequiredArgument {
            span,
            name,
        }) = err
        else {
            panic!("unexpected error: {err:#?}");
        };

        assert_eq!(name, "bb".to_string());
        assert_eq!(span, Span::new(88, 888));
    }

    fn function_call(args: Vec<Arg>) -> FunctionCall {
        FunctionCall {
            variant: FunctionCallVariant::FunctionCall {
                function: "dummy".into(),
            },
            args,
            span: Span::new(999, 1),
        }
    }

    fn prepare_ctxt() -> Context {
        let mut ctxt = Context::default();
        ctxt.declare("old".into(), "dummy".into()).unwrap();
        ctxt
    }
}
