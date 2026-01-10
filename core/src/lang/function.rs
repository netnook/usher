use crate::lang::{
    AstNode, Block, Context, Dict, Eval, EvalStop, Identifier, InternalProgramError, Key, List,
    Span, Value, Var, accept_default,
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

                match arg {
                    Arg::Positional(value) => {
                        call_context
                            .declare(param.key().clone(), value.eval(ctxt)?)
                            .expect("variable should not already be declared");
                    }
                    Arg::Named(_) => {
                        positional_mode = false;
                    }
                }
            }

            if !positional_mode {
                let Arg::Named(arg) = &arg else {
                    return Err(
                        InternalProgramError::FunctionCallPositionalArgAfterNamedArg {
                            span: arg.span(),
                        }
                        .into(),
                    );
                };

                if !params.iter().any(|p| p.key() == &arg.name.key) {
                    return Err(InternalProgramError::FunctionCallNoSuchParameter {
                        name: arg.name.key.as_string(),
                        span: arg.name.span,
                    }
                    .into());
                };

                if call_context.contains_key(&arg.name.key) {
                    return Err(InternalProgramError::FunctionCallParamAlreadySet {
                        name: arg.name.key.as_string(),
                        span: arg.name.span,
                    }
                    .into());
                }

                call_context
                    .declare(arg.name.key.clone(), arg.eval(ctxt)?)
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

// FIXME: rename to MacroUtils ?
#[allow(dead_code)] // FIXME remove allow once in use
pub struct ArgUtils {}

#[allow(dead_code)] // FIXME remove allow once in use
impl ArgUtils {
    pub(crate) fn to_string(
        val: Value,
        span: Span,
        param_name: &str,
    ) -> Result<StringCell, EvalStop> {
        match val {
            Value::Str(val) => Ok(val),
            _ => Err(Self::type_error(val, ValueType::String, span, param_name)),
        }
    }
    pub(crate) fn to_int(val: Value, span: Span, param_name: &str) -> Result<isize, EvalStop> {
        match val {
            Value::Integer(val) => Ok(val),
            _ => Err(Self::type_error(val, ValueType::Integer, span, param_name)),
        }
    }
    pub(crate) fn to_float(val: Value, span: Span, param_name: &str) -> Result<f64, EvalStop> {
        match val {
            Value::Float(val) => Ok(val),
            _ => Err(Self::type_error(val, ValueType::Float, span, param_name)),
        }
    }
    pub(crate) fn to_bool(val: Value, span: Span, param_name: &str) -> Result<bool, EvalStop> {
        match val {
            Value::Bool(val) => Ok(val),
            _ => Err(Self::type_error(val, ValueType::Boolean, span, param_name)),
        }
    }
    pub(crate) fn to_list(val: Value, span: Span, param_name: &str) -> Result<List, EvalStop> {
        match val {
            Value::List(val) => Ok(val),
            _ => Err(Self::type_error(val, ValueType::List, span, param_name)),
        }
    }
    pub(crate) fn to_dict(val: Value, span: Span, param_name: &str) -> Result<Dict, EvalStop> {
        match val {
            Value::Dict(val) => Ok(val),
            _ => Err(Self::type_error(val, ValueType::Dict, span, param_name)),
        }
    }
    fn type_error(val: Value, expected: ValueType, span: Span, param_name: &str) -> EvalStop {
        InternalProgramError::FunctionCallBadArgType {
            name: param_name.to_string(),
            expected,
            actual: val.value_type(),
            span,
        }
        .into()
    }
    pub(crate) fn param_already_set_error(param_name: &str, span: Span) -> EvalStop {
        InternalProgramError::FunctionCallParamAlreadySet {
            name: param_name.to_string(),
            span,
        }
        .into()
    }
    pub(crate) fn missing_argument_error(param_name: &str, span: Span) -> EvalStop {
        InternalProgramError::FunctionCallMissingRequiredArgument {
            name: param_name.to_string(),
            span,
        }
        .into()
    }
}

#[derive(PartialEq, Clone)]
pub enum Arg {
    Positional(PositionalArg),
    Named(NamedArg),
}

impl Arg {
    pub(crate) fn span(&self) -> Span {
        match self {
            Arg::Positional(arg) => arg.span(),
            Arg::Named(arg) => arg.span(),
        }
    }
}

impl<T> Accept<T> for Arg {
    fn accept(&self, visitor: &mut impl Visitor<T>) -> VisitorResult<T> {
        match self {
            Arg::Positional(arg) => visitor.visit_node(&arg.expr),
            Arg::Named(arg) => visitor.visit_node(&arg.expr),
        }
    }
}

impl core::fmt::Debug for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            match self {
                Arg::Positional(arg) => arg.expr.fmt(f),
                Arg::Named(arg) => {
                    arg.name.key.fmt(f)?;
                    f.write_str(": ")?;
                    arg.expr.fmt(f)
                }
            }
        } else {
            match self {
                Arg::Positional(arg) => f.debug_tuple("Arg").field(&arg).finish(),
                Arg::Named(arg) => f.debug_tuple("Arg").field(&arg).finish(),
            }
        }
    }
}

impl Eval for Arg {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        match self {
            Arg::Positional(arg) => arg.eval(ctxt),
            Arg::Named(arg) => arg.eval(ctxt),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct PositionalArg {
    pub(crate) expr: AstNode,
}

impl PositionalArg {
    pub(crate) fn span(&self) -> Span {
        self.expr.span()
    }
}

impl Eval for PositionalArg {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        self.expr.eval(ctxt)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct NamedArg {
    pub(crate) name: Identifier,
    pub(crate) expr: AstNode,
}

impl NamedArg {
    pub(crate) fn span(&self) -> Span {
        Span::merge(self.name.span, self.expr.span())
    }
}

impl Eval for NamedArg {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        self.expr.eval(ctxt)
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
        lang::{Dict, List, Nillable, Span},
        parser::tests::{_function_call, arg, b, dict, f, i, id, l, list, nil, s, var},
    };
    use usher_macros::UsherArgs;

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

    // FIXME: move this to correct place for macro testing
    #[test]
    fn test_arg_structs_types_optional() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: Option<StringCell>,
            bbb: Option<isize>,
            ccc: Option<f64>,
            ddd: Option<bool>,
            eee: Option<List>,
            fff: Option<Dict>,
            ggg: Option<Value>,
        }

        let call = _function_call!(
            "foo",
            arg!(s("a-string")),
            arg!(i(42)),
            arg!(f(42.3)),
            arg!(b(true)),
            arg!(l(list!().into())),
            arg!(l(dict!().into())),
            arg!(s("foo"))
        )
        .spanned(10, 5);
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, Some("a-string".into()));
        assert_eq!(args.bbb, Some(42));
        assert_eq!(args.ccc, Some(42.3));
        assert_eq!(args.ddd, Some(true));
        assert_eq!(args.eee, Some(List::new()));
        assert_eq!(args.fff, Some(Dict::new()));
        assert_eq!(args.ggg, Some(Value::Str("foo".into())));
    }

    #[test]
    fn test_arg_structs_missing_optional() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: Option<StringCell>,
        }

        let call = _function_call!("foo",);
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, None);
    }

    #[test]
    fn test_arg_structs_nillable() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            // aaa: Nillable<StringCell>,
            aaa: (usize, Nillable<StringCell>),
            bbb: Option<(usize, Nillable<StringCell>)>,
            // bbb: Nillable<StringCell>,
            // ccc: Option<Nillable<StringCell>>,
            ddd: Option<Nillable<StringCell>>,
            eee: Option<Nillable<StringCell>>,
        }

        let call = _function_call!(
            "foo",
            arg!("aaa", nil()),
            arg!("bbb", s("foo")),
            //arg!("ccc",nil()),
            arg!("ddd", nil()),
            arg!("eee", s("bar"))
        );
        // let call = _function_call!("foo", arg!(s("a")));
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, (0, Nillable::Nil));
        assert_eq!(args.bbb, Some((1, Nillable::Some("foo".into()))));
        // assert_eq!(args.ccc, None);
        assert_eq!(args.ddd, Some(Nillable::Nil));
        assert_eq!(args.eee, Some(Nillable::Some("bar".into())));
    }

    // #[test]
    // fn test_arg_structs_nilable_none() {
    //     #[derive(UsherArgs)]
    //     struct TestArgs {
    //         // #[usher(no_nil)]
    //         aaa: Option<StringCell>,
    //     }
    //     // args_struct!(TestArgs, arg(aaa, 0, string | nil, optional));

    //     let call = _function_call!("foo",);
    //     let mut ctxt = Context::default();

    //     let args = TestArgs::new(&call, &mut ctxt).unwrap();

    //     assert_eq!(args.aaa, None);
    // }

    // #[test]
    // fn test_arg_structs_nilable_nil() {
    //     args_struct!(TestArgs, arg(aaa, 0, string | nil, optional));

    //     let call = _function_call!("foo", arg!(nil()));
    //     let mut ctxt = Context::default();

    //     let args = TestArgs::new(&call, &mut ctxt).unwrap();

    //     assert_eq!(args.aaa, Some(MaybeNil::Nil));
    // }

    // #[test]
    // fn test_arg_structs_nilable_val() {
    //     args_struct!(TestArgs, arg(aaa, 0, string | nil, optional));

    //     let call = _function_call!("foo", arg!(s("x")));
    //     let mut ctxt = Context::default();

    //     let args = TestArgs::new(&call, &mut ctxt).unwrap();

    //     assert_eq!(args.aaa, Some(MaybeNil::Some("x".into())));
    // }

    #[test]
    fn test_arg_structs_types_required() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: StringCell,
            bbb: isize,
            ccc: f64,
            ddd: bool,
            eee: List,
            fff: Dict,
            ggg: Value,
        }

        let call = _function_call!(
            "foo",
            arg!(s("a-string")),
            arg!(i(42)),
            arg!(f(42.3)),
            arg!(b(true)),
            arg!(l(list!().into())),
            arg!(l(dict!().into())),
            arg!(s("foo"))
        )
        .spanned(10, 5);
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, "a-string".into());
        assert_eq!(args.bbb, 42);
        assert_eq!(args.ccc, 42.3);
        assert!(args.ddd);
        assert_eq!(args.eee, List::new());
        assert_eq!(args.fff, Dict::new());
        assert_eq!(args.ggg, Value::Str("foo".into()));
    }

    #[test]
    fn test_arg_structs_missing_required() {
        #[derive(UsherArgs, Debug)]
        #[usher(internal)]
        struct TestArgs {
            _aaa: StringCell,
        }

        let call = _function_call!("foo",).spanned(10, 5);
        let mut ctxt = Context::default();

        assert_eq!(
            TestArgs::eval(&call, &mut ctxt).unwrap_err(),
            InternalProgramError::FunctionCallMissingRequiredArgument {
                name: "_aaa".to_string(),
                span: Span::new(10, 5)
            }
            .into_stop()
        );
    }

    #[test]
    fn test_arg_structs_named_args() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: StringCell,
            bbb: isize,
            ccc: f64,
            ddd: bool,
            eee: List,
            fff: Dict,
        }

        let call = _function_call!(
            "foo",
            arg!("aaa", s("a-string")),
            arg!("bbb", i(42)),
            arg!("ccc", f(42.3)),
            arg!("ddd", b(true)),
            arg!("eee", l(list!().into())),
            arg!("fff", l(dict!().into()))
        )
        .spanned(10, 5);
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, "a-string".into());
        assert_eq!(args.bbb, 42);
        assert_eq!(args.ccc, 42.3);
        assert!(args.ddd);
        assert_eq!(args.eee, List::new());
        assert_eq!(args.fff, Dict::new());
    }

    #[test]
    fn test_arg_structs_star_args() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: Vec<StringCell>,
        }

        let call = _function_call!(
            "foo",
            arg!(s("a-string")),
            arg!(s("b-string")),
            arg!(s("c-string"))
        )
        .spanned(10, 5);
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(
            args.aaa,
            vec!["a-string".into(), "b-string".into(), "c-string".into(),]
        );
    }

    #[test]
    fn test_arg_structs_mixed_positional_and_named_args() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs {
            aaa: isize,
            bbb: isize,
            ccc: isize,
            ggg: Vec<isize>,
            ddd: isize,
            eee: isize,
            fff: isize,
        }

        let call = _function_call!(
            "foo",
            arg!(i(1)),
            arg!(i(2)),
            arg!(i(3)),
            arg!(i(7)),
            arg!(i(8)),
            arg!("ddd", i(4)),
            arg!("fff", i(5)),
            arg!("eee", i(6))
        )
        .spanned(10, 5);
        let mut ctxt = Context::default();

        let args = TestArgs::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, 1);
        assert_eq!(args.bbb, 2);
        assert_eq!(args.ccc, 3);
        assert_eq!(args.ddd, 4);
        assert_eq!(args.eee, 6);
        assert_eq!(args.fff, 5);
        assert_eq!(args.ggg, vec![7, 8]);
    }

    #[test]
    fn test_arg_structs_too_many_args() {
        #[derive(UsherArgs, Debug)]
        #[usher(internal)]
        struct TestArgs {
            _aaa: StringCell,
            _bbb: StringCell,
        }

        let call = _function_call!(
            "foo",
            arg!(s("a-string")),
            arg!(s("b-string")),
            arg!(s("c-string").spanned(42, 24))
        )
        .spanned(10, 5);
        let mut ctxt = Context::default();

        assert_eq!(
            TestArgs::eval(&call, &mut ctxt).unwrap_err(),
            InternalProgramError::FunctionCallUnexpectedArgument {
                span: Span::new(42, 24)
            }
            .into_stop()
        );
    }

    #[test]
    fn test_arg_structs_bad_named_arg() {
        #[derive(UsherArgs, Debug)]
        #[usher(internal)]
        struct TestArgs {
            _aaa: StringCell,
            _bbb: StringCell,
        }

        let call = _function_call!(
            "foo",
            arg!("_aaa", s("a-string")),
            arg!(id("bad").spanned(42, 24), s("bad-string")),
            arg!("_bbb", s("b-string"))
        )
        .spanned(10, 5);
        let mut ctxt = Context::default();

        assert_eq!(
            TestArgs::eval(&call, &mut ctxt).unwrap_err(),
            InternalProgramError::FunctionCallNoSuchParameter {
                name: "bad".to_string(),
                span: Span::new(42, 24)
            }
            .into_stop()
        );
    }

    #[test]
    fn test_arg_structs_bad_type_positional() {
        #[derive(UsherArgs, Debug)]
        #[usher(internal)]
        struct TestArgs {
            _aaa: StringCell,
        }

        let call = _function_call!("foo", arg!(i(22).spanned(12, 13)));
        let mut ctxt = Context::default();

        assert_eq!(
            TestArgs::eval(&call, &mut ctxt).unwrap_err(),
            InternalProgramError::FunctionCallBadArgType {
                name: "_aaa".to_string(),
                expected: ValueType::String,
                actual: ValueType::Integer,
                span: Span::new(12, 13)
            }
            .into_stop()
        );
    }

    #[test]
    fn test_arg_structs_bad_type_named() {
        #[derive(UsherArgs, Debug)]
        #[usher(internal)]
        struct TestArgs {
            _aaa: isize,
        }

        let call = _function_call!(
            "foo",
            arg!(id("_aaa").spanned(3, 4), s("bad").spanned(5, 6))
        );
        let mut ctxt = Context::default();

        assert_eq!(
            TestArgs::eval(&call, &mut ctxt).unwrap_err(),
            InternalProgramError::FunctionCallBadArgType {
                name: "_aaa".to_string(),
                expected: ValueType::Integer,
                actual: ValueType::String,
                span: Span::new(3, 8)
            }
            .into_stop()
        );
    }

    #[test]
    fn test_arg_structs_bad_type_star() {
        #[derive(UsherArgs, Debug)]
        #[usher(internal)]
        struct TestArgs {
            _aaa: Vec<f64>,
        }

        let call = _function_call!("foo", arg!(f(1.1)), arg!(b(true).spanned(5, 6)));
        let mut ctxt = Context::default();

        assert_eq!(
            TestArgs::eval(&call, &mut ctxt).unwrap_err(),
            InternalProgramError::FunctionCallBadArgType {
                name: "_aaa".to_string(),
                expected: ValueType::Float,
                actual: ValueType::Boolean,
                span: Span::new(5, 6)
            }
            .into_stop()
        );
    }

    #[test]
    fn test_arg_structs_eval_order() {
        #[derive(UsherArgs)]
        #[usher(internal)]
        struct TestArgs2 {
            aaa: isize,
            bbb: isize,
            star: Vec<isize>,
            ccc: isize,
            ddd: isize,
            eee: isize,
        }

        let call = _function_call!(
            "foo",
            arg!(_function_call!(id("test_next_id"),)),
            arg!(_function_call!(id("test_next_id"),)),
            arg!(_function_call!(id("test_next_id"),)),
            arg!(_function_call!(id("test_next_id"),)),
            arg!("ddd", _function_call!(id("test_next_id"),)),
            arg!("ccc", _function_call!(id("test_next_id"),)),
            arg!("eee", _function_call!(id("test_next_id"),))
        );
        let mut ctxt = Context::default();

        let args = TestArgs2::eval(&call, &mut ctxt).unwrap();

        assert_eq!(args.aaa, 0);
        assert_eq!(args.bbb, 1);
        assert_eq!(args.star, vec![2, 3]);
        assert_eq!(args.ccc, 5);
        assert_eq!(args.ddd, 4);
        assert_eq!(args.eee, 6);
    }
}
