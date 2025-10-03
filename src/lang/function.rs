use crate::lang::{
    AstNode, Block, Context, Dict, Eval, EvalStop, Identifier, InternalProgramError, Key, KeyValue,
    List, Span, Value, Var, accept_default,
    value::Func,
    visitor::{Accept, Visitor, VisitorResult},
};
use std::rc::Rc;

#[derive(PartialEq)]
pub struct FunctionDef {
    pub(crate) name: Option<Var>,
    pub(crate) params: Vec<Param>,
    pub(crate) body: Block,
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
                .finish()
        }
    }
}

accept_default!(FunctionDef, name:opt:var, params:vec:param, body:block,);

#[derive(PartialEq, Clone)]
pub enum Param {
    Required(Var),
    Optional(Var, AstNode), // FIXME: AstNode should probably be a Value!!!
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
                    default_value.fmt(f)
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
            Param::Optional(var, default_value) => {
                match visitor.visit_var(var) {
                    s @ VisitorResult::Stop(_) => return s,
                    VisitorResult::Continue => {}
                };
                match visitor.visit_node(default_value) {
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
        let f = Value::Func(Func::Func(Rc::clone(self)));
        if let Some(name) = &self.name {
            ctxt.set(&name.ident.key, f.clone());
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

#[derive(PartialEq, Debug, Clone)]
pub enum BuiltInFunc {
    Print,
    EPrint,
    Add,
}

impl BuiltInFunc {
    // FIXME: git rid of this
    pub fn by_name(key: &Key) -> Option<BuiltInFunc> {
        let key = key.0.as_str();
        match key {
            "print" => Some(BuiltInFunc::Print),
            "eprint" => Some(BuiltInFunc::EPrint),
            _ => None,
        }
    }

    pub fn call(
        &self,
        ctxt: &mut Context,
        args: Vec<Value>,
        span: &Span,
    ) -> Result<Value, EvalStop> {
        match self {
            BuiltInFunc::Print => {
                let mut first = true;
                for arg in args {
                    if first {
                        first = false;
                    } else {
                        ctxt.stdout(", ");
                    }
                    ctxt.stdout(arg.as_string()?.as_ref());
                }
                ctxt.stdout("\n");
                Ok(Value::Nil)
            }
            BuiltInFunc::EPrint => {
                let mut first = true;
                for arg in args {
                    if first {
                        first = false;
                    } else {
                        ctxt.stderr(", ");
                    }
                    ctxt.stderr(arg.as_string()?.as_ref());
                }
                ctxt.stderr("\n");
                Ok(Value::Nil)
            }
            BuiltInFunc::Add => {
                let this = ctxt.get_this().unwrap_or(Value::Nil);
                match this {
                    Value::List(list) => {
                        let mut list = list.borrow_mut();
                        for arg in args {
                            list.add(arg);
                        }
                        Ok(Value::Nil)
                    }
                    // FIXME: panic - this should not be reachable - can this be ensured programattically
                    _ => InternalProgramError::MethodNotApplicable {
                        name: "add".to_string(),
                        to: this.value_type(),
                        span: *span,
                    }
                    .into(),
                }
            }
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct FunctionCall {
    pub(crate) on: Box<AstNode>,
    pub(crate) method: Option<Identifier>,
    pub(crate) args: Vec<Arg>,
    pub(crate) span: Span,
}

impl core::fmt::Debug for FunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            let mut w = f.debug_struct("FunctionCall");
            w.field("on", &self.on);
            if let Some(method) = &self.method {
                w.field("method", &method.key);
            }
            if !self.args.is_empty() {
                w.field("args", &self.args);
            }
            w.finish()
        } else {
            f.debug_struct("FunctionCall")
                .field("on", &self.on)
                .field("method", &self.method)
                .field("args", &self.args)
                .field("span", &self.span)
                .finish()
        }
    }
}

impl Eval for FunctionCall {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let (this, func) = self.resolve(ctxt)?;

        match func {
            Func::Func(func) => {
                // build the function call context
                // FIXME: normal function call should have a new context, but what about closures ?
                let mut call_context =
                    self.build_call_context(ctxt, func.params(), this, &self.span)?;

                match func.call(&mut call_context) {
                    v @ Ok(_) => v,
                    e @ Err(EvalStop::Error(_)) => e,
                    Err(EvalStop::Return(value)) => Ok(value),
                    Err(EvalStop::Break) => todo!(),
                    Err(EvalStop::Continue) => todo!(),
                    Err(EvalStop::Throw) => todo!(),
                }
            }
            Func::BuiltInFunc(func) => self.eval_builtin(ctxt, &func, this, &self.span),
        }
    }
}

impl FunctionCall {
    fn build_call_context(
        &self,
        ctxt: &mut Context,
        params: &[Param],
        this: Option<Value>,
        call_span: &Span,
    ) -> Result<Context, EvalStop> {
        let mut call_context = ctxt.new_function_call_ctxt();

        if let Some(this) = this {
            call_context.declare_this(this);
        }

        let mut positional_mode = true;
        for (i, arg) in self.args.iter().enumerate() {
            if positional_mode {
                let Some(param) = params.get(i) else {
                    return Err(
                        InternalProgramError::FunctionCallTooManyArgs { span: arg.span() }.into(),
                    );
                };

                if arg.name.is_some() {
                    positional_mode = false;
                } else {
                    call_context.declare(param.key().clone(), arg.value.eval(ctxt)?);
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

                call_context.declare(arg_name.key.clone(), arg.value.eval(ctxt)?);
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
                        // FIXME: defaul val should be a constant Value and not need evaluating ?
                        call_context.declare(var.ident.key.clone(), val.eval(ctxt)?);
                    }
                }
                Param::OtherPositional(_) => {}
                Param::OtherNamed(_) => {}
            }
        }

        Ok(call_context)
    }

    fn eval_builtin(
        &self,
        ctxt: &mut Context,
        func: &BuiltInFunc,
        this: Option<Value>,
        span: &Span,
    ) -> Result<Value, EvalStop> {
        // build the function call context
        // FIXME: normal function call should have a new context, but what about closures ?
        let mut call_context = ctxt.new_function_call_ctxt();

        if let Some(this) = this {
            call_context.declare_this(this);
        }

        // evaluate the args
        let mut args = Vec::with_capacity(self.args.len());
        for arg_def in &self.args {
            args.push(arg_def.eval(ctxt)?);
        }

        match func.call(&mut call_context, args, span) {
            v @ Ok(_) => v,
            e @ Err(EvalStop::Error(_)) => e,
            Err(EvalStop::Return(value)) => Ok(value),
            Err(EvalStop::Break) => todo!(),
            Err(EvalStop::Continue) => todo!(),
            Err(EvalStop::Throw) => todo!(),
        }
    }

    fn resolve(&self, ctxt: &mut Context) -> Result<(Option<Value>, Func), EvalStop> {
        let on = self.on.eval(ctxt)?;

        if let Some(method) = &self.method {
            match &on {
                Value::Dict(dict) => {
                    let maybe_func = dict.borrow().get(&method.key);
                    if let Some(maybe_func) = maybe_func {
                        if let Value::Func(func) = maybe_func {
                            return Ok((Some(on), func));
                        } else {
                            return Err(InternalProgramError::ExpectedFunction {
                                got: maybe_func.value_type(),
                                span: method.span,
                            }
                            .into_stop());
                        }
                    };
                    if let Some(func) = Dict::built_in_func(&method.key) {
                        return Ok((Some(on), func));
                    }
                }
                Value::List(_) => {
                    if let Some(func) = List::built_in_func(&method.key) {
                        return Ok((Some(on), func));
                    }
                }
                _ => {}
            }

            // FIXME: better error - list all methods ?
            Err(InternalProgramError::NoSuchMethod {
                name: method.as_string(),
                from: on.value_type(),
                span: method.span,
            }
            .into_stop())
        } else if let Value::Func(func) = on {
            Ok((None, func))
        } else {
            Err(InternalProgramError::ExpectedFunction {
                got: on.value_type(),
                span: self.on.span(),
            }
            .into_stop())
        }
    }
}

accept_default!(FunctionCall, on:node, args:vec:arg,);

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

    fn span(&self) -> Span {
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

        Err(EvalStop::Return(val))
    }
}

accept_default!(ReturnStmt, value:opt:node,);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        lang::Span,
        parser::tests::{arg, i, id, nil, s, var},
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
        let params = vec![Param::Required(var("aa")), Param::Required(var("bb"))];
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
        let params = vec![Param::Required(var("aa")), Param::Required(var("bb"))];
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
        let params = vec![
            Param::Optional(var("aa"), s("x").into()),
            Param::Optional(var("bb"), s("y").into()),
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
        let params = vec![
            Param::Optional(var("aa"), s("x").into()),
            Param::Optional(var("bb"), s("y").into()),
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
        let params = vec![
            Param::Optional(var("aa"), s("x").into()),
            Param::Optional(var("bb"), s("y").into()),
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
        let params = vec![
            Param::Required(var("aa")),
            Param::Required(var("bb")),
            Param::Required(var("cc")),
            Param::Optional(var("dd"), s("x").into()),
            Param::Required(var("ee")),
            Param::Optional(var("ff"), s("z").into()),
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
        let params = vec![
            Param::Optional(var("aa"), s("x").into()),
            Param::Optional(var("bb"), s("y").into()),
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

        let EvalStop::Error(InternalProgramError::FunctionCallTooManyArgs { span }) = err else {
            panic!("unexpected error: {err:#?}");
        };

        assert_eq!(span, Span::new(7, 7));
    }

    #[test]
    fn test_build_call_context_error_positional_after_named() {
        let params = vec![
            Param::Optional(var("aa"), s("x").into()),
            Param::Optional(var("bb"), s("y").into()),
            Param::Optional(var("cc"), s("z").into()),
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
        let params = vec![
            Param::Optional(var("aa"), s("x").into()),
            Param::Optional(var("bb"), s("y").into()),
            Param::Optional(var("cc"), s("z").into()),
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
        let params = vec![
            Param::Optional(var("aa"), s("x").into()),
            Param::Optional(var("bb"), s("y").into()),
            Param::Optional(var("cc"), s("z").into()),
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
        let params = vec![
            Param::Required(var("aa")),
            Param::Required(var("bb")),
            Param::Optional(var("cc"), s("z").into()),
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
            on: Box::new(s("dummy").into()),
            method: None,
            args,
            span: Span::new(999, 1),
        }
    }

    fn prepare_ctxt() -> Context {
        let mut ctxt = Context::default();
        ctxt.set(&"old".into(), "dummy".into());
        ctxt
    }
}
