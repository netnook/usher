use crate::lang::{
    AstNode, Block, Context, Dict, Eval, EvalStop, Identifier, InternalProgramError, KeyValue,
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
                w.field("name", &name.ident.name);
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
pub struct Param {
    pub(crate) name: Var,
    pub(crate) default_value: Option<AstNode>,
}

impl core::fmt::Debug for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let minimal = f.sign_minus();
        if minimal {
            if let Some(default_value) = &self.default_value {
                self.name.ident.name.fmt(f)?;
                f.write_str(": ")?;
                default_value.fmt(f)
            } else {
                self.name.ident.name.fmt(f)
            }
        } else {
            f.debug_struct("Param")
                .field("name", &self.name)
                .field("default_value", &self.default_value)
                .finish()
        }
    }
}

accept_default!(Param, name:var, default_value:opt:node,);

impl Eval for Rc<FunctionDef> {
    fn eval(&self, ctxt: &mut Context) -> Result<Value, EvalStop> {
        let f = Value::Func(Func::Func(Rc::clone(self)));
        if let Some(name) = &self.name {
            ctxt.set(&name.ident, f.clone());
        };
        Ok(f)
    }
}

impl FunctionDef {
    pub fn call(
        &self,
        ctxt: &mut Context,
        args: Vec<Value>,
        _span: &Span,
    ) -> Result<Value, EvalStop> {
        let param_count = self.params.len();
        let arg_count = args.len();
        let mut positional_count = usize::min(param_count, arg_count);

        // assign positional args
        for i in 0..positional_count {
            if i == param_count && i == arg_count {
                break;
            } else if i == param_count {
                todo!("too many arguments")
            } else if i == arg_count {
                todo!("no more args, fill in defaults")
            }
            let param = &self.params[i];
            let arg = &args[i];

            if let Value::KeyValue(_) = arg {
                positional_count = i;
                break;
            }

            ctxt.declare(&param.name.ident, arg.clone());
        }

        // set defaults for remaing args
        for i in positional_count..param_count {
            let param = &self.params[i];

            let Some(default_value) = &param.default_value else {
                todo!("no value specified for parameter")
            };

            // FIXME: default value should always be constant/literal
            let default_value = default_value.eval(&mut Context::default())?;

            ctxt.declare(&param.name.ident, default_value);
        }

        // set named arg values
        for i in positional_count..arg_count {
            let arg = &args[i];

            let Value::KeyValue(arg) = arg else {
                todo!("Expected only key value pairs after first one")
            };

            for param in &self.params[0..positional_count] {
                if arg.key == param.name.ident.name {
                    todo!("attempt to assign named arg to param already assigned positionally")
                }
            }

            // FIXME: declare/ctxt should take Rc<String> rather than Ident (so that span in not needed)
            let arg_ident = Identifier::new((*arg.key).clone(), Span::new(888, 888));
            if !ctxt.contains_key(&arg_ident) {
                todo!("unknown argument {k}", k = arg.key)
            }

            // FIXME: would be good if we could avoid cloning these value here
            ctxt.declare(&arg_ident, arg.value.clone());
        }

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
    pub fn by_name(key: &str) -> Option<BuiltInFunc> {
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
                w.field("method", &method.name);
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

        // evaluate the args
        let mut args = Vec::with_capacity(self.args.len());
        for arg_def in &self.args {
            args.push(arg_def.eval(ctxt)?);
        }

        // FIXME: normal function call should have a new context, but what about closures ?
        let mut context = ctxt.new_function_call_ctxt();

        if let Some(this) = this {
            context.declare_this(this);
        }

        match func.call(&mut context, args, &self.span) {
            v @ Ok(_) => v,
            e @ Err(EvalStop::Error(_)) => e,
            Err(EvalStop::Return(value)) => Ok(value),
            Err(EvalStop::Break) => todo!(),
            Err(EvalStop::Continue) => todo!(),
            Err(EvalStop::Throw) => todo!(),
        }
    }
}

impl FunctionCall {
    fn resolve(&self, ctxt: &mut Context) -> Result<(Option<Value>, Func), EvalStop> {
        let on = self.on.eval(ctxt)?;

        if let Some(method) = &self.method {
            match &on {
                Value::Dict(dict) => {
                    let maybe_func = dict.borrow().get(&method.name);
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
                    if let Some(func) = Dict::built_in_func(&method.name) {
                        return Ok((Some(on), func));
                    }
                }
                Value::List(_) => {
                    if let Some(func) = List::built_in_func(&method.name) {
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
                name.name.fmt(f)?;
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
                key: name.name.clone(),
                value: val,
            })))
        } else {
            self.value.eval(ctxt)
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
mod tests {}
