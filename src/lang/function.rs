use crate::lang::{
    AstNode, Block, Context, Eval, EvalStop, Identifier, InternalProgramError, Span, Value, Var,
    accept_default,
    value::{Func, ValueType},
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
        params: Vec<Value>,
        span: &Span,
    ) -> Result<Value, EvalStop> {
        let _ = ctxt;
        let _ = params;
        let _ = span;
        todo!()
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum BuiltInFunc {
    Print,
    Add,
}

impl BuiltInFunc {
    pub fn by_name(key: &str) -> Option<BuiltInFunc> {
        match key {
            "print" => Some(BuiltInFunc::Print),
            "add" => Some(BuiltInFunc::Add),
            _ => None,
        }
    }

    pub fn call(
        &self,
        ctxt: &mut Context,
        params: Vec<Value>,
        span: &Span,
    ) -> Result<Value, EvalStop> {
        match self {
            BuiltInFunc::Print => {
                for p in params {
                    println!("got {}", p.as_string()?);
                }
                Ok(Value::Nil)
            }
            BuiltInFunc::Add => {
                let this = ctxt.get_this().unwrap_or(Value::Nil);
                match this {
                    Value::List(list) => {
                        let mut list = list.borrow_mut();
                        for v in params {
                            list.add(v);
                        }
                        Ok(Value::Nil)
                    }
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
        let on = self.on.eval(ctxt)?;

        // get the method
        let (this, func) = match &self.method {
            Some(method_name) => {
                let method = match &on {
                    Value::Dict(on) => {
                        on.borrow()
                            .get(&method_name.name)
                            .map(|v| match v {
                                Value::Func(f) => Ok(f),
                                other => InternalProgramError::ExpectedFunction {
                                    got: other.value_type(),
                                    span: Span::new(0, 0),
                                }
                                .into(),
                            })
                            .transpose()?
                            .or_else(|| BuiltInFunc::by_name(&method_name.name).map(|f| f.into()))
                            .ok_or_else(|| {
                                InternalProgramError::NoSuchMethod {
                                    name: method_name.name.clone(),
                                    from: ValueType::Dict,
                                    span: Span::new(0, 0), // FIXME: fix span
                                }
                                .into_stop()
                            })?
                    }
                    Value::List(_) => {
                        BuiltInFunc::by_name(&method_name.name)
                            .map(|f| f.into())
                            .ok_or_else(|| {
                                InternalProgramError::NoSuchMethod {
                                    name: method_name.name.clone(),
                                    from: ValueType::List,
                                    span: Span::new(0, 0), // FIXME: fix span
                                }
                                .into_stop()
                            })?
                    }
                    _ => {
                        return InternalProgramError::NoSuchMethod {
                            name: method_name.name.clone(),
                            from: on.value_type(),
                            span: Span::new(0, 0), // FIXME: fix span
                        }
                        .into();
                    }
                };
                (on, method)
            }
            None => {
                let func = match on {
                    Value::Func(func) => func,
                    _ => {
                        return InternalProgramError::ExpectedFunction {
                            got: on.value_type(),
                            span: Span::new(0, 0),
                        }
                        .into();
                    }
                };
                (Value::Nil, func)
            }
        };

        // evaluate the args
        let mut args = Vec::with_capacity(self.args.len());
        for arg_def in &self.args {
            args.push(arg_def.eval(ctxt)?);
        }

        // FIXME: normal function call should have a new context, but what about closures ?
        let mut context = Context::new();
        context.declare_this(this);

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
        if self.name.is_some() {
            todo!("named arg not handleld");
        }
        self.value.eval(ctxt)
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
