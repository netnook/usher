use crate::lang::{Identifier, THIS, Value};
use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};

#[derive()]
pub struct Context {
    inner: Rc<RefCell<ContextInner>>,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            inner: Rc::new(RefCell::new(ContextInner {
                parent: None,
                vars: HashMap::new(),
                stdout: Output::StdOut,
                stderr: Output::StdErr,
            })),
        }
    }
}

impl Context {
    pub fn get(&mut self, ident: &Identifier) -> Option<Value> {
        self.inner.borrow().get(&ident.name)
    }

    pub fn contains_key(&mut self, ident: &Identifier) -> bool {
        self.inner.borrow().contains_key(&ident.name)
    }

    pub fn set(&mut self, ident: &Identifier, value: Value) {
        if *ident.name == THIS {
            panic!("should not be able to modify 'this'")
        }
        self.inner.borrow_mut().set(&ident.name, value);
    }

    pub fn declare(&mut self, ident: &Identifier, value: Value) {
        if *ident.name == THIS {
            panic!("should not be able to declare 'this'")
        }
        self.inner.borrow_mut().declare(&ident.name, value);
    }

    pub(crate) fn get_this(&self) -> Option<Value> {
        self.inner.borrow().get(THIS)
    }

    pub(crate) fn declare_this(&self, value: Value) {
        self.inner.borrow_mut().declare(THIS, value);
    }

    pub fn reset(&mut self) {
        self.inner.borrow_mut().reset();
    }

    pub fn set_stdout(&mut self, out: Output) {
        self.inner.borrow_mut().set_stdout(out);
    }
    pub fn set_stderr(&mut self, out: Output) {
        self.inner.borrow_mut().set_stderr(out);
    }

    pub fn stdout(&mut self, str: &str) {
        self.inner.borrow_mut().stdout(str);
    }
    pub fn stderr(&mut self, str: &str) {
        self.inner.borrow_mut().stderr(str);
    }

    pub(crate) fn new_scope(&self) -> Self {
        let b = self.inner.borrow();
        Self {
            inner: Rc::new(RefCell::new(ContextInner {
                parent: Some(Rc::clone(&self.inner)),
                vars: HashMap::default(),
                stdout: b.stdout.clone(),
                stderr: b.stderr.clone(),
            })),
        }
    }

    pub(crate) fn new_function_call_ctxt(&self) -> Context {
        let b = self.inner.borrow();
        let mut ctxt = Context::default();
        ctxt.set_stdout(b.stdout.clone());
        ctxt.set_stderr(b.stderr.clone());
        ctxt
    }
}

#[derive()]
pub struct ContextInner {
    pub parent: Option<Rc<RefCell<ContextInner>>>,
    pub vars: HashMap<String, Value>,
    pub stdout: Output,
    pub stderr: Output,
}

impl ContextInner {
    fn get(&self, ident: &str) -> Option<Value> {
        if let Some(v) = self.vars.get(ident) {
            return Some(v.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().get(ident);
        }
        None
    }

    fn contains_key(&self, ident: &str) -> bool {
        if self.vars.contains_key(ident) {
            return true;
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().contains_key(ident);
        }
        false
    }

    fn set(&mut self, ident: &str, value: Value) {
        if let Some(v) = self.do_set(ident, value) {
            self.vars.insert(ident.to_string(), v);
        }
    }

    fn do_set(&mut self, ident: &str, value: Value) -> Option<Value> {
        if self.vars.contains_key(ident) {
            self.vars.insert(ident.to_string(), value);
            return None;
        }
        if let Some(parent) = &self.parent {
            return parent.borrow_mut().do_set(ident, value);
        }
        Some(value)
    }

    fn declare(&mut self, ident: &str, value: Value) {
        self.vars.insert(ident.to_string(), value);
    }

    fn reset(&mut self) {
        self.vars.clear();
    }

    fn set_stdout(&mut self, out: Output) {
        self.stdout = out;
    }
    fn set_stderr(&mut self, err: Output) {
        self.stderr = err;
    }

    fn stdout(&mut self, str: &str) {
        self.stdout.write_all(str.as_bytes()).expect("FIXME")
    }
    fn stderr(&mut self, str: &str) {
        self.stderr.write_all(str.as_bytes()).expect("FIXME")
    }
}

#[derive(Clone)]
pub enum Output {
    StdOut,
    StdErr,
    Capture(Rc<RefCell<Vec<u8>>>),
}

impl Write for Output {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Output::StdOut => std::io::stdout().write(buf),
            Output::StdErr => std::io::stderr().write(buf),
            Output::Capture(v) => {
                v.borrow_mut().extend(buf);
                Ok(buf.len())
            }
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Output::StdOut => std::io::stdout().flush(),
            Output::StdErr => std::io::stderr().flush(),
            Output::Capture(_) => Ok(()),
        }
    }
}
