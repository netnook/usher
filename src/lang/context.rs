use crate::lang::{Key, This, Value};
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
    pub fn get(&self, key: &Key) -> Option<Value> {
        self.inner.borrow().get(key)
    }

    pub fn contains_key(&mut self, key: &Key) -> bool {
        self.inner.borrow().contains_key(key)
    }

    pub fn set(&mut self, key: &Key, value: Value) {
        if key.is_this() {
            panic!("should not be able to modify 'this'")
        }
        self.inner.borrow_mut().set(key, value);
    }

    pub fn declare(&mut self, key: Key, value: Value) {
        if key.is_this() {
            panic!("should not be able to declare 'this'")
        }
        self.inner.borrow_mut().declare(key, value);
    }

    pub(crate) fn get_this(&self) -> Option<Value> {
        self.inner.borrow().get(&This::key())
    }

    pub(crate) fn declare_this(&self, value: Value) {
        self.inner.borrow_mut().declare(This::key(), value);
    }

    #[allow(dead_code)]
    pub(crate) fn size(&self) -> usize {
        self.inner.borrow().len()
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
    pub fn get_stdout(&mut self) -> Output {
        self.inner.borrow_mut().stdout.clone()
    }
    pub fn get_stderr(&mut self) -> Output {
        self.inner.borrow_mut().stderr.clone()
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
    pub vars: HashMap<Key, Value>,
    pub stdout: Output,
    pub stderr: Output,
}

impl ContextInner {
    fn get(&self, key: &Key) -> Option<Value> {
        if let Some(v) = self.vars.get(key) {
            return Some(v.ref_clone());
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().get(key);
        }
        None
    }

    fn contains_key(&self, key: &Key) -> bool {
        if self.vars.contains_key(key) {
            return true;
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().contains_key(key);
        }
        false
    }

    fn set(&mut self, key: &Key, value: Value) {
        if let Some(v) = self.do_set(key, value) {
            self.vars.insert(key.clone(), v);
        }
    }

    fn do_set(&mut self, key: &Key, value: Value) -> Option<Value> {
        if self.vars.contains_key(key) {
            // FIXME: can we avoid cloning key if alrady in ctxt ?
            self.vars.insert(key.clone(), value);
            return None;
        }
        if let Some(parent) = &self.parent {
            return parent.borrow_mut().do_set(key, value);
        }
        Some(value)
    }

    fn declare(&mut self, key: Key, value: Value) {
        self.vars.insert(key, value);
    }

    fn len(&self) -> usize {
        self.vars.len()
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
