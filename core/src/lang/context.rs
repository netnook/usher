use crate::lang::{Key, This, Value};
use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};
use thiserror::Error;

#[derive(Error, Debug)]
#[error("Name already declared.")]
pub struct AlreadyDeclared {}

#[derive(Error, Debug)]
#[error("Name not declared.")]
pub struct NotDeclared {}

#[derive(Debug, Clone)]
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

    pub fn set(&mut self, key: &Key, value: Value) -> Result<(), NotDeclared> {
        if key.is_this() {
            panic!("should not be able to modify 'this'")
        }
        self.inner.borrow_mut().set(key, value)
    }

    pub fn declare(&mut self, key: Key, value: Value) -> Result<(), AlreadyDeclared> {
        if key.is_this() {
            panic!("should not be able to declare 'this'")
        }
        self.inner.borrow_mut().declare(key, value)
    }

    pub(crate) fn get_this(&self) -> Option<Value> {
        self.inner.borrow().get(&This::key())
    }

    pub(crate) fn declare_this(&self, value: Value) -> Result<(), AlreadyDeclared> {
        self.inner.borrow_mut().declare(This::key(), value)
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
}

#[derive(Debug)]
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

    fn set(&mut self, key: &Key, value: Value) -> Result<(), NotDeclared> {
        if let Some(v) = self.vars.get_mut(key) {
            *v = value;
            return Ok(());
        };
        if let Some(parent) = &self.parent {
            return parent.borrow_mut().set(key, value);
        }
        Err(NotDeclared {})
    }

    fn declare(&mut self, key: Key, value: Value) -> Result<(), AlreadyDeclared> {
        if self.vars.contains_key(&key) {
            return Err(AlreadyDeclared {});
        }
        self.vars.insert(key, value);
        Ok(())
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
}

#[derive(Clone, Debug)]
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_declared_get_set_direct() {
        let key = "key".into();
        let val1: Value = "val1".into();
        let val2: Value = "val2".into();

        // ctxt.set(&key, val)
        let mut ctxt = Context::default();

        // before declaration
        assert_eq!(ctxt.get(&key), None);
        ctxt.set(&key, val1.clone())
            .expect_err("set undeclared var should error");

        // after declaration
        ctxt.declare(key.clone(), val1.clone()).unwrap();
        assert_eq!(ctxt.get(&key), Some(val1.clone()));

        // modify
        ctxt.set(&key, val2.clone()).unwrap();
        assert_eq!(ctxt.get(&key), Some(val2.clone()));

        // attempt to re-declare
        ctxt.declare(key, val1.clone()).expect_err("");
    }

    #[test]
    fn test_declared_get_set_direct_with_parent() {
        let key = "key".into();
        let val1: Value = "val1".into();
        let val2: Value = "val2".into();

        // ctxt.set(&key, val)
        let mut parent_ctxt = Context::default();
        let mut ctxt = parent_ctxt.new_scope();

        // before declaration
        assert_eq!(parent_ctxt.get(&key), None);
        assert_eq!(ctxt.get(&key), None);
        parent_ctxt
            .set(&key, val1.clone())
            .expect_err("set undeclared var should error");
        ctxt.set(&key, val1.clone())
            .expect_err("set undeclared var should error");

        // after declaration
        ctxt.declare(key.clone(), val1.clone()).unwrap();
        assert_eq!(parent_ctxt.get(&key), None);
        assert_eq!(ctxt.get(&key), Some(val1.clone()));

        // modify
        ctxt.set(&key, val2.clone()).unwrap();
        assert_eq!(parent_ctxt.get(&key), None);
        assert_eq!(ctxt.get(&key), Some(val2.clone()));

        // attempt to re-declare
        ctxt.declare(key.clone(), val1.clone()).expect_err("");

        // attempt to declare on parent
        parent_ctxt.declare(key.clone(), val1.clone()).unwrap();
        assert_eq!(ctxt.get(&key), Some(val2.clone()));
        assert_eq!(parent_ctxt.get(&key), Some(val1.clone()));
    }

    #[test]
    fn test_declared_get_set_inderict() {
        let key = "key".into();
        let val1: Value = "val1".into();
        let val2: Value = "val2".into();
        let val3: Value = "val3".into();

        // ctxt.set(&key, val)
        let mut parent_ctxt = Context::default();
        let mut ctxt = parent_ctxt.new_scope();

        // before declaration
        assert_eq!(parent_ctxt.get(&key), None);
        assert_eq!(ctxt.get(&key), None);
        parent_ctxt
            .set(&key, val1.clone())
            .expect_err("set undeclared var should error");
        ctxt.set(&key, val1.clone())
            .expect_err("set undeclared var should error");

        // after declaration
        parent_ctxt.declare(key.clone(), val1.clone()).unwrap();
        assert_eq!(parent_ctxt.get(&key), Some(val1.clone()));
        assert_eq!(ctxt.get(&key), Some(val1.clone()));

        // modify on parent
        parent_ctxt.set(&key, val2.clone()).unwrap();
        assert_eq!(parent_ctxt.get(&key), Some(val2.clone()));
        assert_eq!(ctxt.get(&key), Some(val2.clone()));

        // modify on child
        ctxt.set(&key, val3.clone()).unwrap();
        assert_eq!(parent_ctxt.get(&key), Some(val3.clone()));
        assert_eq!(ctxt.get(&key), Some(val3.clone()));

        // attempt to re-declare
        parent_ctxt
            .declare(key.clone(), val1.clone())
            .expect_err("");

        // declare on child
        ctxt.declare(key.clone(), val1.clone()).unwrap();
        assert_eq!(parent_ctxt.get(&key), Some(val3.clone()));
        assert_eq!(ctxt.get(&key), Some(val1.clone()));
    }
}
