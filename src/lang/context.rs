use crate::lang::{Identifier, THIS, Value};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Default)]
pub struct Context {
    pub inner: Rc<RefCell<ContextInner>>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get(&mut self, ident: &Identifier) -> Option<Value> {
        self.inner.borrow().get(&ident.name)
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

    pub(crate) fn new_child(&self) -> Self {
        Self {
            inner: Rc::new(RefCell::new(ContextInner {
                parent: Some(Rc::clone(&self.inner)),
                vars: HashMap::default(),
            })),
        }
    }
}

#[derive(Debug, Default)]
pub struct ContextInner {
    pub parent: Option<Rc<RefCell<ContextInner>>>,
    pub vars: HashMap<String, Value>,
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
}
