use std::{
    cell::RefCell,
    collections::{HashMap, LinkedList},
    rc::Rc,
};

use crate::LLVMValue;

#[derive(Debug, Clone)]
pub struct VarTable<'ctx>(LinkedList<Rc<RefCell<HashMap<String, LLVMValue<'ctx>>>>>);

impl<'ctx> VarTable<'ctx> {
    pub fn new() -> Self {
        let mut list = LinkedList::new();
        list.push_front(Rc::new(RefCell::new(HashMap::new())));

        Self(list)
    }

    pub fn with_super(sup: &Self) -> Self {
        let mut list = sup.0.clone();
        list.push_front(Rc::new(RefCell::new(HashMap::new())));

        Self(list)
    }

    /// Search for a variable in all scopes.
    pub fn get(&self, name: &str) -> Option<LLVMValue<'ctx>> {
        self.0
            .iter()
            .find_map(|map| map.borrow().get(name).cloned())
    }

    /// Insert a new variable into the current scope.
    pub fn insert<S: Into<String>>(&mut self, name: S, val: LLVMValue<'ctx>) {
        self.0
            .front()
            .map(|map| map.borrow_mut().insert(name.into(), val));
    }
}
