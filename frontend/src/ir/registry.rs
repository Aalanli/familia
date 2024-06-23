use std::cell::RefCell;
use std::hash::Hash;
use std::rc::Rc;
use std::{cell::Cell, collections::HashMap};

use anyhow::{anyhow, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NodeID {
    pub id: u32,
}

#[derive(Debug, Clone)]
pub struct Registry<T: ?Sized> {
    data: RefCell<HashMap<u32, Box<T>>>,
    id: Cell<u32>,
}

impl<T: ?Sized> Registry<T> {
    pub fn new() -> Self {
        Registry {
            data: RefCell::new(HashMap::new()),
            id: Cell::new(1), // treat 0 as None, or empty, since Default for NodeID is 0
        }
    }

    pub fn temporary_id(&self) -> NodeID {
        let id = self.id.get();
        self.id.set(id + 1);
        NodeID { id }
    }

    pub fn get(&self, id: NodeID) -> Option<&T> {
        let data = &*self.data.borrow();
        if let Some(ptr) = data.get(&id.id).map(|x| x) {
            // this is safe because we return a reference to the heap, whose lifetime is the same as the registry
            Some(unsafe { &*(ptr.as_ref() as *const T) })
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, id: NodeID) -> Option<&mut T> {
        let data = &mut *self.data.borrow_mut();
        if let Some(ptr) = data.get_mut(&id.id).map(|x| x) {
            Some(unsafe { &mut *(ptr.as_mut() as *mut T) })
        } else {
            None
        }
    }

    // pub fn pop(&mut self, id: NodeID) -> Option<T> {
    //     self.data.borrow_mut().remove(&id.id).map(|x| *x)
    // }

    pub fn iter(&self) -> impl Iterator<Item = NodeID> {
        self.data
            .borrow()
            .keys()
            .map(|id| NodeID { id: *id })
            .collect::<Vec<_>>()
            .into_iter()
    }
}

impl<T> Registry<T> {
    pub fn insert_with(&self, id: NodeID, value: T) {
        self.data.borrow_mut().insert(id.id, Box::new(value));
    }

    pub fn insert(&self, value: T) -> NodeID {
        let id = self.id.get();
        self.data.borrow_mut().insert(id, Box::new(value));
        self.id.set(id + 1);
        NodeID { id }
    }
}

impl<T: ?Sized> Registry<T> {
    pub fn insert_with_boxed(&self, id: NodeID, value: Box<T>) {
        self.data.borrow_mut().insert(id.id, value);
    }

    pub fn insert_boxed(&self, value: Box<T>) -> NodeID {
        let id = self.id.get();
        self.data.borrow_mut().insert(id, value);
        self.id.set(id + 1);
        NodeID { id }
    }
}


use crate::transforms::query::DefaultIdentity;

#[derive(Clone)]
pub struct GenericUniqueRegistry {
    data: RefCell<HashMap<u32, Rc<dyn DefaultIdentity>>>,
    rev: RefCell<HashMap<Rc<dyn DefaultIdentity>, NodeID>>,
    id: Cell<u32>,
}

impl GenericUniqueRegistry {
    pub fn new() -> Self {
        GenericUniqueRegistry {
            data: RefCell::new(HashMap::new()),
            rev: RefCell::new(HashMap::new()),
            id: Cell::new(1),
        }
    }

    pub fn contains<T>(&self, value: &T) -> bool
    where T: DefaultIdentity {
        let val: &dyn DefaultIdentity = value;
        self.rev.borrow().contains_key(val)
    }

    pub fn insert<T: DefaultIdentity>(&self, value: T) -> NodeID {
        let val: &dyn DefaultIdentity = &value;
        if let Some(id) = self.rev.borrow().get(val) {
            return *id;
        }
        let value: Rc<dyn DefaultIdentity> = Rc::new(value);
        let id = self.id.get();
        self.data.borrow_mut().insert(id, value.clone());
        self.rev.borrow_mut().insert(value, NodeID { id });
        self.id.set(id + 1);
        NodeID { id }
    }

    pub fn insert_with(&self, id: NodeID, value: impl DefaultIdentity) -> Result<()> {
        let val: &dyn DefaultIdentity = &value;
        if self.rev.borrow().contains_key(val) {
            return Err(anyhow!("Value already exists in registry"));
        }
        if id.id >= self.id.get() {
            return Err(anyhow!("ID out of bounds"));
        }

        let value: Rc<dyn DefaultIdentity> = Rc::new(value);
        self.data.borrow_mut().insert(id.id, value.clone());
        self.rev.borrow_mut().insert(value, id);
        Ok(())
    }

    pub fn get(&self, id: NodeID) -> Option<&dyn DefaultIdentity> {
        self.data.borrow().get(&id.id).map(|x| {
            let ptr = x.as_ref() as *const dyn DefaultIdentity;
            unsafe { &*ptr }
        })
    }

    pub fn temporary_id(&self) -> NodeID {
        let id = self.id.get();
        self.id.set(id + 1);
        NodeID { id }
    }

    pub fn iter(&self) -> impl Iterator<Item = NodeID> {
        self.data
            .borrow()
            .keys()
            .map(|id| NodeID { id: *id })
            .collect::<Vec<_>>()
            .into_iter()
    }
}
#[cfg(test)]
mod test_registry {
    use super::*;

    #[test]
    fn test_registry() {
        let reg = Registry::new();
        let id1 = reg.insert(1);
        let id2 = reg.insert(2);
        assert_eq!(reg.get(id1), Some(&1));
        assert_eq!(reg.get(id2), Some(&2));
        // assert_eq!(reg.pop(id1), Some(1));
        assert_eq!(reg.get(id1), None);
        assert_eq!(reg.get(id2), Some(&2));
    }

    #[test]
    fn test_unique_registry() {
        let reg = GenericUniqueRegistry::new();
        let id1 = reg.insert(1);
        let id2 = reg.insert(2);
        assert_eq!(reg.insert(1), id1);
    }

    #[test]
    fn test_registry_mut() {
        let mut reg = Registry::new();
        let mut ids = vec![];
        for i in 0..10 {
            let id = reg.insert(i);
            ids.push(id);
            *reg.get_mut(id).unwrap() += 1;
        }

        for (i, id) in ids.into_iter().enumerate() {
            reg.insert(1);
            assert_eq!(*reg.get(id).unwrap(), i + 1);
        }
    }
}
