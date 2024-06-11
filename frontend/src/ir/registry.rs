
use std::cell::RefCell;
use std::{cell::Cell, collections::HashMap};
use std::hash::Hash;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeID {
    pub id: u32,
}

pub struct Registry<T: ?Sized> {
    data: HashMap<u32, Box<T>>,
    id: Cell<u32>,
    // mark as not syncable
    _marker: std::marker::PhantomData<*const ()>,
}


impl<T> Registry<T> {
    pub fn new() -> Self {
        Registry {
            data: HashMap::new(),
            id: Cell::new(0),
            _marker: std::marker::PhantomData,
        }
    }

    pub fn insert(&self, value: T) -> NodeID {
        let map = &self.data as *const HashMap<u32, Box<T>> as *mut HashMap<u32, Box<T>>;
        let id = self.id.get();
        unsafe { // this makes it unsafe to sync
            (*map).insert(id, Box::new(value));
        }
        self.id.set(id + 1);
        NodeID { id }
    }

    pub fn get(&self, id: NodeID) -> Option<&T> {
        self.data.get(&id.id).map(|x| &(**x))
    }

    pub fn get_mut(&mut self, id: NodeID) -> Option<&mut T> {
        self.data.get_mut(&id.id).map(|x| &mut (**x))
    }

    pub fn pop(&mut self, id: NodeID) -> Option<T> {
        self.data.remove(&id.id).map(|x| *x)
    }

    pub fn iter(&self) -> impl Iterator<Item = NodeID> {
        self.data.keys().map(|id| NodeID { id: *id }).collect::<Vec<_>>().into_iter()
    }
}

unsafe impl<T: Send> Send for Registry<T> {}

pub struct UniqueRegistry<T> {
    registry: Registry<T>,
    rev: RefCell<HashMap<T, NodeID>>,
}

impl<T: Clone + Hash + Eq> UniqueRegistry<T> {
    pub fn new() -> Self {
        UniqueRegistry {
            registry: Registry::new(),
            rev: RefCell::new(HashMap::new()),
        }
    }

    pub fn insert(&self, value: T) -> NodeID {
        if let Some(id) = self.rev.borrow().get(&value) {
            return *id;
        }
        let id = self.registry.insert(value.clone());
        self.rev.borrow_mut().insert(value, id);
        id
    }

    pub fn get(&self, id: NodeID) -> Option<&T> {
        self.registry.get(id)
    }
}


#[cfg(test)]
mod test_registry {
    use super::*;

    #[test]
    fn test_registry() {
        let mut reg = Registry::new();
        let id1 = reg.insert(1);
        let id2 = reg.insert(2);
        assert_eq!(reg.get(id1), Some(&1));
        assert_eq!(reg.get(id2), Some(&2));
        assert_eq!(reg.pop(id1), Some(1));
        assert_eq!(reg.get(id1), None);
        assert_eq!(reg.get(id2), Some(&2));
    }

    #[test]
    fn test_unique_registry() {
        let reg = UniqueRegistry::new();
        let id1 = reg.insert(1);
        let id2 = reg.insert(2);
        assert_eq!(reg.get(id1), Some(&1));
        assert_eq!(reg.get(id2), Some(&2));
        assert_eq!(reg.insert(1), id1);
    }
}
