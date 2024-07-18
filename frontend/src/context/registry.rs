use std::cell::RefCell;
use std::hash::Hash;
use std::sync::atomic::AtomicU32;
use std::sync::{Arc, Mutex};
use std::collections::HashMap;


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NodeID {
    pub id: u32,
}

#[derive(Debug)]
pub struct Registry<T: ?Sized> {
    data: RefCell<HashMap<u32, Box<T>>>,
    id: AtomicU32,
}

impl<T: ?Sized> Registry<T> {
    pub fn new() -> Self {
        Registry {
            data: RefCell::new(HashMap::new()),
            id: AtomicU32::new(1), // treat 0 as None, or empty, since Default for NodeID is 0
        }
    }

    pub fn temporary_id(&self) -> NodeID {
        let id = self.id.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
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

    pub fn insert_with(&self, id: NodeID, value: Box<T>) {
        self.data.borrow_mut().insert(id.id, value);
    }

    pub fn insert(&self, value: Box<T>) -> NodeID {
        let id = self.id.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        self.data.borrow_mut().insert(id, value);
        let id = NodeID { id };
        id
    }

    pub fn pop(&mut self, id: NodeID) -> Option<Box<T>> {
        self.data.borrow_mut().remove(&id.id)
    }

    pub fn id(&self) -> &AtomicU32 {
        &self.id
    }
}

use crate::query::DefaultIdentity;

pub struct GenericUniqueRegistry {
    data: Mutex<HashMap<u32, Arc<dyn DefaultIdentity>>>,
    rev: Mutex<HashMap<Arc<dyn DefaultIdentity>, NodeID>>,
    id: AtomicU32
}

impl GenericUniqueRegistry {
    pub fn new() -> Self {
        GenericUniqueRegistry {
            data: Mutex::new(HashMap::new()),
            rev: Mutex::new(HashMap::new()),
            id: AtomicU32::new(1),
        }
    }

    // pub fn contains<T>(&self, value: &T) -> bool
    // where
    //     T: DefaultIdentity,
    // {
    //     let val: &dyn DefaultIdentity = value;
    //     self.rev.lock().unwrap().contains_key(val)
    // }

    pub fn delete(&mut self, id: NodeID) {
        let value = self.data.get_mut().unwrap().remove(&id.id).unwrap();
        let val: &dyn DefaultIdentity = value.as_ref();
        self.rev.get_mut().unwrap().remove(val);
    }

    pub fn insert<T: DefaultIdentity>(&self, value: T) -> NodeID {
        let val: &dyn DefaultIdentity = &value;
        if let Some(id) = self.rev.lock().unwrap().get(val) {
            return *id;
        }
        let value: Arc<dyn DefaultIdentity> = Arc::new(value);
        let id = self.id.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        self.data.lock().unwrap().insert(id, value.clone());
        self.rev.lock().unwrap().insert(value, NodeID { id });
        let id = NodeID { id };
        id
    }

    pub fn insert_with(&self, id: NodeID, value: impl DefaultIdentity) -> Result<(), &'static str> {
        let val: &dyn DefaultIdentity = &value;
        let mut rev = self.rev.lock().unwrap();
        let mut data = self.data.lock().unwrap();
        if rev.contains_key(val) {
            return Err("Value already exists in registry");
        }
        if id.id >= self.id.load(std::sync::atomic::Ordering::SeqCst) {
            return Err("ID out of bounds");
        }

        let value: Arc<dyn DefaultIdentity> = Arc::new(value);
        data.insert(id.id, value.clone());
        rev.insert(value, id);
        Ok(())
    }

    pub fn get(&self, id: NodeID) -> Option<&dyn DefaultIdentity> {
        self.data.lock().unwrap().get(&id.id).map(|x| {
            let ptr = x.as_ref() as *const dyn DefaultIdentity;
            unsafe { &*ptr }
        })
    }

    pub fn temporary_id(&self) -> NodeID {
        let id = self.id.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        NodeID { id }
    }

    pub fn id(&self) -> &AtomicU32 {
        &self.id
    }
}

