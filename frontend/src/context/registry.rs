use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;

use anyhow::{anyhow, Result};

#[derive(Debug)]
pub struct Registry<Idx, T: ?Sized> {
    data: RefCell<HashMap<Idx, Box<T>>>,
}

impl<Idx: Eq + Hash + Copy, T: ?Sized> Registry<Idx, T> {
    pub fn new() -> Self {
        Registry {
            data: RefCell::new(HashMap::new()),
        }
    }

    pub fn get(&self, id: Idx) -> Option<&T> {
        let data = &*self.data.borrow();
        if let Some(ptr) = data.get(&id).map(|x| x) {
            // this is safe because we return a reference to the heap, whose lifetime is the same as the registry
            Some(unsafe { &*(ptr.as_ref() as *const T) })
        } else {
            None
        }
    }

    pub fn get_mut(&mut self, id: Idx) -> Option<&mut T> {
        let data = &mut *self.data.borrow_mut();
        if let Some(ptr) = data.get_mut(&id).map(|x| x) {
            Some(unsafe { &mut *(ptr.as_mut() as *mut T) })
        } else {
            None
        }
    }

    pub fn insert_with(&self, id: Idx, value: Box<T>) -> Result<()> {
        let mut data = self.data.borrow_mut();
        if data.contains_key(&id) {
            return Err(anyhow!("cannot insert duplicate key"));
        }

        data.insert(id, value);
        Ok(())
    }

    pub fn get_insert_with(&self, id: Idx, f: impl FnOnce() -> Box<T>) -> &T {
        if let Some(x) = self.get(id) {
            return x;
        } else {
            let mut data = self.data.borrow_mut();
            if !data.contains_key(&id) {
                data.insert(id, f());
            }
        }
        self.get(id).unwrap()
    }

    pub fn pop(&mut self, id: Idx) -> Option<Box<T>> {
        self.data.borrow_mut().remove(&id)
    }
}
