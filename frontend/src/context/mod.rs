use std::any::{self, Any, TypeId};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;
use std::sync::atomic::AtomicU32;
use std::sync::Mutex;

use derive_new::new;

mod registry;

use im::Vector;
use registry::Registry;


pub trait DefaultIdentity: 'static {
    fn default_hash(&self) -> u64;
    fn default_compare(&self, other: &dyn DefaultIdentity) -> bool;
    fn as_any(&self) -> &dyn Any;
}

impl<T: Hash + Eq + 'static> DefaultIdentity for T {
    fn default_hash(&self) -> u64 {
        use std::hash::Hasher;
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }

    fn default_compare(&self, other: &dyn DefaultIdentity) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<T>() {
            self == other
        } else {
            false
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl PartialEq for dyn DefaultIdentity {
    fn eq(&self, other: &Self) -> bool {
        self.default_compare(other)
    }
}

impl Eq for dyn DefaultIdentity {}

impl Hash for dyn DefaultIdentity {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.default_hash().hash(state);
    }
}


/// Attributes are information computed in the passes
/// they are uniqued by type to each node
/// as_str expects a single line string
pub trait Attribute: 'static {
    fn as_any(&self) -> &dyn Any;
    fn as_str(&self) -> String;
    fn name(&self) -> &'static str;
}

impl<T: Display + 'static> Attribute for T {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn as_str(&self) -> String {
        self.to_string()
    }

    fn name(&self) -> &'static str {
        std::any::type_name::<T>()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(u32);


pub struct IR {
    globals: HashMap<any::TypeId, Box<dyn Any>>,
    attributes: Registry<u32, Registry<any::TypeId, dyn Attribute>>,
    registry: Registry<u32, dyn Any>,
    unique_registry: RefCell<HashMap<Box<dyn DefaultIdentity>, u32>>,
    id: AtomicU32
}

impl IR {
    fn new_id(&self) -> NodeId {
        NodeId(self.id.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }

    pub fn insert_resource(&mut self, value: impl Any) {
        self.globals.insert(value.type_id(), Box::new(value));
    }

    pub fn get_global<T: 'static>(&self) -> Option<&T> {
        self.globals
            .get(&std::any::TypeId::of::<T>())
            .and_then(|any| any.downcast_ref::<T>())
    }

    pub fn get_global_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.globals
            .get_mut(&std::any::TypeId::of::<T>())
            .and_then(|any| any.downcast_mut::<T>())
    }

}

#[macro_export]
macro_rules! impl_id {
    ($id:ident, $node:ident) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $id(NodeID);

        impl $id {
            
        }
    };
}