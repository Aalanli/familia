use std::any::{self, Any, TypeId};
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::sync::Mutex;

mod registry;

use im::Vector;
pub use registry::NodeID;
use registry::{GenericUniqueRegistry, Registry};


pub trait IDVisitor {
    fn visit<I: ID>(&mut self, id: I);
}

pub trait IRNode {
    fn visit_ids<V: IDVisitor>(&self, v: &mut V);
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

pub struct IR {
    globals: HashMap<any::TypeId, Box<dyn Any>>,
    attributes: HashMap<NodeID, HashMap<any::TypeId, Box<dyn Attribute>>>,
    registry: Registry<dyn Any>,
    unique_registry: GenericUniqueRegistry,
    ids: Mutex<HashMap<TypeId, Vector<NodeID>>>,
}

impl IR {
    pub fn new() -> Self {
        IR {
            globals: HashMap::new(),
            registry: Registry::new(),
            attributes: HashMap::new(),
            unique_registry: GenericUniqueRegistry::new(),
            ids: Mutex::new(HashMap::new())
        }
    }

    pub fn insert_global(&mut self, value: impl Any) {
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

    pub fn delete<I: ID>(&mut self, id: I) {
        if I::IS_UNIQUE {
            self.unique_registry.delete(id.get());
        } else {
            self.registry.pop(id.get());
        }
        let mut ids = self.ids.lock().unwrap();
        if let Some(ids) = ids.get_mut(&id.type_id()) {
            if let Ok(id) = ids.binary_search(&id.get()) {
                ids.remove(id);
            }
        }
    }

    pub fn get<I: ID>(&self, id: I) -> &I::Node {
        if I::IS_UNIQUE {
            let res = self.unique_registry.get(id.get()).unwrap();
            res.as_any().downcast_ref::<I::Node>().unwrap()
        } else {
            let any = self.registry.get(id.get()).unwrap();
            any.downcast_ref::<I::Node>().unwrap()
        }
    }

    pub fn get_mut<I: ID>(&mut self, id: I) -> &mut I::Node {
        // a hacky way to enforce during compile time that I is not unique
        let _ = I::CHECK;
        let any = self.registry.get_mut(id.get()).unwrap();
        any.downcast_mut::<I::Node>().unwrap()
    }

    pub fn insert<I: ID>(&self, node: I::Node) -> I {
        let id;
        if I::IS_UNIQUE {
            id = self.unique_registry.insert(node);
        } else {
            id = self.registry.insert(Box::new(node));
        }

        let mut ids = self.ids.lock().unwrap();
        let tid = TypeId::of::<I>();
        if !ids.contains_key(&tid) {
            ids.insert(tid, Vector::new());
        }
        ids.get_mut(&tid).unwrap().push_back(id);

        I::wrap(id)
    }

    pub fn insert_with<I: ID>(&self, id: I, node: I::Node) {
        if I::IS_UNIQUE {
            self.unique_registry.insert_with(id.get(), node).unwrap();
        } else {
            self.registry.insert_with(id.get(), Box::new(node));
        }

        let mut ids = self.ids.lock().unwrap();
        let tid = TypeId::of::<I>();
        if !ids.contains_key(&tid) {
            ids.insert(tid, Vector::new());
        }
        ids.get_mut(&tid).unwrap().insert_ord(id.get());
    }

    pub fn temporary_id<I: ID>(&self) -> I {
        if I::IS_UNIQUE {
            I::wrap(self.unique_registry.temporary_id())
        } else {
            I::wrap(self.registry.temporary_id())
        }
    }

    pub fn insert_attribute<A: Attribute, I: ID>(&mut self, id: I, attr: A) {
        let map = self.attributes.entry(id.get()).or_insert_with(HashMap::new);
        map.insert(any::TypeId::of::<A>(), Box::new(attr));
    }

    pub fn get_attribute<A: Attribute, I: ID>(&self, id: I) -> Option<&A> {
        let map = self.attributes.get(&id.get())?;
        map.get(&any::TypeId::of::<A>())?
            .as_any()
            .downcast_ref::<A>()
    }

    pub fn iter<I: ID>(&self) -> impl Iterator<Item = I> {
        let mut ids = self.ids.lock().unwrap();
        let tid = TypeId::of::<I>();
        if !ids.contains_key(&tid) {
            ids.insert(tid, Vector::new());
        }
        ids.get(&tid).unwrap().clone().into_iter().map(|x| I::wrap(x))
    }
}

pub trait ID: Copy + Eq + Ord + Hash + 'static {
    type Node: IRNode + Hash + Eq + 'static;
    const IS_UNIQUE: bool;
    fn get(&self) -> NodeID;
    fn wrap(node: NodeID) -> Self;
}

trait CheckNotUnique: ID {
    const CHECK: ();
}

impl<T: ID> CheckNotUnique for T {
    const CHECK: () = [()][T::IS_UNIQUE as usize];
}

#[macro_export]
macro_rules! impl_id {
    ($id:ident, $node:ident, $unique:ident) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $id(NodeID);

        impl ID for $id {
            type Node = $node;
            const IS_UNIQUE: bool = $unique;
            fn id(&self) -> NodeID {
                self.0
            }

            fn new(node: NodeID) -> Self {
                $id(node)
            }
        }
    };
}

