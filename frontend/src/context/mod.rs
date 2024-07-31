use std::any::{self, Any, TypeId};
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::sync::atomic::AtomicU32;

use anyhow::Result;
use derive_new::new;

mod registry;
use registry::Registry;

pub trait DefaultIdentity {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(u32);

#[derive(new)]
pub struct Db {
    #[new(default)]
    globals: HashMap<any::TypeId, Box<dyn Any>>,
    #[new(value = "Registry::new()")]
    attributes: Registry<NodeId, Registry<any::TypeId, dyn Any>>,
    #[new(value = "Registry::new()")]
    registry: Registry<NodeId, dyn Any>,
    #[new(value = "Registry::new()")]
    unique_registry: Registry<NodeId, dyn DefaultIdentity>,
    #[new(default)]
    unique_keys: RefCell<HashMap<Box<dyn DefaultIdentity>, NodeId>>,
    #[new(default)]
    id: AtomicU32,
}

impl Db {
    fn new_id(&self) -> NodeId {
        NodeId(self.id.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }

    pub fn insert_resource(&mut self, value: impl Any) {
        self.globals.insert(value.type_id(), Box::new(value));
    }

    pub fn get_resource<T: 'static>(&self) -> Option<&T> {
        self.globals
            .get(&std::any::TypeId::of::<T>())
            .and_then(|any| any.downcast_ref::<T>())
    }

    pub fn get_resource_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.globals
            .get_mut(&std::any::TypeId::of::<T>())
            .and_then(|any| any.downcast_mut::<T>())
    }

    pub fn has_attribute<T: 'static>(&self, id: NodeId) -> bool {
        self.get_attribute::<T>(id).is_some()
    }

    pub fn add_attribute<T: 'static>(&self, id: NodeId, attr: T) -> Result<()> {
        let map = self
            .attributes
            .get_insert_with(id, || Box::new(Registry::new()));
        map.insert_with(any::TypeId::of::<T>(), Box::new(attr))
    }

    pub fn get_attribute<T: 'static>(&self, id: NodeId) -> Option<&T> {
        self.attributes
            .get(id)?
            .get(TypeId::of::<T>())?
            .downcast_ref::<T>()
    }

    pub fn pop_attribute<T: 'static>(&mut self, id: NodeId) -> Option<T> {
        let attr = self.attributes.get_mut(id)?.pop(TypeId::of::<T>())?;
        if let Ok(x) = attr.downcast::<T>() {
            Some(*x)
        } else {
            None
        }
    }

    pub fn insert_unique(&self, x: impl DefaultIdentity + Clone + 'static) -> NodeId {
        let y: &dyn DefaultIdentity = &x;
        let mut regs = self.unique_keys.borrow_mut();
        if let Some(k) = regs.get(y) {
            return *k;
        }
        let id = self.new_id();
        regs.insert(Box::new(x.clone()), id);
        self.unique_registry.insert_with(id, Box::new(x)).unwrap();
        id
    }

    pub fn insert(&self, x: impl Any) -> NodeId {
        let id = self.new_id();
        self.registry.insert_with(id, Box::new(x)).unwrap();
        id
    }

    pub fn get(&self, id: NodeId) -> Option<&dyn Any> {
        self.registry.get(id)
    }

    pub fn get_unique(&self, id: NodeId) -> Option<&dyn Any> {
        Some(self.unique_registry.get(id)?.as_any())
    }

    pub fn get_mut(&mut self, id: NodeId) -> Option<&mut dyn Any> {
        self.registry.get_mut(id)
    }

    pub fn pop(&mut self, id: NodeId) -> Box<dyn Any> {
        self.registry.pop(id).unwrap()
    }
}

#[macro_export]
macro_rules! impl_id {
    ($id:ident, $node:ty) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $id(crate::context::NodeId);

        #[allow(dead_code)]
        impl $id {
            pub fn new(ir: &crate::context::Db, node: $node) -> $id {
                $id(ir.insert(node))
            }

            pub fn get<'a>(&self, ir: &'a crate::context::Db) -> Option<&'a $node> {
                Some(ir.get(self.0)?.downcast_ref::<$node>().unwrap())
            }

            pub fn get_mut<'a>(&self, ir: &'a mut crate::context::Db) -> Option<&'a mut $node> {
                Some(ir.get_mut(self.0)?.downcast_mut::<$node>().unwrap())
            }

            pub fn pop(&self, ir: &mut crate::context::Db) -> $node {
                *ir.pop(self.0).downcast::<$node>().unwrap()
            }

            pub fn get_attribute<'a, T: 'static>(
                &self,
                ir: &'a crate::context::Db,
            ) -> Option<&'a T> {
                ir.get_attribute(self.0)
            }

            pub fn add_attribute<T: 'static>(
                &self,
                attr: T,
                ir: &crate::context::Db,
            ) -> anyhow::Result<()> {
                ir.add_attribute(self.0, attr)
            }

            pub fn has_attribute<T: 'static>(&self, ir: &crate::context::Db) -> bool {
                ir.has_attribute::<T>(self.0)
            }

            pub fn pop_attribute<T: 'static>(&self, ir: &mut crate::context::Db) -> Option<T> {
                ir.pop_attribute(self.0)
            }

            pub fn dyn_id(&self) -> crate::context::NodeId {
                self.0
            }
        }

        impl std::ops::Index<$id> for crate::context::Db {
            type Output = $node;

            fn index(&self, index: $id) -> &Self::Output {
                index.get(self).unwrap()
            }
        }

        impl std::ops::IndexMut<$id> for crate::context::Db {
            fn index_mut(&mut self, index: $id) -> &mut Self::Output {
                index.get_mut(self).unwrap()
            }
        }
    };

    ($id:ident, intern $node:ty) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $id(crate::context::NodeId);

        #[allow(dead_code)]
        impl $id {
            pub fn new(ir: &crate::context::Db, node: $node) -> $id {
                $id(ir.insert_unique(node))
            }

            pub fn get<'a>(&self, ir: &'a crate::context::Db) -> Option<&'a $node> {
                Some(ir.get_unique(self.0)?.downcast_ref::<$node>().unwrap())
            }

            pub fn get_attribute<'a, T: 'static>(
                &self,
                ir: &'a crate::context::Db,
            ) -> Option<&'a T> {
                ir.get_attribute(self.0)
            }

            pub fn add_attribute<T: 'static>(
                &self,
                attr: T,
                ir: &crate::context::Db,
            ) -> anyhow::Result<()> {
                ir.add_attribute(self.0, attr)
            }

            pub fn has_attribute<T: 'static>(&self, ir: &crate::context::Db) -> bool {
                ir.has_attribute::<T>(self.0)
            }

            pub fn pop_attribute<T: 'static>(&self, ir: &mut crate::context::Db) -> Option<T> {
                ir.pop_attribute(self.0)
            }

            pub fn dyn_id(&self) -> crate::context::NodeId {
                self.0
            }
        }

        impl std::ops::Index<$id> for crate::context::Db {
            type Output = $node;

            fn index(&self, index: $id) -> &Self::Output {
                index.get(self).unwrap()
            }
        }
    };
}

#[cfg(test)]
mod test_ctx {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Bar(i32);

    impl_id!(BarId, intern Bar);

    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
    pub struct Foo(String);

    impl_id!(FooId, Foo);

    #[test]
    fn test_db() {
        let ir = Db::new();
        let id = BarId::new(&ir, Bar(2));
        let id2 = BarId::new(&ir, Bar(2));
        assert_eq!(id, id2);
        let bar = &ir[id];
        insta::assert_debug_snapshot!(bar, @r###"
        Bar(
            2,
        )
        "###);
    }
}
