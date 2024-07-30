use std::any::{self, Any, TypeId};
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::ops::ControlFlow;
use std::sync::atomic::AtomicU32;
use std::sync::Arc;

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
    ($id:ident, $node:ident) => {
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

    ($id:ident, intern $node:ident) => {
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
            ) -> Result<()> {
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


use bevy_reflect::{FromReflect, GetTypeRegistration, Reflect, ReflectRef, TypePath};

#[derive(TypePath, Clone)]
pub struct P<T: ?Sized>{x: Box<T>}

#[allow(non_snake_case)]
pub fn P<T>(x: T) -> P<T> {
    P{ x: Box::new(x) }
}

impl<T: ?Sized + Reflect + TypePath> Reflect for P<T> {
    fn get_represented_type_info(&self) -> Option<&'static bevy_reflect::TypeInfo> {
        self.x.get_represented_type_info()
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self.x.into_any()
    }

    fn as_any(&self) -> &dyn Any {
        self.x.as_any()
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        self.x.as_any_mut()
    }

    fn into_reflect(self: Box<Self>) -> Box<dyn Reflect> {
        self.x.into_reflect()
    }

    fn as_reflect(&self) -> &dyn Reflect {
        self.x.as_reflect()
    }

    fn as_reflect_mut(&mut self) -> &mut dyn Reflect {
        self.x.as_reflect_mut()
    }

    fn try_apply(&mut self, value: &dyn Reflect) -> std::result::Result<(), bevy_reflect::ApplyError> {
        self.x.try_apply(value)
    }

    fn set(&mut self, value: Box<dyn Reflect>) -> std::result::Result<(), Box<dyn Reflect>> {
        self.x.set(value)
    }

    fn reflect_ref(&self) -> bevy_reflect::ReflectRef {
        self.x.reflect_ref()
    }

    fn reflect_mut(&mut self) -> bevy_reflect::ReflectMut {
        self.x.reflect_mut()
    }

    fn reflect_owned(self: Box<Self>) -> bevy_reflect::ReflectOwned {
        self.x.reflect_owned()
    }

    fn clone_value(&self) -> Box<dyn Reflect> {
        self.x.clone_value()
    }
}

impl<T: Clone + Reflect + TypePath> FromReflect for P<T> {
    fn from_reflect(reflect: &dyn Reflect) -> Option<Self> {
        Some(reflect.as_any().downcast_ref::<P<T>>()?.clone())
    }
}

impl<T: GetTypeRegistration> GetTypeRegistration for P<T> {
    fn get_type_registration() -> bevy_reflect::TypeRegistration {
        T::get_type_registration()
    }
}


#[derive(TypePath, Clone)]
pub struct PArc<T: ?Sized>{ x: Arc<T> }


#[allow(non_snake_case)]
pub fn PArc<T>(x: T) -> PArc<T> {
    PArc{ x: Arc::new(x) }
}

impl<T: Clone + Reflect + TypePath> Reflect for PArc<T> {
    fn get_represented_type_info(&self) -> Option<&'static bevy_reflect::TypeInfo> {
        (*self.x).get_represented_type_info()
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        let a: T = (*self.x).clone();
        Box::new(a).into_any()
    }

    fn as_any(&self) -> &dyn Any {
        (*self.x).as_any()
    }

    fn as_any_mut(&mut self) -> &mut dyn Any {
        Arc::make_mut(&mut self.x).as_any_mut()
    }

    fn into_reflect(self: Box<Self>) -> Box<dyn Reflect> {
        let a: T = (*self.x).clone();
        Box::new(a).into_reflect()
    }

    fn as_reflect(&self) -> &dyn Reflect {
        (*self.x).as_reflect()
    }

    fn as_reflect_mut(&mut self) -> &mut dyn Reflect {
        Arc::make_mut(&mut self.x).as_reflect_mut()
    }

    fn try_apply(&mut self, value: &dyn Reflect) -> std::result::Result<(), bevy_reflect::ApplyError> {
        Arc::make_mut(&mut self.x).try_apply(value)
    }

    fn set(&mut self, value: Box<dyn Reflect>) -> std::result::Result<(), Box<dyn Reflect>> {
        Arc::make_mut(&mut self.x).set(value)
    }

    fn reflect_ref(&self) -> bevy_reflect::ReflectRef {
        (*self.x).reflect_ref()
    }

    fn reflect_mut(&mut self) -> bevy_reflect::ReflectMut {
        Arc::make_mut(&mut self.x).reflect_mut()
    }

    fn reflect_owned(self: Box<Self>) -> bevy_reflect::ReflectOwned {
        let a: T = (*self.x).clone();
        Box::new(a).reflect_owned()
    }

    fn clone_value(&self) -> Box<dyn Reflect> {
        self.x.clone_value()
    }
}

impl<T: Clone + Reflect + TypePath> FromReflect for PArc<T> {
    fn from_reflect(reflect: &dyn Reflect) -> Option<Self> {
        Some(reflect.as_any().downcast_ref::<PArc<T>>()?.clone())
    }
}

impl<T: GetTypeRegistration> GetTypeRegistration for PArc<T> {
    fn get_type_registration() -> bevy_reflect::TypeRegistration {
        T::get_type_registration()
    }
}



fn visit<B>(x: &dyn Reflect, f: &mut impl FnMut(&dyn Any) -> ControlFlow<B>) -> ControlFlow<B> {
    f(x.as_any())?;

    match x.reflect_ref() {
        ReflectRef::Struct(s) => {
            for field in s.iter_fields() {
                visit(field, f)?;
            }
        }
        ReflectRef::List(lx) => {
            for l in lx.iter() {
                visit(l, f)?;
            }
        }
        _ => {}
    }
    ControlFlow::Continue(())
}


#[cfg(test)]
mod test_ctx {
    use std::sync::Arc;

    use super::*;
    use bevy_reflect::{Reflect, ReflectKind};

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

    #[derive(Reflect)]
    struct Varg<T> {
        a: T, 
        b: i32,
        c: Vec<u32>
    }

    #[test]
    fn test_reflect() {
        let foo: Varg<P<u32>> = Varg { a: P(7u32), b: 2, c: vec![1, 2, 3] };

        let mut v = vec![];
        visit::<()>(&foo, &mut |x| {
            if let Some(y) = x.downcast_ref::<u32>() {
                v.push(*y);
            }
            ControlFlow::Continue(())
        });
        insta::assert_debug_snapshot!(v, @r###"
        [
            7,
            1,
            2,
            3,
        ]
        "###);
    }

    #[test]
    fn test_reflect2() {
        let a = Arc::new(1);
        let b: &dyn Reflect = &a;
        let ReflectRef::Value(c) = b.reflect_ref() else {unreachable!()};
        insta::assert_debug_snapshot!(b, @"Reflect(std::sync::Arc<i32>)");
        insta::assert_debug_snapshot!(c, @"Reflect(std::sync::Arc<i32>)");
    }

    #[test]
    fn test_reflect2_5() {
        let a = PArc(1);
        let b: &dyn Reflect = &a;
        let ReflectRef::Value(c) = b.reflect_ref() else {unreachable!()};
        insta::assert_debug_snapshot!(b, @"Reflect(familia_frontend::context::PArc<i32>)");
        insta::assert_debug_snapshot!(c, @"1");
    }

    #[test]
    fn test_reflect3() {
        let a = P(1);
        let b: &dyn Reflect = &a;
        let ReflectRef::Value(c) = b.reflect_ref() else {unreachable!()};
        insta::assert_debug_snapshot!(b, @"Reflect(familia_frontend::context::P<i32>)");
        insta::assert_debug_snapshot!(c, @"1");
    }

    
}
