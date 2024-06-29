use std::any::{self, Any};
use std::hash::Hash;
use std::collections::HashMap;

pub use super::registry::NodeID;
use super::registry::{GenericUniqueRegistry, Registry};
use crate::ast::Span;

pub struct IR {
    globals: HashMap<any::TypeId, Box<dyn Any>>,
    registry: Registry<dyn Any>,
    unique_registry: GenericUniqueRegistry,
}

impl IR {
    pub fn new() -> Self {
        IR {
            globals: HashMap::new(),
            registry: Registry::new(),
            unique_registry: GenericUniqueRegistry::new(),
        }
    }

    fn get_any(&self, id: NodeID) -> Option<&dyn Any> {
        self.registry.get(id)
    }

    fn get_any_mut(&mut self, id: NodeID) -> Option<&mut dyn Any> {
        self.registry.get_mut(id)
    }

    pub fn insert_global(&mut self, value: impl Any) {
        self.globals.insert(value.type_id(), Box::new(value));
    }

    pub fn get_global<T: 'static>(&self) -> Option<&T> {
        self.globals.get(&std::any::TypeId::of::<T>()).and_then(|any| any.downcast_ref::<T>())
    }

    pub fn get_global_mut<T: 'static>(&mut self) -> Option<&mut T> {
        self.globals.get_mut(&std::any::TypeId::of::<T>()).and_then(|any| any.downcast_mut::<T>())
    }

    pub fn get<I: ID>(&self, id: I) -> &I::Node {
        if I::IS_UNIQUE {
            let res = self.unique_registry.get(id.id()).unwrap();
            res.as_any().downcast_ref::<I::Node>().unwrap()
        } else {
            let any = self.get_any(id.id()).unwrap();
            any.downcast_ref::<I::Node>().unwrap()
        }
    }

    pub fn get_mut<I: ID>(&mut self, id: I) -> &mut I::Node {
        // a hacky way to enforce during compile time that I is not unique
        let _ = I::CHECK;
        let any = self.get_any_mut(id.id()).unwrap();
        any.downcast_mut::<I::Node>().unwrap()
        
    }

    pub fn insert<I: ID>(&self, node: I::Node) -> I {
        if I::IS_UNIQUE {
            let id = self.unique_registry.insert(node);
            I::new(id)
        } else {
            let id = self.registry.insert_boxed(Box::new(node));
            I::new(id)
        }
    }

    pub fn insert_with<I: ID>(&self, id: I, node: I::Node) {
        if I::IS_UNIQUE {
            self.unique_registry.insert_with(id.id(), node).unwrap();
        } else {
            self.registry.insert_with_boxed(id.id(), Box::new(node));
        }
    }

    pub fn temporary_id<I: ID>(&self) -> I {
        if I::IS_UNIQUE {
            I::new(self.unique_registry.temporary_id())
        } else {
            I::new(self.registry.temporary_id())
        }
    }

    pub fn iter<I: ID>(&self) -> impl Iterator<Item = I> {
        let mut ids = vec![];
        if I::IS_UNIQUE {
            self.unique_registry.for_each(|id| {
                let node = self.unique_registry.get(id).unwrap().as_any().type_id() == std::any::TypeId::of::<I::Node>();
                if node {
                    ids.push(I::new(id));
                }
            });
        } else {
            self.registry.for_each(|id| {
                let node = self.registry.get(id).unwrap().type_id() == std::any::TypeId::of::<I::Node>();
                if node {
                    ids.push(I::new(id));
                }
            });
        }
        ids.into_iter()
    }

    pub fn iter_stable<I: ID>(&self) -> impl Iterator<Item = I> {
        let mut ids = self.iter::<I>().collect::<Vec<_>>();
        ids.sort_by_key(|x| x.id());
        ids.into_iter()
    }
}


pub trait ID: Copy + Eq + Ord + Hash {
    type Node: Hash + Eq + 'static;
    const IS_UNIQUE: bool;
    fn id(&self) -> NodeID;
    fn new(node: NodeID) -> Self;
}

trait CheckNotUnique: ID {
    const CHECK: ();
}

impl<T: ID> CheckNotUnique for T {
    const CHECK: () = [()][T::IS_UNIQUE as usize];
}

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


impl_id!(SymbolID, Symbol, true);

impl SymbolID {
    pub fn get_symbol<'a>(&self, ir: &'a IR) -> &'a Symbol {
        ir.get(*self)
    }

    pub fn get_str<'a>(&self, ir: &'a IR) -> &'a str {
        &self.get_symbol(ir).name
    }

    pub fn insert(ir: &IR, name: &str) -> SymbolID {
        ir.insert(Symbol {
            name: name.to_string(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub name: String,
}

impl_id!(PathID, Path, false);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub symbols: Vec<SymbolID>,
    pub span: Span,
}

impl_id!(VarID, Var, false);

impl VarID {
    pub fn type_of(&self, ir: &IR) -> TypeID {
        ir.get(*self).ty.unwrap()
    }

    pub fn name_of<'ir>(&self, ir: &'ir IR) -> &'ir str {
        let var = ir.get(*self);
        var.name.get_str(ir)
    }

    pub fn new_var(
        ir: &IR,
        name: Option<SymbolID>,
        ty: Option<TypeID>,
        span: Option<Span>,
    ) -> VarID {
        let name = name.unwrap_or_else(|| SymbolID::insert(ir, ""));
        let id = ir.temporary_id();
        ir.insert(Var { id, name, ty, span })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub id: VarID,
    pub name: SymbolID,
    pub ty: Option<TypeID>,
    pub span: Option<Span>,
    // pub decl_ty_path: Option<PathID>,
}

impl_id!(TypeID, Type, true);

impl TypeID {
    pub fn insert(ir: &IR, kind: TypeKind) -> TypeID {
        ir.insert(Type { kind })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub kind: TypeKind,
}

impl_id!(TypeDeclID, TypeDecl, false);

impl TypeDeclID {
    pub fn type_id(&self, ir: &IR) -> TypeID {
        let decl = ir.get(*self);
        decl.decl
    }

    pub fn type_of<'a>(&self, ir: &'a IR) -> &'a Type {
        let ty_id = ir.get(*self).decl;
        ir.get(ty_id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDecl {
    pub name: SymbolID,
    pub decl: TypeID,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeKind {
    I32,
    Void,
    String,
    Struct { fields: Vec<(SymbolID, TypeID)> },
    Rec { id: TypeID },
}

impl Default for TypeKind {
    fn default() -> Self {
        TypeKind::Void
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncDecl {
    pub name: SymbolID,
    pub args: Vec<(SymbolID, TypeID)>,
    pub ret_ty: TypeID,
}

impl_id!(FuncID, FuncImpl, false);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncImpl {
    pub decl: FuncDecl,
    pub vars: Vec<VarID>,
    pub body: Vec<OPID>,
    pub builtin: bool
}

impl_id!(OPID, OP, false);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OP {
    pub id: OPID,
    pub kind: OPKind,
    pub res: Option<VarID>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OPKind {
    GetAttr {
        obj: VarID,
        attr: SymbolID,
        idx: Option<usize>,
    },
    Call {
        // path: PathID,
        func: FuncID,
        args: Vec<VarID>,
    },
    Add {
        lhs: VarID,
        rhs: VarID,
    },
    Struct {
        fields: Vec<(SymbolID, VarID)>,
    },
    Return {
        value: VarID,
    },
    Constant(ConstKind),
    Assign {
        lhs: VarID,
        rhs: VarID,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstKind {
    I32(i32),
    String(SymbolID),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassDecl {
    pub name: SymbolID,
    pub repr_ty: TypeID,
}

impl_id!(ClassID, ClassImpl, false);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ClassImpl {
    pub decl: ClassDecl,
    pub methods: Vec<FuncID>,
}
