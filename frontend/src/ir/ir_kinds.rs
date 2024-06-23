use std::any::Any;
use std::hash::Hash;


pub use super::registry::NodeID;
use super::registry::{Registry, GenericUniqueRegistry};
use crate::ast::Span;

pub struct IR {
    registry: Registry<dyn Any>,
    unique_registry: GenericUniqueRegistry,
}

impl IR {
    pub fn new() -> Self {
        IR {
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

    pub fn get<I: ID>(&self, id: I) -> Option<&I::Node> {
        let any = self.get_any(id.id())?;
        any.downcast_ref::<I::Node>()
    }

    pub fn get_mut<I: ID>(&mut self, id: I) -> Option<&mut I::Node> {
        let any = self.get_any_mut(id.id())?;
        any.downcast_mut::<I::Node>()
    }

    pub fn insert<I: ID>(&self, node: I::Node) -> I {
        let id = self.registry.insert_boxed(Box::new(node));
        I::new(id)
    }

    pub fn insert_with<I: ID>(&self, id: I, node: I::Node) {
        self.registry.insert_with_boxed(id.id(), Box::new(node));
    }

    pub fn temporary_id<I: ID>(&self) -> I {
        I::new(self.registry.temporary_id())
    }

    pub fn get_unique<U: UniqueID>(&self, id: U) -> Option<&U::Node> {
        let res = self.unique_registry.get(id.id())?;
        res.as_any().downcast_ref::<U::Node>()
    }

    pub fn insert_unique<U: UniqueID>(&self, node: U::Node) -> U {
        let id = self.unique_registry.insert(node);
        U::new(id)
    }

    pub fn insert_unique_with<U: UniqueID>(&self, id: U, node: U::Node) {
        self.unique_registry.insert_with(id.id(), node).unwrap();
    }

    pub fn temporary_unique_id<U: UniqueID>(&self) -> U {
        U::new(self.unique_registry.temporary_id())
    }

    pub fn iter_ids<I: ID>(&self) -> impl Iterator<Item = (I, &I::Node)> {
        self.registry.iter().filter_map(|x| {
            let node = self.get::<I>(I::new(x))?;
            Some((I::new(x), node))
        })
    }

    pub fn iter_unique_ids<U: UniqueID>(&self) -> impl Iterator<Item = (U, &U::Node)> {
        self.unique_registry.iter().filter_map(|x| {
            let node = self.get_unique::<U>(U::new(x))?;
            Some((U::new(x), node))
        })
    }
}

pub trait ID: Copy + Eq + Ord + Hash {
    type Node: 'static;
    fn new(node: NodeID) -> Self;
    fn id(self) -> NodeID;
}

pub trait UniqueID: Copy + Eq + Ord + Hash {
    type Node: Hash + Eq + 'static;
    fn new(node: NodeID) -> Self;
    fn id(self) -> NodeID;
}

macro_rules! impl_id {
    ($id:ident, $node:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
        pub struct $id(NodeID);

        impl ID for $id {
            type Node = $node;
            fn new(node: NodeID) -> Self {
                $id(node)
            }
            fn id(self) -> NodeID {
                self.0
            }
        }
    };
}

macro_rules! impl_unique_id {
    ($id:ident, $node:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
        pub struct $id(NodeID);

        impl UniqueID for $id {
            type Node = $node;
            fn new(node: NodeID) -> Self {
                $id(node)
            }
            fn id(self) -> NodeID {
                self.0
            }
        }
    };
}

impl_unique_id!(SymbolID, Symbol);

impl SymbolID {
    pub fn get_symbol<'a>(&self, ir: &'a IR) -> Option<&'a Symbol> {
        ir.get_unique(*self)
    }

    pub fn get_str<'a>(&self, ir: &'a IR) -> Option<&'a str> {
        Some(&self.get_symbol(ir)?.name)
    }

    pub fn insert_symbol(ir: &IR, name: &str) -> SymbolID {
        ir.insert_unique(Symbol { name: name.to_string() })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Symbol {
    pub name: String,
}

impl_id!(PathID, Path);

#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct Path {
    pub symbols: Vec<SymbolID>,
    pub span: Span,
}

impl_id!(VarID, Var);

impl VarID {
    pub fn type_of(&self, ir: &IR) -> TypeID {
        ir.get(*self).unwrap().ty.unwrap()
    }

    pub fn name_of<'ir>(&self, ir: &'ir IR) -> &'ir str {
        let var = ir.get(*self).unwrap();
        var.name.get_str(ir).unwrap()
    }

    pub fn new_var(ir: &IR, name: Option<SymbolID>, ty: Option<TypeID>, span: Option<Span>) -> VarID {
        let name = name.unwrap_or_else(|| SymbolID::insert_symbol(ir, ""));
        let id = ir.temporary_id();
        ir.insert(Var { id, name, ty, span })
    }
}

#[derive(Clone, Default)]
pub struct Var {
    pub id: VarID,
    pub name: SymbolID,
    pub ty: Option<TypeID>,
    pub span: Option<Span>,
    // pub decl_ty_path: Option<PathID>,
}

impl_unique_id!(TypeID, Type);

impl TypeID {
    pub fn insert_type(ir: &IR, kind: TypeKind) -> TypeID {
        ir.insert_unique(Type { kind })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Type {
    pub kind: TypeKind,
}

impl_id!(TypeDeclID, TypeDecl);

#[derive(Clone, Default)]
pub struct TypeDecl {
    pub name: SymbolID,
    pub decl: TypeID,
    pub span: Span,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeKind {
    I32,
    Void,
    Struct { fields: Vec<(SymbolID, TypeID)> },
    Decl { decl: TypeDeclID },
}

impl Default for TypeKind {
    fn default() -> Self {
        TypeKind::Void
    }
}

#[derive(Debug, Clone, Default)]
pub struct FuncDecl {
    pub name: SymbolID,
    pub args: Vec<(SymbolID, TypeID)>,
    pub ret_ty: TypeID,
}

impl_id!(FuncID, FuncImpl);

#[derive(Debug, Clone, Default)]
pub struct FuncImpl {
    pub decl: FuncDecl,
    pub vars: Vec<VarID>,
    pub body: Vec<OPID>,
}

impl_id!(OPID, OP);

#[derive(Clone)]
pub struct OP {
    pub id: OPID,
    pub kind: OPKind,
    pub res: Option<VarID>,
    pub span: Span,
}

#[derive(Debug, Clone)]
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
    Constant {
        value: i32,
    },
    Assign {
        lhs: VarID,
        rhs: VarID,
    },
}

#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub name: SymbolID,
    pub repr_ty: TypeID,
}

impl_id!(ClassID, ClassImpl);

#[derive(Debug, Clone)]
pub struct ClassImpl {
    pub decl: ClassDecl,
    pub methods: Vec<FuncID>,
}
