use std::any::Any;

use anyhow::Result;

use super::registry::{Registry, UniqueRegistry};
pub use super::registry::NodeID;
use crate::ast::Span;


pub struct IR {
    symbols: UniqueRegistry<Symbol>,
    paths: UniqueRegistry<Path>,
    types: UniqueRegistry<Type>,
    ir: Registry<dyn Any>
}

impl IR {
    pub fn new() -> Self {
        IR {
            symbols: UniqueRegistry::new(),
            paths: UniqueRegistry::new(),
            types: UniqueRegistry::new(),
            ir: Registry::new(),
        }
    }

    fn get_any(&self, id: NodeID) -> Option<&dyn Any> {
        self.ir.get(id)
    }

    fn get_any_mut(&mut self, id: NodeID) -> Option<&mut dyn Any> {
        self.ir.get_mut(id)
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
        let id = self.ir.insert_boxed(Box::new(node));
        I::new(id)
    }

    pub fn insert_with<I: ID>(&self, id: NodeID, node: I::Node) -> I {
        self.ir.insert_with_boxed(id, Box::new(node));
        I::new(id)
    }

    pub fn temporary_id(&self) -> NodeID {
        self.ir.temporary_id()
    }

    // These are separate because we need hashing utility
    //   perhaps in the future we can unify this, so that we have dyn Hash capability
    pub fn get_symbol(&self, id: SymbolID) -> Option<&Symbol> {
        self.symbols.get(id.0)
    }

    pub fn insert_symbol(&self, symbol: Symbol) -> SymbolID {
        SymbolID(self.symbols.insert(symbol))
    }

    pub fn get_path(&self, id: PathID) -> Option<&Path> {
        self.paths.get(id.0)
    }

    pub fn insert_path(&self, path: Path) -> PathID {
        PathID(self.paths.insert(path))
    }

    // We have temporary ids because we want to be able to have recursively referring types
    pub fn get_type(&self, id: TypeID) -> Option<&Type> {
        self.types.get(id.0)
    }

    pub fn insert_type(&self, ty: Type) -> TypeID {
        TypeID(self.types.insert(ty))
    }

    pub fn temporary_type_id(&self) -> TypeID {
        TypeID::new(self.temporary_id())
    }

    pub fn insert_type_with_id(&self, id: TypeID, ty: Type) -> Result<()> {
        self.types.insert_with(id.0, ty)
    }    
}


pub trait ID: Copy + Eq + std::hash::Hash {
    type Node: 'static;
    fn new(node: NodeID) -> Self;
    fn id(self) -> NodeID;
}

macro_rules! impl_id {
    ($id:ident, $node:ident) => {
        #[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
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

impl_id!(SymbolID, Symbol);

#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct Symbol {
    pub id: SymbolID,
    pub name: String,
}

impl_id!(PathID, Path);

#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct Path {
    pub symbols: Vec<SymbolID>,
    pub span: Span,
}

impl_id!(VarID, Var);

#[derive(Clone, Default)]
pub struct Var {
    pub id: VarID,
    pub name: SymbolID,
    pub ty: Option<TypeID>,
    pub decl_ty_path: Option<PathID>,
    pub span: Span,
}

impl_id!(TypeID, Type);

#[derive(Clone, PartialEq, Eq, Hash, Default)]
pub struct Type {
    pub id: TypeID,
    pub kind: TypeKind,
}

impl_id!(TypeDeclID, TypeDecl);

#[derive(Clone, Default)]
pub struct TypeDecl {
    pub id: TypeDeclID,
    pub name: SymbolID,
    pub decl: TypeID,
    pub span: Span,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum TypeKind {
    I32,
    Void,
    Struct { fields: Vec<(SymbolID, TypeID)> },
}

impl Default for TypeKind {
    fn default() -> Self {
        TypeKind::Void
    }
}

#[derive(Clone, Default)]
pub struct FuncDecl {
    pub name: SymbolID,
    pub args: Vec<(SymbolID, TypeID)>,
    pub ret_ty: TypeID,
}

impl_id!(FuncID, FuncImpl);

#[derive(Clone, Default)]
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
    pub var: VarID,
    pub span: Span,
}

#[derive(Clone)]
pub enum OPKind {
    GetAttr {
        obj: VarID,
        attr: SymbolID,
        idx: Option<usize>,
    },
    Call {
        path: PathID,
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
}

pub struct ClassDecl {
    pub name: SymbolID,
    pub repr_ty: TypeID,
}

impl_id!(ClassID, ClassImpl);

pub struct ClassImpl {
    pub decl: ClassDecl,
    pub methods: Vec<FuncID>,
}
