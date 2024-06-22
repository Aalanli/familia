use std::any::Any;

use anyhow::Result;

pub use super::registry::NodeID;
use super::registry::{Registry, UniqueRegistry};
use crate::ast::Span;

pub struct IR {
    symbols: UniqueRegistry<Symbol>,
    // paths: UniqueRegistry<Path>,
    types: UniqueRegistry<Type>,
    ir: Registry<dyn Any>,
}

impl IR {
    pub fn new() -> Self {
        IR {
            symbols: UniqueRegistry::new(),
            // paths: UniqueRegistry::new(),
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

    pub fn insert_with<I: ID>(&self, id: I, node: I::Node) {
        self.ir.insert_with_boxed(id.id(), Box::new(node));
    }

    pub fn temporary_id<I: ID>(&self) -> I {
        I::new(self.ir.temporary_id())
    }

    pub fn new_var(&self, name: Option<SymbolID>, ty: Option<TypeID>, span: Option<Span>) -> VarID {
        let name = name.unwrap_or_else(|| self.insert_symbol(""));
        let id = self.temporary_id();
        self.insert(Var { id, name, ty, span })
    }

    pub fn iter_ids<I: ID>(&self) -> impl Iterator<Item = (I, &I::Node)> {
        self.ir.iter().filter_map(|x| {
            let node = self.get::<I>(I::new(x))?;
            Some((I::new(x), node))
        })
    }

    // These are separate because we need hashing utility
    //   perhaps in the future we can unify this, so that we have dyn Hash capability
    pub fn get_symbol(&self, id: SymbolID) -> Option<&Symbol> {
        self.symbols.get(id.0)
    }

    pub fn insert_symbol(&self, symbol: &str) -> SymbolID {
        SymbolID(self.symbols.insert(Symbol {
            name: symbol.to_string(),
        }))
    }

    // pub fn get_path(&self, id: PathID) -> Option<&Path> {
    //     self.paths.get(id.0)
    // }

    // pub fn insert_path(&self, path: Path) -> PathID {
    //     PathID(self.paths.insert(path))
    // }

    // We have temporary ids because we want to be able to have recursively referring types
    pub fn get_type(&self, id: TypeID) -> Option<&Type> {
        self.types.get(id.0)
    }

    pub fn insert_type(&self, ty: TypeKind) -> TypeID {
        TypeID(self.types.insert(Type { kind: ty }))
    }

    pub fn insert_type_with_id(&self, id: TypeID, ty: TypeKind) -> Result<()> {
        self.types.insert_with(id.0, Type { kind: ty })
    }
}

pub trait ID: Copy + Eq + Ord + std::hash::Hash {
    type Node: 'static;
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

impl_id!(SymbolID, Symbol);

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
        ir.get_symbol(ir.get(*self).unwrap().name).unwrap().name.as_str()
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

impl_id!(TypeID, Type);

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
