use super::registry::{Registry, UniqueRegistry, NodeID};
use crate::ast::Span;

pub struct IR {
    symbols: UniqueRegistry<Symbol>,
    paths: UniqueRegistry<Path>,
    types: UniqueRegistry<Type>,

    vars: Registry<Var>,
    type_decls: Registry<TypeDecl>,
    func_decls: Registry<FuncImpl>,
    class_decls: Registry<ClassImpl>,
    ops: Registry<OP>,
    // class_decls: Registry<ClassDecl>,
}

impl IR {
    pub fn new() -> Self {
        IR {
            symbols: UniqueRegistry::new(),
            paths: UniqueRegistry::new(),
            types: UniqueRegistry::new(),
            vars: Registry::new(),
            type_decls: Registry::new(),
            func_decls: Registry::new(),
            class_decls: Registry::new(),
            ops: Registry::new(),
        }
    }

    pub fn get<I: ID>(&self, id: I) -> &I::Node {
        id.get(self)
    }

    pub fn get_mut<I: IDMut>(&mut self, id: I) -> &mut I::Node {
        id.get_mut(self)
    }

    pub fn insert<I: ID>(&self, node: I::Node) -> I {
        I::new(self, node)
    }

    pub fn symbol_name(&self, id: SymbolID) -> &str {
        &self.get(id).name
    }

    pub fn functions(&self) -> impl Iterator<Item = FuncID> {
        self.func_decls.iter().map(|id| FuncID(id))
    }

    pub fn type_decls(&self) -> impl Iterator<Item = TypeID> {
        self.type_decls.iter().map(|id| TypeID(id))
    }
}


pub trait ID: Copy + Eq + std::hash::Hash {
    type Node;
    fn new(ir: &IR, node: Self::Node) -> Self;
    fn get(self, ir: &IR) -> &Self::Node;
}

pub trait IDMut: ID {
    fn get_mut(self, ir: &mut IR) -> &mut Self::Node;
}

/// A unique interned string based only on the contents
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolID(NodeID);

impl ID for SymbolID {
    type Node = Symbol;
    fn new(ir: &IR, node: Self::Node) -> Self {
        SymbolID(ir.symbols.insert(node))
    }
    fn get(self, ir: &IR) -> &Self::Node {
        ir.symbols.get(self.0).unwrap()
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub id: SymbolID,
    pub name: String,
}

/// A unique interned path based on the string and span
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PathID(NodeID);

impl ID for PathID {
    type Node = Path;
    fn new(ir: &IR, node: Self::Node) -> Self {
        PathID(ir.paths.insert(node))
    }
    fn get(self, ir: &IR) -> &Self::Node {
        ir.paths.get(self.0).unwrap()
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub symbols: Vec<SymbolID>,
    pub span: Span,
}


#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarID(NodeID); // unique variable

impl ID for VarID {
    type Node = Var;
    fn new(ir: &IR, node: Self::Node) -> Self {
        VarID(ir.vars.insert(node))
    }
    fn get(self, ir: &IR) -> &Self::Node {
        ir.vars.get(self.0).unwrap()
    }
}

impl IDMut for VarID {
    fn get_mut(self, ir: &mut IR) -> &mut Self::Node {
        ir.vars.get_mut(self.0).unwrap()
    }
}

#[derive(Clone)]
pub struct Var {
    pub id: VarID,
    pub name: SymbolID,
    pub ty: Option<TypeID>,
    pub decl_ty_path: Option<PathID>,
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeID(NodeID);

impl ID for TypeID {
    type Node = Type;
    fn new(ir: &IR, node: Self::Node) -> Self {
        TypeID(ir.types.insert(node))
    }
    fn get(self, ir: &IR) -> &Self::Node {
        ir.types.get(self.0).unwrap()
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Type {
    pub id: TypeID,
    pub kind: TypeKind,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeDeclID(NodeID);

impl ID for TypeDeclID {
    type Node = TypeDecl;
    fn new(ir: &IR, node: Self::Node) -> Self {
        TypeDeclID(ir.type_decls.insert(node))
    }
    fn get(self, ir: &IR) -> &Self::Node {
        ir.type_decls.get(self.0).unwrap()
    }
}

impl IDMut for TypeDeclID {
    fn get_mut(self, ir: &mut IR) -> &mut Self::Node {
        ir.type_decls.get_mut(self.0).unwrap()
    }
}

#[derive(Clone)]
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

#[derive(Clone)]
pub struct FuncDecl {
    pub name: SymbolID,
    pub args: Vec<(SymbolID, TypeID)>,
    pub ret_ty: TypeID,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncID(NodeID);

impl ID for FuncID {
    type Node = FuncImpl;
    fn new(ir: &IR, node: Self::Node) -> Self {
        FuncID(ir.func_decls.insert(node))
    }
    fn get(self, ir: &IR) -> &Self::Node {
        ir.func_decls.get(self.0).unwrap()
    }
}

impl IDMut for FuncID {
    fn get_mut(self, ir: &mut IR) -> &mut Self::Node {
        ir.func_decls.get_mut(self.0).unwrap()
    }
}

pub struct FuncImpl {
    pub decl: FuncDecl,
    pub vars: Vec<VarID>,
    pub body: Vec<OPID>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct OPID(NodeID);

impl ID for OPID {
    type Node = OP;
    fn new(ir: &IR, node: Self::Node) -> Self {
        OPID(ir.ops.insert(node))
    }
    fn get(self, ir: &IR) -> &Self::Node {
        ir.ops.get(self.0).unwrap()
    }
}

impl IDMut for OPID {
    fn get_mut(self, ir: &mut IR) -> &mut Self::Node {
        ir.ops.get_mut(self.0).unwrap()
    }
}

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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClassImplID(NodeID);

impl ID for ClassImplID {
    type Node = ClassImpl;
    fn new(ir: &IR, node: Self::Node) -> Self {
        ClassImplID(ir.class_decls.insert(node))
    }
    fn get(self, ir: &IR) -> &Self::Node {
        ir.class_decls.get(self.0).unwrap()
    }
}

impl IDMut for ClassImplID {
    fn get_mut(self, ir: &mut IR) -> &mut Self::Node {
        ir.class_decls.get_mut(self.0).unwrap()
    }
}

pub struct ClassImpl {
    pub decl: ClassDecl,
    pub methods: Vec<FuncID>,
}
