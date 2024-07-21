use derive_new::new;
use crate::ProgramSource;

pub use super::lexer::Span;

#[allow(non_snake_case)]
pub fn P<T>(x: T) -> Box<T> {
    Box::new(x)
}

#[salsa::interned]
pub struct Symbol<'db> {
    #[return_ref]
    pub sym: String
}

#[salsa::interned]
pub struct Path<'db> {
    #[return_ref]
    pub symbols: Vec<Symbol<'db>>
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug, salsa::Update)]
pub struct PathSpan<'db> {
    pub path: Path<'db>,
    pub span: Span,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug, salsa::Update)]
pub struct SymbolSpan<'db> {
    pub sym: Symbol<'db>,
    pub span: Span
}


#[salsa::interned]
pub struct Type<'db> {
    #[return_ref]
    pub kind: TypeKind<'db>,
    pub span: Span,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug, salsa::Update)]
pub enum TypeKind<'db> {
    Void,
    Struct { fields: Vec<TypedVar<'db>> },
    Symbol(Path<'db>),
}

#[derive(Eq, PartialEq, Clone, Hash, Debug, salsa::Update)]
pub struct Var<'db> {
    pub name: SymbolSpan<'db>,
    pub ty: Option<Type<'db>>,
    pub span: Span
}

impl<'db> Var<'db> {
    pub fn into_tvar(self, db: &dyn crate::Db) -> Option<TypedVar<'db>> {
        if self.ty.is_none() {
            crate::Diagnostic::report_syntax_err(db, "expected type annotation");
        }
        Some(TypedVar { name: self.name, ty: self.ty?, span: self.span })
    }
}

#[derive(Eq, PartialEq, Clone, Hash, Debug, salsa::Update)]
pub struct TypedVar<'db> {
    pub name: SymbolSpan<'db>,
    pub ty: Type<'db>,
    pub span: Span
}


#[salsa::tracked]
pub struct Module<'db> {
    #[id]
    pub name: Symbol<'db>,
    pub name_span: Span,
    #[return_ref]
    pub body: Vec<Decls<'db>>,

    pub src: ProgramSource
}

#[salsa::tracked]
pub struct InterfaceDecl<'db> {
    #[id]
    pub name: Symbol<'db>,
    pub name_span: Span,
    #[return_ref]
    pub body: Vec<Decls<'db>>
}

#[salsa::tracked]
pub struct ClassImpl<'db> {
    #[id]
    pub name: Symbol<'db>,
    pub name_span: Span,
    pub for_it: Option<Path<'db>>,
    #[return_ref]
    pub body: Vec<Decls<'db>>
}

#[salsa::tracked]
pub struct FnDecl<'db> {
    #[id]
    pub name: Symbol<'db>,
    pub name_span: Span,
    #[return_ref]
    pub args: Vec<Var<'db>>,
    pub res_ty: Type<'db>,
}

#[salsa::tracked]
pub struct FnImpl<'db> {
    #[id]
    pub name: Symbol<'db>,
    pub name_span: Span,
    #[return_ref]
    pub args: Vec<Var<'db>>,
    pub res_ty: Type<'db>,
    #[return_ref]
    pub body: Vec<Stmt<'db>>,
}

#[salsa::tracked]
pub struct TypeDecl<'db> {
    #[id]
    pub name: Symbol<'db>,
    pub name_span: Span,
    pub type_: Type<'db>,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug, salsa::Update)]
pub enum Decls<'db> {
    Interface(InterfaceDecl<'db>),
    Class(ClassImpl<'db>),
    FnDecl(FnDecl<'db>),
    FnImpl(FnImpl<'db>),
    TyDecl(TypeDecl<'db>)
}

#[derive(Eq, PartialEq, Clone, Hash, Debug, salsa::Update)]
pub struct Stmt<'db> {
    pub kind: StmtKind<'db>,
    pub span: Span
}

#[derive(Eq, PartialEq, Clone, Hash, Debug, salsa::Update)]
pub enum StmtKind<'db> {
    LetStmt { var: Var<'db>, expr: Expr<'db> },
    ExprStmt(Expr<'db>),
    AssignStmt { lhs: Expr<'db>, rhs: Expr<'db> },
    ReturnStmt(Expr<'db>)
}

#[derive(Eq, PartialEq, Clone, Hash, Debug, salsa::Update)]
pub struct Expr<'db> {
    pub kind: ExprKind<'db>,
    pub span: Span,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug, salsa::Update)]
pub enum ExprKind<'db> {
    Var(Var<'db>),
    VoidLit,
    IntLit(i32),
    StringLit(Symbol<'db>),
    MethodCall {
        expr: Box<Expr<'db>>,
        sym: SymbolSpan<'db>,
        args: Vec<Expr<'db>>,
    },
    GetAttr {
        expr: Box<Expr<'db>>,
        sym: SymbolSpan<'db>,
    },
    Add {
        lhs: Box<Expr<'db>>,
        rhs: Box<Expr<'db>>,
    },
    Call {
        path: PathSpan<'db>,
        args: Vec<Expr<'db>>,
    },
    Struct {
        args: Vec<(SymbolSpan<'db>, Expr<'db>)>,
    },
}

