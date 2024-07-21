use derive_new::new;
pub use super::lexer::Span;


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
pub struct Ident<'db> {
    sym: Symbol<'db>,
    span: Span
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
    I32,
    String,
    This,
    Self_,
    Struct { fields: Vec<(Ident<'db>, Type<'db>)> },
    Symbol(Path<'db>),
}

#[derive(Eq, PartialEq, Clone, Hash, Debug, salsa::Update)]
pub struct Var<'db> {
    pub name: Ident<'db>,
    pub ty: Option<Type<'db>>,
    pub span: Span
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
    kind: ExprKind<'db>,
    span: Span,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug, salsa::Update)]
pub enum ExprKind<'db> {
    Var(Var<'db>),
    VoidLit,
    IntLit(i32),
    StringLit(Symbol<'db>),
    MethodCall {
        expr: Box<Expr<'db>>,
        sym: Ident<'db>,
        args: Vec<Expr<'db>>,
    },
    GetAttr {
        expr: Box<Expr<'db>>,
        sym: Ident<'db>,
    },
    Add {
        lhs: Box<Expr<'db>>,
        rhs: Box<Expr<'db>>,
    },
    Call {
        path: Path<'db>,
        args: Vec<Expr<'db>>,
    },
    Struct {
        args: Vec<(Ident<'db>, Expr<'db>)>,
    },
}

#[salsa::accumulator]
#[derive(new)]
pub struct Diagnostic {
    pub span: Span,
    pub msg: String,
    #[new(value = "ErrorKind::Error")]
    pub kind: ErrorKind,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    Error,
    Warning
}


#[test]
fn test() {
    
}
