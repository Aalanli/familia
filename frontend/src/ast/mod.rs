mod lexer;
pub use lexer::{Symbol, Lexer, LexError, Ident, Loc, Span, Tok};

pub mod ptr;
pub use ptr::P;


#[derive(Clone, Debug)]
pub struct Var {
    pub name: Ident,
    pub ty: Option<Type>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum TypeKind {
    Void,
    I32,
    Struct { fields: Vec<Var> },
    Symbol(Path),
}

#[derive(Clone, Debug)]
pub struct AST {
    pub decls: Vec<Decl>,
}

#[derive(Clone, Debug)]
pub struct Decl {
    pub kind: DeclKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum DeclKind {
    TypeDecl {
        name: Ident,
        decl: Type,
    },
    FnDecl {
        name: Ident,
        args: Vec<Var>,
        ty: Type,
    },
    FnImpl {
        name: Ident,
        args: Vec<Var>,
        ty: Type,
        body: Vec<Stmt>,
    },
    ClassImpl {
        name: Ident,
        sub_decls: Vec<Decl>,
    }
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    LetStmt { var: Var, expr: Expr },
    ExprStmt { expr: Expr },
    // AssignStmt { lhs: Var, rhs: Expr },
    ReturnStmt { expr: Expr },
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Var(Var),
    IntLit(i32),
    GetAttr { exp: P<Expr>, sym: Ident },
    Add { lhs: P<Expr>, rhs: P<Expr> },
    Call { path: Path, args: Vec<Expr> },
    Struct { args: Vec<(Ident, Expr)> },
}

#[derive(Clone, Debug)]
pub struct Path {
    pub path: Vec<Ident>,
    pub span: Span,
}
