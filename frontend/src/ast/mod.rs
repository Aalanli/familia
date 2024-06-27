mod lexer;

use std::fmt::Display;

pub use lexer::{Ident, LexError, Lexer, Loc, Span, Symbol, Tok};

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
    String,
    Struct { fields: Vec<Var> },
    Symbol(Path),
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
    },
    Module {
        name: Ident,
        file: Option<String>,
        top: bool,
        decls: Vec<Decl>,
    },
}

impl Decl {
    pub fn name(&self) -> &Ident {
        match &self.kind {
            DeclKind::TypeDecl { name, .. } => name,
            DeclKind::FnDecl { name, .. } => name,
            DeclKind::FnImpl { name, .. } => name,
            DeclKind::ClassImpl { name, .. } => name,
            DeclKind::Module { name, .. } => name,
        }
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
    StringLit(String),
    GetAttr { exp: P<Expr>, sym: Ident },
    Add { lhs: P<Expr>, rhs: P<Expr> },
    Call { path: Path, args: Vec<Expr> },
    Struct { args: Vec<(Ident, Expr)> },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Path {
    pub path: Vec<Ident>,
    pub span: Span,
}

impl Path {
    pub fn len(&self) -> usize {
        self.path.len()
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, ident) in self.path.iter().enumerate() {
            write!(f, "{}", ident)?;
            if i != self.path.len() - 1 {
                write!(f, "::")?;
            }
        }
        Ok(())
    }
}

pub trait Visitor<'a>: Sized {
    fn visit_decl(&mut self, decl: &'a Decl) {
        default_visit_decl(decl, self);
    }

    fn visit_var(&mut self, var: &'a Var) {
        default_visit_var(var, self);
    }

    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        default_visit_stmt(stmt, self);
    }

    fn visit_expr(&mut self, expr: &'a Expr) {
        default_visit_expr(expr, self);
    }

    fn visit_path(&mut self, _path: &'a Path) {}

    fn visit_type(&mut self, ty: &'a Type) {
        default_visit_type(ty, self);
    }
}

pub fn default_visit_decl<'a>(decl: &'a Decl, visitor: &mut impl Visitor<'a>) {
    match &decl.kind {
        DeclKind::TypeDecl { decl, .. } => {
            visitor.visit_type(decl);
        }
        DeclKind::FnDecl { ty, args, .. } => {
            for arg in args {
                visitor.visit_var(arg);
            }
            visitor.visit_type(ty);
        }
        DeclKind::FnImpl { args, ty, body, .. } => {
            for arg in args {
                visitor.visit_var(arg);
            }
            visitor.visit_type(ty);
            for stmt in body {
                visitor.visit_stmt(stmt);
            }
        }
        DeclKind::ClassImpl { sub_decls, .. } => {
            for sub_decl in sub_decls {
                visitor.visit_decl(sub_decl);
            }
        }
        DeclKind::Module { decls, .. } => {
            for decl in decls {
                visitor.visit_decl(decl);
            }
        }
    }
}

pub fn default_visit_type<'a>(ty: &'a Type, visitor: &mut impl Visitor<'a>) {
    match &ty.kind {
        TypeKind::Void => {}
        TypeKind::I32 => {}
        TypeKind::String => {}
        TypeKind::Struct { fields } => {
            for field in fields {
                visitor.visit_var(field);
            }
        }
        TypeKind::Symbol(path) => {
            visitor.visit_path(path);
        }
    }
}

pub fn default_visit_var<'a>(var: &'a Var, visitor: &mut impl Visitor<'a>) {
    if let Some(ty) = &var.ty {
        visitor.visit_type(ty);
    }
}

pub fn default_visit_expr<'a>(expr: &'a Expr, visitor: &mut impl Visitor<'a>) {
    match &expr.kind {
        ExprKind::Var(var) => {
            visitor.visit_var(var);
        }
        ExprKind::IntLit(_) => {}
        ExprKind::StringLit(_) => {}
        ExprKind::GetAttr { exp, .. } => {
            visitor.visit_expr(exp);
        }
        ExprKind::Add { lhs, rhs } => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        ExprKind::Call { args, path } => {
            for arg in args {
                visitor.visit_expr(arg);
            }
            visitor.visit_path(path);
        }
        ExprKind::Struct { args } => {
            for (_, arg) in args {
                visitor.visit_expr(arg);
            }
        }
    }
}

pub fn default_visit_stmt<'a>(stmt: &'a Stmt, visitor: &mut impl Visitor<'a>) {
    match &stmt.kind {
        StmtKind::LetStmt { var, expr } => {
            visitor.visit_var(var);
            visitor.visit_expr(expr);
        }
        StmtKind::ExprStmt { expr } => {
            visitor.visit_expr(expr);
        }
        StmtKind::ReturnStmt { expr } => {
            visitor.visit_expr(expr);
        }
    }
}
