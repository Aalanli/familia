use std::ops::ControlFlow;
use std::ops::Deref;
use std::ops::DerefMut;

use crate::ProgramSrcId;
use crate::impl_id;

use derive_new::new;

pub use super::lexer::Span;


// for convenience in lalrpop
#[allow(non_snake_case)]
pub fn P<T>(x: T) -> Box<T> {
    Box::new(x)
}

impl_id!(Symbol, intern String);

impl_id!(Path, intern Vec<Symbol>);

#[derive(new, Debug, Clone, Hash, PartialEq, Eq)]
pub struct WithSpan<T> {
    x: T,
    pub span: Span
}

impl<T> Deref for WithSpan<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.x
    }
}

impl<T> DerefMut for WithSpan<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.x
    }
}

impl_id!(TypeId, Type);
pub type Type = WithSpan<TypeKind>;
pub type SymbolSpan = WithSpan<Symbol>;
pub type PathSpan = WithSpan<Path>;


#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum TypeKind {
    Void,
    Struct { fields: Vec<TypedVar> },
    Symbol(Path),
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct Var {
    pub name: SymbolSpan,
    pub ty: Option<TypeId>,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct TypedVar {
    pub name: SymbolSpan,
    pub ty: TypeId,
}

impl_id!(ModuleId, Module);
pub struct Module {
    pub name: Option<SymbolSpan>,
    pub body: Vec<Decls>,
    pub src: ProgramSrcId,
    pub span: Span
}

impl_id!(InterfaceDeclId, InterfaceDecl);
pub struct InterfaceDecl {
    pub name: SymbolSpan,
    pub body: Vec<Decls>,
    pub span: Span
}

impl_id!(ClassImplId, ClassImpl);
pub struct ClassImpl {
    pub name: SymbolSpan,
    pub for_it: Option<PathSpan>,
    pub body: Vec<Decls>,
    pub span: Span
}

impl_id!(FnDeclId, FnDecl);
pub struct FnDecl {
    pub name: SymbolSpan,
    pub args: Vec<TypedVar>,
    pub res_ty: TypeId,
    pub span: Span
}

impl_id!(FnImplId, FnImpl);
pub struct FnImpl {
    pub decl: FnDecl,
    pub body: Vec<Stmt>,
    pub span: Span
}

impl_id!(TypeDeclId, TypeDecl);
pub struct TypeDecl {
    pub name: SymbolSpan,
    pub type_: TypeId,
    pub span: Span
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum Decls {
    Interface(InterfaceDeclId),
    Class(ClassImplId),
    FnDecl(FnDeclId),
    FnImpl(FnImplId),
    TyDecl(TypeDeclId),
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum StmtKind {
    LetStmt { var: Var, expr: Expr },
    ExprStmt(Expr),
    AssignStmt { lhs: Expr, rhs: Expr },
    ReturnStmt(Expr),
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug)]
pub enum ExprKind {
    Var(Var),
    VoidLit,
    IntLit(i32),
    StringLit(Symbol),
    MethodCall {
        expr: Box<Expr>,
        sym: SymbolSpan,
        args: Vec<Expr>,
    },
    GetAttr {
        expr: Box<Expr>,
        sym: SymbolSpan,
    },
    Add {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Call {
        path: PathSpan,
        args: Vec<Expr>,
    },
    Struct {
        args: Vec<(SymbolSpan, Expr)>,
    },
}

pub trait Visitor: Sized {
    type Result: VisitorResult;

    fn visit_stmt(&mut self, stmt: &Stmt) -> Self::Result {
        walk_stmt(self, stmt)
    }
}

pub fn walk_stmt<'db, V: Visitor>(visitor: &mut V, stmt: &Stmt) -> V::Result {
    todo!()
}

/// Similar to the `Try` trait, but also implemented for `()`.
pub trait VisitorResult {
    type Residual;
    fn output() -> Self;
    fn from_residual(residual: Self::Residual) -> Self;
    fn from_branch(b: ControlFlow<Self::Residual>) -> Self;
    fn branch(self) -> ControlFlow<Self::Residual>;
}

impl VisitorResult for () {
    type Residual = core::convert::Infallible;

    fn output() -> Self {}
    fn from_residual(_: Self::Residual) -> Self {}
    fn from_branch(_: ControlFlow<Self::Residual>) -> Self {}
    fn branch(self) -> ControlFlow<Self::Residual> {
        ControlFlow::Continue(())
    }
}

impl<T> VisitorResult for ControlFlow<T> {
    type Residual = T;

    fn output() -> Self {
        ControlFlow::Continue(())
    }
    fn from_residual(residual: Self::Residual) -> Self {
        ControlFlow::Break(residual)
    }
    fn from_branch(b: Self) -> Self {
        b
    }
    fn branch(self) -> Self {
        self
    }
}

#[macro_export]
macro_rules! try_visit {
    ($e:expr) => {
        match $crate::visit::VisitorResult::branch($e) {
            core::ops::ControlFlow::Continue(()) => (),
            #[allow(unreachable_code)]
            core::ops::ControlFlow::Break(r) => {
                return $crate::visit::VisitorResult::from_residual(r);
            }
        }
    };
}

#[macro_export]
macro_rules! visit_opt {
    ($visitor: expr, $method: ident, $opt: expr $(, $($extra_args: expr),* )?) => {
        if let Some(x) = $opt {
            $crate::try_visit!($visitor.$method(x $(, $($extra_args,)* )?));
        }
    }
}

#[macro_export]
macro_rules! walk_list {
    ($visitor: expr, $method: ident, $list: expr $(, $($extra_args: expr),* )?) => {
        for elem in $list {
            $crate::try_visit!($visitor.$method(elem $(, $($extra_args,)* )?));
        }
    }
}

#[macro_export]
macro_rules! walk_visitable_list {
    ($visitor: expr, $list: expr $(, $($extra_args: expr),* )?) => {
        for elem in $list {
            $crate::try_visit!(elem.visit_with($visitor $(, $($extra_args,)* )?));
        }
    }
}

#[cfg(test)]
mod test_ast {
}
