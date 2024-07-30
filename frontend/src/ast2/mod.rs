use std::ops::ControlFlow;

use crate::ProgramSource;
use derive_new::new;

pub use super::lexer::Span;

#[allow(non_snake_case)]
pub fn P<T>(x: T) -> Box<T> {
    Box::new(x)
}

#[salsa::interned]
pub struct Symbol<'db> {
    #[return_ref]
    pub sym: String,
}

#[salsa::interned]
pub struct Path<'db> {
    #[return_ref]
    pub symbols: Vec<Symbol<'db>>,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug, salsa::Update)]
pub struct PathSpan<'db> {
    pub path: Path<'db>,
    pub span: Span,
}

#[derive(Eq, PartialEq, Copy, Clone, Hash, Debug, salsa::Update)]
pub struct SymbolSpan<'db> {
    pub sym: Symbol<'db>,
    pub span: Span,
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
    pub span: Span,
}

impl<'db> Var<'db> {
    pub fn into_tvar(self, db: &dyn crate::Db) -> Option<TypedVar<'db>> {
        if self.ty.is_none() {
            crate::Diagnostic::report_syntax_err(db, "expected type annotation");
        }
        Some(TypedVar {
            name: self.name,
            ty: self.ty?,
            span: self.span,
        })
    }
}

#[derive(Eq, PartialEq, Clone, Hash, Debug, salsa::Update)]
pub struct TypedVar<'db> {
    pub name: SymbolSpan<'db>,
    pub ty: Type<'db>,
    pub span: Span,
}

#[salsa::tracked]
pub struct Module<'db> {
    #[id]
    pub name: Symbol<'db>,
    pub name_span: Span,
    #[return_ref]
    pub body: Vec<Decls<'db>>,

    pub src: ProgramSource,
}

#[salsa::tracked]
pub struct InterfaceDecl<'db> {
    #[id]
    pub name: Symbol<'db>,
    pub name_span: Span,
    #[return_ref]
    pub body: Vec<Decls<'db>>,
}

#[salsa::tracked]
pub struct ClassImpl<'db> {
    #[id]
    pub name: Symbol<'db>,
    pub name_span: Span,
    pub for_it: Option<Path<'db>>,
    #[return_ref]
    pub body: Vec<Decls<'db>>,
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
    TyDecl(TypeDecl<'db>),
}

#[derive(Eq, PartialEq, Clone, Hash, Debug, salsa::Update)]
pub struct Stmt<'db> {
    pub kind: StmtKind<'db>,
    pub span: Span,
}

#[derive(Eq, PartialEq, Clone, Hash, Debug, salsa::Update)]
pub enum StmtKind<'db> {
    LetStmt { var: Var<'db>, expr: Expr<'db> },
    ExprStmt(Expr<'db>),
    AssignStmt { lhs: Expr<'db>, rhs: Expr<'db> },
    ReturnStmt(Expr<'db>),
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

pub trait Visitor<'db>: Sized {
    type Result: VisitorResult;

    fn visit_stmt(&mut self, stmt: &Stmt<'db>) -> Self::Result {
        walk_stmt(self, stmt)
    }
}

pub fn walk_stmt<'db, V: Visitor<'db>>(visitor: &mut V, stmt: &Stmt<'db>) -> V::Result {
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
    #[cfg(feature = "nightly")]
    type Residual = !;

    #[cfg(not(feature = "nightly"))]
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
    #[salsa::interned]
    struct Foo<'db> {
        pub name: String,
        #[no_eq]
        pub idx: i32
    }

    #[test]
    fn test_intern_eq() {
        let db = salsa::default_database();
        let foo1 = Foo::new(&db, "name".into(), 1);
        let foo2 = Foo::new(&db, "name".into(), 2);
        let foo3 = Foo::new(&db, "name".into(), 1);
        assert!(foo1 != foo2);
        assert!(foo1 == foo3);
    }
}