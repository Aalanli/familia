use std::rc::Rc;

use std::collections::HashMap;

pub mod ptr;
pub use ptr::P;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Symbol {
    name: Rc<str>,
}

impl Symbol {
    pub fn from_str(s: &str) -> Self {
        Symbol {
            name: Rc::from(s)
        }
    }

    pub fn view(&self) -> &str {
        &*self.name
    }
}

#[derive(Clone)]
pub struct Var {
    pub name: Symbol,
    pub ty: Option<Type>
}

#[derive(Clone)]
pub enum Type {
    Void,
    I32,
    Struct {
        fields: HashMap<Symbol, Type>,
    },
    Symbol(Symbol)
}

impl Type {
    pub fn from_var(vars: Vec<Var>) -> Type {
        let mut map = HashMap::new();
        for v in vars {
            map.insert(v.name, v.ty.unwrap());
        }
        Type::Struct {
            fields: map
        }
    }
}

pub struct AST {
    pub decls: Vec<Decl>
}

pub enum Decl {
    TypeDecl {
        name: Symbol,
        decl: Type,
    },
    FnDecl {
        name: Symbol,
        args: Vec<Var>,
        ty: Type
    },
    FnImpl {
        name: Symbol,
        args: Vec<Var>,
        ty: Type,
        body: Vec<Stmt>
    }
}


pub enum Stmt {
    LetStmt {
        var: Var,
        expr: Expr,
    },
    ExprStmt {
        expr: Expr,
    },
    AssignStmt {
        lhs: Var,
        rhs: Expr,
    },
    ReturnStmt {
        expr: Expr
    }
}

#[derive(Clone)]
pub enum Expr {
    Var(Var),
    IntLit(i32),
    GetAttr {
        exp: P<Expr>,
        sym: Symbol,
    },
    Add {
        lhs: P<Expr>,
        rhs: P<Expr>,
    },
    Call {
        symbol: Symbol,
        args: Vec<Expr>,
    },
    Struct {
        args: Vec<(Symbol, Expr)>
    },
}