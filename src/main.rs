
pub mod irw {
    use std::hash::{Hash, Hasher, DefaultHasher};
    use std::cell::OnceCell;
    use std::ops::Deref;
    use std::rc::{Rc, Weak};

    #[derive(Clone)]
    pub struct IRW<T: ?Sized> {
        data: Rc<T>,
        // hash: OnceCell<u64>,
    }

    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct IRHash(u64);

    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct IRUid(usize);

    impl<T: Clone> IRW<T> {
        pub fn new(data: T) -> Self {
            IRW {
                data: Rc::new(data),
                // hash: OnceCell::new(),
            }
        }

        pub fn get_mut(&mut self) -> &mut T {
            // self.hash = OnceCell::new();
            Rc::make_mut(&mut self.data)
        }

        // pub fn hash(&self) -> IRHash {
        //     let hash = self.hash.get_or_init(|| {
        //         let mut hasher = DefaultHasher::new();
        //         self.data.hash(&mut hasher);
        //         hasher.finish()
        //     });
        //     IRHash(*hash)
        // }

        pub fn uid(&self) -> IRUid {
            let ptr = Rc::as_ptr(&self.data) as usize;
            IRUid(ptr)
        }
    }


    impl<T> Deref for IRW<T> {
        type Target = T;

        fn deref(&self) -> &Self::Target {
            &self.data
        }
    }

    // impl<T: Hash + Clone> Hash for IRW<T> {
    //     fn hash<H: Hasher>(&self, state: &mut H) {
    //         let h = self.hash();
    //         h.0.hash(state);
    //     }
    // }
}

pub mod ast {
    use std::rc::Rc;

    use std::collections::HashMap;
    use lalrpop_util::state_machine::ParseError;

    pub use crate::irw::IRW;

    #[derive(Clone, PartialEq, Eq, Hash)]
    pub struct Symbol {
        name: Rc<str>,
    }

    impl Symbol {
        pub fn from_str(s: &str) -> Self {
            Symbol {
                name: Rc::from(s)
            }
        }
    }

    #[derive(Clone)]
    pub struct Var {
        pub name: Symbol,
        pub ty: Option<IRW<Type>>
    }

    #[derive(Clone)]
    pub enum Type {
        Void,
        I32,
        Struct {
            fields: HashMap<Symbol, IRW<Type>>,
        }
    }

    impl Type {
        pub fn from_var(vars: Vec<Var>) -> IRW<Type> {
            let mut map = HashMap::new();
            for v in vars {
                map.insert(v.name, v.ty.unwrap());
            }
            IRW::new(Type::Struct {
                fields: map
            })
        }
    }

    // fn test() {
    //     let a = Some(1);
    //     a.map
    // }

    pub enum AST {
        TypeDecl {
            name: Symbol,
            decl: IRW<Type>,
        },
        FnDecl {
            name: Symbol,
            args: Vec<Var>,
            ty: IRW<Type>
        },
        FnImpl {
            name: Symbol,
            args: Vec<Var>,
            ty: IRW<Type>,
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
            exp: IRW<Expr>,
            sym: Symbol,
        },
        Add {
            lhs: IRW<Expr>,
            rhs: IRW<Expr>,
        },
        Call {
            symbol: Symbol,
            args: Vec<Expr>,
        }
    }
}

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub familia);

fn main() {
    let k = familia::ASTParser::new().parse("fn test(a:i32):i32").unwrap();

}
