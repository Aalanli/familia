use std::collections::HashMap;
use std::hash::Hash;

use either::{Either, Left, Right};

use crate::ast;
use crate::ast::{Span, Visitor};
use crate::ir;

#[derive(Debug, Clone)]
struct PointerHashKey<'a, K> {
    key: &'a K,
}

#[derive(Debug, Clone)]
struct PointerHashMap<'a, K, V> {
    map: HashMap<PointerHashKey<'a, K>, V>,
}

impl<'a, K> Hash for PointerHashKey<'a, K> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.key as *const K).hash(state);
    }
}

impl<'a, K> PartialEq for PointerHashKey<'a, K> {
    fn eq(&self, other: &Self) -> bool {
        self.key as *const K == other.key as *const K
    }
}

impl<'a, K> Eq for PointerHashKey<'a, K> {}

impl<'a, K, V> PointerHashMap<'a, K, V> {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn insert(&mut self, key: &'a K, value: V) -> Option<V> {
        self.map.insert(PointerHashKey { key }, value)
    }

    fn get(&self, key: &'a K) -> Option<&V> {
        self.map.get(&PointerHashKey { key })
    }
}


#[derive(Debug)]
pub enum AstError<'a> {
    InvalidClassNest(&'a ast::Ident, Span),
    PathNotFound {
        path: &'a ast::Path,
        index: usize,
        decl: Either<&'a ast::Decl, &'a ast::AST>,
    }
}

// disallow nested classes for now
fn check_basic(ast: &ast::AST) -> Result<(), AstError> {
    for decl in ast.decls.iter() {
        if let ast::DeclKind::ClassImpl { sub_decls, .. } = &decl.kind {
            for sub_decl in sub_decls.iter() {
                if let ast::DeclKind::ClassImpl { name, .. } = &sub_decl.kind {
                    return Err(AstError::InvalidClassNest(name, sub_decl.span));
                }
            }
        }
    }
    Ok(())
}

type PathMap<'a> = PointerHashMap<'a, ast::Path, &'a ast::Decl>;

struct GetPath<'a> {
    paths: Vec<&'a ast::Path>,
}

impl<'a> ast::Visitor<'a> for GetPath<'a> {
    fn visit_path(&mut self, path: &'a ast::Path) {
        self.paths.push(path);
    }
}

fn find_path<'a>(ast: &'a ast::Decl, path: &'a ast::Path, pindex: usize) -> Result<&'a ast::Decl, AstError<'a>> {
    match &ast.kind {
        ast::DeclKind::ClassImpl { sub_decls, name } => {
            if name.name == path.path[pindex].name {
                if pindex == path.path.len() - 1 {
                    return Ok(ast);
                }
                for sub_decl in sub_decls.iter() {
                    let p = find_path(sub_decl, path, pindex + 1);
                    if p.is_ok() {
                        return p;
                    }
                }
            }
        },
        ast::DeclKind::FnImpl { name, .. } => {
            if name.name == path.path[pindex].name {
                if pindex == path.path.len() - 1 {
                    return Ok(ast);
                }
            }
        }
        ast::DeclKind::TypeDecl { name, .. } => {
            if name.name == path.path[pindex].name {
                if pindex == path.path.len() - 1 {
                    return Ok(ast);
                }
            }
        }
        ast::DeclKind::FnDecl { name, .. } => {
            if name.name == path.path[pindex].name {
                if pindex == path.path.len() - 1 {
                    return Ok(ast);
                }
            }
        }
    }
    Err(AstError::PathNotFound {
        path,
        index: pindex,
        decl: Left(ast),
    })
}

// every path must be fully qualified for now
fn check_path(ast: &ast::AST) -> Result<PathMap, AstError> {
    let mut path_map = PointerHashMap::new();
    let mut visitor = GetPath { paths: Vec::new() };
    visitor.visit_ast(ast);
    
    'p: for path in visitor.paths {
        for sub_decl in ast.decls.iter() {
            let d = find_path(sub_decl, path, 0);
            if d.is_ok() {
                path_map.insert(path, d.unwrap());
                continue 'p;
            }    
        }
        return Err(AstError::PathNotFound {
            path,
            index: 0,
            decl: Right(ast),
        });
    }
    Ok(path_map)
}

#[cfg(test)]
mod ast_to_ir_test {
    use super::*;
    use crate::parse;

    #[test]
    fn test_check_path() {
        let ast = parse(
            "\
            class A {
                fn foo(a: i32): i32 { return (a + 1); }
                class B {
                    fn bar(a: i32): i32 { return (a + 2); }
                }
            }
            fn main() {
                A::foo(1);
                A::B::bar(2);
            }
            "
        ).unwrap();

        let _t = check_path(&ast).unwrap();
        // for (k, v) in _t.map.iter() {
        //     println!("{} -> {}", k.key, v.name());
        // }
    }
}
