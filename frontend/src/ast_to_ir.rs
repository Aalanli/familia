use std::collections::HashMap;

use either::{Either, Left, Right};

use crate::ast;
use crate::ast::{Span, Visitor};
use crate::ir;

#[derive(Debug)]
pub enum AstError<'a> {
    InvalidClassNest(&'a ast::Ident, Span),
    PathNotFound {
        path: &'a ast::Path,
        index: usize,
        decl: Either<&'a ast::Decl, &'a ast::AST>,
    }
}

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

type PathMap<'a> = HashMap<&'a ast::Path, &'a ast::Decl>;

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
    let mut path_map = HashMap::new();
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

