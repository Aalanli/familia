use std::collections::HashMap;

use crate::ast;
use crate::ast::Span;
use crate::ir;

pub enum AstError {
    InvalidClassNest(ast::Ident, Span)
}

pub fn check_basic(ast: &ast::AST) -> Result<(), AstError> {
    for decl in ast.decls.iter() {
        if let ast::DeclKind::ClassImpl { name, sub_decls } = &decl.kind {
            for sub_decl in sub_decls.iter() {
                if let ast::DeclKind::ClassImpl { name, sub_decls } = &sub_decl.kind {
                    return Err(AstError::InvalidClassNest(name.clone(), sub_decl.span));
                }
            }
        }
    }
    Ok(())
}
