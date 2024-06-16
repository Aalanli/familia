use std::collections::HashMap;
use std::hash::Hash;
use anyhow::{Result, anyhow};
use either::Either;

use crate::Program;
use crate::ast::{self, Decl};
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

// disallow nested classes for now
fn check_basic(program: &Program) -> Result<()> {
    if let ast::DeclKind::Module { name, file, top, decls } = &program.ast.kind {
        for decl in decls.iter() {
            if let ast::DeclKind::ClassImpl { sub_decls, .. } = &decl.kind {
                for sub_decl in sub_decls.iter() {
                    if let ast::DeclKind::ClassImpl { name, .. } = &sub_decl.kind {
                        return Err(program.err(decl.span, "nested classes not allowed for now"));
                    }
                }
            }
        }
        Ok(())
    } else {
        Err(program.err(program.ast.span, "expected module"))
    }
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

fn find_path<'a>(program: &Program, ast: &'a ast::Decl, path: &'a ast::Path, pindex: usize) -> Result<&'a ast::Decl> {
    match &ast.kind {
        ast::DeclKind::Module { decls, .. } => {
            for sub_decl in decls.iter() {
                let p = find_path(program, sub_decl, path, pindex);
                if p.is_ok() {
                    return p;
                }
            }
        },
        ast::DeclKind::ClassImpl { sub_decls, name } => {
            if name.name == path.path[pindex].name {
                if pindex == path.path.len() - 1 {
                    return Ok(ast);
                }
                for sub_decl in sub_decls.iter() {
                    let p = find_path(program, sub_decl, path, pindex + 1);
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

    Err(program.err(path.span, "could not find path"))
}

// every path must be fully qualified for now
fn check_path(ast: &Program) -> Result<PathMap> {
    let mut path_map = PointerHashMap::new();
    let mut visitor = GetPath { paths: Vec::new() };
    visitor.visit_decl(&ast.ast);
    
    for path in visitor.paths {
        let d = find_path(&ast, &ast.ast, path, 0);
        if d.is_ok() {
            path_map.insert(path, d.unwrap());
            continue;
        }    
        return Err(ast.err(path.span, "unbound path"));
    }
    Ok(path_map)
}

struct CollectTypeDecl<'a> {
    decls: Vec<&'a ast::Decl>,
}

impl<'a> ast::Visitor<'a> for CollectTypeDecl<'a> {
    fn visit_decl(&mut self, decl: &'a ast::Decl) {
        match &decl.kind {
            ast::DeclKind::TypeDecl { .. } => {
                self.decls.push(decl);
            }
            _ => {}
        }
    }
}

struct CollectFnImpl<'a> {
    impls: Vec<&'a ast::Decl>,
}

impl<'a> ast::Visitor<'a> for CollectFnImpl<'a> {
    fn visit_decl(&mut self, decl: &'a ast::Decl) {
        match &decl.kind {
            ast::DeclKind::FnImpl { .. } => {
                self.impls.push(decl);
            }
            _ => {}
        }
    }
}

/*
struct CollectTypePath<'a> {
    paths: Vec<&'a ast::Path>,
}

impl<'a> ast::Visitor<'a> for CollectTypePath<'a> {
    fn visit_type(&mut self, ty: &'a ast::Type) {
        match &ty.kind {
            ast::TypeKind::Symbol(path) => {
                self.paths.push(path);
            }
            _ => { ast::default_visit_type(ty, self); }
        }
    }
}

fn check_circular_type_dep<'a>(ast: &'a ast::AST, path_map: PathMap<'a>) -> Result<(), AstError<'a>> {
    let mut visitor = CollectTypeDecl { decls: Vec::new() };
    visitor.visit_ast(ast);
    for decl in visitor.decls {
        let mut visited = PointerHashMap::new();
        let mut stack = Vec::new();
        stack.push(decl);
        let mut worklist = CollectTypePath { paths: Vec::new() };
        worklist.visit_decl(decl);
        while let Some(p) = worklist.paths.pop() {
            if visited.get(p).is_some() {
                return Err(AstError::CircularDependency {
                    dep_cycle: stack,
                });
            }
            visited.insert(p, ());
            if let Some(d) = path_map.get(p) {
                stack.push(d);
                worklist.visit_decl(d);
            }
        }

    }

    
    Ok(())
}
*/

struct ASTToIR<'a> {
    path_map: PathMap<'a>,
    program: &'a Program,
    ty_decl_to_id: PointerHashMap<'a, ast::Decl, ir::TypeDeclID>,
    fn_impl_to_id: PointerHashMap<'a, ast::Decl, ir::FuncID>,
}

impl<'a> ASTToIR<'a> {
    fn recursively_constuct_types(&mut self, ty: &ast::Type, ir: &mut ir::IR) -> ir::TypeKind {
        let ty_kind;
        match &ty.kind {
            ast::TypeKind::Void => {
                ty_kind = ir::TypeKind::Void;   
            }
            ast::TypeKind::I32 => {
                ty_kind = ir::TypeKind::I32;
            }
            ast::TypeKind::Struct { fields } => {
                let mut field_ids = Vec::new();
                for field in fields {
                    let fsym = ir.insert_symbol(field.name.get_str());
                    // by the grammar, ty is always Some
                    let fty = field.ty.as_ref().unwrap();
                    let fty_kind = self.recursively_constuct_types(fty, ir);
                    field_ids.push((fsym, ir.insert_type(fty_kind)));
                }
                ty_kind = ir::TypeKind::Struct { fields: field_ids };
            }
            ast::TypeKind::Symbol(path) => {
                let decl = self.path_map.get(path).unwrap();
                let id = self.ty_decl_to_id.get(*decl).unwrap();
                ty_kind = ir::TypeKind::Decl { decl: *id };
            }
        }
        ty_kind
    }
    
    fn insert_type_decls(&mut self, ir: &mut ir::IR) {
        let mut visitor = CollectTypeDecl { decls: Vec::new() };
        visitor.visit_decl(&self.program.ast);
        for decl in visitor.decls.iter() {
            let id = ir.temporary_id();
            self.ty_decl_to_id.insert(*decl, id);
        }

        for decl in visitor.decls {
            let id = *self.ty_decl_to_id.get(decl).unwrap();
            let ast::DeclKind::TypeDecl { decl: ty, name } = &decl.kind else { unreachable!() };
            let tkind = self.recursively_constuct_types(ty, ir);
            ir.insert_with(id, ir::TypeDecl { name: ir.insert_symbol(name.name.view()), decl: ir.insert_type(tkind), span: decl.span });
        }
    }

    fn get_type(&mut self, ty: &ast::Type, ir: &mut ir::IR) -> ir::TypeID {
        let ty_kind = self.recursively_constuct_types(ty, ir);
        ir.insert_type(ty_kind)
    }

    fn get_var(&mut self, var: &ast::Var, ir: &mut ir::IR) -> ir::VarID {
        let ty = var.ty.as_ref().map(|ty| self.get_type(ty, ir));
        let id = ir.temporary_id();
        let var = ir::Var { id, name: ir.insert_symbol(var.name.get_str()), ty, span: var.span };
        ir.insert_with(id, var);
        id
    }

    fn insert_expr(&mut self, var_map: &mut HashMap<ir::SymbolID, ir::VarID>, ops: &mut Vec<ir::OPID>, expr: &ast::Expr, ir: &mut ir::IR) -> ir::VarID {
        // match &expr.kind {
            
        // }
        todo!()
    }

    fn insert_stmts(&mut self, var_map: &mut HashMap<ir::SymbolID, ir::VarID>, ops: &mut Vec<ir::OPID>, stmt: &ast::Stmt, ir: &mut ir::IR) {
        let op_kind;
        match &stmt.kind {
            ast::StmtKind::ReturnStmt { expr } => {
                let var = self.insert_expr(var_map, ops, expr, ir);
                op_kind = ir::OPKind::Return { value: var };
            }
            ast::StmtKind::ExprStmt { expr } => {
                let _var = self.insert_expr(var_map, ops, expr, ir);
                return;
            }
            ast::StmtKind::LetStmt { var, expr } => {
                let var_id = self.get_var(var, ir);
                let expr_id = self.insert_expr(var_map, ops, expr, ir);
                op_kind = ir::OPKind::Assign { lhs: var_id, rhs: expr_id };
            }
        }
        let op_id = ir.temporary_id();
        ir.insert_with(op_id, ir::OP { kind: op_kind, span: stmt.span, var: None, id: op_id });
    }


    fn insert_fn_decl(&mut self, ir: &mut ir::IR) {
        let mut visitor = CollectFnImpl { impls: Vec::new() };
        visitor.visit_decl(&self.program.ast);

        for decl in visitor.impls.iter() {
            let id = ir.temporary_id();
            self.fn_impl_to_id.insert(*decl, id);
        }

        for decl in visitor.impls {
            let id = *self.fn_impl_to_id.get(decl).unwrap();
            let ast::DeclKind::FnImpl { name, args, ty, body } = &decl.kind else { unreachable!() };
            let func_decl = ir::FuncDecl {
                name: ir.insert_symbol(name.name.view()),
                args: args.iter().map(|arg| (ir.insert_symbol(arg.name.get_str()), self.get_type(arg.ty.as_ref().unwrap(), ir))).collect(),
                ret_ty: self.get_type(ty, ir),
            };
            let mut var_map = HashMap::new();
            let mut ops = Vec::new();
            for stmt in body.iter() {
                self.insert_stmts(&mut var_map, &mut ops, stmt, ir);
            }
        }
    }



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
            ".into(), None
        ).unwrap();

        let _t = check_path(&ast).unwrap();
        // for (k, v) in _t.map.iter() {
        //     println!("{} -> {}", k.key, v.name());
        // }
    }
}
