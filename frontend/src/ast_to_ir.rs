use anyhow::Result;
use std::collections::HashMap;
use std::hash::Hash;

use crate::ast;
use crate::ast::Visitor;
use crate::ir;
use crate::Program;

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
    if let ast::DeclKind::Module { decls, .. } = &program.ast.kind {
        for decl in decls.iter() {
            if let ast::DeclKind::ClassImpl { sub_decls, .. } = &decl.kind {
                for sub_decl in sub_decls.iter() {
                    if let ast::DeclKind::ClassImpl { .. } = &sub_decl.kind {
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

fn find_path<'a>(
    program: &Program,
    ast: &'a ast::Decl,
    path: &'a ast::Path,
    pindex: usize,
) -> Result<&'a ast::Decl> {
    match &ast.kind {
        ast::DeclKind::Module { decls, .. } => {
            for sub_decl in decls.iter() {
                let p = find_path(program, sub_decl, path, pindex);
                if p.is_ok() {
                    return p;
                }
            }
        }
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
        }
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
            _ => {
                ast::default_visit_decl(decl, self);
            }
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
            _ => {
                ast::default_visit_decl(decl, self);
            }
        }
    }
}

struct CollectClassImpl<'a> {
    impls: Vec<&'a ast::Decl>,
}

impl<'a> ast::Visitor<'a> for CollectClassImpl<'a> {
    fn visit_decl(&mut self, decl: &'a ast::Decl) {
        match &decl.kind {
            ast::DeclKind::ClassImpl { .. } => {
                self.impls.push(decl);
            }
            _ => {}
        }
        ast::default_visit_decl(decl, self);
    }
}

struct ASTToIR<'a> {
    path_map: PathMap<'a>,
    program: &'a Program,
    ty_decl_to_id: PointerHashMap<'a, ast::Decl, ir::TypeDeclID>,
    fn_impl_to_id: PointerHashMap<'a, ast::Decl, ir::FuncID>,
    class_impl_to_id: PointerHashMap<'a, ast::Decl, ir::ClassID>,
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
            let ast::DeclKind::TypeDecl { decl: ty, name } = &decl.kind else {
                unreachable!()
            };
            let tkind = self.recursively_constuct_types(ty, ir);
            ir.insert_with(
                id,
                ir::TypeDecl {
                    name: ir.insert_symbol(name.name.view()),
                    decl: ir.insert_type(tkind),
                    span: decl.span,
                },
            );
        }
    }

    fn get_type(&mut self, ty: &ast::Type, ir: &mut ir::IR) -> ir::TypeID {
        let ty_kind = self.recursively_constuct_types(ty, ir);
        ir.insert_type(ty_kind)
    }

    fn make_var(&mut self, var: &ast::Var, ir: &mut ir::IR) -> ir::VarID {
        let ty = var.ty.as_ref().map(|ty| self.get_type(ty, ir));
        let id = ir.temporary_id();
        let var = ir::Var {
            id,
            name: ir.insert_symbol(var.name.get_str()),
            ty,
            span: Some(var.span),
        };
        ir.insert_with(id, var);
        id
    }

    fn insert_expr(
        &mut self,
        var_map: &mut HashMap<ir::SymbolID, ir::VarID>,
        ops: &mut Vec<ir::OPID>,
        expr: &ast::Expr,
        ir: &mut ir::IR,
    ) -> ir::VarID {
        match &expr.kind {
            ast::ExprKind::Var(v) => {
                let sym = ir.insert_symbol(v.name.get_str());
                let var_id = var_map.get(&sym).unwrap();
                return *var_id;
            }
            ast::ExprKind::IntLit(i) => {
                let id = ir.temporary_id();
                let var = ir.new_var(None, None, Some(expr.span));
                ir.insert_with(
                    id,
                    ir::OP {
                        kind: ir::OPKind::Constant { value: *i },
                        span: expr.span,
                        res: Some(var),
                        id,
                    },
                );
                ops.push(id);
                return var;
            }
            ast::ExprKind::GetAttr { exp, sym } => {
                let expr_id = self.insert_expr(var_map, ops, exp, ir);
                let var = ir.new_var(None, None, Some(expr.span));
                let id = ir.temporary_id();
                ir.insert_with(
                    id,
                    ir::OP {
                        kind: ir::OPKind::GetAttr {
                            obj: expr_id,
                            attr: ir.insert_symbol(sym.get_str()),
                            idx: None,
                        },
                        span: expr.span,
                        res: Some(var),
                        id,
                    },
                );
                ops.push(id);
                return var;
            }
            ast::ExprKind::Call { path, args } => {
                let var_ids = args
                    .iter()
                    .map(|expr| self.insert_expr(var_map, ops, expr, ir))
                    .collect();
                let func_id = *self
                    .fn_impl_to_id
                    .get(*self.path_map.get(path).unwrap())
                    .unwrap();
                let var = ir.new_var(None, None, Some(expr.span));
                let id = ir.temporary_id();
                ir.insert_with(
                    id,
                    ir::OP {
                        kind: ir::OPKind::Call {
                            func: func_id,
                            args: var_ids,
                        },
                        span: expr.span,
                        res: Some(var),
                        id,
                    },
                );
                ops.push(id);
                return var;
            }
            ast::ExprKind::Add { lhs, rhs } => {
                let lhs_id = self.insert_expr(var_map, ops, lhs, ir);
                let rhs_id = self.insert_expr(var_map, ops, rhs, ir);
                let var = ir.new_var(None, None, Some(expr.span));
                let id = ir.temporary_id();
                ir.insert_with(
                    id,
                    ir::OP {
                        kind: ir::OPKind::Add {
                            lhs: lhs_id,
                            rhs: rhs_id,
                        },
                        span: expr.span,
                        res: Some(var),
                        id,
                    },
                );
                ops.push(id);
                return var;
            }
            ast::ExprKind::Struct { args } => {
                let mut field_ids = Vec::new();
                for (sym, expr) in args.iter() {
                    let var_id = self.insert_expr(var_map, ops, expr, ir);
                    field_ids.push((ir.insert_symbol(sym.get_str()), var_id));
                }
                let var = ir.new_var(None, None, Some(expr.span));
                let id = ir.temporary_id();
                ir.insert_with(
                    id,
                    ir::OP {
                        kind: ir::OPKind::Struct { fields: field_ids },
                        span: expr.span,
                        res: Some(var),
                        id,
                    },
                );
                ops.push(id);
                return var;
            }
        }
    }

    fn insert_stmts(
        &mut self,
        var_map: &mut HashMap<ir::SymbolID, ir::VarID>,
        ops: &mut Vec<ir::OPID>,
        stmt: &ast::Stmt,
        ir: &mut ir::IR,
    ) {
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
                let var_id = self.make_var(var, ir);
                let var_sym = ir.insert_symbol(var.name.get_str());
                var_map.insert(var_sym, var_id);
                let expr_id = self.insert_expr(var_map, ops, expr, ir);
                op_kind = ir::OPKind::Assign {
                    lhs: var_id,
                    rhs: expr_id,
                };
            }
        }
        let op_id = ir.temporary_id();
        ir.insert_with(
            op_id,
            ir::OP {
                kind: op_kind,
                span: stmt.span,
                res: None,
                id: op_id,
            },
        );
    }

    fn insert_fn_impl(&mut self, ir: &mut ir::IR) {
        let mut visitor = CollectFnImpl { impls: Vec::new() };
        visitor.visit_decl(&self.program.ast);

        for decl in visitor.impls.iter() {
            let id = ir.temporary_id();
            self.fn_impl_to_id.insert(*decl, id);
        }

        for decl in visitor.impls {
            let id = *self.fn_impl_to_id.get(decl).unwrap();
            let ast::DeclKind::FnImpl {
                name,
                args,
                ty,
                body,
            } = &decl.kind
            else {
                unreachable!()
            };
            let func_decl = ir::FuncDecl {
                name: ir.insert_symbol(name.name.view()),
                args: args
                    .iter()
                    .map(|arg| {
                        (
                            ir.insert_symbol(arg.name.get_str()),
                            self.get_type(arg.ty.as_ref().unwrap(), ir),
                        )
                    })
                    .collect(),
                ret_ty: self.get_type(ty, ir),
            };
            let func_vars: Vec<_> = args.iter().map(|v| self.make_var(v, ir)).collect();
            let mut var_map =
                func_vars
                    .iter()
                    .zip(args.iter())
                    .fold(HashMap::new(), |mut acc, (var_id, arg)| {
                        acc.insert(ir.insert_symbol(arg.name.get_str()), *var_id);
                        acc
                    });
            let mut ops = Vec::new();
            for stmt in body.iter() {
                self.insert_stmts(&mut var_map, &mut ops, stmt, ir);
            }

            ir.insert_with(
                id,
                ir::FuncImpl {
                    decl: func_decl,
                    vars: func_vars,
                    body: ops,
                },
            );
        }
    }

    pub fn insert_class_impls(&mut self, ir: &mut ir::IR) {
        let mut visitor = CollectClassImpl { impls: Vec::new() };
        visitor.visit_decl(&self.program.ast);

        for decl in visitor.impls.iter() {
            let id = ir.temporary_id();
            self.class_impl_to_id.insert(*decl, id);
        }

        for decl in visitor.impls {
            let id = *self.class_impl_to_id.get(decl).unwrap();
            let ast::DeclKind::ClassImpl { name, sub_decls } = &decl.kind else {
                unreachable!()
            };
            let class_decl = ir::ClassDecl {
                name: ir.insert_symbol(name.name.view()),
                repr_ty: ir.insert_type(ir::TypeKind::Void),
            };
            let mut class_methods = vec![];
            for sdecl in sub_decls.iter() {
                if let ast::DeclKind::FnImpl { .. } = sdecl.kind {
                    let func_id = *self.fn_impl_to_id.get(sdecl).unwrap();
                    class_methods.push(func_id);
                }
            }
            ir.insert_with(
                id,
                ir::ClassImpl {
                    decl: class_decl,
                    methods: class_methods,
                },
            );
        }
    }

    pub fn lower_ir(&mut self, ir: &mut ir::IR) {
        self.insert_type_decls(ir);
        self.insert_fn_impl(ir);
        self.insert_class_impls(ir);
    }
}

pub fn ast_to_ir(program: &Program) -> Result<ir::IR> {
    check_basic(program)?;
    let path_map = check_path(program)?;
    let mut ast_to_ir = ASTToIR {
        path_map,
        program,
        ty_decl_to_id: PointerHashMap::new(),
        fn_impl_to_id: PointerHashMap::new(),
        class_impl_to_id: PointerHashMap::new(),
    };
    let mut ir = ir::IR::new();
    ast_to_ir.lower_ir(&mut ir);
    Ok(ir)
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
            .into(),
            None,
        )
        .unwrap();

        let _t = check_path(&ast).unwrap();
        // for (k, v) in _t.map.iter() {
        //     println!("{} -> {}", k.key, v.name());
        // }
    }

    #[test]
    fn test_ast_to_ir() {
        let ast = parse(
            "\
            type T = {a: i32, b: i32}
            type R = {a: T, b: T}
            class A {
                fn foo(a: T): i32 { return (a.a + 1); }
            }
            fn main() {
                A::foo({a: 1, b: 2});
            }
            "
            .into(),
            None,
        )
        .unwrap();

        let _ir = ast_to_ir(&ast).unwrap();
        println!("{}", ir::print_basic(&_ir));
    }
}
