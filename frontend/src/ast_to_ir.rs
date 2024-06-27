use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;

use lazy_static::lazy_static;

use crate::prelude::*;
use crate::ast as ast;
use ast::Decl;
use ast::Visitor;
use crate::ir as ir;


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


lazy_static!(
    static ref BUILTIN_TYPES: HashMap<&'static str, ir::TypeKind> = {
        let mut m = HashMap::new();
        m.insert("i32", ir::TypeKind::I32);
        m.insert("String", ir::TypeKind::String);
        m
    };
);

struct BuiltinFnProto {
    name: &'static str,
    args: Vec<ir::TypeKind>,
    ret_ty: ir::TypeKind,
}

lazy_static!(
    static ref BUILTIN_FNS: HashMap<&'static str, BuiltinFnProto> = {
        let mut m = HashMap::new();
        m.insert("print", BuiltinFnProto { 
            name: "print", 
            args: vec![ir::TypeKind::String], 
            ret_ty: ir::TypeKind::Void
        });
        m.insert("to_str", BuiltinFnProto { 
            name: "to_str", 
            args: vec![ir::TypeKind::I32], 
            ret_ty: ir::TypeKind::String
        });
        m.insert("str_concat", BuiltinFnProto { 
            name: "str_concat", 
            args: vec![ir::TypeKind::String, ir::TypeKind::String], 
            ret_ty: ir::TypeKind::String
        });
        m
    };

);

fn get_builtin_type(path: &ast::Path) -> PResult<ir::TypeKind> {
    if path.len() == 1 {
        let name = path.path[0].name.view();
        if let Some(ty) = BUILTIN_TYPES.get(name) {
            return Ok(ty.clone());
        }
    }

    Err(ProgramError { error_message: "unbound type name", span: Some(path.span), ..Default::default() })
}

fn is_builtin_fn(path: &ast::Path) -> bool {
    if path.len() == 1 {
        let name = path.path[0].name.view();
        if BUILTIN_FNS.contains_key(name) {
            return true;
        }
    }
    false
}

fn check_duplicate_name<'a>(src: &'a ModSource, ast: &Decl) -> PhaseResult<'a, ()> {
    fn check_duplicate_name_helper<'a>(src: &'a ModSource, ast: &Decl) {
        let mut names = HashSet::new();
        match &ast.kind {
            ast::DeclKind::Module { name, file, top, decls } => {
                for subdecl in decls {
                    let name = subdecl.name();
                    if names.contains(name) {
                        src.add_err(ProgramError {
                            error_type: ErrorType::Error,
                            error_message: "duplicate name",
                            span: Some(subdecl.span),
                            highlight_message: None,
                        });
                    } else {
                        names.insert(name);
                    }
                }
            }
            ast::DeclKind::ClassImpl { name, sub_decls } => {
                for subdecl in sub_decls {
                    let name = subdecl.name();
                    if names.contains(name) {
                        src.add_err(ProgramError {
                            error_type: ErrorType::Error,
                            error_message: "duplicate name",
                            span: Some(subdecl.span),
                            highlight_message: None,
                        });
                    } else {
                        names.insert(name);
                    }
                }
            }
            _ => {}
        }

    }
    check_duplicate_name_helper(src, ast);
    src.commit_error(())
}

// disallow nested classes for now
fn check_basic<'a>(src: &'a ModSource, ast: &Decl) -> PhaseResult<'a, ()> {
    check_duplicate_name(src, ast)?;
    if let ast::DeclKind::Module { decls, .. } = &ast.kind {
        for decl in decls.iter() {
            if let ast::DeclKind::ClassImpl { sub_decls, .. } = &decl.kind {
                for sub_decl in sub_decls.iter() {
                    if let ast::DeclKind::ClassImpl { .. } = &sub_decl.kind {
                        src.add_err(ProgramError {
                            error_type: ErrorType::Error,
                            error_message: "unvalid class nest",
                            span: Some(sub_decl.span),
                            highlight_message: None,
                        });
                    }
                }
            }
        }
    } else {
        src.add_err(ProgramError { error_message: "expected a module", error_type: err::InternalError ,..Default::default() });
    }
    src.commit_error(())
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
    ast: &'a ast::Decl,
    path: &'a ast::Path,
    pindex: usize,
) -> PResult<&'a ast::Decl> {
    match &ast.kind {
        ast::DeclKind::Module { decls, .. } => {
            for sub_decl in decls.iter() {
                let p = find_path(sub_decl, path, pindex);
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
                    let p = find_path(sub_decl, path, pindex + 1);
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

    Err(ProgramError { error_message: "unbound path", span: Some(ast.span), ..Default::default() })
}

// every path must be fully qualified for now
fn check_path<'a, 's>(src: &'s ModSource, ast: &'a Decl) -> PhaseResult<'s, PathMap<'a>> {
    let mut path_map = PointerHashMap::new();
    let mut visitor = GetPath { paths: Vec::new() };
    visitor.visit_decl(&ast);

    for path in visitor.paths {
        let d = find_path(ast, path, 0);
        if d.is_ok() {
            path_map.insert(path, d.unwrap());
            continue;
        } else if get_builtin_type(path).is_ok() {
            continue;
        } else if is_builtin_fn(path) {
            continue;
        }
        src.add_err(d.unwrap_err());
    }
    src.commit_error(path_map)
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

pub struct PrimitiveRegistry {
    pub i32: ir::TypeID,
    pub string: ir::TypeID,
    pub void: ir::TypeID,
    pub print: ir::FuncID,
    pub to_str: ir::FuncID,
    pub concat: ir::FuncID,

}

struct ASTToIR<'a> {
    ast: &'a ast::Decl,
    path_map: PathMap<'a>,
    builtins_fn_map: HashMap<&'static str, ir::FuncID>,
    ty_decl_to_id: PointerHashMap<'a, ast::Decl, ir::TypeDeclID>,
    ty_decl_to_ty_id: PointerHashMap<'a, ast::Decl, ir::TypeID>,
    fn_impl_to_id: PointerHashMap<'a, ast::Decl, ir::FuncID>,
    class_impl_to_id: PointerHashMap<'a, ast::Decl, ir::ClassID>,
}

impl<'a> ASTToIR<'a> {
    fn recursively_construct_types(&self, ty: &ast::Type, ir: &mut ir::IR) -> PResult<ir::TypeKind> {
        let ty_kind;
        match &ty.kind {
            ast::TypeKind::Void => {
                ty_kind = ir::TypeKind::Void;
            }
            ast::TypeKind::I32 => {
                ty_kind = ir::TypeKind::I32;
            }
            ast::TypeKind::String => {
                ty_kind = ir::TypeKind::String;
            }
            ast::TypeKind::Struct { fields } => {
                let mut field_ids = Vec::new();
                for field in fields {
                    let fsym = ir::SymbolID::insert(ir, field.name.get_str());
                    // by the grammar, ty is always Some
                    let fty = field.ty.as_ref().unwrap();
                    let fty_kind = self.recursively_construct_types(fty, ir)?;
                    field_ids.push((fsym, ir::TypeID::insert_type(ir, fty_kind)));
                }
                ty_kind = ir::TypeKind::Struct { fields: field_ids };
            }
            ast::TypeKind::Symbol(path) => {

                if let Some(decl) = self.path_map.get(path) {
                    let id = self.ty_decl_to_ty_id.get(*decl).unwrap();
                    ty_kind = ir::TypeKind::Rec { id: *id };
                } else {
                    ty_kind = get_builtin_type(path)?;
                }
            }
        }
        Ok(ty_kind)
    }

    fn insert_builtin_fns(&mut self, ir: &mut ir::IR) {
        let fdecl = ir::FuncDecl { 
            name: ir::SymbolID::insert(ir, "print"), 
            args: vec![(ir::SymbolID::insert(ir, "a"), ir::TypeID::insert_type(ir, ir::TypeKind::String))], 
            ret_ty: ir::TypeID::insert_type(ir, ir::TypeKind::Void) 
        };
        let fimpl = ir::FuncImpl {
            decl: fdecl,
            vars: vec![],
            body: vec![],
            builtin: true,
        };

        let fid = ir.insert(fimpl);
        self.builtins_fn_map.insert("print", fid);

        let fdecl = ir::FuncDecl { 
            name: ir::SymbolID::insert(ir, "to_str"), 
            args: vec![(ir::SymbolID::insert(ir, "a"), ir::TypeID::insert_type(ir, ir::TypeKind::I32))], 
            ret_ty: ir::TypeID::insert_type(ir, ir::TypeKind::String) 
        };
        let fimpl = ir::FuncImpl {
            decl: fdecl,
            vars: vec![],
            body: vec![],
            builtin: true,
        };

        let fid = ir.insert(fimpl);
        self.builtins_fn_map.insert("to_str", fid);
        
    }

    fn insert_type_decls<'s>(&mut self, ir: &mut ir::IR, src: &'s ModSource) -> PhaseResult<'s, ()> {
        let mut visitor = CollectTypeDecl { decls: Vec::new() };
        visitor.visit_decl(&self.ast);
        for decl in visitor.decls.iter() {
            let id = ir.temporary_id();
            self.ty_decl_to_id.insert(*decl, id);
            let tid = ir.temporary_id();
            self.ty_decl_to_ty_id.insert(*decl, tid);
        }

        for decl in visitor.decls {
            let id = *self.ty_decl_to_id.get(decl).unwrap();
            let tid = *self.ty_decl_to_ty_id.get(decl).unwrap();
            let ast::DeclKind::TypeDecl { decl: ty, name } = &decl.kind else {
                unreachable!()
            };
            let tkind = self.recursively_construct_types(ty, ir);
            if let Err(e) = tkind {
                src.add_err(e);
                continue;
            }
            let tkind = tkind.unwrap();
            ir.insert_with(tid, ir::Type{ kind: tkind });
            ir.insert_with(
                id,
                ir::TypeDecl {
                    name: ir::SymbolID::insert(ir, name.name.view()),
                    decl: tid,
                    span: decl.span,
                },
            );
        }
        src.commit_error(())
    }

    fn get_type(&self, ty: &ast::Type, ir: &mut ir::IR) -> PResult<ir::TypeID> {
        let ty_kind = self.recursively_construct_types(ty, ir)?;
        Ok(ir::TypeID::insert_type(ir, ty_kind))
    }

    fn make_var(&mut self, var: &ast::Var, ir: &mut ir::IR) -> PResult<ir::VarID> {
        let ty = var.ty.as_ref().map(|ty| self.get_type(ty, ir)).transpose()?;
        let id = ir.temporary_id();
        let var = ir::Var {
            id,
            name: ir::SymbolID::insert(ir, var.name.get_str()),
            ty,
            span: Some(var.span),
        };
        ir.insert_with(id, var);
        Ok(id)
    }

    fn insert_expr(
        &mut self,
        var_map: &mut HashMap<ir::SymbolID, ir::VarID>,
        ops: &mut Vec<ir::OPID>,
        expr: &ast::Expr,
        parent_name: Option<ir::SymbolID>,
        ir: &mut ir::IR,
    ) -> ir::VarID {
        match &expr.kind {
            ast::ExprKind::Var(v) => {
                let sym = ir::SymbolID::insert(ir, v.name.get_str());
                let var_id = var_map.get(&sym).unwrap();
                return *var_id;
            }
            ast::ExprKind::IntLit(i) => {
                let id = ir.temporary_id();
                let var = ir::VarID::new_var(ir, parent_name, None, Some(expr.span));
                ir.insert_with(
                    id,
                    ir::OP {
                        kind: ir::OPKind::Constant(ir::ConstKind::I32(*i)),
                        span: expr.span,
                        res: Some(var),
                        id,
                    },
                );
                ops.push(id);
                return var;
            }
            ast::ExprKind::StringLit(s) => {
                let id = ir.temporary_id();
                let var = ir::VarID::new_var(ir, parent_name, None, Some(expr.span));
                ir.insert_with(
                    id,
                    ir::OP {
                        kind: ir::OPKind::Constant(ir::ConstKind::String(ir::SymbolID::insert(ir, s))),
                        span: expr.span,
                        res: Some(var),
                        id,
                    },
                );
                ops.push(id);
                return var;
            }
            ast::ExprKind::GetAttr { exp, sym } => {
                let expr_id = self.insert_expr(var_map, ops, exp, parent_name, ir);
                let var = ir::VarID::new_var(ir, parent_name, None, Some(expr.span));
                let id = ir.temporary_id();
                ir.insert_with(
                    id,
                    ir::OP {
                        kind: ir::OPKind::GetAttr {
                            obj: expr_id,
                            attr: ir::SymbolID::insert(ir, sym.get_str()),
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
                    .map(|expr| self.insert_expr(var_map, ops, expr, parent_name, ir))
                    .collect();
                let func_id = *self
                    .fn_impl_to_id
                    .get(*self.path_map.get(path).unwrap()).unwrap_or_else(|| {
                        assert!(is_builtin_fn(path));
                        self.builtins_fn_map.get(path.path[0].name.view()).unwrap()
                    });


                let var = ir::VarID::new_var(ir, parent_name, None, Some(expr.span));
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
                let lhs_id = self.insert_expr(var_map, ops, lhs, parent_name, ir);
                let rhs_id = self.insert_expr(var_map, ops, rhs, parent_name, ir);
                let var = ir::VarID::new_var(ir, parent_name, None, Some(expr.span));
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
                    let var_id = self.insert_expr(var_map, ops, expr, parent_name, ir);
                    field_ids.push((ir::SymbolID::insert(ir, &sym.name.view()), var_id));
                }
                let var = ir::VarID::new_var(ir, parent_name, None, Some(expr.span));
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
                let var = self.insert_expr(var_map, ops, expr, None, ir);
                op_kind = ir::OPKind::Return { value: var };
            }
            ast::StmtKind::ExprStmt { expr } => {
                let _var = self.insert_expr(var_map, ops, expr, None, ir);
                return;
            }
            ast::StmtKind::LetStmt { var, expr } => {
                let var_sym = ir::SymbolID::insert(ir, var.name.get_str());
                let expr_id = self.insert_expr(var_map, ops, expr, Some(var_sym), ir);
                var_map.insert(var_sym, expr_id);
                // op_kind = ir::OPKind::Assign {
                //     lhs: var_id,
                //     rhs: expr_id,
                // };
                return;
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
        ops.push(op_id);
    }

    fn insert_fn_impl<'s>(&mut self, ir: &mut ir::IR, src: &'s ModSource) -> PhaseResult<'s, ()> {
        let mut visitor = CollectFnImpl { impls: Vec::new() };
        visitor.visit_decl(&self.ast);

        for decl in visitor.impls.iter() {
            let id = ir.temporary_id();
            self.fn_impl_to_id.insert(*decl, id);
        }

        'fns: for decl in visitor.impls {
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

            let mut fn_args = vec![];
            for arg in args {
                let ty = self.get_type(arg.ty.as_ref().unwrap(), ir);
                let sym = ir::SymbolID::insert(ir, arg.name.get_str());
                match ty {
                    Ok(ty) => fn_args.push((sym, ty)),
                    Err(e) => {
                        src.add_err(e);
                        continue 'fns;
                    }
                }
            }
            let fn_ret_ty = self.get_type(ty, ir);
            if fn_ret_ty.is_err() {
                src.add_err(fn_ret_ty.unwrap_err());
                continue 'fns;
            }

            let func_decl = ir::FuncDecl {
                name: ir::SymbolID::insert(ir, name.name.view()),
                args: fn_args,
                ret_ty: fn_ret_ty.unwrap(),
            };

            let func_vars: PResult<Vec<_>> = args.iter().map(|v| self.make_var(v, ir)).collect();
            if func_vars.is_err() {
                src.add_err(func_vars.unwrap_err());
                continue 'fns;
            }
            let func_vars = func_vars.unwrap();
            let mut var_map =
                func_vars
                    .iter()
                    .zip(args.iter())
                    .fold(HashMap::new(), |mut acc, (var_id, arg)| {
                        acc.insert(ir::SymbolID::insert(ir, arg.name.get_str()), *var_id);
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
                    builtin: false,
                },
            );
        }
        src.commit_error(())
    }

    pub fn insert_class_impls(&mut self, ir: &mut ir::IR) {
        let mut visitor = CollectClassImpl { impls: Vec::new() };
        visitor.visit_decl(&self.ast);

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
                name: ir::SymbolID::insert(ir, name.name.view()),
                repr_ty: ir::TypeID::insert_type(ir, ir::TypeKind::Void),
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

    pub fn lower_ir<'s>(&mut self, ir: &mut ir::IR, src: &'s ModSource) -> PhaseResult<'s, ()> {
        self.insert_builtin_fns(ir);
        self.insert_type_decls(ir, src)?;
        self.insert_fn_impl(ir, src)?;
        self.insert_class_impls(ir);
        Ok(())
    }
}


pub fn ast_to_ir<'s, 'a>(src: &'s ModSource, ast: &'a Decl) -> PhaseResult<'s, ir::IR> {
    check_basic(src, ast)?;
    let path_map = check_path(src, ast)?;
    let mut ast_to_ir = ASTToIR {
        path_map,
        ast,
        builtins_fn_map: HashMap::new(),
        ty_decl_to_id: PointerHashMap::new(),
        ty_decl_to_ty_id: PointerHashMap::new(),
        fn_impl_to_id: PointerHashMap::new(),
        class_impl_to_id: PointerHashMap::new(),
    };
    let mut ir = ir::IR::new();
    ast_to_ir.lower_ir(&mut ir, src)?;
    Ok(ir)
}


#[cfg(test)]
mod ast_to_ir_test {
    use super::*;
    use crate::parse;

    #[test]
    fn test_check_path() {
        let src = "\
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
            ".into();
        let ast = parse(&src).unwrap();

        let _t = check_path(&src, &ast).unwrap();
        // for (k, v) in _t.map.iter() {
        //     println!("{} -> {}", k.key, v.name());
        // }
    }

    #[test]
    fn test_ast_to_ir() {
        let src =
        "\
        type T = {a: i32, b: i32}
        type R = {a: T, b: T}
        class A {
            fn foo(a: T): i32 { return (a.a + 1); }
        }
        fn main() {
            A::foo({a: 1, b: 2});
        }
        ".into();
        let ast = parse(&src
        )
        .unwrap();
        // println!("{:?}", ast);

        let _ir = ast_to_ir(&src, &ast).unwrap();
        println!("{}", ir::print_basic(&_ir));
    }
}
