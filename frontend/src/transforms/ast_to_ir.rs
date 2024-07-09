use lazy_static::lazy_static;
use std::collections::HashMap;
use std::collections::HashSet;

use super::utils::PointerHashMap;
use super::PrimitiveRegistry;
use crate::ast;
use crate::ir;
use crate::prelude::*;

use ast::Decl;
use ast::Visitor;

pub fn ast_to_ir(src: ModSource, ast: Decl) -> PhaseResult<ir::IR> {
    check_basic(&src, &ast)?;
    let path_analysis = path_analysis(&src, &ast)?;
    let path_to_decl = path_to_decl(&src, &ast, &path_analysis)?;
    assert_no_type_cycle(&src, &path_to_decl, &ast)?;
    let ir = ir::IR::new();
    let mut state = LowerModule {
        src,
        path_map: path_to_decl,
        ir,
        temp: Temporaries::new(),
    };
    insert_builtins(&mut state);
    visit_module(&mut state, &ast).and_then(|id| {
        state.ir.insert_global(id);
        Some(())
    });
    state.src.commit_error(state.ir)
}

lazy_static! {
    static ref BUILTIN_TYPES: HashMap<&'static str, ir::TypeKind> = {
        let mut m = HashMap::new();
        m.insert("i32", ir::TypeKind::I32);
        m.insert("String", ir::TypeKind::String);
        m.insert("This", ir::TypeKind::This);
        m.insert("Self", ir::TypeKind::Self_);
        m
    };
}

const BUILTIN_INCLUDE: &'static str = "\
fn print(s: String)
fn to_str(i: i32): String
";

lazy_static! {
    static ref BUILTIN_INC: Vec<ast::Decl> = {
        let src = BUILTIN_INCLUDE.into();
        let m = crate::parse(&src).unwrap();
        if let ast::DeclKind::Module { decls, .. } = m.kind {
            decls
        } else {
            unreachable!()
        }
    };
    static ref BUILTIN_FNS: HashSet<&'static str> = {
        let mut s = HashSet::new();
        for decl in BUILTIN_INC.iter() {
            if let ast::DeclKind::FnDecl { name, .. } = &decl.kind {
                s.insert(name.get_str());
            }
        }
        s
    };
}

fn is_builtin_fn(path: &ast::Path) -> bool {
    if path.len() == 1 {
        let name = path.path[0].name.view();
        BUILTIN_FNS.contains(name)
    } else {
        false
    }
}

fn get_builtin_fn(path: &ast::Path, ir: &ir::IR) -> ir::FuncID {
    let name = path.path[0].name.view();
    let glob = ir.get_global::<PrimitiveRegistry>().unwrap();
    let id = glob.fns.get(name).unwrap();
    *id
}

fn get_builtin_type(path: &ast::Path) -> PResult<ir::TypeKind> {
    if path.len() == 1 {
        let name = path.path[0].name.view();
        if let Some(ty) = BUILTIN_TYPES.get(name) {
            return Ok(ty.clone());
        }
    }

    Err(ProgramError {
        error_message: "unbound type name",
        span: Some(path.span),
        ..Default::default()
    })
}

fn is_builtin_type(path: &ast::Path) -> bool {
    if path.len() == 1 {
        let name = path.path[0].get_str();
        if BUILTIN_TYPES.contains_key(name) {
            return true;
        }
    }
    false
}

fn insert_builtins(state: &mut LowerModule<'_>) {
    let ti32 = ir::TypeID::insert(&state.ir, ir::TypeKind::I32);
    let tstring = ir::TypeID::insert(&state.ir, ir::TypeKind::String);
    let tvoid = ir::TypeID::insert(&state.ir, ir::TypeKind::Void);
    let tthis = ir::TypeID::insert(&state.ir, ir::TypeKind::This);
    let tself = ir::TypeID::insert(&state.ir, ir::TypeKind::Self_);

    let mut fns = HashMap::new();
    for decl in BUILTIN_INC.iter() {
        if let ast::DeclKind::FnDecl { name, args, ty } = &decl.kind {
            let fn_decl = get_fn_decl(state, Span::default(), name, args, ty).unwrap();
            let id = state.ir.insert(ir::FuncImpl {
                decl: fn_decl,
                vars: vec![],
                body: vec![],
                builtin: true,
            });
            fns.insert(name.get_str().into(), id);
        }
    }
    let regs = PrimitiveRegistry {
        i32: ti32,
        string: tstring,
        void: tvoid,
        This: tthis,
        Self_: tself,
        fns,
    };
    state.ir.insert_global(regs);
}

#[derive(Debug)]
struct PathAnalysis<'a> {
    name: &'a ast::Ident,
    decl: &'a ast::Decl,
    sub_decls: HashMap<&'a str, PathAnalysis<'a>>,
}

fn path_analysis_helper<'s, 'a>(src: &'s ModSource, ast: &'a Decl) -> PathAnalysis<'a> {
    let mut sub_decls = HashMap::new();
    for decl in ast.sub_decls() {
        let name = decl.name();
        if sub_decls.contains_key(name.get_str()) || BUILTIN_TYPES.contains_key(name.get_str()) {
            src.add_err(ProgramError {
                error_message: "duplicate definition",
                span: Some(name.span),
                ..Default::default()
            });
        }
        sub_decls.insert(name.get_str(), path_analysis_helper(src, decl));
    }
    PathAnalysis {
        name: ast.name(),
        decl: ast,
        sub_decls,
    }
}

fn path_analysis<'s, 'a>(src: &'s ModSource, ast: &'a Decl) -> PhaseResult<PathAnalysis<'a>> {
    let path_analysis = path_analysis_helper(src, ast);
    src.commit_error(path_analysis)
}

// disallow nested classes for now
fn check_basic<'a>(src: &'a ModSource, ast: &Decl) -> PhaseResult<()> {
    if let ast::DeclKind::Module { decls, .. } = &ast.kind {
        for decl in decls.iter() {
            if let ast::DeclKind::ClassImpl { sub_decls, .. } = &decl.kind {
                for sub_decl in sub_decls.iter() {
                    if let ast::DeclKind::ClassImpl { .. } = &sub_decl.kind {
                        src.add_err(ProgramError {
                            error_type: ErrorType::Error,
                            error_message: "unvalid class nest",
                            span: Some(sub_decl.name().span),
                            highlight_message: None,
                        });
                    }
                }
            }
            if let ast::DeclKind::InterfaceImpl {
                name,
                sub_decls: body,
            } = &decl.kind
            {
                for sub_decl in body.iter() {
                    if let ast::DeclKind::FnDecl { .. } = &sub_decl.kind {
                    } else {
                        src.add_err(ProgramError {
                            error_type: ErrorType::Error,
                            error_message: "expected a function declaration",
                            span: Some(sub_decl.name().span),
                            highlight_message: None,
                        });
                    }
                }
            }
        }
    } else {
        src.add_err(ProgramError {
            error_message: "expected a module",
            error_type: err::InternalError,
            ..Default::default()
        });
    }
    src.commit_error(())
}

struct GetPath<'a> {
    paths: Vec<&'a ast::Path>,
}

impl<'a> ast::Visitor<'a> for GetPath<'a> {
    fn visit_path(&mut self, path: &'a ast::Path) {
        self.paths.push(path);
    }
}

#[derive(Debug)]
struct PathToDeclAnalysis<'a> {
    path_map: HashMap<UniquePathHashKey<'a>, &'a ast::Decl>,
}
impl<'a> PathToDeclAnalysis<'a> {
    fn get(&self, path: &ast::Path) -> Option<&'a ast::Decl> {
        self.path_map.get(&UniquePathHashKey(path)).copied()
    }
}

// every path must be fully qualified for now
fn path_to_decl<'a, 's>(
    src: &'s ModSource,
    ast: &'a Decl,
    paths: &PathAnalysis<'a>,
) -> PhaseResult<PathToDeclAnalysis<'a>> {
    let mut path_map = HashMap::new();
    let mut visitor = GetPath { paths: Vec::new() };
    visitor.visit_decl(&ast);

    'p: for path in visitor.paths {
        let mut cur = paths;
        if is_builtin_type(path) || is_builtin_fn(path) {
            continue;
        }
        for seg in path.path.iter() {
            if let Some(next) = cur.sub_decls.get(seg.get_str()) {
                cur = next;
            } else {
                src.add_err(ProgramError {
                    error_message: "unbound path",
                    span: Some(seg.span),
                    ..Default::default()
                });
                continue 'p;
            }
        }
        path_map.insert(UniquePathHashKey(path), cur.decl);
    }
    src.commit_error(PathToDeclAnalysis { path_map })
}

struct CollectTypeDecl<'a> {
    decls: Vec<&'a ast::Decl>,
}

impl<'a> ast::Visitor<'a> for CollectTypeDecl<'a> {
    fn visit_decl(&mut self, decl: &'a ast::Decl) {
        if let ast::DeclKind::TypeDecl { .. } = &decl.kind {
            self.decls.push(decl);
        } else {
            ast::default_visit_decl(decl, self);
        }
    }
}

fn assert_no_type_cycle_helper<'s, 'a>(
    src: &'s ModSource,
    ast: &'a ast::Decl,
    path_map: &'a PathToDeclAnalysis<'a>,
    visited: &mut PointerHashMap<'a, ast::Decl, ()>,
) -> PResult<()> {
    if visited.contains_key(ast) {
        return Err(ProgramError {
            error_message: "type cycle",
            span: Some(ast.span),
            ..Default::default()
        });
    }

    if let ast::DeclKind::TypeDecl { decl, .. } = &ast.kind {
        visited.insert(ast, ());
        for sub_path in decl.kind.ref_paths() {
            if is_builtin_type(sub_path) {
                continue;
            }
            let decl = path_map.get(sub_path).unwrap();
            assert_no_type_cycle_helper(src, decl, path_map, visited)?;
        }
        visited.remove(ast);
    }
    Ok(())
}

fn assert_no_type_cycle<'s, 'a>(
    src: &'s ModSource,
    path_map: &PathToDeclAnalysis<'a>,
    ast: &'a ast::Decl,
) -> PhaseResult<()> {
    let mut visitor = CollectTypeDecl { decls: Vec::new() };
    visitor.visit_decl(ast);
    for decl in visitor.decls.into_iter() {
        let mut visited = PointerHashMap::new();
        if let Err(e) = assert_no_type_cycle_helper(src, decl, path_map, &mut visited) {
            src.add_err(e);
        }
    }

    src.commit_error(())
}

struct Temporaries<'a> {
    ty_decl_to_id: PointerHashMap<'a, ast::Decl, ir::TypeDeclID>,
    fn_impl_to_id: PointerHashMap<'a, ast::Decl, ir::FuncID>,
    class_impl_to_id: PointerHashMap<'a, ast::Decl, ir::ClassID>,
    itf_impl_to_id: PointerHashMap<'a, ast::Decl, ir::InterfaceID>,
    path_ty_cache: HashMap<UniquePathHashKey<'a>, ir::TypeID>,
}

impl<'a> Temporaries<'a> {
    fn new() -> Self {
        Temporaries {
            ty_decl_to_id: PointerHashMap::new(),
            fn_impl_to_id: PointerHashMap::new(),
            class_impl_to_id: PointerHashMap::new(),
            itf_impl_to_id: PointerHashMap::new(),
            path_ty_cache: HashMap::new(),
        }
    }
}

#[derive(Debug)]
struct UniquePathHashKey<'a>(&'a ast::Path);

impl std::hash::Hash for UniquePathHashKey<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for seg in self.0.path.iter() {
            seg.get_str().hash(state);
        }
    }
}

impl std::cmp::Eq for UniquePathHashKey<'_> {}

impl std::cmp::PartialEq for UniquePathHashKey<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.0.path.len() == other.0.path.len()
            && self
                .0
                .path
                .iter()
                .zip(other.0.path.iter())
                .all(|(a, b)| a.get_str() == b.get_str())
    }
}

struct LowerModule<'a> {
    src: ModSource,
    path_map: PathToDeclAnalysis<'a>,
    ir: ir::IR,
    temp: Temporaries<'a>,
}

fn construct_types_helper<'a>(
    src: &ModSource,
    path_map: &PathToDeclAnalysis<'a>,
    ir: &ir::IR,
    temp: &mut Temporaries<'a>,
    ty_decl: &'a ast::Type,
) -> Option<ir::TypeID> {
    match &ty_decl.kind {
        ast::TypeKind::Void => Some(ir::TypeID::insert(&ir, ir::TypeKind::Void)),
        ast::TypeKind::I32 => Some(ir::TypeID::insert(&ir, ir::TypeKind::I32)),
        ast::TypeKind::String => Some(ir::TypeID::insert(&ir, ir::TypeKind::String)),
        ast::TypeKind::This => Some(ir::TypeID::insert(&ir, ir::TypeKind::This)),
        ast::TypeKind::Self_ => Some(ir::TypeID::insert(&ir, ir::TypeKind::Self_)),
        ast::TypeKind::Struct { fields } => {
            let mut field_ids = Vec::new();
            for field in fields {
                let fsym = ir::SymbolID::insert(&ir, field.name.get_str());
                let fty = field.ty.as_ref().unwrap();
                let fty_id = construct_types_helper(src, path_map, ir, temp, fty)?;
                field_ids.push((fsym, fty_id));
            }
            let id = ir::TypeID::insert(&ir, ir::TypeKind::Struct { fields: field_ids });
            Some(id)
        }
        ast::TypeKind::Symbol(path) => {
            if let Some(ty) = temp.path_ty_cache.get(&UniquePathHashKey(path)) {
                return Some(*ty);
            }
            let ty = if is_builtin_type(path) {
                let ty = get_builtin_type(path);
                if let Err(e) = ty {
                    src.add_err(e);
                    return None;
                }
                Some(ir::TypeID::insert(&ir, ty.unwrap()))
            } else {
                let decl = path_map.get(path).unwrap();
                match &decl.kind {
                    ast::DeclKind::TypeDecl { decl, .. } => {
                        let id = construct_types_helper(src, path_map, ir, temp, decl)?;
                        Some(id)
                    }
                    _ => {
                        src.add_err(ProgramError {
                            error_message: "expected a type declaration",
                            span: Some(path.span),
                            ..Default::default()
                        });
                        None
                    }
                }
            }?;
            temp.path_ty_cache.insert(UniquePathHashKey(path), ty);
            Some(ty)
        }
    }
}

fn construct_types<'a>(state: &mut LowerModule<'a>, ty_decl: &'a ast::Type) -> Option<ir::TypeID> {
    let LowerModule {
        src,
        path_map,
        ir,
        temp,
    } = state;
    construct_types_helper(src, path_map, ir, temp, ty_decl)
}

fn visit_module<'a>(state: &mut LowerModule<'a>, decl: &'a ast::Decl) -> Option<ir::ModuleID> {
    if let ast::DeclKind::Module {
        name,
        file,
        top,
        decls,
    } = &decl.kind
    {
        let mut module = ir::Module::default();
        for decl in decls {
            if decl.kind.is_type_decl() {
                visit_type_decl(state, decl).and_then(|id| {
                    module.types.push(id);
                    Some(())
                });
            } else if decl.kind.is_fn_impl() {
                visit_fn_impl(state, decl).and_then(|id| {
                    module.funcs.push(id);
                    Some(())
                });
            } else if decl.kind.is_class_impl() {
                visit_class_impl(state, decl).and_then(|id| {
                    module.classes.push(id);
                    Some(())
                });
            } else if decl.kind.is_interface_impl() {
                visit_interface_impl(state, decl).and_then(|id| {
                    module.interfaces.push(id);
                    Some(())
                });
            } else {
                state.src.add_err(ProgramError {
                    error_message: "expected a type declaration, function implementation, class implementation, or interface implementation",
                    span: Some(decl.span),
                    ..Default::default()
                });
            }
        }

        let globals = state.ir.get_global::<PrimitiveRegistry>().unwrap();
        for f in module.funcs.iter() {
            let func = state.ir.get(*f);
            if func.decl.name == ir::SymbolID::insert(&state.ir, "main") {
                if func.decl.ret_ty != globals.void || func.decl.args.len() != 0 {
                    state.src.add_err(ProgramError {
                        error_message: "main function must have type (): ()",
                        span: Some(func.decl.span),
                        ..Default::default()
                    });
                }
                module.main = Some(*f);
            }
        }
        if module.main.is_none() {
            state.src.add_err(ProgramError {
                error_message: "no main function",
                ..Default::default()
            });
        }
        module.src = Some(state.src.clone());

        Some(state.ir.insert(module))
    } else {
        let err = ProgramError {
            error_message: "expected a module",
            span: Some(decl.span),
            ..Default::default()
        };
        state.src.add_err(err);
        return None;
    }
}

fn visit_type_decl<'a>(state: &mut LowerModule<'a>, decl: &'a ast::Decl) -> Option<ir::TypeDeclID> {
    if let ast::DeclKind::TypeDecl { decl: tdecl, name } = &decl.kind {
        let tid = construct_types(state, tdecl)?;
        let id = state.ir.insert(ir::TypeDecl {
            name: ir::SymbolID::insert(&state.ir, name.name.view()),
            decl: tid,
            span: tdecl.span,
        });
        state.temp.ty_decl_to_id.insert(decl, id);
        Some(id)
    } else {
        let err = ProgramError {
            error_message: "expected a type declaration",
            span: Some(decl.span),
            ..Default::default()
        };
        state.src.add_err(err);
        return None;
    }
}

fn get_fn_decl<'a>(
    state: &mut LowerModule<'a>,
    span: ast::Span,
    name: &ast::Ident,
    args: &'a Vec<ast::Var>,
    ty: &'a ast::Type,
) -> Option<ir::FuncDecl> {
    let rty = construct_types(state, ty)?;
    let args: Option<Vec<_>> = args
        .iter()
        .map(|arg| {
            if arg.ty.is_none() {
                state.src.add_err(ProgramError {
                    error_message: "expected a type",
                    span: Some(arg.span),
                    ..Default::default()
                });
                return None;
            }
            let ty = construct_types(state, arg.ty.as_ref().unwrap())?;
            let sym = ir::SymbolID::insert(&state.ir, arg.name.get_str());
            Some((sym, ty))
        })
        .collect();
    let args = args?;
    Some(ir::FuncDecl {
        name: ir::SymbolID::insert(&state.ir, name.name.view()),
        args,
        span,
        ret_ty: rty,
    })
}

fn visit_var<'a>(state: &mut LowerModule<'a>, var: &'a ast::Var) -> Option<ir::VarID> {
    let ty = var.ty.as_ref().map(|ty| construct_types(state, ty))?;
    let id = state.ir.insert(ir::Var {
        name: ir::SymbolID::insert(&state.ir, var.name.get_str()),
        ty,
        span: Some(var.span),
    });
    Some(id)
}

fn visit_fn_impl<'a>(state: &mut LowerModule<'a>, decl: &'a ast::Decl) -> Option<ir::FuncID> {
    if let ast::DeclKind::FnImpl {
        name,
        args: fargs,
        ty,
        body,
    } = &decl.kind
    {
        let fdecl = get_fn_decl(state, decl.span, name, fargs, ty)?;
        let args: Option<Vec<_>> = fargs.iter().map(|arg| visit_var(state, arg)).collect();
        let args = args?;
        let mut var_map = HashMap::new();
        for (fvar, var) in fargs.iter().zip(args.iter()) {
            var_map.insert(ir::SymbolID::insert(&state.ir, fvar.name.get_str()), *var);
        }
        let mut ops = Vec::new();
        for stmt in body.iter() {
            insert_stmts(state, &mut var_map, &mut ops, stmt);
        }
        let func = ir::FuncImpl {
            decl: fdecl,
            vars: args,
            body: ops,
            builtin: false,
        };
        if let Some(id) = state.temp.fn_impl_to_id.get(decl) {
            state.ir.insert_with(*id, func);
            Some(*id)
        } else {
            let id = state.ir.insert(func);
            state.temp.fn_impl_to_id.insert(decl, id);
            Some(id)
        }
    } else {
        let err = ProgramError {
            error_message: "expected a function implementation",
            span: Some(decl.span),
            ..Default::default()
        };
        state.src.add_err(err);
        return None;
    }
}

fn visit_expr(
    state: &mut LowerModule<'_>,
    var_map: &mut HashMap<ir::SymbolID, ir::VarID>,
    ops: &mut Vec<ir::OPID>,
    expr: &ast::Expr,
    parent_name: Option<ir::SymbolID>,
) -> ir::VarID {
    match &expr.kind {
        ast::ExprKind::Var(v) => {
            let sym = ir::SymbolID::insert(&state.ir, v.name.get_str());
            let var_id = var_map.get(&sym).unwrap();
            return *var_id;
        }
        ast::ExprKind::IntLit(i) => {
            let id = state.ir.temporary_id();
            let var = ir::VarID::new_var(&state.ir, parent_name, None, Some(expr.span));
            state.ir.insert_with(
                id,
                ir::OP {
                    kind: ir::OPKind::Constant(ir::ConstKind::I32(*i)),
                    span: expr.span,
                    res: Some(var),
                },
            );
            ops.push(id);
            return var;
        }
        ast::ExprKind::StringLit(s) => {
            let id = state.ir.temporary_id();
            let var = ir::VarID::new_var(&state.ir, parent_name, None, Some(expr.span));
            state.ir.insert_with(
                id,
                ir::OP {
                    kind: ir::OPKind::Constant(ir::ConstKind::String(ir::SymbolID::insert(
                        &state.ir, s,
                    ))),
                    span: expr.span,
                    res: Some(var),
                },
            );
            ops.push(id);
            return var;
        }
        ast::ExprKind::GetAttr { exp, sym } => {
            let expr_id = visit_expr(state, var_map, ops, exp, parent_name);
            let var = ir::VarID::new_var(&state.ir, parent_name, None, Some(expr.span));
            let id = state.ir.temporary_id();
            state.ir.insert_with(
                id,
                ir::OP {
                    kind: ir::OPKind::GetAttr {
                        obj: expr_id,
                        attr: ir::SymbolID::insert(&state.ir, sym.get_str()),
                        idx: None,
                    },
                    span: expr.span,
                    res: Some(var),
                },
            );
            ops.push(id);
            return var;
        }
        ast::ExprKind::MethodCall { exp, sym, args } => {
            let obj = visit_expr(state, var_map, ops, exp, parent_name);
            let var_ids = args
                .iter()
                .map(|expr: &ast::Expr| visit_expr(state, var_map, ops, expr, parent_name))
                .collect();
            let var = ir::VarID::new_var(&state.ir, parent_name, None, Some(expr.span));

            let op = state.ir.insert(ir::OP {
                kind: ir::OPKind::MethodCall {
                    obj,
                    method: ir::SymbolID::insert(&state.ir, sym.get_str()),
                    args: var_ids,
                },
                res: Some(var),
                span: expr.span,
            });
            ops.push(op);
            var
        }
        ast::ExprKind::Call { path, args } => {
            let var_ids = args
                .iter()
                .map(|expr: &ast::Expr| visit_expr(state, var_map, ops, expr, parent_name))
                .collect();
            if is_builtin_fn(path) || state.path_map.get(path).unwrap().kind.is_fn_impl() {
                let func_id = state
                    .path_map
                    .get(path)
                    .map(|decl| {
                        *state
                            .temp
                            .fn_impl_to_id
                            .or_insert_with(decl, || state.ir.temporary_id())
                    })
                    .unwrap_or_else(|| {
                        assert!(is_builtin_fn(path));
                        get_builtin_fn(path, &state.ir)
                    });
                let func_ret = state.ir.get(func_id).decl.ret_ty;
                let var =
                    ir::VarID::new_var(&state.ir, parent_name, Some(func_ret), Some(expr.span));
                let id = state.ir.temporary_id();
                state.ir.insert_with(
                    id,
                    ir::OP {
                        kind: ir::OPKind::Call {
                            func: func_id,
                            args: var_ids,
                        },
                        span: expr.span,
                        res: Some(var),
                    },
                );
                ops.push(id);
                return var;
            }
            let decl = state.path_map.get(path).unwrap();
            if decl.kind.is_class_impl() {
                let class_id = state
                    .path_map
                    .get(path)
                    .map(|decl| {
                        *state
                            .temp
                            .class_impl_to_id
                            .or_insert_with(decl, || state.ir.temporary_id())
                    })
                    .unwrap();
                assert!(var_ids.len() == 1);
                let var = ir::VarID::new_var(&state.ir, parent_name, None, Some(expr.span));
                let op = state.ir.insert(ir::OP {
                    kind: ir::OPKind::ClsCtor {
                        cls: class_id,
                        arg: var_ids[0],
                    },
                    span: expr.span,
                    res: Some(var),
                });
                ops.push(op);
                return var;
            } else {
                state.src.add_err(ProgramError {
                    error_message: "expected a function declaration or class implementation",
                    span: Some(path.span),
                    ..Default::default()
                });
                return ir::VarID::new_var(&state.ir, parent_name, None, Some(expr.span));
            }
        }
        ast::ExprKind::Add { lhs, rhs } => {
            let lhs_id = visit_expr(state, var_map, ops, lhs, parent_name);
            let rhs_id = visit_expr(state, var_map, ops, rhs, parent_name);
            let var = ir::VarID::new_var(&state.ir, parent_name, None, Some(expr.span));
            let id = state.ir.temporary_id();
            state.ir.insert_with(
                id,
                ir::OP {
                    kind: ir::OPKind::Add {
                        lhs: lhs_id,
                        rhs: rhs_id,
                    },
                    span: expr.span,
                    res: Some(var),
                },
            );
            ops.push(id);
            return var;
        }
        ast::ExprKind::Struct { args } => {
            let mut field_ids = Vec::new();
            for (sym, expr) in args.iter() {
                let var_id = visit_expr(state, var_map, ops, expr, parent_name);
                field_ids.push((ir::SymbolID::insert(&state.ir, &sym.name.view()), var_id));
            }
            let var = ir::VarID::new_var(&state.ir, parent_name, None, Some(expr.span));
            let id = state.ir.temporary_id();
            state.ir.insert_with(
                id,
                ir::OP {
                    kind: ir::OPKind::Struct { fields: field_ids },
                    span: expr.span,
                    res: Some(var),
                },
            );
            ops.push(id);
            return var;
        }
    }
}

fn insert_stmts(
    state: &mut LowerModule<'_>,
    var_map: &mut HashMap<ir::SymbolID, ir::VarID>,
    ops: &mut Vec<ir::OPID>,
    stmt: &ast::Stmt,
) {
    let op_kind;
    match &stmt.kind {
        ast::StmtKind::ReturnStmt { expr } => {
            let var = visit_expr(state, var_map, ops, expr, None);
            op_kind = ir::OPKind::Return { value: var };
        }
        ast::StmtKind::ExprStmt { expr } => {
            let _var = visit_expr(state, var_map, ops, expr, None);
            return;
        }
        ast::StmtKind::LetStmt { var, expr } => {
            let var_sym = ir::SymbolID::insert(&state.ir, var.name.get_str());
            let expr_id = visit_expr(state, var_map, ops, expr, Some(var_sym));
            let var_id = ir::VarID::new_var(&state.ir, Some(var_sym), None, Some(stmt.span));
            let op = state.ir.insert(ir::OP {
                kind: ir::OPKind::Let { value: expr_id },
                span: stmt.span,
                res: Some(var_id),
            });
            ops.push(op);
            var_map.insert(var_sym, var_id);
            // op_kind = ir::OPKind::Assign {
            //     lhs: var_id,
            //     rhs: expr_id,
            // };
            return;
        }
        ast::StmtKind::AssignStmt { lhs, rhs } => {
            let lhs_id = visit_expr(state, var_map, ops, lhs, None);
            let rhs_id = visit_expr(state, var_map, ops, rhs, None);
            op_kind = ir::OPKind::Assign {
                lhs: lhs_id,
                rhs: rhs_id,
            };
        }
    }
    let op_id = state.ir.insert(ir::OP {
        kind: op_kind,
        span: stmt.span,
        res: None,
    });
    ops.push(op_id);
}

fn path_to_type<'a>(state: &mut LowerModule<'a>, path: &'a ast::Path) -> Option<ir::TypeID> {
    if is_builtin_type(path) {
        let ty = get_builtin_type(path);
        if let Err(e) = ty {
            state.src.add_err(e);
            return None;
        }
        Some(ir::TypeID::insert(&state.ir, ty.unwrap()))
    } else {
        let decl = state.path_map.get(path).unwrap();
        match &decl.kind {
            ast::DeclKind::TypeDecl { decl, .. } => construct_types(state, decl),
            _ => {
                state.src.add_err(ProgramError {
                    error_message: "expected a type declaration",
                    span: Some(path.span),
                    ..Default::default()
                });
                None
            }
        }
    }
}

fn visit_class_impl<'a>(state: &mut LowerModule<'a>, decl: &'a ast::Decl) -> Option<ir::ClassID> {
    if let ast::DeclKind::ClassImpl {
        name,
        for_it,
        sub_decls,
        repr_ty,
    } = &decl.kind
    {
        let repr_type = if let Some(repr) = repr_ty {
            Some(path_to_type(state, repr)?)
        } else {
            None
        };

        let mut methods = vec![];
        let mut types = vec![];
        for sub_decl in sub_decls {
            if sub_decl.kind.is_fn_impl() {
                visit_fn_impl(state, sub_decl).and_then(|id| {
                    methods.push(id);
                    Some(())
                });
            } else if sub_decl.kind.is_type_decl() {
                visit_type_decl(state, sub_decl).and_then(|id| {
                    types.push(id);
                    Some(())
                });
            } else {
                state.src.add_err(ProgramError {
                    error_message: "expected a function implementation, or type declaration",
                    span: Some(sub_decl.span),
                    ..Default::default()
                });
            }
        }
        let itf = for_it.as_ref().map(|x| {
            let decl = state.path_map.get(x).unwrap();
            *state
                .temp
                .itf_impl_to_id
                .or_insert_with(decl, || state.ir.temporary_id())
        });
        let cls = ir::ClassImpl {
            name: ir::SymbolID::insert(&state.ir, name.name.view()),
            for_itf: itf,
            methods,
            repr_type,
            types,
        };

        if let Some(id) = state.temp.class_impl_to_id.get(decl) {
            state.ir.insert_with(*id, cls);
            Some(*id)
        } else {
            let id = state.ir.insert(cls);
            state.temp.class_impl_to_id.insert(decl, id);
            Some(id)
        }
    } else {
        let err = ProgramError {
            error_message: "expected a class implementation",
            span: Some(decl.span),
            ..Default::default()
        };
        state.src.add_err(err);
        return None;
    }
}

fn visit_interface_impl<'a>(
    state: &mut LowerModule<'a>,
    decl: &'a ast::Decl,
) -> Option<ir::InterfaceID> {
    if let ast::DeclKind::InterfaceImpl {
        name,
        sub_decls: body,
    } = &decl.kind
    {
        let mut methods = vec![];
        for sub_decl in body {
            if let ast::DeclKind::FnDecl { name, args, ty } = &sub_decl.kind {
                let Some(fdecl) = get_fn_decl(state, sub_decl.span, name, args, ty) else {
                    continue;
                };
                methods.push(fdecl);
            } else {
                state.src.add_err(ProgramError {
                    error_message: "expected a function declaration",
                    span: Some(sub_decl.span),
                    ..Default::default()
                });
            }
        }
        let itf = ir::InterfaceImpl {
            name: ir::SymbolID::insert(&state.ir, name.name.view()),
            methods,
        };
        if let Some(id) = state.temp.itf_impl_to_id.get(decl) {
            state.ir.insert_with(*id, itf);
            Some(*id)
        } else {
            let id = state.ir.insert(itf);
            state.temp.itf_impl_to_id.insert(decl, id);
            Some(id)
        }
    } else {
        let err = ProgramError {
            error_message: "expected an interface implementation",
            span: Some(decl.span),
            ..Default::default()
        };
        state.src.add_err(err);
        None
    }
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
            "
        .into();
        let ast = parse(&src).unwrap();

        check_basic(&src, &ast).expect_err("expected error");
        let path_analysis = path_analysis(&src, &ast).unwrap();
        let _path_to_decl = path_to_decl(&src, &ast, &path_analysis).unwrap();
    }

    #[test]
    fn test_ast_to_ir() -> PhaseResult<()> {
        let src = "\
        type T = {a: i32, b: i32}
        type R = {a: T, b: T}
        class A {
            fn foo(a: T): i32 { return (a.a + 1); }
        }
        fn main() {
            A::foo({a: 1, b: 2});
        }
        "
        .into();
        let ast = parse(&src).unwrap();
        let _ir = ast_to_ir(src, ast)?;
        println!("{}", ir::print_basic(&_ir));
        Ok(())
    }

    #[test]
    fn test_call_builtin() {
        let src = "\
        fn main() {
            print(to_str(1));
        }
        "
        .into();
        let ast = parse(&src).unwrap();
        let _ir = ast_to_ir(src, ast).unwrap();
        println!("{}", ir::print_basic(&_ir));
    }

    #[test]
    fn test_type_cycle() {
        let src = [
            "\
        type A = {a: B}
        type B = {a: C, b: C}
        type C = {a: A}",
            "\
        type A = {a: A}",
            "\
        type A = {a: B}
        type B = {a: C}
        type C = {a: B}",
        ];
        for src in src {
            let src = src.into();
            let ast = parse(&src).unwrap();
            check_basic(&src, &ast).unwrap();
            let path_analysis = path_analysis(&src, &ast).unwrap();
            let path_to_decl = path_to_decl(&src, &ast, &path_analysis).unwrap();
            assert_no_type_cycle(&src, &path_to_decl, &ast).expect_err("expected type cycle");
        }
    }

    #[test]
    fn test_this() {
        let src = "\
        interface Foo {
            fn foo(this, a: i32): Self
        }
        class Bar {
            fn foo(this, a: This): This {
                return this;
            }
        }
        fn main() {
        }
        "
        .into();
        let ast = parse(&src);
        if let Err(e) = ast {
            println!("{}", e);
        } else {
            let ast = ast.unwrap();

            let _ir = ast_to_ir(src, ast);
            if let Err(e) = _ir {
                println!("{}", e);
            } else {
                println!("{}", ir::print_basic(&_ir.unwrap()));
            }
        }
    }

    #[test]
    fn test_methods() {
        let src = "\
        interface Foo {
            fn foo(this, a: i32): Self
        }
        type B = { a: i32 }
        class Bar for Foo(B) {
            fn foo(this, a: i32): Self {
                print(a);
                this.a = (a + 1);
                return this;
            }
        }
        fn main() {
            let b = Bar({a: 3});
            
        }
        "
        .into();
        let ast = parse(&src);
        if let Err(e) = ast {
            println!("{}", e);
        } else {
            let ast = ast.unwrap();

            let _ir = ast_to_ir(src, ast);
            if let Err(e) = _ir {
                println!("{}", e);
            } else {
                println!("{}", ir::print_basic(&_ir.unwrap()));
            }
        }
    }
}
