mod ast_to_ir;
pub mod utils;

pub use ast_to_ir::ast_to_ir;

#[derive(Clone)]
#[allow(non_snake_case)]
pub struct PrimitiveRegistry {
    pub i32: ir::TypeID,
    pub string: ir::TypeID,
    pub void: ir::TypeID,
    pub This: ir::TypeID,
    pub Self_: ir::TypeID,
    pub fns: HashMap<String, ir::FuncID>,
}

use std::{
    collections::{HashMap},
    hash::Hash,
    rc::Rc,
};

use either::Either;
use lazy_static::lazy_static;

use crate::prelude::*;
use crate::query::*;

use crate::ir;

pub struct VarParent {
    pub var_parent: HashMap<ir::VarID, Either<ir::OPID, ir::FuncID>>,
}

impl VarParent {
    pub fn new(ir: &ir::IR) -> Self {
        let mut var_parent = HashMap::new();

        for id in ir.iter::<ir::OPID>() {
            if let Some(v) = ir.get(id).res {
                var_parent.insert(v, Either::Left(id));
            }
        }
        for id in ir.iter::<ir::FuncID>() {
            for v in ir.get(id).vars.iter() {
                var_parent.insert(*v, Either::Right(id));
            }
        }

        VarParent { var_parent }
    }

    pub fn parent(&self, var: ir::VarID) -> Option<Either<ir::OPID, ir::FuncID>> {
        self.var_parent.get(&var).copied()
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct VarParentAnalysis;
impl Query for VarParentAnalysis {
    type Result = Rc<VarParent>;

    fn query(&self, q: &QueryAnalysis) -> Self::Result {
        Rc::new(VarParent::new(q.ir()))
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct GetAttrIdxQuery(pub ir::OPID);

#[derive(Clone, Hash, PartialEq, Eq)]
struct VarTypeQuery(pub ir::VarID);

impl Query for GetAttrIdxQuery {
    type Result = PResult<usize>;

    fn query(&self, q: &QueryAnalysis) -> Self::Result {
        let ir = q.ir();
        let op = ir.get(self.0);
        match &op.kind {
            ir::OPKind::GetAttr { obj, attr, .. } => {
                let ty = q.query(VarTypeQuery(*obj)).unwrap()?;
                let struct_ty = ir.get(ty);
                let idx = match &struct_ty.kind {
                    ir::TypeKind::Struct { fields } => {
                        fields.iter().position(|(name, _)| *name == *attr).unwrap()
                    }
                    _ => {
                        return Err(ProgramError {
                            span: Some(op.span),
                            error_message: "Invalid get attribute on non-struct type",
                            ..Default::default()
                        })
                    }
                };
                Ok(idx)
            }
            _ => panic!("Expected GetAttr, got {:?}", op.kind),
        }
    }
}

fn get_fn_type(ir: &ir::IR, fdecl: &ir::FuncDecl) -> ir::TypeID {
    let args = fdecl.args.iter().map(|(_, ty)| *ty).collect::<Vec<_>>();
    let ret_ty = fdecl.ret_ty;
    ir::TypeID::insert(ir, ir::TypeKind::Fn(args, ret_ty))
}

fn has_itf_repr_ty(ir: &ir::IR, itf: ir::InterfaceID) -> bool {
    let itf = ir.get(itf);
    let this = ir::TypeID::this(ir);
    for fdecl in itf.methods.iter() {
        if fdecl.ret_ty == this || fdecl.args[1..].iter().any(|(_, ty)| *ty == this) {
            return false;
        }
        if fdecl.args.get(0).map(|(_, ty)| *ty) != Some(this) {
            return false;
        }
    }
    return true;
}

fn has_cls_repr_ty(ir: &ir::IR, cls: ir::ClassID) -> bool {
    let cls = ir.get(cls);
    cls.for_itf
        .map(|itf| has_itf_repr_ty(ir, itf))
        .unwrap_or(false)
}

// #[derive(Clone, Hash, PartialEq, Eq)]
// pub struct NormalizeItfMethodTypes(pub ir::InterfaceID, pub ir::ClassID);

// impl Query for NormalizeItfMethodTypes {
//     type Result = PResult<ir::TypeID>;
//     fn query(&self, q: &QueryAnalysis) -> Self::Result {
//         let (itf, cls) = (self.0, self.1);

//         if !has_cls_repr_ty(q.ir(), cls) {
//             return Err(ProgramError {
//                 error_message: "No representation type for the interface",
//                 ..Default::default()
//             });
//         }

//         let itf = q.ir().get(itf);

//     }
// }

impl Query for VarTypeQuery {
    type Result = PResult<ir::TypeID>;

    fn query(&self, q: &QueryAnalysis) -> Self::Result {
        let ir = q.ir();
        let prim = ir.get_global::<PrimitiveRegistry>().unwrap();
        let parent = q.query(VarParentAnalysis).unwrap().parent(self.0).unwrap();
        let Either::Left(parent) = parent else {
            return Ok(ir.get(self.0).ty.unwrap());
        };

        let op = ir.get(parent);
        let ty = match &op.kind {
            ir::OPKind::Add { lhs, rhs } => {
                let lty = q.query(VarTypeQuery(*lhs)).unwrap()?;
                let rty = q.query(VarTypeQuery(*rhs)).unwrap()?;
                if lty != rty {
                    return Err(ProgramError {
                        span: Some(op.span),
                        error_message: "Type mismatch",
                        ..Default::default()
                    });
                }
                if lty == prim.string {
                    prim.string
                } else if lty == prim.i32 {
                    prim.i32
                } else {
                    return Err(ProgramError {
                        span: Some(op.span),
                        error_message: "Invalid type for add",
                        ..Default::default()
                    });
                }
            }
            ir::OPKind::GetAttr { obj, attr, .. } => {
                let ty = q.query(VarTypeQuery(*obj)).unwrap()?;
                let struct_ty = ir.get(ty);
                // assume no cycle

                if let ir::TypeKind::Struct { fields } = &struct_ty.kind {
                    let idx = fields.iter().position(|(name, _)| *name == *attr).unwrap();
                    let field_ty = fields[idx].1;
                    q.query_default(GetAttrIdxQuery(parent), || Ok(idx))
                        .unwrap()?;
                    field_ty
                } else {
                    return Err(ProgramError {
                        span: Some(op.span),
                        error_message: "Expected struct type for get attribute",
                        ..Default::default()
                    });
                }
            }
            ir::OPKind::Call { func, .. } => {
                let decl = ir.get(*func);
                decl.decl.ret_ty
            }
            ir::OPKind::Struct { fields } => {
                let field_tys = fields
                    .iter()
                    .map(|(s, var)| Ok((*s, q.query(VarTypeQuery(*var)).unwrap()?)))
                    .collect::<PResult<_>>()?;
                let struct_ty = ir::TypeID::insert(ir, ir::TypeKind::Struct { fields: field_tys });
                struct_ty
            }
            ir::OPKind::Constant(c) => match c {
                ir::ConstKind::I32(_) => prim.i32,
                ir::ConstKind::String(_) => prim.string,
                ir::ConstKind::IArray(_) => unimplemented!(),
            },
            ir::OPKind::Let { value } => q.query(VarTypeQuery(*value)).unwrap()?,
            &ir::OPKind::ClsCtor { cls, arg: _ } => {
                let impl_cls = ir.get(cls);
                if impl_cls.for_itf.is_none() {
                    return Err(ProgramError {
                        span: Some(op.span),
                        error_message: "No representation type, missing \"for\" clause",
                        ..Default::default()
                    });
                }
                if !has_cls_repr_ty(ir, cls) {
                    return Err(ProgramError {
                        span: Some(op.span),
                        error_message: "No representation type for the interface",
                        ..Default::default()
                    });
                }

                let itf = impl_cls.for_itf.unwrap();
                ir::TypeID::insert(ir, ir::TypeKind::Itf(itf))
            }
            ir::OPKind::MethodCall { obj, method, args } => {
                let itf_ty_id = q.query(VarTypeQuery(*obj)).unwrap()?;
                let self_ty_id = ir::TypeID::self_(ir);
                let obj_ty = ir.get(itf_ty_id);
                // there shouldn't exist any "This" types

                match &obj_ty.kind {
                    ir::TypeKind::Itf(itf) => {
                        let itf = ir.get(*itf);
                        let method = itf.methods.iter().find(|f| f.name == *method);
                        if method.is_none() {
                            return Err(ProgramError {
                                span: Some(op.span),
                                error_message: "Invalid method call",
                                ..Default::default()
                            });
                        }
                        let method = method.unwrap();
                        if method.args.len() - 1 != args.len()
                            || !method.args[1..].iter().zip(args.iter()).all(|(a, b)| {
                                let ty = q.query(VarTypeQuery(*b)).unwrap().unwrap();
                                // make sure to normalize all Self to the representation type
                                if a.1 == self_ty_id {
                                    ty == itf_ty_id
                                } else {
                                    ty == a.1
                                }
                            })
                        {
                            return Err(ProgramError {
                                span: Some(op.span),
                                error_message: "Type mismatch",
                                ..Default::default()
                            });
                        }

                        if method.ret_ty == self_ty_id {
                            return Ok(itf_ty_id);
                        }
                        return Ok(method.ret_ty);
                    }
                    _ => {
                        return Err(ProgramError {
                            span: Some(op.span),
                            error_message: "Expected interface type",
                            ..Default::default()
                        });
                    }
                }
            }
            _ => panic!("Unexpected op kind"),
        };
        if let Some(var_ty) = ir.get(self.0).ty {
            if var_ty != ty {
                return Err(ProgramError {
                    span: Some(op.span),
                    error_message: "Type mismatch",
                    ..Default::default()
                });
            }
        }
        Ok(ty)
    }
}

fn subsititute_cls_repr_type(ir: &mut ir::IR) -> PhaseResult<()> {
    let this = ir::TypeID::this(ir);
    let self_ = ir::TypeID::self_(ir);
    let src = ir.get_global::<ir::ModuleID>().unwrap();
    let src = ir.get(*src).src.clone().unwrap();
    
    let mut need_remap = vec![];
    for id in ir.iter::<ir::ClassID>() {
        let cls = ir.get(id);
        let new_this_ty = cls.repr_type.unwrap_or_else(|| ir::TypeID::void(ir));

        let itf_ty = cls
            .for_itf
            .map(|itf| ir::TypeID::insert(ir, ir::TypeKind::Itf(itf)));

        let has_repr = has_cls_repr_ty(ir, id);
        let remap_ty = |ty: &mut ir::TypeID, span| {
            if *ty == this {
                *ty = new_this_ty;
            } else if *ty == self_ {
                if has_repr {
                    *ty = itf_ty.unwrap();
                } else {
                    src.add_err(ProgramError {
                        span,
                        error_message: "Self is not allowed, no representation type",
                        ..Default::default()
                    });
                }
            }
        };

        for fid in cls.methods.clone() {
            let f = ir.get_mut(fid);
            for t in f.decl.args.iter_mut() {
                remap_ty(&mut t.1, Some(f.decl.span));
            }
            remap_ty(&mut f.decl.ret_ty, Some(f.decl.span));
            let f = ir.get(fid);
            for v in f.vars.iter() {
                need_remap.push(*v);
            }
            for op in f.body.iter() {
                for v in ir.get(*op).res() {
                    need_remap.push(v);
                }
            }
        }

        for v in need_remap.iter() {
            let var = ir.get_mut(*v);
            let Some(ty) = var.ty.as_mut() else { continue };
            remap_ty(ty, var.span);
        }
    }
    src.commit_error(())
}

fn rewrite_var_types<'s>(ir: &mut ir::IR, src: &'s ModSource) -> PhaseResult<()> {
    let query = QueryAnalysis::new(ir);

    let var_ids = ir.iter::<ir::VarID>().collect::<Vec<_>>();
    for id in var_ids.iter() {
        let ty = query.query(VarTypeQuery(*id)).unwrap();
        if let Err(e) = ty {
            src.add_err(e);
        }
    }
    src.commit_error(())?;

    let query = query.finish();

    let op_ids = ir.iter::<ir::OPID>();
    for id in op_ids {
        let op = ir.get_mut(id);
        match &mut op.kind {
            ir::OPKind::GetAttr { idx, .. } => {
                *idx = Some(query.get(&GetAttrIdxQuery(id)).unwrap().unwrap());
            }
            _ => {}
        }
    }

    for var_ids in var_ids {
        let ty = query.get(&VarTypeQuery(var_ids)).unwrap().unwrap();
        ir.get_mut(var_ids).ty = Some(ty);
    }

    Ok(())
}

pub fn transform_ir<'s>(ir: &mut ir::IR) -> PhaseResult<()> {
    let module = ir.get_global::<ir::ModuleID>().unwrap();
    let src = ir.get(*module).src.clone().unwrap();
    subsititute_cls_repr_type(ir)?;
    rewrite_var_types(ir, &src)?;
    insert_rts_fns(ir);
    lower_to_rts(ir)?;
    Ok(())
}

struct RTSFnProto {
    name: &'static str,
    arg_tys: Vec<ir::TypeKind>,
    ret_ty: ir::TypeKind,
}

lazy_static! {
    static ref RTS_PRIMITIVES: Vec<RTSFnProto> = vec![
        RTSFnProto {
            name: "__rts_gc_init",
            arg_tys: vec![],
            ret_ty: ir::TypeKind::Void,
        },
        RTSFnProto {
            name: "__rts_gc_destroy",
            arg_tys: vec![],
            ret_ty: ir::TypeKind::Void,
        },
        RTSFnProto {
            name: "__rts_gc_alloc",
            arg_tys: vec![ir::TypeKind::Ptr(None), ir::TypeKind::I32],
            ret_ty: ir::TypeKind::Ptr(None),
        },
        RTSFnProto {
            name: "__rts_new_string",
            arg_tys: vec![ir::TypeKind::I32, ir::TypeKind::Ptr(None)],
            ret_ty: ir::TypeKind::String,
        },
        RTSFnProto {
            name: "__rts_string_length",
            arg_tys: vec![ir::TypeKind::String],
            ret_ty: ir::TypeKind::I32,
        },
        RTSFnProto {
            name: "__rts_string_data",
            arg_tys: vec![ir::TypeKind::String],
            ret_ty: ir::TypeKind::I32,
        },
        RTSFnProto {
            name: "__rts_string_print",
            arg_tys: vec![ir::TypeKind::String],
            ret_ty: ir::TypeKind::Void,
        },
        RTSFnProto {
            name: "__rts_string_concat",
            arg_tys: vec![ir::TypeKind::String, ir::TypeKind::String],
            ret_ty: ir::TypeKind::String,
        },
        RTSFnProto {
            name: "__rts_int_to_string",
            arg_tys: vec![ir::TypeKind::I32],
            ret_ty: ir::TypeKind::String,
        },
    ];
}

#[derive(Clone)]
pub struct RTSRegistry {
    pub fns: Rc<HashMap<&'static str, ir::FuncID>>,
}

fn insert_rts_fns(ir: &mut ir::IR) {
    let mut fns = HashMap::new();
    for proto in RTS_PRIMITIVES.iter() {
        let ret_ty = ir::TypeID::insert(ir, proto.ret_ty.clone());
        let decl = ir::FuncDecl {
            name: ir::SymbolID::insert(ir, proto.name),
            span: Span::default(),
            args: proto
                .arg_tys
                .iter()
                .map(|ty| {
                    (
                        ir::SymbolID::insert(ir, ""),
                        ir::TypeID::insert(ir, ty.clone()),
                    )
                })
                .collect(),
            ret_ty,
        };
        let id = ir.insert(ir::FuncImpl {
            decl,
            vars: vec![],
            body: vec![],
            builtin: true,
        });
        fns.insert(proto.name, id);
    }
    ir.insert_global(RTSRegistry { fns: Rc::new(fns) });
}

pub struct TypeGCAttr {
    pub gc_mark_root: ir::FuncID,
    pub gc_pop_root: ir::FuncID,
}

fn add_gc_mark_root(
    ir: &ir::IR,
    _rts_registry: &RTSRegistry,
    tyid: ir::TypeID,
    _ops: &mut Vec<ir::OPID>,
    _var: ir::VarID,
) {
    let _ty = ir.get(tyid);
    // match &ty.kind {
    //     ir::TypeKind::I32 => {}
    //     ir::TypeKind::Void => {}
    //     ir::TypeKind::String => {
    //         ops.push(ir::OPID::insert(ir, ir::OPKind::Call {
    //             func: rts_registry.fns["__rts_gc_mark_root"],
    //             args: vec![var],
    //             ret
    //         }));
    //     }
    // }
}

fn add_gc_ty_attrs(_ir: &mut ir::IR) {}

fn lower_to_rts(ir: &mut ir::IR) -> PhaseResult<()> {
    let mut prim = ir.get_global::<PrimitiveRegistry>().unwrap().clone();
    let rts = ir.get_global::<RTSRegistry>().unwrap().clone();
    let src = *ir.get_global::<ir::ModuleID>().unwrap();
    { // insert gc init and destroy
        let main_fn = ir.get(src).main.unwrap();
        let fn_span = ir.get(main_fn).decl.span;
        let init = ir.insert(ir::OP {
            kind: ir::OPKind::Call {
                func: rts.fns["__rts_gc_init"],
                args: vec![],
            },
            span: fn_span,
            res: Some(ir.insert(ir::Var {
                ty: Some(prim.void),
                span: None,
                name: ir::SymbolID::insert(ir, ""),
            }))
        });
        let destroy = ir.insert(ir::OP {
            kind: ir::OPKind::Call {
                func: rts.fns["__rts_gc_destroy"],
                args: vec![],
            },
            span: fn_span,
            res: Some(ir.insert(ir::Var {
                ty: Some(prim.void),
                span: None,
                name: ir::SymbolID::insert(ir, ""),
            })),
        });
        let f = ir.get_mut(main_fn);
        f.body.insert(0, init);
        f.body.push(destroy);
    }


    let src = ir.get(src).src.clone().unwrap();
    let mut op_remap = HashMap::new();
    for fs in ir.iter::<ir::FuncID>() {
        let f = ir.get(fs);
        if f.builtin {
            continue;
        }
        for op_id in &f.body {
            let op = ir.get(*op_id);
            match &op.kind {
                ir::OPKind::Add { lhs, rhs } => {
                    let lty = ir.get(*lhs).ty.unwrap();
                    if lty == prim.string {
                        let op_kind = ir::OPKind::Call {
                            func: rts.fns["__rts_string_concat"],
                            args: vec![*lhs, *rhs],
                        };
                        let op = ir::OP {
                            kind: op_kind,
                            span: op.span,
                            res: op.res,
                        };
                        op_remap.insert(*op_id, op);
                    }
                }
                ir::OPKind::Call { func, args } => {
                    if *func == prim.fns["print"] {
                        if !(args.len() == 1 && ir.get(args[0]).ty.unwrap() == prim.string) {
                            src.add_err(ProgramError {
                                span: Some(op.span),
                                error_message: "Invalid print",
                                ..Default::default()
                            });
                            continue;
                        }
                        let op_kind = ir::OPKind::Call {
                            func: rts.fns["__rts_string_print"],
                            args: args.clone(),
                        };
                        let op = ir::OP {
                            kind: op_kind,
                            span: op.span,
                            res: op.res,
                        };
                        op_remap.insert(*op_id, op);
                    } else if *func == prim.fns["to_str"] {
                        assert!(args.len() == 1 && ir.get(args[0]).ty.unwrap() == prim.i32);
                        let op_kind = ir::OPKind::Call {
                            func: rts.fns["__rts_int_to_string"],
                            args: args.clone(),
                        };
                        let op = ir::OP {
                            kind: op_kind,
                            span: op.span,
                            res: op.res,
                        };
                        op_remap.insert(*op_id, op);
                    }
                }
                _ => {}
            }
        }
    }
    for op_id in ir.iter::<ir::OPID>() {
        if let Some(new_op) = op_remap.remove(&op_id) {
            let old_op = ir.get_mut(op_id);
            *old_op = new_op;
        }
    }
    ir.delete(prim.fns["print"]);
    ir.delete(prim.fns["to_str"]);
    prim.fns.remove("print");
    prim.fns.remove("to_str");
    *ir.get_global_mut::<PrimitiveRegistry>().unwrap() = prim;

    src.commit_error(())
}

#[cfg(test)]
mod test_type_infer {
    use super::*;

    fn generate_ir_from_str(s: &str) -> ir::IR {
        let src = s.into();
        let ast = crate::parse(&src).unwrap();
        let mut ir = crate::ast_to_ir(src, ast).unwrap();
        transform_ir(&mut ir).unwrap();
        ir
    }

    #[test]
    fn test1() {
        generate_ir_from_str(
            "\
            type T = {a: i32, b: i32}
            fn foo(a: T, b: i32): i32 {
                return (a.a + b);
            }
            fn main() {
                foo({a: 1, b: 2}, 3);
            }",
        );
    }

    #[test]
    fn test2() {
        let _ir = generate_ir_from_str(
            "\
            class S {
                type F = {a: i32, b: T}
                fn foo(a: S::F, b: i32): i32 {
                    return (a.a + b);
                }
            }

            fn bar(a: i32, b: i32): i32 {
                let c = (a + 1);
                let d = (b + 1);
                return (c + d);
            }

            type T = {a: i32, b: i32}
            fn main() {
                S::foo({a: 1, b: {a: 2, b: 3}}, 4);
            }",
        );
        println!("{}", ir::print_basic(&_ir));
    }

    #[test]
    fn test_let() {
        let _ir = generate_ir_from_str(
            "\
            fn bar(a: i32, b: i32): i32 {
                let c = (a + 1);
                let d = (b + 1);
                return (c + d);
            }
            fn main() {}
        ",
        );
        println!("{}", ir::print_basic(&_ir));
    }

    #[test]
    fn test_str() {
        let _ir = generate_ir_from_str(
            "\
            fn foo(a: String): String {
                let b = to_str(1);
                let c = (a + \"1\");
                print(c);
                return (b + c);
            }
            fn main() {}
            ",
        );
        println!("{}", ir::print_basic(&_ir));
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
                print(to_str(a));
                this.a = (a + 1);
                return this;
            }
        }
        interface Baz {
            fn baz(this, a: Self): Self
        }

        class Qux for Baz(B) {
            fn baz(this, a: Self): Self {
                this.a = (this.a + 1);
                return a.baz(Qux(this));
            }
        }

        fn main() {
            let b = Bar({a: 3});
            b.foo(1);
        }
        ";
        let _ir = generate_ir_from_str(&src);
        println!("{}", ir::print_basic(&_ir));
    }
}
