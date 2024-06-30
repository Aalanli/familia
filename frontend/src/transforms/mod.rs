use std::{
    collections::{HashMap, HashSet},
    hash::Hash, rc::Rc,
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
struct ContainsCycleQuery(ir::TypeID);

impl Query for ContainsCycleQuery {
    type Result = bool;

    fn query(&self, q: &QueryAnalysis) -> Self::Result {
        let ir = q.ir();
        let ty = ir.get(self.0);
        match &ty.kind {
            ir::TypeKind::I32 => false,
            ir::TypeKind::Void => false,
            ir::TypeKind::String => false,
            ir::TypeKind::Ptr { .. } => false,
            ir::TypeKind::Struct { fields } => {
                for (_, field_ty) in fields {
                    match q.query(ContainsCycleQuery(*field_ty)) {
                        Ok(true) => return true,
                        Ok(false) => {}
                        Err(QueryCycle) => return true,
                    }
                }
                false
            }
            ir::TypeKind::Rec { id } => {
                q.query(ContainsCycleQuery(*id)).map_or(true, |x| x)
            }
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct InlinedTypeQuery(ir::TypeID);

impl Query for InlinedTypeQuery {
    type Result = ir::TypeID;

    fn query(&self, q: &QueryAnalysis) -> Self::Result {
        let ir = q.ir();
        let ty = ir.get(self.0);
        match &ty.kind {
            ir::TypeKind::I32 => self.0,
            ir::TypeKind::Void => self.0,
            ir::TypeKind::String => self.0,
            ir::TypeKind::Ptr { .. } => self.0,
            ir::TypeKind::Struct { fields } => {
                let field_tys = fields
                    .iter()
                    .map(|(s, ty)| (*s, q.query(InlinedTypeQuery(*ty)).unwrap()))
                    .collect();
                ir::TypeID::insert(ir, ir::TypeKind::Struct { fields: field_tys })
            }
            ir::TypeKind::Rec { id } => {
                q.query(InlinedTypeQuery(*id)).unwrap()
            }
        }
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
                    },
                };
                Ok(idx)
            },
            _ => panic!("Expected GetAttr, got {:?}", op.kind),
        }
    }
}


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
                let mut struct_ty = ir.get(ty);
                // assume no cycle
                while let ir::TypeKind::Rec { id } = &struct_ty.kind {
                    struct_ty = ir.get(*id);
                }

                if let ir::TypeKind::Struct { fields } = &struct_ty.kind {
                    let idx = fields.iter().position(|(name, _)| *name == *attr).unwrap();
                    let field_ty = fields[idx].1;
                    q.query_default(GetAttrIdxQuery(parent), || Ok(idx)).unwrap()?;
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
                let struct_ty = ir::TypeID::insert(
                    ir,
                    ir::TypeKind::Struct { fields: field_tys },
                );
                struct_ty
            }
            ir::OPKind::Constant(c) => {
                match c {
                    ir::ConstKind::I32(_) => prim.i32,
                    ir::ConstKind::String(_) => prim.string,
                    ir::ConstKind::IArray(_) => unimplemented!(),
                }
            },
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


fn rewrite_var_types<'s>(ir: &mut ir::IR, src: &'s ModSource) -> PhaseResult<'s, ()> {
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

pub fn transform_ir<'s>(ir: &mut ir::IR, src: &'s ModSource) -> PhaseResult<'s, ()> {
    rewrite_var_types(ir, src)?;
    insert_rts_fns(ir);
    lower_to_rts(ir);
    Ok(())
}

struct RTSFnProto {
    name: &'static str,
    arg_tys: Vec<ir::TypeKind>,
    ret_ty: ir::TypeKind,
}

lazy_static!(
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
            name: "__rts_new_string",
            arg_tys: vec![ir::TypeKind::I32, ir::TypeKind::Ptr],
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
);

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
            args: proto.arg_tys.iter().map(|ty| (ir::SymbolID::insert(ir, ""), ir::TypeID::insert(ir, ty.clone()))).collect(),
            ret_ty,
        };
        let id = ir.insert(ir::FuncImpl { 
            decl, vars: vec![], body: vec![], builtin: true,
        });
        fns.insert(proto.name, id);
    }
    ir.insert_global(RTSRegistry { fns: Rc::new(fns) });
}

pub struct TypeGCAttr {
    pub gc_mark_root: ir::FuncID,
    pub gc_pop_root: ir::FuncID,
}

fn add_gc_mark_root(ir: &ir::IR, rts_registry: &RTSRegistry, tyid: ir::TypeID, ops: &mut Vec<ir::OPID>, var: ir::VarID) {
    let ty = ir.get(tyid);
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

fn add_gc_ty_attrs(ir: &mut ir::IR) {

}

fn lower_to_rts(ir: &mut ir::IR) {
    let prim = ir.get_global::<PrimitiveRegistry>().unwrap().clone();
    let rts = ir.get_global::<RTSRegistry>().unwrap();
    let mut op_remap = HashMap::new();
    for fs in ir.iter::<ir::FuncID>() {
        let f = ir.get(fs);
        if f.builtin { continue; }
        for op_id in &f.body {
            let op = ir.get(*op_id);
            match &op.kind {
                ir::OPKind::Add { lhs, rhs } => {
                    let lty = ir.get(*lhs).ty.unwrap();
                    if lty == prim.string {
                        let op_kind = ir::OPKind::Call {
                            func: rts.fns["__rts_string_concat"],
                            args: vec![*lhs, *rhs]
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
                    if *func == prim.print {
                        assert!(args.len() == 1 && ir.get(args[0]).ty.unwrap() == prim.string);
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
                    } else if *func == prim.to_str {
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
    ir.delete(prim.print);
    ir.delete(prim.to_str);
}

#[cfg(test)]
mod test_type_infer {
    use super::*;

    fn generate_ir_from_str(s: &str) -> ir::IR {
        let src = s.into();
        let ast = crate::parse(&src).unwrap();
        let mut ir = crate::ast_to_ir(&src, &ast).unwrap();
        transform_ir(&mut ir, &src).unwrap();
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
            }"
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
            "
        );
        println!("{}", ir::print_basic(&_ir));

    }
}

