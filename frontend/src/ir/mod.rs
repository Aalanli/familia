use std::collections::HashSet;
use std::{cell::Ref, collections::HashMap};

use anyhow::{Error, Result};

mod ir;
mod registry;

pub use ir::ast_to_ir;
pub use ir::{
    FuncID, FuncImpl, NodeID, OPKind, Symbol, SymbolID, Type, TypeID, TypeKind, Var, VarID, IR,
    OPID, dump_ir
};

pub fn typecheck(ir: &mut IR) -> Result<()> {
    let mut var_types = ir
        .vars()
        .filter(|(id, var)| var.ty.is_some())
        .map(|(id, var)| (*id, var.ty.unwrap()))
        .collect::<HashMap<_, _>>();

    for (_, func) in ir.functions() {
        let ret_ty = func.decl.ret_ty;
        for op_id in func.body.iter() {
            let op = ir.get_op(*op_id).clone();
            match &op.kind {
                OPKind::Add { lhs, rhs } => {
                    if var_types[lhs] != ir.i32_id() {
                        return Err(Error::msg("type mismatch, not i32"));
                    }
                    if var_types[rhs] != ir.i32_id() {
                        return Err(Error::msg("type mismatch, not i32"));
                    }
                    var_types.insert(op.var, ir.i32_id());
                }
                OPKind::Constant { value } => {
                    if ir.get_type(ret_ty).kind != TypeKind::I32 {
                        return Err(Error::msg("type mismatch"));
                    }
                    var_types.insert(op.var, ir.i32_id());
                }
                OPKind::Call { func, args } => {
                    let func = ir.get_function(*func);
                    for (arg, ty) in func.decl.args.iter().zip(args.iter()) {
                        if var_types[ty] != ir.get_type(arg.1).id {
                            return Err(Error::msg("type mismatch"));
                        }
                    }
                    var_types.insert(op.var, ret_ty);
                }
                OPKind::GetAttr { obj, attr, idx } => {
                    let obj_ty = var_types[obj];
                    let obj_ty = ir.get_type(obj_ty);
                    if let TypeKind::Struct { fields, .. } = &obj_ty.kind {
                        let field_ty = fields
                            .iter()
                            .find(|(sym, _)| sym == attr)
                            .ok_or(Error::msg("field not found"))?
                            .1;
                        var_types.insert(op.var, field_ty);
                    } else {
                        return Err(Error::msg("type mismatch"));
                    }
                }
                OPKind::Return { value } => {
                    let value_ty = var_types[value];
                    if value_ty != ret_ty {
                        return Err(Error::msg("type mismatch"));
                    }
                }
                OPKind::Struct { fields } => {
                    let struct_ty = TypeKind::Struct {
                        fields: fields
                            .iter()
                            .map(|(sym, ty)| (*sym, var_types[ty]))
                            .collect(),
                    };
                    let ty = ir.new_type(struct_ty);
                    if let Some(var_ty) = var_types.get(&op.var) {
                        if *var_ty != ty {
                            return Err(Error::msg("type mismatch"));
                        }
                    } else {
                        var_types.insert(op.var, ty);
                    }
                }
            }
        }
    }

    for (id, ty) in var_types.iter() {
        ir.get_var_mut(*id).ty = Some(*ty);
    }
    Ok(())
}

pub fn check_no_corecursive_types(ir: &IR) -> Result<()> {
    let mut visited = HashSet::new();
    let mut work_list = ir.types();
    while let Some(ty_id) = work_list.pop() {
        if visited.contains(&ty_id) {
            return Err(Error::msg("corecursive types"));
        }
        let ty = ir.get_type(ty_id);
        match &ty.kind {
            TypeKind::Struct { fields } => {
                visited.insert(ty_id);
                for (_, field_ty) in fields {
                    if let TypeKind::Struct { .. } = ir.get_type(*field_ty).kind {
                        work_list.push(*field_ty);
                    }
                }
            }
            _ => {}
        }
    }

    Ok(())
}
