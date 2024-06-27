use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::query::*;

use crate::ir;

pub struct VarParent {
    pub var_parent: HashMap<ir::VarID, ir::OPID>,
}

impl VarParent {
    pub fn new(ir: &ir::IR) -> Self {
        let mut var_parent = HashMap::new();

        for id in ir.iter::<ir::OPID>() {
            if let Some(v) = ir.get(id).res {
                var_parent.insert(v, id);
            }
        }

        VarParent { var_parent }
    }

    pub fn parent(&self, var: ir::VarID) -> Option<ir::OPID> {
        self.var_parent.get(&var).copied()
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct ContainsCycle(ir::TypeID);

impl Query for ContainsCycle {
    type Result = bool;

    fn query(&self, q: &QueryAnalysis) -> Self::Result {
        let ir = q.ir();
        let ty = ir.get(self.0);
        match &ty.kind {
            ir::TypeKind::I32 => false,
            ir::TypeKind::Void => false,
            ir::TypeKind::String => false,
            ir::TypeKind::Struct { fields } => {
                for (_, field_ty) in fields {
                    match q.query(ContainsCycle(*field_ty)) {
                        Ok(true) => return true,
                        Ok(false) => {}
                        Err(QueryCycle) => return true,
                    }
                }
                false
            }
            ir::TypeKind::Rec { id } => {
                q.query(ContainsCycle(*id)).map_or(true, |x| x)
            }
        }
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct InlinedType(ir::TypeID);

impl Query for InlinedType {
    type Result = ir::TypeID;

    fn query(&self, q: &QueryAnalysis) -> Self::Result {
        let ir = q.ir();
        let ty = ir.get(self.0);
        match &ty.kind {
            ir::TypeKind::I32 => self.0,
            ir::TypeKind::Void => self.0,
            ir::TypeKind::String => self.0,
            ir::TypeKind::Struct { fields } => {
                let field_tys = fields
                    .iter()
                    .map(|(s, ty)| (*s, q.query(InlinedType(*ty)).unwrap()))
                    .collect();
                ir::TypeID::insert_type(ir, ir::TypeKind::Struct { fields: field_tys })
            }
            ir::TypeKind::Rec { id } => {
                q.query(InlinedType(*id)).unwrap()
            }
        }
    }
}

pub fn flatten_typedecl(ir: &mut ir::IR) {
    let query = QueryAnalysis::new(ir);
    let mut inlined = HashMap::new();
    for decl_id in ir.iter::<ir::TypeDeclID>() {
        let ty = ir.get(decl_id);
        let cycle = query.query(ContainsCycle(ty.decl)).map_or(true, |x| x);
        assert!(!cycle, "Cycle detected in type {:?}", ty.name.get_str(ir));
        let inlined_ty = query.query(InlinedType(ty.decl)).unwrap();
        inlined.insert(decl_id, inlined_ty);
    }

    for decl_id in ir.iter::<ir::TypeDeclID>()
    {
        let inlined_ty = inlined[&decl_id];
        let decl = ir.get_mut(decl_id);
        decl.decl = inlined_ty;
    }
}

struct BasicTypeInfer<'a> {
    var_types: HashMap<ir::VarID, ir::TypeID>,
    get_attr_idx: HashMap<ir::OPID, usize>,
    var_parent: &'a VarParent,
    ir: &'a ir::IR,
}

impl<'a> BasicTypeInfer<'a> {
    fn new(ir: &'a ir::IR, var_parent: &'a VarParent) -> Self {
        let var_types = HashMap::new();
        BasicTypeInfer {
            var_types,
            var_parent,
            get_attr_idx: HashMap::new(),
            ir,
        }
    }

    fn type_of(&mut self, id: ir::VarID) -> ir::TypeID {
        if let Some(ty) = self.var_types.get(&id) {
            return *ty;
        }
        let var = self.ir.get(id);
        if let Some(ty) = var.ty {
            self.var_types.insert(id, ty);
            return ty;
        }

        let parent = self
            .var_parent
            .parent(id)
            .expect(&format!("No parent for var {:?}", id.name_of(self.ir)));
        let op = self.ir.get(parent);
        let ty = match &op.kind {
            ir::OPKind::Add { .. } => {
                // TODO: should check types
                ir::TypeID::insert_type(&mut self.ir, ir::TypeKind::I32)
            }
            ir::OPKind::GetAttr { obj, attr, .. } => {
                let ty = self.type_of(*obj);
                let mut struct_ty = self.ir.get(ty);
                if let ir::TypeKind::Rec { id } = &struct_ty.kind {;
                    struct_ty = self.ir.get(*id);
                }
                if let ir::TypeKind::Struct { fields } = &struct_ty.kind {
                    let idx = fields.iter().position(|(name, _)| *name == *attr).unwrap();
                    let field_ty = fields[idx].1;
                    self.get_attr_idx.insert(parent, idx);
                    field_ty
                } else {
                    panic!("Expected struct type, got {:?}", struct_ty.kind);
                }
            }
            ir::OPKind::Call { func, .. } => {
                let decl = self.ir.get(*func);
                decl.decl.ret_ty
            }
            ir::OPKind::Struct { fields } => {
                let field_tys = fields
                    .iter()
                    .map(|(s, var)| (*s, self.type_of(*var)))
                    .collect();
                let struct_ty = ir::TypeID::insert_type(
                    &mut self.ir,
                    ir::TypeKind::Struct { fields: field_tys },
                );
                struct_ty
            }
            ir::OPKind::Constant { .. } => ir::TypeID::insert_type(&mut self.ir, ir::TypeKind::I32),
            _ => panic!("Unexpected op kind"),
        };

        self.var_types.insert(id, ty);

        ty
    }
}

fn rewrite_var_types(ir: &mut ir::IR) {
    let var_parent = VarParent::new(ir);
    let mut infer = BasicTypeInfer::new(ir, &var_parent);

    for id in ir.iter::<ir::VarID>() {
        infer.type_of(id);
    }

    let BasicTypeInfer {
        var_types,
        get_attr_idx,
        ..
    } = infer;
    let var_ids = ir.iter::<ir::VarID>();
    for id in var_ids {
        let ty = var_types[&id];
        let var = ir.get_mut(id);
        var.ty = Some(ty);
    }

    let op_ids = ir.iter::<ir::OPID>();
    for id in op_ids {
        let op = ir.get_mut(id);
        match &mut op.kind {
            ir::OPKind::GetAttr { idx, .. } => {
                *idx = Some(get_attr_idx[&id]);
            }
            _ => {}
        }
    }
}

pub fn transform_ir(ir: &mut ir::IR) {
    rewrite_var_types(ir);
}


#[cfg(test)]
mod test_type_infer {
    use super::*;

    fn generate_ir_from_str(s: &str) -> ir::IR {
        let src = s.into();
        let ast = crate::parse(&src).unwrap();
        let mut ir = crate::ast_to_ir(&src, &ast).unwrap();
        rewrite_var_types(&mut ir);
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
}

