use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::BasicValueEnum;
use inkwell::OptimizationLevel;

use anyhow::{Error, Result};

use familia_frontend::ir;

struct CodeGen<'ctx, 'ir> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    ir: &'ir ir::IR,
    types: HashMap<ir::TypeID, BasicTypeEnum<'ctx>>,
}

/// TODO:
/// - disallow recursive and mutually recursive types
/// - remove void types from everywhere except function returns, or just make it a char
/// - only have stack allocated types

pub fn generate_llvm(ir: &ir::IR) -> Result<String> {
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let mut codegen = CodeGen {
        context: &context,
        module,
        builder,
        ir,
        types: HashMap::new(),
    };

    codegen.codegen(ir)?;
    Ok(codegen.module.to_string())
}

impl<'ctx, 'ir> CodeGen<'ctx, 'ir> {
    pub fn codegen(&mut self, ir: &ir::IR) -> Result<()> {
        for ty_id in ir.types() {
            if ir.get_decl_type_name(ty_id).is_some() {
                self.codegen_type(ty_id)?;
            }
        }

        for fn_id in ir.functions() {
            self.codegen_function(*fn_id.0);
        }

        Ok(())
    }

    fn codegen_function(&mut self, fn_id: ir::FuncID) {
        let func = self.ir.get_function(fn_id);
        let fn_ty;
        let param_ty = func
            .decl
            .args
            .iter()
            .map(|(_, ty_id)| self.codegen_type(*ty_id).unwrap().into())
            .collect::<Vec<_>>();
        if let ir::TypeKind::Void = self.ir.get_type(func.decl.ret_ty).kind {
            fn_ty = self.context.void_type().fn_type(&param_ty, false);
        } else {
            let ret_ty = self.codegen_type(func.decl.ret_ty).unwrap();
            fn_ty = ret_ty.fn_type(param_ty.as_slice(), false);
        }
        let func_name = &self.ir.get_symbol(func.decl.name).name;
        let llvm_func = self.module.add_function(func_name, fn_ty, None);
        let block = self.context.append_basic_block(llvm_func, "entry");
        self.builder.position_at_end(block);

        let mut arg_map = HashMap::new();
        for (llv, var) in llvm_func.get_param_iter().zip(func.vars.iter()) {
            arg_map.insert(*var, llv);
        }

        for op_id in &func.body {
            self.codegen_op(&mut arg_map, *op_id);
        }
    }

    fn codegen_op(
        &mut self,
        arg_map: &mut HashMap<ir::VarID, BasicValueEnum<'ctx>>,
        op_id: ir::OPID,
    ) {
        let op = self.ir.get_op(op_id);
        match &op.kind {
            ir::OPKind::Add { lhs, rhs } => {
                let lhs = arg_map[lhs];
                let rhs = arg_map[rhs];
                let sum = self
                    .builder
                    .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "sum")
                    .unwrap();
                arg_map.insert(op.var, sum.into());
            }
            ir::OPKind::Constant { value } => {
                let value = self.context.i32_type().const_int(*value as u64, false);
                arg_map.insert(op.var, value.into());
            }
            ir::OPKind::Return { value } => {
                let value = arg_map[value];
                self.builder.build_return(Some(&value)).unwrap();
            }
            _ => {}
        }
    }

    fn codegen_type(&mut self, ty_id: ir::TypeID) -> Result<BasicTypeEnum<'ctx>> {
        if let Some(ty) = self.types.get(&ty_id) {
            return Ok(ty.clone());
        }
        let ty: BasicTypeEnum<'_> = match &self.ir.get_type(ty_id).kind {
            ir::TypeKind::Void => {
                panic!("void type not supported")
            }
            ir::TypeKind::I32 => self.context.i32_type().into(),
            ir::TypeKind::Struct { fields } => {
                let mut field_types = Vec::new();
                for field in fields {
                    let field_ty = self.codegen_type(field.1)?;
                    field_types.push(field_ty);
                }

                if let Some(name) = self.ir.get_decl_type_name(ty_id) {
                    let name = &self.ir.get_symbol(name).name;
                    let struct_ty = self.context.opaque_struct_type(name);
                    struct_ty.set_body(field_types.as_slice(), false);
                    struct_ty.into()
                } else {
                    self.context
                        .struct_type(field_types.as_slice(), false)
                        .into()
                }
            }
        };
        self.types.insert(ty_id, ty.clone());
        Ok(ty)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use familia_frontend::{ast_to_ir, ir::dump_ir, parse};
    #[test]
    fn test_struct() {
        let context = Context::create();
        let module = context.create_module("test");

        let ty = context.opaque_struct_type("footy");
        ty.set_body(
            &[context.f128_type().into(), context.i32_type().into()],
            false,
        );
        assert!(!ty.is_opaque());

        let builder1 = context.create_builder();
        let builder2 = context.create_builder();
        let struct_ty = context.struct_type(
            &[context.i32_type().into(), context.f64_type().into()],
            false,
        );
        let fn_type = context
            .void_type()
            .fn_type(&[struct_ty.into(), context.i8_type().into()], false);
        let fn_type2 = ty.fn_type(
            &[context.i32_type().into(), context.i32_type().into()],
            false,
        );

        let func1 = module.add_function("foo", fn_type, None);
        let func2 = module.add_function("bar", fn_type2, None);

        let entry1 = context.append_basic_block(func1, "entry");
        let entry2 = context.append_basic_block(func2, "entry");
        builder1.position_at_end(entry1);
        builder2.position_at_end(entry2);

        builder1.build_return(None).unwrap();
        builder2.build_return(None).unwrap();

        let module_str = module.print_to_string().to_string();
        // println!("{}", module_str);
    }

    #[test]
    fn test_gen() {
        let ast = parse("fn foo(): i32 { return (1 + 2); }").unwrap();
        let ir = ast_to_ir(&ast).unwrap();
        println!("{}", dump_ir(&ir));
        let llvm = super::generate_llvm(&ir).unwrap();
        println!("{}", llvm);
    }
}
