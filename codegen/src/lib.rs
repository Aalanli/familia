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
use ir::IR;

struct IRState<'ir> {
    ir: &'ir IR,
    namer: ir::IRNamer
}

struct CodeGenState<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    var_ids: HashMap<ir::VarID, BasicValueEnum<'ctx>>,
    types: HashMap<ir::TypeID, BasicTypeEnum<'ctx>>,
}

fn generate_llvm_type<'ctx, 'ir>(code: &mut CodeGenState<'ctx>, ir: &IRState<'ir>, ty_id: ir::TypeID) -> BasicTypeEnum<'ctx> {
    let ty_decl = ir.ir.get_type(ty_id).unwrap();
    let res = match &ty_decl.kind {
        ir::TypeKind::Struct { fields } => {
            let struct_ty = code.context.struct_type(
                &fields.iter().map(|&ty| 
                    generate_llvm_type(code, ir, ty.1).into()).collect::<Vec<_>>(),
                false
            );
            struct_ty.into()
        }
        ir::TypeKind::Decl { decl } => {
            code.context.get_struct_type(&ir.namer.name_type(*decl)).unwrap().into()
        }
        ir::TypeKind::I32 => {
            code.context.i32_type().into()
        }
        ir::TypeKind::Void => {
            panic!("void type not allowed here");
        }
    };
    code.types.insert(ty_id, res);
    res
}

fn codegen_type_decls<'ctx, 'ir>(code: &mut CodeGenState<'ctx>, ir: &IRState<'ir>) {
    for (ty_id, ..) in ir.ir.iter_ids::<ir::TypeDeclID>() {
        code.context.opaque_struct_type(&ir.namer.name_type(ty_id));
    }

    for (ty_id, decl) in ir.ir.iter_ids::<ir::TypeDeclID>() {
        let llvm_ty = code.context.get_struct_type(&ir.namer.name_type(ty_id)).unwrap();
        let ty = ir.ir.get_type(decl.decl).unwrap();
        if let ir::TypeKind::Struct { fields } = &ty.kind {
            llvm_ty.set_body(
                &fields.iter().map(|&ty| 
                    generate_llvm_type(code, ir, ty.1).into()).collect::<Vec<_>>(),
                false
            );
        } else {
            panic!("only struct types allowed here");
        }
    }
}

fn codegen_fn<'ctx, 'ir>(code: &mut CodeGenState<'ctx>, ir: &IRState<'ir>) {
    for (func_id, func) in ir.ir.iter_ids::<ir::FuncID>() {
        let fn_type = generate_llvm_type(code, ir, func.decl.ret_ty).fn_type(
            &func.decl.args.iter().map(|&(_, ty)| generate_llvm_type(code, ir, ty).into()).collect::<Vec<_>>(),
            false
        );
        let llvm_func = code.module.add_function(&ir.namer.name_func(func_id), fn_type, None);
        let entry = code.context.append_basic_block(llvm_func, "entry");
        code.builder.position_at_end(entry);
        for (i, arg) in llvm_func.get_param_iter().enumerate() {
            arg.set_name(&ir.namer.name_var(func.vars[i]));
        }
        for op in &func.body {
            codegen_op(code, ir, *op);
        }
    }
}

fn codegen_op<'ctx, 'ir>(code: &mut CodeGenState<'ctx>, ir: &IRState<'ir>, op: ir::OPID) {
    let op = ir.ir.get(op).unwrap();
    match &op.kind {
        ir::OPKind::Constant { value } => {
            let ty = ir.ir.get(op.res.unwrap()).unwrap().ty.unwrap();
            let const_val = code.context.i32_type().const_int(*value as u64, false);
            code.var_ids.insert(op.res.unwrap(), const_val.into());
        }
        ir::OPKind::Add { lhs, rhs } => {
            let lhs_llvm = code.var_ids[lhs];
            let rhs_llvm = code.var_ids[rhs];
            let res = code.builder.build_int_add(lhs_llvm.into_int_value(), rhs_llvm.into_int_value(), "add").unwrap();
            code.var_ids.insert(op.res.unwrap(), res.into());
        }
        ir::OPKind::Call { func, args } => {
            let func = code.module.get_function(&ir.namer.name_func(*func)).unwrap();
            let args = args.iter().map(|&arg| code.var_ids[&arg].into()).collect::<Vec<_>>();
            let res = code.builder.build_call(func, &args, "call").unwrap();
            code.var_ids.insert(op.res.unwrap(), res.try_as_basic_value().left().unwrap());
        }
        ir::OPKind::GetAttr { obj, attr, idx } => {
            let obj = code.var_ids[obj];
            let idx = idx.unwrap();
            let struct_ty = obj.get_type().into_struct_type();
            let res = code.builder.build_extract_value(obj.into_struct_value(), idx as u32, "get_attr").unwrap();
            code.var_ids.insert(op.res.unwrap(), res.into());
        }
        ir::OPKind::Return { value } => {
            let value = code.var_ids[value];
            code.builder.build_return(Some(&value));
        }
        ir::OPKind::Struct { fields } => {
            let struct_ty = code.types[&ir.ir.get(op.res.unwrap()).unwrap().ty.unwrap()].into_struct_type();
            let mut values = vec![];
            for (i, field) in fields.iter().enumerate() {
                let value = code.var_ids[&field.1];
                values.push(value);
            }
            // let res = code.builder
            // code.var_ids.insert(op.res.unwrap(), res.into());
        }
        _ => {}
    }
}



/// TODO:
/// - disallow recursive and mutually recursive types
/// - remove void types from everywhere except function returns, or just make it a char
/// - only have stack allocated types

// pub fn generate_llvm(ir: &ir::IR) -> Result<String> {
//     let context = Context::create();
//     let module = context.create_module("main");
//     let builder = context.create_builder();

//     let mut codegen = CodeGenState {
//         context: &context,
//         module,
//         builder,
//         types: HashMap::new(),
//     };

//     // codegen.codegen(ir)?;
//     Ok(codegen.module.to_string())
// }

#[cfg(test)]
mod tests {
    use super::*;
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

    use inkwell::{
        passes::PassBuilderOptions,
        targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
    };
    
    fn run_passes_on(module: &Module) {
        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Aggressive,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();
    
        let passes: &[&str] = &[
            "instcombine",
            "reassociate",
            "gvn",
            "simplifycfg",
            // "basic-aa",
            "mem2reg",
        ];
    
        module
            .run_passes(
                passes.join(",").as_str(),
                &target_machine,
                PassBuilderOptions::create(),
            )
            .unwrap();
    
        // let t = target_machine
        //     .write_to_memory_buffer(module, inkwell::targets::FileType::Assembly)
        //     .unwrap();
        // println!("{}", String::from_utf8(t.as_slice().to_vec()).unwrap());
    }

    #[test]
    fn test_arg_ty() {
        let context = Context::create();
        let module = context.create_module("test");

        let ty = context.opaque_struct_type("footy");
        ty.set_body(
            &[context.f128_type().into(), context.i32_type().into()],
            false,
        );

        let fn_ty = ty.fn_type(&[ty.into()], false);
        let func = module.add_function("foo", fn_ty, None);
        let entry = context.append_basic_block(func, "entry");
        let builder = context.create_builder();
        builder.position_at_end(entry);
        let s = builder.build_alloca(ty, "s").unwrap();
        let arg = func.get_first_param().unwrap();
        builder.build_store(s, arg).unwrap();
        let a = context.i32_type().const_int(1, false);
        let arg_1 = builder.build_struct_gep(ty, s, 1, "gep").unwrap();
        builder.build_store(arg_1, a).unwrap();
        let y = builder.build_load(ty, s, "load").unwrap();
        builder.build_return(Some(&y)).unwrap();

        let fn_ty = ty.fn_type(&[context.f128_type().into(), context.i32_type().into()], false);
        let func = module.add_function("bar", fn_ty, None);
        let entry = context.append_basic_block(func, "entry");
        let builder = context.create_builder();
        builder.position_at_end(entry);
        let a = func.get_nth_param(0).unwrap();
        let b = func.get_nth_param(1).unwrap();
        let a = context.f128_type().const_float(1.0);
        let b = context.i32_type().const_int(1, false);

        let c = ty.const_named_struct(&[a.into(), b.into()]);
        builder.build_return(Some(&c)).unwrap();

        // run_passes_on(&module);
        let module_str = module.print_to_string().to_string();
        println!("{}", module_str);
    }
}
