use std::any::Any;
use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, PointerValue};
use inkwell::OptimizationLevel;

use anyhow::{Error, Result};

use familia_frontend as frontend;
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
    var_ids: HashMap<ir::VarID, PointerValue<'ctx>>,
    types: HashMap<ir::TypeID, BasicTypeEnum<'ctx>>,
}

impl<'ctx> CodeGenState<'ctx> {
    fn new_stack_var<'ir>(&mut self, ir: &IRState<'ir>, var: ir::VarID) -> PointerValue<'ctx> {
        let var_ty = var.type_of(&ir.ir);
        let ty = self.get_type(ir, var_ty);
        let alloca = self.builder.build_alloca(ty, "").unwrap();
        self.var_ids.insert(var, alloca);
        alloca
    }

    fn get_type<'ir>(&mut self, ir: &IRState<'ir>, ty_id: ir::TypeID) -> BasicTypeEnum<'ctx> {
        if !self.types.contains_key(&ty_id) {
            let llvm_type = generate_llvm_type(self, ir, ty_id);
            self.types.insert(ty_id, llvm_type);
        }
        self.types[&ty_id]
    }

    fn get_var_type<'ir>(&mut self, ir: &IRState<'ir>, var: ir::VarID) -> BasicTypeEnum<'ctx> {
        self.get_type(ir, var.type_of(&ir.ir))
    }
    
    fn load_var<'ir>(&mut self, ir: &IRState<'ir>, var: ir::VarID) -> BasicValueEnum<'ctx> {
        let ptr = self.var_ids[&var];
        let pointee_ty = self.get_var_type(ir, var);
        self.builder.build_load(pointee_ty, ptr, "").unwrap().into()
    }
}

fn generate_llvm_type<'ctx, 'ir>(code: &mut CodeGenState<'ctx>, ir: &IRState<'ir>, ty_id: ir::TypeID) -> BasicTypeEnum<'ctx> {
    let ty_decl = ir.ir.get_unique(ty_id).unwrap();
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
        let ty = ir.ir.get_unique(decl.decl).unwrap();
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
        let fn_type;
        let arg_types = func.decl.args.iter().map(|&(_, ty)| code.get_type(ir, ty).into()).collect::<Vec<_>>();
        if func.decl.ret_ty == ir::TypeID::insert_type(&ir.ir, ir::TypeKind::Void) {
            fn_type = code.context.void_type().fn_type(
                &arg_types,
                false
            );
        } else {
            fn_type = code.get_type(ir, func.decl.ret_ty).fn_type(
                &arg_types,
                false
            );
        }
        code.module.add_function(&ir.namer.name_func(func_id), fn_type, None);
    }

    for (func_id, func) in ir.ir.iter_ids::<ir::FuncID>() {
        let llvm_func = code.module.get_function(ir.namer.name_func(func_id)).unwrap();
        let entry = code.context.append_basic_block(llvm_func, "entry");
        code.builder.position_at_end(entry);
        for (i, arg) in llvm_func.get_param_iter().enumerate() {
            arg.set_name(&ir.namer.name_var(func.vars[i]));
            let sarg = code.new_stack_var(ir, func.vars[i]);
            code.builder.build_store(sarg, arg).unwrap();
        }
        for op in &func.body {
            codegen_op(code, ir, *op);
        }
    }
}

fn codegen_op<'ctx, 'ir>(code: &mut CodeGenState<'ctx>, ir: &IRState<'ir>, op: ir::OPID) {
    let op = ir.ir.get(op).unwrap();
    match &op.kind { // every var corresponds to a stack pointer
        ir::OPKind::Constant { value } => {
            let const_val = code.context.i32_type().const_int(*value as u64, false);
            let var = code.new_stack_var(ir, op.res.unwrap());
            code.builder.build_store(var, const_val).unwrap();
            code.var_ids.insert(op.res.unwrap(), var);
        }
        ir::OPKind::Add { lhs, rhs } => {
            let lhs_llvm = code.load_var(ir, *lhs);
            let rhs_llvm = code.load_var(ir, *rhs);
            
            let res_ptr = code.new_stack_var(ir, op.res.unwrap());
            let res_val = code.builder.build_int_add(lhs_llvm.into_int_value(), rhs_llvm.into_int_value(), "").unwrap();
            code.builder.build_store(res_ptr, res_val).unwrap();
        }
        ir::OPKind::Call { func, args } => {
            let func = code.module.get_function(&ir.namer.name_func(*func)).unwrap();
            let args = args.iter().enumerate().map(|(i, &arg)| {
                let arg_val = code.load_var(ir, arg);
                let arg_ty = arg_val.get_type();
                let expected_arg_ty = func.get_nth_param(i as u32).unwrap().get_type();
                if arg_ty != expected_arg_ty {
                    panic!("expected arg type {:?}, got {:?}", expected_arg_ty, arg_ty);
                }
                arg_val.into()
            }).collect::<Vec<_>>();
            let res_ptr = code.new_stack_var(ir, op.res.unwrap());
            let res_val = code.builder.build_call(func, &args, "call").unwrap().try_as_basic_value();
            if res_val.is_left() {
                code.builder.build_store(res_ptr, res_val.left().unwrap()).unwrap();
            }
        }
        ir::OPKind::GetAttr { obj, idx, .. } => {
            let obj_val = code.var_ids[obj];
            let obj_ty = code.get_var_type(ir, *obj).into_struct_type();
            let res_ty = code.get_var_type(ir, op.res.unwrap());
            let idx = idx.unwrap();
            let field = code.builder.build_struct_gep(obj_ty, obj_val, idx as u32, "").unwrap();
            let field_val = code.builder.build_load(res_ty, field, "").unwrap();
            let res_ptr = code.new_stack_var(ir, op.res.unwrap());
            code.builder.build_store(res_ptr, field_val).unwrap();
        }
        ir::OPKind::Return { value } => {
            let ret_val = code.load_var(ir, *value);
            code.builder.build_return(Some(&ret_val)).unwrap();
        }
        ir::OPKind::Struct { fields } => {
            let res = code.new_stack_var(ir, op.res.unwrap());
            let struct_ty = code.get_type(ir, op.res.unwrap().type_of(&ir.ir)).into_struct_type();
            for (i, (_, field)) in fields.iter().enumerate() {
                let field_val = code.load_var(ir, *field);
                let field_ptr = code.builder.build_struct_gep(struct_ty, res, i as u32, "").unwrap();
                code.builder.build_store(field_ptr, field_val).unwrap();                
            }
        }
        ir::OPKind::Assign { lhs, rhs } => {
            let rhs_val = code.load_var(ir, *rhs);
            let lhs_ptr = code.var_ids[lhs];
            code.builder.build_store(lhs_ptr, rhs_val).unwrap();
        }
    }
}



/// TODO:
/// - disallow recursive and mutually recursive types
/// - remove void types from everywhere except function returns, or just make it a char
/// - only have stack allocated types

pub fn generate_llvm(ir: &ir::IR) -> Result<String> {
    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let mut codegen = CodeGenState {
        context: &context,
        module,
        builder,
        types: HashMap::new(),
        var_ids: HashMap::new(),
    };

    let ir_state = IRState {
        ir,
        namer: ir::IRNamer::new(ir, false),
    };

    codegen_type_decls(&mut codegen, &ir_state);
    codegen_fn(&mut codegen, &ir_state);


    // codegen.codegen(ir)?;
    Ok(codegen.module.to_string())
}

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

    fn run_frontend(s: &str) -> ir::IR {
        let ast = frontend::parse(s.into(), None).unwrap();
        let mut ir = frontend::ast_to_ir(&ast).unwrap();
        frontend::transform_ir(&mut ir);
        ir
    }

    #[test]
    fn test_codegen1() {
        let ir = run_frontend("\
            type T = {a: i32, b: i32}
            fn foo(a: T, b: i32): i32 {
                return (a.a + b);
            }
        ");

        // println!("{}", ir::print_basic(&ir));

        let llvm = generate_llvm(&ir).unwrap();
        println!("{}", llvm);
    }

    #[test]
    fn test_codegen2() {
        let ir = run_frontend("\
            fn bar(a: i32, b: i32): i32 {
                let c = (a + 1);
                let d = (b + 1);
                return (c + d);
            }
        ");

        let llvm = generate_llvm(&ir).unwrap();
        println!("{}", llvm);
    }

    
    #[test]
    fn test_codegen3() {
        let ir = run_frontend("\
            type T = {a: i32, b: i32}
            type R = {a: T}
            fn foo(a: T): i32 {
                return a.a;
            }

            fn bar(a: i32, b: i32): i32 {
                return a;
            }

            fn main() {
                let a = {a: 1, b: 2};
                let c = {a: a};
                bar(3, a.a);
                foo(a);

            }
        ");
        println!("{}", ir::print_basic(&ir));
        // let llvm = generate_llvm(&ir).unwrap();
        // println!("{}", llvm);
    }

}
