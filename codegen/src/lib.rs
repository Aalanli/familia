use std::collections::HashMap;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{AsTypeRef, BasicType, BasicTypeEnum, PointerType};
use inkwell::values::{BasicValueEnum, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};

use anyhow::Result;

use familia_frontend as frontend;
use familia_frontend::ir;
use ir::IR;

struct IRState<'ir> {
    ir: &'ir IR,
    namer: ir::IRNamer,
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

fn generate_llvm_type<'ctx, 'ir>(
    code: &mut CodeGenState<'ctx>,
    ir: &IRState<'ir>,
    ty_id: ir::TypeID,
) -> BasicTypeEnum<'ctx> {
    let ty_decl = ir.ir.get(ty_id);
    let res = match &ty_decl.kind {
        ir::TypeKind::Struct { fields } => {
            let struct_ty = code.context.struct_type(
                &fields
                    .iter()
                    .map(|&ty| generate_llvm_type(code, ir, ty.1).into())
                    .collect::<Vec<_>>(),
                false,
            );
            struct_ty.into()
        }
        ir::TypeKind::I32 => code.context.i32_type().into(),
        ir::TypeKind::Void => {
            panic!("void type not allowed here");
        }
        ir::TypeKind::String => code.context.ptr_type(AddressSpace::default()).into(),
        ir::TypeKind::Ptr => code.context.ptr_type(AddressSpace::default()).into(),
    };
    code.types.insert(ty_id, res);
    res
}

fn codegen_type_decls<'ctx, 'ir>(code: &mut CodeGenState<'ctx>, ir: &IRState<'ir>) {
    for ty_id in ir.ir.iter_stable::<ir::TypeDeclID>() {
        code.context
            .opaque_struct_type(&ir.namer.name_type_decl(ty_id));
    }

    for ty_id in ir.ir.iter_stable::<ir::TypeDeclID>() {
        let decl = ir.ir.get(ty_id);
        let llvm_ty = code
            .context
            .get_struct_type(&ir.namer.name_type_decl(ty_id))
            .unwrap();
        let ty = ir.ir.get(decl.decl);
        if let ir::TypeKind::Struct { fields } = &ty.kind {
            llvm_ty.set_body(
                &fields
                    .iter()
                    .map(|&ty| generate_llvm_type(code, ir, ty.1).into())
                    .collect::<Vec<_>>(),
                false,
            );
        } else {
            panic!("only struct types allowed here");
        }
    }
}

fn codegen_fn<'ctx, 'ir>(code: &mut CodeGenState<'ctx>, ir: &IRState<'ir>) {
    for func_id in ir.ir.iter_stable::<ir::FuncID>() {
        let func = ir.ir.get(func_id);
        let fn_type;
        let arg_types = func
            .decl
            .args
            .iter()
            .map(|&(_, ty)| code.get_type(ir, ty).into())
            .collect::<Vec<_>>();
        if func.decl.ret_ty == ir::TypeID::insert(&ir.ir, ir::TypeKind::Void) {
            fn_type = code.context.void_type().fn_type(&arg_types, false);
        } else {
            fn_type = code
                .get_type(ir, func.decl.ret_ty)
                .fn_type(&arg_types, false);
        }
        code.module
            .add_function(&ir.namer.name_func(func_id), fn_type, None);
    }

    for func_id in ir.ir.iter::<ir::FuncID>() {
        let func = ir.ir.get(func_id);
        if func.builtin {
            continue;
        }
        let llvm_func = code
            .module
            .get_function(ir.namer.name_func(func_id))
            .unwrap();
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
        if func.decl.ret_ty == ir::TypeID::insert(&ir.ir, ir::TypeKind::Void) {
            code.builder.build_return(None).unwrap();
        }
    }
}

fn codegen_op<'ctx, 'ir>(code: &mut CodeGenState<'ctx>, ir: &IRState<'ir>, op: ir::OPID) {
    let op = ir.ir.get(op);
    match &op.kind {
        // every var corresponds to a stack pointer
        ir::OPKind::Constant(c) => match c {
            ir::ConstKind::I32(value) => {
                let const_val = code.context.i32_type().const_int(*value as u64, false);
                let var = code.new_stack_var(ir, op.res.unwrap());
                code.builder.build_store(var, const_val).unwrap();
            }
            ir::ConstKind::String(s) => {
                let str = s.get_str(&ir.ir);
                let str_val = code.builder.build_global_string_ptr(str, "str").unwrap();

                let res = code.new_stack_var(ir, op.res.unwrap());
                let rts_new_str = code.module.get_function("__rts_new_string").unwrap();
                let const_len = code
                    .context
                    .i32_type()
                    .const_int(str.as_bytes().len() as u64, false);
                let ptr = code
                    .builder
                    .build_call(
                        rts_new_str,
                        &[const_len.into(), str_val.as_pointer_value().into()],
                        "",
                    )
                    .unwrap();
                code.builder
                    .build_store(res, ptr.try_as_basic_value().left().unwrap())
                    .unwrap();
            }
            _ => unimplemented!("codegen for {:?}", c),
        },
        ir::OPKind::Add { lhs, rhs } => {
            let lhs_llvm = code.load_var(ir, *lhs);
            let rhs_llvm = code.load_var(ir, *rhs);

            let res_ptr = code.new_stack_var(ir, op.res.unwrap());
            let res_val = code
                .builder
                .build_int_add(lhs_llvm.into_int_value(), rhs_llvm.into_int_value(), "")
                .unwrap();
            code.builder.build_store(res_ptr, res_val).unwrap();
        }
        ir::OPKind::Call { func, args } => {
            let func = code
                .module
                .get_function(&ir.namer.name_func(*func))
                .unwrap();
            let args = args
                .iter()
                .enumerate()
                .map(|(i, &arg)| {
                    let ptr = code.var_ids[&arg];

                    // let arg_ty = arg_val.get_type();
                    // ignore the actual type of var for now
                    let expected_arg_ty = func.get_nth_param(i as u32).unwrap().get_type();
                    let arg_val = code.builder.build_load(expected_arg_ty, ptr, "").unwrap();

                    // if arg_ty != expected_arg_ty {
                    //     panic!("expected arg type {:?}, got {:?}", expected_arg_ty, arg_ty);
                    // }
                    arg_val.into()
                })
                .collect::<Vec<_>>();

            let res_val = code
                .builder
                .build_call(func, &args, "call")
                .unwrap()
                .try_as_basic_value();
            if !(op.res.is_none()
                || op.res.unwrap().type_of(&ir.ir)
                    == ir::TypeID::insert(&ir.ir, ir::TypeKind::Void))
            {
                let res_ptr = code.new_stack_var(ir, op.res.unwrap());
                if res_val.is_left() {
                    code.builder
                        .build_store(res_ptr, res_val.left().unwrap())
                        .unwrap();
                }
            }
        }
        ir::OPKind::GetAttr { obj, idx, .. } => {
            let obj_val = code.var_ids[obj];
            let obj_ty = code.get_var_type(ir, *obj).into_struct_type();
            let res_ty = code.get_var_type(ir, op.res.unwrap());
            let idx = idx.unwrap();
            let field = code
                .builder
                .build_struct_gep(obj_ty, obj_val, idx as u32, "")
                .unwrap();
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
            let struct_ty = code
                .get_type(ir, op.res.unwrap().type_of(&ir.ir))
                .into_struct_type();
            for (i, (_, field)) in fields.iter().enumerate() {
                let field_val = code.load_var(ir, *field);
                let field_ptr = code
                    .builder
                    .build_struct_gep(struct_ty, res, i as u32, "")
                    .unwrap();
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
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();

    let passes: &[&str] = &[
        // "instcombine",
        // "reassociate",
        // "gvn",
        // "simplifycfg",
        // "basic-aa",
        // "mem2reg",
        "default<O1>",
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

pub enum OptLevel {
    None,
    O1,
}

impl Default for OptLevel {
    fn default() -> Self {
        OptLevel::None
    }
}

pub fn generate_llvm(ir: &ir::IR, opt_level: OptLevel) -> Result<String> {
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
        namer: ir::IRNamer::new(ir),
    };

    codegen_type_decls(&mut codegen, &ir_state);
    codegen_fn(&mut codegen, &ir_state);

    if let OptLevel::O1 = opt_level {
        run_passes_on(&codegen.module);
    }

    Ok(codegen.module.to_string())
}

use inkwell::llvm_sys::core;
fn make_pointer_type<'a>(ty: BasicTypeEnum<'a>) -> PointerType<'a> {
    unsafe {
        let ptr_ty = core::LLVMPointerType(ty.as_type_ref(), 0);
        PointerType::new(ptr_ty)
    }
}

#[cfg(test)]
mod tests {
    use inkwell::AddressSpace;

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

        let fn_ty = ty.fn_type(
            &[context.f128_type().into(), context.i32_type().into()],
            false,
        );
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

    #[test]
    fn test_merge_modules() {
        let context = Context::create();
        let module1 = context.create_module("test1");
        let module2 = context.create_module("test2");

        let ty = context.opaque_struct_type("footy");
        let ptr_ty = make_pointer_type(ty.into());

        // let ty = context.ptr_type(AddressSpace::default());

        let fn_ty = context.void_type().fn_type(&[ptr_ty.into()], false);
        module1.add_function("foo", fn_ty, Some(inkwell::module::Linkage::External));

        let ty = context.struct_type(
            &[context.i32_type().into(), context.i32_type().into()],
            false,
        );
        let ptr_ty = make_pointer_type(ty.into());
        let fn_ty = context.void_type().fn_type(&[ptr_ty.into()], false);

        let func = module2.add_function("foo", fn_ty, Some(inkwell::module::Linkage::External));
        let entry = context.append_basic_block(func, "entry");
        let builder = context.create_builder();
        builder.position_at_end(entry);
        let arg = func.get_first_param().unwrap();
        let a = context.i32_type().const_int(1, false);
        let i1 = builder
            .build_struct_gep(ty, arg.into_pointer_value(), 0, "i1")
            .unwrap();
        builder.build_store(i1, a).unwrap();

        builder.build_return(None).unwrap();

        println!("{}", module1.print_to_string().to_string());
        println!("{}", module2.print_to_string().to_string());

        module1.link_in_module(module2).unwrap();

        println!("{}", module1.print_to_string().to_string());
    }

    #[test]
    fn test_const_str() {
        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();
        let print_ty = context
            .void_type()
            .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false);
        let foo_ty = context.void_type().fn_type(&[], false);
        module.add_function("print", print_ty, None);
        let foo = module.add_function("foo", foo_ty, None);
        let entry = context.append_basic_block(foo, "entry");
        builder.position_at_end(entry);
        let print = module.get_function("print").unwrap();
        // let str_arr = context.const_string("hello".as_bytes(), false);
        // let local = builder.build_alloca(str_arr.get_type(), "str").unwrap();
        // builder.build_store(local, str_arr).unwrap();

        // let str_ptr = builder.build_bit_cast(local, context.ptr_type(AddressSpace::default()), "str_ptr").unwrap();
        // // let str_ptr = builder.build_global_string_ptr("hello world", "str").unwrap();
        // // let str_val = builder.build_load(str_ptr.get_value_type().into(), str_ptr.into(), "").unwrap();

        // builder.build_call(print, &[str_ptr.into()], "").unwrap();

        let str = builder.build_global_string_ptr("hello", "str").unwrap();
        let str_ptr = str.as_pointer_value();

        builder.build_call(print, &[str_ptr.into()], "").unwrap();

        let str = builder.build_global_string_ptr("hello2", "str").unwrap();
        let str_ptr = str.as_pointer_value();

        builder.build_call(print, &[str_ptr.into()], "").unwrap();

        builder.build_return(None).unwrap();
        let module_str = module.print_to_string().to_string();
        println!("{}", module_str);
    }

    fn run_frontend(s: &str) -> ir::IR {
        let src = s.into();
        let ast = frontend::parse(&src).unwrap();
        let mut ir = frontend::ast_to_ir(&src, &ast).unwrap();
        frontend::transform_ir(&mut ir, &src).unwrap();
        ir
    }

    #[test]
    fn test_codegen1() {
        let ir = run_frontend(
            "\
            type T = {a: i32, b: i32}
            fn foo(a: T, b: i32): i32 {
                return (a.a + b);
            }
        ",
        );

        println!("{}", ir::print_basic(&ir));

        let llvm = generate_llvm(&ir, Default::default()).unwrap();
        println!("{}", llvm);
    }

    #[test]
    fn test_codegen2() {
        let ir = run_frontend(
            "\
            fn bar(a: i32, b: i32): i32 {
                let c = (a + 1);
                let d = (b + 1);
                return (c + d);
            }
        ",
        );

        let llvm = generate_llvm(&ir, Default::default()).unwrap();
        println!("{}", llvm);
    }

    #[test]
    fn test_codegen3() {
        let ir = run_frontend(
            "\
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
        ",
        );
        println!("{}", ir::print_basic(&ir));
        let llvm = generate_llvm(&ir, Default::default()).unwrap();
        println!("{}", llvm);
    }

    #[test]
    fn test_codegen4() {
        let ir = run_frontend(
            "\
            fn foo(a: String): String {
                let b = to_str(1);
                let b1 = \"2\";
                let c = (a + \"1\");
                print(c);
                return (b + c);
            }
            ",
        );
        println!("{}", ir::print_basic(&ir));
        let llvm = generate_llvm(&ir, Default::default()).unwrap();
        println!("{}", llvm);
    }
}
