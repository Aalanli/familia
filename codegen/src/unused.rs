use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::OptimizationLevel;

use std::error::Error;

type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn jit_compile_sum(&self) -> Option<JitFunction<SumFunc>> {
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        let function = self.module.add_function("sum", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0)?.into_int_value();
        let y = function.get_nth_param(1)?.into_int_value();
        let z = function.get_nth_param(2)?.into_int_value();

        let sum = self.builder.build_int_add(x, y, "sum").unwrap();
        let sum = self.builder.build_int_add(sum, z, "sum").unwrap();

        self.builder.build_return(Some(&sum)).unwrap();

        unsafe { self.execution_engine.get_function("sum").ok() }
    }
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
            OptimizationLevel::None,
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

    let t = target_machine
        .write_to_memory_buffer(module, inkwell::targets::FileType::Assembly)
        .unwrap();
    println!("{}", String::from_utf8(t.as_slice().to_vec()).unwrap());
}

fn main() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::Aggressive)?;
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
    };

    let sum = codegen
        .jit_compile_sum()
        .ok_or("Unable to JIT compile `sum`")?;
    run_passes_on(&codegen.module);
    let m = codegen.module.to_string();

    println!("{}", m);

    let x = 1u64;
    let y = 2u64;
    let z = 3u64;

    unsafe {
        println!("{} + {} + {} = {}", x, y, z, sum.call(x, y, z));
        assert_eq!(sum.call(x, y, z), x + y + z);
    }

    Ok(())
}
