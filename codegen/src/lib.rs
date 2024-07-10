use std::collections::HashMap;
use std::io::Write;
use std::ops::Deref;

use familia_frontend::transforms::RTSRegistry;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, StructType};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};

use either::Either;

use familia_frontend as frontend;
use familia_frontend::ir;
use frontend::transforms::PrimitiveRegistry;
use ir::IR;
mod rts;
use rts::add_rts;
mod utils;
pub use utils::object_to_executable;


struct IRState<'ir> {
    ir: &'ir IR,
    namer: ir::IRNamer<'ir>,
}

impl<'ir> IRState<'ir> {
    pub fn namer(&self) -> &ir::IRNamer {
        &self.namer
    }
}

impl Deref for IRState<'_> {
    type Target = IR;

    fn deref(&self) -> &Self::Target {
        self.ir
    }
}

struct CodeGenState<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    var_ids: HashMap<ir::VarID, PointerValue<'ctx>>,
    repr_types: HashMap<ir::TypeID, BasicTypeEnum<'ctx>>,
    struct_types: HashMap<ir::TypeID, Option<StructType<'ctx>>>,
    vtables: HashMap<ir::ClassID, PointerValue<'ctx>>,
    functions: HashMap<ir::FuncID, FunctionValue<'ctx>>,
    value_types: HashMap<ir::TypeID, BasicTypeEnum<'ctx>>,
}

impl<'ctx> CodeGenState<'ctx> {
    fn new(context: &'ctx Context, module: Module<'ctx>) -> Self {
        let builder = context.create_builder();
        CodeGenState {
            context,
            module,
            builder,
            var_ids: HashMap::new(),
            repr_types: HashMap::new(),
            struct_types: HashMap::new(),
            vtables: HashMap::new(),
            functions: HashMap::new(),
            value_types: HashMap::new(),
        }
    }

    fn get_fn_val<'ir>(&mut self, ir: &IRState<'ir>, func: ir::FuncID) -> FunctionValue<'ctx> {
        if self.functions.contains_key(&func) {
            return self.functions[&func];
        }
        let fn_type = get_fn_type(self, ir, func);
        let llvm_func = self
            .module
            .add_function(&ir.namer.name_func(func), fn_type, None);
        self.functions.insert(func, llvm_func);
        llvm_func
    }

    fn get_rts_fn<'ir>(&mut self, ir: &IRState<'ir>, f: &str) -> FunctionValue<'ctx> {
        let regs = ir.get_global::<RTSRegistry>().unwrap();
        let f = regs.fns[f];
        self.get_fn_val(ir, f)
    }

    fn build_vtable<'ir>(&mut self, ir: &IRState<'ir>, cls: ir::ClassID) -> PointerValue<'ctx> {
        if !self.vtables.contains_key(&cls) {
            let itf = ir.get(cls).for_itf.unwrap();
            let mut methods = vec![];
            let cls_impl = ir.get(cls);
            for method in ir.get(itf).methods.iter() {
                let cls_fn = *cls_impl
                    .methods
                    .iter()
                    .find(|x| ir.get(**x).decl.name == method.name)
                    .unwrap();
                let fn_val = self.get_fn_val(ir, cls_fn);
                let fn_ptr = fn_val.as_global_value().as_pointer_value();
                methods.push(fn_ptr.into());
            }
            let vtable_ty = self.context.struct_type(
                &vec![self.context.ptr_type(AddressSpace::default()).into(); methods.len()],
                false,
            );

            let vtable_val = vtable_ty.const_named_struct(&methods);
            let glob = self.module.add_global(
                vtable_ty,
                Some(AddressSpace::default()),
                ir.get(itf).name.get_str(ir),
            );
            glob.set_initializer(&vtable_val);
            self.vtables.insert(cls, glob.as_pointer_value());
        }
        self.vtables[&cls]
    }

    fn get_var<'ir>(&mut self, ir: &IRState<'ir>, var: ir::VarID) -> PointerValue<'ctx> {
        if self.var_ids.contains_key(&var) {
            return self.var_ids[&var];
        }
        let var_ty = var.type_of(&ir.ir);
        let ty = self.get_repr_type(ir, var_ty);
        let alloca = self
            .builder
            .build_alloca(ty, &*ir.namer().name_var(var))
            .unwrap();
        self.var_ids.insert(var, alloca);
        alloca
    }

    fn get_repr_type<'ir>(&mut self, ir: &IRState<'ir>, ty_id: ir::TypeID) -> BasicTypeEnum<'ctx> {
        if !self.repr_types.contains_key(&ty_id) {
            let llvm_type = match &ir.get(ty_id).kind {
                ir::TypeKind::I32 => self.context.i32_type().into(),
                ir::TypeKind::String => self.context.ptr_type(AddressSpace::default()).into(),
                ir::TypeKind::Void => self.context.i8_type().into(),
                ir::TypeKind::Struct { .. } => {
                    self.context.ptr_type(AddressSpace::default()).into()
                }
                ir::TypeKind::Itf(_) => itf_repr_ty(self),
                ir::TypeKind::Ptr(_) => self.context.ptr_type(AddressSpace::default()).into(),
                x => panic!("no repr type {:?}", x),
            };
            self.repr_types.insert(ty_id, llvm_type);
        }
        self.repr_types[&ty_id]
    }

    fn get_shallow_struct_type<'ir>(
        &mut self,
        ir: &IRState<'ir>,
        ty_id: ir::TypeID,
    ) -> Option<StructType<'ctx>> {
        if !self.struct_types.contains_key(&ty_id) {
            let ty = match &ir.get(ty_id).kind {
                ir::TypeKind::Struct { fields } => {
                    let mut field_types = Vec::new();
                    for field in fields {
                        let field_ty = self.get_repr_type(ir, field.1);
                        field_types.push(field_ty);
                    }
                    Some(self.context.struct_type(&field_types, false))
                }
                _ => None,
            };
            self.struct_types.insert(ty_id, ty);
        }
        self.struct_types[&ty_id]
    }

    fn get_var_type<'ir>(&mut self, ir: &IRState<'ir>, var: ir::VarID) -> BasicTypeEnum<'ctx> {
        self.get_repr_type(ir, var.type_of(&ir.ir))
    }

    fn load_var<'ir>(&mut self, ir: &IRState<'ir>, var: ir::VarID) -> BasicValueEnum<'ctx> {
        let ptr = self.var_ids[&var];
        let pointee_ty = self.get_var_type(ir, var);
        self.builder.build_load(pointee_ty, ptr, "").unwrap().into()
    }
}

fn itf_repr_ty<'ctx>(code: &mut CodeGenState<'ctx>) -> BasicTypeEnum<'ctx> {
    code.context
        .struct_type(
            &[
                code.context.ptr_type(AddressSpace::default()).into(),
                code.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        )
        .into()
}

fn make_itf_val<'ctx>(
    code: &mut CodeGenState<'ctx>,
    vtable: PointerValue<'ctx>,
    data: PointerValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let itf_ty = itf_repr_ty(code).into_struct_type();
    let val = itf_ty.get_poison();
    let val = code
        .builder
        .build_insert_value(val, vtable, 0, "vtable")
        .unwrap()
        .into_struct_value();
    let val = code
        .builder
        .build_insert_value(val, data, 1, "data")
        .unwrap()
        .into_struct_value();
    val.into()
}

// assumes that the interface has a compatible representation type
fn get_itf_fn_type<'ctx, 'ir>(
    code: &mut CodeGenState<'ctx>,
    ir: &IRState<'ir>,
    decl: &ir::FuncDecl,
) -> FunctionType<'ctx> {
    let mut map_itf_meth_ty = |ty| {
        if ty == ir::TypeID::this(ir) {
            return code.context.ptr_type(AddressSpace::default()).into();
        } else if ty == ir::TypeID::self_(ir) {
            return itf_repr_ty(code).into();
        }
        code.get_repr_type(ir, ty)
    };

    let arg_types: Vec<BasicMetadataTypeEnum> = decl
        .args
        .iter()
        .map(|&(_, ty)| map_itf_meth_ty(ty).into())
        .collect::<Vec<_>>();
    let ret_ty = decl.ret_ty;
    let fn_type = map_itf_meth_ty(ret_ty).fn_type(arg_types.as_slice(), false);
    fn_type
}

fn get_fn_type<'ctx, 'ir>(
    code: &mut CodeGenState<'ctx>,
    ir: &IRState<'ir>,
    func_id: ir::FuncID,
) -> FunctionType<'ctx> {
    let func = ir.get(func_id);
    let fn_type;
    let arg_types = func
        .decl
        .args
        .iter()
        .map(|(_, t)| code.get_repr_type(ir, *t).into())
        .collect::<Vec<_>>();
    if func.decl.ret_ty == ir::TypeID::void(ir) {
        fn_type = code.context.void_type().fn_type(&arg_types, false);
    } else {
        fn_type = code
            .get_repr_type(ir, func.decl.ret_ty)
            .fn_type(&arg_types, false);
    }
    fn_type
}

fn initialize_codegen<'ctx, 'ir>(code: &mut CodeGenState<'ctx>, ir: &IRState<'ir>) {
    let prim_regs = ir.get_global::<PrimitiveRegistry>().unwrap();
    code.repr_types
        .insert(prim_regs.i32, code.context.i32_type().into());
    code.repr_types.insert(
        prim_regs.string,
        code.context.ptr_type(AddressSpace::default()).into(),
    );
    code.repr_types
        .insert(prim_regs.void, code.context.i8_type().into());
    for (name, f) in prim_regs.fns.iter() {
        let fn_type = get_fn_type(code, ir, *f);
        code.module
            .add_function(name, fn_type, Some(Linkage::External));
    }
}

fn codegen_fn<'ctx, 'ir>(code: &mut CodeGenState<'ctx>, ir: &IRState<'ir>) {
    for func_id in ir.iter_stable::<ir::FuncID>() {
        let func = ir.ir.get(func_id);
        if func.builtin {
            continue;
        }
        let llvm_func = code.get_fn_val(ir, func_id);
        let entry = code.context.append_basic_block(llvm_func, "entry");
        code.builder.position_at_end(entry);
        for (i, arg) in llvm_func.get_param_iter().enumerate() {
            arg.set_name(&ir.namer.name_var(func.vars[i]));
            let sarg = code.get_var(ir, func.vars[i]);
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

fn codegen_op<'ctx, 'ir>(code: &mut CodeGenState<'ctx>, ir: &IRState<'ir>, opid: ir::OPID) {
    let op = ir.ir.get(opid);
    // println!("{:?}", op.kind);
    match &op.kind {
        // every var corresponds to a stack pointer
        ir::OPKind::Let { value } => codegen_let(code, ir, op.res.unwrap(), *value),
        ir::OPKind::Constant(c) => codegen_const_op(code, ir, op.res.unwrap(), c),
        ir::OPKind::Add { lhs, rhs } => {
            let lhs_llvm = code.load_var(ir, *lhs);
            let rhs_llvm = code.load_var(ir, *rhs);

            let res_ptr = code.get_var(ir, op.res.unwrap());
            let res_val = code
                .builder
                .build_int_add(lhs_llvm.into_int_value(), rhs_llvm.into_int_value(), "")
                .unwrap();
            code.builder.build_store(res_ptr, res_val).unwrap();
        }
        ir::OPKind::Call { func, args } => codegen_call(code, ir, *func, args, op.res),
        ir::OPKind::GetAttr { obj, idx, .. } => {
            codegen_getattr(code, ir, *obj, idx.unwrap(), op.res.unwrap())
        }
        ir::OPKind::MethodCall { obj, method, args } => {
            codegen_methodcall(code, ir, *obj, *method, args, op.res.unwrap())
        }
        ir::OPKind::ClsCtor { cls, arg } => codegen_cls_ctor(code, ir, *cls, *arg, op.res.unwrap()),
        ir::OPKind::Return { value } => {
            let ret_val = code.load_var(ir, *value);
            code.builder.build_return(Some(&ret_val)).unwrap();
        }
        ir::OPKind::Struct { fields } => codegen_struct(code, ir, fields, op.res.unwrap()),
        ir::OPKind::Assign { lhs, rhs } => {
            let rhs_val = code.load_var(ir, *rhs);
            let lhs_ptr = code.var_ids[lhs];
            code.builder.build_store(lhs_ptr, rhs_val).unwrap();
        }
    }
}

fn codegen_const_op<'ctx, 'ir>(
    code: &mut CodeGenState<'ctx>,
    ir: &IRState<'ir>,
    res: ir::VarID,
    c: &ir::ConstKind,
) {
    match c {
        ir::ConstKind::I32(value) => {
            let const_val = code.context.i32_type().const_int(*value as u64, false);
            let var = code.get_var(ir, res);
            code.builder.build_store(var, const_val).unwrap();
        }
        ir::ConstKind::String(s) => {
            let str = s.get_str(&ir.ir);
            let str_val = code.builder.build_global_string_ptr(str, "str").unwrap();

            let res = code.get_var(ir, res);
            let rts_new_str = code.get_rts_fn(ir, "__rts_new_string");
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
    }
}

fn codegen_call<'ctx, 'ir>(
    code: &mut CodeGenState<'ctx>,
    ir: &IRState<'ir>,
    func: ir::FuncID,
    args: &[ir::VarID],
    res: Option<ir::VarID>,
) {
    let func = code.get_fn_val(ir, func);
    // println!("{}", func.get_name().to_str().unwrap());
    assert!(func.count_params() == args.len() as u32);

    let args = args
        .iter()
        .enumerate()
        .map(|(i, &arg)| {
            let ptr = code.var_ids[&arg];
            // println!("gen {i}");

            let arg_ty = code.get_var_type(ir, arg);
            let expected_arg_ty = func.get_nth_param(i as u32).unwrap();
            if arg_ty != expected_arg_ty.get_type() {
                panic!("expected arg type {:?}, got {:?}", expected_arg_ty, arg_ty);
            }
            let arg_val = code.builder.build_load(arg_ty, ptr, "").unwrap();
            arg_val.into()
        })
        .collect::<Vec<_>>();

    let res_val = code
        .builder
        .build_call(func, &args, "call")
        .unwrap()
        .try_as_basic_value();
    if !(res.is_none() || res.unwrap().type_of(&ir.ir) == ir::TypeID::void(ir)) {
        let res_ptr = code.get_var(ir, res.unwrap());
        if res_val.is_left() {
            code.builder
                .build_store(res_ptr, res_val.left().unwrap())
                .unwrap();
        }
    }
}

fn codegen_getattr<'ctx, 'ir>(
    code: &mut CodeGenState<'ctx>,
    ir: &IRState<'ir>,
    obj: ir::VarID,
    idx: usize,
    res: ir::VarID,
) {
    // every struct is allocated on the heap, first remove the stack pointer indirection
    let obj_val = code.load_var(ir, obj).into_pointer_value();
    let struct_ty = code.get_shallow_struct_type(ir, obj.type_of(&ir)).unwrap();

    let field = code
        .builder
        .build_struct_gep(struct_ty, obj_val, idx as u32, "")
        .unwrap();

    // this is for the purpose of "foo.bar = 1"
    // this has the effect of aliasing a pointer to the field
    // we make sure that all "value types" like i32 are copied when used as a function parameter
    code.var_ids.insert(res, field);
}

fn codegen_struct<'ctx, 'ir>(
    code: &mut CodeGenState<'ctx>,
    ir: &IRState<'ir>,
    fields: &[(ir::SymbolID, ir::VarID)],
    res: ir::VarID,
) {
    let struct_ty = code.get_shallow_struct_type(ir, res.type_of(&ir)).unwrap();
    let size = struct_ty.size_of().unwrap();
    let size = code
        .builder
        .build_int_cast(size, code.context.i32_type(), "")
        .unwrap();
    let gc_alloc = code.get_rts_fn(ir, "__rts_gc_alloc");

    // TODO: the first argument should be the configuration struct specifying
    // the fields that are pointers allocated by the gc
    // leak memory for now
    let nullptr = code.context.ptr_type(AddressSpace::default()).const_null();
    let gc_ptr = code
        .builder
        .build_call(gc_alloc, &[nullptr.into(), size.into()], "alloc")
        .unwrap()
        .try_as_basic_value()
        .left()
        .unwrap()
        .into_pointer_value();

    for (i, (_, field)) in fields.iter().enumerate() {
        let field_val = code.load_var(ir, *field);
        let field_ptr = code
            .builder
            .build_struct_gep(struct_ty, gc_ptr, i as u32, "")
            .unwrap();
        code.builder.build_store(field_ptr, field_val).unwrap();
    }
    let res_ptr = code.get_var(ir, res);
    code.builder.build_store(res_ptr, gc_ptr).unwrap();
}

fn codegen_cls_ctor<'ctx, 'ir>(
    code: &mut CodeGenState<'ctx>,
    ir: &IRState<'ir>,
    cls: ir::ClassID,
    arg: ir::VarID,
    res: ir::VarID,
) {
    let vtable_ptr = code.build_vtable(ir, cls);
    let data = code.load_var(ir, arg).into_pointer_value();
    let itf_val = make_itf_val(code, vtable_ptr, data);
    let res_var = code.get_var(ir, res);
    code.builder.build_store(res_var, itf_val).unwrap();
}

fn codegen_methodcall<'ctx, 'ir>(
    code: &mut CodeGenState<'ctx>,
    ir: &IRState<'ir>,
    obj: ir::VarID,
    method: ir::SymbolID,
    args: &[ir::VarID],
    res_var: ir::VarID,
) {
    let itf = ir.get(obj).ty.unwrap();
    let ir::TypeKind::Itf(itf) = ir.get(itf).kind else {
        panic!("methodcall of non-itf object")
    };
    let itf = ir.get(itf);
    let method_idx = itf.methods.iter().position(|x| x.name == method).unwrap();
    let vtable_ty = code.context.struct_type(
        &vec![code.context.ptr_type(AddressSpace::default()).into(); itf.methods.len()],
        false,
    );

    let obj_val = code.load_var(ir, obj).into_struct_value();
    let vtable = code
        .builder
        .build_extract_value(obj_val, 0, "")
        .unwrap()
        .into_pointer_value();
    let data = code
        .builder
        .build_extract_value(obj_val, 1, "")
        .unwrap()
        .into_pointer_value();
    let fn_ptr = code
        .builder
        .build_struct_gep(vtable_ty, vtable, method_idx as u32, "")
        .unwrap();
    let fn_ty = get_itf_fn_type(code, ir, &itf.methods[method_idx]);
    let mut args_vals = vec![data.into()];
    for arg in args {
        let arg_val = code.load_var(ir, *arg).into();
        args_vals.push(arg_val);
    }
    let res = code
        .builder
        .build_indirect_call(fn_ty, fn_ptr, &args_vals, "meth")
        .unwrap()
        .try_as_basic_value();
    if let Either::Left(res) = res {
        let res_ptr = code.get_var(ir, res_var);
        code.builder.build_store(res_ptr, res).unwrap();
    }
}

fn codegen_let<'ctx, 'ir>(
    code: &mut CodeGenState<'ctx>,
    ir: &IRState<'ir>,
    var: ir::VarID,
    value: ir::VarID,
) {
    let value = code.load_var(ir, value);
    let var_ptr = code.get_var(ir, var);
    code.builder.build_store(var_ptr, value).unwrap();
}

/// TODO:
/// - disallow recursive and mutually recursive types
/// - remove void types from everywhere except function returns, or just make it a char
/// - only have stack allocated types
use inkwell::{
    passes::PassBuilderOptions,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
};

fn get_target_machine() -> TargetMachine {
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
    target_machine
}

fn run_passes_on(module: &Module) {
    let target_machine = get_target_machine();
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
}

fn get_obj<W: Write>(module: &Module, w: &mut W) {
    let target_machine = get_target_machine();
    let t = target_machine
        .write_to_memory_buffer(module, inkwell::targets::FileType::Object)
        .unwrap();
    w.write_all(t.as_slice()).unwrap();
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

pub struct CodeGenOptions<'a> {
    pub opt_level: OptLevel,
    pub add_rts: bool,
    pub write_obj: bool, // otherwise dump the string
    pub output: &'a mut dyn Write,
}

pub fn generate_llvm(ir: &ir::IR, options: &mut CodeGenOptions) {
    let context = Context::create();
    let module = context.create_module("main");

    let mut codegen = CodeGenState::new(&context, module);
    let ir_state = IRState {
        ir,
        namer: ir::IRNamer::new(ir),
    };

    initialize_codegen(&mut codegen, &ir_state);
    codegen_fn(&mut codegen, &ir_state);

    if options.add_rts {
        let rts = add_rts(&context);
        codegen.module.link_in_module(rts).unwrap();
    }

    if let OptLevel::O1 = options.opt_level {
        run_passes_on(&codegen.module);
    }

    if options.write_obj {
        get_obj(&codegen.module, &mut options.output);
    } else {
        options
            .output
            .write_all(codegen.module.print_to_string().to_bytes())
            .unwrap();
    }
}

pub fn dump_llvm(ir: &ir::IR, opt_level: OptLevel) -> String {
    let mut str = vec![];
    let mut options = CodeGenOptions {
        opt_level,
        add_rts: false,
        write_obj: false,
        output: &mut str,
    };

    generate_llvm(ir, &mut options);
    String::from_utf8(str).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use inkwell::llvm_sys::core;
    use inkwell::types::{AsTypeRef, PointerType};
    use inkwell::AddressSpace;
    fn make_pointer_type<'a>(ty: BasicTypeEnum<'a>) -> PointerType<'a> {
        unsafe {
            let ptr_ty = core::LLVMPointerType(ty.as_type_ref(), 0);
            PointerType::new(ptr_ty)
        }
    }

    fn run_frontend(s: &str) -> ir::IR {
        let src = s.into();
        let ast = frontend::parse(&src).unwrap();
        let mut ir = frontend::ast_to_ir(src, ast).unwrap();
        frontend::transform_ir(&mut ir).unwrap();
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
            fn main() {}
        ",
        );

        println!("{}", ir::print_basic(&ir));

        let llvm = dump_llvm(&ir, OptLevel::None);
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
            fn main() {
                (to_str(1));
            }
        ",
        );
        println!("{}", ir::print_basic(&ir));

        let llvm = dump_llvm(&ir, OptLevel::None);
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
                // let c = {a: a};
                // bar(3, a.a);
                // foo(a);

            }
        ",
        );
        println!("{}", ir::print_basic(&ir));
        let llvm = dump_llvm(&ir, OptLevel::None);
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
            fn main() {
                print(foo(\"1\"));
            }
            ",
        );
        println!("{}", ir::print_basic(&ir));
        let llvm = dump_llvm(&ir, OptLevel::None);
        println!("{}", llvm);
    }

    #[test]
    fn test_codegen5() {
        let ir = run_frontend(
            "\
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
            ",
        );
        println!("{}", ir::print_basic(&ir));
        let llvm = dump_llvm(&ir, OptLevel::None);
        println!("{}", llvm);
    }

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

        let _module_str = module.print_to_string().to_string();
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
        let _a = func.get_nth_param(0).unwrap();
        let _b = func.get_nth_param(1).unwrap();
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

    #[test]
    fn test_build_vtable() {
        let context = Context::create();
        let module = context.create_module("test");
        let _builder = context.create_builder();

        let vtable_ty = context.struct_type(
            &[
                context.ptr_type(AddressSpace::default()).into(),
                context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );

        let foo = module.add_function("foo", context.void_type().fn_type(&[], false), None);
        let bar = module.add_function("bar", context.void_type().fn_type(&[], false), None);

        let vtable = vtable_ty.const_named_struct(&[
            foo.as_global_value().as_pointer_value().into(),
            bar.as_global_value().as_pointer_value().into(),
        ]);

        let glob = module.add_global(vtable_ty, Some(AddressSpace::default()), "vtable");
        glob.set_initializer(&vtable);

        println!("{}", module.print_to_string().to_string());
    }

    #[test]
    fn test_call_ptr() {
        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();

        let print_ty = context
            .void_type()
            .fn_type(&[context.ptr_type(AddressSpace::default()).into()], false);
        let print = module.add_function("print", print_ty, None);
        let glob = module.add_global(context.ptr_type(AddressSpace::default()), None, "glob");
        glob.set_initializer(&print.as_global_value().as_pointer_value());

        let main = module.add_function("main", context.void_type().fn_type(&[], false), None);
        let entry = context.append_basic_block(main, "entry");
        builder.position_at_end(entry);
        let ptr = module.get_global("glob").unwrap().as_pointer_value();
        // ptr into function
        builder
            .build_indirect_call(
                print_ty,
                ptr,
                &[context
                    .ptr_type(AddressSpace::default())
                    .const_null()
                    .into()],
                "tr",
            )
            .unwrap();

        println!("{}", module.print_to_string().to_string());
    }
}
