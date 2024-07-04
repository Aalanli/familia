use std::{cell::RefCell, collections::HashMap};

use super::*;

/// Represents a block of text that can be printed.
struct Doc {

}

impl Doc {
    fn pop_head(&mut self) -> String {
        unimplemented!()
    }

    fn push_head(&mut self, s: &str) {
        unimplemented!()
    }

    fn pop_tail(&mut self) -> String {
        unimplemented!()
    }

    fn push_tail(&mut self, s: &str) {
        unimplemented!()
    }
}



fn arg_list(head: &str, tail: &str, sep: &str, args: impl Iterator<Item = String>) -> String {
    let args = args.collect::<Vec<_>>();
    let mut head = head.to_string();
    let len_args = args.len();
    for (i, arg) in args.into_iter().enumerate() {
        head.push_str(&arg);
        if i != len_args - 1 {
            head.push_str(&sep);
        }
    }
    head.push_str(&tail);
    head
}

pub fn print_basic(ir: &IR) -> String {
    let printer = BasicPrinter::new(ir);
    printer.print_ir()
}

pub struct IRNamer {
    fn_names: HashMap<FuncID, String>,
    type_prefix_ids: HashMap<TypeDeclID, String>,
    type_decl_tys: HashMap<TypeID, TypeDeclID>,
    class_prefix_ids: HashMap<ClassID, String>,
    var_prefix: HashMap<VarID, String>,
    global_prefix: HashMap<GlobalConstID, String>,
    cache: RefCell<HashMap<NodeID, Box<str>>>,
}

impl IRNamer {
    pub fn new(ir: &IR) -> Self {
        IRNamer {
            fn_names: Self::compute_id_name(ir, |id: FuncID| {
                ir.get(id).decl.name.get_str(ir).into()
            }),
            type_prefix_ids: Self::compute_id_name(ir, |id: TypeDeclID| {
                ir.get(id).name.get_str(ir).into()
            }),
            type_decl_tys: ir
                .iter()
                .map(|id: TypeDeclID| {
                    let ty = ir.get(id);
                    (ty.decl, id)
                })
                .collect::<HashMap<_, _>>(),
            class_prefix_ids: Self::compute_id_name(ir, |id: ClassID| {
                ir.get(id).name.get_str(ir).into()
            }),
            var_prefix: Self::compute_id_name(ir, |id: VarID| ir.get(id).name.get_str(ir).into()),
            global_prefix: Self::compute_id_name(ir, |id: GlobalConstID| {
                ir.get(ir.get(id).var).name.get_str(ir).into()
            }),
            cache: RefCell::new(HashMap::new()),
        }
    }

    fn compute_id_name<T: ID>(ir: &IR, prefix: impl Fn(T) -> String) -> HashMap<T, String> {
        let mut inserted = HashMap::new();
        let mut names = HashMap::new();
        for id in ir.iter_stable::<T>() {
            let name = prefix(id);
            if !inserted.contains_key(&name) {
                inserted.insert(name.to_string(), 0);
                names.insert(id, if name == "" { "0".to_string() } else { name });
            } else {
                let count = inserted.get_mut(&name).unwrap();
                *count += 1;
                names.insert(id, format!("{}{}", name, count));
            }
        }
        names
    }

    fn cacher(&self, id: NodeID, f: impl FnOnce() -> String) -> &str {
        if let Some(name) = self.cache.borrow().get(&id) {
            return unsafe {
                let name = &**name as *const str;
                &*name
            };
        }
        let name = f();
        self.cache.borrow_mut().insert(id, Box::from(name.as_str()));
        let b = self.cache.borrow();
        let s = b.get(&id).unwrap();
        unsafe {
            let s = &**s as *const str;
            &*s
        }
    }

    pub fn name_type_decl(&self, ty: TypeDeclID) -> &str {
        self.cacher(ty.id(), || format!("{}", self.type_prefix_ids[&ty]))
    }

    pub fn try_name_type(&self, ty: TypeID) -> Option<&str> {
        self.type_decl_tys
            .get(&ty)
            .map(|decl| self.name_type_decl(*decl))
    }

    pub fn name_var(&self, var: VarID) -> &str {
        self.cacher(var.id(), || format!("{}", self.var_prefix[&var]))
    }

    pub fn name_func(&self, func: FuncID) -> &str {
        self.cacher(func.id(), || format!("{}", self.fn_names[&func]))
    }

    pub fn name_class(&self, class: ClassID) -> &str {
        self.cacher(class.id(), || format!("{}", self.class_prefix_ids[&class]))
    }

    pub fn name_global(&self, global: GlobalConstID) -> &str {
        self.cacher(global.id(), || format!("{}", self.global_prefix[&global]))
    }
}

pub struct BasicPrinter<'ir> {
    ir_namer: IRNamer,
    ir: &'ir IR,
}

impl<'ir> BasicPrinter<'ir> {
    pub fn new(ir: &'ir IR) -> Self {
        BasicPrinter {
            ir_namer: IRNamer::new(ir),
            ir,
        }
    }

    pub fn print_tykind(&self, ty: &TypeKind) -> String {
        match ty {
            TypeKind::Struct { fields } => {
                return arg_list(
                    "{",
                    "}",
                    ", ",
                    fields.iter().map(|(name, ty)| {
                        format!("{}: {}", name.get_str(&self.ir), self.print_type(*ty))
                    }),
                );
            }
            TypeKind::This => {
                return format!("This");
            }
            TypeKind::Self_ => {
                return format!("Self");
            }
            TypeKind::Ptr => {
                return format!("@ptr");
            }
            TypeKind::I32 => {
                return "i32".to_string();
            }
            TypeKind::Void => {
                return "()".to_string();
            }
            TypeKind::String => {
                return "String".to_string();
            }
        }
    }

    pub fn print_type(&self, ty: TypeID) -> String {
        let ty = self.ir.get(ty);
        self.print_tykind(&ty.kind)
    }

    pub fn print_type_decl(&self, ty_id: TypeDeclID) -> String {
        let ty = self.ir.get(ty_id);
        let decl = self.print_type(ty.decl);
        format!("type @{} = {}", self.ir_namer.name_type_decl(ty_id), decl)
    }

    pub fn print_var(&self, var_id: VarID) -> String {
        let var = self.ir.get(var_id);
        if var.ty.is_none() {
            return format!("%{}", self.ir_namer.name_var(var_id));
        }
        let ty = self.print_type(var.ty.unwrap());
        format!("%{}: {}", self.ir_namer.name_var(var_id), ty)
    }

    pub fn print_constkind(&self, c: &ConstKind) -> String {
        match c {
            ConstKind::I32(i) => {
                return format!("constant({})", i);
            }
            ConstKind::String(s) => {
                return format!("constant(\"{}\")", s.get_str(self.ir));
            }
            ConstKind::IArray(v) => {
                let args = arg_list("[", "]", ", ", v.iter().map(|i| format!("{}", i)));
                return format!("constant({})", args);
            }
        }
    }

    pub fn print_op(&self, op_id: &OPID) -> String {
        let op = self.ir.get(*op_id);
        let args;
        let name;
        match &op.kind {
            OPKind::Let { value } => {
                return format!(
                    "{} = let {}",
                    self.print_var(op.res.unwrap()),
                    self.print_var(*value)
                );
            }
            OPKind::Assign { lhs, rhs } => {
                args = vec![*lhs, *rhs];
                name = "assign";
            }
            OPKind::Constant(c) => {
                return format!(
                    "{} = {}",
                    self.print_var(op.res.unwrap()),
                    self.print_constkind(c)
                );
            }
            OPKind::Add { lhs, rhs } => {
                args = vec![*lhs, *rhs];
                name = "add";
            }
            OPKind::Call { func, args } => {
                return format!(
                    "{} = call @{}{}",
                    self.print_var(op.res.unwrap()),
                    self.ir_namer.name_func(*func),
                    arg_list(
                        "(",
                        ")",
                        ", ",
                        args.iter().map(|var| { self.print_var(*var) })
                    )
                );
            }
            OPKind::ClsCtor { cls, arg } => {
                return format!(
                    "{} = cls_ctor @{}({})",
                    self.print_var(op.res.unwrap()),
                    self.ir_namer.name_class(*cls),
                    self.print_var(*arg)
                );
            }
            OPKind::MethodCall { obj, method, args } => {
                return format!(
                    "{} = call @{}{}{}",
                    self.print_var(op.res.unwrap()),
                    self.print_var(*obj),
                    method.get_str(self.ir),
                    arg_list(
                        "(",
                        ")",
                        ", ",
                        args.iter().map(|var| { self.print_var(*var) })
                    )
                );
            }
            OPKind::GetAttr { obj, attr, idx } => {
                return format!(
                    "{} = getattr[@{}, idx={:?}]({})",
                    self.print_var(op.res.unwrap()),
                    attr.get_str(&self.ir),
                    idx,
                    self.print_var(*obj),
                );
            }
            OPKind::Return { value } => {
                args = vec![*value];
                name = "return";
            }
            OPKind::Struct { fields } => {
                args = fields.iter().map(|(_, var)| *var).collect();
                name = "struct_ctor";
            }
        }
        let args = arg_list("(", ")", ", ", args.iter().map(|var| self.print_var(*var)));
        // println!("printing {name}{args}");
        if let Some(ret) = op.res {
            return format!("{} = {}{}", self.print_var(ret), name, args);
        }
        format!("{}{}", name, args)
    }

    pub fn print_function(&self, func_id: FuncID) -> String {
        let func = self.ir.get(func_id);
        let args = if func.builtin {
            arg_list(
                "(",
                ")",
                ", ",
                func.decl
                    .args
                    .iter()
                    .map(|(_, ty)| format!("{}", self.print_type(*ty))),
            )
        } else {
            arg_list("(", ")", ", ", func.vars.iter().map(|v| self.print_var(*v)))
        };
        let ret_ty = self.print_type(func.decl.ret_ty);
        let prefix = self.ir_namer.name_func(func_id);
        if func.builtin {
            return format!("fn @{}{}: {} {{ builtin! }}", prefix, args, ret_ty);
        }
        let body = func
            .body
            .iter()
            .map(|op| self.print_op(op))
            .collect::<Vec<_>>()
            .join("\n ");

        format!("fn @{}{}: {} {{\n {body}\n}}  ", prefix, args, ret_ty,)
    }

    pub fn print_function_stub(&self, func: FuncID) -> String {
        let fimpl = self.ir.get(func);
        let args =
            arg_list(
                "(",
                ")",
                ", ",
                fimpl.decl.args.iter().map(|(name, ty)| {
                    format!("{}: {}", name.get_str(&self.ir), self.print_type(*ty))
                }),
            );
        let ret_ty = self.print_type(fimpl.decl.ret_ty);
        let prefix = self.ir_namer.name_func(func);
        format!("fn @{}{}: {} {{...}}", prefix, args, ret_ty,)
    }

    pub fn print_class(&self, class_id: ClassID) -> String {
        let class = self.ir.get(class_id);
        let mut str = format!("class @{}", self.ir_namer.name_class(class_id));
        if let Some(ty) = class.repr_type {
            str += "(";
            str += &self.print_type(ty);
            str += ")";
        }
        str += " {\n";
        for methods in class.methods.iter() {
            str += " ";
            str.push_str(&self.print_function_stub(*methods));
            str += "\n";
        }
        str += "\n}";
        str
    }

    pub fn print_global(&self, global_id: GlobalConstID) -> String {
        let global = self.ir.get(global_id);
        format!(
            "global @{} = {}",
            self.print_var(global.var),
            self.print_constkind(&global.value)
        )
    }

    pub fn print_ir(&self) -> String {
        let mut lines = Vec::new();
        for id in self.ir.iter_stable::<TypeDeclID>() {
            lines.push(self.print_type_decl(id));
        }
        for id in self.ir.iter_stable::<GlobalConstID>() {
            lines.push(self.print_global(id));
        }
        for id in self.ir.iter_stable::<FuncID>() {
            lines.push(self.print_function(id));
        }
        for id in self.ir.iter_stable::<ClassID>() {
            lines.push(self.print_class(id));
        }
        lines.push(String::new());
        lines.join("\n")
    }
}
