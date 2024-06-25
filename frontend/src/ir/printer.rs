use std::{cell::RefCell, collections::HashMap};

use super::*;

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
    fn_prefix_ids: HashMap<FuncID, u32>,
    type_prefix_ids: HashMap<TypeDeclID, u32>,
    class_prefix_ids: HashMap<ClassID, u32>,
    var_prefix: HashMap<VarID, u32>,
    cache: RefCell<HashMap<NodeID, Box<str>>>,
}

impl IRNamer {
    pub fn new(ir: &IR, stable: bool) -> Self {
        IRNamer {
            fn_prefix_ids: Self::compute_id_name(ir, stable),
            type_prefix_ids: Self::compute_id_name(ir, stable),
            class_prefix_ids: Self::compute_id_name(ir, stable),
            var_prefix: Self::compute_id_name(ir, stable),
            cache: RefCell::new(HashMap::new()),
        }
    }

    fn compute_id_name<T: ID>(ir: &IR, stable: bool) -> HashMap<T, u32> {
        let mut prefix_ids = HashMap::new();
        for (i, id) in ir.iter_stable::<T>().enumerate() {
            prefix_ids.insert(id, i as u32);
        }
        prefix_ids
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

    pub fn name_type(&self, ty: TypeDeclID) -> &str {
        self.cacher(ty.id(), || format!("t{}", self.type_prefix_ids[&ty]))
    }

    pub fn name_var(&self, var: VarID) -> &str {
        self.cacher(var.id(), || format!("{}", self.var_prefix[&var]))
    }

    pub fn name_func(&self, func: FuncID) -> &str {
        self.cacher(func.id(), || format!("f{}", self.fn_prefix_ids[&func]))
    }

    pub fn name_class(&self, class: ClassID) -> &str {
        self.cacher(class.id(), || format!("c{}", self.class_prefix_ids[&class]))
    }
}

pub struct BasicPrinter<'ir> {
    ir_namer: IRNamer,
    ir: &'ir IR,
}

impl<'ir> BasicPrinter<'ir> {
    pub fn new(ir: &'ir IR) -> Self {
        BasicPrinter {
            ir_namer: IRNamer::new(ir, true),
            ir,
        }
    }

    pub fn print_type(&self, ty: TypeID) -> String {
        let ty = self.ir.get(ty);
        match &ty.kind {
            TypeKind::Struct { fields } => {
                return arg_list(
                    "{",
                    "}",
                    ", ",
                    fields.iter().map(|(name, ty)| {
                        format!(
                            "{}: {}",
                            name.get_str(&self.ir),
                            self.print_type(*ty)
                        )
                    }),
                );
            }
            TypeKind::Decl { decl } => {
                return format!("@{}", self.ir_namer.name_type(*decl));
            }
            TypeKind::I32 => {
                return "i32".to_string();
            }
            TypeKind::Void => {
                return "()".to_string();
            }
        }
    }

    pub fn print_type_decl(&self, ty_id: TypeDeclID) -> String {
        let ty = self.ir.get(ty_id);
        let decl = self.print_type(ty.decl);
        format!("type @{} = {}", self.ir_namer.name_type(ty_id), decl)
    }

    pub fn print_var(&self, var_id: VarID) -> String {
        let var = self.ir.get(var_id);
        if var.ty.is_none() {
            return format!("%{}", self.ir_namer.name_var(var_id));
        }
        let ty = self.print_type(var.ty.unwrap());
        format!("%{}: {}", self.ir_namer.name_var(var_id), ty)
    }

    pub fn print_op(&self, op_id: &OPID) -> String {
        let op = self.ir.get(*op_id);
        let args;
        let name;
        match &op.kind {
            OPKind::Assign { lhs, rhs } => {
                args = vec![*lhs, *rhs];
                name = "assign";
            }
            OPKind::Constant { value } => {
                return format!("{} = constant({value})", self.print_var(op.res.unwrap()));
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
        let args = arg_list(
            "(",
            ")",
            ", ",
            func.vars.iter().map(|var| self.print_var(*var)),
        );
        let ret_ty = self.print_type(func.decl.ret_ty);
        let prefix = self.ir_namer.name_func(func_id);
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
        let args = arg_list(
            "(",
            ")",
            ", ",
            fimpl.decl.args.iter().map(|(name, ty)| {
                format!(
                    "{}: {}",
                    name.get_str(&self.ir),
                    self.print_type(*ty)
                )
            }),
        );
        let ret_ty = self.print_type(fimpl.decl.ret_ty);
        let prefix = self.ir_namer.name_func(func);
        format!("fn @{}{}: {} {{...}}", prefix, args, ret_ty,)
    }

    pub fn print_class(&self, class_id: ClassID) -> String {
        let class = self.ir.get(class_id);
        let mut str = format!("class @{}", self.ir_namer.name_class(class_id));
        str += " {\n";
        for methods in class.methods.iter() {
            str += " ";
            str.push_str(&self.print_function_stub(*methods));
            str += "\n";
        }
        str += "\n}";
        str
    }

    pub fn print_ir(&self) -> String {
        let mut lines = Vec::new();
        for id in self.ir.iter_stable::<TypeDeclID>() {
            lines.push(self.print_type_decl(id));
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
