use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use super::*;

/// Represents a block of text that can be printed.
struct Doc {}

impl Doc {
    fn pop_head(&mut self) -> String {
        unimplemented!()
    }

    fn push_head(&mut self, _s: &str) {
        unimplemented!()
    }

    fn pop_tail(&mut self) -> String {
        unimplemented!()
    }

    fn push_tail(&mut self, _s: &str) {
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

struct CachedNamer<'ir, T> {
    names: RefCell<HashMap<T, Rc<str>>>,
    prefixes: RefCell<HashMap<Rc<str>, u32>>,
    f: Box<dyn Fn(T) -> String + 'ir>,
}

impl<'ir, T: Hash + Eq + Clone> CachedNamer<'ir, T> {
    fn new(f: impl Fn(T) -> String + 'ir) -> Self {
        CachedNamer {
            names: RefCell::new(HashMap::new()),
            prefixes: RefCell::new(HashMap::new()),
            f: Box::new(f),
        }
    }

    fn name(&self, id: T) -> Rc<str> {
        if let Some(name) = self.names.borrow().get(&id) {
            return name.clone();
        }
        let name = (*self.f)(id.clone());
        let name = Rc::from(name);
        let name_str: &str = &*name;
        let mut prefix = self.prefixes.borrow_mut();
        let mut names = self.names.borrow_mut();
        if !prefix.contains_key(name_str) {
            prefix.insert(name.clone(), 0);
            let name = if name_str == "" { Rc::from("0") } else { name };
            names.insert(id.clone(), name.clone());
            return name;
        }
        let i = prefix.get_mut(name_str).unwrap();
        *i += 1;
        let new_name: Rc<str> = Rc::from(format!("{}{}", name_str, i));
        names.insert(id.clone(), new_name.clone());
        new_name
    }
}

pub struct IRNamer<'ir> {
    fn_names: CachedNamer<'ir, FuncID>,
    type_prefix_ids: CachedNamer<'ir, TypeDeclID>,
    type_decl_tys: HashMap<TypeID, TypeDeclID>,
    class_prefix_ids: CachedNamer<'ir, ClassID>,
    itf_prefix_ids: CachedNamer<'ir, InterfaceID>,
    var_prefix: CachedNamer<'ir, VarID>,
    global_prefix: CachedNamer<'ir, GlobalConstID>,
}

impl<'ir> IRNamer<'ir> {
    pub fn new(ir: &'ir IR) -> Self {
        IRNamer {
            fn_names: CachedNamer::new(|id: FuncID| ir.get(id).decl.name.get_str(ir).into()),
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
            class_prefix_ids: CachedNamer::new(|id: ClassID| ir.get(id).name.get_str(ir).into()),
            itf_prefix_ids: CachedNamer::new(|id: InterfaceID| ir.get(id).name.get_str(ir).into()),
            var_prefix: CachedNamer::new(|id: VarID| ir.get(id).name.get_str(ir).into()),
            global_prefix: Self::compute_id_name(ir, |id: GlobalConstID| {
                ir.get(ir.get(id).var).name.get_str(ir).into()
            }),
        }
    }

    fn compute_id_name<T: ID>(
        ir: &'ir IR,
        prefix: impl Fn(T) -> String + 'ir,
    ) -> CachedNamer<'ir, T> {
        let names = CachedNamer::new(prefix);
        for id in ir.iter_stable::<T>() {
            names.name(id);
        }
        names
    }

    pub fn name_type_decl(&self, ty: TypeDeclID) -> Rc<str> {
        self.type_prefix_ids.name(ty)
    }

    pub fn try_name_type(&self, ty: TypeID) -> Option<Rc<str>> {
        self.type_decl_tys
            .get(&ty)
            .map(|decl| self.name_type_decl(*decl))
    }

    pub fn name_var(&self, var: VarID) -> Rc<str> {
        self.var_prefix.name(var)
    }

    pub fn name_func(&self, func: FuncID) -> Rc<str> {
        self.fn_names.name(func)
    }

    pub fn name_class(&self, class: ClassID) -> Rc<str> {
        self.class_prefix_ids.name(class)
    }

    pub fn name_interface(&self, itf: InterfaceID) -> Rc<str> {
        self.itf_prefix_ids.name(itf)
    }

    pub fn name_global(&self, global: GlobalConstID) -> Rc<str> {
        self.global_prefix.name(global)
    }
}

pub struct BasicPrinter<'ir> {
    ir_namer: IRNamer<'ir>,
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
            TypeKind::Ptr(tid) => {
                return format!("@ptr{:?}", tid.map(|tid| self.print_type(tid)));
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
            TypeKind::Fn(args, ret) => {
                let args = arg_list("(", ")", ", ", args.iter().map(|ty| self.print_type(*ty)));
                return format!("fn{} -> {}", args, self.print_type(*ret));
            }
            TypeKind::Itf(itf) => {
                return format!("Itf @{}", self.ir_namer.name_interface(*itf));
            }
        }
    }

    pub fn print_type(&self, ty: TypeID) -> String {
        if let Some(decl) = self.ir_namer.try_name_type(ty) {
            return format!("@{}", decl);
        }
        let ty = self.ir.get(ty);
        self.print_tykind(&ty.kind)
    }

    pub fn print_type_decl(&self, ty_id: TypeDeclID) -> String {
        let ty = self.ir.get(ty_id);
        let ty = self.ir.get(ty.decl);
        let decl = self.print_tykind(&ty.kind);
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
            ConstKind::Void => {
                return format!("constant(())");
            }
            ConstKind::I32(i) => {
                return format!("constant({})", i);
            }
            ConstKind::String(s) => {
                return format!("constant(\"{}\")", s.get_str(self.ir).replace("\n", "\\n"));
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

    pub fn print_function(&self, func_id: FuncID, parent: Option<impl AsRef<str>>) -> String {
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
        if let Some(par) = parent {
            let par: &str = par.as_ref();
            format!(
                "fn @{}{}: {} [parent={par}] {{\n {body}\n}}  ",
                prefix, args, ret_ty,
            )
        } else {
            format!("fn @{}{}: {} {{\n {body}\n}}  ", prefix, args, ret_ty,)
        }
    }

    pub fn print_function_stub(&self, func: &FuncDecl, prefix: &str) -> String {
        let args = arg_list(
            "(",
            ")",
            ", ",
            func.args
                .iter()
                .map(|(name, ty)| format!("{}: {}", name.get_str(&self.ir), self.print_type(*ty))),
        );
        let ret_ty = self.print_type(func.ret_ty);
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
            let decl = &self.ir.get(*methods).decl;
            str.push_str(&self.print_function_stub(decl, &decl.name.get_str(self.ir)));
            str += "\n";
        }
        str += "}";
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

    pub fn print_interface(&self, itf_id: InterfaceID) -> String {
        let itf = self.ir.get(itf_id);
        let mut str = format!("interface @{}", self.ir_namer.name_interface(itf_id));
        str += " {\n";
        for methods in itf.methods.iter() {
            str += " ";
            str.push_str(&self.print_function_stub(methods, &methods.name.get_str(self.ir)));
            str += "\n";
        }
        str += "}";
        str
    }

    pub fn print_ir(&self) -> String {
        let mut lines = Vec::new();
        for id in self.ir.iter_stable::<TypeDeclID>() {
            lines.push(self.print_type_decl(id));
        }
        for id in self.ir.iter_stable::<GlobalConstID>() {
            lines.push(self.print_global(id));
        }
        let mut fn_to_class = HashMap::new();
        for id in self.ir.iter_stable::<InterfaceID>() {
            lines.push(self.print_interface(id));
        }
        for id in self.ir.iter_stable::<ClassID>() {
            for method in self.ir.get(id).methods.iter() {
                fn_to_class.insert(*method, id);
            }
            lines.push(self.print_class(id));
        }
        for id in self.ir.iter_stable::<FuncID>() {
            let parent = fn_to_class.get(&id).map(|id| self.ir_namer.name_class(*id));
            lines.push(self.print_function(id, parent));
        }
        lines.push(String::new());
        lines.join("\n")
    }
}
