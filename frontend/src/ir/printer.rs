use std::cell::Cell;
use std::{collections::HashMap, hash::Hash};

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

pub struct BasicPrinter<'ir> {
    fn_prefix_ids: HashMap<FuncID, u32>,
    type_prefix_ids: HashMap<TypeDeclID, u32>,
    class_prefix_ids: HashMap<ClassID, u32>,
    var_prefix: HashMap<VarID, u32>,
    ir: &'ir IR,
}

impl<'ir> BasicPrinter<'ir> {
    pub fn new(ir: &'ir IR) -> Self {
        let mut fn_prefix_ids = HashMap::new();
        let mut type_prefix_ids = HashMap::new();
        let mut class_prefix_ids = HashMap::new();
        let mut var_prefix = HashMap::new();
        for (i, (id, _)) in ir.iter_ids::<FuncID>().enumerate() {
            fn_prefix_ids.insert(id, i as u32);
        }
        for (i, (id, _)) in ir.iter_ids::<TypeDeclID>().enumerate() {
            type_prefix_ids.insert(id, i as u32);
        }
        for (i, (id, _)) in ir.iter_ids::<ClassID>().enumerate() {
            class_prefix_ids.insert(id, i as u32);
        }
        for (i, (id, _)) in ir.iter_ids::<VarID>().enumerate() {
            var_prefix.insert(id, i as u32);
        }
        BasicPrinter {
            fn_prefix_ids,
            type_prefix_ids,
            class_prefix_ids,
            var_prefix,
            ir,
        }
    }

    pub fn print_type(&self, ty: TypeID) -> String {
        let ty = self.ir.get_type(ty).unwrap();
        match &ty.kind {
            TypeKind::Struct { fields } => {
                return arg_list(
                    "{",
                    "}",
                    ", ",
                    fields.iter().map(|(name, ty)| {
                        format!(
                            "{}: {}",
                            self.ir.get_symbol(*name).unwrap().name,
                            self.print_type(*ty)
                        )
                    }),
                );
            }
            TypeKind::Decl { decl } => {
                return format!("@t{}", self.type_prefix_ids[decl]);
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
        let ty = self.ir.get(ty_id).unwrap();
        let decl = self.print_type(ty.decl);
        format!("type @t{} = {}", self.type_prefix_ids[&ty_id], decl)
    }

    pub fn print_var(&self, var_id: VarID) -> String {
        let var = self.ir.get(var_id).unwrap();
        if var.ty.is_none() {
            return format!("%{}", self.var_prefix[&var_id]);
        }
        let ty = self.print_type(var.ty.unwrap());
        format!("%{}: {}", self.var_prefix[&var_id], ty)
    }

    pub fn print_op(&self, op_id: &OPID) -> String {
        let op = self.ir.get(*op_id).unwrap();
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
                return format!("{} = call @f{}{}", self.print_var(op.res.unwrap()), self.fn_prefix_ids[func], arg_list("(", ")", ", ", args.iter().map(|var| {
                    self.print_var(*var)
                })));
            }
            OPKind::GetAttr { obj, attr, idx } => {
                args = vec![*obj];
                name = "getattr";
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
        let args = arg_list("(", ")", ", ", args.iter().map(|var| {
            self.print_var(*var)
        }));
        if let Some(ret) = op.res {
            return format!(
                "{} = {}{}",
                self.print_var(ret),
                name,
                args
            );
        }
        format!("{}{}", name, args)
    }

    pub fn print_function(&self, func_id: FuncID) -> String {
        let func = self.ir.get(func_id).unwrap();
        let args = arg_list("(", ")", ", ", func.vars.iter().map(|var| {
            self.print_var(*var)
        }));
        let ret_ty = self.print_type(func.decl.ret_ty);
        let prefix = self.fn_prefix_ids[&func_id];
        let body = func.body.iter().map(|op| {
            self.print_op(op)
        }).collect::<Vec<_>>().join("\n ");

        format!(
            "fn @f{}{}: {} {{\n {body}\n}}  ",
            prefix,
            args,
            ret_ty,
        )
    }

    pub fn print_function_stub(&self, func: FuncID) -> String {
        let fimpl = self.ir.get(func).unwrap();
        let args = arg_list("(", ")", ", ", fimpl.decl.args.iter().map(|(name, ty)| {
            format!(
                "{}: {}",
                self.ir.get_symbol(*name).unwrap().name,
                self.print_type(*ty)
            )
        }));
        let ret_ty = self.print_type(fimpl.decl.ret_ty);
        let prefix = self.fn_prefix_ids[&func];
        format!(
            "fn @f{}{}: {} {{...}}",
            prefix,
            args,
            ret_ty,
        )
    }

    pub fn print_class(&self, class_id: ClassID) -> String {
        let class = self.ir.get(class_id).unwrap();
        let mut str = format!("class @c{}", self.class_prefix_ids[&class_id]);
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
        for (id, _) in self.ir.iter_ids::<TypeDeclID>() {
            lines.push(self.print_type_decl(id));
        }
        for (id, _) in self.ir.iter_ids::<FuncID>() {
            lines.push(self.print_function(id));
        }
        for (id, _) in self.ir.iter_ids::<ClassID>() {
            lines.push(self.print_class(id));
        }
        lines.join("\n")
    }
}

