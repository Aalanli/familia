use std::{cell::Ref, collections::HashMap};

use anyhow::{Error, Result};
use std::cell::{RefCell, Cell};

use super::ast;

pub struct IR {
    symbols: HashMap<SymbolID, Symbol>,
    symbol_map: HashMap<String, SymbolID>,
    types: RefCell<HashMap<TypeID, Type>>,
    type_map: RefCell<HashMap<TypeKind, TypeID>>,
    vars: HashMap<VarID, Var>,
    funcs: HashMap<FuncID, FuncImpl>,
    ops: HashMap<OPID, OP>,

    i32_id: TypeID,
    void_id: TypeID,
    id: Cell<usize>,
}

impl IR {
    pub fn new() -> Self {
        let mut ir = IR {
            symbols: HashMap::new(),
            symbol_map: HashMap::new(),
            types: RefCell::new(HashMap::new()),
            type_map: RefCell::new(HashMap::new()),
            vars: HashMap::new(),
            funcs: HashMap::new(),
            ops: HashMap::new(),
            // temporary hack
            i32_id: TypeID(NodeID(0)),
            void_id: TypeID(NodeID(0)),
            id: Cell::new(0),
        };
        ir.i32_id = ir.new_type(TypeKind::I32);
        ir.void_id = ir.new_type(TypeKind::Void);

        ir
    }

    fn new_id(&self) -> NodeID {
        let id = self.id.get();
        self.id.set(id + 1);
        NodeID(id)
    }

    pub fn new_symbol(&mut self, name: &str) -> SymbolID {
        if self.symbol_map.contains_key(name) {
            self.symbol_map[name]
        } else {
            let id = SymbolID(self.new_id());
            let symbol = Symbol {
                id: id,
                name: name.to_string(),
            };
            self.symbols.insert(id, symbol);
            self.symbol_map.insert(name.to_string(), id);
            id
        }
    }

    pub fn get_symbol(&self, id: SymbolID) -> &Symbol {
        self.symbols.get(&id).unwrap()
    }

    pub fn new_function(&mut self, func: FuncImpl) -> FuncID {
        let id = FuncID(self.new_id());
        self.funcs.insert(id, func);
        id
    }

    pub fn get_function(&self, id: FuncID) -> &FuncImpl {
        self.funcs.get(&id).unwrap()
    }

    pub fn get_function_mut(&mut self, id: FuncID) -> &mut FuncImpl {
        self.funcs.get_mut(&id).unwrap()
    }

    pub fn new_op(&mut self, kind: OPKind) -> OPID {
        let id = OPID(self.new_id());
        let op = OP {
            id: id,
            kind: kind,
            var: VarID(self.new_id()),
        };
        self.ops.insert(id, op);
        id
    }

    pub fn get_op(&self, id: OPID) -> &OP {
        self.ops.get(&id).unwrap()
    }

    pub fn get_op_var(&self, id: OPID) -> VarID {
        self.get_op(id).var
    }

    pub fn get_op_mut(&mut self, id: OPID) -> &mut OP {
        self.ops.get_mut(&id).unwrap()
    }

    pub fn new_var(&mut self, name: SymbolID, ty: Option<TypeID>) -> VarID {
        let id = VarID(self.new_id());
        let var = Var {
            id: id,
            name: name,
            ty: ty,
        };
        self.vars.insert(id, var);
        id
    }

    pub fn get_var(&self, id: VarID) -> &Var {
        self.vars.get(&id).unwrap()
    }

    pub fn get_var_mut(&mut self, id: VarID) -> &mut Var {
        self.vars.get_mut(&id).unwrap()
    }

    pub fn new_type(&self, kind: TypeKind) -> TypeID {
        if self.type_map.borrow().contains_key(&kind) {
            return self.type_map.borrow()[&kind];
        }
        
        let id = TypeID(self.new_id());
        self.type_map.borrow_mut().insert(kind.clone(), id);
        let ty = Type { id: id, kind: kind };
        self.types.borrow_mut().insert(id, ty);
        id
    }
    
    pub fn get_type(&self, id: TypeID) -> Ref<Type> {
        let a = self.types.borrow();
        let b = Ref::map(a, |a| a.get(&id).unwrap());
        b
    }

    pub fn i32_id(&self) -> TypeID {
        self.i32_id
    }

    pub fn void_id(&self) -> TypeID {
        self.void_id
    }
    
    pub fn types<'a>(&'a self) -> Vec<TypeID> {
        self.types.borrow().keys().cloned().collect()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeID(usize);
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolID(NodeID);

pub struct Symbol {
    pub id: SymbolID,
    pub name: String,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarID(NodeID); // unique variable
pub struct Var {
    pub id: VarID,
    pub name: SymbolID,
    pub ty: Option<TypeID>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeID(NodeID);
pub struct Type {
    pub id: TypeID,
    pub kind: TypeKind,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum TypeKind {
    I32,
    Void,
    Struct { fields: Vec<(SymbolID, TypeID)>, name: Option<SymbolID> },
}

pub struct FuncDecl {
    pub name: SymbolID,
    pub args: Vec<(SymbolID, TypeID)>,
    pub ret_ty: TypeID,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncID(NodeID);
pub struct FuncImpl {
    pub decl: FuncDecl,
    pub vars: Vec<VarID>,
    pub body: Vec<OPID>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct OPID(NodeID);

#[derive(Clone)]
pub struct OP {
    pub id: OPID,
    pub kind: OPKind,
    pub var: VarID,
}

#[derive(Clone)]
pub enum OPKind {
    GetAttr {
        obj: VarID,
        attr: SymbolID,
        idx: Option<usize>,
    },
    Call {
        func: FuncID,
        args: Vec<VarID>,
    },
    Add {
        lhs: VarID,
        rhs: VarID,
    },
    Struct {
        fields: Vec<(SymbolID, VarID)>,
    },
    Return {
        value: VarID,
    },
    Constant {
        value: i32,
    },
}

struct VarScope {
    vars: Vec<HashMap<SymbolID, VarID>>,
}

impl VarScope {
    fn new() -> Self {
        VarScope { vars: vec![] }
    }

    fn with_new_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.vars.push(HashMap::new());
        let t = f(self);
        self.vars.pop();
        t
    }

    fn insert(&mut self, symbol: SymbolID, var: VarID) {
        self.vars.last_mut().unwrap().insert(symbol, var);
    }

    fn get(&self, symbol: SymbolID) -> Option<VarID> {
        for scope in self.vars.iter().rev() {
            if let Some(var) = scope.get(&symbol) {
                return Some(*var);
            }
        }
        None
    }
}

struct AstToIR {
    ir: IR,
    symbol_map: HashMap<ast::Symbol, NodeID>,
}

impl AstToIR {
    pub fn new() -> Self {
        AstToIR {
            ir: IR::new(),
            symbol_map: HashMap::new(),
        }
    }

    pub fn ast(&mut self, ast: &ast::AST) -> Result<()> {
        for decl in ast.decls.iter() {
            match decl {
                ast::Decl::TypeDecl { name, decl } => {
                    self.symbol_map.insert(name.clone(), self.ir.new_id());
                }
                ast::Decl::FnDecl { name, args, ty } => {
                    return Err(Error::msg("no fn decl in global scope"));
                }
                ast::Decl::FnImpl {
                    name,
                    args,
                    ty,
                    body,
                } => {
                    self.symbol_map.insert(name.clone(), self.ir.new_id());
                }
            }
        }
        for decl in ast.decls.iter() {
            match decl {
                ast::Decl::TypeDecl { name, decl } => {
                    self.type_decl(name, decl)?;
                }
                ast::Decl::FnDecl { name, args, ty } => {
                    return Err(Error::msg("no fn decl in global scope"));
                }
                _ => {}
            }
        }

        for decl in ast.decls.iter() {
            match decl {
                ast::Decl::FnImpl {
                    name,
                    args,
                    ty,
                    body,
                } => {
                    self.fn_impl(name, args, ty, body)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn convert_ty(&mut self, ty: &ast::Type) -> Result<TypeKind> {
        let ty = match ty {
            ast::Type::I32 => TypeKind::I32,
            ast::Type::Void => TypeKind::Void,
            ast::Type::Symbol(s) => {
                panic!("unresolved symbol");
            }
            ast::Type::Struct { fields } => {
                let fields = fields
                    .iter()
                    .map(|(name, ty)| {
                        let name = self.intern_symbol(name);

                        let ty = match ty {
                            ast::Type::I32 => self.ir.new_type(TypeKind::I32),
                            ast::Type::Void => self.ir.new_type(TypeKind::Void),
                            ast::Type::Symbol(s) => TypeID(self.get_symbol(s)?),
                            // struct type
                            st_ty => {
                                let ty = self.convert_ty(st_ty)?;
                                self.ir.new_type(ty)
                            }
                        };
                        Ok((name, ty))
                    })
                    .collect::<Result<Vec<_>>>()?;
                TypeKind::Struct { fields, name: None }
            }
        };
        Ok(ty)
    }

    pub fn get_type_id(&mut self, ty: &ast::Type) -> Result<TypeID> {
        if let ast::Type::Symbol(s) = ty {
            return Ok(TypeID(self.get_symbol(s)?));
        }
        let ty = self.convert_ty(ty)?;
        Ok(self.ir.new_type(ty))
    }

    pub fn type_decl(&mut self, name: &ast::Symbol, decl: &ast::Type) -> Result<()> {
        let id = self.get_symbol(name)?;
        // remap type alias to use original type id
        if let ast::Type::Symbol(s) = decl {
            let sid = self.get_symbol(s)?;
            self.symbol_map.insert(name.clone(), sid);
            return Ok(());
        }
        let mut ty = self.convert_ty(decl)?;
        if let TypeKind::Struct { name: id_name, .. } = &mut ty {
            *id_name = Some(self.intern_symbol(name));
        }
        

        // TODO: perhaps refactor to inside IR impl block
        let tid = TypeID(id);
        self.ir.type_map.borrow_mut().insert(ty.clone(), tid);
        self.ir.types.borrow_mut().insert(tid, Type { id: tid, kind: ty });

        Ok(())
    }

    pub fn fn_impl(
        &mut self,
        name: &ast::Symbol,
        args: &Vec<ast::Var>,
        ty: &ast::Type,
        body: &[ast::Stmt],
    ) -> Result<()> {
        let fn_id = FuncID(self.get_symbol(name)?);

        let mut new_vars = vec![];
        let mut var_scope = VarScope::new();

        let body = var_scope.with_new_scope(|scope| {
            for v in args.iter() {
                if v.ty.is_none() {
                    return Err(Error::msg("no type for arg"));
                }
                let s = self.intern_symbol(&v.name);
                let ty = self.get_type_id(ty)?;
                let var_id = self.ir.new_var(s, Some(ty));
                scope.insert(s, var_id);
                new_vars.push(var_id);
            }
            self.stmt(scope, body)
        })?;

        let func = FuncImpl {
            decl: FuncDecl {
                name: self.intern_symbol(name),
                args: args
                    .iter()
                    .map(|v| {
                        let s = self.intern_symbol(&v.name);
                        let ty = self.get_type_id(v.ty.as_ref().unwrap()).unwrap();
                        (s, ty)
                    })
                    .collect(),
                ret_ty: self.get_type_id(ty)?,
            },
            vars: new_vars,
            body,
        };

        self.ir.funcs.insert(fn_id, func);
        Ok(())
    }

    pub fn stmt(&mut self, var_scope: &mut VarScope, stmt: &[ast::Stmt]) -> Result<Vec<OPID>> {
        let mut ops = vec![];
        for stmt in stmt.iter() {
            match stmt {
                ast::Stmt::LetStmt { var, expr } => {
                    let s = self.intern_symbol(&var.name);
                    let ty = var.ty.as_ref().map(|ty| self.get_type_id(ty)).transpose()?;

                    let var_id = self.ir.new_var(s, ty);

                    self.expr(var_scope, expr, &mut ops)?;
                    // bind the last op to the variable
                    self.ir.get_op_mut(*ops.last().unwrap()).var = var_id;
                    // corresponds to ast::Expr::Var
                    var_scope.insert(s, var_id);
                }
                ast::Stmt::ExprStmt { expr } => {
                    self.expr(var_scope, expr, &mut ops)?;
                }
                ast::Stmt::ReturnStmt { expr } => {
                    self.expr(var_scope, expr, &mut ops)?;
                }
            }
        }
        Ok(ops)
    }

    pub fn expr(
        &mut self,
        var_scope: &mut VarScope,
        expr: &ast::Expr,
        ops: &mut Vec<OPID>,
    ) -> Result<VarID> {
        match expr {
            ast::Expr::Var(v) => {
                let s = self.intern_symbol(&v.name);
                let var = var_scope.get(s).ok_or(Error::msg("unbound variable"))?;
                Ok(var)
            }
            ast::Expr::IntLit(i) => {
                let op = self.ir.new_op(OPKind::Constant { value: *i });
                ops.push(op);
                Ok(self.ir.get_op_var(op))
            }
            ast::Expr::GetAttr { exp, sym } => {
                let exp = self.expr(var_scope, exp, ops)?;
                let sym = self.intern_symbol(sym);
                let op = self.ir.new_op(OPKind::GetAttr {
                    obj: exp,
                    attr: sym,
                    idx: None,
                });
                ops.push(op);
                Ok(self.ir.get_op_var(op))
            }
            ast::Expr::Add { lhs, rhs } => {
                let lhs = self.expr(var_scope, lhs, ops)?;
                let rhs = self.expr(var_scope, rhs, ops)?;
                let op = self.ir.new_op(OPKind::Add { lhs, rhs });
                ops.push(op);
                Ok(self.ir.get_op_var(op))
            }
            ast::Expr::Call { symbol, args } => {
                let fn_id = self.get_symbol(symbol)?;

                let args = args
                    .iter()
                    .map(|arg| self.expr(var_scope, arg, ops))
                    .collect::<Result<Vec<_>>>()?;
                let op = self.ir.new_op(OPKind::Call {
                    func: FuncID(fn_id),
                    args,
                });
                ops.push(op);
                Ok(self.ir.get_op_var(op))
            }
            ast::Expr::Struct { args } => {
                let args = args
                    .iter()
                    .map(|(sym, expr)| {
                        let sym = self.intern_symbol(sym);
                        let expr = self.expr(var_scope, expr, ops)?;
                        Ok((sym, expr))
                    })
                    .collect::<Result<Vec<_>>>()?;
                let op = self.ir.new_op(OPKind::Struct { fields: args });
                ops.push(op);
                Ok(self.ir.get_op_var(op))
            }
        }
    }

    fn get_symbol(&self, symbol: &ast::Symbol) -> Result<NodeID> {
        self.symbol_map
            .get(symbol)
            .ok_or(Error::msg("unbound symbol"))
            .cloned()
    }

    pub fn intern_symbol(&mut self, symbol: &ast::Symbol) -> SymbolID {
        return self.ir.new_symbol(symbol.view());
    }
}

pub fn typecheck(ir: &mut IR) -> Result<()> {
    let mut var_types = ir.vars.iter().filter(|(id, var)| {var.ty.is_some()}).map(|(id, var)| (*id, var.ty.unwrap())).collect::<HashMap<_, _>>();

    for func_id in ir.funcs.keys() {
        let func = ir.get_function(*func_id);
        let ret_ty = func.decl.ret_ty;
        for op_id in func.body.iter() {
            let op = ir.get_op(*op_id).clone();
            match &op.kind {
                OPKind::Add { lhs, rhs } => {
                    if var_types[lhs] != ir.i32_id() {
                        return Err(Error::msg("type mismatch, not i32"));
                    }
                    if var_types[rhs] != ir.i32_id() {
                        return Err(Error::msg("type mismatch, not i32"));
                    }
                    var_types.insert(op.var, ir.i32_id());
                }
                OPKind::Constant { value } => {
                    if ir.get_type(ret_ty).kind != TypeKind::I32 {
                        return Err(Error::msg("type mismatch"));
                    }
                    var_types.insert(op.var, ir.i32_id());
                }
                OPKind::Call { func, args } => {
                    let func = ir.get_function(*func);
                    for (arg, ty) in func.decl.args.iter().zip(args.iter()) {
                        if var_types[ty] != ir.get_type(arg.1).id {
                            return Err(Error::msg("type mismatch"));
                        }
                    }
                    var_types.insert(op.var, ret_ty);
                }
                OPKind::GetAttr { obj, attr, idx } => {
                    let obj_ty = var_types[obj];
                    let obj_ty = ir.get_type(obj_ty);
                    if let TypeKind::Struct { fields, .. } = &obj_ty.kind {
                        let field_ty = fields.iter().find(|(sym, _)| sym == attr).ok_or(Error::msg("field not found"))?.1;
                        var_types.insert(op.var, field_ty);
                    } else {
                        return Err(Error::msg("type mismatch"));
                    }
                }
                OPKind::Return { value } => {
                    let value_ty = var_types[value];
                    if value_ty != ret_ty {
                        return Err(Error::msg("type mismatch"));
                    }
                }
                OPKind::Struct { fields } => {
                    let struct_ty = TypeKind::Struct { fields: fields.iter().map(|(sym, ty)| (*sym, var_types[ty])).collect(), name: None };
                    let ty = ir.new_type(struct_ty);
                    if let Some(var_ty) = var_types.get(&op.var) {
                        if *var_ty != ty {
                            return Err(Error::msg("type mismatch"));
                        }
                    } else {
                        var_types.insert(op.var, ty);
                    }
                }
            }
        }
    }
    Ok(())
}

use crate::printer::{Doc, text, lines};

struct IRPrinter<'ir> {
    ir: &'ir IR,
    var_prefixes: HashMap<&'ir str, u32>,
    // symbol_prefixes: HashMap<&'ir str, u32>,
}

impl<'ir> IRPrinter<'ir> {
    pub fn new(ir: &'ir IR) -> Self {
        IRPrinter { ir, var_prefixes: HashMap::new() }
    }

    fn name_var(&mut self, var: VarID) -> Doc {
        let var = self.ir.get_var(var);
        let symbol = self.ir.get_symbol(var.name);
        
        let prefix = *self.var_prefixes.get(&*symbol.name).unwrap_or(&0);
        self.var_prefixes.insert(&*symbol.name, prefix + 1);
        text(format!("{}{}", symbol.name, prefix))
    }

    
}