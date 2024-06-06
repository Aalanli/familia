use std::collections::HashMap;

use anyhow::{Error, Result};

use super::ast;

pub struct IR {
    symbols: HashMap<SymbolID, Symbol>,
    symbol_map: HashMap<String, SymbolID>,
    types: HashMap<TypeID, Type>,
    type_map: HashMap<TypeKind, TypeID>,
    vars: HashMap<VarID, Var>,
    funcs: HashMap<FuncID, FuncImpl>,
    ops: HashMap<OPID, OP>,

    id: usize,
}

impl IR {
    pub fn new() -> Self {
        let ir = IR {
            symbols: HashMap::new(),
            symbol_map: HashMap::new(),
            types: HashMap::new(),
            type_map: HashMap::new(),
            vars: HashMap::new(),
            funcs: HashMap::new(),
            ops: HashMap::new(),
            id: 0,
        };

        ir
    }

    fn new_id(&mut self) -> NodeID {
        let id = self.id;
        self.id += 1;
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

    pub fn new_type(&mut self, kind: TypeKind) -> TypeID {
        if self.type_map.contains_key(&kind) {
            return self.type_map[&kind];
        }

        let id = TypeID(self.new_id());
        self.type_map.insert(kind.clone(), id);
        let ty = Type { id: id, kind: kind };
        self.types.insert(id, ty);
        id
    }

    pub fn get_type(&self, id: TypeID) -> &Type {
        self.types.get(&id).unwrap()
    }

    pub fn get_type_mut(&mut self, id: TypeID) -> &mut Type {
        self.types.get_mut(&id).unwrap()
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
    Struct { fields: Vec<(SymbolID, TypeID)> },
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
    pub body: Vec<OPID>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct OPID(NodeID);
pub struct OP {
    pub id: OPID,
    pub kind: OPKind,
    pub var: VarID,
}

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

    fn with_new_scope(&mut self, f: impl FnOnce(&mut Self)) {
        self.vars.push(HashMap::new());
        f(self);
        self.vars.pop();
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
                ast::Decl::FnImpl {
                    name,
                    args,
                    ty,
                    body,
                } => {
                    self.fn_impl(name, args, ty, body)?;
                }
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
                            ty => {
                                let ty = self.convert_ty(ty)?;
                                self.ir.new_type(ty)
                            }
                        };
                        Ok((name, ty))
                    })
                    .collect::<Result<Vec<_>>>()?;
                TypeKind::Struct { fields }
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
        let ty = self.convert_ty(decl)?;

        // TODO: perhaps refactor to inside IR impl block
        let tid = TypeID(id);
        self.ir.type_map.insert(ty.clone(), tid);
        self.ir.types.insert(tid, Type { id: tid, kind: ty });

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
        for v in args.iter() {
            if v.ty.is_none() {
                return Err(Error::msg("no type for arg"));
            }
            let s = self.intern_symbol(&v.name);
            let ty = self.get_type_id(ty)?;
            let var_id = self.ir.new_var(s, Some(ty));
            new_vars.push(var_id);
        }

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

use crate::printer::{Doc, text, lines};

struct IRPrinter {}

impl IRPrinter {
    pub fn new() -> Self {
        IRPrinter {}
    }

    pub fn ir(&self, ir: &IR) -> Doc {
        // let mut doc = Doc::text("IR\n");
        // for (id, symbol) in ir.symbols.iter() {
        //     doc = doc.concat(Doc::text(&format!("{}: {}\n", id.0, symbol.name)));
        // }
        // doc
        todo!()
    }
}
