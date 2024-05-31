# %%
from dataclasses import dataclass
from enum import Enum
from typing import List, Union, TypeVar, Generic, Optional, Dict, Any, Tuple, cast, Callable
from lark import Lark, Tree

class Doc:
    def indent(self) -> 'Doc':
        return Indent(self)

    def __mul__(self, other: 'Doc') -> 'Doc':
        return Concat([self, other])
    
    def __add__(self, other: 'Doc') -> 'Doc':
        return Lines([self, other])

    def to_str(self, indent: int = 0) -> str:
        raise NotImplementedError()

class Lines(Doc):
    def __init__(self, lines: List[Doc]):
        self.lines = lines
    
    def to_str(self, indent: int = 0) -> str:
        return '\n'.join([l.to_str(indent) for l in self.lines])
    
class Text(Doc):
    def __init__(self, text: str):
        assert '\n' not in text
        self.text = text
    
    def to_str(self, indent: int = 0) -> str:
        return ' ' * indent + self.text

class Indent(Doc):
    def __init__(self, doc: Doc):
        self.doc = doc
    
    def to_str(self, indent: int = 0) -> str:
        return self.doc.to_str(indent + 2)

class Concat(Doc):
    def __init__(self, docs: List[Doc]):
        self.docs = docs
    
    def to_str(self, indent: int = 0) -> str:
        if len(self.docs) > 0:
            return self.docs[0].to_str(indent) +\
                  ''.join([d.to_str(0) for d in self.docs[1:]])
        return ''

def join_doc(docs: List[Doc], sep: str) -> Doc:
    if len(docs) == 0:
        return Text('')
    return docs[0] * Concat([Text(sep) * d for d in docs[1:]])


class DocOptions:
    def __init__(self, show_link: bool = False):
        self.show_link = show_link


class Transformer:
    def __init__(self, memo: bool = True):
        self.memo = memo
        self.memo_map: Dict[int, AST] = {}
        # self.func_map: Dict[Any, Callable] = {}

    def __call__(self, node: 'AST') -> 'AST':
        if self.memo and id(node) in self.memo_map:
            return self.memo_map[id(node)]
        method_name = f'visit_{node.__class__.__name__}'
        if hasattr(self, method_name):
            method = getattr(self, method_name)
            new_node = method(node)
            assert isinstance(new_node, AST)
        else:
            new_node = node.transform(self)
        if self.memo:
            self.memo_map[id(node)] = new_node
        return new_node

class Visitor:
    def __call__(self, node: 'AST'):
        assert isinstance(node, AST), "expected AST node, got " + str(type(node))
        method_name = f'visit_{node.__class__.__name__}'
        if hasattr(self, method_name):
            method = getattr(self, method_name)
            method(node)
        else:
            node.visit(self)



class AST:
    def __str__(self) -> str:
        return self.doc(DocOptions()).to_str(0)
    
    def doc(self, opt: DocOptions) -> Doc:
        raise NotImplementedError()
    
    def visit(self, visitor: Visitor):
        raise NotImplementedError()

    def transform(self, visitor: Transformer) -> 'AST':
        raise NotImplementedError()

class Symbol(AST):
    def __init__(self, names: Union[str, Tuple[str, ...]]):
        self.names: Tuple[str, ...] = (names,) if isinstance(names, str) else names

    def doc(self, opt: DocOptions) -> Doc:
        return Text('@' + '::'.join(self.names))
    
    def __hash__(self) -> int:
        return hash(self.names)

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Symbol) and self.names == other.names
    
    def visit(self, visitor: Visitor):
        pass

    def transform(self, visitor: Transformer) -> 'Symbol':
        return self
    

class Type(AST):
    def __hash__(self) -> int:
        return hash(str(self))
    
    def __eq__(self, other: object) -> bool:
        return isinstance(other, Type) and hash(self) == hash(other)
    
    def visit(self, visitor: Visitor):
        pass

    def transform(self, visitor: Transformer) -> 'Type':
        return self

class I32(Type):
    def doc(self, opt: DocOptions) -> Doc:
        return Text('i32')

class This(Type):
    def doc(self, opt: DocOptions) -> Doc:
        return Text('This')

class Self(Type):
    def doc(self, opt: DocOptions) -> Doc:
        return Text('Self')

class Void(Type):
    def doc(self, opt: DocOptions) -> Doc:
        return Text('()')

class SymbolType(Type):
    def __init__(self, sym: Symbol):
        self.symbol = sym
    
    def doc(self, opt: DocOptions) -> Doc:
        return Text(str(self.symbol))
    
    def __hash__(self) -> int:
        return hash(self.symbol)

class StructType(Type):
    def __init__(self, fields: Dict[str, Type]):
        self.fields = fields
    
    def doc(self, opt: DocOptions) -> Doc:
        return Text('{') * join_doc([Text(f'{f}:') * ty.doc(opt) for f, ty in self.fields.items()], ', ') * Text('}')
    
    def visit(self, visitor: Visitor):
        for _, f in self.fields.items():
            f.visit(visitor)
    
    def transform(self, visitor: Transformer) -> 'StructType':
        new_fields = {}
        for f, ty in self.fields.items():
            new_ty = visitor(ty)
            assert isinstance(new_ty, Type)
            new_fields[f] = new_ty
        return StructType(new_fields)

i32 = I32()
this_t = This()
self_t = Self()
void_t = Void()

class VarKind(Enum):
    THIS = 0
    SELF = 1
    LOCAL = 2

class Var(AST):
    def __init__(self, name: str, ty: Optional[Type] = None, kind = VarKind.LOCAL):
        self.name = name
        self.ty = ty
        self.kind = kind
    
    def doc(self, opt: DocOptions) -> Doc:
        if opt.show_link and self.ty is None:
            return Text(f'(?ty {self.name})')
        return Text(self.name)
    
    def visit(self, visitor: Visitor):
        if self.ty is not None:
            self.ty.visit(visitor)
    
    def transform(self, visitor: Transformer) -> 'Var':
        if self.ty is not None:
            ty = visitor(self.ty)
            assert isinstance(ty, Type)
            return Var(self.name, ty)
        return self


class Program(AST):
    def __init__(self, body: list[AST]):
        self.body = body
    
    def doc(self, opt: DocOptions) -> Doc:
        return Lines([b.doc(opt) for b in self.body])
    
    def transform(self, visitor: Transformer) -> 'Program':
        return Program([visitor(b) for b in self.body])
    
    def visit(self, visitor: Visitor):
        for b in self.body:
            visitor(b)
        

class TypeDef(AST):
    def __init__(self, name: Symbol, ty: StructType):
        self.name = name
        self.type = ty

    def doc(self, opt: DocOptions) -> Doc:
        return Text(f'type {self.name} = ') * self.type.doc(opt)

    def transform(self, visitor: Transformer) -> 'TypeDef':
        new_name = visitor(self.name)
        assert isinstance(new_name, Symbol)
        new_ty = visitor(self.type)
        assert isinstance(new_ty, StructType)
        return TypeDef(new_name, new_ty)
        
    def visit(self, visitor: Visitor):
        visitor(self.name)
        visitor(self.type)


class FuncDecl(AST):
    def __init__(self, name: Symbol, args: list[Var], ret: Type):
        assert isinstance(ret, Type)
        self.name = name
        self.args = args
        self.ret = ret
    
    def doc(self, opt: DocOptions) -> Doc:
        def show_ty_doc(x: Var) -> Doc:
            return x.doc(opt) * Text(': ') * x.ty.doc(opt) if x.ty is not None else x.doc(opt)

        return Text(f'fn {self.name}(') * \
            join_doc([show_ty_doc(a) for a in self.args], ', ') * Text(f'): {self.ret}')
        
    def transform(self, visitor: Transformer) -> 'FuncDecl':
        new_name = visitor(self.name)
        assert isinstance(new_name, Symbol)
        args = []
        for a in self.args:
            x = visitor(a)
            assert isinstance(x, Var)
            args.append(x)
        ret = visitor(self.ret)
        assert isinstance(ret, Type)
        return FuncDecl(new_name, args, ret)
    
    def visit(self, visitor: Visitor):
        visitor(self.name)
        for a in self.args:
            visitor(a)
        visitor(self.ret)


class FuncDef(AST):
    def __init__(self, fn_decl: FuncDecl, body: list[AST]):
        self.fn_decl = fn_decl
        self.body = body
    
    def doc(self, opt: DocOptions) -> Doc:
        return (self.fn_decl.doc(opt) * Text(' {')) + Lines([b.doc(opt) for b in self.body]).indent() + Text('}')
    
    def transform(self, visitor: Transformer) -> 'FuncDef':
        decl = visitor(self.fn_decl)
        assert isinstance(decl, FuncDecl)
        return FuncDef(decl, [visitor(b) for b in self.body])

    def visit(self, visitor: Visitor):
        visitor(self.fn_decl)
        for b in self.body:
            visitor(b)


class ClassDef(AST):
    def __init__(self, name: Symbol, interface: Symbol, body: list[AST]):
        self.name = name
        self.interface = interface
        self.body = body
    
    def doc(self, opt: DocOptions) -> Doc:
        return Text(f'class {self.name} for {self.interface} ' + '{') + Lines([b.doc(opt) for b in self.body]).indent() + Text('}') 
    
    def transform(self, visitor: Transformer) -> 'ClassDef':
        new_name = visitor(self.name)
        assert isinstance(new_name, Symbol)
        new_interface = visitor(self.interface)
        assert isinstance(new_interface, Symbol)
        return ClassDef(new_name, new_interface, [visitor(b) for b in self.body])
    
    def visit(self, visitor: Visitor):
        visitor(self.name)
        visitor(self.interface)
        for b in self.body:
            visitor(b)
    
    def get_method(self, name: str) -> Optional[FuncDef]:
        for b in self.body:
            if isinstance(b, FuncDef) and b.fn_decl.name == name:
                return b
        return None
    

class InterfaceDef(AST):
    def __init__(self, name: Symbol, body: list[AST]):
        self.name = name
        self.body = body
    
    def doc(self, opt: DocOptions) -> Doc:
        return Text(f'interface {self.name} ' + '{') + Lines([b.doc(opt) for b in self.body]).indent() + Text('}')
    
    def transform(self, visitor: Transformer) -> 'InterfaceDef':
        new_name = visitor(self.name)
        assert isinstance(new_name, Symbol)
        return InterfaceDef(new_name, [visitor(b) for b in self.body])
    
    def visit(self, visitor: Visitor):
        visitor(self.name)
        for b in self.body:
            visitor(b)


class ExprStmt(AST):
    def __init__(self, expr: AST):
        self.expr = expr

    def doc(self, opt: DocOptions) -> Doc:
        return self.expr.doc(opt) * Text(';')
    
    def transform(self, visitor: Transformer) -> 'ExprStmt':
        return ExprStmt(visitor(self.expr))

    def visit(self, visitor: Visitor):
        visitor(self.expr)


class LetStmt(AST):
    def __init__(self, value: Var, expr: AST):
        self.value = value
        self.expr = expr

    def doc(self, opt: DocOptions) -> Doc:
        def show_ty_doc(x: Var) -> Doc:
            return x.doc(opt) * Text(': ') * x.ty.doc(opt) if x.ty is not None else x.doc(opt)
        return Text(f'let ') * show_ty_doc(self.value) * Text(' = ') * self.expr.doc(opt) * Text(';')
    
    def transform(self, visitor: Transformer) -> 'LetStmt':
        var = visitor(self.value)
        assert isinstance(var, Var)
        return LetStmt(var, visitor(self.expr))
    
    def visit(self, visitor: Visitor):
        visitor(self.value)
        visitor(self.expr)


class ReturnStmt(AST):
    def __init__(self, expr: AST):
        self.expr = expr
    
    def doc(self, opt: DocOptions) -> Doc:
        return Text('return ') * self.expr.doc(opt) * Text(';')
    
    def transform(self, visitor: Transformer) -> 'ReturnStmt':
        return ReturnStmt(visitor(self.expr))
    
    def visit(self, visitor: Visitor):
        visitor(self.expr)        


class GetAttrExpr(AST):
    def __init__(self, obj: AST, attr: Symbol):
        self.obj = obj
        self.attr = attr

    def doc(self, opt: DocOptions) -> Doc:
        return self.obj.doc(opt) * Text(f'.{self.attr}')

    def transform(self, visitor: Transformer) -> 'GetAttrExpr':
        obj = visitor(self.obj)
        attr = visitor(self.attr)
        assert isinstance(attr, Symbol)

        return GetAttrExpr(obj, attr)
    
    def visit(self, visitor: Visitor):
        visitor(self.obj)
        visitor(self.attr)

class CallExpr(AST):
    def __init__(self, fn: AST, args: list[AST]):
        self.fn = fn
        self.args = args
    
    def doc(self, opt: DocOptions) -> Doc:
        inner = [a.doc(opt) for a in self.args]
        return self.fn.doc(opt) * Text('(') * Concat(inner) * Text(')')
    
    def transform(self, visitor: Transformer) -> 'CallExpr':
        return CallExpr(visitor(self.fn), [visitor(a) for a in self.args])
    
    def visit(self, visitor: Visitor):
        visitor(self.fn)
        for a in self.args:
            visitor(a)

# class MethodCallExpr(AST):
#     def __init__(self, obj: AST, method: Symbol, args: list[AST]):
#         self.obj = obj
#         self.method = method
#         self.args = args
    
#     def doc(self, opt: DocOptions) -> Doc:
#         inner = [a.doc(opt) for a in self.args]
#         return self.obj.doc(opt) * Text('.') * self.method.doc(opt) * Text('(') * Concat(inner) * Text(')')
    
#     def transform(self, visitor: Transformer) -> 'MethodCallExpr':
#         return MethodCallExpr(
#             visitor(self.obj), 
#             visitor(self.method), 
#             [visitor(a) for a in self.args]
#         )
    
#     def visit(self, visitor: Visitor):
#         visitor(self.obj)
#         visitor(self.method)
#         for a in self.args:
#             visitor(a)

class IntExpr(AST):
    def __init__(self, value: int):
        self.value = value

    def __str__(self):
        return str(self.value)
    
    def doc(self, opt: DocOptions) -> Doc:
        return Text(str(self.value))

    def transform(self, visitor: Transformer) -> AST:
        return self
    
    def visit(self, visitor: Visitor):
        pass

class BinOpExpr(AST):
    def __init__(self, op: str, lhs: AST, rhs: AST):
        self.op = op
        self.lhs = lhs
        self.rhs = rhs

    def doc(self, opt: DocOptions) -> Doc:
        return Text('(') * self.lhs.doc(opt) * Text(f' {self.op} ') * self.rhs.doc(opt) * Text(')')
    
    def transform(self, visitor: Transformer) -> 'BinOpExpr':
        return BinOpExpr(self.op, visitor(self.lhs), visitor(self.rhs))
    
    def visit(self, visitor: Visitor):
        visitor(self.lhs)
        visitor(self.rhs)

class ConstructorExpr(AST):
    def __init__(self, args: list[tuple[str, AST]]):
        self.args = args

    def __str__(self):
        arg_list = ', '.join([f'{name}: {expr}' for name, expr in self.args])
        return '{' + arg_list + '}'
    
    def doc(self, opt: DocOptions) -> Doc:
        inner = [Text(f'{name}: ') * expr.doc(opt) for name, expr in self.args]
        return Text('{') * join_doc(inner, ', ') * Text('}')
    
    def transform(self, visitor: Transformer) -> 'ConstructorExpr':
        return ConstructorExpr([(name, visitor(expr)) for name, expr in self.args])
    
    def visit(self, visitor: Visitor):
        for name, expr in self.args:
            visitor(expr)

class ParseTree:
    def visit(self, node: Tree) -> AST:
        method_name = f'visit_{str(node.data)}'
        if hasattr(self, method_name):
            method = getattr(self, method_name)
            return method(node)
        else:
            raise NotImplementedError(f'visit_{str(node.data)} not implemented')
    
    type_dispatch = {
        'i32': i32,
        'this_t': this_t,
        'self_t': self_t,
        '()': void_t
    }
    @staticmethod
    def visit_type(t: Tree) -> Type:
        # print(type(t), t)
        if isinstance(t, Tree) and t.data == 'custom_type':
            return SymbolType(Symbol(str(t.children[0])))
        s = t.data if isinstance(t, Tree) else str(t)
        if s in ParseTree.type_dispatch:
            return ParseTree.type_dispatch[s]
        raise NotImplementedError(f'Unknown type {s}')

    def visit_arg(self, node: Tree) -> Var:
        if str(node.data) == 'this_arg':
            return Var("this", this_t, VarKind.THIS)
        name = str(node.children[0])
        ty = ParseTree.visit_type(node.children[1])
        var = Var(name, ty)
        return var
    
    def visit_var(self, node: Tree) -> Var:
        name = str(node.children[0])
        return Var(name)
    
    def visit_number(self, node: Tree) -> IntExpr:
        return IntExpr(int(str(node.children[0])))

    def visit_self(self, node: Tree) -> Var:
        return Var("self", self_t, VarKind.SELF) # technically self refers to the class, thus not a type
    
    def visit_this(self, node: Tree) -> Var:
        return Var("this", this_t, VarKind.THIS)

    def visit_function_decl(self, node: Tree) -> FuncDecl:
        children = node.children
        name = Symbol(str(children[0]))
        args = []
        for c in children[1:-1]:
            if len(c.children) > 0 or c.data == 'this_arg':
                args.append(self.visit_arg(c))
        ret_ty = ParseTree.visit_type((children[-1])) if children[-1] is not None else void_t
        return FuncDecl(name, args, ret_ty)

    def visit_function_def(self, node: Tree) -> FuncDef:
        children = node.children
        decl = self.visit_function_decl(children[0])
        body = [self.visit(c) for c in children[1:]]
        assert all(isinstance(b, AST) for b in body), body
        return FuncDef(decl, cast(list[AST], body))
    
    def visit_expr_stmt(self, node: Tree) -> ExprStmt:
        expr = self.visit(node.children[0])
        return ExprStmt(expr)
    
    def visit_let_stmt(self, node: Tree) -> LetStmt:
        children = node.children
        if isinstance(children[0], Tree):
            var = self.visit_arg(children[0])
        else:
            var = Var(str(children[0]))
        
        expr = self.visit(children[1])
        return LetStmt(var, expr)
    
    def visit_return_stmt(self, node: Tree) -> ReturnStmt:
        expr = self.visit(node.children[0])
        return ReturnStmt(expr)
    
    def visit_bin_op_expr(self, node: Tree) -> BinOpExpr:
        children = node.children
        lhs = self.visit(children[0])
        rhs = self.visit(children[2])
        return BinOpExpr(str(children[1].data), lhs, rhs)
    
    def visit_call_expr(self, node: Tree) -> CallExpr:
        children = node.children
        fn = self.visit(children[0])
        args = [self.visit(c) for c in children[1:] if c.data != 'expr']
        return CallExpr(fn, args)

    def visit_constructor_expr(self, node: Tree) -> ConstructorExpr:
        children = node.children
        args = []
        for i in range(0, len(children), 2):
            name = str(children[i])
            expr = self.visit(children[i+1])
            args.append((name, expr))
        return ConstructorExpr(args)
    
    def visit_get_attr(self, node: Tree) -> GetAttrExpr:
        children = node.children
        obj = self.visit(children[0])
        attr = str(children[1])
        return GetAttrExpr(obj, Symbol(attr))
    
    def visit_class_def(self, node: Tree) -> ClassDef:
        children = node.children
        name = Symbol(str(children[0]))
        cfor = Symbol(str(children[1]))
        body = [self.visit(c) for c in children[2:]]
        return ClassDef(name, cfor, body)
    
    def visit_interface_def(self, node: Tree) -> InterfaceDef:
        children = node.children
        name = Symbol(str(children[0]))
        body = [self.visit(c) for c in children[1:]]
        return InterfaceDef(name, body)

    def visit_typedef(self, node: Tree) -> TypeDef:
        children = node.children
        name = str(children[0])
        new_fields = {}
        for f in [self.visit(c) for c in children[1:]]:
            assert isinstance(f, Var)
            assert f.ty is not None
            new_fields[f.name] = f.ty
        return TypeDef(Symbol(name), StructType(new_fields))
    
    def visit_program(self, node: Tree) -> Program:
        assert node.data == 'program'
        children = node.children
        return Program([self.visit(c) for c in children])


class CanonicalClassDefn(AST):
    def __init__(self, 
                 name: Symbol, 
                 interface: Symbol, 
                 funcs: Dict[Symbol, FuncDef],
                 repr_type: StructType):
        self.name = name
        self.interface = interface
        self.funcs = funcs
        self.repr_type = repr_type

    def doc(self, opt: DocOptions) -> Doc:
        return Text(f'class {self.name} for {self.interface}(') * self.repr_type.doc(opt) * Text(') {') +\
            Lines([self.funcs[f].doc(opt) for f in self.funcs]).indent() +\
            Text('}')

    def transform(self, visitor: Transformer) -> AST:
        new_funcs = {}
        for f, func in self.funcs.items():
            new_func = visitor(func)
            assert isinstance(new_func, FuncDef)
            new_funcs[f] = new_func
        new_repr_type = visitor(self.repr_type)
        assert isinstance(new_repr_type, StructType)
        return CanonicalClassDefn(self.name, self.interface, new_funcs, new_repr_type)
    
    def visit(self, visitor: Visitor):
        for f in self.funcs.values():
            f.visit(visitor)
        self.repr_type.visit(visitor)

class CanonicalizeClass(Transformer):
    def visit_ClassDef(self, node: ClassDef) -> CanonicalClassDefn:
        funcs = {}
        repr_type = None
        for b in node.body:
            if isinstance(b, FuncDef):
                funcs[b.fn_decl.name] = b
            elif isinstance(b, TypeDef) and b.name == Symbol('This'):
                repr_type = b.type
                # print(repr_type)
            else: # TODO: associated types
                raise NotImplementedError(f'Unknown class body {b}')
        assert repr_type is not None
        return CanonicalClassDefn(node.name, node.interface, funcs, repr_type)

class CanonicalItfDefn(AST):
    def __init__(self, name: Symbol, funcs: Dict[Symbol, FuncDecl]):
        self.name = name
        self.funcs = funcs
    
    def doc(self, opt: DocOptions) -> Doc:
        return Text(f'interface {self.name} ' + '{') +\
            Lines([self.funcs[f].doc(opt) for f in self.funcs]).indent() +\
            Text('}')
    
    def transform(self, visitor: Transformer) -> AST:
        new_funcs = {}
        for f, func in self.funcs.items():
            new_func = visitor(func)
            assert isinstance(new_func, FuncDecl)
            new_funcs[f] = new_func
        return CanonicalItfDefn(self.name, new_funcs)

    def visit(self, visitor: Visitor):
        for f in self.funcs.values():
            f.visit(visitor)

class CanonicalizeItf(Transformer):
    def visit_InterfaceDef(self, node: InterfaceDef) -> CanonicalItfDefn:
        funcs = {}
        for b in node.body:
            if isinstance(b, FuncDecl):
                funcs[b.name] = b
        return CanonicalItfDefn(node.name, funcs)

class ScopedTransformer(Transformer):
    def __init__(self, memo=False) -> None:
        super().__init__(memo)
        self.scope: List[Union[CanonicalClassDefn, CanonicalItfDefn, Program]] = []
    
    def lookup_name(self, name: Symbol, idx=-1) -> Optional[AST]:
        if isinstance(self.scope[idx], Program):
            for b in self.scope[idx].body:
                if isinstance(b, TypeDef) and b.name == name:
                    return b
                elif isinstance(b, FuncDef) and b.fn_decl.name == name:
                    return b
                elif isinstance(b, CanonicalClassDefn) and b.name == name:
                    return b
                elif isinstance(b, CanonicalItfDefn) and b.name == name:
                    return b
        elif isinstance(self.scope[idx], CanonicalClassDefn):
            if name in self.scope[idx].funcs:
                return self.scope[idx].funcs[name]
            return self.lookup_name(name, idx-1)
        elif isinstance(self.scope[idx], CanonicalItfDefn):
            if name in self.scope[idx].funcs:
                return self.scope[idx].funcs[name]
            return self.lookup_name(name, idx-1)
        return None
    
    def __call__(self, node: AST) -> AST:
        if isinstance(node, (CanonicalClassDefn, CanonicalItfDefn, Program)):
            self.scope.append(node)
            new_node = super().__call__(node)
            self.scope.pop()
        else:
            new_node = super().__call__(node)
        return new_node

class ScopedVisitor(Visitor):
    def __init__(self) -> None:
        self.scope: List[Union[CanonicalClassDefn, CanonicalItfDefn, Program]] = []
    
    def lookup_name(
        self, 
        node: Union[CanonicalClassDefn, CanonicalItfDefn, Program],
        name: str
    ) -> Optional[AST]:
        if isinstance(node, Program):
            for b in node.body:
                if isinstance(b, TypeDef) and b.name == name:
                    return b
                elif isinstance(b, FuncDef) and b.fn_decl.name == name:
                    return b
                elif isinstance(b, CanonicalClassDefn) and b.name == name:
                    return b
                elif isinstance(b, CanonicalItfDefn) and b.name == name:
                    return b
        elif isinstance(node, CanonicalClassDefn):
            if name in node.funcs:
                return node.funcs[name]
        elif isinstance(node, CanonicalItfDefn):
            if name in node.funcs:
                return node.funcs[name]
        return None
    
    def lookup_path(self, path: Tuple[str, ...], idx=0) -> Optional[AST]:
        node = self.scope[idx]
        for p in path:
            l = self.lookup_name(node, p)
            if l is None:
                return None
            node = l
        return node

    def __call__(self, node: AST):
        if isinstance(node, (CanonicalClassDefn, CanonicalItfDefn, Program)):
            self.scope.append(node)
            super().__call__(node)
            self.scope.pop()
        else:
            super().__call__(node)

class ResolveSymbolAndVars(ScopedTransformer):
    def __init__(self) -> None:
        super().__init__(memo=False)
        self.vars: Dict[str, Var] = {}
    
    def visit_This(self, node: This) -> AST:
        assert isinstance(self.scope[-1], (CanonicalClassDefn, CanonicalItfDefn))
        if isinstance(self.scope[-1], CanonicalClassDefn):
            return self.scope[-1].repr_type
        # raise RuntimeError("Unable to get This from", self.scope[-1])
        return node
    
    def visit_Self(self, node: Self) -> AST:
        assert isinstance(self.scope[-1], (CanonicalClassDefn, CanonicalItfDefn))
        if isinstance(self.scope[-1], CanonicalClassDefn):
            return SymbolType(self.scope[-1].interface)
        return SymbolType(self.scope[-1].name)
    
    def visit_Var(self, node: Var) -> AST:
        if node.name == 'print':
            return Symbol('print')
        if node.name in self.vars:
            return self.vars[node.name]
        lookup = self.lookup_name(Symbol(node.name))
        if isinstance(lookup, (FuncDef, TypeDef, CanonicalClassDefn, CanonicalItfDefn)):
            return Symbol(node.name)
        raise RuntimeError(f'Unknown variable {node.name}')
    
    def visit_LetStmt(self, node: LetStmt) -> AST:
        self.vars[node.value.name] = node.value
        return node.transform(self)
    
    def visit_FuncDef(self, node: FuncDef) -> AST:
        return node.transform(self)
    
    def visit_FuncDecl(self, node: FuncDecl) -> AST:
        self.vars = {}
        for v in node.args:
            ty = self(v.ty)
            # print(ty, type(self.scope[-1]))
            assert isinstance(ty, Type)
            v.ty = ty
            self.vars[v.name] = v

        new_decl = node.transform(self)
        return new_decl

    def visit_GetAttrExpr(self, node: GetAttrExpr) -> AST:
        obj = self(node.obj)
        if isinstance(obj, Symbol):
            return Symbol(obj.names + (node.attr.names[0],))
        return GetAttrExpr(obj, node.attr)

class TypeInfer(ScopedVisitor):
    def __init__(self) -> None:
        super().__init__()
        self.tys: Dict[int, Type] = {}
    
    def visit_Var(self, node: Var) -> None:
        assert node.ty is not None
        self.tys[id(node)] = node.ty
    
    def visit_CallExpr(self, node: CallExpr) -> None:
        node.visit(self)
        if isinstance(node.fn, Symbol):
            if node.fn.names == ('print',):
                assert len(node.args) == 1
                self.tys[id(node)] = void_t
                return
            lookup = self.lookup_name(self.scope[-1], node.fn.names)
            assert isinstance(lookup, FuncDef)
            assert len(lookup.fn_decl.args) == len(node.args)
            assert all(a.ty == self.tys[id(b)] for a, b in zip(lookup.fn_decl.args, node.args))
            self.tys[id(node)] = lookup.fn_decl.ret
        elif isinstance(node.fn, GetAttrExpr) and id(node.fn.obj) in self.tys:
            obj_ty = self.tys[id(node.fn.obj)]
            if isinstance(obj_ty, SymbolType):
                assert False

        raise NotImplementedError(f'Unknown function call {node.fn}')
    

with open("grammar.lark", 'r') as f:
    grammar = f.read()
parser = Lark(grammar, start='program', parser='earley')

with open('examples/basic.fm', 'r') as f:
    tree = parser.parse(f.read())

# print(tree.pretty())
v = ParseTree()
ast = v.visit(tree)
# print(ast)
# print(ast.doc(DocOptions(show_link=True)).to_str(0))
ast = CanonicalizeClass()(ast)
# print(ast.doc(DocOptions(show_link=True)).to_str(0))
ast = CanonicalizeItf()(ast)
# print(ast.doc(DocOptions(show_link=True)).to_str(0))

ast = ResolveSymbolAndVars()(ast)
print(ast.doc(DocOptions(show_link=True)).to_str(0))

