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
    def __init__(self, name: str):
        self.name = name

    def doc(self, opt: DocOptions) -> Doc:
        return Text('@' + self.name)
    
    def __hash__(self) -> int:
        return hash(self.name)

    def __eq__(self, other: object) -> bool:
        return isinstance(other, Symbol) and self.name == other.name
    
    def visit(self, visitor: Visitor):
        pass

    def transform(self, visitor: Transformer) -> 'Symbol':
        return self

class Path(AST):
    def __init__(self, path: Tuple[Symbol,...]) -> None:
        self.path = path
    
    def doc(self, opt: DocOptions) -> Doc:
        return Text('@' + '::'.join(map(lambda x: x.name, self.path)))
    
    def __hash__(self) -> int:
        return hash(self.path)
    
    def __eq__(self, other: object) -> bool:
        return isinstance(other, Path) and self.path == other.path
    
    def visit(self, visitor: Visitor):
        pass

    def transform(self, visitor: Transformer) -> 'Path':
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
    def __init__(self, name: str, kind: VarKind, ty: Optional[Type] = None):
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
            return Var(self.name, self.kind, ty)
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
            return Var("this", VarKind.THIS, this_t)
        name = str(node.children[0])
        ty = ParseTree.visit_type(node.children[1])
        var = Var(name, VarKind.LOCAL, ty)
        return var
    
    def visit_var(self, node: Tree) -> Var:
        name = str(node.children[0])
        return Var(name, VarKind.LOCAL)
    
    def visit_number(self, node: Tree) -> IntExpr:
        return IntExpr(int(str(node.children[0])))

    def visit_self(self, node: Tree) -> Var:
        return Var("self", VarKind.SELF, self_t) # technically self refers to the class, thus not a type
    
    def visit_this(self, node: Tree) -> Var:
        return Var("this", VarKind.THIS, this_t)

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
            var = Var(str(children[0]), VarKind.LOCAL)
        
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

class CanonicalProgram(AST):
    def __init__(
            self, 
            interfaces: Dict[Symbol, CanonicalItfDefn], 
            type_defs: Dict[Symbol, TypeDef],
            classes: Dict[Symbol, CanonicalClassDefn],
            functions: Dict[Symbol, FuncDef]
        ):
        self.interfaces = interfaces
        self.classes = classes
        self.functions = functions
        self.type_defs = type_defs
    
    def doc(self, opt: DocOptions) -> Doc:
        return Lines([
            Lines([i.doc(opt) for i in self.interfaces.values()]),
            Lines([t.doc(opt) for t in self.type_defs.values()]),
            Lines([c.doc(opt) for c in self.classes.values()]),
            Lines([f.doc(opt) for f in self.functions.values()])
        ])
    
    def transform(self, visitor: Transformer) -> AST:
        new_interfaces = {}
        for i, itf in self.interfaces.items():
            new_itf = visitor(itf)
            assert isinstance(new_itf, CanonicalItfDefn)
            new_interfaces[i] = new_itf
        new_classes = {}
        for c, cls in self.classes.items():
            new_cls = visitor(cls)
            assert isinstance(new_cls, CanonicalClassDefn)
            new_classes[c] = new_cls
        new_functions = {}
        for f, func in self.functions.items():
            new_func = visitor(func)
            assert isinstance(new_func, FuncDef)
            new_functions[f] = new_func
        new_type_defs = {}
        for t, ty in self.type_defs.items():
            new_ty = visitor(ty)
            assert isinstance(new_ty, TypeDef)
            new_type_defs[t] = new_ty
        return CanonicalProgram(new_interfaces, new_type_defs, new_classes, new_functions)
    
    def visit(self, visitor: Visitor):
        for i in self.interfaces.values():
            visitor(i)
        for c in self.classes.values():
            visitor(c)
        for f in self.functions.values():
            visitor(f)
        for t in self.type_defs.values():
            visitor(t)

class CanonicalizeProgram(Transformer):
    def visit_Program(self, node: Program) -> CanonicalProgram:
        interfaces = {}
        classes = {}
        functions = {}
        type_defs = {}
        for b in node.body:
            if isinstance(b, CanonicalItfDefn):
                itf = b
                interfaces[itf.name] = itf
            elif isinstance(b, CanonicalClassDefn):
                cls = b
                classes[cls.name] = cls
            elif isinstance(b, FuncDef):
                functions[b.fn_decl.name] = b
            elif isinstance(b, TypeDef):
                type_defs[b.name] = b
            else:
                raise NotImplementedError(f'Unknown body {b}')
        return CanonicalProgram(interfaces, type_defs, classes, functions)

class ScopedTransformer(Transformer):
    def __init__(self, memo=False) -> None:
        super().__init__(memo)
        self.scope: List[Union[CanonicalClassDefn, CanonicalItfDefn, CanonicalProgram]] = []
    
    def lookup_name(self, name: Symbol, idx=-1) -> Optional[AST]:
        prog = self.scope[idx]
        if isinstance(prog, CanonicalProgram):
            if name in prog.interfaces:
                return prog.interfaces[name]
            if name in prog.classes:
                return prog.classes[name]
            if name in prog.functions:
                return prog.functions[name]
            if name in prog.type_defs:
                return prog.type_defs[name]
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
        if isinstance(node, (CanonicalClassDefn, CanonicalItfDefn, CanonicalProgram)):
            self.scope.append(node)
            new_node = super().__call__(node)
            self.scope.pop()
        else:
            new_node = super().__call__(node)
        return new_node

class ScopedVisitor(Visitor):
    def __init__(self) -> None:
        self.scope: List[Union[CanonicalClassDefn, CanonicalItfDefn, CanonicalProgram]] = []
    
    def lookup_name(
        self, 
        node: Union[CanonicalClassDefn, CanonicalItfDefn, CanonicalProgram],
        name: Symbol
    ) -> Optional[AST]:
        if isinstance(node, CanonicalProgram):
            if name in node.interfaces:
                return node.interfaces[name]
            if name in node.classes:
                return node.classes[name]
            if name in node.functions:
                return node.functions[name]
            if name in node.type_defs:
                return node.type_defs[name]
        elif isinstance(node, CanonicalClassDefn):
            if name in node.funcs:
                return node.funcs[name]
        elif isinstance(node, CanonicalItfDefn):
            if name in node.funcs:
                return node.funcs[name]
        return None
    
    def lookup_path(self, path: Path, idx=0) -> Optional[AST]:
        node = self.scope[idx]
        for p in path.path:
            l = self.lookup_name(node, p)
            if l is None:
                return None
            node = l
        return node

    def __call__(self, node: AST):
        if isinstance(node, (CanonicalClassDefn, CanonicalItfDefn, CanonicalProgram)):
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
            assert v.ty is not None
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
            return Path((obj, node.attr))
        return GetAttrExpr(obj, node.attr)

class TypeInfer(ScopedVisitor):
    def __init__(self) -> None:
        super().__init__()
        self.tys: Dict[int, Type] = {}
    
    def visit_Var(self, node: Var) -> None:
        assert node.ty is not None
        self.tys[id(node)] = node.ty
    
    def visit_IntExpr(self, node: IntExpr) -> None:
        self.tys[id(node)] = i32
    
    def visit_BinOpExpr(self, node: BinOpExpr) -> None:
        node.visit(self)
        assert id(node.lhs) in self.tys, f'{node.lhs} not in tys'
        assert id(node.rhs) in self.tys, f'{node.rhs} not in tys'
        assert self.tys[id(node.lhs)] == i32
        assert self.tys[id(node.rhs)] == i32
        self.tys[id(node)] = i32
    
    def visit_ConstructorExpr(self, node: ConstructorExpr) -> None:
        node.visit(self)
        self.tys[id(node)] = StructType({name: self.tys[id(expr)] for name, expr in node.args})
    
    def visit_GetAttrExpr(self, node: GetAttrExpr) -> None:
        node.visit(self)
        assert id(node.obj) in self.tys, f'{node.obj} not in tys'
        base_ty = self.tys[id(node.obj)]
        if isinstance(base_ty, SymbolType):
            lookup = self.lookup_name(self.scope[0], base_ty.symbol)
            assert isinstance(lookup, TypeDef)
            ty = lookup.type
            assert node.attr.name in ty.fields, f'{node.attr.name} not in {ty}'
            self.tys[id(node)] = ty.fields[node.attr.name]
        elif isinstance(base_ty, StructType):
            assert node.attr.name in base_ty.fields, f'{node.attr.name} not in {base_ty}'
            self.tys[id(node)] = base_ty.fields[node.attr.name]
        else:
            raise NotImplementedError(f'Unknown base type {base_ty}')
        # other case is interfaces, handled in CallExpr

    def visit_CallExpr(self, node: CallExpr) -> None:
        if isinstance(node.fn, GetAttrExpr):
            self(node.fn.obj)
        else:
            node.visit(self)

        def handle_function(fn: FuncDecl, args: List[Type]):
            assert len(fn.args) == len(args)
            assert all(a.ty == b for a, b in zip(fn.args, args)), f'{[t.ty for t in fn.args]} != {args}'
            self.tys[id(node)] = fn.ret
        
        if isinstance(node.fn, Symbol):
            if node.fn.name == 'print':
                assert len(node.args) == 1
                self.tys[id(node)] = void_t
                return
            # lol, this could be much better
            # TODO: error check name resolution
            lookup = self.lookup_name(self.scope[0], node.fn)
            if isinstance(lookup, FuncDef):
                handle_function(lookup.fn_decl, [self.tys[id(a)] for a in node.args])
            elif isinstance(lookup, CanonicalClassDefn):
                assert len(node.args) == 1
                assert self.tys[id(node.args[0])] == lookup.repr_type
                self.tys[id(node)] = SymbolType(lookup.interface)
            else:
                raise NotImplementedError(f'Unknown function {node.fn}')
        elif isinstance(node.fn, GetAttrExpr):
            # TODO: this is very naive
            base = node.fn.obj
            attr = node.fn.attr
            assert id(base) in self.tys
            assert isinstance(base, Var), "By now, all . should be resolved to Paths"
            base_ty = self.tys[id(base)]
            if isinstance(base_ty, SymbolType):
                lookup = self.lookup_name(self.scope[0], base_ty.symbol)
                assert isinstance(lookup, CanonicalItfDefn)
                assert node.fn.attr in lookup.funcs
                func_fn = lookup.funcs[node.fn.attr]
                handle_function(func_fn, [this_t] + [self.tys[id(a)] for a in node.args])
            elif isinstance(base_ty, StructType):
                assert base.kind == VarKind.THIS, f"Only this is allowed for now, got {base.kind}"
                assert isinstance(self.scope[-1], CanonicalClassDefn)
                assert attr in self.scope[-1].funcs
                func_fn = self.scope[-1].funcs[attr].fn_decl
                handle_function(func_fn, [base_ty] + [self.tys[id(a)] for a in node.args])
            
        elif isinstance(node.fn, Path):
            assert len(node.fn.path) == 2
            lookup = self.lookup_path(node.fn)
            if isinstance(lookup, FuncDecl):
                func = lookup
            elif isinstance(lookup, FuncDef):
                func = lookup.fn_decl
            handle_function(func, [self.tys[id(a)] for a in node.args])
        else:
            raise NotImplementedError(f'Unknown function call {node.fn}')
    
    def visit_LetStmt(self, node: LetStmt) -> None:
        self(node.expr)
        assert id(node.expr) in self.tys
        if node.value.ty is not None:
            assert node.value.ty == self.tys[id(node.expr)]
        else:
            node.value.ty = self.tys[id(node.expr)]
            self.tys[id(node.value)] = self.tys[id(node.expr)]
        self.tys[id(node)] = void_t


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
ast = CanonicalizeProgram()(ast)
ast = ResolveSymbolAndVars()(ast)
infer = TypeInfer()
infer(ast)
print(ast.doc(DocOptions(show_link=True)).to_str(0))


