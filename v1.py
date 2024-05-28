# %%
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

class Type:
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name
    
    def doc(self) -> Doc:
        return Text(self.name)
    
    def __hash__(self) -> int:
        return hash(self.name)
    
    def __eq__(self, other: object) -> bool:
        return hash(self) == hash(other)

class I32(Type):
    def __init__(self):
        super().__init__('i32')

class This(Type):
    def __init__(self):
        super().__init__('This')

class Self(Type):
    def __init__(self):
        super().__init__('Self')

class Void(Type):
    def __init__(self):
        super().__init__('()')
    
    def __str__(self):
        return '()'

i32 = I32()
this_t = This()
self_t = Self()
void_t = Void()


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
        assert isinstance(node, AST)
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


class TypeAlias(AST):
    def __init__(self, ty: Union[str, Type]):
        self.type = ty
        self.parent: Optional[AST] = None

    def doc(self, opt: DocOptions) -> Doc:
        if opt.show_link:
            if isinstance(self.type, str):
                return Text(f'(?{self.type})')
        return Text(f'{self.type}')
    
    def visit(self, visitor: Visitor):
        pass

    def transform(self, visitor: Transformer) -> 'TypeAlias':
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
    def __init__(self, name: str, fields: list['VarExpr']):
        self.name = name
        self.fields = fields

    def doc(self, opt: DocOptions) -> Doc:
        return Text(f'type {self.name} = ' + "{") + Lines([f.doc(opt) for f in self.fields]).indent() + Text("}")

    def transform(self, visitor: Transformer) -> 'TypeDef':
        fields = []
        for f in self.fields:
            x = visitor(f)
            assert isinstance(x, VarExpr)
            fields.append(x)
        
        return TypeDef(self.name, fields)
    
    def visit(self, visitor: Visitor):
        for f in self.fields:
            visitor(f)


class FuncDecl(AST):
    def __init__(self, name: str, args: list['VarExpr'], ret: TypeAlias):
        assert isinstance(ret, TypeAlias)
        self.name = name
        self.args = args
        self.ret = ret
    
    def doc(self, opt: DocOptions) -> Doc:
        return Text(f'fn {self.name}(') * \
            join_doc([a.doc(opt) for a in self.args], ', ') * Text(f'): {self.ret}')
        
    def transform(self, visitor: Transformer) -> 'FuncDecl':
        return FuncDecl(self.name, [visitor(a) for a in self.args], visitor(self.ret))
    
    def visit(self, visitor: Visitor):
        for a in self.args:
            visitor(a)
        visitor(self.ret)


class FuncDef(AST):
    def __init__(self, fn_decl: FuncDecl, body: list["Stmt"]):
        self.fn_decl = fn_decl
        self.body = body
    
    def doc(self, opt: DocOptions) -> Doc:
        return (self.fn_decl.doc(opt) * Text(' {')) + Lines([b.doc(opt) for b in self.body]).indent() + Text('}')
    
    def transform(self, visitor: Transformer) -> 'FuncDef':
        return FuncDef(visitor(self.fn_decl), [visitor(b) for b in self.body])

    def visit(self, visitor: Visitor):
        visitor(self.fn_decl)
        for b in self.body:
            visitor(b)


class ClassDef(AST):
    def __init__(self, name: str, interface: str, body: list[AST]):
        self.name = name
        self.interface = interface
        self.body = body
    
    def doc(self, opt: DocOptions) -> Doc:
        return Text(f'class {self.name} for {self.interface} ' + '{') + Lines([b.doc(opt) for b in self.body]).indent() + Text('}') 
    
    def transform(self, visitor: Transformer) -> 'ClassDef':
        return ClassDef(self.name, self.interface, [visitor(b) for b in self.body])
    
    def visit(self, visitor: Visitor):
        for b in self.body:
            visitor(b)
    

class InterfaceDef(AST):
    def __init__(self, name: str, body: list[AST]):
        self.name = name
        self.body = body
    
    def doc(self, opt: DocOptions) -> Doc:
        return Text(f'interface {self.name} ' + '{') + Lines([b.doc(opt) for b in self.body]).indent() + Text('}')
    
    def transform(self, visitor: Transformer) -> 'InterfaceDef':
        return InterfaceDef(self.name, [visitor(b) for b in self.body])
    
    def visit(self, visitor: Visitor):
        for b in self.body:
            visitor(b)


class Stmt(AST):
    pass


class Expr(AST):
    pass


class ExprStmt(Stmt):
    def __init__(self, expr: Expr):
        self.expr = expr

    def doc(self, opt: DocOptions) -> Doc:
        return self.expr.doc(opt) * Text(';')
    
    def transform(self, visitor: Transformer) -> 'ExprStmt':
        return ExprStmt(visitor(self.expr))

    def visit(self, visitor: Visitor):
        visitor(self.expr)


class LetStmt(Stmt):
    def __init__(self, value: 'VarExpr', expr: Expr):
        self.value = value
        self.expr = expr

    def doc(self, opt: DocOptions) -> Doc:
        return Text(f'let ') * self.value.doc(opt) * Text(' = ') * self.expr.doc(opt) * Text(';')
    
    def transform(self, visitor: Transformer) -> 'LetStmt':
        return LetStmt(visitor(self.value), visitor(self.expr))
    
    def visit(self, visitor: Visitor):
        visitor(self.value)
        visitor(self.expr)


class ReturnStmt(Stmt):
    def __init__(self, expr: Expr):
        self.expr = expr
    
    def doc(self, opt: DocOptions) -> Doc:
        return Text('return ') * self.expr.doc(opt) * Text(';')
    
    def transform(self, visitor: Transformer) -> 'ReturnStmt':
        return ReturnStmt(visitor(self.expr))
    
    def visit(self, visitor: Visitor):
        visitor(self.expr)        


class GetAttrExpr(Expr):
    def __init__(self, obj: Expr, attr: str):
        self.obj = obj
        self.attr = attr

    def doc(self, opt: DocOptions) -> Doc:
        return self.obj.doc(opt) * Text(f'.{self.attr}')

    def transform(self, visitor: Transformer) -> 'GetAttrExpr':
        return GetAttrExpr(visitor(self.obj), self.attr)
    
    def visit(self, visitor: Visitor):
        visitor(self.obj)

class CallExpr(Expr):
    def __init__(self, fn: Expr, args: list[Expr]):
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


class IntExpr(Expr):
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


class VarExpr(Expr):
    def __init__(self, name: str, ty: Optional[TypeAlias] = None, parent: Optional[Expr] = None):
        self.name = name
        self.ty = ty
        self.parent = parent

    def doc(self, opt: DocOptions) -> Doc:
        show_ty = lambda x: x * Text(': ') * self.ty.doc(opt) if self.ty is not None else x * Text(': ?')
        if opt.show_link:
            if self.parent is None:
                return show_ty(Text(f'(?{self.name})'))
        return show_ty(Text(self.name))
    
    def transform(self, visitor: Transformer) -> 'VarExpr':
        return VarExpr(self.name, visitor(self.ty) if self.ty is not None else None, visitor(self.parent) if self.parent is not None else None)

    def visit(self, visitor: Visitor):
        if self.ty is not None:
            visitor(self.ty)
        if self.parent is not None:
            visitor(self.parent)


class BinOpExpr(Expr):
    def __init__(self, op: str, lhs: Expr, rhs: Expr):
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


class ConstructorExpr(Expr):
    def __init__(self, args: list[tuple[str, Expr]]):
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
    
    @staticmethod
    def visit_type(t: Tree) -> TypeAlias:
        # print(type(t), t)
        if isinstance(t, Tree) and t.data == 'custom_type':
            return TypeAlias(str(t.children[0]))
        s = t.data if isinstance(t, Tree) else str(t)
        if s == 'This':
            return TypeAlias("This")
        elif s == 'Self':
            return TypeAlias("Self")
        elif s == 'i32':
            return TypeAlias(i32)
        elif s == '()':
            return TypeAlias(void_t)
        else:
            raise NotImplementedError(f'{s} not implemented')

    def visit_arg(self, node: Tree) -> VarExpr:
        if str(node.data) == 'this_arg':
            return VarExpr("this", TypeAlias("This"))
        name = str(node.children[0])
        ty = ParseTree.visit_type(node.children[1])
        var = VarExpr(name, ty)
        return var
    
    def visit_var(self, node: Tree) -> VarExpr:
        name = str(node.children[0])
        return VarExpr(name)
    
    def visit_number(self, node: Tree) -> IntExpr:
        return IntExpr(int(str(node.children[0])))

    def visit_self(self, node: Tree) -> VarExpr:
        return VarExpr("self", TypeAlias("Self"))
    
    def visit_this(self, node: Tree) -> VarExpr:
        return VarExpr("this", TypeAlias("This"))

    def visit_function_decl(self, node: Tree) -> FuncDecl:
        children = node.children
        name = str(children[0])
        args = [self.visit_arg(c) for c in children[1:-1] if len(c.children) > 0]
        ret_ty = ParseTree.visit_type((children[-1])) if children[-1] is not None else TypeAlias(void_t)
        return FuncDecl(name, args, ret_ty)

    def visit_function_def(self, node: Tree) -> FuncDef:
        children = node.children
        decl = self.visit_function_decl(children[0])
        body = [self.visit(c) for c in children[1:]]
        assert all(isinstance(b, Stmt) for b in body), body
        return FuncDef(decl, cast(list[Stmt], body))
    
    def visit_expr_stmt(self, node: Tree) -> ExprStmt:
        expr = self.visit(node.children[0])
        assert isinstance(expr, Expr)
        return ExprStmt(expr)
    
    def visit_let_stmt(self, node: Tree) -> LetStmt:
        children = node.children
        if isinstance(children[0], Tree):
            var = self.visit_arg(children[0])
        else:
            var = VarExpr(str(children[0]))
        
        expr = self.visit(children[1])
        assert isinstance(expr, Expr)
        return LetStmt(var, expr)
    
    def visit_return_stmt(self, node: Tree) -> ReturnStmt:
        expr = self.visit(node.children[0])
        assert isinstance(expr, Expr)
        return ReturnStmt(expr)
    
    def visit_bin_op_expr(self, node: Tree) -> BinOpExpr:
        children = node.children
        lhs = self.visit(children[0])
        assert isinstance(lhs, Expr)
        rhs = self.visit(children[2])
        assert isinstance(rhs, Expr)
        return BinOpExpr(str(children[1].data), lhs, rhs)
    
    def visit_call_expr(self, node: Tree) -> CallExpr:
        children = node.children
        fn = self.visit(children[0])
        assert isinstance(fn, Expr)
        args = [self.visit(c) for c in children[1:] if c.data != 'expr']
        assert all(isinstance(a, Expr) for a in args)
        return CallExpr(fn, cast(list[Expr], args))

    def visit_constructor_expr(self, node: Tree) -> ConstructorExpr:
        children = node.children
        args = []
        for i in range(0, len(children), 2):
            name = str(children[i])
            expr = self.visit(children[i+1])
            assert isinstance(expr, Expr)
            args.append((name, expr))
        return ConstructorExpr(args)
    
    def visit_get_attr(self, node: Tree) -> GetAttrExpr:
        children = node.children
        obj = self.visit(children[0])
        assert isinstance(obj, Expr)
        attr = str(children[1])
        return GetAttrExpr(obj, attr)
    
    def visit_class_def(self, node: Tree) -> ClassDef:
        children = node.children
        name = str(children[0])
        cfor = str(children[1])
        body = [self.visit(c) for c in children[2:]]
        return ClassDef(name, cfor, body)
    
    def visit_interface_def(self, node: Tree) -> InterfaceDef:
        children = node.children
        name = str(children[0])
        body = [self.visit(c) for c in children[1:]]
        return InterfaceDef(name, body)

    def visit_typedef(self, node: Tree) -> TypeDef:
        children = node.children
        name = str(children[0])
        fields = [self.visit(c) for c in children[1:]]
        assert all(isinstance(f, VarExpr) for f in fields)
        return TypeDef(name, cast(list[VarExpr], fields))
    
    def visit_program(self, node: Tree) -> Program:
        assert node.data == 'program'
        children = node.children
        return Program([self.visit(c) for c in children])

with open("grammar.lark", 'r') as f:
    grammar = f.read()
parser = Lark(grammar, start='program', parser='earley')

with open('basic.fm', 'r') as f:
    tree = parser.parse(f.read())

class PrintVars(Visitor):
    def visit_VarExpr(self, node: VarExpr):
        print(node.name)

# print(tree.pretty())
v = ParseTree()
ast = v.visit(tree)
# print(ast)
print(ast.doc(DocOptions(show_link=True)).to_str(0))


class GetDefn(Visitor):
    def __init__(self):
        self.class_defn: Dict[str, ClassDef] = {}
        self.inf_defn: Dict[str, InterfaceDef] = {}
        self.func_defn: Dict[str, FuncDef] = {}
        self.type_defn: Dict[str, TypeDef] = {}

    def visit_ClassDef(self, node: ClassDef):
        self.class_defn[node.name] = node
    
    def visit_InterfaceDef(self, node: InterfaceDef):
        self.inf_defn[node.name] = node
    
    def visit_FuncDef(self, node: FuncDef):
        self.func_defn[node.fn_decl.name] = node
    
    def visit_TypeDef(self, node: TypeDef):
        self.type_defn[node.name] = node

class UpdateDefn(Transformer):
    def __init__(self, defn: GetDefn):
        super().__init__()
        self.defn = defn
        self.var_scope: List[Dict[str, VarExpr]] = []
        self.parent: List[AST] = []
    
    def visit(self, node: AST) -> AST:
        self.parent.append(node)
        ast = node.visit(self)
        self.parent.pop()
        return ast
    
    def visit_FuncDef(self, node: FuncDef) -> FuncDef:
        fn_decl = node.transform(self)
        assert isinstance(fn_decl, FuncDecl)
        self.var_scope.append({a.name: a for a in fn_decl.args})
        body = [b.transform(self) for b in node.body]
        self.var_scope.pop()
        return FuncDef(fn_decl, body)
    
    def visit_VarExpr(self, node: VarExpr):
        par = self.parent[-1]
        # note to self: CallExpr
        assert isinstance(par, (FuncDecl, LetStmt))
        ty = node.ty.transform(self) if node.ty is not None else None
        new_node = VarExpr(node.name, ty, par)
        return new_node

    def visit_TypeAlias(self, node: TypeAlias):
        return TypeAlias(self.defn.type_defn[node.type.name])

