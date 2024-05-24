# %%
from lark import Lark

with open("grammar.lark", 'r') as f:
    grammar = f.read()
parser = Lark(grammar, start='program', parser='earley')

with open('basic.fm', 'r') as f:
    tree = parser.parse(f.read())

print(tree.pretty())

class Type:
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name
    
    def __hash__(self) -> int:
        return hash(self.name)
    
    def __eq__(self, other: object) -> bool:
        return id(self) == id(other)

class I32(Type):
    def __init__(self):
        super().__init__('i32')

class This(Type):
    def __init__(self):
        super().__init__('This')

class Self(Type):
    def __init__(self):
        super().__init__('Self')

i32 = I32()
this_t = This()
self_t = Self()

class Value:
    def __init__(self, type: Type, name: str):
        self.type = type
        self.name = name

    def __str__(self):
        return f'{self.name} : {self.type}'
    
    def __hash__(self) -> int:
        return hash((self.type, self.name))

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Value):
            return False
        return (self.type, self.name) == (other.type, other.name)


class ThisValue(Value):
    def __init__(self):
        super().__init__(this_t, 'this')
    
    def __str__(self):
        return 'this'


class SelfValue(Value):
    def __init__(self):
        super().__init__(self_t, 'self')
    
    def __str__(self):
        return 'self'


class AST:
    def __str__(self) -> str:
        raise NotImplementedError()

def show_arg(args: list[Value], newline=False) -> str:
    if newline:
        return ',\n'.join(map(str, args))
    else:
        return ', '.join(map(str, args))

class TypeDef(AST):
    def __init__(self, name: str, fields: list[Value]):
        self.name = name
        self.fields = fields

    def __str__(self):
        return f'type {self.name} = ' + "{" + show_arg(self.fields, newline=True) + "}"

class FuncDecl(AST):
    def __init__(self, name: str, args: list[Value], ret: Type):
        self.name = name
        self.args = args
        self.ret = ret
    
    def __str__(self):
        return f'fn {self.name}({show_arg(self.args)}): {self.ret}'

class FuncDef(AST):
    def __init__(self, name: str, args: list[Value], ret: Type, body: list["Stmt"]):
        self.fn_decl = FuncDecl(name, args, ret)
        self.body = body

    def __str__(self):
        return str(self.fn_decl) + "{" + '\n'.join(map(str, self.body)) + "}"

class Stmt(AST):
    pass

class Expr(AST):
    pass

class LetStmt(Stmt):
    def __init__(self, value: Value, expr: Expr):
        self.value = value
        self.expr = expr

    def __str__(self):
        return f'let {self.value} = {self.expr};'

class ReturnStmt(Stmt):
    def __init__(self, expr: Expr):
        self.expr = expr

    def __str__(self):
        return f'return {self.expr};'

class GetAttrExpr(Expr):
    def __init__(self, obj: Expr, attr: str):
        self.obj = obj
        self.attr = attr

    def __str__(self):
        return f'{self.obj}.{self.attr}'

class ColonExpr(Expr):
    def __init__(self, value: Value, accesses: list[str]):
        self.value = value
        self.accesses = accesses
    
    def __str__(self):
        return f'{self.value.name}::{"::".join(self.accesses)}'

class CallExpr(Expr):
    def __init__(self, fn: Expr, args: list[Expr]):
        self.fn = fn
        self.args = args
    
    def __str__(self):
        return f'{self.fn}({show_arg(self.args)})'

class IntExpr(Expr):
    def __init__(self, value: int):
        self.value = value

    def __str__(self):
        return str(self.value)

class VarExpr(Expr):
    def __init__(self, value: Value):
        self.value = value

    def __str__(self):
        return str(self.value)
    
class BinOpExpr(Expr):
    def __init__(self, op: str, lhs: Expr, rhs: Expr):
        self.op = op
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self):
        return f'({self.lhs} {self.op} {self.rhs})'


