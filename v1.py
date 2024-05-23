# %%
from lark import Lark

with open("grammar.lark", 'r') as f:
    grammar = f.read()
parser = Lark(grammar, start='program', parser='lalr')

with open('test1.fm', 'r') as f:
    tree = parser.parse(f.read())

