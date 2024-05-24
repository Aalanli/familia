# %%

from llvmlite import ir

# Create some useful types
double = ir.DoubleType()
i32 = ir.IntType(32)
fnty = ir.FunctionType(double, (double, double))

# Create an empty module...
module = ir.Module(name=__file__)
# and declare a function named "fpadd" inside it

var = ir.GlobalVariable(module, ir.ArrayType(i32, 12), "a", 0)
# initialize var to array of 12
var.initializer = ir.Constant(ir.ArrayType(i32, 12), [ir.Constant(i32, i) for i in range(12)])

func = ir.Function(module, fnty, name="fpadd")

# Now implement the function
block = func.append_basic_block(name="entry")
builder = ir.IRBuilder(block)
a, b = func.args
res1 = builder.fadd(a, b, name="res")
a_local_ptr = builder.gep(var, [ir.Constant(i32, 0), ir.Constant(i32, 1)], inbounds=True, name="a_local")
a_local = builder.load(a_local_ptr, name="a_local")
a_local_f64 = builder.sitofp(a_local, double)
result = builder.fadd(res1, a_local_f64, name="res2")

builder.ret(result)

func2 = ir.Function(module, ir.FunctionType(i32, ()), name="main")
block2 = func2.append_basic_block(name="entry")
builder2 = ir.IRBuilder(block2)
c = builder2.call(func, [ir.Constant(double, 1.0), ir.Constant(double, 2.0)], name="res")
cond = builder2.icmp_signed("<", c, ir.Constant(double, 3.0))
blocki = func2.append_basic_block(name="if")
builderi = ir.IRBuilder(blocki)
builderi.ret(c)
blocke = func2.append_basic_block(name="else")
buildere = ir.IRBuilder(blocke)
buildere.ret(ir.Constant(i32, 0))

builder2.cbranch(cond, blocki, blocke)




# Print the module IR
print(module)