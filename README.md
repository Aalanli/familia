# familia

## Python V1
See grammar.lark for formal grammar

**Supported:**
- integer arithmetic
- product types `{(NAME: TYPE)+}`
- class dispatch types to interfaces
- function/class/interface generics
- parametric polymorphism
- existential packing/unpacking
- print (`print: i32 -> ()`)


**Not supported:**
- class inheritance
- interface inheritance
- multiple representation types
- nested classes
- nested interfaces
- family polymorphism
- further specialization on `This` parameters
- existentials `Set[out String]`
- other primitive types (`bool`, `f32`, `String`, `List`, `Dict`)

**Notes**
- reference sematics for now, everything is mutable and no free
- memory semantics not decided, probably something like mutable value semantics; something more interesting than reference semantics in typical GC languages.
