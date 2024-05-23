# familia

## V1
```
program :=  (typeDef | functionDef | classDef | interfaceDef)*

type := '()' | 'i32' | 'This' | 'Self' | TYPE_NAME
term := 'this' | 'self' | VAR_NAME
typeDef := 'type' TYPE_NAME '=' '{' [arg] (',' arg)* '}'

arg := VAR_NAME ':' type

functionDecl := 'fn' FN_NAME '(' [arg] (',' arg)* ')' [':' TYPE_NAME] 
functionDef := functionDecl '{' fnBody '}'

classDef := 'class' CLASS_NAME 'for' INTERFACE_NAME '{' (typeDef | functionDef)* '}'
interfaceDef := 'interface' INTERFACE_NAME '{' functionDecl* '}'

fnBody := 
    | assignStmt
    | returnStmt

assignStmt := VAR_NAME '=' expr ';'
returnStmt := 'return' expr ';'

expr := 
    | INT_LIT
    | VAR_NAME
    | '(' expr BIN_OP expr ')'
    | expr '.' FN_NAME '(' [expr] (',' expr)* ')'
    | FN_NAME '(' [expr] (',' expr)* ')'

BIN_OP := '+' | '-' | '*' | '/'
```

- Grammar atoms
  - `VAR_NAME`
  - `TYPE_NAME`
  - `FN_NAME`
  - `CLASS_NAME`
  - `INTERFACE_NAME`
  - `INT_LIT`
- only primative type is `i32`, copy/value semantics
- products are structurally typed, interface object types are nominally typed

**Supported:**
- product type
- classes implement interfaces
- existential packing/unpacking
- print (`print: i32 -> ()`)

Should model:

Non object type interfaces
```
interface Eq {
    fn eq(This, This): bool
}

interface Add { // overload `+`
    fn add(This, This): This
}

interface Sub { // overload `-`
    fn sub(This, This): This
}

// etc.
```

Function generics

```
class myAdd for Add(i32) {
    fn add(a: i32, b: i32): i32 {...}
}

fn foo[T where Add](a: T, b: T): T {...}

let a: i32 = ...;
let b: i32 = ...;
foo(a, b) // natural class
foo[i32 with myAdd](a, b) // custom class

// generalized method call
a.(Add.add)(b) // natural class
a.(myAdd.add)(b) // custom dispatcher
```

*note no parametric interfaces so far*

**Not supported:**
- class inheritance
- interface inheritance
- generalized method calling syntax `e.(d.m)(...)`
- models `Set[E with cihash]`
- nested classes
- nested interfaces
- family polymorphism
- existentials `Set[out String]`
- other primitive types (`bool`, `f32`, `String`, `List`, `Dict`)

