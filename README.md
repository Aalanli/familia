# familia

## V1
Currently support dynamic polymorphism, simple string, integer, struct types, and basic print functionality. The compiler is rather immature, and does not support a lot of functionality. I am currently in the midst of rewriting the codebase as copilot made a mess of it; lesson learned (my fault for using it).

This probably only works on linux, but to setup and run:
1. download and build llvm18:
```bash
$ ./setup.sh
```
2. download and install [rust](https://www.rust-lang.org/tools/install)
3. export the following path, globally (for example, in .bashrc)
```bash
export LLVM_SYS_180_PREFIX=".../familia/thirdparty"
```
4. run `cargo build` or `cargo build --release`
5. the executable is either `./target/debug/familia` or `./target/release/familia`
- run `./target/.../familia --help` to see the available options

```bash
$ ./target/.../familia --help
A compiler for the familia language

Usage: familia [OPTIONS] <FILE> [MODE] [OPT_LEVEL]

Arguments:
  <FILE>
          The input file, ending in .fm

  [MODE]
          [default: exe]

          Possible values:
          - dump-ir:   dump the ir
          - dump-llvm: dump the llvm ir
          - exe:       output as an executable file

  [OPT_LEVEL]
          [default: none]

          Possible values:
          - none: no optimizations
          - o1:   -O1

Options:
  -o, --output <OUTPUT>
          The output file

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```

## Planned features
1. figure out memory semantics
2. add control-flow
3. implement parametric polymorphism
4. implement family inheritance


### Mutable Value Semantics
Before adding parametric polymorphism and family inheritance, I want to nail down the memory semantics.
After paying attention to the Hylo programming language's memory model, I think MVS is the way to go for simplicity. MVS also in particular 
has no complex interactions with generics that we see with Rust's lifetimes, where low-level implementation details such as mutability and
lifetimes leak over to the interface. Additionally, it does not have the undesirable effects of reference semantics, particularly in combination
with generics and interfaces, namely that of unsound mutable casting and state leakage. 

However I think that Hylo introduces unnecessary complexity in dealing with the ergonomic issues of MVS, by introducing 4 parameter passing conventions
to deal with each of the cases: (1) pass by mutable reference (2) pass by immutable reference (3) pass by mutable value (4) pass by immutable value.

I would like to start simple and only have 2 parameter passing conventions, to address (1) when we want a function to mutate a value (2) when we only
need the function to observe a value.

When we only need to observe a value
```
fn to_int(s: &String): i32 {...}
```

When we need to take ownership of a value
```
fn append(mut s: String): String { // mutably own
    ...
    return s;
}
```
- observed values cannot be returned
- passing a parameter directly moves the value, it is up to the callee whether to mutate it or not.

```
let t = "123";
let t = append(t);
```
- we can only change the value of a variable in the same scope by rebinding it to the same name.
- However, this introduces some difficulties for receiver methods of interfaces that need to change something
- The solution is accepting some kind of convention where we return the owned `this` from interface methods, or in general receiver functions with the dot syntax.

```
interface AddI {
    fn get(&this): i32
    fn inc(@out this): i32 // something like this
}

fn add(a: AddI) {
    let b: i32 = a.inc();
    // to mean
    // let (a, b) = a.inc();
}

```
So we take the convention that we must return some instance of `this` along with whatever data that we
also want to return.

```
class A1 for AddI {
    ...
    fn inc(mut @out this): i32 {
        ...
        return 1; // means implicitly returning this as well as 1
    }
}
```
The `@out` would mean to rebind to whatever name that the caller passed in, thereby simulating mutation.
This interpretation has some interesting consequences for composite structures. 

```
fn foo(@out a: i32): i32 {...}
let x: {a: i32, b: i32} = {a: 1, b: 2};
let y: i32 = foo(x.a);
// means:
// let (x.a, y) = foo(x.a);
```
So it seems like we defined a new variable `x.a`, but this is invalid syntax. I think the solution here is the functional
update semantics, where we copy all of `x` except `a`, which is replaced by the new value. Of course, this everything is uniquely owned, this is equivalent to mutating 
`x.a`. 
