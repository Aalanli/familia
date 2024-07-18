# familia

## V1
Currently support dynamic polymorphism, simple string, integer, struct types. And basic print functionality.

### Mutable Value Semantics
Before adding parametric polymorphism and family inheritance, I want to nail down the memory semantics.
After paying attention to the Hylo programming language's memory model, I think MVS is the way to go for simplicity. MVS also in particular 
has no complex interactions with generics that we see with Rust's lifetimes, where low-level implementation details such as mutability and
lifetimes leak over to the interface. Additionally, it does not have the undesirable effects of reference semantics, particularly in combination
with generics and interfaces, namely that of mutable casting and state leakage. 

However I think that Hylo introduces unnecessary complexity in dealing with the ergonomic issues of MVS, by introducing 4 parameter passing conventions
to deal with each of the cases: (1) pass by mutable reference (2) pass by immutable reference (3) pass by mutable value (4) pass by immutable value.

I would like to start simple and only have 2 parameter passing conventions, to address (1) when we want a function to mutate a value (2) when we only
need the function to observe a value.

When we only need to observe a value
```
fn to_int(s: String): i32 {...}
```

When we need to take ownership of a value
```
fn append(own s: String): {@out s: String} { // mutably own
    ...
    return {s: s}
}

```

- observed values cannot be returned and cannot be stored anywhere
- passing a parameter via `own` moves the value into the function, and we can have some syntax sugar to simulate passing by mutable reference
  - only `own` parameters can be mutated, and in practice I don't see a strong reason to have another parameter passing convention for immutably owned parameters. This is to avoid widening the interface surface area. 

```
let t = "123";
let {..} = append(t); // potential syntax to simulate mutabilty
```

- since we have anonymous struct types, we can have `let {..} = ...` to mean assign on the lhs what we simulate by mutation. 

We can apply the same idea to implementations of interfaces:

```
interface AddI {
    fn get(this): i32
    fn inc(own this): {@out this, old: i32}
}
```
So we take the convention that we must return some instance of `this` along with whatever data that we
also want to return.

