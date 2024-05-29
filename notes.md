# Familia rough notes

## Typeclasses and their implementations

**keywords: `interface`, `class`**

- **A class declaration does not define a type**
  - so `class` should be renamed to `adaptor` because that's what they are essentially, they adapt concrete types (`i32`, `String`, `{a: i32, b: String}`, etc) to typeclasses
- Interfaces declare their representation types and their parametric types. Representation types go in parentheses, parametric types go in square brackets

    `interface I[T1, ..., Tn](R1, ..., Rm) {...}`
- Some interfaces can define an object type, like how some rust traits can define `dyn Trait`
  - Interfaces that have multiple representation types cannot have an object type
  - Interfaces where the representation type is used other than the receiver position cannot define an object type
  - The use of object types in familia is not as explicit as rust, which has the `dyn` keyword. Ex:
```
interface T {...}
interface I1 {...}

// object type
class c1 for I1(T) {...}

// representation type
class c2[R] for I1(R) where T(R) {...} 
```

- Interfaces can also be used to constrain types.
- I think typo in figure 6b. If `Set[E]` supports `Self add(E)` then the `add` for `Map` probably won't work, because its probably typed `Self add(K, V)`. So adapting `Map[E, ?]` to `Set[E]` is not well defined. Probably what's meant is:
```
class mapset[E where Eq] for Set[E](Map[E, void]) {...}
```
- class declarations can be done in 5 favors
```
// 1.1 Implicit This
class c11[T1, ...] for I1[T1, ...] 
where Ij(Ti, ...) g1, ...
{
    This = {...}
    ...
}

// 1.2 Retroactively adapting concrete type
type R = ...;
class c12[T1, ...] for I1[T1, ...](R)
where Ij(Ti, ...) g1, ...
{...}

// 2. Adapting object type
class c2[T1, ...] for I2[T1, ...](O)
where Ij(Ti, ...) g1, ... 
{...}

// 3. Remapping
class c3[T1, ..., R1, ..., Rn] for I3[T1, ...](R1, ..., Rn)
where Ij[...](Rk, ...) r1, ...
{...}

// 4. Adapting multiple representation types
class c4[T1, ...] for I4[T1, ...](R1, ...)
where 
    Ij(Ti, ...) g1, ...
    Ik(Ri, ...) r1, ...
{...}

// 5. Natural class
class String for Eq(String) {...}
// actually this is exactly the same as rust's way of saying
impl<S: String> Eq<S> for S {...}
// It would be nice if familia had something like this, where the library author 
// can specify defaults, with statements like 'forall T. String(T) => Eq(T)'. 
// Which I think is the intention behind natural classes, but Nominally typed.
```
- Contrary to the paper I don't think natural classes should be generated structurally. Eg, if `String` has `equal` method and `Eq` has a method of the same name, then `String` satisfies `Eq(String)`. This should be done with intent by the user, similar to a trait declaration in rust.

- classes can be used in 2 ways
```
// 1. To construct an object type (If the interface has one)
class c1 for I1 {...}
let x: I1 = c1(...);

// 2. To satisfy a constraint
fn f[E](a: E, b: E) where Eq(E) {...}
interface Set[E] where Eq(E) {...}

class e1 for Eq(String) {...}

let a: String = ...;

f(a, a); // natural class
f[String with e1](a, a); // custom class

let s1: Set[String] = hashset[String]();
let s2: Set[String with e1] = hashset[String with e1]();
```
- constraint satisfaction becomes tricky if the constraint has an object type
```
class lazy_string for String {...}
fn foo[S](s: S) where String(S) {...}

let x: String = lazy_string(...);
foo(x); // existentially packed 
foo[String with lazy_string](x); // witness

let s1: Set[String] = ...;
let s2: Set[String with lazy_string] = ...;

s1.insert(x); // legal
s2.insert(x); // legal
```
- s2 type declaration does not say that all elements of the set must have `lazy_string` as its class. Only the necessary methods for constraint satisfaction are bound.
- classes can mix and match different dispatchers
```
type R = ...;
class c1 for I1(R) {...}
class c2 for I1(I1) {
    fn m(This, ...) {...}
}

let x: I1 = c1(...);
x.(c2.m)(...); // idk why you need a special syntax
c2.m(x, ...); // why not just this?
```
- classes can also dispatch on subtypes of the current representation type
```
class c1 for I1(R) {
    fn foo(this, b: This) {...} // or fn foo(this, b: R)

    fn foo(this, b: R1) {...}
}

let a: R = ...;
c1.foo(a, a);
let b: R1 = ...;
c1.foo(b, b);
```
- 4 ways of enabling classes. Or different scopes compile down to `with c` in different ways.
- not going to think about `out I` feature for now.
- I think the further specialization example for `setPO` in figure 7 is not deeply related to the natural class polymorphism, and could be simulated with inheritance for object types. So not going to include it for now.

## Family polymorphism and inheritance
- not sure what are the interesting interactions or design patterns that emerge when we add family polymorphism to the picture.
```
class c1 {
    interface I1 {
        type T;
        fn m(this, T)
    }
}

abstract class c2 {
    type T;
    
    interface I1 {
        fn m(this, T)
    }
}

// this should be allowed, as it is in rust
fn foo[H](a: H) where c1.I1(H) {...}

// should satisfy the constraint c1.I1(...)
interface I2 extends c1.I1 {
    type T = i32;
    ...
}

fn bar[H](a: H) where c2.I1(H) {...}

// no way to specify T and thus construct a term
interface I3 extends c2.I1 {...}

class c3 extends c2 {
    type T = i32;

    interface I1 extends I3 {...}
}

// now this should work:
let a: c3.I1 = ...;
bar(a);

// since c3.I1 is a subtype of I3 which is a subtype of c2.I1. But such 
// interfaces like I3 are always vacuous since they are like abstract classes
// should we disallow such definitions?
```
- since the unbound types and classes are like implicit parameters, they can be like keyword arguments to the type constructor
```
interface I3 extends c2.I1[type T = i32] {...}
```
- Then actually everything makes since and `I3` is not vacuous
- inherited code should not change variance

## Featherweight Familia ($F^2$)
- Adding the super interface `Any` is a great escape hatch. Eg. `Any` supports type safe casting with `Option` type, as in Rust and Haskell.

- \[INST] Because interfaces in $F^2$ only have 1 representation type, and $H$ is an interface path, and thus already has all its parameters filled (the square brackets). I think $H^{(i)}(T_2^{(i)})\{\overline T_1 / \overline X\}$ means replacing $T^{(i)}_2$ with the appropriate $T_1^{(i)}$. So if class $d$ dispatches $T_3$ (`class d for I(T_3)`), and $H(T_1)$ entails $I(T_3)$, then replacing any instance of `X where H(X)` with `T1 with d` is fine.
- \[E-PACK] is pretty straightforward
- \[E-UNPACK] don't know why it's necessary for $X \notin FTV(T)$ and $p \notin FCV(T)$. However, I don't think this is too critical; standard virtual dispatch table applies in practice, where unpack is just a model.
- \[E-CALL] Three ways to method call, natural class (corresponds to polymorphism), a named constraint class, or a concrete named class. 
- Well-formedness of paths for figure 22 is more straightforward, just some rules for path correctness subject to syntax features.
- Path linkages for Figure 23 is also pretty straightforward. Not going to look too precisely for now since its fairly orthogonal to the polymorphism features.
- Constraint entailment: If $variance(\mathbb Q) = 0$ and $K \vdash T_2 \leq T_1$ then I think there is no subtyping relation between $Q(T_1) \leq Q(T_2)$? Assuming $0$ means invariant.
  - otherwise everything is straightforward
