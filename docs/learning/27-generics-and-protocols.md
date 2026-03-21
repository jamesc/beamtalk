## Generics and Protocols

Beamtalk's type system goes beyond simple annotations with two powerful features:
**parametric types** (generics) for type-safe containers and wrappers, and
**structural protocols** for formalizing duck-typing contracts. Both are
compile-time only — zero runtime cost.

> **Status:** This chapter documents the design from
> [ADR 0068](../ADR/0068-parametric-types-and-protocols.md). These features are
> being implemented incrementally. Code examples show the target syntax and
> semantics. See the [implementation tracking](../ADR/0068-parametric-types-and-protocols.md#implementation)
> section for current progress.

## Why generics?

Without generics, container types lose the type of their contents:

```
result := File read: "config.json"   // Type checker sees: Result
config := result unwrap              // -> Object (not String!)
config size                          // No completions, no type checking
```

With generics, the type flows through:

```
result :: Result(String, IOError) := File read: "config.json"
config := result unwrap              // -> String
config size                          // String has 'size' -- completions work
```

## Generic class declarations

Classes declare type parameters in parentheses after the class name:

```
sealed Value subclass: Result(T, E)
  field: okValue :: T = nil
  field: errReason :: E = nil

  sealed unwrap -> T =>
    self.isOk ifTrue: [
      self.okValue
    ] ifFalse: [(Erlang beamtalk_result) unwrapError: self.errReason]

  sealed map: block :: Block(T, R) -> Result(R, E) =>
    self.isOk ifTrue: [Result ok: (block value: self.okValue)] ifFalse: [self]
```

Type parameters are **bare uppercase identifiers** — by convention single
letters: `T`, `E`, `K`, `V`, `R`. They can appear in:

- Field annotations: `field: okValue :: T`
- Method parameter types: `block :: Block(T, R)`
- Method return types: `-> T`, `-> Result(R, E)`
- Nested generic types: `Block(T, Result(R, E))`

### Why parentheses, not angle brackets?

Beamtalk uses `Result(T, E)` instead of `Result<T, E>` because `<` is a
binary message (comparison operator). Parentheses avoid any ambiguity.
This follows [Gleam's](https://gleam.run/) approach on the BEAM.

## Using generic types

Supply concrete types when using a generic class as a type annotation:

```
// Variable annotation
result :: Result(String, IOError) := File read: "config.json"
result unwrap    // Type checker knows: -> String

// Method parameter
processResult: r :: Result(Integer, Error) -> Integer =>
  r unwrap + 1   // r unwrap is Integer, Integer has '+'

// Actor state
Actor subclass: Cache(K, V)
  state: store :: Dictionary(K, V) = Dictionary new
```

## Constructor inference

You don't always need to write the type parameters explicitly. The compiler
infers them from constructor arguments:

```
r := Result ok: 42                  // Inferred: Result(Integer, Dynamic)
r unwrap                            // -> Integer

r2 := Result error: #file_not_found // Inferred: Result(Dynamic, Symbol)
r2 error                            // -> Symbol
```

When type parameters can't be inferred, they fall back to `Dynamic` (the
existing behavior — no warnings, no type checking on those positions).

## Block type parameters

`Block(...)` is special-cased — the **last** type parameter is always the
return type:

| Annotation | Meaning |
|---|---|
| `Block(R)` | Zero-argument block returning `R` |
| `Block(A, R)` | One-argument block with arg type `A`, returning `R` |
| `Block(A, B, R)` | Two-argument block with arg types `A`, `B`, returning `R` |

Example:

```
// map: takes a block from T to R, returns Result(R, E)
sealed map: block :: Block(T, R) -> Result(R, E) =>
  ...
```

## Generic inheritance

Generic classes can extend other generic classes:

```
// Array passes its E to Collection's E
Collection(E) subclass: Array(E)

// IntArray fixes E to Integer
Collection(Integer) subclass: IntArray
```

When you call inherited methods, the type checker composes the substitutions
through the inheritance chain.

## Protocols — formalizing duck typing

Protocols define named message sets. A class **automatically conforms** to a
protocol if it responds to all required messages — no `implements:` needed.

### Defining a protocol

```
Protocol define: Printable
  /// Return a human-readable string representation.
  asString -> String

Protocol define: Comparable
  < other :: Self -> Boolean
  > other :: Self -> Boolean
  <= other :: Self -> Boolean
  >= other :: Self -> Boolean

Protocol define: Collection(E)
  size -> Integer
  do: block :: Block(E, Object)
  collect: block :: Block(E, Object) -> Self
  select: block :: Block(E, Boolean) -> Self
```

Protocol bodies use method signatures without `=>` (no implementation body).

### Using protocols as types

Protocol names work in type annotations just like class names — the compiler
resolves the name and determines whether to check structurally (protocol) or
nominally (class):

```
// Printable guarantees asString
display: thing :: Printable =>
  Transcript show: thing asString

// Generic protocol type
printAll: items :: Collection(Object) =>
  items do: [:each | Transcript show: each asString]
```

### Automatic conformance

No declaration needed — the compiler checks the class's method table:

```
// String has asString -> conforms to Printable
// Integer has asString -> conforms to Printable
display: "hello"           // String conforms
display: 42                // Integer conforms
display: Counter spawn     // Counter conforms (asString inherited from Object)
```

Classes that override `doesNotUnderstand:` conform to every protocol (they
can respond to any message).

### Protocol composition

Require multiple protocols with `&`:

```
sort: items :: Collection(Object) & Comparable => ...
```

Extend a protocol:

```
Protocol define: Sortable
  extending: Comparable
  sortKey -> Object
```

### Type parameter bounds

Constrain type parameters to protocol conformance:

```
Actor subclass: Logger(T :: Printable)
  log: item :: T =>
    Transcript show: item asString    // Guaranteed by Printable bound
```

## Union types

Union types express that a value may be one of several types:

```
x :: Integer | String := getValue
x asString             // Both Integer and String have asString
x size                 // Warning: Integer does not respond to 'size'
x + 1                  // Warning: String does not respond to '+'
```

The most common union is `String | nil` — Beamtalk's Option/Maybe pattern:

```
name :: String | nil := dictionary at: "name"
name size              // Warning: UndefinedObject does not respond to 'size'
```

## Control flow narrowing

The type checker narrows types inside conditional blocks:

```
// Class identity check
process: x :: Object =>
  x class = Integer ifTrue: [
    x + 1          // x is Integer here
  ]

// Early return narrows the rest of the method
validate: x :: Object =>
  x isNil ifTrue: [^nil]
  x doSomething    // x is non-nil from here on
```

**Union + narrowing compose** — the killer feature:

```
name :: String | nil := dictionary at: "name"
name isNil ifTrue: [^"unknown"]
name size              // name narrowed to String — nil eliminated
```

## Diagnostic philosophy

The type system **never blocks compilation**:

| What you add | What you get |
|---|---|
| No types | No warnings — fully dynamic, everything works |
| Annotations | Helpful warnings where the checker can verify |
| Protocols | Conformance warnings when shapes don't match |
| `typed` modifier | Completeness warnings for missing annotations |

Type mismatches are always **warnings**. Code always compiles and runs.
In CI, `--warnings-as-errors` enforces that warnings are fixed before merging.

## Runtime introspection

Generic type information survives in `__beamtalk_meta/0` for tooling:

```
> :help Result >> unwrap
unwrap -> T

> Integer conformsTo: Printable
=> true

> Integer protocols
=> #(Printable, Comparable)

> Printable requiredMethods
=> #(#asString)
```

This is **not** reified generics — no runtime type checks, no generic tags
on instances. It's introspection data for the REPL, `:help`, and IDE tooling.

## Summary

```
// Generic class
Value subclass: Stack(E)
  field: items :: Array(E) = #()
  push: item :: E -> Self => ...
  pop -> E => ...

// Protocol
Protocol define: Printable
  asString -> String

// Protocol as type
display: thing :: Printable => Transcript show: thing asString

// Union + narrowing
name :: String | nil := lookup: key
name isNil ifTrue: [^"default"]
name size  // safe — narrowed to String

// Type parameter bound
Logger(T :: Printable) ...
```

- Generics use parentheses: `Result(T, E)`, not `Result<T, E>`
- Protocols are structural — automatic conformance, no `implements:`
- Union types + narrowing compose for null-safe patterns
- All type info is compile-time only — zero runtime cost
- Warnings, never errors — code always compiles

## Exercises

**1. Read a generic annotation.** What does `map: block :: Block(T, R) -> Result(R, E)`
mean? What are `T`, `R`, and `E`?

<details>
<summary>Hint</summary>

- `T` is the type parameter of the current `Result(T, E)` — the ok value type
- `R` is a method-local type parameter — the block's return type
- `E` is the error type parameter of the current `Result(T, E)`
- The method takes a block from `T` to `R` and returns a new `Result(R, E)`
- `R` is inferred from the block argument at each call site
</details>

**2. Protocol conformance.** Does `Array` conform to `Printable`? What about
`Collection(E)`? How would you check?

<details>
<summary>Hint</summary>

`Array` conforms to `Printable` because it inherits `asString` from `Object`.
`Array` also conforms to `Collection(E)` because it has `size`, `do:`,
`collect:`, and `select:`.

Check at runtime: `Array conformsTo: Printable` returns `true`.
Check at compile time: use `:: Printable` as a type annotation and see if
warnings appear.
</details>

**3. Narrowing.** Write a method that takes `x :: Integer | String` and returns
the value doubled — `x * 2` for integers, `x ++ x` for strings. Use narrowing
to avoid type warnings.

<details>
<summary>Hint</summary>

```
double: x :: Integer | String =>
  x class = Integer ifTrue: [
    ^x * 2       // x narrowed to Integer — has '*'
  ]
  x ++ x         // x is String here (Integer case returned early)
```

The `^` (early return) after the `ifTrue:` block eliminates Integer from the
union for the rest of the method, leaving only String.
</details>

Next: Appendix — [Glossary](26-glossary.md)
