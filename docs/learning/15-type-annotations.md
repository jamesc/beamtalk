## Type Annotations

Beamtalk uses **gradual typing**: annotations are optional. Unannotated code
runs without type errors. Adding annotations enables tooling (IDE hints,
docs, future type checking) but never breaks existing code.

Syntax:

```
param :: Type          — annotate a method parameter
-> ReturnType          — annotate the return type
state: slot :: Type    — annotate a slot
```

The `::` delimiter was chosen because `:` alone is ambiguous in Smalltalk
keyword syntax (it's the selector delimiter). Double-colon is borrowed from
Haskell where it means "has type".

> Note: Type annotations are currently parsed and stored in the AST, but
> type checking is not yet enforced at compile time. They serve as
> documentation and will drive future tooling.

## Method parameter annotations

Annotate a parameter by adding `:: Type` after the parameter name.
The return type goes after all parameters, as `-> Type` before `=>`.

These all run as normal, they're just self-documenting:

```beamtalk
// Unary with return type:
// getValue -> Integer => self.value

// Keyword with typed parameter:
// deposit: amount :: Integer => ...

// Keyword with typed parameter and return type:
// deposit: amount :: Integer -> Integer => ...

// Multiple parameters, all typed:
// transfer: amount :: Integer to: target :: BankAccount => ...

// Mixed: some typed, some untyped:
// inject: initial into: block :: Block => ...
```

## State slot annotations

Annotate instance variable slots:

```beamtalk
// state: balance :: Integer = 0
// state: owner :: String = ""
```

## Union types and special types

Return `Integer` or `String`:

```beamtalk
// -> Integer | String
```

Return `Self` (the same class as the receiver):

```beamtalk
// collect: block :: Block -> Self => ...
```

Nil (no meaningful return):

```beamtalk
// do: block :: Block -> Nil => ...
```

## Built-in type names

Common types recognised by the type system:

```
Integer   Float   Number (Integer | Float)
String    Symbol  Character
Boolean   Nil
Block     Object  Self
Array     Dictionary  Collection
```

User-defined class names are also valid types: `Point`, `BankAccount`, `Counter`.
Any user class name works — these are resolved at class load time.

## Practical examples (runnable)

Since annotations are ignored at runtime in the current implementation,
these examples run identically to unannotated versions.
We assert on behaviour, not on the annotations themselves.

```beamtalk
3 + 4           // => 7
"hello" size    // => 5
```

## A fully-annotated class — complete example

Here is the fully-annotated version of the Counter actor from chapter 11.
The annotations describe intent; runtime behaviour is unchanged.

```beamtalk
// Actor subclass: TypedCounter
//   state: value :: Integer = 0
//
//   // Returns the new value after incrementing
//   increment -> Integer =>
//     self.value := self.value + 1
//
//   // Accept only integers, return Integer
//   incrementBy: n :: Integer -> Integer =>
//     self.value := self.value + n
//
//   // Always returns Integer
//   getValue -> Integer =>
//     self.value
//
//   // Reset to a specific value
//   resetTo: newValue :: Integer -> Integer =>
//     self.value := newValue
```

## Why bother with annotations?

Even without enforcement, annotations give you:

1. **Documentation** — readers know what types are expected
2. **IDE tooling** — hover types, autocomplete (when LSP supports it)
3. **Generated docs** — `beamtalk doc` includes types in HTML output
4. **Future checking** — the type checker can be enabled incrementally

The philosophy: "Annotate what you know, leave unknown things unannotated."
Don't annotate everything for the sake of it — annotate public APIs and
complex internal methods first.

## Canonical formatting

`beamtalk fmt` normalises annotation spacing:

```
Canonical:     deposit: amount :: Integer -> Integer =>
Also accepted: deposit: amount::Integer -> Integer =>
```

Always put a space on both sides of `::`.

## Summary

```
param :: Type                  — parameter annotation
methodName -> ReturnType =>    — return type
state: slot :: Type = default  — slot annotation
```

- Union: `-> Integer | String`
- Special: `-> Self`, `-> Nil`

Annotations are optional and currently informational.
Type checking is planned for a future release.

Next: Chapter 16 — BEAM Interop
