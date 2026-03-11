<!-- btfixture: fixtures/ch15typed_point.bt -->
<!-- btfixture: fixtures/ch15typed_counter.bt -->
<!-- btfixture: fixtures/ch15typed_classes.bt -->

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

## Typed classes — the `typed` keyword

Prefix your class declaration with `typed` to declare it as a typed class.
This enables the compiler to verify annotation syntax and store type
metadata for tooling.

### A typed value class

```beamtalk
typed Object subclass: Ch15TypedPoint
  state: x :: Integer = 0
  state: y :: Integer = 0

  getX -> Integer => self.x
  getY -> Integer => self.y

  distanceSquared -> Integer =>
    (self.x * self.x) + (self.y * self.y)
```

```beamtalk
TestCase subclass: Ch15TypedClasses

  testTypedValueClass =>
    p := Ch15TypedPoint new: #{#x => 3, #y => 4}
    self assert: p getX equals: 3
    self assert: p getY equals: 4
    self assert: p distanceSquared equals: 25
```

### A typed actor

```beamtalk
typed Actor subclass: Ch15TypedCounter
  state: value :: Integer = 0

  increment -> Integer =>
    self.value := self.value + 1
    self.value

  add: amount :: Integer -> Integer =>
    self.value := self.value + amount
    self.value

  current -> Integer => self.value
```

```beamtalk
  testTypedActor =>
    c := Ch15TypedCounter spawn
    self assert: c increment equals: 1
    self assert: (c add: 5) equals: 6
    self assert: c current equals: 6
```

### Typed vs untyped

| Aspect | `Object subclass:` | `typed Object subclass:` |
|--------|---------------------|--------------------------|
| Annotations | Optional, ignored | Optional, stored as metadata |
| Runtime behavior | Identical | Identical |
| Tooling | None | IDE hints, generated docs |
| Future checking | Not eligible | Opt-in type checking |

## Practical examples (runnable)

Since annotations are ignored at runtime in the current implementation,
these examples run identically to unannotated versions.
We assert on behaviour, not on the annotations themselves.

```beamtalk
3 + 4           // => 7
"hello" size    // => 5
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
typed Object subclass: ...     — enable type metadata
typed Actor subclass: ...      — typed actor
```

- Union: `-> Integer | String`
- Special: `-> Self`, `-> Nil`

Annotations are optional and currently informational.
Type checking is planned for a future release.

## Exercises

**1. Annotate a method.** Write a method signature for `add: a :: Integer to: b :: Integer -> Integer`
and explain what each `::` and `->` means.

<details>
<summary>Hint</summary>

```text
add: a :: Integer to: b :: Integer -> Integer =>
  a + b
```

- `a :: Integer` — parameter `a` has type `Integer`
- `b :: Integer` — parameter `b` has type `Integer`
- `-> Integer` — the return type is `Integer`
- `::` means "has type", `->` means "returns"
</details>

**2. Typed class.** Write a `typed Object subclass: Temperature` with a
`degrees :: Float` slot and a `isFreezing -> Boolean` method that checks if
degrees is below 0.

<details>
<summary>Hint</summary>

```text
typed Object subclass: Temperature
  state: degrees :: Float = 0.0
  isFreezing -> Boolean => self.degrees < 0
```

The `typed` keyword enables type metadata storage. Annotations are currently
informational — they don't enforce types at runtime.
</details>

**3. When to annotate.** What's the practical difference between
`typed Object subclass:` and plain `Object subclass:` today? When would you
choose one over the other?

<details>
<summary>Hint</summary>

Both behave identically at runtime. `typed` stores annotation metadata for
IDE tooling, generated docs (`beamtalk doc`), and future type checking.
Choose `typed` for public APIs and libraries. Plain classes are fine for
scripts, tests, and internal code where documentation overhead isn't worth it.
</details>

Next: Chapter 16 — BEAM Interop
