# ADR 0068: Parametric Types and Protocols

## Status
Proposed (2026-03-20)

## Context

Phases 1 and 2 of ADR 0025 (Gradual Typing) are complete — the compiler infers types from known classes and supports optional `:: Type` annotations that generate Dialyzer `-spec` attributes. However, two critical features remain unimplemented: **parametric (generic) types** and **structural protocols**.

### The Immediate Problem: Result Loses Type Information

ADR 0060 introduced `Result` as a sealed Value class for expected-failure handling. Every accessor returns `Object`, erasing the wrapped type:

```beamtalk
result := File read: "config.json"   // Type checker sees: Result
config := result unwrap              // → Object (not String!)
config size                          // ⚠️ No completions, no type checking
```

The `Result` class declares `field: okValue :: Object` because there is no way to express "the type the caller put in." The same problem affects `map:`, `andThen:`, `valueOr:`, and every other Result combinator. ADR 0060 explicitly acknowledges this: *"Future parameterized types (`-> Result(String, IOError)`) will be needed for full type safety."*

This is not just a Result problem — it applies to any container or wrapper type:

```beamtalk
stack := Stack new             // Stack of what?
stack push: 42
item := stack pop              // → Object, not Integer
item + 1                       // ⚠️ No checking — Object doesn't have '+'
```

### The Broader Gap: Protocols Need Generics

ADR 0025 Phase 3 defines structural protocols — named message sets like `Printable` (requires `asString`) and `Collection` (requires `size`, `do:`, `collect:`). But useful protocol definitions require type parameters:

```beamtalk
// Without generics, Collection's collect: can't express its return type
Protocol define: Collection
  requiring: [size, do:, collect:]   // collect: returns... what? Collection of what?

// With generics, we can express the relationship
Protocol define: Collection(E)
  requiring: [size -> Integer, do: :: Block(E, Object), collect: :: Block(E, Object) -> Self]
```

Generics are a prerequisite for expressive protocols. This ADR therefore addresses **generics first, then protocols**, as an integrated type system extension.

### Current Infrastructure

The AST already defines `TypeAnnotation::Generic { base, parameters, span }` but it is **never produced by the parser** — only constructible programmatically. The type checker has two escape hatches that bail out on non-simple types:

- `check_return_type` (validation.rs:251): `_ => return` skips any union/generic return type
- `is_assignable_to` (validation.rs:492): string-level `contains('<')` bypass
- `set_param_types` (inference.rs:147): non-`Simple` annotations → `Dynamic`

`ClassDefinition` has no `type_params` field. Classes cannot declare themselves as generic.

### Constraints

1. **Type erasure** (ADR 0025): All type information is compile-time only. Zero runtime cost.
2. **Warnings, not errors** (ADR 0025): Type mismatches produce warnings, never block compilation.
3. **Gradual adoption**: Untyped code must continue working unchanged. `Result ok: 42` without annotations stays valid.
4. **BEAM integration**: Generic annotations should generate appropriate Dialyzer `-spec` attributes.
5. **Forward compatibility**: The design must support future protocol bounds (`T :: Printable`) without breaking changes.

## Decision

Beamtalk adopts **declaration-site parametric types** with **compile-time substitution** and **structural protocols**, implemented in two ordered stages. Type parameters use **parenthesis syntax** — `Result(T, E)` — keeping `<` reserved exclusively as a binary message (comparison operator).

### Stage 1: Parametric Types (Generics)

#### Class-Level Type Parameter Declaration

Classes declare type parameters after the class name using parentheses:

```beamtalk
sealed Value subclass: Result(T, E)
  field: okValue :: T = nil
  field: errReason :: E = nil

  sealed unwrap -> T =>
    self.isOk ifTrue: [
      self.okValue
    ] ifFalse: [(Erlang beamtalk_result) unwrapError: self.errReason]

  sealed map: block :: Block(T, R) -> Result(R, E) =>
    self.isOk ifTrue: [Result ok: (block value: self.okValue)] ifFalse: [self]

  sealed andThen: block :: Block(T, Result(R, E)) -> Result(R, E) =>
    self.isOk ifTrue: [block value: self.okValue] ifFalse: [self]
```

Type parameters are bare uppercase identifiers (by convention single letters: `T`, `E`, `K`, `V`, `R`). They appear in:
- Field type annotations: `field: okValue :: T`
- Method parameter types: `block :: Block(T, R)`
- Method return types: `-> T`, `-> Result(R, E)`
- Nested generic types: `Block(T, Result(R, E))`

#### Usage-Site Type Application

When using a generic class as a type annotation, concrete types replace the parameters:

```beamtalk
// Annotating a variable
result :: Result(String, IOError) := File read: "config.json"
result unwrap    // Type checker knows: → String

// Annotating a method parameter
processResult: r :: Result(Integer, Error) -> Integer =>
  r unwrap + 1   // ✅ r unwrap is Integer, Integer has '+'

// Annotating state
Actor subclass: Cache(K, V)
  state: store :: Dictionary(K, V) = Dictionary new
```

#### Type Inference Through Generics

The type checker performs **positional substitution**: when it encounters `Result(String, IOError)`, it maps `T → String`, `E → IOError`, and substitutes through all method signatures of `Result`:

```beamtalk
r :: Result(Integer, Error) := computeSomething
r unwrap          // Return type T → Integer ✅
r map: [:v | v asString]   // Block param T → Integer, return Result(String, Error)
r error           // Return type E → Error ✅
```

When the concrete type parameters are unknown (no annotation, no inference context), type parameters fall back to `Dynamic` — preserving the current behavior:

```beamtalk
r := Result ok: 42         // r is Result (no type params inferred)
r unwrap                   // → Dynamic (T is unknown)
r unwrap + 1               // No warning — Dynamic bypasses checking
```

#### Constructor Type Inference

For named constructors (`ok:`, `error:`, `new`), the compiler infers type parameters from the argument types:

```beamtalk
r := Result ok: 42                  // Inferred: Result(Integer, Dynamic)
r unwrap                            // → Integer ✅

r2 := Result error: #file_not_found // Inferred: Result(Dynamic, Symbol)
r2 error                            // → Symbol ✅
```

This is limited to direct constructor calls with literal or already-typed arguments. Complex expressions fall back to `Dynamic` parameters.

#### Dialyzer Spec Generation

Generic types generate expanded Dialyzer specs with concrete types substituted:

```beamtalk
processResult: r :: Result(Integer, Error) -> Integer => r unwrap + 1
```

Generates:
```erlang
-spec processResult(#{
  '__class__' := 'Elixir.Result',
  'okValue' := integer(),
  'errReason' := any()  % Error maps to any() in Dialyzer
}) -> integer().
```

Unresolved type parameters map to `any()` in Dialyzer specs.

#### REPL Examples

```beamtalk
> r := Result ok: 42
=> Result ok: 42

> r unwrap
=> 42
// Type info: Integer (inferred from Result(Integer, Dynamic))

> r map: [:v | v asString]
=> Result ok: "42"
// Type info: Result(String, Dynamic)

> r2 :: Result(String, IOError) := File read: "test.txt"
=> Result ok: "hello world"

> r2 unwrap size
=> 11
// Type info: Integer (String >> size -> Integer)
```

#### Error Examples

```beamtalk
// When type params are known, checking works through them
r :: Result(Integer, Error) := computeSomething
r unwrap ++ " suffix"
// ⚠️ Warning: Integer does not respond to '++'
//    Did you mean '+'?

// Mismatched type application
x :: Result(Integer, Error) := Result ok: "hello"
// ⚠️ Warning: Result(Integer, Error) expected Integer for T, got String
```

#### What Is NOT Included in Stage 1

- **Variance rules** (covariance/contravariance): All type parameters are invariant. `Result(Integer, E)` is not assignable to `Result(Number, E)`. Variance can be added later without breaking changes.
- **Type parameter bounds/constraints** (`T :: Printable`): Deferred to Stage 2 (protocols). Type parameters are unbounded — any type is accepted.
- **Higher-kinded types** (`F(_)`): Not planned. Beamtalk is not Haskell.
- **Type parameter inference from usage patterns**: Only constructor calls infer type params. Method chains don't propagate backwards.
- **Generic methods** (methods with their own type params independent of the class): Deferred. Class-level params cover the immediate needs.

### Stage 2: Structural Protocols

#### Protocol Definition

Protocols define named message sets. A class conforms to a protocol if it responds to all required messages — no `implements:` declaration needed.

```beamtalk
Protocol define: Printable
  requiring: [asString -> String]

Protocol define: Comparable
  requiring: [< :: Self -> Boolean, > :: Self -> Boolean,
              <= :: Self -> Boolean, >= :: Self -> Boolean]

Protocol define: Collection(E)
  requiring: [size -> Integer, do: :: Block(E, Object),
              collect: :: Block(E, Object) -> Array(Object),
              select: :: Block(E, Boolean) -> Self]
```

Protocol names are **bare identifiers** (uppercase, like class names). Protocols and classes share a single namespace — having both a class and a protocol named `Printable` is a compile error. Required methods use the same `:: Type` and `-> ReturnType` annotation syntax as regular methods.

#### Protocol Type Syntax

Protocol types use the **same syntax as class types** in type annotations — bare identifiers. The compiler resolves the name and determines whether to perform nominal (class) or structural (protocol) checking:

```beamtalk
// Nominal type — compiler looks up Integer, finds a class → nominal check
deposit: amount :: Integer => ...

// Structural/protocol type — compiler looks up Printable, finds a protocol → structural check
display: thing :: Printable =>
  Transcript show: thing asString    // ✅ Printable guarantees asString

// Generic protocol type
printAll: items :: Collection(Object) =>
  items do: [:each | Transcript show: each asString]
```

No special wrapper syntax is needed — name resolution is sufficient because protocols and classes share a namespace. This is the same model used by TypeScript (interfaces) and Swift (protocols).

#### Conformance Checking

Conformance is **structural and automatic**:

```beamtalk
// String has asString → conforms to Printable
// Integer has asString → conforms to Printable
// Counter has asString (from Object) → conforms to Printable

display: "hello"           // ✅ String conforms to Printable
display: 42                // ✅ Integer conforms to Printable
display: Counter spawn     // ✅ Counter conforms to Printable
```

The type checker verifies conformance by checking that the class's method table (including inherited methods) contains all required selectors with compatible signatures.

When conformance cannot be verified (Dynamic values, runtime-constructed classes):

```beamtalk
display: someUnknownValue
// ⚠️ Warning: cannot verify Printable conformance for Dynamic value
```

#### Runtime Protocol Queries

```beamtalk
> Integer conformsTo: Printable
=> true

> Integer protocols
=> #(Printable, Comparable)

> Printable requiredMethods
=> #(asString)

> Printable conformingClasses
=> #(Integer, Float, String, Boolean, Symbol, Array, ...)
```

Runtime queries use the protocol registry compiled into module attributes. `conformsTo:` and `protocols` are messages on class objects; `requiredMethods` and `conformingClasses` are messages on protocol objects.

#### Protocol Composition

```beamtalk
// Require multiple protocols
sort: items :: Collection(Object) & Comparable => ...

// Protocol extending another
Protocol define: Sortable
  extending: Comparable
  requiring: [sortKey -> Object]
```

#### Generic Protocol Bounds (Connecting Stages 1 and 2)

Once both stages are complete, type parameters can be bounded by protocols:

```beamtalk
// T must conform to Printable
Actor subclass: Logger(T :: Printable)
  log: item :: T =>
    Transcript show: item asString    // ✅ Guaranteed by Printable bound
```

This is the natural composition of Stage 1 (type parameters) and Stage 2 (protocols as type constraints).

## Prior Art

### Strongtalk (Primary Influence)
Optional, structural typing for Smalltalk designed by Gilad Bracha. Strongtalk introduced **protocols as named message sets** with structural conformance — no `implements:` needed. We adopt this model directly. Strongtalk used `<Type>` syntax for all type annotations; we use `:: Type` for annotations (per ADR 0053) and bare names for both classes and protocols (the compiler resolves which is which).

**Adopted:** Structural conformance, protocols as message sets, type erasure.
**Adapted:** Bare-name protocol types instead of Strongtalk's angle-bracket wrapper.

### TypeScript (Inference and Generics Model)
TypeScript's generics use `<T>` syntax with structural compatibility. Generic interfaces (`interface Stack<T> { push(item: T): void }`) map directly to our protocol design. TypeScript infers generic type arguments from constructor calls and assignment context — we adopt the same inference strategy for constructors. TypeScript interfaces and classes share a namespace, with no special syntax to distinguish them in type position — we adopt this approach.

**Adopted:** Constructor inference, structural compatibility, shared namespace for protocols/classes.
**Adapted:** Parenthesis syntax `(T)` instead of angle brackets `<T>` to avoid overloading `<` (a binary message in Beamtalk).

### Gleam (Generics on BEAM)
Gleam has full Hindley-Milner generics with complete type inference. `fn push(stack: Stack(a), item: a) -> Stack(a)` uses parentheses for type application. Gleam proves that parametric types work well on BEAM with type erasure — generated BEAM code is identical with or without generics.

**Adopted:** Parenthesis syntax for type application (`Result(T, E)`), type erasure for generics on BEAM (zero runtime cost).
**Rejected:** Mandatory typing, lowercase type variable convention.

### Swift (Protocols with Associated Types)
Swift combines protocols with associated types for parametric protocol definitions. `protocol Collection { associatedtype Element }` is equivalent to our `Protocol define: Collection(E)`. Swift requires explicit conformance declarations (`struct Foo: Collection`); we reject this in favor of automatic structural conformance.

**Adopted:** Parametric protocols, generic protocol constraints (`T: Protocol`).
**Rejected:** Explicit conformance declarations (too much boilerplate for a Smalltalk-family language).

### Elixir (Protocols on BEAM)
Elixir protocols are **nominal** — types must explicitly implement them (`defimpl Printable, for: Integer`). This is the opposite of our structural approach. However, Elixir proves that protocol dispatch on BEAM works well and generates efficient code.

**Rejected:** Nominal/explicit conformance.
**Learned:** Protocol dispatch is efficient on BEAM; protocol metadata can live in module attributes.

### Pony (Structural Typing with Capabilities)
Pony combines structural subtyping with reference capabilities. Its `interface` keyword defines structural types similar to our protocols. Pony requires explicit `fun` signatures in interfaces — we simplify to selector lists with optional type annotations.

**Learned:** Structural typing composes well with actor-model languages.

## User Impact

### Newcomer (from Python/JS/Ruby)
- **Generics:** "`Result(Integer, Error)` reads naturally — like a function signature." Parenthesis syntax is familiar from Python's `List[int]` or Gleam's `Result(ok, err)`.
- **Protocols:** "Like TypeScript interfaces but you don't have to say `implements`." Automatic conformance removes a friction point.
- **Risk:** Generic syntax in class definitions (`Value subclass: Result(T, E)`) mixes Smalltalk keyword syntax with generic parentheses — could look unfamiliar. Mitigation: users encounter generic *usage* (`:: Result(Integer, Error)`) long before they write generic *definitions*.

### Smalltalk Developer
- **Generics:** Smalltalk has no generics — this is a departure. However, it's purely optional. Untyped code works exactly as before. Generics only appear in type annotations, which are themselves optional.
- **Protocols:** Protocols formalize what Smalltalk developers already do informally — "this object must respond to `asString`." The structural model is pure Smalltalk duck-typing made explicit.
- **Risk:** "Type parameters feel alien to Smalltalk." Mitigation: `typed` classes opt into generics; regular classes never see them.

### Erlang/Elixir Developer
- **Generics:** "It generates proper Dialyzer specs with concrete types — I get real BEAM-level checking!" Generic annotations produce expanded `-spec` with `integer()`, `binary()`, etc. instead of `any()`.
- **Protocols:** "Like Elixir protocols but structural — no `defimpl` boilerplate." Familiar concept, less ceremony.
- **Risk:** Structural protocols may not map cleanly to Elixir's nominal protocol dispatch for interop. Mitigation: Beamtalk protocols are compile-time only; at the BEAM level, it's still standard message dispatch.

### Production Operator
- **All:** Zero runtime cost — type erasure means identical BEAM bytecode. Same observability, same hot code reloading, same `observer` experience.
- **Risk:** None specific to this change. Type checking remains warnings-only.

### Tooling Developer (LSP)
- **Generics:** Major win — `result unwrap` now returns `String` instead of `Object`. Completions after `unwrap` show String methods. Hover shows `Result(String, IOError)`.
- **Protocols:** Protocol-typed parameters get completions for all required methods. "Go to protocol definition" becomes possible.
- **This is the primary driver** — generics exist to make tooling accurate.

## Steelman Analysis

### Option A: Annotation-Only Generics (Rejected)

Parse `Result(Integer, IOError)` at usage sites but don't add type params to class definitions. Hardcode substitution rules for known stdlib types.

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "I only need to write `:: Result(Integer, Error)` — I never have to define generic classes myself" |
| **Smalltalk purist** | "This keeps class definitions pure Smalltalk — no parenthesized params in class headers" |
| **BEAM veteran** | "Generates the same Dialyzer specs with less compiler complexity" |
| **Operator** | "Less compiler machinery = fewer compiler bugs = more stable builds" |
| **Language designer** | "YAGNI — solve the Result problem without building a full generics system" |

**Why rejected:** Doesn't scale. Every new generic class requires hardcoded substitution rules in the type checker. Users can't define their own generic classes. The type checker becomes a bag of special cases rather than a principled system.

### Option B: Full Parametric Polymorphism (Rejected)

Complete generics with variance annotations, bounded quantification, generic methods, and type parameter inference from usage patterns.

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "If we're going to do generics, do them right — I know variance from Kotlin/Scala" |
| **Language designer** | "A sound type system with variance prevents real bugs that invariant generics miss" |
| **BEAM veteran** | "Gleam has full HM inference — Beamtalk should match it for credibility" |

**Why rejected:** Massive implementation effort (XL+) for marginal benefit. Variance rules and bounded quantification add complexity that most Beamtalk users will never encounter. The gradual typing philosophy means we can always add variance later — invariant generics are a subset of variant generics, not a dead end.

### Option C: Pragmatic Declaration-Site Generics (Chosen)

Classes declare type parameters. Methods reference them. Type checker substitutes. No variance, no bounds (until protocols land), no HKTs.

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "`Result(Integer, Error)` reads cleanly — like Gleam or Python type hints" |
| **Smalltalk purist** | "Type params are optional — my untyped code doesn't change at all" |
| **BEAM veteran** | "Generates proper Dialyzer specs and the compiler catches real bugs" |
| **Operator** | "Zero runtime cost, same bytecode, same observability" |
| **Language designer** | "Simple enough to implement well, extensible to variance/bounds later" |

### Tension Points

- **Smalltalk purists** would prefer no generics at all — but they accept that type annotations are optional and generics only appear inside them.
- **Language designers** would prefer variance rules from day one — but invariant generics are forward-compatible and dramatically simpler.
- **TypeScript/Java developers** expect angle-bracket `<T>` syntax — but `<` is a binary message in Beamtalk, and parentheses are unambiguous. Gleam validates this choice on BEAM.

## Alternatives Considered

### Alternative A: Angle Bracket Syntax (`Result<T, E>`)

Use `Result<Integer, IOError>` following TypeScript/Java/Rust convention.

```beamtalk
sealed Value subclass: Result<T, E>
  field: okValue :: T = nil
  unwrap -> T => ...
```

**Rejected because:**
- `<` is a binary message (comparison operator) in Beamtalk — giving it dual meaning as both a message send and a type parameter delimiter adds parsing complexity and cognitive load
- Parentheses keep `<` purely as a message, with no dual meaning anywhere in the language
- Gleam (the closest typed BEAM language) successfully uses parentheses for the same purpose
- ADR 0025's earlier `<>` examples predate the current syntax decisions and are not binding

### Alternative B: Square Bracket Syntax (`Result[T, E]`)

Use `Result[Integer, IOError]` following Scala/Python convention.

```beamtalk
sealed Value subclass: Result[T, E]
  field: okValue :: T = nil
  unwrap -> T => ...
```

**Rejected because:**
- Square brackets are used for array/block literals: `[1, 2, 3]` and `[:x | x + 1]`
- `Array[Integer]` is visually ambiguous with `Array` followed by a block literal
- Parentheses don't have this collision

### Alternative C: No Class-Level Declaration — Infer Everything

Don't add type params to classes. Instead, infer generic relationships from method signatures.

**Rejected because:**
- Inferring generic relationships from `Object` return types is unsound — every class that returns `Object` would look generic
- No way to distinguish "this method intentionally returns Object" from "this method returns the type parameter"
- Explicit declaration is clearer and more maintainable

### Alternative D: Nominal Protocol Conformance (`implements:`)

Require explicit `implements:` declarations instead of automatic structural conformance.

```beamtalk
Value subclass: Point
  implements: [Printable, Comparable]
  ...
```

**Rejected because:**
- Violates Smalltalk's duck-typing philosophy — objects are defined by what they respond to, not what they declare
- ADR 0025 already decided on structural conformance
- Boilerplate burden — every class must list every protocol it conforms to
- Breaks when protocols are added later (existing classes must be updated)

### Alternative E: Protocols Before Generics

Implement protocols first (Phase 3), then generics (Phase 4), following ADR 0025's original ordering.

**Rejected because:**
- Protocols without generics can't express useful container contracts (`Collection(E)`)
- The immediate pain point (Result typing) requires generics, not protocols
- Protocols are more useful once they can reference type parameters

### Alternative F: Angle-Bracket Protocol Type Wrapper (`<Printable>`)

Use `<Printable>` in type annotations to distinguish protocol types from class types, following Strongtalk convention.

```beamtalk
display: thing :: <Printable> => ...   // structural check
deposit: amount :: Integer => ...      // nominal check
```

**Rejected because:**
- Protocols and classes share a namespace — the compiler already knows whether `Printable` is a protocol or a class from name resolution
- The wrapper is redundant information — it tells the compiler what it already knows
- TypeScript and Swift both use bare names for interface/protocol types without any wrapper
- Fewer syntax concepts to learn — one way to reference types, not two

## Consequences

### Positive
- `Result unwrap` returns the actual wrapped type instead of `Object` — unlocks downstream type checking and IDE completions
- `Array`, `Dictionary`, and other container types gain element-type tracking
- Generic annotations generate precise Dialyzer `-spec` attributes (concrete types instead of `any()`)
- Protocols formalize Smalltalk's informal "responds to these messages" contracts
- Both features are purely additive — all existing untyped code works unchanged
- Forward-compatible: invariant generics can be extended with variance; unbounded params can gain protocol bounds
- Removes the escape hatches in the type checker (`_ => return`, `contains('<')` bypass)
- `<` remains exclusively a binary message — no dual meaning anywhere in the language

### Negative
- Generics add complexity to the type checker — substitution, constructor inference, and generic-aware method resolution
- Class definition syntax becomes slightly more complex for generic classes (`Result(T, E)`)
- Two-letter naming convention for type parameters (`T`, `E`, `K`, `V`) is implicit — could be confusing without documentation
- Protocol conformance checking on large class hierarchies has compilation performance implications
- Structural conformance can produce surprising results — a class may accidentally conform to a protocol it knows nothing about
- Generic method signatures are more complex to read: `map: block :: Block(T, R) -> Result(R, E)`

### Neutral
- Generated BEAM bytecode is unchanged — type erasure means zero runtime cost
- Existing tests don't need modification
- Dynamic features (`doesNotUnderstand:`, runtime class creation) continue working unchanged
- The `typed` class modifier works orthogonally — `typed` generic classes get thorough checking, non-typed ones get inference only

## Implementation

### Stage 1: Parametric Types (Size: L)

**Phase 1a: Parser and AST (M)**
- Add `type_params: Vec<Identifier>` to `ClassDefinition` in `ast.rs`
- Parse `(T, E)` after class name in `parse_class_definition` (declarations.rs)
- Parse `Collection(Integer)` in `parse_single_type_annotation` — consume `(` `)` with nested type annotations
- Update `skip_double_colon_type` lookahead to handle `Name(...)` sequences
- Add `type_params: Vec<EcoString>` to `ClassInfo` in class_hierarchy
- Update `TypeAnnotation::Generic` delimiter semantics from angle brackets to parentheses

**Phase 1b: Type Checker Substitution (L)**
- Build substitution map: when encountering `Result(Integer, IOError)`, map `{T → Integer, E → IOError}`
- Apply substitution to method return types during inference: `unwrap -> T` becomes `unwrap -> Integer`
- Apply substitution to parameter types during validation
- Replace escape hatches: `_ => return` → generic-aware matching; `contains('<')` → structural comparison
- Handle `set_param_types` for generic annotations: extract base type and substitute

**Phase 1c: Constructor Inference (M)**
- For `Result ok: 42`, infer `T = Integer` from the argument type
- For `Result error: #not_found`, infer `E = Symbol` from the argument type
- Store inferred type params on the expression's `InferredType`

**Phase 1d: Spec Codegen (S)**
- Extend `spec_codegen.rs` to expand generic types with concrete substitutions
- Map type parameters to Dialyzer type expressions
- Unresolved parameters → `any()`

**Phase 1e: Stdlib Annotations (M)**
- Update `Result.bt`: `Result(T, E)`, fields `:: T` / `:: E`, method return types
- Update collection classes: `Array(E)`, `Dictionary(K, V)`, `Set(E)`
- Update `Block` types if applicable

### Stage 2: Structural Protocols (Size: L)

**Phase 2a: Protocol AST and Parser (M)**
- Add `ProtocolDefinition` to `ast.rs`: name, type params, required methods with signatures, span
- Parse `Protocol define: Name requiring: [...]` syntax
- Protocol names resolve to protocol objects in the same namespace as classes

**Phase 2b: Protocol Registry and Conformance (L)**
- Protocol registry in `semantic_analysis/`: maps protocol names → required message sets
- Conformance engine: check class method tables against protocol requirements
- Type checker integration: when a type annotation resolves to a protocol name, perform structural conformance checking instead of nominal class checking
- Handle generic protocols: `Collection(Integer)` conformance substitutes `E → Integer` in required signatures

**Phase 2c: Runtime Queries (M)**
- `conformsTo:` primitive on class objects
- `protocols` primitive returning conforming protocol list
- `requiredMethods` on protocol objects
- Protocol metadata compiled into module attributes

**Phase 2d: Type Parameter Bounds (S)**
- Allow `T :: Printable` in class/protocol type parameter declarations
- Type checker verifies that concrete type arguments conform to the bound

### Implementation Tracking

| Phase | Description | Size | Dependencies |
|-------|-------------|------|--------------|
| 1a | Parser and AST for generic types | M | None |
| 1b | Type checker substitution | L | 1a |
| 1c | Constructor type inference | M | 1b |
| 1d | Dialyzer spec generation for generics | S | 1a |
| 1e | Stdlib generic annotations | M | 1a, 1b |
| 2a | Protocol AST and parser | M | None (parallel with 1) |
| 2b | Protocol registry and conformance | L | 2a, 1b |
| 2c | Runtime protocol queries | M | 2b |
| 2d | Type parameter bounds | S | 1b, 2b |

## Migration Path

No migration is required. Both features are purely additive:

- **Generics:** Existing classes without type parameters continue working unchanged. Adding `(T, E)` to a class definition is opt-in. Existing `:: Result` annotations remain valid (equivalent to `:: Result(Dynamic, Dynamic)`).
- **Protocols:** Protocol definitions are new syntax. Existing code is not affected. Conformance is automatic — classes don't need `implements:` declarations.

The only stdlib change is updating `Result.bt` and collection classes to declare their type parameters and update field/method annotations. This is a non-breaking change — the runtime behavior is identical (type erasure).

## References

- Related issues: [BT-1157](https://linear.app/beamtalk/issue/BT-1157) — Epic: Phase 3 & 4 Type System
- Related ADRs:
  - [ADR 0025: Gradual Typing and Protocols](0025-gradual-typing-and-protocols.md) — Foundation; Phases 1-2 complete
  - [ADR 0053: Double-Colon Type Annotation Syntax](0053-double-colon-type-annotation-syntax.md) — `::` delimiter
  - [ADR 0060: Result Type](0060-result-type-hybrid-error-handling.md) — Motivating use case for generics
  - [ADR 0032: Early Class Protocol](0032-early-class-protocol.md) — Class introspection foundation
- Prior art:
  - [Strongtalk: Typechecking Smalltalk in a Production Environment](https://bracha.org/oopsla93.pdf) — Bracha & Griswold, 1993
  - [TypeScript Generics](https://www.typescriptlang.org/docs/handbook/2/generics.html) — Inference model
  - [Swift Protocols](https://docs.swift.org/swift-book/LanguageGuide/Protocols.html) — Associated types
  - [Gleam Type System](https://gleam.run/frequently-asked-questions/) — Parenthesis generics on BEAM
  - [Elixir Protocols](https://hexdocs.pm/elixir/protocols.html) — Nominal protocols on BEAM
- Documentation: `docs/beamtalk-language-features.md`, `docs/internal/type-system-design.md`
