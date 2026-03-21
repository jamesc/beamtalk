# ADR 0068: Parametric Types and Protocols

## Status
Accepted (2026-03-20)

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
  size -> Integer
  do: block :: Block
  collect: block :: Block -> Self   // collect: returns... Self of what element type?

// With generics, we can express the relationship
Protocol define: Collection(E)
  size -> Integer
  do: block :: Block(E, Object)
  collect: block :: Block(E, Object) -> Self
```

Generics are a prerequisite for expressive protocols. This ADR therefore addresses **generics first, then protocols**, as an integrated type system extension.

### Current Infrastructure

The AST already defines `TypeAnnotation::Generic { base, parameters, span }` but it is **never produced by the parser** — only constructible programmatically. The type checker has two escape hatches that bail out on non-simple types:

- `check_return_type` (validation.rs:251): `_ => return` skips any union/generic return type
- `is_assignable_to` (validation.rs:492): string-level `contains('<')` and `contains('|')` bypasses — any type containing `<`, `|`, or `#` is unconditionally accepted
- `set_param_types` (inference.rs:147): non-`Simple` annotations → `Dynamic`

`ClassDefinition` has no `type_params` field. Classes cannot declare themselves as generic.

### Constraints

1. **Type erasure** (ADR 0025): All type information is compile-time only. Zero runtime cost.
2. **Warnings, not errors** (ADR 0025): Type mismatches produce warnings, never block compilation.
3. **Gradual adoption**: Untyped code must continue working unchanged. `Result ok: 42` without annotations stays valid.
4. **BEAM integration**: Generic annotations should generate Dialyzer `-spec` attributes for FFI/interop (see Dialyzer section below).
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
// Annotating a method parameter
processResult: r :: Result(Integer, Error) -> Integer =>
  r unwrap + 1   // ✅ r unwrap is Integer, Integer has '+'

// Annotating a method return type (propagates to all callers)
readConfig -> Result(String, IOError) => File read: "config.json"

// Annotating state
Actor subclass: Cache(K, V)
  state: store :: Dictionary(K, V) = Dictionary new
```

#### Type Inference Through Generics

The type checker performs **positional substitution**: when it encounters `Result(String, IOError)`, it maps `T → String`, `E → IOError`, and substitutes through all method signatures of `Result`:

```beamtalk
// computeSomething declares -> Result(Integer, Error)
r := computeSomething
r unwrap          // Return type T → Integer ✅
r map: [:v | v asString]   // Block param T → Integer, return Result(String, Error)
r error           // Return type E → Error ✅
```

When the concrete type parameters are unknown — because the value comes from a method whose return type is bare `Result` with no type params — they fall back to `Dynamic`, preserving the current behavior:

```beamtalk
// someMethod's return type is just -> Result (no type params declared)
r := someMethod
r unwrap                   // → Dynamic (T is unknown — no inference context)
r unwrap + 1               // No warning — Dynamic bypasses checking
// 💡 Hint: someMethod returns unparameterized Result — consider annotating
//    its return type (-> Result(Integer, Error)) to enable type checking

// Fix: annotate the method's return type (propagates to all callers)
someMethod -> Result(Integer, Error) => ...
r := someMethod
r unwrap + 1               // ✅ Integer has '+'
```

Note that constructor calls like `Result ok: 42` **do** infer type params from their arguments (see Constructor Type Inference below). The Dynamic fallback only applies when type params are genuinely unknowable — typically from unparameterized method return types or Erlang FFI calls.

#### Constructor Type Inference

For named constructors (`ok:`, `error:`, `new`), the compiler infers type parameters from the argument types:

```beamtalk
r := Result ok: 42                  // Inferred: Result(Integer, Dynamic)
r unwrap                            // → Integer ✅

r2 := Result error: #file_not_found // Inferred: Result(Dynamic, Symbol)
r2 error                            // → Symbol ✅
```

This is limited to direct constructor calls with literal or already-typed arguments. Complex expressions fall back to `Dynamic` parameters.

#### Dialyzer Spec Generation (FFI/Interop Boundary)

Dialyzer specs serve the **BEAM interop boundary**, not pure Beamtalk code. For Beamtalk-to-Beamtalk calls, the Beamtalk type checker is the primary tool — it understands class hierarchies, message sends, sealed classes, and `doesNotUnderstand:` overrides. Dialyzer sees only BEAM bytecode (`gen_server:call`, map operations) and knows none of this.

Dialyzer specs are valuable when:
- **Erlang/Elixir calls Beamtalk** — Beamtalk actors are `gen_server` modules; specs let Dialyzer verify that callers pass the right types at the BEAM boundary
- **Beamtalk calls Erlang/Elixir** — specs on the Erlang side let Dialyzer cross-check against what Beamtalk passes in (where Beamtalk's own type checker returns `Dynamic`)
- **Mixed-language projects** — Dialyzer provides the common type language across BEAM languages

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

#### Runtime Type Representation

Beamtalk does **not** fully erase types at the BEAM level. Every compiled class exports `__beamtalk_meta/0` containing `method_info` maps with `return_type` and `param_types` entries (used by REPL chain completion, `:help`, and `CompiledMethod` introspection — see ADR 0045). This means generic type information must survive into the runtime representation, or introspection will lie about method signatures.

**Parameterized methods store type parameter references**, not erased `none`:

```erlang
%% __beamtalk_meta/0 for Result(T, E)
#{method_info => #{
    'unwrap' => #{arity => 0, return_type => {type_param, 'T', 0}, ...},
    'error'  => #{arity => 0, return_type => {type_param, 'E', 1}, ...},
    'map:'   => #{arity => 1,
                  return_type => {generic, 'Result', [{type_param, 'R', -1}, {type_param, 'E', 1}]},
                  param_types => [{generic, 'Block', [{type_param, 'T', 0}, {type_param, 'R', -1}]}],
                  ...}
  },
  type_params => ['T', 'E'],   %% declared type parameter names
  ...
}
```

The `{type_param, Name, Index}` tagged tuple preserves the parameter name and its position in the class's type parameter list (`-1` for method-local params like `R` in `map:`). This enables:

- **`:help` display**: `Result >> unwrap` shows `unwrap -> T` (not `unwrap -> Object` or `unwrap -> ???`)
- **REPL chain completion**: When the workspace knows `r :: Result(Integer, Error)`, it substitutes `T → Integer` into `{type_param, 'T', 0}` to resolve `r unwrap` as `Integer` and offer Integer completions
- **`CompiledMethod` introspection**: Future `returnType` / `paramTypes` messages can return the parameterized form for tooling
- **Fallback to Dynamic**: When concrete type params are unknown, `{type_param, ...}` entries are treated as Dynamic — same behavior as `none` today, but with the information preserved for when context is available

The `method_return_types` map on the class gen_server (used for fast chain-completion lookups) stores the tagged tuples directly. The chain-resolution code in `beamtalk_repl_ops_dev.erl` gains a substitution step: if it encounters `{type_param, _, Index}`, it looks up the concrete type from the caller's annotation context.

**This is NOT reified generics** (Java's alternative to erasure). There are no runtime type checks, no generic type tags on instances, no `instanceof Result(Integer, Error)`. The type parameter metadata lives on the *class* (in `method_info`), not on *instances*. It's introspection data for tooling, not a runtime type system.

**Contrast with Java's erasure problem:** Java erased generics from `.class` files but kept `instanceof`, `getClass()`, and reflection APIs that expected to find them — a mismatch. Beamtalk stores type params in `method_info` (which tooling already reads) and doesn't pretend they're absent. No mismatch, no lies.

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

#### Union Type Checking

Union types (`Integer | String`, `String | False`) are already parsed and stored in the AST as `TypeAnnotation::Union`. The type checker currently skips them (`_ => return`). Stage 1 adds proper union checking: a message send on a union-typed value warns unless **all** members of the union respond to that selector.

```beamtalk
// All members must respond to the message
x :: Integer | String := getValue
x asString             // ✅ Both Integer and String have asString
x size                 // ⚠️ Warning: Integer does not respond to 'size'
                       //    (String does, but Integer doesn't)
x + 1                  // ⚠️ Warning: String does not respond to '+'
                       //    (Integer does, but String doesn't)
```

The nullable pattern (`String | nil`) is the most common union — it's Beamtalk's Option/Maybe type. `nil` in type position resolves to `UndefinedObject` (the singleton's class), just as `nil` in expression position evaluates to the singleton instance. Without union checking, these are invisible to the type system:

```beamtalk
name :: String | nil := dictionary at: "name"
name size              // ⚠️ Warning: UndefinedObject does not respond to 'size'
                       //    Hint: check for nil before sending 'size'
```

Similarly, `false` in type position resolves to `False` — used for Erlang FFI patterns where functions return `false` on failure:

```beamtalk
entry :: Tuple | false := ErlangLists keyfind: key   // lists:keyfind returns false on miss
```

**Union + narrowing compose** — this is where both features pay off together:

```beamtalk
name :: String | nil := dictionary at: "name"
name isNil ifTrue: [^"unknown"]
name size              // ✅ name is narrowed to String — nil eliminated by early return
```

**Return type of union message sends:** When a message is valid on all union members but returns different types, the return type is the union of return types. `(Integer | Float) abs` returns `Integer | Float` (both have `abs` returning their own type). If all members return the same type, the return type is that type: `(Integer | String) asString` returns `String`.

**Union representation in `InferredType`:** Unions are represented as a new variant alongside `Known` and `Dynamic`:

```rust
enum InferredType {
    Known { class_name: EcoString, type_args: Vec<InferredType>, provenance: TypeProvenance },
    Union { members: Vec<InferredType>, provenance: TypeProvenance },
    Dynamic,
}
```

#### Control Flow Narrowing (Simple Cases)

When the type checker recognises a type-testing message send followed by `ifTrue:` / `ifFalse:`, it narrows the variable's type inside the block scope:

```beamtalk
// class identity check — narrows to exact class
process: x :: Object =>
  x class = Integer ifTrue: [
    x + 1          // ✅ x is Integer here — has '+'
  ]
  x + 1            // ⚠️ x is Object here — no narrowing outside the block

// kind check — narrows to class including subclasses
process: x :: Object =>
  x isKindOf: Number ifTrue: [
    x abs           // ✅ x is Number here
  ]

// early return narrows the rest of the method
validate: x :: Object =>
  x isNil ifTrue: [^nil]
  x doSomething    // ✅ x is non-nil for the remainder — narrowed by early return
```

**Supported narrowing patterns (Stage 1):**

| Pattern | Narrows to | Scope |
|---|---|---|
| `x class = Foo ifTrue: [...]` | `x` is `Foo` in true block | True block only |
| `x isKindOf: Foo ifTrue: [...]` | `x` is `Foo` in true block | True block only |
| `x isNil ifTrue: [^...]` | `x` is non-nil after the statement | Rest of method |
| `x isNil ifTrue: [^...] ifFalse: [...]` | `x` is non-nil in false block | False block |

**Not supported in Stage 1:**
- `respondsTo:` narrowing (requires protocol integration — Stage 2)
- False-branch complement types (`ifFalse:` knowing "x is NOT Integer" — requires difference types)
- Narrowing through `and:`/`or:` chains
- Narrowing stored in variables (`isValid := x class = Integer; isValid ifTrue: [...]`)

These can be added incrementally — each new pattern is a new AST shape to recognise, not a new mechanism.

#### What Is NOT Included in Stage 1

- **Variance rules** (covariance/contravariance): All type parameters are invariant in Stage 1. This is fine because Beamtalk's class hierarchy is shallow with sealed leaf types — `Integer`, `String`, `Float` etc. have no subclasses, so `Array(Integer)` vs `Array(Number)` rarely arises. **Variance becomes necessary in Stage 2** when protocol types create meaningful subtyping: `Array(Integer)` passed where `Array(Printable)` is expected requires covariance. Stage 2 must address variance alongside protocol-typed generic parameters — it is not indefinitely deferred.
- **Type parameter bounds/constraints** (`T :: Printable`): Deferred to Stage 2 (protocols). Type parameters are unbounded — any type is accepted.
- **Higher-kinded types** (`F(_)`): Not planned. Beamtalk is not Haskell.
- **Chain-backwards inference**: `r unwrap + 1` does NOT infer that `T = Integer` by reasoning backwards from `+`. Type params are inferred from constructor arguments and method arguments at call sites (forward/call-site inference), not from downstream usage of return values.
- **Local variable type annotations** (`x :: Result(Integer, Error) := expr`): Deliberately not implemented. The ADR originally proposed this syntax as an escape hatch for when inference can't determine types. In practice, the right fix is to annotate the *method return type* at the source (`someMethod -> Result(Integer, Error) =>`), which propagates to all callers automatically. Variable annotations would add parser complexity for a case better solved at declaration sites. Can be revisited if a genuine need arises that can't be solved by method/parameter/state annotations.
- **Full bidirectional type checking**: No expected-type propagation from assignment targets into subexpressions. This could be added later without breaking changes.

### Design Challenges

The following challenges were identified during design and have specific solutions. These are not deferred — they must be addressed in Stage 1.

#### Challenge 1: Method-Local Type Parameters (the `R` Problem)

`Result`'s `map:` method introduces a type variable `R` that is **not** a type parameter of `Result`:

```beamtalk
sealed map: block :: Block(T, R) -> Result(R, E) =>
  self.isOk ifTrue: [Result ok: (block value: self.okValue)] ifFalse: [self]
```

`R` is the block's return type — unknown until the call site. This is effectively a generic method, not just a generic class.

**Solution: Implicit method-local type params via call-site inference.** Any identifier in type position that is neither a known class/protocol name nor a class-level type parameter is treated as a method-local type parameter. Its value is inferred from the arguments at each call site:

```beamtalk
r :: Result(Integer, Error) := computeSomething

// At this call site, the block returns String, so R = String
r map: [:v | v asString]
// Type checker infers: Block(Integer, String) → R = String → Result(String, Error)

// At this call site, the block returns Integer, so R = Integer
r map: [:v | v + 1]
// Type checker infers: Block(Integer, Integer) → R = Integer → Result(Integer, Error)
```

This is lightweight call-site unification — the type checker matches the argument types against the parameter's generic type to solve for unknown variables. It is **not** full Hindley-Milner inference; it only solves variables that appear in parameter positions and can be determined from the provided arguments.

When a method-local type param cannot be inferred (no matching argument), it falls back to `Dynamic`:

```beamtalk
// R cannot be inferred if the block is stored in a variable
myBlock := [:v | v asString]
r map: myBlock    // Block type params unknown → R = Dynamic → Result(Dynamic, Error)
```

#### Challenge 2: Block Is Not a Normal Generic Class

`Block` is `sealed Object subclass: Block` with `@intrinsic` methods. Blocks take 0, 1, or 2+ arguments — they have **variable-arity type parameters**, which can't be expressed as a fixed class-level `Block(A, R)`.

**Solution: Block type params are special-cased in the type checker.** `Block(...)` in a type annotation is not treated as a regular generic class application. Instead, the type checker interprets it as:

- `Block(R)` — zero-argument block returning `R`
- `Block(A, R)` — one-argument block with arg type `A`, returning `R`
- `Block(A, B, R)` — two-argument block with arg types `A` and `B`, returning `R`

The last type parameter is always the return type; all preceding ones are argument types. `Block.bt` itself is not modified to declare type params — the type checker handles Block as a built-in generic form, similar to how TypeScript treats function types `(a: A) => R` specially rather than as a generic class.

This special-casing is limited to `Block` only. All other generic types are regular declaration-site generics.

#### Challenge 3: Self Type with Generic Type Arguments

The existing `Self` return type resolves to the receiver's class name via string comparison. With generics, `Self` must carry type arguments:

```beamtalk
Collection(E) subclass: Array(E)
  // Inherited from Collection: select: -> Self
  // For Array(Integer), Self should be Array(Integer), not bare Array

arr :: Array(Integer) := Array withAll: #[1, 2, 3]
filtered := arr select: [:x | x > 1]
// filtered should be Array(Integer), not Array(Dynamic)
```

**Solution: Extend `InferredType::Known` to carry optional type arguments and provenance.**

```rust
/// Tracks where a type came from — enables precise error messages
/// and determines how far inference should propagate.
enum TypeProvenance {
    Declared(Span),      // user wrote :: Type at this location
    Inferred(Span),      // compiler inferred from expression at this location
    Substituted(Span),   // derived from a generic substitution at this location
}

enum InferredType {
    Known {
        class_name: EcoString,
        type_args: Vec<InferredType>,  // empty for non-generic types
        provenance: TypeProvenance,
    },
    Union {
        members: Vec<InferredType>,    // e.g., [Known("String"), Known("False")]
        provenance: TypeProvenance,
    },
    Dynamic,
}
```

**Provenance** tracks whether a type was explicitly declared by the user, inferred by the compiler, or derived from a generic substitution. This serves two purposes:

1. **Error messages distinguish declared vs inferred types:**

   ```beamtalk
   // Declared — user owns the assertion
   x :: Result(Integer, Error) := someMethod
   x unwrap ++ "hello"
   // ⚠️ Warning: Integer does not respond to '++'
   //    Note: x declared as Result(Integer, Error) on line 1

   // Inferred — compiler guessed, user can override
   x := Result ok: 42
   x unwrap ++ "hello"
   // ⚠️ Warning: Integer does not respond to '++'
   //    Note: x inferred as Result(Integer, Dynamic) from constructor on line 1
   //    Hint: add a type annotation if this inference is wrong
   ```

2. **Expected-type propagation uses provenance to calibrate confidence.** Both declared and inferred types propagate forward through chains, but error messages always trace back to the origin and tell the user whether they wrote the type or the compiler guessed it. This lets users decide whether to trust the inference or anchor it with an explicit annotation.

When resolving `Self` for a receiver with known type args, the type args propagate:
- Receiver `Array(Integer)` + return type `Self` → `Array(Integer)`
- Receiver `Result(String, Error)` + return type `Self` → `Result(String, Error)`

This also enables generic type flow through chains: `r map: [:v | v asString]` returns `Result(String, Error)`, and further calls on that result carry `String` and `Error` through.

#### Challenge 4: Generic Inheritance and Superclass Type Application

When a generic class extends another, the type parameter mapping must be explicit:

```beamtalk
// Array passes its E to Collection's E
Collection(E) subclass: Array(E)

// IntArray fixes E to Integer
Collection(Integer) subclass: IntArray

// SortedArray passes E through and adds a constraint (Stage 2)
Array(E) subclass: SortedArray(E)
```

The superclass in the class header is now a **type application**, not just a name. When `arr :: Array(Integer)` calls a method inherited from `Collection`, the type checker must:
1. Know that `Array(E)` extends `Collection(E)` — so Array's `E` maps to Collection's `E`
2. Substitute `E → Integer` through Collection's method signatures too

**Solution: Store the superclass type application in `ClassInfo`.**

```rust
struct ClassInfo {
    // ... existing fields ...
    type_params: Vec<EcoString>,
    superclass_type_args: Vec<TypeParamMapping>,  // how our params map to super's params
}
```

For `Collection(E) subclass: Array(E)`, `Array`'s `superclass_type_args` records that `E` (position 0) maps to Collection's position 0. For `Collection(Integer) subclass: IntArray`, the mapping is a concrete type, not a param reference.

When the type checker resolves an inherited method, it composes the substitution: caller's type args → current class's params → superclass's params.

#### Challenge 5: Constructor Inference Bridges Class and Instance

`Result ok: 42` calls a **class method** where the parameter is `:: T`. But `T` is a type param of `Result` **instances**. The class object itself is not parameterized — there's no `Result(Integer, Error)` class object.

**Solution: Class methods that reference instance type params trigger inference.** The type checker recognises that `T` in a class method's parameter type refers to the enclosing class's type params. When `ok:` receives an Integer argument, the checker infers `T = Integer` and returns `Result(Integer, Dynamic)` as the inferred type of the expression.

This works because class methods conceptually construct instances — they are the bridge between the unparameterized class object and parameterized instances. The type checker treats class method calls on generic classes as implicit type application sites.

#### Challenge 6: Control Flow Narrowing Through Blocks

TypeScript narrows types inside `if` branches — lexical scopes that the compiler directly controls. In Beamtalk, `ifTrue:` takes a **block argument** — a closure. The type checker needs to recognise that certain message patterns create narrowing contexts and thread the narrowed type into the block's scope.

**Solution: Pattern-match on the AST shape, not on general message semantics.** The type checker recognises a fixed set of narrowing idioms:

1. When visiting a message send like `[expr] class = [ClassName] ifTrue: [block]`, the checker:
   - Identifies the cascade: binary send `class =` producing a Boolean, followed by `ifTrue:` with a block argument
   - Determines which variable `expr` refers to
   - Pushes a scope refinement `{variable → ClassName}` into the block's scope before type-checking the block body

2. For early-return narrowing (`x isNil ifTrue: [^nil]`), the checker:
   - Recognises the block contains a non-local return (`^`)
   - After the statement, pushes the complement refinement (x is non-nil) into the current method scope for all subsequent statements

This is **not** a general narrowing framework — it's a small set of recognised patterns. Each new pattern (e.g., `respondsTo:` in Stage 2) is a new case in the pattern matcher, not a new mechanism. The type checker already walks the AST and already has scoped environments; narrowing adds refinement entries to those environments.

#### Challenge 7: @primitive Methods in Generic Classes

`Array(E)`, `Dictionary(K, V)`, and `Block` methods are `@primitive` — dispatched to Erlang functions that know nothing about type parameters. The type params exist only in annotations.

**This is fine** — type erasure means the runtime dispatch is unchanged. The type checker uses the annotations for checking and inference, and codegen generates the same primitive dispatch as today. The `__beamtalk_meta/0` function carries the type param metadata for tooling, but the actual method dispatch ignores it completely. This is exactly the same as how `typed` classes generate identical bytecode to non-typed classes.

### Stage 2: Structural Protocols

#### Protocol Definition

Protocols define named message sets. A class conforms to a protocol if it responds to all required messages — no `implements:` declaration needed.

```beamtalk
Protocol define: Printable
  /// Return a human-readable string representation.
  asString -> String

Protocol define: Comparable
  < other :: Self -> Boolean
  > other :: Self -> Boolean
  <= other :: Self -> Boolean
  >= other :: Self -> Boolean

Protocol define: Collection(E)
  /// The number of elements in this collection.
  size -> Integer

  /// Iterate over each element, evaluating the block for side effects.
  do: block :: Block(E, Object)

  /// Transform each element, returning a new collection of the same kind.
  collect: block :: Block(E, Object) -> Self

  /// Return elements matching the predicate.
  select: block :: Block(E, Boolean) -> Self
```

Protocol bodies use **class-body style** — method signatures without `=>` implementations. This supports parameter names, type annotations, return types, and doc comments on each required method. The parser distinguishes protocol method signatures from class method definitions by the absence of `=>`.

Protocol names are **bare identifiers** (uppercase, like class names). Protocols and classes share a single namespace — having both a class and a protocol named `Printable` is a compile error.

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

Conformance checking uses a **three-tier model** depending on what information is available:

**Tier 1: Compile-time ClassHierarchy (batch compilation).** The type checker walks the full superclass chain via `ClassHierarchy` — statically defined methods only. Methods added at runtime (`Counter >> newMethod => ...`) are invisible. This is the same method table used for dispatch and existing type inference.

**Tier 2: REPL workspace (live development).** In the REPL, the workspace tracks live method additions and class redefinitions. Conformance checking in the REPL can use the runtime method table for more accurate results than batch compilation.

**Tier 3: `doesNotUnderstand:` bypass.** Classes that override `doesNotUnderstand:` can respond to *any* message — they structurally conform to every protocol. The conformance checker treats these classes as conforming without checking individual selectors, matching the existing ADR 0025 rule that suppresses unknown-message warnings for DNU classes.

```beamtalk
// Tier 1: compile-time — walks full hierarchy
display: "hello"           // ✅ String has asString (from Object hierarchy)
display: 42                // ✅ Integer has asString (from Object hierarchy)
display: Counter spawn     // ✅ Counter has asString (inherited from Object)

// Tier 3: DNU bypass — conforms to everything
Actor subclass: Proxy
  doesNotUnderstand: msg => self.target forward: msg
display: Proxy spawn       // ✅ No warning — Proxy has DNU override
```

When conformance cannot be verified (Dynamic values, runtime-constructed classes):

```beamtalk
display: someUnknownValue
// ⚠️ Warning: cannot verify Printable conformance for Dynamic value
```

#### Diagnostic Philosophy: Warnings, Never Errors

The type system — including generics and protocol conformance — **never produces errors** (ADR 0025). Code always compiles and runs. This principle extends unchanged to all features in this ADR:

| Situation | Diagnostic | Severity |
|---|---|---|
| Unknown message send | "Counter does not respond to 'foo'" | Warning |
| Protocol conformance unverifiable | "cannot verify Printable conformance" | Warning |
| Type mismatch in argument | "expected Integer, got String" | Warning |
| Missing annotation in `typed` class | "untyped parameter in typed class" | Warning |
| Unparameterized generic return | "someMethod returns unparameterized Result" | Warning + hint |
| Namespace collision (class + protocol) | "Printable is already defined as a class" | **Error** (structural) |
| Invalid type param reference | "T is not a type parameter of this class" | **Error** (structural) |
| Malformed protocol definition | "expected method signature" | **Error** (parse) |

Only parse and structural errors block compilation — never type checking results. The escalation model:

- **No types** → no warnings, fully dynamic, everything works as today
- **Add annotations** → get helpful warnings where the checker can verify
- **Add protocols** → get conformance warnings when shapes don't match
- **Add `typed` modifier** → get completeness warnings for missing annotations
- **Never blocked** → code always compiles and runs

ADR 0025 rejected a **language-level** strict mode (where the same code behaves differently depending on a compiler flag). The language always produces warnings, never errors, for type issues. However, the **build pipeline** enforces warnings via `--warnings-as-errors` on `test-stdlib`, `test-docs`, and `test-examples` (PR #1567). This is the adoption forcing function:

- **Dev time / REPL**: Warnings only — experiment freely, nothing blocks you
- **CI / `just test`**: `--warnings-as-errors` — type warnings fail the build, you must fix or annotate
- **Same language everywhere**: The compiler always emits warnings. The build system decides whether warnings are acceptable to merge.

This avoids TypeScript's `--strict` problem (same code, different behavior per config) while providing the enforcement that drives adoption. The forcing function is in the build pipeline, not the language semantics. Every new type checking feature in this ADR (generics, unions, narrowing, protocols) produces warnings that `--warnings-as-errors` will enforce in CI automatically — no additional configuration needed.

#### Runtime Protocol Queries

```beamtalk
> Integer conformsTo: Printable
=> true

> Integer protocols
=> #(Printable, Comparable)

> Printable requiredMethods
=> #(#asString)

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
  /// The key used for sort ordering.
  sortKey -> Object
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
- **Generics:** "When I call Beamtalk actors from Erlang, Dialyzer now sees proper specs with concrete types instead of `any()`." Generic annotations produce expanded `-spec` at the FFI boundary — this is where Dialyzer adds real value, not for pure Beamtalk code (where Beamtalk's own type checker is more capable).
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

Parse `Result(Integer, IOError)` at usage sites but don't add type params to class definitions. Hardcode substitution rules for known stdlib types (Result, Array, Dictionary, Set, Block).

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "I only need to write `:: Result(Integer, Error)` — I never have to define generic classes myself. Less to learn." |
| **Smalltalk purist** | "This keeps class definitions pure Smalltalk — no parenthesized params polluting class headers. The type system stays invisible to class authors." |
| **BEAM veteran** | "Generates the same Dialyzer specs at the FFI boundary with dramatically less compiler complexity. Fewer moving parts in the compiler means fewer compiler bugs." |
| **Operator** | "For the next 2 years, only stdlib types need generics. Hardcoding 5 substitution rules is less risky than building a generics engine that might have subtle bugs." |
| **Language designer** | "YAGNI — we only need Result, Array, Dictionary, Set, and Block. That's 5 types, not a generics system. If users eventually need custom generic classes, we can upgrade then — the annotation syntax is forward-compatible." |

**Why rejected:** The YAGNI argument is genuinely compelling for the short term. But the type checker becomes a bag of special cases rather than a principled system — each new generic type requires new hardcoded rules instead of falling out from a general mechanism. More importantly, user-defined generic classes (e.g., `Stack(E)`, `Cache(K, V)`) would be impossible without upgrading to declaration-site generics. The annotation syntax is forward-compatible, but the type checker internals would need a rewrite.

### Option B: Full Parametric Polymorphism (Rejected)

Complete generics with variance annotations (`+T` covariant, `-T` contravariant), bounded quantification, generic methods with explicit type params, and type parameter inference from usage patterns.

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "In TypeScript and Kotlin, `List<Integer>` is assignable to `List<Number>`. If Beamtalk's invariant generics reject this, I'll hit a wall fast and blame the type system." |
| **Smalltalk purist** | "If you're going to impose a type system on Smalltalk, at least make it sound. Half-measures are worse than nothing — they give false confidence." |
| **BEAM veteran** | "Gleam ships full HM inference with zero annotations needed. If Beamtalk's generics require annotations that Gleam infers automatically, we'll look like the inferior BEAM typed language." |
| **Operator** | "Invariant generics mean I can't pass `Array(Integer)` where `Array(Number)` is expected — I'll end up casting everything, which defeats the purpose of types." |
| **Language designer** | "Variance is the difference between a toy type system and a real one. Without it, every container type hits a wall at subtyping boundaries. Adding variance later means migrating every existing generic annotation — it's not as painless as the ADR claims." |

**Why rejected:** The variance argument is the strongest — invariant generics *will* surprise users at subtyping boundaries. However: (1) Beamtalk's class hierarchy is shallow (most types are sealed leaf classes), so subtyping boundaries are rare in practice; (2) variance can be added later as an *extension* to existing invariant generics — existing code remains valid, it just gains more permissive assignability; (3) the implementation cost of variance (XL+) would delay everything else in this ADR by months. The pragmatic path is: ship invariant generics, collect real-world evidence of where variance is needed, then add it with data rather than speculation.

### Option C: Pragmatic Declaration-Site Generics (Chosen)

Classes declare type parameters. Methods reference them. Type checker substitutes. No variance, no bounds (until protocols land), no HKTs. Union checking and simple control flow narrowing included.

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "`Result(Integer, Error)` reads cleanly — like Gleam or Python type hints. Union checking catches my `String | nil` mistakes. Narrowing means I don't need verbose null-checking patterns." |
| **Smalltalk purist** | "Type params are optional — my untyped code doesn't change at all. Protocols formalize what I already do informally. The structural conformance model IS duck typing, just explicit." |
| **BEAM veteran** | "Generates proper Dialyzer specs at the FFI boundary. Union types match Erlang's `{ok, V} \| {error, R}` pattern naturally." |
| **Operator** | "Zero runtime cost, same bytecode, same observability. The type system helps my team catch bugs without any production impact." |
| **Language designer** | "Simple enough to implement well, extensible to variance/bounds later. Union checking and narrowing compose into something genuinely useful without HM complexity." |

### Option D: Generics Only — Defer Protocols, Unions, and Narrowing (Not Chosen)

Ship only Stage 1 (generic type params and substitution). Defer protocols, union checking, and narrowing to separate ADRs.

| Cohort | Strongest argument |
|--------|-------------------|
| **Operator** | "Smaller scope = less risk. Ship generics, prove they work, then add protocols. One big ADR with 10 implementation phases is a recipe for scope creep." |
| **Language designer** | "Protocols are a separate concept from generics. Bundling them forces both to ship together — if protocols slip, generics slip too. Decouple them." |
| **BEAM veteran** | "Elixir shipped protocols and generics independently. They don't have to be one thing." |

**Why not chosen:** Generics without union checking leaves a hole — `String | nil` is the most common type annotation, and without union checking it's decoration. Narrowing without unions is pointless. Protocols without generics can't express `Collection(E)`. The features compose into something greater than the sum — shipping them separately means each is less useful in isolation. However, the staged implementation (Stage 1 before Stage 2) does allow generics + unions + narrowing to ship before protocols if needed.

### Tension Points

- **Smalltalk purists** would prefer no generics at all — but they accept that type annotations are optional and generics only appear inside them.
- **Language designers** would prefer variance rules from day one — but invariance is fine for Stage 1 because Beamtalk's class hierarchy is shallow with sealed leaf types (no subclasses of Integer, String, etc.). Variance becomes necessary in Stage 2 (Phase 2e) when protocol types create structural subtyping — `Array(Printable)` accepting `Array(Integer)`. This is explicitly planned, not indefinitely deferred.
- **TypeScript/Java developers** expect angle-bracket `<T>` syntax — but `<` is a binary message in Beamtalk, and parentheses are unambiguous. Gleam validates this choice on BEAM.
- **Pragmatists** would prefer a smaller ADR (generics only) — but union checking and narrowing are needed for generics to be useful in practice (`String | nil` is the killer use case).
- **The variance question is resolved by staging.** Invariance is fine for Stage 1 (sealed leaf classes have no subtyping). Variance is planned for Stage 2 Phase 2e (protocol types create the subtyping relationships that demand it). This is not "add variance someday" — it's "add variance when the feature that needs it ships."

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
- Square brackets are used for block literals: `[:x | x + 1]`
- `Array[Integer]` is visually ambiguous with `Array` followed by a block literal
- Array literals use `#[1, 2, 3]` — `Array[Integer]` could be confused with a hash-prefixed literal
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
- Generic annotations generate precise Dialyzer `-spec` attributes at the BEAM interop boundary (concrete types instead of `any()` for Erlang/Elixir callers) — but see prerequisite below on Dialyzer validation
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
- **Dialyzer spec validation gap:** We generate `-spec` attributes but have no automated verification that they are valid or that Dialyzer can use them to catch bugs. Existing `spec_codegen.rs` unit tests only verify string output, not Dialyzer acceptance. This gap exists today for simple type annotations and will widen with generic specs — a malformed generic spec could go undetected. See prerequisite in Implementation.

### Neutral
- Generated BEAM bytecode is unchanged — type erasure means zero runtime cost
- Existing tests don't need modification
- Dynamic features (`doesNotUnderstand:`, runtime class creation) continue working unchanged
- The `typed` class modifier works orthogonally — `typed` generic classes get thorough checking, non-typed ones get inference only

## Implementation

### Prerequisite: Dialyzer Spec Validation (BT-1565)

Dialyzer specs serve the FFI/interop boundary — they let Erlang/Elixir callers get type-checked when calling Beamtalk actors. For pure Beamtalk, the Beamtalk type checker is the right tool (it understands message sends, class hierarchies, `doesNotUnderstand:`, etc.; Dialyzer just sees `gen_server:call`).

Before expanding spec generation to handle generics, we need confidence that the specs we generate for interop are actually valid. Today:

- `spec_codegen.rs` unit tests verify rendered string output — not that Dialyzer accepts the specs
- `just dialyzer` runs on hand-written runtime `.erl` files only, not on compiled `.bt` output
- CI only runs Dialyzer when runtime `.erl` files change — stdlib specs are never checked
- No cross-language Dialyzer test verifies that an Erlang caller of a Beamtalk actor gets correct type checking

**Required before Phase 1d (generic spec codegen):**
1. Add a CI step that compiles stdlib `.bt` classes to BEAM and runs Dialyzer on the output — verifying that existing `-spec` attributes are valid
2. Add an integration test with an Erlang module calling a typed Beamtalk actor — verifying Dialyzer catches type mismatches at the FFI boundary
3. Include a negative test (intentionally wrong types from Erlang side → Dialyzer warning)

Without this, we'd be generating increasingly complex generic specs (`Result(integer(), any())`) with no verification that Dialyzer can parse or use them at the boundary. This is tracked as [BT-1565](https://linear.app/beamtalk/issue/BT-1565).

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

**Phase 1d: Codegen — Spec and Meta (M)**
- Extend `spec_codegen.rs` to expand generic types with concrete substitutions in Dialyzer specs
- Unresolved type parameters → `any()` in specs
- Emit `type_params` list in `__beamtalk_meta/0` output
- Emit `{type_param, Name, Index}` tagged tuples in `method_info` return_type/param_types entries
- Update `method_return_types` map population in `beamtalk_object_class.erl` to handle tagged tuples
- Update chain-resolution in `beamtalk_repl_ops_dev.erl` to substitute type params when caller context provides concrete types

**Phase 1e: Stdlib Annotations (M)**
- Update `Result.bt`: `Result(T, E)`, fields `:: T` / `:: E`, method return types
- Update collection classes: `Array(E)`, `Dictionary(K, V)`, `Set(E)`
- Update `Block` types if applicable

**Phase 1f: Union Type Checking (M)**
- Add `InferredType::Union` variant to represent union types in the checker
- When encountering `TypeAnnotation::Union`, build `InferredType::Union` with each member resolved
- On message send to a union-typed receiver: check selector exists on ALL members, warn if any member lacks it
- Return type of union sends: union of member return types (simplified to single type if all agree)
- Remove the `_ => return` and `contains('|')` escape hatches for unions
- Resolve `nil` → `UndefinedObject`, `false` → `False`, `true` → `True` in type position
- Handle `TypeAnnotation::FalseOr` (existing AST variant) as `T | False` union
- Resolve `nil` in type position to `UndefinedObject`, `false` to `False`, `true` to `True`

**Phase 1g: Control Flow Narrowing (M)**
- Add narrowing environment to scope: `HashMap<VariableId, InferredType>` refinement layer pushed/popped per block
- Pattern-match `[expr] class = [ClassName] ifTrue: [block]` — push `{expr → ClassName}` into block scope
- Pattern-match `[expr] isKindOf: [ClassName] ifTrue: [block]` — same narrowing
- Early-return narrowing: `[expr] isNil ifTrue: [^...]` — push non-nil refinement for rest of method
- `ifFalse:` gets the complement for nil checks (`isNil ifTrue: [^...] ifFalse: [block]` — `expr` is non-nil in false block)

### Stage 2: Structural Protocols (Size: L)

**Phase 2a: Protocol AST and Parser (M)**
- Add `ProtocolDefinition` to `ast.rs`: name, type params, required method signatures (with param names, types, return types, doc comments), span
- Parse `Protocol define: Name` followed by class-body-style method signatures (no `=>` body)
- Distinguish protocol method signatures from class method definitions by absence of `=>`
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

**Phase 2e: `respondsTo:` Narrowing (S)**
- Extend Stage 1 narrowing patterns: `x respondsTo: #selector ifTrue: [...]` narrows `x` to "has that method" in the true block
- Requires protocol registry to determine what conformance means — `respondsTo: #asString` narrows to Printable conformance if Printable is defined

**Phase 2f: Variance for Protocol-Typed Parameters (M)**
- Invariant generics break when protocol types create subtyping: `Array(Integer)` should be assignable to `Array(Printable)` because Integer conforms to Printable, but invariant checking rejects it
- Add covariance for type parameters in read-only positions (return types, immutable fields)
- Practical rule: sealed Value classes with no mutating methods on the type parameter are covariant by default — `Array(E)` is covariant in `E` because Array is immutable
- Actor state fields that use type params remain invariant (mutation invalidates covariance)
- This only matters for protocol-typed bounds — class-typed bounds between sealed leaf types have no subtyping to worry about

### Implementation Tracking

**Epic:** [BT-1567](https://linear.app/beamtalk/issue/BT-1567)
**Status:** Completed in v0.3.0

| Phase | Issue | Description | Size | Dependencies |
|-------|-------|-------------|------|--------------|
| prereq | [BT-1565](https://linear.app/beamtalk/issue/BT-1565) | Dialyzer spec validation CI | M | None |
| 1a | [BT-1568](https://linear.app/beamtalk/issue/BT-1568) | Parser and AST for generic types | M | None |
| 1 | [BT-1569](https://linear.app/beamtalk/issue/BT-1569) | Extend InferredType with type_args, Union, provenance | M | None |
| 1b | [BT-1570](https://linear.app/beamtalk/issue/BT-1570) | Type checker generic substitution | L | BT-1568, BT-1569 |
| 1c | [BT-1571](https://linear.app/beamtalk/issue/BT-1571) | Constructor type inference | M | BT-1570 |
| 1f | [BT-1572](https://linear.app/beamtalk/issue/BT-1572) | Union type checking | M | BT-1569 |
| 1g | [BT-1573](https://linear.app/beamtalk/issue/BT-1573) | Control flow narrowing | M | BT-1569, BT-1572 |
| 1d | [BT-1574](https://linear.app/beamtalk/issue/BT-1574) | Codegen — Dialyzer specs for generics | M | BT-1568, BT-1565 |
| 1d | [BT-1575](https://linear.app/beamtalk/issue/BT-1575) | Codegen — runtime meta for generics | M | BT-1568 |
| 1e | [BT-1576](https://linear.app/beamtalk/issue/BT-1576) | Stdlib generic annotations | M | BT-1570, BT-1574, BT-1575 |
| — | [BT-1577](https://linear.app/beamtalk/issue/BT-1577) | Generic inheritance — superclass type application | M | BT-1570 |
| 2a | [BT-1578](https://linear.app/beamtalk/issue/BT-1578) | Protocol AST and parser | M | BT-1568 |
| 2b | [BT-1579](https://linear.app/beamtalk/issue/BT-1579) | Protocol registry and conformance | L | BT-1570, BT-1578 |
| 2c | [BT-1580](https://linear.app/beamtalk/issue/BT-1580) | Runtime protocol queries | M | BT-1579 |
| 2d | [BT-1581](https://linear.app/beamtalk/issue/BT-1581) | Type parameter bounds | S | BT-1579 |
| 2e | [BT-1582](https://linear.app/beamtalk/issue/BT-1582) | respondsTo: narrowing | S | BT-1573, BT-1579 |
| 2f | [BT-1583](https://linear.app/beamtalk/issue/BT-1583) | Variance for protocol-typed params | M | BT-1579, BT-1581 |
| — | [BT-1584](https://linear.app/beamtalk/issue/BT-1584) | Documentation — language spec update | M | BT-1576, BT-1579 |

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
