# Type System Design

This document describes the type system design for Beamtalk, including implementation strategy and technical decisions.

**Status**: Implementation Phase (ADR 0068 accepted)
**Related Docs**: [semantic-analysis.md](semantic-analysis.md), [beamtalk-language-features.md](../beamtalk-language-features.md), [ADR 0068](../ADR/0068-parametric-types-and-protocols.md)

---

## Table of Contents

- [Design Goals](#design-goals)
- [Type System Approach](#type-system-approach)
- [Architecture: InferredType (ADR 0068)](#architecture-inferredtype-adr-0068)
- [Parametric Types (Generics)](#parametric-types-generics)
- [Structural Protocols](#structural-protocols)
- [Union Types and Narrowing](#union-types-and-narrowing)
- [Background: Smalltalk vs Modern Typing](#background-smalltalk-vs-modern-typing)
- [Erlang Type Systems](#erlang-type-systems)
- [Hindley-Milner Type Inference](#hindley-milner-type-inference)
- [Implementation Strategy](#implementation-strategy)
- [Challenges for Beamtalk](#challenges-for-beamtalk)
- [References](#references)

---

## Design Goals

Beamtalk aims for **gradual typing** with strong tooling support:

1. **Optional type annotations** - types improve tooling but aren't required
2. **Type inference** - minimize annotations needed in practice
3. **IDE-friendly** - LSP can provide completions, hover info, refactoring
4. **BEAM interop** - handle Erlang's dynamic types and union types
5. **Incremental adoption** - add types gradually to existing code

**Philosophy**: Types serve the developer, not the compiler. They should make code easier to write and maintain, not harder.

---

## Type System Approach

### Gradual/Optional Static Typing

Beamtalk follows TypeScript/Python's approach - types are optional annotations that improve tooling:

```beamtalk
// Optional - types inferred
increment => self.value := self.value + 1

// Explicit when you want tooling/safety
getValue -> Integer => self.value

// Typed parameters for public APIs
transferFrom: source: Account amount: Money -> Boolean =>
  source withdraw: amount
  self deposit: amount
  true
```

### Features Planned

From [beamtalk-language-features.md](../beamtalk-language-features.md) and [ADR 0068](../ADR/0068-parametric-types-and-protocols.md):

- **Basic types**: `Integer`, `Float`, `String`, `Boolean`
- **Union types**: `Integer | Error` (for Erlang interop)
- **Singleton types**: `#north | #south | #east | #west` (atoms)
- **False-or types**: `Integer | False` (option/maybe pattern)
- **Generic types**: `Result(T, E)`, `Array(E)`, `Dictionary(K, V)` (parenthesis syntax)
- **Structural protocols**: `Printable`, `Comparable`, `Collection(E)` (duck-typing made explicit)
- **Control flow narrowing**: `x isNil ifTrue: [^nil]; x doSomething` (nil-safe)

---

## Architecture: InferredType (ADR 0068)

ADR 0068 introduces a richer `InferredType` representation to support generics, unions, and provenance tracking:

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

### Provenance

Provenance tracks whether a type was explicitly declared, inferred by the compiler, or derived from a generic substitution. This improves error messages:

- **Declared** — user owns the assertion: `x declared as Result(Integer, Error) on line 1`
- **Inferred** — compiler guessed, user can override: `x inferred as Result(Integer, Dynamic) from constructor; add annotation if wrong`
- **Substituted** — derived from generic resolution: helps trace through chains

### Type Arguments on InferredType::Known

The `type_args` field on `Known` enables generic type flow through method chains:

```beamtalk
r :: Result(Integer, Error) := computeSomething
r map: [:v | v asString]   // Result(String, Error) — type args carried through
```

When resolving `Self` for a receiver with known type args, the type args propagate: `Array(Integer)` + return type `Self` resolves to `Array(Integer)`.

---

## Parametric Types (Generics)

### Overview

Beamtalk adopts declaration-site parametric types with compile-time substitution. Classes declare type parameters after the class name using parenthesis syntax: `Result(T, E)`. This keeps `<` reserved as a binary message (comparison operator), following Gleam's approach on BEAM.

### Class-Level Type Parameters

```rust
struct ClassDefinition {
    // ... existing fields ...
    type_params: Vec<Identifier>,  // e.g., [T, E] for Result(T, E)
}

struct ClassInfo {
    // ... existing fields ...
    type_params: Vec<EcoString>,
    superclass_type_args: Vec<TypeParamMapping>,
}
```

### Substitution Algorithm

When the type checker encounters `Result(Integer, IOError)`:

1. Build substitution map: `{T -> Integer, E -> IOError}`
2. Apply substitution to method return types: `unwrap -> T` becomes `unwrap -> Integer`
3. Apply substitution to parameter types during validation

### Method-Local Type Parameters

Methods can introduce type variables not declared at the class level. In `map: block :: Block(T, R) -> Result(R, E)`, `R` is a method-local type param inferred from call-site arguments:

```beamtalk
r :: Result(Integer, Error) := computeSomething
r map: [:v | v asString]   // R = String inferred from block return type
// Result: Result(String, Error)
```

### Block Special-Casing

`Block(...)` is interpreted specially — the last type parameter is always the return type:

- `Block(R)` — zero-arg block returning `R`
- `Block(A, R)` — one-arg block
- `Block(A, B, R)` — two-arg block

### Constructor Type Inference

Class methods that reference instance type params trigger inference:

```beamtalk
Result ok: 42              // T = Integer from argument -> Result(Integer, Dynamic)
Result error: #not_found   // E = Symbol from argument -> Result(Dynamic, Symbol)
```

### Runtime Type Representation

Parameterized methods store type parameter references in `__beamtalk_meta/0`:

```erlang
#{method_info => #{
    'unwrap' => #{arity => 0, return_type => {type_param, 'T', 0}, ...},
    'error'  => #{arity => 0, return_type => {type_param, 'E', 1}, ...}
  },
  type_params => ['T', 'E']
}
```

This is introspection data for tooling (REPL chain completion, `:help`), not reified generics.

---

## Structural Protocols

### Overview

Protocols define named message sets. Conformance is structural and automatic — no `implements:` declaration needed. This formalizes Smalltalk's duck-typing philosophy.

### Protocol Registry

The compiler maintains a protocol registry mapping protocol names to required message sets. At type-checking time, when a type annotation resolves to a protocol name, the checker performs structural conformance checking.

### Conformance Tiers

1. **Compile-time ClassHierarchy** — statically defined methods only
2. **REPL workspace** — includes live method additions and class redefinitions
3. **`doesNotUnderstand:` bypass** — DNU-overriding classes conform to every protocol

### Protocol Metadata

Protocol definitions compile into module attributes for runtime queries (`conformsTo:`, `protocols`, `requiredMethods`, `conformingClasses`).

---

## Union Types and Narrowing

### Union Type Checking

A message send on a union-typed receiver warns unless all union members respond to the selector. Return type is the union of member return types (simplified if all agree).

### Control Flow Narrowing

The type checker pattern-matches on known AST shapes to narrow types:

- `x class = Foo ifTrue: [...]` — narrows `x` to `Foo` in true block
- `x isKindOf: Foo ifTrue: [...]` — same
- `x isNil ifTrue: [^...]` — narrows `x` to non-nil for rest of method
- `x respondsTo: #selector ifTrue: [...]` — narrows to protocol conformance (Stage 2)

This is not a general narrowing framework but a fixed set of recognized idioms, each added as a new case in the AST pattern matcher.

---

## Background: Smalltalk vs Modern Typing

### How Smalltalk Handled Types

Smalltalk had **no static type detection** - types were discovered entirely at runtime:

**1. Duck Typing via Message Dispatch**
```smalltalk
obj doSomething
"Runtime looks up 'doSomething' in obj's class method dictionary"
"If found → execute"
"If not found → invoke doesNotUnderstand: #doSomething"
```

**2. Type is Class Membership**
```smalltalk
obj class            "Returns the class object"
obj isKindOf: Integer    "Explicit type check"
obj isMemberOf: String   "Exact class check"
```

But idiomatic Smalltalk **avoided** these checks - just send messages and let it fail.

**3. Runtime Errors, Not Compile Errors**
- Sending invalid message → `MessageNotUnderstood` exception at runtime
- Variables held references to any object; no compile-time constraints
- The image environment caught errors immediately during development

### Smalltalk Philosophy

**"Don't ask what type it is, ask what it can do."**

- No interfaces or protocols declared - just send messages
- If object responds correctly, it's the "right type"
- `doesNotUnderstand:` allowed dynamic message handling (metaprogramming)

### Why This Won't Work for Modern Tooling

Without static types, IDEs can't provide:
- **Autocomplete** - don't know what methods exist
- **Go to definition** - don't know where method is defined
- **Refactoring** - can't safely rename methods
- **Type errors** - catch bugs before running code

```beamtalk
// Without types - IDE knows nothing
doSomething: obj =>
  obj.???  // What methods exist? No idea!

// With types - IDE has full completion
doSomething: (obj: Counter) =>
  obj.  // Shows: increment, decrement, value, reset...
```

---

## Erlang Type Systems

The BEAM ecosystem has two main approaches to typing:

### 1. Dialyzer - "Success Typing"

**Philosophy**: Optimistic analysis - only warn on PROVABLE errors.

| Traditional Static Typing | Success Typing (Dialyzer) |
|---------------------------|---------------------------|
| "This MIGHT fail → reject" | "This WILL DEFINITELY fail → warn" |
| Conservative | Optimistic |
| False positives possible | Zero false positives |
| Blocks code from compiling | Post-compilation analysis |

**How it works:**

```erlang
-spec add(integer(), integer()) -> integer().
add(A, B) -> A + B.

add("hello", "world")  % Dialyzer: DEFINITE ERROR
```

**Inference from usage:**
```erlang
% No spec - Dialyzer infers from code
get_value(#{value := V}) -> V.
% Infers: "takes map with 'value' key, returns any type"
```

**Only warns on PROVABLE errors:**
```erlang
% DEFINITELY wrong - always crashes
bad() -> 
    X = "hello",
    X + 1.  % Dialyzer warns: strings can't be added

% MIGHT be wrong - no warning
maybe_bad(X) -> 
    X + 1.  % Could be string or number - Dialyzer silent
```

**Advantages:**
- ✅ No false positives - warnings are real
- ✅ Works on existing code - no rewrite needed
- ✅ Finds dead code and unreachable branches

**Limitations:**
- ⚠️ False negatives - misses bugs it can't prove
- ⚠️ Slow - whole-program analysis
- ⚠️ Cryptic errors - warnings hard to understand
- ⚠️ **Post-compilation** - not great for IDE tooling

### 2. Gradualizer - True Gradual Typing

Newer tool that enforces type specs more strictly than Dialyzer:

```erlang
-spec add(integer(), integer()) -> integer().
add(A, B) -> A + B.
% Gradualizer enforces specs at compile time, like TypeScript
```

More like TypeScript's approach - optional but enforced when present.

### Why Not Use Dialyzer for Beamtalk?

**Problem**: Dialyzer is a **post-compilation** tool:
1. Write Beamtalk code
2. Compile to Core Erlang → BEAM
3. Run Dialyzer on BEAM bytecode
4. Get type errors (maybe)

**Too late for IDE tooling** - we need types during editing, not after compilation.

---

## Hindley-Milner Type Inference

**Hindley-Milner (HM)** is the classical type inference algorithm used in ML, Haskell, and Gleam.

### Core Algorithm

**3 main steps:**

**1. Generate fresh type variables**
```rust
let x = 5        // x: T0 (unknown)
let y = x + 1    // y: T1, constraint: T0 = Int, T1 = Int
```

**2. Collect constraints**
```rust
fn add(a, b) { a + b }
// a: T0, b: T1, result: T2
// Constraint: T0 = Int, T1 = Int, T2 = Int (from + operator)
```

**3. Unify (solve constraints)**
```rust
// T0 = Int, T1 = Int → solve to concrete types
// Check for conflicts: T0 = Int AND T0 = String → ERROR
```

### Basic Rust Types

```rust
enum Type {
    Int,
    Bool,
    String,
    Var(u32),                      // Type variable T0, T1, etc.
    Func(Box<Type>, Box<Type>),    // a -> b
}
```

### Complexity Breakdown

#### Easy Parts (1-2 days)
✅ **Basic inference** - literals, simple expressions  
✅ **Unification algorithm** - well-documented  
✅ **Type variables** - just generate unique IDs

#### Medium Parts (1-2 weeks)
⚠️ **Let-polymorphism** - generalizing types  
⚠️ **Type environment** - tracking variable scopes  
⚠️ **Error messages** - making failures understandable

Example of let-polymorphism:
```beamtalk
// Polymorphic identity - works for any type
id := [:x | x]
id value: 5        // id: Integer -> Integer
id value: 'hello'  // id: String -> String (SAME block!)
```

#### Hard Parts (1-2 months)
🔴 **Subtyping** - inheritance hierarchies  
🔴 **Mutable references** - tracking borrowing/ownership  
🔴 **Row polymorphism** - record types with unknown fields  
🔴 **Type classes/traits** - ad-hoc polymorphism

### Implementation Complexity

**Realistic timeline:**
- **MVP (basic inference)**: 1-2 weeks
- **Practical (good errors, polymorphism)**: 1-2 months
- **Production (all features, fast)**: 3-6 months

---

## Implementation Strategy

### Phase 1: Basic Type Checking (No Inference)

**Goal**: Check explicit type annotations, no inference yet.

```beamtalk
// Error: parameter types don't match annotation
add: (x: Integer) (y: String) -> Integer =>
  x + y  // ERROR: y is String, not Integer
```

**Components needed:**
1. Parse type annotations → AST type nodes
2. Build symbol table with declared types
3. Check expressions against declared types
4. Report type errors with spans

**Estimated effort**: 2-3 weeks

### Phase 2: Basic Hindley-Milner Inference

**Goal**: Infer types for local variables and simple expressions.

```beamtalk
// Infer types from usage
count := 0           // count: Integer
name := 'Alice'      // name: String
result := count + 1  // result: Integer
```

**Components needed:**
1. Type variables and substitution
2. Constraint generation from AST
3. Unification algorithm
4. Error reporting with inferred types

**Reference implementations:**
- [tcr/rust-hindley-milner](https://github.com/tcr/rust-hindley-milner) - clean Rust HM
- [Gleam type checker](https://github.com/gleam-lang/gleam/tree/main/compiler-core/src/type_) - production Rust-to-BEAM

**Estimated effort**: 3-4 weeks

### Phase 3: Parametric Types and Generics (ADR 0068 Stage 1)

**Goal**: Declaration-site parametric types with compile-time substitution.

```beamtalk
// Generic class declaration — parenthesis syntax
sealed Value subclass: Result(T, E)
  field: okValue :: T = nil
  field: errReason :: E = nil
  unwrap -> T => ...

// Usage-site type application
result :: Result(String, IOError) := File read: "config.json"
result unwrap    // -> String (type checker substitutes T -> String)

// Constructor inference
r := Result ok: 42    // Inferred: Result(Integer, Dynamic)
```

**Components needed (see [ADR 0068 implementation phases](../ADR/0068-parametric-types-and-protocols.md#implementation)):**

1. **Parser and AST** (Phase 1a): `type_params` on `ClassDefinition`, parse `Name(Type, ...)` in annotations
2. **InferredType extension** (Phase 1): `type_args`, `Union` variant, `TypeProvenance`
3. **Type checker substitution** (Phase 1b): positional substitution, replace escape hatches
4. **Constructor inference** (Phase 1c): infer type params from constructor arguments
5. **Union type checking** (Phase 1f): check all union members respond to message
6. **Control flow narrowing** (Phase 1g): `isNil`, `class =`, `isKindOf:` patterns
7. **Codegen** (Phases 1d, 1e): Dialyzer spec expansion, runtime meta with `{type_param, Name, Index}`

**Design choices:**
- Parenthesis syntax `Result(T, E)` not `Result<T, E>` — `<` stays as binary message
- Invariant type parameters (variance added in Stage 2 with protocols)
- Block type params special-cased (last param = return type)
- Method-local type params inferred from call-site arguments

### Phase 4: Structural Protocols (ADR 0068 Stage 2)

**Goal**: Named message sets with automatic structural conformance.

```beamtalk
Protocol define: Printable
  asString -> String

Protocol define: Collection(E)
  size -> Integer
  do: block :: Block(E, Object)
  collect: block :: Block(E, Object) -> Self

// Protocol as type annotation — structural check
display: thing :: Printable =>
  Transcript show: thing asString

// Protocol bounds on type parameters
Actor subclass: Logger(T :: Printable)
  log: item :: T => Transcript show: item asString
```

**Components needed:**

1. **Protocol AST and parser** (Phase 2a): `ProtocolDefinition` node, method signatures without `=>`
2. **Protocol registry and conformance** (Phase 2b): structural checking against required message sets
3. **Runtime queries** (Phase 2c): `conformsTo:`, `protocols`, `requiredMethods`
4. **Type parameter bounds** (Phase 2d): `T :: Printable` in type param declarations
5. **`respondsTo:` narrowing** (Phase 2e): protocol-aware narrowing
6. **Variance** (Phase 2f): covariance for immutable type params, invariance for mutable

---

## Challenges for Beamtalk

### 1. Actor Message Passing

What's the type of a message sent to an actor?

```beamtalk
// Sending message to another actor
counter increment
// What type is 'counter'? What type does 'increment' return?
```

**Options:**
- **Option A**: Messages typed by actor's interface
  ```beamtalk
  counter: Counter  // Counter class defines message types
  ```
- **Option B**: Messages are dynamically typed (like Erlang)
  ```beamtalk
  counter: Actor  // All actors have type Actor, messages unchecked
  ```
- **Option C**: Message protocols (like Elixir protocols)
  ```beamtalk
  counter: Incrementable  // Protocol defines message contract
  ```

**Recommendation**: Start with Option A (typed by class), consider protocols later.

### 2. Dynamic Features

Beamtalk supports `doesNotUnderstand:` for metaprogramming:

```beamtalk
doesNotUnderstand: message =>
  // Dynamically handle any message not defined in class
  self forwardTo: proxy message: message
```

**Problem**: Static type checker can't know what messages are valid.

**Solution**: Mark dynamic objects explicitly:
```beamtalk
proxy: Dynamic  // Opts out of static checking
```

### 3. BEAM Interop

Erlang is dynamically typed - how do we type Erlang calls?

```erlang
% Erlang returns {ok, Value} | {error, Reason}
{ok, Value} = erlang:some_function(Arg)
```

**Solution**: Use union types:
```beamtalk
result: {ok: Value} | {error: Reason} := 
  Erlang call: 'some_function' args: [arg]

// Pattern match on result
result match: [
  {ok: value} -> value,
  {error: reason} -> self handleError: reason
]
```

### 4. Hot Code Reloading

Type checking with live updates:

```beamtalk
// Change method signature while system is running
getValue -> Integer => self.value
// Change to:
getValue -> String => self.value toString
```

**Problem**: Type changes break callers at runtime.

**Solutions:**
- Gradual migration: old and new versions coexist
- Runtime type checks on messages
- Recompile dependent modules automatically

---

## References

### Type Theory
- **"Types and Programming Languages" (TAPL)** by Benjamin Pierce - comprehensive type theory textbook
- **"Algorithm W Step by Step"** - classic HM inference paper
- [Type Inference - veera.app](https://veera.app/type_inference.html) - approachable HM tutorial

### Rust Implementations
- [Ricky Han's Rust HM Tutorial](https://rickyhan.com/jekyll/update/2018/05/26/hindley-milner-tutorial-rust.html) - complete working example
- [tcr/rust-hindley-milner](https://github.com/tcr/rust-hindley-milner) - clean Rust implementation
- [nwoeanhinnogaehr/algorithmw-rust](https://github.com/nwoeanhinnogaehr/algorithmw-rust) - Algorithm W in Rust

### Reference Compilers
- [Gleam Type Checker](https://github.com/gleam-lang/gleam/tree/main/compiler-core/src/type_) - production HM for BEAM in Rust
- [TypeScript Compiler](https://github.com/microsoft/TypeScript) - gradual typing reference
- [Rust Compiler Type Inference](https://rustc-dev-guide.rust-lang.org/type-inference.html) - how rustc extends HM

### Erlang Type Systems
- [Dialyzer User Guide](https://www.erlang.org/doc/apps/dialyzer/dialyzer.html) - success typing documentation
- [Learn You Some Erlang - Dialyzer](https://learnyousomeerlang.com/dialyzer) - accessible explanation
- [Gradualizer](https://github.com/josefs/Gradualizer) - gradual typing for Erlang

### Related Beamtalk Docs
- [beamtalk-language-features.md](../beamtalk-language-features.md) - language design with type syntax
- [ADR 0068: Parametric Types and Protocols](../ADR/0068-parametric-types-and-protocols.md) - definitive design for generics, protocols, unions, narrowing
- [ADR 0025: Gradual Typing and Protocols](../ADR/0025-gradual-typing-and-protocols.md) - foundation (Phases 1-2 complete)
- [ADR 0053: Double-Colon Type Annotation Syntax](../ADR/0053-double-colon-type-annotation-syntax.md) - `::` delimiter
- [ADR 0060: Result Type](../ADR/0060-result-type-hybrid-error-handling.md) - motivating use case for generics
- [semantic-analysis.md](semantic-analysis.md) - compiler phase before type checking
- [design-tooling-ide.md](design-tooling-ide.md) - LSP integration needs type information
