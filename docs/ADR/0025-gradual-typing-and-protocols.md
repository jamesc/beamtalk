# ADR 0025: Gradual Typing and Protocols

## Status
Proposed (2026-02-15)

## Context

Beamtalk is a dynamically-typed, Smalltalk-inspired language targeting the BEAM VM. Today, all type errors are caught at runtime — sending an unknown message crashes with `doesNotUnderstand:`, and passing the wrong type to a primitive crashes with a BEAM exception.

However, the compiler already knows substantial type information at compile time:

- **ClassHierarchy** tracks every class, its methods, superclass chain, and sealed status
- **Built-in types** (Integer, String, Float, Boolean, List, etc.) have fully known method tables
- **State declarations** specify field names and default values
- **AST TypeAnnotation** enum already supports simple, union, generic, and singleton types
- **Parser** already parses state type annotations (`state: value: Integer = 0`)
- **MethodDefinition** has a `return_type: Option<TypeAnnotation>` field (currently always `None`)
- **TypeChecker** stub exists in semantic analysis with the right DDD positioning

Despite all this infrastructure, the compiler discards type information — it doesn't check message sends, doesn't infer types from assignments, and doesn't generate Erlang `-spec` attributes. Every typo and type mismatch is a runtime surprise.

Meanwhile, the IDE tooling vision (ADR 0024) depends on type information for quality completions, hover, and diagnostics. Without types, the language service can only offer syntactic suggestions.

### The Core Tension

Beamtalk must serve two masters:

1. **Interactive-first development** — REPL exploration, live coding, hot reload. Types must never block experimentation.
2. **Production safety** — Catch bugs before deployment. Types should catch what they can without impeding development velocity.

The question is not *whether* to add types, but *how* — in a way that preserves Smalltalk's dynamism while providing TypeScript-level tooling quality.

## Decision

Beamtalk adopts **gradual typing with structural protocols**, implemented in four phases:

### Design Principles

1. **Types are always optional** — Untyped code compiles and runs exactly as today. No existing code breaks.
2. **Warnings, not errors** — Type mismatches produce compiler warnings, not errors. Code always compiles. (A strict mode flag can promote warnings to errors for CI.)
3. **Compile-time only** — Type checking happens entirely at compile time. No runtime cost, no type tags, no overhead.
4. **Structural, not nominal** — Type compatibility is determined by what messages an object responds to (its "shape"), not its class hierarchy. A `Duck` and a `Person` that both have `walk` are both valid where "something that walks" is expected.
5. **Infer first, annotate for precision** — The compiler infers what it can from class definitions and assignments. Annotations add precision where inference falls short.
6. **BEAM integration** — Type annotations generate Dialyzer `-spec` attributes in Core Erlang, giving Erlang-level type checking for free at the BEAM boundary.

### Phase 1: Type Inference from Known Classes (Zero New Syntax)

The compiler uses existing class definitions to infer types and check message sends.

```beamtalk
Actor subclass: Counter
  state: value = 0
  increment => self.value := self.value + 1
  getValue => self.value

c := Counter spawn
c increment              // ✅ Counter has 'increment'
c getValue               // ✅ Counter has 'getValue'
c decrement              // ⚠️ Warning: Counter does not respond to 'decrement'
                         //    Hint: Did you mean 'increment'?

3 + "hello"              // ⚠️ Warning: Integer '+' expects Number, got String
```

**Inference rules:**
- `x := Counter spawn` → `x` has type `Counter`
- `x := 42` → `x` has type `Integer`
- `x := 'hello'` → `x` has type `String`
- `x := true` → `x` has type `Boolean`
- `x := someUnknownThing` → `x` has type `Dynamic` (no checking)
- Method calls on known types check against ClassHierarchy method tables
- `doesNotUnderstand:` remains valid — if a class defines it, any message is accepted

**What the compiler already knows:**
- Every class and its methods (via `ClassHierarchy`)
- Superclass chains and inheritance
- Sealed classes (no unknown subclasses)
- Built-in primitive types and their full method tables

### Phase 2: Optional Type Annotations

Developers can annotate state fields, method parameters, and return types for extra precision.

```beamtalk
Actor subclass: BankAccount
  state: balance: Integer = 0           // State type annotation
  state: owner: String                  // Required — no default

  deposit: amount: Integer =>           // Parameter type annotation
    self.balance := self.balance + amount

  getBalance -> Integer =>              // Return type annotation
    self.balance

  transfer: amount: Integer to: target: BankAccount =>
    self withdraw: amount
    target deposit: amount
```

**Parameter type syntax:** Type follows the parameter name with `:` separator, matching state declaration syntax.

```beamtalk
// Keyword message with typed parameters
deposit: amount: Integer =>  ...

// Multiple keywords, each typed
transfer: amount: Integer to: target: BankAccount => ...

// Unary — no parameters, just return type  
getBalance -> Integer => ...

// Binary — type on operand
+ other: Number -> Number => ...
```

**Return type syntax:** `-> Type` before the `=>` body separator.

**Codegen:** Annotations generate Erlang `-spec` attributes in Core Erlang module attributes:

```erlang
% Generated Core Erlang attributes
attributes ['spec' = [{'deposit', {type, fun, [{type, product, [{type, integer, []}]}, 
                                                {type, integer, []}]}}],
            'behaviour' = ['gen_server']]
```

This enables Dialyzer to perform additional checking at the BEAM level — two layers of type safety.

### Phase 3: Protocols (Structural Typing)

Protocols define named sets of messages that types must support.

```beamtalk
// Protocol definition — a named "shape"
Protocol define: Printable
  requiring: [asString]

Protocol define: Comparable
  requiring: [<, >, <=, >=]

Protocol define: Collection
  requiring: [size, do:, collect:, select:]
```

**Conformance is structural and automatic:**

```beamtalk
// Counter has 'asString' (inherited from Object) → conforms to Printable
// No "implements" declaration needed

// Use protocols as type constraints
printAll: items: <Printable> =>
  items do: [:each | Transcript show: each asString]

// Error when shape doesn't match
printAll: #(1, 2, 3)          // ✅ Integer conforms to Printable
printAll: someOpaqueValue      // ⚠️ Warning: cannot verify Printable conformance
```

**Protocol query in REPL:**

```beamtalk
> Counter conformsTo: Printable
=> true
> Counter protocols
=> #(Printable, Comparable)
> Printable requiredMethods
=> #(asString)
```

### Phase 4: Advanced Type Features (Future)

```beamtalk
// Union types
state: result: Integer | Error

// Singleton/enum types
state: direction: #north | #south | #east | #west

// False-or pattern (Option/Maybe)
state: cache: Integer | False = false

// Generic types (parametric polymorphism)
Protocol define: Stack<T>
  requiring: [push: <T>, pop -> <T>, isEmpty -> Boolean]

// Type narrowing in control flow
x class = Integer ifTrue: [
  x + 1       // Compiler knows x is Integer here
]
```

## Prior Art

### Strongtalk (Primary Influence)
**Approach:** Optional, structural, protocol-based typing for Smalltalk. Designed by Gilad Bracha (who later designed Newspeak and co-designed the JVM spec).

**Key decisions we adopt:**
- Types are optional and compile-time only (no runtime cost)
- Structural typing via protocols ("message sets"), not nominal class hierarchies
- Separate type from class — a class is an implementation, a protocol is an interface
- Subtyping ≠ subclassing

**Key decisions we adapt:**
- Strongtalk uses `<Type>` annotations on parameters — we use `param: Type` to match our state declaration syntax
- Strongtalk has explicit `implements:` — we use automatic structural conformance (more TypeScript-like)

### TypeScript (Structural Inference Model)
**Approach:** Gradual, structural typing layered on JavaScript with aggressive inference.

**What we adopt:**
- Structural compatibility ("duck typing" at compile time)
- Type inference from assignments and return values
- `any`/`Dynamic` escape hatch for untyped code
- Warnings as default, strict mode opt-in

**What we adapt:**
- TypeScript has interface declarations — we use protocols (message-oriented, not property-oriented)
- TypeScript infers from control flow — we defer this to Phase 4

### Gleam (Full Static on BEAM)
**Approach:** Mandatory Hindley-Milner typing with no dynamic escape.

**What we reject:**
- Mandatory types — breaks REPL exploration and live coding
- No `doesNotUnderstand:` — eliminates proxy/delegation patterns
- All-or-nothing — incompatible with gradual adoption

**What we learn:**
- Type specs CAN be encoded in Core Erlang as module attributes
- BEAM hot code loading works fine with typed code
- Type erasure at runtime means zero overhead (same approach we use)

### Dylan (Gradual + Dispatch Optimization)
**Approach:** Optional types with dispatch optimization when types are known.

**What we adopt:**
- Optional annotations for performance hints (sealed + types → bypass gen_server)
- Unification-based inference for parameter types
- "Dynamic by default, static where annotated" philosophy

## User Impact

### Newcomer (from Python/JS/Ruby)
- **Phase 1:** "The editor catches my typos!" — immediate value, zero learning curve
- **Phase 2:** "I can add types like TypeScript" — familiar concept
- **Phase 3:** "Protocols are like TypeScript interfaces" — maps to known concepts
- **Risk:** None — everything is optional, nothing breaks

### Smalltalk Developer
- **Phase 1:** "Finally, the compiler uses what it already knows" — pure upside
- **Phase 2:** "Type annotations are optional — I can still explore freely"
- **Phase 3:** "Protocols formalize what we already do informally with message protocols"
- **Risk:** "Don't let types become mandatory" — our design prevents this by principle

### Erlang/Elixir Developer
- **Phase 2:** "It generates Dialyzer specs — I get BEAM-level checking!" — huge win
- **Phase 3:** "Protocols are like Elixir protocols" — familiar concept
- **Risk:** None — Dialyzer integration is the expected path on BEAM

### Production Operator
- **All phases:** Zero runtime cost, same BEAM bytecode, same observability
- **Phase 2:** Dialyzer specs enable standard BEAM type checking in CI
- **Risk:** None — type checking is compile-time only

### Tooling Developer (LSP)
- **Phase 1:** Type inference enables precise completions and hover
- **Phase 2:** Return types enable signature help and go-to-type
- **Phase 3:** Protocol types enable interface-level navigation
- **This is the primary driver** — types exist to serve tooling

## Steelman Analysis

### For Full Static (Option C — Rejected)
- 🧑‍💻 **Newcomer**: "I want the compiler to catch everything — TypeScript strict mode is great"
- 🎩 **Smalltalk purist**: "I have nothing good to say about mandatory types"
- ⚙️ **BEAM veteran**: "Gleam proves full static works on BEAM and catches more bugs"
- 🏭 **Operator**: "Mandatory types mean fewer production crashes — I'd trade REPL convenience"
- 🎨 **Language designer**: "Sound type systems are more elegant — no 'Dynamic' escape hatch"

### For Pure Inference Only (No Annotations — Rejected)
- 🧑‍💻 **Newcomer**: "Zero syntax to learn — the compiler just figures it out"
- 🎩 **Smalltalk purist**: "Smalltalk never needed annotations and neither should we"
- ⚙️ **BEAM veteran**: "But without annotations, no Dialyzer specs — I lose BEAM tooling"
- 🎨 **Language designer**: "Inference alone can't disambiguate — `x := self getValue` returns what?"

### Tension Points
- **Newcomers and operators** would prefer stricter checking — but Smalltalk purists insist on optionality
- **BEAM veterans** want `-spec` generation — which requires annotations (Phase 2), not just inference
- **Resolution:** Gradual approach satisfies all: inference for exploration, annotations for production, specs for BEAM

## Alternatives Considered

### Alternative A: Mandatory Static Typing (Gleam-style)
Full Hindley-Milner type system with no dynamic escape.

```beamtalk
// Every binding must be typed
Actor subclass: Counter
  state: value: Integer = 0
  increment -> Integer => self.value := self.value + 1

c := Counter spawn      // Error if Counter.spawn return type unknown
c increment              // Checked at compile time
c foo                    // Compile ERROR (not warning)
```

**Rejected because:**
- Breaks `doesNotUnderstand:` — eliminates proxy/delegation patterns fundamental to Smalltalk
- Breaks live REPL exploration — every experiment requires type annotations
- Breaks dynamic class creation (`create_subclass/3` at runtime)
- Breaks Tonel-style method addition (`Counter >> newMethod => ...`)
- Incompatible with gradual adoption — all-or-nothing forces full migration

### Alternative B: Nominal Typing (Java-style)
Type compatibility based on declared class hierarchy, not message sets.

```beamtalk
// Must explicitly declare interface conformance
Counter implements: Incrementable   // Explicit declaration required
```

**Rejected because:**
- Requires "implements" boilerplate — friction for dynamic language users
- Doesn't match Smalltalk's message-passing model (objects respond to messages, not implement interfaces)
- Forces planning ahead — you must declare interfaces before using them
- Structural typing (our choice) is strictly more flexible

## Consequences

### Positive
- Compiler catches typos and type mismatches at compile time without requiring any code changes
- IDE tooling (completions, hover, diagnostics) dramatically improves with type information
- Dialyzer `-spec` generation brings BEAM-standard type checking for free
- Protocols provide a clean abstraction for polymorphic code without inheritance coupling
- All existing code continues to work unchanged — purely additive
- `sealed` + type info enables future optimizations (bypass gen_server for known dispatch)
- Error messages become actionable: "Counter doesn't respond to 'foo'" instead of runtime crash

### Negative
- Type inference has limits — some code can't be typed without annotations (typed as `Dynamic`)
- Implementing a type checker is significant engineering effort (especially Phase 3-4)
- Type error messages must be carefully designed to not overwhelm users
- Protocol conformance checking on large hierarchies has performance implications for compilation
- Two-layer checking (Beamtalk + Dialyzer) could produce duplicate or contradictory warnings

### Neutral
- Generated BEAM bytecode is unchanged — types are erased at compile time
- Runtime performance is unaffected (no type tags, no runtime checks)
- Existing tests don't need modification
- Dynamic features (`doesNotUnderstand:`, runtime class creation) continue working

## Implementation

### Phase 1: Type Inference (M-L effort)

**Components:**
- `semantic_analysis/type_checker.rs` — Fill in the stub with inference rules
- `semantic_analysis/class_hierarchy/mod.rs` — Extend `MethodInfo` with return type tracking
- `source_analysis/diagnostic.rs` — Add type-related warning diagnostics
- `queries/completion_provider.rs` — Use inferred types for better completions
- `queries/hover_provider.rs` — Show inferred types on hover

**Inference approach:**
1. Walk AST top-down through assignments
2. Track variable types in scope (extend existing `Scope`)
3. On message send: look up receiver type in ClassHierarchy, check method exists
4. On binary op: check operand types against known operator signatures
5. Emit warnings for mismatches, not errors

### Phase 2: Optional Annotations (M effort)

**Components:**
- `source_analysis/parser/declarations.rs` — Parse `-> ReturnType` in method definitions
- `ast.rs` — Wire `return_type` field, add parameter type support
- `codegen/core_erlang/` — Generate `-spec` attributes in Core Erlang module attributes
- `semantic_analysis/type_checker.rs` — Check annotated types against inferred types

### Phase 3: Protocols (L effort)

**Components:**
- `ast.rs` — Add `ProtocolDefinition` AST node
- `source_analysis/parser/declarations.rs` — Parse `Protocol define:` syntax
- `semantic_analysis/` — Protocol registry, conformance checking
- `codegen/` — Generate protocol metadata
- `runtime/` — Runtime protocol query support (`conformsTo:`, `protocols`)

### Phase 4: Advanced Types (XL effort)

Union types, generic types, singleton types, type narrowing. Deferred to future ADR.

## Implementation Tracking

| Phase | Issue | Description | Size | Status |
|-------|-------|-------------|------|--------|
| 1 | TBD | Type inference from class definitions | L | Planned |
| 2 | TBD | Optional type annotations + Dialyzer spec generation | M | Planned |
| 3 | TBD | Protocol definitions and structural conformance | L | Planned |
| 4 | TBD | Advanced types (union, generic, singleton, narrowing) | XL | Future |

## References

- Related ADRs:
  - [ADR 0005: BEAM Object Model](0005-beam-object-model-pragmatic-hybrid.md) — Class hierarchy and method resolution
  - [ADR 0013: Class Variables, Methods, Instantiation](0013-class-variables-class-methods-instantiation.md) — Class system that types check against
  - [ADR 0024: Static-First IDE Tooling](0024-static-first-live-augmented-ide-tooling.md) — Types serve the tooling story
- Language spec: `docs/beamtalk-language-features.md` — Optional Type Annotations, Protocols sections
- Existing infrastructure:
  - `crates/beamtalk-core/src/semantic_analysis/type_checker.rs` — Stub TypeChecker
  - `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/` — ClassHierarchy with method tables
  - `crates/beamtalk-core/src/ast.rs` — TypeAnnotation enum (lines 451-540)
- Prior art:
  - [Strongtalk: Typechecking Smalltalk in a Production Environment](https://bracha.org/oopsla93.pdf) — Bracha & Griswold, 1993
  - [TypeScript and the Dawn of Gradual Types](https://github.com/readme/featured/typescript-gradual-types) — GitHub README
  - [Extending Dylan's Type System](https://pure.itu.dk/en/publications/extending-dylans-type-system-for-better-type-inference-and-error-/) — Mehnert, 2010
  - [Gleam Type System](https://gleam.run/frequently-asked-questions/) — Full static on BEAM
  - [Core Erlang Typespec Encoding](https://stackoverflow.com/questions/75816103/typespecs-encoding-in-core-erlang) — `-spec` in Core Erlang attributes
