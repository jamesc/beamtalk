# ADR 0025: Gradual Typing and Protocols

## Status
Accepted (2026-02-15)

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
2. **Warnings, not errors** — Type mismatches always produce compiler warnings, never errors. Code always compiles. There is no "strict mode" — the language has one behavior everywhere.
3. **Compile-time only** — Type checking happens entirely at compile time. No runtime cost, no type tags, no overhead.
4. **Structural, not nominal** — Type compatibility is determined by what messages an object responds to (its "shape"), not its class hierarchy. A `Duck` and a `Person` that both have `walk` are both valid where "something that walks" is expected.
5. **Infer first, annotate for precision** — The compiler infers what it can from class definitions and assignments. Annotations add precision where inference falls short.
6. **BEAM integration** — Type annotations generate Dialyzer `-spec` attributes in Core Erlang, giving Erlang-level type checking for free at the BEAM boundary.
7. **Per-class typing contracts** — Classes can opt into thorough type checking with the `typed` modifier. This is a contract with the compiler, not a language mode — semantics are identical.

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
- `x := "hello"` → `x` has type `String`
- `x := true` → `x` has type `Boolean`
- `x := someUnknownThing` → `x` has type `Dynamic` (no checking)
- Method calls on known types check against ClassHierarchy method tables
- `doesNotUnderstand:` semantics — the type checker only relaxes unknown-message warnings for classes that **override** `doesNotUnderstand:` (not merely inherit it from `ProtoObject`). Explicit dynamic sends (`perform:`) also bypass checking.

**Ambiguous control flow defaults to Dynamic:**

```beamtalk
// Control flow producing different types → Dynamic (no checking)
x := condition ifTrue: [Counter spawn] ifFalse: [Timer spawn]
x increment    // No warning — x is Dynamic (could be either type)

// Single-branch or same-type → inferred
y := condition ifTrue: [42] ifFalse: [0]
y + 1          // ✅ y is Integer (both branches return Integer)
```

Phase 4 adds union types and type narrowing for control flow. In Phase 1, the rule is simple: if the compiler can't determine a single type, it falls back to `Dynamic` and stops checking. No false positives from conservative inference.

**Block/closure typing — inferred from context:**

Blocks are ubiquitous in beamtalk (control flow, iteration, callbacks). The type checker infers block parameter and return types from the message they're passed to:

```beamtalk
// collect: expects a block that takes an element and returns a value
// The compiler knows List>>collect: takes Block<E, R>
items collect: [:x | x + 1]     // x inferred as element type, result as Integer

// Unknown context → block parameters are Dynamic
myBlock := [:x | x + 1]         // x is Dynamic (no context yet)
items collect: myBlock           // Now context exists, but too late to check x

// Literal blocks in known positions are fully checked
3 timesRepeat: [counter increment]  // ✅ counter checked as Counter
```

The key insight: **literal blocks at message-send sites can be typed from context** (the compiler knows what `collect:` expects). Stored blocks assigned to variables lose context and become `Dynamic`. This matches the existing control-flow vs stored-block distinction (ADR for block semantics).

**What the compiler already knows:**
- Every class and its methods (via `ClassHierarchy`)
- Superclass chains and inheritance
- Sealed classes (no unknown subclasses)
- Built-in primitive types and their full method tables

**Boundaries of inference:**

- **Erlang interop:** Calls to Erlang modules (`erlang:length/1`, `lists:reverse/1`) return `Dynamic` unless we ship type stubs for common modules. Phase 2 could add a mechanism for declaring external module types.
- **REPL re-definitions:** When a class is re-defined in the REPL, the type checker must invalidate cached types for that class and re-check dependents. This aligns with the workspace model (ADR 0004) — type state is per-workspace.
- **Dynamic class creation:** `create_subclass/3` at runtime creates classes the compiler has never seen. These are `Dynamic` — the type checker doesn't attempt to type runtime-generated classes.
- **`doesNotUnderstand:` proxies:** If a class defines `doesNotUnderstand:`, the type checker accepts any message to instances of that class (no warnings for unknown selectors).

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

**Parameter type syntax:** Type follows the parameter name with `:` separator, matching state declaration syntax. Note: this requires a parser change — currently `amount:` tokenizes as a keyword selector part. The parser must distinguish `keyword: paramName: Type` (three-part pattern) from `keyword: paramName` (two-part). This is a Phase 2 parser change, not yet implemented.

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

**AST change required:** `MethodDefinition.parameters` currently uses `Vec<Identifier>` (names only). Phase 2 must change this to `Vec<ParameterDefinition>` with both name and optional `TypeAnnotation`, mirroring how `StateDeclaration` already stores `type_annotation: Option<TypeAnnotation>`.

**Return type syntax:** `-> Type` before the `=>` body separator. The AST field (`MethodDefinition.return_type: Option<TypeAnnotation>`) already exists but the parser does not yet populate it.

**Codegen:** Annotations generate Erlang `-spec` entries in Core Erlang module `attributes`. The spec is encoded as an abstract type representation in the module attributes list — the same format Dialyzer reads from `.beam` debug info:

```erlang
% Generated Core Erlang module attributes (abstract spec representation)
attributes ['spec' = [{'deposit', {type, fun, [{type, product, [{type, integer, []}]}, 
                                                {type, integer, []}]}}],
            'behaviour' = ['gen_server']]
```

This enables Dialyzer to perform additional checking at the BEAM level — two layers of type safety.

### Phase 2b: Typed Classes (Per-Class Typing Contract)

The `typed` modifier declares that a class opts into thorough type checking. This is purely a compile-time concept — the generated BEAM bytecode is identical whether a class is typed or not. Same gen_server, same dispatch, same runtime behavior. `typed` is erased during compilation, just like all other type information.

```beamtalk
// Regular class — inference warnings only
Actor subclass: Counter
  state: value = 0
  increment => self.value := self.value + 1

// Typed class — compiler checks everything
typed Actor subclass: BankAccount
  state: balance: Integer = 0
  state: owner: String

  deposit: amount: Integer -> Integer =>
    self.balance := self.balance + amount

  withdraw: amount =>                    // ⚠️ Warning: untyped parameter in typed class
    self.balance := self.balance - amount

  getBalance -> Integer => self.balance
```

**What `typed` means:**
- All state fields should have type annotations (warn if missing)
- All method parameters should have type annotations (warn if missing)
- All methods should declare return types (warn if missing)
- Message sends checked for argument type compatibility (not just method existence)
- Generates complete Dialyzer `-spec` for every method
- Compiler reports which methods lack annotations, guiding the developer

**What `typed` does NOT change:**
- Same compilation, same bytecode, same runtime semantics
- Untyped callers can still call typed classes freely
- `doesNotUnderstand:` still works if defined
- It's advice to the compiler, not a different language

**Inheritance:** `typed` is sticky — subclasses of a typed class are automatically typed:

```beamtalk
typed Actor subclass: BankAccount
  state: balance: Integer = 0
  ...

// SavingsAccount is automatically typed (inherits from typed class)
BankAccount subclass: SavingsAccount
  state: interestRate: Float = 0.05     // ✅ Typed — compiler checks

  accrue => ...                          // ⚠️ Warning: missing return type in typed class
```

This parallels how `sealed` is inherited — if the base class makes a contract, subclasses honor it.

**Escape hatch for false positives:**

When the type checker is wrong — and it will be — developers need a way to say "trust me." The `asType:` message casts a value to a known type:

```beamtalk
typed Actor subclass: MessageRouter
  state: handlers: Dictionary = Dictionary new

  dispatch: message =>
    handler := (self.handlers at: message class) asType: Handler  // "I know this is a Handler"
    handler handle: message                                        // No warning — handler is Handler
```

`asType:` is a compile-time type assertion — it tells the type checker to treat the value as the given type. It generates no runtime code (type erasure). If the assertion is wrong, the runtime error is the same as today. (Note: `as:` was considered but conflicts with the proposed workspace registration API in ADR 0004.)

For inline suppression of specific warnings, a comment directive works:

```beamtalk
  processRaw: data =>
    // @type: Dynamic
    result := self dangerousReflectiveThing: data   // No type checking on this line
    result
```

**Use cases:**
- Library authors: "this public API is fully typed"
- Production services: "this actor handles money — type everything"
- Gradual adoption: type the critical classes, leave the rest loose

**Stdlib note:** Standard library classes (`lib/*.bt`) should be **fully annotated** (parameter types, return types) in Phase 2 to generate complete Dialyzer specs and enable precise LSP completions. However, they do not need the `typed` modifier — their method bodies are primitive-dispatched (`@primitive`), so there is no Beamtalk logic for the type checker to verify. Annotations are the valuable part; `typed` is for classes where the compiler can verify that the implementation matches the contract.

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

**Note on naming convention:** This ADR uses bare identifiers for protocol names and required methods (`Printable`, `asString`). The existing language spec (`beamtalk-language-features.md`) uses symbol syntax (`#Stringable`, `#asString`). The final surface syntax for protocol definitions will be resolved during Phase 3 implementation — bare identifiers are used here for readability.

**Protocol type syntax:** Protocol types use angle brackets (`<ProtocolName>`) to distinguish them from concrete class types. Plain identifiers (`Integer`, `Counter`) denote specific classes; angle brackets (`<Printable>`) denote "any object conforming to this protocol." This makes the structural vs. nominal distinction visible at the call site.

**Conformance is structural and automatic:**

```beamtalk
// Counter has 'asString' (inherited from Object) → conforms to Printable
// No "implements" declaration needed

// Use protocol type constraint (angle brackets = structural type)
printAll: items: <Printable> =>
  items do: [:each | Transcript show: each asString]

// Concrete type constraint (no brackets = nominal type)
deposit: amount: Integer => ...

// Error when shape doesn't match
printAll: #(1, 2, 3)          // ✅ Integer conforms to Printable
printAll: someOpaqueValue      // ⚠️ Warning: cannot verify Printable conformance
```

**Protocol query in REPL:**

```beamtalk
> Counter conformsTo: Printable
=> true
> Counter protocols
=> #(Printable)
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
- Warnings by default, `typed` class modifier for thorough checking

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
- **Risk:** Warning fatigue if inference produces too many false positives on idiomatic dynamic code. Mitigation: tune to high-confidence checks only in Phase 1.

### Smalltalk Developer
- **Phase 1:** "Finally, the compiler uses what it already knows" — pure upside
- **Phase 2:** "Type annotations are optional — I can still explore freely"
- **Phase 3:** "Protocols formalize what we already do informally with message protocols"
- **Risk:** Cultural pressure to add types everywhere. Mitigation: the language never requires annotations — `typed` is opt-in per class, and untyped code is first-class.

### Erlang/Elixir Developer
- **Phase 2:** "It generates Dialyzer specs — I get BEAM-level checking!" — huge win
- **Phase 3:** "Protocols are like Elixir protocols" — familiar concept
- **Risk:** Beamtalk type-checking warnings and Dialyzer warnings may overlap or contradict. Mitigation: Phase 2 must define deduplication strategy (see Consequences/Negative).

### Production Operator
- **All phases:** Zero runtime cost, same BEAM bytecode, same observability
- **Phase 2:** Dialyzer specs enable standard BEAM type checking in CI
- **Risk:** No way to fail the build on type warnings without a `--strict` flag. Mitigation: `typed` classes make annotation completeness visible in source; CI can lint for `typed` class annotation coverage.

### Tooling Developer (LSP)
- **Phase 1:** Type inference enables precise completions and hover
- **Phase 2:** Return types enable signature help and go-to-type
- **Phase 3:** Protocol types enable interface-level navigation
- **This is the primary driver** — types exist to serve tooling
- **Risk:** Type cache invalidation on REPL re-definitions. Phase 1 must handle class hierarchy changes incrementally.

## Steelman Analysis

### For Full Static (Option C — Rejected)
- 🧑‍💻 **Newcomer**: "I want the compiler to catch everything — TypeScript strict mode is great"
- 🎩 **Smalltalk purist**: "I have nothing good to say about mandatory types"
- ⚙️ **BEAM veteran**: "Gleam proves full static works on BEAM and catches more bugs"
- 🏭 **Operator**: "Mandatory types mean fewer production crashes — I'd trade REPL convenience"
- 🎨 **Language designer**: "Sound type systems are more elegant — no 'Dynamic' escape hatch"
- **Our answer:** `typed` classes give operators and library authors the strictness they want, scoped to the classes that matter, without imposing it globally.

### For Pure Inference Only (No Annotations — Rejected)
- 🧑‍💻 **Newcomer**: "Zero syntax to learn — the compiler just figures it out"
- 🎩 **Smalltalk purist**: "Smalltalk never needed annotations and neither should we"
- ⚙️ **BEAM veteran**: "But without annotations, no Dialyzer specs — I lose BEAM tooling"
- 🏭 **Operator**: "If inference catches 80% of bugs with zero cost, why add annotation complexity?"
- 🎨 **Language designer**: "Inference alone can't disambiguate — `x := self getValue` returns what? And annotations create a second language to maintain."
- **Our answer:** Phase 1 IS pure inference. Annotations (Phase 2) are additive — you only annotate when inference falls short or you want Dialyzer integration.

### For Global Strict Mode (--strict flag — Rejected)
- 🏭 **Operator**: "I want CI to fail on type warnings — just give me a flag"
- 🧑‍💻 **Newcomer**: "TypeScript has `strict: true` and it works fine"
- **Why rejected:** Strict mode means the same code behaves differently depending on who runs it. "Works in dev, fails in CI" is a terrible developer experience. Two modes means testing two behaviors. The `typed` class modifier is better — strictness is a property of the code itself, not the environment.

### Tension Points
- **Newcomers and operators** would prefer stricter checking — `typed` classes give them per-class opt-in without imposing it globally
- **BEAM veterans** want `-spec` generation — which requires annotations (Phase 2), not just inference
- **Resolution:** Gradual approach with `typed` classes satisfies all: inference for exploration, `typed` for production-critical classes, specs for BEAM

## Alternatives Considered

### Alternative A: Do Nothing (Status Quo)
Keep the compiler fully dynamic — no type checking, no inference, no warnings.

```beamtalk
// Today: typos are only caught at runtime
c := Counter spawn
c decremet                // No warning — crashes at runtime with doesNotUnderstand:
```

**Rejected because:**
- IDE tooling (ADR 0024) cannot provide quality completions without type info
- The compiler already has class hierarchy data — discarding it is waste
- Every typo is a runtime surprise, even when the compiler could have caught it
- However: this IS the current state, and it works. Type checking is an investment with deferred payoff.

### Alternative B: Mandatory Static Typing (Gleam-style)
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

### Alternative C: Nominal Typing (Java-style)
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

### Alternative D: Global Strict Mode Flag
A `--strict` compiler flag that promotes type warnings to errors, like TypeScript's `strict: true`.

**Rejected because:**
- Same code behaves differently depending on who runs it — "works in dev, fails in CI"
- Forces testing two configurations (strict and non-strict)
- Environment-dependent behavior is a design smell
- `typed` class modifier is better — strictness is a property of the code itself, visible in source, inherited through the class hierarchy, and consistent everywhere

## Consequences

### Positive
- Compiler catches typos and type mismatches at compile time without requiring any code changes
- IDE tooling (completions, hover, diagnostics) dramatically improves with type information
- Dialyzer `-spec` generation brings BEAM-standard type checking for free
- Protocols provide a clean abstraction for polymorphic code without inheritance coupling
- All existing code continues to work unchanged — purely additive
- `sealed` + type info enables future optimizations (bypass gen_server for known dispatch)
- Error messages become actionable: "Counter doesn't respond to 'foo'" instead of runtime crash
- `typed` classes let library authors and production teams opt into thorough checking without imposing it globally
- No strict mode flag — one behavior everywhere, no "works in dev, fails in CI" surprises

### Negative
- Type inference has limits — some code can't be typed without annotations (typed as `Dynamic`)
- Implementing a type checker is significant engineering effort (especially Phase 3-4)
- Type error messages must be carefully designed to not overwhelm users
- Protocol conformance checking on large hierarchies has performance implications for compilation
- Two-layer checking (Beamtalk + Dialyzer) could produce duplicate or contradictory warnings — Phase 2 must define a deduplication strategy (e.g., suppress Beamtalk warnings for methods with `-spec`)
- Warning fatigue risk: if warnings are never errors, codebases may accumulate permanent warning debt. `typed` classes mitigate this by making annotation completeness visible.
- REPL invalidation of the type cache adds complexity to the workspace model — re-defining a class must propagate type changes to all dependents

### Neutral
- Generated BEAM bytecode is unchanged — types are erased at compile time
- Runtime performance is unaffected (no type tags, no runtime checks)
- Existing tests don't need modification
- Dynamic features (`doesNotUnderstand:`, runtime class creation) continue working

## Implementation

### Phase 1: Type Inference (M-L effort)

**Components:**
- `semantic_analysis/type_checker.rs` — Fill in the stub with inference rules
- `semantic_analysis/class_hierarchy/mod.rs` — Extend `MethodInfo` with return type tracking and expected block parameter types for context-sensitive block inference
- `source_analysis/parser/mod.rs` — Add type-related warning diagnostics (extends existing `Diagnostic` struct)
- `queries/completion_provider.rs` — Use inferred types for better completions
- `queries/hover_provider.rs` — Show inferred types on hover

**Inference approach:**
1. Walk AST top-down through assignments
2. Track variable types in scope (extend existing `Scope`)
3. On message send: look up receiver type in ClassHierarchy, check method exists
4. On binary op: check operand types against known operator signatures
5. For literal blocks at message-send sites: look up callee method signature to infer block parameter/return types from context; stored blocks assigned to variables become `Dynamic`
6. Emit warnings for mismatches, not errors

### Phase 2: Optional Annotations (M effort)

**Components:**
- `source_analysis/parser/declarations.rs` — Parse `-> ReturnType` in method definitions; disambiguate keyword parameter type syntax
- `ast.rs` — Wire `return_type` field; replace `parameters: Vec<Identifier>` with `Vec<ParameterDefinition>` containing name + optional type annotation
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
| 1 | TBD | Type inference from class definitions | M-L | Planned |
| 2 | TBD | Optional type annotations + Dialyzer spec generation | M | Planned |
| 3 | TBD | Protocol definitions and structural conformance | L | Planned |
| 4 | TBD | Advanced types (union, generic, singleton, narrowing) | XL | Future |

## Migration Path

No migration needed. This is purely additive — all existing code compiles and runs exactly as before. Type checking introduces new warnings only; no existing behavior changes.

- **Phase 1:** Zero code changes required. New warnings may appear for existing code.
- **Phase 2:** No code changes required. Annotations are opt-in.
- **Phase 2b:** `typed` modifier is opt-in per class.
- **Phase 3:** Protocol definitions are new syntax; no existing code affected.

## References

- Related ADRs:
  - [ADR 0005: BEAM Object Model](0005-beam-object-model-pragmatic-hybrid.md) — Class hierarchy and method resolution
  - [ADR 0006: Unified Method Dispatch](0006-unified-method-dispatch.md) — ClassHierarchy used for both dispatch and type checking
  - [ADR 0013: Class Variables, Methods, Instantiation](0013-class-variables-class-methods-instantiation.md) — Class system that types check against
  - [ADR 0015: Exception Hierarchy](0015-repl-error-objects-and-exception-hierarchy.md) — Exception classes for future typed exception handling (Phase 4)
  - [ADR 0018: Document Tree Codegen](0018-document-tree-codegen.md) — `-spec` generation should use Document API
  - [ADR 0023: String Interpolation](0023-string-interpolation-and-binaries.md) — Type inference applies within interpolation expressions
  - [ADR 0024: Static-First IDE Tooling](0024-static-first-live-augmented-ide-tooling.md) — Types serve the tooling story
- Language spec: `docs/beamtalk-language-features.md` — General language overview. Note: its "Optional Type Annotations" and "Protocols" sections describe an earlier design (explicit `implements:` and symbol-named protocols); ADR 0025 supersedes those details with the structural, automatic-conformance approach described here.
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
