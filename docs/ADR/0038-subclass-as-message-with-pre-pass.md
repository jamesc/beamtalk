# ADR 0038: `subclass:` as Real Message with Compiler Pre-Pass

## Status
Proposed (2026-02-23)

## Context

### Problem Statement

`Object subclass: Counter` is currently recognised by the Beamtalk parser as a grammar production — a class definition construct. The parser emits a `ClassDefinition` AST node, and all subsequent compiler phases treat class definitions as inherently static, first-class language constructs.

This contradicts Principle 6 (Messages All The Way Down): in Smalltalk, `subclass:` is just a message sent to a class object. There is no special grammar for defining a class. The parser sees `Object subclass: Counter` the same way it sees any other keyword message send.

The question arose naturally while building out ADR 0032 (Early Class Protocol) and ADR 0036 (Full Metaclass Tower): if `Class` and `Behaviour` are real Beamtalk objects that understand messages, why is the *creation* of a class not also a message? You can inspect `Counter superclass` as a message, you can send `Counter canUnderstand: #foo` as a message — but you cannot intercept or override the creation of `Counter` itself.

### Current State

The parser (`declarations.rs`) contains a dedicated grammar production for class definitions. The `ClassDefinition` AST node carries:
- Class name and superclass name
- State declarations (field names and defaults)
- Method definitions
- Modifiers (`abstract`, `sealed`, `typed`)

All compiler phases that follow — `ClassHierarchy::build`, semantic analysis validators, codegen routing (actor vs value type), LSP indexing — consume `ClassDefinition` nodes directly and assume class structure is fully known at parse time.

The class creation message `subclass:` does not exist as a Beamtalk method in any stdlib file. It is handled entirely by the compiler.

### Constraints

- **No persistent image**: Beamtalk compiles to BEAM modules loaded from disk on cold start. The Pharo approach (pure message dispatch into a live image) is not directly available without significant infrastructure work.
- **Static analysis needs hierarchy early**: Codegen must know whether a class inherits from `Actor` before generating module structure. LSP completions require the hierarchy to be indexed before answering queries.
- **Bootstrap ordering (ADR 0032)**: `ProtoObject → Object → Behaviour → Class → Actor` must be wired before user classes load. Whatever mechanism creates classes must participate in this sequence.
- **ADR 0024 (Static-First, Live-Augmented Tooling)**: The LSP operates on parsed source, augmented by live queries when a workspace is running. The static layer must still provide completions without a live process.
- **REPL self-hosting trajectory (ADR 0034)**: As stdlib self-hosting progresses, the Beamtalk REPL will eventually be ported to pure Beamtalk. At that point, evaluating `Object subclass: Foo ...` in the REPL must work *at runtime* — without invoking the Rust compiler. Whatever dynamic class creation mechanism is chosen now becomes the foundation for that path. A design that makes dynamic creation a second-class afterthought will need to be revisited when REPL self-hosting arrives.

### The Metaclass Parallel

ADR 0032 and ADR 0036 already solve a closely analogous bootstrap problem. The metaclass tower (`Counter class class == Metaclass`) is established before user code runs via a pre-wiring pass that sets up the initial object relationships. The class protocol then operates via normal message dispatch. This ADR proposes the same pattern for class *creation*: a compiler pre-pass wires up the hierarchy statically, after which `subclass:` can be treated as a real message at runtime.

## Decision

Introduce a **class definition recognition pre-pass** in the compiler and treat `subclass:` as a real Beamtalk message at the protocol level.

Concretely:

1. **Grammar change**: `Object subclass: Counter` becomes a message send expression in the parser, not a dedicated grammar production. The existing `ClassDefinition` AST node is replaced by a `MessageSend` node with the receiver `Object`, selector `subclass:`, and a class body literal.

2. **Pre-pass**: A new compiler phase, run immediately after parsing and before semantic analysis, pattern-matches `X subclass: Y` message sends and extracts class hierarchy information. This pre-pass produces the same `ClassHierarchy` data structure that semantic analysis and codegen already consume. All downstream phases are unchanged.

3. **Runtime method**: `subclass:` is defined as a sealed method on `Class` in `stdlib/src/Class.bt`, backed by an `@intrinsic classDefine` that registers the class with the runtime. This makes `subclass:` inspectable, overridable (on custom root classes outside the sealed stdlib hierarchy), and consistent with the message-passing model.

4. **Syntax**: User-visible syntax is **unchanged**. `Object subclass: Counter` continues to work exactly as before. The change is internal — the parser now treats it as a message send that the pre-pass recognises, rather than a dedicated grammar rule.

### What Changes

```
Before: parse → ClassDefinition AST → semantic analysis → codegen
After:  parse → MessageSend AST → pre-pass (recognise subclass: patterns)
                                        → ClassHierarchy (same structure)
                                 → semantic analysis (unchanged)
                                 → codegen (unchanged)
```

The pre-pass is deliberately narrow: it only recognises the canonical `X subclass: Y` pattern (with optional `state:` arguments and a method body). Unusual patterns (dynamic `subclass:` calls with computed receivers or arguments) are not recognised by the pre-pass and do not define classes statically — they compile as ordinary message sends.

### Runtime Protocol

```beamtalk
// stdlib/src/Class.bt — added method
/// Define a new class as a subclass of the receiver.
/// Called by the compiler-generated bootstrap for each class definition.
/// Can be overridden on custom root classes to intercept class creation.
///
/// ## Examples
/// ```beamtalk
/// Object subclass: Point   // => Point (the new class object)
/// ```
sealed subclass: classSpec =>
  @intrinsic classDefine
```

For stdlib classes and all classes inheriting from `Object`, this method is `sealed` — it cannot be overridden. This matches the existing sealed protocol on `Behaviour` (ADR 0032) and limits the blast radius during the v0.1 metaclass tower bootstrap.

### REPL Session

User-visible behaviour is unchanged:

```beamtalk
>> Object subclass: Point
   state: x = 0
   state: y = 0
   x => self.x
   y => self.y
=> Point

>> Point new
=> a Point (x: 0, y: 0)

>> Point superclass
=> Object

>> Point canUnderstand: #subclass:
=> true

>> Object respondsTo: #subclass:
=> true
```

The key new capability: `subclass:` is now an inspectable message, not a parser special form.

### Error Examples

```beamtalk
>> Object subclass: 42
=> Error: subclass: argument must be a class name, got Integer

>> Object subclass: Point
   subclass: Point   // duplicate definition
=> Error: class Point is already defined — use hot reload to update
```

## Prior Art

### Pharo / Squeak Smalltalk

`subclass:instanceVariableNames:classVariableNames:package:` is a plain keyword message sent to a class object. The parser has no special knowledge of it. `ClassBuilder` (a regular Smalltalk object) receives the message and constructs the new class, wires up metaclasses, and registers it in the system dictionary.

**How tooling works without grammar**: Pharo's IDE operates on the *live image* — a persistent object graph in memory. The IDE queries the live class hierarchy via reflection (`allSubclasses`, `allSelectors`, etc.) rather than parsing source text. There is no source-to-model mismatch because the model *is* the running system.

**What we adopt**: The principle that `subclass:` is a regular message, inspectable and in principle overridable.

**What doesn't translate**: The image-based tooling strategy. Beamtalk compiles to BEAM modules and cannot rely on a persistent live image for the static LSP layer. The pre-pass bridges this gap.

### Newspeak

Class declarations in Newspeak are formal grammar productions, but they **evaluate to class objects** as first-class values. The syntax is parsed by the compiler, but the semantics are that evaluating a class declaration produces a result. Classes are nested, late-bound, and can be parameterised.

Newspeak uses **exemplars** for IDE tooling: the IDE maintains live prototype instances of each class, queries them reflectively, and keeps them in sync with source edits. This augments static parsing with live reflection.

**What we adopt**: The idea that class definitions should produce first-class values (class objects), even if the syntax is still parsed structurally. Our pre-pass plays the role Newspeak's parser plays for static tooling; the runtime method plays the role the exemplar plays for live reflection.

**What doesn't translate**: Newspeak's fully nested class hierarchy and late-bound class names — Beamtalk's flat namespace (ADR 0031) makes this unnecessary for v0.1.

### GNU Smalltalk

Like Pharo, `subclass:` is a message send, but GNU Smalltalk is file-based rather than image-based. Source files are loaded and the message sends execute to build the class hierarchy. External tools rely on recognising the `subclass:` pattern in source to build a static model — effectively a pre-pass.

**What we adopt**: This is the closest precedent to Beamtalk's approach. GNU Smalltalk shows that file-based class creation via `subclass:` messages works, and external tooling can handle it via pattern recognition.

### Ruby

Ruby has both `class Foo < Bar` (grammar) and `Class.new(Bar) { ... }` (runtime). The grammar form is syntactic sugar that the parser desugars to a `type`-like call. Sorbet and RBS files provide static declarations for dynamically-created classes.

**What we avoid**: The two-tier tooling story (good support for grammar form, degraded support for dynamic form). Our pre-pass means all `subclass:` calls that match the canonical pattern get full static support; no stub files needed.

### Elixir

`defmodule` is a compile-time macro. The compiler fully understands module structure statically. No runtime interception is possible.

**Contrast**: Beamtalk deliberately goes in the opposite direction — `subclass:` remains statically analysable via the pre-pass, but is also a runtime message. Elixir's approach trades runtime flexibility for simplicity; Beamtalk's approach keeps both.

### Python

`class Foo(Bar): ...` desugars to `type('Foo', (Bar,), {...})`. The CPython compiler does this transformation explicitly. Static type checkers (mypy, pyright) recognise the `class` syntax and build a type model from it; dynamic `type()` calls are opaque to them (requiring stub files).

**What we adopt**: The insight that a grammar form can be a thin wrapper over a runtime operation. Beamtalk's pre-pass is the equivalent of the Python compiler's desugar step.

## User Impact

### Newcomer (from Python/JS/Ruby)

- **No visible change**: Syntax is identical. The newcomer writes `Object subclass: Foo` and it works exactly as before.
- **Improved discoverability**: `Object respondsTo: #subclass:` returns `true`. Sending `Object class` then `Counter class` and asking what messages they understand now includes `subclass:`. The class creation protocol is part of the explorable message vocabulary.
- **Error messages unchanged**: The pre-pass validates the same constraints as the grammar did (duplicate class names, sealed superclass violations). Errors appear at the same point in the workflow.

### Smalltalk Developer

- **Long-awaited alignment**: The single biggest departure from Smalltalk semantics is corrected. `subclass:` is now a real message, consistent with Principle 6.
- **Inspectable**: `Object whichClassIncludesSelector: #subclass:` returns `Class`. `Class >> #subclass:` can be browsed like any other method.
- **Overridable on custom roots**: A user writing a DSL or metaclass extension can subclass `ProtoObject` and define a custom `subclass:` — the sealed restriction only applies to classes inheriting from `Object`.
- **Still grammar-structured**: The Smalltalk purist may note that the syntax is still parsed structurally (the pre-pass recognises the pattern rather than truly evaluating a dynamic dispatch). This is a pragmatic compromise for a file-based compiler; the semantics are correct even if the evaluation order is constrained.

### Erlang/BEAM Developer

- **No observable change at the BEAM level**: Class modules are generated identically. The `bt@counter` module looks the same whether `subclass:` was grammar or a message.
- **Debuggable creation protocol**: `dbg:tpl(beamtalk_class, define, ...)` can now trace class creation through a single well-defined code path.
- **Consistent with BEAM module loading**: The pre-pass runs at compile time; the runtime `classDefine` intrinsic runs at module load time — consistent with how Erlang modules initialise their state on load.

### Production Operator

- **No performance impact**: The pre-pass is a single-pass O(N) scan of the parsed AST. Compile times are unchanged.
- **No hot-reload impact**: Class hot reload (live patching) works via the existing `beamtalk_class_registry` mechanism. The change to `subclass:` being a message rather than grammar is invisible to the reload path.
- **Cleaner bootstrap sequence**: The `classDefine` intrinsic participates in the same bootstrap ordering as `Behaviour` and `Class` (ADR 0032). One less special case in the bootstrap sequence.

### Tooling Developer

- **Pre-pass is a seam**: The pre-pass produces the same `ClassHierarchy` structure that all downstream tools already consume. LSP, semantic analysis, and codegen have zero changes.
- **Pattern recognition, not evaluation**: The pre-pass is not a full evaluator — it pattern-matches the canonical `X subclass: Y` form. Tooling authors do not need to simulate message dispatch to find class definitions.
- **Richer AST**: The `ClassDefinition` node becomes a `MessageSend` node. LSP hover on `subclass:` can now show the method signature and documentation from `Class.bt`, rather than synthetic compiler-generated help text.

## Steelman Analysis

### Option A: Keep `subclass:` as Grammar (Status Quo)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "The current syntax is clear and learnable. Telling people 'this is a message send' when they're first learning the language adds cognitive overhead for zero visible benefit. Grammar gives better error messages at parse time." |
| **Smalltalk purist** | "The pre-pass is still structural analysis of source, not dynamic message dispatch. Until `subclass:` can truly be intercepted at runtime with arbitrary logic (like Pharo's `ClassBuilder`), calling it a 'message' is marketing, not engineering." |
| **BEAM veteran** | "Every layer of abstraction is a failure mode. Grammar is the simplest possible representation of a class definition. The pre-pass adds a new compiler phase, new code paths, new tests, and new failure modes. For what gain?" |
| **Operator** | "If it ain't broke, don't fix it. The existing grammar approach is battle-tested and simple. Don't add complexity to satisfy a philosophical principle." |
| **Language designer** | "You're solving the wrong problem. The question isn't 'grammar or message?' — it's 'what semantics do I want, and what syntax best expresses them?' Keep `Object subclass: Counter` as grammar — it's unambiguous, toolable, learnable. But compile the grammar form to a call on a `ClassBuilder` protocol that the runtime exposes. Then you get clean syntax *and* runtime interceptability *and* a foundation for a self-hosted REPL. TypeScript didn't make `class` a runtime function call — but Roslyn exposes the entire compilation process as an API. That's the model. Separate the concerns rather than conflating them." |

**Tension point**: The BEAM veteran and operator cohorts have the strongest argument for the status quo. The language designer cohort makes the most interesting argument — not for the status quo, but for a *different* approach (Alternative D below) that achieves more than Option B while adding less complexity.

### Option C: Fully Dynamic `subclass:` (No Pre-Pass)

| Cohort | Strongest argument |
|--------|-------------------|
| **Smalltalk purist** | "This is the only honest approach. If `subclass:` is a message, it must be dispatched dynamically. The pre-pass approach gives you the *syntax* of a message without the *semantics*. Real Smalltalk lets you replace `ClassBuilder` entirely. Do it right." |
| **Language designer** | "Option B (pre-pass) creates a split reality: the compiler thinks in terms of statically-extracted class definitions while the runtime thinks in terms of message sends. Eventually these two representations diverge. A fully dynamic approach has one source of truth: the runtime." |
| **Newcomer (REPL user)** | "I want to define a class dynamically in the REPL without writing a source file. If class creation is truly a message, I can do `Object subclass: (Transcript readLine asSymbol)` interactively." |
| **REPL self-hosting** | "When the Beamtalk REPL is ported to pure Beamtalk, it will evaluate `Object subclass: Foo` without going through the Rust compiler. Only a fully dynamic mechanism makes this possible without special-casing the REPL." |

**Why we defer it**: The primary blocker is ADR 0024 (Static-First, Live-Augmented Tooling). Without pre-pass extraction, the LSP cannot provide completions for a class until after it has been evaluated in a running workspace. For users not running a workspace (editing a file offline), the IDE would have no knowledge of class structure. This degrades the experience for a majority of users. The fully dynamic approach is the right long-term direction but requires the Live-Augmented layer to be more mature first. The ClassBuilder protocol (Alternative D) bridges this gap — it provides the dynamic foundation Option C requires while letting Option B's pre-pass continue to drive the static layer.

## Alternatives Considered

### Alternative A: Keep `subclass:` as Grammar (Status Quo)

Retain the dedicated parser grammar production. `subclass:` remains a syntax form, not a message.

**Rejected because**: Violates Principle 6. Makes `subclass:` the only significant class-related operation that is not inspectable via message vocabulary. As the class protocol matures (ADR 0032, 0036), this becomes an increasingly conspicuous inconsistency. The pre-pass approach eliminates the inconsistency at low cost.

### Alternative C: Fully Dynamic `subclass:` Without Pre-Pass

Make `subclass:` a true runtime message send with no pre-pass extraction. The compiler treats all message sends uniformly; class hierarchy information is known only after evaluation.

**Rejected because**: Breaks static LSP tooling (ADR 0024 Phase 1). Users editing source without a live workspace would get no completions, hover info, or go-to-definition for class members. Codegen cannot determine actor vs value-type routing without hierarchy information. Deferred until the Live-Augmented tooling layer matures sufficiently to make this viable.

### Alternative D: Grammar Compiles to ClassBuilder Protocol

Keep `Object subclass: Counter` as dedicated grammar — preserving clean parse-time error messages and unambiguous syntax. But make the grammar form **compile to a call on a `ClassBuilder` protocol** that the runtime exposes as a real message chain. Dynamic class creation (e.g., in a self-hosted REPL) goes through the same protocol directly.

```beamtalk
// What the user writes (grammar, unchanged):
Object subclass: Counter
  state: count = 0
  increment => count := count + 1

// What the compiler emits (desugar target):
Class classBuilder
  createNamed: #Counter
  superclass: Object
  fields: #{ count => 0 }
  methods: #{ increment => [...] }
```

`Class >> classBuilder` returns a `ClassBuilder` object. `ClassBuilder` is a real stdlib class with methods `createNamed:superclass:fields:methods:`. The grammar form is syntactic sugar — identical in semantics to calling the builder directly.

**For the self-hosted REPL**, evaluating `Object subclass: Foo` at runtime bypasses the Rust compiler entirely and calls `Class classBuilder createNamed: #Foo ...` directly. The static LSP layer continues to use the grammar form and pre-pass for file-based source. No special-casing of the REPL is needed — the dynamic path is just the protocol path.

**Framework authors** can intercept class creation by overriding `classBuilder` on their root class:

```beamtalk
ProtoObject subclass: TrackedRoot
  class classBuilder => TrackingClassBuilder new
```

Every subclass of `TrackedRoot` will go through `TrackingClassBuilder` — without any compiler changes.

**Why this is stronger than Option B**: The pre-pass in Option B is structural pattern matching — it understands `subclass:` because the pre-pass knows to look for it. The ClassBuilder approach makes the grammar form genuinely compositional: the compiler doesn't understand `subclass:` as a special concept, it just desugars to a builder call. If you add a new kind of class definition in future (e.g., `TraitDefinition`), you add a new builder method — not a new pre-pass case.

**Why this is deferred**: `ClassBuilder` requires runtime infrastructure (a new stdlib class, builder protocol methods, integration with `beamtalk_class_registry`) that is best built incrementally on top of the pre-pass mechanism established in Option B. Option B is Phase 1; Alternative D is Phase 2.

**Relationship to Option B**: Alternative D is not a rejection of Option B — it is its completion. Option B establishes the pre-pass seam and makes `subclass:` an inspectable message. Alternative D replaces the `@intrinsic classDefine` backing with a real `ClassBuilder` protocol, giving the REPL and framework authors a clean dynamic creation path.

### Alternative E: Two Syntaxes — Grammar for Static, Message for Dynamic

Retain the grammar form `Object subclass: Counter` for static class definitions, and add a separate runtime method `Object defineSubclass: Counter` for dynamic use.

**Rejected because**: Creates two class creation paths that must be kept in sync. Confuses users about which form to use. Alternative D achieves the same goal (one syntax, both static and runtime semantics) more cleanly via the builder desugar.

## Consequences

### Positive
- **Principle 6 alignment**: `subclass:` is now a message, consistent with "Messages All The Way Down"
- **Inspectable creation protocol**: `Object respondsTo: #subclass:` is `true`; the method can be found via `whichClassIncludesSelector:` and browsed in the REPL
- **Custom root classes**: Classes rooted at `ProtoObject` (rather than `Object`) can provide custom `subclass:` implementations — enabling future DSL and metaclass work without compiler changes
- **Cleaner AST**: No dedicated `ClassDefinition` node; class definitions are message sends in the AST like everything else
- **Precedent for future dynamism**: The pre-pass establishes the pattern for eventually supporting fully dynamic class creation (Alternative C / D) as the live tooling layer matures
- **ADR 0024 LSP seam preserved**: The pre-pass outputs the same `ClassHierarchy` structure; all tooling is unchanged
- **REPL self-hosting foundation**: The `classDefine` intrinsic established here is the hook that a future `ClassBuilder` protocol (Alternative D) replaces — making it possible to evaluate class definitions in a pure-Beamtalk REPL without involving the Rust compiler

### Negative
- **Pre-pass is still structural**: A Smalltalk purist can correctly observe that the pre-pass is pattern matching on syntax, not true dynamic dispatch. The runtime `classDefine` intrinsic fires at module load time, not at parse time. This is a pragmatic compromise.
- **New compiler phase**: The pre-pass adds a new phase to the compilation pipeline — new code to maintain, test, and debug.
- **Error message timing shifts**: Some class definition errors (e.g., malformed state declarations) shift from parse errors to pre-pass errors or semantic errors. Error quality must be maintained deliberately.

### Neutral
- **User-visible syntax is unchanged**: No migration needed for existing code.
- **Codegen is unchanged**: Actor vs value-type routing continues to read from `ClassHierarchy` built by the pre-pass, which is identical in structure to what the grammar produced.
- **Performance unchanged**: The pre-pass is a single O(N) scan; compile times are not materially affected.

## Implementation

### Phase 1: Pre-Pass Infrastructure

**Affected components:** `beamtalk-core` (parser, new pre-pass module)

1. Add a `ClassDefinitionRecogniser` pre-pass module in `crates/beamtalk-core/src/source_analysis/`
2. The pre-pass accepts a parsed AST (containing `MessageSend` nodes) and returns a `ClassHierarchy`, matching the existing `ClassHierarchy::build(module)` interface
3. Update the parser's `declarations.rs` to emit `MessageSend` nodes for `X subclass: Y` patterns instead of `ClassDefinition` nodes — the rest of the AST is unchanged
4. Wire the pre-pass into the compilation pipeline between parsing and semantic analysis
5. Verify that all existing tests pass with zero changes to semantic analysis, validators, or codegen

### Phase 2: Runtime Method

**Affected components:** `stdlib/src/Class.bt`, `beamtalk-core` (codegen/intrinsics)

1. Add `@intrinsic classDefine` codegen handler in the compiler
2. Add the `subclass:` sealed method to `Class.bt` backed by `@intrinsic classDefine`
3. Register `subclass:` in the static `ClassHierarchy` builtins (Rust semantic analysis)
4. Add a post-bootstrap assertion verifying `Object respondsTo: #subclass:` returns `true`

### Phase 3: AST Cleanup

**Affected components:** `beamtalk-core` (AST, all consumers)

1. Remove the `ClassDefinition` AST node type (now superseded by `MessageSend` + pre-pass)
2. Update all AST consumers (formatter, LSP hover, definition provider) to recognise class-defining message sends via pre-pass output rather than dedicated AST node type
3. Update diagnostic messages to maintain error quality parity with the previous grammar form

### Future Work (Not Part of This ADR)

- **ClassBuilder Protocol (Alternative D)**: Replace the `@intrinsic classDefine` backing with a real `ClassBuilder` stdlib class. This is the natural Phase 2 — builds directly on top of the pre-pass seam established here. Prerequisite for REPL self-hosting and framework-level class creation interception.
- **Live-Augmented Dynamic Classes**: Once ADR 0024's Live-Augmented layer is sufficiently mature, lift the pre-pass requirement for classes defined in a running workspace — enabling fully dynamic class creation via `subclass:` message sends at the REPL
- **Custom `subclass:` on ProtoObject roots**: Document and test the override path for DSL authors

## References
- Related ADRs: [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md) (Object Model), [ADR 0006](0006-unified-method-dispatch.md) (Unified Dispatch), [ADR 0013](0013-class-variables-class-methods-instantiation.md) (Instantiation Protocol), [ADR 0024](0024-static-first-live-augmented-ide-tooling.md) (Static-First Tooling), [ADR 0032](0032-early-class-protocol.md) (Early Class Protocol), [ADR 0036](0036-full-metaclass-tower.md) (Full Metaclass Tower)
- Principle: [Principle 6 — Messages All The Way Down](../beamtalk-principles.md)
- Prior art: [Pharo Reflective Core Booklet](https://books.pharo.org/booklet-ReflectiveCore/), [Newspeak Specification](https://newspeaklanguage.org/spec/newspeak-spec.pdf), [GNU Smalltalk: Creating Classes](https://www.gnu.org/software/smalltalk/manual/html_node/Creating-classes.html)
