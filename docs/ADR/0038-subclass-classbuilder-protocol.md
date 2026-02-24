# ADR 0038: `subclass:` Grammar Desugaring to ClassBuilder Protocol

## Status
Accepted (2026-02-24)

## Context

### Problem Statement

`Object subclass: Counter` is currently a grammar production. The parser emits a `ClassDefinition` AST node and the compiler handles class registration via hardcoded Erlang calls to `beamtalk_object_class:start/2`. There is no Beamtalk-level protocol for class creation — it is an opaque compiler operation.

This creates two related problems:

1. **Principle 6 violation**: `subclass:` is the only significant class-related operation that is not an inspectable Beamtalk message. Every other class protocol method (`superclass`, `canUnderstand:`, `methods`, etc.) is defined in `Behaviour.bt` or `Class.bt` following ADR 0032. Class *creation* is the conspicuous exception.

2. **No dynamic class creation path**: When the Beamtalk REPL is eventually ported to pure Beamtalk (ADR 0034 trajectory), evaluating `Object subclass: Foo` at the prompt must work at runtime without a Rust compiler round-trip. Today there is no mechanism for this. The REPL self-hosting path requires a dynamic class creation story.

### Current State

The compiler pipeline for class definitions is:

```text
parse → ClassDefinition AST → ClassHierarchy::build → codegen → bt@counter.core → erlc → bt@counter.beam
                                                                      ↓
                                              module load triggers beamtalk_object_class:start/2
                                              (hardcoded Erlang, not visible from Beamtalk)
```

`beamtalk_object_class:start/2` is called with raw atoms and maps extracted from the compiled module attributes. There is no `ClassBuilder` object, no `subclass:` method in any `.bt` file, and no way to intercept or customise class creation from Beamtalk code.

### Constraints

- **Grammar must stay**: `Object subclass: Counter` is unambiguous, toolable, and learnable. The static LSP layer (ADR 0024 Phase 1) builds the class hierarchy from parsed source without a live workspace. This must continue to work. The syntax is not changing.
- **Static analysis unchanged**: `ClassHierarchy::build` reads `ClassDefinition` AST nodes today. This pipeline is not changing — codegen is the integration point, not the parser or semantic analyser.
- **Bootstrap ordering (ADR 0032)**: `ProtoObject → Object → Behaviour → Class → Metaclass → Actor` must be wired before user classes load. `ClassBuilder` joins this sequence.
- **No persistent image**: Beamtalk compiles to BEAM modules loaded from disk on cold start. The dynamic path must work without a persistent object graph.
- **REPL self-hosting trajectory (ADR 0034)**: When the REPL is ported to pure Beamtalk, it must be able to evaluate `Object subclass: Foo ...` at runtime — defining and using a new class interactively without invoking the Rust compiler. This requires a dynamic class creation path that is first-class, not bolted on.

### The Metaclass Parallel

ADR 0032 and ADR 0036 establish `Behaviour`, `Class`, and `Metaclass` as real Beamtalk stdlib classes that define the class *protocol*. Once those exist, class creation is the only remaining part of the class system still handled by opaque Erlang infrastructure. This ADR completes the transition: class *creation* moves to the same Beamtalk-visible, inspectable layer.

## Decision

Keep `Object subclass: Counter` as **grammar** — no syntax change, no pre-pass, no AST restructuring. The `ClassDefinition` AST node, `ClassHierarchy::build`, LSP indexing, and semantic analysis validators are all **unchanged**.

What changes is what **codegen emits** for a class definition. Instead of hardcoded Erlang registration calls, every class definition compiles to a `ClassBuilder` protocol invocation. `ClassBuilder` is a real Beamtalk stdlib class — bootstrapped early, inspectable, and callable directly at runtime.

### Two Paths to Class Creation

**Path 1 — Compiled (file-based, production)**

```text
.bt source → Rust compiler (grammar → ClassDefinition AST → codegen)
           → bt@counter.core (methods as Core Erlang functions)
           → erlc → bt@counter.beam
           → module load triggers ClassBuilder call in module init
           → class registered with runtime, BEAM dispatch active
```

The compiler generates the full BEAM module (method functions, dispatch tables, OTP callbacks for actors). `ClassBuilder` handles *registration* — wiring the new class into the runtime class system. The BEAM module provides the performance-optimised method implementations.

**Path 2 — Dynamic (REPL / runtime, interpreted)**

```text
ClassBuilder called directly at runtime
→ new class gen_server created with name, superclass, fields
→ methods stored as closures in gen_server state
→ class registered with runtime, dynamic dispatch active
→ no BEAM module, no compiler round-trip
```

Dynamic classes work fully with the existing message dispatch system. Methods are closures rather than compiled BEAM functions, so performance is lower — this is the trade-off for interactive speed. Dynamic classes can be redefined freely. Their gen_server processes are long-lived — they persist until explicitly stopped via `removeFromSystem` or node shutdown, consistent with compiled class lifecycle.

### ClassBuilder.bt

```beamtalk
// stdlib/src/ClassBuilder.bt
/// Fluent builder for creating and registering Beamtalk classes.
///
/// Used by the compiler for all compiled class definitions (Path 1)
/// and directly for dynamic class creation at runtime (Path 2).
///
/// ## Compiled use (conceptual — codegen emits Core Erlang, not Beamtalk):
/// ```
/// Object classBuilder
///   name: #Counter;
///   fields: #{ count => 0 };
///   methods: #{ increment => [...], value => [...] };
///   register
/// ```
///
/// ## Dynamic use (REPL, frameworks, metaprogramming):
/// ```beamtalk
/// cls := Object classBuilder
///   name: #Greeter;
///   addMethod: #greet body: [ 'Hello!' printNl ];
///   register.
/// Greeter new greet   // => Hello!
/// ```
Actor subclass: ClassBuilder
  state: className     = nil
  state: superclassRef = nil
  state: fieldSpecs    = #{}
  state: methodSpecs   = #{}
  state: modifiers     = #()

  /// Set the class name.
  name: aSymbol =>
    className := aSymbol

  /// Set the superclass.
  superclass: aClass =>
    superclassRef := aClass

  /// Set all fields at once (used by codegen).
  fields: aDict =>
    fieldSpecs := aDict

  /// Set all methods at once (used by codegen).
  methods: aDict =>
    methodSpecs := aDict

  /// Add a single field with a default value (fluent API for dynamic use).
  addField: aName default: aValue =>
    fieldSpecs := fieldSpecs at: aName put: aValue

  /// Add a single method as a block (fluent API for dynamic use).
  /// The block receives self as first argument.
  addMethod: aSelector body: aBlock =>
    methodSpecs := methodSpecs at: aSelector put: aBlock

  /// Apply a class modifier (:abstract, :sealed, :typed).
  modifier: aSymbol =>
    modifiers := modifiers add: aSymbol

  /// Register the class with the runtime and return the new class object.
  /// Stops the builder process after registration — the builder is
  /// single-use and should not be retained.
  ///
  /// For compiled classes (Path 1): called from module init, wires the
  /// BEAM module's pre-compiled methods into the class gen_server.
  /// If the class already exists (hot reload), updates the existing
  /// gen_server state via `update_class/2` rather than failing.
  ///
  /// For dynamic classes (Path 2): creates a class gen_server with
  /// closure-based methods, no BEAM module required.
  register =>
    @intrinsic classBuilderRegister
```

`Class` gets a factory method:

```beamtalk
// stdlib/src/Class.bt — added
/// Return a new ClassBuilder for creating a subclass of the receiver.
///
/// ## Examples
/// ```beamtalk
/// Object classBuilder name: #Foo; register
/// ```
classBuilder => ClassBuilder new superclass: self
```

### Codegen Change

For every `ClassDefinition` AST node, codegen emits a ClassBuilder call in the generated module's `-on_load` (or equivalent init hook) rather than a direct `beamtalk_object_class:start/2` call:

```erlang
%% Before (hardcoded registration):
-on_load(register_class/0).
register_class() ->
    beamtalk_object_class:start('Counter', #{
        superclass => 'Actor',
        fields => [count],
        instance_methods => #{...}
    }).

%% After (ClassBuilder protocol — cascade sends all messages to same builder):
-on_load(register_class/0).
register_class() ->
    CB = beamtalk_primitive:send('bt@object':'module_class'(), 'classBuilder', []),
    _ = beamtalk_primitive:send(CB, 'name:', [#{'$sym' => 'Counter'}]),
    _ = beamtalk_primitive:send(CB, 'fields:', [#{'count' => 0}]),
    _ = beamtalk_primitive:send(CB, 'methods:', [#{'increment' => fun ?MODULE:'increment'/1,
                                                    'value'     => fun ?MODULE:'value'/1}]),
    beamtalk_primitive:send(CB, 'register', []).
```

The BEAM module still provides the compiled method functions (`increment/1`, `value/1`). ClassBuilder wires them into the class gen_server. Dynamic classes pass closures instead of function references — the class gen_server stores and dispatches either form.

### REPL Self-Hosting Design

When the Beamtalk REPL is ported to pure Beamtalk, the two paths map cleanly to two user experiences:

**Interactive / prototyping** — Path 2 (dynamic, no compiler):

```beamtalk
// User types in REPL — live, no compiler round-trip:
Object subclass: Point
  state: x = 0
  state: y = 0
  distanceTo: other => ((self.x - other.x) squared + (self.y - other.y) squared) sqrt

p := Point new
// => a Point (x: 0, y: 0)
```

The REPL desugars the class definition to a cascade on the ClassBuilder: `Object classBuilder name: ...; addField: ...; register`. The builder is an Actor — each cascaded message mutates its state. `register` creates the class and stops the builder process. The class is live immediately. Methods are interpreted closures.

**Compile and save** — Path 1 (compiled, via compiler port ADR 0022):

```beamtalk
// User explicitly compiles for performance/persistence:
Compiler compileFile: 'Point.bt'.
// => bt@point.beam generated, loaded, ClassBuilder called in module init
```

The REPL self-hosting story requires both paths but does not require them simultaneously. Path 2 unblocks interactive development; Path 1 is needed for performance-critical and persistent classes. This ADR delivers Path 2. Path 1 is already partially in place — the codegen change (emitting ClassBuilder calls) is the only new piece.

### Bootstrap Sequence

```text
ProtoObject → Object → Behaviour → Class → Metaclass → ClassBuilder → Actor → user modules
```

`ClassBuilder` is an Actor subclass — each builder instance is a short-lived gen_server process that accumulates configuration via cascaded messages. It can be pre-wired in the bootstrap sequence using the same mechanism as `Behaviour` and `Class` (ADR 0032) — a hand-wired Erlang module populated before the normal class loading sequence begins. `ClassBuilder` is placed after `Metaclass` because it is a user-facing stdlib class, not part of the metaclass tower. Post-bootstrap assertion: `Class respondsTo: #classBuilder` must return `true`.

### Cascade Pattern and Builder Lifecycle

ClassBuilder uses the **cascade pattern**: all setter messages (`;`-separated) are sent to the same builder instance, which mutates its internal state. The final `register` message (no `;`) is the cascade's return value — the newly created class object.

```beamtalk
Object classBuilder
  name: #Counter;
  fields: #{ count => 0 };
  methods: #{ increment => [...], value => [...] };
  register
```

This avoids constructor explosion (one method with N positional arguments) — the same motivation behind Pharo's `ClassBuilder`. Each aspect of the class definition is a separate, named message.

**Cleanup**: `register` is a terminal operation. After creating the class and wiring it into the runtime, `register` stops the builder's gen_server process. The builder is single-use: created, configured via cascade, consumed by `register`, then gone. No builder process leaks. For compiled classes (Path 1), the builder is created and stopped within the `on_load` function. For dynamic classes (Path 2), the builder is created and stopped within the REPL expression evaluation.

**Why Actor, not value type**: Cascades send all messages to the *same* receiver — they discard intermediate return values. This requires mutable state, which means Actor (gen_server). A value type with immutable-update semantics (BT-833) would require chained return values, which is incompatible with cascade syntax. Since cascades are the idiomatic Smalltalk pattern for builders, Actor is the natural choice. The process-per-builder cost is negligible: one short-lived process per class definition, created at module load time or REPL evaluation.

### REPL Session

```beamtalk
>> Class respondsTo: #classBuilder
=> true

>> Object classBuilder class
=> ClassBuilder

>> ClassBuilder localMethods
=> [name:, superclass:, fields:, methods:, addField:default:, addMethod:body:, modifier:, register]

// Define a class dynamically — no file, no compiler:
>> dog := Object classBuilder
     name: #Dog;
     addField: #name default: 'Rex';
     addMethod: #speak body: [ self.name , ' says Woof!' ];
     register
=> Dog

>> Dog new speak
=> "Rex says Woof!"

>> Dog superclass
=> Object

>> Dog canUnderstand: #speak
=> true
```

### Error Examples

```beamtalk
>> ClassBuilder new name: #Dog; register   // bypasses factory, no superclass set
=> Error: ClassBuilder register requires superclass to be set

>> Object classBuilder name: nil; register
=> Error: ClassBuilder name: requires a Symbol argument

>> Object classBuilder name: #Counter; register
=> Error: class Counter already exists — send reload: to update a live class
```

## Prior Art

### Pharo / Squeak Smalltalk

`ClassBuilder` is a real Smalltalk class in Pharo. `subclass:instanceVariableNames:...` sends on `Class` ultimately delegate to a `ClassBuilder` instance that constructs the class, wires the metaclass, and registers it. Framework authors can provide custom `ClassBuilder` subclasses to intercept class creation. The IDE works because it operates on the live image — the class hierarchy is always available as live objects.

**What we adopt**: The `ClassBuilder` pattern itself — a dedicated object that encapsulates class creation logic, injectable and inspectable.

**What doesn't translate**: The image-based IDE model. Beamtalk's static LSP layer (ADR 0024) uses the grammar-parsed `ClassHierarchy`, not live object queries. Our grammar stays grammar; the ClassBuilder is the *runtime* integration point, not the *static analysis* integration point.

### Newspeak

Class declarations are grammar that evaluates to class objects. Newspeak uses **exemplars** — live prototype instances — for IDE tooling. The IDE can query an exemplar to learn a class's methods and fields, bridging static syntax and live reflection.

**What we adopt**: The idea that class definitions should produce first-class values. In Beamtalk, `Object classBuilder ... register` returns the new class object, which can be assigned, passed, and inspected.

**What we don't have yet**: Newspeak's exemplars — the reason Newspeak can go fully dynamic without a grammar anchor is that the IDE can query live prototype instances for class structure. We don't have that live tooling layer yet (ADR 0024 Phase 2). Our grammar is a scaffold that substitutes for exemplars until the Live-Augmented layer matures. This is an engineering prioritization for v0.1, not a permanent architectural commitment.

### GNU Smalltalk

File-based like Beamtalk. `subclass:` is a message send; external tools pattern-match source files to build a static class model. This is the closest prior art for the "grammar stays grammar, runtime gets protocol" split — GNU Smalltalk's tools effectively implement a pre-pass over Smalltalk source as a convention.

**What we improve on**: Our grammar *is* grammar — the compiler has authoritative parse-time knowledge. GNU Smalltalk tools have to guess from patterns. We get better static analysis than GNU Smalltalk while preserving the message-send semantics.

### Python

`class Foo(Bar): ...` desugars to a `type()` call (or custom metaclass call). This is exactly the pattern: grammar stays grammar, but it compiles to a runtime protocol invocation. The `__init_subclass__` hook (Python 3.6+) and custom metaclasses allow intercepting class creation without changing syntax.

**What we adopt**: Grammar-to-protocol desugar as a principled architecture, not a hack. Python's `__init_subclass__` hook is spiritually similar to overriding `classBuilder` on a custom root class.

### Elixir

`defmodule` is a compile-time macro. No runtime interception. No dynamic class creation.

**Contrast**: Beamtalk deliberately goes further — ClassBuilder makes class creation inspectable and interceptable at runtime, which Elixir's macro model does not support. The trade-off: Beamtalk requires ClassBuilder to be bootstrapped; Elixir's `defmodule` has no bootstrap concerns.

### TypeScript / Roslyn (C#)

These aren't class creation systems, but they establish the right architectural principle: **keep syntax as syntax, but expose the compilation process as a first-class API**. TypeScript's `class` keyword is grammar; the TypeScript compiler API lets tools and plugins participate in the compilation pipeline. C#'s Roslyn makes the entire compiler a library.

**What we adopt**: The principle that the right answer to "grammar or message?" is "grammar for syntax, protocol for semantics." You don't need to make `class` a runtime call to get runtime flexibility — you need to expose the *protocol* that `class` desugars to.

## User Impact

### Newcomer (from Python/JS/Ruby)

- **Zero visible change**: Syntax is identical. The newcomer writes `Object subclass: Foo` and it works exactly as before.
- **REPL surprise (good)**: Discovering that `Object classBuilder name: #Dog; ... ; register` works interactively — defining a class without writing a file — is a delightful REPL moment. This is only possible with the dynamic path.
- **Discoverable protocol**: `Class respondsTo: #classBuilder` returns `true`. The class creation vocabulary is part of the message system they're already exploring.

### Smalltalk Developer

- **This is what they expected**: `ClassBuilder` exists in Pharo. Beamtalk now has one too. The pattern is immediately familiar.
- **Interceptable creation**: Overriding `classBuilder` on a custom `ProtoObject` root class to return a custom builder is exactly what Pharo framework authors do with `ClassBuilder` subclasses. This now works in Beamtalk.
- **Grammar trade-off acknowledged**: A purist will note that `Object subclass: Counter` is still syntax, not a pure message send. The ADR is honest about this — grammar is the right choice for the static analysis layer. The runtime path is what matters for the Smalltalk object model, and that path is now fully message-based.

### Erlang/BEAM Developer

- **Observable registration protocol**: `dbg:tpl(beamtalk_class_builder, register, ...)` traces every post-bootstrap class registration. The direct `beamtalk_object_class:start/2` path is retained only for bootstrap classes (`ProtoObject` through `Metaclass`), which is the same infrastructure bypass already present for `Behaviour` and `Class`.
- **Dynamic classes are a new capability**: Closures stored in a class gen_server are inspectable via Observer. A new class defined at the REPL prompt appears in `beamtalk_class_registry:all_classes/0` immediately.

### Production Operator

- **Compiled classes semantically equivalent**: Path 1 produces semantically equivalent BEAM bytecode — method functions (`increment/1`, `value/1`, etc.) are unchanged; only the module init function changes to emit the ClassBuilder call instead of a direct `beamtalk_object_class:start/2` call. Observable class behaviour and performance characteristics are unaffected.
- **Dynamic classes are opt-in**: Path 2 is only used when explicitly called. No compiled class degrades to interpreted closures without the operator knowing.
- **Hot reload simplified**: Reloading a module triggers the ClassBuilder call in the module init, which updates the class gen_server state. One code path for initial load and reload.

### Tooling Developer

- **Static analysis unchanged**: `ClassHierarchy::build` reads `ClassDefinition` AST nodes. No pre-pass, no AST restructuring, no changes to LSP indexing.
- **ClassBuilder is the runtime seam**: When the live workspace is running, the LSP can query ClassBuilder-registered classes to augment static results — this is ADR 0024's Live-Augmented path, now with a clean query point.
- **No two-tier problem**: There is one syntax (`Object subclass: Foo`) and one runtime protocol (`ClassBuilder`). Dynamic classes created via ClassBuilder directly are visible to tools via `beamtalk_class_registry`. No stub files or separate declaration formats needed.

## Steelman Analysis

### Alternative A: Keep Grammar as Grammar (Status Quo, No ClassBuilder)

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "The current behaviour is clear and learnable. Adding a ClassBuilder protocol that most users will never touch adds cognitive overhead to the mental model for zero visible benefit." |
| **BEAM veteran** | "Bootstrap ordering is the most fragile part of any OTP application. You're adding a new class (`ClassBuilder`) to a sequence that must complete correctly before any user module loads — and you're doing it to change what generated `on_load` functions call. If `ClassBuilder` isn't wired before a user module loads, you get `undef` at startup, not at test time. The current `beamtalk_object_class:start/2` call is two lines and has no ordering dependency beyond `pg` being started. Every new entry in the bootstrap sequence is a new way to brick the runtime." |
| **Operator** | "If it ain't broke, don't fix it. Compiled class creation works. Don't touch the bootstrap sequence to satisfy a philosophical principle about message sends." |
| **Smalltalk purist** | "Even this doesn't satisfy me fully — `subclass:` is still grammar. You've added ClassBuilder but the entry point is still a parser special form, not a true message." |
| **Language designer** | "Grammar gives the compiler authoritative parse-time knowledge and richer error messages. If ClassBuilder is only ever called by codegen output and not meaningfully by user code, it's gold-plating." |

**Tension point**: The BEAM veteran and operator arguments are the strongest. For v0.1, stability matters more than protocol purity. The counter is that REPL self-hosting makes ClassBuilder a *practical requirement*, not just a philosophical preference — the dynamic path is a real feature need, not gold-plating.

### Alternative B: Pre-Pass (Previous Draft of This ADR)

| Cohort | Strongest argument |
|--------|-------------------|
| **Language designer** | "The pre-pass is the minimum viable change that makes `subclass:` inspectable without requiring new stdlib infrastructure upfront. Ship small, iterate." |
| **BEAM veteran** | "A narrow pre-pass is simpler than a new ClassBuilder class with its own bootstrap sequencing. Less surface area, less to debug." |
| **Pragmatist** | "Option B unblocks ADR 0032/0036 completion immediately. ClassBuilder can come later once the metaclass tower is stable." |

**Why it is superseded by this ADR**: The pre-pass requires replacing the `ClassDefinition` AST node with a `MessageSend` node, updating all AST consumers, and adding pattern-matching logic — all to move complexity from the grammar to a pre-pass that is *still* structural analysis. This is not simpler than ClassBuilder; it just moves the special case. ClassBuilder is the right abstraction: keep the grammar node, change the codegen target. No pre-pass is needed at all.

### Alternative C: Fully Dynamic `subclass:` (No Grammar)

| Cohort | Strongest argument |
|--------|-------------------|
| **Smalltalk purist** | "This is the only honest approach. The grammar form is still a special case in the compiler. A true 'messages all the way down' design would have no grammar production for class definitions at all." |
| **REPL self-hosting** | "If we're going to have a self-hosted REPL anyway, the compiler becomes a library and the 'grammar' form is just a well-known pattern that the compiler library handles. At that point, 'grammar or message?' dissolves as a distinction." |
| **Language designer** | "Newspeak's fully dynamic approach works, and it works *without* grammar because Newspeak uses exemplars — live prototype instances that the IDE can query to learn a class's structure. The reason Beamtalk needs grammar for the static layer is that we don't have exemplars. That's an engineering prioritization for v0.1, not a language design conclusion. Keeping grammar as the static anchor is admitting we haven't built the live tooling layer yet." |

**Why we defer it**: This is an honest prioritization, not a principled objection. ADR 0024's static LSP layer requires the class hierarchy from source without a live workspace — because we haven't built the Live-Augmented layer that would make exemplars viable. Newspeak succeeds without grammar because its IDE operates on live objects; ours doesn't yet. This ADR establishes ClassBuilder as the dynamic protocol that makes Alternative C viable once the Live-Augmented layer (ADR 0024 Phase 2) matures. The grammar stay is a scaffold, not a commitment.

### Tension Points

- BEAM veterans and operators prefer Alternative A; the REPL self-hosting requirement shifts this because dynamic class creation is a real feature, not philosophy.
- Smalltalk purists prefer Alternative C; this ADR is the pragmatic path toward it, not away from it.
- The language designer (Anders-style argument) is satisfied by this ADR: grammar for syntax, protocol for semantics, with the compiler desugaring the grammar form to the protocol. This is the TypeScript/Roslyn pattern applied to class creation.

## Alternatives Considered

### Alternative A: Keep Grammar as Grammar (Status Quo)

Retain hardcoded Erlang registration. No ClassBuilder.

**Rejected because**: Creates no dynamic class creation path, leaving REPL self-hosting as an architectural dead end. The hardcoded registration Erlang is also the last major piece of class infrastructure not visible from Beamtalk — inconsistent with the ADR 0032/0036 direction.

### Alternative B: Pre-Pass Pattern Matching

Add a compiler pre-pass that pattern-matches `X subclass: Y` message sends to extract class hierarchy information. Replace `ClassDefinition` AST node with `MessageSend`. Keep `@intrinsic classDefine` as the runtime hook.

**Rejected because**: The pre-pass adds complexity without meaningful benefit over Alternative D (this ADR). It still requires changing the AST node type, updating all AST consumers, and adding a new compiler phase — all while producing structural analysis that is semantically identical to the existing grammar production. ClassBuilder achieves more (real dynamic creation path, REPL self-hosting foundation) with less change to the compiler (no AST restructuring, no pre-pass).

### Alternative C: Fully Dynamic `subclass:` Without Grammar

Remove the grammar production. `subclass:` is a pure runtime message.

**Rejected because**: Breaks the static LSP layer (ADR 0024 Phase 1) until the Live-Augmented layer matures. Deferred — this ADR is the stepping stone.

### Alternative D: Two Syntaxes

Keep grammar for static, add `defineSubclass:` for dynamic use as a separate selector.

**Rejected because**: Two class creation paths to maintain and document. ClassBuilder unifies both paths under one protocol. Not steelmanned separately because the strongest argument for a second selector is subsumed by Alternative B's "ship small" pragmatism — if you're going to add a runtime API, a full ClassBuilder protocol (this ADR) is strictly more capable than a second selector with no builder pattern.

### Alternative E: Hybrid — Direct Registration for Compiled, ClassBuilder for Dynamic Only

Keep compiled class `on_load` calling `beamtalk_object_class:start/2` directly (current behaviour, unchanged). Add ClassBuilder as a new stdlib class used only for Path 2 (dynamic/REPL class creation). No codegen change.

**Steelman**: This is the minimum change that delivers the REPL self-hosting unblocking goal. It avoids all codegen risk, bootstrap complexity for compiled classes, and the two-path reasoning required to explain why compiled `on_load` calls something different from what users see in REPL sessions. A pragmatic engineer would argue: if ClassBuilder is never called by codegen output, shipping it as compiled-class infrastructure is gold-plating. Ship Path 2 now; revisit unified registration when the REPL self-hosting work validates the design.

**Rejected because**: Under Alternative E, compiled class creation remains an opaque Erlang operation indefinitely. `Object subclass: Counter` in a `.bt` file would still compile to a call that is invisible from Beamtalk — Principle 6 would be satisfied only for the interactive path, not the primary path. This creates a permanent two-tier model: classes created interactively go through the inspectable protocol; classes compiled from source bypass it. That inconsistency tends to calcify — "compiled classes use the old path" reliably stays "compiled classes always use the old path" because the motivation to close the gap disappears once the dynamic path ships. The codegen change (emitting ClassBuilder calls in module init instead of a direct `start/2` call) is contained and low-risk — it is the correct engineering choice to make the architecture consistent end-to-end.

## Consequences

### Positive
- **Principle 6 satisfied**: Class creation is now a Beamtalk-visible protocol, not an opaque Erlang operation
- **REPL self-hosting unblocked**: Path 2 (dynamic closures) enables the REPL to define and use classes interactively without a compiler round-trip
- **Framework interceptability**: Custom root classes can override `classBuilder` to intercept all subclass creation — custom class builders, tracing, validation
- **Single registration protocol for post-bootstrap classes**: `beamtalk_class_builder_register` becomes the canonical registration path for all user-defined classes. Bootstrap classes (`ProtoObject` through `Metaclass`) continue to use direct `beamtalk_object_class:start/2` as they must be wired before `ClassBuilder` itself exists — this is the same infrastructure bypass that `Behaviour` and `Class` already use (ADR 0032)
- **No AST changes**: Grammar stays grammar, ClassDefinition AST node unchanged, LSP and semantic analysis untouched
- **Inspectable**: `Object classBuilder`, `ClassBuilder localMethods`, `Object respondsTo: #classBuilder` all work
- **Clean upgrade path to Alternative C**: Once ADR 0024's Live-Augmented layer is mature, dropping the grammar special case in favour of full dynamism is an incremental change on top of this ADR

### Negative
- **Bootstrap complexity**: ClassBuilder must be pre-wired in the bootstrap sequence before any user classes load — the same challenge as Behaviour and Class (ADR 0032), already solved
- **Process-per-builder at startup**: Each class definition spawns a short-lived ClassBuilder gen_server during module `on_load`. For a project with N classes, that is N process spawns + N `gen_server:stop` calls at startup. On BEAM, process spawn is ~3µs — a 100-class project adds ~0.3ms. Negligible, but measurable in profiling
- **Dynamic class performance**: Path 2 classes use closure dispatch rather than compiled BEAM functions. Suitable for interactive prototyping; not suitable for performance-critical production classes without compilation
- **Two kinds of classes in the registry**: Compiled classes (with BEAM modules) and dynamic classes (closures only) coexist in `beamtalk_class_registry`. Tooling must handle both. Serialisation/persistence of dynamic classes is not defined in this ADR
- **Sealed class check at runtime**: The compiler currently enforces sealed classes at parse/analysis time. The `classBuilder` factory unconditionally returns a builder. For the dynamic path, `register` must check whether the superclass is sealed and reject the registration — this moves a compile-time check to runtime for Path 2
- **on_load ordering constraint**: The ClassBuilder call chain from generated module `on_load` functions requires `bt@class` and `ClassBuilder` to be loaded before any user module. This is guaranteed by the topo-sort bootstrap ordering — the same constraint that ensures superclasses are available already. No new failure mode is introduced, but the constraint is now implicit in ClassBuilder's position in the bootstrap sequence rather than explicit in a direct `start/2` call

### Neutral
- **User syntax is unchanged**: `Object subclass: Counter` works identically before and after
- **Codegen emission changes**: The generated Core Erlang for class module init changes (ClassBuilder cascade instead of direct registry call), but the BEAM modules are semantically equivalent
- **Dynamic classes are transient**: Dynamic classes defined at the REPL do not persist across restarts. This is expected behaviour, consistent with how process state works in Beamtalk

## Implementation

### Phase 1: ClassBuilder Bootstrap and Codegen Change

**Affected components:** `stdlib/src/ClassBuilder.bt`, `stdlib/src/Class.bt`, `beamtalk-core` (codegen), `beamtalk_runtime` (class gen_server, registry)

1. Implement `beamtalk_class_builder.erl` — the Erlang backing for `@intrinsic classBuilderRegister`. Handles both Path 1 (compiled BEAM module, function references) and Path 2 (dynamic, closures). Delegates to `beamtalk_object_class:start/2` for first registration; detects `{error, {already_started, _}}` and falls back to `beamtalk_object_class:update_class/2` for hot reload. After registration, stops the builder's gen_server process (`gen_server:stop/1`). Returns the newly created class object. Validate with a single hand-wired test class before proceeding.
2. Update bootstrap in `beamtalk_bootstrap.erl`: pre-wire ClassBuilder as an Actor subclass before user classes load. Bootstrap sequence: `ProtoObject → Object → Behaviour → Class → Metaclass → ClassBuilder → Actor`.
3. Add `Class.bt` method: `classBuilder => ClassBuilder new superclass: self`
4. Create `stdlib/src/ClassBuilder.bt` with cascade-based builder API. ClassBuilder is an Actor — no BT-833 dependency needed. Alternatively, hand-wire as `beamtalk_class_builder_bt.erl` for v0.1 (consistent with `beamtalk_class_bt.erl` and `beamtalk_metaclass_bt.erl`).
5. Update codegen (`crates/beamtalk-core/src/codegen/core_erlang/`) to emit ClassBuilder cascade in generated module init instead of direct `beamtalk_object_class:start/2` calls
6. Add post-bootstrap assertion: `Class respondsTo: #classBuilder` must return `true`
7. Run full test suite — all existing class definition behaviour must be preserved

### Phase 2: Dynamic Class Creation (Path 2)

**Affected components:** `beamtalk_class_builder.erl`, `beamtalk_object_class.erl`, `stdlib/src/ClassBuilder.bt`

1. Implement the closure-based class gen_server state: when methods are provided as blocks (not function references), the class gen_server stores and invokes them via closure dispatch
2. Implement dynamic dispatch mechanism: define how message sends to dynamic class instances resolve to closures in the class gen_server state when no BEAM module with `dispatch/3` exists
3. Verify dynamic class creation in REPL tests — `ClassBuilder >> addMethod:body:` is already defined in the Decision section; this phase implements the runtime mechanism that makes it work
4. Verify dynamic classes are visible in Observer and via `Class allSubclasses` (uses existing `beamtalk_class_registry:all_classes/0`)

### Phase 3: REPL Integration

**Affected components:** REPL (existing Erlang REPL or future Beamtalk REPL)

1. Wire the REPL evaluator to call Path 2 when evaluating a class definition expression in a live workspace
2. Verify the interactive experience: define a class, use it, redefine it, verify the new definition takes effect
3. Add e2e tests covering dynamic class definition and use at the REPL

### Future Work (Not This ADR)

- **REPL self-hosting**: Port the REPL to pure Beamtalk using Path 2 for dynamic classes and the compiler port (ADR 0022) for compiled classes
- **ClassBuilder persistence**: Define how dynamic classes can be serialised (e.g., to `.bt` source) for persistence across restarts
- **Alternative C (fully dynamic)**: Once ADR 0024's Live-Augmented layer matures, the grammar special case can be dropped and `Object subclass: Counter` in source becomes a genuine message send that the compiler happens to also pattern-match for static analysis

## References
- Related ADRs: [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md) (Object Model), [ADR 0006](0006-unified-method-dispatch.md) (Unified Dispatch), [ADR 0013](0013-class-variables-class-methods-instantiation.md) (Instantiation Protocol), [ADR 0022](0022-embedded-compiler-via-otp-port.md) (Compiler Port), [ADR 0024](0024-static-first-live-augmented-ide-tooling.md) (Static-First Tooling), [ADR 0032](0032-early-class-protocol.md) (Early Class Protocol), [ADR 0034](0034-stdlib-self-hosting-in-beamtalk.md) (Stdlib Self-Hosting), [ADR 0036](0036-full-metaclass-tower.md) (Full Metaclass Tower)
- Principle: [Principle 6 — Messages All The Way Down](../beamtalk-principles.md)
- Prior art: [Pharo ClassBuilder](https://github.com/pharo-project/pharo/blob/Pharo12/src/Kernel-CodeModel/ClassBuilder.class.st), [Pharo Reflective Core Booklet](https://books.pharo.org/booklet-ReflectiveCore/), [Newspeak Specification](https://newspeaklanguage.org/spec/newspeak-spec.pdf), [GNU Smalltalk: Creating Classes](https://www.gnu.org/software/smalltalk/manual/html_node/Creating-classes.html)
