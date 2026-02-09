# ADR 0013: Class Variables, Class-Side Methods, and Instantiation Protocol

## Status
Proposed (2026-02-09)

## Context

ADR 0005 committed to "full Smalltalk metaclass model as the target" with classes as first-class objects backed by gen_server processes. Phase 1 (implemented) provides a fixed protocol (`methods`, `superclass`, `new`/`spawn`) via `beamtalk_object_class.erl`. Phase 2 requires extending this to support class variables, class-side methods, and the `new`/`initialize` instantiation chain.

### What works today

- **Class processes**: Every class has a gen_server process holding a `#class_state{}` record (methods, superclass, fields, etc.)
- **Instance creation**: `Point new` compiles to `call 'point':'new'()` (direct module call). `Counter spawn` creates a gen_server process.
- **Workspace bindings**: `Transcript` and `Beamtalk` dispatch through `persistent_term` to singleton actors (ADR 0010).
- **Reflection**: `Beamtalk classNamed: #Counter` returns a `#beamtalk_object{}` wrapping the class pid.

### What doesn't work

1. **No class variables**: Classes can't hold shared state (e.g., `UniqueInstance` for singletons). The `#class_state{}` record has no slot for them.
2. **No class-side methods**: No syntax to define methods on the class (vs. on instances). All methods in a `.bt` file are instance methods.
3. **No dynamic class dispatch**: `cls := Point. cls new` fails — the compiler only generates direct module calls for `ClassReference` AST nodes, not for variables holding class objects.
4. **No `initialize` hook**: `new` returns a map with defaults; there's no post-creation initialization protocol.

### Motivating use cases

**REPL introspection** (interactive-first language):
```beamtalk
cls := Beamtalk classNamed: #Point
cls new: #{x => 3, y => 4}
cls methods
```

**Factory pattern** (classic OO):
```beamtalk
Object subclass: ShapeFactory
  state: shapeClass = nil
  create => self.shapeClass new
```

**Singleton pattern** (Transcript, SystemDictionary):
```beamtalk
Object subclass: TranscriptStream
  classVar: uniqueInstance = nil
  
  class uniqueInstance =>
    self.uniqueInstance ifNil: [self.uniqueInstance := self basicNew initialize].
    self.uniqueInstance
```

## Decision

### 1. Class Variables

**Syntax**: `classVar:` declarations alongside `state:` declarations.

```beamtalk
Object subclass: TranscriptStream
  state: buffer = #()
  classVar: uniqueInstance = nil
  classVar: maxBuffer = 1000
```

**Semantics**:
- Class variables are **shared state on the class process** (not on instances).
- Stored in the class gen_server state (`#class_state{class_vars :: map()}`).
- Accessible from both class-side and instance-side methods via `self.varName` on the class side, and a yet-to-be-determined syntax on the instance side.
- Mutable via `:=` assignment (same as instance state).
- **Not inherited** by subclasses — each class has its own class variables (class instance variables in Pharo terminology). This matches Pharo's class instance variables, which is what singletons actually need.

**Storage**: In the class gen_server process state. Access compiles to `gen_server:call(ClassPid, {get_class_var, VarName})` / `gen_server:call(ClassPid, {set_class_var, VarName, Value})`.

### 2. Class-Side Methods

**Syntax**: `class` prefix before method definition.

```beamtalk
Object subclass: TranscriptStream
  state: buffer = #()
  classVar: uniqueInstance = nil

  // Instance methods (no prefix)
  show: text => self.buffer := self.buffer ++ #(text)
  
  // Class-side methods (class prefix)
  class uniqueInstance =>
    self.uniqueInstance ifNil: [self.uniqueInstance := self basicNew initialize].
    self.uniqueInstance

  class new => self error: 'Use uniqueInstance instead'
```

**Semantics**:
- `class` prefix declares a method on the class object, not on instances.
- Inside class-side methods, `self` refers to the class object (the gen_server process).
- `self.uniqueInstance` accesses a class variable.
- Class-side methods are stored in the class process and dispatched via `gen_server:call`.
- Class-side `new` can be overridden (e.g., to prevent direct instantiation).

### 3. Instantiation Protocol: new → basicNew → initialize

**Chain**:
```
ClassName new        →  ClassName basicNew initialize
ClassName new: args  →  ClassName basicNew initialize: args
```

- **`basicNew`**: Allocates the object (creates map with default field values for value types, or starts gen_server for actors). This is the primitive — users rarely override it.
- **`initialize`**: Post-creation hook. Default implementation returns `self`. Users override to set up invariants.
- **`new`**: The public API. Default implementation calls `basicNew initialize`. Can be overridden for singletons, factories, or to raise errors.

**For value types** (Object subclasses):
```beamtalk
Point new            // → Point basicNew initialize → #{$beamtalk_class => 'Point', x => 0, y => 0}
Point new: #{x => 3} // → Point basicNew initialize: #{x => 3} → #{$beamtalk_class => 'Point', x => 3, y => 0}
```

**For actors** (Actor subclasses):
```beamtalk
Counter spawn        // Unchanged — actors use spawn, not new
Counter new          // → Error: "Use spawn instead of new for actors"
```

The actor `new` error is already implemented (codegen generates `beamtalk_error` with hint).

### 4. Dynamic Class Dispatch

When the receiver is not a compile-time `ClassReference`, dispatch goes through the runtime:

```beamtalk
cls := Beamtalk classNamed: #Point   // cls is a #beamtalk_object{class='Class', pid=ClassPid}
cls new                               // → gen_server:call(ClassPid, {new, []})
cls methods                           // → gen_server:call(ClassPid, methods)
```

**Two paths** (ADR 0005 principle: "runtime dispatch is the contract, direct calls are the optimization"):

1. **Optimized path** (compile-time known class): `Point new` → `call 'point':'new'()` (direct module call, zero overhead). This is what works today.
2. **Dynamic path** (runtime class object): `cls new` → `gen_server:call(ClassPid, {new, Args})`. The class process handles the message and calls the module function internally.

The class process (`beamtalk_object_class.erl`) already handles `new` in `handle_call` — the gap is only in **codegen** not generating the dynamic dispatch path.

## Prior Art

### Smalltalk-80 / Pharo

**Class variables** (`classVariableNames:`) are shared across the entire hierarchy. **Class instance variables** are per-class (stored on the metaclass). Pharo singletons use class instance variables.

**Class-side methods** are defined on the metaclass. Every class `Foo` has a metaclass `Foo class`. Method lookup on the class side walks `Foo class → FooSuper class → ... → Class → Behavior → Object`.

**Instantiation**: `new → basicNew → initialize` chain. `basicNew` is a primitive that allocates memory. `initialize` is a hook.

**What we adopt**: The `new → basicNew → initialize` chain, class-side method concept, and class instance variables (not shared class variables).

**What we adapt**: No metaclass tower. Class-side methods live on the class gen_server process directly. Simpler model that works well on BEAM.

### Erlang/OTP

No class system. Module attributes are compile-time constants. Per-module mutable state uses `persistent_term`, ETS, or process state.

**What we adopt**: Using gen_server process state for class variables aligns perfectly with OTP patterns.

### Newspeak

Class-side state via "class declarations" in the class header. Modules are instantiable — class definitions are essentially factories.

**What we note**: Newspeak's approach of classes-as-modules-as-factories is elegant but more radical than we need. Our gen_server approach is simpler.

### Ruby

`class << self` or `self.method_name` for class-side methods. Class variables (`@@var`) are shared across hierarchy (widely considered a design mistake). Class instance variables (`@var` on the class) are per-class.

**What we reject**: Ruby's `@@var` sharing across hierarchy. We follow Pharo's class instance variable model (per-class).

## User Impact

### Newcomer
- `class` prefix for class methods is intuitive — reads naturally.
- `new`/`initialize` pattern is familiar from most OO languages.
- `classVar:` is explicit about what it does.

### Smalltalk Developer
- Will expect `classVariableNames:` shared across hierarchy — we use per-class variables instead (Pharo class instance variable semantics). This is actually what they want for singletons.
- `class uniqueInstance` instead of defining on `TranscriptStream class` — minor syntax difference.
- `new → basicNew → initialize` chain is identical.

### Erlang/BEAM Developer
- Class variables stored in gen_server state is natural OTP.
- `gen_server:call` for class-side dispatch is standard.
- No new runtime concepts — just wiring existing patterns.

### Operator
- Class variable state lives in the class process — observable via `observer`, restartable via supervision.
- Singleton actors are just named processes — standard OTP monitoring.

## Steelman Analysis

### "Just use workspace bindings for singletons (no class variables needed)"
**For**: ADR 0010 already solves Transcript/Beamtalk. Why add complexity?
**Against**: Workspace bindings are a deployment concern, not a language feature. Users can't create their own singletons. Factory pattern requires class variables. This limits the language to only framework-provided singletons.

### "Use Erlang persistent_term for class variables instead of gen_server state"
**For**: ~13ns reads, lock-free, already used for workspace bindings.
**Against**: `persistent_term:put` triggers a global GC scan on every write. Fine for rarely-changing bindings, terrible for mutable class state. gen_server state is the standard OTP pattern for mutable state.

### "Full metaclass tower (Smalltalk-80 style)"
**For**: Maximum compatibility with Smalltalk patterns. Class-side method inheritance works automatically.
**Against**: Enormous complexity for BEAM. Every class needs a metaclass process. Method lookup doubles. Most users never need metaclass inheritance. We can add it later (ADR 0005 Phase 2) if needed.

## Alternatives Considered

### Alternative: `shared:` instead of `classVar:`
```beamtalk
Object subclass: Counter
  shared: instanceCount = 0
```
Rejected: "shared" is ambiguous (shared with whom?). `classVar:` is explicit about scope and familiar to Smalltalk developers.

### Alternative: Annotation-based class methods
```beamtalk
@classMethod
uniqueInstance => ...
```
Rejected: Annotations are not Smalltalk-idiomatic. The `class` prefix reads more naturally and keeps the method definition syntax consistent.

### Alternative: Separate class body block
```beamtalk
Object subclass: TranscriptStream
  // instance side
  show: text => ...
  
  classSide:
    uniqueInstance => ...
```
Rejected: Adds structural complexity. Pharo's browser has two "tabs" (instance/class side), but in a text file the `class` prefix per method is clearer and avoids ordering ambiguity.

## Consequences

### Positive
- Enables singleton pattern, factory pattern, and REPL class introspection.
- `new → basicNew → initialize` gives users control over object construction.
- Dynamic class dispatch makes classes true first-class objects.
- Builds on existing gen_server infrastructure — minimal new runtime concepts.
- `classVar:` is simple to parse (same pattern as `state:`).

### Negative
- Class variable access from instance methods needs to call the class process (`gen_server:call`), adding ~1μs latency per access. This is acceptable for the patterns class variables serve (configuration, caching, singletons) but not for hot-path per-message state.
- Two kinds of `self` (class-side vs instance-side) could confuse newcomers, though the `class` prefix makes context clear.
- Parser, AST, codegen, and runtime all need changes — medium-sized cross-cutting feature.

### Neutral
- Does not introduce metaclass hierarchy (deferred to ADR 0005 Phase 2).
- Compatible with future metaclass addition — class-side methods would move from class gen_server to metaclass process.
- Does not change actor instantiation (`spawn` stays unchanged).

## Implementation

### Phase 1: `new` for Value Types + Dynamic Dispatch (BT-221 + BT-246)
**Parser**: No changes needed — `Point new` already parses.
**Codegen**: When receiver is not a `ClassReference`, emit `gen_server:call(ClassPid, {Selector, Args})` instead of `beamtalk_primitive:send`. The class process already handles `new` in `handle_call`.
**Runtime**: `beamtalk_object_class` already implements `new/1`, `new/2`, `methods/1`, `superclass/1`.
**Test**: `cls := Beamtalk classNamed: #Point. cls new` works end-to-end.

### Phase 2: Class-Side Methods + `initialize` (BT-246 continued)
**Parser**: Add `class` prefix token to method definitions. Store separately in AST (`class_methods: Vec<MethodDefinition>`).
**Codegen**: Generate class-side methods as `handle_call` handlers in the class module (or register them via `beamtalk_object_class:put_method/3`).
**Runtime**: Route class-side message sends to the class process via `gen_server:call`.
**Test**: `TranscriptStream uniqueInstance` returns singleton.

### Phase 3: Class Variables
**Parser**: Add `classVar:` declaration (same as `state:` but different AST node).
**AST**: Add `class_variables: Vec<StateDeclaration>` to `ClassDefinition`.
**Codegen**: Initialize class variables in the class gen_server `init/1`. Generate `gen_server:call` for class variable access from class-side methods.
**Runtime**: Add `class_vars` field to `#class_state{}`. Handle `{get_class_var, Name}` and `{set_class_var, Name, Value}` in `handle_call`.
**Test**: Singleton pattern with class variable storage.

### Phase 4: Instance-Side Access to Class Variables
**Design decision**: How instances read class variables. Options:
- `self class varName` — explicit, verbose
- `ClassName varName` — requires compile-time class knowledge  
- Implicit lookup — Smalltalk-style, searches class then instance variables

Deferred to implementation — pick simplest option that works.

## Migration Path

No breaking changes. All existing code continues to work:
- `Point new` still compiles to direct module call (optimized path).
- `Counter spawn` unchanged.
- New syntax (`classVar:`, `class` prefix) is additive.

## References
- Related issues: BT-246 (first-class class objects), BT-221 (universal new), BT-234 (metaclass hierarchy)
- Related ADRs: ADR 0005 (BEAM object model), ADR 0010 (global objects and singleton dispatch)
- Prior art: Pharo by Example Ch. 6 (Instance Side and Class Side), Pharo singleton MOOC slides
- Documentation: `docs/beamtalk-language-features.md` (class definition section)
