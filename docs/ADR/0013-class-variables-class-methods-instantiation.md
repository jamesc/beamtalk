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

### "Skip class variables — just spawn dedicated processes for shared state"

**Erlang/BEAM developer**: "The whole point of BEAM is that state belongs in processes, not language-level class constructs. If you want a singleton counter, spawn a `counter_registry` gen_server. If you want shared config, use `persistent_term` or `application:get_env`. Class variables are an OO concept that fights against BEAM's process-oriented model. Every BEAM developer already knows how to manage state in processes — adding a new abstraction (class variables) is one more thing to learn with no real payoff."

**Newcomer**: "I don't understand what 'class variable' means vs 'state'. Why are there two kinds of state? In Python I'd just use a module-level variable."

**Response**: Class variables aren't about raw state management — they're about **object protocol**. The singleton pattern (`uniqueInstance`), the factory pattern (class-side `new:` override), and framework hooks (`allSubclasses`) are standard OO idioms that users from any OO language will reach for. Telling them "spawn a process instead" breaks the object metaphor that Beamtalk promises. Under the hood, class variables *are* process state (gen_server state) — we're not fighting BEAM, we're wrapping it in the right abstraction.

### "Use ETS or persistent_term for class variable storage"

**Erlang/BEAM developer**: "ETS gives you concurrent reads (~100ns), atomic writes, no gen_server bottleneck. `persistent_term` gives ~13ns reads for rarely-changing values. gen_server:call adds ~5-10μs per access and serializes all reads. For a language that compiles to BEAM, you should use BEAM's strengths, not add a gen_server bottleneck."

**Operator**: "I can inspect ETS tables in observer. gen_server state requires attaching to the process. ETS is more observable in production."

**Response**: The performance argument is real but misplaced for class variables. Class variables serve patterns like singletons (read once, cache), instance counting (low frequency), and configuration (rarely changes) — not hot-path per-message state. The gen_server approach gives us: (1) crash recovery via supervision (ETS tables die with their owner), (2) consistent mutation semantics (no race conditions), (3) identical model to actor state (less to learn). If profiling reveals a bottleneck, we can optimize specific patterns to ETS/persistent_term without changing the language semantics — the storage is an implementation detail hidden behind `classVar:`.

### "Full metaclass tower (Smalltalk-80 style) instead of flat class-side methods"

**Smalltalk developer**: "The metaclass hierarchy is what makes Smalltalk's class system powerful. `TestCase class >> allTestSelectors` is inherited by `MyTestCase class` automatically. Without metaclass inheritance, every framework that needs class-side hooks (SUnit, Seaside, Magritte) must reinvent method discovery. You're building half a class system. Also, in Pharo, class instance variables ARE metaclass instance variables — your `classVar:` is actually metaclass state, you're just not calling it that."

**Response**: This is the strongest argument against our approach. Metaclass inheritance *is* genuinely useful for frameworks. However: (1) We can add metaclass inheritance later (ADR 0005 Phase 2) without breaking `classVar:` or `class` method syntax — the syntax works with either a flat model or a metaclass tower. (2) The metaclass tower doubles the number of processes and complicates method lookup for a feature most user code never exercises. (3) Beamtalk's primary audience is building BEAM applications, not building Smalltalk frameworks — we optimize for the common case. (4) `perform:` + `methods` already enables framework-style method discovery without metaclass inheritance.

### "`classVar:` is misleading — it's NOT Smalltalk classVariableNames:"

**Smalltalk developer**: "In Smalltalk, `classVariableNames:` declares variables shared across the entire class hierarchy. Your `classVar:` is actually Pharo's 'class instance variable' — per-class, not shared. Using the name `classVar:` will confuse every Smalltalk developer who expects hierarchy-wide sharing. You should call it `classInstVar:` or similar to be honest about what it does."

**Newcomer**: "I assumed `classVar:` would be inherited by subclasses, like class fields in Java. The per-class behavior is surprising."

**Response**: Fair naming concern. However: (1) Hierarchy-shared class variables are widely considered a design mistake — even Ruby's `@@var` is a known footgun, and Pharo documentation recommends class instance variables for nearly all use cases. (2) `classInstVar:` is jargon that only makes sense if you already understand the metaclass model. (3) The name `classVar:` communicates the right mental model for 90% of users: "a variable that belongs to the class, not to instances." We document the per-class semantics explicitly and note the Smalltalk divergence in the syntax rationale.

### "Class-side methods aren't needed — just use module functions"

**Erlang/BEAM developer**: "Every Beamtalk class already compiles to an Erlang module. Module functions *are* class-side methods. `Point new` already compiles to `call 'point':'new'()`. Why add `class` prefix syntax when you can just define module-level functions? The BEAM already has this concept — it's called a module."

**Response**: Module functions work for the compile-time-known case (`Point new`), but fail for the dynamic case (`cls new` where cls is a variable). The `class` prefix isn't about module functions — it's about methods on the class *object* that participate in message dispatch. When you write `cls := Beamtalk classNamed: #Point. cls new`, the runtime must dispatch `new` to the class process, not to a module. The `class` prefix tells the compiler "register this method on the class process's dispatch table" rather than "generate a module function." Both paths can coexist: direct module calls as the optimization, class process dispatch as the contract.

## Alternatives Considered

### Alternative: `shared:` instead of `classVar:`
```beamtalk
Object subclass: Counter
  shared: instanceCount = 0
```
Rejected: "shared" is ambiguous (shared with whom? — other instances? other classes? other processes?). `classVar:` is explicit about scope and familiar to OO developers, even if its exact semantics differ from Smalltalk's `classVariableNames:`.

### Alternative: `classInstVar:` (Pharo-accurate naming)
```beamtalk
Object subclass: TranscriptStream
  classInstVar: uniqueInstance = nil
```
Rejected: Only meaningful to developers who understand the metaclass model. `classVar:` communicates the right intuition ("variable on the class") for 90% of users. The per-class (non-inherited) semantics are documented explicitly.

### Alternative: Annotation-based class methods
```beamtalk
@classMethod
uniqueInstance => ...
```
Rejected: Annotations are not Smalltalk-idiomatic. The `class` prefix reads more naturally ("class method uniqueInstance") and keeps the method definition syntax consistent. Annotations suggest metadata, not behavior.

### Alternative: Separate class body block
```beamtalk
Object subclass: TranscriptStream
  // instance side
  show: text => ...
  
  classSide:
    uniqueInstance => ...
```
Rejected: Adds structural complexity and ordering ambiguity (what if instance methods appear after `classSide:`?). Pharo's browser has two "tabs" (instance/class side), but in a text file the `class` prefix per method is clearer — you can freely interleave instance and class methods grouped by concern.

### Alternative: Defer everything — only implement dynamic dispatch (Phase 1)
Ship BT-221 + BT-246 without `classVar:` or `class` methods. Let users use workspace bindings for singletons and module functions for class-level behavior. Add class variables later if demand appears.

This is a viable incremental approach. The risk is that Phase 1 without Phase 2-3 leaves an awkward gap: users can call `cls new` dynamically but can't define their own class-side methods or singletons. We include all phases in this ADR for design coherence, but Phase 1 can ship independently.

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
