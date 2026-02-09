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
- **Class-side methods are inherited** through the superclass chain, just like instance methods. If `Object` defines `class new`, all subclasses inherit it. A subclass can override with its own `class new`.

**Inheritance example**:
```beamtalk
Object subclass: TestCase
  class allTestSelectors =>
    self methods select: [:m | m startsWith: 'test']

Object subclass: MyTest
  // Inherits allTestSelectors from TestCase — no need to redefine
  testAddition => self assert: (1 + 1) equals: 2
```

**Implementation**: The existing `build_flattened_methods` machinery in `beamtalk_object_class.erl` already walks the superclass chain to pre-compute inherited method tables (O(1) lookup at dispatch time, with cascading invalidation when a parent class changes). Class-side methods reuse this same infrastructure — a `flattened_class_methods` table alongside the existing `flattened_methods` table. No metaclass processes needed; the class gen_server handles both instance-side and class-side dispatch.

### 3. Instantiation Protocol

Value types and actors have **different instantiation semantics**, reflecting the fundamental difference between immutable values and stateful processes.

**Value types** (Object subclasses) — construction, not initialization:
```beamtalk
Point new              // → #{$beamtalk_class => 'Point', x => 0, y => 0}
Point new: #{x => 3}   // → #{$beamtalk_class => 'Point', x => 3, y => 0}
```
Value types are immutable maps. `new` creates a map with default field values. `new:` merges provided arguments with defaults. There is no `initialize` hook — you can't mutate an immutable value after creation. This is the honest design: value types are **constructed**, not initialized.

**Actors** (Actor subclasses) — initialization via gen_server:
```beamtalk
Counter spawn              // → starts gen_server, returns #beamtalk_object{}
Counter spawnWith: #{n => 5} // → starts gen_server with initial state
Counter new                // → Error: "Use spawn instead of new for actors"
```

Actors use `spawn`/`spawnWith:` (already implemented). The gen_server `init/1` callback serves as the initialization hook. The `new` error is already implemented (codegen generates `beamtalk_error` with hint).

**Overriding `new` on the class side** (for singletons, factories):
```beamtalk
Object subclass: TranscriptStream
  classVar: uniqueInstance = nil
  
  class new => self error: 'Use uniqueInstance instead'
  class uniqueInstance =>
    self.uniqueInstance ifNil: [self.uniqueInstance := super new].
    self.uniqueInstance
```

Class-side `new` can be overridden via the `class` prefix. `super new` calls the default constructor. This enables singletons, object pools, and factory patterns without an `initialize` hook.

**Why no `initialize` for value types?**

In Smalltalk, `new → basicNew → initialize` works because `initialize` mutates the freshly allocated object. Beamtalk value types are immutable maps — `self.x := 5` inside `initialize` would be meaningless (or would require `initialize` to use functional-update semantics that differ from every other method). Rather than create a confusing special case, we accept that value types and actors have different construction models — just as they already have different instantiation syntax (`new` vs `spawn`).

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

**Future optimization**: The dynamic path can be made ~10x faster for methods that don't access class variables. The `#beamtalk_object{}` record already carries the module name — extract it and use `apply(Module, Selector, Args)` (~0.5μs) instead of routing through the class gen_server (~5-10μs). The gen_server path is only needed when the method accesses class variable state. This is a codegen optimization pass that requires no language-level changes.

The class process (`beamtalk_object_class.erl`) already handles `new` in `handle_call` — the gap is only in **codegen** not generating the dynamic dispatch path.

### 5. `super` in Class-Side Methods

Class-side `super` walks the **class-side** inheritance chain, not the instance-side chain:

```beamtalk
Object subclass: TranscriptStream
  classVar: uniqueInstance = nil

  class new => self error: 'Use uniqueInstance instead'
  class uniqueInstance =>
    self.uniqueInstance ifNil: [self.uniqueInstance := super new].  // calls Object's class-side new
    self.uniqueInstance
```

`super new` in a class-side method dispatches to the parent class's `flattened_class_methods` table — the same mechanism as instance-side `super`, but using the class-side method table. The codegen generates `gen_server:call(ClassPid, {super_class_send, Selector, Args, DefiningClass})`, and the class process looks up the method in the parent's class-side table.

### 6. Virtual Metaclasses

Smalltalk developers expect `Point class` to return a metaclass object that responds to messages. We achieve full Smalltalk metaclass API compatibility with **zero extra processes** using virtual metaclasses.

**The trick**: The `#beamtalk_object{}` record is `{beamtalk_object, Class, ClassMod, Pid}`. The same class pid can be wrapped with different `Class` names to distinguish instance-side vs class-side dispatch:

```beamtalk
Point                   // → {beamtalk_object, 'Class', point, ClassPid}   (class reference)
Point class             // → {beamtalk_object, 'Point class', point, ClassPid}  (metaclass reference — SAME pid)
```

When the class process receives a message, it checks the `Class` field in the `#beamtalk_object{}`:
- **`'Class'` or class name**: dispatch through `flattened_class_methods` (class-side protocol — `new`, `uniqueInstance`, user-defined class methods)
- **`'X class'` (metaclass)**: dispatch through metaclass protocol (`methods` returns class-side selectors, `superclass` returns parent metaclass, `class` returns `'Metaclass'`)

**Full Smalltalk API**:
```beamtalk
Point class                    // → {beamtalk_object, 'Point class', point, ClassPid}
Point class methods            // → class-side method selectors
Point class superclass         // → 'Object class' (metaclass of parent)
Point class class              // → 'Metaclass'
Point class allTestSelectors   // → inherited class-side method works
```

**What this gives us**:
- `x class` returns a real object, not an atom — you can send messages to it
- Class-side method lookup via `flattened_class_methods` with inheritance
- `superclass` on the metaclass mirrors the instance-side hierarchy
- Compatible with Smalltalk reflection idioms

**What this does NOT give us** (deliberate simplification):
- No `Metaclass` instances — `Metaclass` is a fixed class, not user-extensible
- No `Point class class class` infinite tower — terminates at `Metaclass`
- No per-metaclass state (metaclass instance variables) — class variables serve this role

This covers ~95% of Smalltalk metaclass usage. The remaining 5% (custom metaclasses, metaclass mixins) is esoteric and can be added later if needed.

## Prior Art

### Smalltalk-80 / Pharo

**Class variables** (`classVariableNames:`) are shared across the entire hierarchy. **Class instance variables** are per-class (stored on the metaclass). Pharo singletons use class instance variables.

**Class-side methods** are defined on the metaclass. Every class `Foo` has a metaclass `Foo class`. Method lookup on the class side walks `Foo class → FooSuper class → ... → Class → Behavior → Object`.

**Instantiation**: `new → basicNew → initialize` chain. `basicNew` is a primitive that allocates memory. `initialize` is a hook that mutates the fresh object.

**What we adopt**: Class-side method inheritance, class instance variables (not shared class variables), and `new` as the public constructor API.

**What we adapt**: No metaclass *processes* — virtual metaclasses via the same class gen_server process with dual dispatch tables. No `initialize` for value types (they're immutable maps, not mutable heap objects). Metaclass tower terminates at `Metaclass` (no infinite regression).

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

### "Full metaclass tower (Smalltalk-80 style) instead of virtual metaclasses"

**Smalltalk developer**: "Virtual metaclasses are clever but they're a lie. `Point class` doesn't return a real metaclass object — it returns the same pid with a different tag. You can't add metaclass-specific instance variables. You can't define methods on individual metaclasses independently. In Pharo, I can do `Point class addInstVarNamed: 'cache'` to add a class instance variable dynamically. Your virtual metaclasses can't do that. You're at 95% compatibility, but the 5% you're missing is exactly the 5% that metaprogramming frameworks need."

**Response**: This is the strongest remaining argument. The 5% gap (custom metaclass state, per-metaclass method definitions, metaclass mixins) does matter for advanced frameworks. However: (1) The virtual metaclass approach is forward-compatible — if we later need real metaclass processes, the syntax (`class` prefix, `classVar:`) and the API (`Point class methods`, `Point class superclass`) don't change. Only the runtime implementation changes. (2) Zero extra processes means zero extra supervision complexity, zero extra memory, and zero extra failure modes. (3) The 95% we cover handles SUnit, singleton, factory, and REPL introspection — the use cases driving this ADR. We can promote virtual metaclasses to real metaclass processes later if the 5% gap becomes a blocker.

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
- Virtual metaclasses give ~95% Smalltalk metaclass compatibility with zero extra processes.
- Class-side method inheritance enables framework patterns (SUnit, Seaside-style).
- Dynamic class dispatch makes classes true first-class objects.
- Builds on existing gen_server infrastructure — `build_flattened_methods` reused for class-side table.
- `classVar:` is simple to parse (same pattern as `state:`).
- Honest split: value types are constructed (`new`/`new:`), actors are initialized (`spawn`). No pretending immutable maps support mutation hooks.

### Negative
- Class variable access from instance methods needs to call the class process (`gen_server:call`), adding ~1μs latency per access. This is acceptable for the patterns class variables serve (configuration, caching, singletons) but not for hot-path per-message state.
- Two kinds of `self` (class-side vs instance-side) could confuse newcomers, though the `class` prefix makes context clear.
- Parser, AST, codegen, and runtime all need changes — medium-sized cross-cutting feature.
- Value types not having `initialize` is a divergence from Smalltalk that will surprise Smalltalkers expecting `new → basicNew → initialize` everywhere.

### Neutral
- Does not change actor instantiation (`spawn` stays unchanged).
- Virtual metaclasses replace ADR 0005 Phase 2 — no separate metaclass work needed later.
- Value types and actors have different construction models (`new`/`new:` vs `spawn`/`spawnWith:`), matching their existing semantic split.

## Implementation

### Phase 1: `new` for Value Types + Dynamic Dispatch (BT-221 + BT-246)
**Parser**: No changes needed — `Point new` already parses.
**Codegen**: When receiver is not a `ClassReference`, emit `gen_server:call(ClassPid, {Selector, Args})` instead of `beamtalk_primitive:send`. The class process already handles `new` in `handle_call`.
**Runtime**: `beamtalk_object_class` already implements `new/1`, `new/2`, `methods/1`, `superclass/1`.
**Test**: `cls := Beamtalk classNamed: #Point. cls new` works end-to-end.

### Phase 2: Class-Side Methods with Inheritance (BT-246 continued)
**Parser**: Add `class` prefix token to method definitions. Store separately in AST (`class_methods: Vec<MethodDefinition>`).
**AST**: Add `class_methods: Vec<MethodDefinition>` to `ClassDefinition`.
**Codegen**: Generate class-side methods registered via `beamtalk_object_class` during class bootstrap.
**Runtime**: Add `class_methods` and `flattened_class_methods` fields to `#class_state{}`. Reuse `build_flattened_methods` for class-side table. Route class-side message sends to the class process via `gen_server:call`, dispatching through `flattened_class_methods`.
**Test**: `TranscriptStream uniqueInstance` returns singleton. `MyTest allTestSelectors` inherits from `TestCase`.

### Phase 3: Virtual Metaclasses
**Codegen**: Change `class` intrinsic to return `#beamtalk_object{class='X class', pid=ClassPid}` instead of an atom.
**Runtime**: Class process checks `Class` field to distinguish class-side vs metaclass-side dispatch. Metaclass `superclass` returns parent's metaclass name. Metaclass `class` returns `'Metaclass'`.
**Bootstrap**: Register `Metaclass` class in bootstrap (~50 lines).
**Test**: `Point class methods` returns class-side selectors. `Point class superclass` returns `'Object class'`. `Point class class` returns `'Metaclass'`.

### Phase 4: Class Variables
**Parser**: Add `classVar:` declaration (same as `state:` but different AST node).
**AST**: Add `class_variables: Vec<StateDeclaration>` to `ClassDefinition`.
**Codegen**: Initialize class variables in the class gen_server `init/1`. Generate `gen_server:call` for class variable access from class-side methods.
**Runtime**: Add `class_vars` field to `#class_state{}`. Handle `{get_class_var, Name}` and `{set_class_var, Name, Value}` in `handle_call`.
**Test**: Singleton pattern with class variable storage.

### Phase 5: Instance-Side Access to Class Variables

**Recommended approach**: `self class varName` — explicit message send to the class object.

```beamtalk
Object subclass: Counter
  classVar: instanceCount = 0
  state: value = 0
  
  class new =>
    self.instanceCount := self.instanceCount + 1.
    super new
  
  getInstanceCount => self class instanceCount   // explicit class var access
```

**Why this approach**: (1) No new syntax — reuses existing `self class` message + chained send. (2) Makes the cost visible — it's clearly a message send, not a field access. (3) No variable shadowing — instance vars and class vars have distinct access patterns. (4) Works polymorphically — `self class` resolves to the actual runtime class.

**Performance note**: `self class instanceCount` involves two dispatches (get class, then get var). For hot paths, cache the value in a local: `count := self class instanceCount`. In practice, instance-side class var access is rare — most class var usage is in class-side methods where `self.varName` works directly.

## Parser Note: `class` Keyword Disambiguation

The `class` token appears in two contexts:
1. **Method definition prefix**: `class uniqueInstance => ...` (at declaration level)
2. **Unary message**: `self class` or `x class` (in expressions)

The parser distinguishes these by context: at the class body's declaration level, `class` followed by an identifier and `=>` is a class-side method definition. Inside an expression (method body, block, REPL), `class` is a unary message. 

**Edge cases to handle**:
- `class class => ...` — a class-side method named `class`. Legal but discouraged; parser matches `class <ident> =>` pattern.
- `class => ...` — instance method named `class` (no prefix, overrides the intrinsic). The parser sees `<ident> =>` without `class` prefix.

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
