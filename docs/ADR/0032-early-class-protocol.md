# ADR 0032: Early Class Protocol — Behaviour and Class in Beamtalk Stdlib

## Status
Accepted (2026-02-20)

## Context

### Problem Statement

Class hierarchy walking logic is duplicated across six locations in the codebase, each with its own bugs:

| Location | Responsibility | Known Issues |
|---|---|---|
| `beamtalk_class_dispatch.erl` | 12 hardcoded class-side selectors | `superclass` returns bare atom, not class object |
| `beamtalk_dispatch.erl` | Fast+slow dispatch, `responds_to` | BT-721: slow path only checked local methods |
| `beamtalk_class_hierarchy.erl` | Flattened method table builder | BT-510: out-of-order loading yields incomplete tables |
| `beamtalk_object_class.erl` | Class gen_server, `has_method` | `has_method/2` checks local methods only |
| `beamtalk_primitive.erl` | Primitive `responds_to/2` | Does not walk hierarchy at all |
| `class_hierarchy/mod.rs` | Static compile-time hierarchy | Must be manually kept in sync with runtime |

There are no `Behaviour.bt`, `Class.bt`, or `Metaclass.bt` files. All class-side behavior is hardcoded in Erlang with no way to extend or override from Beamtalk. This contradicts Principle 6 (Messages All The Way Down) and Principle 8 (Reflection as Primitive).

Recent bugs illustrate the cost of this duplication:
- **BT-721** (2026-02-19): `respondsTo:` couldn't detect inherited methods because `responds_to_slow` called `has_method/2`, which only checks local methods.
- **BT-510**: Superclass not registered yet during init produces incomplete flattened tables; compensated by async `{rebuild_flattened, ChangedClass}` broadcasts that create a race window.

### Current State

Class objects are `{beamtalk_object, 'Counter class', counter, ClassPid}` tuples (ADR 0013 virtual metaclasses). Messages to class objects route through `beamtalk_class_dispatch:class_send/3`, which pattern-matches on 12 hardcoded selectors before falling through to user-defined class methods.

The class gen_server (`beamtalk_object_class.erl`) holds:
- `instance_methods` / `class_methods` — local method tables
- `flattened_methods` / `flattened_class_methods` — pre-computed inherited tables
- `superclass` — stored as an atom, requiring registry lookup for every hierarchy traversal
- `class_variables` — class-side state

Hierarchy walking is implemented in Erlang across multiple modules with no single authoritative code path. The `Object >> respondsTo:` intrinsic delegates to `beamtalk_dispatch:responds_to/2`, which has its own fast path (flattened table) and slow path (recursive gen_server walks). Neither delegates to a canonical Beamtalk implementation.

The flattened method table — a pre-computed cache of all inherited methods — is itself a major source of complexity. It requires `beamtalk_class_hierarchy.erl` to build and rebuild the tables, an O(N) broadcast cascade (`invalidate_subclass_flattened_tables`) that fires to every class process on any change, and an async rebuild mechanism that creates the race window behind BT-510. The hierarchy is shallow (max depth 6, typical 4), making the cache a premature optimization whose complexity cost exceeds its performance benefit at v0.1.

### Constraints

- **Bootstrap ordering**: ProtoObject, Object, and Actor must be registered before any user classes load (ADR 0006). Behaviour and Class must be part of this bootstrap sequence. Methods that depend on Behaviour (e.g., `Object>>respondsTo:`) are unavailable during the bootstrap window — this is acceptable because no user code runs during bootstrap. A post-bootstrap assertion must verify the class protocol is functional (e.g., `Object canUnderstand: #class` returns `true`).
- **Virtual metaclasses**: ADR 0013's design (same pid, different class tag) must be preserved. This ADR does not introduce real metaclass processes.
- **No premature optimization**: The flattened method table cache (`beamtalk_class_hierarchy.erl`) is removed. Hierarchy depth is shallow (max 6, typical 4) and the cache is the root cause of BT-510's race window and the O(N) rebuild broadcast cascade. Chain walking is correct by construction; caching can be re-introduced later as a targeted optimization (e.g., sealed method promotion, hot call site caching) once profiling data exists.
- **No traits/mixins**: Behaviour is a regular class in the hierarchy, not a mixin or trait.
- **Three codegen paths**: Actor / Value Type / Primitive Type (ADR 0007). Behaviour methods must be accessible from all class objects regardless of their instance-side codegen path.

## Decision

Introduce `Behaviour` and `Class` as real Beamtalk stdlib classes that define the class protocol. Move hierarchy-walking logic from hardcoded Erlang into Beamtalk methods backed by thin primitives for raw data access. Remove the flattened method table cache entirely — dispatch walks the class chain directly, which is correct by construction and eliminates the BT-510 race window.

### Class Hierarchy (Extended)

```
ProtoObject
  └─ Object
       ├─ Behaviour (abstract — class protocol: hierarchy, method dict, instance creation)
       │    └─ Class (sealed concrete — name, subclasses, class variables)
       ├─ Integer, String, Float, Boolean, ... (primitives)
       ├─ Point, Color, ... (value types)
       └─ Actor
            └─ Counter, MyService, ... (user actors)
```

`Class` is sealed — users cannot subclass it at v0.1. All methods on `Behaviour` are sealed — users cannot override the class protocol. This limits the blast radius of the class protocol (reloading `Behaviour.bt` only matters for stdlib maintainers) and prevents subtle dispatch bugs from user overrides. Both restrictions can be relaxed in future releases as the metaclass tower matures.

`Behaviour` and `Class` are regular classes in the instance hierarchy. Every class object is treated as an instance of `Class` for dispatch purposes. When a class-side message is not found in the user-defined class methods, dispatch walks the `Class` → `Behaviour` → `Object` → `ProtoObject` chain — the same mechanism used for any other object. No special cases, no hardcoded selectors.

Concretely, `class_send/3` becomes:

```erlang
class_send(ClassPid, Selector, Args, ClassObject) ->
    %% 1. Check user-defined class methods (local class method table)
    case lookup_class_method(ClassPid, Selector) of
        {ok, Method} -> invoke(Method, Args, ClassObject);
        not_found ->
            %% 2. Dispatch as instance of Class — normal chain walk
            beamtalk_dispatch:dispatch('Class', Selector, Args, ClassObject)
    end.
```

This eliminates all 12 hardcoded selector clauses. Class objects are just objects — dispatch like one.

### Behaviour.bt — The Core Class Protocol

```beamtalk
// lib/Behaviour.bt
/// The abstract superclass of all class-describing objects.
/// Provides method dictionary access, hierarchy queries,
/// and instance creation protocol.
///
/// In Smalltalk terms, Behaviour defines the protocol shared by
/// Class and Metaclass. In Beamtalk, class objects dispatch to
/// Behaviour methods via the class-side fallback chain.
abstract Object subclass: Behaviour

  // --- Hierarchy queries ---

  /// Return the superclass of the receiver, or nil for root classes.
  sealed superclass => @intrinsic classSuperclass

  /// Return all superclasses in order from immediate parent to root.
  ///
  /// ## Examples
  /// ```beamtalk
  /// Counter allSuperclasses   // => [Actor, Object, ProtoObject]
  /// ```
  sealed allSuperclasses =>
    result := List new.
    current := self superclass.
    [current notNil] whileTrue: [
      result add: current.
      current := current superclass
    ].
    result

  /// Return direct subclasses of the receiver.
  sealed subclasses => @intrinsic classSubclasses

  /// Return all subclasses transitively (breadth-first).
  sealed allSubclasses => @intrinsic classAllSubclasses

  /// Test whether the receiver inherits from aClass (strict — self is not included).
  ///
  /// ## Examples
  /// ```beamtalk
  /// Counter inheritsFrom: Actor    // => true
  /// Counter inheritsFrom: Counter  // => false
  /// ```
  sealed inheritsFrom: aClass =>
    current := self superclass.
    [current notNil] whileTrue: [
      current == aClass ifTrue: [^ true].
      current := current superclass
    ].
    false

  /// Test whether aBehaviour is the receiver or one of its ancestors.
  ///
  /// ## Examples
  /// ```beamtalk
  /// Counter includesBehaviour: Counter  // => true
  /// Counter includesBehaviour: Actor    // => true
  /// Counter includesBehaviour: Integer  // => false
  /// ```
  sealed includesBehaviour: aBehaviour =>
    self == aBehaviour or: [self inheritsFrom: aBehaviour]

  // --- Method dictionary ---

  /// Return all selectors understood by instances (includes inherited).
  /// Walks the class chain collecting local methods at each level.
  ///
  /// ## Examples
  /// ```beamtalk
  /// Counter methods   // => [increment, getValue, class, respondsTo:, ...]
  /// ```
  sealed methods =>
    result := Set new.
    current := self.
    [current notNil] whileTrue: [
      result addAll: current localMethods.
      current := current superclass
    ].
    result asList

  /// Return only selectors defined locally in this class (not inherited).
  sealed localMethods => @intrinsic classLocalMethods

  /// Test whether instances of the receiver understand the given selector
  /// (checks full inheritance chain).
  ///
  /// ## Examples
  /// ```beamtalk
  /// Counter canUnderstand: #increment  // => true
  /// Counter canUnderstand: #class      // => true (inherited from ProtoObject)
  /// Counter canUnderstand: #bogus      // => false
  /// ```
  sealed canUnderstand: selector =>
    current := self.
    [current notNil] whileTrue: [
      (current includesSelector: selector) ifTrue: [^ true].
      current := current superclass
    ].
    false

  /// Test whether the selector is defined locally in this class
  /// (does not check superclasses).
  ///
  /// ## Examples
  /// ```beamtalk
  /// Counter includesSelector: #increment  // => true
  /// Counter includesSelector: #class      // => false (defined in ProtoObject)
  /// ```
  sealed includesSelector: selector => @intrinsic classIncludesSelector

  /// Walk the hierarchy and return the class that defines the given selector,
  /// or nil if no class defines it.
  ///
  /// ## Examples
  /// ```beamtalk
  /// Counter whichClassIncludesSelector: #class  // => ProtoObject
  /// ```
  sealed whichClassIncludesSelector: selector =>
    (self includesSelector: selector) ifTrue: [^ self].
    self superclass
      ifNotNil: [:s | s whichClassIncludesSelector: selector]

  // --- Instance variable queries ---

  /// Return the names of instance variables declared in this class (not inherited).
  sealed instanceVariableNames => @intrinsic classInstVarNames

  /// Return all instance variable names including inherited, in slot order.
  sealed allInstanceVariableNames =>
    result := List new.
    current := self.
    [current notNil] whileTrue: [
      result addAll: current instanceVariableNames.
      current := current superclass
    ].
    result

  // --- Identity ---

  /// Test whether this is a Behaviour (class-describing object).
  sealed isBehaviour => true

  /// Test whether this is a metaclass. Returns false; overridden in Metaclass.
  sealed isMeta => false
```

### Class.bt — Concrete Class Identity

```beamtalk
// lib/Class.bt
/// A concrete class in the Beamtalk system. Adds name, class variable
/// access, and class identity protocol on top of Behaviour.
sealed Behaviour subclass: Class

  /// Return the name of the class as a Symbol.
  ///
  /// ## Examples
  /// ```beamtalk
  /// Counter name         // => #Counter
  /// Integer name         // => #Integer
  /// ```
  sealed name => @intrinsic className

  /// Return a human-readable string representation.
  ///
  /// ## Examples
  /// ```beamtalk
  /// Counter printString  // => "Counter"
  /// ```
  sealed printString => self name asString

  /// Test whether this is a Class (not a Metaclass).
  sealed isClass => true

  /// Return the metaclass sentinel.
  /// (Full metaclass tower is future work — see design-metaprogramming.md)
  sealed class => @intrinsic classClass
```

### Updated Object.bt — respondsTo: Delegates to canUnderstand:

```beamtalk
// In lib/Object.bt — replace the intrinsic with a Beamtalk method
sealed respondsTo: selector: Symbol -> Boolean =>
  self class canUnderstand: selector
```

This single line replaces five Erlang code paths. `canUnderstand:` is a pure Beamtalk chain walk on `Behaviour` — no Erlang fast path, no flattened table, no duplication.

### New Instance-Side Methods on Object

```beamtalk
// In lib/Object.bt — hierarchy-aware type testing

/// Test whether the receiver is an instance of aClass or any of its subclasses.
///
/// ## Examples
/// ```beamtalk
/// 42 isKindOf: Integer     // => true
/// 42 isKindOf: Object      // => true
/// 42 isKindOf: String      // => false
/// ```
sealed isKindOf: aClass: Class -> Boolean =>
  aClass includesBehaviour: self class

/// Test whether the receiver is an exact instance of aClass (not a subclass).
///
/// ## Examples
/// ```beamtalk
/// 42 isMemberOf: Integer   // => true
/// 42 isMemberOf: Object    // => false
/// ```
sealed isMemberOf: aClass: Class -> Boolean =>
  self class == aClass
```

### REPL Session

```
>> Counter superclass
=> Actor

>> Counter allSuperclasses
=> [Actor, Object, ProtoObject]

>> Counter inheritsFrom: Object
=> true

>> Counter includesBehaviour: Actor
=> true

>> Counter canUnderstand: #increment
=> true

>> Counter canUnderstand: #class
=> true

>> Counter whichClassIncludesSelector: #class
=> ProtoObject

>> Counter methods
=> [increment, getValue, class, respondsTo:, printString, ...]

>> Counter localMethods
=> [increment, getValue]

>> myCounter respondsTo: #increment
=> true

>> myCounter isKindOf: Actor
=> true

>> myCounter isMemberOf: Counter
=> true

>> myCounter isMemberOf: Actor
=> false
```

### Error Examples

```
>> Counter whichClassIncludesSelector: #nonExistent
=> nil

>> 42 isKindOf: "not a class"
=> Error: isKindOf: expects a Class argument

>> Counter inheritsFrom: Counter
=> false
```

### Runtime Changes

#### Dispatch Change

The 12 hardcoded clauses in `class_send/3` are replaced by normal object dispatch. When a class-side message is not found in user-defined class methods, `class_send/3` delegates to `beamtalk_dispatch:dispatch('Class', Selector, Args, ClassObject)` — walking the `Class` → `Behaviour` → `Object` → `ProtoObject` chain like any other object. No special cases remain.

#### Flattened Table Removal

The pre-computed flattened method tables (`flattened_methods`, `flattened_class_methods`) are removed from the class gen_server state. All dispatch walks the class chain directly via `classSuperclass` + `classIncludesSelector`.

**Deleted infrastructure:**
- `beamtalk_class_hierarchy.erl` — the entire flattened table builder module
- `invalidate_subclass_flattened_tables/1` — the O(N) broadcast cascade
- `{rebuild_flattened, ChangedClass}` message handling — the async rebuild mechanism (BT-510 root cause)
- `flattened_methods` / `flattened_class_methods` fields in `#class_state{}`
- `try_flattened_lookup/2` fast path in `beamtalk_dispatch.erl`

**Instance dispatch** now walks the chain: for each level, check local methods via `has_method`, then follow `superclass` to the next level. At max depth 6 (typical 4), this means at most ~12 gen_server calls for a method defined at the root — microseconds on a local node.

**Future optimization path:** When profiling data identifies hot dispatch paths, a smarter cache can be introduced that exploits sealed methods as optimization hints — sealed methods are guaranteed stable, so their resolution can be cached without invalidation. Hot call sites can be promoted to direct dispatch. This is a targeted, data-driven optimization rather than the current blanket flatten-everything-at-registration approach.

#### New Intrinsics

Thin data-access intrinsics for raw gen_server state reads (7 intrinsics). Hierarchy-walking logic lives in Beamtalk, not Erlang.

| Intrinsic | Backing Erlang | Purpose |
|---|---|---|
| `classSuperclass` | `gen_server:call(ClassPid, superclass)` → returns class object, not atom | Single hop up the chain |
| `classSubclasses` | `beamtalk_class_registry:direct_subclasses/1` → returns class objects | Direct children from ETS |
| `classAllSubclasses` | `beamtalk_class_registry:all_subclasses/1` → returns class objects | Transitive children from ETS |
| `classLocalMethods` | `gen_server:call(ClassPid, methods)` → local keys only | Local method dict |
| `classIncludesSelector` | Local method dict check only | Local containment test |
| `classInstVarNames` | `gen_server:call(ClassPid, instance_variables)` | Local instance variables |
| `className` | `gen_server:call(ClassPid, class_name)` | Class identity |
| `classClass` | Virtual metaclass tag (ADR 0013) | Metaclass identity |

**Methods that are now pure Beamtalk** (previously required Erlang infrastructure):
- `methods` — walks chain collecting `localMethods` at each level
- `canUnderstand:` — walks chain checking `includesSelector:` at each level
- `allInstanceVariableNames` — walks chain collecting `instanceVariableNames` at each level
- `allSuperclasses`, `inheritsFrom:`, `includesBehaviour:`, `whichClassIncludesSelector:` — all chain walks in Beamtalk

**Key fix**: `classSuperclass` returns a proper class object (`{beamtalk_object, ...}`) instead of a bare atom. This fixes the inconsistency where `Counter class` returns an object but `Counter superclass` returned an atom.

## Prior Art

### Pharo/Squeak Smalltalk

The gold standard. Pharo defines a four-class tower: `Behavior → ClassDescription → Class → Metaclass`.

**What we adopt:**
- `Behaviour` as the abstract base for class-describing objects with hierarchy queries and method dictionary access
- `canUnderstand:` on Behaviour (class-side query), `respondsTo:` on Object (instance-side, delegates to `self class canUnderstand:`)
- `includesSelector:` (local only) vs `canUnderstand:` (full chain) distinction
- `whichClassIncludesSelector:` for debugging — walks chain and returns defining class
- `allSuperclasses`, `allSubclasses`, `inheritsFrom:`, `includesBehaviour:`
- `isKindOf:` and `isMemberOf:` on Object (called `isKindOf:` and `isMemberOf:` in Pharo too)

**What we defer:**
- `ClassDescription` (method categorization/protocols, comments) — useful but not needed for the hierarchy bug fix
- Real metaclass instances — ADR 0013's virtual metaclass approach is sufficient for now
- `methodDict` returning a real MethodDictionary object — we return selector lists instead
- `CompiledMethod` objects (`>>` operator, method introspection) — deferred to design-metaprogramming.md
- `compile:` (dynamic method compilation from strings)
- `addSelector:withMethod:` / `removeSelector:` (dynamic method mutation)
- `allInstances` (already exists via ETS tracking, not part of this ADR)
- `basicNew` / `basicNew:` (low-level instance creation bypassing `initialize`)

**What we change:**
- Beamtalk uses `Behaviour` (British spelling, matching Newspeak) rather than `Behavior`
- No `ClassDescription` layer — `Class` inherits directly from `Behaviour`
- `methods` returns all selectors (Pharo uses `allSelectors`); `localMethods` returns local-only (Pharo uses `selectors`)

### Erlang/Elixir

Erlang modules are atoms with `module_info/0,1` for introspection — no hierarchy, no objects. Elixir adds `__info__/1` and protocol dispatch but still has no class objects or inheritance chains.

Beamtalk deliberately diverges from the BEAM norm here. Classes are actor-like processes that respond to messages, not bare module atoms. This is the core of the object model (ADR 0005) and this ADR extends it consistently.

### Ruby

Ruby's `Module#ancestors` returns the full MRO as a list — directly comparable to our `allSuperclasses`. Ruby's `respond_to?` and `is_a?` / `kind_of?` map to our `respondsTo:` and `isKindOf:`. Ruby's `instance_methods(false)` (local only) maps to our `localMethods`.

Ruby's approach validates the ergonomics: classes as real objects with rich query methods makes the REPL and debugging experience significantly better.

### Objective-C

ObjC's `isa` pointer chain is architecturally similar to Beamtalk's `{beamtalk_object, Class, ClassMod, Pid}` record and class chain walk. ObjC's `+respondsToSelector:`, `+instancesRespondToSelector:`, `-isKindOfClass:`, `-isMemberOfClass:` all have direct Beamtalk equivalents in this ADR. ObjC's per-class method cache is a future optimization path for Beamtalk — at v0.1 we walk the chain directly, as early ObjC implementations did before adding the cache.

### Newspeak

Newspeak eliminates the metaclass tower entirely, using nested class declarations and mirror-based reflection instead. While Beamtalk takes inspiration from Newspeak's module system, we preserve the Smalltalk-style class protocol because it is more discoverable in an interactive REPL environment. Newspeak's mirror-based approach is better suited to capability-secure environments; Beamtalk's process isolation provides security guarantees at a different level.

We adopt Newspeak's British spelling (`Behaviour` rather than `Behavior`).

## User Impact

### Newcomer (from Python/JS/Ruby)
- **Discoverable**: `Counter methods` in the REPL shows everything a class can do. `Counter superclass` and `Counter allSuperclasses` make the hierarchy explorable.
- **Familiar**: `isKindOf:` maps to Python's `isinstance()`, Ruby's `is_a?`, JS's `instanceof`. `respondsTo:` maps to Ruby's `respond_to?` and Python's `hasattr`.
- **Error messages**: `Counter whichClassIncludesSelector: #bogus` returns `nil` (safe). Sending a Behaviour message to a non-class (e.g., `42 canUnderstand: #foo`) raises a clear `does_not_understand` error.

### Smalltalk Developer
- **Faithful**: The `Behaviour`/`Class` split, `canUnderstand:` vs `respondsTo:` distinction, and `whichClassIncludesSelector:` all follow Pharo conventions.
- **Missing but planned**: `ClassDescription` (protocols), real metaclass tower, `CompiledMethod` objects, `methodDict` — these are acknowledged deferrals, not omissions.
- **Naming**: `methods` (not `allSelectors`) and `localMethods` (not `selectors`) are more intuitive for non-Smalltalkers. Smalltalk developers will adapt quickly.

### Erlang/BEAM Developer
- **Observable**: Class processes are still gen_servers visible in Observer. The intrinsics are thin wrappers around gen_server calls — no magic.
- **Debuggable**: `Counter whichClassIncludesSelector: #increment` directly answers "where is this method defined?" — a common debugging question on BEAM.
- **Simpler internals**: No flattened table cache, no rebuild cascade, no `beamtalk_class_hierarchy.erl`. Dispatch walks the chain — easy to trace with `dbg`.

### Production Operator
- **Fewer bugs**: One code path for hierarchy queries instead of five reduces production surprises.
- **No rebuild cascade**: The O(N) `invalidate_subclass_flattened_tables` broadcast and async `{rebuild_flattened, ...}` handling are eliminated. Hot reload of a class simply updates its local method dict — no cascade, no race window.
- **No new processes**: No additional gen_servers or ETS tables. Behaviour/Class methods route through existing class processes.

## Steelman Analysis

### Alternative B: Full Metaclass Tower

| Cohort | Strongest argument for the full tower |
|---|---|
| **Smalltalk purist** | "Without real metaclasses, `Counter class class` returns a sentinel atom — that's a lie. The parallel hierarchy is fundamental to Smalltalk's elegance and enables class method inheritance to work correctly. Every compromise here creates a special case somewhere." |
| **Language designer** | "If you build on the virtual metaclass hack now, you'll need to migrate away from it later. The bootstrap complexity is a one-time cost; the architectural clarity pays dividends forever. Do it right once." |

**Why we defer it:** The virtual metaclass design (ADR 0013) works correctly for all current use cases. The full tower requires reworking class-side dispatch fundamentally and introduces bootstrap circularity (`Metaclass class class == Metaclass`) that is complex to implement on BEAM. This ADR is explicitly designed as a stepping stone — the `Behaviour`/`Class` hierarchy makes Option B an incremental addition rather than a rewrite.

### Alternative C: Consolidate in Erlang Only

| Cohort | Strongest argument for keeping it in Erlang |
|---|---|
| **BEAM veteran** | "Erlang is the right language for infrastructure code. Class hierarchy walking is plumbing, not user logic. Keep it in Erlang where it's debuggable with `observer`, `recon`, and `dbg`. Adding a Beamtalk layer means another abstraction to debug through when things go wrong." |
| **Operator** | "Fewer moving parts is always better. The bootstrap sequence is already fragile (BT-510). Don't add more Beamtalk code that depends on the class system being alive to implement the class system." |

**Why we reject it:** Consolidating in Erlang fixes the duplication bug but doesn't address extensibility. Users can't add class-side methods, can't override `whichClassIncludesSelector:` for custom debugging, and the system remains closed. This conflicts with Principles 6 and 8. The bootstrap concern is mitigated by registering Behaviour and Class as part of the existing bootstrap sequence (Phase 1 below).

### Alternative D: Fix the Specific Bugs Only

| Cohort | Strongest argument for fixing bugs and deferring |
|---|---|
| **Pragmatist** | "BT-721 is a one-line fix. BT-510 is a synchronization fix. You're proposing 8 new intrinsics, 2 new stdlib classes, removing the flattened table, and changing the bootstrap sequence to fix two bugs. Ship the fixes now, ship the architecture when you actually need metaprogramming." |
| **Operator** | "Every new abstraction layer is a new failure mode. The bootstrap sequence is already the most fragile part of the system. Don't touch it until you have to." |

**Why we reject it:** The bug fixes address correctness but not completeness. Smalltalk developers — a primary audience for v0.1 — will evaluate Beamtalk by exploring class objects in the REPL. `Counter methods`, `Counter allSuperclasses`, `Counter canUnderstand: #increment` are the vocabulary they expect. Without `Behaviour`/`Class`, that vocabulary doesn't exist. This is not a future need — it's a v0.1 first-impression requirement.

### Tension Points

- BEAM veterans and operators prefer the simplicity of Option C. Smalltalk purists want the purity of Option B. Option A is the pragmatic middle ground — faithful enough for Smalltalkers, incremental for BEAM veterans, and leaves a clean upgrade path to Option B.
- The "Erlang for infrastructure" argument has merit for raw data access (which remains in Erlang via thin intrinsics), but not for user-facing reflection queries that benefit from being written in the language itself.

## Alternatives Considered

### Alternative B: Full Metaclass Tower

Implement the complete Pharo model: `Behaviour → ClassDescription → Class → Metaclass` as four real Beamtalk classes. Every metaclass would be a distinct instance of `Metaclass`. The metaclass hierarchy would parallel the instance hierarchy: `Counter class superclass == Actor class`.

```beamtalk
Counter class class        // => Metaclass (real class, not sentinel atom)
Counter class superclass   // => Actor class
Metaclass class class      // => Metaclass (self-grounding)
```

**Rejected because:** The ADR 0013 virtual metaclass design would need reworking. The bootstrap circularity (`Metaclass class class == Metaclass`) is complex on BEAM. This ADR is designed so Option B is an incremental addition later — `Behaviour` and `Class` are the foundation that the full tower builds on.

### Alternative C: Consolidate in Erlang Only

Keep all class protocol in Erlang but consolidate the 5+ implementations into one authoritative module (`beamtalk_class_protocol.erl`).

**Rejected because:** Doesn't address extensibility. Users can't define or override class-side protocol methods in Beamtalk. Conflicts with Principles 6 (Messages All The Way Down) and 8 (Reflection as Primitive). Fixes the duplication but not the architecture.

### Alternative D: Fix the Specific Bugs Only

Fix BT-721 (make `responds_to_slow` use the flattened table) and BT-510 (synchronize class registration ordering) directly, without introducing `Behaviour`/`Class` or changing the architecture. Defer the class protocol to a later release.

**Rejected because:** While the bug fixes are small (BT-721 is a one-line change), the missing class protocol is a user-visible gap, not just an internal inconsistency. Smalltalk developers evaluating Beamtalk at v0.1 will immediately try `Counter superclass`, `Counter methods`, and `Counter allSuperclasses` in the REPL — these are the first things a Smalltalker does to explore a system. An impoverished class protocol makes the language feel incomplete to a key audience. The bugs are symptoms; the missing abstraction is the disease.

## Consequences

### Positive
- **Single source of truth**: Hierarchy walking logic exists once in Beamtalk, not five times in Erlang
- **BT-510 eliminated by design**: Removing the flattened table removes the async rebuild cascade and its race window entirely. No table to get out of sync.
- **Massive complexity reduction**: `beamtalk_class_hierarchy.erl` (flattened table builder), the O(N) `invalidate_subclass_flattened_tables` broadcast, and `{rebuild_flattened, ...}` handling are all deleted. The class gen_server state shrinks by two fields.
- **More logic in Beamtalk**: `methods`, `canUnderstand:`, `allInstanceVariableNames`, `allSuperclasses`, `inheritsFrom:`, `whichClassIncludesSelector:` are all pure Beamtalk chain walks — 7 intrinsics down from 11, and the intrinsics are thin data reads, not logic.
- **Extensibility foundation**: `Behaviour`/`Class` establish the class protocol in Beamtalk rather than Erlang. All methods are sealed at v0.1 to limit blast radius; unsealing specific methods is a future option as the metaclass tower matures.
- **Discoverability**: `Counter methods`, `Counter allSuperclasses`, `Counter canUnderstand: #x` are explorable in the REPL
- **Bug reduction**: `respondsTo:` becomes `self class canUnderstand: selector` — one line, no duplication
- **Foundation for metaprogramming**: `Behaviour`/`Class` are the base that the full metaclass tower (design-metaprogramming.md) builds on
- **Principle alignment**: Satisfies Principles 6 (Messages All The Way Down), 8 (Reflection as Primitive), and 11 (Live Patching is a Message Send)

### Negative
- **Bootstrap complexity**: Behaviour and Class must be registered before any user classes load — extends the existing bootstrap sequence
- **Additional abstraction layer**: Debugging class-side dispatch now traverses Beamtalk → intrinsic → Erlang gen_server (one more layer than before)
- **Linear dispatch cost**: Without the flattened table, inherited method dispatch costs 2 gen_server calls per hierarchy level (vs O(1) before). At max depth 6, this is ~12 calls for a root-level method — microseconds on a local node, but measurable under extreme load. Mitigated by shallow hierarchies at v0.1 and a future smart cache path (sealed method promotion, hot call site caching).

### Neutral
- **`superclass` now returns class objects, not atoms**: Technically a breaking change, but the previous behavior (returning bare atoms) was inconsistent with `class` (which returns objects). Any code comparing `Counter superclass == 'Actor'` must change to `Counter superclass == Actor`. This is a bug fix, not a regression.
- **`methods` vs `allSelectors` naming**: Different from Pharo's naming but more intuitive for non-Smalltalkers. Documented in Prior Art.

## Implementation

### Phase 0: Wire Check — Dispatch Fallthrough Proof

**Affected components:** Runtime (`beamtalk_runtime`)

Prove the core assumption before building the full feature: a class-side message not found in user-defined class methods can be dispatched through the `Class` instance method chain.

1. Hardcode one test method on a temporary `Class` bootstrap entry (e.g., `testClassProtocol`)
2. Send `Counter testClassProtocol` and verify it dispatches through the `Class` chain
3. Verify the virtual metaclass tag is preserved through the dispatch
4. Remove the test method — the mechanism works, proceed to Phase 1

### Phase 1: Remove Flattened Table

**Affected components:** Runtime (`beamtalk_runtime`)

1. Remove `flattened_methods` and `flattened_class_methods` from `#class_state{}`
2. Delete `beamtalk_class_hierarchy.erl` entirely
3. Remove `invalidate_subclass_flattened_tables/1` from `beamtalk_class_registry.erl`
4. Remove `{rebuild_flattened, _}` message handling from `beamtalk_object_class.erl`
5. Update `beamtalk_dispatch.erl`: replace `try_flattened_lookup/2` fast path with direct chain walk via `has_method/2` + `superclass/1` at each level
6. Update `beamtalk_dispatch:responds_to/2` to use the same chain walk
7. Update all tests that depend on flattened table internals

### Phase 2: Bootstrap, Intrinsics, and Stdlib Classes

**Affected components:** Runtime, stdlib (`lib/`), codegen (`beamtalk-core`)

1. Implement 8 thin data-access intrinsics in a new `beamtalk_behaviour_intrinsics.erl`:
   - `classSuperclass/1` — returns class object (not atom)
   - `classSubclasses/1`, `classAllSubclasses/1` — return class object lists
   - `classLocalMethods/1` — local selector list
   - `classIncludesSelector/2` — local containment test
   - `classInstVarNames/1` — local instance variable names
   - `className/1`, `classClass/1` — identity queries

2. Update `beamtalk_bootstrap.erl` to register `Behaviour` and `Class`:
   ```
   ProtoObject → Object → Behaviour → Class → Actor → user modules
   ```

3. Update `class_send/3` in `beamtalk_class_dispatch.erl`: replace 12 hardcoded selector clauses with a fallback to `beamtalk_dispatch:dispatch('Class', Selector, Args, ClassObject)`.

4. Create `lib/Behaviour.bt` — hierarchy queries and method dictionary (chain walks in Beamtalk)
5. Create `lib/Class.bt` — name, identity, printString (sealed)
6. Update `lib/Object.bt`:
   - Replace `respondsTo:` intrinsic with `self class canUnderstand: selector`
   - Add `isKindOf:` and `isMemberOf:`
7. Register Behaviour and Class in the static `ClassHierarchy` (Rust semantic analysis)
8. Update `build_stdlib.rs` to include Behaviour and Class in the auto-generated builtins
9. Add post-bootstrap assertion verifying the class protocol is functional

### Phase 3: Cleanup

**Affected components:** Runtime, codegen

1. Consolidate `responds_to` implementations in `beamtalk_dispatch.erl`, `beamtalk_primitive.erl`, and `beamtalk_actor.erl` to delegate to `canUnderstand:` via the class process
2. Remove duplicate `has_method` paths in `beamtalk_object_class.erl`
3. Remove the 12 hardcoded selector clauses from `class_send/3` (now handled by Behaviour/Class chain)
4. Update tests in `tests/stdlib/` for the new API

### Phase 4: Extended Protocol (Future)

Not part of this ADR, but enabled by it:
- `ClassDescription` with method categorization/protocols
- Full metaclass tower (design-metaprogramming.md Phases 1-5)
- `CompiledMethod` objects and `>>` operator
- Dynamic method addition/removal (`addSelector:withMethod:`)
- Smart dispatch cache: sealed method promotion, hot call site caching based on profiling data

## Migration Path

### Breaking Change: `superclass` Returns Class Object

Previously, `Counter superclass` returned the atom `'Actor'`. After this ADR, it returns a class object (`{beamtalk_object, 'Actor class', actor, Pid}`).

Code that compares against atoms must be updated:

```beamtalk
// Before (broken):
Counter superclass == 'Actor'

// After (correct):
Counter superclass == Actor
Counter superclass name == #Actor
```

This change is intentional — the atom return was a bug, inconsistent with `Counter class` which already returns a class object.

### Breaking Change: `methods` Includes Inherited

The `methods` selector on class objects already returned flattened (inherited) selectors, so this is not a behavioral change. The new `localMethods` selector provides the local-only behavior that was previously unavailable.

## Implementation Tracking

**Epic:** [BT-731](https://linear.app/beamtalk/issue/BT-731) — Early Class Protocol
**Issues:**
- BT-732: Wire Check — Dispatch Fallthrough Proof (S)
- BT-733: Remove Flattened Method Table (M)
- BT-734: Bootstrap Intrinsics and Stdlib Behaviour/Class (L)
- BT-735: Cleanup — Consolidate Duplicate Dispatch Paths (M)
**Status:** Planned

## References
- Related ADRs: [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md) (Object Model), [ADR 0006](0006-unified-method-dispatch.md) (Unified Dispatch), [ADR 0007](0007-compilable-stdlib-with-primitive-injection.md) (Compilable Stdlib), [ADR 0013](0013-class-variables-class-methods-instantiation.md) (Virtual Metaclasses), [ADR 0015](0015-repl-error-objects-and-exception-hierarchy.md) (Error Hierarchy — new intrinsics must use `beamtalk_error:raise/1`)
- Design docs: [design-metaprogramming.md](../internal/design-metaprogramming.md), [design-self-as-object.md](../internal/design-self-as-object.md)
- Fixed bugs: BT-721 (respondsTo: for inherited methods), BT-510 (out-of-order class loading)
- Pharo sources: [Behavior.class.st](https://github.com/pharo-project/pharo/blob/Pharo12/src/Kernel-CodeModel/Behavior.class.st), [Class.class.st](https://github.com/pharo-project/pharo/blob/Pharo12/src/Kernel-CodeModel/Class.class.st), [Metaclass.class.st](https://github.com/pharo-project/pharo/blob/Pharo12/src/Kernel-CodeModel/Metaclass.class.st)
- [Pharo Reflective Core Booklet](https://books.pharo.org/booklet-ReflectiveCore/)
- [Smalltalk-80 Blue Book, Chapter 5](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf)
