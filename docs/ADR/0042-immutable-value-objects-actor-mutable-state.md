# ADR 0042: Immutable Value Objects and Actor-Only Mutable State

## Status
Proposed (2026-02-25)

## Context

### The State-Threading Quagmire

BeamTalk compiles to Core Erlang on the BEAM VM, which has no mutable variables. The current compiler maps Smalltalk's mutable-object semantics onto BEAM using compile-time state threading — an additional `StateAcc` parameter threaded through generated code, effectively implementing a state monad transformation as a compiler pass.

This approach has proven deeply problematic:

1. **Self-sends require data dependency analysis.** Sequential self-sends that mutate state (`self name: 'foo'. self age: 42.`) must be detected and compiled with explicit state threading between them.

2. **Conditional mutations** require reconciling which version of "self" emerges from branches of `ifTrue:ifFalse:`.

3. **Blocks that modify local state** are currently disallowed — blocks cannot modify enclosing local variables. This breaks common Smalltalk patterns and hurts the "it feels like Smalltalk" developer experience.

4. **Higher-order message sends are unsupported** due to the complexity of threading state through arbitrary block-passing patterns.

5. **Deep call chains with self-mutation** require propagating changed state back up through the return path, which Smalltalk makes invisible (pointer mutation) but the BEAM requires explicit handling for.

ADR 0041 (Universal State-Threading Block Protocol) solved the block composability aspect by making state threading universal rather than whitelist-gated. This is largely implemented and working — blocks passed to user-defined HOMs now correctly propagate mutations. However, ADR 0041 addresses one facet of a deeper problem: **trying to emulate mutable-object semantics on an immutable platform**. Self-send chains, conditional mutations, and deep call-chain propagation remain complex even with universal block threading.

### The Root Cause

The fundamental tension is between Smalltalk's model (objects are mutable places in memory, assignment mutates a slot) and the BEAM's model (all data is immutable, state lives in processes). Every compiler pass that threads state, every `StateAcc` map, every dual codegen path for pure/stateful blocks exists because we're fighting the platform.

### What Actually Needs Mutability?

Examining real Smalltalk codebases, mutable state falls into two categories:

1. **Identity-bearing stateful entities** — counters, connections, UI widgets, services. These have a stable identity that persists across state changes. On the BEAM, these naturally map to **processes** (gen_servers).

2. **Value computations** — points, colors, dates, collections, DTOs. These are computed, passed around, and transformed. They don't need identity — two `Point x: 3 y: 4` instances are interchangeable. On the BEAM, these naturally map to **immutable terms** (maps, tuples, lists).

The state-threading complexity exists entirely because we try to make category 2 behave like it has mutable slots. If we stop doing that, the compiler problem disappears.

## Decision

Adopt **immutable value semantics** as the default for all BeamTalk objects. Mutable state is confined to **actor processes** (BEAM processes / OTP gen_servers), where `self.slot :=` is allowed and compiles to direct `maps:put` on the state map.

### Two Kinds of Entities

1. **Value Objects** — Immutable. Every "mutating" message returns a new object. `self.slot :=` is a compile error. These compile directly to BEAM terms (maps) with no transformation.

2. **Actors** — BEAM processes wrapping a gen_server. `self.slot :=` is allowed inside actor methods, compiling to direct `maps:put` on the state map. Auto-generated `with*:` methods serve as the public API for external callers. The gen_server manages state persistence across method invocations.

### Value Object Semantics

#### Declaration and Construction

Value objects are declared with `state:` and constructed via the existing `new`/`new:` protocol:

```smalltalk
Value subclass: Point
  state: x = 0
  state: y = 0

point := Point new: #{#x => 3, #y => 4}.   "Map-based — any subset of fields, defaults fill the rest"
point := Point new.                          "All defaults — Point(0, 0)"
point := Point x: 3 y: 4.                   "All-fields keyword constructor (auto-generated)"
```

**Syntax note:** Today, value types use `Object subclass:` with `state:` declarations. This ADR introduces `Value subclass:` as the explicit declaration form. `Value` becomes a new root class in the hierarchy between `Object` and user value classes. The `state:` keyword is retained as the universal slot declaration for both value types and actors — the `Value` vs `Actor` superclass determines immutability semantics, not the declaration keyword. This is deliberate: `state:` declares "the named data this object holds," which is the same concept regardless of mutability. Whether those slots are immutable (value types) or mutable (actors) is determined by the class kind, not the declaration keyword — just as Smalltalk's `instanceVariableNames:` doesn't change meaning based on whether the class uses mutable or copy-on-write semantics. Introducing separate keywords (`slots:` vs `state:`) was considered and rejected as unnecessary churn: the `Value`/`Actor` superclass already provides a clear, visible signal at the declaration site.

**Construction forms:** Three ways to construct a value object, avoiding Smalltalk's constructor explosion (2^N constructors for N fields):

1. **`new: #{...}`** — Map-based, pass any subset of fields. Defaults fill the rest. This is the primary construction form and already exists today. Ideal for programmatic construction, partial initialization, and agent use.
2. **`new`** — All defaults. Already exists today.
3. **`ClassName field1: val1 field2: val2`** — Auto-generated all-fields keyword constructor. One method per class, always requires all fields. Provides LSP discoverability for free (method selector completion works out of the box). If usage data shows this is unused, it can be removed.

**Discoverability:** Field names are discoverable via `Point fieldNames` (returns `#[x, y]`) in the REPL and via `:help Point`. The keyword constructor additionally makes fields visible through IDE autocomplete — typing `Point x:<TAB>` triggers standard method selector completion.

**Class-level state:** `classState:` declares class-level mutable state (it lives in the class object's gen_server state), regardless of whether the class is a Value or Actor type:

```smalltalk
Value subclass: Point
  classState: instanceCount = 0
  state: x = 0
  state: y = 0

  class create: x y: y =>
    self.instanceCount := self.instanceCount + 1.
    self new: #{#x => x, #y => y}
```

#### Functional Updates via `with*:`

"Mutating" messages return a new object. The compiler auto-generates `with*:` methods for each slot:

```smalltalk
point := Point new: #{#x => 3, #y => 4}.
point2 := point withX: 5.       "Returns new Point(5, 4). point is unchanged."

list := List new.
list2 := list add: 'hello'.     "Returns new List containing 'hello'."
```

For a value class with declared slots `name`, `age`, `email`, the compiler generates:

- `name`, `age`, `email` — getters (read-only)
- `withName:`, `withAge:`, `withEmail:` — functional setters (return new object with that slot changed)
- `ClassName name: val1 age: val2 email: val3` — all-fields keyword constructor (one method, all fields required)

Under the hood, these compile to trivial Erlang map operations. For **value types**, where `Self` is the map directly:

```erlang
%% getter (value type — Self is the map)
'x'/1 = fun (Self) -> call 'maps':'get'('x', Self)

%% functional setter — preserves all slots, including subclass slots
'withX:'/2 = fun (Self, NewX) -> call 'maps':'put'('x', NewX, Self)
```

For **actors**, the same `with*:` methods exist but the dispatch framework mediates: `self` in an actor method is a `#beamtalk_object{}` record (see "self and Actor Identity" below), and `self withValue: 5` is a self-send that goes through `Module:dispatch`, which provides the underlying state map to the method. The generated `with*:` code operates on the state map, not the `#beamtalk_object{}` record.

**Subclass safety:** Since `with*:` compiles to `maps:put` on the existing map, it preserves all slots — including slots added by subclasses. `aColorPoint withX: 5` returns a new map with the same `class`, `color`, `y`, and updated `x`. The result is still a `ColorPoint`, not a `Point`. This works because Erlang maps preserve all keys through `maps:put`. No virtual dispatch or factory method is needed.

**Subclass depth:** Value type subclassing has no artificial depth limit. The map representation handles inheritance naturally — each subclass adds its own slots to the map. Dispatch chains walk up the class hierarchy via `superclass/0` delegation (the existing mechanism). Since maps have O(log N) key access (small maps use flat arrays with O(N) scan; large maps use HAMTs with O(log₃₂ N) lookup — effectively constant for realistic slot counts), deep hierarchies impose no meaningful per-slot-access penalty. The practical limit is the same as any Smalltalk class hierarchy — deep hierarchies are a design smell, not a technical constraint.

**Equality:** Value objects use **structural equality**. Two `Point` instances with the same `x` and `y` are equal (`=:=`). This follows naturally from the Erlang map representation — internally `#{'$beamtalk_class' => 'Point', '__class_mod__' => ..., x => 3, y => 4}`, where all metadata keys are identical for instances of the same class, so structural equality reduces to slot-value comparison. For value objects, `==` (loose equality) and `=:=` (strict equality) both compare structurally. Actors use **process identity** — two actors with identical state are different objects if they are different processes.

#### No Instance Variable Assignment on Value Types

Value types prohibit `:=` assignment to instance variables. If you want a modified version, you create a new one:

```smalltalk
"In a Value subclass method:"
self x := 5.        "COMPILER ERROR — no instance variable assignment on value type"

"Instead:"
self withX: 5       "Returns new object"
```

Actors DO allow `self.slot :=` — see "Actor Semantics" below.

**Reflection API:** `fieldAt:put:` on value types raises a `#beamtalk_error{type => immutable_value}` at runtime. On actors, `fieldAt:put:` works normally (actors have mutable state). Immutability is enforced both at compile time (`self.slot :=` rejected by semantic analysis) and at runtime (reflection API raises error on value types). The compile-time check catches the common case; the runtime check catches the reflection path.

#### Local Variable Rebinding

Local variables can be rebound with `:=`. Each rebinding creates a new value — this is shadowing, not mutation, consistent with Elixir's model and with BeamTalk's own REPL semantics:

```smalltalk
x := 0.
x := x + 1.       "Rebinding — x is now 1"
x := x * 2.       "Rebinding — x is now 2"
```

**Blocks capture the value at binding time, not a mutable slot.** Rebinding a local inside a block propagates via ADR 0041's state-threading protocol (already implemented):

```smalltalk
count := 0.
items do: [:each | count := count + each].
count  "=> sum of items — state threaded through block"
```

This is consistent with how the REPL already works — top-level `:=` rebinds workspace variables, and blocks inside REPL expressions can mutate captured locals via state threading. Without local rebinding in compiled code, there would be a pedagogical cliff: code that works in the REPL would break when moved to a class method.

**Rationale for rebinding over single-assignment:**

- **REPL consistency.** The REPL already allows `x := x + 1`. Compiled methods must do the same, or code cannot move between REPL and class definitions without rewriting.
- **Elixir precedent.** Elixir allows rebinding everywhere and it was critical to onboarding Ruby developers. BeamTalk faces the same transition with Smalltalk developers.
- **ADR 0041 handles it.** The universal state-threading block protocol is implemented and working. Local variable rebinding inside blocks is exactly the problem it solves — no new compiler infrastructure needed.
- **Objects stay immutable.** Local rebinding doesn't affect the object model. `self x := 5` is still a compiler error. Only local variables (method temporaries) can be rebound.

**What rebinding is NOT:**

- It is not instance variable mutation. `self x := 5` remains illegal.
- It is not reference mutation. Rebinding `x` doesn't affect any other reference to the old value.
- Under the hood, it compiles to a new SSA binding (like Erlang/LLVM), or to `StateAcc` updates when captured by a block.

#### Future: `update*:` Methods

A future enhancement could add `update*:` methods that take a block for slot transformation:

```smalltalk
point updateX: [:x | x + 1].     "equivalent to: point withX: point x + 1"
```

This is not part of the initial implementation but is a natural extension.

### Actor Semantics

#### Declaration and State

Actors are declared with `state:` and default values. State is initialized when the actor is spawned:

```smalltalk
Actor subclass: Counter
  state: value = 0

counter := Counter spawn.         "Spawns a gen_server process"
```

#### Methods and State Mutation

Inside actor methods, slots are updated directly with `:=`. This compiles to the same underlying slot-write operation that `with*:` uses (today: `maps:put` on the state map), but without a dispatch call or intermediate variables:

```smalltalk
Actor subclass: Counter
  state: value = 0

  increment => self.value := self.value + 1
  decrement => self.value := self.value - 1
  getValue  => self.value
```

The compiler auto-generates `with*:` methods as the **public API** for external callers. Inside actor methods, direct slot assignment is preferred:

- **No dispatch overhead:** `self.slot := expr` compiles to the slot-write operation directly (today: `maps:put(slot, Expr, StateN)`), without going through message dispatch. `self withSlot: expr` is a self-send through dispatch, requiring `{reply, Result, NewState}` tuple unpacking.
- **Safer:** With `self.slot :=`, the state map always reflects all assignments. With `with*:` chains, forgetting to capture an intermediate variable silently loses state updates — a data-loss footgun with no compile-time or runtime error.
- **Same underlying operation as `with*:`:** Both `self.slot := expr` and the auto-generated `withSlot:` compile to the same slot-write code (today: `maps:put`). The difference is dispatch, not the write itself. The distinction is "no message send" vs "message send," not "validated" vs "unvalidated."
- **`with*:` is the public API:** External callers use `with*:`, which is dispatch-mediated and overridable. If a subclass needs validation on slot writes, it overrides `withSlot:`, which gates the *external* API. This is analogous to Erlang's direct state map manipulation inside `handle_call`, and to Swift's `mutating` methods which bypass `willSet`/`didSet` property observers.
- **Compiler warning when `withSlot:` is overridden:** If any class in the hierarchy has overridden `withSlot:` (adding validation, logging, or other logic), the compiler warns on `self.slot :=` usage: *"`self.x :=` bypasses `ValidatedPoint#withX:` — use `self withX:` if validation is needed."* The compiler has full visibility into the class hierarchy at compile time (via `ClassHierarchy`), so this check is free. This ensures developers are aware when `self.slot :=` would skip custom logic, and the choice to bypass it is explicit and visible in the code. The warning can be suppressed with a pragma or annotation if the bypass is intentional.

```smalltalk
"FOOTGUN — with*: chain, uncaptured intermediate silently loses balance update:"
deposit: amount =>
  self withBalance: self balance + amount.            "← result discarded!"
  self withTransactions: (self transactions add: amount)  "operates on ORIGINAL state"

"SAFE — direct slot assignment, impossible to lose an update:"
deposit: amount =>
  self.balance := self.balance + amount.
  self.transactions := self.transactions add: amount
```

#### Return Value and State Update Rules

The gen_server framework uses two rules to interpret method results:

1. **Last expression (no `^`):** The result is the reply to the caller. The gen_server state is the accumulated result of all `self.slot :=` assignments in the method body. If the method contains no slot assignments (pure query), the original state is preserved.

2. **Early return (`^`):** The `^` expression is the reply to the caller. State accumulated up to the `^` point is preserved — any slot assignments before `^` are kept.

Under the hood, `self.slot := expr` compiles to `StateN = maps:put(slot, Expr, StateN-1)` — sequential state variable renaming within the method body. The generated `handle_call` returns `{reply, LastExpr, FinalState}`.

```smalltalk
"State mutation — last expression is the reply:"
increment => self.value := self.value + 1
"Compiles to: State1 = maps:put(value, maps:get(value, State0) + 1, State0)"
"Generated: {reply, NewValue, State1}"

"Query — last expression is the reply, state unchanged:"
getValue => self.value
"Generated: {reply, Value, State0}"

"Mixed — mutate then return a specific value:"
incrementAndGetOld =>
  | old |
  old := self.value.
  self.value := self.value + 1.
  ^old
"State updated to State1, old value returned"
"^old throws {$bt_nlr, Token, old, State1}"
```

**Self-sends within actor methods** still go through `Module:dispatch`, which returns `{reply, Result, NewState}`. The compiler threads the new state forward. But for simple slot updates, direct `self.slot :=` avoids the dispatch round-trip entirely.

**Why this eliminates self-send dependency analysis:** Under the status quo, `self name: 'foo'. self age: 42.` requires the compiler to detect the data dependency between sequential slot mutations. Under ADR 0042, direct slot assignments (`self.slot :=`) compile to sequential `maps:put` calls with explicit state variable threading — no dependency analysis needed. Self-sends to other methods (`self someMethod`) go through dispatch and return `{reply, Result, NewState}`, which the compiler threads automatically.

#### Sequential State Updates in Actor Methods

Multi-step state updates are straightforward — each line updates state directly:

```smalltalk
Actor subclass: Account
  state: balance = 0
  state: transactions = List new

  deposit: amount =>
    self.balance := self.balance + amount.
    self.transactions := self.transactions add: amount

  depositAll: amounts =>
    amounts do: [:amount |
      self.balance := self.balance + amount
    ]
```

Slot assignments inside blocks are handled by ADR 0041's state-threading protocol — the same mechanism that threads local variable rebindings through blocks. The `self.slot :=` is syntactic sugar for updating the state map variable, which ADR 0041 threads through the block like any other captured local.

#### `self` and Actor Identity

Inside an actor method, `self` is a `#beamtalk_object{class, class_mod, pid}` record that carries both **identity** (the process pid) and **state access** (slot reads dispatch to the underlying state map). This is the existing implementation and it naturally solves the "how do I get my pid?" question.

```smalltalk
"Inside the actor method:"
increment => self.value := self.value + 1      "direct state map update"
notifyOther: other => other update: self.pid   "self.pid returns the actor's pid"

"Outside — counter is a pid, message goes through gen_server:"
counter increment.
counter notifyOther: logger.
```

The `#beamtalk_object{}` record is the actor's "calling card" — it can be passed to other actors as a reference. From outside, message sends go through `gen_server:call/cast`. From inside (self-sends), the dispatch bypasses gen_server to avoid deadlock, calling `Module:dispatch` directly.

**`self.pid`** returns the underlying BEAM process identifier (`erlang:element(4, Self)` on the `#beamtalk_object{}` tuple). This is defined as a method on the `Actor` base class (not auto-generated per-class). It is useful for:
- Passing your reference to another actor (`other register: self.pid`)
- Monitoring/linking (`self.pid` can be passed to Erlang's `monitor/2`)
- Logging and debugging (pid identifies the actor in crash dumps)

**State map vs `self`:** Internally, the gen_server callbacks receive a plain state map (`State`). The runtime's `make_self/1` wraps it in `#beamtalk_object{}` before each method invocation. Slot reads on `self` dispatch to `maps:get` on the state map. `with*:` methods operate on the state map and return a new state map. The `#beamtalk_object{}` wrapper is transparent — developers work with `self` uniformly.

### Message Send Semantics: Call vs Cast

**See ADR 0043 (Sync-by-Default Actor Messaging).** The call/cast protocol (`.` for synchronous `gen_server:call`, `!` for fire-and-forget `gen_server:cast`) is specified in its own ADR, independent of the value/actor object model.

### Cascade Semantics

**Deferred to ADR 0044.** Cascade semantics for immutable value objects are a significant design question — Smalltalk cascades (`;`) send multiple messages to the same receiver, but with immutable objects each message returns a new object, making traditional fan-out semantics problematic. The leading candidate is cascade-as-pipeline on value objects (each message sent to the *result* of the previous), which would give BeamTalk Elixir-style `|>` pipelines for free (BT-506). However, the type-dependent semantics (pipeline on values, fan-out on actors), dynamic receiver handling, and interaction with ADR 0039 deserve their own focused ADR.

For the initial implementation of ADR 0042, explicit chaining works correctly and unambiguously:

```smalltalk
((point withX: 5) withY: 10).
(items select: [:x | x > 0]) collect: [:x | x * 2].
```

### REPL Semantics

The REPL and compiled code have **consistent rebinding semantics**. Local variable rebinding works the same way in both contexts:

```smalltalk
"In the REPL:"
x := 0.
x := x + 1.
items do: [:each | x := x + each].

"In a compiled method — identical behavior:"
myMethod =>
  x := 0.
  x := x + 1.
  items do: [:each | x := x + each].
```

Under the hood, the mechanisms differ — the REPL threads state through workspace bindings managed by `beamtalk_repl_shell` (ADR 0040), while compiled methods use SSA bindings or ADR 0041's `StateAcc` for block captures — but the observable behavior is identical. Code can move freely between the REPL and class definitions without rewriting.

## Prior Art

### Erlang/OTP — The Platform We Compile To

Erlang embraces immutability as a core language feature. Variables are single-assignment. State lives in processes (gen_servers). This ADR aligns BeamTalk with the platform rather than fighting it.

**Adopted:** The process-as-state-container model. Actor = gen_server with functional state transitions.

### Clojure — Immutable by Default, Controlled Mutability

Clojure's persistent data structures are immutable. Mutable state is managed through controlled references (atoms, refs, agents). This separation of "values" from "identity" directly inspired BeamTalk's value object / actor split.

**Adopted:** The values-vs-identity distinction. Value objects are Clojure's persistent maps; actors are Clojure's atoms/agents.

**Similar:** Both allow local rebinding. Clojure uses `loop/recur` for accumulation; BeamTalk uses `:=` rebinding and `inject:into:`.

### Elixir — Rebinding as Ergonomic Immutability

Elixir is fully immutable on BEAM but allows variable **rebinding** — `x = x + 1` creates a new binding that shadows the old one. This is semantically distinct from mutation (closures capture the value at binding time, not a mutable slot), but it *feels* like mutation to developers coming from Ruby.

This was a deliberate design choice by José Valim to ease the Ruby-to-BEAM transition. Elixir faced exactly the same problem BeamTalk faces: bringing developers from a mutable OOP language (Ruby) to an immutable platform (BEAM). Elixir's solution was to make immutability invisible at the surface syntax level while keeping it real at the semantic level.

Accumulation patterns use `Enum.reduce/3` (equivalent to `inject:into:`). Elixir allows rebinding captured variables inside anonymous functions, but the rebinding does not propagate back to the enclosing scope — `fn x -> count = count + x end` creates a new binding of `count` inside the fn, but the outer `count` is unchanged. BeamTalk's approach differs here: with ADR 0041's state threading, rebinding *does* propagate through blocks, which is more powerful than Elixir but requires compiler infrastructure.

**Adopted:** The precedent that BEAM languages can successfully onboard developers from mutable-OOP backgrounds. Elixir proved it with Ruby developers; BeamTalk aims to do the same with Smalltalk developers.

**Relevant insight:** Elixir's rebinding is essentially what BeamTalk's REPL already does — top-level `:=` creates new bindings in the workspace, not mutations. The REPL "feels mutable" but is semantically immutable. This ADR extends that principle to the whole language.

**Adopted:** Rebinding within method bodies. Like Elixir, BeamTalk allows `x := x + 1` as a rebinding (not mutation). This is essential for REPL/compiled-code consistency and for onboarding Smalltalk developers who expect mutable temporaries. The syntax is different (`:=` vs `=`) but the semantics are the same: new binding, old value unchanged, closures capture at binding time.

### Gleam — Strict Immutability on BEAM

Gleam compiles to Erlang with strictly immutable semantics. `let x = 1` followed by `let x = x + 1` is allowed (shadowing, like Rust), but there is no variable reassignment and no mutable state outside processes. Accumulation uses `list.fold`.

Gleam is the strictest BEAM language and leans into it — the community accepts strict immutability as a feature, not a limitation. Gleam has grown significantly since its 1.0 release, validating that strict immutability does not prevent adoption on BEAM.

**Adopted:** The validation that strict immutability works on BEAM without driving users away. If Gleam can build a thriving community with no mutable variables, no classes, and no OOP, BeamTalk can certainly work with immutable objects and Smalltalk's rich collection protocol.

**Difference:** Gleam has no OOP — no classes, no methods, no inheritance. BeamTalk keeps the Smalltalk class model, which provides richer vocabulary for functional patterns (`inject:into:` reads more naturally than `list.fold`).

### Newspeak — Smalltalk's Successor

Newspeak (Gilad Bracha's successor to Smalltalk) retains mutable instance variables but adds module-level encapsulation. It doesn't attempt immutability.

**Not adopted:** Newspeak's approach assumes a traditional VM with mutable heap objects. Not viable on BEAM without the state-threading tax.

### Swift Value Types

Swift distinguishes between value types (`struct`) and reference types (`class`). Value types are copied on assignment and cannot be mutated without `mutating` keyword (which creates a new value under the hood).

**Adopted:** The distinction between value types (copied, immutable in effect) and reference types (identity-bearing, mutable). Swift's `mutating` is analogous to BeamTalk's `with*:` — both create new values, just with different syntax.

### Pony — Reference Capabilities

Pony uses reference capabilities (`iso`, `val`, `ref`, `box`, `trn`, `tag`) to distinguish mutable from immutable data at the type level. Closures copy captured variables — mutation doesn't propagate.

**Insight adopted:** The distinction between "values you can share" and "entities with identity" is fundamental. BeamTalk makes this distinction at the class declaration level (`Value subclass:` vs `Actor subclass:`) rather than at the type annotation level.

### Alan Kay on Erlang

Alan Kay has acknowledged that Erlang captures the message-passing spirit of Smalltalk better than most Smalltalk implementations. Processes communicating via messages — not objects sharing mutable memory — was the original vision.

**Adopted:** The actor model IS the Smalltalk vision, properly realized. Value objects are the data that flows between actors.

## User Impact

### Newcomer (from Python/JS/Ruby)
**Positive:** The mental model is simple — things are either data (immutable, like Python tuples/frozen dataclasses) or services (actors, like microservices with state). No hidden aliasing bugs where modifying one reference affects another. Local variable rebinding works as expected (`count := count + 1`), so accumulator patterns are familiar.

**Concern:** No instance variable assignment (`self x := 5`) is a departure from Ruby/Python class patterns. Must learn `with*:` methods for updating objects. The distinction between "I can rebind locals but not slots" requires explanation.

**Mitigation:** Auto-generated `with*:` methods make immutable updates ergonomic. Good error messages at the `self.slot :=` boundary should guide toward `self withSlot: newValue`. The `collect:`, `select:`, `inject:into:` vocabulary provides rich alternatives to imperative collection manipulation.

### Smalltalk Developer
**Concern:** Mutable instance variables are fundamental to Smalltalk's model. The Visitor pattern, the Builder pattern, the Observer pattern — they all assume mutable state. `self x := 5` not working is a significant departure. The "it's just Smalltalk" story weakens.

**Positive:** Local variable rebinding works (`result := result + each` inside blocks), so method-level accumulator patterns are preserved. Blocks and HOMs work universally — no whitelist, no mysterious failures. The actor model preserves Smalltalk's core insight (objects communicating via messages).

**Mitigation:** Position as "Smalltalk's message-passing philosophy on an immutable platform." The `with*:` methods are analogous to Smalltalk's copy-based patterns (`shallowCopy` + modify). Document common Smalltalk patterns and their BeamTalk equivalents.

### Erlang/BEAM Developer
**Positive:** This is how they already think. Immutable data, processes for state, `gen_server` for managed state transitions. BeamTalk becomes a natural Smalltalk-syntax skin over BEAM idioms rather than an impedance mismatch.

**Neutral:** The value/actor split pairs naturally with ADR 0043's sync/async messaging (`.` for call, `!` for cast). BEAM developers will find both familiar.

### Operator / Production User

**Positive — Reasoning about production systems becomes dramatically simpler.**

Immutable data means no race conditions on shared state. Actor state transitions are serialized through the gen_server mailbox. When something goes wrong at 3am, the operator knows: the bug is in an actor's state transition, and the state at the time of the crash is in the crash dump, fully inspectable. There's no hidden aliasing, no "which reference mutated this object," no shared mutable state between processes. The failure domain is always a single actor.

**Positive — Observability is native.**

Since actors are gen_servers, standard BEAM tooling works out of the box: `sys:get_state/1` to inspect any actor's current state, `observer` to see process trees and message queues, `recon` for production profiling. Value objects are plain maps/tuples — they show up legibly in crash dumps, trace output, and log messages without decoding. There are no compiler-generated `StateAcc` maps or hidden state-threading artifacts to decode.

**Positive — Operational semantics match BEAM idioms.**

Supervision trees, hot code reloading, and distributed Erlang all assume immutable data flowing between processes. This ADR makes BeamTalk a natural citizen of the BEAM ecosystem rather than a special case. OTP patterns (gen_server, gen_statem, supervisor) apply directly. The operator's existing BEAM mental model transfers without translation.

**Positive — Team onboarding from the BEAM ecosystem.**

If hiring from the Erlang/Elixir pool — the natural talent pool for a BEAM language — immutability is not a learning curve, it's the *absence* of one. BEAM developers already think in accumulators, folds, and process state. `inject:into:` is `lists:foldl` with Smalltalk syntax. `collect:` is `lists:map`. They'd be more confused by mutable semantics on BEAM than by immutability — "wait, this variable changes? On BEAM? How?" Immutability makes BeamTalk a Smalltalk-syntax gateway into the BEAM for operators who already have BEAM teams, rather than a Smalltalk that happens to run on BEAM. The addition of `self.slot :=` and variable rebinding gives these developers an ergonomic shorthand they can use or ignore — the functional model they know is still there underneath.

**Concern — Team onboarding from outside the BEAM ecosystem.**

With local variable rebinding (ADR 0041) and `self.slot :=` for actors (this ADR), the onboarding curve is significantly reduced. Developers from Python/Ruby/JS can write `result := result + each` in blocks and `self.count := self count + 1` in actor methods — familiar imperative patterns that compile to functional state threading under the hood. The purely functional style (`inject:into:`, `with*:` copies) remains available and idiomatic, but is no longer the *only* way. This is a gentler on-ramp than Elixir offered — Elixir required learning `Enum.reduce` with no mutable escape hatch, and still succeeded in hiring from the Ruby community.

**Concern — Error messages at the immutability boundary.**

When a developer tries `self x := 5` in a value type method, the error message must be exceptional. Not just "cannot assign to slot on value type" but guidance: "use `self withX: 5` to create a new instance with the updated slot." Poor error messages at this boundary would turn a design decision into a daily frustration for the team.

**Concern — Library ecosystem.**

With `self.slot :=` for actors and `with*:` for values, library authors have clear idioms for both mutable and immutable patterns. The risk of "poorly-adapted mutable-Smalltalk ports" is reduced — actor libraries can use familiar assignment syntax, while value-type libraries naturally compose through functional copies. The remaining concern is consistency: will the ecosystem converge on idiomatic patterns, or will some libraries use `self.slot :=` where `with*:` chains would be cleaner (and vice versa)? Style guides and stdlib examples will matter more than language enforcement here.

## Steelman Analysis

### Best Argument for Alternative 1: Continue with State Threading (Status Quo)

| Cohort | Their strongest argument |
|--------|------------------------|
| **Newcomer** | "I can't even write a simple accumulator loop? `result := 0. items do: [:each \| result := result + each]` is the first thing I'd try." — **Addressed:** ADR 0041 rebinding makes this work. Newcomers can write imperative accumulation patterns; functional style (`inject:into:`) is available but not forced. |
| **Smalltalk purist** | "Mutable instance variables are fundamental to Smalltalk's 50-year history. The Visitor, Builder, Observer patterns all assume mutable state." — **Largely addressed:** `self.slot :=` in actors provides familiar mutable-feeling instance variables. Value types are the only departure from Smalltalk convention, and `with*:` is a reasonable functional equivalent. Classic mutable patterns work naturally in Actor subclasses. |
| **BEAM veteran** | "ADR 0041 is working. Self-sends need dependency analysis, not language restrictions. Why restrict instead of solving the compiler problem?" — **Addressed differently:** We didn't restrict — we gave both `self.slot :=` (actor sugar over functional state) AND rebinding (ADR 0041). The compiler does the state threading; the developer writes natural code. |
| **Language designer** | "You're confusing 'hard to compile' with 'wrong design.' Kotlin and Swift solve harder problems on harder platforms." — **Agreed and addressed:** `self.slot :=` is exactly the kind of compiler sugar they advocate — imperative syntax, functional implementation. The compiler handles `StateN` threading transparently. |
| **Operator** | "If hiring from outside the BEAM world, 'you can't mutate anything' is a hard sell." — **Addressed:** Actors feel mutable (`self.slot :=`), rebinding works in blocks, only value types enforce immutability — which BEAM hires already expect. The hiring story is "Smalltalk that feels natural" not "Erlang with different syntax." |

### Best Argument for Alternative 3: Full Immutability, No Local Rebinding (Gleam-Style)

| Cohort | Their strongest argument |
|--------|------------------------|
| **Language designer** | "Rebinding is a half-measure. It *looks* like mutation but isn't — that's confusing, not ergonomic. Gleam proves you can succeed on BEAM with strict immutability. If you allow `x := x + 1`, newcomers will expect `self x := 5` to work too. Draw the line clearly: nothing mutates, everything is a new value. The collection protocol handles accumulation." |
| **BEAM veteran** | "Single-assignment is the Erlang way. Every BEAM developer already thinks in accumulators and folds. Rebinding adds compiler complexity (ADR 0041 state threading through blocks) for a feature your core audience doesn't need. Keep it simple — `inject:into:` is just `lists:foldl`." |
| **Compiler engineer** | "Without rebinding, you don't need ADR 0041's state threading at all — blocks are pure closures with zero overhead. No `StateAcc`, no pack/unpack, no tuple returns. The compiler is maximally simple. Every line of state-threading code is a line that can have bugs." |

### Best Argument for Alternative 4: Functional-Only Actor State (with\*: chains only)

| Cohort | Their strongest argument |
|--------|------------------------|
| **Language designer** | "Allowing `self.slot :=` in actors but not value types creates two mental models for state. A developer has to remember which kind of class they're in before knowing whether assignment works. Uniform `with*:` everywhere is one rule: 'everything is a new value.' That's simpler to teach, simpler to reason about, and avoids the slippery slope toward mutable-everything." |
| **BEAM veteran** | "Erlang gen_servers are pure functional state machines — `handle_call(Msg, From, State) -> {reply, Reply, NewState}`. That's the BEAM way. Introducing `self.slot :=` inside actors is syntactic sugar that hides the functional reality. When debugging, the developer will see state variable renaming in the Core Erlang output anyway. Leaky abstraction." |
| **Compiler engineer** | "Direct `self.slot :=` inside actor methods means the compiler needs to track which slots have been modified and generate the correct `StateN` chain. That's a simpler form of state threading, but it's still state threading. With `with*:` only, the compiler doesn't need to track slot assignments at all — each `with*:` is a self-send and the dispatch protocol handles state. The compiler is dumber and that's good." |

### Best Argument for Alternative 2: Process-Per-Object

| Cohort | Their strongest argument |
|--------|------------------------|
| **Smalltalk purist** | "This is the only alternative that actually preserves Smalltalk's object model. Every object has identity, state, and behavior. Message sends are message sends, not function calls. Alan Kay said it's about the messages — and this is the only design where every message is actually a message." |
| **Language designer** | "Erlang already proved that millions of lightweight processes work. The overhead argument is about today's BEAM, not tomorrow's. If you optimize process creation (pooling, pre-allocation), you could have true Smalltalk semantics on BEAM. This is the ambitious choice." |

### Why This ADR's Approach Wins Despite the Steelmans

**Against Alternative 1 (Status Quo):** The BEAM veteran's argument is the strongest — ADR 0041 is working, why not finish? However:

1. **Block composability was the bounded problem. What remains is unbounded.** ADR 0041 solved blocks — a well-defined scope with clear calling conventions. Self-send dependency analysis, conditional mutation reconciliation, and deep call-chain propagation are fundamentally harder because they involve inter-method, inter-class, and runtime-polymorphic code paths. Each new language feature (async, pattern matching, etc.) multiplies the state-threading surface area.

2. **The Kotlin/Swift analogy breaks down.** Those languages have mutable memory — the compiler transforms are source-to-source within a mutable execution model. On BEAM, every "mutation" is a data structure copy. The compilation target is fundamentally different, not just harder.

3. **ADR 0041's investment is preserved and active.** The implementation continues to serve local rebinding and actor slot assignments through blocks — the bounded problem it was designed for. The unbounded problems (cross-method self-send chains, cross-class state propagation, deep call chains) are eliminated for value types (immutable) and bounded for actors (gen_server method boundary).

**Against Alternative 3 (Full Immutability, No Rebinding):** The consistency argument is decisive. The REPL must allow rebinding for interactive exploration. If compiled code prohibits rebinding, code cannot move between REPL and class definitions without rewriting. Elixir faced this exact choice and chose rebinding everywhere — it was essential to the Ruby-to-BEAM transition. BeamTalk faces the same transition with Smalltalk developers and must make the same choice. Additionally, ADR 0041's state-threading infrastructure is already implemented, making local rebinding through blocks a solved problem with near-zero marginal cost.

**Against Alternative 4 (with\*:-only actors):** The language designer's "two mental models" concern is the strongest argument. However, the two models reflect a genuine semantic difference: value types ARE immutable data, actors ARE mutable processes. Using the same syntax (`with*:`) for both hides a real distinction — on a value type, `withX: 5` creates a new object; on an actor, it mutates state and returns a value. That's the same syntax with different semantics, which is arguably worse than different syntax for different semantics. The compiler engineer's argument is technically correct but misses the safety point: `with*:` chains have a silent data-loss footgun where uncaptured intermediates discard state updates. The state-tracking for `self.slot :=` is strictly simpler than tracking `with*:` self-sends through dispatch — it's sequential `maps:put`, not dispatch round-trips. The BEAM veteran's "leaky abstraction" concern is valid but applies equally to `with*:` — both compile to state variable renaming in Core Erlang. The question is which surface syntax leads to fewer bugs, and `self.slot :=` wins by eliminating the uncaptured-intermediate footgun entirely.

**Against Alternative 2 (Process-Per-Object):** The Smalltalk purist's argument is philosophically compelling but practically disqualifying. `Point x: 3 y: 4` allocating a process is a ~1000x overhead for the most common operation in any program. No optimization can close that gap — processes require mailboxes, reduction counting, and GC roots. This is not a performance concern; it's a category error.

### Tension Points

- **Smalltalk fidelity vs. platform alignment.** Reasonable people disagree on whether BeamTalk should prioritize "feels like Smalltalk" or "works naturally on BEAM." This ADR chooses a pragmatic middle: actors support `self.slot :=` (Smalltalk-familiar), value types are immutable (BEAM-native). The strongest counter: "if `self x := 5` works in actors but not value types, you've created a confusing inconsistency — in Smalltalk, all objects work the same way."

- **"Smalltalk-inspired" vs. "Smalltalk."** This ADR moves BeamTalk from "Smalltalk on BEAM" toward "Smalltalk-inspired language on BEAM." That's a positioning shift, not just a technical one. It affects marketing, documentation, community expectations, and which developers the language attracts. The Erlang/Elixir community may find it more natural; the Smalltalk community may find it alienating. Actor `self.slot :=` softens this — actors feel like Smalltalk objects, and value types feel like Erlang terms with Smalltalk syntax.

- **Where rebinding ends and mutation begins.** Local rebinding is semantically immutable (shadowing), but it *looks* like mutation (`x := x + 1`). This is a feature for onboarding (familiar syntax) but a potential source of confusion when developers expect reference semantics. The key distinction — rebinding a local doesn't affect other references to the old value — must be taught clearly.

- **ADR 0041 scope: asset, not burden.** ADR 0041's state-threading infrastructure handles the remaining mutable-semantics needs: local rebinding through blocks and actor slot assignments through blocks. It no longer needs to solve cross-method self-send chains, cross-class state propagation, or deep call-chain threading. This is a significant scope reduction — from "thread all mutable state everywhere" to "thread local rebindings and actor slots through blocks within a single method body" — which makes the infrastructure easier to maintain and less likely to produce edge-case bugs.

## Alternatives Considered

### Alternative 1: Continue with Compile-Time State Threading (Status Quo)

Maintain the current compiler strategy. ADR 0041's universal state-threading protocol is largely implemented, and block composability for HOMs is working. Continue extending state threading to cover remaining edge cases (self-send chains, conditional mutations, deep call chains).

**Rejected because:** While ADR 0041 solved block composability, the remaining state-threading problems are fundamentally harder. Self-send dependency analysis, conditional mutation reconciliation, and deep call-chain propagation have produced a steady stream of edge cases (BT-868, BT-900, BT-904, BT-894). These aren't implementation bugs — they're inherent complexity from emulating mutable semantics on an immutable platform. The ADR 0041 infrastructure remains valuable (and is retained), but extending it to cover full mutable-object semantics would be an open-ended commitment.

### Alternative 2: Process-Per-Object

Every object is a BEAM process. Mutation is process state. Method calls become gen_server calls.

**Rejected because:** Catastrophic overhead for fine-grained objects. A `Point` shouldn't spawn a process. Loses synchronous method composition. Makes `2 + 3` into two gen_server calls.

### Alternative 3: Immutable Objects, No Local Rebinding (Gleam-Style Full Immutability)

Keep objects immutable **and** prohibit local variable rebinding. Accumulation patterns require functional combinators only:

```smalltalk
"The only way to accumulate:"
result := items inject: 0 into: [:sum :each | sum + each].

"This would be a compiler error:"
result := 0.
items do: [:each | result := result + each].   "ERROR — cannot rebind local"
```

**Rejected because:** This creates an inconsistency with the REPL, which must allow rebinding for interactive exploration. Code that works in the REPL would break when moved to a class definition — a pedagogical cliff that Elixir deliberately avoided and that BeamTalk cannot afford. Additionally, ADR 0041's state-threading infrastructure is already implemented, making local rebinding through blocks a solved problem with near-zero additional compiler cost. The purity gain is not worth the consistency loss.

### Alternative 4: Functional-Only Actor State (with\*: chains, no self.slot :=)

Require actors to express all state transitions through auto-generated `with*:` functional setters. Prohibit `self.slot :=` inside actor methods — all state updates use the same `with*:` pattern as value types:

```smalltalk
"with*: only — multi-step update requires intermediate variable:"
deposit: amount =>
  | s1 |
  s1 := self withBalance: self balance + amount.
  s1 withTransactions: (s1 transactions add: amount)
```

**Rejected because:** The `with*:` chain pattern for actors has a silent data-loss footgun: forgetting to capture an intermediate variable (`self withBalance: ...` instead of `s1 := self withBalance: ...`) silently discards the state update with no compile-time or runtime error. This violates the goal of eliminating hidden complexity and surprising edge cases. Additionally, `self withSlot: expr` on an actor is a self-send through dispatch — it returns `{reply, Result, NewState}` and requires tuple unpacking — which is slower than `self.slot := expr` compiling to a direct `maps:put`. The functional purity buys nothing for actors: the gen_server already serializes state transitions, so there is no concurrency benefit from treating methods as pure functions. Direct slot assignment (`self.slot :=`) is both cheaper (no dispatch) and safer (impossible to lose an update) than `with*:` chains for actor-internal state mutation.

### Alternative 5: Trait/Protocol-Based Mutability

Use a trait or protocol to opt into mutability:

```smalltalk
Value subclass: Point
  uses: Mutable
  state: x = 0
  state: y = 0
```

**Rejected because:** This is just Alternative 4 with extra syntax. The compiler still needs to handle mutable and immutable paths. The complexity isn't eliminated, it's disguised.

### Alternative 6: Immutable by Default with `mutable` Class Modifier

Allow specific value classes to opt into mutable instance variables via a declaration modifier:

```smalltalk
mutable Object subclass: Builder
  state: parts = List new

  addPart: part => self.parts := self.parts add: part
```

This would provide a migration path for Smalltalk patterns that genuinely require mutable state without making all value types mutable.

**Rejected because:** Any class using `mutable` reintroduces the full state-threading problem for that class — self-send dependency analysis, conditional mutations, block slot writes. The compiler must maintain dual codegen paths. Composability suffers: a `mutable` value type passed to a function expecting an immutable value creates a Liskov substitution violation — the caller assumes immutability, but the value can change under it. The clean dichotomy (value = immutable, actor = mutable process) is lost, and the complexity budget returns to the status quo for any codebase that uses `mutable` classes.

### Alternative 7: Frozen/Thawed Builder Pattern

Allow a mutable builder phase followed by immutable freeze:

```smalltalk
builder := Point thaw.
builder x: 1.
builder y: 2.
point := builder freeze.   "=> immutable Point x: 1 y: 2"
```

This would be more ergonomic than chained `with*:` calls for constructing objects with many slots.

**Rejected because:** The primary construction path is `new: #{...}` (map-based, any subset of fields) plus the all-fields keyword constructor (`Point x: 1 y: 2`), which together handle the multi-field case directly. For modification of existing objects, `with*:` chains or cascades work and are idiomatic in the functional world (Erlang records, Gleam records, Swift structs). The freeze/thaw pattern adds a new concept (mutable transient objects) that exists only during construction — a narrow use case that doesn't justify the conceptual overhead. If deep nested updates prove painful, lens-like `update*:` methods (noted as a future enhancement) are a better solution.

## Consequences

### Positive

- **Eliminates compile-time state threading for value objects entirely.** Value objects are immutable data — they map directly to BEAM terms. No `StateAcc`, no pack/unpack, no dual codegen paths.
- **Dramatically simplifies actor state codegen.** Actor `self.slot :=` compiles to direct `maps:put` — no dispatch round-trip, no `{reply}` tuple unpacking. Self-send dependency analysis is eliminated: sequential slot assignments are sequential `maps:put` calls with explicit state variable threading.
- **Eliminates the with\*: intermediate-capture footgun for actors.** Direct slot assignment makes it impossible to silently lose a state update by forgetting to capture an intermediate variable.
- **Blocks are safe and composable.** Slot assignments in actor blocks use ADR 0041's state-threading protocol (same as local rebinding). Value type blocks have no mutation concern. HOMs work universally.
- **Higher-order messages work universally.** Blocks can be freely passed, stored, and executed. Local rebinding through blocks uses ADR 0041's universal protocol — no whitelist, no special cases.
- **Concurrency is natural.** Immutable data on the BEAM is the happy path — no copying overhead, no race conditions.
- **Enables advanced runtime features.** Immutable state enables checkpoint, fork, replay, and time-travel debugging. Actor state transitions can be modeled as event sourcing.
- **Compiler simplification** allows focus on differentiating features: liveness, image-based development, agentic AI runtime.
- **Complements ADR 0043** (Sync-by-Default Actor Messaging) — the value/actor model pairs naturally with `.` (call) and `!` (cast) semantics.

### Negative

- **Two state models.** Value types prohibit `self.slot :=` while actors allow it. This is a genuine semantic split — in Smalltalk, all objects work the same way. Developers must know whether they're in a Value or Actor class before knowing whether slot assignment works. The `Value`/`Actor` superclass makes this visible at the declaration site, but it's still a rule to learn.
- **Potential verbosity for nested value updates.** Deep state updates on nested value objects require `with*:` chains: `person withAddress: (person address withCity: 'Portland')`. Lens-like patterns may be needed for ergonomics.
- **Breaking change from current behavior.** Existing value type code that uses mutable instance variables must be rewritten to use `with*:` functional setters. Actor code using `self.slot :=` is unaffected.
- **ADR 0041 remains exercised.** Local rebinding inside blocks and actor slot assignments inside blocks both require state threading. The compiler is simpler than the status quo (no cross-method or cross-class state threading), but not as simple as full immutability would have been.
- **Live patching of value object classes requires migration protocol.** Value objects have no process, so there is no `code_change` callback. An existing value object in the system after a class definition change (e.g., new slot added) is an instance of the old layout. For image-based development, a value migration protocol (analogous to database schema migrations) may be needed. This is a known constraint inherited from Erlang's data model — Erlang records have the same issue — and should be addressed in the image-based development ADR.
- **Block escape at value/actor boundary.** A block that captures rebindable locals and is passed to an actor as a callback may execute asynchronously. ADR 0041's state threading works synchronously — the `StateAcc` is threaded through the call chain. If a block escapes to an async context, the captured state snapshot is frozen at escape time. This is the same semantics as Elixir closures (capture by value), and is correct, but may surprise developers who expect the rebinding to propagate across async boundaries.

### Neutral

- **ADR 0041 remains active and essential.** ADR 0041's universal state-threading protocol is largely implemented and continues to serve its purpose: threading local variable rebindings and actor slot assignments through blocks. The scope is narrower than under the status quo — no cross-method or cross-class state threading — but the infrastructure is exercised by any block that rebinds a captured local or assigns to an actor slot. This is exactly the bounded problem ADR 0041 was designed for.
- **ADR 0005 alignment.** ADR 0005 (BEAM Object Model) already distinguishes value types from actors. This ADR formalizes the immutability constraint that was implicit in ADR 0005's design.
- **Collection mutation methods return new collections.** `add:`, `remove:`, `addAll:` return the new collection, not the affected element — a deliberate departure from Smalltalk convention (where `OrderedCollection>>add:` returns the added element), consistent with immutable value semantics. This is already implemented in the stdlib (`List>>add:` returns a new `List`; ADR 0037).
- **Collection protocol completeness is important but not critical.** With local rebinding, developers can always fall back to `do:` with accumulator rebinding. However, `inject:into:`, `collect:`, `select:`, `reject:`, and `detect:` should still be comprehensive — idiomatic functional patterns are cleaner than imperative accumulation and should be the encouraged style.
- **Auto-generated `with*:` methods are load-bearing.** Without them, immutability is tedious boilerplate. The compiler must generate them reliably for all declared slots.
- **ADR 0043 leverages Value/Actor classification.** The compile-time and runtime guards for `!` (cast) on value types are defined in ADR 0043, using the Value/Actor classification introduced here.

## Implementation

### Phase 0: Wire Check (Proof of Concept)

- Compile one `Value subclass:` declaration with auto-generated getters and `with*:` methods
- Verify the generated Core Erlang compiles and produces correct output
- Compile one `Actor subclass:` with `self.slot :=`
- Verify `self.slot :=` compiles to `maps:put` and gen_server state management works
- **Goal:** Prove the core codegen patterns work before rewriting all class compilation
- **Effort:** S

### Phase 1: Language Model Changes (M)

**Lexer/Parser:**
- `!` (bang) token and cast syntax are specified in ADR 0043 (Sync-by-Default Actor Messaging)

**Parser:**
- Support `Value subclass:` as a new declaration form (today value types use `Object subclass:`)
- Reject instance variable assignment (`self.slot :=`) in value type methods with clear error message guiding toward `self withSlot:`. Allow `self.slot :=` in actor methods.

**Semantic Analysis:**
- Classify classes as Value or Actor based on declaration keyword (replacing the current `is_actor_class()` heuristic that walks the superclass chain)
- Verify no slot mutation (`self.slot :=`) in value object methods
- `!` receiver validation is specified in ADR 0043
- Update block mutation analysis (`block_analysis.rs`) to distinguish "slot write on value type" (prohibited) from "slot write on actor" (allowed)
- Warn on `self.slot :=` when `withSlot:` has been overridden anywhere in the class hierarchy (walk `ClassHierarchy` to check for user-defined `with*:` methods)

### Phase 2: Codegen Changes (L)

**Value Object Codegen:**
- Generate immutable map-based objects from `Value subclass:` + `state:` declarations
- Auto-generate getters and `with*:` functional setters for each declared slot
- Auto-generate all-fields keyword constructor (one per class)
- Remove slot-write codegen for value types (no `self.slot :=` → `maps:put` paths)
- **Affected:** `value_type_codegen.rs`, `mod.rs`, `class_hierarchy/mod.rs`

**Actor Codegen:**
- Generate gen_server module with `handle_call`/`handle_cast` callbacks
- Call/cast routing (`.` vs `!`) is specified in ADR 0043
- Compile `self.slot := expr` to the same slot-write operation as `with*:` (today: `StateN = maps:put(slot, Expr, StateN-1)`), but without dispatch
- Actor methods receive state map, return `{reply, LastExpr, FinalState}`
- Auto-generate getters and `with*:` as public API for actor state
- **Affected:** `actor_codegen.rs`, `mod.rs`, `beamtalk_dispatch.erl`

### Phase 3: Runtime Changes (M)

- Call/cast dispatch routing is specified in ADR 0043
- Update actor framework to interpret method return values (new state vs early return via `^`)
- Add `pid` method to `Actor` base class (extracts pid from `#beamtalk_object{}` record)
- Verify REPL binding semantics are preserved (workspace rebinding still works)
- **Affected:** `beamtalk_dispatch.erl`, `beamtalk_actor.erl`, `beamtalk_repl_shell.erl`, `Actor.bt`

### Phase 4: Migration and Testing (L)

- Update all existing stdlib classes to Value/Actor model (`Object subclass:` → `Value subclass:` where appropriate)
- Replace `self.slot :=` patterns with `self withSlot:` patterns in **value type** methods (actors retain `self.slot :=`)
- Comprehensive error message testing for rejected patterns (`self.slot :=` on value types)
- Verify all existing stdlib and e2e tests pass
- Performance benchmarks for value object creation and `with*:` operations

## Migration Path

### Existing Value Types

Current value types (Point, Color, etc.) already behave mostly as immutable values. Migration involves:
1. Change class declaration from `Object subclass:` to `Value subclass:`
2. Remove any setter methods (replaced by auto-generated `with*:`)
3. Replace `self x := val` with return of `self withX: val`

### Existing Actors

Current actors already use gen_server. Migration is minimal:
1. Change class declaration to `Actor subclass:` with `state:`
2. Existing `self.slot :=` patterns continue to work — no rewrite needed

### Existing Tests

Value type tests using instance variable assignment (`self.slot := value`) must be rewritten. Actor tests using `self.slot :=` are unaffected. Local variable rebinding continues to work, so many test patterns are unaffected. A migration guide with common pattern translations should accompany this change:

| Current Pattern | New Pattern | Notes |
|---|---|---|
| `self.count := self.count + 1` (in Value) | `self withCount: self count + 1` | Value type → functional setter |
| `self.x := val. self.y := val2` (in Value) | `(self withX: val) withY: val2` | Value type sequential → chain |
| `self.value := self.value + delta` (in Actor) | Unchanged — `self.slot :=` allowed in actors | No migration needed |
| `Object subclass: Point` | `Value subclass: Point` | Superclass declaration change |
| `result := 0. items do: [:each \| result := result + each]` | Unchanged — local rebinding still works | Or use `inject:into:` for idiomatic style |

## References

### Related ADRs
- ADR 0005 (BEAM Object Model) — formalized by this ADR; value type immutability made explicit
- ADR 0006 (Unified Method Dispatch) — `is_actor_class()` heuristic replaced by explicit `Value`/`Actor` declaration
- ADR 0013 (Class Variables, Class-Side Methods, Instantiation) — `state:` and `classState:` keywords retained; `Value subclass:` introduced as new declaration form
- ADR 0025 (Gradual Typing) — gradual typing will improve static coverage over time but is not a prerequisite for Value/Actor classification
- ADR 0028 (BEAM Interop) — immutability is compile-time only, not enforced at BEAM boundary
- ADR 0035 (Field-Based Reflection API) — `fieldAt:put:` on value types raises `#beamtalk_error{type => immutable_value}` at runtime (closes the deferred access control question); `fieldAt:`/`fieldNames` API unchanged
- ADR 0038 (Subclass/ClassBuilder Protocol) — ClassBuilder must recognize `Value` as a root class
- ADR 0037 (Collection Class Hierarchy) — collection classes (List, Array, Dictionary, Range) must be classified as Value or Actor; value-type collections using `self.slot :=` need migration to `with*:` functional setters
- ADR 0039 (Syntax Pragmatism vs Smalltalk) — cascade semantics deferred to ADR 0044; ADR 0039's fan-out semantics remain unchanged until then
- ADR 0044 (Cascade-as-Pipeline, future) — pipeline cascade (`;` chains on return value) is the natural builder pattern for immutable values. **Key design tension:** pipeline cascade does not rebind the original variable, so `p := Point new. p; withX: 5; withY: 4.` leaves `p` unchanged — unlike actors where the PID is stable and state mutates in-place. This means pipeline cascade is most powerful inside **blocks and higher-order messages** where the result flows through without rebinding: `points collect: [:p | p; withX: 5; withY: 4]`. ADR 0044 should evaluate cascade semantics primarily in the context of blocks, HOMs, and expression-level construction — not as a mutation substitute.
- ADR 0043 (Sync-by-Default Actor Messaging) — `.` vs `!` call/cast syntax split out from this ADR into its own focused decision
- ADR 0040 (Workspace-Native REPL Commands) — REPL binding semantics preserved
- ADR 0041 (Universal State-Threading Block Protocol) — scope narrowed to local rebinding and actor slot assignments through blocks; remains active and essential

### External References
- Alan Kay on Erlang: acknowledgment that Erlang captures the message-passing spirit of Smalltalk
- Clojure persistent data structures: immutable-by-default with controlled state via atoms/refs
- Gleam: immutable-by-default language on BEAM, strict immutability validated at scale
- Elixir: rebinding as ergonomic immutability, successful Ruby-to-BEAM developer transition
- Swift value types: struct vs class distinction, `mutating` keyword as analogous pattern
- Erlang/OTP gen_server: functional state transition model adopted for actors
- Pony reference capabilities: value vs identity distinction at the type level

### Related Issues
- BT-506 — pipeline operator; cascade-as-pipeline candidate deferred to ADR 0044
- BT-868, BT-900, BT-904, BT-894 — state-threading edge cases that motivated this decision
