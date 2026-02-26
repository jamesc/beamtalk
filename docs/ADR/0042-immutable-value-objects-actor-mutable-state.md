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

Adopt **immutable value semantics** as the default for all BeamTalk objects. Mutable state is confined to **actor processes** (BEAM processes / OTP gen_servers).

### Two Kinds of Entities

1. **Value Objects** — Immutable. Every "mutating" message returns a new object. These compile directly to BEAM terms (maps, tuples, lists) with no transformation.

2. **Actors** — BEAM processes wrapping a gen_server. The only place where mutable state exists. State transitions are expressed functionally (methods return new state), but the process itself maintains identity over time.

### Value Object Semantics

#### Declaration and Construction

Value objects are declared with `slots:` and constructed via auto-generated keyword constructors:

```smalltalk
Value subclass: Point
  slots: x = 0, y = 0

point := Point x: 3 y: 4.
```

**Syntax note:** Today, value types use `Object subclass:` with `state:` declarations. This ADR renames `state:` to `slots:` as the universal slot declaration keyword for both value types and actors, and introduces `Value subclass:` as the explicit declaration form. `Value` becomes a new root class in the hierarchy between `Object` and user value classes.

The `slots:` keyword aligns with Smalltalk tradition (Pharo's Slot framework) and is neutral on mutability — a slot is a named position in an object, whether immutable (value types) or mutable (actors). The `Value` vs `Actor` superclass determines immutability semantics. The reflection API is renamed to match: `slotAt:`, `slotAt:put:`, `slotNames` (superseding ADR 0035's `fieldAt:`, `fieldAt:put:`, `fieldNames`).

**Class-level slots:** The existing `classState:` keyword is renamed to `classSlots:` for vocabulary consistency. Class slots are mutable (they live in the class object's gen_server state), regardless of whether the class is a Value or Actor type:

```smalltalk
Value subclass: Point
  classSlots: instanceCount = 0
  slots: x = 0, y = 0

  class create: x y: y =>
    self.instanceCount := self.instanceCount + 1.
    self x: x y: y
```

#### Functional Updates via `with*:`

"Mutating" messages return a new object. The compiler auto-generates `with*:` methods for each slot:

```smalltalk
point := Point x: 3 y: 4.
point2 := point withX: 5.       "Returns new Point(5, 4). point is unchanged."

list := List new.
list2 := list add: 'hello'.     "Returns new List containing 'hello'."
```

For a value class with declared slots, the compiler generates:

- `name`, `age`, `email` — getters
- `withName:`, `withAge:`, `withEmail:` — functional setters (return new object with that slot changed)
- `ClassName slot1: val1 slot2: val2 ...` — keyword constructor

Under the hood, these compile to trivial Erlang map operations. For **value types**, where `Self` is the map directly:

```erlang
%% getter (value type — Self is the map)
'x'/1 = fun (Self) -> call 'maps':'get'('x', Self)

%% functional setter — preserves all slots, including subclass slots
'withX:'/2 = fun (Self, NewX) -> call 'maps':'put'('x', NewX, Self)
```

For **actors**, the same `with*:` methods exist but the dispatch framework mediates: `self` in an actor method is a `#beamtalk_object{}` record (see "self and Actor Identity" below), and `self withValue: 5` is a self-send that goes through `Module:dispatch`, which provides the underlying state map to the method. The generated `with*:` code operates on the state map, not the `#beamtalk_object{}` record.

**Subclass safety:** Since `with*:` compiles to `maps:put` on the existing map, it preserves all slots — including slots added by subclasses. `aColorPoint withX: 5` returns a new map with the same `class`, `color`, `y`, and updated `x`. The result is still a `ColorPoint`, not a `Point`. This works because Erlang maps preserve all keys through `maps:put`. No virtual dispatch or factory method is needed.

**Subclass depth:** Value type subclassing has no artificial depth limit. The map representation handles inheritance naturally — each subclass adds its own slots to the map. Dispatch chains walk up the class hierarchy via `superclass/0` delegation (the existing mechanism). Since maps have O(1) key access regardless of size, deep hierarchies impose no per-slot-access penalty. The practical limit is the same as any Smalltalk class hierarchy — deep hierarchies are a design smell, not a technical constraint.

**Equality:** Value objects use **structural equality**. Two `Point` instances with the same `x` and `y` are equal (`=:=`). This follows naturally from the Erlang map representation — internally `#{'$beamtalk_class' => 'Point', '__class_mod__' => ..., x => 3, y => 4}`, where all metadata keys are identical for instances of the same class, so structural equality reduces to slot-value comparison. For value objects, `==` (loose equality) and `=:=` (strict equality) both compare structurally. Actors use **process identity** — two actors with identical state are different objects if they are different processes.

#### No Instance Variable Assignment

There is no `:=` assignment to instance variables. Period. If you want a modified version of an object, you create a new one:

```smalltalk
"This does not exist:"
self x := 5.        "COMPILER ERROR — no instance variable assignment on value type"

"Instead:"
self withX: 5       "Returns new object"
```

**Reflection API:** `slotAt:put:` on value types raises `#immutable_value` at runtime (renamed from `fieldAt:put:` in ADR 0035). Immutability is enforced both at compile time (`self.slot :=` rejected by semantic analysis) and at runtime (reflection API raises error). The compile-time check catches the common case; the runtime check catches the reflection path.

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

Actors are declared with `slots:` and default values. State is initialized when the actor is spawned:

```smalltalk
Actor subclass: Counter
  slots: value = 0

counter := Counter new.          "Spawns a gen_server process"
```

#### Methods as State Transitions

Actor methods are pure functions on the state map. They express state transitions by returning a new version of the state:

```smalltalk
Actor subclass: Counter
  slots: value = 0

  increment => self withValue: self value + 1
  decrement => self withValue: self value - 1
  getValue  => ^self value
```

The compiler auto-generates the same `with*:`/getter pattern for actor slots as for value object slots.

#### Return Value and State Update Rules

The gen_server framework uses two rules to interpret method results:

1. **Last expression (no `^`):** The result is the reply to the caller. The compiler tracks the current state version through `with*:` chains — the most recent state map derived from `self` becomes the new gen_server state. If the method doesn't derive a new state (pure query), the original state is preserved.

2. **Early return (`^`):** The `^` expression is the reply to the caller. The gen_server state is updated to whatever state version existed at the `^` point — any mutations before `^` are preserved, the early return does not discard them.

Under the hood, the compiler tracks state through the existing `{reply, Result, NewState}` return tuples from self-sends — the same mechanism used today for self-send state threading. When an actor method calls `self withValue: 5`, this is a self-send that goes through `Module:dispatch`, which returns `{reply, NewMap, NewMap}`. The codegen unpacks this: the result goes to the user variable, and `NewState` becomes the next `StateN` in the compiler's state tracking chain. The generated `handle_call` always receives an explicit `{reply, Result, StateN}` tuple. **This is not local-variable analysis** — the compiler does not need to infer which local variable "represents state." State flows through the dispatch return path, exactly as today.

```smalltalk
"State transition — last expression is new state map:"
increment => self withValue: self value + 1
"self withValue: is a self-send → dispatch returns {reply, NewState, NewState}"
"Generated handle_call reply: {reply, NewState, NewState}"

"Query — ^ returns a value, state unchanged:"
getValue => ^self value
"No self-sends that modify state → State is unchanged"
"Generated: {reply, Value, OriginalState}"

"Mixed — mutate then return a non-state value:"
incrementAndGetOld =>
  | old s1 |
  old := self value.
  s1 := self withValue: self value + 1.
  ^old
"self withValue: dispatch returns {reply, NewMap, State1}"
"s1 = NewMap, compiler tracks State1"
"^old throws {$bt_nlr, Token, old, State1} — state updated to State1"
```

**Why this eliminates self-send dependency analysis:** Under the status quo, `self name: 'foo'. self age: 42.` requires the compiler to detect the data dependency between sequential slot mutations. Under ADR 0042, each self-send goes through dispatch and returns `{reply, Result, NewState}` — the state is threaded through the return path automatically. No dependency analysis is needed because each self-send explicitly receives and returns state through the dispatch protocol.

#### Sequential State Updates in Actor Methods

Since actor methods are functions on the state map, sequential updates bind intermediate results:

```smalltalk
Actor subclass: Account
  slots: balance = 0, transactions = List new

  deposit: amount =>
    | s1 |
    s1 := self withBalance: self balance + amount.
    s1 withTransactions: (s1 transactions add: amount)
```

For complex updates, `inject:into:` threads state through iterations:

```smalltalk
  depositAll: amounts =>
    amounts inject: self into: [:state :amount |
      state withBalance: state balance + amount
    ]
```

#### `self` and Actor Identity

Inside an actor method, `self` is a `#beamtalk_object{class, class_mod, pid}` record that carries both **identity** (the process pid) and **state access** (slot reads dispatch to the underlying state map). This is the existing implementation and it naturally solves the "how do I get my pid?" question.

```smalltalk
"Inside the actor method:"
increment => self withValue: self value + 1   "self.value reads from state map"
notifyOther: other => other update: self pid!  "self pid returns the actor's pid"

"Outside — counter is a pid, message goes through gen_server:"
counter increment.
counter notifyOther: logger.
```

The `#beamtalk_object{}` record is the actor's "calling card" — it can be passed to other actors as a reference. From outside, message sends go through `gen_server:call/cast`. From inside (self-sends), the dispatch bypasses gen_server to avoid deadlock, calling `Module:dispatch` directly.

**`self pid`** returns the underlying BEAM process identifier (`erlang:element(4, Self)` on the `#beamtalk_object{}` tuple). This is defined as a method on the `Actor` base class (not auto-generated per-class). It is useful for:
- Passing your reference to another actor (`other register: self pid`)
- Monitoring/linking (`self pid` can be passed to Erlang's `monitor/2`)
- Logging and debugging (pid identifies the actor in crash dumps)

**State map vs `self`:** Internally, the gen_server callbacks receive a plain state map (`State`). The runtime's `make_self/1` wraps it in `#beamtalk_object{}` before each method invocation. Slot reads on `self` dispatch to `maps:get` on the state map. `with*:` methods operate on the state map and return a new state map. The `#beamtalk_object{}` wrapper is transparent — developers work with `self` uniformly.

### Message Send Semantics: Call vs Cast

Actor message sends default to **synchronous calls** (`gen_server:call`), consistent with Smalltalk's synchronous message-send model. Asynchronous casts use `!` as a statement terminator:

```smalltalk
counter increment.                "Synchronous call (gen_server:call)"
counter increment!                "Asynchronous cast (gen_server:cast)"

counter add: 5.                   "Keyword message — sync call"
counter add: 5!                   "Keyword message — async cast"

counter at: key put: value.       "Multi-keyword — sync"
counter at: key put: value!       "Multi-keyword — async cast"
```

**Rule:** `.` (period) terminates a synchronous call. `!` (bang) terminates an asynchronous cast.

**Cast return semantics:** A cast (`!`) has no return value. It is a statement, not an expression. Using a cast in an expression context is a **compiler error**:

```smalltalk
counter increment!                "Valid — fire and forget"
counter increment.                "Valid — sync call, result discarded"
x := counter increment.           "Valid — x receives reply"
x := counter increment!           "COMPILER ERROR — cast has no return value"
```

**Rationale:**
- Smalltalk messages are synchronous. Defaulting to sync is faithful to the mental model.
- Avoids JavaScript-style async/await infection where `Future` propagates through the entire codebase.
- `gen_server:call` is Erlang's default interaction pattern — this aligns with BEAM idioms.
- Explicit `!` makes fire-and-forget semantics visible. No return value, no backpressure.
- `!` echoes Erlang's `!` send operator, providing BEAM familiarity.

**Deadlock avoidance:** Actor-to-actor synchronous calls (`.`) can deadlock if actor A calls actor B while B is calling A — both block waiting for a reply that can never arrive. This is the standard gen_server deadlock pattern. For actor-to-actor coordination, prefer `!` (fire-and-forget) by default. Use `.` only when the caller needs the reply and is not at risk of cyclic waits (e.g., a client calling a service, or an actor calling a stateless value computation). Self-sends within an actor method bypass gen_server entirely (direct dispatch), so they are always safe.

### Cascade Semantics: Pipelines for Value Objects

Smalltalk cascades (`;`) send multiple messages to the same receiver. With immutable objects, the traditional cascade semantics are problematic — each message returns a new object, but the cascade discards intermediate results. `point withX: 5; withY: 10` under Smalltalk semantics sends both messages to the *original* `point`, losing the `withX:` result.

**Decision:** Cascades on value objects use **pipeline semantics** — each subsequent message is sent to the *result* of the previous one, not the original receiver. On actors, cascades retain Smalltalk's traditional fan-out semantics (all messages sent to the same pid).

#### Value Object Cascades (Pipeline Semantics)

```smalltalk
point withX: 5; withY: 10.
"Equivalent to: (point withX: 5) withY: 10"
"Result: Point with x=5, y=10"

"Collection pipelines — this is Elixir's |> for free:"
items select: [:x | x > 0]; collect: [:x | x * 2]; inject: 0 into: [:sum :x | sum + x].
"Equivalent to: ((items select: [:x | x > 0]) collect: [:x | x * 2]) inject: 0 into: [...]"
```

This is faithful to the *intent* of cascades — performing a sequence of operations — while adapting to immutability. In Smalltalk, cascades chain side effects on the same mutable receiver. In BeamTalk, cascades chain transformations, threading each result to the next message. The programming pattern is the same: "do this, then this, then this."

#### Actor Cascades (Traditional Smalltalk Fan-Out)

```smalltalk
logger log: 'starting'; log: 'processing'; log: 'done'.
"Three messages sent to the same actor pid (gen_server:call x3)"

counter increment; increment; increment!
"Two sync increments, then one async cast — all to the same pid"
```

For actors, traditional cascade semantics are correct and useful — the receiver has stable identity (it's a pid), and each message is a side effect on that identity. There's no ambiguity about "which receiver" because the receiver is a process, not a value that might change.

#### Why Type-Dependent Semantics Are Safe Here

The cascade semantics are type-dependent: pipeline on values, fan-out on actors. This is a deliberate design choice, not an accident. The key insight is that the semantics align with what the developer *intends*:

- **Value objects:** The developer chains `with*:` or collection operations to build up a result. Pipeline semantics do what they mean. Traditional fan-out would silently discard intermediate results — a bug, not a feature.
- **Actors:** The developer sends multiple commands to a service. Fan-out does what they mean. Pipeline semantics would thread return values through unrelated messages — nonsensical.

The type-dependency mirrors `;`'s existing behavior: even in Smalltalk, cascades interact with the receiver's semantics (some messages return `self`, others don't). BeamTalk makes the rule explicit and predictable: values pipeline, actors fan out.

**This gives BeamTalk pipelines (BT-506).** The `;` cascade operator on value objects is functionally equivalent to Elixir's `|>` pipeline operator. No new syntax is needed — the existing cascade syntax naturally becomes the pipeline operator when applied to immutable values.

#### Dynamic Receivers and Mid-Chain Type Changes

For statically-known receivers, the compiler emits pipeline (value) or fan-out (actor) code directly. For dynamically-typed receivers (method parameters, collection elements), the generated code checks `is_pid(Receiver)` **once at cascade entry** and selects either pipeline or fan-out for the entire chain:

- **Pipeline (value receiver):** each message is sent to the result of the previous step
- **Fan-out (actor/pid receiver):** all messages are sent to the original receiver

The check happens once because the receiver type is consistent within a cascade — a value object cascade produces values, an actor cascade operates on the same pid. Mid-chain type changes (e.g., a value method returning an actor) would be unusual and are not supported within a single cascade; use explicit chaining with `.` for cross-type sequences.

**Why fan-out is required for actors:** `gen_server:call` returns the method's reply value (e.g., the new state map), not the actor's pid. Pipeline on `counter increment; getValue` would send `getValue` to the state map (a plain Erlang map), not to the counter actor. Fan-out ensures all messages reach the same pid.

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

**Neutral:** The `.` vs `!` for call/cast maps directly to `gen_server:call` vs `gen_server:cast`. Familiar.

### Operator / Production User

**Positive — Reasoning about production systems becomes dramatically simpler.**

Immutable data means no race conditions on shared state. Actor state transitions are serialized through the gen_server mailbox. When something goes wrong at 3am, the operator knows: the bug is in an actor's state transition, and the state at the time of the crash is in the crash dump, fully inspectable. There's no hidden aliasing, no "which reference mutated this object," no shared mutable state between processes. The failure domain is always a single actor.

**Positive — Observability is native.**

Since actors are gen_servers, standard BEAM tooling works out of the box: `sys:get_state/1` to inspect any actor's current state, `observer` to see process trees and message queues, `recon` for production profiling. Value objects are plain maps/tuples — they show up legibly in crash dumps, trace output, and log messages without decoding. There are no compiler-generated `StateAcc` maps or hidden state-threading artifacts to decode.

**Positive — Operational semantics match BEAM idioms.**

Supervision trees, hot code reloading, and distributed Erlang all assume immutable data flowing between processes. This ADR makes BeamTalk a natural citizen of the BEAM ecosystem rather than a special case. OTP patterns (gen_server, gen_statem, supervisor) apply directly. The operator's existing BEAM mental model transfers without translation.

**Positive — Team onboarding from the BEAM ecosystem.**

If hiring from the Erlang/Elixir pool — the natural talent pool for a BEAM language — immutability is not a learning curve, it's the *absence* of one. BEAM developers already think in accumulators, folds, and process state. `inject:into:` is `lists:foldl` with Smalltalk syntax. `collect:` is `lists:map`. They'd be more confused by mutable semantics on BEAM than by immutability — "wait, this variable changes? On BEAM? How?" Immutability makes BeamTalk a Smalltalk-syntax gateway into the BEAM for operators who already have BEAM teams, rather than a Smalltalk that happens to run on BEAM.

**Concern — Team onboarding from outside the BEAM ecosystem.**

If hiring from Python/Ruby/JS backgrounds, the accumulation pattern learning curve is real. `inject:into:` instead of `result := result + each` is unfamiliar. However, this is the same curve that Elixir successfully navigated — Elixir hired heavily from the Ruby community and those developers learned `Enum.reduce` without the language collapsing. The question is whether BeamTalk's target audience is "Smalltalk developers who want BEAM" or "BEAM developers who want Smalltalk syntax." For the latter — the more likely early adopter pool — immutability is a feature, not a tax.

**Concern — Error messages at the immutability boundary.**

When a developer tries `self x := 5` in a value type method, the error message must be exceptional. Not just "cannot assign to slot on value type" but guidance: "use `self withX: 5` to create a new instance with the updated slot." Poor error messages at this boundary would turn a design decision into a daily frustration for the team.

**Concern — Library ecosystem.**

If BeamTalk libraries are written by people who understand the immutable model, they'll be clean and composable. But if the ecosystem is small and libraries are ported from mutable-Smalltalk patterns, the operator may encounter poorly-adapted code that works around immutability in awkward ways. The quality of the ecosystem matters more than the quality of the core language for production use.

## Steelman Analysis

### Best Argument for Alternative 1: Continue with State Threading (Status Quo)

| Cohort | Their strongest argument |
|--------|------------------------|
| **Newcomer** | "I can't even write a simple accumulator loop? `result := 0. items do: [:each \| result := result + each]` is the first thing I'd try. Every intro tutorial for every language starts with mutable accumulators. Forcing `inject:into:` for basic operations is hostile to beginners — you're making me learn a new concept before I can count things." |
| **Smalltalk purist** | "This isn't Smalltalk anymore. Mutable instance variables are fundamental to the language's 50-year history. The Visitor pattern, the Builder pattern, the Observer pattern — they all assume mutable state. If I wanted Erlang semantics, I'd write Erlang. You've renamed Erlang processes 'actors' and called it a Smalltalk." |
| **BEAM veteran** | "ADR 0041 is already implemented and working. You've solved the hardest part — block composability for HOMs. Self-send chains and conditional mutations are harder, but they're bounded problems with known solutions (SSA, CPS transforms). You're one compiler pass away from full mutable semantics. Why throw away months of working infrastructure to restrict the language instead?" |
| **Language designer** | "You're confusing 'hard to compile' with 'wrong design.' Kotlin compiles mutable closures to Ref-boxing on the JVM. Swift compiles `mutating struct` methods to in-place COW. These are harder platforms than BEAM for this problem, and they solved it. The state-threading approach works — ADR 0041 proves it. Self-sends need dependency analysis, not language restrictions." |
| **Operator** | "I'm going to deploy this in production and my team needs to hire for it. If I'm hiring from outside the BEAM world — general OOP developers attracted by Smalltalk syntax — 'you can't mutate anything' is a hard sell. Mutable semantics are what make Smalltalk feel accessible to mainstream developers. You're narrowing the appeal to people who already think functionally." |

### Best Argument for Alternative 3: Full Immutability, No Local Rebinding (Gleam-Style)

| Cohort | Their strongest argument |
|--------|------------------------|
| **Language designer** | "Rebinding is a half-measure. It *looks* like mutation but isn't — that's confusing, not ergonomic. Gleam proves you can succeed on BEAM with strict immutability. If you allow `x := x + 1`, newcomers will expect `self x := 5` to work too. Draw the line clearly: nothing mutates, everything is a new value. The collection protocol handles accumulation." |
| **BEAM veteran** | "Single-assignment is the Erlang way. Every BEAM developer already thinks in accumulators and folds. Rebinding adds compiler complexity (ADR 0041 state threading through blocks) for a feature your core audience doesn't need. Keep it simple — `inject:into:` is just `lists:foldl`." |
| **Compiler engineer** | "Without rebinding, you don't need ADR 0041's state threading at all — blocks are pure closures with zero overhead. No `StateAcc`, no pack/unpack, no tuple returns. The compiler is maximally simple. Every line of state-threading code is a line that can have bugs." |

### Best Argument for Alternative 2: Process-Per-Object

| Cohort | Their strongest argument |
|--------|------------------------|
| **Smalltalk purist** | "This is the only alternative that actually preserves Smalltalk's object model. Every object has identity, state, and behavior. Message sends are message sends, not function calls. Alan Kay said it's about the messages — and this is the only design where every message is actually a message." |
| **Language designer** | "Erlang already proved that millions of lightweight processes work. The overhead argument is about today's BEAM, not tomorrow's. If you optimize process creation (pooling, pre-allocation), you could have true Smalltalk semantics on BEAM. This is the ambitious choice." |

### Why This ADR's Approach Wins Despite the Steelmans

**Against Alternative 1 (Status Quo):** The BEAM veteran's argument is the strongest — ADR 0041 is working, why not finish? However:

1. **Block composability was the bounded problem. What remains is unbounded.** ADR 0041 solved blocks — a well-defined scope with clear calling conventions. Self-send dependency analysis, conditional mutation reconciliation, and deep call-chain propagation are fundamentally harder because they involve inter-method, inter-class, and runtime-polymorphic code paths. Each new language feature (async, pattern matching, etc.) multiplies the state-threading surface area.

2. **The Kotlin/Swift analogy breaks down.** Those languages have mutable memory — the compiler transforms are source-to-source within a mutable execution model. On BEAM, every "mutation" is a data structure copy. The compilation target is fundamentally different, not just harder.

3. **ADR 0041's investment is preserved and active.** The implementation continues to serve local rebinding through blocks — the bounded problem it was designed for. The unbounded problems (self-send chains, conditional slot mutations, deep call chains) are eliminated by immutable objects.

**Against Alternative 3 (Full Immutability, No Rebinding):** The consistency argument is decisive. The REPL must allow rebinding for interactive exploration. If compiled code prohibits rebinding, code cannot move between REPL and class definitions without rewriting. Elixir faced this exact choice and chose rebinding everywhere — it was essential to the Ruby-to-BEAM transition. BeamTalk faces the same transition with Smalltalk developers and must make the same choice. Additionally, ADR 0041's state-threading infrastructure is already implemented, making local rebinding through blocks a solved problem with near-zero marginal cost.

**Against Alternative 2 (Process-Per-Object):** The Smalltalk purist's argument is philosophically compelling but practically disqualifying. `Point x: 3 y: 4` allocating a process is a ~1000x overhead for the most common operation in any program. No optimization can close that gap — processes require mailboxes, reduction counting, and GC roots. This is not a performance concern; it's a category error.

### Tension Points

- **Smalltalk fidelity vs. platform alignment.** Reasonable people disagree on whether BeamTalk should prioritize "feels like Smalltalk" or "works naturally on BEAM." This ADR chooses platform alignment with Smalltalk ergonomics: local rebinding preserves the feel of Smalltalk's mutable temporaries, while immutable objects align with BEAM's strengths. The strongest counter: "if `self x := 5` doesn't work, it's not Smalltalk — it's Erlang in a trench coat."

- **"Smalltalk-inspired" vs. "Smalltalk."** This ADR moves BeamTalk from "Smalltalk on BEAM" toward "Smalltalk-inspired language on BEAM." That's a positioning shift, not just a technical one. It affects marketing, documentation, community expectations, and which developers the language attracts. The Erlang/Elixir community may find it more natural; the Smalltalk community may find it alienating. Local rebinding softens this — method bodies still *feel* like Smalltalk, even if the object model is different.

- **Where rebinding ends and mutation begins.** Local rebinding is semantically immutable (shadowing), but it *looks* like mutation (`x := x + 1`). This is a feature for onboarding (familiar syntax) but a potential source of confusion when developers expect reference semantics. The key distinction — rebinding a local doesn't affect other references to the old value — must be taught clearly.

- **ADR 0041 scope: asset, not burden.** ADR 0041's state-threading infrastructure handles exactly the remaining mutable-semantics need (local rebinding through blocks). It no longer needs to solve self-send chains, conditional slot mutations, or deep call-chain propagation. This is a significant scope reduction — from "thread all mutable state everywhere" to "thread local rebindings through blocks" — which makes the infrastructure easier to maintain and less likely to produce edge-case bugs.

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

### Alternative 4: Hybrid — Value Objects + Mutable Actor Fields

Allow actors to have traditional mutable instance variable assignment (`self x := 5`) while keeping value objects immutable.

**Rejected because:** This reintroduces all the state-threading complexity for actor methods. Self-sends in actor methods would need dependency analysis. Blocks in actor methods that mutate slots would need state threading. The compiler simplification benefit is lost.

### Alternative 5: Trait/Protocol-Based Mutability

Use a trait or protocol to opt into mutability:

```smalltalk
Value subclass: Point
  uses: Mutable
  slots: x = 0, y = 0
```

**Rejected because:** This is just Alternative 4 with extra syntax. The compiler still needs to handle mutable and immutable paths. The complexity isn't eliminated, it's disguised.

### Alternative 6: Immutable by Default with `mutable` Class Modifier

Allow specific value classes to opt into mutable instance variables via a declaration modifier:

```smalltalk
mutable Object subclass: Builder
  slots: parts = List new

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

**Rejected because:** The primary construction path is the keyword constructor (`Point x: 1 y: 2`), which handles the multi-field case directly. For modification of existing objects, `with*:` chains or cascades work and are idiomatic in the functional world (Erlang records, Gleam records, Swift structs). The freeze/thaw pattern adds a new concept (mutable transient objects) that exists only during construction — a narrow use case that doesn't justify the conceptual overhead. If deep nested updates prove painful, lens-like `update*:` methods (noted as a future enhancement) are a better solution.

## Consequences

### Positive

- **Eliminates compile-time state threading for value objects entirely.** Value objects are immutable data — they map directly to BEAM terms. No `StateAcc`, no pack/unpack, no dual codegen paths.
- **Eliminates state-threading for actor methods.** Actor methods are pure functions on state maps. The gen_server framework handles state persistence. No self-send analysis needed.
- **Blocks are safe and composable.** No instance variable mutation in blocks. Local rebinding through blocks is handled by ADR 0041's proven infrastructure. HOMs work universally.
- **Higher-order messages work universally.** Blocks can be freely passed, stored, and executed. Local rebinding through blocks uses ADR 0041's universal protocol — no whitelist, no special cases.
- **Concurrency is natural.** Immutable data on the BEAM is the happy path — no copying overhead, no race conditions.
- **Enables advanced runtime features.** Immutable state enables checkpoint, fork, replay, and time-travel debugging. Actor state transitions can be modeled as event sourcing.
- **Compiler simplification** allows focus on differentiating features: liveness, image-based development, agentic AI runtime.
- **`.` vs `!` for call/cast** makes concurrency semantics explicit with minimal syntax.

### Negative

- **Not faithful Smalltalk.** No mutable instance variables. Common Smalltalk patterns involving slot assignment (`self x := 5`) require adaptation to `with*:` methods. The "it's just Smalltalk" story weakens.
- **Potential verbosity.** Deep state updates on nested value objects require `with*:` chains: `person withAddress: (person address withCity: 'Portland')`. Lens-like patterns may be needed for ergonomics.
- **Breaking change from current behavior.** Existing code that uses mutable instance variables must be rewritten to use `with*:` functional setters.
- **ADR 0041 remains exercised.** Local rebinding inside blocks still requires state threading. The compiler is simpler than full mutable-object semantics (no self-send analysis, no conditional mutation reconciliation), but not as simple as full immutability would have been.
- **`state:` → `slots:`, `classState:` → `classSlots:`, and `fieldAt:` → `slotAt:` renames.** These are breaking changes to the declaration syntax (ADR 0013) and reflection API (ADR 0035). Existing code using `state:`, `classState:`, or `fieldAt:` must be updated. Bundled with this ADR to avoid multiple separate breaking changes.
- **Live patching of value object classes requires migration protocol.** Value objects have no process, so there is no `code_change` callback. An existing value object in the system after a class definition change (e.g., new slot added) is an instance of the old layout. For image-based development, a value migration protocol (analogous to database schema migrations) may be needed. This is a known constraint inherited from Erlang's data model — Erlang records have the same issue — and should be addressed in the image-based development ADR.
- **Block escape at value/actor boundary.** A block that captures rebindable locals and is passed to an actor as a callback may execute asynchronously. ADR 0041's state threading works synchronously — the `StateAcc` is threaded through the call chain. If a block escapes to an async context, the captured state snapshot is frozen at escape time. This is the same semantics as Elixir closures (capture by value), and is correct, but may surprise developers who expect the rebinding to propagate across async boundaries.
- **`!` safety is two-tier: compile-time + runtime.** For statically-known receiver types (assigned from a constructor, `self` in actor methods), the compiler already classifies classes as value or actor via `is_actor_class()` and can reject `!` on value types at compile time. For dynamically-typed receivers (parameters, collection elements), the dispatch layer checks `is_pid(Receiver)` at runtime and raises `#not_an_actor` if `!` is used on a value object. This is the same pattern as any unsupported message send — it fails at dispatch. No additional type system work (ADR 0025) is needed as a prerequisite.

### Neutral

- **ADR 0041 remains active and essential.** ADR 0041's universal state-threading protocol is largely implemented and continues to serve its purpose: threading local variable rebindings through blocks. The scope is narrower than under the status quo — only local variable rebinding needs threading, not instance variable mutation or self-send chains — but the infrastructure is exercised by any block that rebinds a captured local. This is exactly the bounded problem ADR 0041 was designed for.
- **ADR 0005 alignment.** ADR 0005 (BEAM Object Model) already distinguishes value types from actors. This ADR formalizes the immutability constraint that was implicit in ADR 0005's design.
- **Collection protocol completeness is important but not critical.** With local rebinding, developers can always fall back to `do:` with accumulator rebinding. However, `inject:into:`, `collect:`, `select:`, `reject:`, and `detect:` should still be comprehensive — idiomatic functional patterns are cleaner than imperative accumulation and should be the encouraged style.
- **Auto-generated `with*:` methods are load-bearing.** Without them, immutability is tedious boilerplate. The compiler must generate them reliably for all declared slots.

## Implementation

### Phase 0: Wire Check (Proof of Concept)

- Compile one `Value subclass:` declaration with auto-generated getters and `with*:` methods
- Verify the generated Core Erlang compiles and produces correct output
- Compile one `Actor subclass:` with `.` (call) and `!` (cast) message sends
- Verify the gen_server routing works for both call and cast
- **Goal:** Prove the core codegen patterns work before rewriting all class compilation
- **Effort:** S

### Phase 1: Language Model Changes (M)

**Lexer:**
- Add `!` (bang) as a recognized token (`TokenKind::Bang`). Currently `!` falls into the error branch of the lexer's main dispatch. Must be added to `token.rs` and handled in `lexer.rs`.

**Parser:**
- Recognize `!` as a statement terminator (cast syntax), distinct from `.` (call)
- Support `Value subclass:` as a new declaration form (today value types use `Object subclass:`)
- Rename `state:` → `slots:` as the declaration keyword for both value types and actors
- Rename `classState:` → `classSlots:` as the class-level declaration keyword
- Reject instance variable assignment (`self.slot :=`) in value type methods with clear error message guiding toward `self withSlot:`
- Detect cast-in-expression-context errors (`x := foo bar!`)

**Semantic Analysis:**
- Classify classes as Value or Actor based on declaration keyword (replacing the current `is_actor_class()` heuristic that walks the superclass chain)
- Verify no slot mutation (`self.slot :=`) in value object methods
- Verify `!` is only used on actor-type receivers (not value objects, not `ErlangModule` proxies)
- Update block mutation analysis (`block_analysis.rs`) to distinguish "slot write on value type" (prohibited) from "slot write on actor" (allowed)

### Phase 2: Codegen Changes (L)

**Value Object Codegen:**
- Generate immutable map-based objects from `Value subclass:` + `slots:` declarations
- Auto-generate getters and `with*:` functional setters for each declared slot
- Auto-generate keyword constructors
- Remove slot-write codegen for value types (no `self.slot :=` → `maps:put` paths)
- **Affected:** `value_type_codegen.rs`, `mod.rs`, `class_hierarchy/mod.rs`

**Actor Codegen:**
- Generate gen_server module with `handle_call`/`handle_cast` callbacks
- Map `.` message sends to `gen_server:call`
- Map `!` message sends to `gen_server:cast`
- Actor methods receive state map as `self`, return new state map
- Auto-generate getters and `with*:` for actor state
- **Affected:** `actor_codegen.rs`, `mod.rs`, `beamtalk_dispatch.erl`

**Cascade Codegen:**
- Value object cascades: compile `a foo; bar; baz` as `baz(bar(foo(a)))` — pipeline threading each result to the next message send
- Actor cascades: compile `a foo; bar; baz` as `foo(a), bar(a), baz(a)` — fan-out to the same pid, return result of last message
- Receiver type determines semantics: `is_actor_class()` at compile time, `is_pid` at runtime for dynamic receivers
- Dynamic receivers: emit runtime branch checking `is_pid(Receiver)` once at cascade entry, then use pipeline or fan-out path accordingly
- **Affected:** `cascade_codegen.rs` (or equivalent in `mod.rs`), `beamtalk_dispatch.erl`

### Phase 3: Runtime Changes (M)

- Update `beamtalk_dispatch` to route calls vs casts based on message send type
- Update actor framework to interpret method return values (new state vs early return via `^`)
- Add `pid` method to `Actor` base class (extracts pid from `#beamtalk_object{}` record)
- Verify REPL binding semantics are preserved (workspace rebinding still works)
- Define `!` behavior in REPL context (casts produce no result for auto-await)
- **Affected:** `beamtalk_dispatch.erl`, `beamtalk_actor.erl`, `beamtalk_repl_shell.erl`, `Actor.bt`

### Phase 4: Migration and Testing (L)

- Update all existing stdlib classes to Value/Actor model (`Object subclass:` → `Value subclass:` where appropriate)
- Rename `state:` → `slots:` in all class declarations (stdlib, tests, examples)
- Rename `classState:` → `classSlots:` in all class declarations
- Rename `fieldAt:` → `slotAt:`, `fieldAt:put:` → `slotAt:put:`, `fieldNames` → `slotNames` in runtime and stdlib
- Replace `self.slot :=` patterns with `self withSlot:` patterns in all value type methods
- Comprehensive error message testing for rejected patterns (`self.slot :=` on value types, `!` on value objects)
- Verify all existing stdlib and e2e tests pass
- Performance benchmarks for value object creation and `with*:` operations

## Migration Path

### Existing Value Types

Current value types (Point, Color, etc.) already behave mostly as immutable values. Migration involves:
1. Change class declaration from `Object subclass:` to `Value subclass:`
2. Rename `state:` → `slots:` in declarations
3. Remove any setter methods (replaced by auto-generated `with*:`)
4. Replace `self x := val` with return of `self withX: val`

### Existing Actors

Current actors already use gen_server. Migration involves:
1. Change class declaration to `Actor subclass:` with `slots:`
2. Ensure methods return new state (via `with*:` methods) rather than mutating slots
3. Replace `self x := val. self y := val2.` with `(self withX: val) withY: val2`

### Existing Tests

Tests using instance variable assignment (`self.slot := value`) must be rewritten. Local variable rebinding continues to work, so many test patterns are unaffected. A migration guide with common pattern translations should accompany this change:

| Current Pattern | New Pattern | Notes |
|---|---|---|
| `self.count := self.count + 1` | `self withCount: self count + 1` (return as new state) | Instance variable → functional setter |
| `self.x := val. self.y := val2` | `(self withX: val) withY: val2` | Sequential slot updates → chain |
| `self.value := self.value + delta` | `self withValue: self value + delta` | Actor state transition |
| `Object subclass: Point` | `Value subclass: Point` | Declaration keyword change |
| `Object subclass: Point` with `state:` | `Value subclass: Point` with `slots:` | Superclass + declaration keyword |
| `classState: count = 0` | `classSlots: count = 0` | Class-level declaration keyword |
| `result := 0. items do: [:each \| result := result + each]` | Unchanged — local rebinding still works | Or use `inject:into:` for idiomatic style |

## References

### Related ADRs
- ADR 0005 (BEAM Object Model) — formalized by this ADR; value type immutability made explicit
- ADR 0006 (Unified Method Dispatch) — `is_actor_class()` heuristic replaced by explicit `Value`/`Actor` declaration
- ADR 0013 (Class Variables, Class-Side Methods, Instantiation) — `state:` renamed to `slots:` for all class declarations
- ADR 0025 (Gradual Typing) — `!` safety works without type system (compile-time class check + runtime pid guard); gradual typing will improve static coverage over time but is not a prerequisite
- ADR 0028 (BEAM Interop) — `!` restricted to actor targets; immutability is compile-time only, not enforced at BEAM boundary
- ADR 0035 (Field-Based Reflection API) — **partially superseded**: `fieldAt:`/`fieldNames` renamed to `slotAt:`/`slotNames`; closes the deferred `slotAt:put:` access control for value types (compile-time + runtime enforcement)
- ADR 0038 (Subclass/ClassBuilder Protocol) — ClassBuilder must recognize `Value` as a root class
- ADR 0037 (Collection Class Hierarchy) — collection classes (List, Array, Dictionary, Range) must be classified as Value or Actor; `Range` init pattern using `self.slot :=` needs migration to keyword constructor
- ADR 0039 (Syntax Pragmatism vs Smalltalk) — cascade semantics diverge from Smalltalk: pipeline on values, fan-out on actors. **ADR 0039 must be updated** at implementation time to document the new cascade semantics (currently documents fan-out as "What We Keep")
- ADR 0040 (Workspace-Native REPL Commands) — REPL binding semantics preserved; `!` defined for REPL context
- ADR 0041 (Universal State-Threading Block Protocol) — scope narrowed to local rebinding through blocks; remains active and essential

### External References
- Alan Kay on Erlang: acknowledgment that Erlang captures the message-passing spirit of Smalltalk
- Clojure persistent data structures: immutable-by-default with controlled state via atoms/refs
- Gleam: immutable-by-default language on BEAM, strict immutability validated at scale
- Elixir: rebinding as ergonomic immutability, successful Ruby-to-BEAM developer transition
- Swift value types: struct vs class distinction, `mutating` keyword as analogous pattern
- Erlang/OTP gen_server: functional state transition model adopted for actors
- Pony reference capabilities: value vs identity distinction at the type level

### Related Issues
- BT-506 — pipeline operator; resolved by cascade-as-pipeline semantics on value objects
- BT-868, BT-900, BT-904, BT-894 — state-threading edge cases that motivated this decision
