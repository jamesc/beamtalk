# ADR 0078: Actor Initialize Inheritance

## Status
Accepted (2026-04-12)

## Context

BT-1947 introduced typed state declarations without defaults (`state: db :: Database`), where the type annotation replaces the need for a `= default`. The compiler stores `nil` internally and trusts the `initialize` method to set the field before use.

BT-1949 added a runtime check: after `initialize` returns, generated code verifies that all typed-no-default fields are no longer `nil`, raising `UninitializedStateError` if any remain unset. However, this check only covers the **current class's own fields** — inherited typed-no-default fields from parent classes are not validated.

### Current Initialization Flow

When a child actor class extends a parent:

1. Child's `init/1` calls parent's `init/1` with `__skip_initialize__ => true`
2. Parent returns its default state (typed-no-default fields are `nil`)
3. Child merges its own fields on top (parent defaults → child defaults → user args)
4. Only the **leaf class's** `initialize` is dispatched via `handle_continue`

This means parent-defined `initialize` methods never run for subclasses. If a parent declares `state: db :: Database` and initializes it in its own `initialize`, a subclass will inherit the `nil` default but never run the parent's initialization logic.

### The Problem

```beamtalk
Actor subclass: DatabaseActor
  state: db :: Database

  initialize => self.db := Database connect   // sets db

DatabaseActor subclass: CachingDatabaseActor
  state: cache :: Cache

  initialize => self.cache := Cache new
  // db is nil — parent's initialize never ran
  // CachingDatabaseActor has no idea it needs to set db
```

This is a known anti-pattern in Smalltalk ("forgetting `super initialize`") but in classic Smalltalk it's purely convention — there's no enforcement. Beamtalk can do better by not requiring the programmer to remember at all.

### Constraints

- `super` dispatch already works in Beamtalk (via `beamtalk_dispatch:super/5`)
- Actors are OTP `gen_server` processes — initialization runs in `handle_continue`
- Typed state declarations (BT-1947) provide the metadata to detect which fields need initialization
- Any solution should not preclude future evolution toward declarative slot initializers (Newspeak-style)

## Decision

Auto-chain `initialize` methods up the class hierarchy. When an actor spawns, codegen dispatches `initialize` on every class in the hierarchy that defines one, **deepest ancestor first** (parent before child). The programmer never writes `super initialize` — the platform handles it.

### Auto-Chain Semantics

When `handle_continue` receives the `initialize` token:

1. Walk the class hierarchy from `Actor` (root) down to the leaf class
2. For each class that defines its own `initialize` method, dispatch it against the current state
3. Each `initialize` sees the state left by the previous one (parent's fields are already set)
4. After the full chain completes, run the BT-1949 `UninitializedStateError` check on all fields (including inherited)
5. If any `initialize` in the chain fails, stop the actor immediately — same as current behavior

### Code Examples

**Just works — no super call needed:**
```beamtalk
Actor subclass: DatabaseActor
  state: db :: Database

  initialize =>
    self.db := Database connect

DatabaseActor subclass: CachingDatabaseActor
  state: cache :: Cache

  initialize =>
    // db is already set — DatabaseActor's initialize ran first
    self.cache := Cache new
```

**Deep hierarchy — each level handles its own fields:**
```beamtalk
Actor subclass: BaseService
  state: logger :: Logger

  initialize =>
    self.logger := Logger forClass: self class

BaseService subclass: DatabaseService
  state: db :: Database

  initialize =>
    // logger is already set by BaseService's initialize
    self.db := Database connect

DatabaseService subclass: CachingDatabaseService
  state: cache :: Cache

  initialize =>
    // logger and db are already set
    self.cache := Cache new
```

**No initialize needed — parent handles everything:**
```beamtalk
DatabaseActor subclass: ReadOnlyDatabaseActor
  // No initialize — inherits DatabaseActor's initialize which sets db
  // No new typed-no-default fields — nothing to do
  query: sql => self.db query: sql
```

**Runtime error if parent's initialize forgets a field:**
```text
UninitializedStateError: DatabaseActor field 'db' (:: Database) was not initialized
```

**Compiler warning if `super initialize` is used (redundant):**
```text
warning: explicit `super initialize` is unnecessary — parent initializers run automatically
  --> my_actor.bt:12:5
   |
12 |     super initialize
   |     ^^^^^^^^^^^^^^^^
   = help: Remove this line — Beamtalk auto-chains initialize up the hierarchy
```

### Execution Order

Initialize methods execute **parent-first** (most distant ancestor → leaf):

```
Actor.initialize        (no-op default)
  → BaseService.initialize   (sets logger)
    → DatabaseService.initialize   (sets db)
      → CachingDatabaseService.initialize   (sets cache)
```

This matches constructor chaining in Java, Kotlin, and Swift — parent is fully initialized before child runs.

### Error Handling

If any `initialize` in the chain raises an error:
- The chain stops immediately (no further initializers run)
- The actor stops with the error
- `safe_spawn` translates this to a catchable `InstantiationError`
- Under a supervisor, the child start fails and the supervisor's restart strategy applies

### Runtime Validation

After the full chain completes, extend the BT-1949 post-initialize check to validate **all** typed-no-default fields in the inheritance chain. This catches cases where:
- A class's `initialize` forgets to set one of its own fields
- A parent class adds a new typed-no-default field but doesn't update its `initialize`
- Cross-file inheritance where the compiler can't see the full hierarchy

## Prior Art

### Pharo/Squeak Smalltalk
Explicit `super initialize` is convention but not enforced. Forgetting it is one of the most common Smalltalk bugs. Instance variables default to `nil` silently. Some teams use lint rules but the language itself provides no safety net.

**Improved upon**: Beamtalk auto-chains instead of relying on the programmer to remember. The classic Smalltalk bug is eliminated by design.

### Newspeak
Slot initializer expressions are declarative — evaluated automatically at construction time for each class in the hierarchy. No explicit super call needed. Parent slot initializers run as part of object creation.

**Adopted the principle**: The platform handles parent initialization, not the programmer. Auto-chaining `initialize` is the imperative equivalent of Newspeak's declarative slot initializers.

**Noted as future direction**: Declarative slot initializers would complement auto-chained `initialize` — slot initializers handle declarative setup, `initialize` handles imperative post-construction logic.

### Swift
Designated initializers must call `super.init(...)`. The compiler enforces this at compile time. Two-phase initialization prevents access to `self` before all properties are set.

**Departed from**: Swift requires explicit `super.init()`. Beamtalk auto-chains because in a flat-state model (single map), there's no two-phase concern — all fields exist in the map from the start, just with `nil` values.

### Kotlin
Constructor chaining is automatic via `class Sub : Parent(args)`. `init {}` blocks run in declaration order.

**Adopted**: Kotlin's automatic constructor chaining model. Parent `init` blocks run before child `init` blocks without explicit calls.

### Pony
No inheritance — uses composition and traits. All actor fields must be definitely assigned in the constructor (compile-time enforcement).

**Noted**: Pony's approach eliminates the problem entirely but doesn't apply to Beamtalk's inheritance model.

### Erlang/OTP
No inheritance in gen_server. Each module's `init/1` is independent. "Inheritance" is manual module calls with state merging — exactly what Beamtalk codegen does today.

**Context**: Beamtalk's class inheritance already departs from OTP's flat module model. Auto-chaining `initialize` is consistent with that departure — if we have inheritance, initialization should follow the hierarchy automatically.

## User Impact

### Newcomer (from Python/JS/Ruby)
Cannot get this wrong — parent initialization runs automatically. No `super()` call to remember. Matches the "it just works" expectation from Kotlin and modern Java.

### Smalltalk Developer
A departure from Smalltalk's explicit `super initialize` convention, but one that eliminates Smalltalk's most common initialization bug. The auto-chain model is closer to Newspeak than Pharo.

### Erlang/BEAM Developer
Auto-chaining adds implicit behavior inside `handle_continue` that has no OTP equivalent. However, class inheritance itself has no OTP equivalent — this is consistent with the existing departure. Each `initialize` call is visible in traces as a `dispatch` call.

### Production Operator
Initialization failures are caught immediately at spawn time (fail-fast) as a catchable `InstantiationError`. Under a supervisor, the child start fails and the supervisor's restart strategy applies. The execution order (parent-first) is deterministic and traceable.

## Steelman Analysis

For each rejected alternative, the strongest possible argument from each cohort.

### Alternative B: Explicit `super initialize` with compiler warning

- 🧑‍💻 **Newcomer**: "At least I can see the call chain explicitly in my code. With auto-chaining, I have to *know* that parents run first — it's invisible."
- 🎩 **Smalltalk purist**: "Explicit is better than implicit. I want to control *when* parent initialization runs relative to my own setup. What if I need to set something before the parent's initialize?"
- ⚙️ **BEAM veteran**: "I can trace `super initialize` in the code. Auto-chaining is invisible magic inside codegen that I can't see in the source."
- 🎨 **Language designer**: "B has the lowest migration cost to D (slot initializers). With B, you just delete the `super initialize` line. With A, you have to *add* logic to suppress auto-chaining if slot initializers conflict."

**Rejected because**: If you should *always* call `super initialize`, making it explicit just imports a known-bad pattern from Smalltalk and adds a compiler warning to compensate. The warning is an admission that the design is error-prone. Auto-chaining does the right thing by default — the programmer literally cannot forget.

**Why this is still a close call**: The language designer's point about B → D migration is real. With auto-chaining, when slot initializers arrive, we need to ensure that a field with a slot initializer doesn't *also* get set by `initialize`. But this is a solvable problem at the slot initializer design stage — not a reason to reject auto-chaining now.

### Alternative C: Validate-only (flat initialization)

- 🧑‍💻 **Newcomer**: "One method, one place to look, everything is explicit. No hidden parent logic."
- ⚙️ **BEAM veteran**: "In OTP, each module owns its own `init/1`. One process, one init, one state map. The flat model matches how gen_server actually works."
- 🏭 **Production operator**: "I can look at one file and know exactly what happens at startup. No chain to trace."
- 🎨 **Language designer**: "Beamtalk's state is a flat map. The child already has direct access to `self.db`. Why pretend there's an encapsulation boundary?"

**Rejected because**: When a parent adds a new typed-no-default field, every subclass must be updated — even subclasses that have no knowledge of the parent's internals. This coupling is fragile. Auto-chaining lets parent and child evolve independently.

### Alternative D: Declarative slot initializers (Newspeak-style)

- 🧑‍💻 **Newcomer**: "The declaration IS the initialization. This is how Swift, Kotlin, and Dart work."
- 🎩 **Smalltalk purist**: "This is Newspeak's best idea. No `initialize`, no `super`, no convention to forget."
- 🎨 **Language designer**: "Every other option is a workaround for the fact that typed-no-default fields don't have initializers. D fixes the root cause."

**Noted as future direction**: D is the right long-term answer, but XL implementation effort. Auto-chaining (A) is complementary — when D arrives, slot initializers handle declarative setup and `initialize` handles imperative post-construction logic. The auto-chaining semantics don't need to change.

### Tension Points

- **The Smalltalk purist wants explicit control** — but the 40-year track record shows programmers forget `super initialize`. The purist's preference causes the purist's most common bug.
- **The BEAM veteran wants OTP purity** — but class inheritance already breaks that. Auto-chaining is consistent with the existing departure.
- **The language designer worries about A → D migration** — but auto-chaining + slot initializers are complementary, not conflicting. The migration concern is solvable at D's design stage.
- **Everyone agrees on D long-term** — A is the best interim solution because it shares D's core principle: the platform handles parent initialization, not the programmer.

## Consequences

### Positive
- Eliminates the classic "forgot super initialize" bug by design
- Parent and child can evolve independently — adding fields to a parent doesn't break subclasses
- No new syntax to learn — initialization "just works"
- Runtime validation (BT-1949) provides a safety net for any remaining gaps
- Complementary with future declarative slot initializers (Option D)

### Negative
- Implicit behavior — parent `initialize` runs without any visible call in the child's source
- Programmer cannot control when parent `initialize` runs relative to child setup (always parent-first)
- Each `initialize` in the chain must be independent — child's initialize cannot assume parent's initialize hasn't run yet
- Compiler warning needed for explicit `super initialize` (now redundant and potentially harmful if it causes double-init)

### Neutral
- Execution order (parent-first) matches Java, Kotlin, Swift constructor chaining
- `Actor.bt` defines `initialize -> Nil => nil`, so the chain always terminates cleanly
- BT-1949's post-initialize check in `handle_continue` is extended to validate all inherited fields

## Implementation

### Phase 1: Auto-chain codegen + inherited field validation (M)
- **Codegen**: Modify `handle_continue` generation in `callbacks.rs` to walk the class hierarchy and dispatch `initialize` on each class that defines one, parent-first. Remove `__skip_initialize__` flag from `init/1` — parent `initialize` methods are no longer suppressed, they're explicitly chained by the leaf's `handle_continue`.
- **Codegen**: Extend `generate_post_initialize_check` to validate all typed-no-default fields in the hierarchy, not just the current class's own (BT-1951).
- **Files**: `crates/beamtalk-core/src/codegen/core_erlang/gen_server/callbacks.rs`, `crates/beamtalk-core/src/codegen/core_erlang/gen_server/state.rs`
- **Tests**:
  - BUnit test: parent sets typed-no-default field in initialize, child doesn't mention it — works
  - BUnit test: parent forgets its own field — `UninitializedStateError`
  - BUnit test: deep hierarchy (3 levels) — all initializers run in order
  - BUnit test: child with no initialize, parent has initialize — parent's runs

### Phase 2: Compiler warning for redundant `super initialize` (S)
- **Type checker**: When an `initialize` method contains `super initialize`, emit a warning: "explicit `super initialize` is unnecessary — parent initializers run automatically."
- **Files**: `crates/beamtalk-core/src/semantic_analysis/type_checker/`
- **Test**: BUnit test with `super initialize` in body — verify warning is emitted

### Note on `__skip_initialize__`

The `__skip_initialize__` flag in `init/1` currently prevents parent `initialize` from running when the parent's `init/1` is called as a state-building helper. With auto-chaining, this flag is still needed for `init/1` (parent's init should still just return state, not dispatch initialize). The change is in `handle_continue` — instead of dispatching initialize once on the leaf, it dispatches initialize on each class in the chain.

### Future Evolution: Declarative Slot Initializers
When the language evolves to support initializer expressions on state declarations (e.g., `state: db :: Database = Database connect`):
- Slot initializers run at `init/1` time (before `handle_continue`)
- `initialize` methods run at `handle_continue` time (after slot initializers)
- Auto-chaining still applies to `initialize` — it handles imperative post-construction logic
- The compiler should warn if an `initialize` method sets a field that already has a slot initializer

## Migration Path

No migration needed — this is purely additive:
- Existing code without `initialize`: no change
- Existing code with `initialize` that doesn't call `super initialize`: parent initializers now run automatically (this is the desired fix)
- Existing code with explicit `super initialize`: gets a compiler warning (redundant), but behavior is unchanged if parent's `initialize` is idempotent
- The `super initialize` warning can be suppressed with `@expect` during migration

## References
- Related issues: BT-1949 (UninitializedStateError check), BT-1951 (inherited field validation)
- Related ADRs: ADR 0065 (OTP primitives for actor lifecycle), ADR 0067 (state/field keywords)
- Prior art: Newspeak slot initializers, Kotlin automatic constructor chaining, Swift designated initializers
- Documentation: `docs/beamtalk-language-features.md` (Actor Lifecycle Hooks)
