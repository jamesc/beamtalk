# ADR 0078: Actor Initialize Inheritance

## Status
Proposed (2026-04-12)

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

This is a known anti-pattern in Smalltalk ("forgetting `super initialize`") but in classic Smalltalk it's purely convention — there's no enforcement. Beamtalk can do better.

### Constraints

- `super` dispatch already works in Beamtalk (via `beamtalk_dispatch:super/5`)
- Actors are OTP `gen_server` processes — initialization runs in `handle_continue`
- Typed state declarations (BT-1947) provide the metadata to detect which fields need initialization
- Any solution should not preclude future evolution toward declarative slot initializers (Newspeak-style)

## Decision

Require explicit `super initialize` in actor `initialize` methods when the class inherits typed-no-default state fields. Enforce this with a **compiler warning** (not an error, to avoid breaking existing code).

### Compiler Warning

When all of these conditions are true:
- The class is an Actor subclass (not Value or Object)
- The class defines an `initialize` method
- An ancestor class has typed-no-default state fields (`state: x :: Type`, no `= default`)

Then the compiler checks whether the `initialize` method body contains a `super initialize` send. If not, emit:

```text
warning: initialize does not call `super initialize`
  --> my_actor.bt:10:3
   |
10 |   initialize =>
   |   ^^^^^^^^^^
   = help: DatabaseActor declares `state: db :: Database` which requires initialization
   = help: Add `super initialize` to ensure inherited fields are set
```

### Runtime Validation

Extend the BT-1949 post-initialize check to validate **all** typed-no-default fields in the inheritance chain, not just the current class's own fields. This catches cases where:
- The programmer ignores the warning
- The parent class is in a different compilation unit (cross-file inheritance)
- The parent adds a new typed-no-default field after the child was written

### Code Examples

**Correct usage:**
```beamtalk
Actor subclass: DatabaseActor
  state: db :: Database

  initialize =>
    self.db := Database connect

DatabaseActor subclass: CachingDatabaseActor
  state: cache :: Cache

  initialize =>
    super initialize              // calls DatabaseActor's initialize, sets db
    self.cache := Cache new       // sets cache
```

**Warning on missing super:**
```beamtalk
DatabaseActor subclass: BadCachingActor
  state: cache :: Cache

  initialize =>
    self.cache := Cache new
    // warning: initialize does not call `super initialize`
    // DatabaseActor declares `state: db :: Database` which requires initialization
```

**Runtime error if warning is ignored:**
```text
UninitializedStateError: BadCachingActor field 'db' (:: Database) was not initialized
```

**No warning needed when parent has no typed-no-default fields:**
```beamtalk
Actor subclass: Counter
  state: value = 0           // has default, no issue

Counter subclass: LoggingCounter
  state: log :: Array

  initialize =>
    self.log := Array new     // no warning — Counter has no typed-no-default fields
```

**No warning when class has no initialize:**
```beamtalk
Counter subclass: DoubleCounter
  // No initialize, no typed-no-default fields — nothing to warn about
  doubleIncrement => self.value := self.value + 2
```

### Super Initialize Semantics

`super initialize` follows standard Beamtalk super-dispatch semantics:
- Skips the current class, starts method lookup at the immediate superclass
- If the superclass doesn't define `initialize`, walks further up the chain
- `Actor.bt` defines a default `initialize -> Nil => nil`, so `super initialize` always finds a method — it never raises `doesNotUnderstand`
- If no intermediate ancestor overrides `initialize`, the call reaches `Actor`'s no-op default and returns `nil`
- The parent's `initialize` executes with access to the same state map (flat state model)

**Idempotency requirement**: Parent `initialize` methods should be safe to call via `super`. Avoid side effects that cannot be repeated (e.g., spawning linked processes without guards). This is the same contract as Smalltalk's `super initialize`.

## Prior Art

### Pharo/Squeak Smalltalk
Explicit `super initialize` is convention but not enforced. Forgetting it is one of the most common Smalltalk bugs. Instance variables default to `nil` silently. Some teams use lint rules but the language itself provides no safety net.

**Adopted**: The explicit `super initialize` call pattern.
**Improved**: Compiler warning when inherited typed fields exist, plus runtime validation.

### Newspeak
Slot initializer expressions are declarative — evaluated automatically at construction time for each class in the hierarchy. No explicit super call needed. Parent slot initializers run as part of object creation.

**Noted as future direction**: Declarative slot initializers (see Future Evolution below) would eliminate the need for both `initialize` and `super initialize` in most cases. The current decision is designed to not preclude this evolution.

### Swift
Designated initializers must call `super.init(...)`. The compiler enforces this at compile time and requires all stored properties to be assigned before the super call. Two-phase initialization prevents access to `self` before all properties are set.

**Adapted**: Compiler warning (not error) for missing super-init. We chose a warning because Beamtalk's `initialize` is optional and the runtime check provides a safety net.

### Kotlin
Constructor chaining is automatic via `class Sub : Parent(args)`. `lateinit var` properties are checked at runtime on access (not at construction time).

**Rejected**: Kotlin's access-time checking. Beamtalk checks at initialization time (fail-fast) rather than on first access (fail-late).

### Pony
No inheritance — uses composition and traits. All actor fields must be definitely assigned in the constructor (compile-time enforcement).

**Noted**: Pony's approach eliminates the problem entirely but doesn't apply to Beamtalk's inheritance model.

### Erlang/OTP
No inheritance in gen_server. Each module's `init/1` is independent. "Inheritance" is manual module calls with state merging — exactly what Beamtalk codegen does today.

**Context**: Beamtalk's `init/1` chaining with `__skip_initialize__` is already the OTP-idiomatic pattern. This ADR layers programmer-facing initialization semantics on top.

## User Impact

### Newcomer (from Python/JS/Ruby)
Familiar pattern — most OOP languages require calling `super().__init__()` or equivalent. The compiler warning guides them. The error message names the specific field and parent class.

### Smalltalk Developer
This IS the Smalltalk pattern, but with the safety net Smalltalk never had. The warning catches the classic "forgot super initialize" bug at compile time.

### Erlang/BEAM Developer
No change to the gen_server model. `super initialize` is just a dispatch call within `handle_continue`. Observable in `:observer` and `sys:get_state/1` as normal.

### Production Operator
Initialization failures are caught immediately at spawn time (fail-fast) as a catchable `InstantiationError`, not as mysterious `doesNotUnderstand` on `nil` later. Under a supervisor, the child start fails and the supervisor's restart strategy applies. Clear error messages identify the uninitialized field.

## Steelman Analysis

For each rejected alternative, the strongest possible argument from each cohort. If a steelman doesn't make you pause, it's not strong enough.

### Alternative A: Auto-chain initialize up the hierarchy

- 🧑‍💻 **Newcomer**: "Every time I subclass, I have to remember a magic incantation (`super initialize`) or my app breaks at runtime. In Python, `__init_subclass__` handles this. In Java, the compiler forces `super()`. Beamtalk gives me a *warning* I can ignore and a *runtime crash* when I do. Auto-chaining means I literally cannot get this wrong."
- 🎩 **Smalltalk purist**: "Newspeak proved that the language should handle initialization, not the programmer. `super initialize` is a 40-year-old mistake — every Smalltalk team warns newcomers about it, every Smalltalk linter checks for it. We're building a new language. Why import a known-bad pattern and then bolt on a compiler warning to compensate for its badness?"
- ⚙️ **BEAM veteran**: "OTP supervisors auto-chain child specs. OTP applications auto-start dependencies. The BEAM philosophy is: declare the dependency graph, let the platform handle ordering. Auto-chaining `initialize` is the same principle — declare state, let codegen handle setup."
- 🎨 **Language designer**: "The slot initializer future (Option D) will *also* auto-run parent initializers. If we ship auto-chaining now, Option D is an evolution of the same model — declarative expressions replace imperative methods, but the chaining semantics don't change. With Option B, we ship one model now and replace it with a fundamentally different model later."

**Rejected because**: Auto-chaining creates an implicit contract between parent and child that conflicts with declarative slot initializers. If a parent has both a slot initializer (`state: db :: Database = Database connect`) and an `initialize` method, auto-chaining would run both — double-initializing the field. Removing auto-chaining to fix this is a breaking change. With explicit `super initialize`, the programmer simply deletes the line when slot initializers make it unnecessary.

**Why this is still a close call**: The "known-bad pattern" argument from the Smalltalk purist is genuinely strong. We're choosing B partly because it's the path of least commitment, not because `super initialize` is a great design. The compiler warning is an admission that the pattern is error-prone.

### Alternative C: Validate-only (flat initialization)

- 🧑‍💻 **Newcomer**: "One method, one place to look, everything is explicit. I can read `initialize` and see every field being set. No hidden parent logic running that I have to know about. When I debug, there's one `initialize` call in the stack, not a chain."
- ⚙️ **BEAM veteran**: "In OTP, each module owns its own `init/1`. There's no inheritance chain. One process, one init, one state map. `super initialize` creates a Smalltalk abstraction that fights the platform — BEAM processes don't have parent processes that initialize them. The flat model matches how gen_server actually works."
- 🏭 **Production operator**: "I can look at one file and know exactly what happens at startup. With `super initialize`, I have to trace a chain of classes to understand initialization — and if any parent's initialize has side effects (opens connections, starts timers), I need to understand the whole chain to debug startup failures."
- 🎨 **Language designer**: "This is the honest design. Beamtalk's state is a flat map, not an object with encapsulated instance variables. The child *already* has direct access to `self.db` — there's no encapsulation boundary. Why pretend there is one by hiding initialization behind `super`? Let the runtime check enforce the contract, and let the programmer decide how to satisfy it."

**Rejected because**: When a parent adds a new typed-no-default field, every subclass must be updated — even subclasses that have no knowledge of the parent's internals. This coupling is fragile and scales poorly with deep hierarchies. The "no encapsulation" argument is honest about the current state, but `super initialize` at least lets parent and child evolve independently.

**Why this is still a close call**: The BEAM veteran's argument is architecturally sound — Beamtalk's state *is* flat, and `super initialize` imports an abstraction from languages where instance variables are scoped per class. We're choosing B partly because the alternative (DRY violations) is worse in practice, even if C is more honest about the runtime model.

### Alternative D: Declarative slot initializers (Newspeak-style)

- 🧑‍💻 **Newcomer**: "I write `state: db :: Database = Database connect` and it just works. No initialize method, no super calls, no warnings to satisfy. The declaration IS the initialization. This is how modern languages work — Swift, Kotlin, Dart all let you initialize fields inline."
- 🎩 **Smalltalk purist**: "This is Newspeak's best idea and it's the one Gilad Bracha got right. Slot initializers are evaluated in declaration order during construction, automatically for every class in the hierarchy. No `initialize` method, no `super` call, no convention to forget. The language handles it."
- 🎨 **Language designer**: "Every other option is a workaround for the fact that typed-no-default fields don't have initializers. Option D fixes the root cause. B adds a warning to compensate for a missing feature. C adds a runtime check to compensate for a missing feature. D *is* the feature. Every line of code we write for B is technical debt with a known expiry date."
- ⚙️ **BEAM veteran**: "I'm worried about evaluation order and `self` access during initialization. But Newspeak solved this — initializer expressions can reference constructor parameters and earlier slots but not `self`. The constraints are well-understood. And the generated code is simpler: each field becomes a map entry computed at spawn time. No `handle_continue` dispatch chain."

**Noted as future direction**: The language designer's argument — "every line of B is tech debt with a known expiry" — is the strongest argument in this ADR. We chose B because D is XL effort and we need a solution now. But D should not be deferred indefinitely.

### Tension Points

- **The Smalltalk purist and the language designer both argue against B** — the purist because `super initialize` is a known-bad pattern, the designer because B is temporary scaffolding. Neither is wrong.
- **The BEAM veteran is genuinely split**: C matches the platform model, but B is more practical for evolving hierarchies. Neither option feels native to BEAM.
- **The newcomer prefers A or D** — both "just work" without remembering patterns. B requires learning a convention and heeding a warning.
- **The key insight is not that B is the best design — it's that B has the lowest migration cost to D.** Auto-chaining (A) creates implicit behavior that must be removed. Validate-only (C) forces patterns that must be rewritten. `super initialize` (B) is a line that gets deleted. That's it.

## Consequences

### Positive
- Catches the classic "forgot super initialize" bug at compile time
- Runtime validation provides a safety net for cross-file inheritance
- No breaking changes to existing code (warning, not error)
- Clean migration path to declarative slot initializers in the future
- Familiar pattern for developers from most OOP backgrounds

### Negative
- Programmers must remember to write `super initialize` (mitigated by compiler warning)
- Warning is a lint-level check, not a hard guarantee — can be suppressed with `@expect`
- Parent `initialize` methods must be idempotent-safe (same contract as Smalltalk's `super initialize`)
- When slot initializers (Option D) arrive, developers must remove `super initialize` lines (mechanical but required)

### Neutral
- `super` dispatch for `initialize` uses the existing `beamtalk_dispatch:super/5` — no new runtime mechanism
- The `__skip_initialize__` flag mechanism in `init/1` remains unchanged
- BT-1949's post-initialize check in `handle_continue` is extended, not replaced

## Implementation

### Phase 1: Compiler Warning + Super Safety (S)
These must ship together — the warning tells developers to add `super initialize`, so `super initialize` must be safe before the warning fires.
- **Type checker**: When checking an `initialize` method, walk the superclass chain via `ClassHierarchy` to find ancestor typed-no-default fields. If any exist and the method body has no `super` send with selector `initialize`, emit a warning.
- **Runtime verification**: Confirm `super initialize` is safe when no intermediate ancestor overrides `initialize` (reaches `Actor`'s no-op default). This should already work — `Actor.bt` defines `initialize -> Nil => nil` — but needs a test to prove it.
- **Files**: `crates/beamtalk-core/src/semantic_analysis/type_checker/`
- **Tests**:
  - BUnit test: subclass that omits `super initialize` — verify warning is emitted
  - BUnit test: actor subclass of Actor base calling `super initialize` — verify no error

### Phase 2: Inherited Field Validation (S)
- **Codegen**: Extend `generate_post_initialize_check` in `callbacks.rs` to walk the superclass chain and collect all typed-no-default fields, not just the current class's own. BT-1951 covers this.
- **Files**: `crates/beamtalk-core/src/codegen/core_erlang/gen_server/callbacks.rs`
- **Test**: BUnit test with parent typed-no-default field, child forgets to init → `UninitializedStateError`

### Future Evolution: Declarative Slot Initializers
When the language evolves to support initializer expressions on state declarations (e.g., `state: db :: Database = Database connect`), the `super initialize` pattern becomes optional:
- Slot initializers would run automatically for each class in the hierarchy
- `initialize` methods would be used only for imperative post-construction logic
- The compiler warning for missing `super initialize` would be removed for classes where all inherited typed-no-default fields have slot initializers

**Migration from B → D**: When slot initializers land, the compiler should emit a new warning: "`super initialize` is unnecessary — parent fields have slot initializers." This gives developers a clear signal to remove the `super initialize` line. The migration is mechanical: delete `super initialize`, verify tests pass.

## Migration Path

No migration needed — this is purely additive:
- Existing code without typed-no-default fields: no change
- Existing code with typed-no-default fields: gets a warning if `super initialize` is missing
- The warning can be suppressed with `@expect` if the programmer knows what they're doing

## References
- Related issues: BT-1949 (UninitializedStateError check), BT-1951 (inherited field validation)
- Related ADRs: ADR 0065 (OTP primitives for actor lifecycle), ADR 0067 (state/field keywords)
- Prior art: Pharo `super initialize`, Newspeak slot initializers, Swift designated initializers
- Documentation: `docs/beamtalk-language-features.md` (Actor Lifecycle Hooks)
