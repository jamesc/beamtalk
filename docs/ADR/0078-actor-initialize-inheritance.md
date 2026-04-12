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

```
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
```
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
Initialization failures are caught immediately at spawn time (fail-fast), not as mysterious `doesNotUnderstand` on `nil` later. Clear error message identifies the uninitialized field.

## Steelman Analysis

### Alternative A: Auto-chain initialize up the hierarchy
- **Newcomer**: "Just works — I don't have to think about parent setup"
- **Smalltalk purist**: "Implicit magic — I lose control of initialization order"
- **BEAM veteran**: "Hidden gen_server callbacks I can't see in traces"
- **Language designer**: "Creates a future breaking change if we move to slot initializers — auto-chaining would need to be removed"

**Rejected because**: Auto-chaining adds implicit behavior that conflicts with declarative slot initializers. Consider: if a parent declares `state: db :: Database = Database connect` (slot initializer), the slot initializer runs at construction time. If auto-chaining also runs the parent's `initialize` (which may also call `Database connect`), the field gets initialized twice — once declaratively, once imperatively. Removing auto-chaining to fix this would be a breaking change for any code that relied on it. With explicit `super initialize`, the programmer simply deletes the `super initialize` line when they add a slot initializer — no implicit behavior to undo.

### Alternative C: Validate-only (flat initialization)
- **Newcomer**: "Simple — one method sets everything"
- **Smalltalk purist**: "Violates encapsulation — child must know parent's initialization internals"
- **BEAM veteran**: "Most predictable — one init callback, easy to trace"
- **Language designer**: "Forces DRY violations and breaks when parent adds new typed fields"

**Rejected because**: Forcing the child to duplicate parent initialization logic is fragile and violates encapsulation. When a parent adds a new typed-no-default field, all subclasses must be updated — even if they have no knowledge of the parent's internals.

### Alternative D: Declarative slot initializers (Newspeak-style)
- **Newcomer**: "Most readable — each field declares its own setup"
- **Smalltalk purist**: "Newspeak's best idea — and it eliminates super-init entirely"
- **BEAM veteran**: "Concerned about evaluation order and error handling complexity"
- **Language designer**: "The right long-term answer, but XL implementation effort"

**Noted as future direction**: See Future Evolution below.

### Tension Points
- Newcomers and Smalltalk purists agree on Option D long-term but differ on the interim solution
- BEAM veterans prefer the simplest runtime model (C) but accept B's explicitness
- The key insight: B is the only option that doesn't create a future migration burden when D arrives

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
