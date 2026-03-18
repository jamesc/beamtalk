# ADR 0067: Separate state:/field: Keywords by Class Kind

## Status
Accepted (2026-03-18)

## Context

Beamtalk has three class kinds — Object, Value, and Actor — but currently accepts the `state:` keyword on all of them. This creates a footgun: an `Object subclass:` with `state:` compiles silently, but updates via `self.slot :=` return an updated snapshot that callers must rebind (e.g., `c := c increment`). Without rebinding, `c` still references the original value and changes appear lost.

**The footgun in action:**
```beamtalk
Object subclass: Counter
  state: count = 0

  increment => self.count := self.count + 1
  value => self.count

c := Counter new.
c increment.
c value              // => 0   — c was not rebound to the updated snapshot
```

The assignment `self.count := self.count + 1` compiles and runs — `increment` returns an updated snapshot, but callers must rebind (e.g., `c := c increment`). Without rebinding, `c` still references the original value. Object subclasses have no process to persist state automatically, and the returns-new-self semantics are implicit, making it easy to silently lose updates.

ADR 0042 established the two-category object model (immutable Value, mutable Actor) but left `Object subclass:` in an ambiguous middle ground. A codebase audit found 21 `Object subclass:` files with `state:` declarations across the repository — 8 in stdlib test fixtures, 7 in e2e test fixtures, and 6 in example projects. More critically, the same footgun appears in real-world Beamtalk applications:

**Symphony** (agent orchestration platform) has 5 `Object subclass:` classes with `state:` declarations — `WorkspaceManager`, `LinearClient`, `Config`, `CandidateFilter`, `PromptRenderer` — all configured once and used immutably. Every one should be a `Value subclass:`. Any mutation would silently fail.

**Exdura** (workflow engine) correctly uses `Object subclass:` for abstract bases designed for subclassing (`Activity`, `Workflow`) — these are method-only and work as intended. This validates that the method-only Object pattern is natural for framework extension points.

The footgun is not theoretical. Real-world projects are hitting it.

Meanwhile, `state:` is used for both mutable actor state and immutable value fields — the same keyword with fundamentally different semantics depending on the superclass. This is confusing for newcomers and Smalltalk developers alike.

### The role of Object subclass

Existing stdlib Object subclasses fall into clear categories:

**Method-only utility classes** (no instance data needed):
- `Ets`, `AtomicCounter` — wrap Erlang FFI, hold no Beamtalk state
- `File`, `Logger`, `Random`, `System`, `OS`, `Timer` — static API surfaces
- `Json`, `Yaml` — serialisation utilities
- `Exception` — wraps `#beamtalk_error{}` records via primitives

**Abstract protocol providers:**
- `Collection` — abstract typed root for Set, Bag, List, etc.
- `Behaviour`, `Supervisor`, `DynamicSupervisor` — abstract OTP integration

**Framework classes:**
- `TestCase` — test framework base (currently uses `state:` via subclasses; addressed in Migration)

These classes work perfectly without instance data. Object serves three roles, none of which require instance data:

1. **Protocol provider** — common methods inherited by all Value and Actor subclasses: `isNil`, `respondsTo:`, `printString`, `hash`, `error:`, `yourself`
2. **FFI namespace** — zero-overhead class-method wrappers around Erlang modules and OTP primitives (Json, System, File, Ets, Random). No instances, no process, no deadlock risk. User-defined Object subclasses can use the same pattern to wrap Erlang NIFs, ports, or ETS tables.
3. **Abstract extension point** — framework contracts designed for subclassing, where subclasses define methods but hold no data (Supervisor, DynamicSupervisor, Collection, and external examples like Exdura's Activity and Workflow)

## Decision

Enforce a three-kind data model where instance data keywords are restricted by class kind:

| Class Kind | Data Keyword | Semantics | Construction | Instance Process |
|------------|-------------|-----------|--------------|-----------------|
| **Actor** | `state:` (permitted, not required) | Mutable process state, `self.slot :=` persists via gen_server | `spawn` / `spawnWith:` | Yes |
| **Value** | `field:` | Immutable data slots, `self.slot :=` is compile error | `new` / `new:` / keyword ctor | No |
| **Object** | *(none)* | Methods only, no instance data, not instantiable | *(none)* | No |

**Note on "Instance Process":** This refers to whether each *instance* is backed by a gen_server process for Beamtalk-managed state. Object subclasses may still wrap external processes — e.g. `Supervisor` starts an OTP supervisor via Erlang FFI, and `AtomicCounter` wraps ETS tables. The restriction is on Beamtalk `state:`/`field:` declarations, not on what Erlang backing code does. Similarly, `@native` Actors (ADR 0056) have an instance process but no `state:` declarations — their state lives entirely in the hand-written Erlang gen_server. All class kinds have a class-level gen_server (the metaclass tower from ADR 0036), which is why `classState:` works on all kinds.

### Keyword semantics

**`state:` — Actor only:**
```beamtalk
Actor subclass: Counter
  state: count :: Integer = 0

  increment => self.count := self.count + 1   // persists in gen_server state
  value => self.count
```

**`field:` — Value only:**
```beamtalk
Value subclass: Point
  field: x :: Integer = 0
  field: y :: Integer = 0

  plus: other => Point x: (self.x + other x) y: (self.y + other y)
  // self.x := 5   ← compile error: immutable value field
```

**Object — class methods only, not instantiable:**
```beamtalk
Object subclass: MathHelper
  class factorial: n =>
    n <= 1
      ifTrue: [1]
      ifFalse: [n * (self factorial: n - 1)]
```

### REPL examples

```beamtalk
// Value — immutable, structural equality
p1 := Point x: 3 y: 4.
p2 := p1 withX: 5.           // => Point(x: 5, y: 4)
p1                            // => Point(x: 3, y: 4) — unchanged

// Actor — mutable process, identity equality
c := Counter spawn.
c increment.
c value                       // => 1 — state persisted

// Object — class methods only, not instantiable
MathHelper factorial: 5       // => 120

// Handles to external mutable state — use Value
sealed Value subclass: UserCache
  field: table

  class new => super new: (Ets new: #userCache type: #set)
  at: key => self.table at: key
  at: key put: value =>
    self.table at: key put: value.
    self

cache := UserCache new.
cache at: "james" put: user1.
cache at: "james"             // => user1
```

### Error examples

```beamtalk
// state: on a Value → compile error
Value subclass: BadValue
  state: x = 0
// ⛔ error: use 'field:' for Value subclass data declarations, not 'state:'

// field: on an Actor → compile error
Actor subclass: BadActor
  field: x = 0
// ⛔ error: use 'state:' for Actor subclass data declarations, not 'field:'

// state: or field: on an Object → compile error
Object subclass: BadObject
  state: x = 0
// ⛔ error: Object subclass cannot have instance data declarations;
//    use 'Value subclass:' for immutable data or 'Actor subclass:' for mutable state

// self.slot := on a Value → existing compile error (unchanged from ADR 0042)
Value subclass: ImmutablePoint
  field: x = 0
  broken => self.x := 5
// ⛔ error: cannot assign to field on immutable Value type

// new/new: on an Object → compile error (Object is not instantiable)
MathHelper new
// ⛔ error: Object subclass is not instantiable — use class methods directly

// new/new: on an Actor → compile error
Counter new
// ⛔ error: Actor subclass uses 'spawn', not 'new'
```

### Construction protocol

Each class kind has its own construction mechanism. `new`/`new:` move from Object to Value:

| Kind | Constructor | Rationale |
|------|------------|-----------|
| **Value** | `new` / `new:` / keyword ctor | Creates immutable data |
| **Actor** | `spawn` / `spawnWith:` | Creates gen_server process |
| **Object** | *(not instantiable)* | Class-method namespace, no instances needed |

Currently Actor inherits `new`/`new:` from Object and overrides them to throw `InstantiationError` at runtime — a code smell where methods are inherited just to be broken. With class kinds enforced at compile time, `new`/`new:` on Actor or Object becomes a compile error, and the runtime overrides in Actor.bt are removed.

**Exception impact:** All ~30 usages of `SomeError new signal: "msg"` become `SomeError signal: "msg"` via a new class-side `signal:` primitive. This is acceptable since `signal`/`signal:` are already intrinsics. See [BT-1524](https://linear.app/beamtalk/issue/BT-1524) for implementation.

### What stays the same

- `classState:` remains available on all class kinds (class-level mutable state lives in the class object's gen_server)
- Auto-generated getters work the same for both `field:` and `state:`
- Auto-generated `with*:` methods work the same for both
- Auto-generated keyword constructors work the same for both
- `fieldAt:` / `fieldAt:put:` reflection works the same

### TestCase migration

`TestCase` is currently `Object subclass:` and its subclasses use `state:` for setUp instance variables. Under the new model, TestCase becomes `Value subclass: TestCase` — setUp returns a new self with fields set, and the test runner passes this value to each test method. This follows the same functional pattern used by Erlang's EUnit and Elixir's ExUnit.

```beamtalk
// Before (broken footgun):
Object subclass: TestCase
  state: counter = nil

  setUp => self.counter := Counter spawn
  testIncrement =>
    self.counter increment.
    self assert: (self.counter value) equals: 1

// After (Value with functional setUp):
Value subclass: TestCase
  field: counter = nil

  setUp => self withCounter: (Counter spawn)
  testIncrement =>
    self.counter increment.
    self assert: (self.counter value) equals: 1
```

**Why Value, not Actor:**
1. **No process overhead** — no gen_server spawn/stop per test, no message-passing for setUp/test/tearDown
2. **No special cases** — TestCase follows the same rules as every other Value subclass
3. **Proven pattern** — both EUnit and ExUnit use functional setUp (return fixture data, pass to tests). Neither uses mutable state for test fixtures.
4. **Better isolation** — each test gets a fresh copy of the setUp'd value; no possibility of one test corrupting state for the next
5. **No Erlang runtime changes** — `beamtalk_test_case.erl` already threads state between setUp → test → tearDown; it just needs to use the return value from setUp instead of the mutated self

**Syntax change:** `self.counter := Counter spawn` becomes `self withCounter: (Counter spawn)`. For multiple fields, `with*:` calls chain via cascades:

```beamtalk
setUp =>
  self withCounter: (Counter spawn);
       withDb: (DB connect)
```

## Prior Art

### Smalltalk (Pharo/Squeak)
No language-level value/mutable distinction. All objects can hold instance variables. Immutability is convention only — no keyword distinguishes mutable from immutable slots. ProtoObject provides minimal protocol; Object adds standard methods. Both can hold instance variables.

**What we take:** ProtoObject/Object split for minimal vs full protocol.
**What we reject:** Universal instance variables. On the BEAM, there is no mutable heap — the platform has two fundamentally different execution models (processes vs pure functions), and the language should make that distinction legible.

### Newspeak
Closest prior art as a Smalltalk descendant. Distinguishes mutable (`::=`) from immutable (`=`) slots at the field level. A `Value` mixin enforces deep immutability at the class level — all slots must be immutable, and they may only hold other values.

**What we take:** The idea that class-level markers constrain field mutability. Beamtalk's `field:` vs `state:` is analogous to Newspeak's `=` vs `::=`, but tied to class kind rather than slot operator.
**What we adapt:** Newspeak allows mixed mutable/immutable slots in a single class. Beamtalk restricts by class kind — simpler, matches BEAM semantics.

### Elixir
Structs (immutable data) vs GenServer (mutable process state) are completely different constructs — `defstruct` vs `use GenServer`. Developers choose based on whether state must survive across message boundaries.

ExUnit (Elixir's test framework) validates the functional test fixture pattern: `setup` blocks return a context map, which is pattern-matched in each test's function head. No mutable state, no process per test:

```elixir
setup do
  pid = start_supervised!(MyServer)
  %{server: pid}          # returned context map
end

test "increment", %{server: pid} do
  assert MyServer.get(pid) == 0
end
```

**What we take:** The clean separation maps directly to our Value/Actor split. ExUnit's functional setup pattern validates our Value TestCase design. Elixir validates that this works on the BEAM.
**What we adapt:** Elixir uses different syntax entirely. Beamtalk uses the same class-based syntax with different keywords, keeping the object model unified.

### Gleam
Everything is immutable. Custom types are algebraic data types. Mutable state requires an actor (process). No choice to make for data — it's always immutable.

**What we take:** Validation that "immutable data + mutable actors" is the natural BEAM model.

### Swift (struct vs class)
Two declaration keywords: `struct` (value type, copy semantics) vs `class` (reference type, shared semantics). Fields use `let` (immutable) vs `var` (mutable). Apple guidance: "prefer structs."

**What we take:** Two-level distinction — class kind AND field keyword. Swift validates that making value/reference semantics explicit at the declaration site eliminates an entire category of bugs.

### Pony (reference capabilities)
Three class kinds: `class` (mutable, single-actor), `actor` (identity only, message-send), `primitive` (immutable, shareable). Fields use `let` (assign once) vs `var` (reassignable). Reference capabilities provide compile-time data-race freedom.

**What we take:** The class-kind-restricts-field-mutability pattern. Pony's `class`/`actor`/`primitive` maps conceptually to our Object/Actor/Value.
**What we reject:** Full reference capabilities are overkill — BEAM process isolation already provides data-race safety.

### Kotlin (data class vs class)
`data class` auto-generates `equals`, `hashCode`, `copy`, `toString`. Properties use `val` (read-only) vs `var` (mutable). `value class` provides zero-overhead type-safe wrappers.

**What we take:** Auto-derived behaviour by class kind. Beamtalk value classes get structural equality; actor classes get identity equality — matching Kotlin's `data class` pattern.

### Summary

| Language | Value Mechanism | Mutable Mechanism | Class-Kind Restriction | Field-Level Control |
|----------|----------------|-------------------|----------------------|---------------------|
| Pharo | Convention | Convention | None | None |
| Newspeak | `=` slot + Value mixin | `::=` slot | Opt-in mixin | `=` vs `::=` |
| Elixir | `defstruct` | GenServer | Different constructs | N/A (all immutable) |
| Gleam | Custom type | Actor (process) | Different constructs | N/A (all immutable) |
| Swift | `struct` | `class` | `struct` vs `class` | `let` vs `var` |
| Pony | `val` rcap | `ref` rcap | `class`/`actor`/`primitive` | `let` vs `var` |
| Kotlin | `data class` | `class` | Different keywords | `val` vs `var` |
| **Beamtalk** | **`Value` + `field:`** | **`Actor` + `state:`** | **Value/Actor/Object** | **`field:` vs `state:`** |

Every language that distinguishes value from mutable does it at two levels: class/type level AND field/property level. Beamtalk's proposal follows this established pattern.

## User Impact

### Newcomer (from Python/JS/Ruby)
**Positive:** No more silent state loss. The compiler tells you exactly what to do — `state:` for Actor, `field:` for Value, nothing for Object. The error messages guide you to the right choice.

**Learning curve:** Must understand the Value/Actor distinction up front. But this is a BEAM concept they'll need to learn anyway, and making it syntactically visible is better than discovering it via mysterious runtime behaviour.

### Smalltalk developer
**Friction:** In Smalltalk, any class can hold instance variables. Making Object method-only breaks that expectation.

**Mitigation:** The footgun this prevents (silent state loss) doesn't exist in Smalltalk because Smalltalk has a mutable heap. The restriction is honest about what the BEAM platform actually provides. Newspeak (a Smalltalk descendant) already has the `Value` mixin concept.

### Erlang/BEAM developer
**Natural fit:** Maps directly to BEAM concepts — `field:` is a record/map, `state:` is gen_server state. Object-with-no-data is a behaviour module. No surprises.

### Production operator
**Improved observability:** If you see `state:`, you know there's a process. If you see `field:`, you know it's a map. No ambiguity about where state lives or how to debug it.

### Tooling developer (LSP/IDE)
**Improved analysis:** The parser/AST carries the class kind, so completions can offer `state:` only in Actor context and `field:` only in Value context. Error diagnostics are straightforward — wrong keyword for class kind.

## Steelman Analysis

### Alternative B: Lean Into Existing Object Snapshot Semantics

Keep `state:` on Object, leaning into the existing snapshot/rebinding model where `self.slot :=` already returns an updated value and callers rebind.

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "I shouldn't have to understand the Value/Actor distinction just to hold some data. Let me start with Object, and I'll graduate to Actor when I need a process." |
| **Smalltalk purist** | "Closer to Smalltalk's uniform model — every class can hold data. The BEAM is an implementation detail; the language shouldn't leak it into the class hierarchy." |
| **BEAM veteran** | "Elixir's structs are exactly this — immutable maps with update syntax. The pattern is proven, and having it as the default Object behaviour would feel natural to Elixir developers." |
| **Operator** | "Fewer class kinds to reason about at runtime — two execution models (process / no-process) is simpler to monitor than three class kinds." |
| **Language designer** | "A returns-new-self Object preserves the uniform object model — every class can hold data, which is more composable and requires fewer special cases in the language." |

**Why we rejected it:** Object already has snapshot/rebinding semantics — this alternative just makes them the documented, endorsed model. But that is exactly what Value already does. Having both Object-with-explicit-returns-new-self AND Value creates two ways to do the same thing, and users constantly ask "should this be Object or Value?" The ETS-backed Value pattern (immutable handle to external mutable state) proves that Value alone handles the "I need mutable external state without a process" case cleanly.

### Alternative C: Status Quo + Compiler Warnings

Keep `state:` everywhere, add warnings on Object subclass with state.

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "Warnings let me learn at my own pace — the compiler tells me what to fix without blocking my exploration. I can focus on getting something working first and clean up the class kinds later." |
| **Smalltalk purist** | "Smalltalk survived 40+ years with mutable instance variables on every class. The problem isn't the language model, it's that the BEAM handles mutation differently. A warning educates; a hard error dictates." |
| **BEAM veteran** | "Zero migration cost. Existing code keeps compiling. Warnings appear in CI logs and get fixed incrementally. This is the Erlang way — deprecate gently, don't break." |
| **Operator** | "Gradual rollout means no big-bang migration. I can track warning counts in CI and drive them to zero on my own timeline rather than being forced to fix everything at once." |
| **Language designer** | "Warnings preserve backward compatibility while signalling intent. Beamtalk is pre-1.0 — but establishing a pattern of breaking changes now sets a bad precedent for the ecosystem as it grows." |

**Why we rejected it:** Warnings are ignorable. The footgun is severe enough (silent state loss) that it should be a hard error. If we're going to warn on every `Object subclass:` with `state:`, we're admitting the code is wrong — so make it a compile error and guide users to the right choice. However, the deprecation-period approach (Phase 2 warnings before Phase 4 hard errors) incorporates this alternative's strength — existing code keeps compiling during the transition.

### Alternative D: Ban state: on Object, no field: keyword

Keep `state:` as the only data keyword for both Value and Actor. Just make `state:` on Object a compile error.

| Cohort | Strongest argument |
|--------|-------------------|
| **Newcomer** | "One keyword to learn, not two. The class declaration already tells me if it's Value or Actor — why do I need the field to repeat that information?" |
| **Smalltalk purist** | "`state:` is already established vocabulary. Adding `field:` when the class kind already encodes mutability is ceremony for ceremony's sake." |
| **BEAM veteran** | "80% of the value for 20% of the cost. The footgun is eliminated, no new keyword to learn, no migration for existing Value files." |
| **Operator** | "Smaller change = less risk. I'd rather ship the Object ban now and debate `field:` later if we find `state:` on Value is actually confusing in practice." |
| **Language designer** | "Redundant information in syntax is a smell. Swift needs `let`/`var` because a single `class` can have both. In Beamtalk, Value means ALL fields are immutable — the keyword carries zero new information." |

**Why we rejected it:** `state:` carrying fundamentally different semantics depending on class kind — immutable on Value, mutable on Actor — is exactly the kind of non-local reasoning this ADR aims to eliminate. When an agent or developer reads `state: x = 0`, they must look up the class hierarchy to know if `x` is mutable. With `field:`, the keyword itself carries the semantics: `field:` = fixed data, `state:` = changeable state. This is local information — you know the mutability contract without looking anywhere else. Every reference language with this distinction (Swift `let`/`var`, Pony `let`/`var`, Newspeak `=`/`::=`, Kotlin `val`/`var`) uses separate keywords precisely because local readability trumps minimalism.

### Tension Points

- **Option D is the strongest rejected alternative.** It solves the primary problem (silent state loss) with the least disruption. The `field:` keyword's value is in local readability — whether that justifies migration of 12+ Value files is a judgement call. We chose readability.
- Smalltalk purists prefer B or C, but acknowledge the BEAM makes A necessary
- BEAM veterans are split between A and D — both are pragmatic, A is more thorough
- The "bookkeeping slot" concern ("what about Object subclasses that need one field?") is addressed by Value: if it needs data, it *is* a value on the BEAM. There is no third runtime representation.

## Alternatives Considered

### Allow state: on Object with deprecation warnings (Option C)

Add compiler warnings for `state:` on `Object subclass:` declarations, pointing users to Value or Actor.

```beamtalk
Object subclass: Point
  state: x = 0    // ⚠️ warning: state: on Object subclass is deprecated;
                   //    use 'Value subclass:' with 'field:' for immutable data
```

Rejected because warnings are ignorable, and the silent state loss footgun is severe enough to warrant a hard error. Half-measures prolong the confusion.

### Lean into existing Object snapshot semantics (Option B)

Keep `state:` on Object, leaning into the existing snapshot/rebinding model where `self.slot :=` already returns an updated value and callers rebind.

```beamtalk
Object subclass: Point
  state: x = 0
  state: y = 0

  moveRight: dx =>
    self.x := self.x + dx    // already returns new Point; caller rebinds
```

Rejected because this is exactly what Value already does. Endorsing the same semantics on both Object and Value creates two ways to do the same thing without adding capability. The conversation that led to this ADR confirmed this: when asked "what's the client code difference?", there was none.

### Just ban state: on Object, keep state: for Value and Actor (Option D)

Make `state:` on `Object subclass:` a compile error, but keep `state:` as the keyword for both Value and Actor. No `field:` keyword introduced.

```beamtalk
Value subclass: Point
  state: x = 0    // same keyword as Actor, but immutable because Value

Actor subclass: Counter
  state: count = 0    // same keyword, but mutable because Actor
```

This captures 80% of the value (eliminates the Object footgun) with much less change (no new keyword, no migration of existing Value files). Rejected because `state:` carrying fundamentally different semantics depending on class kind — immutable on Value, mutable on Actor — is the confusion this ADR aims to resolve. Two keywords that match their semantics (`field:` = fixed data, `state:` = changeable state) is clearer than one overloaded keyword. The prior art (Swift `let`/`var`, Pony `let`/`var`, Newspeak `=`/`::=`, Kotlin `val`/`var`) consistently shows that languages benefit from separate keywords for immutable vs mutable data.

### field: as slot-level annotation (Newspeak-style)

Allow both `field:` and `state:` on any class kind, letting developers choose per-slot.

```beamtalk
Actor subclass: CachedService
  state: cache = #{}       // mutable
  field: name = ""         // immutable
```

Rejected because mixed mutability within a single class is confusing and doesn't map cleanly to BEAM runtime representations. An Actor's state is a single map — all fields are either in the gen_server state or they aren't.

## Consequences

### Positive
- Eliminates the silent state loss footgun entirely — a hard compile error replaces mysterious runtime behaviour
- Makes the Value/Actor choice explicit and visible at the declaration site
- `field:` and `state:` keywords carry semantic meaning — readers immediately know the mutability contract
- Better IDE/LSP support — completions and diagnostics can use class kind context
- Aligns with established patterns in Swift, Pony, Kotlin, and Newspeak
- Validates the "immutable handle to external mutable state" pattern (ETS-backed Values)

### Negative
- Breaking change for existing `Object subclass:` with `state:` (21 files identified, all in tests/examples)
- Smalltalk developers lose the ability to add instance variables to any class
- `field:` is a new keyword that must be learned
- TestCase migrates from Object to Value, requiring all test subclass setUp methods to change from `self.x := value` to `self withX: value` — a mechanical but widespread syntax change

### Neutral
- `classState:` is unchanged — all class kinds can have class-level mutable state
- Auto-generated getters, `with*:` methods, and keyword constructors are unchanged
- Value types continue to use `state:` internally during the deprecation period (renamed to `field:` in migration)
- ProtoObject/Object hierarchy is unchanged — Object remains the common protocol provider
- Collection hierarchy requires reclassification: `Collection` is currently `abstract typed Object subclass:`, but its subclasses (Set, Bag, Interval) have `state:` declarations and are semantically immutable values. Collection must become `abstract typed Value subclass: Collection` so its subclasses can use `field:`. This is a mechanical change — Collection has no `state:` declarations of its own, and all Collection subclasses already behave as values (operations return new collections)
- Queue and Stream are `Object subclass:` with no `state:` declarations (FFI-backed). They remain valid Object subclasses under the new model, though Queue is semantically a value type (its doc says "It is a value type")

## Implementation

### Phase 1: Parser support for `field:` keyword and ClassKind propagation
- Add `field:` as a synonym for `state:` in the parser
- Both keywords parse to `StateDeclaration` in the AST (internal representation unchanged)
- Add a `declared_keyword` field to `StateDeclaration` to track which keyword was used
- **ClassKind propagation:** Currently `ClassKind::from_superclass_name` only recognises the literal names `"Actor"` and `"Value"` — so `Collection subclass: Set` gets `ClassKind::Object` even if Collection inherits from Value. The semantic analysis phase must resolve ClassKind by walking the class hierarchy: if any ancestor is Value, the subclass is Value; if any ancestor is Actor, the subclass is Actor. This is required for Collection subclasses (Set, Bag, Interval) and TestCase subclasses to inherit the correct kind.
- **Affected:** `crates/beamtalk-core/src/parser/`, `crates/beamtalk-core/src/ast.rs`, `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/mod.rs`

### Phase 2: Compiler warnings (deprecation period)
- Warn when `state:` is used on `Value subclass:` ("use field: instead")
- Warn when `field:` is used on `Actor subclass:` ("use state: instead")
- Warn when `state:` or `field:` is used on `Object subclass:` ("Object cannot have instance data")
- **Affected:** `crates/beamtalk-core/src/semantic_analysis/validators/class_validators.rs`

### Phase 3: Migrate existing code
- Update all 21 `Object subclass:` files with `state:` to use `Value subclass:` with `field:`
- Reclassify `Collection` from `abstract typed Object subclass:` to `abstract typed Value subclass:` (enables subclasses to use `field:`)
- Update existing `Value subclass:` files to use `field:` instead of `state:` (including `Value.bt`'s own doc comment example)
- Migrate `TestCase` from `Object subclass:` to `Value subclass:` with `field:` — the Erlang test runner (`beamtalk_test_case.erl`) already threads state between setUp → test → tearDown; it needs minimal changes to use the return value from setUp as the test fixture
- Update examples, test fixtures, and documentation
- **Affected:** stdlib, tests, examples, docs

### Phase 4: Hard errors
- Promote warnings to compile errors
- `state:` on non-Actor → error
- `field:` on non-Value → error
- Any data declaration on Object → error
- **Affected:** `crates/beamtalk-core/src/semantic_analysis/validators/class_validators.rs`

### Phase 5: Documentation and tooling
- Update language features documentation
- Update LSP completions to offer `field:` in Value context, `state:` in Actor context
- Update error messages with migration guidance
- **Affected:** docs, LSP, REPL help

## Migration Path

### Automated migration

A compiler warning in Phase 2 identifies all affected files. The fix is mechanical:

**Object subclass with state: → Value subclass with field:**
```beamtalk
// Before:
Object subclass: Point
  state: x = 0
  state: y = 0

// After:
Value subclass: Point
  field: x = 0
  field: y = 0
```

**Value subclass with state: → field:**
```beamtalk
// Before:
sealed Value subclass: HTTPRequest
  state: method :: String = ""

// After:
sealed Value subclass: HTTPRequest
  field: method :: String = ""
```

**TestCase → Value subclass (functional setUp):**
```beamtalk
// Before:
Object subclass: TestCase

// After:
Value subclass: TestCase

// Test subclass before:
TestCase subclass: CounterTest
  state: counter = nil
  setUp => self.counter := Counter spawn

// Test subclass after:
TestCase subclass: CounterTest
  field: counter = nil
  setUp => self withCounter: (Counter spawn)
```

### Migration inventory

**21 files** need `Object subclass:` → `Value subclass:` + `state:` → `field:`:

| Location | Files | Classes |
|----------|-------|---------|
| stdlib/test/fixtures/ | 8 | Point, ClassSelfDispatch, NlrFieldMutation, ValueTypeBuilder, NlrNestedHom, StringFormatter, ClassVarPoint, Shape |
| tests/e2e/fixtures/ | 7 | Point, Transaction, Message, Alpha, Beta, Beverage, Tag |
| examples/gof-patterns/ | 6 | CelsiusThermometer, HtmlElement, HtmlBuilder, CommandHistory, TextBuffer, Sorter |

**Collection reclassification** (1 file):
`stdlib/src/Collection.bt`: `abstract typed Object subclass:` → `abstract typed Value subclass:` (enables subclasses Set, Bag, Interval to use `field:`)

**Stdlib Value subclasses** needing `state:` → `field:` rename (8 files):
Result, HTTPRequest, HTTPResponse, HTTPRoute, HTTPRouter, SupervisionSpec (stdlib/src), ValuePoint, InspectPair (stdlib/test/fixtures)

**Collection subclasses** needing `state:` → `field:` after Collection reclassification (3 files):
Set, Bag, Interval (currently `Collection subclass:`, will inherit Value kind after BT-1532)

**Doc comment update:**
`stdlib/src/Value.bt`: doc comment example uses `state: x = 0` — update to `field: x = 0`

**Special case — TestCase:**
`stdlib/src/TestCase.bt`: `Object subclass:` → `Value subclass:` with `field:` (subclasses migrate `state:` → `field:`, setUp syntax changes from `self.x := value` to `self withX: value`)

**External projects** requiring migration (identified during audit):
- **Symphony:** 5 files — WorkspaceManager, LinearClient, Config, CandidateFilter, PromptRenderer (`Object subclass:` with `state:` → `Value subclass:` with `field:`)
- **Exdura:** 0 files affected — Object subclasses (Activity, Workflow) are method-only, no `state:` declarations

### Timeline

- Phase 1–2 can ship together (parser + warnings)
- Phase 3 can be a single migration PR
- Phase 4 ships after one release cycle with warnings
- Phase 5 is ongoing

## Implementation Tracking

**Epic:** [BT-1526](https://linear.app/beamtalk/issue/BT-1526)
**Status:** Planned

| Phase | Issue | Title | Size |
|-------|-------|-------|------|
| 1 | BT-1527 | Parse `field:` keyword and track declared keyword in AST | S |
| 1 | BT-1528 | ClassKind hierarchy propagation | M |
| 2 | BT-1529 | Compiler warnings for wrong keyword/class-kind combinations | M |
| 3 | BT-1530 | Migrate stdlib Value subclasses: `state:` → `field:` | S |
| 3 | BT-1531 | Migrate Object-with-state fixtures to Value subclass | S |
| 3 | BT-1532 | Reclassify Collection as Value subclass | M |
| 3 | BT-1533 | Convert TestCase to Value subclass with functional setUp | M |
| 3 | BT-1534 | Migrate test subclass setUp syntax | S |
| 4 | BT-1535 | Promote keyword/class-kind warnings to compile errors | S |
| 5 | BT-1536 | Update language features documentation | S |
| 5 | BT-1537 | LSP completions for field:/state: by class kind | S |

**Follow-up:** [BT-1524](https://linear.app/beamtalk/issue/BT-1524) (Object non-instantiable, move new/new: to Value, Exception class signal:)

## References
- Related issues: [BT-1520](https://linear.app/beamtalk/issue/BT-1520/adr-separate-statefield-keywords-by-class-kind), [BT-1524](https://linear.app/beamtalk/issue/BT-1524/make-object-non-instantiable-move-newnew-to-value-add-class-signal-to) (Object non-instantiable)
- Related ADRs: [ADR 0005](0005-beam-object-model-pragmatic-hybrid.md) (BEAM Object Model), [ADR 0042](0042-immutable-value-objects-actor-mutable-state.md) (Immutable Value Objects)
- Newspeak Value mixin: [Newspeak Specification](https://newspeaklanguage.org/spec/newspeak-spec.pdf)
- Swift struct vs class: [Value and Reference Types](https://www.swift.org/documentation/articles/value-and-reference-types.html)
- Pony reference capabilities: [Tutorial](https://tutorial.ponylang.io/reference-capabilities/reference-capabilities.html)
