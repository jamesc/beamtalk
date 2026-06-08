# ADR 0094: Object String Representation Protocols (printString / displayString / inspect)

## Status
Accepted (2026-06-08)

## Implementation Tracking

**Epic:** BT-2458 — Object String Representation Protocols (ADR 0094)

**Issues:**

| # | Issue | Phase | Size |
|---|---|---|---|
| 1 | BT-2459 — Shared structural renderer runtime primitive | 1 (Foundation) | M |
| 2 | BT-2460 — Repoint `Value` printString to renderer; decouple recursion from `inspect` | 2 (Core) | M |
| 3 | BT-2461 — Drop the `a`/`an` prefix: default `Object` printString to bare class name | 2 (Core) | S |
| 4 | BT-2462 — Actor/Supervisor `Kind(Class, pid)` + remove hard-coded REPL paths (timeout fallback) | 2 (Core) | M |
| 5 | BT-2463 — E2E btscript test + interpolation hook | 3 (Validation) | S |
| 6 | BT-2464 — Document `printString`/`displayString` contract + surface parity | 3 (Docs) | M |

**Deferred follow-up (`inspect` as tooling/action verb):** BT-2397.

**Status:** Planned

## Context

### The problem

Beamtalk inherits Smalltalk's default object descriptor: an instance with no
custom printing renders as `"a ClassName"` (`"a Point"`, `"a Counter"`). This is
the first thing a developer sees when they evaluate a fresh domain object at the
REPL, and it is almost useless — it tells you the class (which you usually
already know) and nothing about the instance's state.

Three problems compound this:

1. **The default is worse than Pharo's.** Pharo's `Object>>printOn:` is
   article-aware (`an OrderedCollection`). Beamtalk's default
   (`Object.bt:92`) is a bare `"a " ++ self class printString` with no vowel
   handling, so it produces ungrammatical output like `"a Integer"`,
   `"a Object"`.

2. **The good output exists but is unreachable by default.** `Value` already has
   a structural `inspect` (`Value.bt:63`) that produces
   `ClassName(field: value, ...)` — exactly the descriptor we want. But it is
   wired to `inspect`, and **nothing on the default display path calls
   `inspect`.** The REPL renders Value results by dispatching `printString` as a
   message send (`beamtalk_repl_json.erl:316`), falling back to
   `beamtalk_runtime_api:print_string/1` if dispatch fails. Actor instances are
   rendered by a separate hard-coded path that bypasses `printString` entirely.
   Either way, the structural output from `inspect` is invisible unless the user
   explicitly types `someValue inspect`. The `Value.bt` doc comment even *claims*
   `Point new // => Point(x: 0, y: 0)`, but the live REPL shows `a Point`.

3. **Three overlapping protocols with no clear contract.** Beamtalk has
   `printString`, `displayString`, and `inspect`, and developers cannot tell
   when to use which. `displayString` defaults to `printString`; `inspect`
   defaults to `printString`. In practice developers only ever reach for
   `printString` and never understand what `displayString` is for — yet
   `displayString` is silently invoked by **string interpolation** on every
   `{...}` segment (`expressions.rs:92`), so it is in fact pervasively used,
   just never *named*. And `inspect` is named like it should do something rich
   (open an inspector, as it does in Pharo) but is typed to return a plain
   `String` — the name promises tooling, the type delivers text.

### Current state

There are **multiple parallel implementations** that must be reconciled:

- **Compiled stdlib** (`stdlib/src/*.bt`): `Object.bt:92`
  `printString => "a " ++ self class printString`; `displayString` and
  `inspect` both delegate to `printString`. `Value.bt:63` overrides `inspect`
  (only) with the structural form, which recursively calls `inspect` on each
  field (`Value.bt:65`).
- **Runtime object dispatch** (`beamtalk_object_ops.erl:97-130`): handles dispatch
  for actor/object instances. `printString`/`displayString` produce
  `"a ClassName"`; `inspect` *already* produces the structural
  `ClassName(field: ...)` form for actor instances (BT-1167) via `inspect_field`
  (which itself sends `inspect`) — but again, only via `inspect`, which the REPL
  never calls.
- **Runtime primitive** (`beamtalk_primitive:print_string/1`): the fallback
  formatter for terms dispatched outside gen_server context.
- **Runtime reflection** (`beamtalk_reflection:inspect_string/1`): a third,
  separately-exported structural formatter.
- **REPL formatter** (`beamtalk_repl_json.erl`): the hard-coded `#Actor<...>` /
  `#Supervisor<...>` paths (see above).

A canonical structural renderer must be chosen and the redundant paths removed or
delegated to it — this ADR must specify which becomes canonical.

Several platform-primitive wrapper classes already diverge from `"a Foo"` by
delegating to their Erlang identity: `Pid`, `Port`, `Reference`, and `Tuple` do
`printString => self asString`, yielding `#Pid<0.123.0>`, `#Port<0.5>`, etc.

Additionally, `beamtalk_repl_json.erl` has a **third rendering path** that
bypasses `printString` entirely for actor and supervisor instances:
- Actor instances render as `#Actor<ClassName,PID>` (e.g. `#Actor<Counter,_>`)
- Supervisor instances render as `#Supervisor<ClassName,PID>` or
  `#DynamicSupervisor<ClassName,PID>`
- Future instances render as `#Future<...>`

This hard-coded REPL formatting is inconsistent with the `printString` result
(which returns `"a Counter"`) and must be reconciled.

### Constraints

- **Three-kind data model (ADR 0067, ADR 0042).** `Value` has immutable `field:`
  slots; `Actor` has `state:` behind a process/message boundary; plain `Object`
  has *no instance data and is not instantiable* — its concrete instances are
  platform-primitive wrappers (`Supervisor`, `AtomicCounter`, `FileHandle`)
  carrying an external Erlang handle.
- **Actor state must not be read for display.** Reading an actor's state to print
  it requires a synchronous call into the gen_server — racy, potentially
  deadlocking (the process may be mid-call), and a confidentiality/abstraction
  violation. Actors are opaque behind their message boundary by design.
- **Codegen rule.** Any change to generated string output must flow through the
  `Document`/typed-leaf API, never `format!`.
- **REPL output is e2e-tested.** Changing the default descriptor will churn a
  large amount of golden test output; that churn is intended but must be done
  deliberately.

## Decision

Adopt a **two-string-protocol** model (`printString` = Debug, `displayString` =
Display), drop the `a`/`an` prefix entirely, and make a derived structural
descriptor the default for `Value`.

**Scope note:** the redesign of `inspect` into a tooling/action verb is
**deferred to a separate follow-up ADR**, to be designed alongside the LiveView
live-tooling surface (now under way). `inspect` is barely used today, so leaving
it as-is for now carries no cost, and its right shape depends on what the live UI
actually needs. This ADR therefore makes `inspect` a no-op concern: it continues
to return a `String` (delegating to `printString`), and is **not** changed here.
This deliberately keeps the breaking change out of this ADR — see Alternatives
("Split the bundle").

### 1. Protocol roles

| Protocol | Role (Rust analogue) | Returns | Who calls it | Override when |
|---|---|---|---|---|
| `printString` | **Debug** | `String` | you, the **REPL**, logs, and any *other* `printString` that nests this object | almost never — it is derived |
| `displayString` | **Display** | `String` | the **language** (string interpolation `{...}`), user-facing output surfaces | the object has a natural human form (e.g. `Money → $10.50`) |
| `inspect` | *(unchanged — deferred)* | `String` (for now) | **you**, explicitly | future LiveView-driven ADR will make it a tooling/action verb |

- `printString` is the **developer/debug** representation and the **REPL default**.
  It shows structure and is self-describing.
- `displayString` is the **user-facing/Display** representation. It **defaults to
  `printString`** and is the hook the language pulls during string interpolation
  and on user-facing surfaces. Developers rarely call it directly; they *override*
  it when a value has a natural human rendering. (`String` already demonstrates
  the split: `printString` → `"hi"` quoted, `displayString` → `hi` plain.)
- `inspect` is **unchanged by this ADR** — it continues to return a `String`
  (delegating to `printString`). A follow-up ADR, designed against the LiveView
  live-tooling surface, will repurpose it as the verb that *produces or opens a
  structured inspector* (returning structured data / opening a UI pane where one
  exists, returning the receiver otherwise). That is the intended end-state; it is
  deferred here because (a) `inspect` is barely used today, (b) its right shape
  depends on what the live UI needs, and (c) it is the only *breaking* part of the
  redesign, which should not block the non-breaking `printString` win.

  One concrete consequence for this ADR: the structural renderer recurses into
  nested fields via **`printString`**, not `inspect`. Today `Value.bt:65` recurses
  via `inspect`; since `printString` now produces the same structural output, the
  renderer (and `Value`'s `inspect`, which simply delegates to `printString`) calls
  `printString` on fields. This removes the recursion's dependence on `inspect`,
  so the future `inspect` change cannot break structural printing.

### 2. Default `printString` by class kind

The `a`/`an` prefix is **removed entirely**. No vowel logic is needed because the
prefix is gone.

Three visually distinct forms, one per category of thing:

| Class kind | Default `printString` | Form | Rationale |
|---|---|---|---|
| **Value** | `ClassName(field: value, ...)` | class-headed, **labelled** fields | immutable `field:` data — show the data |
| **Actor** | `Actor(ClassName, pid)` | kind-headed, **positional** | a live process; the *kind* matters, state is behind the message boundary |
| **Supervisor** | `Supervisor(ClassName, pid)` / `DynamicSupervisor(ClassName, pid)` | kind-headed, **positional** | a supervising process; ancestry determines the kind head |
| **Object** (plain reference) | `ClassName`, or a class-defined form | bare label | no Beamtalk-visible fields and not a process; per-class override where a handle is meaningful |

Examples:

```beamtalk
Point x: 3 y: 4              // => Point(x: 3, y: 4)
Point new                   // => Point(x: 0, y: 0)
Message sender: "Alice" text: "Hi"   // => Message(sender: "Alice", text: "Hi")
Point new: #{}              // (no fields) => Point()

counter := Counter spawn    // => Actor(Counter, 0.123.0)
sup := MySup startLink      // => Supervisor(MySup, 0.200.0)
pool := MyPool startLink    // => DynamicSupervisor(MyPool, 0.201.0)
```

**Why the `Kind(Class, pid)` family.** The three categories — data, live process,
plain reference — read as three distinct shapes:
- **Value**: class-headed with **labelled** fields — `Point(x: 3, y: 4)`.
- **Process** (Actor/Supervisor): **kind-headed** with **positional** args —
  `Actor(Counter, 0.123.0)`. The kind (`Actor`/`Supervisor`/`DynamicSupervisor`) is
  foregrounded so a live process is *immediately* recognisable as one, not mistaken
  for data or a version string. This generalises the format supervisors already use
  (`#Supervisor<MySup, 0.200.0>`) into one consistent family for everything that is
  a process.
- **Plain reference**: bare `ClassName`, or whatever the class defines.

A live actor is categorically different from a reference-Object handle (a process
with a mailbox and lifecycle vs. a synchronous wrapper around an external Erlang
resource), so it earns a distinct, self-labelling shape rather than sharing a
generic `ClassName<id>` form with handles.

**Distinguishing Value from process forms.** Both are `Word(...)`, but: Value forms
carry `field:` **labels** (`Point(x: 3, y: 4)`) while process forms are
**positional** (`Actor(Counter, 0.123.0)`); and the process heads
(`Actor`/`Supervisor`/`DynamicSupervisor`) are **reserved kind words** that no
user Value class may shadow. The combination makes the two unambiguous.

**Rejected delimiters.** `Counter@0.123.0` (Erlang `node@host` style) was rejected
— a bare `Class@x.y.z` reads as a *version* (`Counter@0.1.0`), obscuring that it is
a live actor. `Counter<0.123.0>` was rejected because `<`/`>` are comparison
operators and the bare angle form doesn't signal "actor" strongly enough. The
existing `#`-prefixed `#Actor<...>` was preferred over `@` but still loses to the
`Kind(...)` family, which drops the foreign `#` Erlang sigil and uses Beamtalk's
native `()`.

**`#` on raw primitives unchanged.** The bare platform primitives keep their
Erlang-native rendering (`#Pid<0.123.0>`, `#Port<0.5>`, `#Ref<0.1.2.3>`) — these
*are* Erlang terms, so the `#…<>` form is correct for them. The consequence: a
domain actor and its raw underlying pid render differently
(`Actor(Counter, 0.123.0)` vs `#Pid<0.123.0>`), which is *useful* — they are
genuinely different things (a typed actor vs. a raw process handle).

**Replaces the existing REPL format.** This supersedes the hard-coded
`#Actor<ClassName,PID>` / `#Supervisor<ClassName,PID>` paths in
`beamtalk_repl_json.erl` (~123 test assertions). Those paths are replaced by
`printString` dispatch (with the timeout-fallback in Critical Risks), so the REPL
and `printString` produce identical output — fixing today's split where actor
`printString` returned `"a Counter"` while the REPL rendered `#Actor<Counter,_>`.

### 3. Structural format: `ClassName(field: value, ...)`

Chosen over `%ClassName{...}` (Elixir) and `#ClassName{...}` (Erlang record)
because:

- It is **already** what `Value inspect` emits — lowest churn.
- It reads as Beamtalk: `(field: value)` echoes keyword-message rhythm.
- It introduces **no new sigil**. `%` is foreign to Beamtalk; `#` is already the
  symbol sigil (`#foo`), so `#Point{...}` would visually collide with symbol
  syntax.
- It occupies an otherwise-empty syntactic slot: constructors are keyword
  messages (`Point x: 3 y: 4`), and `()` is used only for parametric types
  (`List(Symbol)`), whose contents are *types*, not `field: value` pairs. So
  `Point(x: 3, y: 4)` cannot be mistaken for runnable code or a type.

Fields are rendered in sorted order for deterministic output, each value rendered
via its own `printString` (Debug form — strings stay quoted inside structural
output). This is a deliberate change from today: `Value.bt:65` currently calls
`(self fieldAt: name) inspect` for nested values, and `beamtalk_object_ops.erl`
calls `inspect_field` which sends `inspect`. Under the new model, nested rendering
calls `printString`, and `Value`'s `inspect` simply delegates to `printString`
(same output) — decoupling structural recursion from `inspect` so the deferred
`inspect` redesign can't break it.

### 4. Nested rendering: bounded recursion

Nested values expand recursively (so `Line(from: Point(x: 0, y: 0), to: Point(x:
3, y: 4))` shows in full), bounded by:

- **depth cap** (default 5) — guards pathological nesting
- **width cap** (default ~50 elements per collection, matching Elixir's `:limit`)
- **total-length cap** — guards wide-and-shallow blowups
- **cycle guard** — prints an elision marker (`...`) / back-reference instead of
  looping

When any bound is hit, the truncated position renders as `...`. Common cases (a
`Line` of two `Point`s, a small record-of-records) expand fully; only genuinely
large or cyclic graphs are elided. Values are immutable trees post-ADR 0042, so
cycles are rare among `Value`s; the guard primarily protects against
actor/collection references.

### 5. Relationship to `asString` and the `Printable` protocol

`asString` is a separate conversion method (not a display protocol) that returns
a "natural" string form — `42 asString` → `"42"`, `#foo asString` → `"foo"`. It
is not defined on `Object` and only exists on classes where a string conversion
is meaningful. `displayString` and `asString` overlap for many types (both return
`"42"` for an integer) but diverge where `displayString` is for *human display*
and `asString` is for *lossless conversion* (e.g. a hypothetical `Color` might
have `asString → "#FF0000"` and `displayString → "red"`). This ADR does not
change `asString`.

The `Printable` protocol (ADR 0068) requires `asString` and `printString` — both
remain string-returning methods, so protocol conformance is unaffected. `inspect`
is not part of any protocol and its contract change creates no conformance issues.
`displayString` is also not part of `Printable`; whether to add it is deferred.

### 6. Reconcile the two implementations

The compiled stdlib (`Object.bt`, `Value.bt`) and the runtime fallback
(`beamtalk_object_ops.erl`, `beamtalk_primitive.erl`, `beamtalk_reflection.erl`)
must produce **byte-identical** output for the same value. The structural
renderer (format, sorting, bounds, cycle guard) is implemented once as a shared
runtime primitive and called from both paths. All Core Erlang fragments are built
via the `Document`/typed-leaf API.

## Prior Art

| Language | Debug repr | Display repr | "Inspect" action | What we take |
|---|---|---|---|---|
| **Smalltalk (Pharo/Squeak)** | `printString` / `printOn:` → `a Foo` by default; library classes override heavily | `displayString` / `displayNb` | `inspect` **opens an Inspector window** (side-effecting) | We keep `inspect` as the *action* (faithful to Pharo), but make the default *string* useful instead of `a Foo`. |
| **Elixir** | `inspect/1` / `Inspect` protocol → `%Struct{...}`, derivable, bounded (`:limit`) | `to_string/1` / `String.Chars` | `IO.inspect/2` (prints + returns value for pipelining) | Structural-by-default, bounded recursion, the Debug/Display split. We map `Inspect`→`printString`, `String.Chars`→`displayString`. |
| **Rust** | `Debug` (`{:?}`), `#[derive(Debug)]` → `Point { x: 3, y: 4 }` | `Display` (`{}`), hand-written | `dbg!()` macro | The clean two-trait split: derive Debug, hand-write Display. This is the core of our model. |
| **Erlang** | `~p` → `#rec{}`, `<0.1.0>`, `#Port<...>` | `~s` (iolist) | `observer` / `recon` | The `#…<>` form for raw handles; the `Kind(Class, pid)` shape generalises Erlang's habit of tagging opaque terms by kind. |
| **Gleam** | `string.inspect` → `Point(3, 4)` | `to_string` per-type | — | Confirms `ClassName(...)` reads naturally on the BEAM. |

**Net:** we adopt Rust's two-trait conceptual split, Elixir's structural-default +
bounded recursion, Erlang's handle notation for opaque instances, and Smalltalk's
`inspect`-as-action — rejecting only Smalltalk's `a Foo` default and the foreign
`%`/`#` sigils.

## User Impact

- **Newcomer (from Python/JS/Ruby):** A fresh object prints its data
  (`Point(x: 3, y: 4)`), matching the `__repr__`/`toString` expectation. No more
  baffling `a Point`. `displayString` is something they *override* for pretty
  output, not something they must understand up front.
- **Smalltalk developer:** Loses the familiar `a Foo`. `printString`/`displayString`
  still exist with the Debug/Display intent they always implied. (`inspect` will
  later become a faithful Pharo-style Inspector action — deferred to the LiveView
  follow-up ADR.) The departure is justified: `a Foo` is the single most-criticised
  default in the family.
- **Erlang/BEAM developer:** `Actor(Counter, 0.123.0)` / `Supervisor(MySup, …)`
  clearly tag live processes by kind, while raw primitives keep their exact Erlang
  form (`#Pid<0.123.0>`). Structural `ClassName(...)` is unsurprising next to `~p`.
- **Operator:** Actor instances never trigger a state-reading call for display —
  no deadlock/race risk from logging or inspecting a live actor. Bounded
  recursion guarantees log lines can't blow up on a deep/cyclic graph.
- **Tooling developer:** unifying the actor/value display on `printString` gives
  one predictable text representation across REPL, logs, and LSP hovers. (The
  richer structured-inspector hook comes when `inspect` is repurposed in the
  LiveView follow-up ADR — ADR 0085 live image.)

## Steelman Analysis

### Keep `a Foo` (status quo default)
- 🎩 **Smalltalk purist:** "It's honest — the base object genuinely knows only its
  class. Showing fields presumes a data shape that identity objects don't have."
- 🏭 **Operator:** "It can never be expensive or unsafe — no field reads, no
  recursion, no cycle risk. Zero surprises in a log."
- *Why rejected:* the safety argument only applies to Actors/Objects, which we
  *do* keep opaque. For `Value` (pure data) `a Foo` discards exactly the
  information the developer wants, and the structural renderer is bounded so the
  cost argument is bounded too.

### `%ClassName{...}` (Elixir sigil)
- ⚙️ **BEAM veteran:** "Instantly signals 'BEAM struct' and is unmistakably a
  printed repr, not runnable code."
- 🎨 **Language designer:** "The `%` makes the Debug/source distinction explicit,
  like Elixir deliberately does."
- *Why rejected:* introduces a sigil with no other role in Beamtalk; the
  "obviously not code" benefit is already provided by the fact that `()` +
  `field:` pairs aren't valid constructor or type syntax.

### `#ClassName{...}` (Erlang record)
- ⚙️ **BEAM veteran:** "Literally Erlang record syntax — maximal BEAM familiarity."
- *Why rejected:* `#` is Beamtalk's symbol sigil, so `#Point` parses (to the eye)
  as the symbol `Point`; the format would fight the language's own grammar.

### Collapse to two string protocols (drop `inspect` entirely / alias it)
- 🧑‍💻 **Newcomer:** "Two concepts (Debug + Display) is less to learn than three."
- *Why partially adopted:* this ADR *does* land on two **string** protocols
  (`printString`, `displayString`). `inspect` is neither deleted nor (yet)
  repurposed — it stays a string alias of `printString` for now, and is reserved
  to become the tooling-action verb in the LiveView follow-up ADR. Deleting it
  would waste a well-known name and leave no verb for "open a rich inspector."

### Tension points
- **REPL shows Debug vs Display:** Pharo shows `printString`; IEx shows the
  Debug-equivalent (`inspect`). We choose Debug (`printString`) in the REPL — a
  developer poking at structure wants the structural form. A reasonable person
  could prefer Display; we judge Debug correct for a developer REPL.
- **Plain `Object` structural vs opaque:** moot under ADR 0067 — plain `Object`
  has no instance fields, so structural output is vacuous; opaque label falls out
  of the data model.

## Alternatives Considered

See Steelman Analysis for `a Foo`, `%Struct{}`, `#Record{}`, and delete-`inspect`.
Also rejected: top-level-only (depth-1) nesting (rejected in favour of bounded
recursion because depth-1 loses detail exactly where nested data is most
interesting). Three further alternatives deserve explicit treatment:

### Minimal fix: just repoint `printString` to the existing structural logic
The narrowest change that solves the *stated* problem: override `Value`
`printString` to call the structural logic that already exists in `Value inspect`,
and override `Actor` `printString` to produce `Actor(ClassName, pid)`. Two method
overrides plus a handful of test updates. No new shared renderer, no `displayString`
re-documentation, no `inspect` contract change, no `Printable` impact.

- **Pro:** ~80% of the developer value (REPL shows structure) at ~5% of the cost
  and zero breaking changes. Value fields are flat immutable data, so the
  depth/cycle infrastructure is arguably unnecessary for the initial win.
- **Con:** leaves the three-protocol confusion unresolved (the user's *explicit*
  second goal), leaves `inspect` as a misleadingly-named string method, and
  leaves the `#Actor<...>` REPL path inconsistent with `printString`.
- **Disposition: adopted.** This ADR *is* the non-breaking core (structural
  `printString` for `Value`, `Actor(Class, pid)` for actors, drop `a`/`an`, unify the
  REPL paths). The `inspect` contract change is **split out to a separate
  follow-up ADR** designed against the LiveView surface — `inspect` is barely used
  today and its right shape depends on the live UI, so deferring it costs nothing
  and removes the only breaking change from this one.

### Separate `Inspectable` protocol (Elixir's actual model)
Rather than repurpose `inspect`, introduce a distinct `Inspectable` protocol (à la
Elixir's `Inspect`), leaving `Printable` and the existing `inspect` method
untouched. Tooling dispatches `Inspectable`.

- **Pro:** sidesteps the breaking `inspect` return-type change entirely; cleaner
  protocol separation.
- **Con:** adds a protocol concept; leaves `inspect` (the method) still meaning
  "return a string", perpetuating the name/behaviour mismatch the user dislikes.
- **Disposition:** rejected because the user explicitly wants `inspect` *itself* to
  become the tooling verb; but worth revisiting if the `inspect` migration is
  deferred.

### Actor format: alternatives to `Actor(Class, pid)`
Three other shapes for the live-process form were weighed:

- **`Counter<0.123.0>`** (bare class-headed, angle brackets). *Rejected:* `<`/`>`
  are comparison operators, and the bare angle form doesn't signal "this is an
  actor" — it reads like a generic/parametric instance.
- **`Counter@0.123.0`** (Erlang `node@host` style). *Rejected:* `Class@x.y.z` reads
  as a **version string** (`Counter@0.1.0`), actively obscuring that it is a live
  process. This was the deciding objection.
- **`#Actor<Counter, 0.123.0>`** (keep today's `#`-prefixed format). *Rejected, but
  preferred over the two above:* it at least carries the `Actor` kind label. Loses
  to `Actor(Counter, 0.123.0)` only because the latter drops the foreign `#` Erlang
  sigil and uses Beamtalk's native `()`.

**Decision:** `Actor(Class, pid)` — kind-headed, positional, native brackets. The
kind label is what makes a live process recognisable; the `()` keeps it in
Beamtalk's idiom; raw primitives keep their `#Pid<…>` Erlang form, so an actor and
its raw pid read distinctly (intended).

## Consequences

### Positive
- Fresh `Value` objects are self-describing at the REPL with zero developer work.
- The `a Foo` wart — and its ungrammatical `a Integer` form — is gone.
- Clear, documented contract: `printString` = Debug, `displayString` = Display
  (the interpolation hook). (`inspect` = open tooling, once the follow-up lands.)
- Actors and supervisors print safely (no state reads) and legibly as a consistent
  `Kind(Class, pid)` family, recognisable at a glance as live processes.
- Leaves `inspect` cleanly positioned to become the editor/LSP inspector hook in
  the LiveView follow-up ADR, with structural recursion already decoupled from it.

### Negative
- **Large golden-output churn.** Measured scope: 9 BUnit assertions use `"a Foo"`
  patterns, ~123 REPL protocol test assertions use `#Actor<...>` /
  `#Supervisor<...>` / `#Future<...>` format, and 14 stdlib files define
  `inspect -> String`. This is mechanical but extensive and touches e2e-tested
  REPL output (CLAUDE.md flags this as confirm-before-change — this ADR is that
  confirmation).
- The `inspect` redesign (the only *breaking* change — `String` →
  inspector/receiver) is **deferred to a follow-up ADR**, so it is not a
  consequence of this ADR. The 14 stdlib `inspect -> String` definitions stay as-is
  for now. (When that ADR lands, the circular-migration and gradual-typing-crash
  risks below apply — they are pre-documented here so the follow-up inherits them.)
- Two implementations (compiled + runtime) must be kept in exact lockstep;
  divergence would be a subtle correctness bug.

### Neutral
- `displayString` still defaults to `printString`, so existing overrides keep
  working; the change is documentation/clarity plus the new default *target*.
- Bounded-recursion limits are tunable constants; defaults chosen to match Elixir.

## Implementation

High-level, roughly phased. Phases 1-2 must be completed together (the
structural renderer is useless without callers). Phase 4 (`inspect`) can be
deferred independently.

1. **Shared structural renderer** (runtime): one Erlang primitive producing
   `ClassName(field: value, ...)` with sorted fields, per-field `printString`
   dispatch, and the depth/width/length/cycle bounds. Used by both compiled
   `Value` stdlib and the runtime `beamtalk_object_ops` fallback. Primitive-backed
   collections (`Array`, `List`, `Set`, `Dictionary`, `Bag`, `String`) already
   override `printString` and will not use this renderer.
2. **`printString` defaults + REPL unification:**
   - Repoint `Value` `printString` to the structural renderer.
   - Repoint `Object.bt:92` from `"a " ++ ...` to `self class name asString` (bare
     class name, no article).
   - Update `beamtalk_object_ops.erl` `printString`/`displayString` clauses to
     produce `Actor(ClassName, pid)` for actors,
     `Supervisor(ClassName, pid)` / `DynamicSupervisor(ClassName, pid)` for
     supervisors, and bare `ClassName` for plain objects.
   - **Remove the hard-coded REPL formatting paths** in
     `beamtalk_repl_json.erl` (`#Actor<...>`, `#Supervisor<...>`) and replace
     them with `printString` dispatch, so actor formatting is unified.
   - Keep `Pid`/`Port`/`Reference`/`Tuple` delegating to `asString` (already
     consistent — `#Pid<0.123.0>` etc.).
3. **`displayString`:** keep default-to-`printString`; document the
   interpolation-hook role in `Object.bt` and the language docs.
4. **`inspect`:** *out of scope for this ADR.* Left returning `String` (delegating
   to `printString`). Its redesign into a tooling/action verb is a follow-up ADR
   designed against the LiveView surface. The only change here is ensuring `Value`'s
   `inspect` delegates to `printString` (same output) so structural recursion no
   longer depends on `inspect`.
5. **Test migration:** update ~9 BUnit assertions expecting `"a Foo"`, ~123 REPL
   protocol test assertions expecting `#Actor<...>`, and any Rust codegen golden
   tests.
6. **Docs:** update `Printable.bt`, `Value.bt`/`Object.bt` doc comments (the
   `Value.bt` comment already claims the structural output — it becomes true),
   `docs/beamtalk-language-features.md`, and `docs/development/surface-parity.md`
   (display output is a cross-surface behaviour).

Affected components: stdlib (`.bt`), runtime (`beamtalk_object_ops`,
`beamtalk_primitive`, `beamtalk_reflection`, `beamtalk_runtime_api`,
`beamtalk_repl_json`), codegen (string-interpolation `displayString` dispatch is
unchanged but verified), and the broad test corpus.

### Critical implementation risks

These were surfaced in review and must be handled or the change introduces latent
runtime crashes:

1. **Circular migration of `inspect`** *(applies to the deferred follow-up ADR).*
   `Value.bt:65` calls `(self fieldAt: name) inspect` recursively, and
   `beamtalk_object_ops` `inspect_field` sends `inspect`. **This ADR pre-empts the
   problem** by switching the structural renderer's field recursion to `printString`
   now (while `inspect` still returns a string), so the later `inspect` contract
   change cannot break structural printing.
2. **Latent gradual-typing crash** *(applies to the deferred follow-up ADR).*
   `Number.bt:94` (and 13 others) define `inspect -> String => self printString`.
   Removing these later makes `42 inspect` return the receiver/inspector, not
   `"42"`; because typing is gradual, any remaining `... ++ x inspect` compiles but
   crashes at runtime. The follow-up ADR must find every
   `inspect`-result-used-as-string site before removing the overrides.
3. **Actor `printString` dispatch safety (resolved: timeout-with-fallback).**
   Replacing the hard-coded `#Actor<...>` REPL path with `printString` dispatch
   means contacting the actor process, which can block or fail if the actor is
   busy/dead/mid-call. **Decision:** the display path attempts `printString`
   dispatch with a short timeout, and falls back to a tuple-derived
   `Actor(ClassName, pid)` (read directly from the `#beamtalk_object{}` tuple, no
   message round-trip) on timeout/error/dead-process. Consequences:
   - **Default actors** (no `printString` override): the dispatch result *is*
     `Actor(ClassName, pid)`, byte-identical to the fallback — so the fast path and
     the dispatch path show the same thing. In practice the implementation can skip
     dispatch entirely for un-overridden actors.
   - **Custom-`printString` actors:** the override is honoured when the actor is
     responsive (the normal case); only an unresponsive actor degrades to
     `Actor(ClassName, pid)` — and reading its custom state was unsafe in exactly
     that case anyway.
   - The REPL therefore never hangs on a wedged actor, and never silently shows
     stale/empty state.
4. **Canonical renderer.** Exactly one of `beamtalk_primitive:print_string`,
   `beamtalk_object_ops:dispatch(inspect,...)`, and
   `beamtalk_reflection:inspect_string/1` must become canonical; the others must
   delegate to it or be deleted. The ADR mandates byte-identical output — three
   independent formatters cannot guarantee that.
5. **Stale `runtimeCalledSelectors`** *(applies to the deferred follow-up ADR).*
   `SystemNavigation.bt:2006` lists `#inspect` as a runtime-invoked selector (so
   `unusedSelectors` doesn't flag `inspect` overrides). This stays correct for now
   (the inspector surface still invokes the method). When `inspect` is repurposed,
   this entry must be re-evaluated.
6. **`inspect` name collision** *(applies to the deferred follow-up ADR).* The
   REPL already has an `"op": "inspect"` protocol operation
   (`beamtalk_repl_ops_actors`) that calls `sys:get_state` — unrelated to the
   Beamtalk `inspect` method. When the follow-up repurposes the method as "open
   tooling", the relationship between the two must be documented (and ideally
   unified) for contributors. `SystemNavigation.bt:2006` `runtimeCalledSelectors`
   lists `#inspect` and must be re-evaluated at that point.

## Migration Path

- **`a Foo` in user code/tests:** no source migration needed for `.bt` programs;
  only output expectations change. Test assertions are updated as part of this
  work.
- **`inspect` callers:** no migration in this ADR — `inspect` still returns a
  string. (The follow-up ADR that repurposes `inspect` will own this migration,
  including the gradual-typing crash risk: a leftover `x inspect` in a `++` compiles
  but crashes at runtime, so that ADR must grep all call sites and the 14
  `inspect -> String` overrides before changing the contract.)
- **Custom `printString`/`displayString` overrides:** continue to work unchanged.
  Authors who previously overrode `printString` only to escape `a Foo` may now
  delete the override and accept the derived default.

## References
- Related issues: BT-2458 (Epic); BT-2459, BT-2460, BT-2461, BT-2462, BT-2463, BT-2464
- **Follow-up ADR (to be written):** `inspect` as a tooling/action verb, designed
  against the LiveView live-tooling surface. Owns the breaking `inspect`
  return-type change, the 14 `inspect -> String` migrations, the gradual-typing
  crash risk, the `runtimeCalledSelectors` update, and the `"op": "inspect"` /
  `inspect`-method naming unification.
- Related ADRs: ADR 0042 (immutable values / actor-only mutable state),
  ADR 0067 (state/field keywords by class kind), ADR 0068 (structural protocol
  conformance — `Printable`), ADR 0023 (string interpolation), ADR 0085 (editor
  live image representation), ADR 0036 (metaclass tower)
- Code: `stdlib/src/Object.bt:92`, `stdlib/src/Value.bt:63`,
  `stdlib/src/Printable.bt`, `stdlib/src/String.bt:563`,
  `runtime/apps/beamtalk_runtime/src/beamtalk_object_ops.erl:97`,
  `runtime/apps/beamtalk_workspace/src/beamtalk_repl_json.erl:316`,
  `crates/beamtalk-core/src/codegen/core_erlang/expressions.rs:92`
