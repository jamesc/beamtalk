# ADR 0118: ChildSupervisor — Config-Preserving Runtime-Added Children

## Status
Implemented (2026-08-31)

## Context

ADR 0059 gave Beamtalk two supervision primitives: `Supervisor` (static
children, known at class-definition time) and `DynamicSupervisor` (runtime-
added children, backed by OTP `simple_one_for_one`).

While wiring a `DynamicSupervisor(Monitor)` into a real application
(`beamtalk-watcher`), it became clear that `DynamicSupervisor` cannot
provide OTP-native "restart preserves this specific child's original
config" behavior. `simple_one_for_one` is a single shared, anonymous child
*template*: every `startChild:` call is started via the same template MFA,
and a crash-triggered restart always replays the template's own args, never
the specific args a particular `startChild:` call happened to pass. There is
no way to fix this from inside `DynamicSupervisor` — it is inherent to the
strategy, not a bug.

Standard OTP has a primitive for exactly this: a plain (non-
`simple_one_for_one`) supervisor — `one_for_one` here — where each child is
added at runtime via `supervisor:start_child(Sup, FullChildSpec)`, with its
*own* unique `{Id, MFA, Args, ...}` baked into that child's permanent spec.
OTP remembers each child's own spec forever and replays those exact
original args on restart. This is the more traditional OTP dynamic-child
pattern, predating `simple_one_for_one`.

Before this ADR, the only workaround was for the *owning actor* to track
each child's config itself, watch each child via `Actor>>onExit:`, and
manually respawn + reconfigure on crash — reimplementing in user code
something OTP already does natively.

## Decision

Add `ChildSupervisor`, a third abstract supervision base class alongside
`Supervisor` and `DynamicSupervisor`, backed by a plain `one_for_one`
supervisor rather than `simple_one_for_one`.

```beamtalk
ChildSupervisor(Monitor) subclass: MonitorSupervisor
  class childClass => Monitor

sup := (MonitorSupervisor supervise) unwrap
m := (sup startChild: #{#check => aCheck, #interval => 30}) unwrap
// crash → OTP restarts `m`'s replacement with the *same* args
// automatically, no onExit:/manual-respawn needed.
```

Each `startChild:`/`startChild` call registers a distinct, permanent OTP
child spec (its own id, its own args) via `supervisor:start_child/2`, so
`supervisionPolicy` (`#permanent`/`#transient`/`#temporary`) on the child
class works exactly as documented — including config-preserving automatic
restart.

### Resolved design questions

The originating issue (BT-3366) flagged four open questions as `needs-spec`.
They were resolved as follows:

**Naming — a new class, not a strategy parameter.** `ChildSupervisor` is a
standalone abstract base class, parallel to `Supervisor` and
`DynamicSupervisor`, rather than a mode/strategy flag on a shared base. This
follows ADR 0059's own precedent (`Supervisor` and `DynamicSupervisor` are
already two flat, non-inheriting classes rather than one parameterized
class) and keeps the static/dynamic-template/dynamic-permanent distinction
visible at the point of `subclass:`.

**Child identity — internally generated, not caller-supplied.** The OTP
child spec `id` is generated per `startChild:` call
(`erlang:unique_integer/1`), not exposed to Beamtalk code. Callers get back
an opaque child handle (the same `Actor(ClassName, Pid)` wrapper
`DynamicSupervisor` already returns) and use it with `terminateChild:`.
Using an integer rather than `list_to_atom/1`-ing a caller-supplied name
avoids leaking the atom table across long-running `startChild:`/
`terminateChild:` cycles — atoms are never garbage-collected on BEAM.

**`terminateChild:` deletes the spec.** Unlike `DynamicSupervisor`'s
`simple_one_for_one` (where `supervisor:delete_child/2` on the shared
template crashes the supervisor — there is no per-child spec to delete),
`ChildSupervisor`'s children each own a real, permanent OTP child spec that
OTP keeps forever unless explicitly deleted. `terminateChild:` therefore
always follows a successful `supervisor:terminate_child/2` with
`supervisor:delete_child/2`, so repeated add/remove cycles never leak spec
entries.

**Strategy and restart-limit semantics — same as `DynamicSupervisor`,
`one_for_one` only.** `maxRestarts`/`restartWindow` behave identically to
today's `DynamicSupervisor` (an OTP `intensity`/`period` pair on the
supervisor itself). The OTP strategy is fixed at `one_for_one` — a
`ChildSupervisor`'s children are independent by construction (each carries
its own config), so `one_for_one`'s "only the crashed child restarts"
semantics is the only sensible default. A configurable
`one_for_all`/`rest_for_one` strategy was not added: those strategies are
about *sibling* relationships between children, which don't yet have an
established meaning for a set of heterogeneous, dynamically-named children.
This can be revisited if a concrete use case emerges.

### What `ChildSupervisor` deliberately omits

Matching `DynamicSupervisor`'s existing shape (not `Supervisor`'s):
`ChildSupervisor` has no `children`/`which:`-style listing method — only
`count`. A caller that needs to reach a specific running child again after
holding onto its handle (e.g. across a crash) uses named registration
(`Actor>>registerAs:`/`Actor class>>named:`, ADR 0079), the same mechanism
`DynamicSupervisor` children already use for this.

## Prior Art

See ADR 0059 §Prior Art for the full survey (Elixir, Erlang OTP, Pharo,
Gleam, Newspeak, Akka) — the reasoning there (declarative class-body
overrides, actor-owned `supervisionPolicy` defaults, no per-message-type
strategy) applies unchanged to `ChildSupervisor`.

The specific primitive `ChildSupervisor` exposes is the traditional
`supervisor:start_child/2` full-childspec pattern from Erlang/OTP — the same
mechanism `Supervisor`'s static children already use internally
(`build_child_specs/1`), just invoked at runtime instead of at `init/1`
time, and with dynamically generated rather than statically declared ids.

## User Impact

**Newcomer:** the choice between three supervision base classes needs a
clear decision rule. Documentation states it as: children known at startup
→ `Supervisor`; a homogeneous pool of interchangeable workers →
`DynamicSupervisor`; runtime-added children that each need their own,
differing config to survive a crash → `ChildSupervisor`.

**BEAM veteran:** `ChildSupervisor` is recognizable immediately as "OTP
`one_for_one` with `start_child/2`", the traditional dynamic-child pattern —
no new OTP concepts, just a Beamtalk-level name for an existing, familiar
Erlang pattern.

**Operator:** the generated supervisor is standard OTP `one_for_one`; all
existing tooling (`observer:start()`, `supervisor:which_children/1`,
`recon`) works unchanged. Child ids are opaque integers rather than
meaningful atoms — a minor observability trade-off, made for atom-table
safety (see above).

## Alternatives Considered

### Caller-supplied child ids

Let `startChild:` accept an explicit `id: aSymbol` alongside the config
dict, mirroring `Supervisor`'s `SupervisionSpec withId:`.

**Rejected because:** the class of problems this issue targets (many
runtime-added, differently-configured children, added and removed over a
long-running process's lifetime) makes caller-supplied atom ids an atom-leak
foot-gun by construction — every `startChild:` call is atom-table pressure
unless callers thread their own reuse discipline through the id space
themselves. An internally generated integer id sidesteps the problem
entirely, and named registration already covers "I need to find this child
again by a stable name" without touching the OTP-level id.

### Leave the spec registered after `terminateChild:`

Skip `supervisor:delete_child/2` and rely on `supervisionPolicy: #temporary`
to prevent restart of a terminated child, matching `DynamicSupervisor`'s
"never call `delete_child`" rule.

**Rejected because:** that rule exists for `DynamicSupervisor` specifically
because its child spec is a *shared template*, not a per-child entry —
`delete_child` there would remove the only spec and break every future
`startChild`. `ChildSupervisor` has no such shared template: each child's
spec is genuinely per-instance and genuinely unused once terminated.
Leaving it registered would accumulate dead spec entries in the supervisor
process for the lifetime of the node — an unbounded, unnecessary leak this
ADR's acceptance criteria explicitly call out avoiding.

## Consequences

### Positive
- OTP-native config-preserving restart for heterogeneous runtime-added
  children, without any `onExit:`/manual-respawn user code.
- Reuses `SupervisionSpec`/`childSpec`/`spec_to_otp` end to end for start-fn
  selection and restart/shutdown defaulting — no duplicated child-spec
  construction logic between `Supervisor` and `ChildSupervisor`.
- Same `count`/`terminateChild:`/`stop` shape as `DynamicSupervisor` — no
  new concepts for a caller already familiar with that class.

### Negative
- A third supervision base class widens the "which one do I use?" decision
  surface for newcomers. Mitigated by the decision rule in the doc
  contrast table (`docs/beamtalk-language-features.md`).
- Opaque integer child ids are less legible than `Supervisor`'s class-name
  atom ids when inspecting a live tree with `observer`/`recon`.

### Neutral
- `ChildSupervisor` and `DynamicSupervisor` are structurally similar at the
  Beamtalk call-site level (`startChild`/`startChild:`/`terminateChild:`);
  the difference is entirely in restart semantics under crash, which is
  the whole point of the new class existing.

## Implementation

- **AST/semantic analysis:** `SupervisorKind::Child` variant
  (`crates/beamtalk-core/src/ast/class.rs`); `is_child_supervisor_subclass`
  hierarchy query; third arm in the `supervisor_kind` writeback pass.
- **Codegen:** `generate_child_supervisor` in
  `crates/beamtalk-core/src/codegen/core_erlang/supervisor_codegen.rs`,
  structurally identical to `generate_dynamic_supervisor` (reuses
  `generate_sup_start_link`/`generate_dynamic_child_class` as-is) except for
  `init/1`, which delegates to a new `beamtalk_supervisor:child_init/2`.
- **Runtime** (`runtime/apps/beamtalk_runtime/src/beamtalk_supervisor.erl`):
  `child_init/2` (plain `one_for_one`, empty initial child list);
  `childStartChild/1,2` (builds a fresh childspec via
  `SupervisionSpec`/`childSpec`/`spec_to_otp`, overriding only `id` with an
  `erlang:unique_integer/1`); `childTerminateChild/2` (looks up the live
  child spec id via `supervisor:which_children/1` pid-matching, then
  `terminate_child/2` + `delete_child/2`). `is_supervisor/1` and
  `process_label/1` (display formatting) extended to recognize
  `ChildSupervisor` ancestry.
- **Stdlib:** `stdlib/src/ChildSupervisor.bt`, mirroring
  `DynamicSupervisor.bt`'s shape.
- **Tests:** `stdlib/test/child_supervisor_defaults_test.bt` (BUnit —
  defaults, subclass overrides, startChild/terminateChild lifecycle);
  `tests/repl-protocol/cases/child_supervisor.btscript` (e2e — the
  config-preserving-restart regression: kill a child started with
  `#{#value => 99}`, poll for restart, assert the replacement's value is
  still 99).

## References
- Related issues: BT-3366 (this ADR), BT-3365 (narrower `DynamicSupervisor
  startChild:` arity bug found while investigating this)
- Related ADRs: ADR 0059 (Supervision Tree Syntax), ADR 0080 (Supervisor
  Lifecycle Result), ADR 0092 (Supervision Tree Introspection), ADR 0079
  (Named Actor Registration — used for post-restart child lookup)
- Documentation: `docs/beamtalk-language-features.md` (contrast table),
  `stdlib/src/ChildSupervisor.bt`
