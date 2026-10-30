# ADR 0142: The Class Process Only Owns Class State — Stateless Class Methods Run in the Caller

## Status
Implemented (2026-10-30); accepted 2026-10-29. Epic BT-4297.

> **Implemented note (2026-10-30).** What shipped differs from the text below in these ways:
>
> - **Rule 2 is chain-wide (deviation from §1).** A class keeps its class process when it *or any class in its superclass chain* declares class variables, not only when the resolved method's defining class does. A subclass that declares none of its own holds its ancestors' class variables in its own process, and a method it defines itself reaches them through a class-side `self` send, which would raise `class_state_unreachable` in the caller. The runtime rule is `beamtalk_class_metadata:chain_has_class_variables/1`, the compile-time rule `ClassHierarchy::runs_class_method_in_caller` (Rust), and the derived `direct_call_ok/2` flag folds the class-variable facts over the whole chain (BT-4300, BT-4301).
> - **The guarded static call** also checks `erlang:function_exported/3`, so a method removed by `removeSelector:` or a reload takes the `class_send` fallback instead of raising `undef`, and gets the class pid by `erlang:whereis/1` of the compile-time registered name. The direct call stays in tail position. Class-side `self foo` and explicit own-class sends go through `class_self_direct_ok/4`, sealed or not; no unguarded class-side direct call remains. The `class_var_abi` is 3 (the class-method calling convention changed); modules compiled for 2 are refused at load.
> - **Flag erasure** is as amended in §2 below: a class's own derived inputs are raised before its change is visible, its descendants' flags are erased just after.
> - **Benchmarks** (`benchmarks/vs-elixir/results/bench-bt4300.md`, `bench-bt4301.md`). Rows 3b and 3c went from about 200 reductions through the class process to about 24 with the in-caller dynamic path (BT-4300), then to 7.03 with the guarded static call (BT-4301), meeting the ≤ 10 gate. **Row 3a misses its ≤ 3 target**: it went from 2 to 7.03, the cost of the guard, the `function_exported/3` check and the real class reference that the unguarded `nil` call skipped. The miss was accepted for merge; BT-4316 is the follow-up to bring it back.
> - **`performLocally:withArguments:`** now resolves through the class-method chain from the receiving class (an inherited selector is found, a subclass override wins), but applies compiled methods only: a selector whose nearest definition is an ADR 0084 runtime fun raises `does_not_understand`.
> - **`class-process-ffi`** now also fires in classes that declare `classState:` (they used to be exempt), with a hint to move the call into an instance method or an Actor.
> - **Open questions**: an ancestor's class-side extension is not reached by the in-caller owner memo, as it is not on the class-process path (BT-4307); and an explicit own-class send `Base foo` inside an inherited class method runs with the subclass as `self` (BT-4317).

## Context

### Problem

Every class has a gen_server, the class process. Today a class method runs
inside that process unless three conditions all hold:
- the class is `sealed`;
- it has no class variables;
- the method is declared `class sealed`.

These are ADR 0129 Phase 0b's rules.

Everything else, open classes and non-`class sealed` methods alike, is a
`gen_server:call`. The `benchmarks/vs-elixir` suite measures it:

| Row | Send | Beamtalk | Elixir |
|---|---|---|---|
| 3a | `class sealed` method of a sealed class | 2 red / 0 w, 0.9 ns | 2 red, 0.8 ns |
| 3b | class method, sealed class, method not `class sealed` | 145 red / 444 w, 2.3 µs | 2.9 ns |
| 3c | class method on an open class | 146 red / 480 w, 2.6 µs | 2.8 ns |

The cost is not only speed. The class process is a **node-wide
serialisation point**: every caller of every class method of that class
queues on one process. It also makes class methods behave unlike the
functions they look like:

- **Caller-sensitive FFI breaks.**
  - `gen_tcp:accept` blocks every other caller of the class.
  - `controlling_process`, `self()`, `link`, `monitor` and the process
    dictionary act on the class process, not the caller. Socket code gets
    `not_owner`.
  - The `class-process-ffi` lint exists only to warn about this.
- **Re-entrancy fails.** A block passed into a class method runs in the class
  process (*Passing Blocks Through Class Methods*, BT-3022). If it sends
  back to the same class, the result is `dispatch_error`. ADR 0109 had to
  special-case `File open:do:` to escape this.
- **Where code runs depends on a subclassing decision.** Sealing a class for
  API reasons moves its `class sealed` methods out of the process; unsealing
  it moves them back.

What the class process actually protects is **class variables**. ADR 0130
made the class process their home for the duration of an invocation. A class
with no class variables has nothing for the process to own.

For Catalina, class-side sends are about 1% of per-message CPU (the
`echo_c10_64B` profile recorded in epic BT-4291), so this ADR is mostly about
correctness and contention, not throughput.

### Current state

Two mechanisms put class methods in the caller today:

1. **ADR 0129 Phase 0b direct calls, plus BT-4099 constructors.**
   - Rust computes eligibility (`ClassInfo::is_direct_call_eligible`:
     sealed + no class variables + `class sealed` + not a spawn constructor)
     and bakes it into `__beamtalk_meta/0` as `direct_class_methods`.
   - Static call sites call `Mod:class_sel(nil, …)` unguarded. Dynamic class
     sends consult the baked table (`lookup_direct_call/3`).
   - Trusting a compile-time verdict without a guard is the source of four
     known caveats:
     - BT-4126: stale descendants after a hot patch;
     - BT-4133: class-side extensions are bypassed;
     - BT-4134: a reparent leaves a stale verdict;
     - BT-4037: callers pass `nil` as `ClassSelf`.
2. **Open class-side self-sends (BT-3700).**
   - `class_self_direct_ok/4` reads one `persistent_term` flag per send,
     maintained by `beamtalk_class_shadow_flags`.
   - The flag is erased under a per-tag lock before an extension or ADR 0084
     runtime fun becomes visible.
   - This is a guarded direct call (ADR 0025 amendment, 2026-10-29), but it is used only for
     self-sends inside an open class.

### Constraints

- **The guard is the contract** (ADR 0025 amendment, 2026-10-29). A direct call needs a run-time
  guard proving it reaches what dynamic dispatch would reach, with a
  byte-identical fallback.
- **ADR 0130:**
  - Class variables live in the class process during an invocation.
  - They are not inherited: a method may touch only its own class's variables.
  - §1 forbids nesting one class-variable home inside another.
- **Escape hatches are guaranteed** (ADR 0100):
  - ADR 0084 runtime class-method funs shadow compiled methods;
  - class-side extensions (ADR 0066);
  - `removeSelector:` (ADR 0112);
  - hot reload.
- **No image mechanics** (principles §4). Behaviour must not depend on
  whether code runs in the REPL, `beamtalk run`, `beamtalk test` or a
  release.
- **One rule implementation.** The process-bound selector list and the
  eligibility rule are computed once, in Rust, and reach the runtime as
  generated metadata (ADR 0129).

## Decision

### 1. A class method runs in the caller unless the class needs its process

A class method runs **in the caller** when all of these hold:

1. **The receiving class has no class variables, and is not shadowed.** This
   is checked at run time by the per-class flag `direct_call_ok(C)`. It is
   the existing `beamtalk_class_shadow_flags` derived flag, which gains one
   input. It is true exactly when:
   - `C` has no class variables (`classState:`), which is the new input;
   - no ADR 0084 runtime class-method fun is installed on `C`;
   - no class-side extension shadows a compiled method of `C`;
   - the class has finished bootstrap (`mark_ready`).
2. **The class defining the resolved method also has no class variables.**
   *(As implemented, this is taken over the receiving class's whole superclass
   chain; see the Implemented note under Status.)*
   - A class method touches only its own class's variables, through the
     receiver's class-process home (ADR 0130).
   - Take a method defined on a stateful `Base` and sent to a stateless `Sub`.
     Today it runs in `Sub`'s process and raises `undeclared_class_variable`;
     run in the caller, it would raise `class_state_unreachable` instead. So
     it stays in the class process, and its error does not change.
   - Methods that run in the caller install no class-variable home, so ADR
     0130 §1's no-nesting rule is untouched.
3. **The selector is not process-bound.** These keep their current routing:
   - `new`, `new:`, `startLink` and `startLink:` (ADR 0129). Value constructors
     keep their BT-4099 direct path.
   - `spawn` and `spawnWith:`, which have dedicated clauses in
     `beamtalk_class_dispatch`.
   - `Supervisor` `supervise`/`spawn*`, which are routed before
     `class_send_plain`.
   - `TestCase` run selectors, which answer `test_spawn` and need the
     gen_server's `noreply`.

   Rust owns this list, in `class_info.rs` next to `is_direct_call_eligible`.
   It reaches the runtime as generated metadata. The `class-state-abroad` lint
   and the migration lint (§4) read the same list.

`sealed` and `class sealed` play no part. A class that needs a serialising
home declares class state (`classState:`) or delegates to an Actor, the
idiomatic BEAM owner. No new keyword is added.

### 2. How sends reach it

**Dynamic class sends.** These are sends through a variable, `perform:`,
`classNamed:`, a class passed as an argument, or an Erlang caller of
`class_send`. They all go through `beamtalk_class_dispatch:class_send_plain/3`.
- When `direct_call_ok(C)` holds, resolve the defining class and module in
  the caller. A memo, `{class_method_owner, C, Sel}` in
  `beamtalk_class_derived_cache`, caches the result (the BT-4109
  `resolve_owner` pattern). Apply the method in the caller if rule 2 holds.
  Otherwise use the class process, as today.
- Deriving the memo walks the chain exactly as the class process does today.
  A class-side extension or ADR 0084 fun at each ancestor counts as that
  ancestor's method.
- A shadow on an *ancestor* never clears the receiver's flag, so the memo is
  what keeps it correct. Every change that can alter the answer must
  invalidate the derived cache:
  - `beamtalk_extensions:register` and `remove` already do.
  - `beamtalk_class_metadata:set_runtime_class_methods/2` does not today.
    Phase 1 adds it.

**Static class sends with a literal receiver** (`Point origin`, or class-side
`self foo`), where the selector is one that `C` itself defines, compile to:

```erlang
case call 'beamtalk_class_shadow_flags':'direct_call_ok'('Point class', 'Point') of
  <'true'> -> call 'bt@app@point':'class_origin'(PointRef)
  <_>      -> <today's class_send>
end
```

- `PointRef` is the class reference built from the compile-time registered
  name (BT-4295), passed as the real `ClassSelf`. This retires the `nil` convention and its per-call prologue
  lookup (BT-4037).
- An **inherited** class method sent to a literal receiver goes through the
  dynamic path, which itself now runs in the caller through the memo. No call
  site names a superclass module, so nothing goes stale when the hierarchy
  changes.
- **When the flag is erased.**
  - Today it is erased only when an extension is registered or removed
    (`beamtalk_extensions:register`/`remove`), or when an ADR 0084 runtime
    fun is installed (`beamtalk_class_metadata:set_runtime_class_methods/2`).
    See `beamtalk_class_shadow_flags:set/2` (`:102`) and `clear/2` (`:139`).
  - This ADR adds erasure on every other metadata write for `C`: reload,
    a change to class variables, `removeSelector:`, and `put_class_method`.

  **Amended (BT-4299, BT-4301, 2026-10-30).** For a fact the flag is
  derived from (an extension, a runtime fun, the class's own class
  variables), the raw input is raised and the class's own flag erased under
  the per-tag lock *before* the change becomes visible, so no send sees the
  old `true`. The other metadata writes (reload, `removeSelector:`,
  `put_class_method`) only erase the derived flag: that forces the next send
  to re-derive it from the raw inputs, but does not hold the fast path off
  during the write. A method those writes remove is caught by the guarded
  call's `function_exported/3` check, and the in-caller owner memo is kept
  consistent by the derived cache's generation counter, not by the erase.
- **Amended (BT-4301, 2026-10-30): descendants' flags.** Because the flag
  folds the class-variable facts of the whole chain, a change to a class
  (gaining or losing class variables, a reload that re-parents it) also
  erases every descendant's flag. That erasure happens *after* the
  ancestor's row write, under each descendant's tag lock, which is always
  taken, so it also removes a value an in-flight install derived from the
  old row. Between the write and the erase a descendant's static send can
  still see its old `true` and run in the caller: the same short window a
  reload has for its own new code. The window is accepted; no stale value
  outlives the erase. See the `beamtalk_class_shadow_flags` moduledoc and
  `beamtalk_class_metadata:put_class_variables/2`.
- This guard **replaces** the unguarded ADR 0129 static calls, which closes
  BT-4126, BT-4133 and BT-4134 for class methods.

### 3. What moves into the caller

- **Blocks.** A block passed into such a class method runs in the caller.
  - Re-entrant sends to the same class stop raising `dispatch_error`.
  - Inside an actor, the block runs at home in the ADR 0137 sense.
- **Process-local effects** belong to the caller:
  - `self()` and the process dictionary;
  - links, monitors and timeouts;
  - any resource created through FFI, such as ETS tables, ports and timers.
- **Class-process services become no-ops.** The class process adds these
  around a call; the caller already is the context they recreate:
  - seeding the caller's session context;
  - mirroring `beamtalk_dispatch_caller_pid`;
  - adopting the entry group leader;
  - the 60 s / 300 s class-call timeouts.
- **One reader needs checking.** `File open:mode:`'s handle-ownership rule
  reads the mirrored caller pid. Phase 1 must make its no-mirror path resolve
  to `self()`, with a test in the REPL, in `beamtalk run` and in
  `beamtalk test`.

### 4. Migration lint

The `class-process-ffi` lint is inverted into `class-method-process-resource`.
It warns on FFI in a class method of a stateless class that does either of
these:
- creates a process-owned resource (`ets:new`, `open_port`, `spawn_link`,
  `link`, `monitor`, `register`, timers);
- relies on `self()` or the process dictionary.

The message names the fix: an Actor, or `classState:`. The lint reuses the
existing validator and its curated table.

### Example

```beamtalk
Object subclass: Paths
  class join: a :: String with: b :: String -> String => a ++ "/" ++ b
  class each: items :: List do: aBlock -> Nil =>
    items do: [:x | aBlock value: x]
    nil
```

```beamtalk
Paths join: "a" with: "b"                       // => "a/b", computed in the caller: no class-process hop
Paths each: #(1, 2) do: [:x | Paths join: "x" with: x printString]
                                                // => nil; the block runs in the caller, so re-entering Paths is fine
                                                // (today: dispatch_error)
```

A class with class state keeps its process:

```beamtalk
Object subclass: Counter
  classState: count :: Integer = 0
  class bump -> Integer => self.count := self.count + 1
```

```beamtalk
Counter bump   // => 1, in Counter's class process, as today
```

### Error behaviour

There are no new errors. Every error a class method raises today is raised
with the same value. The one difference is *where*: an `exit` inside a
stateless class method now exits the caller, as an Erlang function call
would, instead of being relayed back from the class process.

## Prior Art

- **Pharo / Squeak / Newspeak.** Class methods are ordinary methods on the
  metaclass and run in the caller. There is no class process. *Adopted.*
- **Erlang/OTP.** Module functions run in the caller. You get serialisation
  by owning a process (a gen_server), never as a side effect of where code is
  defined. *Adopted:* the class process exists only for class state, and an
  Actor is the serialising home.
- **Elixir.** Module functions, including struct constructors and
  `defdelegate`, run in the caller; an `Agent` or `GenServer` is the explicit
  owner of shared state. *Adopted*, as for Erlang.
- **Akka / Pony.** Shared mutable state lives in an actor; everything else is
  a plain call. *Consistent* with this ADR.

## User Impact

- **Newcomer.** Class methods behave like functions in a module, which is
  what someone from Python, JS or Elixir expects. There is no hidden process
  to learn about until they declare class state.
- **Smalltalk developer.** This is how Pharo has always worked. Sends keep
  their semantics: DNU, `perform:`, extensions and live patching all behave
  as before.
- **Erlang/Elixir developer.** Caller-sensitive FFI such as
  `gen_tcp:accept`, `controlling_process` and `self()` does what it says. A
  serialising process is something they write, as on OTP.
- **Production operator.**
  - The per-class node-wide queue, and the latency spikes and timeouts behind
    it, go away for stateless classes.
  - Per-class load no longer shows as a class-process mailbox in `observer`
    or `Tracing`.
  - Resources created in stateless class methods belong to the caller; the
    migration lint flags them.
- **Tooling developer.** One rule, read from one place. The lints and codegen
  share `is_direct_call_eligible` and the process-bound selector list.

## Steelman Analysis

### A. Only sealed, stateless classes run class methods in the caller
- 🏭 **Operator:** "A smaller blast radius: only classes someone sealed move."
- ⚙️ **BEAM veteran:** "A sealed class has no subclasses, so there is no
  subclass-with-class-variables case to guard."
- 🎨 **Language designer:** "It extends ADR 0129 instead of replacing it."

*Why not:*
- Open-class class methods stay at ~145 red, behind the node-wide queue.
- `sealed` keeps deciding which process code runs in.
- Framework base classes are open by design.

### B. Status quo (ADR 0129's rule)
- 🎩 **Smalltalk purist:** "The class object is an object, and objects in
  Beamtalk are processes."
- 🏭 **Operator:** "Nothing changes, nothing to migrate."

*Why not:*
- The class process is already a source of surprises: the
  `class-process-ffi` lint, `dispatch_error` on re-entry, and ADR 0109's
  special case.
- The unguarded verdicts behind BT-4126, BT-4133, BT-4134 and BT-4037 stay.

### C. Unguarded trust in compile-time verdicts, extended to every stateless class
- ⚙️ **BEAM veteran:** "No flag read at all."

*Why not:* it is the source of the four staleness caveats. A
`persistent_term` read costs about one reduction.

### Tension points
- **Purists against pragmatists.** Purists may want class methods to stay
  "inside the class object". Pharo, Erlang and newcomer intuition all agree
  they run in the caller.
- **Operators gain and lose.** They gain no node-wide queue and no timeouts.
  They lose implicit ownership of resources created in class methods; the
  migration lint and the CHANGELOG cover that.

## Alternatives Considered

### Only sealed, stateless classes run in the caller
Considered with the project owner and rejected: process placement should
follow class state, not subclassing (steelman A).

### Status quo
Rejected (steelman B).

### Unguarded compile-time verdicts
Rejected (steelman C).

## Consequences

### Positive
- Class methods of stateless classes cost a flag read plus a remote call,
  instead of ~145 red / ~450 w and a node-wide queue (rows 3b, 3c).
  `Exception signal:` and `Result ok:` follow from the general rule.
- Caller-sensitive FFI in class methods works, so the original complaint
  behind the `class-process-ffi` lint disappears.
- Re-entrant class methods stop raising `dispatch_error`. ADR 0109's
  `open:do:` special case becomes an instance of the general rule.
- One class-side rule replaces two, and the unguarded verdicts behind
  BT-4126, BT-4133, BT-4134 and BT-4037 go away.
- Method-level `class sealed` no longer matters for dispatch. That removes
  the last blocker on ADR 0049 (removing method-level `sealed`).

### Negative
- **A semantic change for stateless class methods.**
  - Resources created through FFI (ETS tables, ports, timers, links,
    monitors) now belong to the caller and die with it.
  - Implicit serialisation through the class process is gone.
  - A hung class method blocks its caller, instead of timing out a
    `gen_server:call`.
  - An `exit` inside the method exits the caller.
- **Blocks passed from an actor into a stateless class method now run at
  home** (ADR 0137). They see and write live state, and the
  `actor-state-abroad` lint's findings change.
- **Every class-side direct call pays a guard.** It costs one
  `persistent_term` read, where ADR 0129's static calls paid nothing. Row 3a
  goes from 2 red to ~3.

### Neutral
- Classes with class variables keep their process unchanged.
- Catalina's throughput barely moves (~1% of per-message CPU). The win is
  correctness and the removal of a node-wide queue.

## Implementation

All phases sit under epic BT-4297, after BT-4292, the differential harness of
epic BT-4291.

**Phase 0 — differential rows (S).** Add these class-side rows to BT-4292's
differential fixture:
- an inherited class method touching a stateful parent's class variables
  (its error must not change);
- an extension or runtime fun on an *ancestor* of the receiving class;
- each process-bound selector;
- a re-entrant block.

Results and error shapes must be identical. Add vs-elixir rows for an
open-class class method.

**Phase 1 — runtime (M).**
- Add "has class variables" to the inputs of
  `beamtalk_class_shadow_flags:direct_call_ok/2`, erased on class-variable
  changes under the same per-tag lock.
- `class_send_plain/3` runs the method in the caller when the flag holds,
  using the owner memo, which also checks rule 2.
- `set_runtime_class_methods/2` invalidates the derived cache.
- The process-bound selector list arrives as metadata.
- Pass the real class reference as `ClassSelf`.
- `File open:mode:` resolves ownership to `self()` without the mirror.
- Files: `beamtalk_class_dispatch.erl`, `beamtalk_class_shadow_flags.erl`,
  `beamtalk_class_derived_cache.erl`, `beamtalk_class_metadata.erl`,
  `beamtalk_object_class.erl`, `beamtalk_file.erl`.
- Subsumes BT-4073 and BT-4114.
- Gate: rows 3b and 3c ≤ 10 red, with no `'$gen_call'` in the profile.

**Phase 2 — codegen and lint (M).**
- Literal-receiver and class-side self-sends to own selectors emit the
  guarded direct call, and ADR 0129 static calls get the same guard.
- Retire the `nil` `ClassSelf` convention.
- Add the `class-method-process-resource` lint (§4).
- Update `is_direct_call_eligible` (`class_info.rs`, shared with the
  `class-state-abroad` lint) to the new rule, and use it in REPL codegen too
  (`crates/beamtalk-repl/src/codegen.rs`,
  `set_direct_call_eligible_from_hierarchy`).

**Phase 3 — docs (S).**
- Amend these ADRs:
  - 0129 (constraint 1);
  - 0013 §4;
  - 0010 (stale "zero-overhead class sends");
  - 0137 (blocks in class methods);
  - 0130 (§5's "class sealed, stateless" wording; the in-caller rule adds no
    class-variable home);
  - 0109 (its `open:do:` exception is now the general rule);
  - 0084 (runtime funs apply in the caller when the class has no class
    variables).
- Update `docs/beamtalk-language-features.md`: *Class-Side Methods*,
  *Passing Blocks Through Class Methods* and *Self-Sends and Overrides*.
- Update CLAUDE.md's "Blocks into class methods" rule.
- Add a CHANGELOG migration note.

## Implementation Tracking

**Epic:** BT-4297

**Issues:**
- Phase 0: BT-4298, class-side differential rows and vs-elixir rows. Blocked by BT-4292.
- Phase 1a: BT-4299, the flag's class-variable input and erasure points, plus process-bound selector metadata.
- Phase 1b: BT-4300, dynamic class sends run in the caller. This subsumes BT-4114.
- Phase 2a: BT-4301, guarded static class sends, the `nil` `ClassSelf` retired, and one eligibility rule.
- Phase 2b: BT-4302, the `class-method-process-resource` lint.
- Phase 3: BT-4303, REPL-protocol e2e test, docs, ADR amendments, CLAUDE.md and CHANGELOG.

**Status:** Done (2026-10-30). Follow-ups: BT-4316 (row 3a), BT-4317, BT-4307.

## Migration Path

This ADR changes behaviour only for class methods of classes **without**
class variables.

- **Resources owned by the class process.** Code that creates an ETS table,
  port, timer, link or monitor in such a class method, and expects it to
  outlive the caller, must move ownership:
  - into an Actor (preferred: an `Actor subclass: TableOwner` that creates
    the resource in `initialize`), or
  - into a class with `classState:`, which keeps its process.

  The `class-method-process-resource` lint flags direct FFI cases.
- **Implicit serialisation.** Read-modify-write sequences in class methods
  that relied on queuing must go through an Actor. No lint can detect this,
  so the CHANGELOG calls it out.
- **Suppressions.** `@expect class_process_ffi` suppressions in stateless
  classes become unnecessary. The lint reports them as unused, per the
  existing `@expect` rules.
- **Erlang callers of `class_send`** see the same results, now computed in
  their own process.

## References
- Related issues: BT-4118, BT-4112 (epic), BT-4291 (guarded static dispatch
  epic), BT-4292, BT-4295, BT-4073, BT-4114, BT-4115,
  BT-4126, BT-4133, BT-4134, BT-4037, BT-3700, BT-3022.
- Related ADRs: 0025 (its 2026-10-29 amendment is the guard contract this ADR
  applies), 0010, 0013, 0049,
  0066, 0084, 0100, 0109, 0112, 0129, 0130, 0137.
- Documentation: `benchmarks/vs-elixir/README.md`,
  `docs/development/benchmarks.md` (BT-3676/3690/3700),
  `docs/beamtalk-principles.md` §4.
