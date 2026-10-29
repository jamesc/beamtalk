# ADR 0142: Guarded Static Dispatch — the Guard Is the Contract

## Status
Proposed (2026-10-28)

## Context

### Problem

A Beamtalk send costs far more than the equivalent Elixir call, and most of
that cost is lookup machinery, not semantics. The `benchmarks/vs-elixir`
suite (jamesc/beamtalk#4508, the research input BT-4118 asked for) measures
it in deterministic reductions and words:

| Row | Send | Beamtalk | Elixir | Cause |
|---|---|---|---|---|
| 1b / 4c | typed send to an **open** Value (`p x`, `p m: 1`) | 29 red / 28 w, ~160 ns | `p.x` 1.5 ns | full `beamtalk_message_dispatch:send/3` (22 calls) |
| 5b | self-send inside an open Value | 61 red / 59 w | — | a second full send |
| 12b / 8a | a bare class reference (`Error`, `Point`) | rebuilt per use | a module atom | `binary_to_atom` + `whereis` + 2 ETS reads (`module_name_safe`) + a fresh tuple |

The 12b/8a cause is the current code, after BT-4065. The README's table
predates that change and still lists a `gen_server:call` for those rows.

The README states the conclusion: "for open classes, late binding is
structural, but its cost is not: a guarded direct call costs a map-key
match". It also notes that "everything else … is fixable without changing
the language".

Class-side sends are slow too (rows 3b/3c, ~145 red, through the class
process). They involve a separate semantic decision and are handled by
ADR 0143, which builds on this ADR's guard contract.

### What a real program sends

Catalina (jamesc/beamtalk-catalina, 0.2.0) is the Thousand Island port that
motivates this epic. Every class in it is `typed`. All measurements below
were taken on 2026-10-29, on beamtalk main f5accd5.

**Send sites.** A compiler instrumented to print each dynamic-dispatch site
with its receiver's static type finds **220 dynamic send sites** across
Catalina's 21 source files:

| Receiver's static type | Sites | Examples |
|---|---|---|
| Built-in collections and primitives (`Dictionary` 31, `List` 22, `Tuple` 10, `Binary` 2, `Boolean` 2, `Integer` 1) | 68 | `Dictionary at:put:` 14, `List add:` 12, `Tuple at:` 8, `List includes:` 6 |
| `Supervisor` subclasses (`ServerSupervisor`, `ConnectionSupervisor`, …) | 43 | instances are `beamtalk_supervisor` tuples, not tagged maps |
| Declared `Object` | 9 | "any object" |
| Open Value class (`SupervisionSpec`) | 6 | |
| `Exception`/`Error`, `Symbol`, `Pid` | 15 | |
| Actors (`Listener`, `Handler`) | 9 | |
| Untyped: `Dynamic` 60, negation 6, other 4 | 70 | `Result` checks (`error`, `isError`, `isOk`), socket calls on `Dynamic` transport values (`close:`, `send:data:`) |

Two-thirds of the sites already have a static type. The largest single group
is built-in collections, which BT-4069's rule excludes entirely.

**Per message.** `bench_profile` ran on the `echo_c10_64B` row: 10
connections, 64-byte messages, call-time breakpoints on every server
function, 7,651 messages in 5 s. The table shows each group's share of server
CPU. Breakpoints inflate small, frequently called functions, so the shares
are approximate.

| Group | Share | Per message | Addressed by |
|---|---|---|---|
| Actor state-cell sync (ADR 0137): `beamtalk_actor_state` `store`, `call_foreign`, `reflect_cell` | ~14% | 27 `store`, 6 `call_foreign` | nothing yet (BT-4279) |
| Inherited actor self-sends: `beamtalk_dispatch` `super`/`lookup_in_class_chain`/`invoke_resolved`, plus superclass, metadata and extension lookups | ~11% | 6 | BT-4109 cached the owner, but the path around the cache remains (BT-4280) |
| Dynamic sends to Values and built-ins: `beamtalk_message_dispatch`, `beamtalk_primitive`, `beamtalk_tagged_map` | ~9% | 9 sends (4 tagged-map, 4 by representation) | **this ADR, §2 and §2b** |
| Class-object resolution: `whereis_class`, `registry_name` (`binary_to_atom`), `class_name_for_pid`, derived-cache reads | ~6% | 9 | **this ADR, §3** (class references); BT-4127 (`obj class`) |
| Telemetry, `strip_local_temps` | ~6% | 2 events, 12 strips | BT-4113 |
| Value construction: `beamtalk_class_instantiation` declared-fields walk | ~4% | 4 | nothing yet (BT-4281) |
| FFI proxy (`apply_with_coercion`) | ~3.5% | 6 | BT-4153 (done), residual |
| Catalina and harness code, OTP, I/O | ~30% | | the floor |

This ADR addresses about 15% of Catalina's per-message CPU. The two largest
runtime costs are outside its scope and are tracked separately: state-cell
sync (~14%) and the inherited-self-send path (~11%). Class-side sends
(ADR 0143) are about 1%.

### Current state

`typed_sealed_value_target` (`dispatch_codegen.rs`) implements BT-4069. When a
receiver's static type is a *sealed* tagged-map class, it emits:

`case maps:get('$beamtalk_class', R) of 'C' -> 'bt@…c':dispatch(Sel, Args, R); _ -> beamtalk_message_dispatch:send(R, Sel, Args) end`

- Its doc comment says: "The guard, not the annotation, is trusted."
- Row 1a went from 29.1 red / 28 w to 3.0 / 0.
- The rule is gated on `sealed`, but its soundness comes entirely from the
  guard.
- Built-in receivers (`List`, `Dictionary`, …) are excluded outright.

### Constraints

- **Runtime dispatch is the contract; direct calls are the optimisation**
  (ADR 0005, quoted in ADR 0013). No send may answer differently because it
  was compiled statically.
- **Escape hatches are guaranteed** (ADR 0100): `doesNotUnderstand:`,
  `perform:`, extensions (ADR 0066, which can override *inherited* methods),
  `removeSelector:` re-exposing an inherited method (ADR 0112), and hot reload
  (principles §2, §11).
- **Types are warnings** (ADR 0025, non-goal "mandatory static typing"). An
  annotation can be a lie at run time, so it may pick a target but never prove
  one.
- **No image mechanics** (principles §4). An identifier must mean the same
  thing in every context: REPL, `beamtalk run`, `beamtalk test`, releases. A
  class process restarts, so its pid is not a constant.
- **One rule implementation across Rust and Erlang**
  (architecture-principles §6–7). Routing knowledge that crosses the boundary
  is pinned by a shared fixture.
- **ADRs 0127, 0112 and 0066 rely on inherited methods not being copied.**

## Decision

### 1. The guard is the contract

A send may compile to a direct call only behind a **run-time guard that
proves the direct target is exactly what dynamic dispatch would reach**. On a
guard miss it falls back to a **byte-identical dynamic send**.

- Static facts pick the candidate target: a declared or inferred type, a
  class literal, or `self` inside its own class. They never prove it.
- `sealed` and `typed` do not change what any send means. They only make
  guards hit more often, or let the compiler pick a candidate at all.

BT-4069 already follows this rule. This ADR makes it the rule for every
static dispatch, and ADR 0143 applies it to the class side.

### 2. Tagged-map receivers: guarded by class tag

The `is_sealed` requirement is dropped. For any receiver whose static type is
`Known{C}` (provenance Declared, Inferred or Substituted), where `C`'s
instances are tagged maps (Value or Object kind), the send compiles to the
existing BT-4069 shape:

```erlang
case call 'erlang':'is_map'(R) of
  <'true'> -> case call 'maps':'get'('$beamtalk_class', R, 'undefined') of
                <'Point'> -> call 'bt@app@point':'dispatch'('x', [], R)
                <_>       -> call 'beamtalk_message_dispatch':'send'(R, 'x', [])
              end
  <'false'> -> call 'beamtalk_message_dispatch':'send'(R, 'x', [])
end
```

**Why this is sound without `sealed`.**
- The tag names the receiver's *exact* class.
- The dynamic path for a receiver tagged `C` resolves `C`'s module and calls
  the same `Mod:dispatch/3` (`tagged_class_dispatch` → `value_type_send`; its
  one pre-check, `is_ivar_method`, only affects the excluded `fieldAt:put:`).
- `Mod:dispatch/3` then does everything the dynamic path relies on:
  extensions first in its default arm, then `SuperMod:dispatch/3` for
  inherited selectors, then `doesNotUnderstand:args:` or the DNU error.
- A subclass instance carries its own tag, misses the guard, and takes the
  dynamic path. Late binding to subclass overrides is untouched.

The candidate is a module name, not a method. That is why a same-module
reload, `removeSelector:` and extensions need no invalidation: a remote call
reaches the newest loaded `Mod`, and all three act inside `Mod:dispatch/3` or
the registries it reads.

**Accepted caveat: the module name is pinned at compile time.** The dynamic
path looks the module up at run time (`beamtalk_class_metadata:lookup_module/1`),
but the guarded call names the module the compiler saw. Two cases diverge:
- A REPL redefinition that moves the class into a *different* module
  (BT-4200): stale call sites keep reaching the old module's code.
- `removeFromSystem`: stale call sites get a raw `undef`, where the dynamic
  path gives `does_not_understand` (BT-4149).

Both already exist for BT-4069's sealed receivers; this rule widens them to
every typed receiver. Both happen only during development, and recompiling
the callers fixes them. This ADR accepts the caveat rather than pay a
registry read on every send, and the language-features hot-reload section
documents it. A zero-cost fix is a follow-up issue: for example, a retired
class module whose `dispatch/3` forwards to `beamtalk_message_dispatch:send/3`.

Scope:
- **Self-sends in open Values** are covered when the method runs on an
  instance of its own class, because the type checker types `self` as
  `Known{C}` (Inferred). In an *inherited* method running on a subclass
  instance, `self` is tagged with the subclass. That send misses the guard
  and stays a full dynamic send: correct, but not faster.
- **Excluded: classes whose instances are not tagged maps.**
  - `Supervisor` and its subclasses: their instances are
    `{beamtalk_supervisor, …}` tuples.
  - The declared type `Object`, which means "any object"; a guard on the tag
    `'Object'` would never hit.
  - Without these two exclusions, 52 of Catalina's 220 sites would gain a
    guard that can never hit.
  - The representation facts come from the routing fixture (§2b).
- **Exclusions kept from BT-4069:**
  - runtime-only builtins without a generated module;
  - module-less (ClassBuilder) classes;
  - the `Exception` hierarchy;
  - classes with a cross-file parent;
  - binary selectors;
  - `fieldAt:put:`;
  - receivers whose class name more than one package in scope exports
    (BT-4151 fails closed to dynamic).
- **Unions, `Dynamic` and untyped receivers** stay dynamic. Catalina's 70
  untyped sites are a typing problem at the FFI and transport boundary, not a
  dispatch problem.
- **Actor receivers** are out of scope. A send to an actor is a `gen_server`
  call whatever its type; BT-4113 covers its hook cost.

### 2b. Built-in receivers: guarded by representation

Instances of built-in classes such as `List`, `Dictionary` and `Tuple` have
no class tag: they *are* the Erlang term. The dynamic path routes them by
representation:
- `beamtalk_message_dispatch:send/3` → `beamtalk_primitive:send/3` →
  `dispatch_via_module/3` → `module_for_value/2` → `Mod:dispatch(Sel, Args, R)`;
- a map without `'$beamtalk_class'` goes through `send_map/3` to Dictionary's
  module.

When a receiver's static type is `Known{B}` for one of these built-ins, the
guard is the **representation test** `module_for_value` uses, and the target
is the module it would pick:

```erlang
case call 'erlang':'is_list'(R) of
  <'true'>  -> call 'bt@stdlib@list':'dispatch'('includes:', [X], R)
  <'false'> -> call 'beamtalk_message_dispatch':'send'(R, 'includes:', [X])
end
```

| Static type | Guard | Target |
|---|---|---|
| `List` | `is_list(R)` | `bt@stdlib@list` |
| `Dictionary` | `is_map(R)` and no `'$beamtalk_class'` key | `bt@stdlib@dictionary` |
| `Tuple` | `is_tuple(R)` and the first element is not a reserved tag (`beamtalk_object`, `beamtalk_future`, `beamtalk_supervisor`, …) | `bt@stdlib@tuple` |
| `Integer`, `Float`, `Boolean`, `Symbol`, … | the matching `module_for_value/1` clause | its module |

- **`String` and `Binary` keep their current routing.** Their selector-aware
  fast path consults `beamtalk_extensions:has('String', Sel)` at run time.
  Matching it would need a guard of its own, so these two wait for a later
  phase if measurements justify one.
- **Pinned to one fixture.** The table (guard, reserved tuple tags, target
  module) is generated from `tagged_map_routing_conformance.json`, extended
  with representation rows, and is never restated in Rust. The Erlang side's
  conformance test pins these to the same rows:
  - `module_for_value/1`;
  - `beamtalk_primitive:send/3`'s tuple and map clauses;
  - `beamtalk_message_dispatch:send/3`'s future and supervisor clauses.
- **Why it is sound.**
  - The guard is the exact clause the dynamic path would take, and the target
    is the module it would call.
  - Extensions on built-ins and DNU are handled inside `Mod:dispatch/3`, as
    today.
  - Anything that fails the guard takes the dynamic path. That includes a
    lying annotation and a reserved tagged tuple typed as `Tuple`.

### 3. Class references: compile-time names, run-time pid

A bare class name compiles to a reference built from a compile-time atom, one
BIF and the existing module lookup:

```erlang
case call 'erlang':'whereis'('beamtalk_class_Point') of
  <'undefined'> -> <class_not_found, as today>
  <Pid>         -> {'beamtalk_object', 'Point class', call 'beamtalk_object_class':'module_name_safe'(Pid), Pid}
end
```

- **The registered-name atom** (`beamtalk_class_<Name>`) is computed at
  compile time with `leaf::atom`. Today
  `beamtalk_class_registry:registry_name/1` runs `binary_to_atom` on every
  use.
- **The module stays a run-time lookup.** The class gen_server writes it in
  `init/1` and `merge_identity/5`, so it stays correct when the REPL
  redefines a class into a new module.
  - A `persistent_term` would save about one ETS read, but every REPL
    redefinition would then trigger a global GC scan.
  - Phase 2 measures both before choosing.
- **The pid is never a constant.** A class process can restart, and an
  identifier must mean the same thing in every context (principles §4).
- **Names only where only a name is used.** For an `on:do:` class filter, or
  `isKindOf:` with a literal argument, codegen passes the atom and builds no
  tuple.
- **REPL session bindings** that shadow a class name keep today's lookup,
  checked first.

### 4. What does not change

- **`perform:`** stays a dynamic send.
- **DNU, `respondsTo:` and extensions** keep their order and semantics. They
  are reached inside `Mod:dispatch/3`.
- **Hot reload:** a reloaded module is reached by remote calls. The
  module-identity caveat of §2 applies.
- **`typed`** stays compile-time only *semantically*. ADR 0025's claim that
  generated code is "identical whether a class is typed or not" is amended:
  types may change the code, never the result.
- **Inherited methods are never copied** (ADR 0127 stands).

### Example

```beamtalk
Value subclass: Point
  field: x :: Integer = 0
  field: y :: Integer = 0

  norm1 -> Integer => self.x abs + self.y abs
  double: p :: Point -> Integer => p norm1 * 2      // `p norm1` is a guarded direct call
  tally: seen :: List -> Integer => seen size       // `seen size` is guarded by is_list (§2b)
```

```beamtalk
(Point x: 3 y: -4) double: (Point x: 1 y: 1)   // => 4
Point >> norm1 => 42                           // recompiles Point's recorded source; same module, so the guard reaches the new code
(Point x: 3 y: -4) double: (Point x: 1 y: 1)   // => 84
(Point new) tally: #(1, 2, 3)                  // => 3
```

### Error behaviour

There are no new errors. A lying annotation misses the guard and gets the
dynamic answer, including `doesNotUnderstand`:

```beamtalk
Point new double: "not a point"
// the type checker warns at compile time (ADR 0025), as today;
// at run time: ERROR: String does not understand 'norm1', identical to the untyped send
```

## Prior Art

- **Self / Strongtalk.**
  - Self's customisation and polymorphic inline caches make the common case a
    class check plus a direct jump, with deoptimisation on change.
  - Strongtalk's optional types deliberately do **not** drive dispatch.
  - *Adopted:* "check the class, then jump" as the shape, and types as hints,
    never proofs.
  - *Rejected:* per-site mutable caches.
- **Pharo / Squeak.** Every send is a lookup, made cheap by the Cog JIT's
  inline caches, and only control-flow selectors are inlined. Beamtalk has no
  JIT, so its compiler supplies what Cog's caches discover.
- **Erlang.** Remote calls `Mod:f()` always reach the newest loaded code. That
  is why a guarded `Mod:dispatch/3` needs no invalidation on a same-module
  reload.
- **Elixir.**
  - Struct dispatch is a pattern match on `__struct__`, the same shape as our
    tag guard.
  - Protocol consolidation compiles dispatch into a `case` at build time,
    with an unconsolidated dynamic fallback in development.
  - *Adopted:* the tag guard, and the representation guard of §2b, which is
    exactly how a consolidated protocol dispatches on built-in types.
- **JVM / Swift / Dart.**
  - `final`/`sealed` plus class-hierarchy analysis enable devirtualisation,
    and the JVM keeps it sound with guards and deoptimisation.
  - *Adapted:* we keep the guard and drop the reliance on `sealed`, because a
    map-key match is cheap enough to use everywhere.
- **Julia.** World-age and backedge invalidation recompile callers when a
  method they inlined changes. We avoid that by never naming a method body at
  a call site, only a module whose `dispatch/3` is itself late-bound.

## User Impact

- **Newcomer.** Adding `:: Point` or `:: List` makes a send faster and
  changes nothing else.
- **Smalltalk developer.** Message-passing semantics are untouched. Every
  send answers what the dynamic lookup answers, and DNU, `perform:`,
  extensions and live patching all work as before.
- **Erlang/Elixir developer.** Hot paths compile to a `case` plus a remote
  call, readable in `.core` files and in `recon` traces.
- **Production operator.** Hot reload is unaffected, apart from the
  development-time module-identity caveat (§2). There is nothing to
  configure.
- **Tooling developer.** The dynamic-site report (Phase 0) shows every send
  that stayed dynamic and why. That makes "add a type here" an actionable
  hint.

## Steelman Analysis

Option A is the decision above. Options B–E are the alternatives.

### B. Sealed is the contract (BT-4118's original framing)
- 🧑‍💻 **Newcomer:** "One rule: seal it to make it fast."
- 🎩 **Smalltalk purist:** "Dynamic stays dynamic unless I explicitly give it
  up."
- ⚙️ **BEAM veteran:** "A sealed class can never gain subclasses, so a direct
  call needs no subclass reasoning."
- 🎨 **Language designer:** "The keyword that already exists carries the
  guarantee."

*Why not:*
- Annotations can lie, so the guard is needed anyway. With it in place,
  `sealed` adds nothing to soundness and only shrinks coverage: rows 1b, 4c
  and 5b stay slow.
- Framework base classes (`Handler`, `Server`) are open by design.

### C. Run-time inline caches
- 🎩 **Smalltalk purist:** "This is how Self and Cog do it, and it needs no
  types."
- 🧑‍💻 **Newcomer:** "Untyped and REPL code get faster too."

*Why not:* BEAM has no cheap mutable per-call-site slot.
- A `persistent_term` per site means a global GC on every update.
- The process dictionary is per process and leaks.

Static candidates plus a guard reach the same shape with no mutable state.
The untyped share (70 of Catalina's 220 sites) is better reduced by typing
the FFI and transport boundary.

### D. Copy-down / consolidation of inherited methods
- 🏭 **Operator:** "No guard and no delegation hop in steady state."
- ⚙️ **BEAM veteran:** "It's Elixir protocol consolidation."

*Why not:*
- For Values, inheritance costs about 4 reductions per level
  (`SuperMod:dispatch/3`).
- Copy-down contradicts ADR 0127 ("inherited methods are never copied"),
  ADR 0112 (`removeSelector:` re-exposes the inherited method) and ADR 0066
  (extensions override inherited methods).
- It needs a recompile fan-out on every superclass edit.

The actor-side inherited path *is* expensive in Catalina (~11%). But that
cost is the run-time path around BT-4109's cache, not the lookup, so BT-4280
fixes it without copying.

### E. Class references as load-time constants (pid baked in)
- 🏭 **Operator:** "Zero cost per use."

*Why not:* a class process restarts, and a REPL redefinition can move a class
to a new module, so a baked pid breaks principles §4. A `whereis/1` on a
compile-time atom costs almost nothing.

### Tension points
- Purists would rather keep "every send is a lookup", while the measurements
  say lookups cost about 9% of Catalina's per-message CPU. The guard keeps the
  lookup's *answer* and drops its *cost*, which is the compromise this ADR
  takes.

## Alternatives Considered

### Sealed is the contract
Rejected: the guard is needed regardless (see steelman B).

### Run-time inline caches
Rejected: there is no cheap mutable per-site storage on BEAM (steelman C).

### Copy-down of inherited methods
Rejected: the cost is small for Values, and copy-down conflicts with ADRs
0066, 0112 and 0127 (steelman D). Revisit only if a profile shows Value
inheritance hops above 5% of a real workload.

### Load-time class constants
Rejected: pids and REPL module names are not constants (steelman E).

## Consequences

### Positive
- Typed sends to any tagged-map class, open or sealed, and to the built-in
  collections (§2b) cost about what row 1a costs today: ~3 red / 0 w, down
  from 29 / 28. Self-sends in open Values follow.
- Class references stop building an atom per use.
- One instance-side rule replaces the sealed-only special case, and its
  routing facts live in one fixture.
- Catalina: §2 and §2b convert about 74 of 220 dynamic sites (34%) and
  address ~15% of per-message CPU.

### Negative
- **Module identity is pinned at typed call sites (§2).** A REPL
  redefinition into a new module, or `removeFromSystem`, leaves stale call
  sites reaching the old code, or getting a raw `undef`, until they are
  recompiled. This is already true for sealed receivers; it now applies to
  every typed receiver.
- Code grows slightly at typed send sites: each gets a guard plus the
  fallback.
- The fixture becomes load-bearing for performance as well as correctness.

### Neutral
- `sealed` keeps its meaning ("no user subclasses"). It now only *helps*
  guards hit (an exact type is never a subclass instance) and no longer gates
  dispatch.
- ADR 0025's "identical bytecode" claim becomes "identical results".
- `String`/`Binary` receivers, untyped sends and actor sends are unchanged.
  The largest Catalina runtime costs are tracked outside this ADR:
  - state-cell sync, BT-4279;
  - the inherited actor self-send path, BT-4280;
  - Value construction, BT-4281;
  - actor hooks, BT-4113.

## Implementation

All phases sit under epic BT-4112. Each phase adds a vs-elixir row or a
Catalina gate before the change, and records the before and after numbers.

**Phase 0 — harness (S).**
- Generalise BT-4150's differential test. A fixture runs each send shape,
  guarded-direct against `beamtalk_message_dispatch:send/3`, over:
  - plain and keyword sends, `perform:`, `perform:withArguments:timeout:`
    (rewritten in `send/3` before classification);
  - DNU, and extensions, including one overriding an inherited method;
  - `removeSelector:`, a live patch, a subclass receiver, a lying annotation;
  - each §2b built-in, including a reserved tagged tuple typed as `Tuple`.

  Results *and* error shapes must be identical.
- Add vs-elixir rows for an open-Value typed send, typed `List includes:`,
  `Dictionary at:put:` and `Tuple at:` sends, and a class-reference loop.
- Make the dynamic-site report a supported tool. The
  `BEAMTALK_CODEGEN_DIAGNOSTICS` "dynamic dispatch" hint gains the receiver's
  static type and the reason it stayed dynamic. Record Catalina's per-type
  table and per-message profile (Context) as the baseline.
- Extend the routing fixture with representation rows (§2b), and pin the
  Erlang side to it.

**Phase 1 — tagged-map receivers (S–M), codegen.**
- Drop `is_sealed` from `typed_sealed_value_target`, and rename it
  `typed_tagged_target`.
- Make sure Value self-sends in open classes reach it.
- Add the `Supervisor` and `Object` exclusions.
- Land BT-4151's ambiguity check first or together.
- Files: `dispatch_codegen.rs`, `tests/typed_sealed.rs`.
- Gate: rows 1b and 4c within 10% of row 1a; row 5b down by at least 25 red.

**Phase 1b — built-in receivers (M), codegen.**
- Emit the §2b representation guards from the fixture-generated table.
- Files: `dispatch_codegen.rs`, the routing fixture and its EUnit
  conformance test.
- Gate:
  - the built-in rows cost within 10% of row 1a;
  - Catalina's dynamic sites fall from 220 to at most 150;
  - the dynamic-send share of the `echo_c10_64B` profile falls by at least
    half.

**Phase 2 — class references (S).**
- Use the compile-time registered-name atom and `whereis/1`, plus the
  existing `module_name_safe/1` row. Measure against a `persistent_term`
  first (§3).
- Pass the bare name where only the name is used.
- Gate: rows 8a and 12b, and the `whereis_class`/`registry_name` share of
  the Catalina profile.

**Phase 3 — docs (S).**
- Amend ADR 0025 ("identical bytecode" becomes "identical results").
- Update `docs/beamtalk-language-features.md`:
  - the hot-reload section: the module-identity caveat;
  - the performance notes on typed sends;
  - `sealed` (it no longer gates instance-side dispatch).

**Conformance.** The compiler only names candidates. The guard decides.
Instance-side routing knowledge that crosses the Rust/Erlang boundary is
pinned by `tagged_map_routing_conformance.json`, and the Phase 0 differential
test pins behaviour.

## References
- Related issues:
  - BT-4118 (this ADR), BT-4112 (epic);
  - BT-4069, BT-4065, BT-4117, BT-4127, BT-4149, BT-4150, BT-4151, BT-4200;
  - outside this ADR's scope: BT-4113, BT-4279, BT-4280, BT-4281.
- Related ADRs: 0005, 0025, 0066, 0100, 0112, 0127, 0143 (applies this
  contract to the class side).
- Documentation: `benchmarks/vs-elixir/README.md`, Catalina's
  `bench/harness/bench_profile.erl`, `docs/beamtalk-principles.md` §2, §4,
  §11.
