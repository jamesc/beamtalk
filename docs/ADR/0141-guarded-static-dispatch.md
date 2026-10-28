# ADR 0141: Guarded Static Dispatch — the Guard Is the Contract, and the Class Process Only Owns Class State

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
| 3b | class method, sealed class, method not `class sealed` | 145 red / 444 w, 2.3 µs | `Mod.fun(arg)` 2.9 ns | `gen_server:call` into the class process |
| 3c | class method on an open class | 146 red / 480 w, 2.6 µs | 2.8 ns | same |
| 12b / 8a | a bare class reference (`Error`, `Point`) | rebuilt per use | a module atom | `binary_to_atom` + `whereis` + 2 ETS reads (`module_name_safe`) + a fresh tuple |

The 12b/8a cause is the current code, after BT-4065. The README's table
predates that change and still lists a `gen_server:call` for those rows.

The README states the conclusion: "for open classes, late binding is
structural, but its cost is not: a guarded direct call costs a map-key
match". It also notes that "everything else … is fixable without changing
the language".

Class-side sends have a second cost besides speed. Every caller in the node
queues on one class process. The `class-process-ffi` lint (BT-3999 family)
exists because running a class method in the class process is *wrong* for
caller-sensitive FFI. For example, `gen_tcp:accept` blocks every caller, and
`controlling_process`, `self()`, `link` and `monitor` act on the wrong
process.

### Current state: three special cases, three rules

Static dispatch already exists, but as three unrelated rules:

1. **Sealed-Value typed receivers (BT-4069).**
   - When the receiver's static type is a sealed tagged-map class,
     `typed_sealed_value_target` (`dispatch_codegen.rs`) emits:
     `case maps:get('$beamtalk_class', R) of 'C' -> 'bt@…c':dispatch(Sel, Args, R); _ -> beamtalk_message_dispatch:send(R, Sel, Args) end`.
   - Its doc comment says: "The guard, not the annotation, is trusted."
   - Row 1a went from 29.1 red / 28 w to 3.0 / 0.
2. **`class sealed` methods of sealed, stateless classes (ADR 0129 Phase 0b),
   plus synthesized constructors (BT-4099).**
   - These are direct `Mod:class_sel(nil, …)` calls, statically and through
     `lookup_direct_call/3` for dynamic class sends.
   - The eligibility verdict is computed in Rust and baked into
     `__beamtalk_meta/0`.
   - Static call sites trust the verdict unguarded. That causes the
     BT-4126, BT-4133, BT-4134 and BT-4037 staleness caveats.
3. **Open class-side self-sends (BT-3700).**
   - `class_self_direct_ok/4` reads one `persistent_term` flag per send,
     maintained by `beamtalk_class_shadow_flags`.
   - The flag is erased under a per-tag lock before an extension or ADR 0084
     runtime fun becomes visible.

The first is gated on `sealed`, though its soundness comes entirely from the
guard. The second is gated on `sealed` and `class sealed`, though what the
class process actually protects is class variables (ADR 0130). The third is
the general mechanism, used in one corner.

### Constraints

- **Runtime dispatch is the contract; direct calls are the optimisation**
  (ADR 0005, quoted in ADR 0013). No send may answer differently because it
  was compiled statically.
- **Escape hatches are guaranteed** (ADR 0100): `doesNotUnderstand:`,
  `perform:`, extensions (ADR 0066, which can override *inherited* methods),
  ADR 0084 runtime class-method funs, `removeSelector:` re-exposing an
  inherited method (ADR 0112), and hot reload (principles §2, §11).
- **Types are warnings** (ADR 0025, non-goal "mandatory static typing"). An
  annotation can be a lie at run time, so it may pick a target but never
  prove one.
- **No image mechanics** (principles §4). An identifier must mean the same
  thing in every context: REPL, `beamtalk run`, `beamtalk test`, releases. A
  class process restarts, so its pid is not a constant.
- **One rule implementation across Rust and Erlang**
  (architecture-principles §6–7). The compiler may pick candidates, the
  runtime decides, and any routing knowledge that crosses the boundary is
  pinned by a shared fixture.
- **ADR 0127 / 0112 / 0066 rely on inherited methods not being copied.**

## Decision

### 1. The guard is the contract

A send may compile to a direct call only behind a **run-time guard that
proves the direct target is exactly what dynamic dispatch would reach**. On a
guard miss it falls back to a **byte-identical dynamic send**.

- Static facts pick the candidate target: a declared or inferred type, a
  class literal, or `self` inside its own class.
- They never prove it.
- `sealed` and `typed` do not change what any send means. They only make
  guards hit more often, or let the compiler pick a candidate at all.

This is the rule BT-4069 already follows. The ADR makes it the only rule.

### 2. Instance side: guarded direct calls for any known tagged-map class

The `is_sealed` requirement is dropped from `typed_sealed_value_target`. For
any receiver whose static type is `Known{C}` (provenance Declared, Inferred
or Substituted), where `C` is a tagged-map class (Value or Object kind), the
send compiles to the existing BT-4069 shape:

```erlang
case call 'erlang':'is_map'(R) of
  <'true'> -> case call 'maps':'get'('$beamtalk_class', R, 'undefined') of
                <'Point'> -> call 'bt@app@point':'dispatch'('x', [], R)
                <_>       -> call 'beamtalk_message_dispatch':'send'(R, 'x', [])
              end
  <'false'> -> call 'beamtalk_message_dispatch':'send'(R, 'x', [])
end
```

**Why this is sound without `sealed`:**
- The tag names the receiver's *exact* class.
- The dynamic path for a receiver tagged `C` resolves `C`'s module and calls
  the same `Mod:dispatch/3` (`tagged_class_dispatch` → `value_type_send`).
- `Mod:dispatch/3` then does everything the dynamic path relies on:
  extensions first in its default arm, then `SuperMod:dispatch/3` for
  inherited selectors, then `doesNotUnderstand:args:` or the DNU error.
- A subclass instance carries its own tag, misses the guard, and takes the
  dynamic path, so late binding to subclass overrides is untouched.

The candidate is a module name, not a method. That is why a same-module
reload, `removeSelector:` and extensions need no invalidation:
- A remote call reaches the newest loaded version of `Mod`.
- Hot reload, `removeSelector:` and extension registration all act inside
  `Mod:dispatch/3` or the registries it reads.

**Accepted caveat: the module name is pinned at compile time.** The dynamic
path looks the module up at run time (`beamtalk_class_metadata:lookup_module/1`).
The guarded call names the module the compiler saw. Two cases diverge:
- A REPL redefinition that moves the class into a *different* module (BT-4200).
  Stale call sites keep reaching the old module's code.
- `removeFromSystem`. Stale call sites get a raw `undef` where the dynamic path
  gives `does_not_understand` (BT-4149).

Both cases already exist for BT-4069's sealed receivers. Dropping `is_sealed`
widens them to every typed receiver. Both are development-time operations, and
recompiling the callers fixes them. This ADR accepts the caveat rather than
pay a registry read on every send. It documents the caveat in the
language-features hot-reload section. A zero-cost fix is a follow-up issue:
for example, a retired class module whose `dispatch/3` forwards to
`beamtalk_message_dispatch:send/3`.

Consequences of the rule:
- **Self-sends in open Values** are covered when the method runs on an
  instance of its own class. The type checker types `self` as `Known{C}`
  (Inferred), so row 5b becomes a guarded direct call. In an *inherited*
  method running on a subclass instance, `self` is tagged with the subclass.
  That self-send always misses the guard and stays a full dynamic send, which
  is correct but not faster.
- **Unchanged exclusions:**
  - `NON_TAGGED_MAP_CLASSES` (primitives keep their own routing, pinned by
    `tagged_map_routing_conformance.json`);
  - runtime-only builtins without a generated module;
  - module-less (ClassBuilder) classes;
  - the `Exception` hierarchy;
  - classes with a cross-file parent;
  - binary selectors;
  - `fieldAt:put:`;
  - receivers whose class name is exported by more than one package in scope
    (BT-4151 fails closed to dynamic).
- **Unions, `Dynamic` and untyped receivers** stay dynamic.
- **Actor receivers** are out of scope. A send to an actor is a `gen_server`
  call whatever its type, and its hook cost is BT-4113.

### 3. Class side: the class process exists only to own class state

A class method runs **in the caller** unless the receiving class needs its
process. The test is a per-class run-time flag, `direct_call_ok(C)`. It is
the existing `beamtalk_class_shadow_flags` derived flag, which gains one
input. The flag is true exactly when all of these hold:

- `C` has no class variables (`classState:`), which is the new input;
- no ADR 0084 runtime class-method fun is installed on `C`;
- no class-side extension shadows a compiled method of `C`;
- the class has finished bootstrap (`mark_ready`).

The **defining** class of the resolved method must also have no class
variables. A class method may only touch the class variables its own class
declares, and it reads them through the receiver's class-process home (ADR
0130). Take a method defined on a stateful `Base` and sent to a stateless
`Sub`:
- Today it runs in `Sub`'s process and raises `undeclared_class_variable`.
- Run in the caller, it would raise `class_state_unreachable` instead.

So such a method stays in the class process, with an unchanged error. The
memo below records the owner's flag together with the owner. Methods run in
the caller install no class-variable home, so ADR 0130 §1's "no nested home"
invariant is untouched.

The selector must also not be process-bound. These selectors keep their
current routing:
- `new`, `new:`, `startLink` and `startLink:` (ADR 0129). Value constructors
  keep their BT-4099 direct path.
- `spawn` and `spawnWith:`, which have dedicated clauses in
  `beamtalk_class_dispatch`.
- `Supervisor` `supervise`/`spawn*`, which are routed before `class_send_plain`.
- `TestCase` run selectors, which answer `test_spawn` and need the
  gen_server's `noreply`.

Rust owns this list, in `class_info.rs` next to `is_direct_call_eligible`. It
reaches the runtime as generated metadata, and the `class-state-abroad` lint
and the migration lint read the same list.

Neither `sealed` nor `class sealed` is part of the test.

**Dynamic class sends** (`beamtalk_class_dispatch:class_send_plain/3`, used
by variables, `perform:`, `classNamed:`, a class passed as an argument, and
Erlang callers):
- When `direct_call_ok(C)` holds, resolve the defining class and module in
  the caller. Use a memo `{class_method_owner, C, Sel}` in
  `beamtalk_class_derived_cache` (the BT-4109 `resolve_owner` pattern), then
  apply it in the caller if the owner also has no class variables.
- Deriving the memo walks the chain exactly as the class process does today.
  A class-side extension or an ADR 0084 runtime fun at each ancestor counts as
  that ancestor's method. A shadow on an *ancestor* never clears the
  receiver's flag, so the memo is what keeps it correct. Every change that
  can alter the answer must therefore invalidate the derived cache:
  - `beamtalk_extensions:register` and `remove` already do.
  - `beamtalk_class_metadata:set_runtime_class_methods/2` does not today, and
    Phase 2 adds it.
- Otherwise use the class process, as today.
- A subclass with class variables that inherits a method from a stateless
  parent fails its own flag, so it still runs in its process. ADR 0130
  semantics are preserved by construction.

**Static class sends with a literal receiver** (`Point origin`, and
class-side `self foo`) whose selector `C` itself defines compile to:

```erlang
case call 'beamtalk_class_shadow_flags':'direct_call_ok'('Point class', 'Point') of
  <'true'> -> call 'bt@app@point':'class_origin'(PointRef)
  <_>      -> <today's class_send>
end
```

- `PointRef` is the class reference of §4, passed as the real `ClassSelf`.
  This retires the `nil` convention and its per-call prologue lookup
  (BT-4037).
- An **inherited** class method sent to a literal receiver stays on the
  dynamic path, which itself now runs in the caller through the memo. No call
  site names a superclass module, so nothing goes stale when the hierarchy
  changes. The memo costs one ETS read; measure it in Phase 2 before
  optimising further.
- The flag is erased on every metadata write for `C`: reload, a change to
  class variables, `removeSelector:`, `put_class_method`, and extension
  register or remove. It is erased under the existing per-tag lock *before*
  the change becomes visible. Once erased, call sites take the slow path
  until it is re-derived.
- This guard **replaces** the unguarded trust model for ADR 0129 static
  direct calls. That closes the stale-verdict caveats of BT-4126, BT-4133 and
  BT-4134 for class methods.

**What moves:**
- A block passed into such a class method runs in the caller, so the
  `dispatch_error` on re-entrant sends to the same class goes away. Inside an
  actor, it runs at home in the ADR 0137 sense.
- `self()`, the process dictionary, links, monitors, timeouts and any
  resource created through FFI (ETS tables, ports, timers) all belong to the
  caller.
- The services the class process adds around a call become no-ops, because
  the caller already is the context they recreate:
  - seeding the caller's session context;
  - mirroring `beamtalk_dispatch_caller_pid`;
  - adopting the entry group leader;
  - the 60 s / 300 s class-call timeouts.
  
  One reader needs checking. `File open:mode:`'s handle-ownership rule reads
  the mirrored caller pid, and Phase 2 must make its no-mirror path resolve to
  `self()`. A test covers it in the REPL, in `beamtalk run` and in
  `beamtalk test`.

A class that needs a serialising home declares class state (`classState:`) or
delegates to an Actor, which is the idiomatic BEAM owner. No new keyword is
added.

### 4. Class references: compile-time names, run-time pid

A bare class name compiles to a reference built from a compile-time atom, one
BIF and the existing module lookup:

```erlang
case call 'erlang':'whereis'('beamtalk_class_Point') of
  <'undefined'> -> <class_not_found, as today>
  <Pid>         -> {'beamtalk_object', 'Point class', call 'beamtalk_object_class':'module_name_safe'(Pid), Pid}
end
```

- The registered-name atom (`beamtalk_class_<Name>`) is computed at compile
  time with `leaf::atom`. Today `beamtalk_class_registry:registry_name/1` runs
  `binary_to_atom` on every use.
- The module stays a run-time lookup. The class gen_server writes it in
  `init/1` and `merge_identity/5`, so it is still correct when the REPL
  redefines a class into a new module.
  - A `persistent_term` would save about one ETS read. But every REPL
    redefinition would then trigger a global GC scan, so the ETS read stays.
  - Phase 4 measures both before choosing.
- The pid is never a constant. A class process can restart, and an
  identifier must mean the same thing in every context (principles §4).
- Where only the name is needed (an `on:do:` class filter, `isKindOf:` on a
  literal), codegen passes the atom and builds no tuple.
- REPL session bindings that shadow a class name keep today's lookup first.

### 5. What does not change

- **`perform:`** stays a dynamic send. It benefits from §3 because dynamic
  class sends also run in the caller.
- **DNU, `respondsTo:` and extensions** keep their order and semantics. On
  the instance side they are reached inside `Mod:dispatch/3`; on the class
  side the flag excludes shadowed classes.
- **Hot reload:**
  - A reloaded module is reached by remote calls. The module-identity caveat
    of §2 still applies.
  - A class that gains class variables, an extension or a runtime fun fails
    its flag before the change is visible.
  - `removeSelector:` is a metadata write, so it erases the flag.
- **`typed`** stays compile-time only *semantically*. ADR 0025's claim that
  generated code is "identical whether a class is typed or not" is amended:
  types may change the code, never the result.
- **Inherited methods are never copied** (ADR 0127 stands).

### REPL session

```beamtalk
Value subclass: Point
  field: x :: Integer = 0
  field: y :: Integer = 0

  class origin -> Point => self x: 0 y: 0
  norm1 -> Integer => self.x abs + self.y abs
  double: p :: Point -> Integer => p norm1 * 2      // `p norm1` is a guarded direct call
```

```beamtalk
(Point x: 3 y: -4) double: (Point x: 1 y: 1)   // => 4
Point origin                                   // => a Point (0, 0), computed in the caller: no class-process hop
Point >> norm1 => 42                           // recompiles Point's recorded source; same module, so the guard reaches the new code
(Point x: 3 y: -4) double: (Point x: 1 y: 1)   // => 84
Point class >> origin => self x: 9 y: 9        // also a recompile: the same guarded direct call reaches the new class_origin
Point origin                                   // => a Point (9, 9), still in the caller
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

- **Self / Strongtalk (Smalltalk family).**
  - Self's customisation and polymorphic inline caches make the common case a
    class check plus a direct jump, with deoptimisation on change.
  - Strongtalk's optional types deliberately do **not** drive dispatch:
    types are for checking, and the VM's type feedback does the
    optimisation.
  - *Adopted:* "check the class, then jump" as the shape, and types as hints,
    never proofs.
  - *Rejected:* per-site mutable caches. See Alternatives.
- **Pharo / Squeak.** Everything is a lookup, made cheap by the Cog JIT's
  inline caches, and only control-flow selectors are inlined by the compiler.
  There is no class process analogue, because class methods run in the
  caller. *Adopted:* class methods run where they are called.
- **Erlang.**
  - Remote calls `Mod:f()` always reach the newest loaded code. That is
    why a guarded `Mod:dispatch/3` needs no invalidation on a same-module
    reload.
  - Serialisation is something you ask for by owning a process, never a side
    effect of where code is defined. *Adopted:* the class process exists only
    for class state.
- **Elixir.**
  - Struct dispatch is a pattern match on `__struct__`, the same shape as our
    tag guard.
  - Protocol consolidation turns protocol dispatch into a compiled `case` at
    build time, with an unconsolidated dynamic fallback in development.
  - *Adopted:* the tag guard. *Considered:* consolidation (see "Copy-down").
- **JVM / Swift / Dart.**
  - `final`/`sealed` and class-hierarchy analysis enable devirtualisation.
    The JVM keeps it sound with guards and deoptimisation when new classes
    load.
  - *Adapted:* we keep the guard and drop the reliance on `sealed`, because
    our guard is cheap enough (a map-key match) to use everywhere.
- **Julia.** Its world-age and backedge invalidation recompiles callers when
  a method they inlined is redefined. That is powerful but heavy. We avoid
  the need by never naming a method body, inherited or not, at a call site.
  We only name a module whose `dispatch/3` is itself late-bound.

## User Impact

- **Newcomer.**
  - Adding `:: Point` makes a send faster and changes nothing else.
  - Class methods behave like functions in a module, which is what someone
    from Python, JS or Elixir expects. There is no hidden process to learn
    about until they declare class state.
- **Smalltalk developer.**
  - Message-passing semantics are untouched. Every send still answers what
    the dynamic lookup answers, and DNU, `perform:`, extensions and live
    patching all work as before.
  - Class methods running in the caller is how Pharo has always worked.
- **Erlang/Elixir developer.**
  - Hot paths compile to `case` + a remote call, readable in `.core` and in
    `recon` traces.
  - Class methods no longer serialise through a hidden `gen_server`, so
    caller-sensitive FFI (`gen_tcp:accept`, `controlling_process`, `self()`)
    does what it says.
  - If they want a serialising process, they write an Actor, as on OTP.
- **Production operator.**
  - The node-wide queue on each class process disappears for stateless
    classes, and with it a class of latency spikes and timeouts.
  - Hot reload is unaffected.
  - The visible change is that resources created in a stateless class method
    are owned by the caller (the migration lint below flags them).
- **Tooling developer.**
  - The rules live in one place each: the compiler picks candidates, and the
    runtime flag decides.
  - `beamtalk build` can report which sites are guarded-direct, extending the
    existing "dynamic dispatch (receiver type unknown)" diagnostic.

## Steelman Analysis

Option A is the decision above. Options B–E are the alternatives.

### B. Sealed is the contract (BT-4118's original framing)
- 🧑‍💻 **Newcomer:** "One rule: seal it to make it fast."
- 🎩 **Smalltalk purist:** "Dynamic stays dynamic unless I explicitly give it
  up. Optimisation should be opt-in."
- ⚙️ **BEAM veteran:** "Sealed classes can never gain subclasses, so a
  sealed direct call needs no subclass reasoning."
- 🏭 **Operator:** "A smaller blast radius: only classes someone sealed
  change process placement."
- 🎨 **Language designer:** "The keyword that already exists carries the
  guarantee, with no new concepts."

*Why not:*
- The instance side needs the guard anyway, because annotations can lie.
  With the guard, `sealed` adds nothing to soundness and only shrinks
  coverage (1b, 4c and 5b stay slow).
- On the class side it ties *where code runs* to a *subclassing* decision.
  Sealing a class for API reasons would silently move its class methods
  between processes.
- Framework base classes (`Handler`, `Server`) are open by design, so the
  code that needs the speed would never get it.

### C. Run-time inline caches
- 🎩 **Smalltalk purist:** "This is how Self and Cog do it, and it needs no
  types."
- 🧑‍💻 **Newcomer:** "Untyped and REPL code get faster too."

*Why not:* BEAM has no cheap mutable per-call-site slot. A `persistent_term`
per site means global GC on every update, and the process dictionary is
per-process and leaks. Static candidates plus a guard reach the same shape
with no mutable state.

### D. Copy-down / consolidation of inherited methods
- 🏭 **Operator:** "No guard and no delegation hop in steady state."
- ⚙️ **BEAM veteran:** "It's Elixir protocol consolidation."

*Why not:*
- BT-4109's owner memo and the Value `SuperMod:dispatch/3` delegation leave
  about 4 reductions per inheritance level.
- Copy-down contradicts ADR 0127 ("inherited methods are never copied"),
  ADR 0112 (`removeSelector:` re-exposes the inherited method with nothing to
  invalidate) and ADR 0066 (extensions override inherited methods).
- It also needs a recompile fan-out on every superclass edit.

### E. Class references as load-time constants (pid baked in)
- 🏭 **Operator:** "Zero cost per use."

*Why not:* a class process restarts, and a REPL redefinition can move a class
to a new module. A baked pid would mean different things before and after,
breaking principles §4. One `whereis/1` on a compile-time atom costs almost
nothing.

### Tension points
- **Purists vs. pragmatists on §3.** Purists may want class methods to stay
  "inside the class object". Pharo, Erlang and the newcomer intuition all
  agree they run in the caller. The `class-process-ffi` lint is the existing
  evidence that the class process surprises users more than it helps them.
- **Operators are split.** They gain no node-wide queue and no timeouts, but
  lose implicit ownership of resources created in class methods. The
  migration lint and CHANGELOG entry cover the loss.

## Alternatives Considered

### Sealed is the contract
See the steelman above. Rejected: the guard is needed regardless, so
coupling to `sealed` buys only lost coverage and a confusing class-side rule.

### Only sealed, stateless classes run class methods in the caller
This is the smaller version of §3, considered with the user.
- It removes the need for a subclass guard, since sealed classes have none.
- It keeps open-class class methods at ~145 red behind the node-wide queue.
- It makes `sealed` decide process placement.

Rejected in favour of tying placement to the one thing the class process is
for: class state (ADR 0130).

### Run-time inline caches
Rejected: no cheap mutable per-site storage on BEAM (see steelman C).

### Copy-down of inherited methods
Rejected on cost and benefit: about 4 reductions per level saved, against
conflicts with ADRs 0066, 0112 and 0127 and a reload fan-out (see steelman
D). Revisit only if a profile of real programs shows inheritance hops above
5% of a workload.

### Load-time class constants
Rejected: pids and REPL module names are not constants (see steelman E).

### Unguarded trust in compile-time verdicts (today's ADR 0129 static path)
Rejected: it is the source of BT-4126, BT-4133, BT-4134 and BT-4037. A
`persistent_term` read is about one reduction.

## Consequences

### Positive
- Typed sends to any tagged-map class, open or sealed, cost about what row 1a
  costs today (~3 red / 0 w, from 29 / 28). Self-sends in open Values follow.
- Class methods of stateless classes cost a flag read plus a remote call,
  instead of ~145 red / ~450 w and a node-wide queue (rows 3b, 3c).
  `Exception signal:` (BT-4115) and `Result ok:` follow from the general
  rule.
- Caller-sensitive FFI in class methods (`gen_tcp:accept`,
  `controlling_process`, `self()`) works. The `class-process-ffi` lint's
  original complaint disappears for stateless classes.
- Re-entrant class methods (a block that sends to the same class) stop
  raising `dispatch_error` for stateless classes.
- One dispatch rule replaces three: the BT-4069 sealed gate, ADR 0129's
  sealed + `class sealed` gate, and BT-3700's corner. The unguarded static
  verdicts behind BT-4126, BT-4133, BT-4134 and BT-4037 go away.
- Method-level `class sealed` no longer matters for dispatch, which removes
  the last blocker on ADR 0049 (removing method-level `sealed`).

### Negative
- **Semantic change for stateless class methods.**
  - Resources created through FFI (ETS tables, ports, timers, links,
    monitors) now belong to the caller and die with it.
  - Implicit serialisation through the class process is gone.
  - A hung class method now blocks its caller instead of timing out a
    `gen_server:call`.
  - An `exit` inside the method exits the caller.
- **Blocks passed from an actor into a stateless class method now run at
  home** (ADR 0137). They see and write live state, so findings from the
  `actor-state-abroad` lint change.
- **Every class-side direct call pays a guard.** It costs one
  `persistent_term` read, where ADR 0129's static calls paid nothing (row 3a
  moves from 2 red to ~3).
- Per-class load is no longer visible as a class-process mailbox in
  observer or `Tracing`.
- Code grows slightly at typed send sites: the guard plus the fallback.

### Neutral
- `sealed` keeps its meaning ("no user subclasses"). It now only *helps* the
  type checker pick candidates (an exact type is never a union) and no
  longer gates dispatch.
- ADR 0025's "identical bytecode" claim is reworded to "identical results".
- Primitive receivers (`NON_TAGGED_MAP_CLASSES`) are unchanged. A future ADR
  could apply the same guard idea with `is_integer`/`is_binary` guards.

## Implementation

All phases sit under epic BT-4112. Each phase adds a vs-elixir row or gate
before the change and records before/after (`just bench-vs-elixir`,
deterministic reductions and words).

**Phase 0 — harness (S).**
- Generalise BT-4150's differential test. A fixture runs each send shape,
  guarded-direct versus `beamtalk_message_dispatch:send/3`, over: plain and
  keyword sends, `perform:`, DNU, extensions (including one overriding an
  inherited method), `removeSelector:`, a live patch, a subclass receiver, a
  lying annotation, `perform:withArguments:timeout:` (rewritten in `send/3`
  before classification), an inherited class method touching a stateful
  parent's class variables (§3: its error must not change), and an extension
  or runtime fun on an *ancestor* of the receiving class.
- Results *and* error shapes must be identical.
- Add vs-elixir rows for an open-Value typed send, an open-class class
  method, and a class-reference loop.

**Phase 1 — instance side (S–M), codegen.**
- Drop `is_sealed` from `typed_sealed_value_target` and rename it
  (`typed_tagged_target`).
- Make sure Value self-sends in open classes reach it.
- Keep all existing exclusions, and land BT-4151's ambiguity check first or
  together.
- Files: `dispatch_codegen.rs`, `tests/typed_sealed.rs`.
- Gate: rows 1b and 4c within 10% of 1a; 5b reduced by at least 25 red.

**Phase 2 — class side, runtime (M).**
- Add "has class variables" to the inputs of
  `beamtalk_class_shadow_flags:direct_call_ok/2`, erased on class-variable
  changes by the same per-tag lock.
- `class_send_plain/3` runs the method in the caller when the flag holds,
  resolving the owner through a `{class_method_owner, C, Sel}` memo in
  `beamtalk_class_derived_cache`.
- The memo also checks that the owner has no class variables.
- `set_runtime_class_methods/2` invalidates the derived cache.
- The process-bound selector list arrives as metadata.
- Pass the real class reference as `ClassSelf`.
- `File open:mode:`'s ownership rule resolves to `self()` without the
  mirrored caller pid.
- Files: `beamtalk_class_dispatch.erl`, `beamtalk_class_shadow_flags.erl`,
  `beamtalk_class_derived_cache.erl`, `beamtalk_class_metadata.erl`,
  `beamtalk_object_class.erl`, `beamtalk_file.erl`.
- Subsumes BT-4073. Coordinate with BT-4115, whose part A becomes a special
  case.
- Gate: rows 3b and 3c ≤ 10 red, with no `'$gen_call'` in the profile.

**Phase 3 — class side, codegen + lint (M).**
- Literal-receiver and class-side self-sends to own selectors emit the
  guarded direct call. ADR 0129 static calls get the same guard.
- Retire the `nil` `ClassSelf` convention and its prologue.
- Invert the `class-process-ffi` lint into `class-method-process-resource`.
  It warns on FFI in a stateless class method that creates a process-owned
  resource (`ets:new`, `open_port`, `spawn_link`, `link`, `monitor`,
  `register`, timers) or relies on `self()`/the process dictionary. Name the
  fix in the message: an Actor or `classState:`. Reuse the validator and its
  curated table.
- Update `is_direct_call_eligible` (`class_info.rs`, shared with the
  `class-state-abroad` lint) to the new rule. There is still one
  implementation.
- REPL codegen uses the same rule (`crates/beamtalk-repl/src/codegen.rs`,
  `set_direct_call_eligible_from_hierarchy`).

**Phase 4 — class references (S).**
- Use the compile-time registered-name atom and `whereis/1`, plus the
  existing `module_name_safe/1` row. Measure it against a `persistent_term`
  before switching (§4).
- Pass the bare name where only the name is used.
- Gate: rows 8a and 12b.

**Phase 5 — docs (S).**
- Amend these ADRs:
  - 0025 ("identical bytecode" becomes "identical results");
  - 0129 (constraint 1);
  - 0013 §4;
  - 0010 (stale "zero-overhead class sends");
  - 0137 (blocks in class methods);
  - 0130 (§5's "class sealed, stateless" wording; the in-caller rule adds no
    class-variable home);
  - 0109 (§3 generalises its block-scoped `open:do:` exception);
  - 0084 (runtime funs are applied in the caller when the class has no class
    variables).
- Update `docs/beamtalk-language-features.md`: *Class-Side Methods*,
  *Passing Blocks Through Class Methods* and *Self-Sends and Overrides*.
- Update CLAUDE.md's "Blocks into class methods" rule.
- Add a CHANGELOG migration note.

**Conformance.**
- The compiler only names candidates: a module for the instance side, an
  own-selector function for the class side.
- The runtime flag and the guard decide. No eligibility rule exists in both
  languages.
- The instance-side routing knowledge that crosses the boundary stays pinned
  by `tagged_map_routing_conformance.json`, and Phase 0's differential test
  pins behaviour.

## Migration Path

Only §3 changes behaviour, and only for class methods of classes **without**
class variables.

- **Resources owned by the class process.** Code that creates an ETS table,
  port, timer, link or monitor in such a class method, and expects it to
  outlive the caller, must move ownership:
  - into an Actor (preferred: `Actor subclass: TableOwner` that creates the
    resource in `initialize`), or
  - into a class with `classState:`, which keeps its process.
  - The `class-method-process-resource` lint flags direct FFI cases.
- **Implicit serialisation.** Read-modify-write sequences in class methods
  that relied on queuing must go through an Actor. There is no lint for this,
  so the CHANGELOG calls it out.
- **Suppressions.** `@expect class_process_ffi` suppressions in stateless
  classes become unnecessary. The lint reports them as unused, per the
  existing `@expect` rules.
- **Erlang callers of `class_send`** see the same results, now computed in
  their own process.

## References
- Related issues: BT-4118 (this ADR), BT-4112 (epic), BT-4069, BT-4099,
  BT-4109, BT-4065, BT-4073, BT-4113, BT-4115, BT-4117, BT-4126, BT-4127,
  BT-4133, BT-4134, BT-4037, BT-4149, BT-4150, BT-4151, BT-3700
- Related ADRs: 0005, 0006, 0010, 0013, 0025, 0032, 0049, 0066, 0084, 0100,
  0109, 0112, 0119, 0127, 0129, 0130, 0137
- Documentation: `benchmarks/vs-elixir/README.md`,
  `docs/development/benchmarks.md` (BT-3676/3690/3700),
  `docs/beamtalk-principles.md` §2, §4, §11
