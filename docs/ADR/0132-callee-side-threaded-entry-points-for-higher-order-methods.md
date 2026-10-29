# ADR 0132: Callee-Side Threaded Entry Points for Higher-Order Methods

## Status
Superseded by [ADR 0133](0133-scope-of-captured-variable-mutation-in-blocks.md) (2026-10-18); Accepted 2026-10-17; Proposed 2026-10-16

ADR 0133 decides that a block which escapes (stored, returned, passed to a
user-defined method or sent to another process) may read captured variables
but not assign them, so no callee-side threaded entry is needed. Phases 0a
to 1c shipped (BT-3866 to BT-3871); nothing in Beamtalk called a threaded
entry before this ADR was superseded, and ADR 0133 Phase 2 removes them.

## Implementation Tracking

**Epic (Phases 0 and 1):** [BT-3865](https://linear.app/beamtalk/issue/BT-3865)
**Status:** Planned. Phases 2 to 6 are not scheduled: Open Question 5 asks for a count of real §6 hits first.

| Phase | Issue | Description | Size | PR |
|---|---|---|---|---|
| 0a | [BT-3866](https://linear.app/beamtalk/issue/BT-3866) | Pin the §6 inventory (I1–I4, I6–I8) as `PIN-BUG` BUnit and repl-protocol cases | S | |
| 0b | [BT-3867](https://linear.app/beamtalk/issue/BT-3867) | I5: a stored Tier 2 block sent `value` in a class or value-type method as an inline producer | M | |
| 0c | [BT-3868](https://linear.app/beamtalk/issue/BT-3868) | Runtime floor (`send_threaded/4`, `call_threaded/3`), metadata keys, `MethodInfo::threading_params`, wire check, baselines | M | |
| 1a | [BT-3869](https://linear.app/beamtalk/issue/BT-3869) | `VersionPrefix::Acc`, `ScopeKind::Threaded`, the `MethodBody` builder, four verifier checks | M | |
| 1b | [BT-3870](https://linear.app/beamtalk/issue/BT-3870) | Generalize `callee_threads_block_param` (greatest fixpoint); populate `threading_params` | M | |
| 1c | [BT-3871](https://linear.app/beamtalk/issue/BT-3871) | Threaded entry for class methods | M | [#4361](https://github.com/jamesc/beamtalk/pull/4361) |
| 1d | [BT-3872](https://linear.app/beamtalk/issue/BT-3872) | Threaded entry for value-type methods | M | |
| 1e | [BT-3873](https://linear.app/beamtalk/issue/BT-3873) | Caller-side `callee_threaded_send` producer; §6 narrowed for exact receivers; actor locals-only rule | M | |
| 1f | [BT-3874](https://linear.app/beamtalk/issue/BT-3874) | Corpus with oracle, pin flips, repl-protocol case, measurement, docs | M | |

Dependency order: {3866, 3867, 3868, 3869} → 3870 (after 3868) → {3871, 3872} → 3873 → 3874.

Two file references in the text below are corrected by this tracking entry: `call_threaded/3` lives in `beamtalk_dispatch.erl` (there is no `beamtalk_block.erl`), and the generated builtins file is `crates/beamtalk-core/src/semantic_analysis/class_hierarchy/generated_builtins.rs`.

**Deviation from §2 (BT-3871 review, #4361):** the plain `class_<sel>` wrapper does not hand the body an empty map. With `~{}~` a Tier 2 block reaching the plain entry from a late-bound send ran through `call_threaded/3` and its writes were dropped silently, which §4 forbids. The wrapper passes a sentinel as `Acc@0` (the atom `plain` in #4361; `{plain, ClassTag, Selector}` since BT-3885, below); `call_threaded/3` with the sentinel applies an arity-N fun as before, raises `stateful_block_dispatch` for a Tier 2 fun without running it, and sends `value` to a non-fun callable. The sentinel is threaded through `Acc` unchanged and never used as a map.

**Deviation from §2's tier gate (BT-3885):** the gate below said `is_function(B, n+1)` applies a Tier 2 fun with `Acc`. Arity alone cannot tell a Tier 2 fun from a plain block that takes one argument more than it is called with, so with a real `Acc` map that gate had to *run* such a block (its side effects happened, with the caller's `Acc` bound to its last parameter) before its reply showed the mistake, and a reply that happened to be `{_, Map}` (an Erlang `{ok, #{...}}`) was taken for a Tier 2 reply, the foreign map becoming the caller's `Acc`. Tier 2 funs are now **marked at creation**: codegen mints every Tier 2 block as `letrec 'BtTier2Block'/N+1 = fun (..., StateAcc) -> ... in 'BtTier2Block'/N+1` (`generate_block_stateful_fun`, the one place Tier 2 funs are created), the Erlang compiler names the lifted function `-F/A-BtTier2Block/N+1-K-`, and `beamtalk_dispatch:is_tier2_block/1` reads that from `erlang:fun_info(F, name)`. The gate decides the tier **before applying anything**: an arity-N+1 fun runs with `Acc` only if it is marked; unmarked, it raises `arity_mismatch` unrun, whatever `Acc` is. The mark and the plain-entry sentinel have one source, `crates/beamtalk-codegen/src/core_erlang/threading_abi.rs`, rendered into the generated `runtime/apps/beamtalk_runtime/include/beamtalk_threading_abi.hrl` (`just check-generated-builtins`, `checked_in_header_matches_the_generator`), and an EUnit test checks a block codegen compiled against the runtime's check end to end. The sentinel carries the class method's name so the refusal says which method refused the block (`'twice:' on Twice class cannot thread a block that assigns captured variables through this send; the block was not run`) instead of the structural fallback's wording about `perform:` and `value`.

**Deviation from §2's collection-HOM row (review of #4361):** the ADR 0128 fold over `Acc` runs for a **list** receiver only. The plain `class_<sel>` wrapper must answer exactly what the structural lowering answered before this ADR, and that lowering guarded the receiver with `is_list` and sent a non-list receiver the collection message itself; folding every receiver through `beamtalk_collection:to_list` changed what a `Dictionary collect:` answers (a list, not a `Dictionary`), made a `Stream collect:` eager and an infinite `Stream detect:ifNone:` hang. So the threaded body emits `case is_list(Recv) of true -> <fold over Acc> ; false -> {Recv sel: ..., Acc} end`: a non-list receiver takes the plain send with `Acc` unchanged (its callee runs the block as a plain callable, so a Tier 2 block raises its structured error there, never silently). The `ifNone:` handler of `detect:ifNone:` is invoked through `call_threaded/3` like every other callable (a non-block handler gets its `value` send, not a raw `apply`). One error kind does change on the list path: a plain block of the wrong arity handed to a collection HOM of a threading class method now raises `stateful_block_dispatch` from `call_threaded/3` (a block of arity N+1 is indistinguishable from a Tier 2 block by arity; BT-3885 tracks a dedicated message) where `lists:map`'s `badarity` used to surface as `argument_error`; a Symbol callable raises `does_not_understand` where it raised `internal_error`. Two agreement rules were also tightened on the same review: a mention of a threading parameter in a conditional / `and:` / `or:` send's *argument* is not a root use (the index leaves the parameter plain, as codegen's sequencing rule, which walks only such a send's receiver, requires), and a use's own operands are root operand sites (a use of a second threading parameter inside the first's argument, `a value: (b value)`, is sequenced ahead of it; BT-3890).

**Deviation from §2's NLR row (BT-3871 review, #4361):** Phase 1c ships no NLR-carrying threaded body. A `^` throws the `Acc` version current where its block is lowered, and a threaded callee can run the block after a later `Acc` step (`b value: [:x | ^x]`, a stored block, a block a callee keeps), so the `{Val, AccAtThrow}` catch would drop the caller's writes. The threading index therefore gives a method whose body holds any block that does `^` no threading parameters (`facts::body_has_block_nlr`, over-approximate); it keeps its plain entry and compiles as before. The verifier's `NlrInThreadedBody` refuses an NLR node in a threaded body; `NlrSlotShapeMismatch` keeps the slot shape Phase 3 will need. Carrying the live `Acc` to the throw is Phase 3. Stdlib effect: `Collection>>detect:ifNone:` loses its (instance-side) threading parameter.

## Context

### Problem statement

ADR 0131 made an outer-local write inside a *compiler-known* construct (a
loop, a fold, a conditional, `on:do:`, `Result tryDo:`, a `Dictionary`
lookup handler, an evaluated literal block) a state effect the construct
hands back to its frame. It deliberately stopped at the one boundary the
compiler does not control: a block passed to a **user-defined higher-order
method** (HOM). Its §6 makes that a compile error outside an actor instance
method, because a class method or a value-type method has no channel through
which a callee could return the block's writes:

```beamtalk
sealed Object subclass: Twice
  class sealed twice: aBlock =>
    aBlock value
    aBlock value

Object subclass: Caller
  class run =>
    t := 0
    Twice twice: [t := t + 1]      // error: `twice:` cannot return the write
    t
```

```text
error: block writes outer local `t`, but `twice:` cannot return the write
  ┌─ caller.bt:8:18
8 │     Twice twice: [t := t + 1]
  │                  ^^^^^^^^^^^^ `t` is written here
  = help: return the new value from the block and assign it: `t := Twice twice: [t + 1]`,
          or use a control-flow message (`do:`, `inject:into:`, `on:do:`) that threads locals
```

In Pharo this program answers 2. ADR 0041 promised the same ("local
mutations work in all blocks"). Today it is an error in a class method and
a value-type method, and in an actor instance method it works only for the
narrow set of callees BT-3755 could prove safe: a `sealed` method of the
*same* class that invokes the block directly at the top level of its body,
as the callable of a collection message, or passes it on to another such
method. A callee that invokes the block inside a `do:` body, a conditional
arm, an operand, or a nested closure, an inherited callee, an overridable
one, and every callee reached through any receiver but a bare `self` are
the same error (`stdlib/test/fixtures/adr0132section6_actor.bt.pending`
pins the four actor shapes; the class and value-type shapes are the ADR
0131 §6 error at every send).

ADR 0131 named this gap its Alternative D and kept it out of scope: "it
changes the calling convention of every block-taking method, including
stdlib natives and Erlang FFI, and needs a migration of compiled code (hot
reload of mixed old/new modules). That is its own ADR." This is that ADR.

### Current state

**How a Tier 2 block reaches a callee today.** ADR 0041 compiles a block
that writes an outer local as a Tier 2 closure, `fun (Args..., StateAcc) ->
{Result, StateAcc1}`, with the written locals under `'__local__t'` keys of
the map. Three things can then happen at the callee:

| Callee | What happens to the block's write |
|---|---|
| A §1 construct (`do:`, `inject:into:`, `on:do:`, …) | Inlined by codegen; the write is a `LocalRebind` prelude (ADR 0131). Correct everywhere. |
| An actor instance method of the same class, `sealed`, reached by a bare `self` send, using the block only in the BT-3755 shapes | `generate_tier2_self_send_open` packs the locals into the actor `State` before the self-dispatch, the callee is compiled with that parameter as a Tier 2 parameter (`tier2_method_info`, a per-class pre-scan), its `b value` is `apply B(State)` threading the gen_server state, and the caller reads the `'__local__'` keys back out of the returned `State`. Correct, verified, but the channel *is* the actor state map, and the caller and callee must be compiled together. |
| Anything else | The callee's `b value` is the structural fallback (`intrinsics.rs`): `is_function(B, 0)` applies it, else `is_function(B, 1)` raises `#beamtalk_error{kind = stateful_block_dispatch}`. A callee that forwards the block to `do:`/`collect:`/`select:` runs the ADR 0128 fold with a frozen state and **drops the write** (ADR 0128 §"Explicitly narrowed"). ADR 0131 §6 turns both into the compile error above wherever it can see the shape. |

Note the last row: **a user HOM's plain function already pays an
`is_function/2` guard on every block invocation.** There is no zero-overhead
Tier 1 path through a method body; that path exists only for the inlined
stdlib constructs, which never enter a method body at all. This is what
decides §2's "one body" below.

**Calling conventions and modules.** Every class compiles to its own BEAM
module (one class per file, ADR 0040; `compiled_module_name` in
`driver.rs`), reloaded one at a time (`Counter reload`, ADR 0082
`compile:source:`, REPL redefinition). The ADR 0130 convention for a class
method is `class_<sel>(ClassSelf, A1, …, An) -> Result`, bare, with class
variables written in place in the class process. A value-type method is
`'<sel>'/n+1 = fun (Self, A1, …, An) -> Result`, reached through
`'dispatch'/3 = fun (Selector, Args, Self)` (ADR 0006). A `sealed` actor
method is `'__sealed_<sel>'(A1, …, An, Self, State) -> {'reply', Result,
State1}`. **Self-sends are late-bound in all three kinds**: an actor
self-send goes through `safe_dispatch` on the instance's own
`'__class_mod__'` (BT-3666), a class-side self-send through
`class_self_send`, a value-type instance send through `dispatch/3`; only a
`sealed` class or method is statically bound. `super` always targets another
module. A direct-called class method (ADR 0129 Phase 0b: a sealed class with
no `classState:` and a `class sealed` method, `ClassInfo::is_direct_call_eligible`)
is listed in the module's `'__beamtalk_meta'/0` as `direct_class_methods =>
#{Selector => SafeFn}`, and `beamtalk_class_dispatch:class_send/3` reads that
map (cached at class registration, refreshed on reload) to call
`Module:SafeFn(nil, Args...)` in the caller's process. **Eligibility is
computed once in Rust from the cross-file `ClassHierarchy` and shipped as
generated metadata; the runtime has no second implementation.** That pattern
is reused below. `dispatch/3` and `handle_class_method_call` resolve a
selector by walking the hierarchy and checking the ADR 0066 extension
registry at each level before the class's own methods; ADR 0084
runtime-installed funs take part in the same walk.

**One threaded entry already exists.** ADR 0131 §5 gave
`beamtalk_result:'tryDo:'` an arity-2 clause, `'tryDo:'(Block, StateAcc)
when is_function(Block, 1)`, answering `{Result, StateAcc1}`, sharing
`protect/1` and the catch sequence with the arity-1 clause, with a Rust ↔
Erlang conformance fixture. Codegen chooses the arity from the block
literal. That is a per-method callee-side `{Result, StateAcc}` protocol for
exactly one native. This ADR generalizes it.

**Size of the surface.** 837 method headers in `stdlib/src/*.bt`, of which
130 name a block-like parameter and 40 of those are `@primitive` or
`@intrinsic` (Erlang-implemented or codegen-lowered). The rest are ordinary
Beamtalk HOM bodies, and most of them forward the block to a §1 construct.

### The inventory

ADR 0131 §6 and BT-3742 define the inventory as "the set of call sites §6
rejects". Enumerated from the §6 validator (`validators/local_threading.rs`
`check_send`), its fixtures, and ADR 0131's close-out:

| # | Shape (the block writes outer local `t`) | Context | Today | This ADR |
|---|---|---|---|---|
| I1 | `X hom: [t := t + 1]`, `X` a literal class, user HOM invoking its parameter directly (`b value`, `b value: x`, a `value`-family cascade) as a statement, an assignment RHS or a `^` value | class, value-type | §6 error | **Phase 1** (static or index-known callee), **Phase 2** (unknown receiver) |
| I2 | as I1, callee invokes the parameter inside a `do:` body, a conditional arm, an operand, an `on:do:`/`ensure:` block, or a nested closure (`f := [b value]. f value`) | all three (BT-3755's four actor probes) | §6 error | **Phase 3** |
| I3 | as I1, callee inherited from a superclass, or overridable (class or method not `sealed`) | actor (self-send) | §6 error | **Phase 4** (late-bound self-dispatch, threaded) |
| I4 | callee passes the parameter on, bare, to another HOM, including `super` | all | actor only, same-class chain | **Phase 1** (same module), **Phase 2** (cross-module, `super`) |
| I5 | stored Tier 2 block sent `value` directly, no callee (`b := [t := t + 1]. b value. t`; ADR 0131 probe s4) | class, value-type | §6 error | **Phase 0** (an ADR 0131 inline `value` producer over a fresh map; needs nothing from this ADR and ships first) |
| I6 | stored Tier 2 block as the callable of a collection message (`items do: blk`) | class, value-type, REPL, and actor (ADR 0128 §"Explicitly narrowed") | §6 error | **Phase 2** (the ADR 0128 fold seeded from the pack, §3) |
| I7 | class-side `self hom: [t := t + 1]` in a class method | class | §6 error | **Phase 1** (sealed), **Phase 4** (late-bound) |
| I8 | a Tier 2 block passed to a class method of a class with `classState:` or not `class sealed` (runs in the class process, ADR 0109) | any | §6 error | **Phase 4** |
| I9 | an Erlang-implemented (native) stdlib HOM that invokes the block | any | §6 error, or `stateful_block_dispatch` | **Phase 5**, per native, opt-in; default a structured error. `Block`'s own intrinsics with a non-literal receiver or argument (`aBlock ensure: […]`, `c ifTrue: aBlock`, `d at: k ifAbsent: aBlock`, `aBlock whileTrue`) are codegen-lowered and are **Phase 3** |
| I10 | an Erlang FFI argument (`(Erlang lists) map: [...] with: ...`) | any | lossy, warned | unchanged (ADR 0041 §Erlang Interop Boundary) |
| I11 | the block escapes the method: `^[…]`, last expression, field or class-variable store, collection literal (BT-3756) | any | §6 error | unchanged, **permanent** |
| I12 | `at:ifAbsentPut:` / `removeKey:ifAbsent:` on a `Dictionary` | any | §6 error | unchanged for `Dictionary` (immutable, no behaviour to lower); a user class defining either selector is an ordinary HOM, I1 |
| I13 | cascade part, string interpolation, `match:` subject | any | §6 error (ADR 0131 close-out) | unchanged (no statement boundary for a prelude) |
| I14 | a keyword `whileTrue:`/`whileFalse:` condition block (BT-3782) | class | §6 error | unchanged (BT-3743) |
| I15 | a runtime-installed class method fun (ADR 0084 `put_class_method/4`) | class | n/a | no entry; structured error at run time (Open Question 2) |
| I16 | an ADR 0127 trait provision routing the block through `self do:` | value-type, actor | value-type works (`self_send_hom_sackful.bt`); actor only through the BT-912 channel | value-type unchanged; actor via **Phase 4** |
| I17 | a Tier 2 block passed to an asynchronous send (`!`, a `spawns_block` method, `Future`, `Timer`) | any | §6 error or a dropped write | §6 error, **permanent**: the block runs after the caller's frame is gone (I11's rule at run time) |

### Constraints

- ADR 0041: the Tier 2 closure shape `fun (Args..., StateAcc) -> {Result,
  StateAcc1}` stays. Locals stay immutable Core Erlang variables. No process
  dictionary cell, no mutable reference (ADR 0041 Alternative A, ADR 0128
  Option 1, both rejected and not reopened here).
- ADR 0111 / CLAUDE.md: a state-threading change states its invariant for
  every scope kind, is owned by the `ThreadedIr` node that owns the scope,
  adds the `VerifyError` that fails on the repro first, and is proved over a
  generated corpus against an execution oracle. No per-call-site sync,
  refresh or commit.
- ADR 0122: families are data (`ThreadedFamilies`, `eligible_families`),
  with per-site capability (Decision 4). The only trailing family today,
  `SelfVt`, is still rejected at `Letrec` bodies, `on:do:`/`ensure:` arms,
  bare and stored blocks, conditional arms that hold a fold, and every fold
  but `do:`, `collect:` and `inject:into:` (ADR 0122 BT-3827 addendum;
  BT-3854 open). A second trailing family inherits that map of gaps.
- ADR 0129 Phase 0b: a `class sealed` method of a sealed stateless class is
  direct-called in the caller; its eligibility is Rust-computed metadata.
- ADR 0130: class variables are not a threading family. A block's
  class-variable writes are live only at home; abroad they raise
  `class_state_unreachable`. Nothing here changes that.
- ADR 0131: the `LocalRebind`/`ConstructTuple`/`BranchArm`/`MethodBody`
  nodes, the `threaded_locals_of` recognizer and the §2 per-frame lowering
  rule are the only way an outer-local write reaches its frame. Its Phase 0
  position diagnostic (`DENY_SET`/`ALLOW_SET`) is still in force (BT-3841);
  552 of 733 measured statement shapes answer wrong with it bypassed.
- Late binding: a `self` send that is late-bound today must stay late-bound
  whatever block it carries. A send that reaches a subclass override when
  the block is pure and the lexical method when the block writes a local is
  the BT-3666 → BT-3696 failure class (`docs/agents/expanded.md` § A
  state-threading fix is general or it is not a fix).
- Hot code reload: a module compiled before this ADR and one compiled after
  it coexist in one node, and each class reloads alone. Neither direction
  may crash or silently drop a write.
- ADR 0111's ≤3 % build-time gate (user CPU, the ADR 0131 methodology) and
  ADR 0041's ~65 ns Tier 2 call budget, measured with
  `runtime/perf/self_send_bench` for the self-send path.

## Decision

**A method that invokes or forwards a block parameter is compiled as one
body with two entry points. The body is lowered as a `ThreadedIr`
`MethodBody` frame over a new trailing family, `Acc`, the opaque map a
caller hands over; every invocation of the parameter is gated per
invocation on the block's tier and threads `Acc` through every construct
the body contains. The plain entry (today's function name and arity) calls
the body with no map (the `plain` sentinel, see the deviation note in the
tracking section: an empty map would silently drop a Tier 2 block's
writes) and answers the bare result; the threaded entry
(`'__t2_<sel>'`, one arity higher) takes `Acc` and answers `{Result,
Acc1}`. Threaded entries are advertised in `'__beamtalk_meta'/0` and
resolved through the same late-bound dispatch walk a plain send uses. A
send that passes a Tier 2 block value is, on the caller's side, one more
ADR 0131 producer: it packs the block's locals, calls the threaded entry,
and rebinds from the returned map. Which methods have a threaded entry is
decided by one predicate in `beamtalk-core`, read by the §6 diagnostic, by
codegen, by the metadata emitter and by the cross-file class index.**

### 1. Which methods get a threaded entry

`semantic_analysis::tier2_callee::callee_threads_block_param(class, selector,
index)` is kept as the one predicate and **generalized**. Parameter `index`
of `selector` is a *threading parameter* when every use of it in the method
body is one of:

- a **direct invocation** (`b value`, `b value: x`, `b valueWithArguments:`,
  a `value`-family cascade), anywhere a `value` send can sit: a statement,
  an assignment RHS, a `^` value, an operand, a statement or value inside a
  conditional arm, a loop or fold body, an `on:do:`/`ensure:` block, a
  `Result tryDo:` block, or an evaluated literal block (ADR 0131's frames);
- the **callable argument of a collection message** (`items do: b`,
  `items inject: 0 into: b`, the ADR 0128 opaque-callable folds) or the
  non-literal receiver or argument of one of `Block`'s own intrinsics
  (`b ensure: […]`, `[…] on: E do: b`, `c ifTrue: b`, `d at: k ifAbsent: b`,
  `b whileTrue`), all of which codegen lowers;
- a **bare argument of another send**, to any receiver: `self`, `super`, a
  literal class, a variable. A forward to a callee that §4 resolves
  statically threads when that callee has a threaded entry for the
  position; a forward to an **unknown** receiver counts as threading and
  lowers to the runtime path inside the body (§4), which raises a structured
  error if the resolved callee cannot thread. The predicate is a greatest
  fixpoint: a cycle of forwards (`walk: b => b value: self. children do:
  [:c | c walk: b]`, or a method forwarding to itself) is assumed to
  thread, so recursive HOMs have an entry. Today's `visiting.contains → false`
  in `tier2_callee.rs` is the opposite and is changed;
- the receiver or argument of a **nested block literal** that is itself
  only used in these ways (`f := [b value]. f value`): a block that invokes
  or forwards a Tier 2 value is Tier 2 by containment (§2);
- `arity` or `respondsTo:` on the parameter, which in the body are lowered
  with the hidden `Acc` parameter subtracted (a Tier 2 fun's `fun_info`
  arity is one higher than its user-visible arity).

A parameter with any other use (stored in a field, a class variable or a
collection literal; returned; passed to an Erlang FFI call; passed to an
asynchronous send, a `spawns_block` method or any callee that runs the block
after the caller's frame is gone) is **not** a threading parameter, and the
method has no threaded entry for it. BT-3755's four shapes (top-level direct
invocation, the `last := b value: e` loop shape, the bare callable form,
pass-on) are the subset Phase 1 lights up; the rest is Phase 3.

A method has **one** threaded entry, with every threading parameter
accepted in either tier. There is no wrapper for a Tier 1 block passed
alongside a Tier 2 one: the per-invocation gate in §2 handles any mix, and
a non-fun callable (a `Symbol`, an object answering `value:`, ADR 0128's
opaque callables) goes through the gate's dynamic `value`-family send as it
does today.

The predicate's answer is **delivered, never re-derived**: codegen emits it
into `'__beamtalk_meta'/0` as

```erlang
'tier2_methods'       => #{ 'each:do:' => {'__t2_each:do:', [1]} },
'tier2_class_methods' => #{ 'twice:'   => {'__t2_class_twice:', [0]} }
```

(selector → entry function name and the threading positions; class-side
entries are named `'__t2_class_<sel>'` to mirror `class_<sel>` and keep a
class with both `twice:` and `class twice:` compiling), beside
`direct_class_methods`. The same answer is recorded as
`MethodInfo::threading_params` in the cross-file `ClassHierarchy` (the
precedent is `spawns_block`), filled for the stdlib in
`generated_builtins.rs` and for package classes from their metadata, so the
compiler can answer "does this known class's method thread" for a class in
another file. The runtime reads the metadata (§4); the §6 validator reads
the predicate through the index (§5); the override lint reads it (§6).
There is one source.

### 2. One body, two entry points

For a class method:

```erlang
%% The body, lowered once over Acc. ABlock is a threading parameter.
'__t2_class_twice:'/3 = fun (ClassSelf, ABlock, Acc0) ->
    case call 'beamtalk_dispatch':'call_threaded'(ABlock, [], Acc0) of
      <{_V1, Acc1}> when 'true' ->
        case call 'beamtalk_dispatch':'call_threaded'(ABlock, [], Acc1) of
          <{V2, Acc2}> when 'true' -> {V2, Acc2}
        end
    end

%% The plain entry: today's name and arity, unchanged for every caller.
'class_twice:'/2 = fun (ClassSelf, ABlock) ->
    call 'erlang':'element'(1, apply '__t2_class_twice:'/3 (ClassSelf, ABlock, ~{}~))
```

`beamtalk_dispatch:call_threaded(B, Args, Acc)` is the per-invocation tier
gate: `is_function(B, n)` applies a Tier 1 fun and answers `{Result, Acc}`
unchanged; `is_function(B, n+1)` applies a Tier 2 fun with `Acc`; anything
else is the dynamic `value`-family send (`beamtalk_dispatch`), answering
`{Result, Acc}`. (Amended by BT-3885, see Implementation Tracking: an arity
n+1 fun is Tier 2 only when it carries the Tier 2 mark codegen gives every
Tier 2 block, read before it is applied; any other fun of the wrong arity
raises `arity_mismatch` without running.) It replaces the structural fallback's `is_function(B, n)`
check for threading parameters, so the plain path pays what it pays today
plus one tuple. This is ADR 0128's per-element `is_function/2` gate, moved
from the fold into the one helper every invocation uses.

**Why one body and not two.** A second, Tier-1-only body would keep the
plain path free of the tuple. It would also lower every threading method
twice (build time), roughly double its code, and leave two bodies that must
agree. The plain path already pays an arity guard per invocation (Context),
so the second body buys one tuple allocation per call and nothing else.
Rejected (Alternatives). The `.beam` size and the plain-path cost are
measured in Phase 1 against today's structural fallback.

For a value-type method the entry is `'__t2_twice:'/3 = fun (Self, ABlock,
Acc0) -> {Result, AccN}` and `'dispatch'/3` keeps answering the plain
function. For an actor instance method (Phase 4), the body is one function
and the two entries are two `dispatch/4` clauses (and two `'__sealed_'`
functions for a `sealed` method): the plain clause replies `{'reply',
Result, State1}` as today, and the threaded clause replies `{'reply',
{Result, AccN}, State1}`, so the caller's map rides *inside the reply value*
and the gen_server protocol, `safe_dispatch` and the `{reply, _, _}`
3-tuple are untouched. The ADR 0131 §5 `tryDo:` twin is the native form of
the same shape: one implementation, two arities.

**`Acc` is a trailing family, never the scratch map.** ADR 0131 §2 defined
`ThreadedStmt::MethodBody { frame, threads, body }` and noted it has no
production builder. The threading body is its first: `MethodBody { frame:
Threaded, threads: [], families: [Acc, …] }`. `Acc` is a new storage family
(`VersionPrefix::Acc`, versions `Acc0`, `Acc1`, …) that travels as a
*trailing slot* of every construct's tuple, as `SelfVt` does, and is read
and written only by `call_threaded` and by forwards. It is **never** the
slot-2 scratch `StateAcc` map of a loop or fold, and `RebindShape::for_frame`
never seeds a `StateAcc` plan from it: the body's own locals go through ADR
0131's frames exactly as in any class or value-type method, under their
own `'__local__'` keys in their own scratch map, and no key of the body's
ever enters `Acc`. In scope kind `ScopeKind::Threaded`, `eligible_families`
answers `[Acc]` for a class body, `[SelfVt, Acc]` for a value-type body that
writes a field, and `[State, Acc]` for an actor body; the ADR 0131 ordering
rule (`State` extracted first) extends to `Acc` last. The verifier check is
`VerifyError::AccAsScratchSeed` (an `Acc` version used as a `StateAcc` seed
or `Put` target) and `OwnLocalKeyInAcc` (a `'__local__'` `Put` whose map is
an `Acc` version).

**What this inherits.** Because `Acc` is a trailing family, it has
`SelfVt`'s per-site capability map (Constraints): today a trailing family
is carried by `do:`, `collect:`, `inject:into:`, the conditionals and
`on:do:`/`ensure:` at the method root, and rejected elsewhere. Phase 3 is
the work of lifting those rejections for `Acc` at every site a threading
parameter can be invoked: `Letrec` bodies, the search and keyed folds
(`select:`, `reject:`, `detect:`, `count:`, `anySatisfy:`, …), conditional
arms that hold a fold, `on:do:`/`ensure:` arms, bare and stored blocks, and
the NLR boundary. It is sized L, and its proof is a per-site capability
corpus as BT-3827's was, not a list of fixed shapes. Until a site is lifted,
a threading parameter invoked there makes the method non-threading (the
predicate reads the same capability table, so the §6 error names the site).

The invariant for `Acc`, per scope kind, once a site is lifted:

| Scope kind inside the body | Invariant for `Acc` after it |
|---|---|
| top-level statement | `Acc_{n+1}` is the map after every gated invocation the statement made, in source order |
| conditional arm (`BranchArm`) | the arm's closer carries `Acc` out as a trailing slot; both arms rebind the same version |
| `Letrec` loop body (`whileTrue:`/`timesRepeat:`/`to:do:`) | `Acc` is a loop parameter; the recursive call passes the body's final version; the loop's result tuple carries it |
| `Foldl` loop body (`do:`/`collect:`/`inject:into:`/…) | `Acc` is a trailing slot of the fold accumulator, beside the fold's own `StateAcc` slot, never inside it |
| `on:do:`/`ensure:` arm | the protected body's final `Acc` on the non-raising path; the `Acc` on entry on the raising path (the ADR 0131 protected-region rule: a raise discards the region's writes, here the caller's writes made inside the region) |
| bare block / evaluated literal block | a block literal that invokes a threading parameter is Tier 2 by containment and takes and returns `Acc` as a trailing slot of its own map (below); an evaluated `[...] value` is the one-arm conditional of ADR 0131 (BT-3841 PR 6) |
| stored closure invoked later in the body (`f := [b value]. f value`) | `f` is Tier 2 by containment; `f value` is ADR 0131 §1's inline Tier 2 `value` producer, carrying `Acc` out as a trailing slot |
| NLR boundary, the body's own `^` | the `$bt_nlr` 4-tuple's state slot carries the body's family tuple with `Acc` last (`Acc` for a class body, `{SelfN, AccN}` for a value-type body, `{StateN, AccN}` for an actor body); the entry's catch unpacks that shape and answers `{Val, AccAtThrow}`, so the caller's writes made before the `^` are kept (ADR 0041 §State-Carrying NLR). Checked by `VerifyError::NlrSlotShapeMismatch` |
| NLR boundary, the *caller's* block's `^` crossing the body | the throw carries the caller's own slot (§3) and a foreign token; the body's catch re-raises it untouched, and the caller's writes made inside the construct are discarded, as ADR 0131 §2 says for every producer |

**Tier 2 by containment.** Today a block is Tier 2 when it writes an outer
local (`captured_mutations_for_block`). This ADR adds: a block literal that
invokes a threading parameter or a Tier 2 local, or forwards one in a
threading position, is Tier 2. Its own map is its own (its outer-local
writes under `'__local__'` keys, as today), and the `Acc` it carries for
the parameter it forwards is **nested under one reserved key**,
`'__acc__'`, never merged: `self each: [:x | b value: x. n := n + 1]`
compiles the block as `fun (X, S) -> … call_threaded(B, [X], maps:get('__acc__', S)) …`,
putting the returned `Acc` back under the same key, and the body's local
`n` under `'__local__n'` in `S`. A caller and a callee with a local of the
same name therefore never share a key, and a recursive HOM nests one level
per forward along the chain, as values, with nothing aliased. The rule lives
beside `captured_mutations_for_block` in `beamtalk-core`'s block facts, so
the §6 validator and codegen agree on it. Inside such a block, and inside a
body, `'__local__'` and `'__acc__'` reads use `maps:get/2`, not the
`maps:get/3` creation-time fallback of `generate_block_stateful_fun`, so a
crossed or missing key crashes with a `badkey` instead of silently reading
the wrong value; the corpus includes same-named caller/callee locals and
recursion.

**Why a new family and not `State`.** The REPL root already threads an
opaque map it does not name (the bindings), and it does so as the `State`
family. Reusing `State` would be the smallest change. It is rejected because
`State` carries actor meaning the body must not inherit: `make_self`,
`'field__'` keys, the BT-2717 stray-key hazard, and the
`ActorStateInClassMethod` verifier rule that ADR 0131 extended to value-type
methods precisely to keep `State` out of them.

### 3. The caller side is a producer

A send that passes a Tier 2 block value (a literal that writes an outer
local, or a Tier 2 local) to a non-§1 selector is lowered as an ADR 0131
**producer**, `callee_threaded_send`, in `local_threading_producer`'s case
list:

```text
prelude: [ Bind   { Acc0 ← ~{ '__local__t' => T }~ },             -- pack, one Put per local
           ConstructTuple { carrier: CT, doc: <threaded call>, threads: ["t"] },
           LocalRebind { local: "t", carrier: CT, slot: Key("__local__t") } ]
value:   element(1, CT)
```

The `LocalRebind`s are lowered by the enclosing frame under ADR 0131 §2's
table, unchanged, so the send works wherever ADR 0131 lets a producer sit.
ADR 0131 §1a's sibling-read rule applies: `t + (Twice twice: [t := t + 1])`
reads `t` before the send's rebind. **The producer is gated by the same
position allow-set as every other producer** (`ALLOW_SET`,
`validators/local_threading.rs`) until BT-3841 retires it: a position the
allow-set rejects for a loop is rejected for a threaded send too, so this
ADR adds no position that ADR 0131's measurement has not shown right.

The pack, per caller kind:

- **Class method, value-type method:** a fresh map seeded with the block's
  `threaded_locals_of` set. The block is compiled as it is today; its map
  *is* `Acc`.
- **Actor instance method:** a fresh map too. A Tier 2 block's `StateAcc`
  in an actor is today the actor `State` itself: field writes, self-sends
  and the NLR slot all go through it. A block bound for a threaded entry
  may run in another process and gets a map the actor's fields are not in,
  so it is compiled **locals-only**: it may write outer locals; a field
  write or a self-send inside it is a compile error (the §6 diagnostic,
  "the callee runs the block with the actor's state out of reach"), unless
  the send is one the existing BT-912 same-process channel accepts, which
  then applies unchanged. A `^` inside it throws the 4-tuple with the
  caller's `State` captured at the pack (the block cannot have changed it),
  so the actor method's NLR catch adopts the right state. Checked by
  `VerifyError::ActorStateInThreadedBlock`, with an actor corpus shape.
- **Value-type method with field writes (TestCase):** the same rule for
  `SelfVt`: a block bound for a threaded entry may not write a field; its
  `^` carries the captured `Self`.
- **REPL:** the pack is the bindings map itself, and the producer adopts
  element 2 whole as the next `State` version (ADR 0131 §2's `MethodBody
  (REPL)` row, BT-3795), so a binding the block introduces (`Twice twice:
  [z := 1]` with `z` unbound) is adopted rather than read back per key. The
  body never writes into `Acc`, so the bindings come back as the block
  left them.

**I5 and I6, which have no callee.** A stored Tier 2 block sent `value` in a
class or value-type method (I5) is ADR 0131 §1's inline Tier 2 `value`
producer over a fresh map and ships in Phase 0, independent of everything
else here. A stored Tier 2 block passed to a collection message (I6) is the
ADR 0128 opaque-callable fold with its `StateAcc` slot seeded from the pack
above instead of from the actor `State` (ADR 0131 Phase 2d's "its own
`__local__` seeding, designed then"): in class, value-type and REPL context
the slot is the fresh map or the bindings; in actor context it stays the
actor `State`, which is where the block's keys already are. The fold's
per-element `is_function/2` gate becomes `call_threaded`.

The existing BT-912 actor channel (`generate_tier2_self_send_open`,
`tier2_method_info`, packing into `State`) is kept: it is correct, verified,
same-process, and cheaper than a pack for the sends it accepts. The
producer above is used for every send it does not accept, so the two never
apply to the same send (the agreement test asserts it). Whether to retire
it after Phase 4 is Open Question 1, decided by `self_send_bench`.

### 4. Resolution: the plain send's binding, threaded

A threaded call binds **exactly as the plain send would**. There are four
paths, and the choice is the one codegen already makes for the plain send:

| The plain send is… | The threaded call is… |
|---|---|
| **Statically bound, same module** (a `self` send in a `sealed` class or to a `sealed` method, or a send to the enclosing class's own direct-called class method) | a direct call of the entry: `call 'mod':'__t2_class_twice:'(ClassSelf, B, Acc0)`. Same process, no lookup. Only ever same-module, because ADR 0040 makes every other class another module. |
| **Late-bound `self`** (actor `safe_dispatch` on `'__class_mod__'`; class-side `class_self_send`; value-type `dispatch/3`) | the **threaded variant of the same path**: `safe_dispatch_threaded`, `class_self_send_threaded`, a threaded `dispatch/3` clause, each resolving the receiver's actual class and then looking for the entry there. Never `send_threaded/4`: a class-side self-send through the general path would hit `handle_class_self_call`'s deadlock guard. |
| **`super`** | `super_threaded/5`, starting the walk at the superclass as the plain super send does (mirroring `class_super_dispatch/4`'s receiver-tag handling, BT-4001), so an override that forwards to `super` (`twice: b => self log. ^super twice: b`) never resolves back to itself. |
| **Anything else** (a literal class in another module, a variable, an argument, an inferred union, `perform:`) | `call 'beamtalk_dispatch':'send_threaded'(Receiver, 'twice:', [B], Acc0)`, answering `{Result, Acc1}`. |

`send_threaded/4` is **the same walk as `dispatch/3`** with one difference
at the end. It resolves the selector exactly as the plain send does
(extensions at each level, ADR 0084 runtime funs, the class's own methods,
then the superclass; `class_send/3`'s rules for a class receiver, including
the direct-call map and the gen_server path) and, at the implementation
that walk resolves to:

- a compiled module whose metadata lists an entry for the selector: calls
  it, guarded by `erlang:function_exported/3` against a module reloaded
  between the metadata cache and the call (ADR 0129's `lookup_direct_call`
  guard), through the class gen_server for a class method that is not
  direct-called (Phase 4: `class_method_call` gains a threaded variant whose
  reply is the `{Result, Acc1}` value);
- a compiled module with no entry, an ADR 0066 extension, an ADR 0084
  runtime fun, or a `doesNotUnderstand:` proxy that would forward the send:
  raises `#beamtalk_error{kind = stateful_block_dispatch}` naming the class
  and selector, with the hint "`twice:` on `Keeper` does not thread its
  block: return the new value from the block and assign it where it is
  called";
- nothing (the selector is not understood): `does_not_understand`, exactly
  as the plain send.

An **actor receiver** is the same walk: a plain actor send is a synchronous
`gen_server:call` (ADR 0006), so before Phase 4 the resolved actor module
has no threaded `dispatch/4` clause and `send_threaded/4` raises
`stateful_block_dispatch`; from Phase 4 it makes the same synchronous call
and unwraps the `{Result, Acc1}` carried inside the reply. The `{Result,
Acc}` shape is produced only by a threaded clause and consumed only by a
threaded call, so it is never handed back as an ordinary result. The
asynchronous forms (`!`, a `Future`-returning send) are I17: a Tier 2 block
in one is a compile error, and no threaded variant of them exists.

`perform:`-family sends are not a special case: `perform:` is resolved by
the walk to the performed selector as today (ADR 0006), user overrides of
`perform:` included, and the entry is looked up at whatever that resolves
to. The arity ambiguity `beamtalk_error.erl` records today ("wrong number
of arguments, or the block captures mutable state") disappears for this
error: the lookup is by metadata, not by fun arity, so the message says one
thing. `respondsTo:` and `selectors` answer the plain selector only; an
entry is not a selector, its name is not a valid one, and it is not part of
the ADR 0087 synthetic-method cross-reference.

**Compile time.** The §6 error for a send whose callee cannot thread is
reported only for an **exact** receiver: a literal class, `self` or `super`
in a `sealed` class, or an inferred type that is exactly one known class
(not a union, not a `:: Base` that a subclass may satisfy), looked up in
`MethodInfo::threading_params`. For every other receiver the send compiles
to the runtime path, and the compiler reports a `threading-resolved-at-run-time`
warning at the block, suppressible with `@expect
threading_resolved_at_run_time`, because the error such a send can still
raise is one the LSP cannot show. This is the ADR 0131 `Dictionary` lookup
rule (BT-3798) applied to callees: a compile error where the compiler can
see the answer, a runtime-guarded path where it cannot, and never a silent
drop. Every surface (CLI, REPL, LSP, MCP) answers the exact-receiver
question from the same `ClassHierarchy` index (`surface-parity.md` gains
the row).

**Mixed old and new modules under hot reload.** The plain entry keeps its
name, arity and behaviour, so an old caller of a new callee is unchanged. A
new caller of an old callee: a direct entry call is emitted only within one
module, and every other call goes through a path that reads the callee's
metadata and checks `function_exported/3`, so it finds no entry and raises
the structured error at the one send that needs threading, never an
`undef`. A newly compiled module on a runtime older than this ADR fails
with `undef beamtalk_dispatch:send_threaded/4` at its first threaded send:
that is the compiler/runtime version floor the `otp-support` release gate
and ADR 0131 §5's `tryDo:/2` precedent already enforce, and the stdlib is
built with its compiler. Load order follows ADR 0125's rule for upgrades,
receivers before senders: a callee module is loaded before the callers that
expect its entry. No `.appup`/relup change: the entry is an added export
and an added metadata key, neither of which touches process state, so
`beamtalk_hot_reload:code_change/3` has nothing to migrate. (ADR 0125 §3.4
is the wire-shape skew contract; module skew within one node is this
paragraph.)

### 5. What §6 becomes

ADR 0131 §6's rule, "a Tier 2 block value with no return channel is a
compile error", stands; what changes is the set with no channel. After this
ADR it is: I10 (FFI, warned, exempt), I11 and I17 (escapes and asynchronous
sends, permanent), I12 for a `Dictionary` receiver, I13 and I14, a send
whose exact receiver's method is known not to thread (§4), and, until the
site is lifted in Phase 3, a callee that invokes the parameter at a site
`Acc` cannot yet reach. A send to a non-exact receiver is **not** an error:
it compiles to the runtime path with the warning. **Phase 0 leaves §6
unchanged**: nothing is narrowed before the path that replaces it exists.
The §6 validator keeps reading `callee_threads_block_param`, now through the
index for every exact receiver rather than the enclosing actor only, and the
agreement test (every Tier 2 argument the §6 pass accepts lowers to a
producer, every one it rejects does not) extends to the new producer.

### 6. Overrides, inheritance, sealing

A threaded entry is resolved on the receiver's actual class by the plain
send's own binding, so an inherited HOM threads through its inherited entry,
and an override is reached like any override. The one new hazard is an
override that drops the entry: `Base twice:` threads, `Sub twice:` stores
its block, and `Sub twice: [t := t + 1]` through a `Base`-typed receiver
raises at run time where `Base` would have threaded. The compiler reports it
where it can see it: a method that overrides one with an entry for position
`i`, and has none, gets a `twin-dropped-in-override` warning naming the
superclass, suppressible with `@expect twin_dropped_in_override`. It is a
lint, not a guarantee, exactly as ADR 0130's `class-state-abroad` is.
Sealing decides only how a send is bound (§4's first row), as it does for a
plain send; BT-3755 required `sealed` because the actor channel needed the
callee compiled in the same pre-scan, and a metadata-resolved entry does
not.

### 7. Class variables, processes, and the hop

A Tier 2 block handed to a class method of a class with `classState:` or a
method that is not `class sealed` runs in that class's process (ADR 0109).
Its map is a value: it travels in the `class_method_call` message and back
in the reply, so **the outer-local write survives the hop**. The block's
class-variable rules are ADR 0130's and unchanged: a write to its home
class's variables from the foreign process raises `class_state_unreachable`,
a read answers the captured value. Both are documented together in
`beamtalk-language-features.md` § Passing Blocks Through Class Methods. A
direct-called class method's entry runs in the caller's process and needs
neither hop nor copy.

### REPL session

```beamtalk
> sealed Object subclass: Twice
>   class sealed twice: aBlock =>
>     aBlock value
>     aBlock value
> t := 0
0
> Twice twice: [t := t + 1]
2
> t
2
> Object subclass: Keeper
>   classState: saved = nil
>   class keep: aBlock => self.saved := aBlock
> Keeper keep: [t := t + 1]
error: block writes outer local `t`, but `keep:` cannot return the write: `keep:` stores its block in `self.saved`
  = help: return the new value from the block and assign it, or keep the state where the block is stored
> k := Keeper
> k keep: [t := t + 1]
warning: `keep:` is resolved at run time; if the receiver's method cannot thread the block this raises
ERROR: stateful_block_dispatch: 'keep:' on Keeper does not thread its block
  hint: return the new value from the block and assign it where it is called
> t
2
```

The last error is the runtime path (§4's fourth row): through a variable
the compiler cannot see `Keeper`, so the send resolves at run time, finds no
entry, and raises. `t` is untouched either way; nothing is dropped silently.
In a body that forwards the block on, the block may already have run (with
whatever side effects it has) before a downstream resolution raises; what
is guaranteed is the error, not that no code ran.

### Error examples

```beamtalk
Object subclass: Base
  class twice: b => b value. b value

Base subclass: Sub
  classState: saved = nil
  class twice: b => self.saved := b         // warning: twin-dropped-in-override
```

```text
warning: `twice:` overrides `Base twice:`, which threads its block parameter `b`, but this method stores it
  = note: a caller passing a block that writes a local will get `stateful_block_dispatch` here where `Base` threads it
  = help: invoke `b` instead of storing it, or add `@expect twin_dropped_in_override`
```

(`Base twice:` is not `class sealed`: overriding a sealed method is already
the hard error "Cannot override sealed method", so the lint only ever fires
on an overridable one.)

## Prior Art

- **Pharo / Squeak.** Any method can run a block and the caller's temps
  update, because a written temp is a heap cell shared by the block (a temp
  vector). This is the semantics users expect and the one ADR 0041 promised.
  BEAM has no shared cell; the threaded entry reproduces the observable
  behaviour by returning the cell's contents instead.
- **Erlang / Elixir.** A closure captures values; nothing it does rebinds
  its creator's variable. Elixir 1.7 made even `if`/`case` rebinding local.
  This is the honest version of "keep §6 forever" (Alternative D below):
  nobody misses the feature in a language that never offered it. Beamtalk
  offered it.
- **Haskell `State`.** `s -> (a, s)` is the entry's signature, and "lift a
  function into the monad" is what the body's lowering does. ADR 0041 cited
  it for the block; this ADR applies it to the method that runs the block.
- **Rust `FnMut`.** A callee's *signature* says whether it accepts a
  mutating closure (`FnMut`) and the compiler guarantees the writes are
  visible after the call; a callee that stores the closure needs a
  different bound. The entry is an inferred, not declared, `FnMut`-accepting
  signature, and §1's "threading parameter" is the `FnMut` / stored-closure
  distinction. Rust's choice to make it part of the type is the strongest
  argument for Alternative F (a declared annotation).
- **Swift.** Closures are non-escaping by default and must be marked
  `@escaping` to be stored; captured `var`s are shared boxes. BT-3756's
  escape rule (I11) is Swift's distinction with the default inverted: a
  block that escapes cannot be stateful here, rather than needing a mark.
- **Koka / effect handlers.** A function that runs an effectful closure is
  effectful in its row, and an effect-polymorphic body is compiled once and
  instantiated per use. One body with a per-invocation gate and two entry
  points is that compilation strategy without the type.
- **Gleam.** No mutation; `use` sugar and explicit accumulators make the
  data flow visible. It is what §6's help text has always pointed users at,
  and what the body does mechanically so the user does not have to.
- **Pony / Dart isolates.** A closure sent to another actor or isolate
  cannot mutate the sender's locals. §7's rule for the class-process hop is
  the same boundary, with the map-as-value making the write survive where
  Pony would forbid the send; I17 is the boundary kept where the frame is
  gone.
- **Within Beamtalk.** `beamtalk_result:'tryDo:'/2` (ADR 0131 §5) is the
  first threaded entry. ADR 0129 Phase 0b's `direct_class_methods` is the
  metadata resolution pattern. ADR 0128's opaque-callable fold and its
  per-element tier gate are the in-body lowering for forwarded blocks. ADR
  0118 and 0131's producers are the caller-side lowering. Nothing here is a
  new kind of mechanism; it is the four existing ones applied to the one
  boundary they stopped at.

## User Impact

- **Newcomer.** `Twice twice: [t := t + 1]` does what it looks like it
  does, in a class, a value type, an actor and the REPL. The error they
  can still hit says why the callee cannot thread ("stores its block") and
  what to write instead. A runtime `stateful_block_dispatch` through a
  variable receiver is new to them; it names the class and selector and
  gives the same rewrite, and the warning at the send told them it could
  happen.
- **Smalltalk developer.** The last large departure from "a block can
  write my temps" goes away for compiled callees. Three remain and are
  documented: a block that escapes the method or is sent asynchronously
  cannot write (I11, I17), a raise inside a protected region discards its
  writes (ADR 0131), and a block bound for another class's method cannot
  write the actor's fields. The override lint is the price of not having
  shared cells: in Pharo an override that stores the block simply works;
  here it raises.
- **Erlang/BEAM developer.** One extra exported function per HOM with a
  threading parameter, `'__t2_<sel>'/n+1`, and two metadata keys. Calling
  a Beamtalk HOM from Erlang with a plain fun is unchanged (the plain
  entry). Hand-writing a native HOM that should thread means adding an
  arity-`n+1` clause the way `'tryDo:'/2` did, with a conformance fixture
  (`docs/development/erlang-guidelines.md` gains the rule).
- **Production operator.** No runtime protocol change for existing calls;
  the entry is additive, hot reload of mixed modules degrades to a
  structured error at the one send that needs a missing entry, never a
  crash or a wrong answer; callees load before callers on upgrade. Code
  size grows by the `Acc` threading in each threading HOM's body, not by a
  second body (§Consequences). Observable with standard tools: an entry is
  a named function in the module, visible in `observer`, `recon` and stack
  traces.
- **Tooling developer.** The entry is invisible to the AST and the type
  system (ADR 0041 §Interaction with Gradual Typing: `StateAcc` is not part
  of a block's type, and the entry is not part of a method's). The §6
  diagnostic, the "why" help text, the run-time-resolution warning and the
  override lint are semantic analysis over the `ClassHierarchy` index, so
  the LSP reports them as you type, on every surface alike. Completions and
  hover show the plain selector only.

## Steelman Analysis

### A. One body, two entry points, metadata-resolved (chosen)
- 🧑‍💻 **Newcomer**: "I never see it. The program that looked right now runs right, and the error I can still get tells me what the callee does with my block."
- 🎩 **Smalltalk purist**: "This is the closest BEAM can get to a temp vector: any compiled method that runs my block hands my temps back. The escape rule is the only thing I give up, and I give it up to the VM, not to the compiler's convenience."
- ⚙️ **BEAM veteran**: "It's `tryDo:/2` for every HOM: one body, one extra export, two metadata keys, the gen_server reply shape untouched, and mixed-module reload degrades to a structured error. Erlang callers keep the plain function."
- 🏭 **Operator**: "Additive. No relup, no wire change. The failure mode under skew is an error naming the selector, not a dropped write I find in a ledger three weeks later."
- 🎨 **Language designer**: "It is the fourth application of a mechanism that already exists (producers, families as data, metadata resolution, the native twin), not a fifth mechanism. `MethodBody` finally has a builder, and the verifier obligations are the same shape as ADR 0131's."

### B. Universal caller-side protocol (every block-taking method takes `StateAcc`, ADR 0041 §Caller-Side Protocol as written)
- 🎩 **Smalltalk purist**: "One convention for every method is simpler to explain than 'some methods have an entry'."
- 🎨 **Language designer**: "No predicate, no metadata, no runtime resolution: the calling convention *is* the channel."
- ⚙️ **BEAM veteran**: "Every HOM pays a tuple and a map parameter on every call, pure block or not; every native and every Erlang caller of a Beamtalk HOM changes arity; and under hot reload an old caller and a new callee disagree on arity, so both arities must exist anyway during migration, which is A with extra steps."
- **Why not:** it is A plus a mandatory migration of every block-taking method and every external caller, for no additional coverage. ADR 0131 Alternative D's objection was aimed at exactly this reading.

### C. Mutable reference cell (a `make_ref()`-keyed process-dictionary cell for each threaded local, cleaned up on return, NLR and exception)
- 🧑‍💻 **Newcomer**: "It's how every other language does it."
- 🎩 **Smalltalk purist**: "A temp vector by another name; the callee needs no entry at all."
- **Why not:** rejected in ADR 0041 (Alternative A) and ADR 0128 (Option 1) and the reasons have not changed: a class-process callee runs the block in *another* process, where the cell does not exist; an escaping block would read a dead cell; cleanup under `$bt_nlr`, exceptions and `ensure:` is a new runtime mechanism the `ThreadedIr` verifier cannot see; and 100–500 ns per access against the map's ~15 ns. The one thing it would buy over A, threading through a native that cannot have an entry, is I9, which A handles per native.

### D. Keep ADR 0131 §6 as the permanent answer (the issue asks for this steelman)
- 🏭 **Operator**: "Zero runtime change, zero hot-reload story, zero new error at run time. The compiler already rejects the shape and the LSP already shows it. Every program that compiles today keeps compiling."
- ⚙️ **BEAM veteran**: "Elixir never had this and nobody writes `Enum.each` with a mutated outer variable; they fold. The help text already says `inject:into:`."
- 🎨 **Language designer**: "Explicit accumulators are better code. §6 nudges people to write `t := Twice twice: [t + 1]`, which is honest about the data flow. And I5 and I6 need no callee protocol at all; they are ADR 0131 producers the epic simply has not written yet."
- 🧑‍💻 **Newcomer**: (no strong case: the error is at a shape they wrote by analogy to the `do:` three lines above, which worked.)
- 🎩 **Smalltalk purist**: (no case: it bans the ordinary Smalltalk idiom and breaks ADR 0041's documented promise.)
- **Why not as the end state:** the asymmetry it leaves is the problem. The same `self hom: [t := t + 1]` works in an actor and errors in a value type; it works for a `sealed` same-class callee and errors for the inherited one; ADR 0127's trait provisions (`Enumerable` via `self do:`) thread on a value type and not on an actor. A rule a user cannot predict from the source is worse than either "always" or "never". **Adopted as the floor:** §6 stays wherever the compiler can prove no channel (§5), Phase 0 ships I5 and the runtime floor before any entry exists, and no phase narrows §6 before the path that replaces it is in.

### E. Call-site specialization (inline a statically known, sealed, same-module callee so the block becomes a §1 construct; ADR 0128 Option 4, BT-3610's recommendation)
- 🏭 **Operator**: "No runtime change at all, and the hot path is as fast as an inlined `do:`."
- 🎨 **Language designer**: "It reuses ADR 0131's producers directly, with no new family."
- **Why not:** with one class per module it only covers same-class sealed callees that are not recursive, so I3, I4 across classes, I8, I9 and every stdlib HOM stay errors; a `^` inside the inlined callee changes meaning (its NLR target becomes the caller's method); code size grows per call site rather than per method; and the "which callee" question still needs §4's resolution for every receiver that is not a literal class. Kept as a possible optimization of A for the same-module case (Open Question 4), not as the mechanism.

### F. A declared annotation (`class sealed twice: aBlock threads: aBlock =>` or a `@threads` pragma) instead of an inferred predicate
- ⚙️ **BEAM veteran**: "Rust makes `FnMut` part of the signature for a reason: the override hazard in §6 becomes a type error instead of a lint."
- 🎨 **Language designer**: "Explicit is better than inferred for something that is part of a method's contract."
- 🎩 **Smalltalk purist**: "Smalltalk has no such annotation; a method that runs a block runs it."
- 🧑‍💻 **Newcomer**: "I would not know to write it, and the error telling me to would be the §6 error with extra steps."
- **Why not now:** inference costs the user nothing and the predicate is already one place; an annotation would be a second source the predicate must agree with. The override hazard is real but is the ADR 0130 `class-state-abroad` class of hazard, handled the same way (a lint). If the lint proves insufficient, an annotation can be added on top of the same predicate as a *constraint* ("this method must have an entry") without changing the mechanism. Deferred, Open Question 3.

### G. Static-only entries (same-module sealed `self` sends and exact-receiver direct-called class methods, guarded; every other receiver stays a §6 error)
- 🏭 **Operator**: "No `send_threaded`, no metadata walk, no new runtime error, no override lint, no hot-reload story. One rule: the compiler must see the callee."
- 🎨 **Language designer**: "It is a predictable rule, and it ships in one phase."
- 🧑‍💻 **Newcomer**: "I would not understand why `Twice twice:` works and `k twice:` does not, when `k` is `Twice`."
- 🎩 **Smalltalk purist**: "Smalltalk sends are late-bound; a feature that only works for early-bound sends is not the feature."
- **Why not as the end state:** with one class per module and late-bound `self` sends, "statically bound" is a small set (sealed same-class sends and direct-called class literals), and I3, I4 across classes, I7 for a non-sealed class and I8 stay errors. **Adopted as Phase 1's scope**, with the index-known exact-receiver case: it is the part that needs no runtime walk and proves the body lowering before resolution is built, and it is where the `self_send_bench` cost is measured.

### H. Two bodies (a Tier-1-only plain body beside the threaded one)
- 🏭 **Operator**: "The plain path stays exactly what it is today, byte for byte."
- ⚙️ **BEAM veteran**: "A pure block through a HOM pays no tuple."
- **Why not:** the plain path already pays an `is_function/2` guard per invocation (Context), so the second body saves one tuple allocation per call; it costs a second lowering of every threading method, roughly doubled code, and two bodies that must agree, with "same source, same compiler" as the only thing keeping them equal. The CLAUDE.md duplication rule asks for one implementation; the measured plain-path cost (Phase 1) is the check that this choice holds.

### Tension points
- Operators and BEAM veterans split between A and D: D is free today, A is the only option that removes the actor/class asymmetry. Phase 0 and the structured runtime error are what let an operator accept A.
- Smalltalk purists prefer C's semantics and reject its mechanism once the class-process hop is explained; A is their second choice and the only one that survives the hop.
- Language designers are split between A and F. The predicate-as-one-source rule decides it for A now and leaves F as an addition.
- Operators prefer G; everyone else finds it half a feature. Making it Phase 1 gives the operator the measurable step and the others the end state.
- Everyone agrees B is A with a mandatory migration, H is A with a second body, and E is at most an optimization.

## Alternatives Considered

### B, C, D, E, F, G, H
See the Steelman Analysis.

### Reuse the `State` family for the body's carrier instead of adding `Acc`
Smaller diff (the REPL root already threads an opaque map as `State`).
Rejected in §2: `State` carries actor-specific meaning and a verifier rule
whose whole purpose is to keep it out of class and value-type methods.

### `Acc` as the body's scratch `StateAcc` map (slot 2) rather than a trailing family
It would reuse every `StateAcc`-mode lowering unchanged. Rejected in §2: the
body's own `'__local__'` keys and the caller's would share one map, so a
caller and a callee with a local of the same name, or a recursive HOM,
would read each other's values silently; and a `StateAcc` plan seeded from
`Acc` would carry the BT-2717 stray-key hazard into another frame's locals.
The trailing-family choice costs the per-site capability work of Phase 3
and buys a key space that cannot collide.

### Extend the BT-912 actor channel to class and value-type methods by giving them a `State` parameter
This is B restricted to compiled Beamtalk methods: it changes the arity of
every class and value-type method, direct-called ones included, and so
every runtime caller (`class_send/3`, `dispatch/3`, ADR 0084 funs, the
supervisor paths). Rejected for the same reason as B.

### An entry per (method, parameter) rather than per method
`'__t2_0_twice:'`, `'__t2_1_each:do:'`, one for each threading position
and one for each combination a caller might use. Rejected: the
combinatorics are unbounded for a method with several block parameters,
and the per-invocation gate makes one entry accept any mix.

## Consequences

### Positive
- I1–I8 of the inventory work in class methods, value-type methods, actor
  methods and the REPL, with one mechanism. The actor/class asymmetry, the
  `sealed`/inherited asymmetry and the ADR 0127 trait asymmetry go away.
- ADR 0131 §6 narrows to the cases that are provably channel-less, and its
  help text can say what the callee does with the block.
- `stateful_block_dispatch` becomes a precise error: the runtime knows,
  from metadata, that the callee does not thread, instead of guessing from
  a fun arity.
- `MethodBody` gets its production builder; the predicate BT-3755 wrote
  becomes the single source for the entry, the diagnostic, the metadata,
  the index and the lint; the verifier gains checks that fail on the
  pending fixtures before the fix.
- The `tryDo:/2` precedent becomes the rule for natives, so a hand-written
  HOM that should thread has a documented, fixture-checked shape.
- I5 ships on its own in Phase 0, a plain ADR 0131 producer.
- **The tier is decided before a block runs** (BT-3885). A Tier 2 fun carries
  a mark from creation, so the gate never applies a wrong-arity block to find
  out what it is, and no reply shape is ever trusted: a plain block that
  answers `{ok, #{...}}` cannot become the caller's `Acc`. The same mark lets
  the plain-entry refusal be precise (`stateful_block_dispatch` only for a
  real Tier 2 block, naming the class method; `arity_mismatch` for a block of
  the wrong arity).

### Negative
- **Every Tier 2 invocation through the gate pays the mark check** (BT-3885):
  one `erlang:fun_info(F, name)`, one `atom_to_binary/2` and a scan of the
  name's last few bytes, about 100 to 140 ns on the BT-3885 microbenchmark
  (`call_threaded/3` with a marked Tier 2 fun and a map `Acc`: about 140 ns,
  against about 13 ns for the old arity-only gate). Tier 1 invocations and the
  plain entry's Tier 1 path pay nothing (11 to 12 ns before and after); the
  check runs only for a fun of arity n+1. Phase 1f's measurement includes it;
  if it shows up, the decision can be memoised per lifted name.
- **The mark relies on the Erlang compiler's lifted-function naming**
  (`-F/A-Name/Arity-K-` for a `letrec` fun), an implementation detail that has
  been stable across OTP releases but is not a documented contract. A change
  fails loudly, not silently: every Tier 2 block would be refused as
  `arity_mismatch`, and the end-to-end EUnit test over a compiled Tier 2 block
  (`beamtalk_threaded_class_entries_tests:compiled_mark_test_`, run through
  `beamtalk_dispatch_tests`) goes red.
- **An unmarked Tier 2 fun is refused.** A Tier 2 block compiled before
  BT-3885, or a hand-written Erlang `fun(Args..., Acc)`, raises
  `arity_mismatch` at a threaded entry. Erlang code that means to hand the gate
  a Tier 2 block mints it as a named fun called `?BT_TIER2_BLOCK_NAME`
  (`beamtalk_threading_abi.hrl`) that uses its own name, as the EUnit helper
  `beamtalk_tier2_test_block:mark/1` does (a test pins that spelling to the
  generated name).
- **The plain path of a threading HOM pays one tuple per block invocation**
  and threads the `plain` sentinel as `Acc` through its body's constructs (a trailing
  slot in each loop or fold tuple). Phase 1 measures it against today's
  structural fallback on the same body.
- **Code size** grows by the `Acc` threading in each threading HOM's body
  and one thin entry function; about 90 stdlib methods qualify (130
  block-taking headers minus the 40 natives and intrinsics). Phase 1
  reports the `.beam` size delta.
- **Build time.** Lowering a threading body over `Acc` costs more than
  lowering it plain. Phases 1 and 3 each measure against the ≤3 % gate with
  ADR 0131's methodology; a method with no threading parameter costs nothing.
- **Per-call resolution cost.** A cross-module threaded send pays the
  dispatch walk, a metadata lookup and a `function_exported/3` check on
  top of the pack and unpack. With one class per module that is every
  threaded send to another class. Phase 2 benchmarks it against ADR 0041's
  ~65 ns budget and `self_send_bench`.
- **A new runtime error at a non-exact receiver,** and the warning that
  announces it. A Tier 2 block sent through a variable to a callee with no
  entry raises where today it is a compile error; the LSP cannot show it,
  and in a forwarding chain the block may already have run before the
  raise. The error is structured and names the fix.
- **The override lint is not a guarantee** (§6), as `class-state-abroad`
  is not. A subclass that drops an entry breaks a caller at run time.
- **A new trailing family** with `SelfVt`'s capability gaps to lift (Phase
  3, L): `verify()`, `render()`, the hand-built IR fixtures,
  `eligible_families` and every per-site capability table gain `Acc`.
- **A block bound for another class's method cannot touch actor fields**
  (§3): a new, documented restriction on actor blocks passed to HOMs,
  reported at compile time.
- **Metadata grows** by two keys per module, read on class registration and
  reload; `class_send/3`'s cache gains a sibling; the `ClassHierarchy`
  `MethodInfo` gains `threading_params` and the stdlib's generated builtins
  carry it.
- **The class-process hop copies the map twice** (in the message, in the
  reply). The map holds only the block's written locals, so it is small,
  and the hop already copies the arguments and the result.
- **Two channels in actors.** The BT-912 `State` channel and the entry
  coexist for different sends; the agreement test keeps them from
  overlapping (Open Question 1).

### Neutral
- The FFI boundary (I10) stays lossy with a warning; an Erlang callee can
  never return a map it does not know about.
- Escapes (I11), asynchronous sends (I17), `Dictionary`
  `at:ifAbsentPut:`/`removeKey:ifAbsent:` (I12), cascade/interpolation/
  `match:` subjects (I13) and the keyword `whileTrue:` condition (I14) are
  unchanged and remain ADR 0131's.
- The REPL display for the probe shapes is the transcript in §REPL
  session, approved by this ADR as ADR 0131 approved its own (CLAUDE.md §
  REPL output); any other display change needs its own sign-off.

## Implementation

| Phase | Scope | Proof | Size |
|---|---|---|---|
| 0 | **Pins, the floor, I5 and the wire check.** Move the four `adr0131section6callee` pending fixtures and the class/value-type shapes of I1–I4, I6–I8 into `PIN-BUG` BUnit files over all three method kinds and the REPL. Add `tier2_methods`/`tier2_class_methods` to `'__beamtalk_meta'/0` (empty), `MethodInfo::threading_params` (empty), and `beamtalk_dispatch:send_threaded/4` and `beamtalk_dispatch:call_threaded/3` raising the precise `stateful_block_dispatch`. §6 unchanged. I5 as ADR 0131's inline `value` producer over a fresh map. Wire check: one hand-written Erlang entry (`'__t2_class_twice:'/3` in a test module with a hand-written metadata entry) reached through `send_threaded/4` from a compiled class method whose Tier 2 block is packed and unpacked by hand in the fixture, proving the walk, the lookup, the `function_exported/3` guard, the entry call and the map round trip before any codegen exists. Baseline build time, `.beam` sizes, `self_send_bench`. | Every pin red as a compile error or the structured runtime error; I5 green; the wire check green; no silent wrong answer. | S |
| 1 | **Static entries (Alternative G as scope).** `VersionPrefix::Acc`, `ScopeKind::Threaded`, `eligible_families`; the `MethodBody` builder; `call_threaded/3`; the predicate generalized to the §1 direct-invocation, callable-argument and same-module pass-on uses with the greatest-fixpoint rule; one body with two entries, metadata and `threading_params` emission (stdlib via `generated_builtins.rs`); the `callee_threaded_send` producer (§3) for statically bound same-module sends and exact-receiver direct-called class methods, guarded by `function_exported/3` across modules; the §6 narrowing for exact receivers with "why" help; the actor locals-only rule for blocks bound for an entry. Verifier: `AccAsScratchSeed`, `OwnLocalKeyInAcc`, `NlrSlotShapeMismatch`, `ActorStateInThreadedBlock`. | I1, I4 (same module), I7 (sealed) pins green in class and value-type; the agreement test extended; `just verify-threaded-ir`; the `local_touch` corpus gains `hom_call`, same-named caller/callee locals, recursion and NLR shapes, with the **oracle** that the explicit-accumulator spelling of each program (`t := X hom: [... t + 1]` via `inject:into:`) answers the same; plain-path cost, `.beam` size and build-time measured. | L |
| 2 | **Resolution.** `send_threaded/4` as the `dispatch/3` walk with the entry lookup at the resolved implementation (extensions, runtime funs and DNU proxies → structured error; DNU → `does_not_understand`); `super_threaded/5`; the non-exact-receiver warning; cross-module forwards and forwards to unknown receivers inside a body; I6 (the ADR 0128 fold seeded from the pack, with `call_threaded` as its gate, in class, value-type, REPL and actor context); the REPL bindings-map rule. | I1 and I4 for unknown and cross-module receivers, I6 pins; `send_threaded` benchmarked against the ~65 ns budget; a module-skew test (new caller, old callee; old caller, new callee; reload mid-cache). | M |
| 3 | **Callee frames: `Acc` capability per site.** Lift the trailing-family rejections for `Acc` at `Letrec` bodies, the search and keyed folds, arms holding a fold, `on:do:`/`ensure:` arms, bare and stored blocks (Tier 2 by containment with the `'__acc__'` nesting) and the NLR boundary; `Block`'s own intrinsics with a non-literal receiver or argument; `arity`/`respondsTo:` lowering. | I2 and the I9 intrinsic row; a per-site capability corpus (site × fold kind × body × position × tier mix), as BT-3827's; `.core` diff harness lists the expected diffs. | L |
| 4 | **Late binding, processes, actors.** `safe_dispatch_threaded`, `class_self_send_threaded`, the threaded `dispatch/3` clause; the threaded `class_method_call` variant through `invoke_class_method/7` (I8); actor bodies as paired `dispatch/4` clauses and `'__sealed_'` functions with the `{Result, Acc}` reply; I3; the override lint (§6). | I3, I7 (late-bound), I8 pins; `hom_composability_actor.bt`, `self_send_hom_pool.bt`, `tier2stored_block_matrix_actor.bt` unchanged; `self_send_bench` before and after; the BT-3755 exemption in `local_threading.rs` reduced to the agreement test's non-overlap rule. | M |
| 5 | **Natives, on demand.** For each Erlang-implemented HOM that should thread: an arity-`n+1` clause sharing the body (the `'tryDo:'/2` shape), a hand-written metadata entry in the native facade, and a `.bt` conformance fixture asserting both arities agree on the raising and non-raising paths. The Erlang guideline. | Per native. | S each |
| 6 | **Close-out.** Docs (`beamtalk-language-features.md` § What Works and What Doesn't, § Passing Blocks Through Class Methods; `docs/agents/expanded.md` § State-Threading Codegen; `debugging.md` verifier table; `erlang-guidelines.md`; `surface-parity.md`); ADR 0131 §6 and ADR 0128 §"Explicitly narrowed" amended to point here; flip every pin; final measurement; Status to Implemented. | CI. | S |

Dependency order: 0 → 1 → 2 → 3 → 4; 5 and 6 after 4. Phase 1 lands with
the verifier checks and the producer together; shipping a body without the
producer, or the producer without the §6 narrowing for exact receivers,
would reintroduce a silent path. The `expanded.md` proof rule is met as
follows: the invariant per scope kind is §2's table; each `VerifyError` is
shown to fail on hand-built IR of the pending fixtures' shapes before its
lowering exists (the fixtures themselves are compile errors today and have
no IR, so the hand-built IR is the repro); the execution oracle is the
explicit-accumulator spelling; the corpus is generated; the self-send path
reports `self_send_bench`.

Affected components: `beamtalk-core` (`semantic_analysis/tier2_callee.rs`,
`block_facts.rs` for containment and the capability table,
`validators/local_threading.rs`, `class_hierarchy` `MethodInfo`,
`generated_builtins.rs`, the two lints), `beamtalk-codegen`
(`threaded_ir/{ir,verify,build}.rs`, `generator/version.rs` for `Acc`,
`threaded_expr.rs` for the producer, `gen_server/methods.rs`,
`value_type_codegen.rs`, `dispatch_codegen.rs`, `class_meta.rs`,
`spec_codegen.rs` for the entry's `-spec`,
`control_flow/list_ops/opaque_fold.rs`, `intrinsics.rs`), the runtime
(`beamtalk_dispatch.erl` `send_threaded/4` and `super_threaded/5`,
`call_threaded/3` beside it, `beamtalk_actor.erl`
`safe_dispatch_threaded`, `beamtalk_class_dispatch.erl`,
`beamtalk_object_class.erl` for the threaded class call,
`beamtalk_error.erl`), stdlib natives per Phase 5, BUnit fixtures,
`tests/repl-protocol/cases/`, `runtime/perf/self_send_bench`.

**Shared-kernel check.** The predicate has one home,
`tier2_callee::callee_threads_block_param`, read by the §6 validator, codegen,
the metadata emitter and the `ClassHierarchy` index; the runtime reads the
emitted metadata and re-derives nothing (the ADR 0129 pattern). Tier 2 by
containment and the per-site capability table live in `block_facts`. The
caller-side lowering is an ADR 0131 producer and reuses `threaded_locals_of`;
no second threaded-set walk. `Acc` goes through ADR 0122's families-as-data,
not a per-construct slot. The tier gate has one implementation,
`call_threaded/3`, used by the body, the opaque fold and the structural
fallback. The Rust ↔ Erlang boundaries are two: the metadata shape (a Rust
emission test plus an Erlang test that `send_threaded/4` honours a compiled
module's map and raises on a module without it, both over one `.bt`
fixture) and each native entry's conformance fixture, as for `tryDo:/2`.

## Migration Path

No correct program changes behaviour. Programs that are ADR 0131 §6 errors
today compile and run once their callee has an entry. The one new way a
program can fail is the runtime `stateful_block_dispatch` through a
non-exact receiver whose callee does not thread; such a program was a
compile error before, so nothing that compiled before this ADR regresses,
and the warning at the send names it. An actor block passed to a HOM that
also writes a field becomes a compile error where today it is a §6 error
or a dropped write. A hand-written Erlang HOM that users pass stateful
blocks to gains an entry clause in Phase 5 or raises the structured error.
Modules compiled before this ADR need no change and keep working as callees
of old callers; recompiling them adds their entries. On upgrade, load
callees before callers.

## Open Questions

1. **Retire the BT-912 actor channel.** After Phase 4 every actor send has
   an entry path. `generate_tier2_self_send_open` packs into `State` and
   needs the callee in the same pre-scan; the entry does not, but costs a
   pack. Decided by `self_send_bench` after Phase 4; the default is to keep
   it while it is measurably cheaper.
2. **Runtime-installed class methods (ADR 0084).** A `put_class_method/4`
   fun has no entry and no metadata; `send_threaded/4` raises for it. Whether
   `ClassBuilder` grows an entry form (a fun of arity `n+2` registered beside
   the `n+1` one) is left to a need.
3. **A declared annotation on top of the predicate** (Alternative F) if the
   override lint proves insufficient in practice.
4. **Same-module inlining** (Alternative E) as an optimization of a
   statically bound entry call, if Phase 1's measurement shows the pack and
   tuple on a hot same-class HOM matter.
5. **Which natives first.** The candidates are the Erlang-implemented HOMs
   users pass stateful blocks to in practice, counted from real §6 hits
   before Phase 5 is scheduled; `Future`, `Timer` and every asynchronous
   callee are never candidates (I17).

## References
- Related issues: BT-3742 (this ADR), BT-3755 (the actor exemption and its
  four pending probes), BT-3756 (escapes), BT-3610 / BT-3583 (the actor
  self-send HOM gap, ADR 0128), BT-912 (the actor channel), BT-853 / BT-852
  (ADR 0041 Tier 2 caller and callee sides), BT-3666 / BT-3696 (late-bound
  self-sends), BT-3738, BT-3743 (ADR 0131 epic), BT-3841 (Phase 0 diagnostic
  retirement), BT-3827 / BT-3854 (trailing-family capability), BT-3813,
  BT-3839
- Related ADRs: ADR 0041 (universal block protocol; §Caller-Side Protocol,
  §Erlang Interop Boundary, §State-Carrying NLR), ADR 0006 (dispatch),
  ADR 0040 (one class per file), ADR 0066 (extensions), ADR 0084 (runtime
  class-method funs), ADR 0087 (synthetic-method cross-reference), ADR 0109
  (blocks run in the class process), ADR 0111 (verifier), ADR 0118
  (`ThreadedValue`), ADR 0122 (families as data, per-site capability), ADR
  0125 (releases and skew), ADR 0127 (traits), ADR 0128 (opaque-callable
  folds; §"Explicitly narrowed"), ADR 0129 (direct-called class methods,
  metadata resolution), ADR 0130 (class variables), ADR 0131 (outer-local
  producers; §5, §6, Alternative D, Open Question 1)
- Code: `crates/beamtalk-core/src/semantic_analysis/tier2_callee.rs`,
  `crates/beamtalk-core/src/semantic_analysis/validators/local_threading.rs`
  (`check_send`), `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen.rs`
  (`generate_tier2_self_send_open`, `scan_class_for_tier2_blocks`,
  `class_self_send_call_doc`), `crates/beamtalk-codegen/src/core_erlang/driver.rs`
  (`compute_direct_call_eligible`), `crates/beamtalk-codegen/src/core_erlang/blocks.rs`
  (`generate_erlang_interop_wrapper`, `generate_block_stateful_fun`),
  `crates/beamtalk-codegen/src/core_erlang/intrinsics.rs` (the structural
  fallback), `crates/beamtalk-codegen/src/core_erlang/class_meta.rs`,
  `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl`
  (`class_send/3`), `runtime/apps/beamtalk_runtime/src/beamtalk_dispatch.erl`,
  `runtime/apps/beamtalk_stdlib/src/beamtalk_result.erl` (`'tryDo:'/2`),
  `stdlib/test/fixtures/adr0132section6_actor.bt.pending`,
  `stdlib/test/fixtures/hom_composability_actor.bt`,
  `stdlib/test/fixtures/self_send_hom_pool.bt`
- Documentation: `docs/beamtalk-language-features.md` § Control Flow and
  Mutations, § Passing Blocks Through Class Methods;
  `docs/agents/expanded.md` § State-Threading Codegen;
  `docs/development/architecture-principles.md` §6, §7;
  `docs/development/surface-parity.md`
