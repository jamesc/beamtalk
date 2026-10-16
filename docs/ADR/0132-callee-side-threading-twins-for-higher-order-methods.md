# ADR 0132: Callee-Side Threading Twins for Higher-Order Methods

## Status
Proposed (2026-10-16)

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
Object subclass: Twice
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
the same error (`stdlib/test/fixtures/adr0131section6callee_actor_pending.bt.pending`
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

**Calling conventions.** The ADR 0130 convention for a class method is
`class_<sel>(ClassSelf, A1, …, An) -> Result`, bare, with class variables
written in place in the class process. A value-type method is
`'<sel>'/n+1 = fun (Self, A1, …, An) -> Result`, reached through
`'dispatch'/3 = fun (Selector, Args, Self)` (ADR 0006). A `sealed` actor
method is `'__sealed_<sel>'(A1, …, An, Self, State) -> {'reply', Result,
State1}`. A direct-called class method (ADR 0129 Phase 0b: `class sealed`
on a sealed class with no `classState:`) is listed in the module's
`'__beamtalk_meta'/0` as `direct_class_methods => #{Selector => SafeFn}`,
and `beamtalk_class_dispatch:class_send/3` reads that map (cached at class
registration, refreshed on reload) to call `Module:SafeFn(nil, Args...)` in
the caller's process instead of the class gen_server. **Eligibility is
computed once in Rust and shipped as generated metadata; the runtime has no
second implementation.** That pattern is reused below.

**One twin already exists.** ADR 0131 §5 gave `beamtalk_result:'tryDo:'`
an arity-2 clause, `'tryDo:'(Block, StateAcc) when is_function(Block, 1)`,
answering `{Result, StateAcc1}`, sharing `protect/1` and the catch sequence
with the arity-1 clause, with a Rust ↔ Erlang conformance fixture. Codegen
chooses the arity from the block literal. That is a per-method
callee-side `{Result, StateAcc}` protocol for exactly one native. This ADR
generalizes it.

### The inventory

ADR 0131 §6 and BT-3742 define the inventory as "the set of call sites §6
rejects". Enumerated from the §6 validator (`validators/local_threading.rs`
`check_send`), its fixtures, and ADR 0131's close-out:

| # | Shape (the block writes outer local `t`) | Context | Today | This ADR |
|---|---|---|---|---|
| I1 | `X hom: [t := t + 1]`, user HOM invoking its parameter directly (`b value`, `b value: x`, a `value`-family cascade) as a statement, an assignment RHS or a `^` value | class, value-type | §6 error | **Phase 1** |
| I2 | as I1, callee invokes the parameter inside a `do:` body, a conditional arm, an operand, an `on:do:`/`ensure:` block, or a nested closure (`f := [b value]. f value`) | all three (BT-3755's four actor probes) | §6 error | **Phase 2** |
| I3 | as I1, callee inherited from a superclass, or overridable (class or method not `sealed`) | actor (self-send) | §6 error | **Phase 1** (runtime resolution) |
| I4 | callee passes the parameter on, bare, to another HOM | all | actor only, same-class chain | **Phase 1** |
| I5 | stored Tier 2 block sent `value` directly, no callee (`b := [t := t + 1]. b value. t`; ADR 0131 probe s4) | class, value-type | §6 error | **Phase 1** (a producer with a fresh map) |
| I6 | stored Tier 2 block as the callable of a collection message (`items do: blk`) | class, value-type, REPL | §6 error (ADR 0128 narrowed) | **Phase 2** (opaque fold seeded from the map) |
| I7 | class-side `self hom: [t := t + 1]` in a class method | class | §6 error | **Phase 1** |
| I8 | a Tier 2 block passed to a class method of a class with `classState:` or not `class sealed` (runs in the class process, ADR 0109) | any | §6 error | **Phase 3** |
| I9 | an Erlang-implemented (native) stdlib HOM that invokes the block | any | §6 error, or `stateful_block_dispatch` | **Phase 4**, per native, opt-in; default a structured error |
| I10 | an Erlang FFI argument (`(Erlang lists) map: [...] with: ...`) | any | lossy, warned | unchanged (ADR 0041 §Erlang Interop Boundary) |
| I11 | the block escapes the method: `^[…]`, last expression, field or class-variable store, collection literal (BT-3756) | any | §6 error | unchanged, **permanent** |
| I12 | `at:ifAbsentPut:` / `removeKey:ifAbsent:` on a `Dictionary` | any | §6 error | unchanged for `Dictionary` (immutable, no behaviour to lower); a user class defining either selector is an ordinary HOM, I1 |
| I13 | cascade part, string interpolation, `match:` subject | any | §6 error (ADR 0131 close-out) | unchanged (no statement boundary for a prelude) |
| I14 | a keyword `whileTrue:`/`whileFalse:` condition block (BT-3782) | class | §6 error | unchanged (BT-3743) |
| I15 | a runtime-installed class method fun (ADR 0084 `put_class_method/4`) | class | n/a | no twin; structured error at run time (Open Question 2) |
| I16 | an ADR 0127 trait provision routing the block through `self do:` | value-type: works (`self_send_hom_sackful.bt`); actor: BT-912 channel | unchanged; actor via Phase 3 |

### Constraints

- ADR 0041: the Tier 2 closure shape `fun (Args..., StateAcc) -> {Result,
  StateAcc1}` stays. Locals stay immutable Core Erlang variables. No process
  dictionary cell, no mutable reference (ADR 0041 Alternative A, ADR 0128
  Option 1, both rejected and not reopened here).
- ADR 0111 / CLAUDE.md: a state-threading change states its invariant for
  every scope kind, is owned by the `ThreadedIr` node that owns the scope,
  adds the `VerifyError` that fails on the repro first, and is proved over a
  generated corpus. No per-call-site sync, refresh or commit.
- ADR 0122: families are data (`ThreadedFamilies`, `eligible_families`).
- ADR 0129 Phase 0b: a `class sealed` method of a sealed stateless class is
  direct-called in the caller; its eligibility is Rust-computed metadata.
- ADR 0130: class variables are not a threading family. A block's
  class-variable writes are live only at home; abroad they raise
  `class_state_unreachable`. Nothing here changes that.
- ADR 0131: the `LocalRebind`/`ConstructTuple`/`BranchArm`/`MethodBody`
  nodes, the `threaded_locals_of` recognizer and the §2 per-frame lowering
  rule are the only way an outer-local write reaches its frame.
- Hot code reload: a module compiled before this ADR and one compiled after
  it coexist in one node (ADR 0125 §3.4's skew contract; ADR 0105 live
  re-check). Neither direction may crash or silently drop a write.
- ADR 0111's ≤3 % build-time gate (user CPU, the ADR 0131 methodology) and
  ADR 0041's ~65 ns Tier 2 call budget.

## Decision

**A method that invokes or forwards a block parameter is compiled twice:
once as it is today, and once as a *threading twin* that takes a `StateAcc`
map after its ordinary parameters and returns `{Result, StateAcc1}`. The
twin's body is lowered as one `ThreadedIr` `MethodBody` frame over a new
opaque family, `Acc`, so every invocation of the parameter, in every
construct ADR 0131 covers, threads the caller's map. Twins are advertised in
`'__beamtalk_meta'/0` and resolved by the runtime the way ADR 0129 resolves
direct-called class methods. A send that passes a Tier 2 block value is, on
the caller's side, one more ADR 0131 producer: it packs the block's locals,
calls the twin, and rebinds from the returned map. Which methods have a twin
is decided by one predicate in `beamtalk-core`, read by the §6 diagnostic,
by codegen and by the metadata emitter.**

### 1. Which methods get a twin

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
  `items inject: 0 into: b`, the ADR 0128 opaque-callable folds);
- a **bare argument of another send** whose callee (resolved as §4 resolves
  it) has a twin for that position, including a send to `self` or `super`;
- the receiver or argument of a **nested block literal** that is itself
  only used in these ways (`f := [b value]. f value`): a block that invokes
  or forwards a Tier 2 value is Tier 2 by containment (§2).

A parameter with any other use (stored in a field, a class variable or a
collection literal; returned; passed to an Erlang FFI call; asked anything
but `value`-family, `numArgs` and `respondsTo:`; passed to a send whose
callee has no twin) is **not** a threading parameter, and the method has no
twin for it. BT-3755's four shapes (top-level direct invocation, the `last
:= b value: e` loop shape, the bare callable form, pass-on) are the subset
this ADR's Phase 1 lights up; the rest is Phase 2.

A method has **one twin**, `'__t2_<sel>'`, with every threading parameter
Tier 2. A caller that passes a Tier 1 block in a threading position
alongside a Tier 2 one wraps it, `fun (Args..., S) -> {apply B(Args...),
S}`, which is ADR 0041 §Erlang Interop Boundary's converse wrapper and costs
one tuple. A method with no threading parameter has no twin and nothing
changes for it.

The predicate's answer is **delivered, never re-derived**: codegen emits it
into `'__beamtalk_meta'/0` as

```erlang
'tier2_methods' => #{ 'twice:' => {'__t2_twice:', [0]},
                      'each:do:' => {'__t2_each:do:', [1]} }
```

(selector → twin function name and the threading positions), beside
`direct_class_methods`. The runtime reads it (§4); the §6 validator reads
the predicate (§5); the override lint reads it (§6). There is one source.

### 2. What a twin is

For a class method:

```erlang
%% Today (unchanged):
'class_twice:'/2 = fun (ClassSelf, ABlock) ->
    let _ = <structural-fallback apply ABlock> in <structural-fallback apply ABlock>

%% The twin. ABlock is a Tier 2 parameter: fun (Acc) -> {V, Acc1}.
'__t2_twice:'/3 = fun (ClassSelf, ABlock, Acc0) ->
    let {_V1, Acc1} = apply ABlock (Acc0) in
    let {V2, Acc2}  = apply ABlock (Acc1) in
    {V2, Acc2}
```

For a value-type method, `'__t2_twice:'/3 = fun (Self, ABlock, Acc0) ->
{Result, AccN}`. For an actor instance method (Phase 3), the twin is a
`dispatch/4` clause and a `'__sealed_'` function whose reply is `{'reply',
{Result, AccN}, StateN}`: the caller's map rides *inside the reply value*,
so the gen_server protocol and `safe_dispatch` are untouched and a twin
reached from another process works the same way as one reached from the
same process. The ADR 0131 §5 `tryDo:` twin is the native form of the same
shape.

**The twin's body is one `MethodBody` frame over the `Acc` family.** ADR
0131 §2 defined `ThreadedStmt::MethodBody { frame, threads, body }` and
noted it has no production builder. The twin is its first: `MethodBody {
frame: Twin, threads: [], carrier: Acc }`. `Acc` is a new storage family
(`VersionPrefix::Acc`, versions `Acc0`, `Acc1`, …), the opaque map the
caller handed over, whose keys the callee never names. In twin scope
`eligible_families` answers `[Acc]` for a class twin, `[Acc, SelfVt]` for a
value-type twin that writes a field, and `[State, Acc]` for an actor twin.
Every construct the body contains then threads `Acc` through its existing
families-as-data path (ADR 0122's `extract_family_slots`), exactly as it
threads `State` in an actor method today:

| Scope kind inside a twin | Invariant for `Acc` after it |
|---|---|
| top-level statement | `Acc_{n+1}` is the map after every Tier 2 invocation the statement made, in source order |
| conditional arm (`BranchArm`) | the arm's closer carries `Acc` out as a family slot; both arms rebind the same version |
| `Letrec` loop body (`whileTrue:`/`timesRepeat:`/`to:do:`) | `Acc` is a loop parameter; the recursive call passes the body's final version; the loop's result tuple carries it |
| `Foldl` loop body (`do:`/`collect:`/`inject:into:`/…) | `Acc` is a slot of the fold accumulator, as `State` is in an actor fold (ADR 0128's `{Slot, [FoundFlag,] StAcc}`) |
| `on:do:`/`ensure:` arm | the protected body's final `Acc` on the non-raising path; the `Acc` on entry on the raising path (the ADR 0131 protected-region rule: a raise discards the region's writes) |
| bare block / evaluated literal block | a block literal that invokes a Tier 2 parameter is itself Tier 2 (Tier 2 by containment) and takes and returns `Acc`; an evaluated `[...] value` is the one-arm conditional of ADR 0131 (BT-3841 PR 6) |
| stored closure invoked later in the body (`f := [b value]. f value`) | `f` is a Tier 2 local by containment; `f value` is ADR 0131 §1's inline Tier 2 `value` producer over `Acc` |
| NLR boundary | the `$bt_nlr` 4-tuple's state slot carries `Acc` (ADR 0041 §State-Carrying NLR; in a value-type twin that also threads `SelfVt`, the slot carries the family tuple `{SelfN, AccN}`); the twin's catch answers `{Val, AccAtThrow}` |

**Tier 2 by containment.** Today a block is Tier 2 when it writes an outer
local (`captured_mutations_for_block`). This ADR adds: a block literal that
invokes a Tier 2 parameter or a Tier 2 local, or forwards one in a
threading position, is Tier 2 with an empty write set. It takes and returns
`Acc` and does nothing else with it. The rule lives beside
`captured_mutations_for_block` in `beamtalk-core`'s block facts, so the §6
validator and codegen agree on it.

**Why a new family and not `State`.** The REPL root already threads an
opaque map it does not name (the bindings), and it does so as the `State`
family. Reusing `State` for the twin would be the smallest change. It is
rejected because `State` carries actor meaning the twin must not inherit:
`make_self`, `'field__'` keys, the BT-2717 stray-key hazard, and the
`ActorStateInClassMethod` verifier rule that ADR 0131 extended to value-type
methods precisely to keep `State` out of them. `Acc` is a fourth
`VersionPrefix`, which ADR 0122 designed families-as-data for; adding it
touches `eligible_families`, the prefix enum, and nothing in the construct
lowerings.

### 3. The caller side is a producer

A send that passes a Tier 2 block value (a literal that writes an outer
local, or a Tier 2 local) to a non-§1 selector is lowered as an ADR 0131
**producer**, `callee_threaded_send`, in `local_threading_producer`'s case
list:

```text
prelude: [ Bind   { Acc0 ← ~{ '__local__t' => T }~ },             -- pack, one Put per local
           ConstructTuple { carrier: CT, doc: <twin call>, threads: ["t"] },
           LocalRebind { local: "t", carrier: CT, slot: Key("__local__t") } ]
value:   element(1, CT)
```

The pack is a fresh map in a class or value-type method (`~{}~` seeded with
the block's `threaded_locals_of` set). In an actor instance method and in
the REPL it is the same fresh map: the actor `State` and the REPL bindings
are *not* used as the carrier, because the callee may run in another
process and its map comes back as a value regardless. The `LocalRebind`s
are lowered by the enclosing frame under ADR 0131 §2's table, unchanged, so
the send works as a statement, an assignment RHS, an operand, inside an arm
or a loop body, and in the REPL as an operand site. ADR 0131 §1a's
sibling-read rule applies: `t + (Twice twice: [t := t + 1])` reads `t`
before the send's rebind.

The existing BT-912 actor channel (`generate_tier2_self_send_open`,
`tier2_method_info`, packing into `State`) is kept through Phase 2 for the
shapes it serves today and retired in Phase 3 when actor twins exist (Open
Question 1); until then the producer above is used for every send the old
channel does not accept, so the two never apply to the same send.

### 4. Resolution: static when known, runtime when not, error when known not to

| The receiver's class and method are… | The caller emits |
|---|---|
| **Statically known, in the same compilation unit** (a `self`/`super` send, a class in the same file, the enclosing class) and the method has a twin for that position | a direct call of the twin: `call 'mod':'__t2_twice:'(ClassSelf, B, Acc0)` (class, direct-called), or the value-type function, or the `'__sealed_'` twin (actor Phase 3). Same process, no lookup. |
| **Statically known not to thread**: the method is in the unit (or in the compiler's class index for stdlib) and has no twin for that position; or the receiver is a class object, `self`/`super` or an inferred known type whose method stores, returns or FFI-forwards the block | the ADR 0131 §6 compile error, with a help text that now says *why*: "`keep:` stores its block in `self.saved`", "`map:with:` is an Erlang call". |
| **Unknown** (a variable, an argument, an inferred union, `perform:`, a class not in the unit) | `call 'beamtalk_dispatch':'send_threaded'(Receiver, 'twice:', [B], Acc0)`, which answers `{Result, Acc1}`. |

`send_threaded/4` is the one runtime entry. It resolves the receiver's
class as `dispatch/3` does (ADR 0006's hierarchy walk, `class_send/3` for a
class receiver), reads `tier2_methods` from the class's cached metadata, and:

- calls the twin (direct, or through the class gen_server for a class
  method that is not direct-called: `class_method_call` gains a threaded
  variant whose reply is the `{Result, Acc1}` value, §Phase 3);
- or raises `#beamtalk_error{kind = stateful_block_dispatch}` naming the
  class and selector, with the hint "`twice:` on `Twice` does not thread
  its block: return the new value from the block and assign it where it is
  called". The arity ambiguity `beamtalk_error.erl` records today ("wrong
  number of arguments, or the block captures mutable state") disappears:
  the lookup is by metadata, not by fun arity, so the message says one
  thing.

A `perform:`-family send with a Tier 2 block argument (`k perform: #twice:
with: [t := t + 1]`) is the unknown case: `send_threaded/4` is called with
`'perform:with:'` and resolves the performed selector at run time, as
`perform:` resolves it today (ADR 0006), so reflection reaches a twin the
same way a direct send does. `respondsTo:` and `selectors` answer the plain
selector only; a twin is not a selector.

This is the ADR 0131 `Dictionary` lookup rule (BT-3798) applied to callees:
a compile error where the compiler can see the answer, a runtime-guarded
path where it cannot, and never a silent drop. Nothing in Erlang re-derives
the predicate: the metadata is the predicate's answer, as
`direct_class_methods` is for ADR 0129.

**Mixed old and new modules under hot reload.** The plain function keeps
its name, arity and behaviour, so an old caller of a new callee is
unchanged. A new caller of an old callee: a direct twin call is emitted
only within one compilation unit (always rebuilt together), and every
cross-module send goes through `send_threaded/4`, which finds no
`tier2_methods` in the old module's metadata and raises the structured
error at the one send that needs threading. No `.appup`/relup change (ADR
0125): the twin is an added export and an added metadata key, neither of
which touches process state, so `beamtalk_hot_reload:code_change/3` has
nothing to migrate. Externally compiled
packages fall under the ADR 0128 / `otp-support` release gate, as ADR 0131
§5's `tryDo:/2` does.

### 5. What §6 becomes

ADR 0131 §6's rule, "a Tier 2 block value with no return channel is a
compile error", stands; what changes is the set with no channel. After this
ADR it is: I10 (FFI, warned, exempt), I11 (escapes, permanent), I12 for a
`Dictionary` receiver, I13 and I14, and a send whose callee is statically
known not to thread (§4 row 2). A send to an unknown receiver is **not** an
error: it compiles to `send_threaded/4`. The §6 validator keeps reading
`callee_threads_block_param`, now over every class in the unit and the
stdlib index rather than the enclosing actor only, and the agreement test
(every Tier 2 argument the §6 pass accepts lowers to a producer, every one
it rejects does not) extends to the new producer.

### 6. Overrides, inheritance, sealing

A twin is resolved on the receiver's actual class, so an inherited HOM
threads through its inherited twin, and an override is reached like any
override. The one new hazard is an override that drops the twin: `Base
twice:` threads, `Sub twice:` stores its block, and `Sub twice: [t := t +
1]` through a `Base`-typed receiver raises at run time where `Base` would
have threaded. The compiler reports it where it can see it: a method that
overrides one with a twin for position `i`, and has none, gets a
`twin-dropped-in-override` warning naming the superclass, suppressible with
`@expect twin_dropped_in_override`. It is a lint, not a guarantee, exactly
as ADR 0130's `class-state-abroad` is. Sealing changes nothing about
twins: BT-3755 required `sealed` only because the actor channel needed the
callee compiled in the same pre-scan; a metadata-resolved twin does not.

### 7. Class variables, processes, and the hop

A Tier 2 block handed to a class method of a class with `classState:` or a
method that is not `class sealed` runs in that class's process (ADR 0109).
Its map is a value: it travels in the `class_method_call` message and back
in the reply, so **the outer-local write survives the hop**. The block's
class-variable rules are ADR 0130's and unchanged: a write to its home
class's variables from the foreign process raises `class_state_unreachable`,
a read answers the captured value. Both are documented together in
`beamtalk-language-features.md` § Passing Blocks Through Class Methods. A
direct-called class method's twin runs in the caller's process and needs
neither hop nor copy.

### REPL session

```beamtalk
> Object subclass: Twice
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
ERROR: stateful_block_dispatch: 'keep:' on Keeper does not thread its block
  hint: return the new value from the block and assign it where it is called
> t
2
```

The last error is the runtime path (§4 row 3): through a variable the
compiler cannot see `Keeper`, so the send resolves at run time, finds no
twin, and raises before the block runs. `t` is untouched either way; nothing
is dropped silently.

### Error examples

```beamtalk
Object subclass: Base
  class sealed twice: b => b value. b value

Base subclass: Sub
  classState: saved = nil
  class twice: b => self.saved := b         // warning: twin-dropped-in-override
```

```text
warning: `twice:` overrides `Base twice:`, which threads its block parameter `b`, but this method stores it
  = note: a caller passing a block that writes a local will get `stateful_block_dispatch` here where `Base` threads it
  = help: invoke `b` instead of storing it, or add `@expect twin_dropped_in_override`
```

## Prior Art

- **Pharo / Squeak.** Any method can run a block and the caller's temps
  update, because a written temp is a heap cell shared by the block (a temp
  vector). This is the semantics users expect and the one ADR 0041 promised.
  BEAM has no shared cell; the twin reproduces the observable behaviour by
  returning the cell's contents instead.
- **Erlang / Elixir.** A closure captures values; nothing it does rebinds
  its creator's variable. Elixir 1.7 made even `if`/`case` rebinding local.
  This is the honest version of "keep §6 forever" (Alternative D below):
  nobody misses the feature in a language that never offered it. Beamtalk
  offered it.
- **Haskell `State`.** `s -> (a, s)` is the twin's signature, and "lift a
  function into the monad" is what generating a twin does. ADR 0041 cited
  it for the block; this ADR applies it to the method that runs the block.
- **Rust `FnMut`.** A callee's *signature* says whether it accepts a
  mutating closure (`FnMut`) and the compiler guarantees the writes are
  visible after the call; a callee that stores the closure needs a
  different bound. The twin is an inferred, not declared, `FnMut`-accepting
  signature, and §1's "threading parameter" is the `FnMut` / stored-closure
  distinction. Rust's choice to make it part of the type is the strongest
  argument for Alternative F (a declared `threads` annotation).
- **Swift.** Closures are non-escaping by default and must be marked
  `@escaping` to be stored; captured `var`s are shared boxes. BT-3756's
  escape rule (I11) is Swift's distinction with the default inverted: a
  block that escapes cannot be stateful here, rather than needing a mark.
- **Koka / effect handlers.** A function that runs an effectful closure is
  effectful in its row, and the compiler specializes bodies per effect
  instantiation. "One body, two entry points" (Open Question 4) is that
  specialization; the twin is the monomorphic instance.
- **Gleam.** No mutation; `use` sugar and explicit accumulators make the
  data flow visible. It is what §6's help text has always pointed users at,
  and what the twin does mechanically so the user does not have to.
- **Pony / Dart isolates.** A closure sent to another actor or isolate
  cannot mutate the sender's locals. §7's rule for the class-process hop is
  the same boundary, with the map-as-value making the write survive where
  Pony would forbid the send.
- **Within Beamtalk.** `beamtalk_result:'tryDo:'/2` (ADR 0131 §5) is the
  first twin. ADR 0129 Phase 0b's `direct_class_methods` is the metadata
  resolution pattern. ADR 0128's opaque-callable fold is the in-callee
  lowering for forwarded blocks. ADR 0118 and 0131's producers are the
  caller-side lowering. Nothing here is a new kind of mechanism; it is the
  four existing ones applied to the one boundary they stopped at.

## User Impact

- **Newcomer.** `Twice twice: [t := t + 1]` does what it looks like it
  does, in a class, a value type, an actor and the REPL. The error they
  can still hit says why the callee cannot thread ("stores its block") and
  what to write instead. A runtime `stateful_block_dispatch` through a
  variable receiver is new to them; it names the class and selector and
  gives the same rewrite.
- **Smalltalk developer.** The last large departure from "a block can
  write my temps" goes away for compiled callees. Two remain and are
  documented: a block that escapes the method cannot write (I11), and a
  raise inside a protected region discards its writes (ADR 0131). The
  override lint is the price of not having shared cells: in Pharo an
  override that stores the block simply works; here it raises.
- **Erlang/BEAM developer.** One extra exported function per HOM with a
  threading parameter, `'__t2_<sel>'/n+1`, and one metadata key. Calling
  a Beamtalk HOM from Erlang with a plain fun is unchanged (the plain
  function). Hand-writing a native HOM that should thread means adding an
  arity-`n+1` clause the way `'tryDo:'/2` did, with a conformance fixture
  (`docs/development/erlang-guidelines.md` gains the rule).
- **Production operator.** No runtime protocol change for existing calls;
  the twin is additive, hot reload of mixed modules degrades to a
  structured error at the one send that needs a missing twin, never a
  crash or a wrong answer. Code size grows by one function per threading
  HOM (§Consequences). Observable with standard tools: a twin is a named
  function in the module, visible in `observer`, `recon` and stack traces.
- **Tooling developer.** The twin is invisible to the AST and the type
  system (ADR 0041 §Interaction with Gradual Typing: `StateAcc` is not part
  of a block's type, and the twin is not part of a method's). The §6
  diagnostic, the "why" help text and the override lint are semantic
  analysis, so the LSP reports them as you type. Completions and hover
  show the plain selector only.

## Steelman Analysis

### A. Per-method threading twins, metadata-resolved (chosen)
- 🧑‍💻 **Newcomer**: "I never see it. The program that looked right now runs right, and the error I can still get tells me what the callee does with my block."
- 🎩 **Smalltalk purist**: "This is the closest BEAM can get to a temp vector: any compiled method that runs my block hands my temps back. The escape rule is the only thing I give up, and I give it up to the VM, not to the compiler's convenience."
- ⚙️ **BEAM veteran**: "It's `tryDo:/2` for every HOM: one extra export, one metadata key, the gen_server reply shape untouched, and mixed-module reload degrades to a structured error. Erlang callers keep the plain function."
- 🏭 **Operator**: "Additive. No relup, no wire change. The failure mode under skew is an error naming the selector, not a dropped write I find in a ledger three weeks later."
- 🎨 **Language designer**: "It is the fourth application of a mechanism that already exists (producers, families as data, metadata resolution, the native twin), not a fifth mechanism. `MethodBody` finally has a builder, and the verifier obligations are the same shape as ADR 0131's."

### B. Universal caller-side protocol (every block-taking method takes `StateAcc`, ADR 0041 §Caller-Side Protocol as written)
- 🎩 **Smalltalk purist**: "One convention for every method is simpler to explain than 'some methods have a twin'."
- 🎨 **Language designer**: "No predicate, no metadata, no runtime resolution: the calling convention *is* the channel."
- ⚙️ **BEAM veteran**: "Every HOM pays a tuple and a map parameter on every call, pure block or not, which ADR 0041's two-tier design was written to avoid; every native and every Erlang caller of a Beamtalk HOM changes arity; and under hot reload an old caller and a new callee disagree on arity, so both arities must exist anyway during migration, which is Option A with extra steps."
- **Why not:** it is A plus a mandatory migration of every block-taking method and every external caller, for no additional coverage. ADR 0131 Alternative D's objection was aimed at exactly this reading.

### C. Mutable reference cell (a `make_ref()`-keyed process-dictionary cell for each threaded local, cleaned up on return, NLR and exception)
- 🧑‍💻 **Newcomer**: "It's how every other language does it."
- 🎩 **Smalltalk purist**: "A temp vector by another name; the callee needs no twin at all."
- **Why not:** rejected in ADR 0041 (Alternative A) and ADR 0128 (Option 1) and the reasons have not changed: a class-process callee runs the block in *another* process, where the cell does not exist; an escaping block would read a dead cell; cleanup under `$bt_nlr`, exceptions and `ensure:` is a new runtime mechanism the `ThreadedIr` verifier cannot see; and 100–500 ns per access against the map's ~15 ns. The one thing it would buy over A, threading through a native that cannot have a twin, is I9, which A handles per native.

### D. Keep ADR 0131 §6 as the permanent answer (the issue asks for this steelman)
- 🏭 **Operator**: "Zero runtime change, zero hot-reload story, zero new error at run time. The compiler already rejects the shape and the LSP already shows it. Every program that compiles today keeps compiling."
- ⚙️ **BEAM veteran**: "Elixir never had this and nobody writes `Enum.each` with a mutated outer variable; they fold. The help text already says `inject:into:`."
- 🎨 **Language designer**: "Explicit accumulators are better code. §6 nudges people to write `t := Twice twice: [t + 1]`, which is honest about the data flow."
- 🧑‍💻 **Newcomer**: (no strong case: the error is at a shape they wrote by analogy to the `do:` three lines above, which worked.)
- 🎩 **Smalltalk purist**: (no case: it bans the ordinary Smalltalk idiom and breaks ADR 0041's documented promise.)
- **Why not as the end state:** the asymmetry it leaves is the problem. The same `self hom: [t := t + 1]` works in an actor and errors in a value type; it works for a `sealed` same-class callee and errors for the inherited one; ADR 0127's trait provisions (`Enumerable` via `self do:`) thread on a value type and not on an actor. A rule a user cannot predict from the source is worse than either "always" or "never". D also does not fix I5 and I6, which have no callee at all. **Adopted as the floor:** §6 stays wherever the compiler can prove no channel (§5), and Phase 0 ships before any twin so no phase makes a silent wrong answer.

### E. Call-site specialization (inline a statically known, sealed, same-unit callee so the block becomes a §1 construct; ADR 0128 Option 4, BT-3610's recommendation)
- 🏭 **Operator**: "No runtime change at all, and the hot path is as fast as an inlined `do:`."
- 🎨 **Language designer**: "It reuses ADR 0131's producers directly, with no new family."
- **Why not:** it only covers callees in the same compilation unit that are not recursive, so I3, I8, I9 and every stdlib HOM stay errors; a `^` inside the inlined callee changes meaning (its NLR target becomes the caller's method); code size grows per call site rather than per method; and the "which callee" question still needs §4's resolution for every receiver that is not a literal class. Kept as a possible optimization of A for the same-unit case (Open Question 4), not as the mechanism.

### F. A declared annotation (`class sealed twice: aBlock threads: aBlock =>` or a `@threads` pragma) instead of an inferred predicate
- ⚙️ **BEAM veteran**: "Rust makes `FnMut` part of the signature for a reason: the override hazard in §6 becomes a type error instead of a lint."
- 🎨 **Language designer**: "Explicit is better than inferred for something that is part of a method's contract."
- 🎩 **Smalltalk purist**: "Smalltalk has no such annotation; a method that runs a block runs it."
- 🧑‍💻 **Newcomer**: "I would not know to write it, and the error telling me to would be the §6 error with extra steps."
- **Why not now:** inference costs the user nothing and the predicate is already one place; an annotation would be a second source the predicate must agree with. The override hazard is real but is the ADR 0130 `class-state-abroad` class of hazard, handled the same way (a lint). If the lint proves insufficient, an annotation can be added on top of the same predicate as a *constraint* ("this method must have a twin") without changing the mechanism. Deferred, Open Question 3.

### Tension points
- Operators and BEAM veterans split between A and D: D is free today, A is the only option that removes the actor/class asymmetry. Phase 0 and the structured runtime error are what let an operator accept A.
- Smalltalk purists prefer C's semantics and reject its mechanism once the class-process hop is explained; A is their second choice and the only one that survives the hop.
- Language designers are split between A and F. The predicate-as-one-source rule decides it for A now and leaves F as an addition.
- Everyone agrees B is A with a mandatory migration, and that E is at most an optimization.

## Alternatives Considered

### B, C, D, E, F
See the Steelman Analysis.

### Reuse the `State` family for the twin's carrier instead of adding `Acc`
Smaller diff (the REPL root already threads an opaque map as `State`).
Rejected in §2: `State` carries actor-specific meaning and a verifier rule
whose whole purpose is to keep it out of class and value-type methods.

### Extend the BT-912 actor channel to class and value-type methods by giving them a `State` parameter
This is B restricted to compiled Beamtalk methods: it changes the arity of
every class and value-type method, direct-called ones included, and so
every runtime caller (`class_send/3`, `dispatch/3`, ADR 0084 funs, the
supervisor paths). Rejected for the same reason as B.

### A twin per (method, parameter) rather than per method
`'__t2_0_twice:'`, `'__t2_1_each:do:'`, one for each threading position
and one for each combination a caller might use. Rejected: the
combinatorics are unbounded for a method with several block parameters,
and the Tier 1 wrapper in §1 costs one tuple per call, which is within the
ADR 0041 budget.

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
  becomes the single source for the twin, the diagnostic, the metadata and
  the lint; the verifier gains checks that fail on the pending fixtures
  before the fix.
- The `tryDo:/2` precedent becomes the rule for natives, so a hand-written
  HOM that should thread has a documented, fixture-checked shape.

### Negative
- **Code size.** One extra function per HOM with a threading parameter,
  with its own body. A rough count: 112 of the stdlib's 621 `.bt` method
  headers name a block-like parameter, and most of those are §1 constructs'
  own implementations or pure forwarders; the Phase 1 measurement counts
  the twins actually emitted and the `.beam` size delta. If it is
  material, Open Question 4 (one body, two entry points) is the fallback.
- **Build time.** Lowering the twin body is a second lowering of the
  method. Phases 1 and 2 each measure against the ≤3 % gate with ADR 0131's
  methodology; a twin is emitted only for methods that have a threading
  parameter, so the common method costs nothing.
- **A new runtime error at an unknown receiver.** A Tier 2 block sent
  through a variable to a callee with no twin raises where today it is a
  compile error. The error is structured, names the fix, and fires before
  the block runs; but it is reachable in production in a way the compile
  error was not. Phase 0 ships the error before any twin exists.
- **The override lint is not a guarantee** (§6), as `class-state-abroad`
  is not. A subclass that drops a twin breaks a caller at run time.
- **A new family.** `Acc` is a fourth `VersionPrefix`; `verify()`,
  `render()`, the hand-built IR fixtures and `eligible_families` gain cases
  for it.
- **Metadata grows** by one key per module, read on class registration and
  reload; `class_send/3`'s cache gains a sibling.
- **The class-process hop copies the map twice** (in the message, in the
  reply). The map holds only the block's written locals, so it is small,
  and the hop already copies the arguments and the result.
- **Two channels in actors until Phase 3.** The BT-912 `State` channel and
  the twin coexist for different sends; the agreement test keeps them from
  overlapping, and Phase 3 retires the first.

### Neutral
- The FFI boundary (I10) stays lossy with a warning; an Erlang callee can
  never return a map it does not know about.
- Escapes (I11), `Dictionary` `at:ifAbsentPut:`/`removeKey:ifAbsent:`
  (I12), cascade/interpolation/`match:` subjects (I13) and the keyword
  `whileTrue:` condition (I14) are unchanged and remain ADR 0131's.
- The REPL display for the probe shapes is the transcript in §REPL
  session, approved by this ADR as ADR 0131 approved its own (CLAUDE.md §
  REPL output); any other display change needs its own sign-off.

## Implementation

| Phase | Scope | Proof | Size |
|---|---|---|---|
| 0 | **Pins, the floor, and the wire check.** Move the four `adr0131section6callee` pending fixtures and the class/value-type twins of I1–I7 into `PIN-BUG` BUnit files over all three method kinds and the REPL. Add `tier2_methods` to `'__beamtalk_meta'/0` (empty) and `beamtalk_dispatch:send_threaded/4` raising the precise `stateful_block_dispatch`. Wire check: one hand-written Erlang twin (`'__t2_twice:'/3` in a test module with a hand-written `tier2_methods` entry) reached through `send_threaded/4` from a compiled class method whose Tier 2 block is packed and unpacked by hand in the fixture, proving the metadata lookup, the twin call and the map round trip before any codegen exists. Record baseline build time and `.beam` sizes. | Every pin red as a compile error or the structured runtime error; the wire check green; no silent wrong answer. | S |
| 1 | **Class and value-type twins, direct shapes.** `VersionPrefix::Acc`, `ScopeKind::Twin`, `eligible_families` for twin scope; the `MethodBody` builder; `callee_threads_block_param` generalized to the §1 direct-invocation, callable-argument and pass-on uses in every class of the unit; twin emission and metadata; `callee_threaded_send` producer (§3) with static direct calls in-unit and `send_threaded/4` otherwise; the §6 narrowing (§5) with "why" help; I5 (stored Tier 2 block `value` in class/value-type) as the inline `value` producer over a fresh map. Verifier: `TwinAccumulatorNotReturned`, `Tier2ParamAppliedWithoutAccumulator`, `ActorStateInClassMethod` scoped so `Acc` is the only opaque family in a twin. | I1, I4, I5, I7 pins green in class and value-type; the agreement test extended; `just verify-threaded-ir`; `local_touch` corpus gains a `hom_call` shape; build-time and size measurement. | L |
| 2 | **Callee frames.** Tier 2 by containment; the parameter invoked in arms, loop and fold bodies, `on:do:`/`ensure:`, evaluated blocks and nested closures (I2); I6 (ADR 0128's opaque fold seeded from `Acc` in class, value-type and REPL context, the "designed then" ADR 0128 left open). | I2, I6 pins; corpus `hom_call` nested under every construct; `.core` diff harness lists the expected diffs. | M |
| 3 | **Processes and actors.** The threaded `class_method_call` variant through `invoke_class_method/7` (I8); actor twins as `dispatch/4` and `'__sealed_'` clauses with the `{Result, Acc}` reply; I3 through runtime resolution; retire `generate_tier2_self_send_open`'s `State` packing and `tier2_method_info` in favour of the producer (Open Question 1); the override lint (§6). | I3, I8 pins; `hom_composability_actor.bt`, `self_send_hom_pool.bt`, `tier2stored_block_matrix_actor.bt` unchanged; the BT-3755 exemption in `local_threading.rs` deleted. | M |
| 4 | **Natives, on demand.** For each Erlang-implemented HOM that should thread: an arity-`n+1` clause sharing the body (the `'tryDo:'/2` shape), a hand-written `tier2_methods` entry in the native facade's metadata, and a `.bt` conformance fixture asserting both arities agree on the raising and non-raising paths. The Erlang guideline. | Per native. | S each |
| 5 | **Close-out.** Docs (`beamtalk-language-features.md` § What Works and What Doesn't, § Passing Blocks Through Class Methods; `docs/agents/expanded.md` § State-Threading Codegen; `debugging.md` verifier table; `erlang-guidelines.md`); ADR 0131 §6 and ADR 0128 §"Explicitly narrowed" amended to point here; flip every pin; final measurement; Status to Implemented. | CI. | S |

Dependency order: 0 → 1 → 2 → 3; 4 and 5 after 3. Phase 1 lands with the
verifier checks and the producer together; shipping a twin without the
producer, or the producer without the §6 narrowing, would reintroduce a
silent path.

Affected components: `beamtalk-core` (`semantic_analysis/tier2_callee.rs`,
`block_facts.rs` for containment, `validators/local_threading.rs`, the
override lint), `beamtalk-codegen` (`threaded_ir/{ir,verify,build}.rs`,
`generator/version.rs` for `Acc`, `threaded_expr.rs` for the producer,
`gen_server/methods.rs`, `value_type_codegen.rs`, `dispatch_codegen.rs`,
`class_meta.rs`, `spec_codegen.rs` for the twin's `-spec`,
`control_flow/list_ops/opaque_fold.rs`), the runtime
(`beamtalk_dispatch.erl` `send_threaded/4`, `beamtalk_class_dispatch.erl`,
`beamtalk_object_class.erl` for the threaded class call,
`beamtalk_error.erl`), stdlib natives per Phase 4, BUnit fixtures,
`tests/repl-protocol/cases/`.

**Shared-kernel check.** The twin predicate has one home,
`tier2_callee::callee_threads_block_param`, read by the §6 validator, codegen
and the metadata emitter; the runtime reads the emitted metadata and
re-derives nothing (the ADR 0129 pattern). Tier 2 by containment lives beside
`captured_mutations_for_block` in `block_facts`. The caller-side lowering is
an ADR 0131 producer and reuses `threaded_locals_of`; no second threaded-set
walk. `Acc` goes through ADR 0122's families-as-data, not a per-construct
slot. The Rust ↔ Erlang boundaries are two: the metadata shape (a Rust
emission test plus an Erlang test that `send_threaded/4` honours a compiled
module's map and raises on a module without it, both over one `.bt` fixture)
and each native twin's conformance fixture, as for `tryDo:/2`.

## Migration Path

No correct program changes behaviour. Programs that are ADR 0131 §6 errors
today compile and run once their callee has a twin. The one new way a
program can fail is the runtime `stateful_block_dispatch` through an unknown
receiver whose callee does not thread; such a program was a compile error
before, so nothing that compiled before this ADR regresses. A hand-written
Erlang HOM that users pass stateful blocks to gains a twin clause in Phase 4
or raises the structured error. Modules compiled before this ADR need no
change and keep working as callees of old callers; recompiling them adds
their twins.

## Open Questions

1. **Retire the BT-912 actor channel.** After Phase 3 every actor send has
   a twin path. `generate_tier2_self_send_open` packs into `State` and
   needs the callee in the same pre-scan; the twin does not. The default is
   to retire it in Phase 3 once the three actor fixtures named there pass
   on the twin alone, and to keep it only if the measured cost of the
   fresh-map pack on a same-process self-send is material.
2. **Runtime-installed class methods (ADR 0084).** A `put_class_method/4`
   fun has no twin and no metadata; `send_threaded/4` raises for it. Whether
   `ClassBuilder` grows a twin form (a fun of arity `n+2` registered beside
   the `n+1` one) is left to a need.
3. **A declared annotation on top of the predicate** (Alternative F) if the
   override lint proves insufficient in practice.
4. **One body, two entry points.** If Phase 1's size or build-time
   measurement is over budget, the plain function can become a wrapper that
   calls the twin with an empty map and drops the result, at the cost of a
   per-invocation `is_function/2` gate in the one body (ADR 0128's
   per-element gate) and the loss of the Tier 1 zero-overhead path through
   that method. Measured, not decided.
5. **Which natives first.** The candidates are the Erlang-implemented HOMs
   users pass stateful blocks to in practice; `Future`, `Timer` and every
   asynchronous callee are never candidates (the block runs after the
   caller's frame is gone, which is I11's rule at run time).

## References
- Related issues: BT-3742 (this ADR), BT-3755 (the actor exemption and its
  four pending probes), BT-3756 (escapes), BT-3610 / BT-3583 (the actor
  self-send HOM gap, ADR 0128), BT-912 (the actor channel), BT-853 / BT-852
  (ADR 0041 Tier 2 caller and callee sides), BT-3738, BT-3743 (ADR 0131
  epic), BT-3841 (Phase 0 diagnostic retirement), BT-3813, BT-3839
- Related ADRs: ADR 0041 (universal block protocol; §Caller-Side Protocol,
  §Erlang Interop Boundary, §State-Carrying NLR), ADR 0006 (dispatch),
  ADR 0084 (runtime class-method funs), ADR 0109 (blocks run in the class
  process), ADR 0111 (verifier), ADR 0118 (`ThreadedValue`), ADR 0122
  (families as data), ADR 0125 (releases and skew), ADR 0127 (traits),
  ADR 0128 (opaque-callable folds; §"Explicitly narrowed"), ADR 0129
  (direct-called class methods, metadata resolution), ADR 0130 (class
  variables), ADR 0131 (outer-local producers; §5, §6, Alternative D,
  Open Question 1)
- Code: `crates/beamtalk-core/src/semantic_analysis/tier2_callee.rs`,
  `crates/beamtalk-core/src/semantic_analysis/validators/local_threading.rs`
  (`check_send`), `crates/beamtalk-codegen/src/core_erlang/dispatch_codegen.rs`
  (`generate_tier2_self_send_open`, `scan_class_for_tier2_blocks`),
  `crates/beamtalk-codegen/src/core_erlang/blocks.rs`
  (`generate_erlang_interop_wrapper`),
  `crates/beamtalk-codegen/src/core_erlang/class_meta.rs`,
  `runtime/apps/beamtalk_runtime/src/beamtalk_class_dispatch.erl`
  (`class_send/3`), `runtime/apps/beamtalk_stdlib/src/beamtalk_result.erl`
  (`'tryDo:'/2`),
  `stdlib/test/fixtures/adr0131section6callee_actor_pending.bt.pending`,
  `stdlib/test/fixtures/hom_composability_actor.bt`,
  `stdlib/test/fixtures/self_send_hom_pool.bt`
- Documentation: `docs/beamtalk-language-features.md` § Control Flow and
  Mutations, § Passing Blocks Through Class Methods;
  `docs/agents/expanded.md` § State-Threading Codegen;
  `docs/development/architecture-principles.md` §6, §7
