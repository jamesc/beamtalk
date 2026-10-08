# ADR 0131: Outer-Local Rebinds as `ThreadedValue` Preludes in Every Context

## Status
Proposed (2026-10-08)

## Context

### Problem statement

A block that writes a local of its enclosing method is ordinary Smalltalk:

```beamtalk
count := 0
r := ([count := count + 1. 1] on: Error do: [:e | 0]) + 1
```

ADR 0041 promised this works "in all blocks". It works reliably only when
the threading construct (the loop, list-op, conditional or `on:do:`/`ensure:`
whose block writes `count`) is a **statement**. As the **right-hand side of
a local assignment** it works for the constructs someone has wired (loops,
list-ops, `ifTrue:ifFalse:`, `on:do:`) and not for others (`ifNil:`,
`detect:ifNone:`, `at:ifAbsent:`; second table below). Anywhere else, in
**operand position** (a receiver, an argument, a binary operand, a `^` value
inside an arm, a field or class-variable write's value, an element of a
literal), the outer-local write is broken, in every method context.

BT-3738 was filed as "threading constructs in operand position inside class
methods leak their `{Value, StateAcc}` tuple". Reproducing it showed it is
neither class-method specific nor limited to a tuple leak. A 13-shape probe
(one class or value or actor per shape, each run as a BUnit test on the debug
build, 2026-10-08):

| # | Body (after `t := 0`) | Expected | Class method | Value-type method | Actor method |
|---|---|---|---|---|---|
| o1 | `r := ([t := t + 1. 1] on: Error do: [:e \| 0]) + 1` | `#[2, 1]` | `Tuple` DNU `+` | `Tuple` DNU `+` | `Tuple` DNU `+` |
| o2 | `r := ([t := t + 1. 1] ensure: [t := t + 1]) + 1` | `#[2, 2]` | `Tuple` DNU `+` | `Tuple` DNU `+` | `Tuple` DNU `+` |
| o3 | `r := (4 =:= 5 ifTrue: [2] ifFalse: [t := t + 1. 1]) + 1` | `#[2, 1]` | unbound `State` (erlc) | unbound `State` (erlc) | **`#[2, 0]`, write dropped** |
| o4 | `r := (#(1, 2) collect: [:x \| t := t + x]) size` | `#[2, 3]` | **`#[2, 0]`** | **`#[2, 0]`** | **`#[2, 0]`** |
| o5 | `self.n := [t := t + 1. 1] on: Error do: [:e \| 0]` | `#[1, 1]` | tuple stored in `n`, `t` dropped | pass | pass |
| o6 | `cond ifTrue: [^#[([t := t + 1. 1] on: Error do: [:e \| 0]), t]]` | `#[1, 1]` | tuple in result | tuple in result | tuple in result |
| o7 | `r := [c ifTrue: [2] ifFalse: [([t := t + 1. 1] on: …) + 1]. 7] on: Error do: [:x \| 3]` | `#[7, 1]` | **`#[3, 0]`** | **`#[3, 0]`** | **`#[3, 0]`** |
| o8 | as o7 without the trailing `7` | `#[2, 1]` | **`#[3, 0]`** | **`#[3, 0]`** | **`#[3, 0]`** |
| o9/o10 | o1 as `r :=` / as `^` | | `Tuple` DNU | `Tuple` DNU | `Tuple` DNU |
| o11 | statement-position conditional, then `[t := t + 1] ensure: [nil]` | `#[1, 2]` | pass | pass | pass |
| o12 | `self.n := (c ifTrue: [2] ifFalse: [t := t + 1. 1]) + 1` | `#[2, 1]` | unbound `State` | unbound `State` | **`#[2, 0]`** |
| o13 | `self.n := c ifTrue: [2] ifFalse: [t := t + 1. 1]` | `#[1, 1]` | `value` via `perform:` raises | pass | pass |

Bold cells are **silent wrong answers**: the program runs and returns a
value that is not what the source says. Only o11, the statement-position
control, passes everywhere.

A second probe (same method, same day) over other block-taking selectors,
all in **assign-RHS or statement position**, shows the per-position wiring
is incomplete even there:

| # | Body (after `t := 0`) | Expected | Class | Value-type | Actor |
|---|---|---|---|---|---|
| s1 | `r := d at: #k ifAbsent: [t := t + 1. 0]` | `#[0, 1]` | `function_clause` crash | crash | crash |
| s2 | `r := #(1, 2) detect: [:x \| x > 5] ifNone: [t := t + 1. 0]` | `#[0, 1]` | **verifier panic** (`verify.rs`) | unbound `State` | **`#[0, 0]`** |
| s4 | `b := [t := t + 1]. b value. t` (stored closure) | `1` | `value` via `perform:` raises | raises | pass |
| s6 | `(Erlang lists) map: [:x \| t := t + 1. x] with: #(1)` | lossy by design | pass (write dropped, ADR 0041 warning) | pass | pass |
| s7 | `r := nil ifNil: [t := t + 1. 1]` | `#[1, 1]` | arity crash | arity crash | pass |
| s9 | `r := (#(1, 2) reject: [:x \| t := t + 1. false]) size` | `#[2, 2]` | **`#[2, 0]`** | **`#[2, 0]`** | **`#[2, 0]`** |
| s3/s5/s8 | `inject:into:` RHS, `ifTrue:ifFalse:` RHS, statement `do:` | | pass | pass | pass |

s4 contradicts `beamtalk-language-features.md`'s "local mutation in stored
closure works via Tier 2": it works only in an actor instance method. s2
in a class method is the one shape today's `ThreadedIr` verifier *does* see,
and it reports it as a panic, not a diagnostic.

Two related failures share the cause:

- **`Result tryDo:`** with a block that writes an outer local raises
  `tryDo: expected a zero-arity block, got: Block/1` in every context, in every
  position, including as a plain statement. The block is compiled as a Tier 2
  `fun (StateAcc) -> {V, StateAcc1}` and passed to the native
  `beamtalk_result:'tryDo:'/1`, which only accepts arity 0. (The BT-3718
  fix made the codegen accept the block. It did not make the runtime call it.)
- **A Tier 2 block passed to any non-intrinsic send outside an actor
  instance method** (`CvA ap: [t := t + 1. 1]` where `class ap: b => b value`)
  raises `Cannot call 'value' on Block via perform:` at the first `value`.
  Only actor self-sends carry a `StateAcc` back (BT-912, via the gen_server
  `State`). Class and value-type methods have no return channel for it.

The `local_touch` shape of the ADR 0130 class-variable agreement corpus
(`crates/beamtalk-core/src/test_helpers/class_var_program.rs`), which adds
`t := t + 1` to the head of every nested block, measures the same thing
end-to-end. At `CV_CORPUS_CASES=300`, **147 of 300 programs fail (49 %)**:
39 do not compile (all `unbound variable 'State'` / `'StateN'` / `'StateAcc'`
in `class_run` or a `hN:` helper), and 108 answer wrong in all three
spellings (most raise at runtime, from a leaked tuple, `tryDo:` arity or a
`perform:` on a Tier 2 block; 36 return the raw `{V, #{'__local__t' => …}}`
tuple). In-process codegen with the `ThreadedIr` verifier passes all 1000
programs drawn: **the verifier sees none of this**, because the broken
shapes never reach the IR as IR.

### Why it keeps recurring

The outer-local family is threaded by **position**, not by construct. Each
position that works has its own hand-written rebind:

| Position / mode | Where the rebind lives today |
|---|---|
| Last / `^` (class, value-type) | `lower_threaded_last` (drops element 2) |
| Assign-RHS (class, value-type) | `emit_threaded_assign_rhs` → `emit_vt_threaded_local_assignment`, `emit_vt_conditional_assign_rhs`, `emit_vt_exception_assign_rhs` |
| Assign-RHS (actor) | `emit_actor_threaded_assign_rhs_stmts` + `push_threaded_var_rebinds` |
| Loop body, `StateAcc` mode | `generate_local_var_assignment_in_loop` + `push_control_flow_threaded_var_rereads` |
| Loop body, direct-params / hybrid | `lower_direct_var_update_in_loop_bind` (`direct_params_list_op_result` side channel) |
| Loop body, plain `let` fast path | `try_generate_block_local_plain_let`, with four "fall back if…" guards (Tier 2 call, control-flow-with-mutations, BT-3493 field write, REPL) |
| Nested value-type `do:` | `lower_nested_vt_do` write-back (BT-3718) |
| `ensure:` cleanup | `rebind_threaded_vars_from_state` (BT-3718) |

Every row is a fix for a position someone hit (BT-912, BT-2342, BT-2349,
BT-2358, BT-2814, BT-3162, BT-3428, BT-3492, BT-3493, BT-3718). A position
with no row (a receiver, an argument, a binary operand, a field write's
value, a `^` inside an arm) gets the construct's raw `{Value, StateAcc}`
tuple. The BT-3718 agent found that a fix in one row "fixed one corpus program
and broke another" for exactly this reason: whether a rebind should be a
plain `let`, a `maps:put` into `StateAcc`, a `Gensym` rebind of a direct
parameter or a slot in a tuple accumulator depends on the **enclosing
frame's** threading mode, and a producer running inside an expression has
no way to ask.

This is the problem ADR 0118 solved for actor self-sends ("every one of the
ten recent fixes is the same patch applied to a new syntactic position"). ADR
0118 made a state-effecting expression return a `ThreadedValue` (a prelude of
`ThreadedStmt`s plus a pure value) that every consumer splices. It did that
for the `State`, `SelfVt` and (since removed by ADR 0130) `ClassVars`
families. It never did it for outer locals: `inline_control_flow_producer`
does wrap conditionals as producers, but it extracts only the actor `State`
family (`actor_conditional_families()` is hard-coded to `[State]`). That is
why o3/o12 reference an unbound `State` in class and value-type methods, and
why in an actor the `__local__t` key comes back in `State` while the Core
Erlang variable `T` is never re-read from it.

### Constraints

- ADR 0041: the `{Result, StateAcc}` calling convention for Tier 2 blocks
  stays. Locals stay immutable Core Erlang variables. There is no process
  dictionary or mutable cell.
- ADR 0111 / CLAUDE.md state-threading rule: the fix must be general (state
  the invariant for every scope kind), owned by the `ThreadedIr` node that
  owns the scope, with a `VerifyError` that fails on the repro first and
  proof over a generated corpus. No per-call-site sync, refresh or commit
  step.
- ADR 0118 §Decision 3: source evaluation order is a language guarantee.
  `(items at: i) + ([t := t + 1] on: …)` must raise from `at:` before the
  block runs.
- ADR 0122: families are data (`ThreadedFamilies`). A site declares which
  families it can carry and rejects the rest through the shared rejection
  function.
- ADR 0130: class variables are not a threading family. Nothing here touches
  them, and the `OnDoCatch` class-variable restore stays as it is.
- ADR 0111's ≤3 % build-time gate applies.

## Decision

**An outer-local write inside a threading construct is a state effect of
that construct's expression, in every position and every context. Every
local-threading construct is a `ThreadedValue` producer. Its prelude carries
one `LocalRebind` node per threaded local. The enclosing frame, not the
producer, decides how a `LocalRebind` is lowered. The per-position rebind
paths are deleted.**

### 1. Every local-threading construct is a producer

`threaded_expression` gains one producer, `local_threading_producer`, ahead
of its existing cases. It recognizes every construct that returns a
`{Value, StateAcc}` (or flat `{Value, L1, …, Ln}`) tuple for threaded outer
locals:

- loops (`whileTrue:`/`whileFalse:`/`timesRepeat:`/`to:do:`/`to:by:do:`/
  `repeat`), foldl list-ops and `do:`, and the block-taking lookup selectors
  (`detect:ifNone:`, `at:ifAbsent:`, `at:ifAbsentPut:`, …) that take a
  block literal;
- the ADR 0128 opaque-callable folds, **in actor instance context only**:
  ADR 0128 records that a non-literal callable cannot thread captured
  locals in class or value-type context at all (its §"Explicitly narrowed");
  there the fold is a §6 case, not a producer;
- read+write conditionals (`ifTrue:` … `match:`; this is
  `inline_control_flow_producer` with its family list taken from context,
  §3);
- `on:do:` and `ensure:`;
- `Result tryDo:` (§5);
- an inline Tier 2 `value`/`value:`… call on a block-valued local.

Its result is:

```text
prelude: [ ConstructTuple { carrier: CF, doc: <construct tuple>, threads: ["t", …] },
           LocalRebind { local: "t", carrier: CF, slot: Key("__local__t") | Pos(k) },
           … one per threaded local, in `threads` order … ]
value:   element(1, CF)
```

**One recognizer, one set.** Today the threaded set is computed by two
functions that do not agree on coverage: `get_control_flow_threaded_vars`
(loops and list-ops; delegates to `compute_threaded_locals_for_loop`, which
returns *empty in REPL mode* and does not recognize `tryDo:`, a Tier 2
`value` call, or a lookup selector) and `conditional_threaded_locals`
(conditionals and `on:do:`/`ensure:`). This ADR merges them into one
`threaded_locals_of(expr) -> Option<ThreadedLocals>` in
`threading_analysis.rs`, which is both the producer's recognizer (`Some`
means "this construct threads") and the set every tuple builder packs
from. It covers every bullet above, in every context including the REPL
(whose set is the bindings the construct writes). A gate that says "this
threads" can therefore never pair with a smaller write-back set: this is
the BT-3738 acceptance criterion for `lower_nested_vt_do`, generalized.

**Transitive closure.** A construct's set includes the sets of every
producer nested anywhere in its blocks (o7/o8: the outer `on:do:` must
carry `t`, written only inside a conditional arm's inner `on:do:`). Today
`compute_threaded_locals_for_loop` adds nested list-op and loop writes but
not every nested producer kind. `threaded_locals_of` is defined as the
closure, and a verifier check (`ThreadedLocalDropped` below, applied at
each nesting level) catches a construct whose frame rebinds a local its
own `threads` does not list.

A construct whose threaded set is empty is not a producer, and nothing
changes for it.

### 1a. Sequencing: a local read before a sibling rebinds it

ADR 0118 §3's sequencing rule binds every earlier sibling that is "not a
literal or plain variable" to a temp before a later sibling's prelude runs.
That exemption for plain variables was safe because, until this ADR, no
prelude could change a local. Now one can: in

```beamtalk
t + ([t := t + 1. 1] on: Error do: [:e | 0])
```

the left operand `t` must read the value *before* the right operand's
`LocalRebind`. Source order (and Pharo) gives `0 + 1`. The rule is
amended: a plain-variable sibling is trivial only if **no later sibling's
threaded set contains it**; otherwise it is snapshot to a temp like any
other value. `sequence_children` has the later siblings' sets in hand
(it already calls `subexpr_needs_prelude` on each), so this is a local
change to the exemption test, not a new pass. The verifier backs it:
`VerifyError::LocalReadAfterSiblingRebind` rejects a spliced value that
reads local `x` by its post-rebind identity when a `LocalRebind` for `x`
sits in a *later* sibling's prelude. The shape is added to the probe and
to the `local_touch` corpus generator (a `Touch` on the left of a binary
op whose right operand is a protected block).

### 2. `LocalRebind` is lowered by its frame

`ThreadedStmt` gains:

```rust
/// ADR 0131: the outer local `local` takes the value its construct threaded
/// out, read from `carrier` at `slot`. How the new value is bound is decided
/// by the enclosing frame's recorded mode, never by the producer.
LocalRebind { local: String, carrier: String, slot: CarrierSlot, frame: FrameId, span: Span },
```

`ThreadedStmt::Threaded { mode, frame, .. }` already records each loop or
conditional frame's resolved `ThreadingMode` (`DirectParams`,
`TupleAcc(g)`, `Hybrid`, `StateAcc(reason)`) **in the IR**. This ADR adds
the two frame kinds that have no node today, as nodes rather than as a side
table: `ThreadedStmt::MethodBody { frame, threads, body }` (the method's
root frame; `threads` is empty except in the REPL, where it is the bindings
map's keys) and `ThreadedStmt::BranchArm { frame, threads, body }` (one
conditional or handler arm, whose `threads` are the locals the arm's closer
packs into its `__local__` `StateAcc` keys, `seed_conditional_locals`'s
set). A `LocalRebind`'s frame is then structural: its mode and its frame's
`threads` are read off the enclosing node by the lowering and by the
verifier alike, with nothing to push and pop. This is the "shared way for a
producer to learn the enclosing frame's threading mode" that BT-3738 asks
for: the producer does not learn it. It emits a mode-free node, and the
node's lowering looks up the enclosing frame.

**The lowering key is (frame mode × membership).** A frame's mode says how
its *own* threaded locals travel; it says nothing about a local the frame
does not thread. A nested producer can rebind a local that is not in the
enclosing frame's `threads` (a temp declared inside a loop body, a
method-level local the frame's own analysis did not list), and that local
must not be forced into the frame's parameter list or its `StateAcc` map
(in an actor, a stray `__local__` key would persist into the gen_server
`State`, the BT-2717 class of bug). So:

| Enclosing frame mode | `local` ∈ frame `threads` | `local` ∉ frame `threads` |
|---|---|---|
| `MethodBody` (any context but REPL) | n/a (`threads` is empty) | `let T1 = <read> in`, `bind_var(t, T1)` |
| `MethodBody` (REPL) | `Bind { State_{n+1} ← Put("t", <read>) }` into the bindings map | n/a |
| `StateAcc(_)` loop / handler body | `Bind { State_{n+1} ← Put("__local__t", <read>) }` + `bind_var` | plain `let` + `bind_var` |
| `DirectParams` / `Hybrid` | `Bind { Gensym(T1) ← Direct(<read>) }`, joining the existing `final_loop_arg_identities` chain | plain `let` + `bind_var` |
| `TupleAcc(g)` | as `DirectParams`; the fold's closing tuple picks up the new identity | plain `let` + `bind_var` |
| `BranchArm` | `Bind { State_{n+1} ← Put("__local__t", <read>) }` into the arm's seeded `StateAcc` | plain `let` + `bind_var` |

`<read>` is `maps:get(Key, element(2, CF))` for a `StateAcc`-carrying
construct and `element(k, CF)` for a flat-tuple one. The construct reports
which (`CarrierSlot`), because the construct built the tuple. In an actor
instance frame `element(2, CF)` is also the `State` family's next version;
the `State` `Bind` (ADR 0122's `extract_family_slots`) is emitted **first**,
and each `LocalRebind` then reads from that new `State` version, so one
carrier read feeds both.

With this in place, these become ordinary consumers that splice a prelude,
and are deleted: `emit_threaded_assign_rhs` and its three `emit_vt_*`
helpers, `emit_actor_threaded_assign_rhs_stmts`'s raw-tuple path,
`push_threaded_var_rebinds`, `push_control_flow_threaded_var_rereads`, the
`direct_params_list_op_result` side channel, the "control-flow-with-mutations"
guard in `try_generate_block_local_plain_let`, `lower_nested_vt_do`'s
write-back and `rebind_threaded_vars_from_state`.

**Last position does not discard, except at the method root.** A
construct in last position of a *loop body, branch arm, Tier 2 block body
or `on:do:` body* must still rebind: its locals are that frame's threaded
output (`[…] whileTrue: [([t := t + 1] on: Error do: […])]` carries `t`
out of the loop body through the rebind). Only a `MethodBody` frame's last
expression may drop its rebinds, and it records that with an explicit
`DiscardLocals { carrier }` node so the choice is visible to the verifier.
`lower_threaded_last` becomes that one consumer.

**Non-local return.** A `^` thrown from inside a producer's construct
leaves the method, so the construct's `LocalRebind`s never run; that is
correct, and the NLR 4-tuple's state slot carries what the method's catch
needs (ADR 0041 §State-Carrying Non-Local Returns, unchanged). The
`ThreadedLocalDropped` check below is defined on the fall-through path and
ignores the throw path.

### 3. Families are taken from context, not hard-coded

`inline_control_flow_producer` and the conditional builders take their
`ThreadedFamilies` from the context (ADR 0122's `eligible_families`): `[State]`
in an actor instance method, `[SelfVt]` in a value-type method when it
writes a field, and **no `State` family** in a class method or a value-type
method that writes no field. In those, the scratch slot is the local
`StateAcc` map, seeded from `~{}~`, as ADR 0122 §2 already specifies. This
removes the unbound-`State` class (o3, o12 and 39 corpus programs) at its
source, and it is what BT-3725's `ActorStateInClassMethod` check was written
to catch. BT-3725's rule is extended to value-type methods (`ScopeKind`
gains `ValueType`): a value-type method never references the actor `State`
family.

### 4. Verifier obligations

Each check fails on a repro before the fix:

- **`VerifyError::ThreadedLocalDropped { local, carrier }`.** A
  `ConstructTuple` whose `threads` contains `local` must be followed in the
  same frame by a `LocalRebind` for `local` before the frame ends, or
  before any reference to the carrier other than a family `Bind`
  (`State`/`SelfVt` extraction, which legitimately reads the carrier first)
  or a `DiscardLocals` at a `MethodBody` frame. Applied at every nesting
  level, it also catches the transitive-closure gap of §1: a frame that
  rebinds `t` while its own enclosing construct's `threads` omits `t`.
  Fails on o4, o7, o8, s2, s9 and actor o3/o12.
- **`VerifyError::LocalRebindModeMismatch { frame, mode, member, lowered_as }`.**
  A `LocalRebind` lowered to a shape the §2 table does not allow for its
  frame's mode and membership (a plain `let` for a member of a `StateAcc`
  frame, a `Put` for a non-member, a `Gensym` chain entry for a local that
  is not a loop parameter). This covers the BT-3718 `ensure:` rebind and
  nested `do:` write-back, which today are guarded only by a hand-written
  BUnit fixture.
- **`VerifyError::LocalReadAfterSiblingRebind`** (§1a).
- **`ActorStateInClassMethod`** (BT-3725), extended to `ScopeKind::ValueType`.
  Fails on o3/o12 and s2 in value-type methods.
- **`StateEffectEscapesExpression`** (ADR 0118 §5), already defined, now also
  reported for a `LocalRebind` left in a prelude that is closed in an
  `Opaque` context (a Tier 1 closure body or an FFI argument).

`report_threaded_ir_verify_errors` reports all five. Nothing gets a new
`debug_assert!`.

### 5. `Result tryDo:` is a catch-boundary construct

The catch boundary stays in the runtime; codegen only chooses the arity.
`beamtalk_result` gains one clause:

```erlang
%% ADR 0131: the Tier 2 twin of 'tryDo:'/1. `Block` is fun(StateAcc) ->
%% {Value, StateAcc1}. A raise inside the block discards its local writes
%% (the StateAcc the caller passed in is returned) exactly as it discards
%% its class-variable writes (protect/1), and as on:do: does.
-spec 'tryDo:'(fun((map()) -> {term(), map()}), map()) -> {t(), map()}.
'tryDo:'(Block, StateAcc) when is_function(Block, 1) ->
    try beamtalk_class_vars:protect(fun() -> Block(StateAcc) end) of
        {Value, StateAcc1} -> {from_tagged_tuple({ok, Value}), StateAcc1}
    catch
        throw:NLR when ?IS_NLR(NLR) -> throw(NLR);
        Class:Reason:Stack ->
            ExObj = beamtalk_exception_handler:ensure_wrapped(Class, Reason, Stack),
            {from_tagged_tuple({error, ExObj}), StateAcc}
    end.
```

`protect/1`, `?IS_NLR`, `ensure_wrapped` and `from_tagged_tuple` are the
same calls `'tryDo:'/1` makes, so the Tier 1 and Tier 2 paths share one
implementation of the boundary rather than a copy across the Rust/Erlang
line (CLAUDE.md § No duplicate implementations). Codegen, when the argument
is a Tier 2 block literal, emits `call 'beamtalk_result':'tryDo:'(Block,
StateAcc)` and treats the result as a `StateAcc`-carrying construct tuple:
it is a §1 producer exactly like a `StateAcc`-mode loop, with no new
`OnDoCatch` clause shape. `'tryDo:'/1` is unchanged for Tier 1 blocks and
dynamic sends. A runtime ↔ codegen conformance fixture (a `.bt` that
exercises both arities and asserts identical `Result`s, class-variable
state and local state on the raising and non-raising paths) is the
Rust/Erlang-boundary check CLAUDE.md asks for.

This answers each objection that removed the AST lowering from PR #4187:

| #4187 objection | Answer here |
|---|---|
| A final `t := e` was lost in `Result ok: (t := e)` | There is no rewrite: the block body compiles as any Tier 2 body, and the last statement is a statement. |
| The cascade-receiver rewrite | No rewrite. In a cascade `Result tryDo: […]; …` the first message is still a send of `tryDo:` and takes the same arity choice. |
| An `Exception` filter vs a native catch-all | The native catch-all is the only one; nothing is reimplemented in codegen. |
| Handler-parameter aliasing | There is no handler block. |
| "Only helps once operand-position producers exist" | §1 makes the call a producer. |

The arity choice keys on the **selector and a literal Tier 2 block
argument**, not on the receiver being spelled `Result`: a shadowed or
aliased `Result` still gets the right arity, and a non-`Result` receiver
that happens to answer `tryDo:` gets the Tier 2 fun and a plain `{Value,
StateAcc}` protocol, the same as any user HOM (§6).

### 6. A Tier 2 block with no return channel is a compile error

Outside an actor instance method there is no channel through which a callee
can return a `StateAcc`. So a **Tier 2 block value** (a block literal that
writes an outer local, or a local bound to one and not reassigned) that
flows to a send that is neither a §1 construct nor an actor self-send is
rejected at compile time:

```text
error: block writes outer local `t`, but `ap:` cannot return the write
  ┌─ cv_a.bt:5:16
5 │     r := CvA ap: [t := t + 1. 1]
  │                  ^^^^^^^^^^^^^^^ `t` is written here
  = help: return the new value from the block and assign it: `t := CvA ap: [t + 1]`,
          or use a control-flow message (`do:`, `inject:into:`, `on:do:`) that threads locals
```

The same diagnostic, pointing at the binding, covers `b := [t := t + 1].
CvA ap: b` and a stored closure invoked directly (`b value`, s4): ADR 0128
already records that forwarding such a block to `do:`/`collect:`/`select:`
in class or value-type context **silently drops the write**, so the value
case is a wrong answer today, not only a crash. This replaces today's
runtime `perform:` failure and the native-callee arity crash. The same send
compiled from an actor instance method keeps working (BT-912).

**Exempt:** an Erlang FFI argument (`(Erlang lists) map: [:x | t := t + 1.
x] with: …`, s6). ADR 0041 §Erlang Interop Boundary defines that as lossy
by design, with a warning, and `generate_erlang_interop_wrapper` already
implements it; this ADR keeps it.

**What it costs.** A Tier 2 block value that is stored but never invoked
through a channel-less send (`d at: #k put: [t := t + 1]`, a block only
asked `numArgs`) compiles and runs today and will be rejected. The
alternative, proving the block is never invoked, needs escape analysis the
compiler does not have. The rule is accepted as stated: the write in such
a block could never have taken effect, so the program's author almost
certainly meant something else, and the help text says what.

**One predicate, in `beamtalk-core`.** The "is a §1 construct" test and
this diagnostic must agree, or the LSP accepts code that fails at runtime
(or rejects code that compiles). Both are computed from
`state_threading_selectors` and the block facts in
`semantic_analysis` (`block_analyzer`, `block_facts`), which codegen
already consumes; the §1 recognizer `threaded_locals_of` is implemented
over those facts, not over a codegen-private table. The agreement is
checked by a corpus test: every Tier 2 argument the §6 pass accepts must
lower to a producer, and every one it rejects must not. It is a
`#beamtalk_error{}`-shaped diagnostic with a structured code, so the LSP
reports it as you type. Giving class and value-type HOMs a real return
channel (a callee-side `{Result, StateAcc}` protocol) is out of scope and
stays an open question (below).

### REPL and error examples

```beamtalk
> t := 0
0
> r := ([t := t + 1. 1] on: Error do: [:e | 0]) + 1
2
> t
1
> (#(1, 2) collect: [:x | t := t + x]) size
2
> t
4
> (Result tryDo: [t := t + 1. 1 / 0]) isError
true
> t                                   // the protected region raised: its write is discarded
4
```

A local written inside a protected region whose block then raises keeps the
value it had on entering the region. This is what `on:do:` does today in
every context (checked 2026-10-08), and it matches ADR 0130 §4's rule for
class variables. `Result tryDo:` keeps the same rule, which is why §5's
catch arm returns the `StateAcc` it was given. **This is a deliberate
departure from Pharo**, where temps are shared cells and a write before
the raise survives: on BEAM the protected region's state is a value that
is simply not returned. It is documented as such in
`beamtalk-language-features.md` § Control Flow and Mutations (phase 5).

The REPL threads locals through its bindings map, the outermost `StateAcc`
(ADR 0041); its root frame is the `MethodBody (REPL)` row of §2's table.
The REPL is a fourth column of the probe matrix and gets
`tests/repl-protocol/cases/` coverage for every shape, because REPL
display is covered by e2e tests and any change to it needs sign-off
(CLAUDE.md § REPL output). The values shown above are what the shapes
*should* print; whether a shape's display changes at all is recorded per
phase.

## Prior Art

- **Pharo / Squeak.** Blocks close over method temporaries by reference
  (heap-allocated "temp vectors" for any temp a block writes). Operand
  position is not special because the write is to a shared cell. Beamtalk
  cannot do this on BEAM without a mutable cell (ruled out by ADR 0041/0042),
  so it must thread. This ADR makes threading as position-independent as a
  cell is.
- **Erlang / Elixir.** No mutable locals. Elixir's compiler lowers to Core
  Erlang in A-normal form, where every compound sub-expression is bound by a
  `let` before use. Elixir's own block rebinding rule (variables rebound
  inside `if`/`case` do not leak out since 1.7) is the restriction this ADR
  rejects as user-facing semantics (§Alternatives C). It is fine for Elixir
  because Elixir never promised Smalltalk block semantics.
- **Gleam.** No mutation. `use` and explicit accumulator threading make the
  data flow visible in the source. This is the shape §6's help text points
  users to when no channel exists.
- **Compilers generally (SSA construction).** A write inside a structured
  construct becomes a φ at the construct's exit, placed by the dominance
  structure of the enclosing region, not by the instruction that produced the
  value. `LocalRebind` lowered by its frame is that idea: the frame (region)
  decides the φ's form.
- **Within Beamtalk.** ADR 0118 is the direct precedent, the same move for
  the `State` family. ADR 0122 supplies the families-as-data machinery this
  reuses.

## User Impact

- **Newcomer.** Code that looks right stops returning tuples or wrong
  numbers. The one new error (§6) names the variable, the send, and a
  rewrite. Before, they got `Tuple does not understand '+'`, which points
  nowhere near the cause.
- **Smalltalk developer.** `x := (coll inject: 0 into: […]) + (… on: … do:
  [count := count + 1])` behaves as in Pharo on the non-raising path. Two
  departures, both because BEAM has no shared cell: §6 (in Pharo a user HOM
  can write a caller's temp; the error says so, and the actor case still
  works), and the protected-region rule (a write inside an `on:do:` or
  `tryDo:` block that then raises is discarded; Pharo keeps it). Both are
  documented in the language reference.
- **Erlang/BEAM developer.** Generated Core Erlang stays ANF-shaped `let`
  chains. A rebind is a visible `let T1 = maps:get(…)`. Nothing new reaches
  the runtime except `Result tryDo:`'s inline `try`, which calls the same
  runtime functions as before.
- **Operator.** A class of silent wrong answers becomes either correct or a
  compile error. Hot code reload is unaffected (no runtime protocol change).
  The ≤3 % build-time gate applies (one `Vec` per producing expression, as in
  ADR 0118).
- **Tooling developer.** §6 is a semantic-analysis diagnostic, so the LSP
  reports it as you type. No AST changes.

## Steelman Analysis

### B. AST-level ANF hoist (rewrite each operand-position construct to `__tN := …` ahead of its statement)
- 🧑‍💻 **Newcomer**: "Same visible result, sooner."
- ⚙️ **BEAM veteran**: "It's what Elixir's compiler does: ANF at the source level, simple and predictable, and it reuses the assign-RHS path that already works."
- 🎨 **Language designer**: "It's a smaller diff, and one pre-pass instead of a new IR node."
- **Why not:** to keep evaluation order it must hoist every earlier sibling too, which re-implements ADR 0118's sequencing rule outside the IR. It adds no verifier obligation, so the next position bug is silent again. Synthetic names leak into diagnostics and the LSP. And the assign-RHS path it would reuse is itself broken in some contexts (o5/o13 in class methods), so it inherits those bugs instead of fixing the cause.

### C. Reject the shape at compile time ("an outer local written inside an operand-position construct is an error")
- 🏭 **Operator**: "A compile error beats a silent wrong answer, and it's an S-sized change I can have this week."
- 🎩 **Smalltalk purist**: (no strong case: it bans ordinary Smalltalk.)
- ⚙️ **BEAM veteran**: "Elixir made this exact choice in 1.7 and nobody misses the old behaviour."
- **Why not as the end state:** it contradicts ADR 0041's documented guarantee and the language docs ("local mutations work in all blocks"). The rule is about *position*, which users don't think in. And it still leaves `Result tryDo:` broken in statement position. **Adopted as Phase 0** (below): ship the diagnostic for the shapes not yet migrated, then remove it phase by phase as producers land. That way nothing is silently wrong in between.

### D. Give every HOM a callee-side `{Result, StateAcc}` protocol (fix §6 instead of rejecting)
- 🎩 **Smalltalk purist**: "This is the real Smalltalk semantics: any method can run my block and my temps update."
- 🎨 **Language designer**: "One protocol everywhere, so §6 disappears."
- **Why not now:** it changes the calling convention of every block-taking method, including stdlib natives and Erlang FFI, and needs a migration of compiled code (hot reload of mixed old/new modules). That is its own ADR. This ADR doesn't block it, because §6's error is exactly the set of call sites that protocol would light up.

### E. Lower `Result tryDo:` to an `OnDoCatch` node in codegen (the first draft of §5)
- 🎨 **Language designer**: "`tryDo:` *is* `on:do:` with a catch-all and a `Result` wrapper. One IR node for both means one verifier obligation for both."
- 🏭 **Operator**: "No runtime change means no runtime release to coordinate."
- **Why not:** the catch sequence (`protect/1`, the NLR pass-through, `ensure_wrapped`, `from_tagged_tuple`) would exist twice, once in Erlang for the Tier 1 arity and once as Core Erlang emitted by Rust, with "same runtime sequence" as the only thing keeping them equal. CLAUDE.md requires a shared implementation or a conformance fixture for a rule that crosses the Rust/Erlang boundary. A second runtime clause (§5) shares the implementation and still needs only the fixture. It also keys on the selector and arity rather than on the receiver being spelled `Result`. Rejected on the duplication rule; the review that found this is what changed §5.

### Tension points
- Operators favour C for speed. Smalltalkers and language designers favour A for correctness. Phasing C in as Phase 0 of A gives both.
- BEAM veterans find B more familiar. The deciding argument against it is verifiability, which is the lesson of ADR 0111 and 0118.
- Language designers are split on E vs §5. "One IR node" is elegant; "one implementation of the boundary" is what the project's duplication rule asks for, and it wins.

## Alternatives Considered

### A narrow class-method fix (BT-3738 as filed)
Guard on frame, `in_loop_body`, direct params and hybrid loop in the
class-method producer only. The BT-3738 agent tried each guard. Each fixed
one corpus program and broke another, and the same shapes stay broken in
value-type and actor methods (table above). Rejected under CLAUDE.md's "a
state-threading fix is general or it is not a fix".

### B, C, D, E
See the Steelman Analysis.

### Do nothing (keep wiring positions as they are reported)
Ten issues over eight months (BT-912 … BT-3718) each wired one more
position. The second probe table shows the wiring is still incomplete in
assign-RHS and statement position after all of them, and the `local_touch`
corpus shows a 49 % failure rate that none of the per-position fixes
moved. Rejected: the per-position approach has had its chance.

## Consequences

### Positive
- One mechanism for outer-local writes in every position and every context.
  About ten per-position paths (§2's list) are deleted, and the two
  threaded-set functions become one.
- The probe shapes and the `local_touch` corpus become verifier-checked.
  A future position bug is a `VerifyError`, not a wrong answer.
- `Result tryDo:` works with stateful blocks in every position, and stored
  closures (s4) get a correct error instead of a `perform:` crash.
- BT-3725's class-method `State` rule gets its value-type twin, and the
  unbound-`State` class goes away at its source.
- `local_touch` can join `Shapes::ENABLED`, which closes the last gap in the
  ADR 0130 agreement property.

### Negative
- L–XL across six phases, touching every control-flow lowering. Each phase
  runs under ADR 0122's `.core` diff harness and **lists** its expected
  diffs rather than claiming none: §3 changes the conditional families in
  class and value-type methods (statement position included), §1a adds a
  temp bind to some binary operands, and deleting the
  "control-flow-with-mutations" guard can change a loop's mode selection
  (`StateAccFallbackReason::ControlFlowMutations`). A diff outside the
  listed set fails the phase.
- §6 rejects code that compiles today. Nearly all of it fails at runtime
  or silently drops a write (ADR 0128), so no *correct* program breaks;
  but a Tier 2 block that is stored and never invoked compiles and runs
  today and will be rejected (§6 "What it costs").
- Two new IR node kinds (`MethodBody`, `BranchArm`) and a `ConstructTuple`
  variant with a payload: more IR surface for `verify()`, `render()` and
  the hand-built fixtures to cover.
- `beamtalk_result` gains a clause, so this is a runtime change, with the
  version-skew implications any runtime change has (a compiled module that
  emits the arity-2 call needs a runtime that has it; the stdlib is built
  with the compiler, so this is only a concern for externally compiled
  packages, handled by the ADR 0128/`otp-support` release gate).
- The protected-region rule ("a raise discards local writes made inside
  it") is now documented language semantics and a deliberate departure
  from Pharo, where it was previously only an undocumented consequence of
  codegen.

### Neutral
- The REPL root frame is `MethodBody (REPL)` in §2's table; REPL cases are
  added for each shape and display changes are recorded per phase.
- The NLR path is unchanged: a `^` from inside a producer's construct
  bypasses its rebinds by design (§2).

## Implementation

| Phase | Scope | Proof |
|---|---|---|
| 0 | **One** temporary diagnostic, not one per shape: a construct with a non-empty `threaded_locals_of` set in any position other than statement or assign-RHS of a wired construct is a compile error, driven by an explicit allow-set of `(construct, position, context)` that later phases grow. Plus the §6 diagnostic (block literals and values, FFI exempt). Add the probe matrix (o-, s- and the §1a shape) as a BUnit file over all three method contexts and as `repl-protocol` cases, with `PIN-BUG` assertions citing this ADR's epic. | Probe file red → compile-error on every bold cell. No silent wrong answer remains. S-sized; ships first. |
| 1 | `threaded_locals_of` in `threading_analysis.rs` over `semantic_analysis` block facts, replacing `get_control_flow_threaded_vars`/`compute_threaded_locals_for_loop`/`conditional_threaded_locals` (transitive closure, REPL-aware). IR: `ConstructTuple`, `LocalRebind`, `DiscardLocals`, `MethodBody`, `BranchArm` nodes; `ThreadedLocalDropped`, `LocalRebindModeMismatch`, `LocalReadAfterSiblingRebind`, `ScopeKind::ValueType`. Verifier unit tests on hand-built IR for each, failing before phase 2. The §1a sequencing amendment. | Unit tests. `just verify-threaded-ir` clean. Byte-identical `.core` (phase 1 adds nodes but no producer). |
| 2 | `local_threading_producer` for loops, list-ops, `do:`, lookup selectors and (actor-only) opaque folds, with the §2 per-local lowering rule. Delete the assign-RHS and loop-body rebind paths for them. | o4, s1, s2, s9, corpus `do`/`fold`/`while`/… with `local_touch`. `.core` diffs limited to the listed set. |
| 3 | Conditionals and `match:` families from context (§3). `on:do:`/`ensure:` as producers. Delete `lower_nested_vt_do` write-back and `rebind_threaded_vars_from_state`. **This is BT-3738.** | o1–o3, o5–o13, s7 in all contexts. |
| 4 | `beamtalk_result:'tryDo:'/2` and its conformance fixture; codegen arity choice (§5). | `tryDo:` probes. `local_touch` + `try_do` corpus. |
| 5 | Close-out: `local_touch` into `Shapes::ENABLED`, un-`#[ignore]` both properties, `CV_CORPUS_CASES=500` green, remove Phase 0's allow-set diagnostic, flip every `PIN-BUG`, docs (`beamtalk-language-features.md` "What Works and What Doesn't" and the protected-region rule, `docs/agents/expanded.md` § State-Threading Codegen, `debugging.md` verifier table), build-time measurement. | CI. |

Phases 2 and 3 land **with** the §2 per-local rule and the §1a sequencing
fix already in from phase 1; shipping either producer without them would
reintroduce the "fixed one program, broke another" pattern.

Affected components: `beamtalk-codegen` (`threaded_ir/{ir,verify,emit,build}.rs`,
`util.rs`'s `threaded_expression`/`sequence_children`,
`threading_analysis.rs`, `control_flow/*`, `threaded_expr.rs`,
`blocks.rs`, `value_type_codegen.rs`, `gen_server/methods.rs`),
`beamtalk-core` (`semantic_analysis/{block_analyzer,block_facts}.rs` for §6
and the facts `threaded_locals_of` reads, `test_helpers/class_var_program.rs`),
`beamtalk_stdlib` (`beamtalk_result.erl`, §5), stdlib tests,
`tests/repl-protocol/cases/`.

**Shared-kernel check.** The threaded-local set has exactly one source after
phase 1, `threaded_locals_of`, built over `semantic_analysis` block facts so
the §6 pass in `beamtalk-core` and the producer in `beamtalk-codegen` read
the same facts (the dependency direction core ← codegen is preserved).
Families come from ADR 0122's `ThreadedFamilies` / `eligible_families`. No
new selector table is introduced: the recognizer reuses
`beamtalk_core::state_threading_selectors`. The `tryDo:` boundary has one
implementation (`protect/1` in the runtime) and a conformance fixture
across the Rust/Erlang line.

## Migration Path

No *correct* program changes behaviour. Programs that today fail at runtime
or answer wrong either become correct (phases 2–4) or get a compile error
(§6 and the phase 0 allow-set diagnostic). The §6 error's help text gives
the rewrite. The one working shape §6 rejects, a Tier 2 block value stored
and never invoked through a channel-less send, needs its block rewritten to
return the value rather than write the local.

## Open Questions

1. **Callee-side Tier 2 protocol for class and value-type HOMs** (Alternative
   D). This needs its own ADR. Until then §6 stands.
2. Should a `LocalRebind` whose local is dead after the construct be elided?
   The verifier exemption would need a liveness fact the IR does not carry
   today. The default is to emit it (correctness first) and measure.

## References
- Related issues: BT-3738 (becomes phase 3), BT-3718, BT-3725, BT-3737, BT-2717,
  BT-3694, BT-912, BT-3493
- Related ADRs: ADR 0041, ADR 0111, ADR 0118, ADR 0122, ADR 0128, ADR 0130
- Documentation: `docs/beamtalk-language-features.md` § Control Flow and
  Mutations; `docs/agents/expanded.md` § State-Threading Codegen;
  `docs/development/debugging.md` § ThreadedIr verifier
