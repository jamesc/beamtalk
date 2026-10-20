# ADR 0131: Outer-Local Rebinds as `ThreadedValue` Preludes in Every Context

## Status
Accepted (2026-10-08)

## Implementation Tracking

**Epic:** [BT-3743](https://linear.app/beamtalk/issue/BT-3743) (closed 2026-10-17 at a boundary); open follow-ups: [BT-3883](https://linear.app/beamtalk/issue/BT-3883)
**Status:** All phases merged. The Phase 0 position diagnostic is retained (see Remaining work at close-out); the ADR's Status line is unchanged.

| Phase | Issue | Description | Size | PR |
|---|---|---|---|---|
| 0a | [BT-3744](https://linear.app/beamtalk/issue/BT-3744) | Pin the probe matrix as BUnit, repl-protocol and `local_touch` corpus shapes | S | [#4209](https://github.com/jamesc/beamtalk/pull/4209) |
| 0b | [BT-3745](https://linear.app/beamtalk/issue/BT-3745) | §6 Tier 2 block-value error; one allow-set error for unmigrated shapes | M | [#4210](https://github.com/jamesc/beamtalk/pull/4210) |
| 1a | [BT-3746](https://linear.app/beamtalk/issue/BT-3746) | `threaded_locals_of`: one transitively-closed, REPL-aware recognizer | M | [#4223](https://github.com/jamesc/beamtalk/pull/4223), [#4230](https://github.com/jamesc/beamtalk/pull/4230) |
| 1b | [BT-3747](https://linear.app/beamtalk/issue/BT-3747) | IR nodes `ConstructTuple`, `LocalRebind`, `DiscardLocals`, `MethodBody`, `BranchArm` | M | [#4211](https://github.com/jamesc/beamtalk/pull/4211) |
| 1c | [BT-3748](https://linear.app/beamtalk/issue/BT-3748) | Verifier checks; `ScopeKind::ValueType`; §1a sequencing amendment | M | [#4237](https://github.com/jamesc/beamtalk/pull/4237) |
| 2 | [BT-3749](https://linear.app/beamtalk/issue/BT-3749) | `local_threading_producer` for loops, list-ops, `do:`, lookup selectors, actor-only opaque folds | M | [#4248](https://github.com/jamesc/beamtalk/pull/4248), [#4256](https://github.com/jamesc/beamtalk/pull/4256), [#4257](https://github.com/jamesc/beamtalk/pull/4257), [#4260](https://github.com/jamesc/beamtalk/pull/4260), [#4264](https://github.com/jamesc/beamtalk/pull/4264), [#4270](https://github.com/jamesc/beamtalk/pull/4270) |
| 2d | [BT-3798](https://linear.app/beamtalk/issue/BT-3798) | `Dictionary at:ifAbsent:` as a producer (a guarded inline lowering); retire the unreachable `OpaqueFold` construct | M | [#4287](https://github.com/jamesc/beamtalk/pull/4287) |
| 3a | [BT-3738](https://linear.app/beamtalk/issue/BT-3738) | Conditional families from context; conditionals as producers (operand sites in the REPL); their arms as `BranchArm` frames in methods | M | [#4279](https://github.com/jamesc/beamtalk/pull/4279) |
| 3b | [BT-3817](https://linear.app/beamtalk/issue/BT-3817) | `on:do:`/`ensure:` as producers; delete the remaining write-back paths; `match:` | M | [#4285](https://github.com/jamesc/beamtalk/pull/4285) |
| 3c | [BT-3822](https://linear.app/beamtalk/issue/BT-3822) | Conditionals as producers at loop/fold body statements; delete `direct_params_list_op_result`; flip `stLoopArm` | M | [#4293](https://github.com/jamesc/beamtalk/pull/4293) |
| 4 | [BT-3750](https://linear.app/beamtalk/issue/BT-3750) | `beamtalk_result:'tryDo:'/2` + conformance fixture; codegen arity choice | S | [#4289](https://github.com/jamesc/beamtalk/pull/4289) |
| 5a | [BT-3751](https://linear.app/beamtalk/issue/BT-3751) | Enable `local_touch`; flip every pin (the allow-set diagnostic is kept: 552 of 733 measured statement shapes still drop writes with it bypassed; see BT-3841) | S | [#4299](https://github.com/jamesc/beamtalk/pull/4299) |
| 5b | [BT-3752](https://linear.app/beamtalk/issue/BT-3752) | Docs, build-time measurement, known limitations | S | this PR |
| follow-up | [BT-3910](https://linear.app/beamtalk/issue/BT-3910) (spec: [BT-3860](https://linear.app/beamtalk/issue/BT-3860)) | A local assignment in value position is a producer (§1, §1a, §2 `CarrierSlot::Whole`, §4 `UnthreadedLocalAssignment`); retire the BT-3864 `ValuePositionLocalAssignment` stopgap; `^t := e` is legal, with the `dead_returned_assignment` lint | M | this PR |
| follow-up | [BT-3841](https://linear.app/beamtalk/issue/BT-3841) | Retire the Phase 0 position diagnostic (`DENY_SET`/`ALLOW_SET`); ADR Status to Implemented | L | open |

Dependency order: 3744 → 3745 → {3746, 3747} → 3748 → 3749 → 3738 → 3817 → 3750 → 3751 → 3752.
Alternative D is tracked separately as [BT-3742](https://linear.app/beamtalk/issue/BT-3742).

Related fixes that landed inside the epic: [#4232](https://github.com/jamesc/beamtalk/pull/4232) (BT-3753, statement-position shapes added to the Phase 0 check), [#4235](https://github.com/jamesc/beamtalk/pull/4235) (BT-3754, `dead_assignment` lint), [#4261](https://github.com/jamesc/beamtalk/pull/4261) (BT-3755, actor self-send exemption), [#4265](https://github.com/jamesc/beamtalk/pull/4265) (BT-3756, escaping blocks).

### Remaining work at close-out

Every phase above is merged and the user-facing behaviour is documented in `beamtalk-language-features.md` § Control Flow and Mutations. The epic's goal, *no silent wrong answer at any point*, holds. The goal *every position and every context lowers through a producer, so the Phase 0 position diagnostic is gone* does not yet hold. The ADR's Status stays **Accepted** until [BT-3841](https://linear.app/beamtalk/issue/BT-3841) closes.

**Boundary (2026-10-17).** [BT-3743](https://linear.app/beamtalk/issue/BT-3743) is closed and its open children moved to [BT-3883](https://linear.app/beamtalk/issue/BT-3883). The epic was planned as ten issues and grew to 57, because every run of the measurement harness surfaced shapes that still drop writes and each one was filed as its own issue; per-issue cycle time was about 2.5x ADR 0130's. The rule from here: a newly measured shape is a row in the checked-in measured tables (`adr0131_statement_measured.tsv`, `adr0131_operand_measured.tsv`) and, where user-visible, a `PIN-BUG` case citing BT-3841, **not** a new issue, unless it is reachable from real stdlib or example code or answers wrong without a compile error. A shape that is a Tier 2 block reaching a higher-order method belongs to ADR 0132's §6 inventory, not here. Items 1 to 10 below keep their issue numbers; their live parent is BT-3883.

What remains, in priority order, verified against the code on `main` at the time of writing:

1. **[BT-3841](https://linear.app/beamtalk/issue/BT-3841): the Phase 0 position diagnostic is retained** (`DENY_SET`/`ALLOW_SET` in `crates/beamtalk-core/src/semantic_analysis/validators/local_threading.rs`, category `UnmigratedLocalThreading`, message "`X` writes outer local `t`, but as <position> in <context> the write is not threaded back yet (BT-3743)"). Phase 5a ([BT-3751](https://linear.app/beamtalk/issue/BT-3751)) bypassed it and ran every shape it rejects on a BEAM: of 733 measured statement shapes, 552 silently answer wrong, 43 fail `erlc`, and 138 answer right (rejected only because the table's rows are coarser than the measurement). Deleting it would turn compile errors into silent wrong answers, so it stays; BT-3841 retires it as the items below make the remaining shapes producers. The Phase 5a table row's "drop the diagnostic" is therefore BT-3841's work, not Phase 5a's. The §6 diagnostic (a Tier 2 block with no return channel) is permanent and is not part of this.
2. **[BT-3833](https://linear.app/beamtalk/issue/BT-3833): three legacy write-back paths are still alive**: the `push_control_flow_threaded_var_rereads`/`rebind_threaded_vars_from_state` pair for a conditional arm that is not a producer but whose nested loop writes an arm-local; `lower_foldl_body`'s write-back for a non-producer nested list-op that writes a local; and the control-flow-with-mutations guard in `try_generate_block_local_plain_let`. The "delete the remaining write-back paths" goal of Phases 3b and 3c is therefore only partly met.
3. **[BT-3839](https://linear.app/beamtalk/issue/BT-3839)** (done): a `Dictionary` lookup whose handler writes an outer local is a producer in a loop or fold body in class, value-type and actor methods (`keys do: [:k | d at: k ifAbsent: [misses := misses + 1. 0]]`), under the same site rule as a conditional (an actor fold body's value-bearing sites stay errors, `is_actor_fold_value_site`). A lookup inside a closure that is not inlined keeps its §6 error ([BT-3862](https://linear.app/beamtalk/issue/BT-3862)), and a `detect:ifNone:` handler write in an actor loop or fold body keeps its `UnmigratedLocalThreading` error (measured `bad` in `adr0131_statement_probes.tsv`; [BT-3861](https://linear.app/beamtalk/issue/BT-3861)); class and value-type methods lower it since BT-3841. The verifier check that a fold body returns its accumulator is [BT-3863](https://linear.app/beamtalk/issue/BT-3863).
4. **[BT-3823](https://linear.app/beamtalk/issue/BT-3823)** (Done, [#4346](https://github.com/jamesc/beamtalk/pull/4346)): REPL conditional arms, `on:do:`/`ensure:` blocks, `Result tryDo:` blocks and evaluated block literals are producer frames (the o7/o8 REPL pins flipped). Still open from it: a construct inside a REPL loop or fold body ([BT-3876](https://linear.app/beamtalk/issue/BT-3876)), `match:` arms as producers ([BT-3875](https://linear.app/beamtalk/issue/BT-3875); a `match:` arm write is the value-position compile error of [BT-3864](https://linear.app/beamtalk/issue/BT-3864)), and a block literal sent a `value` selector of another arity at the REPL root ([BT-3881](https://linear.app/beamtalk/issue/BT-3881)).
5. **[BT-3753](https://linear.app/beamtalk/issue/BT-3753)** (Done, [#4232](https://github.com/jamesc/beamtalk/pull/4232)): added the statement-position rows to the Phase 0 check, so the `DENY_SET` rows it introduced are retired with item 1, not separately.
6. **Plain-closure case ([BT-3813](https://linear.app/beamtalk/issue/BT-3813))**: `^xs do: [...]` inside a plain closure in a class or value-type method is still rejected; the arm and handler cases are done.
7. **[BT-3824](https://linear.app/beamtalk/issue/BT-3824)** (done): `Dictionary` defines `at:ifPresent:`/`at:ifPresent:ifAbsent:`, and their handlers are producers like `at:ifAbsent:` (the present handler's parameter is bound to the found value). `at:ifAbsentPut:` and `removeKey:ifAbsent:` stay §6 errors permanently by decision, since an immutable `Dictionary` has no such behaviour. The three `Dictionary` lookups are producers only for `Dictionary` receivers (inline behind a runtime guard; other receivers raise a structured `type_error`, and a receiver statically known not to be a `Dictionary` is a compile error).
8. **[BT-3835](https://linear.app/beamtalk/issue/BT-3835)** (Done): a conditional producer's `LocalRebind` in a `StateAcc` fold frame emitted a redundant identity `MapPut`. A `MapPut` cell whose read goes through the frame's own current map is now lowered to `RebindLowering::MapHeld` (the value is bound, the map takes no version step), checked by `HeldRebindNotIdentity` (BT-3857).
9. **[BT-3834](https://linear.app/beamtalk/issue/BT-3834)** (Done): a counted loop whose body holds a non-producer conditional with a nested loop writing an arm-local panicked the debug verifier with `StateAccLoopWithoutLocals`. A loop or fold that carries nothing now lowers as a plain loop (§2, BT-3834 amendment).
10. **[BT-3842](https://linear.app/beamtalk/issue/BT-3842)**: Phase 5a review nits (a shared block-scope helper for `arms_lower_as_producer` and `OuterWriteWalker`; a non-`Result` `tryDo:` falls through to a generic internal error).

11. **[BT-3860](https://linear.app/beamtalk/issue/BT-3860) / [BT-3910](https://linear.app/beamtalk/issue/BT-3910)** (Done): a local assignment in value position is a producer (§1 amendment). Like BT-3841, it was part of the bar for moving the ADR's Status to Implemented. The BT-3864 stopgap error (`ValuePositionLocalAssignment`) is retired by it.

Two scope notes. `Result tryDo:` is a producer only for a literal `Result` receiver; any other receiver keeps its dynamic send and a writing block is a compile error (§5 implementation note). The non-REPL method root is still the implicit frame (`RebindFrame::implicit_root`); the REPL root's `MethodBody` node is built only for a prelude that holds a value-position assignment's `LocalRebind` (BT-3910, §2 amendment).

### Build-time measurement

Method per ADR 0111 Addendum 7/10: two separate release binaries (`cargo build --release --bin beamtalk`, `CARGO_INCREMENTAL=0`, built from clean target directories), cold `runtime/apps/beamtalk_stdlib/ebin/` before each run, `beamtalk build-stdlib --quiet --warnings-as-errors` (117 files), user CPU from `getrusage(RUSAGE_CHILDREN)`, one unmeasured warm-up per side, then alternating baseline/HEAD per round to spread drift evenly. Baseline is `e50454b4a`, the commit immediately before Phase 0a (#4209); HEAD is `9a504d421` (Phase 5a, #4299), the last code change of the epic (this PR is docs only). Linux, 4 vCPU shared VM, so the noise floor is high (see Addenda 3/6/7).

| | user CPU (s) | wall-clock (s) |
|---|---|---|
| baseline, run 1 (mean of 5) | 10.31 (range 10.01-10.79) | 8.35 |
| HEAD, run 1 (mean of 5) | 10.61 (range 10.31-10.81) | 8.05 |
| Δ run 1 | **+2.92%** | -3.56% |
| baseline, run 2 (mean of 10) | 10.20 (range 10.02-10.58) | 7.46 |
| HEAD, run 2 (mean of 10) | 10.27 (range 9.95-10.59) | 7.32 |
| Δ run 2 | **+0.61%** | -1.88% |
| Δ pooled (15 rounds per side) | **+1.4%** | |

The gate is <= 3% user CPU, and both runs are inside it. The first run (the 5-round mean the methodology prescribes) landed at +2.92%, close to the limit, and every one of its five rounds had HEAD at or above baseline; a repeat with 10 rounds measured +0.61%, with overlapping ranges, so the first figure is not reproducible beyond the environment's noise. The honest reading is a small cost of roughly 0.5-1.5% user CPU, not distinguishable from noise at this sample size, and within the gate. Wall-clock is lower on HEAD in both runs, which is noise rather than a speed-up.


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
| s6 | `(Erlang lists) map: [:x \| t := t + 1. x] with: #(1)` | lossy by design | pass (write dropped, ADR 0041 warning) | pass | pass (compile error since BT-3917, ADR 0133) |
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
  there the fold is a §6 case, not a producer (**Amendment, Phase 2d,
  BT-3798:** no user program reaches the actor case either, so it is not a
  producer and codegen has no construct for it. A stored block is promoted
  to a Tier 2 local only when every later use is a `value`-family send, and
  passing it to a collection higher-order method (`#(1, 2) do: blk`) is an
  unsafe use; §6 additionally rejects `blk` flowing to `do:`/`collect:`.
  If a future ADR makes a stored block passable to one, it needs its own
  `__local__` seeding, designed then);
- read+write conditionals (`ifTrue:` … `match:`; this is
  `inline_control_flow_producer` with its family list taken from context,
  §3);
- `on:do:` and `ensure:`;
- `Result tryDo:` (§5);
- an inline Tier 2 `value`/`value:`… call on a block-valued local.

**Amendment, Phase 2d (BT-3798): the lookup selectors.** The receiver type of
`r := d at: #k ifAbsent: [t := t + 1. 0]` is unknown at compile time: the
selector is implemented by the `Dictionary` primitive, by stdlib methods that
call `block value` themselves (`Ets at:ifAbsent:`) and by any user class, and
an inline lowering is valid for only one of them. Instead of a callee-side
tier protocol (Alternative D, BT-3742), the lookup is a **guarded inline
lowering for a `Dictionary` receiver**, lowered as a conditional whose
condition is a runtime receiver guard
(`beamtalk_map:lookup_for_threading/3`) and whose arm is the handler (a
`BranchArm` frame): a `Dictionary` (an untagged map) answers `{ok, Value}` or
`error`, and any other receiver, including a value-type instance (also a map,
but tagged), raises a structured `type_error` when the handler writes an outer
local, instead of dropping the write. The guard is for receivers whose class is
not known: a receiver statically known NOT to be a `Dictionary` (`self` or
`super` outside a `Dictionary`, a class object, an inferred type that is a
known class other than `Dictionary`, its subclasses and its ancestors, or a
union of such) would raise on every call even when the key is present, so it
keeps the §6 compile error (`receiver_statically_not_dictionary` in
`block_facts`, read by the Phase 0 check; codegen lowers whatever Phase 0
accepts and never sees the rejected programs). The lookups are rows of ONE table in
`beamtalk-core` (`LOOKUP_SHAPES`), read by the Phase 0 check and codegen
through `conditional_arm_blocks`; a row is `dictionary_inline` only when
`Dictionary` implements the selector (a test pins that against the class
hierarchy). `Dictionary` implements `at:ifAbsent:` and, since BT-3824,
`at:ifPresent:` and `at:ifPresent:ifAbsent:`: each row records its handlers'
parameter counts (`block_params`), and the lowering runs a one-parameter
handler in the found arm with the parameter bound to the found value and a
parameterless one in the absent arm, answering what the primitive answers
for a missing handler (the found value, or `nil`). `at:ifAbsentPut:` and
`removeKey:ifAbsent:` stay §6 errors permanently: they assume a mutable
dictionary, which `Dictionary` is not, so it will not define them (users
write `at:ifAbsent:` then `at:put:`) and they have no behaviour to lower. The REPL keeps the §6 error.

**Amendment, BT-3839: lookups in loop and fold bodies.** A lookup is a producer
wherever a conditional is: at the method root, in `BranchArm` frames and at a
loop or fold body's producer sites (`is_loop_body_producer_site`, no lookup
exception), its handlers' `LocalRebind`s lowered by the frame's mode. The one
codegen gap was that a loop had two gates: the lowering threaded a loop when
its threaded set (`loop_threaded_locals`) was non-empty, while the statement
classifier (`control_flow_has_mutations`) asked the block-level mutation
heuristic, which does not see through a lookup's handler (a block argument of a
send it does not inline). An actor `do:` around a handler-writing lookup (in
an arm, or behind a nested loop) was then lowered as a threaded fold that its
statement never unpacked, so the write was dropped. Both now read one rule
(`loop_body_has_carried_mutations`: the threaded set, or the heuristic when the
scope has a map to carry). A lookup
inside a closure that is not inlined (a stored block, a block sent `value:`)
has no frame for the producer and keeps the §6 error.

**Amendment, BT-3841: keyed iteration got the same guarded-inline decision.**
`keysAndValuesDo:` and `doWithKey:` are dictionary folds inlined for a
`Dictionary` receiver only (`Ets` and any class may define either selector).
They reuse the lookup design rather than a second one: the one static rule
(`receiver_statically_not_dictionary`, shared with the lookups) keeps a
permanent §6 `Tier2BlockNoReturnChannel` error for a receiver known not to be a
`Dictionary`, whether the block writes an outer local or object state, and an
unknown receiver goes through the runtime guard
`beamtalk_map:pairs_for_threading/2` (untagged map only, else a structured
`type_error`); a map literal needs neither.

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

**Amendment (BT-3860, BT-3910): a local assignment in value position is a
producer.** A local assignment whose value is used by the expression around
it is a producer: an operand, a receiver (including a conditional's or a
cascade's), an argument, a collection-literal or interpolated element, the
value of another assignment or of `^`, or a `match:` arm value. Examples:
`x := (t := t + 1) + 1`, `#[t, (t := 7), t]`, `(t := t + 1) > 0 ifTrue: [...]`.
It is the local counterpart of the BT-3803 field-assignment producer. Unlike
the other §1 producers it builds no `{Value, StateAcc}` tuple: the assigned
value *is* the carrier. Its prelude is the value's own prelude, then `let V =
<value>` (a `ConstructTuple` threading the one local), then one
`LocalRebind { local: t, carrier: V, slot: CarrierSlot::Whole }` lowered by
the enclosing frame (§2's table). It answers `V`. A statement-position
assignment is unchanged: it keeps its existing lowering and is not a
producer. Neither is an assignment inside a block literal used as a value,
since that assignment is a statement of the block. Which assignments are
value-position is one rule, `value_position_local_assignments` in
`beamtalk-core`'s `block_facts` (a parenthesised statement counts as a
statement where the body's lowering binds it: before the last statement of a
value-type or actor method body, in an inlined arm, and as the last statement
of a method body, whose value nothing reads a local after), computed once per
method body (or REPL input) as `value_position_assignment_spans`; every producer gate
(`subexpr_needs_prelude`, the loop-body and method-root statement gates, the
actor `classify_body_expr`'s conditional-receiver probe, the REPL assembler)
reads that set. A conditional (producer or not) whose receiver holds one
sequences the receiver ahead (`conditional_receiver_assignment_operand`), so
the `LocalRebind` reaches the caller's frame instead of a closed document.
Before this amendment the shape was rejected by the Phase 0
`ValuePositionLocalAssignment` error (BT-3864, this ADR's Alternative C
applied to this shape); that error is deleted.

`^t := e` is legal (maintainer decision, 2026-10-17), with one exception. It
is ordinary Smalltalk, and nothing runs after the return that could read `t`
except an `ensure:` cleanup that encloses the `^`. Codegen does not deliver the
new value to that cleanup: the `^` is a throw, and the cleanup runs from the
`catch` arm over the entry `StateAcc` with no `LocalRebind` of the protected
body's writes. The same holds for the statement form `t := t + 1. ^t`
(measured on a BEAM in class, value-type and actor methods: the cleanup
reads the old `t`, silently). So a `^` out of the protected block of an
`ensure:` (written in that block or in a block literal nested in it) whose
cleanup reads a local, bound outside the `ensure:`, that block writes stays a
compile error (`UnmigratedLocalThreading`, BT-3947 lowers it), reported by
the one recognizer in `validators/local_threading.rs`
(`report_nlr_out_of_ensure`) with the hint to keep the `^` out of the protected
block; `dead_returned_assignment` asks the compiler and stays silent inside
that error's span. The throw can also come from a block value created elsewhere
(BT-3993; measured stale in a class, a value type and an actor): the protected
block is treated as possibly holding a `^` when it sends a `value`-family
message to anything but a block literal (a local, parameter, field, call result
or collection element), or when a local that is not a constant, a parameter or a
field (parentheses unwrapped, cascades included) is the receiver or an argument
of any non-binary send (`b on: Error do: [...]`, `b ensure: [...]`,
`b whileFalse`, `h run`, `self run: b`). A cleanup read counts only for a local
bound outside the `ensure:`, the parameters of enclosing blocks included. The
static view cannot tell which value is a block, so the rule is fully
conservative: it over-rejects (for example `assoc value`, `conn close`, or an
ordinary argument) rather than read a stale local silently. Binary operands and
literal or constant arguments do not count. One route stays open (a residual
for BT-3950): a self-send whose method evaluates a stored `^` block, as in
`[t := 1. self run] ensure: [t printString]` with `run => self.blk value`. The
protected block holds no block value and no `^`, and rejecting every self-send
would be too broad, so the rule does not see it. An `on:do:`
protected body has no such position: a `^` passes through its catch untouched,
no handler runs, and nothing reads `t` afterwards. Everywhere else the write is
dead, and the `beamtalk-lint` rule `dead_returned_assignment` warns and
suggests `^e`, including the non-local form `[:k | ^t := e]`.

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

**Amendment (BT-3910): the value-position assignment is a sibling rebind.**
The sibling-snapshot rule counts the new producer: a value-position `t := e`
puts `t` in its expression's reachable set (`reachable_threaded_locals`), so in
`#[t, (t := 7), t]` the first `t` is snapshot before the rebind and the third
reads the new identity. The answer is `#[0, 7, 7]`, as in Pharo.
`LocalReadAfterSiblingRebind` covers it unchanged.

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

**The REPL root adopts the bindings whole (Phase 2d, BT-3795).** The
`MethodBody (REPL)` row's carrier is the bindings map itself: the REPL
builders return `{Value, Bindings}`, so element 2 *is* the next `State`
version. The producer emits one `State` `Bind` from `element(2, CF)` (the
family case above, no `LocalRebind`, an empty `threads`) rather than one
`Put` per local. A per-key `maps:get` would crash on a binding that the
construct introduces and an empty loop never ran (`(#() collect: [:x | z :=
x]) size`, where `z` must stay undefined), and the statement-level REPL path
adopts element 2 the same way (ADR 0041), so a statement and an operand
cannot disagree. Only a loop or fold nested in an operand is a REPL site
(`repl_operand_sites`): a statement or an assignment's whole value keeps the
REPL's own lowering.

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

**Phase 3c amendment (BT-3822): conditionals are producers in loop and fold
frames.** `is_loop_body_producer_site` (`block_facts`) no longer excludes a
conditional outside an arm: at a statement of a loop or fold body, a
conditional that `conditional_producer_writes` names is a producer, exactly as
an `on:do:`/`ensure:` is (Phase 3b). The one rule is read by the Phase 0
check (`arm_frame_conditional_producer`) and by codegen
(`with_body_producer_sites` → `is_producer_frame_for`). A `Dictionary` lookup
stays a root/arm-frame producer only (§6). The invariant, for the versioned
local after each scope kind:

- *top-level statement / method root*: unchanged (Phase 3a).
- *conditional arm*: the arm is a `BranchArm` frame; a nested conditional in
  it is a producer and rebinds into the arm's seeded `StateAcc`.
- *`Letrec` loop body* (`whileTrue:`/`timesRepeat:`/`to:do:`) and *`Foldl` loop
  body* (`do:`/`collect:`/`inject:into:`/…): the conditional's construct tuple
  carries every local its arms write; the body frame lowers one `LocalRebind`
  per local by its mode (`LoopParam` for direct params / tuple accumulator,
  `MapPut` for a `StateAcc` frame), and the loop's closing recursive call or
  tuple reads the rebound identities, so a write in an arm reaches the next
  iteration and the code after the loop.
- *`on:do:`/`ensure:` arm*, *bare block*, *stored closure*: unchanged; a
  conditional there is not a frame site, so the closure/escape rows decide.
- *NLR boundary*: a `^` inside an arm skips the rebinds by design.

A direct-params or tuple-acc loop or fold frame has no `StateAcc`, so a
conditional producer there keeps a private scratch map (a fresh `maps:new()`
seeded with the construct's own locals), as an `on:do:`/`ensure:` producer does
(`conditional_has_private_scratch`); a `StateAcc` or hybrid frame continues its
own map.

**What Phase 3c deletes, and what it keeps.** The `direct_params_list_op_result`
side channel and the list-op branches that fed it are deleted (a list op in a
direct-params body is a producer, lowered with `with_cleared_direct_params_loop`).
Three write-backs stay (BT-3833), each reachable from a program Phase 0 accepts today and
none from a conditional in a loop or fold body (probed: removing each one
silently dropped the write):

- `push_control_flow_threaded_var_rereads` (and its helper
  `rebind_threaded_vars_from_state`): the arm of a conditional that is not a
  producer (it writes no outer local) whose nested loop or fold writes a local
  *of that arm* (`adr0131cond_non_producer_test.bt`). Deleting it needs the arms
  of every non-producer conditional to be `BranchArm` frames.
- `lower_foldl_body`'s write-back of a non-producer nested list-op that writes a
  local (`anySatisfy:` as a statement of a `do:` body).
- the "control-flow-with-mutations" guard in `try_generate_block_local_plain_let`
  and `generate_local_var_assignment_in_loop`'s unwrap: a construct that returns
  `{Value, State}` but threads no outer local (an actor list-op that only sends to
  `self`).

The verifier obligation for the conditional producer is the existing
`ThreadedLocalDropped` (a construct tuple whose local is not rebound), shown to
fire for conditionals in every loop and fold frame mode by
`local_rebind_verifier_corpus.rs`; the agreement tests
(`local_producer_agreement_conditional.rs`) compare the core rule and codegen
both ways for conditionals in every loop and fold frame, including the
conditionals that are not producers.

**BT-3834 amendment: a loop or fold that carries nothing is not a `StateAcc`
loop.** The gate that sends a loop or fold to its threading lowering (a
`ThreadingPlan`) was the mutation analysis of its body
(`block_arg_needs_threading`) or a non-empty threaded set. The analysis is a
heuristic; when it said "threads" and the threaded set was empty (a write to a
local of an arm, an `ensure:` or an `on:do:` block nested in the body that it
misread as an outer write: BT-3834, BT-3848), the plan fell back to `StateAcc`
with no locals, seeded from the ambient `State`, which a class method or a
value-type method writing no field does not have. The invariant, per scope kind:
a loop or fold whose threaded set is empty selects a `StateAcc` plan only when
the scope has a carrier to seed it with: the actor `State` (actor instance
method), the REPL bindings, or a family the body threads (`SelfVt` in a
value-type method that writes a field). In a class method or a value-type
method writing no field it lowers as a plain loop. This holds for every
`Letrec` loop (`timesRepeat:`, `to:do:`, `to:by:do:`, `whileTrue:`/
`whileFalse:`) and every `Foldl` loop (`do:`, `collect:`, `inject:into:`, the
search and keyed folds, `eachWithIndex:`/`do:separatedBy:`), whatever encloses
it (a method body, a conditional arm, an `on:do:`/`ensure:` block, another loop
body). One predicate decides it (`loop_body_has_carried_mutations`, with
`loop_body_needs_threading` adding the threaded set for the counted loops and
folds), read by every loop and fold gate and by the statement classifier
(`control_flow_has_mutations`) for a loop or fold's block arguments, so the
lowering and the caller that unpacks it cannot disagree. Conditional arms and
exception blocks keep `block_arg_needs_threading`. The verifier check is the
existing `ActorStateInClassMethod { StateAccLoopWithoutLocals }`; the
class-variable corpus gains the `block_local` shape (a block-introduced local
rebound inside a loop nested in that block), which with `local_touch` off
reproduces both the BT-3834 and the BT-3848 panics on the pre-BT-3848 analysis
without this gate and none with it.

**Amendment (BT-3910): `CarrierSlot::Whole`.** `CarrierSlot` gains `Whole`:
`<read>` is the carrier itself, with no `element`/`maps:get`. Every row of the
table above applies as written. Examples: `Let` + `bind_var` at a non-REPL
`MethodBody`; `Put("t", V)` into the bindings map at the REPL `MethodBody`;
`Put("__local__t", V)` in a `StateAcc` loop, handler or `BranchArm` frame when
`t` is a member; a `Gensym` chain entry in `DirectParams`/`TupleAcc`/`Hybrid`
frames. In an actor frame no `State` family `Bind` precedes it, because the
assignment carries no family.

**Amendment (BT-3910): the REPL threads its bindings in every frame.** In a REPL
input the map a frame runs over is the bindings map itself, so the REPL root,
a REPL arm and a `StateAcc` loop or fold body thread *every* binding: `t` is a
member whatever the frame's recorded set, which is empty or partial because the
REPL threads its loops through the map rather than through `__local__` keys.
The one rule is `RebindFrame::with_repl_binding`, read by the producer and by
the verifier (`ScopeKind::Repl`) alike. A value-position assignment at the REPL
root is verified as an explicit `MethodBody` node (the REPL's first production
one); a REPL arm is registered as a rebind frame of its branch.

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
- **`VerifyError::UnthreadedLocalAssignment { local, span }`** (BT-3910). An
  identifier-target assignment whose value is consumed must be lowered as the
  value-position producer. The `Expression::Assignment` arm of
  `generate_expression` that emitted only the value is gone as a lowering; any
  path that still reaches it for a consumed local assignment reports this error
  through `report_threaded_ir_verify_errors`. Fails on every row of the BT-3860
  repro table when the producer gate is disabled
  (`tests/local_assignment_producer.rs`); the `dropped_assignment_probe`
  agreement tests are the corpus proof, recording zero hits over the generated
  corpus in methods and in the REPL (eval and trace), and the class-variable
  corpus generator's `value_assign` shape (`Shapes::VALUE_ASSIGN`) runs the same
  rewrite on a BEAM.
- **`ActorStateInClassMethod`** (BT-3725), extended to `ScopeKind::ValueType`.
  Fails on o3/o12 and s2 in value-type methods.
- **`StateEffectEscapesExpression`** (ADR 0118 §5), already defined, now also
  reported for a `LocalRebind` left in a prelude that is closed in an
  `Opaque` context (a Tier 1 closure body or an FFI argument).
- **`VerifyError::FoldLocalsNotCarried { threads, found }`** (added by
  BT-3863). A fold frame that threads locals must end its lambda body in an
  accumulator of its mode's shape: a typed `FoldAccumulator` whose carry is
  the repack of `threads` for a tuple accumulator, or the `StateAcc` map for a
  map accumulator, never the body's raw value. Fails on the BT-3839 repro (a
  nested loop's raw value returned as a `do:` accumulator) when the fold's
  gate and statement classifier are split again. It checks the shape, not
  which versions the accumulator reads: a write lost inside an opaque
  statement before a correctly shaped tail (the same split in a `collect:`
  body) is caught by the loop-body producer-agreement check instead, which
  `fold_locals_carried_corpus.rs` runs under the same fault for the
  `collect:`, `inject:into:` and satisfy folds.

`report_threaded_ir_verify_errors` reports all seven. Nothing gets a new
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

The arity choice keys on the **selector and a literal block argument that
writes an outer local**. *Implementation note (BT-3750):* the producer also
requires the receiver to be the `Result` class reference, because the
emitted call is `beamtalk_result:'tryDo:'/2` itself: a `tryDo:` sent to any
other receiver (a user class that happens to answer `tryDo:`, a variable)
keeps its dynamic send, and its writing block is a §6 / Phase 0 compile
error instead of a Tier 2 fun reaching a callee that cannot return the
write. The block is lowered as a `BranchArm` frame over the fun's `StateAcc`
parameter, with the same rule as a conditional arm
(`conditional_arms_lower_as_producer`), so a loop, fold, conditional or
`on:do:` inside it rebinds its locals into that frame. In the REPL the fun
runs over the bindings map itself, so `'tryDo:'/2` answers the `{Value,
Bindings}` tuple the REPL assembler unpacks.

### 6. A Tier 2 block with no return channel is a compile error

Outside an actor instance method there is no channel through which a callee
can return a `StateAcc`. So a **Tier 2 block value** (a block literal that
writes an outer local, or a local bound to one and not reassigned) that
flows to a send that is neither a §1 construct nor an actor self-send **or
escapes the method** is rejected at compile time:

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
compiled from an actor instance method keeps working (BT-912) only when the
callee hands the block's `StateAcc` back (BT-3755): a `sealed` method of the
same class that invokes the block directly (`b value` as a statement, an
assignment or a `^`), as the callable of a collection message (`do:`,
`inject:into:`, ... ADR 0128), in the body of a `do:` that assigns the
invocation to a local bound before it, or that passes the block on to another
such method. An inherited callee, an overridable one, and any other use of the
block (inside a loop or conditional, as an operand, stored) drop the write or
raise, so they are the same error until the callee-side `{Result, StateAcc}`
protocol exists (BT-3742). One predicate,
`semantic_analysis::tier2_callee::callee_threads_block_param`, decides it for
both the diagnostic and codegen's class pre-scan.

**Escaping without a send (BT-3756).** The same error covers a Tier 2 block
value that leaves the method without being sent anywhere: returned (`^[t := t
+ 1]`, the method's last expression, or `^b`), stored in a field or class
variable (`self.f := b`), or placed in a list, array or map literal (`#[b]`).
Once the method frame is gone nothing can hand `t` back, in any method kind;
probed before the fix, each shape compiled and then raised `Cannot call 'value'
on Block ... captures mutable state` at the first later `value`, except the
actor `b := [...]. self.f := b`, which silently lost the write. A block that
writes no local of the method (a pure closure, or its own locals only) is
unaffected; an actor block that writes a `self.x` field passes §6 but is
codegen's `FieldAssignmentInUnsupportedBlock` error in every escape shape
(BT-2792, pinned for each shape by BT-3806). A block literal assigned straight to an instance
field is already the block analyzer's "cannot mutate captured variable ...
stored in a field" error, reported once; §6 reports it for a class variable.
The escape classification is `block_facts::escape_sites`, the one behind
`escaping_blocks`; a REPL expression list is not checked (BT-3806 pinned what
the REPL does, the same runtime error, in the repl-protocol case
`adr0131_section6_escape_pins`; rejecting it there waits on a maintainer
decision, BT-3858).

BT-3806 follows a returned value through what makes it up (`escape_sites`
only; `escaping_blocks`, the census's and the class-state-abroad lint's,
still takes it as written): a last-statement or returned local assignment
(`b := [t := t + 1]` as the method's last statement, `^(b := [..])`), the last
statement of each arm of an inlined conditional, an evaluated literal block,
an `on:do:` body or handler, or an `ensure:` body, and a `match:` arm body,
repeatedly (`c ifTrue: [[t := t + 1]] ifFalse: [...]`). A local counts as
the block of its latest binding in walk order however often it is assigned
(`b := nil. b := [t := t + 1]. ^b`), and a local bound to such a value
(`b := c ifTrue: [[..]] ...`) as that block. Probed before the fix, every one
of these raised the same `Cannot call 'value' on Block ... captures mutable
state` at the first `value` in class, value-type and actor methods (the
actor's last-statement `b := [...]` answered `nil`). Still not seen: a local
rebound to something else in only one branch before it escapes (any rebind
forgets the block), and a block that is the value of a non-inlined send.

BT-3859 closes two gaps in that binding map. A block bound through a
conditional arm whose write targets a local first bound in that arm
(`b := c ifTrue: [u := 0. [u := u + 1]] ifFalse: [nil]. ^b`) had its writes
taken after the arm's scope was gone, so `u` looked like the block's own: the
walker now records each value block's write while it is at the block, the
same walk the literal path reports from. And a local that aliases a
block-valued local (`c := b. ^c`, `c := x ifTrue: [b] ifFalse: [nil]`, an
arm-local `b` included) now counts as that block, reported at the block's own
binding. A method whose last statement is a local assignment answers the
assigned value in class, value-type and actor methods alike (an actor method
answered `nil` there until BT-3882), so a block that is that value is
returned from the method and §6 reports it so in every context.

**A `whileTrue:`/`whileFalse:` condition block (BT-3782).** The loop's
*body* is a §1 construct, but its keyword *condition* is a block receiver
whose outer-local write only has a return channel where codegen lowers the
loop through its `StateAcc` (`while_loop_threads`): a literal body and a
condition with a state effect (a self-send or field write), in a value-type
or actor instance method. Any other condition that writes an outer local
(`[i := i + 1. i < 3] whileTrue: [nil]`) was compiled as a Tier 2 closure and
called with the wrong arity (`function expects 1 arguments but was called
with 0`), and in a class method hangs, so it is the same error until the
condition is lowered as a producer (BT-3743). A top-level (REPL) expression is
not narrowed. The unary `whileTrue`/`whileFalse` condition is unchanged. The
agreement is pinned by `lint_and_section6_agree` and
`while_condition_outer_local_write_is_rejected_unless_codegen_threads_it`.
Phase 0's position allow-set does not cover this shape (it accepts any
statement position), so this is a §6 rule, not an allow-set row; Phase 5
removes it together with the lowering.

**Not exempt (BT-3917):** an Erlang FFI argument (`(Erlang lists) map: [:x |
t := t + 1. x] with: …`, s6) was first exempt as lossy by design (ADR 0041
§Erlang Interop Boundary). ADR 0133 now reports a block literal that assigns a
captured local, passed as an FFI argument, as an escaping block; a block that
only reads captured locals still compiles.

**What it costs.** A Tier 2 block value that is stored but never invoked
through a channel-less send (`d at: #k put: [t := t + 1]`, a block only
asked `numArgs`) compiles and runs today and will be rejected. The
alternative, proving the block is never invoked, needs escape analysis the
compiler does not have. The rule is accepted as stated, as an error and
not a warning: the write in such a block could never have taken effect
wherever it is later invoked, so a warning would let the author ship a
program that is already wrong, and the help text says what they meant.

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
(CLAUDE.md § REPL output). The transcript above is the **approved display**
for the probe shapes: every one of them prints a wrong value, a leaked
tuple or an error today, and each phase's `repl-protocol` cases assert the
correct value without a further per-PR sign-off. Any other REPL display
change (a prompt, a format, a shape outside the probe) still needs its
own.

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
  added for each probe shape, with the ADR's transcript as the approved
  display.
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
| 5 | Close-out: `local_touch` into `Shapes::ENABLED`, un-`#[ignore]` both properties, `CV_CORPUS_CASES=500` green, remove Phase 0's allow-set diagnostic (*amended: retained, see Implementation Tracking § Remaining work at close-out; BT-3841*), flip every `PIN-BUG`, docs (`beamtalk-language-features.md` "What Works and What Doesn't" and the protected-region rule, `docs/agents/expanded.md` § State-Threading Codegen, `debugging.md` verifier table), build-time measurement. | CI. |

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

### Phase 0 close-out measurement (BT-3841)

Phase 5a (BT-3751) could not delete the Phase 0 position diagnostic
(`DENY_SET` / `ALLOW_SET`): with it bypassed, most rejected statement shapes
answered wrong or failed to compile. BT-3841 retires it as a series of PRs and
keeps the measurement as a checked-in harness, so every number below is
reproducible and every PR moves it:

- `just measure-adr0131-statements` compiles every probe cell through the real
  `beamtalk test` pipeline with the position check bypassed (a build with the
  `beamtalk-core/measure-bypass` cargo feature reads env
  `BEAMTALK_ADR0131_MEASURE_BYPASS_PHASE0`), runs it on a BEAM and compares it
  with the ADR-expected answer. The statement matrix is the 2,834 class,
  value-type and actor cells of `adr0131_statement_probes.tsv` that are not
  `gap` lines (every looping role runs its body twice); the operand matrix is
  1,482 cells (a construct as the value of an assignment or field assignment, a
  receiver, an argument, a binary operand, a cascade part, a literal element, a
  `match:` subject or an interpolation, in a method body, an arm and a `do:`
  body). Results: `adr0131_statement_measured.tsv`, `adr0131_operand_measured.tsv`
  (`right`, `wrong` or `erlc`), summarised in `adr0131_phase0_summary.tsv`.
- `adr0131_phase0_agrees_with_measured_answers` asserts both directions for the
  statement matrix (a cell that is not `right` is rejected, a `right` cell is
  accepted), `adr0131_phase0_operand_positions_are_sound` the soundness
  direction for the operand matrix.

Statement cells that do not answer right (after the evaluated literal block PR on top of the enumerations: 60, all in actor methods; 229 before the enumerations landed and 443 after PR 5, of which 21 are cells of the plain-`detect:` kind added to the matrix there; 422 after PR 4; 520 after PR
3, 571 after PR
2, 663 after PR 1's three-deep roles, 595 in the 5a population), by family and
method context, with the work each needs:

| Family | class | value | actor | total | Classification |
|---|---|---|---|---|---|
| `eachWithIndex:` / `do:separatedBy:` | 0 | 0 | 0 | 0 | done (was 66 / 52 / 43 = 161): producers in `producer_body_arg_index` with index/separator parameters, the same frame as `do:` |
| `detect:ifNone:` search or handler (and plain `detect:`, same fold; 241 plain-detect probe cells measured, class and value right outside the permanent shapes) | 0 | 0 | 44 | 44 | done in PR 4 for class and value-type methods (an early-exit fold, below); the 44 actor cells (nested statements in loop bodies, arms and protected bodies) wait for an actor-frame site rule |
| `[...] value` as a statement | 0 | 0 | 0 | 0 | done in PR 6 (was 58 / 39 / 35 = 132): an evaluated literal block is an arm-like producer, below |
| any statement inside a `[...] value` body | 0 | 0 | 0 | 0 | done in PR 6 (was 38 / 38 / 14 = 90): the evaluated-block frame is a `BranchArm` frame for the constructs nested in it |
| `keysAndValuesDo:` / `doWithKey:` | 0 | 0 | 0 | 0 | done in PR 2 (its cells inside a `[...] value` body are in the row above) |
| `anySatisfy:` / `allSatisfy:` | 0 | 0 | 0 | 0 | done in PR 3 as producers answering their boolean; PR 4 added the early exit |
| plain `detect:` (kind added in PR 5) | 0 | 0 | 16 | 16 | measured right in class and value everywhere but the permanent shapes; the actor cells wait with `detect:ifNone:` |
| **total** | **0** | **0** | **60** | **60** | |

By block role: loop bodies 24, protected bodies 18, three-deep nestings 8,
handlers 6, arm/loop mixes 4 (123 / 78 / 38 / 32 / 29 before PR 6 and the
enumerations; arms 41, the method body 12 and evaluated blocks 90 are 0).

**Amendment, BT-3841 PR 6: an evaluated literal block is an arm-like producer.**
`[...] value` (the unary `value` sent to a block literal with no parameters) is
the degenerate conditional: one arm that always runs. `literal_block_value_arm`
(`block_facts.rs`) is its one site rule, and `conditional_arm_blocks` returns the
block as the single arm, so the conditional producer rule
(`conditional_producer_writes`), the site rules (`sequenced_operand_sites`,
`loop_body_producer_sites`), the Phase 0 check (arm frames in
`Walker::expr`, `check_construct`) and codegen's producer
(`generate_block_value_with_mutations`: the seeded scratch map, a `BranchArm`
frame for the block, `conditional_tuple_to_threaded_value`) all read the same
rule. The receiver rule: the receiver is a block literal (parentheses are not
peeled), so its class is statically `Block` and no other class's `value` can run;
`value:` / `value:value:` / `valueWithArguments:` bind arguments outside the frame
and keep their legacy lowering and rows, as does the REPL root. The block's
writes are also the enclosing block's writes for the mutation analysis
(`analyze_expression` and the cross-scope collector read it like an `on:do:`
receiver), so a `do:` body whose only outer write is inside a nested `[...] value`
is threaded. The operand matrix moved too: the `value` kind went from 1 to 61 cells
answering right (assignment, field, receiver, argument, operand, literal), the
cascade, interpolation and `match:` positions stay rejected like a conditional's.

Residual `gap` cells (BT-3776, a construct writing only a local of its own block):
the kinds `value` and every kind inside a `[...] value` body are now measured with the
other cells (`GAP_MEASURED_KINDS`, `gap_is_measured`) and held to the same rule. Of the
134 `gap` cells of this family, 132 answer right and 2 do not: `iterIter` and
`armIterIter` with a `local` target in an actor method (the outer `do:` does not see a
write-only `t := u` behind a nested `do:`, the same failure for all 31 kinds there, not
specific to `value`), which stay with BT-3776 and out of the measured set.

**Amendment, BT-3776: the block-local cells are measured, and the `gap` verdict is
retired for them.** Every former `gap` line (targets `local` and `dlocal`) was run on a
BEAM with the check bypassed and joined the measured table, so
`adr0131_phase0_agrees_with_measured_answers` holds them to the same rule as every other
cell (the `GAP_MEASURED_KINDS` / `ADR0131_MEASURE_GAPS` scaffolding is gone). Every
class and value-type cell answers right, and so does every actor cell except
`detect:` / `detect:ifNone:` (search block or handler) in a protected body (erlc), a
handler or `ensure:` block, or directly in a loop or counted-loop body (the fold answers
`{Value, State}` with the local packed into a state the body discards): 48 cells. The
review added six containers with a loop as the innermost block (`loopArm`, `timesArm`,
`loopIter`, `timesIter`, `loopLoop`, `timesLoop`, 684 cells): every class, value-type
and actor cell answers right except the 36 actor detect ones, which answer wrong. All 84
are rejected by the `deny_block_local` rows of `DENY_SET` (`crosses: false`, keyed on
the innermost role too) until BT-3878 lowers those folds as producers. A second review
round added the reverse nesting, a `do:` body, a `collect:` body or an evaluated block
innermost in a `whileTrue:` or `timesRepeat:` body (`iterLoop`, `collLoop`, `valLoop`,
`iterTimes`, `collTimes`, `valTimes`, 684 cells): every cell answers right in every
context. Under a `whileTrue:` body they stay accepted; under a counted loop the row keeps
every inner role rejected, because `DoBody` / `ListOpBody` also name the keyed,
enumeration, fold, select and satisfy blocks, unmeasured there, so the 18 actor detect
cells of `iterTimes` / `collTimes` / `valTimes` are over-rejected (`KNOWN_OVER_REJECTED`)
until BT-3878. The rows are coarser than the measurement where it is silent: every inner
role under a counted loop, and the innermost protected-body, handler and loop rows under
the list-op and
evaluated-block outer roles (and the protected-body and handler ones under a `do:` body
or a loop), inferred from the measured shapes that share their lowering. The `iterIter` /
`armIterIter` actor cells above answer right since. The re-measurement also found twelve
actor `outer` `detect:ifNone:` cells in `do:` / `collect:` roles that answer right but
were recorded `wrong`; they are `over` (`KNOWN_OVER_REJECTED`) until BT-3841 carves them
out. The only `gap` lines left are two REPL method-body rows (unmeasured, BT-3823).

**Amendment, BT-3841 PR 4: the satisfy and detect folds exit early.** The
inline fold of a writing `anySatisfy:`, `allSatisfy:`, `detect:` and
`detect:ifNone:` is a `lists:foldl`, which has no early exit, so the first
version of the satisfy producers ran the block for every element (BT-3805: a
silently different answer from the non-writing form, and a block that can raise
on elements the form never visits). The fold lambda now returns its accumulator
unchanged once the answer is decided (`BoolAcc` is the decisive value, or
`FoundFlag` is `true`), so the block runs up to and including the first
decisive element in every method kind and in the REPL. `detect:ifNone:` is a
producer in class and value-type methods (its search block and its handler
pack into one accumulator); in an actor method the nested statement shapes
still answer wrong or fail to compile and keep the Phase 0 rejection.

The statement matrix nests at most three blocks; `RIGHT_STATEMENTS`
rows name the exact depth and none may name a shape past two blocks. The
unfinished families above are rejected past depth two in class, value-type and
actor methods (the review found an `anySatisfy:` in an arm in a protected body
in a `do:` body that the coarse rows accepted and that raises `bad argument` in
a value-type method); the REPL keeps its coarse rows, unmeasured past depth
one, until BT-3823.

Operand cells (1,482): 824 answer right, 576 wrong, 82 `erlc`; Phase 0 accepts
708, all of them right (zero unsound), and rejects the 658 that do not answer
right plus 116 that do. By position, the cells that do not answer right:
assignment 30, field assignment 27, receiver 43, argument 43, binary operand
43, literal element 43, cascade part 143, interpolation 143, `match:` subject
143. The first six are (a): the operand sites `sequenced_operand_sites` and
`repl_operand_sites` already name for some constructs extend to the rest. The
last three (429 cells) are the likely permanent §6 rejections: a cascade
shares one receiver across parts, an interpolation builds one binary and a
`match:` subject is a single expression, so there is no statement boundary for
a `LocalRebind` prelude to sit on without rewriting the expression; they stay
compile errors unless the ADR is extended with an ANF hoist for them.

The REPL rows (93 `bad` lines of the probe table) are not measured here
(BT-3823). Plain stored closures and Erlang FFI arguments are §6 and not part
of either matrix; `^` positions are not measured (BT-3813).

## Migration Path

No *correct* program changes behaviour. Programs that today fail at runtime
or answer wrong either become correct (phases 2–4) or get a compile error
(§6 and the phase 0 allow-set diagnostic). The §6 error's help text gives
the rewrite. The one working shape §6 rejects, a Tier 2 block value stored
and never invoked through a channel-less send, needs its block rewritten to
return the value rather than write the local.

## Open Questions

1. **Callee-side Tier 2 protocol for class and value-type HOMs** (Alternative
   D). This needs its own ADR, tracked as BT-3742. Until then §6 stands,
   and the set of sites §6 rejects is the inventory that ADR must cover.
2. Should a `LocalRebind` whose local is dead after the construct be elided?
   The verifier exemption would need a liveness fact the IR does not carry
   today. The default is to emit it (correctness first) and measure.

## References
- Related issues: BT-3738 (becomes phase 3), BT-3742 (Alternative D), BT-3718, BT-3725, BT-3737, BT-2717,
  BT-3694, BT-912, BT-3493
- Related ADRs: ADR 0041, ADR 0111, ADR 0118, ADR 0122, ADR 0128, ADR 0130
- Documentation: `docs/beamtalk-language-features.md` § Control Flow and
  Mutations; `docs/agents/expanded.md` § State-Threading Codegen;
  `docs/development/debugging.md` § ThreadedIr verifier
