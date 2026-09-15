# ADR 0122: Scoping the `ThreadedIr` Storage-Family Generalization

## Status
Proposed (2026-09-14; revised after review 2026-09-15)

## Context

### Problem statement

ADR 0120 documented that three storage families — Actor `State`,
`ClassVars`, and value-type `SelfVt` — each need their own hand-written
answer to "does this construct's body mutate this family, and how do I
thread the mutation through it," bolted on per construct family (loop,
conditional, exception handling, list-op accumulator, `match:` arm) as
each gap was found. ADR 0120 deliberately deferred generalizing that
consumer-side logic, setting an explicit revisit trigger: "the next new
gap in this family (a fourth distinct call site, or a fourth storage
family)." Its Addendum 1 recorded that the trigger fired — Epic
[BT-3490](https://linear.app/beamtalk/issue/BT-3490) closed the three
gaps ADR 0120 named plus a fourth found independently
([BT-3489](https://linear.app/beamtalk/issue/BT-3489), `match:` arms) —
and filed [BT-3499](https://linear.app/beamtalk/issue/BT-3499) to scope
the generalization as its own design decision, using the five now-fixed
call sites as worked examples rather than hypothetical shapes. This ADR
is that scoping decision. It answers the two questions ADR 0120's
Constraints section left open and makes the generalize-vs-defer call.
**No production code changes as part of this ADR** — per BT-3499's
acceptance criteria, this is a design decision, not the generalization
itself.

### Current state

The five call sites, and what each one hand-writes today:

1. **Loop** (`counted_loops.rs`/`while_loops.rs`) —
   `loop_body_threads_class_vars`/`loop_body_threads_value_self`
   (`crates/beamtalk-codegen/src/core_erlang/control_flow/analysis.rs:106-180`)
   are near-identical detectors, each walking only *top-level* loop-body
   statements (deliberately non-recursive — a nested self-send tripping
   this predicate caused a real regression, `analysis.rs:334-343`),
   matching a bare field write or same-class self-send. Emission
   (`vt_construct_extra_slot`,
   `crates/beamtalk-codegen/src/core_erlang/value_type_codegen.rs:2229-2254`)
   hand-matches `None | ClassVars | ValueSelf` into a fixed trailing tuple
   position.
2. **Conditional** (`conditionals.rs` for Actor `State`;
   `value_type_codegen.rs` for `ClassVars`/`SelfVt`) —
   `is_conditional_with_vt_local_threading`/
   `is_conditional_with_vt_self_field_threading`
   (`value_type_codegen.rs:2593-2686`) plus `VtCondBaseline`/
   `VtCondSlots`/`build_vt_conditional_branch_pieces`/
   `finish_vt_conditional_branch`/`rebind_vt_conditional_mutations`
   (`value_type_codegen.rs:128-142`, `:3556-3860`). Note that this group
   is primarily the **outer-local** mutation merge machinery
   (`VtBranchPieces.local_values`, the `all_mutations` walk, the
   `element/N` extraction loop); the `ClassVars`/`SelfVt` slots are two
   ~15-line arms appended after the locals in
   `finish_vt_conditional_branch` (`:3748-3773`) and picked up at
   `next_slot = all_mutations.len() + 1` in `rebind_vt_conditional_mutations`.
   The 2-field `VtCondSlots`/`VtCondBaseline` structs are already a slot
   list in all but name.
3. **`on:do:`/`ensure:`** (`exception_handling.rs`) —
   `exception_blocks_thread_value_self` (`exception_handling.rs:209-216`)
   plus `push_exception_arm`/`exception_self_slot(_doc)`/
   `seed_exception_arm_self` (`exception_handling.rs:98-180`) build the
   same `{Result, StateAcc, Self1}` shape as the loop's. This site asks
   **only** about `SelfVt`; it has no `ClassVars` slot and no `ClassVars`
   detector. Its own doc comment (`exception_handling.rs:484-491`) asserts
   that class-method class-var mutations are "threaded entirely
   separately" via `emit_class_var_result_unwrap`'s let-chain and so never
   need a slot here. **That assertion is false** — see "Evidence found
   during review" below.
4. **`match:` arm** — `match_needs_mutation_threading`
   (`crates/beamtalk-codegen/src/core_erlang/gen_server/methods.rs:3251-3290`)
   is *recursive* (via the 141-line shared `control_flow_has_mutations`
   walk, `methods.rs:3531-3671`), unlike the other four's top-level-only
   walk, and supports only `State`/`ClassVars` — a value-type instance
   method's `self.field :=` inside a match arm is rejected outright
   (`patterns/match_lowering.rs:56-83`) because `match:` never built the
   N-arm merge machinery `ifTrue:`/`ifTrue:ifFalse:` have. Its emission
   for the two families it does support reuses the conditional's own
   branch-merge directly, with no tuple shape of its own.
5. **Letrec+Foldl rejection** —
   `reject_unthreadable_value_self_field_write` (`analysis.rs:241-280`) is
   already genuinely unified: one function, parameterized by one bool,
   called identically from both `lower_letrec_body` and
   `lower_foldl_body` (`body.rs:324`, `:690`). But this site is not
   rejection-only for `ClassVars`: `ThreadingPlan.threads_class_vars`
   (`control_flow/plan.rs:141-173`) has a second, **Foldl** shape in which
   the fold accumulator carries a `ClassVars` slot in a *leading* tuple
   position (`plan.rs:165-166`), where the Letrec shape uses a *trailing*
   `letrec` fun parameter. So "extra slot" already means two different
   positions depending on the construct.

Three further `ClassVars`-only side channels with no `SelfVt` counterpart
exist alongside the five sites and are part of the same mechanism:
`loop_threads_class_vars`, `last_loop_class_var`, and
`last_foldl_class_var_peak` on `LoopMode`
(`control_flow/loop_mode.rs:51-89`), with their save/reset/restore in
`generator/branch_guard.rs` and the version fast-forward in
`generator/version.rs:145-157`.

Measured (signature to closing brace, doc comments excluded): the eight
named detectors total **~140 lines** (plus the 141-line recursive walk
behind `match:`); the listed emitters total **~280 lines** (~380 including
`build_vt_conditional_branch_pieces_inner` and `resolve_mutation_value_docs`),
of which the conditional group is ~60-70%. Within the conditional group,
roughly half the lines are `ClassVars`/`SelfVt`-specific; the rest is
outer-local threading and shared scaffolding that a storage-family
generalization does not touch.

At the **identity** layer, all three families are already unified:
`VersionPrefix::{State, ClassVars, SelfVt}`
(`crates/beamtalk-codegen/src/core_erlang/threaded_ir/ir.rs:151-187`) are
three variants of one enum feeding one `VersionedVar`/`VersionCounter`,
and `verify()`'s linearity/liveness walk (`threaded_ir/verify.rs`) treats
all three uniformly — its only prefix-specific branch is the ADR-0110
`ClassVars` shadow-write check (`verify.rs:372-375`). What is **not**
unified is the *consumer* logic listed above: two independent boolean
fields (`ThreadingPlan.threads_class_vars`/`threads_value_self`,
`control_flow/plan.rs:173,207`) computed by two independent predicates,
and hand-enumerated `None | ClassVars | ValueSelf` matches rather than a
data-driven "which storage families does this construct need to carry"
list.

### Evidence found during review: a sixth gap, in production-reachable code

Answering this ADR's own open question about site 3 with a compiled
repro (class-method context, `classState: runs = 0`, `class bump =>
self.runs := self.runs + 1`) gave three distinct outcomes for the same
"class var mutated inside `on:do:`/`ensure:`" shape:

```beamtalk
// (a) direct write in the try body — clean compile-time rejection
//     ("Cannot assign to field 'runs' inside this block")
class directEnsure =>
  self.runs := 0
  [
    self.runs := self.runs + 1
    nil
  ] ensure: [nil]
  self.runs

// (b) same-class self-send in the try body — compiles, mutation SILENTLY
//     DROPPED: the generated Core Erlang binds `ClassVars2` inside the
//     try-body fun and discards it; the post-ensure: read is
//     `maps:get('runs', ClassVars1)` — the pre-try version — and the
//     method returns `ClassVars1`, so `self.runs` reads 0 and the class
//     var stays 0. Identical for the cleanup block, the on:do: try body
//     (last and non-last position), and the on:do: handler block.
class selfSendEnsure =>
  self.runs := 0
  [
    self bump
    nil
  ] ensure: [nil]
  self.runs

// (c) self-send plus a co-occurring local mutation — ThreadedIr verify
//     failure (NonLinearVersion/UnboundVersion on ClassVars1: consumed
//     once, produced never, inside a branch arm); a debug-build panic,
//     an erlc unbound-variable crash otherwise.
class selfSendEnsureWithLocal =>
  self.runs := 0
  seen := 0
  [
    self bump
    seen := seen + 1
  ] ensure: [nil]
  self.runs
```

Outcome (b) is BT-3486 (the `SelfVt` silent drop in the same construct)
for `ClassVars`, and outcome (c) is the ADR 0111 verifier doing its job
on the same gap. Unlike every gap ADR 0120 scoped, this one is not
`TestCase`-only: class variables mutated from class methods are ordinary
production code (ADR 0110, Implemented). It was not found by BT-3490's
axis-4 matrix because `mutation_corpus_class_method.bt`'s `on:do:`/
`ensure:` fragments assert an outer local (`t`), not the class var.
Filed as BT-3506. It is also the strongest evidence this ADR has for its
thesis: BT-3486 added a `SelfVt` slot to `on:do:`/`ensure:` with a
`SelfVt`-only detector, and nobody was made to ask whether `ClassVars`
needed one too — the exact "forgot a family at a site" failure a
family-parameterized detector exists to make impossible.

### Constraints

- `just verify-threaded-ir` is `test-stdlib` + `test-bunit` under
  `debug_assertions` (`Justfile:1179-1180`): it hard-panics on any
  `ThreadedIr` `VerifyError` while compiling the corpus, which is what
  caught outcome (c) above. It verifies *invariants*, not *output
  stability*: a version graph can be perfectly well-formed (every version
  produced, consumed exactly once) while the generated tuple shape or slot
  order differs from before. There is **no** byte-identical Core Erlang
  diff over the stdlib + bootstrap-test corpus today — no `insta`
  snapshots or golden `.core` files under `crates/beamtalk-codegen`, and
  no dump flag on the CLI. The nearest reusable pieces are
  `beam_compiler.rs:572-587` (already writes `.core` files into the build
  dir) and `test-package-compiler`'s 96 `*_codegen.snap` snapshots, which
  are a hand-curated set disjoint from the corpus.
- Every one of the three "conditionally-appended slot" call sites carries
  a comment documenting a real regression from predicate-widening (the
  loop's nested-self-send regression above; BT-3492/BT-3493's shared
  statement-classifier bugs, ADR 0120 Addendum 1). A generalization has to
  reproduce each of those special cases exactly.
- The generalization's scope is **production-reachable**: `ClassVars` in
  class-method loops and conditionals is ordinary code (ADR 0110), and
  the sixth gap above is in it. Only the `SelfVt` half of the scope is
  `TestCase`-only (ADR 0120's BT-1533 exemption).
- Per-family behaviour that a family-agnostic slot list still cannot
  erase: `Bind` construction differs (`ClassVars` ⇒ `shadow_write: true`,
  enforced by `verify.rs:372-375`; `SelfVt` ⇒ `maps:put` on the instance
  map), and the Foldl accumulator puts its slot in a leading, not
  trailing, position.

## Decision

**Generalize now, but scoped — and narrower in shape than ADR 0120's
sketch: unify the *detector* and the *slot-list type*, and make each
site's emitter consume that list. Do not merge the three emitters into
one.**

This answers the two questions ADR 0120's Constraints section left open:

**Does Actor's `State`/`StateAcc` fit the same abstraction as
`ClassVars`/`SelfVt`'s "extra slot"?** Yes — it already does, and the
first draft of this ADR was wrong to say otherwise. `StateAcc` is element
2 of the very same result tuple the other two families are appended to
(`{'nil', StateAcc[, ClassVars | Self1]}`, `while_loops.rs:405-409`,
`counted_loops.rs:403`), emitted unconditionally, and the Actor
conditional merge (`conditionals.rs:522-626`) uses the identical
both-or-neither discipline — a `{Value, StateAcc1}` in the mutating arm
and a synthetic `{'nil', State}` in the other — that
`finish_vt_conditional_branch` uses one position further right. In a
`Vec<VersionPrefix>` model, `State` is the *easy* case: a slot whose
predicate is constant-true, sitting before the conditional ones. What
genuinely differs is that `StateAcc` doubles as the ambient `__local__`
scratch map (ADR 0120's "free rider" point), and that its code path
(`conditionals.rs`, `with_branch_context`) is a separate, working,
production-critical implementation. So the reason to leave `State` out of
this generalization is **migration risk, not abstraction fit** — and the
slot-list type must be shaped so that `State` is addable later (order:
locals scratch map, then `State`/`ClassVars`/`SelfVt` in a fixed
canonical order) rather than a warmed-over two-family enum.

**Can a data-driven `Vec<VersionPrefix>` walk replace the five
hand-enumerated detectors/emitters without changing Actor/`ClassVars`
codegen output?** The detectors, yes: `find_class_var_mutating_stmt` and
`find_value_self_mutating_stmt` are near-identical top-level-statement
walks, and the `on:do:`/`ensure:` detector is a thin wrapper on the same
predicate. The emitters, only partly: their bulk is outer-local threading
and construct-specific tuple conventions (leading vs trailing slot, the
`try`/`catch` seeding), which a storage-family generalization does not
touch. Realistic dead code from unifying the family-specific arms is
**100-150 lines**, plausibly net-zero once the shared type and its tests
exist. The value of this decision is therefore not line count; it is the
property the sixth gap above proves is missing: **a site that receives a
family it does not carry must fail loudly.** `ThreadedIr::verify()`
already provides exactly that failure (outcome (c) is it) — what is
missing is the single detector that reports every eligible family to
every site, so the failure fires at development time rather than only for
the shapes someone thought to test. `match:` keeps its own recursive,
asymmetric detector; its emission already reuses the conditional's merge.

Concretely (sketch, not a signature commitment):

```rust
// control_flow/analysis.rs — next to reject_unthreadable_value_self_field_write
/// Storage families a construct body mutates, in canonical slot order.
pub(in crate::core_erlang) struct ThreadedFamilies(Vec<VersionPrefix>);

/// One walk, parameterized by which families this context may thread
/// (Actor: [State]; class method: [State, ClassVars]; value type: [State, SelfVt]).
fn body_threaded_families(&self, body: &[Expression], eligible: &[VersionPrefix])
    -> ThreadedFamilies
```

Each site then replaces its `bool` pair / `VtLoopExtraSlot` / `VtCondSlots`
/ `exception_self_slot` gate with "for each family in the list, append its
slot in this site's position" — keeping the site's own tuple layout, its
locals machinery, and its narrowing comments intact. `State` appears in
every `eligible` list so the type is honest about it, but the Actor
emitters are not rewired in this ADR.

Explicitly out of scope:

- **Migrating Actor `State` emission** onto the new list. It stays on
  `conditionals.rs`/`with_branch_context`. See the sharper trigger under
  Implementation.
- **`match:`'s detector and family-support asymmetry.** It keeps its
  recursive detector and its rejection for `SelfVt`.
- **Merging the three emitters into one function.** Rejected on the
  measured numbers above; see Alternatives.

## Prior Art

The immediate precedent is internal: ADR 0111 built the shared IR and
verifier first and deferred "which constructs route through it" to
narrower issues; ADR 0120 continued that by patching three gaps narrowly
rather than generalizing under pressure. This ADR continues it one step
further, scoping the generalization to the part the evidence supports.

Two external precedents bear directly on the *shape* chosen, and both
point the same way — compute the set of things needing a merge, then merge
exactly that set, rather than hand-writing one merge per thing:

- **SSA construction** places φ-functions per join point for the set of
  variables assigned on some incoming path (Cytron et al., dominance
  frontiers). No production SSA builder hand-writes a per-variable φ
  arm; the variable set is data, and adding a variable class does not
  add a code path. Beamtalk's "which storage families does this
  construct carry" question is the same question asked of a fixed,
  tiny variable set.
- **Erlang's own compiler** computes, for `case`/`if`/`receive`/`try`,
  the set of variables bound in each clause and reports a variable bound
  in some but not all clauses as *unsafe* (and "exported from case"
  when used afterwards). That is the both-or-neither discipline every
  branch site in this ADR reimplements — computed once over a set. The
  `try` case is also why site 3 needs a slot at all: bindings made inside
  `try` are not visible after it, so state that must survive has to
  travel through the try's value.

Adopted: the set-driven model (`ThreadedFamilies`). Not adopted: full SSA
φ placement over locals — ADR 0041/0111's `StateAcc` scratch map already
solves locals differently and this ADR does not reopen that.

## User Impact

No end-user-visible impact from the generalization itself. The affected
persona is a Beamtalk *contributor* working on `ThreadedIr` consumer code
who today has to remember to ask "does this also need a
`ClassVars`/`SelfVt` arm" for any new construct — a question BT-3490's
review missed once (`match:`) and this review found missed again (site
3). After this ADR, a site that ignores a family the detector reports
fails `verify()` on the first corpus compile.

The sixth gap itself does have user-visible impact until BT-3506 lands:
a class method that mutates a class var via a self-send inside
`on:do:`/`ensure:` silently loses the write. A Smalltalk developer will
find that surprising precisely because `ensure:` is the idiom for "this
must happen"; an Erlang developer will recognise the `try`-scoping cause
immediately. The fix is BT-3506, not this ADR.

## Steelman Analysis

**For the full three-family generalization (including `State` and
`match:`):** every family and call site left outside the unified
mechanism is a place the "did we handle every storage family" question
can still be forgotten — the failure mode that produced the `match:` gap
and the site-3 gap. A contributor extending `match:` to support `SelfVt`,
or a future construct that needs `StateAcc` conditionally, still
hand-writes the mechanism from scratch.

**For scoping as decided:** `State` fits the abstraction but not the
risk budget: its emitters are the production-critical Actor path, its
scratch map doubles as locals storage, and nothing found in this review
makes migrating it cheaper than it was. Keeping `State` *representable*
in the type while leaving its emitters alone costs nothing and preserves
the option. `match:`'s recursive, 2-of-3-family detector would need more
special-casing than it saves.

**For merging the emitters into one (the first draft's implicit plan):**
one emitter means one place to get slot order and baseline capture
right, and the three sites' family-specific arms really are the same
~15 lines three times. Against: those arms sit inside ~280-380 lines of
locals threading and construct-specific layout that would have to move
with them, the Foldl leading-slot and Letrec trailing-param conventions
would need a position parameter, and the measured payoff is 100-150
lines against a regression surface that includes production `ClassVars`
code — with no output-diff harness yet to prove it safe.

**For detectors-only and stop:** almost all of the "impossible to forget
a family" value lives in the detector, at a fraction of the emitter risk,
with a differential test as a complete gate. This ADR largely agrees —
which is why the detector is issue 1 and the emitter work is "consume the
list," not "rewrite." What "and stop" would give up is the loud failure:
a detector that reports `ClassVars` to a site whose emitter still has a
`SelfVt`-only gate changes nothing until the emitter reads the list.

**Where reasonable people disagree:** whether to file BT-3506's fix as
the first consumer of the new list (this ADR's choice — it is the site
whose gap the list would have caught) or as a narrow BT-3486-style patch
landed first and refactored later. The narrow patch is faster to ship;
the list-first order proves the mechanism on the exact shape that
motivated it.

## Alternatives Considered

### Full three-family generalization (State + ClassVars + SelfVt + match:)
Unify all five call sites, including Actor `State` emission and `match:`'s
detector, under one mechanism. **Rejected for now**: `State`'s emitters
are the production-critical Actor path with a working, separate
implementation, and `match:`'s recursive, 2-of-3-family detector needs
enough special-casing to undermine the unification. `State` stays
representable in the type so this remains a migration, not a redesign.

### Merge the three emitters into one function
The first draft's implicit reading of "generalize." **Rejected** on
measurement: the family-specific code is ~15 lines per site; the rest is
outer-local threading and layout that differs per construct (Foldl
leading slot, Letrec trailing parameter, `try`/`catch` seeding). Merging
would move ~300 lines to save ~100-150, against production `ClassVars`
code, before a corpus-level output diff exists.

### Detectors only, then stop
Land the unified detector (issue 1) with its differential test and defer
all emitter changes. **Adopted in part**: issue 1 is exactly this. Not
adopted as the end state, because a detector whose output no emitter
reads cannot make a site fail loudly — and the site-3 gap shows the
emitters are where families get forgotten.

### `State` + `ClassVars` + `SelfVt` for loops only
`vt_construct_extra_slot` is the one emitter where `State`'s absence from
the slot list is purely historical, so this would test the "State fits"
answer at the smallest possible size. **Deferred**, and recorded as the
concrete trigger for folding `State` in (see Implementation): it is the
right first `State` migration when one is wanted, but it is a migration
of Actor codegen and this ADR does not undertake one.

### Defer again, with the same open-ended "next gap" trigger
**Rejected**: that trigger has fired twice (ADR 0120 Addendum 1) and a
third time during this review. BT-3499 was filed to stop the cycle.

## Consequences

### Positive
- One detector reports every eligible family to every site, and
  `ThreadedIr::verify()` already fails a site that does not carry what
  it was told — closing the "forgot a family" class that produced the
  `match:` gap and the site-3 gap, at development time.
- The sixth gap (BT-3506) gets fixed as the first consumer of the new
  mechanism, on the exact shape that motivated it.
- `State` becomes representable in the slot-list type at zero migration
  cost, so folding it in later is additive.
- A corpus-level `.core` output diff (issue 0) exists afterwards for
  every future codegen refactor, not just this one.

### Negative
- Net line count is roughly unchanged; anyone expecting the ~350-line
  saving the first draft implied will be disappointed.
- The scope is production-reachable via `ClassVars`; the validation gate
  is mandatory, not optional, and issue 0 is real infrastructure work
  before any emitter changes.
- `match:` and Actor `State` emission remain hand-written; a contributor
  extending either still carries the original burden.
- Per-family configuration survives in the emitters (`shadow_write` for
  `ClassVars`, `maps:put` for `SelfVt`, leading vs trailing position) —
  "family-agnostic" describes the detector and the type, not every line
  of emission.

### Neutral
- Six smaller issues instead of one large one, each sized like
  BT-3486/3487/3488/3489 — consistent with ADR 0120's issue-by-issue
  discipline.

## Implementation

Recommended issue breakdown (final split TBD at pick-up time):

0. **Corpus `.core` diff harness.** A `just` recipe that compiles the
   stdlib + bootstrap-test corpus with `.core` retained
   (`beam_compiler.rs` already writes them) and diffs the tree against a
   baseline from `main`. Required before any emitter change; reusable
   for every later codegen refactor.
1. **Unified detector and `ThreadedFamilies` type** in
   `control_flow/analysis.rs`, next to the shared rejection function,
   replacing `find_class_var_mutating_stmt`/`find_value_self_mutating_stmt`
   and the `on:do:` wrapper. Pure refactor, gated by a differential test
   asserting old and new detectors agree on every construct in the
   corpus; the type carries `State` as an always-eligible member.
2. **BT-3506: the site-3 `ClassVars` gap**, fixed as the first consumer
   of (1): `on:do:`/`ensure:` grows a `ClassVars` slot exactly as BT-3486
   grew its `SelfVt` one, driven by the list rather than a second
   family-specific detector. Decide at the same time whether the direct
   write (outcome (a)) stays rejected or becomes supported, so the two
   families are consistent for the same shape. Also correct the
   `exception_body_outer_state` doc comment.
3. **Loop emitter consumes the list**: `VtLoopExtraSlot`/
   `vt_construct_extra_slot` read `ThreadedFamilies` instead of
   re-deriving the predicates; tuple layout unchanged; `.core` diff clean.
4. **Conditional emitter consumes the list**: `VtCondSlots`/
   `VtCondBaseline` become the list plus baseline versions; the locals
   machinery is untouched. Largest and riskiest; last.
5. **Side-channel audit**: catalogue `loop_threads_class_vars`,
   `last_loop_class_var`, `last_foldl_class_var_peak`, and the Foldl
   leading slot, and either route them through the list or document them
   as the per-family configuration that legitimately remains.

Each issue validates against `cargo test -p beamtalk-codegen`,
`just verify-threaded-ir`, `just test-bunit`/`test-stdlib`,
`just ci-changed`, and — from issue 2 onward — a clean issue-0 diff, with
any intentional shape change (issue 2's new slot) reviewed as a diff, not
assumed. A site that receives a family it does not carry is a
`VerifyError` reported through `report_threaded_ir_verify_errors`, never
a new `debug_assert!` at the call site.

**Sharper trigger for the excluded scope**, replacing ADR 0120's
twice-fired "next gap": fold Actor `State` in by migrating
`vt_construct_extra_slot` first (the "loops only" alternative) when either
a construct needs `StateAcc` to become conditionally absent, or a second
`State`-family gap is found in Actor loop/conditional emission; revisit
`match:`'s detector when it needs `SelfVt`. Each names the specific shape
that would reopen the scope this ADR narrows.

## Migration Path
Not applicable — this ADR changes no language-level behaviour; it scopes
an internal codegen refactor. BT-3506 changes observable behaviour (a
silently-dropped write becomes a kept one), which is a bug fix.

## References
- Related issues: [BT-3499](https://linear.app/beamtalk/issue/BT-3499)
  (this ADR's driving issue), BT-3506 (the site-3 `ClassVars` gap found
  during review), BT-3484/3486/3487/3488/3489 (the five call sites used
  as worked examples), BT-3490 (the epic that closed them and fired ADR
  0120's deferral trigger), BT-3491 (the rejection diagnostic's wording,
  which outcome (a) above also exhibits)
- Related ADRs: [ADR 0120](0120-value-type-self-threading-scope.md)
  (`SelfVt` threading; its Addendum 1 files BT-3499), ADR 0111
  (`ThreadedIr` verifier — the mechanism that caught outcome (c)), ADR
  0110 (`ClassVars` shadow-write; why `Bind` construction stays
  per-family), ADR 0041 (universal state-threading block protocol; the
  `StateAcc` scratch map)
- Code: `control_flow/analysis.rs` (detectors and shared rejection),
  `control_flow/plan.rs` (`ThreadingPlan.threads_class_vars`/
  `threads_value_self`, the Foldl leading slot), `value_type_codegen.rs`
  (`VtLoopExtraSlot`, `VtCondSlots`, `finish_vt_conditional_branch`),
  `control_flow/exception_handling.rs` (`exception_self_slot`,
  `exception_body_outer_state`), `control_flow/loop_mode.rs` (the
  `ClassVars`-only side channels), `gen_server/methods.rs`
  (`match_needs_mutation_threading`), `threaded_ir/ir.rs`
  (`VersionPrefix`), `threaded_ir/verify.rs`
- External: Cytron et al., "Efficiently Computing Static Single Assignment
  Form and the Control Dependence Graph" (1991); Erlang Reference Manual,
  "Expressions — Variables" (unsafe/exported variables in `case`/`try`)
