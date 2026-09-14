# ADR 0122: Scoping the `ThreadedIr` Storage-Family Generalization

## Status
Proposed (2026-09-14)

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
   `value_type_codegen.rs` for `ClassVars`/`SelfVt`) — the richest and
   most independent of the five. `is_conditional_with_vt_local_threading`/
   `is_conditional_with_vt_self_field_threading`
   (`value_type_codegen.rs:2593-2686`) plus `VtCondBaseline`/
   `VtCondSlots`/`build_vt_conditional_branch_pieces`/
   `finish_vt_conditional_branch` (`value_type_codegen.rs:2975-3778`, ~250
   lines) reinvent, by hand, almost exactly what `with_branch_context`
   already does for Actor `State` — a save/reset/restore discipline per
   arm, both-or-neither slot gating, fixed slot order.
3. **`on:do:`/`ensure:`** (`exception_handling.rs`) —
   `exception_blocks_thread_value_self` (`exception_handling.rs:209-216`)
   plus `push_exception_arm`/`exception_self_slot(_doc)`
   (`exception_handling.rs:88-146`) build the same `{Result, StateAcc,
   Self1}` shape as the loop's. This site has **no** `ClassVars` slot at
   all today — a class-method self-send inside a try/handler body threads
   through the ordinary `threaded_expression`/Bind-splicing path instead
   (`exception_handling.rs:482-491`). Whether that is safe by construction
   or a latent gap parallel to what BT-3486 fixed for `SelfVt` was not
   resolvable by static reading alone.
4. **`match:` arm** — `match_needs_mutation_threading`
   (`crates/beamtalk-codegen/src/core_erlang/gen_server/methods.rs:3251-3290`)
   is *recursive* (`control_flow_has_mutations`), unlike the other four's
   top-level-only walk, and supports only `State`/`ClassVars` — a
   value-type instance method's `self.field :=` inside a match arm is
   rejected outright (`match_lowering.rs:56-83`) because `match:` never
   built the N-arm merge machinery `ifTrue:`/`ifTrue:ifFalse:` have. Its
   emission for the two families it does support reuses the conditional's
   own branch-merge directly, with no tuple shape of its own.
5. **Letrec+Foldl rejection** —
   `reject_unthreadable_value_self_field_write` (`analysis.rs:241-280`) is
   already genuinely unified: one function, parameterized by one bool,
   called identically from both `lower_letrec_body` and
   `lower_foldl_body` (`body.rs:324`, `:690`).

At the **identity** layer, all three families are already unified:
`VersionPrefix::{State, ClassVars, SelfVt}`
(`crates/beamtalk-codegen/src/core_erlang/threaded_ir/ir.rs:151-187`) are
three variants of one enum feeding one `VersionedVar`/`VersionCounter`,
and `verify()`'s linearity/liveness walk (`threaded_ir/verify.rs`) treats
all three uniformly — its only prefix-specific branch is the ADR-0110
`ClassVars` shadow-write check. What is **not** unified is the *consumer*
logic listed above: two independent boolean fields
(`ThreadingPlan.threads_class_vars`/`threads_value_self`,
`control_flow/plan.rs:173,207`) computed by two independent predicates,
and hand-enumerated `None | ClassVars | ValueSelf` matches rather than a
data-driven "which storage families does this construct need to carry"
list.

### Constraints

- `just verify-threaded-ir` runs `ThreadedIr::verify()` over the full
  stdlib + bootstrap-test corpus and must stay clean. Critically, this is
  *necessary but not sufficient* evidence for a safe generalization:
  `verify()`'s linearity/liveness check would not catch a subtly-reordered
  or wrongly-gated slot, because a version graph can be perfectly
  well-formed (every version produced, used exactly once) while the
  *generated Core Erlang tuple shape* differs from before.
- Every one of the three "conditionally-appended slot" call sites (loop,
  conditional, `on:do:`/`ensure:`) carries a comment documenting a real
  regression from exactly this kind of predicate-widening — the loop's
  nested-self-send regression above, and BT-3492/BT-3493's shared
  statement-classifier bugs (ADR 0120 Addendum 1). A generalization has to
  reproduce each of those hard-won special cases exactly.
- Actor's `State`/`StateAcc` is structurally different from
  `ClassVars`/`SelfVt`, not just named differently — see Decision below.
  Any generalization spanning all three families has to model both
  shapes or explicitly not attempt to.

## Decision

**Generalize now, but scoped — not the full three-family generalization
ADR 0120 originally sketched.**

This answers the two questions ADR 0120's Constraints section left open:

**Does Actor's `State`/`StateAcc` fit the same abstraction as
`ClassVars`/`SelfVt`'s "extra slot"?** No, and the gap is structural, not
incidental. `State`/`StateAcc` is a real `gen_server` fun parameter,
present on every call whether or not anything mutates it
(`ThreadingPlan::initial_state_var`, `plan.rs:82`) — a non-mutating
construct carries it for free, with nothing to detect or gate.
`ClassVars`/`SelfVt` have no such standing parameter in class-method or
value-type context: every call site has to *detect* a mutation, then
*conditionally* append exactly one extra trailing slot (`{'nil',
StateAcc}` → `{'nil', StateAcc, ClassVars}` or `{'nil', StateAcc,
Self1}`, the two mutually exclusive by construction), with every
non-mutating sibling arm carrying a same-shaped, unchanged-value slot so
the fixed-at-compile-time `element/N` extraction stays valid regardless
of which arm ran. "Always-present parameter" vs. "conditionally-appended
slot" is a real fork in the calling convention. It is not a reason to
abandon generalizing `ClassVars`/`SelfVt` together; it is a reason to
leave `State` out of that generalization's scope rather than force a
third shape into it.

**Can a data-driven `Vec<VersionPrefix>` walk replace the five
hand-enumerated detectors/emitters without changing Actor/`ClassVars`
codegen output?** For three of the five sites — loop, conditional,
`on:do:`/`ensure:` — yes, plausibly: those three already share the
"conditionally-appended slot" shape identified above, and their detectors
are already close to identical (top-level-statement walk, bare
field-write/self-send match). `match:` resists folding into the same
unparameterized function: its detector is recursive where the others are
not, and its family support is asymmetric (2 of 3, not the other four's
symmetric-and-mutually-exclusive pair). That is a per-site configuration
knob, not a blocker, but it means the end state is "one generalized
mechanism with a small per-site config," not "zero per-site code" — and
it argues for leaving `match:`'s own detector alone rather than forcing it
into the shared walk.

Total hand-written logic across the five sites is roughly 250 lines of
detectors (~40% structurally identical, ~60% genuinely per-construct) and
~450-500 lines of emission (the conditional alone is about half of that).
Unify the `ClassVars`/`SelfVt` "conditionally-appended extra slot"
mechanism shared by the loop, the conditional, and `on:do:`/`ensure:`
into one `Vec<VersionPrefix>`-driven detector-and-emitter. Explicitly
keep two things out of scope:

- **Actor `State`/`StateAcc`** stays a special case. With `State`/Actor
  code paths untouched, the generalization cannot silently change Actor
  codegen, because it never runs on that path — this is the direct fix
  for the regression risk ADR 0120's Steelman Analysis worried about.
- **`match:`'s detector and family-support asymmetry** stays separate.
  Keep its own recursive detector and its rejection for the unsupported
  third family; only let its existing branch-merge reuse call into the
  generalized emitter's slot-append logic for the two families it already
  supports, as an optional follow-up, not a precondition.

## Prior Art

Internal only — as with ADR 0120, there is no external language/runtime
precedent for this specific `ThreadedIr` consumer-side duplication. The
relevant precedent is this codebase's own history: ADR 0111 built the
shared IR and verifier first, deliberately deferring "which constructs
actually route through it" to later, narrower issues; ADR 0120 continued
that discipline by patching three gaps narrowly rather than generalizing
under time pressure. This ADR continues the same discipline one step
further: use the concrete, now-fixed call sites as the design's test
material, and scope the generalization to the part of the problem
(`ClassVars`/`SelfVt`) the evidence actually supports unifying, rather
than the part (`State`) the evidence says is a different shape.

## User Impact

No end-user-visible impact, for the same reason ADR 0120 had none: every
site this ADR discusses is reachable only via `CodeGenContext::ValueType`
(the `TestCase` exemption, BT-1533) or the `Actor`/class-method contexts
that already compile correctly today. The affected persona is exclusively
a Beamtalk *contributor* working on `ThreadedIr` consumer code — loops,
conditionals, exception handling, list-ops, `match:` — who today has to
remember to ask "does this also need a `ClassVars`/`SelfVt` arm" for any
new construct, a question ADR 0120's own review process proved gets
missed. Scoping the generalization to three of five sites means a
contributor extending the loop, conditional, or `on:do:`/`ensure:`
mechanism benefits immediately; a contributor extending `match:` still
carries the existing manual burden, a known, accepted cost of this
decision rather than an oversight.

## Steelman Analysis

**For the full three-family generalization (including `State` and
`match:`):** every family and call site left outside the unified
mechanism is a place the "did we handle every storage family" question
can still be forgotten — exactly the failure mode that produced the
`match:` gap in the first place (found only because BT-3490's epic went
looking, not because CI caught it). A contributor extending `match:` to
support `SelfVt` as a third family, or a future construct that needs
`StateAcc` to become conditionally absent, still has to hand-write the
mechanism from scratch, having learned nothing new from the three sites
that did get unified.

**For scoping to `ClassVars`/`SelfVt` only (this ADR's decision):** `Q1`
found a genuine, load-bearing structural difference between `State`'s
always-present parameter and `ClassVars`/`SelfVt`'s conditionally-added
slot — folding `State` in anyway would mean building an abstraction that
has to represent "sometimes there's no parameter to add a version of at
all," which is a materially different problem from "sometimes there's an
extra slot to append." Attempting it now, under evidence that says it
doesn't cleanly fit, risks exactly the kind of premature, not-yet-settled
abstraction ADR 0120 warned against building under pressure. Excluding
`match:`'s detector similarly avoids retrofitting a recursive,
asymmetric-family-support shape into a walk designed around the other
four's top-level-only, symmetric shape — a forced fit there would cost
more in special-casing than it saves in unification.

**For deferring the whole thing again:** the three-site subset this ADR
recommends unifying is still a real, non-trivial migration against a
regression-sensitive corpus — `verify-threaded-ir` passing is not
sufficient evidence a generalization preserved exact codegen output (see
Constraints). A maintainer who weighs the corpus-wide validation cost more
heavily than the ~350 lines of duplication saved could reasonably pick
"defer again, revisit only if a sixth call site needs `State` or
`match:`'s asymmetry addressed" instead of committing to the scoped
generalization now.

**Where reasonable people disagree:** whether the `ClassVars`/`SelfVt`
subset is "unified enough to be worth doing now" versus "still risky
enough to defer a third time" is the same kind of judgment call ADR 0120
flagged for its original three-vs-next-instance threshold — this ADR
picks "now, but scoped" because the evidence (Q1's structural finding,
Q2's per-site detector/emitter reading) is concrete rather than
hypothetical, which is precisely the condition ADR 0120's Decision said
would justify revisiting.

## Alternatives Considered

### Full three-family generalization (State + ClassVars + SelfVt + match:)
Unify all five call sites, including Actor `State`/`StateAcc` and
`match:`'s recursive/asymmetric detector, under one mechanism. **Rejected
for now**: Q1 found `State`'s "always-present parameter" shape genuinely
does not fit the "conditionally-appended slot" abstraction the other two
families share, and `match:`'s recursive, 2-of-3-family detector would
need enough special-casing to undermine the unification's value. Forcing
either in now risks building an abstraction around a not-yet-settled
shape, the exact risk ADR 0120 warned about.

### Defer again, with the same open-ended "next gap" trigger
Continue ADR 0120's original deferral without scoping anything. **Rejected**:
that trigger has already fired twice (ADR 0120 Addendum 1) without the
generalization work happening, and BT-3499 was filed specifically to stop
that cycle by producing a concrete decision — deferring a third time with
the same unsharpened trigger would repeat the pattern rather than resolve
it.

### Scoped generalization limited to loop + conditional only (no `on:do:`/`ensure:`)
ADR 0120 itself considered this narrower scope (its "Scoped,
mechanism-only generalization" alternative) and rejected it because it
wouldn't fix the Foldl/exception-handling gaps found at the time. Those
gaps are now fixed independently (BT-3486, BT-3487/3488's shared
rejection), but `on:do:`/`ensure:`'s extra-slot emission
(`push_exception_arm`/`exception_self_slot`) is structurally the same
"conditionally-appended slot" shape as the loop's and the conditional's —
excluding it from this ADR's scope would leave a third, still-duplicated
copy of the same mechanism for no structural reason. **Rejected**:
include `on:do:`/`ensure:` in the scoped generalization; the loop +
conditional-only boundary made sense against 2026-09-09's gap count, not
against the fuller five-site picture this ADR works from.

## Consequences

### Positive
- The three sites that share the "conditionally-appended slot" shape
  (loop, conditional, `on:do:`/`ensure:`) converge on one detector and one
  emitter, removing ~350-400 lines of duplicated logic and the specific
  regression class (predicate-widening missing a sibling call site) that
  produced BT-3484's follow-up bugs.
- `State`/Actor codegen paths are untouched by construction, since they
  are out of scope — no risk of silently changing already-passing Actor
  programs from this generalization.
- `match:` keeps its own correctly-scoped detector rather than being
  force-fit into an abstraction that doesn't match its shape, leaving the
  door open to route its two supported families through the generalized
  emitter later without committing to that now.

### Negative
- `match:` remains a standalone, hand-written mechanism — a future
  contributor extending it still faces the original "remember every
  storage family" burden this ADR does not resolve for that site.
- The `on:do:`/`ensure:` triage item (does a class-method self-send inside
  a try/handler body actually thread `ClassVars` correctly today?) is an
  open question this ADR surfaces but does not answer; it has to be
  resolved as part of, or before, generalizing that site.
- The migration's validation cost (byte-identical `Document`-output
  diffing over the full corpus, not just `verify-threaded-ir`) is real
  engineering work, not a free refactor.

### Neutral
- Total implementation cost is spread across five smaller issues rather
  than one large one, mirroring the size and validation bar BT-3486/
  3487/3488/3489 already established — consistent with, not a departure
  from, ADR 0120's own issue-by-issue discipline.

## Implementation

Recommended issue breakdown (final split TBD at pick-up time, each sized
like BT-3486/3487/3488/3489 — roughly one PR):

1. Extract one `Vec<VersionPrefix>`-parameterized top-level-statement
   mutation detector, replacing
   `find_class_var_mutating_stmt`/`find_value_self_mutating_stmt`
   (already near-identical). Land as a pure refactor gated by a
   differential test asserting the old and new detectors agree on every
   construct in the stdlib + bootstrap-test corpus, before anything is
   wired to depend on it.
2. Triage whether `on:do:`/`ensure:` has a latent `ClassVars`-in-try-body
   gap parallel to BT-3486's `SelfVt` one — resolve or explicitly rule out
   before the generalized emitter has to decide whether that site ever
   needs a `ClassVars` slot.
3. Generalize the loop's `VtLoopExtraSlot`/`vt_construct_extra_slot`
   emission to consume the new detector plus a data-driven ordered slot
   list. Smallest of the three emitters — do this one first to prove the
   pattern.
4. Generalize `on:do:`/`ensure:`'s `push_exception_arm`/
   `exception_self_slot(_doc)` the same way.
5. Generalize the conditional's `VtCondBaseline`/`VtCondSlots`/
   `build_vt_conditional_branch_pieces`/`finish_vt_conditional_branch` —
   largest and riskiest; do it last, once issues 3 and 4 have validated
   the shared emitter against two simpler sites.

Each issue validates against `cargo test -p beamtalk-codegen`,
`just verify-threaded-ir`, `just test-bunit`/`test-stdlib`,
`just ci-changed`, and — because `verify-threaded-ir` alone is not
sufficient per the Constraints section above — a byte-identical
`Document` output diff between the old and new emitter over the full
stdlib + bootstrap-test corpus, with the old emitter only deleted once
that diff is clean.

**Sharper trigger for the excluded scope**, replacing ADR 0120's
already-twice-fired "next gap" trigger: revisit folding `State` into the
same abstraction if a sixth call site needs `StateAcc` to become
conditionally-absent the way `ClassVars`/`SelfVt` are (unlikely on
current evidence — `gen_server` methods always take `State`); revisit
`match:`'s detector if it needs to support `SelfVt` as a third family
(its own sixth call site). Each names the specific shape that would
justify reopening the scope this ADR deliberately narrows, rather than
any new gap in general.

## Migration Path
Not applicable — this ADR changes no existing behavior; it scopes a
future internal refactor of codegen-internal mechanisms with no
user-visible surface.

## References
- Related issues: [BT-3499](https://linear.app/beamtalk/issue/BT-3499)
  (this ADR's driving issue), BT-3484/3486/3487/3488/3489 (the five call
  sites used as worked examples), BT-3490 (the epic that closed them and
  fired ADR 0120's deferral trigger)
- Related ADRs: [ADR 0120](0120-value-type-self-threading-scope.md)
  (`SelfVt` threading, its Addendum 1 records the trigger firing and
  files BT-3499), ADR 0111 (`ThreadedIr` verifier and its own
  issue-by-issue design discipline), ADR 0110 (`ClassVars` shadow-write
  mechanism)
- Code: `crates/beamtalk-codegen/src/core_erlang/value_type_codegen.rs`
  (`VtLoopExtraSlot`, `VtCondBaseline`/`VtCondSlots`), `control_flow/plan.rs`
  (`ThreadingPlan.threads_class_vars`/`threads_value_self`),
  `control_flow/analysis.rs` (the five detectors),
  `exception_handling.rs` (`push_exception_arm`/`exception_self_slot`),
  `gen_server/methods.rs` (`match_needs_mutation_threading`),
  `threaded_ir/ir.rs` (`VersionPrefix`), `threaded_ir/verify.rs`
