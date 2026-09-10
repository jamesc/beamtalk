# ADR 0120: Value-Type `Self` Threading — Patch the Remaining Gaps, or Generalize `ThreadedIr`'s Storage Families?

## Status
Proposed (2026-09-09)

## Context

### Problem statement

`self.field := value` in `CodeGenContext::ValueType` threads through `Self`/`SelfN`
(`VersionPrefix::SelfVt`) — a version chain that started life (BT-3466) as a
single, unthreaded emission: correct for a field write not inside any loop or
conditional, wrong (silently dropped, or an `erlc` crash) for one that is.
BT-3483/BT-3484 fixed the two shapes BT-3484's own repro named — a
`self.field :=` as the sole top-level statement of a `to:do:`-style Letrec
loop body, and as a top-level, non-last statement of an `ifTrue:`/
`ifTrue:ifFalse:` conditional — by mirroring the two existing precedents for
the identical problem with `ClassVars` (BT-3168 for loops, BT-3159 for
conditionals): a hand-built extra `letrec` parameter / result-tuple slot for
loops, a hand-built branch-merge tuple slot for conditionals. `SelfVt` is now
the **third** storage family (after `State`, `ClassVars`) with its own,
independently-written copy of this "thread a mutation through a construct"
mechanism.

Reviewing BT-3484's landed fix for consistency (rather than taking the PR's
own scope statement at face value) found three more shapes broken the same
way, none touched by that PR, none a regression it introduced (each
reproduces identically on the pre-BT-3484 commit):

1. A conditional **nested inside** a loop
   (`1 to: 5 do: [:i | flag ifTrue: [self.total := ...]]]`) — `erlc: unbound
   variable 'State'`. The identical shape for a **class variable** instead
   gets a clean, safe "Cannot assign to field" compile-time rejection — so
   this is also an inconsistency between two storage families for the same
   AST shape, not just a missing feature.
2. A `do:`/`collect:`/`select:`/etc. (Foldl-shaped) loop with `self.field :=`
   directly in its block — `erlc: unbound variable 'State'`. Foldl's
   accumulator has no matching extra slot at all yet (pre-existing BT-3169
   tech debt shared with `ClassVars`).
3. `on:do:`/`ensure:` with `self.field :=` in the try body — compiles
   cleanly and **silently drops the mutation**. Confirmed via the generated
   Core Erlang: the `Self1` binding is computed and discarded; the
   post-`ensure:` read sees the pre-`ensure:` `Self`. The Actor equivalent
   (`state.field :=` in the identical shape) threads correctly, via the same
   `StateAcc` mechanism its outer-local threading already uses — so, same
   root cause as BT-3484, a fourth location.

Pattern: every construct family that threads state (loops, conditionals,
Foldl list-ops, exception handling) has its own hand-written logic for
"does this body mutate `State`?" / "does it mutate `ClassVars`?", and now
"does it mutate `Self`?" is a third, separately-maintained answer to the same
question, bolted on per family, per issue (BT-3168, BT-3159, BT-3484, and —
if patched the same way — three more issues for the shapes above). This is
the duplication CLAUDE.md's "Duplication & the Shared-Leaf-Module Pattern"
section names directly: "Module X sits below Y in the dependency graph" is
not a reason to duplicate; the shared mechanism belongs in one place both
depend on.

### Why the severity is bounded

`self.field := value` compiles under `CodeGenContext::ValueType` **only**
via the `TestCase` exemption (BT-1533) — ADR 0042 makes it an unconditional
compile error on every other `Value subclass:`, regardless of position, and
`FieldWriteSite::for_context` never returns `ValueType` for `Actor`/`Repl`
context. So every shape in this ADR — the two BT-3484 fixed and the three
found reviewing it — is reachable **only** by a BUnit test fixture mutating
its own `field:`, never by a real `Value` object at runtime. Nothing here is
a production correctness bug. The worst case is a confusing compile error
(shapes 1-2) or a test that silently asserts against stale data and passes
when it should fail (shape 3, the `ensure:` case) — a testing-infrastructure
integrity problem, not a customer-facing one.

### Constraints

- `ThreadedIr::verify()` runs over the full stdlib + bootstrap-test corpus
  (`just verify-threaded-ir`) and must stay clean — any generalization has a
  large, real regression surface to prove itself against, not just the
  handful of shapes named above.
- `VersionPrefix` (`ir.rs`) already models `State`/`ClassVars`/`SelfVt`/
  `Local`/`Gensym` as one enum with uniform version-driven rendering — the
  *identity* layer is already unified. What is **not** unified is the
  *consumer* logic: `ThreadingPlan.threads_class_vars` /
  `threads_value_self` are two independent boolean fields computed by two
  independent predicates (`loop_body_threads_class_vars` /
  `loop_body_threads_value_self`); `VtLoopExtraSlot`/`VtBranchPieces` are
  hand-enumerated two-or-three-way matches (`None | ClassVars | ValueSelf`)
  rather than a data-driven set of "which storage families does this
  construct need to carry"; `while_loops.rs`/`counted_loops.rs` build the
  `letrec` parameter list and result tuple shape by hand per family.
- Actor's own `State`/`StateAcc` mechanism is a special case even among the
  three: it is *always* the ambient scratch map (outer locals and field
  writes share it for free), where `ClassVars`/`SelfVt` are each an
  *additional*, conditionally-present slot layered on top. A full
  generalization has to model both shapes (the "free rider" and the
  "extra slot") under one abstraction, or explicitly decide not to unify
  Actor's case and only unify the `ClassVars`/`SelfVt` "extra slot" pair.

## Decision

**Fix the three confirmed gaps now, using the same narrow, mirrored-precedent
style BT-3168/BT-3159/BT-3484 already established** (one issue per gap, or
one issue covering all three if they turn out to share a single root fix —
TBD at implementation time). Do **not** undertake the full `ThreadedIr`
storage-family generalization as a precondition for those fixes.

Treat the generalization as a separately-tracked, explicitly-deferred
option, not a rejected one: **the trigger to revisit is the next new gap in
this family** (a fourth distinct call site, or a fourth storage family).
Three independent instances of the identical mechanism (`State`, `ClassVars`,
`SelfVt`) is exactly the "rule of three" threshold past which continuing to
hand-copy stops being the cheaper option — but we are at three, having just
paid the cost of writing the third copy, not yet past it. Piling a fourth
(Foldl) and fifth (exception handling) *targeted* copy on top to close the
found gaps is consistent with where we already are; committing to a full
consumer-side generalization *right now*, under time pressure from three
newly-found bugs, is how a rushed abstraction gets built around a
not-yet-fully-understood problem shape.

## Prior Art

Internal only — there is no external language/runtime precedent for this
specific `ThreadedIr` consumer-side duplication; the relevant precedent is
this codebase's own history: ADR 0111 built the shared IR and verifier
first, deliberately deferring "which constructs actually route through it"
to later, narrower issues (its own Addendum 9 documents the `ClassVars`
loop-threading design questions being resolved against compiled repros,
issue by issue, rather than speculatively up front). This ADR proposes
continuing that same discipline: let real, found gaps drive the design of a
generalization, rather than generalizing ahead of the third data point
turning into a fourth.

## User Impact

No end-user-visible impact either way — see "Why the severity is bounded"
above. The affected persona is exclusively a BeamTalk *contributor* writing
or debugging a BUnit `TestCase` fixture that mutates its own `field:` inside
a loop, conditional, list-op, or `ensure:`/`on:do:` block. Patching narrowly
gets each contributor's actual blocker fixed fastest; deferring the
generalization means each of the three follow-up fixes will look, in the
diff, like "yet another copy of the same mechanism" — a known, accepted cost
of this decision, not an oversight.

## Steelman Analysis

**For generalizing now:** every week this stays unfixed at the mechanism
level is a week in which the next contributor to touch loops, conditionals,
Foldl, or exception-handling threading has to remember to ask "does this
also need a `SelfVt`/`ClassVars` arm" — and BT-3484's own review just proved
that question gets missed (three sibling gaps, found only because someone
went looking, not because CI caught them). A generic abstraction makes that
question impossible to forget, because there is no per-family arm left to
add. The cost of three narrow follow-up fixes plus the eventual
generalization (paid twice) is real, and could be avoided by paying for the
generalization once, now, while the three concrete shapes are freshly
understood as test cases.

**For patching narrowly:** the three gaps are `TestCase`-only, low-severity,
and independently fixable in isolation, each in roughly the shape BT-3484
already validated end-to-end (compile, `verify-threaded-ir`, ground-truth
runtime check). A generalization has to get the Actor "free rider" case and
the `ClassVars`/`SelfVt` "extra slot" case both right, prove itself against
the *entire* stdlib + bootstrap-test corpus (not three new test cases), and
risks silently changing Actor/`ClassVars` codegen output for the thousands
of already-passing programs that exercise it today — a much larger,
higher-consequence change to justify for a bug class that cannot reach
production code.

**Where reasonable people disagree:** whether "three instances" or "the next
instance" is the right generalization trigger is a judgment call, not a
provable threshold — this ADR picks "the next instance" deliberately, but a
maintainer who weighs the review-time cost of re-discovering sibling gaps
more heavily than the refactor risk could reasonably pick "now" instead.

## Alternatives Considered

### Full generalization now
Replace `VtLoopExtraSlot`/`VtBranchPieces`'s hand-enumerated `None |
ClassVars | ValueSelf` matches with a data-driven `Vec<VersionPrefix>` (or
equivalent) of "extra threaded storages" computed once per construct, and
extend `while_loops.rs`/`counted_loops.rs`'s `letrec` parameter/tuple-shape
construction, plus the Foldl and exception-handling consumers, to walk that
list generically instead of hand-matching each family. Fixes the three found
gaps and prevents a fourth. **Rejected as the immediate next step**: real
design work (how does Actor's "free rider" `StateAcc` fit the same
abstraction as `ClassVars`/`SelfVt`'s "extra slot"?) against a large
regression surface, undertaken under the pressure of three fresh bugs rather
than a settled design — see Decision above for the deferral trigger.

### Scoped, mechanism-only generalization (loops + conditionals only)
A middle ground: generalize just the loop/conditional "extra slot"
mechanism `VtLoopExtraSlot`/`VtBranchPieces` already share between
`ClassVars`/`SelfVt` (smaller surface than full `ThreadedIr`, since it stays
inside `value_type_codegen.rs`/`plan.rs`, the files BT-3484 already
touched), without attempting to also unify Foldl or exception-handling in
the same pass. **Rejected for now, revisit if the "next instance" trigger
fires on a loop/conditional-shaped gap specifically** (rather than the
Foldl/exception-handling gaps already found, which a loop/conditional-only
generalization wouldn't fix anyway) — narrowing the generalization's scope
to match the two mechanisms already duplicated doesn't remove the need to
separately patch Foldl and exception-handling threading, so it doesn't
avoid paying for three follow-up fixes either; it only reduces one of them
to a smaller diff.

### Do nothing (leave the three gaps unfixed)
**Rejected**: shape 3 (`ensure:`/`on:do:` silent drop) is a silent-failure
mode in test infrastructure, which is worse than a compile error — a
contributor's test can pass while asserting against stale data. Low
severity is not zero severity.

## Consequences

### Positive
- Each of the three fixes lands independently, reviewable and revertible on
  its own, in the same validated shape (`verify-threaded-ir` +
  `just ci-changed` + a hand-checked runtime ground truth) BT-3484 already
  proved out.
- No new risk introduced to Actor/`ClassVars` codegen paths, which a
  generalization would necessarily touch.
- Keeps the door open to generalize later with three-plus concrete,
  already-fixed shapes as design/test material, rather than speculating
  ahead of them.

### Negative
- A fourth (and, if Foldl and exception-handling are separate issues, a
  fifth) hand-copied implementation of "thread a mutation through a
  construct" lands in the codebase, exactly the duplication this ADR
  acknowledges as a real cost.
- Whoever eventually does the generalization inherits four-or-five
  call sites to reconcile instead of two.

### Neutral
- Total near-term implementation cost (three narrow fixes) is likely lower
  than the generalization's, but that comparison inverts if a fourth
  distinct gap is found before the generalization is undertaken — this is
  the accepted bet the Decision section makes explicit.

## Implementation

Recommended near-term issues (final split TBD at pick-up time):
1. `ensure:`/`on:do:` value-type `self.field :=` silent drop — highest
   priority given the silent-failure severity. Seed the construct's scratch
   map from `Self` when the try/handler body contains a field write
   (mirroring BT-3177's existing empty-map seed for the locals-only case),
   thread and rebind `Self` afterward, mirroring BT-3484's loop/conditional
   rebind.
2. Foldl-shaped list-op (`do:`/`collect:`/etc.) with a top-level
   `self.field :=` in its block — needs the accumulator to carry an extra
   `Self` slot the way BT-3169 never gave `ClassVars` either; likely blocked
   on, or shares a fix with, that pre-existing `ClassVars`-in-Foldl gap.
3. Conditional nested inside a loop — at minimum, extend the existing
   "Cannot assign to field... inside this block" safety-net check
   (`FieldAssignmentInUnsupportedBlock` or its sibling) to catch this
   `ValueType` shape the same way it already catches it for `ClassVar`,
   turning the `erlc` crash into the same clean, pre-existing diagnostic —
   full support (rather than a clean rejection) is a nice-to-have, not
   required by this ADR's severity assessment.

Each should follow BT-3484's own validation bar: `cargo test -p
beamtalk-codegen`, `just verify-threaded-ir`, `just test-bunit`/
`test-stdlib`, `just ci-changed`, and a hand-checked runtime ground truth
via the compiled BEAM — not just "compiles."

## Migration Path
Not applicable — no existing behavior changes; these are bug fixes to
currently-broken (crash or silent-drop) shapes.

## References
- Related issues: BT-3466 (`FieldWriteSite`/`SelfVt` introduced), BT-3483
  (found the original gap via test-coverage investigation), BT-3484 (fixed
  the two top-level shapes; this ADR reviews its aftermath)
- Related ADRs: ADR 0042 (Value type immutability — why this whole code path
  is `TestCase`-only), ADR 0111 (`ThreadedIr` verifier and its own
  issue-by-issue design discipline, cited as prior art above), ADR 0110
  (`ClassVars` shadow-write mechanism BT-3484 partly mirrors)
- Code: `crates/beamtalk-codegen/src/core_erlang/value_type_codegen.rs`
  (`VtLoopExtraSlot`, `VtBranchPieces`), `control_flow/plan.rs`
  (`ThreadingPlan.threads_class_vars`/`threads_value_self`),
  `control_flow/analysis.rs` (`loop_body_threads_class_vars`/
  `loop_body_threads_value_self`), `threaded_ir/ir.rs` (`VersionPrefix`)
