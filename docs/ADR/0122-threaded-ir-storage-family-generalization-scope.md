# ADR 0122: Unify `ThreadedIr` Storage-Family Threading

## Status
Accepted (2026-09-15)

## Context

Three storage families get threaded through control-flow constructs: Actor
`State`, `ClassVars`, and value-type `SelfVt`. ADR 0120 deferred unifying
the per-construct "does this body mutate family X, and how do I carry it
out" logic, with a trigger of "the next gap." The trigger fired
([BT-3490](https://linear.app/beamtalk/issue/BT-3490) closed four gaps),
and [BT-3499](https://linear.app/beamtalk/issue/BT-3499) asked two
questions: does `State` fit the same abstraction as the other two, and can
one data-driven `Vec<VersionPrefix>` replace the hand-written detectors and
emitters without changing codegen output for passing programs?

### What is hand-written today

| Site | Detector | Emitter | Families | Slot position |
|------|----------|---------|----------|---------------|
| Loop (`counted_loops.rs`/`while_loops.rs`) | `loop_body_threads_class_vars` / `loop_body_threads_value_self` — two near-identical top-level-statement walks | `vt_construct_extra_slot`: `None \| ClassVars \| ValueSelf` | ClassVars, SelfVt (exclusive) | trailing |
| Conditional, value-type (`value_type_codegen.rs`) | `is_conditional_with_vt_local_threading` / `..._self_field_threading` | `VtCondSlots`, `finish_vt_conditional_branch`: two ~15-line arms after the locals | ClassVars, SelfVt | trailing |
| Conditional, Actor (`conditionals.rs`) | `needs_mutation_threading` | `with_branch_context`, six `generate_*_with_mutations` | State | element 2 |
| `on:do:`/`ensure:` (`exception_handling.rs`) | `exception_blocks_thread_value_self` | `exception_self_slot` | **SelfVt only** | trailing |
| `match:` (`gen_server/methods.rs`) | `match_needs_mutation_threading` — recursive, unlike the others | reuses the conditional's merge | State, ClassVars (no SelfVt) | n/a |
| Foldl list-ops (`plan.rs`) | `threads_class_vars`, Foldl shape | fold accumulator | ClassVars | **leading** |

Plus three `ClassVars`-only side channels on `LoopMode`
(`loop_threads_class_vars`, `last_loop_class_var`,
`last_foldl_class_var_peak`) with no `SelfVt` counterpart.

Every row differs from every other row in at least one column. That is
six sites, three detection rules, two slot positions, and a family matrix
with holes in it.

The *identity* layer is already unified: `VersionPrefix::{State, ClassVars,
SelfVt}` is one enum, and `ThreadedIr::verify()` treats all three the same
(its only family-specific check is ADR 0110's `ClassVars` shadow write).
Only the consumer logic above is duplicated.

### `State` already fits

`StateAcc` is element 2 of the same result tuple the other families are
appended to (`{'nil', StateAcc[, ClassVars | Self1]}`), and the Actor
conditional merge uses the same both-or-neither arm discipline
(`{Value, StateAcc1}` in the mutating arm, `{'nil', State}` in the other).
In a slot-list model it is the trivial case: the predicate is always true.
It is on a separate code path because it was written first, not because
it is different.

### A sixth gap

Answering "does site 3 have a `ClassVars` gap?" with a compiled repro
(class-method context):

```beamtalk
Object subclass: CvProbe
  classState: runs = 0

  class bump => self.runs := self.runs + 1

  // compiles; returns 0; class var stays 0
  class selfSendEnsure =>
    self.runs := 0
    [
      self bump
      nil
    ] ensure: [nil]
    self.runs

  // ThreadedIr verify failure: ClassVars1 consumed, never produced
  class selfSendEnsureWithLocal =>
    self.runs := 0
    seen := 0
    [
      self bump
      seen := seen + 1
    ] ensure: [nil]
    self.runs
```

The self-send's `ClassVars2` is bound inside the try-body fun and discarded;
the post-`ensure:` read uses the pre-try `ClassVars1`. Same for the cleanup
block, the `on:do:` body and handler, last and non-last position. The site's
own doc comment (`exception_body_outer_state`, `exception_handling.rs:484-491`)
says class-var mutations are "threaded entirely separately" and never need a
slot here; the generated code above is the counterexample. A direct
`self.runs :=` in the same place is a compile-time rejection — so `SelfVt`
is *supported* here (BT-3486) while `ClassVars` is *rejected* or *dropped*.
This is BT-3486 for `ClassVars`, it is in production-reachable code (class
vars are ordinary code, ADR 0110), and BT-3490's matrix missed it because
its `on:do:` fragments assert a local, not the class var. Filed as
[BT-3506](https://linear.app/beamtalk/issue/BT-3506).

### Why the gaps keep happening

BT-3489 (`match:`) and BT-3506 are the same mistake: a site was allowed to
ask about *some* families, and the person adding the site — an agent, in
both cases — asked about the ones in front of them. Every exception in the
table above is a place that mistake can recur. The maintenance cost model
for this codebase is "agents forget unwritten rules," and the architecture
that survives that is one with no unwritten rules: one detector, one slot
list, one emission helper, and per-site differences expressed as data the
type checks rather than as code someone has to remember exists.

### Constraints

- `just verify-threaded-ir` is `test-stdlib` + `test-bunit` under
  `debug_assertions`. It catches invariant violations (it caught the second
  repro shape). It does **not** catch a changed tuple shape or slot order,
  and there is no byte-identical `.core` diff over the corpus today. Without
  one, migrating the Actor path is a leap; with one, it is a diff you read.
- The scope is production-reachable: `ClassVars` in class methods, and all
  of Actor `State`.
- The measured duplication is small (~140 lines of detectors, ~300 of
  emitters, most of the latter outer-local threading). Line count is not
  the payoff; the absence of exceptions is.

## Decision

**One detector, one slot list, one emission helper, for all three families
and all six sites. Per-site differences become capability data. Build the
`.core` diff harness first and migrate one site at a time, byte-identical
except where the change is the point.**

1. **One detector**, recursive everywhere:
   `body_threaded_families(body, eligible) -> ThreadedFamilies`, in
   `control_flow/analysis.rs`. The top-level-only walks go away. A site
   that cannot carry a nested mutation *rejects* it via the existing shared
   rejection function — capability is what varies between sites, not
   detection.
2. **One `ThreadedFamilies` type**: an ordered `Vec<VersionPrefix>`. Slot 2
   is always the scratch map (`State` in Actor context, an empty map
   elsewhere); `ClassVars` and `SelfVt` follow in canonical order;
   **trailing everywhere**. Foldl's leading slot is normalized to trailing
   as part of its migration.
3. **One emission helper** that appends the families' slots to a
   construct's result tuple and extracts them afterwards, used by every
   site including the Actor conditional and loops. `with_branch_context`,
   the six `generate_*_with_mutations`, `VtCondSlots`,
   `vt_construct_extra_slot`, `exception_self_slot`, and the Foldl
   accumulator path all route through it.
4. **Per-site capabilities are data.** Each site declares which families it
   can carry; the helper rejects the rest. `match:` declares
   `[State, ClassVars]`, so its `SelfVt` rejection is driven by the
   declaration, not a hand-written arm. Family-specific binding
   (`shadow_write: true` for `ClassVars`, `maps:put` for `SelfVt`) lives on
   `VersionPrefix`, once.
5. The three `ClassVars`-only side channels are folded into the list or
   deleted.
6. A site that receives a family it does not carry fails `verify()`. It
   already does; no new `debug_assert!`.

```rust
// control_flow/analysis.rs
/// Storage families a construct body mutates, in canonical slot order.
pub(in crate::core_erlang) struct ThreadedFamilies(Vec<VersionPrefix>);

fn body_threaded_families(&self, body: &[Expression], eligible: &[VersionPrefix])
    -> ThreadedFamilies

// per-site capability, data not code
const MATCH_ARM_FAMILIES: &[VersionPrefix] = &[VersionPrefix::State, VersionPrefix::ClassVars];
```

Precondition for every migration step: the corpus `.core` diff is empty,
or the non-empty diff is the intended change (BT-3506's new slot, the Foldl
slot move) and is reviewed as such.

## Prior Art

Two precedents solve the same problem the same way: compute the set of
things that need a merge, then merge that set.

- **SSA construction** places φ-functions per join point for the set of
  variables assigned on some path (Cytron et al.). Nobody hand-writes a
  φ arm per variable. **Adopted:** the set-driven model.
- **Erlang's compiler** computes the variables bound in each clause of
  `case`/`try` and rejects one bound in some clauses but not all as
  "unsafe." That is the both-or-neither discipline every branch site here
  reimplements. It is also why site 3 needs a slot at all: bindings inside
  `try` are not visible after it. **Adopted:** the discipline, as a
  computed set.
- **Full SSA over locals:** not adopted. ADR 0041/0111's `StateAcc`
  scratch map already handles locals and this ADR does not reopen that.

## User Impact

None from the unification itself. The persona is a contributor — usually an
agent — adding a construct or a family. Today they have to know which of
six sites ask about which families. Afterwards there is one detector, one
list, and a capability declaration the compiler checks; forgetting fails
the first corpus compile.

BT-3506 has user impact until it lands: a class method that mutates a class
var via a self-send inside `ensure:` silently loses the write. Smalltalk
developers will find that surprising because `ensure:` is the "this must
happen" idiom; Erlang developers will spot the `try` scoping immediately.

## Steelman Analysis

| Alternative | Strongest case for it | Why not |
|-------------|----------------------|---------|
| Scope to `ClassVars`/`SelfVt`; leave `State` and `match:` alone | `State`'s emitters are the production Actor path and already work. `match:` is recursive and 2-of-3. Smallest blast radius. | Leaves six exceptions in place, and both real gaps were an exception someone did not know about. With the `.core` harness, migrating the Actor path is mechanical and diff-verified — exactly the work agents do well. The risk is one-time; the exceptions are forever. |
| Merge nothing; unify the detector only | Almost all the "can't forget" value at a fraction of the risk. | A detector nobody reads changes nothing. Site 3 is where families get forgotten — the emitters have to consume the list. |
| Keep top-level-only detection at loop sites | It was narrowed after a real regression (`tickInLoopConditional`). | The regression was a *carry* problem, not a *detect* problem: the loop could not thread a nested write. Rejecting nested writes at sites that cannot carry them — which the shared rejection function already does — keeps the safety and removes the second detection rule. |

Where reasonable people disagree: whether BT-3506 should be the first
consumer of the new mechanism (proves it on the motivating shape) or a fast
BT-3486-style patch landed first and refactored later.

## Alternatives Considered

### Scoped generalization (`ClassVars`/`SelfVt` only)
This ADR's own first draft. Rejected: it optimizes for the risk of one
migration over the cost of every future change, and the evidence says
future changes are where the bugs come from. See Steelman.

### Detectors only
Rejected as an end state; adopted as the first step. See Steelman.

### Keep Foldl's leading slot
Rejected. Two slot positions is one rule too many. The move is a visible
`.core` diff reviewed on its own issue.

### Defer again
Rejected. ADR 0120's "next gap" trigger has fired twice since it was set:
BT-3489 during BT-3490, and BT-3506 during this ADR.

## Consequences

### Positive
- No unwritten rules: one detector, one list, one helper, capabilities as
  data. A site that ignores a family fails `verify()` at development time.
- BT-3506 gets fixed as the first consumer, on the shape that motivated it.
- Actor `State`, `ClassVars`, and `SelfVt` threading are one mechanism,
  which is what ADR 0111's identity layer already assumed.
- A corpus `.core` diff exists afterwards for every future codegen
  refactor.

### Negative
- XL, not L: roughly ten issues, three of them touching the Actor path.
- Net line count is roughly unchanged.
- Two intentional codegen changes (BT-3506's slot, the Foldl slot move)
  have to be reviewed as diffs rather than proven identical.
- Until the migration completes, the codebase has both the old paths and
  the new helper. Each issue deletes what it replaces so the overlap is
  one site at a time, never all of them.

### Neutral
- Per-family binding still differs (`shadow_write`, `maps:put`). It lives
  on `VersionPrefix`, once, not at the sites.

## Implementation

Each issue is one PR, sized like BT-3486/3487/3488/3489, and deletes the
code it replaces.

0. **Corpus `.core` diff harness.** A `just` recipe that compiles the
   stdlib + bootstrap-test corpus with `.core` retained
   (`beam_compiler.rs` already writes them) and diffs the tree against
   `main`. Nothing below starts without it.
1. **Detector and `ThreadedFamilies`** in `analysis.rs`, recursive, gated
   by a differential test against the old detectors on every construct in
   the corpus (the only expected differences are nested mutations the old
   walks did not report, which the rejection function turns into the same
   errors as today).
2. **Emission helper** (append slots, extract slots), unit-tested on the
   three tuple shapes in the table.
3. **BT-3506** as the first consumer: `on:do:`/`ensure:` grows a
   `ClassVars` slot, driven by the list. Decide whether the direct-write
   shape becomes supported or stays rejected; both families must agree.
   Fix the `exception_body_outer_state` doc comment. Intended diff.
4. **Value-type loop** onto the helper (`VtLoopExtraSlot`,
   `vt_construct_extra_slot`). Identical diff.
5. **Value-type conditional** onto the helper (`VtCondSlots`,
   `VtCondBaseline`, the two family arms). Identical diff.
6. **Actor conditional** onto the helper (`with_branch_context`, the six
   `generate_*_with_mutations`). Identical diff.
7. **Actor and class-method loops** onto the helper (`while_loops.rs`,
   `counted_loops.rs` `letrec` parameter and result tuple). Identical diff.
8. **Foldl** onto the helper; slot moves from leading to trailing.
   Intended diff.
9. **`match:`** declares `[State, ClassVars]` and drops
   `match_needs_mutation_threading` for the shared detector. Identical
   diff.
10. **Side channels**: fold `loop_threads_class_vars`, `last_loop_class_var`,
    `last_foldl_class_var_peak` into the list or delete them. Identical
    diff.

Every issue: `cargo test -p beamtalk-codegen`, `just verify-threaded-ir`,
`just test-bunit`/`test-stdlib`, `just ci-changed`, and the issue-0 diff —
empty, or the intended change and nothing else.

## Migration Path
Not applicable to the language. BT-3506 changes observable behaviour (a
dropped write becomes a kept one); that is a bug fix. The Foldl slot move
changes generated code, not behaviour.

## References
- Related issues: BT-3499 (this ADR), BT-3506 (site-3 gap), BT-3484/3486/
  3487/3488/3489 (the sites), BT-3490 (the epic), BT-3491 (the rejection
  diagnostic's wording, which the direct-write shape also hits)
- Related ADRs: [0120](0120-value-type-self-threading-scope.md) (`SelfVt`
  threading; Addendum 1 files BT-3499), 0111 (the verifier and the
  unified identity layer), 0110 (`ClassVars` shadow write), 0041
  (`StateAcc` scratch map)
- Code: `control_flow/analysis.rs`, `control_flow/plan.rs`,
  `control_flow/conditionals.rs`, `value_type_codegen.rs`,
  `control_flow/exception_handling.rs`, `control_flow/loop_mode.rs`,
  `gen_server/methods.rs`, `threaded_ir/ir.rs`, `threaded_ir/verify.rs`
- External: Cytron et al., "Efficiently Computing Static Single Assignment
  Form and the Control Dependence Graph" (1991); Erlang Reference Manual,
  "Expressions — Variables"
