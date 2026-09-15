# ADR 0122: Scoping the `ThreadedIr` Storage-Family Generalization

## Status
Proposed (2026-09-15)

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
| Conditional (`value_type_codegen.rs`) | `is_conditional_with_vt_local_threading` / `..._self_field_threading` | `VtCondSlots`, `finish_vt_conditional_branch`: two ~15-line arms after the locals | ClassVars, SelfVt | trailing |
| `on:do:`/`ensure:` (`exception_handling.rs`) | `exception_blocks_thread_value_self` | `exception_self_slot` | **SelfVt only** | trailing |
| `match:` (`gen_server/methods.rs`) | `match_needs_mutation_threading` — recursive, unlike the others | reuses the conditional's merge | State, ClassVars (no SelfVt) | n/a |
| Foldl list-ops (`plan.rs`) | `threads_class_vars`, Foldl shape | fold accumulator | ClassVars | **leading** |

Plus three `ClassVars`-only side channels on `LoopMode`
(`loop_threads_class_vars`, `last_loop_class_var`,
`last_foldl_class_var_peak`) with no `SelfVt` counterpart.

The *identity* layer is already unified: `VersionPrefix::{State, ClassVars,
SelfVt}` is one enum, and `ThreadedIr::verify()` treats all three the same
(its only family-specific check is ADR 0110's `ClassVars` shadow write).
Only the consumer logic above is duplicated.

### What the numbers say

- The eight named detectors total ~140 lines. The emitters total ~280-380,
  and the conditional emitter is mostly **outer-local** threading — the
  `ClassVars`/`SelfVt` parts are two ~15-line arms. A family
  generalization cannot delete the locals machinery. Realistic dead code:
  100-150 lines, plausibly net-zero once the shared type exists.
- `State` already fits. `StateAcc` is element 2 of the same result tuple
  the other families are appended to (`{'nil', StateAcc[, ClassVars |
  Self1]}`), and the Actor conditional merge uses the same both-or-neither
  arm discipline. In a slot-list model it is the trivial case: predicate
  always true. What differs is that its emitters are the production Actor
  path and `StateAcc` doubles as the locals scratch map.

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
block, the `on:do:` body and handler, last and non-last position. A direct
`self.runs :=` in the same place is a compile-time rejection — so `SelfVt`
is *supported* here (BT-3486) while `ClassVars` is *rejected* or *dropped*.
This is BT-3486 for `ClassVars`, it is in production-reachable code (class
vars are ordinary code, ADR 0110), and BT-3490's matrix missed it because
its `on:do:` fragments assert a local, not the class var. Filed as
[BT-3506](https://linear.app/beamtalk/issue/BT-3506).

It is also the whole argument: BT-3486 added a `SelfVt`-only detector to
that site, and nothing made anyone ask about `ClassVars`.

### Constraints

- `just verify-threaded-ir` is `test-stdlib` + `test-bunit` under
  `debug_assertions`. It catches invariant violations (it caught the second
  shape above). It does **not** catch a changed tuple shape or slot order,
  and there is no byte-identical `.core` diff over the corpus today.
- The scope is production-reachable via `ClassVars`; only the `SelfVt` half
  is `TestCase`-only.
- Some per-family behaviour stays regardless: `ClassVars` binds with
  `shadow_write: true` (ADR 0110), `SelfVt` with `maps:put` on the instance
  map; Foldl's slot is leading, Letrec's trailing.

## Decision

**Unify the detector and the slot-list type. Make each site's emitter read
the list. Leave the emitters, Actor `State`, and `match:` otherwise
alone.**

1. One detector, `body_threaded_families(body, eligible) -> ThreadedFamilies`,
   in `control_flow/analysis.rs` next to the shared rejection function,
   replacing the two loop walks and the `on:do:` wrapper. `eligible` is the
   context's family set (Actor: `[State]`; class method: `[State,
   ClassVars]`; value type: `[State, SelfVt]`).
2. `ThreadedFamilies` is an ordered `Vec<VersionPrefix>` in canonical slot
   order. `State` is a member so the type is honest about it; its emitters
   are not rewired.
3. Each site replaces its bool pair / `VtLoopExtraSlot` / `VtCondSlots` /
   `exception_self_slot` gate with "for each family in the list, append its
   slot in this site's position." Tuple layouts, locals machinery, and the
   narrowing comments stay where they are.
4. A site that receives a family it does not carry fails `verify()` — it
   already does; that is what the second repro shape shows. No new
   `debug_assert!`.
5. BT-3506 lands as the first consumer of (1)-(3).

```rust
// control_flow/analysis.rs
/// Storage families a construct body mutates, in canonical slot order.
pub(in crate::core_erlang) struct ThreadedFamilies(Vec<VersionPrefix>);

fn body_threaded_families(&self, body: &[Expression], eligible: &[VersionPrefix])
    -> ThreadedFamilies
```

Not in scope, with the trigger that reopens it:

- **Migrating `State` emission** onto the list. Reopen by migrating
  `vt_construct_extra_slot` first, when a construct needs `StateAcc` to be
  conditionally absent or a second `State` gap turns up in Actor
  loop/conditional emission.
- **`match:`'s detector.** Reopen when it needs `SelfVt`.
- **Merging the three emitters into one function.** See Alternatives.

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

None from the generalization. The persona is a contributor adding a
construct or a family, who today has to remember to ask "does this need a
`ClassVars`/`SelfVt` arm?" — a question BT-3490 missed once (`match:`) and
this ADR found missed again (site 3). Afterwards, forgetting fails the
first corpus compile.

BT-3506 has user impact until it lands: a class method that mutates a class
var via a self-send inside `ensure:` silently loses the write. Smalltalk
developers will find that surprising because `ensure:` is the "this must
happen" idiom; Erlang developers will spot the `try` scoping immediately.

## Steelman Analysis

| Alternative | Strongest case for it | Why not |
|-------------|----------------------|---------|
| Unify all three families and `match:` | Every site left out is a place the question can still be forgotten. | `State`'s emitters are the production Actor path; `match:` is recursive and 2-of-3. Keeping `State` in the *type* preserves the option at no cost. |
| Merge the three emitters | The family arms really are the same ~15 lines three times. | They sit inside ~300 lines of locals threading and construct-specific layout (leading vs trailing, `try` seeding). Moving ~300 to save ~100-150, against production `ClassVars` code, before an output-diff harness exists. |
| Detectors only, then stop | Almost all the "can't forget" value, a fraction of the risk, a differential test as a complete gate. | A detector nobody reads changes nothing. Site 3 is where families get forgotten — the emitters have to consume the list. |

Where reasonable people disagree: whether BT-3506 should be the first
consumer of the new mechanism (proves it on the motivating shape) or a fast
BT-3486-style patch landed first and refactored later.

## Alternatives Considered

### Full three-family generalization
Rejected for now. `State` fits the abstraction but not the risk budget;
`match:` needs more special-casing than it saves. `State` stays in the type
so this is a later migration, not a redesign.

### Merge the emitters into one function
Rejected on measurement: ~15 family-specific lines per site inside ~300
lines of construct-specific machinery, with two different slot positions.

### Detectors only
Adopted as issue 1; not as the end state. See Steelman.

### `State` + `ClassVars` + `SelfVt` for loops only
Deferred; recorded as the trigger for folding `State` in.
`vt_construct_extra_slot` is the one emitter where `State`'s absence is
purely historical, so it is the right first migration when one is wanted.

### Defer again
Rejected. The "next gap" trigger has fired three times.

## Consequences

### Positive
- One detector reports every eligible family to every site; a site that
  ignores one fails `verify()` at development time.
- BT-3506 gets fixed as the first consumer, on the shape that motivated it.
- `State` is representable at zero migration cost.
- A corpus `.core` diff exists afterwards for every future codegen refactor.

### Negative
- Net line count is roughly unchanged.
- The scope is production-reachable via `ClassVars`; issue 0 is real
  infrastructure work before any emitter change.
- `match:` and `State` emission stay hand-written.
- Per-family configuration survives in the emitters (`shadow_write`,
  `maps:put`, slot position). "Family-agnostic" describes the detector and
  the type, not every line of emission.

### Neutral
- Six small issues, each sized like BT-3486/3487/3488/3489.

## Implementation

0. **Corpus `.core` diff harness.** A `just` recipe that compiles the
   stdlib + bootstrap-test corpus with `.core` retained
   (`beam_compiler.rs` already writes them) and diffs the tree against
   `main`. Required before any emitter change.
1. **Detector and `ThreadedFamilies`** in `analysis.rs`, gated by a
   differential test: old and new detectors agree on every construct in
   the corpus.
2. **BT-3506** as the first consumer: `on:do:`/`ensure:` grows a
   `ClassVars` slot exactly as BT-3486 grew the `SelfVt` one, driven by the
   list. Decide whether the direct-write shape becomes supported or stays
   rejected; the two families must agree. Fix the
   `exception_body_outer_state` doc comment.
3. **Loop emitter reads the list** (`VtLoopExtraSlot`,
   `vt_construct_extra_slot`). Layout unchanged; diff clean.
4. **Conditional emitter reads the list** (`VtCondSlots`,
   `VtCondBaseline`). Locals machinery untouched. Largest; last.
5. **Side-channel audit**: route `loop_threads_class_vars`,
   `last_loop_class_var`, `last_foldl_class_var_peak`, and the Foldl
   leading slot through the list, or document them as the per-family
   configuration that stays.

Every issue: `cargo test -p beamtalk-codegen`, `just verify-threaded-ir`,
`just test-bunit`/`test-stdlib`, `just ci-changed`, and from issue 2 on a
clean issue-0 diff, with intentional shape changes (issue 2's new slot)
reviewed as a diff.

## Migration Path
Not applicable. BT-3506 changes observable behaviour (a dropped write
becomes a kept one); that is a bug fix.

## References
- Related issues: BT-3499 (this ADR), BT-3506 (site-3 gap), BT-3484/3486/
  3487/3488/3489 (the five sites), BT-3490 (the epic), BT-3491 (the
  rejection diagnostic's wording, which the direct-write shape also hits)
- Related ADRs: [0120](0120-value-type-self-threading-scope.md) (`SelfVt`
  threading; Addendum 1 files BT-3499), 0111 (the verifier), 0110
  (`ClassVars` shadow write), 0041 (`StateAcc` scratch map)
- Code: `control_flow/analysis.rs`, `control_flow/plan.rs`,
  `value_type_codegen.rs`, `control_flow/exception_handling.rs`,
  `control_flow/loop_mode.rs`, `gen_server/methods.rs`,
  `threaded_ir/ir.rs`, `threaded_ir/verify.rs`
- External: Cytron et al., "Efficiently Computing Static Single Assignment
  Form and the Control Dependence Graph" (1991); Erlang Reference Manual,
  "Expressions — Variables"
