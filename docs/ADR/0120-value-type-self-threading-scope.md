# ADR 0120: Value-Type `Self` Threading — Patch the Remaining Gaps, or Generalize `ThreadedIr`'s Storage Families?

## Status
Implemented (2026-09-09). The generalization this ADR deferred is decided
in [ADR 0122](0122-threaded-ir-storage-family-generalization-scope.md).

## Context

`self.field := value` in a value type (`CodeGenContext::ValueType`, reachable
only through the `TestCase` exemption, BT-1533) is threaded through
`Self`/`SelfN` (`VersionPrefix::SelfVt`). BT-3466 introduced it as a single
unthreaded emission: right for a write outside any construct, wrong inside
one. BT-3484 fixed the two shapes its repro named — a write as the top-level
statement of a `to:do:` loop body, and as a non-last statement of an
`ifTrue:` — by copying the `ClassVars` precedents (BT-3168 loops, BT-3159
conditionals): a hand-built extra `letrec` parameter and result-tuple slot.

That made `SelfVt` the third storage family (after `State` and `ClassVars`)
with its own copy of "thread a mutation through a construct." Reviewing
BT-3484 found three more broken shapes, all pre-existing:

| Shape | Failure | Same shape with a class var |
|-------|---------|-----------------------------|
| Conditional nested in a loop | `erlc: unbound variable 'State'` | clean "Cannot assign to field" rejection |
| `do:`/`collect:`/… (Foldl) body | `erlc: unbound variable 'State'` | same gap (BT-3169) |
| `on:do:`/`ensure:` try body | compiles; mutation **silently dropped** | n/a (Actor `State` threads correctly) |

Severity is bounded: every shape is `TestCase`-only, so the worst case is a
test asserting against stale data, not a production bug.

What is already shared: `VersionPrefix` models all three families as one
enum and `verify()` treats them uniformly. What is not: each construct has
its own "does this body mutate X" predicate and its own hand-built slot,
and Actor `State` is the ambient scratch map rather than an extra slot.

## Decision

**Fix the three gaps now, in the same narrow style as BT-3168/3159/3484.
Defer the generalization. Revisit on the next new gap — a fourth call site
or a fourth family.**

Three copies of one mechanism is the rule-of-three threshold; we are at it,
not past it. Generalizing under pressure from three fresh bugs, before the
`State` "free rider" vs. `ClassVars`/`SelfVt` "extra slot" question is
designed, is how a rushed abstraction gets built.

## Prior Art

Internal. ADR 0111 built the shared IR and verifier first and let real gaps
drive which constructs route through it, issue by issue. Same discipline
here.

## User Impact

None user-visible. A contributor writing a `TestCase` fixture that mutates
its own field inside a loop, conditional, list-op, or `ensure:` gets their
blocker fixed fastest; each fix will look, in the diff, like another copy
of the mechanism. That is the accepted cost.

## Steelman Analysis

**Generalize now:** BT-3484's review found three sibling gaps only because
someone looked; a generic mechanism makes the "does this need a `SelfVt`
arm" question impossible to forget. Paying for three narrow fixes and then
the generalization is paying twice.

**Patch narrowly (chosen):** the gaps are `TestCase`-only and independently
fixable in BT-3484's validated shape. A generalization has to get Actor's
free-rider case right, prove itself against the whole corpus, and risks
Actor/`ClassVars` output for thousands of passing programs — for a bug
class that cannot reach production.

Whether "three" or "the next one" is the trigger is a judgment call.

## Alternatives Considered

### Full generalization now
Rejected as the next step: real design work against a large regression
surface, under pressure.

### Loops + conditionals only
Rejected: does not cover the Foldl or exception-handling gaps, so the three
narrow fixes are still needed.

### Do nothing
Rejected: the `ensure:` silent drop is a test-integrity failure.

## Consequences

### Positive
- Each fix lands, reviews, and reverts on its own.
- No risk to Actor/`ClassVars` codegen.
- Three fixed, merged shapes become the generalization's test material.

### Negative
- A fourth and fifth hand-copied mechanism land.
- Whoever generalizes inherits five call sites, not two.

### Neutral
- The cost comparison inverts if a fourth gap turns up first. That is the
  bet.

## Implementation

1. `on:do:`/`ensure:` silent drop (highest priority): seed the construct's
   scratch map from `Self` when a block writes a field, thread and rebind
   `Self` afterwards, as BT-3484 did for loops.
2. Foldl body: the accumulator needs a `Self` slot; likely shares a fix with
   the `ClassVars`-in-Foldl gap.
3. Conditional in a loop: extend the existing "Cannot assign to field …
   inside this block" rejection to cover `ValueType`; full support is not
   required.

Validation bar per fix: `cargo test -p beamtalk-codegen`,
`just verify-threaded-ir`, `just test-bunit`/`test-stdlib`,
`just ci-changed`, and a hand-checked runtime result.

## Outcome (2026-09-11)

Epic [BT-3490](https://linear.app/beamtalk/issue/BT-3490) closed all three
gaps plus one found on the way:

| Gap | Issue / PR | Mechanism |
|-----|------------|-----------|
| `on:do:`/`ensure:` | [BT-3486](https://linear.app/beamtalk/issue/BT-3486) / #3836 | new trailing tuple slot — a third `SelfVt` call site |
| Conditional in a loop | [BT-3488](https://linear.app/beamtalk/issue/BT-3488) / #3834 | extended the shared rejection function; no new mechanism |
| Foldl body | [BT-3487](https://linear.app/beamtalk/issue/BT-3487) / #3848 | already fixed by BT-3488's shared rejection; zero new code |
| `match:` arm (not one of the three) | [BT-3489](https://linear.app/beamtalk/issue/BT-3489) / #3837 | Actor: reuses the conditional merge; value type: rejection. A fourth call site with its own detector |

The trigger fired: five `SelfVt` call sites, and BT-3489's crash also hits
`Actor subclass:` — the "TestCase-only" premise held for the three gaps
this ADR scoped and not for the fourth. Filed
[BT-3499](https://linear.app/beamtalk/issue/BT-3499) to scope the
generalization rather than patch a sixth gap narrowly; the answer is
ADR 0122.

Also filed, not counted as call sites: BT-3491 (rejection diagnostic
wording), BT-3492 (BT-3486's tuple unextracted in some positions), BT-3493
(a statement-classifier bug in ADR 0111 Addendum 5).

## References
- Issues: BT-3466, BT-3483, BT-3484, BT-3486/3487/3488/3489, BT-3490,
  BT-3499
- ADRs: 0042 (value-type immutability — why this is `TestCase`-only), 0110
  (`ClassVars` shadow write), 0111 (`ThreadedIr` verifier), 0122 (the
  generalization)
- Code: `value_type_codegen.rs` (`VtLoopExtraSlot`, `VtBranchPieces`),
  `control_flow/plan.rs`, `control_flow/analysis.rs`, `threaded_ir/ir.rs`
