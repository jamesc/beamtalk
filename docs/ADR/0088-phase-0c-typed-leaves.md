# ADR 0088 Phase 0c Audit: Typed-Document-Leaves Comparison

**Date:** 2026-05-28
**Issue:** BT-2316 (epic [BT-2313](https://linear.app/beamtalk/issue/BT-2313))
**Compares:** the cerl-direct rewrite measured in [Phase 0a](0088-phase-0a-audit.md) against a typed-Document-leaves rewrite of the same three functions.
**Status:** Recommendation — **Withdraw ADR 0088** in favour of the typed-Document-leaves refactor.

## Executive Summary

Phase 0a measured cerl-direct's projected codegen shrinkage at ~6-9% by
character count and qualified its recommendation pending a comparison
against the typed-Document-leaves alternative. This memo runs that
comparison: three rewrites of the same three functions, same fixtures,
same byte-for-byte equivalence tests.

| Tier | Function | Char shrinkage (cerl) | Char shrinkage (typed-leaves) |
|---|---|---|---|
| Leaf | `beamtalk_class_attribute` | **+1.5%** (slightly worse) | **−4.0%** |
| Medium | Logger metadata-map + log-call | **−13.2%** | **−18.4%** |
| High | `generate_pack_prefix` | **−11.4%** | **−6.2%** |
| **Aggregate (sum across 3 fns)** | | **−9.5%** | **−8.7%** |

**Typed-Document-leaves captures essentially the same shrinkage as
cerl-direct** — within ~0.8% on the aggregate, and ahead on two
of the three function tiers. The leaf tier in particular swings from
"cerl is *slightly worse* than the original (+1.5%)" to "typed-leaves
delivers a clean −4.0%", because typed-leaves doesn't pay cerl's
AST-construction tax on small functions.

This satisfies the decision criterion in [BT-2316](https://linear.app/beamtalk/issue/BT-2316):

> If typed-Document-leaves comes in **within ~3% of cerl's shrinkage**:
> cerl's marginal benefit is too small to justify a 30× migration cost.
> ADR 0088 should be withdrawn in favour of the typed-leaves refactor.

**Recommendation:** **Withdraw ADR 0088 (Phases 1–4 of the cerl
migration) and pursue typed-Document-leaves as a smaller refactor** that
closes the same [BT-875](https://linear.app/beamtalk/issue/BT-875)
recurrence vector at a fraction of the cost. The Phase 0b wire-cost
data (~10% compile-speed bonus from skipping `core_scan`+`core_parse`)
is the *only* meaningful thing typed-leaves leaves on the table; if
that win matters later, it can be revisited as an independent ADR
without bundling it with the 58K-LOC AST restructure ADR 0088 proposed.

## Methodology

Same as the BT-2314 audit, with one addition: a third "typed-leaves"
column. The three audit functions are unchanged (`beamtalk_class_attribute`,
the Logger metadata-map + log-call fragments, `generate_pack_prefix`).
The originals and the cerl rewrites live in
`crates/beamtalk-core/src/codegen/core_erlang/cerl_audit.rs`; the
typed-leaves rewrites live in the sibling
`crates/beamtalk-core/src/codegen/core_erlang/typed_leaves_audit.rs`.

* LOC counts the *body* of each function (signature + braces excluded).
* Char count uses non-whitespace bytes per line, summed — the more
  honest metric (LOC is sensitive to rustfmt's multi-arg-call
  collapse choices, char count isn't).
* Helper-call count is the number of `docvec!`, `Document::*`,
  `atom(...)`, `var(...)`, `cons(...)`, `map(...)`, `tuple(...)`,
  `call(...)`, etc. calls in the body — a rough proxy for "ceremony
  per emitted Core Erlang fragment".
* Text-equivalence is asserted by `to_pretty_string()` byte-for-byte
  match against the original — same fixtures as the cerl audit. All
  9 tests pass (`cargo test -p beamtalk-core typed_leaves_audit`).

### The typed-leaves API in this audit

Two helpers, both returning `Document<'static>`:

```rust
pub fn atom(name: impl Into<String>) -> Document<'static> {
    docvec!["'", Document::String(name.into()), "'"]
}
pub fn var(name: impl Into<String>) -> Document<'static> {
    Document::String(name.into())
}
```

That's it. Ten lines including doc comments. In a production
typed-leaves refactor these would replace direct `Document::String(...)`
call sites and `Document::String` itself would be removed (or made
private), closing the BT-875 vector for good. For Phase 0c the helpers
sit alongside `Document::String` so the audit can be measured without
disturbing production code.

Note the asymmetry vs. cerl's ~250-LOC support module: cerl needs an
`Expr` sum type with 12 variants and a `to_doc()` renderer.
Typed-leaves needs nothing beyond what `Document` already provides.

## Per-function results

### Rewrite #1 (leaf): `beamtalk_class_attribute`

| Variant | Body LOC | Char count | Helper calls |
|---|---|---|---|
| Original | 17 | 275 | 8 |
| cerl-direct | 15 (−12%) | 279 (**+1.5%**) | 9 |
| **typed-leaves** | **12 (−29%)** | **264 (−4.0%)** | **8** |

**Cerl is actually slightly worse than the original by char count.** The
atom-quote ceremony savings are real but get eaten by the verbosity of
`tuple(vec![atom(name), atom(superclass)]).to_doc()`. Typed-leaves
avoids the AST round-trip entirely — `atom(name)` is just three
characters — so it banks the savings without paying for them.

```rust
// Original
docvec![
    "{'", Document::String(name), "', '", Document::String(superclass), "'}"
]

// cerl
tuple(vec![atom(name), atom(superclass)]).to_doc()

// typed-leaves
docvec!["{", atom(name), ", ", atom(superclass), "}"]
```

### Rewrite #2 (medium): Logger metadata-map + log-call

| Variant | Body LOC | Char count | Helper calls |
|---|---|---|---|
| Original | 20 | 370 | 6 |
| cerl-direct | 15 (−25%) | 321 (−13%) | 14 |
| **typed-leaves** | **20 (0%)** | **302 (−18%)** | **6** |

Char count is the headline result here — typed-leaves wins (−18% vs
cerl's −13%) because it can mix static template strings with dynamic
typed leaves. Cerl forces the whole structure into AST construction,
which inflates the helper-call count from 6 to 14.

LOC numerically favours cerl (−25% vs 0%), but inspection shows rustfmt
collapses the multi-arg `map(vec![...])` constructor onto fewer lines
while the typed-leaves `docvec![..., atom(x), ", ", ...]` shape keeps
one element per line. The char count is the apples-to-apples measure.

```rust
// Original
"'beamtalk_class' => '", Document::String(ctx_class), "', "

// cerl (inside a map(vec![...]) constructor)
(atom("beamtalk_class"), atom(ctx_class))

// typed-leaves
"'beamtalk_class' => ", atom(ctx_class), ", "
```

### Rewrite #3 (high-complexity): `generate_pack_prefix`

| Variant | Body LOC | Char count | Helper calls |
|---|---|---|---|
| Original | 33 | 876 | 9 |
| **cerl-direct** | **23 (−30%)** | **776 (−11%)** | 10 |
| typed-leaves | 34 (+3%) | 822 (−6%) | 9 |

This is the one tier where cerl decisively wins on char count (−11% vs
typed-leaves' −6%). The function has many `Document::String(var_name)`
sites and cerl's `var(x)` is a touch shorter than the typed-leaves
`var(x.clone())` because cerl's `var(impl Into<String>)` swallows the
clone. A more carefully designed typed-leaves API that takes `&str`
might claw some of that back, but for this audit the rules were the
same as cerl's API.

```rust
// Original (per maps:put let)
docvec![
    "let ", Document::String(packed_var), " = call 'maps':'put'('",
    Document::String(key), "', ", Document::String(core_var), ", ",
    Document::String(current), ") in ",
]

// cerl
pack_exprs.push(let_open(
    packed_var.clone(),
    call("maps", "put", vec![atom(key), var(core_var), var(current)]),
));

// typed-leaves
docvec![
    "let ", var(packed_var), " = call 'maps':'put'(",
    atom(key), ", ", var(core_var), ", ", var(current), ") in ",
]
```

## Aggregate

Summed across the three functions:

| Variant | Total chars (3 fns) | Shrinkage vs original |
|---|---|---|
| Original | 1,521 | — |
| cerl-direct | 1,376 | **−9.5%** |
| typed-leaves | 1,388 | **−8.7%** |

**Within 0.8% of each other on the aggregate.** This is well inside the
BT-2316 decision criterion's "within ~3%" threshold.

## What typed-leaves doesn't give that cerl does

Two things, both small:

1. **The ~10% compile-speed win** Phase 0b measured (cerl wire
   eliminates `core_scan`+`core_parse`). Typed-leaves changes nothing
   on the wire; the BEAM side still scans and parses text. For a build
   of N modules where each module averages a few-ms `compile:forms/2`
   cost, this is the order of hundreds of µs per module — noticeable
   for huge projects, not transformative.

2. **Structural type safety inside the codegen tree.** A `Document::Vec`
   says nothing about what its contents mean; a `cerl::Expr::Call`
   does. This matters when refactoring across the tree, which is rare
   in practice — codegen rewrites tend to be additive (new selectors,
   new lowerings) rather than structural.

Both are real, neither justifies a 58K-LOC migration when ~9% of the
shrinkage benefit can be had for the cost of a `Document::String`
audit.

## What typed-leaves gives that cerl doesn't

1. **~30× cheaper migration.** The typed-leaves refactor replaces
   `Document::String(...)` call sites with `atom(...)` / `var(...)` /
   `string_lit(...)` / `int_lit(...)` calls — same number of call
   sites as cerl would touch, but each is a one-token edit, not a
   structural rewrite. A rough upper bound: a few hundred call sites
   across the ~55 codegen files, mechanically transformable with
   targeted greps. Versus cerl's full reshape of every function's
   return type and every helper signature.

2. **No wire format change.** No Erlang-side modifications. No new
   ETF encoder. No `binary_to_term` safety review. No Port handler
   variant. The Phase 0b napkin found ETF was cheap, but cheap is not
   free — there's still real work to ship Phase 0b's prototype as a
   production wire (cross-platform OTP behaviour, error handling on
   the BEAM side, atom-table safety in production payloads). All of
   that goes away.

3. **Tiny support code.** Ten lines of helpers vs cerl's ~250-LOC
   AST + renderer. Easier to review, easier to extend, fewer moving
   parts when something breaks.

4. **Same BT-875 closure.** Once `Document::String` is removed from
   the public Document API and the typed helpers are the only way to
   produce a Core Erlang fragment, the recurrence vector is sealed.
   This is the entire original motivation for ADR 0088; typed-leaves
   delivers it at 1/30th the cost.

## Recommendation

**Withdraw ADR 0088 (Phases 1–4 of the cerl-direct migration).**

The empirical case for cerl-direct rested on two pillars:

1. **Shrinkage justifies the migration cost.** Phase 0a found this
   pillar wobbly (~6-9%); Phase 0c finds it broken — typed-leaves
   captures essentially the same shrinkage at a fraction of the cost.

2. **The wire cost doesn't disqualify ETF.** Phase 0b found ETF cheap.
   But this is a *necessary, not sufficient* finding; it removes a
   blocker on cerl-direct without arguing for it.

With both pillars examined, the case to spend 58K LOC on a wire
restructure to get ~1% more shrinkage and ~10% compile-speed
improvement does not hold up. **The typed-Document-leaves refactor is
the better path** — same simplification, same BT-875 closure, no wire
change, no Erlang-side risk, ~30× cheaper to ship.

Open a follow-up issue (or a fresh ADR if the scope warrants) to plan
the typed-leaves rollout. Keep [BT-875](https://linear.app/beamtalk/issue/BT-875)
as the umbrella — typed-leaves is its long-term fix.

If the ~10% compile-speed bonus from Phase 0b becomes important later
(e.g. for very large projects where build time dominates), revisit
cerl-as-wire as an *independent* ADR — a much smaller scope than
ADR 0088 proposed, because by then the codegen will already be
typed-leaves clean and only the wire question remains.

## Files

* New: `crates/beamtalk-core/src/codegen/core_erlang/typed_leaves_audit.rs`
  (throwaway, `#[cfg(test)]`-only; 9 text-equivalence tests)
* New: `docs/ADR/0088-phase-0c-typed-leaves.md` (this memo)
* Reuses: `cerl_audit::{beamtalk_class_attribute_original,
  logger_log_call_original, generate_pack_prefix_original,
  PackPrefixInputs}` for the comparison fixtures.

## References

* Phase 0a memo: [`docs/ADR/0088-phase-0a-audit.md`](0088-phase-0a-audit.md)
* Phase 0b memo: [`docs/ADR/0088-phase-0b-napkin.md`](0088-phase-0b-napkin.md)
* The ADR being evaluated: [`docs/ADR/0088-direct-cerl-emission.md`](0088-direct-cerl-emission.md)
* The recurrence vector at stake: [BT-875](https://linear.app/beamtalk/issue/BT-875)
* Decision criterion source: [BT-2316](https://linear.app/beamtalk/issue/BT-2316)
