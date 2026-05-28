# ADR 0088 Phase 0a Audit: Codegen Shrinkage Measurement

**Date:** 2026-05-28
**Issue:** BT-2314 (part of epic BT-2313)
**Evaluator:** 3-function rewrite + projection across 58 codegen files (49,351 non-test LOC)
**Status:** Recommendation — **Qualified**: in-between zone (≈10–12% projected shrinkage), neither clear-proceed nor clear-withdraw.

## Executive Summary

The audit rewrote 3 representative `Document`-based codegen functions on a
throwaway `cerl::*` Rust AST mirror and measured the size delta. Aggregate
shrinkage lands at **~11% by non-whitespace character count** and **12–30%
by non-blank LOC** (LOC has higher variance because rustfmt collapses
multi-line constructor calls):

| Tier            | Function                              | Body LOC               | Char count        |
| --------------- | ------------------------------------- | ---------------------- | ----------------- |
| Leaf utility    | `beamtalk_class_attribute`            | 17 → 15 (**−12%**)     | 275 → 279 (+1.5%) |
| Medium          | Logger metadata-map + log-call        | 25 → 20 (**−20%**)     | 460 → 411 (−11%)  |
| High-complexity | `ThreadingPlan::generate_pack_prefix` | 33 → 23 (**−30%**)     | 876 → 776 (−11%)  |

By **character count** (the more honest of the two metrics — see Methodology),
all three rewrites cluster near **11%**. The leaf utility is a wash (the
ceremony shifts from atom-quote punctuation to `tuple(vec![atom(...), ...])`
Rust verbosity); the medium and high-complexity rewrites both deliver ~11%
shrinkage. The high-complexity LOC win of 30% reflects rustfmt's ability to
collapse short `cerl::*` constructor chains onto single lines that the
original's multi-element `docvec!` calls could not — real but partly
cosmetic.

This sits **below the ADR's 15% "proceed unconditionally" gate** but **above
the 5% "withdraw" gate**.

**Recommendation:** This is the **qualified middle**. The audit does *not*
support proceeding with Phase 1 on simplification grounds alone; the per-line
savings exist but are modest and converge to ~11% regardless of function
complexity — not the "bigger functions benefit more" curve the ADR's most
optimistic scenario predicts. Before committing to Phase 1, gather **Phase 0b** data (ETF
encode/decode cost) and explicitly compare against the **typed-Document-leaves**
alternative on the same three functions. See §"What would shift the
recommendation" below.

## Methodology

### What was rewritten

Three functions, one per size tier mandated by the ADR's "real audit"
requirement (Appendix A used a biased 2-file sample of `util.rs`-like code; the
audit had to include the 4.2K-LOC `control_flow/mod.rs` hot spot):

1. **Leaf utility (`util.rs`)**: `beamtalk_class_attribute` — builds the
   `'beamtalk_class' = [{Name, Super}, ...]` module attribute fragment.
   Pure leaf, no generator state, almost all atom-quote ceremony.

2. **Medium (`intrinsics.rs`)**: the metadata-map + `logger:log/3` call
   fragments inside `try_generate_logger_intrinsic`. Pulled the two map/call
   fragments out of the 115-LOC parent function — the rest of that function
   (selector parsing, arg hoisting, dispatch fall-through) is orthogonal to
   the Document/cerl choice and would look identical in both designs.

3. **High-complexity (`control_flow/mod.rs`)**: `ThreadingPlan::generate_pack_prefix`,
   the function that emits the `maps:put`-chain prefix that loads a `StateAcc`
   map for stateful loops. Body has 3 control-flow branches (early-return,
   ValueType vs. actor context, threaded-locals loop), each emitting `let`-chains.
   Pulled out of `control_flow/mod.rs` (4,237 LOC, the hot spot Appendix A
   explicitly flagged as missing).

### Rewrite location

All rewrites + a minimal `cerl::*` mirror + the original functions (verbatim
copies, isolated from `CoreErlangGenerator` so they need no compiler instance)
live in `crates/beamtalk-core/src/codegen/core_erlang/cerl_audit.rs`. The file
is `#[cfg(test)]`-gated so it ships no production code, and is to be deleted
when the gate decision lands. The minimal `cerl::*` mirror has 12 node kinds
(Atom, Var, Int, Map, Tuple, List, Cons, Call, Let, LetOpen, Seq, Raw) — a
small fraction of the 20–30 the real ADR-0088 Phase 1 would need.

### Semantic equivalence check

Each rewrite is paired with the verbatim original; tests render both to text
via `to_pretty_string()` and assert byte-equality on a representative input
matrix:

* `beamtalk_class_attribute`: empty, single class, three classes
* Logger fragment: four log levels (debug/info/warning/error)
* `generate_pack_prefix`: empty locals, actor context with two locals,
  ValueType context with two locals, direct-params early-return

All 9 tests pass:

```text
test codegen::core_erlang::cerl_audit::tests::beamtalk_class_attribute_empty_matches ... ok
test codegen::core_erlang::cerl_audit::tests::beamtalk_class_attribute_single_class_matches ... ok
test codegen::core_erlang::cerl_audit::tests::beamtalk_class_attribute_multi_class_matches ... ok
test codegen::core_erlang::cerl_audit::tests::logger_log_call_matches ... ok
test codegen::core_erlang::cerl_audit::tests::logger_log_call_various_levels_match ... ok
test codegen::core_erlang::cerl_audit::tests::pack_prefix_empty_locals_matches ... ok
test codegen::core_erlang::cerl_audit::tests::pack_prefix_actor_context_matches ... ok
test codegen::core_erlang::cerl_audit::tests::pack_prefix_value_type_context_matches ... ok
test codegen::core_erlang::cerl_audit::tests::pack_prefix_direct_params_returns_nil ... ok
```

### Measurement rules

* **LOC** counts non-blank, non-comment body lines (signature + closing brace
  excluded; doc-comments excluded).
* **Char count** counts non-whitespace characters in the body (comments
  stripped). This is a more honest proxy than line count when one style has a
  habit of wrapping single expressions across many lines (Document does, cerl
  does too — but cerl does it less).
* **Branching (cyclomatic)** is `1 + (number of decision points)`. Counted by
  hand — the rewrites preserve the original control flow exactly, so this is
  unchanged across rewrites (a key audit finding: cerl-direct does not change
  *algorithmic* complexity).
* **Helper-call count** is the number of constructor invocations
  (`Document::String`, `docvec!`, `Document::Nil`, … on the original side;
  `atom`, `var`, `call`, `let_open`, `tuple`, `map`, `cons`, `seq`, on the
  rewrite side).

## Per-function comparisons

### Rewrite 1: Leaf utility — `beamtalk_class_attribute`

**Original** (verbatim, in `cerl_audit.rs` as `beamtalk_class_attribute_original`):

```rust
pub fn beamtalk_class_attribute_original(classes: &[(String, String)]) -> Document<'static> {
    if classes.is_empty() {
        return Document::Nil;
    }
    let entries = classes.iter().map(|(name, superclass)| {
        docvec![
            "{'",
            Document::String(name.clone()),
            "', '",
            Document::String(superclass.clone()),
            "'}"
        ]
    });
    docvec![
        ",\n     'beamtalk_class' = [",
        join(entries, &Document::Str(", ")),
        "]"
    ]
}
```

**Rewrite** (`beamtalk_class_attribute_cerl`):

```rust
pub fn beamtalk_class_attribute_cerl(classes: &[(String, String)]) -> Document<'static> {
    use cerl::*;
    if classes.is_empty() {
        return Document::Nil;
    }
    let entries: Vec<_> = classes
        .iter()
        .map(|(name, superclass)| tuple(vec![atom(name.clone()), atom(superclass.clone())]).to_doc())
        .collect();
    docvec![
        ",\n     'beamtalk_class' = [",
        join(entries, &Document::Str(", ")),
        "]",
    ]
}
```

| Metric                     | Original | Rewrite        | Δ      |
| -------------------------- | -------- | -------------- | ------ |
| Body LOC (non-blank)       | 17       | 15             | −12%   |
| Char count (no whitespace) | 275      | 279            | +1.5%  |
| Branching (cyclomatic)     | 2        | 2              | 0      |
| Helper calls               | 5        | 6 (+1 `tuple`) | +1     |

**Reading:** The LOC win is real (the `"{'", name, "', '", superclass, "'}"`
five-part atom dance collapses to `tuple(vec![atom(name), atom(superclass)])`).
But the character count is **slightly worse** — the Rust verbosity (`tuple`,
`vec!`, `.iter().map().collect::<Vec<_>>()` plus `.to_doc()` because the
surrounding attribute glue is still text) eats the savings. The leading
`",\n     "` and `"'beamtalk_class' = [..."` glue is *not* Core Erlang — it's
attribute-list plumbing one level up — so cerl-direct cannot remove it.
**In a function this small, ceremony shifts; it doesn't shrink.**

### Rewrite 2: Medium — Logger metadata-map + log-call fragments

**Original** (verbatim, no-metadata branch of `try_generate_logger_intrinsic`):

```rust
let metadata_map_doc = docvec![
    "~{",
    "'domain' => ['beamtalk' | ['user']], ",
    "'beamtalk_class' => '",
    Document::String(ctx_class.to_string()),
    "', ",
    "'beamtalk_selector' => '",
    Document::String(ctx_selector.to_string()),
    "'",
    "}~",
];
docvec![
    "call 'logger':'log'('",
    Document::String(level.to_string()),
    "', ",
    Document::String(msg_doc_text.to_string()),
    ", ",
    metadata_map_doc,
    ")"
]
```

**Rewrite** (`logger_log_call_cerl`):

```rust
pub fn logger_log_call_cerl(level: &str, msg_doc_text: &str, ctx_class: &str, ctx_selector: &str)
    -> Document<'static>
{
    use cerl::*;
    let metadata = map(vec![
        (atom("domain"), cons(atom("beamtalk"), Expr::List(vec![atom("user")]))),
        (atom("beamtalk_class"), atom(ctx_class)),
        (atom("beamtalk_selector"), atom(ctx_selector)),
    ]);
    let log_call = call(
        "logger", "log",
        vec![atom(level), Expr::Raw(msg_doc_text.to_string()), metadata],
    );
    log_call.to_doc()
}
```

| Metric                  | Original | Rewrite | Δ      |
| ----------------------- | -------- | ------- | ------ |
| Body LOC (non-blank)    | 25       | 20      | −20%   |
| Char count (no whitespace) | 460  | 411     | −10.7% |
| Branching (cyclomatic)  | 1        | 1       | 0      |
| Helper calls            | 6        | 14      | +8     |

**Reading:** This is closer to the ADR's optimistic prediction. The metadata
map (a 9-element punctuation tower in the original) collapses to a 4-element
`Vec<(Expr, Expr)>`. The `call 'logger':'log'('debug', ..., ...)` ceremony
collapses to `call("logger", "log", vec![atom("debug"), ..., ...])`. Char
count drops ~11% even after accounting for `cerl::*` import boilerplate. The
helper-call count *increases* because cerl-direct expresses structure with
many small constructors — but those constructors carry zero punctuation, so
the net per-character density is much lower.

### Rewrite 3: High-complexity — `ThreadingPlan::generate_pack_prefix`

**Original** (excerpt — the per-iteration `maps:put` let):

```rust
pack_docs.push(docvec![
    "let ",
    Document::String(packed_var.clone()),
    " = call 'maps':'put'('",
    Document::String(key),
    "', ",
    Document::String(core_var),
    ", ",
    Document::String(current),
    ") in ",
]);
```

**Rewrite** (`generate_pack_prefix_cerl` — same iteration):

```rust
pack_exprs.push(let_open(
    packed_var.clone(),
    call("maps", "put", vec![atom(key), var(core_var), var(current)]),
));
```

Full-function metrics:

| Metric                     | Original                                    | Rewrite | Δ      |
| -------------------------- | ------------------------------------------- | ------- | ------ |
| Body LOC (non-blank)       | 33                                          | 23      | −30%   |
| Char count (no whitespace) | 876                                         | 776     | −11.4% |
| Branching (cyclomatic)     | 4 (if/match/for + early-return)             | 4       | 0      |
| Helper calls               | 17 (2 docvec + 14 Doc::String + 1 Doc::Vec) | 9       | −8     |

**Reading — the most important data point of the audit:** Per `maps:put`
emission, cerl-direct saves **6 of 9 lines** (the punctuation lines) *inside
the loop body* — a 67% per-call shrinkage. But that loop is only 9 of the 33
original-function lines; the surrounding control flow (early-return,
ValueType branch, fresh-temp generation, vector building, threading state
through `current`) is **unchanged**.

The 30% LOC headline overstates the picture because rustfmt collapses the
`call("maps", "put", vec![atom(key), var(core_var), var(current)])` chain
onto a single line where the original `docvec![ "let ", Document::String(...),
" = call 'maps':'put'('", Document::String(key), ..., ") in ", ]` ran 9
multi-line lines. **The honest measurement is the −11% character count**,
which is consistent across the medium and high-complexity rewrites.

The audit's key finding: **per-line savings are real but ceiling-bound by the
proportion of a function that is pure-emission vs. control-flow.**
`generate_pack_prefix` is roughly 50/50, and it shrinks ~11% by chars.
Functions dominated by emission (logger fragment) shrink in the same
ballpark. Functions dominated by control flow (most of `control_flow/mod.rs`,
which is largely analysis + threading-plan construction, not emission) will
shrink less.

## Projection across the codegen

The codegen layer is 49,351 non-test LOC across 58 files. Projection uses the
**character-count basis** (~11% per emission-heavy fragment) rather than the
LOC basis, because raw LOC mixes real shrinkage with rustfmt-collapse noise.
Naive per-file shrinkage by `Document::*` / `docvec!` ref density:

| File                                                | LOC   | Doc refs | Refs/LOC | Est. tier      |
| --------------------------------------------------- | ----- | -------- | -------- | -------------- |
| `control_flow/mod.rs`                               | 4,237 | 314      | 7.4%     | high-complex   |
| `gen_server/methods.rs`                             | 3,861 | 355      | 9.2%     | medium-high    |
| `dispatch_codegen.rs`                               | 3,326 | 264      | 7.9%     | medium-high    |
| `expressions.rs`                                    | 3,097 | 348      | 11.2%    | medium         |
| `value_type_codegen.rs`                             | 3,063 | 290      | 9.5%     | medium-high    |
| `intrinsics.rs`                                     | 2,365 | 181      | 7.7%     | medium         |
| `control_flow/list_ops/transform_ops.rs`            | 2,205 | 510      | 23.1%    | emission-heavy |
| `gen_server/callbacks.rs`                           | 1,608 | 168      | 10.4%    | medium         |
| Other 50 files                                      | 25,589 | ~1,559   | ~6.1%    | mostly low-medium |
| **Total non-test**                                  | **49,351** | **3,989** | **8.1%** | — |

Linking these density buckets to the per-function shrinkage tiers measured
above:

* **Emission-heavy** (>15% Doc-refs/LOC, e.g. `transform_ops.rs` at 23%):
  expect ~12–15% shrinkage by chars. ~2,200 LOC. Estimated savings:
  **260–330 LOC** of equivalent character density.
* **Medium-high emission** (8–12% Doc-refs/LOC): ~13,500 LOC. Expect ~11%
  shrinkage (matches the logger and pack-prefix data points). Estimated
  savings: **~1,500 LOC.**
* **Low-medium emission** (<8% Doc-refs/LOC, e.g. `control_flow/mod.rs` body
  outside its emission hot spots): ~33,500 LOC. Expect 4–8% shrinkage —
  `generate_pack_prefix` was 11% in this bucket, but it's a relatively
  emission-heavy function within that file; the file's average is much
  lower. Estimated savings: **1,340–2,680 LOC.**

**Projected total codegen shrinkage: ~3,100–4,500 LOC, or 6–9% of 49K LOC.**

This is **below the 15% "proceed unconditionally" threshold** the ADR sets.
It is **above the 5% "withdraw" threshold**.

### Caveats and bias correction

* **Selection bias:** all three rewrites are *known-emission-heavy* functions
  within their tier (we picked the metadata-map fragment, not the
  selector-parsing portion of `try_generate_logger_intrinsic`). The projection
  uses Doc-ref density to correct for this, but the rewrites themselves
  understate average-case complexity.

* **Renderer overhead unaccounted for:** the audit's `cerl::Expr::to_doc()`
  is ~35 LOC and would not exist in production Phase 1 — it's audit-only
  scaffolding. Production would ETF-encode directly. This means the audit
  *understates* shrinkage by about a function-renderer's worth (~30–50 LOC
  globally, negligible).

* **Cerl mirror itself adds LOC:** Phase 1 of ADR 0088 would need 20–30 node
  kinds with full annotation, line-number, and pattern support, plus
  constructors and ETF encoders. Estimate ~800–1,500 LOC of new infrastructure
  not present today. Net savings shrink to **2,000–4,300 LOC, or 4–9%.**

* **`format!()`-violation surface:** the audit does *not* directly measure the
  BT-875 vector (one of the ADR's strongest non-shrinkage benefits). Both
  cerl-direct and typed-Document-leaves close it equally well — this is a
  wash for the gate decision but matters for the typed-leaves alternative.

## Recommendation against the ADR's gates

The ADR 0088 (proposed) Status section sets three gates:

> * Phase 0a ≥15% projected shrinkage → migration justified on simplification grounds alone
> * Phase 0a ≤5% shrinkage → typed-Document-leaves alternative likely wins; ADR may be withdrawn
> * Between 5% and 15% → qualified recommendation explaining what additional data would decide

The audit lands in the **between zone**:

* Raw projected shrinkage: ~6–9% of codegen LOC (character-count basis).
* After deducting Phase 1 cerl infrastructure cost (estimated 800–1,500 LOC
  of new node-type, builder, and ETF-encoder code): ~4–7% net shrinkage.

### What would shift the recommendation

**Toward proceed (Phase 0b critical):**
The ADR's *strongest* remaining argument after this audit is the wire-cost
argument (Phase 0b). If ETF encode/decode is a small fraction of per-compile
cost (the ADR's hopeful case), cerl-direct unlocks compile-time wins
*independent* of LOC shrinkage. If Phase 0b lands favourably, the modest
codegen shrinkage measured here is sufficient justification to proceed —
because the simplification is a side benefit, not the primary motivation.

**Toward withdraw (typed-Document-leaves alternative):**
The same three rewrites should be done with typed-Document-leaves
(`Atom | VarName | StringLit | …` replacing `Document::String` for known
kinds, keeping the rest of `Document` intact). If typed-leaves achieves ≥60%
of the cerl-direct shrinkage measured here at ~1/30th the LOC cost (the ADR's
own counter-argument estimate), withdrawal in favour of the cheaper refactor
is justified — modest codegen shrinkage does not buy the 58K-LOC migration
the ADR proposes. **This is the single highest-value follow-up experiment.**

**Toward pivot (Alternative 7, Port+NIF):**
If Phase 0b shows ETF dominates per-compile cost, the ADR should pivot to
NIF-for-conversion before any Phase 1 work — the codegen shrinkage measured
here is too small to justify the migration on its own.

### Net recommendation

**Qualified: do not proceed with Phase 1 yet.**

1. Run Phase 0b (wire-cost napkin) — this remains in the epic and is now
   load-bearing for the decision.
2. **Add a Phase 0c**: typed-Document-leaves rewrite of the same three audit
   subjects. The ADR's own typed-leaves counter-argument is untested — this
   audit's data makes it *more* important, not less, to know how the
   alternative measures up against cerl-direct at the same three functions.
3. Decide Phase 1 *only* after both Phase 0b and Phase 0c data are in.

The audit specifically does **not** support the ADR's Phase 1 plan on the
simplification dimension alone. Shrinkage is real but plateaus at ~11% by
characters regardless of function complexity — it does not grow proportionally
with the emission-heaviness of the file, so the largest files (where migration
cost is highest) gain proportionally no more than small leaf utilities. The
cheaper typed-leaves alternative is now a serious contender and must be
measured before commitment.

## No regressions

* Rewritten functions live in `cerl_audit.rs`, gated `#[cfg(test)]`. Zero
  production code is touched. Originals in `util.rs`, `intrinsics.rs`, and
  `control_flow/mod.rs` are unchanged.
* `just test` passes: 9 new cerl_audit tests + all existing tests.
* The throwaway `cerl::*` types and rewrites will be deleted in a follow-up
  cleanup once the gate decision is made. The file is clearly marked as
  throwaway at the top.

## Files added/modified

* **New:** `crates/beamtalk-core/src/codegen/core_erlang/cerl_audit.rs`
  (throwaway, ~760 LOC including doc comments + 9 tests)
* **New:** `docs/ADR/0088-phase-0a-audit.md` (this memo)
* **Modified:** `crates/beamtalk-core/src/codegen/core_erlang/mod.rs` — added
  `#[cfg(test)] mod cerl_audit;` declaration

## References

* ADR (proposed): `docs/ADR/0088-direct-cerl-emission.md` (Phase 0a, Appendix A)
* Epic: BT-2313 (ADR 0088 Phase 0 decision gates)
* Audit rig: `crates/beamtalk-core/src/codegen/core_erlang/cerl_audit.rs`
* Hot-spot file: `crates/beamtalk-core/src/codegen/core_erlang/control_flow/mod.rs`
  (4,237 LOC, the one Appendix A explicitly flagged as the missing sample)
* Original `Document` API: `crates/beamtalk-core/src/codegen/core_erlang/document.rs`
* Related: BT-875 (eliminate `format!()` violations in Core Erlang codegen) —
  closed by typed-Document-leaves at far lower cost
