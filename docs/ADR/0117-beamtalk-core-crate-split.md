# ADR 0117: Splitting `beamtalk-core` into Sub-Crates

## Status
Proposed (2026-08-30)

## Context

`beamtalk-core` is 260,210 lines — by far the largest crate in the workspace (all other crates combined are smaller). It came up as a side question while executing BT-3323 (the Rust-coverage epic): would splitting it into sub-crates help with build times, compile-time module boundaries, or general navigability?

`docs/development/architecture-principles.md` §1 already documents an *aspirational* internal layering for `beamtalk-core`:

```
│ beamtalk-core (library)             │  ← Compiler core (reusable)
│  ├─ queries/     (Language Service) │
│  ├─ parse/       (Lexer, Parser)    │
│  ├─ analyse/     (Semantic Analysis)│
│  └─ codegen/     (Core Erlang gen)  │
```

with a stated rule: `codegen` depends on `parse`; `queries` depends on all three; nothing depends upward. That diagram describes a clean DAG. This ADR checked whether the codebase still matches it, since if it does, extracting each layer into its own crate is close to mechanical.

### What the real dependency graph looks like

Per-module line counts (`find <dir> -name '*.rs' | xargs wc -l`) and cross-module `use crate::<module>` edges, extracted directly from the source (not from the design doc):

| Module | Lines | Depends on (real, as of this session) |
|---|---|---|
| `codegen` | 90,631 | ast, method_source_walker, **repl**, semantic_analysis, source_analysis, test_helpers, **unparse** |
| `semantic_analysis` | 95,668 | ast, ast_walker, compilation, source_analysis, state_threading_selectors, test_helpers, **unparse** |
| `source_analysis` | 25,658 | ast, method_source_walker, test_helpers, **unparse** |
| `unparse` | 5,612 | ast, **codegen**, source_analysis |
| `ast` | 4,522 | **source_analysis**, test_helpers |
| `compilation` | 1,772 | ast, source_analysis, test_helpers |
| `repl` | 1,246 | ast, **codegen**, source_analysis |
| `queries` | 16,812 | ast, compilation, language_service, method_source_walker, semantic_analysis, source_analysis, unparse |
| `language_service` | 7,560 | ast, compilation, semantic_analysis, source_analysis, test_helpers |
| `lint` | 3,928 | ast, ast_walker, semantic_analysis, source_analysis |
| `project` | 412 | file_walker, test_helpers only |

Bold entries are the edges that don't fit the documented layering. Tracing them by hand:

- **`ast` ⇄ `source_analysis`** — `ast` uses `source_analysis::Span` and `source_analysis::parser` (a position/span type living inside the parser module, and something from the parser itself), while `source_analysis` naturally depends on `ast` to build nodes. Two-way.
- **`source_analysis` → `unparse`** — every one of `source_analysis`'s references to `unparse::format_source`/`reindent_method_source`/`unparse_method` is inside `#[cfg(test)]` code (`parser/tests/method_tests.rs`, a `#[test]` fn in `parser/expressions.rs`, and the whole `#[cfg(test)] mod method_span_corpus_tests` in `source_analysis/mod.rs`) — there is no production call site. This is test-only, exactly like the `codegen`↔`repl` edge below, not a two-way production dependency. `unparse` depends on `source_analysis` for parsing (`lex_with_eof`, `parse`, `parse_method`) and span types in production, so the edge is one-way in the direction that matters for a split.
- **`unparse` ⇄ `codegen`** — `unparse/mod.rs` and `unparse/leaf.rs` import `codegen::core_erlang::document::{Document, leaf}` directly: the typed-leaf `Document`/`docvec!` API mandated by ADR 0089 physically lives inside `codegen::core_erlang::document`, and `unparse` is built on top of it to get the same pretty-printing machinery for Beamtalk source (not Core Erlang). Going the other way, `codegen`'s `gen_server/native_facade.rs` and `gen_server/methods.rs` call `unparse::unparse_method_display_signature` / `unparse_type_annotation_display` to render human-readable signatures into generated code. Two-way, and the `Document`/`leaf` half of it is exactly the "Shared-Leaf-Module Pattern" violation §6 of the same principles doc already warns about — the shared leaf lives inside one of its two consumers instead of below both.
- **`codegen` ⇄ `repl`** — `repl`'s own `generate_repl_expression` depends on `codegen::core_erlang::{CodeGenError, document::Document, document::leaf}`, which is expected (REPL-specific codegen reuses the general codegen machinery). Going the other way, `codegen`'s *test* files (`property_tests.rs`, `core_erlang/tests/{gen_server,dispatch,expressions}.rs`) import `repl::codegen::generate_repl_expression` to validate REPL-specific codegen paths from inside `codegen`'s own test tree. This half is test-only, not production coupling.

Tracing reachability through the *production-only* edges above (dropping `codegen`↔`repl` and `source_analysis`→`unparse`, both test-only): `codegen` depends on `semantic_analysis`, which depends on `compilation`, which depends on `source_analysis`; `unparse` depends on `source_analysis` and `codegen` depends on `unparse` (`native_facade.rs`/`methods.rs`) while `unparse` depends on `codegen` (`Document`/`leaf`). That alone is a production cycle: `{ast, source_analysis, unparse, codegen, semantic_analysis, compilation}`.

**Correction (post-review):** the module-dependency table above was built by grepping `use crate::<module>` paths only and does not distinguish production code from `#[cfg(test)]`/`#[test]` code, and it undercounts real edges even within that scope. Verifying it against the source directly during review turned up three real, production edges the table omits entirely:

- `semantic_analysis::type_checker::validation::check_arg_sendability` (production, `pub(super) fn`) calls `crate::queries::announce_sites_query::is_announce_selector` — i.e. `semantic_analysis → queries`, in production.
- `queries::diagnostic_provider::compute_project_diagnostics_with_analysis` (production, `pub fn`) calls `crate::lint::check_near_miss_dividers` — i.e. `queries → lint`, in production.
- `queries` and `language_service` depend on each other extensively in production (dozens of call sites each direction — e.g. `queries::completion_provider`/`definition_provider`/`hover_provider` import `language_service::*` types at the top of the file, while `language_service::mod.rs`'s `check_native_delegate`, `find_selector_send_sites`, `find_implementors`, etc. call straight into `crate::queries::*`).

Since the ADR's own table already lists `queries` as depending on `semantic_analysis` and on `language_service`, these three findings close real two-way production cycles: **`queries ⇄ semantic_analysis`** and **`queries ⇄ language_service`**. Neither is test-only, so — unlike the `codegen`↔`repl` and `source_analysis`↔`unparse` edges — neither can be waved off as a dev-dependency (Cargo does permit cyclic dev-dependencies within a workspace; it does not permit them in ordinary `[dependencies]`). `queries` is not acyclic with respect to the SCC, and `language_service` is not acyclic with respect to `queries`. **This invalidates the "genuinely acyclic … 28,712 lines" claim this section originally made, and with it the premise of Decision step 1 below** (see the correction note there). `lint` and `project` are not implicated by these three findings and still appear to be one-way leaves.

The remaining ~6,400 lines are shared leaf modules (`ast_walker`, `ffi_receiver`, `method_source_walker`, `state_threading_selectors`, `test_helpers`, `file_walker`, `ffi_type_specs`, `erlang`, `synthetic_selectors`, `tool_expr`) consumed across nearly every module above; **only four of the ten are actually `pub(crate)`** in `lib.rs` (`ast_walker`, `ffi_receiver`, `method_source_walker`, `state_threading_selectors`) — the other six (`test_helpers`, `file_walker`, `ffi_type_specs`, `erlang`, `synthetic_selectors`, `tool_expr`) are already `pub`. Several of them are also not pure leaves: `test_helpers.rs` and `erlang.rs` both reach upward into `semantic_analysis`/`codegen`/`repl`/`unparse`, and `ffi_type_specs.rs` reaches into `codegen`/`semantic_analysis`. A crate split would still need a visibility audit, but starting from "6 of 10 already public, several already depend upward" rather than "all 10 are crate-private, pure downward leaves."

### Constraints

- CLAUDE.md's ThreadedIr rule and the extensive `verify()`-backed state-threading invariants live inside `codegen` — the part of the crate a split would most need to touch (to break the `codegen`↔`unparse` and `codegen`↔`repl` edges) is also the part the codebase treats as most sensitive to regress.
- `architecture-principles.md` §1 already decided enforcement is "document only (no automated enforcement)... solo developer, code review sufficient" for the existing binary/library layer boundaries. A crate split is a stronger form of the same enforcement mechanism this doc already considered and deferred.
- This came up as a tangent during BT-3323 (Rust coverage epic); it is explicitly out of scope for that epic and is being evaluated on its own merits here.

## Decision

**Do not attempt a full crate split now.** Instead:

> **⚠️ Correction (post-review, not yet resolved by the author):** Step 1 below is written as "requires no cycle-breaking... near-zero regression risk." That is false as written — see the correction note in Context above. `queries ⇄ semantic_analysis` and `queries ⇄ language_service` are both real, production, two-way dependencies, so `queries` and `language_service` are part of the same tangle as the rest of the crate, not a clean downstream layer sitting atop it. Step 1 as scoped is not implementable without first breaking those two cycles (or accepting `queries`/`language_service` into whatever the first split boundary is). This needs the author to re-decide the shape of step 1, not a mechanical edit — left as-is below pending that call. Step 2's four items are also incomplete (they don't cover the two newly-found cycles) and item 2's and item 4's framing needs adjusting — see the notes inline.

1. **Extract the acyclic downstream layer** (`queries`, `language_service`, `lint`, `project` — 28,712 lines, ~11% of the crate) into 1-2 new library crates sitting atop the existing `beamtalk-core` (which keeps the 225k-line SCC as-is). ~~This requires no cycle-breaking~~ **(see correction above — this is no longer accurate)**, touches none of the ThreadedIr-verified codegen paths, and gives a real, compiler-enforced boundary for the part of the crate that's genuinely independent today. `queries` (16,812 lines, itself depends on `language_service`) is the natural anchor; `lint` and `project` can fold in alongside it or stay separate — worth a short spike to decide, not a design question this ADR needs to settle up front.
2. **File the cycle-breaking fixes as separate, standalone hygiene work** (not gated on a crate-split decision — each is independently valuable per architecture-principles.md §6). The original four (below) are not the complete list — see the correction above for two more (`queries⇄semantic_analysis`, `queries⇄language_service`) the author still needs to scope:
   - Extract `codegen::core_erlang::document` (the `Document`/`leaf`/`docvec!` API) into its own leaf module beneath both `codegen` and `unparse`, rather than living inside `codegen`. This is the biggest single edge and a textbook Shared-Leaf-Module Pattern fix. Note this only removes `unparse`'s dependency on `codegen`; `codegen` still depends on `unparse` in production (`unparse_method_display_signature` etc. in `native_facade.rs`/`methods.rs`) — that remaining edge is one-way and does not itself need breaking for a split.
   - Move the REPL-specific codegen test cases (`property_tests.rs`, `core_erlang/tests/{gen_server,dispatch,expressions}.rs`'s uses of `repl::codegen::*`) into `repl`'s own test suite. Worth doing for hygiene/clarity, but note Cargo permits cyclic *dev*-dependencies in a workspace, so this edge (being test-only) was never actually a blocker for splitting `codegen` and `repl` into separate crates — the "fully removes codegen's only edge into repl" framing overstates why this matters.
   - Extract `source_analysis::Span` (and whatever `source_analysis::parser` item `ast` needs) into a leaf module beneath both `ast` and `source_analysis`.
   - ~~Investigate why `source_analysis` calls into `unparse`~~ — resolved by this review (see Context correction): every such call is `#[cfg(test)]`-only (round-trip/formatting tests), not a production call site, so there is no design question here. The fix is the same shape as the REPL-tests item above: move `source_analysis`'s unparse-round-trip tests (`parser/tests/method_tests.rs`, the relevant `#[test]` in `parser/expressions.rs`, `method_span_corpus_tests.rs`) into `unparse`'s own test tree. Size S, not "unknown/possibly a redesign."
3. **Revisit a full split only after all cycle-breaking fixes are done** (the original four plus whatever the author scopes for `queries⇄semantic_analysis`/`queries⇄language_service`), and only if the payoff still looks worthwhile at that point (see Consequences — it may not).

## Prior Art

Large Rust compilers commonly split into many crates for exactly the boundary-enforcement reason this ADR's Decision leans on, not primarily for parallel-build speed on a sequential pipeline:

- **rustc** is split into dozens of `rustc_*` crates (`rustc_ast`, `rustc_parse`, `rustc_hir`, `rustc_middle`, `rustc_codegen_ssa`, ...), largely along the same lexer→parser→HIR→typeck→codegen pipeline shape this crate has, with tools like `rustc_ast_pretty` occupying the same kind of position `unparse` does here.
- **rust-analyzer** splits its `syntax`/`parser`/`hir`/`hir-def`/`ide` layers into separate crates specifically so lower layers (syntax, parsing) can be reused and tested independently of the IDE-facing layers — the same shape as this crate's `language_service`/`queries` sitting atop the parser/analysis core.

Both are evidence that the *target* shape (once the cycles are fixed) is a well-trodden pattern for compiler workspaces this size — not evidence that skipping the cycle-breaking prerequisite is safe. Neither is a direct parallel for the "should we do this now" question this ADR answers, since both are `rustc`/`rust-analyzer`-scale projects with dedicated teams; the closer comparison for *when* to pay this cost is this repo's own `architecture-principles.md` §1, which already chose "document only" enforcement for the workspace's existing binary/library boundary on the grounds of solo-developer scale.

## User Impact

This is an internal Rust-workspace-organization decision, not a Beamtalk language-design one, so the usual newcomer/Smalltalk-developer/Erlang-developer/operator personas from other ADRs don't apply — nothing here is visible from the REPL or from Beamtalk source code. The relevant stakeholders instead:

- **Contributor navigating the codebase** — unaffected either way in the short term (Decision defers the big split); the acyclic-layer extraction (step 1) gives a small win: `cargo doc`/`rust-analyzer` can show a smaller, more specific dependency set for `queries`/`language_service`/`lint` work.
- **CI/build-time steward** — no meaningful change from step 1 alone (11% of a crate that's still fundamentally a sequential pipeline won't move wall-clock build time much). The cycle-breaking fixes (step 2) are pure internal moves, also no build-time effect expected.
- **Tooling developer** (LSP/MCP, both consumers of `beamtalk-core`'s public API) — no interface change from this decision; a future full split (Alternative A) would change import paths (`beamtalk_core::codegen::...` → `beamtalk_codegen::...` or similar) and is exactly the kind of breaking change that should wait until the cycles are already fixed and validated independently, per the Decision's ordering.

## Steelman Analysis

### Alternative A (full split now) — the argument for it
A compiler maintainer optimizing for the *end state* would argue: doing the cycle-breaking and the split together is one round of review and one round of regression-testing instead of two, and the cycle fixes alone (Decision step 2) don't return any visible value until the split actually happens — so why not finish the job? This is a real argument when the team has spare capacity and low risk-aversion; it's weaker here because `codegen` carries the ThreadedIr `verify()` invariants CLAUDE.md flags as the codebase's most safety-critical, best-tested area — exactly where "two changes in one review" is the more expensive way to find a regression, not the cheaper one.

### Alternative B (no action) — the argument for it
An operator or a maintainer wary of process overhead would argue: `architecture-principles.md` §1 already decided "document only, code review sufficient" is enough enforcement for a much simpler boundary (binary crates vs. `beamtalk-core`) at solo-developer scale — extending that same judgment to internal modules within `beamtalk-core` is consistent, and the acyclic-layer extraction (Decision step 1) is arguably solving a problem no one has actually reported (compile times, navigability pain) rather than a problem this investigation set out to find. This is a fair challenge to step 1 specifically: it's a low-cost, low-risk change, but "low-cost" isn't the same as "requested" — worth the maintainer confirming compile-time/navigability pain is real before spending even the modest effort step 1 needs.

**Tension point:** the honest case for doing nothing at all (Alternative B) is stronger than the case for doing everything now (Alternative A) — this ADR's Decision sits deliberately between them, and a maintainer who agrees with B's skepticism should treat Decision step 1 as optional rather than drop the ADR's more clearly-justified step 2 (the cycle fixes, which pay down a documented principle violation regardless of whether any split ever happens).

## Alternatives Considered

### A. Full split now, cycle-breaking included in the same effort
Break all four cycles and split into ~8 crates (`beamtalk-ast`, `beamtalk-source-analysis`, `beamtalk-semantic-analysis`, `beamtalk-codegen`, `beamtalk-unparse`, `beamtalk-repl`, `beamtalk-compilation`, plus the already-acyclic `beamtalk-language-service`/`beamtalk-queries`/`beamtalk-lint`) in one XL effort.

Rejected for now: this bundles the highest-risk part of the work (editing `codegen`'s Document API and `source_analysis`'s parser, both load-bearing and heavily tested) with the split itself, so a regression is hard to attribute to "the refactor" vs. "the split." Splitting the hygiene fixes out first (this ADR's Decision) means each can be validated independently against the existing test suite before any crate boundary changes.

### B. No action at all
Leave `beamtalk-core` as one crate indefinitely; treat this investigation as answering the question without committing to any follow-up.

Rejected: the acyclic-layer extraction (Decision step 1) is low-risk, low-effort, and delivers a real, if modest, boundary win — there's no reason to leave that on the table just because the bigger split isn't worth doing yet. The cycle-breaking fixes (step 2) also independently pay down a documented architecture-principle violation (§6) regardless of whether a crate split ever happens. (See Steelman Analysis above for the strongest version of this alternative's case.)

### C. Merge `codegen` and `unparse` into one crate permanently, split everything else
Given how tightly `unparse` depends on `codegen`'s Document/leaf API, one option is to treat them as permanently one unit (a `beamtalk-codegen` crate covering both Core-Erlang generation and Beamtalk-source pretty-printing) and only extract the *other* three cycle edges.

Not rejected — this is a plausible shape for a future full split (Alternative A) once the `codegen`↔`repl` and `ast`↔`source_analysis`↔`unparse` edges are broken; recorded here so it isn't lost, not decided now since no split is happening yet.

## Consequences

### Positive
- ~~Extracting `queries`/`language_service`/`lint`/`project` gives a real, `cargo`-enforced boundary for ~11% of the crate at low cost and near-zero regression risk.~~ **No longer accurate as scoped** — see the correction under Decision. `lint`/`project` alone (4,340 lines, ~1.7%) may still qualify; `queries`/`language_service` do not until their cycles with `semantic_analysis` are broken.
- The four cycle-breaking fixes each independently resolve a documented Shared-Leaf-Module Pattern violation (architecture-principles.md §6), improving code health whether or not a further split ever happens.
- This investigation corrects the architecture-principles.md diagram's implicit claim that `beamtalk-core`'s internals are already a clean DAG — that assumption should not be relied on elsewhere until the cycles are fixed.

### Negative
- The full split (Alternative A) remains undone; if compile-time or navigability pain is the actual motivating problem, this decision doesn't solve it yet — it defers the biggest 86.5% of the crate.
- Splitting even the acyclic layer means widening whatever `pub(crate)` items `queries`/`language_service`/`lint` currently reach into across the new crate boundary — needs a visibility audit before the split, not assumed to be free.

### Neutral
- Given the pipeline shape (`source_analysis` → `semantic_analysis` → `codegen`/`unparse` is fundamentally sequential even once cycle-free), a full split's main benefit is boundary enforcement and touch-surface reduction on unrelated changes — not parallel build speedup, since a linear dependency chain compiles serially with or without crate boundaries. This tempers how much Alternative A is worth pursuing even after the cycles are fixed; that judgment call is deferred to whoever revisits this (Decision step 3), informed by real profiling at that time rather than assumed here.

## Implementation

Rough phases, each independently landable and independently valuable (none blocks on the others except step 3, which needs step 2 done first):

1. **Acyclic-layer extraction** (Decision step 1): **blocked on rescoping** — `queries` and `language_service` are not acyclic w.r.t. the SCC (see the correction under Decision above), so this step needs the author to decide whether to (a) break `queries⇄semantic_analysis` and `queries⇄language_service` first, expanding step 2, or (b) fold `queries`/`language_service` into the same crate as the SCC and extract only `lint`/`project` (which do appear to still be genuine one-way leaves) here. Size: unknown until rescoped.
2. **Cycle-breaking fixes** (Decision step 2), each its own PR:
   - Extract `codegen::core_erlang::document` to a shared leaf module. Size: M (touches every `use crate::codegen::core_erlang::document::*` call site). Does not remove `codegen`'s own dependency on `unparse` (`unparse_method_display_signature` etc.) — that edge is one-way and doesn't need fixing for a split.
   - Move REPL-codegen tests from `codegen`'s test tree into `repl`'s. Size: S. Hygiene/clarity, not split-blocking (Cargo allows cyclic dev-dependencies).
   - Extract `source_analysis::Span` (+ needed parser item) to a shared leaf module. Size: S-M.
   - Move `source_analysis`'s unparse-round-trip tests into `unparse`'s test tree (confirmed test-only, not a redesign). Size: S.
   - **New, not yet sized:** break `queries⇄semantic_analysis` (`semantic_analysis::type_checker::validation::check_arg_sendability` → `queries::announce_sites_query::is_announce_selector`) and `queries⇄language_service` (extensive, both directions — completion/definition/hover providers on one side, `check_native_delegate`/`find_selector_send_sites`/`find_implementors` etc. on the other). Author needs to scope these; the `language_service⇄queries` edge in particular looks substantial, not a quick extraction.
3. **Re-evaluate full split** (Decision step 3): once step 2 lands, re-run this ADR's dependency-graph extraction — grepping `use crate::` paths only, as the original did, was not sufficient; verify production vs. `#[cfg(test)]` for each edge found — to confirm the SCC is actually gone, then decide whether the full split (Alternative A, informed by Alternative C's merge-codegen-and-unparse shape) is still worth doing given real profiling data at that time.

None of this is scheduled against a Linear epic yet — file issues under a new epic if the maintainer wants to act on this ADR, parented separately from BT-3323 (which this is out of scope for).

## References
- Related issues: none yet — this ADR did not originate from a Linear issue; came up during BT-3323 (Rust coverage epic)
- Related ADRs: ADR 0089 (Typed Document Leaves) — the `Document`/`leaf`/`docvec!` API whose placement inside `codegen` is the root of the biggest cycle edge found here
- Documentation: `docs/development/architecture-principles.md` §1 (Layered Architecture & Dependency Rules), §6 (Duplication & the Shared-Leaf-Module Pattern)
