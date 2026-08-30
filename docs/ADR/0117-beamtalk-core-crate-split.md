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
- **`source_analysis` ⇄ `unparse`** — `source_analysis` calls `unparse::format_source`, `unparse::reindent_method_source`, `unparse::unparse_method` (the parser reaches into the pretty-printer, apparently for diagnostic/quick-fix text), while `unparse` depends on `source_analysis` for AST/span types. Two-way.
- **`unparse` ⇄ `codegen`** — `unparse/mod.rs` and `unparse/leaf.rs` import `codegen::core_erlang::document::{Document, leaf}` directly: the typed-leaf `Document`/`docvec!` API mandated by ADR 0089 physically lives inside `codegen::core_erlang::document`, and `unparse` is built on top of it to get the same pretty-printing machinery for Beamtalk source (not Core Erlang). Going the other way, `codegen`'s `gen_server/native_facade.rs` and `gen_server/methods.rs` call `unparse::unparse_method_display_signature` / `unparse_type_annotation_display` to render human-readable signatures into generated code. Two-way, and the `Document`/`leaf` half of it is exactly the "Shared-Leaf-Module Pattern" violation §6 of the same principles doc already warns about — the shared leaf lives inside one of its two consumers instead of below both.
- **`codegen` ⇄ `repl`** — `repl`'s own `generate_repl_expression` depends on `codegen::core_erlang::{CodeGenError, document::Document, document::leaf}`, which is expected (REPL-specific codegen reuses the general codegen machinery). Going the other way, `codegen`'s *test* files (`property_tests.rs`, `core_erlang/tests/{gen_server,dispatch,expressions}.rs`) import `repl::codegen::generate_repl_expression` to validate REPL-specific codegen paths from inside `codegen`'s own test tree. This half is test-only, not production coupling.

Tracing reachability through these edges: `codegen` depends on `semantic_analysis`, which depends on `compilation`, which depends on `source_analysis`, which depends on `unparse`, which depends back on `codegen` — and `codegen` also depends directly on `repl`, which depends back on `codegen`. Everything in that chain is mutually reachable, i.e. one strongly-connected component:

**`{ast, source_analysis, unparse, codegen, repl, semantic_analysis, compilation}` = 225,109 lines — 86.5% of the crate.**

Rust's crate graph must be a DAG; a cycle anywhere in a set of modules means *none* of them can be split into separate crates without first breaking every cycle edge that touches them. This isn't "a linear pipeline with modest parallelism benefit" (the assumption this investigation started with) — it's that the substantial majority of `beamtalk-core`, including its two largest modules (`codegen` and `semantic_analysis`), cannot be split at all under the current module structure.

What's left is genuinely acyclic today: `language_service` (7,560), `lint` (3,928), `queries` (16,812), and `project` (412) — 28,712 lines (~11%) — each depends only on the SCC (or on each other) and nothing in the SCC depends back on them.

The remaining ~6,400 lines are already-shared, crate-private leaf modules (`ast_walker`, `ffi_receiver`, `method_source_walker`, `state_threading_selectors`, `test_helpers`, `file_walker`, `ffi_type_specs`, `erlang`, `synthetic_selectors`, `tool_expr`) declared `pub(crate)` in `lib.rs` and consumed across nearly every module above. Any crate split would need every one of these promoted to `pub`, widening their visibility from "anyone in this crate" to "anyone depending on whichever new crate hosts them."

### Constraints

- CLAUDE.md's ThreadedIr rule and the extensive `verify()`-backed state-threading invariants live inside `codegen` — the part of the crate a split would most need to touch (to break the `codegen`↔`unparse` and `codegen`↔`repl` edges) is also the part the codebase treats as most sensitive to regress.
- `architecture-principles.md` §1 already decided enforcement is "document only (no automated enforcement)... solo developer, code review sufficient" for the existing binary/library layer boundaries. A crate split is a stronger form of the same enforcement mechanism this doc already considered and deferred.
- This came up as a tangent during BT-3323 (Rust coverage epic); it is explicitly out of scope for that epic and is being evaluated on its own merits here.

## Decision

**Do not attempt a full crate split now.** Instead:

1. **Extract the acyclic downstream layer** (`queries`, `language_service`, `lint`, `project` — 28,712 lines, ~11% of the crate) into 1-2 new library crates sitting atop the existing `beamtalk-core` (which keeps the 225k-line SCC as-is). This requires no cycle-breaking, touches none of the ThreadedIr-verified codegen paths, and gives a real, compiler-enforced boundary for the part of the crate that's genuinely independent today. `queries` (16,812 lines, itself depends on `language_service`) is the natural anchor; `lint` and `project` can fold in alongside it or stay separate — worth a short spike to decide, not a design question this ADR needs to settle up front.
2. **File the four cycle-breaking fixes as separate, standalone hygiene work** (not gated on a crate-split decision — each is independently valuable per architecture-principles.md §6):
   - Extract `codegen::core_erlang::document` (the `Document`/`leaf`/`docvec!` API) into its own leaf module beneath both `codegen` and `unparse`, rather than living inside `codegen`. This is the biggest single edge and a textbook Shared-Leaf-Module Pattern fix.
   - Move the REPL-specific codegen test cases (`property_tests.rs`, `core_erlang/tests/{gen_server,dispatch,expressions}.rs`'s uses of `repl::codegen::*`) into `repl`'s own test suite. This fully removes `codegen`'s only edge into `repl` (it's test-only today).
   - Extract `source_analysis::Span` (and whatever `source_analysis::parser` item `ast` needs) into a leaf module beneath both `ast` and `source_analysis`.
   - Investigate why `source_analysis` calls into `unparse` (`format_source`, `reindent_method_source`, `unparse_method`) — this is the one edge that isn't an obvious shared-type extraction; it may need an actual redesign (moving the call site up out of `source_analysis`, or exposing what `unparse` needs as data rather than having the parser call the pretty-printer directly).
3. **Revisit a full split only after all four are fixed**, and only if the payoff still looks worthwhile at that point (see Consequences — it may not).

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
- Extracting `queries`/`language_service`/`lint`/`project` gives a real, `cargo`-enforced boundary for ~11% of the crate at low cost and near-zero regression risk.
- The four cycle-breaking fixes each independently resolve a documented Shared-Leaf-Module Pattern violation (architecture-principles.md §6), improving code health whether or not a further split ever happens.
- This investigation corrects the architecture-principles.md diagram's implicit claim that `beamtalk-core`'s internals are already a clean DAG — that assumption should not be relied on elsewhere until the cycles are fixed.

### Negative
- The full split (Alternative A) remains undone; if compile-time or navigability pain is the actual motivating problem, this decision doesn't solve it yet — it defers the biggest 86.5% of the crate.
- Splitting even the acyclic layer means widening whatever `pub(crate)` items `queries`/`language_service`/`lint` currently reach into across the new crate boundary — needs a visibility audit before the split, not assumed to be free.

### Neutral
- Given the pipeline shape (`source_analysis` → `semantic_analysis` → `codegen`/`unparse` is fundamentally sequential even once cycle-free), a full split's main benefit is boundary enforcement and touch-surface reduction on unrelated changes — not parallel build speedup, since a linear dependency chain compiles serially with or without crate boundaries. This tempers how much Alternative A is worth pursuing even after the cycles are fixed; that judgment call is deferred to whoever revisits this (Decision step 3), informed by real profiling at that time rather than assumed here.

## Implementation

Rough phases, each independently landable and independently valuable (none blocks on the others except step 3, which needs step 2 done first):

1. **Acyclic-layer extraction** (Decision step 1): spike whether `queries`+`language_service`+`lint`+`project` become one crate or two/three; move the code; fix up `Cargo.toml` dependency edges; re-run full test suite. Size: M.
2. **Four cycle-breaking fixes** (Decision step 2), each its own PR:
   - Extract `codegen::core_erlang::document` to a shared leaf module. Size: M (touches every `use crate::codegen::core_erlang::document::*` call site).
   - Move REPL-codegen tests from `codegen`'s test tree into `repl`'s. Size: S.
   - Extract `source_analysis::Span` (+ needed parser item) to a shared leaf module. Size: S-M.
   - Redesign `source_analysis`'s three calls into `unparse`. Size: unknown until investigated — likely M, possibly larger if it turns out to be load-bearing for diagnostics/quick-fixes rather than incidental.
3. **Re-evaluate full split** (Decision step 3): once step 2 lands, re-run this ADR's dependency-graph extraction to confirm the SCC is gone, then decide whether the full split (Alternative A, informed by Alternative C's merge-codegen-and-unparse shape) is still worth doing given real profiling data at that time.

None of this is scheduled against a Linear epic yet — file issues under a new epic if the maintainer wants to act on this ADR, parented separately from BT-3323 (which this is out of scope for).

## References
- Related issues: none yet — this ADR did not originate from a Linear issue; came up during BT-3323 (Rust coverage epic)
- Related ADRs: ADR 0089 (Typed Document Leaves) — the `Document`/`leaf`/`docvec!` API whose placement inside `codegen` is the root of the biggest cycle edge found here
- Documentation: `docs/development/architecture-principles.md` §1 (Layered Architecture & Dependency Rules), §6 (Duplication & the Shared-Leaf-Module Pattern)
