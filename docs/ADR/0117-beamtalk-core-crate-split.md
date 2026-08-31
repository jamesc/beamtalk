# ADR 0117: Splitting `beamtalk-core` into Sub-Crates

## Status
Accepted (2026-08-30)

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

Since the ADR's own table already lists `queries` as depending on `semantic_analysis` and on `language_service`, these three findings close real two-way production cycles: **`queries ⇄ semantic_analysis`** and **`queries ⇄ language_service`**. Neither is test-only, so — unlike the `codegen`↔`repl` and `source_analysis`↔`unparse` edges — neither can be waved off as a dev-dependency (Cargo does permit cyclic dev-dependencies within a workspace; it does not permit them in ordinary `[dependencies]`). `queries` is not acyclic with respect to the SCC, and `language_service` is not acyclic with respect to `queries`. **This invalidates the "genuinely acyclic … 28,712 lines" claim this section originally made.** `lint` and `project` are not implicated by these three findings and still appear to be one-way leaves. The Decision below has been rewritten around this corrected graph, framed by DDD bounded context rather than by Rust module.

The remaining ~6,400 lines are shared leaf modules (`ast_walker`, `ffi_receiver`, `method_source_walker`, `state_threading_selectors`, `test_helpers`, `file_walker`, `ffi_type_specs`, `erlang`, `synthetic_selectors`, `tool_expr`) consumed across nearly every module above; **only four of the ten are actually `pub(crate)`** in `lib.rs` (`ast_walker`, `ffi_receiver`, `method_source_walker`, `state_threading_selectors`) — the other six (`test_helpers`, `file_walker`, `ffi_type_specs`, `erlang`, `synthetic_selectors`, `tool_expr`) are already `pub`. Several of them are also not pure leaves: `erlang.rs` reaches upward into `codegen`/`repl` and `ffi_type_specs.rs` reaches into `codegen`/`semantic_analysis`, both in genuine production code. (`test_helpers.rs` looked the same at first grep, but doesn't hold up under the same production-vs-test scrutiny applied elsewhere in this section: its only upward reference — `semantic_analysis::class_hierarchy::DeclaredType` — is inside `pub mod test_support`, gated `#[cfg(any(test, feature = "test"))]`; its apparent `unparse` reference is a doc comment, not code; it has no reference to `codegen`/`repl` at all. It's a pure leaf in production.) A crate split would still need a visibility audit, but starting from "6 of 10 already public, two genuinely depend upward" rather than "all 10 are crate-private, pure downward leaves."

### What `beamtalk-core`'s actual consumers use (post-review addition)

`beamtalk-core` has three consumers today: `beamtalk-cli`, `beamtalk-lsp`, `beamtalk-mcp`. Grepping each consumer crate's own source for `beamtalk_core::<module>` (i.e. what they use across the public-API boundary, not `beamtalk-core`'s internal `crate::` edges) gives an empirical answer to "is there a shared core beneath the compiler proper":

| Module | `beamtalk-lsp` | `beamtalk-mcp` | `beamtalk-cli` |
|---|:---:|:---:|:---:|
| `source_analysis`, `semantic_analysis`, `unparse`, `queries`, `language_service`, `project` | ✓ | ✓ | ✓ |
| `lint` | | ✓ | ✓ |
| **`codegen`** (90,631 lines, ~35% of the crate) | | | ✓ (only) |
| **`repl`** | | | ✓ (only) |

Neither `beamtalk-lsp` nor `beamtalk-mcp` references `codegen` or `repl` anywhere in their own source. Only `beamtalk-cli` does (it's the only consumer that actually compiles to Core Erlang / builds and runs code). So the shared-core boundary isn't just an abstract DDD label — it's already visible in how the three consumers actually use the crate: **parse + analyze + unparse + query/lint is what LSP and MCP need; `codegen` (and its REPL-codegen sibling `repl`) is CLI-only.**

The catch: today they get `codegen` anyway, transitively, through exactly one thread — `unparse` imports `codegen::core_erlang::document::{Document, leaf}` (the edge already flagged above), and `semantic_analysis` depends on `unparse` in production via `class_hierarchy::class_info::format_default_value`, which calls `unparse::unparse_literal_display` (`declared_type.rs`'s own call to `unparse::unparse_type_annotation_display` is test-only, inside `#[cfg(test)] mod tests` — not a second production call site). So `beamtalk-lsp`/`beamtalk-mcp` currently pull in all of `codegen` despite never calling it. Extracting the Document API to a shared leaf (Decision step 4 below) isn't just intra-Compilation hygiene, then — it's the one fix that would let a future `beamtalk-analysis`-shaped crate exclude `codegen` entirely, which *would* be a real, measurable build-time/type-checking-scope win for LSP and MCP specifically (not for the CLI, which needs `codegen` regardless) — see the revised Consequences note.

### Constraints

- CLAUDE.md's ThreadedIr rule and the extensive `verify()`-backed state-threading invariants live inside `codegen` — the part of the crate a split would most need to touch (to break the `codegen`↔`unparse` and `codegen`↔`repl` edges) is also the part the codebase treats as most sensitive to regress.
- `architecture-principles.md` §1 already decided enforcement is "document only (no automated enforcement)... solo developer, code review sufficient" for the existing binary/library layer boundaries. A crate split is a stronger form of the same enforcement mechanism this doc already considered and deferred.
- This came up as a tangent during BT-3323 (Rust coverage epic); it is explicitly out of scope for that epic and is being evaluated on its own merits here.

## Decision

**Do not attempt a full crate split now. Resolve the cross-context cycles first, and use this repo's own DDD-context vocabulary — `Language Service`, `Compilation`, `Runtime`, `REPL` (CLAUDE.md § Architecture) — as the split boundary, not the internal Rust module list the original version of this Decision used.**

### Why bounded contexts, not modules

The corrected dependency graph (Context, above) contains two qualitatively different kinds of cycle:

1. **Cycles *within* the compiler pipeline** — `ast` ⇄ `source_analysis`, `unparse` ⇄ `codegen`, plus `semantic_analysis`/`compilation` calling down into `source_analysis`. This is `{ast, source_analysis, unparse, codegen, semantic_analysis, compilation}` — 225,109 lines. Every module in it belongs to one DDD bounded context: **Compilation**. A cycle *within* one bounded context is internal cohesion, not an architecture violation — Rust `mod`s inside a single crate are not required to be acyclic with each other, only crates are. None of this needs to move for a crate split to be possible; it can stay exactly one crate indefinitely.
2. **Cycles *across* what are supposed to be separate bounded contexts** — `queries ⇄ semantic_analysis` and `queries ⇄ language_service`. `queries` and `language_service` are meant to be the **Language Service** context (`architecture-principles.md`'s own diagram labels the layer `queries/ (Language Service)`, and CLAUDE.md lists "Language Service" as a top-level DDD context) — a *consumer* of Compilation, not a peer entangled with it. These are the cycles that actually block a split, because they cross a context boundary.

The original version of this Decision (see git history) picked the wrong four cycles to fix first: three of the four (`unparse`↔`codegen`'s Document API, the REPL-test move, the `source_analysis`/`unparse` test move) are *intra*-Compilation or already test-only — good hygiene, but they were never what stood between this codebase and a Language-Service/Compilation split. The two cycles that actually matter (`queries⇄semantic_analysis`, `queries⇄language_service`) weren't in the original table at all. Revised plan:

1. **Add a CI-enforced module-dependency check.** A script that fails CI when a new `use crate::<module>` edge crosses a declared bounded-context boundary in the wrong direction (Compilation → Language Service is fine; the reverse is not). This is the single highest-leverage step: it's what would have caught `architecture-principles.md` §1's diagram silently going stale in the first place — nothing noticed `queries`/`semantic_analysis` drifting into a cycle until this ADR's review dug for it by hand. Pays off whether or not any crate ever actually gets split, costs one script, and touches no production code. Size: S. **Do this first, regardless of anything else below.**
2. **Extract the modules that are genuinely one-way today**: `lint` (3,928 lines) and `project` (412 lines) depend only on Compilation and each other, and nothing depends back on them in production. `repl` (1,246 lines) depends only on `codegen`/`ast`/`source_analysis` in production (its one back-edge, `codegen`'s test files calling `repl::codegen`, is test-only). Together ~5,600 lines, ~2% of the crate — smaller than this ADR's original 11% estimate (which wrongly included `queries`/`language_service`), but real and low-risk today, no cycle-breaking required. Size: S-M.
3. **Break the two Language-Service ↔ Compilation cycles** — the actual prerequisite for splitting anything bigger:
   - **`queries ⇄ semantic_analysis`** — one edge: `semantic_analysis::type_checker::validation::check_arg_sendability` (ADR 0103's process-boundary sendability check) calls `queries::announce_sites_query::is_announce_selector`. That function is a three-string membership check (`pub(crate)`, no dependency on the rest of `queries`' AST-mining machinery) — a shared *vocabulary fact* ("which selectors mean this is an announce send"), not a query, needed by both a semantic check and a static-discoverability query. This crate already has the right pattern for exactly this situation: `synthetic_selectors.rs`, a `pub`, top-level leaf module whose own doc comment says "Keeping the selector-name computation here... makes this module the single source of truth and removes the drift risk." Move `ANNOUNCE_SELECTORS`/`is_announce_selector` into a leaf module of the same shape (new file, or folded into `synthetic_selectors.rs` if the author judges the theme close enough) and have both `semantic_analysis` and `queries::announce_sites_query` depend on it instead of one reaching into the other. Size: S.
   - **`queries ⇄ language_service`** — extensive, both directions, and not yet fully inventoried (this review sampled it, not exhaustively — needs a spike before sizing). The sampled shape: `language_service` (the LSP-facing orchestrator — file/project indexing, `ProjectIndex`) calls into `queries` for actual query behavior (`check_native_delegate`, `find_selector_send_sites`, `find_implementors`, ...) — the expected direction, Language Service's orchestrator consuming its own query implementations. Going the other way, `queries`' provider modules (`completion_provider`, `definition_provider`, `document_symbols_provider`, `hover_provider`, ...) import plain result/protocol types defined in `language_service` (`Position`, `Location`, `Completion`, `DocumentSymbol`, `HoverInfo`, `ByteOffset`). Two options, in order of preference:
     - **(a, recommended default) Merge `queries` and `language_service` into one module.** They are already one DDD context per CLAUDE.md; splitting that context into two Rust modules with no enforced boundary between them is very plausibly *why* they drifted into an unnoticed cycle. Merging removes the cycle by construction and matches the DDD-context framing this Decision is built on.
     - **(b)** If the author has a specific reason to keep them separate (e.g. wanting `queries` usable standalone by a non-LSP consumer — the MCP search tool in ADR 0062 is the closest candidate, worth checking whether it actually needs `queries` without `language_service`), extract the shared protocol types into a leaf module beneath both, and audit that no other `queries → language_service` behavioral call remains beyond type usage.
     Size: M, pending the spike.
4. **Do the remaining intra-Compilation hygiene fixes whenever convenient** — none of these block a Language-Service/Compilation split, so they're not on the critical path, but the first item has a second justification beyond hygiene (see Context, "What `beamtalk-core`'s actual consumers use", and Consequences/Neutral): it's the one thing standing between `beamtalk-lsp`/`beamtalk-mcp` and never having to compile `codegen` at all, so consider prioritizing it ahead of the other three:
   - Extract `codegen::core_erlang::document` (`Document`/`leaf`/`docvec!`) into **its own crate** (e.g. `beamtalk-cerl-doc`), not just an internal leaf module — a real crate boundary enforces the split at compile time rather than by convention, and it's the first physically separable piece of any eventual larger split (see the refinement note under step 5). Checked during review: `document/` (1,090 lines) reaches back into `codegen` for exactly two things, both easily freed — `escape_atom_chars`/`escape_core_erlang_string` (`util.rs`, ~20 lines) and `CoreErlangGenerator::binary_string_literal`/`binary_byte_segments` (`gen_server/spawn.rs`; `binary_string_literal` is `pub(crate)`, `binary_byte_segments` is `pub(in crate::codegen::core_erlang)` — narrower, but both static with no `&self`, so both move cleanly). All three move into the new crate together, ~1,150 lines total, with zero remaining dependency on the rest of `codegen`. `unparse` and `codegen` both depend on the new crate; neither depends on the other for this anymore. Size: M (touches every `use crate::codegen::core_erlang::document::*` call site, but the extraction itself is small and self-contained). (Leaves `codegen`'s own one-way dependency on `unparse` — `unparse_method_display_signature` etc. — untouched; that edge doesn't need fixing.)
   - Move the REPL-codegen test cases out of `codegen`'s test tree into `repl`'s. Size: S. (Hygiene only — Cargo permits cyclic dev-dependencies, so this was never split-blocking.)
   - Extract `source_analysis::Span` (+ needed parser item) to a shared leaf beneath `ast` and `source_analysis`. Size: S-M.
   - Move `source_analysis`'s unparse-round-trip tests into `unparse`'s test tree. Size: S. (Also hygiene only, for the same dev-dependency reason.)
5. **Split at the bounded-context boundary once step 3 lands**: `beamtalk-compilation` (the current SCC, `ast`/`source_analysis`/`unparse`/`codegen`/`semantic_analysis`/`compilation`, ~225k lines, staying one crate — no further internal splitting proposed, see Consequences on why parallel-build payoff is low for a linear pipeline regardless) beneath `beamtalk-language-service` (merged `queries`+`language_service`, ~24k lines, per option 3a) and `beamtalk-repl` (~1.2k lines), with `lint`/`project` already extracted in step 2. Re-run the dependency-graph extraction first to confirm — by reading production vs. test call sites directly, not by grepping `use crate::` paths, which is what produced the wrong graph in the first place (see Context).
   - **Refinement worth costing out at that point, given the consumer-usage evidence in Context**: once step 4's Document-API extraction lands, `codegen` (90,631 lines) has no production dependents left except `beamtalk-cli` and `beamtalk-repl`'s own codegen helper. A `beamtalk-codegen` crate separate from the rest of Compilation (`ast`/`source_analysis`/`semantic_analysis`/`unparse`/`compilation`) would let `beamtalk-lsp`/`beamtalk-mcp` depend on the analysis crate without `codegen` at all — a real build-time/type-checking-scope win for those two, unlike the rest of this split (see Consequences/Neutral). Whether this is worth doing alongside or separately from the Language-Service extraction is a sizing question for whoever executes this step, not decided here.

## Prior Art

Large Rust compilers commonly split into many crates for exactly the boundary-enforcement reason this ADR's Decision leans on, not primarily for parallel-build speed on a sequential pipeline:

- **rustc** is split into dozens of `rustc_*` crates (`rustc_ast`, `rustc_parse`, `rustc_hir`, `rustc_middle`, `rustc_codegen_ssa`, ...), largely along the same lexer→parser→HIR→typeck→codegen pipeline shape this crate has, with tools like `rustc_ast_pretty` occupying the same kind of position `unparse` does here.
- **rust-analyzer** splits its `syntax`/`parser`/`hir`/`hir-def`/`ide` layers into separate crates specifically so lower layers (syntax, parsing) can be reused and tested independently of the IDE-facing layers — the same shape as this crate's `language_service`/`queries` sitting atop the parser/analysis core.

Both are evidence that the *target* shape (once the cycles are fixed) is a well-trodden pattern for compiler workspaces this size — not evidence that skipping the cycle-breaking prerequisite is safe. Neither is a direct parallel for the "should we do this now" question this ADR answers, since both are `rustc`/`rust-analyzer`-scale projects with dedicated teams; the closer comparison for *when* to pay this cost is this repo's own `architecture-principles.md` §1, which already chose "document only" enforcement for the workspace's existing binary/library boundary on the grounds of solo-developer scale.

## User Impact

This is an internal Rust-workspace-organization decision, not a Beamtalk language-design one, so the usual newcomer/Smalltalk-developer/Erlang-developer/operator personas from other ADRs don't apply — nothing here is visible from the REPL or from Beamtalk source code. The relevant stakeholders instead:

- **Contributor navigating the codebase** — the CI dependency-direction check (step 1) is the first tangible win: any future edge that crosses Compilation/Language-Service in the wrong direction fails CI instead of silently rotting, as the original two undocumented cycles did. The `lint`/`project`/`repl` extraction (step 2) gives a small further win: `cargo doc`/`rust-analyzer` show a smaller, more specific dependency set for that work.
- **CI/build-time steward** — no meaningful wall-clock change from steps 1-4 alone (~2% of a crate that's still fundamentally a sequential pipeline). The eventual `beamtalk-compilation`/`beamtalk-language-service` split (step 5) is explicitly about boundary enforcement and touch-surface reduction, not build parallelism (see Consequences, Neutral).
- **Tooling developer** (LSP/MCP, both consumers of `beamtalk-core`'s public API) — no interface change from steps 1-4; step 5 would change import paths (`beamtalk_core::queries::...` → `beamtalk_language_service::...` or similar) and is exactly the kind of breaking change that should wait until the cross-context cycles are already fixed and validated independently, per the Decision's ordering. The `queries`/`language_service` merge (step 3, option a) is itself an internal rename only if both are already `beamtalk-core` — no external consumer currently imports `language_service` separately from `queries` to break.

## Steelman Analysis

### Alternative A (full split now) — the argument for it
A compiler maintainer optimizing for the *end state* would argue: doing the cycle-breaking and the split together is one round of review and one round of regression-testing instead of two, and the cross-context cycle fixes alone (Decision step 3) don't return any visible value until the split actually happens (step 5) — so why not finish the job? This is a real argument when the team has spare capacity and low risk-aversion; it's weaker here because `codegen` carries the ThreadedIr `verify()` invariants CLAUDE.md flags as the codebase's most safety-critical, best-tested area, and the step-4 hygiene fixes touch it directly — exactly where "two changes in one review" is the more expensive way to find a regression, not the cheaper one.

### Alternative B (no action) — the argument for it
An operator or a maintainer wary of process overhead would argue: `architecture-principles.md` §1 already decided "document only, code review sufficient" is enough enforcement for a much simpler boundary (binary crates vs. `beamtalk-core`) at solo-developer scale — extending that same judgment to internal modules within `beamtalk-core` is consistent, and the `lint`/`project`/`repl` extraction (Decision step 2) is arguably solving a problem no one has actually reported (compile times, navigability pain) rather than a problem this investigation set out to find. This is a fair challenge to step 2 specifically: it's a low-cost, low-risk change, but "low-cost" isn't the same as "requested" — worth the maintainer confirming compile-time/navigability pain is real before spending even the modest effort step 2 needs. It's a weaker challenge to step 1 (the CI check) and step 3 (the cross-context cycle fixes): both pay down a real, already-drifted architecture violation regardless of whether anyone ever asked for a crate split.

**Tension point:** the honest case for doing nothing at all (Alternative B) is stronger than the case for doing everything now (Alternative A) — this ADR's Decision sits deliberately between them, and a maintainer who agrees with B's skepticism should treat Decision step 2 (the `lint`/`project`/`repl` extraction) as optional rather than drop steps 1 and 3 (the CI check and the cross-context cycle fixes, which pay down a documented principle violation regardless of whether any split ever happens).

## Alternatives Considered

### A. Full split now, module-by-module, cycle-breaking included in the same effort
Break all cycles (originally scoped as four; corrected during review to include the two cross-context ones) and split into ~8 module-shaped crates (`beamtalk-ast`, `beamtalk-source-analysis`, `beamtalk-semantic-analysis`, `beamtalk-codegen`, `beamtalk-unparse`, `beamtalk-repl`, `beamtalk-compilation`, `beamtalk-language-service`, `beamtalk-queries`, `beamtalk-lint`) in one XL effort.

Rejected for now: bundles the highest-risk work (editing `codegen`'s Document API and `source_analysis`'s parser, both load-bearing and heavily tested) with the split itself, so a regression is hard to attribute to "the refactor" vs. "the split." Also rejected on its own terms once the graph was corrected: splitting one-module-per-crate ignores that most of these modules (`ast`/`source_analysis`/`unparse`/`codegen`/`semantic_analysis`/`compilation`) are one DDD bounded context (Compilation) with no reason to be separate crates — see Alternative D.

### B. No action at all
Leave `beamtalk-core` as one crate indefinitely; treat this investigation as answering the question without committing to any follow-up.

Rejected: the CI dependency-direction check and the `lint`/`project`/`repl` extraction (Decision steps 1-2) are low-risk, low-effort, and deliver a real, if modest, boundary win — there's no reason to leave those on the table just because the bigger split isn't worth doing yet. Breaking the two cross-context cycles (step 3) also independently pays down a documented architecture-principle violation (§6) regardless of whether a crate split ever happens. (See Steelman Analysis above for the strongest version of this alternative's case.)

### C. Merge `codegen` and `unparse` into one crate permanently, split everything else
Given how tightly `unparse` depends on `codegen`'s Document/leaf API, one option is to treat them as permanently one unit (a `beamtalk-codegen` crate covering both Core-Erlang generation and Beamtalk-source pretty-printing) and only extract the *other* three cycle edges.

Not rejected — subsumed by Alternative D below: under the bounded-context framing, `codegen` and `unparse` are both inside the single `beamtalk-compilation` crate anyway, so they end up merged by default without needing to special-case it.

### D. Split along DDD bounded contexts instead of Rust modules (adopted — see Decision)
Treat `{ast, source_analysis, unparse, codegen, semantic_analysis, compilation}` as one **Compilation** crate (matching CLAUDE.md's own DDD-context list) rather than trying to decompose it into ~7 module-shaped crates. Only split where a *context* boundary is crossed: `beamtalk-compilation` ← `beamtalk-language-service` (merged `queries`+`language_service`) ← nothing (LSP/MCP consume it), plus `beamtalk-repl` and the already-independent `lint`/`project`.

This was not in the original version of this ADR — it emerged from this review's discovery that the two cycles blocking a split (`queries⇄semantic_analysis`, `queries⇄language_service`) are both *cross-context* while the four cycles the original Decision focused on fixing are mostly *intra-context* (three of four are inside Compilation; the fourth, `queries⇄language_service`, is intra-Language-Service). Adopted because it directly explains which cycles are architecturally significant (cross-context) versus which are just internal cohesion (intra-context, fine to leave as-is), which Alternative A/C's module-by-module framing did not distinguish.

## Consequences

### Positive
- The CI dependency-direction check (step 1) is cheap and pays off immediately: it's exactly the mechanism that would have caught `queries`/`semantic_analysis` drifting into a cycle before this review had to find it by hand, and it's valuable whether or not any crate is ever actually split.
- Extracting `lint`/`project`/`repl` (step 2, ~5,600 lines, ~2%) gives a real, `cargo`-enforced boundary today, at low cost and low regression risk, with no cycle-breaking required.
- Breaking `queries⇄semantic_analysis` (step 3) is a small, well-precedented Shared-Leaf-Module fix (`synthetic_selectors.rs` is the template) that also resolves a §6 violation independent of any split.
- Framing the eventual split by DDD bounded context (Compilation / Language Service / REPL) rather than by Rust module means most of the crate's internal cycles (three of the original four: the Document API, the REPL-test move, the `source_analysis`/`unparse` test move) turn out not to need fixing at all for a split to be possible — they're intra-context cohesion, not cross-context coupling. That's a smaller, more honest scope than the original Decision's four-fixes-then-split plan.
- This investigation corrects the architecture-principles.md diagram's implicit claim that `beamtalk-core`'s internals are already a clean DAG — that assumption should not be relied on elsewhere until the cross-context cycles (step 3) are fixed.

### Negative
- The full split remains undone; if compile-time or navigability pain is the actual motivating problem, this decision doesn't solve it yet.
- The `queries⇄language_service` merge (step 3, option a) is a real design change, not a mechanical extraction — it needs a spike to inventory the full edge set (this review sampled it, not exhaustively) before it can be sized or scheduled with confidence.
- Splitting even `lint`/`project`/`repl` means widening whatever `pub(crate)` items they reach into across the new crate boundary — needs a visibility audit before the split, not assumed to be free. (Several of the crate's "shared leaf" modules are already `pub` rather than `pub(crate)` — see Context — so this may be smaller than it first looks.)

### Neutral
- Given the pipeline shape (`source_analysis` → `semantic_analysis` → `codegen`/`unparse` is fundamentally sequential even once cycle-free), a `beamtalk-compilation`/`beamtalk-language-service` split's main benefit is boundary enforcement and touch-surface reduction on unrelated changes — not parallel build speedup, since a linear dependency chain compiles serially with or without crate boundaries. This tempers how much the split (Decision step 5) is worth pursuing even after step 3 lands; that judgment call is deferred to whoever executes step 5, informed by real profiling at that time rather than assumed here.
- **This "no build-time benefit" framing holds for `beamtalk-cli` (needs the whole pipeline regardless) but not for `beamtalk-lsp`/`beamtalk-mcp`** — see the Context addition on actual consumer usage. Neither references `codegen` (90,631 lines, ~35% of the crate) or `repl` at all; they currently compile them anyway only because `unparse`'s Document-API dependency on `codegen` (Decision step 4's first item) drags it in transitively. If that extraction lands and `codegen`/`repl` end up in their own crate separate from whatever `beamtalk-lsp`/`beamtalk-mcp` actually depend on, that *is* a real, measurable win for those two — smaller compile units, smaller `rust-analyzer` project scope while working on tooling code. This is a concrete reason to prioritize that specific hygiene item over the other three in step 4, independent of whether the full bounded-context split (step 5) ever happens.

## Implementation

Phases mirror the Decision's five steps. 1-2 are independent and can land in either order; 3 doesn't require 1-2 but should follow the CI check (1) so the fix is guarded on landing; 4 is unordered hygiene; 5 requires 3 done first.

1. **CI dependency-direction check** (Decision step 1): script + CI job asserting the declared bounded-context boundaries (Compilation → Language Service allowed; reverse forbidden). Size: S. Do first.
2. **Extract `lint`, `project`, `repl`** (Decision step 2): move to 1-2 new crates atop `beamtalk-core`; fix up `Cargo.toml` edges; re-run full test suite. Size: S-M.
3. **Break the two cross-context cycles** (Decision step 3), each its own PR:
   - `queries⇄semantic_analysis`: extract `ANNOUNCE_SELECTORS`/`is_announce_selector` to a shared leaf module (pattern: `synthetic_selectors.rs`). Size: S.
   - `queries⇄language_service`: spike first to inventory the full edge set (this review sampled ~10 call sites, not exhaustive), then either merge the two modules (recommended) or extract shared protocol types to a leaf. Size: M, pending the spike.
4. **Intra-Compilation hygiene fixes** (Decision step 4), each its own PR, none split-blocking:
   - Extract `codegen::core_erlang::document` to a shared leaf module. Size: M (touches every `use crate::codegen::core_erlang::document::*` call site).
   - Move REPL-codegen tests from `codegen`'s test tree into `repl`'s. Size: S.
   - Extract `source_analysis::Span` (+ needed parser item) to a shared leaf module. Size: S-M.
   - Move `source_analysis`'s unparse-round-trip tests into `unparse`'s test tree. Size: S.
5. **Bounded-context split** (Decision step 5): once step 3 lands, re-run this ADR's dependency-graph extraction — verifying production vs. `#[cfg(test)]` for each edge, not grepping `use crate::` paths only, which is what produced the original wrong graph (see Context) — to confirm the cross-context cycles are actually gone. Then extract `beamtalk-compilation` (the former SCC, staying one crate) and `beamtalk-language-service` (merged `queries`+`language_service`), informed by real profiling data on whether the payoff is worth it at that point (see Consequences, Neutral).

None of this is scheduled against a Linear epic yet — file issues under a new epic if the maintainer wants to act on this ADR, parented separately from BT-3323 (which this is out of scope for).

## Implementation Tracking

**Epic:** BT-3338
**Issues:** BT-3339 (CI check), BT-3340 (lint/project/repl extraction), BT-3341 (queries⇄semantic_analysis fix, blocked by BT-3339), BT-3342 (queries⇄language_service fix, blocked by BT-3339 — done, merged `queries` into `language_service`), BT-3343 (Document API crate — done, extracted into `beamtalk-cerl-doc`), BT-3344 (REPL-codegen test move — done), BT-3345 (Span extraction), BT-3346 (unparse-round-trip test move)
**Status:** In progress. BT-3343, BT-3341, BT-3344, and BT-3342 have landed; the rest are still Planned. Decision step 5 (the full bounded-context crate split) is explicitly out of scope for this epic — revisit once the ADR's dependency-graph extraction is re-run to confirm the cross-context cycles are gone.

## References
- Related issues: none yet — this ADR did not originate from a Linear issue; came up during BT-3323 (Rust coverage epic)
- Related ADRs: ADR 0089 (Typed Document Leaves) — the `Document`/`leaf`/`docvec!` API whose placement inside `codegen` is the root of the biggest cycle edge found here
- Documentation: `docs/development/architecture-principles.md` §1 (Layered Architecture & Dependency Rules), §6 (Duplication & the Shared-Leaf-Module Pattern)
