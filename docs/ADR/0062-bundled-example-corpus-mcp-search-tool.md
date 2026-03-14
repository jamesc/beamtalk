# ADR 0062: Bundled Example Corpus and Search Tool for MCP Server

## Status
Proposed (2026-03-14)

## Context

### The Problem

Agents working via the Beamtalk MCP server have no way to verify Beamtalk syntax before writing `.bt` code. The CLAUDE.md rule — "grep for 3+ existing codebase examples of the pattern" — only works when the agent has repo access. An external agent connected over MCP stdio has no repo, no test files, and no examples. This leads to hallucinated syntax and incorrect patterns.

The MCP server already exposes `evaluate`, `complete`, `docs`, `lint`, and other tools. An agent can evaluate code and get completions, but it cannot ask "show me how closures work in Beamtalk" or "what's the syntax for pattern matching?". The gap is between knowing-what (API docs via the `docs` tool) and knowing-how (working examples showing idiomatic patterns).

### What We Already Have

Beamtalk has rich content that could serve as an example corpus:

| Source | Count | Description |
|--------|-------|-------------|
| `stdlib/test/fixtures/*.bt` | ~97 files | Complete class definitions — actors, value classes, NLR, coordination, error handling |
| `tests/e2e/fixtures/*.bt` | ~52 files | Real-world patterns — bank accounts, chat rooms, supervision, hot reload |
| `docs/learning/fixtures/*.bt` | ~50 files | Pedagogical examples — typed classes, destructuring, FFI, supervision |
| `stdlib/test/*.bt` | ~151 files | BUnit test cases exercising the fixtures |
| `docs/learning/*.md` | 27 files | Structured learning modules with prose explanations |
| `examples/**/*.bt` | 6 projects | Getting-started, bank, chat-room, GoF patterns, OTP tree, SICP |
| `docs/beamtalk-language-features.md` | 1 file | Curated language reference |
| `crates/beamtalk-core/src/unparse/fixtures/*.bt` | 7 files | Parser round-trip fixtures showing valid syntax patterns |
| `stdlib/bootstrap-test/*.bt` | ~20 files | Bootstrap primitive tests (too low-level for agents) |

The test fixtures are the most valuable source — they are complete, self-contained, tested class definitions that directly answer "how do I write X in Beamtalk?". For example, `stdlib/test/fixtures/counter.bt` is a 15-line actor with state, mutation, and accessors; `tests/e2e/fixtures/bank_account.bt` shows error handling, early returns, and string interpolation in a realistic domain.

These are already maintained, tested, and current. The problem is making them accessible to agents that don't have the repo.

### Prior Art: Production MCP Servers

Two production MCP servers already implement documentation-search patterns:

**Context7** (Upstash) — indexes 9,000+ libraries into a searchable corpus. Two-tool pattern: `resolve-library-id` resolves a library name to an ID, then `query-docs` searches that library's docs. The indirection handles multi-library disambiguation but adds a round-trip. For a single-language server like ours, the indirection is unnecessary overhead.

**Atlassian Forge MCP** — bundles how-to guides and code snippets as named MCP tools (`search-forge-docs`, `forge-ui-kit-developer-guide`). Each guide is a curated, pre-chunked document. Works well for a fixed set of topics but doesn't scale to ad-hoc queries.

### Constraints

- The MCP server ships as a single binary via `cargo-dist`. Sidecar files add packaging complexity.
- The corpus must work offline — no network calls at query time.
- Agent context windows are limited. Results must be concise (bounded token count).
- The corpus will drift from source files unless there's a regeneration mechanism.

## Decision

### Single `search_examples` Tool

Add one MCP tool to the `beamtalk-mcp` server:

```text
search_examples(query: string, limit?: integer) -> CallToolResult
```

**Parameters:**

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `query` | string | yes | — | Natural-language or keyword query (e.g. "closures", "pattern matching actors") |
| `limit` | integer | no | 5 | Maximum number of results to return (capped at 20) |

**Returns:** A list of matching examples, each containing:

```json
{
  "id": "collections-array-do",
  "title": "Iterating an Array with do:",
  "category": "collections",
  "tags": ["Array", "do:", "closures", "iteration"],
  "source": "// Array iteration\nfruits := #('apple' 'banana' 'cherry').\nfruits do: [:fruit | Transcript show: fruit].",
  "explanation": "The do: message sends a block to each element. The block receives one argument — the current element."
}
```

This is a single-tool pattern (not Context7's two-tool pattern) because we have exactly one corpus for one language. No disambiguation step needed.

### Corpus Format and Storage

The corpus is a JSON file checked into the repo at `crates/beamtalk-examples/corpus.json` and embedded at compile time via `include_bytes!`. It is deserialized once at startup using `std::sync::LazyLock`.

**Why checked-in JSON (not `build.rs`, not bincode):**

- `serde_json` is already a dependency — no new crates
- A checked-in file is reviewable in PRs and diffable
- CI can verify freshness: `just build-corpus && git diff --exit-code crates/beamtalk-examples/corpus.json`
- No `build.rs` latency on every `cargo build`
- JSON is human-readable for manual inspection and editing

### Corpus Entry Schema

```rust
#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct CorpusEntry {
    /// Unique identifier (e.g. "closures-value-capture").
    pub id: String,
    /// Human-readable title.
    pub title: String,
    /// Top-level category for grouping.
    pub category: String,
    /// Searchable tags — class names, selector names, concepts.
    pub tags: Vec<String>,
    /// Beamtalk source code (the example itself).
    pub source: String,
    /// Brief explanation of what the example demonstrates.
    pub explanation: String,
}
```

### Content Sources and Chunking Strategy

Not all source content is equally useful as a corpus entry. The strategy is curated extraction, not bulk ingestion. **Test fixtures are the primary source** — they are complete, self-contained class definitions that directly answer "how do I write X?".

| Source | Strategy | Granularity | Value |
|--------|----------|-------------|-------|
| `stdlib/test/fixtures/*.bt` | Include as whole-file examples | One entry per fixture file | **High** — complete class defs: actors, value classes, NLR, coordination, HOM |
| `tests/e2e/fixtures/*.bt` | Include as whole-file examples | One entry per fixture file | **High** — real-world patterns: bank accounts, chat rooms, supervision, hot reload |
| `docs/learning/fixtures/*.bt` | Include as whole-file examples | One entry per fixture file | **High** — pedagogical, tied to learning modules, typed classes, destructuring, FFI |
| `stdlib/test/*.bt` | Extract individual test methods | One entry per test method (with class context) | **Medium** — shows how to *use* patterns, but test scaffolding adds noise |
| `examples/**/*.bt` | Extract key patterns from each project | One entry per notable pattern | **Medium** — multi-file projects, harder to chunk into standalone entries |
| `docs/learning/*.md` | Extract code blocks with surrounding explanation | One entry per code block with prose | **Medium** — explanatory context, but code is often duplicated from fixtures |
| `docs/beamtalk-language-features.md` | Extract each feature section's examples | One entry per language feature | **Medium** — reference-style, terse |
| `crates/beamtalk-core/src/unparse/fixtures/*.bt` | Include selectively | One entry per file | **Low** — compiler internals, but shows valid syntax patterns |
| `stdlib/bootstrap-test/*.bt` | Skip | — | **Low** — too low-level for agent consumption |

**Estimated corpus size:** ~300-400 entries, ~1-1.5MB as JSON.

### Search Mechanism

Keyword-based scoring with weighted fields. No external dependencies.

```text
score = (title_matches * 10) + (tag_matches * 8) + (category_matches * 5)
      + (explanation_matches * 2) + (source_matches * 1)
```

The query is tokenized into keywords (split on whitespace, lowercased). Each keyword is matched against each field. Results are sorted by score descending and truncated to `limit`.

**Why not semantic/vector search:** The corpus is small (~300 entries) and domain-specific. Agents are prompted with CLAUDE.md which names Beamtalk constructs explicitly, so queries are keyword-rich by design. Adding a vector search model (even a small ONNX one) would add ~30MB to the binary and a new dependency category. If keyword search proves insufficient in practice, semantic search can be added later without changing the tool interface.

### Corpus Generation

A `just build-corpus` task runs a binary crate that:

1. Walks the content source directories
2. Parses `.bt` files using `beamtalk-core` to extract class names, selectors, and method boundaries
3. Parses `.md` files to extract fenced code blocks with surrounding context
4. Writes `crates/beamtalk-examples/corpus.json`

The generator lives at `crates/beamtalk-examples/build-corpus/` as a small binary crate. It depends on `beamtalk-core` for parsing `.bt` files — this is the key reason the generator is a crate rather than a standalone script. A script cannot access workspace dependencies, so it would have to use fragile regex heuristics instead of the real parser for method boundary detection.

**Freshness check in CI:** `just ci` runs `just build-corpus` and asserts no diff. This catches corpus drift when test files or examples change.

### Crate Structure

A new `beamtalk-examples` library crate owns the corpus types, storage, and search logic. The corpus generator is a sub-binary crate within it.

```text
crates/beamtalk-examples/
├── Cargo.toml           # lib crate — depends on serde, serde_json
├── corpus.json          # checked-in generated corpus
├── src/
│   ├── lib.rs           # re-exports
│   ├── corpus.rs        # CorpusEntry, Corpus, LazyLock deserialization
│   └── search.rs        # weighted keyword search scoring
└── build-corpus/
    ├── Cargo.toml       # binary crate — depends on beamtalk-examples + beamtalk-core
    └── src/
        └── main.rs      # corpus generator: parses .bt/.md files, writes corpus.json
```

**Why a separate crate (not modules in `beamtalk-mcp`):**

The corpus generator needs two dependencies: `beamtalk-core` (to parse `.bt` files) and the `CorpusEntry` type (to serialize the corpus). If `CorpusEntry` lives in `beamtalk-mcp`, the generator would depend on the entire MCP server — REPL client, MCP transport, etc. — just for one struct. A shared `beamtalk-examples` crate gives a clean dependency graph:

```text
beamtalk-mcp        → beamtalk-examples  (search at runtime)
beamtalk-examples   → serde, serde_json  (no other deps)
build-corpus        → beamtalk-examples + beamtalk-core  (generate corpus)
```

No circular dependencies, no pulling MCP machinery into the generator. And if the LSP later wants example lookups, it depends on `beamtalk-examples` directly — no extraction refactor needed.

The MCP server's `server.rs` adds a thin `search_examples` tool handler that delegates to `beamtalk_examples::search()`.

### Tool Registration

The `search_examples` tool is registered alongside existing tools in the `#[tool_router]` impl block. Unlike `evaluate` or `complete`, it does **not** use the REPL client — it queries the embedded corpus directly. This means it works even when the REPL is disconnected.

```rust
#[derive(Debug, serde::Deserialize, schemars::JsonSchema)]
pub struct SearchExamplesParams {
    /// Search query — keywords or natural language describing what you're looking for.
    #[schemars(description = "Keywords or natural language query (e.g. 'closures', 'actor state', 'pattern matching')")]
    pub query: String,
    /// Maximum number of results (default 5, max 20).
    #[schemars(description = "Maximum results to return. Default 5, max 20.")]
    pub limit: Option<usize>,
}
```

### Search Telemetry

The claim that keyword search with synonym tags is sufficient for ~300 entries needs to be falsifiable. Without observability, search quality degrades silently — we won't know that agents are searching for "for loop" and getting zero results until someone manually investigates.

**What to log (structured, to the MCP server's log output):**

| Field | Purpose |
|---|---|
| `query` | The raw query string |
| `result_count` | Number of results returned |
| `top_score` | Score of the highest-ranked result (0 = no matches) |
| `duration_us` | Search latency in microseconds |

This is local-only logging via `tracing` (already a dependency of `beamtalk-mcp`), not a phone-home service. The MCP server runs on the developer's machine; logs go to stderr or a log file.

**What this enables:**

- **Zero-result queries** reveal vocabulary gaps — queries that agents ask but the corpus can't answer. These directly inform synonym tag additions (Phase 3 curation).
- **Low-score queries** reveal weak matches — the corpus has something relevant but the scoring didn't surface it well. These inform weight tuning.
- **Query distribution** reveals which topics agents ask about most, guiding corpus expansion priorities.
- **Duration tracking** establishes a baseline for deciding whether search performance matters (it probably doesn't at ~300 entries, but the data confirms it).

**Eval workflow:** Periodically review logs (or pipe to a script) to extract zero-result and low-score queries. Each one becomes either a synonym tag addition, a new corpus entry, or evidence that keyword search is hitting its ceiling and semantic search should be reconsidered.

A `just search-eval` task can parse structured logs and produce a summary report — top failing queries, score distribution, result count histogram.

### Versioning and Staleness

- The corpus is regenerated by `just build-corpus` and checked in
- CI enforces that the checked-in corpus matches what the generator produces
- The corpus carries no version field — it's as current as the last `just build-corpus` run
- The embedded binary ships whatever corpus was checked in at build time

## Prior Art

| System | Corpus Source | Search | Bundled? | Tool Count |
|--------|-------------|--------|----------|------------|
| **Context7** | 9,000+ libraries from npm/PyPI/docs sites | Keyword + embeddings | No (cloud service) | 2 (`resolve-library-id`, `query-docs`) |
| **Forge MCP** | Curated Atlassian guides | Named guide lookup | Yes (in server) | Multiple named tools |
| **Pharo** | Class/method comments in the image | `Finder` tool, string/example search | Yes (in image) | N/A (not MCP) |
| **Elixir HexDocs** | Package documentation | Full-text search | No (web service) | N/A |
| **This ADR** | Test files, examples, learning docs | Weighted keyword scoring | Yes (`include_bytes!`) | 1 (`search_examples`) |

**What we adopt:**
- **Context7's corpus-and-search pattern** — a searchable index of examples, not just API docs. Context7 proved that agents benefit enormously from example code alongside documentation.
- **Forge MCP's bundled approach** — embedding content in the server binary for zero-config, offline operation. No cloud service, no API keys, no network dependency.

**What we adapt:**
- **Context7's two-tool pattern → single tool.** Context7 needs `resolve-library-id` because it indexes 9,000+ libraries and must disambiguate. We have one language with one corpus. The indirection adds latency and complexity for no benefit.
- **Forge MCP's named guides → keyword search.** Forge uses fixed tool names per guide (`forge-ui-kit-developer-guide`). This doesn't scale to ad-hoc queries. We use a single searchable tool with keyword matching instead.

**What we reject:**
- **Pharo's live Finder** — searches the running image for examples by evaluating expressions. Requires a live system with all classes loaded. Our corpus must work offline, without a REPL connection.
- **Full semantic search** — vector embeddings, ONNX models, etc. Overkill for ~300 entries in a single-language domain where queries are keyword-rich.

## User Impact

### Agent Developer (primary consumer)
- **Before:** Agent must hallucinate Beamtalk syntax or ask the user for examples. No programmatic way to look up "how do I iterate an array?"
- **After:** `search_examples("array iteration")` returns working, tested code. The agent can verify patterns before generating `.bt` files.
- **Caveat:** Keyword search requires knowing approximate terminology. An agent asking "for loop" won't find `do:` unless tags include "loop" as a synonym. Tag quality is critical.

### Language Developer (corpus maintainer)
- **New workflow:** When adding tests or examples, run `just build-corpus` to regenerate the corpus. CI catches forgotten regeneration.
- **Caveat:** Manual curation of tags and explanations adds maintenance burden. Mitigated by generating reasonable defaults from test method names and file paths.

### Production Operator
- **Binary size increase:** ~1MB for the embedded corpus. The `beamtalk-mcp` binary grows from ~20MB to ~21MB.
- **Memory:** Corpus is deserialized once into ~1MB of heap. Negligible for a dev tool.
- **No runtime impact on existing tools:** `search_examples` is independent of the REPL connection.

### Newcomer
- **Discoverability:** An agent using Beamtalk MCP can now self-serve examples. "Show me how closures work" returns working code, not a hallucination.

## Steelman Analysis

### Alternative: Separate file instead of embedded binary

| Cohort | Strongest argument |
|---|---|
| **Operator** | "A 1-3MB corpus baked into the binary inflates every deploy, even when no agent ever calls `search_examples`. Ship it as `corpus.json` next to the binary — users who don't use MCP agents pay zero cost. Also lets users swap in a custom corpus without recompiling." |
| **Language dev** | "Separate file means I can regenerate the corpus without a full `cargo build`. Run a script, get a new `corpus.json`, restart the server. Faster iteration on taxonomy and chunking." |

**Counter:** Distribution complexity. `cargo-dist` ships a single binary today. A sidecar file means packaging changes, install instructions, and a runtime "file not found" failure mode. The 1MB cost is negligible for a dev tool binary that's already ~20MB.

### Alternative: Live REPL search (no static corpus)

| Cohort | Strongest argument |
|---|---|
| **Smalltalk purist** | "The system should be self-describing. If an agent wants examples, it should ask the running system — `ExamplesFinder search: 'blocks'`. This is live, always current, and the corpus is exactly what's loaded. You're building a dead snapshot of content that drifts from the real system. The freshness problem you're solving with CI checks simply doesn't exist with a live approach." |
| **Pragmatist** | "The MCP server already has a running REPL connection. Once Beamtalk has modules, you emit an `ExamplesFinder` module as part of the dev-mode stdlib — opt-in for development, stripped from production builds. No Rust crate, no JSON file, no build step. The module hot-reloads with the stdlib, so the corpus is always current. The bloat argument doesn't apply — production binaries never see it." |

**Counter:** The dev-only module approach is viable and addresses the bloat concern. The remaining arguments against it are operational:

1. **REPL coupling.** A REPL-based approach ties search availability to REPL connectivity. The static corpus works during startup, reconnection, and error states — exactly when an agent most needs to look up examples (e.g., diagnosing why a REPL connection failed). The MCP server's `search_examples` tool works even with `"repl_connected": false`.
2. **Search evolution.** Keyword search in Erlang is straightforward, but if we later want synonym expansion, fuzzy matching, or (eventually) semantic search, maintaining that in Erlang means a parallel implementation. The Rust-side approach keeps search logic in one place, co-located with the MCP server that consumes it.
3. **Module system dependency.** This approach blocks on Beamtalk's module system, which doesn't exist yet. The static corpus can ship today.

Of these, (3) is the strongest near-term argument but weakest long-term. Once modules land, this alternative becomes more compelling — especially if the corpus proves hard to keep fresh via CI checks.

### Alternative: Semantic/vector search

| Cohort | Strongest argument |
|---|---|
| **AI tooling advocate** | "Agents don't always use the right keywords. 'How do I loop over elements' should find `do:` and `collect:`, but keyword search requires the agent to already know those names. Semantic search closes that vocabulary gap. The corpus is small enough that a lightweight model (e.g., ONNX MiniLM) adds ~30MB, not 200MB. You're building a search tool for AI agents — the one user who would most benefit from semantic understanding — and giving them grep." |

**Counter:** The vocabulary gap is real but addressable without a model. Synonym tags (e.g., "loop" → `do:`, "lambda" → blocks, "for each" → `do:`) close the most common mismatches at negligible cost. The corpus is ~300 entries — small enough that well-chosen tags cover the search space. Adding an ONNX model introduces a new dependency category (native ML runtime), complicates cross-compilation, and increases binary size by 30MB+ for a marginal improvement over tags. Crucially, structured telemetry on every search call (query, result_count, top_score) makes this a data-driven decision: if zero-result queries exceed ~15% or cluster around vocabulary mismatches that tags can't cover, that's concrete evidence to invest in semantic search. The tool interface (`search_examples(query, limit)`) is deliberately stable — the backend can be swapped without changing the agent integration.

### Alternative: Context7-style two-tool pattern

| Cohort | Strongest argument |
|---|---|
| **MCP protocol purist** | "A `list_categories` or `browse_examples(category)` tool lets agents discover the corpus structure before searching. Without it, the agent is shooting blind — it doesn't know whether to search 'actors', 'processes', 'concurrency', or 'message passing' for the same concept. Context7's resolve step isn't just disambiguation — it's discoverability." |
| **Agent developer** | "Two tools let agents build a mental model of the corpus. First call lists topics, second call dives into one. With a single search tool, the agent has no way to know what topics exist. It can't ask 'what categories of examples do you have?' — it can only guess keywords." |

**Counter:** The discoverability argument is real but doesn't justify a second tool. A single `search_examples` call with a broad query (e.g., "concurrency") already returns results with `category` fields that reveal the corpus structure. We could also add a `categories` field to the tool's schema description listing available categories. The two-tool pattern doubles the integration surface (agents must learn two tools, handle the round-trip, deal with empty resolve results) for a discoverability benefit achievable within one tool.

### Alternative: LSP bundling instead of MCP

| Cohort | Strongest argument |
|---|---|
| **IDE developer** | "The LSP already runs in every editor session. If the corpus lives there, VSCode completions and hover docs can show examples inline — not just MCP agents. One corpus, two consumers." |

**Counter:** The LSP has repo access (it runs in the workspace). It can already grep test files. The corpus solves a problem specific to MCP agents that lack repo access. Bundling into the LSP adds binary bloat for a capability the LSP doesn't need. That said, the `beamtalk-examples` crate architecture makes LSP integration trivial if desired later — just add the dependency.

### Tension Points

- **Freshness vs simplicity:** A checked-in JSON file is simple but can drift. A `build.rs` generator is always fresh but adds build complexity. CI freshness checks are the compromise.
- **Curation vs automation:** Hand-curated tags and explanations produce better search results but don't scale. Auto-generated entries from test method names are lower quality but zero-maintenance. The recommendation is auto-generate with manual refinement.
- **Crate boundary:** A separate `beamtalk-examples` crate adds workspace complexity but is justified by the generator's dependency needs (it needs `beamtalk-core` + `CorpusEntry` without pulling in MCP server deps).

## Alternatives Considered

### Alternative A: Context7-Style Two-Tool Pattern

Expose `resolve_topic(query) -> topic_id` and `get_examples(topic_id, limit) -> examples` as separate tools.

**Rejected because:** We have one language, one corpus. The indirection adds a mandatory round-trip that wastes agent turns. Context7 needs disambiguation across 9,000+ libraries; we don't. A single `search_examples` tool serves the same purpose with less friction.

### Alternative B: Forge-Style Named Guide Tools

Expose each content category as a separate named tool: `closures_guide`, `collections_guide`, `actors_guide`, etc.

**Rejected because:** Fixed tool names don't support ad-hoc queries. An agent looking for "how to send a message to an actor" has to guess which guide name matches. Adding new topics requires adding new tools (code changes, not just corpus updates). Search is strictly more flexible.

### Alternative C: `build.rs` Corpus Generation

Generate the corpus at compile time via `build.rs` in `beamtalk-mcp`, parsing test files and emitting a serialized corpus to `OUT_DIR`.

**Rejected because:** Runs on every `cargo build`, adding latency to every compile cycle. Creates a build-time dependency on `beamtalk-core`'s parser (already a dependency, but using it in `build.rs` is a different compilation unit). The corpus doesn't change on every build — it changes when content source files change. A manual `just build-corpus` with CI freshness checks is more appropriate.

### Alternative D: No Corpus — Enhance Existing `docs` Tool

Extend the existing `docs` MCP tool to return examples alongside API documentation, pulling from `///` doc comments that contain code blocks.

**Rejected because:** `///` doc comments are API-level documentation, not tutorial-style examples. They document what a method does, not how to use the language feature it exemplifies. The gap is "how do I use closures?" not "what does `Array >> #do:` return?". Different content, different tool.

### Alternative E: Keep Everything in `beamtalk-mcp` (No Separate Crate)

Keep `CorpusEntry`, `Corpus`, and search logic as modules inside `beamtalk-mcp` rather than creating a separate `beamtalk-examples` crate. Only one consumer exists today.

**Rejected because:** The corpus generator binary needs both `beamtalk-core` (to parse `.bt` files) and the `CorpusEntry` type (to serialize the corpus). If `CorpusEntry` lives in `beamtalk-mcp`, the generator depends on the entire MCP server — REPL client, MCP transport, etc. — just for one struct. A separate `beamtalk-examples` crate with minimal dependencies (`serde`, `serde_json`) gives a clean dependency graph and avoids pulling MCP machinery into the generator. The separate crate is justified by the generator's needs today, not hypothetical future consumers.

## Consequences

### Positive
- **Agents can verify syntax before writing code** — the primary goal. `search_examples("closures")` returns tested, working Beamtalk code.
- **Zero-config, offline operation** — no API keys, no network, no sidecar files. The corpus ships inside the binary.
- **No new dependencies** — uses `serde_json` (already present) for deserialization, no vector DB or embedding model.
- **Stable tool interface** — the `search_examples(query, limit)` signature supports upgrading the search backend (keyword → semantic) without changing the agent integration.
- **Data-driven search evolution** — structured telemetry on every search call makes keyword search quality falsifiable. Zero-result and low-score queries directly inform tag curation and provide evidence for or against upgrading to semantic search.
- **Content reuse** — leverages existing test files and learning docs rather than creating new content.

### Negative
- **Maintenance burden** — the corpus must be regenerated when content sources change. CI freshness checks catch drift but `just build-corpus` is a manual step.
- **Keyword search limitations** — vocabulary mismatch (e.g., "for loop" vs `do:`) requires careful tagging. Quality depends on tag curation.
- **Binary size increase** — ~1MB added to the `beamtalk-mcp` binary.
- **Staleness risk** — the shipped corpus is as current as the last `just build-corpus` run. Agents on older binary versions get older examples.

### Neutral
- **No impact on existing MCP tools** — `search_examples` is additive. All existing tools (`evaluate`, `docs`, `complete`, etc.) are unchanged.
- **No REPL dependency** — unlike most MCP tools, `search_examples` works without a REPL connection.
- **Corpus format is internal** — the JSON schema is a build artifact, not a public API. It can change freely between versions.

## Implementation

### Phase 1: `beamtalk-examples` Crate and Corpus Generator (S)

**Affected components:** New `beamtalk-examples` crate, build tooling

1. Create `crates/beamtalk-examples/` library crate with `CorpusEntry`, `Corpus`, `LazyLock` deserialization, and `search.rs`
2. Create `crates/beamtalk-examples/build-corpus/` binary crate (depends on `beamtalk-examples` + `beamtalk-core`)
3. Implement extractors for each content source:
   - `.bt` fixture files (`stdlib/test/fixtures/`, `tests/e2e/fixtures/`, `docs/learning/fixtures/`): include as whole-file entries with auto-generated tags from class names, selectors, and file path
   - `.bt` test files (`stdlib/test/*.bt`): extract individual test methods with class context
   - `.bt` example files (`examples/**/*.bt`): extract notable patterns
   - `.md` learning modules: extract fenced code blocks with surrounding prose
   - `beamtalk-language-features.md`: extract per-feature examples
4. Generate `crates/beamtalk-examples/corpus.json`
5. Add `just build-corpus` task
6. Add CI freshness check to `just ci`
7. Add unit tests for search scoring and edge cases (empty query, no matches, limit capping)

### Phase 2: MCP Tool Integration (S)

**Affected components:** `beamtalk-mcp`

1. Add `beamtalk-examples` as a dependency of `beamtalk-mcp`
2. Add `SearchExamplesParams` and `search_examples` tool handler in `server.rs` — delegates to `beamtalk_examples::search()`
3. Add structured `tracing` telemetry — log query, result_count, top_score, duration_us on every search call
4. Update server instructions text to mention the new tool

### Phase 3: Corpus Curation (M)

**Affected components:** Content

1. Review auto-generated corpus entries for quality
2. Add synonym tags for common vocabulary mismatches (e.g., "loop" → `do:`, "lambda" → blocks)
3. Write explanations for entries that lack sufficient context
4. Validate that representative queries return useful results

### Phase 4: Search Eval Tooling (S)

**Affected components:** Build tooling

1. Add `just search-eval` task that parses structured logs and produces a summary: zero-result queries, low-score queries, score distribution, query frequency
2. Use eval results to drive synonym tag additions and corpus expansion
3. Establish a baseline for search quality — if zero-result rate exceeds a threshold (e.g., >15% of queries), escalate to semantic search evaluation

## References
- Related issues: BT-1347
- Related ADRs: [ADR 0033](0033-runtime-embedded-documentation.md) (Runtime-embedded documentation — prior art for embedding content in the runtime)
- Context7: https://github.com/upstash/context7
- Forge MCP: https://developer.atlassian.com/platform/forge/forge-mcp/
- MCP specification: https://modelcontextprotocol.io/specification/2025-06-18/server/resources
- Existing MCP server: `crates/beamtalk-mcp/src/server.rs`
