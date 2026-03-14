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

```
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

The corpus is a JSON file checked into the repo at `crates/beamtalk-mcp/corpus.json` and embedded at compile time via `include_bytes!`. It is deserialized once at startup using `std::sync::LazyLock`.

**Why checked-in JSON (not `build.rs`, not bincode):**

- `serde_json` is already a dependency — no new crates
- A checked-in file is reviewable in PRs and diffable
- CI can verify freshness: `just build-corpus && git diff --exit-code`
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

```
score = (title_matches * 10) + (tag_matches * 8) + (category_matches * 5)
      + (explanation_matches * 2) + (source_matches * 1)
```

The query is tokenized into keywords (split on whitespace, lowercased). Each keyword is matched against each field. Results are sorted by score descending and truncated to `limit`.

**Why not semantic/vector search:** The corpus is small (~300 entries) and domain-specific. Agents are prompted with CLAUDE.md which names Beamtalk constructs explicitly, so queries are keyword-rich by design. Adding a vector search model (even a small ONNX one) would add ~30MB to the binary and a new dependency category. If keyword search proves insufficient in practice, semantic search can be added later without changing the tool interface.

### Corpus Generation

A `just build-corpus` task runs a Rust script that:

1. Walks the content source directories
2. Parses `.bt` files to extract test methods and example patterns
3. Parses `.md` files to extract fenced code blocks with surrounding context
4. Writes `crates/beamtalk-mcp/corpus.json`

The script lives at `crates/beamtalk-mcp/build-corpus/` as a small binary crate (or a script in `scripts/`). It uses `beamtalk-core` for parsing `.bt` files to extract method boundaries.

**Freshness check in CI:** `just ci` runs `just build-corpus` and asserts no diff. This catches corpus drift when test files or examples change.

### Module Structure

New code lives in `crates/beamtalk-mcp/src/`:

```
crates/beamtalk-mcp/src/
├── main.rs          # existing
├── server.rs        # existing — add search_examples tool handler
├── client.rs        # existing
├── workspace.rs     # existing
├── corpus.rs        # NEW — CorpusEntry, Corpus, deserialization, LazyLock
└── search.rs        # NEW — keyword search scoring
```

No new crate. The MCP server is the only consumer. If the LSP later needs corpus access, extraction to a `beamtalk-examples` library crate is a mechanical refactor.

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
| **Smalltalk purist** | "The system should be self-describing. If an agent wants examples, it should ask the running system — `ExampleFinder search: 'blocks'`. This is live, always current, and the corpus is exactly what's loaded. You're building a dead snapshot of content that drifts from the real system." |
| **Pragmatist** | "The MCP server already has a running REPL connection. You don't need a build step, a corpus format, or search code. Just add a REPL op that greps loaded test sources. Ten lines of Erlang." |

**Counter:** The whole point is helping agents that don't have the repo. An external agent using the MCP server over stdio has no test files loaded. The REPL approach only works for agents with full repo access — and those agents can already grep.

### Alternative: Semantic/vector search

| Cohort | Strongest argument |
|---|---|
| **AI tooling advocate** | "Agents don't always use the right keywords. 'How do I loop over elements' should find `do:` and `collect:`, but keyword search requires the agent to already know those names. Semantic search closes that vocabulary gap. The corpus is small enough that a lightweight model (e.g., ONNX MiniLM) adds ~30MB, not 200MB." |

**Counter:** Beamtalk agents are prompted with CLAUDE.md which names the constructs. Queries are keyword-rich by design. The vocabulary gap is small for a single-language technical corpus. If keyword search proves insufficient in practice, semantic search can be added later without changing the tool interface.

### Alternative: Context7-style two-tool pattern

| Cohort | Strongest argument |
|---|---|
| **MCP protocol purist** | "Browsable topic resolution is worth the extra round-trip. `resolve-library-id` lets the agent discover what's available before searching. A flat keyword search gives no affordance for browsing the corpus structure." |

**Counter:** We have one corpus for one language. The "browsing" affordance can be achieved by querying with an empty or generic term. Adding a second tool doubles the integration surface for agents with no functional benefit.

### Alternative: LSP bundling instead of MCP

| Cohort | Strongest argument |
|---|---|
| **IDE developer** | "The LSP already runs in every editor session. If the corpus lives there, VSCode completions and hover docs can show examples inline — not just MCP agents. One corpus, two consumers." |

**Counter:** The LSP has repo access (it runs in the workspace). It can already grep test files. The corpus solves a problem specific to MCP agents that lack repo access. Bundling into the LSP adds binary bloat for a capability the LSP doesn't need. If we later want LSP example lookups, it can call the same corpus module as a library dependency — but that's a separate decision.

### Tension Points

- **Freshness vs simplicity:** A checked-in JSON file is simple but can drift. A `build.rs` generator is always fresh but adds build complexity. CI freshness checks are the compromise.
- **Curation vs automation:** Hand-curated tags and explanations produce better search results but don't scale. Auto-generated entries from test method names are lower quality but zero-maintenance. The recommendation is auto-generate with manual refinement.
- **Single crate vs shared library:** Keeping corpus code in `beamtalk-mcp` is simpler now but means extraction later if the LSP wants it. YAGNI wins for v0.1.

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

### Alternative E: New `beamtalk-examples` Library Crate

Extract corpus storage and search into a shared library crate that both `beamtalk-mcp` and `beamtalk-lsp` can depend on.

**Rejected for now:** Only one consumer exists today. Creating a crate for a single consumer contradicts the "don't design for hypothetical future requirements" principle. If the LSP needs it later, extraction is a mechanical refactor — move `corpus.rs` and `search.rs`, add `Cargo.toml`, update imports.

## Consequences

### Positive
- **Agents can verify syntax before writing code** — the primary goal. `search_examples("closures")` returns tested, working Beamtalk code.
- **Zero-config, offline operation** — no API keys, no network, no sidecar files. The corpus ships inside the binary.
- **No new dependencies** — uses `serde_json` (already present) for deserialization, no vector DB or embedding model.
- **Stable tool interface** — the `search_examples(query, limit)` signature supports upgrading the search backend (keyword → semantic) without changing the agent integration.
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

### Phase 1: Corpus Generator (S)

**Affected components:** Build tooling

1. Create `scripts/build-corpus.rs` (or a small crate at `crates/beamtalk-mcp/build-corpus/`)
2. Implement parsers for each content source:
   - `.bt` test files: extract test methods with class context
   - `.bt` example files: extract notable patterns
   - `.md` learning modules: extract fenced code blocks with prose
   - `beamtalk-language-features.md`: extract per-feature examples
3. Generate `crates/beamtalk-mcp/corpus.json`
4. Add `just build-corpus` task
5. Add CI freshness check to `just ci`

### Phase 2: Search and MCP Tool (S)

**Affected components:** `beamtalk-mcp`

1. Add `corpus.rs` — `CorpusEntry`, `Corpus` structs, `LazyLock` deserialization from `include_bytes!`
2. Add `search.rs` — weighted keyword search
3. Add `SearchExamplesParams` and `search_examples` tool handler in `server.rs`
4. Update server instructions text to mention the new tool
5. Add unit tests for search scoring and edge cases (empty query, no matches, limit capping)

### Phase 3: Corpus Curation (M)

**Affected components:** Content

1. Review auto-generated corpus entries for quality
2. Add synonym tags for common vocabulary mismatches (e.g., "loop" → `do:`, "lambda" → blocks)
3. Write explanations for entries that lack sufficient context
4. Validate that representative queries return useful results

## References
- Related issues: BT-1347
- Related ADRs: [ADR 0033](0033-runtime-embedded-documentation.md) (Runtime-embedded documentation — prior art for embedding content in the runtime)
- Context7: https://github.com/upstash/context7
- Forge MCP: https://developer.atlassian.com/platform/forge/forge-mcp/
- MCP specification: https://modelcontextprotocol.io/specification/2025-06-18/server/resources
- Existing MCP server: `crates/beamtalk-mcp/src/server.rs`
