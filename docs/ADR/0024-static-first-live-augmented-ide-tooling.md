# ADR 0024: Static-First, Live-Augmented IDE Tooling

## Status
Accepted (2026-02-15)

## Context

Beamtalk is a dynamic, Smalltalk-inspired language. Dynamic features — duck typing, `doesNotUnderstand:`, extension methods, hot code reloading — create fundamental challenges for IDE tooling. When a developer types `counter.` in their editor, the language service must answer: "What methods are available?" Without a static type system, this question has no single correct answer at edit time.

The project has an informal design document (`docs/internal/design-tooling-ide.md`, 2026-02-01) that explored this problem space but was never formalized as an architecture decision. Meanwhile, significant infrastructure has been built:

- **Language Service API** (`LanguageService` trait) with completion, hover, diagnostics, go-to-definition, and find-references providers (BT-14)
- **Semantic analysis** with `ClassHierarchy`, `NameResolver`, block context analysis, sealed enforcement (BT-140, BT-279)
- **JSON-RPC 2.0 daemon** for IDE communication (BT-46)
- **Persistent workspaces** — long-lived BEAM nodes that survive REPL disconnects (ADR 0004)
- **Browser/WebSocket connectivity** to running workspaces (ADR 0017)

This ADR formalizes the tooling architecture, replacing the informal design document with a concrete decision.

### The Problem

Static-only analysis is insufficient for a dynamic language:

| Dynamic Feature | What Static Analysis Misses |
|----------------|----------------------------|
| Duck typing | Can't determine receiver type without inference |
| `doesNotUnderstand:` | Any message might be handled |
| Extension methods | Methods added at runtime |
| Hot code reload | Method table changes while running |
| `perform:` / reflection | Selector determined at runtime |

But pure live introspection (Smalltalk image-style) is also insufficient:

| Limitation | Why It Matters |
|-----------|----------------|
| No offline editing | Can't provide completions for new files before they're loaded |
| Cold start problem | No BEAM running when first opening a project |
| CI/batch workflows | Linting and diagnostics must work without a running system |
| Performance | Network round-trips to a running BEAM add latency |

### Current State

The language service provides single-file, static-only tooling:

| Provider | What It Does Today | What's Missing |
|----------|-------------------|----------------|
| Completions | Keywords + identifiers + class hierarchy methods | Receiver type inference, class-side filtering, cross-file symbols |
| Hover | Literal types, identifier names, message selectors | Type-aware `self: Counter` display, doc comments |
| Diagnostics | Parse errors, undefined vars, sealed violations, block mutations | Cross-file resolution, class variable validation |
| Go-to-definition | First-assignment heuristic (same file only) | Method definitions, class navigation, cross-file |
| Find references | All usages of name in same file | Cross-file, rename refactoring |

All operations are single-file, position-agnostic (position is validated but not used to filter results), and purely static.

### Constraints

1. **Interactive-first** (Principle 1) — tooling must support live development, not just batch workflows
2. **Compiler IS the language service** (Principle 13) — one code path, not separate tools
3. **<100ms response time** — IDE queries must feel instant
4. **Files are source of truth** (Principle 5) — no image snapshots, plain `.bt` files
5. **BEAM-native** — must work with Erlang/OTP introspection capabilities

## Decision

Beamtalk adopts a **static-first, live-augmented** architecture for IDE tooling:

1. **Static analysis is the foundation** — always available, even without a running workspace
2. **Live workspace augmentation** — when a workspace is connected, query the running BEAM for additional information
3. **Graceful degradation** — the IDE experience improves as more information becomes available, but never requires a running system

### Three-Tier Information Model

```
┌─────────────────────────────────────────────────────┐
│ Tier 3: Live Workspace (optional)                   │
│   Query running BEAM for:                           │
│   • Runtime method tables (extensions, DNU)         │
│   • Live object inspection                          │
│   • Actual values and types                         │
│   • Hot-reloaded method changes                     │
│                                                     │
├─────────────────────────────────────────────────────┤
│ Tier 2: Cross-File Static Analysis                  │
│   Project-wide symbol index:                        │
│   • Cross-file go-to-definition                     │
│   • Find all references across project              │
│   • Class hierarchy across multiple files            │
│   • Import/dependency resolution                    │
│                                                     │
├─────────────────────────────────────────────────────┤
│ Tier 1: Single-File Static Analysis (always on)     │
│   Parse + semantic analysis:                        │
│   • Syntax diagnostics                              │
│   • Identifier completions                          │
│   • Local go-to-definition                          │
│   • Class hierarchy (within file)                   │
│   • Hover info (literals, selectors, fields)        │
│                                                     │
└─────────────────────────────────────────────────────┘
```

Each tier strictly extends the one below. Higher tiers add information and may correct lower-tier results — for example, Tier 2 may resolve a Tier 1 "undefined class" diagnostic when the class is defined in another file. Tier 3 adds runtime-only information but does not override static diagnostics (see Conflict Resolution below).

### Architecture

```
┌──────────────┐     ┌──────────────────────────────────────────┐
│   Editor     │     │            Language Server                │
│  (VS Code,   │◄───►│                                          │
│   Neovim,    │ LSP │  ┌────────────┐    ┌──────────────────┐  │
│   Helix)     │     │  │  Static    │    │  Live Connector  │  │
│              │     │  │  Analysis  │    │  (optional)      │  │
└──────────────┘     │  │            │    │                  │  │
                     │  │ • Parser   │    │ • WebSocket to   │  │
                     │  │ • Semantic │    │   workspace BEAM │  │
                     │  │ • Hierarchy│    │ • Method queries │  │
                     │  │ • Symbol   │    │ • Object inspect │  │
                     │  │   Index    │    │ • Push updates   │  │
                     │  └─────┬──────┘    └────────┬─────────┘  │
                     │        │                    │             │
                     │        ▼                    ▼             │
                     │  ┌─────────────────────────────────────┐  │
                     │  │        Query Merger                  │  │
                     │  │  Combines static + live results      │  │
                     │  │  Static wins on conflicts            │  │
                     │  └─────────────────────────────────────┘  │
                     └──────────────────────────────────────────┘
                                        │
                                        ▼
                     ┌──────────────────────────────────────────┐
                     │         Workspace BEAM Node              │
                     │  (ADR 0004 — persistent, detached)       │
                     │                                          │
                     │  • beamtalk_workspace_sup                │
                     │  • Loaded modules + class tables         │
                     │  • Running actors                        │
                     │  • Extension registry                    │
                     └──────────────────────────────────────────┘
```

### Static Analysis (Tier 1 + 2)

Static analysis is the primary source of truth. It works without any running system and covers the common case — developers editing code, navigating definitions, fixing errors.

**Tier 1 (single-file, exists today):**
- Lexing, parsing, semantic analysis per file
- `ClassHierarchy` built from AST (MRO, sealed enforcement)
- Completions from keywords + identifiers + hierarchy methods
- Diagnostics from parse errors + semantic checks
- Go-to-definition via first-assignment heuristic
- Cached per file in `SimpleLanguageService`

**Known issue: semantic analysis divergence.** Principle 13 states "the compiler IS the language service," and lexing, parsing, and codegen are genuinely shared. However, semantic analysis has diverged: `beamtalk build` calls `semantic_analysis::analyse()`, while `SimpleLanguageService` calls `ClassHierarchy::build()` separately and routes diagnostics through a different path. Primitive validation is also duplicated in the daemon. This means a diagnostic that fires in `beamtalk build` may not appear in the IDE, and vice versa. Phase 1 should unify semantic analysis — make `analyse()` the single path that both build and language service consume, with `ClassHierarchy` as an output of analysis rather than a parallel computation.

**Tier 2 (cross-file, to be built):**
- Project-wide symbol index (class names, method selectors, globals)
- Cross-file `ClassHierarchy` merging (class defined in A, used in B)
- Cross-file go-to-definition and find-references
- Stdlib symbols always available (pre-indexed from `lib/*.bt`)
- Incremental updates on file save

### Live Augmentation (Tier 3)

When a workspace is running (which is common — `beamtalk repl` starts one), the language server can connect to it for additional information:

**What the live connector provides:**

```
LSP → Workspace: "What methods does Counter respond to?"
Workspace → LSP: [increment, getValue, value, class, respondsTo:, ...]
                  (includes extension methods, runtime additions)

LSP → Workspace: "What is the type of `counter` binding?"
Workspace → LSP: #Actor<Counter,0.173.0>

LSP → Workspace: "What actors are running?"
Workspace → LSP: [{class: Counter, pid: <0.173.0>}, ...]
```

**When live augmentation activates:**
- Completion on an identifier whose type is unknown statically → query workspace
- Hover on a variable bound in the REPL session → show live value
- Diagnostics for a message send where static analysis can't confirm the method exists → check workspace before reporting error

**When live augmentation does NOT activate:**
- Static analysis has a definitive answer (known class, known method)
- No workspace is connected
- Response would exceed latency budget (<100ms)

### Conflict Resolution

Static analysis and live introspection may disagree. Rules:

1. **Tier 2 corrects Tier 1** — cross-file analysis resolves false positives from single-file analysis (e.g., "undefined class" cleared when class found in another file)
2. **Static diagnostics are authoritative for static code** — if static analysis reports a definitive error (syntax error, sealed violation, wrong arity), it stands even if the method exists at runtime. However, for "method not found" warnings on receivers that use `doesNotUnderstand:` or dynamic dispatch, diagnostics should be downgraded to informational severity and marked `(dynamic site)`.
3. **Live completions are additive** — runtime methods appear alongside static suggestions, clearly marked as `(runtime)` to distinguish them from statically-known methods
4. **Static go-to-definition wins** — navigate to source definition, not runtime dispatch target
5. **Live hover augments** — show both static type info and runtime value when available

### Workspace Connection Lifecycle

The live connector must handle workspace lifecycle gracefully:

- **No workspace:** Tier 3 disabled, static-only mode (Tier 1 + 2)
- **Workspace connecting:** Brief "connecting..." indicator; static results served immediately
- **Workspace connected:** Full Tier 3 augmentation via ADR 0017's Cowboy WebSocket (reuse the same transport, not a separate connection)
- **Workspace restart:** Graceful degradation to static-only until reconnected; cached live data invalidated
- **Multiple workspaces** (ADR 0004): LSP connects to the workspace matching the project root (`SHA256(cwd)` per ADR 0004). If multiple workspaces exist, the project-local workspace takes precedence.
- **Latency budget:** Tier 3 queries must complete within a per-provider budget (completions: 50ms, hover: 30ms, diagnostics: 100ms). If a live query exceeds its budget, serve static results only. Implementation should use prefetch/push where possible to avoid synchronous round-trips.

### No Type Annotation Syntax (For Now)

This ADR explicitly defers the question of type annotation syntax. The combination of:
- Class hierarchy walking (knows all methods on `Counter`)
- Assignment-based inference (`x := Counter spawn` → `x` is `Counter`)
- Live workspace queries (fallback for dynamic cases)

...provides sufficient information for good IDE tooling without introducing annotation syntax. A future ADR can revisit if experience shows gaps.

### REPL Session Examples

**Tier 1 — Static completions (always available):**
```
// User types: counter incr<TAB>
// Completions offered:
//   increment    (Counter)         ← from ClassHierarchy
//   inspect      (Object)          ← inherited
```

**Tier 2 — Cross-file navigation:**
```
// In main.bt:
counter := Counter spawn
counter increment    ← Ctrl+Click → opens counter.bt, line with `increment =>`
```

**Tier 3 — Live augmentation:**
```
// SimpleProxy forwards all messages via doesNotUnderstand:args:
// Static analysis cannot know what methods proxy responds to.
// With live workspace connected:

proxy := SimpleProxy new: counter

// In editor, typing: proxy incr<TAB>
// Completions:
//   increment    (runtime)         ← live workspace knows proxy forwards to Counter
//   inspect      (runtime)         ← Counter inherits from Object
//   identityHash (ProtoObject)     ← from static hierarchy (ProtoObject subclass)
```

## Prior Art

### ElixirLS — Hybrid Static + Runtime (Closest Analogue)

ElixirLS combines AST parsing for in-progress editing with runtime module reflection for compiled dependencies. The `ElixirSense` engine queries loaded modules via Erlang's reflection APIs. Dialyzer provides additional type analysis.

**What we adopt:** The hybrid model — static for editing, runtime for compiled/loaded code. ElixirLS proves this works well on BEAM.

**What we adapt:** ElixirLS reflects on compiled `.beam` files. Beamtalk reflects on the running workspace, which includes live actors and dynamic extensions — richer than module reflection alone.

### ELP (Erlang Language Platform) — Pure Static, Rust-Based

WhatsApp's ELP is inspired by rust-analyzer: incremental analysis, `RootDatabase` for semantic state, VFS abstraction. Purely static — no runtime connection.

**What we adopt:** The incremental analysis architecture and Rust implementation approach. Our `SimpleLanguageService` cache follows a similar pattern.

**What we reject:** Pure static is insufficient for a Smalltalk-inspired language with duck typing and dynamic dispatch.

### Pharo/Squeak — Pure Live Introspection

Smalltalk IDEs are the running system — the class browser queries live objects, the debugger modifies running code. There is no offline mode.

**What we adopt:** The aspiration that developers should be able to inspect live objects from the IDE. Tier 3 provides this.

**What we reject:** Requiring a running image for any tooling. Beamtalk files are source of truth (Principle 5), and IDE features must work on cold start.

### TypeScript — Static Foundation for Dynamic Language

TypeScript made JavaScript toolable through optional type annotations and a language server that IS the compiler. The type system powers completions, navigation, and refactoring.

**What we adopt:** "Compiler IS the language service" (Principle 13). Our `LanguageService` trait follows TypeScript's architecture.

**What we defer:** Type annotation syntax. TypeScript needed annotations because JavaScript has no class hierarchy for method discovery. Beamtalk's class hierarchy + live workspace provides sufficient information without annotations.

### Gleam — Static Types on BEAM

Gleam has a full static type system and provides rich IDE tooling through type-driven analysis.

**What we observe:** Gleam proves that excellent BEAM tooling is possible. But Gleam achieved it by being statically typed — a design choice Beamtalk deliberately avoids (dynamic dispatch is core to the Smalltalk heritage).

### Newspeak/Hopscotch — Live IDE, No Global Namespace

Newspeak's Hopscotch IDE follows the Smalltalk tradition of live introspection but adds compositional UI and hyperlinked tool navigation. Like Pharo, the IDE IS the running system — there's no offline mode.

**What we adopt:** The aspiration of tool composability — fluidly moving from code browsing to inspection to debugging. Beamtalk's workspace browser (ADR 0017) aims for similar navigation.

**What we reject:** Same as Pharo — requiring a live system for basic editing. Newspeak's no-global-namespace design actually makes static analysis harder, not easier. Beamtalk's class hierarchy provides a static backbone that Newspeak lacks.

## User Impact

### Newcomer (from Python/JS/Ruby)

The three-tier model is invisible — completions "just work." As they use the REPL more (workspace running), completions get richer without any configuration. This matches the experience of modern editors with TypeScript/Pyright where more context → better suggestions.

**Downside:** Without type annotations, completions may feel less precise than TypeScript/Pyright. When working with duck-typed code (no explicit class), static completions fall back to generic Object methods — less helpful than a typed language would offer.

### Smalltalk Developer

Tier 3 (live augmentation) preserves the Smalltalk experience of "the IDE knows what the running system knows." A connected workspace means `counter.` shows all methods including runtime additions — just like a Pharo class browser. The key difference is graceful degradation: when no image is running, static analysis still works.

**Downside:** Tier 3 is a future phase — initial releases will be static-only, which is less capable than a Pharo class browser. Smalltalk developers will need to adjust to "the IDE doesn't know everything yet."

### Erlang/Elixir Developer

Familiar from ElixirLS — static analysis for most editing, with runtime introspection available. The daemon protocol (JSON-RPC 2.0) is standard. Diagnostics include Erlang-relevant checks (sealed classes map to `-sealed` attribute concepts).

**Downside:** No Dialyzer-style deep type analysis. Beamtalk's static analysis is class-hierarchy-based, not constraint-based like Dialyzer.

### Production Operator

No impact on production systems. The live connector is read-only (queries only, no mutations). Workspace introspection uses the same mechanisms as `observer` and `recon` — standard BEAM tooling.

### Tooling Developer

Clear architecture with well-defined boundaries:
- Write a new provider → implement against `LanguageService` trait
- Add static analysis → extend `ClassHierarchy` or `NameResolver`
- Add live queries → extend `LiveConnector` (Tier 3)
- Each tier is independently testable

## Steelman Analysis

### Alternative A: Pure Static (ELP-style)

| Cohort | Strongest Argument |
|--------|-------------------|
| 🧑‍💻 **Newcomer** | "Works immediately, no setup — I open a file and get completions" |
| 🎩 **Smalltalk purist** | *(Weak)* "At least it doesn't depend on a running image I might corrupt" |
| ⚙️ **BEAM veteran** | "ELP proves pure static works for Erlang at WhatsApp scale" |
| 🏭 **Operator** | "No runtime dependency — tooling can't affect production" |
| 🎨 **Language designer** | "Simpler architecture, fewer failure modes, easier to maintain" |

**Tension:** Newcomers and operators prefer simplicity. But Smalltalk developers lose the live introspection that defines their workflow. BEAM veterans note that ELP works for Erlang (static modules) but Beamtalk's dynamic dispatch is fundamentally different.

### Alternative B: Pure Live (Pharo-style)

| Cohort | Strongest Argument |
|--------|-------------------|
| 🧑‍💻 **Newcomer** | *(Weak)* "If the REPL auto-starts, I'd always have completions" |
| 🎩 **Smalltalk purist** | "This IS Smalltalk — the IDE is the running system, period" |
| ⚙️ **BEAM veteran** | "BEAM has excellent introspection — `Module:module_info/0` gives everything" |
| 🏭 **Operator** | *(Weak)* "At least I could inspect production from the IDE" |
| 🎨 **Language designer** | "Maximum accuracy — shows what the system actually does, not what we guess" |

**Tension:** Smalltalk purists strongly prefer this, but it breaks CI/batch workflows and requires a running system for basic editing. The cold-start problem is real — opening a fresh project with no workspace means zero completions.

### Alternative C: Type Annotations First (TypeScript-style)

| Cohort | Strongest Argument |
|--------|-------------------|
| 🧑‍💻 **Newcomer** | "I know this from TypeScript — add types, get completions" |
| 🎩 **Smalltalk purist** | *(Hostile)* "This is NOT Smalltalk anymore" |
| ⚙️ **BEAM veteran** | "Dialyzer specs work well — `@spec` annotations are proven on BEAM" |
| 🏭 **Operator** | "Type errors caught at compile time = fewer production incidents" |
| 🎨 **Language designer** | "Most information per keystroke — types power everything" |

**Tension:** Newcomers from TypeScript would welcome this, but Smalltalk developers would revolt. Adding type syntax is a language design decision with far-reaching consequences. The hybrid approach defers this without closing the door.

### Chosen: Static-First, Live-Augmented (Hybrid)

This approach captures the benefits of pure static (works offline, fast, CI-friendly) while preserving the Smalltalk aspiration (live system introspection when available). It's the only option where every cohort has at least a "good" experience, even if no cohort gets their "perfect" experience.

## Alternatives Considered

### Alternative D: Status Quo (Single-File Static Only)

Keep the current `SimpleLanguageService` with per-file caching, no cross-file analysis, no live connection. Incrementally improve providers within the single-file model.

**Rejected because:** While functional for small files, single-file analysis cannot resolve cross-file class references — the common case in any real project. `Counter spawn` in `main.bt` can't offer `Counter` completions if `Counter` is defined in `counter.bt`. This gap grows as the stdlib and project size increase. The status quo is a foundation, not a destination.

### Alternative A: Pure Static Analysis

Rely entirely on AST parsing, class hierarchy, and eventual type inference. No connection to running systems.

**Rejected because:** Beamtalk's interactive-first principle (Principle 1) requires tooling that understands the running system. Extension methods, `doesNotUnderstand:` handlers, and hot-reloaded code would be invisible to a purely static tool. This works for Erlang (static modules) but not for a Smalltalk-inspired language with dynamic dispatch.

### Alternative B: Pure Live Introspection

Follow Pharo's model — the IDE queries the running system for all information.

**Rejected because:** Violates "files are source of truth" (Principle 5). Doesn't work for CI, batch compilation, or cold-start editing. Would require mandatory workspace startup before any IDE features work.

### Alternative C: Type Annotations First

Design a type annotation syntax and build all tooling on top of the type checker.

**Rejected (deferred) because:** Premature — the class hierarchy already provides method discovery for the common case. Type annotations are a significant language design decision that deserves its own ADR when evidence shows they're needed. The hybrid approach works without them.

## Consequences

### Positive

- IDE works immediately on project open (Tier 1 — no setup required)
- Experience improves progressively as workspace connects (Tier 2 → 3)
- Compatible with CI/batch workflows (static analysis doesn't need BEAM)
- Preserves Smalltalk live development experience when workspace is running
- Architecture naturally supports future type system if one is added
- Existing `LanguageService` trait and providers remain valid — extend, don't replace
- Each tier is independently testable and deployable

### Negative

- More complex than pure static or pure live alone — two code paths to maintain
- Live connector adds a network dependency (WebSocket to workspace)
- Developers may be confused by completions that appear/disappear as workspace connects/disconnects
- Conflict resolution rules add cognitive load for tooling developers
- Tier 3 accuracy depends on workspace state matching editor state (stale workspace = stale suggestions)

### Neutral

- BT-301 (ADR: Hybrid Static+Dynamic Tooling) is superseded by this ADR
- BT-456 (Epic: LSP Class System Integration) gains an architectural foundation
- The informal design document (`docs/internal/design-tooling-ide.md`) is superseded — its content is captured here and in the implementation
- No type annotation syntax is introduced; this decision is deferred to a future ADR (BT-304)
- The daemon protocol (JSON-RPC 2.0) is unaffected — it remains the transport between editor and language server
- The MCP server (BT-512) and LSP live connector should share a unified workspace introspection API to avoid duplicating the BEAM query surface
- Tier 3 live queries reuse ADR 0017's Cowboy WebSocket transport — no new connection mechanism is introduced

### DDD Model Impact

This ADR introduces new domain concepts that extend the **Language Service** bounded context in `docs/beamtalk-ddd-model.md`:

**New Aggregate:**
- `ProjectIndex` (Aggregate Root) — manages the project-wide symbol index across all files. Replaces `SimpleLanguageService`'s per-file `HashMap` with a merged view. Owns the cross-file `ClassHierarchy`.

**New Domain Services:**
- `LiveConnector` — queries a running workspace BEAM for runtime method tables, live values, and actor state. Stateless: each query is independent. Belongs to Language Service context but crosses into Workspace context.
- `QueryMerger` — combines static analysis results with live workspace results. Applies conflict resolution rules (Tier 2 corrects Tier 1; Tier 3 augments with `(runtime)` label). Pure function: `merge(static_results, live_results) → merged_results`.

**New Value Objects:**
- `TierSource` — enum `{Static, CrossFile, Runtime}` annotating where a completion/diagnostic originated. Enables UI labeling (`(runtime)`) and conflict resolution.
- `ProviderBudget` — per-provider latency limit (completions: 50ms, hover: 30ms, diagnostics: 100ms). Used by `LiveConnector` to decide whether to serve live or static-only results.

**Extended Entities:**
- `Completion` — gains `source: TierSource` field to distinguish static vs runtime suggestions
- `CachedFile` — gains reference to `ProjectIndex` for cross-file resolution

**Relationship Changes:**
- Language Service context gains a **dependency on Workspace context** (Tier 3 only, optional). This is a controlled anti-corruption layer — the `LiveConnector` translates workspace domain concepts (actors, modules, method tables) into Language Service value objects (completions, hover info).

**DDD Model Update Required:** `docs/beamtalk-ddd-model.md` should be updated when Phase 1 implementation begins to reflect `ProjectIndex`, `LiveConnector`, and `TierSource`.

## Implementation

### Affected Components

| Component | Changes | Phase |
|-----------|---------|-------|
| `crates/beamtalk-core/src/language_service/` | Add cross-file `ProjectIndex`, extend `LanguageService` trait | 1 |
| `crates/beamtalk-core/src/queries/completion_provider.rs` | Receiver type inference, class-side filtering, cross-file symbols, live method merge | 1, 2 |
| `crates/beamtalk-core/src/queries/hover_provider.rs` | Type-aware `self` display, doc comments, live value display | 1, 2 |
| `crates/beamtalk-core/src/queries/diagnostic_provider.rs` | Cross-file resolution, class variable validation | 1 |
| `crates/beamtalk-core/src/queries/` | New `definition_provider.rs` (extract from mod.rs) | 1 |
| `crates/beamtalk-core/src/semantic_analysis/` | Cross-file `ClassHierarchy` merging | 1 |
| `crates/beamtalk-cli/src/commands/daemon/` | LSP protocol (replace custom JSON-RPC), live connector | 1, 2 |
| `runtime/apps/beamtalk_workspace/src/` | Introspection API for live queries | 2 |

### Phased Rollout

**Phase 1 — Class-Aware Static Analysis + Cross-File Index** (BT-456, near-term):
- Receiver type inference from assignments (`x := Counter spawn` → `x` is `Counter`)
- Class-side method filtering in completions
- Self-type hover (`self: Counter` vs `self: Counter class`)
- Extract `definition_provider.rs` from `language_service/mod.rs`
- Class variable diagnostics
- Project-wide symbol index (class names, method selectors)
- Cross-file go-to-definition and find-references
- Cross-file `ClassHierarchy` merging (class defined in A, used in B)
- Stdlib pre-indexing (all `lib/*.bt` classes always available)
- Incremental index updates on file save
- Tests: extend existing provider unit tests, multi-file integration tests, E2E tests for class-aware completions

**Phase 2 — Live Workspace Connection** (longer-term):
- **Phase 2a (wire-check):** Minimal round-trip — LSP queries workspace for `allClasses` via existing TCP protocol; validate latency is within budget. No UI changes, just prove the plumbing works.
- Phase 2b: Runtime method table queries and completion merging
- Live value display in hover
- Push notifications for method table changes (hot reload)
- Runtime completions marked as `(runtime)`
- Tests: integration tests with running workspace, latency benchmarks

**Phase 3 — Full LSP Protocol** (longer-term):
- Replace custom JSON-RPC daemon with standard LSP server
- Document symbols, workspace symbols, rename, code actions
- Publish VS Code / Neovim / Helix extensions

## Implementation Tracking

| Phase | Epic/Issue | Status |
|-------|-----------|--------|
| Phase 1 | BT-456 (Epic: LSP Class System Integration) | Backlog |
| Phase 2 | TBD (depends on ADR 0017 implementation) | Not started |
| Phase 3 | TBD | Not started |

## References

- Related issues: BT-456 (Epic: LSP Class System Integration), BT-301 (superseded), BT-253 (REPL message protocol)
- Related ADRs: ADR 0004 (Persistent Workspace Management), ADR 0006 (Unified Method Dispatch), ADR 0013 (Class Variables — LSP section), ADR 0017 (Browser Connectivity)
- Supersedes: `docs/internal/design-tooling-ide.md` (informal design document)
- Documentation: `docs/beamtalk-ddd-model.md` (Language Service bounded context)
- Prior art: [ElixirLS](https://github.com/elixir-lsp/elixir-ls), [ELP](https://github.com/WhatsApp/erlang-language-platform), [rust-analyzer](https://github.com/rust-lang/rust-analyzer)
- Research: [Execution vs Parse-based Language Servers](https://stefan-marr.de/papers/dls-marr-et-al-execution-vs-parse-based-language-servers/) (Marr et al.)
