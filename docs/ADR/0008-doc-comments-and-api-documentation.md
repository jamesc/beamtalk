# ADR 0008: Doc Comments and API Documentation

## Status
Accepted (2026-02-12)

## Context

Beamtalk's standard library lives in 26 `lib/*.bt` files that were recently converted from documentation-only stubs to compilable source with `@primitive` pragmas (BT-293, ADR 0007 Phase 2). During conversion, verbose API documentation headers were removed, leaving concise class comments and method signatures as the only documentation.

**The problem:** There is no structured way to document methods, classes, or modules in Beamtalk source code. The language has `//` line comments and `/* */` block comments, but no **doc-comment syntax** that tooling can extract for:

- **LSP hover information** — What does `Integer>>+` do? What are its parameters?
- **Generated API reference** — An HTML/terminal reference like Elixir's HexDocs or Rust's rustdoc
- **REPL help** — `:help Integer +` should show documentation inline
- **IDE completions** — Method completions should show a brief description

**Current state:**
- The lexer preserves comments as leading/trailing trivia on tokens
- The AST stores `leading_comments` at the Module level only — not on Classes or Methods
- The LSP hover provider exists but uses hardcoded strings, not extracted comments
- There is no `beamtalk doc` command or documentation generation tooling

**Constraints:**
- Must work with the existing `//` and `/* */` comment syntax (Beamtalk doesn't use Smalltalk's `"..."` comments)
- Must be Git-friendly — text in source files, not images or databases
- Should support Markdown formatting for rich documentation
- Must integrate with the compiler-as-language-service architecture (Principle #13)
- Should align with Beamtalk's interactive-first philosophy — docs should be discoverable from the REPL

## Decision

Adopt **`///` triple-slash doc comments** for class and method documentation, following the Gleam/Rust convention.

### Method Documentation

```beamtalk
Object subclass: Integer
  /// Add two integers. Returns the sum.
  ///
  /// Supports arbitrary precision — no overflow.
  ///
  /// ## Examples
  /// ```beamtalk
  /// 3 + 4       // => 7
  /// 100 + -50   // => 50
  /// ```
  + other => @primitive '+'

  /// Absolute value of this integer.
  ///
  /// ## Examples
  /// ```beamtalk
  /// -42 abs   // => 42
  /// 7 abs     // => 7
  /// ```
  abs => (self < 0) ifTrue: [self negated] ifFalse: [self]
```

### Class Documentation

```beamtalk
/// Integer - Whole number arithmetic and operations
///
/// Integers in Beamtalk are arbitrary precision (Erlang integers).
/// All arithmetic operations return integers unless explicitly
/// converted with `asFloat`.
///
/// ## BEAM Mapping
/// Beamtalk integers map directly to Erlang integers.
///
/// ## Examples
/// ```beamtalk
/// 42 class           // => Integer
/// 2 ** 100           // => 1267650600228229401496703205376
/// 17 % 5             // => 2
/// 1 to: 5 do: [:n | Transcript show: n]
/// ```
Object subclass: Integer
  // ... methods
```

### REPL Help

```
beamtalk> :help Integer +
Integer >> + other

Add two integers. Returns the sum.
Supports arbitrary precision — no overflow.

Examples:
  3 + 4       // => 7
  100 + -50   // => 50

beamtalk> :help Integer abs
Integer >> abs

Absolute value of this integer.

Examples:
  -42 abs   // => 42
  7 abs     // => 7
```

### Error Example (Misuse)

```beamtalk
// Regular comments are NOT documentation
// This will NOT appear in :help or LSP hover
+ other => @primitive '+'

/// This IS documentation — appears in tooling
- other => @primitive '-'
```

### Generated Documentation

`beamtalk doc` generates HTML reference documentation from `///` comments:

```bash
beamtalk doc lib/           # Generate docs for stdlib
beamtalk doc src/           # Generate docs for user project
open docs/index.html        # Browse generated reference
```

## Prior Art

| Language | Syntax | Format | Runtime Access | Tooling |
|----------|--------|--------|----------------|---------|
| **Gleam** | `///` method, `////` module | Markdown | No | gleam docs |
| **Rust** | `///` item, `//!` module | Markdown | No | rustdoc |
| **Elixir** | `@doc`, `@moduledoc` | Markdown | Yes (Code.fetch_docs) | ExDoc |
| **Erlang (OTP 27+)** | `-doc`, EDoc | Markdown/HTML | Yes (EEP-48, code:get_doc) | shell_docs |
| **Pharo** | `"..."` class/method comments | Plain text | Yes (comment protocol) | Browser |
| **Newspeak** | `(* ... *)` | Plain text | No | None |
| **Python** | `"""..."""` docstrings | reStructuredText | Yes (__doc__) | Sphinx |

**What we adopted:**
- **Gleam/Rust `///` syntax** — Cleanest integration with existing `//` comment syntax. The lexer already handles `//`; extending to `///` is a minimal change. Markdown support comes free.
- **EEP-48 doc chunks** — BEAM-standard documentation format. Docs are compiled into `.beam` files and accessible at runtime via `code:get_doc/1`. This aligns with Smalltalk's philosophy (docs are always available) and enables interop with Erlang/Elixir tooling.
- **`README.md` for package/project docs** — Like Hex/Cargo/Go, prose-level documentation lives in Markdown files, not in source code comments. `beamtalk doc` includes `README.md` as the landing page for generated HTML documentation.

**What we adapted:**
- **Elixir's doc testing** — `/// ```beamtalk` blocks could be validated by the test runner (future work), similar to Elixir's doctests.

**What we rejected:**
- **`////` for module docs** — Beamtalk doesn't have modules (one class per file). Counting slashes (`///` vs `////`) is error-prone. Package-level docs belong in `README.md`, not in source comments. If modules are added later, this decision can be revisited.
- **Elixir `@doc` pragma as syntax** — We use `///` for authoring (familiar, tooling-friendly) but compile to EEP-48 like Elixir does. No `@doc` attribute needed in source.
- **Pharo `"..."` comments** — Beamtalk deliberately chose `//` over Smalltalk's `"..."` for comments (see `docs/beamtalk-syntax-rationale.md`). Using `"..."` for docs would be inconsistent.
- **Python docstrings** — Requires string expressions as the first statement in a body, which doesn't fit Beamtalk's `method => body` syntax.

## User Impact

### Newcomer (from Python/JS/Ruby)
`///` is immediately recognizable — it's the same convention as Rust, TypeScript (`/** */`), and similar to Python docstrings. They'll expect `:help` in the REPL and LSP hover. The `## Examples` section with `// =>` mirrors E2E test format, reinforcing a single pattern.

### Smalltalk Developer
Pharo uses `"..."` comments for class/method docs. The `///` syntax is a departure but follows Beamtalk's existing choice of `//` over `"..."`. Smalltalk developers expect browsable documentation — the `beamtalk doc` command and LSP hover provide this. The REPL `:help` command mirrors Pharo's browser experience.

### Erlang/Elixir Developer
Elixir developers will miss `@doc` and runtime reflection. However, `///` with Markdown is familiar from many languages, and generated HTML docs are the expected output. The `beamtalk doc` command mirrors `mix docs`. Runtime doc access (`:help` in REPL) provides the interactive experience Elixir developers expect.

### Tooling Developer
`///` is the simplest to implement — it's a lexer-level distinction (check for three slashes vs two). The trivia system already preserves comments on tokens. Extending the AST to attach doc comments to Class/Method nodes is straightforward. The LSP hover provider just reads the attached doc string.

## Steelman Analysis

### Strongest Counterarguments Against This Decision

#### 1. "Docs are the wrong investment right now" (Priorities)

Beamtalk has no type system, no pattern matching, no package manager, and an incomplete stdlib. Writing doc infrastructure — lexer changes, AST extensions, EEP-48 codegen, chunk injection, REPL `:help`, HTML generation — before the language stabilizes means documenting APIs that will change. Every hour on `///` parsing is an hour not spent on language features that users actually need. Gleam shipped versions 0.1–0.30 without doc tooling and it was fine. Ship the language first, add docs when the API surface is stable.

**Why we accept this cost:** The ADR establishes *design decisions*, not delivery timelines. Deciding on `///` now means every new stdlib method gets docs from day one — retrofitting docs onto 200+ methods later is worse than growing them incrementally. The implementation phases (1–6) are deliberately sized so Phase 1 (lexer + AST) is small and can ship early without blocking language work. We're not building `beamtalk doc` HTML generation before pattern matching — we're recording the syntax decision so stdlib authors write `///` comments today.

#### 2. "EEP-48 can't express doc inheritance" (Architecture)

When someone asks `:help Counter`, they should see Counter's own methods *and* inherited methods from Actor and Object. But EEP-48 docs are per-module — each `.beam` file contains only its own docs with no concept of class hierarchy. Building `:help` that walks the inheritance chain means building a doc resolution system *on top of* EEP-48, not just mapping into it. This is a second doc system — one that understands Beamtalk's class hierarchy, method resolution order, and overriding. The EEP-48 chunk becomes a storage format, not a query system.

**Why we accept this cost:** The inheritance walk is already implemented for method dispatch (`beamtalk_object_class:find_method/2`). Doc inheritance reuses the same infrastructure — walk the class chain, call `code:get_doc/1` on each module, merge results. This is ~50 lines of Erlang, not a second system. Elixir's `IEx.Helpers.h/1` does the same for behaviours and protocols. The per-module storage is correct — each class documents *its own* methods. The REPL `:help` command composes them at query time.

#### 3. "`///` locks you in before the language can express docs" (Premature Commitment)

Beamtalk might eventually have string interpolation, rich text objects, structured annotations (`@metadata`), or Newspeak-style module declarations. Choosing `///` now means docs are always flat Markdown strings parsed from comments — a design from the 1990s. If the language later gets first-class annotation syntax, the doc system can't use it without a breaking change or migration. Starting with `///` is easy, but it may become the legacy format that every future feature has to work around.

**Why we accept this cost:** `///` is a *syntax* choice, not a *semantic* commitment. The AST stores `doc_comment: Option<String>` — any future annotation system can populate that same field. If `@doc` attributes arrive later, the compiler can accept both `///` and `@doc` (like Erlang accepts both `-doc` and EDoc comments). Markdown-in-comments is the dominant industry pattern (Rust, Gleam, Go, Swift, Kotlin) — it's not 1990s design, it's current best practice. The risk of premature commitment is real but small: `///` is trivially parseable and any migration tool can convert `///` to `@doc` mechanically.

#### 4. "Post-`erlc` BEAM rewriting is fragile" (Engineering Risk)

The plan says "inject docs post-compilation via `beam_lib:build_module/2`." But Beamtalk shells out to `erlc` for Core Erlang → BEAM compilation — it doesn't control the full pipeline like Elixir does. Adding a post-processing step that rewrites `.beam` files means: (a) debugging compilation failures has another suspect, (b) the build pipeline can't be a simple `erlc` invocation anymore, (c) future OTP versions could change BEAM file internals, (d) the chunk injection step must handle every edge case `erlc` handles (compressed modules, native code, etc.). This is fragile plumbing for a young compiler.

**Why we accept this cost:** `beam_lib:build_module/2` is a stable OTP API specifically designed for chunk manipulation — it's not low-level file surgery. Elixir, Gleam, and LFE all use similar post-processing for their metadata. The BEAM file format has been stable for decades and OTP guarantees backward compatibility for `beam_lib`. The post-processing step is isolated and testable: generate chunk data → call one OTP function → write file. If it becomes problematic, an alternative is to generate a companion `.docs` file and teach the REPL to load docs separately (but this loses EEP-48 interop).

### Residual Tension

- **Doc inheritance** is architecturally straightforward but needs design work: how to present inherited vs overridden methods, how to handle `doesNotUnderstand:`, and how deep to walk the chain.
- **EEP-48 selector mapping** needs a concrete design decision during Phase 2: how `to:by:do:` maps to `{function, Name, Arity}` tuples and what goes in the Metadata map.
- **Priority sequencing** is the most practical concern — Phase 1 (lexer + AST) should ship alongside stdlib work, but Phases 2–6 should wait until core language features stabilize.

## Alternatives Considered

### Alternative: `@doc` Pragma

```beamtalk
Object subclass: Integer
  @doc 'Add two integers. Returns the sum.'
  + other => @primitive '+'
```

Rejected because `///` + EEP-48 achieves the same runtime accessibility without a new pragma. The `@doc` syntax would add parser complexity, and the doc string would need its own flow through codegen — whereas `///` trivia naturally attaches to AST nodes and compiles to EEP-48 doc chunks alongside existing codegen.

### Alternative: Convention-based `//` Comments

```beamtalk
Object subclass: Integer
  // Add two integers. Returns the sum.
  + other => @primitive '+'
```

Rejected because there's no way to distinguish documentation comments from implementation comments. The comment `// Helper for overflow checking` shouldn't appear in API docs, but `// Add two integers` should. Without a syntactic marker, tooling would either include everything (noisy) or use fragile heuristics.

## Consequences

### Positive
- **Runtime-accessible:** Docs are embedded in `.beam` files via EEP-48, accessible at runtime via `code:get_doc/1` — aligns with Smalltalk philosophy and BEAM ecosystem conventions
- **BEAM interop:** Erlang and Elixir tools can read Beamtalk docs natively (IEx `h/1`, Erlang shell `h/2`)
- **Tooling-ready:** LSP hover, completions, and REPL `:help` can extract structured documentation
- **Familiar:** `///` is widely recognized (Rust, Gleam, C#, Dart, Swift)
- **Incremental:** Can start with lexer/AST support, add EEP-48 codegen, then REPL `:help`
- **Markdown support:** Rich formatting, code examples, and links in documentation
- **Doctest potential:** `/// ```beamtalk` blocks could be tested automatically (future work)

### Negative
- **New syntax to learn:** Developers must know `///` vs `//` distinction
- **Stdlib rewrite:** All 26 `lib/*.bt` files need doc comments added
- **Codegen complexity:** Generating EEP-48 `Docs` chunks requires building the `docs_v1` term structure and injecting it into the `.core` → `.beam` pipeline

### Neutral
- The existing `//` comment syntax is unaffected — regular comments remain for implementation notes
- The `/* */` block comment syntax is unaffected
- Generated documentation format (HTML vs terminal) is an implementation detail, not part of this ADR

## Resolved Questions

1. **`////` for module docs:** **Removed — just use `///`.** Beamtalk doesn't have modules (one class per file). Package-level docs belong in `README.md`. If modules are added later, a module doc syntax can be introduced then.

2. **Doc inheritance:** **Yes — walk the hierarchy.** `:help Counter spawn` should show docs inherited from `Actor >> spawn` if Counter doesn't override the documentation, following Pharo's approach. This matches the existing `respondsTo:` hierarchy walking (ADR 0006) and provides better UX for users exploring unfamiliar classes.

3. **Structured parameter docs:** **Freeform Markdown only**, following Rust and Gleam. Parameter names are inferred from the method signature. Use conventional headings (`## Arguments`, `## Examples`) for structure. No `@param`/`@returns` tags — keeps it simple and avoids JSDoc-style syntax in a Smalltalk-inspired language.

4. **Doctest interaction with ADR 0014:** **Deferred.** The doctest mechanism will be designed when implementing doctests. The `/// ```beamtalk` syntax is reserved for future use but the integration with ADR 0014's test framework is not decided here.

## Implementation

### Phase 1: Write Stdlib `///` Docs (S)
- Add `///` doc comments to all 26 classes and their methods in `lib/*.bt`
- Add `README.md` for package-level overview documentation
- Follow convention: first line = summary, `## Examples` section with `// =>` assertions
- **Zero infrastructure needed** — `///` is currently a regular comment. Docs are immediately readable in source, on GitHub, and in code review.
- This is the content investment that all future phases consume.

### Phase 2: Lexer + AST (S)
- Extend lexer to recognize `///` as `Trivia::DocComment`
- Add `doc_comment: Option<String>` to `MethodDefinition` and `ClassDefinition` in AST
- `///` before a class definition attaches to the `ClassDefinition` node
- `///` before a method definition attaches to the `MethodDefinition` node
- Parser extracts leading doc-comment trivia and attaches to the following AST node
- **Note:** Trivia currently attaches to *tokens*, not AST nodes. The parser must collect doc-comment trivia from the leading trivia of the first token of a method/class definition and "lift" it to the enclosing AST node during parsing. This is similar to how `leading_comments` is already handled for `Module`.

### Phase 3: EEP-48 + REPL `:help` (M)
- Generate EEP-48 `docs_v1` chunks in compiled `.beam` files so docs are runtime-accessible
- Use the standard `docs_v1` structure:
  ```erlang
  {docs_v1, Anno, beamtalk, <<"text/markdown">>, ModuleDoc, Metadata, Docs}
  ```
- `BeamLanguage` = `beamtalk` (identifies docs as Beamtalk-originated)
- `Format` = `<<"text/markdown">>` (raw Markdown from `///` comments)
- `ModuleDoc` = class `///` doc (the `///` before the class definition, or `none` if absent)
- `Docs` = list of `{{function, Name, Arity}, Anno, Signature, Doc, Metadata}` for each documented method
- **Implementation note:** Core Erlang doesn't directly support doc chunks. The `Docs` chunk must be added post-compilation via `beam_lib:build_module/2` or by generating the chunk alongside the `.core` → `.beam` pipeline.
- Implement `:help ClassName` and `:help ClassName selector` in the REPL
- **Doc delivery:** Use `code:get_doc/1` to fetch docs from compiled `.beam` files at runtime (EEP-48). No source files or separate index needed — docs travel with compiled code.
- **Doc inheritance:** Walk the class hierarchy to find docs for inherited methods (reuses existing `find_method` chain walk). If `Counter` doesn't document `spawn`, show `Actor`'s docs for it.
- **Markdown rendering for terminal:** Use `shell_docs:render/2` (OTP built-in) for EEP-48 docs.

### Phase 4: LSP Integration (S)
- Hover provider reads `doc_comment` from Method/Class nodes (compile-time, from AST)
- Completion provider includes first line of doc in completion items
- **Markdown rendering:** LSP protocol natively supports Markdown in `MarkupContent` — pass raw Markdown to the editor, no rendering needed on our side
- **Depends on:** LSP server existing (separate epic, not yet started)

### Phase 5: `beamtalk doc` Command (M)
- Generate HTML reference from EEP-48 chunks (or directly from `///` comments)
- Use `README.md` as the landing page for generated documentation
- Index by class, method name, category
- Include cross-references between classes
- **Markdown rendering for HTML:** Requires a Markdown-to-HTML library (e.g., `pulldown-cmark` or `comrak`)

### Future: Live Doc Editing
- Workspace supports `Integer doc: 'Updated'` — writes `///` back to `.bt` file, recompiles, hot-reloads (see Steelman #3)
- Full Smalltalk-style source retention deferred — EEP-48 provides the critical runtime path

## Migration Path

**No breaking changes.** The `///` syntax is new — existing `//` comments are unaffected. The stdlib files will need doc comments added incrementally (Phase 1).

Recommended migration order:
1. Core types first: Integer, Float, Number, String, Symbol
2. Collections: List, Dictionary, Set, Tuple, Association
3. Hierarchy: ProtoObject, Object, Actor, Block, CompiledMethod
4. Boolean/Nil: True, False, UndefinedObject
5. Error hierarchy: Exception, Error, RuntimeError, TypeError, InstantiationError
6. System: SystemDictionary, TranscriptStream, File

## References
- Related issues: BT-293 (stdlib conversion that removed API docs)
- Related ADRs: ADR 0007 (compilable stdlib with `@primitive` injection), ADR 0014 (test framework — doctest interaction)
- EEP-48: https://www.erlang.org/doc/apps/kernel/eep48_chapter.html (BEAM documentation storage standard)
- Erlang `code:get_doc/1`: https://www.erlang.org/doc/man/code.html#get_doc-1
- Gleam documentation: https://tour.gleam.run/functions/documentation-comments/
- Rust rustdoc: https://doc.rust-lang.org/rustdoc/how-to-write-documentation.html
- Elixir ExDoc: https://hexdocs.pm/elixir/writing-documentation.html
- Beamtalk syntax rationale: `docs/beamtalk-syntax-rationale.md` (comments decision)
- Beamtalk principles: `docs/beamtalk-principles.md` (Principle #13: compiler as language service)
