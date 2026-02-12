# ADR 0008: Doc Comments and API Documentation

## Status
Proposed (2026-02-06)

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

#### 1. "You're building two doc systems" (Architecture)

`///` is parsed by the Rust compiler, but EEP-48 chunks live in BEAM. The doc pipeline touches every layer: lexer → parser → AST attachment → codegen → post-`erlc` chunk injection. If `@doc` were a pragma instead, it could flow through the existing pragma infrastructure (`@primitive`, `@intrinsic`, `@load`) and codegen would be simpler — just another module attribute. We chose the familiar *syntax* (`///`) but it creates a non-trivial *codegen path* that doesn't exist for any other comment type.

**Why we accept this cost:** The pragma path is simpler for codegen but worse for authoring. `@doc 'long markdown string'` is awkward — string escaping, multi-line handling, no IDE support for Markdown-in-strings. `///` gets syntax highlighting, Markdown preview, and familiar ergonomics for free. The codegen complexity is a one-time cost; the authoring experience is paid on every doc comment written.

#### 2. "EEP-48 format mismatch — Beamtalk methods aren't Erlang functions" (Interop)

EEP-48's `Docs` list uses `{Kind, Name, Arity}` tuples designed for Erlang's `function/arity` model. Beamtalk has unary messages (`abs`), binary operators (`+`), and keyword messages (`to:do:`) — none map cleanly. A keyword message `to:by:do:` becomes a single function with arity 3, but the "name" in EEP-48 would need to encode the selector. Elixir solved this for its model, but Beamtalk's message dispatch is further from Erlang's.

**Why we accept this cost:** EEP-48 is extensible — the `Metadata` map on each doc entry can carry Beamtalk-specific fields (selector, message kind). The `Name` field can use the Beamtalk selector string. This is the same trade-off every non-Erlang BEAM language makes — Elixir, Gleam, LFE all map their models onto EEP-48. The interop benefit (Erlang shell `h/2` works on Beamtalk modules) outweighs the mapping complexity.

#### 3. "Docs should be objects, not frozen strings" (Philosophy)

In Smalltalk, you can programmatically modify documentation — `MyClass comment: 'Updated docs'`. Docs are part of the live image. EEP-48 docs are frozen at compile time. For an "interactive-first" language promising hot code reloading and live development, static docs in `.beam` files mean the live environment can't evolve its own documentation. This contradicts Principle #1 (Interactive-first).

**Why we accept this cost:** EEP-48 docs are frozen per-module, but modules are hot-reloadable. When a class is redefined in the REPL, the new `.beam` includes updated docs. Full Smalltalk-style `comment:` mutation (changing docs without recompiling) is deferred to the source retention future work. For now, "recompile to update docs" is acceptable — Elixir works this way and developers find it natural.

#### 4. "`README.md` for package docs is un-Smalltalk" (Philosophy)

In Smalltalk, everything lives in the image — there are no external files. Using `README.md` for package-level docs introduces a file-based concern into a language that aspires to be interactive-first. A Smalltalk purist would say the package overview should be a class comment on the package's root class.

**Why we accept this cost:** Beamtalk is file-based, not image-based. `README.md` is the universal standard for project documentation — GitHub renders it, package managers index it, every developer expects it. Using `///` for class/method docs and `README.md` for project docs matches developer expectations. If Beamtalk gets an image-based workflow later, the README can be imported into the image.

### Residual Tension

- **"Docs as objects" remains the strongest unresolved philosophical tension** — EEP-48 is pragmatically correct but philosophically un-Smalltalk. Source retention (future work) would close this gap.
- **EEP-48 format mapping** needs a concrete design decision during Phase 2 implementation: how selectors map to `{Kind, Name, Arity}` tuples.
- **The two-system cost** is real but bounded — the doc chunk injection is a well-understood post-processing step, not an ongoing maintenance burden.

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

### Phase 1: Lexer + AST (S)
- Extend lexer to recognize `///` as `Trivia::DocComment`
- Add `doc_comment: Option<String>` to `MethodDefinition` and `ClassDefinition` in AST
- `///` before a class definition attaches to the `ClassDefinition` node
- `///` before a method definition attaches to the `MethodDefinition` node
- Parser extracts leading doc-comment trivia and attaches to the following AST node
- **Note:** Trivia currently attaches to *tokens*, not AST nodes. The parser must collect doc-comment trivia from the leading trivia of the first token of a method/class definition and "lift" it to the enclosing AST node during parsing. This is similar to how `leading_comments` is already handled for `Module`.

### Phase 2: EEP-48 Doc Chunks in Codegen (M)
- Generate EEP-48 `docs_v1` chunks in compiled `.beam` files so docs are runtime-accessible
- Use the standard `docs_v1` structure:
  ```erlang
  {docs_v1, Anno, beamtalk, <<"text/markdown">>, ModuleDoc, Metadata, Docs}
  ```
- `BeamLanguage` = `beamtalk` (identifies docs as Beamtalk-originated)
- `Format` = `<<"text/markdown">>` (raw Markdown from `///` comments)
- `ModuleDoc` = class `///` doc (the `///` before the class definition, or `none` if absent)
- `Docs` = list of `{{function, Name, Arity}, Anno, Signature, Doc, Metadata}` for each documented method
- Docs become accessible at runtime via `code:get_doc/1` and `shell_docs:render/2`
- Erlang/Elixir tools can read Beamtalk docs natively (BEAM ecosystem interop)
- **Implementation note:** Core Erlang doesn't directly support doc chunks. The `Docs` chunk must be added post-compilation via `beam_lib:build_module/2` or by generating the chunk alongside the `.core` → `.beam` pipeline.

### Phase 3: LSP Integration (S)
- Hover provider reads `doc_comment` from Method/Class nodes (compile-time, from AST)
- Completion provider includes first line of doc in completion items
- **Markdown rendering:** LSP protocol natively supports Markdown in `MarkupContent` — pass raw Markdown to the editor, no rendering needed on our side

### Phase 4: REPL `:help` (M)
- `:help ClassName` shows class `///` doc
- `:help ClassName selector` shows method `///` doc
- **Doc delivery:** Use `code:get_doc/1` to fetch docs from compiled `.beam` files at runtime (EEP-48). No source files or separate index needed — docs travel with compiled code.
- **Doc inheritance:** Walk the class hierarchy to find docs for inherited methods (like Pharo). If `Counter` doesn't document `spawn`, show `Actor`'s docs for it.
- Extend existing `:help` command to dispatch `:help ClassName selector` (currently only shows generic command help)
- **Markdown rendering for terminal:** Use `shell_docs:render/2` (OTP built-in) for EEP-48 docs, or a crate like `termimad` for styled terminal output from raw Markdown.

### Phase 5: Stdlib Documentation (M)
- Add `///` doc comments to all classes and methods in `lib/*.bt`
- Add `README.md` for package-level overview documentation
- Follow convention: first line = summary, `## Examples` section with `// =>` assertions

### Phase 6: `beamtalk doc` Command (M)
- Generate HTML reference from `///` comments (or read from EEP-48 chunks)
- Use `README.md` as the landing page for generated documentation
- Index by class, method name, category
- Include cross-references between classes
- **Markdown rendering for HTML:** Requires a Markdown-to-HTML library (e.g., `pulldown-cmark` or `comrak`)

### Future: Smalltalk-style Source Retention
- Full Smalltalk-style live source access (`MyClass >> #method` returns source) is deferred
- Requires architectural decision about image-based vs file-based workflow
- EEP-48 docs provide the critical runtime documentation path; source retention adds browsing capability on top

## Migration Path

**No breaking changes.** The `///` syntax is new — existing `//` comments are unaffected. The stdlib files will need doc comments added incrementally (Phase 4).

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
