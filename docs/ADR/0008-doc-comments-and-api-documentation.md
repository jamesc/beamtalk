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

Adopt **`///` triple-slash doc comments** for method and class documentation, and **`////` quadruple-slash doc comments** for module-level documentation, following the Gleam/Rust convention.

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
//// Integer - Whole number arithmetic and operations
////
//// Integers in Beamtalk are arbitrary precision (Erlang integers).
//// All arithmetic operations return integers unless explicitly
//// converted with `asFloat`.
////
//// ## BEAM Mapping
//// Beamtalk integers map directly to Erlang integers.
////
//// ## Examples
//// ```beamtalk
//// 42 class           // => Integer
//// 2 ** 100           // => 1267650600228229401496703205376
//// 17 % 5             // => 2
//// 1 to: 5 do: [:n | Transcript show: n]
//// ```

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
| **Pharo** | `"..."` class/method comments | Plain text | Yes (comment protocol) | Browser |
| **Newspeak** | `(* ... *)` | Plain text | No | None |
| **Python** | `"""..."""` docstrings | reStructuredText | Yes (__doc__) | Sphinx |

**What we adopted:**
- **Gleam/Rust `///` syntax** — Cleanest integration with existing `//` comment syntax. The lexer already handles `//`; extending to `///` is a minimal change. Markdown support comes free.
- **Gleam's `////` for modules** — Cleaner than Rust's `//!` which requires being inside the item.

**What we adapted:**
- **Elixir's doc testing** — `/// ```beamtalk` blocks could be validated by the test runner (future work), similar to Elixir's doctests.

**What we rejected:**
- **Elixir `@doc` pragma** — Would reuse the `@` pragma syntax but adds significant complexity (parser + codegen + runtime metadata storage). The `///` approach gives 90% of the benefit at 30% of the cost. Runtime doc access can be added later if needed.
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

### Option B: `@doc` Pragma (Rejected)

| Cohort | Strongest Argument |
|--------|-------------------|
| 🧑‍💻 **Newcomer** | "I can query docs at runtime — `Integer doc: #+` — which makes discovery natural" |
| 🎩 **Smalltalk purist** | "Documentation as message sends is the most Smalltalk thing possible — everything is an object, including docs" |
| ⚙️ **BEAM veteran** | "Elixir proved this works brilliantly — `Code.fetch_docs/1` is essential for production introspection" |
| 🏭 **Operator** | "Runtime docs mean I can inspect a running system's API without source code" |
| 🎨 **Language designer** | "This is the most composable — docs are data, not syntax, so they compose with metaprogramming" |

**Why rejected despite strong steelman:** Implementation cost is 3-4× higher (parser + codegen + runtime metadata storage + reflection protocol). The `///` approach delivers tooling benefits (LSP, REPL, generated docs) immediately. Runtime doc access via `@doc` can be added as a complementary feature later without breaking `///`.

### Option C: Convention-based `//` Comments (Rejected)

| Cohort | Strongest Argument |
|--------|-------------------|
| 🧑‍💻 **Newcomer** | "No new syntax to learn — I already know `//` comments" |
| 🎩 **Smalltalk purist** | "Minimal language surface area — don't add syntax when convention suffices" |
| ⚙️ **BEAM veteran** | "Ship fast, iterate later — we don't need perfect docs for an alpha language" |
| 🎨 **Language designer** | "YAGNI — add doc syntax when there's real demand, not speculatively" |

**Why rejected:** Without a syntactic distinction, tooling can't reliably tell documentation from implementation comments. Every `// helper function` would be treated as docs. The YAGNI argument is valid for the doc generation tool, but the syntax decision should be made early because it affects every stdlib file.

### Tension Points

- **Smalltalk purists strongly prefer `@doc`** (docs as objects) but accept `///` as pragmatic
- **BEAM veterans are split** — Elixir's `@doc` is beloved, but `///` is good enough
- **Newcomers and tooling developers agree** on `///` — lowest friction, best tooling ROI
- **Operators care about runtime access** which only `@doc` provides — but `:help` in REPL covers 90% of the use case

## Alternatives Considered

### Alternative: `@doc` Pragma

```beamtalk
Object subclass: Integer
  @doc 'Add two integers. Returns the sum.'
  + other => @primitive '+'
```

Rejected because the implementation cost is significantly higher: the pragma would need to be parsed, the doc string would need to flow through codegen to runtime metadata, and a reflection protocol (`doc:`) would need to be implemented. The `///` approach provides the same tooling benefits (LSP hover, generated docs, REPL help) with only lexer and AST changes.

### Alternative: Convention-based `//` Comments

```beamtalk
Object subclass: Integer
  // Add two integers. Returns the sum.
  + other => @primitive '+'
```

Rejected because there's no way to distinguish documentation comments from implementation comments. The comment `// Helper for overflow checking` shouldn't appear in API docs, but `// Add two integers` should. Without a syntactic marker, tooling would either include everything (noisy) or use fragile heuristics.

## Consequences

### Positive
- **Tooling-ready:** LSP hover, completions, and REPL help can extract structured documentation
- **Familiar:** `///` is widely recognized (Rust, Gleam, C#, Dart, Swift)
- **Incremental:** Can start with just lexer support, add doc generation later
- **Composable with `@doc`:** If runtime doc access is needed later, `@doc` can complement `///` without replacing it
- **Markdown support:** Rich formatting, code examples, and links in documentation
- **Doctest potential:** `/// ```beamtalk` blocks could be tested automatically (future work)

### Negative
- **New syntax to learn:** Developers must know `///` vs `//` distinction
- **Not runtime-accessible:** Unlike Elixir's `@doc`, docs aren't available via reflection at runtime (until/unless `@doc` is added later)
- **Stdlib rewrite:** All 26 `lib/*.bt` files need doc comments added
- **Doc delivery for REPL:** The `:help` command needs access to doc strings at runtime, but the stdlib is precompiled to `.beam` files (ADR 0007). This requires either shipping source alongside binaries, a separate doc index, or embedding docs in BEAM modules — each with trade-offs (see Phase 3)

### Neutral
- The existing `//` comment syntax is unaffected — regular comments remain for implementation notes
- The `/* */` block comment syntax is unaffected
- Generated documentation format (HTML vs terminal) is an implementation detail, not part of this ADR

## Resolved Questions

1. **`////` vs `///` for class docs:** **Keep `////` for module/class docs.** The explicit distinction follows Gleam convention and makes intent clear, even though each file currently contains one class. If multi-class files are supported later, the distinction becomes essential.

2. **Doc inheritance:** **Yes — walk the hierarchy.** `:help Counter increment` should show docs inherited from `Actor >> increment` if Counter doesn't override the documentation, following Pharo's approach. This matches the existing `respondsTo:` hierarchy walking (ADR 0006) and provides better UX for users exploring unfamiliar classes.

3. **Structured parameter docs:** **Freeform Markdown only**, following Rust and Gleam. Parameter names are inferred from the method signature. Use conventional headings (`## Arguments`, `## Examples`) for structure. No `@param`/`@returns` tags — keeps it simple and avoids JSDoc-style syntax in a Smalltalk-inspired language.

4. **Doctest interaction with ADR 0014:** **Deferred.** The doctest mechanism will be designed when implementing doctests. The `/// ```beamtalk` syntax is reserved for future use but the integration with ADR 0014's test framework is not decided here.

## Implementation

### Phase 1: Lexer + AST (S)
- Extend lexer to recognize `///` as `Trivia::DocComment` and `////` as `Trivia::ModuleDocComment`
- Add `doc_comment: Option<String>` to `MethodDefinition` and `ClassDefinition` in AST
- Parser extracts leading `///` trivia and attaches to the following AST node
- **Note:** Trivia currently attaches to *tokens*, not AST nodes. The parser must collect doc-comment trivia from the leading trivia of the first token of a method/class definition and "lift" it to the enclosing AST node during parsing. This is similar to how `leading_comments` is already handled for `Module`.

### Phase 2: LSP Integration (S)
- Hover provider reads `doc_comment` from Method/Class nodes
- Completion provider includes first line of doc in completion items
- **Markdown rendering:** LSP protocol natively supports Markdown in `MarkupContent` — pass raw Markdown to the editor, no rendering needed on our side

### Phase 3: REPL `:help` (M)
- `:help ClassName` shows class `////` doc
- `:help ClassName selector` shows method `///` doc
- **Doc inheritance:** Walk the class hierarchy to find docs for inherited methods (like Pharo). If `Counter` doesn't document `increment`, show `Actor`'s docs for it.
- **Doc delivery strategy** (choose one during implementation):
  - **Option A: Parse-at-startup** — Parse `lib/*.bt` source files at REPL startup to extract docs. Simple but requires source files to be present alongside compiled `.beam` files. Adds startup latency (~26 files).
  - **Option B: Pre-compiled doc index** — `beamtalk build-stdlib` generates a doc index file (JSON/ETF) alongside `.beam` files. REPL loads the index at startup. Faster, no source dependency at runtime.
  - **Option C: Embedded in BEAM** — Compile `///` docs into module attributes (similar to Elixir's `Code.fetch_docs/1`). Adds codegen complexity but docs travel with compiled code. This overlaps with the future `@doc` pragma and may warrant a separate ADR.
- Extend existing `:help` command to dispatch `:help ClassName selector` (currently only shows generic command help)
- **Markdown rendering for terminal:** Requires either a crate like `termimad` for styled terminal output, or a minimal custom pass that strips `#` headings, indents code blocks, and applies basic formatting. No Markdown rendering dependency exists in the project today.

### Phase 4: Stdlib Documentation (M)
- Add `///` doc comments to all methods in `lib/*.bt`
- Add `////` module docs to all 26 stdlib files
- Follow convention: first line = summary, `## Examples` section with `// =>` assertions

### Phase 5: `beamtalk doc` Command (M)
- Generate HTML reference from `///` comments
- Index by class, method name, category
- Include cross-references between classes
- **Markdown rendering for HTML:** Requires a Markdown-to-HTML library (e.g., `pulldown-cmark` or `comrak`)

### Future: `@doc` for Runtime Access
- If runtime doc reflection is needed, add `@doc` as a complementary pragma
- `@doc` could be auto-generated from `///` at compile time
- This would be a separate ADR

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
- Gleam documentation: https://tour.gleam.run/functions/documentation-comments/
- Rust rustdoc: https://doc.rust-lang.org/rustdoc/how-to-write-documentation.html
- Elixir ExDoc: https://hexdocs.pm/elixir/writing-documentation.html
- Beamtalk syntax rationale: `docs/beamtalk-syntax-rationale.md` (comments decision)
- Beamtalk principles: `docs/beamtalk-principles.md` (Principle #13: compiler as language service)
