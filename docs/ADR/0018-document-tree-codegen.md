# ADR 0018: Document Tree Code Generation (Wadler-Lindig Pretty Printer)

## Status
Proposed (2026-02-11)

## Context

Beamtalk generates Core Erlang text (ADR 0003) via **direct string emission** — 1,100+ `write!`/`writeln!` macro calls across 22 files in `crates/beamtalk-core/src/codegen/core_erlang/`. The current architecture uses a single `String` output buffer with manual indentation tracking:

```rust
// Current approach: imperative string building
pub(super) struct CoreErlangGenerator {
    output: String,       // Direct string buffer
    indent: usize,        // Manual indentation counter
    // ...
}

// Example: generating a method table
writeln!(self.output, "'method_table'/0 = fun () ->")?;
self.indent += 1;
self.write_indent()?;
write!(self.output, "~{{")?;
for (i, (name, arity)) in methods.iter().enumerate() {
    if i > 0 { write!(self.output, ", ")?; }
    write!(self.output, "'{name}' => {arity}")?;
}
writeln!(self.output, "}}~")?;
self.indent -= 1;
```

### Problems with Direct String Emission

**1. Refactoring is error-prone and expensive.** BT-454 (rename all compiled modules from `beamtalk_*` to `bt@stdlib@*`) requires finding and updating hardcoded module name strings scattered across 22 files. There's no single abstraction for "emit a module reference" — just raw `write!` calls with inline strings.

**2. Indentation is fragile.** The `self.indent` counter must be manually incremented/decremented in matched pairs. Forgetting a decrement or nesting incorrectly produces malformed Core Erlang that `erlc` rejects — but the Rust code compiles fine.

**3. No composability.** Code fragments can't be built independently and combined. Every `write!` call mutates a shared `String` buffer, so you can't build a function body, inspect it, test it in isolation, or compose it with other fragments.

**4. Testing requires full string comparison.** Snapshot tests compare entire generated files. There's no way to unit-test individual code generation fragments (e.g., "does this method table generate correctly?") without running the full pipeline.

**5. The codebase is large and growing.** 1,100+ `write!`/`writeln!` calls across 22 files, with the heaviest files being:

| File | `write!` calls | Purpose |
|------|---------------|---------|
| `primitive_implementations.rs` | 136 | Intrinsic method bodies |
| `counted_loops.rs` | 126 | Loop codegen |
| `gen_server/methods.rs` | 98 | Method dispatch |
| `value_type_codegen.rs` | 87 | Value type modules |
| `intrinsics.rs` | 80 | Intrinsic dispatch |

As the language grows (pattern matching, exception handling, type annotations), this approach will become increasingly difficult to maintain.

### What Other Rust-Based Compilers Do

**Gleam** (Rust → Erlang/JS) uses a `Document` algebraic data type based on Wadler-Lindig's "Strictly Pretty" algorithm. Code generation builds a tree of document nodes, then renders once:

```rust
// Gleam's approach — declarative document composition
let module = docvec![
    header,
    "-compile([no_auto_import, nowarn_unused_vars]).",
    line(),
    exports,
    join(statements, lines(2)),
];
module.to_pretty_string(80)
```

Gleam's `Document` enum has variants: `Str`, `Line`, `Nest`, `Group`, `Vec`, `Break`. The `docvec!` macro provides ergonomic composition. Gleam uses this for both Erlang and JavaScript backends — same `Document` type, different rendering.

**Other compilers:**
- **rustc** uses `rustc_ast_pretty` (Wadler-style document trees) for AST pretty-printing
- **prettyplease** (Rust ecosystem) uses Wadler-style documents for `syn` AST formatting
- **SWC** (Rust → JS) uses structured AST → document rendering
- The `pretty` crate on crates.io provides a ready-made Wadler-Lindig implementation

**Pattern:** Modern Rust compilers overwhelmingly use document trees for text code generation, not direct string emission.

## Decision

**Replace direct `write!`/`writeln!` string emission with a Wadler-Lindig document tree for Core Erlang code generation.**

Introduce a `Document` enum (or use the `pretty` crate) and a `docvec!` macro for composing Core Erlang output. The code generator builds a tree of `Document` nodes, which is rendered to a string in a final pass.

### What This Looks Like

**Before (current):**
```rust
fn generate_method_table(&mut self, methods: &[(String, usize)]) -> Result<()> {
    writeln!(self.output, "'method_table'/0 = fun () ->")?;
    self.indent += 1;
    self.write_indent()?;
    write!(self.output, "~{{")?;
    for (i, (name, arity)) in methods.iter().enumerate() {
        if i > 0 { write!(self.output, ", ")?; }
        write!(self.output, "'{name}' => {arity}")?;
    }
    writeln!(self.output, "}}~")?;
    self.indent -= 1;
    Ok(())
}
```

**After (document tree):**
```rust
fn generate_method_table(&self, methods: &[(String, usize)]) -> Document {
    let entries = join(
        methods.iter().map(|(name, arity)| docvec!["'", name, "' => ", arity]),
        ", "
    );
    docvec![
        "'method_table'/0 = fun () ->",
        nest(INDENT, docvec![line(), "~{", entries, "}~"]),
    ]
}
```

**Key differences:**
- Returns a `Document` instead of mutating `self.output`
- No manual `indent += 1` / `indent -= 1` — `nest()` handles it declaratively
- The result is composable — can be embedded in a larger document
- Can be unit tested: `assert_eq!(generate_method_table(&methods).to_string(), "...")`

### Document Type

A minimal `Document` enum for Core Erlang generation (based on Gleam's approach, ~200 lines):

```rust
pub enum Document<'a> {
    /// A string literal
    Str(&'a str),
    /// An owned string
    String(String),
    /// A newline followed by current indentation
    Line,
    /// Increase indentation for nested content
    Nest(isize, Box<Document<'a>>),
    /// A sequence of documents
    Vec(Vec<Document<'a>>),
    /// Empty document
    Nil,
}
```

Core Erlang doesn't need Gleam's full pretty-printer (no `Group`, `Break`, `ForceBroken` — we don't need automatic line-wrapping decisions). A simplified subset suffices.

### Approach: Roll Our Own vs. Use a Crate

**Recommended: Roll a minimal implementation** (~200 lines), following Gleam's proven design.

| Option | Pros | Cons |
|--------|------|------|
| `pretty` crate | Full Wadler-Lindig, well-tested | Overkill — Core Erlang doesn't need line-wrapping heuristics; adds dependency |
| Gleam-style minimal | Exactly what we need, no unused features, easy to understand | Must write ~200 lines |
| Full Wadler-Lindig custom | Future-proof for formatting tools | Unnecessary complexity |

Core Erlang has fixed formatting (no "fit on one line vs. break" decisions), so we only need: `Str`, `String`, `Line`, `Nest`, `Vec`, `Nil`, and a `docvec!` macro.

## Prior Art

| Compiler | Language | Approach | Notes |
|----------|----------|----------|-------|
| **Gleam** | Rust → Erlang/JS | Custom `Document` tree (~750 lines) | Wadler-Lindig with `docvec!` macro; shared across Erlang + JS backends |
| **rustc** | Rust → LLVM | `rustc_ast_pretty` module | Document tree for AST pretty-printing |
| **prettyplease** | Rust syn → Rust | Wadler-style documents | Formats generated Rust code |
| **Elm compiler** | Haskell → JS | Wadler pretty-printer | Standard in Haskell ecosystem |
| **PureScript** | Haskell → JS | `Doc` type with `render` | Composable document fragments |
| **OCaml compiler** | OCaml → native | `Format` module | Built-in pretty-printing with boxes |

**Universal pattern:** Compilers that emit text-based output use document trees. Direct string concatenation is the exception, not the norm.

**Gleam's evolution:** Gleam started with simpler codegen and grew into the `Document` approach as complexity increased. Beamtalk is at a similar inflection point — 1,100+ write calls across 22 files.

## User Impact

This is a **purely internal refactoring** — it changes how the compiler generates Core Erlang, not what it generates. No user-facing behavior changes.

| Persona | Impact |
|---------|--------|
| **Beamtalk developer** | None — same REPL, same error messages, same compiled output |
| **Compiler contributor** | Significant improvement — codegen is easier to read, write, test, and refactor |
| **AI agent** | Significant improvement — structured codegen is easier to modify correctly than scattered `write!` calls |

### Contributor Experience (Primary Beneficiary)

**Before:** Adding a new codegen feature requires:
1. Understanding the `self.output` mutation flow across multiple files
2. Manually tracking indentation state
3. Writing snapshot tests that compare entire generated files
4. Risk of indentation bugs that produce valid Rust but invalid Core Erlang

**After:** Adding a new codegen feature requires:
1. Writing a function that returns a `Document`
2. Composing it with existing document fragments using `docvec!`
3. Unit testing the fragment in isolation
4. Indentation is handled declaratively

## Steelman Analysis

### Option A: Document Tree (Recommended)

- 🧑‍💻 **Newcomer contributor**: "I can understand `docvec!['init'/1 = fun () ->', nest(4, body)]` immediately — it reads like the Core Erlang it produces. I don't need to trace `self.indent` mutations across files."
- 🎩 **Smalltalk purist**: "Smalltalk compilers use composable IR representations internally. A document tree is the Smalltalk way — objects representing structure, rendered lazily."
- ⚙️ **BEAM veteran**: "The generated Core Erlang is identical either way. I don't care how the compiler builds the string internally, as long as the output is correct."
- 🏭 **Operator**: "No runtime impact. But fewer codegen bugs means fewer bad BEAM files in production."
- 🎨 **Language designer**: "This is the right abstraction level. The codegen should express *what* Core Erlang to produce, not *how* to concatenate strings."

### Option B: Keep `write!` (Status Quo)

- 🧑‍💻 **Newcomer contributor**: "I already know `write!` from Rust — no new concepts to learn. The pattern is simple even if verbose."
- 🎩 **Smalltalk purist**: "Don't fix what isn't broken. Ship language features, not infrastructure."
- ⚙️ **BEAM veteran**: "Same argument — the output is what matters, not the internal representation."
- 🏭 **Operator**: "Any refactoring risks regressions. The current code works."
- 🎨 **Language designer**: "The write! approach is battle-tested in this codebase. A rewrite introduces risk for aesthetic benefit."

### Tension Points

- **Risk vs. maintainability**: The status quo has zero risk today but increasing maintenance cost as features grow. The document tree has one-time migration risk but reduces ongoing cost.
- **Familiarity**: `write!` is standard Rust; `docvec!` is a custom macro. But `docvec!` is learnable in minutes and the pattern is well-documented (Gleam, Wadler-Lindig papers).
- **Scope**: This is a large refactoring (~1,100 call sites across 22 files). It could be done incrementally (file by file) or atomically.

## Alternatives Considered

### 1. Template Engine (Tera, Askama)

Use a template engine with Core Erlang templates containing placeholders.

```
// hypothetical template
module '{{ module_name }}' [{{ exports }}]
  attributes [{{ attributes }}]

{% for function in functions %}
'{{ function.name }}'/{{ function.arity }} = fun ({{ function.params }}) ->
    {{ function.body }}
{% endfor %}
```

**Rejected because:**
- Core Erlang's nested expression structure doesn't map well to flat templates
- Recursive expression generation (message sends inside message sends) requires programmatic construction
- Template debugging is harder than Rust code debugging
- Would require a new dependency and template language knowledge

### 2. Typed Core Erlang IR

Build a full typed intermediate representation of Core Erlang:

```rust
enum CoreExpr {
    Let { var: String, value: Box<CoreExpr>, body: Box<CoreExpr> },
    Apply { fun: Box<CoreExpr>, args: Vec<CoreExpr> },
    Case { expr: Box<CoreExpr>, clauses: Vec<CoreClause> },
    Map { pairs: Vec<(CoreExpr, CoreExpr)> },
    Literal(CoreLiteral),
    // ...
}
```

**Rejected because:**
- Significantly more work than a document tree (~2,000+ lines for a full Core Erlang AST)
- Over-engineered for our needs — we don't transform or optimize the IR, we just emit it
- The beamtalk AST → Core Erlang AST mapping would be a large rewrite
- A document tree provides 90% of the benefit at 10% of the cost

A typed IR may become valuable later (for optimization passes, multiple backends), but it's premature now. The document tree doesn't preclude adding an IR later — they serve different purposes.

### 3. Use the `pretty` Crate

Use the existing `pretty` crate from crates.io instead of rolling our own.

**Rejected because:**
- The `pretty` crate implements full Wadler-Lindig with line-wrapping heuristics (`Group`, `Break`, `ForceBroken`) — complexity we don't need
- Core Erlang has fixed formatting; we never need "fit on one line or break" decisions
- A minimal custom implementation (~200 lines) is simpler, has zero dependencies, and is tailored to our needs
- Gleam took the same approach (custom ~750 line implementation) for similar reasons

### 4. Incremental Adoption — `write!` + Document Hybrid

Keep `write!` for existing code, use `Document` only for new code.

**Partially adopted:** The migration strategy (see Implementation) is incremental. But the end goal is full migration — a permanent hybrid would be confusing for contributors who must learn both patterns.

## Consequences

### Positive

- **Easier refactoring**: Module name changes (BT-454), dispatch pattern changes, and new language features require changing document constructors, not hunting through `write!` calls
- **Declarative indentation**: `nest()` eliminates manual `indent += 1` / `indent -= 1` pairs and the bugs they cause
- **Composable fragments**: Code generation functions return `Document` values that can be composed, tested, and reused
- **Unit testable**: Individual codegen functions can be tested without running the full pipeline
- **Readable codegen**: `docvec!` expressions read like the Core Erlang they produce
- **AI-agent friendly**: Structured document construction is easier for AI agents to modify correctly than scattered string mutations

### Negative

- **Migration effort**: ~1,100 `write!` call sites across 22 files need conversion (estimated L-sized effort)
- **New concept**: Contributors must learn the `Document` type and `docvec!` macro (mitigated: simple API, well-documented pattern)
- **Regression risk**: Any large refactoring risks introducing bugs (mitigated: existing snapshot tests catch output changes)
- **No user-visible benefit**: This is pure internal improvement — hard to justify over feature work

### Neutral

- **Generated output unchanged**: Byte-for-byte identical Core Erlang output (verified by snapshot tests)
- **Performance**: Document tree adds one allocation + render pass. Negligible compared to `erlc` compilation time
- **Future backends**: If Beamtalk ever adds an Erlang source backend (ADR 0003 leaves this open), the `Document` type would be reusable

## Implementation

### Phase 0: Foundation (~S)
1. Add `Document` enum and `docvec!` macro in `crates/beamtalk-core/src/codegen/core_erlang/document.rs`
2. Implement `to_string()` / `render()` for the document type
3. Add unit tests for the document primitives

### Phase 1: Leaf Functions (~M)
Convert the simplest, most self-contained codegen functions first:
- `util.rs` — helper functions
- `operators.rs` — binary/unary operators (12 calls)
- `erlang_types.rs` — type mappings (10 calls)
- `repl_codegen.rs` — REPL wrappers (10 calls)

### Phase 2: Expression Core (~M)
- `expressions.rs` — literals, identifiers, message sends (66 calls)
- `intrinsics.rs` — intrinsic dispatch (80 calls)

### Phase 3: Control Flow (~M)
- `control_flow/mod.rs`, `while_loops.rs`, `counted_loops.rs`, `list_ops.rs`, `exception_handling.rs` (249 calls total)

### Phase 4: Gen Server (~L)
- `gen_server/methods.rs`, `callbacks.rs`, `dispatch.rs`, `spawn.rs`, `state.rs` (293 calls total)

### Phase 5: Module Generation (~M)
- `beamtalk_module.rs`, `value_type_codegen.rs`, `actor_codegen.rs`, `dispatch_codegen.rs`, `primitive_implementations.rs` (367 calls total)

### Phase 6: Cleanup (~S)
- Remove `self.output: String` and `self.indent: usize` from `CoreErlangGenerator`
- Change `CoreErlangGenerator` methods from `&mut self → Result<()>` to `&self → Document`
- Update `mod.rs` entry point to build document tree and render once

**Verification at each phase:** Run `just ci` — snapshot tests ensure output is identical.

**Each phase can be a separate PR.** The hybrid approach (some functions return `Document`, others still `write!`) works during migration because document fragments can be rendered to strings and written to the buffer.

## Migration Path

Not applicable — this is an internal refactoring with no user-facing changes.

## References

- Related ADR: [ADR 0003 — Keep Core Erlang as Primary Code Generation Target](0003-core-erlang-vs-erlang-source.md)
- Gleam's Document implementation: [compiler-core/src/pretty.rs](https://github.com/gleam-lang/gleam/blob/main/compiler-core/src/pretty.rs) (~750 lines)
- Gleam's Erlang codegen using documents: [compiler-core/src/erlang.rs](https://github.com/gleam-lang/gleam/blob/main/compiler-core/src/erlang.rs) (65 `docvec!` calls)
- Wadler-Lindig paper: ["Strictly Pretty" (2000) by Christian Lindig](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200)
- `pretty` crate: [docs.rs/pretty](https://docs.rs/pretty/latest/pretty/)
- `prettyplease` crate: [github.com/dtolnay/prettyplease](https://github.com/dtolnay/prettyplease)
- BT-454: Rename all compiled modules (motivating example for refactoring difficulty)
