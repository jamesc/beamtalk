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

**1. Indentation is fragile.** The `self.indent` counter must be manually incremented/decremented in matched pairs. Forgetting a decrement or nesting incorrectly produces malformed Core Erlang that `erlc` rejects — but the Rust code compiles fine. This class of bug is invisible at compile time.

**2. No composability.** Code fragments can't be built independently and combined. Every `write!` call mutates a shared `String` buffer, so you can't build a function body, inspect it, test it in isolation, or compose it with other fragments. Adding new codegen features requires understanding the mutation flow across multiple files.

**3. Testing requires full string comparison.** The codegen subsystem has 196 snapshot tests and 170 unit tests, with 84 assertions that directly inspect `self.output` string content. There's no way to unit-test individual code generation fragments (e.g., "does this method table generate correctly?") without running the full pipeline.

**4. The codebase is large and growing.** 1,100+ `write!`/`writeln!` calls across 22 files, with the heaviest files being:

| File | `write!` calls | Purpose |
|------|---------------|---------|
| `primitive_implementations.rs` | 136 | Intrinsic method bodies |
| `counted_loops.rs` | 126 | Loop codegen |
| `gen_server/methods.rs` | 98 | Method dispatch |
| `value_type_codegen.rs` | 87 | Value type modules |
| `intrinsics.rs` | 80 | Intrinsic dispatch |

As the language grows (pattern matching, exception handling, type annotations), this approach will become increasingly difficult to maintain.

**Note:** Some specific refactoring tasks (e.g., module renaming) can be addressed independently by extending existing abstractions like `ModuleName` in `erlang_types.rs`. The document tree is a long-term architectural improvement for the codegen subsystem as a whole.

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

Gleam's `Document` enum has variants: `Str`, `Line`, `Nest`, `Group`, `Vec`, `Break`. The `docvec!` macro provides ergonomic composition. Gleam uses this for both Erlang and JavaScript backends — same `Document` type, different rendering. Gleam's implementation is ~875 lines including tests.

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
| **Gleam** | Rust → Erlang/JS | Custom `Document` tree (~875 lines) | Wadler-Lindig with `docvec!` macro; shared across Erlang + JS backends |
| **rustc** | Rust → LLVM | `rustc_ast_pretty` module | Document tree for AST pretty-printing |
| **prettyplease** | Rust syn → Rust | Wadler-style documents | Formats generated Rust code |
| **Elm compiler** | Haskell → JS | Wadler pretty-printer | Standard in Haskell ecosystem |
| **PureScript** | Haskell → JS | `Doc` type with `render` | Composable document fragments |
| **OCaml compiler** | OCaml → native | `Format` module | Built-in pretty-printing with boxes |

**Universal pattern:** Compilers that emit text-based output use document trees. Direct string concatenation is the exception, not the norm.

**Gleam's evolution:** Gleam started with simpler codegen and grew into the `Document` approach as complexity increased. Beamtalk is at a similar inflection point — 1,100+ write calls across 22 files.

**Why not a full Core Erlang IR?** The prior art compilers listed above all use document trees rather than typed target-language IRs for their text backends. A typed Core Erlang IR (Alternative 2, below) would only become valuable if beamtalk needed to *transform* or *optimize* the generated Core Erlang before emission — which it currently doesn't, since `erlc` handles all optimization passes.

## User Impact

This is a **purely internal refactoring** — it changes how the compiler generates Core Erlang, not what it generates. No user-facing behavior changes.

| Persona | Impact |
|---------|--------|
| **Newcomer** | None — same REPL, same error messages, same compiled output |
| **Smalltalk developer** | None — language semantics and syntax unchanged |
| **Erlang/BEAM developer** | None — generated Core Erlang is byte-for-byte identical |
| **Operator** | None — no runtime impact; fewer codegen bugs means fewer bad BEAM files in production |
| **Compiler contributor** | Significant improvement — codegen is easier to read, write, test, and refactor |

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

### The Strongest Argument Against This ADR

**This is a pure refactoring of the largest subsystem in a ~15k-line compiler that has multiple active epics of unimplemented language features. The refactoring produces zero user-visible value. Every hour spent migrating `write!` calls is an hour not spent on pattern matching, type inference, or the features that will determine whether Beamtalk has users.**

This is a legitimate concern. The ADR proceeds despite it because:
1. The migration is designed to be **organic, not dedicated** — new code uses `Document`, old code migrates opportunistically during feature work
2. Codegen is the subsystem that *every* language feature touches — improving its architecture reduces the cost of all future features
3. The `Document` type itself is ~200 lines of net-new code; the migration cost is spread across feature PRs, not front-loaded

### Option A: Document Tree (Recommended)

- 🧑‍💻 **Newcomer contributor**: "I can understand `docvec!['init'/1 = fun () ->', nest(4, body)]` immediately — it reads like the Core Erlang it produces. I don't need to trace `self.indent` mutations across files."
- 🎩 **Smalltalk developer**: "Smalltalk compilers use composable IR representations internally. A document tree is the Smalltalk way — objects representing structure, rendered lazily."
- ⚙️ **BEAM veteran**: "The generated Core Erlang is identical either way. I don't care how the compiler builds the string internally, as long as the output is correct."
- 🏭 **Operator**: "No runtime impact. But fewer codegen bugs means fewer bad BEAM files in production."
- 🎨 **Language designer**: "This is the right abstraction level. The codegen should express *what* Core Erlang to produce, not *how* to concatenate strings."

### Option B: Keep `write!` (Status Quo)

- 🧑‍💻 **Newcomer contributor**: "I already know `write!` from Rust — no new concepts to learn. The pattern is simple even if verbose."
- 🎩 **Smalltalk developer**: "Don't fix what isn't broken. Ship language features, not infrastructure."
- ⚙️ **BEAM veteran**: "Same argument — the output is what matters. Don't touch what's working."
- 🏭 **Operator**: "Any refactoring risks regressions. The current code works and has 196 snapshot tests validating it."
- 🎨 **Language designer**: "The `write!` approach is battle-tested in this codebase. The refactoring competes with feature work for attention — it must earn its priority."

### Option C: Helper Methods Only (No Document Tree)

- 🧑‍💻 **Newcomer contributor**: "`indented()` closures are idiomatic Rust. No new abstraction to learn."
- 🎩 **Smalltalk developer**: "Incremental improvement. Ship small fixes now, rethink architecture later."
- ⚙️ **BEAM veteran**: "Centralizing module names into helpers solves the real pain point without touching 1,100 call sites."
- 🏭 **Operator**: "Zero-risk change — each helper can be adopted one call site at a time."
- 🎨 **Language designer**: "This is 80% of the value at 20% of the cost. But it doesn't solve composability or fragment testing."

### Tension Points

- **Risk vs. maintainability**: The status quo has zero risk today but increasing maintenance cost as features grow. The document tree has one-time migration risk but reduces ongoing cost. Helper methods sit in the middle — low risk, moderate improvement.
- **Familiarity**: `write!` is standard Rust; `docvec!` is a custom macro. But `docvec!` is learnable in minutes and the pattern is well-documented (Gleam, Wadler-Lindig papers).
- **Timing**: Multiple active epics will change codegen substantially. Doing a dedicated migration now risks conflicting with feature PRs. Organic migration avoids this by folding migration into feature work.
- **Opportunity cost**: Time spent on infrastructure vs. features. Mitigated by organic migration — the `Document` type itself is ~200 lines; no dedicated migration phases compete with feature work.

## Alternatives Considered

### 1. Helper Methods on CoreErlangGenerator (80/20 Solution)

Extract helper methods that encapsulate common patterns without introducing a new intermediate representation:

```rust
impl CoreErlangGenerator {
    fn indented(&mut self, body: impl FnOnce(&mut Self) -> Result<()>) -> Result<()> {
        self.indent += 1;
        body(self)?;
        self.indent -= 1;
        Ok(())
    }

    fn emit_call(&mut self, module: &str, function: &str, args: &[&str]) -> Result<()> { ... }
    fn emit_let(&mut self, var: &str, body: impl FnOnce(&mut Self) -> Result<()>) -> Result<()> { ... }
}
```

**Partially adopted:** `indented()` and targeted helpers should be introduced regardless — they provide immediate value at zero risk and can be adopted one call site at a time. However, helpers alone don't solve composability (fragments can't be returned, stored, or tested independently) or the fundamental problem of interleaved mutation. Helper methods are a stepping stone, not the destination.

### 2. Template Engine (Tera, Askama)

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

### 3. Typed Core Erlang IR

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

### 4. Use the `pretty` Crate

Use the existing `pretty` crate from crates.io instead of rolling our own.

**Rejected because:**
- The `pretty` crate implements full Wadler-Lindig with line-wrapping heuristics (`Group`, `Break`, `ForceBroken`) — complexity we don't need today
- Core Erlang currently has fixed formatting; we don't need "fit on one line or break" decisions
- A minimal custom implementation (~200 lines) is simpler, has zero dependencies, and is tailored to our needs
- Gleam took the same approach (custom ~875 line implementation) for similar reasons

**Future note:** If pattern matching compilation produces deeply nested `case` expressions where line-wrapping matters for debugging generated `.core` files, we may need `Group`/`Break` variants. The minimal `Document` type can be extended with these variants without adopting the full `pretty` crate — Gleam demonstrates this progression.

### 5. Incremental Adoption — `write!` + Document Hybrid

Keep `write!` for existing code, use `Document` only for new code.

**Partially adopted:** The migration strategy (see Implementation) is incremental. But the end goal is full migration — a permanent hybrid would be confusing for contributors who must learn both patterns.

## Consequences

### Positive

- **Easier refactoring**: Module name changes, dispatch pattern changes, and new language features require changing document constructors, not hunting through `write!` calls
- **Declarative indentation**: `nest()` eliminates manual `indent += 1` / `indent -= 1` pairs and the bugs they cause
- **Composable fragments**: Code generation functions return `Document` values that can be composed, tested, and reused
- **Unit testable**: Individual codegen functions can be tested without running the full pipeline
- **Readable codegen**: `docvec!` expressions read like the Core Erlang they produce
- **AI-agent friendly**: Structured document construction is easier for AI agents to modify correctly than scattered string mutations

### Negative

- **Migration effort**: ~1,100 `write!` call sites across 22 files need conversion over time (mitigated: organic migration during feature work, not a dedicated project)
- **New concept**: Contributors must learn the `Document` type and `docvec!` macro (mitigated: simple API, well-documented pattern)
- **Regression risk**: Any large refactoring risks introducing bugs (mitigated: 196 snapshot tests catch output changes)
- **Test migration**: 170 codegen unit tests with 84 assertions that directly inspect `self.output` content will need updating as each file migrates to return `Document` values
- **Hybrid-state confusion**: During the organic migration period, some functions return `Document` while others write to `self.output` — contributors must understand both patterns (mitigated: clear documentation of which pattern to use where; new code always uses `Document`)
- **No user-visible benefit**: This is pure internal improvement — it competes with feature work for attention

### Neutral

- **Generated output unchanged**: Byte-for-byte identical Core Erlang output (verified by snapshot tests)
- **Performance**: Document tree adds one allocation + render pass. Negligible compared to `erlc` compilation time
- **Future backends**: If Beamtalk ever adds an Erlang source backend (ADR 0003 leaves this open), the `Document` type would be reusable
- **Source maps**: If source-level debugging is added later, the document tree enables it more naturally than `write!` — an `Annotated(Span, Box<Document>)` variant can carry source positions through construction, and the renderer emits Core Erlang line annotations centrally. The current `write!` approach would require manually inserting line annotations across 22 files. Gleam demonstrates this pattern with `-file()` annotations emitted via `docvec!`

## Implementation

### Adoption Strategy: New Code First, Organic Migration

Rather than a dedicated multi-phase migration project that competes with feature work, adopt the document tree **organically**:

### Phase 0: Foundation (~S — single PR)
1. Add `Document` enum and `docvec!` macro in `crates/beamtalk-core/src/codegen/core_erlang/document.rs`
2. Implement `to_string()` / `render()` for the document type
3. Add unit tests for the document primitives
4. Add a `write_document()` bridge method to `CoreErlangGenerator` that renders a `Document` to `self.output` — enabling gradual per-function migration

### Phase 1: New Code Convention
- All **new** codegen functions return `Document` instead of writing to `self.output`
- `write_document()` bridge allows new `Document`-returning functions to coexist with old `write!` code
- No dedicated migration of existing code

### Phase 2: Opportunistic Migration (Ongoing)
- When touching a file for a **feature** (e.g., pattern matching, metaclasses, new control flow), migrate that file's functions to return `Document`
- Each feature PR naturally migrates the functions it modifies
- 196 snapshot tests verify byte-for-byte identical output at each step

### Phase 3: Cleanup (When Migration Naturally Completes)
- Remove `self.output: String` and `self.indent: usize` from `CoreErlangGenerator`
- Change remaining `CoreErlangGenerator` methods from `&mut self → Result<()>` to `&self → Document`
- Update `mod.rs` entry point to build document tree and render once

**Approximate migration order** (based on which subsystems feature work will touch first):

| Subsystem | Files | `write!` calls | Likely Feature Trigger |
|-----------|-------|---------------|----------------------|
| Control flow | 5 files | 249 | Block semantics (BT-204) |
| Gen server | 5 files | 293 | Actor runtime (BT-207) |
| Module generation | 5 files | 367 | Metaclasses (BT-319) |
| Expressions + intrinsics | 2 files | 146 | Stdlib (BT-205) |
| Leaf functions | 5 files | 45 | Various |

**Verification:** Run `just ci` after every migration — snapshot tests ensure output is identical.

**Key advantage:** No dedicated migration phases compete with feature work. The document tree earns its keep by making each feature PR's codegen changes cleaner and more testable.

## Migration Path

Not applicable — this is an internal refactoring with no user-facing changes.

## References

- Related ADR: [ADR 0003 — Keep Core Erlang as Primary Code Generation Target](0003-core-erlang-vs-erlang-source.md)
- Related ADR: [ADR 0007 — Compilable Standard Library with Primitive Injection](0007-compilable-stdlib-with-primitive-injection.md) (class kind routing maintained by document tree migration)
- Related ADR: [ADR 0014 — Beamtalk Test Framework](0014-beamtalk-test-framework.md) (196 codegen snapshot tests serve as verification)
- Gleam's Document implementation: [compiler-core/src/pretty.rs](https://github.com/gleam-lang/gleam/blob/main/compiler-core/src/pretty.rs) (~875 lines)
- Gleam's Erlang codegen using documents: [compiler-core/src/erlang.rs](https://github.com/gleam-lang/gleam/blob/main/compiler-core/src/erlang.rs) (65 `docvec!` calls)
- Wadler-Lindig paper: ["Strictly Pretty" (2000) by Christian Lindig](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200)
- `pretty` crate: [docs.rs/pretty](https://docs.rs/pretty/latest/pretty/)
- `prettyplease` crate: [github.com/dtolnay/prettyplease](https://github.com/dtolnay/prettyplease)
