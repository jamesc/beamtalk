# ADR 0044: Comments as First-Class AST Nodes

## Status
Proposed 2026-02-28

## Context

### Current State

Comments in Beamtalk are currently handled as token-level trivia. The lexer captures
them correctly — the `Trivia` enum distinguishes `LineComment`, `BlockComment`, and
`DocComment` — but the parser discards them when building the AST, with one exception:
`///` doc comments are lifted onto `ClassDefinition` and `MethodDefinition` nodes per
ADR 0008. All other comments are lost after parsing.

This means:

```beamtalk
// Calculate compound interest using P * (1 + r/n)^(nt)
calculateInterest: principal rate: rate =>
  principal * ((1 + (rate / 12)) raisedTo: 12)
```

After parsing, the comment is gone. The `MethodDefinition` node has no record it existed.

### Why This Is Now a Problem

Three converging needs expose this gap:

**1. Formatter / unparser.** The lint framework (BT-951) and the lint violation cleanup
epic (BT-962) make it clear that Beamtalk needs a `beamtalk fmt` command. A formatter
must round-trip source files losslessly. If comments are not in the AST, the formatter
silently drops them — which is unacceptable.

**2. Live tool persistence.** A developer tool should allow classes to be built or
modified incrementally in a live environment and then persisted back to `.bt` files on
disk. Beamtalk is disk-backed (not image-based), so persistence means unparse-to-source.
A class modified in a live tool was originally written with comments; those comments
must survive the round-trip. Additionally, a class *created* in a live tool (no source
file origin, no token stream to recover from) needs a way to attach comments to
synthesized AST nodes.

**3. Tooling == compiler.** ADR 0024 established the principle that the compiler
pipeline should be the language service — one parser, one AST, every tool consuming
the same data. This is Anders Hejlsberg's key insight from TypeScript: the moment you
have two parsers or two ASTs, they diverge and tooling becomes a maintenance burden.
Comments being absent from the compiler's AST is a direct violation: a hypothetical
formatter would need a separate parse to recover them, breaking the principle.

### Constraints

- `.bt` files remain the source of truth on disk. This is not a full Smalltalk image.
- The AST must support both parsed code (comments from tokens) and synthesized code
  (comments attached programmatically, no source positions available).
- The existing `doc_comment: Option<String>` on classes and methods (ADR 0008) must
  be preserved and generalised, not replaced.
- Comment handling must not complicate the codegen pipeline — the compiler ignores
  comments.
- Synthesised AST nodes (from live tools or auto-generated code like `with*:` methods)
  have no source positions. Comment attachment must work without valid spans.

## Decision

Comments are first-class data attached directly to AST nodes. Every comment belongs
to exactly one AST node, either as a **leading comment** (appears before the node in
source) or a **trailing comment** (appears at the end of the same line as the node).
No separate comment collections exist at any scope.

### The Core Invariant

> **Every comment belongs to exactly one AST node. No separate flat comment lists
> exist at module scope or any other scope.**

The association rule:
- A comment that appears *before* a node → leading comment on that node
- A comment that appears at the *end of the same line* as a node → trailing comment on that node
- A comment after the last node in a file → trailing comment on the last node
- A comment before the first node in a file → leading comment on the first node

This rule is unambiguous and eliminates the dual-storage anti-pattern.

### Doc Comment vs Regular Comment Deduplication

`///` doc comments remain in `doc_comment: Option<String>` on `ClassDefinition` and
`MethodDefinition` as established by ADR 0008. They are **not** duplicated into
`CommentAttachment.leading`. The parser handles them in separate passes:
`collect_doc_comment()` extracts `///` trivia into `doc_comment`;
`collect_comment_attachment()` extracts `//` and `/* */` trivia into `CommentAttachment`.

This avoids Go's dual-storage anti-pattern: each comment type has exactly one home.

| Comment syntax | AST storage | Runtime storage | Established by |
|---------------|-------------|-----------------|----------------|
| `///` doc comment | `doc_comment: Option<String>` | `CompiledMethod.doc` (object state) | ADR 0008 (AST field), ADR 0033 (runtime path) |
| `//` line comment | `CommentAttachment.leading` or `.trailing` | Not compiled — formatter only | This ADR |
| `/* */` block comment | `CommentAttachment.leading` or `.trailing` | Not compiled — formatter only | This ADR |

Note: ADR 0033 superseded ADR 0008's EEP-48 doc chunk generation. `///` comments now
compile to `doc:` message sends that populate `CompiledMethod.doc` at class load time,
rather than being embedded as BEAM file chunks. The `doc_comment` AST field remains the
correct vehicle for carrying this data through compilation. Regular `//` and `/* */`
comments have no runtime representation — they exist in the AST solely for the
formatter/unparser.

### Synthesised AST Nodes

Comments on synthesised AST nodes (from live tools, auto-generated `with*:` methods, or
`ClassBuilder` codegen) use `Span::default()` for the span field. Consumers that inspect
comment spans (LSP, diagnostics) must treat `Span::default()` as "no source location" —
consistent with how synthesised `Expression` nodes already handle missing spans.

### Section-Divider Comments

Comments that serve as section dividers between methods (e.g. `// ---- Query Methods ----`)
are not semantically associated with any single method. Under the "leading on following
node" rule, they attach as leading comments on the next method. This is imperfect — if
that method is deleted, the section comment disappears. However, this is an acceptable
trade-off: section dividers are rare in idiomatic Beamtalk, the formatter can emit them
faithfully during round-trips, and the alternative (free-floating comments) reintroduces
dual storage. A future refinement could add an explicit `SectionComment` node at the
class body level if this proves to be a practical problem.

### AST Changes

#### New `CommentAttachment` type

```rust
/// Comments attached to an AST node.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct CommentAttachment {
    /// Comments appearing on lines immediately before this node.
    /// Ordered top-to-bottom as they appear in source.
    pub leading: Vec<Comment>,
    /// A single end-of-line comment on the same line as this node.
    pub trailing: Option<Comment>,
}

impl CommentAttachment {
    pub fn is_empty(&self) -> bool {
        self.leading.is_empty() && self.trailing.is_none()
    }
}
```

#### Expression nodes carry comments

Rather than adding `CommentAttachment` to all 19 `Expression` variants, comments attach
at the **statement sequence level** — the granularity where comments naturally live in
source code. A comment between two expressions in a method body belongs to one of those
expressions, not to a sub-expression within them.

The `ExpressionStatement` wrapper carries the attachment:

```rust
/// An expression in a statement position, with optional surrounding comments.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionStatement {
    pub comments: CommentAttachment,
    pub expression: Expression,
}
```

Method bodies, block bodies, and module-level expression sequences all become
`Vec<ExpressionStatement>` instead of `Vec<Expression>`.

Note: comments inside block bodies (e.g. `[:each | // Transform... \n each asUppercase]`)
are covered because `Block.body` is also `Vec<ExpressionStatement>`. Comments between
sub-expressions within a single expression (e.g. between arguments in a keyword send)
are **not** preserved by this design — they remain at statement granularity only. This
is a deliberate scope limitation: sub-expression comments are rare and the formatter can
handle them via whitespace preservation rather than AST attachment.

#### Class, method, and state declaration nodes

`ClassDefinition` and `MethodDefinition` already have `doc_comment: Option<String>` for
`///` comments. They gain a full `CommentAttachment` for `//` and `/* */` comments that
appear before the definition (separate from doc comments):

```rust
pub struct MethodDefinition {
    pub comments: CommentAttachment,   // // and /* */ above the method
    pub doc_comment: Option<String>,   // /// doc comment (existing, ADR 0008)
    // ... existing fields unchanged ...
}

pub struct ClassDefinition {
    pub comments: CommentAttachment,   // // and /* */ above the class
    pub doc_comment: Option<String>,   // /// doc comment (existing, ADR 0008)
    // ... existing fields unchanged ...
}
```

`StateDeclaration` gains both `CommentAttachment` (for `//`/`/* */`) and
`doc_comment: Option<String>` (for `///`). State fields commonly carry explanatory
comments; capturing `///` at parse time is necessary so the formatter, LSP, and future
field reflection can all use the same data source:

```rust
pub struct StateDeclaration {
    pub comments: CommentAttachment,   // // and /* */ above the field
    pub doc_comment: Option<String>,   // /// doc comment (field-level)
    // ... existing fields unchanged (name, type_annotation, default_value, span) ...
}
```

**Runtime compilation of field doc comments is deferred.** The `///` on a state field
is collected by the parser and available throughout the AST pipeline (formatter, LSP,
semantic analysis), but the compiler does not yet emit anything for it. A follow-up ADR
covering `FieldDescriptor` objects will define the runtime storage and accessor API.

`FieldDescriptor` is the right model — consistent with `Class` and `CompiledMethod`
being first-class objects you introspect via message sends. A `FieldDescriptor` will
carry `.name`, `.doc`, `.typeAnnotation`, `.defaultValue` as messages, participate in
the metaclass tower, and be accessible via `MyClass fields` returning a collection of
`FieldDescriptor` instances held by the class object. This mirrors how `MyClass methods`
returns `CompiledMethod` instances from the class's method dictionary today. Both are
class-side objects — not per-instance data.

Type annotations will be stored on `FieldDescriptor` (and on `CompiledMethod` for
consistency) as inert metadata — accessible via reflection but never enforced at
runtime. This follows the Strongtalk model: type annotations are optional, the runtime
performs no type checks, but the annotations survive as data so live tools and
reflection APIs can read them without re-parsing source. The Elixir `@spec` precedent
on BEAM validates this: typespecs are compiled into module attributes, used by Dialyzer
and documentation tooling, ignored by the VM. A synthesised class built by a live tool
with no source file would otherwise have no way to expose type information.

The current `MethodDefinition.return_type` and `ParameterDefinition.type_annotation`
fields in the AST are discarded by codegen today. The `FieldDescriptor` ADR will define
the compilation target for both field and method type annotations, and `CompiledMethod`
will be updated in the same pass for consistency.

#### Module

`Module.leading_comments: Vec<Comment>` (currently the only comment store) is replaced
by leading comments on the first item in the module. If the module is empty, file-level
comments become trailing on a synthetic `EmptyFile` node or are preserved in a
`file_leading_comments` field on `Module` as a narrow exception documented explicitly
in the invariant.

### Parser Changes

The parser attaches comments during construction using the existing trivia infrastructure:

```rust
fn collect_comment_attachment(&mut self) -> CommentAttachment {
    let mut leading = Vec::new();
    for trivia in self.current_token().leading_trivia() {
        match trivia {
            Trivia::LineComment(text) | Trivia::BlockComment(text) => {
                leading.push(Comment { content: text.into(), kind: ..., span: ... });
            }
            Trivia::DocComment(_) => { /* handled separately by collect_doc_comment() */ }
            Trivia::Whitespace(_) => {}
        }
    }
    let trailing = self.collect_trailing_comment();
    CommentAttachment { leading, trailing }
}
```

**Note on trailing comments:** Leading comments are found in the *current* token's
leading trivia — comments that precede the token the parser is about to consume.
Trailing comments (end-of-line after a node) require a different mechanism: after
constructing a node, the parser checks the *next* token's leading trivia for a comment
on the same line as the node's last token. This is a post-parse attachment step, not a
look-ahead into the current token.

**Note on `collect_doc_comment` interaction:** The existing `collect_doc_comment()` uses
`_ => lines.clear()` — any non-doc trivia (including `//` comments) resets the doc
comment accumulator. This means a `//` comment interleaved before `///` lines is correctly
excluded from the doc comment. The `collect_comment_attachment()` function processes the
same trivia but skips `DocComment` entries, so each comment type has exactly one owner.
The ordering dependency is: `collect_doc_comment()` runs first (for `///`), then
`collect_comment_attachment()` runs on the same trivia (for `//` and `/* */`).

### Unparser / Formatter

The unparser emits comments at their attached positions:

```rust
fn unparse_expression_statement(stmt: &ExpressionStatement) -> Document {
    let leading = stmt.comments.leading.iter()
        .map(|c| docvec![c.text(), Document::Newline])
        .collect();
    let trailing = stmt.comments.trailing.as_ref()
        .map(|c| docvec![Document::Text("  "), c.text()])
        .unwrap_or(Document::Nil);
    docvec![leading, unparse_expression(&stmt.expression), trailing]
}
```

### Programmatic Construction (Live Tools)

A class built in a live tool with no source origin attaches comments directly:

```rust
let method = MethodDefinition {
    comments: CommentAttachment {
        leading: vec![Comment::line("Calculate compound interest")],
        trailing: None,
    },
    doc_comment: Some("P * (1 + r/n)^(nt)".into()),
    // ...
};
```

No source positions needed. The comment is data, not recovered from text.

## Prior Art

| Language | Approach | Key Lesson |
|----------|----------|------------|
| **Newspeak** | Comments are first-class metadata on AST nodes, accessible via mirrors at runtime. Tagged metadata uses `(*:tag: ... *)` syntax. | Direct ancestor. Validates Option A. Comments as data, not trivia, is the right model for a reflective language. |
| **Pharo** | `RBComment` objects attached to enclosing sequence nodes by source interval. Not full AST nodes but associated data. | Comments should be associated data, not recovered from text gaps. The interval-based approach is fragile under AST manipulation. |
| **Go (gofmt)** | Dual storage: flat `Comments []*CommentGroup` on `File` AND `Doc`/`Comment` fields on individual nodes. | The Go team calls free-floating comments "the single biggest mistake" in the AST design. Dual storage causes constant synchronisation burden. **Avoid.** |
| **Gleam** | Separate `Vec<Comment>` sorted by position; consumed by position during formatter traversal. | Clean separation but makes programmatic AST construction with comments impossible. Known idempotency bugs. |
| **Elixir** | `Code.string_to_quoted_with_comments/2` returns AST + separate comment list. Sourceror attaches comments to node metadata. | The separate-list approach is backward-compatible but fragile. Sourceror (node metadata) is closer to Option A and is the preferred third-party approach. |
| **Rust (rustfmt)** | Comments not in AST; recovered from source text gaps between spans (`missed_spans.rs`). | Acknowledged as rustfmt's primary source of bugs. Completely unsuitable for synthesised ASTs. **Avoid.** |
| **Tree-sitter** | Comments are full CST nodes (`extras`), siblings of code nodes. | Maximum fidelity. Every tree walk must handle comment siblings. Appropriate for a generic parser framework, overkill for a single language's compiler. |

## User Impact

**Newcomer** (coming from Python/JS/Ruby): No visible change to the language syntax.
Comments work exactly as before. The benefit appears in tooling: `beamtalk fmt` preserves
their comments rather than silently deleting them. This builds trust that the tools are
safe to run.

**Smalltalk developer**: Consistent with Pharo and Newspeak models. Comments surviving
refactoring and formatting is the expected behaviour in Smalltalk environments. The
live-edit-then-persist model matches the Pharo workflow of editing methods in a browser
and filing out to disk.

**Erlang/BEAM developer**: No impact on compiled output from this ADR — `CommentAttachment`
fields (`//` and `/* */`) are skipped by codegen entirely and never appear in Core Erlang
or BEAM bytecode. `StateDeclaration.doc_comment` and type annotations are also skipped
for now (deferred to the `FieldDescriptor` ADR).

The `FieldDescriptor` ADR will introduce compilation targets for field doc comments,
field type annotations, and method type annotations — compiled to message sends to the
**class object** during class definition, following the same pattern as `CompiledMethod.doc`
(ADR 0033). These are class-side operations: `CompiledMethod` instances live in the
class's method dictionary, and `FieldDescriptor` instances will be held by the class
object — neither are per-instance data. The compiled output is class-definition
behaviour in Core Erlang, not BEAM metadata chunks or instance initialisation.
Regular `//` and `/* */` comments never reach the runtime under any scenario.

**Tooling developer** (LSP, formatter, refactoring tools): Major improvement. Every
tool gets complete AST data in one parse. No secondary pass to recover comments. No
separate token stream to maintain. Synthesising new AST nodes with comments is a simple
field assignment.

**Production operator**: No impact. Comments do not affect runtime behaviour, hot code
reloading, or OTP integration.

## Steelman Analysis

### Option B: Separate Comment Index (Gleam/Elixir style)

**🧑‍💻 Newcomer:** "This changes nothing about the AST I'm used to. I can ignore the
comment list entirely if I'm writing a tool that doesn't care about comments."

**🎩 Smalltalk purist:** "Keeping comments out of the AST is cleaner — in Smalltalk,
comments are documentation, not program structure. A separate list honours that."

**⚙️ BEAM veteran:** "The Gleam team chose this approach and it works for their formatter.
If it's good enough for Gleam it's good enough for Beamtalk."

**🎨 Language designer:** "AST nodes stay simple. No wrapper types, no field additions
to every variant. Easier to reason about the AST in isolation."

**Tension:** The steelman is strongest for static formatting of existing source files.
It collapses entirely for synthesised ASTs (live tools) — there is no viable way to
attach comments to nodes that have no source positions. This is the decisive failure.

### Option C: Concrete Syntax Tree (Tree-sitter style)

**🧑‍💻 Newcomer:** "The tree is complete. I never have to worry about lost information."

**⚙️ BEAM veteran:** "Incremental parsing, precise source maps, IDE-quality tooling from
day one."

**🎨 Language designer:** "Full fidelity is the only honest answer. Everything else is
approximation."

**Tension:** Tree-sitter is a parser *framework*, not a language compiler. The overhead
of handling comment siblings in every AST walk, and the rearchitecture required, is not
justified when the language already has a working compiler pipeline. The CST approach is
worth reconsidering if Beamtalk ever adopts tree-sitter as its primary parser.

### Option D: Preserve Token Stream (TypeScript/Roslyn style)

**🧑‍💻 Newcomer:** "Zero changes to the AST. I can learn one thing at a time."

**⚙️ BEAM veteran:** "This is the industry standard. TypeScript, Roslyn, and rust-analyzer
all do it this way. Proven at massive scale."

**🎨 Language designer:** "The token stream is already produced by the lexer. Keeping it
is natural — we're just not throwing it away. The AST stays clean, the formatter gets
what it needs, and nobody else pays a tax."

**🏭 Operator:** "Zero runtime impact — the token stream is only retained for tooling
paths, not compilation."

**Tension:** This is the strongest alternative for pure formatting. But it fails on two
fronts, not one. First, synthesised ASTs have no token stream — a class built in a live
tool cannot emit comments because there are no tokens to walk. Second, and more
fundamentally, token preservation is a *formatting* concept only: it says nothing about
where type annotations and doc comments live on `FieldDescriptor` and `CompiledMethod`
at runtime. Those runtime objects need their data stored as fields — which is exactly
what Option A provides. A system that uses Option D for formatting and Option A for
runtime reflection ends up with two comment/annotation storage models anyway, which is
the split we are trying to avoid.

### Tension Points

Option B is faster to implement and has no AST churn. Option D (token preservation) is
the industry standard for formatters. Option A is the only approach that handles both
synthesised ASTs from live tools *and* runtime metadata storage on `FieldDescriptor`
and `CompiledMethod`.

Live tool persistence and `FieldDescriptor` are not hypothetical future concerns —
they are confirmed requirements. `FieldDescriptor` will store `.doc`, `.typeAnnotation`,
and `.defaultValue` as runtime fields; `CompiledMethod` will gain type annotation
storage in the same pass. Option B and D have no answer for synthesised objects with no
source file origin. Building the formatter on Option B or D and then rebuilding on
Option A when `FieldDescriptor` lands means paying the AST churn twice. The decision
is to pay it once, now.

## Alternatives Considered

### Alternative A: Span-Gap Recovery (rustfmt style)

Recover comments from source text by examining the gap between adjacent node spans.
Requires no AST changes.

**Rejected:** Rustfmt maintainers describe this as the primary source of formatter bugs.
Completely impossible for synthesised AST nodes (no source text to examine). Violates
the "tooling == compiler" principle by requiring the formatter to re-examine source text
rather than using the AST.

### Alternative B: Separate Comment Index (Gleam/Elixir style)

Store a `Vec<Comment>` sorted by byte position alongside the `Module`. During formatting,
consume comments positionally.

**Rejected:** Cannot attach comments to synthesised AST nodes created by live tools
(no source positions). The Go team, who chose this approach (their flat `Comments` list),
call it "the single biggest mistake" in their AST design. Known idempotency bugs in
Gleam's formatter stem from this approach.

### Alternative C: Concrete Syntax Tree

Represent every token including comments as a tree node.

**Rejected:** Major rearchitecture of the parser and all downstream consumers. Every
tree walk must handle comment siblings. Appropriate if Beamtalk adopts tree-sitter;
premature without it.

### Alternative D: Preserve Token Stream Alongside AST (TypeScript/Roslyn style)

Keep the AST unchanged. Preserve the token stream from the lexer alongside the AST.
The formatter reconstructs comments by walking tokens using their positions relative
to AST node spans.

This is the approach used by TypeScript, Roslyn (C#), and rust-analyzer. It has zero
AST churn and zero impact on downstream consumers (codegen, lint, LSP).

**Rejected:** Fails the synthesised-AST use case. A class created in a live tool has
no token stream — the formatter cannot emit comments because there are no tokens to
walk. For formatting existing source files this approach works well, but Beamtalk's
persistence model requires unparsing ASTs that may never have been parsed from source.
Additionally, the token stream duplicates span information already in the AST, creating
a synchronisation surface (not dual *storage* of comments, but dual *representation* of
source structure). The `ast_walker` and lint passes would still need to be
comment-unaware, meaning comment-dependent tooling (the formatter) uses a fundamentally
different data path than comment-independent tooling (the linter) — a split that
undermines "tooling == compiler."

### Alternative E: Do Nothing / Keep Comments in Trivia Only

Keep the current behaviour. Tools that need comments re-lex the source.

**Rejected:** Directly violates "tooling == compiler". Means the formatter would need
a separate parse, two data structures for the same file, and complex synchronisation.
The exact failure mode that Go and Elixir are trying to escape from.

## Consequences

### Positive
- The formatter / unparser can round-trip `.bt` files losslessly without a secondary parse
- Live tools can synthesise new AST nodes with comments attached as plain data
- One parse serves all consumers: compiler, LSP, formatter, linter, live tools
- `CommentAttachment` is a natural extension of the existing `doc_comment` pattern (ADR 0008)
- Codegen is unaffected — it skips `CommentAttachment` fields silently

### Negative
- **Significant AST churn.** Method bodies and block bodies change from `Vec<Expression>`
  to `Vec<ExpressionStatement>`. Of the 14 `Vec<Expression>` declarations in `ast.rs`,
  **6 are statement-position fields** that change (`Module.expressions`,
  `ClassDefinition.expressions`, `MethodDefinition.body` × 3 variants, `Block.body`).
  The remaining ~4 (`MessageSend.arguments`, list/array `elements`, etc.) are
  sub-expression positions and stay as `Vec<Expression>`.
  Downstream, 35 files contain ~190 `.body` references across codegen, lint, LSP,
  semantic analysis, and tests. All pattern matches and iteration at statement positions
  must unwrap `ExpressionStatement` to reach `Expression`. The `ast_walker` module
  (BT-961) must be updated to traverse `ExpressionStatement`. Test helpers that
  construct `body: vec![expr1, expr2]` gain boilerplate wrapping.
- Parser must attach comments during construction rather than discarding trivia — moderate
  complexity increase
- `CommentAttachment` adds fields to `ClassDefinition` and `MethodDefinition`
- **Memory overhead.** Each `ExpressionStatement` adds ~32 bytes
  (`Vec<Comment>` fat pointer + `Option<Comment>`) even when no comments are present.
  Consider `Option<Box<CommentAttachment>>` (8 bytes when None) if profiling shows this
  matters; the decision defers optimisation to implementation.

### Neutral
- `Module.leading_comments` is replaced by leading comments on the first item in the module — equivalent semantics, different location
- `ExpressionStatement` is a new wrapper type; it does not affect language semantics
- Comments remain invisible to the codegen pipeline
- **Error recovery:** Near `Expression::Error` nodes (from parse errors), comment
  attachment proceeds normally — comments attach to the nearest valid node. If a parse
  error prevents node construction, orphaned comments attach to the error recovery node.
  This is no worse than the current behaviour (comments are discarded entirely).
- **REPL scope boundary:** REPL input is parsed as a standalone expression sequence.
  Comments entered in the REPL attach to `ExpressionStatement` nodes within that
  sequence. They do not persist across REPL evaluations (the REPL evaluates and discards
  the AST). This is correct — REPL comments are transient.

## Implementation

### Phase 1: AST Types (S)
- Add `CommentAttachment` struct to `ast.rs`
- Add `ExpressionStatement` wrapper with convenience constructor (`ExpressionStatement::bare(expr)`)
- Update `ClassDefinition` and `MethodDefinition` with `comments: CommentAttachment`
- Add `comments: CommentAttachment` and `doc_comment: Option<String>` to
  `StateDeclaration` (state fields need both `//` and `///` captured at parse time)
- Change `Vec<Expression>` to `Vec<ExpressionStatement>` in statement-position fields
  (~6 fields in `ast.rs`: `Module.expressions`, `ClassDefinition.expressions`,
  `MethodDefinition.body` × 3 variants, `Block.body`)
- Update `Module` to remove `leading_comments` (moved to first-item leading)

**Affected:** `crates/beamtalk-core/src/ast.rs`

### Phase 2: Parser (M)
- Extend `collect_comment_attachment()` to handle `LineComment` and `BlockComment` trivia
  (extending the existing `collect_doc_comment()` pattern)
- Attach `CommentAttachment` when constructing `ExpressionStatement` nodes
- Attach `CommentAttachment` and collect `doc_comment` when constructing
  `ClassDefinition`, `MethodDefinition`, and `StateDeclaration`
- Codegen skips `StateDeclaration.doc_comment` (no runtime target yet — deferred to
  `FieldDescriptor` ADR)
- Update `parse_module()` to remove `leading_comments` collection
- Ensure `///` doc comments are **not** duplicated into `CommentAttachment` (dedup rule)

**Affected:** `crates/beamtalk-core/src/source_analysis/parser/mod.rs`

### Phase 3: Downstream Updates (L)

This is the largest phase by volume. ~35 files reference `.body` on methods/blocks and
must unwrap `ExpressionStatement`. Key subsystems:

- Update `ast_walker` module (BT-961) to traverse `ExpressionStatement`
- Update codegen to traverse `Vec<ExpressionStatement>` (skip `comments` field)
- Update all 6 lint passes to traverse `ExpressionStatement`
- Update semantic analysis validators
- Update LSP providers
- Update all AST construction in tests (use `ExpressionStatement::bare()` helper)

**Affected:** `crates/beamtalk-core/src/ast_walker.rs`,
`crates/beamtalk-core/src/codegen/`, `crates/beamtalk-core/src/lint/`,
`crates/beamtalk-core/src/source_analysis/`, `crates/beamtalk-lsp/src/`

### Phase 4: Unparser / Formatter (L)
- Implement `unparse_expression_statement()` emitting leading and trailing comments
- Implement full AST → source unparser for all node types
- Implement `beamtalk fmt` CLI command using the unparser
- Add `fmt-beamtalk` target to `Justfile`
- Round-trip validation: parse → unparse → parse must produce equivalent ASTs

**Affected:** `crates/beamtalk-core/src/unparse/` (new module),
`crates/beamtalk-cli/src/commands/fmt.rs` (new command)

## Migration Path

This is an internal compiler change. There is no change to Beamtalk syntax or
semantics. Existing `.bt` files are unaffected. The migration is entirely within the
Rust codebase:

- All `Vec<Expression>` in statement positions become `Vec<ExpressionStatement>`
- Pattern matches on method/class nodes gain a `comments` field (ignored with `..` in
  existing code until consumers are updated)
- The compiler, linter, and LSP must update traversal code but produce identical output

A linked ADR will cover **dirty/clean class tracking** in the workspace — knowing which
in-memory class representations have been modified since last written to disk, so
persistence can be selective.

A second linked ADR will cover **`FieldDescriptor` objects** — first-class runtime
objects (consistent with `Class` and `CompiledMethod`) that expose `.name`, `.doc`,
`.typeAnnotation`, and `.defaultValue` via message sends. That ADR will define:
- The compilation target for `StateDeclaration.doc_comment` and `StateDeclaration.type_annotation`
- Runtime storage of type annotations on `CompiledMethod` (parameter types, return type) for consistency
- The `MyClass fields` accessor API returning a collection of `FieldDescriptor` instances

Until that ADR lands, field doc comments and type annotations are collected at parse
time and available throughout the AST pipeline (formatter, LSP, type checker) but not
compiled to runtime objects.

## References

- Related issues: BT-962 (lint cleanup epic), BT-963–966 (lint violation fixes)
- Related ADRs:
  - ADR 0008 — Doc Comments and API Documentation (established `doc_comment: Option<String>` on AST nodes)
  - ADR 0033 — Runtime-Embedded Documentation (superseded ADR 0008's EEP-48 chunk generation; `///` now compiles to `doc:` message sends populating `CompiledMethod.doc` at load time)
  - ADR 0018 — Document Tree Code Generation (Wadler-Lindig; unparser will use the same `Document` API)
  - ADR 0024 — Static-First, Live-Augmented IDE Tooling ("tooling == compiler" principle)
  - ADR 0035 — Field-Based Reflection API (renamed instVar→field; `FieldDescriptor` is the natural next step)
  - Future ADR: `FieldDescriptor` objects — runtime compilation target for `StateDeclaration.doc_comment`
- Prior art:
  - [Pharo RBComment discussion](http://forum.world.st/Why-RBComment-is-not-RBCommentNode-td5094575.html)
  - [Gleam formatter source](https://github.com/gleam-lang/gleam/blob/main/compiler-core/src/format.rs)
  - [Go issue #20744: Free-floating comments](https://github.com/golang/go/issues/20744)
  - [Elixir `Code.string_to_quoted_with_comments/2`](https://hexdocs.pm/elixir/Code.html)
  - [Newspeak Language Specification](https://newspeaklanguage.org/spec/newspeak-spec.pdf)
