# ADR 0089: Typed Document Leaves for Core Erlang Codegen

## Status

**Accepted (2026-05-28).** Long-term fix for the BT-875 recurrence
vector, replacing the withdrawn Phases 1–4 of
[ADR 0088](0088-direct-cerl-emission.md). Acceptance authorises the
flag-day migration described in *Implementation* below.

## Implementation Tracking

**Epic:** [BT-2319](https://linear.app/beamtalk/issue/BT-2319) — Typed Document Leaves Refactor (ADR 0089)

| Phase | Issue | Description | Status |
|---|---|---|---|
| 1 | [BT-2320](https://linear.app/beamtalk/issue/BT-2320) | Add `document::leaf` API + migration lint | Backlog |
| 1 | [BT-2321](https://linear.app/beamtalk/issue/BT-2321) | Add `unparse::leaf` API + migrate `unparse/mod.rs` | Backlog |
| 2 | [BT-2322](https://linear.app/beamtalk/issue/BT-2322) | Migrate leaf utilities + small codegen files | Backlog |
| 2 | [BT-2323](https://linear.app/beamtalk/issue/BT-2323) | Migrate `repl/codegen.rs` | Backlog |
| 2 | [BT-2324](https://linear.app/beamtalk/issue/BT-2324) | Migrate `expressions.rs` | Backlog |
| 2 | [BT-2325](https://linear.app/beamtalk/issue/BT-2325) | Migrate `intrinsics.rs` | Backlog |
| 2 | [BT-2326](https://linear.app/beamtalk/issue/BT-2326) | Migrate `value_type_codegen.rs` | Backlog |
| 2 | [BT-2327](https://linear.app/beamtalk/issue/BT-2327) | Migrate `dispatch_codegen.rs` | Backlog |
| 2 | [BT-2328](https://linear.app/beamtalk/issue/BT-2328) | Migrate `gen_server/` | Backlog |
| 2 | [BT-2329](https://linear.app/beamtalk/issue/BT-2329) | Migrate `control_flow/` | Backlog |
| 2 | [BT-2330](https://linear.app/beamtalk/issue/BT-2330) | Migrate `mod.rs` + remaining | Backlog |
| 3 | [BT-2331](https://linear.app/beamtalk/issue/BT-2331) | Flag-day: remove `Document::String` and `Document::Eco` | Backlog |

**Recommended start:** BT-2320 (Phase 1, no dependencies).

## Context

### Why this ADR exists

[ADR 0018](0018-document-tree-codegen.md) established the Wadler-Lindig
`Document` tree as the only sanctioned way to construct Core Erlang
output. The rule in `CLAUDE.md` is unambiguous:

> All Core Erlang codegen MUST use `Document` / `docvec!` API. **NEVER**
> use `format!()` or string concatenation to produce Core Erlang
> fragments — not even for "simple" atoms, arities, or map keys.

That discipline holds at the *combinator* level. The renderer can no
longer be bypassed; everything passes through `Document`. But the
`Document` enum still has a typed escape hatch:

```rust
pub enum Document<'a> {
    Str(&'a str),
    String(String),   // ← BT-875 recurrence vector
    Eco(ecow::EcoString),
    Line, Nest(..), Vec(..), Group(..), Break {..}, Nil,
}
```

`Document::String(...)` accepts arbitrary text. An author who reaches for
`Document::String(format!("'{name}'"))` or
`Document::String(name.replace('\'', "\\'"))` produces well-formed
combinator code that still ships a manually-rendered Core Erlang
fragment. [BT-875](https://linear.app/beamtalk/issue/BT-875) is the
running history of this class of bug. The CLAUDE.md prohibition catches
it during review; the type system does not.

### What [ADR 0088](0088-direct-cerl-emission.md) was going to do, and why it isn't

ADR 0088 proposed replacing the `Document` tree with a typed `cerl::Expr`
AST shipped over the Port as ETF. That closes BT-875 *structurally* —
there is no leaf that accepts a raw string — and unlocks annotation
fidelity for downstream consumers (BT-aware stack traces, debugger
support). The cost is a 58K-LOC migration across ~55 codegen files.

Phase 0 of ADR 0088 measured the trade-off:

- **Phase 0a** ([memo](0088-phase-0a-audit.md)): projected ~9.5% char
  shrinkage from cerl-direct on three representative functions.
  Qualified — between the 5% withdraw gate and the 15% proceed gate.
- **Phase 0b** ([memo](0088-phase-0b-napkin.md)): ETF cost is 2.4–3.4%
  of per-compile time; the cerl wire is ~7.7–10% faster than the text
  wire end-to-end on the same fixtures. Not nearly enough to drive the
  decision by itself.
- **Phase 0c** ([memo](0088-phase-0c-typed-leaves.md)): the
  typed-Document-leaves alternative captured **−8.7%** aggregate char
  shrinkage on the same three functions vs cerl's **−9.5%** — within
  0.8% on the aggregate, ahead on 2 of 3 function tiers, at roughly
  **1/30th the migration cost ratio**.

Phase 0c's recommendation, accepted in
[PR #2352](https://github.com/jamesc/beamtalk/pull/2352), withdrew
ADR 0088 Phases 1–4 in favour of typed-Document-leaves. This ADR turns
that recommendation into a buildable plan.

### The BT-875 recurrence vector this refactor closes

Today an author can ship any of these:

```rust
docvec!["{'", Document::String(name), "', '", Document::String(super_), "'}"]
docvec!["call '", Document::String(module), "':'", Document::String(fun), "'"]
docvec!["let ", Document::String(var), " = ", body, " in ", ...]

// Same vector through Document::Eco — live today in actor_codegen.rs et al.:
docvec!["module '", Document::Eco(self.module_name.clone()), "' [", ...]
docvec!["'", Document::Eco(method.selector.name()), "'/", ...]
```

In each case the author has manually rendered atom-quote or
variable-name punctuation around a typed-but-text-shaped leaf
(`String` or `Eco`). The leaf carries no semantic information; the
punctuation has to be re-typed at every site; mistakes are typos
rather than type errors. BT-875's eight cleanup commits all share
this shape.

Replacing the open `String` leaf with a typed-leaf API —
`atom(name)`, `var(name)`, `string_lit(s)`, `int_lit(i)`, `float_lit(f)`,
`fname(name, arity)` — makes the leaf carry intent and makes the
punctuation a property of the helper, not of the call site:

```rust
docvec!["{", atom(name), ", ", atom(super_), "}"]
docvec!["call ", atom(module), ":", atom(fun), "(", join(args, ", "), ")"]
docvec!["let ", var(name), " = ", body, " in ", ...]
```

`Document::String` is then **removed from the enum**, the BT-875 vector
becomes unrepresentable, and the renderer is unchanged.

### Out-of-tree consumers of `Document`

The `Document` enum is also used by ~2 modules outside the
`codegen/core_erlang/` tree:

- `crates/beamtalk-core/src/repl/codegen.rs` (~19 `Document::String`
  sites) — REPL-specific Core Erlang codegen. The leaf shapes match
  the core_erlang set (variable names dominate, all `var()`
  candidates). **In scope** for the migration; treated as part of
  Phase B.
- `crates/beamtalk-core/src/unparse/mod.rs` (~65 `Document::String`
  sites) — the *Beamtalk source* unparser, not a Core Erlang
  emitter. It produces Beamtalk syntax (where atoms don't need
  Core Erlang quoting, and the BT-875 vector doesn't apply).
  **Out of scope** for the BT-875-driven typed-leaf API — the
  unparser's leaves carry different semantics.

The Beamtalk-side unparser nonetheless depends on the
`Documentable<'a> for String` impl. To avoid breaking it during
Phase B, the unparser migrates to construct its `Document<'static>`
leaves through a small parallel API (`unparse::leaf`) that wraps
`Document::Str` for compile-time fragments and exposes a
`pub(crate) Document::Owned(String)` constructor for runtime-derived
text. The unparse-leaf API is intentionally separate from
`document::leaf` because the escaping rules differ (no atom-quote
ceremony, no Core Erlang string escaping). Implementation epic must
schedule unparse migration before the `Documentable<'a> for String`
impl is removed.

### `Document::Eco` — the sibling escape hatch

`Document::Eco(ecow::EcoString)` carries the same BT-875 risk as
`Document::String`. There are ~57 production `Document::Eco(...)` call
sites in the codegen tree, used primarily for module names and selector
names passed from the AST as `EcoString` values. The migration must
cover both variants.

`Document::Str(&'static str)` is *not* at risk. Its ~467 uses are
compile-time string literals (fixed Core Erlang fragments like `"\n"`,
`", "`, `"'dispatch'/3"`). An author cannot inject dynamic content
through a `&'static str` — the Rust type system prevents it. `Str`
remains as the only text-leaf variant after the migration.

### Constraints

- **ADR 0018 discipline preserved.** No regression to `format!()` or raw
  string construction; the discipline becomes structural rather than
  conventional.
- **No wire change.** The Port still ships text Core Erlang; the
  Erlang side still calls `core_scan` + `core_parse` + `compile:forms`.
  ADR 0088's cerl-as-wire decision is deferred to a separate,
  smaller, wire-only ADR if compile-time bottlenecks emerge later
  ([Phase 0c memo §"What typed-leaves doesn't give"](0088-phase-0c-typed-leaves.md)).
- **Renderer untouched.** `Document`'s pretty-printer (`Group`,
  `Break`, `Nest`, `Line`) and its ~245 inline codegen unit tests are
  unaffected. Helpers return `Document<'static>` and slot into the
  existing combinator surface.
- **Annotation extensibility.** [ADR 0088](0088-direct-cerl-emission.md)'s
  cerl path was going to carry per-node annotations for BT-aware stack
  traces. Typed-leaves needs its own extension story (sketched below,
  full design deferred to a sibling ADR).

## Decision

**Replace `Document::String` and `Document::Eco` with a typed-leaf API
in a single flag-day migration, then remove both variants from the
enum.**

The seven decisions called out by [BT-2318](https://linear.app/beamtalk/issue/BT-2318):

### 1. Leaf variant set

Seven helpers, all returning `Document<'static>`:

| Helper | Renders as | Example call site today |
|---|---|---|
| `atom(name)` | `'name'` (quoted Core Erlang atom, escaped) | `Document::String(class.name())` inside atom-quotes |
| `var(name)` | `VarName` (bare Core Erlang variable) | `Document::String(var_name.clone())` |
| `string_lit(s)` | `"escaped string"` (escaped Core Erlang string literal) | `Document::String(escape_core_erlang_string(s))` |
| `int_lit(i: i64)` | integer literal | `Document::String(n.to_string())` |
| `float_lit(f: f64)` | float literal in Core Erlang form | rare; today via `Document::String(format!("{:?}", f))` |
| `fname(name, arity)` | `'name'/arity` (function-name / arity pair for remote calls) | `docvec!["'", Document::String(fun), "'/", Document::String(arity.to_string())]` |
| `binary_lit(s)` | Core Erlang binary syntax (`#{#<65>(8,...), ...}#`) for a UTF-8 string | `Document::String(Self::binary_string_literal(s))` (~11 production sites) |

The set is grounded in the survey of ~2,300 `Document::String(...)` call
sites across the codegen tree: dominantly variable names and atom names,
with a long tail of literal numbers, strings, function-arity pairs, and
binary literals. The `binary_lit` helper was added after the initial
audit surfaced ~11 `Document::String(Self::binary_string_literal(...))`
sites in `gen_server/methods.rs`, `expressions.rs`, and
`gen_server/callbacks.rs` — the audit prototype's
`Document::String`-only grep missed them because the
`binary_string_literal` call hides the leaf shape.

**Escaping contract.** `atom(name)` and `string_lit(s)` are responsible
for their own escaping — `atom` calls `escape_atom_chars` internally
(handling `'` → `\'` and `\` → `\\`), `string_lit` calls
`escape_core_erlang_string`. Callers pass raw text. This contract is
load-bearing: it removes the "did I remember to escape this?" decision
from every call site and concentrates it in the helper. Unit tests for
each helper must cover non-trivial inputs (e.g.
`atom("it's").to_pretty_string() == "'it\\'s'"`).

Text-valued helpers (`atom`, `var`, `string_lit`, `fname`) accept
`impl Into<String>` to match the audit prototype's ergonomics. A
`&str`-taking overload may be added during migration if the `.clone()`
overhead is visible in compile-time profiles, but is not part of the
baseline API. `int_lit` takes `i64` (or a wider integer type) and
`float_lit` takes `f64`, so callers cannot pass arbitrary strings
through the numeric helpers.

### 2. Where the API lives

A new submodule `document::leaf`, accessed as:

```rust
use crate::codegen::core_erlang::document::leaf::{atom, var, string_lit, int_lit, float_lit, fname, binary_lit};
```

Rejected alternatives:

- **Free functions alongside `Document::Str` in `document`**: pollutes
  the top-level module surface. `document` already exports the
  combinator surface (`docvec!`, `join`, `nest`, `group`, `break_`);
  adding six leaf helpers next to them obscures the layering
  (combinators vs leaves).
- **New `Document::Atom`, `Document::Var`, ... enum variants**:
  requires renderer changes (each new variant needs a render arm), and
  loses the property that a leaf *is* a `Document` you can build with
  the existing combinator surface. The audit's design — helpers that
  return `Document<'static>` — keeps `Document` itself unchanged.

### 3. `Document::String` and `Document::Eco` deletion strategy

**Remove from public API, flag-day migration.**

A single PR adds the `document::leaf` helpers, migrates all ~2,360 call
sites mechanically, and removes `Document::String` and `Document::Eco`
from the public enum. A `pub(super)` variant `Document::Owned(String)`
survives inside the `document` module as the internal backing for the
typed-leaf helpers — but it is invisible to codegen call sites. No
deprecation window, no parallel paths.

This is the most aggressive of the three options the issue called out
and is chosen for two reasons:

- **BT-875 closure is the entire point of the refactor.** A
  deprecation window leaves the recurrence vector open during the
  migration period; a soft seal leaves it open indefinitely. The Phase
  0c audit's headline result (typed-leaves captures ~30× cost savings)
  is what makes a flag-day affordable.
- **The edit is mechanical.** Each site is a one-token swap once the
  variant is identified (`atom`/`var`/`string_lit`/`int_lit`/`float_lit`/`fname`).
  Variant classification can be partially scripted (atom-quote
  surrounding context, `.to_string()` on a number) and audited site by
  site. There is no structural rewrite — the surrounding `docvec!` and
  combinator code is unchanged.

Trade-off: one large PR carries a heavier review burden than a series
of smaller ones. Mitigated by (a) the mechanical nature of the edit
(reviewers verify the variant classification, not the codegen
correctness), (b) the existing ~245 codegen unit tests asserting on
`to_pretty_string()` output (the byte-for-byte parity check is
automatic), and (c) the full behavioural suite
(`just test-stdlib`/`test-bunit`/`test-repl-protocol`) verifying
end-to-end equivalence.

### 4. Migration ordering

The ADR specifies only the principle. The implementation epic
(via `/plan-adr`) decides per-PR sequencing.

**Principle**: leaf-first by call-site density, batched per file or
small subdirectory so each batch is reviewable independently. Files
with high `Document::String` density (`control_flow/list_ops/transform_ops.rs`
at 438 sites, `control_flow/mod.rs` at 206) are individually large
enough to warrant their own batches; small files (`operators.rs` at 7,
`spec_codegen.rs` at 7) can be combined.

The flag-day-deletion constraint means the final PR in the series
removes `Document::String` only after every site has migrated. The
epic's sequencing must end with a "remove variant" PR that no longer
has any unmigrated call sites to address.

### 5. BT-875 closure proof

**Structural after Phase B — the public-API variants cease to exist.**

Today, the BT-875 vector is open through *both* `Document::String`
and `Document::Eco`. The `Document::Eco(self.module_name.clone())`
pattern embedded between `'...'` atom-quotes is currently live in
multiple files (notably `actor_codegen.rs` and `dispatch_codegen.rs`)
and is structurally identical to the `Document::String(format!("'{name}'"))`
shape that BT-875 catalogs. The CLAUDE.md rule + review process
catch new instances; the type system does not prevent them.

Once Phase B lands and `Document::String` and `Document::Eco` are
removed from the public `Document` enum, no codegen call site can
construct an arbitrary-text leaf. The internal `Document::Owned(String)` variant is
`pub(super)` — visible only to the `document` module and its `leaf`
submodule, where it is wrapped in typed helpers.

`Document::Str(&'static str)` remains public, but it accepts only
compile-time string constants. An author cannot pass a runtime-computed
`&str` to `Document::Str` without unsafe lifetime extension, which
Rust's borrow checker prevents. The BT-875 recurrence vector is
unrepresentable through normal code; the CLAUDE.md prohibition
becomes redundant (kept as documentation of intent).

Two secondary defences:

- **Lint during migration.** While the migration is in flight, an
  in-tree grep check (`just lint-no-new-document-string`) reports any
  *new* `Document::String(...)` or `Document::Eco(...)` site appearing
  in a PR. This prevents the migration's own scope from drifting
  upward as unrelated feature work adds new call sites. Both
  variants are equally BT-875 vectors and both must be blocked.
- **`format!()` ban remains.** The `CLAUDE.md` rule against `format!()`
  for Core Erlang fragments stays. It catches the *other* vector
  (string-concatenation bypass), which is independent of `Document::String`.

The flag-day removal makes the *primary* defence structural; the lint
and the CLAUDE.md rule become belt-and-braces.

### 6. Annotation extensibility (for BT-aware stack traces)

Sketched here; full design deferred to a sibling ADR scheduled with
the BT-aware stack-traces consumer.

**Sketch.** Add a future sibling variant to `Document`:

```rust
pub enum Document<'a> {
    // existing variants...
    Annotated(Annotation, Box<Document<'a>>),
}

pub struct Annotation {
    pub file: Option<EcoString>,
    pub line: Option<u32>,
    pub column: Option<u32>,
    // extensible
}
```

The pretty-printer renders `Document::Annotated(ann, inner)` as just
`inner` (the annotation is transparent to text output). A second
*annotation-collecting* renderer — produced as part of the stack-trace
consumer's work — walks the tree and emits `-file`/`-line` directives
into the text stream at the right boundaries, or records a side-table
of `{module, function, source_line} → {file, line, col}` mappings the
BEAM-side stack-trace decoder consumes.

This is materially weaker than the cerl path's per-node annotations
(which would survive into BEAM bytecode via `debug_info`). The
trade-off was accepted in the [Phase 0c memo](0088-phase-0c-typed-leaves.md);
if the stack-traces consumer concludes that the side-table approach
isn't viable, the cerl-as-wire question reopens as an independent ADR.

### 7. Migration footprint estimate

| Metric | Estimate | Source |
|---|---|---|
| `Document::String(...)` in `codegen/core_erlang/` | **~2,300** | `grep -rEc "Document::String\(" crates/beamtalk-core/src/codegen/core_erlang/` |
| `Document::Eco(...)` in `codegen/core_erlang/` | **~60** | same grep for `Document::Eco(` |
| `Document::String(...)` in `repl/codegen.rs` | **~19** | REPL Core Erlang codegen |
| `Document::String(...)` in `unparse/mod.rs` | **~65** | Beamtalk source unparser — migrates to parallel `unparse::leaf` API (different escaping rules) |
| Total sites in scope for `document::leaf` migration | **~2,380** | core_erlang String + Eco + REPL String |
| Total sites in `unparse::leaf` migration | **~65** | scheduled before `Documentable<'a> for String` impl is removed |
| Files touched | **~36** | core_erlang (~34) + repl/codegen.rs + unparse/mod.rs |
| Aggregate char shrinkage | **−8.7%** | [Phase 0c memo](0088-phase-0c-typed-leaves.md), 3-function projection |
| Helper-library LOC | **~50** | seven functions + doc comments, audit baseline is 10 LOC for two |
| Wire-format changes | **0** | typed-leaves does not touch the Port |
| Erlang-side changes | **0** | typed-leaves does not touch `beamtalk_compiler_server` |

The hot spots by call-site count:

| File | Sites |
|---|---|
| `control_flow/list_ops/transform_ops.rs` | 438 |
| `control_flow/mod.rs` | 206 |
| `dispatch_codegen.rs` | 179 |
| `control_flow/list_ops/search_ops.rs` | 173 |
| `gen_server/methods.rs` | 169 |
| `expressions.rs` | 167 |
| `value_type_codegen.rs` | 136 |
| `intrinsics.rs` | 131 |
| (long tail) | rest |

## Prior Art

The Phase 0c memo's prior-art column was the cerl-direct path itself;
this ADR's prior art is "typed wrappers around a string-buffered
pretty-printer", which is a more common pattern.

| System | Approach | What we adopt / reject |
|---|---|---|
| **Gleam's `Document`** (the ancestor of ours, from which ADR 0018 borrows directly) | Leaves are `String` / `EcoString`; no semantic typing on leaves; identifier escaping handled per-call-site by the codegen author. | Same shape we have today. We're departing from the ancestor for the same reason BT-875 keeps recurring: when the leaf accepts arbitrary text, the author owns the escape boundary. |
| **rustc's `rustc_ast_pretty`** | Wraps `String` buffer with `Printer` API; tokens are typed (`hardbreak`, `space`, `ibox`, `cbox`) but identifiers are still rendered via `word(s)` taking arbitrary text. | Same trade-off as us; rustc gets away with it because rendered identifiers don't cross a syntactic safety boundary (rustc emits Rust source that rustc itself reparses with full diagnostics). For Core Erlang we emit text that `core_parse` rejects with line-number errors, which is exactly where BT-875 bugs hide. |
| **Pharo / Squeak's `Smalltalk Compiler`** | Compiler operates on AST nodes in-image; "codegen text" doesn't exist as a stable artefact — bytecode emission is a method on the node. | The Smalltalk way is "compiler in the image", which collapses the text layer entirely. We can't match this directly (Rust compiler, BEAM runtime). The principle that survives is "leaves carry intent, not arbitrary text"; typed-leaves applies it. |
| **LFE's `lfe_codegen`** | Produces `cerl` records directly; no text layer, no leaf-typing question. | This is the path [ADR 0088](0088-direct-cerl-emission.md) was going to take. Phase 0c data shows it isn't worth ~30× the migration cost for our case. |
| **Elixir's `Macro.to_string`** | Quoted-form-to-string conversion uses typed AST nodes; the inverse direction (text codegen) doesn't exist because Elixir doesn't emit text. | Same as LFE — no text layer. Not a reachable design point for us without rebuilding the wire (ADR 0088). |
| **Pretty-printer libraries in general** (`prettyprinter` Haskell, `pretty` Wadler) | The leaves are strings; semantic typing of leaves is application-specific. | Confirms the typed-leaf API is *application code*, not pretty-printer library code. We add the typed helpers in our crate, not in the Document library. |

**Pattern.** Pretty-printer libraries leave leaf-typing to the
application; languages that emit text codegen either (a) accept the
recurring-typo cost (rustc, Gleam, current Beamtalk), (b) layer typed
helpers on top (this ADR), or (c) skip the text layer entirely
(Pharo, LFE, Elixir). Path (c) is the cleanest but the most expensive;
path (b) is the affordable compromise.

## User Impact

| Persona | Impact |
|---|---|
| **Newcomer** | None — language semantics, REPL output, compiled artefacts unchanged. |
| **Smalltalk developer** | None — language semantics unchanged. |
| **Erlang/BEAM developer** | None — generated Core Erlang text and `.beam` artefacts byte-for-byte identical. |
| **Production operator** | None — no runtime change. Compile time unchanged (no wire change). |
| **Compiler contributor** | Net positive but with adjustment cost. New codegen authors learn `leaf::atom`/`leaf::var`/`leaf::string_lit` instead of reaching for `Document::String`. The escape-boundary class of bug (BT-875) becomes impossible by construction. Compile errors for the variant ("use of `Document::String` is undefined") direct authors to the typed helpers. ~50 lines of new library code to learn; the existing combinator surface (`docvec!`, `join`, `nest`, `group`, `break_`) is unchanged. |
| **Tooling developer** | Mildly positive. The annotation-extension sketch (§6) leaves a path for future BT-aware stack traces without re-opening ADR 0088. |

### Discoverability

The `document::leaf` submodule and its seven helpers are discoverable
via standard `rustdoc` / `cargo doc` browsing and via the `use`
statement at the top of every migrated codegen file. New codegen
contributors who reach for `Document::String` get a compile error
("no variant `String` on `Document`") immediately after the migration
PR lands; the error message can be paired with a `compile_error!`
helper that points at `document::leaf` for one release cycle after
the deletion.

## Steelman Analysis

### Option A: Typed-Leaf Helpers + Flag-Day Removal (Recommended)

- 🧑‍💻 **Newcomer contributor**: "When I add a new codegen function, I never see a `Document::String` in the codebase to copy. The seven helpers in `document::leaf` are the alphabet. I can't accidentally write the BT-875 bug because the variant doesn't exist."
- 🎩 **Smalltalk purist**: "Smalltalk's compiler is in the image precisely so that 'codegen text' isn't a thing you have to escape. We can't match that on BEAM, but we can match the principle: *leaves carry intent, not arbitrary text*. The typed-leaf API is the smallest change that makes that principle structural rather than conventional."
- ⚙️ **BEAM veteran**: "The wire is unchanged. The Erlang side is unchanged. The 245 codegen unit tests asserting on `to_pretty_string()` are unchanged. This is a refactor of one Rust enum and ~2,300 call sites; the rest of the pipeline doesn't know the migration happened."
- 🏭 **Operator**: "BT-875 keeps recurring because the type system permits it. After this ADR ships, the BT-875 class of bug is unrepresentable. No runtime change, no compile-time change, no new failure modes. The operational risk is zero."
- 🎨 **Language designer**: "The compounding cost of ADR 0018's combinator-only discipline + ad-hoc string leaves is the typo-rate at every leaf. Typed leaves move ~80% of that cost into the type system; the remaining ~20% (`format!()`-style raw-string bypass) is already covered by the CLAUDE.md rule. The combination closes the recurrence vector at the cost of ~50 LOC of new helpers."

### Option B: Soft Seal — `Document::String` `pub(super)`, Keep the Variant

- 🧑‍💻 **Newcomer contributor**: "The variant is still there for the rare case where the typed leaves don't cover what I need. I have an escape hatch if I run into a weird Core Erlang fragment that doesn't fit `atom`/`var`/`string_lit`."
- 🎩 **Smalltalk purist**: "Hard-removing a public API is a strong commitment. Soft-sealing it lets us reverse course if the typed-leaf set turns out to be incomplete in production. Smalltalk has always preferred refinement to deletion."
- ⚙️ **BEAM veteran**: "Erlang's habit is to deprecate, not delete. `Document::String` becomes `pub(super)`, the helpers are the public face; nothing breaks if some test or one-off tool needs the raw variant."
- 🏭 **Operator**: "Lower-risk PR. A soft seal can ship in pieces: leaves first, then seal, then optional removal. Each step is independently revertible. A flag-day PR is one big revert."
- 🎨 **Language designer**: "The 'soft seal' is honest about the API boundary — the public surface is the helpers; the variant is an implementation detail. Hard-removing the variant conflates 'what's exported' with 'what exists'."
- 🛠️ **Sharpest argument — Operator (reprise)**: "A flag-day PR that touches ~2,300 sites is at the edge of reviewable. Soft-seal-then-deprecate has a track record (ADR 0018's migration model) of working without coordinated cutovers. Why pay the flag-day cost for a result that's strictly worse than the deprecation path on review effort?"

### Option C: Status Quo — CLAUDE.md Rule + Manual Review

- 🧑‍💻 **Newcomer contributor**: "BT-875 was *already cleaned up*; the current `format!()` count is zero. Whatever review process caught the last round will catch the next round. Refactoring 2,300 sites to prevent a bug class that has zero open instances feels like over-engineering."
- 🎩 **Smalltalk purist**: "Smalltalk values *flexibility* — the ability to express any code shape. Restricting the leaf variants pre-commits us to a leaf taxonomy we may revisit. If the helpers are useful, add them; don't remove the underlying flexibility."
- ⚙️ **BEAM veteran**: "BEAM-language pretty-printers (Gleam's included) all leave leaf-typing as application concern. We're not behind the state of the art."
- 🏭 **Operator**: "Zero refactor cost. Zero migration risk. The CLAUDE.md rule has held since the BT-875 cleanup. Process beats migration."
- 🎨 **Language designer**: "BT-875's recurrence rate has been declining since the original cleanup. The bug class isn't *zero*, but it's small enough that a 2K-site mechanical refactor is disproportionate."
- 🛠️ **Sharpest argument — Compiler architect**: "The whole point of ADR 0018 was that *structured data* prevents string-construction bugs. The fact that BT-875 keeps recurring suggests the problem isn't the type system — it's the *culture* of accepting `Document::String(...)` as the leaf shape. A culture problem doesn't get fixed by another type. The fix is review discipline + CLAUDE.md, both of which are already in place."

### Option D: Full Cerl-Direct Migration (the rejected ADR 0088 path)

Referenced for completeness; not re-litigated. See
[ADR 0088](0088-direct-cerl-emission.md) Status block for the rejection
rationale and [Phase 0c memo](0088-phase-0c-typed-leaves.md) for the
empirical data driving it.

### Tension Points

- **Flag-day vs deprecation window.** Option A's flag-day is harder to
  review than B's gradual seal, but A's result is unambiguously
  BT-875-closed while B's depends on the seal holding (and on someone
  eventually doing the deletion). We chose A because (i) the variant
  classification is mechanical and reviewable by counting site
  categories, (ii) the per-PR alternative has a track record (ADR
  0018, ADR 0088) of leaving long tails, and (iii) the result of A is
  binary — BT-875 either has a recurrence vector or it doesn't, and
  A makes it not.
- **Helper API completeness.** The seven-helper set covers the call-site
  audit's dominant shapes. If a Core Erlang fragment exists that none
  of the six covers, we have to either extend the set or fall back to
  rendering by hand. The flag-day audit must surface every such site
  *before* the deletion PR lands; if any are found, the helper set
  grows before the deletion. This is the load-bearing risk in
  Option A.
- **Annotation extensibility.** §6 sketches a `Document::Annotated`
  variant; the stack-traces consumer is on the hook for the actual
  design. If that consumer concludes side-tables aren't viable, the
  cerl-as-wire ADR reopens. This is a deliberate deferral, not a
  resolved question.

## Alternatives Considered

### 1. Status quo — CLAUDE.md rule + manual review (current BT-875 enforcement)

Keep `Document::String` as-is. Trust the CLAUDE.md rule and code review
to catch new `format!()` / string-concatenation regressions.

**Rejected because:**

- BT-875 has eight historical cleanup commits. The recurrence rate is
  low but non-zero, and each recurrence requires re-establishing the
  invariant across the entire codegen tree.
- The CLAUDE.md rule is convention; the type system permits the
  violation. The Phase 0c memo's typed-leaves audit demonstrated that
  the structural fix costs ~50 LOC of helpers and is achievable in
  a single coordinated PR.
- The "compiler architect" steelman (status quo is enough; the
  problem is cultural) is real but doesn't account for the
  *compounding* cost of every codegen author having to internalise
  the rule. Typed leaves transfer that cost from runtime author
  vigilance to one-time API design.

### 2. `Document::String` and `Document::Eco` as `pub(super)` — soft seal, no removal

Move `Document::String` and `Document::Eco` behind a module-private
visibility so only the `document::leaf` helpers can construct them;
the variants continue to exist as implementation details.

**Rejected because:**

- The variant continues to exist, and the long history of "we'll
  remove this when migration is done" in this codebase (ADR 0018,
  ADR 0088) is that *the removal slips*. Soft sealing without a
  forcing function leaves the recurrence vector ajar.
- Migration effort is identical to the flag-day option — every call
  site still has to migrate to `document::leaf` — but the result is
  weaker (BT-875 not closed structurally).
- The "operator" steelman (lower-risk PR, can ship in pieces) is
  valid; the counter is that the per-PR migration risk is the
  *classification accuracy* (atom vs var vs literal), and that risk
  is identical whether the variant is removed at the end or not. The
  flag-day version pays the review cost once.

### 3. Full sum-type replacement, hard delete (the recommended option, articulated as the alternative for the reader who wants the full reasoning)

Replace `Document::String` with the seven-helper `document::leaf` API and
remove the variant in one PR.

**Accepted as Decision.** See *Decision* §3 above for the full
articulation.

### 4. Full cerl-direct migration (the rejected ADR 0088 path)

Replace the `Document` tree with a typed `cerl::Expr` AST and ship ETF
across the Port.

**Rejected because:**

- [ADR 0088](0088-direct-cerl-emission.md) Phases 1–4 are explicitly
  withdrawn following the
  [Phase 0c memo](0088-phase-0c-typed-leaves.md)'s data: typed-leaves
  captures −8.7% aggregate shrinkage vs cerl's −9.5%, at roughly
  1/30th the migration cost ratio.
- The cerl-as-wire compile-time bonus (~10% from skipping
  `core_scan`+`core_parse`) is deferred to a separate, smaller,
  wire-only ADR if compile-time bottlenecks emerge later.
- BT-aware stack-traces (the original annotation-fidelity motivator
  for ADR 0088) can be served by the §6 annotation-extension sketch
  for typed-leaves. If that turns out to be infeasible in practice,
  the cerl-as-wire ADR can be reopened — but it's no longer
  bundled with the codegen restructure.

### 5. Extend `Document` with typed leaf enum variants directly

Add `Document::Atom(String)`, `Document::Var(String)`, etc. variants to
the enum; the renderer adds new arms for each.

**Rejected because:**

- Requires renderer changes — each new variant needs a render arm
  and a `fits_deque` arm. The audit's helper-functions approach keeps
  the renderer untouched.
- Loses the property that a leaf *is* a `Document` you can build with
  the existing combinator surface. The audit's design — helpers
  returning `Document<'static>` built from existing variants — keeps
  composition uniform.
- Doesn't add semantic power over the helper approach; the helpers
  produce the same rendered output (`'name'` for `atom`, `VarName`
  for `var`) without expanding the enum surface.

## Consequences

### Positive

- **BT-875 closed structurally.** The recurrence vector ceases to
  exist. The CLAUDE.md rule against `format!()` for codegen
  remains as the other half of the invariant, but the typed-leaf
  half is enforced by the type system.
- **Codegen call sites read like intent.** `atom(name)` says "this is
  an atom"; `var(name)` says "this is a Core Erlang variable". The
  punctuation lives in the helper, not at every call site. The
  Phase 0c memo's char-count delta (−8.7%) is approximately the
  amount of atom-quote ceremony that disappears.
- **Renderer, wire, and Erlang side unchanged.** No new operational
  surface. The Port still ships text. `core_scan`/`core_parse` still
  run. All ~245 codegen unit tests assert on `to_pretty_string()`
  output unchanged.
- **Migration cost ~30× cheaper than ADR 0088.** Seven helpers + ~2,300
  mechanical edits + one variant removal, versus 58K LOC of codegen
  restructure plus wire-format change plus Erlang-side migration.
- **Annotation extensibility preserved.** §6 sketches a path for
  BT-aware stack traces that doesn't reopen ADR 0088.

### Negative

- **One large coordinated PR.** ~2,300 sites across ~30 files in a
  single PR is at the edge of reviewable. Mitigated by the
  mechanical nature of the edit (reviewers check variant
  classification, not codegen correctness), the existing
  `to_pretty_string()` byte-for-byte test surface, and the full
  behavioural test suite.
- **Helper-set completeness is load-bearing.** If a call site exists
  that none of the seven helpers covers, the migration either (a)
  extends the helper set before the flag-day, or (b) falls back to
  a temporary `Document::Str(&'static str)` for the static parts of
  that fragment. The flag-day audit must surface any such site
  before the deletion PR lands.
- **No wire-level annotation propagation.** Unlike the cerl path,
  source positions don't survive into BEAM bytecode via cerl's
  native annotation channel. BT-aware stack traces will rely on the
  §6 side-table approach. If that turns out to be infeasible, the
  cerl-as-wire question reopens (as a separate, smaller ADR per the
  [Phase 0c memo](0088-phase-0c-typed-leaves.md)).
- **`int_lit`/`float_lit` format numbers to strings internally.** The
  helpers accept numeric types (`i64`, `f64`) and format via
  `to_string`. Type-safe at the call-site boundary; rendered output is
  deterministic. Negligible risk — number formatting is well-bounded —
  but the rendered string is not structurally validated against Core
  Erlang number grammar.

### Neutral

- **Generated `.beam` artefacts byte-for-byte identical.** Verified
  by an ad-hoc `cmp -l` of compiled fixtures before/after, the
  ~245 codegen unit tests asserting on `to_pretty_string()` output,
  and the full behavioural suite during the flag-day PR.
- **`Document` enum: `String` and `Eco` removed from public API.**
  Renamed to `pub(super) Document::Owned(String)` as the internal
  backing for the typed-leaf helpers (still inside the `document`
  module; invisible outside it). `Eco` is removed entirely (the
  helpers convert `EcoString` to `String` at the boundary; the
  `EcoString` import cost in the renderer disappears). The renderer
  loses one match arm (`Eco`) and renames one (`String` → `Owned`);
  no other change.
- **No new dependencies.** Helpers live in the existing
  `beamtalk-core` crate.

## Implementation

This is a single epic with a flag-day deletion. The `/plan-adr`
output decomposes the implementation; this section names the phases at
the level the ADR commits to.

### Phase A: Helper Library + Lint (S)

- Restructure `document.rs` → `document/mod.rs` (a mechanical move —
  `mod document;` in the parent remains unchanged; all existing
  `use crate::codegen::core_erlang::document::*` paths still resolve).
- Add `crates/beamtalk-core/src/codegen/core_erlang/document/leaf.rs`
  with the seven helpers: `atom`, `var`, `string_lit`, `int_lit`,
  `float_lit`, `fname`, `binary_lit`. Each has doc comments naming the
  rendered Core Erlang form and links to the call-site classes it
  replaces.
  Internally, the helpers construct dynamic text via a
  `pub(super) Document::Owned(String)` variant (renamed from
  `Document::String` to signal its restricted scope) that is not
  exported from the `document` module. External codegen code cannot
  construct `Owned` directly — the typed-leaf helpers are the only
  public path.
- Add unit tests for each helper covering the rendered byte form
  (e.g. `atom("foo").to_pretty_string() == "'foo'"`,
  `string_lit("a\"b").to_pretty_string() == "\"a\\\"b\""`).
- Add `just lint-no-new-document-string` task: greps for
  `Document::String(...)` outside `document::leaf` and the audit
  modules; reports the count. CI uses it to flag new sites
  introduced during migration PRs.

### Phase B: Flag-Day Migration + Variant Removal (L)

A single PR that:

1. Greps every `Document::String(...)` and `Document::Eco(...)` site
   across the codegen tree, classifies it (atom / var / string_lit /
   int_lit / float_lit / fname), and rewrites it to the corresponding
   helper call. The classification is mostly mechanical (surrounding
   atom-quotes ⇒ `atom`, capitalised identifier ⇒ `var`,
   `.to_string()` on a number ⇒ `int_lit`/`float_lit`); ambiguous
   sites get a per-site reviewer note in the PR.
2. Removes `Document::String` and `Document::Eco` from the `Document`
   enum in `document.rs`. Removes the `Documentable<'a> for String`
   and `Documentable<'a> for EcoString` impls. The
   `Documentable<'a> for usize` / `for isize` impls migrate to call
   `int_lit` internally. `Document::Str(&'static str)` stays — it
   can only carry compile-time constants and is not a BT-875 vector.
3. Removes the `cerl_audit.rs` and `typed_leaves_audit.rs` modules
   ([Phase 0c memo](0088-phase-0c-typed-leaves.md) flagged them as
   throwaway).
4. Updates `CLAUDE.md` to reflect the new invariant: the typed-leaf
   API is the only way to introduce a leaf; `Document::String` no
   longer exists. The `format!()` ban remains.

The PR ships green: all ~245 codegen unit tests, the proptest suite
(`core_erlang_validity_tests.rs`), and the full behavioural suite
(`just test-stdlib`/`test-bunit`/`test-repl-protocol`) pass. Generated
`.beam` artefacts are byte-for-byte identical to the pre-PR baseline.

### Phase C: Helper Refinements (S, ongoing)

After Phase B lands, opportunistic refinements as feature work
exercises the helpers:

- Add `&str`-taking overloads if `.into()` overhead is visible in
  compile profiles.
- Add helpers for newly-discovered leaf classes if future codegen
  needs them (e.g. `bitstring_lit` for raw binary segments). The
  seven-helper set covers all currently-known shapes; future
  additions are additive.
- Refine error messages for accidental `String`-passing.

### Phase D: Annotation Extension (Deferred — sibling ADR)

When the BT-aware stack-traces consumer is ready to ship, the
`Document::Annotated(Annotation, Box<Document>)` variant + the
annotation-collecting renderer + the side-table format land as a
sibling ADR. Not part of this epic.

### Affected Components

- **New**: `crates/beamtalk-core/src/codegen/core_erlang/document/leaf.rs`
  — seven helpers plus unit tests.
- **Modified**: `crates/beamtalk-core/src/codegen/core_erlang/document.rs`
  — remove `Document::String` and `Document::Eco` variants, remove
  `Documentable<'a> for String` and `Documentable<'a> for EcoString`
  impls, update `Documentable for usize/isize` to delegate to
  `int_lit`. `Document::Str(&'static str)` remains as the only
  text-leaf variant. Renderer unchanged (fewer match arms).
- **Modified**: every file under
  `crates/beamtalk-core/src/codegen/core_erlang/` containing
  `Document::String(...)` or `Document::Eco(...)` — ~34 files,
  ~2,360 sites, all mechanical.
- **Modified**: `crates/beamtalk-core/src/repl/codegen.rs` —
  ~19 `Document::String(...)` sites (all variable names, all
  `var()` candidates).
- **Modified**: `crates/beamtalk-core/src/unparse/mod.rs` — migrate
  ~65 `Document::String(...)` sites to a parallel `unparse::leaf`
  API (separate from `document::leaf`; different escaping semantics).
  Must precede the `Documentable<'a> for String` impl removal.
- **Removed**: `crates/beamtalk-core/src/codegen/core_erlang/cerl_audit.rs`
  and `typed_leaves_audit.rs` (throwaway audit modules).
- **Modified**: `CLAUDE.md` — note that typed-leaf API is the only
  way to introduce a leaf; `Document::String` no longer exists.
- **New**: `just lint-no-new-document-string` task — grep-based
  enforcement during migration; can be removed after Phase B ships
  (since the variant is gone).
- **Unchanged**: Erlang side (`beamtalk_compiler_server`,
  `beamtalk_build_worker`, `beamtalk_compiler_port`), wire format,
  ETF, Port boundary, all renderer behaviour.

### Verification

- **Unit tests**: the ~245 inline codegen unit tests in
  `crates/beamtalk-core/src/codegen/core_erlang/tests/` assert on
  `to_pretty_string()` output. They are unchanged and must pass
  byte-for-byte. The helper-library unit tests added in Phase A
  assert each helper's rendered form.
- **Proptest**: `core_erlang_validity_tests.rs`
  (parseability of generated text) runs on the post-migration code
  and must pass.
- **Behavioural suite**: `just test-stdlib`,
  `just test-bunit`, `just test-repl-protocol` run on the
  post-migration code and must pass.
- **Codegen-diff check**: the byte-for-byte parity of `.beam`
  artefacts is verified ad-hoc during Phase B by compiling a fixed
  fixture set (`stdlib/test/*.bt`) before and after, then `cmp -l`-ing
  the outputs. ADR 0088 contemplated a permanent `just codegen-diff`
  task but it was never landed; this ADR does not require it as a
  standing harness (the unit-test byte-for-byte `to_pretty_string()`
  assertions plus the behavioural suite are sufficient).

## References

- Related issues:
  - [BT-2318](https://linear.app/beamtalk/issue/BT-2318) — this ADR
  - [BT-875](https://linear.app/beamtalk/issue/BT-875) — the recurrence
    vector this ADR closes structurally
  - [BT-2316](https://linear.app/beamtalk/issue/BT-2316) — the Phase 0c
    audit that drove the recommendation
  - [BT-2313](https://linear.app/beamtalk/issue/BT-2313) — ADR 0088
    Phase 0 epic (parent of BT-2316)
- Related ADRs:
  - [ADR 0018](0018-document-tree-codegen.md) — the `Document`
    combinator surface this ADR refines (preserved; only the leaf
    shape changes)
  - [ADR 0088](0088-direct-cerl-emission.md) — the cerl-direct
    proposal whose Phases 1–4 this ADR supersedes
- Audit memos:
  - [Phase 0a — Codegen shrinkage audit](0088-phase-0a-audit.md)
  - [Phase 0b — Wire-mechanism napkin](0088-phase-0b-napkin.md)
  - [Phase 0c — Typed-leaves comparison](0088-phase-0c-typed-leaves.md)
- Audit prototype:
  `crates/beamtalk-core/src/codegen/core_erlang/typed_leaves_audit.rs`
  (10 LOC of helpers + 3 rewrites; the starting point for the
  seven-helper API)
- Current Document API:
  `crates/beamtalk-core/src/codegen/core_erlang/document.rs`
- CLAUDE.md codegen rule (enforced structurally by this ADR):
  *"All Core Erlang codegen MUST use `Document` / `docvec!` API. **NEVER**
  use `format!()` or string concatenation to produce Core Erlang
  fragments."*
