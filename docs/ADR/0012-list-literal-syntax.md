# ADR 0012: Collection Literal Syntax and the `#` Data Literal System

## Status
Accepted (2026-02-08)

## Context

Beamtalk currently has no way to express list literals in source code. The language has map literals (`#{key => value}`), symbol literals (`#name`), and block literals (`[body]`), but no collection literal for ordered sequences.

This blocks several features:
- **`perform:withArgs:`** requires passing a list of arguments (BT-172)
- **Collection iteration E2E tests** need list data to iterate over (BT-72)
- **Standard library collection classes** need literal syntax (BT-331)
- **Pattern matching** on lists needs corresponding literal syntax

### Current State

**Partial implementation exists but is disconnected:**
- AST: `Literal::Array(Vec<Literal>)` variant exists, documented as `#(1 2 'three')` (Smalltalk-style)
- Codegen: `generate_literal` handles `Literal::Array` → Erlang list `[1, 2, 3]`
- Semantic analysis: `ClassHierarchy` registers both Array (11 methods) and List (13 methods)
- Control flow: Full list iteration codegen (`do:`, `collect:`, `select:`, `reject:`, `inject:into:`)
- **No lexer/parser support**: No syntax to create these values from Beamtalk source

### The `[...]` Overloading Problem

Beamtalk uses `[...]` for block (closure) literals:
```beamtalk
[42]                    // Block returning 42
[:x | x + 1]           // Block with parameter
[self doSomething]      // Block calling a method
```

Using `[1, 2, 3]` for lists would create ambiguity:
- `[42]` — block or single-element list?
- `[]` — empty block or empty list?

While commas could disambiguate multi-element cases (`[1, 2, 3]` vs `[expr. expr]`), the single-element and empty cases require either trailing commas (`[42,]`), breaking changes (`[]` = list), or type-system-level semantic analysis to warn on misuse. This complexity is avoidable.

### The `#` Prefix Already Means "Data"

Beamtalk already uses `#` as a data literal prefix:
- `#symbol` — atom literal
- `#'quoted symbol'` — atom with spaces
- `#{key => value}` — map/dictionary literal

This isn't coincidental — `#` consistently marks "this is literal data, not executable code." Extending this principle to collection literals creates a coherent, unambiguous design system.

### Constraints

1. **Zero ambiguity**: No parser disambiguation, no semantic analysis needed to catch misuse
2. **Interactive-first**: Must work naturally in REPL sessions
3. **Backward compatible**: Existing block syntax `[...]` must be completely unchanged
4. **Composable**: Must work in method arguments, assignments, and pattern matching
5. **Extensible**: Design should accommodate future literal types (tuples, regex, binaries)

## Decision

**Use `#(expr, expr, ...)` for list literals. The `#` prefix marks all data literals in Beamtalk, forming a coherent family: `#symbol`, `#{map}`, `#(list)`.**

### Syntax

```beamtalk
// List literals
empty := #()                       // Empty list
numbers := #(1, 2, 3)             // List of integers
mixed := #(1, "hello", #ok)       // Heterogeneous list
nested := #(#(1, 2), #(3, 4))    // Nested lists
single := #(42)                   // Single-element list — unambiguous!

// Cons syntax (prepend)
prepended := #(0 | numbers)       // #(0, 1, 2, 3)

// Blocks — completely unchanged
block := [42]                     // Block returning 42
adder := [:x | x + 1]            // Block with parameter
multi := [x := 1. y := 2. x + y] // Multi-statement block
```

### The `#` Data Literal Family

`#` followed by a delimiter creates a data literal. The delimiter determines the type:

| Syntax | Type | BEAM Type | Status |
|--------|------|-----------|--------|
| `#symbol` | Symbol | atom | ✅ Implemented |
| `#'quoted'` | Symbol | atom | ✅ Implemented |
| `#{k => v}` | Dictionary | map | ✅ Implemented |
| `#(1, 2, 3)` | List | linked list | **This ADR** |

**The rule:** `#` means data. Bare delimiters mean code.

| Bare | Hash-prefixed |
|------|---------------|
| `(expr)` — grouping | `#(1, 2, 3)` — list literal |
| `[body]` — block/closure | — |
| — | `#{k => v}` — map literal |

### Future Data Literals (Not In This ADR)

This design naturally extends to other literal types. These are **not decided here** but shown to demonstrate the system's extensibility:

| Syntax | Type | BEAM Type | Precedent |
|--------|------|-----------|-----------|
| `{1, 2, 3}` | Array | tuple (tagged) | Erlang `{A, B, C}`, Smalltalk Array |
| `#/pattern/` | Regex | compiled regex | Clojure `#""`, Ruby `%r{}` |
| `#r"raw string"` | Raw string | binary | Python `r""`, Rust `r""` |
| `#b<<1, 2>>` | Binary | bitstring | Erlang `<<1,2>>` |

**Array literals** (`{1, 2, 3}`) require a separate ADR to address the Array vs Tuple distinction — user-created arrays (data structures) should be a different class from raw Erlang tuples (FFI interop results like `{ok, Value}`). The likely approach is runtime tagging (`{beamtalk_array, {...}}`) following the same pattern as `beamtalk_object`, but this has significant implementation implications (~10 files, ~125 lines) that deserve their own design review. See "Future Work" section below.

Each future literal type requires its own ADR when the time comes.

### REPL Session

```
> #(1, 2, 3)
#(1, 2, 3)

> numbers := #(10, 20, 30)
#(10, 20, 30)

> numbers head
10

> numbers tail
#(20, 30)

> #(0 | numbers)
#(0, 10, 20, 30)

> numbers collect: [:n | n * 2]
#(20, 40, 60)

> numbers select: [:n | n > 15]
#(20, 30)

> #()
#()

> #(42) head
42

> obj perform: #add:to: withArgs: #(3, 4)
7
```

### Error Examples

```
> #(1, 2, 3) value
ERROR: List does not understand 'value'
  Hint: 'value' is a Block method. Lists use head, tail, size, collect:, etc.

> (1, 2, 3)
ERROR: Expected ')' after expression, found ','
  Hint: For a list literal, use #(1, 2, 3) — note the # prefix.

> numbers at: 5
ERROR: Index 5 out of bounds for list of size 3
  Hint: List indices start at 1 and go up to 3

> [1, 2, 3]
ERROR: Unexpected ',' in block body
  Hint: For a list literal, use #(1, 2, 3). Blocks use [body] or [:x | body].
```

### BEAM Mapping

| Beamtalk | Erlang | BEAM Type | Class |
|----------|--------|-----------|-------|
| `#(1, 2, 3)` | `[1, 2, 3]` | Linked list (cons cells) | List |
| `#()` | `[]` | Empty list | List |
| `#(h \| t)` | `[H \| T]` | Cons cell | List |
| `#{a => 1}` | `#{a => 1}` | Map | Dictionary |
| `#symbol` | `symbol` | Atom | Symbol |

## Prior Art

### Smalltalk Family

| Language | Literal Array | Dynamic Array | List |
|----------|--------------|---------------|------|
| **Pharo/Squeak** | `#(1 2 3)` — compile-time, immutable | `{1. 2. 1+2}` — runtime, mutable | `OrderedCollection` — no literal |
| **Newspeak** | Similar to Pharo | Similar to Pharo | No literal syntax |
| **GNU Smalltalk** | `#(1 2 3)` | `{1. 2. 3}` | No literal syntax |

Smalltalk uses `#(...)` with space-separated elements. Beamtalk modernizes this with comma separation while keeping the `#` prefix. The continuity is deliberate — Smalltalk developers will recognize `#(...)` as "collection literal."

### BEAM Languages

| Language | List Syntax | Type | Pattern Match |
|----------|------------|------|---------------|
| **Erlang** | `[1, 2, 3]` | Linked list | `[H \| T]` |
| **Elixir** | `[1, 2, 3]` | Linked list | `[h \| t]` |
| **Gleam** | `[1, 2, 3]` | Linked list | `[first, ..rest]` |
| **LFE** | `'(1 2 3)` or `(list 1 2 3)` | Linked list | Pattern match |
| **Beamtalk** | `#(1, 2, 3)` | Linked list | `#(head \| tail)` |

Beamtalk's `#(...)` differs from other BEAM languages' `[...]` by one prefix character. This is a deliberate trade-off: the `#` prefix buys zero ambiguity, zero breaking changes, and a coherent literal system.

### Languages Using Prefix for Data Literals

| Language | Prefix System | Examples |
|----------|--------------|---------|
| **Clojure** | `#` dispatch | `#{}` set, `#""` regex, `#inst` tagged literal |
| **Elixir** | `~` sigils | `~r//` regex, `~w[]` word list, `~c''` charlist |
| **Ruby** | `%` sigils | `%w[]` word array, `%i[]` symbol array, `%r{}` regex |
| **Python** | Letter prefix | `r""` raw, `b""` bytes, `f""` format |
| **Rust** | Letter prefix | `r""` raw, `b""` bytes, `br""` raw bytes |

Beamtalk's `#` prefix is most similar to **Clojure's dispatch character** — a single character that says "the next delimiter creates data, not code." The difference: Clojure uses `#()` for anonymous functions, while Beamtalk keeps `[block]` for closures and uses `#()` for lists.

### Other Languages with Block/List Ambiguity

| Language | Blocks | Lists | Resolution |
|----------|--------|-------|------------|
| **Ruby** | `{ \|x\| x + 1 }` or `do...end` | `[1, 2, 3]` | Different delimiters |
| **Elixir** | `fn x -> x + 1 end` | `[1, 2, 3]` | Different syntax entirely |
| **Swift** | `{ x in x + 1 }` | `[1, 2, 3]` | Different delimiters |
| **Beamtalk** | `[body]` | `#(1, 2, 3)` | Prefix distinguishes |

Most languages avoid the problem by using different delimiters for blocks and lists. Beamtalk achieves this via the `#` prefix rather than a wholly different bracket type.

## User Impact

### Newcomer (from Python/JS/Ruby)
`#(1, 2, 3)` has a small learning cost — one extra character compared to `[1, 2, 3]`. However, the `#` prefix is immediately learnable: "hash means data." The comma-separated elements inside are familiar. No trailing-comma gotchas, no ambiguity surprises.

### Smalltalk Developer
`#(1, 2, 3)` will feel natural — it's Smalltalk's `#(1 2 3)` with commas. The `#` prefix meaning "literal data" carries over directly. Blocks remain untouched as `[body]`.

### Erlang/Elixir Developer
One prefix character different from Erlang's `[1, 2, 3]`. Maps already use `#{}` in both languages, so `#()` for lists is a consistent extension. Pattern matching with `#(head | tail)` maps cleanly to Erlang's `[H | T]`.

### Production Operator
Lists compile to standard Erlang linked lists — fully observable in Observer, compatible with `:recon`, appear naturally in crash dumps. No wrapper types, no indirection. The `#()` syntax is purely a source-level convention.

### Tooling Developer
Parser is trivial: `#(` is a two-character token (like `#{`). No disambiguation logic, no lookahead. LSP can immediately provide list method completions after `#(`. Error messages are straightforward — if someone writes `[1, 2, 3]`, suggest `#(1, 2, 3)`.

## Steelman Analysis

### For `#(1, 2, 3)` — Prefixed Parens (Recommended)

- 🧑‍💻 **Newcomer**: "One extra character but zero confusion — I never accidentally create a block when I want a list"
- 🎩 **Smalltalk purist**: "This IS Smalltalk's `#(...)` array literal! Just with commas. Blocks stay sacred as `[...]`"
- ⚙️ **BEAM veteran**: "It's not `[...]` but it's close enough, and I already write `#{}` for maps — consistent"
- 🏭 **Operator**: "Zero parser ambiguity means zero edge-case bugs. Simple is reliable"
- 🎨 **Language designer**: "This isn't just a list syntax — it's a design system. `#` = data, clean extensibility for tuples/regex/binary later"

### For `[1, 2, 3]` — BEAM-native

- 🧑‍💻 **Newcomer**: "This is what I'd type without reading any docs — same as Python, JS, Ruby, JSON"
- ⚙️ **BEAM veteran**: "Identical to Erlang/Elixir/Gleam — zero learning curve, code moves between languages"
- 🎨 **Language designer**: "Most familiar syntax possible; disambiguation via commas is clever"

### For `#[1, 2, 3]` — Prefixed Brackets

- ⚙️ **BEAM veteran**: "Brackets say 'list' to me — `#[...]` is the closest to Erlang's `[...]`"
- 🎨 **Language designer**: "`#[]` for lists, `{}` for arrays, `#{}` for maps — each delimiter has its own meaning"

### Tension Points

| Concern | `#(...)` | `[...,]` | `#[...]` |
|---------|----------|----------|----------|
| Ambiguity | None | `[42]` block vs list | None |
| Backward compat | Full | `[]` changes meaning | Full |
| BEAM familiarity | Close (one char) | Identical | Close (one char) |
| Single-element | `#(42)` — clean | `[42,]` — trailing comma | `#[42]` — clean |
| Semantic analysis needed | No | Yes (L effort) | No |
| Extensible design | ✅ Family: `#()`, `#[]`, `#{}` | ❌ Ad hoc | ✅ Same family |
| Smalltalk heritage | ✅ `#(...)` is Smalltalk | ❌ Departs | Partial |

**The decisive factor:** `[...,]` requires building type inference (2-3 weeks, BT-140) just to provide decent error messages for the `[42]` block-vs-list confusion. `#(...)` makes that mistake structurally impossible, freeing the type system work to focus on actual language semantics rather than mitigating syntax ambiguity.

## Alternatives Considered

### Alternative A: `[1, 2, 3]` — BEAM-native Brackets

```beamtalk
numbers := [1, 2, 3]
empty := []
single := [42,]          // Trailing comma required for single-element
```

**Rejected because:**
- **Single-element ambiguity**: `[42]` is a block, `[42,]` is a list — easy to confuse, requires semantic analysis (2-3 weeks of type inference work) to warn appropriately
- **Empty list breaking change**: `[]` must change from empty block to empty list
- **Parser complexity**: Requires parsing first expression then branching on comma vs period/bracket — fragile
- **No design system**: Solves lists but provides no framework for tuples, regex, or other future literals
- **Error messages are reactive**: You need type inference to say "this block doesn't understand `head`" — with `#()`, the mistake can't happen

### Alternative B: `#[1, 2, 3]` — Prefixed Brackets

```beamtalk
numbers := #[1, 2, 3]
empty := #[]
single := #[42]           // Unambiguous
```

**Not rejected outright** — this is a reasonable alternative. However:
- `#(...)` is preferred because Smalltalk developers recognize `#(...)` as the collection literal form
- If the community strongly prefers brackets for lists, this remains viable

### Alternative C: `List(1, 2, 3)` — Message Send

```beamtalk
numbers := List with: 1 with: 2 with: 3
```

**Rejected because:**
- No literal syntax — verbose for the most common collection type
- Can't pattern match on message sends
- Doesn't compose well in method arguments
- Every modern language has collection literals

## Consequences

### Positive
- **Zero ambiguity**: `#(...)` is always a list, `[...]` is always a block — no parser tricks, no semantic analysis
- **Zero breaking changes**: Block syntax completely unchanged, `[]` stays as empty block
- **Coherent design system**: `#` = data prefix extends naturally to tuples, regex, binaries
- **Smalltalk heritage**: `#(...)` is recognizable to Smalltalk developers as a collection literal
- **Simple parser**: `#(` is a two-character token, like `#{` — no disambiguation logic needed
- **Unlocks features**: Enables `perform:withArgs:`, collection E2E tests, stdlib iteration
- **Efficient codegen**: Maps directly to Erlang linked lists — no wrapper types

### Negative
- **Not BEAM-identical**: `#(1, 2, 3)` vs Erlang's `[1, 2, 3]` — one prefix character different
- **Extra character**: Two characters more per list literal compared to `[...]` (the `#` and using `()` instead of `[]`)
- **Parenthesis overloading**: `()` used for both grouping `(3 + 4) * 2` and lists `#(1, 2, 3)` — mitigated by the `#` prefix always being present on lists
- **Not what Python/JS developers expect**: They'll try `[1, 2, 3]` first — but the error message guides them

### Neutral
- `Literal::Array` AST variant and codegen already exist — implementation is wiring up lexer/parser
- List methods (`head`, `tail`, `prepend:`, `collect:`, etc.) already have codegen support via `list_ops.rs`
- The `#` prefix convention aligns with Erlang's own `#{map}` and record `#record{field}` syntax

## Implementation

### Phase 1: Lexer + Parser (S)
- Add `ListOpen` token to lexer: `#(` (two-character token, like `MapOpen` `#{`)
- Reuse existing `BinarySelector(",")` token as list element separator (same approach as map literals)
- Add parser rule: `#(` starts list literal, comma-separated expressions, `)` closes
- Support `#()` (empty), `#(expr)` (single), `#(expr, expr, ...)` (multi)
- Support cons syntax: `#(expr | expr)` for prepend
- Rename AST `Literal::Array` to `Literal::List` for clarity

**Files:**
- `crates/beamtalk-core/src/source_analysis/token.rs` — Add `ListOpen` token
- `crates/beamtalk-core/src/source_analysis/lexer.rs` — Lex `#(` as `ListOpen`
- `crates/beamtalk-core/src/source_analysis/parser/expressions.rs` — `parse_list_literal()`
- `crates/beamtalk-core/src/ast.rs` — Rename `Literal::Array` → `Literal::List`

### Phase 2: Codegen + Tests (S)
- Codegen already handles `Literal::Array` → Erlang list — rename and verify
- Add expression-level list support (dynamic lists with non-literal elements)
- Add unit tests for parser
- Add E2E tests: `tests/e2e/cases/list_literals.bt`

**Files:**
- `crates/beamtalk-core/src/codegen/core_erlang/expressions.rs` — `Expression::ListLiteral` support
- `tests/e2e/cases/list_literals.bt` — New E2E test file

### Phase 3: Pattern Matching (M)
- Enable `#(head | tail)` in pattern positions (AST `Pattern::List` already exists)
- Wire up pattern matching codegen for list destructuring
- E2E tests for pattern matching

### Phase 4: Stdlib + Runtime Dispatch (M)
- Create `lib/List.bt` with primitive-backed methods
- Implement `beamtalk_list.erl` runtime dispatch module
- Wire up `head`, `tail`, `prepend:`, `size`, `at:`, `reversed`, `++`

## Future Work: Array Literals and Tuple/Array Separation

Array literal syntax (`{1, 2, 3}`) is intentionally deferred from this ADR. The key design question — **how to distinguish user-created arrays from raw Erlang tuples** — requires its own ADR.

### The Problem

Erlang uses tuples for two fundamentally different purposes:
- **User data**: `{3, 4}` (a point), `{255, 128, 0}` (RGB color)
- **Protocol/interop**: `{ok, Value}`, `{error, Reason}`, `{reply, Result, State}`

These have different interfaces (arrays want `at:`, `collect:`; tuples want `isOk`, `unwrap`) but are the same BEAM type (`is_tuple/1`).

### Likely Direction: Runtime Tagging

The preferred approach is tagging user arrays at runtime, following the same pattern as `beamtalk_object`:

```erlang
%% User writes {1, 2, 3}, codegen produces:
{beamtalk_array, {1, 2, 3}}          %% Tagged → class 'Array'

%% Erlang FFI returns {ok, 42}:
{ok, 42}                              %% Raw → class 'Tuple'

%% class_of dispatch:
class_of({beamtalk_array, _}) -> 'Array';
class_of(X) when is_tuple(X) -> 'Tuple';
```

This separates the classes cleanly but has significant implementation implications (~10 files, ~125 lines) including pattern matching, interop boundaries, and display formatting. A dedicated ADR should address:

1. Syntax: `{1, 2, 3}` (bare braces) — likely choice since braces have no code use
2. Runtime representation: tagged vs untagged
3. Pattern matching: `{a, b} := point` must handle the tag
4. Interop: `asTuple` / `asArray` conversion methods
5. Naming: Array (Smalltalk-compatible) vs Tuple (Erlang-compatible)
6. Impact on existing `beamtalk_tuple.erl` and `beamtalk_primitive.erl`

## Migration Path

No migration needed — list literals are a new feature.

Documentation updates:
- `docs/beamtalk-syntax-rationale.md` syntax table: `[a, b, c]` → `#(a, b, c)` for lists
- `docs/beamtalk-language-features.md`: `#[1, 2, 3, 4, 5]` → `#(1, 2, 3, 4, 5)` in control flow section

## References
- BT-72: Implement List literal parsing and codegen
- BT-172: E2E tests for self-as-object (discovered need for list literals)
- BT-331: Implement compilable stdlib collection classes
- BT-74: Implement Array/tuple operations codegen
- ADR 0005: BEAM Object Model — type hierarchy with Array/List as primitives
- ADR 0007: Compilable Standard Library — list iteration primitives
- `docs/beamtalk-syntax-rationale.md` line 371 — prior syntax table (to be updated)
- `docs/beamtalk-language-features.md` lines 964-965 — Array/List implementation status
- Clojure reader dispatch: https://clojure.org/guides/weird_characters
- Pharo literal arrays: https://book.gtoolkit.com/working-with-collections-in-pharo-w9fc31ubksh9va7i1516z5mt
