# ADR 0012: List Literal Syntax

## Status
Proposed (2026-02-08)

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

**The syntax rationale doc** (`docs/beamtalk-syntax-rationale.md` line 371) already lists `[a, b, c]` as the intended list syntax but provides no disambiguation rules.

### The Disambiguation Challenge

Beamtalk uses `[...]` for block (closure) literals:
```beamtalk
[42]                    // Block returning 42
[:x | x + 1]           // Block with parameter
[self doSomething]      // Block calling a method
```

Any list literal syntax using `[...]` must be unambiguously distinguishable from blocks.

### Constraints

1. **BEAM-native**: Beamtalk compiles to Erlang linked lists — the primary sequence type on BEAM
2. **Interactive-first**: Must work naturally in REPL sessions
3. **Backward compatible**: Existing block syntax must continue to work unchanged
4. **Composable**: Must work in method arguments, assignments, and pattern matching

## Decision

**Use `[expr, expr, ...]` for list literals, disambiguated from blocks by the presence of commas.**

### Syntax

```beamtalk
// List literals (comma-separated)
empty := []                        // Empty list
numbers := [1, 2, 3]              // List of integers
mixed := [1, "hello", #ok]        // Heterogeneous list
nested := [[1, 2], [3, 4]]        // Nested lists
single := [42,]                   // Single-element list (trailing comma)

// Cons syntax (head | tail)
prepended := [0 | numbers]        // [0, 1, 2, 3]

// Blocks (unchanged — no commas)
block := [42]                     // Block returning 42
adder := [:x | x + 1]            // Block with parameter
multi := [x := 1. y := 2. x + y] // Multi-statement block
```

### Disambiguation Rules

The parser determines whether `[...]` is a list or a block based on what follows the opening bracket:

| After `[` | Then sees | Interpretation | Example |
|-----------|-----------|----------------|---------|
| `]` | — | Empty list | `[]` |
| `:identifier` | `\|` | Block (parameters) | `[:x \| x + 1]` |
| `expr` | `,` | List (comma separator) | `[1, 2, 3]` |
| `expr` | `\|` | Cons list | `[head \| tail]` |
| `expr` | `]` | Block (single expression) | `[42]` |
| `expr` | `.` or newline | Block (statement separator) | `[x := 1. x + 1]` |

**Key insight:** Commas never appear in block syntax. Blocks use periods (`.`) or newlines to separate statements, and colons (`:`) with pipes (`|`) for parameters. This makes comma an unambiguous list indicator.

### Single-Element Lists

Since `[42]` must remain a block for backward compatibility, single-element lists use a trailing comma:

```beamtalk
[42]     // Block returning 42 (backward compatible)
[42,]    // List containing 42 (trailing comma)
```

This follows the same convention as Rust's single-element tuples `(42,)`.

Alternative: use a message send for single-element lists:
```beamtalk
List with: 42    // Also creates [42]
```

### Empty List

`[]` is the empty list. This is a **minor breaking change** — `[]` previously parsed as an empty block. In practice, empty blocks don't appear in any Beamtalk source files (only in Erlang code comments). Empty blocks can be written as `[nil]` or `[| nil]` if ever needed.

### REPL Session

```
> [1, 2, 3]
[1, 2, 3]

> numbers := [10, 20, 30]
[10, 20, 30]

> numbers head
10

> numbers tail
[20, 30]

> [0 | numbers]
[0, 10, 20, 30]

> numbers collect: [:n | n * 2]
[20, 40, 60]

> numbers select: [:n | n > 15]
[20, 30]

> args := [1, 2, 3,]
[1, 2, 3]

> counter perform: #increment
// works

> obj perform: #add:to: withArgs: [3, 4]
// works — list of arguments
```

### Error Examples

```
> [1, 2, 3] value
ERROR: List does not understand 'value'
  Hint: 'value' is a Block method. Did you mean to create a block? Use [expr] without commas.

> [1 2 3]
ERROR: Expected ',' or ']' after list element, found '2'
  Hint: List elements must be separated by commas: [1, 2, 3]

> numbers at: 5
ERROR: Index 5 out of bounds for list of size 3
  Hint: List indices start at 1 and go up to 3
```

### BEAM Mapping

| Beamtalk | Erlang | BEAM Type |
|----------|--------|-----------|
| `[1, 2, 3]` | `[1, 2, 3]` | Linked list (cons cells) |
| `[]` | `[]` | Empty list |
| `[h \| t]` | `[H \| T]` | Cons cell |
| `#{a => 1}` | `#{a => 1}` | Map |

Lists are Erlang linked lists — O(1) prepend, O(n) append, O(n) random access. This matches every other BEAM language.

## Prior Art

### Smalltalk Family

| Language | Literal Array | Dynamic Array | List |
|----------|--------------|---------------|------|
| **Pharo/Squeak** | `#(1 2 3)` — compile-time, immutable | `{1. 2. 1+2}` — runtime, mutable | `OrderedCollection` — no literal |
| **Newspeak** | Similar to Pharo | Similar to Pharo | No literal syntax |
| **GNU Smalltalk** | `#(1 2 3)` | `{1. 2. 3}` | No literal syntax |

Smalltalk uses `#(...)` with space-separated elements for compile-time arrays and `{...}` with period-separated elements for runtime arrays. Neither has a list literal. Beamtalk diverges here because BEAM's primary sequence type is the linked list, not an array.

### BEAM Languages

| Language | List Syntax | Type | Pattern Match |
|----------|------------|------|---------------|
| **Erlang** | `[1, 2, 3]` | Linked list | `[H \| T]` |
| **Elixir** | `[1, 2, 3]` | Linked list | `[h \| t]` |
| **Gleam** | `[1, 2, 3]` | Linked list | `[first, ..rest]` |
| **LFE** | `'(1 2 3)` or `(list 1 2 3)` | Linked list | Pattern match |

**Every BEAM language** uses `[1, 2, 3]` for lists. This is the universal convention on the platform.

### Other Languages with Block/List Ambiguity

| Language | Blocks | Lists | Resolution |
|----------|--------|-------|------------|
| **Ruby** | `{ \|x\| x + 1 }` or `do...end` | `[1, 2, 3]` | Different delimiters |
| **Elixir** | `fn x -> x + 1 end` | `[1, 2, 3]` | Different syntax entirely |
| **Swift** | `{ x in x + 1 }` | `[1, 2, 3]` | Different delimiters |

Beamtalk is unique in using `[...]` for both blocks and lists. The comma-based disambiguation is novel but principled — it leverages the fact that Beamtalk's statement separator is the period/newline, never the comma.

## User Impact

### Newcomer (from Python/JS/Ruby)
`[1, 2, 3]` is instantly familiar — identical to Python, JavaScript, Ruby, and JSON. Zero learning curve for the most common collection type. The single-element trailing comma `[42,]` is familiar from Python tuples.

### Smalltalk Developer
This departs from Smalltalk's `#(1 2 3)` array syntax. However:
- Beamtalk already uses commas in map literals (`#{a => 1, b => 2}`)
- The BEAM's primary sequence is the linked list, not the array
- Smalltalk never had a list literal at all — `OrderedCollection` required explicit construction
- Block syntax `[:x | x + 1]` is completely preserved

### Erlang/Elixir Developer
Identical syntax to what they already know. `[1, 2, 3]`, `[H | T]`, `[]` all work exactly as expected. Pattern matching with cons cells follows Erlang convention. This maximizes BEAM ecosystem familiarity.

### Production Operator
Lists compile to standard Erlang linked lists — fully observable in Observer, compatible with `:recon`, appear naturally in crash dumps. No wrapper types, no indirection.

### Tooling Developer
The AST already has `Literal::Array`. Parser disambiguation is local (one token lookahead after first expression). LSP can provide completions for list methods after `[...]` when comma is detected.

## Steelman Analysis

### Option A: `[1, 2, 3]` — BEAM-native (Recommended)

- 🧑‍💻 **Newcomer**: "This is what I'd type without reading any docs — same as Python, JS, Ruby, JSON"
- 🎩 **Smalltalk purist**: "Smalltalk never had list literals anyway. This fills a real gap, and blocks are untouched"
- ⚙️ **BEAM veteran**: "Finally — same syntax as Erlang/Elixir/Gleam. I can move code between languages without mental translation"
- 🏭 **Operator**: "Standard Erlang lists in crash dumps and Observer. No surprises"
- 🎨 **Language designer**: "Comma disambiguation is elegant — one character difference between `[42]` (block) and `[42,]` (list) is minimal syntax tax for full backward compatibility"

### Option B: `#(1, 2, 3)` — Smalltalk-inspired

- 🧑‍💻 **Newcomer**: "The `#` prefix clearly says 'this is data, not code' — no confusion with blocks"
- 🎩 **Smalltalk purist**: "This honors Smalltalk's `#(...)` tradition while modernizing with commas instead of spaces"
- ⚙️ **BEAM veteran**: "It's different from Erlang, but at least it's unambiguous. I can learn it"
- 🏭 **Operator**: "No ambiguity in parsing means fewer edge-case bugs in the compiler"
- 🎨 **Language designer**: "Zero ambiguity is worth a prefix character. `#` already means 'literal data' (symbols, maps)"

### Option C: `#[1, 2, 3]` — Prefixed brackets

- 🧑‍💻 **Newcomer**: "Brackets mean list, `#` means literal — I get it"
- 🎩 **Smalltalk purist**: "Keeps `[...]` sacred for blocks while providing a clear list literal"
- ⚙️ **BEAM veteran**: "Close enough to Erlang syntax — just an extra `#`"
- 🏭 **Operator**: "Clear visual distinction between blocks and lists in code review"
- 🎨 **Language designer**: "Consistent with `#{}` for maps — `#[]` for lists, `#()` for tuples would form a nice family"

### Tension Points

- **Newcomers and BEAM veterans** strongly prefer Option A — it's what they already know
- **Smalltalk purists** lean toward B or C — explicit prefix avoids overloading `[...]`
- **Language designers** are split: A is most familiar but B/C are more formally clean
- **The single-element case** `[42,]` is the main cost of Option A — Options B and C don't have this issue
- **Beamtalk's syntax rationale** already documents Option A as the intended design

## Alternatives Considered

### Alternative B: `#(1, 2, 3)` — Smalltalk Array Syntax

```beamtalk
numbers := #(1, 2, 3)
empty := #()
single := #(42)          // Unambiguous — no block conflict
```

**Rejected because:**
- Inconsistent with every BEAM language (Erlang, Elixir, Gleam all use `[...]`)
- Parentheses `()` are already used for grouping expressions: `(3 + 4) * 2`
- Smalltalk's `#(...)` is compile-time-only and space-separated — adapting it with commas pleases neither Smalltalk purists nor BEAM developers
- The `#` prefix is already overloaded: `#symbol`, `#{map}`, `#(array)` — cognitive load
- Compiles to Erlang lists anyway — misleading to use different syntax

### Alternative C: `#[1, 2, 3]` — Prefixed Brackets

```beamtalk
numbers := #[1, 2, 3]
empty := #[]
single := #[42]           // Unambiguous — no block conflict
```

**Rejected because:**
- Extra character for every list literal — high frequency construct deserves shortest syntax
- Not consistent with any BEAM language
- `#{}` for maps but `#[]` for lists creates a "why not `[]`?" question
- Pattern matching with `#[head | tail]` looks noisy compared to Erlang's `[H | T]`
- The disambiguation problem is solvable without a prefix

### Alternative D: `List(1, 2, 3)` — Message Send

```beamtalk
numbers := List with: 1 with: 2 with: 3
// or
numbers := List fromElements: #(1, 2, 3)
```

**Rejected because:**
- No literal syntax — verbose, unattractive for common operation
- Can't pattern match on message sends
- Doesn't compose well in method arguments
- Every other language has collection literals; this would feel like a gap

## Consequences

### Positive
- **BEAM-native**: Identical syntax to Erlang, Elixir, and Gleam — zero learning curve for BEAM developers
- **Familiar**: Same syntax as Python, JavaScript, Ruby, JSON — most developers already know it
- **Composable**: Works in assignments, method arguments, pattern matching, and REPL
- **Unlocks features**: Enables `perform:withArgs:`, collection E2E tests, stdlib iteration
- **Efficient codegen**: Maps directly to Erlang linked lists — no wrapper types

### Negative
- **Single-element ambiguity**: `[42]` is a block, `[42,]` is a list — requires trailing comma for single element
- **Empty list change**: `[]` changes from empty block to empty list — minor breaking change (no existing code affected)
- **Parser complexity**: Disambiguation requires parsing first expression then branching on comma/period/bracket — more complex than prefix approach
- **`[expr | expr]`**: Cons syntax `[h | t]` in expression position could initially confuse Smalltalk users who expect `|` only in block parameter syntax

### Neutral
- `Literal::Array` AST variant and codegen already exist — implementation is wiring up lexer/parser, not building from scratch
- Does not affect tuple syntax (future `{a, b, c}` — separate decision)
- List methods (`head`, `tail`, `prepend:`, `collect:`, etc.) already have codegen support via `list_ops.rs`

## Implementation

### Phase 1: Lexer + Parser (S)
- Add `Comma` token to lexer (currently missing)
- Modify parser: after `[`, parse first expression, then branch on `,` (list) vs `.`/newline/`]` (block)
- Handle `[]` as empty list, `[expr,]` as single-element list, `[h | t]` as cons
- Update `Literal::Array` or rename to `Literal::List`

**Files:**
- `crates/beamtalk-core/src/source_analysis/token.rs` — Add `Comma` token
- `crates/beamtalk-core/src/source_analysis/lexer.rs` — Lex `,`
- `crates/beamtalk-core/src/source_analysis/parser/expressions.rs` — `parse_list_or_block()`

### Phase 2: Codegen + Tests (S)
- Codegen already handles `Literal::Array` → Erlang list — just needs expression-level list support
- Add unit tests for parser disambiguation
- Add E2E tests: `tests/e2e/cases/list_literals.bt`

**Files:**
- `crates/beamtalk-core/src/codegen/core_erlang/expressions.rs` — May need `Expression::ListLiteral` support
- `tests/e2e/cases/list_literals.bt` — New E2E test file

### Phase 3: Pattern Matching (M)
- Enable `[head | tail]` in pattern positions (AST `Pattern::List` already exists)
- Wire up pattern matching codegen for list destructuring

### Phase 4: Stdlib + Runtime Dispatch (M)
- Create `lib/List.bt` with primitive-backed methods
- Implement `beamtalk_list.erl` runtime dispatch module
- Wire up `head`, `tail`, `prepend:`, `size`, `at:`, `reversed`, `++`

## References
- BT-72: Implement List literal parsing and codegen
- BT-172: E2E tests for self-as-object (discovered need for list literals)
- BT-331: Implement compilable stdlib collection classes
- BT-74: Implement Array/tuple operations codegen
- ADR 0005: BEAM Object Model — type hierarchy with Array/List as primitives
- ADR 0007: Compilable Standard Library — list iteration primitives
- `docs/beamtalk-syntax-rationale.md` line 371 — prior design intent for `[a, b, c]` syntax
- `docs/beamtalk-language-features.md` lines 964-965 — Array/List implementation status
