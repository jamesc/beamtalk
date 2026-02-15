# ADR 0023: String Interpolation Syntax and Compilation

## Status
Accepted (2026-02-15)

## Context

Beamtalk needs string interpolation — the ability to embed expressions inside string literals. This is "table stakes" for a modern language (see `docs/beamtalk-syntax-rationale.md`), but the design involves several coupled decisions:

1. **Quote convention**: Which quote character(s) are used for strings?
2. **Interpolation syntax**: How does the programmer embed expressions?
3. **Compilation**: What BEAM construct does interpolation compile to?
4. **Conversion**: How are non-string values converted to strings?

### Current State

- **Strings are UTF-8 binaries** (`<<"...">>` in Erlang) — this is settled
- The **lexer** currently uses single quotes (`'...'`) for strings and double quotes (`"..."`) for "interpolated strings" — but this was a preliminary choice inherited from Smalltalk convention, not a deliberate design decision
- The **parser, AST, and codegen** have no interpolation support
- `test-package-compiler/cases/future_string_interpolation/main.bt` documents expected behavior
- ~150 `.bt` files use single-quoted strings throughout

### Constraints

- Must produce a **binary** (not charlist, not iolist) — beamtalk strings are binaries everywhere
- Must work with **UTF-8** content including emoji and CJK characters
- Must compose with existing **message-send** semantics — expressions inside interpolation should be normal beamtalk expressions
- Should not conflict with **existing syntax**: `#{...}` maps, `$a` character literals, `[...]` blocks. Note: `{a, b}` tuple patterns and planned tuple/array literals use `{` in _expression_ context, but `{` inside `"..."` strings is unambiguously interpolation — the lexer handles strings in a separate mode.
- Must be **lexer-friendly** — tokenization should be straightforward, using well-understood mode-switching (same technique as Kotlin, Swift, Elixir)

## Decision

### 1. Double Quotes Only — Drop Single-Quoted Strings

Beamtalk uses **double quotes** as the sole string literal syntax. Single-quoted strings are **removed from the language**.

```beamtalk
name := "Alice"
greeting := "Hello, {name}!"
plain := "No braces, no interpolation"
```

**Rationale:**

- **Modern convention**: Rust, Go, Swift, Kotlin — the dominant modern languages — all use double quotes for strings. Developers reaching for `"hello"` should not be surprised.
- **Eliminates "which quote?" confusion**: Elixir's single-vs-double quote distinction is [one of its most well-known gotchas](https://pragtob.wordpress.com/2024/05/01/10-elixir-gotchas/). The Elixir community [proposed deprecating single-quote charlists](https://groups.google.com/g/elixir-lang-core/c/_pAjPEayLLI) and added the `~c"..."` sigil as a replacement — effectively admitting the distinction was a mistake.
- **No charlists in beamtalk**: Elixir's quote distinction exists for Erlang charlist interop. Beamtalk has no charlists — there is no technical reason for two string types. Charlists are not more efficient than binaries (they use 8x more memory as linked lists of integers).
- **Smalltalk departure is justified**: Smalltalk uses single quotes, but beamtalk already departs from Smalltalk in many ways (comments, statement separators, operators). Following the modern mainstream serves developers better.
- **`$a` handles character literals**: The Erlang-style `$a` syntax already fills the role that single quotes serve in Rust/Go/Swift (character literals).

### 2. All Strings Support Interpolation via `{expr}`

Every double-quoted string can contain `{expr}` interpolation. If there are no `{expr}` segments, the string compiles identically to a plain string literal — zero overhead.

```beamtalk
// Plain string (no braces, no interpolation)
greeting := "Hello, world!"

// Interpolation with variables
name := "Alice"
message := "Hello, {name}!"

// Expressions inside braces
result := "Sum: {2 + 3}"

// Message sends
info := "Length: {name length}"
detail := "Upper: {name uppercase}"

// Multi-line
report := "Name: {name}
Age: {age}
Status: active"
```

Literal braces are escaped with backslash:

```beamtalk
"Set notation: \{1, 2, 3\}"   // => Set notation: {1, 2, 3}
"JSON-like: \{\"key\": 42\}"  // => JSON-like: {"key": 42}
```

This is the same approach as **Swift** — all strings can interpolate, no prefix or quote distinction needed. The simplest possible mental model: one string type, one quote style, interpolation just works.

### 3. Compilation: Binary Construction

Interpolation compiles to **Erlang binary construction**, producing a flat binary:

```beamtalk
// Source
"Hello, {name}!"
```

```erlang
%% Generated Core Erlang (conceptual — actual dispatch uses object system)
let _Name_str = call 'beamtalk_dispatch':'send'(Name, 'printString', []) in
  #{#<72>(8,1,'integer',['unsigned'|['big']]),
    #<101>(8,1,'integer',['unsigned'|['big']]),
    ...
    (_Name_str)/binary,
    #<33>(8,1,'integer',['unsigned'|['big']])}#
```

Strings without interpolation compile to simple binary literals as today — no overhead.

This is the same approach Elixir uses: eager binary construction, not iolists. The result is always a binary (String), which means `length`, `at:`, `++`, etc. all work immediately.

### 4. Auto-Conversion: `printString` Message

Non-string values are converted via `printString`:

```beamtalk
age := 30
"Age: {age}"   // age printString => "30", then binary concat
```

The `printString` message is already the standard way to get a string representation in beamtalk (following Smalltalk convention). This is analogous to:
- Elixir's `String.Chars.to_string/1` protocol
- Python's `__str__`
- Ruby's `to_s`
- Pharo's `asString`

If `printString` is not understood by the receiver, the standard `doesNotUnderstand:` error is raised — no silent failures.

### REPL Session

```text
Alice
> "Hello, {name}!"
Hello, Alice!
> "2 + 2 = {2 + 2}"
2 + 2 = 4
> "{name length} chars"
5 chars
> "Braces: \{escaped\}"
Braces: {escaped}
> "No braces here"
No braces here
```

### Error Examples

```text
> "Hello, {undefined}"
ERROR: UndefinedObject does not understand 'printString'
  Hint: Variable 'undefined' is not defined in this scope

> "Missing close brace {name"
ERROR: Unterminated interpolation expression at line 1
  Hint: Add closing '}' to complete the expression

> "Empty: {}"
ERROR: Empty interpolation expression at line 1
  Hint: Add an expression between '{' and '}'
```

### Edge Cases

- **Empty braces `{}`**: Syntax error — an expression is required between `{` and `}`
- **Nested braces**: Expressions containing blocks or tuples work naturally — the lexer tracks brace depth to find the matching `}` for the interpolation. Example: `"Result: {[:x | x * 2] value: 3}"` works because `[...]` block braces don't count.
- **`printString` failure**: If the interpolated value does not understand `printString`, the standard `doesNotUnderstand:` error is raised — no silent fallback or placeholder text
- **Lexer mode**: Inside `"..."`, the lexer switches to string mode. `{` starts an embedded expression sub-lexer that tracks brace depth. This is the same approach used by Kotlin, Swift, and Elixir — well-understood lexer technique.

### Future Syntax Considerations

This ADR does not constrain future decisions on:
- **Heredocs / triple-quoted strings** (`"""..."""`): If added, they should use the same `{expr}` interpolation — no new delimiter needed
- **Raw strings** (e.g., `#r"..."` from ADR 0012): Should NOT interpolate — raw means raw
- **Binary literals** (`#b<<...>>`): Separate syntax, not affected by string interpolation

## Prior Art

| Language | String Quotes | Interpolation | Delimiter | Compiles to | Auto-convert |
|----------|---------------|---------------|-----------|-------------|--------------|
| **Swift** | `"..."` only | Built-in (all strings) | `\()` | String concat | `description` |
| **Rust** | `"..."` only | No (use `format!` macro) | N/A | N/A | `Display` trait |
| **Go** | `"..."` only | No (use `fmt.Sprintf`) | N/A | N/A | `Stringer` |
| **Kotlin** | `"..."` only | Built-in (all strings) | `${}` / `$name` | String concat | `toString()` |
| **Elixir** | `"..."` (strings) / `'...'` (charlists) | Quote-style | `#{}` | Binary `<>` | `to_string` |
| **Ruby** | `"..."` (interp) / `'...'` (literal) | Quote-style | `#{}` | String concat | `to_s` |
| **Python** | `"..."` / `'...'` (both literal) | Prefix (`f"..."`) | `{}` | `__format__` | `__str__` |
| **C#** | `"..."` only | Prefix (`$"..."`) | `{}` | `String.Format` | `ToString()` |
| **Erlang** | `"..."` (charlists) / `'...'` (atoms) | None | N/A | N/A | N/A |
| **Pharo** | `'...'` only | Message (`format:`) | N/A | Message send | `asString` |

### What We Adopted

- **Swift/Kotlin's "all strings interpolate"**: No prefix, no quote distinction. Simplest mental model. If a string has `{expr}`, it interpolates; if not, it's plain.
- **Double quotes as sole string syntax**: Matches Rust, Go, Swift, Kotlin — the modern mainstream.
- **`{expr}` delimiters**: Clean, minimal — no extra sigil needed. Unambiguous since `#{}` is maps and `$` is characters.
- **Elixir's binary compilation**: Eager binary construction is simple, correct, and matches beamtalk's "strings are binaries" model.
- **Smalltalk's `printString`**: Auto-conversion uses the existing object protocol, not a separate trait/interface.

### What We Rejected

- **Single-quoted strings**: See Alternatives below.
- **Quote-style distinction** (Elixir/Ruby): See Alternatives below.
- **Prefix approach** (Python/C#): See Alternatives below.
- **Pharo's `format:` message**: Too verbose for common use.
- **Swift's `\()` delimiter**: Unfamiliar to most developers; `{}` is more widely understood.
- **Iolist compilation**: See Alternatives below.

## User Impact

### Newcomer (from Python/JS/Rust/Go)
- Double quotes for strings — immediately natural, zero surprise
- No "which quote does what?" confusion — there's only one kind
- Interpolation just works — no prefix to remember, no special quote to choose

### Smalltalk Developer
- Departure from Smalltalk's single-quoted strings — but beamtalk already departs in comments (`//` not `"..."`), statement separators (newlines not `.`), and operators
- `printString` conversion preserves Smalltalk's object protocol
- The pragmatic departure improves the language for the 99% of developers who are not Smalltalk veterans

### Erlang/BEAM Developer
- No charlist confusion — beamtalk has one string type (binary), one quote style
- Compiles to familiar binary construction (`<<...>>`)
- No iolist surprises — result is always a binary
- Avoids Elixir's charlist gotcha entirely

### Production Operator
- No runtime overhead for strings without interpolation — compiles to plain binary literal
- No lazy evaluation or deferred allocation (unlike iolists)
- Deterministic memory behavior — binary is allocated once

## Steelman Analysis

### Option A: Double Quotes Only with Universal Interpolation (Chosen)

| Cohort | Best argument for this option |
|--------|------------------------------|
| 🧑‍💻 **Newcomer** | "One string type, one quote style — I never have to think about which quote to use. `{expr}` just works when I need it" |
| 🎩 **Smalltalk** | "Beamtalk is Smalltalk-inspired, not Smalltalk-compatible. This makes the language accessible to millions more developers" |
| ⚙️ **BEAM veteran** | "Elixir's charlist confusion is eliminated entirely. One type, one syntax — no gotchas" |
| 🏭 **Operator** | "No overhead for plain strings. Interpolation compiles to the same binary construction the BEAM already optimizes" |
| 🎨 **Designer** | "Simplest possible design. Zero cognitive overhead. Scales perfectly — nothing to add, nothing to remove" |

### Option B: Quote-Style Distinction `'literal'` vs `"interpolated"` (Rejected)

| Cohort | Best argument for this option |
|--------|------------------------------|
| 🧑‍💻 **Newcomer (from Ruby)** | "Ruby, Elixir use this pattern — it's familiar from the BEAM ecosystem" |
| 🎩 **Smalltalk** | "Single quotes stay as literal strings, preserving Smalltalk convention. Double quotes are an extension" |
| ⚙️ **BEAM veteran** | "Elixir does this — consistency within the BEAM ecosystem" |
| 🎨 **Designer** | "Two string types give an escape hatch for `{` without escaping" |

### Option C: Prefix Syntax `f"Hello, {name}!"` (Rejected)

| Cohort | Best argument for this option |
|--------|------------------------------|
| 🧑‍💻 **Newcomer (from Python)** | "Python's f-strings are explicit — I always know when interpolation is happening" |
| ⚙️ **BEAM veteran** | "An explicit prefix is a clear signal for code review — easy to grep for side effects" |
| 🎨 **Designer** | "Prefix system scales to future string types: `r"raw"`, `b"bytes"`, etc." |

### Option D: Message Send `"Hello, {1}" format: {name}` (Rejected)

| Cohort | Best argument for this option |
|--------|------------------------------|
| 🎩 **Smalltalk** | "Pure message-based — no syntax extensions needed. Pharo does this." |
| 🎨 **Designer** | "Maximum orthogonality — everything is a message" |

### Tension Points

- **Swift/Kotlin vs Ruby/Elixir**: The "all strings interpolate" model is simpler but loses the literal-string escape hatch. In practice, `\{` escaping is sufficient — Python, C#, and Kotlin all use it successfully.
- **Smalltalk heritage vs modern convention**: Single quotes are deeply Smalltalk. But beamtalk targets a broader audience, and the pragmatic departure is consistent with other syntax choices already made.
- **Elixir ecosystem alignment vs correctness**: Following Elixir's quote convention maintains BEAM ecosystem consistency. But Elixir's own community acknowledges the charlist distinction is a gotcha — we should learn from their mistake, not repeat it.

## Alternatives Considered

### Alternative A: Keep Single-Quoted Strings (Smalltalk Convention)

```beamtalk
name := 'Alice'                      // single quotes for strings
greeting := "Hello, {name}!"         // double quotes for interpolation
literal := 'Hello, {name}!'          // single quotes = no interpolation
```

**Why rejected:**
- Two string syntaxes with different behavior — #1 beginner confusion in Ruby and Bash
- Elixir community [proposed deprecating](https://groups.google.com/g/elixir-lang-core/c/_pAjPEayLLI) single-quote charlists for exactly this reason
- Beamtalk has no charlists — there is no technical reason for single-quoted strings
- Conflicts with modern convention (Rust, Go, Swift, Kotlin all use double quotes)
- Single quotes in beamtalk mean something different from Erlang (atoms) — confusing for BEAM developers too

### Alternative B: Prefix Syntax (`f"..."`)

```beamtalk
name := "Alice"
greeting := f"Hello, {name}!"        // f-prefix for interpolation
literal := "Hello, {name}!"          // no prefix = no interpolation
```

**Why rejected:**
- Extra character on every interpolated string — a tax paid thousands of times
- Python needed `f` because it had three formatting systems and needed explicit opt-in. Beamtalk has none — no backward compatibility constraint.
- "Scales to future prefixes" is speculative (YAGNI) — no concrete plans for `r"..."`, `b"..."`, etc.
- Swift and Kotlin proved "all strings interpolate" works without a prefix

### Alternative C: Sigil Delimiter (`${expr}` or `#{expr}`)

```beamtalk
"Hello, ${name}!"                    // $ prefix on expressions
"Hello, #{name}!"                    // # prefix on expressions
```

**Why rejected:**
- `#{` is already taken — it's the map literal syntax (`#{#x => 1}`)
- `$` is used for character literals (`$a` = 97) — visual confusion
- Extra character inside every interpolation adds noise
- `{expr}` alone is unambiguous within double-quoted strings

### Alternative D: Iolist Compilation

```erlang
%% Could compile to iolist instead of binary
[<<"Hello, ">>, Name, <<"!">>]
```

**Why rejected:**
- Beamtalk strings are binaries — returning an iolist would break `length`, `at:`, `uppercase`, etc.
- Elixir and Gleam both compile interpolation to binary, not iolist
- Charlists/iolists are not more efficient than binaries for most use cases (binaries are contiguous, cache-friendly, 8x less memory than linked lists)
- A future `StringBuilder` class or Stream pattern (see ADR 0021) can provide iolist semantics for performance-critical output building without affecting the string type

## Consequences

### Positive
- One string type, one quote style — simplest possible mental model
- Matches the modern mainstream (Rust, Go, Swift, Kotlin)
- Eliminates Elixir's #1 gotcha (charlist confusion)
- Universal interpolation — no prefix or mode to remember
- Zero overhead for plain strings (no `{expr}` → compiles to plain binary literal)
- Clean compilation to BEAM binary construction
- `printString` auto-conversion leverages existing Smalltalk object protocol
- No ambiguity with existing syntax — `{` inside `"..."` is lexed in string mode (separate from expression-level `{` for tuples/arrays)

### Negative
- **Breaking change**: ~150 `.bt` files must be updated from `'...'` to `"..."` (mechanical find-replace, but large diff)
- Departs from Smalltalk's single-quote convention — Smalltalk developers must adjust
- Documentation overhaul: `beamtalk-syntax-rationale.md`, `beamtalk-language-features.md`, examples, tests, and `lib/*.bt` all reference single-quoted strings
- `\{` escaping required for literal braces in strings — minor but new escape to learn
- Symbols (`#'hello world'`) use single quotes for quoting — this is a different context but could cause momentary confusion if single quotes appear nowhere else

### Neutral
- The `InterpolatedString` and `String` token kinds in the lexer will be unified
- `test-package-compiler/cases/future_string_interpolation/main.bt` must be updated
- Iolist optimization remains available as a future concern via StringBuilder/Stream
- Double-quoted strings in Erlang are charlists — beamtalk's double-quoted strings are binaries. This is a deliberate divergence, consistent with Elixir's convention.
- This ADR bundles two logically coupled decisions (quote convention + interpolation). They are bundled because the quote change is motivated by interpolation — double quotes are chosen specifically because all strings should interpolate. Implementing one without the other would leave the language in an inconsistent intermediate state.

## Implementation

Since the language is pre-release with no external users, the quote change and interpolation can be implemented together without a deprecation cycle.

### Phase 1: Quote Convention + Lexer/Parser
- Update lexer: `"..."` becomes the sole string syntax, remove `'...'` string support
- `#'quoted symbols'` remain valid (single quotes in symbol context only)
- Mechanical `sed` to update all ~150 `.bt` files from `'...'` to `"..."`
- Enhance `"..."` lexing to detect `{expr}` segments via lexer mode-switching (track brace depth for nested expressions)
- Reject empty `{}` as syntax error
- Parse `{expr}` segments as embedded expressions
- Add `StringInterpolation` AST node with segments: `[Literal("Hello, "), Expression(name), Literal("!")]`
- **Affected**: `lexer.rs`, `parser/mod.rs`, `ast.rs`, all `.bt` files

### Phase 2: Codegen + Runtime
- Generate binary construction: `<<literal_bytes, (printString_result)/binary, ...>>`
- Insert `printString` dispatch for non-literal segments via object system
- Plain strings (no `{expr}`) compile to simple binary literals as today — zero overhead
- Ensure `printString` is implemented on all stdlib classes (currently implemented on 18 core classes)
- Update `tests/stdlib/string_interpolation.bt` and `test-package-compiler/cases/future_string_interpolation/main.bt`
- **Affected**: `codegen/core_erlang/expressions.rs`, `lib/*.bt`, `tests/stdlib/`, `test-package-compiler/cases/`

### Phase 3: Documentation
- Update `docs/beamtalk-language-features.md` — string section, examples
- Update `docs/beamtalk-syntax-rationale.md` — interpolation and quote sections
- Update `AGENTS.md` — syntax examples, test format examples
- Update `examples/*.bt` and `README.md`

### Estimated Size: L (lexer/parser/codegen changes + mechanical file updates)

## Migration Path

### Single-Quote to Double-Quote Migration

All existing `.bt` files must be updated. This is a mechanical transformation:

```bash
# For each .bt file, replace single-quoted strings with double-quoted
# Must be careful to preserve:
# - #'quoted symbols' (single quotes in symbol context)
# - $'  character literals (already use $, not quotes)
# - Escaped single quotes within strings
```

The migration can be done in one batch commit before interpolation is implemented, keeping the two changes separate and reviewable.

### No User Code Migration

The language is pre-release — no external user code needs migration.

## References
- Related issues: BT-39 (Define string interpolation syntax)
- Related ADRs: ADR 0003 (Core Erlang target), ADR 0021 (Streams and IO Design)
- Elixir charlist deprecation discussion: https://groups.google.com/g/elixir-lang-core/c/_pAjPEayLLI
- Elixir gotchas (charlist confusion): https://pragtob.wordpress.com/2024/05/01/10-elixir-gotchas/
- Swift string interpolation: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/stringsandcharacters/
- Kotlin string templates: https://kotlinlang.org/docs/strings.html#string-templates
- Beamtalk syntax rationale: `docs/beamtalk-syntax-rationale.md`
- Beamtalk language features: `docs/beamtalk-language-features.md`
