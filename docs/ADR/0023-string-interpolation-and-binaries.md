# ADR 0023: String Interpolation Syntax and Compilation

## Status
Proposed (2026-02-14)

## Context

Beamtalk needs string interpolation — the ability to embed expressions inside string literals. This is "table stakes" for a modern language (see `docs/beamtalk-syntax-rationale.md`), but the design involves several coupled decisions:

1. **Syntax**: How does the programmer indicate "this string has interpolation"?
2. **Delimiter**: How are embedded expressions delimited within the string?
3. **Compilation**: What BEAM construct does interpolation compile to?
4. **Conversion**: How are non-string values converted to strings?

### Current State

- **Strings are UTF-8 binaries** (`<<"...">>` in Erlang) — this is settled
- The **lexer** already distinguishes double-quoted strings (`"..."`) from single-quoted (`'...'`), tokenizing doubles as `InterpolatedString` — but this was a preliminary choice, not a deliberate design decision
- The **parser, AST, and codegen** have no interpolation support
- `test-package-compiler/cases/future_string_interpolation/main.bt` documents expected behavior using the preliminary `"Hello, {name}!"` syntax
- `docs/beamtalk-syntax-rationale.md` briefly mentions interpolation but with minimal rationale

### Constraints

- Must produce a **binary** (not charlist, not iolist) — beamtalk strings are binaries everywhere
- Must work with **UTF-8** content including emoji and CJK characters
- Must compose with existing **message-send** semantics — expressions inside interpolation should be normal beamtalk expressions
- Should not conflict with **existing syntax**: `{a, b}` tuples, `#{...}` maps, `$a` character literals, `[...]` blocks
- Must be **lexer-friendly** — tokenization should be straightforward, not require parser feedback

## Decision

### 1. Prefix Syntax: `f'...'`

Use an `f` prefix on single-quoted strings to indicate interpolation:

```beamtalk
name := 'Alice'
age := 30

// Interpolated string (f-prefix)
greeting := f'Hello, {name}!'

// Plain string (no prefix)  
literal := 'Hello, {name}!'     // => Hello, {name}!

// Expressions inside braces
message := f'{name} is {age} years old'

// Message sends
info := f'Length: {name length}'

// Operators
result := f'Sum: {2 + 3}'

// Nested message sends
detail := f'Upper: {name uppercase}'

// Multi-line
report := f'Name: {name}
Age: {age}
Status: active'
```

Both single and double quotes remain available for plain strings (no behavioral difference without the `f` prefix). This departs from the preliminary design where double quotes implied interpolation.

### 2. Expression Delimiter: `{expr}`

Embedded expressions use `{` and `}`:

```beamtalk
f'Value: {expr}'
```

Literal braces are escaped with backslash:

```beamtalk
f'Set notation: \{1, 2, 3\}'   // => Set notation: {1, 2, 3}
'Already literal: {no problem}' // No prefix, no interpolation
```

### 3. Compilation: Binary Construction

Interpolation compiles to **Erlang binary construction**, producing a flat binary:

```beamtalk
// Source
f'Hello, {name}!'
```

```erlang
%% Generated Core Erlang
let _Name_str = call 'beamtalk_string':'printString'(Name) in
  #{#<72>(8,1,'integer',['unsigned'|['big']]),
    #<101>(8,1,'integer',['unsigned'|['big']]),
    ...
    (_Name_str)/binary,
    #<33>(8,1,'integer',['unsigned'|['big']])}#
```

This is the same approach Elixir uses: eager binary construction, not iolists. The result is always a binary (String), which means `length`, `at:`, `++`, etc. all work immediately.

### 4. Auto-Conversion: `printString` Message

Non-string values are converted via `printString`:

```beamtalk
age := 30
f'Age: {age}'   // age printString => '30', then binary concat
```

The `printString` message is already the standard way to get a string representation in beamtalk (following Smalltalk convention). This is analogous to:
- Elixir's `String.Chars.to_string/1` protocol
- Python's `__str__`
- Ruby's `to_s`
- Pharo's `asString`

If `printString` is not understood by the receiver, the standard `doesNotUnderstand:` error is raised — no silent failures.

### REPL Session

```
> name := 'Alice'
Alice
> f'Hello, {name}!'
Hello, Alice!
> f'2 + 2 = {2 + 2}'
2 + 2 = 4
> f'{name length} chars'
5 chars
> 'No interpolation: {name}'
No interpolation: {name}
> f'Braces: \{escaped\}'
Braces: {escaped}
```

### Error Examples

```
> f'Hello, {undefined}'
ERROR: UndefinedObject does not understand 'printString'
  Hint: Variable 'undefined' is not defined in this scope

> f'Missing close brace {name'
ERROR: Unterminated interpolation expression at line 1
  Hint: Add closing '}' to complete the expression
```

## Prior Art

| Language | Syntax | Prefix? | Delimiter | Compiles to | Auto-convert |
|----------|--------|---------|-----------|-------------|--------------|
| **Elixir** | `"Hello #{name}"` | No (quote-style) | `#{}` | Binary `<>` | `to_string` protocol |
| **Gleam** | `"Hello ${name}"` | No (planned) | `${}` | Binary concat | No (type-checked) |
| **Pharo** | `'Hi {1}' format: {name}` | No (message) | N/A | Message send | `asString` |
| **Python** | `f"Hello {name}"` | Yes (`f`) | `{}` | `str.__format__` | `__str__` |
| **Ruby** | `"Hello #{name}"` | No (quote-style) | `#{}` | String concat | `to_s` |
| **C#** | `$"Hello {name}"` | Yes (`$`) | `{}` | `String.Format` | `ToString()` |
| **Swift** | `"Hello \(name)"` | No (built-in) | `\()` | String concat | `description` |
| **Erlang** | None | N/A | N/A | N/A | N/A |

### What We Adopted

- **Python/C# prefix approach**: Explicit `f` prefix makes intent visible. The reader always knows whether a string interpolates.
- **`{expr}` delimiters**: Clean, minimal — same as Python/C#. No extra sigil needed (`#` or `$`).
- **Elixir's binary compilation**: Eager binary construction is simple, correct, and matches beamtalk's "strings are binaries" model.
- **Smalltalk's `printString`**: Auto-conversion uses the existing object protocol, not a separate trait/interface.

### What We Rejected

- **Elixir/Ruby's quote-style distinction**: See Alternatives below.
- **Gleam's type-checked approach**: Beamtalk is dynamically typed; requiring only String arguments would be unnecessarily restrictive.
- **Pharo's `format:` message**: Too verbose for common use. `f'Hello, {name}!'` is clearer than `'Hello, {1}!' format: {name}`.
- **Swift's `\()` delimiter**: Unfamiliar to most developers; `{}` is more widely understood.

## User Impact

### Newcomer (from Python/JS/Ruby)
- `f'...'` is immediately familiar from Python — zero learning curve
- Both quote styles work for plain strings — no "which quote does what?" confusion
- Error messages guide them when they forget `f` or misuse braces

### Smalltalk Developer
- Departure from Smalltalk (Pharo uses `format:`) but justified — interpolation is syntactic sugar for a very common pattern
- `printString` conversion preserves Smalltalk's object protocol
- Single-quoted strings remain the default, matching Smalltalk convention

### Erlang/BEAM Developer
- Compiles to familiar binary construction (`<<...>>`)
- No iolist surprises — result is always a binary
- `printString` maps to a well-defined message, predictable at the BEAM level

### Production Operator
- No runtime overhead beyond necessary string conversion
- No lazy evaluation or deferred allocation (unlike iolists)
- Deterministic memory behavior — binary is allocated once

## Steelman Analysis

### Option A: Quote-Style `"Hello, {name}!"` (Rejected)

| Cohort | Best argument for this option |
|--------|------------------------------|
| 🧑‍💻 **Newcomer** | "Ruby, Elixir, and Swift all use quote-style — it's the most common approach, and one less character to type" |
| 🎩 **Smalltalk** | "Smalltalk only has single-quoted strings; adding double-quotes with special behavior is a clean extension that doesn't break anything" |
| ⚙️ **BEAM veteran** | "Elixir uses this exact pattern and it works great — no reason to diverge from what the BEAM ecosystem already does" |
| 🏭 **Operator** | "Fewer characters in source = slightly smaller source files; prefix is unnecessary ceremony" |
| 🎨 **Designer** | "The cleanest syntax is one with the fewest sigils — quotes already carry meaning, no need for another marker" |

### Option B: Prefix `f'Hello, {name}!'` (Chosen)

| Cohort | Best argument for this option |
|--------|------------------------------|
| 🧑‍💻 **Newcomer** | "Python's f-strings taught me this — I immediately know what `f'...'` means. With quote-style, I'd need to memorize which quote does what" |
| 🎩 **Smalltalk** | "Single-quoted strings stay as simple strings, just like Smalltalk. The `f` prefix is an explicit opt-in to non-Smalltalk behavior — honest about the departure" |
| ⚙️ **BEAM veteran** | "Erlang has no interpolation at all. An explicit prefix is a clear signal that 'something special is happening here' — easier to grep for, easier to review" |
| 🏭 **Operator** | "When I'm debugging a production issue and reading code, `f'...'` immediately tells me 'expressions are evaluated here' — I know where to look for side effects" |
| 🎨 **Designer** | "This keeps the string system orthogonal: quotes control quoting, prefixes control behavior. If we later add raw strings (`r'...'`), regex (`re'...'`), or heredocs, the prefix system scales" |

### Option C: Message Send `'Hello, {1}' format: {name}` (Rejected)

| Cohort | Best argument for this option |
|--------|------------------------------|
| 🎩 **Smalltalk** | "This is pure Smalltalk — no syntax extensions, just a message. Pharo does exactly this and it works fine" |
| 🎨 **Designer** | "No parser changes needed. Everything stays as messages. Maximum orthogonality." |

### Tension Points

- **Newcomers and BEAM veterans agree**: Prefix is clearest (Python familiarity + explicit intent)
- **Smalltalk purists are split**: Some prefer message-only purity, but most accept interpolation as justified sugar
- **Language designers are split**: Quote-style is more minimal, but prefix scales better to future string types
- **Quote-style's hidden cost**: Overloading single vs double quotes means newcomers must learn "which quote does what" — this is the #1 beginner confusion in Ruby and Bash

## Alternatives Considered

### Alternative A: Quote-Style Distinction

```beamtalk
name := 'Alice'
greeting := "Hello, {name}!"     // Double quotes = interpolated
literal := 'Hello, {name}!'      // Single quotes = literal
```

**Why rejected:**
- Overloads quote characters with behavioral semantics — newcomers must memorize which quote interpolates
- Every double-quoted string is parsed for `{expr}` even when not intended — `"JSON: {key}"` would be misinterpreted
- Single vs double quote distinction is the #1 beginner confusion in Ruby and Bash
- Reduces flexibility: can't use double quotes for a plain string
- The preliminary design in the syntax rationale doc chose this without thorough analysis

### Alternative B: Sigil Delimiter (`${expr}` or `#{expr}`)

```beamtalk
f'Hello, ${name}!'    // $ prefix on expressions
f'Hello, #{name}!'    // # prefix on expressions
```

**Why rejected:**
- `#{` is already taken — it's the map literal syntax (`#{#x => 1}`)
- `$` is used for character literals (`$a` = 97) — visual confusion
- Extra character inside every interpolation adds noise
- `{expr}` alone is unambiguous since the `f` prefix already signals interpolation

### Alternative C: Iolist Compilation

```erlang
%% Could compile to iolist instead of binary
[<<"Hello, ">>, Name, <<"!">>]
```

**Why rejected:**
- Beamtalk strings are binaries — returning an iolist would break `length`, `at:`, `uppercase`, etc.
- Elixir and Gleam both compile interpolation to binary, not iolist
- Iolists are an optimization concern, not a user-facing string concern
- A future `StringBuilder` class or Stream pattern (see ADR 0021) can provide iolist semantics for performance-critical output building

## Consequences

### Positive
- Explicit syntax — readers always know when interpolation is active
- Familiar to Python/C# developers (largest language communities)
- Scales to future string prefixes (`r'raw'`, `re'regex'`, `b'bytes'`)
- Clean compilation to BEAM binary construction — no iolist complexity
- `printString` auto-conversion leverages existing object protocol
- No ambiguity with existing syntax (`{}` tuples, `#{}` maps, `$a` chars)

### Negative
- Departs from the preliminary design documented in `beamtalk-syntax-rationale.md` and `beamtalk-language-features.md` — docs must be updated
- One extra character (`f`) compared to quote-style distinction
- `f` prefix is not a message — adds non-message syntax to a message-oriented language
- Lexer must recognize `f'` as a token prefix, not an identifier followed by a string

### Neutral
- Double-quoted strings become identical to single-quoted strings (both are plain string literals)
- The `InterpolatedString` token kind in the lexer will be repurposed for `f'...'` strings
- `test-package-compiler/cases/future_string_interpolation/main.bt` must be updated to use `f'...'` syntax
- Iolist optimization remains available as a future concern via StringBuilder/Stream

## Implementation

### Phase 1: Lexer + Parser
- Recognize `f'...'` (and `f"..."`) as interpolated string tokens
- Parse `{expr}` segments as embedded expressions
- Add `StringInterpolation` AST node with segments: `[Literal("Hello, "), Expression(name), Literal("!")]`
- **Affected**: `lexer.rs`, `parser/mod.rs`, `ast.rs`

### Phase 2: Codegen
- Generate binary construction: `<<literal_bytes, (printString_result)/binary, ...>>`
- Insert `printString` calls for non-literal segments
- **Affected**: `codegen/core_erlang/expressions.rs`

### Phase 3: Runtime + Tests
- Ensure `printString` is implemented on all stdlib classes (Integer, Float, Boolean, List, etc.)
- Add stdlib tests in `tests/stdlib/string_interpolation.bt`
- Add compiler tests in `test-package-compiler/cases/`
- **Affected**: `lib/*.bt`, `tests/stdlib/`, `test-package-compiler/cases/`

### Phase 4: Documentation + Cleanup
- Update `docs/beamtalk-language-features.md`
- Update `docs/beamtalk-syntax-rationale.md`
- Remove or update preliminary `InterpolatedString` lexer support for `"..."` (make double quotes equivalent to single quotes)

### Estimated Size: L (spans lexer, parser, AST, codegen, runtime, docs)

## Migration Path

The preliminary `"Hello, {name}!"` syntax was never implemented — no user code needs migration.

Documentation updates:
- `docs/beamtalk-syntax-rationale.md` — Update interpolation section
- `docs/beamtalk-language-features.md` — Update string section
- `test-package-compiler/cases/future_string_interpolation/main.bt` — Update to `f'...'`

## References
- Related issues: BT-39 (Define string interpolation syntax)
- Related ADRs: ADR 0003 (Core Erlang target), ADR 0021 (Streams and IO Design)
- Python PEP 498: Literal String Interpolation (f-strings)
- Elixir String module: https://hexdocs.pm/elixir/String.html
- Pharo string interpolation plugin: https://github.com/guillep/pharo-string-interpolation
- Beamtalk syntax rationale: `docs/beamtalk-syntax-rationale.md`
- Beamtalk language features: `docs/beamtalk-language-features.md`
