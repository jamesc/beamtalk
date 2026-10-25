# ADR 0138: String Literal Escape Sequences

## Status
Accepted (2026-10-25)

## Context

Beamtalk string literals have no usable escape sequences. The lexer
(`lex_string` in `crates/beamtalk-core/src/source_analysis/lexer.rs`) recognizes
only `""` (a literal `"`) and `\{` / `\}` (a literal brace — the backslash is
consumed; the language guide's claim that it is preserved is stale, see below).
Any other backslash sequence is passed through verbatim: `"GET / HTTP/1.1\r\n"`
is the 19 characters `GET / HTTP/1.1\r\n`, backslashes included. There is no
`String crlf`/`String cr` either, so HTTP code — status lines, headers, chunk
framing, raw test requests — spells CRLF via
`(Character value: 13) asString ++ (Character value: 10) asString` (BT-4100).

### The silent failure

This is not an aesthetics complaint. `"a\rb"` *looks like* CR to every
developer and means backslash-r, and nothing says so. The Buccaneer spikes
proved the danger: a Headers CR/LF-rejection check — a security boundary —
passed all its tests while dead against real data, because the tests' `"\r\n"`
literals never contained carriage returns. That is the kind of bug that
justifies a breaking lexer change.

The codebase is full of exhibits. These are written as if `\n` were newline:

- `stdlib/src/string.bt:330`: `"a\nb\nc" lines // => #("a", "b", "c")`
- `stdlib/src/inspector.bt:245`: `// => "Inspector(Point)\n  x: 3\n  y: 4"`
- Language guide § Character Methods:
  `(Character value: 10) asString // => "\n" (LF, not "10")`

Every one of them is literal backslash-n today.

### Current state

- Strings are double-quoted only (ADR 0023); all support `{expr}`
  interpolation; they compile to UTF-8 binaries (ADR 0086).
- `\{` / `\}` produce a literal brace (backslash consumed). Proven by
  `string_interpolation_test.bt` (`"\{" size equals: 1`) and by the working
  `uuid_test.bt` regex `"^[0-9a-f]\{8}-…"`, which only matches if `\{` lexes to
  `{`. The guide's § Strings table ("backslash preserved") contradicts the
  code and the tests; the code wins.
- `\"` today yields backslash-quote (2 chars) while keeping the string open —
  the same silent class as `\r`, and the first thing every newcomer tries.
- `$` character literals already support `$\n $\t $\r $\\ $'`
  (`lex_character`); anything else after `$\` falls through silently to the
  bare character.
- `String printString` doubles internal quotes and wraps in quotes; it does
  not escape anything else, so a string containing a real newline already
  prints ambiguously.

### Constraints

- Edge-case literal meanings may change (accepted for this decision); the
  breakage must be loud and mechanical, never silent.
- The decision must keep `{expr}` interpolation, `""` doubling, and the
  double-quotes-only rule (ADR 0023) intact.

## Decision

**Six escapes, lexed in the string; unknown escape is a compile error;
`printString` shares the table and round-trips.**

### The table (a ceiling, not a starting bid)

| Escape | Meaning |
|--------|---------|
| `\r` | CR (U+000D) |
| `\n` | LF (U+000A) |
| `\t` | TAB (U+0009) |
| `\\` | backslash |
| `\"` | `"` (alongside the existing `""` doubling, which stays valid) |
| `\0` | exactly NUL (U+0000) — no C-style octal continuation |

Boundary principle: in-tree use today, or tried by every newcomer; nothing
byte-oriented, nothing archaic. `\xHH` is out — it fights the UTF-8 string
model (`\xFF` = error? latin-1?) and needs real design; deferred with
prejudice. `\u{…}` is out — covered by `Character value:` /
`String fromCodePoints:` until someone needs astral literals. `\e` is out
(niche; `Character value: 27` works). `\a \b \f \v` are out (archaic, likely
never). Each deferral is documented with its reason so the next person does
not relitigate it.

Existing behavior kept: `""` → `"`; `\{` / `\}` → literal brace (enshrined;
the guide is corrected, no behavior change).

### Unknown escape = compile error

This is the whole safety case, and what makes "six" safe. A backslash followed
by anything outside the table (today's `\d` regexes, `C:\path`s, the
`uuid_test.bt` `\z`) fails compilation with an error pointing at the
backslash, suggesting `\\` for a literal one. Every existing offender becomes
a loud, one-time, pointed-at-`\` fix instead of a silent behavior change. The
rule is also a ratchet: anything omitted fails loudly, so the set can only
widen deliberately. That is the answer to "why not more" — more can come later
without breaking anything.

### One table, both directions

The lexer and `printString` share the escape mapping, with round-trip tests:
`printString` output re-parses to the identical string. Once `"a<LF>b"` exists,
logs and debuggers must emit `\n` or produce ambiguous output — this is in
scope, not a follow-up. (`printString` keeps `""` doubling for quotes and adds
backslash escapes for `\`, CR, LF, TAB, NUL; backslash is escaped first.)

### com.example.Grapheme unity

Escapes must produce byte-identical binaries to the `Character value:` path so
segmentation agrees. Acceptance: `"\r\n" size =:= 1` (CRLF is one grapheme)
and equality with the `Character`-built string.

### `$` character literals: untouched

`$\n` and friends already work; `Character value: 10` covers the rest. The
silent fall-through in `lex_character` (unknown `$\x` yields `x`) is noted as
a known wart for later, out of scope here.

```beamtalk
// Before (BT-4100 workaround):
nl := (Character value: 13) asString ++ (Character value: 10) asString
req := "GET / HTTP/1.1{nl}host: x{nl}{nl}"

// After:
req := "GET / HTTP/1.1\r\nhost: x\r\n\r\n"
```

REPL:

```beamtalk
> "a\nb" size
=> 3
> "a\nb" printString
=> ""a\nb""
> "\q"
× Unknown escape '\q' — use '\\' for a literal backslash
```

## Prior Art

| Language | Strings | What we take |
|----------|---------|--------------|
| **Elixir** | `"…"` with `\n \r \t \\ \" \xHH \u…`, `#{…}` interpolation, `~S` no-escape sigil | Closest reference: escapes + interpolation in double quotes on BEAM. We take a deliberately smaller table, no sigils for v1. |
| **Erlang** | `"…"` with C-style escapes incl. octal | Same runtime conventions; we reject octal continuation (`\0` is exactly NUL). |
| **Gleam** | `"…"` with `\n \r \t \\ \" \u{…}` | Same shape on BEAM; supports `\u{}` which we defer. |
| **Pharo/Squeak** | `'…'`, no backslash escapes, `''` doubling, `String crlf` / `String tab` constants | The ancestor and the stdlib-constants precedent. Rejected as the *primary* fix: constants don't fix `"a\rb"` looking like CR — the silent failure this ADR exists to kill. Kept as a complementary nicety implementers may add. |
| **Rust / Python** | Raw strings (`r"…"` / `r"…"`) alongside escapes | The literal-prefix precedent. Rejected for v1: `\\d` regexes are standard-issue annoyance (Java, Python-without-`r`), not a gap — and unknown-escape-as-error already makes every such site loud. |

Smalltalk's `''` doubling is the ancestor we deliberately depart from by
*adding* `\"` alongside the kept `""` — additive, so existing literals using
`""` are untouched.

## User Impact

- **Newcomer**: `"a\nb"` and `"say \"hi\""` finally do what every other
  language taught them. Unknown escapes error with a pointer instead of
  silently misbehaving. Discoverable in the REPL in one line.
- **Smalltalk developer**: a pragmatic departure in the ADR 0039 line
  (`//` comments, PEMDAS, interpolation). `""` doubling still works, so old
  muscle memory survives; the break is pick-uppable in minutes.
- **Erlang/BEAM developer**: the table is the Elixir subset they expect;
  generated code stays plain UTF-8 binaries. No new runtime machinery.
- **Operator**: no hot-reload or performance implications (compile-time
  only). Logs become *less* ambiguous via `printString` round-tripping.
- **Tooling developer**: one table in one place (`lex_string` + shared
  `printString` mapping); unknown-escape errors come with spans. No AST
  change — strings still lex to the same token kinds.

## Steelman Analysis

### Rejected alternative: stdlib constants only (`String crlf`, …)

- 🧑‍💻 **Newcomer**: "No new syntax to learn — I just call a method."
- 🎩 **Smalltalk purist**: "This is the Pharo way — `String crlf` is proven over decades."
- ⚙️ **BEAM veteran**: "Zero lexer risk; it's just library code."
- 🏭 **Operator**: "Nothing breaks, nothing migrates."
- 🎨 **Language designer**: "Keeps the literal syntax minimal."

Genuine strength, and it fails on one point: it leaves `"a\rb"` meaning
backslash-r — the exact silent failure, on a security boundary, that motivated
the ADR.

### Rejected alternative: raw/escaped literal prefix

- 🧑‍💻 **Newcomer**: "Rust/Python-style `r"…"` is familiar, and old literals never change meaning."
- 🎩 **Smalltalk purist**: "Single-quoted strings could come back as raw — Smalltalk gets its quotes back."
- ⚙️ **BEAM veteran**: "Elixir's `~S` proves sigils work on BEAM."
- 🏭 **Operator**: "Fully backward compatible; zero migration."
- 🎨 **Language designer**: "Two literal kinds, each honest — no silent reinterpretation."

Genuine strength, and it fails on two: it doesn't fix `"a\rb"` either (the
default literal stays a footgun), and it spends the "which quote?" confusion
ADR 0023 deliberately killed. V1 doesn't need it; `\\d` is annoyance, not gap.

### Tension points

- Purists and operators prefer zero-breakage options; the security exhibit
  overrules them — a check that passes while dead is worse than a loud
  migration.
- Newcomers and BEAM veterans agree on Elixir-style escapes; the disagreement
  is only about table size, settled by the ratchet (unknown = error).

## Alternatives Considered

### Stdlib constants only
`String cr / lf / crlf / tab` as the whole fix. Rejected: leaves the silent
`"a\rb"` failure in place. May still be added as a complement.

### Raw/escaped literal prefix
A second literal form (sigil, `r"…"`-style, or revived single quotes) carrying
escapes while plain `"…"` stays literal. Rejected for v1: doesn't fix the
default literal's silent failure; reintroduces the quote-confusion ADR 0023
removed. Revisit only if `\\`-heavy domains (regex) demand it.

### Warn on backslash, no escapes
Keep passthrough but warn on any `\x`. Rejected: warnings rot (see every
`@expect` ever); the Headers bug would have shipped with a warning nobody
read. An error is the only honest response to an unrepresentable intent.

## Consequences

### Positive

- The `"a\rb"` silent-failure class is eliminated: every backslash sequence
  either means something standard or fails compilation with a pointer.
- HTTP/CRLF-heavy code becomes writable and readable
  (`"GET / HTTP/1.1\r\nhost: x\r\n\r\n"`).
- `printString` output becomes unambiguous and re-parseable.
- The escape set can widen later without breaking anything (ratchet).

### Negative

- Breaking change to literals containing backslashes: `\r \n \t \" \0`
  change meaning; all other `\x` become compile errors. Migration is
  mechanical (`\` → `\\`) but touches stdlib, corpus/examples, docs, and
  downstream packages (catalina, buccaneer, http/yaml/collections).
- `\"` alongside `""` is a mild two-spellings wart (kept: erroring on `\"`
  would be hostile to every newcomer).
- `\{` uses in migrated code need eyes, not just search-replace (behavior
  already = literal brace; verify each site meant that).

### Neutral

- `{expr}` interpolation, `""` doubling, `$` literals: unchanged.
- No runtime cost: purely compile-time (lexer) plus `printString` formatting.

## Implementation

1. **Lexer** (`source_analysis/lexer.rs`, `lex_string`): implement the six
   escapes; unknown escape → `TokenKind::Error` with span + `\\` hint.
   Phases: table, error path, unit tests (each escape, unknown-each,
   `""`/`\{` regression, `"\r\n" size =:= 1` + `Character`-built equality).
2. **`printString`** (`stdlib/src/string.bt` + builtins regen): escape `\`
   first, then `"`, CR, LF, TAB, NUL; round-trip tests
   (re-parse `printString` output, compare identical).
3. **Docs**: rewrite the guide's § Strings escape table (including correcting
   the stale `\{` "backslash preserved" note); fix the three `\n`-as-newline
   doc exhibits (`string.bt:330`, `inspector.bt:245`, guide § Character
   Methods).
4. **Migration audit**: stdlib, compiler corpus/examples, catalina,
   buccaneer (including new `"\r"` literals), http/yaml/collections, doc
   snippets. Known in-repo loud breaks: `uuid_test.bt` `\z` → `\\z`;
   `change_log`/`console`/`subprocess` `\n` literals (re-verify each test's
   intent — several were written *as if* `\n` were newline and may now pass
   for the first time, or need `\\n`).
5. **Single source of truth**: the escape mapping lives in exactly one shared
   table used by both the lexer and `printString` (same crate boundary rules
   as ever — grep before adding a second copy); round-trip tests enforce it.

Effort: S (lexer + tests) + S (`printString` + round-trip) + migration sweep.

## Migration Path

- `\` followed by `r n t " 0` or `\` takes the new meaning — intended.
- Any other `\x` is a compile error pointing at the backslash: write `\\x`
  for a literal backslash (regex `\d` → `\\d`, paths `C:\t` → `C:\\t`).
- `\{` / `\}`: unchanged behavior (literal brace); confirm each site during
  the audit rather than bulk-replacing.
- Downstream packages get the same loud errors on upgrade; no silent drift.

## References

- Related issues: BT-4100 (this decision), BT-4079 (Buccaneer spikes),
  BT-4019 (Catalina spikes, `telemetry` FFI context)
- Related ADRs: ADR 0023 (double quotes, interpolation), ADR 0039 (syntax
  pragmatism), ADR 0086 (string subclass of binary)
- Documentation: `docs/beamtalk-language-features.md` (§ Strings,
  § Character Methods)
