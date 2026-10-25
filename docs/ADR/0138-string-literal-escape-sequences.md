# ADR 0138: String Literal Escape Sequences

## Status
Accepted (2026-10-25)

## Context

Beamtalk string literals have no usable escape sequences. The lexer
(`lex_string` in `crates/beamtalk-core/src/source_analysis/lexer.rs`) recognizes
only `""` (a literal `"`) and `\{` / `\}` (a literal brace — the backslash is
consumed; the language guide's claim that it is preserved is stale, see below).
Any other backslash sequence is passed through verbatim: `"GET / HTTP/1.1\r\n"`
is 18 characters — `GET / HTTP/1.1` plus literal backslash-r-backslash-n.
There is no
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

Every one of them is literal backslash-n today. A fourth exhibit lives in the
repo's own prior ADR: ADR 0023 documents
`"JSON-like: \{\"key\": 42\}" // => JSON-like: {"key": 42}` — written as if
`\"` consumed the backslash, which it does not.

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

- Edge-case literal meanings may change (accepted for this decision), but the
  breakage must be bounded and reviewable: unknown escapes fail loudly, and
  the six in-table sequences are the only silent reinterpretations.
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
never) — with one caution for a future author: `\b` has a live PCRE meaning
(word boundary), so migrated `\\b` regex sites assume `\b` stays an error;
never add backspace-`\b` without re-auditing them. Each deferral is
documented with its reason so the next person does not relitigate it.

Known wart introduced here: `"\0"` is NUL while `$\0` still yields `0`
(U+0030) via the untouched `$` fall-through. Accepted alongside the rest of
`$` (see below); `$` gets no new behavior in this ADR.

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

### One mapping, every direction, pinned by a corpus

The lexer, `printString`, and the unparser share one escape mapping. Three
languages cannot literally share one table (Rust lexer, Beamtalk
`printString`, Erlang-adjacent consumers), so the mapping is pinned by a
shared conformance corpus instead: lex-value ↔ `printString` ↔ `fmt`
round-trips over every escape, including braces and backslashes. Once
`"a<LF>b"` exists, logs and debuggers must emit `\n` or produce ambiguous
output — this is in scope, not a follow-up. (`printString` keeps `""`
doubling for quotes and escapes `\` first, then `"`, `{` (as `\{` — without
this a brace-containing string re-parses as interpolation and the mandated
round-trip test fails), CR, LF, TAB, NUL. Bare `}` is safe to leave.)

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
| **Rust / Python** | Raw strings (`r"…"` / `r"…"`) alongside escapes | The literal-prefix precedent. Rejected for v1: `\\d` regexes are standard-issue annoyance (Java, Python-without-`r`), not a gap — and unknown-escape-as-error already makes every such site loud. If demand ever materializes, the revisit path is ADR 0012's reserved `#r"raw string"` (and `#/…/` regex), not a new sigil design. |

Smalltalk's `''` doubling is the ancestor we deliberately depart from by
*adding* `\"` alongside the kept `""` — additive, so existing literals using
`""` are untouched.

## User Impact

- **Newcomer**: `"a\nb"` and `"say \"hi\""` finally do what every other
  language taught them. Unknown escapes error with a pointer instead of
  silently misbehaving. Discoverable in the REPL in one line. Cost: regexes
  need doubling (`"\\d"`, the Java-style annoyance) — no raw strings in v1.
- **Smalltalk developer**: a pragmatic departure in the ADR 0039 line
  (`//` comments, PEMDAS, interpolation). `""` doubling still works, so old
  muscle memory survives; the break is pick-uppable in minutes. Cost: backslash
  escapes in strings are un-Smalltalk, full stop — this is the departure, and
  the security exhibit is the justification.
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

### Rejected alternative: warn on backslash, no escapes

- 🧑‍💻 **Newcomer**: "I get told about every `\` without breaking my build."
- 🎩 **Smalltalk purist**: "No syntax change at all — the language stays minimal."
- ⚙️ **BEAM veteran**: "Cheapest possible change; dialyzer-style nudges work."
- 🏭 **Operator**: "Zero migration risk; warnings don't break deploys."
- 🎨 **Language designer**: "Reversible: warn now, decide later with data."

Genuine strength, and it fails on one point: warnings rot, and the Headers
bug is the proof — a passing suite with a warning nobody reads ships the same
vulnerability. (Status quo has no steelman worth writing: nobody defends a
demonstrated silent failure on a security boundary.)

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
removed. Revisit only if `\\`-heavy domains (regex) demand it — via ADR
0012's reserved `#r"…"`, not a new design.

### Strictness without reinterpretation
Error on every `\x` except `\{ \} ""` (no new meanings at all), with CRLF
spelled via new stdlib constants (`String crlf`, …). This kills the silent
class with *zero* reinterpretation risk — strictly safer than the chosen path
— and was the strongest unconsidered option at review time. Not chosen: the
BT-4100 acceptance criterion is "CR LF writable without a helper", and the
judgment call is that `"a\r\n"` writability (the Elixir-convergent shape every
downstream HTTP package wants) is worth the six bounded reinterpretations over
constants-plus-error. If the breakage audit turns up worse than expected, this
is the fallback: keep the error machinery, drop the new meanings.

### Warn on backslash, no escapes
Keep passthrough but warn on any `\x`. Rejected: warnings rot (see every
`@expect` ever); the Headers bug would have shipped with a warning nobody
read. An error is the only honest response to an unrepresentable intent.

### Do nothing (status quo)
Leave the lexer as-is; HTTP code keeps the `Character value:` helper.
Rejected: it preserves the demonstrated silent-failure class on a security
boundary, and every downstream package (catalina, buccaneer, exdura) pays the
helper tax forever. The migration cost of deciding now is bounded and
mechanical; the cost of every future developer re-discovering the footgun is
not.

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
  and `\\` change meaning *silently* (no diagnostic — the ratchet catches
  only non-table escapes). Unknown `\x` becomes a compile error, which is
  loud and mechanical (`\` → `\\`), but the six in-table sequences need
  human review at every site: grep any `\` before `r n t " \ 0` and judge
  intent. Blind `\` → `\\` replacement corrupts `\{` (into backslash +
  interpolation start). Audit surface: stdlib, corpus/examples,
  `docs/learning` (executed via `test-learn`), `docs/*.md` snippets, and
  downstream packages (catalina, buccaneer, http/yaml/collections) — whose
  in-table sites cannot be audited from here, so "no silent drift" is
  asserted for this repo, not for them.
- `\"` alongside `""` is a mild two-spellings wart (kept: erroring on `\"`
  would be hostile to every newcomer).
- `\{` uses in migrated code need eyes, not just search-replace (behavior
  already = literal brace; verify each site meant that).

### Neutral

- `{expr}` interpolation, `""` doubling, `$` literals: unchanged.
- No runtime cost: purely compile-time (lexer) plus `printString` formatting.

## Implementation

1. **Lexer** (`source_analysis/lexer.rs`, `lex_string`): implement the six
   escapes; unknown escape → `TokenKind::Error` with span + `\\` hint, using
   the existing lowercase-diagnostic convention (a message starting with a
   lowercase letter, as the single-quote removal does) so the parser reports
   it as complete-but-invalid rather than incomplete input. Phases: table,
   error path, unit tests (each escape, unknown-each, `""`/`\{` regression,
   `"\r\n" size =:= 1` + `Character`-built equality).
2. **`printString`** (`stdlib/src/string.bt` + builtins regen): escape `\`
   first, then `"`, CR, LF, TAB, NUL; round-trip tests
   (re-parse `printString` output, compare identical). This extends, not
   contradicts, ADR 0094: `printString` is Debug, and unambiguous Debug
   output is the point.
3. **Unparser** (`unparse/leaf.rs`, `string_content`): ship-blocking. It
   emits raw backslashes today, so after this change `fmt` would emit source
   that re-parses to a different value or fails to compile. Extend it to
   escape `\\` (mandatory) and control characters canonically (`\n`, `\r`,
   `\t`), with formatter round-trip tests over escape-containing values
   (idempotency property tests cover this shape). Reconcile the two
   producers while here: `escape_string_literal` (`unparse/mod.rs`, used by
   `tool_expr`, MCP editing, hover, codegen doc comments, stdlib metadata)
   emits `\"` where `leaf` emits `""` — pick one convention for generated
   code and document it. `char_lit` already emits the table — no change
   there. Note `escape_string_literal` already emits `\"`, which misparses
   today (backslash-quote, 2 chars) and becomes correct under this ADR —
   another exhibit for the decision.
3. **Docs**: rewrite the guide's § Strings escape table (including correcting
   the stale `\{` "backslash preserved" note); fix the three `\n`-as-newline
   doc exhibits (`string.bt:330`, `inspector.bt:245`, guide § Character
   Methods).
4. **Migration audit** (verified inventory, not a guess):
   - `docs/learning/06-strings.md`: `multiline size // => 22` becomes 20 —
     executed via `test-learn` (`test-docs --warnings-as-errors`), so this
     goes red on day one. Update value + prose.
   - `examples/sicp/.../scheme_reader.bt` `isWs:`: `#(" ", "\n", "\t", "\r")`
     silently starts matching real whitespace (today it matches nothing) —
     intended behavior arriving by accident; re-verify the scheme tests'
     intent.
   - Self-consistent fixtures (`change_log_test`, `subprocess_test` printf,
     `console_test`): pass before *and* after while testing different
     inputs — verify intent, don't just re-bless.
   - `uuid_test.bt`: compile-blocked until `\z` → `\\z` (the loud case
     working as designed); the trailing-`"\n"` cases then test new behavior.
   - `just build-corpus` + builtins regen (`check-corpus` freshness fails
     otherwise), catalina, buccaneer (including new `"\r"` literals),
     http/yaml/collections, `docs/*.md` snippets.
5. **Conformance, not a shared table**: three languages cannot share one
   table, so pin the mapping with a shared corpus — extend
   `string_and_character_literal_span_corpus.json` or add an escape corpus
   asserting lex-value ↔ `printString` ↔ `fmt` round-trips. The Rust-side hub
   is `unparse_literal_display` (already the documented single source of
   truth for literal-to-source rendering); `printString` is the Beamtalk
   side; the corpus keeps them honest.
6. **Error-channel acceptance**: the unknown-escape diagnostic's span must
   cover the offending backslash; the message starts lowercase (REPL
   completeness convention); verify the REPL prints the error rather than a
   continuation prompt for `"\q"`.

Effort: S (lexer + tests) + S (`printString` + unparser + round-trip) + migration sweep.

## Migration Path

Two classes of breakage, handled differently:

- **Loud (unknown `\x`)**: compile error pointing at the backslash. Fix is
  mechanical: write `\\x` for a literal backslash (regex `\d` → `\\d`).
  `uuid_test.bt`'s `\z` is the in-repo example.
- **Silent (in-table `\r \n \t \" \\ \0`)**: meaning changes with no
  diagnostic. NOT mechanical: grep every `\` before `r n t " \ 0` and judge
  intent at each site. `C:\new\test` (path) becomes `C:` + LF + `ew` +
  TAB + `est` — the reviewer must decide `C:\\new\\test`. Never bulk
  `\` → `\\`: it corrupts `\{` into backslash + interpolation start.

`\{` / `\}`: unchanged behavior (literal brace); confirm each site rather
than bulk-replacing. Downstream packages get loud errors for unknown
escapes on upgrade; their in-table sites need the same human grep — run it
before upgrading catalina, buccaneer, http/yaml/collections, exdura.

## References

- Related issues: BT-4100 (this decision), BT-4079 (Buccaneer spikes),
  BT-4019 (Catalina spikes, `telemetry` FFI context)
- Related ADRs: ADR 0012 (reserved `#r"…"`, `#/…/` — the revisit path),
  ADR 0023 (double quotes, interpolation), ADR 0039 (syntax pragmatism),
  ADR 0086 (string subclass of binary), ADR 0094 (`printString` = Debug
  contract, extended here)
- Documentation: `docs/beamtalk-language-features.md` (§ Strings,
  § Character Methods)
