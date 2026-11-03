# ADR 0143: Binary Construction Syntax

## Status
Implemented (2026-11-03); accepted 2026-11-01. Epic BT-4138.
Amended 2026-11-03 (BT-4380) to match the
match-side semantics that shipped in BT-4143 (#4729) and BT-4376 (#4759),
and to settle the design questions the first draft left open (§ Segment
values, § Literal checks, § String values, § Result class, § Runtime
failures).

> **Implemented note (2026-11-03).** Shipped as described, except: the
> run-time error names the failing segment's type and value but not yet its
> position (§ Runtime failures; BT-4406: codegen does not emit the Core
> `{segment, N}` annotation, so the BEAM reports index 0), and the `String` unwrap is not yet skipped for values
> statically known to be `Binary` (§ String values; BT-4407). A
> parenthesised string literal (`<<("hi")/binary>>`) is still a string
> literal, so it gets the same compile error as `<<"hi"/binary>>`.

## Context

BT-4096/4097/4098 completed binary pattern *matching*: `<<n:8, payload:n/binary>>`
parses in match arms and lowers to Core Erlang binary matching. Erlang's other
half — *building* binaries — has no Beamtalk equivalent. Encoding stays manual:

```beamtalk
frame := Binary fromBytes: #(16, 3) concat: payload
```

Buccaneer needs construction for HTTP/2 frame encoding, where a frame is a
mix of integer fields, flags, and payload bytes. The manual route works but
hides the wire layout and cannot express sub-byte fields or sizes computed
from values in one place.

### Current state

- Match side: `<<...>>` parses in patterns only, with `value:size/type`
  segments (`crates/beamtalk-core/src/ast/pattern.rs` `BinarySegment`).
  BT-4143 (#4729) added `utf16`/`utf32`, `unit:N`, and float, string and
  character literal values; BT-4376 (#4759) aligned the unit rules and
  float-segment literals with `erlc`. The shipped rules are summarised in
  `docs/beamtalk-language-features.md` § Binary segment values and checks;
  this ADR reuses them unchanged.
- Expression side: `Expression` (`crates/beamtalk-core/src/ast/expression.rs`)
  has **no** binary-construction variant. The only lowering that emits Core
  binary construction (`#{...}#`) is `StringInterpolation`, which builds UTF-8
  text via `printString` (ADR 0023).
- `BinarySegment`'s value is a `Pattern` and its `size` is an `Expression`;
  construction needs `Expression` values with optional size/type specifiers.

### Constraints

- `<` is reserved as a binary message (parenthesis generics were chosen over
  `Result<T, E>` for exactly this reason). `<<` lexes as its own token
  (`LtLt`), the pattern delimiter in match arms; it is not a binary selector
  (it has no binding power and cannot be declared as a method). So a
  `<<...>>` expression form is decided by position: leading `<<` where an
  operand is expected opens construction, and infix `<<` (`a << b`) is a
  targeted parse error ("'<<' is not a binary operator"), never a construction.
- The match and construction grammars should not drift into two dialects:
  one segment vocabulary to learn, one Core shape to lower.
- Construction must compose with sizes computed from values
  (`<<len:(header size), payload:len/binary>>`), since frame encoding needs it.

## Decision

Reuse `<<...>>` in **expression position** with the **same segment grammar**
as patterns. One grammar, two directions: in a match arm `<<...>>`
destructures, in an expression it builds.

```beamtalk
len := payload size
frame := <<16:8, 3:8, len:24, payload/binary>>
frame match: [<<16:8, 3:8, n:24, rest:n/binary>> -> rest toBytes]
```

Segment shape (mirrors `BinarySegment`, values are expressions):

```beamtalk
<<v:4>>                        // integer value, 4 bits
<<p/binary>>                   // whole binary
<<s:8/unit:2>>                 // size 8, unit 2 (16 bits)
<<1.5/float>>                  // float literal
<<$A>>                         // character literal (65)
<<"hi">>                       // string literal (raw bytes, no /binary needed)
<<h:16/big-unsigned-integer>>   // spelled type chain, same vocabulary as patterns
```

REPL sketch (after implementation):

```text
> payload := Binary fromBytes: #(104, 105)
> <<16:8, payload/binary>> size
3
> <<16:8, payload/binary>> toBytes
#(16, 104, 105)
```

Diagnostic examples:

```text
> <<v:4>>                      // v unbound
ERROR: unresolved identifier 'v'
> <<(-1):4>>                   // literal out of range: warns, as in patterns
WARNING: the value -1 does not fit in an 'unsigned' binary segment of size 4 and is truncated to 15
> <<"hi"/binary>>              // can never succeed: error, as in patterns
ERROR: a literal string in a binary pattern must not have a type or a size
```

Core lowering is the same `#{...}#` construction the match side and string
interpolation already emit. Verified against `erlc +to_core` (2026-11-01):

```erlang
%% t1(V, P) -> <<V:4, P/binary>>.
#{#<V>(4,1,'integer',['unsigned'|['big']]),
  #<P>(('all'|'compiler_generated'),8,'binary',['unsigned'|['big']])}#
%% <<N:8/unit:2>>  ->  (8,2,'integer',['unsigned'|['big']])
%% <<1.5/float>>   ->  8 constant integer segments (constant-folded bytes)
%% <<$A>>          ->  #{#<65>(8,1,'integer',['unsigned'|['big']])}#
%% <<"hi"/utf16-big>> -> #{#<0>...,#<104>...,#<0>...,#<105>...}#
```

Note: `erlc` does **not** reject `<<"hi"/binary>>` as a construction. It
warns ("binary construction will fail because of a type mismatch") and the
expression raises `badarg` at run time; in a pattern the same segment is a
compile error ("a literal string in a binary pattern must not have a type or
a size"). Beamtalk makes it a compile error in both directions (§ Literal
checks). The construction spelling for embedded bytes is the bare string
literal `<<"hi">>`; the `/binary` suffix is for variables, as on the match
side.

### Segment values

`name:size` lexes as one keyword token (`Keyword("name:")`), so a segment
value cannot be an arbitrary expression: `<<x size:8>>` would read as the
keyword send `x size: 8`. A construction segment value is therefore
restricted to what the match side already accepts in that position, plus
parentheses:

- a variable (`v`, `v:8`, `v/binary`);
- a literal: integer (optionally negative, `-1:8/signed`), float, character
  or string;
- a parenthesised expression, for anything else: `<<(payload size):16>>`,
  `<<(self.len):8>>`, `<<(a + b):4>>`.

Sizes keep the pattern rule: an integer literal, a variable, or a
parenthesised expression. Anything else is a parse error whose hint is to
parenthesise. One segment parser serves both directions; the value
production differs only in admitting a parenthesised expression where a
pattern admits `_`.

### Literal checks

One policy for both directions: **a literal that cannot do what its
segment asks is a warning, unless the segment can never succeed at all, in
which case it is an error.** Concretely:

| Shape | Pattern (shipped, #4729/#4759) | Construction (this ADR) | `erlc` construction |
|---|---|---|---|
| Integer literal does not fit (`<<300>>`, `<<(-1):4>>`) | warning: arm never matches | **warning**; the value is truncated to its low bits | silent truncation (`<<15:4>>`) |
| Float literal in a non-float segment (`<<1.5>>`) | warning | **warning**; raises at run time | warning, run-time `badarg` |
| Integer literal in a `binary`/`bits` segment | warning | **warning**; raises at run time | warning, run-time `badarg` |
| Float size not 16/32/64 bits | warning | **warning** | warning |
| Non-ASCII character in a default 8-bit segment (`<<$é>>`) | warning | **warning**; builds byte 233 | silent |
| Type/size on a string literal (`<<"hi"/binary>>`) | error | **error** | warning, run-time `badarg` |
| Negative literal size; size/unit on `utf*`; `unit:` without a size on integer/float; conflicting `unit:` | error | **error** | error |

Why warnings for misfits rather than errors (the first draft said errors):

- **Parity with the match side.** #4729 made pattern misfits warnings,
  matching `erlc`. One grammar should give one verdict for the same
  literal; a value that warns in a match arm and errors when built would be
  two dialects.
- **The BEAM defines the result.** A misfit literal in construction is not
  undefined: it truncates exactly as a variable holding the same value
  does. Making the literal an error while the variable silently truncates
  would punish the case the compiler can see and pass the one it cannot.
- **Strictness is still available.** The warning names the bad literal;
  `--warnings-as-errors` turns it into an error, and `@expect` silences a
  deliberate one (an all-ones mask written as `-1`, say).
- Beamtalk is still stricter than `erlc`, which truncates `<<(-1):4>>` and
  `<<$é>>` without a word.

The error rows are the shapes that can never produce a value. `<<"hi"/binary>>`
always raises; reporting that at compile time costs nothing and is what the
match side already does through the shared `check_binary_literal_shapes`.

### String values

A `String` is the tagged term `{beamtalk_string, Bytes}` (BT-4173), so a
String held in a variable is not a BEAM binary. Construction treats it the
way the match side treats a String subject, by its UTF-8 bytes:

- In a `binary`/`bytes`/`bits`/`bitstring` segment, a `String` value
  contributes its UTF-8 bytes (`<<name/binary>>` with `name := "hé"` builds
  `#(104, 195, 169)`). Codegen unwraps the tag: a `{beamtalk_string, B}`
  value becomes `B`, and any other value passes through unchanged for the
  BEAM to check. The unwrap is skipped when the value is statically a
  `Binary`.
- A string **literal** with no specifiers is its UTF-8 bytes, as in
  patterns: `<<"é">>` builds `#(195, 169)`. This deliberately differs from
  `erlc`, where `<<"é">>` is the single byte 233 (each character truncated
  to 8 bits). With `/utf8`, `/utf16` or `/utf32` a string literal encodes
  one character at a time.
- In an `integer`, `float` or `utf*` segment a `String` value is a type
  error: a warning when the type checker knows the value is a `String`,
  otherwise the run-time failure below.

### Result class

Construction always builds bytes, never text: the result is never a
`String` (class is construction type, BT-4170; use `asString` or
`String fromIolist:` to get one). Per ADR 0141 the class follows the
shape:

- a `Binary` when the total bit size is a multiple of 8;
- a `Bitstring` otherwise (`<<1:3>>`, `<<1:4, 1:5>>`).

Statically, the type checker answers `Binary` when every segment's bit size
is known and the sum is a multiple of 8 (literal sizes times units, default
sizes, and `binary`/`bytes`/`utf*` segments, which are whole bytes), and
`Bitstring` when the known sum is not. When a `bits`/`bitstring` value or a
variable size with a sub-byte unit leaves alignment unknown, it answers
`Bitstring | Binary`, the union `Bitstring>>concat:` already answers;
callers narrow with a `:: Binary` match arm.

### Runtime failures

A construction that fails at run time (a non-integer in an `integer`
segment, a variable size that is negative or not an integer, a non-binary
in a `binary` segment, a `binary` segment whose size exceeds the value)
raises the BEAM's `badarg`, with an `error_info` cause naming the segment
(`{SegmentIndex, Type, Reason, Value}`, OTP's `format_bs_fail`).

Generated code does not wrap construction in `try`: success stays one
allocation with no handler frame. The raw `badarg` reaches the existing
conversion point, `beamtalk_exception_handler:wrap_raw/2`, which `on:do:`,
`ensure:`, actor boundaries and the REPL already use, and which classifies
`badarg` as an `argument_error` `#beamtalk_error{}`. Phase 2 extends that
path to read the `format_bs_fail` cause, so the message names the segment
and value ("binary construction: segment 2 expects a binary, got 1")
instead of the generic "invalid argument".

## Prior Art

- **Erlang**: one `<<...>>` form for both directions; segment specifiers
  (`Size/Type-Signedness-Endianness-Unit`) identical in match and construction.
  We adopt the shape and the shared vocabulary. We do not adopt silent
  truncation of literals: an out-of-range literal warns, as it does in
  patterns (§ Literal checks), and a string literal stands for its UTF-8
  bytes rather than one truncated byte per character.
- **Elixir**: same `<<>>` reuse (`<<v::4, p::binary>>`). Confirms the shared
  grammar is learnable. Nothing to reject.
- **Smalltalk**: no bit syntax; fields built with shifts and masks or
  `ByteArray` writes. A pure-message alternative (`Binary build: [...]`)
  would preserve message-passing purity but buries the wire layout in nested
  arrays and needs its own specifier DSL anyway.
- **Gleam**: `<<v:size(4), p:bytes>>` — same reuse idea with keyword-ish
  specifiers. Our `:` separator already plays that role.

## User Impact

- **Newcomer**: one `<<...>>` to learn; the match examples in the guide read
  directly as construction. Sizes computed from values work the same way in
  both directions.
- **Smalltalk developer**: a syntax extension, not a message. Justified as in
  ADR 0023: binary layout is a data literal, like string interpolation, and
  the Buccaneer frame case is unreadable as nested sends.
- **Erlang/BEAM developer**: segments lower to the verified Core shapes above;
  `observer`/`dbg` see ordinary binary construction.
- **Operator**: construction allocates one flat binary, as `<<...>>` does in
  Erlang; no iolist laziness.
- **Tooling**: one segment vocabulary shared with patterns; unparse must
  round-trip both. Leading `<<` opening construction while infix `<<` is a
  parse error is a parser-position rule, documented in Implementation.

## Steelman Analysis

- **Reuse `<<...>>` (chosen)**: Newcomer — "I copy the match example and it
  builds." Smalltalker — "A literal for a literal; interpolation set the
  precedent." BEAM veteran — "Same Core I already debug." Operator — "One
  allocation, predictable."
- **`Binary build:` message form**: Smalltalker — "Everything stays a message;
  no grammar risk." Newcomer — "Long, but greppable." Rejected on layout
  readability for the frame case, not on principle.
- **Interpolation-only (no new syntax)**: Designer — "Smallest change."
  Rejected: text-only, cannot express integer fields, units, or endianness.

Tension: Smalltalk purity vs one shared Erlang-shaped literal. Precedent
(ADR 0023 compiling interpolation to binary construction) already resolved
this trade-off toward the literal.

## Alternatives Considered

### `Binary build: #()` message form

```beamtalk
frame := Binary build: #(#(16 size: 8), #(payload spec: #binary)))
```

No grammar change, fully greppable. Rejected: the wire layout disappears
into nested arrays, and the specifier DSL (`size:`, `spec:`) duplicates the
pattern vocabulary with different punctuation — two dialects for one thing.

### Interpolation-only extension

Stretch `"..."` interpolation to cover bytes. Rejected: interpolation is
text via `printString`; integer bit fields, units, and endianness do not
belong in string semantics.

### Separate construction-only delimiters

E.g. `#b[...]` or `<<|...|>>`. Rejected: a third spelling for the same
segments, with no disambiguation benefit once the leading-`<<` rule exists.

## Consequences

### Positive
- One segment grammar for match and build; guide examples compose
  (build-then-match round-trips).
- Frame encoding becomes a layout, not nested sends.
- Core shapes already verified; lowering reuses the existing `#{...}#` path.

### Negative
- Parser gains a position-dependent `<<` rule: leading `<<` opens
  construction, infix `<<` is a targeted error rather than a send.
- New `Expression` variant plus type-checker/codegen/unparse threading (L).

### Neutral
- Shared specifier tables (`BinarySegmentType`, signedness, endianness)
  extend to expressions; no duplicated vocabulary. Conformance via
  build-then-match round-trip tests plus the example corpus.

## Implementation

1. AST: `Expression::BinaryConstruction { segments, span }` reusing
   `BinarySegmentType`/`BinarySignedness`/`BinaryEndianness`; values and sizes
   are `Expression`s restricted as in § Segment values. Parser and type checker run in the caller for any
   blocks (class-process rule, ADR 0142): segments contain value expressions,
   not blocks into class methods.
2. Parser: leading-`<<` in operand position parses construction; infix `<<`
   is a targeted parse error (`<<` was never a binary selector). Same
   specifier parser and the same `check_binary_literal_shapes` as patterns
   (§ Literal checks), with the "can never match" wording reworded for
   construction ("will be truncated", "will fail").
3. Codegen: emit `#{...}#` per the verified shapes; literal floats fold as
   `erlc` does; bare string literals emit their UTF-8 bytes; `String`
   values in byte segments are unwrapped (§ String values). Type checker:
   result class per § Result class. Runtime: a `format_bs_fail` clause in
   `wrap_raw/2` (§ Runtime failures).
4. Unparse: round-trip both directions with identical spelling.
5. Tests: build-then-match round-trips incl. sizes from values; unparse
   corpus; guide section next to patterns.
6. BT-4143 (#4729) and BT-4376 (#4759) have merged, so the shared files
   (`parser/expressions.rs`, `unparse/mod.rs`, guide,
   `binary_patterns_test.bt`) are free.

## Implementation Tracking

**Epic:** BT-4138
**Issues:** BT-4346 (Phase 1: AST + parser + unparse) → BT-4347 (Phase 2: codegen, blocked by BT-4346) → BT-4348 (Phase 3: tests + e2e + docs, blocked by BT-4347)
**Status:** Implemented — BT-4346 (#4782), BT-4347 (#4787), BT-4348; follow-ups BT-4406, BT-4407
**Note:** BT-4142 (design + build monolith) retired as superseded by this split; BT-4143 (match-side parity) landed in #4729 and BT-4376 (`erlc` unit/float alignment) in #4759; BT-4380 amended this ADR to match them.

## References
- Related issues: BT-4142 (superseded), BT-4143, BT-4097, BT-4173, BT-4376, BT-4380, BT-4346, BT-4347, BT-4348
- Related ADRs: ADR 0023 (interpolation compiles to binary construction),
  ADR 0141 (Bitstring class for unaligned tails), ADR 0142 (class-process
  caller rule), ADR 0052 (Pratt binary-operator parsing)
- Documentation: `docs/beamtalk-language-features.md` § Binary patterns and
  § Binary segment values and checks
