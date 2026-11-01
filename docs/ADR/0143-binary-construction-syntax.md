# ADR 0143: Binary Construction Syntax

## Status
Accepted (2026-11-01)

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
  BT-4143 (in flight) extends it with `utf16`/`utf32`, `unit:`, and literal
  segment values.
- Expression side: `Expression` (`crates/beamtalk-core/src/ast/expression.rs`)
  has **no** binary-construction variant. The only lowering that emits Core
  binary construction (`#{...}#`) is `StringInterpolation`, which builds UTF-8
  text via `printString` (ADR 0023).
- `BinarySegment`'s value is a `Pattern` and its `size` is an `Expression`;
  construction needs `Expression` values with optional size/type specifiers.

### Constraints

- `<` is reserved as a binary message (parenthesis generics were chosen over
  `Result<T, E>` for exactly this reason). `<<` already lexes as a binary
  selector for sends (`a << b`) and as the pattern delimiter in match arms,
  so any `<<...>>` expression form must disambiguate by position: leading
  `<<` where an operand is expected opens construction; infix `<<` stays a send.
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

Error examples (all `#beamtalk_error{}`):

```text
> <<v:4>>                      // v unbound
ERROR: unresolved identifier 'v'
> <<(-1):4>>                   // literal out of range, checked as in patterns (BT-4097)
ERROR: binary segment value -1 does not fit in 4 bits
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

Note: `<<"hi"/binary>>` as a *construction* with a literal string is rejected
by `erlc` (`badarg` at compile time); the construction spelling for embedded
bytes is the bare string literal `<<"hi">>`. The `/binary` suffix is for
variables, as on the match side.

## Prior Art

- **Erlang**: one `<<...>>` form for both directions; segment specifiers
  (`Size/Type-Signedness-Endianness-Unit`) identical in match and construction.
  We adopt the shape and the shared vocabulary. We do not adopt implicit
  integer defaults silently: out-of-range literals are compile-time errors,
  as decided for patterns.
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
  round-trip both. The leading-`<<` vs infix-`<<` disambiguation is a
  parser-position rule, documented in Implementation.

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
- Parser gains a position-dependent `<<` rule (leading vs infix); needs
  precise diagnostics when a send was meant.
- New `Expression` variant plus type-checker/codegen/unparse threading (L).

### Neutral
- Shared specifier tables (`BinarySegmentType`, signedness, endianness)
  extend to expressions; no duplicated vocabulary. Conformance via
  build-then-match round-trip tests plus the example corpus.

## Implementation

1. AST: `Expression::BinaryConstruction { segments, span }` reusing
   `BinarySegmentType`/`BinarySignedness`/`BinaryEndianness`; values and sizes
   are `Expression`s. Parser and type checker run in the caller for any
   blocks (class-process rule, ADR 0142): segments contain value expressions,
   not blocks into class methods.
2. Parser: leading-`<<` in operand position parses construction; infix `<<`
   stays a binary send. Same specifier parser as patterns (BT-4143 output).
3. Codegen: emit `#{...}#` per the verified shapes; literal floats fold as
   `erlc` does; bare string literals emit raw bytes.
4. Unparse: round-trip both directions with identical spelling.
5. Tests: build-then-match round-trips incl. sizes from values; unparse
   corpus; guide section next to patterns.
6. Sequence **after BT-4143 merges** (shared files: `parser/expressions.rs`,
   `unparse/mod.rs`, guide, `binary_patterns_test.bt`).

## Implementation Tracking

**Epic:** BT-4138
**Issues:** BT-4346 (Phase 1: AST + parser + unparse) → BT-4347 (Phase 2: codegen, blocked by BT-4346) → BT-4348 (Phase 3: tests + e2e + docs, blocked by BT-4347)
**Status:** Planned
**Note:** BT-4142 (design + build monolith) retired as superseded by this split; BT-4143 (match-side parity) landed in #4729.

## References
- Related issues: BT-4142 (superseded), BT-4143, BT-4097, BT-4346, BT-4347, BT-4348
- Related ADRs: ADR 0023 (interpolation compiles to binary construction),
  ADR 0141 (Bitstring class for unaligned tails), ADR 0142 (class-process
  caller rule), ADR 0052 (Pratt binary-operator parsing)
- Documentation: `docs/beamtalk-language-features.md` § Binary patterns
