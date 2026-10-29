# ADR 0141: Bitstring — a Sealed Class for Unaligned Bitstrings

## Status
Implemented (2026-10-28)

## Context

BT-4097 added `bits`/`bitstring` segments to binary patterns, so a pattern can
bind a tail that is not a whole number of bytes:

```beamtalk
(Binary fromBytes: #(234)) match: [<<_:3, rest/bitstring>> -> rest]
```

`rest` here is 5 bits. It matches again fine, but nothing else works. An
unaligned bitstring fails `is_binary/1`, so `beamtalk_primitive:class_of/1`
fell through to `'Object'`, and `module_for_value/1` had no arm for it. Every
`Binary`/`Collection` selector, including the two most natural ones (`rest
size`, `rest toBytes`), raised `Object does not understand`, and the value
printed as a generic Object. No semantics were defined: is `size` bits or
bytes? Does `toBytes` pad, truncate, or raise?

This is a type-level question, not a lowering fix: the value needs a class.

## Decision

### The class follows the value's shape

A new sealed leaf class, `Bitstring`, is a **sibling** of `Binary`:

```beamtalk
sealed typed Value subclass: Bitstring native: beamtalk_bitstring
```

The class is decided by the value's shape, as Erlang decides it. A bitstring
whose bit size is a multiple of 8 **is** a binary, and has class `Binary` (or
`String` when tagged). Any other bitstring is a `Bitstring`. No tag is needed:

- `class_of(X) when is_bitstring(X) -> 'Bitstring'`, after the `is_binary` arm.
- `module_for_value/1` maps it to `bt@stdlib@bitstring`.

So an operation can turn one into the other. Joining 1 bit and 7 bits answers
a `Binary`, just as `<<A/bitstring, B/bitstring>>` does in Erlang.

### Protocol

| Selector | Answer |
|---|---|
| `bitSize` | Integer, the number of bits (`bit_size/1`) |
| `toBits` | `List(Integer)` of 0/1, most significant bit first |
| `concat: aBitstringOrBinary` | the joined bits: a `Binary` when the total is byte-aligned, else a `Bitstring`. A `String` argument is joined by its bytes, as in `Binary>>concat:` |
| `asBinary` | `Result(Binary, Error)`. Always an error for a `Bitstring`: kind `#unaligned_bitstring`, hint naming `bitSize` and `toBits`. No silent padding or truncation |
| `printString` / `displayString` | Erlang notation, e.g. `<<2:3>>` (what `io_lib:format("~w")` prints) |
| `=:=`, `==`, `hash` | inherited structural equality |

`Binary` gains `bitSize` (8 × its byte count; a `String` counts its UTF-8
bytes), so `bitSize` works on either class.

Everything else on `Binary`/`Collection` stays bytes-only and is
**deliberately not understood** by `Bitstring`: `size`, `do:`, `at:`,
`byteAt:`, `part:size:`, `toBytes`, `includesSubstring:`, `indexOf:`,
`asString`, `asStringUnchecked`, `asBase64`, `asBase64Url`, `asHex`, and all
Collection protocol (`collect:`, `select:`, `inject:into:`, `isEmpty`,
`includes:`, …).

```beamtalk
tail := (Binary fromBytes: #(234)) match: [<<_:3, rest/bitstring>> -> rest]
tail class        // => Bitstring
tail bitSize      // => 5
tail toBits       // => #(0, 1, 0, 1, 0)
tail printString  // => "<<10:5>>"
tail asBinary     // => Result error: (unaligned_bitstring)
tail size
// => does_not_understand: Bitstring does not understand 'size'
//    Hint: a Bitstring is not byte-aligned (5 bits); use 'bitSize'/'toBits',
//    or 'asBinary' to check alignment
```

### Where the does-not-understand error is raised

The error is the structured `does_not_understand` (`#beamtalk_error{}`, class
`Bitstring`, the selector), with the hint above. It is raised by
`beamtalk_primitive:send/3`'s bitstring arm: a selector that
`bt@stdlib@bitstring:has_method/1` rejects raises it directly; every other
selector dispatches through the compiled class as usual.

It is deliberately **not** a `Bitstring>>doesNotUnderstand:args:` override in
`bitstring.bt`. A class that overrides `doesNotUnderstand:args:` is "open" to
the compiler: the type checker stops reporting unknown selectors on it,
protocol conformance treats it as conforming to every protocol (ADR 0025 tier
3), and `SystemNavigation dnuHandlers` lists it. That is the opposite of
"deliberately not understood": `tail size` on a `Bitstring`-typed receiver
should be a compile-time warning, and `tail respondsTo: #size` must answer
`false`. Raising from the send path keeps the class closed and gives the same
runtime error.

### Patterns

`b :: Bitstring` is a legal type pattern (sealed leaf). Its test is the
guard-safe `is_bitstring(X) andalso not is_binary(X)`. `:: Binary` is
unchanged (BT-4228): a `Bitstring` is not a `Binary`. A `/bitstring` segment
binds whatever the bits are, so its binding is `Binary` or `Bitstring` at run
time; the type checker does not type segment bindings, so nothing changes
statically.

## Prior Art

- **Erlang** has one type with a subset: every `binary()` is a `bitstring()`.
  `is_binary/1` is `is_bitstring/1` plus alignment. We keep Erlang's shape rule
  and its operations (`bit_size/1`, `<<A/bitstring, B/bitstring>>`), but not
  its subset relation as a class relation (see Alternatives).
- **Elixir** follows Erlang: `is_bitstring/1` and `is_binary/1`, with
  `String`/`Binary` functions defined only for binaries. Calling a binary
  function on an unaligned bitstring raises `ArgumentError`, which is our
  "deliberately not understood" in a dynamically typed setting.
- **Smalltalk** has `ByteArray` and no bit-level type; bit fields are read with
  integer masks. Nothing to adopt.

## User Impact

- **Newcomer:** an unaligned tail now prints as `<<10:5>>` and answers a
  helpful error on `size`, naming the bit count and the right messages.
- **Smalltalk developer:** a sealed class with a small, closed protocol; no
  surprising `size` units.
- **Erlang/BEAM developer:** the class follows `is_binary`/`is_bitstring`
  exactly; concatenation crosses back to `Binary` as it does in Erlang.
- **Operator:** no runtime cost on any other value: the extra `send/3` arm
  matches only unaligned bitstrings.

## Steelman Analysis

- **Bitstring as Binary's superclass** (Erlang developer): "It *is* the subset
  relation; `isKindOf: Bitstring` on a binary should be true." True in Erlang's
  type language, but `Binary` is a `Collection` of bytes whose `size` counts
  bytes. A `Bitstring` superclass would need `size` in bits, or no Collection
  protocol at all, and `Binary` would then override `size` with a different
  unit, which breaks substitutability.
- **Pad or truncate on `toBytes`** (pragmatist): "Just give me the bytes." Every
  padding rule invents bits, every truncation loses them, silently. `asBinary`
  answering a `Result` makes the alignment question explicit instead.

## Alternatives Considered

### `Bitstring` as the superclass of `Binary`

Erlang's subset relation as inheritance. Rejected: `Binary`'s `Collection`
protocol counts bytes, so `Binary` would override `size` with a different unit
than its superclass, breaking substitutability. It would also widen `::
Binary` / `isKindOf:` reasoning beyond BT-4228's closed `{Binary, String}`
subtree.

### Unaligned values as `Binary`, with a per-method unaligned branch

`size`, `at:` and `do:` have no honest byte answer for 3 bits. Every padding or
truncation rule silently invents or loses data.

### Forbid unaligned tails at run time

Removes BT-4097's bit-level protocol parsing (`<<flags:3, v:5,
rest/bitstring>>`). Rejected.

### A `Bitstring>>doesNotUnderstand:args:` override

Gives the same runtime error, but makes the class open to the type checker and
protocol conformance (see Decision). Rejected in favour of raising from the
send path.

## Consequences

### Positive
- Unaligned bitstrings have a class, a printed form and a defined protocol.
- Every byte-level message on them fails with one exact, explained error.
- `:: Bitstring` patterns let code branch on alignment.

### Negative
- One more primitive class to keep in the routing fixture
  (`tagged_map_routing_conformance.json`) and the codegen `TYPE_TESTS` table.
- A `Bitstring` has no constructor of its own: values come from patterns,
  from `concat:`, or from Erlang.

### Neutral
- `Binary` gains `bitSize`; nothing else on `Binary` changes.

## Implementation

- Runtime: `beamtalk_primitive` — `class_of/1`, `module_for_value/1`,
  `print_string/1`, `display_string/1`, and the `send/3` bitstring arm.
- Stdlib: `stdlib/src/bitstring.bt` + `beamtalk_bitstring.erl`;
  `Binary>>bitSize` (`beamtalk_binary:bitSize/1`).
- Codegen: `TYPE_TESTS` entry `Bitstring`; `NON_TAGGED_MAP_CLASSES` entry.
- Tests: `stdlib/test/bitstring_test.bt`, `binary_patterns_test.bt`, runtime
  EUnit.
- Docs: language guide binary-patterns section and class reference.

## References
- Related issues: BT-4139, BT-4097, BT-4228
- Related ADRs: ADR 0025 (gradual typing and protocols), ADR 0107 (type
  patterns)
- Documentation: `docs/beamtalk-language-features.md` § Binary patterns
