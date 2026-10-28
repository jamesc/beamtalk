# ADR 0140: Text-Default Binaries — a String Is the Raw Binary, Binary Is Explicit

## Status
Proposed (2026-10-28)

## Context

### What BT-4170 decided

Before epic BT-4170, a BEAM binary's class was decided by its *content*. A binary holding valid UTF-8 dispatched as `String`, and anything else as `Binary` (BT-2999). That made class depend on data. `do:` yielded different element kinds for different contents (BT-4168), and a protocol buffer could change class halfway through a parse when its bytes happened to form valid UTF-8.

BT-4170 fixed this with a rule worth keeping: **class follows construction, never content.** It implemented the rule by choosing a default representation:

* a bare binary is always a `Binary` (bytes);
* a `String` is the tagged term `{beamtalk_string, Bin}`, produced only by string literals, text-returning natives and validating conversions (`asString`, `asStringUnchecked`).

### What the default costs

Most binaries crossing from Erlang into Beamtalk are text, so under this default each one must be converted explicitly. An audit of the sibling repos (2026-10-28, `beamtalk-{buccaneer,catalina,collections,exdura,http,symphony,uat,watcher,yaml}`) found:

| Binaries from Erlang used as **text** | Sites |
|---|---|
| native `.erl` modules tagging results as `String` | 133 (`beamtalk-http` 67 — cowboy fields; `beamtalk-yaml` 66) |
| `asStringUnchecked` in `.bt` code | 52 (http method/path, HTTP response bodies, subprocess output lines) |
| `Headers stringOrOpaque:` in buccaneer | 27 (`decode_packet` methods, paths, header names and values) |
| **total** | **≈ 210** |

| Binaries from Erlang used as **bytes** | Sites |
|---|---|
| entry points: socket `recv` (gen_tcp/ssl), `decode_packet` rest buffers, `file:read_file`, `binary:copy` | ≈ 10, all in buccaneer and catalina |
| downstream byte operations (`part:size:`, `byteAt:`, `byte_size`, binary patterns) | a few dozen, behind those entry points |

These are counts of code sites, not of traffic. Two things stand out:

* **Text outnumbers bytes about 20 to 1, and bytes are concentrated.** Byte data enters through a handful of networking and storage entry points in two repos.
* **The default pushed application code back into sniffing.** buccaneer's `stringOrOpaque:` is "valid UTF-8 → `String`, else leave it", which is the content-based rule BT-4170 removed from the runtime, now handwritten in an application.

### The interop bug no boundary rule fixes

A tagged `String` is never Erlang-equal to the same text as a raw binary. Beamtalk-built data holds tagged Strings, while Erlang-built data (`json:decode` maps, cowboy headers, `binary:split` results) holds raw binaries. Any rule at the FFI boundary converts the wrong way for one of the two populations:

```beamtalk
Erlang lists member: "a" with: #("a")              // => false today (raw "a" vs tagged "a")
Erlang maps get: "k" with: decodedJsonMap          // => works today (raw "k" vs raw key)
```

Today's shallow unwrap gets the second right and the first wrong. Passing Strings tagged would get the first right and the second wrong (BT-4243; see "Spec-directed conversion" under Alternatives). The mismatch lives in the representation itself, not in the boundary.

### Constraints

1. **Keep BT-4170's rule.** A value's class follows how it was constructed. No runtime decides a class from content.
2. **Nothing fails silently** (the maintainer's requirement). Where a wrong choice is possible, it must fail loudly or be diagnosed.
3. **Erlang interop needs no conversion** for the common case (text).
4. Bytes stay a first-class, distinct type for the code that needs them: networking, storage, crypto and serialization.
5. Every Beamtalk surface (REPL, `beamtalk run`, `beamtalk test`, releases) behaves the same (CLAUDE.md: no image mechanics).

## Decision

**Flip the default representation. A raw BEAM binary is a `String`. A `Binary` is the explicitly constructed tagged term `{beamtalk_binary, Bin}`.**

Class still follows construction: a raw binary is *always* a `String`, whatever its content, and a `Binary` exists only where code constructed one. Only which side carries the tag changes.

### 1. Representation and class

| Value | Representation | Created by |
|---|---|---|
| `String` | raw binary `<<"…">>` | string literals, string operations, every binary Erlang returns |
| `Binary` | `{beamtalk_binary, Bin}` | `Binary fromBytes:`, `aString asBinary`, binary-pattern `/binary` segments, byte-level natives, `Binary serialize:` |

* `beamtalk_primitive:class_of/1` gives `String` for any raw binary and `Binary` for the tag. A non-byte-aligned bitstring stays outside both classes; the `Bitstring` class is specified in BT-4139.
* The class hierarchy is unchanged: `sealed Binary subclass: String`.
  * `x :: String` tests `is_binary(X)`.
  * `x :: Binary` stays subclass-aware: the tag **or** a raw binary (BT-4228's two-shape test, mirrored).
  * `isKindOf:` agrees with both.
* `aString asBinary` wraps the bytes in the tag, with no copy. `aBinary asString` keeps its BT-4169 contract: `Result(String, Error)`, validating UTF-8. `asStringUnchecked` unwraps without validation.

### 2. Invalid UTF-8 in a String is loud

A raw binary from Erlang is a `String` even when its bytes are not valid UTF-8. That is construction, not content: it came from Erlang, and Erlang's default is text. Methods that need graphemes (`size`, `at:`, `do:`, `reverse`, `asUppercase`, `includesSubstring:`, …) **raise** a structured error on invalid UTF-8:

```
#invalid_utf8: String>>size: the receiver is not valid UTF-8
  hint: if this data is bytes, wrap it with `asBinary` where it enters (e.g. after a socket recv)
```

Erlang's `string` module already raises `badarg` on invalid input (`string:length(<<255,254>>)`, `string:reverse/1`). The boundary maps that to `#invalid_utf8`, so this costs nothing extra on the valid path. Byte-agnostic operations (`=`, `hash`, `,`/concatenation, `printString`) do **not** raise:

* equality and concatenation compare and join bytes, as Erlang does;
* `printString`/`displayString` of an invalid String render the bytes escaped (`<<255,254>>`), so printing never raises.

### 3. Erlang interop needs no conversion

* **Arguments.** Strings are raw binaries, so they reach Erlang untouched at any depth. `beamtalk_erlang_proxy` stops unwrapping Strings. In stock mode it unwraps **top-level `Binary`** arguments to their raw bytes instead, the mirror image of today's rule. The `badarg` retry stays as it is, minus the String unwrap: it still deep-unwraps nested Binaries, coerces charlists and converts actor proxies to pids.
* **Results.** Every binary Erlang returns is a `String`, so no coercion is needed. The `printable_unicode_list` charlist guess (`coerce_charlist_result/1`) is unchanged by this ADR.
* **Nested Binaries** passed to Erlang stay tagged, the mirror of today's nested-String hazard. This case is rare, because bytes travel through the few byte-level APIs, which take them as top-level arguments.

```beamtalk
Erlang lists member: "a" with: #("a")              // => true
Erlang maps get: "k" with: decodedJsonMap          // => the value
Erlang ets insert: t with: {"k", 1}                // Erlang code can look it up by <<"k">>
Erlang erlang term_to_binary: "x"                  // the same bytes Erlang produces for <<"x">>
```

### 4. Byte-level code marks its entry points: the expert API

Code that handles bytes wraps them once, where they enter:

```beamtalk
buffer := (Erlang gen_tcp recv: socket with: 0) map: [:raw | raw asBinary]
digest := (Erlang crypto hash: #sha256 with: payload) asBinary
```

Stdlib natives that return bytes return `Binary` directly, so their callers write nothing:

* `File readBytes:`
* `Binary serialize:`
* digest and crypto wrappers
* socket wrappers

Natives that return text stop tagging: the 133 tagging sites in `beamtalk-http`/`-yaml` and the stdlib's `string_from_binary` calls become the identity.

A byte entry point that is missed makes its data a `String`. For non-text payloads (images, compressed data, most binary protocols) the first grapheme operation raises `#invalid_utf8`, whose hint names the fix. **The one silent case** is byte data that happens to be valid multi-byte UTF-8 and is then measured or indexed through String methods (`size` counts graphemes, not bytes). Two mitigations:

* ASCII protocol data (HTTP heads, most text protocols) gives identical answers either way.
* The migration audits every `size`/`at:`/`part:size:` use on socket and file buffers in buccaneer and catalina (Migration Path).

### 5. Binary patterns

A binary pattern matches a `String` (raw) directly and a `Binary` through its tag. `/binary` segments bind **Strings** (raw sub-binaries), since text is the default. Byte-level code that needs a `Binary` wraps the segment with `asBinary`. Matching stays representation-agnostic, as BT-4173/BT-4223 arranged in the other direction.

### 6. What doesn't change

* BT-4170's rule: class by construction, never by content.
* `Binary` keeps its byte semantics: `size` counts bytes, `at:` is 1-based bytes, `do:` yields Integers, `byteAt:` is 0-based.
* `String` keeps grapheme semantics.
* `asString` keeps its BT-4169 `Result` contract.

## Prior Art

| Language | Default for a binary/bytes value | What we take |
|---|---|---|
| **Elixir** | A string *is* a binary, and bytes and text share the type; `String.valid?/1` checks UTF-8, and `byte_size` vs `String.length` keep the operations apart | Text-default representation and zero-cost Erlang interop. We keep a distinct `Binary` class, which Elixir lacks |
| **Erlang** | Binaries are text by convention in most APIs (`binary:split`, `json`, `string`); bytes APIs are named (`crypto`, `gen_tcp` in binary mode) | Matching the convention is what makes interop free |
| **Python 3** | `str` (text) and `bytes` are distinct types, and literals are text. Bytes come from explicit byte APIs (`open(..., 'rb')`, `socket.recv`) | Text as the default and bytes as an explicit expert type, entered at I/O boundaries; this is closest to the decision |
| **Rust** | `String` vs `Vec<u8>`/`&[u8]`; `String::from_utf8` validates; I/O (`Read::read`) yields bytes | The distinct byte type and the validating conversion (`asString` → `Result`) |
| **Pharo** | `String`/`ByteString` vs `ByteArray`; socket APIs offer both `receiveData` (String) and byte variants | The Smalltalk precedent: byte arrays are a separate class used by byte-level code |
| **Gleam** | `String` and `BitArray` are both raw binaries; only the static type separates them | Not adoptable: Gleam has no dynamic dispatch. Beamtalk dispatches on `class_of`, so one of the two needs a runtime marker, and this ADR puts it on the rarer one |

## User Impact

* **Newcomer:** text from any Erlang library is a `String` with no conversion. `lists:member`, `maps:get` and JSON fields behave as in Elixir. Bytes only appear when they ask for them.
* **Smalltalk developer:** `String` and `Binary` stay distinct classes with distinct semantics, like Pharo's `String`/`ByteArray`. Class still follows construction.
* **Erlang/BEAM developer:** a Beamtalk `String` *is* an Erlang binary. Their libraries, ETS tables, messages and `term_to_binary` see the same bytes they would from Erlang. Only `Binary` carries a tag, and a top-level Binary is unwrapped at the call boundary.
* **Byte-level library author (buccaneer, catalina):** bytes must be marked once where they enter. A missed entry point is loud for non-UTF-8 data and silent only for valid multi-byte UTF-8 measured as text, which the migration audit targets.
* **Production operator:** no per-call conversion for text, so FFI calls get cheaper. Invalid UTF-8 in text is a structured, named error, not a crash in Erlang's `string` module.
* **Tooling developer:** `:: String`/`:: Binary` type tests and the checker's FFI result types change: `binary()` results become `String`, which matches the spec reader's existing `String | Binary` mapping better than today's runtime does.

## Steelman Analysis

| Alternative | Best case for it | Why not |
|---|---|---|
| **Keep BT-4170's default (status quo) plus a hint-level lint** | 🏭 Operator: "The epic just landed in 8 repos; don't churn it again. A lint flags the nested-String hazard." | The ~210 conversion sites stay, buccaneer's sniff stays, and the Erlang-built-data mismatch (BT-4243) stays unfixable. Every new Erlang library adds conversion sites |
| **Spec-directed conversion at the FFI boundary** (the first draft of this ADR) | ⚙️ BEAM veteran: "`-spec` is the contract; convert exactly where the callee wants text." | Rejected after review. Passing Strings tagged into `term()` positions breaks Erlang-built data (decoded JSON maps, cowboy headers), which today's shallow unwrap gets right. The signature registry is missing in the REPL compiler, dependency builds and single-file builds, so behaviour would differ by surface. Stubs would become behaviour, against ADR 0075. Removing the `badarg` retry would break tested calls (`erlang:monitor` with an actor proxy, `io_lib:format`, `os:cmd`) |
| **One class, no tag (the Elixir model)** | 🎨 Language designer: "Zero tags, zero conversions; name the byte operations differently." | It gives up the runtime distinction between bytes and text that BT-4170 was for. `:: String` vs `:: Binary` patterns and `do:` element types would become meaningless |
| **Keep tagged Strings, and make Erlang-returned binaries Strings at every native boundary** | 🎩 Smalltalk purist: "Keep the representation; just tag more eagerly." | Inline FFI results are untyped binaries, so tagging them all is the flip under another name, with the tag on the common side and the mismatch kept |

**Tension:** byte-level authors (buccaneer) benefit most from the status quo, and everyone else benefits from the flip. The audit's 20:1 ratio, plus the fact that buccaneer itself re-sniffs text, decides it. The expert path stays fully available, just explicit.

## Alternatives Considered

See the steelman table. One rejected alternative needs more detail:

### Spec-directed conversion (the first draft of ADR 0140)

The first draft kept tagged Strings and converted each FFI argument according to the callee's Erlang spec: `binary()` → unwrap, `term()` → pass tagged. A three-pass review rejected it:

1. Its premise ("make both sides tagged") fails for Erlang-built data, which holds raw binaries. Today `Erlang maps get: "k" with: decodedJsonMap` works; under the draft it would miss silently.
2. `NativeTypeRegistry` isn't passed to codegen in the REPL compile port, for dependencies (`deps/path.rs` passes `None`) or for single-file builds. The same call would behave differently by surface, against constraint 5.
3. Removing the `badarg` retry for typed calls would break tested behaviour: actor-proxy → pid coercion (`testErlangMonitorWithActorProxy`), `io_lib:format` with Strings, and `os:cmd` with `io_lib:chars()`.
4. Stubs would start to change behaviour, which ADR 0075 rules out, and edited stubs don't invalidate callers in incremental builds.

These failures come from keeping two representations of text and trying to reconcile them at the boundary. This ADR removes the second representation instead.

## Consequences

### Positive
* Erlang interop for text needs no conversion in either direction. BT-4243 is fixed for Beamtalk-built and Erlang-built data alike.
* About 210 conversion sites in sibling repos, and the stdlib's text-tagging calls, become deletable. buccaneer's `stringOrOpaque:` sniff goes away.
* Beamtalk Strings in ETS, messages and `term_to_binary` output are what Erlang code expects.
* BT-4170's construction rule is preserved, and `Binary` keeps its exact byte semantics.

### Negative
* **It reverses BT-4173's representation.** All 697 references to the `beamtalk_string` tag (59 runtime modules, 9 codegen files) change, and so do:
  * the stdlib natives;
  * the `:: String`/`:: Binary` type tests and binary patterns (BT-4222, BT-4223, BT-4228);
  * the sibling-repo migrations (BT-4172), mostly by deleting conversions and adding `asBinary` at about 10 byte entry points.
* **One silent case remains:** byte data that is valid multi-byte UTF-8, missed at its entry point and measured as text. It is mitigated by the migration audit of buffer code, not eliminated.
* String methods raise `#invalid_utf8` on invalid data where today a `Binary` would answer. Code that relied on Erlang-returned bytes being `Binary` without wrapping them changes behaviour; this is loud.
* Nested `Binary` values passed to Erlang stay tagged, the mirrored hazard, now on the rare type.
* Like ADR 0130, a runtime representation change cannot be hot-upgraded: restart and recompile.

### Neutral
* `Binary` and `String` stay distinct classes with the same hierarchy (`String` subclasses `Binary`).
* `asString`'s `Result` contract and `asStringUnchecked` are unchanged; they now unwrap a tag instead of adding one.

## Implementation

0. **Spike to measure the breakage (S).** On a branch, flip `class_of`/`module_for_value`, string-literal codegen (`leaf::tagged_string_lit` → raw `binary_lit`) and the `Binary` constructors. Run `just test-stdlib`, BUnit and the sibling repos' suites, and count failures by category, before planning the rest.
1. **Runtime representation (L).**
   * `beamtalk_primitive` (`class_of`, dispatch, `printString`/`displayString` escaping for invalid UTF-8);
   * the `{beamtalk_binary, _}` constructor and accessors;
   * the proxy's top-level unwrap moved from String to Binary;
   * `#invalid_utf8` mapping in `beamtalk_string`.
2. **Stdlib (M).** Text natives stop tagging. Byte natives (`File` byte reads, `Binary serialize:`, digest, sockets) return `Binary`. Rewrite `binary.bt`/`string.bt` internals for the flipped tag.
3. **Codegen (M).** String literals become raw binaries. Update the type tests (`:: String` = `is_binary`; `:: Binary` = tag or raw), the binary-pattern scrutinee (unwrap the Binary tag; bind segments as Strings) and the conformance fixtures.
4. **Type checker (S).** FFI `binary()` results are typed `String`. Update `isKindOf:` and narrowing tables.
5. **Docs (S).** Update the language features' Binary/String section, the native-Erlang guide ("Strings at the FFI boundary" becomes "Binaries at the FFI boundary") and the ADR 0139 Binaries row.
6. **Sibling repos (M).** Per repo: delete the text conversions, add `asBinary` at byte entry points, and audit buffer arithmetic in buccaneer and catalina (Migration Path).

## Migration Path

* **Text code:** `asStringUnchecked`/`stringOrOpaque:` calls on Erlang results become no-ops. They are removed in the migration, and the build stays green before removal because both methods accept a String.
* **Byte code:** every place bytes enter from Erlang gets `asBinary`:
  * socket `recv` (gen_tcp/ssl);
  * `decode_packet` rest buffers;
  * `file:read_file`;
  * crypto, `term_to_binary`.

  Grep list for the audit: `gen_tcp`, `ssl`, `decode_packet`, `read_file`, `crypto`, `term_to_binary`, `binary:copy`, then every `size`, `at:`, `part:size:` and `byteAt:` on the resulting values.
* **Persisted terms:** no sibling repo writes Beamtalk terms to disk; the audit found only in-memory `persistent_term` use. For safety, `class_of` keeps reading a legacy `{beamtalk_string, Bin}` as a `String` for one release, and the next release removes it.
* **Deployment:** restart and recompile, as for ADR 0130.

## References
- Related issues: BT-4243 (driving bug), BT-4170 / BT-4173 / BT-4168 / BT-4169 / BT-4171 / BT-4172 (the epic this amends), BT-4222 / BT-4228 (type tests), BT-4223 (binary patterns, FFI hazard documentation), BT-4139 (Bitstring)
- Related ADRs: ADR 0075 (FFI types), ADR 0076 / 0101 (FFI boundary), ADR 0129 (no image mechanics), ADR 0130 (restart-required representation change), ADR 0139 (BEAM abstraction boundaries, Binaries row)
- Audit data: sibling-repo site counts, 2026-10-28 (Context)
