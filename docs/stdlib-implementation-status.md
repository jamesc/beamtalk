# Stdlib Implementation Status

> **Last updated:** 2026-08-02
> **Issue:** BT-247, BT-1808, BT-2869, BT-2976
> **Methodology:** Audit of `stdlib/src/*.bt` files, compiler intrinsics (`intrinsics.rs`, `primitive_bindings.rs`),
> runtime dispatch modules (`beamtalk_*.erl`), stdlib test coverage (`stdlib/bootstrap-test/*.btscript`), and REPL protocol test coverage (`tests/repl-protocol/cases/*.btscript`).
>
> **Per-class method counts** below are derived directly from `stdlib/src/*.bt` by counting top-level method
> signatures (2-space-indented lines containing `=>`, excluding `///` doc comments). Re-run per class with:
> `grep -cP '^  (?!//)\S.*=>' stdlib/src/ClassName.bt` (counts both class- and instance-side methods). This
> undercounts nothing but can double-count a rare multi-line keyword signature that itself contains a nested
> `=>` before the real one — spot-check unusually large deltas. The **Mechanism** column is derived the same
> way: a body of exactly `@primitive ...` is `@primitive selector`, a body containing `@intrinsic` is
> `intrinsic`, a body of exactly `self delegate` is `native delegate` (dispatches to the class's `native:`
> Erlang module), anything else is `pure BT`. Because this pass prioritized selector/mechanism/count accuracy
> over exhaustively re-verifying test coverage, tables regenerated in this pass (BT-2976) omit a per-row **E2E**
> column rather than guess; see `stdlib/bootstrap-test/`, `stdlib/test/`, and `tests/repl-protocol/cases/` for
> current coverage, and BT-408 for the outstanding test-coverage-gap tracking issue.

## Executive Summary

| Metric | Value |
|--------|-------|
| **Stdlib .bt files** | 109 |
| **Runtime-only classes** | 0 (all classes have stdlib/src/*.bt) |
| **Missing .bt files** | 0 |
| **Protocols** | 2 (Printable, JsonRepresentable) |

## Status Categories

| Symbol | Meaning |
|--------|---------|
| ✅ Implemented | Fully working — compiler intrinsic, runtime dispatch, or pure Beamtalk |
| ❌ Not Implemented | Defined in stdlib but no backing implementation |
| 🧪 Tested | Has stdlib or E2E test coverage |

## Implementation Mechanisms

| Mechanism | Description | Example |
|-----------|-------------|---------|
| **Compiler intrinsic** | Inlined at call site by codegen (`intrinsics.rs`) | `Block >> value`, `Object >> class` |
| **@primitive selector** | Runtime dispatch via `beamtalk_*.erl` module | `Integer >> +`, `String >> length` |
| **Pure Beamtalk** | Compiled from `.bt` source (ADR 0007) | `True >> not`, `Integer >> isEven` |

---

## Tier 1: Core Classes

### ProtoObject (`stdlib/src/proto_object.bt`)

**Class:** `ProtoObject` — superclass: `nil` (root class)
**Methods:** 9/9 implemented (100%)

| Selector | Mechanism | Status | Notes |
|----------|-----------|--------|-------|
| `==` | intrinsic | ✅ | Value equality (Erlang `==`, non-strict) |
| `/=` | intrinsic | ✅ | Value inequality (negation of `==`) |
| `=:=` | intrinsic | ✅ | Strict value equality (Erlang `=:=`) |
| `=/=` | intrinsic | ✅ | Strict value inequality (negation of `=:=`) |
| `class` | intrinsic | ✅ | Type introspection |
| `doesNotUnderstand:args:` | intrinsic | ✅ | Fallback for unknown messages |
| `perform:withArguments:` | intrinsic | ✅ | Dynamic dispatch |
| `performLocally:withArguments:` | intrinsic | ✅ | Execute a class method in the caller's process, bypassing gen_server dispatch |
| `perform:withArguments:timeout:` | intrinsic | ✅ | Dynamic dispatch with explicit timeout |

### Object (`stdlib/src/object.bt`)

**Class:** `Object` — superclass: `ProtoObject`
**Methods:** 27/27 implemented (100%)

| Selector | Mechanism | Status | Notes |
|----------|-----------|--------|-------|
| `class delegate` (class-side) | pure BT | ✅ | Class-side `delegate` sentinel (ADR 0101, BT-2720) |
| `class` | intrinsic | ✅ | Return the class of the receiver |
| `isNil` | pure BT | ✅ | Returns `false` for all objects except nil |
| `notNil` | pure BT | ✅ | Returns `true` for all objects except nil |
| `ifNil:` | pure BT | ✅ | Returns self (not nil) |
| `ifNotNil:` | pure BT | ✅ | Evaluates `notNilBlock` with self |
| `ifNil:ifNotNil:` | pure BT | ✅ | Evaluates `notNilBlock` with self |
| `ifNotNil:ifNil:` | pure BT | ✅ | Evaluates `notNilBlock` with self |
| `printString` | pure BT | ✅ | `'a ' ++ self class printString` (BT-477) |
| `displayString` | pure BT | ✅ | Delegates to `printString`; override for user-facing display |
| `inspect` | pure BT | ✅ | Opens a navigable `Inspector` cursor on the receiver |
| `yourself` | pure BT | ✅ | Returns self |
| `hash` | intrinsic | ✅ | `erlang:phash2/1` |
| `equals:` | pure BT | ✅ | Overridable value-equality counterpart to `=:=` |
| `respondsTo:` | intrinsic | ✅ | `beamtalk_primitive:responds_to/2` |
| `fieldNames` | intrinsic | ✅ | Async for actors |
| `fieldAt:` | intrinsic | ✅ | Async for actors |
| `fieldAt:put:` | intrinsic | ✅ | Async for actors (returns new state) |
| `perform:` | intrinsic | ✅ | Dynamic dispatch (unary) |
| `perform:withArguments:` | intrinsic | ✅ | Dynamic dispatch with arguments |
| `subclassResponsibility` | pure BT | ✅ | Calls `self error:` — pure Beamtalk method (BT-405) |
| `notImplemented` | pure BT | ✅ | Calls `self error:` — WIP stub marker |
| `show:` | pure BT | ✅ | Nil-safe Transcript output (no newline); returns `self` |
| `showCr:` | pure BT | ✅ | Nil-safe Transcript output (with newline); returns `self` |
| `isKindOf:` | pure BT | ✅ | `self class includesBehaviour: aClass` |
| `error:` | intrinsic | ✅ | Smalltalk-style error signaling |
| `delegate` (instance-side) | pure BT | ✅ | Delegate message dispatch to the backing Erlang module (ADR 0101, BT-2720) |

`sealed` remains a method **modifier** (e.g. `sealed getValue => ...`), not a selector — it is not counted above.

_Note:_ `new` and `new:` have moved to `Value` (see below). Object subclasses without data (FFI namespaces, abstract extension points) cannot be directly instantiated.

### Value (`stdlib/src/value.bt`)

**Class:** `Value` — superclass: `Object`
**Methods:** 3/3 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `new` | intrinsic `basicNew` | ✅ | 🧪 | Inline codegen for value type instantiation |
| `new:` | intrinsic `basicNewWith` | ✅ | 🧪 | Instantiation with constructor args |
| `inspect` | pure BT | ✅ | | `ClassName(field: value, ...)` format |

### Collection (`stdlib/src/collection.bt`)

**Class:** `Collection` — superclass: `Value` — `abstract typed`
**Methods:** 33/33 implemented (100%)
**Note:** Abstract superclass for List, Set, Array, Binary, etc. Provides default iteration built on `size` and `do:`.

| Selector | Mechanism | Status | Notes |
|----------|-----------|--------|-------|
| `class withAll:` | pure BT | ✅ | Factory — create a collection of this type from a list |
| `size` | pure BT (abstract) | ✅ | Subclass must implement |
| `do:` | pure BT (abstract) | ✅ | Subclass must implement |
| `printString` | pure BT | ✅ | Developer-readable string representation |
| `species` | pure BT | ✅ | Class used to build results from collection operations |
| `isEmpty` | pure BT | ✅ | `self size =:= 0` |
| `isNotEmpty` | pure BT | ✅ | `self isEmpty not` |
| `includes:` | pure BT | ✅ | Linear search via `do:` |
| `inject:into:` | @primitive selector | ✅ | Fold with accumulator |
| `collect:` | pure BT | ✅ | Map via `do:` |
| `parallelCollect:` | pure BT | ✅ | Like `collect:`, but evaluates `block` for every element concurrently via `Parallel all:` (BT-2974) |
| `parallelCollect:maxConcurrency:` | pure BT | ✅ | Bounded-concurrency `parallelCollect:` — runs in chunks of at most `maxConcurrency` (BT-3006 follow-up) |
| `runChunked:maxConcurrency:` | pure BT | ✅ | Internal helper backing `parallelCollect:maxConcurrency:` |
| `runChunked:maxConcurrency:acc:` | pure BT | ✅ | `internal` — tail-recursive accumulator helper for the above |
| `select:` | pure BT | ✅ | Filter via `do:` |
| `reject:` | pure BT | ✅ | Negated filter |
| `detect:` | pure BT | ✅ | First match |
| `detect:ifNone:` | pure BT | ✅ | First match with default |
| `anySatisfy:` | pure BT | ✅ | Test if any element satisfies `block` |
| `allSatisfy:` | pure BT | ✅ | Test if all elements satisfy `block` |
| `noneSatisfy:` | pure BT | ✅ | Test if no element satisfies `block` |
| `count:` | pure BT | ✅ | Count elements for which `block` returns true |
| `sum` | @primitive selector | ✅ | Sum all elements; `0` for an empty collection |
| `max` | @primitive selector | ✅ | Largest element |
| `min` | @primitive selector | ✅ | Smallest element |
| `average` | @primitive selector | ✅ | Mean of the elements as a Float |
| `eachWithIndex:` | pure BT | ✅ | Iterate with 1-based index |
| `do:separatedBy:` | pure BT | ✅ | Iterate, evaluating `separatorBlock` between elements |
| `asList` | pure BT | ✅ | Convert to a `List`, in iteration order |
| `asArray` | pure BT | ✅ | Convert to an `Array` |
| `asSet` | pure BT | ✅ | Convert to a `Set`, discarding duplicates |
| `asBag` | pure BT | ✅ | Convert to a `Bag`, counting occurrences |
| `asString` | pure BT | ✅ | String representation |

### Binary (`stdlib/src/binary.bt`)

**Class:** `Binary` — superclass: `Collection` — `@sealed typed` ([ADR 0086](ADR/0086-string-subclass-of-binary.md))
**Methods:** 22/22 implemented (100%)
**Note:** Byte-level data and serialization. Parent of String. Maps to Erlang `binary()`.

| Selector | Mechanism | Status | Notes |
|----------|-----------|--------|-------|
| `class serialize:` | pure BT (Erlang FFI) | ✅ | External term format, via `(Erlang beamtalk_binary) serialize:` |
| `class deserialize:` | pure BT (Erlang FFI) | ✅ | Reverse of serialize |
| `class fromIolist:` | pure BT (Erlang FFI) | ✅ | Build from iolist |
| `class fromBytes:` | pure BT (Erlang FFI) | ✅ | Build from byte list |
| `class deserializeWithUsed:` | pure BT (Erlang FFI) | ✅ | Returns `#(value, bytesConsumed)` |
| `class fromBase64:` | pure BT (Erlang FFI) | ✅ | Decode a standard (RFC 4648 §4) base64 string |
| `class fromBase64Url:` | pure BT (Erlang FFI) | ✅ | Decode a URL-safe (RFC 4648 §5) base64 string |
| `class fromHex:` | pure BT (Erlang FFI) | ✅ | Decode a hexadecimal string |
| `size` | @primitive selector | ✅ | Byte count |
| `do:` | @primitive selector | ✅ | Iterate bytes |
| `printString` | @primitive selector | ✅ | Developer representation |
| `at:` | @primitive selector | ✅ | 1-based byte access |
| `byteAt:` | @primitive selector | ✅ | 0-based byte access |
| `byteSize` | @primitive selector | ✅ | Same as `size` |
| `part:size:` | @primitive selector | ✅ | Zero-copy byte-level slice |
| `concat:` | @primitive selector | ✅ | Byte concatenation |
| `toBytes` | @primitive selector | ✅ | Byte list |
| `asString` | @primitive selector | ✅ | Validate UTF-8 and return as String |
| `asStringUnchecked` | @primitive selector | ✅ | Return as String without UTF-8 validation |
| `asBase64` | pure BT (Erlang FFI) | ✅ | Standard (RFC 4648 §4) base64 encoding |
| `asBase64Url` | pure BT (Erlang FFI) | ✅ | URL-safe (RFC 4648 §5) base64 encoding |
| `asHex` | pure BT (Erlang FFI) | ✅ | Lowercase hexadecimal encoding |

### Number (`stdlib/src/number.bt`)

**Class:** `Number` — superclass: `Value` — `abstract typed`
**Methods:** 18/18 implemented (100%)

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `+` | pure BT (abstract — `Integer`/`Float` override as `@primitive`) | ✅ | `Number>>+` |
| `-` | pure BT (abstract — `Integer`/`Float` override as `@primitive`) | ✅ | `Number>>-` |
| `*` | pure BT (abstract — `Integer`/`Float` override as `@primitive`) | ✅ | `Number>>*` |
| `/` | pure BT (abstract — `Integer`/`Float` override as `@primitive`) | ✅ | `Number>>/` |
| `<` | pure BT (abstract — `Integer`/`Float` override as `@primitive`) | ✅ | `Magnitude>><` |
| `>` | pure BT (abstract — `Integer`/`Float` override as `@primitive`) | ✅ | `Magnitude>>>` |
| `<=` | pure BT (abstract — `Integer`/`Float` override as `@primitive`) | ✅ | `Magnitude>><=` |
| `>=` | pure BT (abstract — `Integer`/`Float` override as `@primitive`) | ✅ | `Magnitude>>>=` |
| `isZero` | pure BT | ✅ | `Number>>isZero` |
| `isPositive` | pure BT | ✅ | `Number>>positive` |
| `isNegative` | pure BT | ✅ | `Number>>negative` |
| `sign` | pure BT | ✅ | `Number>>sign` |
| `between:and:` | pure BT | ✅ | `Magnitude>>between:and:` |
| `reciprocal` | pure BT | ✅ | `Number>>reciprocal` |
| `degreesToRadians` | pure BT | ✅ | `Number>>degreesToRadians` |
| `radiansToDegrees` | pure BT | ✅ | `Number>>radiansToDegrees` |
| `isInteger` | pure BT | ✅ | `Magnitude>>isInteger` (identity/type check) |
| `isFloat` | pure BT | ✅ | `Magnitude>>isFloat` (identity/type check) |

### Integer (`stdlib/src/integer.bt`)

**Class:** `Integer` — superclass: `Number` — `@sealed`
**Methods:** 55/55 implemented (100%)
**Note:** `**` is Integer-only (see [Pharo Comparison](#pharo-comparison-notable-gaps) below) — `Float` has the
equivalent `raisedTo:` but no separate `**` operator.

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `+` | @primitive selector | ✅ | `Integer>>+` |
| `-` | @primitive selector | ✅ | `Integer>>-` |
| `*` | @primitive selector | ✅ | `Integer>>*` |
| `/` | @primitive selector | ✅ | `Integer>>/` |
| `div:` | @primitive selector | ✅ | `Integer>>//` |
| `%` | @primitive selector | ✅ | `Integer>>\\` |
| `**` | @primitive selector | ✅ | `Integer>>raisedTo:` |
| `=:=` | @primitive selector | ✅ | `Integer>>=` |
| `=/=` | @primitive selector | ✅ | `Integer>>~=` |
| `/=` | @primitive selector | ✅ | `Integer>>~=` |
| `<` | @primitive selector | ✅ | `Integer>><` |
| `>` | @primitive selector | ✅ | `Integer>>>` |
| `<=` | @primitive selector | ✅ | `Integer>><=` |
| `>=` | @primitive selector | ✅ | `Integer>>>=` |
| `negated` | pure BT | ✅ | `Integer>>negated` |
| `abs` | pure BT | ✅ | `Integer>>abs` |
| `rounded` | pure BT | ✅ | `Integer>>rounded` (identity) |
| `ceiling` | pure BT | ✅ | `Integer>>ceiling` (identity) |
| `floor` | pure BT | ✅ | `Integer>>floor` (identity) |
| `truncated` | pure BT | ✅ | `Integer>>truncated` (identity) |
| `squared` | pure BT | ✅ | `Number>>squared` |
| `roundTo:` | pure BT | ✅ | `Number>>roundTo:` |
| `truncateTo:` | pure BT | ✅ | `Number>>truncateTo:` |
| `isEven` | pure BT | ✅ | `Integer>>even` |
| `isOdd` | pure BT | ✅ | `Integer>>odd` |
| `min:` | pure BT | ✅ | `Magnitude>>min:` |
| `max:` | pure BT | ✅ | `Magnitude>>max:` |
| `timesRepeat:` | intrinsic | ✅ | `Integer>>timesRepeat:` |
| `to:do:` | intrinsic | ✅ | `Integer>>to:do:` |
| `to:by:do:` | intrinsic | ✅ | `Integer>>to:by:do:` |
| `to:` | pure BT | ✅ | `Number>>to:` (returns an `Interval`) |
| `to:by:` | pure BT | ✅ | `Number>>to:by:` (returns an `Interval`) |
| `asFloat` | @primitive selector | ✅ | `Integer>>asFloat` |
| `asString` | @primitive selector | ✅ | `Integer>>asString` |
| `printString` | @primitive selector | ✅ | `Integer>>printString` |
| `bitAnd:` | @primitive selector | ✅ | `Integer>>bitAnd:` |
| `bitOr:` | @primitive selector | ✅ | `Integer>>bitOr:` |
| `bitXor:` | @primitive selector | ✅ | `Integer>>bitXor:` |
| `bitShift:` | @primitive selector | ✅ | `Integer>>bitShift:` |
| `bitNot` | @primitive selector | ✅ | `Integer>>bitNot` |
| `factorial` | pure BT | ✅ | `Integer>>factorial` |
| `gcd:` | pure BT | ✅ | `Integer>>gcd:` |
| `lcm:` | pure BT | ✅ | `Integer>>lcm:` |
| `isLetter` | @primitive selector | ✅ | Character classification (BT-461) |
| `isDigit` | @primitive selector | ✅ | Character classification (BT-461) |
| `isUppercase` | @primitive selector | ✅ | Character classification (BT-461) |
| `isLowercase` | @primitive selector | ✅ | Character classification (BT-461) |
| `isWhitespace` | @primitive selector | ✅ | Character classification (BT-461) |
| `sqrt` | @primitive selector | ✅ | `Number>>sqrt` |
| `log` | @primitive selector | ✅ | `Number>>ln` (natural log) |
| `ln` | @primitive selector | ✅ | `Number>>ln` — alias for `log` |
| `log2` | @primitive selector | ✅ | `Number>>log:` (base 2) |
| `log10` | @primitive selector | ✅ | `Number>>log` (base 10) |
| `exp` | @primitive selector | ✅ | `Number>>exp` |
| `raisedTo:` | @primitive selector | ✅ | `Number>>raisedTo:` |

### String (`stdlib/src/string.bt`)

**Class:** `String` — superclass: `Binary` — `@sealed` ([ADR 0086](ADR/0086-string-subclass-of-binary.md))
**Methods:** 68/68 implemented (100%)
**Correction (BT-2976):** the real selector for substring search is `includesSubstring:`, and the real selector
for grapheme iteration is `do:` (`each:`/`includes:` were internal `@primitive` dispatch-table tag strings in
the source, not the public Beamtalk selector — the previous revision of this table listed the internal tag as
if it were the callable method name).

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `class withAll:` | @primitive selector | ✅ | `String class>>withAll:` |
| `class fromCodePoint:` | @primitive selector | ✅ | `Character>>asString` (inverse) |
| `class fromCodePoints:` | @primitive selector | ✅ | N/A |
| `class fromIolist:` | @primitive selector | ✅ | N/A (BEAM-specific) |
| `=:=` | @primitive selector | ✅ | `String>>=` |
| `=/=` | @primitive selector | ✅ | `String>>~=` |
| `/=` | @primitive selector | ✅ | `String>>~=` |
| `<` | @primitive selector | ✅ | `String>><` |
| `>` | @primitive selector | ✅ | `String>>>` |
| `<=` | @primitive selector | ✅ | `String>><=` |
| `>=` | @primitive selector | ✅ | `String>>>=` |
| `++` | @primitive selector | ✅ | `String>>,` |
| `,` | @primitive selector | ✅ | `String>>,` |
| `length` | @primitive selector | ✅ | `String>>size` |
| `size` | pure BT | ✅ | `String>>size` |
| `at:` | @primitive selector | ✅ | `String>>at:` |
| `first` | @primitive selector | ✅ | `SequenceableCollection>>first` |
| `last` | @primitive selector | ✅ | `SequenceableCollection>>last` |
| `uppercase` | @primitive selector | ✅ | `String>>asUppercase` |
| `lowercase` | @primitive selector | ✅ | `String>>asLowercase` |
| `capitalize` | @primitive selector | ✅ | `String>>capitalized` |
| `trim` | @primitive selector | ✅ | `String>>trimBoth` |
| `trimLeft` | @primitive selector | ✅ | `String>>trimLeft` |
| `trimRight` | @primitive selector | ✅ | `String>>trimRight` |
| `reverse` | @primitive selector | ✅ | `String>>reversed` |
| `includesSubstring:` | @primitive selector | ✅ | `String>>includesSubstring:` |
| `startsWith:` | @primitive selector | ✅ | `String>>beginsWith:` |
| `endsWith:` | @primitive selector | ✅ | `String>>endsWith:` |
| `indexOf:` | @primitive selector | ✅ | `String>>indexOfSubCollection:` |
| `split:` | @primitive selector | ✅ | N/A |
| `splitOn:` | @primitive selector | ✅ | N/A |
| `repeat:` | @primitive selector | ✅ | N/A |
| `lines` | @primitive selector | ✅ | `String>>lines` |
| `words` | @primitive selector | ✅ | `String>>substrings` |
| `replaceAll:with:` | @primitive selector | ✅ | `String>>replaceAll:with:` |
| `replaceFirst:with:` | @primitive selector | ✅ | `String>>copyReplaceFirst:with:` |
| `take:` | @primitive selector | ✅ | `String>>first:` |
| `drop:` | @primitive selector | ✅ | `String>>allButFirst:` |
| `padLeft:` | @primitive selector | ✅ | `String>>padLeftTo:` |
| `padRight:` | @primitive selector | ✅ | `String>>padRightTo:` |
| `padLeft:with:` | @primitive selector | ✅ | `String>>padLeftTo:with:` |
| `padRight:with:` | @primitive selector | ✅ | `String>>padRightTo:with:` |
| `isEmpty` | pure BT | ✅ | `String>>isEmpty` |
| `isNotEmpty` | pure BT | ✅ | `String>>isNotEmpty` |
| `isBlank` | @primitive selector | ✅ | `String>>isAllSeparators` |
| `isDigit` | @primitive selector | ✅ | `String>>isAllDigits` |
| `isAlpha` | @primitive selector | ✅ | `String>>isAllLetters` |
| `asInteger` | @primitive selector | ✅ | `String>>asInteger` |
| `asFloat` | @primitive selector | ✅ | `String>>asFloat` |
| `asAtom` | @primitive selector | ✅ | N/A (BEAM-specific) |
| `asList` | @primitive selector | ✅ | `String>>asArray` |
| `do:` | @primitive selector | ✅ | `String>>do:` |
| `collect:` | @primitive selector | ✅ | `String>>collect:` |
| `select:` | @primitive selector | ✅ | `String>>select:` |
| `reject:` | @primitive selector | ✅ | `String>>reject:` |
| `stream` | @primitive selector | ✅ | N/A |
| `matchesRegex:` | @primitive selector | ✅ | N/A |
| `matchesRegex:options:` | @primitive selector | ✅ | N/A |
| `firstMatch:` | @primitive selector | ✅ | N/A |
| `allMatches:` | @primitive selector | ✅ | N/A |
| `replaceRegex:with:` | @primitive selector | ✅ | N/A |
| `replaceAllRegex:with:` | @primitive selector | ✅ | N/A |
| `splitRegex:` | @primitive selector | ✅ | N/A |
| `printString` | pure BT | ✅ | `String>>printString` |
| `asString` | pure BT | ✅ | identity |
| `displayString` | pure BT | ✅ | identity |
| `urlEncoded` | pure BT (Erlang FFI) | ✅ | N/A |
| `urlDecoded` | pure BT (Erlang FFI) | ✅ | N/A |

### List (`stdlib/src/list.bt`)

**Class:** `List` — superclass: `Collection` — `@sealed typed`
**Methods:** 44/44 implemented (100%)
**Note:** List in Beamtalk maps to Erlang linked lists. Literal syntax: `#(1, 2, 3)`. Renamed from Array in BT-419 — `Array` is reserved for a future tuple-backed O(1)-indexed collection.
**Migration:** BT-419 — migrated from hand-written `beamtalk_list.erl` (Option B) to compiled `stdlib/src/list.bt` with BIF mappings (Option A). Complex operations delegate to the **`beamtalk_list.erl`** (and, for `inject:into:`, **`beamtalk_collection.erl`**) helper modules — corrected in this pass (BT-2976); the module names were previously misspelled as `beamtalk_list_ops.erl` / `beamtalk_collection_ops.erl`, which do not exist in this repo.
**Corrections (BT-2976):** `add:` now appends to the **end** of the list (O(n)); `addFirst:` is the O(1) prepend. `indexOf:` is `pure BT` (not a primitive delegate). `eachWithIndex:` is a self-hosted compiler intrinsic (BT-2703), not a `beamtalk_list` delegate. `++` lowers straight to the `erlang:++` BIF, not a `beamtalk_list` function.

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `class withAll:` | @primitive selector | ✅ | Identity — a list is already a List |
| `class new:` | pure BT | ✅ | Convenience alias for `withAll:` |
| `size` | @primitive selector | ✅ | `SequenceableCollection>>size` |
| `isEmpty` | @primitive selector | ✅ | `Collection>>isEmpty` |
| `first` | @primitive selector | ✅ | `SequenceableCollection>>first` |
| `rest` | @primitive selector | ✅ | `SequenceableCollection>>allButFirst` |
| `last` | @primitive selector | ✅ | `SequenceableCollection>>last` |
| `at:` | @primitive → `beamtalk_list:at/2` | ✅ | `SequenceableCollection>>at:` |
| `includes:` | @primitive selector | ✅ | `Collection>>includes:` |
| `sort` | @primitive selector | ✅ | `SequenceableCollection>>sort` |
| `sort:` | @primitive → `beamtalk_list:sort_with/2` | ✅ | `SequenceableCollection>>sort:` |
| `reversed` | @primitive selector | ✅ | `SequenceableCollection>>reversed` |
| `unique` | @primitive → `beamtalk_list:unique/1` | ✅ | `Collection>>asSet asArray` |
| `detect:` | @primitive → `beamtalk_list:detect/2` | ✅ | `Collection>>detect:` |
| `detect:ifNone:` | pure BT | ✅ | `Collection>>detect:ifNone:` |
| `do:` | @primitive → `beamtalk_list:do/2` | ✅ | `Collection>>do:` |
| `asList` | pure BT | ✅ | Identity override of `Collection>>asList` |
| `collect:` | @primitive selector | ✅ | `Collection>>collect:` |
| `select:` | @primitive selector | ✅ | `Collection>>select:` |
| `reject:` | @primitive → `beamtalk_list:reject/2` | ✅ | `Collection>>reject:` |
| `inject:into:` | @primitive → `beamtalk_collection:inject_into/3` | ✅ | `Collection>>inject:into:` |
| `take:` | @primitive → `beamtalk_list:take/2` | ✅ | `SequenceableCollection>>first:` |
| `drop:` | @primitive → `beamtalk_list:drop/2` | ✅ | `SequenceableCollection>>allButFirst:` |
| `flatten` | @primitive selector | ✅ | `Collection>>flattened` |
| `flatMap:` | @primitive selector | ✅ | `Collection>>flatCollect:` |
| `count:` | @primitive selector | ✅ | `Collection>>count:` |
| `anySatisfy:` | @primitive selector | ✅ | `Collection>>anySatisfy:` |
| `allSatisfy:` | @primitive selector | ✅ | `Collection>>allSatisfy:` |
| `++` | @primitive BIF (`erlang:++`) | ✅ | `SequenceableCollection>>,` |
| `printString` | @primitive selector | ✅ | `List>>printString` |
| `from:to:` | @primitive → `beamtalk_list:from_to/3` | ✅ | `SequenceableCollection>>copyFrom:to:` |
| `indexOf:` | pure BT | ✅ | `SequenceableCollection>>indexOf:` |
| `zip:` | @primitive → `beamtalk_list:zip/2` | ✅ | `SequenceableCollection>>with:collect:` |
| `groupBy:` | @primitive → `beamtalk_list:group_by/2` | ✅ | `Collection>>groupedBy:` |
| `partition:` | pure BT | ✅ | `Collection>>partition:` |
| `takeWhile:` | @primitive selector | ✅ | N/A |
| `dropWhile:` | @primitive selector | ✅ | N/A |
| `intersperse:` | @primitive → `beamtalk_list:intersperse/2` | ✅ | N/A |
| `addFirst:` | @primitive selector | ✅ | O(1) prepend (returns a new list) |
| `add:` | @primitive selector | ✅ | O(n) append (returns a new list) — see correction above |
| `stream` | @primitive selector | ✅ | Lazy `Stream` over the list elements |
| `atRandom` | @primitive selector | ✅ | N/A |
| `join` | @primitive selector | ✅ | Join a list of strings with no separator |
| `join:` | @primitive selector | ✅ | Join a list of strings with a separator |

### Block (`stdlib/src/block.bt`)

**Class:** `Block` — superclass: `Object` — `@sealed`
**Methods:** 11/11 implemented (100%)
**Correction (BT-2976):** `describe` no longer exists on `Block` (removed along with most other classes' `describe` methods — see the Object/Error section notes above).

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `value` | intrinsic | ✅ | `BlockClosure>>value` |
| `value:` | intrinsic | ✅ | `BlockClosure>>value:` |
| `value:value:` | intrinsic | ✅ | `BlockClosure>>value:value:` |
| `value:value:value:` | intrinsic | ✅ | `BlockClosure>>value:value:value:` |
| `whileTrue:` | intrinsic | ✅ | `BlockClosure>>whileTrue:` |
| `whileFalse:` | intrinsic | ✅ | `BlockClosure>>whileFalse:` |
| `repeat` | intrinsic | ✅ | `BlockClosure>>repeat` |
| `on:do:` | intrinsic | ✅ | `BlockClosure>>on:do:` |
| `ensure:` | intrinsic | ✅ | `BlockClosure>>ensure:` |
| `arity` | @primitive selector | ✅ | `BlockClosure>>argumentCount` |
| `valueWithArguments:` | intrinsic | ✅ | `BlockClosure>>valueWithArguments:` |

### True (`stdlib/src/true.bt`) & False (`stdlib/src/false.bt`)

**Class:** `True` / `False` — superclass: `Boolean` — `@sealed`
**Methods:** 7/7 implemented each (100%)
**Inherits:** `and:`, `or:`, `xor:`, `isBoolean` from `Boolean` (which also declares `ifTrue:ifFalse:`/`ifTrue:`/`ifFalse:`/`not` as `subclassResponsibility` abstract protocol — see the `Boolean` table below).
**Correction (BT-2976):** `describe` no longer exists on `True`/`False`.

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `ifTrue:ifFalse:` | pure BT | ✅ | `Boolean>>ifTrue:ifFalse:` |
| `ifTrue:` | pure BT | ✅ | `Boolean>>ifTrue:` |
| `ifFalse:` | pure BT | ✅ | `Boolean>>ifFalse:` |
| `not` | pure BT | ✅ | `Boolean>>not` |
| `isTrue` | pure BT | ✅ | N/A |
| `isFalse` | pure BT | ✅ | N/A |
| `printString` | pure BT | ✅ | `Boolean>>printString` |

### UndefinedObject (`stdlib/src/undefined_object.bt`)

**Class:** `UndefinedObject` — superclass: `Object` — `@sealed`
**Methods:** 10/10 implemented (100%)
**Correction (BT-2976):** `describe` no longer exists on `UndefinedObject`.

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `isNil` | pure BT | ✅ | `UndefinedObject>>isNil` |
| `notNil` | pure BT | ✅ | `UndefinedObject>>notNil` |
| `ifNil:` | pure BT | ✅ | `UndefinedObject>>ifNil:` |
| `ifNotNil:` | pure BT | ✅ | `UndefinedObject>>ifNotNil:` |
| `ifNil:ifNotNil:` | pure BT | ✅ | `UndefinedObject>>ifNil:ifNotNil:` |
| `ifNotNil:ifNil:` | pure BT | ✅ | `UndefinedObject>>ifNotNil:ifNil:` |
| `copy` | pure BT | ✅ | `UndefinedObject>>shallowCopy` |
| `deepCopy` | pure BT | ✅ | `UndefinedObject>>deepCopy` |
| `shallowCopy` | pure BT | ✅ | `UndefinedObject>>shallowCopy` |
| `printString` | pure BT | ✅ | `UndefinedObject>>printString` |

### Float (`stdlib/src/float.bt`)

**Class:** `Float` — superclass: `Number` — `@sealed`
**Methods:** 48/48 implemented (100%)
**Correction (BT-2976):** the previous revision of this table (25 methods) predated the trigonometric and
logarithmic methods below, and separately claimed some of them (`sqrt`, `log`/`ln`/`exp`, trig) as "missing" in
the Pharo-comparison section further down — both are stale; all are implemented. `Float` does **not** define a
separate `**` operator (unlike `Integer`) — use `raisedTo:`, which `**` desugars to on `Integer`.

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `class pi` | @primitive selector | ✅ | `Float class>>pi` |
| `class e` | @primitive selector | ✅ | N/A |
| `class infinity` | pure BT | ✅ | `Float class>>infinity` — raises (BEAM has no IEEE 754 infinity) |
| `+` | @primitive selector | ✅ | `Float>>+` |
| `-` | @primitive selector | ✅ | `Float>>-` |
| `*` | @primitive selector | ✅ | `Float>>*` |
| `/` | @primitive selector | ✅ | `Float>>/` |
| `=:=` | @primitive selector | ✅ | `Float>>=` |
| `=/=` | @primitive selector | ✅ | `Float>>~=` |
| `/=` | @primitive selector | ✅ | `Float>>~=` |
| `<` | @primitive selector | ✅ | `Float>><` |
| `>` | @primitive selector | ✅ | `Float>>>` |
| `<=` | @primitive selector | ✅ | `Float>><=` |
| `>=` | @primitive selector | ✅ | `Float>>>=` |
| `negated` | pure BT | ✅ | `Float>>negated` |
| `abs` | pure BT | ✅ | `Float>>abs` |
| `min:` | pure BT | ✅ | `Magnitude>>min:` |
| `max:` | pure BT | ✅ | `Magnitude>>max:` |
| `rounded` | @primitive selector | ✅ | `Float>>rounded` |
| `ceiling` | @primitive selector | ✅ | `Float>>ceiling` |
| `floor` | @primitive selector | ✅ | `Float>>floor` |
| `truncated` | @primitive selector | ✅ | `Float>>truncated` |
| `squared` | pure BT | ✅ | `Number>>squared` |
| `roundTo:` | pure BT | ✅ | `Number>>roundTo:` |
| `truncateTo:` | pure BT | ✅ | `Number>>truncateTo:` |
| `isNaN` | pure BT | ✅ | `Float>>isNaN` (always `false` — BEAM has no NaN) |
| `isInfinite` | pure BT | ✅ | `Float>>isInfinite` (always `false` — BEAM has no Infinity) |
| `isZero` | pure BT | ✅ | `Float>>isZero` |
| `asInteger` | @primitive selector | ✅ | `Float>>asInteger` |
| `asString` | @primitive selector | ✅ | `Float>>asString` |
| `printString` | @primitive selector | ✅ | `Float>>printString` |
| `sin` | @primitive selector | ✅ | `Number>>sin` |
| `cos` | @primitive selector | ✅ | `Number>>cos` |
| `tan` | @primitive selector | ✅ | `Number>>tan` |
| `sinh` | @primitive selector | ✅ | `Number>>sinh` |
| `cosh` | @primitive selector | ✅ | `Number>>cosh` |
| `tanh` | @primitive selector | ✅ | `Number>>tanh` |
| `asin` | @primitive selector | ✅ | `Number>>arcSin` |
| `acos` | @primitive selector | ✅ | `Number>>arcCos` |
| `atan` | @primitive selector | ✅ | `Number>>arcTan` |
| `atan2:` | @primitive selector | ✅ | `Number>>arcTan:` |
| `sqrt` | @primitive selector | ✅ | `Number>>sqrt` |
| `log` | @primitive selector | ✅ | `Number>>ln` (natural log) |
| `ln` | @primitive selector | ✅ | `Number>>ln` — alias for `log` |
| `log2` | @primitive selector | ✅ | `Number>>log:` (base 2) |
| `log10` | @primitive selector | ✅ | `Number>>log` (base 10) |
| `exp` | @primitive selector | ✅ | `Number>>exp` |
| `raisedTo:` | @primitive selector | ✅ | `Number>>raisedTo:` |

---

## Tier 2: Standard Classes

### Actor (`stdlib/src/actor.bt`)

**Class:** `Actor` — superclass: `Object` — `@sealed`
**Methods:** 26/26 implemented (100%)
**Correction (BT-2976):** `describe` no longer exists on `Actor` — the class-side registration (`spawnAs:`,
`named:`, `allRegistered`, …) and lifecycle/monitoring protocol below were added since the last audit
(BT-2966-era work) and were entirely undocumented here.
**Update (BT-3071):** `new`/`new:` (previously codegen-injected error stubs invisible to this source-based
audit) are now real `class sealed new` / `new:` declarations on `actor.bt` — both always raise
`instantiation_error` ("Use spawn instead" / "Use spawnWith: instead"); see `docs/ADR/0013-class-variables-class-methods-instantiation.md`.

| Selector | Mechanism | Status | Notes |
|----------|-----------|--------|-------|
| `class spawn` | intrinsic | ✅ | `gen_server:start_link` with default state |
| `class spawnWith:` | intrinsic | ✅ | With constructor args |
| `class new` | intrinsic | ✅ | Always raises `instantiation_error` — actors use `spawn`, not `new` |
| `class new:` | intrinsic | ✅ | Always raises `instantiation_error` — actors use `spawnWith:`, not `new:` |
| `class spawnAs:` | pure BT | ✅ | Atomically spawn and register under `name` |
| `class spawnWith:as:` | pure BT | ✅ | Atomically spawn with init args and register under `name` |
| `class named:` | pure BT | ✅ | Look up a registered actor by name, checked against the receiver class |
| `class allRegistered` | pure BT | ✅ | Every currently-registered Beamtalk actor, as `Actor` proxies |
| `class supervisionPolicy` | pure BT | ✅ | Default OTP restart policy for this actor class |
| `class isSupervisor` | pure BT | ✅ | Whether this class is a supervisor |
| `class supervisionSpec` | pure BT | ✅ | `SupervisionSpec` for this actor class with default settings |
| `registerAs:` | pure BT | ✅ | Register this (already-spawned) actor under `name` |
| `unregister` | pure BT | ✅ | Unregister this actor's name, if any (idempotent) |
| `unregisterName` | pure BT (Erlang FFI) | ✅ | Internal FFI seam (ADR 0101 Part 4) |
| `registeredName` | pure BT (Erlang FFI) | ✅ | The `Symbol` this actor is registered under, or `nil` |
| `isRegistered` | pure BT (Erlang FFI) | ✅ | Whether this actor currently has a registered name |
| `withTimeout:` | pure BT | ✅ | Wrap this actor with a custom message timeout |
| `initialize` | pure BT | ✅ | Optional lifecycle hook, called automatically after spawn |
| `terminate:` | pure BT | ✅ | Optional lifecycle hook, called when the actor is shutting down |
| `delegate` | intrinsic | ✅ | Delegate message dispatch to the backing Erlang module |
| `pid` | intrinsic | ✅ | Return the raw Erlang PID backing this actor |
| `monitor` | intrinsic | ✅ | Create an Erlang monitor on this actor's process |
| `onExit:` | intrinsic | ✅ | Register a callback invoked when this actor exits |
| `stop` | intrinsic | ✅ | Gracefully stop this actor (`gen_server:stop`) |
| `kill` | intrinsic | ✅ | Forcefully kill this actor (`exit(Pid, kill)`) |
| `isAlive` | intrinsic | ✅ | Check if this actor's process is still alive |

### File (`stdlib/src/file.bt`)

**Class:** `File` — superclass: `Object` (native: `beamtalk_file`)
**Methods:** 22/22 implemented (100%) — all class-level methods
**Correction (BT-2976):** the previous revision of this table (3 methods: `exists:`, `readAll:`,
`writeAll:contents:`) predates the binary I/O, directory, and path operations below — all implemented, all
dispatching via `native delegate` (`self delegate`) to `beamtalk_file.erl`, not `@primitive`.

| Selector | Mechanism | Status | Notes |
|----------|-----------|--------|-------|
| `class exists:` | native delegate | ✅ | Test if a file exists at the given path |
| `class readAll:` | native delegate | ✅ | Read a file as a `Result(String, Error)` |
| `class writeAll:contents:` | native delegate | ✅ | Write text to a file, creating or overwriting it |
| `class readBinary:` | native delegate | ✅ | Read a file as raw `Result(Binary, Error)` |
| `class writeBinary:contents:` | native delegate | ✅ | Write binary data to a file |
| `class appendBinary:contents:` | native delegate | ✅ | Append binary data to a file, creating it if needed |
| `class lines:` | native delegate | ✅ | Lazy `Stream` of lines from a file |
| `class open:do:` | native delegate | ✅ | Block-scoped file handle with automatic cleanup |
| `class open:mode:` | native delegate | ✅ | Open a file in the given mode, returning a `FileHandle` |
| `class open:mode:do:` | native delegate | ✅ | Block-scoped handle in a given mode |
| `class lastModified:` | native delegate | ✅ | Last modification time of a file |
| `class isDirectory:` | native delegate | ✅ | Test if a path is a directory |
| `class isFile:` | native delegate | ✅ | Test if a path is a regular file |
| `class mkdir:` | native delegate | ✅ | Create a directory (errors if the parent is missing) |
| `class mkdirAll:` | native delegate | ✅ | Create a directory and all missing parents |
| `class listDirectory:` | native delegate | ✅ | List directory entries as a `List` of `String` |
| `class delete:` | native delegate | ✅ | Delete a file or empty directory |
| `class deleteAll:` | native delegate | ✅ | Recursively delete a directory tree |
| `class rename:to:` | native delegate | ✅ | Rename or move a file or directory |
| `class absolutePath:` | native delegate | ✅ | Resolve a relative path to its absolute path |
| `class cwd` | native delegate | ✅ | Current working directory |
| `class tempDirectory` | native delegate | ✅ | OS temporary directory path |

### Beamtalk / BeamtalkInterface (`stdlib/src/beamtalk_interface.bt`)

**Class:** `BeamtalkInterface` — superclass: `Actor`
**Methods:** 20/20 implemented (100%)
**Correction (BT-2976):** the previous revision of this table (4 methods) predated the logger/debug-target
control-plane and Erlang-module-help selectors below.

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `class current` | pure BT | ✅ | Current singleton instance (nil before workspace bootstrap) |
| `class current:` | pure BT | ✅ | Set the current singleton instance |
| `allClasses` | pure BT | ✅ | `Smalltalk>>allClasses` |
| `classNamed:` | pure BT | ✅ | `Smalltalk>>at:` |
| `globals` | pure BT | ✅ | `Smalltalk>>globals` |
| `help:` | pure BT | ✅ | Class documentation: name, superclass, method signatures |
| `help:selector:` | pure BT | ✅ | Detailed documentation for a specific method |
| `erlangHelp:` | pure BT | ✅ | Documentation for an Erlang module (type sigs + EEP-48 docs) |
| `erlangHelp:selector:` | pure BT | ✅ | Documentation for a specific Erlang module function |
| `version` | pure BT | ✅ | Beamtalk version string |
| `logLevel` | pure BT | ✅ | Current OTP primary log level |
| `logLevel:` | pure BT | ✅ | Set the OTP primary log level |
| `logFormat` | pure BT | ✅ | Current log format (`#text`/`#json`) |
| `logFormat:` | pure BT | ✅ | Switch the log format on the file handler |
| `debugTargets` | pure BT | ✅ | Available debug target symbols |
| `enableDebug:` | pure BT | ✅ | Enable debug logging for a subsystem/class/actor |
| `disableDebug:` | pure BT | ✅ | Disable debug logging for a subsystem/class/actor |
| `activeDebugTargets` | pure BT | ✅ | Currently enabled debug targets |
| `disableAllDebug` | pure BT | ✅ | Disable all debug targets |
| `loggerInfo` | pure BT | ✅ | Formatted description of the current logger state |

### Dictionary (`stdlib/src/dictionary.bt` — BT-418)

**Class:** `Dictionary(K, V)` — superclass: `Collection` — `@sealed`
**Helper module:** `beamtalk_map.erl` (complex operations) — corrected in this pass (BT-2976); previously
misspelled `beamtalk_map_ops.erl`, which does not exist.
**Methods:** 15/15 implemented (100%)
**Correction (BT-2976):** `describe` no longer exists on `Dictionary`; `includes:` (value membership) and
`collect:` were undocumented.

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `size` | @primitive selector | ✅ | `Dictionary>>size` |
| `keys` | @primitive selector | ✅ | `Dictionary>>keys` |
| `values` | @primitive selector | ✅ | `Dictionary>>values` |
| `at:` | @primitive selector | ✅ | `Dictionary>>at:` |
| `at:ifAbsent:` | @primitive selector | ✅ | `Dictionary>>at:ifAbsent:` |
| `at:put:` | @primitive selector | ✅ | `Dictionary>>at:put:` |
| `includesKey:` | @primitive selector | ✅ | `Dictionary>>includesKey:` |
| `removeKey:` | @primitive selector | ✅ | `Dictionary>>removeKey:` |
| `merge:` | @primitive selector | ✅ | `Dictionary>>merge:` |
| `includes:` | @primitive selector | ✅ | `Dictionary>>includes:` (value membership) |
| `do:` | @primitive selector | ✅ | `Collection>>do:` (iterates values) |
| `collect:` | pure BT | ✅ | Maps `block` over values, returning a new `Dictionary` |
| `doWithKey:` | @primitive selector | ✅ | `Dictionary>>keysAndValuesDo:` |
| `keysAndValuesDo:` | pure BT (delegates to `doWithKey:`) | ✅ | `Dictionary>>keysAndValuesDo:` |
| `printString` | @primitive selector | ✅ | `Dictionary>>printString` |

### Set (`stdlib/src/set.bt` — BT-73)

**Class:** `Set(E)` — superclass: `Collection` — `@sealed`
**Helper module:** `beamtalk_set.erl` (ordsets operations + tagged map wrapping) — corrected in this pass
(BT-2976); previously misspelled `beamtalk_set_ops.erl`, which does not exist.
**Representation:** Tagged map `#{'$beamtalk_class' => 'Set', elements => [sorted_list]}`
**Methods:** 17/17 implemented (100%)
**Correction (BT-2976):** `describe` no longer exists on `Set`. `new` (no-arg) is inherited unchanged from
`Value`, not redefined in `set.bt`, so it is not counted here; `class new:`/`class withAll:` are Set's own
list-based constructors, and `stream` and `asSet` (identity override) were undocumented.

| Selector | Mechanism | Status | Notes | Pharo Equivalent |
|----------|-----------|--------|-------|-------------------|
| `class withAll:` | @primitive selector | ✅ | Create from a list, deduplicating | `Set>>withAll:` |
| `class new:` | pure BT | ✅ | Convenience alias for `withAll:` | `Set>>new:` |
| `size` | @primitive selector | ✅ | `length(Elements)` | `Set>>size` |
| `isEmpty` | @primitive selector | ✅ | `Elements == []` | `Set>>isEmpty` |
| `includes:` | @primitive selector | ✅ | `ordsets:is_element` | `Set>>includes:` |
| `add:` | @primitive selector | ✅ | `ordsets:add_element` | `Set>>add:` |
| `remove:` | @primitive selector | ✅ | `ordsets:del_element` | `Set>>remove:` |
| `union:` | @primitive selector | ✅ | `ordsets:union` | `Set>>union:` |
| `intersection:` | @primitive selector | ✅ | `ordsets:intersection` | `Set>>intersection:` |
| `difference:` | @primitive selector | ✅ | `ordsets:subtract` | `Set>>difference:` |
| `isSubsetOf:` | @primitive selector | ✅ | `ordsets:is_subset` | `Set>>isSubsetOf:` |
| `asList` | @primitive selector | ✅ | Returns sorted elements | `Set>>asArray` |
| `asSet` | pure BT | ✅ | Identity override of `Collection>>asSet` | N/A |
| `fromList:` | @primitive selector | ✅ | `ordsets:from_list` | `Set>>addAll:` |
| `do:` | @primitive selector | ✅ | Iterate elements | `Set>>do:` |
| `printString` | @primitive selector | ✅ | `beamtalk_primitive:print_string/1` | `Set>>printString` (BT-477) |
| `stream` | @primitive selector | ✅ | Lazy `Stream` over set elements | N/A |

### Tuple (`stdlib/src/tuple.bt`)

**Class:** `Tuple` — superclass: `Collection` — `@sealed typed`
**Methods:** 13/13 implemented (100%)
**Note:** BEAM-specific, wraps Erlang result tuples `{ok, Value}` / `{error, Reason}`.
**Correction (BT-2976):** `class withAll:`/`class new:`, `do:`, and `atRandom` were undocumented.

| Selector | Mechanism | Status | Notes |
|----------|-----------|--------|-------|
| `class withAll:` | @primitive selector | ✅ | Create a Tuple from a list of elements |
| `class new:` | pure BT | ✅ | Convenience alias for `withAll:` |
| `size` | @primitive selector | ✅ | `tuple_size` |
| `at:` | @primitive selector | ✅ | 1-based index via `element` |
| `isOk` | @primitive selector | ✅ | `{ok, _}` pattern match |
| `isError` | @primitive selector | ✅ | `{error, _}` pattern match |
| `unwrap` | pure BT | ✅ | Extract value or raise |
| `unwrapOr:` | pure BT | ✅ | Extract or return default |
| `unwrapOrElse:` | pure BT | ✅ | Extract or evaluate block |
| `asString` | @primitive selector | ✅ | String representation |
| `printString` | pure BT | ✅ | Human-readable representation |
| `do:` | @primitive selector | ✅ | Iterate over each element |
| `atRandom` | @primitive selector | ✅ | Return a random element |

### Symbol (`stdlib/src/symbol.bt`)

**Class:** `Symbol` — superclass: `Object` — `@sealed`
**Methods:** 8/8 implemented (100%)
**Correction (BT-2976):** `describe` no longer exists on `Symbol`; `=/=` and `displayString` were undocumented.

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `asString` | @primitive selector | ✅ | `Symbol>>asString` |
| `asAtom` | @primitive selector | ✅ | N/A (BEAM-specific) |
| `printString` | @primitive selector | ✅ | `Symbol>>printString` |
| `=:=` | @primitive selector | ✅ | `Symbol>>=` |
| `=/=` | @primitive selector | ✅ | `Symbol>>~=` |
| `/=` | @primitive selector | ✅ | `Symbol>>~=` |
| `displayString` | pure BT | ✅ | User-facing string without the `#` prefix |
| `hash` | @primitive selector | ✅ | `Symbol>>hash` |

### Exception (`stdlib/src/exception.bt`)

**Class:** `Exception` — superclass: `Object`
**Methods:** 11/11 implemented (100%)
**Correction (BT-2976):** `describe` no longer exists on `Exception`; `class signal`/`class signal:` and
`stackTrace` were undocumented.

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `class signal:` | @primitive selector | ✅ | `Exception class>>signal:` |
| `class signal` | @primitive selector | ✅ | `Exception class>>signal` |
| `message` | @primitive selector | ✅ | `Exception>>messageText` |
| `hint` | @primitive selector | ✅ | N/A |
| `kind` | @primitive selector | ✅ | N/A |
| `selector` | @primitive selector | ✅ | N/A |
| `errorClass` | @primitive selector | ✅ | N/A |
| `printString` | @primitive selector | ✅ | `Exception>>printString` |
| `stackTrace` | @primitive selector | ✅ | Returns `List(StackFrame)` |
| `signal` | @primitive selector | ✅ | `Exception>>signal` |
| `signal:` | @primitive selector | ✅ | `Exception>>signal:` |

### Error (`stdlib/src/error.bt`)

**Class:** `Error` — superclass: `Exception`
**Methods:** 0 — empty subclass, inherits `Exception`'s protocol entirely
**Correction (BT-2976):** `describe` no longer exists — `error.bt` currently declares no methods of its own
(previously documented as `describe`, 1/1). Same for `InstantiationError`, `RuntimeError`, and `TypeError`
below — all are now empty `Error` subclasses that exist purely to be caught by class via `on:do:`.

### TranscriptStream (`stdlib/src/transcript_stream.bt`)

**Class:** `TranscriptStream` — superclass: `Actor` (native: `beamtalk_transcript_stream`)
**Methods:** 9/9 implemented (100%)
**Correction (BT-2976):** `class current`/`class current:`/`class resetCurrent` (singleton accessors) were undocumented.

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `class current` | pure BT | ✅ | N/A |
| `class current:` | pure BT | ✅ | N/A |
| `class resetCurrent` | pure BT | ✅ | N/A |
| `show:` | native delegate | ✅ | `Transcript>>show:` — accepts `Printable` |
| `cr` | native delegate | ✅ | `Transcript>>cr` |
| `subscribe` | native delegate | ✅ | N/A |
| `unsubscribe` | native delegate | ✅ | N/A |
| `recent` | native delegate | ✅ | N/A |
| `clear` | native delegate | ✅ | N/A |

---

### CompiledMethod (`stdlib/src/compiled_method.bt`)

**Class:** `CompiledMethod` — superclass: `Object`
**Methods:** 6/6 implemented (100%)
**Correction (BT-2976):** `doc` (method documentation string) was undocumented.

| Selector | Mechanism | Status | Pharo Equivalent |
|----------|-----------|--------|-------------------|
| `selector` | @primitive selector | ✅ | `CompiledMethod>>selector` |
| `source` | @primitive selector | ✅ | `CompiledMethod>>sourceCode` |
| `doc` | @primitive selector | ✅ | `CompiledMethod>>comment` |
| `argumentCount` | @primitive selector | ✅ | `CompiledMethod>>numArgs` |
| `printString` | @primitive selector | ✅ | `CompiledMethod>>printString` |
| `asString` | @primitive selector | ✅ | `CompiledMethod>>asString` |

### Character (`stdlib/src/character.bt`)

**Class:** `Character` — superclass: `Integer` — `@sealed`
**Methods:** 19/19 implemented (100%)
**Correction (BT-2976):** `describe` no longer exists on `Character`; `class value:` (construct from code point)
was already documented but is now the only class-side method.

| Selector | Mechanism | Status | Notes |
|----------|-----------|--------|-------|
| `class value:` | @primitive selector | ✅ | Construct from code point |
| `=:=` | @primitive selector | ✅ | Character equality |
| `=/=` | @primitive selector | ✅ | Character strict inequality |
| `/=` | @primitive selector | ✅ | Character not-equal |
| `<` | @primitive selector | ✅ | Ordering |
| `>` | @primitive selector | ✅ | Ordering |
| `<=` | @primitive selector | ✅ | Ordering |
| `>=` | @primitive selector | ✅ | Ordering |
| `asInteger` | @primitive selector | ✅ | Unicode code point |
| `asString` | @primitive selector | ✅ | Single-character string |
| `printString` | @primitive selector | ✅ | Display representation |
| `hash` | @primitive selector | ✅ | Hash value |
| `isLetter` | @primitive selector | ✅ | Unicode letter check |
| `isDigit` | @primitive selector | ✅ | Unicode digit check |
| `isUppercase` | @primitive selector | ✅ | Case check |
| `isLowercase` | @primitive selector | ✅ | Case check |
| `isWhitespace` | @primitive selector | ✅ | Whitespace check |
| `uppercase` | @primitive selector | ✅ | Case conversion |
| `lowercase` | @primitive selector | ✅ | Case conversion |

### Boolean (`stdlib/src/boolean.bt`)

**Class:** `Boolean` — superclass: `Value` — `abstract sealed`
**Methods:** 8/8 implemented (100%)
**Correction (BT-2976):** `Boolean` now also declares `ifTrue:ifFalse:`, `ifTrue:`, `ifFalse:`, and `not` as
abstract protocol (`self subclassResponsibility`, BT-2834/BT-2886) — `True`/`False` still provide the concrete
overrides (see their table above).

| Selector | Mechanism | Status | Notes |
|----------|-----------|--------|-------|
| `ifTrue:ifFalse:` | pure BT (abstract) | ✅ | `self subclassResponsibility` |
| `ifTrue:` | pure BT (abstract) | ✅ | `self subclassResponsibility` |
| `ifFalse:` | pure BT (abstract) | ✅ | `self subclassResponsibility` |
| `not` | pure BT (abstract) | ✅ | `self subclassResponsibility` |
| `isBoolean` | pure BT | ✅ | Type check |
| `and:` | pure BT | ✅ | Logical AND (lazy) |
| `or:` | pure BT | ✅ | Logical OR (lazy) |
| `xor:` | pure BT | ✅ | Logical XOR |

### TestCase (`stdlib/src/test_case.bt`)

**Class:** `TestCase` — superclass: `Value`
**Methods:** 17/17 implemented (100%)
**Correction (BT-2976):** suite-level fixtures (`setUpOnce`/`tearDownOnce`/`suiteFixture`), `skip`/`skip:`,
`assertOk:`, `assertError:equals:`, and the class-side test runner (`class runAll`/`class run:`/`class serial`)
were undocumented.

| Selector | Mechanism | Status | Notes |
|----------|-----------|--------|-------|
| `class runAll` | pure BT | ✅ | Run all test methods in this class |
| `class run:` | pure BT | ✅ | Run the named test method |
| `class serial` | pure BT | ✅ | Whether this test class must run serially |
| `setUp` | pure BT | ✅ | Override for test setup |
| `tearDown` | pure BT | ✅ | Override for test cleanup |
| `setUpOnce` | pure BT | ✅ | Suite-level fixture, run once before all tests |
| `tearDownOnce` | pure BT | ✅ | Suite-level cleanup, run once after all tests |
| `suiteFixture` | pure BT | ✅ | Access the fixture set by `setUpOnce` |
| `assert:` | pure BT | ✅ | Assert truthy |
| `deny:` | pure BT | ✅ | Assert falsy |
| `assert:equals:` | pure BT | ✅ | Assert equality |
| `should:raise:` | pure BT | ✅ | Assert exception |
| `fail:` | pure BT | ✅ | Fail with message |
| `skip:` | pure BT | ✅ | Skip the current test with a reason |
| `skip` | pure BT | ✅ | Skip the current test with no reason |
| `assertOk:` | pure BT | ✅ | Assert `result` is a successful `Result`, return its value |
| `assertError:equals:` | pure BT | ✅ | Assert `result` is an error `Result` matching `expected` |

### InstantiationError (`stdlib/src/instantiation_error.bt`)

**Class:** `InstantiationError` — superclass: `Error`
**Methods:** 0 — empty subclass, inherits `Error`/`Exception`'s protocol entirely
**Correction (BT-2976):** `describe` no longer exists (previously documented as 1/1).

### RuntimeError (`stdlib/src/runtime_error.bt`)

**Class:** `RuntimeError` — superclass: `Error`
**Methods:** 0 — empty subclass, inherits `Error`/`Exception`'s protocol entirely
**Correction (BT-2976):** `describe` no longer exists (previously documented as 1/1).

### TypeError (`stdlib/src/type_error.bt`)

**Class:** `TypeError` — superclass: `Error`
**Methods:** 0 — empty subclass, inherits `Error`/`Exception`'s protocol entirely
**Correction (BT-2976):** `describe` no longer exists (previously documented as 1/1).

---

## Protocols

### Printable (`stdlib/src/printable.bt`)

**Protocol:** `Printable` — structural protocol (ADR 0068, BT-1766)
**Required methods:** 2

| Selector | Return Type | Notes |
|----------|-------------|-------|
| `asString` | `String` | Human-readable string representation |
| `printString` | `String` | Developer-oriented representation (debugging, REPL) |

**Conformance:** Automatic — any class implementing both `asString` and `printString` conforms.
Most stdlib classes conform because `Object` provides a default `printString` and subclasses typically override `asString`.

**Usage:** `TranscriptStream >> show:` accepts `Printable`, so conforming objects can be displayed directly without manual `asString` calls.

### JsonRepresentable (`stdlib/src/json_representable.bt`)

**Protocol:** `JsonRepresentable` — structural protocol (ADR 0068, BT-2818)
**Required methods:** 1

| Selector | Return Type | Notes |
|----------|-------------|-------|
| `asJson` | `Object` | Returns a natively JSON-representable value (typically a `Dictionary` with wire-format keys) |

**Conformance:** Automatic — any class implementing `asJson` conforms.

**Usage:** `Json generate:` and `Json prettyPrint:` (see `Json`) dispatch to `asJson` for any value that is not one of the natively JSON-representable types (`Dictionary`, `List`, `String`, `Integer`, `Float`, `Boolean`, `nil`). The returned value is converted recursively, so it may itself contain further `JsonRepresentable` objects.

---

## Additional Stdlib Classes

The following stdlib `.bt` classes exist but have not yet received a full method-level audit; that audit is tracked as future work (see Methodology above).

| Class | Superclass | File | Notes |
|-------|------------|------|-------|
| `ActorSpawned` | `Announcement` | `actor_spawned.bt` | System event: actor started (ADR 0093) |
| `ActorStopped` | `Announcement` | `actor_stopped.bt` | System event: actor terminated (ADR 0093) |
| `Announcement` | `Value` | `announcement.bt` | Base event type for the typed Observer substrate (ADR 0093) |
| `AnnouncementNavigation` | `Object` | `announcement_navigation.bt` | Live subscription-graph introspection queries (ADR 0093 §7) |
| `Announcer` | `Object` | `announcer.bt` | Typed pub/sub dispatcher handle (ADR 0093) |
| `Array` | `Collection` | `array.bt` | Fixed-size O(1) indexed collection (Erlang tuple-backed) |
| `AtomicCounter` | `Object` | `atomic_counter.bt` | Lock-free counter via `atomics` |
| `BEAMError` | `Error` | `beamerror.bt` | Wraps raw BEAM exceptions |
| `Bag` | `Collection` | `bag.bt` | Multiset / counted collection |
| `Behaviour` | `Object` | `behaviour.bt` | Metaclass introspection |
| `BindingChanged` | `Announcement` | `binding_changed.bt` | System event: workspace binding changed (ADR 0093) |
| `BindingsView` | `Object` | `bindings_view.bt` | Live Dictionary-protocol view over session/workspace bindings (ADR 0081) |
| `ChangeEntry` | `Value` | `change_entry.bt` | One recorded in-memory method mutation (ADR 0082 Phase 1) |
| `ChangeLog` | `Value` | `change_log.bt` | Navigable view of pending workspace changes (ADR 0082 Phase 1) |
| `Class` | `Behaviour` | `class.bt` | Class mirror |
| `ClassBuilder` | `Object` | `class_builder.bt` | Dynamic class creation |
| `ClassLoaded` | `Announcement` | `class_loaded.bt` | System event: class loaded/redefined (ADR 0093) |
| `ClassRemoved` | `Announcement` | `class_removed.bt` | System event: class removed (ADR 0093) |
| `Console` | `Object` | `console.bt` | This process's stdin/stdout/stderr (ADR 0099 §1) |
| `DateTime` | `Value` | `date_time.bt` | Date/time value type |
| `Digest` | `Object` | `digest.bt` | Cryptographic hash functions and HMAC (`crypto:hash/2`, `crypto:mac/4`) |
| `Duration` | `Value` | `duration.bt` | Span of time stored as total milliseconds; accepted by timeout-taking APIs |
| `DynamicSupervisor` | `Object` | `dynamic_supervisor.bt` | OTP DynamicSupervisor wrapper |
| `Erlang` | `Object` | `erlang.bt` | Direct Erlang module access |
| `ErlangModule` | `Object` | `erlang_module.bt` | Erlang module wrapper |
| `Ets` | `Object` | `ets.bt` | Shared in-memory table wrapper (OTP `ets`) |
| `ExitError` | `Error` | `exit_error.bt` | Process exit wrapper |
| `FileHandle` | `Object` | `file_handle.bt` | File I/O handle |
| `FlushCompleted` | `Announcement` | `flush_completed.bt` | System event: `Workspace flush` finished (ADR 0093) |
| `Inspector` | `Object` | `inspector.bt` | Live, immutable cursor for navigating into a single object (ADR 0095) |
| `InspectorField` | `Value` | `inspector_field.bt` | Immutable record for one drillable inspected field (ADR 0095 §2) |
| `Interval` | `Collection` | `interval.bt` | Arithmetic sequence (1 to: 10) |
| `Json` | `Object` | `json.bt` | JSON parse/stringify |
| `Logger` | `Object` | `logger.bt` | OTP logger wrapper |
| `Metaclass` | `Behaviour` | `metaclass.bt` | Metaclass mirror |
| `OS` | `Object` | `os.bt` | OS-level operations |
| `ObjectStateChanged` | `Announcement` | `object_state_changed.bt` | System event: watched actor commits a state write (ADR 0095 §5) |
| `Package` | `Object` | `package.bt` | Package management |
| `Parallel` | `Object` | `parallel.bt` | Block-based fan-out/join combinators (`all:`, `all:timeout:`, `any:`) — spawns one linked+monitored process per block, blocks the caller, returns plain `Result` values; no awaitable future/promise ever escapes into user code (BT-2974, ADR 0104) |
| `Pid` | `Object` | `pid.bt` | BEAM process identifier |
| `Port` | `Object` | `port.bt` | BEAM port wrapper |
| `ProcessNavigation` | `Value` | `process_navigation.bt` | Live supervision-tree introspection queries (ADR 0092) |
| `Program` | `Object` | `program.bt` | The running program/invocation (ADR 0099 §2) |
| `Protocol` | `Object` | `protocol.bt` | Protocol mirror |
| `Queue` | `Collection` | `queue.bt` | FIFO queue |
| `Random` | `Object` | `random.bt` | Random number generation |
| `ReactiveSubprocess` | `Actor` | `reactive_subprocess.bt` | Streaming subprocess |
| `Reference` | `Object` | `reference.bt` | BEAM reference wrapper |
| `Regex` | `Value` | `regex.bt` | Regular expressions |
| `Result` | `Value` | `result.bt` | Ok/Error result type |
| `RetryPolicy` | `Value` | `retry_policy.bt` | Configurable exponential backoff and retry execution (BT-2973) |
| `Server` | `Actor` | `server.bt` | OTP Server base class |
| `Session` | `Object` | `session.bt` | First-class handle to a REPL session (ADR 0081) |
| `StackFrame` | `Object` | `stack_frame.bt` | Stack trace inspection |
| `Stream` | `Object` | `stream.bt` | Lazy sequences |
| `Subprocess` | `Actor` | `subprocess.bt` | OS subprocess management |
| `Subscription` | `Object` | `subscription.bt` | Unsubscribe token returned by `when:do:` et al (ADR 0093) |
| `SubscriptionNode` | `Value` | `subscription_node.bt` | Immutable snapshot record for one live subscription (ADR 0093 §7) |
| `SupervisionChildAdded` | `Announcement` | `supervision_child_added.bt` | System event: supervisor started a child (ADR 0093) |
| `SupervisionChildCrashed` | `Announcement` | `supervision_child_crashed.bt` | System event: supervised child failed to start or crashed (ADR 0093) |
| `SupervisionNode` | `Value` | `supervision_node.bt` | Immutable snapshot record for one process in the live supervision tree (ADR 0092) |
| `SupervisionSpec` | `Value` | `supervision_spec.bt` | Supervisor child specs |
| `SupervisionTree` | `Value` | `supervision_tree.bt` | Navigable snapshot of the live supervision tree (ADR 0092) |
| `Supervisor` | `Object` | `supervisor.bt` | OTP Supervisor wrapper |
| `System` | `Object` | `system.bt` | System info and control |
| `SystemAnnouncer` | `Announcer` | `system_announcer.bt` | Singleton system event bus (ADR 0093 Layer 2) |
| `SystemNavigation` | `Object` | `system_navigation.bt` | Class-registry navigation queries ("who implements X") |
| `TestResult` | `Value` | `test_result.bt` | BUnit test result |
| `TestRunner` | `Object` | `test_runner.bt` | BUnit test runner |
| `ThrowError` | `Error` | `throw_error.bt` | Non-local return error |
| `Time` | `Object` | `time.bt` | Time operations |
| `TimeoutProxy` | `Object` | `timeout_proxy.bt` | Timeout wrapper |
| `Timer` | `Object` | `timer.bt` | Timer operations |
| `Tracing` | `Object` | `tracing.bt` | Actor observability |
| `Uuid` | `Value` | `uuid.bt` | RFC 9562 UUIDs (v4 random, v7 time-ordered) |
| `WorkspaceInterface` | `Actor` | `workspace_interface.bt` | Workspace management |

---

## Pharo Comparison: Notable Gaps

> **Tracked in existing issues:**
> - **BT-44**: Missing collection methods (`sort`, `detect:`, `take:`, `flatMap:`, etc.)
> - **BT-331**: Compilable stdlib collection classes (Dictionary ✅, List ✅, Set ✅)
> - **BT-408**: E2E test coverage for untested stdlib methods

Methods that Pharo users would expect but Beamtalk does **not** define or implement:

### ProtoObject

| Pharo Method | Beamtalk Status | Priority |
|-------------|-----------------|----------|
| `~~` (not identical) | ❌ Not defined | Low |

### Object

| Pharo Method | Beamtalk Status | Priority |
|-------------|-----------------|----------|
| `copy` | ❌ Not defined (except UndefinedObject) | Medium |
| `deepCopy` | ❌ Not defined (except UndefinedObject) | Low |
| `halt` | ❌ Not defined | Low |
| `assert:` | ❌ Not defined | Medium |
| `deny:` | ❌ Not defined | Low |

### Integer

| Pharo Method | Beamtalk Equivalent | Priority |
|-------------|---------------------|----------|
| `isPrime` | ❌ Not defined | Low |

### Float

| Pharo Method | Beamtalk Equivalent | Priority |
|-------------|---------------------|----------|
| `**` (exponentiation) | ❌ Not defined (Integer has it) | Medium |

### String

| Pharo Method | Beamtalk Equivalent | Priority |
|-------------|---------------------|----------|
| `copyFrom:to:` | ❌ Not defined (use `take:`/`drop:` combination) | Low |
| `asSymbol` | ❌ Not defined | Low |
| `match:` | ❌ Not defined | Low |

### List / Collection

| Pharo Method | Beamtalk Equivalent | Priority |
|-------------|---------------------|----------|
| `remove:` | ❌ Not defined on `List`/`Collection` (only `Set>>remove:`) | Medium |
| `asSet` | ✅ `Collection>>asSet` (BT-2976: previously required the `(Set new) fromList: aList` workaround; now a direct method) | Low |
| `asDictionary` | ❌ Not defined | Low |
| `with:collect:` | ❌ Not defined | Low |
| `at:put:` | ❌ Not defined (lists are immutable linked lists) | Low |

### Block

| Pharo Method | Beamtalk Equivalent | Priority |
|-------------|---------------------|----------|
| `cull:` | ❌ Not defined | Low |
| `newProcess` / `fork` | ❌ Not defined (use Actor >> spawn) | Low |

---

## Missing `.bt` Files

All stdlib classes now have corresponding `stdlib/src/*.bt` definitions. `Collection` is now defined in `stdlib/src/collection.bt` as an abstract typed subclass of `Value`.

---

## Test Coverage Gaps

Test coverage is now spread across both `stdlib/bootstrap-test/` (224 assertions) and `tests/repl-protocol/cases/` (1883 assertions) —
a large swing from the previous audit's 1046/213 split. These counts are a snapshot for this audit date, not a trend line:
assertions move between suites over time as tests are added, migrated, or consolidated, so don't read the swing itself as a
coverage regression or expansion.
Many previously untested methods now have stdlib test coverage. The following gaps remain for methods
with no coverage in either test suite (per-method status below has not been re-verified against the current
counts and may itself be stale — see BT-408):

**Correction (BT-2976):** the `describe` selector referenced throughout this section no longer exists on any of
these classes (see the per-class tables above) — those entries are removed below rather than left as
permanently-untested phantoms. There is no `Association` class in this repo (never was in `stdlib/src/`); the
row previously here has been removed.

### High Priority (Core functionality untested)

| Class | Untested Methods |
|-------|-----------------|
| **Integer** | `**`, `min:`, `max:`, `timesRepeat:`, `to:do:`, `to:by:do:` |
| **Float** | `/=`, `<=`, `>=`, `printString` |
| **String** | `,`, `lines`, `asAtom`, `printString` |
| **List** | `detect:ifNone:`, `printString` |
| **Block** | `repeat` |

### Medium Priority

| Class | Untested Methods |
|-------|-----------------|
| **Object** | `inspect` |
| **Actor** | `spawnWith:` |
| **UndefinedObject** | `ifNotNil:`, `ifNil:ifNotNil:`, `ifNotNil:ifNil:`, `copy`, `deepCopy`, `shallowCopy`, `printString` |
| **True/False** | `isTrue`, `isFalse`, `printString` |
| **Exception** | `signal`, `signal:` |
| **TranscriptStream** | `subscribe`, `unsubscribe`, `recent`, `clear` |
| **BeamtalkInterface** | `globals` |

---

## Testing Methodology

For each method, testing was performed in this priority order:

1. **Stdlib tests** (`stdlib/bootstrap-test/*.btscript`) — compiled expression tests (ADR 0014)
2. **E2E test files** (`tests/repl-protocol/cases/*.btscript`) — REPL integration tests
3. **Compiler intrinsics** (`crates/beamtalk-core/src/codegen/core_erlang/intrinsics.rs`) — verified codegen handler exists
4. **Primitive bindings** (`crates/beamtalk-core/src/codegen/core_erlang/primitive_bindings.rs`, `primitive_implementations.rs`) — verified selector-based dispatch codegen
5. **Runtime dispatch** (`runtime/apps/beamtalk_runtime/src/beamtalk_*.erl`) — verified dispatch clause handles the selector
6. **Pure Beamtalk** (`stdlib/src/*.bt`) — verified method body compiles (not just a comment)

A method is marked ✅ if at least one implementation path exists (intrinsic, runtime dispatch, or compiled Beamtalk).
A method is marked 🧪 if a stdlib or E2E test file exercises it with a `// =>` assertion.
