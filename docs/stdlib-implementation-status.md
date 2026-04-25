# Stdlib Implementation Status

> **Last updated:** 2026-04-01
> **Issue:** BT-247, BT-1808
> **Methodology:** Audit of `stdlib/src/*.bt` files, compiler intrinsics (`intrinsics.rs`, `primitive_bindings.rs`),
> runtime dispatch modules (`beamtalk_*.erl`), stdlib test coverage (`stdlib/bootstrap-test/*.btscript`), and REPL protocol test coverage (`tests/repl-protocol/cases/*.btscript`).

## Executive Summary

| Metric | Value |
|--------|-------|
| **Stdlib .bt files** | 76 |
| **Runtime-only classes** | 0 (all classes have stdlib/src/*.bt) |
| **Missing .bt files** | 0 |
| **Protocols** | 1 (Printable) |

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

### ProtoObject (`stdlib/src/ProtoObject.bt`)

**Class:** `ProtoObject` — superclass: `nil` (root class)
**Methods:** 5/5 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `==` | intrinsic | ✅ | 🧪 | Identity comparison |
| `/=` | intrinsic | ✅ | 🧪 | Not-equal comparison |
| `class` | intrinsic | ✅ | 🧪 | Type introspection |
| `doesNotUnderstand:args:` | intrinsic | ✅ | 🧪 | Fallback for unknown messages |
| `perform:withArguments:` | intrinsic | ✅ | 🧪 | Dynamic dispatch |

### Object (`stdlib/src/Object.bt`)

**Class:** `Object` — superclass: `ProtoObject`
**Methods:** 23/23 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `isNil` | intrinsic + pure BT | ✅ | 🧪 | Pattern match at call site; Object.bt returns `false` |
| `notNil` | intrinsic + pure BT | ✅ | 🧪 | Pattern match at call site; Object.bt returns `true` |
| `ifNil:` | intrinsic + pure BT | ✅ | 🧪 | Inline pattern match |
| `ifNotNil:` | intrinsic + pure BT | ✅ | 🧪 | Inline pattern match |
| `ifNil:ifNotNil:` | intrinsic + pure BT | ✅ | 🧪 | Inline pattern match |
| `ifNotNil:ifNil:` | intrinsic + pure BT | ✅ | 🧪 | Inline pattern match |
| `printString` | pure BT | ✅ | 🧪 | `'a ' ++ self class printString` (BT-477) |
| `displayString` | pure BT | ✅ | | Delegates to `printString`; override for user-facing display |
| `inspect` | pure BT | ✅ | | Delegates to `printString` |
| `yourself` | intrinsic + pure BT | ✅ | 🧪 | Returns self |
| `hash` | intrinsic | ✅ | 🧪 | `erlang:phash2/1` |
| `respondsTo:` | intrinsic | ✅ | 🧪 | `beamtalk_primitive:responds_to/2` |
| `fieldNames` | intrinsic | ✅ | 🧪 | Async for actors |
| `fieldAt:` | intrinsic | ✅ | 🧪 | Async for actors |
| `fieldAt:put:` | intrinsic | ✅ | 🧪 | Async for actors |
| `perform:` | intrinsic | ✅ | 🧪 | Dynamic dispatch |
| `perform:withArgs:` | intrinsic | ✅ | 🧪 | Dynamic dispatch with args |
| `subclassResponsibility` | pure BT | ✅ | 🧪 | Calls `self error:` — pure Beamtalk method (BT-405) |
| `notImplemented` | pure BT | ✅ | | Calls `self error:` — WIP stub marker |
| `show:` | pure BT | ✅ | | Nil-safe Transcript output (no newline); returns `self` |
| `showCr:` | pure BT | ✅ | | Nil-safe Transcript output (with newline); returns `self` |
| `isKindOf:` | pure BT | ✅ | | `self class includesBehaviour: aClass` |
| `error:` | intrinsic | ✅ | 🧪 | Smalltalk-style error signaling |
| `sealed` | modifier | ✅ | 🧪 | Method modifier preventing override |

_Note:_ `sealed` is a method **modifier** in Beamtalk (for example, `sealed getValue => ...`), not an `Object` selector. It is documented here for completeness but is not counted as a stdlib method.

_Note:_ `new` and `new:` have moved to `Value` (see below). Object subclasses without data (FFI namespaces, abstract extension points) cannot be directly instantiated.

### Value (`stdlib/src/Value.bt`)

**Class:** `Value` — superclass: `Object`
**Methods:** 3/3 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `new` | intrinsic `basicNew` | ✅ | 🧪 | Inline codegen for value type instantiation |
| `new:` | intrinsic `basicNewWith` | ✅ | 🧪 | Instantiation with constructor args |
| `inspect` | pure BT | ✅ | | `ClassName(field: value, ...)` format |

### Collection (`stdlib/src/Collection.bt`)

**Class:** `Collection` — superclass: `Value` — `abstract typed`
**Methods:** 12/12 implemented (100%)
**Note:** Abstract superclass for List, Set, Array, Binary, etc. Provides default iteration built on `size` and `do:`.

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `withAll:` | pure BT | ✅ | | Factory — create from list |
| `size` | abstract | ✅ | | Subclass must implement |
| `do:` | abstract | ✅ | | Subclass must implement |
| `isEmpty` | pure BT | ✅ | 🧪 | `self size =:= 0` |
| `isNotEmpty` | pure BT | ✅ | 🧪 | `self isEmpty not` |
| `includes:` | pure BT | ✅ | | Linear search via `do:` |
| `collect:` | pure BT | ✅ | 🧪 | Map via `do:` |
| `select:` | pure BT | ✅ | 🧪 | Filter via `do:` |
| `reject:` | pure BT | ✅ | 🧪 | Negated filter |
| `inject:into:` | pure BT | ✅ | 🧪 | Fold via `do:` |
| `detect:` | pure BT | ✅ | | First match |
| `detect:ifNone:` | pure BT | ✅ | | First match with default |

### Binary (`stdlib/src/Binary.bt`)

**Class:** `Binary` — superclass: `Collection` — `@sealed typed` ([ADR 0069](ADR/0069-string-subclass-of-binary.md))
**Methods:** 14/14 implemented (100%)
**Note:** Byte-level data and serialization. Parent of String. Maps to Erlang `binary()`.

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `class serialize:` | @primitive selector | ✅ | 🧪 | External term format |
| `class deserialize:` | @primitive selector | ✅ | 🧪 | Reverse of serialize |
| `class deserializeWithUsed:` | @primitive selector | ✅ | | Returns `#(value, bytesConsumed)` |
| `class fromBytes:` | @primitive selector | ✅ | 🧪 | Build from byte list |
| `class fromIolist:` | @primitive selector | ✅ | 🧪 | Build from iolist |
| `byteSize` | @primitive selector | ✅ | 🧪 | Byte count |
| `byteAt:` | @primitive selector | ✅ | 🧪 | 0-based byte access |
| `toBytes` | @primitive selector | ✅ | 🧪 | Byte list |
| `part:size:` | @primitive selector | ✅ | 🧪 | Byte-level slice |
| `concat:` | @primitive selector | ✅ | 🧪 | Byte concatenation |
| `size` | @primitive selector | ✅ | 🧪 | Same as `byteSize` |
| `do:` | @primitive selector | ✅ | 🧪 | Iterate bytes |
| `printString` | pure BT | ✅ | | Developer representation |
| `asString` | @primitive selector | ✅ | | UTF-8 decode |

### Number (`stdlib/src/Number.bt`)

**Class:** `Number` — superclass: `Value` — `abstract typed`
**Methods:** 5/5 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `isZero` | pure BT | ✅ | 🧪 | `Number>>isZero` |
| `isPositive` | pure BT | ✅ | 🧪 | `Number>>positive` |
| `isNegative` | pure BT | ✅ | 🧪 | `Number>>negative` |
| `sign` | pure BT | ✅ | 🧪 | `Number>>sign` |
| `between:and:` | pure BT | ✅ | 🧪 | `Magnitude>>between:and:` |

### Integer (`stdlib/src/Integer.bt`)

**Class:** `Integer` — superclass: `Number` — `@sealed`
**Methods:** 38/38 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `+` | @primitive selector | ✅ | 🧪 | `Integer>>+` |
| `-` | @primitive selector | ✅ | 🧪 | `Integer>>-` |
| `*` | @primitive selector | ✅ | 🧪 | `Integer>>*` |
| `/` | @primitive selector | ✅ | 🧪 | `Integer>>/` |
| `%` | @primitive selector | ✅ | 🧪 | `Integer>>\\` |
| `**` | @primitive selector | ✅ | | `Integer>>raisedTo:` |
| `=:=` | @primitive selector | ✅ | 🧪 | `Integer>>=` |
| `/=` | @primitive selector | ✅ | 🧪 | `Integer>>~=` |
| `<` | @primitive selector | ✅ | 🧪 | `Integer>><` |
| `>` | @primitive selector | ✅ | 🧪 | `Integer>>>` |
| `<=` | @primitive selector | ✅ | 🧪 | `Integer>><=` |
| `>=` | @primitive selector | ✅ | 🧪 | `Integer>>>=` |
| `negated` | pure BT | ✅ | 🧪 | `Integer>>negated` |
| `abs` | pure BT | ✅ | 🧪 | `Integer>>abs` |
| `isEven` | pure BT | ✅ | 🧪 | `Integer>>even` |
| `isOdd` | pure BT | ✅ | 🧪 | `Integer>>odd` |
| `min:` | pure BT | ✅ | | `Magnitude>>min:` |
| `max:` | pure BT | ✅ | | `Magnitude>>max:` |
| `timesRepeat:` | intrinsic | ✅ | | `Integer>>timesRepeat:` |
| `to:do:` | intrinsic | ✅ | | `Integer>>to:do:` |
| `to:by:do:` | intrinsic | ✅ | | `Integer>>to:by:do:` |
| `asFloat` | @primitive selector | ✅ | 🧪 | `Integer>>asFloat` |
| `asString` | @primitive selector | ✅ | 🧪 | `Integer>>asString` |
| `printString` | @primitive selector | ✅ | 🧪 | `Integer>>printString` |
| `describe` | pure BT | ✅ | | N/A |
| `bitAnd:` | @primitive selector | ✅ | 🧪 | `Integer>>bitAnd:` |
| `bitOr:` | @primitive selector | ✅ | 🧪 | `Integer>>bitOr:` |
| `bitXor:` | @primitive selector | ✅ | 🧪 | `Integer>>bitXor:` |
| `bitShift:` | @primitive selector | ✅ | 🧪 | `Integer>>bitShift:` |
| `bitNot` | @primitive selector | ✅ | 🧪 | `Integer>>bitNot` |
| `factorial` | pure BT | ✅ | 🧪 | `Integer>>factorial` |
| `gcd:` | pure BT | ✅ | 🧪 | `Integer>>gcd:` |
| `lcm:` | pure BT | ✅ | 🧪 | `Integer>>lcm:` |
| `isLetter` | @primitive selector | ✅ | | Character classification (BT-461) |
| `isDigit` | @primitive selector | ✅ | | Character classification (BT-461) |
| `isUppercase` | @primitive selector | ✅ | | Character classification (BT-461) |
| `isLowercase` | @primitive selector | ✅ | | Character classification (BT-461) |
| `isWhitespace` | @primitive selector | ✅ | | Character classification (BT-461) |

### String (`stdlib/src/String.bt`)

**Class:** `String` — superclass: `Binary` — `@sealed` ([ADR 0069](ADR/0069-string-subclass-of-binary.md))
**Methods:** 48/48 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `=:=` | @primitive selector | ✅ | 🧪 | `String>>=` |
| `/=` | @primitive selector | ✅ | 🧪 | `String>>~=` |
| `<` | @primitive selector | ✅ | 🧪 | `String>><` |
| `>` | @primitive selector | ✅ | 🧪 | `String>>>` |
| `<=` | @primitive selector | ✅ | 🧪 | `String>><=` |
| `>=` | @primitive selector | ✅ | 🧪 | `String>>>=` |
| `++` | @primitive selector | ✅ | 🧪 | `String>>,` |
| `,` | pure BT | ✅ | | `String>>,` |
| `length` | @primitive selector | ✅ | 🧪 | `String>>size` |
| `size` | pure BT | ✅ | 🧪 | `String>>size` |
| `at:` | @primitive selector | ✅ | 🧪 | `String>>at:` |
| `uppercase` | @primitive selector | ✅ | 🧪 | `String>>asUppercase` |
| `lowercase` | @primitive selector | ✅ | 🧪 | `String>>asLowercase` |
| `capitalize` | @primitive selector | ✅ | 🧪 | `String>>capitalized` |
| `trim` | @primitive selector | ✅ | 🧪 | `String>>trimBoth` |
| `trimLeft` | @primitive selector | ✅ | 🧪 | `String>>trimLeft` |
| `trimRight` | @primitive selector | ✅ | 🧪 | `String>>trimRight` |
| `reverse` | @primitive selector | ✅ | 🧪 | `String>>reversed` |
| `includes:` | @primitive selector | ✅ | 🧪 | `String>>includesSubstring:` |
| `startsWith:` | @primitive selector | ✅ | 🧪 | `String>>beginsWith:` |
| `endsWith:` | @primitive selector | ✅ | 🧪 | `String>>endsWith:` |
| `indexOf:` | @primitive selector | ✅ | 🧪 | `String>>indexOfSubCollection:` |
| `split:` | @primitive selector | ✅ | 🧪 | N/A |
| `splitOn:` | @primitive selector | ✅ | 🧪 | N/A |
| `repeat:` | @primitive selector | ✅ | 🧪 | N/A |
| `lines` | @primitive selector | ✅ | | `String>>lines` |
| `words` | @primitive selector | ✅ | 🧪 | `String>>substrings` |
| `replaceAll:with:` | @primitive selector | ✅ | 🧪 | `String>>replaceAll:with:` |
| `replaceFirst:with:` | @primitive selector | ✅ | 🧪 | `String>>copyReplaceFirst:with:` |
| `take:` | @primitive selector | ✅ | 🧪 | `String>>first:` |
| `drop:` | @primitive selector | ✅ | 🧪 | `String>>allButFirst:` |
| `padLeft:` | @primitive selector | ✅ | 🧪 | `String>>padLeftTo:` |
| `padRight:` | @primitive selector | ✅ | 🧪 | `String>>padRightTo:` |
| `padLeft:with:` | @primitive selector | ✅ | 🧪 | `String>>padLeftTo:with:` |
| `isBlank` | @primitive selector | ✅ | 🧪 | `String>>isAllSeparators` |
| `isDigit` | @primitive selector | ✅ | 🧪 | `String>>isAllDigits` |
| `isAlpha` | @primitive selector | ✅ | 🧪 | `String>>isAllLetters` |
| `isEmpty` | pure BT | ✅ | 🧪 | `String>>isEmpty` |
| `isNotEmpty` | pure BT | ✅ | 🧪 | `String>>isNotEmpty` |
| `asInteger` | @primitive selector | ✅ | 🧪 | `String>>asInteger` |
| `asFloat` | @primitive selector | ✅ | 🧪 | `String>>asFloat` |
| `asAtom` | @primitive selector | ✅ | | N/A (BEAM-specific) |
| `asList` | @primitive selector | ✅ | 🧪 | `String>>asArray` |
| `each:` | @primitive selector | ✅ | 🧪 | `String>>do:` |
| `collect:` | @primitive selector | ✅ | 🧪 | `String>>collect:` |
| `select:` | @primitive selector | ✅ | 🧪 | `String>>select:` |
| `describe` | pure BT | ✅ | | N/A |
| `printString` | pure BT | ✅ | | `String>>printString` |

### List (`stdlib/src/List.bt`)

**Class:** `List` — superclass: `Collection` — `@sealed typed`
**Methods:** 38/38 implemented (100%)
**Note:** List in Beamtalk maps to Erlang linked lists. Literal syntax: `#(1, 2, 3)`. Renamed from Array in BT-419 — `Array` is reserved for a future tuple-backed O(1)-indexed collection.
**Migration:** BT-419 — migrated from hand-written `beamtalk_list.erl` (Option B) to compiled `stdlib/src/List.bt` with BIF mappings (Option A). Complex operations delegate to `beamtalk_list_ops.erl`.

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `size` | @primitive BIF (`erlang:length`) | ✅ | 🧪 | `SequenceableCollection>>size` |
| `isEmpty` | @primitive BIF (`=:= []`) | ✅ | 🧪 | `Collection>>isEmpty` |
| `first` | @primitive BIF (`hd`) | ✅ | 🧪 | `SequenceableCollection>>first` |
| `last` | @primitive BIF (`lists:last`) | ✅ | 🧪 | `SequenceableCollection>>last` |
| `rest` | @primitive BIF (`tl`) | ✅ | 🧪 | `SequenceableCollection>>allButFirst` |
| `at:` | @primitive → `beamtalk_list_ops:at/2` | ✅ | 🧪 | `SequenceableCollection>>at:` |
| `includes:` | @primitive BIF (`lists:member`) | ✅ | 🧪 | `Collection>>includes:` |
| `add:` | @primitive BIF (prepend `[Arg\|Self]`) | ✅ | 🧪 | `OrderedCollection>>add:` |
| `sort` | @primitive BIF (`lists:sort`) | ✅ | 🧪 | `SequenceableCollection>>sort` |
| `sort:` | @primitive → `beamtalk_list_ops:sort_with/2` | ✅ | 🧪 | `SequenceableCollection>>sort:` |
| `reversed` | @primitive BIF (`lists:reverse`) | ✅ | 🧪 | `SequenceableCollection>>reversed` |
| `unique` | @primitive BIF (`lists:usort`) | ✅ | 🧪 | `Collection>>asSet asArray` |
| `flatten` | @primitive BIF (`lists:flatten`) | ✅ | 🧪 | `Collection>>flattened` |
| `do:` | @primitive → `beamtalk_list_ops:do/2` | ✅ | 🧪 | `Collection>>do:` |
| `collect:` | @primitive BIF (`lists:map`) | ✅ | 🧪 | `Collection>>collect:` |
| `select:` | @primitive BIF (`lists:filter`) | ✅ | 🧪 | `Collection>>select:` |
| `reject:` | @primitive → `beamtalk_list_ops:reject/2` | ✅ | 🧪 | `Collection>>reject:` |
| `inject:into:` | @primitive → `beamtalk_collection_ops:inject_into/3` | ✅ | 🧪 | `Collection>>inject:into:` |
| `detect:` | @primitive → `beamtalk_list_ops:detect/2` | ✅ | 🧪 | `Collection>>detect:` |
| `detect:ifNone:` | @primitive → `beamtalk_list_ops:detect_if_none/3` | ✅ | | `Collection>>detect:ifNone:` |
| `flatMap:` | @primitive BIF (`lists:flatmap`) | ✅ | 🧪 | `Collection>>flatCollect:` |
| `count:` | @primitive BIF (foldl count) | ✅ | 🧪 | `Collection>>count:` |
| `anySatisfy:` | @primitive BIF (`lists:any`) | ✅ | 🧪 | `Collection>>anySatisfy:` |
| `allSatisfy:` | @primitive BIF (`lists:all`) | ✅ | 🧪 | `Collection>>allSatisfy:` |
| `++` | @primitive → `beamtalk_list_ops:concat/2` | ✅ | 🧪 | `SequenceableCollection>>,` |
| `from:to:` | @primitive → `beamtalk_list_ops:from_to/3` | ✅ | 🧪 | `SequenceableCollection>>copyFrom:to:` |
| `indexOf:` | @primitive → `beamtalk_list_ops:index_of/2` | ✅ | 🧪 | `SequenceableCollection>>indexOf:` |
| `eachWithIndex:` | @primitive → `beamtalk_list_ops:each_with_index/2` | ✅ | 🧪 | `SequenceableCollection>>withIndexDo:` |
| `take:` | @primitive → `beamtalk_list_ops:take/2` | ✅ | 🧪 | `SequenceableCollection>>first:` |
| `drop:` | @primitive → `beamtalk_list_ops:drop/2` | ✅ | 🧪 | `SequenceableCollection>>allButFirst:` |
| `takeWhile:` | @primitive BIF (`lists:takewhile`) | ✅ | 🧪 | N/A |
| `dropWhile:` | @primitive BIF (`lists:dropwhile`) | ✅ | 🧪 | N/A |
| `zip:` | @primitive → `beamtalk_list_ops:zip/2` | ✅ | 🧪 | `SequenceableCollection>>with:collect:` |
| `groupBy:` | @primitive → `beamtalk_list_ops:group_by/2` | ✅ | 🧪 | `Collection>>groupedBy:` |
| `partition:` | @primitive → `beamtalk_list_ops:partition/2` | ✅ | 🧪 | `Collection>>partition:` |
| `intersperse:` | @primitive → `beamtalk_list_ops:intersperse/2` | ✅ | 🧪 | N/A |
| `describe` | pure BT | ✅ | | N/A |

### Block (`stdlib/src/Block.bt`)

**Class:** `Block` — superclass: `Object` — `@sealed`
**Methods:** 12/12 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `value` | intrinsic `blockValue` | ✅ | 🧪 | `BlockClosure>>value` |
| `value:` | intrinsic `blockValue1` | ✅ | 🧪 | `BlockClosure>>value:` |
| `value:value:` | intrinsic `blockValue2` | ✅ | 🧪 | `BlockClosure>>value:value:` |
| `value:value:value:` | intrinsic `blockValue3` | ✅ | 🧪 | `BlockClosure>>value:value:value:` |
| `whileTrue:` | intrinsic `whileTrue` | ✅ | 🧪 | `BlockClosure>>whileTrue:` |
| `whileFalse:` | intrinsic `whileFalse` | ✅ | 🧪 | `BlockClosure>>whileFalse:` |
| `repeat` | intrinsic `repeat` | ✅ | | `BlockClosure>>repeat` |
| `on:do:` | intrinsic `onDo` | ✅ | 🧪 | `BlockClosure>>on:do:` |
| `ensure:` | intrinsic `ensure` | ✅ | 🧪 | `BlockClosure>>ensure:` |
| `arity` | @primitive selector | ✅ | 🧪 | `BlockClosure>>argumentCount` |
| `valueWithArguments:` | @primitive selector | ✅ | 🧪 | `BlockClosure>>valueWithArguments:` |
| `describe` | pure BT | ✅ | | N/A |

### True (`stdlib/src/True.bt`) & False (`stdlib/src/False.bt`)

**Class:** `True` / `False` — superclass: `Boolean` — `@sealed`
**Methods:** 8/8 implemented each (100%)
**Inherits:** `and:`, `or:`, `xor:`, `isBoolean` from `Boolean`

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `ifTrue:ifFalse:` | pure BT | ✅ | 🧪 | `Boolean>>ifTrue:ifFalse:` |
| `ifTrue:` | pure BT | ✅ | 🧪 | `Boolean>>ifTrue:` |
| `ifFalse:` | pure BT | ✅ | 🧪 | `Boolean>>ifFalse:` |
| `not` | pure BT | ✅ | 🧪 | `Boolean>>not` |
| `isTrue` | pure BT | ✅ | | N/A |
| `isFalse` | pure BT | ✅ | | N/A |
| `describe` | pure BT | ✅ | | N/A |
| `printString` | pure BT | ✅ | | `Boolean>>printString` |

### UndefinedObject (`stdlib/src/UndefinedObject.bt`)

**Class:** `UndefinedObject` — superclass: `Object` — `@sealed`
**Methods:** 11/11 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `isNil` | pure BT | ✅ | 🧪 | `UndefinedObject>>isNil` |
| `notNil` | pure BT | ✅ | 🧪 | `UndefinedObject>>notNil` |
| `ifNil:` | pure BT | ✅ | 🧪 | `UndefinedObject>>ifNil:` |
| `ifNotNil:` | pure BT | ✅ | | `UndefinedObject>>ifNotNil:` |
| `ifNil:ifNotNil:` | pure BT | ✅ | | `UndefinedObject>>ifNil:ifNotNil:` |
| `ifNotNil:ifNil:` | pure BT | ✅ | | `UndefinedObject>>ifNotNil:ifNil:` |
| `copy` | pure BT | ✅ | | `UndefinedObject>>shallowCopy` |
| `deepCopy` | pure BT | ✅ | | `UndefinedObject>>deepCopy` |
| `shallowCopy` | pure BT | ✅ | | `UndefinedObject>>shallowCopy` |
| `describe` | pure BT | ✅ | | N/A |
| `printString` | pure BT | ✅ | | `UndefinedObject>>printString` |

### Float (`stdlib/src/Float.bt`)

**Class:** `Float` — superclass: `Number` — `@sealed`
**Methods:** 25/25 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `+` | @primitive selector | ✅ | 🧪 | `Float>>+` |
| `-` | @primitive selector | ✅ | 🧪 | `Float>>-` |
| `*` | @primitive selector | ✅ | 🧪 | `Float>>*` |
| `/` | @primitive selector | ✅ | 🧪 | `Float>>/` |
| `=:=` | @primitive selector | ✅ | 🧪 | `Float>>=` |
| `/=` | @primitive selector | ✅ | | `Float>>~=` |
| `<` | @primitive selector | ✅ | 🧪 | `Float>><` |
| `>` | @primitive selector | ✅ | 🧪 | `Float>>>` |
| `<=` | @primitive selector | ✅ | | `Float>><=` |
| `>=` | @primitive selector | ✅ | | `Float>>>=` |
| `negated` | pure BT | ✅ | 🧪 | `Float>>negated` |
| `abs` | pure BT | ✅ | 🧪 | `Float>>abs` |
| `min:` | pure BT | ✅ | 🧪 | `Magnitude>>min:` |
| `max:` | pure BT | ✅ | 🧪 | `Magnitude>>max:` |
| `rounded` | @primitive selector | ✅ | 🧪 | `Float>>rounded` |
| `ceiling` | @primitive selector | ✅ | 🧪 | `Float>>ceiling` |
| `floor` | @primitive selector | ✅ | 🧪 | `Float>>floor` |
| `truncated` | @primitive selector | ✅ | 🧪 | `Float>>truncated` |
| `isNaN` | pure BT | ✅ | 🧪 | `Float>>isNaN` |
| `isInfinite` | pure BT | ✅ | 🧪 | `Float>>isInfinite` |
| `isZero` | pure BT | ✅ | 🧪 | `Float>>isZero` |
| `asInteger` | @primitive selector | ✅ | 🧪 | `Float>>asInteger` |
| `asString` | @primitive selector | ✅ | 🧪 | `Float>>asString` |
| `printString` | @primitive selector | ✅ | | `Float>>printString` |
| `describe` | pure BT | ✅ | | N/A |

---

## Tier 2: Standard Classes

### Actor (`stdlib/src/Actor.bt`)

**Class:** `Actor` — superclass: `Object` — `@sealed`
**Methods:** 5/5 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `spawn` | intrinsic `actorSpawn` | ✅ | 🧪 | `gen_server:start_link` |
| `spawnWith:` | intrinsic `actorSpawnWith` | ✅ | | With constructor args |
| `new` | pure BT | ✅ | | Error: "Use spawn instead" |
| `new:` | pure BT | ✅ | | Error: "Use spawnWith: instead" |
| `describe` | pure BT | ✅ | | Returns string literal |

### File (`stdlib/src/File.bt`)

**Class:** `File` — superclass: `Object`
**Methods:** 3/3 implemented (100%) — all class-level methods

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `exists:` | @primitive selector | ✅ | 🧪 | `FileReference>>exists` |
| `readAll:` | @primitive selector | ✅ | 🧪 | `FileReference>>contents` |
| `writeAll:contents:` | @primitive selector | ✅ | 🧪 | `FileReference>>writeStream` |

### Beamtalk / BeamtalkInterface (`stdlib/src/BeamtalkInterface.bt`)

**Class:** `BeamtalkInterface` — superclass: `Actor`
**Methods:** 4/4 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `allClasses` | @primitive selector | ✅ | 🧪 | `Smalltalk>>allClasses` |
| `classNamed:` | @primitive selector | ✅ | 🧪 | `Smalltalk>>at:` |
| `globals` | @primitive selector | ✅ | | `Smalltalk>>globals` |
| `version` | @primitive selector | ✅ | 🧪 | N/A |

### Dictionary (`stdlib/src/Dictionary.bt` — BT-418)

**Class:** `Dictionary(K, V)` — superclass: `Collection` — `@sealed`
**Helper module:** `beamtalk_map_ops.erl` (complex operations)
**Methods:** 12 — all implemented

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `keys` | @primitive selector | ✅ | 🧪 | `Dictionary>>keys` |
| `values` | @primitive selector | ✅ | 🧪 | `Dictionary>>values` |
| `size` | @primitive selector | ✅ | 🧪 | `Dictionary>>size` |
| `at:` | @primitive selector | ✅ | 🧪 | `Dictionary>>at:` |
| `at:ifAbsent:` | @primitive selector | ✅ | 🧪 | `Dictionary>>at:ifAbsent:` |
| `at:put:` | @primitive selector | ✅ | 🧪 | `Dictionary>>at:put:` |
| `includesKey:` | @primitive selector | ✅ | 🧪 | `Dictionary>>includesKey:` |
| `removeKey:` | @primitive selector | ✅ | 🧪 | `Dictionary>>removeKey:` |
| `merge:` | @primitive selector | ✅ | 🧪 | `Dictionary>>merge:` |
| `doWithKey:` | @primitive selector | ✅ | 🧪 | `Dictionary>>doWithKey:` |
| `keysAndValuesDo:` | pure BT (delegates to `doWithKey:`) | ✅ | 🧪 | `Dictionary>>keysAndValuesDo:` |
| `describe` | pure BT | ✅ | | `Dictionary>>printString` |

### Set (`stdlib/src/Set.bt` — BT-73)

**Class:** `Set(E)` — superclass: `Collection` — `@sealed`
**Helper module:** `beamtalk_set_ops.erl` (ordsets operations + tagged map wrapping)
**Representation:** Tagged map `#{'$beamtalk_class' => 'Set', elements => [sorted_list]}`
**Methods:** 14 — all implemented

| Selector | Status | Notes | Pharo Equivalent |
|----------|--------|-------|------------------|
| `new` | ✅ | Auto-generated, empty set | `Set>>new` |
| `size` | ✅ | `length(Elements)` | `Set>>size` |
| `isEmpty` | ✅ | `Elements == []` | `Set>>isEmpty` |
| `includes:` | ✅ | `ordsets:is_element` | `Set>>includes:` |
| `add:` | ✅ | `ordsets:add_element` | `Set>>add:` |
| `remove:` | ✅ | `ordsets:del_element` | `Set>>remove:` |
| `union:` | ✅ | `ordsets:union` | `Set>>union:` |
| `intersection:` | ✅ | `ordsets:intersection` | `Set>>intersection:` |
| `difference:` | ✅ | `ordsets:subtract` | `Set>>difference:` |
| `isSubsetOf:` | ✅ | `ordsets:is_subset` | `Set>>isSubsetOf:` |
| `asList` | ✅ | Returns sorted elements | `Set>>asArray` |
| `fromList:` | ✅ | `ordsets:from_list` | `Set>>addAll:` |
| `do:` | ✅ | Iterate elements | `Set>>do:` |
| `printString` | ✅ | `beamtalk_primitive:print_string/1` | `Set>>printString` (BT-477) |
| `describe` | ✅ | Returns `'a Set'` | `Set>>printString` |

**Test coverage:** 14 of 15 methods tested in `stdlib/bootstrap-test/set.bt` (47 assertions). Only `describe` lacks test coverage.

### Tuple (`stdlib/src/Tuple.bt`)

**Class:** `Tuple` — superclass: `Collection` — `@sealed typed`
**Methods:** 8/8 implemented (100%)
**Note:** BEAM-specific, wraps Erlang result tuples `{ok, Value}` / `{error, Reason}`.

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `size` | @primitive selector | ✅ | | `tuple_size` |
| `at:` | @primitive selector | ✅ | | 1-based index via `element` |
| `isOk` | @primitive selector | ✅ | | `{ok, _}` pattern match |
| `isError` | @primitive selector | ✅ | | `{error, _}` pattern match |
| `unwrap` | @primitive selector | ✅ | | Extract value or raise |
| `unwrapOr:` | @primitive selector | ✅ | | Extract or return default |
| `unwrapOrElse:` | @primitive selector | ✅ | | Extract or evaluate block |
| `asString` | @primitive selector | ✅ | | String representation |

### Symbol (`stdlib/src/Symbol.bt`)

**Class:** `Symbol` — superclass: `Object` — `@sealed`
**Methods:** 7/7 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `asString` | @primitive selector | ✅ | 🧪 | `Symbol>>asString` |
| `asAtom` | @primitive selector | ✅ | 🧪 | N/A (BEAM-specific) |
| `printString` | @primitive selector | ✅ | 🧪 | `Symbol>>printString` |
| `describe` | @primitive selector | ✅ | 🧪 | N/A |
| `=:=` | @primitive selector | ✅ | 🧪 | `Symbol>>=` |
| `/=` | @primitive selector | ✅ | 🧪 | `Symbol>>~=` |
| `hash` | @primitive selector | ✅ | 🧪 | `Symbol>>hash` |

### Exception (`stdlib/src/Exception.bt`)

**Class:** `Exception` — superclass: `Object`
**Methods:** 9/9 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `message` | @primitive selector | ✅ | 🧪 | `Exception>>messageText` |
| `hint` | @primitive selector | ✅ | 🧪 | N/A |
| `kind` | @primitive selector | ✅ | 🧪 | N/A |
| `selector` | @primitive selector | ✅ | 🧪 | N/A |
| `errorClass` | @primitive selector | ✅ | 🧪 | N/A |
| `printString` | @primitive selector | ✅ | 🧪 | `Exception>>printString` |
| `describe` | pure BT | ✅ | | N/A |
| `signal` | @primitive selector | ✅ | | `Exception>>signal` |
| `signal:` | @primitive selector | ✅ | | `Exception>>signal:` |

### Error (`stdlib/src/Error.bt`)

**Class:** `Error` — superclass: `Exception`
**Methods:** 1/1 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `describe` | pure BT | ✅ | | N/A |

### TranscriptStream (`stdlib/src/TranscriptStream.bt`)

**Class:** `TranscriptStream` — superclass: `Actor` (native: `beamtalk_transcript_stream`)
**Methods:** 6/6 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `show:` | native delegate | ✅ | 🧪 | `Transcript>>show:` — accepts `Printable` |
| `cr` | native delegate | ✅ | 🧪 | `Transcript>>cr` |
| `subscribe` | native delegate | ✅ | | N/A |
| `unsubscribe` | native delegate | ✅ | | N/A |
| `recent` | native delegate | ✅ | | N/A |
| `clear` | native delegate | ✅ | | N/A |

---

### CompiledMethod (`stdlib/src/CompiledMethod.bt`)

**Class:** `CompiledMethod` — superclass: `Object`
**Methods:** 5/5 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `selector` | @primitive selector | ✅ | 🧪 | `CompiledMethod>>selector` |
| `source` | @primitive selector | ✅ | 🧪 | `CompiledMethod>>sourceCode` |
| `argumentCount` | @primitive selector | ✅ | 🧪 | `CompiledMethod>>numArgs` |
| `printString` | @primitive selector | ✅ | 🧪 | `CompiledMethod>>printString` |
| `asString` | @primitive selector | ✅ | | `CompiledMethod>>asString` |

### Character (`stdlib/src/Character.bt`)

**Class:** `Character` — superclass: `Integer` — `@sealed`
**Methods:** 19/19 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `=:=` | @primitive selector | ✅ | 🧪 | Character equality |
| `/=` | @primitive selector | ✅ | 🧪 | Character not-equal |
| `<` | @primitive selector | ✅ | 🧪 | Ordering |
| `>` | @primitive selector | ✅ | 🧪 | Ordering |
| `<=` | @primitive selector | ✅ | 🧪 | Ordering |
| `>=` | @primitive selector | ✅ | 🧪 | Ordering |
| `asInteger` | @primitive selector | ✅ | 🧪 | Unicode code point |
| `asString` | @primitive selector | ✅ | 🧪 | Single-character string |
| `printString` | @primitive selector | ✅ | 🧪 | Display representation |
| `describe` | pure BT | ✅ | | N/A |
| `hash` | @primitive selector | ✅ | 🧪 | Hash value |
| `isLetter` | @primitive selector | ✅ | 🧪 | Unicode letter check |
| `isDigit` | @primitive selector | ✅ | 🧪 | Unicode digit check |
| `isUppercase` | @primitive selector | ✅ | 🧪 | Case check |
| `isLowercase` | @primitive selector | ✅ | 🧪 | Case check |
| `isWhitespace` | @primitive selector | ✅ | 🧪 | Whitespace check |
| `uppercase` | @primitive selector | ✅ | 🧪 | Case conversion |
| `lowercase` | @primitive selector | ✅ | 🧪 | Case conversion |
| `class value:` | @primitive selector | ✅ | 🧪 | Construct from code point |

### Boolean (`stdlib/src/Boolean.bt`)

**Class:** `Boolean` — superclass: `Value` — `abstract`
**Methods:** 4/4 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `isBoolean` | pure BT | ✅ | 🧪 | Type check |
| `and:` | pure BT | ✅ | 🧪 | Logical AND |
| `or:` | pure BT | ✅ | 🧪 | Logical OR |
| `xor:` | pure BT | ✅ | 🧪 | Logical XOR |

### TestCase (`stdlib/src/TestCase.bt`)

**Class:** `TestCase` — superclass: `Value`
**Methods:** 7/7 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `setUp` | pure BT | ✅ | 🧪 | Override for test setup |
| `tearDown` | pure BT | ✅ | 🧪 | Override for test cleanup |
| `assert:` | @primitive selector | ✅ | 🧪 | Assert truthy |
| `assert:equals:` | @primitive selector | ✅ | 🧪 | Assert equality |
| `deny:` | @primitive selector | ✅ | 🧪 | Assert falsy |
| `should:raise:` | @primitive selector | ✅ | 🧪 | Assert exception |
| `fail:` | @primitive selector | ✅ | 🧪 | Fail with message |

### InstantiationError (`stdlib/src/InstantiationError.bt`)

**Class:** `InstantiationError` — superclass: `Error`
**Methods:** 1/1 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `describe` | pure BT | ✅ | 🧪 | Error description |

### RuntimeError (`stdlib/src/RuntimeError.bt`)

**Class:** `RuntimeError` — superclass: `Error`
**Methods:** 1/1 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `describe` | pure BT | ✅ | 🧪 | Error description |

### TypeError (`stdlib/src/TypeError.bt`)

**Class:** `TypeError` — superclass: `Error`
**Methods:** 1/1 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `describe` | pure BT | ✅ | 🧪 | Error description |

---

## Protocols

### Printable (`stdlib/src/Printable.bt`)

**Protocol:** `Printable` — structural protocol (ADR 0068, BT-1766)
**Required methods:** 2

| Selector | Return Type | Notes |
|----------|-------------|-------|
| `asString` | `String` | Human-readable string representation |
| `printString` | `String` | Developer-oriented representation (debugging, REPL) |

**Conformance:** Automatic — any class implementing both `asString` and `printString` conforms.
Most stdlib classes conform because `Object` provides a default `printString` and subclasses typically override `asString`.

**Usage:** `TranscriptStream >> show:` accepts `Printable`, so conforming objects can be displayed directly without manual `asString` calls.

---

## Classes Added Since v0.3.1

The following stdlib `.bt` files have been added since the previous audit (2026-02-12). They are listed here for completeness; full method-level audit is tracked as future work.

| Class | Superclass | File | Notes |
|-------|------------|------|-------|
| `Array` | `Collection` | `Array.bt` | Fixed-size O(1) indexed collection (Erlang tuple-backed) |
| `AtomicCounter` | `Object` | `AtomicCounter.bt` | Lock-free counter via `atomics` |
| `BEAMError` | `Error` | `BEAMError.bt` | Wraps raw BEAM exceptions |
| `Bag` | `Collection` | `Bag.bt` | Multiset / counted collection |
| `Behaviour` | `Object` | `Behaviour.bt` | Metaclass introspection |
| `Class` | `Behaviour` | `Class.bt` | Class mirror |
| `ClassBuilder` | `Object` | `ClassBuilder.bt` | Dynamic class creation |
| `DateTime` | `Value` | `DateTime.bt` | Date/time value type |
| `DynamicSupervisor` | `Object` | `DynamicSupervisor.bt` | OTP DynamicSupervisor wrapper |
| `Erlang` | `Object` | `Erlang.bt` | Direct Erlang module access |
| `ErlangModule` | `Object` | `ErlangModule.bt` | Erlang module wrapper |
| `ExitError` | `Error` | `ExitError.bt` | Process exit wrapper |
| `FileHandle` | `Object` | `FileHandle.bt` | File I/O handle |
| `Interval` | `Collection` | `Interval.bt` | Arithmetic sequence (1 to: 10) |
| `Json` | `Object` | `Json.bt` | JSON parse/stringify |
| `Logger` | `Object` | `Logger.bt` | OTP logger wrapper |
| `Metaclass` | `Behaviour` | `Metaclass.bt` | Metaclass mirror |
| `OS` | `Object` | `OS.bt` | OS-level operations |
| `Package` | `Object` | `Package.bt` | Package management |
| `Pid` | `Object` | `Pid.bt` | BEAM process identifier |
| `Port` | `Object` | `Port.bt` | BEAM port wrapper |
| `Printable` | protocol | `Printable.bt` | String representation protocol |
| `Protocol` | `Object` | `Protocol.bt` | Protocol mirror |
| `Queue` | `Collection` | `Queue.bt` | FIFO queue |
| `Random` | `Object` | `Random.bt` | Random number generation |
| `ReactiveSubprocess` | `Actor` | `ReactiveSubprocess.bt` | Streaming subprocess |
| `Reference` | `Object` | `Reference.bt` | BEAM reference wrapper |
| `Regex` | `Value` | `Regex.bt` | Regular expressions |
| `Result` | `Value` | `Result.bt` | Ok/Error result type |
| `Server` | `Actor` | `Server.bt` | OTP Server base class |
| `StackFrame` | `Object` | `StackFrame.bt` | Stack trace inspection |
| `Stream` | `Object` | `Stream.bt` | Lazy sequences |
| `Subprocess` | `Actor` | `Subprocess.bt` | OS subprocess management |
| `SupervisionSpec` | `Value` | `SupervisionSpec.bt` | Supervisor child specs |
| `Supervisor` | `Object` | `Supervisor.bt` | OTP Supervisor wrapper |
| `System` | `Object` | `System.bt` | System info and control |
| `TestResult` | `Value` | `TestResult.bt` | BUnit test result |
| `TestRunner` | `Object` | `TestRunner.bt` | BUnit test runner |
| `ThrowError` | `Error` | `ThrowError.bt` | Non-local return error |
| `Time` | `Object` | `Time.bt` | Time operations |
| `TimeoutProxy` | `Object` | `TimeoutProxy.bt` | Timeout wrapper |
| `Timer` | `Object` | `Timer.bt` | Timer operations |
| `Tracing` | `Object` | `Tracing.bt` | Actor observability |
| `WorkspaceInterface` | `Actor` | `WorkspaceInterface.bt` | Workspace management |

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
| `remove:` | ❌ Not defined | Medium |
| `asSet` | ✅ Via `(Set new) fromList: aList` | Low |
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

All stdlib classes now have corresponding `stdlib/src/*.bt` definitions. `Collection` is now defined in `stdlib/src/Collection.bt` as an abstract typed subclass of `Value`.

---

## Test Coverage Gaps

Test coverage is now spread across both `stdlib/bootstrap-test/` (1046 assertions) and `tests/repl-protocol/cases/` (213 assertions).
Many previously untested methods now have stdlib test coverage. The following gaps remain for methods
with no coverage in either test suite:

### High Priority (Core functionality untested)

| Class | Untested Methods |
|-------|-----------------|
| **Integer** | `**`, `min:`, `max:`, `timesRepeat:`, `to:do:`, `to:by:do:`, `describe` |
| **Float** | `/=`, `<=`, `>=`, `printString`, `describe` |
| **String** | `,`, `lines`, `asAtom`, `describe`, `printString` |
| **List** | `detect:ifNone:`, `describe`, `printString` |
| **Block** | `repeat`, `describe` |

### Medium Priority

| Class | Untested Methods |
|-------|-----------------|
| **Object** | `inspect`, `describe` |
| **Actor** | `spawnWith:`, `new` (error case), `new:` (error case), `describe` |
| **UndefinedObject** | `ifNotNil:`, `ifNil:ifNotNil:`, `ifNotNil:ifNil:`, `copy`, `deepCopy`, `shallowCopy`, `describe`, `printString` |
| **True/False** | `isTrue`, `isFalse`, `describe`, `printString` |
| **Dictionary** | `describe` |
| **Set** | `describe` |
| **Association** | `printString`, `describe` |
| **Exception** | `describe`, `signal`, `signal:` |
| **Error** | `describe` |
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
