# Stdlib Implementation Status

> **Last updated:** 2026-02-11
> **Issue:** BT-247
> **Methodology:** Audit of `lib/*.bt` files, compiler intrinsics (`intrinsics.rs`, `primitive_bindings.rs`),
> runtime dispatch modules (`beamtalk_*.erl`), and E2E test coverage (`tests/e2e/cases/*.bt`).

## Executive Summary

| Metric | Value |
|--------|-------|
| **Total stdlib methods** | 297 |
| **✅ Implemented** | 297 (100%) |
| **❌ Not Implemented** | 0 (0%) |
| **E2E test coverage** | 212 methods (71.4%) |
| **Stdlib .bt files** | 22 |
| **Runtime-only classes** | 1 (CompiledMethod) |
| **Missing .bt files** | 0 |

## Status Categories

| Symbol | Meaning |
|--------|---------|
| ✅ Implemented | Fully working — compiler intrinsic, runtime dispatch, or pure Beamtalk |
| ❌ Not Implemented | Defined in stdlib but no backing implementation |
| 🧪 E2E | Has end-to-end test coverage |

## Implementation Mechanisms

| Mechanism | Description | Example |
|-----------|-------------|---------|
| **Compiler intrinsic** | Inlined at call site by codegen (`intrinsics.rs`) | `Block >> value`, `Object >> class` |
| **@primitive selector** | Runtime dispatch via `beamtalk_*.erl` module | `Integer >> +`, `String >> length` |
| **Pure Beamtalk** | Compiled from `.bt` source (ADR 0007) | `True >> not`, `Integer >> isEven` |
| **Runtime only** | Erlang module with no `.bt` definition | `CompiledMethod >> selector` |

---

## Tier 1: Core Classes

### ProtoObject (`lib/ProtoObject.bt`)

**Class:** `ProtoObject` — superclass: `nil` (root class)
**Methods:** 5/5 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `==` | intrinsic | ✅ | 🧪 | Identity comparison |
| `~=` | intrinsic | ✅ | 🧪 | Not-equal comparison |
| `class` | intrinsic | ✅ | 🧪 | Type introspection |
| `doesNotUnderstand:args:` | intrinsic | ✅ | 🧪 | Fallback for unknown messages |
| `perform:withArguments:` | intrinsic | ✅ | 🧪 | Dynamic dispatch |

### Object (`lib/Object.bt`)

**Class:** `Object` — superclass: `ProtoObject`
**Methods:** 22/22 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `new` | intrinsic `basicNew` | ✅ | 🧪 | Inline codegen for value type instantiation |
| `new:` | intrinsic `basicNewWith` | ✅ | 🧪 | Instantiation with constructor args |
| `isNil` | intrinsic + pure BT | ✅ | 🧪 | Pattern match at call site; Object.bt returns `false` |
| `notNil` | intrinsic + pure BT | ✅ | 🧪 | Pattern match at call site; Object.bt returns `true` |
| `ifNil:` | intrinsic + pure BT | ✅ | 🧪 | Inline pattern match |
| `ifNotNil:` | intrinsic + pure BT | ✅ | 🧪 | Inline pattern match |
| `ifNil:ifNotNil:` | intrinsic + pure BT | ✅ | 🧪 | Inline pattern match |
| `ifNotNil:ifNil:` | intrinsic + pure BT | ✅ | 🧪 | Inline pattern match |
| `printString` | intrinsic | ✅ | 🧪 | `beamtalk_primitive:print_string/1` |
| `inspect` | pure BT | ✅ | | Calls `self describe` |
| `describe` | pure BT | ✅ | | Returns `'an Object'` |
| `yourself` | intrinsic + pure BT | ✅ | 🧪 | Returns self |
| `hash` | intrinsic | ✅ | 🧪 | `erlang:phash2/1` |
| `respondsTo:` | intrinsic | ✅ | 🧪 | `beamtalk_primitive:responds_to/2` |
| `instVarNames` | intrinsic | ✅ | 🧪 | Async for actors |
| `instVarAt:` | intrinsic | ✅ | 🧪 | Async for actors |
| `instVarAt:put:` | intrinsic | ✅ | 🧪 | Async for actors |
| `perform:` | intrinsic | ✅ | 🧪 | Dynamic dispatch |
| `perform:withArgs:` | intrinsic | ✅ | 🧪 | Dynamic dispatch with args |
| `->` | intrinsic | ✅ | 🧪 | Association creation (key-value pair) |
| `subclassResponsibility` | pure BT | ✅ | 🧪 | Calls `self error:` — pure Beamtalk method (BT-405) |
| `error:` | intrinsic | ✅ | 🧪 | Smalltalk-style error signaling |

### Number (`lib/Number.bt`)

**Class:** `Number` — superclass: `Object` — `abstract`
**Methods:** 5/5 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `isZero` | pure BT | ✅ | 🧪 | `Number>>isZero` |
| `isPositive` | pure BT | ✅ | 🧪 | `Number>>positive` |
| `isNegative` | pure BT | ✅ | 🧪 | `Number>>negative` |
| `sign` | pure BT | ✅ | 🧪 | `Number>>sign` |
| `between:and:` | pure BT | ✅ | 🧪 | `Magnitude>>between:and:` |

### Integer (`lib/Integer.bt`)

**Class:** `Integer` — superclass: `Number` — `@sealed`
**Methods:** 33/33 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `+` | @primitive selector | ✅ | 🧪 | `Integer>>+` |
| `-` | @primitive selector | ✅ | 🧪 | `Integer>>-` |
| `*` | @primitive selector | ✅ | 🧪 | `Integer>>*` |
| `/` | @primitive selector | ✅ | 🧪 | `Integer>>/` |
| `%` | @primitive selector | ✅ | 🧪 | `Integer>>\\` |
| `**` | @primitive selector | ✅ | | `Integer>>raisedTo:` |
| `=` | @primitive selector | ✅ | 🧪 | `Integer>>=` |
| `~=` | pure BT | ✅ | 🧪 | `Integer>>~=` |
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

### String (`lib/String.bt`)

**Class:** `String` — superclass: `Object` — `@sealed`
**Methods:** 48/48 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `=` | @primitive selector | ✅ | 🧪 | `String>>=` |
| `~=` | pure BT | ✅ | 🧪 | `String>>~=` |
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

### List (`lib/List.bt`)

**Class:** `List` — superclass: `Object` — `@sealed`
**Methods:** 37/37 implemented (100%)
**Note:** List in Beamtalk maps to Erlang linked lists. Literal syntax: `#(1, 2, 3)`. Renamed from Array in BT-419 — `Array` is reserved for a future tuple-backed O(1)-indexed collection.
**Migration:** BT-419 — migrated from hand-written `beamtalk_list.erl` (Option B) to compiled `lib/List.bt` with BIF mappings (Option A). Complex operations delegate to `beamtalk_list_ops.erl`.

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
| `inject:into:` | @primitive BIF (`lists:foldl`) | ✅ | 🧪 | `Collection>>inject:into:` |
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

### Block (`lib/Block.bt`)

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

### True (`lib/True.bt`) & False (`lib/False.bt`)

**Class:** `True` / `False` — superclass: `Object` — `@sealed`
**Methods:** 11/11 implemented each (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `ifTrue:ifFalse:` | pure BT | ✅ | 🧪 | `Boolean>>ifTrue:ifFalse:` |
| `ifTrue:` | pure BT | ✅ | 🧪 | `Boolean>>ifTrue:` |
| `ifFalse:` | pure BT | ✅ | 🧪 | `Boolean>>ifFalse:` |
| `and:` | pure BT | ✅ | 🧪 | `Boolean>>and:` |
| `or:` | pure BT | ✅ | 🧪 | `Boolean>>or:` |
| `not` | pure BT | ✅ | 🧪 | `Boolean>>not` |
| `xor:` | pure BT | ✅ | 🧪 | `Boolean>>xor:` |
| `isTrue` | pure BT | ✅ | | N/A |
| `isFalse` | pure BT | ✅ | | N/A |
| `describe` | pure BT | ✅ | | N/A |
| `printString` | pure BT | ✅ | | `Boolean>>printString` |

### UndefinedObject (`lib/UndefinedObject.bt`)

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

### Float (`lib/Float.bt`)

**Class:** `Float` — superclass: `Number` — `@sealed`
**Methods:** 25/25 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `+` | @primitive selector | ✅ | 🧪 | `Float>>+` |
| `-` | @primitive selector | ✅ | 🧪 | `Float>>-` |
| `*` | @primitive selector | ✅ | 🧪 | `Float>>*` |
| `/` | @primitive selector | ✅ | 🧪 | `Float>>/` |
| `=` | @primitive selector | ✅ | 🧪 | `Float>>=` |
| `~=` | pure BT | ✅ | | `Float>>~=` |
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

### Actor (`lib/Actor.bt`)

**Class:** `Actor` — superclass: `Object` — `@sealed`
**Methods:** 5/5 implemented (100%)

| Selector | Mechanism | Status | E2E | Notes |
|----------|-----------|--------|-----|-------|
| `spawn` | intrinsic `actorSpawn` | ✅ | 🧪 | `gen_server:start_link` |
| `spawnWith:` | intrinsic `actorSpawnWith` | ✅ | | With constructor args |
| `new` | pure BT | ✅ | | Error: "Use spawn instead" |
| `new:` | pure BT | ✅ | | Error: "Use spawnWith: instead" |
| `describe` | pure BT | ✅ | | Returns string literal |

### File (`lib/File.bt`)

**Class:** `File` — superclass: `Object`
**Methods:** 3/3 implemented (100%) — all class-level methods

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `exists:` | @primitive selector | ✅ | 🧪 | `FileReference>>exists` |
| `readAll:` | @primitive selector | ✅ | 🧪 | `FileReference>>contents` |
| `writeAll:contents:` | @primitive selector | ✅ | 🧪 | `FileReference>>writeStream` |

### Beamtalk / SystemDictionary (`lib/SystemDictionary.bt`)

**Class:** `SystemDictionary` — superclass: `Actor`
**Methods:** 4/4 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `allClasses` | @primitive selector | ✅ | 🧪 | `Smalltalk>>allClasses` |
| `classNamed:` | @primitive selector | ✅ | 🧪 | `Smalltalk>>at:` |
| `globals` | @primitive selector | ✅ | | `Smalltalk>>globals` |
| `version` | @primitive selector | ✅ | 🧪 | N/A |

### Dictionary (`lib/Dictionary.bt` — BT-418)

**Stdlib module:** `lib/Dictionary.bt` → `beamtalk_dictionary`
**Helper module:** `beamtalk_map_ops.erl` (complex operations)
**Methods:** 11 — all implemented

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
| `keysAndValuesDo:` | @primitive selector | ✅ | 🧪 | `Dictionary>>keysAndValuesDo:` |
| `describe` | pure BT | ✅ | | `Dictionary>>printString` |

### Set (`lib/Set.bt` — BT-73)

**Stdlib module:** `lib/Set.bt` → `beamtalk_set`
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
| `describe` | ✅ | Returns `'a Set'` | `Set>>printString` |

**E2E coverage:** 13 of 14 methods tested in `tests/e2e/cases/set.bt` (47 assertions). Only `describe` lacks E2E coverage.

### Tuple (`lib/Tuple.bt`)

**Class:** `Tuple` — superclass: `Object` — `@sealed`
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

### Symbol (`lib/Symbol.bt`)

**Class:** `Symbol` — superclass: `Object` — `@sealed`
**Methods:** 7/7 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `asString` | @primitive selector | ✅ | 🧪 | `Symbol>>asString` |
| `asAtom` | @primitive selector | ✅ | 🧪 | N/A (BEAM-specific) |
| `printString` | @primitive selector | ✅ | 🧪 | `Symbol>>printString` |
| `describe` | @primitive selector | ✅ | 🧪 | N/A |
| `=` | @primitive selector | ✅ | 🧪 | `Symbol>>=` |
| `~=` | @primitive selector | ✅ | 🧪 | `Symbol>>~=` |
| `hash` | @primitive selector | ✅ | 🧪 | `Symbol>>hash` |

### Association (`lib/Association.bt`)

**Class:** `Association` — superclass: `Object` — `@sealed`
**Methods:** 5/5 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `key` | @primitive selector | ✅ | 🧪 | `Association>>key` |
| `value` | @primitive selector | ✅ | 🧪 | `Association>>value` |
| `asString` | @primitive selector | ✅ | 🧪 | `Association>>printString` |
| `printString` | pure BT | ✅ | | `Association>>printString` |
| `describe` | pure BT | ✅ | | N/A |

### Exception (`lib/Exception.bt`)

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

### Error (`lib/Error.bt`)

**Class:** `Error` — superclass: `Exception`
**Methods:** 1/1 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `describe` | pure BT | ✅ | | N/A |

### TranscriptStream (`lib/TranscriptStream.bt`)

**Class:** `TranscriptStream` — superclass: `Actor`
**Methods:** 6/6 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `show:` | @primitive selector | ✅ | 🧪 | `Transcript>>show:` |
| `cr` | @primitive selector | ✅ | 🧪 | `Transcript>>cr` |
| `subscribe` | @primitive selector | ✅ | | N/A |
| `unsubscribe` | @primitive selector | ✅ | | N/A |
| `recent` | @primitive selector | ✅ | | N/A |
| `clear` | @primitive selector | ✅ | | N/A |

---

## Tier 3: Runtime-Only Classes (No `.bt` File)

These classes are implemented entirely in Erlang runtime modules with no corresponding `lib/*.bt` definition.

### CompiledMethod (`beamtalk_compiled_method.erl`)

**Runtime module:** `beamtalk_compiled_method.erl`
**Methods:** 3 — all implemented

| Selector | Status | E2E | Pharo Equivalent |
|----------|--------|-----|------------------|
| `selector` | ✅ | 🧪 | `CompiledMethod>>selector` |
| `source` | ✅ | 🧪 | `CompiledMethod>>sourceCode` |
| `argumentCount` | ✅ | 🧪 | `CompiledMethod>>numArgs` |

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

All stdlib classes now have corresponding `lib/*.bt` definitions. The only runtime-only class is `CompiledMethod`,
which is implemented entirely in `beamtalk_compiled_method.erl`.

| Class | Status | Notes |
|-------|--------|-------|
| `Collection` | N/A | Abstract concept — not planned as a standalone `.bt` file |
| `SequenceableCollection` | N/A | Abstract concept — not planned as a standalone `.bt` file |

---

## E2E Test Coverage Analysis

Methods with no E2E test coverage that should be tested:

### High Priority (Core functionality untested)

| Class | Untested Methods |
|-------|-----------------|
| **Integer** | `**`, `min:`, `max:`, `timesRepeat:`, `to:do:`, `to:by:do:`, `describe` |
| **Float** | `~=`, `<=`, `>=`, `printString`, `describe` |
| **String** | `,`, `lines`, `asAtom`, `describe`, `printString` |
| **List** | `detect:ifNone:`, `describe`, `printString` |
| **Block** | `repeat`, `describe` |
| **Tuple** | ALL methods (0 E2E coverage — no tuple literal syntax yet) |

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
| **SystemDictionary** | `globals` |

---

## Testing Methodology

For each method, testing was performed in this priority order:

1. **E2E test files** (`tests/e2e/cases/*.bt`) — checked for explicit `// =>` assertions exercising the method
2. **Compiler intrinsics** (`crates/beamtalk-core/src/codegen/core_erlang/intrinsics.rs`) — verified codegen handler exists
3. **Primitive bindings** (`crates/beamtalk-core/src/codegen/core_erlang/primitive_bindings.rs`, `primitive_implementations.rs`) — verified selector-based dispatch codegen
4. **Runtime dispatch** (`runtime/apps/beamtalk_runtime/src/beamtalk_*.erl`) — verified dispatch clause handles the selector
5. **Pure Beamtalk** (`lib/*.bt`) — verified method body compiles (not just a comment)

A method is marked ✅ if at least one implementation path exists (intrinsic, runtime dispatch, or compiled Beamtalk).
A method is marked 🧪 if an E2E test file exercises it with a `// =>` assertion.
