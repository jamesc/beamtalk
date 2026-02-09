# Stdlib Implementation Status

> **Last updated:** 2026-02-09
> **Issue:** BT-247
> **Methodology:** Audit of `lib/*.bt` files, compiler intrinsics (`intrinsics.rs`, `primitive_bindings.rs`),
> runtime dispatch modules (`beamtalk_*.erl`), and E2E test coverage (`tests/e2e/cases/*.bt`).

## Executive Summary

| Metric | Value |
|--------|-------|
| **Total stdlib methods** | 185 |
| **✅ Implemented** | 183 (98.9%) |
| **❌ Not Implemented** | 2 (1.1%) |
| **E2E test coverage** | 67 methods (36.2%) |
| **Stdlib .bt files** | 12 |
| **Runtime-only classes** | 3 (Dictionary, Tuple, CompiledMethod) |
| **Missing .bt files** | 6 (ProtoObject, Collection, SequenceableCollection, Set, Dictionary, List) |

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
| **Runtime only** | Erlang module with no `.bt` definition | `Dictionary >> keys`, `Tuple >> unwrap` |

---

## Tier 1: Core Classes

### Object (`lib/Object.bt`)

**Class:** `Object` — superclass: `ProtoObject`
**Methods:** 20/20 implemented (100%)

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
| `subclassResponsibility` | intrinsic | ✅ | 🧪 | Raises `beamtalk_error` |

### Integer (`lib/Integer.bt`)

**Class:** `Integer` — superclass: `Object` — `@sealed`
**Methods:** 28/28 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `+` | @primitive selector | ✅ | 🧪 | `Integer>>+` |
| `-` | @primitive selector | ✅ | 🧪 | `Integer>>-` |
| `*` | @primitive selector | ✅ | 🧪 | `Integer>>*` |
| `/` | @primitive selector | ✅ | 🧪 | `Integer>>/` |
| `%` | @primitive selector | ✅ | | `Integer>>\\` |
| `**` | @primitive selector | ✅ | | `Integer>>raisedTo:` |
| `=` | @primitive selector | ✅ | 🧪 | `Integer>>=` |
| `~=` | pure BT | ✅ | 🧪 | `Integer>>~=` |
| `<` | @primitive selector | ✅ | | `Integer>><` |
| `>` | @primitive selector | ✅ | | `Integer>>>` |
| `<=` | @primitive selector | ✅ | | `Integer>><=` |
| `>=` | @primitive selector | ✅ | | `Integer>>>=` |
| `negated` | pure BT | ✅ | 🧪 | `Integer>>negated` |
| `abs` | pure BT | ✅ | 🧪 | `Integer>>abs` |
| `isZero` | pure BT | ✅ | 🧪 | `Integer>>isZero` |
| `isPositive` | pure BT | ✅ | | `Integer>>positive` |
| `isNegative` | pure BT | ✅ | | `Integer>>negative` |
| `isEven` | pure BT | ✅ | 🧪 | `Integer>>even` |
| `isOdd` | pure BT | ✅ | 🧪 | `Integer>>odd` |
| `min:` | pure BT | ✅ | | `Magnitude>>min:` |
| `max:` | pure BT | ✅ | | `Magnitude>>max:` |
| `timesRepeat:` | intrinsic | ✅ | | `Integer>>timesRepeat:` |
| `to:do:` | intrinsic | ✅ | | `Integer>>to:do:` |
| `to:by:do:` | intrinsic | ✅ | | `Integer>>to:by:do:` |
| `asFloat` | @primitive selector | ✅ | | `Integer>>asFloat` |
| `asString` | @primitive selector | ✅ | | `Integer>>asString` |
| `printString` | @primitive selector | ✅ | | `Integer>>printString` |
| `describe` | pure BT | ✅ | | N/A |

### String (`lib/String.bt`)

**Class:** `String` — superclass: `Object` — `@sealed`
**Methods:** 36/36 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `=` | @primitive selector | ✅ | 🧪 | `String>>=` |
| `~=` | pure BT | ✅ | 🧪 | `String>>~=` |
| `<` | @primitive selector | ✅ | | `String>><` |
| `>` | @primitive selector | ✅ | | `String>>>` |
| `<=` | @primitive selector | ✅ | | `String>><=` |
| `>=` | @primitive selector | ✅ | | `String>>>=` |
| `++` | @primitive selector | ✅ | 🧪 | `String>>,` |
| `,` | pure BT | ✅ | | `String>>,` |
| `length` | @primitive selector | ✅ | 🧪 | `String>>size` |
| `size` | pure BT | ✅ | | `String>>size` |
| `at:` | @primitive selector | ✅ | | `String>>at:` |
| `uppercase` | @primitive selector | ✅ | | `String>>asUppercase` |
| `lowercase` | @primitive selector | ✅ | | `String>>asLowercase` |
| `capitalize` | @primitive selector | ✅ | | `String>>capitalized` |
| `trim` | @primitive selector | ✅ | | `String>>trimBoth` |
| `trimLeft` | @primitive selector | ✅ | | `String>>trimLeft` |
| `trimRight` | @primitive selector | ✅ | | `String>>trimRight` |
| `reverse` | @primitive selector | ✅ | | `String>>reversed` |
| `includes:` | @primitive selector | ✅ | | `String>>includesSubstring:` |
| `startsWith:` | @primitive selector | ✅ | | `String>>beginsWith:` |
| `endsWith:` | @primitive selector | ✅ | | `String>>endsWith:` |
| `indexOf:` | @primitive selector | ✅ | | `String>>indexOfSubCollection:` |
| `split:` | @primitive selector | ✅ | | N/A |
| `splitOn:` | @primitive selector | ✅ | | N/A |
| `repeat:` | @primitive selector | ✅ | | N/A |
| `isEmpty` | pure BT | ✅ | 🧪 | `String>>isEmpty` |
| `isNotEmpty` | pure BT | ✅ | | `String>>isNotEmpty` |
| `asInteger` | @primitive selector | ✅ | | `String>>asInteger` |
| `asFloat` | @primitive selector | ✅ | | `String>>asFloat` |
| `asAtom` | @primitive selector | ✅ | | N/A (BEAM-specific) |
| `asList` | @primitive selector | ✅ | | `String>>asArray` |
| `each:` | @primitive selector | ✅ | | `String>>do:` |
| `collect:` | @primitive selector | ✅ | | `String>>collect:` |
| `select:` | @primitive selector | ✅ | | `String>>select:` |
| `describe` | pure BT | ✅ | | N/A |
| `printString` | pure BT | ✅ | | `String>>printString` |

### Array (`lib/Array.bt`)

**Class:** `Array` — superclass: `Object` — `@sealed`
**Methods:** 10/10 implemented (100%)
**Note:** Array in Beamtalk maps to Erlang lists (not tuples). Literal syntax: `#(1, 2, 3)`.

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `size` | @primitive selector | ✅ | 🧪 | `Array>>size` |
| `isEmpty` | @primitive selector | ✅ | 🧪 | `Array>>isEmpty` |
| `first` | @primitive selector | ✅ | 🧪 | `Array>>first` |
| `rest` | @primitive selector | ✅ | 🧪 | `Array>>allButFirst` |
| `do:` | @primitive selector | ✅ | | `Array>>do:` |
| `collect:` | @primitive selector | ✅ | | `Array>>collect:` |
| `select:` | @primitive selector | ✅ | | `Array>>select:` |
| `reject:` | @primitive selector | ✅ | | `Array>>reject:` |
| `inject:into:` | @primitive selector | ✅ | | `Array>>inject:into:` |
| `describe` | pure BT | ✅ | | N/A |

### Block (`lib/Block.bt`)

**Class:** `Block` — superclass: `Object` — `@sealed`
**Methods:** 9/11 implemented (81.8%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `value` | intrinsic `blockValue` | ✅ | 🧪 | `BlockClosure>>value` |
| `value:` | intrinsic `blockValue1` | ✅ | 🧪 | `BlockClosure>>value:` |
| `value:value:` | intrinsic `blockValue2` | ✅ | 🧪 | `BlockClosure>>value:value:` |
| `value:value:value:` | intrinsic `blockValue3` | ✅ | 🧪 | `BlockClosure>>value:value:value:` |
| `whileTrue:` | intrinsic `whileTrue` | ✅ | 🧪 | `BlockClosure>>whileTrue:` |
| `whileFalse:` | intrinsic `whileFalse` | ✅ | | `BlockClosure>>whileFalse:` |
| `repeat` | intrinsic `repeat` | ✅ | | `BlockClosure>>repeat` |
| `on:` | @primitive selector | **❌** | | `BlockClosure>>on:do:` |
| `ensure:` | @primitive selector | **❌** | | `BlockClosure>>ensure:` |
| `arity` | @primitive selector | ✅ | | `BlockClosure>>argumentCount` |
| `describe` | pure BT | ✅ | | N/A |

> **⚠️ `on:` and `ensure:` are declared with `@primitive` in Block.bt but have no runtime handler.**
> These are critical for exception handling (`try/catch` equivalent) and resource cleanup (`try/finally` equivalent).
> **Tracked in:** BT-338 (Implement Error/Exception class hierarchy)

### True (`lib/True.bt`) & False (`lib/False.bt`)

**Class:** `True` / `False` — superclass: `Object` — `@sealed`
**Methods:** 10/10 implemented each (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `ifTrue:ifFalse:` | pure BT | ✅ | 🧪 | `Boolean>>ifTrue:ifFalse:` |
| `ifTrue:` | pure BT | ✅ | 🧪 | `Boolean>>ifTrue:` |
| `ifFalse:` | pure BT | ✅ | 🧪 | `Boolean>>ifFalse:` |
| `and:` | pure BT | ✅ | 🧪 | `Boolean>>and:` |
| `or:` | pure BT | ✅ | 🧪 | `Boolean>>or:` |
| `not` | pure BT | ✅ | 🧪 | `Boolean>>not` |
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

**Class:** `Float` — superclass: `Object` — `@sealed`
**Methods:** 17/17 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `+` | @primitive selector | ✅ | | `Float>>+` |
| `-` | @primitive selector | ✅ | | `Float>>-` |
| `*` | @primitive selector | ✅ | | `Float>>*` |
| `/` | @primitive selector | ✅ | | `Float>>/` |
| `=` | @primitive selector | ✅ | | `Float>>=` |
| `~=` | pure BT | ✅ | | `Float>>~=` |
| `<` | @primitive selector | ✅ | | `Float>><` |
| `>` | @primitive selector | ✅ | | `Float>>>` |
| `<=` | @primitive selector | ✅ | | `Float>><=` |
| `>=` | @primitive selector | ✅ | | `Float>>>=` |
| `negated` | pure BT | ✅ | | `Float>>negated` |
| `abs` | pure BT | ✅ | | `Float>>abs` |
| `min:` | pure BT | ✅ | | `Magnitude>>min:` |
| `max:` | pure BT | ✅ | | `Magnitude>>max:` |
| `asString` | @primitive selector | ✅ | | `Float>>asString` |
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

### Beamtalk / SystemDictionary (`lib/Beamtalk.bt`)

**Class:** `Beamtalk` — superclass: `Object`
**Methods:** 3/3 implemented (100%)

| Selector | Mechanism | Status | E2E | Pharo Equivalent |
|----------|-----------|--------|-----|------------------|
| `allClasses` | @primitive selector | ✅ | 🧪 | `Smalltalk>>allClasses` |
| `classNamed:` | @primitive selector | ✅ | 🧪 | `Smalltalk>>at:` |
| `globals` | @primitive selector | ✅ | | `Smalltalk>>globals` |

---

## Tier 3: Runtime-Only Classes (No `.bt` File)

These classes are implemented entirely in Erlang runtime modules with no corresponding `lib/*.bt` definition.

### Dictionary (`beamtalk_map.erl`)

**Runtime module:** `beamtalk_map.erl`
**Methods:** 10 — all implemented

| Selector | Status | Notes | Pharo Equivalent |
|----------|--------|-------|------------------|
| `keys` | ✅ | `maps:keys` | `Dictionary>>keys` |
| `values` | ✅ | `maps:values` | `Dictionary>>values` |
| `size` | ✅ | `maps:size` | `Dictionary>>size` |
| `at:` | ✅ | `maps:get` | `Dictionary>>at:` |
| `at:ifAbsent:` | ✅ | Fallback block | `Dictionary>>at:ifAbsent:` |
| `at:put:` | ✅ | `maps:put` | `Dictionary>>at:put:` |
| `includesKey:` | ✅ | `maps:is_key` | `Dictionary>>includesKey:` |
| `removeKey:` | ✅ | `maps:remove` | `Dictionary>>removeKey:` |
| `merge:` | ✅ | `maps:merge` | `Dictionary>>merge:` |
| `keysAndValuesDo:` | ✅ | Iteration | `Dictionary>>keysAndValuesDo:` |

### Tuple (`beamtalk_tuple.erl`)

**Runtime module:** `beamtalk_tuple.erl`
**Methods:** 8 — all implemented
**Note:** BEAM-specific, wraps Erlang result tuples `{ok, Value}` / `{error, Reason}`.

| Selector | Status | Notes |
|----------|--------|-------|
| `size` | ✅ | `tuple_size` |
| `at:` | ✅ | 1-based index via `element` |
| `isOk` | ✅ | `{ok, _}` pattern match |
| `isError` | ✅ | `{error, _}` pattern match |
| `unwrap` | ✅ | Extract value or raise |
| `unwrapOr:` | ✅ | Extract or return default |
| `unwrapOrElse:` | ✅ | Extract or evaluate block |
| `asString` | ✅ | String representation |

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
> - **BT-338**: Block `on:do:` / `ensure:` (exception handling)
> - **BT-334**: Float methods (`rounded`, `ceiling`, `floor`, `isNaN`) and Number hierarchy
> - **BT-44**: Missing collection methods (`sort`, `detect:`, `take:`, `flatMap:`, etc.)
> - **BT-331**: Compilable stdlib collection classes (Dictionary, List, Set)
> - **BT-408**: E2E test coverage for untested stdlib methods

Methods that Pharo users would expect but Beamtalk does **not** define or implement:

### ProtoObject (No `.bt` file exists)

| Pharo Method | Beamtalk Status | Priority |
|-------------|-----------------|----------|
| `==` (identity) | ✅ Intrinsic (handled by codegen) | — |
| `~~` (not identical) | ❌ Not defined | Low |
| `doesNotUnderstand:` | ✅ Runtime (beamtalk_error) | — |
| `identityHash` | ✅ `hash` intrinsic | — |

### Object

| Pharo Method | Beamtalk Status | Priority |
|-------------|-----------------|----------|
| `copy` | ❌ Not defined (except UndefinedObject) | Medium |
| `deepCopy` | ❌ Not defined (except UndefinedObject) | Low |
| `error:` | ✅ Intrinsic | — |
| `halt` | ❌ Not defined | Low |
| `assert:` | ❌ Not defined | Medium |
| `deny:` | ❌ Not defined | Low |

### Integer

| Pharo Method | Beamtalk Equivalent | Priority |
|-------------|---------------------|----------|
| `factorial` | ❌ Not defined | Low |
| `gcd:` | ❌ Not defined | Low |
| `lcm:` | ❌ Not defined | Low |
| `isPrime` | ❌ Not defined | Low |
| `bitAnd:` | ❌ Not defined | Medium |
| `bitOr:` | ❌ Not defined | Medium |
| `bitShift:` | ❌ Not defined | Medium |

### Float

| Pharo Method | Beamtalk Equivalent | Priority |
|-------------|---------------------|----------|
| `isNaN` | ❌ Not defined | Medium |
| `isInfinite` | ❌ Not defined | Medium |
| `rounded` | ❌ Not defined | Medium |
| `ceiling` | ❌ Not defined | Medium |
| `floor` | ❌ Not defined | Medium |
| `truncated` | ❌ Not defined | Medium |
| `**` (exponentiation) | ❌ Not defined (Integer has it) | Medium |

### String

| Pharo Method | Beamtalk Equivalent | Priority |
|-------------|---------------------|----------|
| `replaceAll:with:` | ❌ Not defined (runtime has `replace:with:`) | Medium |
| `copyFrom:to:` | ❌ Not defined (runtime has `substring:to:`) | Low |
| `asSymbol` | ❌ Not defined | Low |
| `match:` | ❌ Not defined | Low |

### Array / Collection

| Pharo Method | Beamtalk Equivalent | Priority |
|-------------|---------------------|----------|
| `add:` | ❌ Not defined | High |
| `remove:` | ❌ Not defined | Medium |
| `sort` | ❌ Not defined | High |
| `sort:` | ❌ Not defined | High |
| `reversed` | ❌ Not defined | Medium |
| `asSet` | ❌ Not defined | Low |
| `asDictionary` | ❌ Not defined | Low |
| `detect:` | ❌ Not defined | High |
| `detect:ifNone:` | ❌ Not defined | High |
| `anySatisfy:` | ❌ Not defined | Medium |
| `allSatisfy:` | ❌ Not defined | Medium |
| `count:` | ❌ Not defined | Medium |
| `with:collect:` | ❌ Not defined | Low |
| `at:` | ❌ Not defined (lists are linked — access by index is O(n)) | Medium |
| `at:put:` | ❌ Not defined | Low |
| `last` | ❌ Not defined | Medium |
| `includes:` | ❌ Not defined | High |

### Block

| Pharo Method | Beamtalk Equivalent | Priority |
|-------------|---------------------|----------|
| `on:do:` | ❌ `on:` defined but not implemented | **Critical** |
| `ensure:` | ❌ Defined but not implemented | **Critical** |
| `valueWithArguments:` | ❌ Not defined | Medium |
| `cull:` | ❌ Not defined | Low |
| `newProcess` / `fork` | ❌ Not defined (use Actor >> spawn) | Low |

### Boolean

| Pharo Method | Beamtalk Equivalent | Priority |
|-------------|---------------------|----------|
| `xor:` | ❌ Not defined | Low |

---

## Missing `.bt` Files

These classes are either referenced in the original issue or have runtime support but no stdlib definition:

| Class | Status | Runtime Support | Priority |
|-------|--------|----------------|----------|
| `ProtoObject` | ❌ No `.bt` file | Intrinsics handle `class`, `==`, `~=` | Medium (BT-375) |
| `Collection` | ❌ No `.bt` file | N/A (abstract) | Low |
| `SequenceableCollection` | ❌ No `.bt` file | N/A (abstract) | Low |
| `Set` | ❌ No `.bt` file | No runtime support | Low |
| `Dictionary` | ❌ No `.bt` file | `beamtalk_map.erl` has full support | Medium |
| `List` | ❌ No `.bt` file | `beamtalk_list.erl` handles Array dispatch | Low |

---

## E2E Test Coverage Analysis

Methods with no E2E test coverage that should be tested:

### High Priority (Core functionality untested)

| Class | Untested Methods |
|-------|-----------------|
| **Integer** | `%`, `**`, `<`, `>`, `<=`, `>=`, `isPositive`, `isNegative`, `min:`, `max:`, `timesRepeat:`, `to:do:`, `to:by:do:`, `asFloat`, `asString`, `printString` |
| **Float** | ALL methods (0/17 E2E coverage) |
| **String** | `<`, `>`, `<=`, `>=`, `,`, `size`, `at:`, `uppercase`, `lowercase`, `capitalize`, `trim`, `trimLeft`, `trimRight`, `reverse`, `includes:`, `startsWith:`, `endsWith:`, `indexOf:`, `split:`, `splitOn:`, `repeat:`, `isNotEmpty`, `asInteger`, `asFloat`, `asAtom`, `asList`, `each:`, `collect:`, `select:` |
| **Array** | `do:`, `collect:`, `select:`, `reject:`, `inject:into:` |
| **Block** | `whileFalse:`, `repeat`, `arity` |
| **Dictionary** | ALL methods (0 E2E coverage) |
| **Tuple** | ALL methods (0 E2E coverage) |

### Medium Priority

| Class | Untested Methods |
|-------|-----------------|
| **Object** | `inspect`, `describe` |
| **Actor** | `spawnWith:`, `new` (error case), `new:` (error case) |
| **UndefinedObject** | `ifNotNil:`, `ifNil:ifNotNil:`, `ifNotNil:ifNil:`, `copy`, `deepCopy`, `shallowCopy` |
| **True/False** | `isTrue`, `isFalse`, `describe`, `printString` |

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
