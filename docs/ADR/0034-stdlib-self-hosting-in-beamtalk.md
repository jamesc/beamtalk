# ADR 0034: Stdlib Self-Hosting — Moving Logic from Erlang to Beamtalk

## Status
Proposed (2026-02-22)

## Context

ADR 0007 established the compilable stdlib with `@primitive`/`@intrinsic` injection, enabling Beamtalk source files (`.bt`) to declare the user-facing API while delegating to Erlang runtime modules for implementation. This was the right bootstrap strategy: get the system working first, then progressively self-host.

Today the stdlib has **67 `.bt` files** (~5,140 lines) backed by **57 Erlang modules** (~12,700 lines) in the runtime. The split is:

| Category | Example | Lines |
|----------|---------|-------|
| Already pure Beamtalk | `True.bt`, `Boolean.bt`, `Number.bt`, error subclasses | ~430 |
| Thin `@primitive` stubs over Erlang | `Collection.bt`, `Dictionary.bt`, `TestCase.bt` | ~4,700 |
| Erlang-only (no `.bt` file at all) | `Future`, `FileHandle` | ~500 |
| Runtime infrastructure (must stay Erlang) | dispatch, actor, bootstrap, class registry | ~8,000 |

Three problems motivate this ADR:

**1. Algorithmic logic trapped in Erlang.** Collection operations like `collect:`, `select:`, `reject:`, `detect:`, `inject:into:` in `beamtalk_collection_ops.erl` (108 lines), list operations like `zip:`, `groupBy:`, `partition:` in `beamtalk_list_ops.erl` (~150 lines of algorithmic code), and tuple unwrapping in `beamtalk_tuple_ops.erl` are pure functional logic with no dependency on Erlang BIFs. They could be expressed as Beamtalk methods, following the Smalltalk tradition where abstract collection operations are defined in terms of `do:`.

**2. Missing `.bt` files for user-visible classes.** `Future` (309 lines, returned by every async message send) and `FileHandle` (created by `File open:do:`) have no Beamtalk source. Users interact with these objects but they are invisible to the compiler, type checker, LSP, and documentation system.

**3. Test assertions are opaque.** `assert:`, `deny:`, `assert:equals:` in `beamtalk_test_case.erl` are pure logic (check a condition, raise an error) but live entirely in Erlang. Users writing tests cannot see or extend the assertion logic.

### Constraints

- **Bootstrap ordering** (ADR 0006): ProtoObject → Object → Behaviour → Class → Actor must register before user modules load. Pure-BT methods on these classes can only use features available after bootstrap.
- **Performance**: Collection operations are hot-path. Pure-BT versions dispatch through message sends; Erlang versions call BIFs directly. Non-local returns (`^`) from inside `do:` blocks compile to throw/catch on BEAM, adding allocation cost vs Erlang's tail-recursive early termination. These tradeoffs must be measured before committing.
- **No O(1) list cons today**: Beamtalk's `add:` (append) is O(n) (`erlang:'++'`). Without an O(1) prepend, accumulator-based collection building would be O(n²). This ADR introduces `addFirst:` as a new O(1) primitive to solve this.
- **Erlang interop**: Some operations (`sort`, `reverse`, `++`, string grapheme ops) fundamentally need Erlang BIF access and should remain as `@primitive`.
- **Process dictionary**: `beamtalk_collection_ops:to_list/1` uses the process dictionary for accumulation — a pattern unavailable in pure Beamtalk. Operations that depend on `to_list` must use the `addFirst:` + `reversed` pattern instead.

## Decision

We adopt a **phased self-hosting strategy** that progressively moves stdlib logic from Erlang to Beamtalk, following the Smalltalk/Pharo pattern of building higher-order operations on a minimal primitive surface. **Each phase is gated on benchmarks** confirming acceptable performance before proceeding.

### Phase 1: Add Missing `.bt` Files (Future, FileHandle)

Add `Future.bt` and `FileHandle.bt` as `@primitive`-stub classes, making them visible to the compiler, type checker, LSP, and documentation.

```beamtalk
/// A future represents an asynchronous computation result.
///
/// Every message send to an actor returns a Future. Use `await` to
/// block until the result is available, or `whenResolved:` to register
/// a callback.
///
/// ## Examples
///
/// ```beamtalk
/// future := counter increment.
/// future await.
/// // => 1
/// ```
sealed Object subclass: Future
  /// Block until resolved (30s default timeout).
  await => @primitive "await"

  /// Block until resolved or timeout (in milliseconds).
  await: timeout: Integer => @primitive "await:"

  /// Block until resolved with no timeout.
  awaitForever => @primitive "awaitForever"

  /// Register a callback for when the future resolves.
  whenResolved: block: Block => @primitive "whenResolved:"

  /// Register a callback for when the future is rejected.
  whenRejected: block: Block => @primitive "whenRejected:"

  printString -> String => @primitive "printString"
```

> **Note:** This is the minimal `@primitive` stub for the *existing* Future API. The full
> Future class design — combinators (`then:`, `all:`, `race:`), auto-await semantics, and
> Duration integration — is a separate ADR scoped by BT-507.

```beamtalk
/// A handle to an open file, available within a `File open:do:` block.
///
/// ## Examples
///
/// ```beamtalk
/// File open: 'example.txt' do: [:handle |
///   handle lines.
///   // => #("line 1", "line 2")
/// ].
/// ```
sealed Object subclass: FileHandle
  /// Return all lines as a List of Strings.
  lines -> List => @primitive "lines"

  printString -> String => @primitive "printString"
```

### Phase 2: Add `addFirst:` and Self-Host Collection Protocol

#### Prerequisite: `addFirst:` — O(1) List Cons

Before self-hosting collection operations, add `addFirst:` to `List.bt` as a new `@primitive` that compiles to O(1) list cons:

```beamtalk
// List.bt — new method
/// Prepend item to the front of the list. O(1).
///
/// ## Examples
///
/// ```beamtalk
/// #(2, 3) addFirst: 1.
/// // => #(1, 2, 3)
/// ```
addFirst: item -> List => @primitive "addFirst:"
```

This compiles to Core Erlang `[Item|Self]` — a single cons cell allocation. Combined with `reversed` (which compiles to `lists:reverse/1`), this enables the classic functional accumulator pattern: prepend during iteration, reverse at the end. This is exactly how Gleam implements `map`/`filter` in pure Gleam, and how Pharo's `OrderedCollection` accumulates results.

The codegen addition in `primitives/list.rs` is trivial:
```rust
"addFirst:" => {
    let p0 = params.first().map_or("_Item", String::as_str);
    Some(docvec!["[", p0.to_string(), "|Self]"])
}
```

#### Self-host Collection protocol

Move the abstract `Collection` operations from `beamtalk_collection_ops.erl` into pure Beamtalk on `Collection.bt`. With `addFirst:` available, `do:` is the true primitive boundary — all higher-order operations compose on it, exactly as in Pharo.

**Primitive surface** — two methods per concrete collection:

| Method | Stays primitive | Reason |
|--------|----------------|--------|
| `do:` | Yes — `subclassResponsibility`, overridden per concrete class | Each concrete collection implements iteration via its backing BIF (`lists:foreach`, `maps:foreach`, etc.) |
| `size` | Yes — `subclassResponsibility`, overridden per concrete class | Backed by `erlang:length`, `maps:size`, etc. |

`inject:into:` becomes pure Beamtalk on Collection, built on `do:`:

```beamtalk
/// Fold collection from left with an accumulator.
inject: initial into: block: Block =>
  acc := initial.
  self do: [:each | acc := block value: acc value: each].
  acc
```

Everything above `do:` and `size` becomes pure Beamtalk.

**Before** (current — every method delegates to Erlang):
```beamtalk
// Collection.bt today
collect: block: Block => @primitive "collect:"
select: block: Block => @primitive "select:"
reject: block: Block => @primitive "reject:"
inject: initial into: block: Block => @primitive "inject:into:"
```

**After** (pure Beamtalk on the abstract class):
```beamtalk
// Collection.bt — self-hosted
// Accumulator-based operations (addFirst: + reversed for O(n) total)

/// Fold collection from left with an accumulator.
inject: initial into: block: Block =>
  acc := initial.
  self do: [:each | acc := block value: acc value: each].
  acc

/// Return a new list with block applied to each element.
collect: block: Block -> List =>
  (self inject: #() into: [:acc :each | acc addFirst: (block value: each)]) reversed

/// Return elements for which block returns true.
select: block: Block -> List =>
  (self inject: #() into: [:acc :each |
    (block value: each) ifTrue: [acc addFirst: each] ifFalse: [acc]
  ]) reversed

/// Return elements for which block returns false.
reject: block: Block -> List =>
  self select: [:each | (block value: each) not]

// Early-termination operations (do: with non-local return)

/// Return the first element for which block returns true, or nil.
detect: block: Block =>
  self detect: block ifNone: [nil]

/// Return the first element for which block returns true, or evaluate
/// noneBlock if no element matches.
detect: block: Block ifNone: noneBlock: Block =>
  self do: [:each | (block value: each) ifTrue: [^ each]].
  noneBlock value

/// Return true if block returns true for any element.
anySatisfy: block: Block -> Boolean =>
  self do: [:each | (block value: each) ifTrue: [^ true]].
  false

/// Return true if block returns true for all elements.
allSatisfy: block: Block -> Boolean =>
  self do: [:each | (block value: each) ifFalse: [^ false]].
  true

/// Return true if collection includes anObject.
includes: anObject -> Boolean =>
  self anySatisfy: [:each | each =:= anObject]
```

**Performance characteristics:**
- `collect:`, `select:`, `reject:`: O(n) total — each `addFirst:` is O(1) cons, final `reversed` is O(n). This matches Gleam's implementation and is equivalent to the Erlang BIF-backed versions.
- `inject:into:`: O(n) — single pass via `do:`.
- `detect:`, `anySatisfy:`, `allSatisfy:`, `includes:`: Use `^` (non-local return) inside `do:` blocks for early termination. On BEAM, non-local returns compile to throw/catch, which has measurable allocation cost compared to Erlang's tail-recursive early termination. If benchmarks show unacceptable overhead, these four methods should remain `@primitive` while the accumulator-based operations still move to pure BT.

**Concrete collections keep their optimized overrides.** `List.bt`, `Set.bt`, and `Dictionary.bt` all retain their `@primitive` overrides for `collect:`, `select:`, `reject:`, `detect:`, `inject:into:`, `includes:`, `anySatisfy:`, `allSatisfy:` — these are backed by BIF fast-paths (`lists:map`, `lists:filter`, `lists:foldl`, `lists:any`, etc.). The pure-BT versions on `Collection` serve as the **default for user-defined collections** that subclass `Collection` without overriding these methods.

### Phase 3: Self-Host Algorithmic List/Tuple/Dictionary Operations

Move pure-logic operations from Erlang `*_ops.erl` modules into Beamtalk:

**List.bt** — algorithmic methods become pure BT:
```beamtalk
/// Return the index of the first occurrence of anObject, or -1.
indexOf: anObject -> Integer =>
  index := 0.
  self do: [:each |
    (each =:= anObject) ifTrue: [^ index].
    index := index + 1
  ].
  -1

/// Iterate with both element and index.
eachWithIndex: block: Block -> Nil =>
  index := 0.
  self do: [:each |
    block value: each value: index.
    index := index + 1
  ]
```

**Tuple.bt** — unwrap operations become pure BT:
```beamtalk
/// Unwrap an {ok, Value} tuple. Raises on {error, _} or non-ok/error tuples.
unwrap =>
  self isOk ifTrue: [self at: 2] ifFalse: [
    self isError
      ifTrue: [self error: "Called unwrap on error tuple: {self}"]
      ifFalse: [self error: "unwrap requires {ok, Value} or {error, Reason} tuple"]
  ]

/// Unwrap an {ok, Value} tuple, or return default.
unwrapOr: default =>
  self isOk ifTrue: [self at: 2] ifFalse: [default]

/// Unwrap an {ok, Value} tuple, or evaluate block with error reason.
unwrapOrElse: block: Block =>
  self isOk ifTrue: [self at: 2] ifFalse: [block value]
```

> **Note:** The Erlang `unwrap` has three clauses: `{ok, Value}`, `{error, Reason}`, and any other tuple. The BT version preserves all three cases using `isOk` and `isError` checks, raising distinct errors for error-tuples vs non-ok/error tuples; implementations MUST preserve the original error details (for example, include the `Reason` or rethrow the original error) when handling `{error, Reason}` so debugging fidelity is not lost compared to the Erlang implementation.

**Dictionary.bt** — iteration-based methods become pure BT:
```beamtalk
/// Evaluate block for each key-value pair, passing an Association.
do: block: Block -> Nil => @primitive "do:"

/// Evaluate block with key and value as separate arguments.
keysAndValuesDo: block: Block -> Nil =>
  self do: [:assoc | block value: assoc key value: assoc value]
```

### Phase 4: Self-Host Test Assertions

Move assertion logic from `beamtalk_test_case.erl` into `TestCase.bt`:

```beamtalk
/// Assert that condition is true.
assert: condition: Object -> Nil =>
  condition ifFalse: [
    self fail: "Assertion failed: expected true, got {condition}"
  ]

/// Assert that actual equals expected.
assert: actual equals: expected -> Nil =>
  (actual =:= expected) ifFalse: [
    self fail: "Expected {expected}, got {actual}"
  ]

/// Assert that condition is false.
deny: condition: Object -> Nil =>
  condition ifTrue: [
    self fail: "Denial failed: expected false, got {condition}"
  ]
```

`should:raise:`, `fail:`, `runAll`, `run:`, and test lifecycle (`setUp`/`tearDown` orchestration, test discovery) remain as `@primitive` — they require process spawning, try/catch infrastructure, and class reflection that the runtime provides.

**Bootstrapping trust:** Self-hosted assertions depend on `ifFalse:`, `ifTrue:`, string interpolation, and `fail:` all working correctly. Since `fail:` remains `@primitive` (Erlang-backed), the error-raising path is stable. The `ifTrue:`/`ifFalse:` methods on `True.bt`/`False.bt` are already pure Beamtalk and have been exercised since the earliest stdlib work. String interpolation (ADR 0023) is compiler-generated and tested independently. The risk of a silent assertion failure is low, but as a safeguard: existing Erlang-backed bootstrap tests (`stdlib/bootstrap-test/*.bt`) continue to validate core primitives independently.

### What Stays in Erlang

The following categories **must remain** as Erlang:

| Category | Modules | Reason |
|----------|---------|--------|
| Object system infrastructure | `beamtalk_actor`, `beamtalk_object_class`, `beamtalk_dispatch`, `beamtalk_primitive`, `beamtalk_message_dispatch`, `beamtalk_class_dispatch` | Beamtalk code runs on these — bootstrap paradox |
| Class registry & instantiation | `beamtalk_class_registry`, `beamtalk_class_instantiation`, `beamtalk_dynamic_object` | Required before any `.bt` module loads |
| Bootstrap stubs | `beamtalk_bootstrap`, `beamtalk_behaviour_bt`, `beamtalk_class_bt` | Needed before stdlib compiles |
| Future state machine | `beamtalk_future` (internal process loop) | Raw BEAM process, not a gen_server; process-level message receive |
| Exception handling | `beamtalk_exception_handler` | Called by compiler-generated try/catch |
| BIF wrappers | `beamtalk_string_ops` (grapheme), `beamtalk_regex_ops`, `beamtalk_file_ops`, `beamtalk_json_ops`, `beamtalk_system_ops`, `beamtalk_random_ops`, `beamtalk_character_ops` | Fundamentally wrap Erlang/OTP modules — all renamed to `_ops` suffix to signal they are primitive implementation modules |
| Concrete collection primitives | `do:`, `size` per concrete class; all BIF-backed overrides on `List`/`Set`/`Dictionary` | Back the primitive surface; BIF fast-paths for concrete types |
| OTP infrastructure | `beamtalk_runtime_app`, `beamtalk_runtime_sup`, `beamtalk_stdlib` | Supervision, module loading |

> **Naming convention:** All Erlang modules that exist solely to wrap primitives or BIFs for use by `.bt` files **must** use the `_ops` suffix (e.g., `beamtalk_list_ops`, `beamtalk_regex_ops`). This makes the boundary between pure-BT logic and primitive Erlang implementation visually clear in file listings, stack traces, and code review. Modules without `_ops` are infrastructure (actor loop, bootstrap, OTP app/supervisor) that Beamtalk code does not call directly.

## Prior Art

### Pharo/Squeak Smalltalk

The collection protocol is almost entirely pure Smalltalk layered on `do:`. The abstract `Collection` defines `collect:`, `select:`, `reject:`, `detect:`, `inject:into:` all in terms of `do:`. The **species pattern** (`species` returns the appropriate result class; `copyEmpty` uses it) ensures collection-returning operations produce the correct type. What stays in the VM: object allocation, dispatch, and `Array>>do:` as a primitive. Everything above is pure Smalltalk.

**What we adopt:** `do:` as the primitive boundary; all higher-order operations as pure Beamtalk on the abstract class. `addFirst:` + `reversed` replaces Pharo's `OrderedCollection>>addLast:` for O(n) accumulation.

**What we adapt:** We don't implement `species` — abstract `Collection` methods return `List` unconditionally. Concrete classes override with optimized primitives when needed.

### Newspeak

Takes self-hosting further than Pharo: the VM provides only allocation, dispatch, and I/O. Compiler, IDE, testing framework, regex — all written in Newspeak. Platform modules implement a fixed primitive interface; all shared logic is pure Newspeak.

**What we adopt:** The aspiration of minimizing the Erlang surface to a fixed primitive interface.

### Gleam

Uses `@external(erlang, "module", "function")` only for `length`, `reverse`, `flatten`. All higher-order operations (`map`, `filter`, `fold`, `find`, `any`, `all`, `partition`) are pure tail-recursive Gleam with a final `lists:reverse`. The dual-target design (Erlang + JavaScript) forces minimal `@external` use.

**What we adopt:** The principle that `@external`/`@primitive` should be the minimum for correctness, not performance. Pure-language implementations are preferred.

**What we match:** With `addFirst:` (O(1) cons) + `reversed`, we use exactly the same pattern as Gleam: prepend during iteration, reverse at the end.

### Elixir

`Enum` is entirely Elixir. It delegates via the `Enumerable` protocol, which for `List` ultimately calls `:lists.foldl`. The compiler stays in Erlang; an `elixir_bootstrap.erl` provides stubs for `def`/`defmodule`/`@` so `Kernel.ex` can load.

**What we adopt:** The bootstrap-stub pattern (already used: `beamtalk_behaviour_bt.erl`, `beamtalk_class_bt.erl`).

### Ruby (YJIT)

Ruby 3.4 moved `Array#each`, `Array#map`, `Array#select` from C to pure Ruby (under YJIT). The reason: C↔Ruby boundary prevents inlining. Pure Ruby lets YJIT inline the entire call including the block.

**What we observe:** The primitive boundary is not fixed — as compilation quality improves, more can move to the language. Today's `@primitive` overrides on `List` can be reconsidered if the BEAM JIT improves.

## User Impact

**Newcomer:** No change to the API. Collection operations work exactly as before. The benefit is indirect: `Future.bt` and `FileHandle.bt` become visible in docs, LSP completion, and `class` reflection.

**Smalltalk developer:** The stdlib structure now matches Pharo: abstract `Collection` defines higher-order operations in terms of `do:`/`inject:into:`, concrete classes override where performance demands. This is the canonical Smalltalk pattern and will feel immediately familiar.

**Erlang/BEAM developer:** Concrete collection operations (`List collect:`, `List select:`) still compile to `lists:map`, `lists:filter` — the same BIFs they'd use in Erlang. The abstract fallbacks use message sends, which is slightly slower but only triggers for user-defined collections.

**Production operator:** No change to runtime behavior for existing code. The dispatch path is identical for primitive-backed methods. Pure-BT methods on the abstract class add one message-send layer vs direct BIF call, but only for non-List/Set/Dictionary collections.

**Tooling developer:** `Future.bt` and `FileHandle.bt` enable LSP completion, go-to-definition, type inference, and documentation generation for two previously invisible classes.

## Steelman Analysis

### Alternative: Keep Everything in Erlang (Status Quo)

- **Newcomer**: "I don't need to know or care where the implementation lives — it just works."
- **BEAM veteran**: "Erlang code is debuggable with standard BEAM tools (observer, dbg, recon). Pure BT methods add a dispatch layer that makes stack traces longer. Non-local returns from `do:` blocks compile to throw/catch — I know what that costs."
- **Operator**: "No migration risk. Erlang has 25+ years of battle-tested collection operations."
- **Language designer**: "The current architecture clearly separates interface (`.bt`) from implementation (`.erl`). This is a valid design — it's how Elixir's compiler works."

### Alternative: Aggressive Self-Hosting (Move Everything Possible)

- **Smalltalk purist**: "A language that can't express its own stdlib is incomplete. Pharo's image is >95% Smalltalk. We should aspire to that."
- **Newcomer**: "If I can read the stdlib source in Beamtalk, I can learn the language by reading it."
- **Language designer**: "Self-hosting tests the language's expressiveness. If `collect:` can't be written in Beamtalk, the language has a gap."

### Tension Points

- BEAM veterans prefer the status quo (predictable stack traces, BIF performance). Smalltalk purists want maximum self-hosting.
- The phased approach resolves this: concrete classes keep BIF fast-paths, abstract classes get pure-BT defaults. Both cohorts get what they care about.
- Non-local return cost is a genuine concern — `detect:`, `anySatisfy:`, `allSatisfy:` may need to stay `@primitive` if benchmarks show unacceptable throw/catch overhead. The ADR explicitly gates these on measurement.

## Alternatives Considered

### Alternative A: Full Self-Hosting Including Concrete Collections

Move `List collect:`, `List select:`, etc. from `@primitive` to pure Beamtalk, eliminating the Erlang `beamtalk_list_ops.erl` module entirely.

Rejected because:
- `lists:map/2` and `lists:filter/2` are functions in the stdlib `lists` module, not BEAM BIFs; the compiler may inline some `lists` functions (e.g., when `inline_list_funcs` is enabled), but inlining is distinct from being a BIF. Replacing concrete fast-paths with BT message sends can measurably slow `collect:`/`select:` on lists, so concrete classes should retain their optimized, BIF-backed overrides.
- Concrete classes can always be reconsidered later if the BEAM JIT improves (Ruby YJIT precedent).

### Alternative B: Only Add Missing `.bt` Files, No Self-Hosting

Add `Future.bt` and `FileHandle.bt` but don't move any logic from Erlang to Beamtalk.

Rejected because:
- It leaves pure-algorithmic logic in Erlang where Beamtalk can express it. This misses the opportunity to validate the language's expressiveness and to provide readable stdlib source.
- User-defined collections (subclasses of Collection) currently get no default implementations — they must override every method. Pure-BT defaults on `Collection` fix this.

## Consequences

### Positive
- `Future` and `FileHandle` become first-class citizens: visible to compiler, type checker, LSP, and docs
- User-defined collections inherit working default implementations from `Collection`
- Stdlib source becomes a learning resource — readable Beamtalk, not opaque Erlang
- Test assertions become inspectable and extensible in Beamtalk
- Validates the language's expressiveness: if we can't write `collect:` in Beamtalk, we have a gap
- Reduces Erlang surface area by ~400-600 lines

### Negative
- Pure-BT abstract collection methods are slower than direct BIF calls (mitigated: concrete classes retain BIF fast-paths; only user-defined collections use the abstract defaults)
- Non-local returns in `detect:`/`anySatisfy:`/`allSatisfy:` have throw/catch overhead on BEAM — may need to stay `@primitive` if benchmarks show problems
- More complex dispatch for abstract collection operations (one extra message-send layer)
- Migration requires careful testing to ensure behavioral equivalence, particularly for `unwrap` (three-clause behavior)

### Neutral
- Concrete collection performance unchanged (BIF-backed `@primitive` overrides retained on List, Set, Dictionary)
- No user-facing API changes — all methods have the same names and behavior
- Bootstrap ordering unchanged — no new constraints

## Implementation

### Phase 1: Missing `.bt` Files (Small)
- Add `stdlib/src/Future.bt` with `@primitive` stubs
- Add `stdlib/src/FileHandle.bt` with `@primitive` stubs
- Wire Future dispatch through `beamtalk_primitive.erl` (FileHandle already dispatches via `send_file_handle/3`)
- Update `beamtalk_stdlib.erl` loader to include new modules
- **Affected components:** stdlib, runtime dispatch tables
- **Tests:** Verify `Future await`, `FileHandle lines` still work; verify `Future class` returns `Future`
- **Estimated size:** S

### Phase 2: `addFirst:` + Collection Self-Hosting (Medium)
- Add `addFirst:` primitive to `List.bt` — codegen: `[Item|Self]` (O(1) cons)
- Add codegen entry in `primitives/list.rs` for `"addFirst:"`
- Move `inject:into:`, `collect:`, `select:`, `reject:` from `@primitive` to pure Beamtalk on `Collection.bt` using `do:` + `addFirst:` + `reversed`
- Move `detect:`, `detect:ifNone:`, `includes:`, `anySatisfy:`, `allSatisfy:` from `@primitive` to pure BT using `do:` with non-local returns — **gated on benchmarks showing acceptable throw/catch overhead**
- Keep `do:`, `size` as the primitive boundary on `Collection`
- Keep all concrete class overrides (`List`/`Set`/`Dictionary` retain BIF-backed `@primitive` for every method)
- Remove corresponding functions from `beamtalk_collection_ops.erl`
- **Affected components:** stdlib (`Collection.bt`, `List.bt`), runtime (`beamtalk_collection_ops.erl`), codegen (`primitives/list.rs`)
- **Tests:** Add tests for `addFirst:`; add tests for a user-defined Collection subclass to verify default implementations; benchmark pure-BT vs BIF-backed paths
- **Estimated size:** M

### Phase 3: Algorithmic Operations (Medium)
- Move `indexOf:`, `eachWithIndex:` from `beamtalk_list_ops.erl` to `List.bt`
- Move `unwrap`, `unwrapOr:`, `unwrapOrElse:` from `beamtalk_tuple_ops.erl` to `Tuple.bt` — preserving all three clauses (ok, error, other)
- Move `keysAndValuesDo:`, `at:ifAbsent:` from `beamtalk_map_ops.erl` to `Dictionary.bt`
- Move `format_string` from `beamtalk_association.erl` to `Association.bt`
- **Affected components:** stdlib (`.bt` files), runtime (`*_ops.erl` files), codegen (primitive dispatch tables)
- **Tests:** Existing stdlib/test/*.bt tests validate behavioral equivalence; add edge-case tests for three-clause `unwrap`
- **Estimated size:** M

### Phase 4: Test Assertions (Small)
- Move `assert:`, `deny:`, `assert:equals:` from `beamtalk_test_case.erl` to `TestCase.bt`
- Keep `should:raise:`, `fail:`, `runAll`, `run:`, lifecycle as `@primitive`
- **Affected components:** stdlib (`TestCase.bt`), runtime (`beamtalk_test_case.erl`)
- **Tests:** Existing test suites exercise assertions; bootstrap tests (`stdlib/bootstrap-test/`) provide independent Erlang-backed validation of core primitives
- **Estimated size:** S

## Migration Path

No user-facing migration needed. All method signatures and behavior remain identical. The change is internal: implementation moves from Erlang to Beamtalk. Concrete collection performance is unaffected (BIF overrides retained).

Testing strategy:
- Existing `stdlib/test/*.bt` and `stdlib/bootstrap-test/*.bt` tests validate behavioral equivalence
- Add explicit tests for user-defined Collection subclasses to verify default implementations work
- Benchmark `collect:`/`select:` on abstract vs concrete paths to quantify performance difference
- Benchmark `detect:`/`anySatisfy:` to measure non-local return (throw/catch) overhead vs Erlang tail-recursive versions
- Verify `unwrap` three-clause behavior with edge cases: `{ok, val}`, `{error, reason}`, `{other, val}`

## References
- Related ADRs: ADR 0005 (Object Model), ADR 0006 (Dispatch), ADR 0007 (Compilable Stdlib), ADR 0013 (Class Protocol), ADR 0023 (String Interpolation), ADR 0032 (Early Class Protocol)
- Related issues: BT-507 (Future class and async combinators — separate ADR for Future's full API design), BT-792 (Full Metaclass Tower ADR)
- Pharo Collection hierarchy: `Collection>>collect:` defined in terms of `do:` + `species`
- Gleam stdlib: `@external` only for `length`/`reverse`/`flatten`; all HOFs are pure Gleam
- Ruby YJIT: C→Ruby migration for `Array#each`, `Array#map` in Ruby 3.4
