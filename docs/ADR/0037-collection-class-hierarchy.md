# ADR 0037: Collection Class Hierarchy — Shallow Abstract Layer with Minimal Primitive Surface

## Status
Accepted (2026-02-23)

## Context

When BT-443 was filed, Beamtalk's collection types (`List`, `Dictionary`, `Set`, `Tuple`) were completely flat — each an independent `Object` subclass with its own `dispatch/3` module and no shared protocol. This caused duplicated method implementations across types (every type had its own `size`, `isEmpty`, `includes:`, `do:`, `collect:`, `select:`, `reject:`, `inject:into:`) and meant that polymorphism across collection types was impossible.

Smalltalk-80 solves this with a deep hierarchy:

```text
Collection (abstract)
  ├── SequenceableCollection (abstract — ordered, indexed)
  │     ├── ArrayedCollection
  │     │     ├── Array
  │     │     └── String
  │     ├── OrderedCollection
  │     └── LinkedList
  ├── Set
  │     └── Dictionary
  └── Bag
```

This ADR decides how much of that hierarchy Beamtalk should adopt, given the constraints of the BEAM runtime, our sealed primitive types backed by Erlang BIFs, and the v0.1 scope.

### Current state (at time of decision)

- `Collection.bt` — abstract superclass, `do:` and `size` as subclass-responsibility, 12 pure-BT shared methods (ADR-0034 Phase 2b, BT-815)
- `List.bt` — `sealed Collection subclass: List`, Erlang linked list
- `Set.bt` — `sealed Collection subclass: Set`, Erlang ordsets
- `Dictionary.bt` — `sealed Collection subclass: Dictionary`, Erlang map
- `Tuple.bt` — `sealed Collection subclass: Tuple`, Erlang tuple
- `String.bt` — `sealed Collection subclass: String`, Erlang binary, grapheme-cluster elements
- No `SequenceableCollection.bt`; precedent set by `Number.bt` for value-type abstract superclass pattern

### Constraints

- **Sealed primitives**: `List`, `Set`, `Dictionary`, `Tuple`, `String` are all BEAM-backed types. Inserting an intermediate abstract class between `Collection` and these types is possible (superclass delegation works, per ADR-0006 and the `Number → Integer/Float` chain) but requires a coherent design rationale.
- **BIF performance**: Concrete methods like `List collect:` delegate to `lists:map/2`. These must stay as `@primitive` overrides — any hierarchy design must preserve these fast paths.
- **v0.1 scope**: ADR-0031 (Flat Namespace) established that v0.1 should minimise complexity. Hierarchy layers that don't unlock new user-visible capability should be deferred.
- **No mixin/trait support yet**: ADR-0025 (Gradual Typing and Protocols) is still Accepted but not implemented. A trait-based design that avoids class hierarchy entirely is not yet possible.

## Decision

We adopt a **shallow single-layer hierarchy**: one abstract class (`Collection`) directly above five sealed concrete types. No intermediate `SequenceableCollection` or other abstract subclasses for v0.1. String **is** a Collection.

```text
Object
  └── Collection (abstract)
        ├── List     (sealed — Erlang linked list)
        ├── Set      (sealed — Erlang ordsets)
        ├── Dictionary (sealed — Erlang map)
        ├── Tuple    (sealed — Erlang tuple)
        └── String   (sealed — Erlang binary, grapheme-cluster elements)
```

### Primitive surface

`Collection` defines exactly three subclass-responsibility methods. All sealed concrete classes implement them as `@primitive`:

| Method | Reason |
|--------|--------|
| `do: block: Block -> Nil` | Backed by `lists:foreach`, `maps:foreach`, etc. — BIF dispatch per type |
| `size -> Integer` | Backed by `erlang:length`, `maps:size`, `ordsets:size`, `erlang:tuple_size` |
| `printString -> String` | Type-specific formatting (`"List(…)"`, `"Set(…)"`, etc.) |

Every other shared collection method is pure Beamtalk on `Collection`, built on `do:` and `size`. This is identical to Pharo's design, where `do:` is the primitive boundary and all higher-order operations compose on it.

### Shared protocol on `Collection` (pure Beamtalk)

```beamtalk
abstract Object subclass: Collection

  size -> Integer => self subclassResponsibility
  do: block: Block -> Nil => self subclassResponsibility
  printString -> String => self subclassResponsibility

  isEmpty -> Boolean => self size =:= 0
  isNotEmpty -> Boolean => self isEmpty not

  includes: element -> Boolean =>
    self do: [:each | each =:= element ifTrue: [^true]].
    false

  inject: initial into: block: Block => @primitive "inject:into:"   // (*)

  collect: block: Block =>
    (self inject: #() into: [:acc :each | acc addFirst: (block value: each)]) reversed

  select: block: Block =>
    (self inject: #() into: [:acc :each |
      (block value: each) ifTrue: [acc addFirst: each] ifFalse: [acc]
    ]) reversed

  reject: block: Block =>
    self select: [:each | (block value: each) not]

  detect: block: Block =>
    self do: [:each | (block value: each) ifTrue: [^each]].
    nil

  detect: block: Block ifNone: noneBlock: Block =>
    self do: [:each | (block value: each) ifTrue: [^each]].
    noneBlock value

  anySatisfy: block: Block -> Boolean =>
    self do: [:each | (block value: each) ifTrue: [^true]].
    false

  allSatisfy: block: Block -> Boolean =>
    self do: [:each | (block value: each) ifFalse: [^false]].
    true

  asString -> String => self printString
```

> **(\*)** `inject:into:` is kept as `@primitive` on the abstract class. The pure-BT version using local-variable mutation with `do:` does not thread state correctly for abstract-class methods in v0.1: the compiler emits `lists:foreach` (no accumulator) rather than `lists:foldl` for this pattern. Concrete classes provide their own `@primitive` overrides anyway, so this only affects user-defined subclasses. See ADR-0034.

### Concrete classes keep BIF-backed overrides

Each sealed class retains `@primitive` overrides for every performance-critical method:

```beamtalk
sealed Collection subclass: List
  do: block: Block -> Nil    => @primitive "do:"       // lists:foreach
  size -> Integer            => @primitive "size"      // erlang:length
  collect: block: Block      => @primitive "collect:"  // lists:map
  select: block: Block       => @primitive "select:"   // lists:filter
  inject: initial into: b    => @primitive "inject:into:"  // lists:foldl
  // ... etc.
```

The pure-BT abstract defaults serve **user-defined Collection subclasses** that don't override individual methods.

### Species Pattern

`collect:`, `select:`, and `reject:` return a collection of the **receiver's type**, not always `List`. This is implemented functionally via a class-side `withAll:` factory on each sealed type:

```beamtalk
// On Collection (abstract)
species => self class

collect: block: Block =>
  result := (self inject: #() into: [:acc :each | acc addFirst: (block value: each)]) reversed.
  self species withAll: result

select: block: Block =>
  result := (self inject: #() into: [:acc :each |
    (block value: each) ifTrue: [acc addFirst: each] ifFalse: [acc]
  ]) reversed.
  self species withAll: result

reject: block: Block =>
  self select: [:each | (block value: each) not]
```

Each sealed type provides a class-side `withAll:` factory:

| Type | `class withAll: list` | Notes |
|------|-----------------------|-------|
| `List` | returns list unchanged | identity |
| `Set` | deduplicates | `ordsets:from_list` |
| `Array` | allocates array | `array:from_list` |
| `String` | joins graphemes | `list join` |
| `Tuple` | `erlang:list_to_tuple` | existing BIF |
| `Dictionary` | overrides `collect:` entirely | block receives values; result maps original keys to transformed values |

`species` is overridable — user-defined subclasses can override it to return a different class if needed. The metaclass tower (ADR-0036) makes `self class` a fully dispatchable first-class object, so `self species withAll:` is a normal class-side message send.

```beamtalk
#(1, 2, 3) collect: [:x | x * 2]           // => #(2, 4, 6)      (List)
((Set new add: 1) add: 2) select: [:x | x > 1]   // => Set(2)     (Set)
#[10, 20, 30] collect: [:x | x + 1]         // => #[11, 21, 31]   (Array — planned, BT-822)
"hi" collect: [:g | g uppercase]             // => #("H", "I")     (List — String>>collect: returns List since grapheme-mapped results aren't necessarily valid String graphemes)
```

> **String exception:** `String>>collect:` returns `List` rather than `String`, because the result of mapping over graphemes is not guaranteed to be a valid string of graphemes. `String>>select:` returns `String` (filtered graphemes remain valid). `String>>withAll:` is `list join` and is used by `select:`.

### Type annotation narrowing with species

> **Note:** This table describes the target state after BT-822 (species pattern) is implemented. Until then, `collect:`, `select:`, and `reject:` on the abstract class all return `List`. `String.bt` already implements `select: -> String` — filtered graphemes are always valid graphemes, so no species plumbing is needed for `select:`. The `-> String` column for `Array`, `Set`, `Tuple`, and `List` reflect the planned post-BT-822 state.

The abstract `Collection` methods carry the widest safe return type. Concrete classes narrow the return type via covariant override:

| Class | `collect:` return type | `select:` return type | `reject:` return type |
|-------|----------------------|-----------------------|-----------------------|
| `Collection` (abstract) | `-> Collection` | `-> Collection` | `-> Collection` |
| `List` | `-> List` | `-> List` | `-> List` |
| `Set` | `-> Set` | `-> Set` | `-> Set` |
| `Array` | `-> Array` | `-> Array` | `-> Array` |
| `Tuple` | `-> Tuple` | `-> Tuple` | `-> Tuple` |
| `String` | `-> List` | `-> String` | `-> String` |
| `Dictionary` | `-> Dictionary` | n/a (overrides entirely) | n/a |

Code written against the abstract `Collection` type gets `-> Collection` — correct but wide. Code using a concrete type statically (e.g., a `List` variable) gets the narrower concrete return type. This is covariant return type narrowing: the concrete override is a subtype of the abstract declaration.

`String>>collect:` is the only exception where the return type is *not* the receiver's type — mapping graphemes may produce a heterogeneous list, not a valid string. `String>>select:` and `reject:` do return `String` because filtered graphemes are always valid graphemes.

The implementation requires a new `Array` type (see below) before `Array>>withAll:` can be provided. Until then, `Array` is not yet in the hierarchy.

### String is a Collection

`String` is `sealed Collection subclass: String`. In Smalltalk-80 and Pharo, `String` is a `SequenceableCollection` of `Character`; Beamtalk follows this spirit but with a grapheme-cluster element type rather than a codepoint-integer `Character`.

**Element type:** Iteration over a `String` yields single-grapheme-cluster `String` values. `String>>at:` returns a `String` (the grapheme at that position), not a `Character` (which is `sealed Integer subclass: Character` — a codepoint integer). This is the correct semantic for a UTF-8 binary: a grapheme cluster may span multiple codepoints, so the natural atomic unit is a one-grapheme `String`.

```beamtalk
"hello" do: [:g | Transcript show: g]   // g is a String, each a single grapheme
"hello" at: 1                           // => "h"  (a String)
"hello" collect: [:g | g uppercase]     // => #("H", "E", "L", "L", "O")  (a List of Strings)
```

**Resolved implementation gaps:** The following changes bring `String` into full Collection conformance:

| Change | Resolution |
|--------|------------|
| `do:` | Renamed from `each:` → `do:` to match Collection protocol |
| `select:` return type | Remains `-> String` — the Erlang primitive returns a binary; filtered graphemes are always valid graphemes. `List>>join` / `join:` added for callers who do need a List. |
| `includes:` | Renamed to `includesSubstring:` for substring containment; Collection default `includes:` handles grapheme membership |
| `at:` return type | Changed from `-> Character` to `-> String` (grapheme-cluster, matching BEAM runtime) |

### REPL examples

```beamtalk
// Polymorphism across concrete types
#(1, 2, 3) size                            // => 3
#{#a => 1, #b => 2} size                   // => 2
((Set new add: 1) add: 2) size             // => 2
"hello" size                               // => 5

// Shared abstract protocol works on all types
#(1, 2, 3) isEmpty                         // => false
#(1, 2, 3) includes: 2                     // => true
#(1, 2, 3) collect: [:x | x * 2]          // => #(2, 4, 6)
#{#a => 1, #b => 2} collect: [:v | v * 2] // => #{#a => 2, #b => 4}  (after species/BT-822)
"hi" collect: [:g | g uppercase]           // => #("H", "I")

// User-defined Collection subclass inherits defaults
Collection subclass: Range
  from: start: Integer to: end: Integer => self new init: start end: end

  init: start end: end =>
    self start := start.
    self end := end

  size -> Integer => self end - self start + 1

  do: block: Block -> Nil =>
    i := self start.
    [i <= self end] whileTrue: [
      block value: i.
      i := i + 1
    ]

  printString -> String => "Range({self start}..{self end})"

r := Range from: 1 to: 5.
r size             // => 5
r isEmpty          // => false
r includes: 3      // => true
r collect: [:x | x * x]  // => #(1, 4, 9, 16, 25)
r select: [:x | x isOdd] // => #(1, 3, 5)
r inject: 0 into: [:sum :x | sum + x]  // => 15  (works via to_list → do: dispatch)
```

> **Note on `inject:into:` for user-defined subclasses:** The abstract `@primitive inject:into:` on `Collection` works generically for any subclass that implements `do:`. The runtime helper `beamtalk_collection_ops:inject_into/3` converts the receiver to a list by dispatching `do:` on the object, then folds over the result. User-defined subclasses only need to override `inject:into:` if they require custom accumulation semantics (e.g., Dictionary's key-preserving fold over values).

### Error examples

```beamtalk
// Attempting to instantiate the abstract class
Collection new
// => Error: Collection is abstract and cannot be instantiated

// Missing do: override in a Collection subclass
Collection subclass: BadCollection
  size -> Integer => 3
  printString -> String => "BadCollection"

BadCollection new do: [:x | x]
// => Error: subclassResponsibility — Collection>>do: not overridden
```

## Prior Art

### Pharo / Squeak Smalltalk

Pharo's `Collection` is the gold standard: abstract class with `do:` as the sole primitive, all higher-order operations (`collect:`, `select:`, `reject:`, `detect:`, `inject:into:`) as pure Smalltalk building on `do:`. The **species pattern** (`species` returns the result class, `copyEmpty` creates the accumulator) handles the question "what type does `collect:` return on a `Set`?". `String` is a `SequenceableCollection` of `Character`.

**What we adopt:** `do:` as the primitive boundary; pure-BT higher-order operations on the abstract class.

**What we adapt:** The species pattern — `collect:`, `select:`, `reject:` returning the receiver's type — is adopted but implemented functionally rather than with Pharo's mutation-based `copyEmpty` + `add:`. Each sealed type provides a class-side `withAll:` factory; `Collection>>species` returns `self class`; the abstract implementations end with `self species withAll: result`. See Species Pattern section below.

**What we defer:** `SequenceableCollection`. Pharo's intermediate layer requires `at:` as a shared protocol with consistent performance contracts across types — deferred to a future ADR.

### Newspeak

Minimises the VM surface to allocation, dispatch, and I/O. Collections are entirely self-hosted. No separate abstract layer for "sequenceable" — the hierarchy is flatter than Pharo's.

**What we adopt:** The aspiration of minimising the Erlang primitive surface to a fixed interface.

### Gleam

Gleam's `list` module uses `@external(erlang, ...)` only for `length`, `reverse`, `flatten`. All higher-order functions (`map`, `filter`, `fold`, `any`, `all`) are pure Gleam using the prepend-then-reverse accumulator pattern (`[element | acc]` + `lists.reverse`). There is no collection hierarchy — each type has its own module.

**What we adopt:** The O(1) prepend + `reversed` pattern for accumulator-based operations. `addFirst:` (BT-814) serves the same role as `[H|T]` in Gleam.

**What we consciously diverge from:** Gleam's flat per-module model. We add the abstract `Collection` layer to enable polymorphism and default implementations — a feature Gleam doesn't have because it targets both Erlang and JavaScript.

### Elixir

`Enumerable` is a protocol, not an abstract class. Any type can declare `Enumerable` conformance by implementing three callbacks. `Enum` module functions work on anything implementing `Enumerable`. There is no inheritance relationship between `List`, `Map`, `MapSet`.

**What we observe:** The protocol approach is more flexible than inheritance for adding enumerable behaviour to types that don't share an ancestor. This is relevant to ADR-0025 (Gradual Typing). For v0.1, single-inheritance from `Collection` is simpler and sufficient.

**Possible future direction:** If ADR-0025 delivers typed protocols, `Enumerable` as a protocol could supersede or complement the `Collection` abstract class. The current design doesn't preclude this.

## User Impact

**Newcomer (coming from Python/Ruby/JS):** The `Collection` abstract class is invisible day-to-day. Users work with `List`, `Set`, `Dictionary`, `Tuple` directly. The benefit surfaces when they write a custom container: it inherits working `collect:`, `select:`, `detect:`, etc. for free. Discoverability: `Collection class` in the REPL shows the abstract API; `Collection subclasses` lists all registered subclasses.

**Smalltalk developer:** The `Collection` → `do:` → higher-order operations pattern is canonical Smalltalk. The absence of `SequenceableCollection` will be noticed, but the pattern is the same. `String` as a `Collection` is consistent with Pharo, though iterating grapheme-cluster `String`s rather than `Character` codepoints is a departure. The species pattern (`collect:` on a `Set` returns a `Set`, etc.) matches Pharo's design; `String>>collect:` returns `List` rather than `String` (since mapped graphemes aren't guaranteed valid). The `Tuple` inclusion as a `Collection` is unusual but follows naturally from BEAM tuple semantics.

**Erlang/BEAM developer:** `List collect:` still compiles to `lists:map/2` — the BIF path is unchanged. The abstract defaults only activate for non-standard collection subclasses. Stack traces for standard types are unaffected. `Tuple` as a `Collection` maps to `erlang:tuple_to_list` + iteration, which is familiar from Erlang's own `tuple_to_list/1`.

**Production operator:** No change to runtime behavior for existing code. Abstract `Collection` methods add one message-send layer only for user-defined subclasses, not for `List`/`Set`/`Dictionary`/`Tuple`. Memory overhead: zero — `Collection` is a class object, not a per-instance allocation.

**Tooling developer:** `Collection` as a declared abstract type enables LSP to show the complete shared protocol for any collection type through superclass lookup. `printString` as abstract means the compiler and LSP can verify all Collection subclasses implement it.

## Steelman Analysis

### Option B: Full Smalltalk Hierarchy (SequenceableCollection etc.)

- 🧑‍💻 **Newcomer**: "Full hierarchy gives me type-level precision — if I receive a `SequenceableCollection`, I *know* it supports `at:`, `first`, `last`, `from:to:`. The contract is richer."
- 🎩 **Smalltalk purist**: "This is the canonical design from Goldberg & Robson. Every serious Smalltalk has it. `String` as a `SequenceableCollection` of `Character` is semantically correct and enables uniform algorithms over text and collections. Omitting it is a regression."
- ⚙️ **BEAM veteran**: "More precise types in dispatch: the compiler can specialise `SequenceableCollection>>at:` vs `Dictionary>>at:` without runtime class checks."
- 🏭 **Operator**: "More abstract classes → more method dispatch entries in the hierarchy → slightly more lookup work, but predictable."
- 🎨 **Language designer**: "Hierarchy encodes invariants. `SequenceableCollection` guarantees ordering, indexing, and bounded size. `Collection` only guarantees iteration. These are different contracts and should be different types."

**Why rejected for v0.1:** `List` (O(n) `at:`), `Tuple` (O(1) `at:`), and `String` (O(n) grapheme `at:`) have fundamentally different performance contracts for indexed access — grouping them under a shared `SequenceableCollection` with `at:` would misrepresent these contracts. `Tuple` is indexed but immutable — it doesn't need `OrderedCollection` semantics. The complexity cost outweighs the benefit at v0.1 scope. **Revisit when a shared ordered-type protocol is needed.**

### Option C: Traits / Protocol (no abstract class)

- 🧑‍💻 **Newcomer**: "Traits are like interfaces in Java/TypeScript — clear, explicit, structural. I'd understand what `Enumerable` means without knowing the class hierarchy."
- 🎩 **Smalltalk purist**: "Newspeak uses mixins; Pharo 11 introduced traits for exactly this kind of shared protocol. Traits compose better than inheritance for orthogonal concerns."
- ⚙️ **BEAM veteran**: "Elixir's `Enumerable` protocol is the obvious comparison. It works beautifully in Elixir. Structural dispatch avoids the inheritance coupling."
- 🏭 **Operator**: "Protocols are explicit. I can check conformance at compile time. No surprise method resolution order."
- 🎨 **Language designer**: "Traits separate the *what* (Enumerable protocol) from the *how* (implementation per type). Abstract classes conflate them. As the stdlib grows, trait composition will be more flexible."

**Why rejected for v0.1:** ADR-0025 (Gradual Typing) is Accepted but not implemented. We can't use typed protocols as the primary mechanism for shared collection behaviour until the protocol system exists. The current abstract class design is a valid starting point that doesn't preclude an `Enumerable` protocol being layered on later.

### Option D: Flat — No Shared Abstract Class

- ⚙️ **BEAM veteran**: "This is how Erlang does it. Each module is self-contained. No inheritance overhead. `lists:map/2` knows it's operating on a list and can be optimised accordingly."
- 🏭 **Operator**: "Simple, fast, predictable. Every method dispatch goes directly to the concrete type's module."
- 🎩 **Smalltalk purist** (against): "A language with no Collection hierarchy is not Smalltalk. Polymorphism across collection types is a core feature. `Bag`, `OrderedCollection`, and user-defined collections become impossible to write generically."

**Why rejected:** User-defined collection subclasses need default implementations. Without `Collection`, every custom container must implement `collect:`, `select:`, `reject:`, etc. from scratch. This was the status quo before BT-815 and it was a known gap.

### Tension Points

- **Smalltalk purists vs. v0.1 scope**: Purists want `SequenceableCollection` to encode the ordered-indexed contract as a type. This is the right long-term design but requires resolving the `at:` performance contract mismatch across `List`, `Tuple`, and `String`. The tension is explicit and the deferral is conditional.
- **Traits vs. inheritance**: BEAM veterans and language designers favour traits (Elixir `Enumerable` model); the current hierarchy is a pragmatic interim. If ADR-0025 delivers, `Enumerable` as a protocol could complement or partially replace `Collection` as an abstract class.
- **Species vs. no-species**: Pharo's species pattern returns the *correct* type from `collect:` on a `Set` (another `Set`). We are implementing this (BT-822) — the species pattern is the accepted design. Until BT-822 lands, the abstract fallback returns `List`.

## Alternatives Considered

### `SequenceableCollection` between `Collection` and `List`/`Tuple`

A `SequenceableCollection` abstract class could group ordered, indexed types (`List`, `Tuple`) and share `at:`, `first`, `last`, `from:to:`, `reversed`, `eachWithIndex:` at that level.

Rejected for v0.1. The fundamental problem is the `at:` performance contract:

| Type | `at:` complexity | Backing structure |
|------|-----------------|-------------------|
| `Tuple` | O(1) | Erlang tuple — contiguous memory, `element/2` is pointer arithmetic |
| `List` | O(n) | Erlang linked list — `lists:nth/2` walks n nodes |
| `String` | O(n) | Erlang binary — grapheme clusters are variable-width UTF-8, indexing requires scanning from the start |

There is no BEAM primitive that gives O(1) dynamic random access to a variable-length sequence. The `array` module provides O(log n) access via a sparse tree, but there is no Beamtalk type backed by it yet. Erlang's own idiom is to avoid index-based access on lists entirely.

A `SequenceableCollection` abstract class with `at:` would encode a false uniformity: code written against the abstraction might loop `1 to: coll size do: [:i | coll at: i]` assuming reasonable random access, producing O(n²) on `List` and `String`. In Pharo this problem is less acute because `Array` (O(1)) is the primary sequence type and `OrderedCollection` wraps it — `SequenceableCollection>>at:` is O(1) for all practical Pharo types.

Methods that could be safely shared (`first`, `last`, `reversed`) are too thin to justify the layer. A well-designed `SequenceableCollection` would need either: (a) no `at:` in the shared contract, or (b) a new `Vector`/`Array` type backed by the Erlang `array` module as the primary `SequenceableCollection` leaf — leaving `List` under `Collection` with no indexed-access protocol. Either path is a design decision in itself. Deferred pending that analysis.

### `String` as a separate `SequenceableCollection` branch

Rather than putting `String` directly under `Collection`, it could be placed under a `SequenceableCollection` along with `List` and `Tuple`, since all three support `at:`, `first`/`last`, indexed access. Rejected for v0.1 for the same reasons `SequenceableCollection` is deferred generally — `List` (O(n) `at:`) and `Tuple` (O(1) `at:`) have different performance contracts that `SequenceableCollection` would misrepresent. Placing `String` directly under `Collection` is the consistent v0.1 choice.

### No species pattern — always return `List` from accumulator operations

Pharo's `species` method returns the appropriate result class so `aSet collect: [...]` returns a `Set`, not an `Array`. An alternative is to always return `List` from abstract accumulator operations. Arguments for this simpler approach: (a) implementation is trivial — no constructor protocol needed, (b) the most common use case is transforming to a list anyway.

**Rejected in favour of the species pattern** (BT-822): the semantics are genuinely surprising to Smalltalk users ("I filtered a `Set`, why do I get a `List`?") and the constructor protocol (`withAll:`) is straightforward for the sealed concrete types. The metaclass tower (ADR-0036) makes `self species withAll:` a normal class-side message send. The species pattern is implemented in the abstract class in terms of `self species withAll:`, with each sealed type providing the factory. Until BT-822 is complete, the fallback is to return `List`.

## Consequences

### Positive
- User-defined `Collection` subclasses inherit 10+ working methods (`collect:`, `select:`, `reject:`, `detect:`, `anySatisfy:`, `allSatisfy:`, `isEmpty`, `isNotEmpty`, `includes:`, `asString`) for free, needing only `do:`, `size`, and `printString`
- Polymorphism across all five concrete types: any code taking a `Collection` works with `List`, `Set`, `Dictionary`, `Tuple`, or `String`
- `String` is a `Collection` — consistent with Smalltalk-80 convention; uniform protocol across all container types
- Stdlib source is readable Beamtalk: the abstract operations on `Collection` are a learning resource
- Validates language expressiveness: `collect:`, `select:`, `reject:` written in pure Beamtalk with non-trivial control flow
- No performance regression for existing code: concrete types retain BIF-backed `@primitive` overrides

### Negative
- Species pattern is planned (BT-822) but not yet implemented — until then, `collect:`, `select:`, `reject:` return `List` on all types
- No `SequenceableCollection` — `at:`, `first`, `last` are not part of the shared abstract protocol; cannot be written generically over ordered collections
- `inject:into:` on abstract `Collection` remains `@primitive` (compiler limitation for local-variable mutation in abstract methods) — the runtime dispatches `do:` generically so user-defined subclasses inherit a working `inject:into:` for free; override only if custom accumulation semantics are needed (e.g., Dictionary's key-preserving fold)
- String's `includes:` (substring containment) renamed to `includesSubstring:` — existing code must migrate; `each:` renamed to `do:`; `at:` return type changed from `Character` to `String`

### Neutral
- Five sealed concrete types: `List`, `Set`, `Dictionary`, `Tuple`, `String` — no new concrete types needed for v0.1
- Bootstrap ordering unchanged — `Collection.bt` loads after `Object.bt`, before concrete collection types

## Implementation

This ADR documents decisions that are **already implemented** as of 2026-02-23. The work was done across multiple issues tracked under ADR-0034:

| Phase | Issue | Status | Description |
|-------|-------|--------|-------------|
| Hierarchy structure | BT-443 (this ADR) | ✅ Accepted | Design decision documented |
| `addFirst:` primitive | BT-814 | ✅ Implemented | O(1) list cons for accumulator pattern |
| Abstract Collection protocol | BT-815 | ✅ Implemented | 10 pure-BT methods on `Collection.bt` |
| `Number.bt` precedent | BT-334 | ✅ Implemented | Abstract numeric superclass pattern |

All ADR-0034 implementation work is now complete:

| Phase | Issue | Status | Description |
|-------|-------|--------|-------------|
| Phase 1 | BT-813 | ✅ Done | `Future.bt` and `FileHandle.bt` as `@primitive` stubs |
| Phase 2a | BT-814 | ✅ Done | `addFirst:` O(1) list cons primitive |
| Phase 2b | BT-815 | ✅ Done | Abstract Collection protocol in pure Beamtalk |
| Phase 3 | BT-816 | ✅ Done | Self-host `List` algorithmic ops (`indexOf:`, `eachWithIndex:`) |
| Phase 3 | BT-817 | ✅ Done | Self-host `Tuple` unwrap ops |
| Phase 3 | BT-818 | ✅ Done | Self-host `Dictionary keysAndValuesDo:` and `Association` formatting |
| Phase 4 | BT-819 | ✅ Done | Self-host `TestCase` assertions |

Resolved gaps addressed by this ADR:

| Topic | Status | Resolution |
|-------|--------|------------|
| `String>>do:` primitive | ✅ Done | Renamed `each:` → `do:` |
| `String>>select:` return type | ✅ Done | Remains `-> String`; `List>>join` / `join:` added for callers who need a List |
| `String>>includes:` override | ✅ Done | Renamed to `includesSubstring:`; Collection default handles grapheme membership |

Future design work deferred by this ADR:

| Topic | When to revisit | Prerequisite |
|-------|----------------|--------------|
| Species pattern + `Array` type | Planned — see BT-822 | Requires `Array` implementation for `Array>>withAll:` |
| `SequenceableCollection` | After `Array` is added | `Array` (O(log n) `at:`) + `Tuple` (O(1) `at:`) + `String` (O(n) `at:`) gives three meaningful subtypes; `at:` performance contracts still diverge so careful design needed |
| `Enumerable` as typed protocol | After ADR-0025 (Gradual Typing) is implemented | ADR-0025 implementation |

## Migration Path

This ADR introduces three breaking changes to `String`:

| Old API | New API | Search/replace |
|---------|---------|----------------|
| `includes:` (substring test) | `includesSubstring:` | `includes:` → `includesSubstring:` on String receivers |
| `each:` (iteration) | `do:` | `each:` → `do:` on String receivers |
| `at:` returns `Character` | `at:` returns `String` | Code expecting a `Character` (integer) from `String>>at:` must adapt to receiving a single-grapheme `String` |

Since Beamtalk is pre-v0.1, no deprecation period or codemod is provided. The renames were applied to all existing stdlib tests and examples as part of this ADR.

## References
- Related issues: BT-443, BT-814, BT-815, BT-822 (Array + species pattern implementation)
- Related ADRs: ADR-0005 (Object Model), ADR-0006 (Dispatch), ADR-0007 (Compilable Stdlib), ADR-0013 (Class Protocol), ADR-0025 (Gradual Typing and Protocols — note: ADR-0025 Phase 3 uses `Collection` as a protocol name; if implemented, rename to `Enumerable` to avoid collision with this abstract class), ADR-0032 (Early Class Protocol), ADR-0034 (Stdlib Self-Hosting — note: ADR-0034 describes `inject:into:` becoming pure BT; the compiler limitation documented here means it remains `@primitive` on the abstract class, though concrete classes still use `@primitive` overrides directly), ADR-0036 (Full Metaclass Tower — enables `self class` as dispatchable first-class object, required for species pattern)
- Pharo Collection hierarchy: `Collection>>collect:` defined in terms of `do:` + species pattern
- Gleam stdlib: accumulator pattern with `[H|T]` + `lists:reverse`
- Smalltalk-80 Blue Book: Chapter 8 — Collections
