# ADR 0037: Collection Class Hierarchy — Shallow Abstract Layer with Minimal Primitive Surface

## Status
Accepted (2026-02-23)

## Context

When BT-443 was filed, Beamtalk's collection types (`List`, `Dictionary`, `Set`, `Tuple`) were completely flat — each an independent `Object` subclass with its own `dispatch/3` module and no shared protocol. This caused duplicated method implementations across types (every type had its own `size`, `isEmpty`, `includes:`, `do:`, `collect:`, `select:`, `reject:`, `inject:into:`) and meant that polymorphism across collection types was impossible.

Smalltalk-80 solves this with a deep hierarchy:

```
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
- `String.bt` — `sealed Object subclass: String`, Erlang binary — **not** a Collection
- No `SequenceableCollection.bt`; precedent set by `Number.bt` for value-type abstract superclass pattern

### Constraints

- **Sealed primitives**: `List`, `Set`, `Dictionary`, `Tuple`, `String` are all BEAM-backed types. Inserting an intermediate abstract class between `Collection` and these types is possible (superclass delegation works, per ADR-0006 and the `Number → Integer/Float` chain) but requires a coherent design rationale.
- **BIF performance**: Concrete methods like `List collect:` delegate to `lists:map/2`. These must stay as `@primitive` overrides — any hierarchy design must preserve these fast paths.
- **v0.1 scope**: ADR-0031 (Flat Namespace) established that v0.1 should minimise complexity. Hierarchy layers that don't unlock new user-visible capability should be deferred.
- **No mixin/trait support yet**: ADR-0025 (Gradual Typing and Protocols) is still Accepted but not implemented. A trait-based design that avoids class hierarchy entirely is not yet possible.

## Decision

We adopt a **shallow single-layer hierarchy**: one abstract class (`Collection`) directly above the four sealed concrete types. No intermediate `SequenceableCollection` or other abstract subclasses for v0.1. String is **not** a Collection.

```
Object
  ├── Collection (abstract)
  │     ├── List     (sealed — Erlang linked list)
  │     ├── Set      (sealed — Erlang ordsets)
  │     ├── Dictionary (sealed — Erlang map)
  │     └── Tuple    (sealed — Erlang tuple)
  └── String         (sealed — Erlang binary, NOT a Collection)
```

### Primitive surface

`Collection` defines exactly two subclass-responsibility methods. All sealed concrete classes implement them as `@primitive`:

| Method | Reason |
|--------|--------|
| `do: block: Block -> Nil` | Backed by `lists:foreach`, `maps:foreach`, etc. — BIF dispatch per type |
| `size -> Integer` | Backed by `erlang:length`, `maps:size`, `ordsets:size`, `erlang:tuple_size` |

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

### String is not a Collection

`String` is `sealed Object subclass: String`, not a `Collection` subclass. In Smalltalk-80, `String` is a `SequenceableCollection` of `Character`; in Pharo, this enables uniform `collect:`, `select:`, and `do:` over characters. Beamtalk departs from this for three reasons:

1. **No `Character` type in v0.1**: Beamtalk has no `Character` class; strings are Erlang binaries, iterated as grapheme clusters (Unicode code points) via Erlang's `unicode` module. The unit of iteration is unclear without `Character`.
2. **API incompatibility**: String iteration yields grapheme strings (multi-byte sequences), not single characters. `collect:` on a String returning a `List` of grapheme-strings is surprising and inconsistent with List's semantics.
3. **v0.1 scope**: String-as-collection is a significant semantic commitment. Deferring it avoids locking in a decision before `Character` and Unicode protocols are designed.

If a `Character` class is added later, this decision can be revisited via a new ADR.

### REPL examples

```beamtalk
// Polymorphism across concrete types
#(1, 2, 3) size           // => 3
#{a: 1, b: 2} size        // => 2
#{1, 2, 3} size           // => 3

// Shared abstract protocol works on all types
#(1, 2, 3) isEmpty        // => false
#(1, 2, 3) includes: 2    // => true
#(1, 2, 3) collect: [:x | x * 2]  // => #(2, 4, 6)
#{a: 1, b: 2} collect: [:pair | pair value * 2]  // => #(2, 4)

// User-defined Collection subclass inherits defaults
Object subclass: Range
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
r inject: 0 into: [:sum :x | sum + x]  // => 15
```

### Error examples

```beamtalk
// Attempting to instantiate the abstract class
Collection new
// => Error: Collection is abstract and cannot be instantiated

// Missing do: override in a subclass
Object subclass: BadCollection
  size -> Integer => 3

BadCollection new do: [:x | x]
// => Error: subclassResponsibility — Collection>>do: not overridden
```

## Prior Art

### Pharo / Squeak Smalltalk

Pharo's `Collection` is the gold standard: abstract class with `do:` as the sole primitive, all higher-order operations (`collect:`, `select:`, `reject:`, `detect:`, `inject:into:`) as pure Smalltalk building on `do:`. The **species pattern** (`species` returns the result class, `copyEmpty` creates the accumulator) handles the question "what type does `collect:` return on a `Set`?". `String` is a `SequenceableCollection` of `Character`.

**What we adopt:** `do:` as the primitive boundary; pure-BT higher-order operations on the abstract class.

**What we adapt:** We don't implement the species pattern — `collect:`, `select:`, `reject:` always return `List`, regardless of receiver type. This is simpler and sufficient for v0.1 (Pharo's species pattern requires user-defined collection constructors and `add:` semantics that aren't fully designed yet).

**What we defer:** `SequenceableCollection` and `String`-as-collection. Pharo has both, but they require `Character` and `at:` as a shared protocol — work deferred to future ADRs.

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

**Smalltalk developer:** The `Collection` → `do:` → higher-order operations pattern is canonical Smalltalk. The absence of `SequenceableCollection` will be noticed, but the pattern is the same. The no-species simplification (always return `List`) differs from Pharo but is easy to understand. The `Tuple` inclusion as a `Collection` is unusual but follows naturally from BEAM tuple semantics.

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

**Why rejected for v0.1:** No `Character` class means `String`-as-`SequenceableCollection` has an unresolved element type. `Tuple` is indexed but immutable — it doesn't need `OrderedCollection` semantics. `List` and `Tuple` differ enough that a shared `SequenceableCollection` interface would be thin. The complexity cost outweighs the benefit at v0.1 scope. **Revisit when `Character` is designed.**

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

- **Smalltalk purists vs. v0.1 scope**: Purists want `SequenceableCollection` and `String`-as-collection. These are the right long-term design but require `Character` and a Unicode protocol that isn't designed yet. The tension is explicit and the deferral is conditional.
- **Traits vs. inheritance**: BEAM veterans and language designers favour traits (Elixir `Enumerable` model); the current hierarchy is a pragmatic interim. If ADR-0025 delivers, `Enumerable` as a protocol could complement or partially replace `Collection` as an abstract class.
- **Species vs. no-species**: Pharo's species pattern returns the *correct* type from `collect:` on a `Set` (another `Set`). We always return `List`. This is a genuine simplification that may surprise experienced Smalltalk users. The trade-off is explicit.

## Alternatives Considered

### `SequenceableCollection` between `Collection` and `List`/`Tuple`

A `SequenceableCollection` abstract class could group ordered, indexed types (`List`, `Tuple`) and share `at:`, `first`, `last`, `from:to:`, `reversed`, `eachWithIndex:` at that level.

Rejected for v0.1: `List` is a linked list (O(n) `at:`), `Tuple` is a fixed-size random-access structure (O(1) `at:`). Their `at:` performance characteristics differ fundamentally — grouping them under a shared abstract `at:` would misrepresent the contract. Additionally, with no `String`-as-collection, `SequenceableCollection` would only contain two types and provide minimal value over the current direct `Collection` subclassing. Deferred pending `Character` design.

### `String` as `sealed Collection subclass: String`

`String` is `sealed Object subclass: String`, not a `Collection`. Making it a `Collection` subclass would require defining the element type for iteration (`do: [:char | ...]`). Without a `Character` class, the element would be a single-grapheme `String`, which is confusing (`'hello' collect: [:c | c]` returning `#('h', 'e', 'l', 'l', 'o')` as a `List` of `String`s). Deferred pending `Character` ADR.

### Species pattern for `collect:`/`select:`/`reject:`

Pharo's `species` method returns the appropriate result class so `aSet collect: [...]` returns a `Set`, not an `Array`. We always return `List` from abstract accumulator operations. Rejected because: (a) the semantics are surprising ("I filtered a `Set`, why do I get a `List`?"), (b) it requires `Collection` subclasses to implement a constructor protocol that isn't yet designed, and (c) the most common use case is transforming to a list anyway.

## Consequences

### Positive
- User-defined `Collection` subclasses inherit 10+ working methods (`collect:`, `select:`, `reject:`, `detect:`, `anySatisfy:`, `allSatisfy:`, `isEmpty`, `isNotEmpty`, `includes:`, `asString`) for free, needing only `do:`, `size`, and `printString`
- Polymorphism across all four concrete types: any code taking a `Collection` works with `List`, `Set`, `Dictionary`, or `Tuple`
- Stdlib source is readable Beamtalk: the abstract operations on `Collection` are a learning resource
- Validates language expressiveness: `collect:`, `select:`, `reject:` written in pure Beamtalk with non-trivial control flow
- No performance regression for existing code: concrete types retain BIF-backed `@primitive` overrides

### Negative
- `String` is not a `Collection` — breaks from Smalltalk-80 convention; developers coming from Pharo will notice
- `collect:` on a `Set` returns `List`, not `Set` — no species pattern; may surprise Smalltalk-experienced users
- No `SequenceableCollection` — `at:`, `first`, `last` are not part of the shared abstract protocol; cannot be written generically over ordered collections
- `inject:into:` on abstract `Collection` remains `@primitive` (compiler limitation for local-variable mutation in abstract methods) — user-defined subclasses must implement it if they need custom accumulation

### Neutral
- Four sealed concrete types: `List`, `Set`, `Dictionary`, `Tuple` — no new concrete types needed for v0.1
- `String` remains independently tested and maintained as `sealed Object subclass`
- Bootstrap ordering unchanged — `Collection.bt` loads after `Object.bt`, before concrete collection types

## Implementation

This ADR documents decisions that are **already implemented** as of 2026-02-23. The work was done across multiple issues tracked under ADR-0034:

| Phase | Issue | Status | Description |
|-------|-------|--------|-------------|
| Hierarchy structure | BT-443 (this ADR) | ✅ Accepted | Design decision documented |
| `addFirst:` primitive | BT-814 | ✅ Implemented | O(1) list cons for accumulator pattern |
| Abstract Collection protocol | BT-815 | ✅ Implemented | 10 pure-BT methods on `Collection.bt` |
| `Number.bt` precedent | BT-334 | ✅ Implemented | Abstract numeric superclass pattern |

Remaining open work (from ADR-0034 implementation tracking):

| Phase | Issue | Status | Description |
|-------|-------|--------|-------------|
| Phase 3 | BT-816 | Open | Self-host `List` algorithmic ops (`indexOf:`, `eachWithIndex:`) |
| Phase 3 | BT-817 | Open | Self-host `Tuple` unwrap ops |
| Phase 3 | BT-818 | Open | Self-host `Dictionary keysAndValuesDo:` and `Association` formatting |
| Phase 4 | BT-819 | Open | Self-host `TestCase` assertions |

Future design work deferred by this ADR:

| Topic | When to revisit | Prerequisite |
|-------|----------------|--------------|
| `SequenceableCollection` | When `Character` class is designed | `Character` ADR |
| `String` as `Collection` | After `Character` and Unicode protocol ADR | `Character` ADR |
| `Enumerable` as typed protocol | After ADR-0025 (Gradual Typing) is implemented | ADR-0025 implementation |
| Species pattern for `collect:`/`select:` | After collection constructors are designed | Constructor protocol ADR |

## References
- Related issues: BT-443, BT-814, BT-815
- Related ADRs: ADR-0005 (Object Model), ADR-0006 (Dispatch), ADR-0007 (Compilable Stdlib), ADR-0013 (Class Protocol), ADR-0025 (Gradual Typing and Protocols), ADR-0032 (Early Class Protocol), ADR-0034 (Stdlib Self-Hosting)
- Pharo Collection hierarchy: `Collection>>collect:` defined in terms of `do:` + species pattern
- Gleam stdlib: accumulator pattern with `[H|T]` + `lists:reverse`
- Smalltalk-80 Blue Book: Chapter 8 — Collections
