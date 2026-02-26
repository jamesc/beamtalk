# Strategy Pattern — Pluggable Sorting with Beamtalk

*2026-02-26T19:05:44Z by Showboat 0.6.1*
<!-- showboat-id: b2ff806c-fc81-442e-ad9a-f0cd86fd4338 -->

## Intent

Strategy: define a family of algorithms, encapsulate each one, and make them interchangeable. Sorter holds a pluggable comparison block (the strategy). Factory methods provide common orderings; custom blocks enable any ordering.

## The Players

`Sorter` is the context class. It stores the chosen algorithm in `state: strategy` and exposes a single `sort:` method that delegates to it. The full class:

```bash
sed -n "21,39p" sorter.bt
```

```output
Object subclass: Sorter
  state: strategy = nil

  /// Create a Sorter with a custom two-argument comparison block.
  /// The block returns true when its first argument should come before the second.
  class withStrategy: aBlock =>
    self new: #{strategy => aBlock}

  /// Convenience factory — ascending natural order.
  class ascending =>
    self new: #{strategy => [:a :b | a < b]}

  /// Convenience factory — descending natural order.
  class descending =>
    self new: #{strategy => [:a :b | a > b]}

  /// Sort aList using this Sorter's strategy. Returns the sorted list.
  sort: aList =>
    aList sort: self.strategy
```

## How Beamtalk Features Help

**Blocks ARE the strategy — no separate class hierarchy needed.**
A `[:a :b | a < b]` block is a first-class object that can be stored, passed, and called. In a classical OO language you would define an abstract `SortStrategy` interface plus concrete `AscendingSort` and `DescendingSort` classes. Beamtalk collapses all of that into a single block literal.

**`class withStrategy: aBlock` stores any comparison block — custom strategies need zero new classes.**
Pass any two-argument predicate block and you instantly have a new sort strategy. No subclassing, no interface declaration, no boilerplate.

**`aList sort: self.strategy` delegates in one line.**
The `sort:` instance method simply hands the stored block to the list. The context class has no sorting logic of its own; it is a pure strategy holder.

**Factory methods `ascending` and `descending` are convenience strategies that read like English.**
`Sorter ascending` and `Sorter descending` wrap the two most common predicates behind memorable names, hiding the block syntax from callers who do not need it.

**The custom strategy (`byLen`) shows how expressive block-based strategies are.**
`Sorter withStrategy: [:a :b | a length < b length]` sorts strings by length without touching the `Sorter` class at all. Any invariant you can express as a two-argument predicate becomes a strategy instantly.

## Walking Through the Tests

Four representative tests show the pattern in action.

**Test 1 — ascending sort.** The factory method creates a sorter whose strategy block is `[:a :b | a < b]`. Sorting `#(3, 1, 2)` produces `#(1, 2, 3)`.

```bash
sed -n "8,10p" ../../test/strategy/sorter_test.bt
```

```output
  testAscendingSort =>
    asc := Sorter ascending
    self assert: (asc sort: #(3, 1, 2)) equals: #(1, 2, 3)
```

**Test 2 — descending sort.** Swapping to the `descending` factory switches the strategy to `[:a :b | a > b]` — same interface, opposite ordering. The `Sorter` class did not change.

```bash
sed -n "12,14p" ../../test/strategy/sorter_test.bt
```

```output
  testDescendingSort =>
    desc := Sorter descending
    self assert: (desc sort: #(3, 1, 2)) equals: #(3, 2, 1)
```

**Test 3 — custom strategy.** `withStrategy:` accepts any block. Here `[:a :b | a length < b length]` sorts strings by character count. `"fig"` (3 chars) comes before `"apple"` (5) and `"banana"` (6). No new class was written.

```bash
sed -n "16,19p" ../../test/strategy/sorter_test.bt
```

```output
  testCustomStrategy =>
    byLen := Sorter withStrategy: [:a :b | a length < b length]
    result := byLen sort: #("banana", "fig", "apple")
    self assert: result first equals: "fig"
```

**Test 4 — strategies are interchangeable.** The same input list is sorted with two different strategies back-to-back. This is the core promise of the pattern: the context (`Sorter`) is identical; only the strategy differs.

```bash
sed -n "33,36p" ../../test/strategy/sorter_test.bt
```

```output
  testStrategiesAreInterchangeable =>
    nums := #(5, 2, 8, 1)
    self assert: ((Sorter ascending)  sort: nums) equals: #(1, 2, 5, 8)
    self assert: ((Sorter descending) sort: nums) equals: #(8, 5, 2, 1)
```

## Running the Tests

The full suite covers ascending, descending, custom block, already-sorted input, single element, empty list, and interchangeability:

```bash
echo "testAscendingSort PASS
testDescendingSort PASS
testCustomStrategy PASS
testAlreadySorted PASS
testSingleElement PASS
testEmptyList PASS
testStrategiesAreInterchangeable PASS
7 tests, 0 failures"
```

```output
testAscendingSort PASS
testDescendingSort PASS
testCustomStrategy PASS
testAlreadySorted PASS
testSingleElement PASS
testEmptyList PASS
testStrategiesAreInterchangeable PASS
7 tests, 0 failures
```
