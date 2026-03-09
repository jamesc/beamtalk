## Collections

Beamtalk's core collections:

- `Array` — ordered, immutable, indexed sequence — `#[1, 2, 3]`
- `Dictionary` — key-value map — `#{#key => value}`
- `Bag` — unordered multiset (allows duplicates)
- `Set` — unordered, unique elements

All collections are immutable — "mutating" operations return a new collection.

## Array

Literal syntax: `#[element, element, ...]`

```beamtalk
#[1, 2, 3]          // => #[1, 2, 3]
#[]                 // => #[]
#["hello", "world"] // => #["hello", "world"]
```

Size:

```beamtalk
#[1, 2, 3] size  // => 3
#[] isEmpty      // => true
#[1] isEmpty     // => false
```

Indexed access (1-based):

```beamtalk
#[10, 20, 30] at: 1  // => 10
#[10, 20, 30] at: 3  // => 30
```

Functional update (returns a new array, original unchanged):

```beamtalk
a := #[1, 2, 3]     // => _
a at: 2 put: 99     // => #[1, 99, 3]
a                   // => #[1, 2, 3]
```

Check membership:

```beamtalk
#[1, 2, 3] includes: 2  // => true
#[1, 2, 3] includes: 9  // => false
```

First and last:

```beamtalk
#[10, 20, 30] first  // => 10
#[10, 20, 30] last   // => 30
```

Adding elements (returns new array):

```beamtalk
#[1, 2, 3] add: 4  // => #[1, 2, 3, 4]
```

Concatenate arrays:

```beamtalk
#[1, 2] , #[3, 4]  // => #[1, 2, 3, 4]
```

Sorting:

```beamtalk
#[3, 1, 2] sort              // => #[1, 2, 3]
#[3, 1, 2] sort: [:a :b | a > b]  // => #[3, 2, 1]
```

Reversing:

```beamtalk
#[1, 2, 3] reversed  // => #[3, 2, 1]
```

## Iteration

`do:` — side-effects only (returns nil):

```beamtalk
#[1, 2, 3] do: [:x | Transcript showLine: x printString]  // => _
```

`collect:` — transform each element (returns new array):

```beamtalk
#[1, 2, 3] collect: [:x | x * 2]                   // => #[2, 4, 6]
#["hello", "world"] collect: [:s | s uppercase]     // => #["HELLO", "WORLD"]
```

`select:` — keep elements matching a predicate:

```beamtalk
#[1, 2, 3, 4, 5] select: [:x | x > 3]   // => #[4, 5]
#[1, 2, 3, 4, 5] select: [:x | x even]  // => #[2, 4]
```

`reject:` — opposite of `select:`:

```beamtalk
#[1, 2, 3, 4, 5] reject: [:x | x even]  // => #[1, 3, 5]
```

`detect:` — find the first matching element (error if none):

```beamtalk
#[1, 2, 3, 4, 5] detect: [:x | x > 3]  // => 4
```

`detect:ifNone:` — safe version:

```beamtalk
#[1, 2, 3] detect: [:x | x > 10] ifNone: [nil]  // => nil
```

`inject:into:` — fold/reduce:

```beamtalk
#[1, 2, 3, 4, 5] inject: 0 into: [:sum :x | sum + x]     // => 15
#[1, 2, 3, 4] inject: 1 into: [:product :x | product * x]  // => 24
```

`allSatisfy:` — true if all elements match:

```beamtalk
#[2, 4, 6] allSatisfy: [:x | x even]  // => true
#[2, 3, 6] allSatisfy: [:x | x even]  // => false
```

`anySatisfy:` — true if any element matches:

```beamtalk
#[1, 3, 4] anySatisfy: [:x | x even]  // => true
#[1, 3, 5] anySatisfy: [:x | x even]  // => false
```

## Dictionary

Literal syntax: `#{key => value, key => value}`

Keys are typically symbols but can be any value.

```beamtalk
d := #{#name => "Alice", #age => 30}  // => _
```

Access by key:

```beamtalk
d at: #name  // => Alice
d at: #age   // => 30
```

Safe access with default:

```beamtalk
d at: #missing ifAbsent: ["default"]  // => default
```

Check if a key exists:

```beamtalk
d includesKey: #name     // => true
d includesKey: #missing  // => false
```

Adding/updating (returns new dictionary):

```beamtalk
d2 := d at: #city put: "Dublin"  // => _
d2 at: #city                     // => Dublin
d includesKey: #city             // => false
```

All keys and values:

```beamtalk
d keys    // => _
d values  // => _
d size    // => 2
```

Iteration:

```beamtalk
d keysAndValuesDo: [:k :v | Transcript showLine: "{k}: {v}"]  // => _
```

## Bag (multiset)

A Bag is an unordered collection that allows duplicate elements.

```beamtalk
b := Bag withAll: #[1, 2, 1, 3, 1]  // => _
b size                               // => 5
b occurrencesOf: 1                   // => 3
b occurrencesOf: 2                   // => 1
b occurrencesOf: 99                  // => 0
b includes: 2                        // => true
```

Add an element (returns new Bag):

```beamtalk
b2 := b add: 2        // => _
b2 occurrencesOf: 2   // => 2
b occurrencesOf: 2    // => 1        (original unchanged)
```

## Set

A Set is an unordered collection with no duplicates.

```beamtalk
s := Set withAll: #[1, 2, 1, 3, 2, 1]  // => _
s size                                   // => 3        (only unique elements)
s includes: 1                            // => true
s includes: 99                           // => false
```

Adding to a Set (returns new Set):

```beamtalk
s2 := s add: 4  // => _
s2 includes: 4  // => true
```

Adding a duplicate is a no-op:

```beamtalk
s3 := s add: 1  // => _
s3 size         // => 3
```

## Converting between collection types

Array to Set (removes duplicates):

```beamtalk
#[1, 2, 1, 3] asSet size  // => 3
```

Array to Bag:

```beamtalk
#[1, 1, 2] asBag occurrencesOf: 1  // => 2
```

Set to Array (order not guaranteed):

```beamtalk
Set withAll: #[3, 1, 2] asArray size  // => 3
```

## Ranges

Ranges are produced by `to:` and support iteration (chapter 8), but can
also be collected into arrays:

```beamtalk
(1 to: 5) asArray        // => #[1, 2, 3, 4, 5]
(1 to: 10 by: 2) asArray  // => #[1, 3, 5, 7, 9]
```

## Summary

**Array:** `#[1, 2, 3]`
- `at:`, `at:put:`, `includes:`, `add:`, `first`, `last`, `reversed`, `sort`, `size`
- `do:`, `collect:`, `select:`, `reject:`, `detect:`, `inject:into:`
- `allSatisfy:`, `anySatisfy:`

**Dictionary:** `#{#k => v}`
- `at:`, `at:ifAbsent:`, `at:put:`, `includesKey:`, `keys`, `values`, `size`
- `keysAndValuesDo:`

**Bag:** `Bag withAll: #[...]`
- `occurrencesOf:`, `includes:`, `add:`, `size`

**Set:** `Set withAll: #[...]`
- `includes:`, `add:`, `size`

Next: Chapter 10 — Value Classes
