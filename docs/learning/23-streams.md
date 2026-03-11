## Streams

Streams provide **lazy evaluation** for processing sequences. Unlike arrays,
which compute everything up front, streams only produce values when you ask
for them — making them ideal for large datasets, pipelines, and even infinite
sequences.

## Creating streams

### From a starting value

`Stream from:` creates an infinite stream of consecutive integers:

```beamtalk
s := Stream from: 1  // => _
s take: 5             // => [1,2,3,4,5]
```

### With a custom step

`Stream from:by:` lets you control how each value is produced from the last:

```beamtalk
evens := Stream from: 0 by: [:n | n + 2]  // => _
evens take: 5                               // => [0,2,4,6,8]
```

```beamtalk
powers := Stream from: 1 by: [:n | n * 2]  // => _
powers take: 6                               // => [1,2,4,8,16,32]
```

### From a collection

`Stream on:` wraps an existing collection as a stream:

```beamtalk
s := Stream on: #(10, 20, 30)  // => _
s asList                        // => [10,20,30]
```

Collections also respond to `stream`:

```beamtalk
s := #(1, 2, 3) stream  // => _
s class                  // => Stream
s asList                 // => [1,2,3]
```

## Lazy pipeline operations

Stream operations are **lazy** — they return a new stream without evaluating
anything. Values are only computed when a terminal operation forces them.

### Transforming with collect:

```beamtalk
squares := (Stream from: 1) collect: [:n | n * n]  // => _
squares take: 5                                      // => [1,4,9,16,25]
```

### Filtering with select: and reject:

```beamtalk
odds := (Stream from: 1) select: [:n | n isOdd]  // => _
odds take: 5                                       // => [1,3,5,7,9]
```

```beamtalk
noThrees := (Stream from: 1) reject: [:n | (n % 3) =:= 0]  // => _
noThrees take: 5                                              // => [1,2,4,5,7]
```

### Skipping with drop:

```beamtalk
s := (Stream from: 1) drop: 3  // => _
s take: 4                       // => [4,5,6,7]
```

### Chaining operations

Pipeline operations compose naturally — each returns a new stream:

```beamtalk
result := ((Stream from: 1) select: [:n | n isEven]) collect: [:n | n * n]  // => _
result take: 4  // => [4,16,36,64]
```

## Terminal operations

Terminal operations force evaluation and return a concrete value.

### take: — materialize the first N elements

`take:` returns a List:

```beamtalk
(Stream from: 10) take: 3  // => [10,11,12]
```

### asList — materialize all elements

Only use `asList` on finite streams (from collections):

```beamtalk
(Stream on: #(1, 2, 3)) asList  // => [1,2,3]
```

### inject:into: — fold/reduce

```beamtalk
sum := (Stream on: #(1, 2, 3, 4, 5)) inject: 0 into: [:acc :n | acc + n]  // => _
sum  // => 15
```

### detect: — find the first match

```beamtalk
(Stream from: 1) detect: [:n | n > 10]  // => 11
```

```beamtalk
(Stream on: #(1, 2, 3)) detect: [:n | n > 100]  // => nil
```

### anySatisfy: and allSatisfy:

```beamtalk
(Stream on: #(1, 2, 3, 4)) anySatisfy: [:n | n > 3]  // => true
(Stream on: #(1, 2, 3, 4)) allSatisfy: [:n | n > 0]  // => true
(Stream on: #(1, 2, 3, 4)) allSatisfy: [:n | n > 2]  // => false
```

### do: — iterate with side effects

```beamtalk
count := 0                                              // => _
(Stream on: #(1, 2, 3)) do: [:n | count := count + n]  // => _
count                                                    // => 6
```

## Infinite streams

Because streams are lazy, you can work with infinite sequences safely.
Just make sure to limit output with `take:` or `detect:`:

```beamtalk
fibs := Stream from: #(1, 1) by: [:pair | #(pair at: 2, (pair at: 1) + (pair at: 2))]  // => _
first10 := (fibs take: 10) collect: [:pair | pair at: 1]  // => _
first10  // => [1,1,2,3,5,8,13,21,34,55]
```

## Collection interop

Streams integrate with Beamtalk's collection protocol. Convert any collection
to a stream and back:

```beamtalk
original := #(10, 20, 30, 40, 50)                        // => _
filtered := (original stream select: [:n | n > 20]) asList  // => _
filtered  // => [30,40,50]
```

```beamtalk
"hello" stream class  // => Stream
```

## Empty streams

All operations handle empty streams gracefully:

```beamtalk
empty := Stream on: #()                     // => _
empty asList                                 // => []
(empty select: [:n | true]) asList           // => []
(empty collect: [:n | n * 2]) asList         // => []
empty detect: [:n | true]                    // => nil
empty anySatisfy: [:n | true]                // => false
empty allSatisfy: [:n | true]                // => true
```

## Summary

**Construction:**

```text
Stream from: start                 → infinite stream (start, start+1, ...)
Stream from: start by: [:n | ...]  → infinite stream with custom step
Stream on: collection              → finite stream from collection
collection stream                  → finite stream (List, String, Set)
```

**Lazy pipeline (return a new Stream):**

```text
stream collect: [:n | ...]   → transform each element
stream select: [:n | ...]    → keep matching elements
stream reject: [:n | ...]    → remove matching elements
stream drop: n               → skip first n elements
```

**Terminal operations (force evaluation):**

```text
stream take: n                          → List of first n elements
stream asList                           → List of all elements
stream inject: init into: [:acc :n | …] → folded value
stream detect: [:n | ...]               → first match or nil
stream anySatisfy: [:n | ...]           → Boolean
stream allSatisfy: [:n | ...]           → Boolean
stream do: [:n | ...]                   → nil (side effects)
```

Next: Chapter 24 — Reflection & Metaprogramming
