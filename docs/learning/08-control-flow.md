## Control Flow

Beamtalk has no `if`, `while`, or `for` keywords. Control flow is expressed
through messages sent to booleans, numbers, and blocks. This is pure
Smalltalk heritage — and it's more powerful than it first appears.

## Conditionals: ifTrue: ifFalse:

Send `ifTrue:` to a boolean with a block. The block runs if the boolean is true.

```beamtalk
true ifTrue: ["yes"]   // => yes
false ifTrue: ["yes"]  // => false       (ifTrue: returns the receiver when the condition is false)
```

`ifFalse:` — runs the block when false:

```beamtalk
false ifFalse: ["no"]  // => no
true ifFalse: ["no"]   // => true
```

`ifTrue:ifFalse:` — like if/else:

```beamtalk
true ifTrue: ["yes"] ifFalse: ["no"]   // => yes
false ifTrue: ["yes"] ifFalse: ["no"]  // => no
```

`ifFalse:ifTrue:` — reversed (rare but valid):

```beamtalk
false ifFalse: ["no"] ifTrue: ["yes"]  // => no
```

The result of `ifTrue:ifFalse:` is the value of whichever block ran:

```beamtalk
score := 85   // => _
grade := score >= 90
  ifTrue: ["A"]
  ifFalse: [score >= 80
    ifTrue: ["B"]
    ifFalse: ["C"]]
// => _
grade  // => B
```

## Nil-aware conditionals

```beamtalk
nil ifNil: ["nothing here"]                                   // => nothing here
nil ifNil: ["nothing"] ifNotNil: [:v | "got {v}"]            // => nothing
42 ifNil: ["nothing"] ifNotNil: [:v | "got {v}"]             // => got 42
42 ifNotNil: [:v | v * 2]                                    // => 84
nil ifNotNil: [:v | v * 2]                                   // => nil
```

## whileTrue: / whileFalse:

Send `whileTrue:` to a condition block (a block that returns a boolean).
The body block runs repeatedly while the condition is true.

```beamtalk
i := 0                          // => _
[i < 5] whileTrue: [i := i + 1]  // => _
i                               // => 5
```

`whileFalse:` — runs the body until the condition becomes true:

```beamtalk
j := 0                             // => _
[j >= 3] whileFalse: [j := j + 1]   // => _
j                                  // => 3
```

Building a result inside `whileTrue:`:

```beamtalk
result := 0  // => _
k := 1       // => _

[k <= 10] whileTrue: [
  result := result + k
  k := k + 1
]
// => _

result  // => 55       (sum 1..10)
```

## timesRepeat:

Run a block a fixed number of times:

```beamtalk
count := 0                         // => _
5 timesRepeat: [count := count + 1]  // => _
count                              // => 5
```

## to:do: — numeric iteration

Iterate over a range of integers (inclusive at both ends):

```beamtalk
sum := 0                               // => _
1 to: 5 do: [:n | sum := sum + n]     // => _
sum                                    // => 15
```

`to:by:do:` — with a custom step:

```beamtalk
evens := 0                                 // => _
2 to: 10 by: 2 do: [:n | evens := evens + n]  // => _
evens                                      // => 30       (2+4+6+8+10)
```

Counting down (negative step):

```beamtalk
countdown := 0                                    // => _
5 to: 1 by: -1 do: [:n | countdown := countdown + n]  // => _
countdown                                         // => 15       (5+4+3+2+1)
```

`to:collect:` — build an array from a range:

```beamtalk
squares := (1 to: 5) collect: [:n | n * n]  // => _
squares                                      // => #[1, 4, 9, 16, 25]
```

## and: / or: — short-circuit boolean logic

`and:` and `or:` are keyword messages that take blocks. The block is only
evaluated if needed (short-circuit evaluation).

```beamtalk
x := 5            // => _
x > 0 and: [x < 10]  // => true
x > 0 or: [x < 0]    // => true
```

Without short-circuit (the block is not evaluated when not needed):

```beamtalk
false and: [1/0]   // => false   (the division block never runs)
true or: [1/0]     // => true    (the division block never runs)
```

## Case-like dispatch: caseOf:

Use `caseOf:` for switch/case-style dispatch on a value:

```beamtalk
day := #monday  // => _

day caseOf: {
  [#monday]    -> ["Start of work week"],
  [#friday]    -> ["End of work week"],
  [#saturday]  -> ["Weekend!"],
  [#sunday]    -> ["Weekend!"]
} otherwise: ["Midweek"]
// => Start of work week

#wednesday caseOf: {
  [#monday] -> ["Monday"],
  [#friday] -> ["Friday"]
} otherwise: ["Other day"]
// => Other day
```

## Early exit with `^`

Inside a method, `^` returns from the method immediately.
At the top level of a script, `^` returns the value from the current expression.

A common pattern: validate inputs and return early:

```beamtalk
// processOrder: order =>
//   order isNil ifTrue: [^"error: nil order"]
//   order isEmpty ifTrue: [^"error: empty order"]
//   // ... normal processing
//   "ok"
```

Inside a block (inside a method), `^` exits the *method*, not just the block:

```beamtalk
// firstPositive: items =>
//   items do: [:each |
//     each > 0 ifTrue: [^each]]
//   nil
```

## Summary

Conditionals:

```
bool ifTrue: [...]
bool ifFalse: [...]
bool ifTrue: [...] ifFalse: [...]
val ifNil: [...] ifNotNil: [:v | ...]
```

Loops:

```
[condition] whileTrue: [body]
[condition] whileFalse: [body]
n timesRepeat: [body]
start to: end do: [:i | body]
start to: end by: step do: [:i | body]
```

Logic:

```
bool and: [...]   (short-circuit)
bool or: [...]    (short-circuit)
```

Dispatch:

```
val caseOf: {...} otherwise: [...]
```

Next: Chapter 9 — Collections
