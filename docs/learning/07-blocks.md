## Blocks

A block is an anonymous function — a chunk of code you can store in a
variable, pass as an argument, and evaluate later. Blocks are how Beamtalk
implements control flow, iteration, and callbacks.

Block syntax:

```
[body]           — zero arguments
[:x | body]      — one argument
[:x :y | body]   — two arguments
```

## Basic blocks

A block is an object. Create it with `[]`, evaluate it with `value`:

```beamtalk
[42] value      // => 42
[3 + 4] value   // => 7
```

A block with one argument:

```beamtalk
[:x | x + 1] value: 5  // => 6
```

Two arguments:

```beamtalk
[:x :y | x + y] value: 3 value: 4  // => 7
```

Three arguments:

```beamtalk
[:x :y :z | x + y + z] value: 1 value: 2 value: 3  // => 6
```

Blocks can contain multiple statements — the last expression is returned.
Here two statements are chained: first `x * 2` is assigned to `doubled`,
then `doubled + 1` is the return value:

```beamtalk
[:x | doubled := x * 2. doubled + 1] value: 5  // => 11
```

## Storing blocks in variables

```beamtalk
add := [:x :y | x + y]  // => _
add value: 10 value: 20  // => 30

square := [:n | n * n]   // => _
square value: 7          // => 49
```

## Variable capture (closures)

Blocks close over their enclosing scope. Variables defined outside the block
are captured by reference.

```beamtalk
base := 100                    // => _
addBase := [:n | n + base]     // => _
addBase value: 5               // => 105
addBase value: 42              // => 142
```

The block sees the current value of captured variables:

```beamtalk
factor := 2                    // => _
multiply := [:n | n * factor]  // => _
multiply value: 5              // => 10
```

## Higher-order blocks (blocks that return blocks)

A block can return another block. This is how you build "factories":

```beamtalk
makeAdder := [:n | [:x | x + n]]  // => _
addFive := makeAdder value: 5      // => _
addFive value: 10                  // => 15
addFive value: 3                   // => 8

addTen := makeAdder value: 10      // => _
addTen value: 7                    // => 17
```

## valueWithArguments:

Pass arguments as an array when you have them in a collection:

```beamtalk
[:x :y | x + y] valueWithArguments: #(3, 4)  // => 7
[:x | x * 2] valueWithArguments: #(5)         // => 10
[42] valueWithArguments: #()                   // => 42
```

## Block arity

`arity` returns the number of arguments a block expects:

```beamtalk
[42] arity           // => 0
[:x | x] arity       // => 1
[:x :y | x + y] arity  // => 2
[:a :b :c | a] arity   // => 3
```

## Blocks and control flow

Control flow in Beamtalk is implemented via messages that take blocks.
This is covered fully in Chapter 8, but here's a preview.

`ifTrue:` evaluates the block if the receiver is true:

```beamtalk
true ifTrue: [42]   // => 42
false ifTrue: [42]  // => false
```

`whileTrue:` evaluates the body block while the condition block is true:

```beamtalk
i := 0                          // => _
[i < 3] whileTrue: [i := i + 1]  // => _
i                               // => 3
```

## Non-local returns with `^`

Inside a block, `^` is a **non-local return** — it exits the *enclosing method*,
not just the block. This enables clean early-exit patterns.

Example (this works correctly inside a method):

```beamtalk
// firstPositive: items =>
//   items do: [:x | x > 0 ifTrue: [^x]]
//   nil
```

When `^` fires inside the block, the *method* returns immediately.

In Beamtalk, `^` inside a block is a **non-local return** — it exits the *enclosing method* (or activation), not just the block. In these learning guide scripts the enclosing activation is the script runner itself, so the non-local return still yields:

```beamtalk
[:x | ^x + 10] value: 5  // => 15
```

Inside a class method, `^` inside a block exits the entire method immediately — useful for early returns but can cause surprises if the block outlives the method.

## whileFalse: / whileTrue:

```beamtalk
n := 0                            // => _
[n >= 5] whileFalse: [n := n + 1]  // => _
n                                 // => 5
```

`whileTrue:` (already seen above):

```beamtalk
counter := 10                              // => _
[counter > 0] whileTrue: [counter := counter - 3]  // => _
counter                                    // => -2
```

## timesRepeat:

```beamtalk
total := 0                         // => _
5 timesRepeat: [total := total + 1]  // => _
total                              // => 5
```

## Summary

- Create: `[body]`   `[:x | body]`   `[:x :y | body]`
- Evaluate: `block value`, `block value: arg`, `block value: arg1 value: arg2`, `block valueWithArguments: #(args)`
- Inspect: `block arity`
- Capture: blocks close over outer variables
- Non-local `^`: exits the *enclosing method* (not just the block)

## Exercises

**1. Max block.** Write a block that takes two numbers and returns the larger one.
Test it with `value: 3 value: 7` and `value: 10 value: 2`.

<details>
<summary>Hint</summary>

```text
max := [:a :b | (a > b) ifTrue: [a] ifFalse: [b]]
max value: 3 value: 7     // => 7
max value: 10 value: 2    // => 10
```
</details>

**2. Block factory.** Create a `makeMultiplier` block that takes a factor and
returns a new block that multiplies its argument by that factor. Use it to
create a tripler and a doubler.

<details>
<summary>Hint</summary>

```text
makeMultiplier := [:factor | [:n | n * factor]]
triple := makeMultiplier value: 3
triple value: 5     // => 15
double := makeMultiplier value: 2
double value: 7     // => 14
```

The inner block captures `factor` from the outer block's scope.
</details>

**3. Arity exploration.** Create blocks with 0, 1, 2, and 3 arguments. Verify
each one's `arity`, then call the 3-argument block using `valueWithArguments:`.

<details>
<summary>Hint</summary>

```text
[42] arity                                                // => 0
[:x | x] arity                                            // => 1
[:x :y | x + y] arity                                     // => 2
[:x :y :z | x + y + z] arity                              // => 3
[:x :y :z | x + y + z] valueWithArguments: #(10, 20, 30)  // => 60
```
</details>

Next: Chapter 8 — Control Flow
