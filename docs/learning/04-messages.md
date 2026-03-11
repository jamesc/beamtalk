## Messages

This is the most important chapter. In Beamtalk, *everything* you do is
a message send. There is no special syntax for method calls, operators,
or conditionals — they are all messages.

A message send has a receiver (the object), a selector (the message name),
and optionally arguments.

```
receiver selector
receiver selector: argument
receiver selector: arg1 anotherPart: arg2
```

There are three kinds of messages, in order of precedence (high to low):

1. **Unary** — no arguments
2. **Binary** — one argument, symbolic name
3. **Keyword** — one or more arguments, colon-suffixed name parts

## Unary messages

A unary message has no arguments. It's just a word sent to an object.

```beamtalk
42 class      // => Integer
42 isZero     // => false
-5 abs        // => 5
"hello" size  // => 5
"hello" reverse  // => olleh
"hello" uppercase  // => HELLO
true not      // => false
```

You can chain unary messages — they evaluate left to right:

```beamtalk
-5 abs negated  // => -5
```

Read as: `(-5 abs) negated` = `5 negated` = `-5`

## Binary messages

A binary message has exactly one argument and uses a symbolic name:
`+`, `-`, `*`, `/`, `++`, `=`, `<`, `>`, etc.

```beamtalk
3 + 4         // => 7
10 - 3        // => 7
"Hello" ++ " World"  // => Hello World
3 < 5         // => true
```

Binary messages evaluate left-to-right when at the same precedence level,
but within binary messages Beamtalk uses **standard math precedence**
(unlike classic Smalltalk which is pure left-to-right).

This is the biggest syntactic departure from Smalltalk:

```beamtalk
2 + 3 * 4  // => 14
```

In classic Smalltalk, this would be 20 (strict left-to-right).
In Beamtalk, it's 14 — standard math wins.

Precedence order (high to low within binary):

```
**   (exponentiation, right-associative)
* / %
+ - ++
< > <= >=
= == =:= /= =/=
```

```beamtalk
2 ** 10      // => 1024
10 / 2 + 1   // => 6.0
```

Use parentheses to override:

```beamtalk
(2 + 3) * 4  // => 20
```

## Keyword messages

A keyword message has one or more arguments. Each argument is preceded by a
keyword — a word ending with a colon.

```
receiver keyword: argument
receiver keyword: arg1 anotherKeyword: arg2
```

Single keyword:

```beamtalk
"hello world" includesSubstring: "world"  // => true
```

Two keywords (they form a single message selector):

```beamtalk
d := #{#a => 1, #b => 2}   // => _
d at: #a                    // => 1
d at: #c ifAbsent: [99]     // => 99
```

More examples:

```beamtalk
#[1, 2, 3, 4, 5] inject: 0 into: [:sum :each | sum + each]  // => 15
"a,b,c" split: ","                                           // => ["a","b","c"]
```

## Message precedence: the full picture

When expressions mix message types, precedence determines order:

1. Unary (highest)
2. Binary
3. Keyword (lowest)

This means unary messages bind tighter than binary, which bind tighter
than keyword.

Unary before binary:

```beamtalk
2 + 3 factorial  // => 8
```

Binary before keyword — the keyword message receives the result of the binary expression:

```beamtalk
#[1, 2, 3] inject: 0 into: [:sum :each | sum + each]  // => 6
```

A common source of confusion — keyword messages have lowest precedence.
This sends `at: (3 + 4)`, not `(d at: 3) + 4`:

```beamtalk
d2 := #{3 => "three", 7 => "seven"}  // => _
d2 at: 3 + 4                          // => seven
```

If you want binary to happen after keyword, use parentheses:

```beamtalk
(d2 at: 3) ++ " and more"  // => three and more
```

## Cascades

A cascade sends multiple messages to the *same receiver* using semicolons.
The result of a cascade is the result of the last message.

With cascade (all go to the same array):

```beamtalk
a := #[1, 2, 3]               // => _
a includes: 1; includes: 2    // => true
```

Cascade is especially useful for building up state (shown more in chapter 9 with collections).

## Parentheses

Parentheses always override precedence and change the order of evaluation.
When in doubt, add parentheses.

```beamtalk
3 + (4 * 2)      // => 11
(3 + 4) * 2      // => 14
(2 + 3) * (4 + 1)  // => 25
```

## doesNotUnderstand

If you send a message an object doesn't understand, it raises a
`doesNotUnderstand` error (DNU). This is Beamtalk's equivalent of
"method not found" in other languages.

```beamtalk
42 unknownMessage  // => ERROR:does_not_understand
```

You can check if an object understands a message before sending it:

```beamtalk
42 respondsTo: #isZero        // => true
42 respondsTo: #unknownMessage  // => false
```

## Summary

Three kinds of messages:

```
Unary    receiver msg                   highest precedence
Binary   receiver op arg
Keyword  receiver key: arg key2: arg2   lowest precedence
```

Precedence (left to right, then by kind): Unary > Binary (math precedence within) > Keyword

Use parentheses to override.
Use semicolons for cascade (same receiver, multiple messages).

## Exercises

**1. Precedence puzzle.** What does `2 + 3 * 4 factorial` evaluate to? Work
through the precedence rules (unary > binary) before running it.

<details>
<summary>Hint</summary>

Unary binds tightest: `4 factorial` → `24`. Then binary with math precedence:
`3 * 24` → `72`, then `2 + 72` → `74`.
</details>

**2. Keyword precedence.** Given `d := #{7 => "seven", 3 => "three"}`, what does
`d at: 3 + 4` return? How would you change it to get `"three"` concatenated
with `" and more"`?

<details>
<summary>Hint</summary>

`d at: 3 + 4` evaluates the binary `3 + 4` first (→ `7`), then sends
`at: 7` to `d`, returning `"seven"`. To get `"three and more"`, use
parentheses: `(d at: 3) ++ " and more"`.
</details>

**3. Cascade practice.** Create an array `#[1, 2, 3]` and use cascade (`;`) to
check `includes: 1`, `includes: 2`, and `includes: 5` on the same receiver.
What is the result of the cascade expression?

<details>
<summary>Hint</summary>

```text
#[1, 2, 3] includes: 1; includes: 2; includes: 5
```

The result of a cascade is the last message's result: `false` (since 5 is
not in the array).
</details>

Next: Chapter 5 — Arithmetic & Comparison
