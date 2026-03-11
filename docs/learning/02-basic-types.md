## Basic Types

Beamtalk has a small set of built-in types. Everything is an object, so
every value responds to messages.

Types covered:
- `Integer` — whole numbers
- `Float` — decimal numbers
- `String` — UTF-8 text
- `Boolean` — true and false
- `Nil` — the absence of a value
- `Symbol` — interned identifiers (`#name`)
- `Character` — a single Unicode codepoint (`$a`)

## Integers

Integers are arbitrary-precision whole numbers (backed by Erlang's bignum).

```beamtalk
42                    // => 42
-7                    // => -7
1000000000000000000   // => 1000000000000000000
```

Useful messages:

```beamtalk
42 class    // => Integer
42 isZero   // => false
0 isZero    // => true
-5 abs      // => 5
-5 negated  // => 5
5 negated   // => -5
```

Integer predicates:

```beamtalk
(3 isKindOf: Integer)  // => true
(3 isKindOf: Float)    // => false
3 isEven               // => false
4 isEven               // => true
3 isOdd                // => true
```

## Floats

Floats are 64-bit IEEE 754 doubles. Division of integers always returns a Float.

```beamtalk
3.14   // => 3.14
-2.5   // => -2.5
```

Division produces a float:

```beamtalk
10 / 4  // => 2.5
10 / 2  // => 5.0
```

Float-specific messages:

```beamtalk
3.7 floor      // => 3
3.2 ceiling    // => 4
3.7 truncated  // => 3
3.7 rounded    // => 4
3.14 class              // => Float
(3.14 isKindOf: Float)  // => true
```

## Strings

Strings are UTF-8 encoded and fully Unicode-aware. Use double quotes.

```beamtalk
"Hello, World!"  // => Hello, World!
```

String length counts grapheme clusters (user-visible characters), not bytes:

```beamtalk
"hello" size  // => 5
"café" size   // => 4
```

Concatenation with `++`:

```beamtalk
"Hello" ++ ", " ++ "World!"  // => Hello, World!
```

String interpolation with `{expression}`:

```beamtalk
name := "Alice"   // => _
"Hello, {name}!"  // => Hello, Alice!
```

Case conversion:

```beamtalk
"hello" uppercase  // => HELLO
"HELLO" lowercase  // => hello
```

Trimming whitespace:

```beamtalk
"  hello  " trim  // => hello
```

Checking contents:

```beamtalk
"hello" isEmpty              // => false
"" isEmpty                   // => true
"hello" includesSubstring: "ell"  // => true
```

Splitting and joining:

```beamtalk
"a,b,c" split: ","  // => ["a","b","c"]
```

Converting to/from other types:

```beamtalk
42 printString    // => 42
"42" asInteger    // => 42
"3.14" asFloat    // => 3.14
```

## Booleans

There are exactly two boolean values: `true` and `false`.
They respond to conditional messages — note that if/else are NOT keywords,
they are messages sent to booleans (chapter 8 covers this fully).

```beamtalk
true          // => true
false         // => false
true class    // => True
false class   // => False
```

Logical operations:

```beamtalk
true not   // => false
false not  // => true
```

`and:` and `or:` take blocks for short-circuit evaluation:

```beamtalk
true and: [false]   // => false
false and: [true]   // => false
true or: [false]    // => true
false or: [true]    // => true
```

`xor:`:

```beamtalk
true xor: false  // => true
true xor: true   // => false
```

Checking type:

```beamtalk
true isBoolean              // => true
(42 isKindOf: Boolean)      // => false
```

## Nil

`nil` represents the absence of a value. It is the only instance of `UndefinedObject`.

```beamtalk
nil           // => nil
nil class     // => UndefinedObject
nil isNil     // => true
nil notNil    // => false
42 isNil      // => false
42 notNil     // => true
```

`ifNil:` and `ifNotNil:` are common patterns for handling optional values:

```beamtalk
nil ifNil: ["was nil"] ifNotNil: [:v | "had value: {v}"]  // => was nil
42 ifNil: ["was nil"] ifNotNil: [:v | "had value: {v}"]   // => had value: 42
```

## Symbols

Symbols are interned identifiers, written with a `#` prefix.
Two symbols with the same name are the same object (identity equality).
They are commonly used as dictionary keys, method selectors, and enum-like values.

```beamtalk
#hello          // => hello
#hello class    // => Symbol
```

Symbol equality (symbols with the same name are identical, so use `=:=`):

```beamtalk
#hello =:= #hello  // => true
#hello =:= #world  // => false
```

Symbols are often used as dictionary keys:

```beamtalk
d := #{#name => "Alice", #age => 30}  // => _
d at: #name                            // => Alice
```

Converting to strings:

```beamtalk
#hello asString   // => hello
```

## Characters

A character literal is written with a `$` prefix. Characters represent a
single Unicode codepoint and are stored as integers.

```beamtalk
$A        // => 65
$a class  // => Integer
```

Character predicates work on the integer codepoint:

```beamtalk
65 isUppercase  // => true
97 isLowercase  // => true
53 isDigit      // => true
65 isLetter     // => true
```

## Type checking and conversion summary

Every object responds to `class`, `isNil`, `notNil`, `printString`, and `respondsTo:`.

```beamtalk
42 class                      // => Integer
42 printString                // => 42
42 respondsTo: #isZero        // => true
42 respondsTo: #unknownMessage  // => false
```

## Exercises

**1. Type conversion chain.** Convert the string `"123"` to an integer, multiply
it by 3, then convert the result back to a string. What do you get?

<details>
<summary>Hint</summary>

`("123" asInteger * 3) printString` gives `"369"`. Use `asInteger` to parse
and `printString` to convert back.
</details>

**2. Nil-safe access.** Write an expression using `ifNil:ifNotNil:` that returns
`"unknown"` when given `nil` and the value's class name when given a real value.
Test it with both `nil` and `42`.

<details>
<summary>Hint</summary>

`nil ifNil: ["unknown"] ifNotNil: [:v | v class printString]` returns `"unknown"`.
`42 ifNil: ["unknown"] ifNotNil: [:v | v class printString]` returns `"Integer"`.
</details>

**3. Symbol vs String.** Create a symbol `#hello` and a string `"hello"`. Are they
equal with `=:=`? What about after converting the symbol with `asString`?

<details>
<summary>Hint</summary>

`#hello =:= "hello"` is `false` — they are different types. But
`#hello asString =:= "hello"` is `true` — `asString` converts the symbol to
a string value.
</details>
