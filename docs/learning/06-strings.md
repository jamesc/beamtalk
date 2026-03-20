## Strings

Beamtalk strings are UTF-8 binaries. String operations are grapheme-aware:
`size` counts visible characters (grapheme clusters), not bytes.

String literals use double quotes. Single quotes are NOT string delimiters.

## String literals

```beamtalk
"Hello, World!"                  // => Hello, World!
"Unicode: café, 日本語, 🌍"        // => Unicode: café, 日本語, 🌍
"" isEmpty                       // => true
```

A literal double-quote inside a string — double it:

```beamtalk
"She said ""hello"""  // => She said "hello"
```

## String interpolation

Embed any expression inside `{}` to interpolate it.
The expression is evaluated and converted to a string via `printString`.

```beamtalk
name := "Alice"          // => _
"Hello, {name}!"         // => Hello, Alice!

x := 42                  // => _
"The answer is {x}."     // => The answer is 42.
```

Expressions, not just variables:

```beamtalk
"2 + 2 = {2 + 2}"  // => 2 + 2 = 4
```

Nested quotes work inside `{}`:

```beamtalk
items := #[1, 2, 3]           // => _
"Items: {items size} total"    // => Items: 3 total
```

Escape `{` with `\{` to suppress interpolation:

```beamtalk
"Literal: \{name\}"  // => Literal: {name}
```

## Size and indexing

`size` / `length` count grapheme clusters (visible characters):

```beamtalk
"hello" size  // => 5
"café" size   // => 4
"🌍" size     // => 1
```

Access individual characters (1-based indexing):

```beamtalk
"hello" at: 1  // => h
"hello" at: 5  // => o
"café" at: 4   // => é
```

## Concatenation and building strings

```beamtalk
"Hello" ++ ", " ++ "World!"  // => Hello, World!
```

Join a list of strings with a separator:

```beamtalk
#("a", "b", "c") join: ", "        // => a, b, c
#("Hello", " ", "World") join: ""  // => Hello World
```

Repeat a string using `*`:

```text
"ab" * 3  // => ababab
```

## Searching and testing

```beamtalk
"hello world" includesSubstring: "world"  // => true
"hello world" includesSubstring: "xyz"    // => false
"hello" startsWith: "hel"                 // => true
"hello" endsWith: "llo"                   // => true
"hello" indexOf: "l"                      // => 3
"" isEmpty                                // => true
"hello" isEmpty                           // => false
"hello" isNotEmpty                        // => true
```

## Case conversion

```beamtalk
"hello" uppercase  // => HELLO
"HELLO" lowercase  // => hello
```

Unicode-aware (German sharp-s):

```beamtalk
"straße" uppercase  // => STRASSE
```

## Trimming

```beamtalk
"  hello  " trim  // => hello
```

## Splitting

```beamtalk
"a,b,c" split: ","           // => ["a","b","c"]
"hello world foo" split: " "  // => ["hello","world","foo"]
```

Split on any whitespace:

```beamtalk
"hello   world" words  // => ["hello","world"]
```

## Replacing

```beamtalk
"hello world" replaceAll: "world" with: "Beamtalk"  // => hello Beamtalk
```

## Conversion

String to number:

```beamtalk
"42" asInteger   // => 42
"3.14" asFloat   // => 3.14
```

Number to string:

```beamtalk
42 printString    // => 42
3.14 printString  // => 3.14
```

Symbol to string:

```beamtalk
#hello asString   // => hello
```

## Character access

Collect applies a block to each character and joins the results back into a string:

```beamtalk
"hello" collect: [:ch | ch uppercase]  // => HELLO
```

## Multiline strings

There is no multi-line string literal syntax. Use `++` to concatenate lines:

```beamtalk
multiline := "line 1" ++ "\n" ++ "line 2" ++ "\n" ++ "line 3"  // => _
multiline size  // => 22
```

## Summary

- Literals: `"double quotes only"`
- Interpolation: `"Hello, {name}!"`
- Concatenation: `"a" ++ "b"`
- Size: `str size` (grapheme clusters)
- Search: `includesSubstring:`, `startsWith:`, `endsWith:`, `indexOf:`
- Transform: `uppercase`, `lowercase`, `trim`, `replaceAll:with:`
- Split/Join: `split:`, `words`, `join:`
- Convert: `asInteger`, `asFloat`, `printString`

## Exercises

**1. Self-describing string.** Write an expression that produces
`"hello has 5 characters"` using string interpolation and the `size` message.

<details>
<summary>Hint</summary>

```text
word := "hello"
"{word} has {word size} characters"
```
</details>

**2. Clean and normalize.** Take the string `"  Hello, World!  "` and produce
`"hello, world!"` — trimmed and lowercased — in a single expression chain.

<details>
<summary>Hint</summary>

`"  Hello, World!  " trim lowercase` — unary messages chain left-to-right.
</details>

**3. Split, reverse, rejoin.** Split `"one:two:three"` on `":"`, reverse the
resulting array, and join it back with `"-"`. What string do you get?

<details>
<summary>Hint</summary>

```text
("one:two:three" split: ":") reversed join: "-"
```

Result: `"three-two-one"`.
</details>

Next: Chapter 7 — Blocks
