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
"Literal: \{name\}"  // => Literal: \{name\}
```

## Size and indexing

`size` / `length` count grapheme clusters (visible characters):

```beamtalk
"hello" size  // => 5
"café" size   // => 4        (not 5 bytes)
"🌍" size     // => 1        (one emoji grapheme, even though it's multiple bytes)
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

Join an array of strings with a separator:

```beamtalk
#["a", "b", "c"] join: ", "        // => a, b, c
#["Hello", " ", "World"] join: ""  // => Hello World
```

Repeat a string:

```beamtalk
"ab" * 3  // => ababab
```

## Searching and testing

```beamtalk
"hello world" includesSubstring: "world"  // => true
"hello world" includesSubstring: "xyz"    // => false
"hello" startsWith: "hel"                 // => true
"hello" endsWith: "llo"                   // => true
"hello" indexOf: $l                       // => 3        (first occurrence, 1-based)
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
"  hello  " trimSeparators   // => hello
"  hello  " withBlanksTrimmed  // => hello
```

## Splitting

```beamtalk
"a,b,c" substrings: ","          // => #[a, b, c]
"hello world foo" substrings: " "  // => #[hello, world, foo]
```

Split on any whitespace:

```beamtalk
"hello   world" substrings  // => #[hello, world]
```

## Replacing

```beamtalk
"hello world" copyReplaceAll: "world" with: "Beamtalk"  // => hello Beamtalk
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

Symbol to/from string:

```beamtalk
#hello asString   // => hello
"hello" asSymbol  // => hello
```

## Character access

Iterate over characters with `each:` or `do:`:

```beamtalk
count := 0                             // => _
"hello" do: [:ch | count := count + 1]  // => _
count                                  // => 5
```

Collect characters into a new collection:

```beamtalk
"hello" collect: [:ch | ch uppercase]  // => HELLO
```

## Multiline strings

There is no multi-line string literal syntax. Use `++` to concatenate lines:

```beamtalk
multiline := "line 1" ++ "\n" ++ "line 2" ++ "\n" ++ "line 3"  // => _
multiline size  // => 20
```

## Summary

- Literals: `"double quotes only"`
- Interpolation: `"Hello, {name}!"`
- Concatenation: `"a" ++ "b"`
- Size: `str size` (grapheme clusters)
- Search: `includesSubstring:`, `startsWith:`, `endsWith:`, `indexOf:`
- Transform: `uppercase`, `lowercase`, `trimSeparators`, `copyReplaceAll:with:`
- Split/Join: `substrings:`, `join:`
- Convert: `asInteger`, `asFloat`, `asSymbol`, `printString`

Next: Chapter 7 — Blocks
