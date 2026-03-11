## Regular Expressions

Beamtalk provides regex support through both `String` methods and the `Regex` class.
Most operations accept either a pattern string or a compiled `Regex` object.

## Matching

`matchesRegex:` tests whether a string matches a pattern:

```beamtalk
"hello world" matchesRegex: "hello"      // => true
"hello world" matchesRegex: "^world"     // => false
"hello world" matchesRegex: "world$"     // => true
"abc123" matchesRegex: "^[a-z]+[0-9]+$"  // => true
```

## Finding matches

`firstMatch:` returns the first match, or `nil` if none:

```beamtalk
"hello world" firstMatch: "[aeiou]"       // => e
"hello world" firstMatch: "[0-9]+"        // => nil
"abc 123 def 456" firstMatch: "[0-9]+"    // => 123
```

`allMatches:` returns all matches as a list:

```beamtalk
matches := "cat and cat" allMatches: "cat"  // => _
matches size                                // => 2
```

```beamtalk
vowels := "hello" allMatches: "[aeiou]"  // => _
vowels size                              // => 2
```

```beamtalk
noMatch := "xyz" allMatches: "[0-9]"  // => _
noMatch size                          // => 0
```

## Replacement

`replaceRegex:with:` replaces the **first** match:

```beamtalk
"hello world" replaceRegex: "[aeiou]" with: "*"  // => h*llo world
```

`replaceAllRegex:with:` replaces **all** matches:

```beamtalk
"hello world" replaceAllRegex: "[aeiou]" with: "*"  // => h*ll* w*rld
```

## Splitting

`splitRegex:` splits a string on a pattern:

```beamtalk
parts := "one,two,,three" splitRegex: ","  // => _
parts size                                 // => 4
```

```beamtalk
words := "hello   world" splitRegex: " +"  // => _
words size                                 // => 2
```

## Compiled regex objects

For repeated use, compile a pattern once with `Regex from:`:

```beamtalk
r := (Regex from: "[0-9]+") unwrap  // => _
"abc 123 def" firstMatch: r         // => 123
"no digits" matchesRegex: r         // => false
"99 bottles" matchesRegex: r        // => true
```

Access the original pattern:

```beamtalk
r := (Regex from: "[a-z]+") unwrap  // => _
r source                            // => [a-z]+
```

## Case-insensitive matching

Pass `#(#caseless)` as options:

```beamtalk
"Hello" matchesRegex: "hello"                     // => false
"Hello" matchesRegex: "hello" options: #(#caseless)  // => true
```

Compiled with options:

```beamtalk
r := (Regex from: "hello" options: #(#caseless)) unwrap  // => _
"HELLO WORLD" matchesRegex: r                            // => true
"HELLO WORLD" firstMatch: r                              // => HELLO
```

## Error handling

Invalid patterns return an error `Result`:

```beamtalk
bad := Regex from: "[invalid"  // => _
bad isError                    // => true
```

## Summary

**Matching:**

```text
string matchesRegex: pattern                    → Boolean
string matchesRegex: pattern options: #(#caseless)  → Boolean
```

**Finding:**

```text
string firstMatch: pattern    → String or nil
string allMatches: pattern    → List of Strings
```

**Replacing:**

```text
string replaceRegex: pattern with: replacement      → String (first)
string replaceAllRegex: pattern with: replacement   → String (all)
```

**Splitting:**

```text
string splitRegex: pattern    → List of Strings
```

**Compiled regex:**

```text
Regex from: pattern                       → Result<Regex>
Regex from: pattern options: #(#caseless) → Result<Regex>
regex source                              → String
```

Next: Chapter 20 — JSON
