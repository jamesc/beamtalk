## JSON

Beamtalk's `Json` class handles parsing and generation of JSON data.

## Parsing primitives

`Json parse:` returns a `Result` — unwrap it to get the value:

```beamtalk
(Json parse: "42") unwrap     // => 42
(Json parse: "3.14") unwrap   // => 3.14
(Json parse: "true") unwrap   // => true
(Json parse: "false") unwrap  // => false
(Json parse: "null") unwrap   // => nil
```

## Type mapping

JSON types map to Beamtalk types:

| JSON | Beamtalk |
|------|----------|
| `42` | Integer |
| `3.14` | Float |
| `true` / `false` | Boolean |
| `null` | nil |
| `"text"` | String |
| `[1, 2]` | List |
| `{"k": "v"}` | Dictionary |

```beamtalk
(Json parse: "42") unwrap class    // => Integer
(Json parse: "3.14") unwrap class  // => Float
```

## Arrays

JSON arrays become Lists:

```beamtalk
list := (Json parse: "[1, 2, 3]") unwrap  // => _
list size                                  // => 3
list at: 1                                 // => 1
list at: 3                                 // => 3
```

```beamtalk
(Json parse: "[]") unwrap size  // => 0
```

## Objects and nested structures

JSON objects become Dictionaries with string keys. Since Beamtalk uses
`{...}` for string interpolation, build JSON objects using `Json generate:`
from a Dictionary:

```beamtalk
json := Json generate: #{"name" => "Alice", "age" => 30}  // => _
json class                                                  // => String
obj := (Json parse: json) unwrap                            // => _
obj at: "name"                                              // => Alice
obj at: "age"                                               // => 30
obj size                                                    // => 2
```

Nested structures work naturally:

```beamtalk
inner := #{"name" => "Bob", "scores" => #(10, 20, 30)}  // => _
json := Json generate: #{"user" => inner}                 // => _
data := (Json parse: json) unwrap                         // => _
user := data at: "user"                                    // => _
user at: "name"                                            // => Bob
scores := user at: "scores"                                // => _
scores size                                                // => 3
scores at: 2                                               // => 20
```

## Generating JSON

`Json generate:` converts Beamtalk values to JSON strings:

```beamtalk
Json generate: 42       // => 42
Json generate: "hello"  // => "hello"
Json generate: true     // => true
Json generate: nil      // => null
```

Collections:

```beamtalk
Json generate: #(1, 2, 3)  // => [1,2,3]
Json generate: #()         // => []
```

## Pretty printing

`Json prettyPrint:` adds indentation for readability:

```beamtalk
pretty := Json prettyPrint: #(1, 2, 3)  // => _
pretty class                             // => String
```

## Round-tripping

Parse and generate are inverses for simple values:

```beamtalk
(Json parse: (Json generate: 42)) unwrap              // => 42
(Json parse: (Json generate: "hello")) unwrap          // => hello
(Json parse: (Json generate: #(1, 2, 3))) unwrap size  // => 3
```

## Error handling

Invalid JSON returns an error Result:

```beamtalk
result := Json parse: "not valid json"  // => _
result isError                          // => true
result isOk                             // => false
```

```beamtalk
(Json parse: "42") isOk  // => true
```

## Summary

**Parsing:**

```text
Json parse: jsonString   → Result<value>
result unwrap            → parsed value (raises on error)
result isOk / isError    → Boolean
```

**Generating:**

```text
Json generate: value       → JSON String
Json prettyPrint: value    → pretty-printed JSON String
```

**Type mapping:** Integer, Float, Boolean, nil, String, List, Dictionary.

Next: Chapter 21 — DateTime & Time
