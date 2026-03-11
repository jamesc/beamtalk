<!-- btfixture: fixtures/ch14array_destructuring.bt -->
<!-- btfixture: fixtures/ch14map_destructuring.bt -->
<!-- btfixture: fixtures/ch14tuple_destructuring.bt -->
<!-- btfixture: fixtures/ch14binary_patterns.bt -->
<!-- btfixture: fixtures/ch14match_patterns.bt -->

## Pattern Matching

Beamtalk supports destructuring assignment for arrays, maps, and Erlang tuples.
Unlike full Smalltalk, these are assignment patterns — not match expressions
with multiple branches (for branch dispatch use `match:`, later in this chapter).

Forms:

```
#[a, b] := array        — array destructuring
#{#key => var} := dict  — map destructuring
{a, b} := tuple         — Erlang tuple destructuring
match: [...]             — multi-branch pattern dispatch
```

`_` (underscore) is the wildcard — it matches and discards.

## Array destructuring

```beamtalk
TestCase subclass: Ch14ArrayDestructuring

  testBasic =>
    #[a, b] := #[10, 20]
    self assert: a equals: 10
    self assert: b equals: 20

  testThreeElements =>
    #[a, b, c] := #[1, 2, 3]
    self assert: a equals: 1
    self assert: b equals: 2
    self assert: c equals: 3

  testWildcard =>
    // _ discards the matched element:
    #[_, b] := #[99, 42]
    self assert: b equals: 42

  testExtraElementsIgnored =>
    // Pattern has fewer slots than array — extras are silently ignored:
    #[a, b] := #[10, 20, 30, 40]
    self assert: a equals: 10
    self assert: b equals: 20

  testStrings =>
    #[first, second] := #["hello", "world"]
    self assert: first equals: "hello"
    self assert: second equals: "world"

  testSequential =>
    #[a, b] := #[1, 2]
    #[c, d] := #[3, 4]
    self assert: a + b + c + d equals: 10

  testInCollectBlock =>
    // Destructuring inside iteration blocks:
    pairs := #[#[1, 10], #[2, 20], #[3, 30]]
    sums := pairs collect: [:pair |
      #[a, b] := pair
      a + b
    ]
    self assert: sums equals: #[11, 22, 33]

  testInSelectBlock =>
    result := #[#[1, 10], #[15, 5], #[2, 20]]
      select: [:pair |
        #[a, b] := pair
        a < b
      ]
    self assert: result equals: #[#[1, 10], #[2, 20]]

  testInInjectBlock =>
    result := #[#[1, 10], #[2, 20]]
      inject: 0
      into: [:acc :pair |
        #[a, b] := pair
        acc + a + b
      ]
    self assert: result equals: 33
```

## Map destructuring

```beamtalk
TestCase subclass: Ch14MapDestructuring

  testSingleKey =>
    dict := #{#name => "Alice"}
    #{#name => name} := dict
    self assert: name equals: "Alice"

  testMultipleKeys =>
    dict := #{#x => 10, #y => 20}
    #{#x => x, #y => y} := dict
    self assert: x equals: 10
    self assert: y equals: 20

  testWildcard =>
    dict := #{#sid => "abc123", #runner => "runner1"}
    #{#sid => sid, #runner => _} := dict
    self assert: sid equals: "abc123"

  testSubsetDestructuring =>
    // Only some keys — extra keys in the dict are ignored:
    dict := #{#a => 1, #b => 2, #c => 3}
    #{#a => a, #c => c} := dict
    self assert: a equals: 1
    self assert: c equals: 3

  testMissingKeyRaises =>
    self should: [
      #{#missing => _val} := #{#present => 42}
    ] raise: RuntimeError

  testInCollectBlock =>
    dicts := #[#{#x => 1, #y => 10}, #{#x => 2, #y => 20}]
    sums := dicts collect: [:d |
      #{#x => x, #y => y} := d
      x + y
    ]
    self assert: sums equals: #[11, 22]
```

## Erlang tuple destructuring

Beamtalk can destructure Erlang tuples using `{}` pattern.
This is mainly useful when interoperating with Erlang code that returns
`{ok, Value}` or `{error, Reason}` tuples.

```beamtalk
TestCase subclass: Ch14TupleDestructuring

  testBasic =>
    {a, b} := Erlang erlang list_to_tuple: #[10, 20]
    self assert: a equals: 10
    self assert: b equals: 20

  testTaggedTuple =>
    // Common Erlang pattern: {ok, Value} or {error, Reason}
    {status, value} := Erlang erlang list_to_tuple: #[#ok, 42]
    self assert: status equals: #ok
    self assert: value equals: 42

  testLiteralPatternInFirstSlot =>
    // A literal in the pattern matches only that value:
    {#ok, value} := Erlang erlang list_to_tuple: #[#ok, 42]
    self assert: value equals: 42

  testLiteralMismatchRaises =>
    // Mismatch raises a RuntimeError (badmatch):
    self should: [
      {#ok, _val} := Erlang erlang list_to_tuple: #[#error, 42]
    ] raise: RuntimeError

  testWildcard =>
    {_, b} := Erlang erlang list_to_tuple: #[#ignore, 99]
    self assert: b equals: 99
```

## The match: expression

For multi-branch dispatch on a value, use `match:`. This is more powerful
than destructuring assignment — it tests multiple patterns and runs the
first matching branch.

### Literal and variable patterns

```beamtalk
TestCase subclass: Ch14MatchPatterns

  testLiteralMatch =>
    result := #ok match: [
      #ok -> "success";
      #error -> "failure";
      _ -> "unknown"
    ]
    self assert: result equals: "success"

  testVariableCapture =>
    result := 42 match: [x -> x + 1]
    self assert: result equals: 43
```

### Guard expressions (when:)

Add a `when:` guard to constrain when an arm matches. The guard is a
block that must evaluate to `true` for the arm to fire:

```beamtalk
  testGuardMatch =>
    result := 10 match: [
      x when: [x > 0] -> "positive";
      _ -> "non-positive"
    ]
    self assert: result equals: "positive"

  testMultipleGuards =>
    result := 42 match: [
      x when: [x > 100] -> "big";
      x when: [x > 10] -> "medium";
      _ -> "small"
    ]
    self assert: result equals: "medium"
```

### Array patterns in match:

```beamtalk
  testArrayPatternInMatch =>
    result := #[10, 20] match: [
      #[h, t] -> h + t;
      _ -> 0
    ]
    self assert: result equals: 30

  testArrayPatternWithGuard =>
    result := #[1, 2] match: [
      #[x, y] when: [x < y] -> "ascending";
      _ -> "other"
    ]
    self assert: result equals: "ascending"
```

### Nested patterns

Patterns can nest — match arrays within arrays:

```beamtalk
  testNestedArrayPattern =>
    result := #[#[1, 2], 3] match: [
      #[#[a, b], c] -> a + b + c;
      _ -> 0
    ]
    self assert: result equals: 6
```

### Map patterns in match:

```beamtalk
  testMapPatternInMatch =>
    d := #{#event => "click", #x => 5}
    result := d match: [
      #{#event => evName} -> evName;
      _ -> "unknown"
    ]
    self assert: result equals: "click"
```

### Constructor patterns (Result)

Match on `Result ok:` and `Result error:` arms:

```beamtalk
  testConstructorPattern =>
    result := (Result ok: 42) match: [
      Result ok: v -> v;
      Result error: _ -> 0
    ]
    self assert: result equals: 42

  testConstructorPatternWithGuard =>
    result := (Result ok: 5) match: [
      Result ok: v when: [v > 3] -> "big";
      Result ok: _ -> "small";
      Result error: _ -> "err"
    ]
    self assert: result equals: "big"
```

### Duplicate variables as equality constraints

Repeating a variable name in a pattern constrains both positions to be
equal:

```beamtalk
  testDuplicateVariableEquality =>
    result := #[3, 3] match: [
      #[x, x] -> "equal";
      _ -> "differ"
    ]
    self assert: result equals: "equal"
```

## Binary pattern matching (match:)

For matching on binary data (strings as UTF-8 bytes, binary protocols),
use the `match:` expression with `<< >>` binary segments.

Syntax:

```
expr match: [
  <<segment, ...>> -> result;
  <<segment, ...>> when: [guard] -> result;
  _ -> fallback
]
```

Segment types: `name:8` (8-bit int), `name/binary` (rest as binary)

```beamtalk
TestCase subclass: Ch14BinaryPatterns

  testMatchFirstByte =>
    // "A" is byte 65 in UTF-8:
    result := "ABC" match: [
      <<first:8, _/binary>> -> first;
      _ -> 0
    ]
    self assert: result equals: 65

  testMatchRestBinary =>
    result := "ABC" match: [
      <<_:8, rest/binary>> -> rest size;
      _ -> 0
    ]
    self assert: result equals: 2

  testMatchWildcard =>
    result := "hello" match: [
      <<_/binary>> -> true;
      _ -> false
    ]
    self assert: result

  testMatchWithGuard =>
    result := "X" match: [
      <<first:8, _/binary>> when: [first > 200] -> "high";
      _ -> "low"
    ]
    self assert: result equals: "low"
```

## Summary

**Destructuring assignment:**

```
#[a, b, c] := someArray           — array
#{#key => var} := dict             — map
{a, b} := erlangTuple              — tuple
```

**match: expression:**

```
expr match: [
  literal -> result;               — literal match
  x -> result;                     — variable capture
  x when: [guard] -> result;       — guarded match
  #[a, b] -> result;               — array pattern
  #[#[a, b], c] -> result;         — nested pattern
  #{#key => v} -> result;          — map pattern
  Result ok: v -> result;          — constructor pattern
  #[x, x] -> result;              — equality constraint
  <<byte:8, rest/binary>> -> r;    — binary pattern
  _ -> fallback                    — wildcard
]
```

Wildcard: `_` (matches and discards any value)

## Exercises

**1. Nested destructuring.** Given `pairs := #[#[1, 2], #[3, 4]]`, use
destructuring inside `inject:into:` to sum all four values.

<details>
<summary>Hint</summary>

```text
pairs := #[#[1, 2], #[3, 4]]
pairs inject: 0 into: [:acc :pair |
  #[a, b] := pair
  acc + a + b
]
// => 10
```
</details>

**2. match: with guards.** Write a `match:` expression that classifies a number
as `"even"` or `"odd"` using a guard that checks `isEven`.

<details>
<summary>Hint</summary>

```text
7 match: [
  x when: [x isEven] -> "even";
  _ -> "odd"
]
// => "odd"
```
</details>

**3. Map pattern matching.** Given a dictionary `#{#event => "click", #x => 5, #y => 10}`,
use a map pattern in `match:` to extract the event name.

<details>
<summary>Hint</summary>

```text
d := #{#event => "click", #x => 5, #y => 10}
d match: [
  #{#event => name} -> name;
  _ -> "unknown"
]
// => "click"
```

Map patterns match a subset of keys — extra keys are ignored.
</details>

Next: Chapter 15 — Type Annotations
