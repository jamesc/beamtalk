# Beamtalk Language Features

Planned language features for beamtalk. See [beamtalk-principles.md](beamtalk-principles.md) for design philosophy and [beamtalk-syntax-rationale.md](beamtalk-syntax-rationale.md) for syntax design decisions.

**Status:** Design phase - syntax and semantics subject to change.

**Syntax note:** Beamtalk uses a cleaned-up Smalltalk syntax: `//` comments (not `"..."`), standard math precedence (not left-to-right), and optional statement terminators (newlines work).

---

## Table of Contents

- [String Encoding and UTF-8](#string-encoding-and-utf-8)
- [Core Syntax](#core-syntax)
- [Async Message Passing](#async-message-passing)
- [Pattern Matching](#pattern-matching)
- [Live Patching](#live-patching)
- [Smalltalk + BEAM Mapping](#smalltalk--beam-mapping)
- [Tooling](#tooling)
- [Inspiration Sources](#inspiration-sources)
- [References](#references)

---

## String Encoding and UTF-8

**Beamtalk strings are UTF-8 by default.** This follows modern BEAM conventions and matches Elixir's approach.

### String Types

```
// Double-quoted strings - UTF-8 binaries
name := "Alice"
greeting := "Hello, 世界! 🌍"

// String interpolation (ADR 0023)
message := "Welcome, {name}!"
emoji := "Status: {status} ✓"

// All strings are <<"UTF-8 binary">> in Erlang
```

### Character Encoding

| Beamtalk | Erlang/BEAM | Notes |
|----------|-------------|-------|
| `"hello"` | `<<"hello">>` | UTF-8 binary |
| `"Hi, {name}"` | `<<"Hi, ", Name/binary>>` | Interpolated UTF-8 (ADR 0023) |
| Grapheme cluster | Via `:string` module | `"👨‍👩‍👧‍👦"` is one grapheme, multiple codepoints |
| `$a` | `97` (codepoint) | Character literal = Unicode codepoint |

### String Operations (Grapheme-Aware)

String operations respect Unicode grapheme clusters (user-perceived characters):

```
// Length in graphemes, not bytes
"Hello" length        // => 5
"世界" length          // => 2 (not 6 bytes)
"👨‍👩‍👧‍👦" length        // => 1 (family emoji is 1 grapheme, 7 codepoints)

// Slicing by grapheme
"Hello" at: 1         // => "H"
"世界" at: 1           // => "世"

// Iteration over graphemes
"Hello" each: [:char | Transcript show: char]

// Case conversion (locale-aware)
"HELLO" lowercase     // => "hello"
"straße" uppercase    // => "STRASSE" (German ß → SS)
```

### Binary Pattern Matching for Bytes

When you need byte-level access (network protocols, file formats):

```
// Parse UTF-8 manually
parseUtf8: <<codepoint/utf8, rest/binary>> =>
  Transcript show: codepoint
  self parseUtf8: rest

parseUtf8: <<>> => // done

// Parse fixed-size header + UTF-8 body
parsePacket: <<version:8, length:16/big, body:length/binary, rest/binary>> =>
  // body is a UTF-8 string
  message := body asString
  self process: message

// Build binary with UTF-8 content
packet := <<1, messageBytes/utf8>>
```

### BEAM Mapping

| Beamtalk | Erlang | Notes |
|----------|--------|-------|
| `"string"` | `<<"string">>` | Binary, not charlist |
| `"世界"` | `<<228,184,150,231,149,140>>` | UTF-8 encoded bytes |
| String operations | `:string` module | Grapheme-aware (`:string.length/1`) |
| `$x` | Integer codepoint | `$a` = 97, `$世` = 19990 |
| Charlist (legacy) | `[104,101,108,108,111]` | Via Erlang interop |

### Why UTF-8 by Default?

1. **Modern web/API standard** - JSON, HTTP, REST APIs all use UTF-8
2. **Compact for ASCII** - 1 byte per ASCII character (most code/English text)
3. **Elixir compatibility** - Seamless interop with Elixir libraries
4. **BEAM convention** - Erlang's `:string` module is Unicode-aware
5. **Agent/LLM-friendly** - AI models output UTF-8; easy integration

### Legacy Charlist Support

Charlists are Erlang lists of integer codepoints. Beamtalk uses binaries for strings, but you can convert when needed for Erlang interop via `binary_to_list` / `list_to_binary`.

### Implementation Status

| Feature | Status |
|---------|--------|
| UTF-8 string literals | ✅ Implemented - Lexer and parser support |
| String interpolation | ✅ Implemented (ADR 0023) - `"Hello, {name}!"` compiles to binary construction with `printString` dispatch |
| Grapheme-aware length | ✅ Available via Erlang `:string` module |
| Unicode normalization | ✅ Available via Erlang `:unicode` module |
| Case folding | ✅ Available via Erlang `:string` module |
| Binary pattern UTF-8 | ❌ Not yet implemented (BT-663) |

---

## Core Syntax

### Implementation Status

| Feature | Status | Test Coverage |
|---------|--------|---------------|
| Actor definition & spawning | ✅ Implemented | `actor_spawn`, `actor_spawn_with_args` |
| Unary messages | ✅ Implemented | `async_unary_message`, `unary_operators` |
| Binary messages/operators | ✅ Implemented | `binary_operators` |
| Keyword messages | ✅ Implemented | `async_keyword_message`, `multi_keyword_complex_args` |
| Cascades | ✅ Implemented | `cascades`, `cascade_complex` |
| Blocks/closures | ✅ Implemented | `blocks_no_args`, `empty_blocks`, `nested_blocks` |
| Field access & assignment | ✅ Implemented (with limitations) | `actor_state_mutation` |
| Class definitions | ✅ Implemented | `class_definition` |
| Map literals | ✅ Implemented | `map_literals` |
| Pattern matching (`match:`) | ✅ Implemented | `pattern_matching` (stdlib + E2E) |
| Comments | ✅ Implemented | `comment_handling` |
| Async message sends | ✅ Implemented | `async_with_await`, `async_keyword_message` |

### Actor Definition

```
Actor subclass: Counter
  state: value = 0

  increment => self.value := self.value + 1
  decrement => self.value := self.value - 1
  getValue => ^self.value
  incrementBy: delta => self.value := self.value + delta
```

### Value Types vs Actors

**Status:** ✅ Implemented

Beamtalk distinguishes between **value types** (immutable data) and **actors** (concurrent processes):

```
// Value type - plain Erlang map, no process
Object subclass: Point
  state: x = 0
  state: y = 0
  
  // Methods return new instances (immutable)
  plus: other => Point new: #{x => (self.x + other x), y => (self.y + other y)}
  describe => "Point({self.x}, {self.y})"

// Actor - process with mailbox
Actor subclass: Counter
  state: count = 0
  
  // Methods mutate state via message passing
  increment => self.count := self.count + 1
  getCount => ^self.count
```

**Key differences:**

| Aspect | Value Types (`Object subclass:`) | Actors (`Actor subclass:`) |
|--------|----------------------------------|----------------------------|
| Instantiation | `Point new` or `Point new: #{x => 5}` | `Counter spawn` or `Counter spawnWith: #{count => 0}` |
| Runtime | Plain Erlang map/record | BEAM process (gen_server) |
| Mutation | Immutable - methods return new instances | Mutable - methods modify state |
| Message passing | N/A (direct function calls) | Async messages with futures |
| Concurrency | Copied when sent between processes | Process isolation, mailbox queuing |
| Use cases | Data structures, coordinates, money | Services, stateful entities, concurrent tasks |

**Class hierarchy:**
```
ProtoObject (minimal - identity, DNU)
  └─ Object (reflection + new)
       ├─ Integer, String (primitives)
       ├─ Point, Color (value types)
       └─ Actor (process-based + spawn)
            └─ Counter, Server (actors)
```

**Why this matters:**
- **Performance**: Value types avoid process overhead for simple data
- **Semantics**: Clear distinction between data and concurrent entities
- **BEAM interop**: Maps naturally to Erlang records/maps vs gen_server processes
- **Idiomatic code**: Use value types for data, actors for behavior

### Message Sends

```
// Unary message
counter increment

// Binary message (standard math precedence: 2 + 3 * 4 = 14)
3 + 4

// Keyword message
dict at: #name put: "hello"

// Cascade - multiple messages to same receiver
Transcript show: "Hello"; cr; show: "World"
```

### Message Precedence (high to low)
1. Unary messages: `3 factorial`
2. Binary messages: `3 + 4` (with standard math precedence within binary)
3. Keyword messages: `dict at: #name`

### Binary Operators

Binary operators follow standard math precedence (highest to lowest):

#### Exponentiation (highest precedence)
- `**` - Power: `2 ** 10` → `1024` (right-associative)

#### Multiplicative
- `*` - Multiplication: `3 * 4` → `12`
- `/` - Division: `10 / 2` → `5`
- `%` - Modulo/Remainder: `17 % 5` → `2`

#### Additive
- `+` - Addition: `3 + 4` → `7`
- `-` - Subtraction: `10 - 3` → `7`
- `++` - String concatenation: `"Hello" ++ " World"` → `"Hello World"`

#### Comparison
- `<` - Less than: `3 < 5` → `true`
- `>` - Greater than: `5 > 3` → `true`
- `<=` - Less than or equal: `3 <= 3` → `true`
- `>=` - Greater than or equal: `5 >= 3` → `true`

#### Equality (lowest precedence)
- `=:=` - Strict equality (Erlang `=:=`): `5 =:= 5` → `true`
- `==` - Loose equality (Erlang `==`): `5 == 5.0` → `true`
- `/=` - Loose inequality (Erlang `/=`): `5 /= 6` → `true`
- `=/=` - Strict inequality (Erlang `=/=`): `5 =/= 6` → `true`

**Note on `and`/`or`:** These are **not** binary operators. They are keyword messages that take blocks for short-circuit evaluation:
```
// Short-circuit AND - second block only evaluated if first is true
result := condition and: [expensiveCheck()]

// Short-circuit OR - second block only evaluated if first is false  
result := condition or: [fallbackValue()]
```

### Field Access and Assignment

Direct field access within actors using dot notation:

```
// Read field
current := self.value

// Write field
self.value := 10

// Explicit assignment
self.value := self.value + 1
self.count := self.count - delta
self.total := self.total * factor
```

**Note:** `self.field` compiles to direct map access, not a message send. For external access to another actor's state, use message sends.

**Parenthesized assignment:** Field assignments can be used as expressions when wrapped in parentheses — `(self.x := 5)` returns the assigned value:

```
// Assignment as expression (returns 6)
(self.x := 5) + 1

// Field assignments as sequential statements
self.x := 5.
self.y := self.x + 1.
^self.y
```

**Limitation:** Field assignments in stored blocks (non-control-flow) are not supported:

```
// NOT SUPPORTED: field assignment inside stored blocks
nestedBlock := [:m | self.x := m]
nestedBlock value: 10

// SUPPORTED: field mutation in control flow blocks
true ifTrue: [self.x := 5]
```

### Blocks (Closures)

```
// Block with no arguments
[self doSomething]

// Block with arguments
[:x :y | x + y]

// Block with local variables
[:x | temp := x * 2. temp + 1]
```

---

## Control Flow and Mutations

Beamtalk supports Smalltalk-style control flow via messages to booleans and blocks, with one important enhancement: **literal blocks in control flow positions can mutate local variables and fields**.

### The Simple Rule

> **Literal blocks in control flow positions can mutate (both local vars AND fields). Stored/passed closures cannot.**

This enables idiomatic Smalltalk patterns while maintaining clear semantics on the BEAM.

### Control Flow Constructs

These message sends detect literal blocks and generate tail-recursive loops with state threading:

| Construct | Example | Mutations Allowed |
|-----------|---------|-------------------|
| `whileTrue:` / `whileFalse:` | `[count < 10] whileTrue: [count := count + 1]` | ✅ |
| `timesRepeat:` | `5 timesRepeat: [sum := sum + n]` | ✅ |
| `to:do:` | `1 to: 10 do: [:n \| total := total + n]` | ✅ |
| `do:`, `collect:`, `select:`, `reject:` | `items do: [:x \| sum := sum + x]` | ✅ |
| `inject:into:` | `items inject: 0 into: [:acc :x \| acc + x]` | ✅ |

### Local Variable Mutations

```
// Simple counter
count := 0.
[count < 10] whileTrue: [count := count + 1].
// count is now 10

// Multiple variables
sum := 0.
product := 1.
i := 1.
[i <= 5] whileTrue: [
    sum := sum + i.
    product := product * i.
    i := i + 1
].
// sum = 15, product = 120, i = 6

// Collection iteration
numbers := #(1, 2, 3, 4, 5)
total := 0.
numbers do: [:n | total := total + n].
// total = 15

// With index
result := 0.
1 to: 10 do: [:n | result := result + n].
// result = 55 (sum of 1..10)
```

### Field Mutations

Mutations to actor state (`self.field`) work the same way:

```
Actor subclass: Counter [
    state: value = 0
    state: count = 0
    
    // ✅ Field mutation in control flow
    increment =>
        [self.value < 10] whileTrue: [
            self.value := self.value + 1
        ].
        ^self.value
    
    // ✅ Multiple fields
    incrementBoth =>
        [self.value < 10] whileTrue: [
            self.value := self.value + 1.
            self.count := self.count + 1
        ]
]
```

### Mixed Mutations

Local variables and fields can be mutated together:

```
processItems =>
    total := 0.
    self.processed := 0.
    
    self.items do: [:item |
        total := total + item.
        self.processed := self.processed + 1
    ].
    
    ^total
```

### What's Forbidden

Stored or passed closures cannot mutate:

```
// ❌ ERROR: Field assignment in stored closure
badBlock =>
    myBlock := [self.value := self.value + 1].
    // ERROR: Cannot assign to field 'value' inside a stored closure.
    
// ⚠️ WARNING: Local mutation in stored closure has no effect
testWarning =>
    count := 0.
    myBlock := [count := count + 1].
    // WARNING: Assignment to 'count' has no effect on outer scope.
    
    10 timesRepeat: myBlock.
    ^count  // Still 0, not 10
    
// ✅ CORRECT: Use literal blocks in control flow
testCorrect =>
    count := 0.
    10 timesRepeat: [count := count + 1].  // ✅ Works!
    ^count  // Now 10
```

### Why This Design?

| Property | ✅ Benefit |
|----------|-----------|
| **Simple** | One rule covers everything |
| **Orthogonal** | Same behavior for local vars and fields |
| **Smalltalk-like** | Natural iteration patterns work |
| **Safe** | Escaping closures can't cause confusion |
| **Good DX** | Clear errors with fix suggestions |
| **BEAM-idiomatic** | Compiles to tail recursion + state threading |

### Error Messages

When you accidentally store a mutating closure, you get helpful guidance:

```
Error: Cannot assign to field 'sum' inside a stored closure.

Field assignments require immediate execution context for state threading.

Fix: Use control flow directly, or extract to a method:
  // Instead of:
  myBlock := [:item | self.sum := self.sum + item].
  items do: myBlock.
  
  // Write:
  items do: [:item | self.sum := self.sum + item].
  
  // Or use a method:
  addToSum: item => self.sum := self.sum + item.
  items do: [:item | self addToSum: item].
```

---

## Async Message Passing

Beamtalk uses **async-first** message passing, unlike Smalltalk's synchronous model. Messages to actors return **futures**.

### Default: Async with Futures

```
// Load the Counter actor
:load examples/counter.bt

// Spawn an actor — returns a reference
c := Counter spawn

// Messages to actors return futures
c increment             // returns a Future
c increment await       // explicitly wait for completion
c getValue await        // => 2
```

### REPL Auto-Await

In the REPL, futures are **automatically awaited** for a natural, synchronous feel:

```
> c := Counter spawn
#Actor<Counter,_>

> c increment
1

> c increment
2

> c getValue
2
```

Outside the REPL (in compiled code), you must explicitly `await` or use continuations.

### BEAM Mapping

| Beamtalk | BEAM |
|----------|------|
| Async send | `gen_server:cast` + future process |
| `await` | `receive` block or `gen_server:call` |
| Future | Lightweight process holding a result |

---

## Pattern Matching

Smalltalk lacks pattern matching - this is a major ergonomic addition.

### Match Expression

**Status:** ✅ Implemented

The `match:` keyword message takes a block of pattern arms separated by `;`:

```beamtalk
// Basic match with literals
x match: [1 -> "one"; 2 -> "two"; _ -> "other"]

// Variable binding in patterns
42 match: [n -> n + 1]
// => 43

// Symbol matching
status match: [#ok -> "success"; #error -> "failure"; _ -> "unknown"]

// String matching
greeting match: ["hello" -> "hi"; _ -> "huh?"]

// Guard clauses with when:
x match: [
  n when: [n > 100] -> "big";
  n when: [n > 10] -> "medium";
  _ -> "small"
]

// Negative number patterns
temp match: [-1 -> "minus one"; 0 -> "zero"; _ -> "other"]

// Match on computed expression
(3 + 4) match: [7 -> "correct"; _ -> "wrong"]
```

**Supported pattern types:**

| Pattern | Example | Description |
|---------|---------|-------------|
| Wildcard | `_` | Matches anything |
| Literal integer | `42` | Exact integer match |
| Literal float | `3.14` | Exact float match |
| Literal string | `"hello"` | Exact string match |
| Literal symbol | `#ok` | Exact symbol match |
| Literal character | `$a` | Exact character match |
| Negative number | `-1` | Negative integer/float match |
| Variable | `x` | Binds matched value to name |
| Tuple | `{a, b}` | Destructure tuple (patterns supported; tuple literals planned) |

**Guard expressions** support: `>`, `<`, `>=`, `<=`, `=:=`, `=/=`, `/=`, `+`, `-`, `*`, `/`

### Destructuring Assignment

```
// Destructure tuple
{x, y, z} := point coordinates

// Destructure in block
results collect: [{#ok, v} -> v; {#error, _} -> nil]

// Nested destructuring
{name, {street, city}} := person address
```

---

## Live Patching

Hot code reload with dedicated syntax.

```
// Patch a method on running actors
patch Counter >> #increment {
  Telemetry log: "incrementing"
  self.value := self.value + 1
}

// Patch with state migration
patch Agent >> state {
  // Add new field, migrate existing
  self.memory := self.history ifNil: [OrderedCollection new]
}
```

---

## Smalltalk + BEAM Mapping

| Smalltalk/Newspeak Concept | Beamtalk/BEAM Mapping |
|----------------------------|----------------------|
| Object | Process with state (actor) |
| Class | Module + constructor function |
| Instance variable | Process state map field |
| Field access (`self.x`) | `maps:get('x', State)` |
| Field write (`self.x := v`) | `maps:put('x', v, State)` |
| Message send | Async: `gen_server:cast` + future |
| Sync message | `gen_server:call` (opt-in) |
| Future/Promise | Lightweight process or ref |
| `await` | Block on future / `receive` |
| `whenResolved:` | Callback on future completion |
| Block | Erlang fun (closure) |
| Image | Running node(s) |
| Workspace | Connected REPL to live node |
| Class browser | Code introspection via `code:` module |
| Global variable | Registered process or application env |

---

## Standard Library

Core classes implemented and tested:

| Class | Status | Description | Test Coverage |
|-------|--------|-------------|---------------|
| **Actor** | ✅ Implemented | Base class for all actors (BEAM processes) | `actor_spawn`, `actor_state_mutation` |
| **Block** | ✅ Implemented | First-class closures | `stdlib_block`, `blocks_no_args` |
| **Boolean** | ✅ Implemented | `True` and `False` with control flow | `stdlib_boolean` |
| **Integer** | ✅ Implemented | Arbitrary precision arithmetic | `stdlib_integer` |
| **String** | ✅ Implemented | UTF-8 text with operations | `stdlib_string`, `string_operations` |
| **Array** | 🔮 Planned | Fixed-size indexed collection (tuple-backed, O(1) access) | — |
| **List** | ✅ Implemented | Linked list with fast prepend (`#()` syntax) | `stdlib_list`, `collections` |
| **Dictionary** | ✅ Implemented | Key-value map | `stdlib_dictionary`, `map_literals` |
| **Set** | ✅ Implemented | Unordered unique elements | `stdlib_set` |
| **Stream** | ✅ Implemented | Lazy, closure-based sequences ([ADR 0021](ADR/0021-streams-and-io-design.md)) | `stream`, `stream_collections`, `file_stream` |
| **Nil** | ✅ Implemented | Null object pattern | `stdlib_nil`, `stdlib_nil_object` |

For detailed API documentation, see [`lib/README.md`](../lib/README.md).

### Stream — Lazy Pipelines

**Status:** ✅ Implemented ([ADR 0021](ADR/0021-streams-and-io-design.md))

Stream is Beamtalk's universal interface for sequential data. A single, sealed, closure-based type that unifies collection processing, file I/O, and generators under one protocol.

Operations are either **lazy** (return a new Stream) or **terminal** (force evaluation and return a result). Nothing computes until a terminal operation pulls elements through.

#### Constructors

```beamtalk
// Infinite stream starting from a value, incrementing by 1
Stream from: 1                     // 1, 2, 3, 4, ...

// Infinite stream with custom step function
Stream from: 1 by: [:n | n * 2]   // 1, 2, 4, 8, ...

// Stream from a collection (List, String, Set, Dictionary)
Stream on: #(1, 2, 3)             // wraps collection lazily

// Collection shorthand — any collection responds to `stream`
#(1, 2, 3) stream                  // same as Stream on: #(1, 2, 3)
"hello" stream                     // Stream over characters
#{#a => 1} stream                  // Stream over Associations
(Set new add: 1) stream            // Stream over set elements

// File streaming — lazy, constant memory
File lines: "data.csv"            // Stream of lines
File open: "data.csv" do: [:handle |
  handle lines take: 10           // block-scoped handle
]
```

#### Lazy Operations

Lazy operations return a new Stream without evaluating anything:

| Method | Description | Example |
|--------|-------------|---------|
| `select:` | Filter elements matching predicate | `s select: [:n \| n > 2]` |
| `collect:` | Transform each element | `s collect: [:n \| n * 10]` |
| `reject:` | Exclude elements matching predicate | `s reject: [:n \| n isEven]` |
| `drop:` | Skip first N elements | `s drop: 5` |

```beamtalk
// Build a pipeline — nothing computes yet
s := Stream from: 1
s := s select: [:n | n isEven]
s := s collect: [:n | n * n]
// s is still a Stream — no values computed
```

#### Terminal Operations

Terminal operations force evaluation and return a concrete result:

| Method | Description | Example |
|--------|-------------|---------|
| `take:` | First N elements as List | `s take: 5` → `[2,4,6,8,10]` |
| `asList` | Materialize entire stream to List | `s asList` → `[1,2,3]` |
| `do:` | Iterate with side effects, return nil | `s do: [:n \| Transcript show: n]` |
| `inject:into:` | Fold/reduce with initial value | `s inject: 0 into: [:sum :n \| sum + n]` |
| `detect:` | First matching element, or nil | `s detect: [:n \| n > 10]` |
| `anySatisfy:` | True if any element matches | `s anySatisfy: [:n \| n > 2]` |
| `allSatisfy:` | True if all elements match | `s allSatisfy: [:n \| n > 0]` |

```beamtalk
// Terminal forces computation through the pipeline
((Stream from: 1) select: [:n | n isEven]) take: 5
// => [2,4,6,8,10]

(Stream on: #(1, 2, 3, 4)) inject: 0 into: [:sum :n | sum + n]
// => 10
```

#### printString — Pipeline Inspection

Stream's `printString` shows pipeline structure, not values — keeping the REPL inspectable even for lazy data:

```beamtalk
(Stream from: 1) printString
// => Stream(from: 1)

(Stream on: #(1, 2, 3)) printString
// => Stream(on: [...])

((Stream from: 1) select: [:n | n isEven]) printString
// => Stream(from: 1) | select: [...]
```

#### Eager vs Lazy — The Boundary

Collections keep their **eager** methods (`select:`, `collect:`, `do:`, etc.) for simple cases. The `stream` message is the explicit opt-in to **lazy** evaluation:

```beamtalk
// Eager — List methods return a List immediately
#(1, 2, 3, 4, 5) select: [:n | n > 2]
// => [3,4,5]  (a List)

// Lazy — stream methods return a Stream (unevaluated)
(#(1, 2, 3, 4, 5) stream) select: [:n | n > 2]
// => Stream  (unevaluated — call asList or take: to materialize)
```

The receiver makes the boundary visible: you always know whether you're working with a Collection (eager) or a Stream (lazy).

#### File Streaming

`File lines:` returns a lazy Stream of lines — constant memory, safe for large files:

```beamtalk
// Read lines lazily
(File lines: "data.csv") do: [:line | Transcript show: line]

// Pipeline composition
(File lines: "app.log") select: [:l | l includes: "ERROR"]

// Block-scoped handle for explicit lifecycle control
File open: "data.csv" do: [:handle |
  handle lines take: 10
]
// handle closed automatically when block exits
```

**Cross-process constraint:** File-backed Streams must be consumed by the same process that created them (BEAM file handles are process-local). To pass file data to an actor, materialize first: `(File lines: "data.csv") take: 100` returns a List that can be sent safely. Collection-backed Streams have no such restriction.

#### Side-Effect Timing ⚠️

Side effects in lazy pipelines run at **terminal** time, not at definition time:

```beamtalk
// This prints NOTHING — the pipeline is just a recipe
s := (Stream on: #(1, 2, 3)) collect: [:n | Transcript show: n. n * 2]

// This is when printing actually happens
s asList
// Transcript shows: 1, 2, 3
// => [2,4,6]
```

If you need immediate side effects, use the eager collection method (`List do:`) or call a terminal operation right away.

### Pragma Annotations (`@primitive` and `@intrinsic`)

**Status:** ✅ Implemented ([ADR 0007](ADR/0007-compilable-stdlib-with-primitive-injection.md))

The standard library uses pragma annotations to declare methods whose implementations are provided by the compiler or runtime rather than written in Beamtalk code.

There are two pragma forms:

| Pragma | Syntax | Purpose |
|--------|--------|---------|
| `@primitive 'selector'` | Quoted selector | **Selector-based dispatch** — routes through runtime dispatch modules (`beamtalk_primitive.erl` → type-specific modules). Used for arithmetic, comparison, string operations, etc. |
| `@intrinsic name` | Unquoted identifier | **Structural intrinsic** — the compiler generates specialized code inline. Used for spawning, block evaluation, control flow, reflection, etc. |

Both forms are semantically equivalent at the compiler level (they produce the same AST node), but the naming convention distinguishes their intent:

**`@primitive` (quoted)** — runtime-dispatched method implementations:
```
// In lib/Integer.bt
+ other => @primitive '+'
asString => @primitive 'asString'
```

**`@intrinsic` (unquoted)** — compiler structural intrinsics:
```
// In lib/Block.bt
value => @intrinsic blockValue
whileTrue: bodyBlock => @intrinsic whileTrue

// In lib/Actor.bt
sealed spawn => @intrinsic actorSpawn

// In lib/Object.bt
new => @intrinsic basicNew
hash => @intrinsic hash
```

The full list of structural intrinsics: `actorSpawn`, `actorSpawnWith`, `blockValue`, `blockValue1`–`blockValue3`, `whileTrue`, `whileFalse`, `repeat`, `onDo`, `ensure`, `timesRepeat`, `toDo`, `toByDo`, `basicNew`, `basicNewWith`, `hash`, `respondsTo`, `instVarNames`, `instVarAt`, `instVarAtPut`, `dynamicSend`, `dynamicSendWithArgs`, `error`.

---

## Tooling

Tooling is part of the language, not an afterthought. Beamtalk is designed to be used interactively.

### VS Code Extension

| Feature | Description | Status |
|---------|-------------|--------|
| **Live Agent Browser** | Tree view of running actors, supervisors, state | Planned |
| **Click-to-Inspect** | Select any actor → see state, mailbox, methods | Planned |
| **Hot-Patch Editor** | Edit method → Ctrl+S → live in <200ms | Planned |
| **Message Timeline** | Sequence diagram of actor interactions | Planned |
| **Distributed REPL** | Send messages to actors on any node | Planned |
| **Syntax Highlighting** | Smalltalk-style message syntax | Planned |
| **Autocomplete** | Message selectors, actor names, state fields | Planned |
| **Go to Definition** | Jump to actor/method definitions | Planned |
| **Error Diagnostics** | Inline errors with source spans | Planned |
| **Hover Documentation** | Show method signatures and docs | Planned |

### Web Dashboard (Phoenix LiveView)

| Feature | Description |
|---------|-------------|
| **Cluster Topology** | Visual map of nodes, actors, supervision trees |
| **Real-time Metrics** | Messages/sec, crash rates, mailbox depths |
| **Process Inspector** | Click any process → state, mailbox, links |
| **Browser-based Patching** | Edit and deploy code from browser |
| **Telemetry Dashboard** | OpenTelemetry/Langfuse integration |
| **Message Flow** | Live visualization of message passing |
| **Memory/GC Stats** | Per-process memory, GC pauses |

### CLI Tools

```bash
# Project management (implemented)
beamtalk new myapp          # ✅ Create new project
beamtalk build              # ✅ Compile to BEAM
beamtalk run                # ✅ Compile and start
beamtalk check              # ✅ Check for errors without compiling
beamtalk daemon start/stop  # ✅ Manage compiler daemon

# Development
beamtalk repl               # ✅ Interactive REPL (basic implementation)
beamtalk attach node@host   # ❌ Not yet implemented
beamtalk inspect pid        # ❌ Not yet implemented

# Debugging (planned)
beamtalk trace actor        # ❌ Not yet implemented
beamtalk profile            # ❌ Not yet implemented  
beamtalk observer           # ❌ Not yet implemented
```

### REPL Features

```
// Spawn and interact
counter := Counter spawn
counter increment
counter getValue await  // => 1

// Inspect running actors
counter inspect         // Opens browser
counter state           // => {value: 1}
counter mailbox peek    // => []

// Hot patch
patch Counter >> #increment {
  Transcript log: "incrementing"
  self.value := self.value + 1
}

// Evaluate in actor context
counter eval: [self.value := 100]
```

### Testing Framework (ADR 0014)

Beamtalk includes a native test framework inspired by Smalltalk's SUnit. Tests are compiled directly to EUnit modules and run on BEAM — no REPL daemon needed.

#### Expression Tests (`// =>` assertions)

The simplest way to test — expressions with expected results:

```beamtalk
// tests/stdlib/arithmetic.bt

1 + 2
// => 3

"hello" size
// => 5

"hello" , " world"
// => hello world
```

Run with `just test-stdlib`.

#### TestCase Classes (✅ Implemented)

SUnit-style test classes with lifecycle methods and assertions:

```beamtalk
// test/counter_test.bt

TestCase subclass: CounterTest

  testInitialValue =>
    self assert: (Counter spawn getValue await) equals: 0

  testIncrement =>
    self assert: (Counter spawn increment await) equals: 1

  testMultipleIncrements =>
    | counter |
    counter := Counter spawn
    3 timesRepeat: [counter increment await]
    self assert: (counter getValue await) equals: 3
```

Each test method gets a fresh instance with `setUp` → test → `tearDown` lifecycle.

#### Assertion Methods

| Method | Description | Example |
|--------|-------------|---------|
| `assert:` | Assert condition is true | `self assert: (x > 0)` |
| `assert:equals:` | Assert two values are equal | `self assert: result equals: 42` |
| `deny:` | Assert condition is false | `self deny: list isEmpty` |
| `should:raise:` | Assert block raises error | `self should: [1 / 0] raise: #badarith` |
| `fail:` | Unconditional failure | `self fail: "not implemented"` |

#### Running Tests

```bash
# Run TestCase tests in test/ directory
beamtalk test

# Run expression tests (stdlib)
just test-stdlib

# Run all tests (unit + stdlib + E2E + runtime)
just test-all
```

#### REPL Integration (✅ Implemented)

Run tests interactively from the REPL:

```text
> :load test/counter_test.bt
Loaded CounterTest

> CounterTest runAll
Running 2 tests...
  ✓ testIncrement
  ✓ testMultipleIncrements
2 passed, 0 failed

> CounterTest run: #testIncrement
  ✓ testIncrement
```

### Observability Integration

| System | Integration |
|--------|-------------|
| **OpenTelemetry** | Automatic spans for message sends |
| **Prometheus** | Metrics export (message rates, latencies) |
| **Langfuse** | LLM agent tracing and prompt management |
| **Grafana** | Dashboard templates for BEAM metrics |
| **Honeycomb** | Distributed tracing across nodes |

### Debug/Trace Syntax

```
// Trace all messages to an actor
counter trace: #all

// Trace specific messages
counter trace: [#increment, #getValue]

// Conditional breakpoint
counter breakOn: #increment when: [self.value > 100]

// Log message flow
Tracer enable: #messageFlow for: Counter
```

---

## Inspiration Sources

### From Smalltalk/Newspeak
- Message-based syntax
- Live programming
- Async actors (Newspeak)
- Reflection

### From Erlang
- Pattern matching
- Binary syntax
- Process links/monitors
- Distribution

### From Elixir/Gleam
- Exhaustive matching

### From Dylan
- Sealing

---

## References

- [Newspeak Language](https://newspeaklanguage.org/)
- [Dylan Reference](https://opendylan.org/documentation/)
- [Elixir Guides](https://elixir-lang.org/getting-started/)
- [Gleam Language Tour](https://gleam.run/book/tour/)
- [BEAM Book](https://blog.stenmans.org/theBeamBook/)

---

## Reflection API

**Status:** ✅ Implemented as of 2026-02-05

All Beamtalk objects automatically respond to standard reflection messages. These are generated by the compiler for every actor class.

### Core Reflection Methods

#### `class`

Returns the class name as an atom.

```beamtalk
counter := Counter spawn
counter class    // => Counter
```

#### `respondsTo: selector`

Checks if an object responds to a given method selector. Pass a symbol literal.

```beamtalk
counter respondsTo: #increment   // => true
counter respondsTo: #nonExistent // => false
```

#### `instVarNames`

Returns a list of instance variable (field) names. Internal fields (starting with `__`) are filtered out.

```beamtalk
counter instVarNames    // => ["value"]
```

#### `instVarAt: fieldName`

Returns the value of an instance variable by name (symbol):

```beamtalk
counter instVarAt: #value    // => 0
```

Returns `{error, field_not_found}` if the field doesn't exist.

#### `instVarAt:put:`

Sets the value of an instance variable. Returns the new value.

```beamtalk
counter instVarAt: #value put: 42    // => 42
```

Returns `{error, field_not_found}` if the field doesn't exist.

**Note:** Only works for mutable instance variables in actors. Primitives (integers, strings) are immutable.

#### `perform:` and `perform:withArguments:`

Dynamically dispatches to a method with the given selector:

```beamtalk
// Zero-arity method
counter perform: #getValue    // => <current value>

// Method with arguments (typically used in doesNotUnderstand:)
doesNotUnderstand: selector args: arguments =>
  self perform: selector withArguments: arguments
```

**Relationship to Smalltalk:** Beamtalk provides `perform:` for zero-arity methods and `perform:withArguments:` for methods with arguments, avoiding the proliferation of `perform:with:with:with:` variants.

### Implementation Notes

All reflection methods are automatically generated by the compiler for every actor class. They are added to the method table and dispatch logic at compile time.

Reflection methods are declared in the stdlib via pragma annotations (see [ADR 0007](ADR/0007-compilable-stdlib-with-primitive-injection.md)). For example, `respondsTo:` and `instVarNames` are declared in `lib/Object.bt` with `@intrinsic respondsTo` and `@intrinsic instVarNames` pragmas that generate specialized compiler intrinsic code. The `perform:` and `perform:withArguments:` methods are declared in `lib/Object.bt` with `@intrinsic dynamicSend`, which generates code to send the target selector directly to the actor mailbox.

### Limitations (As of 2026-02-05)

- `instVarAt:` and `instVarAt:put:` require symbol field names (`#fieldName`)
- No reflection on class methods (only instance methods)
- No reflection on method metadata (arity, parameters, etc.)
- Primitives don't support reflection yet

**Note:** Symbol literals (`#selector`) work correctly. Future semantic validation will provide better error messages when users accidentally pass identifiers instead of symbols.

### Future Enhancements

- Reflection support for primitives
- Better error messages for symbol validation (semantic analysis)
- Class-level reflection (`allInstances`, `allSubclasses`)
- Method metadata (`methodDict`, `sourceCodeAt:`)
- Block reflection (`sourceNode`, `numArgs`)

See [ADR 0005](ADR/0005-beam-object-model-pragmatic-hybrid.md) for full object model design.

## Extension Methods

Extension methods allow adding new methods to existing classes without subclassing.
This follows Pharo's proven extension method design.

### How Extensions Work

Extensions are registered at runtime via the extension registry (`beamtalk_extensions`).
When a message is sent that doesn't match a class's compiled methods, the dispatch
checks the extension registry before walking to the superclass or raising a
`does_not_understand` error.

### Dispatch Priority

For both Actor classes and value types, the dispatch order is:

1. **Compiled methods** — Methods defined in the class source
2. **Extension methods** — Methods registered via the extension registry (ADR 0005)
3. **Inherited methods** — Methods from superclass chain (hierarchy walk)
4. **doesNotUnderstand:args:** — Custom error handler if defined
5. **Error** — Structured `#beamtalk_error{kind=does_not_understand}`

### Supported Class Types

| Class Type | Extension Support |
|------------|-------------------|
| **Value types** (Integer, String, Boolean, etc.) | ✅ Supported |
| **Actor classes** (user-defined via `Actor subclass:`) | ✅ Supported |

### Extension Function Signature

Extension functions receive `(Args, Self)`:
- `Args` — List of message arguments
- `Self` — The receiver object (value for primitives, `#beamtalk_object{}` for actors)

### Runtime Registration (Erlang API)

```erlang
%% Register an extension method
Fun = fun([], Self) -> 42 end,
beamtalk_extensions:register('Counter', myMethod, Fun, my_library).

%% Check if extension exists
beamtalk_extensions:has('Counter', myMethod).  % => true

%% List all extensions on a class
beamtalk_extensions:list('Counter').  % => [{myMethod, my_library}]
```

### Implementation Notes

- Extensions use a global ETS-based registry with conflict tracking
- Last-writer-wins policy for conflicting registrations
- Provenance tracking records which library registered each extension
- Generated dispatch includes try/catch for bootstrap safety (ETS table may not exist yet)

### Future: `extend` Syntax

A dedicated `extend` keyword message for defining extensions in Beamtalk source
is planned but not yet implemented:

```beamtalk
// Future syntax (not yet available):
Counter extend
  reset => self setValue: 0

Integer extend
  factorial => self <= 1 ifTrue: [1] ifFalse: [self * (self - 1) factorial]
```

### References

- Extension registry: `runtime/apps/beamtalk_runtime/src/beamtalk_extensions.erl`
- ADR 0005: `docs/ADR/0005-beam-object-model-pragmatic-hybrid.md` (Q3: Extension methods)
- Design doc: `docs/internal/design-self-as-object.md` (Section 3.3)

---

## Not Yet Implemented

> The following features are planned but not yet implemented. They are kept here for review and future reference.

### Pattern Matching in Message Handlers

```
// Match on message structure
handle: {#ok, value} => self process: value
handle: {#error, reason} => self logError: reason
handle: _ => self handleUnknown

// Match with guards
process: n when: [n > 0] => self positive: n
process: n when: [n < 0] => self negative: n
process: 0 => self zero
```

### Pipe Operator (Elixir-inspired)

Clean data flow through transformations.

#### Sync Pipe

```
data
  |> Transform with: options
  |> Filter where: [:x | x > 0]
  |> Sort by: #name
```

#### Async Pipe

```
// Each step returns future, flows through
data
  |>> agent1 process
  |>> agent2 validate
  |>> agent3 store
```

---

### With Blocks (Elixir/Gleam-inspired)

Early exit on error without deep nesting.

#### Problem: Nested Error Handling

```
// Ugly pyramid of doom
(file open: path) ifOk: [:f |
  (f read) ifOk: [:data |
    (Json parse: data) ifOk: [:json |
      self process: json]]]
```

#### Solution: With Blocks

```
with: [
  file := File open: path
  data := file read
  json := Json parse: data
] do: [
  self process: json
] else: [:error |
  self handleError: error
]
```

---

### Result Type (Gleam-inspired)

Explicit error handling as alternative to exceptions.

```
// Return Result instead of raising
divide: a by: b -> Result =>
  b = 0
    ifTrue: [Error new: #divisionByZero]
    ifFalse: [Ok new: a / b]

// Chain with map
(self divide: 10 by: x)
  |> map: [:v | v * 2]
  |> unwrapOr: 0

// Pattern match on result
result := self divide: a by: b
result match: [
  {#ok, v} -> self use: v
  {#error, e} -> self report: e
]
```

---

### Comprehensions (Elixir-inspired)

Declarative iteration with filtering.

```
// List comprehension with filter
for: [x in: 1 to: 10; x > 5]
yield: [x * 2]
// => [12, 14, 16, 18, 20]

// Multiple generators
for: [x in: 1 to: 3; y in: 1 to: 3]
yield: [{x, y}]

// Parallel iteration
for: [name in: names; age in: ages]
yield: [Person new name: name age: age]
```

---

### Supervision (OTP-inspired)

Declarative fault tolerance.

```
Supervisor subclass: WebApp
  children: [
    {DatabasePool, scale: 10},
    HTTPRouter spawn,
    {MetricsCollector, interval: 5000}
  ]
  strategy: #oneForOne

// Actor with supervision spec
Actor subclass: Worker
  supervisor: #transient  // Restart on crash, not normal exit
  maxRestarts: 5
  restartWindow: 60  // seconds
```

---

### Protocols (Elixir-inspired)

Type-based dispatch for polymorphism.

```
// Define protocol
Protocol define: #Stringable
  requiring: [#asString]

// Implement for types
Integer implements: #Stringable
  asString => self printString

Point implements: #Stringable
  asString => "({self x}, {self y})"

// Use generically
items collect: [:x | x asString]
```

---

### Optional Type Annotations (Dylan-inspired)

Types are optional - add them gradually for safety and optimization.

#### Basic Annotations

```
Actor subclass: Counter
  state: value: Integer = 0

  increment => self.value := self.value + 1
  getValue -> Integer => ^self.value

// Typed parameters
transferFrom: source: Account amount: Money -> Boolean =>
  source withdraw: amount
  self deposit: amount
  ^true
```

#### Limited Types

```
// Singleton type - exactly one value
state: direction: #north | #south | #east | #west

// Union type
state: result: Integer | Error

// False-or type (Option/Maybe)
state: cache: Integer | False = false

// Subtype constraint
process: items: <Collection> => ...
```

#### Sealing for Optimization

```
// Sealed class - no subclasses, enables optimization
sealed Actor subclass: Point
  state: x: Float, y: Float

  distanceTo: other: Point -> Float =>
    ((self.x - other x) squared + (self.y - other y) squared) sqrt

// Sealed method - final implementation
Counter >> sealed getValue -> Integer => ^self.value
```

#### BEAM Integration

- Generate Dialyzer `-spec` annotations
- Type info becomes guards: `when is_integer(X)`
- Sealed methods can bypass `gen_server` overhead

---

### Conditions and Restarts (Dylan-inspired)

Recoverable exceptions with options.

#### Traditional Exception

```
[file open: path]
  on: FileNotFound
  do: [:e | ^nil]
```

#### Conditions with Restarts

```
// Handler chooses recovery option
[file open: path]
  on: FileNotFound
  restarts: [
    #useDefault -> [^self defaultFile],
    #retry -> [^self open: (self promptForPath)],
    #createNew -> [^File create: path]
  ]

// Signaler offers restart options
FileNotFound signal: path
  restarts: [#useDefault, #retry, #createNew]
```

#### BEAM Mapping

- Conditions → `throw`/`catch` with restart metadata
- Handler selects restart; execution continues
- Unhandled → crash (supervisor takes over)

---

### Binary Pattern Matching

```
// Parse a network packet
parsePacket: <<version: 8, length: 16/big, payload: length/binary, rest/binary>> =>
  Packet new version: version payload: payload

// Build binary
packet := <<16r01, messageLength: 16/big, messageBytes/binary>>
```
