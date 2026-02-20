# Beamtalk Language Features

Planned language features for beamtalk. See [beamtalk-principles.md](beamtalk-principles.md) for design philosophy and [beamtalk-syntax-rationale.md](beamtalk-syntax-rationale.md) for syntax design decisions.

**Status:** Active development — implemented features are stable; planned sections are marked inline.

**Syntax note:** Beamtalk uses a cleaned-up Smalltalk syntax: `//` comments (not `"..."`), standard math precedence (not left-to-right), and optional statement terminators (newlines work).

---

## Table of Contents

- [String Encoding and UTF-8](#string-encoding-and-utf-8)
- [Core Syntax](#core-syntax)
- [Gradual Typing (ADR 0025)](#gradual-typing-adr-0025)
- [Async Message Passing](#async-message-passing)
- [Pattern Matching](#pattern-matching)
- [Live Patching](#live-patching)
- [Namespace and Class Visibility](#namespace-and-class-visibility)
- [Smalltalk + BEAM Mapping](#smalltalk--beam-mapping)
- [Tooling](#tooling)
- [Inspiration Sources](#inspiration-sources)
- [References](#references)

---

## String Encoding and UTF-8

**Beamtalk strings are UTF-8 by default.** This follows modern BEAM conventions and matches Elixir's approach.

### String Types

```beamtalk
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

```beamtalk
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

---

## Core Syntax

### Actor Definition

```beamtalk
Actor subclass: Counter
  state: value = 0

  increment => self.value := self.value + 1
  decrement => self.value := self.value - 1
  getValue => ^self.value
  incrementBy: delta => self.value := self.value + delta
```

### Value Types vs Actors

Beamtalk distinguishes between **value types** (immutable data) and **actors** (concurrent processes):

```beamtalk
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
```text
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

```beamtalk
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
```beamtalk
// Short-circuit AND - second block only evaluated if first is true
result := condition and: [expensiveCheck()]

// Short-circuit OR - second block only evaluated if first is false  
result := condition or: [fallbackValue()]
```

### Field Access and Assignment

Direct field access within actors using dot notation:

```beamtalk
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

```beamtalk
// Assignment as expression (returns 6)
(self.x := 5) + 1

// Field assignments as sequential statements
self.x := 5
self.y := self.x + 1
^self.y
```

**Limitation:** Field assignments in stored blocks (non-control-flow) are not supported:

```beamtalk
// NOT SUPPORTED: field assignment inside stored blocks
nestedBlock := [:m | self.x := m]
nestedBlock value: 10

// SUPPORTED: field mutation in control flow blocks
true ifTrue: [self.x := 5]
```

### Blocks (Closures)

```beamtalk
// Block with no arguments
[self doSomething]

// Block with arguments
[:x :y | x + y]

// Block with local variables
[:x | temp := x * 2. temp + 1]
```

---

## Gradual Typing (ADR 0025)

Beamtalk supports **optional type annotations** and **typed classes**. Type checks are compile-time warnings (not hard errors), so interactive workflows remain fast.

### Typed Class Syntax

```beamtalk
typed Actor subclass: TypedAccount
  state: balance: Integer = 0
  state: owner: String = ""

  deposit: amount: Integer -> Integer =>
    self.balance := self.balance + amount
    ^self.balance

  balance -> Integer => ^self.balance
```

### Annotation Forms

```beamtalk
// Unary return annotation
getBalance -> Integer => self.balance

// Keyword parameter annotation
deposit: amount: Integer => self.balance := self.balance + amount

// Keyword parameter annotation (space before colon also supported)
deposit: amount : Integer => self.balance := self.balance + amount

// Binary parameter + return annotation
+ other : Number -> Number => other

// Multiple keyword parameters with annotations
sum: left: Integer with: right: Integer -> Integer => left + right

// Union type annotations parse (full checking is phased in)
maybeName: flag: Boolean -> Integer | String =>
  ^flag ifTrue: [1] ifFalse: ["none"]
```

### Current Semantics

- Type diagnostics are warnings, never compile-stopping errors.
- `typed` classes require parameter/return annotations on non-primitive methods.
- State annotations (`state: value: Integer = 0`) are checked for defaults and `self.field := ...` assignments.
- Complex annotations (e.g., unions/generics) are parsed and accepted; deeper checking is phased in.

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

```beamtalk
// Simple counter
count := 0
[count < 10] whileTrue: [count := count + 1]
// count is now 10

// Multiple variables
sum := 0
product := 1
i := 1
[i <= 5] whileTrue: [
    sum := sum + i
    product := product * i
    i := i + 1
]
// sum = 15, product = 120, i = 6

// Collection iteration
numbers := #(1, 2, 3, 4, 5)
total := 0
numbers do: [:n | total := total + n]
// total = 15

// With index
result := 0
1 to: 10 do: [:n | result := result + n]
// result = 55 (sum of 1..10)
```

### Field Mutations

Mutations to actor state (`self.field`) work the same way:

```beamtalk
Actor subclass: Counter
  state: value = 0
  state: count = 0

  // Field mutation in control flow
  increment =>
    [self.value < 10] whileTrue: [
      self.value := self.value + 1
    ]
    ^self.value

  // Multiple fields
  incrementBoth =>
    [self.value < 10] whileTrue: [
      self.value := self.value + 1
      self.count := self.count + 1
    ]
```

### Mixed Mutations

Local variables and fields can be mutated together:

```beamtalk
processItems =>
    total := 0
    self.processed := 0
    
    self.items do: [:item |
        total := total + item
        self.processed := self.processed + 1
    ]
    
    ^total
```

### What's Forbidden

Stored or passed closures cannot mutate:

```beamtalk
// ❌ ERROR: Field assignment in stored closure
badBlock =>
    myBlock := [self.value := self.value + 1]
    // ERROR: Cannot assign to field 'value' inside a stored closure.
    
// ⚠️ WARNING: Local mutation in stored closure has no effect
testWarning =>
    count := 0
    myBlock := [count := count + 1]
    // WARNING: Assignment to 'count' has no effect on outer scope.
    
    10 timesRepeat: myBlock
    ^count  // Still 0, not 10
    
// ✅ CORRECT: Use literal blocks in control flow
testCorrect =>
    count := 0
    10 timesRepeat: [count := count + 1]  // ✅ Works!
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

```beamtalk
// This won't compile — stored closure can't mutate fields
myBlock := [:item | self.sum := self.sum + item]
items do: myBlock
```

```text
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

```beamtalk
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

```text
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

### Destructuring in Match Arms

Pattern matching can bind variables in match arms:

```beamtalk
// Variable captures the matched value
42 match: [x -> x + 1]
// => 43

// Variable binding with guard
10 match: [x when: [x > 100] -> "big"; x when: [x > 5] -> "medium"; _ -> "small"]
// => "medium"

// Tuple destructuring in match arms (parser supports; runtime planned)
// {1, 2} match: [{a, b} -> a + b]
```

> **Note:** Destructuring assignment (`{x, y} := expr`) and `collect:` with pattern blocks are planned but not yet implemented.

---

## Live Patching

Hot code reload via message sends — no dedicated `patch` syntax needed.

```beamtalk
// Replace a method on a running class
Counter >> increment =>
  Telemetry log: "incrementing"
  self.value := self.value + 1

// Redefine a class to update all future instances
Actor subclass: Counter
  state: value = 0, lastModified = nil
  increment => self.value := self.value + 1
  getValue => ^self.value
```

---

## Namespace and Class Visibility

Beamtalk v0.1 uses a **flat global namespace** (ADR 0031). All classes are globally
visible — no `import`, `export`, or namespace declaration is needed or available.

### How `:load` works

When you use `:load path/to/file.bt` in the REPL:

1. The file is compiled to a BEAM module named `bt@class_name` (ADR 0016)
2. The module's `on_load` hook registers each class with the class registry
3. If a class with the same name already exists (from a previous `:load`), the new
   definition **hot-reloads** the class — existing actors continue to run with the
   new code on their next message

```beamtalk
:load examples/counter.bt
// => Loaded: Counter

c := Counter spawn
c increment
// => 1

// Reloading the same file updates the class silently (same BEAM module)
:load examples/counter.bt
// => Loaded: Counter
```

### Class collision warnings

If two files from **different packages** define the same class name, the BEAM module
atoms differ (e.g. `bt@counter` vs `bt@other_pkg@counter`), and Beamtalk emits a
warning to alert you to the collision:

```text
:load my_app/counter.bt
// => Loaded: Counter

:load other_pkg/counter.bt
// => Loaded: Counter
// warning: Class 'Counter' redefined (was bt@counter, now bt@other_pkg@counter)
```

The second definition wins — the class is hot-reloaded with the new implementation.

### Naming conventions

To avoid collisions, use **package-specific prefixes** for classes that might conflict:

```beamtalk
// ❌ Too generic — likely to collide with other packages
Object subclass: Logger ...

// ✓ Package-scoped name — unlikely to collide
Object subclass: MyAppLogger ...
```

### v0.2 plan

A `import`/module system is planned for v0.2. Until then, class names must be
globally unique. See [known-limitations.md](known-limitations.md) and
[ADR 0031](ADR/0031-flat-namespace-for-v01.md) for details.

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
| `await` | Block on future / `receive` |
| Block | Erlang fun (closure) |
| Image | Running node(s) |
| Workspace | Connected REPL to live node |
| Class browser | REPL introspection: `Beamtalk allClasses`, `:h Class` |

---

## Standard Library

Core classes implemented and tested:

| Class | Description |
|-------|-------------|
| **Actor** | Base class for all actors (BEAM processes) |
| **Block** | First-class closures |
| **Boolean** | `True` and `False` with control flow |
| **Integer** | Arbitrary precision arithmetic |
| **String** | UTF-8 text with operations |
| **List** | Linked list with fast prepend (`#()` syntax) |
| **Dictionary** | Key-value map |
| **Set** | Unordered unique elements |
| **Stream** | Lazy, closure-based sequences ([ADR 0021](ADR/0021-streams-and-io-design.md)) |
| **Nil** | Null object pattern |

For detailed API documentation, see [API Reference](https://jamesc.github.io/beamtalk/apidocs/).

### Stream — Lazy Pipelines

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

The standard library uses pragma annotations to declare methods whose implementations are provided by the compiler or runtime rather than written in Beamtalk code.

There are two pragma forms:

| Pragma | Syntax | Purpose |
|--------|--------|---------|
| `@primitive 'selector'` | Quoted selector | **Selector-based dispatch** — routes through runtime dispatch modules (`beamtalk_primitive.erl` → type-specific modules). Used for arithmetic, comparison, string operations, etc. |
| `@intrinsic name` | Unquoted identifier | **Structural intrinsic** — the compiler generates specialized code inline. Used for spawning, block evaluation, control flow, reflection, etc. |

Both forms are semantically equivalent at the compiler level (they produce the same AST node), but the naming convention distinguishes their intent:

**`@primitive` (quoted)** — runtime-dispatched method implementations:
```beamtalk
// In lib/Integer.bt
+ other => @primitive '+'
asString => @primitive 'asString'
```

**`@intrinsic` (unquoted)** — compiler structural intrinsics:
```beamtalk
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

### CLI Tools

```bash
# Project management
beamtalk new myapp          # Create new project
beamtalk build              # Compile to BEAM
beamtalk run                # Compile and start
beamtalk check              # Check for errors without compiling
beamtalk daemon start/stop  # Manage compiler daemon

# Development
beamtalk repl               # Interactive REPL
```

### REPL Features

```beamtalk
// Spawn and interact
counter := Counter spawn
counter increment
counter getValue await  // => 1
```

### VS Code Extension

The [Beamtalk VS Code extension](https://github.com/jamesc/beamtalk/tree/main/editors/vscode) provides:

- Syntax highlighting for `.bt` files
- Language Server Protocol (LSP) integration
- Error diagnostics with source spans

### Testing Framework

Beamtalk includes a native test framework inspired by Smalltalk's SUnit.

```beamtalk
// test/counter_test.bt

TestCase subclass: CounterTest

  testInitialValue =>
    self assert: (Counter spawn getValue await) equals: 0

  testIncrement =>
    self assert: (Counter spawn increment await) equals: 1

  testMultipleIncrements =>
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
beamtalk test
```

#### REPL Integration

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
