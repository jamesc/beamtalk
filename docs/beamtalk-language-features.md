# Beamtalk Language Features

Planned language features for beamtalk. See [beamtalk-principles.md](beamtalk-principles.md) for design philosophy and [beamtalk-syntax-rationale.md](beamtalk-syntax-rationale.md) for syntax design decisions.

**Status:** Active development — implemented features are stable; planned sections are marked inline.

**Syntax note:** Beamtalk uses a modernised Smalltalk syntax: `//` comments (not `"..."`), standard math precedence (not left-to-right), and optional statement terminators (newlines work).

---

## Table of Contents

- [String Encoding and UTF-8](#string-encoding-and-utf-8)
- [Core Syntax](#core-syntax)
- [Gradual Typing (ADR 0025)](#gradual-typing-adr-0025)
- [Actor Message Passing](#actor-message-passing)
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

// Escape sequences inside strings:
//   ""   doubled delimiter → literal double-quote character
//   \{   backslash preserved → literal \{  (prevents interpolation)
//   \}   backslash preserved → literal \}
quote := """"                    // 1-char string containing "
dialog := "She said ""hello"""  // → She said "hello"
// Note: \{ and \} keep the backslash in the string value (current lexer behavior)

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
// Value type - immutable data, no process (ADR 0042)
Value subclass: Point
  state: x = 0
  state: y = 0

  // Methods return new instances (immutable)
  plus: other => Point new: #{x => (self.x + other x), y => (self.y + other y)}
  printString => "Point({self.x}, {self.y})"

// Actor - process with mailbox
Actor subclass: Counter
  state: count = 0

  // Methods mutate state via message passing
  increment => self.count := self.count + 1
  getCount => ^self.count
```

`Object subclass:` also produces a value type (legacy form). Prefer `Value subclass:` for new code to make intent explicit.

**Key differences:**

| Aspect | Value Types (`Value subclass:` / `Object subclass:`) | Actors (`Actor subclass:`) |
|--------|------------------------------------------------------|----------------------------|
| Instantiation | `Point new` or `Point new: #{x => 5}` | `Counter spawn` or `Counter spawnWith: #{count => 0}` |
| Runtime | Plain Erlang map/record | BEAM process (gen_server) |
| Mutation | Immutable - methods return new instances | Mutable - methods modify state |
| Message passing | N/A (direct function calls) | Sync messages (gen_server:call) |
| Concurrency | Copied when sent between processes | Process isolation, mailbox queuing |
| Use cases | Data structures, coordinates, money | Services, stateful entities, concurrent tasks |

**Class hierarchy:**
```text
ProtoObject (minimal - identity, DNU)
  └─ Object (reflection + new)
       ├─ Integer, String (primitives)
       ├─ Value (immutable value objects)
       │    └─ Point, Color (value types)
       └─ Actor (process-based + spawn)
            └─ Counter, Server (actors)
```

**Why this matters:**
- **Performance**: Value types avoid process overhead for simple data
- **Semantics**: Clear distinction between data and concurrent entities
- **BEAM interop**: Maps naturally to Erlang records/maps vs gen_server processes
- **Idiomatic code**: Use value types for data, actors for behavior

### Value subclass: in Depth

`Value subclass:` defines an immutable value object. All slots are set at construction time; there is no mutation.

#### Construction forms

Three forms create instances — all produce equivalent results:

```beamtalk
// 1. new — all slots get their declared defaults
p := Point new                       // => Point(0, 0)

// 2. new: — provide a map of slot values; missing keys keep defaults
p := Point new: #{#x => 3, #y => 4}  // => Point(3, 4)

// 3. Keyword constructor — auto-generated from slot names
p := Point x: 3 y: 4                 // => Point(3, 4)
```

The keyword constructor form (`Point x: 3 y: 4`) is preferred for readability. The argument order follows the order the slots were declared.

#### with*: functional setters

Each slot automatically gets a `with<SlotName>:` method that returns a **new instance** with that slot changed. The original object is unchanged.

```beamtalk
p  := Point x: 1 y: 2
p2 := p withX: 10       // new object: x=10, y=2
p  x                     // => 1   (original unchanged)
p2 x                     // => 10
p2 y                     // => 2

// Chaining
p3 := (Point new withX: 5) withY: 7   // x=5, y=7
```

#### Immutability enforcement

Direct slot mutation is illegal in value types:

- **Compile-time:** `self.x := newX` inside a `Value subclass:` method is a compile error (`Cannot assign to slot`).
- **Runtime:** `fieldAt:put:` raises `immutable_value` (use `with*:` instead).

```beamtalk
// Compile error — rejected before the code runs:
// Value subclass: BadPoint
//   state: x = 0
//   badSetX: v => self.x := v   ← error: Cannot assign to slot

// Runtime error:
p := Point x: 1 y: 2
p fieldAt: #x put: 99   // raises: immutable_value
```

#### Value equality

Value objects compare by **structural equality**: two objects with the same class and the same slot values are equal (`==`).

```beamtalk
p1 := Point x: 3 y: 4
p2 := Point x: 3 y: 4
p1 == p2    // => true

p3 := Point x: 9 y: 9
p1 == p3    // => false

// with*: result equals a freshly constructed object
(p1 withX: 10) == (Point x: 10 y: 4)   // => true
```

#### Value objects in collections

Value objects work seamlessly with all collection methods:

```beamtalk
points := #((Point x: 1 y: 1), (Point x: 2 y: 2), (Point x: 3 y: 3))

// collect: transforms elements
points collect: [:p | p x]           // => #(1, 2, 3)

// select: filters elements
points select: [:p | p x > 1]        // => #(Point(2,2), Point(3,3))

// inject:into: folds
points inject: 0 into: [:sum :p | sum + p x]   // => 6
```

#### Reflection

```beamtalk
p := Point x: 3 y: 4
p fieldAt: #x          // => 3
p fieldAt: #y          // => 4
p fieldNames size      // => 2  (contains #x and #y; order is not guaranteed)
p class                // => Point
Point superclass       // => Value
```

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
- `=` - Legacy alias for `=:=` (strict equality). Prefer `=:=` instead. `beamtalk lint` warns on `x = true` / `x = false`.

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

**Non-local returns:** `^` inside a block returns from the *enclosing method*, not just from the block. This is the standard Smalltalk non-local return semantics and enables clean early-exit patterns:

```beamtalk
Object subclass: Finder
  firstPositive: items =>
    items do: [:x | x > 0 ifTrue: [^x]]
    nil   // returned only if no positive element found

Object subclass: Validator
  validate: x =>
    x isNil ifTrue: [^"missing"]
    x isEmpty ifTrue: [^"empty"]
    "ok"
```

`^` at the top level of a method body is an early return (the method exits immediately). `^` inside a block argument causes the *method* to exit with that value.

### Abstract and Stub Methods

Empty method bodies are a **compile-time error**. Use one of these two explicit forms instead:

```beamtalk
// Abstract interface contract — must be overridden by subclasses
area => self subclassResponsibility

// Work-in-progress stub — not yet implemented
processPayment => self notImplemented
```

| Method | Purpose | Error message |
|--------|---------|---------------|
| `subclassResponsibility` | Abstract method; subclass must override | `"This method is abstract and must be overridden by a subclass"` |
| `notImplemented` | Work-in-progress stub | `"Method not yet implemented"` |

Both methods raise a runtime error with a clear message. The distinction is intent: `subclassResponsibility` signals an interface contract, while `notImplemented` marks incomplete work.

---

## Gradual Typing (ADR 0025)

Beamtalk supports **optional type annotations** and **typed classes**. Type checks are compile-time warnings (not hard errors), so interactive workflows remain fast.

### Typed Class Syntax

```beamtalk
typed Actor subclass: TypedAccount
  state: balance :: Integer = 0
  state: owner :: String = ""

  deposit: amount :: Integer -> Integer =>
    self.balance := self.balance + amount
    ^self.balance

  balance -> Integer => ^self.balance
```

### Annotation Forms

```beamtalk
// Unary return annotation
getBalance -> Integer => self.balance

// Keyword parameter annotation
deposit: amount :: Integer => self.balance := self.balance + amount

// Binary parameter + return annotation
+ other :: Number -> Number => other

// Multiple keyword parameters with annotations
sum: left :: Integer with: right :: Integer -> Integer => left + right

// Union type annotations parse (full checking is phased in)
maybeName: flag :: Boolean -> Integer | String =>
  ^flag ifTrue: [1] ifFalse: ["none"]

// Self return type — resolves to the static receiver class at call sites
// (only valid in return position, not parameters)
collect: block :: Block -> Self =>
  self species withAll: (self inject: #() into: [:acc :each |
    acc addFirst: (block value: each)
  ]) reversed

// At call sites, Self resolves to the static receiver type:
// (List new collect: [:each | each])  — inferred return type: List
// (Set new collect: [:each | each])   — inferred return type: Set
```

### Current Semantics

- Type mismatch diagnostics are warnings, never compile-stopping errors.
- Invalid annotation forms (e.g., `Self` in parameter position) are errors.
- `typed` classes require parameter/return annotations on non-primitive methods.
- State annotations (`state: value :: Integer = 0`) are checked for defaults and `self.field := ...` assignments.
- Complex annotations (e.g., unions/generics) are parsed and accepted; deeper checking is phased in.
- `Self` in return position resolves to the static receiver class. Using `Self` as a parameter type is an error (unsound with subclassing).

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

## Actor Message Passing

Beamtalk uses **sync-by-default** actor message passing (ADR 0043). The `.` message send operator uses `gen_server:call`, which blocks the caller until the actor processes the message and returns a value. This is the same natural synchronous feel as Smalltalk, while preserving full process isolation and fault tolerance.

### Default: Sync with Direct Return

```beamtalk
// Load the Counter actor
:load examples/counter.bt

// Spawn an actor — returns a reference
c := Counter spawn

// Messages to actors return values directly
c increment             // => 1
c increment             // => 2
c getValue              // => 2
```

### REPL and Compiled Code

Actor sends behave identically in REPL and compiled code — both return values directly:

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

### Explicit Async Cast (`!`)

For fire-and-forget scenarios, use the `!` (bang) operator, which uses `gen_server:cast` and returns `nil` immediately:

```beamtalk
// Fire-and-forget — does not block, returns nil
c ! increment
```

Use `!` when you intentionally don't need the result and don't want to block.

### Deadlock Prevention

Because `.` sends block the caller (gen_server:call), two actors calling each other creates a deadlock. The default timeout is **5000ms**, after which a `#timeout` error is raised:

```beamtalk
// DeadlockA calls DeadlockB, which calls DeadlockA — timeout after 5s
self should: [a callPeer] raise: #timeout
```

Design actor interactions to avoid circular synchronous calls. Use `!` (cast) when an actor needs to notify another without expecting a response.

### BEAM Mapping

| Beamtalk | BEAM |
|----------|------|
| `.` send (sync) | `gen_server:call` — blocks until reply |
| `!` send (async cast) | `gen_server:cast` — returns immediately |
| Timeout | `gen_server:call` default 5000ms timeout |

### Actor-to-Actor Coordination

Because `.` sends are synchronous, when an actor method calls another actor internally, the caller **waits** for the nested call to complete before continuing. The sync barrier pattern (explicit round-trip queries) is generally no longer needed:

```beamtalk
// With sync-by-default: bus notify: calls receive: on each subscriber
// synchronously. When notify: returns, all subscribers have processed it.
bus notify: "hello".
col eventCount          // => 1 (already processed)
```

The sync barrier pattern is only needed when using `!` (cast) sends internally:

```beamtalk
// If bus uses `!` internally to forward to subscriber:
bus notify: "hello".    // bus sends subscriber ! receive: "hello" internally
col events.             // barrier: ensures col processed the cast message
col eventCount          // => 1 (now correctly reflects the event)
```

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
// Canonical Counter (already running in the workspace)
Actor subclass: Counter
  state: value = 0
  increment => self.value := self.value + 1
  getValue => self.value

// Replace a single method — existing instances pick it up immediately
Counter >> increment =>
  Telemetry log: "incrementing"
  self.value := self.value + 1

// Redefine the class to add state — new instances get the updated shape
Actor subclass: Counter
  state: value = 0
  state: lastModified = nil
  increment =>
    self.value := self.value + 1
    self.lastModified := DateTime now
  getValue => self.value
```

---

## Workspace and Reflection API

Beamtalk exposes workspace operations and system reflection as typed message sends
(ADR 0040). Two singleton objects provide the primary interface:

### `Beamtalk` — System reflection (BeamtalkInterface)

Provides access to the class registry, documentation, and system namespace.
Analogous to Pharo's `Smalltalk` image facade.

| Method | Returns | Description |
|--------|---------|-------------|
| `version` | `String` | Beamtalk version string |
| `allClasses` | `List` | All registered class names (symbols) |
| `classNamed: #Name` | `Object` or `nil` | Look up a class by name |
| `globals` | `Dictionary` | Snapshot of system namespace (class names → class objects) |
| `help: aClass` | `String` | Class documentation: name, superclass, method signatures |
| `help: aClass selector: #sel` | `String` | Documentation for a specific method |

```beamtalk
Beamtalk version
// => "0.1.0"

Beamtalk allClasses includes: #Integer
// => true

Beamtalk classNamed: #Counter
// => Counter (or nil if not loaded)

(Beamtalk globals) at: #Integer
// => Integer

(Beamtalk help: Integer)
// => "== Integer < Number ==\n..."

(Beamtalk help: Integer selector: #+)
// => "Integer >> +\n..."
```

### `Workspace` — Project operations (WorkspaceInterface)

Provides file loading, testing, and actor introspection. Scoped to the running
workspace. Analogous to Pharo's `Smalltalk` project facade.

| Method | Returns | Description |
|--------|---------|-------------|
| `load: "path"` | `nil` or Error | Compile and load a `.bt` file or directory |
| `classes` | `List` | All loaded user classes (those with a recorded source file) |
| `testClasses` | `List` | Loaded classes that inherit from `TestCase` |
| `globals` | `Dictionary` | Project namespace: singletons + loaded user classes |
| `test` | `TestResult` | Run all loaded test classes |
| `test: AClass` | `TestResult` | Run a specific test class |
| `actors` | `List` | All live actors as object references |
| `actorAt: pidStr` | `Object` or `nil` | Look up a live actor by pid string |
| `actorsOf: AClass` | `List` | All live actors of the given class |
| `bind: value as: #Name` | `Nil` | Register a value in the workspace namespace |
| `unbind: #Name` | `Nil` | Remove a registered name from the namespace |

```beamtalk
(Workspace load: "examples/counter.bt")
// => nil  (Counter is now registered)

(Workspace classes) includes: Counter
// => true

(Workspace testClasses) includes: CounterTest
// => true

(Workspace test: CounterTest) failed
// => 0  (all tests pass)

(Workspace actors) size
// => 3  (number of live actors)
```

### Class-based reload via `Behaviour >> reload`

Every class records the source file it was compiled from. You can reload a class
directly via a message send — no file path needed:

| Method | Returns | Description |
|--------|---------|-------------|
| `sourceFile` | `String` or `nil` | Path the class was compiled from; `nil` for stdlib/dynamic classes |
| `reload` | `self` | Recompile from `sourceFile`, hot-swap BEAM module |

```beamtalk
Counter sourceFile
// => "examples/counter.bt"

Counter reload
// => Counter  (recompiled and hot-swapped)

Integer sourceFile
// => nil  (stdlib built-in, no source file)

Integer reload
// => Error: Integer has no source file — stdlib classes cannot be reloaded
```

Hot-swap semantics follow BEAM conventions: live actors running the old code
continue their current message; the next dispatch uses the new code.

### REPL shortcuts (`:` commands) are thin wrappers

The REPL `:` commands are convenience aliases that desugar to the native message sends:

| REPL shortcut | Beamtalk native equivalent |
|---------------|---------------------------|
| `:load path` | `Workspace load: "path"` |
| `:reload Counter` | `Counter reload` |
| `:modules` | `Workspace classes` |
| `:test` | `Workspace test` |
| `:test CounterTest` | `Workspace test: CounterTest` |
| `:help Counter` | `Beamtalk help: Counter` |
| `:help Counter increment` | `Beamtalk help: Counter selector: #increment` |

The native forms work from compiled code, scripts, and actor methods — not just
the REPL.

---

## Namespace and Class Visibility

Beamtalk v0.1 uses a **flat global namespace** (ADR 0031). All classes are globally
visible — no `import`, `export`, or namespace declaration is needed or available.

### How loading works

When you load a file — using `:load path/to/file.bt` or `Workspace load: "path/to/file.bt"`:

1. The file is compiled to a BEAM module named `bt@class_name` (ADR 0016)
2. The module's `on_load` hook registers each class with the class registry
3. If a class with the same name already exists (from a previous load), the new
   definition **hot-reloads** the class — existing actors continue to run with the
   new code on their next message
4. The class records its source file path for future `reload` calls

```beamtalk
// Via : shortcut
:load examples/counter.bt
// => Loaded: Counter

// Via native message send (works from compiled code too)
(Workspace load: "examples/counter.bt")

c := Counter spawn
c increment
// => 1

// Reload by class name (class-based, not file-based)
Counter reload
// => Counter

// Or via : shortcut (desugars to Counter reload)
:reload Counter
// => Counter
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

### Protected stdlib class names

Beamtalk's standard library classes (e.g., `Integer`, `String`, `Array`, `Actor`,
`Object`, `Boolean`) are **protected** against redefinition in user code. There
are two layers of protection:

**Compile-time warning** — fires for all stdlib class names (both `stdlib/src/*.bt`
classes and runtime-only built-ins like `Future`):

```beamtalk
// ❌ Compile-time warning: Class name `Integer` conflicts with a stdlib class.
//    Loading will fail because stdlib class names are protected.
Object subclass: Integer
  | x |
  => x: v [ x := v ]
```

**Runtime load-time error** — fires for fully-featured stdlib classes that are
backed by `stdlib/src/*.bt` source files and loaded under the `bt@stdlib@*` module
prefix. Attempting to load user code that redefines one of these returns a
structured error:

```text
:load my_integers.bt
// => Error: Cannot redefine stdlib class 'Integer'
//    Hint: Choose a different name. `Integer` is a protected stdlib class name.
```

If you need to customise stdlib behaviour, subclass instead of redefining:

```beamtalk
// ✓ Subclass is fine
Integer subclass: SafeInteger
  => divSafe: divisor [
    divisor = 0 ifTrue: [ ^ 0 ].
    self / divisor
  ]
```


### v0.2 plan

A `import`/module system is planned for v0.2. Until then, class names must be
globally unique. See [known-limitations.md](known-limitations.md) and
[ADR 0031](ADR/0031-flat-namespace-for-v01.md) for details.

---

## Smalltalk + BEAM Mapping

| Smalltalk/Newspeak Concept | Beamtalk/BEAM Mapping |
|----------------------------|----------------------|
| Object (value type) | Plain Erlang map (no process) |
| Actor | BEAM process (gen_server) with state map |
| Class | Module + constructor function |
| Instance variable | Process state map field |
| Field access (`self.x`) | `maps:get('x', State)` |
| Field write (`self.x := v`) | `maps:put('x', v, State)` |
| `.` message send | `gen_server:call` — sync, blocks for result |
| `!` message send | `gen_server:cast` — async fire-and-forget |
| Block | Erlang fun (closure) |
| Image | Running node(s) |
| Workspace | Connected REPL to live node (`Workspace` singleton) |
| Class browser | REPL introspection: `Beamtalk allClasses`, `Beamtalk help: Class` |

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
| **Interval** | Arithmetic sequence of integers (`1 to: 10`, `1 to: 10 by: 2`) |
| **Bag** | Multiset — allows duplicate elements, counts occurrences |
| **Stream** | Lazy, closure-based sequences ([ADR 0021](ADR/0021-streams-and-io-design.md)) |
| **Nil** | Null object pattern |

For detailed API documentation, see [API Reference](https://jamesc.github.io/beamtalk/apidocs/).

### Interval — Arithmetic Sequences

An `Interval` represents an arithmetic sequence of integers without materialising a list. Create one with `to:` or `to:by:` on any `Integer`:

```beamtalk
1 to: 10                    // => (1 to: 10) — 10 elements: 1, 2, ..., 10
1 to: 10 by: 2             // => (1 to: 10 by: 2) — 5 elements: 1, 3, 5, 7, 9
10 to: 1 by: -1            // => (10 to: 1 by: -1) — 10 elements: 10, 9, ..., 1

(1 to: 10) size            // => 10
(1 to: 10) first           // => 1
(1 to: 10) last            // => 10
(1 to: 10) includes: 5     // => true

// Interval supports the full Collection protocol:
(1 to: 5) inject: 0 into: [:sum :x | sum + x]   // => 15
(1 to: 10) select: [:x | x isEven]              // => #(2, 4, 6, 8, 10)
(1 to: 5) collect: [:x | x * x]                 // => #(1, 4, 9, 16, 25)
```

### Bag — Multisets

A `Bag` is an unordered collection that allows duplicate elements. It is backed by a `Dictionary` mapping elements to occurrence counts. Like other collections, Bag is immutable — mutating operations return a new Bag.

```beamtalk
Bag new class                           // => Bag
(Bag new add: 1) occurrencesOf: 1      // => 1

b := Bag withAll: #(1, 2, 1, 3, 1)
b size                                  // => 5 (total occurrences)
b occurrencesOf: 1                      // => 3
b includes: 2                           // => true
b includes: 9                           // => false

// Bag mutating operations return new Bags:
b2 := b add: 2                         // one more occurrence of 2
b2 occurrencesOf: 2                    // => 2
b3 := b add: 4 withCount: 5           // add 5 occurrences of 4
b4 := b remove: 1                      // remove one occurrence of 1
b4 occurrencesOf: 1                    // => 2

// do: iterates each element once per occurrence:
(Bag withAll: #(1, 1, 2)) inject: 0 into: [:sum :x | sum + x]  // => 4
```

### Stream — Lazy Pipelines

Stream is Beamtalk's universal interface for sequential data. A single, sealed, closure-based type that unifies collection processing, file I/O, and generators under one protocol.

Operations are either **lazy** (return a new Stream) or **terminal** (force evaluation and return a result). Nothing computes until a terminal operation pulls elements through.

#### Constructors

```beamtalk
// Infinite stream starting from a value, incrementing by 1
Stream from: 1                     // 1, 2, 3, 4, ...

// Infinite stream with custom step function
Stream from: 1 by: [:n | n * 2]   // 1, 2, 4, 8, ...

// Stream from a collection (List, String, Set)
Stream on: #(1, 2, 3)             // wraps collection lazily

// Collection shorthand — List, String, and Set respond to `stream`
#(1, 2, 3) stream                  // same as Stream on: #(1, 2, 3)
"hello" stream                     // Stream over characters
(Set new add: 1) stream            // Stream over set elements

// Dictionary iteration — use doWithKey: instead of stream
#{#a => 1} doWithKey: [:k :v | Transcript show: k]

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

#### File I/O and Directory Operations

`File` provides class methods for reading, writing, and managing files and directories. All paths must be relative (absolute paths and `..` traversal are rejected for safety).

| Method | Returns | Description |
|--------|---------|-------------|
| `File exists: path` | `Boolean` | Test if a file exists |
| `File readAll: path` | `String` | Read entire file contents |
| `File writeAll: path contents: text` | `nil` | Write text to file (create/overwrite) |
| `File isFile: path` | `Boolean` | Test if path is a regular file |
| `File isDirectory: path` | `Boolean` | Test if path is a directory |
| `File mkdir: path` | `nil` | Create a directory (parent must exist) |
| `File mkdirAll: path` | `nil` | Create directory and all parents |
| `File listDirectory: path` | `List` | List entry names in a directory |
| `File delete: path` | `nil` | Delete a file or empty directory |
| `File deleteAll: path` | `nil` | Recursively delete a directory tree |
| `File rename: from to: to` | `nil` | Rename/move a file or directory |
| `File absolutePath: path` | `String` | Resolve relative path to absolute |
| `File tempDirectory` | `String` | OS temporary directory path |

```beamtalk
File writeAll: "output.txt" contents: "hello"
File readAll: "output.txt"              // => "hello"
File mkdirAll: "target/data/logs"
File listDirectory: "target/data"       // => ["logs"]
File rename: "output.txt" to: "target/data/output.txt"
File delete: "target/data/output.txt"
File deleteAll: "target/data"
```

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

### Diagnostic Suppression (`@expect`)

The `@expect` directive suppresses a specific category of diagnostic on the immediately following expression. It is a first-class language construct (not a comment) parsed as an expression in any expression list.

```beamtalk
@expect dnu
someObject unknownMessage   // DNU hint suppressed

@expect type
42 + "hello"                // type warning suppressed

@expect unused
x := computeSomething       // unused-variable warning suppressed

@expect all
anything                    // any diagnostic suppressed
```

**Suppression categories:**

| Category | Suppresses |
|----------|-----------|
| `dnu` | Does-not-understand hints |
| `type` | Type mismatch warnings |
| `unused` | Unused variable warnings |
| `all` | Any diagnostic on the following expression |

**Stale directives:** If `@expect` does not suppress any diagnostic (because no matching diagnostic exists on the following expression), the compiler emits an error to prevent directives from silently becoming out of date.

`@expect` works inside method bodies, class definitions, and at module scope.

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
// In stdlib/src/Integer.bt
+ other => @primitive '+'
asString => @primitive 'asString'
```

**`@intrinsic` (unquoted)** — compiler structural intrinsics:
```beamtalk
// In stdlib/src/Block.bt
value => @intrinsic blockValue
whileTrue: bodyBlock => @intrinsic whileTrue

// In stdlib/src/Actor.bt
sealed spawn => @intrinsic actorSpawn

// In stdlib/src/Object.bt
new => @intrinsic basicNew
hash => @intrinsic hash
```

The full list of structural intrinsics: `actorSpawn`, `actorSpawnWith`, `blockValue`, `blockValue1`–`blockValue3`, `whileTrue`, `whileFalse`, `repeat`, `onDo`, `ensure`, `timesRepeat`, `toDo`, `toByDo`, `basicNew`, `basicNewWith`, `hash`, `respondsTo`, `fieldNames`, `fieldAt`, `fieldAtPut`, `dynamicSend`, `dynamicSendWithArgs`, `error`.

---

### Ets — Shared In-Memory Tables

`Ets` wraps OTP ets tables for sharing mutable state between actors without message-passing overhead. Tables are named and public by default, so any process can read and write them.

#### Creating a Table

```beamtalk
// Create a named public table
cache := Ets new: #myCache type: #set

// Table types
Ets new: #t1 type: #set          // one entry per key (unordered)
Ets new: #t2 type: #orderedSet   // one entry per key (sorted keys)
Ets new: #t3 type: #bag          // multiple entries per key, values differ
Ets new: #t4 type: #duplicateBag // multiple identical entries per key

// Look up an existing named table from another actor
cache := Ets named: #myCache
```

#### Reading and Writing

```beamtalk
cache at: "key" put: 42         // insert or update
cache at: "key"                 // => 42
cache at: "missing"             // => nil (not an error)
cache at: "missing" ifAbsent: [0]  // => 0 (evaluate block when absent)
cache includesKey: "key"        // => true
cache includesKey: "other"      // => false
```

#### Other Operations

```beamtalk
cache size                      // => number of entries
cache keys                      // => List of all keys (order unspecified for #set)
cache removeKey: "key"          // delete entry; no-op if key is absent
cache delete                    // destroy the table; frees all memory
```

#### Cross-Actor Sharing

ETS tables are process-owned but publicly readable and writable. Create a named table in one actor, then retrieve it from another:

```beamtalk
// Actor A — create the table
cache := Ets new: #requestCache type: #set
cache at: "token" put: "abc123"

// Actor B — retrieve by name
cache := Ets named: #requestCache
cache at: "token"               // => "abc123"
```

#### Table Lifecycle

The owning process (the one that called `Ets new:type:`) holds the table. When that process terminates, the table is automatically deleted by the OTP runtime. Only the owning actor may call `delete` on the table — other actors should request deletion by messaging the owner. Call `delete` explicitly in the owner to release memory before the process exits.

---

### Queue — O(1) Amortised FIFO Queue

`Queue` wraps Erlang's `:queue` module, providing O(1) amortised enqueue and dequeue. It is a value type: each mutation returns a new `Queue` rather than modifying the receiver. Use `Queue` instead of `List` when O(1) amortised head/tail access matters.

#### Creating a Queue

```beamtalk
q := Queue new        // empty queue
```

#### Enqueueing and Dequeueing

```beamtalk
q2 := q enqueue: 1
q3 := q2 enqueue: 2
result := q3 dequeue         // => {1, <Queue with [2]>}
value := result at: 1       // => 1
rest := result at: 2        // => Queue containing [2]
```

`dequeue` returns a `Tuple` of `{value, newQueue}`. Raises `empty_queue` if the queue is empty.

#### Other Operations

```beamtalk
q peek                        // => front element without removing (raises empty_queue if empty)
q isEmpty                     // => true or false
q size                        // => number of elements
```

---

### AtomicCounter — Lock-Free Shared Counter

`AtomicCounter` provides a named integer counter backed by `ets:update_counter`. Increments and decrements are atomic and safe for concurrent access from multiple actors without message-passing overhead.

#### Creating a Counter

```beamtalk
c := AtomicCounter new: #requests     // create named counter starting at 0
c := AtomicCounter named: #requests   // look up existing counter from another actor
```

#### Atomic Operations

```beamtalk
c increment                    // atomically add 1, return new value
c incrementBy: 5               // atomically add N, return new value
c decrement                    // atomically subtract 1, return new value
c decrementBy: 3               // atomically subtract N, return new value
c value                        // instantaneous read; may observe a stale value under concurrent updates or reset
c reset                        // set to 0, return nil
c delete                       // destroy the backing ETS table
```

#### Cross-Actor Sharing

```beamtalk
// Actor A — create
c := AtomicCounter new: #hits

// Actor B — look up by name and increment
c := AtomicCounter named: #hits
c increment
```

#### Counter Lifecycle

Each `AtomicCounter` owns its own named ETS table. When `delete` is called, the table is destroyed and the counter is stale. Any subsequent operations on a deleted counter raise `stale_counter`.

---

See [Tooling](beamtalk-tooling.md) for CLI tools, REPL, VS Code extension, and testing framework.
