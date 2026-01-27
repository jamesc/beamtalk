# Beamtalk Language Features

Planned language features for beamtalk. See [beamtalk-principles.md](beamtalk-principles.md) for design philosophy, [beamtalk-syntax-rationale.md](beamtalk-syntax-rationale.md) for syntax design decisions, and [beamtalk-interop.md](beamtalk-interop.md) for Erlang/Elixir integration.

**Status:** Design phase - syntax and semantics subject to change.

**Syntax note:** Beamtalk uses a cleaned-up Smalltalk syntax: `//` comments (not `"..."`), standard math precedence (not left-to-right), and optional statement terminators (newlines work).

---

## Core Syntax

### Actor Definition

```
Actor subclass: Counter
  state: value = 0

  increment => self.value += 1
  decrement => self.value -= 1
  getValue => ^self.value
  incrementBy: delta => self.value += delta
```

### Message Sends

```
// Unary message
counter increment

// Binary message (standard math precedence: 2 + 3 * 4 = 14)
3 + 4

// Keyword message
array at: 1 put: 'hello'

// Cascade - multiple messages to same receiver
Transcript show: 'Hello'; cr; show: 'World'
```

### Message Precedence (high to low)
1. Unary messages: `3 factorial`
2. Binary messages: `3 + 4` (with standard math precedence within binary)
3. Keyword messages: `array at: 1`

### Blocks (Closures)

```
// Block with no arguments
[self doSomething]

// Block with arguments
[:x :y | x + y]

// Block with local variables
[:x | | temp | temp := x * 2; temp + 1]
```

---

## Async Message Passing

Beamtalk uses **async-first** message passing (like Newspeak), unlike Smalltalk's synchronous model.

### Default: Async with Futures

```
// Returns immediately with a future
result := agent analyze: data

// Explicitly wait for value
value := result await

// Continuation style - non-blocking
agent analyze: data
  whenResolved: [:value | self process: value]
  whenRejected: [:error | self handle: error]
```

### Sync Opt-In

```
// Blocks until complete - use sparingly
value := agent analyzeSync: data
```

### BEAM Mapping

| Beamtalk | BEAM |
|----------|------|
| Async send | `gen_server:cast` + future process |
| `await` | `receive` block or `gen_server:call` |
| `whenResolved:` | Callback on future completion |
| Future | Lightweight process or ref |

---

## Pattern Matching (Erlang-inspired)

Smalltalk lacks pattern matching - this is a major ergonomic addition.

### In Message Handlers

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

### Destructuring Assignment

```
// Destructure tuple
{x, y, z} := point coordinates

// Destructure in block
results collect: [{#ok, v} -> v; {#error, _} -> nil]

// Nested destructuring
{name, {street, city}} := person address
```

### Binary Pattern Matching

```
// Parse a network packet
parsePacket: <<version: 8, length: 16/big, payload: length/binary, rest/binary>> =>
  Packet new version: version payload: payload

// Build binary
packet := <<16r01, messageLength: 16/big, messageBytes/binary>>
```

---

## Pipe Operator (Elixir-inspired)

Clean data flow through transformations.

### Sync Pipe

```
data
  |> Transform with: options
  |> Filter where: [:x | x > 0]
  |> Sort by: #name
```

### Async Pipe

```
// Each step returns future, flows through
data
  |>> agent1 process
  |>> agent2 validate
  |>> agent3 store
```

---

## With Blocks (Elixir/Gleam-inspired)

Early exit on error without deep nesting.

### Problem: Nested Error Handling

```
// Ugly pyramid of doom
(file open: path) ifOk: [:f |
  (f read) ifOk: [:data |
    (Json parse: data) ifOk: [:json |
      self process: json]]]
```

### Solution: With Blocks

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

## Result Type (Gleam-inspired)

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

## Comprehensions (Elixir-inspired)

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

## Supervision (OTP-inspired)

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

## Live Patching

Hot code reload with dedicated syntax.

```
// Patch a method on running actors
patch Counter >> #increment {
  Telemetry log: 'incrementing'
  self.value += 1
}

// Patch with state migration
patch Agent >> state {
  // Add new field, migrate existing
  self.memory := self.history ifNil: [OrderedCollection new]
}
```

---

## Protocols (Elixir-inspired)

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

## Optional Type Annotations (Dylan-inspired)

Types are optional - add them gradually for safety and optimization.

### Basic Annotations

```
Actor subclass: Counter
  state: value: Integer = 0

  increment => self.value += 1
  getValue -> Integer => ^self.value

// Typed parameters
transferFrom: source: Account amount: Money -> Boolean =>
  source withdraw: amount
  self deposit: amount
  ^true
```

### Limited Types

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

### Sealing for Optimization

```
// Sealed class - no subclasses, enables optimization
sealed Actor subclass: Point
  state: x: Float, y: Float

  distanceTo: other: Point -> Float =>
    ((self.x - other x) squared + (self.y - other y) squared) sqrt

// Sealed method - final implementation
Counter >> sealed getValue -> Integer => ^self.value
```

### BEAM Integration

- Generate Dialyzer `-spec` annotations
- Type info becomes guards: `when is_integer(X)`
- Sealed methods can bypass `gen_server` overhead

---

## Conditions and Restarts (Dylan-inspired)

Recoverable exceptions with options.

### Traditional Exception

```
[file open: path]
  on: FileNotFound
  do: [:e | ^nil]
```

### Conditions with Restarts

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

### BEAM Mapping

- Conditions → `throw`/`catch` with restart metadata
- Handler selects restart; execution continues
- Unhandled → crash (supervisor takes over)

---

## Method Combinations (Dylan/CLOS-inspired)

Cross-cutting concerns via before/after/around methods.

```
// Primary method
Agent >> processMessage: msg =>
  self.model generate: msg

// Before - runs first
Agent >> before processMessage: msg =>
  Telemetry emit: #messageReceived with: msg

// After - runs last
Agent >> after processMessage: msg =>
  self.messageCount += 1

// Around - wraps
Agent >> around processMessage: msg =>
  | start result |
  start := Time now
  result := self proceed  // Call next in chain
  Metrics record: #duration value: (Time now - start)
  ^result
```

### Execution Order
```
around (enter) → before → primary → after → around (exit)
```

### Use Cases

| Combination | Use Case |
|-------------|----------|
| `before` | Validation, logging, authorization |
| `after` | Notification, cleanup, metrics |
| `around` | Timing, transactions, caching |

---

## Smalltalk + BEAM Mapping

| Smalltalk/Newspeak Concept | Beamtalk/BEAM Mapping |
|----------------------------|----------------------|
| Object | Process with state (actor) |
| Class | Module + constructor function |
| Message send | Async: `gen_server:cast` + future |
| Sync message | `gen_server:call` (opt-in) |
| Future/Promise | Lightweight process or ref |
| `await` | Block on future / `receive` |
| `whenResolved:` | Callback on future completion |
| Block | Erlang fun (closure) |
| Image | Running node(s) |
| Workspace | Connected REPL to live node |
| Class browser | Code introspection via `code:` module |
| Instance variable | Process state map |
| Global variable | Registered process or application env |

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
# Project management
beamtalk new myapp          # Create new project
beamtalk build              # Compile to BEAM
beamtalk run                # Compile and start
beamtalk test               # Run tests

# Development
beamtalk repl               # Interactive REPL
beamtalk attach node@host   # Attach to running node
beamtalk inspect pid        # Inspect process state

# Debugging
beamtalk trace actor        # Trace message flow
beamtalk profile            # Performance profiling
beamtalk observer           # Launch observer GUI
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
  Transcript log: 'incrementing'
  self.value += 1
}

// Evaluate in actor context
counter eval: [self.value := 100]
```

### Testing Framework

```
// Unit tests for actors
TestCase subclass: CounterTest

  testIncrement =>
    | counter |
    counter := Counter spawn
    counter increment await
    self assert: (counter getValue await) equals: 1

  testConcurrency =>
    | counter futures |
    counter := Counter spawn
    futures := (1 to: 100) collect: [:_ | counter increment]
    futures do: [:f | f await]
    self assert: (counter getValue await) equals: 100

// Property-based testing
PropertyTest subclass: CounterProperties

  incrementNeverNegative =>
    forAll: [n in: PositiveInteger]
    check: [
      | counter |
      counter := Counter spawn
      n timesRepeat: [counter increment await]
      (counter getValue await) >= 0
    ]
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

### From Elixir
- Pipe operator
- With blocks
- Protocols
- Comprehensions

### From Gleam
- Result types
- Use expressions
- Exhaustive matching

### From Dylan
- Sealing
- Conditions/restarts
- Method combinations
- Limited types

---

## References

- [Newspeak Language](https://newspeaklanguage.org/)
- [Dylan Reference](https://opendylan.org/documentation/)
- [Elixir Guides](https://elixir-lang.org/getting-started/)
- [Gleam Language Tour](https://gleam.run/book/tour/)
- [BEAM Book](https://blog.stenmans.org/theBeamBook/)
