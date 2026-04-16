# Beamtalk Language Features

Language features for Beamtalk. See [beamtalk-principles.md](beamtalk-principles.md) for design philosophy and [beamtalk-syntax-rationale.md](beamtalk-syntax-rationale.md) for syntax design decisions.

**Status:** v0.3.1 — implemented features are stable, including generics, protocols, union types, and control flow narrowing. See [ADR 0068](ADR/0068-parametric-types-and-protocols.md) for the type system design.

**Syntax note:** Beamtalk uses a modernised Smalltalk syntax: `//` comments (not `"..."`), standard math precedence (not left-to-right), and optional statement terminators (newlines work).

---

## Table of Contents

- [String Encoding and UTF-8](#string-encoding-and-utf-8)
- [Core Syntax](#core-syntax)
  - [Class-Side Methods (ADR 0048)](#class-side-methods-adr-0048)
  - [Doc Comments (`///`)](#doc-comments-)
  - [Erlang FFI](#erlang-ffi)
  - [Loading Code into the Workspace](#loading-code-into-the-workspace)
- [Gradual Typing (ADR 0025)](#gradual-typing-adr-0025)
- [Parametric Types — Generics (ADR 0068)](#parametric-types--generics-adr-0068)
- [Structural Protocols (ADR 0068)](#structural-protocols-adr-0068)
  - [Printable Protocol and Display Methods](#printable-protocol-and-display-methods)
- [Union Types and Narrowing (ADR 0068)](#union-types-and-narrowing-adr-0068)
- [Actor Message Passing](#actor-message-passing)
- [Server — OTP Interop (ADR 0065)](#server--otp-interop-adr-0065)
- [Supervision Trees (ADR 0059)](#supervision-trees-adr-0059)
- [Named Actor Registration (ADR 0079)](#named-actor-registration-adr-0079)
- [Pattern Matching](#pattern-matching)
- [Live Patching](#live-patching)
- [Actor Observability and Tracing (ADR 0069)](#actor-observability-and-tracing-adr-0069)
- [Namespace and Class Visibility](#namespace-and-class-visibility)
  - [Visibility and Access Control (ADR 0071)](#visibility-and-access-control-adr-0071)
- [Smalltalk + BEAM Mapping](#smalltalk--beam-mapping)
- [Tooling](#tooling)
- [Inspiration Sources](#inspiration-sources)
- [References](#references)

---

## String Encoding and UTF-8

**Beamtalk strings are UTF-8 by default.** This follows modern BEAM conventions and matches Elixir's approach. String is a subclass of Binary (`Collection > Binary > String`) — see [Binary — Byte-Level Data](#binary--byte-level-data) for byte-level operations inherited by String.

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

### Inherited Byte-Level Methods

String inherits byte-level methods from Binary. These provide unambiguous byte access regardless of grapheme semantics:

```beamtalk
// Byte access (inherited from Binary)
"hello" byteAt: 0      // => 104 (byte value, 0-based)
"hello" byteSize       // => 5 (byte count)
"café" byteSize        // => 5 (bytes — more than 4 graphemes due to UTF-8)

// Byte-level slicing returns Binary, not String
"hello" part: 0 size: 3  // => Binary (raw bytes, not String)

// Byte-level concatenation returns Binary
"hello" concat: " world"  // => Binary (use ++ for String concatenation)

// Byte list conversion
"hello" toBytes        // => #(104, 101, 108, 108, 111)
```

See [Binary — Byte-Level Data](#binary--byte-level-data) for the full Binary API.

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
  getValue => self.value
  incrementBy: delta => self.value := self.value + delta
```

### Actor Lifecycle Hooks

Actors support two optional lifecycle hooks:

- **`initialize`** — called automatically after `spawn`, before the actor is returned to the caller. Use it to set up resources or compute derived state.
- **`terminate: reason`** — called automatically during graceful shutdown (`stop`). Use it to clean up resources. The `reason` parameter indicates why the actor is stopping (e.g., `#normal`).

```beamtalk
Actor subclass: ResourceActor
  state: handle = nil

  initialize =>
    self.handle := Resource open

  terminate: reason =>
    self.handle isNil ifFalse: [self.handle close]

  doWork => self.handle process
```

**Key behaviour:**

| Aspect | `initialize` | `terminate:` |
|--------|--------------|--------------|
| Called on | `spawn` / `spawnWith:` | `stop` (graceful shutdown) |
| Error effect | Spawn fails with `InstantiationError` | Shutdown proceeds anyway |
| Called on `kill`? | N/A | No — `kill` bypasses `terminate:` |
| Actor state | Accessible via `self.field` | Accessible via `self.field` |

Both hooks are optional — actors without them work normally.

### Three Class Kinds (ADR 0067)

Beamtalk has three class kinds with distinct data keywords and construction protocols:

| Class Kind | Data Keyword | Semantics | Construction | Instance Process |
|------------|-------------|-----------|--------------|-----------------|
| **Value** | `field:` | Immutable data slots, `self.slot :=` is compile error | `new` / `new:` / keyword ctor | No |
| **Actor** | `state:` (permitted, not required) | Mutable process state, `self.slot :=` persists via gen_server | `spawn` / `spawnWith:` | Yes |
| **Object** | *(none)* | No Beamtalk-managed data; often class-methods-only, but can have instances with runtime-backed state (ETS, handles) | Custom constructors | No |

```beamtalk
// Value — immutable data, no process (ADR 0042)
Value subclass: Point
  field: x = 0
  field: y = 0

  // Methods return new instances (immutable)
  plus: other => Point new: #{x => (self.x + other x), y => (self.y + other y)}
  printString => "Point({self.x}, {self.y})"

// Actor — process with mailbox
Actor subclass: Counter
  state: count = 0

  // Methods mutate state via message passing
  increment => self.count := self.count + 1
  getCount => self.count

// Object — no Beamtalk-managed data; commonly class-methods-only
Object subclass: MathHelper
  class factorial: n =>
    n <= 1
      ifTrue: [1]
      ifFalse: [n * (self factorial: n - 1)]
```

**Key differences:**

| Aspect | Value (`Value subclass:`) | Actor (`Actor subclass:`) | Object (`Object subclass:`) |
|--------|--------------------------|---------------------------|----------------------------|
| Data keyword | `field:` | `state:` | *(none — compile error)* |
| Instantiation | `Point new` or `Point x: 3 y: 4` | `Counter spawn` or `Counter spawnWith: #{count => 0}` | Not instantiable |
| Runtime | Plain Erlang map | BEAM process (gen_server) | Class methods only |
| Mutation | Immutable — methods return new instances | Mutable — methods modify state | N/A |
| Message passing | N/A (direct function calls) | Sync messages (gen_server:call) | N/A |
| Equality | Structural (by value) | Identity (by process) | N/A |
| Use cases | Data structures, coordinates, money | Services, stateful entities, concurrent tasks | FFI namespaces, protocol providers, abstract bases |

**Class hierarchy:**
```text
ProtoObject (minimal — identity, DNU)
  └─ Object (protocol provider — reflection, equality, error handling)
       ├─ Integer, String (primitives)
       ├─ Value (immutable value objects — field:)
       │    ├─ Point, Color (value types)
       │    ├─ Collection (abstract)
       │    │    └─ Set, Bag, Interval
       │    └─ TestCase (BUnit test base)
       └─ Actor (process-based — state: + spawn)
            └─ Counter, Server (actors)
```

**Why this matters:**
- **Clarity**: The data keyword tells you the mutability contract — `field:` = fixed, `state:` = changeable — without looking up the class hierarchy
- **Safety**: Using the wrong keyword is a compile error, not a silent runtime footgun
- **Performance**: Value types avoid process overhead for simple data
- **BEAM alignment**: `field:` maps to Erlang maps, `state:` maps to gen_server state

### Object's Three Roles

`Object subclass:` cannot have instance data (`field:` or `state:` is a compile error). Object serves three purposes:

1. **Protocol provider** — common methods inherited by all Value and Actor subclasses: `isNil`, `respondsTo:`, `printString`, `hash`, `error:`, `yourself`, `show:`, `showCr:` (debug output to Transcript; replaces the former `trace:`/`traceCr:` which are deprecated aliases)
2. **FFI namespace** — zero-overhead class-method wrappers around Erlang modules and OTP primitives (e.g., `Json`, `System`, `File`, `Ets`, `Random`). No instances, no process
3. **Abstract extension point** — framework contracts designed for subclassing, where subclasses define methods but hold no data (e.g., `Supervisor`, `DynamicSupervisor`)

```beamtalk
// FFI namespace — wraps Erlang modules as class methods
Object subclass: Json
  class parse: str => // ... Erlang FFI
  class stringify: obj => // ... Erlang FFI

// Abstract extension point — designed for subclassing
abstract Object subclass: Supervisor
  class children => self subclassResponsibility
```

### Wrong Keyword Errors

The compiler enforces keyword/class-kind rules with clear error messages:

```beamtalk
// state: on a Value — compile error
Value subclass: BadValue
  state: x = 0
// error: use 'field:' for Value subclass data declarations, not 'state:'

// field: on an Actor — compile error
Actor subclass: BadActor
  field: x = 0
// error: use 'state:' for Actor subclass data declarations, not 'field:'

// Any data declaration on an Object — compile error
Object subclass: BadObject
  state: x = 0
// error: Object subclass cannot have instance data declarations;
//   use 'Value subclass:' for immutable data or 'Actor subclass:' for mutable state
```

### Class Modifiers

Class definitions support optional modifier keywords before the superclass:

| Modifier | Meaning | Example |
|----------|---------|---------|
| `sealed` | Cannot be subclassed by user code | `sealed Object subclass: Stream` |
| `abstract` | Must be subclassed; cannot be instantiated directly | `abstract Object subclass: Supervisor` |
| `typed` | All fields and methods require type annotations (ADR 0025) | `typed Actor subclass: TypedAccount` |

Modifiers can be combined: `sealed typed Collection subclass: Array`.

Most stdlib classes are `sealed` — this prevents user code from subclassing built-in types like `Integer`, `String`, `Array`, `Result`, and `Stream`. If you need custom behaviour, compose with these types rather than subclassing them.

**Performance:** Sealed actor classes benefit from a direct-call optimization — self-sends within the class emit direct function calls instead of dynamic dispatch, since the compiler knows no subclass can override the method. This is automatic and requires no user intervention.

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
//   field: x = 0
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
2 + 3 * 4

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
result := condition and: [self expensiveCheck]

// Short-circuit OR - second block only evaluated if first is false  
result := condition or: [self fallbackValue]
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
self.y
```

**Limitation:** Field assignments (`self.x :=`) in stored closures are a compile error — they require control-flow context for state threading. Local variable mutations in stored closures work fine (ADR 0041 Tier 2).

```beamtalk
// ❌ ERROR: field assignment inside stored closure
nestedBlock := [:m | self.x := m]

// ✅ Field mutation in control flow blocks
true ifTrue: [self.x := 5]

// ✅ Local variable mutation in stored closure (Tier 2)
count := 0
myBlock := [count := count + 1]
10 timesRepeat: myBlock   // count => 10
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

### Class-Side Methods (ADR 0048)

Methods prefixed with `class` belong to the class itself, not to instances. They are called on the class name directly.

```beamtalk
Object subclass: MathUtils
  class factorial: n =>
    n <= 1
      ifTrue: [1]
      ifFalse: [n * (self factorial: n - 1)]

  class fibonacci: n =>
    n <= 1
      ifTrue: [n]
      ifFalse: [(self fibonacci: n - 1) + (self fibonacci: n - 2)]

MathUtils factorial: 10    // => 3628800
MathUtils fibonacci: 10    // => 55
```

Common uses:
- **Factory methods:** `Counter spawn`, `Point x: 3 y: 4` (auto-generated from `field:` declarations)
- **FFI namespaces:** `File readAll: path`, `Json parse: str` — class methods wrapping Erlang modules
- **Supervisor configuration:** `class children`, `class strategy` — pure metadata for OTP init
- **Singleton access:** `class current` — return the singleton instance (see `classState:` below)

**`classState:`** declares mutable class-level state — shared across all instances and accessible from class methods. Used for singletons:

```beamtalk
sealed Object subclass: MyRegistry
  classState: current = nil

  class current => self.current
  class current: instance => self.current := instance
```

`classState:` is distinct from `state:` (per-instance actor state) and `field:` (per-instance immutable data). It stores values at the class level, analogous to Smalltalk class variables.

### Doc Comments (`///`)

Triple-slash comments (`///`) are structured documentation parsed into the AST and queryable at runtime via `Beamtalk help:`. They support Markdown formatting and a `## Examples` convention with fenced code blocks.

```beamtalk
/// Counter — A simple incrementing actor.
///
/// Demonstrates actor state and message passing.
///
/// ## Examples
/// ```beamtalk
/// c := Counter spawn
/// c increment   // => 1
/// ```
Actor subclass: Counter
  state: value = 0

  /// Increment the counter by 1 and return the new value.
  ///
  /// ## Examples
  /// ```beamtalk
  /// Counter spawn increment   // => 1
  /// ```
  increment => self.value := self.value + 1
```

Query documentation at runtime:

```beamtalk
Beamtalk help: Counter
// => "== Counter < Actor ==\n  increment\n  ..."

Beamtalk help: Counter selector: #increment
// => "Counter >> increment\n  Increment the counter by 1..."
```

Doc comments flow from source → AST → compiled BEAM module → runtime. They are not stripped at compilation. The `## Examples` blocks are the source for `Beamtalk help:` output and can be verified by the test framework.

### Erlang FFI

Beamtalk provides direct access to all Erlang modules via the `Erlang` gateway object (ADR 0028). Send a unary message with the module name to get a proxy, then send messages as normal:

```beamtalk
// Call any Erlang module function
Erlang lists reverse: #(3, 2, 1)      // => [1, 2, 3]
Erlang erlang node                     // => current node atom
Erlang maps merge: #{#a => 1} with: #{#b => 2}

// Store a module proxy for repeated use
proxy := Erlang crypto
proxy strong_rand_bytes: 16            // => random binary
```

The `(Erlang module)` pattern is used throughout the stdlib to wrap Erlang functions as Beamtalk class methods:

```beamtalk
// How File.readAll: is implemented — a thin wrapper
Object subclass: File
  class readAll: path :: String -> String =>
    (Erlang beamtalk_file) readAll: path
```

**Keyword mapping:** Beamtalk keyword selectors map to Erlang function names by joining with underscores. `Erlang maps merge: a with: b` calls `maps:merge_with(A, B)`. Unary selectors map directly: `Erlang erlang node` calls `erlang:node()`.

**Result conversion (ADR 0076):** Erlang functions that return `{ok, Value}` or `{error, Reason}` tuples are automatically converted to `Result` objects at the FFI boundary. This means FFI calls use the same error-handling idiom as native Beamtalk code:

```beamtalk
// FFI calls returning ok/error tuples become Result objects
result := Erlang file read_file: "/tmp/hello.txt"
result              // => Result ok: "Hello, world!\n"
result value        // => "Hello, world!\n"

// Use Result combinators directly on FFI returns
result map: [:content | content size]
// => Result ok: 14

// Error path
result := Erlang file read_file: "/nonexistent"
result              // => Result error: enoent
result isError      // => true

// Chain FFI calls with andThen:
(Erlang file read_file: "/tmp/config.json")
  andThen: [:content | Erlang json decode: content]
  mapError: [:e | "Config load failed: " ++ e message]

// Bare ok atoms (e.g. file:write_file/2) become Result ok: nil
Erlang file write_file: "/tmp/out.txt" with: "data"
// => Result ok: nil
```

**Conversion rules:**
- `{ok, Value}` becomes `Result ok: Value`
- `{error, Reason}` becomes `Result error: Reason`
- Bare `ok` atom becomes `Result ok: nil`
- Bare `error` atom becomes `Result error: nil`
- Tuples with 3+ elements, non-ok/error tuples, and non-tuple values pass through unchanged

**Scope:** Conversion applies only to FFI calls via `Erlang module method: args`. Messages received from Erlang processes via `receive` or actor mailboxes remain raw Tuples. Use `Result fromTuple:` to explicitly convert those:

```beamtalk
// Converting a Tuple received from a message
tuple := receiveMessage  // raw {ok, data} Tuple from Erlang process
result := Result fromTuple: tuple
result value  // => data
```

**Migration from Tuple-based FFI code:**

```beamtalk
// Before (Tuple-based, pre-ADR-0076 — FFI returned raw Tuples):
result := Erlang file read_file: path
result isOk ifTrue: [result unwrap] ifFalse: ["error"]  // Tuple methods

// After (Result-based):
result := Erlang file read_file: path
result ifOk: [:content | content] ifError: [:e | "error"]
// Or simply:
result value  // raises on error
```

**Error handling:** Errors from Erlang calls are wrapped as `BEAMError`, `ExitError`, or `ThrowError` — catchable with `on:do:`:

```beamtalk
[Erlang erlang error: #badarg] on: BEAMError do: [:e | e message]
// => "badarg"
```

### Type Specs for Native Modules

When writing native Erlang modules that implement Beamtalk class methods, prefer the exported Dialyzer-facing types for `Result` and wrapped error values instead of bare `map()`. These types are primarily useful for Erlang `-spec` annotations and Dialyzer; Beamtalk's spec importer may resolve them to `Dictionary` in generated Beamtalk signatures rather than `Result(...)` / `Exception`.

| Type | Description |
|------|-------------|
| `beamtalk_result:t()` | Any Result (unparameterized) |
| `beamtalk_result:t(OkType, ErrType)` | Result with known ok/error types |
| `beamtalk_error:t()` | Exception tagged map (the wrapped error visible to Beamtalk) |

**Example specs:**

```erlang
-module(beamtalk_mylib).
-include_lib("beamtalk_runtime/include/beamtalk.hrl").

%% Unparameterized — any Result
-spec 'readConfig:'(binary()) -> beamtalk_result:t().
'readConfig:'(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            beamtalk_result:from_tagged_tuple({ok, Bin});
        {error, Reason} ->
            Err0 = beamtalk_error:new(io_error, 'MyLib', 'readConfig:'),
            Err1 = beamtalk_error:with_details(Err0, #{path => Path, reason => Reason}),
            beamtalk_result:from_tagged_tuple({error, Err1})
    end.

%% Parameterized — precise ok/error types for Dialyzer
-spec 'parse:'(binary()) -> beamtalk_result:t(map(), beamtalk_error:t()).
'parse:'(Data) -> ...
```

These types are defined in:
- `runtime/apps/beamtalk_stdlib/src/beamtalk_result.erl` — `t/0` and `t/2`
- `runtime/apps/beamtalk_runtime/src/beamtalk_error.erl` — `t/0` and `error/0`

### Loading Code into the Workspace

Beamtalk source files are loaded into the live workspace via `:load` or the `Workspace` singleton. Loaded classes are immediately available — existing actors pick up new code on next dispatch.

```beamtalk
// Via REPL shortcut
:load examples/counter.bt
// => Loaded: Counter

// Via native message send (works from compiled code and MCP)
Workspace load: "examples/counter.bt"

// Load an entire directory (compiles all .bt files in dependency order)
Workspace load: "src/"

// Reload a specific class from its source file
Counter reload
// => Counter  (recompiled and hot-swapped)

// Or via REPL shortcut
:reload Counter
```

See [Workspace and Reflection API](#workspace-and-reflection-api) for the full `Workspace` singleton interface.

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
    self.balance

  balance -> Integer => self.balance
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
- Data annotations (`field: x :: Integer = 0` on Value, `state: x :: Integer = 0` on Actor) are checked for defaults and assignments. When a type annotation is present, the default value is optional (`field: x :: Integer` / `state: x :: Integer`) — the type annotation is the contract, and `nil` is used internally.
- Complex annotations (e.g., unions/generics) are parsed and accepted; deeper checking is phased in.
- `Self` in return position resolves to the static receiver class. Using `Self` as a parameter type is an error (unsound with subclassing).

### Dynamic Type Visibility (ADR 0077)

When the compiler cannot determine an expression's type, it infers `Dynamic`. Beamtalk makes Dynamic visible so you can see exactly where the compiler lacks type information and why.

#### Dynamic with Reasons

Each `Dynamic` type carries a reason explaining why the type could not be determined:

| Reason | Description | What to fix |
|--------|-------------|-------------|
| unannotated parameter | Parameter has no type annotation | Add `:: Type` to the parameter |
| unannotated return | Method has no return type and body could not be inferred | Add `-> Type` return annotation |
| dynamic receiver | Receiver is Dynamic, so message send result is Dynamic | Fix the receiver's type first |
| ambiguous control flow | Control flow produces incompatible types | Add type annotations to branches |
| untyped FFI | Erlang FFI call with no spec or all-Dynamic spec | Add `-spec` to the Erlang module |
| *(none)* | Fallback — no specific reason available | Shown as plain `Dynamic` |

#### LSP Hover

When hovering over an expression in the editor, the LSP shows the inferred type including Dynamic with its reason:

```text
Identifier: `handler` — Type: Dynamic (unannotated return)
Identifier: `result` — Type: Dynamic (dynamic receiver)
Identifier: `data` — Type: Dynamic (unannotated parameter)
Identifier: `count` — Type: Integer
```

When the reason is Unknown, the hover shows just `Type: Dynamic`. Previously, Dynamic expressions showed no type information at all — the type line was omitted entirely.

### Typed Class Diagnostics

`typed` classes opt into thorough type checking. In addition to requiring parameter and return annotations on methods, `typed` classes produce warnings for:

#### Missing State Field Annotations

State fields without type annotations produce a warning:

```beamtalk
typed Actor subclass: BankAccount
  state: balance = 0          // warning: Missing type annotation for state field
                               //          `balance` in typed class `BankAccount`
  state: owner :: String = ""  // OK — annotated
```

#### Dynamic Inference Warnings

When an expression in a `typed` class infers as Dynamic (for a root-cause reason like `unannotated parameter` or `unannotated return`), the compiler warns. Propagated reasons like `dynamic receiver` are not warned on separately since the root cause already has its own warning.

```beamtalk
typed Actor subclass: BankAccount
  process: handler =>
    handler doWork    // warning: expression inferred as Dynamic in typed class
                      //          `BankAccount` (unannotated parameter)
```

### Suppressing Type Warnings with `@expect type`

When Dynamic dispatch is intentional (e.g., a method that deliberately accepts any object), suppress the warning with `@expect type` on the preceding line:

```beamtalk
typed Actor subclass: BankAccount
  process: handler =>
    @expect type
    handler doWork    // no warning — suppressed
```

`@expect type` suppresses all type-related warnings on the next expression, including type mismatches, does-not-understand hints, and Dynamic inference warnings. `@expect all` also works as a broader suppression.

---

## Parametric Types — Generics (ADR 0068)

Beamtalk supports **declaration-site parametric types** (generics) with compile-time substitution. Type parameters use **parenthesis syntax** — `Result(T, E)` — keeping `<` reserved exclusively as a binary message (comparison operator). All generic type information is erased at runtime (zero cost).

### Declaring a Generic Class

Classes declare type parameters in parentheses after the class name:

```beamtalk
sealed Value subclass: Result(T, E)
  field: okValue :: T = nil
  field: errReason :: E = nil

  sealed unwrap -> T =>
    self.isOk ifTrue: [
      self.okValue
    ] ifFalse: [(Erlang beamtalk_result) unwrapError: self.errReason]

  sealed map: block :: Block(T, R) -> Result(R, E) =>
    self.isOk ifTrue: [Result ok: (block value: self.okValue)] ifFalse: [self]

  sealed andThen: block :: Block(T, Result(R, E)) -> Result(R, E) =>
    self.isOk ifTrue: [block value: self.okValue] ifFalse: [self]
```

Type parameters are bare uppercase identifiers (by convention single letters: `T`, `E`, `K`, `V`, `R`). They appear in:
- Field type annotations: `field: okValue :: T`
- Method parameter types: `block :: Block(T, R)`
- Method return types: `-> T`, `-> Result(R, E)`
- Nested generic types: `Block(T, Result(R, E))`

### Using Generic Types

When using a generic class as a type annotation, concrete types replace the parameters:

```beamtalk
// Annotating a variable
result :: Result(String, IOError) := File read: "config.json"
result unwrap    // Type checker knows: -> String

// Annotating a method parameter
processResult: r :: Result(Integer, Error) -> Integer =>
  r unwrap + 1   // r unwrap is Integer, Integer has '+'

// Annotating state
Actor subclass: Cache(K, V)
  state: store :: Dictionary(K, V) = Dictionary new
```

### Type Inference Through Generics

The type checker performs **positional substitution**: when it encounters `Result(String, IOError)`, it maps `T -> String`, `E -> IOError`, and substitutes through all method signatures:

```beamtalk
r :: Result(Integer, Error) := computeSomething
r unwrap          // Return type T -> Integer
r map: [:v | v asString]   // Block param T -> Integer, return Result(String, Error)
r error           // Return type E -> Error
```

When concrete type parameters are unknown, they fall back to `Dynamic`:

```beamtalk
r := someMethod        // someMethod returns bare Result (no type params)
r unwrap               // -> Dynamic (T is unknown)
r unwrap + 1           // No warning — Dynamic bypasses checking
```

### Constructor Type Inference

For named constructors (`ok:`, `error:`, `new`), the compiler infers type parameters from the argument types:

```beamtalk
r := Result ok: 42                  // Inferred: Result(Integer, Dynamic)
r unwrap                            // -> Integer

r2 := Result error: #file_not_found // Inferred: Result(Dynamic, Symbol)
r2 error                            // -> Symbol
```

### Generic Inheritance

When a generic class extends another, the type parameter mapping must be explicit:

```beamtalk
// Array passes its E to Collection's E
Collection(E) subclass: Array(E)

// IntArray fixes E to Integer
Collection(Integer) subclass: IntArray

// SortedArray passes E through
Array(E) subclass: SortedArray(E)
```

### Block Type Parameters

`Block(...)` is special-cased — the last type parameter is always the return type:

- `Block(R)` — zero-argument block returning `R`
- `Block(A, R)` — one-argument block with arg type `A`, returning `R`
- `Block(A, B, R)` — two-argument block, returning `R`

### Design Constraints

- **Type erasure**: All type information is compile-time only. Zero runtime cost.
- **Warnings, not errors**: Type mismatches produce warnings, never block compilation.
- **Invariant type parameters**: No covariance/contravariance in Stage 1 (added with protocols in Stage 2).
- **Parenthesis syntax**: `Result(T, E)` not `Result<T, E>` — keeps `<` as a pure binary message.

### Dialyzer Spec Generation

Generic annotations generate expanded Dialyzer specs with concrete types at the BEAM interop boundary:

```beamtalk
processResult: r :: Result(Integer, Error) -> Integer => r unwrap + 1
```

Generates:
```erlang
-spec processResult(#{
  '__class__' := 'Elixir.Result',
  'okValue' := integer(),
  'errReason' := any()
}) -> integer().
```

Unresolved type parameters map to `any()` in Dialyzer specs.

### REPL Type Display

The REPL displays generic type information when available:

```beamtalk
> :help Result >> unwrap
unwrap -> T
```

When the workspace knows the concrete type parameters, `:help` substitutes them:

```beamtalk
> r := Result ok: 42
> r unwrap
=> 42
// Type info: Integer (inferred from Result(Integer, Dynamic))
```

---

## Structural Protocols (ADR 0068)

Protocols define named message sets. A class conforms to a protocol if it responds to all required messages — no `implements:` declaration needed. This is Smalltalk's duck-typing philosophy made explicit.

### Defining a Protocol

```beamtalk
Protocol define: Printable
  /// Return a human-readable string representation.
  asString -> String
  /// Return a developer-oriented representation (for debugging/REPL).
  printString -> String

Protocol define: Comparable
  < other :: Self -> Boolean
  > other :: Self -> Boolean
  <= other :: Self -> Boolean
  >= other :: Self -> Boolean

Protocol define: Collection(E)
  /// The number of elements in this collection.
  size -> Integer

  /// Iterate over each element.
  do: block :: Block(E, Object)

  /// Transform each element, returning a new collection of the same kind.
  collect: block :: Block(E, Object) -> Self

  /// Return elements matching the predicate.
  select: block :: Block(E, Boolean) -> Self
```

Protocol bodies use **class-body style** — method signatures without `=>` implementations. Doc comments are supported on each required method.

### Using Protocols as Types

Protocol names are used in type annotations the same way as class names — the compiler resolves the name and determines whether to perform nominal (class) or structural (protocol) checking:

```beamtalk
// Structural/protocol type — Printable guarantees asString
display: thing :: Printable =>
  Transcript show: thing asString

// Generic protocol type
printAll: items :: Collection(Object) =>
  items do: [:each | Transcript show: each asString]
```

### Automatic Conformance

Conformance is structural and automatic — no `implements:` declaration needed:

```beamtalk
// String has asString -> conforms to Printable
// Integer has asString -> conforms to Printable
display: "hello"           // String conforms to Printable
display: 42                // Integer conforms to Printable
display: Counter spawn     // Counter conforms to Printable (inherited from Object)
```

Classes that override `doesNotUnderstand:` conform to every protocol (they can respond to any message).

### Protocol Composition

```beamtalk
// Require multiple protocols
sort: items :: Collection(Object) & Comparable => ...

// Protocol extending another
Protocol define: Sortable
  extending: Comparable
  /// The key used for sort ordering.
  sortKey -> Object
```

### Class Method Requirements (BT-1611)

Protocols can require class-side methods using the `class` prefix, the same syntax as class definitions:

```beamtalk
Protocol define: Serializable
  asString -> String
  class fromString: aString :: String -> Self
```

A class conforms to `Serializable` only if it has both the instance method `asString` and the class method `fromString:`. This is useful for factory methods, singleton patterns, and other class-level contracts.

### Type Parameter Bounds

Type parameters can be bounded by protocols:

```beamtalk
// T must conform to Printable
Actor subclass: Logger(T :: Printable)
  log: item :: T =>
    Transcript show: item asString    // Guaranteed by Printable bound
```

### Runtime Protocol Queries

```beamtalk
> Integer conformsTo: Printable
=> true

> Integer protocols
=> #(Printable, Comparable)

> Printable requiredMethods
=> #(#asString, #printString)

> Printable conformingClasses
=> #(Integer, Float, String, Boolean, Symbol, Array, ...)
```

### Diagnostic Philosophy

Protocol conformance issues are **warnings, never errors**:

| Situation | Severity |
|---|---|
| Protocol conformance unverifiable | Warning |
| Missing method for protocol | Warning |
| Namespace collision (class + protocol same name) | Error (structural) |

### Printable Protocol and Display Methods

The `Printable` protocol is the standard contract for objects that can represent themselves as strings. It requires two methods:

- **`asString`** — a human-readable representation (for end-user display)
- **`printString`** — a developer-oriented representation (for debugging, logging, and REPL display)

Most stdlib classes conform automatically because `Object` provides a default `printString` (`"a ClassName"`) and most subclasses implement `asString`. Custom classes only need to implement these two methods to conform:

```beamtalk
Value subclass: Point
  field: x = 0
  field: y = 0

  // Human-readable
  asString -> String => "({self.x}, {self.y})"

  // Developer-readable (REPL display)
  printString -> String => "Point({self.x}, {self.y})"
```

The related display methods on `Object` are:

| Method | Behaviour |
|--------|-----------|
| `asString` | Human-readable string (override per class) |
| `printString` | Developer-readable string (REPL/inspector uses this) |
| `displayString` | User-facing display; defaults to `printString` |
| `inspect` | Inspection; defaults to `printString` |
| `show: value` | Write `value` to Transcript (nil-safe, returns `self`) |
| `showCr: value` | Write `value` to Transcript followed by newline (nil-safe, returns `self`) |

`show:` and `showCr:` are convenience methods on `Object` that delegate to `TranscriptStream`. They are nil-safe — when no transcript is active (e.g. batch compilation), they silently do nothing and return `self`, making them safe for cascaded chains:

```beamtalk
// Cascaded output
Transcript show: "Hello"; cr; show: "World"

// show:/showCr: on any object — nil-safe
42 show: "value: "
42 showCr: "hello world"
```

`TranscriptStream >> show:` accepts any `Printable` value, so custom classes that conform to `Printable` work directly with `Transcript show:` without manual `asString` conversion.

---

## Union Types and Narrowing (ADR 0068)

### Union Types

Union types express that a value may be one of several types:

```beamtalk
// All members must respond to the message
x :: Integer | String := getValue
x asString             // Both Integer and String have asString
x size                 // Warning: Integer does not respond to 'size'
x + 1                  // Warning: String does not respond to '+'
```

The nullable pattern (`String | nil`) is the most common union — Beamtalk's Option/Maybe type:

```beamtalk
name :: String | nil := dictionary at: "name"
name size              // Warning: UndefinedObject does not respond to 'size'
```

Similarly, `false` in type position resolves to `False` — used for Erlang FFI patterns:

```beamtalk
entry :: Tuple | false := ErlangLists keyfind: key
```

### Control Flow Narrowing

When the type checker recognises a type-testing pattern followed by `ifTrue:` / `ifFalse:`, it narrows the variable's type inside the block scope:

```beamtalk
// class identity check — narrows to exact class
process: x :: Object =>
  x class = Integer ifTrue: [
    x + 1          // x is Integer here — has '+'
  ]
  x + 1            // x is Object here — no narrowing outside the block

// kind check — narrows to class including subclasses
process: x :: Object =>
  x isKindOf: Number ifTrue: [
    x abs           // x is Number here
  ]

// early return narrows the rest of the method
validate: x :: Object =>
  x isNil ifTrue: [^nil]
  x doSomething    // x is non-nil for the remainder
```

**Supported narrowing patterns:**

| Pattern | Narrows to | Scope |
|---|---|---|
| `x class = Foo ifTrue: [...]` | `x` is `Foo` in true block | True block only |
| `x isKindOf: Foo ifTrue: [...]` | `x` is `Foo` in true block | True block only |
| `x isNil ifTrue: [^...]` | `x` is non-nil after the statement | Rest of method |
| `x isNil ifTrue: [^...] ifFalse: [...]` | `x` is non-nil in false block | False block |

### Union + Narrowing Compose

```beamtalk
name :: String | nil := dictionary at: "name"
name isNil ifTrue: [^"unknown"]
name size              // name is narrowed to String — nil eliminated by early return
```

---

## Control Flow and Mutations

Beamtalk supports Smalltalk-style control flow via messages to booleans and blocks, with full mutation support via a universal state-threading protocol ([ADR 0041](ADR/0041-universal-state-threading-block-protocol.md)).

### How It Works

The compiler uses a two-tier optimization for block mutations:

- **Tier 1 (stdlib control flow):** `whileTrue:`, `do:`, `collect:`, `timesRepeat:`, etc. — inlined tail-recursive loops with versioned state variables. Zero overhead.
- **Tier 2 (user-defined methods):** All other methods accepting blocks — universal `{Result, StateAcc}` protocol. Pure blocks have no overhead; stateful blocks pay ~65ns per invocation.

**Local variable mutations** work in all blocks — including stored closures and blocks passed to user-defined higher-order methods. **Field mutations** (`self.x :=`) require control-flow context and are a compile error in stored closures.

### Control Flow Constructs

These message sends are Tier 1 optimized — the compiler generates inlined tail-recursive loops with zero overhead:

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
    self.value

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

### What Works and What Doesn't

**Local variable mutations** work in all blocks — including stored closures and user-defined higher-order methods (ADR 0041 Tier 2):

```beamtalk
// ✅ Local mutation in stored closure — works via Tier 2 protocol
count := 0
myBlock := [count := count + 1]
10 timesRepeat: myBlock
count  // => 10

// ✅ Local mutation in user-defined HOM — works via Tier 2 protocol
count := 0
items myCustomLoop: [:x | count := count + x]
count  // => sum of items
```

**Field mutations** (`self.x :=`) require control-flow context and are a compile error in stored closures:

```beamtalk
// ❌ ERROR: Field assignment in stored closure
badBlock =>
    myBlock := [self.value := self.value + 1]
    // ERROR: Cannot assign to field 'value' inside a stored closure.

// ✅ CORRECT: Field mutation in control flow
increment =>
    10 timesRepeat: [self.value := self.value + 1]  // ✅ Works!
```

### Why This Design?

| Property | ✅ Benefit |
|----------|-----------|
| **Universal** | Local variable mutations work in all blocks — no whitelist |
| **Smalltalk-like** | Natural iteration patterns work, including user-defined HOMs |
| **Safe** | Field mutations in stored closures are caught at compile time |
| **Good DX** | Clear errors with fix suggestions |
| **BEAM-idiomatic** | Compiles to tail recursion + state threading |
| **Performant** | Stdlib hot paths are zero overhead (Tier 1); user HOMs ~65ns (Tier 2) |

### Error Messages

When you accidentally assign to a field inside a stored closure, the compiler provides guidance:

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

### Custom Timeouts

The default `.` send timeout is 5000ms. For actors that may take longer (database queries, HTTP calls), use `withTimeout:` to create a timeout proxy:

```beamtalk
// Wrap an actor with a custom timeout (milliseconds)
slowDb := db withTimeout: 30000
slowDb query: sql              // forwarded with 30s timeout
slowDb stop                    // stop the proxy when done

// Infinite timeout (use with care — blocks indefinitely)
infDb := db withTimeout: #infinity
infDb query: sql
infDb stop
```

`withTimeout:` returns a `TimeoutProxy` — a lightweight actor that forwards ordinary messages to the target via `doesNotUnderstand:args:` using the specified timeout. Lifecycle messages such as `stop` apply to the proxy itself, not the target. This is pure message passing with no special syntax or reserved keywords.

**Lifecycle:** The proxy is a separate actor process. Capture the reference and call `stop` when finished to avoid leaking processes.

### BEAM Mapping

| Beamtalk | BEAM |
|----------|------|
| `.` send (sync) | `gen_server:call` — blocks until reply |
| `!` send (async cast) | `gen_server:cast` — returns immediately |
| Timeout | `gen_server:call` default 5000ms timeout |
| `withTimeout:` | Proxy wrapping `gen_server:call/3` with custom timeout |
| `performLocally:withArguments:` | Direct in-process call bypassing gen_server |

### Caller-Process Class Method Dispatch

Class methods normally execute inside the class object's gen_server process. For long-running class methods (batch processing, report generation) that would block all other messages to the class, use `performLocally:withArguments:` to execute in the caller's process instead:

```beamtalk
// Normal dispatch — runs in MyClass gen_server process
MyClass computeReport

// Local dispatch — runs in caller's process, doesn't block the class
MyClass performLocally: #computeReport withArguments: #()

// With arguments
MyClass performLocally: #add:to: withArguments: #(3, 7)
```

**Limitations:** Local dispatch calls the method directly on the target class module — it does not walk the superclass chain. Class variable mutations are discarded (the call runs outside the class gen_server's state). Use this only for stateless or read-only class methods.

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

## Server — OTP Interop (ADR 0065)

`Server` is an **abstract** subclass of `Actor` for BEAM-level OTP interop. The class hierarchy expresses the abstraction boundary:

```text
Object
  └── Actor           # Beamtalk objects — messages, state, Timer
        └── Server    # BEAM processes — handleInfo:, raw OTP interop (abstract)
```

- **`Actor`** — Beamtalk-level: message-passing, state, Timer API, lifecycle (`initialize`, `terminate:`). Most users, most of the time.
- **`Server`** — BEAM-level: raw message handling (`handleInfo:`), and the natural home for future OTP features (named registration, `trapExit`, `codeChange:from:`).

### Defining a Server

Use `Server subclass:` when you need to receive raw Erlang messages (timer events, monitor DOWN tuples, system messages). All existing Actor methods continue to work — Server inherits everything from Actor.

```beamtalk
Server subclass: PeriodicWorker
  state: count = 0

  initialize =>
    Erlang erlang send_after: 1000 dest: (self pid) msg: #tick

  handleInfo: msg =>
    msg match: [
      #tick -> [
        self.count := self.count + 1.
        Erlang erlang send_after: 1000 dest: (self pid) msg: #tick
      ];
      _ -> nil
    ]

  getValue => self.count
```

### handleInfo: Semantics

- Defined on `Server` with a **default no-op** implementation — messages are ignored unless you override it.
- **Error handling:** errors in `handleInfo:` are logged and the server continues (log-and-continue). A bad message does not crash the server.
- Sending `handleInfo:` to a plain `Actor` raises `doesNotUnderstand` — only Server subclasses have this method.

### Migration: Actor to Server

Promoting an Actor to a Server is a one-word change. All existing methods continue to work:

```beamtalk
// Before
Actor subclass: MyThing
  // ...

// After — all existing methods still work, handleInfo: now available
Server subclass: MyThing
  handleInfo: msg => ...
```

### Timer Lifecycle

Timer processes (`Timer every:do:` and `Timer after:do:`) are **linked to the calling process** via `spawn_link`. This means:

- When the actor dies, linked Timer processes die automatically — no orphaned ticks
- `cancel` still works for explicit lifecycle control
- User code errors in Timer blocks are wrapped in `catch` — they do not crash the Timer process

```beamtalk
Actor subclass: Ticker
  state: count = 0

  initialize =>
    Timer every: 1000 do: [self tick!]   // async cast — MUST use ! not .

  tick => self.count := self.count + 1
  getValue => self.count
```

No `state:` for the timer reference, no `terminate:` cleanup needed — the link handles it.

### BEAM Mapping

| Beamtalk | BEAM |
|----------|------|
| `Server subclass:` | gen_server with `handle_info/2` dispatch |
| `handleInfo: msg` | `handle_info(Msg, State)` callback |
| `Actor subclass:` | gen_server with `handle_info/2` ignore stub |
| Timer `spawn_link` | Timer process linked to calling process |

---

## Supervision Trees (ADR 0059)

Beamtalk provides declarative OTP supervision trees via `Supervisor subclass:` and `DynamicSupervisor subclass:`. This is the Beamtalk idiom for "let it crash" fault tolerance — define which actors should be restarted automatically, and how.

### Static Supervisor

Subclass `Supervisor` and override `class children` to return a list of actor classes (or `SupervisionSpec` values for per-child configuration). The supervisor starts all children at startup using OTP `one_for_one` strategy by default.

> **Important:** `class children`, `class strategy`, `class maxRestarts`, and `class restartWindow` are called during supervisor startup from the OTP `init/1` callback — before the class gen_server is available. These methods must be **pure** (return literal values only). Do not send messages to `self`, call other class methods via dispatch, or read class variables from within these methods.

```beamtalk
Supervisor subclass: WebApp
  class children => #(DatabasePool HTTPRouter MetricsCollector)
```

Start the supervisor with `supervise`. It registers under its class name so it can be found from anywhere:

```beamtalk
app := WebApp supervise
// => #Supervisor<WebApp,_>

// Idempotent — returns the already-running instance if called again
app2 := WebApp supervise
// => #Supervisor<WebApp,_>

// Find the running instance by class name (no reference needed)
WebApp current
// => #Supervisor<WebApp,_>
```

Inspect and manage children:

```beamtalk
app count                  // => 3  (number of running children)
app children               // => ["DatabasePool","HTTPRouter","MetricsCollector"]  (child ids)
app which: DatabasePool    // => #Actor<DatabasePool,_>  (running child instance)
app terminate: HTTPRouter  // gracefully stop a single child
app stop                   // stop the supervisor and all children

// After stop:
WebApp current             // => nil
```

### Class-Side Configuration Defaults

Override these class methods in your subclass to customise restart behaviour:

| Method | Default | Description |
|--------|---------|-------------|
| `class strategy` | `#oneForOne` | OTP restart strategy (`#oneForOne`, `#oneForAll`, `#restForOne`) |
| `class maxRestarts` | `10` | Max restarts before supervisor gives up |
| `class restartWindow` | `60` | Time window (seconds) for `maxRestarts` |

```beamtalk
Supervisor subclass: CriticalApp
  class children => #(Database Cache)
  class strategy => #oneForAll       // restart all if any child crashes
  class maxRestarts => 3             // give up after 3 crashes in 60 seconds
```

### Actor Supervision Policy

Each actor class declares its OTP restart policy via `class supervisionPolicy`:

```beamtalk
Actor subclass: DatabasePool
  class supervisionPolicy => #permanent   // always restart on crash

Actor subclass: RequestHandler
  class supervisionPolicy => #transient   // restart only on abnormal exit

Actor subclass: BackgroundJob
  class supervisionPolicy => #temporary   // never restart (default)
```

### SupervisionSpec — Per-Child Overrides

Use `SupervisionSpec` when you need to override a child's restart policy, provide startup arguments, or set a custom shutdown timeout:

```beamtalk
Supervisor subclass: WebApp
  class children =>
    #(DatabasePool
      HTTPRouter supervisionSpec withRestart: #transient
      (MetricsCollector supervisionSpec withId: #metrics withArgs: #{#port => 9090}))
```

Use `withShutdown:` to set a graceful shutdown timeout (in milliseconds) for children that need time to drain connections or flush state. The default is 5000ms for workers and `infinity` for nested supervisors.

```beamtalk
HttpServer supervisionSpec withShutdown: 30000   // 30s graceful shutdown
```

### Dynamic Supervisor

Subclass `DynamicSupervisor` to manage pools of actors started at runtime. Override `class childClass` to declare which actor class the pool manages.

```beamtalk
DynamicSupervisor(Worker) subclass: WorkerPool
  class childClass => Worker
```

```beamtalk
pool := WorkerPool supervise
// => #DynamicSupervisor<WorkerPool,_>

// Start children dynamically
w1 := pool startChild          // => #Actor<Worker,_>
w2 := pool startChild          // => #Actor<Worker,_>
pool count                     // => 2

// Terminate a specific child
pool terminateChild: w1        // => nil
pool count                     // => 1

// Stop the whole pool
pool stop
WorkerPool current             // => nil
```

### Nested Supervisors

Supervisors can be nested — include another supervisor class in `children`:

```beamtalk
Supervisor subclass: AppRoot
  class children => #(DatabaseSupervisor WebTierSupervisor MetricsSupervisor)
```

Nested supervisor children are identified by `isSupervisor => true` and started via OTP `start_link/0`, ensuring they are correctly linked into the supervision tree. The outer supervisor shuts down inner supervisors (and all their children) gracefully on `stop`.

```beamtalk
root := AppRoot supervise
root count                          // => 3
root which: DatabaseSupervisor      // => #Supervisor<DatabaseSupervisor,_>
```

### BEAM Mapping

| Beamtalk | BEAM |
|----------|------|
| `Supervisor subclass:` | `-behaviour(supervisor)` with `one_for_one` |
| `DynamicSupervisor(C) subclass:` | `-behaviour(supervisor)` with `simple_one_for_one` |
| `supervise` | `supervisor:start_link({local, Module}, Module, [])` |
| `current` | `whereis(Module)` |
| `count` | `supervisor:count_children/1` (active count) |
| `children` | `supervisor:which_children/1` (running child ids) |
| `which: Class` | find child by module in `which_children` result |
| `withShutdown:` | `shutdown` field in child spec (default 5000ms workers, infinity supervisors) |
| `stop` | `gen_server:stop/1` |

---

## Named Actor Registration (ADR 0079)

Named actor registration gives a process a **stable identity** (a `Symbol`) that survives supervised restarts. A name-resolving proxy re-resolves the name on every message send, so a held reference keeps working even when the underlying actor is restarted with a fresh pid.

Under the hood this maps directly to OTP's process registry (`erlang:register/2`, `gen_server:start_link({local, Name}, ...)`), so registered Beamtalk actors show up in `observer`, `recon`, and `erlang:registered/0` with the names you chose.

### API Surface

| Method | Kind | Returns | Semantics |
|--------|------|---------|-----------|
| `Class spawnAs: name` | class-side | `Result(Self, Error)` | Atomic spawn + register. Equivalent to `gen_server:start_link({local, Name}, ...)` — the name is registered during process startup. |
| `Class spawnWith: initArgs as: name` | class-side | `Result(Self, Error)` | Same as `spawnAs:` but with initialization arguments. |
| `Class named: name` | class-side | `Result(Self, Error)` | Look up a registered actor. `Self` resolves to the receiver class at the call site, so `Counter named:` returns a `Counter`. |
| `Actor allRegistered` | class-side | `List(Actor)` | Enumerates currently-registered Beamtalk actors. Excludes raw OTP-registered processes (`kernel_sup`, `logger`, …). |
| `actor registerAs: name` | instance | `Result(Self, Error)` | Register an already-spawned actor. Non-atomic — prefer `spawnAs:` when the name is known up front. |
| `actor unregister` | instance | `Symbol` | `#ok`. Idempotent — unregistering an unregistered actor is not an error. |
| `actor registeredName` | instance | `Symbol` or `nil` | Currently-registered name, or `nil`. |
| `actor isRegistered` | instance | `Boolean` | Whether the actor currently has a registered name. |

Supervised children gain naming through `SupervisionSpec withName:`, which tells the runtime to start the child with `{local, Name}` registration so the name is re-established every time the supervisor restarts the child:

```beamtalk
EventStore supervisionSpec withName: #eventStore withRestart: #permanent
```

### Errors

Registration returns `Result(Self, Error)` rather than raising — callers branch explicitly on outcome:

| Condition | Result |
|-----------|--------|
| `spawnAs:` / `registerAs:` — duplicate registration | `Result error: (beamtalk_error name_registered)` |
| `spawnAs:` / `registerAs:` — name is in the reserved list | `Result error: (beamtalk_error reserved_name)` |
| `spawnAs:` / `registerAs:` — non-Symbol name | `Result error: (beamtalk_error type_error)` |
| `Class named:` — no actor registered under this name | `Result error: (beamtalk_error name_not_registered)` |
| `Class named:` — name is registered but the actor is not a `Class` or subclass | `Result error: (beamtalk_error wrong_class)` |
| Send to a proxy whose name is *not currently registered* (the target died or was unregistered after lookup) | Raises `beamtalk_error no_such_process` |

The asymmetry is deliberate: `named:` returns a Result because name-absence is an expected outcome the caller must branch on; sending to a vanished proxy raises because the caller has already committed to a send.

### Worked Example — Migrating from `Supervisor which:`

#### Before named registration

The pre-ADR pattern uses a supervisor-local lookup (`which:`) and an `initialize:` hook to re-wire dependencies after each restart:

```beamtalk
typed Supervisor subclass: ExduraSupervisor
  class strategy -> Symbol => #restForOne
  class children -> List(SupervisionSpec) =>
    storeSpec := EventStore supervisionSpec withRestart: #permanent
    poolSpec := ActivityWorkerPool supervisionSpec withRestart: #permanent
    engineSpec := WorkflowEngine supervisionSpec withRestart: #permanent
    #(storeSpec, poolSpec, engineSpec)

  // Re-runs after every restart to rebuild cached pids.
  class initialize: sup :: Supervisor -> Nil =>
    store := sup which: EventStore
    pool := sup which: ActivityWorkerPool
    engine := sup which: WorkflowEngine
    engine initWithStore: store pool: pool
    nil
```

#### After named registration

Naming each child eliminates the `initialize:` hook, and the supervisor strategy is freed from the rewire-on-restart constraint:

```beamtalk
typed Supervisor subclass: ExduraSupervisor
  class strategy -> Symbol => #oneForOne
  class children -> List(SupervisionSpec) => #(
    EventStore supervisionSpec withName: #eventStore withRestart: #permanent,
    ActivityWorkerPool supervisionSpec withName: #workerPool withRestart: #permanent,
    WorkflowEngine supervisionSpec withName: #workflowEngine withRestart: #permanent
  )
  // No initialize: hook — WorkflowEngine looks up its dependencies by name.
```

`WorkflowEngine` now calls `(Actor named: #eventStore) unwrap` at use-time — automatically picking up the current pid across restarts — and cross-tree consumers (HTTP handlers, REPL workspaces, tests) can reach supervised actors directly without routing through the supervisor.

### Proxy Semantics

`Class named:` returns a lightweight **name-resolving proxy**. The proxy does not cache a pid; each message send re-resolves the name via the Erlang runtime. This is the key restart-survival property:

```beamtalk
engine := (WorkflowEngine named: #workflowEngine) unwrap
engine runWorkflow: w1    // resolves #workflowEngine, sends to that pid
// (#workflowEngine crashes; the supervisor restarts it under the same name)
engine runWorkflow: w2    // re-resolves #workflowEngine, sends to the NEW pid
```

A few caveats the proxy intentionally does *not* paper over:

- **`monitor:` and `onExit:` are pid-level** — they watch the *current* pid, not the name. A monitor does not re-arm when the supervisor restarts the process under the same name. A future "watch a name" API can be added separately; for now, prefer `isAlive` polling via the proxy when you need restart-aware liveness.
- **Equality is identity-shape-based.** Two proxies with the same name are equal. A proxy and a direct-pid reference are **not equal**, even if they currently resolve to the same pid — the whole point of a name proxy is to be a *different kind of reference*.
- **`unregister` makes the proxy dead.** After unregistering, further sends through a proxy raise `#no_such_process` even if the underlying actor is still running. Re-register under a new name if you want to keep routing messages through a name-resolving proxy.

### Reserved Names

A static blocklist of OTP-kernel atoms is rejected at registration time, regardless of whether the corresponding process is currently running. Attempting `spawnAs: #logger` or `spawnAs: #kernel_sup` returns `Result error: (beamtalk_error reserved_name)`.

The reserved set covers:

- OTP kernel and stdlib processes: `application_controller`, `code_server`, `erl_prim_loader`, `erl_signal_server`, `error_logger`, `erts_code_purger`, `file_server_2`, `global_group`, `global_name_server`, `inet_db`, `init`, `kernel_refc`, `kernel_safe_sup`, `kernel_sup`, `logger`, `logger_handler_watcher`, `logger_proxy`, `logger_std_h_default`, `logger_sup`, `net_kernel`, `net_sup`, `rex`, `socket_registry`, `standard_error`, `standard_error_sup`, `standard_error_writer`, `user`, `user_drv`, `user_drv_reader`, `user_drv_writer`.
- Any atom prefixed with `beamtalk_` (reserves the namespace for runtime infrastructure).

See `beamtalk_actor:reserved_name/1` in the runtime for the authoritative list and the policy rationale (ADR 0079 §Errors).

### Scope

This release covers **local (per-node)** registration. Cluster-wide (`#global`) and pluggable (`{via, Module, Term}`) scopes are deferred to a future ADR — the API is designed to admit them additively via a `scope:` keyword. Users who need cluster registration today can call the Erlang `global` module via FFI.

### BEAM Mapping

| Beamtalk | BEAM |
|----------|------|
| `Class spawnAs: #foo` | `gen_server:start_link({local, foo}, Module, #{})` |
| `Class spawnWith: args as: #foo` | `gen_server:start_link({local, foo}, Module, args)` |
| `actor registerAs: #foo` | `erlang:register(foo, Pid)` |
| `actor unregister` | Beamtalk-wrapped idempotent `erlang:unregister(foo)` — Beamtalk catches the `badarg` raw Erlang raises when the name is absent and returns `ok`, so repeated/unnecessary unregisters are safe |
| `Class named: #foo` | `erlang:whereis(foo)` + Beamtalk class check via `'$beamtalk_actor'` process-dict marker |
| `Actor allRegistered` | `erlang:registered/0` filtered by the process-dict marker |
| Proxy send (`proxy foo`) | `gen_server:call(foo, ...)` — name-resolved per send |
| `SupervisionSpec withName:` | Child MFA uses `{beamtalk_actor, spawnAs, [Name, Module, ...]}` so the supervisor re-registers the name on every restart |

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

// Array destructuring in match arms (BT-1296)
#[10, 20] match: [
  #[h, t] -> h + t;
  _ -> 0
]
// => 30

// Dict/map destructuring in match arms (BT-1296)
#{#event => "click", #x => 5} match: [
  #{#event => evName} -> evName;
  _ -> "unknown"
]
// => "click"

// Nested array patterns
#[#[1, 2], 3] match: [
  #[#[a, b], c] -> a + b + c;
  _ -> 0
]
// => 6

// Constructor patterns (Result ok:/error: only in this release)
(Result ok: 42) match: [
  Result ok: v    -> v;
  Result error: _ -> 0
]
// => 42
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
| Tuple | `{a, b}` | Destructure tuple in assignment and match arms |
| Array | `#[a, b]` | Match and destructure an Array by exact size; nested arrays supported |
| Array rest | `#[a, ...rest]` | Destructure first elements, bind remaining to a sub-array (destructuring assignment only) |
| Dict/Map | `#{#k => v}` | Match a Dictionary containing key `#k`, bind value to `v`; partial match (other keys ignored) |
| Constructor | `Result ok: v` | Match sealed type by constructor (Phase 1: Result only) |

**Exhaustiveness checking (BT-1299):** `match:` on a sealed type with constructor patterns must cover all known variants or include a wildcard `_` arm, or the compiler emits an error:

```beamtalk
// Compile error: missing error: arm
r match: [Result ok: v -> v + 1]

// Fine: all variants covered
r match: [Result ok: v -> v + 1; Result error: _ -> 0]

// Fine: wildcard suppresses the check
r match: [Result ok: v -> v + 1; _ -> 0]
```

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

// Tuple destructuring in match arms
t := Erlang erlang list_to_tuple: #(#ok, 42)
t match: [{#ok, v} -> v; {#error, _} -> 0]
// => 42
```

### Rest Patterns in Destructuring (BT-1251)

The `...identifier` syntax in array destructuring captures remaining elements:

```beamtalk
#[first, ...rest] := #[1, 2, 3, 4, 5]
// first = 1, rest = #[2, 3, 4, 5]

#[a, b, ...tail] := #[10, 20, 30, 40]
// a = 10, b = 20, tail = #[30, 40]

#[...all] := #[1, 2, 3]
// all = #[1, 2, 3]

#[head, ..._] := #[1, 2, 3]
// head = 1 (rest discarded)
```

The rest element must be the last in the pattern. Rest patterns are supported in destructuring assignment only — they are not yet supported in `match:` arms.

> **Note:** Tuple destructuring works in both assignment (`{x, y} := expr`) and `match:` arms. `collect:` with pattern blocks is not yet supported.

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

## Extension Methods (Open Classes)

The `>>` syntax adds methods to existing classes without redefining them (ADR 0066).
Extensions work on any class including built-in value types.

```beamtalk
// Instance method
String >> shout => self uppercase ++ "!"

// Class-side method
String class >> fromJson: s => // ...parse JSON string

// Keyword method with typed parameter
Array >> chunksOf: n :: Integer => // ...split into n-sized chunks

// Binary method
Point >> + other :: Point => Point x: self x + other x y: self y + other y
```

### Type annotations on extensions

Extensions support the same `-> ReturnType` annotation as regular methods.
Additionally, extensions accept `:: -> ReturnType` as a visual separator between
the selector and return type — especially useful on unary methods where there
are no parameters to carry `::` annotations.

```beamtalk
// Standard return type syntax (same as inside a class)
String >> reversed -> String => self reverse

// Extension-style: `:: ->` separates selector from return type
Integer >> factorial :: -> Integer =>
  self <= 1
    ifTrue: [1]
    ifFalse: [self * (self - 1) factorial]

String >> words :: -> Array => self split: " "

// Typed parameters with :: -> return type
Map >> at: key :: String put: value :: Integer :: -> Map => // ...
```

Both forms are equivalent — the return type flows to the type checker identically.
The `:: ->` form is preferred for unary extensions; the `->` form is preferred
when parameters already have `::` annotations (to avoid consecutive `::` tokens).

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
| `allClasses` | `List` | All registered classes (class objects) |
| `classNamed: #Name` | `Object` or `nil` | Look up a class by name |
| `globals` | `Dictionary` | Snapshot of system namespace (class names → class objects) |
| `help: aClass` | `String` | Class documentation: name, superclass, method signatures |
| `help: aClass selector: #sel` | `String` | Documentation for a specific method |

```beamtalk
Beamtalk version
// => "0.3.1"

Beamtalk allClasses includes: Integer
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
| `:sync` | `Workspace sync` |
| `:load path` | `Workspace load: "path"` |
| `:reload Counter` | `Counter reload` |
| `:test` | `Workspace test` |
| `:test CounterTest` | `Workspace test: CounterTest` |
| `:help Counter` | `Beamtalk help: Counter` |
| `:help Counter increment` | `Beamtalk help: Counter selector: #increment` |

The native forms work from compiled code, scripts, and actor methods — not just
the REPL.

---

## Actor Observability and Tracing (ADR 0069)

The `Tracing` class provides actor observability and performance telemetry. It is a sealed, class-only facade (like `System` and `Logger`) — all methods are class-side, there are no instances. See [ADR 0069](ADR/0069-actor-observability-and-tracing.md) for the full design.

Two levels of instrumentation are available:

- **Always-on aggregates** — per-actor, per-method call counts and durations with negligible overhead (~150ns/call). No setup required.
- **Detailed trace events** — individual call/return/error records captured to a ring buffer. Requires explicit `Tracing enable`.

### Tracing Lifecycle

```beamtalk
// Enable detailed trace capture
Tracing enable
// => nil

// Check if tracing is active
Tracing isEnabled
// => true

// Disable trace capture (aggregates continue)
Tracing disable
// => nil

// Clear all trace events and aggregate stats
Tracing clear
// => nil
```

### Aggregate Stats (Always-On)

Aggregate stats are collected for every actor dispatch, even when trace capture is disabled. They include call counts, total duration, min/max/average times, and error counts.

```beamtalk
// All per-actor, per-method stats
Tracing stats
// => #{...}  (Dictionary keyed by actor/selector)

// Stats for a specific actor
Tracing statsFor: myCounter
// => #{...}
```

### Trace Event Queries

When trace capture is enabled, individual call events are recorded to a ring buffer. These are available for querying even after the actor has stopped.

```beamtalk
// All captured events (newest first)
Tracing traces
// => #(...)

// Events for a specific actor
Tracing tracesFor: myCounter
// => #(...)

// Events for a specific actor + method
Tracing tracesFor: myCounter selector: #increment
// => #(...)
```

### Analysis Methods

Analysis methods compute rankings from aggregate stats. Each takes a limit parameter for the number of results.

```beamtalk
// Top N methods by average duration (slowest first)
Tracing slowMethods: 10
// => #(...)

// Top N methods by call count (most called first)
Tracing hotMethods: 10
// => #(...)

// Top N methods by error + timeout rate
Tracing errorMethods: 5
// => #(...)

// Top N actors by message queue length (live snapshot)
Tracing bottlenecks: 5
// => #(...)
```

### Live Health

Health methods provide point-in-time snapshots of actor and VM state.

```beamtalk
// Per-actor health: queue depth, memory, reductions, status
Tracing healthFor: myCounter
// => #{queue_len => 0, memory => 1234, status => #waiting, ...}

// VM overview: schedulers, memory, process count, run queues
Tracing systemHealth
// => #{scheduler_count => 8, process_count => 42, ...}
```

### Configuration

The trace event ring buffer has a configurable capacity (default 10,000 events). When full, the oldest events are evicted.

```beamtalk
// Query current buffer capacity
Tracing maxEvents
// => 10000

// Set buffer capacity
Tracing maxEvents: 50000
// => nil
```

### Typical Workflow

```beamtalk
// 1. Create and exercise an actor
c := Counter spawn
10 timesRepeat: [c increment]

// 2. Check always-on aggregates (no enable needed)
Tracing statsFor: c
// => #{increment => #{count => 10, avg_us => 42, ...}, ...}

// 3. Enable trace capture for detailed events
Tracing enable

// 4. Exercise the actor some more
5 timesRepeat: [c increment]

// 5. Query detailed traces
Tracing tracesFor: c selector: #increment
// => #(#{selector => #increment, duration_us => 38, ...}, ...)

// 6. Find bottlenecks
Tracing slowMethods: 5
// => #(...)

// 7. Clean up
Tracing disable
Tracing clear
```

### Propagated Context (Advanced)

Actor messages automatically carry a propagated context map across boundaries. This is invisible to Beamtalk code — no user action is required. The context enables distributed tracing when OpenTelemetry is added as a project dependency: parent/child span correlation across actor calls works immediately with no Beamtalk changes. See [ADR 0069](ADR/0069-actor-observability-and-tracing.md) for details.

### Relationship to Logging (ADR 0064)

`Tracing` and `Logger` address complementary observability concerns:

| Concern | API | ADR |
|---------|-----|-----|
| **What is happening** — log messages, debug output | `Logger info:`, `Beamtalk enableDebug:` | [ADR 0064](ADR/0064-runtime-logging-control-and-observability-api.md) |
| **How fast is it happening** — timing, call counts, bottlenecks | `Tracing stats`, `Tracing slowMethods:` | [ADR 0069](ADR/0069-actor-observability-and-tracing.md) |

---

## Namespace and Class Visibility

Beamtalk uses a **flat global namespace** (ADR 0031). All classes are globally
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
Value subclass: Integer
  field: x = 0
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
  divSafe: divisor =>
    divisor == 0 ifTrue: [^0]
    self / divisor
```


### Namespace

Class names must be globally unique. A package namespace and dependency system is designed ([ADR 0070](ADR/0070-package-namespaces-and-dependencies.md)) but not yet implemented. See [known-limitations.md](known-limitations.md) and
[ADR 0031](ADR/0031-flat-namespace-for-v01.md) for details.

### Visibility and Access Control (ADR 0071)

Beamtalk classes and methods are **public by default** — visible to any package. The `internal` modifier restricts visibility to the defining package only. Enforcement is **compile-time only**, with zero runtime overhead.

**Core principle:** Visibility controls *dependency*, not *knowledge*. Internal classes and methods are fully visible to browsing, reflection, and documentation tools — you just cannot name them in compiled code from outside the package. The REPL's `:browse`, `:doc`, and `:source` commands work on internal items normally.

#### Class-Level `internal`

Mark a class as `internal` to hide it from other packages:

```beamtalk
// Public (default) — available to any package
Actor subclass: HttpClient
  get: url => ...

// Internal — only visible within this package
internal Actor subclass: ConnectionPool
  state: connections = #{}

  acquire => ...
  release: conn => ...
```

Cross-package references to internal classes produce a compile error:

```text
error[E0401]: Class 'ConnectionPool' is internal to package 'http' and cannot be referenced from 'my_app'
  --> src/app.bt:5:12
   |
 5 |     http@ConnectionPool new
   |          ^^^^^^^^^^^^^^
   |
   = note: 'ConnectionPool' is declared 'internal' in package 'http'
```

#### Method-Level `internal`

Mark individual methods as `internal` to hide implementation helpers on public classes:

```beamtalk
Actor subclass: HttpClient
  state: config = #{}

  // Public — part of the package API
  get: url => ...
  post: url body: body => ...

  // Internal — implementation details, not callable from outside the package
  internal buildHeaders: request => ...
  internal retryWithBackoff: block maxAttempts: n => ...
```

When the compiler can determine the receiver type (via type annotations, literal class references, or type inference), cross-package sends to internal methods produce a compile error:

```text
error[E0403]: Method 'buildHeaders:' is internal to package 'http' and cannot be called from 'my_app'
  --> src/app.bt:10:5
   |
10 |     client buildHeaders: req
   |            ^^^^^^^^^^^^^^
```

For untyped dynamic sends where the receiver type is unknown, no enforcement — the message send succeeds at runtime, consistent with the "visibility controls dependency, not knowledge" principle.

#### Combining Modifiers

`internal` composes with all existing class modifiers in any order:

```beamtalk
// Internal abstract base — must be subclassed within the package
internal abstract Actor subclass: InternalAbstractBase
  state: label = "base"
  getLabel => self.label
  compute => self subclassResponsibility

// Internal sealed — cannot be subclassed, even within the package
internal sealed Actor subclass: InternalSealedCache
  state: data = 0
  store: val => self.data := val
  retrieve => self.data

// Internal typed — type annotations required on methods
internal typed Actor subclass: InternalTypedConfig
  state: setting :: Integer = 0
  getSetting -> Integer => self.setting
  setSetting: val :: Integer -> Integer => self.setting := val

// Modifier order is flexible
abstract internal Actor subclass: AlsoValid
  ...
```

| Combination | Valid? | Notes |
|-------------|--------|-------|
| `internal sealed` | Yes | Prevents subclassing even within the package |
| `internal abstract` | Yes | Internal base class, must be subclassed within the package |
| `internal typed` | Yes | Internal class with type annotation requirements |
| Stacking order | Any | `internal` can appear anywhere in the modifier list |

#### Library Author Patterns

A typical package exposes a few public classes and hides implementation details:

```beamtalk
// json/src/parser.bt — Public API
Object subclass: Parser
  /// Parse a JSON string into a Beamtalk value.
  parse: input :: String => ...

// json/src/parser_state.bt — Internal implementation
internal Value subclass: ParserState
  field: position = 0
  field: buffer = ""

// json/src/token_buffer.bt — Internal implementation
internal Value subclass: TokenBuffer
  field: tokens = #()
```

**Leaked visibility** — if an internal class appears in the public signature of a public method, the compiler emits a hard error. This prevents accidentally exposing implementation types:

```text
error[E0402]: Internal class 'TokenBuffer' appears in public signature of 'Parser >> tokenize:'
  --> src/parser.bt:12:3
   |
12 |   tokenize: input :: String -> TokenBuffer =>
   |                                ^^^^^^^^^^^
   |
   = note: 'TokenBuffer' is declared 'internal' — make it public, or change the return type
```

All methods on an internal class are effectively internal. The method-level modifier is only meaningful on public classes.

#### Metadata

Visibility is recorded in `__beamtalk_meta/0` as a compile-time constant atom (`public` or `internal`). Tooling (LSP, REPL completions) uses this field to filter internal items from cross-package suggestions while still showing them in `:browse` and `:doc` output.

See [ADR 0071](ADR/0071-class-visibility-internal-modifier.md) for the full design, including edge cases (subclassing, protocol conformance, extension methods, `perform:` dynamic sends) and the enforcement model.

---

## Smalltalk + BEAM Mapping

| Smalltalk/Newspeak Concept | Beamtalk/BEAM Mapping |
|----------------------------|----------------------|
| Value object | `Value subclass:` with `field:` — plain Erlang map (no process) |
| Actor | `Actor subclass:` with `state:` — BEAM process (gen_server) |
| Module/utility class | `Object subclass:` — no Beamtalk-managed data; class methods or runtime-backed instances |
| Class | Module + constructor function |
| Instance variable (immutable) | `field:` — value map field |
| Instance variable (mutable) | `state:` — gen_server state map field |
| Field access (`self.x`) | `maps:get('x', State)` |
| Field write (`self.x := v`) | `maps:put('x', v, State)` (Actor only; compile error on Value) |
| `.` message send | `gen_server:call` — sync, blocks for result |
| `!` message send | `gen_server:cast` — async fire-and-forget |
| Block | Erlang fun (closure) |
| Image | Running node(s) |
| Workspace | Connected REPL to live node (`Workspace` singleton) |
| Class browser | REPL introspection: `Beamtalk allClasses`, `Beamtalk help: Class` |

---

## Standard Library

76 classes implemented and tested. For detailed API documentation, see [API Reference](https://www.beamtalk.dev/apidocs/).

**Core types:**

| Class | Description |
|-------|-------------|
| **Integer**, **Float**, **Number** | Arbitrary precision arithmetic |
| **String**, **Symbol**, **Character** | UTF-8 text (String is a subclass of Binary), interned symbols, Unicode characters |
| **Boolean**, **True**, **False** | Boolean values with control flow |
| **Nil** (UndefinedObject) | Null object pattern |
| **Block** | First-class closures |

**Collections:**

| Class | Description |
|-------|-------------|
| **Binary** | Byte-level data — Collection subclass, parent of String ([ADR 0069](ADR/0069-string-subclass-of-binary.md)) |
| **Array** | Fixed-size indexed collection |
| **List** | Linked list with fast prepend (`#()` syntax) |
| **Dictionary** | Key-value map |
| **Set** | Unordered unique elements |
| **Bag** | Multiset — allows duplicate elements, counts occurrences |
| **Tuple** | Fixed-size heterogeneous container |
| **Queue** | O(1) amortised FIFO queue |
| **Interval** | Arithmetic sequence (`1 to: 10`, `1 to: 10 by: 2`) |
| **Stream** | Lazy, closure-based sequences ([ADR 0021](ADR/0021-streams-and-io-design.md)) |
| **Ets** | Shared in-memory tables (BEAM ETS wrapper) |

**Actors and concurrency:**

| Class | Description |
|-------|-------------|
| **Actor** | Base class for all actors (BEAM processes) |
| **Server** | Abstract Actor subclass for BEAM-level OTP interop (`handleInfo:`) ([ADR 0065](ADR/0065-complete-otp-primitives-actor-lifecycle.md)) |
| **Supervisor**, **DynamicSupervisor** | OTP supervision trees ([ADR 0059](ADR/0059-supervision-tree-syntax.md)) |
| **AtomicCounter** | Lock-free shared counter |
| **Timer** | Periodic and one-shot timers (linked to calling process via `spawn_link`) |
| **Pid**, **Reference**, **Port** | BEAM primitive types |

**Error handling:**

| Class | Description |
|-------|-------------|
| **Result** | Typed success/error for expected failures ([ADR 0060](ADR/0060-result-type-hybrid-error-handling.md)) |
| **Error**, **RuntimeError**, **TypeError** | Error hierarchy |
| **BEAMError**, **ExitError**, **ThrowError** | BEAM exception wrappers |
| **Exception** | Base exception type |

**I/O and system:**

| Class | Description |
|-------|-------------|
| **File**, **FileHandle** | File system operations |
| **Subprocess**, **ReactiveSubprocess** | OS process execution ([ADR 0051](ADR/0051-subprocess-execution.md)) |
| **OS**, **System** | Platform info and system operations |
| **Json**, **Yaml** | Data serialisation |
| **Regex** | Regular expression matching |
| **DateTime**, **Time** | Date/time operations |
| **Random** | Random number generation |

**Networking** (in [`beamtalk-http`](https://github.com/jamesc/beamtalk-http)):

| Class | Description |
|-------|-------------|
| **HTTPServer**, **HTTPClient** | HTTP server and client |
| **HTTPRouter**, **HTTPRoute**, **HTTPRouteBuilder** | Declarative HTTP routing |
| **HTTPRequest**, **HTTPResponse** | Request/response objects |

**Observability:**

| Class | Description |
|-------|-------------|
| **Tracing** | Actor observability and performance telemetry — always-on aggregates + opt-in trace capture ([ADR 0069](ADR/0069-actor-observability-and-tracing.md)) |

**Reflection and meta:**

| Class | Description |
|-------|-------------|
| **Class**, **Metaclass**, **ClassBuilder** | Class reflection and dynamic class creation |
| **Behaviour** | Shared behaviour protocol |
| **CompiledMethod** | Method introspection |
| **StackFrame** | Stack trace inspection |
| **TestCase**, **TestResult**, **TestRunner** | BUnit test framework — TestCase is a Value subclass with functional setUp ([ADR 0014](ADR/0014-beamtalk-test-framework.md)) |

### Binary — Byte-Level Data

Binary is a sealed Collection subclass for byte-level data. String is a subclass of Binary that adds grapheme-aware text operations. The class hierarchy is `Collection > Binary > String` ([ADR 0069](ADR/0069-string-subclass-of-binary.md)).

On BEAM, Beamtalk binaries map directly to Erlang binaries (`binary()`). All strings are binaries at runtime — the type system uses the subclass relationship so that String is accepted wherever Binary is expected (e.g. `File writeBinary:contents:` accepts strings without type warnings).

```beamtalk
// Construction
bin := Binary fromBytes: #(104, 101, 108, 108, 111)
bin := Binary fromIolist: #("hello", " ", "world")

// Byte access (1-based, Collection protocol)
bin := Binary fromBytes: #(104, 101, 108)
bin at: 1                    // => "h" (grapheme — runtime dispatches via String)
bin size                     // => 3

// Byte access (0-based, Erlang-compatible)
bin byteAt: 0                // => 104 (byte value)
bin byteSize                 // => 3 (byte count)

// Zero-copy slicing
bin := Binary fromBytes: #(1, 2, 3, 4, 5)
bin part: 1 size: 3          // => Binary (bytes 2, 3, 4)

// Concatenation
a := Binary fromBytes: #(1, 2)
b := Binary fromBytes: #(3, 4)
a concat: b                  // => Binary (1, 2, 3, 4)

// Byte list conversion
bin toBytes                   // => #(1, 2, 3, 4, 5)
Binary fromBytes: #(65, 66)  // => Binary

// UTF-8 decoding (Binary → String)
(Binary fromBytes: #(104, 101, 108, 108, 111)) asString           // => "hello"
(Binary fromBytes: #(104, 101, 108, 108, 111)) asStringUnchecked  // => "hello"

// Serialization (class methods)
etf := Binary serialize: #(1, 2, 3)
Binary deserialize: etf               // => #(1, 2, 3)
Binary deserializeWithUsed: etf       // => #(value, bytesConsumed)

// Collection protocol — Binary is a collection of bytes
bin := Binary fromBytes: #(65, 66, 67, 68, 69)
bin collect: [:ch | ch]       // => "ABCDE" (via String species)
bin select: [:ch | ch /= "C"]  // => "ABDE"
bin includes: "B"             // => true
bin isEmpty                   // => false
```

**Method override table (Binary vs String):**

| Method | On Binary | On String |
|--------|-----------|-----------|
| `at: index` | grapheme (1-based, via String at runtime) | grapheme (1-based) |
| `size` | element count (via String at runtime) | grapheme count |
| `byteAt: offset` | byte value (0-based) | inherited — byte value (0-based) |
| `byteSize` | byte count | inherited — byte count |
| `do: block` | iterate elements (via String at runtime) | iterate graphemes |
| `part: offset size: n` | byte-level slice, returns Binary | inherited — byte-level slice, returns Binary |
| `concat:` | byte concatenation, returns Binary | inherited — byte concatenation, returns Binary |
| `asString` | UTF-8 validation, returns String | no-op, returns self |
| `asStringUnchecked` | unchecked cast to String | no-op, returns self |

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

`File` provides class methods for reading, writing, and managing files and directories. Both relative and absolute paths are accepted; security relies on OS-level permissions (ADR 0063).

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
| `File absolutePath: path` | `String` | Resolve path to absolute |
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

@expect type
42 unknownMethod            // also suppresses method-not-found (DNU) hints

@expect unused
x := computeSomething       // unused-variable warning suppressed

@expect all
anything                    // any diagnostic suppressed (discouraged — use a specific category)
```

**Suppression categories:**

| Category | Suppresses |
|----------|-----------|
| `dnu` | Does-not-understand hints |
| `type` | Type mismatch warnings *and* method-not-found (DNU) hints |
| `unused` | Unused variable warnings |
| `all` | Any diagnostic on the following expression *(discouraged — use a specific category)* |

**`@expect type` for method-not-found diagnostics:**

`@expect type` suppresses DNU hints unconditionally. A common use-case is type-erasure boundaries where `Result.unwrap` (or any other method returning `Object`) causes the type system to lose track of the concrete type:

```beamtalk
// Result.unwrap returns Object — the type system cannot verify 'size' exists
@expect type
self assert: someResult unwrap size equals: 10
```

This is preferred over `@expect dnu` at type-erasure boundaries because it communicates *why* the diagnostic appears: a type-system limitation, not intentional dynamic dispatch.

**Declaration-level `@expect`:** In addition to suppressing diagnostics on expressions, `@expect` can be placed before `state:`/`field:` declarations and method definitions inside a class body. This suppresses diagnostics that fire on the declaration itself:

```beamtalk
typed Object subclass: Collection(E)
  @expect type
  first => (Erlang erlang) hd: self asErlangList   // polymorphic return — suppress missing-annotation warning
```

Declaration-level `@expect` supports the same categories and stale-directive rules as expression-level `@expect`.

**Unknown categories are parse errors:** Writing an unknown category (e.g. `@expect selfcapture`) is rejected at parse time with an error listing the valid names. This prevents typos from silently suppressing nothing.

**Stale directives:** If `@expect` does not suppress any diagnostic (because no matching diagnostic exists on the following expression or declaration), the compiler emits a warning to prevent directives from silently becoming out of date.

`@expect` works inside method bodies, on declarations in class definitions, and at module scope.

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
q size                        // => number of elements (O(n))
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
c reset                        // set to 0, return nil (not atomic with concurrent increments/decrements)
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

### TestCase — BUnit Testing

`TestCase` is a `Value subclass:` — setUp returns a new self with fields set (functional pattern), matching Erlang's EUnit and Elixir's ExUnit. The test runner threads the setUp return value to each test method. Each test gets a fresh copy, so tests cannot corrupt state for each other.

```beamtalk
TestCase subclass: CounterTest
  field: counter = nil

  setUp => self withCounter: (Counter spawn)

  testIncrement =>
    self.counter increment.
    self assert: (self.counter getValue) equals: 1
```

For multiple fields, `with*:` calls chain via cascades:

```beamtalk
TestCase subclass: IntegrationTest
  field: db = nil
  field: cache = nil

  setUp =>
    (self withDb: (DB connect)) withCache: (Cache spawn)

  testLookup =>
    self.cache at: "key" put: "value".
    self assert: (self.cache at: "key") equals: "value"
```

**Key points:**
- Declare test instance variables with `field:` (not `state:`) since TestCase is a Value subclass
- `setUp` returns a new self via `with*:` methods instead of using `self.x :=` assignment
- Each test method receives the setUp'd value as `self` — mutations to actor references work normally, but `self` itself is immutable

#### Suite-Level Setup — setUpOnce / tearDownOnce

For expensive fixtures shared across all tests in a class (database connections, ETS tables, supervisor trees), override `setUpOnce` and `tearDownOnce`. These run once per class, not once per test.

`setUpOnce` returns a fixture value accessible in each test method via `self suiteFixture`:

```beamtalk
TestCase subclass: DatabaseTest
  field: conn = nil

  setUpOnce => Database connect: "test_db"
  tearDownOnce => self suiteFixture close

  setUp => self withConn: self suiteFixture

  testQuery =>
    result := self.conn query: "SELECT 1"
    self assert: result equals: 1

  testInsert =>
    self.conn execute: "INSERT INTO t VALUES (1)"
    self assert: (self.conn query: "SELECT count(*) FROM t") equals: 1
```

**Lifecycle order:** `setUpOnce → (setUp → test → tearDown)* → tearDownOnce`

**Key points:**
- `setUpOnce` returns the fixture value (any type). The default returns `nil`.
- `tearDownOnce` accesses the fixture via `self suiteFixture` for cleanup.
- `tearDownOnce` runs even if tests fail.
- If `setUpOnce` raises an error, all tests in the class fail with a clear message.
- Per-test `setUp`/`tearDown` still run for each test, providing both shared and per-test state.

#### Parallel Test Execution

By default, `beamtalk test` runs test classes concurrently (`--jobs 0` = auto, uses BEAM scheduler count). Each class runs in its own process.

Test classes that touch global state (persistent_term, registered process names, global ETS tables) must opt out by overriding `serial`:

```beamtalk
TestCase subclass: TracingTest

  class serial -> Boolean => true

  setUp => Tracing clear. Tracing disable
  testEnable => self assert: Tracing enable equals: nil
```

Serial classes run alone after all concurrent classes complete.

Use `--jobs 1` for fully sequential execution, or `--jobs N` to limit concurrency.

From the REPL, use `TestRunner runAll: maxJobs` to control concurrency programmatically:

```beamtalk
TestRunner runAll: 4        // run up to 4 classes concurrently
TestRunner runAll            // sequential (default from REPL)
```

---

See [Tooling](beamtalk-tooling.md) for CLI tools, REPL, VS Code extension, and testing framework.
