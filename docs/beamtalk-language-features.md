# Beamtalk Language Features

Language features for Beamtalk. See [beamtalk-principles.md](beamtalk-principles.md) for design philosophy and [beamtalk-syntax-rationale.md](beamtalk-syntax-rationale.md) for syntax design decisions.

**Status:** v0.4.0 — implemented features are stable, including generics, protocols, union types, control flow narrowing, the `typed` class modifier, FFI type inference, package management with qualified `pkg@Class` names, native Erlang sources in packages, named actor registration, and Result-shaped supervisor lifecycles. See [ADR 0068](ADR/0068-parametric-types-and-protocols.md) for the type system design.

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
  - [Keyword Method Patching — `compile:source:` and `tryCompile:source:` (ADR 0082)](#keyword-method-patching--compilesource-and-trycompilesource-adr-0082)
  - [ChangeLog — Tracking In-Memory Changes (ADR 0082)](#changelog--tracking-in-memory-changes-adr-0082)
  - [Live Re-Checking on Reload (ADR 0105)](#live-re-checking-on-reload-adr-0105)
- [Actor Observability and Tracing (ADR 0069)](#actor-observability-and-tracing-adr-0069)
- [Announcements — Typed Events (ADR 0093)](#announcements--typed-events-adr-0093)
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

### Character Literal Methods

Character literals dispatch through the Character method table, so methods like `asString`, `printString`, `uppercase`, `lowercase`, and `class` return Character-appropriate values:

```beamtalk
$A asInteger              // => 65
$A asString               // => "A"
$A printString            // => "$A"
$A uppercase              // => 65
$A lowercase              // => 97
$A class                  // => Character
$A respondsTo: #uppercase // => true
Character value: 65       // => $A
```

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

**Initialization chaining** — `initialize` methods defined on ancestors run automatically, parent-first, before the child's own `initialize`. State threads through each call so the child sees the parent's mutations. Do **not** write `super initialize` yourself — the compiler warns on redundant `super initialize` sends inside an Actor's `initialize`. After the full chain runs, any typed `state:` field without a default (on the child or any ancestor, including cross-file parents) must have been assigned, or the actor crashes with `UninitializedStateError` naming the owning class. See [ADR 0078](ADR/0078-actor-initialize-inheritance.md).

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
       │    ├─ Collection(E) (abstract)
       │    │    └─ Set(E), Bag(E), Interval
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

### Sendability Tiers (ADR 0103)

Because a BEAM send *copies* the term, whether a value survives crossing a
process boundary depends on its class kind. The type checker derives a
**sendability tier** for every inferred type and warns (advisory only, per ADR
0100) when a value's tier is too weak for the boundary it crosses. No new
annotations are required for the common case — the class kind *is* the
annotation.

| Tier | What it is | Boundary behaviour |
|---|---|---|
| `Sendable` | Value kinds, primitives, symbols, `Reference` | copies perfectly — always fine |
| `SendableRef` | Actor kinds and the builtin `Pid` | copies the reference; identity preserved (hover-visible; no v1 diagnostic) |
| `HandleScoped(#scope)` | Object kinds wrapping a scoped runtime handle | `#process` handles warn when sent; `#node` handles are silent in v1 |
| `Unknown` | `Dynamic` / untyped FFI / unclassified Object | silent (nothing to grade on) |

A `Value` composes structurally, inheriting the **weakest** tier of its fields,
and generic collections inherit their element tier (`List(Port)` is
`HandleScoped`). The builtin table classifies the canonical hazards directly:
`Pid` → `SendableRef`, `Port`/`FileHandle` → `HandleScoped(#process)`,
`Reference` → `Sendable`, `Subscription` → `HandleScoped(#node)`.

**Checked boundaries** (a `HandleScoped(#process)` value warns): actor message
arguments, `spawnWith:` initial-state maps, blocks sent to actors (including
`Timer every:do:` and postfix `!` casts), and Announcement payloads. Local
blocks (`do:`, `collect:`, `ifTrue:`) and self-sends do not warn.

#### Declaring handle scope

A user `Object subclass:` that wraps runtime state may declare its scope with a
class-side `handleScope:` clause. The value is symbol-valued and the set is
open (`#process` and `#node` ship first); undeclared Object kinds stay
`Unknown` (silent).

```beamtalk
// node-global handle: fine to send within the node, not across nodes
sealed typed Object subclass: MetricsTable
  handleScope: #node
```

`handleScope:` is only meaningful on `Object`-kind classes (a `Value`/`Actor`
declaration is an advisory no-op). A companion lint nudges FFI-wrapping
(`native:`) Object classes that carry instance behaviour but declare no scope.
Suppress a sendability finding with `@expect sendability`.

Header clauses are parsed in a fixed order — write them as:
`modifiers` → `Superclass subclass: Name(TypeParams)` → `native: module` →
`handleScope: #scope`. In particular `native:` must precede `handleScope:`;
reversing them (`handleScope: #process native: pb`) silently drops the
`native:` clause (it falls into the class body).

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

The `with<SlotName>:` selector is built by uppercasing only the *first* letter of the slot name. Two slots whose names differ only by the case of their first letter (e.g. `x` and `X`) would generate the same setter selector — this is a compile error:

```beamtalk
// Compile error — rejected before the code runs:
// Value subclass: Weird
//   field: x = 0
//   field: X = 0   ← error: both `x` and `X` produce the setter `withX:`
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

// Full chain from self up to (and including) Object (or Object class for metaclass receivers)
Counter superclassChain        // => [Counter, Actor, Object]
Object superclassChain         // => [Object]
Counter class superclassChain  // => [Counter class, Actor class, Object class]
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

Note: bare `=` is **not** a valid Beamtalk operator — it has no entry in the parser's precedence table, so `x = y` fails to parse. Use `=:=` for value equality or `==` for reference equality.

#### Short-circuit boolean operators (`and:`/`or:`)

`and` and `or` are **not** binary operators. They are keyword messages that take blocks for short-circuit evaluation:
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

### Programmatic Class Creation (ClassBuilder) (ADR 0038 / ADR 0084)

`Object subclass: Counter …` is the grammar form, but a class can also be built
**programmatically** by cascading messages to a `ClassBuilder` and ending with
`register`. This is a first-class user API (the same protocol the compiler emits
for the grammar form): `register` returns the new, dispatchable class object.

```beamtalk
// Build a class in one cascade. register returns the canonical class object.
account := Object classBuilder
  name: #Account;
  superclass: Object;
  classVars: #{ #opened => 0 };
  fields: #{ #balance => 0 };
  methods: #{ #balance => [:inst | inst fieldAt: #balance] };
  classMethods: #{ #open => [:self | self.opened := self.opened + 1. self.opened] };
  register

account new balance        // => 0
Account open               // => 1  (also reachable by name — the name IS the class)
Account open               // => 2  (class-variable state threads correctly)
```

Each block value is **compiler-lowered** into the same dispatchable fun a
file-defined method produces, so class-variable mutations thread, and `super` /
`self` resolve — it is not a naive runtime closure. Class methods built this way
are callable and reflectable (ADR 0084).

**Incremental piece API.** A class can be assembled one piece at a time before
`register` — the "browser" use case. The `add*` setters write the same maps the
bulk setters do, and `removeMethod:` / `removeClassMethod:` drop a piece:

```beamtalk
Object classBuilder
  name: #Tally;
  superclass: Object;
  addClassState: #total default: 10;
  addField: #count default: 0;
  addMethod: #answer body: [:inst | 42];
  addClassMethod: #tally body: [:self | self.total := self.total + 5. self.total];
  register

Tally new answer           // => 42
Tally tally                // => 15
```

**Instantiation.** A purely programmatic (module-less) builder class instantiates
with `new` / `new:`, exactly like a compiled value type:

```beamtalk
point := Object classBuilder name: #P; superclass: Object; fields: #{ #x => 0, #y => 0 }; register
point new: #{ #x => 3, #y => 4 }     // => a P  (fields seeded from the map)
```

**Metadata parity.** Optional setters bring a builder-defined class to `:help`
parity with a file-defined one — `methodSignatures:` / `classMethodSignatures:`,
`methodDocs:` / `classMethodDocs:`, `methodReturnTypes:` / `classMethodReturnTypes:`,
`classDoc:`, `meta:`, and `isConstructible:`. The class-side method bodies are
also auto-indexed for `SystemNavigation` source-text queries.

**Class-side live edit.** A registered class's class method can be live-edited
with the same `>>` patcher used for instance methods (see *Live Patching* and
*Extension Methods* below), e.g. `Counter class >> reset => self.opened := 0`.
The class-side dispatch path picks up the new class method immediately (ADR 0084).

> The grammar form (`Object subclass: …`) remains the idiomatic way to define a
> class in source. The programmatic builder is for tooling, REPL exploration, and
> metaprogramming where the class shape is computed rather than written out.

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
// How File.readAll: is implemented — a thin wrapper.
// The `{ok, _} | {error, _}` tuples beamtalk_file returns become a Result
// at the FFI boundary (see "Result conversion" below), so the return type
// is Result(String, Error), not a bare String.
Object subclass: File
  class readAll: path :: String -> Result(String, Error) =>
    (Erlang beamtalk_file) readAll: path
```

> A class that delegates *wholesale* to one Erlang module can declare the module on `subclass:` and replace each body with `=> self delegate` instead of hand-writing the FFI call — see [`native:` for stateless Objects](beamtalk-native-erlang.md#native-stateless-objects--native-for-object). `Stream` (`native: beamtalk_stream`) is the worked example.

**Keyword mapping:** The Erlang function name is taken from the **first keyword** (with its colon removed); the remaining keyword *values* follow as positional arguments. `Erlang maps merge: a with: b` calls `maps:merge(A, B)` (not `maps:merge_with` — only the first keyword names the function). Unary selectors map directly: `Erlang erlang node` calls `erlang:node()`.

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

**Atom-enum precedence (type inference):** When the spec reader maps a pure-atom union type containing atoms beyond `ok`/`error`, it infers a singleton union rather than applying ADR-0076 Result recognition. Only unions whose atoms are a subset of `{ok, error}` infer as `Result`. For example, a spec of `text | json | xml` infers `#text | #json | #xml`, while `ok | error` infers `Result`.

> **⚠️ Type/runtime mismatch for enums containing `ok`/`error`.** Type inference and runtime coercion use different rules. A spec like `ok | error | pending` infers the singleton union `#ok | #error | #pending` at the type level, but runtime `ok`/`error` coercion (`coerce_result/1`) is unconditional and spec-unaware: at runtime a bare `ok` still arrives as `Result ok: nil` and a bare `error` as `Result error: nil`. So code that matches the inferred `#ok`/`#error` singletons passes the type checker but **never matches at runtime** for those branches. When a spec uses `ok` or `error` as semantic enum members, handle those call-site branches as `Result`, not as the inferred singleton (only the non-`ok`/`error` members — here `#pending` — arrive as singletons).

**`undefined` stays `#undefined`:** Erlang `undefined` in an FFI return type spec maps to the singleton `#undefined` (a `Symbol`), not `Nil`. The FFI boundary does not coerce `undefined` → `nil` — callsites that want nil semantics must convert explicitly.

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

**Error handling — wrap by default (ADR 0101):** every FFI call is *safe by default*. The boundary treats the two channels a BEAM function can use independently, and they are mutually exclusive per call (a function either returns or raises):

| Outcome of the call | Channel | Beamtalk result | Handle with |
|---------------------|---------|-----------------|-------------|
| *returns* `{ok, V}` / `{error, R}` | return value | a `Result(V, R)` **value** | `isOk` / `value` / `andThen:` |
| *raises* `error:Reason` (`badarg`, `{badkey,_}`, `function_clause`, …) | exception | a **raised** `#beamtalk_error{}` | `on:do:` / `ensure:`, or bubbles to the REPL |
| *raises* `exit:Reason` | exception | **propagates** unwrapped (an enclosing `on:do:` catches it as `erlang_exit`) | supervision / let-it-crash |
| *raises* `throw:Term` | exception | **passes through** unchanged | `^` / Beamtalk exceptions |

The guarantee: **a user never sees a raw Erlang error tuple.** A `badarg` becomes a structured `#beamtalk_error{}` (kind `type_error`) with a hint, not a bare `{badarg, [...]}` — most visible at the REPL, where the abstraction is most exposed. `exit:`/`throw:` are deliberately *not* wrapped, so `(Erlang erlang) exit: #killed` still terminates the process with reason `killed`.

**Same function, both channels.** Because the channels are orthogonal, one function can use both: `File readAll:` *returns* a `Result error:` for the modeled missing-file case, but *raises* a `#beamtalk_error{}` if called with a non-String path. The rule is **`Result` for expected/recoverable outcomes the API models, exceptions for misuse/faults.** Wrapping changes no return type — a raised `#beamtalk_error{}` is invisible to the type system, so `Result` stays the only type-visible error channel.

A raised FFI error is catchable as `BEAMError`, `ExitError`, or `ThrowError`. The handler block parameter is typed from the exception class argument, so `e` in `on: BEAMError do: [:e | ...]` is inferred as `BEAMError` (not `Dynamic`):

```beamtalk
[Erlang erlang error: #badarg] on: BEAMError do: [:e | e message]
// => "badarg"
// e is typed as BEAMError — `e message` type-checks without warnings
```

A `native:` method's wrapped error carries the Beamtalk `Class`/selector (e.g. `Type error in 'take:' on Stream`), whereas inline `(Erlang …)` FFI carries the Erlang-facing `ErlangModule` context (e.g. `Type error in 'atom_to_list' on ErlangModule`) — a documented limitation, since the proxy knows the MFA but not the calling class.

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

### FFI Collection Element Types

The spec importer carries **collection element types** from Erlang `-spec`
attributes (ADR 0075 amendment), so iterating an FFI-typed list binds block
parameters to the element type instead of `Dynamic`:

| Erlang spec | Imported Beamtalk type |
|-------------|------------------------|
| `[integer()]` | `List(Integer)` |
| `[{atom(), pos_integer(), atom()}]` | `List(Tuple(Symbol, Integer, Symbol))` |
| `{atom(), binary()}` | `Tuple(Symbol, String \| Binary)` |
| `[term()]` / `tuple()` | `List` / `Tuple` (uninformative elements collapse to the bare type) |

```beamtalk
// allSendsIn/1 is specced [{atom(), pos_integer(), atom()}]:
sends := (Erlang beamtalk_interface) allSendsIn: source
sends do: [:s |
  // s is Tuple(Symbol, Integer, Symbol) — no annotation needed
  selector := s at: 1   // inferred Symbol
  arity := s at: 2       // inferred Integer
]
```

**Literal-index tuple access:** Sending `at:` with a *literal* integer index to
a value of a known `Tuple(T1, …, Tn)` type infers the element type at that
1-based index:

```beamtalk
pair := someTupleTyped     // Tuple(Symbol, Integer)
pair at: 1                  // inferred Symbol
pair at: 2                  // inferred Integer
```

A non-literal index (`pair at: i`) or an out-of-range literal falls back to
`Dynamic` — no false-positive warning. An untyped `tuple()` spec (unknown
arity) stays bare `Tuple`, so `at:` on it remains `Dynamic` as before.

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
collect: block :: Block(E, R) -> Self =>
  self species withAll: (self inject: #() into: [:acc :each |
    acc addFirst: (block value: each)
  ]) reversed

// At call sites, Self resolves to the static receiver type:
// (List new collect: [:each | each])  — inferred return type: List
// (Set new collect: [:each | each])   — inferred return type: Set

// Self also substitutes inside nested generic return types
class named: name :: Symbol -> Result(Self, Error) => ...
// (Counter named: #c) — inferred return type: Result(Counter, Error)

// On parameterised receivers, Self preserves type arguments:
// (Box(Integer) new) someMethod — where someMethod -> Result(Self, Error)
//   inferred return type: Result(Box(Integer), Error)

// Self class — the receiver's metatype, i.e. the class object (ADR 0083).
class -> Self class => @primitive "class"
// (Counter new) class — inferred type: the metatype-of-Counter (rendered
// `Counter class`). Sends are routed class-side: `(Counter new) class new`
// infers a Counter instance, and `(Counter new) class instanceCount`
// type-checks against Counter's class-side methods.

// <ClassName> class — a named class metatype annotation.
// Valid in any type position (fields, parameters, return types, locals).
// Resolves to the metatype-of-<ClassName> (a tracked type, ADR 0083), so
// class-side methods on that class resolve without false DNU warnings, and a
// class value flows with type through variables, collections, and FFI returns.
// The metatype is name-only — the class object is unparameterized (ADR 0068),
// so there is no `List(E) class`, only `List class`. Type-erased at runtime.
field: actorClass :: Actor class | nil = nil
// After `actorClass isNil ifTrue: [^nil]`, actorClass is narrowed to
// `Actor class` — class-side methods like `isSupervisor` type-check.

// Class literal inference: a bare class literal (`Counter`) infers as the
// metatype `Counter class`, so class values stored in variables, collections,
// or returned from FFI calls route class-side sends correctly without annotation.
klass := Counter            // inferred Counter class (the metatype)
klass new                   // inferred Counter (an instance)
klass instanceCount         // resolves class-side method

// Metatype subtyping: `C class <: Class <: Behaviour <: Object`, so a class
// value satisfies `:: Class` / `:: Behaviour` parameters and `List(Behaviour)`
// FFI returns. `new` / `basicNew` on a *concrete* class metatype infers an
// instance of that class; on an *abstract* class (e.g. `Collection`,
// `Behaviour`) it stays Dynamic — instantiating an abstract class is an error.
```

### Current Semantics

- Type mismatch diagnostics are warnings, never compile-stopping errors.
- Invalid annotation forms (e.g., `Self` in parameter position) are errors.
- `typed` classes require parameter/return annotations on non-primitive methods.
- Data annotations (`field: x :: Integer = 0` on Value, `state: x :: Integer = 0` on Actor) are checked for defaults and assignments. When a type annotation is present, the default value is optional (`field: x :: Integer` / `state: x :: Integer`) — the type annotation is the contract, and `nil` is used internally.
- Local variable annotations (`name :: Type := expr`) declare the binding's type at type-erasure boundaries. See [Local Variable Type Annotations](#local-variable-type-annotations) below.
- Complex annotations (e.g., unions/generics) are parsed and accepted; deeper checking is phased in.
- `Self` in return position resolves to the static receiver class. Using `Self` as a parameter type is an error (unsound with subclassing).

### Local Variable Type Annotations

Local variables can carry a type annotation using `name :: Type := expr`. The declared type overrides the inferred type of the right-hand side, which is useful at type-erasure boundaries (FFI returns, deserialization, untyped APIs). The annotation is erased at codegen — there is no runtime effect.

```beamtalk
x :: Integer := 42
dict :: Dictionary := Binary deserialize: content
name :: String | nil := dictionary at: "name"
r :: Result(Integer, Error) := computeSomething
```

Supported type forms: simple (`Integer`), parametric (`Result(T, E)`), and union (`String | nil`).

**Type checking:** The compiler warns when the RHS type is unrelated to the declared type (e.g., `x :: Integer := "hello"`). Narrowing assertions — where the declared type is more specific than the inferred type — are accepted silently, since the annotation communicates that the runtime type is known to be more specific (BT-2015). `Dynamic` and `Never` RHS types are always accepted.

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
> Integer conformsTo: #Printable
=> true

> Integer conformsTo: #NonExistentProtocol
=> false

> Integer protocols
=> [#Printable, #Comparable]

> Protocol requiredMethods: #Printable
=> [#asString, #printString]

> Protocol conformingClasses: #Printable
=> [Integer, Float, String, Boolean, Symbol, Array, ...]
```

`conformsTo:` returns `false` for unknown or non-protocol names — a class cannot conform to something that is not a registered protocol.

### Diagnostic Philosophy

Protocol conformance issues are **warnings, never errors**:

| Situation | Severity |
|---|---|
| Protocol conformance unverifiable | Warning |
| Missing method for protocol | Warning |
| Namespace collision (class + protocol same name) | Error (structural) |

### Two-Protocol String Model (Debug / Display)

Beamtalk follows a **two-string-protocol** model (ADR 0094), mirroring Rust's `Debug` / `Display` split:

- **`printString` = Debug.** The self-describing, structural representation used by the REPL, logs, and any *other* `printString` that nests this object. It is the **REPL default** — evaluating an expression shows its `printString`. It is *derived* by default, so you rarely override it.
- **`displayString` = Display.** The human-facing representation. It is the hook the **language** pulls during **string interpolation** — every `{...}` segment renders its value via `displayString`. It **defaults to `printString`**; you *override* it when a value has a natural human form (e.g. `Money` → `$10.50`).
- **`inspect`** is the **navigable-inspector verb** (ADR 0095): `anObject inspect` opens an `Inspector` cursor (`Inspector on: self`), *not* a string. The cursor drills into fields (`at:`/`fields`/`path`), `refresh`es a snapshot, renders a `printString` text tree, and serialises to the MCP/browser wire form (`asDictionaries`/`asDictionary`). For the **structural Debug string** — formerly the `inspect` result — use `printString`. (Repurposed in ADR 0095 Phase 3, [BT-2504](https://linear.app/beamtalk/issue/BT-2504); the redesign ADR 0094 deferred.)

`String` demonstrates the Debug/Display split directly: `"hi" printString` → `"\"hi\""` (quoted, Debug) while `"hi" displayString` → `hi` (plain, Display).

#### Default `printString` forms by class kind

The `a`/`an` article prefix (the old `a Point` / ungrammatical `a Integer` default) is **removed entirely**. The default `printString` now takes one of four visually distinct forms, one per kind of thing:

| Class kind | Default `printString` | Example |
|------------|-----------------------|---------|
| **Value** (immutable data) | `ClassName(field: value, ...)` — class-headed, **labelled** fields, in sorted field order | `Point(x: 3, y: 4)`; no fields → `Point()` |
| **Actor** (live process) | `Actor(ClassName, pid)` — kind-headed, **positional** | `Actor(Counter, 0.123.0)` |
| **Supervisor** (supervising process) | `Supervisor(ClassName, pid)` / `DynamicSupervisor(ClassName, pid)` | `Supervisor(WebApp, 0.200.0)` |
| **Object** (plain reference) | bare `ClassName`, or a class-defined form | `FileHandle`, or e.g. `#Pid<0.123.0>` for raw primitives |

Value forms carry `field:` **labels** while process forms are **positional**; the process heads (`Actor` / `Supervisor` / `DynamicSupervisor`) are **reserved kind words** no user Value class may shadow, so the two shapes are unambiguous. Raw platform primitives (`Pid`, `Port`, `Reference`, `Tuple`) keep their Erlang-native `#Pid<…>` / `#Port<…>` rendering — they *are* Erlang terms.

Nested `Value` fields expand recursively (so `Line(from: Point(x: 0, y: 0), to: Point(x: 3, y: 4))` shows in full), each rendered via its own `printString` (Debug form — strings stay quoted), bounded by depth (default 5), width, and total-length caps with a cycle guard; truncated positions render as `...`.

### Printable Protocol and Display Methods

The `Printable` protocol is the standard contract for objects that can represent themselves as strings. It requires two methods:

- **`asString`** — a human-readable representation (for end-user display)
- **`printString`** — a developer-oriented (Debug) representation (for debugging, logging, and REPL display)

Most stdlib classes conform automatically because `Object` provides a default `printString` (the bare class name, or the structural `ClassName(field: value, ...)` form for `Value` subclasses) and most subclasses implement `asString`. Custom classes only need to implement these two methods to conform:

```beamtalk
Value subclass: Point
  field: x = 0
  field: y = 0

  // Human-readable
  asString -> String => "({self.x}, {self.y})"

  // Developer-readable (REPL display)
  printString -> String => "Point({self.x}, {self.y})"
```

`displayString` is **not** part of `Printable` (deferred per ADR 0094 §5), and `inspect` is not part of any protocol — so the two-protocol changes leave protocol conformance unaffected.

The related display methods on `Object` are:

| Method | Behaviour |
|--------|-----------|
| `asString` | Human-readable string conversion (override per class) |
| `printString` | **Debug** representation — self-describing, structural; the REPL default and what nested rendering uses |
| `displayString` | **Display** representation — the string-interpolation `{...}` hook; defaults to `printString`, override for a natural human form |
| `inspect` | Opens an `Inspector` cursor on the receiver (`Inspector on: self`) — a navigable, drillable view (ADR 0095). For the structural Debug string, use `printString`. |
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

### Navigable Inspector (ADR 0095)

`printString` renders an object to *one* string. The **Inspector** lets you *navigate into* it — drill through fields, render across surfaces (REPL text tree, MCP/browser wire form), and re-snapshot live actor state. `anObject inspect` is the shorthand for `Inspector on: anObject`; it returns a cursor, **not** a string.

```beamtalk
i := (Point x: 3 y: 4) inspect      // an Inspector cursor (#value kind)
i fields                            // the drillable InspectorField records (x, y)
i at: #x                            // Result(Inspector) — a child cursor on the value 3
(i at: #x) unwrap subject           // => 3
i printString                       // an indented text tree (Inspector(Point) + fields)
```

**One polymorphic class, four `kind`s.** A single `Inspector` carries a `kind` tag rather than a subclass per kind — classification lives in the runtime shim:

| `kind` | Subject | Fields are… |
|--------|---------|-------------|
| `#value` | an immutable `Value` (or scalar) | its slots, in ADR 0094 sort order (`#slot`) |
| `#actor` | a live actor | a lazy, timeout-guarded `sys:get_state` snapshot of its state |
| `#collection` | `List`/`Array`/`Set`/`Dictionary`/`Bag` | a **window** (page size 50) of `#element` / `#association` fields |
| `#foreign` | a non-Beamtalk OTP process | best-effort `process_info` + a guarded state snapshot (`#processInfo`) |

A wedged, dead, or non-`sys` actor degrades to a single `#status: #unavailable` field — it never crashes.

**Navigation is immutable.** Every navigation message returns a *new* cursor, so a UI can hold several at once:

| Message | Returns |
|---------|---------|
| `fields` | `List(InspectorField)` — the current window of drillable fields |
| `at: key` | `Result(Inspector)` — a child cursor on that field's value (`#no_such_field` on a miss) |
| `parent` / `root` | the parent cursor / the top of the drill path (`nil` parent at the root) |
| `path` | the breadcrumb of drilled keys from the root |
| `refresh` | a fresh cursor on a newly-captured snapshot (the original is unchanged) |
| `size` | the cheap full element count (for `#collection`), else the field count |
| `page: n` | a new cursor on the `n`-th window (1-based) of a large collection |
| `printString` / `printStringExpanded: depth` | the indented text tree (depth 1 = immediate fields) |
| `asDictionaries` / `asDictionary` | the typed cross-surface wire form (one dict per field / the cursor envelope) |

Each `InspectorField` is an immutable `Value` record with `name` (the navigation key), `label`, `value`, `kind`, and `drillable` (`isLeaf` is its negation).

**`evaluate:` is values-only.** On a `#value` cursor, `i evaluate: "self x + self y"` compiles and runs the expression with `self` bound to the inspected value, returning a `Result` — never raising. On an `#actor` cursor it returns `Result error:` with kind `#actor_eval_unsupported` (actor evaluate-in-context is a deferred §7 follow-up). Live updates are **poll-only**: re-issue `refresh`.

```beamtalk
i := (Point x: 3 y: 4) inspect
(i evaluate: "self x * 10") unwrap        // => 30
(i evaluate: "self nonesuch") isError     // => true  (a Result error:, not a crash)
```

**Deferred §7 seams** (not in v1): actor evaluate-in-context (live routing), per-class `inspectorFields` custom views, `sealedFromInspection`, and push live updates (poll-only for now).

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
name size              // Warning: Nil does not respond to 'size'
```

Similarly, `false` in type position resolves to `False` — used for Erlang FFI patterns:

```beamtalk
entry :: Tuple | false := ErlangLists keyfind: key
```

**Singleton members.** Singleton symbol types (`#foo`, a subtype of `Symbol`) may appear in any type position — including unions — to express a closed set of atom values:

```beamtalk
// a parameter that is an Integer or the sentinel #infinity
withTimeout: ms :: Integer | #infinity => ...

// a closed enum of singletons
restart: policy :: #temporary | #transient | #permanent => ...
```

A singleton receiver resolves methods against `Symbol`'s protocol — `#infinity asString` infers `String`, and an unknown selector produces a DNU hint naming the singleton (e.g. `#infinity does not understand 'frobnicate'`). Equality comparisons (`=:=`, `==`, `/=`, `=/=`) are excluded from this redirect so statically-decidable comparison hints still fire.

Discriminate a singleton union with `=:=` (identity) and the branches narrow — see [Control Flow Narrowing](#control-flow-narrowing).

### Difference and Intersection Types (`\` / `&`)

Beyond `|` (union), type annotations support two more set-theoretic operators ([ADR 0102](ADR/0102-set-theoretic-type-operators.md)):

- **`\` (difference)** — "T without U". Written `Base \ Excluded`, it expresses a co-finite set: every value of `Base` except the named ones. Today this is only meaningful for symbol singletons subtracted from `Symbol` (or from a smaller `\`-chain) — nominal-class difference (e.g. `Object \ Number`) is not yet defined.
- **`&` (intersection)** — the general form of the class/protocol composition ADR 0068 already specified (`Collection(Object) & Comparable`). Class ∩ class reduces via the hierarchy (the narrower class, or `Never` for unrelated sealed classes); class ∩ protocol is the interesting, irreducible case.

```beamtalk
// "any Symbol except #north" — a co-finite atom set
tag :: Symbol \ #north := #south

// chained difference — left-associative, same as subtracting a union:
// equivalent to "any Symbol except {#north, #south}"
heading :: Symbol \ #north \ #south := #east

// union binds looser than difference: `Integer | Symbol \ #foo` is
// `Integer | (Symbol \ #foo)`, not `(Integer | Symbol) \ #foo`
withSentinel: ms :: Integer | Symbol \ #infinity => ...

// intersection: class ∩ protocol (ADR 0068's operator, now general)
describe: value :: Integer & Printable -> String => value asString

// chained intersection — left-associative, same tier as `\`
requires: value :: A & B & C => ...
```

**Precedence** (lowest-binding to highest), all only inside a type annotation — `&` and `\` remain ordinary binary message selectors in value position, unaffected:

| Operator | Meaning | Binding |
|---|---|---|
| `\|` | union | lowest |
| `&` | intersection | middle |
| `\` | difference | middle (left-assoc, same tier as `&`) |
| _(atomic type)_ | class name / singleton / generic | highest |

So `Integer | Symbol \ #foo` parses as `Integer | (Symbol \ #foo)` — `\` binds tighter than `|`. Within one operator, chains are left-associative: `Symbol \ #a \ #b` parses as `(Symbol \ #a) \ #b`, and `A & B & C` parses as `(A & B) & C`.

**Grouping parentheses.** `(...)` is a grouping operator inside a type annotation, just as in value expressions (in addition to its generic-argument use, `Result(T, E)`). A group wraps any annotation — unions, `&`/`\` chains, generics — and groups nest. Grouping is purely syntactic: the parenthesised annotation is the same annotation, so `(Integer)` means `Integer`, and `(A | B) | C` is the same flat three-member union as `A | B | C`.

```beamtalk
// subtract a whole union in one step — equivalent to the chain
// `Symbol \ #a \ #b`, which the algebra normalises to the same result
tag :: Symbol \ (#a | #b) := #c

// a grouped difference as a union member (parens redundant here — `\`
// already binds tighter than `|` — but allowed)
withSentinel: ms :: Integer | (Symbol \ #infinity) => ...
```

Grouping is also how you mix `&` and `\` in one annotation: **mixing them in the same chain without parentheses is a deliberate parse error**, not a left-associative fallback — `A & B \ #c` is rejected with "Cannot mix `&` and `\` in a type annotation without parentheses; parenthesise to disambiguate", because `(A & B) \ #c` and `A & (B \ #c)` differ and neither reading is obviously the intended one. Parenthesise the reading you mean:

```beamtalk
// intersect first, then subtract
narrow -> (A & B) \ #c => ...

// subtract first, then intersect
narrow -> A & (B \ #c) => ...
```

The lexer also greedily merges `\\` (two backslashes) into the Smalltalk modulo selector, so a doubled backslash in type position — the natural typo for `\` — gets a targeted diagnostic ("did you mean `\`?") instead of a confusing generic parse error.

Difference types show up in [Control Flow Narrowing](#control-flow-narrowing) too: the false branch of a singleton equality test (`x =:= #foo`) narrows `x` to `Symbol \ #foo`, and hover displays that co-finite type directly.

### Control Flow Narrowing

When the type checker recognises a type-testing pattern followed by `ifTrue:` / `ifFalse:`, it narrows the variable's type inside the block scope:

```beamtalk
// class identity check — narrows to exact class
process: x :: Object =>
  x class =:= Integer ifTrue: [
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

// isKindOf: guard-and-return also narrows the rest of the method (BT-2825)
render: coll :: Printable | Nil -> String =>
  coll isNil ifTrue: [^""]
  (coll isKindOf: List) ifFalse: [^""]
  items :: List := coll   // no `@expect type` needed — coll is List here
  items inject: "" into: [:acc :item | acc ++ item asString]
```

**Supported narrowing patterns:**

| Pattern | Narrows to | Scope |
|---|---|---|
| `x class =:= Foo ifTrue: [...]` | `x` is `Foo` in true block | True block only |
| `x isKindOf: Foo ifTrue: [...]` | `x` is `Foo` in true block | True block only |
| `x isKindOf: Foo ifFalse: [...]` | `x` is `T \ Foo` in false block | False block only |
| `x isKindOf: Foo ifFalse: [^...]` | `x` is `Foo` after the statement | Rest of method |
| `x isKindOf: Foo ifTrue: [^...]` | `x` is `T \ Foo` after the statement | Rest of method |
| `x isKindOf: Foo ifTrue: [^...] ifFalse: [...]` | `x` is `T \ Foo` after the statement | Rest of method |
| `x isNil ifTrue: [^...]` | `x` is non-nil after the statement | Rest of method |
| `x isNil ifTrue: [self error: "..."]` | `x` is non-nil after the statement | Rest of method |
| `x isNil ifFalse: [...]` | `x` is non-nil in false block | False block |
| `x isNil ifFalse: [^...]` | `x` is nil after the statement | Rest of method |
| `x isNil ifTrue: [^...] ifFalse: [...]` | `x` is non-nil in false block | False block |
| `x ifNotNil: [:v \| ...]` | `v` is non-nil in block | Block only |
| `x ifNil: [...] ifNotNil: [:v \| ...]` | `v` is non-nil in notNil block | NotNil block |
| `x notNil and: [...]` | `x` is non-nil in block, including nested block-argument positions | Block only |

The diverging-guard pattern (`isNil ifTrue: [self error: "..."]`) recognises any block whose body infers as `Never` — including calls to `error:`, `notImplemented`, or any `-> Never` method — not just non-local returns (`^`). Narrowing also works on `self.field` reads: inside `self.field isNil ifFalse: [...]`, the field narrows to non-nil within the block.

**`notNil and:` (BT-2872):** `x notNil and: [...]` narrows `x` to non-nil for the whole block argument — not just where `x` is a further send's receiver, but in any nested position, including as an argument to a binary send inside a further-nested block (`local notNil and: [local > 0 and: [5 >= local]]` narrows `local` inside `5 >= local` too). The narrowing does not survive past the block — a later unguarded use of `x` after the `and:` send is unaffected.

**`isKindOf:` guard-and-return (BT-2825):** the same diverging-guard treatment `isNil` gets also applies to `isKindOf:` — `(x isKindOf: Foo) ifFalse: [^default]` proves `x` is `Foo` for the rest of the method, and `(x isKindOf: Foo) ifTrue: [^default]` proves `x` is `T \ Foo`. This also closes a related gap: when `x`'s declared type is a Protocol (e.g. `Printable`) and `Foo` is a concrete class, the narrowed type collapses to the bare `Foo` — a runtime `isKindOf: Foo` check is a stronger proof than the protocol annotation, so the narrowed value is assignable to a `Foo`-typed local without an `@expect type` escape hatch.

### Conditional Return Type Inference

`ifTrue:ifFalse:` on `Boolean` (and `True`/`False`) is declared as `Block(R), Block(R) -> R` — the type checker unifies both arms to a common return type. The result of a conditional expression is now statically typed rather than `Dynamic`:

```beamtalk
x := condition ifTrue: [42] ifFalse: [0]
// x is inferred as Integer (not Dynamic)

result isOk ifTrue: [result unwrap] ifFalse: [default]
// inferred as the common type of both arms
```

The solo forms — `ifTrue: [...]` and `ifFalse: [...]` — can't unify to a bare `R` the way the two-armed form does: `Boolean>>ifTrue:`/`ifFalse:` are deliberately declared with no return type, because on an unnarrowed `Boolean` receiver the checker can't statically prove whether `True` or `False` handles the send, and the sibling override (e.g. `False>>ifTrue:`) never invokes the block — it returns `self` instead. Rather than collapsing all the way to `Dynamic`, a solo send infers the union of that `Boolean` self-branch and the block's own return type (BT-2868):

```beamtalk
flag :: Boolean := x > y
flag ifTrue: [1]
// inferred as Boolean | Integer, not Dynamic

flag ifFalse: ["no"]
// inferred as Boolean | String
```

A receiver already narrowed to exactly `True` or `False` (e.g. inside an `x isKindOf:`-style guard, or after control-flow narrowing) is unaffected — `True>>ifTrue:`/`False>>ifFalse:` declare concrete `-> R` return types and resolve normally, without the extra `Boolean` union member.

`ifNil:ifNotNil:` (and `ifNotNil:ifNil:`) on nullable receivers also infers a branch-union return type — `typeof(nilBranch) | typeof(notNilBranch)`. A branch containing a non-local return (`^`) contributes `Never`, leaving only the surviving branch's type:

```beamtalk
name :: String | nil := dictionary at: "name"
result := name ifNil: ["unknown"] ifNotNil: [:n | n size]
// result is inferred as String | Integer

value := name ifNil: [^nil] ifNotNil: [:n | n]
// value is inferred as String (nil branch contributes Never, skipped)
```

The solo forms — `ifNil: [...]` and `ifNotNil: [:v | ...]` — infer the same way, unioning the block's return type with the "self" branch (executed when the nil-check doesn't match):

```beamtalk
name :: String | nil := dictionary at: "name"
name ifNil: ["unknown"]
// inferred as String (T | R, where T = String and R = String both dedup)

count :: Integer | nil := dictionary at: "count"
count ifNil: ["none"]
// inferred as Integer | String (T | R)

name ifNotNil: [:n | n size]
// inferred as Integer | Nil (R | Nil)
```

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
Actor(Counter, _)

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
c increment!
```

The `!` is **postfix** — it terminates the send it applies to. A cast is only
legal as a bare statement: using its (always-`nil`) value in an assignment,
return, or argument is a parse error (`cast_in_expression_error`).

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

### Static Typing of Actor Protocols (ADR 0104)

An `Actor subclass:`'s public method set *is* its message protocol — the checker types actor sends exactly as ordinary method sends, with no parallel channel type and no new syntax. Four typing rules apply (all advisory per [ADR 0100](ADR/0100-diagnostic-severity-open-world.md), and static-only — no runtime, codegen, or wire change):

1. **A sync send types as the method's return.** `c increment` on a `Counter` whose `increment` declares (or infers) `-> Integer` types as `Integer`, and forwards its declared/inferred return to callers:

   ```beamtalk
   c := Counter spawn
   c increment          // :: Integer — the method's declared/inferred return
   ```

2. **A bare cast (`!`) types as `Nil`.** The fire-and-forget `gen_server:cast` has no synchronous reply, so a cast *statement* evaluates to `Nil` regardless of the target method's return type. (Using a cast's value in an assignment, return, or argument is already a parse error — see **Explicit Async Cast** above). One consequence: a method declaring a non-`Nil` return whose body *ends* in a bare cast now warns "body returns Nil" — usually a genuine bug where a mutator returns nothing:

   ```beamtalk
   c increment!         // :: Nil — no reply is awaited

   // ⚠️ Method 'bump' declares return type Integer, but body returns Nil
   bump -> Integer => c increment!
   ```

3. **`spawnWith:` keys are checked against `state:` slots.** The keys of a literal init-state map are validated against the actor's declared `state:` slots; an unknown key is a Warning (a provably-failing construction, not merely an unresolved selector) with a typo suggestion naming the nearest slot. When a slot is typed, the literal value's type is checked against it too:

   ```beamtalk
   Counter spawnWith: #{#count => 0}     // :: Counter — key `count` is a declared slot
   Counter spawnWith: #{#cuont => 0}     // ⚠️ unknown state key `cuont` — did you mean `count`?
   ```

   Only a *literal* map is inspected; a `spawnWith:` argument flowing in through a variable is not key-checked. The rule fires only for `Actor subclass:` receivers.

4. **`withTimeout:` is transparent; cross-process DNU grades like a local send.** `withTimeout:` returns a value typed as the *wrapped* actor (not the opaque `TimeoutProxy`), so forwarded calls resolve the wrapped class's real return types. A timeout raises rather than returning, so method return types are unchanged. An unknown selector on a statically-known actor gets the same knowledge-graded [ADR 0100](ADR/0100-diagnostic-severity-open-world.md) diagnostic as a local send — the process boundary is invisible to the checker:

   ```beamtalk
   (db withTimeout: 30000) query: sql    // resolves query:'s real return, not Dynamic

   logger := Logger spawn
   logger logg: "hi"                     // ⚠️ Logger does not understand 'logg:' — did you mean 'log:'?
   ```

See [ADR 0104](ADR/0104-typed-actor-protocols.md) for the full rationale, prior art, and the metaclass-aware constructor inference (ADR 0083) that types `spawn` / `spawnWith:`.

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

Start the supervisor with `supervise`. It registers under its class name so it can be found from anywhere. `supervise` and `terminate:` both return `Result` values ([ADR 0080](ADR/0080-supervisor-lifecycle-result.md)) — use `unwrap` at boot / in the REPL, or `ifOk:ifError:` / `andThen:` for recoverable flows:

```beamtalk
// Boot-style: crash on failure (application boot, test setup, REPL exploration)
app := (WebApp supervise) unwrap
// => Supervisor(WebApp, _)

// Idempotent — second call also returns a successful Result wrapping the
// already-running supervisor, without restarting
(WebApp supervise) isOk
// => true

// Recoverable form — branch on the Result explicitly
(WebApp supervise)
  ifOk:    [:sup | sup count]
  ifError: [:e | Logger error: e message]

// Find the running instance by class name (no reference needed — returns the
// bare supervisor or nil, unchanged from pre-ADR-0080 semantics)
WebApp current
// => Supervisor(WebApp, _)
```

Inspect and manage children:

```beamtalk
app count                                 // => 3  (number of running children)
app children                              // => ["DatabasePool","HTTPRouter","MetricsCollector"]  (child ids)
(app which: DatabasePool) unwrap           // => Actor(DatabasePool, _)  (running child instance)
(app terminate: HTTPRouter) unwrap        // gracefully stop a single child; Result(Nil, Error)
app stop                                  // stop the supervisor and all children (unchanged — Nil)

// After stop:
WebApp current                            // => nil
```

`terminate:` is **idempotent** — terminating a child that is already gone returns `Result ok: nil`, not an error (see [idempotent-startup convention](#supervisor-idempotent-startup-convention-adr-0080) below).

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

Use `withName:` (and the `withName:withRestart:` / `withName:withArgs:` / `withName:withRestart:withArgs:` combinators) to have the supervisor register the child atomically under a Symbol name on each restart. Named specs emit a `#spawnAs:` / `#spawnWith:as:` startFn so re-registration happens in the same OTP call that starts the process — held `Actor named:` references survive restarts (see [ADR 0079](ADR/0079-named-actor-registration.md) and the Actor Named Registration section below). `name` and `classMethod` cannot be combined on the same spec.

```beamtalk
Supervisor subclass: WebApp
  class children =>
    #((Counter supervisionSpec withName: #counter withRestart: #permanent))
```

### Dynamic Supervisor

Subclass `DynamicSupervisor` to manage pools of actors started at runtime. Override `class childClass` to declare which actor class the pool manages.

```beamtalk
DynamicSupervisor(Worker) subclass: WorkerPool
  class childClass => Worker
```

```beamtalk
pool := (WorkerPool supervise) unwrap
// => DynamicSupervisor(WorkerPool, _)

// Start children dynamically — startChild returns Result(C, Error) where C is
// the DynamicSupervisor's child class parameter (Worker here)
w1 := pool startChild unwrap        // => Actor(Worker, _)
w2 := pool startChild unwrap        // => Actor(Worker, _)
pool count                          // => 2

// Recoverable variant — useful when a failing init should not abort the caller
pool startChild
  ifOk:    [:w | w process: 21]
  ifError: [:e | Logger warn: e message]

// Terminate a specific child — idempotent (Ok(nil) even if already gone)
(pool terminateChild: w1) unwrap    // => nil
pool count                          // => 1

// Stop the whole pool (unchanged — Nil, let-it-crash teardown)
pool stop
WorkerPool current                  // => nil
```

### Nested Supervisors

Supervisors can be nested — include another supervisor class in `children`:

```beamtalk
Supervisor subclass: AppRoot
  class children => #(DatabaseSupervisor WebTierSupervisor MetricsSupervisor)
```

Nested supervisor children are identified by `isSupervisor => true` and started via OTP `start_link/0`, ensuring they are correctly linked into the supervision tree. The outer supervisor shuts down inner supervisors (and all their children) gracefully on `stop`.

```beamtalk
root := (AppRoot supervise) unwrap
root count                          // => 3
(root which: DatabaseSupervisor) unwrap  // => Supervisor(DatabaseSupervisor, _)
```

### Lifecycle API returns Result (ADR 0080)

Supervisor lifecycle methods that can fail at a startup / registry boundary return a `Result`:

| Method | Signature | Error `kind`s |
|--------|-----------|---------------|
| `Supervisor class>>supervise` | `-> Result(Self, Error)` | `#supervisor_start_failed`, `#stale_handle` |
| `Supervisor>>terminate: aClass` | `-> Result(Nil, Error)` | `#terminate_failed`, `#stale_handle` |
| `Supervisor>>which: aClass` | `-> Result(Object, Error)` | `#stale_handle` |
| `DynamicSupervisor class>>supervise` | `-> Result(Self, Error)` | `#supervisor_start_failed`, `#stale_handle` |
| `DynamicSupervisor>>startChild` / `startChild: args` | `-> Result(C, Error)` | `#child_start_failed`, `#stale_handle` |
| `DynamicSupervisor>>terminateChild: child` | `-> Result(Nil, Error)` | `#terminate_failed`, `#stale_handle` |

`stop`, `current`, `children`, and `count` are **unchanged** — they are teardown / lookup / inspection operations over an already-valid handle and follow let-it-crash semantics (teardown) or nil-on-miss (lookup), matching the rules established in [ADR 0079](ADR/0079-named-actor-registration.md) for the parallel `Actor` surface.

This mirrors `Actor spawnAs:` / `Class named:` from the [Actor Named Registration](#actor-named-registration-adr-0079) section — both APIs speak `Result` at registry / lifecycle boundaries so call sites that chain actor spawns and supervisor operations stay on a single error idiom.

Errors carry structured `beamtalk_error` values (ADR 0015) with a Symbol `kind` and a human-readable `message`. REPL display shows them as `Result error: (beamtalk_error <kind>)` so they are greppable in logs and aggregatable in metrics.

<a id="supervisor-idempotent-startup-convention-adr-0080"></a>

### Idempotent-startup convention

Across every supervisor lifecycle method, **an operation returns a successful `Result` when the caller's target end state is already in effect**, regardless of whether this call or a prior one established it. The rule is "does the target state hold *now*?" — not "did this call change the input?"

| Method | Target state | Idempotent case |
|--------|--------------|-----------------|
| `supervise` | "this supervisor is running" | OTP `{already_started, Pid}` → `Result ok: sup` |
| `startChild` / `startChild:` | "a child of the configured class is running" | fresh start → `Result ok: child` |
| `terminate:` / `terminateChild:` | "this child is not running" | OTP `{error, not_found}` → `Result ok: nil` |

This matters in practice: you can call `WebApp supervise` at every entry point of your application without branching on "is this the first call?" — the second caller gets the already-running supervisor back in the `ok` branch. Similarly, a cleanup path that calls `app terminate: StaleChild` succeeds whether the child was still alive or already gone, so you never have to swallow a raise to express "stop it if it's running."

`Error` is reserved for outcomes the caller cannot trivially ignore:
- `#supervisor_start_failed` — `start_link` returned a non-`already_started` reason (init crash, config error, resource exhaustion).
- `#child_start_failed` — child's `init/1` raised, or the supervisor rejected the spec.
- `#terminate_failed` — `supervisor:terminate_child/2` returned a reason other than `not_found` (supervisor crash, timeout).
- `#stale_handle` — the supervisor process itself is dead.

### Call-site patterns

**Boot-style: crash on failure.** Use `unwrap` at application boot, test setup, and in the REPL — the resulting exception carries the structured error payload.

```beamtalk
app  := (WebApp supervise) unwrap
pool := (WorkerPool supervise) unwrap
w    := pool startChild unwrap
```

**Recoverable: branch on the Result.** Use `ifOk:ifError:` (or `andThen:` / `mapError:`) when a failure should be logged or retried rather than crashing the caller.

```beamtalk
(WebApp supervise)
  ifOk:    [:sup | Logger info: "app started with " , sup count asString , " children"]
  ifError: [:e   | Logger error: e message]

pool startChild
  ifOk:    [:worker | worker process: job]
  ifError: [:e      | Logger warn: "worker start failed: " , e message]
```

**Idempotent terminate.** `terminate:` / `terminateChild:` naturally express "make sure this child is gone" — no special-casing required.

```beamtalk
// Before ADR 0080 — had to swallow Error because not_found raised
[app terminate: Counter] on: Error do: [:_e | nil]

// After — idempotent: returns Result ok: nil whether fresh terminate or already gone
(app terminate: Counter) unwrap
// real failures still surface as Result error: (beamtalk_error terminate_failed)
(app terminate: Counter) ifError: [:e | Logger warn: e message]
```

See [ADR 0080](ADR/0080-supervisor-lifecycle-result.md) §Migration Path for the full mechanical rewrite guide and common gotchas (chained sends on the return value, type-narrowing in tests, REPL display changes).

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

### Actor Named Registration (ADR 0079)

Actors can be registered under a Symbol name so they can be looked up without passing a reference around, and so supervised restarts stay addressable. Named lookups are **class-checked** — `Counter named: #counter` only returns the registered process if it is a `Counter` (or subclass).

```beamtalk
// Atomic spawn + register — prefer this when the name is known up front
c := (Counter spawnAs: #counter) unwrap
c := (Counter spawnWith: #{#count => 10} as: #counter) unwrap

// Register an already-spawned actor (not atomic w.r.t. spawn)
(c registerAs: #counter) onSuccess: [:c | c increment]

// Typed lookup — Result(Self, Error), so `Counter named:` narrows to Counter
engine := (WorkflowEngine named: #engine) unwrap
(Logger named: #counter)   // => Result error: (beamtalk_error wrong_class)

// Instance queries
c registeredName    // => #counter  (or nil if unnamed)
c isRegistered      // => true

// Idempotent release
c unregister        // => #ok

// Discover all currently-registered Beamtalk actors
Actor allRegistered       // => #(an Actor(Counter), an Actor(Logger))
```

**Restart survival.** When a named actor is started under a supervisor (via `SupervisionSpec withName:`), the runtime dispatches sends through the registered name, not the snapshot pid. Held `Counter named: #counter` references continue to work after a supervisor restart because the name is re-registered atomically in the restarted process's `gen_server:start_link({local, Name}, ...)` call.

Errors are surfaced as `Result` values (or raised as `#beamtalk_error{}` on direct send):

| Kind | When |
|------|------|
| `name_registered` | another process is already registered under the name |
| `name_not_registered` | `named:` lookup found nothing |
| `wrong_class` | `named:` lookup found a process of a different class |
| `reserved_name` | name is in the OTP kernel / stdlib blocklist |
| `no_such_process` | send through a `{registered, Name}` proxy whose name has vanished |

See [ADR 0079](ADR/0079-named-actor-registration.md) for the full design and exposure table.

### Introspecting the Live Supervision Tree (ADR 0092)

Where supervision *syntax* declares a tree, **`Workspace processes`** lets you
walk the *live* one — the dynamic counterpart to `Workspace actors`, and the
process-structure twin of `SystemNavigation`. It returns a navigable
`SupervisionTree` snapshot of the running OTP supervision tree, built as a thin
wrapper over `supervisor:which_children` (no new bookkeeping process).

```beamtalk
tree := Workspace processes        // == ProcessNavigation default tree
tree root                          // => the snapshot root SupervisionNode
tree size                          // => total node count
tree do: [:node | Transcript showLine: node printString]
tree select: [:node | node isSupervisor]
tree findClass: Counter            // => every running Counter as SupervisionNodes
tree nodesOfKind: #beamtalkActor   // => List(SupervisionNode)
```

Each `SupervisionNode` is an immutable record:

```beamtalk
node pid               // => a Pid | nil   (nil for a child mid-restart)
node registeredName    // => Symbol | nil
node kind              // => #beamtalkSupervisor | #beamtalkActor
                       //    | #otpSupervisor | #otpProcess | #restarting
node behaviourClass    // => Class | nil   (nil for foreign OTP processes)
node childCount        // => Integer       (live children; supervisors only)
node strategy          // => Symbol | nil  (#oneForOne … ; supervisors only)
node restartIntensity  // => Dictionary | nil  (configured #{#maxRestarts, #window})
node children          // => List(SupervisionNode)
node parent            // => SupervisionNode | nil
node isSupervisor      // => Boolean
node isBeamtalk        // => Boolean
node status            // => Dictionary | nil   (LAZY — see below)
```

The `kind` field drives rendering: a Beamtalk class badge for `#beamtalkActor` /
`#beamtalkSupervisor` (with `behaviourClass` populated), a foreign-process badge
for `#otpSupervisor` / `#otpProcess`. A child OTP is currently restarting carries
`kind => #restarting` and `pid => nil` — the snapshot never crashes on a process
caught mid-restart.

**Snapshot semantics.** Construction freezes the tree once; iterating it never
re-enters OTP, so a walk is internally consistent and can never deadlock or block
on a busy process. Construction itself is *not* atomic, so the snapshot is a
best-effort point-in-time view — re-call `Workspace processes` to refresh. A node
whose pid has since died is detected lazily: `node status` returns `nil` rather
than raising.

**Lazy state.** `node status` is *not* captured at snapshot time. Calling it
issues a timeout-guarded `sys:get_status` against the node's pid *then* —
returning a `Dictionary` for an alive, `sys`-compliant process, or `nil` for one
that is dead, timed out, or not `sys`-compliant.

**Scopes and scale.** `ProcessNavigation default` (the `Workspace processes`
alias) filters runtime plumbing; `ProcessNavigation system` shows everything,
including runtime internals — a privileged view (ADR 0091). A `from:` constructor
roots a walk at a `Supervisor` handle or a `Pid`, returning a `Result` (the root
may be dead). A `simple_one_for_one` `DynamicSupervisor` with more children than
the cap is reported `truncated` with its `childCount` instead of materialising
every child; opt into full expansion with `ProcessNavigation from: aSup limit: n`.

```beamtalk
ProcessNavigation system tree size                 // everything, incl. infra
(ProcessNavigation from: aSup) unwrap tree         // a rooted subtree
(ProcessNavigation from: aStoppedSup)              // => Result error: (beamtalk_error stale_handle)
```

See [ADR 0092](ADR/0092-supervision-tree-introspection.md) for the full design.

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
  class strategy -> #oneForOne | #oneForAll | #restForOne => #restForOne
  class children -> List(SupervisionSpec) =>
    storeSpec := EventStore supervisionSpec withRestart: #permanent
    poolSpec := ActivityWorkerPool supervisionSpec withRestart: #permanent
    engineSpec := WorkflowEngine supervisionSpec withRestart: #permanent
    #(storeSpec, poolSpec, engineSpec)

  // Re-runs after every restart to rebuild cached pids.
  class initialize: sup :: Supervisor -> Nil =>
    store := (sup which: EventStore) unwrap
    pool := (sup which: ActivityWorkerPool) unwrap
    engine := (sup which: WorkflowEngine) unwrap
    engine initWithStore: store pool: pool
    nil
```

#### After named registration

Naming each child eliminates the `initialize:` hook, and the supervisor strategy is freed from the rewire-on-restart constraint:

```beamtalk
typed Supervisor subclass: ExduraSupervisor
  class strategy -> #oneForOne | #oneForAll | #restForOne => #oneForOne
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

// Nil pattern — matches nil exactly (BT-2854, ADR 0107)
value := nil
value match: [
  nil -> "was nil";
  s :: String -> s;
  _ -> "other"
]
// => "was nil"

// Type patterns — bind and test runtime class (BT-2855, ADR 0107)
x := "hello"
x match: [
  nil -> "nil";
  s :: String -> s size;
  n :: Integer -> n + 1;
  _ -> "other"
]
// => 5

// Guard scoped over the type-pattern binding
x := 42
x match: [
  n :: Integer when: [n > 100] -> "big";
  n :: Integer -> "small";
  _ -> "other"
]
// => "small"

// Mixing type patterns with literal and wildcard patterns
x := "hi"
x match: [
  nil -> 0;
  s :: String -> s size;
  _ -> -1
]
// => 2
```

**Type patterns** test the runtime class of the scrutinee and bind the value to the named variable, narrowed to that class. Subsequent arms see the scrutinee type narrowed by difference (e.g. after a `s :: String` arm, the remaining arms see the type minus `String`).

**Phase A scope (ADR 0107):** type patterns are restricted to concrete/leaf classes only — `binding :: SomeClass` where `SomeClass` has subclasses is a compile error (`"SomeClass has subclasses; type patterns are not yet supported for non-leaf classes"`), never silently-wrong matching. Subclass-polymorphic matching (`binding :: Shape` where `Shape` has subclasses `Circle`/`Square`) is deferred to a future ADR 0107 Phase B. Supported classes today: `String`, `Integer`, `Float`, `List`, `Dictionary`, `Boolean` (uses the `is_boolean/1` BIF; `True`/`False` use exact atom guards — all three are stdlib primitives that bypass the leaf-restriction via a BIF/atom-guard check, not the user-class hierarchy check a `Shape`/`Circle`-style class would go through), `True`, `False`, `Symbol`, `Nil`, `UndefinedObject`, `Block`, `Pid`, `Reference`, `Port`, user-defined `Value` subclasses, `Actor` subclasses, and `Supervisor`/`DynamicSupervisor` subclasses (BT-2870).

Four runtime-representation nuances (see ADR 0107 Implementation for the full codegen rationale):
- **`Supervisor`/`DynamicSupervisor` subclasses use a distinct runtime shape.** A live supervisor reference is a 4-tuple tagged `'beamtalk_supervisor'` (not `'beamtalk_object'` like Actor subclasses, and not a tagged map like Value subclasses). `x :: MySupervisorSubclass` compiles to a tuple-tag check accepting `'beamtalk_supervisor'` at `element(1)` before comparing `element(2)` against the class name (BT-2870).
- **`Character` is not a supported type pattern.** It compiles to a plain Erlang integer with no runtime tag distinguishing it from `Integer`, so `x :: Character` could never be told apart from `x :: Integer` at runtime — use `x :: Integer` instead.
- **`Dictionary` has no `'$beamtalk_class'` tag** — it's a bare Erlang map, unlike tagged `Value`/sealed-class instances (which are also maps, but with a class tag). `x :: Dictionary` is fully supported, but compiles to a different check shape under the hood (a nested map-key test) so it doesn't false-positive on every other map-backed value.
- **A `Symbol` type pattern excludes `nil` and booleans.** `nil`, `true`/`false`, and symbols are all plain Erlang atoms with no distinguishing runtime tag, so `nil match: [s :: Symbol -> ...]` does **not** match, and a `Symbol` arm can never accidentally shadow a `nil ->` or `b :: Boolean` arm regardless of arm order.
- **Bare `true`/`false` are not valid patterns.** Unlike `nil`, `true`/`false` are not reserved pattern keywords — a bare `true ->` arm would otherwise silently parse as a variable binding (`Pattern::Variable`) that matches *any* value, not a boolean-literal test, and — tried first in arm order — would always win. This is rejected with a compile-time diagnostic (BT-2883); use `x :: True` / `x :: False` (exact literal) or `x :: Boolean` (either) instead.
- **Constructor pattern keyword bindings (`Result ok: v`) apply the same `nil`/`true`/`false` rules, but the `true`/`false` fix-it differs.** `Result ok: nil` tests the wrapped value is nil (`Pattern::Nil`), same as top-level `nil`. `Result ok: true`/`Result ok: false` are rejected with a compile-time diagnostic (BT-2884), same as top-level bare `true`/`false` — but since `x :: True`/`x :: False` type patterns aren't supported nested inside a constructor pattern, the diagnostic instead points at binding a variable and testing it in a guard clause: `Result ok: v when: [v =:= true]` / `Result ok: v when: [v =:= false]`.

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
| Nil | `nil` | Matches `nil` exactly; narrows subsequent arms to exclude `Nil` (BT-2854, ADR 0107) |
| Type | `x :: String` | Bind `x` and test runtime class; `x` is narrowed to the named class in the arm body and guard. Phase A: leaf/concrete classes only — see restrictions below (BT-2855, ADR 0107) |

**Exhaustiveness checking (BT-1299):** `match:` on a sealed type with constructor patterns must cover all known variants or include a wildcard `_` arm, or the compiler emits an error:

```beamtalk
// Compile error: missing error: arm
r match: [Result ok: v -> v + 1]

// Fine: all variants covered
r match: [Result ok: v -> v + 1; Result error: _ -> 0]

// Fine: wildcard suppresses the check
r match: [Result ok: v -> v + 1; _ -> 0]
```

**Advisory singleton-union exhaustiveness (BT-2745, ADR 0102):** when the type checker knows a `match:` scrutinee is a closed union of `#symbol` singletons, it emits a **warning** (never an error) for any uncovered members:

```beamtalk
// direction :: #north | #south | #east | #west
direction match: [
  #north -> 0;
  #south -> 180;
  #east  -> 90
]
// ⚠ Warning: non-exhaustive match: `#west` is not handled (residual type: `#west`)
```

This check is advisory — it fires only when the scrutinee type is a union of pure `#symbol` singletons (not `Dynamic`, open `Symbol`, or mixed unions). An unguarded `_ ->` wildcard silences the warning; guarded arms do not count as coverage.

**Asserted exhaustiveness — `matchExhaustive:` (BT-2763, ADR 0106):** `matchExhaustive:` is an opt-in, stricter variant of `match:` that *asserts* exhaustiveness. It parses identically to `match:` (same patterns, guards, and destructuring), but the check runs at **error** severity instead of warning:

```beamtalk
// direction :: #north | #south | #east | #west

// Compile error: matchExhaustive: proves this is NOT exhaustive
direction matchExhaustive: [
  #north -> 0;
  #south -> 180;
  #east  -> 90
]
// ⛔ Error: non-exhaustive matchExhaustive: `#west` is not handled (residual type: `#west`)

// Fine: all four members covered — silent, no diagnostic
direction matchExhaustive: [
  #north -> 0;
  #south -> 180;
  #east  -> 90;
  #west  -> 270
]

// Fine: an unguarded wildcard is still full coverage
direction matchExhaustive: [
  #north -> 0;
  _      -> -1
]
```

If the scrutinee's type is **not** a closed union of `#symbol` singletons (`Dynamic`, a bare/open `Symbol`, or a mixed union), nor a closed `Known | Nil` union (or small closed union of concrete leaf classes) covered by `nil`/`Type` patterns (ADR 0107), `matchExhaustive:` cannot verify the assertion and fails loudly rather than staying silent:

```beamtalk
x matchExhaustive: [#ok -> 1; _ -> 0]
// ⛔ Error: cannot verify `matchExhaustive:` is exhaustive — scrutinee type
//    `Dynamic` is not a closed union of symbol singletons, `nil`, or
//    concrete leaf classes
```

Plain `match:`'s advisory warning behaviour is unchanged by `matchExhaustive:` — the two checks are independent, and only the keyword you write selects between them.

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

Live patching works on the **class side** too (ADR 0084): `ClassName class >> sel
=> body` installs or replaces a class method on a registered class, and class-side
dispatch resolves the new method immediately.

```beamtalk
Object subclass: Registry
  class current => nil

// Live-edit the class method — subsequent class-side sends pick it up
Registry class >> current => "live"
Registry current        // => "live"

// Add a brand-new class-side selector
Registry class >> reset => 0
Registry reset          // => 0
```

> The `>>` live-edit path recompiles the class's recorded source, so it applies
> to classes defined in source (inline `Object subclass:` or `:load`ed files).
> Purely-programmatic `ClassBuilder` classes have no recorded source; supply their
> class methods up front via `classMethods:` / `addClassMethod:body:` instead.

### Saving live edits back to disk — `compile:source:`, ChangeLog, and `flush` (ADR 0082)

Live patches go into memory; they reach the `.bt` source file only when you
**flush**. Between the patch and the flush, every in-memory mutation is recorded
in the workspace **ChangeLog** — the pending-changes view, dirty-state tracker,
and undo store rolled into one ([ADR 0082](ADR/0082-method-level-edit-save-and-changelog.md)).

The model has three layers: in-memory class state (hot-reloaded BEAM), the
ChangeLog (per-workspace append-only log; persists across workspace restart),
and the `.bt` files on disk. Every successful live patch updates memory **and**
appends a ChangeEntry. `Workspace flush` walks pending entries and splices each
patched body back into its source file via byte-span replacement — no AST
re-print, no whole-file reformat — atomically (`<file>.tmp` + rename) with
external-edit conflict detection.

#### The patcher primitives

| Method | Intent | Logs? | Used by |
|--------|--------|-------|---------|
| `aClass >> sel => body` (parser sugar) | durable | yes | humans at the REPL |
| `aClass compile: #sel source: "body"` | durable | yes | MCP `save_method`, browser "Save", REPL editor |
| `aClass tryCompile: #sel source: "body"` | ephemeral (auto-prunes) | yes | MCP `try_method`, agent spikes |
| `Workspace newClass: source at: path` | durable, `kind: #'new-class'` | yes | MCP `save_class`, browser "New File" |

`>>` and `compile:source:` are equivalent in effect — both install the new
method and append a durable ChangeEntry. The keyword form takes the body as a
**String value** so tools (MCP, LSP, browser editors) don't have to escape
quotes or multi-line bodies back into source. `tryCompile:source:` installs in
memory like `compile:source:` but tags the entry as ephemeral — successful
spikes are promoted by re-calling `compile:source:` with the same body. Every
successful in-memory mutation logs unconditionally, including spikes and
patches against stdlib / dependency classes (which are not flushable). The
audit trail is exhaustive on purpose.

#### Canonical patch → changes → flush round trip

```beamtalk
> Counter >> increment => self.value := self.value + 1
=> Counter                          // memory patched
> Workspace changes notEmpty
=> true
> Workspace changes dirtyMethods
=> #{#Counter => #{#increment}}     // per-class set of dirty selectors
> Workspace flush
=> _                                // FlushResult; quiet on success
> Workspace changes isEmpty
=> true                             // flushed entries drop out of the active view
```

`Workspace changes` returns a [`ChangeLog`](#changelog) object (see below).
`Workspace flush` returns a `FlushResult` summary with `#flushed`, `#files`,
`#newClasses`, and `#conflicts`. A non-empty `#conflicts` list means the listed
entries remain pending and require manual reconciliation.

#### Targeted flush

```beamtalk
Workspace flush                                   // every durable + flushable entry
Workspace flush: Counter                          // entries targeting one class
Workspace flush: #'new-class'                     // entries of one kind
Workspace flush: #{ #file => "src/counter.bt" }   // entries against one file
Workspace changes flushKinds: #{#agent}           // only agent-authored entries
Workspace changes flushKinds: #{#agent, #'new-class'}  // both filters AND together
```

#### External-edit conflict — patch, edit on disk, flush

```beamtalk
> Counter >> increment => self.value := self.value + 2   // memory patched
=> Counter
// ... another editor (or `git pull`) modifies examples/counter.bt on disk ...
> Workspace flush
=> _   // FlushResult with #conflicts: [#{#file => "examples/counter.bt",
       //                                  #reason => #external_edit, ...}]
       // The patch stays pending; memory is still ahead of disk.
```

When flush detects an external edit, the offending entries stay in the log and
the user picks the recovery path:

```beamtalk
Workspace changes clear                           // drop the pending ChangeLog entries
                                                  //   (already-installed patches stay in memory
                                                  //    until workspace restart — use `revert:` to
                                                  //    actually re-install the prior method body)
Workspace changes revert: anEntry                 // undo one patch (re-install prior body)
// or open the file, reconcile by hand, then:
Workspace flush                                   // retry once disk matches expectations
```

#### Ephemeral spike → promote → flush

```beamtalk
> Counter tryCompile: #doubled source: "doubled => self.value * 2"
=> Counter                          // memory patched, ChangeEntry logged as ephemeral
> (Counter spawn) doubled
=> 0                                // works — agent decides to keep it
> Counter compile: #doubled source: "doubled => self.value * 2"
=> Counter                          // promoted: durable ChangeEntry layered on top
> Workspace flush
=> _                                // disk gains the new method
```

The earlier ephemeral entry remains in the log for audit and is auto-pruned on
the next workspace restart.

#### Creating a brand-new class file

```beamtalk
> Workspace newClass: "Object subclass: Greeter\n  greet => 'hello'" at: "src/greeter.bt"
=> [Greeter]                        // compiled and installed in memory
> Workspace flush
=> _                                // writes src/greeter.bt
```

`newClass:at:` raises a loud, specific error (no silent fallback) if `path`
already exists, lies outside the project tree, the declared class name does
not match the path basename (ADR 0040 one-class-per-file convention), or a
class of that name is already loaded.

#### `autoflush`

For users who want write-through editor semantics, flip a single workspace
setting:

```beamtalk
Workspace autoflush       // => false  (default)
Workspace autoflush: true // => true   (every successful durable patch flushes immediately)
```

Autoflush persists across workspace restarts. It is **best-effort**, not
transactional — a flush failure under autoflush (external-edit conflict, write
error) leaves memory ahead of disk and the entry pending in the log. The BEAM
module install is not rolled back because live actors may hold references to
the new closures. The error surfaces with a "memory ahead of disk" warning.

Ephemeral patches via `tryCompile:source:` are never autoflushed.

#### Flushability — what `flush` writes

A class is **flushable** iff its `sourceFile` is non-nil **and** lies inside the
current project's source tree. `Workspace flush` writes only entries where
`intent = durable AND flushable = true`. Other entries are reported under
`#conflicts` (for external-edit / target-exists errors) or simply skipped:

- **Stdlib classes** (`Integer`, `String`, ...) — `sourceFile = nil`. Patches
  install in memory and log with `flushable: false`; flush skips them. Smalltalk
  muscle memory (`Integer compile: #double source: "..."`) is supported as live,
  audited, non-flushable drift.
- **Dependency classes** — `sourceFile` outside the project tree. Same shape.
  Flush never writes into the dependency cache (reproducible-build guarantee).
- **Dynamic classes** ([ADR 0038](ADR/0038-subclass-classbuilder-protocol.md)
  `ClassBuilder`) — `sourceFile = nil`. Same shape.

#### ChangeLog

`Workspace changes` returns a `ChangeLog` object (analogous to Pharo's
`Smalltalk changes`). All pending-state queries live on this object, not on the
`Workspace` facade itself.

| Method | Returns | Description |
|--------|---------|-------------|
| `size` | `Integer` | Active (live, re-appliable) entries |
| `isEmpty` / `notEmpty` | `Boolean` | "Is anything dirty?" is `Workspace changes notEmpty` |
| `do: block` | `Nil` | Iterate active entries |
| `select: block` | `List` | Filter **all** entries (reaches orphans, prior-epoch, and shadowed entries too) |
| `dirtyMethods` | `Dictionary` | `#{Class => Set(selectors)}` for the active set |
| `revert: anEntry` | class | Re-install `prev_source` for that entry (itself a durable patch) |
| `clear` | `ChangeLog` | Discard every pending entry without writing to disk (memory keeps the patches until restart) |
| `flushKinds: kinds` | `FlushResult` | Flush only entries matching a Set of `#instance` / `#class` / `#'new-class'` / `#human` / `#agent` symbols (both dimensions AND together) |
| `allEntries` | `List(ChangeEntry)` | Every logged entry, including prior-epoch, orphan, shadowed, and clean entries |
| `activeEntries` | `List(ChangeEntry)` | The default view: current-epoch, non-orphaned entries, collapsed to the latest entry per `(class, selector)` and filtered to those still differing from disk — one row per method that has a net change |

Each `ChangeEntry` carries the patch's body, prior body, byte span, class,
selector, intent (`durable` / `ephemeral`), flushable flag, `authorKind`
(`#human` / `#agent`), and source-file reference. Bodies are stored as plain
`.bt` files under `<workspace>/changes/sources/`; metadata lives in
`<workspace>/changes/changes.jsonl`. `cat`, `less`, `diff`, and `bt fmt` all
work on the source files directly.

Repeated patches to one method — or a patch followed by a `revert:`, which is
itself a patch (ADR 0082 "Undo") — append multiple entries for the same
`(class, selector)`. The default view keeps only the latest (the one
`Workspace flush` would apply) and marks the rest *shadowed*; `e isShadowed`
identifies them, and `select:` still reaches them for audit.

The latest entry is also compared against the current on-disk body: if it
matches (the method was reverted back to its on-disk state) the entry is *clean*
(`e isClean`) and drops out of the default view — there is no net change to
flush. Each entry that *does* differ carries `e diff`, the net on-disk→in-memory
unified diff (lines prefixed `  ` / `- ` / `+ `). So `Workspace changes` answers
"what differs from disk", not "everything I touched this session"; the audit
trail of every entry stays in `allEntries` / `select:`.

The ChangeLog persists across workspace restart. On restart, the workspace
assigns a fresh `epoch` and excludes prior-epoch entries from the active view
(their memory state is gone). The underlying audit log keeps them; reach them
via `Workspace changes select: [:e | e isOrphan]`.

#### REPL and tooling shortcuts

Every operation above is reachable via the REPL meta-commands, MCP tools, LSP
`executeCommand` handlers, and browser actions. These are all thin front-ends
over the Beamtalk language — see [REPL shortcuts](#repl-shortcuts--commands-are-thin-wrappers)
below and the [Tooling guide](beamtalk-tooling.md#changelog-and-flush-adr-0082)
for the surface tables.

### Live Re-Checking on Reload (ADR 0105)

A live edit doesn't just change the class being patched — it can invalidate
every existing caller in the image. Every save above (`>>`, class-body
redefinition, `:load`) triggers an **incremental re-check of known
dependents**: the compiler re-checks the callers `beamtalk_xref` (ADR 0087)
already knows about, using the same type checker that runs at compile time,
and publishes what it finds as live diagnostics — no rebuild, no manual
"find senders" required.

```beamtalk
:load counter.bt
counter := Counter spawn
counter getCount + 1          // => 1

// ... change `getCount -> Integer` to `getCount -> String`, save
// (a plain `Counter >> getCount -> String => ...` live patch) ...

⚠ reload check: Counter>>getCount signature changed;
   2 callers re-checked, 1 stale
   Dashboard>>refresh (dashboard.bt:14): `+` expects a number, `getCount` now returns String
```

Only genuinely-affected callers surface. If `StatsView>>render` also calls
`getCount` but only stringifies the result, it re-checks *clean* against the
new signature and stays silent — the header's "2 re-checked, 1 stale" is its
only trace.

A **removed** selector is a `does_not_understand` waiting to happen, reported
at `Hint` severity (ADR 0100 Rule 1 — a single closed-complete receiver):

```beamtalk
ℹ reload check: Counter>>reset was removed; 1 caller remains
   AdminPanel>>onClick (admin.bt:9): `counter reset` will raise
   does_not_understand at runtime
   (Hint severity per ADR 0100 Rule 1 — single closed receiver)
```

A `state:`/`field:` slot added, removed, or retyped re-checks `spawnWith:`
call sites and the changed slots' generated accessors the same way, under a
`shape_change` classification.

**Advisory, never blocking.** The reload already happened — a finding informs,
it never vetoes. Findings are workspace-session state (LSP diagnostics,
REPL/workspace-UI notices), never persisted, and never fail a build; they
disappear on workspace restart. **Clearing is by replacement**: every
re-check of a caller replaces *all* of its findings attributed to that
changed class with the fresh result — clean or different — so back-to-back
reloads of the same method never leave a stale finding sitting alongside a
current one (supersession), and a later reload that fixes the callee clears
the caller's finding with no edit to the caller at all.

Two related, on-demand operations round out the surface:

- **Pre-save advisory** — `aClass precheckCompile: #selector source: "..."`
  compiles a *pending* edit and reports would-be-stale dependents without
  installing it, so an editor can warn before you save. Never touches the
  live image, the ChangeLog, or the findings store.
- **`Workspace recheckImage`** (also `:recheck image`) — the "complete but
  unbounded" whole-image sweep kept out of the automatic per-reload trigger:
  re-checks every live class the workspace has a recorded source for and
  returns a `checked`/`stale`/`findings` report, on demand.

```beamtalk
(Counter precheckCompile: #getCount source: "getCount -> String => self.value printString")
// => a Dictionary shaped like the reload_check report — findings without installing

Workspace recheckImage
// => _  (checked/stale summary across the whole live image)
```

The dependent lookup is selector-keyed (ADR 0087's xref schema has no
receiver-class component), so it re-checks every candidate caller and lets
the checker's own type inference decide relevance — a `size` sender on an
unrelated class simply re-checks clean. Fan-out is capped per reload (with a
"N more not checked" note); one level of fan-out only, not transitive; and
proxy-routed calls (ADR 0104 §4 forwarding) are invisible to xref, so a
proxy-wrapped caller can go unflagged — see
[ADR 0105](ADR/0105-live-image-recheck-on-reload.md) for the full mechanism,
severity rules, and accepted gaps.

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

### Cross-file extensions

Extensions can target classes defined in other files or in stdlib. The compiler
registers each foreign extension at module load, so it dispatches at runtime just
like a same-file extension. Class-side extensions register under the metaclass tag
(`String class`).

```beamtalk
// In helpers.bt — String is defined in stdlib, not this file
String >> shoutIt => super printString uppercase ++ "!"

// Class-side foreign extension
String class >> banner => "=== String ==="
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
| `allClasses` | `List` | All registered classes (class objects) |
| `classNamed: #Name` | `Object` or `nil` | Look up a class by name |
| `globals` | `Dictionary` | Snapshot of system namespace (class names → class objects) |
| `help: aClass` | `String` | Class documentation: name, superclass, method signatures |
| `help: aClass selector: #sel` | `String` | Documentation for a specific method |

```beamtalk
Beamtalk version
// => "0.4.0"

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
| `newClass: source at: path` | `List(Behaviour)` | Create a brand-new class from source at `path`; logs a `kind: #'new-class'` ChangeEntry (ADR 0082) |
| `classes` | `List` | All loaded user classes (those with a recorded source file) |
| `testClasses` | `List` | Loaded classes that inherit from `TestCase` |
| `globals` | `BindingsView` | Live, write-through view of the workspace-globals layer: singletons + `bind:as:` entries (see [Sessions and binding layers](#sessions-and-binding-layers-adr-0081)) |
| `currentSession` | `Session` or `nil` | The calling process's REPL session (same value as `Session current`); `nil` outside a REPL eval |
| `sessions` | `List(Session)` | All live REPL sessions as `Session` values |
| `test` | `TestResult` | Run all loaded test classes |
| `test: AClass` | `TestResult` | Run a specific test class |
| `actors` | `List` | All live actors as object references |
| `actorAt: pidStr` | `Object` or `nil` | Look up a live actor by pid string |
| `actorsOf: AClass` | `List` | All live actors of the given class |
| `bind: value as: #Name` | `Nil` | Register a value in the workspace namespace |
| `unbind: #Name` | `Nil` | Remove a registered name from the namespace |
| `changes` | `ChangeLog` | Pending in-memory changes (ADR 0082) — see [Saving live edits back to disk](#saving-live-edits-back-to-disk--compilesource-changelog-and-flush-adr-0082) |
| `flush` | `FlushResult` | Write every durable + flushable ChangeEntry back to its source file (ADR 0082) |
| `flush: filter` | `FlushResult` | Flush a subset (Class / Symbol kind / `#{#file => path}`) |
| `autoflush` | `Boolean` | Workspace setting (default `false`); persists across restarts |
| `autoflush: enabled` | `Boolean` | Toggle write-through: every durable patch immediately flushes (best-effort) |

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

### `SystemNavigation` — Cross-class code queries

`SystemNavigation` provides Smalltalk-style live-image queries over the loaded
class registry. Reach the singleton via `SystemNavigation default`.

| Method | Returns | Description |
|--------|---------|-------------|
| `allClasses` | `List(Behaviour)` | All registered classes (class objects) |
| `actorClasses` | `List(Behaviour)` | Classes whose superclass chain includes `Actor`, sorted alphabetically |
| `dnuHandlers` | `List(Behaviour)` | Classes that locally override `doesNotUnderstand:args:`, sorted alphabetically |
| `extendersOf: aClass` | `List(Package)` | Packages contributing extension methods to `aClass` |
| `extensionsBy: aPackage` | `List(Dictionary)` | `#{#class, #selector}` for each extension method `aPackage` contributes |
| `implementorsOf: #sel` | `List(Behaviour)` | Classes that define the given selector |
| `sendersOf: #sel` | `List(Dictionary)` | `#{#class, #selector, #line}` for every method body that sends `#sel` |
| `messagesSentBy: aMethod` | `List(Dictionary)` | `#{#selector, #line}` for every message send in `aMethod`'s body — the outgoing-call dual of `sendersOf:`. `aMethod` must be a `CompiledMethod` (e.g. `Counter >> #increment`). Excludes Erlang FFI sends |
| `referencesTo: aClass` | `List(Dictionary)` | `#{#class, #selector, #line}` for every method body that references the class name |
| `announcementsSentBy: aClass` | `List(Behaviour)` | Distinct `Announcement` subclasses that `aClass` statically emits via `announce:` / `announceAndWait:` / `announceAndWait:timeout:`, sorted by name — the publisher-side dual of `AnnouncementNavigation`. Advisory: only constructor-call arguments (`announce: (PriceChanged newPrice: 42)`) resolve; `Dynamic`/indirect arguments are skipped |
| `announcementSitesSentBy: aClass` | `List(Dictionary)` | `#{#class, #selector, #line, #announcementClass}` for every resolvable announcement emission within `aClass` — the site-level form of `announcementsSentBy:` |
| `ffiSitesFor: aSpec` | `List(Dictionary)` | `#{#class, #selector, #line}` for every method body that calls Erlang `module:function` (optionally arity-qualified, e.g. `"lists:reverse/1"`) |
| `fieldReadersOf: #slot in: aClass` | `List(Dictionary)` | `#{#class, #selector, #line}` for every method that reads field/class var `#slot` while scanning `aClass` + subclasses on instance and class sides |
| `fieldWritersOf: #slot in: aClass` | `List(Dictionary)` | `#{#class, #selector, #line}` for every method that writes field/class var `#slot` while scanning `aClass` + subclasses on instance and class sides |
| `methodsMatching: aRegex` | `List(Dictionary)` | `#{#class, #selector}` for every method whose source matches the regex |
| `selectorsMatching: pattern` | `List(Symbol)` | Selectors matching a case-insensitive substring (e.g., `"print"`) |
| `selectorsForClass: aClass` | `List(Symbol)` | All selectors defined on a class (instance + class + extension) |
| `classesInPackage: aPackage` | `List(Behaviour)` | Class objects belonging to package `aPackage` (Symbol or String; ADR 0070) |
| `subclassesOf: aClass in: aPackage` | `List(Behaviour)` | Subclasses of `aClass` that live in package `aPackage` (`allSubclasses` filtered by package) |
| `unimplementedSelectors` | `List(Dictionary)` | Selectors sent but defined nowhere — a typo-finder lint |
| `unusedSelectors` | `List(Dictionary)` | Selectors defined but sent nowhere — dead-method candidates |

Body-based queries (`sendersOf:`, `referencesTo:`, `ffiSitesFor:`,
`methodsMatching:`, `announcementsSentBy:`, `announcementSitesSentBy:`, and the
selector-lint queries) scan instance-side, class-side, and extension method
bodies. `fieldReadersOf:in:` and
`fieldWritersOf:in:` scan `aClass` + subclasses on instance/class sides (not
extension methods). Each result's `#class` field is the class object for an
instance-side hit and the metaclass object (`Counter class`) for a class-side
hit.

`announcementsSentBy:` is a deliberately *advisory* static analysis of a dynamic
language — the publisher-side dual of `AnnouncementNavigation`'s runtime
subscription queries. It resolves an `announce:` argument to its `Announcement`
subclass only when the argument is a constructor call on a bare class reference
(`announce: (PriceChanged newPrice: 42)` or `announce: PriceChanged new`). A
`Dynamic`-typed or indirect argument (`announce: someVar`, `perform:`-style
dispatch) is unresolvable by construction and is silently skipped, so the result
is discoverability, never a sound or exhaustive emission contract.

```beamtalk
nav := SystemNavigation default

nav implementorsOf: #printString
// => [Object, Integer, String, ...]

nav sendersOf: #increment
// => [#{#class => CounterTest, #selector => #testIncrement, #line => 12}, ...]

nav referencesTo: Counter
// => [#{#class => CounterTest, #selector => #setUp, #line => 5}, ...]

nav methodsMatching: (Regex from: "printString") unwrap
// => [#{#class => Object, #selector => #printString}, ...]

nav selectorsMatching: "print"
// => [#printString, #printOn:, ...]

nav messagesSentBy: (Counter >> #increment)
// => [#{#selector => #+, #line => 2}, ...]

nav announcementsSentBy: PriceTracker
// => [PriceChanged, ...]  (distinct Announcement subclasses PriceTracker emits)

nav announcementSitesSentBy: PriceTracker
// => [#{#class => PriceTracker, #selector => #notify, #line => 4, #announcementClass => PriceChanged}, ...]

nav unimplementedSelectors
// => []  (empty = no typos in the loaded registry)

nav unusedSelectors
// => [#{#class => MyLib, #selector => #helperNoOneCalls}, ...]

nav actorClasses
// => [Actor, ClassBuilder, MyActor, ...]

nav dnuHandlers
// => [ErlangModule, ProtoObject, TimeoutProxy, ...]

nav extendersOf: String
// => [Package(my_lib v1.0.0), ...]

nav extensionsBy: (Package named: "my_lib")
// => [#{#class => String, #selector => #asJson}, ...]

nav classesInPackage: #stdlib
// => [Actor, Array, ...]

nav subclassesOf: Number in: #stdlib
// => [Float, Integer]

nav fieldReadersOf: #value in: Counter
// => [#{#class => Counter, #selector => #getValue, #line => 5}, ...]

nav fieldWritersOf: #value in: Counter
// => [#{#class => Counter, #selector => #increment, #line => 3}, ...]

nav ffiSitesFor: "lists:reverse"
// => [#{#class => MyList, #selector => #reversed, #line => 7}, ...]
```

### Sessions and binding layers (ADR 0081)

The REPL resolves a bare name (`x`, `Transcript`, `Counter`) against **two
binding layers**, each owned by a different object:

| Layer | Owner | Source | Accessor |
|-------|-------|--------|----------|
| **Session locals** | the session (per connection) | `x := 42` typed in the shell | `Session current bindings` |
| **Workspace globals** | the workspace (shared) | singletons (`Transcript`, `Beamtalk`, `Workspace`) + `bind:as:` entries | `Workspace globals` |

Locals are checked first, so a local **shadows** a global of the same name.
Names not found in either layer fall through to the class registry (`Counter`,
`Integer`), then raise `undefined_variable`.

#### `Session` — a first-class session value

`Session` is a factory, mirroring `Date today` / `Smalltalk current`: two
class-side methods return a session *value* you then message. There is **no**
class-side operation mirror (no `Session bindings`) and **no** `globals`
accessor on `Session` — globals are workspace state, reached via `Workspace
globals`.

| Class method | Returns | Description |
|--------------|---------|-------------|
| `Session current` | `Session` or `nil` | The calling process's session; `nil` outside a REPL eval (compiled code has no session) |
| `Session withId: anId` | `Session` or `nil` | Look up a session by its protocol id; `nil` if unknown or no longer alive |

| Instance method | Returns | Description |
|-----------------|---------|-------------|
| `bindings` | `BindingsView` | Live view of this session's locals (the `x := 42` layer) |
| `resolve: #name` | `Object` | Resolve a name the way bare-name lookup does (locals → globals → classes). Shares the one resolver with bare-name lookup, so it raises `undefined_variable` for a name that resolves nowhere — exactly as typing the bare name would |
| `clear` | `nil` | Clear this session's locals (globals remain) |
| `id` | `String` | Stable session identifier (matches the protocol session id) |

```beamtalk
x := 42
// => 42

Session current bindings keys
// => #(#x)

Session current bindings at: #x
// => 42

Session current resolve: #Transcript
// => the Transcript singleton

Session current resolve: #notDefinedAnywhere
// => Error: Undefined variable: notDefinedAnywhere

Session current clear
// => nil
```

Outside a REPL eval (e.g. in a `.bt` file run via `beamtalk run`), `Session
current` returns `nil`. Guard with `ifNotNil:` rather than a predicate:

```beamtalk
Session current ifNotNil: [:s | s clear]
```

#### `BindingsView` — a live, write-through Dictionary view

Both `Session current bindings` and `Workspace globals` return a `BindingsView`:
a small Dictionary-protocol value (`at:`, `at:put:`, `removeKey:`,
`includesKey:`, `keys`, `values`, `size`, `do:`) backed by live state. `at:put:`
returns the value put; `removeKey:` returns `nil`.

```beamtalk
// Session-local write — DEFERRED to end of eval, visible on the NEXT line:
Session current bindings at: #y put: 99
// => 99
y
// => 99

// Workspace-global write — SYNCHRONOUS (routes through bind:as:),
// visible immediately on the next line:
Workspace globals at: #answer put: 42
// => 42
answer
// => 42
```

**One documented asymmetry under the shared type:** session-local writes are
deferred to the end of the current eval (the eval worker holds a state
snapshot), so a same-expression read-back sees the *old* value; workspace-global
writes hit shared ETS immediately. Writing a protected system name through the
globals view raises the same conflict as `Workspace bind:as:`:

```beamtalk
Workspace globals at: #Workspace put: nil
// => Error: Workspace is a system name and cannot be shadowed
```

#### Cross-session access (read-only)

`Session withId:` returns another session by id — used by tooling (LSP, VS Code)
to read the user's session from a separate completion session. Cross-session
**reads** are allowed; **writes** raise `cross_session_mutation_unsupported`:

```beamtalk
// Pick a session that is NOT this one — session ordering is not guaranteed, so
// `sessions first` could be the current session, where a write would succeed
// (self-session) rather than raise. Tooling normally already knows the target id.
myId    := Session current id
otherId := (Workspace sessions collect: [:s | s id]) detect: [:each | each /= myId]
other   := Session withId: otherId

other bindings keys          // => cross-session READ, allowed
other bindings at: #x put: 9 // => Error: Cannot mutate another session's bindings
```

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
| `:changes` | `Workspace changes` |
| `:dirty` | `Workspace changes dirtyMethods` |
| `:flush` | `Workspace flush` |
| `:flush Counter` | `Workspace flush: Counter` |
| `:flush #'new-class'` | `Workspace flush: #'new-class'` |
| `:flush #{ #file => "path" }` | `Workspace flush: #{ #file => "path" }` |

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

## Announcements — Typed Events (ADR 0093)

Announcements are Beamtalk's **typed publish/subscribe substrate** — a first-class
Observer pattern. One part of a program (or the runtime itself) says *"X
happened"* by announcing a typed event; other parts react by subscribing to that
event's class. It is Pharo's `Announcements` + `SystemAnnouncer`, adapted to the
BEAM: subscriptions are process-rooted and cleaned up by `monitor`, dispatch runs
caller-side off concurrent ETS reads (no central bottleneck), and crashing
handlers are isolated. See [ADR 0093](ADR/0093-announcements-event-substrate.md)
for the full design.

The substrate lives in the **core image** (stdlib + runtime), not an optional
package, because the *system* publishes through it — so it is always available,
no dependency to add.

### Events — subclass `Announcement`

An **announcement** is an immutable, typed payload describing a fact. Subclass
`Announcement` and add `field:` slots for the event's data. Because
`Announcement` is a `Value`, you get keyword-constructor ergonomics and `field:`
accessors for free:

```beamtalk
Announcement subclass: PriceChanged
  field: newPrice :: Number = nil

event := PriceChanged newPrice: 42
event newPrice    // => 42
event class       // => PriceChanged
```

### Announcer — a per-instance dispatcher

`Announcer new` mints a fresh dispatcher handle (an opaque identity handle, like
`Pid`). Subscribe with `when:do:`, publish with `announce:`:

```beamtalk
a := Announcer new

// Subscribe: returns a Subscription token. The handler block receives the event.
sub := a when: PriceChanged do: [:e | Transcript showLine: "now " ++ e newPrice printString]
sub class       // => Subscription
sub isActive    // => true

// Publish asynchronously (fire-and-forget). Every matching subscriber runs.
a announce: (PriceChanged newPrice: 42)    // prints "now 42"

// Stop listening.
sub unsubscribe
sub isActive    // => false
```

### Subscription protocol

| Message | Meaning |
|---|---|
| `when: aClass do: aBlock` | Evaluate `aBlock` with the event on each announcement of `aClass` (or a subclass). |
| `when: aClass send: sel to: receiver` | Send `sel` to `receiver` with the event as the sole argument. |
| `when: aClass doOnce: aBlock` | Deliver exactly once, then auto-unsubscribe. Consumed atomically under concurrent announcers. |
| `announce: anEvent` | Publish asynchronously — returns immediately, handlers run fire-and-forget. |
| `announceAndWait: anEvent` | Publish synchronously — block until every handler completes (default 5 s timeout). |
| `announceAndWait: anEvent timeout: ms` | Synchronous publish with a custom per-handler timeout in milliseconds. |
| `unsubscribe: receiver` | Remove every subscription `receiver` holds on this announcer. |

Each `when:…` returns a **distinct** `Subscription` — a process may hold several
to the same class, and re-subscribing never silently replaces an earlier one.

### Synchronous vs asynchronous

`announce:` is asynchronous: it returns immediately and each handler runs in its
own transient process, so a slow or crashing handler never blocks the publisher
or its siblings. `announceAndWait:` is synchronous — it waits for every handler,
with per-handler fault isolation and a timeout, so a wedged handler can never
hang the caller:

```beamtalk
a announceAndWait: (PriceChanged newPrice: 99)
// returns only after all handlers have run (or timed out)
```

A crashing handler is logged and isolated — other subscribers still run, and the
announcer is unaffected:

```beamtalk
a when: PriceChanged do: [:e | e boom]   // this handler will crash
a announce: (PriceChanged newPrice: 1)
// other subscribers still run; the crash is logged, not propagated
```

### MRO matching — subscribe to a superclass

Dispatch walks the event's superclass chain at announce time, so subscribing to a
**superclass** receives every subclass event. Delivery is de-duplicated per
subscription:

```beamtalk
Announcement subclass: UIEvent
UIEvent subclass: ButtonClicked
  field: buttonId :: String = ""

a when: UIEvent do: [:e | Transcript showLine: "ui event"]
a announce: (ButtonClicked buttonId: "submit")   // matches — "ui event"
```

### SystemAnnouncer — watch the runtime live

`SystemAnnouncer current` is the singleton bus the **runtime itself** publishes
onto. System facilities announce well-known discrete events; a tool subscribes
once and filters by event class instead of wiring bespoke notification channels:

```beamtalk
SystemAnnouncer current when: ActorSpawned do: [:e |
  Transcript showLine: e actorClass asString
]
Counter spawn    // the subscription fires: prints "Counter"
```

The system event classes (all `Announcement` subclasses):

| Event | Fields | Announced when |
|---|---|---|
| `ActorSpawned` | `actorClass`, `pid` | a Beamtalk actor starts |
| `ActorStopped` | `actorClass`, `pid`, `reason` | an actor stops |
| `ClassLoaded` | `className` | a class is loaded into the image |
| `ClassRemoved` | `className` | a class is removed |
| `BindingChanged` | `name`, `value`, `sessionId` | a workspace variable is assigned |
| `FlushCompleted` | `files` | `Workspace flush` finishes writing source files |
| `ObjectStateChanged` | `pid`, `actorClass`, `changedSlots` | a *watched* actor commits a state write (opt-in via `beamtalk_object_watch`) |
| `SupervisionChildAdded` | (see ADR 0092) | a supervised child is added |
| `SupervisionChildCrashed` | (see ADR 0092) | a supervised child crashes |

`SystemAnnouncer` is **async-only**: `announceAndWait:` raises
`UnsupportedOperation`, because the shared system bus can have many subscribers
and a synchronous gather would be an unbounded process storm under rapid system
events. Use `announce:` for the system bus, or a per-instance `Announcer` when
you need synchronous dispatch.

```beamtalk
[SystemAnnouncer current announceAndWait: anEvent] on: Error do: [:e | e kind]
// => unsupported_operation
```

### Introspection — the third navigation sibling

The bus is **navigable**, alongside `SystemNavigation` (static classes) and
`ProcessNavigation` (the live supervision tree). There are two levels.

**Object-knows-itself** — a live `Announcer` inspects its own subscriptions:

```beamtalk
a subscriptions          // => a List of SubscriptionNode snapshots
a subscribersOf: PriceChanged   // => subscriptions to exactly PriceChanged
a subscriptionCount      // => total live subscriptions on the bus
```

**Navigator-discovers-system** — `AnnouncementNavigation` queries the graph:

```beamtalk
AnnouncementNavigation default subscribersOf: ActorSpawned
AnnouncementNavigation default announcedClasses    // => distinct event types in use
AnnouncementNavigation of: anAnnouncer             // scope to one announcer
```

Each query returns a read-only snapshot of immutable `SubscriptionNode` value
records (`announcementClass`, `announcer`, `subscriber`, `handlerKind`, `once`).
To *act* on a subscription you cross back to the live `Subscription` token or the
`Announcer` — the read-vs-mutate rule shared by all three navigators:

```beamtalk
node := (a subscribersOf: PriceChanged) first
node announcementClass   // => PriceChanged
node subscriber          // => a Pid
node handlerKind         // => #do      (one of #do | #send | #doOnce)
node once                // => false
```

### Announcements vs `telemetry` (ADR 0069)

Beamtalk has two event buses; reach for the right one. **Measure** with
`telemetry` (spans, counters, durations — [Actor Observability and
Tracing](#actor-observability-and-tracing-adr-0069)); **react** with
Announcements (typed domain events you subscribe to in app logic):

| | `telemetry` (ADR 0069) | Announcements (ADR 0093) |
|---|---|---|
| Purpose | Measurement — spans, counters | Typed domain events you react to |
| Event identity | string list `[beamtalk, actor, dispatch]` | `Announcement` subclass (typed, MRO) |
| Delivery | sync, fire-and-forget | async or sync; isolated; monitored |
| Liveness | none (module-fun handlers) | `monitor`-based per subscriber |

> **Liveness note.** A subscription is bound to the *subscribing* process and
> auto-removed when that process dies — no manual cleanup leaks. In the REPL each
> turn evaluates in a fresh worker process, so a subscription made at one prompt
> is gone by the next; subscribe, announce, and observe within a single
> expression (or from a long-lived actor) when you need a subscription to persist.
>
> **Per-instance isolation.** Each `Announcer new` mints an independent
> dispatcher — subscriptions on announcer A are never matched by an `announce:`
> on announcer B, even for the same event class. `SystemAnnouncer` is the
> canonical multi-subscriber bus for system-wide events. Cross-node delivery to
> a *connected* node works; partition tolerance, replay, and the
> `RecordingAnnouncer`/telemetry-bridge extras live in the optional
> `beamtalk-announcements` package (BT-2454).

---

## Namespace and Class Visibility

Within a package (and the REPL workspace), Beamtalk uses a **flat namespace** — all
classes are visible to each other, with no `import` or `export` declarations. Across
packages, dependencies are declared in `beamtalk.toml` and a dependency's classes are
referenced by their short name, with qualified `package@Class` names available to
resolve collisions ([ADR 0070](ADR/0070-package-namespaces-and-dependencies.md)). The
original v0.1 flat-global-namespace decision is [ADR 0031](ADR/0031-flat-namespace-for-v01.md)
(superseded by ADR 0070).

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

Within a package, class names must be unique. Across packages, the namespace and
dependency system ([ADR 0070](ADR/0070-package-namespaces-and-dependencies.md)) makes
each declared dependency's classes available by their short name; collisions between
dependencies are a compile error, resolved with qualified `package@Class` names. See the
[Package Management guide](beamtalk-packages.md) for `beamtalk.toml`, dependencies, and
qualified names. [ADR 0031](ADR/0031-flat-namespace-for-v01.md) (superseded by ADR 0070)
records the original v0.1 flat-namespace decision.

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

`__beamtalk_meta/0` also carries toolchain provenance keys (`beamtalk_version` and `otp_release` as binary strings) when the module was compiled by a known toolchain (ADR 0098). Workspace attach and tooling use these to detect stale modules without re-reading the on-disk build stamp.

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
| **Binary** | Byte-level data — Collection subclass, parent of String ([ADR 0086](ADR/0086-string-subclass-of-binary.md)) |
| **Array** | Fixed-size indexed collection — O(log n) `at:`/`at:put:`, canonical value equality regardless of edit history ([ADR 0090](ADR/0090-array-canonical-representation.md)) |
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

Binary is a sealed Collection subclass for byte-level data. String is a subclass of Binary that adds grapheme-aware text operations. The class hierarchy is `Collection > Binary > String` ([ADR 0086](ADR/0086-string-subclass-of-binary.md)).

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

### Bag(E) — Multisets

`Bag(E)` is an unordered collection that allows duplicate elements. It is backed by a `Dictionary(E, Integer)` mapping elements to occurrence counts. Like other collections, Bag is immutable — mutating operations return a new Bag.

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
| `type_annotation` | Missing or redundant type annotation warnings in typed classes |
| `inheritance` | Sealed-class/sealed-method constraint errors |
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

`@expect` works inside method bodies, inside block bodies (e.g., `ifTrue: [...]`, `collect: [...]`, `whileTrue: [...]`), on declarations in class definitions, and at module scope (BT-2010).

**Where `@expect` is evaluated (BT-2851):** `@expect` directives are matched and applied by a single function, `apply_expect_directives`, called at the end of both diagnostic pipelines:

- `beamtalk lint` — after parsing, lint passes, and semantic analysis (`collect_diagnostics` in the CLI's `lint` command).
- `beamtalk build` / `beamtalk test` / the LSP — after semantic analysis and all post-analysis passes (`compute_project_diagnostics`, the unified pipeline from BT-2009).

"Stale" means: for a given `@expect` directive, *this specific diagnostic run* produced no diagnostic of a matching category whose span falls inside the annotated expression or declaration. Both pipelines share the same semantic-analysis entry points and the same matching logic, so a diagnostic that either surface can produce is treated identically by both — an `@expect` is not stale on one surface and legitimate on the other for the same diagnostic.

This includes Erlang FFI argument-type diagnostics (`@expect type` on a call like `Erlang lists reverse: 42`): both `beamtalk lint` and `beamtalk build`/`beamtalk test` type-check FFI calls against the same native-type registry, populated by the same extractor (`extract_type_specs`) from OTP/dependency `.beam` files. Earlier, lint read that registry only from an on-disk cache (`_build/type_cache/`) written by a *previous* `beamtalk build`; on a project that had never been built, the cache read returned nothing, lint silently skipped the FFI argument-type check, and any `@expect type` added to suppress the corresponding build-time diagnostic was then reported "stale @expect" by lint. Lint now calls the same live extractor build does — which still reads the on-disk cache as a fast path when it is fresh — so both surfaces populate the registry identically regardless of build order.

### Pragma Annotations (`@primitive` and `@intrinsic`)

The standard library uses pragma annotations to declare methods whose implementations are provided by the compiler or runtime rather than written in Beamtalk code.

There are two pragma forms:

| Pragma | Syntax | Purpose |
|--------|--------|---------|
| `@primitive` | Bare (no selector) | **Selector-based dispatch**, selector inferred from the enclosing method. Equivalent to `@primitive 'sel'` where `sel` is the method's own selector. Preferred form. |
| `@primitive 'selector'` | Quoted selector | Same selector-based dispatch, but with an **explicit selector override** — used when the runtime function name differs from the method selector (e.g. class-side aliases like `signal => @primitive 'classSignal'`). |
| `@intrinsic name` | Unquoted identifier | **Structural intrinsic** — the compiler generates specialized code inline. Used for spawning, block evaluation, control flow, reflection, etc. |

Both forms are semantically equivalent at the compiler level (they produce the same AST node), but the naming convention distinguishes their intent:

**`@primitive` (bare or quoted)** — runtime-dispatched method implementations. A bare `@primitive` infers its selector from the method, so the explicit string is only needed for genuine renames:
```beamtalk
// In stdlib/src/Integer.bt — bare form, selector inferred ('+' and 'asString')
+ other => @primitive
asString => @primitive

// In stdlib/src/Exception.bt — explicit override (method 'signal' → runtime 'classSignal')
class signal => @primitive 'classSignal'
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

**Relationship to `native:` (ADR 0101).** `@primitive` and `@intrinsic` cover native BEAM *value types* and compiler *substrate*. A third mechanism, the class-level `native:` declaration with `=> self delegate` bodies, covers whole-class **delegation** to a single Erlang module (a stateless `Object` such as `Stream`, or an `Actor` gen_server). Pick by what the method needs: guarded dispatch + the open-world extension registry → `@primitive`; the dispatch act itself (`==`, `class`, `perform:`, actor lifecycle) → `@intrinsic`; pure pass-through to one module → `native:`. See [`native:` for stateless Objects](beamtalk-native-erlang.md#native-stateless-objects--native-for-object).

**Known limitation: `whileTrue:`/`whileFalse:`/`repeat`/`on:do:`/`ensure:` via `perform:`.** These five structural intrinsics have real semantics only at the call-site interception the compiler recognizes for a literal message send (e.g. `[cond] whileTrue: [body]` written directly in source). Reaching the compiled method body through any other dispatch path — `perform:`, `perform:withArguments:`, or other generic/dynamic sends — resolves to a placeholder and raises a misleading `does_not_understand` naming the wrong (internal) selector, e.g. `[i < 3] perform: #whileTrue: withArguments: #([i := i + 1])` fails with `Block does not understand 'whileTrue'` rather than working or raising a clear error. `respondsTo:` still returns `true` for these selectors, since the method genuinely is defined — this is a runtime dispatch gap, not a missing method. Fixing it generically would require reimplementing loop and exception-handling control flow (condition re-evaluation, catch/handler dispatch, cleanup-on-exit) over an opaque runtime fun rather than the literal block AST the call-site codegen relies on — a materially bigger change, tracked separately (BT-2908) rather than accepted as part of the smaller List/Collection iteration fix (BT-2888) that closed the equivalent gap for `do:`/`collect:`/`select:`/`reject:`/`inject:into:`.

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
