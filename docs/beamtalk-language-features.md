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
- [Traits (ADR 0127)](#traits-adr-0127)
  - [Which mechanism to use](#which-mechanism-to-use)
  - [Reflection](#reflection)
- [Union Types and Narrowing (ADR 0068)](#union-types-and-narrowing-adr-0068)
  - [Named Type Aliases (`type` Declarations) (ADR 0108)](#named-type-aliases-type-declarations-adr-0108)
- [Actor Message Passing](#actor-message-passing)
- [Server — OTP Interop (ADR 0065)](#server--otp-interop-adr-0065)
- [Supervision Trees (ADR 0059)](#supervision-trees-adr-0059)
- [Named Actor Registration (ADR 0079)](#named-actor-registration-adr-0079)
- [Pattern Matching](#pattern-matching)
- [Live Patching](#live-patching)
  - [Keyword Method Patching — `compile:source:` and `tryCompile:source:` (ADR 0082)](#keyword-method-patching--compilesource-and-trycompilesource-adr-0082)
  - [Removing methods — `removeSelector:` and `removeSelector:ifAbsent:` (ADR 0112)](#removing-methods--removeselector-and-removeselectorifabsent-adr-0112)
  - [ChangeLog — Tracking In-Memory Changes (ADR 0082)](#changelog--tracking-in-memory-changes-adr-0082)
  - [Live Re-Checking on Reload (ADR 0105)](#live-re-checking-on-reload-adr-0105)
- [Actor Observability and Tracing (ADR 0069)](#actor-observability-and-tracing-adr-0069)
- [Application Telemetry Events (ADR 0069 amendment)](#application-telemetry-events-adr-0069-amendment)
- [Announcements — Typed Events (ADR 0093)](#announcements--typed-events-adr-0093)
- [Distribution — Location-Transparent Actors (ADR 0126)](#distribution--location-transparent-actors-adr-0126)
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

// Escape sequences inside strings (ADR 0138):
//   \r   carriage return (U+000D)
//   \n   line feed (U+000A)
//   \t   horizontal tab (U+0009)
//   \\   backslash
//   \"   double quote ("" doubling still works too)
//   \0   NUL (U+0000) — exactly one char, no C-style octal continuation
//   ""   doubled delimiter → literal double-quote character
//   \{   literal { — backslash consumed (prevents interpolation)
//   \}   literal } — backslash consumed
// A backslash before anything else is a compile error pointing at the
// backslash: "\q" → unknown escape '\q' … (use '\\' for a literal backslash).
// Symbol literals (#'…') have no backslash handling: #'a\nb' is backslash-n.
quote := """"                    // 1-char string containing "
dialog := "She said ""hello"""  // → She said "hello"
crlf := "\r\n"                   // 1 grapheme — byte-identical to (Character value: 13) asString ++ (Character value: 10) asString

// In Erlang, strings are tagged tuples holding UTF-8 binaries:
//   "hello"  →  {beamtalk_string, <<"hello">>}
```

### Character Encoding

| Beamtalk | Erlang/BEAM | Notes |
|----------|-------------|-------|
| `"hello"` | `{beamtalk_string, <<"hello">>}` | Tagged UTF-8 binary (BT-4173) |
| `"Hi, {name}"` | `{beamtalk_string, <<"Hi, ", Name/binary>>}` | Interpolated UTF-8 (ADR 0023) |
| Grapheme cluster | Via `:string` module | `"👨‍👩‍👧‍👦"` is one grapheme, multiple codepoints |
| `$a` | `97` (codepoint) | Character literal = Unicode codepoint |

### Character Methods

Character-typed receivers dispatch through the Character method table, so methods like `asString`, `printString`, `uppercase`, `lowercase`, and `class` return Character-appropriate values. This applies to character literals (`$A`), the `Character value:` class factory, chained `uppercase`/`lowercase` sends, `ifTrue:ifFalse:` conditionals whose outcome is statically Character, and single-block `ifTrue:`/`ifFalse:`/`and:`/`or:` sends whose literal receiver takes a Character-answering block — all statically Character-typed forms. A two-branch conditional is transparent when a literal condition takes the known branch, or when both branches agree (a non-constant condition with mixed branches stays Integer-dispatched). A single-block send is transparent only when its receiver is the literal that runs the block (`true ifTrue:`, `false ifFalse:`, `true and:`, `false or:`): on the other path it answers a Boolean (`false ifTrue: [$A]` answers `false`, not `nil`), so a non-constant receiver stays Integer-dispatched:

```beamtalk
$A asInteger              // => 65
$A asString               // => "A"
$A printString            // => "$A"
$A uppercase              // => 65
$A lowercase              // => 97
$A class                  // => Character
$A respondsTo: #uppercase // => true
Character value: 65       // => $A
(Character value: 10) asString   // => "\n" (LF, not "10")
$a uppercase asString            // => "A"
(true ifTrue: [Character value: (48 + 6)] ifFalse: [0]) asString  // => "6"
(true and: [$A]) printString     // => "$A"
(false ifTrue: [$A]) class       // => False
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

// First / last grapheme
"Hello" first         // => "H"
"Hello" last          // => "o"
"über" first          // => "ü" (one grapheme, two UTF-8 bytes)

// Iteration over graphemes
"Hello" each: [:char | Transcript show: char]

// Case conversion (locale-aware)
"HELLO" lowercase     // => "hello"
"Content-TYPE" asciiLowercase  // => "content-type" (only A-Z fold; non-ASCII untouched)
"straße" uppercase    // => "STRASSE" (German ß → SS)
```

### Inherited Byte-Level Methods

String inherits byte-level methods from Binary. These provide unambiguous byte access regardless of grapheme semantics:

```beamtalk
// Byte access (inherited from Binary)
"hello" byteAt: 0      // => 104 (byte value, 0-based)
"hello" toBytes size   // => 5 (byte count — `size` itself counts graphemes)
"café" toBytes size    // => 5 (bytes — more than 4 graphemes due to UTF-8)

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
| `"string"` | `{beamtalk_string, <<"string">>}` | Tagged binary, not charlist |
| `"世界"` | `{beamtalk_string, <<228,184,150,231,149,140>>}` | UTF-8 encoded bytes |
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
- **`terminate: reason`** — called automatically during graceful shutdown (`stop`) and supervisor-initiated shutdown (cascade or direct `aSupervisor terminate: aClass`). Use it to clean up resources. The `reason` parameter indicates why the actor is stopping (e.g., `#normal`, `#shutdown`).

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
| Called on | `spawn` / `spawnWith:` | `stop`, `stopWith:`, supervisor shutdown (`rest_for_one`/`one_for_all` cascade, direct `terminate:`), linked exit |
| Error effect | Spawn fails with `InstantiationError` | Shutdown proceeds anyway |
| Called on `kill`? | N/A | No — `kill` bypasses `terminate:` |
| Side effect | — | Overriding `terminate:` causes the actor to trap exits (`process_flag(trap_exit, true)`); actors without an override keep untrapped link semantics |
| Actor state | Accessible via `self.field` | Accessible via `self.field` |

Both hooks are optional — actors without them work normally.

**Stopping with a reason** — `stop` stops an actor with reason `#normal`. `stopWith: reason` (and `stopWith: reason timeout: ms`) stops it with any reason you choose; `terminate:` receives exactly that reason, and so does every `onExit:` block. This lets an actor route its cleanup on *why* it stopped, OTP-style:

```beamtalk
Server subclass: Connection
  handleInfo: msg =>
    msg == #tcp_closed ifTrue: [self stopWith: {#shutdown, #peer_closed}]

  terminate: reason =>
    reason =:= #normal ifTrue: [^nil]
    // e.g. {shutdown, peer_closed}, {shutdown, read_timeout}, or an error
    self logClose: reason
```

- Sent to `self` from inside one of the actor's own methods, the stop takes effect **after the current message has been handled** — a pending caller still gets its reply — then the actor terminates. (The same applies to `self stop`.) From another process it is `gen_server:stop/3`: the call returns once the actor has terminated (default timeout 5 seconds), and stopping an already-stopped actor succeeds silently.
- `#normal`, `#shutdown` and `{shutdown, term}` tuples are **clean** stops: no crash report, and a `#transient` supervised child is not restarted. Any other reason — a Symbol like `#boom`, an exception — is a **crash**: it is logged, a `#transient` child is restarted, and any process linked to the actor (its supervisor, or the spawner of a `spawnAs:` actor — plain `spawn`/`spawnWith:` do not link) receives the exit signal, as with any OTP process exit.
- Stopping itself from `initialize` (`self stop` or `self stopWith:`) makes the `spawn` fail, since the actor is gone before it is handed back.
- A stop requested earlier in a method still happens if the method then raises: the caller gets the error, then the actor stops.

**Spawn with a monitor (BT-4104).** `Worker spawnMonitored` and `Worker spawnMonitoredWith: initArgs` answer a `{actor, ref}` Tuple (`at: 1` / `at: 2`) whose monitor is created atomically with the process. A separate `X spawn` followed by `monitor` reports `{#DOWN, ref, #process, pid, #noproc}` for an actor that exits straight after `initialize` (for example a `continueWith:` step ending in `stopWith: #normal`); `spawnMonitored` delivers the real reason (`#normal` here). Failures raise `instantiation_error` like `spawn`, leaving no stray monitor. The actor is not linked, and supervisor classes do not support these selectors (OTP has no atomic supervisor start plus monitor).

**The monitor belongs to the process that runs the spawn.** Use a static send (`Worker spawnMonitored`) from an actor or Server method so the `#DOWN` reaches that process's `handleInfo:`. A dynamic send (`cls spawnMonitored`, `perform:`) or a send inside a class method runs in the class's process, which then owns the monitor, so its `#DOWN` is never seen by the sender. This is documented rather than raised, because it cannot be detected on the static-from-class-method path. Linked and named-and-monitored variants are follow-ups.

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
  childSpecs => self subclassResponsibility
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

### Self-Sends and Overrides (BT-3666)

A `self` send is late-bound on the *receiver*, as in every Smalltalk: an inherited (or trait-flattened, ADR 0127) method that does `self foo` runs the `foo` of the receiver's class, so a subclass override is reached. This is what makes the template-method pattern work.

```beamtalk
Actor subclass: Report
  title -> String => "Report"
  render -> String => "== " ++ self title ++ " =="

Report subclass: SalesReport
  title -> String => "Sales"

// SalesReport spawn render  => "== Sales =="
```

This holds for instance-side sends on actors and value classes, for class-side sends (`class foo` / `self foo` inside a class method), and for a protocol's provided methods flattened into a class whose subclass overrides a required selector.

`super` is the exception by design — it is always bound to the superclass of the class that *contains* the method. A `sealed` class cannot be subclassed, so its instance-side self-sends are compiled as direct calls (no lookup); class-side self-sends take the guarded direct call described below, sealed or not. Inside a class method, `super foo` resolves against the superclass's *class* methods (the metaclass chain), not its instance methods, and the compiler checks it there: a selector only the superclass's instances understand is reported as not understood (BT-4001). The lookup starts at the superclass and never consults the sending class, so a class-side extension override (`C class >> peek: x => (super peek: x) + 1`) reaches the superclass's `peek:` instead of itself; at each ancestor a class-side extension counts as one of its class methods, so `super` finds a superclass method whether its class body or an extension defined it. The superclass method runs with the live receiver, so a subclass inheriting the method keeps its own class variables. When no class method on the chain answers the selector, `self` and `super` sends from a class method fall through to the class object's own protocol (`Class` → `Behaviour` → …), exactly as an outside send to the class does: `self name`, `super name` and `super superclass` answer what `C name` and `C superclass` answer, for the receiving class (BT-4252).

Implementation notes: an actor self-send dispatches through the module named by the instance's own `'__class_mod__'` state key (a per-send map lookup); a class-side self-send to a selector the class defines, sealed or not, calls the compiled method directly when the receiver is the defining class and nothing shadows it (`beamtalk_class_dispatch:class_self_direct_ok/4`), and otherwise walks the class hierarchy from the receiving class (`beamtalk_class_dispatch:class_self_send/3`). A class-side self-send runs wherever its calling method runs: in the caller when the class's chain has no class variables, in the class process otherwise ([ADR 0142](ADR/0142-class-process-owns-only-class-state.md)). Every path passes the real class reference as `self` (BT-4037 retired the old `nil`). Only `self foo` late-binds: naming the class explicitly (`Base foo`) calls `Base`'s own method directly, behind the same guard, but the running receiver (`ClassSelf` and its class variables) is still the subclass's, so `self` sends nested inside `Base foo` still late-bind to overrides, `self name` answers the subclass's name, and class-variable writes go to the subclass's class variables. Whether an explicit `Base foo` should instead bind `self` to `Base`, as a literal send does in Smalltalk, is an open question ([BT-4317](https://linear.app/beamtalk/issue/BT-4317)); today's behaviour is the one described here. Class variables live in the class process and are read and written in place (ADR 0130), so a write is visible to every later read wherever it happens: at a class method's top level, inside bare blocks, loops, conditionals, `ensure:`/`on:do:` bodies, and in a stored closure invoked by a later statement. Nothing is threaded through class-side self-sends. A write made inside a catch boundary (`on:do:`, `Result tryDo:`) whose block then raises is discarded with the rest of that protected region (ADR 0130 §4); writes made before entering the boundary are kept. A selector declared `class sealed` cannot be overridden; its sends compile to the same guarded direct call as any other own selector.

**Cost:** every non-sealed actor self-send does a `maps:get('__class_mod__', State, Module)` lookup (tens of nanoseconds). An open-class class-side `self foo` is a guarded direct call: when the receiver is the defining class itself and no class-side extension or runtime-installed method shadows `foo`, it calls the compiled `class_foo` directly (a few `persistent_term` reads more than a sealed class, about 215 ns on a 4-core VM); when the receiver is a subclass it walks the class hierarchy (microseconds). Seal the class (or the selector, class-side) to keep plain direct calls.

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

This compares the **representation**, and cannot be changed: the equality operators are not overridable (see [Equality operators cannot be overridden](#equality-operators-cannot-be-overridden)). A value type whose logical value differs from its stored form — one that is not canonicalised, or that carries a field outside its identity — should override `equals:` and say so in its class documentation.

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

Parentheses around a send's receiver or argument only group: when they are
redundant they mean nothing, so `(self) foo`, `(MyClass) + 1` and
`xs do: ([:x | t := t + x])` compile and run exactly like `self foo`,
`MyClass + 1` and `xs do: [:x | t := t + x]`: the compiled behaviour is the
same with or without them. Reflected method source
(`(MyClass >> #sel) source`) is the text as written and keeps them — a method
written `summed: a => (a * 2) + 1` reflects as exactly that — so the IDE method
editor shows what is on disk. Source files are never rewritten to drop them:
`beamtalk fmt`, and a workspace flush of a method edited in the REPL or IDE,
keep the parentheses as written.

The exception is `super`, which is not a value: it only names where a send's
method lookup starts, so it is valid only as the direct receiver of a message
(`super printString`, `super at: 1 put: x`, `(super printString) size`). Every
other use is a compile error inside a method body, instance or class side:
`(super) printString`, `x := super`, `^ super`, `super` as an argument or a
collection element, `super.count` (fields are read through `self`), and a
cascade on `super` (`super a; b`; send each message to `super` separately). To
use the receiver itself as a value, write `self`
([BT-4251](https://linear.app/beamtalk/issue/BT-4251),
[BT-4258](https://linear.app/beamtalk/issue/BT-4258)).

### Splitting a Message Send Across Lines

A newline acts as a statement separator (see "Core Syntax" above), so whether
a message send can be split across lines — receiver on one line, message on
the next — depends on whether the following line could validly begin a *new*
statement on its own:

```beamtalk
// Keyword messages split fine: a bare keyword part (`copyFrom:`) can never
// start a statement on its own, so it unambiguously continues the receiver
// above, no matter how it's indented.
result := "abc"
  copyFrom: 1
  to: 2

// A unary selector chained after something else on the same line also
// splits fine, because only the *continuation* keyword moves to its own line:
result := "abc" asList
  collect: [:ch | ch uppercase]
```

```beamtalk
// A unary selector alone on its own line, immediately after its intended
// receiver, does NOT continue it — it parses as a new statement instead:
result := "abc"
  asList        // parsed as its own statement, not `"abc" asList`
```

This is intentional, not a bug: a bare identifier like `asList` is itself a
valid statement (e.g. a variable reference), so the parser cannot safely
rejoin it with the line above without risking silently swallowing a genuinely
separate statement that happens to start with a unary-looking name. Keyword
parts don't have this ambiguity — `bar:` alone can never be a complete
statement — which is why only keyword-message continuation is safe to infer
automatically.

**Keep a unary selector on the same line as its receiver.** If the identifier
that follows fails to resolve, the diagnostic hints at this — join the two
lines instead of chasing a phantom spelling mistake.

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

##### Number-on-the-left coercion (`plusFromNumber:` etc.)

`+ - * /` are receiver-dispatched messages, so a value type can overload them as the receiver — `aVector + 5` works because `Vector` defines `+`. That says nothing about `5 + aVector`: a numeric literal (or other statically-numeric receiver) on the **left**, with a non-numeric value type on the right. For that shape, Beamtalk dispatches to a **reflected method** on the right operand instead of crashing, named by a `<verb>FromNumber:` convention:

| Operator | Reflected method |
|---|---|
| `+` | `plusFromNumber:` |
| `-` | `minusFromNumber:` |
| `*` | `timesFromNumber:` |
| `/` | `divFromNumber:` |

A value type opts in per operator by declaring the method it wants to support, alongside its ordinary receiver-dispatch operators:

```beamtalk
Value subclass: Vector
  field: components :: Array = #()

  + other :: Number -> Vector => self collect: [:c | c + other]
  - other :: Number -> Vector => self collect: [:c | c - other]

  plusFromNumber:  n :: Number -> Vector => self collect: [:c | n + c]
  timesFromNumber: n :: Number -> Vector => self collect: [:c | n * c]
  minusFromNumber: n :: Number -> Vector => self collect: [:c | n - c]
```

```beamtalk
aVector + 5   // => Vector(6, 7, 8)   receiver-dispatch `+`, unchanged
5 + aVector   // => Vector(6, 7, 8)   dispatches to `aVector plusFromNumber: 5`
```

**Operand order matters — easy to get backwards for a non-commutative operator.** The reflected method's *receiver* (`self`) is the value that was on the **right** of the original expression; its *parameter* is the value that was on the **left**. `5 - aVector` sends `aVector minusFromNumber: 5`, computing `n - self` (`5` minus each component) — not `self - n`. `aVector - 5` (ordinary receiver-dispatch `-`, unchanged) computes the opposite: each component minus `5`. These are genuinely different computations, not the same subtraction read backwards:

```beamtalk
aVector := Vector withAll: #(1 2 3)
aVector - 5   // => Vector(-4, -3, -2)   each component minus 5 (self - n)
5 - aVector   // => Vector(4, 3, 2)      5 minus each component (n - self)
```

**When an operator doesn't apply, implement it anyway — to reject with intent.** Not every reflected operator makes sense for every type: `5 / aVector` ("a number divided by a vector") rarely has an obvious meaning the way `5 * aVector` (scale) does, and a point-like type (e.g. `Temperature`) may sensibly support `+` but not `*`. Omitting the method leaves `does_not_understand` to fire on a selector — `divFromNumber:` — that the caller never typed, which is harder to connect back to their own `5 / aVector` than an ordinary DNU is. Implementing the method anyway, with a body that rejects explicitly, is better:

```beamtalk
divFromNumber: n :: Number -> Vector =>
  self error: "Cannot divide a number by a Vector — did you mean (aVector / n)?"
```

A type author who commits to number-on-the-left arithmetic at all typically implements all four reflected methods — some as real arithmetic, the rest as deliberate rejections — not a variable subset.

**Missing hook: `does_not_understand`, with a hint.** A type that implements no reflected method at all still fails safely — `5 + aThing` raises an ordinary `does_not_understand` rather than crashing with a raw BEAM `badarith`. Because `plusFromNumber:` is a selector *synthesized* by this mechanism rather than one the caller ever typed, the error carries an added hint naming the original operator:

```beamtalk
5 + "not a vector"
// => ERROR: String does not understand 'plusFromNumber:'
// => ERROR: String has no 'plusFromNumber:' — implement it to support 'number + String' arithmetic
```

This is receiver-dispatch's mirror image, not a new arithmetic mechanism: no `generality`/coercion tower, no `perform:`-based reflective retry — each reflected method is an ordinary, fully-typed method, so static typing (covariant-return refinement) and compile-time DNU checking work on it exactly as they do on `+`/`-`/`*`/`/` themselves. A type that never implements number-on-the-left arithmetic pays no cost for it: a statically-typed call site (`total + delta`, both operands' types known at compile time) compiles to the identical bare arithmetic instruction whether or not this mechanism exists. Comparison operators (`< > <= >=`) are **not** covered by this convention — `5 < aVector` is unaffected. See [ADR 0116](ADR/0116-number-on-the-left-arithmetic-coercion.md) for the full double-dispatch design, the rejected alternatives (a Smalltalk-style `generality` tower, `perform:`-based retry), and the zero-cost measurement for the statically-typed case.

#### Comparison
- `<` - Less than: `3 < 5` → `true`
- `>` - Greater than: `5 > 3` → `true`
- `<=` - Less than or equal: `3 <= 3` → `true`
- `>=` - Greater than or equal: `5 >= 3` → `true`

Comparisons are messages sent to the **receiver**, which decides. `Integer`, `Float`, `Character`, `String` and `Symbol` define them (numbers numerically, characters by codepoint, strings and symbols by codepoint, lexicographically); a value type can define its own (`aMoney < bMoney` reaches `Money>><`). A receiver whose class defines no ordering raises `does_not_understand` — including the non-object primitives `nil`, `true`/`false`, lists and tuples:

```beamtalk
#apple < #banana   // => true
x := nil
x < 5              // => ERROR: UndefinedObject does not understand '<'
```

This holds however the receiver is typed: an untyped local or parameter, or an `:: Integer | Nil` field that holds `nil`. Only a receiver statically known to be a number, character or string (a literal, a `:: Integer`/`:: String`-style parameter or field, `self` in those classes) compiles to the bare comparison instruction; everything else is checked at run time (one inline type test), so there is no "total order over every value" to fall back on.

The **argument** is not checked at run time: a number, character, string or symbol receiver compared with an argument of a different kind falls back to the BEAM's term order and does not raise, so `5 < nil` answers `true` and `#a < 5` answers `false`. The compiler warns when it can see the mismatch (the argument's static type is known and is not the receiver's kind, e.g. `5 < nil`, `$a < 1`, `$x < nil`, `#a < 5`); an untyped or generic argument stays silent. Comparison operators have no number-on-the-left coercion (see above).

##### Arithmetic and comparison on fields: annotate numeric state

Arithmetic and comparison operators are messages, so `self.count + 1` must
dispatch to the object's `+` when `count` holds a `Point` or `Money`. The
compiler emits the bare Erlang arithmetic/comparison instruction only when it
*knows* the operand is a primitive; otherwise it adds a runtime type test that
sends the message to objects. For a field, "knows" means the declared type: a
field annotated `:: Integer`, `:: Float` or `:: Number` takes the bare fast
path, and comparisons also accept `:: Character` / `:: String`. A union with
`Nil` (`:: Integer | Nil`) stays bare for arithmetic only; its comparisons get
the runtime test so a `nil` raises instead of term-ordering (see Comparison
above). An **untyped** field always gets the runtime test,
whatever its default, because writers outside the class can store any object in
it (BT-3925):

```beamtalk
Actor subclass: Counter
  state: count = 0                       // untyped: guarded
  increment => self.count := self.count + 1

c := Counter spawnWith: #{#count => aPoint}
c increment                              // sends Point>>+, no badarith

Actor subclass: FastCounter
  state: count :: Integer = 0            // annotated: bare arithmetic
  increment => self.count := self.count + 1
```

The test is cheap (the BT-3925 counter benchmark measured no slowdown, see
`docs/development/benchmarks.md`), but annotating numeric state with
`:: Integer` is the documented way to get the bare instruction.

#### Equality (lowest precedence)
- `=:=` - Strict equality (Erlang `=:=`): `5 =:= 5` → `true`
- `==` - Loose equality (Erlang `==`): `5 == 5.0` → `true`
- `/=` - Loose inequality (Erlang `/=`): `5 /= 6` → `true`
- `=/=` - Strict inequality (Erlang `=/=`): `5 =/= 6` → `true`

Note: bare `=` is **not** a valid Beamtalk operator — it has no entry in the parser's precedence table, so `x = y` fails to parse. Use `=:=` for value equality or `==` for reference equality.

##### Equality operators cannot be overridden

Unlike the arithmetic (`+ - * /`) and ordering (`< > <= >=`) operators, which are dispatched as messages so a value type can overload them, all four equality operators are lowered directly to the corresponding Erlang BIF at codegen time (ADR 0002). There is no method lookup, so a class-level `=:=` / `=/=` / `==` / `/=` method could never be called. **Declaring one is a compile error** (BT-2997). A *dynamic* send — `x perform: #'==' withArguments: #(y)`, `x respondsTo: #'=='` — works on every receiver (integers, pids, references, value objects, actors): it reaches `ProtoObject`'s declarations, whose bodies are the same BIFs (BT-3919).

```beamtalk
Value subclass: Money
  field: cents :: Integer = 0
  =:= other :: Money -> Boolean => self.cents =:= other cents
  // error: `=:=` cannot be overridden — this method can never be called
```

Use **`equals:`** instead. It is declared on `Object`, defaults to `=:=`, and is an ordinary message send, so overriding it works:

```beamtalk
// Fractions stored unnormalised: 1/2 and 2/4 are different terms…
half := Fraction numer: 1 denom: 2
twoQuarters := Fraction numer: 2 denom: 4

half =:= twoQuarters      // => false  (structural — compares representation)
half equals: twoQuarters  // => true   (dispatches to Fraction>>equals:)
```

This is a deliberate limit, not a gap to be closed. The keyed containers decide identity inside the VM, below anything the language can dispatch — `Dictionary` by Erlang map keys, `Set` by a term-order-sorted list — as do `lists:member/2`, `ets`, and receive-pattern matching. An override the compiler honoured would still be invisible to all of them, so `a =:= b` could report `true` while a `Set` holding both still reported size 2. A class that needs content-based membership must normalise its representation rather than redefine the operator.

Both containers use `=:=` for identity, so they agree with each other and with `List>>includes:` — `Set new add: 1; add: 1.0` holds two elements, matching a `Dictionary` keyed on `1` and `1.0`. What they cannot do is consult a user-defined `equals:`.

##### What honours `equals:`

| | Compares with |
|---|---|
| `includes:` on `Collection`, `List`, `Array`, `Dictionary` (searches values); `List>>indexOf:`; `TestCase>>assert:equals:` | `equals:` |
| `Set` elements, `Dictionary` *keys*, `List>>unique`, `List>>withoutDuplicatesBy:` keys | `=:=` |

The line is searching versus keying: a linear search can afford to ask each element, while membership in a keyed container — or deduplication — needs an order or a hash that a user-defined `equals:` cannot supply. This is the same constraint Java's `equals`/`hashCode` contract expresses.

An `equals:` override must agree with `=:=` wherever `=:=` holds: it may only make *more* values equal, never fewer. The searches rely on that to try raw equality first and dispatch only on a miss.

Where the two notions of equality are genuinely different questions, prefer a domain-specific name to overriding `equals:` — `DateTime` keeps `equals:` structural and offers `sameInstant:` for instant-based comparison.

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

**Parenthesized assignment:** A field assignment used as an operand (the receiver or argument of a send, an element of a literal, an assigned value) must be wrapped in parentheses. There, `(self.x := 5)` answers the assigned value:

```beamtalk
// Assignment as expression (returns 6)
(self.x := 5) + 1

// Field assignments as sequential statements
self.x := 5
self.y := self.x + 1
self.y
```

Parentheses only matter in operand position. Around a whole statement they change nothing. In a value-type method, a trailing field assignment answers the updated `self` whether or not it is parenthesised: `(self.x := 5)` and `self.x := 5` as the last statement both answer the updated `self`. To answer the assigned value, write it explicitly (BT-4092):

```beamtalk
// Value-type method answering the updated self (with or without parentheses)
withX5 =>
  self.x := 5

// Value-type method answering 5
setXTo5 =>
  self.x := 5
  5
```

Before BT-4092, a parenthesised trailing field assignment answered the assigned value.

**Limitation:** In an actor instance method a block reads and writes the actor's fields live, wherever it is made and run (see [Actor Blocks See Live State](#actor-blocks-see-live-state)), and in a class method a block may assign class variables wherever it is written (they live in the class process, [ADR 0130](ADR/0130-class-variables-live-in-the-class-process.md)). In a value-type method, field assignments (`self.x :=`) in stored closures are still a compile error — they require control-flow context for state threading. A closure that assigns an outer local may not be stored at all: a block may assign a captured variable only where it is written directly as the argument of a control-flow construct or a compiler-known collection message (`do:`, `inject:into:`, `on:do:`, ...), and a block that is stored, returned, placed in a collection literal or passed to any other method is rejected at the assignment ([ADR 0133](ADR/0133-scope-of-captured-variable-mutation-in-blocks.md), see [What Works and What Doesn't](#what-works-and-what-doesnt)). Have the block answer the new value and assign it where the block is called.

```beamtalk
// ❌ ERROR in a value-type method; works in an actor instance method (ADR 0137)
nestedBlock := [:m | self.x := m]

// ✅ Field mutation in control flow blocks
true ifTrue: [self.x := 5]

// ✅ Local variable mutation in a control-flow block
count := 0
10 timesRepeat: [count := count + 1]   // count => 10
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

**Scoping:** a variable first assigned inside a block is local to that block —
`temp` above is invisible outside it — and nested blocks see their enclosing
blocks' locals. Assigning a variable declared *outside* the block (a captured
local) is a separate rule: it compiles only where the block is written directly
as the argument of a control-flow construct or a compiler-known collection
message (see [Control Flow and Mutations](#control-flow-and-mutations) and
[ADR 0133](ADR/0133-scope-of-captured-variable-mutation-in-blocks.md)).

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
  **Where a class method runs (ADR 0142).** A class method runs in the *caller's* process, like a function in an Erlang module, unless the class needs its process. The class's gen_server (the class process) runs it only when:
  - **the class or any class in its superclass chain declares class variables** (`classState:`). The rule is chain-wide: a subclass holds its ancestors' class variables in its own process, and its own methods reach them through `self` sends, so it keeps its process for every class method, including ones it defines itself; or
  - **the selector is process-bound**: `new`, `new:`, `startLink`, `startLink:`, `spawn`, `spawnWith:`, `supervise`, `spawnAs:`, `spawnWith:as:`, and the `TestCase` run selectors `runAll`, `runAll:`, `run:` and `run:method:`. These keep their routing whatever the class declares, even when a class defines a class method of that name. The class-variable reflection sends `hasField:` and `clearField:` are answered by the class process itself; or
  - **something shadows the receiving class's own methods**: a class-side extension (ADR 0066) or an ADR 0084 runtime class-method fun installed on that class clears its direct-call flag, and its class process applies the shadow, as before. A runtime fun on an *ancestor* counts as that ancestor's method and is applied in the caller.

  `sealed` and `class sealed` play no part. The rule is decided once, in the compiler (`ClassHierarchy::runs_class_method_in_caller`), and re-checked at run time by one `persistent_term` flag read (`beamtalk_class_shadow_flags:direct_call_ok/2`), so it holds however the method is reached: a literal send, a class in a variable, `perform:`, `Beamtalk classNamed:`, a class passed as an argument, or an Erlang caller of `class_send`. A class that needs a serialising home declares `classState:` or delegates to an Actor. For a pure wrapper the difference is invisible; it matters for FFI that depends on which process runs it, and for resources the method creates, which now belong to the caller and die with it. Two lints, split on that one rule, warn at a direct `(Erlang mod) fn …` call in a class method (a call gets at most one of them):
  - **`class-process-ffi`** — the class or an ancestor declares class variables, so the method runs in the class process, and the call depends on the *calling* process: `(Erlang gen_tcp) accept:` blocks the shared class process (every caller queues behind it), and `controlling_process`, `file:sendfile`, `erlang:self`, the process dictionary, `link` and `monitor` act on the class process, not the caller (`not_owner` failures for sockets the caller owns). The hint names the class (or ancestor) that declares the class variables and says to move the call into an instance method or an Actor. Suppress deliberate serialisation with `@expect class_process_ffi`, or package-wide with `class-process-ffi = "off"` under `[diagnostics]`.
  - **`class-method-process-resource`** — no class in the chain declares class variables, so the method runs in the caller, and the call creates a resource tied to the calling process (`ets:new`, `erlang:open_port`, `spawn_link`, `link`, `monitor`, `register`, `erlang:send_after`/`start_timer`, `timer:send_after`/`send_interval`/`apply_interval`/`apply_repeatedly`) or acts on the calling process itself (`self()`, the process dictionary, `process_flag`, `unlink`, `demonitor`). The resource is the caller's, not the class process's. The fix is to move ownership to an Actor (create the resource in its `initialize`), or declare `classState:` to keep the class process. Suppress with `@expect class_method_process_resource`, or package-wide with `class-method-process-resource = "off"`.

  v1 limits (both): one curated table of `gen_tcp`/`gen_udp`/`ssl`/`erlang`/`ets`/`timer`/`file:sendfile` functions, direct calls only (a helper that does the FFI is not followed), process-bound selectors (`new`, `startLink`, `spawn`, …) are not inspected, and `ClassBuilder classMethods:` blocks are not inspected.
- **Supervisor lifecycle:** `class supervise`, `class current` (configuration such as `childSpecs` and `strategy` is instance-side, ADR 0135)
- **Singleton access:** `class current` — return the singleton instance (see `classState:` below)

**`classState:`** declares mutable class-level state — shared across all instances and accessible from class methods. Used for singletons:

```beamtalk
sealed Object subclass: MyRegistry
  classState: current = nil

  class current => self.current
  class current: instance => self.current := instance
```

`classState:` is distinct from `state:` (per-instance actor state) and `field:` (per-instance immutable data). It stores values at the class level, analogous to Smalltalk class variables.

**Class variables are per class and are not inherited.** A class method may touch only the class variables its own class declares with `classState:`. Reading or writing a variable that only an ancestor declares is a compile error in a class body and in a class-side extension (`Sub class >> m => ...`); a `ClassBuilder` `classMethods:` block only sees the variables its own `classVars:` declares, so it is rejected too (with the generic "cannot access instance field" message). Redeclare it in the subclass:

```beamtalk
Object subclass: Base
  classState: n = 0
  class bump => self.n := self.n + 1

Base subclass: Sub
  classState: n = 0      // Sub's own variable; without it `peek` does not compile
  class peek => self.n
```

Inherited methods follow the receiver, not the class that defines them: `Sub bump` (running `Base`'s `bump`) reads and writes `Sub`'s `n`, which is why `Sub` redeclares it. `Base`'s and `Sub`'s values are independent. `bump` itself compiles in `Base` either way, but without `Sub`'s own `classState: n`, `Sub bump` raises `undeclared_class_variable` at runtime. The runtime's declared-set check is own-only too (BT-3801) and covers reads and writes alike, in method bodies and in the class process's own `get_class_var` read: a write-first inherited method (`class reset => self.n := 0`) raises the same error instead of silently creating `n` in `Sub` (BT-3808), and the class-process read of an undeclared name replies with that error instead of `nil` (BT-3807). A `Sub classState: #n` send also gets the `Undefined class variable` warning, with a hint to redeclare. A class whose declared set cannot be read at runtime (a `ClassBuilder` class) stays lenient: an undeclared name reads `nil` and a write creates it. The out-of-band runtime-API assignment (`set_class_var`, used by bootstrap and tooling rather than by class-method code) is likewise unchecked and may create an undeclared name; later in-process accesses of that name then find it present and are not rejected. (ADR 0013 §1; ADR 0130.)

A class variable is read and written in place in the class's own process; see [Class variables and blocks](#class-variables-and-blocks-adr-0130) for what a block, a caught error and a call from another process see.

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

### Method Category Dividers (`// === Name ===`)

A `// === Name ===` comment placed between methods introduces a **method category** — a named group used across all surfaces: the LSP outline (`documentSymbol`), VS Code breadcrumbs/sticky-scroll, LSP folding ranges (`textDocument/foldingRange`), REPL `:help` grouped method listing, and MCP `docs` structured output.

```beamtalk
Value subclass: Point
  field: x :: Integer
  field: y :: Integer

  // === Accessing ===

  /// The X coordinate.
  x -> Integer => self.x

  /// The Y coordinate.
  y -> Integer => self.y

  // === Arithmetic ===

  /// Add two points.
  + other :: Point -> Point =>
    Point x: self.x + other x y: self.y + other y
```

**Format:** a leading `//` line comment whose trimmed text is `=+ <name> =+`, where the leading and trailing `=` runs are the same length (3 or more) and `<name>` is non-empty. Triple-slash (`///`) and block (`/* */`) comments are not recognized as dividers. A `=`-bordered comment that looks like a divider but doesn't parse as one (mismatched `=` run lengths, too-short runs, `///`/`/* */` instead of `//`) is flagged by `beamtalk lint` as a near-miss (BT-3240). A class with no dividers keeps the existing flat outline — no behavior change.

Methods following a divider belong to that category until the next divider or end of class. Instance-side and class-side methods are merged in true source order (not grouped by side). The REPL falls back to the flat alphabetical listing when grouping can't be trusted (no on-disk `.bt` file, no dividers, or unflushed `>>` patches). The stdlib uses this convention throughout (BT-2601, BT-2626, BT-3237, BT-3239, BT-3240).

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
- `{error, Reason}` becomes a `Result error:` holding an Exception that wraps `Reason` (read the raw term back with `error reason`, see "The raw error term" below)
- Bare `ok` atom becomes `Result ok: nil`
- Bare `error` atom becomes a `Result error:` holding an Exception whose `reason` is `nil`
- Tuples with 3+ elements, non-ok/error tuples, and non-tuple values pass through unchanged

**Inferred type of an FFI Result (BT-4023):** because the boundary always wraps the error reason, the spec reader types the error side as an Exception, never as the spec's reason type. `{ok, T} | {error, R}` and `ok | {error, R}` infer `Result(T, Exception)` and `Result(Nil, Exception)` whatever `R` is (an atom, a `posix()` enumeration, `term()`). The one refinement is a `#beamtalk_error{}` reason, which always wraps to an `Error` subclass and so infers `Result(T, Error)`. The rule lives in one place, `beamtalk_spec_reader:format_result_type/2`.

**Lone `ok`/`error` return specs (ADR 0121, type inference):** A non-union Erlang return spec of `-> ok.` or `-> error.` — including annotated forms like `-> Result :: ok.` — is now recognized as `Result(Nil, Exception)` or `Result(Dynamic, Exception)` respectively, matching the runtime coercion that `coerce_result/1` already applies. Previously these lone atoms fell through to `Symbol` in the type inference, creating a static/dynamic type mismatch.

**Opting out of Result conversion (BT-3836):** Result conversion is unconditional for FFI returns, which is wrong for a Beamtalk-owned module whose functions return *user data* (`Ets` hands back whatever value was stored, so a stored `#ok` must stay `#ok`). Such a module exports `beamtalk_ffi_raw_results/0` returning `true`, and `beamtalk_erlang_proxy` then passes its results through unchanged. Only opt out when none of the module's functions signal success or failure with `ok` / `{ok, _}` / `{error, _}` returns, since the opt-out removes the conversion for those too. The opt-out is runtime-only: the spec reader (ADR 0121) still infers `Result` for an `-> ok` or `ok | error` spec, so an opted-out module must not give its exported functions such return specs, or the inferred type and the runtime value will disagree.

**Atom-enum precedence (type inference):** When the spec reader maps a pure-atom union type containing atoms beyond `ok`/`error`, it infers a singleton union rather than applying ADR-0076 Result recognition. Unions whose atoms are a subset of `{ok, error}` and lone `ok`/`error` atoms infer as `Result`. For example, a spec of `text | json | xml` infers `#text | #json | #xml`, while `ok | error` or a bare `ok` infers `Result`.

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

**The raw error term (`Exception>>reason`, BT-3954).** `message` is a rendered string. To match on the underlying Erlang term, ask for `reason`. It returns the `R` of an FFI `{error, R}` (the exception that `Result error:` wraps) or the reason of a raw `error:`/`exit:`/`throw:` caught via `on:do:`, as an atom or tuple. FFI values pass through the usual boundary coercions first, so a charlist reason arrives as a `String`. When no underlying term was recorded, as for `Error signal:` and `self error:`, `reason` returns `nil`. Stdlib natives follow the same rule (BT-3970, BT-4236): a failure that comes from Erlang/OTP keeps its term after it is classified into a `kind`, so `(File readAll: "missing.txt") error` is a `#file_not_found` whose `reason` is `#enoent`, a failed `Json parse:` keeps the parser's term (e.g. `#unexpected_end`), and an invalid `Regex from:` pattern keeps `re:compile`'s `{ErrCharlist, Position}`, the same term `Erlang re compile:` answers (element 1 is OTP's charlist, a `List`, not a `String`). The one non-OTP case is `Result unwrap` on a plain error value, whose `reason` is that value. Only a `Result error:` produced at the FFI boundary holds an Exception. A `Result error: #sym` built in Beamtalk holds the value itself, so read it with `error`, not `error reason`:

```beamtalk
(Erlang string to_integer: "abc") ifError: [:e | e reason]
// => #no_integer
[Erlang erlang error: #econnreset] on: Exception do: [:e | e reason]
// => #econnreset
[#(1, "x") sum] on: TypeError do: [:e | e reason]
// => #badarith  (classified as type_error, original term still available)
[Error signal: "boom"] on: Error do: [:e | e reason]
// => nil
(File readAll: "missing.txt") error reason
// => #enoent  (classified as file_not_found, posix term still available)
```

**Carrying a reason: `signal:reason:` (BT-4159).** Beamtalk exceptions have no instance fields, so to attach machine-readable data when signalling, use the class-side `signal: message reason: term`. The term can be any value; `reason` answers it in `on:do:` (type `Object | Nil`, so narrow with `isKindOf:` or match), it survives propagation across processes, and uncaught reports print it as a `Reason:` line. There is no separate `error:reason:`; `Error signal: msg reason: term` is the equivalent.

```beamtalk
[Error signal: "closed" reason: #econnreset] on: Error do: [:e | e reason]
// => #econnreset
```

**Structured details (`Exception>>details`, BT-4174).** Natives that know more than the message says record it as structured details: `details` answers a `Dictionary` keyed by Symbol (empty when nothing was recorded), so read the value with `at:` instead of parsing the English `message`. For example, `Binary asString` on invalid UTF-8 answers `Result error:` holding a `parse_error` whose `#offset` is the byte offset of the first invalid sequence. A nested Beamtalk error stored in the details answers as an Exception and an absent (Erlang `undefined`) value as `nil`. Other values are the recorded terms as-is, without the FFI boundary coercions `reason` gets. Natives record text details (a File error's `#path`, `rename:to:`'s `#from`/`#to`) as `String`s, so `(File readAll: "missing.txt") error details at: #path` equals `"missing.txt"` (BT-4229).

```beamtalk
(Binary fromBytes: #(104, 101, 255)) asString error details at: #offset
// => 2
[Error signal: "boom"] on: Error do: [:e | e details]
// => #{}
```

**Re-raising unchanged: `pass` vs `signal` (BT-4057).** `e signal` on a caught exception raises a fresh `error` with a new stacktrace, so a caught `ExitError` or `ThrowError` comes back out as an error. `e pass` re-raises with the original Erlang class (`error`, `exit` or `throw`), reason and call sites, so an outer handler (or the process) sees the original failure: `exit: #normal` still ends the process normally. It re-raises the stacktrace captured when the exception was caught, with each frame's argument values reduced to its arity, as `e stackTrace` shows them. On an exception that was never caught (`Exception new pass`), `pass` behaves like `signal`.

**Where a class-side signal raises (BT-4115).** `Error signal: m`, `MyError signal`, `Error signal: m reason: r` and `Exception signalKind:class:selector:hint:` sent to a class *literal* raise in the calling process: the compiler sees that the class method resolves to one of `Exception`'s class-side signal primitives, which need nothing from the class but its name, and calls the primitive directly instead of going through the class's process. The exception is the same as a dynamic send (`cls signal: m`) raises. A subclass that overrides `class signal:` keeps the ordinary class send, so the override runs. The decision is made when the call site is compiled, as with other direct calls: a `class signal:` override added later by hot reload, or a class-side extension that shadows it, is not seen by call sites compiled before it until they are recompiled. `e stackTrace` builds its `StackFrame` objects each time it is read, not when the exception is caught, so class names are resolved at read time.

```beamtalk
[(Erlang erlang) exit: #shutdown] on: Exception do: [:e | e pass]  // exits with #shutdown
```

In an actor's `!` cast, `handleInfo:` or `continueWith:` step, an `exit` with reason `#normal` or `#shutdown` stops the actor with that raw reason (so `onExit:` receives `#normal`, not an `{Exception, Stacktrace}` tuple); every other failure crashes with `{Exception, Stacktrace}` as described in ADR 0134. `{shutdown, Term}` is deliberately excluded and still crashes the actor. Known limitation: hand-written Erlang (runtime-path) actors still normalise every failure to an error.

For an FFI Result `r`, `r error` is an Exception (the type checker infers `Result(T, Exception)`, see "Inferred type of an FFI Result" above) and `r error reason` is the raw Erlang term. Comparing the Exception itself to a Symbol, as in `r error =:= #closed`, is always false, so the compiler warns: ``comparing an Exception to a Symbol with `=:=` is always false; use `r error reason` ``, with the hint ``did you mean `r error reason =:= #closed`?``. The warning covers `=:=`, `==`, `=/=` and `/=` (the negated forms are always true) in either operand order, for any `Exception`-typed expression compared with a Symbol literal. For an `Error`-typed value the hint also suggests `kind`, since a structured Beamtalk error usually records no raw `reason`. Silence a deliberate comparison with `@expect type`.

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

### Tuple Literals (ADR 0136)

`{e1, e2, ...}` builds a raw Erlang tuple (class `Tuple`). It is the expression
form of the `{a, b}` tuple pattern, and the value is exactly the Erlang term:
symbols and `true`/`false`/`nil` are atoms, so `{#active, false}` is Erlang's
`{active, false}`. This makes FFI option lists read like the Erlang docs:

```beamtalk
(Erlang gen_tcp) listen: port with: #(#binary, {#active, false}, {#reuseaddr, true}, {#backlog, 1024})

pair := {#backlog, opts at: #backlog}   // elements are keyword-level expressions
{#ok, 1} =:= (Tuple withAll: #(#ok, 1)) // => true
{#a, 1} at: 2                           // => 1 (inferred Integer)
single := {#ok}                          // 1-tuple
empty := {}                              // 0-tuple
opts := #({#local, path}, {#nodelay, true},)  // trailing comma is allowed
```

- Elements are separated by `,` — Smalltalk's `{a. b}` brace-array form is a
  syntax error (`.` separates statements). A cascade or assignment inside an
  element needs parentheses, as in a list.
- At the start of a statement, `{a, b} := x` is still a destructuring
  assignment; `{a, b} size` is an expression. `{a, b} := {b, a}` swaps.
- A tuple literal infers the positional type `Tuple(T1, …, Tn)`, so `at:` with a
  literal index is typed (see above). `{}` infers as bare `Tuple`.
- The same positional type can be written as an annotation — parameter, return,
  `state:` and local — with any number of element types (`Tuple()` is bare
  `Tuple`). An annotated tuple behaves exactly like an inferred one:

  ```beamtalk
  marker -> Tuple(Symbol, Reference | Nil) => {#emitted, self.spanContext}

  countOf: entry :: Tuple(Symbol, Integer) -> Integer => entry at: 2   // inferred Integer
  ```

  `Tuple(A, B)` is assignable to bare `Tuple`, and a bare `Tuple` (or `Dynamic`)
  flows into `Tuple(A, B)` gradually. Element types are checked slot by slot, and
  an arity mismatch between two positional tuple types (`Tuple(A, B)` vs
  `Tuple(A, B, C)`) is a type error.
- **`{#ok, x}` is a `Tuple`, not a `Result`.** FFI `ok`/`error` conversion
  (ADR 0076) applies only to values *returned* from Erlang; arguments pass
  through unchanged. Use `Result ok:` / `Result error:` to build a `Result`.
- To interpolate a tuple literal in a string, add a space: `"pair: { {a, b} }"`.
  `"{{...}}"` is rejected as a mistaken literal-brace escape.
- The `#()` List literal takes arbitrary expressions as elements, unlike
  Smalltalk's literal-only arrays: `#(code)` with a bound variable, or
  `#((Point x: 1 y: 1))`, evaluates its elements.
- Tuple literals are not supported inside `when:` guards (nor are list literals).
- Caveat: a tuple whose first element is a runtime tag atom (`#beamtalk_object`,
  `#beamtalk_supervisor`) is misclassified by the runtime's class lookup. This
  is not new — `Tuple withAll:` behaves the same.

### FFI Type Stub Files (ADR 0075 Phase 2)

When the auto-extracted Erlang `-spec` types are missing or imprecise for a module, you can provide hand-written type stubs in a project's `stubs/` directory. Each `.bt` file there uses `declare native:` to declare type signatures for Erlang module functions:

```beamtalk
/// Type declarations for Erlang module `lists`.
declare native: lists
  reverse: list :: List(T) -> List(T)
  seq: from :: Integer to: to :: Integer -> List(Integer)
  member: elem :: T in: list :: List(T) -> Boolean
```

Stub signatures use the same syntax as Protocol method signatures — parameter names with optional `:: Type` annotations and an optional `-> ReturnType` — but never have a `=>` body. Unary functions use a bare name:

```beamtalk
declare native: erlang
  node -> Symbol
  self -> Pid
```

`declare` is a contextual keyword — it remains a legal identifier everywhere else.

**Build integration:** `beamtalk build` and `beamtalk lint` automatically scan the project's `stubs/` directory (no `beamtalk.toml` config needed). Stub declarations merge into the FFI type registry at **function/arity granularity**: a stub for `lists:reverse/1` overrides only that entry; `lists:nth/2`, not declared in the stub, still uses the auto-extracted type from the `.beam` file.

**Version-drift detection:** A stub function/arity that doesn't exist in the corresponding `.beam` module's real exports produces a warning anchored to the stub file, so stubs stay in sync with the Erlang modules they describe.

**Restrictions:**
- `declare native:` is only valid in `stubs/` — using it in `src/` is a hard compile error.
- Project-local `stubs/` only (resolution chain layer 1). Package-bundled and compiler-distribution stubs are a planned follow-up.

**Generating stubs:** Use `beamtalk generate stubs` to bootstrap stub files from `.beam` abstract code:

```bash
beamtalk generate stubs lists maps string
# Output: stubs/lists.bt, stubs/maps.bt, stubs/string.bt
```

### Loading Code into the Workspace

Beamtalk source files are loaded into the live workspace via `:load` or the `Workspace` class-side facade. Loaded classes are immediately available — existing actors pick up new code on next dispatch.

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

See [Workspace and Reflection API](#workspace-and-reflection-api) for the full `Workspace` facade interface.

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

### `late` Slots (ADR 0124)

**A nilable slot is not automatically a `late` candidate.** The question
`late` answers is narrow: is this slot acquired exactly once, by an
explicit lifecycle call, and correct for as long as it stays assigned —
never whether its type merely *permits* `nil`. Most nilable slots in real
Beamtalk code use `nil` to mean something else, and converting them to
`late` would be wrong. Exdura's timer manager resolves its dependencies by
name on **every** read, deliberately, so that a supervisor restart of
either dependency is picked up automatically instead of leaving a stale
reference behind:

```beamtalk
typed Actor subclass: TimerManager
  // (ADR 0079) instead of passing live refs, since neither exists yet
  // when the child spec is built. `engine`/`eventStore` resolve fresh by
  // name in that case, so a `rest_for_one` restart of either is picked up
  // automatically instead of leaving a nil/stale field.
  state: engine :: WorkflowEngine | Nil = nil
  state: eventStore :: EventStore | Nil = nil
```

An ordinary method (`currentEngine`/`currentEventStore`, on the sibling
`ExduraClient`) does the by-name lookup on every call. If `engine` were
converted to `late state: engine :: WorkflowEngine` and assigned once from
a resolved reference, a later restart of `WorkflowEngine` would leave the
stale pid behind, unnoticed — exactly the failure this slot exists to
avoid, and exactly what `late` cannot express: a `late` slot is
absent-or-assigned, never "assigned, but now to the wrong thing." So the
distinguishing question is never *"can this hold `nil`?"* — it is **"would
a stale, already-assigned value here be wrong?"** If yes, the slot stays
nilable and keeps resolving fresh in an ordinary method, the way
`currentEngine` does — that method stays a method, not a slot kind; `late`
has no "recompute on every read" mode and does not try to. Convert only a
slot in the other shape: acquired exactly once by an
explicit lifecycle call (open a subprocess, start a listener, attach a
resource) and correct for the rest of that resource's life — Symphony's
`CodexClient proc` below.

`late` is a declaration-level modifier on `state:`/`classState:` for a slot
in that second shape — legitimately unassigned after `initialize`, acquired
later by an explicit lifecycle call — instead of being declared nilable and
nil-checked at every read:

```beamtalk
typed Actor subclass: CodexClient
  late state: proc :: Subprocess   // not `proc :: Subprocess | Nil = nil`
  state: workspacePath :: String

  launch -> Nil =>
    self.proc := Subprocess open: "/bin/bash" args: #() dir: self.workspacePath
```

The modifier precedes the declaration keyword, matching the class-header
modifier position (`sealed typed Actor subclass: …`). It requires a type
annotation, and that type must not admit `Nil` and must have no default
value — a `late` slot has exactly two states, absent and assigned, and its
declared type is always non-nilable. **Five compile-time Errors govern
where `late` may appear:**

1. `late field:` on a `Value` — a Value is fully constructed and never
   reassigned (below).
2. `late state:`/`classState:` with no type annotation — the point of
   `late` is a non-nilable *declared* type, and an untyped slot already
   defaults to `nil` with no check.
3. `late state:`/`classState:` on a type that admits `nil` — a syntactic
   `T | Nil`, but also `Object` and `ProtoObject` (`nil` is an instance of
   both), `Dynamic` (which accepts every value unchecked), and any alias
   or protocol that admits `nil`. A slot whose type already admits `nil`
   is never in the state `late` describes.
4. `late state:`/`classState:` with a default value — a slot with a
   default is never unset.
5. `late state:` on a `native:` Actor, or `late state:`/`field:` on
   `Object` — already errors before `late` is even considered (ADR 0056
   forbids `state:` on a `native:` Actor; ADR 0067 gives `Object` no
   instance data at all), and `late` does not change that.

```beamtalk
late state: x            // error (2): requires a type annotation
late state: x :: T | Nil // error (3): drop `late` or narrow the type to exclude nil
late state: x :: Object  // error (3): `nil` is an `Object`, so the type admits nil
late state: x :: T = v   // error (4): a slot with a default is never unset
```

`late` is rejected on a Value's `field:` (error 1) — a Value is fully
constructed by `new`/`new:`/its keyword constructor and never reassigned,
so "assigned later" has no meaning; make the field optional
(`| Nil = nil`) or hold the resource in an Actor instead. Outside
declaration position `late` stays an ordinary identifier (`late := 1`) or
method name.

**Slot kinds and `fieldKinds`/`allFieldKinds`.** Every declared slot has a
*kind* — `#eager` (the default, no modifier) or `#late` — reflectable via
`Behaviour>>fieldKinds` (this class's own slots only) and `allFieldKinds`
(this class's plus every inherited slot, the flattened chain, mirroring
`fieldNames`/`allFieldNames`):

```beamtalk
CodexClient fieldKinds
// => #{#proc => #late, #workspacePath => #eager}
```

This draws the identical instance-vs-declared line `fieldNames`/
`allFieldNames` already draw (ADR 0035): **`Cls fieldKinds`/
`Cls allFieldKinds` answer the declared schema** — a `late` slot is always
listed there, assigned or not — while **an instance's `fieldNames` answers
the keys actually present in its state map**, so an unassigned `late` slot
is *not* listed there. That is why `c fieldNames` silently omitting `#proc`
and `c hasField: #proc` answering `false` read as the same fact seen from
two angles, not a contradiction: one reports declared shape, the other
reports assignment.

A `late` slot is excluded from the post-`initialize` definite-assignment
check. Reading it before assignment raises `UninitializedStateError` —
and "before assignment" means exactly that its key is absent from the
state map. Presence — not value — is assignment: if an open-world writer
(`spawnWith:` with a non-literal map, `fieldAt:put:`, `perform:`) has
planted a `nil` there without going through ordinary `self.slot :=`
assignment, the key is present and the read answers `nil`, exactly as
`hasField:` reports it assigned (BT-4061). `late` does not make a
non-nilable declared type sound against those writers; it makes the direct
read and `hasField:` agree on what "assigned" means. `self.slot` (or
`fieldAt: #slot`) is the raising direct read; `hasField: #slot` is the
non-raising presence test to ask first — `Object`'s `hasField:`/
`clearField:` (ADR 0035's `field`-prefixed reflection family) make a
`late` slot usable, not just declarable: `hasField:` answers whether it is
currently assigned, and `clearField:` returns it to unassigned so it can be
acquired again (`maps:remove`, not a sentinel value):

```beamtalk
typed Actor subclass: CodexClient
  late state: proc :: Subprocess
  state: workspacePath :: String

  launch -> Nil =>
    self.proc := Subprocess open: "/bin/bash" args: #() dir: self.workspacePath

  stopProcess -> Nil =>
    (self hasField: #proc)
      ifTrue: [self clearField: #proc]
    nil
```

`hasField:` and `clearField:` are declared on `Object`, so they answer for
every receiver, not only `late`-slot actors: `hasField:` on a `Value`
answers whether the named field is declared, and on a primitive (`Integer`,
`String`, …) always answers `false` — never raises. `clearField:` on a
`Value` is the same "Cannot modify slot on value type" error `fieldAt:put:`
already raises, since a `Value` is fully constructed and never reassigned.
The class-side counterpart (`classState:`) works the same way, both from
inside a class method (`self hasField:`/`self clearField:`) and from
outside (`SomeClass hasField: #x`/`SomeClass clearField: #x`).

**Supplying a `late` slot via `spawnWith:` counts as assigned** — this is
legitimate dependency injection (substituting a value for a slot normally
acquired by a lifecycle call), not a violation of "absent until assigned":
see the `spawnWith:`-injection clause beside the `spawnWith:` key-checking
rule in [Actor Message Passing](#actor-message-passing) below.
**Converting a slot between eager and `late` (or back) is a shape
change and bumps `shapeVersion:`** — see
[Chain and reconcile semantics](#chain-and-reconcile-semantics) for the
reconcile-table row and the reload-time tooling finding that flags a flip
landing without a version bump.

**Unguarded late-slot reads in lifecycle hooks.** A direct `self.slot` read
of a `late` slot inside `terminate:` or `handleInfo:` (or one level of
self-send from either) that isn't guarded by `(self hasField: #slot)
ifTrue: [...]` produces a compile-time warning (`unguarded-late-read`
diagnostic category). In `terminate:` a raised error is swallowed
(`terminate/2`'s dispatch is `try`-wrapped), so an `UninitializedStateError`
from an unguarded read there never surfaces and the cleanup is silently
skipped. In `handleInfo:` the error crashes the server (ADR 0134), so an
unguarded read turns an ordinary message into a crash and restart. The
fix is the `hasField:` guard shown above, not a suppression — like
`definite-assignment`, there is no site-level `@expect` category for this
diagnostic. Per-project severity is configurable via the `[diagnostics]`
table (`unguarded-late-read` key — see the
[Package Management guide](beamtalk-packages.md#diagnostics-section)).

### Definite Assignment (ADR 0124)

The compiler checks, at a class's construction site, that every declared,
typed, non-nilable, no-default, non-`late` slot is guaranteed a real value —
the same predicate ADR 0078's runtime backstop already used for Actors, now
also checked statically, and now for Values too.

**The predicate.** A `state:`/`field:` slot *requires definite assignment*
when it carries a type annotation, has no default value, its declared type
does not admit `Nil` (resolving [aliases](#named-type-aliases-type-declarations-adr-0108)
and intersection/negation operators), and it is not declared `late`:

```beamtalk
state: count :: Integer            // requires assignment
state: label :: String | Nil       // no — Nil is a valid value
state: count :: Integer = 0        // no — has a default
state: count                       // no — untyped, defaults to nil
late state: proc :: Subprocess     // no — declared assigned later (see above)
```

**Where it reports: the construction site, not the declaration.** A
declaration alone is not enough evidence — the slot might be supplied at
every real call site — so the diagnostic fires on `Cls spawn` / `Cls new`,
or a literal-map `Cls spawnWith: #{...}` / `Cls new: #{...}` that omits the
slot; only a *literal* map is inspected, the same boundary the `spawnWith:`
key-checking rule above uses:

```beamtalk
typed Actor subclass: Connection
  state: socket :: Socket
  state: retries :: Integer = 0

Connection spawn
//         ^ warning: `Connection` declares `socket :: Socket` with no
//           default, and no `initialize` in its chain assigns it.
//           `Connection spawn` supplies no `socket`, so it may raise
//           `UninitializedStateError`
```

**The four fixes**, named in the diagnostic's hint: supply the slot at the
construction site, give it a default value, widen its type to admit `Nil`,
or declare it `late` (Actors only, since `late field:` is rejected on a
Value — see above).

**The Actor/Value asymmetry.** For an `Actor subclass:`, this diagnostic is
an early warning in front of ADR 0078's existing runtime check — a spawn it
flags was already going to raise `UninitializedStateError`, just later, at
spawn time instead of at compile time. **For a `Value subclass:`, it is the
*only* check there is** — Values have no `initialize` and no
post-construction runtime validation, so `StoredSnapshot new` on a
typed-no-default field silently yields an instance holding `nil` behind a
declared non-nilable type, with nothing to catch it at runtime. That makes
the default `Warning` severity a real gap, not merely an advisory one, for a
Value-heavy codebase — see escalation below.

**The deserialization limit.** A Value built by deserialization has no
construction site at all:

```beamtalk
typed Value subclass: StoredSnapshot
  field: state :: ReplaySnapshot   // typed, no default

  class fromBinary: content :: Binary -> StoredSnapshot =>
    result :: StoredSnapshot := Binary deserialize: content
    result
```

`Binary deserialize:` is not `new`/`new:`, so the check never inspects this
site — the local type annotation on `result` is an assertion at a
type-erasure boundary, not something the checker can verify, and nothing is
reported here (consistent with the open-world silence of
[ADR 0100](ADR/0100-open-world-diagnostic-policy.md) where knowledge is
absent). A class in this position should default the field or widen it to
`| Nil`.

**Severity and `@expect`.** The diagnostic is `Warning` when the checker's
knowledge is complete — the ancestor chain is fully known, no ancestor is
`native:`, and (for Actors) no `fieldAt:put:`/`perform:` write anywhere in
the class is invisible to the must-analysis — and `Hint` otherwise. There is
**no dedicated `@expect` category** for this diagnostic, by design: `late`
is the language construct that opts a slot out, not an annotation that
silences a warning about it. A test that deliberately constructs an
instance with the slot left unassigned suppresses the diagnostic with
`@expect all`.

**Escalating to `Error`.** Per-project severity is
[ADR 0100](ADR/0100-open-world-diagnostic-policy.md) Rule 3's `[diagnostics]`
table (`definite-assignment = "error"` in `beamtalk.toml` — see the
[Package Management guide](beamtalk-packages.md#diagnostics-section)).
Because a Value's `Warning` has no runtime backstop, a Value-heavy codebase
has more reason to make that escalation than an Actor-heavy one — provided
the bare-`new`-override check is in place: a class that defines its own
`class new`/`new:` already exempts that construction site, the same as any
other override of the auto-generated constructor.

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

// Block parameter annotations — accepted by the parser and preserved by
// `beamtalk fmt`. A union (or `&`/`\` chain) needs grouping parens so its
// `|` doesn't swallow the block's own `|` parameter separator.
#(1, 2, 3) collect: [:x :: Integer | x + 1]
self run: [:x :: (Integer | String) | x]
```

### Current Semantics

- Type mismatch diagnostics are warnings, never compile-stopping errors.
- Invalid annotation forms (e.g., `Self` in parameter position) are errors.
- `typed` classes require parameter/return annotations on non-primitive methods.
- Data annotations (`field: x :: Integer = 0` on Value, `state: x :: Integer = 0` on Actor) are checked for defaults and assignments. When a type annotation is present, the default value is optional (`field: x :: Integer` / `state: x :: Integer`) — the type annotation is the contract, and `nil` is used internally.
- Local variable annotations (`name :: Type := expr`) declare the binding's type at type-erasure boundaries. See [Local Variable Type Annotations](#local-variable-type-annotations) below.
- Complex annotations (e.g., unions/generics) are parsed and accepted; deeper checking is phased in.
- Block parameter annotations (`[:x :: Integer | ...]`) type the parameter and win over any inferred type (BT-4102); an annotation unrelated to the expected parameter type warns. Without an annotation, parameter types are inferred from the enclosing message signature, from a method's declared `-> Block(P, R)` return type (block literal as the tail expression or a top-level `^`), or from an annotated local (`f :: Block(P, R) := [:x | ...]`). Block literals inside `#{...}`/`#(...)` literals get no push-down; annotate their parameters. A method-local or class-level type variable in a method or block parameter annotation (`foo: x :: E`, `[:y :: E | ...]`) resolves through the enclosing method's substitution, which inside the method body is the identity: `E` is a rigid placeholder, the same type a `-> E` return annotation names, so it flows between parameters, block parameters and the return type without warnings. It is never `Dynamic`, never the callee's binding for a same-named variable, and is treated as compatible with any type in mismatch checks (BT-4210).
- Override and protocol-conformance return types are checked: a subclass method whose return type is incompatible with the parent's declared return type now warns, and a `uses:` provision whose return type is incompatible with the protocol's declaration now warns (BT-4175). Parameter types were already checked.
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

**Named type alias emission.** When a module declares `type` aliases (ADR 0108), codegen emits matching named Erlang `-type` attributes into the compiled module. Method annotations referencing an alias name emit `user_type` references in their `-spec` attributes instead of `any()`:

```beamtalk
type Timeout = Integer | #infinity

Actor subclass: Worker
  run: t :: Timeout => // ...
```

Generates:

```erlang
-type 'Timeout'() :: integer() | 'infinity'.
-spec run('Timeout'()) -> any().
```

(The bare `('Timeout'())` form omits the parameter name — both it and the named form `(T :: 'Timeout'())` are valid Erlang specs; codegen emits the bare form, matching typical `erlc` output.)

Cross-package alias references (an annotation referencing an alias exported by a dependency) are not yet resolved in codegen — they still fall through to `any()`.

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

A protocol body may also carry **provided** methods — a signature with `=>` and a body, flattened into every class that names the protocol in a `uses:` line. That is a **trait**, covered in full in [§ Traits](#traits-adr-0127) below. The stdlib's own `Comparable` (`stdlib/src/comparable.bt`) is one: it requires only `<`, and provides `>`, `<=`, `>=`, `between:and:`, `min:`, `max:`.

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

## Traits (ADR 0127)

A **trait** is a `Protocol define:` body that carries at least one **provided**
method — a signature with `=>` and a body, alongside the protocol's ordinary
required signatures (no `=>`). There is no separate `Trait define:` keyword;
a trait is just a protocol with provisions, and it is used exactly like any
other protocol.

```beamtalk
// stdlib/src/comparable.bt
Protocol define: Comparable
  /// Required — the using class must implement this.
  < other :: Self -> Boolean

  // Provided — derived from `<`, flattened into every user.
  > other :: Self -> Boolean => other < self
  <= other :: Self -> Boolean => (other < self) not
  >= other :: Self -> Boolean => (self < other) not
  between: min :: Self and: max :: Self -> Boolean =>
    (self >= min) and: [self <= max]
  min: other :: Self -> Self => (self < other) ifTrue: [self] ifFalse: [other]
  max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]
```

A class receives a trait's provisions by naming it in a `uses:` line at the
top of its body, immediately after the header and before any `state:`/
`field:`/`classState:` declaration or method:

```beamtalk
sealed typed Value subclass: DateTime native: beamtalk_datetime
  uses: Comparable

  < other :: DateTime -> Boolean => self delegate
  // `>`, `<=`, `>=` stay hand-written (each its own FFI call) — DateTime's
  // own methods win over Comparable's provisions (see Precedence below).
  // Gained from Comparable: `between:and:`, `min:`, `max:`.
```

`Self` in a trait's signature is substituted with the using class at
flattening time, so `Comparable>>max: other :: Self -> Self` becomes
`DateTime>>max: other :: DateTime -> DateTime` on `DateTime` — exactly the
signature a hand-written method would declare.

### Composing more than one trait, and `excluding:`/`overriding:`

The full grammar of a `uses:` line:

```
uses: [package@]ProtocolName[(TypeArgs)] [excluding: #(#sel, …)] [overriding: #(#sel, …)]
```

A bare `uses: ProtocolName` resolves by first-definition-wins: the current file, then the rest of the package, then its dependencies, and last the stdlib's own traits (`Comparable`, `Enumerable`) — so any package can `uses: Enumerable(E)` without declaring a dependency, and a package's own same-named protocol takes precedence. A qualified `uses: pkg@ProtocolName` resolves only against that package's protocols, so two dependencies that each export a same-named protocol can be composed without ambiguity.

One trait per `uses:` line — several traits are several lines:

```beamtalk
typed Actor subclass: WorkerPool
  uses: Enumerable(Worker)
  state: workers :: List(Worker) = #()

  elements -> List(Worker) => self.workers
```

`excluding:` drops one or more of a trait's provisions (Pharo's `-`), keeping
the class's own or inherited version instead:

```beamtalk
typed Value subclass: SupervisionTree
  // This class's own `do:` answers `self` (chainable), not `Nil` —
  // `Enumerable`'s provision is dropped before it can conflict.
  uses: Enumerable(SupervisionNode) excluding: #(#do:)
```

`overriding:` acknowledges that a trait's provision replaces a method the
class would otherwise **inherit** from its superclass — required because a
trait provision never silently overrides an inherited method:

```beamtalk
Record subclass: AuditRecord
  uses: Describable overriding: #(#printString)
```

Leaving out `overriding:` there is a **compile error** naming both sides and
both fixes:

```
error: Describable provides `printString`, which AuditRecord would otherwise
       inherit from Record.
  hint: to use Describable's version, write
          uses: Describable overriding: #(#printString)
        to keep Record's version, write
          uses: Describable excluding: #(#printString)
```

`printString` and `displayString` inherited from `Object`/`Value` are
exempt (they're cosmetic defaults every class is expected to replace); every
other inherited selector — including `equals:`/`hash` — needs `overriding:`.

Two traits providing the same selector to one class is a compile error
unless the class defines that selector itself or one `uses:` line excludes
it:

```beamtalk
Value subclass: Report
  uses: Labelled
  uses: Describable
  // error: `printString` is provided by both Labelled and Describable in Report.
  //   Define `printString` in Report, or exclude one:
  //     uses: Describable excluding: #(#printString)
```

### Precedence

Flattening applies, highest precedence first: **class body → trait provision
→ inherited**. A class's own method always wins over a trait's provision,
and a trait's provision — once acknowledged where needed (`overriding:`
above) — wins over what the class would otherwise inherit. This is exactly
what a hand-written method in the class body would mean; after flattening,
there is nothing trait-shaped left for the type checker, dispatch, `super`,
or hot reload to know about.

### Requirements

Every required selector of a used trait must resolve on the class — from the
class body, another used trait's provisions, or the superclass chain —
or it's a compile error naming the missing selector:

```beamtalk
Value subclass: Version
  uses: Comparable
  field: major = 0
// error: Version uses Comparable but does not implement required `<`
//   hint: Comparable requires `< other :: Self -> Boolean`
```

`uses:` on a protocol with **no** provisions is a hint, not an error or
warning — it only checks the protocol's requirements at the class
definition ("assert conformance here"); conformance itself stays structural
(ADR 0068).

### Which mechanism to use

Beamtalk has three ways to share behaviour, and each answers a different
question:

| Mechanism | Answers | Example |
|---|---|---|
| **Superclass** (`X subclass: Y`) | "What *is* this?" — shares representation, state, and a place in the class hierarchy | `Integer`/`Float` are `Number`s |
| **Protocol with provisions** (`uses:`) | "What *capability* does this have?" — derived from a few required methods, usable across unrelated classes and class kinds | `DateTime`, `Duration`, `Uuid`, `String` all `uses: Comparable` despite sharing no common ancestor below `Object` |
| **Extension method** (`Class >> sel => body`) | "This one class I don't own needs one more method" | A one-off addition to a stdlib or dependency class |

A superclass spends the class's *one* superclass slot and claims a kinship
("`DateTime` is-a `Magnitude`") that may not be true — `DateTime`, a
`Duration`, and a `Uuid` share only that they can be ordered, not what they
*are*. A protocol with provisions gives capabilities their own home and
works across the class-kind wall (an `Actor` can use a trait a `Value` also
uses, `Enumerable` above being one). Reach for an extension method only for
a single class, typically one you don't own — it isn't reusable the way a
trait is (two extensions repeating the same body on two classes is exactly
the duplication traits remove).

### Traits across the class kinds

An **Actor** wanting enumeration over what it holds needs its trait's
provisions to work by **snapshot**, not an internal iterator — a block
passed through a self-send to an actor loses `^` and captured-local writes
(BT-3580). `Enumerable(E)` (`stdlib/src/enumerable.bt`) is written this way:
its one requirement is `elements -> List(E)`, and every provision
(`size`, `isEmpty`, `isNotEmpty`, `do:`, `inject:into:`, `select:`,
`collect:`, `detect:ifNone:`, `anySatisfy:`, `count:`) forwards to the
matching `List` primitive over that snapshot — no block ever crosses a
self-send, so every provision runs in the *caller's* process regardless of
whether the receiver is a `Value` or an `Actor`.

`Collection` does not `uses: Enumerable` itself (its own `select:`/
`collect:` already answer `Self` via the species pattern, richer than
`Enumerable`'s plain `List(E)`), but gains `elements -> List(E) => self
asList` so every `List`, `Set`, `Array`, … **conforms** to `Enumerable`
structurally, with no `uses:` needed.

Provisions are **instance-side only** in v1 (`class sel … =>` in a protocol
is rejected). An `Object subclass:` (no instances) that composes a trait is
not an error, but gets nothing useful from it: the provisions flatten onto a
class that will never be instantiated. Class-side provisions are a post-v1
item, tracked in BT-3595.

### Reflection

```beamtalk
DateTime usedProtocols                        // => [#Comparable]
DateTime usesProtocol: #Comparable             // => true
DateTime usesProtocol: #Printable              // => false
Protocol providedMethods: #Comparable          // => [#between:and:, #min:, #max:, #>, #<=, #>=]
Protocol usersOf: #Comparable                  // => [DateTime, Duration, Uuid, String]
(DateTime >> #between:and:) origin             // => Comparable
(DateTime >> #<) origin                        // => nil  (class's own method)
```

`usedProtocols` is distinct from `protocols` (ADR 0068's structural
conformance query, § Runtime Protocol Queries above): a class can
structurally conform to a protocol it never `uses:`, and `usedProtocols`
answers only what it actually composes via `uses:`. `allUsedProtocols`
walks the superclass chain too. `CompiledMethod >> origin` names the
protocol a method was flattened from, or `nil` for a method the class wrote
itself.

Editing a loaded protocol file's provisions and reloading it
(`ProtocolName reload` / `:reload ProtocolName`) atomically recompiles and
hot-swaps every loaded, source-backed user together — all-or-nothing: if any
user's recompile fails against the edited protocol, nothing installs and the
error names the failing user (ADR 0127 §11).

At the REPL, `Describable >> summary => …` adds or replaces a provision in the
protocol's own source, and `Describable removeSelector: #summary` removes one.
Both run through the same all-or-nothing re-expansion of every user, are
recorded in the ChangeLog against the protocol (so `Workspace flush` writes the
protocol file), and are refused for stdlib protocols. This is a live-image
edit only: put a provision in a `.bt` file by writing it inside the
`Protocol define:` body.

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
| `show: value` | Write `value` to `Transcript` (returns `self`) |
| `showCr: value` | Write `value` to `Transcript` followed by newline (returns `self`) |

`show:` and `showCr:` are convenience methods on `Object` that delegate to the `Transcript` class-side facade (ADR 0129). Inside an interactive workspace the output goes to the REPL's transcript; everywhere else (`beamtalk run`, `beamtalk test`, releases) it is one plain `Logger` notice per call in the `[beamtalk, user, transcript]` domain, so they are always safe to call and return `self`. Programs should prefer `Logger` or `Console`; `Transcript recent` and `Transcript clear` are workspace-only and raise `no_workspace` elsewhere:

```beamtalk
// Cascaded output
Transcript show: "Hello"; cr; show: "World"

// show:/showCr: on any object — nil-safe
42 show: "value: "
42 showCr: "hello world"
```

`Transcript show:` accepts any `Printable` value, so custom classes that conform to `Printable` work directly with `Transcript show:` without manual `asString` conversion.

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

**`evaluate:` is values-only.** On a `#value` cursor, `i evaluate: "self x + self y"` compiles and runs the expression with `self` bound to the inspected value, returning a `Result` — failures are `Result error:`, never a raise. The one exception is a `^` (non-local return) from a block captured elsewhere and run by the expression: it is control flow, not a failure, so `evaluate:` re-raises it to its home method (BT-3735). Class-variable writes made before the `^` are kept, as for any `^` that crosses a protected region (ADR 0130 §4); an error exit still discards them. If the home method has already returned (a stale `^`), nothing catches it and the enclosing eval/dispatch boundary reports it as a structured error. On an `#actor` cursor it returns `Result error:` with kind `#actor_eval_unsupported` (actor evaluate-in-context is a deferred §7 follow-up). Live updates are **poll-only**: re-issue `refresh`.

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

### Named Type Aliases (`type` Declarations) (ADR 0108)

A `type` declaration names an existing type annotation for reuse across signatures:

```beamtalk
/// How a supervised child restarts after exit.
type RestartStrategy = #temporary | #transient | #permanent

type OptionalString = String | Nil
type JsonKey = String | Symbol
type Timeout = Integer | #infinity
type PublicTag = Symbol \ (#reserved | #internal)
type PrintableInt = Integer & Printable
```

**Transparent, not nominal.** `RestartStrategy` and `#temporary | #transient | #permanent` are the same type — a reference to the alias expands to its right-hand-side annotation everywhere type annotations are resolved: parameter types, return types, typed locals, both sides of `|`/`\`/`&`, [narrowing](#control-flow-narrowing), and `match:`/`matchExhaustive:` exhaustiveness. Two aliases with the same expansion are interchangeable, and there is no runtime tag distinguishing them — a `RestartStrategy` value *is* the atom `temporary`, exactly the value a hand-written union annotation would carry. A `type` declaration is erased entirely at compile time: no BEAM module, no runtime object, no live process.

```beamtalk
type Direction = #north | #south | #east | #west

Object subclass: Compass
  heading: h :: Direction =>
    h matchExhaustive: [
      #north -> 0;
      #south -> 180;
      #east  -> 90;
      #west  -> 270
    ]

  defaultHeading -> Direction => #north
```

**`type` is a contextual keyword.** It is recognized only at top-level declaration position via a three-token lookahead (`type` + uppercase identifier + `=`). It remains a legal identifier everywhere else — `type := 5`, `foo type`, `type printString` are all unaffected.

**Exhaustiveness is inherited, not new.** Because an alias reference expands to its structural annotation before any checker runs, the [advisory `match:` warning and asserted `matchExhaustive:` error](#match-expression) apply exactly as they would to the spelled-out union — no new exhaustiveness machinery. Adding a member to the alias declaration makes every `matchExhaustive:` over it non-exhaustive in one edit:

```beamtalk
heading match: [
  #north -> 0;
  #south -> 180;
  #east  -> 90
]
// ⚠ Warning: non-exhaustive match: `#west` is not handled (residual type: `#west`)

heading matchExhaustive: [
  #north -> 0;
  #south -> 180;
  #east  -> 90
]
// ⛔ Error: non-exhaustive matchExhaustive: `#west` is not handled (residual type: `#west`)
```

A value outside the named union is flagged the same way an inline union would be:

```beamtalk
p :: RestartStrategy := #premanent
// ⚠ Warning: Type mismatch: declared as RestartStrategy
//    (#temporary | #transient | #permanent), got #premanent
```

**Exported by default, like classes and protocols.** A `type` declaration is visible to any package that depends on the declaring one, unless marked `internal`:

```beamtalk
internal type ParserState = Integer | String
```

An `internal` alias is package-private, following the same model as [class- and method-level `internal`](#visibility-and-access-control-adr-0071) (ADR 0071): usable within the declaring package, a compile error to reference from outside it. Because aliases are transparent, referencing an internal alias from a *public* signature is also a compile error even within the same package (the same "leaked visibility" rule 0071 already applies to internal classes) — and so is reaching one through the expansion of a public alias, since a public alias has no "internal signature" escape hatch of its own:

```beamtalk
internal type Priv = Integer
type Pub = Priv | String
// ⛔ Error: Internal type alias 'Priv' appears in the expansion of
//    public type alias 'Pub'
```

**Constraints:**

- Single-letter names are rejected at parse time — reserved for [ADR 0068's implicit method-local type parameters](#parametric-types--generics-adr-0068): `type T = Integer | Nil` is a declaration error.
- Alias names share the class/protocol namespace: declaring an alias with the same name as an existing class or protocol (or vice versa) is a compile error.
- The RHS accepts any type annotation: unions (`|`), singletons (`#foo`), generics (`List(Integer)`), difference (`\`), intersection (`&`), and combinations of these.
- Aliases may reference other aliases; a reference cycle — including self-reference and cycles that span multiple files — is a compile error at declaration time (`type Ab = Bc | Integer` / `type Bc = Ab | Symbol` → `type alias cycle: 'Ab' → 'Bc' → 'Ab'`). Cross-file cycles (where each half of the cycle is declared in a separate file) are detected when pre-loaded aliases are merged with module-local aliases.
- Parametric aliases (`type Option(T) = T | Nil`) and recursive aliases (`type JsonValue = ... | List(JsonValue)`) are **not yet supported** — both need deferred (lazy) resolution in place of the eager expansion aliases use today. `type Name(...) = ...` is reserved syntax for the future parametric form.
- Doc comments (`///`) above a `type` declaration are preserved and shown by `:help <Alias>` and LSP hover.

**Erlang interop.** Codegen emits a named Erlang `-type` attribute for each alias declared in a module, and method `-spec` attributes reference it via `user_type` instead of expanding to `any()`. Erlang/Elixir consumers of a compiled Beamtalk module see idiomatic Dialyzer types (e.g. `-type 'Timeout'() :: integer() | 'infinity'.`). See [Dialyzer Spec Generation](#dialyzer-spec-generation) for details.

**Tooling.** The LSP offers completions, go-to-definition, find-references, and hover for alias names in `.bt` files. In the REPL, `type` declarations are accepted as input and persist for the rest of the session; declaring `type Direction = ...` at the prompt echoes the declared name (`=> Direction`), the same convention a class declaration echoes (`Actor subclass: Counter` → `Counter`); `:help <Alias>` renders the declaration, its expansion, and any doc comment. Stdlib-declared type aliases (e.g. `RestartStrategy`, `Timeout`) are available automatically in a fresh REPL session — `::` typed locals referencing them resolve without a prior declaration, and `:help RestartStrategy` shows `Declared in: stdlib`. A session-declared `type` with the same name shadows the stdlib alias. The System Browser and the VS Code Workspace Explorer sidebar list declared aliases in a "Type Aliases" section alongside Classes and Protocols. See [`docs/development/surface-parity.md`](development/surface-parity.md) for the full cross-surface matrix.

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
| `(a isNil or: [b isNil]) ifTrue: [^...]` | every tested var is non-nil after the statement | Rest of method |
| `(a isNil or: [b isNil]) ifFalse: [...]` | every tested var is non-nil in false block | False block |
| `(a notNil and: [b notNil]) ifFalse: [^...]` | every tested var is non-nil after the statement | Rest of method |
| `(a notNil and: [b notNil]) ifTrue: [...]` | every tested var is non-nil in true block | True block |
| `x =:= #sym ifTrue: [^...]` | `x` is `T \ #sym` after the statement | Rest of method |
| `x == #sym ifTrue: [^...]` | `x` is `T \ #sym` after the statement | Rest of method |
| `x respondsTo: #sel ifFalse: [^...]` | `x` narrows post-guard | Rest of method |

The diverging-guard pattern (`isNil ifTrue: [self error: "..."]`) recognises any block whose body infers as `Never` — including calls to `error:`, `notImplemented`, or any `-> Never` method — not just non-local returns (`^`). Narrowing also works on `self.field` reads: inside `self.field isNil ifFalse: [...]`, the field narrows to non-nil within the block.

**`notNil and:` (BT-2872):** `x notNil and: [...]` narrows `x` to non-nil for the whole block argument — not just where `x` is a further send's receiver, but in any nested position, including as an argument to a binary send inside a further-nested block (`local notNil and: [local > 0 and: [5 >= local]]` narrows `local` inside `5 >= local` too). The narrowing does not survive past the block — a later unguarded use of `x` after the `and:` send is unaffected.

**Combined nil-guards (BT-4107):** an `or:`-chain of `isNil` tests narrows *every* tested variable — `(a isNil or: [b isNil]) ifTrue: [^nil]` proves both `a` and `b` non-nil afterwards (`or:` is false only when both disjuncts are false), and both are non-nil inside the `ifFalse:` arm. Dually, `(a notNil and: [b notNil]) ifFalse: [^nil]` proves both non-nil afterwards, and both are non-nil inside the `ifTrue:` arm. The complementary directions are disjunctive and never narrow: an `or:` guard that holds (or an `and:` guard that fails) only proves *some* variable is nil, not which. As a consequence, a lone `a notNil` test narrows too (`ifFalse: [^...]` post-guard, `ifTrue:` in-branch).

**`isKindOf:` guard-and-return (BT-2825):** the same diverging-guard treatment `isNil` gets also applies to `isKindOf:` — `(x isKindOf: Foo) ifFalse: [^default]` proves `x` is `Foo` for the rest of the method, and `(x isKindOf: Foo) ifTrue: [^default]` proves `x` is `T \ Foo`. This also closes a related gap: when `x`'s declared type is a Protocol (e.g. `Printable`) and `Foo` is a concrete class, the narrowed type collapses to the bare `Foo` — a runtime `isKindOf: Foo` check is a stronger proof than the protocol annotation, so the narrowed value is assignable to a `Foo`-typed local without an `@expect type` escape hatch.

**Singleton-equality guard-and-return (BT-3369):** the same diverging-guard treatment extends to singleton-equality checks — `x == #undefined ifTrue: [^false]` proves `x` is not `#undefined` for the rest of the method, narrowing to `T \ #undefined`. Both `==` and `=:=` are accepted (they are provably equivalent for atom/symbol comparisons). `respondsTo:` guards also carry forward: `(x respondsTo: #foo) ifFalse: [^default]` narrows `x` post-guard.

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

**Local variable mutations** work in the blocks of control-flow messages (the table below, conditionals, `on:do:`/`ensure:`, `Result tryDo:`), written directly as the message's argument, in every method kind and in the REPL, including when the construct is an operand of a larger expression. A block that assigns a captured local and escapes (stored, returned, or passed to any other method, the actor's own methods included) is a compile error ([ADR 0133](ADR/0133-scope-of-captured-variable-mutation-in-blocks.md), which amends ADR 0041's "local mutations work in all blocks"; see [What Works and What Doesn't](#what-works-and-what-doesnt)). The Tier 2 protocol remains inside the stdlib's own collection messages for dynamic receivers. **Field mutations** (`self.x :=`) in an actor instance method work in every block, stored or not ([Actor Blocks See Live State](#actor-blocks-see-live-state)); class-variable writes in a class method likewise work in every block ([ADR 0130](ADR/0130-class-variables-live-in-the-class-process.md)). In value-type methods they require control-flow context and are a compile error in stored closures.

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

**The rule** ([ADR 0133](ADR/0133-scope-of-captured-variable-mutation-in-blocks.md)): a block may assign a variable it captured only where the compiler lowers the block in place, that is, as the block literal written directly as the argument (or, for `value` and the loops, the receiver) of a control construct or a compiler-known collection message. A block that **escapes**, by being stored in a variable, returned, placed in a collection literal, or passed to any other method (a user-defined method, a class or actor, a stdlib method outside the known list), may read captured variables but not assign them. The compiler reports the assignment, names how the block escapes, and lists the messages that do thread assignments. The idiom is to have the block answer the new value and assign it where the block is called:

```beamtalk
// ❌ ERROR: `t` is assigned in a block passed to `twice:`, which is not a
//    construct the compiler threads it through
t := 0
Twice twice: [t := t + 1]

// ✅ The block answers the value; the caller assigns it
t := Twice twice: [t + 1]

// ✅ A construct the compiler threads
2 timesRepeat: [t := t + 1]
```

This departs from Pharo, where `Twice twice: [t := t + 1]` answers 2: it relies on a heap shared between a block and its home method, which the BEAM does not have (see `docs/beamtalk-principles.md` §4). Reads of captured variables, and `^` from any block, are unaffected. The same diagnostic is reported by `beamtalk build`, the REPL, the LSP and MCP.

**Your own collection classes.** The collection messages above (`do:`, `inject:into:`, `collect:`, `select:`, `detect:ifNone:`, `anySatisfy:`, …) thread a writing block literal for any receiver that implements `do:`, including your own class or actor: the compiler runs the block in place as a fold over the receiver's `do:`, whether that `do:` forwards to an inner `List` or walks by hand, and whether the other messages are the stdlib's or absent (ADR 0133, BT-3903). The exception is an actor's `self do:` (or `super do:`, or any of these messages) from inside its own method: that is a message to the actor's own method, so the block escapes and writing a captured local in it is a compile error (BT-3918). Every message but `do:` is checked first, at run time, against the receiver's class, and the write threads only when the class defines no collection message of its own: the sent message must resolve to the stdlib's own method, or to a provision of a stdlib trait such as `Enumerable` (judged by where the method comes from, not by the class that `uses:` it), **and** none of the collection messages above except `do:` may be a method outside the stdlib. The stdlib's `collect:`, `select:`, `count:`, `eachWithIndex:` and `do:separatedBy:` run through `self inject:into:` (and `reject:` through `select:`), so a class with its own `inject:into:` would have that override run by the plain send but skipped by the in-place fold. Any such override in the class or a package-local trait, or an extension method for the message actually sent, makes a writing send of any of these messages raise `stateful_block_dispatch` rather than silently running the fold over `do:` in its place (known gap: an extension registered at run time that overrides a *different* collection message is not yet detected, [BT-3951](https://linear.app/beamtalk/issue/BT-3951)); a receiver that does not respond to the message raises `does_not_understand`, as the plain send does (BT-3916). To thread a write through such a class, `uses: Enumerable` (the stdlib's provisions thread), define only `do:`, or compute the value without assigning from the block. A block passed to a message of your own (`orders sumOf: [:o | total := ...]`) is not covered; return the value from the block instead.

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

**A block that assigns a captured local may not escape** ([ADR 0133](ADR/0133-scope-of-captured-variable-mutation-in-blocks.md), which made ADR 0131 §6's error the permanent rule). A block literal that assigns a local of the enclosing method (a *Tier 2 block value*, ADR 0041) may only be written where the compiler lowers it in place: as the block of a control-flow message (`whileTrue:`, `do:`, `inject:into:`, the conditionals, `on:do:`/`ensure:`, `Result tryDo:`, `[...] value`, and the handlers of a `Dictionary`'s `at:ifAbsent:`, `at:ifPresent:` and `at:ifPresent:ifAbsent:` in a method body, a conditional or `on:do:` block, or a loop or fold body). A block that escapes is a compile error in every context (class, value-type and actor methods, and the REPL), reported at the assignment and naming the shape:

| Shape | Message |
|---|---|
| `f := [t := t + 1]` | `` `t` is assigned in a block stored in `f` `` |
| `^[t := t + 1]` | `` `t` is assigned in a block returned from `run` `` |
| `self forEach: [:o \| t := t + o]` | `` `t` is assigned in a block passed to `forEach:`, which is not a construct the compiler threads it through `` |
| `self do: [:x \| t := t + x]` (in an actor) | `` `t` is assigned in a block passed to `self do:`, which runs the actor's own `do:` method rather than the collection loop the compiler threads it through `` |
| `Tally onEvent: [:e \| seen := seen + 1]` (`Tally` declares `classState:`) | `` `seen` is assigned in a block passed to a class method (`onEvent:`); it would run in another process `` |
| `Erlang lists foreach: [:e \| t := t + e] with: xs` | `` `t` is assigned in a block passed to Erlang `lists` `foreach:with:`, which calls it as a plain fun `` |
| `#([t := t + 1])` | `` `t` is assigned in a block placed in a collection literal `` |
| `xs collect: [:x \| [t := t + x]]` | `` `t` is assigned in a block answered by the block of a collection operation `` |

A stored block is an error however it is used later, a `value` send included; so is a self-send to a `sealed` method of an actor's own class, which ADR 0131 §6 (BT-3755) exempted and ADR 0133 retires, and so is a collection message (`do:`, `inject:into:`, ...) an actor sends to `self` or `super` inside its own method, which is a message to its own method rather than the inlined collection loop (BT-3918). A class method is said to run in another process when it keeps its class process (its class or an ancestor declares class variables, or the selector is process-bound); a block passed to one that runs in the caller is reported like a block passed to any other method ([ADR 0142](ADR/0142-class-process-owns-only-class-state.md)). A block literal that assigns a captured local and is passed as an Erlang FFI argument (`Erlang lists foreach: [:e \| t := t + e] with: xs`) is the same compile error (BT-3917): Erlang calls it as a plain fun and the write would be dropped. Return the value from the block and use the call's result instead (ADR 0041 §Erlang Interop Boundary); a block that only reads captured variables is fine. The same holds for value-type object state: a block that assigns `self.field` in a value-type method and escapes is the same compile error, reported at the assignment as `self.field` (BT-3922; a `Value` subclass reports `Cannot assign to slot` instead, since value types are immutable). An actor field write, or a class-variable write in a class method, in an escaping block is legal: the block writes the live state at home (ADR 0137) and the live class variable (ADR 0130). Reading captured variables and `^` inside an escaping block are unaffected, and so are writes to the block's own parameters and locals.

**An actor block that reads a field may escape** (ADR 0137; this retires the BT-3952 restriction of ADR 0133's amendment). In an actor instance method a block reads the actor's fields as they are when it runs, not when it was made; see [Actor Blocks See Live State](#actor-blocks-see-live-state). The local-threading rows listed in `validators/local_threading.rs` remain until BT-3913 lowers them.

These lookup handlers are lowered inline, behind a runtime check that the receiver is a `Dictionary`: a handler that writes an outer local sent to anything else (an `Ets`, a value-type instance with its own `at:ifAbsent:`, a user class) raises a structured `type_error` instead of dropping the write. A receiver statically known not to be a `Dictionary` (`self`/`super` outside one, or an inferred type that is a known other class such as `Ets`) keeps the compile error. In a closure that is not inlined (a stored block, a block passed to another method) the enclosing block escapes, and in the REPL the handler is still a compile error.

```text
error: `count` is assigned in a block passed to a class method (`ap:`); it would run in another process
  = the block
  = a block that is stored, returned, or passed to a method the compiler does not lower in place
    may read captured variables but not assign them (ADR 0133). The messages that thread
    assignments, with the block written directly as the argument, are the control constructs
    (ifTrue:, ..., value:value:value:) and the collection messages (do:, ..., at:ifPresent:ifAbsent:)
  help: return the new value from the block and assign it where it is called:
        `count := ... ap: [... count + 1]`
```

The note's list is generated from `crates/beamtalk-core/src/state_threading_selectors.rs`, the one list codegen reads.

```beamtalk
// ❌ ERROR: `ap:` is not a construct the compiler threads the block through
count := 0
CvA ap: [count := count + 1]

// ✅ Return the value instead
count := CvA ap: [count + 1]

// ✅ Or use a control-flow message that threads locals
10 timesRepeat: [count := count + 1]
```

A block that escapes the method without being sent — returned via `^`, implicit last expression, stored in a field or class variable, or placed in a collection literal — is the same error. A method whose last statement is a local assignment answers the assigned value (class, value-type and actor methods alike), so a block stored there is returned too; it is reported as stored:

```text
error: `count` is assigned in a block returned from `makeCounter`
  help: once the block has left this scope, no later call of it can hand `count` back:
        keep the state in a field (`self.count := self.count + 1`) or return the new value
        from the block and assign it where the block is called
```

```beamtalk
// ❌ ERROR: escaping stateful block
makeCounter =>
    count := 0
    ^[count := count + 1]      // returned — compile error

// ❌ ERROR: stored in a field
storeBlock =>
    count := 0
    self.block := [count := count + 1]

// ❌ ERROR: placed in a collection literal
listOfBlocks =>
    count := 0
    #([count := count + 1])
```

**What works where** ([ADR 0131](ADR/0131-outer-local-rebinds-as-threaded-value-preludes.md)). A construct whose blocks write a local of the enclosing method (a *local-threading producer*) answers its value and keeps those writes. The writes are returned alongside the value and rebound in the enclosing frame, so the construct can sit wherever an expression can, not only as a statement. The constructs are the loops and collection messages in the table above (`do:`, `collect:`, `inject:into:`, `eachWithIndex:`, `do:separatedBy:`, `keysAndValuesDo:`, `doWithKey:`, `anySatisfy:`, `allSatisfy:`, `detect:`, `detect:ifNone:`), the conditionals (`ifTrue:`, `ifNil:`, `and:`/`or:`, ...), `on:do:`/`ensure:`, `Result tryDo:`, a block literal sent `value`, and `Dictionary at:ifAbsent:`, `at:ifPresent:` and `at:ifPresent:ifAbsent:`. The positions that work are:

- a statement, the right-hand side of an assignment, a `^` value, the value of a field write (`self.f := ...`) and the operands of a send or binary operator, in class methods, value-type methods and actor methods, and as an operand in the REPL
- a statement or value inside the arm of a conditional, the protected body or handler of `on:do:`/`ensure:`, or the body of a loop or fold, for the combinations listed below as accepted by the compiler

```beamtalk
// An operand-position construct: `+` sees the collect: result, and `t` keeps the writes
t := 0
n := (#(1, 2, 3) collect: [:x | t := t + x]) size + t
// n = 3 + 6 = 9, t = 6

// A sibling that reads the local before a later sibling rebinds it reads the old value
t := 0
r := t + ([t := t + 1. 1] on: Error do: [:e | 0])
// r = 0 + 1 = 1, t = 1
```

Specific forms:

- **Method-body operand positions**: `r := xs collect: [:x | count := count + x]`, `#[xs do: [...], t]`, `^xs do: [...]`
- **Nested loop and fold bodies**: `items do: [:x | result := xs collect: [:y | t := t + y]]`; a conditional whose arm writes an outer local, inside a loop or fold body
- **`do:`** answers `nil` and keeps its writes; **`eachWithIndex:`** and **`do:separatedBy:`** likewise answer `nil` and keep the writes of their blocks; a **`detect:ifNone:`** whose `ifNone:` handler writes an outer local keeps them
- **`keysAndValuesDo:` / `doWithKey:`** (on a `Dictionary`) answer `nil` and keep their writes. **`anySatisfy:` / `allSatisfy:`** answer their boolean and keep their writes, and stop at the first decisive element like the non-writing form: the block runs up to and including that element, so `n := n + 1` counts only the elements up to the match and the later elements are never visited (`#(1, nil) anySatisfy: [:x | n := n + 1. x > 0]` answers `true` after one visit). **`detect:` / `detect:ifNone:`** (class and value-type methods) answer the first match, or the `ifNone:` result, and keep the writes of the search block and of the handler; the search block stops at the first match the same way

In a value-type method, a `do:`, `collect:` or `inject:into:` that writes a field (`self.m := ...`) may sit inside the arm of a conditional (`c ifTrue: [xs do: [:e | self.m := self.m + e]]`): the arm is a frame whose fold hands its updated `self` to the statements after the conditional (BT-3844, BT-3854).
- **Conditionals**: `c ifTrue: [t := t + 1] ifFalse: [0]` and the rest of the family, at method-root sites in all three method kinds and at operand sites in the REPL
- **Evaluated block literals**: `[t := t + 1] value`, and with arguments `[:y | t := y] value: 5` or `[:a :b | t := a + b] value: 2 value: 3`, wherever a conditional is a producer, in all three method kinds: the block is an arm that always runs. The arguments are evaluated first, in order, and the writes are visible to the operands and statements after the send: `{[:y | t := y] value: 5, t}` is `{5, 5}` ([BT-4263](https://linear.app/beamtalk/issue/BT-4263)). A block literal sent a `value` selector of another arity is applied and raises the usual wrong-number-of-arguments error
- **REPL arms**: in the REPL, the arms of a conditional, the blocks of an `on:do:`/`ensure:`, the block of a `Result tryDo:` and an evaluated block (`[...] value`, `[:k | ...] value: x`) are producer frames too: a loop, fold, conditional, exception construct or `[...] value`/`value:` send at a statement, an assignment's value or an operand of an arm statement keeps its writes (`[x ifTrue: [(xs inject: 0 into: [:a :e | t := t + e. a + e]) + 1]] on: Error do: [:e | 0]`); an evaluated block counts only when the send's arity matches the block's, and a mismatched `[t := t + 1] value: 1` in an arm raises the usual wrong-number-of-arguments error
- **`on:do:` and `ensure:`**: `[t := t + 1] ensure: [u := u + 1]` in any producer position; the protected body, handler and cleanup are arms of the producer
- **`Result tryDo:`**: `r := Result tryDo: [t := t + 1. 1 / 0]`, the Tier 2 twin of the one-argument `tryDo:`; the receiver must be the literal `Result` class reference. A `tryDo:` sent to any other receiver keeps its dynamic send, and a writing block passed to it is the "block writes outer local" error
- **`Dictionary at:ifAbsent:`, `at:ifPresent:`, `at:ifPresent:ifAbsent:`**: `d at: #k ifAbsent: [t := t + 1. 0]`, `d at: #k ifPresent: [:v | t := t + v. v]` and `d at: #k ifPresent: [:v | t := t + v. v] ifAbsent: [u := u + 1. 0]` in statement, assignment, operand, `^`, conditional-arm, `on:do:` body and handler positions, and in loop and fold bodies (`keys do: [:k | d at: k ifAbsent: [misses := misses + 1. 0]]`; in an actor method the value of an assignment or an operand directly in a fold body is still a compile error, as for a conditional). The handler is lowered inline behind a runtime check that the receiver is a `Dictionary`; any other receiver (an `Ets`, a user class) raises a structured `type_error` instead of dropping the write, and a receiver statically known not to be a `Dictionary` is a compile error. A lookup in a closure that is not inlined or in the REPL is still a compile error. `at:ifAbsentPut:` and `removeKey:ifAbsent:` handlers are not producers (the §6 error): an immutable `Dictionary` does not define them, so write `at:ifAbsent:` and then `at:put:`

**The protected-region rule.** A local written inside the block of an `on:do:` or `Result tryDo:` that then raises is *discarded*: after the handler runs, the local has the value it had on entering the block. Writes made before entering the block are kept, and writes made by the handler itself (and by an `ensure:` cleanup) are kept. This holds in every method kind and in the REPL, and matches what class variables do at a catch boundary (ADR 0130 §4).

```beamtalk
t := 0
(Result tryDo: [t := t + 1. 1 / 0]) isError   // => true
t                                             // => 0, the write inside the raising block is discarded
```

This is a deliberate departure from Pharo, where temporaries are shared cells and a write made before the raise survives. On BEAM the protected region's locals are a value that is returned only when the block completes. A block that does not raise keeps all its writes.

**`match:` arms.** A `match:` arm is an expression, not a block, but it is the same frame a conditional's block is ([BT-3947](https://linear.app/beamtalk/issue/BT-3947), closing [BT-3875](https://linear.app/beamtalk/issue/BT-3875)): a `match:` whose arm assigns an outer local (`x match: [#a -> (t := t + 1); _ -> 0]`) is a local-threading producer wherever a conditional would be (a statement, an assignment's value, an operand, a loop body, a conditional or `match:` arm), in class, value-type and actor methods and in the REPL, and the write comes back. It stays a compile error where it is not a producer: a write in a guard (`x when: [(t := 1) > 0] -> ...`), or a `match:` that is the subject of another `match:`; bind the value there (`t := x match: [#a -> t + 1; _ -> t]`).

**Position errors.** Some combinations of a construct, the position it sits in and the method kind are not yet lowered as producers and are rejected with a second, temporary error rather than being allowed to drop the write:

```text
error: `detect:ifNone:` writes outer local `t`, but as a statement inside a conditional arm in an actor method
       the write is not threaded back yet (BT-3743)
```

These combinations are measured and listed in `crates/beamtalk-core/src/semantic_analysis/validators/local_threading.rs`; they are mostly `detect:ifNone:` in an actor method at nested positions, an `ifNil:`/`and:` family member or an actor loop in an arm, and, in the REPL, a construct inside a REPL loop or fold body (`#(1, 2) do: [:x | [t := t + x] on: Error do: [:e | 0]]`, BT-3876). The rewrite is the same as for the §6 error: bind the construct's value to a local, or hoist it out of the nesting. Also still compile errors: an assignment inside a conditional arm in a `whileTrue:`/`whileFalse:` condition block in a method (`[i < n and: [x := i. x < 5]] whileTrue: [...]`, BT-3891; assign in the condition block or the loop body instead), a `^` whose value is a local-threading construct inside a plain closure in an actor method (`^xs do: [...]` inside a `detect:ifNone:` handler or sort block; class and value-type methods accept this shape), a cascade, and a conditional's receiver. This error goes away construct by construct as the remaining shapes become producers; it is a known limitation, not a language rule.

**Assignments used as values.** A local assignment whose value is used by the expression around it, rather than standing as its own statement, rebinds the local like any other assignment ([BT-3910](https://linear.app/beamtalk/issue/BT-3910), ADR 0131 §1). That covers an operand (`x := (t := t + 1) + 1`), a collection-literal or interpolated element (`#[t, (t := 7), t]`), the value of another assignment or of `^` (`x := t := 7`, `^t := 7`), and a message receiver, including a conditional's or a cascade's (`(t := t + 1) > 0 ifTrue: [...]`), in methods, in loop and fold bodies and in the REPL alike. The value of the assignment is the assigned value, and a sibling that is read before it sees the old value:

```beamtalk
#[t, (t := 7), t]    // with t = 0: #[0, 7, 7]
```

`^t := e` returns `e` after rebinding `t`. Nothing runs after the return that could read `t`, so the write is dead and `beamtalk lint` warns (`dead_returned_assignment`) and suggests `^e`.

**An `ensure:` cleanup cannot read a local its protected block wrote if the block can exit early.** When the protected block leaves by a `^` (`[^t := t + 1] ensure: [t printString]`, and equally `[t := t + 1. ^t] ensure: [t printString]`, a `^` in a block nested in it, or evaluating a block value that may hold one) or by an error raised after the write, the cleanup runs over the locals the protected block was entered with and would read the old `t`. A cleanup that reads a local the protected block writes is therefore a compile error whenever something after the write may exit that way: a `^`, any message send (an arithmetic operator included), a destructuring assignment or `match:`, or the next iteration of a loop or fold body. This is a deliberate restriction ([ADR 0133](ADR/0133-scope-of-captured-variable-mutation-in-blocks.md), Consequences), not a gap to be closed: lowering it would need a new runtime mechanism. The check over-approximates: anything after the write that may raise counts, even inside an `on:do:` (in the protected block) that catches it, so `[[t := 9. 1 / 0] on: Error do: [:e | 0]] ensure: [log record: t]` is rejected although the cleanup would see 9. A write that is the last thing the protected block does is seen. Assign the local after the `ensure:` (or compute its value outside the protected block), keep the `^` out of the protected block, or do not read the local in the cleanup:

```beamtalk
// ❌ ERROR: if `self work` raises, the cleanup would see the old `count`
[count := count + 1. self work] ensure: [log record: count]

// ✅ Assign outside the protected block
count := count + 1
[self work] ensure: [log record: count]

// ✅ The write is the last thing the protected block does
result := nil
[result := self work] ensure: [log record: result]
```

An actor's fields and class variables are not affected: an `ensure:` cleanup reads them live.

**A `^` in a stored closure answers what the closure captured.** A block that is not run where it is written (stored in a local, passed to a method, ...) captures the values of the locals it reads when it is created. A `^` in such a block that reads a local the method assigns after the block is created, or inside the body of a loop or fold that creates it, may answer the old value, so it is rejected conservatively as a compile error ([BT-4003](https://linear.app/beamtalk/issue/BT-4003)); at the REPL, so is one that reads any workspace binding (a later input may assign it). The rule treats every such block as escaping (as ADR 0133 does for a block passed to a user method), so it also rejects a block that is in fact run before the assignment and would answer right: `t := 0. self withFallback: [^t]. t := 5` is an error even if `withFallback:` runs the block at once. Pass the current value in as an argument (`[:v | ^v] value: t`) or write the block where it is run.

**A `^` in a REPL loop body.** At the REPL, a `^` in the body of a loop or collection operation (`#(1, 2) do: [:x | ^x]`, `[n < 3] whileTrue: [^10]`, also through a conditional arm, an evaluated or protected block or a nested loop in the body) ends the input with its value, as it ends a method ([BT-4288](https://linear.app/beamtalk/issue/BT-4288)). The bindings written before the `^` keep their values, and the rest of the input does not run:

```beamtalk
w := 0
#(1, 2, 3) do: [:x | w := w + x. x == 2 ifTrue: [^w]]   // => 3
w                                                        // => 3
3 timesRepeat: [^8]. w := 99                             // => 8 (w is still 3)
```

Such a `^` inside the value of a top-level assignment (`r := #(1, 2) do: [:x | ^x]`) is a compile error: a method never assigns `r`, but the workspace would bind it to the input's value. So is a loop with a `^` in its body held in a block that is not run where it is written (`b := [#(1, 2) do: [:x | ^x]]`, or such a block passed to a method): the block may run in a later input, after the one that created it has ended, so nothing could catch the `^`. Answer the value with `detect:ifNone:` or `inject:into:` and assign that, or leave the assignment out. A `^` at the root of an input, in a conditional arm, an evaluated or protected block outside a loop, or a stored closure answers its value where it stands.

### Actor Blocks See Live State

In an actor instance method (ADR 0137) a block that reads or writes `self.field`, or sends to `self`, sees and changes the actor's current state, wherever the block is made and wherever it runs: a loop body, an `on:do:` or `ensure:` body, a block with a `^`, a block stored in a field and run by a later message, or a block passed to one of the actor's own methods.

```beamtalk
Actor subclass: Counter
  state: n = 0
  state: onTick = nil

  twice: aBlock =>
    aBlock value
    aBlock value

  addTwice =>
    self twice: [self.n := self.n + 1]
    self.n                      // 2

  arm => self.onTick := [self.n := self.n + 10]
  tick =>
    self.onTick value
    self.n                      // 10, then 20
```

A block *at home* (running in the actor's own process, inside one of its messages) reads and writes the live fields. A block that has been carried *away* to another process (handed to another actor, or run by a task) reads the fields as they were when the block was created; a **write** from there raises `actor_state_unreachable`, and a send to `self` from there is an ordinary message to the actor. `on:do:` restores the actor's fields to their values on entry to the protected block when it handles an error; `ensure:` cleanups see every write the protected block made before it ended; a `^` stores its writes before it leaves. Blocks that **assign a captured local** are still governed by ADR 0133's rule above.

Two older rejections remain: the local-threading rows listed at the end of the [Capturing Variables](#what-works-and-what-doesnt) discussion, and, in an actor method, a loop or collection operation whose block sends to `self` or writes a field used where its value is not sequenced: the receiver of a conditional (`(xs inject: 0 into: [...]) > 3 ifTrue: [...]`, `and:`, `or:`, `ifNil:`), a dictionary lookup's key, a `match:` guard, or an argument of a `super` send, `error:`, a block invocation (`[:z | z] value: ...`), a cast or a self-send that also takes a block literal ([BT-4310](https://linear.app/beamtalk/issue/BT-4310); assign the value to a local in a statement before it). Everywhere else, such a loop or collection operation works as an operand, inside the body of another one too: `#(1, 2) do: [:x | x + (self.items inject: 0 into: [:b :y | b + self bump])]` sends `bump` four times and answers each sum ([BT-4287](https://linear.app/beamtalk/issue/BT-4287); it was a compile error under BT-4003). Field writes in stored closures of value-type methods stay a compile error. Those messages say so when they fire.

### Why This Design?

| Property | ✅ Benefit |
|----------|-----------|
| **Universal** | Local variable mutations work in every control-flow block — no whitelist; a block with no way back is a compile error, not a lost write |
| **Smalltalk-like** | Natural iteration patterns work; a user-defined HOM takes a block that answers a value (ADR 0133) |
| **Safe** | In value-type methods, field mutations in stored closures are caught at compile time; in actors they see live state (ADR 0137), and class-variable writes in class methods write the live variable (ADR 0130) |
| **Good DX** | Clear errors with fix suggestions |
| **BEAM-idiomatic** | Compiles to tail recursion + state threading |
| **Performant** | Stdlib hot paths are zero overhead (Tier 1); user HOMs ~65ns (Tier 2) |

### Error Messages

In a value-type method, a field assignment inside a stored closure is a compile error that suggests moving the mutation into a method of its own:

```beamtalk
addToSum: item => self.sum := self.sum + item
```

The same assignment in an actor instance method compiles and writes the live field, and a class-variable assignment in a class method compiles and writes the live class variable.

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

The method's return value is the reply. A sync method can instead **defer** its reply with `self deferReply` (OTP `{noreply, State}` + `gen_server:reply/2`) and answer the caller later; see [Deferred Replies](#deferred-replies-deferreply).

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

An error raised by a method sent with `!` has no caller to go to, so it
crashes the actor, as in OTP (ADR 0134): `terminate:` runs with the error as
its reason, `onExit:` observers receive it, and a supervisor restarts the
actor according to its policy. If a failure is expected and should not stop
the actor, handle it inside the method with `on:do:`. A sync send (`.`) is
unaffected: its error is returned to the caller and the actor carries on.

`self foo!` inside an actor's own method is a real cast to the actor's own
mailbox: `foo` runs after the current message finishes, and its field writes
are kept. It does not run inline, so the rest of the current method does not
see its effects.

A send to a local bound to `self` (`other := self. other setX: 7`) works
like a self-send on the live state (ADR 0137): it runs against the actor's
current fields and its writes are kept, whether `self` got there by
assignment, as an argument, via `yourself`, or out of a collection. It is an
ordinary send, not a statically resolved self-send, so it pays the dispatch
cost; prefer `self` directly where that matters.

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

   **Supplying a `late` slot by key counts as assigned.** `spawn`'s `init/1` merges the declared defaults with the caller's map, caller winning, so `CodexClient spawnWith: #{#proc => aFakeSubprocess}` puts `proc` in the state map even though it is declared `late` — this is legitimate dependency injection (substituting a value for a slot normally acquired by a lifecycle call), not a violation of `late`'s "absent until assigned" contract, and it also satisfies the [Definite Assignment](#definite-assignment-adr-0124) construction-site check the same way supplying any other unassigned slot does. See [`late` Slots](#late-slots-adr-0124) above.

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

Since [ADR 0142](ADR/0142-class-process-owns-only-class-state.md) a class method already runs in the caller unless its class's chain declares class variables or the selector is process-bound (see [Class-Side Methods](#class-side-methods-adr-0048)), so `performLocally:withArguments:` matters only for those. For a long-running class method of such a class (batch processing, report generation) that would block all other messages to the class, use `performLocally:withArguments:` to execute in the caller's process instead:

```beamtalk
// Normal dispatch — runs in MyClass's class process when MyClass has classState:
MyClass computeReport

// Local dispatch — runs in caller's process, doesn't block the class
MyClass performLocally: #computeReport withArguments: #()

// With arguments
MyClass performLocally: #add:to: withArguments: #(3, 7)
```

**Limitations:** Local dispatch resolves the selector through the class-method chain from the receiving class, so an inherited class method is found and a subclass override wins (BT-4300). It applies compiled methods only: it does not apply ADR 0084 runtime class-method funs, so a selector whose nearest definition is a runtime fun raises `does_not_understand`. Class variables follow the read-only snapshot rule ([ADR 0130](ADR/0130-class-variables-live-in-the-class-process.md) §5): the method *reads* the class variables as of the last completed class-method invocation, and a *write* raises `class_state_read_only`. When `performLocally:withArguments:` is reached from inside the class's own method mid-invocation, it reads and writes the live class variables instead. Use this only for stateless or read-only class methods.

### Passing Blocks Through Class Methods

*See also:* process-sensitive FFI written directly in a class method that runs in the class process runs there too; the `class-process-ffi` lint flags it, and `class-method-process-resource` flags process-tied resources created in one that runs in the caller (see [Class-Side Methods](#class-side-methods-adr-0048)).

**Most class methods run in the caller, and so do their blocks ([ADR 0142](ADR/0142-class-process-owns-only-class-state.md)).** When the class's superclass chain declares no class variables and the selector is not process-bound, the class method runs in the caller's process, however it is reached: statically, through a variable, `perform:`, `Beamtalk classNamed:`, or a class passed as an argument. A block passed into it runs where it was written (inside an actor, that is at home in the [ADR 0137](ADR/0137-actor-closures-see-live-state.md) sense), `self()` and the process dictionary are the caller's, links, monitors, timers and FFI resources it creates belong to the caller, there is no `class_send` timeout, an `exit` inside the method exits the caller, and messaging the same class from inside does not raise `dispatch_error`. Calls are no longer serialised through the class process: two callers run the method at once. `self` is still the class object (the real class reference, even for an inherited method), so `self new`, `self new:`, `self spawn`, `self spawnWith:`, `self class` and class-side self-sends behave as in any other class method (BT-4029).

**When there is a hop.** A class method runs in the class object's gen_server process when the class or any ancestor declares class variables (`classState:`), when the selector is process-bound (`new`, `new:`, `spawn`, `startLink`, ...), or when a class-side extension or runtime fun shadows the receiving class's own methods. A block passed *into* such a method runs there too, not where it was written ([BT-3022](https://linear.app/beamtalk/issue/BT-3022)):

```beamtalk
Object subclass: Driver
  classState: runs :: Integer = 0
  class run: aBlock over: aList -> Nil =>
    aList do: [:x | aBlock value: x]      // aBlock runs in Driver's class process
    nil
```

Arguments and return values cross that boundary as copies, so blocks that compute answers work normally. Two things do *not* survive the hop:

- **Process-local side effects.** A block that writes to the process dictionary (`Erlang erlang put:value:`), reads `self()`, or otherwise depends on running in a particular process affects the *class* process. The caller sees none of it.
- **Re-entrant class sends.** If the block messages the same class whose method is driving it, the class process would have to `gen_server:call` itself. That raises a structured `dispatch_error` naming the selector rather than deadlocking. Read what you need before the call, or hold the resource yourself instead of using the block form. The exception is the class-object protocol that only reads the class: a selector the class itself does not define as a class method (`name`, `superclass`, `doc`, `printString`, `includesSelector:`, ...) answers in place, the same as `self name` in a class method does ([BT-4262](https://linear.app/beamtalk/issue/BT-4262)). Two kinds of protocol still raise `dispatch_error` from inside the class's own class method. One is the class-variable reflection sends `hasField:` and `clearField:`, which only the class process can answer. The other is every protocol method that changes or messages the class: `removeFromSystem`, `renameTo:`, `reload`, `removeSelector:`, `renameSelector:to:`, `compile:source:`, `tryCompile:source:`, `doc:` and `setDocForMethod:to:`. These are refused before they do anything. From a block running in another class's class method while this class waits on it, they cannot reach the class either: `removeFromSystem` and `renameTo:` see that the class is waiting on the caller, directly or through other classes and actors, and raise a structured `timeout` after 5 seconds, before changing anything (a class that is only busy is waited for, as before, unless its own wait chain passes a process that monitors the caller and it stays busy for more than 5 seconds), and the others fail when their call to the class times out ([BT-4290](https://linear.app/beamtalk/issue/BT-4290)). Change a class from outside it.

Non-local return (`^`) *does* cross the boundary: the signal is relayed back and unwinds the enclosing method as it would without the hop. A `^` is not an error, so a class variable written before the block escaped keeps its write ([ADR 0130](ADR/0130-class-variables-live-in-the-class-process.md) §4).

**Hot-patching and direct calls.** A literal send of a class method the class itself defines compiles to a *guarded* direct call: it reads the class's direct-call flag (`beamtalk_class_shadow_flags:direct_call_ok/2`, one `persistent_term` read) and checks that the method is still exported, and otherwise falls back to the ordinary class send, which answers what a dynamic send answers. An inherited method sent to a literal receiver takes the dynamic path, which also runs in the caller, so no call site names a superclass module. Reloading a class, giving it or an ancestor class variables, re-parenting it, `removeSelector:`, a class-side extension or a runtime class-method fun all erase the flag, so code compiled earlier follows the change without being recompiled. A descendant's flag is erased just after an ancestor's change is written, so for a moment a descendant's static send can still run in the caller: the same window a reload has for its own new code.

The `new`/`new:` direct-construction verdicts of `Value` classes are still baked into each compiled module. If a class gains a class-side `new:`, the runtime logs a warning naming every subclass that still builds `new:` directly and so bypasses the new override (BT-4126). Recompile the code the warning names. A release upgrade recompiles everything, so this only affects targeted hot-patches.

#### Class variables and blocks (ADR 0130)

Class variables follow three rules ([ADR 0130](ADR/0130-class-variables-live-in-the-class-process.md)).

1. **One home.** During a class-method invocation a class's variables live in exactly one place, the class's own process. Class methods neither take nor return them, and a write is made in place wherever it happens, so every later read sees it: at a method's top level, in loops and conditionals, inside `do:`/`collect:` and the other collection methods, inside `ensure:`/`on:do:`/`Result tryDo:`, in a stored closure invoked by a later statement, and in a block handed to a class-side higher-order method of the *same* class. Sealing a class or a selector changes how a send is dispatched and nothing about class variables. There is no workaround to learn: a loop body that only writes a class variable, or only sends a class-side message that writes one, compiles and works.
2. **A boundary discards what happened inside it.** An error that crosses a boundary discards every write made inside that boundary; a write made before entering it is kept. There are two boundaries. The *invocation* boundary: an error that escapes a class method discards all of that invocation's writes. The *catch* boundary: `on:do:`, `Result tryDo:` and the other runtime catchers that run Beamtalk blocks restore the variables to their value on entry before the handler runs. A non-local return (`^`) is not an error and discards nothing; `ensure:` is not a boundary (it does not catch), so its cleanup block's writes go with whichever boundary the error reaches.
3. **A block writes its home class's variables only at home, and reads what it captured elsewhere.** A block runs *at home* when it runs inside an invocation of the class it was written in: the same process, or a later invocation of the same class. At home it reads and writes the live variables. Anywhere else (a block carried to another process by a class method of another class that keeps its class process, handed to an actor, or stored or returned and run after its invocation ended) it reads the values the variables had **when the block was created**, as a closure reads a captured variable, and a *write* raises `class_state_unreachable`. A class method of a class whose chain declares no class variables runs in the caller ([ADR 0142](ADR/0142-class-process-owns-only-class-state.md)), so a block handed to it stays at home.

```beamtalk
Object subclass: Counter
  classState: n = 0
  class bump -> Integer => self.n := self.n + 1
  class hook => nil
  class run =>
    b := [self hook]
    #(1, 2, 3) do: [:i | i > 1 ifTrue: [self bump]]
    b value
    self.n

Counter subclass: LoudCounter
  classState: n = 0          // class variables are per class, not inherited (ADR 0013)
  class hook => self bump

Counter run        // => 2
LoudCounter run    // => 3
```

The loop's conditional arm writes through `self bump`, and the stored closure `b` reads and writes the live variables when `b value` runs at home, so `LoudCounter run` answers 3 (the template-method override `hook` bumps once more).

Rule 3, a block abroad:

```beamtalk
Object subclass: Tally
  classState: total = 0
  class add: x => self.total := self.total + x
  class addAll: items =>
    Batch each: items do: [:x | self add: x]
    self.total

Object subclass: Batch
  classState: runs = 0
  class each: items do: aBlock =>
    self.runs := self.runs + 1
    items do: aBlock
    nil

Tally addAll: #(1, 2)
// => ERROR: class_state_unreachable: a block that reads or writes Tally's class variables ran in another process
```

`Batch each:do:` runs in `Batch`'s process, so the block (and the `Tally add:` it sends) runs away from `Tally`'s home. The write raises instead of being silently lost, and the `class-state-abroad` lint warns at the block. A block that only *reads* `self.total` there would answer the value it captured. The fix is to return the value and assign it in the home class's own method, or to read into a local before passing the block.

Rule 2, a catch boundary:

```beamtalk
Object subclass: Ids
  classState: next = 0
  class take =>
    self.next := self.next + 1
    self error: "boom"
  class tryTake =>
    [self take] on: Error do: [:e | nil]
    self.next

Ids tryTake        // => 0
```

The write made inside the protected block is discarded when the error crosses the catch. Had `tryTake` assigned `self.next` *before* entering the block, that write would be kept. Outside the class process each class-method call is its own transaction (in `[X bump. X failingBump] on: Error do: [:e | nil]` from the REPL, `bump`'s write is committed when its call returns), whereas inside one of `X`'s own class methods the protected region is the transaction.

**Errors.**

| Error kind | Raised when |
|---|---|
| `class_state_unreachable` | A block writes a class variable (or a read finds no live or snapshotted state) away from its home class's invocation. `details` carries `class_variable`; the hint says to return the value and assign it in the home class's method. |
| `class_state_read_only` | Code that runs against a read-only snapshot writes a class variable. See below. |
| `abi_mismatch` | The loader refuses a compiled class module built with a different class-variable calling convention. See below. |

**Runtime-owned out-of-process calls read a snapshot.** Supervisor definition (the instance-side configuration methods `static_init`/`dynamic_init` read, ADR 0135, and the `withClassMethod:` factory) and `performLocally:withArguments:` run outside the class process. They read the class variables as of the last completed invocation (a snapshot), and a *write* there raises `class_state_read_only`. When the caller is the class process itself, mid-invocation, they read and write the live variables. A supervisor factory or `performLocally:` method that used to assign a class variable (the assignment was dropped, or for supervisor configuration crashed supervisor startup) now raises; assign the variable from a normal class-side message instead.

**Class-module ABI.** Every compiled class module records its class-variable calling convention as `class_var_abi` in `__beamtalk_meta/0`. The loader (class registration, hot reload, and the release upgrade preflight) refuses a module whose `class_var_abi` is missing or differs from the running runtime's, including every module compiled before ADR 0130, with a structured `abi_mismatch` error that names the module and says to recompile. Hot upgrades across the release that changed the convention are not supported: restart the node and recompile every package.

**The `class-state-abroad` lint.** Where the compiler can see the case, it says what a block means abroad instead of leaving it to run time: a block literal that reads class variables and is passed to an asynchronous send, handed to an actor, stored or returned, or a block literal that writes class variables and is passed to another class's class-side method that runs in that class's process, gets a `class-state-abroad` warning at the block. Suppress it with `@expect class_state_abroad`, or package-wide with `class-state-abroad = "off"` under `[diagnostics]`. It is a lint, not a guarantee: a block passed through a variable or an instance method is only caught at run time, and only for writes. Cascade messages after the first are inspected (e.g. `self log; bump`), and an inherited method (sealed or not) of an open class that makes a late-bound `self` send to a non-sealed selector is flagged when a subclass override could write a class variable.

This matters most when building a `Collection` subclass: implementing `do:` by delegating to a class-side helper works, and so does a helper that reaches back into its own class.

```beamtalk
Collection subclass: Batch
  field: items :: List = #()
  size -> Integer => self.items size

  do: block :: Block -> Nil =>
    Driver run: block over: self.items
    nil
```

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
    Timer send: #tick to: self after: 1000

  handleInfo: msg =>
    msg match: [
      #tick -> self tick;
      _ -> nil
    ]

  tick =>
    self.count := self.count + 1
    Timer send: #tick to: self after: 1000

  getValue => self.count
```

`Timer send:to:after:` delivers the message to the server's mailbox exactly as `erlang:send/2` would, so it arrives in `handleInfo:` (see § Message Timers).

A `match:` arm body is an expression, so an arm that needs more than one statement calls a method (`#tick -> self tick`). Do not write `#tick -> [ ... ]`: that arm answers the block itself without running it (a block that writes a field or sends to `self` fails to compile; see § Match Expression).

### handleInfo: Semantics

- Defined on `Server` with a **default no-op** implementation — messages are ignored unless you override it.
- **Error handling:** an error raised in `handleInfo:` crashes the server, as in OTP (ADR 0134): `terminate:` runs with the error as its reason, `onExit:` observers receive it, a crash report is logged, and a supervisor applies its restart policy. A method sent with `!` and a `continueWith:` step behave the same way (see § Explicit Async Cast and § Continuations); a sync call is the only send whose error goes back to a caller. Unmatched messages are still harmless: the default `handleInfo:` ignores everything, and a `match:` over the message should end with a `_ -> nil` arm so an unexpected message does not raise `#pattern_mismatch`.
- Sending `handleInfo:` to a plain `Actor` raises `doesNotUnderstand` — only Server subclasses have this method.
- **Sending a raw message:** `aServer pid send: msg` (`Pid>>send:`, `erlang:send/2`) puts `msg` in the server's mailbox, where `handleInfo:` receives it. It is not a method call: it is asynchronous, answers `nil`, and sending to a dead pid is a silent no-op. Use `Reference new` (`erlang:make_ref/0`) for a unique tag to correlate replies or timers, and `self processLabel: term` (`proc_lib:set_label/1`) to name an actor's process in observer and crash reports; `self processLabel` reads it back (`nil` when unset).
- **Lint `handleinfo-no-catch-all`** (ADR 0134): a `handleInfo:` whose body is a `match:` with no catch-all arm warns at compile time, because `match:` with no matching arm raises `#pattern_mismatch` on every message no arm names, and that error crashes the server. Add `_ -> nil` as the last arm to ignore unexpected messages. A tuple-pattern arm (`{tag, payload}`) is not a catch-all. Configure it under `[diagnostics]` as `handleinfo-no-catch-all`; it has no dedicated `@expect` category.

  ```beamtalk
  handleInfo: msg =>
    msg match: [
      {#tick, n} -> self.count := self.count + n;
      _ -> nil
    ]
  ```

### onLinkedExit:reason: Semantics

By default a `Server` that traps exits (see `Actor>>terminate:`) treats a linked process's `EXIT` like an untrapped link would: a non-`#normal` reason stops the server with that reason, and `#normal` is ignored. A server that links worker processes on purpose (for example `Erlang erlang spawn_link:`) can handle their exits itself by overriding `onLinkedExit: source reason: reason`:

- Defined on `Server` with a **default no-op**. Overriding it (in the class or any ancestor other than `Server`) is the opt-in; there is no class-side flag.
- **All** non-parent `EXIT`s — normal (`reason` is `#normal`) and abnormal — go to the hook, and only to the hook: they are never also delivered to `handleInfo:`. `EXIT` from the server's parent (supervisor/spawner) stays with the `gen_server` loop: `terminate:` runs and the server stops.
- Overriding the hook makes the server trap exits; no `terminate:` override is needed.
- **Error handling** is the same as `handleInfo:` (ADR 0134): a raise crashes the server. `continueWith:` and `stopWith:` work from the hook.
- **`source` is a `Pid | Port`**: a trapping server also receives `EXIT` from linked ports (`open_port`, `gen_tcp`/`ssl` sockets), so annotate the parameter `Pid | Port`.
- **Hot reload**: the server only traps exits from `init/1`, so adding or removing the override by hot reload applies to instances started after the change (same as `terminate:`).
- **Server-only.** Plain ``Actor`s` keep the default behaviour and never receive the hook.

  ```beamtalk
  Server subclass: StreamHost
    state: resets = 0
    onLinkedExit: _source :: Pid | Port reason: reason :: Object =>
      reason == #normal ifFalse: [self.resets := self.resets + 1]
  ```

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
    Timer every: 1000 do: [self tick!]   // async cast: the block runs away, so it messages the actor

  tick => self.count := self.count + 1
  getValue => self.count
```

A timer block runs in the timer's process, away from the actor (ADR 0137): a
field read there sees the value from when the block was made, a field write
raises `actor_state_unreachable`, and a self-send is an ordinary message to
the actor. Prefer the `!` cast: a synchronous `self tick` from the timer
works, but it blocks the timer until the actor answers, and it deadlocks if
the actor is ever waiting on that timer.

No `state:` for the timer reference, no `terminate:` cleanup needed — the link handles it.

### Message Timers

A block timer runs its block in a separate process, so the block cannot touch the actor's state; it can only send the actor a message. When the actor is a `Server`, deliver the message directly with a **message timer** instead, and handle it in `handleInfo:`:

```beamtalk
t := Timer send: #tick to: self after: 1000          // one-shot; answers a Timer
t := Timer send: {#readTimeout, tag} to: self after: (Duration seconds: 5)
t := Timer send: #tick to: self every: 1000          // repeating
t cancel                                             // => Boolean
t isActive                                           // => Boolean
```

- `to:` takes an Actor (including a Server) or a `Pid`. The message is delivered exactly as `erlang:send/2` would deliver it: a Server receives it in `handleInfo:`, and a plain Actor ignores it.
- The delay is Integer milliseconds or a `Duration`, as for `after:do:`, up to 4294967295 ms.
- A one-shot message timer maps to `erlang:send_after/3`, with no extra process; `cancel` is `erlang:cancel_timer/1` and answers `true` only if the message had not been sent yet. `isActive` is `erlang:read_timer/1`. A target on another node uses a small process instead, because `send_after/3` only accepts local targets.
- A repeating message timer runs in a small process that is **not** linked to the caller. It stops on `cancel`, or when the target dies, so a supervisor-restarted named Server does not inherit the old ticks. Ticks follow fixed deadlines and do not drift; ticks missed while the system was busy are skipped, not replayed. Because the timer is not tied to its creator, a caller that targets another process owns the Timer: keep it and `cancel` it, or it runs for as long as the target lives.
- A timer whose target has died does nothing, as in OTP.
- A message sent just before `cancel` may already be in the mailbox. When a stale tick matters, tag the message with a fresh `Reference new` (`{#tick, ref}`), keep the current tag in state, and ignore messages carrying an old one.

```beamtalk
Server subclass: Poller
  state: timer = nil

  initialize => self.timer := Timer send: #poll to: self every: 5000

  handleInfo: msg =>
    msg match: [
      #poll -> self poll;
      _ -> nil
    ]

  poll => nil

  pause => self.timer cancel
```

### Continuations (`continueWith:`)

`self continueWith: #selector` (or `self continueWith: #sel: withArguments: #(arg)`) asks the actor to run a follow-up step **after the current message has been handled and before any other message in its mailbox**. It is OTP's `{noreply, State, {continue, Term}}` / `handle_continue/2`. Neither alternative gives this ordering: `self foo` is a direct call that runs immediately, and `self pid send: #foo` (a raw message for `handleInfo:`) queues behind everything already in the mailbox.

```beamtalk
Server subclass: Connection
  state: sock = nil

  handleInfo: msg =>
    msg match: [
      {#ready, s} -> self onReady: s;
      _ -> nil
    ]

  onReady: s =>
    self.sock := s
    self continueWith: #handleConnection   // runs before any queued #tcp message

  // Set up the socket here, before the first #tcp message is handled.
  handleConnection => self.sock
```

Both methods are defined on `Actor` and answer `#ok`.

- **Where it works:** any of the actor's own methods, including `initialize`, `handleInfo:`, sync methods, `!` casts, and helper methods they call. A request made in a nested self-send joins the same queue.
- **Ordering:** several requests in one message run in request order. A continuation that requests more queues them **after** the ones already waiting. All of them run before the next mailbox message.
- **Sync methods:** the caller gets its reply first, then the continuation runs. Its state changes are visible to the next message.
- **Arguments:** `continueWith:` takes a unary selector. `continueWith:withArguments:` takes a `List` whose size must match the selector's arity. A non-Symbol selector raises `type_error` at the call site, and a wrong argument count raises `arity_mismatch`. Whether the actor understands the selector is checked only when the step runs.
- **Stop wins:** if the same message also calls `self stopWith:` (in either order), the actor stops and no queued continuation runs. A continuation that itself calls `self stopWith:` stops the actor after that step, and the rest of the queue is dropped.
- **Errors:** if a sync method that requested continuations fails with an error, the caller gets the error and the continuations it requested are dropped. An error inside a continuation is handled exactly like an error in a `!` cast or `handleInfo:` (ADR 0134): it crashes the actor, so no later step runs; `terminate:` and `onExit:` see the error.
- **`initialize`:** a continuation requested in `initialize` runs after `spawn` returns, before any later message (OTP `{continue, _}` after `init/1`). Startup ends when `initialize` returns: `spawn`, `spawnWith:` and supervisor child starts do not wait for the continuation chain, and only `initialize` itself counts against the 5-second initialization timeout. An error in such a continuation is not a spawn failure — `spawn` has already returned — but it crashes the running actor like any other continuation error. This is where slow setup, or a supervised child's first call to its parent, belongs.
- **Starvation:** the continue chain runs before every other message, including system messages such as supervisor shutdown. A continuation that re-requests itself forever starves the mailbox, as in OTP.
- **Only on `self`:** `actor continueWith: #x` sent from another process (sync, async or `!`) raises `invalid_continuation` in the caller.

### Deferred Replies (`deferReply`)

`self deferReply` takes the reply token of the sync call being handled and returns a `ReplyTo`. The method returns without answering: the caller stays blocked until the token is answered with `reply:` or `replyError:`, from any later method of the same actor (`handleInfo:`, another sync method, a `continueWith:` step). It is OTP's `{noreply, State}` with the `From` kept for `gen_server:reply/2`, and it is how a Server implements a blocking read or a flow-control wait without a `receive`.

```beamtalk
Server subclass: Connection
  state: waiting :: Dictionary(Integer, ReplyTo) = #{}

  readData: streamId :: Integer -> Binary =>
    self.waiting := self.waiting at: streamId put: self deferReply
    nil   // ignored: the caller is answered later

  onDataFrame: streamId :: Integer data: data :: Binary =>
    (self.waiting at: streamId) reply: data
    self.waiting := self.waiting removeKey: streamId
```

- **A value:** `ReplyTo` can be stored in state (a field, a `Dictionary`, a `List`). Many callers can be parked at once and answered in any order. `reply: value` makes the caller's send return `value`; `replyError: anError` makes it raise. Both answer `#ok`.
- **Return value ignored:** after deferring, the method's return value is neither encoded nor delivered. A `typed` class needs no `@expect`: the checker skips the return-type check of a method whose body sends `self deferReply` (a conditional deferral also skips it for the non-deferred path).
- **Raise after deferring:** the caller gets that error instead of hanging, and the method's state change is rolled back as for any failed sync call.
- **Silent no-ops:** replying to a caller that already timed out or died, or a second reply to the same caller (the first wins), does nothing.
- **Stopping:** if the actor stops (`stopWith:`, `kill`, a crash) with callers parked, they raise `actor_dead`. `terminate:` may still `reply:` to rescue them.
- **`continueWith:`:** composes; defer in the method and reply from the step.
- **Only on `self`, in a call:** `actor deferReply` from another process (sync, async, `!` or `perform:`) raises `invalid_defer`, as does `self deferReply` from a cast, `handleInfo:`, a continuation, or any code with no sync call in flight. A helper that a sync method calls may defer the outer caller.
- **Cost:** one process-dictionary put/erase per sync call; nothing otherwise.

### BEAM Mapping

| Beamtalk | BEAM |
|----------|------|
| `Server subclass:` | gen_server with `handle_info/2` dispatch |
| `self deferReply` / `replyTo reply:` | `{noreply, State}` / `gen_server:reply(From, _)` |
| `handleInfo: msg` | `handle_info(Msg, State)` callback |
| `self continueWith: #sel` | `{noreply, State, {continue, _}}` + `handle_continue/2` |
| `Actor subclass:` | gen_server with `handle_info/2` ignore stub |
| Timer `spawn_link` | Timer process linked to calling process |

---

## Supervision Trees (ADR 0059)

Beamtalk provides declarative OTP supervision trees via `Supervisor subclass:` and `DynamicSupervisor subclass:`. This is the Beamtalk idiom for "let it crash" fault tolerance — define which actors should be restarted automatically, and how.

### Static Supervisor

Subclass `Supervisor` and override `childSpecs` to return a list of actor classes (or `SupervisionSpec` values for per-child configuration). The supervisor starts all children at startup using OTP `one_for_one` strategy by default.

> **Important:** supervisor configuration (`childSpecs`, `strategy`, `maxRestarts`, `restartWindow`; `childClass`, `maxChildren`, `childShutdown` on `DynamicSupervisor`) is **instance-side** ([ADR 0135](ADR/0135-supervisor-instances-and-start-arguments.md)). The runtime sends each setting to the supervisor being started, from inside its own OTP `init/1`: `self` is that supervisor and `self args` its start args. Reading `self args` and passing `self` as a value are fine; **sending a message to `self`** (`self count`, `self which: X`) fails the start with `#supervisor_start_failed` and a hint, because the supervisor is still starting. See [Instance configuration and start args](#instance-configuration-and-start-args-adr-0135).

```beamtalk
Supervisor subclass: WebApp
  childSpecs => #(DatabasePool HTTPRouter MetricsCollector)
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
app children                              // => the 3 running child objects, in declaration order
(app which: DatabasePool) unwrap           // => Actor(DatabasePool, _)  (running child instance)
(app terminate: HTTPRouter) unwrap        // gracefully stop a single child; Result(Nil, Error)
app stop                                  // stop the supervisor and all children (unchanged — Nil)

// After stop:
WebApp current                            // => nil
```

`terminate:` is **idempotent** — terminating a child that is already gone returns `Result ok: nil`, not an error (see [idempotent-startup convention](#supervisor-idempotent-startup-convention-adr-0080) below).

`which:` and `terminate:` find a child by its class, including one declared `withId:`. When several children share the class, both return `Result error:` with kind `#ambiguous_child` (and `terminate:` terminates nothing); use the by-id API below then.

**By id (BT-3966).** Every child spec has an OTP id: the class name by default, or the `withId:` value (a Symbol or an Integer). These methods work on any supervisor instance — the `supervise` singleton, a `spawn`/`spawnWith:` instance, or a nested supervisor reached through `childWithId:` or `children`:

```beamtalk
Supervisor subclass: ServerSupervisor
  strategy => #restForOne
  childSpecs => #(
    (Listener supervisionSpec withId: #listener withRestart: #permanent),
    (AcceptorPoolSupervisor supervisionSpec withId: #acceptorPool withRestart: #permanent))

server := ServerSupervisor spawn
server whichChildren collect: [:c | c id]       // => #(#listener, #acceptorPool)
listener := (server childWithId: #listener) unwrap
(server terminateChild: #acceptorPool) unwrap   // suspend
(server restartChild: #acceptorPool) unwrap     // resume => the new AcceptorPoolSupervisor
(server terminateChild: #acceptorPool) unwrap
(server deleteChild: #acceptorPool) unwrap      // drop the spec for good
```

| Method | Returns | Errors |
|--------|---------|--------|
| `children` | `List(Object)`: the running children, in declaration order | raises `#stale_handle` |
| `whichChildren` | `List(SupervisorChild)`: one per spec, in declaration order, incl. restarting/stopped | raises `#stale_handle` |
| `childWithId: id` | `Result`: the running child, or `ok: nil` if the spec's child is restarting/stopped | `#child_not_found`, `#type_error`, `#stale_handle` |
| `terminateChild: id` | `Result ok: nil`; idempotent for an unknown or already-gone id | `#terminate_failed`, `#type_error`, `#stale_handle` |
| `restartChild: id` | `Result`: the newly started child | `#child_not_found`, `#child_running`, `#child_start_failed`, `#type_error`, `#stale_handle` |
| `deleteChild: id` | `Result ok: nil` | `#child_not_found`, `#child_running`, `#type_error`, `#stale_handle` |

A `SupervisorChild` is a value with `id`, `child` (nil unless running), `status` (`#running`, `#restarting` or `#stopped`), `type` (`#worker` or `#supervisor`) and `childClass` (nil for a non-Beamtalk module), plus `isRunning` and `isSupervisor`.

> **`restartChild:` needs a `#permanent` or `#transient` child.** The default restart policy is `#temporary`, and OTP removes a temporary child's spec as soon as it terminates, so `restartChild:` on it returns `#child_not_found` (and `terminateChild:` on it again returns `ok: nil`). Declare `withRestart: #permanent` on children you suspend and resume.

**Breaking (BT-3966):** `Supervisor>>children` used to return the child ids as Symbols. It now returns the running child objects, the same as `DynamicSupervisor>>children`; use `whichChildren collect: [:c | c id]` for the ids.

`DynamicSupervisor` has `children -> List(C)`: its live children (restarting ones left out, order unspecified). It copies the whole child list out of the supervisor in one message, O(n) in the number of children, so use it for bulk operations (drain, broadcast) and `startChild:name:` + `ChildClass named:` to find one child. `simple_one_for_one` children have no ids, so `DynamicSupervisor` has no `whichChildren`/`childWithId:`/`restartChild:`/`deleteChild:`; its `terminateChild:` takes the child object.

### Configuration Defaults

Override these instance methods in your subclass to customise restart behaviour:

| Method | Default | Description |
|--------|---------|-------------|
| `childSpecs` | (required) | The children of a static `Supervisor` |
| `strategy` | `#oneForOne` | OTP restart strategy (`#oneForOne`, `#oneForAll`, `#restForOne`) |
| `maxRestarts` | `10` | Max restarts before supervisor gives up |
| `restartWindow` | `60` | Time window (seconds) for `maxRestarts` |
| `childClass` | (required) | The child class of a `DynamicSupervisor` |
| `maxChildren` | `#infinity` | `DynamicSupervisor` child limit |
| `childShutdown` | `nil` | `DynamicSupervisor` child shutdown timeout |
| `initialize` | no-op | Hook run once after a top-level start, in the caller |
| `args` | — | The start args (`nil` for `supervise`); not overridden |

```beamtalk
Supervisor subclass: CriticalApp
  childSpecs => #(Database Cache)
  strategy => #oneForAll       // restart all if any child crashes
  maxRestarts => 3             // give up after 3 crashes in 60 seconds
```

<a id="instance-configuration-and-start-args-adr-0135"></a>

### Instance configuration and start args (ADR 0135)

Configuration is a property of a running supervisor, so it lives on the instance side and is read **from the supervisor being started**, inside its OTP `init/1`. `self` is that supervisor's handle and `self args` its start args, so the same class can be configured differently per instance. `supervise` starts with `nil` args; a supervisor started as a `DynamicSupervisor` child with `startChild: args` gets `args`. OTP restarts replay the same args, so a restarted supervisor recomputes the same configuration. `sup args` also works from outside (it raises `#stale_handle` for a stopped supervisor).

```beamtalk
DynamicSupervisor(Handler) subclass: ConnectionSupervisor
  childClass => self args at: #handler
  maxChildren => self args at: #numConnections
  childShutdown => self args at: #shutdownTimeout

DynamicSupervisor(ConnectionSupervisor) subclass: ServerPools
  childClass => ConnectionSupervisor

pools := ServerPools supervise unwrap
small := (pools startChild: #{#handler => Echo, #numConnections => 10, #shutdownTimeout => 5000}) unwrap
small maxChildren        // => 10
(small args at: #handler) // => Echo
```

Rules for configuration methods:

- **Do not send messages to `self`.** They run inside the supervisor's own `init/1`; a message to the supervisor is a call to itself, which OTP rejects (`calling_self`). The start fails with `#supervisor_start_failed` and the hint "configuration methods run inside the supervisor's init; use `self args` and return specs, do not send messages to `self`". Passing `self` *as a value* (for example in a child's args, so the child can reach its parent) is the intended use.
- **Never synchronously call a supervisor from code it is waiting on**: its configuration methods, those of its nested supervisor children, or the `initialize` of any actor child. The supervisor is blocked in that child's start function during the initial start, every OTP restart and every `startChild:`, so a call to it or an ancestor hangs (from a child's `initialize`, until the 5 s start timeout, which fails the start). The compiler cannot check this.
- **Each setting is a separate send**, so expensive work (reading files, say) belongs in `childSpecs` only.
- **Arg lifetime:** args are replayed on every restart, so any pid or handle in them must outlive the child (an ancestor's handle does; a caller's pid or a socket does not).
- A configuration method that raises, or returns an invalid value (`maxChildren`, `childShutdown`), fails the start once with `#supervisor_start_failed` (or fails the parent's start for a nested supervisor), carrying the error's hint.

The post-start hook is the instance-side `initialize` (it was `class initialize: sup`). It runs once per top-level start, in the caller's process after the supervisor has started, so `self` is the supervisor and messages to it work. If it raises, the supervisor is stopped and the start fails: `spawn` and `spawnWith:` re-raise the error, while `supervise`, `spawnAs:` and `spawnWith:as:` answer `Result error:` with kind `#supervisor_start_failed` (as `Actor spawnAs:` does). Concurrent `supervise` calls on one class are served in arrival order; one that waits too long behind a start that does not finish also answers `#supervisor_start_failed`. Only `class supervisionPolicy` stays class-side. The old class-side forms (`class children`, `class strategy`, …, `class initialize:`) are compile errors with a fix-it: drop `class`, rename `class children` to `childSpecs`, and use `self` for `sup` in `initialize`.

**For Erlang/Elixir developers.** Every OTP concept has a direct counterpart (rows marked † arrive with BT-3966):

| OTP / Elixir | Beamtalk |
|---|---|
| `supervisor:start_link(Mod, Args)` | `MySup spawnWith: args` (`spawn` passes `nil`) |
| `supervisor:start_link({local, N}, Mod, Args)` | `MySup spawnWith: args as: #n` |
| `whereis(N)` | `MySup named: #n` |
| `Mod:init(Args)` | the configuration methods, reading `self args` |
| `{ok, {#{strategy, intensity, period}, Children}}` | `strategy`, `maxRestarts`, `restartWindow`, `childSpecs` |
| `self()` inside `init/1` | `self` inside those methods |
| child spec map; Elixir `{Mod, Arg}` | `Mod supervisionSpec withId: … withArgs: …` |
| `simple_one_for_one` + template | `DynamicSupervisor` + `childClass` |
| Elixir `max_children`; child `shutdown` | `maxChildren`; `childShutdown`, `withShutdown:` |
| `which_children`, `terminate_child`, `restart_child`, `delete_child` | `whichChildren`, `terminateChild:`, `restartChild:`, `deleteChild:` † |
| "never call yourself from `init`" | the same rule; OTP's `calling_self` becomes `#supervisor_start_failed` with a hint |

Three deliberate differences:

1. `init/1` is split into one method per setting, so a subclass overrides `strategy` alone and inherits the rest. Each method is a separate send, so expensive work belongs in `childSpecs` only, not repeated in several settings.
2. `supervise` (idempotent "ensure it is running") has no OTP counterpart; it serves REPL re-evaluation and the `[application]` boot path.
3. `initialize` runs after start in the caller, where an Erlang programmer would put code after `start_link` returns.

<a id="instances-and-arguments-adr-0135"></a>

### Instances and arguments (ADR 0135)

**`supervise` ensures, `spawn` creates.** `supervise` makes sure the one class-named instance is running (registered under the class's module, found by `current`, idempotent). The spawn family makes a new instance per call, so one class can run any number of trees — one per server, per tenant, per test:

| Message (class side) | Registration | Returns |
|---|---|---|
| `supervise` | the class name, idempotent | `Result(Self, Error)` |
| `spawn` / `spawnWith: args` | none (anonymous) | `Self`; raises `#supervisor_start_failed` |
| `spawnAs: name` / `spawnWith: args as: name` | `name` | `Result(Self, Error)`; a taken name is `#name_registered` |
| `named: name` | lookup + class check | `Result(Self, Error)`: `#name_not_registered`, `#wrong_class`, `#type_error` |
| `current` | lookup by class name | `Self` or `nil` (the `supervise` instance only) |

```beamtalk
a := AcceptorSupervisor spawnWith: cfg1
b := AcceptorSupervisor spawnWith: cfg2
a == b                                    // => false
AcceptorSupervisor current                // => nil   (spawned instances are not "current")
(a args at: #port)                        // => cfg1's port
a stop                                    // b keeps running

http := (ServerSupervisor spawnWith: cfg as: #http_server) unwrap
(ServerSupervisor named: #http_server) unwrap == http   // => true
```

Names are atoms: take them from a bounded, statically known set ([ADR 0079](ADR/0079-named-actor-registration.md)), never from runtime data.

- **Starts run in the caller and are unlinked.** All of these start the tree in the calling process — never in the class's process — and then unlink it, like `Actor spawn`: a supervisor that exhausts its restart intensity dies alone, without taking its caller or its class down. Configuration may therefore send to its own class while the tree starts — except when the start is made from inside one of that class's own class methods (`class make => self spawnWith: …`), which run in the class's process like any class method body. To tie a supervisor's lifetime to something, nest it (below) or `stop` it.
- **`initialize` runs once per top-level start** (each `supervise` that started the instance, each spawn), in the caller; never for a nested instance or an OTP restart.
- **Discoverable.** `Node current supervisors` and `Node current processes` list an anonymous top-level instance while it runs and drop it after `stop`.

**Nesting with `supervisionSpec`.** A supervisor class answers `supervisionSpec` like an actor class: type `#supervisor`, shutdown `#infinity`, restart from `class supervisionPolicy` (default `#permanent`). `withId:`, `withArgs:` (the child's `self args`), `withRestart:`, `withShutdown:` and `withName:` (registers the child under a bounded name) apply; `withClassMethod:` and `scope: #global` fail the parent's start with a `#type_error`. Child ids may be Symbols or **Integers**, which suit generated pools and mint no atoms:

```beamtalk
Supervisor subclass: AcceptorPoolSupervisor
  childSpecs =>
    ((1 to: (self args at: #numAcceptors)) collect: [:i |
      AcceptorSupervisor supervisionSpec withId: i withArgs: #{#id => i, #parent => self}]) asList

pool := AcceptorPoolSupervisor spawnWith: #{#numAcceptors => 3}
pool count       // => 3
pool children    // => #(1, 2, 3)
```

A bare supervisor class in `childSpecs` means `X supervisionSpec`, so it is an **anonymous** child: two parents that nest the same bare class run side by side, and `InnerSup current` does not find a nested instance (use `parent which: InnerSup`). A `DynamicSupervisor` whose child class is a supervisor starts a new anonymous child per `startChild` / `startChild: args`, and `startChild: args name: n` registers it.

**The parent handle and the hang rule.** Passing `self` in a child's args (`#parent => self` above) is how a child reaches its parent; replaying it on restart is safe because a child never outlives its parent. **Never synchronously call a supervisor from code it is waiting on**: its configuration methods and those of its nested supervisor children (a call to an ancestor there hangs; a call to `self` fails fast), and the `initialize` of any actor child (a call back stalls 5 s, then fails the start with a hint naming this rule). The same goes for a call from that `initialize` to whoever is starting the supervisor or calling `startChild:` (an owner actor that passed `#owner => self`), which is blocked in that start too. This holds during the initial start, every OTP restart and every `DynamicSupervisor startChild:`. A continuation the child requests in `initialize` (`self continueWith:`) runs after its start has returned, so that is the place for a child's first call to its parent.

**A child's `initialize` completes before its start returns.** Every supervised actor child starts like `spawnWith:`: the supervisor waits for the child's `initialize` before starting the next child (children start in order, as in OTP) and before its own start returns. An `initialize` that raises fails the start: `spawn` raises, and `supervise`/`spawnAs:` answer `Result error:`, with kind `#supervisor_start_failed` and the child's error in the hint (for `startChild`, `#child_start_failed`), instead of the child crash-looping after the start reported success. This is OTP's `init/1` returning `{stop, Reason}`. On an OTP restart a failing `initialize` fails that restart, which counts against `maxRestarts`. A slow `initialize` delays the whole tree's start, and one that takes over 5 s fails it, so put slow or parent-dependent setup in a `continueWith:` step:

```beamtalk
Actor subclass: Listener
  state: port = 0
  state: parent = nil

  initialize =>
    self bind: self.port          // a raise here fails the supervisor's start
    self continueWith: #register  // runs after the start has returned

  register => self.parent count   // safe: the parent is no longer waiting
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
  childSpecs =>
    #(DatabasePool
      HTTPRouter supervisionSpec withRestart: #transient
      (MetricsCollector supervisionSpec withId: #metrics withArgs: #{#port => 9090}))
```

Use `withShutdown:` to set how long a child gets to stop when its supervisor shuts it down — for children that need time to drain connections or flush state. The value is a `ShutdownTimeout`: an Integer (milliseconds, at most 4294967295), `#infinity` (wait as long as it takes) or `#brutalKill` (kill at once; `terminate:` does not run). The default is 5000ms for workers and `#infinity` for nested supervisors. On shutdown the supervisor sends the child `exit(Pid, shutdown)`; a child that has not exited within the timeout is killed, and `onExit:` observers see the reason `#killed`. Only an actor that overrides `terminate:` traps exits, so only such an actor uses the time — any other actor exits as soon as it is told to. `stop` waits for the shutdown to finish, so a long timeout (or `#infinity`) can block the caller of `stop` that long. Any other value makes `supervise` return `Result error:` with kind `#supervisor_start_failed`.

```beamtalk
HttpServer supervisionSpec withShutdown: 30000        // 30s graceful shutdown
SessionCache supervisionSpec withShutdown: #brutalKill // no cleanup needed
```

Use `withName:` (and the `withName:withRestart:` / `withName:withArgs:` / `withName:withRestart:withArgs:` combinators) to have the supervisor register the child atomically under a Symbol name on each restart. Named specs emit a `#spawnAs:` / `#spawnWith:as:` startFn so re-registration happens in the same OTP call that starts the process — held `Actor named:` references survive restarts (see [ADR 0079](ADR/0079-named-actor-registration.md) and the Actor Named Registration section below). `name` and `classMethod` cannot be combined on the same spec.

```beamtalk
Supervisor subclass: WebApp
  childSpecs =>
    #((Counter supervisionSpec withName: #counter withRestart: #permanent))
```

### Dynamic Supervisor

Subclass `DynamicSupervisor` to manage pools of actors started at runtime. Override `childClass` to declare which actor class the pool manages.

```beamtalk
DynamicSupervisor(Worker) subclass: WorkerPool
  childClass => Worker
```

```beamtalk
pool := (WorkerPool supervise) unwrap
// => DynamicSupervisor(WorkerPool, _)

// Start children dynamically — startChild returns Result(C, Error) where C is
// the DynamicSupervisor's child class parameter (Worker here)
w1 := pool startChild unwrap        // => Actor(Worker, _)
w2 := pool startChild unwrap        // => Actor(Worker, _)
pool count                          // => 2

// Start a child with custom init args — args is passed to the child's init/1
// the same way ActorClass spawnWith: args would
w3 := (pool startChild: #{#label => "y"}) unwrap   // => Actor(Worker, _)

// Recoverable variant — useful when a failing init should not abort the caller
pool startChild
  ifOk:    [:w | w process: 21]
  ifError: [:e | Logger warn: e message]
pool count                          // => 4

// Terminate a specific child — idempotent (Ok(nil) even if already gone)
(pool terminateChild: w1) unwrap    // => nil
pool count                          // => 3

// Stop the whole pool (unchanged — Nil, let-it-crash teardown)
pool stop
WorkerPool current                  // => nil
```

**Capping the pool with `maxChildren`.** Override `maxChildren` (default `#infinity`) to bound the number of live children — the Beamtalk equivalent of Elixir `DynamicSupervisor`'s `max_children`. Once the pool holds that many children (`count`, plus any child whose automatic restart is pending), `startChild`, `startChild:` and `startChild:name:` return `Result error:` with kind `#max_children` (the error's details carry the limit) instead of starting a child; terminating a child frees a slot. Automatic restarts of existing children are never refused. The value must be a non-negative Integer or `#infinity` — anything else makes `supervise` return `Result error:` with kind `#supervisor_start_failed` (or, for a pool nested under a static `Supervisor`, fails the parent's start) — and is read from the instance each time the supervisor (re)starts, so it may depend on `self args`.

```beamtalk
DynamicSupervisor(Worker) subclass: ConnectionPool
  childClass => Worker
  maxChildren -> Integer => 2

pool := (ConnectionPool supervise) unwrap
w := pool startChild unwrap        // => Actor(Worker, _)
pool startChild unwrap             // => Actor(Worker, _)
pool startChild error kind         // => #max_children
(pool terminateChild: w) unwrap    // => nil
pool startChild isOk               // => true
```

The limit is checked by the calling process just before the start, so it is exact when a single process starts children (e.g. one acceptor per pool) — concurrent starters racing past the check can briefly overshoot it by up to the number of racers.

**Graceful child shutdown with `childShutdown`.** Override `childShutdown` to set how long each child gets to stop when the pool is stopped (by `stop`, or by a parent supervisor) — a pool-wide analogue of the `shutdown:` option Elixir `DynamicSupervisor` takes per child. It takes the same `ShutdownTimeout` values as `SupervisionSpec withShutdown:` (an Integer in milliseconds, `#infinity` or `#brutalKill`). The default, `nil`, keeps the child class's own `supervisionSpec` value: 5000ms (the OTP default) for an actor, or `#infinity` for a supervisor. A child that overrides `terminate:` gets the full time to finish; one that overruns is killed (`onExit:` reason `#killed`). All children are told to stop at once and share one deadline. Keep `#infinity` for a supervisor child class: a finite value can kill it while it is still stopping its own children (the runtime logs a warning). The value applies to every child of the pool — OTP's `simple_one_for_one` keeps a single child spec for all dynamic children, so there is no per-`startChild:` override; the value is per pool instance, so compute it from `self args` (or use a separate pool class) for children that need a different timeout. An invalid value makes `supervise` return `Result error:` with kind `#supervisor_start_failed`.

```beamtalk
DynamicSupervisor(Connection) subclass: ConnectionPool
  childClass => Connection
  childShutdown -> Integer => 15000   // let in-flight requests finish
```

**Automatic restart replays per-child args.** OTP tracks the exact args each dynamically-started child was started with. A child started via `startChild: args` that later crashes under a `#permanent` or `#transient` restart policy comes back with those same args, not blank defaults. A child started via the no-arg `startChild` restarts the same way. Restart re-runs `init/1` from scratch — it does not resume the child's prior runtime state.

**Named dynamic children — identity that survives restart.** `startChild:` alone gives you a pid, and a `simple_one_for_one`-restarted child comes back as a *different*, unlabelled pid — there is no `which:`/`children` method on `DynamicSupervisor` to rediscover it (see below). `startChild: args name: aSymbol` closes that gap by combining args-replay with [named actor registration](ADR/0079-named-actor-registration.md): the child starts under `{local, aSymbol}` registration, and because OTP replays each dynamic child's own start args (including the name) on automatic restart, a crashed named child re-registers under the same name every time.

```beamtalk
DynamicSupervisor(Monitor) subclass: CheckRegistry
  childClass => Monitor

pool := (CheckRegistry supervise) unwrap
m := (pool startChild: #{#check => "db"} name: #dbCheck) unwrap
// => Actor(Monitor, _)

// ... time passes; #dbCheck's process crashes and OTP restarts it ...

current := (Monitor named: #dbCheck) unwrap   // re-resolves to the NEW pid
current isAlive                                // => true
```

`aSymbol` should come from a bounded, statically-known namespace (e.g. a fixed set of configured checks/workers) — the same atom-exhaustion guidance that applies to `Actor>>spawnAs:` applies here, since names are Erlang atoms.

**Why no `which: id` / `children` method on `DynamicSupervisor`.** Static `Supervisor>>which:` and `>>children` (below) work because OTP's plain `one_for_one` gives every statically-declared child a stable childspec id. `DynamicSupervisor`'s `simple_one_for_one` children are anonymous — matched only by pid — and `supervisor:which_children/1` is a documented performance cliff at scale (it copies the whole child list out of the supervisor process in one message). `startChild:name:` + `Actor named:` gives "find this dynamic child again by a stable identity, surviving restart" without either limitation: a `whereis/1`-backed point lookup, not a full-list scan. This differs from `DynamicSupervisor>>count`, which only reports an aggregate number with no per-child identity at all.

### Nested Supervisors

Supervisors can be nested — include another supervisor class in `childSpecs`:

```beamtalk
Supervisor subclass: AppRoot
  childSpecs => #(DatabaseSupervisor WebTierSupervisor MetricsSupervisor)
```

A nested supervisor class goes through its `supervisionSpec` like any child (type `#supervisor`, shutdown `#infinity`, restart `#permanent` by default), so it is an anonymous child linked into the tree; use `X supervisionSpec withId: … withArgs: …` to give it an id or start args, and nest several of one class (see [Instances and arguments](#instances-and-arguments-adr-0135)). The outer supervisor shuts down inner supervisors (and all their children) gracefully on `stop`.

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
| `Supervisor class>>spawnAs: n` / `spawnWith: args as: n` (both bases) | `-> Result(Self, Error)` | `#name_registered`, `#reserved_name`, `#type_error`, `#supervisor_start_failed` |
| `Supervisor class>>named: n` (both bases) | `-> Result(Self, Error)` | `#name_not_registered`, `#wrong_class`, `#type_error` |
| `Supervisor>>terminate: aClass` | `-> Result(Nil, Error)` | `#terminate_failed`, `#stale_handle` |
| `Supervisor>>which: aClass` | `-> Result(Object, Error)` | `#stale_handle` |
| `DynamicSupervisor class>>supervise` | `-> Result(Self, Error)` | `#supervisor_start_failed`, `#stale_handle` |
| `DynamicSupervisor>>startChild` / `startChild: args` | `-> Result(C, Error)` | `#child_start_failed`, `#max_children`, `#stale_handle` |
| `DynamicSupervisor>>startChild: args name: aSymbol` | `-> Result(C, Error)` | `#child_start_failed`, `#max_children`, `#name_registered`, `#reserved_name`, `#stale_handle` |
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
- `#node_down` — the handle names a supervisor on another node that cannot be reached (down, partitioned, or too slow to answer). The supervisor may still be running, so this is not a stale handle; `details` carries `node` and `reason` (`#nodedown`, `#noconnection` or `#timeout`). Every supervisor handle call (`args`, `count`, `stop`, the child calls above) reports it the same way, where a local handle would report `#stale_handle`.

`isAlive` and `stop` are sealed on `Supervisor` and `DynamicSupervisor`: the runtime answers them on a handle without a method lookup, so overriding either is a compile error. `isAlive` on a remote supervisor answers `true` without a network round trip, as it does for an actor.

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
| `supervise` | `supervisor:start_link({local, Module}, Module, nil)`, in the caller, then `unlink` |
| `spawn` / `spawnWith: args` | `supervisor:start_link(Module, Args)`, in the caller, then `unlink` |
| `spawnAs: n` / `spawnWith: args as: n` | `supervisor:start_link({local, N}, Module, Args)`, then `unlink` |
| `named: n` | `whereis(N)` plus a class check |
| `current` | `whereis(Module)` |
| nested `X supervisionSpec` | child spec `{beamtalk_supervisor, start_supervisor_child, [Module, Args]}` → `Module:start_link(Args)` |
| `count` | `supervisor:count_children/1` (active count) |
| `children` | `supervisor:which_children/1` (running child ids) |
| `which: Class` | find child by module in `which_children` result |
| `withShutdown:` | `shutdown` field in child spec (default 5000ms workers, infinity supervisors; `#brutalKill` → `brutal_kill`) |
| `childShutdown` | `shutdown` field of the `simple_one_for_one` child spec |
| configuration methods | `Module:init(Args)`, run in the supervisor process |
| `args` | the `Args` passed to `init/1` |
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

Where supervision *syntax* declares a tree, **`Node current processes`** lets you
walk the *live* one — the dynamic counterpart to `Node current actors`, and the
process-structure twin of `SystemNavigation`. It returns a navigable
`SupervisionTree` snapshot of the running OTP supervision tree, built as a thin
wrapper over `supervisor:which_children` (no new bookkeeping process).

```beamtalk
tree := Node current processes unwrap   // == ProcessNavigation default tree
tree root                          // => the snapshot root SupervisionNode
tree size                          // => total node count
tree do: [:node | Transcript showCr: node printString]
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
best-effort point-in-time view — re-call `Node current processes` to refresh. A node
whose pid has since died is detected lazily: `node status` returns `nil` rather
than raising.

**Lazy state.** `node status` is *not* captured at snapshot time. Calling it
issues a timeout-guarded `sys:get_status` against the node's pid *then* —
returning a `Dictionary` for an alive, `sys`-compliant process, or `nil` for one
that is dead, timed out, or not `sys`-compliant.

**Scopes and scale.** `ProcessNavigation default` (what `Node current processes`
wraps) filters runtime plumbing; `ProcessNavigation system` shows everything,
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

The pre-ADR pattern uses a supervisor-local lookup (`which:`) and an `initialize` hook to re-wire dependencies after each restart:

```beamtalk
typed Supervisor subclass: ExduraSupervisor
  strategy -> SupervisionStrategy => #restForOne
  childSpecs -> List(SupervisionSpec) =>
    storeSpec := EventStore supervisionSpec withRestart: #permanent
    poolSpec := ActivityWorkerPool supervisionSpec withRestart: #permanent
    engineSpec := WorkflowEngine supervisionSpec withRestart: #permanent
    #(storeSpec, poolSpec, engineSpec)

  // Runs after the initial start to cache pids (not after restarts).
  initialize -> Nil =>
    store := (self which: EventStore) unwrap
    pool := (self which: ActivityWorkerPool) unwrap
    engine := (self which: WorkflowEngine) unwrap
    engine initWithStore: store pool: pool
    nil
```

#### After named registration

Naming each child eliminates the `initialize` hook, and the supervisor strategy is freed from the rewire-on-restart constraint:

```beamtalk
typed Supervisor subclass: ExduraSupervisor
  strategy -> SupervisionStrategy => #oneForOne
  childSpecs -> List(SupervisionSpec) => #(
    EventStore supervisionSpec withName: #eventStore withRestart: #permanent,
    ActivityWorkerPool supervisionSpec withName: #workerPool withRestart: #permanent,
    WorkflowEngine supervisionSpec withName: #workflowEngine withRestart: #permanent
  )
  // No initialize hook — WorkflowEngine looks up its dependencies by name.
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

// Binary patterns (BT-663; BT-4096, BT-4097)
(Binary fromBytes: #(3, 104, 105, 33, 99)) match: [
  <<n:8, payload:n/binary, rest/binary>> -> payload toBytes;
  _ -> #()
]
// => #(104, 105, 33): a size may be a literal, a variable bound by an
// earlier segment, a parameter/local, or a parenthesized expression.
(Binary fromBytes: #(10, 9)) match: [
  <<0:1, v:7, rest/binary>> -> v;
  _ -> -1
]
// => 10: integer literal values match exactly.
(Binary fromBytes: #(234, 7)) match: [
  <<_:3, v:5, rest/bits>> -> v;
  _ -> -1
]
// => 10: bits/bitstring match possibly-unaligned bits.
(Binary fromBytes: #(234)) match: [
  <<_:3, rest/bits>> -> rest class;
  _ -> nil
]
// => Bitstring: an unaligned tail is a Bitstring (ADR 0141); use
// bitSize/toBits on it, not the byte protocol. An aligned tail is a Binary.

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

**Binary patterns** match bytes and bits: `<<first:8, _/binary>>` binds the first byte, `<<_:8, rest/binary>>` the tail. A segment is `name`, `name:size`, `name/type`, or `name:size/type-modifiers`, where the size is an integer literal, a variable, or a parenthesized expression, and the type is `integer` (default), `float`, `binary`/`bytes`, `bits`/`bitstring`, or `utf8` with `signed`/`unsigned` and `big`/`little`/`native` modifiers. A size may name a variable bound by an earlier segment of the same pattern (`<<n:8, payload:n/binary>>`), a parameter, or a local; the value may be an integer literal (`<<0:1, v:7>>`). A size that names an earlier segment of the *same* binary, or a size expression, is only supported when the binary pattern is the whole arm pattern — match the enclosing value first, then the binary. A binary nested inside a tuple in a `match:` arm is matched after the tuple, so its sizes may name a variable the tuple or an earlier nested binary binds (`{n, <<head:n/binary, _/binary>>}`). Destructuring assignment does not accept nested patterns. A binary pattern matches a `String` subject by its UTF-8 bytes, whether it is the whole arm pattern or nested inside a tuple (`{#ok, <<71, 69, 84, 32, rest/binary>>}` matches `{#ok, "GET /x"}`); `/binary` segments always bind a `Binary` (BT-4173, BT-4223). A `/bitstring` (`/bits`) segment binds whatever the bits are: a `Binary` when they are a whole number of bytes, otherwise a [`Bitstring`](#bitstring--unaligned-bits) (ADR 0141), which understands only bit-level messages (`bitSize`, `toBits`, `concat:`, `asBinary`). A type pattern `b :: Bitstring` matches exactly the unaligned values; `b :: Binary` never matches one. Every `_` in a pattern is a fresh anonymous variable, so `{#a, _, x, _}` compiles.

**An arm body is an expression, not a block.** `pattern -> [ ... ]` answers the block as a value and does **not** run its statements: `1 match: [1 -> [2 + 3]; _ -> [0]]` answers a `Block`, not `5`. A zero-parameter block body alongside non-block arms warns (`MatchArmBlock`: "this `match:` arm answers a Block without running it"); a match whose every arm is a block literal is read as an intentional dispatch table and stays silent. In an actor or `Server` method, such a block that writes a field or sends to `self` (`#tick -> [self.count := self.count + 1]`) is additionally a compile error. For an arm that needs several statements, call a method (`#tick -> self tick`), or evaluate the block in place (`#tick -> [ ... ] value`). A single field write can be the arm itself (`{#tick, n} -> self.count := self.count + n`). Suppress a deliberate block-valued arm with `@expect match_arm_block`, or the whole package with `match-arm-block = "off"` under `[diagnostics]`.

**Arms do not thread assignments.** An arm body is not a block, so assigning an
enclosing local inside one has no effect: the compiler warns (`Assignment to
`x` inside a match arm has no effect`) and `--warnings-as-errors` rejects it.
Bind the match's answer instead: `timeout := value match: [...]`.

**Type patterns** test the runtime class of the scrutinee and bind the value to the named variable, narrowed to that class. Subsequent arms see the scrutinee type narrowed by difference (e.g. after a `s :: String` arm, the remaining arms see the type minus `String`).

**Phase A scope (ADR 0107):** type patterns are restricted to concrete/leaf classes only — `binding :: SomeClass` where `SomeClass` has subclasses is a compile error (`"SomeClass has subclasses; type patterns are not yet supported for non-leaf classes"`), never silently-wrong matching. Subclass-polymorphic matching (`binding :: Shape` where `Shape` has subclasses `Circle`/`Square`) is deferred to a future ADR 0107 Phase B. Supported classes today: `String`, `Integer`, `Float`, `List`, `Dictionary`, `Boolean` (uses the `is_boolean/1` BIF; `True`/`False` use exact atom guards — all three are stdlib primitives that bypass the leaf-restriction via a BIF/atom-guard check, not the user-class hierarchy check a `Shape`/`Circle`-style class would go through), `True`, `False`, `Symbol`, `Nil`, `UndefinedObject`, `Block`, `Pid`, `Reference`, `Port`, `Bitstring` (ADR 0141: `is_bitstring` and not `is_binary`, so it never matches a `Binary`), user-defined `Value` subclasses, `Actor` subclasses, and `Supervisor`/`DynamicSupervisor` subclasses (BT-2870).

**`Binary` is the one non-leaf exception (BT-4228).** `Binary` is `sealed` and `String` is its only subclass, so `b :: Binary` is accepted and compiles to a closed, constant-time *subclass-aware* test: it matches a bare binary (valid UTF-8 or not) **or** a `String`, agreeing with `isKindOf: Binary`. To tell bytes from text, put the `String` arm first:

```beamtalk
value match: [
  s :: String -> #text;
  b :: Binary -> #bytes;   // only bare binaries reach here
  _ -> #other
]
```

For exhaustiveness checking, a `b :: Binary` arm covers a `String` member of the scrutinee type, but a `Binary` member is not treated as a closed leaf (so `matchExhaustive:` over a `Binary | Nil` scrutinee cannot be proven; use `match:`).

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
| Tuple | `{a, b}` | Destructure tuple in assignment and match arms (`{a, b}` is also a tuple literal expression — see Tuple Literals) |
| Array | `#[a, b]` | Match and destructure an Array by exact size; nested arrays supported |
| Array rest | `#[a, ...rest]` | Destructure first elements, bind remaining to a sub-array (destructuring assignment only) |
| Dict/Map | `#{#k => v}` | Match a Dictionary containing key `#k`, bind value to `v`; partial match (other keys ignored) |
| Constructor | `Result ok: v` | Match sealed type by constructor (Phase 1: Result only) |
| Binary | `<<n:8, payload:n/binary>>` | Match bytes/bits; sizes may name earlier segments, params/locals, or parenthesized exprs; `bits` for unaligned tails (BT-663, BT-4096, BT-4097) |
| Nil | `nil` | Matches `nil` exactly; narrows subsequent arms to exclude `Nil` (BT-2854, ADR 0107) |
| Type | `x :: String` | Bind `x` and test runtime class; `x` is narrowed to the named class in the arm body and guard. Phase A: leaf/concrete classes only (plus the sealed `Binary`, subclass-aware) — see restrictions below (BT-2855, ADR 0107) |

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

**Pattern failures (BT-3943):** a destructuring assignment whose tuple size, literal slot or array shape does not match, and a `match:` with no matching arm, raise a structured `#pattern_mismatch` error (class `RuntimeError`, the offending value in the error details). A map destructure with a missing key raises `#key_error` instead.

```beamtalk
[{#ok, _v} := {#error, 42}] on: Error do: [:e | e kind]
// => #pattern_mismatch
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

> **Note:** Tuple destructuring works in both assignment (`{x, y} := expr`) and `match:` arms. A tuple pattern may end with a trailing comma (`{x, y,} := expr`), like the tuple literal it mirrors. `collect:` with pattern blocks is not yet supported.

### Value of a Destructuring Assignment (BT-3889)

A destructuring assignment evaluates to its right-hand side, as `x := expr` evaluates to `expr`. Destructuring is a statement, so this is the value of a body whose last statement it is: a method (class, value-type and actor methods alike), a block, a conditional arm, a protected block (`on:do:`, `ensure:`), and the element, accumulator or key a `collect:`, `inject:into:` or `groupBy:` body answers:

```beamtalk
swap => {a, b} := self pair       // answers the pair, not nil

#(1, 3) collect: [:x | #(_a, _b) := #(x, 2)]
// => #(#(1, 2), #(3, 2))
```

A destructure that ends a predicate block (`select:`, `detect:`, `allSatisfy:` and the like) passes its right-hand side, a list, tuple or map, as the predicate, so it raises like any other non-boolean predicate. A `groupBy:` is a fold like `collect:` ([BT-3897](https://linear.app/beamtalk/issue/BT-3897), lowered by BT-3912): in an actor method or at the REPL its block may bind a local, write a field or send to `self`, and the grouped dictionary is the value wherever the `groupBy:` sits.

In an actor method, an evaluated block literal (`[...] value`, `[:k | ...] value: x`, `value:value:`) that binds a local, writes a field or sends to `self` answers its value wherever it sits, and its writes are visible to the operands and statements after it: `{[:y | self.count := y] value: 5, self.count}` is `{5, 5}` ([BT-3982](https://linear.app/beamtalk/issue/BT-3982)). Its arguments are evaluated first, in order. One that assigns a local of its own answers its value inside a `to:do:`, `timesRepeat:`, `whileTrue:` or `whileFalse:` body or condition too ([BT-3947](https://linear.app/beamtalk/issue/BT-3947); BT-3948 had made it a compile error in an actor method), and one whose block writes an outer local is a local-threading producer like a conditional (see [Capturing Variables](#what-works-and-what-doesnt); [BT-4263](https://linear.app/beamtalk/issue/BT-4263) for the `value:` family). Class and value-type methods answer the value everywhere.

The REPL displays the same value. Destructuring is not an expression, so it cannot appear as an operand (`r := ({a, b} := pair)` is a compile error).

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
`#newClasses`, `#removedClasses`, `#skipped`, and `#conflicts`. A non-empty
`#conflicts` list means the listed entries remain pending and require manual
reconciliation; a non-empty `#skipped` list means a pending destructive
(Tier 2) entry was withheld — see
[Destructive flush](#destructive-flush--flushincludingdestructive-and-confirmdestructive-adr-0113)
below.

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
> Workspace newClass: "Object subclass: Greeter\n  greet => ""hello""" at: "src/greeter.bt"
=> [Greeter]                        // compiled and installed in memory
> Workspace flush
=> _                                // writes src/greeter.bt
```

`newClass:at:` raises a loud, specific error (no silent fallback) if `path`
already exists, lies outside the project tree, the declared class name does
not match the path basename (ADR 0040 one-class-per-file convention), or a
class of that name is already loaded.

#### Removing methods — `removeSelector:` and `removeSelector:ifAbsent:` (ADR 0112)

`removeSelector:` completes `Behaviour`'s patch/create/remove trio alongside
`compile:source:` and `newClass:at:`: it drops a method from the receiver's
own method table (instance-side) or the receiver's metaclass's method table
(class-side), then logs a durable `kind: #'remove-method'` ChangeEntry —
exactly the ChangeLog participation a patch gets, just deleting text instead
of replacing it.

```beamtalk
sealed removeSelector: aSelector :: Symbol -> Behaviour
sealed removeSelector: aSelector :: Symbol ifAbsent: absentBlock :: Block(T) -> Behaviour | T
```

```beamtalk
Counter removeSelector: #increment          // instance-side: touches instance_methods
Counter class removeSelector: #ofSize:      // class-side: touches class_methods
```

**Receiver side follows the existing `Counter` vs `Counter class` convention**
already used to pick a side for `>>` extension definitions — there is no
boolean parameter. Both forms return the receiver (`Behaviour`) on success, so
removals chain the same way patches do: `Counter removeSelector: #a; removeSelector: #b`.

Because dispatch always walks the class chain live (ADR 0032 — no flattened
method-table cache), removing an overriding method instantly re-exposes
whatever the superclass defines, with no restart and no cache to invalidate:

```beamtalk
Actor >> initialize => Transcript show: 'base init'
Actor subclass: Counter
  state: value = 0

  initialize => Transcript show: 'counter init'

Counter new                    // prints 'counter init'
Counter removeSelector: #initialize
Counter new                    // prints 'base init' — no restart needed
```

`removeSelector:` also reaches extension methods (ADR 0066 open classes) —
sent to the *target* class, the same way an extension is installed via `>>`:

```beamtalk
String >> shout => self uppercase ++ "!"
"hi" shout                     // => "HI!"
String removeSelector: #shout
"hi" respondsTo: #shout        // => false
```

**The bare form raises; `removeSelector:ifAbsent:` is the paired escape
hatch** — mirroring `Dictionary>>at:ifAbsent:` and Pharo's own
`removeSelector:`/`removeSelector:ifAbsent:` pairing. A selector that is
absent locally — never defined, already removed, or only *inherited* — raises
a structured error with kind `selector_not_found`, deliberately distinct from
`does_not_understand` (the message `removeSelector:` itself was understood
and executed; only its argument had nothing to act on):

```beamtalk
[Counter removeSelector: #bogus] on: Error do: [:e | e kind]
// => selector_not_found

Counter removeSelector: #bogus ifAbsent: ["not found"]
// => "not found"

// #printString is inherited from Object, not local to Counter, so it still
// raises — includesSelector: distinguishes "responds to" from "defines locally".
Counter includesSelector: #printString    // => false
Counter removeSelector: #printString      // raises selector_not_found
```

`removeSelector:ifAbsent:`'s block runs only on absence and its value is
returned instead of the receiver.

> **Process semantics note.** Like every `Behaviour` tower primitive,
> `removeSelector:ifAbsent:` runs in the *caller's* process (the Class →
> Behaviour chain-walk fallthrough), not the receiver class's gen_server. The
> `absentBlock` messaging the receiver class back is an ordinary cross-process
> send — no `dispatch_error` restriction applies here (unlike a block argument
> received by a locally-defined class method of a class that keeps its class
> process; see
> [Passing Blocks Through Class Methods](#passing-blocks-through-class-methods)).

**`removeSelector:` never refuses based on where a class lives — it always
installs the removal in memory. What varies is whether the resulting
ChangeEntry is flushable**, exactly mirroring `compile:source:`'s rule (see
[Flushability](#flushability--what-flush-writes) below) rather than
`removeFromSystem`'s outright block (see
[`removeFromSystem`](#removefromsystem--removing-a-whole-class-bt-785-adr-0113)):

```beamtalk
Integer removeSelector: #printString      // installs in memory, no error
Workspace changes dirtyMethods             // => #{#Integer => #{#printString}}
Workspace flush                            // skips it: not flushable (stdlib)
```

See [ADR 0112](ADR/0112-method-level-removal-language-primitive.md) for the
full design, including extension-removal edge cases, dangling-sender risk,
and the `side` field's role in the ChangeLog schema.

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
| `flushKinds: kinds` | `FlushResult` | Flush only entries matching a Set of `#instance` / `#class` / `#'new-class'` / `#'remove-method'` / `#'remove-class'` / `#'rename-class'` / `#'rename-method'` / `#'class-def'` / `#human` / `#agent` symbols (both dimensions AND together) |
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

The dependent lookup is keyed by selector *and* the receiver's inferred type
(ADR 0115): a candidate sender is dropped before it is ever re-checked when
its recorded receiver type could not dispatch to the changed class — an
unrelated class hierarchy, a protocol the changed class does not conform to,
or the wrong side (a `Counter class` receiver is a dependent of a change to
`Counter`'s class-side methods, never its instance-side ones).

Narrowing is permissive by construction: a send the compiler could not type
(`dynamic`), a send indexed by the runtime live-patch path (which has no
type-checker access and records `dynamic` unconditionally), a row compiled
before ADR 0115, and a receiver type that resolves to no loaded class or
protocol are all kept as candidates. Those still re-check and let
the checker's own type inference decide relevance — a `size` sender on an
unrelated class simply re-checks clean. Fan-out is capped per reload (with a
"N more not checked" note), now as a backstop over the narrowed pool rather
than over every syntactic sender of the selector; one level of fan-out only,
not transitive; and proxy-routed calls (ADR 0104 §4 forwarding) are invisible
to xref, so a proxy-wrapped caller can go unflagged — see
[ADR 0105](ADR/0105-live-image-recheck-on-reload.md) and
[ADR 0115](ADR/0115-xref-receiver-type-key.md) for the full mechanism,
severity rules, and accepted gaps.

### Shape Versioning (ADR 0123)

Reload's structural fallback (add a defaulted field, drop a removed one,
matching by name) handles additive changes for free — it always has. What it
cannot express is a **rename**, a **retype**, or a field **derived** from
others: dropping one field and adding another loses data instead of
transforming it. `shapeVersion:` and `migrateFromVN:` are the two additions
that let a class author say, in ordinary Beamtalk, how a live instance's
state gets from one shape to the next — the same problem hot reload,
persistence, and cross-node distribution all share, solved once.

#### `shapeVersion:` — declaring a version

A class declares its shape version with a **class-header clause**, on its
own line before any `state:`/`field:` lines — the same position
`handleScope:` occupies:

```beamtalk
Actor subclass: Cart
  shapeVersion: 2
  state: items :: List = #()
  state: total :: Integer = 0
```

- The argument is a **positive integer literal** — the compiler must read it
  without evaluating code.
- **Absent means `shapeVersion: 1`.** Every class that predates this feature
  is implicitly at version 1; nothing in an existing corpus needs to change.
- At most one `shapeVersion:` per class, and it is **not inherited**:
  versions and migration chains are per **concrete** class, over the
  flattened (inherited-fields-included) field set — because that is what an
  instance's actual state map holds. A subclass's chain is only the
  `migrateFromVN:` methods *it* defines; to reuse an ancestor's step, call it
  explicitly (`Base migrateFromV1: old`).
- `Cart shapeVersion` reads it back (`Behaviour >> shapeVersion`, sealed,
  defaults to `1` when undeclared):

```beamtalk
Cart shapeVersion
// => 2

Counter shapeVersion    // no shapeVersion: clause declared
// => 1
```

- `native:` classes cannot declare `shapeVersion:` (or `migrateFromVN:`) at
  all — they compile to a facade with no generated `init/1`/`code_change/3`
  to carry a version (ADR 0056); the compiler rejects it.

#### `class migrateFromVN:` — one method per step

A migration from shape *N* to shape *N+1* is a **class-side method**,
`migrateFromVN:`, taking the old fields as a `Dictionary` and returning the
new fields as a `Dictionary`:

```beamtalk
Actor subclass: Cart
  shapeVersion: 2
  state: items :: List = #()
  state: total :: Integer = 0

  /// v1 had only `items`; v2 caches their sum.
  class migrateFromV1: old :: Dictionary -> Dictionary =>
    old at: #total put: (old at: #items) sum
```

A rename the structural fallback cannot express (it would drop `owner` and
default `ownerName` instead of carrying the value across):

```beamtalk
Actor subclass: Account
  shapeVersion: 3
  state: balance :: Integer = 0
  state: ownerName :: String = ""

  class migrateFromV2: old :: Dictionary -> Dictionary =>
    (old at: #ownerName put: (old at: #owner)) removeKey: #owner
```

A `Value`, retyping a field (integer cents → float amount) and adding one:

```beamtalk
Value subclass: Money
  shapeVersion: 2
  field: amount :: Float = 0.0
  field: currency :: Symbol = #USD

  class migrateFromV1: old :: Dictionary -> Dictionary =>
    (old at: #amount put: (old at: #cents) / 100.0) removeKey: #cents
```

Rules, all checked statically:

- Recognised only on the **class side**, with exactly one parameter; `N`
  must be a positive integer.
- A method with `N ≥ shapeVersion` is a **warning** — unreachable, since the
  chain never runs a step at or past the current version:

  ```beamtalk
  Actor subclass: Cart
    shapeVersion: 2
    class migrateFromV7: old => old
  // ⚠️ warning: `migrateFromV7:` is unreachable — Cart's shapeVersion is 2
  ```

- **Gaps are fine.** A missing step is a no-op on the dictionary — the
  structural fallback runs once, after the *whole* chain, against the final
  declared shape. Consequently a hook sees the **raw** dictionary left by the
  hooks before it, and a key introduced at an earlier un-hooked step may be
  absent: guard with `at:ifAbsent:` / `includesKey:` when reading such a key.
- **No class-variable access** — a compile error, not a style rule. A
  migration runs outside the class process (see below), where `self.x` would
  silently read `nil` and a write would be silently dropped; the validator
  refuses both before that contract is ever reached:

  ```beamtalk
  Actor subclass: Cart
    shapeVersion: 2
    classState: taxRate = 0.2
    class migrateFromV1: old => old at: #tax put: (old at: #total) * self.taxRate
  // ⛔ error: `migrateFromV1:` may not access class variables (`self.taxRate`);
  //    migration methods run outside the class process, where class
  //    variables are not available
  ```

- The return type, when annotated, must be `Dictionary`.
- A declaration error is likewise static:

  ```beamtalk
  Actor subclass: Cart
    shapeVersion: "two"
  // ⛔ error: 'shapeVersion:' expects a positive integer literal (got a non-positive value)

  Actor subclass: Cart
    shapeVersion: 2
    shapeVersion: 3
  // ⛔ error: duplicate shapeVersion: declaration (already 2)
  ```

**Ordinary sends are allowed inside a hook, so a migration is not pure** — it
just needs no process context (no live instance, no class process), which is
what makes it callable standalone at the REPL, before any instance depends
on it:

```beamtalk
Cart migrateFromV1: #{#items => #(3, 4)}
// => #{#items => #(3, 4), #total => 7}

Cart migrateShape: #{#items => #(3, 4)} from: 1     // whole chain + reconcile
// => #{#items => #(3, 4), #total => 7}
```

`migrateShape:from:` (sealed, `Behaviour >> migrateShape:from:`) runs the
**full** chain from a given version to the class's current `shapeVersion`,
then reconciles against the declared fields — the identical operation a
reload performs, exposed for standalone testing. It is idempotent at the
current version (`migrateShape:from:` with `aVersion = shapeVersion` runs
reconcile only, unchanged):

```beamtalk
Cart migrateShape: #{#items => #(3, 4), #total => 7} from: 2
// => #{#items => #(3, 4), #total => 7}     // already current — unchanged
```

#### Chain and reconcile semantics

For a reload (or any `migrateShape:from:` call) from version `V` to the
class's current `T`:

1. For each `K` in `V, V+1, …, T-1`: if a `migrateFromVK:` exists, apply it
   to the running dictionary; otherwise that step is a no-op.
2. **Reconcile** against the declared field list (the flattened one,
   inherited fields included). A declared field present in the dictionary is
   kept, whether or not it is `late` (see [`late` Slots](#late-slots-adr-0124));
   absent gets its declared default; absent with no default is `nil` on an
   untyped class and a **failure** on a `typed` one (a migration may not
   leave a typed slot unset, same rule ADR 0078 enforces after
   `initialize`) — **except** a declared-but-absent `late` field (ADR 0124
   §8), which is never defaulted or failed: it simply **stays absent** from
   the reconciled dictionary, on both a `typed` and an untyped class (`late`
   slots declare no default in the first place, so this check runs before
   the has-default/typed-no-default rules above, not as a fallback from
   them). An undeclared key is dropped with a warning.
3. Downgrading (`T < V`) runs reconcile only — `migrateToVN:` downgrade
   hooks are reserved, not defined.

Changing a slot between eager and `late` (or back) is a **shape change** —
it changes what an absent key means to the reconcile step above — and bumps
`shapeVersion:` the same way adding, removing, or retyping a slot does; the
reload-time tooling findings ([ADR 0123](ADR/0123-versioned-state-migration.md)
§4) flag a flip that lands without a version bump the same way they flag a
dropped or retyped field.

Every live actor's state map carries one internal key,
`'__shape_version__'`, written by `init/1` (absent means `1`) and updated to
the class's current version on a successful migration — never visible
through `fieldNames`, `printString`, the Inspector, or any other
reflection surface, exactly like every other internal key.

#### Suspend on failure, not resume-on-old-state or kill

If a `migrateFromVN:` hook raises, the actor is **left suspended** — not
resumed with its old-shaped state under the new code (which is what the
structural-only fallback used to do), and not killed:

```beamtalk
Cart reload
// => Cart
⛔ reload: 1 instance of Cart left suspended — migrateFromV1: raised
   does_not_understand: List>>summ (Cart class >> migrateFromV1:, cart.bt:9)
   state intact at v1; fix the hook and `Cart reload` again, or stop the instances via `Node current actors`
```

The suspended instance's state is intact and inspectable — but not through
an ordinary message send: `anActor inspect` (see
[Navigable Inspector](#navigable-inspector-adr-0095)) is itself a send, so
it lands in the actor's mailbox like any other and cannot be answered while
suspended. `sys:get_state`/`sys:get_status` are OTP **debug** messages, not
`gen_server` calls — they bypass the mailbox entirely, which is what makes a
suspended actor's state genuinely readable (via the Erlang FFI gateway, or
external tools — `observer`, `recon`). Fixing the hook and reloading again
**re-suspends idempotently** and, on success, resumes the actor at the new
shape:

```beamtalk
Cart reload            // migrateFromV1: still raises
// => Cart
Erlang maps get: #items from: (Erlang sys get_state: cart pid)
// => #(3, 4)     // state survives, still v1-shaped — an ordinary send to
                   // `cart` would time out instead; `sys:get_state` bypasses
                   // the mailbox

// ... fix the hook, save ...

Cart reload             // migrateFromV1: now succeeds
// => Cart
cart getTotal            // now answers on the new shape
// => 7
```

A synchronous caller blocked on the suspended actor simply times out with
the usual structured timeout error until the author fixes the hook (or kills
the instance) — the actor never silently continues on a wrong-shaped map,
and nothing is lost in the meantime. See
[ADR 0123](ADR/0123-versioned-state-migration.md) for the full design,
including the runtime-only versioned envelope (`pack/1`/`unpack/1`) that the
persistence and distribution ADRs will consume — not yet exposed as a
Beamtalk-callable method — and the reload-time tooling findings (§4).

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

Beamtalk exposes system reflection and workspace operations as typed message
sends to **class-side facades** (ADR 0040, ADR 0129). `Beamtalk`, `Workspace`,
`Transcript` and `SystemNavigation` are ordinary sealed, stateless classes:
every operation is a `class sealed` method sent to the class itself, there is no
instance to construct, and `Beamtalk new` (etc.) is a compile error. They are
not injected names, so they resolve exactly like `Integer` does and mean the same
thing in the REPL, `beamtalk run`, `beamtalk test` and releases. There is no
`current`, `default` or singleton accessor to call first.

| Facade | Purpose | Needs a workspace? |
|--------|---------|--------------------|
| [`Beamtalk`](#beamtalk--system-reflection) | Class registry, help, release reflection | No |
| [`Workspace`](#workspace--project-operations) | Loading, testing, bindings, flush/change log | Yes (except `isAvailable`) |
| [`Transcript`](#transcript--the-repls-shared-log) | REPL shared log; day-0 output convenience | Only `recent` / `clear` |
| [`SystemNavigation`](#systemnavigation--cross-class-code-queries) | Cross-class code queries | No |

Facts about the BEAM node or the running program are on `Node` and `Program`, and
logging control is on `Logger`; none of these are on `Beamtalk`.

### `Beamtalk` — System reflection

Provides access to the class registry, documentation and release provenance.
Analogous to Smalltalk's `Smalltalk` global. Works everywhere, with no workspace.

| Method | Returns | Description |
|--------|---------|-------------|
| `version` | `String` | Beamtalk version string |
| `allClasses` | `List(Class)` | All registered classes (class objects) |
| `classNamed: #Name` | `Class` or `nil` | Look up a class by name |
| `help: aClass` | `String` | Class documentation: name, superclass, method signatures. Accepts a class, a Symbol or a String |
| `help: aClass selector: #sel` | `String` | Documentation for a specific method |
| `erlangHelp: "module"` | `String` | Type signatures and EEP-48 docs for an Erlang module |
| `erlangHelp: "module" selector: #fun` | `String` | Documentation for one function of an Erlang module |
| `releaseInfo` | `Dictionary` | This node's release provenance; `#{#release => nil}` on a non-release node (never an error) |
| `shapeManifest` | `Dictionary` | `className -> #{#version, #fields, #migrations}` for every registered project class (ADR 0125 §3.4) |

```beamtalk
Beamtalk version
// => "0.4.0"

Beamtalk allClasses includes: Integer
// => true

Beamtalk classNamed: #Counter
// => Counter (or nil if not loaded)

Beamtalk help: Integer
// => "== Integer < Number ==\n..."

Beamtalk help: Integer selector: #+
// => "Integer >> +\n..."
```

`Beamtalk` has no namespace-snapshot accessor. Use `Beamtalk classNamed:` / `Beamtalk allClasses`
for the class registry, or `SystemNavigation` for queries over it.

**Logging and debug control are on `Logger`, not `Beamtalk`** (ADR 0129 §3,
BT-3653). The selectors are unchanged and, like `Logger info:`, need no
workspace:

| Method | Returns | Description |
|--------|---------|-------------|
| `Logger logLevel` / `Logger logLevel: level` | `LogLevel \| #all \| #none` / `Nil` | Read / set the OTP primary log level |
| `Logger logFormat` / `Logger logFormat: format` | `LogFormat` / `Nil` | Read / set the log output format |
| `Logger debugTargets` | `List(Symbol)` | Debug targets available to enable |
| `Logger enableDebug: target` / `Logger disableDebug: target` | `Nil` | Turn debug logging on or off for a target |
| `Logger activeDebugTargets` | `List(Symbol)` | Targets with debug logging currently on |
| `Logger disableAllDebug` | `Nil` | Turn all debug logging off |

### `Workspace` — Project operations

Provides file loading, testing, bindings and the flush/change-log loop. Scoped to
the running workspace: every selector raises a structured `no_workspace` error where
no workspace runs (e.g. `beamtalk test`); `Workspace isAvailable` asks without
raising. It works under `beamtalk run`, `workspace` and `release` modes.

Facts about the BEAM node or the running program are **not** on `Workspace`
(ADR 0129 amendment): node introspection lives on [`Node`](#node-introspection-adr-0129-amendment)
and the root supervisor on `Program rootSupervisor`.

| Method | Returns | Description |
|--------|---------|-------------|
| `isAvailable` | `Boolean` | True iff a workspace is running on this node. Never raises |
| `load: "path"` | `List(Behaviour)` or Error | Compile and load a `.bt` file or directory |
| `newClass: source at: path` | `List(Behaviour)` | Create a brand-new class from source at `path`; logs a `kind: #'new-class'` ChangeEntry (ADR 0082) |
| `moveClass: AClass to: "path"` | `Behaviour` | Move a class's declaration to another file (ADR 0114) |
| `classes` | `List(Behaviour)` | All loaded user classes (those with a recorded source file) |
| `testClasses` | `List(Behaviour)` | Loaded classes that inherit from `TestCase` |
| `bindings` | `BindingsView` | Live, write-through view of the `bind:as:` entries (see [Sessions and binding layers](#sessions-and-binding-layers-adr-0081)) |
| `bind: value as: #Name` | `Nil` | Register a value under a name for REPL evals. Refuses any registered class name (`name_conflict`) |
| `unbind: #Name` | `Nil` | Remove a registered name; raises if it is not found |
| `currentSession` | `Session` or `nil` | The calling process's REPL session (same value as `Session current`); `nil` outside a REPL eval |
| `sessions` | `List(Session)` | All live REPL sessions as `Session` values |
| `sync` | `Dictionary` | Incrementally compile the project's changed files (`:sync`) |
| `recheckImage` | `Dictionary` | Whole-image type re-check, with `checked` / `stale` / `findings` (ADR 0105) |
| `test` | `TestResult` | Run all loaded test classes |
| `test: AClass` | `TestResult` | Run a specific test class |
| `changes` | `ChangeLog` | Pending in-memory changes (ADR 0082) — see [Saving live edits back to disk](#saving-live-edits-back-to-disk--compilesource-changelog-and-flush-adr-0082) |
| `flush` | `Dictionary` | Write every durable + flushable ChangeEntry back to its source file (ADR 0082) |
| `flush: filter` | `Dictionary` | Flush a subset (Class / Symbol kind / `#{#file => path}`) |
| `flush: filter confirmDestructive: bool` | `Dictionary` | Flush a subset, confirming destructive entries (ADR 0113) |
| `flushIncludingDestructive` | `Dictionary` | Flush including destructive entries (ADR 0113) |
| `autoflush` | `Boolean` | Workspace setting (default `false`); persists across restarts |
| `autoflush: enabled` | `Boolean` | Toggle write-through: every durable patch immediately flushes (best-effort) |
| `startSupervisor: AClass` / `stopSupervisor: AClass` | `Supervisor` / `Nil` | Attach / stop a supervisor under the workspace supervisor |
| `dependencies` | `Dictionary(String, Package)` | Direct dependency packages of the workspace |

```beamtalk
Workspace isAvailable
// => true  (false under `beamtalk test`, where every other selector raises no_workspace)

Workspace load: "examples/counter.bt"
// => #(Counter)  (Counter is now registered)

Workspace classes includes: Counter
// => true

Workspace testClasses includes: CounterTest
// => true

(Workspace test: CounterTest) failed
// => 0  (all tests pass)

Node current actors unwrap size
// => 3  (number of live actors; see Node introspection below)
```

### `Transcript` — The REPL's shared log

`Transcript` is a sealed, stateless class-side facade (ADR 0129 §5). It has no
instance, no stream protocol and no capture API. Its behaviour depends on whether
an interactive workspace is running:

| Selector | In an interactive workspace | Elsewhere (`run`, `test`, releases) |
|----------|-----------------------------|-------------------------------------|
| `show: value` | Appends the value's text to the workspace transcript | One Logger `notice` event, domain `[beamtalk, user, transcript]` |
| `cr` | Appends a newline | No-op |
| `showCr: value` | `show:` then `cr` | One Logger event |
| `recent` | The buffered lines (`List(String)`) | Raises `no_workspace` |
| `clear` | Empties the buffer | Raises `no_workspace` |

```beamtalk
Transcript showCr: "Hello"
Transcript show: "a"; cr; show: "b"
Transcript recent
```

Outside a workspace the Logger route prints plain text, one line per event, so two
consecutive `show:` sends are two events, not one line, and ordering relative to
`Console` (which writes synchronously) is not guaranteed. Silencing or redirecting
the output is ordinary Logger configuration on the transcript domain.

**Guidance:** `Transcript` exists for newcomers and REPL use. Programs should use
`Logger` (structured logging, ADR 0064) for diagnostics and `Console` for plain
stdout/stderr. The `show:` / `showCr:` convenience methods on `Object` delegate to
`Transcript`. There is no `showLine:`; use `showCr:`.

### Node introspection (ADR 0129 amendment)

Reads of *node* state are per-node queries on `Node` (ADR 0126), not `Workspace`
selectors. Every one answers a `Result` on every receiver, including
`Node current`, so a selector has one return type; a peer is reached via `erpc`
and its failures come back as `Error` values (`node_down`, `timeout`,
`insecure_distribution`, `remote_code_mismatch`), never raised. They work in every
boot context, including `beamtalk test`.

| Method | Returns | Description |
|--------|---------|-------------|
| `aNode actors` | `Result(List(Actor), Error)` | Every live actor on the node (runtime-owned actor registry) |
| `aNode actorsOf: AClass` | `Result(List(Actor), Error)` | Live actors of the class or a subclass |
| `aNode actorAt: pidStr` | `Result(Actor \| Nil, Error)` | The live actor with that pid string, or `nil` |
| `aNode processes` | `Result(SupervisionTree, Error)` | The node's `default`-scope supervision tree (ADR 0092) |
| `aNode supervisors` | `Result(List(Supervisor), Error)` | Root application supervisor plus workspace-attached ones |
| `aNode shapeSkew` | `Result(Integer, Error)` | Classes whose shape version differs from this node's (ADR 0126 §8) |
| `Node connected` | `List(Node)` | Visible connected peers (replaces `Workspace nodes`) |

`aNode actors` lists **every** live actor, whereas `Actor allRegistered` /
`Actor allRegisteredOn: aNode` list only the **name-registered** ones (`Actor named:`).
The root supervisor and the declared supervisor class are program/manifest facts:

```beamtalk
Node current actors unwrap size          // every live actor on this node
Node current actorsOf: Counter           // => Result ok: #(...)
Node connected collect: [:n | n shapeSkew unwrap]
Program rootSupervisor                   // running application's root supervisor, or nil
(Package named: "my_app") supervisorClass  // declared [application] supervisor class, or nil
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

#### `removeFromSystem` — removing a whole class (BT-785, ADR 0113)

`removeFromSystem` tears a class down entirely: stops its live actors, stops
the class gen_server, purges the BEAM module, and purges every derived
registry (xref, extensions, protocol conformance, compiler cache, workspace
source). Unlike `removeSelector:` (above), it is a hard, unconditional
refusal for stdlib classes and classes with subclasses — there is no
in-memory-but-non-flushable escape hatch.

```beamtalk
Counter removeFromSystem   // => nil (Counter class removed; refuses if Counter has subclasses)
Integer removeFromSystem   // => Error: cannot remove stdlib class
```

A successful removal also logs a durable `kind: #'remove-class'` ChangeEntry
(ADR 0113 Phase 1) — the same audit-trail-is-unconditional rule every other
in-memory mutation follows. Flushing that entry deletes the class's `.bt`
file, which is why it needs its own confirmation gesture — see
[Destructive flush — `flushIncludingDestructive` and `confirmDestructive`](#destructive-flush--flushincludingdestructive-and-confirmdestructive-adr-0113)
below.

For removing a single method while keeping the class, see
[`removeSelector:` / `removeSelector:ifAbsent:`](#removing-methods--removeselector-and-removeselectorifabsent-adr-0112)
above.

A pending (unflushed) `#'remove-class'` entry is revertable, same as any other
ChangeLog entry: `Workspace changes revert: anEntry` recompiles and reinstalls
the whole class from the entry's recorded prior source, reusing the same
install path `newClass:at:` uses (ADR 0113 Phase 3). Before reinstalling, the
runtime compares the current on-disk file against the entry's recorded
`prev_source_ref` snapshot byte-for-byte; if the file was edited externally
(another session, git, an editor) while the removal sat pending, `revert:`
raises a structured error instead of silently discarding the external edit —
the class stays removed and the original entry stays pending (BT-3213). Once
flushed — the `.bt` file actually deleted — the entry is pruned from the
active view and `revert:` has nothing left to act on; recovering a flushed
removal is git's job. `removeSelector:`'s `#'remove-method'` entries are
revertable the same way (they re-install the removed method's recorded prior
body).

#### Renaming a class or method — `renameTo:`, `renameSelector:to:`, and `moveClass:to:` (ADR 0114)

`renameTo:` (class rename) and `renameSelector:to:` / `renameSelector:to:ifAbsent:`
(method rename) give a class or method a new identity in place — unlike
`removeFromSystem` + `newClass:at:`, which would lose instance identity,
in-place undo, and any automatic reference fixing. Both compute their
rewrite sites from the same xref infrastructure `SystemNavigation` exposes
above (`referencesTo:`, `sendersOf:`, `implementorsOf:`) rather than a new
query, follow `removeSelector:`'s established shape (sealed class-side
methods, return the receiver for chaining, raise a structured
`#beamtalk_error{}` — reusing `removeSelector:`'s `selector_not_found` kind —
on an absent source, paired with an `ifAbsent:` escape hatch), and — like
`removeFromSystem` — never touch disk on their own; the resulting entry
needs its own confirmed destructive flush (below) to reach disk.

```beamtalk
sealed renameTo: aNewName :: Symbol -> Behaviour
sealed renameSelector: aSelector :: Symbol to: aNewSelector :: Symbol -> Behaviour
sealed renameSelector: aSelector :: Symbol to: aNewSelector :: Symbol
    ifAbsent: absentBlock :: Block(T) -> Behaviour | T
```

**Receiver and side**, for `renameSelector:to:`, mirrors `removeSelector:`:
`Counter renameSelector: #a to: #b` touches the instance-side table,
`Counter class renameSelector: #a to: #b` touches the class-side table.

```beamtalk
Counter renameTo: #Accumulator
Counter renameSelector: #increment to: #incrementBy
Counter class renameSelector: #ofSize: to: #withCapacity:
```

A collision with an existing name is refused, loudly, the same way
`removeFromSystem` refuses a name it can't act on:

```beamtalk
Counter renameTo: #Accumulator
// => Error: cannot rename Counter to Accumulator — Accumulator already exists

Counter renameSelector: #increment to: #decrement
// => Error: Counter already defines #decrement locally — refusing to overwrite

Counter renameSelector: #bogus to: #anything
// => Error: selector_not_found

Counter renameSelector: #bogus to: #anything ifAbsent: ["not found"]
// => "not found"
```

**`renameTo:` rewrites every in-project reference it can find, immediately, in
memory** — constructor/message sends, type annotations (including generic
parameters like `List(Counter)`), superclass declarations, and extension
declarations (via the union of `referencesTo:` and the class's direct
subclasses). A class live-patched via `>>` since its last full compile is a
real, accepted residual-risk gap here (`referencesTo:`'s reference-indexing
channel is unconditionally empty for live patches), and a plain
string/comment occurrence of a class's name is never rewritten — the same
category of accepted risk `removeSelector:`'s dangling senders already carry,
just concretely named.

**`renameSelector:to:` auto-rewrites only structurally-safe `self`/`super`
sends — not every textual sender `sendersOf:` finds.** `sendersOf: #sel`
returns every send of that selector name anywhere in the project, regardless
of which class's implementation the sender actually meant to call —
auto-rewriting all of them would risk silently corrupting unrelated, working
code that happens to share a selector name. `renameSelector:to:` therefore
splits the results into two tiers, reported on the resulting ChangeEntry:

- **Confirmed (`sites`, rewritten):** the definition itself, plus every
  `self`/`super` send whose owner is the renamed class or a transitive
  subclass — **only when no class in that subclass tree also implements the
  same selector on the same side** (an override anywhere in the hierarchy
  means a `self`/`super` send could be intercepted before reaching this
  class's implementation, so none of that selector's `self`/`super` sites are
  safe to auto-rewrite, not even ones on the class itself).
- **Candidate (`candidate_sites`, reported for review, never auto-rewritten):**
  every send through an arbitrary-expression receiver, every FFI call site,
  every `self`/`super` send whose owner falls outside the renamed class's
  hierarchy, and — when the selector is overridden anywhere in the subclass
  tree — every `self`/`super` send for that selector regardless of owner.

`candidate_sites` is exactly `removeSelector:`'s dangling-sender risk made
visible rather than silent: nothing there is rewritten or written by flush,
but a caller inspecting the returned `ChangeEntry` (`Workspace changes` —
below) sees both counts and can patch the review list manually via
`compile:source:`, the same way any other live patch is made.

**`Workspace moveClass: aClass to: aNewPath`** relocates a class's `.bt` file
without changing its name — the pure file-move counterpart to `renameTo:`,
useful when only where a class lives on disk should change:

```beamtalk
Workspace moveClass: Counter to: "src/math/counter.bt"
```

**Refusal is decided per primitive**, by whether the in-memory effect alone
is safe against a class the project doesn't own the *callers* of:

| Primitive | Stdlib | Dynamic (`ClassBuilder`) | Dependency |
|---|---|---|---|
| `renameTo:` | Refuses | Allowed, not flushable | Refuses |
| `renameSelector:to:` | Allowed, not flushable | Allowed, not flushable | Allowed, not flushable |

`renameTo:` refuses a stdlib or dependency class outright — the xref index
only covers in-project source, so its site-discovery can never be complete
for a class whose callers might live outside the project.
`renameSelector:to:` reuses `removeSelector:`'s narrower granularity
argument: a single-selector rename's blast radius (one stray un-rewritten
`self`/`super` sender) is smaller than a whole class silently losing
referential integrity project-wide, so it installs in memory unconditionally
and only gates the *disk* write.

A successful rename logs a durable `kind: #'rename-class'` /
`#'rename-method'` ChangeEntry — the same audit-trail-is-unconditional rule
`removeFromSystem` follows — carrying the multi-file `sites` list flush will
apply and (for `rename-method`) the `candidate_sites` review list above. Both
kinds join `removeFromSystem` in Tier 2 (below): flushing rewrites every
confirmed site across however many files it spans (a class rename also moves
the `.bt` file itself), which is why it needs its own confirmation gesture.
Unlike `removeFromSystem`, a pending rename entry is revertable —
`Workspace changes revert: anEntry` splices every one of the entry's own
recorded `sites` back to its own prior body, at its own recorded location,
rather than re-running `renameTo:`/`renameSelector:to:` (which would
re-compute site discovery against post-rename state and could disagree with
what the original rename actually touched).

#### Destructive flush — `flushIncludingDestructive` and `confirmDestructive` (ADR 0113)

Every flushable ChangeEntry classifies into one of two tiers:

| Tier | Entries | File survives? | Gate |
|------|---------|-----------------|------|
| 1 | patches, `newClass:at:`, `removeSelector:` (`#'remove-method'`) | Yes — excising a recorded span leaves the file in place, mechanically identical to a patch | None — `Workspace flush` applies it directly |
| 2 | `removeFromSystem` (`#'remove-class'`), `renameTo:` / `Workspace moveClass:to:` (`#'rename-class'`), `renameSelector:to:` (`#'rename-method'`) | No — a class removal deletes the `.bt` file; a rename rewrites one or more files (a class rename also moves the file itself) | Explicit `confirmDestructive` |

`Workspace flush` (no argument) applies only Tier 1. A pending Tier 2 entry is
left pending and reported in the summary's `#skipped` field
(`#{#seq, #class, #reason => #destructive}`), distinct from a conflict:

```beamtalk
Counter removeFromSystem                          // memory step: unconditional, as above
Workspace flush
=> _   // FlushResult with #skipped: [#{#reason => #destructive, ...}]
       // Counter.bt is untouched; the removal stays pending.
```

Reaching Tier 2 always requires an explicit gesture — never a workspace
setting or environment variable:

```beamtalk
Workspace flushIncludingDestructive               // unscoped: applies every pending
                                                    //   Tier 1 + Tier 2 entry
Workspace flush: Counter confirmDestructive: true  // scoped to one class
Workspace changes flushKinds: #( #'remove-class' ) confirmDestructive: true
                                                    // scoped to one kind
```

`flushIncludingDestructive` is a bare unary selector rather than a keyword
message — once the call is unscoped there is no class/kind/file argument left
to attach a `confirmDestructive:` keyword to. The scoped two-keyword forms
(`flush:confirmDestructive:`, `flushKinds:confirmDestructive:`) compose
`confirmDestructive` as one more independent filter dimension on top of the
existing `flush:` / `flushKinds:` mechanisms, the same way `flushKinds:`
already composes entry-kind and author-kind filters.

`autoflush: true` never implies `confirmDestructive: true` — Tier 1 still
autoflushes immediately; a class removal always needs its own, separate
confirmation regardless of the `autoflush` setting. Class-removal deletion is
staged (same-filesystem rename, then unlink) so a crash between the two steps
leaves a recoverable file, never a partial or silent loss; a re-flush finishes
the delete. See [ADR 0113](ADR/0113-destructive-workspace-operations.md) for
the full design, including the external-edit conflict table and delete
atomicity — and [ADR 0114](ADR/0114-class-and-method-rename.md) for how
`renameTo:`/`renameSelector:to:` extend this same tier and gate to genuinely
multi-file staging.

**Editor integration (LSP):** flush uses typed `workspace/applyEdit` resource
operations for structural file changes: a `Workspace newClass:at:` flush emits
a `CreateFile` operation (BT-3212), and a `remove-class` Tier 2 destructive
flush emits a `DeleteFile` operation (BT-3209), so the editor can distinguish
file creation and deletion from ordinary content edits. Both fire regardless of
whether the file was open in the editor. A `rename-method` flush emits a
`TextDocumentEdit` per confirmed site (BT-3275); a `rename-class` flush emits
`TextDocumentEdit` for cross-file reference rewrites and a custom
`beamtalk-lsp/documentMoved` notification (`{oldUri, newUri}`) for the moved
declaration file — replacing the `RenameFile` op that silently no-ops in VS
Code when the old path is already gone (BT-3285). Ordinary method patches
continue to use the generic `TextEdit` shape, gated on the file being open.

### `Program` — This running program (ADR 0099 §2)

`Program` names the running program in every execution context. Besides
`commandName` and `exit` / `exit:`, `Program package` returns the program's
root `Package` (`Package named: <root package>`), so a program can reach its own
manifest facts without hard-coding its name:

```beamtalk
Program package version
```

Every launcher records the root package as the `beamtalk_runtime` app-env key
`root_package` from the Rust-parsed `beamtalk.toml` (`beamtalk repl`,
`beamtalk run` script and service modes, `beamtalk workspace create`, the
escript bootstrap; `beamtalk release` writes it into `sys.config`) and loads the
package's `.app`. It never returns `nil`; it raises a structured error instead:

| Kind | When |
|------|------|
| `no_program_package` | No project (bare `beamtalk repl`) |
| `ambiguous_program_package` | `beamtalk test` runs several packages at once (a single package under test is the root) |
| `package_not_loaded` | The root package's `.app` is not on the code path (run `beamtalk build`) |

`Package classes` reads the `classes` list of the package's `.app`, which
`beamtalk build` writes. A workspace `sync` (`load-project`, `Workspace sync`,
`:sync`) refreshes that in-memory list from the live class registry, so a class
added to the project's `src/` appears in `Package classes` after the sync
without a rebuild. A class defined only by evaluating source in the REPL is
listed once it is synced from a file; the `.app` file on disk is only rewritten
by `beamtalk build`.

#### `Program exit:` / `System halt:` (ADR 0099 §3, BT-3634)

`Program exit: N` triggers a graceful shutdown (`init:stop(N)`) when the program
owns the node. `System halt: N` is an immediate stop that flushes Logger handlers
first.

| Context | `Program exit: N` | `System halt: N` |
|---------|-------------------|-------------------|
| `beamtalk run` (script/service) | Stops with exit status `N` | Immediate halt with status `N` |
| Release `foreground` / `eval` | Stops with exit status `N` | Immediate halt with status `N` |
| Escript | Stops with exit status `N` | Immediate halt with status `N` |
| Actor in a node-owning context | Stops the node gracefully | Immediate halt |
| Workspace (actor send) | Raises `program_exit_outside_entry` | Raises `unsupported` |
| `beamtalk test` | Raises `#program_exit` with `status` | Raises `unsupported` |
| Release console REPL | Ends this session; the release node keeps running | Allowed (the release owns the node) |

### `SystemNavigation` — Cross-class code queries

`SystemNavigation` provides Smalltalk-style live-image queries over the loaded
class registry. It is a stateless class-side facade (ADR 0129): every query is a
`class sealed` method sent to the class itself — there is no instance to
construct, and `SystemNavigation new` is a compile error. The class is a plain
value, so `nav := SystemNavigation` and `nav actorClasses` work too.

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
nav := SystemNavigation

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

A free identifier in a REPL expression (`x`, `answer`, `Counter`) resolves through
**three tiers**, in this order:

| Tier | Owner | Source | Accessor |
|------|-------|--------|----------|
| 1. **Session locals** | the session (per connection) | `x := 42` typed in the shell | `Session current bindings` |
| 2. **Workspace bindings** | the workspace (shared) | `Workspace bind: v as: #name` entries | `Workspace bindings` |
| 3. **Class registry** | the runtime | every loaded class (`Counter`, `Integer`, `Transcript`, `Beamtalk`, `Workspace`) | `Beamtalk allClasses` |

An earlier tier **shadows** a later one: a local named `answer` hides a workspace
binding of the same name, and `Integer := 3` creates a session local that shadows
the `Integer` class for that session. A name found in no tier raises
`undefined_variable`.

Only tiers 1 and 2 are REPL-specific. Method bodies and batch-compiled code (a
`.bt` file under `beamtalk run`, `beamtalk test` or a release) use **only the class
registry**, so `Transcript`, `Beamtalk`, `Workspace` and `SystemNavigation` resolve
there exactly as `Integer` does. They are classes, not entries in a binding layer,
so `Workspace bindings` does not list them.

#### `Session` — a first-class session value

`Session` is a factory, mirroring `Date today` / `Smalltalk current`: two
class-side methods return a session *value* you then message. There is **no**
class-side operation mirror (no `Session bindings`). Workspace-wide bindings are
workspace state, reached via `Workspace bindings`.

| Class method | Returns | Description |
|--------------|---------|-------------|
| `Session current` | `Session` or `nil` | The calling process's session; `nil` outside a REPL eval (compiled code has no session) |
| `Session withId: anId` | `Session` or `nil` | Look up a session by its protocol id; `nil` if unknown or no longer alive |

| Instance method | Returns | Description |
|-----------------|---------|-------------|
| `bindings` | `BindingsView` | Live view of this session's locals (the `x := 42` layer) |
| `resolve: #name` | `Object` | Resolve a name the way bare-name lookup does (locals → workspace bindings → classes). Shares the one resolver with bare-name lookup, so it raises `undefined_variable` for a name that resolves nowhere — exactly as typing the bare name would |
| `clear` | `nil` | Clear this session's locals (workspace bindings remain) |
| `id` | `String` | Stable session identifier (matches the protocol session id) |

```beamtalk
x := 42
// => 42

Session current bindings keys
// => #(#x)

Session current bindings at: #x
// => 42

Session current resolve: #Transcript
// => Transcript (the class, from the class registry)

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

Both `Session current bindings` and `Workspace bindings` return a `BindingsView`:
a small Dictionary-protocol value (`at:`, `at:put:`, `removeKey:`,
`includesKey:`, `keys`, `values`, `size`, `do:`) backed by live state. `at:put:`
returns the value put; `removeKey:` returns `nil`.

```beamtalk
// Session-local write — DEFERRED to end of eval, visible on the NEXT line:
Session current bindings at: #y put: 99
// => 99
y
// => 99

// Workspace-binding write — SYNCHRONOUS (routes through bind:as:),
// visible immediately on the next line:
Workspace bindings at: #answer put: 42
// => 42
answer
// => 42
```

**One documented asymmetry under the shared type:** session-local writes are
deferred to the end of the current eval (the eval worker holds a state
snapshot), so a same-expression read-back sees the *old* value; workspace-binding
writes hit shared ETS immediately. `bind:as:` and `Workspace bindings at:put:` refuse
to shadow any registered class name (`name_conflict`):

```beamtalk
Workspace bindings at: #Workspace put: nil
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

The `Tracing` class provides actor observability and performance telemetry. It is a sealed, class-only facade (like `System` and `Logger`) — all methods are class-side, there are no instances. See [ADR 0069](ADR/0069-actor-observability-and-tracing.md) for the full design. To emit your own application events, use `Telemetry` ([Application Telemetry Events](#application-telemetry-events-adr-0069-amendment)).

Two levels of instrumentation are available:

- **Always-on aggregates** — per-class, per-method call counts and durations with negligible overhead (~120ns/call). No setup required. They are recorded directly on the send path and do not depend on the opt-in actor-dispatch telemetry events ([below](#application-telemetry-events-adr-0069-amendment)).
- **Detailed trace events** — individual call/return/error records captured to a ring buffer. Requires explicit `Tracing enable`. Enabling also collects per-actor detail (see below).

### Tracing Lifecycle

```beamtalk
// Enable detailed trace capture
Tracing enable
// => nil

// Check if tracing is active
Tracing isEnabled
// => true

// Disable trace capture (class aggregates continue;
// per-actor detail is dropped)
Tracing disable
// => nil

// Clear all trace events and aggregate stats
Tracing clear
// => nil
```

### Aggregate Stats (Always-On)

Aggregate stats are collected for every actor dispatch, even when trace capture is disabled. They include call counts, total duration, min/max/average times, and error counts. Stats are keyed by class: one `"class:<Class>"` entry per class with per-method figures. While tracing is enabled, `stats` additionally holds one entry per pid with live actors' per-actor detail.

```beamtalk
// All stats: class entries always, plus per-actor entries while enabled
Tracing stats
// => #{...}  (Dictionary keyed by "class:<Class>", and by pid while enabled)

// Stats for a specific actor (only collected while tracing is enabled;
// answers #{} when tracing is off)
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

// Top N actors by message queue length (live snapshot over every
// registered actor, whether or not tracing is enabled)
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
(Tracing stats at: "class:Counter") at: "methods"
// => #{"increment" => #{calls => 10, ...}, ...}

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
| **What is happening** — log messages, debug output | `Logger info:`, `Logger enableDebug:` | [ADR 0064](ADR/0064-runtime-logging-control-and-observability-api.md) |
| **How fast is it happening** — timing, call counts, bottlenecks | `Tracing stats`, `Tracing slowMethods:` | [ADR 0069](ADR/0069-actor-observability-and-tracing.md) |

Runtime logging configuration lives on class-side `Logger` (BT-3653, [ADR 0129](ADR/0129-class-side-system-facades.md)): `Logger logLevel:`, `Logger logFormat:`, `Logger enableDebug:` / `disableDebug:`, `Logger debugTargets`, `Logger activeDebugTargets`, `Logger disableAllDebug` and `Logger loggerInfo`. These moved from `Beamtalk` with no shim.

---

## Application Telemetry Events (ADR 0069 amendment)

`Telemetry` lets Beamtalk code emit, and attach to, ordinary events on the BEAM [`telemetry`](https://hexdocs.pm/telemetry) bus, the library Phoenix, Ecto and Thousand Island use. Erlang and Elixir handlers, LiveDashboard, StatsD exporters and `opentelemetry_telemetry` see a Beamtalk event exactly as they would one emitted from Erlang. Like `Tracing`, it is a sealed class-side facade with no instances. `Tracing` observes actor dispatch; `Telemetry` is the event bus for your own application events. See the [ADR 0069 amendment](ADR/0069-actor-observability-and-tracing.md#amendment-2026-10-21-application-events-bt-4025).

```beamtalk
// Emit one event
Telemetry execute: #(#myapp, #cache, #miss)
  measurements: #{#count => 1}
  metadata: #{#key => "user:1"}
// => nil

// Time a block: emits [myapp, query, start] and [myapp, query, stop]
Telemetry span: #(#myapp, #query) metadata: #{#table => #users} do: [3 + 4]
// => 7
```

- **Event names** are a non-empty List of Symbols (`#(#myapp, #cache, #miss)`), passed to `telemetry` as the atom list `[myapp, cache, miss]`. There is no dotted shorthand. Any other value raises a `TypeError` (`#type_error`).
- **Measurements** are a Dictionary with Symbol keys; **metadata** is any Dictionary. Both are passed through unchanged.
- **`span:metadata:do:`** answers the block's value. The `start` event carries `#monotonic_time` and `#system_time`; `stop` carries `#monotonic_time` and `#duration` (native time units) and the same metadata. If the block raises, the span emits `exception`, whose metadata adds `#kind`, `#reason` and `#stacktrace`, and re-raises the original error unchanged (in a cast or `handleInfo:` the actor still crashes, ADR 0134). A `^` out of the block is a normal return: the span emits `stop`. All three events share one `#telemetry_span_context` reference. The block is passed to another method, so it cannot assign the method's locals ([ADR 0133](ADR/0133-scope-of-captured-variable-mutation-in-blocks.md)); use its value instead.

### Attaching handlers

```beamtalk
seen := Ets new: #seen type: #set
(Telemetry attach: #auditMisses to: #(#myapp, #cache, #miss) do: [:event :measurements :metadata |
  seen at: #last put: metadata]) unwrap
Telemetry execute: #(#myapp, #cache, #miss) measurements: #{#count => 1} metadata: #{#key => "k"}
(seen at: #last) at: #key
// => "k"
Telemetry detach: #auditMisses
// => nil
```

- `attach:to:do:` answers `Result ok: nil`, or `Result error:` with kind `#handler_exists` when the id is already attached. `attach:toAll:do:` takes a List of event names. Handler ids are Symbols or Strings. `detach:` is idempotent.
- The handler runs **synchronously in the emitting process**, so keep it short (hand work to an actor if it is slow).
- If a handler raises, `telemetry` logs the error and **detaches** it; the emitter is not affected.
- A handler is not re-entered by an emit it causes itself (for example a handler on `#(#beamtalk, #actor, #dispatch, #stop)` that sends to an actor), so it cannot recurse without bound.
- The runtime's actor-dispatch events (`#(#beamtalk, #actor, #dispatch, #start)`, `#stop`, `#exception`) are **opt-in** ([ADR 0069 amendment, BT-4113](ADR/0069-actor-observability-and-tracing.md#amendment-2026-10-28-dispatch-events-are-opt-in-bt-4113)): every actor send would pay for them otherwise. Attaching a handler to one of them through `Telemetry` turns them on, and detaching the last such handler turns them off again; `Tracing enable` also turns them on. Erlang or Elixir code that attaches with `telemetry:attach/4` must set the app env `{beamtalk_runtime, dispatch_telemetry, true}` or call `beamtalk_telemetry:enable_dispatch_events/0`. The `Tracing` aggregates do not depend on them, and the actor lifecycle events are always emitted.
- Class variables follow [ADR 0130](ADR/0130-class-variables-live-in-the-class-process.md): a handler running away from its home class reads the values captured when the block was created, and a write raises `class_state_unreachable`; at home, a raising handler's writes are discarded.
- The handler block belongs to the code that created it: after redefining that class, attach the handler again.
- The runtime's own handler ids (the ones `Tracing` relies on) are reserved: `attach:` and `detach:` refuse them with `#reserved_name`.
- Exporters (StatsD, Prometheus, OpenTelemetry) are still attached in Erlang. Attaching from Beamtalk is mainly for tests and in-image consumers.

`Telemetry` is `native: beamtalk_telemetry`: each method is a `self delegate` pass-through to the runtime module ([`native:` for stateless Objects](beamtalk-native-erlang.md#native-stateless-objects--native-for-object)), so a wrapped error names `Telemetry` and the selector. An emit costs about 0.2–0.4 µs more than a raw `telemetry:execute/3` (`docs/development/benchmarks.md`).

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
sub := a when: PriceChanged do: [:e | Transcript showCr: "now " ++ e newPrice printString]
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

a when: UIEvent do: [:e | Transcript showCr: "ui event"]
a announce: (ButtonClicked buttonId: "submit")   // matches — "ui event"
```

### SystemAnnouncer — watch the runtime live

`SystemAnnouncer current` is the singleton bus the **runtime itself** publishes
onto. System facilities announce well-known discrete events; a tool subscribes
once and filters by event class instead of wiring bespoke notification channels:

```beamtalk
SystemAnnouncer current when: ActorSpawned do: [:e |
  Transcript showCr: e actorClass asString
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
| `NodeUp` | `node` | a visible node connects (ADR 0126 §8; hidden nodes never announce) |
| `NodeDown` | `node`, `reason` | a visible node's connection is lost (`reason` is OTP's `nodedown_reason`, or `#unknown`) |

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

## Distribution — Location-Transparent Actors (ADR 0126)

Beamtalk actors are **location-transparent**: the same `.` (sync), `!`
(cast), and `Future`-returning async sends that work between local actors
work identically against an actor running on another BEAM node in the
cluster. There is no special "remote actor" type — an `Actor` reference is
an `Actor` reference, whether the process behind it lives here or across the
network. Distribution builds directly on the supervision, announcement, and
value-object machinery described earlier in this document; see
[ADR 0126](ADR/0126-distribution-location-transparent-actors.md) for the full
design, including the failure-mode analysis and security model this section
summarizes.

### `Node` — a first-class value

`Node` wraps a BEAM node identity (`name@host`). It is a **value, not a
proxy**: two `Node`s are equal iff their names are equal, and constructing
one never touches the network.

```beamtalk
Node current                                    // => Node(nonode@nohost)
worker := (Node named: #'worker@localhost') unwrap
worker name                                     // => #'worker@localhost'
Node connected                                  // => #()  (visible connected nodes)
worker connect                                  // => Result ok: Node(worker@localhost)
worker isConnected                              // => true
worker disconnect                               // => true
```

`connect` (and `ping`, which auto-connects) apply a security policy (see
Security, below): a same-host connection always succeeds; an off-host
connection is refused with `kind = insecure_distribution` unless this node
runs TLS distribution. `Node named:` only validates the `name@host` shape and
never touches the network, so it always succeeds for a well-formed name —
reachability is only proven by `connect`/`ping`.

### Remote spawn and lookup

Every actor spawn/lookup class-side selector gains an `on:` variant that
targets a specific `Node`. Syntax, return types, and error kinds mirror the
local form exactly — remote spawn always answers a `Result`, because
reaching another node is an expected failure (ADR 0060), not a programming
error:

```beamtalk
worker := (Node named: #'worker@localhost') unwrap
c := (Counter spawnOn: worker) unwrap
c node                                          // => Node(worker@localhost)
c isRemote                                      // => true
c increment                                     // an ordinary sync send — works identically to a local actor
```

| Selector | Local form | Remote form |
|----------|-----------|-------------|
| Anonymous spawn | `spawn` | `spawnOn: node` |
| Named spawn (state args) | `spawnWith: args` | `spawnWith: args on: node` |
| Named + registered spawn | `spawnAs: name` | `spawnAs: name on: node` |
| Lookup by registered name | `named: name` | `named: name on: node` |
| List every registered actor | `allRegistered` | `allRegisteredOn: node` |

**Remote spawn is not idempotent.** A timeout after the far-side spawn
actually succeeded leaves an unowned actor running on the target node — the
spawning session has no way to tell "it never happened" from "it happened,
but the reply was lost". A named spawn (`spawnAs:on:`) retried after
`name_registered` can be recovered with `named:on:`; an anonymous `spawnOn:`
has no such recovery — retry only when idempotency doesn't matter, or use a
named spawn.

**Remote spawn creates no link and no local tracking.** The spawned actor is
never linked back to the spawning process (a link across a node boundary
would turn every partition into a local crash), and the spawning workspace
does not track it — `ActorSpawned` fires on the *target* node, not the
caller's. The actor outlives the spawning session until something else stops
it.

### Cluster-unique names — `scope: #global`

A node-local registered name (`spawnAs:`, ADR 0079) is only visible on the
node that registered it. Requesting `scope: #global` instead registers the
name **cluster-wide**, backed by OTP's `global` module:

```beamtalk
leader := (Scheduler spawnAs: #scheduler scope: #global) unwrap
// from any node in the cluster:
s := (Scheduler named: #scheduler scope: #global) unwrap
s tick
```

`scope: #global` is opt-in — the default stays node-local — and comes with
real operational cost, not just a lookup convenience:

> **Mesh side effects.** `global` keeps a fully connected mesh between every
> node that uses it, and OTP 25+ enables `prevent_overlapping_partitions` by
> default: on partial connectivity, `global` actively **disconnects** nodes to
> restore a consistent view. A program using `scope: #global` will observe
> `NodeDown` events **caused by `global` itself**, not by an actual network
> failure — don't assume every `NodeDown` means a real partition. `global`
> also inherits the full-mesh registry's known scaling limits (every node
> locks the global name table during a registration) — reach for it
> deliberately, not as the default choice for cluster-wide lookup.

When a netsplit heals and the same global name was registered on both sides,
Beamtalk's own resolver keeps the *older* registrant (by start time) and
stops the other with a `#globalNameConflict` reason — never OTP's default
`exit(Pid, kill)`, so the losing actor's `terminate` still runs and its
`ActorStopped` announcement carries the reason.

### The wire — what crosses a node boundary, and what doesn't

A send to a remote actor is encoded before it leaves the sender and decoded
on arrival — transparently, with no change to the sending code. Every
argument and result is walked and classified:

| What you send | Crosses the wire as |
|----------------|---------------------|
| Numbers, Strings, Symbols, `Value` subclasses, `Node` | Copied, `Value` instances enveloped (version-tagged, ADR 0123/0125) |
| Actors, `Pid` | Allowed — pids are node-qualified natively; a registered-name reference is rewritten to carry its origin node |
| A class object (e.g. `Counter`) | Rewritten to a by-name reference, resolved against the **receiving** node's own class registry — a class object never crosses as a remote process reference |
| Blocks (closures) | Allowed, like Erlang funs — see the caveat below |
| `Port`, `FileHandle` (`HandleScoped(#process)`) | **Rejected** at encode time: `kind = not_serialisable` |
| `Ets`, `AtomicCounter`, `Timer`, `Subscription` (`HandleScoped(#node)`) | **Rejected** at encode time: `kind = not_serialisable` — a handle that names a resource on *this* node is meaningless on another |
| An unassigned `late` slot | Travels as absent; the receiver's first read raises `UninitializedStateError` |

**A block whose defining class isn't loaded (at the same version) on the node
where it runs raises `kind = remote_code_mismatch`** there — for a block
invoked during the call that received it, that's the callee, relayed back to
the sender like any other callee error; for a block stored and invoked
later, it surfaces wherever it's eventually invoked.

**Version skew is handled per message, not per connection.** An older
envelope is migrated forward through the receiving class's `migrateFromVN:`
chain; a **newer** envelope than the receiver knows is refused *before* any
migration runs, with `kind = shape_version_ahead`. Two directions of that
refusal have distinct consequences:

- **Request direction:** the sync send's reply is the error — the receiving
  actor's state is untouched, it never saw the message.
- **Reply direction — the undecodable-reply caveat:** if the *caller* can't
  decode the reply (a returned Value is newer than the caller knows), **the
  method has already run and the callee's state has already changed** — the
  caller raises `shape_version_ahead` (`details = #{direction => reply}`),
  but the actor it called is not rolled back. This is a second
  possibly-executed failure mode alongside the timeout caveat below; the ADR
  0125 rule (upgrade the *receiving* side of a class first) avoids it for
  requests, and for replies means upgrading a Value's *callers* first.

> **The timeout-vs-`node_down` caveat.** The ordinary 5000 ms sync-send
> timeout (ADR 0043) applies unchanged across the network. Erlang only
> detects a silent partition after `net_ticktime` (default 60 s) — so during
> a real partition, a sync send will normally raise `timeout` **before**
> anything raises `node_down`. These mean different things: `timeout` says
> "no answer arrived in time" (the call may still be running, or may already
> have completed on the far side — a timed-out call is at-most-once-*reply*,
> not at-most-once-*execution*, identical to a local `gen_server:call`
> timeout); `node_down` says "the runtime has confirmed the node is gone".
> Don't treat a `timeout` as proof the remote actor never ran.

### Cluster events

Node membership and shape-version skew are observed through the ordinary
[Announcements](#announcements--typed-events-adr-0093) substrate, not a
polling API — subscribe to the event class you care about:

```beamtalk
SystemAnnouncer current when: NodeUp do: [:e |
  Transcript showCr: "joined: ", e node name asString
]
SystemAnnouncer current when: NodeDown do: [:e |
  Transcript showCr: "lost ", e node name asString, " (", e reason asString, ")"
]
SystemAnnouncer current when: NodeShapeSkew do: [:e |
  Transcript showCr: e node name asString, ": ", e className asString,
    " local v", e localVersion printString, " remote v", e remoteVersion printString
]
```

- **`NodeUp`** / **`NodeDown`** fire for every visible node join/leave
  (`NodeDown` carries a `reason` Symbol). Only *visible* nodes announce —
  hidden (tooling) nodes never produce an event. Announcements stay
  node-local (ADR 0093): each node announces its own view of membership,
  never a cluster-wide broadcast.
- **`NodeShapeSkew`** is an early-warning announcement, not a per-message
  guard: when a node connects, and again whenever a class reloads while
  peers stay connected, this node compares its own `Beamtalk shapeManifest`
  against each connected peer's and announces one event per class whose
  version differs. It **never refuses the connection** — doing so would make
  ADR 0125's rolling-upgrade procedure impossible — it only makes skew
  visible before the first send actually fails on it (the per-message check
  above is the real safety net). `aNode shapeSkew` (below) surfaces the same
  information as a point-in-time, queryable snapshot rather than only a
  live event stream.

**Supervision-tree introspection reaches across nodes too:**
`ProcessNavigation on: aNode` snapshots the *target* node's `default`-scope
supervision tree via `erpc` (the same walk `ProcessNavigation default` runs
locally, executed remotely) and returns `Result(ProcessNavigation, Error)` —
every `SupervisionNode` in the result carries a `node` field naming which
node it came from:

```beamtalk
(ProcessNavigation on: worker) unwrap tree nodesOfKind: #beamtalkActor
```

`aNode shapeSkew` answers a node's current shape-skew count (how many classes
differ in version from this node's own); mapping it over `Node connected`
gives the cross-surface `nodes` operation reached identically from the REPL,
MCP, and LiveView surfaces (see
[`docs/development/surface-parity.md`](development/surface-parity.md)).
The per-node queries `aNode actors`, `actorsOf:`, `actorAt:`, `processes` and
`supervisors` reach a peer via `erpc` and answer `Result` values; a down peer
yields an `Error` tagged `node_down`, never a raise.

### Security model

`connect` (and anything that connects as a side effect: `ping`, a remote
spawn, `shapeManifest`) applies a host policy: a same-host connection is
always allowed; an off-host connection is refused with `kind =
insecure_distribution` unless this node runs TLS distribution
(`-proto_dist inet_tls`). This guard is advisory for the *language* surface,
not an enforcement boundary — raw `net_kernel` calls via `Erlang` FFI, and
Erlang's own auto-connect on a send to a remote pid, bypass it. There is no
cookie API at the language level (the cookie is a VM-level `-setcookie`
concern, ADR 0020/0058).

Only *visible* nodes participate in distribution transparency (spawn,
lookup, cluster events); a **hidden** node (`-hidden`) never appears in
`Node connected`, never fires `NodeUp`/`NodeDown`, and is excluded from
shape-skew checks — hidden nodes are for tooling that observes or attaches
to a cluster without joining it as a peer. Beamtalk's own attach front (ADR
0097) does not yet start as a hidden node; that is a tracked follow-up, not
part of this ADR.

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

Cross-package references to internal classes produce a compile error (`E0401`):

```text
Class 'ConnectionPool' is internal to package 'http' and cannot be referenced from 'my_app'
  help: 'ConnectionPool' is declared 'internal' in package 'http'
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

When the compiler can determine the receiver type (via type annotations, literal class references, or type inference), cross-package sends to internal methods produce a compile error (`E0403`):

```text
Method 'buildHeaders:' is internal to package 'http' and cannot be called from 'my_app'
  help: 'buildHeaders:' is declared 'internal' in 'HttpClient'
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

**Leaked visibility** — if an internal class appears in the public signature of a public method, the compiler emits a hard error (`E0402`). This prevents accidentally exposing implementation types:

```text
Internal class 'TokenBuffer' appears in public signature of 'Parser >> tokenize:'
  help: 'TokenBuffer' is declared 'internal' — make it public, or change the type
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
| Workspace | Connected REPL to live node (`Workspace` class-side facade) |
| Class browser | REPL introspection: `Beamtalk allClasses`, `Beamtalk help: Class` |

---

## Standard Library

109 classes implemented and tested (`stdlib/src/*.bt`; see [Stdlib Implementation Status](stdlib-implementation-status.md) for the current per-class method audit). For detailed API documentation, see [API Reference](https://www.beamtalk.dev/apidocs/).

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
| **Parallel** | Block-based fan-out/join combinators (`all:`, `all:timeout:`, `any:`) — spawns one linked+monitored process per block, blocks the caller, returns plain `Result` values; no awaitable future/promise value is ever exposed (BT-2974) |
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
| **Json** | Data serialisation (JSON) |
| **Regex** | Regular expression matching |
| **DateTime**, **Time**, **Duration** | Date/time operations (`DateTime>>weekday` → ISO Monday=1..Sunday=7) |
| **Random** | Random number generation |
| **Digest** | Cryptographic hash functions and HMAC |

**Networking** (in [`beamtalk-http`](https://github.com/jamesc/beamtalk-http)):

| Class | Description |
|-------|-------------|
| **HTTPServer**, **HTTPClient** | HTTP server and client |
| **HTTPRouter**, **HTTPRoute**, **HTTPRouteBuilder** | Declarative HTTP routing |
| **HTTPRequest**, **HTTPResponse** | Request/response objects |

**Data formats** (in [`beamtalk-yaml`](https://github.com/jamesc/beamtalk-yaml)):

| Class | Description |
|-------|-------------|
| **Yaml** | YAML parsing and serialisation — not part of stdlib; add it as a package dependency ([Package Management guide](beamtalk-packages.md)) |

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

On BEAM, a Binary is a bare Erlang `binary()`, while a String is the tagged
tuple `{beamtalk_string, Bin}` holding UTF-8 bytes (BT-4173). Representation
is by construction: `Binary fromBytes:` answers a Binary even when the bytes
happen to be valid UTF-8, and a String literal answers a String. The runtime
never sniffs bytes to decide — `class` answers the construction type, and
dispatch follows the class.

```beamtalk
// Construction
bin := Binary fromBytes: #(104, 101, 108, 108, 111)
bin := Binary fromIolist: #("hello", " ", "world")

// Byte access (1-based, Collection protocol) — elements are byte Integers
bin := Binary fromBytes: #(104, 101, 108)
bin at: 1                    // => 104
bin size                     // => 3 (byte count)

// Byte access (0-based, Erlang-compatible)
bin byteAt: 0                // => 104 (byte value)

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

// UTF-8 decoding (Binary → String) answers a Result uniformly (BT-4169)
((Binary fromBytes: #(104, 101, 108, 108, 111)) asString) unwrap  // => "hello"
(Binary fromBytes: #(104, 101, 108, 108, 111)) asStringUnchecked   // => "hello"

// Invalid UTF-8: asString answers `Result error:` with the byte offset
((Binary fromBytes: #(255, 254)) asString) isError                // => true
((Binary fromBytes: #(255, 254)) asString) error message          // => "Invalid UTF-8 at byte offset 0"

// Serialization (class methods)
etf := Binary serialize: #(1, 2, 3)
Binary deserialize: etf               // => #(1, 2, 3)
Binary deserializeWithUsed: etf       // => #(value, bytesConsumed)

// Collection protocol — Binary is a collection of byte Integers
bin := Binary fromBytes: #(65, 66, 67, 68, 69)
bin collect: [:b | b]         // => Binary (65, 66, 67, 68, 69)
bin select: [:b | b /= 67]    // => Binary (65, 66, 68, 69)
bin includes: 66              // => true
bin isEmpty                   // => false

// Byte-level substring search (BT-4190)
data := Binary fromBytes: #(1, 2, 3, 4, 5, 3, 4)
needle := Binary fromBytes: #(3, 4)
data includesSubstring: needle   // => true
data indexOf: needle             // => 3 (1-based byte index of first match)
data indexOf: (Binary fromBytes: #(9, 9))  // => nil (not found)
(Binary fromBytes: #()) includesSubstring: (Binary fromBytes: #())  // => true
(Binary fromBytes: #()) indexOf: (Binary fromBytes: #())            // => nil
```

**Method override table (Binary vs String):**

| Method | On Binary | On String |
|--------|-----------|-----------|
| `at: index` | byte Integer (1-based) | grapheme String (1-based) |
| `size` | byte count | grapheme count (alias for `length`) |
| `byteAt: offset` | byte value (0-based) | inherited — byte value (0-based) |
| `do: block` | iterate bytes, answers nil | iterate graphemes |
| `collect:` / `select:` / `reject:` | byte-level, answer Binary | grapheme-level, answer String |
| `includes:` / `detect:` / `anySatisfy:` / `allSatisfy:` | byte-level (Integer elements) | grapheme-level (String elements) |
| `part: offset size: n` | byte-level slice, returns Binary | inherited — byte-level slice, returns Binary |
| `concat:` | byte concatenation, returns Binary | inherited — byte concatenation, returns Binary |
| `asString` | `Result ok:` String, or `Result error:` with offset | no-op, returns self |
| `asStringUnchecked` | unchecked cast to String | no-op, returns self |
| `includesSubstring:` | byte-level substring test | grapheme-level substring test |
| `indexOf:` | 1-based byte index or `nil` | 1-based grapheme index or `nil` |

(`byteSize` was removed in BT-4173 — `size` is the byte count on Binary.)

#### Binary vs String at Runtime

A Binary and a String holding the same bytes are different values with
different classes: `"hi"` is `{beamtalk_string, <<"hi">>}` (class String)
while `Binary fromBytes: #(104, 105)` is `<<"hi">>` (class Binary), even
though both render as `"hi"`. `class` answers the construction type, and
dynamic dispatch of `size`, `at:`, `do:`, and the Collection protocol follows
the class — byte-level on Binary, grapheme-aware on String:

```beamtalk
bytes := Binary fromBytes: #(255, 254, 253)
bytes class name    // => "Binary"
bytes size          // => 3 (bytes, not graphemes)
bytes printString   // => "<<FF FE FD>>" (hex — never raw invalid UTF-8)

text := Binary fromBytes: #(104, 105)
text class name     // => "Binary" (construction type — never sniffed)
text size           // => 2
text at: 1          // => 104 (byte Integer, not "h")
```

Equality is representation-strict: `"a" =:= (Binary fromBytes: #(97))` is
false, and so is `==` — a String never equals a Binary. Convert explicitly
(`asString` / `asStringUnchecked`) before comparing across the two.

The byte/text distinction also holds at the boundaries: FFI calls and file
reads answer Binary for bytes (`Erlang file read_file:`, `File readBinary:`),
while text entry points (string literals, `File readAll:`, JSON string leaves)
answer String. Native code validates untrusted bytes with UTF-8 checks before
constructing a String; only `asStringUnchecked` skips validation, and only
callers that already know the bytes are text may use it.

```beamtalk
// Byte loop over Binary data — elements are Integers throughout
checksum: data =>
  (0 to: data size - 1) inject: 0 into: [:acc :i | acc + (data byteAt: i)]
```

### Bitstring — Unaligned Bits

A `Bitstring` is a sequence of bits that is not a whole number of bytes
([ADR 0141](ADR/0141-bitstring-sealed-class-for-unaligned-bitstrings.md)). The
class follows the value's shape, as in Erlang: a BEAM bitstring whose bit size
is a multiple of 8 is a binary (`Binary`, or `String`), and any other bitstring
is a `Bitstring`. `Bitstring` is a sealed sibling of `Binary`, not its
superclass or subclass, so `isKindOf: Binary` and `:: Binary` are false for it.
Bitstrings come from binary patterns: a `/bitstring` (or `/bits`) segment
binds whatever bits are left.

```beamtalk
tail := (Binary fromBytes: #(234)) match: [<<_:3, rest/bitstring>> -> rest]
tail class                // => Bitstring
tail bitSize              // => 5
tail toBits               // => #(0, 1, 0, 1, 0)  (most significant bit first)
tail printString          // => "<<10:5>>"  (Erlang notation)
tail asBinary isError     // => true  (kind #unaligned_bitstring)
(Binary fromBytes: #(1, 2)) bitSize   // => 16
```

| Selector | Answer |
|---|---|
| `bitSize` | number of bits (also on `Binary`: 8 × byte count) |
| `toBits` | `List(Integer)` of 0/1, most significant bit first |
| `concat:` | the joined bits — a `Binary` when byte-aligned, else a `Bitstring`; a `String` argument joins by its bytes. `Binary>>concat:` stays bytes-only: a `Bitstring` argument is a `type_error`, so join bits from the Bitstring side |
| `asBinary` | always `Result error:` of kind `#unaligned_bitstring`; no silent padding or truncation |
| `printString` / `displayString` | Erlang notation, e.g. `<<2:3>>` |
| `=:=`, `==`, `hash` | structural equality, inherited |

The byte protocol of `Binary` and `Collection` is deliberately not understood:
`size`, `do:`, `at:`, `byteAt:`, `part:size:`, `toBytes`, `includesSubstring:`,
`indexOf:`, `asString`, `asStringUnchecked`, `asBase64`, `asBase64Url`, `asHex`,
`collect:`, `select:`, `inject:into:`, `isEmpty`, `includes:`, and the rest.
Each raises a structured `does_not_understand` whose hint names the bit size:

```beamtalk
tail size
// => does_not_understand: Bitstring does not understand 'size'
//    Hint: a Bitstring is not byte-aligned (5 bits); use 'bitSize'/'toBits',
//    or 'asBinary' to check alignment
```

### Accessing an Empty Collection

Element accessors and subsequence operations differ deliberately on an empty
collection ([BT-3021](https://linear.app/beamtalk/issue/BT-3021)):

**Element accessors raise `empty_collection`.** `first`, `last`, and `at:` have
no element to answer, and `nil` would be indistinguishable from a stored `nil`:

```beamtalk
#() first                  // raises empty_collection
#() last                   // raises empty_collection
#() at: 1                  // raises empty_collection
"" first                   // raises empty_collection
"" at: 1                   // raises empty_collection
#[] first                  // raises empty_collection
#[] last                   // raises empty_collection
(Array withAll: #()) at: 1 // raises empty_collection
(Binary fromBytes: #()) at: 1   // raises empty_collection
(1 to: 0) first            // raises empty_collection
#() max                    // raises empty_collection (also min, average)
```

`Queue` predates this kind and keeps its own `empty_queue` for compatibility,
but classifies as a `RuntimeError` like the rest of the family, so a single
`on: RuntimeError do:` covers both.

`empty_collection` is a `RuntimeError`, so it can be caught on its own —
crucially, *separately* from a genuine typo, which raises
`does_not_understand`:

```beamtalk
[orders first] on: RuntimeError do: [:e | e kind =:= #empty_collection]
```

**Ordered vs. unordered: `first`/`last` vs. `anyOne`**
([BT-3027](https://linear.app/beamtalk/issue/BT-3027)). Only collections with
a defined iteration order — `Array`, `List`, `Interval`, `String`, `Binary` —
have `first`/`last`. `Set`, `Bag`, and `Dictionary` are unordered and
deliberately do **not** have `first`/`last`: either name would misleadingly
promise an ordering guarantee the collection does not make. Sending `first` or
`last` to one of them raises `does_not_understand`, exactly like any other
unimplemented selector — it is not treated as an empty-collection condition:

```beamtalk
Set new first                    // raises does_not_understand
(Bag withAll: #(1, 2)) last      // raises does_not_understand
```

Instead they answer `anyOne` — *some* element with no ordering guarantee (the
answer may differ between calls), raising `empty_collection` when there is
nothing to answer. `anyOne` is defined once on the abstract `Collection`
class in terms of `do:`, so every collection has it, but it is most useful on
the unordered three:

```beamtalk
(Set new add: 1) anyOne           // => 1
Set new anyOne                    // raises empty_collection
(Bag withAll: #(1, 1, 2)) anyOne  // => 1 or 2 — no ordering guarantee
#{#a => 1} anyOne                 // => 1 (a *value*, consistent with do:)
```

If you specifically want the order `asList` currently answers in, convert
first: `aSet asList first`. That order is an implementation detail (Set's
`asList` happens to sort by term order today), not a contract — prefer
`anyOne` when any single element will do.

**Subsequence operations stay total** and answer the empty collection. `rest`,
`take:`, and `drop:` all have a well-defined answer on empty, so they never
raise — which keeps recursive idioms working without a guard at every step:

```beamtalk
#() rest                   // => #()
#() take: 3                // => #()
#() drop: 3                // => #()
#() sum                    // => 0 (identity element, unlike max/min)
```

An out-of-range index into a *non-empty* collection is a different condition
again, and raises `index_out_of_bounds`:

```beamtalk
#(1, 2, 3) at: 9           // raises index_out_of_bounds
#(1, 2, 3) at: 0           // raises index_out_of_bounds (index < 1 is malformed)
```

**`Interval>>at:` always raises `index_out_of_bounds`, never
`empty_collection`, for any out-of-range index** — including on an empty
interval. This differs from `first`/`last` (which do raise
`empty_collection` on an empty interval): `at:` is a bounds-checked keyed
accessor like `Array>>at:`, and `empty_collection` is reserved for no-argument
accessors (`first`, `last`, `anyOne`) where there is no index to blame:

```beamtalk
(1 to: 10) at: 1     // => 1
(1 to: 10) at: 11    // raises index_out_of_bounds
(1 to: 0) at: 1       // raises index_out_of_bounds (not empty_collection)
```

### Lookups That Find Nothing

A lookup on a *non-empty* collection can still fail, and each way it fails gets
its own kind ([BT-3025](https://linear.app/beamtalk/issue/BT-3025)). None of
them is `does_not_understand` — the receiver understands the selector, so
reporting a dispatch failure would send the reader hunting for a typo.

**`detect:` raises `not_found`** when the search runs to completion with no
element matching. `detect:ifNone:` is the non-raising alternative:

```beamtalk
#(1, 2, 3) detect: [:x | x > 10]            // raises not_found
#(1, 2, 3) detect: [:x | x > 10] ifNone: [0] // => 0
```

Every receiver agrees ([BT-3028](https://linear.app/beamtalk/issue/BT-3028)) —
`List`, `Set`, `Bag`, `Dictionary`, `Interval`, `String`, and `Stream`.
`detect:` answers `E`, and `E` has no in-band way to say "nothing matched", so
the raise is the honest encoding; reach for `detect:ifNone:` whenever a miss is
expected:

```beamtalk
#(1, 2, 3) asSet detect: [:x | x > 10]      // raises not_found
(1 to: 3) detect: [:x | x > 10]             // raises not_found
(Stream on: #(1, 2, 3)) detect: [:n | n > 10]  // raises not_found

(1 to: 3) detect: [:x | x > 10] ifNone: [0] // => 0
```

The error names the *receiver's* class, so a no-match on a Set reports `Set`
rather than the abstract `Collection` the shared implementation lives on. An
empty receiver is just the degenerate no-match case and raises `not_found` too,
not `empty_collection` — `detect:` is a search, not an element accessor.

**`List from:to:` raises `index_out_of_bounds`** for a start index below 1,
matching `at:`. Its other edge cases stay total — an end below the start is an
empty range, and an end past the last index is clamped:

```beamtalk
#(1, 2, 3) from: 0 to: 2   // raises index_out_of_bounds
#(1, 2, 3) from: 3 to: 1   // => #()  (empty range, not an error)
#(1, 2, 3) from: 2 to: 99  // => #(2, 3)  (clamped)
```

`not_found` is a `RuntimeError`, like `empty_collection` and
`index_out_of_bounds`, so `on: RuntimeError do:` catches the whole family and
`e kind` discriminates within it.

### Interval — Arithmetic Sequences

An `Interval` represents an arithmetic sequence of integers without materialising a list. Create one with `to:` or `to:by:` on any `Integer`:

```beamtalk
1 to: 10                    // => (1 to: 10) — 10 elements: 1, 2, ..., 10
1 to: 10 by: 2             // => (1 to: 10 by: 2) — 5 elements: 1, 3, 5, 7, 9
10 to: 1 by: -1            // => (10 to: 1 by: -1) — 10 elements: 10, 9, ..., 1

(1 to: 10) size            // => 10
(1 to: 10) first           // => 1
(1 to: 10) last            // => 10
(1 to: 10) at: 5            // => 5 — bounds-checked; out-of-range raises
                             // index_out_of_bounds (see "Accessing an Empty
                             // Collection" above), never empty_collection
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

// Bag is unordered, so it has no first/last — use anyOne for a single
// element (no ordering guarantee) or asList first/last for a fixed order:
(Bag withAll: #(1, 1, 2)) anyOne        // => 1 or 2, no ordering guarantee
Bag new anyOne                          // raises empty_collection
```

`Set` and `Dictionary` are unordered the same way and follow the same rule —
see "Accessing an Empty Collection" above for the full `first`/`last`/`anyOne`
split.

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
| `detect:` | First matching element; raises `not_found` if none | `s detect: [:n \| n > 10]` |
| `detect:ifNone:` | First matching element, or the default if none | `s detect: [:n \| n > 10] ifNone: [0]` |
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
| `File open: path mode: mode` | `FileHandle` | Open a handle the caller must close |
| `File open: path mode: mode do: block` | block value | Block-scoped handle, closed on exit |
| `File openHandles` | `Array` | Outstanding `open:mode:` handles: `#(path mode owner)` |

```beamtalk
File writeAll: "output.txt" contents: "hello"
File readAll: "output.txt"              // => "hello"
File mkdirAll: "target/data/logs"
File listDirectory: "target/data"       // => ["logs"]
File rename: "output.txt" to: "target/data/output.txt"
File delete: "target/data/output.txt"
File deleteAll: "target/data"
```

#### Incremental File I/O — FileHandle

`File open:mode:` and `File open:mode:do:` give you a `FileHandle` for reading
and writing against the file's *current position*, instead of whole-file
class methods. This is what an append-only log wants: one open handle, an
explicit `sync` per record.

| Mode | Behaviour |
|------|-----------|
| `#read` | Read only; the file must exist |
| `#write` | Truncate or create, write only |
| `#append` | Create if absent; every write lands at end-of-file |
| `#readWrite` | Create if absent; existing contents kept |

Write-capable modes create missing parent directories.

| Method | Returns | Description |
|--------|---------|-------------|
| `handle read: n` | `Binary` | Up to `n` bytes from the current position |
| `handle readAll` | `Binary` | From the current position to end-of-file |
| `handle write: data` | `nil` | Write a String or Binary |
| `handle writeLine: data` | `nil` | Write followed by a newline |
| `handle position` | `Integer` | Current byte offset |
| `handle seek: offset` | `Integer` | Move to an absolute offset, returns it |
| `handle flush` | `nil` | Push buffered writes to the OS |
| `handle sync` | `nil` | fsync — force data to physical storage |
| `handle close` | `nil` | Close the handle (idempotent) |
| `handle isOpen` | `Boolean` | Whether the handle is still open |
| `handle lines` | `Stream(String)` | Lazy lines from the current position |

Every method returns a `Result` except `isOpen`, which answers a Boolean, and
`lines`, which answers a Stream — with no Result to carry an error, `lines`
raises on a closed or write-only handle instead.

```beamtalk
// Block scope — the handle is closed however the block exits
(File open: "events.log" mode: #append do: [:handle |
  handle writeLine: "an event".
  handle sync
]) unwrap

// Caller-owned handle — you close it
handle := (File open: "data.bin" mode: #read) unwrap
(handle seek: 16) unwrap
header := (handle read: 4) unwrap
handle close
```

Reading past end-of-file is not an error: `read:` returns a binary shorter than
requested, and an empty binary once the handle is at end-of-file.

**Closed handles error, they don't crash.** Every operation on a closed handle
returns a structured `Result error:`, as does a read on a `#write` handle or a
write on a `#read` handle:

```beamtalk
(File open: "notes.txt" mode: #read do: [:handle |
  handle close.
  handle read: 1        // => Result error: FileHandle 'read:' is closed
]) unwrap
```

Handles are process-scoped values (`HandleScoped(#process)`): they can be held
across calls but not sent to another actor. The descriptor underneath is a BEAM
process, so it keeps working wherever the handle is held on this node — but it
does not survive crossing a node boundary.

A handle from `open:mode:` is yours to close, but you are not the only backstop.
Beamtalk registers it against an *owner* and closes an owner's outstanding
handles when the owner dies. The rule is the same under the REPL,
`beamtalk run`, `beamtalk test` and a release, and whether the send is static
(`File open: p mode: m`) or dynamic (`File perform: #open:mode: ...`):

1. At the REPL, the **session** owns it, so the handle survives from one
   statement to the next.
2. Otherwise the **calling process** owns it — an actor, a supervisor worker,
   or the plain process a test method or `beamtalk run` script executes in.

The underlying descriptor lives exactly as long as its owner, no longer and no
shorter. `File openHandles` lists every outstanding handle as
`#(path mode owner)` for diagnostics. Still, reach for `open:mode:do:`
whenever a block scope will do — it needs no owner at all.

**Open blocks run in your own process.** `open:do:` and `open:mode:do:` are
lowered at the call site (ADR 0109), so only the open itself touches the `File`
class — the block, and the close that follows it, run where you called from.
A block may therefore message `File` again, holds no shared resource, and is
under no time limit:

```beamtalk
// Fine — the block is running in your process, not inside File
File open: "a.txt" mode: #read do: [:h | File exists: "b.txt"]
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

@expect unresolved_ffi, type
(Erlang some_module) someCall   // suppresses both categories on this one expression
```

**Combined form (BT-3387):** A single `@expect` can list more than one category, separated by commas, to suppress every listed category on the same following expression or declaration — useful when one expression genuinely triggers two independent diagnostics at once (e.g. an FFI call to an unknown module — outside the known-OTP list, the `beamtalk_*` modules, and the package's own dependency/native modules — whose return type is also inferred as `Dynamic`). The directive is only reported stale if *none* of its listed categories match a real diagnostic; if at least one does, the directive is satisfied even though another listed category turned out unnecessary.

**Suppression categories:**

| Category | Suppresses |
|----------|-----------|
| `dnu` | Does-not-understand hints |
| `type` | Type mismatch warnings *and* method-not-found (DNU) hints |
| `unused` | Unused variable warnings |
| `type_annotation` | Missing or redundant type annotation warnings in typed classes |
| `inheritance` | Sealed-class/sealed-method constraint errors |
| `class_state_abroad` | A block that reads or writes its class's variables where it runs outside an invocation of that class (ADR 0130 §5) |
| `class_process_ffi` | A class-side method that runs in its class's process (its class or an ancestor declares class variables) and directly calls process-sensitive FFI (`gen_tcp:accept`, `controlling_process`, `erlang:self`, …) (BT-4031) |
| `class_method_process_resource` | A class-side method that runs in the caller's process (no class variables in its chain, ADR 0142) directly creates a resource tied to the caller (`ets:new`, `open_port`, `link`, `monitor`, timers, …) or acts on the caller (`self()`, the process dictionary, `process_flag`) (BT-4302) |
| `ffi_nested_string` | A `String` nested in a collection argument (`List(String)`, `Dictionary` keys/values, `Tuple`) to a stock Erlang call reaches it tagged; an Erlang comparison against raw binaries misses (BT-4243, see [Strings at the FFI boundary](beamtalk-native-erlang.md#strings-at-the-ffi-boundary)) |
| `actor_state_abroad` | A block that touches its actor's state where it runs outside the actor's process (ADR 0137) *(lint-only, see below)* |
| `dead_assignment` | `beamtalk lint`'s "assignment inside an escaping block" check *(lint-only, see below)* |
| `lint` | Style/redundancy findings: `beamtalk lint`'s unnecessary-parentheses, redundant trailing `^`, cascade-candidate, … passes, plus the unreachable-code / shadowed-variable / unattached-doc-comment advisories *(lint-only, see below)* |
| `all` | Any diagnostic on the following expression *(discouraged — use a specific category)* |

**Lint-only categories (`dead_assignment`, `lint`, `actor_state_abroad`):** the diagnostics these suppress are produced only by `beamtalk lint`'s dedicated passes, which `beamtalk build`/`beamtalk test`, the LSP, and the REPL never run. Those surfaces therefore leave such a directive alone — neither satisfied nor reported stale — and only `beamtalk lint` validates it (BT-3384). `@expect lint` exists for test fixtures that deliberately pin a codegen shape the style lint would otherwise tell you to rewrite (e.g. a trailing `^` whose explicit-return lowering is the thing under test); ordinary code should just take the lint's advice.

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

Declaration-level `@expect` supports the same categories, combined-category lists, and stale-directive rules as expression-level `@expect`.

**Unknown categories are parse errors:** Writing an unknown category (e.g. `@expect selfcapture`) is rejected at parse time with an error listing the valid names. This prevents typos from silently suppressing nothing.

**Stale directives:** If `@expect` does not suppress any diagnostic (because no matching diagnostic exists on the following expression or declaration), the compiler emits a warning to prevent directives from silently becoming out of date.

`@expect` works inside method bodies, inside block bodies (e.g., `ifTrue: [...]`, `collect: [...]`, `whileTrue: [...]`), on declarations in class definitions, inside `Protocol define:` bodies (above a provided method, to suppress a provision diagnostic published in the protocol file — `beamtalk build` and `beamtalk lint` only; BT-3671), and at module scope (BT-2010).

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
// In stdlib/src/integer.bt — bare form, selector inferred ('+' and 'asString')
+ other => @primitive
asString => @primitive

// In stdlib/src/exception.bt — explicit override (method 'signal' → runtime 'classSignal')
class signal => @primitive 'classSignal'
```

**`@intrinsic` (unquoted)** — compiler structural intrinsics:
```beamtalk
// In stdlib/src/block.bt
value => @intrinsic blockValue
whileTrue: bodyBlock => @intrinsic whileTrue

// In stdlib/src/object.bt
new => @intrinsic basicNew
hash => @intrinsic hash
```

The full list of structural intrinsics: `blockValue`, `blockValue1`–`blockValue3`, `whileTrue`, `whileFalse`, `repeat`, `onDo`, `ensure`, `timesRepeat`, `toDo`, `toByDo`, `basicNew`, `basicNewWith`, `hash`, `respondsTo`, `fieldNames`, `fieldAt`, `fieldAtPut`, `dynamicSend`, `dynamicSendWithArgs`, `error`.

**`Actor>>spawn`/`spawnWith:` (BT-3072).** Unlike the intrinsics above, `spawn`/`spawnWith:` are *not* `@intrinsic` — `actor.bt` declares real FFI bodies (`(Erlang beamtalk_actor) doSpawn: self`). The compiler's static `Counter spawn` / `self spawn` call sites still lower directly to `beamtalk_actor:safe_spawn`/`class_self_spawn` (unchanged, to avoid serializing every spawn through the class `gen_server`) — the declared body is the documented source of behaviour for the dynamic dispatch path, mirroring `spawnAs:`/`spawnWith:as:`'s existing FFI-body shape. See `stdlib/src/actor.bt`.

**`Actor>>new`/`new:` (BT-3074).** Also not `@intrinsic` — `actor.bt` declares real bodies that send `Exception signalKind:class:selector:hint:` (BT-3042), the general-purpose pure-Beamtalk way to raise a named error kind with a hint. `class_send` intercepts `new`/`new:` before class-method dispatch ever reaches these declarations (routing instead to the compiled per-actor-module `new/0`/`new/1` stub the runtime's `{new, Args}` fast path requires), so this is purely the documented, xref-visible source for the selector — never the code path an actual send executes. The type checker correctly proves these bodies diverge via the class-side keyword send to `Exception signalKind:class:selector:hint:` (BT-3075 unified the type-string resolver).

**Relationship to `native:` (ADR 0101).** `@primitive` and `@intrinsic` cover native BEAM *value types* and compiler *substrate*. A third mechanism, the class-level `native:` declaration with `=> self delegate` bodies, covers whole-class **delegation** to a single Erlang module (a stateless `Object` such as `Stream`, or an `Actor` gen_server). Pick by what the method needs: guarded dispatch + the open-world extension registry → `@primitive`; the dispatch act itself (`==`, `class`, `perform:`, actor lifecycle) → `@intrinsic`; pure pass-through to one module → `native:`. See [`native:` for stateless Objects](beamtalk-native-erlang.md#native-stateless-objects--native-for-object).

**`whileTrue:`/`whileFalse:`/`repeat`/`on:do:`/`ensure:` via `perform:` (BT-2908).** These five structural intrinsics have real semantics only at the call-site interception the compiler recognizes for a literal message send (e.g. `[cond] whileTrue: [body]` written directly in source). Reaching the compiled method body through any other dispatch path — `perform:`, `perform:withArguments:`, or other generic/dynamic sends — used to resolve to a placeholder and raise a misleading `does_not_understand` naming the wrong (internal) selector. This is now handled the same way BT-2812/BT-2888 handle it for `value*`/`do:`/`collect:`/etc.: a **Tier 1** (pure) condition/receiver/handler/cleanup block gets a real generic implementation — a runtime loop for `whileTrue:`/`whileFalse:`/`repeat`, a runtime `try`/`catch` for `on:do:`/`ensure:` — since neither needs the block's literal AST, only its (opaque, already-evaluated) fun value. A **Tier 2** (stateful, ADR-0041) block reached this way raises a clear `#stateful_block_dispatch` error instead, since a generically-dispatched call site has no live `StateAcc` to thread mutations through. `respondsTo:` returns `true` for these selectors either way, since the method genuinely is defined.

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
cache at: "key" ifPresent: [:v | v + 1]  // => 43 (evaluate block if present; nil if absent)
cache includesKey: "key"        // => true
cache includesKey: "other"      // => false
cache includes: 42              // => true (scan values)
```

#### Iteration and Snapshots

`Ets` conforms to `Enumerable(V)`, so collection messages work over a point-in-time snapshot of values:

```beamtalk
cache do: [:v | Transcript show: v]
cache select: [:v | v > 10]     // => List of matching values
cache collect: [:v | v * 2]     // => List of transformed values

// Key/value iteration
cache keysAndValuesDo: [:k :v | Transcript show: k, ' => ', v]

// Snapshot to Dictionary
d := cache asDictionary          // => Dictionary(K, V)

// Snapshot values to List
vs := cache values               // => List(V)
```

#### Atomic Operations

For `#set` and `#orderedSet` tables only (raises `unsupported_table_type` on bag tables):

```beamtalk
cache at: "hits" increment: 1   // atomic add via ets:update_counter; missing key starts at 0
cache at: "key" update: [:v | (v ifNil: [0]) + 1]  // atomic read-modify-write (CAS loop)
```

`at:update:` runs an optimistic compare-and-swap loop: the block receives the current value (`nil` if missing) and must be side-effect-free, since it may run more than once under contention.

#### Other Operations

```beamtalk
cache size                      // => number of entries
cache isEmpty                   // => true if no entries
cache isNotEmpty                // => true if at least one entry
cache keys                      // => List of all keys (order unspecified for #set)
cache removeKey: "key"          // delete entry; no-op if key is absent
cache removeAll                 // remove all entries, keep the table
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

  setUp => self.counter := Counter spawn

  testIncrement =>
    self.counter increment.
    self assert: (self.counter getValue) equals: 1
```

For multiple fields, chain the assignments — the LAST statement is what the runner threads through as `self`:

```beamtalk
TestCase subclass: IntegrationTest
  field: db = nil
  field: cache = nil

  setUp =>
    self.db := DB connect
    self.cache := Cache spawn

  testLookup =>
    self.cache at: "key" put: "value".
    self assert: (self.cache at: "key") equals: "value"
```

**Key points:**
- Declare test instance variables with `field:` (not `state:`) since TestCase is a Value subclass
- `self.field := value` in the LAST statement of `setUp` returns the updated self — this is a special case for value-type field assignment (BT-833/BT-900), not the general rule that an assignment evaluates to the assigned value. It holds with or without parentheses: `(self.field := value)` as the last statement also returns the updated self (BT-4092)
- Each test method receives the setUp'd value as `self` — mutations to actor references work normally, but `self` itself is immutable

**Any statement after the last `self.field := value` silently drops that field** (BT-3391) — `setUp`'s return value is always its last expression's value, and only a trailing field assignment (or bare `self`) evaluates to the updated self. Adding one more line after your last assignment — a `super setUp` call, a log line, an unrelated local — makes `setUp` return THAT statement's value instead, and every field set above it reads back as its default in every test method, with no compile or runtime error:

```beamtalk
setUp =>
  self.db := DB connect
  self.cache := Cache spawn
  Transcript show: "set up".   // <- adding this line breaks self.db and self.cache!
```

Fix it by ending with an explicit trailing `self`:

```beamtalk
setUp =>
  self.db := DB connect
  self.cache := Cache spawn
  Transcript show: "set up".
  self   // <- carries both field mutations forward
```

`beamtalk build`/`beamtalk lint` warn when a `setUp` mutates a field — via `self.field := value` or a `with<Field>:` send — and its last statement isn't a bare `self`, another field assignment, a `with<Field>:` send, or a `with*:` cascade.

Chained `with*:` setters (`self withCounter: (Counter spawn)`) remain a valid alternative to `self.field := value` — each `with*:` call itself returns the fully updated self, so the chain's own last call already carries every field set through. This only holds when the `with*:` chain is itself the trailing statement of `setUp`: like a bare `self.field := value`, its self-preserving return value only threads through when nothing follows it — the same trap applies, and `beamtalk lint` (BT-3391/BT-3395) warns for both forms.

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

#### Shared Tests — Abstract Test Cases

To run one list of tests against several subjects, write the tests once on an `abstract` `TestCase` subclass with a hook the subclasses answer, and give each subject a concrete subclass. A test class runs its own `test*` methods and those it inherits from every superclass below `TestCase` (as well as an inherited `setUp`/`tearDown`/`setUpOnce`/`tearDownOnce`); an `abstract` test class is never run itself.

```beamtalk
// stdlib/test/fixtures/stack_contract_test.bt
abstract TestCase subclass: StackContractTest
  subject => self subclassResponsibility

  testPushPop =>
    s := self subject new
    s push: 1
    self assert: s pop equals: 1

// stdlib/test/list_stack_test.bt
StackContractTest subclass: ListStackTest
  subject => ListStack
```

Put the abstract class under `fixtures/` (fixtures are compiled for every test file, so the subclasses in other test files can name it as their superclass). `stdlib/test/fixtures/class_var_semantics_matrix_test.bt` is a worked example.

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
