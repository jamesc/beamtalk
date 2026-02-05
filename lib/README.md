# Beamtalk Standard Library

This directory contains the foundational standard library for Beamtalk, following Smalltalk philosophy where "everything is a message send."

## Philosophy

From [beamtalk-principles.md](../docs/beamtalk-principles.md): **"Everything is a message send, following Smalltalk/Newspeak philosophy."**

Even primitive operations like arithmetic, boolean logic, and control flow are implemented as messages to objects. This provides:

1. **Uniform syntax** - No special forms, everything is `receiver message`
2. **First-class operations** - Operators are just messages that can be overridden
3. **Live debugging** - All operations can be traced and inspected
4. **Extensibility** - Users can extend primitives by adding methods

## The Uniform Message-Sending Illusion

From the programmer's perspective, all objects respond to messages the same way:

```beamtalk
// These LOOK identical:
42 + 3              // message send to Integer
'hello' size        // message send to String  
counter increment   // message send to Actor

// Same syntax. Same mental model. Different machinery.
```

But the implementation differs:

| What you write | What actually happens |
|----------------|----------------------|
| `42 + 3` | Compiled to `erlang:'+'(42, 3)` — inline function call |
| `'hello' size` | Compiled to `erlang:byte_size(...)` — function call |
| `counter increment` | Compiled to `gen_server:call(Pid, increment)` — process message |

**The programmer doesn't care** — the abstraction is uniform.

## Class Hierarchy

Beamtalk's class hierarchy distinguishes **value types** (which inherit from Object) from **actors** (which inherit from Actor), similar to Swift's `struct` vs `actor`:

```
ProtoObject (minimal root - identity, DNU)
  └─ Object (value types - reflection, nil testing)
       ├─ Integer, String, etc. (sealed primitives)
       ├─ Point, Color, etc.    (user value types) [instantiation TBD]
       └─ Actor                 (process-based - spawn, mailbox)
            └─ Counter, MyService, etc. (user actors)
```

**Key insight:** Object is the root of most instances (primitives, user value types, and actors). Only minimal proxy classes inherit directly from ProtoObject. Actor extends Object to add process-based concurrency.

**Note:** Universal `new` method for value type instantiation is planned but not yet implemented. Currently only `spawn`/`spawnWith:` are supported for Actor classes. Value type instantiation mechanism is tracked in issue BT-221.

### Choosing Your Base Class

**Decision tree:**

1. **Do you need custom message forwarding or minimal wrapper?**
   - ✅ Inherit from **ProtoObject** (rare - only for proxies and foreign objects)
   - ❌ Continue to step 2

2. **Does your class need process isolation and concurrency?**
   - ✅ Inherit from **Actor** (most application code - use `spawn` to create instances)
   - ❌ Inherit from **Object** (value types like Point, Color, Config - instantiation TBD)

**Examples:**

| Class | Base | Reason |
|-------|------|--------|
| `Counter` | Actor | Needs process, mutable state, concurrent access |
| `Worker` | Actor | Background task with lifecycle |
| `Point` | Object | Immutable value, no concurrency |
| `Config` | Object | Data structure, copied when passed |
| `Proxy` | ProtoObject | Custom message forwarding |

### Value Types vs Actors

Both value types and actors inherit from Object, but Actor adds process-based concurrency:

| | Value Type (`Object subclass:`) | Actor (`Actor subclass:`) |
|---|---|---|
| **Inherits from** | Object | Object (via Actor) |
| **Process** | ❌ No | ✅ Yes (BEAM process) |
| **Instantiate** | TBD (not yet implemented) | `Counter spawn` |
| **State** | In caller's heap | In own process |
| **Pass between actors** | Copied | Send pid reference |
| **Thread safety** | Caller's responsibility | Mailbox serializes |

**Value types** (Object subclass) are great for: Point, Rectangle, Color, Config, DTOs  
**Actors** (Actor subclass) are great for: Services, Agents, Stateful entities

**Note:** The examples below show the *intended* design. Universal instantiation for value types (`Point new`) is not yet implemented in the compiler. Currently only Actor classes support `spawn`/`spawnWith:`. See issue BT-221 for value type instantiation.

```beamtalk
// Value type - no process, value semantics (instantiation TBD)
Object subclass: Point
  state: x, y
  + other => Point new x: (self.x + other x) y: (self.y + other y)

// p := Point new x: 3 y: 4   // ⚠️  Not yet implemented

// Actor - has process, uses spawn  
Actor subclass: Counter
  state: count = 0
  increment => self.count := self.count + 1

c := Counter spawn         // ✅ Actor uses spawn
// c := Counter new        // ⚠️  Not yet implemented (will error when value type instantiation added)
```

Both use the same message-sending syntax — the difference is in state management and instantiation.

See [beamtalk-object-model.md](../docs/beamtalk-object-model.md) for the full design rationale.

### Metaclass Hierarchy

Just as in Smalltalk, the metaclass hierarchy mirrors the instance hierarchy:

```
Instance Hierarchy:          Metaclass Hierarchy:
ProtoObject           <---   ProtoObject class
  └─ Object          <---     └─ Object class
       ├─ Integer   <---          ├─ Integer class
       └─ Actor     <---          └─ Actor class
            └─ Counter <---            └─ Counter class
```

Each class is itself an object - a process holding metadata (methods, superclass, instance variables). This enables:
- Runtime class introspection (`Counter methods`, `Counter superclass`)
- Hot patching methods (`Counter >> #increment put: [...]`)
- Factory methods (`Counter spawnWith: #{value => 10}`)

## Core Classes

### ProtoObject (`ProtoObject.bt`)

Absolute root of the class hierarchy providing only essential messages.

**Key messages:**
- `class` - Returns the object's class (fundamental reflection)
- `doesNotUnderstand:args:` - Fallback for unknown messages (enables proxies)
- `==` - Object identity test (reference equality)
- `~=` - Object inequality test (delegates to `==`)

**When to use:**
- Implementing proxy objects with custom message forwarding
- Wrapping foreign objects or external resources
- Building minimal wrappers with custom dispatch

**Usage:**
```beamtalk
// Proxy that forwards all messages to a delegate
ProtoObject subclass: Proxy
  state: target

  doesNotUnderstand: selector args: arguments =>
    target perform: selector withArguments: arguments
```

For normal classes, inherit from Object (value types) or Actor (processes).

### Object (`Object.bt`)

Root class for value types and parent of Actor. Object inherits from ProtoObject and adds reflection, nil testing, and debugging capabilities that almost all objects need.

**Inheritance:**
- Value types (primitives like Integer, String, and user types like Point, Color) inherit from Object directly
- Actors (Counter, Worker, Services) inherit from Actor, which inherits from Object

**Role in hierarchy:**
- Object is the parent of primitives, user value types, and Actor
- Value type instantiation mechanism is not yet implemented (tracked in BT-221)
- Actor provides `spawn` method for creating process-based instances

**Key messages:**

*Reflection:*
- `class` - Returns the object's class (inherited from ProtoObject)
- `respondsTo:` - Check if object understands a message
- `instVarNames` - List of instance variable names
- `instVarAt:` - Read instance variable by name

*Nil Testing:*
- `isNil` - Returns false for all objects except Nil
- `notNil` - Returns true for all objects except Nil
- `ifNil:` - Conditional execution if nil
- `ifNotNil:` - Conditional execution if not nil
- `ifNil:ifNotNil:` - Two-way conditional
- `ifNotNil:ifNil:` - Reversed-order two-way conditional

*Debugging:*
- `inspect` - Show description on Transcript
- `describe` - Return string description

**Usage:**
```beamtalk
// Value type - no process, value semantics
Object subclass: Point
  state: x, y
  
  + other => Point new x: (self.x + other x) y: (self.y + other y)
  distance => (self.x squared + self.y squared) sqrt

// p := Point new x: 3 y: 4  // ⚠️  Not yet implemented
// p distance                 // Future: will work when value type instantiation added
```

**Note:** Value type instantiation is not yet implemented. See issue BT-221.

### Actor (`Actor.bt`)

Base class for process-based objects. Every Actor instance is a BEAM process with its own mailbox. Actor inherits from Object and adds concurrency features.

**Inheritance:** `ProtoObject → Object → Actor`

**Key differences from Object:**
- Actor instances are BEAM processes with their own state and mailbox
- Use `spawn` to create instances
- Value type instantiation is not yet implemented

**Key messages:**
- `spawn` - Create new instance (class-side message)
- `spawnWith:` - Create with initialization arguments (class-side message)
- All Object messages (inherited: isNil, notNil, inspect, etc.)

**Usage:**
```beamtalk
Actor subclass: Counter
  state: count = 0
  
  increment => self.count := self.count + 1
  getValue => self.count

counter := Counter spawn   // ✅ Actors use spawn
counter increment
value := counter getValue

// counter := Counter new  // ⚠️  Not yet implemented
```

### Beamtalk (`beamtalk.bt`)

Global class for system-wide reflection and introspection. The Beamtalk class provides Smalltalk-style access to the class registry and global namespace.

**Inheritance:** `ProtoObject → Object → Beamtalk`

**Key messages (all class-side):**
- `allClasses` - Returns list of all registered classes in the system
- `classNamed:` - Look up a class by name (symbol), returns nil if not found
- `globals` - Returns the global namespace dictionary (placeholder)

**Usage:**
```beamtalk
// List all classes in the system
classes := Beamtalk allClasses
// => [ProtoObject, Object, Actor, Integer, String, Counter, ...]

// Look up a class by name
CounterClass := Beamtalk classNamed: #Counter
CounterClass name              // => #Counter
CounterClass superclass        // => Actor class object

// Use class object to spawn instances
counter := CounterClass spawn
counter increment

// Handle missing classes
MissingClass := Beamtalk classNamed: #NonExistent
// => nil

MissingClass
  ifNil: [Transcript show: 'Class not found']
  ifNotNil: [:cls | cls spawn]
```

**Implementation:**
- Beamtalk is a value type (Object subclass), not an actor
- All methods are class-level - no instance creation needed
- Acts like a namespace providing system-wide access
- Class methods implemented as compiler primitives
- Calls into runtime class registry (`beamtalk_object_class:all_classes/0`, `beamtalk_object_class:whereis_class/1`)

**Note:** Full testing requires BT-224 (auto-loading standard library).

### Boolean (`True.bt`, `False.bt`)

Control flow via message sends to boolean objects.

**Key messages:**
- `ifTrue: ifFalse:` - Conditional execution
- `and:` - Logical AND (short-circuit)
- `or:` - Logical OR (short-circuit)
- `not` - Logical negation

**Usage:**
```beamtalk
(x > 0) ifTrue: [self process: x] ifFalse: [self reject]

(found) or: [self searchAgain]

valid := enabled not
```

### Nil Testing Protocol (`Nil.bt`, `Object.bt`)

All objects respond to nil-testing messages, enabling nil-safe control flow. Nil represents the absence of a value, equivalent to Smalltalk's UndefinedObject.

**Testing messages (all objects):**
- `isNil` - Returns true for nil, false for all other objects
- `notNil` - Returns false for nil, true for all other objects

**Control flow messages:**
- `ifNil: nilBlock` - Nil: evaluates block; other objects: returns self
- `ifNotNil: notNilBlock` - Nil: returns self; other objects: evaluates block with receiver as argument
- `ifNil: nilBlock ifNotNil: notNilBlock` - Conditional based on nil status
- `ifNotNil: notNilBlock ifNil: nilBlock` - Same as above, reversed order

**Usage:**
```beamtalk
// Testing for nil
value isNil ifTrue: [self useDefault]
42 isNil       // => false
nil isNil      // => true

// Nil-safe access with default
name := user name ifNil: ['Anonymous']

// Safe transformation with ifNotNil:
result := value ifNotNil: [:v | v * 2]
42 ifNotNil: [:v | v + 1]    // => 43
nil ifNotNil: [:v | v + 1]   // => nil

// Conditional on nil status
result ifNil: [0] ifNotNil: [:val | val * 2]

// Real-world pattern
userName ifNil: ['Guest'] ifNotNil: [:name | 'Hello, ' + name + '!']
```

**Implementation:**
- Nil defines `isNil => ^true` and overrides the default behavior
- All other objects inherit `isNil => ^false` and `notNil => ^true` from Object
- `ifNotNil:` passes the receiver to the block: `notNilBlock value: self`

### Block (`Block.bt`)

First-class closures that capture lexical environment.

**Key messages:**
- `value` - Evaluate with no args
- `value:` - Evaluate with one arg
- `value: value:` - Evaluate with two args
- `whileTrue:` - Loop while condition holds
- `repeat` - Loop forever

**Usage:**
```beamtalk
add := [:x :y | x + y]
result := add value: 3 value: 4  // => 7

[counter < 10] whileTrue: [
  self process: counter
  counter := counter + 1
]
```

### Integer (`Integer.bt`)

Arbitrary precision integer arithmetic (Erlang integers).

**Key messages:**
- Arithmetic: `+`, `-`, `*`, `/`, `//`, `%`, `**`
- Comparison: `=`, `~=`, `<`, `<=`, `>`, `>=`
- Iteration: `timesRepeat:`, `to:do:`, `to:by:do:`
- Testing: `isEven`, `isOdd`, `isZero`, `isPositive`, `isNegative`

**Usage:**
```beamtalk
result := 5 + 3 * 2  // => 11 (standard precedence)
quotient := 17 // 5   // => 3

10 timesRepeat: [Transcript show: 'Hello']
1 to: 100 do: [:n | sum := sum + n]
```

### String (`String.bt`)

UTF-8 encoded text with grapheme-aware operations.

**Key messages:**
- Access: `length`, `at:`, `at:put:`
- Transform: `uppercase`, `lowercase`, `trim`, `reverse`
- Concatenate: `++`, `,`
- Search: `includes:`, `startsWith:`, `endsWith:`, `indexOf:`
- Split/join: `split:`, `join:`
- Iterate: `each:`, `collect:`, `select:`

**Usage:**
```beamtalk
greeting := 'Hello, ' ++ name ++ '!'
shout := message uppercase
(text includes: 'error') ifTrue: [self logError]

word each: [:char | Transcript show: char]
caps := word collect: [:char | char uppercase]
```

### Collection Hierarchy

Beamtalk provides a rich collection framework following Smalltalk conventions:

```
Collection (abstract base)
  ├─ SequenceableCollection (ordered, indexed)
  │     ├─ Array (fixed-size tuple)
  │     └─ List (linked list)
  ├─ Set (unordered, unique)
  └─ Dictionary (key-value)
```

### Collection (`Collection.bt`)

Abstract base protocol for all collection types.

**Common messages (all collections):**
- Size: `size`, `isEmpty`, `isNotEmpty`
- Test: `includes:`, `allSatisfy:`, `anySatisfy:`
- Modify: `add:`, `addAll:`, `remove:`
- Iterate: `each:`, `collect:`, `select:`, `reject:`
- Reduce: `inject:into:`, `count:`
- Convert: `asArray`, `asList`, `asSet`

### SequenceableCollection (`SequenceableCollection.bt`)

Protocol for ordered, indexed collections (Array and List).

**Additional messages:**
- Access: `at:`, `at:put:`, `first`, `last`
- Slice: `from:to:`, `first:`, `last:`
- Order: `sorted`, `sortedBy:`, `reversed`
- Index: `indexOf:`, `findFirst:`
- Concat: `++`

### Array (`Array.bt`)

Fixed-size indexed collection using Erlang tuples. Fast O(1) access.

**Usage:**
```beamtalk
point := {10, 20}
x := point at: 1        // => 10
moved := point at: 1 put: 15   // => {15, 20}

numbers := {1, 2, 3, 4, 5}
doubled := numbers collect: [:n | n * 2]  // => {2, 4, 6, 8, 10}
```

### List (`List.bt`)

Linked list using Erlang lists. Fast O(1) prepend.

**Additional messages:**
- `head`, `tail` - Access first element and rest
- `prepend:` - Add to front (O(1))
- `flatten`, `zip:`, `partition:`

**Usage:**
```beamtalk
items := [1, 2, 3, 4, 5]
first := items head          // => 1
rest := items tail           // => [2, 3, 4, 5]
extended := items prepend: 0 // => [0, 1, 2, 3, 4, 5]
```

### Set (`Set.bt`)

Unordered collection of unique elements.

**Additional messages:**
- `union:`, `intersection:`, `difference:`
- `isSubsetOf:`, `isSupersetOf:`, `isDisjointFrom:`

**Usage:**
```beamtalk
colors := Set fromList: [#red, #green, #blue]
warm := Set fromList: [#red, #orange, #yellow]

both := colors intersection: warm     // => {#red}
all := colors union: warm             // => {#red, #green, #blue, #orange, #yellow}
(colors includes: #red) ifTrue: [Transcript show: 'Has red']
```

### Dictionary (`Dictionary.bt`)

Key-value collection using Erlang maps.

**Key messages:**
- `at:`, `at:put:`, `at:ifAbsent:`
- `includesKey:`, `removeKey:`
- `keys`, `values`, `keysAndValuesDo:`
- `merge:`, `collect:`, `select:`

**Usage:**
```beamtalk
person := #{#name => 'Alice', #age => 30}
name := person at: #name              // => 'Alice'
older := person at: #age put: 31      // => #{#name => 'Alice', #age => 31}

person keysAndValuesDo: [:k :v |
  Transcript show: k; show: ' => '; show: v
]
```

## Compiler Integration

The standard library classes are **built-in** to the compiler. When the compiler sees messages like `+`, `ifTrue:ifFalse:`, or `value`, it:

1. **Recognizes the message** as a standard library operation
2. **Generates optimized code** where possible (e.g., direct BEAM bytecode)
3. **Falls back to message dispatch** for dynamic or overridden cases

For example:
```beamtalk
x + y  // Generates: erlang:'+'(X, Y)

condition ifTrue: [action] ifFalse: [alternative]
// Generates: case Condition of
//              true -> Action();
//              false -> Alternative()
//            end
```

This allows the compiler to optimize primitive operations while maintaining the uniform message-send API.

## BEAM Mapping

Each Beamtalk class maps to BEAM/Erlang concepts:

| Beamtalk | Erlang/BEAM | Has Process? | Notes |
|----------|-------------|--------------|-------|
| `ProtoObject` | Abstract root | — | Root of hierarchy; not instantiated |
| `Object` | Erlang map/record | ❌ No | Value type; plain data |
| `Actor` | `gen_server` process | ✅ Yes | Each instance is a separate process |
| `Integer` | Erlang integer | ❌ No | Immediate value; arbitrary precision |
| `Float` | Erlang float | ❌ No | Boxed value; IEEE 754 |
| `True` / `False` | Atoms `true` / `false` | ❌ No | Immediate atoms |
| `Nil` | Atom `nil` | ❌ No | Follows Elixir convention |
| `Atom` | Erlang atom | ❌ No | Immediate value |
| `Block` | Anonymous fun (closure) | ❌ No | Captures lexical scope |
| `String` | Binary `<<"UTF-8">>` | ❌ No | Boxed; UTF-8 encoded |
| `Array` | Erlang tuple `{...}` | ❌ No | Boxed; O(1) access |
| `List` | Erlang list `[...]` | ❌ No | Boxed; O(1) prepend |
| `Set` | `ordsets` (sorted list) | ❌ No | Boxed; O(log n) membership |
| `Dictionary` | Erlang map `#{...}` | ❌ No | Boxed; O(log n) key access |

**Key distinction:** Value types (Object subclasses) have no process — they're BEAM terms. Actor subclasses are processes with their own mailbox.

## Implementation Status

| Class | Status | Notes |
|-------|--------|-------|
| `ProtoObject` | ✅ Defined | Root class, no instances |
| `Object` | ✅ Defined | Value type root; needs `new` method |
| `Actor` | ✅ Defined | Actor root; needs `new` error override |
| `True` / `False` | ✅ Defined | Primitive; needs compiler optimization |
| `Nil` | ✅ Defined | Primitive; needs compiler nil handling |
| `Block` | ✅ Defined | Primitive; needs compiler closure codegen |
| `Integer` | ✅ Defined | Primitive; needs compiler operator codegen |
| `String` | ✅ Defined | Primitive; needs compiler string ops |
| `Collection` | ✅ Defined | Abstract base protocol |
| `SequenceableCollection` | ✅ Defined | Ordered collection protocol |
| `Array` | ✅ Defined | Primitive; needs compiler tuple ops |
| `List` | ✅ Defined | Primitive; needs compiler list ops |
| `Set` | ✅ Defined | Primitive; needs compiler ordsets ops |
| `Dictionary` | ✅ Defined | Primitive; needs compiler map ops |

## Testing

Standard library tests are in `test-package-compiler/cases/stdlib_*/`. Each test case:

1. Creates a `.bt` file using stdlib classes
2. Compiles to Core Erlang
3. Verifies AST via snapshot tests
4. Optionally runs the generated BEAM code

Example test case:
```beamtalk
// test-package-compiler/cases/stdlib_boolean/main.bt
(true) ifTrue: [Transcript show: 'yes'] ifFalse: [Transcript show: 'no']
```

## Future Extensions

Planned additions to the standard library:

- **Number** - Abstract number protocol (Integer, Float, Rational)
- **Float** - Floating-point numbers
- **OrderedSet** - Set that maintains insertion order
- **Stream** - Lazy sequence processing
- **Exception** - Error handling protocol
- **Process** - Low-level process primitives
- **File** - File I/O operations
- **Network** - Socket and HTTP support

## Contributing

When adding new standard library classes:

1. **Follow message-based design** - Everything is a message send
2. **Document thoroughly** - Include usage examples and BEAM mapping
3. **Add license header** - Apache 2.0 (see existing files)
4. **Create test cases** - Add to `test-package-compiler/cases/`
5. **Update this README** - Document new classes and their protocols

See [AGENTS.md](../AGENTS.md) for full development guidelines.
