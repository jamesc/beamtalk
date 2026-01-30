# Beamtalk Standard Library

This directory contains the foundational standard library for Beamtalk, following Smalltalk philosophy where "everything is a message send."

## Philosophy

From [beamtalk-principles.md](../docs/beamtalk-principles.md): **"Everything is a message send, following Smalltalk/Newspeak philosophy."**

Even primitive operations like arithmetic, boolean logic, and control flow are implemented as messages to objects. This provides:

1. **Uniform syntax** - No special forms, everything is `receiver message`
2. **First-class operations** - Operators are just messages that can be overridden
3. **Live debugging** - All operations can be traced and inspected
4. **Extensibility** - Users can extend primitives by adding methods

## Core Classes

### Actor (`Actor.bt`)

Base class for all actors in Beamtalk. Every object is an actor (BEAM process).

**Key messages:**
- `spawn` - Create new instance (compiler primitive)
- `class` - Get actor's class name
- `inspect` - Show description on Transcript
- `describe` - Return string description

**Usage:**
```beamtalk
Actor subclass: MyActor
  state: count = 0
  
  increment => self.count += 1
  getValue => ^self.count

counter := MyActor spawn
counter increment
value := counter getValue
```

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

### Nil (`Nil.bt`)

Represents the absence of a value, equivalent to Smalltalk's UndefinedObject.

**Key messages:**
- `isNil` - Returns true (only Nil returns true)
- `notNil` - Returns false (only Nil returns false)
- `ifNil:` - Evaluate block if nil
- `ifNotNil:` - Returns self without evaluating block (on non-nil objects, evaluates block with receiver as argument - see BT-99)
- `ifNil: ifNotNil:` - Conditional based on nil status

**Usage:**
```beamtalk
// Testing for nil
value isNil ifTrue: [self useDefault]

// Nil-safe access with default
name := user name ifNil: ['Anonymous']

// Conditional on nil
result ifNil: [0] ifNotNil: [:val | val * 2]
```

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

| Beamtalk | Erlang/BEAM | Notes |
|----------|-------------|-------|
| `Actor` | `gen_server` process with state map | Each actor is a separate process |
| `True` / `False` | Atoms `true` / `false` | Optimized by compiler |
| `Nil` | Atom `nil` | Follows Elixir convention |
| `Block` | Anonymous fun (closure) | Captures lexical scope |
| `Integer` | Erlang integer (bignum support) | Arbitrary precision |
| `String` | Binary `<<"UTF-8">>` | UTF-8 encoded, not charlist |
| `Array` | Erlang tuple `{...}` | O(1) access, O(n) update |
| `List` | Erlang list `[...]` | O(1) prepend, O(n) access |
| `Set` | `ordsets` (sorted list) | O(log n) membership |
| `Dictionary` | Erlang map `#{...}` | O(log n) key access |

## Implementation Status

| Class | Status | Notes |
|-------|--------|-------|
| `Actor` | ✅ Defined | Needs compiler spawn support |
| `True` / `False` | ✅ Defined | Needs compiler optimization |
| `Nil` | ✅ Defined | Needs compiler nil handling |
| `Block` | ✅ Defined | Needs compiler closure codegen |
| `Integer` | ✅ Defined | Needs compiler operator codegen |
| `String` | ✅ Defined | Needs compiler string ops |
| `Collection` | ✅ Defined | Abstract base protocol |
| `SequenceableCollection` | ✅ Defined | Ordered collection protocol |
| `Array` | ✅ Defined | Needs compiler tuple ops |
| `List` | ✅ Defined | Needs compiler list ops |
| `Set` | ✅ Defined | Needs compiler ordsets ops |
| `Dictionary` | ✅ Defined | Needs compiler map ops |

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
