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

### Collection (`Collection.bt`)

Protocol for arrays, lists, sets, and maps.

**Key messages:**
- Size: `size`, `isEmpty`
- Access: `at:`, `at:put:`, `first`, `last`
- Modify: `add:`, `remove:`, `removeAt:`
- Iterate: `each:`, `collect:`, `select:`, `reject:`
- Test: `includes:`, `allSatisfy:`, `anySatisfy:`
- Reduce: `inject:into:`

**Usage:**
```beamtalk
numbers := {1, 2, 3, 4, 5}
doubled := numbers collect: [:n | n * 2]
evens := numbers select: [:n | n isEven]
sum := numbers inject: 0 into: [:acc :n | acc + n]

(numbers includes: 3) ifTrue: [Transcript show: 'Found']
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

| Beamtalk | Erlang/BEAM |
|----------|-------------|
| `Actor` | `gen_server` process with state map |
| `True` / `False` | Atoms `true` / `false` |
| `Block` | Anonymous fun (closure) |
| `Integer` | Erlang integer (bignum support) |
| `String` | Binary `<<"UTF-8">>` |
| `Collection` | Tuple `{...}`, List `[...]`, Map `#{...}` |

## Implementation Status

| Class | Status | Notes |
|-------|--------|-------|
| `Actor` | ✅ Defined | Needs compiler spawn support |
| `True` / `False` | ✅ Defined | Needs compiler optimization |
| `Block` | ✅ Defined | Needs compiler closure codegen |
| `Integer` | ✅ Defined | Needs compiler operator codegen |
| `String` | ✅ Defined | Needs compiler string ops |
| `Collection` | ✅ Defined | Needs compiler collection ops |

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
- **List** - Linked list operations (cons, car, cdr)
- **Array** - Fixed-size tuple operations
- **Set** - Unordered unique collection
- **Dictionary** - Key-value map (Erlang map)
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
