# Tooling

Tooling is part of the language, not an afterthought. Beamtalk is designed to be used interactively.

## CLI Tools

```bash
# Project management
beamtalk new myapp          # Create new project
beamtalk build              # Compile to BEAM
beamtalk run                # Compile and start
beamtalk check              # Check for errors without compiling
beamtalk daemon start/stop  # Manage compiler daemon

# Development
beamtalk repl               # Interactive REPL
beamtalk test               # Run test suite
```

## REPL Features

```beamtalk
// Spawn and interact
counter := Counter spawn
counter increment
counter getValue  // => 1
```

## VS Code Extension

The [Beamtalk VS Code extension](https://github.com/jamesc/beamtalk/tree/main/editors/vscode) provides:

- Syntax highlighting for `.bt` files
- Language Server Protocol (LSP) integration
- Error diagnostics with source spans

## Testing Framework

Beamtalk includes a native test framework inspired by Smalltalk's SUnit.

```beamtalk
// stdlib/test/counter_test.bt

TestCase subclass: CounterTest

  testInitialValue =>
    self assert: (Counter spawn getValue) equals: 0

  testIncrement =>
    self assert: (Counter spawn increment) equals: 1

  testMultipleIncrements =>
    counter := Counter spawn
    3 timesRepeat: [counter increment]
    self assert: (counter getValue) equals: 3
```

Each test method gets a fresh instance with `setUp` → test → `tearDown` lifecycle.

### Assertion Methods

| Method | Description | Example |
|--------|-------------|---------|
| `assert:` | Assert condition is true | `self assert: (x > 0)` |
| `assert:equals:` | Assert two values are equal | `self assert: result equals: 42` |
| `deny:` | Assert condition is false | `self deny: list isEmpty` |
| `should:raise:` | Assert block raises error | `self should: [1 / 0] raise: #badarith` |
| `fail:` | Unconditional failure | `self fail: "not implemented"` |

### REPL Integration

Run tests interactively from the REPL using either `:` shortcuts or native message sends:

```text
> :load stdlib/test/counter_test.bt
Loaded CounterTest

> :test CounterTest
Running 1 test class...
  ✓ testIncrement
  ✓ testMultipleIncrements
2 passed, 0 failed

// Equivalent native API — works from compiled code too:
> (Workspace test: CounterTest) failed
// => 0
```
