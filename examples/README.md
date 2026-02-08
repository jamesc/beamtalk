# Beamtalk Examples

This directory contains example Beamtalk programs to help you learn the language.

## Getting Started

**New to Beamtalk?** Start with the [REPL Tutorial](repl-tutorial.md)!

The tutorial walks you through:
- Starting the REPL
- Basic expressions and variables
- Loading files
- Working with actors
- Using REPL commands

## Examples

### Simple Examples

- **hello.bt** - Minimal example showing a simple class
  ```bash
  beamtalk repl
  > :load examples/hello.bt
  > Hello new
  ```

- **counter.bt** - Counter actor with state and methods
  ```bash
  beamtalk repl
  > :load examples/counter.bt
  > c := Counter spawn
  > c increment
  > c increment
  > c getValue
  2
  ```

### Advanced Examples

- **hanoi.bt** - Towers of Hanoi: recursion, multi-keyword messages, Transcript I/O
  ```bash
  beamtalk repl
  > :load examples/hanoi.bt
  > h := Hanoi new
  > h solve: 3 from: 'A' to: 'C' via: 'B'
  ```
- **super_example.bt** - Inheritance and super keyword usage
- **logging_counter.bt** - Subclassing Counter with logging behavior
- **protoobject_proxy.bt** - Proxy pattern with message forwarding

## Running Examples

### In the REPL

```bash
$ beamtalk repl
> :load examples/<filename>.bt
```

### Compiling to BEAM

```bash
$ beamtalk build examples/<filename>.bt
```

## Documentation

- **[REPL Tutorial](repl-tutorial.md)** - Interactive tutorial for beginners
- **[Language Features](../docs/beamtalk-language-features.md)** - Complete syntax reference
- **[Design Principles](../docs/beamtalk-principles.md)** - Philosophy and design goals
- **[Architecture](../docs/beamtalk-architecture.md)** - Compiler and runtime overview

## Contributing Examples

When adding new examples:

1. Include copyright header
2. Add clear comments explaining what the code does
3. Reference the example in this README
4. Test that it loads and runs correctly in the REPL
5. Keep examples focused on one concept

## Getting Help

- Type `:help` in the REPL for command reference
- See main `README.md` for installation and setup
- Report issues at: https://github.com/jamesc/beamtalk/issues
