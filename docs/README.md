# Beamtalk Documentation

Welcome to the Beamtalk documentation! This guide will help you navigate the design documents and understand how Beamtalk brings Smalltalk's live programming philosophy to the BEAM virtual machine.

---

## Quick Start

**New to Beamtalk?** Start here:

1. **[Design Principles](beamtalk-principles.md)** — The core philosophy guiding all decisions
2. **[Language Features](beamtalk-language-features.md)** — Syntax reference and planned features
3. **[Examples](../examples/)** — Simple programs to see the language in action

---

## Documentation Overview

### Core Language Design

| Document | Description |
|----------|-------------|
| [Design Principles](beamtalk-principles.md) | Interactive-first, actors, hot reload, async-first — the foundational ideas |
| [Language Features](beamtalk-language-features.md) | Complete syntax reference: messages, blocks, classes, strings |
| [Syntax Rationale](beamtalk-syntax-rationale.md) | Why we keep certain Smalltalk conventions and diverge from others |
| [Object Model](beamtalk-object-model.md) | How "everything is an object" maps to the BEAM |

### Architecture & Implementation

| Document | Description |
|----------|-------------|
| [Architecture](beamtalk-architecture.md) | Compiler pipeline, runtime, and hot code loading |
| [BEAM Interop](beamtalk-interop.md) | Calling Erlang/Elixir from Beamtalk and vice versa |
| [Testing Strategy](beamtalk-testing-strategy.md) | How we test the compiler and runtime |

### Tooling & Environment

| Document | Description |
|----------|-------------|
| [IDE & Live Development](beamtalk-ide.md) | Smalltalk-style browser, inspector, and debugger |

### Vision & Planning

| Document | Description |
|----------|-------------|
| [For AI Agents](beamtalk-for-agents.md) | Multi-agent AI systems with live actors |
| [Feasibility Assessment](beamtalk-feasibility.md) | Technical and market analysis |

---

## Reading Paths

### "I want to understand the language"

1. [Design Principles](beamtalk-principles.md) — Philosophy and core ideas
2. [Syntax Rationale](beamtalk-syntax-rationale.md) — Why Beamtalk looks the way it does
3. [Language Features](beamtalk-language-features.md) — Full syntax reference
4. [Examples](../examples/) — See it in practice

### "I want to understand how it works"

1. [Architecture](beamtalk-architecture.md) — Compiler and runtime overview
2. [Object Model](beamtalk-object-model.md) — Smalltalk objects on BEAM
3. [BEAM Interop](beamtalk-interop.md) — Integration with Erlang/Elixir
4. [Testing Strategy](beamtalk-testing-strategy.md) — How we verify correctness

### "I want to build AI agents"

1. [For AI Agents](beamtalk-for-agents.md) — Multi-agent AI systems
2. [Design Principles](beamtalk-principles.md) — Live programming for exploration
3. [IDE & Live Development](beamtalk-ide.md) — Interactive debugging and inspection

---

## Code Examples

### Examples Directory ([examples/](../examples/))

Simple programs demonstrating language features:

- [hello.bt](../examples/hello.bt) — Basic expressions and string literals
- [counter.bt](../examples/counter.bt) — Assignment and arithmetic

Use the REPL to load examples:

```
$ beamtalk repl
> :load examples/hello.bt
Loaded
```

### Standard Library ([lib/](../lib/))

The foundational classes implementing Smalltalk's "everything is a message" philosophy:

| Class | Description |
|-------|-------------|
| [Actor](../lib/Actor.bt) | Base class for all actors (BEAM processes) |
| [Block](../lib/Block.bt) | First-class closures |
| [True](../lib/True.bt) / [False](../lib/False.bt) | Boolean control flow via messages |
| [Nil](../lib/Nil.bt) | Null object pattern |
| [Integer](../lib/Integer.bt) | Arbitrary precision arithmetic |
| [String](../lib/String.bt) | UTF-8 text with grapheme operations |
| [Array](../lib/Array.bt) | Fixed-size indexed collection (tuple) |
| [List](../lib/List.bt) | Linked list with fast prepend |
| [Set](../lib/Set.bt) | Unordered unique elements |
| [Dictionary](../lib/Dictionary.bt) | Key-value map |
| [Collection](../lib/Collection.bt) | Abstract collection protocol |

See [lib/README.md](../lib/README.md) for detailed documentation.

### Test Cases ([test-package-compiler/cases/](../test-package-compiler/cases/))

Comprehensive test cases for language features, each containing:
- `.bt` source files
- Expected Core Erlang output (snapshots)

Browse these for real-world usage patterns.

---

## Contributing

See [AGENTS.md](../AGENTS.md) in the repository root for:
- Development guidelines
- Code conventions
- How to add new features
- Testing requirements

---

## About Beamtalk

Beamtalk is a Smalltalk/Newspeak-inspired language targeting the BEAM (Erlang VM). It combines:

- **Smalltalk's simplicity** — Everything is a message send
- **BEAM's reliability** — Lightweight processes, fault tolerance, hot code reload
- **Live development** — No compile-deploy-restart cycle

The compiler is written in Rust and generates Core Erlang, which is then compiled to BEAM bytecode.

**Repository:** [github.com/jamesc/beamtalk](https://github.com/jamesc/beamtalk)
