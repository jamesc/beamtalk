# Beamtalk Documentation

Welcome to the Beamtalk documentation! This guide will help you navigate the design documents and understand how Beamtalk brings Smalltalk's live programming philosophy to the BEAM virtual machine.

**Important:** Beamtalk is Smalltalk-**inspired**, not Smalltalk-**compatible**. We preserve Smalltalk's core ideas (message passing, live environment, uniform syntax) while making pragmatic changes for BEAM and modern development. See [Syntax Rationale](beamtalk-syntax-rationale.md) for details.

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
| [Object Model](ADR/0005-beam-object-model-pragmatic-hybrid.md) | How "everything is an object" maps to the BEAM (ADR 0005) |
| [Known Limitations](known-limitations.md) | What's not yet supported in v0.1, with workarounds |

### Architecture & Implementation

| Document | Description |
|----------|-------------|
| [Architecture](beamtalk-architecture.md) | Compiler pipeline, runtime, and hot code loading |
| [Testing Strategy](development/testing-strategy.md) | How we test the compiler and runtime |

### Vision & Planning

| Document | Description |
|----------|-------------|
| [Agent-Native Development](beamtalk-agent-native-development.md) | AI agents as developers and live actor systems |

---

## Reading Paths

### "I want to understand the language"

1. [Design Principles](beamtalk-principles.md) — Philosophy and core ideas
2. [Syntax Rationale](beamtalk-syntax-rationale.md) — Why Beamtalk looks the way it does
3. [Language Features](beamtalk-language-features.md) — Full syntax reference
4. [Examples](../examples/) — See it in practice

### "I want to understand how it works"

1. [Architecture](beamtalk-architecture.md) — Compiler and runtime overview
2. [Object Model (ADR 0005)](ADR/0005-beam-object-model-pragmatic-hybrid.md) — Smalltalk objects on BEAM
3. [Testing Strategy](development/testing-strategy.md) — How we verify correctness

For implementation details, see [internal/](internal/) documentation.

### "I want to build AI agents"

1. [Agent-Native Development](beamtalk-agent-native-development.md) — Live actor systems for AI agents
2. [Design Principles](beamtalk-principles.md) — Live programming for exploration

---

## Code Examples

### Examples Directory ([examples/](../examples/))

Simple programs demonstrating language features:

- [hello.bt](../examples/hello.bt) — Simple class with a greeting method
- [counter.bt](../examples/counter.bt) — Assignment and arithmetic

Use the REPL to load examples:

```
$ beamtalk repl
> :load examples/hello.bt
Loaded
```

### Standard Library ([stdlib/src/](../stdlib/src/))

The foundational classes implementing Smalltalk's "everything is a message" philosophy:

| Class | Description |
|-------|-------------|
| [Object](../stdlib/src/Object.bt) | Base class for value types; [Actor](../stdlib/src/Actor.bt) for BEAM processes |
| [Block](../stdlib/src/Block.bt) | First-class closures |
| [True](../stdlib/src/True.bt) / [False](../stdlib/src/False.bt) | Boolean control flow via messages |
| [UndefinedObject](../stdlib/src/UndefinedObject.bt) | Null object pattern (`nil` singleton) |
| [Integer](../stdlib/src/Integer.bt) | Arbitrary precision arithmetic |
| [String](../stdlib/src/String.bt) | UTF-8 text with grapheme operations |
| [Array](../stdlib/src/Array.bt) | Fixed-size indexed collection (tuple) |
| [List](../stdlib/src/List.bt) | Linked list with fast prepend |
| [Set](../stdlib/src/Set.bt) | Unordered unique elements |
| [Dictionary](../stdlib/src/Dictionary.bt) | Key-value map |
| [Regex](../stdlib/src/Regex.bt) | Regular expression matching (PCRE2) |
| [DateTime](../stdlib/src/DateTime.bt) | Date and time operations |
| [JSON](../stdlib/src/JSON.bt) | JSON parsing and encoding |
| [Collection](../stdlib/src/Collection.bt) | Abstract collection protocol |

See [stdlib/src/README.md](../stdlib/src/README.md) for detailed documentation.

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

- **Smalltalk's simplicity** — Everything is a message send (with pragmatic BEAM adaptations)
- **BEAM's reliability** — Lightweight processes, fault tolerance, hot code reload
- **Live development** — No compile-deploy-restart cycle

The compiler is written in Rust and generates Core Erlang, which is then compiled to BEAM bytecode.

**Repository:** [github.com/jamesc/beamtalk](https://github.com/jamesc/beamtalk)
