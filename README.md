# Beamtalk

[![CI](https://github.com/jamesc/beamtalk/actions/workflows/ci.yml/badge.svg)](https://github.com/jamesc/beamtalk/actions/workflows/ci.yml)

**A live, interactive Smalltalk for the BEAM VM**

Beamtalk brings Smalltalk's legendary live programming experience to Erlang's battle-tested runtime. Write code in a running system, hot-reload modules without restarts, and scale to millions of concurrent actors.

```beamtalk
// Spawn an actor with state
counter := Counter spawn

// Send messages (async by default)
counter increment
counter increment
value := counter getValue await  // => 2

// Cascades - multiple messages to same receiver
Transcript show: 'Hello'; cr; show: 'World'

// Map literals
config := #{#host => 'localhost', #port => 8080}
```

---

## Why Beamtalk?

| Feature | Benefit |
|---------|---------|
| **Interactive-first** | REPL and live workspace, not batch compilation |
| **Hot code reload** | Edit and reload modules in running systems |
| **Actors everywhere** | Every object is a BEAM process with fault isolation |
| **Async by default** | Message sends return futures; no blocking |
| **Full reflection** | Inspect any actor's state, mailbox, and methods at runtime |
| **BEAM interop** | Call Erlang/Elixir libraries; deploy to existing infrastructure |
| **Supervision built-in** | Declarative fault tolerance with restart strategies |

---

## Key Features

### Actors as Objects

Every Beamtalk object is a BEAM process with its own state and mailbox:

```beamtalk
Actor subclass: Counter
  state: value = 0

  increment => self.value += 1
  decrement => self.value -= 1
  getValue => ^self.value
```

### Async Message Passing

Messages are asynchronous by default, returning futures:

```beamtalk
// Returns immediately with a future
result := agent analyze: data

// Wait when you need the value
value := result await

// Or use continuations
agent analyze: data
  whenResolved: [:value | self process: value]
  whenRejected: [:error | self handle: error]
```

### Pattern Matching

Erlang-inspired pattern matching for clean message handling:

```beamtalk
handle: {#ok, value} => self process: value
handle: {#error, reason} => self logError: reason
handle: _ => self handleUnknown
```

### Declarative Supervision

OTP supervision trees as language-level constructs:

```beamtalk
Supervisor subclass: WebApp
  children: [
    {DatabasePool, scale: 10},
    HTTPRouter spawn,
    MetricsCollector spawn
  ]
  strategy: #oneForOne
```

### Live Patching

Hot-reload with dedicated syntax:

```beamtalk
// Update running actors instantly
patch Agent >> processMessage: msg {
  Telemetry emit: #messageReceived
  ^super processMessage: msg
}
```

---

## Designed for AI Agents

Beamtalk is purpose-built for multi-agent AI systems:

- **Spawn agent swarms** — millions of concurrent LLM-powered actors
- **Live inspection** — browse agent state, memory, and message flow
- **Hot-patch prompts** — edit agent behavior while they run
- **Fault tolerance** — agents crash and restart cleanly via supervision
- **Distributed** — spread agents across BEAM clusters transparently

```beamtalk
Supervisor subclass: ResearchTeam
  children: [
    Researcher spawn model: #claude_opus,
    Critic spawn model: #gpt4_turbo,
    Writer spawn model: #claude_sonnet
  ]
  strategy: #oneForOne

team := ResearchTeam spawn
analysis := team analyze: codeRepo

// Inspect agents while they work
team inspect
```

See [Beamtalk for Agents](docs/beamtalk-for-agents.md) for detailed use cases.

---

## Getting Started

### Prerequisites

- **Rust** (latest stable)
- **Erlang/OTP 26+** with `erlc` on PATH

### Build & Run

```bash
# Clone and build
git clone https://github.com/jamesc/beamtalk.git
cd beamtalk
cargo build

# Start the REPL
cargo run -- repl
```

### REPL Usage

```text
Beamtalk v0.1.0
Type :help for available commands, :exit to quit.

> message := 'Hello, Beamtalk!'
"Hello, Beamtalk!"

> 2 + 3 * 4
14

> :load examples/hello.bt
Loaded

> :bindings
message = "Hello, Beamtalk!"
```

### Load Files

```text
> :load examples/counter.bt
Loaded

> :reload
Reloaded
```

---

## Project Status

**Active development** — the compiler core is working with an interactive REPL.

### What Works Now

- ✅ **REPL** — Interactive evaluation with variable persistence
- ✅ **Lexer & Parser** — Full expression parsing with error recovery
- ✅ **Core Erlang codegen** — Compiles to BEAM bytecode via `erlc`
- ✅ **Actors** — Spawn actors with state, send async messages
- ✅ **Cascades** — Multiple messages to same receiver
- ✅ **Map literals** — `#{key => value}` syntax with Dictionary codegen
- ✅ **Class definitions** — AST support for class and method definitions
- ✅ **Standard library** — Boolean, Block, Integer, String, Collections

### In Progress

- 🔄 **Field assignments** — Actor state mutations
- 🔄 **Method dispatch** — Full message routing
- 🔄 **Supervision trees** — Declarative fault tolerance

### Planned

- 📋 **LSP** — Language server for IDE integration
- 📋 **Live browser** — Smalltalk-style class browser (Phoenix `LiveView`)
- 📋 **Hot patching** — Edit running actors in place

---

## Documentation

📚 **[Documentation Index](docs/README.md)** — Start here for a guided tour

### Core Documents

- [Design Principles](docs/beamtalk-principles.md) — 13 core principles guiding all decisions
- [Language Features](docs/beamtalk-language-features.md) — Syntax, semantics, and examples
- [Syntax Rationale](docs/beamtalk-syntax-rationale.md) — Why we keep/change Smalltalk conventions
- [Object Model](docs/beamtalk-object-model.md) — How Smalltalk objects map to BEAM

### Architecture

- [Architecture](docs/beamtalk-architecture.md) — Compiler pipeline, runtime, hot reload
- [BEAM Interop](docs/beamtalk-interop.md) — Erlang/Elixir integration specification
- [Testing Strategy](docs/beamtalk-testing-strategy.md) — How we verify compiler correctness

### Tooling & Vision

- [IDE and Live Development](docs/beamtalk-ide.md) — Smalltalk-style integrated environment
- [Agent Systems](docs/beamtalk-for-agents.md) — Multi-agent AI use cases
- [Feasibility Assessment](docs/beamtalk-feasibility.md) — Technical and market analysis

---

## Examples & Standard Library

### Examples ([examples/](examples/))

Simple programs demonstrating language features:

```bash
cargo run -- repl
> :load examples/hello.bt
```

### Standard Library ([lib/](lib/))

Foundational classes implementing "everything is a message":

| Class | Description |
|-------|-------------|
| `Actor` | Base class for all actors |
| `Block` | First-class closures |
| `True` / `False` | Boolean control flow |
| `Integer` | Arbitrary precision arithmetic |
| `String` | UTF-8 text operations |
| `Array` / `List` | Ordered collections |
| `Set` / `Dictionary` | Unordered collections |
| `Nil` | Null object pattern |

See [lib/README.md](lib/README.md) for full documentation.

---

## Repository Structure

```text
beamtalk/
├── crates/
│   ├── beamtalk-core/       # Lexer, parser, AST, codegen
│   └── beamtalk-cli/        # Command-line interface & REPL
├── lib/                      # Standard library (.bt files)
├── runtime/                  # Erlang runtime (actors, REPL backend)
├── docs/                     # Design documents
├── examples/                 # Example programs
└── test-package-compiler/    # Snapshot tests for compiler
```

The compiler is written in **Rust** and generates **Core Erlang**, which compiles to BEAM bytecode via `erlc`.

---

## Inspiration

Beamtalk combines ideas from:

- **Smalltalk/Newspeak** — Live programming, message-based syntax, reflection
- **Erlang/BEAM** — Actors, fault tolerance, hot code reload, distribution
- **Elixir** — Pipe operator, protocols, comprehensions, with blocks
- **Gleam** — Result types, exhaustive pattern matching
- **Dylan** — Sealing, conditions/restarts, method combinations
- **TypeScript** — Compiler-as-language-service architecture

---

## Contributing

See [AGENTS.md](AGENTS.md) for development guidelines, coding standards, and task tracking.

We use [Linear](https://linear.app) for issue tracking (project prefix: `BT`).

---

## License

Licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) for details.
