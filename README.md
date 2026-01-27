# Beamtalk

**A live, interactive Smalltalk for the BEAM VM**

Beamtalk brings Smalltalk's legendary live programming experience to Erlang's battle-tested runtime. Write code in a running system, hot-reload behavior without restarts, and scale to millions of concurrent actors.

```smalltalk
"Spawn an actor"
counter := Counter spawn.

"Send messages (async by default)"
counter increment.
counter increment.
value := counter getValue await.  "=> 2"

"Hot-patch while running"
patch Counter >> #increment {
  Telemetry log: 'incrementing'.
  self.value += 1.
}
```

---

## Why Beamtalk?

| Feature | Benefit |
|---------|---------|
| **Interactive-first** | REPL and live workspace, not batch compilation |
| **Hot code reload** | Edit behavior in running systems, sub-200ms feedback |
| **Actors everywhere** | Every object is a BEAM process with fault isolation |
| **Async by default** | Message sends return futures; no blocking |
| **Full reflection** | Inspect any actor's state, mailbox, and methods at runtime |
| **BEAM interop** | Call Erlang/Elixir libraries; deploy to existing infrastructure |
| **Supervision built-in** | Declarative fault tolerance with restart strategies |

---

## Key Features

### Actors as Objects

Every Beamtalk object is a BEAM process with its own state and mailbox:

```smalltalk
Actor subclass: Counter
  state: value = 0

  #increment => self.value += 1
  #decrement => self.value -= 1
  #getValue => ^self.value
```

### Async Message Passing

Messages are asynchronous by default, returning futures:

```smalltalk
"Returns immediately with a future"
result := agent analyze: data.

"Wait when you need the value"
value := result await.

"Or use continuations"
agent analyze: data
  whenResolved: [:value | self process: value]
  whenRejected: [:error | self handle: error].
```

### Pattern Matching

Erlang-inspired pattern matching for clean message handling:

```smalltalk
#handle: {#ok, value} => self process: value.
#handle: {#error, reason} => self logError: reason.
#handle: _ => self handleUnknown.
```

### Declarative Supervision

OTP supervision trees as language-level constructs:

```smalltalk
Supervisor subclass: WebApp
  children: [
    {DatabasePool, scale: 10},
    HTTPRouter spawn,
    MetricsCollector spawn
  ]
  strategy: #oneForOne.
```

### Live Patching

Hot-reload with dedicated syntax:

```smalltalk
"Update running actors instantly"
patch Agent >> #processMessage: msg {
  Telemetry emit: #messageReceived.
  ^super processMessage: msg.
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

```smalltalk
Supervisor subclass: ResearchTeam
  children: [
    Researcher spawn model: #claude_opus,
    Critic spawn model: #gpt4_turbo,
    Writer spawn model: #claude_sonnet
  ]
  strategy: #oneForOne.

team := ResearchTeam spawn.
analysis := team analyze: codeRepo.

"Inspect agents while they work"
team inspect.
```

See [Beamtalk for Agents](docs/beamtalk-for-agents.md) for detailed use cases.

---

## Project Status

**Early development** — compiler and tooling are being built.

The language design is documented:
- [Design Principles](docs/beamtalk-principles.md) — 13 core principles guiding all decisions
- [Language Features](docs/beamtalk-language-features.md) — Planned syntax, semantics, and tooling
- [Agent Systems](docs/beamtalk-for-agents.md) — Multi-agent AI use cases

### Planned Architecture

```
beamtalk/
├── crates/
│   ├── beamtalk-core/     # Lexer, parser, AST, type checking, codegen
│   ├── beamtalk-cli/      # Command-line interface
│   └── beamtalk-lsp/      # Language server
├── lib/                    # Standard library (.bt files)
├── docs/                   # Documentation
└── examples/               # Example programs
```

The compiler is written in Rust and generates Core Erlang, which compiles to BEAM bytecode via `erlc`.

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

TBD
