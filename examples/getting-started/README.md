# Getting Started — Beamtalk Examples Workspace

Introductory Beamtalk examples covering the core language concepts.

## Starting the REPL

```bash
cd examples/getting-started
beamtalk repl
```

## Examples

### hello.bt — Minimal class

```text
> :load src/hello.bt
> Hello new
```

### counter.bt — Stateful actor

```text
> :load src/counter.bt
> c := Counter spawn
> c increment
> c increment
> c getValue
2
```

Tests live under `test/` — run them with `:load test/counter_test.bt`, `:load test/hello_test.bt`, etc.

### hanoi.bt — Recursion and multi-keyword messages

```text
> :load src/hanoi.bt
> h := Hanoi new
> h solve: 3 from: 'A' to: 'C' via: 'B'
```

### logging_counter.bt — Inheritance and `super`

```text
> :load src/counter.bt
> :load src/logging_counter.bt
> lc := LoggingCounter spawn
> lc increment
> lc getValue
```

### stream.bt — Lazy pipelines

```text
> :load src/stream.bt
```

### protoobject_proxy.bt — Transparent proxy pattern

```text
> :load src/protoobject_proxy.bt
```

## What's Next?

- **[bank](../bank/)** — Actor coordination: bank accounts and transfers
- **[chat-room](../chat-room/)** — Multi-actor: actors, inheritance, collections
- **[Language reference](../../docs/beamtalk-language-features.md)**
- **[REPL tutorial](../repl-tutorial.md)**
