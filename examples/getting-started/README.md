# Getting Started — Beamtalk Examples Workspace

Introductory Beamtalk examples covering the core language concepts.

## Starting the REPL

```bash
cd examples/getting-started
beamtalk repl
```

## Examples

### hello.bt — Minimal class

```
> :load hello.bt
> Hello new
```

### counter.bt — Stateful actor

```
> :load counter.bt
> c := Counter spawn
> c increment
> c increment
> c getValue
2
```

`counter_test.bt` contains assertions you can run with `:load counter_test.bt`.

### hanoi.bt — Recursion and multi-keyword messages

```
> :load hanoi.bt
> h := Hanoi new
> h solve: 3 from: 'A' to: 'C' via: 'B'
```

### logging_counter.bt — Inheritance and `super`

```
> :load counter.bt
> :load logging_counter.bt
> lc := LoggingCounter spawn
> lc increment
> lc getValue
```

### stream.bt — Lazy pipelines

```
> :load stream.bt
```

### protoobject_proxy.bt — Transparent proxy pattern

```
> :load protoobject_proxy.bt
```

## What's Next?

- **[bank](../bank/)** — Actor coordination: bank accounts and transfers
- **[chat-room](../chat-room/)** — Multi-actor: actors, inheritance, collections
- **[Language reference](../../docs/beamtalk-language-features.md)**
- **[REPL tutorial](../repl-tutorial.md)**
