# Gang of Four Patterns

> **Coming soon** — Classic design patterns from _Design Patterns: Elements of
> Reusable Object-Oriented Software_ (Gamma, Helm, Johnson, Vlissides),
> implemented idiomatically in Beamtalk.

## Planned Patterns

### Creational
- **Factory Method** — class-side factory messages
- **Singleton** — class variables and `uniqueInstance`
- **Builder** — keyword-message builder chains

### Structural
- **Adapter** — wrapping incompatible interfaces
- **Decorator** — extending behaviour via delegation
- **Proxy** — transparent forwarding (see `getting-started/src/protoobject_proxy.bt`)
- **Composite** — tree structures with uniform interface

### Behavioural
- **Observer** — event subscription and notification
- **Strategy** — interchangeable algorithms via blocks
- **Command** — encapsulating messages as objects
- **Iterator** — `do:`, `collect:`, `select:` on custom collections
- **State** — actor state machines

## Starting the Workspace

```bash
cd examples/gof-patterns
beamtalk repl
```

## What This Will Demonstrate

- How Smalltalk message passing naturally implements many GoF patterns
- Where Beamtalk actors provide a better alternative to classical OOP patterns
- Patterns that are trivial in message-passing vs complex in Java/C++
